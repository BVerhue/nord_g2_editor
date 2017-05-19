{
    This file is part of the Free Component Library

    XML writing routines
    Copyright (c) 1999-2000 by Sebastian Guenther, sg@freepascal.org
    Modified in 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


unit XMLWrite;

{$ifdef fpc}{$MODE objfpc}{$endif}
{$H+}

interface

uses Classes, DOM;

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String); overload;
procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text); overload;
procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream); overload;

procedure WriteXML(Element: TDOMNode; const AFileName: String); overload;
procedure WriteXML(Element: TDOMNode; var AFile: Text); overload;
procedure WriteXML(Element: TDOMNode; AStream: TStream); overload;


// ===================================================================

implementation

uses SysUtils, xmlutils;

type
  TXMLWriter = class;
  TSpecialCharCallback = procedure(Sender: TXMLWriter; const s: DOMString;
    var idx: Integer);

  PAttrFixup = ^TAttrFixup;
  TAttrFixup = record
    Attr: TDOMNode;
    Prefix: PHashItem;
  end;

  TXMLWriter = class(TObject)
  private
    FStream: TStream;
    FInsideTextNode: Boolean;
    FCanonical: Boolean;
    FIndent: WideString;
    FIndentCount: Integer;
    FBuffer: PAnsiChar;
    FBufPos: PAnsiChar;
    FCapacity: Integer;
    FLineBreak: WideString;
    FNSHelper: TNSSupport;
    FAttrFixups: TFPList;
    FScratch: TFPList;
    FNSDefs: TFPList;
    procedure wrtChars(Src: PWideChar; Length: Integer);
    procedure IncIndent;
    procedure DecIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtStr(const ws: WideString); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtChr(c: WideChar); {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtIndent; {$IFDEF HAS_INLINE} inline; {$ENDIF}
    procedure wrtQuotedLiteral(const ws: WideString);
    procedure ConvWrite(const s: WideString; const SpecialChars: TSetOfChar;
      const SpecialCharCallback: TSpecialCharCallback);
    procedure WriteNSDef(B: TBinding);
    procedure NamespaceFixup(Element: TDOMElement);
  protected
    procedure WriteNode(Node: TDOMNode);
    procedure VisitDocument(Node: TDOMNode);
    procedure VisitDocument_Canonical(Node: TDOMNode);
    procedure VisitElement(Node: TDOMNode);
    procedure VisitText(Node: TDOMNode);
    procedure VisitCDATA(Node: TDOMNode);
    procedure VisitComment(Node: TDOMNode);
    procedure VisitFragment(Node: TDOMNode);
    procedure VisitAttribute(Node: TDOMNode);
    procedure VisitEntityRef(Node: TDOMNode);
    procedure VisitDocumentType(Node: TDOMNode);
    procedure VisitPI(Node: TDOMNode);
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
  end;

  TTextStream = class(TStream)
  Private
    F : ^Text;
  Public
    constructor Create(var AFile: Text);
    function Write(Const Buffer; Count: Longint): Longint; override;
  end;

{ ---------------------------------------------------------------------
    TTextStream
  ---------------------------------------------------------------------}


constructor TTextStream.Create(var AFile: Text);
begin
  inherited Create;
  f := @AFile;
end;

function TTextStream.Write(const Buffer; Count: Longint): Longint;
var
  s: AnsiString;
begin
  if Count>0 then
  begin
    SetString(s, PAnsiChar(@Buffer), Count);
    system.Write(f^, s);
  end;
  Result := Count;
end;

{ ---------------------------------------------------------------------
    TXMLWriter
  ---------------------------------------------------------------------}

const
  AttrSpecialChars = ['<', '"', '&', #9, #10, #13];
  TextSpecialChars = ['<', '>', '&', #10, #13];
  CDSectSpecialChars = [']'];
  LineEndingChars = [#13, #10];
  QuotStr = '&quot;';
  AmpStr = '&amp;';
  ltStr = '&lt;';
  gtStr = '&gt;';

constructor TXMLWriter.Create(AStream: TStream);
var
  I: Integer;
begin
  inherited Create;
  FStream := AStream;
  // some overhead - always be able to write at least one extra UCS4
  FBuffer := AllocMem(512+32);
  FBufPos := FBuffer;
  FCapacity := 512;
  // Later on, this may be put under user control
  // for now, take OS setting
  if FCanonical then
    FLineBreak := #10
  else
    FLineBreak := sLineBreak;
  // Initialize Indent string
  // TODO: this must be done in setter of FLineBreak
  SetLength(FIndent, 100);
  FIndent[1] := FLineBreak[1];
  if Length(FLineBreak) > 1 then
    FIndent[2] := FLineBreak[2]
  else
    FIndent[2] := ' ';
  for I := 3 to 100 do FIndent[I] := ' ';
  FIndentCount := 0;
  FNSHelper := TNSSupport.Create;
  FScratch := TFPList.Create;
  FNSDefs := TFPList.Create;
  FAttrFixups := TFPList.Create;
end;

destructor TXMLWriter.Destroy;
var
  I: Integer;
begin
  for I := FAttrFixups.Count-1 downto 0 do
{$IFDEF FPC}
    Dispose(PAttrFixup(FAttrFixups.List^[I]));
{$ELSE}
    Dispose(PAttrFixup(FAttrFixups[I]));
{$ENDIF}
  FAttrFixups.Free;
  FNSDefs.Free;
  FScratch.Free;
  FNSHelper.Free;
  if FBufPos > FBuffer then
    FStream.write(FBuffer^, FBufPos-FBuffer);

  FreeMem(FBuffer);
  inherited Destroy;
end;

procedure TXMLWriter.wrtChars(Src: PWideChar; Length: Integer);
var
  pb: PAnsiChar;
  wc: Cardinal;
  SrcEnd: PWideChar;
begin
  pb := FBufPos;
  SrcEnd := Src + Length;
  while Src < SrcEnd do
  begin
    if pb >= @FBuffer[FCapacity] then
    begin
      FStream.write(FBuffer^, FCapacity);
      Dec(pb, FCapacity);
      if pb > FBuffer then
        Move(FBuffer[FCapacity], FBuffer^, pb - FBuffer);
    end;

    wc := Cardinal(Src^);  Inc(Src);
    case wc of
      0..$7F:  begin
        pb^ := AnsiChar(wc); Inc(pb);
      end;

      $80..$7FF: begin
        pb^ := AnsiChar($C0 or (wc shr 6));
        pb[1] := AnsiChar($80 or (wc and $3F));
        Inc(pb,2);
      end;

      $D800..$DBFF: begin
        if (Src < SrcEnd) and (Src^ >= #$DC00) and (Src^ <= #$DFFF) then
        begin
          wc := ((LongInt(wc) - $D7C0) shl 10) + LongInt(word(Src^) xor $DC00);
          Inc(Src);

          pb^ := AnsiChar($F0 or (wc shr 18));
          pb[1] := AnsiChar($80 or ((wc shr 12) and $3F));
          pb[2] := AnsiChar($80 or ((wc shr 6) and $3F));
          pb[3] := AnsiChar($80 or (wc and $3F));
          Inc(pb,4);
        end
        else
          raise EConvertError.Create('High surrogate without low one');
      end;
      $DC00..$DFFF:
        raise EConvertError.Create('Low surrogate without high one');
      else   // $800 >= wc > $FFFF, excluding surrogates
      begin
        pb^ := AnsiChar($E0 or (wc shr 12));
        pb[1] := AnsiChar($80 or ((wc shr 6) and $3F));
        pb[2] := AnsiChar($80 or (wc and $3F));
        Inc(pb,3);
      end;
    end;
  end;
  FBufPos := pb;
end;

procedure TXMLWriter.wrtStr(const ws: WideString); { inline }
begin
  wrtChars(PWideChar(ws), Length(ws));
end;

{ No checks here - buffer always has 32 extra bytes }
procedure TXMLWriter.wrtChr(c: WideChar); { inline }
begin
  FBufPos^ := AnsiChar(ord(c));
  Inc(FBufPos);
end;

procedure TXMLWriter.wrtIndent; { inline }
begin
  wrtChars(PWideChar(FIndent), FIndentCount*2+Length(FLineBreak));
end;

procedure TXMLWriter.IncIndent;
var
  I, NewLen, OldLen: Integer;
begin
  Inc(FIndentCount);
  if Length(FIndent) < 2 * FIndentCount then
  begin
    OldLen := Length(FIndent);
    NewLen := 4 * FIndentCount;
    SetLength(FIndent, NewLen);
    for I := OldLen to NewLen do
      FIndent[I] := ' ';
  end;
end;

procedure TXMLWriter.DecIndent; { inline }
begin
  if FIndentCount>0 then dec(FIndentCount);
end;

procedure TXMLWriter.ConvWrite(const s: WideString; const SpecialChars: TSetOfChar;
  const SpecialCharCallback: TSpecialCharCallback);
var
  StartPos, EndPos: Integer;
begin
  StartPos := 1;
  EndPos := 1;
  while EndPos <= Length(s) do
  begin
    if (s[EndPos] < #128) and (AnsiChar(ord(s[EndPos])) in SpecialChars) then
    begin
      wrtChars(@s[StartPos], EndPos - StartPos);
      SpecialCharCallback(Self, s, EndPos);
      StartPos := EndPos + 1;
    end;
    Inc(EndPos);
  end;
  if StartPos <= length(s) then
    wrtChars(@s[StartPos], EndPos - StartPos);
end;

procedure AttrSpecialCharCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '"': Sender.wrtStr(QuotStr);
    '&': Sender.wrtStr(AmpStr);
    '<': Sender.wrtStr(ltStr);
    // Escape whitespace using CharRefs to be consistent with W3 spec � 3.3.3
    #9: Sender.wrtStr('&#x9;');
    #10: Sender.wrtStr('&#xA;');
    #13: Sender.wrtStr('&#xD;');
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure TextnodeNormalCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '<': Sender.wrtStr(ltStr);
    '>': Sender.wrtStr(gtStr); // Required only in ']]>' literal, otherwise optional
    '&': Sender.wrtStr(AmpStr);
    #13:
      begin
        // We normalize #13#10 and #13 to FLineBreak, going somewhat
        // beyond the specs here, see issue #13879.
        Sender.wrtStr(Sender.FLineBreak);
        if (idx < Length(s)) and (s[idx+1] = #10) then
          Inc(idx);
      end;
    #10: Sender.wrtStr(Sender.FLineBreak);
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure TextnodeCanonicalCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  case s[idx] of
    '<': Sender.wrtStr(ltStr);
    '>': Sender.wrtStr(gtStr);
    '&': Sender.wrtStr(AmpStr);
    #13: Sender.wrtStr('&#xD;')
  else
    Sender.wrtChr(s[idx]);
  end;
end;

procedure CDSectSpecialCharCallback(Sender: TXMLWriter; const s: DOMString;
  var idx: Integer);
begin
  if (idx <= Length(s)-2) and (s[idx+1] = ']') and (s[idx+2] = '>') then
  begin
    Sender.wrtStr(']]]]><![CDATA[>');
    Inc(idx, 2);
    // TODO: emit warning 'cdata-section-splitted'
  end
  else
    Sender.wrtChr(s[idx]);
end;

{$IFDEF FPC}
const
  TextnodeCallbacks: array[boolean] of TSpecialCharCallback = (
    @TextnodeNormalCallback,
    @TextnodeCanonicalCallback
  );
{$ENDIF}

procedure TXMLWriter.wrtQuotedLiteral(const ws: WideString);
var
  Quote: WideChar;
begin
  // TODO: need to check if the string also contains single quote
  // both quotes present is a error
  if Pos('"', ws) > 0 then
    Quote := ''''
  else
    Quote := '"';
  wrtChr(Quote);
  ConvWrite(ws, LineEndingChars, @TextnodeNormalCallback);
  wrtChr(Quote);
end;

procedure TXMLWriter.WriteNode(node: TDOMNode);
begin
  case node.NodeType of
    ELEMENT_NODE:                VisitElement(node);
    ATTRIBUTE_NODE:              VisitAttribute(node);
    TEXT_NODE:                   VisitText(node);
    CDATA_SECTION_NODE:          VisitCDATA(node);
    ENTITY_REFERENCE_NODE:       VisitEntityRef(node);
    PROCESSING_INSTRUCTION_NODE: VisitPI(node);
    COMMENT_NODE:                VisitComment(node);
    DOCUMENT_NODE:
      if FCanonical then
        VisitDocument_Canonical(node)
      else
        VisitDocument(node);
    DOCUMENT_TYPE_NODE:          VisitDocumentType(node);
    ENTITY_NODE,
    DOCUMENT_FRAGMENT_NODE:      VisitFragment(node);
  end;
end;

procedure TXMLWriter.WriteNSDef(B: TBinding);
begin
  wrtChars(' xmlns', 6);
  if B.Prefix^.Key <> '' then
  begin
    wrtChr(':');
    wrtStr(B.Prefix^.Key);
  end;
  wrtChars('="', 2);
  ConvWrite(B.uri, AttrSpecialChars, @AttrSpecialCharCallback);
  wrtChr('"');
end;

// clone of system.FPC_WIDESTR_COMPARE which cannot be called directly
function Compare(const s1, s2: DOMString): integer;
var
  maxi, temp: integer;
begin
  Result := 0;
  if pointer(S1) = pointer(S2) then
    exit;
  maxi := Length(S1);
  temp := Length(S2);
  if maxi > temp then
    maxi := temp;
{$IFDEF FPC}
  Result := CompareWord(S1[1], S2[1], maxi);
{$ELSE}
  Result := CompareStr( S1, S2);
{$ENDIF}
  if Result = 0 then
    Result := Length(S1)-Length(S2);
end;

function SortNSDefs(Item1, Item2: Pointer): Integer;
begin
  Result := Compare(TBinding(Item1).Prefix^.Key, TBinding(Item2).Prefix^.Key);
end;

function SortAtts(Item1, Item2: Pointer): Integer;
var
  p1: PAttrFixup absolute Item1;
  p2: PAttrFixup absolute Item2;
  s1, s2: DOMString;
begin
  Result := Compare(p1^.Attr.namespaceURI, p2^.Attr.namespaceURI);
  if Result = 0 then
  begin
    // TODO: Must fix the parser so it doesn't produce Level 1 attributes
    if nfLevel2 in p1^.Attr.Flags then
      s1 := p1^.Attr.localName
    else
      s1 := p1^.Attr.nodeName;
    if nfLevel2 in p2^.Attr.Flags then
      s2 := p2^.Attr.localName
    else
      s2 := p2^.Attr.nodeName;
    Result := Compare(s1, s2);
  end;
end;

procedure TXMLWriter.NamespaceFixup(Element: TDOMElement);
var
  B: TBinding;
  i, j: Integer;
  node: TDOMNode;
  s: DOMString;
  action: TAttributeAction;
  p: PAttrFixup;
begin
  FScratch.Count := 0;
  FNSDefs.Count := 0;
  if Element.hasAttributes then
  begin
    j := 0;
    for i := 0 to Element.Attributes.Length-1 do
    begin
      node := Element.Attributes[i];
      if TDOMNode_NS(node).NSI.NSIndex = 2 then
      begin
        if TDOMNode_NS(node).NSI.PrefixLen = 0 then
          s := ''
        else
          s := node.localName;
        FNSHelper.DefineBinding(s, node.nodeValue, B);
        if Assigned(B) then  // drop redundant namespace declarations
          FNSDefs.Add(B);
      end
      else if FCanonical or TDOMAttr(node).Specified then
      begin
        // obtain a TAttrFixup record (allocate if needed)
        if j >= FAttrFixups.Count then
        begin
          New(p);
          FAttrFixups.Add(p);
        end
        else
{$IFDEF FPC}
          p := PAttrFixup(FAttrFixups.List^[j]);
{$ELSE}
          p := PAttrFixup(FAttrFixups[j]);
{$ENDIF}
        // add it to the working list
        p^.Attr := node;
        p^.Prefix := nil;
        FScratch.Add(p);
        Inc(j);
      end;
    end;
  end;

  FNSHelper.DefineBinding(Element.Prefix, Element.namespaceURI, B);
  if Assigned(B) then
    FNSDefs.Add(B);

  for i := 0 to FScratch.Count-1 do
  begin
{$IFDEF FPC}
    node := PAttrFixup(FScratch.List^[i])^.Attr;
{$ELSE}
    node := PAttrFixup(FScratch[i])^.Attr;
{$ENDIF}
    action := FNSHelper.CheckAttribute(node.Prefix, node.namespaceURI, B);
    if action = aaBoth then
      FNSDefs.Add(B);

    if action in [aaPrefix, aaBoth] then
{$IFDEF FPC}
      PAttrFixup(FScratch.List^[i])^.Prefix := B.Prefix;
{$ELSE}
      PAttrFixup(FScratch[i])^.Prefix := B.Prefix;
{$ENDIF}
  end;

  if FCanonical then
  begin
    FNSDefs.Sort(@SortNSDefs);
    FScratch.Sort(@SortAtts);
  end;

  // now, at last, dump all this stuff.
  for i := 0 to FNSDefs.Count-1 do
{$IFDEF FPC}
    WriteNSDef(TBinding(FNSDefs.List^[I]));
{$ELSE}
    WriteNSDef(TBinding(FNSDefs[I]));
{$ENDIF}

  for i := 0 to FScratch.Count-1 do
  begin
    wrtChr(' ');
{$IFDEF FPC}
    with PAttrFixup(FScratch.List^[I])^ do
{$ELSE}
    with PAttrFixup(FScratch[I])^ do
{$ENDIF}
    begin
      if Assigned(Prefix) then
      begin
        wrtStr(Prefix^.Key);
        wrtChr(':');
        wrtStr(Attr.localName);
      end
      else
        wrtStr(Attr.nodeName);

      wrtChars('="', 2);
      // TODO: not correct w.r.t. entities
      ConvWrite(attr.nodeValue, AttrSpecialChars, @AttrSpecialCharCallback);
      wrtChr('"');
    end;
  end;
end;

procedure TXMLWriter.VisitElement(node: TDOMNode);
var
  i: Integer;
  child: TDOMNode;
  SavedInsideTextNode: Boolean;
begin
  if not FInsideTextNode then
    wrtIndent;
  FNSHelper.StartElement;
  wrtChr('<');
  wrtStr(TDOMElement(node).TagName);

  if nfLevel2 in node.Flags then
    NamespaceFixup(TDOMElement(node))
  else if node.HasAttributes then
    for i := 0 to node.Attributes.Length - 1 do
    begin
      child := node.Attributes.Item[i];
      if FCanonical or TDOMAttr(child).Specified then
        VisitAttribute(child);
    end;
  Child := node.FirstChild;
  if Child = nil then
    wrtChars('/>', 2)
  else
  begin
    // TODO: presence of zero-length textnodes triggers the indenting logic,
    // while they should be ignored altogeter.
    SavedInsideTextNode := FInsideTextNode;
    wrtChr('>');
    FInsideTextNode := FCanonical or (Child.NodeType in [TEXT_NODE, CDATA_SECTION_NODE]);
    IncIndent;
    repeat
      WriteNode(Child);
      Child := Child.NextSibling;
    until Child = nil;
    DecIndent;
    if not (node.LastChild.NodeType in [TEXT_NODE, CDATA_SECTION_NODE]) then
      wrtIndent;
    FInsideTextNode := SavedInsideTextNode;
    wrtChars('</', 2);
    wrtStr(TDOMElement(Node).TagName);
    wrtChr('>');
  end;
  FNSHelper.EndElement;
end;

procedure TXMLWriter.VisitText(node: TDOMNode);
begin
{$IFDEF FPC}
  ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, TextnodeCallbacks[FCanonical]);
{$ELSE}
  if FCanonical then
    ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, @TextnodeCanonicalCallback)
  else
    ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, @TextnodeNormalCallback);
{$ENDIF}
end;

procedure TXMLWriter.VisitCDATA(node: TDOMNode);
begin
  if not FInsideTextNode then
    wrtIndent;
  if FCanonical then
    ConvWrite(TDOMCharacterData(node).Data, TextSpecialChars, @TextnodeCanonicalCallback)
  else
  begin
    wrtChars('<![CDATA[', 9);
    ConvWrite(TDOMCharacterData(node).Data, CDSectSpecialChars, @CDSectSpecialCharCallback);
    wrtChars(']]>', 3);
  end;
end;

procedure TXMLWriter.VisitEntityRef(node: TDOMNode);
begin
  wrtChr('&');
  wrtStr(node.NodeName);
  wrtChr(';');
end;

procedure TXMLWriter.VisitPI(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtStr('<?');
  wrtStr(TDOMProcessingInstruction(node).Target);
  if TDOMProcessingInstruction(node).Data <> '' then
  begin
    wrtChr(' ');
    // TODO: How does this comply with c14n??
    ConvWrite(TDOMProcessingInstruction(node).Data, LineEndingChars, @TextnodeNormalCallback);
  end;
  wrtStr('?>');
end;

procedure TXMLWriter.VisitComment(node: TDOMNode);
begin
  if not FInsideTextNode then wrtIndent;
  wrtChars('<!--', 4);
  // TODO: How does this comply with c14n??
  ConvWrite(TDOMCharacterData(node).Data, LineEndingChars, @TextnodeNormalCallback);
  wrtChars('-->', 3);
end;

procedure TXMLWriter.VisitDocument(node: TDOMNode);
var
  child: TDOMNode;
begin
  wrtStr('<?xml version="');
  // Definitely should not escape anything here
  if Length(TXMLDocument(node).XMLVersion) > 0 then
    wrtStr(TXMLDocument(node).XMLVersion)
  else
    wrtStr('1.0');
  wrtChr('"');
  
// DISABLED - we are only able write in UTF-8 which does not require labeling
// writing incorrect encoding will render xml unreadable...
(*
  if Length(TXMLDocument(node).Encoding) > 0 then
  begin
    wrtStr(' encoding="');
    wrtStr(TXMLDocument(node).Encoding);
    wrtChr('"');
  end;
*)
  wrtStr('?>');

  // TODO: now handled as a regular PI, remove this?
  if node is TXMLDocument then
  begin
    if Length(TXMLDocument(node).StylesheetType) > 0 then
    begin
      wrtStr(FLineBreak);
      wrtStr('<?xml-stylesheet type="');
      wrtStr(TXMLDocument(node).StylesheetType);
      wrtStr('" href="');
      wrtStr(TXMLDocument(node).StylesheetHRef);
      wrtStr('"?>');
    end;
  end;

  child := node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
  wrtStr(FLineBreak);
end;

procedure TXMLWriter.VisitDocument_Canonical(Node: TDOMNode);
var
  child, root: TDOMNode;
begin
  root := TDOMDocument(Node).DocumentElement;
  child := node.FirstChild;
  while Assigned(child) and (child <> root) do
  begin
    if child.nodeType in [COMMENT_NODE, PROCESSING_INSTRUCTION_NODE] then
    begin
      WriteNode(child);
      wrtChr(#10);
    end;
    child := child.nextSibling;
  end;
  if root = nil then
    Exit;
  VisitElement(TDOMElement(root));
  child := root.nextSibling;
  while Assigned(child) do
  begin
    if child.nodeType in [COMMENT_NODE, PROCESSING_INSTRUCTION_NODE] then
    begin
      wrtChr(#10);
      WriteNode(child);
    end;
    child := child.nextSibling;
  end;
end;

procedure TXMLWriter.VisitAttribute(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  wrtChr(' ');
  wrtStr(TDOMAttr(Node).Name);
  wrtChars('="', 2);
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    case Child.NodeType of
      ENTITY_REFERENCE_NODE:
        VisitEntityRef(Child);
      TEXT_NODE:
        ConvWrite(TDOMCharacterData(Child).Data, AttrSpecialChars, @AttrSpecialCharCallback);
    end;
    Child := Child.NextSibling;
  end;
  wrtChr('"');
end;

procedure TXMLWriter.VisitDocumentType(Node: TDOMNode);
begin
  wrtStr(FLineBreak);
  wrtStr('<!DOCTYPE ');
  wrtStr(Node.NodeName);
  wrtChr(' ');
  with TDOMDocumentType(Node) do
  begin
    if PublicID <> '' then
    begin
      wrtStr('PUBLIC ');
      wrtQuotedLiteral(PublicID);
      wrtChr(' ');
      wrtQuotedLiteral(SystemID);
    end
    else if SystemID <> '' then
    begin
      wrtStr('SYSTEM ');
      wrtQuotedLiteral(SystemID);
    end;
    if InternalSubset <> '' then
    begin
      wrtChr('[');
      ConvWrite(InternalSubset, LineEndingChars, @TextnodeNormalCallback);
      wrtChr(']');
    end;
  end;
  wrtChr('>');
end;

procedure TXMLWriter.VisitFragment(Node: TDOMNode);
var
  Child: TDOMNode;
begin
  // TODO: TextDecl is probably needed
  // Fragment itself should not be written, only its children should...
  Child := Node.FirstChild;
  while Assigned(Child) do
  begin
    WriteNode(Child);
    Child := Child.NextSibling;
  end;
end;


// -------------------------------------------------------------------
//   Interface implementation
// -------------------------------------------------------------------

procedure WriteXMLFile(doc: TXMLDocument; const AFileName: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    WriteXMLFile(doc, fs);
  finally
    fs.Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; var AFile: Text);
var
  s: TStream;
begin
  s := TTextStream.Create(AFile);
  try
    with TXMLWriter.Create(s) do
    try
      WriteNode(doc);
    finally
      Free;
    end;
  finally
    s.Free;
  end;
end;

procedure WriteXMLFile(doc: TXMLDocument; AStream: TStream);
begin
  with TXMLWriter.Create(AStream) do
  try
    WriteNode(doc);
  finally
    Free;
  end;
end;

procedure WriteXML(Element: TDOMNode; const AFileName: String);
begin
  WriteXMLFile(TXMLDocument(Element), AFileName);
end;

procedure WriteXML(Element: TDOMNode; var AFile: Text);
begin
  WriteXMLFile(TXMLDocument(Element), AFile);
end;

procedure WriteXML(Element: TDOMNode; AStream: TStream);
begin
  WriteXMLFile(TXMLDocument(Element), AStream);
end;



end.
