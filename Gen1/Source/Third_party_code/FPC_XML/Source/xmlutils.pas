{
    This file is part of the Free Component Library

    XML utility routines.
    Copyright (c) 2006 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit xmlutils;

{$ifdef fpc}{$mode objfpc}{$endif}
{$H+}
{$ifopt Q+}{$define overflow_check}{$endif}

interface

uses
  SysUtils, Classes;

function IsXmlName(const Value: WideString; Xml11: Boolean = False): Boolean; overload;
function IsXmlName(Value: PWideChar; Len: Integer; Xml11: Boolean = False): Boolean; overload;
function IsXmlNames(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsXmlNmToken(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsXmlNmTokens(const Value: WideString; Xml11: Boolean = False): Boolean;
function IsValidXmlEncoding(const Value: WideString): Boolean;
function Xml11NamePages: PByteArray;
procedure NormalizeSpaces(var Value: WideString);
function IsXmlWhiteSpace(c: WideChar): Boolean;
function Hash(InitValue: LongWord; Key: PWideChar; KeyLen: Integer): LongWord;
{ beware, works in ASCII range only }
function WStrLIComp(S1, S2: PWideChar; Len: Integer): Integer;
procedure WStrLower(var S: WideString);

type
  TXMLVersion = (xmlVersionUnknown, xmlVersion10, xmlVersion11);

const
  xmlVersionStr: array[TXMLVersion] of WideString = ('', '1.0', '1.1');
  
{ a simple hash table with WideString keys }

type
{$ifndef fpc}
  PtrInt = LongInt;
  TFPList = TList;
{$endif}  

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Key: WideString;
    HashValue: LongWord;
    Next: PHashItem;
    Data: TObject;
  end;
  THashItemArray = array[0..MaxInt div sizeof(Pointer)-1] of PHashItem;
  PHashItemArray = ^THashItemArray;

  THashForEach = function(Entry: PHashItem; arg: Pointer): Boolean;

  THashTable = class(TObject)
  private
    FCount: LongWord;
    FBucketCount: LongWord;
    FBucket: PHashItemArray;
    FOwnsObjects: Boolean;
    function Lookup(Key: PWideChar; KeyLength: Integer; out Found: Boolean; CanCreate: Boolean): PHashItem;
    procedure Resize(NewCapacity: LongWord);
  public
    constructor Create(InitSize: Integer; OwnObjects: Boolean);
    destructor Destroy; override;
    procedure Clear;
    function Find(Key: PWideChar; KeyLen: Integer): PHashItem;
    function FindOrAdd(Key: PWideChar; KeyLen: Integer; var Found: Boolean): PHashItem; overload;
    function FindOrAdd(Key: PWideChar; KeyLen: Integer): PHashItem; overload;
    function Get(Key: PWideChar; KeyLen: Integer): TObject;
    function Remove(Entry: PHashItem): Boolean;
    function RemoveData(aData: TObject): Boolean;
    procedure ForEach(proc: THashForEach; arg: Pointer);
    property Count: LongWord read FCount;
  end;

{ another hash, for detecting duplicate namespaced attributes without memory allocations }

  TExpHashEntry = record
    rev: LongWord;
    hash: LongWord;
    uriPtr: PWideString;
    lname: PWideChar;
    lnameLen: Integer;
  end;
  TExpHashEntryArray = array[0..MaxInt div sizeof(TExpHashEntry)-1] of TExpHashEntry;
  PExpHashEntryArray = ^TExpHashEntryArray;

  TDblHashArray = class(TObject)
  private
    FSizeLog: Integer;
    FRevision: LongWord;
    FData: PExpHashEntryArray;
  public  
    procedure Init(NumSlots: Integer);
    function Locate(uri: PWideString; localName: PWideChar; localLength: Integer): Boolean;
    destructor Destroy; override;
  end;

  TBinding = class
  public
    uri: WideString;
    next: TBinding;
    prevPrefixBinding: TObject;
    Prefix: PHashItem;
  end;

  TAttributeAction = (
    aaUnchanged,
    aaPrefix,         // only override the prefix
    aaBoth            // override prefix and emit namespace definition
  );

  TNSSupport = class(TObject)
  private
    FNesting: Integer;
    FPrefixSeqNo: Integer;
    FFreeBindings: TBinding;
    FBindings: TFPList;
    FBindingStack: array of TBinding;
    FPrefixes: THashTable;
    FDefaultPrefix: THashItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DefineBinding(const Prefix, nsURI: WideString; out Binding: TBinding);
    function CheckAttribute(const Prefix, nsURI: WideString;
      out Binding: TBinding): TAttributeAction;
    function IsPrefixBound(P: PWideChar; Len: Integer; out Prefix: PHashItem): Boolean;
    function GetPrefix(P: PWideChar; Len: Integer): PHashItem;
    function BindPrefix(const nsURI: WideString; aPrefix: PHashItem): TBinding;
    function DefaultNSBinding: TBinding;
    procedure StartElement;
    procedure EndElement;
  end;

{ Buffer builder, used to compose long strings without too much memory allocations }

  PWideCharBuf = ^TWideCharBuf;
  TWideCharBuf = record
    Buffer: PWideChar;
    Length: Integer;
    MaxLength: Integer;
  end;

procedure BufAllocate(var ABuffer: TWideCharBuf; ALength: Integer);
procedure BufAppend(var ABuffer: TWideCharBuf; wc: WideChar);
procedure BufAppendChunk(var ABuf: TWideCharBuf; pstart, pend: PWideChar);
function BufEquals(const ABuf: TWideCharBuf; const Arg: WideString): Boolean;

{ Built-in decoder functions for UTF-8, UTF-16 and ISO-8859-1 }

function Decode_UCS2(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
function Decode_UCS2_Swapped(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
function Decode_UTF8(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
function Decode_8859_1(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;

{$i names.inc}

implementation

var
  Xml11Pg: PByteArray = nil;

{$if CompilerVersion <= 18.5}
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}

function Xml11NamePages: PByteArray;
var
  I: Integer;
  p: PByteArray;
begin
  if Xml11Pg = nil then
  begin
    GetMem(p, 512);
    for I := 0 to 255 do
      p^[I] := ord(Byte(I) in Xml11HighPages);
    p^[0] := 2;
    p^[3] := $2c;
    p^[$20] := $2a;
    p^[$21] := $2b;
    p^[$2f] := $29;
    p^[$30] := $2d;
    p^[$fd] := $28;
    p^[$ff] := $30;

    Move(p^, p^[256], 256);
    p^[$100] := $19;
    p^[$103] := $2E;
    p^[$120] := $2F;
    Xml11Pg := p;
  end;
  Result := Xml11Pg;
end;

function IsXml11Char(Value: PWideChar; var Index: Integer): Boolean; overload;
begin
  if (Value[Index] >= #$D800) and (Value[Index] <= #$DB7F) then
  begin
    Inc(Index);
    Result := (Value[Index] >= #$DC00) and (Value[Index] <= #$DFFF);
  end
  else
    Result := False;
end;

function IsXml11Char(const Value: WideString; var Index: Integer): Boolean; overload;
begin
  if (Value[Index] >= #$D800) and (Value[Index] <= #$DB7F) then
  begin
    Inc(Index);
    Result := (Value[Index] >= #$DC00) and (Value[Index] <= #$DFFF);
  end
  else
    Result := False;
end;

function IsXmlName(const Value: WideString; Xml11: Boolean): Boolean;
begin
  Result := IsXmlName(PWideChar(Value), Length(Value), Xml11);
end;

function IsXmlName(Value: PWideChar; Len: Integer; Xml11: Boolean = False): Boolean;
var
  Pages: PByteArray;
  I: Integer;
begin
  Result := False;
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;

  I := 0;
  if (Len = 0) or not ((Byte(Value[I]) in NamingBitmap[Pages^[hi(Word(Value[I]))]]) or
    (Value[I] = ':') or
    (Xml11 and IsXml11Char(Value, I))) then
      Exit;
  Inc(I);
  while I < Len do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Value[I] = ':') or
      (Xml11 and IsXml11Char(Value, I))) then
        Exit;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNames(const Value: WideString; Xml11: Boolean): Boolean;
var
  Pages: PByteArray;
  I: Integer;
  Offset: Integer;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  Result := False;
  if Value = '' then
    Exit;
  I := 1;
  Offset := 0;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[Offset+hi(Word(Value[I]))]]) or
      (Value[I] = ':') or
      (Xml11 and IsXml11Char(Value, I))) then
    begin
      if (I = Length(Value)) or (Value[I] <> #32) then
        Exit;
      Offset := 0;
      Inc(I);
      Continue;
    end;
    Offset := $100;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNmToken(const Value: WideString; Xml11: Boolean): Boolean;
var
  I: Integer;
  Pages: PByteArray;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  Result := False;
  if Value = '' then
    Exit;
  I := 1;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Value[I] = ':') or
      (Xml11 and IsXml11Char(Value, I))) then
        Exit;
    Inc(I);
  end;
  Result := True;
end;

function IsXmlNmTokens(const Value: WideString; Xml11: Boolean): Boolean;
var
  I: Integer;
  Pages: PByteArray;
begin
  if Xml11 then
    Pages := Xml11NamePages
  else
    Pages := @NamePages;
  I := 1;
  Result := False;
  if Value = '' then
    Exit;
  while I <= Length(Value) do
  begin
    if not ((Byte(Value[I]) in NamingBitmap[Pages^[$100+hi(Word(Value[I]))]]) or
      (Value[I] = ':') or
      (Xml11 and IsXml11Char(Value, I))) then
    begin
      if (I = Length(Value)) or (Value[I] <> #32) then
        Exit;
    end;
    Inc(I);
  end;
  Result := True;
end;

function IsValidXmlEncoding(const Value: WideString): Boolean;
var
  I: Integer;
begin
  Result := False;
  //if (Value = '') or (Value[1] > #255) or not (char(ord(Value[1])) in ['A'..'Z', 'a'..'z']) then
  if (Value = '') or (Value[1] > #255) or not CharInSet(Value[1], ['A'..'Z', 'a'..'z']) then
    Exit;
  for I := 2 to Length(Value) do
    //if (Value[I] > #255) or not (char(ord(Value[I])) in ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) then
    if (Value[I] > #255) or not CharInSet(Value[I], ['A'..'Z', 'a'..'z', '0'..'9', '.', '_', '-']) then
      Exit;
  Result := True;
end;

procedure NormalizeSpaces(var Value: WideString);
var
  I, J: Integer;
begin
  I := Length(Value);
  // speed: trim only whed needed
  if (I > 0) and ((Value[1] = #32) or (Value[I] = #32)) then
    Value := Trim(Value);
  I := 1;
  while I < Length(Value) do
  begin
    if Value[I] = #32 then
    begin
      J := I+1;
      while (J <= Length(Value)) and (Value[J] = #32) do Inc(J);
      if J-I > 1 then Delete(Value, I+1, J-I-1);
    end;
    Inc(I);
  end;
end;

function IsXmlWhiteSpace(c: WideChar): Boolean;
begin
  Result := (c = #32) or (c = #9) or (c = #10) or (c = #13);
end;

function WStrLIComp(S1, S2: PWideChar; Len: Integer): Integer;
var
  counter: Integer;
  c1, c2: Word;
begin
  counter := 0;
  result := 0;
  if Len = 0 then
    exit;
  repeat
    c1 := ord(S1[counter]);
    c2 := ord(S2[counter]);
    if (c1 = 0) or (c2 = 0) then break;
    if c1 <> c2 then
    begin
      if c1 in [97..122] then
        Dec(c1, 32);
      if c2 in [97..122] then
        Dec(c2, 32);
      if c1 <> c2 then
        Break;
    end;
    Inc(counter);
  until counter >= Len;
  result := c1 - c2;
end;

procedure WStrLower(var S: WideString);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if (S[i] >= 'A') and (S[i] <= 'Z') then
      Inc(word(S[i]), 32);
end;

function Hash(InitValue: LongWord; Key: PWideChar; KeyLen: Integer): LongWord;
begin
  Result := InitValue;
  while KeyLen <> 0 do
  begin
{$ifdef overflow_check}{$q-}{$endif}
    Result := Result * $F4243 xor ord(Key^);
{$ifdef overflow_check}{$q+}{$endif}
    Inc(Key);
    Dec(KeyLen);
  end;
end;

function KeyCompare(const Key1: WideString; Key2: Pointer; Key2Len: Integer): Boolean;
begin
{$IFDEF FPC}
  Result := (Length(Key1)=Key2Len) and (CompareWord(Pointer(Key1)^, Key2^, Key2Len) = 0);
{$ELSE}
  Result := (Length(Key1)=Key2Len) and CompareMem(Pointer(Key1), Key2, Key2Len*2);
{$ENDIF}
end;

{ THashTable }

constructor THashTable.Create(InitSize: Integer; OwnObjects: Boolean);
var
  I: Integer;
begin
  inherited Create;
  FOwnsObjects := OwnObjects;
  I := 256;
  while I < InitSize do I := I shl 1;
  FBucketCount := I;
  FBucket := AllocMem(I * sizeof(PHashItem));
end;

destructor THashTable.Destroy;
begin
  Clear;
  FreeMem(FBucket);
  inherited Destroy;
end;

procedure THashTable.Clear;
var
  I: Integer;
  item, next: PHashItem;
begin
  for I := 0 to FBucketCount-1 do
  begin
    item := FBucket^[I];
    while Assigned(item) do
    begin
      next := item^.Next;
      if FOwnsObjects then
        item^.Data.Free;
      Dispose(item);
      item := next;
    end;
    FBucket^[I] := nil;
  end;
end;

function THashTable.Find(Key: PWideChar; KeyLen: Integer): PHashItem;
var
  Dummy: Boolean;
begin
  Result := Lookup(Key, KeyLen, Dummy, False);
end;

function THashTable.FindOrAdd(Key: PWideChar; KeyLen: Integer;
  var Found: Boolean): PHashItem;
begin
  Result := Lookup(Key, KeyLen, Found, True);
end;

function THashTable.FindOrAdd(Key: PWideChar; KeyLen: Integer): PHashItem;
var
  Dummy: Boolean;
begin
  Result := Lookup(Key, KeyLen, Dummy, True);
end;

function THashTable.Get(Key: PWideChar; KeyLen: Integer): TObject;
var
  e: PHashItem;
  Dummy: Boolean;
begin
  e := Lookup(Key, KeyLen, Dummy, False);
  if Assigned(e) then
    Result := e^.Data
  else
    Result := nil;  
end;

function THashTable.Lookup(Key: PWideChar; KeyLength: Integer;
  out Found: Boolean; CanCreate: Boolean): PHashItem;
var
  Entry: PPHashItem;
  h: LongWord;
begin
  h := Hash(0, Key, KeyLength);
  Entry := @FBucket^[h mod FBucketCount];
  while Assigned(Entry^) and not ((Entry^^.HashValue = h) and KeyCompare(Entry^^.Key, Key, KeyLength) ) do
    Entry := @Entry^^.Next;
  Found := Assigned(Entry^);
  if Found or (not CanCreate) then
  begin
    Result := Entry^;
    Exit;
  end;
  if FCount > FBucketCount then  { arbitrary limit, probably too high }
  begin
    Resize(FBucketCount * 2);
    Result := Lookup(Key, KeyLength, Found, CanCreate);
  end
  else
  begin
    New(Result);
    // SetString for WideStrings trims on zero chars [fixed, #14740]
    SetLength(Result^.Key, KeyLength);
    Move(Key^, Pointer(Result^.Key)^, KeyLength*sizeof(WideChar));
    Result^.HashValue := h;
    Result^.Data := nil;
    Result^.Next := nil;
    Inc(FCount);
    Entry^ := Result;
  end;
end;

procedure THashTable.Resize(NewCapacity: LongWord);
var
  p: PHashItemArray;
  chain: PPHashItem;
  i: Integer;
  e, n: PHashItem;
begin
  p := AllocMem(NewCapacity * sizeof(PHashItem));
  for i := 0 to FBucketCount-1 do
  begin
    e := FBucket^[i];
    while Assigned(e) do
    begin
      chain := @p^[e^.HashValue mod NewCapacity];
      n := e^.Next;
      e^.Next := chain^;
      chain^ := e;
      e := n;
    end;
  end;
  FBucketCount := NewCapacity;
  FreeMem(FBucket);
  FBucket := p;
end;

function THashTable.Remove(Entry: PHashItem): Boolean;
var
  chain: PPHashItem;
begin
  chain := @FBucket^[Entry^.HashValue mod FBucketCount];
  while Assigned(chain^) do
  begin
    if chain^ = Entry then
    begin
      chain^ := Entry^.Next;
      if FOwnsObjects then
        Entry^.Data.Free;
      Dispose(Entry);
      Dec(FCount);
      Result := True;
      Exit;
    end;
    chain := @chain^^.Next;
  end;
  Result := False;
end;

// this does not free the aData object
function THashTable.RemoveData(aData: TObject): Boolean;
var
  i: Integer;
  chain: PPHashItem;
  e: PHashItem;
begin
  for i := 0 to FBucketCount-1 do
  begin
    chain := @FBucket^[i];
    while Assigned(chain^) do
    begin
      if chain^^.Data = aData then
      begin
        e := chain^;
        chain^ := e^.Next;
        Dispose(e);
        Dec(FCount);
        Result := True;
        Exit;
      end;
      chain := @chain^^.Next;
    end;
  end;
  Result := False;
end;

procedure THashTable.ForEach(proc: THashForEach; arg: Pointer);
var
  i: Integer;
  e: PHashItem;
begin
  for i := 0 to FBucketCount-1 do
  begin
    e := FBucket^[i];
    while Assigned(e) do
    begin
      if not proc(e, arg) then
        Exit;
      e := e^.Next;
    end;
  end;
end;

{ TDblHashArray }

destructor TDblHashArray.Destroy;
begin
  FreeMem(FData);
  inherited Destroy;
end;

procedure TDblHashArray.Init(NumSlots: Integer);
var
  i: Integer;
begin
  if ((NumSlots * 2) shr FSizeLog) <> 0 then   // need at least twice more entries, and no less than 8
  begin
    FSizeLog := 3;
    while (NumSlots shr FSizeLog) <> 0 do
      Inc(FSizeLog);
    ReallocMem(FData, (1 shl FSizeLog) * sizeof(TExpHashEntry));
    FRevision := 0;
  end;
  if FRevision = 0 then
  begin
    FRevision := $FFFFFFFF;
    for i := (1 shl FSizeLog)-1 downto 0 do
      FData^[i].rev := FRevision;
  end;
  Dec(FRevision);
end;

function TDblHashArray.Locate(uri: PWideString; localName: PWideChar; localLength: Integer): Boolean;
var
  step: Byte;
  mask: LongWord;
  idx: Integer;
  HashValue: LongWord;
begin
  HashValue := Hash(0, PWideChar(uri^), Length(uri^));
  HashValue := Hash(HashValue, localName, localLength);

  mask := (1 shl FSizeLog) - 1;
  step := (HashValue and (not mask)) shr (FSizeLog-1) and (mask shr 2) or 1;
  idx := HashValue and mask;
  result := True;
  while FData^[idx].rev = FRevision do
  begin
    if (HashValue = FData^[idx].hash) and (FData^[idx].uriPtr^ = uri^) and
      (FData^[idx].lnameLen = localLength) and
       CompareMem(FData^[idx].lname, localName, localLength * sizeof(WideChar)) then
      Exit;
    if idx < step then
      Inc(idx, (1 shl FSizeLog) - step)
    else
      Dec(idx, step);
  end;
  with FData^[idx] do
  begin
    rev := FRevision;
    hash := HashValue;
    uriPtr := uri;
    lname := localName;
    lnameLen := localLength;
  end;
  result := False;
end;

{ TNSSupport }

constructor TNSSupport.Create;
var
  b: TBinding;
begin
  inherited Create;
  FPrefixes := THashTable.Create(16, False);
  FBindings := TFPList.Create;
  SetLength(FBindingStack, 16);

  { provide implicit binding for the 'xml' prefix }
  // TODO: move stduri_xml, etc. to this unit, so they are reused.
  DefineBinding('xml', 'http://www.w3.org/XML/1998/namespace', b);
end;

destructor TNSSupport.Destroy;
var
  I: Integer;
begin
  for I := FBindings.Count-1 downto 0 do
{$IFDEF FPC}
    TObject(FBindings.List^[I]).Free;
{$ELSE}
    TObject(FBindings[I]).Free;
{$ENDIF}
  FBindings.Free;
  FPrefixes.Free;
  inherited Destroy;
end;

function TNSSupport.BindPrefix(const nsURI: WideString; aPrefix: PHashItem): TBinding;
begin
  { try to reuse an existing binding }
  result := FFreeBindings;
  if Assigned(result) then
    FFreeBindings := result.Next
  else { no free bindings, create a new one }
  begin
    result := TBinding.Create;
    FBindings.Add(result);
  end;

  { link it into chain of bindings at the current element level }
  result.Next := FBindingStack[FNesting];
  FBindingStack[FNesting] := result;

  { bind }
  result.uri := nsURI;
  result.Prefix := aPrefix;
  result.PrevPrefixBinding := aPrefix^.Data;
  aPrefix^.Data := result;
end;

function TNSSupport.DefaultNSBinding: TBinding;
begin
  result := TBinding(FDefaultPrefix.Data);
end;

procedure TNSSupport.DefineBinding(const Prefix, nsURI: WideString;
  out Binding: TBinding);
var
  Pfx: PHashItem;
begin
  Pfx := @FDefaultPrefix;
  if (nsURI <> '') and (Prefix <> '') then
    Pfx := FPrefixes.FindOrAdd(PWideChar(Prefix), Length(Prefix));
  if (Pfx^.Data = nil) or (TBinding(Pfx^.Data).uri <> nsURI) then
    Binding := BindPrefix(nsURI, Pfx)
  else
    Binding := nil;
end;

function TNSSupport.CheckAttribute(const Prefix, nsURI: WideString;
  out Binding: TBinding): TAttributeAction;
var
  Pfx: PHashItem;
  I: Integer;
  b: TBinding;
  buf: array[0..31] of WideChar;
  p: PWideChar;
begin
  Binding := nil;
  Pfx := nil;
  Result := aaUnchanged;
  if Prefix <> '' then
    Pfx := FPrefixes.FindOrAdd(PWideChar(Prefix), Length(Prefix))
  else if nsURI = '' then
    Exit;
  { if the prefix is already bound to correct URI, we're done }
  if Assigned(Pfx) and Assigned(Pfx^.Data) and (TBinding(Pfx^.Data).uri = nsURI) then
    Exit;

  { see if there's another prefix bound to the target URI }
  // TODO: should use something faster than linear search
  for i := FNesting downto 0 do
  begin
    b := FBindingStack[i];
    while Assigned(b) do
    begin
      if (b.uri = nsURI) and (b.Prefix <> @FDefaultPrefix) then
      begin
        Binding := b;   // found one -> override the attribute's prefix
        Result := aaPrefix;
        Exit;
      end;
      b := b.Next;
    end;
  end;
  { no prefix, or bound (to wrong URI) -> use generated prefix instead }
  if (Pfx = nil) or Assigned(Pfx^.Data) then
  repeat
    Inc(FPrefixSeqNo);
    i := FPrefixSeqNo;    // This is just 'NS'+IntToStr(FPrefixSeqNo);
    p := @Buf[high(Buf)]; // done without using strings
    while i <> 0 do
    begin
      p^ := WideChar(i mod 10+ord('0'));
      dec(p);
      i := i div 10;
    end;
    p^ := 'S'; dec(p);
    p^ := 'N';
    Pfx := FPrefixes.FindOrAdd(p, @Buf[high(Buf)]-p+1);
  until Pfx^.Data = nil;
  Binding := BindPrefix(nsURI, Pfx);
  Result := aaBoth;
end;

function TNSSupport.IsPrefixBound(P: PWideChar; Len: Integer; out
  Prefix: PHashItem): Boolean;
begin
  Prefix := FPrefixes.FindOrAdd(P, Len);
  Result := Assigned(Prefix^.Data) and (TBinding(Prefix^.Data).uri <> '');
end;

function TNSSupport.GetPrefix(P: PWideChar; Len: Integer): PHashItem;
begin
  if Assigned(P) and (Len > 0) then
    Result := FPrefixes.FindOrAdd(P, Len)
  else
    Result := @FDefaultPrefix;
end;

procedure TNSSupport.StartElement;
begin
  Inc(FNesting);
  if FNesting >= Length(FBindingStack) then
    SetLength(FBindingStack, FNesting * 2);
end;

procedure TNSSupport.EndElement;
var
  b, temp: TBinding;
begin
  temp := FBindingStack[FNesting];
  while Assigned(temp) do
  begin
    b := temp;
    temp := b.next;
    b.next := FFreeBindings;
    FFreeBindings := b;
    b.Prefix^.Data := b.prevPrefixBinding;
  end;
  FBindingStack[FNesting] := nil;
  if FNesting > 0 then
    Dec(FNesting);
end;

{ Buffer builder utils }

procedure BufAllocate(var ABuffer: TWideCharBuf; ALength: Integer);
begin
  ABuffer.MaxLength := ALength;
  ABuffer.Length := 0;
  ABuffer.Buffer := AllocMem(ABuffer.MaxLength*SizeOf(WideChar));
end;

procedure BufAppend(var ABuffer: TWideCharBuf; wc: WideChar);
begin
  if ABuffer.Length >= ABuffer.MaxLength then
  begin
    ReallocMem(ABuffer.Buffer, ABuffer.MaxLength * 2 * SizeOf(WideChar));
    FillChar(ABuffer.Buffer[ABuffer.MaxLength], ABuffer.MaxLength * SizeOf(WideChar),0);
    ABuffer.MaxLength := ABuffer.MaxLength * 2;
  end;
  ABuffer.Buffer[ABuffer.Length] := wc;
  Inc(ABuffer.Length);
end;

procedure BufAppendChunk(var ABuf: TWideCharBuf; pstart, pend: PWideChar);
var
  Len: Integer;
begin
  Len := PEnd - PStart;
  if Len <= 0 then
    Exit;
  if Len >= ABuf.MaxLength - ABuf.Length then
  begin
    ABuf.MaxLength := (Len + ABuf.Length)*2;
    // note: memory clean isn't necessary here.
    // To avoid garbage, control Length field.
    ReallocMem(ABuf.Buffer, ABuf.MaxLength * sizeof(WideChar));
  end;
  Move(pstart^, ABuf.Buffer[ABuf.Length], Len * sizeof(WideChar));
  Inc(ABuf.Length, Len);
end;

function BufEquals(const ABuf: TWideCharBuf; const Arg: WideString): Boolean;
begin
  Result := (ABuf.Length = Length(Arg)) and
    CompareMem(ABuf.Buffer, Pointer(Arg), ABuf.Length*sizeof(WideChar));
end;

{ standard decoders }

function Decode_UCS2(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
var
  cnt: Cardinal;
begin
  cnt := OutCnt;         // num of widechars
  if cnt > InCnt div sizeof(WideChar) then
    cnt := InCnt div sizeof(WideChar);
  Move(InBuf^, OutBuf^, cnt * sizeof(WideChar));
  Dec(InCnt, cnt*sizeof(WideChar));
  Dec(OutCnt, cnt);
  Result := cnt;
end;

function Decode_UCS2_Swapped(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
var
  I: Integer;
  cnt: Cardinal;
  InPtr: PAnsiChar;
begin
  cnt := OutCnt;         // num of widechars
  if cnt > InCnt div sizeof(WideChar) then
    cnt := InCnt div sizeof(WideChar);
  InPtr := InBuf;
  for I := 0 to cnt-1 do
  begin
    OutBuf[I] := WideChar((ord(InPtr^) shl 8) or ord(InPtr[1]));
    Inc(InPtr, 2);
  end;
  Dec(InCnt, cnt*sizeof(WideChar));
  Dec(OutCnt, cnt);
  Result := cnt;
end;

function Decode_8859_1(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
var
  I: Integer;
  cnt: Cardinal;
begin
  cnt := OutCnt;         // num of widechars
  if cnt > InCnt then
    cnt := InCnt;
  for I := 0 to cnt-1 do
    OutBuf[I] := WideChar(ord(InBuf[I]));
  Dec(InCnt, cnt);
  Dec(OutCnt, cnt);
  Result := cnt;
end;

function Decode_UTF8(Context: Pointer; InBuf: PAnsiChar; var InCnt: Cardinal; OutBuf: PWideChar; var OutCnt: Cardinal): Integer; stdcall;
const
  MaxCode: array[1..4] of Cardinal = ($7F, $7FF, $FFFF, $1FFFFF);
var
  i, j, bc: Cardinal;
  Value: Cardinal;
begin
  result := 0;
  i := OutCnt;
  while (i > 0) and (InCnt > 0) do
  begin
    bc := 1;
    Value := ord(InBuf^);
    if Value < $80 then
      OutBuf^ := WideChar(Value)
    else
    begin
      if Value < $C2 then
      begin
        Result := -1;
        Break;
      end;
      Inc(bc);
      if Value > $DF then
      begin
        Inc(bc);
        if Value > $EF then
        begin
          Inc(bc);
          if Value > $F7 then  // never encountered in the tests.
          begin
            Result := -1;
            Break;
          end;
        end;
      end;
      if InCnt < bc then
        Break;
      j := 1;
      while j < bc do
      begin
        if InBuf[j] in [#$80..#$BF] then
          Value := (Value shl 6) or (Cardinal(InBuf[j]) and $3F)
        else
        begin
          Result := -1;
          Break;
        end;
        Inc(j);
      end;
      Value := Value and MaxCode[bc];
      // RFC2279 check
      if Value <= MaxCode[bc-1] then
      begin
        Result := -1;
        Break;
      end;
      case Value of
        0..$D7FF, $E000..$FFFF: OutBuf^ := WideChar(Value);
        $10000..$10FFFF:
        begin
          if i < 2 then Break;
          OutBuf^ := WideChar($D7C0 + (Value shr 10));
          OutBuf[1] := WideChar($DC00 xor (Value and $3FF));
          Inc(OutBuf); // once here
          Dec(i);
        end
        else
        begin
          Result := -1;
          Break;
        end;
      end;
    end;
    Inc(OutBuf);
    Inc(InBuf, bc);
    Dec(InCnt, bc);
    Dec(i);
  end;
  if Result >= 0 then
    Result := OutCnt-i;
  OutCnt := i;
end;


initialization

finalization
  if Assigned(Xml11Pg) then
    FreeMem(Xml11Pg);

end.
