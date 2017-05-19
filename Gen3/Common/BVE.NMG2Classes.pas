unit BVE.NMG2Classes;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  System.math,
  System.Character,
  System.SyncObjs,
  BVE.NMG2Types;

type
  TLogLineEvent = procedure(Sender: TObject; const LogLine: string;
    LogCmd: integer) of object;

  IG2Log = interface
    ['{62616541-0D15-4E58-BF70-7997177EA060}']
    function GetEnabled: boolean;
    procedure SetEnabled(const aValue: boolean);
    function GetOnLogLine: TLogLineEvent;
    procedure SetOnLogLine(const aValue: TLogLineEvent);
    procedure add_log_line(tekst: string; log_cmd: integer);
    procedure dump_buffer(var buffer; max_size: integer);
    procedure SaveLog(const filename: string);
    procedure ClearLog;

    procedure AssignLog(Lines: TStrings);

    property Enabled: boolean read GetEnabled write SetEnabled;
    property OnLogLine: TLogLineEvent read GetOnLogLine write SetOnLogLine;
  end;

  TBitReader = class
    FReadBitPointer: integer;
    FBitBuffer: byte;
    constructor Create;
    procedure Init;
    function ReadBits(aStream: TStream; NoOffBits: byte): Cardinal;
  end;

  TBitWriter = class
    FWriteBitPointer: integer;
    FBitBuffer: byte;
    constructor Create;
    procedure Init;
    procedure WriteBits(aStream: TStream; Value: Cardinal; NoOffBits: byte);
    function GetWriteBufferBitsLeft: integer;
  end;

  TPatchChunk = class[Weak]
    FLog: IG2Log;
    FStream: TStream;
    FId: byte;
    FSize: word;
    FBitReader: TBitReader;
    FBitWriter: TBitWriter;
    FWriteBuffer: TMemoryStream;
    FReadBuffer: TMemoryStream;
    FWriteCrc: word;
    FReadCrc: word;
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    procedure Init;
    function PeekID: integer;
    procedure ReadChunk;
    procedure ReadBuffer(aSize: integer);
    function ReadName: string;
    function ReadBits(NoOffBits: byte): Cardinal;
    function GetReadBufferBitsLeft: integer;
    procedure WriteChunk(aId: byte);
    procedure WriteBits(Value: Cardinal; NoOffBits: byte);
    procedure WriteName(Value: string);
    procedure WriteCrc(aStream: TStream);
    procedure Flush;
    procedure DumpChunkData(aStream: TStream);

    property Log: IG2Log read FLog write FLog;
  end;

  TG2Message = class(TMemoryStream)
  private
    FAddReversed: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function WriteMessage(Value: byte): boolean;
    procedure WriteClaviaString(aValue: string);
    procedure AddFront(aMessage: TG2Message); virtual;
    procedure AddBack(aMessage: TG2Message);
    procedure Add(SubMessage: TG2Message);

    procedure AddMessModuleAdd(const aLocation, aModuleType, aModuleIndex, aCol,
      aRow, aUprate, aIsLed: byte; aModeArray: array of byte;
      const aModuleName: string);
    procedure AddMessModuleUprate(const aLocation, aModuleIndex,
      aUprateValue: byte);
    procedure AddMessModuleMove(const aLocation, aModuleIndex, aCol,
      aRow: byte);
    procedure AddMessModuleLabel(const aLocation, aModuleIndex: byte;
      const aLabel: string);
    procedure AddMessModuleColor(const aLocation, aModuleIndex, aColor: byte);
    procedure AddMessModuleDelete(const aLocation, aModuleIndex: byte);

    procedure AddMessCableAdd(const aLocation, aFromModuleIndex, aFromConnIndex,
      aFromConnKind, aToModuleIndex, aToConnIndex, aToConnKind, aColor: byte);
    procedure AddMessCableColor(const aLocation, aFromModuleIndex,
      aFromConnIndex, aFromConnKind, aToModuleIndex, aToConnIndex, aToConnKind,
      aColor: byte);
    procedure AddMessCableDelete(const aLocation, aFromModuleIndex,
      aFromConnIndex, aFromConnKind, aToModuleIndex, aToConnIndex,
      aToConnKind: byte);

    // procedure   AddMessAddCable;
    procedure AddMessGlobalKnobAssign(const aSlotIndex, aLocation, aModuleIndex,
      aParamIndex, aGlobalKnobIndex: byte);
    procedure AddMessKnobAssign(const aLocation, aModuleIndex, aParamIndex,
      aKnobIndex: byte);
    procedure AddMessMidiCCAssign(const aLocation, aModuleIndex, aParamIndex,
      aMidiCC: byte);
    procedure AddMessGlobalKnobDeassign(const aGlobalKnobIndex: byte);
    procedure AddMessKnobDeassign(const aKnobIndex: byte);
    procedure AddMessMidiCCDeassign(const aMidiCC: byte);
    procedure AddMessMorphSet(const aLocation, aModuleIndex, aParamIndex,
      aMorph, aAbsRange, aNegative, aVariation: byte);

    property AddReversed: boolean read FAddReversed write FAddReversed;
    // For undo
  end;

  TG2MessageEvent = function(aSender: TObject; aMsg: TG2Message)
    : boolean of object;

  TG2SendMessage = class(TG2Message)
  private
    FOffset: integer;
    FPrepared: boolean;
    function GetCommand: byte;
    function GetHasResponse: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(aSource: TG2SendMessage); virtual;
    procedure PrepareForSend;
    procedure AddFront(SubMessage: TG2Message); override;
    procedure Init;

    property Command: byte read GetCommand;
    property HasResponse: boolean read GetHasResponse;
    property Offset: integer read FOffset write FOffset;
    property Prepared: boolean read FPrepared write FPrepared;
  end;

  TG2ResponseMessage = class(TG2Message)
  private
    function GetCommand: byte;
    function GetIsEmbedded: boolean;
    function GetIsLedData: boolean;
    function GetIsResourcesUsedData: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(aSource: TG2ResponseMessage); virtual;
    procedure CalcCRC;

    property Command: byte read GetCommand;
    property IsEmbedded: boolean read GetIsEmbedded;
    property IsLedData: boolean read GetIsLedData;
    property IsResourcesUsedData: boolean read GetIsResourcesUsedData;
  end;

  TModuleDefStream = class;

  TModuleDefList = class(TStringList)
  private
    function GetModulePanelDef(const aName: string): TModuleDefStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadModulePanelDefs(const aFilename: string);

    property ModulePanelDef[const aName: string]: TModuleDefStream
      read GetModulePanelDef;
  end;

  TModuleDefStream = class(TMemoryStream)
  public
    constructor Create; overload;
    constructor Create(filename: string); overload;
    destructor Destroy; override;
    function GetNextString(var source: string;
      EndTokens: array of Char): string;
    function GetNextByte(var source: string; EndTokens: array of Char): byte;
    function GetNextInteger(var source: string;
      EndTokens: array of Char): integer;
    procedure ReadSpaces;
    function ReadUntil(EndTokens: array of Char): string;
    function ReadConst(Value: string): boolean;
    function ReadOptions(sl: TStrings;
      ListTokens, EndTokens: array of Char): integer;
    function PeekValue(Name: string;
      ValueEndTokens, EndTokens: array of Char): string;
    function UnQuote(Value: string): string;
  end;

  // Utility class for renumbering, renaming, moving modules
  TModuleChange = class
  private
    FModuleFileName: string;
    FNewModuleName: string;

    FHeight: byte;

    FOldModuleIndex: byte;
    FNewModuleIndex: byte;

    FOldRow: byte;
    FNewRow: byte;
    FOldCol: byte;
    FNewCol: byte;

    function GetModuleFileName: string;
    function GetNewModuleName: string;
    function GetHeight: byte;
    function GetOldModuleIndex: byte;
    function GetNewModuleIndex: byte;
    function GetOldRow: byte;
    function GetOldCol: byte;
    function GetNewRow: byte;
    function GetNewCol: byte;

    procedure SetModuleFileName(const aValue: string);
    procedure SetNewModuleName(const aValue: string);
    procedure SetHeight(const aValue: byte);
    procedure SetOldModuleIndex(const aValue: byte);
    procedure SetNewModuleIndex(const aValue: byte);
    procedure SetOldRow(const aValue: byte);
    procedure SetOldCol(const aValue: byte);
    procedure SetNewRow(const aValue: byte);
    procedure SetNewCol(const aValue: byte);

    procedure AddMessMove(const aLocationIndex: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
  public
    constructor Create(const aModuleIndex: integer;
      const aModuleName, aModuleFileName: string;
      const aRow, aCol, aHeightUnits: integer);
    constructor CopyCreate(aModuleChange: TModuleChange);
    destructor Destroy; override;

    property OldModuleIndex: byte read GetOldModuleIndex
      write SetOldModuleIndex;
    property ModuleFileName: string read GetModuleFileName
      write SetModuleFileName;
    property NewModuleName: string read GetNewModuleName write SetNewModuleName;
    property Height: byte read GetHeight write SetHeight;
    property NewModuleIndex: byte read GetNewModuleIndex
      write SetNewModuleIndex;
    property OldRow: byte read GetOldRow write SetOldRow;
    property NewRow: byte read GetNewRow write SetNewRow;
    property OldCol: byte read GetOldCol write SetOldCol;
    property NewCol: byte read GetNewCol write SetNewCol;
  end;

  TModuleChangeList = class(TObjectList<TModuleChange>)
  private
    FSortColRow: IComparer<TModuleChange>;
  public
    constructor Create;
    constructor CopyCreate(aModuleChangeList: TModuleChangeList);

    function FindModuleChange(const aModuleIndex: byte): TModuleChange;
    procedure MergeList(aModuleChangeList: TModuleChangeList);
    procedure RemoveOverlap;
    procedure AddMessages(const aLocationIndex: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
  end;

  TG2Log = class(TInterfacedObject, IG2Log)
  private
    FEnabled: boolean;
    FLogLines: TStringList;
    FLogLock: TCriticalSection; // for multi threaded access to log

    FOnLogLine: TLogLineEvent;
  protected
    function GetOnLogLine: TLogLineEvent;
    procedure SetOnLogLine(const aValue: TLogLineEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetEnabled(const aValue: boolean);
    function GetEnabled: boolean;
    procedure add_log_line(tekst: string; log_cmd: integer);
    procedure dump_buffer(var buffer; max_size: integer);
    procedure SaveLog(const filename: string);
    procedure AssignLog(Lines: TStrings);
    procedure ClearLog;
  end;

implementation

// ------------------------------------------------------------------------------
//
//                              TModuleDefList
//
// ------------------------------------------------------------------------------

constructor TModuleDefList.Create;
begin
  inherited Create;
end;

destructor TModuleDefList.Destroy;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;
  inherited;
end;

procedure TModuleDefList.LoadModulePanelDefs(const aFilename: string);
var
  sl: TStringList;
  i, m, c: integer;
  b: byte;
  MemStream: TModuleDefStream;
  module_name, line: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(aFilename);
    i := 0;
    while i < sl.Count do
    begin
      if sl[i].Contains('<#File:') then
      begin
        c := 7;
        module_name := '';
        while sl[i].Chars[c] <> '#' do
        begin
          module_name := module_name + sl[i].Chars[c];
          inc(c);
        end;
        m := Add(module_name);
        MemStream := TModuleDefStream.Create;
        Objects[m] := MemStream;
        inc(i);
        while (i < sl.Count) and (not sl[i].Contains('<#File:')) do
        begin
          line := sl[i];
          for c := 0 to line.Length - 1 do
          begin
            b := byte(line.Chars[c]);
            MemStream.Write(b, 1);
          end;
          b := 13;
          MemStream.Write(b, 1);
          inc(i);
        end;
      end
      else
        raise Exception.Create('"<#File:" expected.');
    end;
  finally
    sl.Free;
  end;
end;

function TModuleDefList.GetModulePanelDef(const aName: string)
  : TModuleDefStream;
var
  i: integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Objects[i] as TModuleDefStream
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
//
//                             TModuleDefStream
//
// ------------------------------------------------------------------------------

constructor TModuleDefStream.Create(filename: string);
begin
  inherited Create;
  LoadFromFile(filename);
end;

constructor TModuleDefStream.Create;
begin
  inherited Create;
end;

destructor TModuleDefStream.Destroy;
begin
  inherited;
end;

function TModuleDefStream.GetNextString(var source: string;
  EndTokens: array of Char): string;
var
  i: integer;
begin
  Result := '';
  i := 0;
  while (i < source.Length) and not(source.Chars[i].IsInArray(EndTokens)) do
    inc(i);

  Result := source.Substring(0, i);
  source := source.Substring(i);
end;

function TModuleDefStream.GetNextByte(var source: string;
  EndTokens: array of Char): byte;
var
  Value: string;
begin
  Value := GetNextString(source, EndTokens);
  Result := StrToInt(Value);
end;

function TModuleDefStream.GetNextInteger(var source: string;
  EndTokens: array of Char): integer;
var
  Value: string;
begin
  Value := GetNextString(source, EndTokens);
  Result := StrToInt(Value);
end;

procedure TModuleDefStream.ReadSpaces;
var
  b: byte;
begin
  while (Position < Size) and (Read(b, 1) = 1) and
    (Char(b).IsInArray([' ', #9, #10, #13])) do
  begin
  end;
  if not(Char(b).IsInArray([' ', #9, #10, #13])) then
    Position := Position - 1;
end;

function TModuleDefStream.ReadUntil(EndTokens: array of Char): string;
var
  b: byte;
begin
  Result := '';
  while (Position < Size) and (Read(b, 1) = 1) and
    not(Char(b).IsInArray(EndTokens)) do
  begin
    Result := Result + Char(b);
  end;
end;

function TModuleDefStream.ReadConst(Value: string): boolean;
var
  b: byte;
  i: integer;
begin
  i := 1;
  while (Position < Size) and (i <= Length(Value)) and (Read(b, 1) = 1) and
    (Value[i] = Char(b)) do
    inc(i);
  Result := not(i <= Length(Value));
end;

function TModuleDefStream.ReadOptions(sl: TStrings;
  ListTokens, EndTokens: array of Char): integer;
var
  b: byte;
  s: string;
begin
  while (Position < Size) and (Read(b, 1) = 1) and
    not(Char(b).IsInArray(EndTokens)) do
  begin
    if (Char(b).IsInArray(ListTokens)) then
    begin
      sl.Add(s);
      s := '';
    end
    else
      s := s + Char(b);
  end;
  if s <> '' then
    sl.Add(s);
  Result := sl.Count;
end;

function TModuleDefStream.PeekValue(Name: string;
  ValueEndTokens, EndTokens: array of Char): string;
var
  oldpos, start: integer;
  c: Char;
  found: boolean;
begin
  Result := '';
  oldpos := Position;
  try
    found := False;
    start := Position;
    Read(c, 1);
    while (Position < Size) and not(found) and not(c.IsInArray(EndTokens)) do
    begin
      if c <> Name.Chars[Position - 1 - start] then
      begin
        start := Position;
      end
      else if Position - start = Length(Name) then
        found := True;

      if not(found) then
        Read(c, 1);
    end;

    if found then
    begin
      Read(c, 1);
      while (Position < Size) and not(c.IsInArray(ValueEndTokens)) do
      begin
        Result := Result + c;
        Read(c, 1);
      end;
    end;

  finally
    Position := oldpos;
  end;
end;

function TModuleDefStream.UnQuote(Value: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Value.Length - 1 do
    if Value.Chars[i] <> '"' then
      Result := Result + Value.Chars[i];
end;

// ------------------------------------------------------------------------------
//
//                                TBitReader
//
// ------------------------------------------------------------------------------

constructor TBitReader.Create;
begin
  Init;
end;

procedure TBitReader.Init;
begin
  FReadBitPointer := 8; // Forces a new read of a new byte in the BitBuffer
  FBitBuffer := 0;
end;

function TBitReader.ReadBits(aStream: TStream; NoOffBits: byte): Cardinal;
var
  Mask: byte;
  BitBufferSize, BitsLeft: integer;
begin
  // Clavia uses a kind of compression with the patch data

  BitBufferSize := SizeOf(FBitBuffer) * 8;
  BitsLeft := BitBufferSize - FReadBitPointer; // Bits not read in BitBuffer;

  Mask := $FF shr FReadBitPointer; // Readpointer : msb = 0, lsb = 7 !

  if (NoOffBits - BitsLeft) <= 0 then
  begin // The bits left in the buffer can be used

    Result := (FBitBuffer and Mask) // Clear the bits that are not needed
      shr (BitsLeft - NoOffBits);
    // Move the remaining bits to the right position

    FReadBitPointer := FReadBitPointer + NoOffBits; // New readpointer position

  end
  else
  begin // Read new data

    NoOffBits := NoOffBits - BitsLeft;
    Result := (FBitBuffer and Mask)
    // Clear the bits that are not needed from the old buffer
      shl NoOffBits; // Move the remaining bits to the right position

    while (NoOffBits > 0) do
    begin
      // Read a new byte in the bitbuffer
      if aStream.Read(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
        raise Exception.Create('Read error.');

      if NoOffBits < BitBufferSize then
      begin
        FReadBitPointer := NoOffBits;
        Result := Result + FBitBuffer shr (BitBufferSize - FReadBitPointer);
        // Add the bits from the new bitbuffer}
        NoOffBits := 0;
      end
      else
      begin
        FReadBitPointer := 8;
        NoOffBits := NoOffBits - BitBufferSize;
        Result := Result + FBitBuffer shl (NoOffBits);
        // Add the bits from the new bitbuffer}
      end;
    end;
  end;
end;

// ------------------------------------------------------------------------------
//
//                                TBitWriter
//
// ------------------------------------------------------------------------------

constructor TBitWriter.Create;
begin
  FWriteBitPointer := 0;
  FBitBuffer := 0;
end;

procedure TBitWriter.WriteBits(aStream: TStream; Value: Cardinal;
  NoOffBits: byte);
var
  Mask: Cardinal;
  BitBufferSize, BitsLeft: integer;
begin
  BitBufferSize := SizeOf(FBitBuffer) * 8;

  BitsLeft := BitBufferSize - FWriteBitPointer;

  while NoOffBits > BitsLeft do
  begin

    FBitBuffer := FBitBuffer + Value shr (NoOffBits - BitsLeft);

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    Mask := $FFFFFFFF shr (32 - (NoOffBits - BitsLeft));
    Value := Value and Mask;

    FBitBuffer := 0;
    FWriteBitPointer := 0;

    NoOffBits := NoOffBits - BitsLeft;

    BitsLeft := BitBufferSize - FWriteBitPointer;
  end;

  if NoOffBits = BitsLeft then
  begin

    FBitBuffer := FBitBuffer + Value;

    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');

    FBitBuffer := 0;
    FWriteBitPointer := 0;

  end
  else
  begin

    FBitBuffer := FBitBuffer + Value shl (BitsLeft - NoOffBits);
    FWriteBitPointer := FWriteBitPointer + NoOffBits;

  end;
end;

function TBitWriter.GetWriteBufferBitsLeft: integer;
begin
  Result := SizeOf(FBitBuffer) * 8 - FWriteBitPointer;
end;

procedure TBitWriter.Init;
begin
  FWriteBitPointer := 0;
  FBitBuffer := 0;
end;

// ------------------------------------------------------------------------------
//
//                                TPatchChunk
//
// ------------------------------------------------------------------------------

constructor TPatchChunk.Create(aStream: TStream);
begin
  FStream := aStream;
  // aStream.Position := 0;
  FBitReader := TBitReader.Create;
  FBitWriter := TBitWriter.Create;
  FWriteBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  { if aStream.Size > 0 then begin
    ReadBuffer(aStream.Size);
    end; }
  FWriteCrc := 0;
  FReadCrc := 0;
end;

destructor TPatchChunk.Destroy;
begin
  FReadBuffer.Free;
  FWriteBuffer.Free;
  FBitWriter.Free;
  FBitReader.Free;

  inherited;
end;

procedure TPatchChunk.Init;
begin
  FBitReader.Init;
  FBitWriter.Init;
  FWriteBuffer.Clear;
  FReadBuffer.Clear;
  FWriteCrc := 0;
  FReadCrc := 0;
end;

procedure TPatchChunk.ReadBuffer(aSize: integer);
var
  i: integer;
  Crc: word;
begin
  FReadBuffer.Size := aSize;
  if FStream.Read(FReadBuffer.Memory^, aSize) <> aSize then
    raise Exception.Create('Read error');

  for i := 0 to aSize - 1 do
    FReadCrc := CrcClavia(FReadCrc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);

  if assigned(FLog) then
  begin
    // Calculate another CRC for the buffer, to be able to compare chunkdata in logfile
    Crc := 0;
    for i := 0 to aSize - 1 do
      Crc := CrcClavia(Crc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);

    if assigned(FLog) then
    begin
      FLog.add_log_line('Caluclated Crc : ' + IntToHex(Crc, 4), LOGCMD_NUL);
      FLog.add_log_line('', LOGCMD_NUL);
    end;

  end;

  FReadBuffer.Position := 0;

  FBitReader.FReadBitPointer := 8;
  // Forces a new read of a new byte in the BitBuffer
end;

function TPatchChunk.PeekID: integer;
var
  aId: byte;
begin
  Result := -1;
  if FStream.Read(aId, SizeOf(aId)) = SizeOf(aId) then
  begin
    Result := aId;
    FStream.Position := FStream.Position - 1;
  end;
end;

procedure TPatchChunk.ReadChunk;
var
  bm, bl: byte;
begin
  if FStream.Read(FId, SizeOf(FId)) <> SizeOf(FId) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bm, SizeOf(bm)) <> SizeOf(bm) then
    raise Exception.Create('Error reading chunk.');

  if FStream.Read(bl, SizeOf(bl)) <> SizeOf(bl) then
    raise Exception.Create('Error reading chunk.');

  FSize := bm * 256 + bl;

  if assigned(FLog) then
  begin
    FLog.add_log_line('Chunk id: ' + IntToHex(FId, 2) + ', size: ' +
      IntToStr(FSize), LOGCMD_NUL);
    DumpChunkData(FStream);
  end;

  // Calc crc
  FReadCrc := CrcClavia(FReadCrc, FId);
  FReadCrc := CrcClavia(FReadCrc, bm);
  FReadCrc := CrcClavia(FReadCrc, bl);

  ReadBuffer(FSize);
end;

function TPatchChunk.ReadBits(NoOffBits: byte): Cardinal;
begin
  Result := FBitReader.ReadBits(FReadBuffer, NoOffBits);
end;

procedure TPatchChunk.WriteBits(Value: Cardinal; NoOffBits: byte);
begin
  FBitWriter.WriteBits(FWriteBuffer, Value, NoOffBits);
end;

procedure TPatchChunk.WriteChunk(aId: byte);
var
  BufSize: word;
  bm, bl: byte;
  i: integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');

  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;
  bm := Hi(BufSize);
  bl := Lo(BufSize);

  // Calc crc
  FWriteCrc := CrcClavia(FWriteCrc, aId);
  FWriteCrc := CrcClavia(FWriteCrc, bm);
  FWriteCrc := CrcClavia(FWriteCrc, bl);
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia(FWriteCrc,
      PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(aId, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bm, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(bl, 1) <> 1) then
    raise Exception.Create('Write error');

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

procedure TPatchChunk.Flush;
var
  BufSize: word;
  i: integer;
begin
  // Write the remaining byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');

  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;

  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;

  // Calc crc
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia(FWriteCrc,
      PStaticByteBuffer(FWriteBuffer.Memory)^[i]);

  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');

  FWriteBuffer.Clear;
end;

function TPatchChunk.GetReadBufferBitsLeft: integer;
begin
  Result := SizeOf(FBitReader.FBitBuffer) * 8 - FBitReader.FReadBitPointer +
    (FReadBuffer.Size - FReadBuffer.Position) * 8;
end;

procedure TPatchChunk.WriteCrc(aStream: TStream);
var
  bm, bl: byte;
begin
  bm := (FWriteCrc shr 8) and $FF;
  aStream.Write(bm, SizeOf(bm));
  bl := FWriteCrc and $FF;
  aStream.Write(bl, SizeOf(bl));
end;

function TPatchChunk.ReadName: string;
var
  b: byte;
begin
  Result := '';
  b := ReadBits(8);
  while (b <> 0) do
  begin
    Result := Result + Char(b);
    if Length(Result) = 16 then
      break;
    b := ReadBits(8);
  end;
end;

procedure TPatchChunk.WriteName(Value: string);
var
  i: integer;
  b: byte;
begin
  i := 0;
  while (i < Value.Length) and (i < 16) do
  begin
    b := byte(Value.Chars[i]);
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i < 16) then
  begin
    b := 0;
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

procedure TPatchChunk.DumpChunkData(aStream: TStream);
var
  p, i, c, Position: integer;
  char_line, line: string;
  b: byte;
begin
  if not assigned(FLog) then
    exit;

  Position := aStream.Position;
  try
    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i < FSize) do
    begin
      if c < 16 then
      begin
        aStream.Read(b, 1);
        line := line + IntToHex(b, 2) + ' ';
        if b >= 32 then
          char_line := char_line + chr(b)
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end
      else
      begin
        FLog.add_log_line(IntToHex(p, 6) + ' - ' + line + ' ' + char_line,
          LOGCMD_NUL);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
      FLog.add_log_line(IntToHex(p, 6) + ' - ' + line + stringofchar(' ',
        16 * 3 - Length(line) + 1) + char_line, LOGCMD_NUL);
  finally
    aStream.Position := Position;
  end;
end;

// ------------------------------------------------------------------------------
//
//                               TG2Message
//
// ------------------------------------------------------------------------------

constructor TG2Message.Create;
begin
  inherited;
  FAddReversed := False;
end;

destructor TG2Message.Destroy;
begin
  inherited;
end;

function TG2Message.WriteMessage(Value: byte): boolean;
begin
  Result := Write(Value, 1) = 1;
end;

procedure TG2Message.AddBack(aMessage: TG2Message);
var
  OldSize: integer;
begin
  OldSize := Size;
  Size := Size + aMessage.Size;
  move(PStaticByteBuffer(aMessage.Memory)^[0], PStaticByteBuffer(Memory)
    ^[OldSize], aMessage.Size);
end;

procedure TG2Message.AddFront(aMessage: TG2Message);
var
  OldSize, Offset: integer;
begin
  Offset := 0;
  OldSize := Size - Offset;
  Size := Size + aMessage.Size;

  move(PStaticByteBuffer(Memory)^[Offset], PStaticByteBuffer(Memory)
    ^[aMessage.Size + Offset], OldSize);
  move(PStaticByteBuffer(aMessage.Memory)^[0], PStaticByteBuffer(Memory)
    ^[Offset], aMessage.Size);
end;

procedure TG2Message.Add(SubMessage: TG2Message);
var
  OldSize: integer;
begin
  if not FAddReversed then
  begin
    OldSize := Size;
    Size := Size + SubMessage.Size;
    move(PStaticByteBuffer(SubMessage.Memory)^[0], PStaticByteBuffer(Memory)
      ^[OldSize], SubMessage.Size);
  end
  else
    AddFront(SubMessage);
end;

procedure TG2Message.AddMessGlobalKnobAssign(const aSlotIndex, aLocation,
  aModuleIndex, aParamIndex, aGlobalKnobIndex: byte);
var
  BitWriter: TBitWriter;
  Mess: TG2Message;
begin
  BitWriter := TBitWriter.Create;
  Mess := TG2Message.Create;
  try
    // Assign a global knob
    BitWriter.WriteBits(Mess, S_ASS_GLOBAL_KNOB, 8);
    BitWriter.WriteBits(Mess, aSlotIndex, 4);
    BitWriter.WriteBits(Mess, aLocation, 2);
    BitWriter.WriteBits(Mess, $00, 2);
    BitWriter.WriteBits(Mess, aModuleIndex, 8);
    BitWriter.WriteBits(Mess, aParamIndex, 8);
    BitWriter.WriteBits(Mess, $00, 8);
    BitWriter.WriteBits(Mess, aGlobalKnobIndex, 8);

    Add(Mess);
  finally
    Mess.Free;
    BitWriter.Free;
  end;
end;

procedure TG2Message.AddMessKnobAssign(const aLocation, aModuleIndex,
  aParamIndex, aKnobIndex: byte);
var
  BitWriter: TBitWriter;
  Mess: TG2Message;
begin
  BitWriter := TBitWriter.Create;
  Mess := TG2Message.Create;
  try
    // Assign a knob
    BitWriter.WriteBits(Mess, S_ASSIGN_KNOB, 8);
    BitWriter.WriteBits(Mess, aModuleIndex, 8);
    BitWriter.WriteBits(Mess, aParamIndex, 8);
    BitWriter.WriteBits(Mess, aLocation, 2);
    BitWriter.WriteBits(Mess, $00, 6);
    BitWriter.WriteBits(Mess, $00, 8);
    BitWriter.WriteBits(Mess, aKnobIndex, 8);

    Add(Mess);
  finally
    Mess.Free;
    BitWriter.Free;
  end;
end;

procedure TG2Message.AddMessMidiCCAssign(const aLocation, aModuleIndex,
  aParamIndex, aMidiCC: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    // Assign cc
    WriteByte(Mess, S_ASSIGN_MIDICC);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aParamIndex);
    WriteByte(Mess, aMidiCC);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessCableAdd(const aLocation, aFromModuleIndex,
  aFromConnIndex, aFromConnKind, aToModuleIndex, aToConnIndex, aToConnKind,
  aColor: byte);
var
  BitWriter: TBitWriter;
  Mess: TG2Message;
begin
  BitWriter := TBitWriter.Create;
  Mess := TG2Message.Create;
  try
    BitWriter.WriteBits(Mess, S_ADD_CABLE, 8);
    BitWriter.WriteBits(Mess, 1, 4); // Unknown
    BitWriter.WriteBits(Mess, aLocation, 1);

    // The to-connector must be an input!
    BitWriter.WriteBits(Mess, aColor, 3);
    BitWriter.WriteBits(Mess, aFromModuleIndex, 8);
    BitWriter.WriteBits(Mess, aFromConnKind, 2);
    BitWriter.WriteBits(Mess, aFromConnIndex, 6);
    BitWriter.WriteBits(Mess, aToModuleIndex, 8);
    BitWriter.WriteBits(Mess, aToConnKind, 2);
    BitWriter.WriteBits(Mess, aToConnIndex, 6);

    Add(Mess);
  finally
    Mess.Free;
    BitWriter.Free;
  end;
end;

procedure TG2Message.AddMessCableColor(const aLocation, aFromModuleIndex,
  aFromConnIndex, aFromConnKind, aToModuleIndex, aToConnIndex, aToConnKind,
  aColor: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_CABLE_COLOR);
    WriteByte(Mess, aLocation shl 3 + aColor);
    WriteByte(Mess, aFromModuleIndex);
    WriteByte(Mess, aFromConnKind shl 6 + aFromConnIndex);
    WriteByte(Mess, aToModuleIndex);
    WriteByte(Mess, aToConnKind shl 6 + aToConnIndex);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessCableDelete(const aLocation, aFromModuleIndex,
  aFromConnIndex, aFromConnKind, aToModuleIndex, aToConnIndex,
  aToConnKind: byte);
var
  BitWriter: TBitWriter;
  Mess: TG2Message;
begin
  BitWriter := TBitWriter.Create;
  Mess := TG2Message.Create;
  try
    BitWriter.WriteBits(Mess, S_DEL_CABLE, 8);
    BitWriter.WriteBits(Mess, 1, 7); // Unknown
    BitWriter.WriteBits(Mess, aLocation, 1);
    BitWriter.WriteBits(Mess, aFromModuleIndex, 8);
    BitWriter.WriteBits(Mess, aFromConnKind, 2);
    BitWriter.WriteBits(Mess, aFromConnIndex, 6);
    BitWriter.WriteBits(Mess, aToModuleIndex, 8);
    BitWriter.WriteBits(Mess, aToConnKind, 2);
    BitWriter.WriteBits(Mess, aToConnIndex, 6);

    Add(Mess);
  finally
    Mess.Free;
    BitWriter.Free;
  end;
end;

procedure TG2Message.AddMessGlobalKnobDeassign(const aGlobalKnobIndex: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_DEASS_GLOB_KNOB);
    WriteByte(Mess, $00);
    WriteByte(Mess, aGlobalKnobIndex);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessKnobDeassign(const aKnobIndex: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_DEASSIGN_KNOB);
    WriteByte(Mess, $00);
    WriteByte(Mess, aKnobIndex);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessMidiCCDeassign(const aMidiCC: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_DEASSIGN_MIDICC);
    WriteByte(Mess, aMidiCC);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleAdd(const aLocation, aModuleType,
  aModuleIndex, aCol, aRow, aUprate, aIsLed: byte; aModeArray: array of byte;
  const aModuleName: string);
var
  Mess: TG2Message;
  i: integer;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_ADD_MODULE);
    WriteByte(Mess, aModuleType);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aCol);
    WriteByte(Mess, aRow);
    WriteByte(Mess, 0);
    WriteByte(Mess, aUprate);
    WriteByte(Mess, aIsLed);

    for i := 0 to Length(aModeArray) - 1 do
    begin
      WriteByte(Mess, aModeArray[i]);
    end;

    Mess.WriteClaviaString(aModuleName);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleColor(const aLocation, aModuleIndex,
  aColor: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_SET_MODULE_COLOR);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aColor);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleDelete(const aLocation, aModuleIndex: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try

    WriteByte(Mess, S_DEL_MODULE);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleLabel(const aLocation, aModuleIndex: byte;
  const aLabel: string);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try

    WriteByte(Mess, S_SET_MODULE_LABEL);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    Mess.WriteClaviaString(aLabel);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleMove(const aLocation, aModuleIndex, aCol,
  aRow: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try

    // Add a move module message
    WriteByte(Mess, S_MOV_MODULE);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aCol);
    WriteByte(Mess, aRow);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessModuleUprate(const aLocation, aModuleIndex,
  aUprateValue: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try

    WriteByte(Mess, S_SET_UPRATE);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aUprateValue);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.AddMessMorphSet(const aLocation, aModuleIndex, aParamIndex,
  aMorph, aAbsRange, aNegative, aVariation: byte);
var
  Mess: TG2Message;
begin
  Mess := TG2Message.Create;
  try
    WriteByte(Mess, S_SET_MORPH_RANGE);
    WriteByte(Mess, aLocation);
    WriteByte(Mess, aModuleIndex);
    WriteByte(Mess, aParamIndex);
    WriteByte(Mess, aMorph);
    WriteByte(Mess, aAbsRange);
    WriteByte(Mess, aNegative);
    WriteByte(Mess, aVariation);

    Add(Mess);
  finally
    Mess.Free;
  end;
end;

procedure TG2Message.WriteClaviaString(aValue: string);
var
  i: integer;
  b: byte;
begin
  i := 0;
  while (i < aValue.Length) and (i < 16) do
  begin
    b := byte(aValue.Chars[i]);
    if Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i < 16) then
  begin
    b := 0;
    if Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

// ------------------------------------------------------------------------------
//
//                              TG2SendMessage
//
// ------------------------------------------------------------------------------

procedure TG2SendMessage.Init;
begin
  Clear;
  FPrepared := False;
  WriteMessage(0); // Reserve place for message size
  WriteMessage(0);
end;

constructor TG2SendMessage.Create;
begin
  inherited;
  Init;
end;

destructor TG2SendMessage.Destroy;
begin
  inherited;
end;

function TG2SendMessage.GetCommand: byte;
begin
  Result := 0;
  if Size < 2 then // Invalid message!
    exit;

  // Get send message command
  Result := 0;
  if PStaticByteBuffer(Memory)^[2] = CMD_INIT then
  begin
    Result := CMD_INIT;
  end
  else if Size < 4 then
  begin
    exit; // Invalid message!
  end
  else if (PStaticByteBuffer(Memory)^[3] shr 4) = $02 then
    Result := PStaticByteBuffer(Memory)^[3] and $0F;
end;

function TG2SendMessage.GetHasResponse: boolean;
begin
  Result := False;

  if Size < 3 then // Invalid message!
    exit;

  if PStaticByteBuffer(Memory)^[2] = CMD_INIT then
  begin
    Result := True;
  end
  else if Size < 4 then
  begin
    // Invalid message!
    exit;
  end
  else
    Result := not((PStaticByteBuffer(Memory)^[3] shr 4) = $03);
end;

procedure TG2SendMessage.PrepareForSend;
var
  Crc: word;
  MessageSize: integer;
  b: byte;
begin
  if FPrepared then
    exit;

  MessageSize := (Size + 2);
  PStaticByteBuffer(Memory)^[0] := MessageSize div 256;
  PStaticByteBuffer(Memory)^[1] := MessageSize mod 256;

  // Calc CRC
  Crc := 0;
  Position := 2;
  while Position < Size do
  begin
    Read(b, 1);
    Crc := CrcClavia(Crc, b);
  end;

  // Write CRC
  b := Crc div 256;
  Write(b, 1);
  b := Crc mod 256;
  Write(b, 1);

  FPrepared := True;
end;

procedure TG2SendMessage.AddFront(SubMessage: TG2Message);
var
  OldSize: integer;
begin
  // Destination message holds space for size in front (2 bytes)
  OldSize := Size - FOffset;
  Size := Size + SubMessage.Size;

  move(PStaticByteBuffer(Memory)^[FOffset], PStaticByteBuffer(Memory)
    ^[SubMessage.Size + FOffset], OldSize);
  move(PStaticByteBuffer(SubMessage.Memory)^[0], PStaticByteBuffer(Memory)
    ^[FOffset], SubMessage.Size);
end;

procedure TG2SendMessage.Assign(aSource: TG2SendMessage);
begin
  Size := aSource.Size;
  move(aSource.Memory^, Memory^, Size);
  FAddReversed := aSource.FAddReversed;
  FPrepared := aSource.FPrepared;
  FOffset := aSource.FOffset;
end;


// ------------------------------------------------------------------------------
//
//                             TG2ResponseMessage
//
// ------------------------------------------------------------------------------

constructor TG2ResponseMessage.Create;
begin
  inherited;
end;

destructor TG2ResponseMessage.Destroy;
begin
  inherited;
end;

function TG2ResponseMessage.GetCommand: byte;
begin
  Result := 0;
  if Size < 1 then // Invalid message!
    exit;

  // Get the resonse command
  if PStaticByteBuffer(Memory)^[0] = CMD_INIT then
    Result := CMD_INIT
  else if Size < 4 then
  begin // Invalid message!
    exit;
  end
  else
  begin
    if (PStaticByteBuffer(Memory)^[0] and $F) = 2 then
      Result := PStaticByteBuffer(Memory)^[2] // Embedded
    else
      Result := PStaticByteBuffer(Memory)^[1]; // Extended
  end;
end;

function TG2ResponseMessage.GetIsLedData: boolean;
begin
  Result := False;

  if Size < 4 then
    exit;

  Result := (PStaticByteBuffer(Memory)^[1] in [$00, $01, $02, $03]) and
    (PStaticByteBuffer(Memory)^[3] in [R_LED_DATA, R_VOLUME_DATA]);
end;

function TG2ResponseMessage.GetIsResourcesUsedData: boolean;
begin
  Result := False;

  if Size < 4 then
    exit;

  Result := (PStaticByteBuffer(Memory)^[1] in [$00, $01, $02, $03, $08, $09,
    $0A, $0B]) and (PStaticByteBuffer(Memory)^[3] in [R_RESOURCES_USED]);
end;

procedure TG2ResponseMessage.Assign(aSource: TG2ResponseMessage);
begin
  Size := aSource.Size;
  move(aSource.Memory^, Memory^, Size);
end;

procedure TG2ResponseMessage.CalcCRC;
var
  Crc: word;
  b: byte;
begin
  // Calc CRC
  Crc := 0;
  Position := 0;
  while Position < Size do
  begin
    Read(b, 1);
    Crc := CrcClavia(Crc, b);
  end;

  // Write CRC
  b := Crc div 256;
  Write(b, 1);
  b := Crc mod 256;
  Write(b, 1);

  Position := 0;
end;

function TG2ResponseMessage.GetIsEmbedded: boolean;
begin
  Result := (PStaticByteBuffer(Memory)^[0] and $F) = 2;
end;

// ------------------------------------------------------------------------------
//
//                             TModuleChange
//
// ------------------------------------------------------------------------------

procedure TModuleChange.AddMessMove(const aLocationIndex: byte;
  aSendMessage, aUndoMessage: TG2SendMessage);
begin
  // if not assigned(FWModule) then
  // exit;

  if (FNewCol = FOldCol) and (FNewRow = FOldRow) then
    exit;

  aSendMessage.AddMessModuleMove(aLocationIndex, FNewModuleIndex,
    FNewCol, FNewRow);
  aUndoMessage.AddMessModuleMove(aLocationIndex, FOldModuleIndex,
    FOldCol, FOldRow);
end;

constructor TModuleChange.CopyCreate(aModuleChange: TModuleChange);
begin
  inherited Create;

  FModuleFileName := aModuleChange.FModuleFileName;
  FNewModuleName := aModuleChange.FNewModuleName;

  FHeight := aModuleChange.FHeight;

  FOldModuleIndex := aModuleChange.FOldModuleIndex;
  FNewModuleIndex := aModuleChange.FNewModuleIndex;

  FOldRow := aModuleChange.FOldRow;
  FNewRow := aModuleChange.FNewRow;
  FOldCol := aModuleChange.OldCol;
  FNewCol := aModuleChange.NewCol;
end;

constructor TModuleChange.Create(const aModuleIndex: integer;
  const aModuleName, aModuleFileName: string;
  const aRow, aCol, aHeightUnits: integer);
begin
  inherited Create;

  FModuleFileName := aModuleFileName;
  FNewModuleName := aModuleName;

  FHeight := aHeightUnits;

  FOldModuleIndex := aModuleIndex;
  FNewModuleIndex := aModuleIndex;

  FOldRow := aRow;
  FNewRow := aRow;
  FOldCol := aCol;
  FNewCol := aCol;
end;

destructor TModuleChange.Destroy;
begin
  // SetWeak( @FWModule, nil);
  inherited;
end;

function TModuleChange.GetHeight: byte;
begin
  Result := FHeight;
end;

{ function TModuleChange.GetModule: IG2Module;
  begin
  Result := FWModule;
  end; }

function TModuleChange.GetModuleFileName: string;
begin
  Result := FModuleFileName;
end;

function TModuleChange.GetOldCol: byte;
begin
  Result := FOldCol;
end;

function TModuleChange.GetOldModuleIndex: byte;
begin
  Result := FOldModuleIndex;
end;

function TModuleChange.GetOldRow: byte;
begin
  Result := FOldRow;
end;

function TModuleChange.GetNewCol: byte;
begin
  Result := FNewCol;
end;

function TModuleChange.GetNewModuleIndex: byte;
begin
  Result := FNewModuleIndex;
end;

function TModuleChange.GetNewModuleName: string;
begin
  Result := FNewModuleName;
end;

function TModuleChange.GetNewRow: byte;
begin
  Result := FNewRow;
end;

procedure TModuleChange.SetHeight(const aValue: byte);
begin
  FHeight := aValue;
end;

{ procedure TModuleChange.SetModule(const aValue: IG2Module);
  begin
  SetWeak( @FWModule, aValue);

  FModuleFileName := aValue.ModuleFileName;
  FModuleIndex := aValue.ModuleIndex;
  FHeight := aValue.HeightUnits;
  FNewModuleIndex := aValue.ModuleIndex;
  FNewModuleName := aValue.ModuleName;
  FNewRow := aValue.Row;
  FNewCol := aValue.Col;
  end; }

procedure TModuleChange.SetModuleFileName(const aValue: string);
begin
  FModuleFileName := aValue;
end;

procedure TModuleChange.SetOldCol(const aValue: byte);
begin
  FOldCol := aValue;
end;

procedure TModuleChange.SetOldModuleIndex(const aValue: byte);
begin
  FOldModuleIndex := aValue;
end;

procedure TModuleChange.SetOldRow(const aValue: byte);
begin
  FOldRow := aValue;
end;

procedure TModuleChange.SetNewCol(const aValue: byte);
begin
  FNewCol := aValue;
end;

procedure TModuleChange.SetNewModuleIndex(const aValue: byte);
begin
  FNewModuleIndex := aValue;
end;

procedure TModuleChange.SetNewModuleName(const aValue: string);
begin
  FNewModuleName := aValue;
end;

procedure TModuleChange.SetNewRow(const aValue: byte);
begin
  FNewRow := aValue;
end;

// ------------------------------------------------------------------------------
//
//                           TModuleChangeList
//
// ------------------------------------------------------------------------------

constructor TModuleChangeList.CopyCreate(aModuleChangeList: TModuleChangeList);
var
  i: integer;
begin
  Create;

  for i := 0 to aModuleChangeList.Count - 1 do
  begin
    Add(TModuleChange.CopyCreate(aModuleChangeList[i]));
  end;
end;

constructor TModuleChangeList.Create;
begin
  FSortColRow := TComparer<TModuleChange>.Construct(
    function(const mc1, mc2: TModuleChange): integer
    begin
      if mc1.NewCol > mc2.NewCol then
        Result := 1
      else if mc1.NewCol = mc2.NewCol then
      begin
        if mc1.NewRow > mc2.NewRow then
          Result := 1
        else
        begin
          if mc1.NewRow = mc2.NewRow then
          begin
            Result := 0
          end
          else
            Result := -1;
        end;
      end
      else
        Result := -1;
    end);

  inherited Create(FSortColRow, True);
end;

function TModuleChangeList.FindModuleChange(const aModuleIndex: byte)
  : TModuleChange;
var
  i: integer;
begin
  i := 0;
  while (i < Count) and not(items[i].FOldModuleIndex = aModuleIndex) do
    inc(i);

  if (i < Count) then
    Result := items[i]
  else
    Result := nil;
end;

procedure TModuleChangeList.MergeList(aModuleChangeList: TModuleChangeList);
var
  ModuleChange: TModuleChange;
begin
  if aModuleChangeList.Count = 0 then
    exit;

  // Add a copy of the new modules in the list, the list is sorted by col then row
  for ModuleChange in aModuleChangeList do
  begin
    Add(TModuleChange.CopyCreate(ModuleChange));
  end;
  RemoveOverlap;
end;

procedure TModuleChangeList.RemoveOverlap;
var
  ModuleChange, PrevModuleChange: TModuleChange;
  i, prev_bottom, next_top, overlap: integer;
begin
  Sort;

  // Remove any overlap between modules
  PrevModuleChange := nil;
  for i := 0 to Count - 1 do
  begin
    ModuleChange := items[i];
    if assigned(PrevModuleChange) then
    begin
      if PrevModuleChange.NewCol = ModuleChange.NewCol then
      begin
        prev_bottom := PrevModuleChange.NewRow + PrevModuleChange.Height;
        next_top := ModuleChange.NewRow;

        overlap := prev_bottom - next_top;
        if overlap > 0 then
        begin
          ModuleChange.NewRow := ModuleChange.NewRow + overlap;
        end;

        PrevModuleChange := ModuleChange;

      end
      else
      begin
        PrevModuleChange := nil;
      end;
    end
    else
      PrevModuleChange := ModuleChange;
  end;
end;

{ procedure TModuleChangeList.InsertModuleChange(
  aModuleChange: TModuleChange);
  var i, a, b, c, d, overlap, hole : integer;
  fit, moved : boolean;
  begin
  moved := False;
  if Count > 0 then begin
  hole := 0;
  i := 0;
  fit := false;
  while ( i < Count) and not fit do begin

  if aModuleChange.NewCol < items[i].NewCol then
  inc(i)
  else
  if aModuleChange.NewCol = items[i].NewCol then begin
  // Calc overlap between lines a-b and c-d
  a := items[i].NewRow;
  b := items[i].NewRow + items[i].Height;
  c := aModuleChange.NewRow;
  d := aModuleChange.NewRow + aModuleChange.Height;
  // Calc the hole above the current module
  hole := a - hole;

  overlap := min(b, d) - max(c, a);

  if overlap > 0 then begin
  moved := True;
  // Is the upper point free and does the new module fit in the hole
  // above the current module?
  if (c < a) and (aModuleChange.Height <= hole) then begin
  aModuleChange.NewRow := a - aModuleChange.Height;
  fit := True
  end else begin
  // Try the space under the current module
  aModuleChange.NewRow := b;
  hole := b;
  inc(i);
  end;
  end else begin
  fit := True;
  end;
  end else begin
  fit := True;
  end;
  end;
  end;

  // Add the new module change to the list
  Add(aModuleChange);
  end; }

procedure TModuleChangeList.AddMessages(const aLocationIndex: byte;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    items[i].AddMessMove(aLocationIndex, aSendMessage, aUndoMessage);
end;

// ------------------------------------------------------------------------------
//
//                                 TG2Log
//
// ------------------------------------------------------------------------------

procedure TG2Log.add_log_line(tekst: string; log_cmd: integer);
begin
  if not FEnabled then
    exit;

  if assigned(FOnLogLine) then
  begin
    // Use event if assigned in stead of internal log
    FOnLogLine(self, tekst, log_cmd);
    exit;
  end;

  if (not assigned(FLogLines)) then
    exit;

  FLogLock.Acquire;
  try
    if (not assigned(FLogLines)) then
      exit;

    case log_cmd of
      LOGCMD_NUL:
        FLogLines.Add(tekst);
      LOGCMD_SAV:
        begin
          FLogLines.Add(tekst);
          FLogLines.SaveToFile('G2_log.txt');
        end;
      LOGCMD_HDR:
        begin
          FLogLines.Add('');
          FLogLines.Add(stringofchar('=', Length(tekst)));
          FLogLines.Add(tekst);
        end;
      LOGCMD_TAB:
        FLogLines.Add('    ' + tekst);
      LOGCMD_ERR:
        FLogLines.Add('ERROR : ' + tekst);
    end;

  finally
    FLogLock.Release;
  end;
end;

procedure TG2Log.AssignLog(Lines: TStrings);
begin
  if assigned(FLogLines) then
  begin
    FLogLock.Acquire;
    try
      Lines.Assign(FLogLines);
    finally
      FLogLock.Release;
    end;
  end;
end;

procedure TG2Log.ClearLog;
begin
  if assigned(FLogLines) then
  begin
    FLogLock.Acquire;
    try
      FLogLines.Clear;
    finally
      FLogLock.Release;
    end;
  end;
end;

constructor TG2Log.Create;
begin
  FEnabled := False;
  FLogLock := TCriticalSection.Create;
  FLogLines := TStringList.Create;
end;

destructor TG2Log.Destroy;
begin
  if assigned(FLogLines) then
  begin
    FLogLock.Acquire;
    try
      FreeAndNil(FLogLines);
    finally
      FLogLock.Release;
    end;
  end;
  FLogLock.Free;

  inherited;
end;

procedure TG2Log.dump_buffer(var buffer; max_size: integer);
type
  buf_type = packed array [0 .. 65536 - 1] of byte;
var
  p, i, c: integer;
  char_line, line: string;
begin
  if not FEnabled then
    exit;

  if ((not assigned(FLogLines)) and (not assigned(FOnLogLine))) then
    exit;

  FLogLock.Acquire;
  try

    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i < max_size) do
    begin
      if c < 16 then
      begin
        line := line + IntToHex(buf_type(buffer)[i], 2) + ' ';
        if buf_type(buffer)[i] >= 32 then
          char_line := char_line + chr(buf_type(buffer)[i])
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end
      else
      begin
        if assigned(FOnLogLine) then
          FOnLogLine(self, IntToHex(p, 6) + ' - ' + line + ' ' + char_line,
            LOGCMD_NUL)
        else
          FLogLines.Add(IntToHex(p, 6) + ' - ' + line + ' ' + char_line);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
      if assigned(FOnLogLine) then
        FOnLogLine(self, IntToHex(p, 6) + ' - ' + line + stringofchar(' ',
          16 * 3 - Length(line) + 1) + char_line, LOGCMD_NUL)
      else
        FLogLines.Add(IntToHex(p, 6) + ' - ' + line + stringofchar(' ',
          16 * 3 - Length(line) + 1) + char_line);
  finally
    FLogLock.Release;
  end;
end;

function TG2Log.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

function TG2Log.GetOnLogLine: TLogLineEvent;
begin
  Result := FOnLogLine;
end;

procedure TG2Log.SaveLog(const filename: string);
begin
  if assigned(FLogLines) then
  begin
    FLogLock.Acquire;
    try
      FLogLines.SaveToFile(filename);
    finally
      FLogLock.Release;
    end;
  end;
end;

procedure TG2Log.SetEnabled(const aValue: boolean);
begin
  FEnabled := aValue;
end;

procedure TG2Log.SetOnLogLine(const aValue: TLogLineEvent);
begin
  FOnLogLine := aValue;
end;

end.
