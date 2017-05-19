unit BVE.NMG2Stream;

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf;

const
  MIDI_BLOCK_SIZE = 479;//419; // The number of whole octets in  a full size MIDI packet

type
  // Header for VST patch chunk file
  TFXPHeader = record
    chunkMagic  : packed array[0..3] of byte;        // 'CcnK'
    byteSize    : Cardinal;                          // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of byte;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of byte;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    name        : packed array[0..27] of byte;
    chunkSize   : Cardinal;
  end;

  // Header for VST bank chunk file
  TFXBHeader = record
    chunkMagic  : packed array[0..3] of byte;        // 'CcnK'
    byteSize    : Cardinal;                          // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of byte;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of byte;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    future      : packed array[0..127] of byte;
    chunkSize   : Cardinal;
  end;

  // Base class for loading/saving performances and patches from/to stream
  TG2FileDataStream = class(TInterfacedObject)
  private
    FChecksum         : Integer; // MIDI checksum septet
    FPatchVersion     : byte;
    FPatchType        : byte;
  protected
    //function GetMaxVariations : byte; virtual; abstract;
    //procedure SetMaxVariations( aValue : byte); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    function    LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
    procedure   Read(aChunk : TPatchChunk); virtual;
    procedure   Write(aChunk : TPatchChunk; aVariationCount : byte); virtual;

    //function    ReadClaviaString( aStream : TStream): string;
    //procedure   WriteClaviaString( aStream : TStream; aValue : string);

    function    CreateFromMidiStream(const aStream: TStream; aLog : IG2Log): TMemoryStream;
    procedure   CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
    procedure   SaveMidiToStream( const aStream : TStream);
    function    ParseMidiConst  ( const aStream : TStream; aValue: Byte): Byte;
    function    ParseMidiSeptet ( const aStream : TStream): Byte;
    function    ParseMidiInt    ( const aStream : TStream): Integer;
    function    ParseMidiString ( const aStream : TStream): string;
    procedure   AddMidiByte( aStream : TStream; aByte: Byte);
    procedure   AddMidiInt( aStream : TStream; anInteger: Integer);
    procedure   AddMidiName( aStream : TStream; aName : string);

    class function LoadStreamPatch(aSlot: IG2Slot; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; virtual;
    class function LoadStreamPerf(aConnection: IG2Connection; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; virtual;

    class function LoadFileData(aConnection: IG2Connection; aStream : TStream): TG2FileDataStream; virtual;
    //class function LoadFileStream(aSlot: IG2Slot; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; virtual;
    class function LoadMidiData(aConnection: IG2Connection; aStream: TStream): TG2FileDataStream;
    //class function LoadMidiStream(aConnection: IG2Connection; const aSlotIndex: integer; aStream: TStream): TG2FileDataStream; virtual;

    property PatchVersion : byte read FPatchVersion write FPatchVersion;
    property PatchType : byte read FPatchType write FPatchType;
    //property MaxVariations : byte read GetMaxVariations write SetMaxVariations;
  end;

procedure AddHeaderInfo( aPatchFileType : TPatchFileType; aStream : TStream);

implementation
uses
  System.Math,
  BVE.NMG2Patch,
  BVE.NMG2Perf;

//------------------------------------------------------------------------------
//
//                               TG2DataStream
//
//------------------------------------------------------------------------------

procedure AddHeaderInfo( aPatchFileType : TPatchFileType; aStream : TStream);
var sl : TStringList;
    i, j : integer;
    b : byte;
    s : string;
begin
  sl := TStringList.Create;
  try
    sl.Clear;

    sl.Add('Version=Nord Modular G2 File Format 1');
    case aPatchFileType of
      pftPatch: sl.Add('Type=Patch');
      pftPerf: sl.Add('Type=Performance');
    end;
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := sl[i];
      for j := 0 to s.Length - 1 do begin
        b := byte(s.Chars[j]);
        aStream.Write( b, 1);
      end;
      b := $0d;
      aStream.Write( b, SizeOf( b));
      b := $0a;
      aStream.Write( b, SizeOf( b));
    end;

    b := 0;
    aStream.Write( b, SizeOf( b));
  finally
    sl.Free;
  end;
end;

constructor TG2FileDataStream.Create;
begin
  inherited;
end;

destructor  TG2FileDataStream.Destroy;
begin
  inherited;
end;

procedure TG2FileDataStream.Read( aChunk : TPatchChunk);
begin
  // Abstract
end;

procedure TG2FileDataStream.Write( aChunk : TPatchChunk; aVariationCount : byte);
begin
  // Abstract
end;


function TG2FileDataStream.LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
var sl : TStringList;
    s : string;
    b : byte;
begin
  Result := False;

  sl := TStringList.Create;
  try
    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( b, SizeOf( b));
      case b of
      $0d :; // just skip it
      $0a : begin
              sl.Add(s);
              s := '';
             end;
      $00 : Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    aChunk.ReadBuffer( 2);
    FPatchVersion := aChunk.ReadBits( 8);
    FPatchType := aChunk.ReadBits( 8);
  finally
    sl.Free;
  end;
end;

{function TG2FileDataStream.ReadClaviaString( aStream : TStream): string;
var b : byte;
begin
  Result := '';
  aStream.Read(b, 1);
  while (b <> 0) do begin
    Result := Result + Char(b);
    if Result.Length = 16 then
      break;
    aStream.Read(b, 1);
  end;
end;

procedure TG2FileDataStream.WriteClaviaString( aStream : TStream; aValue : string);
var i : integer;
    b : Byte;
begin
  i := 0;
  while (i < aValue.Length) and (i<16) do begin
    b := byte(aValue.Chars[i]);
    if aStream.Write( b, 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<16) then begin
    b := 0;
    if aStream.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;}

function TG2FileDataStream.CreateFromMidiStream(const aStream: TStream; aLog : IG2Log): TMemoryStream;
var MData, i, Offs : integer;
    b : byte;
    FName        : string;
    BlockCount   : integer;
    CurrentBlock : integer;
    BitWriter    : TBitWriter;
    Line         : string;
begin
  BlockCount     := 0;
  CurrentBlock   := 0;
  Offs := 0;

  Result := TMemoryStream.Create;

  BitWriter := TBitWriter.Create;
  try
    aStream.Position := 0;
    while aStream.Position < aStream.Size do Begin
      Line := '';
      FChecksum := 0;
      ParseMidiConst( aStream, $f0);            // Start sysex
      ParseMidiConst( aStream, $33);            // Clavia
      //ParseMidiConst( aStream, $7f);          // ?
      ParseMidiSeptet( aStream);                // DeviceID
      ParseMidiConst( aStream, $0a);            // G2

      b := ParseMidiSeptet( aStream);
      case b of
        $20 : PatchType := PATCH_DATA;
        $28 : PatchType := PERF_DATA;
        else
          raise Exception.Create( 'Invalid MIDI, data type changed during load');
      end;

      ParseMidiConst( aStream, $00);            // ?
      ParseMidiSeptet(aStream);                 // SlotIndex
      ParseMidiConst( aStream, $00);            // ?
      MData := ParseMidiInt( aStream);          // Block Number

      Line := Line + 'Block : ' + IntToStr(MData);

      if MData <> CurrentBlock then
        raise Exception.Create(Format( 'Invalid MIDI, block %d expected but block %d seen', [ CurrentBlock, MData]));

      MData := ParseMidiInt( aStream);          // Block count

      Line := Line + ' Count : ' + IntToStr(MData);

      if BlockCount = 0 then
        BlockCount := MData
      else
        if BlockCount <> MData then
          raise Exception.Create(Format( 'Block count changed during MIDI load (%d -> %d)', [ BlockCount, MData]));


      FName := ParseMidiString( aStream);       // Patch name

      Line := Line + ' Name : ' + string(FName) + ' Data : ';

      MData := ParseMidiInt( aStream);          // Septet count
      for i := 0 to MData - 2 do begin
        aStream.Read( b, 1);
        FChecksum := ( FChecksum + b) and $7f;
        BitWriter.WriteBits( Result, b, 7);
      end;
      aStream.Read( b, 1);
      FChecksum := ( FChecksum + b) and $7f;
      b := b shr (7-BitWriter.GetWriteBufferBitsLeft); {!}
      BitWriter.WriteBits( Result, b, BitWriter.GetWriteBufferBitsLeft);

      for i := Offs to Result.Size - 1 do
        Line := Line + IntToHex(PStaticByteBuffer(Result.Memory)^[i], 2) + ' ';
      Offs := Result.Size;


      if assigned(aLog) then
        aLog.add_log_line(Line, LOGCMD_NUL);

      MData := FChecksum;

      if ParseMidiSeptet( aStream) <> MData then // Checksum
        raise Exception.Create(Format( 'Invalid MIDI data, checksum error in block %d', [ CurrentBlock]));

      ParseMidiConst( aStream, $f7);            // End of sysex
      inc( CurrentBlock);
    end;
  finally
    BitWriter.Free;
  end;
end;

procedure TG2FileDataStream.CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
var
  i, bits_left : integer;
  b         : Byte;
Begin
  // Writes one MIDI data block to Result

  FChecksum := 0;                                // Cheksum starts at 0
  AddMidiByte( aMidiStream, $f0);                // Header
  AddMidiByte( aMidiStream, $33);                // Header
  AddMidiByte( aMidiStream, $7f);                // Header
  AddMidiByte( aMidiStream, $0a);                // Header
  case PatchType of
    PATCH_DATA : AddMidiByte( aMidiStream, $20);
    PERF_DATA  : AddMidiByte( aMidiStream, $28);
  else
    raise Exception.Create('Unknown data type.');
  end;
  //AddMidiByte( aMidiStream, PatchType);
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiInt ( aMidiStream, aBlockNr   );        // Block number
  AddMidiInt ( aMidiStream, aBlockCount);        // Block count
  AddMidiName( aMidiStream, 'No name'  );        // Patch name

  aSize := Min( (aChunk.GetReadBufferBitsLeft + 6) div 7, MIDI_BLOCK_SIZE);
  AddMidiInt( aMidiStream, aSize);   // Septet count

  i := 0;
  while i < aSize do begin
    if i = aSize - 1 then begin
      bits_left := aChunk.GetReadBufferBitsLeft mod 8;
      b := aChunk.ReadBits( bits_left);
      b := b shl (7 - bits_left); {! Tricky business}
    end else
      b := aChunk.ReadBits( 7) and $7f;
    AddMidiByte( aMidiStream, b);                // Data septets
    inc(i);
  end;

  b := FChecksum and $7f;
  AddMidiByte( aMidiStream, b);                  // Checksum
  AddMidiByte( aMidiStream, $f7);                // End of sysex
End;

procedure TG2FileDataStream.SaveMidiToStream( const aStream: TStream);
// Saves all data as a set of MIDI blocks to a MIDI file
var
  Offset     : Integer;     // Current septet start offset
  BlockNr    : Integer;     // Current MIDI block number
  BlockCount : Integer;     // Total block count
  Remaining  : Integer;     // Total of remaining septets tp handle
  SaveSize   : Integer;     // Save size for current block, in septets
  FileStream : TMemoryStream;
  Chunk      : TPatchChunk;
  b          : byte;
begin
  FileStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create( FileStream);
  try
    b := $00;
    FileStream.Write( b, 1);

    Chunk.WriteBits( PatchVersion, 8);
    Chunk.WriteBits( Patchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;

    if PatchType = PERF_DATA then // TODO
      (self as TG2Perf).WriteSettings( Chunk);

    Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc( FileStream);

    FileStream.Position := 0;
    Chunk.ReadBuffer( FileStream.Size);
    Remaining  := FileStream.Size;
    //BlockCount := ( DataSize + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockCount :=  (Chunk.GetReadBufferBitsLeft div 7 + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockNr    :=  0;
    Offset     :=  0;
    SaveSize   := Min( Remaining, MIDI_BLOCK_SIZE);
    //while Remaining > 0 do begin
    while Chunk.GetReadBufferBitsLeft > 0 do begin

      CreateMidiBlock( BlockNr, BlockCount, Offset, SaveSize, Chunk, aStream);
      Inc( BlockNr);
      Inc( Offset, MIDI_BLOCK_SIZE);
      Remaining := Remaining - SaveSize;
      SaveSize  := Min( Remaining, MIDI_BLOCK_SIZE);
    end;
  finally
    Chunk.Free;
    FileStream.Free;
  end;
end;

function TG2FileDataStream.ParseMidiConst( const aStream : TStream; aValue: Byte): Byte;
var b : Byte;
Begin
// Reads a constant value from a MIDI file

  aStream.Read(b, 1);
  Result := b;
  if b <> aValue then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x expected but %.2x read', [ aValue, b]));
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiInt( const aStream : TStream): Integer;
Begin
// Reads an Integer (0 .. 16383) value from a MIDI file

  Result := 128 * ParseMidiSeptet( aStream);
  Inc( Result, ParseMidiSeptet( aStream));
end;

function TG2FileDataStream.ParseMidiSeptet( const aStream : TStream): Byte;
var b : Byte;
Begin
// Reads an arbitrary data value from a MIDI file

  aStream.Read(b, 1);
  if b > $7f then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x is not MIDI data', [ b]));
  Result := b;
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiString( const aStream : TStream): string;
var b : Byte;
    i : Integer;
Begin
// Reads a String[ 16]\0 from a MIDI file

  Result := '';
  for i := 1 to 16 do begin // Read the 16 characters
    b := parseMidiSeptet( aStream);
    if b <> 0 then // Don't add \0's to pascal string
      Result := Result + Char( b);
  end;
  ParseMidiConst( aStream, 0); // Read the closing \0
end;

procedure TG2FileDataStream.AddMidiByte( aStream : TStream; aByte: Byte);
begin
// Adds a MIDI byte to aData

  aStream.Write( aByte, 1);
  FChecksum := ( FChecksum + aByte) And $7f;
end;

procedure TG2FileDataStream.AddMidiInt( aStream : TStream; anInteger: Integer);
begin
// Adds an integer (0 .. 16383) to aData

  AddMidiByte( aStream, (anInteger shr 7) and $7f); // Write high septet
  AddMidiByte( aStream, anInteger and $7f);         // then  low  septet
end;

procedure TG2FileDataStream.AddMidiName( aStream : TStream; aName : string);
var i : Integer;
begin
  // Adds the patch name as a String[ 16]\0 to aData

  for i := 0 to 15 do                            // Max 16 septets
    if i >= aName.Length then
      AddMidiByte( aStream, 0)
    else
      AddMidiByte( aStream, Byte( aName.Chars[i]) And $7f);
  AddMidiByte( aStream, $00);                    // and finalizing \0
end;

class function TG2FileDataStream.LoadFileData(aConnection: IG2Connection;
  aStream: TStream): TG2FileDataStream;
var Chunk : TPatchChunk;
    bm, bl : Byte;
    Crc : Word;
    G2FileDataStream : TG2FileDataStream;
begin
  Chunk := TPatchChunk.Create( aStream);
  G2FileDataStream := TG2FileDataStream.Create;
  try
    Chunk.Log := aConnection.Log;

    G2FileDataStream.LoadFileHeader( aStream, Chunk);

    if G2FileDataStream.FPatchType = PATCH_DATA then
      Result := TG2Patch.LoadStreamPatch(aConnection.SelectedSlot, aStream, Chunk)
    else
      Result := TG2Perf.LoadStreamPerf(aConnection, aStream, Chunk);

    Result.FPatchVersion := G2FileDataStream.FPatchVersion;
    Result.FPatchType := G2FileDataStream.FPatchType;

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

  finally
    G2FileDataStream.Free;
    Chunk.Free;
  end;
end;

class function TG2FileDataStream.LoadMidiData(aConnection: IG2Connection;
   aStream: TStream): TG2FileDataStream;
var Chunk            : TPatchChunk;
    MemStream        : TMemoryStream;
    G2FileDataStream : TG2FileDataStream;
begin
  G2FileDataStream := TG2FileDataStream.Create;
  try
    MemStream := G2FileDataStream.CreateFromMidiStream( aStream, aConnection.Log);

    MemStream.Position := 0;
    //MemStream.SaveToFile('TestSysEx_converted.bin');
    Chunk := TPatchChunk.Create( MemStream);
    try
      Chunk.Log := aConnection.Log;

      Chunk.ReadBuffer( 3);

      Chunk.ReadBits( 8);
      Chunk.ReadBits( 8);
      Chunk.ReadBits( 8);

      if G2FileDataStream.PatchType = PATCH_DATA then
        Result := TG2Patch.LoadStreamPatch(aConnection.SelectedSlot, aStream, Chunk)
      else
        Result := TG2Perf.LoadStreamPerf(aConnection, aStream, Chunk);

      Result.PatchVersion := G2FileDataStream.PatchVersion;
      Result.PatchType := G2FileDataStream.PatchType;

    finally
      Chunk.Free;
      MemStream.Free;
    end;

  finally
    G2FileDataStream.Free;
  end;
end;

class function TG2FileDataStream.LoadStreamPatch(aSlot: IG2Slot;
  aStream: TStream; aChunk: TPatchChunk): TG2FileDataStream;
begin
  Result := nil;
end;

class function TG2FileDataStream.LoadStreamPerf(aConnection: IG2Connection;
  aStream: TStream; aChunk: TPatchChunk): TG2FileDataStream;
begin
  Result := nil;
end;

end.
