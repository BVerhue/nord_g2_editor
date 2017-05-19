unit BVE.NMG2Patch;

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
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Stream;

type
  TCurrentNote = class
  private
    FNote: TBits7;
    FAttack: TBits7;
    FRelease: TBits7;
  public
    procedure Init;
    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);
  end;

  TCurrentNoteList = class(TObjectList<TCurrentNote>)
  private
    FLastNote: TCurrentNote;

    FNoteCount: TBits5;

    function GetNote(aIndex: integer): TCurrentNote;
    procedure SetNote(aIndex: integer; const aValue: TCurrentNote);
  public
    constructor Create(AOwnsObjects: boolean);
    destructor Destroy; override;
    procedure Init;
    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);
    function AddNote(aValue: TCurrentNote): integer;
    property Items[aIndex: integer]: TCurrentNote read GetNote
      write SetNote; default;
  end;

  TMorphSetting = class
  private
    FDialArray: array of TBits7;
    FModeArray: array of TBits7;

    function GetDial(aVariation: integer): TBits7;
    procedure SetDial(aVariation: integer; aValue: TBits7);
    function GetMode(aVariation: integer): TBits7;
    procedure SetMode(aVariation: integer; aValue: TBits7);
  public
    constructor Create;
    destructor Destroy; override;

    property Dials[Index: integer]: TBits7 read GetDial write SetDial;
    property Modes[Index: integer]: TBits7 read GetMode write SetMode;
  end;

  TPatchParameters = class;

  TMorphParameters = class
  private
    [Weak]
    FPatchParameters: TPatchParameters;

    FVariationCount: TBits8;
    Unknown1: TBits4;
    Unknown2, Unknown3, Unknown4, Unknown5, Unknown6, Unknown7: TBits8; // 0..5
    Unknown8: TBits4;
    FMorphCount: TBits4;
    FReserved1: integer;
    FReserved2: TBits4;
  public
    constructor Create(aPatchSettings: TPatchParameters);
    destructor Destroy; override;
    procedure Init;
    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk; aVariationCount: byte);
  end;

  TPatchParamList = class(TList<IG2ParamMorph>)
  private
    // Keeps the patch parameter values and a list of the parameter morph values

    FPatchVol: TBits7;
    FActiveMuted: TBits7;
    FGlide: TBits7;
    FGlideTime: TBits7;
    FBend: TBits7;
    FSemi: TBits7;
    FVibrato: TBits7;
    FCents: TBits7;
    FRate: TBits7;
    FArpeggiator: TBits7;
    FArpTime: TBits7;
    FArpType: TBits7;
    FOctaveShift: TBits7;
    FSustain: TBits7;
    FOctaves: TBits7;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Copy(aFromVariation: TPatchParamList);

    procedure AddNewMorphParam(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aRange: byte);
    procedure DelMorphParam(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex: byte);
    function FindMorphParam(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aVariation: byte): IG2ParamMorph;
    procedure DeleteModule(const aLocationIndex, aModuleIndex: byte);
  end;

  TPatchParameters = class
  private
    FLocation: TBits2;
    FSectionCnt: TBits8;
    FMorphArray: array of TMorphSetting; // [0..NMORPHS-1]
    FVariationListArray: array of TPatchParamList;
    function GetPatchParamList(aVariation: integer): TPatchParamList;
    procedure SetPatchParamList(aVariation: integer; aValue: TPatchParamList);
    function GetVariationCount: TBits8;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk; aVariationCount: byte);
    property VariationCount: TBits8 read GetVariationCount;
    property PatchParamList[Index: integer]: TPatchParamList
      read GetPatchParamList write SetPatchParamList;
  end;

  TPatchNotes = class
  private
    FText: packed array of byte;

    procedure SetLines(aLines: TStrings);
    function GetLines: TStrings;
    function GetText: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);

    procedure WriteChunk(aStream: TStream);

    property Text: string read GetText;
    property Lines: TStrings read GetLines write SetLines;
  end;

  TG2Patch = class(TG2FileDataStream, IG2Patch)
  private
    [Weak]
    FWSLot: IG2Slot;

    FPatchPartArray: array of IG2PatchPart; // [0..2]
    FPatchParameters: TPatchParameters;
    FPatchSettings: IG2PatchSettings;
    FMorphParameters: TMorphParameters;
    FKnobList: TList<IG2Knob>;
    FControllerList: TList<IG2Controller>;
    FCurrentNoteList: TCurrentNoteList;
    FPatchNotes: TPatchNotes;
    // Not found it in the file so far
    FSelectedMorphIndex: integer;
    FSelectedLocation: integer;
    FSelectedParamPage: integer;
    FSelectedParamPageColumn: integer;

    FEditAllVariations: boolean;

    procedure DoSelectLocation;
    procedure DoSelectParam(aParam: IG2Param);
    procedure DoLabelValueChange(aParam: IG2Param);
    procedure DoMorphChange(aParamMorph: IG2ParamMorph);

    function GetPatchPart(const aIndex: integer): IG2PatchPart;
    function GetPatchName: string;
    procedure SetPatchName(const aValue: string);
    function GetVariationCount: integer;

    function GetSelectedMorphIndex: integer;
    function GetEditAllVariations: boolean;
    function GetSelectedLocation: integer;
    function GetSelectedParamPage: integer;
    function GetSelectedParamPageColumn: integer;
    function GetPerf: IG2Perf;
    function GetPatchSettings: IG2PatchSettings;
    function GetSlot: IG2Slot;

    procedure SetEditAllVariations(const aValue: boolean);
    procedure SetSelectedParamPage(const aValue: integer);
    procedure SetSelectedParamPageColumn(const aValue: integer);
    function GetSlotIndex: integer;
    function GetLog: IG2Log;

  protected
    function GetKnobList: TList<IG2Knob>;
    function GetControllerList: TList<IG2Controller>;
    procedure SetSelectedMorphIndex(const aValue: integer); virtual;
    procedure SetSelectedLocation(const aLocationIndex: integer); virtual;
  public

    constructor Create(aSlot: IG2Slot);
    destructor Destroy; override;

    class function LoadStreamPatch(aSlot: IG2Slot; aStream: TStream;
      aChunk: TPatchChunk): TG2FileDataStream; override;

    procedure add_log_line(tekst: string; log_cmd: integer);

    procedure Init; virtual;
    procedure InitNames;

    procedure Read(aChunk: TPatchChunk); override;
    function ReadChunk(aChunk: TPatchChunk): boolean;
    procedure ReadKnobList(aKnobList: TList<IG2Knob>; aChunk: TPatchChunk);
    procedure ReadControllerList(aControllerList: TList<IG2Controller>;
      aChunk: TPatchChunk);

    function ReadLedData(aStream: TStream): boolean;
    function ReadVolumeData(aStream: TStream): boolean;

    procedure Write(aChunk: TPatchChunk; aVariationCount: byte); override;
    procedure WriteChunk(aID: byte; aChunk: TPatchChunk; aVariationCount: byte);
    procedure WriteKnobList(aKnobList: TList<IG2Knob>; aChunk: TPatchChunk);
    procedure WriteControllerList(aControllerList: TList<IG2Controller>;
      aChunk: TPatchChunk);

    procedure WriteMessSelectParamPage(const aPageIndex: byte;
      aStream: TStream);

    procedure AddMessPatchSettings(aPatchSettings: IG2PatchSettings;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessPatchNotes(aLines: TStringList;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessAssignKnobs(aModule: IG2Module; const aPageIndex: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);

    function ProcessMsg(aSender: TObject; aMsg: TG2Message): boolean;

    procedure ProcessMsgModuleAdd(aMsg: TG2Message);
    procedure ProcessMsgModuleDelete(aMsg: TG2Message);
    procedure ProcessMsgModuleColor(aMsg: TG2Message);
    procedure ProcessMsgModuleMove(aMsg: TG2Message);
    procedure ProcessMsgModuleUprate(aMsg: TG2Message);

    procedure ProcessMsgModuleLabel(aMsg: TG2Message);
    procedure ProcessMsgMorphSet(aMsg: TG2Message);

    procedure ProcessMsgCableAdd(aMsg: TG2Message);
    procedure ProcessMsgCableDelete(aMsg: TG2Message);
    procedure ProcessMsgCableColor(aMsg: TG2Message);

    procedure ProcessMsgKnobAssign(aMsg: TG2Message);
    procedure ProcessMsgKnobDeassign(aMsg: TG2Message);
    procedure ProcessMsgMidiCCAssign(aMsg: TG2Message);
    procedure ProcessMsgMidiCCDeassign(aMsg: TG2Message);

    procedure ProcessMsgParamPageSelect(aMsg: TG2Message);
    procedure ProcessMsgGlobalParamPageSelect(aMsg: TG2Message);

    procedure ProcessMsgPatchDescrChunk(aMsg: TG2Message);

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    function GetModule(LocationIndex, ModuleIndex: integer): IG2Module;
    // virtual;

    function GetMorphKnobParameter(const aIndex: integer): IG2Param;
    function GetPatchKnobParameter(const aIndex: integer): IG2Param;

    procedure InvalidateParameters;
    procedure InvalidateConnectors;
    procedure InvalidateCables;

    function GetMaxModuleIndex(const aLocationIndex: integer): integer;
    function GetNoOffModuleType(const aLocationIndex,
      aModuleType: byte): integer;
    function GetNoOffExtendedModules: integer;
    // Number of modules that are not compatible with original editor

    function GetParameterValue(const aLocationIndex, aModuleIndex, aParamIndex,
      aVariation: byte): byte;
    procedure SetParamInPatch(const aLocationIndex, aModuleIndex, aParamIndex,
      aVariation, aValue: byte); virtual;
    procedure SetParamValue(const aLocationIndex, aModuleIndex, aParamIndex,
      aVariation, aValue: byte); virtual;

    procedure SelectParamInPatch(const aLocationIndex, aModuleIndex,
      aParamIndex: byte);
    procedure SelectParam(const aLocationIndex, aModuleIndex,
      aParamIndex: byte); virtual;

    procedure SetModeInPatch(const aLocationIndex, aModuleIndex, aParamIndex,
      aValue: byte); virtual;
    function GetModeValue(const aLocationIndex, aModuleIndex,
      aParamIndex: byte): byte;
    procedure SetModeValue(const aLocationIndex, aModuleIndex, aParamIndex,
      aValue: byte); virtual;
    procedure SetMorphValue(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aValue, aVariation: byte); virtual;
    function GetMorphValue(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aVariation: byte): byte;

    procedure SetMasterClock(const aValue: byte);
    function GetMasterClock: byte;
    procedure SetMasterClockRun(const aValue: byte);
    function GetMasterClockRun: byte;

    procedure SaveToFile(aStream: TStream);
{$IFDEF MSWINDOWS}
    procedure SaveAsFXP(aStream: TStream);
{$ENDIF}
    function LoadFromFile(aStream: TStream): boolean;

    procedure DeleteModuleFromPatch(const aLocationIndex: integer;
      aModule: IG2Module);
    procedure DeleteCableFromPatch(const aLocationIndex: integer;
      aCable: IG2Cable); virtual;

    function FindParam(const aLocationIndex, aModuleIndex, aParamIndex: byte)
      : IG2Param;
    function FindKnob(const aLocationIndex, aModuleIndex,
      aParamIndex: byte): IG2Knob;
    function FindController(const aMidiCC: byte): IG2Controller; overload;
    function FindController(const aLocationIndex, aModuleIndex,
      aParamIndex: integer): IG2Controller; overload;

    function FindMorph(const aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aVariation: byte): IG2ParamMorph;
    function HasMorph(const aLocationIndex, aModuleIndex, aParamIndex,
      aVariation: byte): boolean;

    procedure CopyVariation(const aFromVariation, aToVariation: byte);

    procedure UnselectModules(const aLocationIndex: integer);

    property PatchPart[const aIndex: integer]: IG2PatchPart read GetPatchPart;
    property Settings: IG2PatchSettings read GetPatchSettings;
    property SlotIndex: integer read GetSlotIndex;

    property Perf: IG2Perf read GetPerf;
    property Slot: IG2Slot read GetSlot;
    property Log: IG2Log read GetLog;

    property SelectedLocation: integer read GetSelectedLocation
      write SetSelectedLocation;
  end;

implementation

uses
  BVE.NMG2Param,
  BVE.NMG2Knob,
  BVE.NMG2Controller,
  BVE.NMG2PatchSettings,
  BVE.NMG2PatchPart,
  BVE.NMG2Cable;

// ------------------------------------------------------------------------------
//
// TCurrentNote
//
// ------------------------------------------------------------------------------

procedure TCurrentNote.Init;
begin
  FNote := 64;
  FAttack := 0;
  FRelease := 0;
end;

procedure TCurrentNote.Read(aChunk: TPatchChunk);
begin
  FNote := aChunk.ReadBits(7);
  FAttack := aChunk.ReadBits(7);
  FRelease := aChunk.ReadBits(7);
end;

procedure TCurrentNote.Write(aChunk: TPatchChunk);
begin
  aChunk.WriteBits(FNote, 7);
  aChunk.WriteBits(FAttack, 7);
  aChunk.WriteBits(FRelease, 7);
end;

// ------------------------------------------------------------------------------
//
// TCurrentNoteList
//
// ------------------------------------------------------------------------------

constructor TCurrentNoteList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);

  FNoteCount := 0;
  FLastNote := TCurrentNote.Create;
end;

destructor TCurrentNoteList.Destroy;
begin
  FNoteCount := 0;
  FLastNote.Free;

  inherited;
end;

function TCurrentNoteList.GetNote(aIndex: integer): TCurrentNote;
begin
  result := TCurrentNote(inherited Items[aIndex]);
end;

procedure TCurrentNoteList.SetNote(aIndex: integer; const aValue: TCurrentNote);
begin
  inherited Items[aIndex] := aValue;
end;

function TCurrentNoteList.AddNote(aValue: TCurrentNote): integer;
begin
  result := inherited Add(aValue);
end;

procedure TCurrentNoteList.Init;
var
  Note: TCurrentNote;
begin
  FLastNote.Init;

  Clear;
  Note := TCurrentNote.Create;
  Note.Init;
  Add(Note);
end;

procedure TCurrentNoteList.Read(aChunk: TPatchChunk);
var
  i: integer;
  Note: TCurrentNote;
begin
  // FLastNote  : TCurrentNote;
  // FNoteCount : TBits5;
  // FNotes     : array of TCurrentNote;
  Clear;

  FLastNote.Read(aChunk);
  FNoteCount := aChunk.ReadBits(5) + 1; // ?

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Num Notes = ' + IntToStr(FNoteCount), LOGCMD_NUL);
    aChunk.Log.add_log_line('Nte Att Rel', LOGCMD_NUL);
  end;

  for i := 0 to FNoteCount - 1 do
  begin
    Note := TCurrentNote.Create;
    Note.Read(aChunk);

    if assigned(aChunk.Log) then
      aChunk.Log.add_log_line(Format('%3d %3d %3d', [Note.FNote, Note.FAttack,
        Note.FRelease]), LOGCMD_NUL);

    Add(Note);
  end;
end;

procedure TCurrentNoteList.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  FLastNote.Write(aChunk);
  FNoteCount := Count - 1;
  aChunk.WriteBits(FNoteCount, 5);
  for i := 0 to Count - 1 do
    Items[i].Write(aChunk);
end;


// ------------------------------------------------------------------------------
//
// TMorphSetting
//
// ------------------------------------------------------------------------------

constructor TMorphSetting.Create;
begin
  SetLength(FModeArray, 0);
  SetLength(FDialArray, 0);
end;

destructor TMorphSetting.Destroy;
begin
  Finalize(FModeArray);
  Finalize(FDialArray);
  inherited;
end;

function TMorphSetting.GetDial(aVariation: integer): TBits7;
begin
  if aVariation < Length(FDialArray) then
    result := FDialArray[aVariation]
  else
    raise Exception.Create('Dial variation ' + IntToStr(aVariation) +
      ' out of range.');
end;

procedure TMorphSetting.SetDial(aVariation: integer; aValue: TBits7);
begin
  if aVariation < Length(FDialArray) then
    FDialArray[aVariation] := aValue
  else
    raise Exception.Create('Dial variation ' + IntToStr(aVariation) +
      ' out of range.');
end;

function TMorphSetting.GetMode(aVariation: integer): TBits7;
begin
  if aVariation < Length(FModeArray) then
    result := FModeArray[aVariation]
  else
    raise Exception.Create('Mode variation ' + IntToStr(aVariation) +
      ' out of range.');
end;

procedure TMorphSetting.SetMode(aVariation: integer; aValue: TBits7);
begin
  if aVariation < Length(FDialArray) then
    FModeArray[aVariation] := aValue
  else
    raise Exception.Create('Mode variation ' + IntToStr(aVariation) +
      ' out of range.');
end;

// ------------------------------------------------------------------------------
//
// TMorphParameters
//
// ------------------------------------------------------------------------------

constructor TMorphParameters.Create(aPatchSettings: TPatchParameters);
begin
  inherited Create;

  FPatchParameters := aPatchSettings;
end;

destructor TMorphParameters.Destroy;
begin
  inherited;
end;

procedure TMorphParameters.Init;
var
  i: integer;
begin
  // TVariations in TPatchParameters should already be created and initialized

  FVariationCount := Length(FPatchParameters.FVariationListArray);
  FMorphCount := NMORPHS;
  FReserved1 := 0;
  FReserved2 := 0;

  for i := 0 to FVariationCount - 1 do
  begin
    Unknown1 := 0;
    Unknown2 := 0;
    Unknown3 := 0;
    Unknown4 := 0;
    Unknown5 := 0;
    Unknown6 := 0;
    Unknown7 := 0;

    Unknown8 := 0;

    FPatchParameters.FVariationListArray[i].Clear;

    FReserved2 := 0;
  end;
end;

procedure TMorphParameters.Read(aChunk: TPatchChunk);
var
  i, j, k, BitsLeft: integer;
  PatchParamList: TPatchParamList;
  VariationIndex: integer;
  MorphParamCount: integer;
  MorphParam: IG2ParamMorph;
  LocationIndex: TBits2;
  ModuleIndex: TBits8;
  ParamIndex: TBits7;
  MorphIndex: TBits4;
  Range: TBits8;
begin
  // FVariationCount : TBits8;
  // FMorphCount     : TBits4;
  // FReserved1      : integer;
  // FReserved2      : TBits4;

  FVariationCount := aChunk.ReadBits(8);
  FMorphCount := aChunk.ReadBits(4);
  FReserved1 := aChunk.ReadBits(20);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Morph param list, varcount : ' +
      IntToStr(FVariationCount) + ', morphcount : ' + IntToStr(FMorphCount),
      LOGCMD_NUL);
  end;

  for i := 0 to FVariationCount - 1 do
  begin
    VariationIndex := aChunk.ReadBits(4);

    // These should be fields in TPatchParamList, otherwise there overwritten here for each PatchParamList, but maybe they are always 0
    Unknown1 := aChunk.ReadBits(4);
    Unknown2 := aChunk.ReadBits(8);
    Unknown3 := aChunk.ReadBits(8);
    Unknown4 := aChunk.ReadBits(8);
    Unknown5 := aChunk.ReadBits(8);
    Unknown6 := aChunk.ReadBits(8);
    Unknown7 := aChunk.ReadBits(8);

    Unknown8 := aChunk.ReadBits(4);

    MorphParamCount := aChunk.ReadBits(8);

    PatchParamList := FPatchParameters.FVariationListArray[VariationIndex];

    if assigned(aChunk.Log) then
    begin
      aChunk.Log.add_log_line('Morph params, variation : ' +
        IntToStr(VariationIndex) + ', count = ' + IntToStr(MorphParamCount),
        LOGCMD_NUL);
      aChunk.Log.add_log_line('Loc Mod Par Mor Rng', LOGCMD_NUL);
    end;

    for j := 0 to MorphParamCount - 1 do
    begin

      LocationIndex := aChunk.ReadBits(2);
      ModuleIndex := aChunk.ReadBits(8);
      ParamIndex := aChunk.ReadBits(7);
      MorphIndex := aChunk.ReadBits(4);
      Range := aChunk.ReadBits(8);

      if assigned(aChunk.Log) then
        aChunk.Log.add_log_line(Format('%3d %3d %3d %3d %3d',
          [LocationIndex, ModuleIndex, ParamIndex, MorphIndex, Range]),
          LOGCMD_NUL);

      k := 0;
      while (k < PatchParamList.Count) and
        not((LocationIndex = byte(ord(PatchParamList[k].LocationIndex))) and
        (ModuleIndex = PatchParamList[k].ModuleIndex) and
        (ParamIndex = PatchParamList[k].ParamIndex) and
        (MorphIndex = PatchParamList[k].MorphIndex)) do
        inc(k);

      if k < PatchParamList.Count then
      begin
        MorphParam := PatchParamList[k];
        MorphParam.Range := Range;
      end
      else
      begin
        MorphParam := TG2ParamMorph.Create;
        // MorphParam.Read( aChunk);
        MorphParam.LocationIndex := LocationIndex;
        MorphParam.ModuleIndex := ModuleIndex;
        MorphParam.ParamIndex := ParamIndex;
        MorphParam.MorphIndex := MorphIndex;
        MorphParam.Range := Range;
        PatchParamList.Add(MorphParam);
      end;
    end;

    BitsLeft := aChunk.GetReadBufferBitsLeft;
    if BitsLeft < 4 then
      // At the end of the chunk we need to read the bits that are left over to align to byte
      FReserved2 := aChunk.ReadBits(BitsLeft)
    else
      FReserved2 := aChunk.ReadBits(4);
  end;
end;

procedure TMorphParameters.Write(aChunk: TPatchChunk; aVariationCount: byte);
var
  i, j: integer;
  PatchParamList: TPatchParamList;
begin
  // NumVariations := min( FVariationCount, FPatch.FMaxVariations);
  // aChunk.WriteBits(FVariationCount, 8);
  aChunk.WriteBits(aVariationCount, 8);
  aChunk.WriteBits(FMorphCount, 4);
  aChunk.WriteBits(FReserved1, 20);

  for i := 0 to aVariationCount - 1 do
  begin

    PatchParamList := FPatchParameters.FVariationListArray[i];

    aChunk.WriteBits(i, 4);

    aChunk.WriteBits(Unknown1, 4);
    aChunk.WriteBits(Unknown2, 8);
    aChunk.WriteBits(Unknown3, 8);
    aChunk.WriteBits(Unknown4, 8);
    aChunk.WriteBits(Unknown5, 8);
    aChunk.WriteBits(Unknown6, 8);
    aChunk.WriteBits(Unknown7, 8);

    aChunk.WriteBits(Unknown8, 4);

    aChunk.WriteBits(PatchParamList.Count, 8);
    for j := 0 to PatchParamList.Count - 1 do
    begin
      PatchParamList.Items[j].Write(aChunk);
    end;

    if i < (aVariationCount - 1) then
      aChunk.WriteBits(FReserved2, 4)
    else
      // At the end of the chunk we need to write the bits that are left over to align to byte
      if aChunk.FBitWriter.GetWriteBufferBitsLeft < 8 then
        // It seems that in case of performances when this is <> 0, the perf. failes to load!
        aChunk.WriteBits( { FReserved2 } 0,
          aChunk.FBitWriter.GetWriteBufferBitsLeft);
  end;
end;

// ------------------------------------------------------------------------------
//
// TPatchParamList
//
// ------------------------------------------------------------------------------

constructor TPatchParamList.Create;
begin
  inherited;
end;

destructor TPatchParamList.Destroy;
begin
  inherited;
end;

procedure TPatchParamList.Copy(aFromVariation: TPatchParamList);
begin
  FPatchVol := aFromVariation.FPatchVol;
  FGlide := aFromVariation.FGlide;
  FGlideTime := aFromVariation.FGlideTime;
  FBend := aFromVariation.FBend;
  FSemi := aFromVariation.FSemi;
  FVibrato := aFromVariation.FVibrato;
  FCents := aFromVariation.FCents;
  FRate := aFromVariation.FRate;
  FArpeggiator := aFromVariation.FArpeggiator;
  FArpTime := aFromVariation.FArpTime;
  FArpType := aFromVariation.FArpType;
  FOctaves := aFromVariation.FOctaves;
  FSustain := aFromVariation.FSustain;
  FOctaveShift := aFromVariation.FOctaveShift;
end;

function TPatchParamList.FindMorphParam(const aLocationIndex, aModuleIndex,
  aParamIndex, aMorphIndex, aVariation: byte): IG2ParamMorph;
var
  i: integer;
begin
  result := nil;
  i := 0;
  while (i < Count) and not((Items[i].LocationIndex = aLocationIndex) and
    (Items[i].ModuleIndex = aModuleIndex) and
    (Items[i].ParamIndex = aParamIndex) and
    (Items[i].MorphIndex = aMorphIndex)) do
    inc(i);

  if (i < Count) then
    result := Items[i];
end;

procedure TPatchParamList.AddNewMorphParam(const aLocationIndex, aModuleIndex,
  aParamIndex, aMorphIndex, aRange: byte);
var
  MorphParam: IG2ParamMorph;
begin
  MorphParam := TG2ParamMorph.Create;
  MorphParam.LocationIndex := aLocationIndex;
  MorphParam.ModuleIndex := aModuleIndex;
  MorphParam.ParamIndex := aParamIndex;
  MorphParam.MorphIndex := aMorphIndex;
  MorphParam.Range := aRange;
  Add(MorphParam);
end;

procedure TPatchParamList.DelMorphParam(const aLocationIndex, aModuleIndex,
  aParamIndex, aMorphIndex: byte);
var
  i: integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].LocationIndex = aLocationIndex) and
    (Items[i].ModuleIndex = aModuleIndex) and
    (Items[i].ParamIndex = aParamIndex) and
    (Items[i].MorphIndex = aMorphIndex)) do
    inc(i);

  if (i < Count) then
  begin
    Delete(i);
  end;
end;

procedure TPatchParamList.DeleteModule(const aLocationIndex,
  aModuleIndex: byte);
var
  i: integer;
begin
  i := 0;
  while (i < Count) do
  begin
    if ((Items[i].LocationIndex = aLocationIndex) and
      (Items[i].ModuleIndex = aModuleIndex)) then
      Delete(i)
    else
      inc(i);
  end;
end;

// ------------------------------------------------------------------------------
//
// TPatchParameters
//
// ------------------------------------------------------------------------------

constructor TPatchParameters.Create;
var
  i: integer;
begin
  inherited Create;

  // Start to create 9 Variations
  SetLength(FVariationListArray, N_USB_VARIATIONS);
  for i := 0 to N_USB_VARIATIONS - 1 do
    FVariationListArray[i] := TPatchParamList.Create;

  SetLength(FMorphArray, NMORPHS);
  for i := 0 to NMORPHS - 1 do
  begin
    FMorphArray[i] := TMorphSetting.Create;
    SetLength(FMorphArray[i].FDialArray, N_USB_VARIATIONS);
    SetLength(FMorphArray[i].FModeArray, N_USB_VARIATIONS);
  end;
end;

procedure TPatchParameters.Init;
var
  i, j: integer;
begin
  FLocation := 2;
  FSectionCnt := 7;

  for i := 0 to VariationCount - 1 do
  begin
    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FDialArray[i] := 0;

    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FModeArray[i] := 1;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FPatchVol := 100;
    FVariationListArray[i].FActiveMuted := 1;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FGlide := 0;
    FVariationListArray[i].FGlideTime := 28;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FBend := 1;
    FVariationListArray[i].FSemi := 1;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FVibrato := 0;
    FVariationListArray[i].FCents := 50;
    FVariationListArray[i].FRate := 64;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FArpeggiator := 0;
    FVariationListArray[i].FArpTime := 3;
    FVariationListArray[i].FArpType := 0;
    FVariationListArray[i].FOctaves := 0;
  end;

  for i := 0 to VariationCount - 1 do
  begin
    FVariationListArray[i].FOctaveShift := 2;
    FVariationListArray[i].FSustain := 1;
  end;
end;

destructor TPatchParameters.Destroy;
var
  i: integer;
begin
  for i := 0 to NMORPHS - 1 do
  begin
    Finalize(FMorphArray[i].FDialArray);
    Finalize(FMorphArray[i].FModeArray);
    FMorphArray[i].Free;
  end;
  Finalize(FMorphArray);

  for i := 0 to Length(FVariationListArray) - 1 do
    FVariationListArray[i].Free;
  Finalize(FVariationListArray);

  inherited;
end;

function TPatchParameters.GetPatchParamList(aVariation: integer)
  : TPatchParamList;
begin
  if aVariation < VariationCount then
    result := FVariationListArray[aVariation]
  else
    raise Exception.Create('Variation index ' + IntToStr(aVariation) +
      ' out of range.');
end;

function TPatchParameters.GetVariationCount: TBits8;
begin
  result := Length(FVariationListArray);
end;

procedure TPatchParameters.SetPatchParamList(aVariation: integer;
  aValue: TPatchParamList);
begin
  if aVariation < VariationCount then
    FVariationListArray[aVariation] := aValue
  else
    raise Exception.Create('Variation index ' + IntToStr(aVariation) +
      ' out of range.');
end;

procedure TPatchParameters.Read(aChunk: TPatchChunk);
var
  i, j, Variation, SrceVarCount: integer;
begin
  // FLocation           : Tbits2;
  // FSectionCnt         : TBits8;
  // FVariationCount     : TBits8;
  // FMorphArray         : array of TMorph;
  // FVariationListArray : array of TPatchParamList;

  FSectionCnt := aChunk.ReadBits(8);
  SrceVarCount := aChunk.ReadBits(8);

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FDialArray[Variation] := aChunk.ReadBits(7);

    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FModeArray[Variation] := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FPatchVol := aChunk.ReadBits(7);
    FVariationListArray[Variation].FActiveMuted := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FGlide := aChunk.ReadBits(7);
    FVariationListArray[Variation].FGlideTime := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FBend := aChunk.ReadBits(7);
    FVariationListArray[Variation].FSemi := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FVibrato := aChunk.ReadBits(7);
    FVariationListArray[Variation].FCents := aChunk.ReadBits(7);
    FVariationListArray[Variation].FRate := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FArpeggiator := aChunk.ReadBits(7);
    FVariationListArray[Variation].FArpTime := aChunk.ReadBits(7);
    FVariationListArray[Variation].FArpType := aChunk.ReadBits(7);
    FVariationListArray[Variation].FOctaves := aChunk.ReadBits(7);
  end;

  aChunk.ReadBits(8); // Sections
  aChunk.ReadBits(7); // Entries

  for i := 0 to SrceVarCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    FVariationListArray[Variation].FOctaveShift := aChunk.ReadBits(7);
    FVariationListArray[Variation].FSustain := aChunk.ReadBits(7);
  end;
end;

procedure TPatchParameters.Write(aChunk: TPatchChunk; aVariationCount: byte);
var
  i, j: integer;
begin
  aChunk.WriteBits(FSectionCnt, 8);
  aChunk.WriteBits(aVariationCount, 8);

  aChunk.WriteBits(1, 8);
  aChunk.WriteBits(16, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphArray[j].FDialArray[i], 7);

    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphArray[j].FModeArray[i], 7);
  end;

  aChunk.WriteBits(2, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FPatchVol, 7);
    aChunk.WriteBits(FVariationListArray[i].FActiveMuted, 7);
  end;

  aChunk.WriteBits(3, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FGlide, 7);
    aChunk.WriteBits(FVariationListArray[i].FGlideTime, 7);
  end;

  aChunk.WriteBits(4, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FBend, 7);
    aChunk.WriteBits(FVariationListArray[i].FSemi, 7);
  end;

  aChunk.WriteBits(5, 8);
  aChunk.WriteBits(3, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FVibrato, 7);
    aChunk.WriteBits(FVariationListArray[i].FCents, 7);
    aChunk.WriteBits(FVariationListArray[i].FRate, 7);
  end;

  aChunk.WriteBits(6, 8);
  aChunk.WriteBits(4, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FArpeggiator, 7);
    aChunk.WriteBits(FVariationListArray[i].FArpTime, 7);
    aChunk.WriteBits(FVariationListArray[i].FArpType, 7);
    aChunk.WriteBits(FVariationListArray[i].FOctaves, 7);
  end;

  aChunk.WriteBits(7, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do
  begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FOctaveShift, 7);
    aChunk.WriteBits(FVariationListArray[i].FSustain, 7);
  end;
end;

// ------------------------------------------------------------------------------
//
// TPatchNotes
//
// ------------------------------------------------------------------------------

constructor TPatchNotes.Create;
begin
  SetLength(FText, 0);
end;

destructor TPatchNotes.Destroy;
begin
  Finalize(FText);

  inherited;
end;

function TPatchNotes.GetLines: TStrings;
var
  i: integer;
begin
  result := TStringList.Create;
  for i := 0 to Length(FText) - 1 do
    result.Text := result.Text + Char(FText[i]);
end;

procedure TPatchNotes.SetLines(aLines: TStrings);
var
  i: integer;
  b: byte;
  notes: string;
begin
  notes := aLines.Text;
  SetLength(FText, notes.Length);
  for i := 0 to Length(FText) - 1 do
  begin
    b := byte(notes.Chars[i]);
    FText[i] := b;
  end;
end;

procedure TPatchNotes.Init;
begin
  SetLength(FText, 0);
end;

procedure TPatchNotes.Read(aChunk: TPatchChunk);
begin
  aChunk.FReadBuffer.Read(FText[0], Length(FText));
end;

procedure TPatchNotes.Write(aChunk: TPatchChunk);
begin
  aChunk.FWriteBuffer.Write(FText[0], Length(FText));
end;

procedure TPatchNotes.WriteChunk(aStream: TStream);
var
  Chunk: TPatchChunk;
begin
  Chunk := TPatchChunk.Create(aStream);
  try
    Write(Chunk);
    Chunk.WriteChunk(C_PATCH_NOTES);
  finally
    Chunk.Free;
  end;
end;

function TPatchNotes.GetText: string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Length(FText) - 1 do
  begin
    result := result + Char(FText[i]);
  end;
end;

// ------------------------------------------------------------------------------
//
// TG2Patch
//
// ------------------------------------------------------------------------------

constructor TG2Patch.Create(aSlot: IG2Slot);
var
  i, KnobCount: integer;
  Knob: IG2Knob;
begin
  inherited Create;

  SetWeak(@FWSLot, aSlot);

  FPatchSettings := TG2PatchSettings.Create;

  SetLength(FPatchPartArray, 3);
  for i := 0 to 2 do // FX, VA and Patch
    FPatchPartArray[i] := TG2PatchPart.Create(self as IG2Patch, i);

  FCurrentNoteList := TCurrentNoteList.Create(True);
  FPatchParameters := TPatchParameters.Create;
  FMorphParameters := TMorphParameters.Create(FPatchParameters);
  FKnobList := TList<IG2Knob>.Create;
  KnobCount := 120;
  for i := 0 to KnobCount - 1 do
  begin
    Knob := TG2Knob.Create(i, nil);
    FKnobList.Add(Knob);
  end;
  FPatchNotes := TPatchNotes.Create;
  FControllerList := TList<IG2Controller>.Create;
  Init;
end;

destructor TG2Patch.Destroy;
begin
  FPatchSettings := nil;

  FControllerList.Free;
  FPatchNotes.Free;
  FKnobList.Free;
  FMorphParameters.Free;
  FPatchParameters.Free;
  FCurrentNoteList.Free;
  Finalize(FPatchPartArray);

  SetWeak(@FWSLot, nil);

  inherited;
end;

procedure TG2Patch.Init;
var
  i: integer;
  Knob: IG2Knob;
  Controller: IG2Controller;
  Param: IG2Param;
begin
  PatchVersion := PATCH_VERSION;
  PatchType := 0;

  if assigned(Perf) then
    Perf.ClearKnobs(SlotIndex);

  FPatchSettings.Init;

  for Knob in FKnobList do
    Knob.Init;

  FControllerList.Clear;

  FCurrentNoteList.Init;

  FPatchParameters.Init;
  FPatchParameters.FLocation := 2;

  FMorphParameters.Init;

  for i := 0 to 2 do
  begin
    FPatchPartArray[i].Init;
  end;

  Param := FindParam(LOCATION_PATCH, PATCH_VOLUME, VOLUME_LEVEL);
  Controller := TController.Create(7, Param);
  FControllerList.Add(Controller);

  Param := FindParam(LOCATION_PATCH, PATCH_SUSTAIN, OCTAVE_SHIFT);
  Controller := TController.Create(17, Param);
  FControllerList.Add(Controller);

  FPatchNotes.Init;

  FEditAllVariations := False;
  FSelectedParamPage := 0;
  FSelectedParamPageColumn := 0;
  FSelectedLocation := LOCATION_VA;
end;

class function TG2Patch.LoadStreamPatch(aSlot: IG2Slot; aStream: TStream;
  aChunk: TPatchChunk): TG2FileDataStream;
begin
  result := TG2Patch.Create(aSlot);
  aChunk.ReadChunk;
  result.Read(aChunk);
end;

procedure TG2Patch.DoLabelValueChange(aParam: IG2Param);
begin
  NotifyObservers(EvtLabelValueChange, aParam);

  if assigned(aParam) then
  begin
    aParam.InvalidateControl;
  end;
end;

procedure TG2Patch.DoMorphChange(aParamMorph: IG2ParamMorph);
begin
  NotifyObservers(EvtMorphChange, aParamMorph);
end;

procedure TG2Patch.DoSelectLocation;
begin
  NotifyObservers(EvtSelectLocation, self as IG2Patch);
end;

procedure TG2Patch.DoSelectParam(aParam: IG2Param);
begin
  NotifyObservers(EvtSelectParam, aParam);
end;

procedure TG2Patch.NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);
begin
  if assigned(Slot) then
    Slot.NotifyObservers(aG2Event, aG2Object);
end;

function TG2Patch.ProcessMsg(aSender: TObject; aMsg: TG2Message): boolean;
var
  Cmd: byte;
  LocationIndex: byte;
  Chunk: TPatchChunk;
begin
  result := True; // Return True if it's a patch command

  repeat
    aMsg.Read(Cmd, 1);

    case Cmd of
      S_SET_UPRATE: // Uprate or downrate module
        begin
          ProcessMsgModuleUprate(aMsg);
        end;
      S_ADD_MODULE: // Add a module
        begin
          ProcessMsgModuleAdd(aMsg);
        end;
      S_SET_MODULE_COLOR: // Change module color
        begin
          ProcessMsgModuleColor(aMsg);
        end;
      S_DEL_MODULE: // Delete module
        begin
          ProcessMsgModuleDelete(aMsg);
        end;
      S_MOV_MODULE: // Move module
        begin
          ProcessMsgModuleMove(aMsg);
        end;
      S_ADD_CABLE: // Add a cable
        begin
          ProcessMsgCableAdd(aMsg);
        end;
      S_DEL_CABLE: // Delete a cable
        begin
          ProcessMsgCableDelete(aMsg);
        end;
      S_CABLE_COLOR: // Change cable color
        begin
          ProcessMsgCableColor(aMsg);
        end;
      S_ASSIGN_KNOB: // Assign knob
        begin
          ProcessMsgKnobAssign(aMsg);
        end;
      S_DEASSIGN_KNOB: // Deassign knob
        begin
          ProcessMsgKnobDeassign(aMsg);
        end;
      S_ASSIGN_MIDICC: // Assign midi controler
        begin
          ProcessMsgMidiCCAssign(aMsg);
        end;
      S_DEASSIGN_MIDICC: // Deassign midi controler
        begin
          ProcessMsgMidiCCDeassign(aMsg);
        end;
      S_SEL_PARAM_PAGE: // Select parameter page
        begin
          ProcessMsgParamPageSelect(aMsg);
        end;
      S_ASS_GLOBAL_KNOB: // Assign knob in global page
        begin
          Perf.ProcessMsgGlobalKnobAssign(aMsg);
        end;
      S_DEASS_GLOB_KNOB: // Deassign global knob
        begin
          Perf.ProcessMsgGlobalKnobDeassign(aMsg);
        end;
      S_SEL_GLOBAL_PAGE: // Select global parameter page
        begin
          ProcessMsgGlobalParamPageSelect(aMsg);
        end;
      S_SET_PARAM_LABEL:
        begin
          LocationIndex := ReadByte(aMsg);
          FPatchPartArray[LocationIndex].ProcessMsgModuleParamLabel(aMsg);
        end;
      S_SET_MODULE_LABEL:
        begin
          ProcessMsgModuleLabel(aMsg);
        end;
      S_SET_MORPH_RANGE:
        begin
          ProcessMsgMorphSet(aMsg);
        end;
      C_PATCH_DESCR, C_MODULE_LIST, C_CURRENT_NOTE_2, C_CABLE_LIST,
        C_PARAM_LIST, C_MORPH_PARAM, C_KNOBS, C_CONTROLLERS, C_PARAM_NAMES,
        C_MODULE_NAMES, C_PATCH_NOTES:
        begin // Process chunks in add module message

          aMsg.Position := aMsg.Position - 1;
          Chunk := TPatchChunk.Create(aMsg);
          try
            Chunk.Log := Log;
            Chunk.ReadChunk;
            result := ReadChunk(Chunk);
          finally
            Chunk.Free;
          end;
        end;
    else
      result := False; // not a patch command
    end;
  until (result = False) or (aMsg.Position >= aMsg.Size - 2);
end;

procedure TG2Patch.ProcessMsgCableAdd(aMsg: TG2Message);
var
  BitReader: TBitReader;
  LocationIndex,
  // Unknown,
  ModuleIndexFrom, ConnFromKind, ConnIndexFrom, ModuleIndexTo,
  // ToConnectorKind,
  ConnIndexTo, CableColor, LinkType: byte;
  Cable: IG2Cable;
  ModuleFrom, ModuleTo: IG2Module;
  ConnFrom, ConnTo: IG2Connector;
begin
  BitReader := TBitReader.Create;
  try
    // Unknown            := BitReader.ReadBits( aMsg, 4);
    BitReader.ReadBits(aMsg, 4); // Unknown
    LocationIndex := BitReader.ReadBits(aMsg, 1);
    CableColor := BitReader.ReadBits(aMsg, 3);
    ModuleIndexFrom := BitReader.ReadBits(aMsg, 8);
    ConnFromKind := BitReader.ReadBits(aMsg, 2);
    ConnIndexFrom := BitReader.ReadBits(aMsg, 6);
    ModuleIndexTo := BitReader.ReadBits(aMsg, 8);
    // ToConnectorKind    := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits(aMsg, 2); // ToConnectorKind
    ConnIndexTo := BitReader.ReadBits(aMsg, 6);

    if FPatchPartArray[LocationIndex].FindCable(ModuleIndexFrom, ConnIndexFrom,
      ModuleIndexTo, ConnIndexTo) = nil then
    begin
      if TConnectorKind(ConnFromKind) = ckInput then
        LinkType := 0
      else
        LinkType := 1;

      ModuleFrom := FPatchPartArray[LocationIndex].FindModule(ModuleIndexFrom);
      ModuleTo := FPatchPartArray[LocationIndex].FindModule(ModuleIndexTo);
      if assigned(ModuleFrom) and assigned(ModuleTo) then
      begin
        if LinkType = 0 then
          ConnFrom := ModuleFrom.InConnector[ConnIndexFrom]
        else
          ConnFrom := ModuleFrom.OutConnector[ConnIndexFrom];
        ConnTo := ModuleTo.InConnector[ConnIndexTo];

        if assigned(ConnFrom) and assigned(ConnTo) then
        begin
          Cable := TG2FileCable.Create(ConnFrom, ConnTo, CableColor);
          PatchPart[LocationIndex].AddCable(Cable);

          ConnFrom.NotifyObservers(evtCableAdd, Cable);
          ConnTo.NotifyObservers(evtCableAdd, Cable);
        end;
      end;

    end
    else
      add_log_line('Cable already exists.', LOGCMD_ERR);
  finally
    BitReader.Free;
  end;
end;

procedure TG2Patch.ProcessMsgCableColor(aMsg: TG2Message);
var
  BitReader: TBitReader;
  LocationIndex,
  // Unknown,
  FromModuleIndex,
  // FromConnectorKind,
  FromConnectorIndex, ToModuleIndex,
  // ToConnectorKind,
  ToConnectorIndex, CableColor: byte;
  Cable: IG2Cable;
  ConnFrom, ConnTo: IG2Connector;
begin
  BitReader := TBitReader.Create;
  try
    // Unknown            := BitReader.ReadBits( aMsg, 4);
    BitReader.ReadBits(aMsg, 4);
    LocationIndex := BitReader.ReadBits(aMsg, 1);
    CableColor := BitReader.ReadBits(aMsg, 3);
    FromModuleIndex := BitReader.ReadBits(aMsg, 8);
    // FromConnectorKind  := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits(aMsg, 2);
    FromConnectorIndex := BitReader.ReadBits(aMsg, 6);
    ToModuleIndex := BitReader.ReadBits(aMsg, 8);
    // ToConnectorKind    := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits(aMsg, 2);
    ToConnectorIndex := BitReader.ReadBits(aMsg, 6);

    Cable := FPatchPartArray[LocationIndex].FindCable(FromModuleIndex,
      FromConnectorIndex, ToModuleIndex, ToConnectorIndex);
    if assigned(Cable) then
    begin
      ConnFrom := Cable.ConnFrom;
      ConnTo := Cable.ConnTo;

      Cable.CableColor := TCableColor(CableColor);

      NotifyObservers(EvtCableColor, Cable);
      ConnFrom.NotifyObservers(EvtCableColor, Cable);
      ConnTo.NotifyObservers(EvtCableColor, Cable);
    end
    else
    begin
      // Try the other way around...
      Cable := FPatchPartArray[LocationIndex].FindCable(ToModuleIndex,
        ToConnectorIndex, FromModuleIndex, FromConnectorIndex);
      if assigned(Cable) then
      begin
        ConnFrom := Cable.ConnFrom;
        ConnTo := Cable.ConnTo;

        Cable.CableColor := TCableColor(CableColor);

        NotifyObservers(EvtCableColor, Cable);
        ConnFrom.NotifyObservers(EvtCableColor, Cable);
        ConnTo.NotifyObservers(EvtCableColor, Cable);
      end
      else
      begin
        add_log_line(' Delete cable : cable in location ' +
          IntToStr(LocationIndex) + ' from module ' + IntToStr(FromModuleIndex)
          + ' connector ' + IntToStr(FromConnectorIndex) + ' to module ' +
          IntToStr(ToModuleIndex) + ' connector ' + IntToStr(ToConnectorIndex) +
          ' not found.', LOGCMD_ERR);
      end;
    end;
  finally
    BitReader.Free;
  end;
end;

procedure TG2Patch.ProcessMsgCableDelete(aMsg: TG2Message);
var
  BitReader: TBitReader;
  LocationIndex,
  // Unknown,
  FromModuleIndex,
  // FromConnectorKind,
  FromConnectorIndex, ToModuleIndex,
  // ToConnectorKind,
  ToConnectorIndex: byte;
  Cable: IG2Cable;
  ConnFrom, ConnTo: IG2Connector;
begin
  BitReader := TBitReader.Create;
  try
    // Unknown            := BitReader.ReadBits( aMsg, 7);
    BitReader.ReadBits(aMsg, 7);
    LocationIndex := BitReader.ReadBits(aMsg, 1);
    FromModuleIndex := BitReader.ReadBits(aMsg, 8);
    // FromConnectorKind  := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits(aMsg, 2);
    FromConnectorIndex := BitReader.ReadBits(aMsg, 6);
    ToModuleIndex := BitReader.ReadBits(aMsg, 8);
    // ToConnectorKind    := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits(aMsg, 2);
    ToConnectorIndex := BitReader.ReadBits(aMsg, 6);

    Cable := FPatchPartArray[LocationIndex].FindCable(FromModuleIndex,
      FromConnectorIndex, ToModuleIndex, ToConnectorIndex);
    if assigned(Cable) then
    begin
      ConnFrom := Cable.ConnFrom;
      ConnTo := Cable.ConnTo;

      DeleteCableFromPatch(LocationIndex, Cable);

      NotifyObservers(EvtCableDelete, Cable);
      ConnFrom.NotifyObservers(EvtCableDelete, Cable);
      ConnTo.NotifyObservers(EvtCableDelete, Cable);
    end
    else
    begin
      // Try the other way around...
      Cable := FPatchPartArray[LocationIndex].FindCable(ToModuleIndex,
        ToConnectorIndex, FromModuleIndex, FromConnectorIndex);
      if assigned(Cable) then
      begin
        ConnFrom := Cable.ConnFrom;
        ConnTo := Cable.ConnTo;

        DeleteCableFromPatch(LocationIndex, Cable);

        NotifyObservers(EvtCableDelete, Cable);
        ConnFrom.NotifyObservers(EvtCableDelete, Cable);
        ConnTo.NotifyObservers(EvtCableDelete, Cable);
      end
      else
      begin
        add_log_line(' Delete cable : cable in location ' +
          IntToStr(LocationIndex) + ' from module ' + IntToStr(FromModuleIndex)
          + ' connector ' + IntToStr(FromConnectorIndex) + ' to module ' +
          IntToStr(ToModuleIndex) + ' connector ' + IntToStr(ToConnectorIndex) +
          ' not found.', LOGCMD_ERR);
      end;
    end;
  finally
    BitReader.Free;
  end;
end;

procedure TG2Patch.ProcessMsgGlobalParamPageSelect(aMsg: TG2Message);
begin
  // TODO
end;

procedure TG2Patch.ProcessMsgKnobAssign(aMsg: TG2Message);
var
  ModuleIndex, ParamIndex, LocationIndex, KnobIndex: byte;
  BitReader: TBitReader;
  Knob: IG2Knob;
  Param: IG2Param;
begin
  BitReader := TBitReader.Create;
  try
    ModuleIndex := BitReader.ReadBits(aMsg, 8);
    ParamIndex := BitReader.ReadBits(aMsg, 8);
    LocationIndex := BitReader.ReadBits(aMsg, 2);
    // Unknown       := BitReader.ReadBits( aMsg, 6);
    // Unknown       := BitReader.ReadBits( aMsg, 8);
    BitReader.ReadBits(aMsg, 6); // Unknown
    BitReader.ReadBits(aMsg, 8); // Unknown
    KnobIndex := BitReader.ReadBits(aMsg, 8);

    Knob := FKnobList.Items[KnobIndex];
    Param := FindParam(LocationIndex, ModuleIndex, ParamIndex);
    // Knob.IsAssigned := 1;
    // Knob.LocationIndex := LocationIndex;
    // Knob.ModuleIndex := ModuleIndex;
    // Knob.ParamIndex := ParamIndex;
    // Param.Knob := Knob;

    NotifyObservers(EvtAssignKnob, Knob);
  finally
    BitReader.Free;
  end;
end;

procedure TG2Patch.ProcessMsgMidiCCAssign(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex, ParamIndex, MidiCC: byte;
  Param: IG2Param;
  Controller: IG2Controller;
begin
  aMsg.Read(LocationIndex, 1);
  aMsg.Read(ModuleIndex, 1);
  aMsg.Read(ParamIndex, 1);
  aMsg.Read(MidiCC, 1);

  Param := FindParam(LocationIndex, ModuleIndex, ParamIndex);
  Controller := FindController(MidiCC);
  if not assigned(Controller) then
  begin
    Controller := TController.Create(MidiCC, Param);
    { Controller.Location := LocationIndex;
      Controller.ModuleIndex := ModuleIndex;
      Controller.ParamIndex := ParamIndex;
      Controller.MidiCC := MidiCC; }

    FControllerList.Add(Controller);
  end
  else
    Controller.Param := Param;

  NotifyObservers(EvtAssignMidiCC, Controller);
end;

procedure TG2Patch.ProcessMsgKnobDeassign(aMsg: TG2Message);
var
  Unknown, KnobIndex: byte;
  Knob: IG2Knob;
begin
  aMsg.Read(Unknown, 1);
  aMsg.Read(KnobIndex, 1);

  Knob := FKnobList[KnobIndex];
  if assigned(Knob) and (Knob.IsAssigned = 1) then
  begin
    Knob.IsAssigned := 0;

    NotifyObservers(EvtDeassignKnob, Knob);
  end;
end;

procedure TG2Patch.ProcessMsgMidiCCDeassign(aMsg: TG2Message);
var
  MidiCC: byte;
  Controller: IG2Controller;
  i: integer;
begin
  aMsg.Read(MidiCC, 1);

  Controller := FindController(MidiCC);
  if assigned(Controller) then
  begin
    // Controller.Parameter.Controller := nil;
    i := FControllerList.IndexOf(Controller);
    FControllerList.Delete(i);
  end;
end;

procedure TG2Patch.ProcessMsgModuleAdd(aMsg: TG2Message);
var
  LocationIndex, ModuleType, ModuleIndex: byte;
  Module: IG2Module;
  i: integer;
begin
  ModuleType := ReadByte(aMsg);
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);

  Module := PatchPart[LocationIndex].CreateModule(ModuleIndex, ModuleType);

  Module.Col := ReadByte(aMsg);
  Module.Row := ReadByte(aMsg);
  Module.ModuleColor := ReadByte(aMsg);
  Module.Uprate := ReadByte(aMsg);
  Module.IsLed := ReadByte(aMsg);
  for i := 0 to Module.ModeCount - 1 do
    Module.ModeInfo[i] := ReadByte(aMsg);
  Module.ModuleName := '';
  Module.ModuleName := ReadClaviaString(aMsg);

  PatchPart[LocationIndex].AddModule(Module);
  Slot.NotifyObservers(evtModuleAdd, Module);
end;

procedure TG2Patch.ProcessMsgModuleColor(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex: byte;
  Module: IG2Module;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  Module := GetModule(LocationIndex, ModuleIndex);

  if assigned(Module) then
  begin
    Module.ModuleColor := ReadByte(aMsg);
  end
  else
    add_log_line('ModuleIndex ' + IntToStr(ModuleIndex) + ' not found.',
      LOGCMD_ERR);
end;

procedure TG2Patch.ProcessMsgModuleDelete(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex: byte;
  Module: IG2Module;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  Module := GetModule(LocationIndex, ModuleIndex);

  if assigned(Module) then
  begin
    NotifyObservers(EvtModuleDelete, Module);

    DeleteModuleFromPatch(LocationIndex, Module);
  end
  else
    add_log_line('ModuleIndex ' + IntToStr(ModuleIndex) + ' not found.',
      LOGCMD_ERR);
end;

procedure TG2Patch.ProcessMsgModuleLabel(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex: byte;
  Module: IG2Module;
  ModuleName: string;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  ModuleName := ReadClaviaString(aMsg);

  Module := GetModule(LocationIndex, ModuleIndex);

  if assigned(Module) then
  begin
    Module.ModuleName := ModuleName;

    NotifyObservers(EvtModuleLabel, Module);
  end
  else
    add_log_line('ModuleIndex ' + IntToStr(ModuleIndex) + ' not found.',
      LOGCMD_ERR);
end;

procedure TG2Patch.ProcessMsgModuleMove(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex: byte;
  Module: IG2Module;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  Module := GetModule(LocationIndex, ModuleIndex);

  if assigned(Module) then
  begin
    Module.Col := ReadByte(aMsg);
    Module.Row := ReadByte(aMsg);
    Module.InvalidateControl;
    Module.InvalidateCables;
  end
  else
    add_log_line('ModuleIndex ' + IntToStr(ModuleIndex) + ' not found.',
      LOGCMD_ERR);
end;

procedure TG2Patch.ProcessMsgModuleUprate(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex: byte;
  Module: IG2Module;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  Module := GetModule(LocationIndex, ModuleIndex);

  if assigned(Module) then
    Module.Uprate := ReadByte(aMsg)
  else
    add_log_line('ModuleIndex ' + IntToStr(ModuleIndex) + ' not found.',
      LOGCMD_ERR);
end;

procedure TG2Patch.ProcessMsgMorphSet(aMsg: TG2Message);
var
  LocationIndex, ModuleIndex, ParamIndex, MorphIndex, Value, Negative,
    Variation: byte;
begin
  LocationIndex := ReadByte(aMsg);
  ModuleIndex := ReadByte(aMsg);
  ParamIndex := ReadByte(aMsg);
  MorphIndex := ReadByte(aMsg);
  Value := ReadByte(aMsg);
  Negative := ReadByte(aMsg);
  Variation := ReadByte(aMsg);
  if Negative = 1 then
    SetMorphValue(LocationIndex, ModuleIndex, ParamIndex, MorphIndex,
      256 - Value, Variation)
  else
    SetMorphValue(LocationIndex, ModuleIndex, ParamIndex, MorphIndex, Value,
      Variation);
end;

procedure TG2Patch.ProcessMsgParamPageSelect(aMsg: TG2Message);
begin
  FSelectedParamPage := ReadByte(aMsg);

  NotifyObservers(EvtParamPageSelect, self as IG2Patch);
end;

procedure TG2Patch.ProcessMsgPatchDescrChunk(aMsg: TG2Message);
var
  Chunk: TPatchChunk;
  b: byte;
begin
  aMsg.Position := aMsg.Position - 1;

  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := Log;

    Chunk.ReadChunk;
    if Chunk.FId = $21 then
    begin
      aMsg.Read(b, 1);
      if b = $2D then
        // Read the extra 2 bytes $2d $00 that comes with patches downloaded with usb (TODO)
        aMsg.Read(b, 1)
      else
        aMsg.Position := aMsg.Position - 1;

      Init;
      Read(Chunk); // Parse patch
    end;
    { FPatch.InitParameters;
      FPatch.InitNames;
      FPatch.SortLeds; }

  finally
    Chunk.Free;
  end;
end;

procedure TG2Patch.SetPatchName(const aValue: string);
begin
  if assigned(Slot) then
    Slot.PatchName := aValue;
end;

procedure TG2Patch.SelectParamInPatch(const aLocationIndex, aModuleIndex,
  aParamIndex: byte);
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Module := nil;
  SelectedLocation := aLocationIndex;

  Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);
  if assigned(Module) then
  begin

    FPatchPartArray[aLocationIndex].FocusedModuleIndex := Module.ModuleIndex;
    Param := Module.Param[aParamIndex];

    if assigned(Param) then
    begin
      Module.SelectedParam := Param;
      DoSelectParam(Param);
    end;
  end;
end;

procedure TG2Patch.SelectParam(const aLocationIndex, aModuleIndex,
  aParamIndex: byte);
begin
  Slot.Connection.ParamSelect(SlotIndex, aLocationIndex, aModuleIndex,
    aParamIndex);
  SelectParamInPatch(aLocationIndex, aModuleIndex, aParamIndex);
end;

procedure TG2Patch.AddMessPatchSettings(aPatchSettings: IG2PatchSettings;
  aSendMessage, aUndoMessage: TG2SendMessage);
var
  SubMess: TG2Message;
begin
  SubMess := TG2Message.Create;
  try
    // Create undo
    FPatchSettings.WriteChunk(SubMess);
    aUndoMessage.Add(SubMess);

    aPatchSettings.WriteChunk(aSendMessage);
  finally
    SubMess.Free;
  end;
end;

procedure TG2Patch.AddMessAssignKnobs(aModule: IG2Module;
  const aPageIndex: byte; aSendMessage, aUndoMessage: TG2SendMessage);
var
  NewKnobIndex: integer;
  Param: IG2Param;
  ParamList: TList<IG2Param>;
begin
  ParamList := aModule.CreateAssignableParamList;
  try
    for Param in ParamList do
    begin
      NewKnobIndex := Param.DefaultKnobIndex;
      if NewKnobIndex >= 0 then
      begin
        NewKnobIndex := NewKnobIndex + aPageIndex * 8;
        Param.AddMessAssignKnob(FKnobList[NewKnobIndex], aSendMessage,
          aUndoMessage);
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

procedure TG2Patch.AddMessPatchNotes(aLines: TStringList;
  aSendMessage, aUndoMessage: TG2SendMessage);
var
  SubMess: TG2Message;
  PatchNotes: TPatchNotes;
begin
  SubMess := TG2Message.Create;
  PatchNotes := TPatchNotes.Create;
  try
    FPatchNotes.WriteChunk(SubMess);
    aUndoMessage.Add(SubMess);

    PatchNotes.SetLines(aLines);
    PatchNotes.WriteChunk(aSendMessage);
  finally
    PatchNotes.Free;
    SubMess.Free;
  end;
end;

procedure TG2Patch.SetEditAllVariations(const aValue: boolean);
begin
  FEditAllVariations := aValue;
end;

procedure TG2Patch.InitNames;
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    FPatchPartArray[i].InitNames;
  end;
end;

function TG2Patch.GetMorphKnobParameter(const aIndex: integer): IG2Param;
var
  Module: IG2Module;
begin
  Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_MORPH);

  result := nil;
  case aIndex of
    0:
      result := Module.Param[0];
    1:
      result := Module.Param[1];
    2:
      result := Module.Param[2];
    3:
      result := Module.Param[3];
    4:
      result := Module.Param[4];
    5:
      result := Module.Param[5];
    6:
      result := Module.Param[6];
    7:
      result := Module.Param[7];
    8:
      result := Module.Param[8];
    9:
      result := Module.Param[9];
    10:
      result := Module.Param[10];
    11:
      result := Module.Param[11];
    12:
      result := Module.Param[12];
    13:
      result := Module.Param[13];
    14:
      result := Module.Param[14];
    15:
      result := Module.Param[15];
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;

function TG2Patch.GetPatchSettings: IG2PatchSettings;
begin
  result := FPatchSettings;
end;

function TG2Patch.GetPatchKnobParameter(const aIndex: integer): IG2Param;
var
  Module: IG2Module;
begin
  result := nil;
  case aIndex of
    0:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_MASTERCLOCK);
        result := Module.Param[0];
      end;
    1:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VOICES);
        result := Module.Param[0];
      end;
    2:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_ARPEGGIATOR);
        result := Module.Param[ARP_SPEED];
      end;
    3:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_ARPEGGIATOR);
        result := Module.Param[ARP_DIRECTION];
      end;
    4:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VIBRATO);
        result := Module.Param[VIBRATO_DEPTH];
      end;
    5:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_GLIDE);
        result := Module.Param[GLIDE_SPEED];
      end;
    6:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_BEND);
        result := Module.Param[BEND_RANGE];
      end;
    7:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VOLUME);
        result := Module.Param[VOLUME_LEVEL];
      end;
    8:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_MASTERCLOCK);
        result := Module.Param[1];
      end;
    9:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VOICES);
        result := Module.Param[1];
      end;
    10:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_ARPEGGIATOR);
        result := Module.Param[ARP_ON_OFF];
      end;
    11:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_ARPEGGIATOR);
        result := Module.Param[ARP_OCTAVES];
      end;
    12:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VIBRATO);
        result := Module.Param[VIBRATO_MOD];
      end;
    13:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_GLIDE);
        result := Module.Param[GLIDE_TYPE];
      end;
    14:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_BEND);
        result := Module.Param[BEND_ON_OFF];
      end;
    15:
      begin
        Module := FPatchPartArray[LOCATION_PATCH].FindModule(PATCH_VOLUME);
        result := Module.Param[VOLUME_MUTE];
      end
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;

procedure TG2Patch.InvalidateParameters;
var
  i: integer;
begin
  for i := 0 to 2 do
  begin
    FPatchPartArray[i].InvalidateParameters;
  end;
end;

procedure TG2Patch.InvalidateConnectors;
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    FPatchPartArray[i].InvalidateConnectors;
  end;
end;

procedure TG2Patch.InvalidateCables;
var
  i: integer;
begin
  for i := 0 to 1 do
  begin
    FPatchPartArray[i].InvalidateCables;
  end;
end;

procedure TG2Patch.Read(aChunk: TPatchChunk);
var
  einde: boolean;
begin
  einde := False;
  repeat
    if not ReadChunk(aChunk) then
    begin
      raise Exception.Create('Error parsing patch data.');
    end
    else if aChunk.FId = C_PATCH_NOTES then
      einde := True; // assuming this is always the last chunk in a patch

    if not einde then
    begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := True;
    end;
  until einde;

  // PatchPart[LOCATION_PATCH].InitParams;
  InitNames;
  // SortLeds;
end;

function TG2Patch.ReadChunk(aChunk: TPatchChunk): boolean;
var
  Location: byte;
begin
  result := True;
  case aChunk.FId of
    C_MODULE_LIST: // $4a
      begin // Module list
        Location := aChunk.ReadBits(2);
        FPatchPartArray[Location].ReadChunk(aChunk);
      end;
    C_CABLE_LIST: // $52
      begin // Cable list
        Location := aChunk.ReadBits(2);
        FPatchPartArray[Location].ReadChunk(aChunk)
      end;
    C_MODULE_NAMES: // $5a
      begin // Module names
        Location := aChunk.ReadBits(2);
        aChunk.ReadBits(6); // Spacer
        FPatchPartArray[Location].ReadChunk(aChunk);
      end;
    C_PARAM_LIST: // $4d
      begin // Parameter list
        Location := aChunk.ReadBits(2);
        case Location of
          0 .. 1:
            FPatchPartArray[Location].ReadChunk(aChunk);
          2:
            FPatchParameters.Read(aChunk);
        end;
      end;
    C_PARAM_NAMES: // $5b
      begin // Parmeter names
        Location := aChunk.ReadBits(2);
        // ModuleCount := aChunk.ReadBits( 8);
        FPatchPartArray[Location].ReadChunk(aChunk);
      end;
    C_PATCH_DESCR: // $21
      begin // Patch description
        FPatchSettings.Read(aChunk);
      end;
    C_CURRENT_NOTE_2: // $69
      begin // Current note
        FCurrentNoteList.Read(aChunk);
      end;
    C_MORPH_PARAM: // $65
      begin // Morph parameter list
        FMorphParameters.Read(aChunk);
      end;
    C_KNOBS: // $62
      begin // Knob assignments
        ReadKnobList(FKnobList, aChunk);
      end;
    C_CONTROLLERS: // $60
      begin // Controller assignments
        ReadControllerList(FControllerList, aChunk);
      end;
    C_PATCH_NOTES: // $6f
      begin // Patch notes
        SetLength(FPatchNotes.FText, aChunk.FSize);
        FPatchNotes.Read(aChunk);
      end;
    S_SEL_PARAM_PAGE: // $2d
      begin // Sel parameter page
        aChunk.FStream.Position := aChunk.FStream.Position - 1;
        // This seems to be there only in patches that come from usb
      end;
  else
    begin
      result := False;
    end;
  end;
end;

procedure TG2Patch.ReadControllerList(aControllerList: TList<IG2Controller>;
  aChunk: TPatchChunk);
var
  i, ControllerCount: integer;
  Controller: IG2Controller;
  Param: IG2Param;
  MidiCC: TBits7;
  LocationIndex: TBits2;
  ModuleIndex: TBits8;
  ParamIndex: TBits7;
begin
  FControllerList.Clear;

  ControllerCount := aChunk.ReadBits(7);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Controller list, count = ' +
      IntToStr(ControllerCount), LOGCMD_NUL);
    aChunk.Log.add_log_line('CC# Loc Mod Par', LOGCMD_NUL);
  end;

  for i := 0 to ControllerCount - 1 do
  begin
    MidiCC := aChunk.ReadBits(7);
    LocationIndex := aChunk.ReadBits(2);
    ModuleIndex := aChunk.ReadBits(8);
    ParamIndex := aChunk.ReadBits(7);

    Param := FindParam(LocationIndex, ModuleIndex, ParamIndex);
    if assigned(Param) then
    begin
      Controller := TController.Create(MidiCC, Param);
      // Controller.Read( aChunk);

      if assigned(aChunk.Log) then
        aChunk.Log.add_log_line(Format('%3d %3d %3d %3d', [Controller.MidiCC,
          Controller.LocationIndex, Controller.ModuleIndex,
          Controller.ParamIndex]), LOGCMD_NUL);
      FControllerList.Add(Controller);
    end;
  end;
end;

procedure TG2Patch.ReadKnobList(aKnobList: TList<IG2Knob>; aChunk: TPatchChunk);
var
  i, KnobCount: integer;
  Param: IG2Param;
  IsAssigned: TBits1;
  LocationIndex: TBits2;
  ModuleIndex: TBits8;
  ParamIndex: TBits7;
begin
  KnobCount := aChunk.ReadBits(16);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('KnobList, count = ' + IntToStr(KnobCount),
      LOGCMD_NUL);
    aChunk.Log.add_log_line('Knb Loc Mod Isl Par', LOGCMD_NUL);
  end;

  for i := 0 to KnobCount - 1 do
  begin

    IsAssigned := aChunk.ReadBits(1);
    if IsAssigned = 1 then
    begin
      LocationIndex := aChunk.ReadBits(2);
      ModuleIndex := aChunk.ReadBits(8);
      aChunk.ReadBits(2); // IsLed
      ParamIndex := aChunk.ReadBits(7);

      Param := FindParam(LocationIndex, ModuleIndex, ParamIndex);
      FKnobList[i].Param := Param;
    end
    else
      FKnobList[i].Param := nil;

    if assigned(aChunk.Log) and (FKnobList[i].IsAssigned = 1) then
      aChunk.Log.add_log_line(Format('%3d %3d %3d %3d %3d',
        [i, ord(FKnobList[i].LocationIndex), FKnobList[i].ModuleIndex,
        FKnobList[i].IsLed, FKnobList[i].ParamIndex]), LOGCMD_NUL);
  end;

end;

function TG2Patch.ReadLedData(aStream: TStream): boolean;
var
  i, j, l: integer;
  Unknown, b, mask: byte;
  Led: IG2Led;
begin
  aStream.Read(Unknown, 1);
  j := 3;
  mask := 0;
  for l := 0 to 1 do
  begin

    i := 0;
    while (i < PatchPart[l].LedList.Count) do
    begin

      if j = 3 then
      begin
        aStream.Read(b, 1);
        mask := $03;
        j := 0;
      end
      else
      begin
        mask := mask shl 2;
        inc(j);
      end;

      Led := PatchPart[l].LedList[i];
      Led.SetValue((b and mask) shr j);

      inc(i);
    end;
  end;
  result := True;
end;

function TG2Patch.ReadVolumeData(aStream: TStream): boolean;
var
  i, l: integer;
  Unknown, b: byte;
  Led: IG2Led;
begin
  for l := 0 to 1 do
  begin
    i := 0;
    while (i < PatchPart[l].LedStripList.Count) do
    begin
      aStream.Read(Unknown, 1); // Unknown
      aStream.Read(b, 1);
      Led := PatchPart[l].LedStripList[i];
      Led.SetValue(b);

      inc(i);
    end;
  end;
  result := True;
end;

procedure TG2Patch.Write(aChunk: TPatchChunk; aVariationCount: byte);
begin
  WriteChunk(C_PATCH_DESCR, aChunk, aVariationCount);

  aChunk.WriteBits(LOCATION_VA, 2);
  FPatchPartArray[LOCATION_VA].WriteChunk(C_MODULE_LIST, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_FX, 2);
  FPatchPartArray[LOCATION_FX].WriteChunk(C_MODULE_LIST, aChunk,
    aVariationCount);

  WriteChunk(C_CURRENT_NOTE_2, aChunk, aVariationCount);

  aChunk.WriteBits(LOCATION_VA, 2);
  FPatchPartArray[LOCATION_VA].WriteChunk(C_CABLE_LIST, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_FX, 2);
  FPatchPartArray[LOCATION_FX].WriteChunk(C_CABLE_LIST, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_PATCH, 2);
  WriteChunk(C_PARAM_LIST, aChunk, aVariationCount);

  aChunk.WriteBits(LOCATION_VA, 2);
  FPatchPartArray[LOCATION_VA].WriteChunk(C_PARAM_LIST, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_FX, 2);
  FPatchPartArray[LOCATION_FX].WriteChunk(C_PARAM_LIST, aChunk,
    aVariationCount);

  WriteChunk(C_MORPH_PARAM, aChunk, aVariationCount);

  WriteChunk(C_KNOBS, aChunk, aVariationCount);

  WriteChunk(C_CONTROLLERS, aChunk, aVariationCount);

  aChunk.WriteBits(LOCATION_PATCH, 2);
  FPatchPartArray[LOCATION_PATCH].WriteChunk(C_PARAM_NAMES, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_VA, 2);
  FPatchPartArray[LOCATION_VA].WriteChunk(C_PARAM_NAMES, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_FX, 2);
  FPatchPartArray[LOCATION_FX].WriteChunk(C_PARAM_NAMES, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_VA, 2);
  aChunk.WriteBits(0, 6);
  FPatchPartArray[LOCATION_VA].WriteChunk(C_MODULE_NAMES, aChunk,
    aVariationCount);

  aChunk.WriteBits(LOCATION_FX, 2);
  aChunk.WriteBits(0, 6);
  FPatchPartArray[LOCATION_FX].WriteChunk(C_MODULE_NAMES, aChunk,
    aVariationCount);

  WriteChunk(C_PATCH_NOTES, aChunk, aVariationCount);
end;

procedure TG2Patch.WriteChunk(aID: byte; aChunk: TPatchChunk;
  aVariationCount: byte);
begin
  case aID of
    C_PATCH_DESCR:
      begin
        FPatchSettings.Write(aChunk);
        aChunk.WriteChunk(C_PATCH_DESCR);
      end;
    C_CURRENT_NOTE_2:
      begin
        FCurrentNoteList.Write(aChunk);
        aChunk.WriteChunk(C_CURRENT_NOTE_2);
      end;
    C_PARAM_LIST:
      begin
        FPatchParameters.Write(aChunk, aVariationCount);
        aChunk.WriteChunk(C_PARAM_LIST);
      end;
    C_MORPH_PARAM:
      begin
        FMorphParameters.Write(aChunk, aVariationCount);
        aChunk.WriteChunk(C_MORPH_PARAM);
      end;
    C_KNOBS:
      begin
        WriteKnobList(FKnobList, aChunk);
        aChunk.WriteChunk(C_KNOBS);
      end;
    C_CONTROLLERS:
      begin
        WriteControllerList(FControllerList, aChunk);
        aChunk.WriteChunk(C_CONTROLLERS);
      end;
    C_PATCH_NOTES:
      begin
        FPatchNotes.Write(aChunk);
        aChunk.WriteChunk(C_PATCH_NOTES);
      end;
  end;
end;

procedure TG2Patch.WriteControllerList(aControllerList: TList<IG2Controller>;
  aChunk: TPatchChunk);
var
  Controller: IG2Controller;
begin
  aChunk.WriteBits(FControllerList.Count, 7);
  for Controller in FControllerList do
    Controller.Write(aChunk);
end;

procedure TG2Patch.WriteKnobList(aKnobList: TList<IG2Knob>;
  aChunk: TPatchChunk);
var
  Knob: IG2Knob;
begin
  aChunk.WriteBits(FKnobList.Count, 16);
  for Knob in FKnobList do
    Knob.Write(aChunk);
end;

function TG2Patch.LoadFromFile(aStream: TStream): boolean;
var
  sl: TStringList;
  Chunk: TPatchChunk;
  s: string;
  b: byte;
  bm, bl: byte;
  Crc: Word;
begin
  result := False;

  Chunk := TPatchChunk.Create(aStream);
  sl := TStringList.Create;
  try
    Chunk.Log := Log;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do
    begin
      aStream.Read(b, SizeOf(b));
      case b of
        $0D:
          ; // just skip it
        $0A:
          begin
            sl.Add(s);
            s := '';
          end;
        $00:
          Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    Init;
    Chunk.ReadBuffer(2);
    PatchVersion := Chunk.ReadBits(8);
    PatchType := Chunk.ReadBits(8);
    Chunk.ReadChunk;
    Read(Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

procedure TG2Patch.SaveToFile(aStream: TStream);
var
  Chunk: TPatchChunk;
begin
  Chunk := TPatchChunk.Create(aStream);
  try
    AddHeaderInfo(pftPatch, aStream);

    Chunk.WriteBits(PatchVersion, 8);
    Chunk.WriteBits(PatchType, 8);
    Chunk.Flush;

    Write(Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
  end;
end;

{$IFDEF MSWINDOWS}

procedure TG2Patch.SaveAsFXP(aStream: TStream);
var
  FXPHeader: TFXPHeader;
  MemStream: TMemoryStream;
  i: integer;
  PatchName: string;

  function SwapBytes(Value: Cardinal): Cardinal; register;
  asm
    BSWAP  EAX
  end;
  { begin
    // TDOD
    end; }

  begin
    MemStream := TMemoryStream.Create;
    try
      SaveToFile(MemStream);

      StringToByteArray('CcnK', FXPHeader.chunkMagic, 4);
      FXPHeader.byteSize := SwapBytes(52 + MemStream.Size);
      StringToByteArray('FPCh', FXPHeader.fxMagic, 4);
      FXPHeader.version := SwapBytes(1);
      StringToByteArray('NMG2', FXPHeader.fxID, 4);
      FXPHeader.fxVersion := SwapBytes(1);
      FXPHeader.numPrograms := SwapBytes(1);
      Fillchar(FXPHeader.Name, 28, #0);
      PatchName := GetPatchName;
      for i := 0 to PatchName.Length - 1 do
        FXPHeader.Name[i] := byte(PatchName.Chars[i]);

      FXPHeader.chunkSize := SwapBytes(MemStream.Size);

      aStream.Write(FXPHeader, SizeOf(FXPHeader));
      aStream.Write(MemStream.Memory^, MemStream.Size);

    finally
      MemStream.Free;
    end;
  end;
{$ENDIF}

procedure TG2Patch.WriteMessSelectParamPage(const aPageIndex: byte;
  aStream: TStream);
begin
  WriteByte(aStream, S_SEL_PARAM_PAGE);
  WriteByte(aStream, aPageIndex);
end;

procedure TG2Patch.add_log_line(tekst: string; log_cmd: integer);
begin
  if assigned(Log) then
    Log.add_log_line(tekst, log_cmd);
end;

procedure TG2Patch.DeleteModuleFromPatch(const aLocationIndex: integer;
  aModule: IG2Module);
var
  aModuleIndex: byte;
  i: integer;
begin
  // aModule.SelectModule( False, True);

  if assigned(Perf) then
    Perf.DeleteModuleFromPerf(SlotIndex, aLocationIndex, aModule);

  aModuleIndex := aModule.ModuleIndex;

  // Delete module from controllerlist
  i := 0;
  while (i < FControllerList.Count) do
  begin
    if (FControllerList[i].ModuleIndex = aModuleIndex) then
      FControllerList.Delete(i)
    else
      inc(i);
  end;

  // FKnobList.DeleteModule( aModuleIndex, ord(aLocation));

  // Delete module from knoblist
  for i := 0 to FKnobList.Count - 1 do
    if (FKnobList[i].ModuleIndex = aModuleIndex) and
      (FKnobList[i].LocationIndex = aLocationIndex) then
      FKnobList[i].IsAssigned := 0;

  // Delete module from patchsettings
  for i := 0 to Length(FPatchParameters.FVariationListArray) - 1 do
    FPatchParameters.PatchParamList[i].DeleteModule(aLocationIndex,
      aModuleIndex);

  FPatchPartArray[aLocationIndex].DeleteModule(aModuleIndex);
end;

procedure TG2Patch.DeleteCableFromPatch(const aLocationIndex: integer;
  aCable: IG2Cable);
begin
  FPatchPartArray[aLocationIndex].DeleteCable(aCable);
end;

function TG2Patch.FindController(const aMidiCC: byte): IG2Controller;
var
  i: integer;
begin
  result := nil;
  i := 0;
  while (i < FControllerList.Count) and
    not(FControllerList.Items[i].MidiCC = aMidiCC) do
    inc(i);

  if (i < FControllerList.Count) then
    result := FControllerList.Items[i];
end;

function TG2Patch.FindController(const aLocationIndex, aModuleIndex,
  aParamIndex: integer): IG2Controller;
var
  i: integer;
begin
  result := nil;
  i := 0;
  while (i < FControllerList.Count) and
    not((FControllerList.Items[i].LocationIndex = aLocationIndex) and
    (FControllerList.Items[i].ModuleIndex = aModuleIndex) and
    (FControllerList.Items[i].ParamIndex = aParamIndex)) do
    inc(i);

  if (i < FControllerList.Count) then
    result := FControllerList.Items[i];
end;

function TG2Patch.FindKnob(const aLocationIndex, aModuleIndex,
  aParamIndex: byte): IG2Knob;
var
  i: integer;
begin
  i := 0;
  while (i < FKnobList.Count) and not((FKnobList[i].IsAssigned = 1) and
    (FKnobList[i].LocationIndex = aLocationIndex) and
    (FKnobList[i].ModuleIndex = aModuleIndex) and
    (FKnobList[i].ParamIndex = aParamIndex)) do
    inc(i);

  if (i < FKnobList.Count) then
  begin
    result := FKnobList.Items[i]
  end
  else
    result := nil;
end;

function TG2Patch.FindParam(const aLocationIndex, aModuleIndex,
  aParamIndex: byte): IG2Param;
begin
  result := FPatchPartArray[aLocationIndex].FindParam(aModuleIndex,
    aParamIndex);
end;

function TG2Patch.GetMaxModuleIndex(const aLocationIndex: integer): integer;
begin
  result := FPatchPartArray[aLocationIndex].GetMaxModuleIndex;
end;

function TG2Patch.GetNoOffModuleType(const aLocationIndex,
  aModuleType: byte): integer;
begin
  result := FPatchPartArray[aLocationIndex].GetNoOffModuleType(aModuleType);
end;

function TG2Patch.GetNoOffExtendedModules: integer;
begin
  result := FPatchPartArray[LOCATION_VA].GetNoOffExtendedModules +
    FPatchPartArray[LOCATION_FX].GetNoOffExtendedModules;
end;

function TG2Patch.GetParameterValue(const aLocationIndex, aModuleIndex,
  aParamIndex, aVariation: byte): byte;
var
  Module: IG2Module;
begin
  result := 0;

  if (aLocationIndex = LOCATION_FX) or (aLocationIndex = LOCATION_VA) then
  begin
    Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);
    if assigned(Module) then
      result := Module.GetParamValue(aVariation, aParamIndex);
  end
  else if aLocationIndex = LOCATION_PATCH then
  begin

    case aModuleIndex of
      PATCH_MORPH:
        begin
          case aParamIndex of
            0 .. 7:
              result := FPatchParameters.FMorphArray[aParamIndex].FDialArray
                [aVariation];
            8 .. 15:
              result := FPatchParameters.FMorphArray[aParamIndex - 8].FModeArray
                [aVariation];
          end;
        end;
      PATCH_VOLUME:
        begin
          case aParamIndex of
            VOLUME_LEVEL:
              result := FPatchParameters.FVariationListArray[aVariation]
                .FPatchVol;
            VOLUME_MUTE:
              result := FPatchParameters.FVariationListArray[aVariation]
                .FActiveMuted;
          end;
        end;
      PATCH_GLIDE:
        begin
          case aParamIndex of
            GLIDE_TYPE:
              result := FPatchParameters.FVariationListArray[aVariation].FGlide;
            GLIDE_SPEED:
              result := FPatchParameters.FVariationListArray[aVariation]
                .FGlideTime;
          end;
        end;
      PATCH_BEND:
        begin
          case aParamIndex of
            BEND_ON_OFF:
              result := FPatchParameters.FVariationListArray[aVariation].FBend;
            BEND_RANGE:
              result := FPatchParameters.FVariationListArray[aVariation].FSemi;
          end;
        end;
      PATCH_VIBRATO:
        begin
          case aParamIndex of
            VIBRATO_MOD:
              result := FPatchParameters.FVariationListArray
                [aVariation].FVibrato;
            VIBRATO_DEPTH:
              result := FPatchParameters.FVariationListArray[aVariation].FCents;
            VIBRATO_RATE:
              result := FPatchParameters.FVariationListArray[aVariation].FRate;
          end;
        end;
      PATCH_ARPEGGIATOR:
        begin
          case aParamIndex of
            ARP_ON_OFF:
              result := FPatchParameters.FVariationListArray[aVariation]
                .FArpeggiator;
            ARP_SPEED:
              result := FPatchParameters.FVariationListArray
                [aVariation].FArpTime;
            ARP_DIRECTION:
              result := FPatchParameters.FVariationListArray
                [aVariation].FArpType;
            ARP_OCTAVES:
              result := FPatchParameters.FVariationListArray
                [aVariation].FOctaves;
          end;
        end;
      PATCH_SUSTAIN:
        begin
          case aParamIndex of
            SUSTAIN_PEDAL:
              result := FPatchParameters.FVariationListArray
                [aVariation].FSustain;
            OCTAVE_SHIFT:
              result := FPatchParameters.FVariationListArray[aVariation]
                .FOctaveShift;
          end;
        end;
    end;
  end;
end;

procedure TG2Patch.SetParamInPatch(const aLocationIndex, aModuleIndex,
  aParamIndex, aVariation, aValue: byte);
var
  Param: IG2Param;
  Module: IG2Module;
begin
  Slot.Connection.ParamSetValue(SlotIndex, aLocationIndex, aModuleIndex,
    aParamIndex, aVariation, aValue);

  if (aLocationIndex = LOCATION_FX) or (aLocationIndex = LOCATION_VA) then
  begin
    Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);
    if assigned(Module) then
      Module.SetParamValue(aVariation, aParamIndex, aValue);
  end
  else if aLocationIndex = LOCATION_PATCH then
  begin

    case aModuleIndex of
      PATCH_MORPH:
        begin
          case aParamIndex of
            0 .. 7:
              FPatchParameters.FMorphArray[aParamIndex].FDialArray[aVariation]
                := aValue;
            8 .. 15:
              FPatchParameters.FMorphArray[aParamIndex - 8].FModeArray
                [aVariation] := aValue;
          end;
        end;
      PATCH_VOLUME:
        begin
          case aParamIndex of
            VOLUME_LEVEL:
              FPatchParameters.FVariationListArray[aVariation].FPatchVol
                := aValue;
            VOLUME_MUTE:
              FPatchParameters.FVariationListArray[aVariation].FActiveMuted
                := aValue;
          end;
        end;
      PATCH_GLIDE:
        begin
          case aParamIndex of
            GLIDE_TYPE:
              FPatchParameters.FVariationListArray[aVariation].FGlide := aValue;
            GLIDE_SPEED:
              FPatchParameters.FVariationListArray[aVariation].FGlideTime
                := aValue;
          end;
        end;
      PATCH_BEND:
        begin
          case aParamIndex of
            BEND_ON_OFF:
              FPatchParameters.FVariationListArray[aVariation].FBend := aValue;
            BEND_RANGE:
              FPatchParameters.FVariationListArray[aVariation].FSemi := aValue;
          end;
        end;
      PATCH_VIBRATO:
        begin
          case aParamIndex of
            VIBRATO_MOD:
              FPatchParameters.FVariationListArray[aVariation].FVibrato
                := aValue;
            VIBRATO_DEPTH:
              FPatchParameters.FVariationListArray[aVariation].FCents := aValue;
            VIBRATO_RATE:
              FPatchParameters.FVariationListArray[aVariation].FRate := aValue;
          end;
        end;
      PATCH_ARPEGGIATOR:
        begin
          case aParamIndex of
            ARP_ON_OFF:
              FPatchParameters.FVariationListArray[aVariation].FArpeggiator
                := aValue;
            ARP_SPEED:
              FPatchParameters.FVariationListArray[aVariation].FArpTime
                := aValue;
            ARP_DIRECTION:
              FPatchParameters.FVariationListArray[aVariation].FArpType
                := aValue;
            ARP_OCTAVES:
              FPatchParameters.FVariationListArray[aVariation].FOctaves
                := aValue;
          end;
        end;
      PATCH_SUSTAIN:
        begin
          case aParamIndex of
            SUSTAIN_PEDAL:
              FPatchParameters.FVariationListArray[aVariation].FSustain
                := aValue;
            OCTAVE_SHIFT:
              FPatchParameters.FVariationListArray[aVariation].FOctaveShift
                := aValue;
          end;
        end;
    end;
  end;

  Param := nil;
  Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);

  if assigned(Module) then
    Param := Module.Param[aParamIndex];

  DoLabelValueChange(Param);
end;

procedure TG2Patch.SetParamValue(const aLocationIndex, aModuleIndex,
  aParamIndex, aVariation, aValue: byte);
begin
  SetParamInPatch(aLocationIndex, aModuleIndex, aParamIndex,
    aVariation, aValue);
end;

function TG2Patch.GetModeValue(const aLocationIndex, aModuleIndex,
  aParamIndex: byte): byte;
var
  Module: IG2Module;
begin
  result := 0;

  Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.ModeCount then
      result := Module.ModeInfoArray[aParamIndex];
end;

procedure TG2Patch.SetModeInPatch(const aLocationIndex, aModuleIndex,
  aParamIndex, aValue: byte);
var
  Module: IG2Module;
begin
  Module := FPatchPartArray[aLocationIndex].FindModule(aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.ModeCount then
    begin
      Module.ModeInfoArray[aParamIndex] := aValue;
    end;
end;

procedure TG2Patch.SetModeValue(const aLocationIndex, aModuleIndex, aParamIndex,
  aValue: byte);
begin
  SetModeInPatch(aLocationIndex, aModuleIndex, aParamIndex, aValue);
end;

function TG2Patch.HasMorph(const aLocationIndex, aModuleIndex, aParamIndex,
  aVariation: byte): boolean;
var
  i: integer;
  PatchParamList: TPatchParamList;
begin
  PatchParamList := FPatchParameters.FVariationListArray[aVariation];
  i := 0;
  while (i < PatchParamList.Count) and
    not((PatchParamList.Items[i].LocationIndex = aLocationIndex) and
    (PatchParamList.Items[i].ModuleIndex = aModuleIndex) and
    (PatchParamList.Items[i].ParamIndex = aParamIndex)) do
    inc(i);

  result := (i < PatchParamList.Count);
end;

function TG2Patch.FindMorph(const aLocationIndex, aModuleIndex, aParamIndex,
  aMorphIndex, aVariation: byte): IG2ParamMorph;
begin
  result := FPatchParameters.FVariationListArray[aVariation].FindMorphParam
    (aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
end;

function TG2Patch.GetMorphValue(const aLocationIndex, aModuleIndex, aParamIndex,
  aMorphIndex, aVariation: byte): byte;
var
  MorphParameter: IG2ParamMorph;
begin
  result := 0;
  if aLocationIndex = LOCATION_PATCH then
  begin
    MorphParameter := FindMorph(aLocationIndex, aModuleIndex, aParamIndex,
      aMorphIndex, aVariation);
    if assigned(MorphParameter) then
      result := MorphParameter.Range;
  end
  else
    result := FPatchPartArray[aLocationIndex].GetMorphValue(aModuleIndex,
      aParamIndex, aMorphIndex, aVariation);
end;

procedure TG2Patch.SetMorphValue(const aLocationIndex, aModuleIndex,
  aParamIndex, aMorphIndex, aValue, aVariation: byte);
var
  MorphParameter: IG2ParamMorph;
begin
  if aValue >= 128 then
    Slot.Connection.ParamSetMorph(SlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMorphIndex, aValue, 1, 256 - aVariation)
  else
    Slot.Connection.ParamSetMorph(SlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMorphIndex, aValue, 0, aVariation);

  MorphParameter := FindMorph(aLocationIndex, aModuleIndex, aParamIndex,
    aMorphIndex, aVariation);
  if not assigned(MorphParameter) then
  begin
    if aValue <> 0 then
      FPatchParameters.FVariationListArray[aVariation].AddNewMorphParam
        (aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex, aValue);
  end
  else
  begin
    if aValue = 0 then
      FPatchParameters.FVariationListArray[aVariation].DelMorphParam
        (aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex)
    else
      MorphParameter.Range := aValue;
  end;

  DoMorphChange(MorphParameter);
end;

procedure TG2Patch.SetMasterClock(const aValue: byte);
begin
  Perf.SetMasterClock(aValue);
end;

function TG2Patch.GetMasterClock: byte;
begin
  result := Perf.MasterClock
end;

procedure TG2Patch.SetMasterClockRun(const aValue: byte);
begin
  Perf.SetMasterClockRun(aValue);
end;

function TG2Patch.GetMasterClockRun: byte;
begin
  result := Perf.MasterClockRun
end;

function TG2Patch.GetVariationCount: integer;
begin
  result := FPatchParameters.VariationCount;
end;

procedure TG2Patch.CopyVariation(const aFromVariation, aToVariation: byte);
var
  i: integer;
begin
  for i := 0 to 2 do
    FPatchPartArray[i].CopyVariation(aFromVariation, aToVariation);
  FPatchParameters.FVariationListArray[aToVariation]
    .Copy(FPatchParameters.FVariationListArray[aFromVariation]);
end;

function TG2Patch.GetPatchPart(const aIndex: integer): IG2PatchPart;
begin
  if (aIndex < Length(FPatchPartArray)) then
    result := FPatchPartArray[aIndex]
  else
    result := nil;
end;

function TG2Patch.GetPerf: IG2Perf;
begin
  if assigned(Slot) then
    result := Slot.Perf
  else
    result := nil;
end;

function TG2Patch.GetSelectedLocation: integer;
begin
  result := FSelectedLocation;
end;

function TG2Patch.GetSelectedMorphIndex: integer;
begin
  result := FSelectedMorphIndex;
end;

function TG2Patch.GetSelectedParamPage: integer;
begin
  result := FSelectedParamPage;
end;

function TG2Patch.GetSelectedParamPageColumn: integer;
begin
  result := FSelectedParamPageColumn;
end;

function TG2Patch.GetSlot: IG2Slot;
begin
  result := FWSLot;
end;

function TG2Patch.GetSlotIndex: integer;
begin
  if assigned(Slot) then
    result := Slot.SlotIndex
  else
    result := 0;
end;

function TG2Patch.GetPatchName: string;
begin
  result := Slot.PatchName
end;

function TG2Patch.GetModule(LocationIndex, ModuleIndex: integer): IG2Module;
begin
  result := FPatchPartArray[LocationIndex].FindModule(ModuleIndex);
end;

function TG2Patch.GetControllerList: TList<IG2Controller>;
begin
  result := FControllerList;
end;

function TG2Patch.GetEditAllVariations: boolean;
begin
  result := FEditAllVariations;
end;

function TG2Patch.GetKnobList: TList<IG2Knob>;
begin
  result := FKnobList;
end;

function TG2Patch.GetLog: IG2Log;
begin
  if assigned(Slot) then
    result := Slot.Log
  else
    result := nil;
end;

procedure TG2Patch.UnselectModules(const aLocationIndex: integer);
begin
  FPatchPartArray[aLocationIndex].UnselectModules;
end;

procedure TG2Patch.SetSelectedLocation(const aLocationIndex: integer);

begin
  if (FSelectedLocation <> aLocationIndex)
  { and (aLocationIndex < LOCATION_PATCH) } then
  begin
    FSelectedLocation := aLocationIndex;

    DoSelectLocation;
  end;
end;

procedure TG2Patch.SetSelectedMorphIndex(const aValue: integer);
begin
  FSelectedMorphIndex := aValue;
end;

procedure TG2Patch.SetSelectedParamPage(const aValue: integer);
begin
  FSelectedParamPage := aValue;
end;

procedure TG2Patch.SetSelectedParamPageColumn(const aValue: integer);
begin
  FSelectedParamPageColumn := aValue;
end;

end.
