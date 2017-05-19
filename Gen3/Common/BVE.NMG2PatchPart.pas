unit BVE.NMG2PatchPart;

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
  BVE.NMG2FileIntf;

type
  TG2PatchPart = class;

  TG2PatchPart = class(TInterfacedObject, IG2PatchPart)
  private
    [Weak]
    FWPatch: IG2Patch;

    FLocationIndex: byte;

    FModuleList: TList<IG2Module>;
    FCableList: TList<IG2Cable>;

    FFocusedModuleIndex: integer;

    FLedList: TList<IG2Led>; // single leds
    FLedStripList: TList<IG2Led>; // mini-vu's and sequencer leds
    FLedComparison: IComparer<IG2Led>;

    FOnAddModule: TAddModuleEvent;
    FOnAddCable: TAddCableEvent;
    FOnLed: TLedEvent;

    function GetSelectedParam: IG2Param;

    function GetPatch: IG2Patch;
    function GetLocationIndex: byte;
    function GetFocusedModuleIndex: integer;

    // List of cables where both modules exist in aModulelist
    function GetInnerCableList(const aModuleList: TList<IG2Module>): TList<IG2Cable>;

    function GetUnitsRect: TRect;
    function GetSelectedUnitsRect: TRect;

    procedure ResetModuleUprates;
    procedure CreateModuleUprateList(const aConnector: IG2Connector;
      const aNewRate: byte; aModuleList: TList<IG2Module>);
    // procedure   SetLocationIndex( const aValue : byte);
    function GetModuleList: TList<IG2Module>;
    function GetCableList: TList<IG2Cable>;
    function GetLedList: TList<IG2Led>;
    function GetLedStripList: TList<IG2Led>;
    function GetVariationCount: integer;
    procedure SetFocusedModuleIndex(const aModuleIndex: integer);

    function GetOnAddModule: TAddModuleEvent;
    function GetOnAddCable: TAddCableEvent;
    function GetOnLedEvent: TLedEvent;

    procedure SetOnAddModule(const aValue: TAddModuleEvent);
    procedure SetOnAddCable(const aValue: TAddCableEvent);
    procedure SetOnLedEvent(const aValue: TLedEvent);
    function GetSlotIndex: byte;
    function GetSlot: IG2Slot;
    function GetPerf: IG2Perf;

  public
    constructor Create(aPatch: IG2Patch; const aLocationIndex: integer);
      reintroduce; virtual;
    destructor Destroy; override;

    function CreateCopy(aPatch: IG2Patch; const aLocationIndex: integer)
      : IG2PatchPart;
    function CreateCopySelected(aPatch: IG2Patch; const aLocationIndex: integer)
      : IG2PatchPart;

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure Init;
    procedure InitNames;
    procedure InitParams;

    procedure Read(aChunk: TPatchChunk);
    function ReadChunk(aChunk: TPatchChunk): boolean;
    procedure ReadModuleList(aModuleList: TList<IG2Module>;
      aChunk: TPatchChunk);
    procedure ReadCableList(aCableList: TList<IG2Cable>; aChunk: TPatchChunk);
    procedure ReadParamList(aModuleList: TList<IG2Module>; aChunk: TPatchChunk);
    procedure ReadParamLabelList(aModuleList: TList<IG2Module>;
      aChunk: TPatchChunk);
    procedure ReadModuleLabelList(aModuleList: TList<IG2Module>;
      aChunk: TPatchChunk);

    procedure Write(aChunk: TPatchChunk; aVariationCount: byte);
    procedure WriteChunk(aID: byte; aChunk: TPatchChunk; aVariationCount: byte);
    procedure WriteModuleList(aModuleList: TList<IG2Module>; aChunk: TPatchChunk);
    procedure WriteCableList(aCableList: TList<IG2Cable>; aChunk: TPatchChunk);
    procedure WriteParamList(aModuleList: TList<IG2Module>;
      aVariationCount: byte; aChunk: TPatchChunk);
    procedure WriteParamLabelList(aModuleList: TList<IG2Module>;
      aChunk: TPatchChunk);
    procedure WriteModuleLabelList(aModuleList: TList<IG2Module>;
      aChunk: TPatchChunk);

    procedure WriteChunkSelectedModules(aModuleList: TList<IG2Module>;
      const aID: byte; const aVariationCount: byte; aChunk: TPatchChunk);
    procedure WriteMessAddModule(const aModule: IG2Module; aStream: TStream);
    procedure WriteMessDeleteModule(const aModuleIndex: byte; aStream: TStream);

    procedure AddMessAddModule(const aModuleTypeID, aCol, aRow: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessAddCable(const aFromModuleIndex: byte;
      aFromConnectorKind: TConnectorKind; aFromConnectorIndex: byte;
      aFromConnectorColor: byte; aToModuleIndex: byte;
      aToConnectorKind: TConnectorKind; aToConnectorIndex: byte;
      aToConnectorColor: byte; aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessDeleteCable(aCable: IG2Cable;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessAddPatchPart(aSrcePatchPart: IG2PatchPart;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessPasteParams(aSrcePatchPart: IG2PatchPart;
      const aFromVariation, aToVariation: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessDeleteModules(aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessUprateChanges(aModuleUprateList: TList<IG2Module>;
      aSendMessage, aUndoMessage: TG2SendMessage);

    function ProcessResponseMsg(aSender: TObject; aMsg: TG2Message): boolean;
    procedure ProcessMsgModuleParamLabel(aMsg: TG2Message);

    function CreateModule(const aModuleIndex, aModuleType: byte): IG2Module;
    procedure AddModule(aModule: IG2Module);
    procedure DeleteModule(aModuleIndex: integer);

    function CreateSelectedModuleList: TList<IG2Module>;

    procedure AddCable(aCable: IG2Cable);
    procedure DeleteCable(aCable: IG2Cable);

    function CreateLed(const aLedType: TLedType;
      const aModuleIndex, aGroupID: byte; const aGroupCount: integer)
      : IG2Led; virtual;
    procedure CreateLeds(const aModuleIndex, aModuleType: byte);

    procedure RenumberModules(aFromIndex: byte);

    function GetNoOffExtendedModules: integer;
    function GetMaxModuleIndex: integer;
    function GetUniqueModuleNameSeqNr(aModuleFileName: string): integer;
    function GetNoOffModuleType(aModuleType: byte): integer;

    function ModuleAtLocation(const aRow, aCol: integer): IG2Module;

    function FindModule(const aModuleIndex: byte): IG2Module;
    function FindCable(const aFromModuleIndex, aFromConnIndex, aToModuleIndex,
      aToConnIndex: byte): IG2Cable;
    function FindParam(const aModuleIndex, aParamIndex: byte): IG2Param;

    function CheckParametersFit(aSrcePatch: IG2PatchPart): boolean;

    function GetModuleRow(const aModule: IG2Module): byte;
    function GetMorphValue(aModuleIndex: byte; aParamIndex: byte;
      aMorphIndex: byte; aVariation: byte): byte;

    procedure CopyVariation(aFromVariation, aToVariation: byte);

    procedure SelectAll;
    procedure UnselectModules;
    procedure SelectModule(const aModuleIndex: byte;
      const aValue, aExclusive: boolean);

    procedure SelectNextModuleParam;
    procedure SelectPrevModuleParam;

    procedure InvalidateParameters;
    procedure InvalidateConnectors;
    procedure InvalidateCables;

    property SlotIndex: byte read GetSlotIndex;
    property LocationIndex: byte read GetLocationIndex;
    property Patch: IG2Patch read GetPatch;
    property Slot: IG2Slot read GetSlot;
    property Perf: IG2Perf read GetPerf;
    property VariationCount: integer read GetVariationCount;
  end;

implementation

uses
  System.Math,
  BVE.NMG2Module,
  BVE.NMG2Cable,
  BVE.NMG2Led,
  BVE.NMG2Param;

// ------------------------------------------------------------------------------
//
// TG2ModuleList
//
// ------------------------------------------------------------------------------

procedure ModuleListSortOnIndex(aModuleList: TList<IG2Module>);
begin
  aModuleList.Sort(TComparer<IG2Module>.Construct(
    function(const Module1, Module2: IG2Module): integer
    begin
      if Module1.ModuleIndex > Module2.ModuleIndex then
        Result := -1
      else if Module1.ModuleIndex = Module2.ModuleIndex then
        Result := 0
      else
        Result := 1;
    end));
end;

procedure ModuleListSortOnPosition(aModuleList: TList<IG2Module>);
begin
  aModuleList.Sort(TComparer<IG2Module>.Construct(
    function(const Module1, Module2: IG2Module): integer
    begin
      if Module1.Row > Module2.Row then
        Result := -1
      else if Module1.Row = Module2.Row then
      begin
        if Module1.Col > Module2.Col then
          Result := -1
        else if Module1.Col = Module2.Col then
          Result := 0
        else
          Result := 1;
      end
      else
        Result := 1;
    end));
end;

procedure ModuleListSortOnTypePosition(aModuleList: TList<IG2Module>);
begin
  aModuleList.Sort(TComparer<IG2Module>.Construct(
    function(const Module1, Module2: IG2Module): integer
    begin
      if Module1.TypeID > Module2.TypeID then
        Result := -1
      else if Module1.TypeID = Module2.TypeID then
      begin
        if Module1.Row > Module2.Row then
          Result := -1
        else if Module1.Row = Module2.Row then
        begin
          if Module1.Col > Module2.Col then
            Result := -1
          else if Module1.Col = Module2.Col then
            Result := 0
          else
            Result := 1;
        end
        else
          Result := 1;
      end
      else
        Result := 1;
    end));
end;

function ModuleListUnitsRect(aModuleList: TList<IG2Module>): TRect;
var
  Module: IG2Module;
begin
  if aModuleList.Count = 0 then
  begin
    Result := Rect(0, 0, 0, 0);
  end
  else
  begin
    Result := Rect(10000, 10000, -10000, -10000);
    for Module in aModuleList do
    begin
      if Module.Row < Result.Top then
        Result.Top := Module.Row;
      if Module.Row + Module.HeightUnits > Result.Bottom then
        Result.Bottom := Module.Row + Module.HeightUnits;
      if Module.Col < Result.Left then
        Result.Left := Module.Col;
      if Module.Col + 1 > Result.Right then
        Result.Right := Module.Col + 1;
    end;
  end;
end;

function ModuleListModuleAtLocation(aModuleList: TList<IG2Module>;
const aRow, aCol: integer): IG2Module;
var
  Module: IG2Module;
begin
  Result := nil;

  for Module in aModuleList do
  begin
    if (Module.Col = aCol) and (aRow >= Module.Row) and
      (aRow < (Module.Row + Module.HeightUnits)) then
    begin
      Result := Module;
      exit;
    end;
  end;
end;

function ModuleListFindModule(aModuleList: TList<IG2Module>;
const aModuleIndex: byte): IG2Module;
var
  Module: IG2Module;
begin
  Result := nil;

  for Module in aModuleList do
  begin
    if (Module.ModuleIndex = aModuleIndex) then
    begin
      Result := Module;
      exit;
    end;
  end;
end;

function ModuleListModuleAbove(aModuleList: TList<IG2Module>;
const aModuleIndex: byte): IG2Module;
var
  Module, NextModule: IG2Module;
  dx, dy, ds: single;
begin
  Module := ModuleListFindModule(aModuleList, aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for NextModule in aModuleList do
  begin
    if NextModule.ParameterCount > 0 then
    begin
      if NextModule.Row < Module.Row then
      begin
        dx := NextModule.Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := NextModule.Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx * dx + dy * dy) < ds) then
        begin
          Result := NextModule;
          ds := sqrt(dx * dx + dy * dy);
        end;
      end;
    end;
  end;
end;

function ModuleListModuleUnder(aModuleList: TList<IG2Module>;
const aModuleIndex: byte): IG2Module;
var
  Module, NextModule: IG2Module;
  dx, dy, ds: single;
begin
  Module := ModuleListFindModule(aModuleList, aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for NextModule in aModuleList do
  begin
    if NextModule.ParameterCount > 0 then
    begin
      if NextModule.Row > Module.Row then
      begin
        dx := NextModule.Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := NextModule.Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx * dx + dy * dy) < ds) then
        begin
          Result := NextModule;
          ds := sqrt(dx * dx + dy * dy);
        end;
      end;
    end;
  end;
end;

function ModuleListModuleLeft(aModuleList: TList<IG2Module>;
const aModuleIndex: byte): IG2Module;
var
  Module, NextModule: IG2Module;
  dx, dy, ds: single;
begin
  Module := ModuleListFindModule(aModuleList, aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for NextModule in aModuleList do
  begin
    if NextModule.ParameterCount > 0 then
    begin
      if NextModule.Col < Module.Col then
      begin
        dx := NextModule.Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := NextModule.Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx * dx + dy * dy) < ds) then
        begin
          Result := NextModule;
          ds := sqrt(dx * dx + dy * dy);
        end;
      end;
    end;
  end;
end;

function ModuleListModuleRight(aModuleList: TList<IG2Module>;
const aModuleIndex: byte): IG2Module;
var
  Module, NextModule: IG2Module;
  dx, dy, ds: single;
begin
  Module := ModuleListFindModule(aModuleList, aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for NextModule in aModuleList do
  begin
    if NextModule.ParameterCount > 0 then
    begin
      if NextModule.Col > Module.Col then
      begin
        dx := NextModule.Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := NextModule.Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx * dx + dy * dy) < ds) then
        begin
          Result := NextModule;
          ds := sqrt(dx * dx + dy * dy);
        end;
      end;
    end;
  end;
end;

function ModuleListMaxModuleIndex(aModuleList: TList<IG2Module>): integer;
var
  Module: IG2Module;
begin
  Result := 0;
  for Module in aModuleList do
    if Module.ModuleIndex > Result then
      Result := Module.ModuleIndex;
end;

function ModuleListNoOffModuleType(aModuleList: TList<IG2Module>;
const aModuleType: byte): integer;
var
  Module: IG2Module;
begin
  Result := 0;
  for Module in aModuleList do
    if Module.TypeID = aModuleType then
      Result := Result + 1;
end;

function ModuleListNoOffExtendedModules(aModuleList: TList<IG2Module>): integer;
var
  Module: IG2Module;
begin
  Result := 0;
  for Module in aModuleList do
    if Module.TypeID in EXTENDED_MODULE_IDS then
      Result := Result + 1;
end;

procedure ModuleListAddWithMove(aModuleList: TList<IG2Module>;

aModule: IG2Module);

var
  i, a, b, c, d, overlap, hole: integer;
  fit: boolean;
begin
  // Add module to modulelist and calc newcol and newrow to
  // fit the module in the existing modules.

  // aModuleList must be sorted on Col Row
  if aModuleList.Count > 0 then
  begin
    hole := 0;
    i := 0;
    fit := false;
    while (i < aModuleList.Count) and not fit do
    begin

      if aModule.NewCol < aModuleList[i].NewCol then
        inc(i)
      else if aModule.NewCol = aModuleList[i].NewCol then
      begin
        // Calc overlap between lines a-b and c-d
        a := aModuleList[i].NewRow;
        b := aModuleList[i].NewRow + aModuleList[i].HeightUnits;
        c := aModule.NewRow;
        d := aModule.NewRow + aModule.HeightUnits;
        // Calc the hole above the current aModule
        hole := a - hole;

        overlap := Min(b, d) - Max(c, a);

        if overlap > 0 then
        begin
          // Is the upper point free and does the new aModule fit in the hole
          // above the current aModule?
          if (c < a) and (aModule.HeightUnits <= hole) then
          begin
            aModule.NewRow := a - aModule.HeightUnits;
            fit := True
          end
          else
          begin
            // Try the space under the current aModule
            aModule.NewRow := b;
            hole := b;
            inc(i);
          end;
        end
        else
        begin
          fit := True;
        end;
      end
      else
      begin
        fit := True;
      end;
    end;

    { if Moved then begin
      // Create move message
      aModule.AddMessMove( aSendMessage, aUndoMessage);
      end; }
  end;

  // Add the new aModule change to the list
  aModuleList.Add(aModule);
end;


// ------------------------------------------------------------------------------
//
// TG2CableList
//
// ------------------------------------------------------------------------------

function CableListFindCable(aCableList: TList<IG2Cable>;
const aModuleIndexFrom, aConnIndexFrom, aModuleIndexTo, aConnIndexTo: byte)
  : IG2Cable;
var
  Cable: IG2Cable;
begin
  Result := nil;

  for Cable in aCableList do
  begin
    if (Cable.ModuleIndexFrom = aModuleIndexFrom) and
      (Cable.ConnIndexFrom = aConnIndexFrom) and
      (Cable.ModuleIndexTo = aModuleIndexTo) and
      (Cable.ConnIndexTo = aConnIndexTo) then
    begin
      Result := Cable;
      exit;
    end;
  end;
end;

// ------------------------------------------------------------------------------
//
// TG2PatchPart
//
// ------------------------------------------------------------------------------

constructor TG2PatchPart.Create(aPatch: IG2Patch;
const aLocationIndex: integer);
begin
  inherited Create;

  SetWeak(@FWPatch, aPatch);

  FLocationIndex := aLocationIndex;

  FFocusedModuleIndex := -1;

  FModuleList := TList<IG2Module>.Create;
  // FSelectedModuleList := TList<IG2Module>.Create;
  FCableList := TList<IG2Cable>.Create;

  FLedComparison := TComparer<IG2Led>.Construct(
    function(const Led1, Led2: IG2Led): integer
    begin
      if Led1.ModuleIndex > Led2.ModuleIndex then
        Result := 1
      else
      begin
        if Led1.ModuleIndex = Led2.ModuleIndex then
        begin
          if Led1.GroupId > Led2.GroupId then
            Result := 1
          else
          begin
            if Led1.GroupId = Led2.GroupId then
              Result := 0
            else
              Result := -1;
          end;
        end
        else
          Result := -1;
      end;
    end);

  FLedList := TList<IG2Led>.Create(FLedComparison);
  FLedStripList := TList<IG2Led>.Create(FLedComparison);
  Init;
end;

function TG2PatchPart.CreateCopy(aPatch: IG2Patch;
const aLocationIndex: integer): IG2PatchPart;
var
  MemStream: TMemoryStream;
  Chunk: TPatchChunk;
  Module: IG2Module;
  i: integer;
begin
  Result := TG2PatchPart.Create(aPatch, aLocationIndex);

  MemStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create(MemStream);
  try
    Write(Chunk, VariationCount);

    MemStream.Position := 0;
    Chunk.ReadChunk;
    Result.Read(Chunk);

    for i := 0 to FModuleList.Count - 1 do
    begin
      Module := Result.FindModule(FModuleList[i].ModuleIndex);
      if assigned(Module) then
        Module.Selected := FModuleList[i].Selected;
    end;

  finally
    Chunk.Free;
    MemStream.Free;
  end;
end;

function TG2PatchPart.CreateCopySelected(aPatch: IG2Patch;
const aLocationIndex: integer): IG2PatchPart;
var
  MemStream: TMemoryStream;
  Chunk: TPatchChunk;
  Module: IG2Module;
  SelectedModuleList: TList<IG2Module>;
  i: integer;
begin
  Result := TG2PatchPart.Create(aPatch, aLocationIndex);

  SelectedModuleList := CreateSelectedModuleList;
  MemStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create(MemStream);
  try
    WriteChunkSelectedModules(SelectedModuleList, C_MODULE_LIST,
      VariationCount, Chunk);
    WriteChunkSelectedModules(SelectedModuleList, C_CABLE_LIST,
      VariationCount, Chunk);
    WriteChunkSelectedModules(SelectedModuleList, C_PARAM_LIST,
      VariationCount, Chunk);
    WriteChunkSelectedModules(SelectedModuleList, C_PARAM_NAMES,
      VariationCount, Chunk);
    WriteChunkSelectedModules(SelectedModuleList, C_MODULE_NAMES,
      VariationCount, Chunk);

    MemStream.Position := 0;
    Chunk.ReadChunk;
    Result.Read(Chunk);

    for i := 0 to FModuleList.Count - 1 do
    begin
      Module := Result.FindModule(FModuleList[i].ModuleIndex);
      if assigned(Module) then
        Module.Selected := FModuleList[i].Selected;
    end;

  finally
    Chunk.Free;
    MemStream.Free;
    SelectedModuleList.Free;
  end;
end;

function TG2PatchPart.CreateLed(const aLedType: TLedType;
const aModuleIndex, aGroupID: byte; const aGroupCount: integer): IG2Led;
begin
  Result := TG2FileLed.Create(aLedType, FLocationIndex, aModuleIndex, aGroupID,
    aGroupCount);
end;

procedure TG2PatchPart.CreateLeds(const aModuleIndex, aModuleType: byte);
var
  i, j, GroupCount: integer;
  Led: IG2Led;
begin
  for i := 0 to High(LedDefs) do
  begin
    if LedDefs[i].ModuleID = aModuleType then
    begin
      GroupCount := GetDataLedsInGroup(aModuleType, LedDefs[i].GroupId);
      if GroupCount = 1 then
      begin
        Led := CreateLed(LedDefs[i].LedType, aModuleIndex, LedDefs[i].GroupId,
          GroupCount);
        FLedList.Add(Led);
      end
      else
      begin
        j := 0;
        while (j < FLedStripList.Count) and
          not((FLedStripList[j].LocationIndex = FLocationIndex) and
          (FLedStripList[j].ModuleIndex = aModuleIndex) and
          (FLedStripList[j].GroupId = LedDefs[i].GroupId)) do
          inc(j);

        if not(j < FLedStripList.Count) then
        begin
          Led := CreateLed(LedDefs[i].LedType, aModuleIndex, LedDefs[i].GroupId,
            GroupCount);
          FLedStripList.Add(Led);
        end;
      end;
    end;
  end;

  for i := 0 to High(MiniVUDefs) do
  begin
    if MiniVUDefs[i].ModuleID = aModuleType then
    begin
      Led := CreateLed(ltMiniVU, aModuleIndex, MiniVUDefs[i].GroupId, 1);
      FLedStripList.Add(Led);
    end;
  end;

  FLedList.Sort;
  FLedStripList.Sort;
end;

destructor TG2PatchPart.Destroy;
var
  Module: IG2Module;
  Cable: IG2Cable;
  Led: IG2Led;
  d: integer;
begin
  for Led in FLedList do
    Led.NotifyDestroy;
  FLedList.Free;
  for Led in FLedStripList do
    Led.NotifyDestroy;
  FLedStripList.Free;

  for Cable in FCableList do
    Cable.NotifyDestroy;
  FCableList.Free;

  for Module in FModuleList do
  begin
    Module.NotifyDestroy;
  end;
  FModuleList.Free;

  SetWeak(@FWPatch, nil);

  inherited;
end;

procedure TG2PatchPart.Init;
var
  i: integer;
  Module: IG2Module;
  Cable: IG2Cable;
  Led: IG2Led;
  d: integer;
begin
  // Remove params from Global knob list
  { if assigned(Perf) then
    for Module in FModuleList do
    Perf.DeleteModuleFromPerf(Module.SlotIndex, Module.LocationIndex, Module); }

  for Led in FLedList do
    Led.NotifyDestroy;
  FLedList.Clear;

  for Led in FLedStripList do
    Led.NotifyDestroy;
  FLedStripList.Clear;

  for Cable in FCableList do
    Cable.NotifyDestroy;
  FCableList.Clear;

  for Module in FModuleList do
  begin
    Module.NotifyDestroy;
  end;
  FModuleList.Clear;

  FFocusedModuleIndex := -1;

  // Location must be assigned
  if FLocationIndex = LOCATION_PATCH then
  begin
    // Init patch parameters
    InitParams;
    // Init morph labels
    Module := FindModule(PATCH_MORPH);
    if not assigned(Module) then
      raise Exception.Create('TG2FilePatchPart.Init : Module ' +
        IntToStr(PATCH_MORPH) + ' not found.');

    for i := 0 to 7 do
      Module.SetParamLabel(8 + i, 0, STD_MORPH_NAMES[i]);
  end;
end;

procedure TG2PatchPart.NotifyObservers(aG2Event: TG2Event;
aG2Object: IG2Object);
begin
  if assigned(Slot) then
    Slot.NotifyObservers(aG2Event, aG2Object);
end;

function TG2PatchPart.ProcessResponseMsg(aSender: TObject;
aMsg: TG2Message): boolean;
begin
  Result := True;
end;

procedure TG2PatchPart.ProcessMsgModuleParamLabel(aMsg: TG2Message);
var
  ModuleIndex, Length: byte;
  Module: IG2Module;
  Chunk: TPatchChunk;
begin
  Chunk := TPatchChunk.Create(aMsg);
  try
    ModuleIndex := ReadByte(aMsg);
    Length := ReadByte(aMsg);
    aMsg.Position := aMsg.Position - 1;

    Module := FindModule(ModuleIndex);
    if not assigned(Module) then
      raise Exception.Create('ProcessMsgModuleParamLabel : Module index ' +
        IntToStr(ModuleIndex) + ' not found.');

    Chunk.ReadBuffer(Length + 1);
    Module.ReadParamLabelList(Chunk);
  finally
    Chunk.Free;
  end;
end;

procedure TG2PatchPart.InitNames;
var
  aName: string;
  Module: IG2Module;
begin
  for Module in FModuleList do
  begin
    aName := GetModuleName(Module.ModuleIndex);
    if aName <> '' then
      Module.ModuleName := aName;
  end;
end;

procedure TG2PatchPart.InitParams;

  procedure AddParam(ID: integer; ParamType: TParamType; ModuleIndex: byte;
  ModuleName: string; ParamIndex: byte; ParamName, DefaultLabel: string;
  LowValue, HighValue, DefaultValue: byte; ButtonText: string;
  InfoFunc: integer);
  var
    Module: IG2Module;
    Param: IG2Param;
  begin
    Module := FindModule(ModuleIndex);
    if not assigned(Module) then
    begin
      Module := CreateModule(ModuleIndex, ModuleIndex);
      AddModule(Module);
      Module.ModuleName := ModuleName;
    end;

    Param := TG2Param.Create(Module, ParamIndex, ParamType, ID, DefaultValue,
      -1, -1, ModuleName, ParamName, ButtonText);

    Module.AddParameter(Param);

    Param.InfoFunctionIndex := InfoFunc;
  end;

begin
  AddParam(182, ptParam, PATCH_VOLUME, 'Vol', VOLUME_LEVEL, 'VolLevel', '', 0,
    127, 100, '', 515);
  AddParam(183, ptParam, PATCH_VOLUME, 'Vol', VOLUME_MUTE, 'VolMute', '', 0, 1,
    0, 'Off;On', 514);
  AddParam(184, ptParam, PATCH_GLIDE, 'Glide', GLIDE_TYPE, 'GlideType', '', 0,
    2, 0, 'Auto;Normal;Off', 512);
  AddParam(185, ptParam, PATCH_GLIDE, 'Glide', GLIDE_SPEED, 'GlideSpeed', '', 0,
    127, 0, '', 511);
  AddParam(186, ptParam, PATCH_BEND, 'Bend', BEND_ON_OFF, 'BendOnOff', '', 0, 1,
    0, 'Off;On', 514);
  AddParam(187, ptParam, PATCH_BEND, 'Bend', BEND_RANGE, 'BendRange', '', 0, 23,
    0, '', 513);
  AddParam(188, ptParam, PATCH_VIBRATO, 'Vibrato', VIBRATO_MOD, 'VibrMod', '',
    0, 2, 0, 'Wheel;AfTouch;Off', 510);
  AddParam(189, ptParam, PATCH_VIBRATO, 'Vibrato', VIBRATO_DEPTH, 'VibrDepth',
    '', 0, 127, 0, '', 509);
  AddParam(190, ptParam, PATCH_VIBRATO, 'Vibrato', VIBRATO_RATE, 'VibrRate', '',
    0, 127, 0, '', 0);
  AddParam(191, ptParam, PATCH_ARPEGGIATOR, 'Arp', ARP_ON_OFF, 'ArpOnOff', '',
    0, 1, 0, 'Off;On', 506);
  AddParam(192, ptParam, PATCH_ARPEGGIATOR, 'Arp', ARP_SPEED, 'ArpSpeed', '', 0,
    3, 0, '1/8;1/8T;1/16;1/16T', 505);
  AddParam(193, ptParam, PATCH_ARPEGGIATOR, 'Arp', ARP_DIRECTION, 'ArpDir', '',
    0, 3, 0, 'Up;Dn;UpDn;Rnd', 507);
  AddParam(194, ptParam, PATCH_ARPEGGIATOR, 'Arp', ARP_OCTAVES, 'ArpOct', '', 0,
    3, 0, '1;2;3;4', 508);
  AddParam(195, ptParam, PATCH_SUSTAIN, 'Oct Shft', OCTAVE_SHIFT, 'Oct.Shft',
    '', 0, 0, 0, '-2;-1;0;1;2', 518);
  AddParam(196, ptParam, PATCH_SUSTAIN, 'Sustain', SUSTAIN_PEDAL, 'Sustain', '',
    0, 1, 0, 'Off;On', 517);

  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 0, 'Wheel', 'Wheel', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 1, 'Vel', 'Vel', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 2, 'Keyb', 'Keyb', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 3, 'Aft.Tch', 'Aft.Tch', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 4, 'Sust.Pd', 'Sust.Pd', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 5, 'Ctrl.Pd', 'Ctrl.Pd', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 6, 'P.Stick', 'P.Stick', 0, 127,
    0, '', 179);
  AddParam(197, ptParam, PATCH_MORPH, 'Morph', 7, 'G.Wh2', 'G.Wh2', 0, 127,
    0, '', 179);

  AddParam(198, ptParam, PATCH_MORPH, 'Morph', 8, 'Wheel', '', 0, 1, 0,
    'Knob;Wheel', 519);
  AddParam(199, ptParam, PATCH_MORPH, 'Morph', 9, 'Vel', '', 0, 1, 0,
    'Knob;Vel', 520);
  AddParam(200, ptParam, PATCH_MORPH, 'Morph', 10, 'Keyb', '', 0, 1, 0,
    'Knob;Keyb', 521);
  AddParam(201, ptParam, PATCH_MORPH, 'Morph', 11, 'Aft.Tch', '', 0, 1, 0,
    'Knob;Aft.Tch', 522);
  AddParam(202, ptParam, PATCH_MORPH, 'Morph', 12, 'Sust.Pd', '', 0, 2, 0,
    'Knob;Sust.Pd;G Wh 1', 523);
  AddParam(203, ptParam, PATCH_MORPH, 'Morph', 13, 'Ctrl.Pd', '', 0, 1, 0,
    'Knob;Ctrl.Pd', 524);
  AddParam(204, ptParam, PATCH_MORPH, 'Morph', 14, 'P.Stick', '', 0, 1, 0,
    'Knob;P.Stick', 525);
  AddParam(205, ptParam, PATCH_MORPH, 'Morph', 15, 'G.Wh2', '', 0, 1, 0,
    'Knob;G.Wh2', 526);

  // "Virtual" paramaters for use in parameter pages
  AddParam(206, ptMasterClock, PATCH_MASTERCLOCK, 'Mast Clk', 0, 'M.Clk', '',
    30, 240, 0, '', 501);
  AddParam(207, ptMasterClock, PATCH_MASTERCLOCK, 'Clk Run', 1, 'Clk.Rn', '', 0,
    1, 0, 'Off;On', 502);

  AddParam(208, ptVoiceMode, PATCH_VOICES, 'Voice Cnt', 0, 'Voices', '', 0, 31,
    0, '', 503);
  AddParam(209, ptVoiceMode, PATCH_VOICES, 'Voice Mod', 1, 'Vce.Mde', '', 0, 2,
    0, 'Poly;Mono;Legato', 504);
end;

procedure TG2PatchPart.InvalidateParameters;
var
  i: integer;
begin
  for i := 0 to FModuleList.Count - 1 do
  begin
    FModuleList[i].InvalidateControl;
    FModuleList[i].InvalidateParameters;
  end;
end;

procedure TG2PatchPart.InvalidateConnectors;
var
  i: integer;
begin
  for i := 0 to FModuleList.Count - 1 do
    FModuleList[i].InvalidateConnectors;
end;

procedure TG2PatchPart.InvalidateCables;
var
  i: integer;
begin
  for i := 0 to FCableList.Count - 1 do
    FCableList[i].Invalidate;
end;

function TG2PatchPart.CheckParametersFit(aSrcePatch: IG2PatchPart): boolean;
var
  SrceModuleList, DestModuleList, SelectedModuleList: TList<IG2Module>;
  sl: TStringList;
  i, m: integer;
begin
  Result := false;

  SelectedModuleList := CreateSelectedModuleList;
  SrceModuleList := TList<IG2Module>.Create;
  DestModuleList := TList<IG2Module>.Create;
  sl := TStringList.Create;
  try
    // Make a list of module types
    for m := 0 to aSrcePatch.ModuleList.Count - 1 do
    begin
      if sl.IndexOf(IntToStr(aSrcePatch.ModuleList[m].TypeID)) = -1 then
        sl.Add(IntToStr(aSrcePatch.ModuleList[m].TypeID));
    end;

    for i := 0 to sl.Count - 1 do
    begin
      SrceModuleList.Clear;
      DestModuleList.Clear;

      for m := 0 to aSrcePatch.ModuleList.Count - 1 do
      begin
        if IntToStr(aSrcePatch.ModuleList[m].TypeID) = sl[i] then
          SrceModuleList.Add(aSrcePatch.ModuleList[m]);
      end;

      for m := 0 to SelectedModuleList.Count - 1 do
      begin
        if IntToStr(SelectedModuleList[m].TypeID) = sl[i] then
          DestModuleList.Add(SelectedModuleList[m]);
      end;

      if SrceModuleList.Count <> DestModuleList.Count then
        exit;
    end;
    Result := True;

  finally
    sl.Free;
    DestModuleList.Free;
    SrceModuleList.Free;
    SelectedModuleList.Free;
  end;
end;

function TG2PatchPart.ModuleAtLocation(const aRow, aCol: integer): IG2Module;
begin
  Result := nil;
  if FLocationIndex = LOCATION_PATCH then
    exit;

  Result := ModuleListModuleAtLocation(FModuleList, aRow, aCol);
end;

procedure TG2PatchPart.Read(aChunk: TPatchChunk);
var
  einde: boolean;
begin
  einde := false;
  repeat
    if not ReadChunk(aChunk) then
    begin
      raise Exception.Create('Error parsing patch data.');
    end
    else if aChunk.FId = C_MODULE_NAMES then
      einde := True; // assuming this is always the last chunk in a patch part

    if not einde then
    begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := True;
    end;
  until einde;
end;

procedure TG2PatchPart.ReadCableList(aCableList: TList<IG2Cable>;
aChunk: TPatchChunk);
var
  i: integer;
  Color: TBits3;
  ModuleIndexFrom: TBits8;
  ConnIndexFrom: TBits6;
  LinkType: TBits1;
  ModuleIndexTo: TBits8;
  ConnIndexTo: TBits6;
  NumCables: TBits10;
  // Unknown         : TBits12;
  Cable: IG2Cable;
  ModuleFrom, ModuleTo: IG2Module;
  ConnFrom, ConnTo: IG2Connector;
begin
  // FLocation   : TBits2;
  // FUnknown    : TBits12;
  // FCableCount : TBits10;
  // FCables     : array of TCable;

  // FUnknown    := aChunk.ReadBits( 12);
  aChunk.ReadBits(12); // Check zero!!!!!
  NumCables := aChunk.ReadBits(10);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Num Cables = ' + IntToStr(NumCables), LOGCMD_NUL);
    aChunk.Log.add_log_line('Col MFr CFr Lkp MTo CTo', LOGCMD_NUL);
  end;

  for i := 0 to NumCables - 1 do
  begin
    Color := aChunk.ReadBits(3);
    ModuleIndexFrom := aChunk.ReadBits(8);
    ConnIndexFrom := aChunk.ReadBits(6);
    LinkType := aChunk.ReadBits(1);
    ModuleIndexTo := aChunk.ReadBits(8);
    ConnIndexTo := aChunk.ReadBits(6);

    if assigned(aChunk.Log) then
      aChunk.Log.add_log_line(Format('%3d %3d %3d %3d %3d %3d',
        [Color, ModuleIndexFrom, ConnIndexFrom, LinkType, ModuleIndexTo,
        ConnIndexTo]), LOGCMD_NUL);

    Cable := CableListFindCable(aCableList, ModuleIndexFrom, ConnIndexFrom,
      ModuleIndexTo, ConnIndexTo);
    if assigned(Cable) then
    begin
      Cable.CableColor := TCableColor(Color);
      // Cable.LinkType := TLinkType(LinkType);
    end
    else
    begin
      ModuleFrom := FindModule(ModuleIndexFrom);
      ModuleTo := FindModule(ModuleIndexTo);
      if assigned(ModuleFrom) and assigned(ModuleTo) then
      begin
        if LinkType = 0 then
          ConnFrom := ModuleFrom.InConnector[ConnIndexFrom]
        else
          ConnFrom := ModuleFrom.OutConnector[ConnIndexFrom];
        ConnTo := ModuleTo.InConnector[ConnIndexTo];

        if assigned(ConnFrom) and assigned(ConnTo) then
        begin
          Cable := TG2FileCable.Create(ConnFrom, ConnTo, Color);
          AddCable(Cable);
        end;
      end;
    end;
  end;
end;

function TG2PatchPart.ReadChunk(aChunk: TPatchChunk): boolean;
begin
  Result := True;
  case aChunk.FId of
    C_MODULE_LIST: // $4a
      begin // Module list
        ReadModuleList(FModuleList, aChunk);
      end;
    C_CABLE_LIST: // $52
      begin // Cable list
        ReadCableList(FCableList, aChunk)
      end;
    C_PARAM_LIST: // $4d
      begin // Parameter list
        ReadParamList(FModuleList, aChunk);
      end;
    C_PARAM_NAMES: // $5b
      begin // Parmeter names
        ReadParamLabelList(FModuleList, aChunk);
      end;
    C_MODULE_NAMES: // $5a
      begin // Module names
        ReadModuleLabelList(FModuleList, aChunk);
      end;
  else
    begin
      Result := false;
    end;
  end;
end;

procedure TG2PatchPart.ReadModuleLabelList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  NumNames, i, ModuleIndex: integer;
  Module: IG2Module;
begin
  // FLocation    : TBits2;
  // FUnknown     : TBits6;
  // FNameCount   : TBits8
  // FModuleNames : array of TModuleName;

  // FUnknown     := aChunk.ReadBits( 6);
  NumNames := aChunk.ReadBits(8);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Module labels, count = ' + IntToStr(NumNames),
      LOGCMD_NUL);
    aChunk.Log.add_log_line('Mod Label', LOGCMD_NUL);
  end;

  for i := 0 to NumNames - 1 do
  begin

    ModuleIndex := aChunk.ReadBits(8);

    Module := FindModule(ModuleIndex);
    if assigned(Module) then
    begin
      Module.ModuleName := aChunk.ReadName;

      if assigned(aChunk.Log) then
        aChunk.Log.add_log_line(Format('%3d', [ModuleIndex]) + ' ' +
          Module.ModuleName, LOGCMD_NUL);
    end
    else if assigned(aChunk.Log) then
      aChunk.Log.add_log_line(Format('%3d', [ModuleIndex]) + ' not found.',
        LOGCMD_NUL);

  end;
end;

procedure TG2PatchPart.ReadModuleList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  i, ModuleCount: integer;
  ModuleIndex, ModuleType: byte;
  Module: IG2Module;
begin
  // FLocation       : TBits2;
  // FModuleCount    : TBits8;
  // FModules        : array of TModule;

  ModuleCount := aChunk.ReadBits(8);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Module list, count = ' + IntToStr(ModuleCount),
      LOGCMD_NUL);
    aChunk.Log.add_log_line('Typ Idx Row Col Clr Upr Isl Un1 Mde', LOGCMD_NUL);
  end;

  for i := 0 to ModuleCount - 1 do
  begin
    ModuleType := aChunk.ReadBits(8);
    ModuleIndex := aChunk.ReadBits(8);

    Module := ModuleListFindModule(aModuleList, ModuleIndex);

    if not assigned(Module) then
    begin
      Module := CreateModule(ModuleIndex, ModuleType);
      Module.Read(aChunk);
      // aModuleList.Add(Module);
      AddModule(Module);
    end
    else
      Module.Read(aChunk);

  end;
end;

procedure TG2PatchPart.ReadParamLabelList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  i, ModuleCount, ModuleIndex: integer;
  Module: IG2Module;
begin
  // FLocation       : Tbits2;
  // FModuleCount    : TBits8
  // FModules        : array of TModuleParamLabelList;

  ModuleCount := aChunk.ReadBits(8);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Param labels, count = ' + IntToStr(ModuleCount),
      LOGCMD_NUL);
    aChunk.Log.add_log_line('Mod Len', LOGCMD_NUL);
  end;

  for i := 0 to ModuleCount - 1 do
  begin
    ModuleIndex := aChunk.ReadBits(8);

    Module := FindModule(ModuleIndex);
    if assigned(Module) then
      Module.ReadParamLabelList(aChunk);
  end;
end;

procedure TG2PatchPart.ReadParamList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  i: integer;
  ModuleCount, ModuleIndex, VariationCount: integer;
  Module: IG2Module;
begin
  // FLocation  : Tbits2;
  // FSetCount  : TBits8;
  // FUnknown   : TBits8  <= this must be variationcount, there seem to be 10 variations in patches that come from usb
  // FParamSets : array of TModuleVariationList;

  ModuleCount := aChunk.ReadBits(8); // Number of modules in the list
  VariationCount := aChunk.ReadBits(8); // Number of variations in the list

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Num sets = ' + IntToStr(ModuleCount) +
      ', var cnt = ' + IntToStr(VariationCount), LOGCMD_NUL);
    aChunk.Log.add_log_line('Var Params', LOGCMD_NUL);
  end;

  for i := 0 to ModuleCount - 1 do
  begin
    ModuleIndex := aChunk.ReadBits(8);

    Module := FindModule(ModuleIndex);
    if assigned(Module) then
      Module.ReadParamList(aChunk, VariationCount);
  end;
end;

procedure TG2PatchPart.Write(aChunk: TPatchChunk; aVariationCount: byte);
begin
  WriteChunk(C_MODULE_LIST, aChunk, aVariationCount);
  WriteChunk(C_CABLE_LIST, aChunk, aVariationCount);
  WriteChunk(C_PARAM_LIST, aChunk, aVariationCount);
  WriteChunk(C_PARAM_NAMES, aChunk, aVariationCount);
  WriteChunk(C_MODULE_NAMES, aChunk, aVariationCount);
end;

procedure TG2PatchPart.WriteCableList(aCableList: TList<IG2Cable>;
aChunk: TPatchChunk);
var
  Cable: IG2Cable;
  Unknown: TBits12;
begin
  Unknown := 0;
  // aChunk.WriteBits(FUnknown, 12);
  aChunk.WriteBits(Unknown, 12); // CHECK!!!

  aChunk.WriteBits(aCableList.Count, 10);
  for Cable in aCableList do
    Cable.Write(aChunk);
end;

procedure TG2PatchPart.WriteChunk(aID: byte; aChunk: TPatchChunk;
aVariationCount: byte);
begin
  case aID of
    C_MODULE_LIST: // $4a Module list
      begin
        WriteModuleList(FModuleList, aChunk);
        aChunk.WriteChunk(C_MODULE_LIST);
      end;
    C_CABLE_LIST: // $52 Cable list
      begin
        WriteCableList(FCableList, aChunk);
        aChunk.WriteChunk(C_CABLE_LIST);
      end;
    C_PARAM_LIST: // $4d Parameter list
      begin
        WriteParamList(FModuleList, aVariationCount, aChunk);
        aChunk.WriteChunk(C_PARAM_LIST);
      end;
    C_PARAM_NAMES: // $5b Parmeter names
      begin
        WriteParamLabelList(FModuleList, aChunk);
        aChunk.WriteChunk(C_PARAM_NAMES);
      end;
    C_MODULE_NAMES: // $5a Module names
      begin
        WriteModuleLabelList(FModuleList, aChunk);
        aChunk.WriteChunk(C_MODULE_NAMES);
      end;
  end;
end;

procedure TG2PatchPart.WriteChunkSelectedModules(aModuleList: TList<IG2Module>;
const aID: byte; const aVariationCount: byte; aChunk: TPatchChunk);
var
  i: integer;
  InnerCableList: TList<IG2Cable>;
begin
  // Write chunks only with data of modules in aModules
  case aID of
    C_MODULE_LIST: // $4a Module list
      begin
        aChunk.WriteBits(aModuleList.Count, 8);
        for i := 0 to aModuleList.Count - 1 do
        begin
          aModuleList[i].Write(aChunk);
        end;
        aChunk.WriteChunk(C_MODULE_LIST);
      end;
    C_CABLE_LIST: // $52 Cable list
      begin
        InnerCableList := GetInnerCableList(aModuleList);
        try
          aChunk.WriteBits($00, 12); // Unknown, probably spacer CHECK
          aChunk.WriteBits(InnerCableList.Count, 10);
          for i := 0 to InnerCableList.Count - 1 do
          begin
            InnerCableList[i].Write(aChunk);
          end;
          aChunk.WriteChunk(C_CABLE_LIST);
        finally
          InnerCableList.Free;
        end;
      end;
    C_PARAM_LIST: // $4d Parameter list
      begin
        WriteParamList(aModuleList, aVariationCount, aChunk);
        aChunk.WriteChunk(C_PARAM_LIST);
      end;
    C_PARAM_NAMES: // $5b Parameter names
      begin
        WriteParamLabelList(aModuleList, aChunk);
        aChunk.WriteChunk(C_PARAM_NAMES);
      end;
    C_MODULE_NAMES: // $5a Module names
      begin
        WriteModuleLabelList(aModuleList, aChunk);
        aChunk.WriteChunk(C_MODULE_NAMES);
      end;
  end;
end;

procedure TG2PatchPart.WriteModuleLabelList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  Module: IG2Module;
  TempList: TList<IG2Module>;
begin
  TempList := TList<IG2Module>.Create;
  try
    // We store the labels ordered by module index because that is how it is
    // done in the Clavia editor
    for Module in aModuleList do
      TempList.Add(Module);

    TempList.Sort(TComparer<IG2Module>.Construct(ModuleIndexCompare));

    aChunk.WriteBits(TempList.Count, 8);
    for Module in TempList do
    begin
      aChunk.WriteBits(Module.ModuleIndex, 8);
      aChunk.WriteName(Module.ModuleName);
    end;
  finally
    TempList.Free;
  end;
end;

procedure TG2PatchPart.WriteModuleList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  Module: IG2Module;
  ModuleCount: integer;
begin
  ModuleCount := aModuleList.Count;
  aChunk.WriteBits(ModuleCount, 8);
  for Module in aModuleList do
    Module.Write(aChunk);
end;

procedure TG2PatchPart.WriteParamLabelList(aModuleList: TList<IG2Module>;
aChunk: TPatchChunk);
var
  Module: IG2Module;
  TempList: TList<IG2Module>;
begin
  // Count modules with params with labels
  TempList := TList<IG2Module>.Create;
  try
    for Module in aModuleList do
      if Module.ParamLabelCount > 0 then
        TempList.Add(Module);

    TempList.Sort(TComparer<IG2Module>.Construct(ModuleIndexCompare));

    aChunk.WriteBits(TempList.Count, 8);

    for Module in TempList do
      if Module.ParamLabelCount > 0 then
        Module.WriteParamLabelList(aChunk);
  finally
    TempList.Free;
  end;
end;

procedure TG2PatchPart.WriteParamList(aModuleList: TList<IG2Module>;
aVariationCount: byte; aChunk: TPatchChunk);
var
  Module: IG2Module;
  TempList: TList<IG2Module>;
begin
  // Count modules with parameters
  TempList := TList<IG2Module>.Create;
  try
    for Module in aModuleList do
      if Module.ParameterCount > 0 then
        TempList.Add(Module);

    TempList.Sort(TComparer<IG2Module>.Construct(ModuleIndexCompare));

    aChunk.WriteBits(TempList.Count, 8);
    // if aModuleList.Count = 0 then
    // aChunk.WriteBits( 0, 8)
    // else
    aChunk.WriteBits(aVariationCount, 8);

    for Module in TempList do
      if Module.ParameterCount > 0 then
        Module.WriteParamList(aChunk, aVariationCount);

    { aChunk.WriteBits(aModuleList.Count, 8);
      if aModuleList.Count = 0 then
      aChunk.WriteBits( 0, 8)
      else
      aChunk.WriteBits( aVariationCount, 8);
      for Module in aModuleList do
      Module.WriteParamList( aChunk, aVariationCount); }
  finally
    TempList.Free;
  end;
end;

procedure TG2PatchPart.WriteMessAddModule(const aModule: IG2Module;
aStream: TStream);
var
  i: integer;
begin
  WriteByte(aStream, S_ADD_MODULE);
  WriteByte(aStream, aModule.TypeID);
  WriteByte(aStream, FLocationIndex);
  WriteByte(aStream, aModule.ModuleIndex);
  WriteByte(aStream, aModule.Col);
  WriteByte(aStream, aModule.Row);
  WriteByte(aStream, 0);
  WriteByte(aStream, aModule.Uprate);
  WriteByte(aStream, aModule.IsLed);

  for i := 0 to aModule.ModeCount - 1 do
  begin
    WriteByte(aStream, aModule.Mode[i].GetValue);
  end;

  WriteClaviaString(aStream, aModule.ModuleName);
end;

procedure TG2PatchPart.WriteMessDeleteModule(const aModuleIndex: byte;
aStream: TStream);
begin
  WriteByte(aStream, S_DEL_MODULE);
  WriteByte(aStream, FLocationIndex);
  WriteByte(aStream, aModuleIndex);
end;

procedure TG2PatchPart.AddMessAddCable(const aFromModuleIndex: byte;
aFromConnectorKind: TConnectorKind; aFromConnectorIndex: byte;
aFromConnectorColor: byte; aToModuleIndex: byte;
aToConnectorKind: TConnectorKind; aToConnectorIndex: byte;
aToConnectorColor: byte; aSendMessage, aUndoMessage: TG2SendMessage);
var
  FromModule, ToModule: IG2Module;
  FromConnector, ToConnector: IG2Connector;
  ModuleUprateList: TList<IG2Module>;
begin
  ModuleUprateList := TList<IG2Module>.Create;
  try
    ResetModuleUprates;

    FromModule := FindModule(aFromModuleIndex);
    ToModule := FindModule(aToModuleIndex);

    // The to-connector must be an input, switch if it's an output
    if aToConnectorKind = ckOutput then
    begin

      FromConnector := ToModule.OutConnector[aToConnectorIndex];
      if aFromConnectorKind = ckInput then
        ToConnector := FromModule.InConnector[aFromConnectorIndex]
      else
        ToConnector := FromModule.OutConnector[aFromConnectorIndex];

      if (not assigned(FromConnector)) or (not assigned(ToConnector)) then
        exit;

      aUndoMessage.AddMessCableDelete(FLocationIndex, aToModuleIndex,
        aToConnectorIndex, ord(aToConnectorKind), aFromModuleIndex,
        aFromConnectorIndex, ord(aFromConnectorKind));

      aSendMessage.AddMessCableAdd(FLocationIndex, aToModuleIndex,
        aToConnectorIndex, ord(aToConnectorKind), aFromModuleIndex,
        aFromConnectorIndex, ord(aFromConnectorKind), aToConnectorColor);
    end
    else
    begin

      ToConnector := ToModule.InConnector[aToConnectorIndex];
      if aFromConnectorKind = ckInput then
        FromConnector := FromModule.InConnector[aFromConnectorIndex]
      else
        FromConnector := FromModule.OutConnector[aFromConnectorIndex];

      if (not assigned(FromConnector)) or (not assigned(ToConnector)) then
        exit;

      aUndoMessage.AddMessCableDelete(FLocationIndex, aFromModuleIndex,
        aFromConnectorIndex, ord(aFromConnectorKind), aToModuleIndex,
        aToConnectorIndex, ord(aToConnectorKind));

      aSendMessage.AddMessCableAdd(FLocationIndex, aFromModuleIndex,
        aFromConnectorIndex, ord(aFromConnectorKind), aToModuleIndex,
        aToConnectorIndex, ord(aToConnectorKind), aFromConnectorColor);
    end;

    CreateModuleUprateList(ToConnector, FromConnector.Rate, ModuleUprateList);

    // Add messages for uprate changes and cable color changes
    AddMessUprateChanges(ModuleUprateList, aSendMessage, aUndoMessage);
  finally
    ModuleUprateList.Free;
  end;
end;

procedure TG2PatchPart.AddMessAddModule(const aModuleTypeID, aCol, aRow: byte;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  ModuleChangeList: TModuleChangeList;
  ModuleChange: TModuleChange;
  ModuleIndex: byte;
  ModuleName: string;
  mi: integer;
  // ParameterLabelList : TParameterLabelList;
  Module: IG2Module;
  SubMess: TG2Message;
begin
  // Make move messages for dest modules to accomodate the modules in the source modules
  ModuleChangeList := TModuleChangeList.Create;
  try
    // Get the module data
    mi := GetDataModuleIndex(aModuleTypeID);

    ModuleIndex := GetMaxModuleIndex + 1;
    ModuleName := ModuleDefs[mi].ModuleName +
      IntToStr(GetUniqueModuleNameSeqNr(ModuleDefs[mi].ModuleName));

    // A Module change for the new module
    ModuleChange := TModuleChange.Create(ModuleIndex, ModuleName,
      ModuleDefs[mi].ModuleName, aRow, aCol, ModuleDefs[mi].Height);

    { ModuleChange.ModuleFileName := ModuleDefs[mi].ModuleName;
      ModuleChange.NewModuleName := ModuleName;
      ModuleChange.Height := ModuleDefs[mi].Height;

      ModuleChange.OldModuleIndex := ModuleIndex;
      ModuleChange.NewModuleIndex := ModuleIndex;

      ModuleChange.OldRow := aRow;
      ModuleChange.NewRow := aRow;
      ModuleChange.OldCol := aCol;
      ModuleChange.NewCol := aCol; }

    ModuleChangeList.Add(ModuleChange);

    // Existing modules must accomodate the new one
    for Module in FModuleList do
    begin
      ModuleChange := TModuleChange.Create(Module.ModuleIndex,
        Module.ModuleName, Module.ModuleFileName, Module.Col, Module.Row,
        Module.HeightUnits);
      ModuleChangeList.Add(ModuleChange);
    end;

    ModuleChangeList.RemoveOverlap;

    ModuleChangeList.AddMessages(FLocationIndex, aSendMessage, aUndoMessage);
  finally
    ModuleChangeList.Free;
  end;

  TG2FileModule.WriteMessNewModule(FLocationIndex, ModuleIndex, aModuleTypeID,
    aRow, aCol, ModuleName, aSendMessage);

  SubMess := TG2Message.Create;
  try
    // Create undo add module message
    WriteMessDeleteModule(ModuleIndex, SubMess);
    aUndoMessage.Add(SubMess);
  finally
    SubMess.Free;
  end;
end;

procedure TG2PatchPart.AddMessAddPatchPart(aSrcePatchPart: IG2PatchPart;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  ModuleChangeList: TModuleChangeList;
  ModuleUprateList: TList<IG2Module>;
  ModuleChange: TModuleChange;
  SelectedModuleList: TList<IG2Module>;
  Module: IG2Module;
  Cable: IG2Cable;
  Chunk: TPatchChunk;
  Msg: TG2Message;
begin
  Msg := TG2Message.Create;
  Chunk := TPatchChunk.Create(Msg);
  SelectedModuleList := aSrcePatchPart.CreateSelectedModuleList;
  try
    // Renumber the selected modules in the source patch part
    aSrcePatchPart.RenumberModules(GetMaxModuleIndex + 1);

    // Uprates must be calculated
    aSrcePatchPart.ResetModuleUprates;

    // Write add modules and undo add modules messages
    for Module in SelectedModuleList do
    begin
      Module.AddMessModuleAdd(FLocationIndex, aSendMessage, aUndoMessage);
    end;

    ModuleUprateList := TList<IG2Module>.Create;
    try
      // Add undo add cables for the inner cables
      for Cable in aSrcePatchPart.CableList do
      begin
        if Cable.ConnFrom.Module.Selected and Cable.ConnTo.Module.Selected then
        begin
          // Inner cable
          aUndoMessage.AddMessCableDelete(Cable.LocationIndex,
            Cable.ModuleIndexFrom, Cable.ConnIndexFrom, ord(Cable.ConnFromKind),
            Cable.ModuleIndexTo, Cable.ConnIndexTo, ord(Cable.ConnToKind));
        end
        else
          // Outer cable
          Cable.ToDelete := True;
      end;

      // Add Uprate changes for the outer cables
      for Cable in aSrcePatchPart.CableList do
      begin
        if (not Cable.ConnFrom.Module.Selected) and Cable.ConnTo.Module.Selected
        then
        begin

          aSrcePatchPart.CreateModuleUprateList(Cable.ConnTo,
            Cable.ConnTo.DefRate, ModuleUprateList);
        end
        else if Cable.ConnFrom.Module.Selected and
          (not Cable.ConnTo.Module.Selected) then
        begin

          aSrcePatchPart.CreateModuleUprateList(Cable.ConnFrom,
            Cable.ConnFrom.DefRate, ModuleUprateList);
        end;
      end;

      // Add messages for uprate changes and cable color changes
      AddMessUprateChanges(ModuleUprateList, aSendMessage, aUndoMessage);

    finally
      ModuleUprateList.Free;
    end;

    // Write chunks
    Chunk.WriteBits(FLocationIndex, 2);
    aSrcePatchPart.WriteChunkSelectedModules(SelectedModuleList, C_CABLE_LIST,
      GetVariationCount, Chunk);
    Chunk.WriteBits(FLocationIndex, 2);
    aSrcePatchPart.WriteChunkSelectedModules(SelectedModuleList, C_PARAM_LIST,
      GetVariationCount, Chunk);
    Chunk.WriteBits(FLocationIndex, 2);
    aSrcePatchPart.WriteChunkSelectedModules(SelectedModuleList, C_PARAM_NAMES,
      GetVariationCount, Chunk);
    Chunk.WriteBits(FLocationIndex, 2);
    Chunk.WriteBits(0, 6);
    aSrcePatchPart.WriteChunkSelectedModules(SelectedModuleList, C_MODULE_NAMES,
      GetVariationCount, Chunk);

    // Make move messages for dest modules to accomodate the modules in the source modules
    ModuleChangeList := TModuleChangeList.Create;
    try
      // First put source modules in
      for Module in SelectedModuleList do
      begin
        ModuleChange := TModuleChange.Create(Module.ModuleIndex,
          Module.ModuleName, Module.ModuleFileName, Module.Row, Module.Col,
          Module.HeightUnits);
        ModuleChangeList.Add(ModuleChange);
      end;

      // Then add destination modules
      for Module in FModuleList do
      begin
        ModuleChange := TModuleChange.Create(Module.ModuleIndex,
          Module.ModuleName, Module.ModuleFileName, Module.Row, Module.Col,
          Module.HeightUnits);
        ModuleChangeList.Add(ModuleChange);
      end;

      ModuleChangeList.RemoveOverlap;

      ModuleChangeList.AddMessages(FLocationIndex, aSendMessage, aUndoMessage);
    finally
      ModuleChangeList.Free;
    end;

    aSendMessage.Add(Msg);

  finally
    SelectedModuleList.Free;
    Chunk.Free;
    Msg.Free;
  end;
end;

procedure TG2PatchPart.AddMessDeleteCable(aCable: IG2Cable;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  ModuleUprateList: TList<IG2Module>;
begin
  ModuleUprateList := TList<IG2Module>.Create;
  try
    ResetModuleUprates;

    aSendMessage.AddMessCableDelete(FLocationIndex, aCable.ModuleIndexFrom,
      aCable.ConnIndexFrom, ord(aCable.ConnFromKind), aCable.ModuleIndexTo,
      aCable.ConnIndexTo, ord(aCable.ConnToKind));

    aUndoMessage.AddMessCableAdd(FLocationIndex, aCable.ModuleIndexFrom,
      aCable.ConnIndexFrom, ord(aCable.ConnFromKind), aCable.ModuleIndexTo,
      aCable.ConnIndexTo, ord(aCable.ConnToKind), ord(aCable.CableColor));

    CreateModuleUprateList(aCable.ConnTo, aCable.ConnTo.DefRate,
      ModuleUprateList);

    // Add messages for uprate changes and cable color changes
    AddMessUprateChanges(ModuleUprateList, aSendMessage, aUndoMessage);
  finally
    ModuleUprateList.Free;
  end;
end;

procedure TG2PatchPart.AddMessDeleteModules(aSendMessage,
  aUndoMessage: TG2SendMessage);
var
  i, j: integer;
  Module: IG2Module;
  CableList: TList<IG2Cable>;
  Cable: IG2Cable;
  Connector, ToConnector: IG2Connector;
  TempPatchPart: IG2PatchPart;
  ModuleUprateList: TList<IG2Module>;
begin
  // Make a copy of the complete patch including selected modules list
  TempPatchPart := CreateCopy(nil, FLocationIndex);

  ModuleUprateList := TList<IG2Module>.Create;
  try
    TempPatchPart.ResetModuleUprates;

    // Go through the modules to be deleted and delete the cables first
    for i := 0 to TempPatchPart.ModuleList.Count - 1 do
    begin
      Module := TempPatchPart.ModuleList[i];
      if Module.Selected then
      begin

        for j := 0 to Module.InConnectorCount - 1 do
        begin
          Connector := Module.InConnector[j];
          CableList := Connector.CreateCableList;
          try
            for Cable in CableList do
              Cable.AddMessCableDelete(aSendMessage, aUndoMessage);
          finally
            CableList.Free;
          end;
        end;

        for j := 0 to Module.OutConnectorCount - 1 do
        begin
          Connector := Module.OutConnector[j];
          CableList := Connector.CreateCableList;
          try
            for Cable in CableList do
            begin
              Cable.AddMessCableDelete(aSendMessage, aUndoMessage);

              ToConnector := Cable.ConnTo;
              if ToConnector = Connector then
                // Switch
                ToConnector := Cable.ConnFrom;
              if not ToConnector.Module.Selected then
              begin
                TempPatchPart.CreateModuleUprateList(ToConnector, 0,
                  ModuleUprateList);
              end;
              Cable.ConnFrom := nil;
              Cable.ConnTo := nil;
            end;
          finally
            CableList.Free;
          end;
        end;

        Module.AddMessModuleDelete(aSendMessage, aUndoMessage);
      end;
    end;

    AddMessUprateChanges(ModuleUprateList, aSendMessage, aUndoMessage);

  finally
    ModuleUprateList.Free;
  end;
end;

procedure TG2PatchPart.AddMessPasteParams(aSrcePatchPart: IG2PatchPart;
const aFromVariation, aToVariation: byte;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  i, j, k, Count: integer;
  SrceParam, DestParam: IG2Param;
  SrceMorph: IG2ParamMorph;
  Chunk, UndoChunk: TPatchChunk;
  SubMess: TG2Message;
  SrceSelectedModuleList, DestSelectedModuleList: TList<IG2Module>;
begin
  // Make a list of module pairs
  aSrcePatchPart.SelectAll;

  SrceSelectedModuleList := aSrcePatchPart.CreateSelectedModuleList;
  DestSelectedModuleList := CreateSelectedModuleList;
  try

    if SrceSelectedModuleList.Count <> DestSelectedModuleList.Count then
      raise Exception.Create
        ('Source modules do not match destination modules.');

    Count := SrceSelectedModuleList.Count;
    if Count = 0 then
      exit;

    ModuleListSortOnTypePosition(SrceSelectedModuleList);
    ModuleListSortOnTypePosition(DestSelectedModuleList);

    // Check types and set the moduleindex in the srcepatch equal to the
    // corresponding module in the destination patch
    for i := 0 to Count - 1 do
      if SrceSelectedModuleList[i].TypeID <> DestSelectedModuleList[i].TypeID
      then
        raise Exception.Create
          ('Source modules do not match destination modules.')
      else
        SrceSelectedModuleList[i].ModuleIndex := DestSelectedModuleList[i]
          .ModuleIndex;

    SubMess := TG2Message.Create;
    Chunk := TPatchChunk.Create(aSendMessage);
    UndoChunk := TPatchChunk.Create(SubMess);
    try
      // Write morph set messages and undo

      for i := 0 to Count - 1 do
      begin
        for j := 0 to SrceSelectedModuleList[i].ParameterCount - 1 do
        begin
          SrceParam := SrceSelectedModuleList[i].Param[j];
          DestParam := DestSelectedModuleList[i].Param[j];

          for k := 0 to NMORPHS - 1 do
          begin

            SrceMorph := SrceParam.Morph[k, aFromVariation];
            if not assigned(SrceMorph) then
              SrceMorph := SrceParam.CreateMorph(k, aFromVariation, 0);
            DestParam.AddMessSetMorph(SrceMorph, aToVariation, aSendMessage,
              aUndoMessage);
          end;
        end;
      end;

      // Write param chunk
      Chunk.WriteBits(FLocationIndex, 2);
      aSrcePatchPart.WriteChunkSelectedModules(SrceSelectedModuleList,
        C_PARAM_LIST, GetVariationCount, Chunk);
      SubMess.Clear;
      UndoChunk.WriteBits(FLocationIndex, 2);
      WriteChunkSelectedModules(DestSelectedModuleList, C_PARAM_LIST,
        GetVariationCount, UndoChunk);
      aUndoMessage.Add(SubMess);

    finally
      UndoChunk.Free;
      Chunk.Free;
      SubMess.Free;
    end;
  finally
    DestSelectedModuleList.Free;
    SrceSelectedModuleList.Free;
  end;
end;

procedure TG2PatchPart.CreateModuleUprateList(const aConnector: IG2Connector;
const aNewRate: byte; aModuleList: TList<IG2Module>);
var
  i, j: integer;
  Connector: IG2Connector;
  CableList: TList<IG2Cable>;
  Cable: IG2Cable;
  Module: IG2Module;
begin
  // This function must be called when a cable is added or deleted
  // aUprateValue is the new uprate value of the from-module

  // This function is called before actual changes in the patch are made, so temporary
  // values are used in module (NewUprate) and connector (NewConnectorColor) to calculte the
  // new state.
  // Before using this function, these temporary values must be reset to the actual values
  // with ResetUprateValues

  // See Chapter 10 in manual.

  Module := aConnector.Module;

  // Does the module already have the same uprate value?
  if Module.Uprate = aNewRate then
    exit;

  // Check to module connector color
  if (aNewRate = 1) and (aConnector.NewRate = 1) then
    exit;

  if (aNewRate = 0) and (aConnector.NewRate = 0) then
    exit;

  // If downrate, downrate the module only if it's not uprated by others cables
  // from other modules

  if (aNewRate = 0) then
  begin
    // Check if module is uprated by other cables than this one
    i := 0;
    while (i < Module.InConnectorCount) do
    begin

      Connector := Module.InConnector[i];
      if Connector <> aConnector then
      begin
        CableList := Connector.CreateCableList;
        try
          j := 0;
          while (j < CableList.Count) do
          begin

            if (not CableList[j].ToDelete) and
              (CableList[j].ConnFrom.NewRate = 1) and
              (CableList[j].ConnFrom.Module <> Connector.Module) then
              exit; // Found another cable that uprates this module

            inc(j);
          end;
        finally
          CableList.Free;
        end;
      end;
      inc(i);
    end;
  end;

  // Add module to list
  aModuleList.Add(Module);
  Module.NewUprate := aNewRate;

  // Go through all the outgoing connectors and recursively calc uprate changes of connecting modules
  i := 0;
  while (i < Module.OutConnectorCount) do
  begin

    Connector := Module.OutConnector[i];
    CableList := Connector.CreateCableList;
    try
      j := 0;
      while (j < CableList.Count) do
      begin

        Cable := CableList[j];
        if (not Cable.ToDelete) and
          (Cable.ConnFrom.NewRate <> Cable.ConnTo.NewRate) then
        begin

          if aModuleList.IndexOf(Cable.ConnTo.Module) = -1 then
          begin
            // Get the other connector from the cable
            if Cable.ConnTo = Connector then
              CreateModuleUprateList(Cable.ConnFrom, aNewRate, aModuleList)
            else
              CreateModuleUprateList(Cable.ConnTo, aNewRate, aModuleList);
          end;

        end;

        inc(j);
      end;
    finally
      CableList.Free;
    end;

    inc(i);
  end;
end;

function TG2PatchPart.CreateSelectedModuleList: TList<IG2Module>;
var
  Module: IG2Module;
begin
  Result := TList<IG2Module>.Create;

  for Module in FModuleList do
    if Module.Selected then
      Result.Add(Module);
end;

procedure TG2PatchPart.AddMessUprateChanges(aModuleUprateList: TList<IG2Module>;
aSendMessage, aUndoMessage: TG2SendMessage);
var
  Module: IG2Module;
  Connector: IG2Connector;
  CableList: TList<IG2Cable>;
  Cable: IG2Cable;
  i, j: integer;
begin
  for Module in aModuleUprateList do
  begin
    aSendMessage.AddMessModuleUprate(Module.LocationIndex, Module.ModuleIndex,
      Module.NewUprate);
    aUndoMessage.AddMessModuleUprate(Module.LocationIndex, Module.ModuleIndex,
      Module.Uprate);

    // Add cable color changes
    i := 0;
    while (i < Module.OutConnectorCount) do
    begin

      Connector := Module.OutConnector[i];
      CableList := Connector.CreateCableList;
      try
        j := 0;
        while (j < CableList.Count) do
        begin

          Cable := CableList[j];

          if (Cable.CableColor <> Connector.NewConnectorColor) and
            (Connector.NewConnectorColor in [ccRed, ccBlue, ccYellow, ccOrange])
          then
          begin
            // Write cable change message
            Cable.AddMessCableColor(ord(Connector.NewConnectorColor),
              aSendMessage, aUndoMessage);
          end;

          inc(j);
        end;
      finally
        CableList.Free;
      end;

      inc(i)
    end;
  end;
end;

function TG2PatchPart.GetModuleList: TList<IG2Module>;
begin
  Result := FModuleList;
end;

function TG2PatchPart.GetCableList: TList<IG2Cable>;
begin
  Result := FCableList;
end;

function TG2PatchPart.GetSelectedParam: IG2Param;
var
  Module: IG2Module;
begin
  Result := nil;
  Module := FindModule(FFocusedModuleIndex);
  if assigned(Module) then
    Result := Module.SelectedParam;
end;

function TG2PatchPart.GetSelectedUnitsRect: TRect;
var
  SelectedModuleList: TList<IG2Module>;
begin
  SelectedModuleList := CreateSelectedModuleList;
  try
    Result := ModuleListUnitsRect(SelectedModuleList);
  finally
    SelectedModuleList.Free;
  end;
end;

function TG2PatchPart.GetSlot: IG2Slot;
begin
  if assigned(FWPatch) then
    Result := FWPatch.Slot
  else
    Result := nil;
end;

function TG2PatchPart.GetSlotIndex: byte;
begin
  if assigned(FWPatch) then
    Result := FWPatch.SlotIndex
  else
    Result := 0;
end;

function TG2PatchPart.GetUniqueModuleNameSeqNr(aModuleFileName: string)
  : integer;
var
  m: integer;
  found: boolean;
begin
  Result := 1;
  repeat
    m := 0;
    while (m < FModuleList.Count) and
      (FModuleList[m].ModuleName <> (aModuleFileName + IntToStr(Result))) do
      inc(m);

    found := m < FModuleList.Count;
    if found then
    begin
      inc(Result);
    end;
  until not found;
end;

function TG2PatchPart.GetUnitsRect: TRect;
begin
  Result := ModuleListUnitsRect(FModuleList);
end;

function TG2PatchPart.GetVariationCount: integer;
begin
  Result := Patch.VariationCount
end;

procedure TG2PatchPart.SetFocusedModuleIndex(const aModuleIndex: integer);
var
  OldModule, Module: IG2Module;
begin
  if aModuleIndex <> FFocusedModuleIndex then
  begin
    OldModule := FindModule(FFocusedModuleIndex);
    Module := FindModule(aModuleIndex);
    FFocusedModuleIndex := aModuleIndex;

    if assigned(Module) then
      SelectModule(aModuleIndex, True, True);

    if assigned(OldModule) then
    begin
      OldModule.InvalidateControl;
      OldModule.InvalidateParameters;
    end;

    if assigned(Module) then
    begin
      Module.InvalidateControl;
      Module.InvalidateParameters;

      NotifyObservers(EvtSelectModule, Module);
    end;

  end;
end;

{ procedure TG2PatchPart.SetLocationIndex( const aValue : byte);
  begin
  FLocationIndex := aValue;
  end; }

procedure TG2PatchPart.SetOnAddCable(const aValue: TAddCableEvent);
begin
  FOnAddCable := aValue;
end;

procedure TG2PatchPart.SetOnAddModule(const aValue: TAddModuleEvent);
begin
  FOnAddModule := aValue;
end;

procedure TG2PatchPart.SetOnLedEvent(const aValue: TLedEvent);
begin
  FOnLed := aValue;
end;

procedure TG2PatchPart.DeleteModule(aModuleIndex: integer);
var
  Module: IG2Module;
  i: integer;
begin
  if FFocusedModuleIndex = aModuleIndex then
    FFocusedModuleIndex := -1;

  // RemoveModuleFromSelection(aModuleIndex);

  Module := ModuleListFindModule(FModuleList, aModuleIndex);
  Module.NotifyDestroy;
  FModuleList.Remove(Module);

  for i := FLedList.Count - 1 downto 0 do
    if (FLedList[i].ModuleIndex = aModuleIndex) then
    begin
      FLedList[i].NotifyDestroy;
      FLedList.Delete(i);
    end;

  for i := FLedStripList.Count - 1 downto 0 do
    if (FLedStripList[i].ModuleIndex = aModuleIndex) then
    begin
      FLedStripList[i].NotifyDestroy;
      FLedStripList.Delete(i);
    end;

  FLedList.Sort;
  FLedStripList.Sort;
end;

procedure TG2PatchPart.AddCable(aCable: IG2Cable);
begin
  FCableList.Add(aCable);

  if assigned(FOnAddCable) then
    FOnAddCable(self, FLocationIndex, aCable);

  NotifyObservers(evtCableAdd, aCable);
end;

procedure TG2PatchPart.DeleteCable(aCable: IG2Cable);
begin
  FCableList.Remove(aCable);
  aCable.NotifyDestroy;
  aCable := nil;
end;

function TG2PatchPart.GetFocusedModuleIndex: integer;
begin
  Result := FFocusedModuleIndex;
end;

function TG2PatchPart.GetInnerCableList(const aModuleList: TList<IG2Module>)
  : TList<IG2Cable>;
var
  Cable: IG2Cable;
begin
  Result := TList<IG2Cable>.Create;
  for Cable in FCableList do
  begin
    if (aModuleList.IndexOf(Cable.ConnFrom.Module) <> -1) and
      (aModuleList.IndexOf(Cable.ConnTo.Module) <> -1) then
      if Result.IndexOf(Cable) = -1 then
        Result.Add(Cable);
  end;
end;

function TG2PatchPart.GetLedStripList: TList<IG2Led>;
begin
  Result := FLedStripList;
end;

function TG2PatchPart.GetLedList: TList<IG2Led>;
begin
  Result := FLedList;
end;

function TG2PatchPart.GetLocationIndex: byte;
begin
  Result := FLocationIndex;
end;

function TG2PatchPart.GetMaxModuleIndex: integer;
begin
  Result := ModuleListMaxModuleIndex(FModuleList);
end;

function TG2PatchPart.GetNoOffExtendedModules: integer;
begin
  Result := ModuleListNoOffExtendedModules(FModuleList);
end;

function TG2PatchPart.GetNoOffModuleType(aModuleType: byte): integer;
begin
  Result := ModuleListNoOffModuleType(FModuleList, aModuleType);
end;

function TG2PatchPart.GetOnAddCable: TAddCableEvent;
begin
  Result := FOnAddCable;
end;

function TG2PatchPart.GetOnAddModule: TAddModuleEvent;
begin
  Result := FOnAddModule;
end;

function TG2PatchPart.GetOnLedEvent: TLedEvent;
begin
  Result := FOnLed;
end;

function TG2PatchPart.FindCable(const aFromModuleIndex, aFromConnIndex,
  aToModuleIndex, aToConnIndex: byte): IG2Cable;
begin
  Result := CableListFindCable(FCableList, aFromModuleIndex, aFromConnIndex,
    aToModuleIndex, aToConnIndex);
end;

function TG2PatchPart.FindModule(const aModuleIndex: byte): IG2Module;
begin
  Result := ModuleListFindModule(FModuleList, aModuleIndex);
end;

procedure TG2PatchPart.CopyVariation(aFromVariation, aToVariation: byte);
var
  Module: IG2Module;
begin
  for Module in FModuleList do
    Module.CopyVariation(aFromVariation, aToVariation);
end;

function TG2PatchPart.FindParam(const aModuleIndex, aParamIndex: byte)
  : IG2Param;
var
  Module: IG2Module;
begin
  Module := FindModule(aModuleIndex);
  if assigned(Module) then
    Result := Module.Param[aParamIndex]
  else
    Result := nil;
end;

function TG2PatchPart.GetPatch: IG2Patch;
begin
  Result := FWPatch;
end;

function TG2PatchPart.GetPerf: IG2Perf;
begin
  if assigned(Slot) then
    Result := Slot.Perf
  else
    Result := nil;
end;

function TG2PatchPart.GetModuleRow(const aModule: IG2Module): byte;
var
  i: integer;
begin
  // Volgorde van module in de kolom, tel aantal modules in de kolom
  // met een kleinere row-waarde
  Result := 0;
  for i := 0 to FModuleList.Count - 1 do
  begin
    if (FModuleList.Items[i].Col = aModule.Col) and
      (FModuleList.Items[i].Row < aModule.Row) then
      inc(Result);
  end;
end;

function TG2PatchPart.GetMorphValue(aModuleIndex, aParamIndex, aMorphIndex,
  aVariation: byte): byte;
var
  MorphParameter: IG2ParamMorph;
begin
  Result := 0;
  MorphParameter := Patch.FindMorph(FLocationIndex, aModuleIndex, aParamIndex,
    aMorphIndex, aVariation);
  if assigned(MorphParameter) then
    Result := MorphParameter.Range;
end;

procedure TG2PatchPart.AddModule(aModule: IG2Module);
begin
  FModuleList.Add(aModule);
  if FLocationIndex <> LOCATION_PATCH then
    CreateLeds(aModule.ModuleIndex, aModule.TypeID);

  SelectModule(aModule.ModuleIndex, True, True);
  FFocusedModuleIndex := aModule.ModuleIndex;

  if assigned(FOnAddModule) then
    FOnAddModule(self, FLocationIndex, aModule);

  // NotifyObservers(EvtModuleAdd, aModule);
end;

procedure TG2PatchPart.SelectAll;
var
  i: integer;
begin
  for i := 0 to FModuleList.Count - 1 do
    if not FModuleList.Items[i].Selected then
      SelectModule(FModuleList.Items[i].ModuleIndex, True, false);
end;

procedure TG2PatchPart.SelectModule(const aModuleIndex: byte;
const aValue, aExclusive: boolean);
var
  Module: IG2Module;
begin
  Module := FindModule(aModuleIndex);

  if assigned(Module) and (Module.Selected <> aValue) then
  begin
    // The patch keeps a list of selected modules
    if aValue then
    begin
      if aExclusive then
        UnselectModules;
    end;
    Module.Selected := aValue;
  end;
end;

procedure TG2PatchPart.UnselectModules;
var
  i: integer;
begin
  for i := 0 to FModuleList.Count - 1 do
    if FModuleList.Items[i].Selected then
      SelectModule(FModuleList.Items[i].ModuleIndex, false, True);
end;

procedure TG2PatchPart.SelectNextModuleParam;
var
  Module: IG2Module;
begin
  Module := FindModule(FFocusedModuleIndex);
  if assigned(Module) then
    Module.SelectNextParam;
end;

procedure TG2PatchPart.SelectPrevModuleParam;
var
  Module: IG2Module;
begin
  Module := FindModule(FFocusedModuleIndex);
  if assigned(Module) then
    Module.SelectPrevParam;
end;

function TG2PatchPart.CreateModule(const aModuleIndex, aModuleType: byte)
  : IG2Module;
begin
  Result := TG2FileModule.Create(self as IG2PatchPart, aModuleType,
    aModuleIndex);

  if (FLocationIndex <> LOCATION_PATCH) then
  begin
    GetDataModuleIndex(aModuleType);
    Result.InitModule;
  end;
end;

procedure TG2PatchPart.ResetModuleUprates;
var
  Module: IG2Module;
  Cable: IG2Cable;
begin
  for Module in FModuleList do
    Module.ResetUprate;

  for Cable in FCableList do
    Cable.ToDelete := false;
end;

procedure TG2PatchPart.RenumberModules(aFromIndex: byte);
var
  StartIndex, delta: integer;
  Module: IG2Module;
  SelectedModuleList: TList<IG2Module>;
begin
  // Renumber and rename selected modules in sub patch before insering in another patch

  SelectedModuleList := CreateSelectedModuleList;
  try
    if SelectedModuleList.Count = 0 then
      exit;

    // Get lowest module number in subpatch

    StartIndex := SelectedModuleList[0].ModuleIndex;
    for Module in SelectedModuleList do
    begin
      if Module.ModuleIndex < StartIndex then
        StartIndex := Module.ModuleIndex;
    end;

    delta := aFromIndex - StartIndex;

    // Renumber modules
    for Module in SelectedModuleList do
    begin
      Module.ModuleIndex := Module.ModuleIndex + delta;
      Module.ModuleName := Module.ModuleFileName + IntToStr(Module.ModuleIndex);
      { for i := 0 to Module.ParameterCount - 1 do
        Module.Param[i].ModuleIndex := Module.Param[i].ModuleIndex + delta;
        for i := 0 to Module.ModeCount - 1 do
        Module.Mode[i].ModuleIndex := Module.Mode[i].ModuleIndex + delta; }
    end;

    // Renumber cables
    { for i := 0 to FCableList.Count - 1 do begin
      if FCableList[i].FromConnector.Module.Selected then
      FCableList[i].ModuleFrom := FCableList[i].ModuleFrom + delta;
      if FCableList[i].ToConnector.Module.Selected then
      FCableList[i].ModuleTo := FCableList[i].ModuleTo + delta;
      end; }
  finally
    SelectedModuleList.Free;
  end;
end;

end.
