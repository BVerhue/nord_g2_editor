unit BVE.NMG2Module;

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
  BVE.NMG2Object,
  BVE.NMG2FileIntf;

type
  TG2FileModule = class;

  TModuleParamValues = class
  private
    // Set of parameter values for one module one variation

    FVariation: TBits8;
    FParamValueArray: array of TBits7;

    function GetParamValue(aIndex: integer): TBits7;
    procedure SetParamValue(aIndex: integer; aValue: TBits7);
    function GetParamCount: integer;
    procedure SetParamCount(const Value: integer);
  public
    constructor Create;
    constructor Copy(aParamValues: TModuleParamValues);
    destructor Destroy; override;
    procedure CopyValues(aParamValues: TModuleParamValues);
    procedure Init;
    procedure Read(aChunk: TPatchChunk; aParamCount: integer);
    procedure Write(aChunk: TPatchChunk);

    property Variation: TBits8 read FVariation write FVariation;
    property ParamValues[aIndex: integer]: TBits7 read GetParamValue
      write SetParamValue;
    property ParamCount: integer read GetParamCount write SetParamCount;
  end;

  TModuleVariationList = class(TObjectList<TModuleParamValues>)
  private
    // Set of parameter values for a module all variations
    // 0 .. 7 : variations 1 .. 8, 8 : init variation

    FParameterCount: integer;

    function GetVariation(aIndex: integer): TModuleParamValues;
    procedure SetVariation(aIndex: integer; const aValue: TModuleParamValues);
  public
    constructor Create(AOwnsObjects: boolean);
    destructor Destroy; override;
    procedure Init;
    procedure Read(aChunk: TPatchChunk; aVariationCount: integer);
    procedure Write(aChunk: TPatchChunk; aVariationCount: integer);
    procedure AddNewVariation(aVariation: byte);
    procedure CopyVariation(aFromVariation, aToVariation: byte);
    function FindVariation(aVariation: integer): TModuleParamValues;
    function AddVariation(aValue: TModuleParamValues): integer;
    property Items[aIndex: integer]: TModuleParamValues read GetVariation
      write SetVariation; default;
    property ParameterCount: integer read FParameterCount write FParameterCount;
  end;

  TParamLabel = class
  private
    FNameArray: array of byte; // [0..6]
    function GetName: string;
    procedure SetName(aValue: string); reintroduce;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init;

    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);

    property ParamLabel: string read GetName write SetName;
  end;

  // Most parameters have one label, some (switches) can have more
  TParamLabelList = class(TObjectList<TParamLabel>)
  private
    FIsString: TBits8;
    FParamLen: TBits8;
    FParamIndex: TBits8;

  public
    constructor Create(AOwnsObjects: boolean);
    constructor CopyCreate(AOwnsObjects: boolean;
      aParamLabelParam: TParamLabelList);
    destructor Destroy; override;

    procedure Init;

    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);

    property IsString: TBits8 read FIsString write FIsString;
    property ParamLen: TBits8 read FParamLen write FParamLen;
    property ParamIndex: TBits8 read FParamIndex write FParamIndex;
  end;

  // Labels of all parameters of a module
  TModuleParamLabelList = class(TObjectList<TParamLabelList>)
  private
    FModuleLen: TBits8;

  public
    constructor Create(AOwnsObjects: boolean);
    constructor CopyCreate(AOwnsObjects: boolean;
      aParamLabelModule: TModuleParamLabelList);
    destructor Destroy; override;

    procedure Init;
    procedure InitModuleParamLabelList(aModuleIndex: byte;
      aModuleType: integer);

    function FindParamLabel(aParamIndex: integer): TParamLabelList;

    function GetModuleLabelsLength: integer;
    function GetParameterLabelCount(aParamIndex: byte): integer;
    procedure AddParamLabel(aParamIndex, aLabelIndex: byte; aName: string);
    procedure AddParamLabels(aParamIndex: byte; aNames: string);

    procedure Read(aChunk: TPatchChunk);
    procedure Write(aChunk: TPatchChunk);

    procedure GetLabels(aLines: TStringList);
    procedure SetLabels(aLines: TStringList);
  end;

  TG2FileModule = class(TG2ObservedObject, IG2Module)
  private
    [Weak] FWPatchPart: IG2PatchPart;

    // File storage
    FTypeID: TBits8;
    FModuleIndex: TBits8;
    FCol: TBits7;
    FRow: TBits7;
    FModuleColor: TBits8;
    FUprate: TBits1; // processing rate increased due to audio modulation
    FIsLed: TBits1; // unknown function
    // FUnknown1     : TBits8;
    FUnknown1: TBits6;
    // FUnknown2     : TBits4;
    FModeCount: TBits4;
    FModeInfoArray: TModeInfoArray;

    // Work properties
    FHeightUnits: integer;
    FSelected: boolean;

    // For copying moving
    FNewCol: byte;
    FNewRow: byte;
    FNewModuleIndex: byte;
    FNewModuleName: string;

    FPage: TModulePage;
    FPageIndex: integer;

    FModuleParamList: TModuleVariationList;
    FModuleParamLabelList: TModuleParamLabelList;

    // Interface
    FSelectedParamIndex: integer;
    FModuleName: string;
    FNewUprate: TBits1; // for calculating uprate changes
    FInConnArray: array of IG2Connector;
    FOutConnArray: array of IG2Connector;
    FParamArray: array of IG2Param;
    FModeArray: array of IG2Param;

    FFunctionList: TList<IG2Function>;

    FModuleFileName: string;

    function GetInputConnector(const ConnectorIndex: integer): IG2Connector;
    function GetOutputConnector(const ConnectorIndex: integer): IG2Connector;
    function GetInputConnectorCount: integer;
    function GetOutputConnectorCount: integer;
    function GetParam(const ParamIndex: integer): IG2Param;
    function GetMode(const ModeIndex: integer): IG2Param;
    function GetParameterCount: integer;
    function GetModeCount: integer;
    function GetColor: integer;
    procedure SetUprate(const Value: TBits1);
    procedure SetModeInfo(const aModeIndex: integer; const aValue: TBits6);
    function GetModeInfo(const aModeIndex: integer): TBits6;
    function GetSelectedParam: IG2Param;
    procedure SetSelectedParam(const aValue: IG2Param);
    function GetSelected: boolean;
    //function GetInfoFunction(const aIndex: integer): IG2Function;

    function GetTypeID: TBits8;
    function GetModuleIndex: TBits8;
    function GetCol: TBits7;
    function GetRow: TBits7;
    function GetModuleColor: TBits8;
    function GetUnknown1: TBits6;
    function GetUprate: TBits1;
    function GetIsLed: TBits1;
    function GetPatchPart: IG2PatchPart;
    function GetModuleName: string;
    function GetHeightUnits: integer;
    function GetSlotIndex: byte;
    function GetLocationIndex: byte;
    function GetNewUprate: TBits1;
    function GetModuleFileName: string;
    function GetPage: TModulePage;
    function GetPageIndex: integer;
    function GetModeInfoArray: TModeInfoArray;
    function GetNewCol: byte;
    function GetNewRow: byte;
    function GetNewModuleIndex: byte;
    function GetNewModuleName: string;

    procedure SetNewCol(const aValue: byte);
    procedure SetNewRow(const aValue: byte);
    procedure SetNewModuleIndex(const aValue: byte);
    procedure SetNewModuleName(const aValue: string);

    procedure SetTypeID(const aValue: TBits8);
    procedure SetIsLed(const aValue: TBits1);
    procedure SetHeightUnits(const aValue: integer);
    procedure SetUnknown1(const aValue: TBits6);
    procedure SetNewUprate(const aValue: TBits1);
    procedure SetModuleFileName(const aValue: string);
    procedure SetSelected(const aValue: boolean);

    function GetParamValue(const aVariation, aParamIndex: byte): integer;
    procedure SetParamValue(const aVariation, aParamIndex, aValue: byte);

    procedure SetParamLabel(const aParamIndex, aLabelIndex: byte;
      const aName: string);
    function GetParamLabel(const aParamIndex, aLabelIndex: byte): string;
    procedure SetParamLabels(const aParamIndex: byte; const aNames: string);
    function GetParamLabelCount: integer;
    function GetFocussed: boolean;
  protected
    procedure SetCol(const Value: TBits7); virtual;
    procedure SetRow(const Value: TBits7); virtual;
    procedure SetModuleColor(const Value: TBits8); virtual;
    procedure SetModuleName(const aValue: string); virtual;
    procedure SetModuleIndex(const aValue: TBits8); virtual;
    function GetRowIndex: integer;
  public
    constructor Create(aPatchPart: IG2PatchPart; const aModuleType: byte;
      const aModuleIndex: integer); reintroduce; virtual;
    destructor Destroy; override;

    class procedure WriteMessNewModule(const aLocationIndex, aModuleIndex,
      aModuleType, aRow, aCol: byte; const aModuleName: string;
      aSendMessage: TG2SendMessage);

    procedure Init;

    procedure Read(aChunk: TPatchChunk);
    procedure ReadParamList(aChunk: TPatchChunk; aVariationCount: integer);
    procedure ReadParamLabelList(aChunk: TPatchChunk);

    procedure Write(aChunk: TPatchChunk);
    procedure WriteParamList(aChunk: TPatchChunk; aVariationCount: integer);
    procedure WriteParamLabelList(aChunk: TPatchChunk);

    procedure InitModule; virtual;
    procedure ResetUprate;

    procedure AddParameter(aParameter: IG2Param);

    procedure CopyVariation(aFromVariation, aToVariation: byte);

    function AddInfoFunction(const aFunctionID: integer;
      const aParamIndex: integer): IG2Function;
    function AddTextFunction(const aFunctionID: integer;
      const aMasterParamIndex: integer; const aDependencies: string)
      : IG2Function;

    procedure InvalidateControl; virtual;
    procedure InvalidateCables;
    procedure InvalidateConnectors;
    procedure InvalidateParameters;

    procedure WriteMessModuleParamLabelList(aStream: TStream);

    function CreateAssignableParamList: TList<IG2Param>;

    procedure AddMessModuleAdd(const aLocationIndex: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessModuleDelete(aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessModuleColor(const aModuleColor: byte;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessModuleMove(aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessModuleLabel(const aLabel: string;
      aSendMessage, aUndoMessage: TG2SendMessage);
    procedure AddMessParamLabel(const aParam: IG2Param; const aIndex: integer;
      const aLabel: string; aSendMessage, aUndoMessage: TG2SendMessage);

    function GetNextParam: IG2Param;
    function GetPrevParam: IG2Param;
    procedure SelectNextParam;
    procedure SelectPrevParam;

    property PatchPart: IG2PatchPart read GetPatchPart;
    property LocationIndex: byte read GetLocationIndex;
    property TypeID: TBits8 read GetTypeID write SetTypeID;
    property ModuleIndex: TBits8 read GetModuleIndex write SetModuleIndex;
    property Col: TBits7 read GetCol write SetCol;
    property Row: TBits7 read GetRow write SetRow;
    property ModuleColor: TBits8 read GetModuleColor write SetModuleColor;
    property ModuleName: string read GetModuleName write SetModuleName;
    property HeightUnits: integer read GetHeightUnits write SetHeightUnits;
    property InConnector[const Index: integer]: IG2Connector
      read GetInputConnector;
    property OutConnector[const Index: integer]: IG2Connector
      read GetOutputConnector;
    property InConnectorCount: integer read GetInputConnectorCount;
    property OutConnectorCount: integer read GetOutputConnectorCount;
    property Focussed: boolean read GetFocussed;
    property Selected: boolean read GetSelected;
    property SelectedParam: IG2Param read GetSelectedParam
      write SetSelectedParam;
  end;

var
  ModuleIndexCompare: TComparison<IG2Module>;

implementation

uses
  BVE.NMG2Param,
  BVE.NMG2Function,
  BVE.NMG2Connector;

constructor TModuleParamValues.Create;
begin
  inherited Create;
end;

constructor TModuleParamValues.Copy(aParamValues: TModuleParamValues);
begin
  CopyValues(aParamValues);
end;

procedure TModuleParamValues.CopyValues(aParamValues: TModuleParamValues);
var
  i: integer;
begin
  FVariation := aParamValues.FVariation;
  SetLength(FParamValueArray, Length(aParamValues.FParamValueArray));
  for i := 0 to Length(aParamValues.FParamValueArray) - 1 do
    FParamValueArray[i] := aParamValues.FParamValueArray[i];
end;

procedure TModuleParamValues.Init;
begin
  SetLength(FParamValueArray, 0);
  FVariation := 0;
end;

destructor TModuleParamValues.Destroy;
begin
  Finalize(FParamValueArray);
  inherited;
end;

function TModuleParamValues.GetParamCount: integer;
begin
  Result := Length(FParamValueArray);
end;

function TModuleParamValues.GetParamValue(aIndex: integer): TBits7;
begin
  if aIndex < Length(FParamValueArray) then
    Result := FParamValueArray[aIndex]
  else
    raise Exception.Create('Param value index ' + IntToStr(aIndex) +
      ' out of range.');
end;

procedure TModuleParamValues.SetParamCount(const Value: integer);
begin
  SetLength(FParamValueArray, Value);
end;

procedure TModuleParamValues.SetParamValue(aIndex: integer; aValue: TBits7);
begin
  if aIndex < Length(FParamValueArray) then
    FParamValueArray[aIndex] := aValue
  else
    raise Exception.Create('Param value index ' + IntToStr(aIndex) +
      ' out of range.');
end;

procedure TModuleParamValues.Read(aChunk: TPatchChunk; aParamCount: integer);
var
  i: integer;
begin
  // FVariation   : Byte;
  // FParamValueArray : array of TBits7;

  SetLength(FParamValueArray, aParamCount);
  for i := 0 to Length(FParamValueArray) - 1 do
    FParamValueArray[i] := aChunk.ReadBits(7);
end;

procedure TModuleParamValues.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  aChunk.WriteBits(FVariation, 8);
  for i := 0 to Length(FParamValueArray) - 1 do
    aChunk.WriteBits(FParamValueArray[i], 7);
end;

// ------------------------------------------------------------------------------
//
// TModuleVariationList
//
// ------------------------------------------------------------------------------

constructor TModuleVariationList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);

  Init;
end;

destructor TModuleVariationList.Destroy;
begin
  inherited;
end;

procedure TModuleVariationList.Init;
begin
  Clear;

  FParameterCount := 0;
end;

function TModuleVariationList.GetVariation(aIndex: integer): TModuleParamValues;
begin
  Result := TModuleParamValues(inherited Items[aIndex]);
end;

procedure TModuleVariationList.SetVariation(aIndex: integer;
  const aValue: TModuleParamValues);
begin
  inherited Items[aIndex] := aValue;
end;

function TModuleVariationList.AddVariation(aValue: TModuleParamValues): integer;
begin
  Result := inherited Add(aValue);
end;

procedure TModuleVariationList.AddNewVariation(aVariation: byte);
var
  ModuleParamValues: TModuleParamValues;
  i, j: integer;
begin
  i := 0;
  while (i < Count) and not(Items[i].FVariation = aVariation) do
    inc(i);

  if i >= Count then
  begin
    ModuleParamValues := TModuleParamValues.Create;
    ModuleParamValues.FVariation := aVariation;
    SetLength(ModuleParamValues.FParamValueArray, FParameterCount);
    for j := 0 to FParameterCount - 1 do
      ModuleParamValues.FParamValueArray[j] := 0;
    Add(ModuleParamValues);
  end;
end;

procedure TModuleVariationList.CopyVariation(aFromVariation,
  aToVariation: byte);
var
  i: integer;
begin
  for i := 0 to Length(Items[aFromVariation].FParamValueArray) - 1 do
    Items[aToVariation].FParamValueArray[i] := Items[aFromVariation]
      .FParamValueArray[i];
end;

function TModuleVariationList.FindVariation(aVariation: integer)
  : TModuleParamValues;
var
  i: integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FVariation <> aVariation) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TModuleVariationList.Read(aChunk: TPatchChunk;
  aVariationCount: integer);
var
  i, j: integer;
  ModuleParamValues: TModuleParamValues;
  param_str: string;
  Variation: byte;
begin
  // Set of parameter values for one module all variations
  // 0 .. 7 : variations 1 .. 8, 8 : init variation
  // FModuleIndex    : TBits8;
  // FParameterCount : TBits7;
  // FVariations     : array[ 0 .. 8] of TModuleParamValues;

  FParameterCount := aChunk.ReadBits(7);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line('Param cnt = ' + IntToStr(FParameterCount),
      LOGCMD_NUL);
    aChunk.Log.add_log_line('Var Params', LOGCMD_NUL);
  end;

  for i := 0 to aVariationCount - 1 do
  begin
    Variation := aChunk.ReadBits(8);
    j := 0;
    while (j < Count) and (Items[j].FVariation <> Variation) do
      inc(j);

    if (j < Count) then
      ModuleParamValues := Items[j]
    else
    begin
      ModuleParamValues := TModuleParamValues.Create { (self) };
      ModuleParamValues.FVariation := Variation;
      Add(ModuleParamValues);
    end;
    ModuleParamValues.Read(aChunk, FParameterCount);

    if assigned(aChunk.Log) then
    begin
      param_str := '';
      for j := 0 to Length(ModuleParamValues.FParamValueArray) - 1 do
        param_str := param_str + IntToStr(ModuleParamValues.FParamValueArray
          [j]) + ' ';

      aChunk.Log.add_log_line(Format('%3d', [ModuleParamValues.FVariation]) +
        ' ' + param_str, LOGCMD_NUL);
    end;
  end;

  i := aVariationCount;
  while (i < N_USB_VARIATIONS) do
  begin
    AddNewVariation(i);
    inc(i);
  end;

  { if assigned(FWPatchPart) then begin
    Module := FWPatchPart.FindModule( FModuleIndex);
    if assigned(Module) then begin

    // Correction parameter count!
    case Module.TypeID of
    140 : begin // Mix1-4S
    if FParameterCount = 8 then begin
    FParameterCount := 9;
    for i := 0 to Count - 1 do begin
    SetLength( Items[i].FParamValueArray, 9);
    Items[i].FParamValueArray[8] := 0;
    end;
    end;
    end;
    end;

    Module.InvalidateParameters;
    end;
    end; }
end;

procedure TModuleVariationList.Write(aChunk: TPatchChunk;
  aVariationCount: integer);
var
  i: integer;
begin
  aChunk.WriteBits(FParameterCount, 7);
  for i := 0 to aVariationCount - 1 do
    Items[i].Write(aChunk);
end;

// ------------------------------------------------------------------------------
//
// TParamLabel
//
// ------------------------------------------------------------------------------

constructor TParamLabel.Create;
begin
  inherited;
  SetLength(FNameArray, 7);
end;

destructor TParamLabel.Destroy;
begin
  Finalize(FNameArray);
  inherited;
end;

procedure TParamLabel.Init;
var
  i: integer;
begin
  SetLength(FNameArray, 7);
  for i := 0 to 6 do
    FNameArray[i] := 0;
end;

function TParamLabel.GetName: string;
var
  i: integer;
begin
  Result := '';
  i := 0;
  while (i < 7) and (FNameArray[i] <> 0) do
  begin
    Result := Result + Char(FNameArray[i]);
    inc(i);
  end;
end;

procedure TParamLabel.SetName(aValue: string);
var
  i: integer;
begin
  i := 0;
  while (i < 7) and (i < aValue.Length) do
  begin
    FNameArray[i] := byte(aValue.Chars[i]);
    inc(i);
  end;
  while (i < 7) do
  begin
    FNameArray[i] := 0;
    inc(i);
  end;
end;

procedure TParamLabel.Read(aChunk: TPatchChunk);
var
  i: integer;
begin
  for i := 0 to 6 do
    FNameArray[i] := aChunk.ReadBits(8);
end;

procedure TParamLabel.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  for i := 0 to 6 do
    aChunk.WriteBits(FNameArray[i], 8);
end;

// ------------------------------------------------------------------------------
//
// TParamLabelList
//
// ------------------------------------------------------------------------------

constructor TParamLabelList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
end;

constructor TParamLabelList.CopyCreate(AOwnsObjects: boolean;
  aParamLabelParam: TParamLabelList);
var
  i, j: integer;
  ParamLabel: TParamLabel;
begin
  inherited Create(AOwnsObjects);

  Init;
  FIsString := aParamLabelParam.FIsString;
  FParamLen := aParamLabelParam.FParamLen;
  FParamIndex := aParamLabelParam.FParamIndex;

  for i := 0 to aParamLabelParam.Count - 1 do
  begin
    ParamLabel := TParamLabel.Create;
    for j := 0 to 6 do
      ParamLabel.FNameArray[j] := aParamLabelParam.Items[i].FNameArray[j];
    Add(ParamLabel);
  end;
end;

destructor TParamLabelList.Destroy;
begin
  inherited;
end;

procedure TParamLabelList.Init;
begin
  FIsString := 0;
  FParamLen := 0;
  FParamIndex := 0;
  Clear;
end;

procedure TParamLabelList.Read(aChunk: TPatchChunk);
begin
  // FIsString   : TBits8; 0 Extra parameter value, 1 : Label
  // FParamLen   : TBits8; if label then label length + parameterindex length else 1
  // FParamIndex : TBits8; if label then parameterindex else extra parameter value
  // FParamNames : array of TParamName;
end;

procedure TParamLabelList.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  if FIsString = 1 then
    FParamLen := 1 + Count * 7;

  aChunk.WriteBits(FIsString, 8);
  aChunk.WriteBits(FParamLen, 8);
  aChunk.WriteBits(FParamIndex, 8);

  if FIsString = 1 then
    for i := 0 to Count - 1 do
      Items[i].Write(aChunk);
end;

// ------------------------------------------------------------------------------
//
// TModuleParamLabelList
//
// ------------------------------------------------------------------------------

constructor TModuleParamLabelList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
end;

procedure TModuleParamLabelList.AddParamLabels(aParamIndex: byte;
  aNames: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := aNames;
    for i := 0 to sl.Count - 1 do
      AddParamLabel(aParamIndex, i, sl[i]);
  finally
    sl.Free;
  end;
end;

constructor TModuleParamLabelList.CopyCreate(AOwnsObjects: boolean;
  aParamLabelModule: TModuleParamLabelList);
var
  i: integer;
begin
  inherited Create(AOwnsObjects);

  Init;
  FModuleLen := aParamLabelModule.FModuleLen;
  for i := 0 to aParamLabelModule.Count - 1 do
  begin
    Add(TParamLabelList.CopyCreate(True, aParamLabelModule.Items[i]));
  end;
end;

destructor TModuleParamLabelList.Destroy;
begin
  inherited;
end;

function TModuleParamLabelList.GetParameterLabelCount
  (aParamIndex: byte): integer;
begin
  Result := Items[aParamIndex].Count;
end;

procedure TModuleParamLabelList.Init;
begin
  FModuleLen := 0;
  Clear;
end;

procedure TModuleParamLabelList.InitModuleParamLabelList(aModuleIndex: byte;
  aModuleType: integer);
var
  i, j, pi, ParamCount: integer;
  sl: TStringList;
begin
  ParamCount := GetDataParamCount(aModuleType);
  sl := TStringList.Create;
  try
    for i := 0 to ParamCount - 1 do
    begin
      pi := GetDataModuleParamIndex(aModuleIndex, i);

      if ModuleParams[pi].slParamLabel <> '' then
      begin
        sl.Clear;
        sl.Delimiter := ';';
        sl.StrictDelimiter := True;
        sl.DelimitedText := ModuleParams[pi].slParamLabel;
        for j := 0 to sl.Count - 1 do
          AddParamLabel(i, j, sl[j]);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TModuleParamLabelList.GetLabels(aLines: TStringList);
var
  i, j: integer;
  sl: TStringList;
  ParamLabelList: TParamLabelList;
begin
  // One line for every parameter in the order of parameter index
  // If a parameter has multiple labels, then the line is a ; delimited string

  sl := TStringList.Create;
  try
    aLines.Clear;
    sl.Delimiter := ';';
    for i := 0 to Count - 1 do
    begin
      ParamLabelList := Items[i];
      sl.Clear;
      for j := 0 to ParamLabelList.Count - 1 do
      begin
        sl.Add(ParamLabelList.Items[j].GetName)
      end;
      aLines.Add(sl.DelimitedText);
    end;
  finally
    sl.Free;
  end;
end;

procedure TModuleParamLabelList.SetLabels(aLines: TStringList);
var
  i, j: integer;
  sl: TStringList;
  ParamLabelList: TParamLabelList;
  ParamLabel: TParamLabel;
begin
  // One line for every parameter in the order of parameter index
  // If a parameter has multiple labels, then the line is a ; delimited string

  sl := TStringList.Create;
  try
    aLines.Clear;
    sl.Delimiter := ';';
    for i := 0 to aLines.Count - 1 do
    begin
      sl.DelimitedText := aLines[i];
      for j := 0 to sl.Count - 1 do
      begin
        ParamLabelList := FindParamLabel(i);
        if not assigned(ParamLabelList) then
        begin
          ParamLabelList := TParamLabelList.Create(True);
          ParamLabelList.FIsString := 1;
          ParamLabelList.FParamLen := 8;
          ParamLabelList.FParamIndex := i;
          Add(ParamLabelList);
        end;

        if (j < ParamLabelList.Count) then
        begin
          ParamLabelList.Items[j].SetName(sl[j]);
        end
        else
        begin
          ParamLabel := TParamLabel.Create;
          ParamLabel.SetName(sl[j]);
          ParamLabelList.Add(ParamLabel);
          FModuleLen := GetModuleLabelsLength;
        end;
      end;
    end;
  finally
    sl.Free;
  end;
end;

function TModuleParamLabelList.FindParamLabel(aParamIndex: integer)
  : TParamLabelList;
var
  i: integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FIsString = 1) and
    (Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TModuleParamLabelList.GetModuleLabelsLength: integer;
var
  i: integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do
  begin
    if Items[i].FIsString = 1 then
    begin
      Result := Result + 3 + 7 * Items[i].Count;
    end
    else
      Result := Result + 3;
  end;
end;

procedure TModuleParamLabelList.AddParamLabel(aParamIndex, aLabelIndex: byte;
  aName: string);
var
  ParamLabelList: TParamLabelList;
  ParamLabel: TParamLabel;
begin
  ParamLabelList := FindParamLabel(aParamIndex);
  if not assigned(ParamLabelList) then
  begin
    ParamLabelList := TParamLabelList.Create(True);
    ParamLabelList.FIsString := 1;
    ParamLabelList.FParamLen := 8;
    ParamLabelList.FParamIndex := aParamIndex;
    Add(ParamLabelList);
  end;

  if (aLabelIndex < ParamLabelList.Count) then
  begin
    ParamLabelList.Items[aLabelIndex].SetName(aName);
  end
  else
  begin
    ParamLabel := TParamLabel.Create;
    ParamLabel.SetName(aName);
    ParamLabelList.Add(ParamLabel);
    FModuleLen := GetModuleLabelsLength;
  end;
end;

procedure TModuleParamLabelList.Read(aChunk: TPatchChunk);
var
  i, j: integer;
  ParamLabelList: TParamLabelList;
  ParamLabel: TParamLabel;
  parlabels_str: string;
begin
  // FModuleIndex : TBits8;
  // FModuleLen   : byte;
  // FParams      : array of TParamLabelParamList;

  // FIsString   : TBits8; 0 Extra parameter value, 1 : Label
  // FParamLen   : TBits8; if label then label length + parameterindex length else 1
  // FParamIndex : TBits8; if label then parameterindex else extra parameter value
  // FParamNames : array of TParamName;

  // This section is also used to store some extra params for SeqNote
  // extra editor parameters
  // [0, 1, mag, 0, 1, octave]
  // mag   : 0=3-octaves,1=2-octaves,2=1-octave
  // octave: 0-9 (c0-c9)

  // FModuleIndex := aChunk.ReadBits( 8);
  FModuleLen := aChunk.ReadBits(8);

  if assigned(aChunk.Log) then
  begin
    aChunk.Log.add_log_line(Format('3d', [FModuleLen]), LOGCMD_NUL);
    aChunk.Log.add_log_line('    Str Len Par Labels', LOGCMD_NUL);
  end;

  // Parameter labels are updated per Module, so first the list of parameter
  // labes of current module will be cleared
  Clear;

  i := FModuleLen;
  while (i > 0) do
  begin
    ParamLabelList := TParamLabelList.Create(True);
    ParamLabelList.FIsString := aChunk.ReadBits(8);
    ParamLabelList.FParamLen := aChunk.ReadBits(8);
    ParamLabelList.FParamIndex := aChunk.ReadBits(8);

    if assigned(aChunk.Log) then
    begin
      parlabels_str := Format('    %3d %3d %3d', [ParamLabelList.FIsString,
        ParamLabelList.FParamLen, ParamLabelList.FParamIndex]);
    end;

    Add(ParamLabelList);

    i := i - 3; // bytes

    if ParamLabelList.FParamLen - 1 > 0 then
    begin // It's a string
      for j := 0 to ((ParamLabelList.FParamLen - 1) div 7) - 1 do
      begin
        ParamLabel := TParamLabel.Create;
        ParamLabel.Read(aChunk);
        ParamLabelList.Add(ParamLabel);

        i := i - 7; // bytes
      end;
      if assigned(aChunk.Log) then
      begin
        for j := 0 to ParamLabelList.Count - 1 do
          parlabels_str := parlabels_str + ' ' + ParamLabelList.Items
            [j].GetName;
      end;
    end;
    if assigned(aChunk.Log) then
      aChunk.Log.add_log_line(parlabels_str, LOGCMD_NUL);
  end;
end;

procedure TModuleParamLabelList.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  FModuleLen := GetModuleLabelsLength;

  aChunk.WriteBits(FModuleLen, 8);

  for i := 0 to Count - 1 do
    Items[i].Write(aChunk);
end;

// ------------------------------------------------------------------------------
//
// TG2FileModule
//
// ------------------------------------------------------------------------------

constructor TG2FileModule.Create(aPatchPart: IG2PatchPart;
  const aModuleType: byte; const aModuleIndex: integer);
begin
  inherited Create;

  SetWeak(@FWPatchPart, aPatchPart);

  FTypeID := aModuleType;
  FModuleIndex := aModuleIndex;

  FSelected := False;

  FModuleParamList := TModuleVariationList.Create(True);
  FModuleParamLabelList := TModuleParamLabelList.Create(True);

  SetLength(FModeInfoArray, 0);
  FSelectedParamIndex := -1;

  FFunctionList := TList<IG2Function>.Create;
end;

destructor TG2FileModule.Destroy;
var
  i: integer;
begin
  if FFunctionList.Count > 0 then
  begin
    for i := 0 to FFunctionList.Count - 1 do
      FFunctionList[i].NotifyDestroy;
  end;
  FreeAndNil(FFunctionList);

  for i := 0 to High(FModeArray) do
    FModeArray[i].NotifyDestroy;
  SetLength(FModeArray, 0);
  Finalize(FModeArray);

  for i := 0 to High(FParamArray) do
    FParamArray[i].NotifyDestroy;
  SetLength(FParamArray, 0);
  Finalize(FParamArray);

  for i := 0 to High(FOutConnArray) do
    FOutConnArray[i].NotifyDestroy;
  SetLength(FOutConnArray, 0);
  Finalize(FOutConnArray);

  for i := 0 to High(FInConnArray) do
    FInConnArray[i].NotifyDestroy;
  SetLength(FInConnArray, 0);
  Finalize(FInConnArray);

  Finalize(FModeInfoArray);

  FreeAndNil(FModuleParamLabelList);
  FreeAndNil(FModuleParamList);

  SetWeak(@FWPatchPart, nil);

  //FObserverList.Clear;

  inherited;
end;

procedure TG2FileModule.Init;
begin
  FTypeID := 0;
  FModuleIndex := 0;
  FRow := 0;
  FCol := 0;
  FModuleColor := 0;
  FUnknown1 := 0;
  FModeCount := 0;
  SetLength(FModeInfoArray, 0);

  FModuleName := '';
  FModuleFileName := '';

  FNewRow := 0;
  FNewCol := 0;
  FNewModuleIndex := 0;
  FNewModuleName := '';
end;

procedure TG2FileModule.InitModule;
var
  i, mi, pi, Count: integer;
begin
  mi := GetDataModuleIndex(FTypeID);

  FUprate := ModuleDefs[mi].Uprate;
  FIsLed := ModuleDefs[mi].IsLed;
  FHeightUnits := ModuleDefs[mi].Height;
  FModuleName := ModuleDefs[mi].ModuleName;
  FModuleFileName := ModuleDefs[mi].ModuleName;
  FModeCount := 0;

  FNewCol := FCol;
  FNewRow := FRow;
  FNewModuleIndex := FModuleIndex;
  FNewModuleName := FModuleName;

  FPage := ModuleDefs[mi].Page;
  FPageIndex := ModuleDefs[mi].PageIndex;

  Count := GetDataInputCount(FTypeID);
  SetLength(FInConnArray, Count);
  for i := 0 to Count - 1 do
  begin
    FInConnArray[i] := TG2FileConnector.Create(self as IG2Module);
    FInConnArray[i].InitConnector(FTypeID, i, ckInput);
  end;

  Count := GetDataOutputCount(FTypeID);
  SetLength(FOutConnArray, Count);
  for i := 0 to Count - 1 do
  begin
    FOutConnArray[i] := TG2FileConnector.Create(self as IG2Module);
    FOutConnArray[i].InitConnector(FTypeID, i, ckOutput);
  end;

  Count := GetDataParamCount(FTypeID);
  SetLength(FParamArray, Count);
  for i := 0 to Count - 1 do
  begin
    pi := GetDataModuleParamIndex(FTypeID, i);

    FParamArray[i] := TG2Param.Create(self as IG2Module, i, ptParam,
      ModuleParams[pi].ParamID, ModuleParams[pi].DefaultValue,
      ModuleParams[pi].DefaultKnob, ModuleParams[pi].ButtonParam,
      ModuleDefs[mi].ModuleName, ModuleParams[pi].ParamName,
      ModuleParams[pi].slParamLabel);
  end;
  if Count > 0 then
    FSelectedParamIndex := 0;

  Count := GetDataModeCount(FTypeID);
  FModeCount := Count;
  SetLength(FModeInfoArray, Count);
  SetLength(FModeArray, Count);
  for i := 0 to Count - 1 do
  begin
    pi := GetDataModuleModeIndex(FTypeID, i);

    FModeArray[i] := TG2Param.Create(self as IG2Module, i, ptMode,
      ModuleModes[pi].ParamID, ModuleModes[pi].DefaultValue, -1, -1,
      ModuleDefs[mi].ModuleName, ModuleModes[pi].ParamName,
      ModuleModes[pi].slParamLabel);
    FModeInfoArray[i] := ModuleModes[pi].DefaultValue;
  end;
end;

procedure TG2FileModule.InvalidateCables;
var
  i: integer;
begin
  for i := 0 to Length(FInConnArray) - 1 do
    FInConnArray[i].InvalidateCables;

  for i := 0 to Length(FOutConnArray) - 1 do
    FOutConnArray[i].InvalidateCables;
end;

procedure TG2FileModule.InvalidateConnectors;
var
  i: integer;
begin
  for i := 0 to Length(FInConnArray) - 1 do
    FInConnArray[i].InvalidateControl;

  for i := 0 to Length(FOutConnArray) - 1 do
    FOutConnArray[i].InvalidateControl;
end;

procedure TG2FileModule.InvalidateControl;
begin
  NotifyObservers(EvtInvalidate, self as IG2Module);
end;

procedure TG2FileModule.InvalidateParameters;
var
  i: integer;
begin
  for i := 0 to Length(FModeArray) - 1 do
    FModeArray[i].InvalidateControl;

  for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].InvalidateControl;
end;

{function TG2FileModule.GetInfoFunction(const aIndex: integer): IG2Function;
begin
  Result := FFunctionList[aIndex];
end;}

function TG2FileModule.GetInputConnector(const ConnectorIndex: integer)
  : IG2Connector;
var
  i: integer;
begin
  i := 0;
  while (i < Length(FInConnArray)) and
    (ConnectorIndex <> FInConnArray[i].ConnectorIndex) do
    inc(i);

  if (i < Length(FInConnArray)) then
    Result := FInConnArray[i]
  else
    raise Exception.Create('Input connector with index ' +
      IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetInputConnectorCount: integer;
begin
  Result := Length(FInConnArray);
end;

function TG2FileModule.GetIsLed: TBits1;
begin
  Result := FIsLed;
end;

function TG2FileModule.GetLocationIndex: byte;
begin
  Result := FWPatchPart.LocationIndex;
end;

function TG2FileModule.GetOutputConnector(const ConnectorIndex: integer)
  : IG2Connector;
var
  i: integer;
begin
  i := 0;
  while (i < Length(FOutConnArray)) and
    (ConnectorIndex <> FOutConnArray[i].ConnectorIndex) do
    inc(i);

  if (i < Length(FOutConnArray)) then
    Result := FOutConnArray[i]
  else
    raise Exception.Create('Output connector with index ' +
      IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetOutputConnectorCount: integer;
begin
  Result := Length(FOutConnArray);
end;

function TG2FileModule.GetMode(const ModeIndex: integer): IG2Param;
var
  i: integer;
begin
  i := 0;
  while (i < Length(FModeArray)) and (ModeIndex <> FModeArray[i].ParamIndex) do
    inc(i);

  if (i < Length(FModeArray)) then
    Result := FModeArray[i]
  else
    Result := nil;
end;

function TG2FileModule.GetPage: TModulePage;
begin
  Result := FPage;
end;

function TG2FileModule.GetPageIndex: integer;
begin
  Result := FPageIndex;
end;

function TG2FileModule.GetParam(const ParamIndex: integer): IG2Param;
var
  i: integer;
begin
  i := 0;
  while (i < Length(FParamArray)) and
    (ParamIndex <> FParamArray[i].ParamIndex) do
    inc(i);

  if (i < Length(FParamArray)) then
    Result := FParamArray[i]
  else
    Result := nil;
end;

function TG2FileModule.GetModeCount: integer;
begin
  Result := Length(FModeInfoArray);
end;

function TG2FileModule.GetParameterCount: integer;
begin
  Result := Length(FParamArray);
end;

function TG2FileModule.GetParamLabel(const aParamIndex,
  aLabelIndex: byte): string;
var
  ParamLabelList: TParamLabelList;
begin
  ParamLabelList := FModuleParamLabelList.FindParamLabel(aParamIndex);
  if assigned(ParamLabelList) and (ParamLabelList.FIsString = 1) then
  begin
    if aLabelIndex < ParamLabelList.Count then
      Result := ParamLabelList.Items[aLabelIndex].GetName
    else
      Result := ParamLabelList.Items[0].GetName;
  end
  else
    Result := '';
end;

function TG2FileModule.GetParamLabelCount: integer;
begin
  Result := FModuleParamLabelList.Count;
end;

function TG2FileModule.GetParamValue(const aVariation,
  aParamIndex: byte): integer;
var
  ParamValues: TModuleParamValues;
begin
  ParamValues := FModuleParamList.FindVariation(aVariation);
  if assigned(ParamValues) then
    Result := ParamValues.ParamValues[aParamIndex]
  else
    Result := 0;
end;

function TG2FileModule.GetPatchPart: IG2PatchPart;
begin
  Result := FWPatchPart;
end;

procedure TG2FileModule.SetModeInfo(const aModeIndex: integer;
  const aValue: TBits6);
begin
  if aModeIndex < Length(FModeInfoArray) then
    FModeInfoArray[aModeIndex] := aValue
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) +
      ' out of range.');
end;

function TG2FileModule.GetModeInfo(const aModeIndex: integer): TBits6;
begin
  if aModeIndex < Length(FModeInfoArray) then
    Result := FModeInfoArray[aModeIndex]
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) +
      ' out of range.');
end;

function TG2FileModule.GetModeInfoArray: TModeInfoArray;
begin
  Result := FModeInfoArray;
end;

function TG2FileModule.GetModuleColor: TBits8;
begin
  Result := FModuleColor;
end;

function TG2FileModule.GetModuleFileName: string;
begin
  Result := FModuleFileName;
end;

function TG2FileModule.GetModuleIndex: TBits8;
begin
  Result := FModuleIndex;
end;

function TG2FileModule.GetModuleName: string;
begin
  Result := FModuleName;
end;

procedure TG2FileModule.SetNewCol(const aValue: byte);
begin
  FNewCol := aValue;
end;

procedure TG2FileModule.SetNewModuleIndex(const aValue: byte);
begin
  FNewModuleIndex := aValue;
end;

procedure TG2FileModule.SetNewModuleName(const aValue: string);
begin
  FNewModuleName := aValue;
end;

function TG2FileModule.GetNewCol: byte;
begin
  Result := FNewCol;
end;

function TG2FileModule.GetNewModuleIndex: byte;
begin
  Result := FNewModuleIndex;
end;

function TG2FileModule.GetNewModuleName: string;
begin
  Result := FNewModuleName;
end;

procedure TG2FileModule.SetNewRow(const aValue: byte);
begin
  FNewRow := aValue;
end;

procedure TG2FileModule.SetNewUprate(const aValue: TBits1);
begin
  FNewUprate := aValue;
end;

procedure TG2FileModule.SetParamLabel(const aParamIndex, aLabelIndex: byte;
  const aName: string);
begin
  FModuleParamLabelList.AddParamLabel(aParamIndex, aLabelIndex, aName);
end;

procedure TG2FileModule.SetParamLabels(const aParamIndex: byte;
  const aNames: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := aNames;
    for i := 0 to sl.Count - 1 do
      FModuleParamLabelList.AddParamLabel(aParamIndex, i, sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TG2FileModule.SetParamValue(const aVariation, aParamIndex,
  aValue: byte);
var
  ParamValues: TModuleParamValues;
begin
  ParamValues := FModuleParamList.FindVariation(aVariation);
  if assigned(ParamValues) then
  begin
    ParamValues.FParamValueArray[aParamIndex] := aValue;
  end;
end;

function TG2FileModule.GetNewRow: byte;
begin
  Result := FNewRow;
end;

function TG2FileModule.GetNewUprate: TBits1;
begin
  Result := FNewUprate;
end;

function TG2FileModule.GetRow: TBits7;
begin
  Result := FRow;
end;

function TG2FileModule.GetRowIndex: integer;
begin
  if assigned(PatchPart) then
    Result := PatchPart.GetModuleRow(self)
  else
    Result := 0;
end;

function TG2FileModule.GetSelectedParam: IG2Param;
begin
  if FSelectedParamIndex = -1 then
    Result := nil
  else
    Result := FParamArray[FSelectedParamIndex];
end;

function TG2FileModule.GetSlotIndex: byte;
begin
  Result := FWPatchPart.SlotIndex;
end;

function TG2FileModule.GetTypeID: TBits8;
begin
  Result := FTypeID;
end;

function TG2FileModule.GetUnknown1: TBits6;
begin
  Result := FUnknown1;
end;

function TG2FileModule.GetUprate: TBits1;
begin
  Result := FUprate;
end;

procedure TG2FileModule.SetSelected(const aValue: boolean);
begin
  FSelected := aValue;
  InvalidateControl;
end;

procedure TG2FileModule.SetSelectedParam(const aValue: IG2Param);
var
  NewValue: integer;
begin
  if assigned(aValue) and (aValue.Module = self as IG2Module) then
    NewValue := aValue.ParamIndex
  else
    NewValue := -1;

  if NewValue <> FSelectedParamIndex then
  begin
    FSelectedParamIndex := NewValue;
    InvalidateControl;
    InvalidateParameters;
  end;
end;

procedure TG2FileModule.SetTypeID(const aValue: TBits8);
begin
  FTypeID := aValue;
end;

function TG2FileModule.GetNextParam: IG2Param;
begin
  Result := nil;
  if Length(FParamArray) = 0 then
    exit;

  inc(FSelectedParamIndex);
  if FSelectedParamIndex >= Length(FParamArray) then
    FSelectedParamIndex := 0;

  Result := FParamArray[FSelectedParamIndex];
end;

function TG2FileModule.GetPrevParam: IG2Param;
begin
  Result := nil;
  if Length(FParamArray) = 0 then
    exit;

  dec(FSelectedParamIndex);
  if FSelectedParamIndex < 0 then
    FSelectedParamIndex := Length(FParamArray) - 1;

  Result := FParamArray[FSelectedParamIndex];
end;

procedure TG2FileModule.SelectNextParam;
var
  Param: IG2Param;
begin
  if FSelectedParamIndex <> -1 then
    FParamArray[FSelectedParamIndex].Selected := False;
  Param := GetNextParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParamIndex := -1;
end;

procedure TG2FileModule.SelectPrevParam;
var
  Param: IG2Param;
begin
  if FSelectedParamIndex <> -1 then
    FParamArray[FSelectedParamIndex].Selected := False;
  Param := GetPrevParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParamIndex := -1;
end;

procedure TG2FileModule.AddParameter(aParameter: IG2Param);
begin
  if Length(FParamArray) < aParameter.ParamIndex + 1 then
    SetLength(FParamArray, aParameter.ParamIndex + 1);
  FParamArray[aParameter.ParamIndex] := aParameter;
end;

function TG2FileModule.AddTextFunction(const aFunctionID: integer;
  const aMasterParamIndex: integer; const aDependencies: string): IG2Function;
begin
  Result := nil;
  if aFunctionID < 0 then
    exit;

  {if FFunctionList.ContainsKey(aFunctionID) then
    Result := FFunctionList[aFunctionID]
  else
  begin
    Result := TG2Function.Create(aFunctionID, self, aMasterParamIndex,
      aDependencies);
    FFunctionList.Add(aFunctionID, Result);
  end;}
  Result := TG2Function.Create(aFunctionID, self, aMasterParamIndex,
    aDependencies);
  FFunctionList.Add(Result);
end;

function TG2FileModule.AddInfoFunction(const aFunctionID: integer;
  const aParamIndex: integer): IG2Function;
begin
  Result := nil;
  if aFunctionID < 0 then
    exit;

  {if FFunctionList.ContainsKey(aFunctionID) then
    Result := FFunctionList[aFunctionID]
  else
  begin
    Result := TG2Function.Create(aFunctionID, self, aParamIndex);
    FFunctionList.Add(aFunctionID, Result);
  end;}
  Result := TG2Function.Create(aFunctionID, self, aParamIndex);
  FFunctionList.Add(Result);
end;

procedure TG2FileModule.AddMessModuleAdd(const aLocationIndex: byte;
  aSendMessage, aUndoMessage: TG2SendMessage);
var
  ModeArray: array of byte;
  i: integer;
begin
  SetLength(ModeArray, Length(FModeArray));
  for i := 0 to Length(FModeArray) - 1 do
    ModeArray[i] := FModeArray[i].GetValue;

  aSendMessage.AddMessModuleAdd(aLocationIndex, FTypeID, FNewModuleIndex,
    FNewCol, FNewRow, FUprate, FIsLed, ModeArray, FNewModuleName);
  aUndoMessage.AddMessModuleDelete(aLocationIndex, FModuleIndex);
end;

procedure TG2FileModule.AddMessModuleColor(const aModuleColor: byte;
  aSendMessage, aUndoMessage: TG2SendMessage);
begin
  aUndoMessage.AddMessModuleColor(LocationIndex, FModuleIndex, FModuleColor);
  aSendMessage.AddMessModuleColor(LocationIndex, FModuleIndex, aModuleColor);
end;

procedure TG2FileModule.AddMessModuleDelete(aSendMessage,
  aUndoMessage: TG2SendMessage);
var
  ModeArray: array of byte;
  Param: IG2Param;
  Morph: IG2ParamMorph;
  i, j, k, Count: integer;
  SubMsg: TG2Message;
  UndoChunk: TPatchChunk;
begin
  // Delete and undo delete controllers, knobs, global knobs, morphs associated with any parameter of the module

  for i := 0 to Length(FParamArray) - 1 do
  begin
    Param := FParamArray[i];

    if assigned(Param.Controller) then
    begin
      Param.AddMessDeassignController(aSendMessage, aUndoMessage);
    end;

    if assigned(Param.Knob) then
    begin
      Param.AddMessDeassignKnob(aSendMessage, aUndoMessage);
    end;

    if assigned(Param.GlobalKnob) then
    begin
      Param.AddMessDeassignGlobalKnob(aSendMessage, aUndoMessage);
    end;

    for j := 0 to NMORPHS - 1 do
    begin
      for k := 0 to N_VARIATIONS - 1 do
      begin // TODO: Maybe N_USB_VARIATIONS?
        Morph := Param.GetMorph(j, k);
        if assigned(Morph) then
        begin
          Param.AddMessClearMorph(Morph, k, aSendMessage, aUndoMessage);
        end;
      end;
    end;
  end;

  SubMsg := TG2Message.Create;
  UndoChunk := TPatchChunk.Create(SubMsg);
  try
    // Undo Delete Parameters

    // Write parameterlist chunk on UndoMessage
    UndoChunk.WriteBits(LocationIndex, 2);
    Count := Length(FParamArray);
    if Count > 0 then
    begin
      UndoChunk.WriteBits(1, 8); // SetCount
      UndoChunk.WriteBits(N_USB_VARIATIONS, 8); // VariationCount, must be 10!
      WriteParamList(UndoChunk, N_USB_VARIATIONS);
    end
    else
    begin
      // No parameters
      UndoChunk.WriteBits(0, 8); // SetCount
      UndoChunk.WriteBits(N_USB_VARIATIONS, 8); // VariationCount, must be 10!
    end;
    UndoChunk.WriteChunk(C_PARAM_LIST);

    aUndoMessage.Add(SubMsg);

    SubMsg.Clear;
    UndoChunk.Init;

    // Write paramnames chunk on UndoMessage
    UndoChunk.WriteBits(S_SET_PARAM_LABEL, 8);
    UndoChunk.WriteBits(LocationIndex, 8);
    WriteParamLabelList(UndoChunk);
    UndoChunk.Flush;
    aUndoMessage.Add(SubMsg);

    SubMsg.Clear;
    UndoChunk.Init;

    UndoChunk.WriteBits(S_SET_MODULE_LABEL, 8);
    UndoChunk.WriteBits(LocationIndex, 8);
    UndoChunk.WriteBits(FModuleIndex, 8); // NameCount
    UndoChunk.WriteName(FModuleName);
    UndoChunk.Flush;
    aUndoMessage.Add(SubMsg);

  finally
    UndoChunk.Free;
    SubMsg.Free;
  end;

  // Delete module message and undo
  SetLength(ModeArray, Length(FModeArray));
  for i := 0 to Length(FModeArray) - 1 do
    ModeArray[i] := FModeArray[i].GetValue;

  aSendMessage.AddMessModuleDelete(LocationIndex, FModuleIndex);
  aUndoMessage.AddMessModuleAdd(LocationIndex, FTypeID, FModuleIndex, FCol,
    FRow, FUprate, FIsLed, ModeArray, FModuleName);
end;

procedure TG2FileModule.AddMessModuleLabel(const aLabel: string;
  aSendMessage, aUndoMessage: TG2SendMessage);
begin
  aUndoMessage.AddMessModuleLabel(LocationIndex, FModuleIndex, FModuleName);
  aSendMessage.AddMessModuleLabel(LocationIndex, FModuleIndex, aLabel);
end;

procedure TG2FileModule.AddMessModuleMove(aSendMessage,
  aUndoMessage: TG2SendMessage);
begin
  if (FNewCol = FCol) and (FNewRow = FRow) then
    exit;

  aSendMessage.AddMessModuleMove(LocationIndex, FNewModuleIndex,
    FNewCol, FNewRow);
  aUndoMessage.AddMessModuleMove(LocationIndex, FModuleIndex, FCol, FRow);
end;

procedure TG2FileModule.AddMessParamLabel(const aParam: IG2Param;
  const aIndex: integer; const aLabel: string;
  aSendMessage, aUndoMessage: TG2SendMessage);
var
  NewModuleParamLabelList: TModuleParamLabelList;
  SubMess: TG2Message;
  Chunk: TPatchChunk;
begin
  SubMess := TG2Message.Create;
  Chunk := TPatchChunk.Create(SubMess);
  try
    Chunk.WriteBits(S_SET_PARAM_LABEL, 8);
    Chunk.WriteBits(LocationIndex, 8);
    WriteParamLabelList(Chunk);
    // FModuleParamLabelList.Write( Chunk);
    // Chunk.WriteChunk( S_SET_PARAM_LABEL);
    Chunk.Flush;
    aUndoMessage.Add(SubMess);

    Chunk.Init;
    SubMess.Clear;

    NewModuleParamLabelList := TModuleParamLabelList.CopyCreate(True,
      FModuleParamLabelList);
    try
      NewModuleParamLabelList.AddParamLabel(aParam.ParamIndex, aIndex, aLabel);

      Chunk.WriteBits(S_SET_PARAM_LABEL, 8);
      Chunk.WriteBits(LocationIndex, 8);
      Chunk.WriteBits(FModuleIndex, 8);
      NewModuleParamLabelList.Write(Chunk);
      Chunk.Flush;
      aSendMessage.Add(SubMess);
    finally
      NewModuleParamLabelList.Free;
    end;
  finally
    Chunk.Free;
    SubMess.Free;
  end;
end;

procedure TG2FileModule.WriteMessModuleParamLabelList(aStream: TStream);
var
  Chunk: TPatchChunk;
begin
  Chunk := TPatchChunk.Create(aStream);
  try
    Chunk.WriteBits(S_SET_PARAM_LABEL, 8);
    Chunk.WriteBits(LocationIndex, 8);
    WriteParamLabelList(Chunk);
    Chunk.WriteChunk(S_SET_PARAM_LABEL);
  finally
    Chunk.Free;
  end;
end;

class procedure TG2FileModule.WriteMessNewModule(const aLocationIndex: byte;
  const aModuleIndex, aModuleType, aRow, aCol: byte; const aModuleName: string;
  aSendMessage: TG2SendMessage);
var
  MemStream: TG2Message;
  Chunk: TPatchChunk;
  Count, i, j, mi, pi: integer;
  ModuleParamLabelList: TModuleParamLabelList;
begin
  // Get the module data
  mi := GetDataModuleIndex(aModuleType);

  MemStream := TG2Message.Create;
  Chunk := TPatchChunk.Create(MemStream);
  try
    Chunk.WriteBits(S_ADD_MODULE, 8);
    Chunk.WriteBits(aModuleType, 8);
    Chunk.WriteBits(aLocationIndex, 8);
    Chunk.WriteBits(aModuleIndex, 8);
    Chunk.WriteBits(aCol, 8);
    Chunk.WriteBits(aRow, 8);
    Chunk.WriteBits(0, 8);
    Chunk.WriteBits(ModuleDefs[mi].Uprate, 8);
    Chunk.WriteBits(ModuleDefs[mi].IsLed, 8);

    Count := GetDataModeCount(aModuleType);
    for i := 0 to Count - 1 do
    begin
      pi := GetDataModuleModeIndex(aModuleType, i);
      Chunk.WriteBits(ModuleModes[pi].DefaultValue, 8);
    end;

    Chunk.WriteName(aModuleName);
    Chunk.Flush;

    // Write empty cablechunk
    Chunk.WriteBits(aLocationIndex, 2);
    Chunk.WriteBits(0, 12); // Unknown
    Chunk.WriteBits(0, 10); // CableCount
    Chunk.WriteChunk($52);

    // Write parameterlist chunk
    Chunk.WriteBits(aLocationIndex, 2);

    Count := GetDataParamCount(aModuleType);
    if Count > 0 then
    begin
      Chunk.WriteBits(1, 8); // SetCount
      Chunk.WriteBits(N_USB_VARIATIONS, 8); // VariationCount, must be 10!
      Chunk.WriteBits(aModuleIndex, 8); // ModuleIndex
      Chunk.WriteBits(Count, 7); // ParamCount
      for i := 0 to N_USB_VARIATIONS - 1 do
      begin // Variations
        Chunk.WriteBits(i, 8);

        for j := 0 to Count - 1 do
        begin
          pi := GetDataModuleParamIndex(aModuleType, j);
          Chunk.WriteBits(ModuleParams[pi].DefaultValue, 7);
        end;
      end;
    end
    else
    begin
      // No parameters
      Chunk.WriteBits(0, 8); // SetCount
      Chunk.WriteBits(N_USB_VARIATIONS, 8); // VariationCount, must be 10!
    end;

    Chunk.WriteChunk($4D);

    // Write paramnames chunk

    if aModuleType = 121 then
    begin
      // In case of SeqNote, 2 parameters are put in the paramnames chunk:
      // [0, 1, mag, 0, 1, octave]
      // mag: 0=3-octaves,1=2-octaves,2=1-octave
      // octave: 0-9 (c0-c9)

      Chunk.WriteBits(aLocationIndex, 2);
      Chunk.WriteBits(1, 8); // ModuleCount
      Chunk.WriteBits(aModuleIndex, 8); // ModuleIndex
      Chunk.WriteBits(6, 8); // ModLen

      Chunk.WriteBits(0, 8); // Magnification
      Chunk.WriteBits(1, 8);
      Chunk.WriteBits(1, 8); // 2 - octaves

      Chunk.WriteBits(0, 8); // Octave
      Chunk.WriteBits(1, 8);
      Chunk.WriteBits(5, 8); // c5

      Chunk.WriteChunk($5B);
    end
    else
    begin
      Chunk.WriteBits(aLocationIndex, 2);

      Count := GetDataParamCount(aModuleType);
      if Count > 0 then
      begin
        Chunk.WriteBits(1, 8); // ModuleCount
        Chunk.WriteBits(aModuleIndex, 8); // ModuleIndex

        ModuleParamLabelList := TModuleParamLabelList.Create(True);
        try
          for i := 0 to Count - 1 do
          begin
            pi := GetDataModuleParamIndex(aModuleType, i);
            if ModuleParams[pi].slParamLabel <> '' then
            begin
              ModuleParamLabelList.AddParamLabels(i,
                ModuleParams[pi].slParamLabel);
            end;
          end;
          ModuleParamLabelList.Write(Chunk);
        finally
          ModuleParamLabelList.Free;
        end;
      end
      else
        Chunk.WriteBits(0, 8); // ModuleCount

      Chunk.WriteChunk($5B);
    end;

    // Write modulenames chunk
    Chunk.WriteBits(aLocationIndex, 2);
    Chunk.WriteBits(0, 6); // Unknown
    Chunk.WriteBits(1, 8); // NameCount

    Chunk.WriteBits(aModuleIndex, 8);
    Chunk.WriteName(aModuleName);
    Chunk.WriteChunk($5A);

    aSendMessage.Add(MemStream);
  finally
    Chunk.Free;
    MemStream.Free;
  end;
end;

procedure TG2FileModule.WriteParamList(aChunk: TPatchChunk;
  aVariationCount: integer);
begin
  aChunk.WriteBits(FModuleIndex, 8);
  FModuleParamList.Write(aChunk, aVariationCount);
end;

procedure TG2FileModule.WriteParamLabelList(aChunk: TPatchChunk);
begin
  aChunk.WriteBits(FModuleIndex, 8);
  FModuleParamLabelList.Write(aChunk);
end;

procedure TG2FileModule.CopyVariation(aFromVariation, aToVariation: byte);
begin
  FModuleParamList.CopyVariation(aFromVariation, aToVariation);
end;

procedure TG2FileModule.Read(aChunk: TPatchChunk);
var
  i: integer;
begin
  // FModuleIndex : TBits8;
  // FRow         : TBits7;
  // FCol         : TBits7;
  // FColor       : TBits8;
  // FUnknown1    : TBits8;
  // FUnknown2    : TBits4; => Seems to be the ModeCount
  // FModeInfoArray    : array of TBits6;

  // FTypeID      := Chunk.ReadBits(aStream, 8);
  // ModuleIndex  := aChunk.ReadBits( 8);
  FCol := aChunk.ReadBits(7);
  FRow := aChunk.ReadBits(7);
  FModuleColor := aChunk.ReadBits(8);
  FUprate := aChunk.ReadBits(1);
  FIsLed := aChunk.ReadBits(1);
  FUnknown1 := aChunk.ReadBits(6);
  FModeCount := aChunk.ReadBits(4);

  FNewCol := FCol;
  FNewRow := FRow;
  FNewModuleIndex := FModuleIndex;
  FNewModuleName := FModuleName;

  if assigned(aChunk.Log) then
    aChunk.Log.add_log_line(Format('%3d %3d %3d %3d %3d %3d %3d %3d %3d',
      [FTypeID, FModuleIndex, FRow, FCol, FModuleColor, FUprate, FIsLed,
      FUnknown1, FModeCount]), LOGCMD_NUL);

  SetLength(FModeInfoArray, FModeCount);
  for i := 0 to Length(FModeInfoArray) - 1 do
    FModeInfoArray[i] := aChunk.ReadBits(6);
end;

procedure TG2FileModule.ReadParamList(aChunk: TPatchChunk;
  aVariationCount: integer);
var
  i: integer;
begin
  FModuleParamList.Read(aChunk, aVariationCount);

  // Correction parameter count!
  case FTypeID of
    140:
      begin // Mix1-4S
        if FModuleParamList.FParameterCount = 8 then
        begin
          FModuleParamList.FParameterCount := 9;
          for i := 0 to FModuleParamList.Count - 1 do
          begin
            SetLength(FModuleParamList.Items[i].FParamValueArray, 9);
            FModuleParamList.Items[i].FParamValueArray[8] := 0;
          end;
        end;
      end;
  end;
  InvalidateParameters;
end;

procedure TG2FileModule.ReadParamLabelList(aChunk: TPatchChunk);
begin
  FModuleParamLabelList.Read(aChunk);
  InvalidateParameters;
end;

procedure TG2FileModule.ResetUprate;
begin
  FNewUprate := FUprate;
end;

procedure TG2FileModule.Write(aChunk: TPatchChunk);
var
  i: integer;
begin
  aChunk.WriteBits(FTypeID, 8);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FCol, 7);
  aChunk.WriteBits(FRow, 7);
  aChunk.WriteBits(FModuleColor, 8);
  aChunk.WriteBits(FUprate, 1);
  aChunk.WriteBits(FIsLed, 1);
  aChunk.WriteBits(FUnknown1, 6);
  aChunk.WriteBits(FModeCount, 4);

  for i := 0 to Length(FModeInfoArray) - 1 do
    aChunk.WriteBits(FModeInfoArray[i], 6);
end;

procedure TG2FileModule.SetCol(const Value: TBits7);
begin
  FCol := Value;
  FNewCol := FCol;
end;

procedure TG2FileModule.SetHeightUnits(const aValue: integer);
begin
  FHeightUnits := aValue;
end;

procedure TG2FileModule.SetIsLed(const aValue: TBits1);
begin
  FIsLed := aValue;
end;

procedure TG2FileModule.SetRow(const Value: TBits7);
begin
  FRow := Value;
  FNewRow := FRow;
end;

procedure TG2FileModule.SetModuleColor(const Value: TBits8);
begin
  if FModuleColor <> Value then
  begin
    FModuleColor := Value;

    NotifyObservers(EvtModuleColor, self as IG2Module);
  end;
end;

procedure TG2FileModule.SetModuleFileName(const aValue: string);
begin
  FModuleFileName := aValue;
end;

procedure TG2FileModule.SetModuleIndex(const aValue: TBits8);
begin
  FModuleIndex := aValue;
  FNewModuleIndex := FModuleIndex;

  { for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].ModuleIndex := aValue;
    for i := 0 to Length(FModeArray) - 1 do
    FModeArray[i].ModuleIndex := aValue; }

  // FModuleParamLabelList.FModuleIndex := aValue;
  // FModuleParamList.FModuleIndex := aValue;
end;

procedure TG2FileModule.SetUnknown1(const aValue: TBits6);
begin
  FUnknown1 := aValue;
end;

procedure TG2FileModule.SetUprate(const Value: TBits1);
begin
  if FUprate <> Value then
  begin
    FUprate := Value;
    InvalidateConnectors;
  end;
end;

function TG2FileModule.GetSelected: boolean;
begin
  Result := FSelected;
end;

procedure TG2FileModule.SetModuleName(const aValue: string);
begin
  FModuleName := aValue;
  FNewModuleName := FModuleName;
  InvalidateControl;
end;

function TG2FileModule.CreateAssignableParamList: TList<IG2Param>;
var
  i: integer;
begin
  Result := TList<IG2Param>.Create;

  for i := 0 to Length(FParamArray) - 1 do
    if FParamArray[i].DefaultKnobIndex >= 0 then
      Result.Add(FParamArray[i]);
end;

function TG2FileModule.GetCol: TBits7;
begin
  Result := FCol;
end;

function TG2FileModule.GetColor: integer;
begin
  if FModuleColor <= 24 then
    Result := ModuleColors[FModuleColor]
  else
    Result := ModuleColors[0];
end;

function TG2FileModule.GetFocussed: boolean;
begin
  if assigned(PatchPart) then
    Result := PatchPart.FocusedModuleIndex = ModuleIndex
  else
    Result := False;
end;

function TG2FileModule.GetHeightUnits: integer;
begin
  Result := FHeightUnits;
end;

Initialization
  ModuleIndexCompare :=
    function(const Left, Right: IG2Module): integer
    begin
      Result := Left.ModuleIndex - Right.ModuleIndex;
    end;

end.
