unit BVE.NMG2Connector;

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
  System.Math,
  BVE.NMG2Types,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  // Connector class

  TG2FileConnector = class(TG2ObservedObject, IG2Connector)
  private
    [weak] FWModule : IG2Module;

    FConnectorKind     : TConnectorKind;
    FConnectorType     : TConnectorType;
    FBandWidth         : TBandWidthType;
    FConnectorDefColor : TCableColor;
    FConnectorIndex    : Byte;
    FConnectorName     : string;

    procedure   SetConnectorKind( aValue : TConnectorKind);
    function    GetConnectorColor : TCableColor;
    function    GetNewConnectorColor: TCableColor;
    function    GetDefRate: TBits1;
    function    GetRate: TBits1;
    function    GetNewRate: TBits1;
    function    GetConnectorKind: TConnectorKind;
    function    GetConnectorDefColor : TCableColor;
    function    GetConnectorIndex: byte;
    function    GetConnectorType: TConnectorType;
    function    GetBandWidth: TBandWidthType;
    function    GetConnectorName: string;
    function    GetModule: IG2Module;
    function    GetSlotIndex: byte;
    function    GetModuleIndex: byte;
    function    GetLocationIndex: byte;
    function    GetConnected: boolean;

    procedure   SetConnectorIndex( const aValue : byte);
    procedure   SetConnectorType( const aValue : TConnectorType);
    procedure   SetBandWidth( const aValue : TBandWidthType);
    procedure   SetConnectorName( const aValue : string);
    procedure   SetConnectorDefColor( aValue : TCableColor); virtual;
    function    GetUprate: TBits1;
    function    GetNewUprate: TBits1;
  public
    constructor Create(aModule: IG2Module); reintroduce; virtual;
    destructor  Destroy; override;

    procedure   InitConnector(aModuleType, aConnectorIndex : Byte; aConnectorKind : TConnectorKind);

    function    CreateCableList: TList<IG2Cable>;

    procedure   InvalidateCables;
    procedure   InvalidateControl; virtual;

    procedure   CalcDefColor;

    property    Module : IG2Module read GetModule;
    property    LocationIndex : byte read GetLocationIndex;
    property    ConnectorKind : TConnectorKind read GetConnectorKind write SetConnectorKind;
    property    ConnectorIndex : byte read GetConnectorIndex write SetConnectorIndex;
    property    ConnectorType : TConnectorType read GetConnectorType write SetConnectorType;
    property    BandWidth : TBandWidthType read GetBandWidth write SetBandWidth;
    property    Uprate: TBits1 read GetUprate;
    property    NewUprate: TBits1 read GetNewUprate;
    property    ConnectorColor : TCableColor read GetConnectorColor;
  end;

implementation

//------------------------------------------------------------------------------
//
//                              TG2FileConnector
//
//------------------------------------------------------------------------------

constructor TG2FileConnector.Create(aModule: IG2Module);
begin
  inherited Create;

  SetWeak(@FWModule, aModule);

  FConnectorKind := ckInput;
  FConnectorDefColor := ccRed;
  FConnectorIndex := 0;
end;

destructor TG2FileConnector.Destroy;
begin
  SetWeak( @FWModule, nil);

  inherited;
end;

function TG2FileConnector.CreateCableList: TList<IG2Cable>;
var PatchPart: IG2PatchPart;
    Cable: IG2Cable;
begin
  Result := TList<IG2Cable>.Create;

  PatchPart := FWModule.PatchPart;
  for Cable in PatchPart.CableList do
    if ((Cable.ModuleIndexFrom = Module.ModuleIndex)
         and (Cable.ConnIndexFrom = FConnectorIndex)
         and (Cable.ConnFromKind = FConnectorKind))
      or ((Cable.ModuleINdexTo = Module.moduleIndex)
           and (Cable.ConnIndexTo = FConnectorIndex)
           and (Cable.ConnToKind = FConnectorKind)) then
      Result.Add(Cable);
end;

procedure TG2FileConnector.InitConnector( aModuleType, aConnectorIndex: Byte; aConnectorKind: TConnectorKind);
var i : integer;
begin
  FConnectorIndex := aConnectorIndex;
  FConnectorKind := aConnectorKind;
  case FConnectorKind of
    ckInput:
      begin
        i := GetDataModuleInputIndex( aModuleType, aConnectorIndex);
        FConnectorName := ModuleInputs[i].ConnectorName;
        FConnectorType := ModuleInputs[i].ConnectorType;
        FBandWidth := ModuleInputs[i].BandWidth;
      end;
    ckOutput:
      begin
        i := GetDataModuleOutputIndex( aModuleType, aConnectorIndex);
        FConnectorName := ModuleOutputs[i].ConnectorName;
        FConnectorType := ModuleOutputs[i].ConnectorType;
        FBandWidth := ModuleOutputs[i].BandWidth;
      end;
  end;
end;

procedure TG2FileConnector.InvalidateCables;
var CableList: TList<IG2Cable>;
    Cable: IG2Cable;
begin
  CableList := CreateCableList;
  try
    for Cable in CableList do
      Cable.Invalidate;
  finally
    CableList.Free;
  end;
end;

procedure TG2FileConnector.InvalidateControl;
begin
  NotifyObservers(EvtInvalidate, self as IG2Connector);
end;

procedure TG2FileConnector.SetBandWidth(const aValue: TBandWidthType);
begin
  FBandWidth := aValue;
end;

procedure TG2FileConnector.SetConnectorDefColor( aValue: TCableColor);
begin
  FConnectorDefColor := aValue;
end;

procedure TG2FileConnector.SetConnectorIndex(const aValue: byte);
begin
  FConnectorIndex := aValue;
end;

procedure TG2FileConnector.SetConnectorKind( aValue : TConnectorKind);
begin
  FConnectorKind := aValue;
end;

procedure TG2FileConnector.SetConnectorName(const aValue: string);
begin
  FConnectorName := aValue;
end;

procedure TG2FileConnector.SetConnectorType(const aValue: TConnectorType);
begin
  FConnectorType := aValue;
end;

function TG2FileConnector.GetConnected: boolean;
var PatchPart: IG2PatchPart;
    Cable: IG2Cable;
begin
  Result := False;

  PatchPart := FWModule.PatchPart;
  if assigned(PatchPart) then
    for Cable in PatchPart.CableList do
      if   ((Cable.ModuleIndexFrom = Module.ModuleIndex)
             and (Cable.ConnIndexFrom = FConnectorIndex)
             and (Cable.ConnFromKind = FConnectorKind))
        or ((Cable.ModuleIndexTo = Module.moduleIndex)
             and (Cable.ConnIndexTo = FConnectorIndex)
             and (Cable.ConnToKind = FConnectorKind)) then
      begin
        Result := True;
        exit;
      end;
end;

function TG2FileConnector.GetConnectorColor: TCableColor;
begin
  if (FConnectorDefColor = ccBlue) and (Uprate = 1) and (FBandWidth = btDynamic) then
    Result := ccRed
  else
    if (FConnectorDefColor = ccYellow) and (Uprate = 1) and (FBandWidth = btDynamic) then
      Result := ccOrange
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetConnectorDefColor: TCableColor;
begin
  Result := FConnectorDefColor;
end;

function TG2FileConnector.GetConnectorIndex: byte;
begin
  Result := FConnectorIndex;
end;

function TG2FileConnector.GetConnectorKind: TConnectorKind;
begin
  Result := FConnectorKind;
end;

function TG2FileConnector.GetConnectorName: string;
begin
  Result := FConnectorName;
end;

function TG2FileConnector.GetConnectorType: TConnectorType;
begin
  Result := FConnectorType;
end;

function TG2FileConnector.GetNewConnectorColor: TCableColor;
begin
  if (FConnectorDefColor = ccBlue) and (NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := ccRed
  else
    if (FConnectorDefColor = ccYellow) and (NewUprate = 1) and (FBandWidth = btDynamic) then
      Result := ccOrange
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetBandWidth: TBandWidthType;
begin
  Result := FBandWidth;
end;

procedure TG2FileConnector.CalcDefColor;
begin
  if FConnectorType = ctLogic then begin
    FConnectorDefColor := ccYellow;
  end else
    if FConnectorType = ctControl then
      FConnectorDefColor := ccBlue
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then FConnectorDefColor := ccRed;
        if FBandWidth = btDynamic then FConnectorDefColor := ccBlue;
      end;
end;

function TG2FileConnector.GetDefRate: TBits1;
begin
  Result := 0;
  // Return signal frequency indicator comming from this connector : 0 - 24kHz, 1 - 96kHz
  if FConnectorType = ctLogic then
    Result := 0
  else
    if FConnectorType = ctControl then
      Result := 0
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then Result := 1;
        if FBandWidth = btDynamic then Result := 0;
      end else
        Result := 0;
end;

function TG2FileConnector.GetLocationIndex: byte;
begin
  Result := FWModule.LocationIndex;
end;

function TG2FileConnector.GetModule: IG2Module;
begin
  Result := FWModule;
end;

function TG2FileConnector.GetModuleIndex: byte;
begin
  Result := FWModule.ModuleIndex;
end;

function TG2FileConnector.GetRate: TBits1;
begin
  if (Uprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

function TG2FileConnector.GetSlotIndex: byte;
begin
  Result := FWModule.SlotIndex;
end;

function TG2FileConnector.GetUprate: TBits1;
begin
  Result := Module.Uprate
end;

function TG2FileConnector.GetNewUprate: TBits1;
begin
  Result := Module.NewUprate
end;

function TG2FileConnector.GetNewRate: TBits1;
begin
  if (NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

end.
