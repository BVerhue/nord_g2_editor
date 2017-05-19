unit BVE.NMG2GraphFMX;

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

{$INCLUDE Capabilities.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.UIConsts,
  System.Math,
  System.Generics.Defaults,
{$IF Defined(VER270)}
  System.Math.Vectors,
{$ENDIF}
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
{$IF Defined(VER260) or Defined(VER270)}
  FMX.Graphics,
{$ENDIF}
  BVE.NMG2Types,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Synth,
  BVE.NMG2Module,
  BVE.NMG2Cable,
  BVE.NMG2Param,
  BVE.NMG2PatchPart,
{$IF Defined(TCPIPNETWORK)}
  BVE.NMG2TCPIP,
{$ENDIF}
  BVE.NMG2ControlsFMX,
  BVE.NMG2Pathdata;

type
  TCreateG2ControlEvent = procedure(Sender: TObject; aModule: IG2Module;
    aControl: TG2BaseControl) of Object;

  TG2ParamEvent = (g2pSet, g2pChange);

  TModuleListItem = class;

  TModuleSelectionList = class(TObjectList<TModuleListItem>)
  private
    [Weak]
    FScrollbox: TScrollbox;

    FItemIndex: integer;
    FFillColor: TAlphaColor;
    procedure SetItemIndex(const Value: integer);
  public
    constructor Create(AOwnsObjects: boolean);
    destructor Destroy; override;

    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ScrollBox: TScrollbox read FScrollbox write FScrollbox;
    property FillColor: TAlphaColor read FFillColor write FFillColor;
  end;

  TModuleListItem = class(TRectangle)
  private
    FTypeID: byte;
    FPageIndex: byte;
    FLabel: TG2Label;
    FImage: TImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PageIndex: byte read FPageIndex write FPageIndex;
    property Image: TImage read FImage;
    property ImageLabel: TG2Label read FLabel;
    property TypeID: byte read FTypeID write FTypeID;
  end;

  TG2SlotModulePanel = class(TG2ModulePanel, IG2Observer)
  private
    [Weak]
    FWModule: IG2Module;
  protected
    procedure SetModule(const Value: IG2Module);
    procedure DoPaintElement(const aElementType: TControlElementType;
      const aElementIndex: integer; aStyleSet: TG2StyleSet); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    procedure PaintOnCanvas(aCanvas: TCanvas; aScale: single);

    property Module: IG2Module read FWModule write SetModule;
  end;

  TG2SlotCable = class(TG2Cable, IG2Observer)
  private
    [Weak]
    FWCable: IG2Cable;
    procedure SetCable(const Value: IG2Cable);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Cable: IG2Cable read FWCable write SetCable;
  end;

  TG2SlotConnector = class(TG2Connector, IG2Observer)
  private
    [Weak]
    FWConnector: IG2Connector;
    procedure SetConnector(const Value: IG2Connector);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Init;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);

    property Connector: IG2Connector read FWConnector write SetConnector;
  end;

procedure CreateModuleImages(aBitmapList: TObjectDictionary<integer, TBitmap>);
procedure CreateModuleControls(aModule: IG2Module; aControl: TControl;
  aConnectParams, aConnectConnectors: boolean; aKnobControl: TKnobControl;
  ProcCreateControl: TCreateG2ControlEvent);
procedure ComponentSetStateStylesRecursive(aControl: TControl;
  aStateStyleList: TG2StateStyleList);
function ConvertToAlpha(aColor: integer): TAlphaColor;
procedure AdvanceFocus(aControl: IControl; const MoveForward: boolean);
function GetFocusedControl(aFMXObject: TFMXObject): IControl;

implementation

// ------------------------------------------------------------------------------
//
// Utils
//
// ------------------------------------------------------------------------------

function ConvertToAlpha(aColor: integer): TAlphaColor;
begin
  Result := $FF000000 + (Cardinal(aColor) and $000000FF) shl 16 +
    (Cardinal(aColor) and $0000FF00) + (Cardinal(aColor) and $00FF0000) shr 16;
end;

function CreateUnityMatrix: TMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

{$IF Defined(VER260) or Defined(VER270)}

function CreateScaleMatrix(sx, sy: single): TMatrix;
begin
  Result.m11 := sx;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := sy;
  Result.m23 := 0;
  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

function CreateTranslateMatrix(tx, ty: single): TMatrix;
begin
  Result.m11 := 1;
  Result.m12 := 0;
  Result.m13 := 0;
  Result.m21 := 0;
  Result.m22 := 1;
  Result.m23 := 0;
  Result.m31 := tx;
  Result.m32 := ty;
  Result.m33 := 1;
end;

function MatrixMultiply(const M1, M2: TMatrix): TMatrix;
begin
  Result.m11 := M1.m11 * M2.m11 + M1.m12 * M2.m21 + M1.m13 * M2.m31;
  Result.m12 := M1.m11 * M2.m12 + M1.m12 * M2.m22 + M1.m13 * M2.m32;
  Result.m13 := M1.m11 * M2.m13 + M1.m12 * M2.m23 + M1.m13 * M2.m33;
  Result.m21 := M1.m21 * M2.m11 + M1.m22 * M2.m21 + M1.m23 * M2.m31;
  Result.m22 := M1.m21 * M2.m12 + M1.m22 * M2.m22 + M1.m23 * M2.m32;
  Result.m23 := M1.m21 * M2.m13 + M1.m22 * M2.m23 + M1.m23 * M2.m33;
  Result.m31 := M1.m31 * M2.m11 + M1.m32 * M2.m21 + M1.m33 * M2.m31;
  Result.m32 := M1.m31 * M2.m12 + M1.m32 * M2.m22 + M1.m33 * M2.m32;
  Result.m33 := M1.m31 * M2.m13 + M1.m32 * M2.m23 + M1.m33 * M2.m33;
end;
{$ENDIF}

procedure ComponentSetStateStylesRecursive(aControl: TControl;
  aStateStyleList: TG2StateStyleList);
var
  i: integer;
begin
  for i := 0 to aControl.ChildrenCount - 1 do
  begin
    if aControl.Children[i] is TG2StyleControl then
    begin
      (aControl.Children[i] as TG2StyleControl).StateStyleList :=
        aStateStyleList;
    end
    else if aControl.Children[i] is TControl then
      ComponentSetStateStylesRecursive(aControl.Children[i] as TControl,
        aStateStyleList);
  end;
end;

procedure GetTabOrderList(aFMXObject: TFMXObject; const List: TInterfaceList;
  AChildren: boolean);
var
  i: integer;
  Control: IControl;
begin
  // if Assigned(FTabList) then
  for i := 0 to aFMXObject.ChildrenCount - 1 do
  begin
    if aFMXObject.Children[i].IsIControl then
    begin
      Control := IControl(aFMXObject.Children[i] as TControl);
      List.Add(Control);
      if AChildren and (Control.GetObject is TFMXObject) then
        GetTabOrderList(TFMXObject(Control), List, AChildren);
    end;
  end;
end;

procedure AdvanceFocus(aControl: IControl; const MoveForward: boolean);
var
  NewFocus: IControl;
  List: TInterfaceList;
  i, CurIdx: integer;

begin
  if not assigned(aControl) then
    exit;

  NewFocus := nil;
  List := TInterfaceList.Create;
  try
    GetTabOrderList(aControl.Parent, List, True);

    CurIdx := List.IndexOf(aControl);
    for i := 0 to List.Count - 1 do
    begin
      if MoveForward then
        CurIdx := (CurIdx + 1) mod List.Count
      else
      begin
        CurIdx := CurIdx - 1;
        if CurIdx < 0 then
          CurIdx := List.Count - 1;
      end;

      if IControl(List[CurIdx]).CheckForAllowFocus then
      begin
        NewFocus := IControl(List[CurIdx]);
        break;
      end;
    end;
  finally
    FreeAndNil(List);
  end;
  if assigned(NewFocus) then
    NewFocus.SetFocus;
end;

function GetFocusedControl(aFMXObject: TFMXObject): IControl;
begin
  if aFMXObject is TForm then
    Result := (aFMXObject as TForm).Focused
  else if assigned(aFMXObject.Parent) then
    Result := GetFocusedControl(aFMXObject.Parent)
  else
    Result := nil;
end;

procedure CreateModuleImage(aPatchPart: IG2PatchPart; const aModuleType: byte;
  aBitmap: TBitmap);
var
  Module: IG2Module;
  ModulePanel: TG2SlotModulePanel;
  Scale: single;
begin
  Module := TG2FileModule.Create(aPatchPart, aModuleType, 0);

  Module.InitModule;

  ModulePanel := TG2SlotModulePanel.Create(nil);

  ModulePanel.Parent := nil;
  ModulePanel.Module := Module;

  CreateModuleControls(Module, ModulePanel, True, True, kcCircular, nil);

  Scale := aBitmap.Width / (UNITS_COL + UNIT_MARGIN * 2);
  aBitmap.Clear(TAlphaColorRec.Alpha);

  ModulePanel.PaintOnCanvas(aBitmap.Canvas, Scale);
  Module.NotifyDestroy;
end;

procedure CreateModuleImages(aBitmapList: TObjectDictionary<integer, TBitmap>);
var
  i: integer;
  Bitmap: TBitmap;
  PatchPart: IG2PatchPart;
begin
  PatchPart := TG2PatchPart.Create(nil, 0);

  i := 0;
  while i <= High(ModuleDefs) do
  begin
    // i := 118;

    Bitmap := TBitmap.Create(UNITS_COL + UNIT_MARGIN * 2,
      (UNITS_ROW + UNIT_MARGIN * 2) * ModuleDefs[i].Height);

    CreateModuleImage(PatchPart, ModuleDefs[i].ModuleID, Bitmap);

    aBitmapList.Add(ModuleDefs[i].ModuleID, Bitmap);

    inc(i);
  end;
end;

// ------------------------------------------------------------------------------
//
// TModuleSelectionList
//
// ------------------------------------------------------------------------------

constructor TModuleSelectionList.Create(AOwnsObjects: boolean);
begin
  inherited Create(AOwnsObjects);
  FItemIndex := -1;
  FFillColor := claWhite;
end;

destructor TModuleSelectionList.Destroy;
begin
  inherited;
end;

procedure TModuleSelectionList.SetItemIndex(const Value: integer);
var
  ItemTop, ItemBottom: single;
  i: integer;
  NewViewportPosition: TPointF;
begin
  if FItemIndex <> Value then
  begin
    if FItemIndex <> -1 then
    begin
      // Items[FItemIndex].Fill.Color := $FFE0E0E0;
      Items[FItemIndex].Fill.Color := FFillColor;
      Items[FItemIndex].FLabel.StateStyleList.StateStyle[csDefault].FontColor
        := claBlack;
    end;

    FItemIndex := Value;

    if FItemIndex <> -1 then
    begin
      Items[FItemIndex].Fill.Color := claCyan;
      ItemTop := 0;
      for i := 0 to FItemIndex - 1 do
      begin
        ItemTop := ItemTop + Items[i].Height;
      end;
      ItemBottom := ItemTop + Items[FItemIndex].Height;
      NewViewportPosition := FScrollbox.ViewportPosition;
      if ItemBottom > FScrollbox.ViewportPosition.Y + FScrollbox.Height then
        NewViewportPosition.Y := ItemBottom - FScrollbox.Height;

      if ItemTop < FScrollbox.ViewportPosition.Y then
        NewViewportPosition.Y := ItemTop;

      FScrollbox.ViewportPosition := NewViewportPosition;

      // if ItemBottom >  (FScrollbox.ViewportPosition.Y + FScrollbox.Height) then
      // FScrollbox.ScrollTo(0, (FScrollbox.ViewportPosition.Y + FScrollbox.Height) - ItemBottom);
      // if ItemTop < FScrollbox.ViewportPosition.Y then
      // FScrollbox.ScrollTo(0, FScrollbox.ViewportPosition.Y - ItemTop);

    end;
  end;
end;

// ------------------------------------------------------------------------------
//
// TModuleListItem
//
// ------------------------------------------------------------------------------

constructor TModuleListItem.Create(AOwner: TComponent);
begin
  inherited;

  Stroke.Kind := TBrushKind.bkNone;

  FLabel := TG2Label.Create(self);
  FLabel.Parent := self;
  FLabel.Align := TAlignLayout.alTop;
  FLabel.Height := 20;
  FLabel.TextAlign := TTextAlign.taCenter;
  FLabel.HitTest := False;
  FLabel.StateStyleList.StateStyle[csDefault].Font.Style := [TFontStyle.fsBold];
  FLabel.StateStyleList.StateStyle[csDefault].Font.Size := 12;
  FLabel.StateStyleList.StateStyle[csSelected].Font.Style :=
    [TFontStyle.fsBold];
  FLabel.StateStyleList.StateStyle[csSelected].Font.Size := 12;

  FImage := TImage.Create(self);
  FImage.Parent := self;
  FImage.Align := TAlignLayout.alClient;
  FImage.HitTest := False;
end;

destructor TModuleListItem.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
//
// TG2SlotModulePanel
//
// ------------------------------------------------------------------------------

constructor TG2SlotModulePanel.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWModule, nil);
end;

destructor TG2SlotModulePanel.Destroy;
begin
  if assigned(Module) then
    Module := nil;
  inherited;
end;

procedure TG2SlotModulePanel.DoPaintElement(const aElementType
  : TControlElementType; const aElementIndex: integer; aStyleSet: TG2StyleSet);
begin
  if assigned(Module) then
    aStyleSet.Fill.Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
end;

procedure TG2SlotModulePanel.SetModule(const Value: IG2Module);
begin
  if FWModule <> Value then
  begin
    if assigned(FWModule) then
      FWModule.RemoveObserver(self);

    SetWeak(@FWModule, Value);

    if assigned(FWModule) then
    begin
      Init;
      FWModule.RegisterObserver(self);
    end;
  end;
end;

procedure TG2SlotModulePanel.Init;
begin
  if not assigned(Module) then
    exit;

  ModuleIndex := Module.ModuleIndex;
  LocationIndex := Module.LocationIndex;
  Selected := Module.Selected;
  Fill.Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
  ModuleLabel := Module.ModuleName;
  Row := Module.Row;
  Col := Module.Col;
  HeightUnits := Module.HeightUnits;
  Repaint;
end;

procedure TG2SlotModulePanel.Update(aG2Event: TG2Event;
  const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtModuleColor:
      begin
        if assigned(Module) then begin
          Fill.Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
          Repaint;
        end;
      end;
    EvtModuleAdd:
      ;
    EvtModuleDelete, EvtDestroy:
      begin
        // G2 Object will be destroyed
        DisposeOf;
      end;
    EvtDisposeControl:
      begin
        // Signal that panel must be destroyed
        DisposeOf;
      end;
    EvtModuleLabel:
      begin
        if assigned(Module) then begin
          ModuleLabel := FWModule.ModuleName;
          Repaint;
        end;
      end;
  end;
end;

procedure TG2SlotModulePanel.PaintOnCanvas(aCanvas: TCanvas; aScale: single);

var
  i: integer;
  G2Control: TG2BaseControl;
  SaveMatrix, M: TMatrix;
begin
  aCanvas.BeginScene;
  try
    SaveMatrix := aCanvas.Matrix;
    M := CreateScaleMatrix(aScale, aScale);
    aCanvas.SetMatrix(M);
    PaintOn(aCanvas);
    for i := 0 to ChildrenCount - 1 do
    begin
      G2Control := (Children[i] as TG2BaseControl);
      SaveMatrix := aCanvas.Matrix;
      try
        aCanvas.SetMatrix
          (MatrixMultiply(CreateTranslateMatrix(G2Control.UnscaledLeft,
          G2Control.UnscaledTop), M));
        G2Control.PaintOn(aCanvas);
      finally
        aCanvas.SetMatrix(SaveMatrix);
      end;
    end;
    aCanvas.SetMatrix(SaveMatrix);
  finally
    aCanvas.EndScene;
  end;
end;


// ------------------------------------------------------------------------------

//
// TG2SlotCable
//
// ------------------------------------------------------------------------------

constructor TG2SlotCable.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWCable, nil);
end;

destructor TG2SlotCable.Destroy;
begin
  if assigned(Cable) then
    Cable := nil;

  inherited;
end;

procedure TG2SlotCable.Init;
var
  P: TPointF;
begin
  if not assigned(Connector1) then
    exit;

  if not assigned(Connector2) then
    exit;

  P.X := Connector1.Position.X + Connector1.Width / 2;
  P.Y := Connector1.Position.Y + Connector1.Height / 2;
  Point1X := P.X + Connector1.ParentControl.Position.X;
  Point1Y := P.Y + Connector1.ParentControl.Position.Y;

  P.X := Connector2.Position.X + Connector2.Width / 2;
  P.Y := Connector2.Position.Y + Connector2.Height / 2;
  Point2X := P.X + Connector2.ParentControl.Position.X;
  Point2Y := P.Y + Connector2.ParentControl.Position.Y;

  Color := ConvertToAlpha(CableColors[ord(FWCable.CableColor)]);

  InitCable;
end;

procedure TG2SlotCable.SetCable(const Value: IG2Cable);
begin
  if FWCable <> Value then
  begin
    if assigned(FWCable) then
      FWCable.RemoveObserver(self);

    SetWeak(@FWCable, Value);

    if assigned(FWCable) then
    begin
      Init;
      FWCable.RegisterObserver(self);
    end;
  end;
end;

procedure TG2SlotCable.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtInvalidate:
      begin
        Init;
      end;
    EvtCableAdd:
      ;
    EvtCableDelete, EvtDestroy:
      begin
        // G2 Object will be destroyed
        DisposeOf;
      end;
    EvtDisposeControl:
      begin
        // Signal that ui cable must be destroyed
        DisposeOf;
      end;
    EvtCableColor:
      ;
  end;
end;

// ------------------------------------------------------------------------------
//
// TG2SlotConnector
//
// ------------------------------------------------------------------------------

constructor TG2SlotConnector.Create(AOwner: TComponent);
begin
  inherited;
  SetWeak(@FWConnector, nil);
end;

destructor TG2SlotConnector.Destroy;
begin
  if assigned(FWConnector) then
    Connector := nil;

  inherited;
end;

procedure TG2SlotConnector.Init;
begin
  if assigned(FWConnector) then
  begin
    ConnectorIndex := FWConnector.ConnectorIndex;
    ConnectorType := FWConnector.ConnectorType;
    BandWidth := FWConnector.BandWidth;
    FWConnector.CalcDefColor;
    ConnectorColor := ConvertToAlpha
      (CableColors[ord(FWConnector.ConnectorColor)]);
    ConnectorKind := FWConnector.ConnectorKind;
    // FG2Connector.OnCreateCable := CreateCable;
  end;
end;

procedure TG2SlotConnector.SetConnector(const Value: IG2Connector);
begin
  if FWConnector <> Value then
  begin
    if assigned(FWConnector) then
      FWConnector.RemoveObserver(self);

    SetWeak(@FWConnector, Value);

    if assigned(FWConnector) then
    begin
      Init;
      FWConnector.RegisterObserver(self);
    end;
  end;
end;

procedure TG2SlotConnector.Update(aG2Event: TG2Event;
  const aG2Object: IG2Object);
var
  CableList: TList<IG2Cable>;
begin
  case aG2Event of
    EvtInvalidate:
      begin
        ConnectorColor := ConvertToAlpha
          (CableColors[ord(FWConnector.ConnectorColor)]);
        CableList := FWConnector.CreateCableList;
        try
          Connected := CableList.Count > 0;
        finally
          CableList.Free;
        end;
        Repaint;
      end;
    EvtCableAdd:
      ;
    EvtCableDelete:
      ;
    EvtCableColor:
      ;
    EvtDestroy:
      begin
        Connector := nil;
      end;
    EvtDisposeControl:
      begin
        DisposeOf;
      end;
  end;
end;


// ------------------------------------------------------------------------------
//
// Creation of module controls
//
// ------------------------------------------------------------------------------

procedure CreateModuleControls(aModule: IG2Module; aControl: TControl;
  aConnectParams, aConnectConnectors: boolean; aKnobControl: TKnobControl;
  ProcCreateControl: TCreateG2ControlEvent);
var
  c: integer;

  procedure AssignWriterToParameter(aControl: TG2ParamControl);
  var
    Param: IG2Param;
  begin
    if not aConnectParams then
      exit;

    Param := aModule.Param[aControl.CodeRef];
    if not assigned(Param) then
      raise Exception.Create('Parameter ' + IntToStr(aControl.CodeRef) +
        ' not found in module ' + aModule.ModuleName);
    aControl.Param := Param;

    // aControl.OnGetTextFunc := Param.GetTextFunc;
    if aControl.InfoFunc <> -1 then
      Param.InfoFunctionIndex := aControl.InfoFunc;
  end;

  procedure AssignWriterToMode(aControl: TG2ParamControl);
  var
    Param: IG2Param;
  begin
    if not aConnectParams then
      exit;

    Param := aModule.Mode[aControl.CodeRef];
    if not assigned(Param) then
      raise Exception.Create('Mode ' + IntToStr(aControl.CodeRef) +
        ' not found in module ' + aModule.ModuleName);
    aControl.Param := Param;
  end;

  procedure AddToLedList(aLed: TG2Led);
  var
    PatchPart: IG2PatchPart;
    i: integer;
  begin
    if not assigned(aModule.PatchPart) then
      exit;

    PatchPart := aModule.PatchPart;

    if assigned(PatchPart) then
    begin
      i := 0;
      while (i < PatchPart.LedList.Count) and
        not((PatchPart.LedList[i].ModuleIndex = aModule.ModuleIndex) and
        (PatchPart.LedList[i].GroupID = aLed.LedGroupID)) do
        inc(i);

      if (i < PatchPart.LedList.Count) then
      begin
        aLed.DataLed := PatchPart.LedList[i];
      end;
    end;
  end;

  procedure AddToLedStripList(aLed: TG2Led);
  var
    PatchPart: IG2PatchPart;

    i: integer;

  begin
    if not assigned(aModule.PatchPart) then
      exit;

    PatchPart := aModule.PatchPart;

    if assigned(PatchPart) then
    begin
      i := 0;
      while (i < PatchPart.LedStripList.Count) and
        not((PatchPart.LedStripList[i].ModuleIndex = aModule.ModuleIndex) and
        (PatchPart.LedStripList[i].GroupID = aLed.LedGroupID)) do
        inc(i);

      if (i < PatchPart.LedStripList.Count) then
      begin
        aLed.DataLed := PatchPart.LedStripList[i];
      end;
    end;
  end;

  procedure SetCommonProperties(aG2Control: TG2BaseControl;
    aID, aXPos, aYPos: integer);
  begin
    aG2Control.Parent := aControl;
    aG2Control.ModuleIndex := aModule.ModuleIndex;
    aG2Control.LocationIndex := aModule.LocationIndex;
    aG2Control.ID := aID;
    aG2Control.UnscaledLeft := aXPos + UNIT_MARGIN;
    aG2Control.UnscaledTop := aYPos + (1 + aYPos / UNITS_ROW) * UNIT_MARGIN;
    if assigned(ProcCreateControl) then
      ProcCreateControl(nil, aModule, aG2Control);
  end;

  function CreateInput(aConnectorDef: TG2ConnectorDef): TG2SlotConnector;
  var
    Connector: IG2Connector;
  begin
    Connector := aModule.InConnector[aConnectorDef.CodeRef];
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aConnectorDef.CodeRef) +
        ' not found in module ' + aModule.ModuleName);

    Result := TG2SlotConnector.Create(aControl);
    SetCommonProperties(Result, aConnectorDef.ID, aConnectorDef.XPos,
      aConnectorDef.YPos);

    Result.BeginUpdate;
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.BandWidth;
    // AssignToInConnector( Result);
    Result.Connector := Connector;
    Result.EndUpdate;
  end;

  function CreateOutput(aConnectorDef: TG2ConnectorDef): TG2SlotConnector;
  var
    Connector: IG2Connector;
  begin
    Connector := aModule.OutConnector[aConnectorDef.CodeRef];
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aConnectorDef.CodeRef) +
        ' not found in module ' + aModule.ModuleName);

    Result := TG2SlotConnector.Create(aControl);
    SetCommonProperties(Result, aConnectorDef.ID, aConnectorDef.XPos,
      aConnectorDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.BandWidth;
    // AssignToOutConnector( Result);
    Result.Connector := Connector;
    Result.EndUpdate;
  end;

  function CreateLabel(aTextDef: TG2TextDef): TG2Label;
  var
    StyleSet: TG2StyleSet;
  begin
    Result := TG2Label.Create(aControl);
    SetCommonProperties(Result, aTextDef.ID, aTextDef.XPos, aTextDef.YPos);
    Result.BeginUpdate;
    Result.LabelText := aTextDef.slText;
    StyleSet := Result.StateStyleList.StateStyle[csDefault];
    StyleSet.Font.Size := aTextDef.FontSize - 2;
    Result.EndUpdate;
  end;

  function CreateLed(aLedDef: TG2LedDef): TG2LedGreen;
  begin
    Result := TG2LedGreen.Create(aControl);
    SetCommonProperties(Result, aLedDef.ID, aLedDef.XPos, aLedDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aLedDef.CodeRef;
    Result.InfoFunc := aLedDef.InfoFunc;
    Result.LedType := aLedDef.LedType;
    Result.LedGroupID := aLedDef.GroupID;
    case aLedDef.LedType of
      ltSequencer:
        begin
          Result.UnscaledWidth := 12;
          Result.UnscaledHeight := 7;
          AddToLedStripList(Result);
        end;
      ltGreen:
        begin
          Result.UnscaledWidth := 8;
          Result.UnscaledHeight := 8;
          if GetDataLedsInGroup(aModule.TypeID, aLedDef.GroupID) > 1 then
            AddToLedStripList(Result)
          else
            AddToLedList(Result);
        end;
    end;
    Result.EndUpdate;
  end;

  function CreateMiniVU(aMiniVUDef: TG2MiniVUDef): TG2MiniVU;
  begin
    Result := TG2MiniVU.Create(aControl);
    SetCommonProperties(Result, aMiniVUDef.ID, aMiniVUDef.XPos,
      aMiniVUDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aMiniVUDef.CodeRef;
    Result.InfoFunc := aMiniVUDef.InfoFunc;
    Result.LegGroupID := aMiniVUDef.GroupID;
    Result.LedType := ltMiniVU;
    AddToLedStripList(Result);
    Result.EndUpdate;
  end;

  function CreateTextField(aTextFieldDef: TG2TextFieldDef): TG2TextField;
  var
    G2Function: IG2Function;
  begin
    Result := TG2TextField.Create(aControl);
    SetCommonProperties(Result, aTextFieldDef.ID, aTextFieldDef.XPos,
      aTextFieldDef.YPos);
    Result.BeginUpdate;
    Result.UnscaledWidth := aTextFieldDef.Width;
    Result.MasterRef := aTextFieldDef.MasterRef;
    Result.TextFunc := aTextFieldDef.TextFunc;
    Result.Dependencies := aTextFieldDef.slDependencies;
    // ParseDependencies( aModule, Result);
    G2Function := aModule.AddTextFunction(aTextFieldDef.TextFunc,
      aTextFieldDef.MasterRef, aTextFieldDef.slDependencies);
    Result.G2Function := G2Function;
    Result.EndUpdate;
  end;

  function CreateTextEdit(aTextEditDef: TG2TextEditDef): TG2BtnTextEdit;
  begin
    Result := TG2BtnTextEdit.Create(aControl);
    SetCommonProperties(Result, aTextEditDef.ID, aTextEditDef.XPos,
      aTextEditDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aTextEditDef.CodeRef;
    Result.InfoFunc := aTextEditDef.InfoFunc;
    Result.ButtonText.DelimitedText := aTextEditDef.slText;
    case aTextEditDef.TextEditType of
      bttPush:
        begin
          Result.ButtonKind := bkMomentary
        end;
      bttCheck:
        begin
          Result.ButtonKind := bkToggle
        end;
    end;
    Result.UnscaledWidth := aTextEditDef.Width;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateKnob(aKnobDef: TG2KnobDef): TG2Knob;
  begin
    Result := TG2Knob.Create(aControl);
    SetCommonProperties(Result, aKnobDef.ID, aKnobDef.XPos, aKnobDef.YPos);
    Result.BeginUpdate; // SetParent sets FUpdating to 0
    Result.CodeRef := aKnobDef.CodeRef;
    Result.InfoFunc := aKnobDef.InfoFunc;
    Result.KnobType := aKnobDef.KnobType;
    Result.SetDefaultDimensions;
    AssignWriterToParameter(Result);

    // if assigned(aModule.PatchPart) and assigned(aModule.PatchPart.Patch) and assigned(aModule.PatchPart.Patch.G2) then begin
    Result.KnobControl := aKnobControl;
    // end;
    Result.EndUpdate;
  end;

  function CreateButtonText(aButtonTextDef: TG2ButtonTextDef): TG2BtnText;
  begin
    Result := TG2BtnText.Create(aControl);
    SetCommonProperties(Result, aButtonTextDef.ID, aButtonTextDef.XPos,
      aButtonTextDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonTextDef.CodeRef;
    Result.InfoFunc := aButtonTextDef.InfoFunc;
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonTextDef.slText;
    Result.ButtonType := aButtonTextDef.ButtonType;
    Result.UnscaledWidth := aButtonTextDef.Width;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonTextDef.slImageID;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateButtonFlat(aButtonFlatDef: TG2ButtonFlatDef): TG2BtnFlat;
  begin
    Result := TG2BtnFlat.Create(aControl);
    SetCommonProperties(Result, aButtonFlatDef.ID, aButtonFlatDef.XPos,
      aButtonFlatDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonFlatDef.CodeRef;
    Result.InfoFunc := aButtonFlatDef.InfoFunc;
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonFlatDef.slText;
    Result.UnscaledWidth := aButtonFlatDef.Width;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonFlatDef.slImageID;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateButtonIncDec(aButtonIncDecDef: TG2ButtonIncDecDef)
    : TG2BtnIncDec;
  begin
    Result := TG2BtnIncDec.Create(aControl);
    SetCommonProperties(Result, aButtonIncDecDef.ID, aButtonIncDecDef.XPos,
      aButtonIncDecDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonIncDecDef.CodeRef;
    Result.InfoFunc := aButtonIncDecDef.InfoFunc;
    case aButtonIncDecDef.ButtonType of
      bidLeftRight:
        begin
          Result.Orientation := otHorizontal;
        end;
      bidUpDown:
        begin
          Result.Orientation := otVertical;
        end;
    end;
    Result.CalcDimensionsFromBtnSize(Result.ButtonWidth, Result.ButtonHeight);
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateLevelShift(aLevelShiftDef: TG2LevelShiftDef): TG2BtnFlat;
  begin
    Result := TG2BtnFlat.Create(aControl);
    SetCommonProperties(Result, aLevelShiftDef.ID, aLevelShiftDef.XPos,
      aLevelShiftDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aLevelShiftDef.CodeRef;
    Result.InfoFunc := aLevelShiftDef.InfoFunc;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aLevelShiftDef.slImageID;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateButtonRadio(aButtonRadioDef: TG2ButtonRadioDef): TG2BtnRadio;
  begin
    Result := TG2BtnRadio.Create(aControl);
    SetCommonProperties(Result, aButtonRadioDef.ID, aButtonRadioDef.XPos,
      aButtonRadioDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonRadioDef.CodeRef;
    Result.InfoFunc := aButtonRadioDef.InfoFunc;
    Result.Orientation := aButtonRadioDef.Orientation;
    Result.ButtonCount := aButtonRadioDef.ButtonCount;
    Result.ButtonWidth := aButtonRadioDef.ButtonWidth;
    Result.CalcDimensionsFromBtnSize(Result.ButtonWidth, Result.ButtonHeight);
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonRadioDef.slText;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aButtonRadioDef.slImageID;
    Result.ImageWidth := aButtonRadioDef.ImageWidth;
    Result.UpsideDown := True;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreateButtonRadioEdit(aButtonRadioEditDef: TG2ButtonRadioEditDef)
    : TG2BtnRadioEdit;
  begin
    Result := TG2BtnRadioEdit.Create(aControl);
    SetCommonProperties(Result, aButtonRadioEditDef.ID,
      aButtonRadioEditDef.XPos, aButtonRadioEditDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aButtonRadioEditDef.CodeRef;
    Result.InfoFunc := aButtonRadioEditDef.InfoFunc;
    Result.ButtonColumns := aButtonRadioEditDef.ButtonColumns;
    Result.ButtonRows := aButtonRadioEditDef.ButtonRows;
    Result.CalcDimensionsFromBtnSize(Result.ButtonWidth, Result.ButtonHeight);
    Result.ButtonText.Delimiter := ';';
    Result.ButtonText.DelimitedText := aButtonRadioEditDef.slText;
    AssignWriterToParameter(Result);
    Result.EndUpdate;
  end;

  function CreatePartSelector(aPartSelectorDef: TG2PartSelectorDef)
    : TG2PartSelector;
  begin
    Result := TG2PartSelector.Create(aControl);
    SetCommonProperties(Result, aPartSelectorDef.ID, aPartSelectorDef.XPos,
      aPartSelectorDef.YPos);
    Result.BeginUpdate;
    Result.CodeRef := aPartSelectorDef.CodeRef;
    Result.InfoFunc := aPartSelectorDef.InfoFunc;
    Result.UnscaledWidth := aPartSelectorDef.Width;
    Result.UnscaledHeight := aPartSelectorDef.Height;
    Result.OptionsText.Delimiter := ';';
    Result.OptionsText.DelimitedText := aPartSelectorDef.slText;
    Result.ImageIDS.Delimiter := ';';
    Result.ImageIDS.DelimitedText := aPartSelectorDef.slImageID;
    // Result.ImageWidth := aPartSelectorDef.ImageWidth;
    // Result.ImageCount := aPartSelectorDef.ImageCount;
    AssignWriterToMode(Result);
    Result.EndUpdate;
  end;

begin
  for c := 0 to High(LineDefs) do
  begin
    if LineDefs[c].ModuleID = aModule.TypeID then
    begin
      //
    end;
  end;

  for c := 0 to High(BitmapDefs) do
  begin
    if BitmapDefs[c].ModuleID = aModule.TypeID then
    begin
      //
    end;
  end;

  for c := 0 to High(TextDefs) do
  begin
    if TextDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateLabel(TextDefs[c]);
    end;
  end;

  for c := 0 to High(ModuleInputs) do
  begin
    if ModuleInputs[c].ModuleID = aModule.TypeID then
    begin
      CreateInput(ModuleInputs[c]);
    end;
  end;

  for c := 0 to High(ModuleOutputs) do
  begin
    if ModuleOutputs[c].ModuleID = aModule.TypeID then
    begin
      CreateOutput(ModuleOutputs[c]);
    end;
  end;

  for c := 0 to High(TextFieldDefs) do
  begin
    if TextFieldDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateTextField(TextFieldDefs[c]);
    end;
  end;

  for c := 0 to High(GraphDefs) do
  begin
    if GraphDefs[c].ModuleID = aModule.TypeID then
    begin
    end;
  end;

  for c := 0 to High(LedDefs) do
  begin
    if LedDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateLed(LedDefs[c]);
    end;
  end;

  for c := 0 to High(MiniVUDefs) do
  begin
    if MiniVUDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateMiniVU(MiniVUDefs[c]);
    end;
  end;

  for c := 0 to High(PartSelectorDefs) do
  begin
    if PartSelectorDefs[c].ModuleID = aModule.TypeID then
    begin
      CreatePartSelector(PartSelectorDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonTextDefs) do
  begin
    if ButtonTextDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateButtonText(ButtonTextDefs[c]);
    end;
  end;

  for c := 0 to High(TextEditDefs) do
  begin
    if TextEditDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateTextEdit(TextEditDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonFlatDefs) do
  begin
    if ButtonFlatDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateButtonFlat(ButtonFlatDefs[c]);
    end;
  end;

  for c := 0 to High(LevelShiftDefs) do
  begin
    if LevelShiftDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateLevelShift(LevelShiftDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonRadioDefs) do
  begin
    if ButtonRadioDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateButtonRadio(ButtonRadioDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonRadioEditDefs) do
  begin
    if ButtonRadioEditDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateButtonRadioEdit(ButtonRadioEditDefs[c]);
    end;
  end;

  for c := 0 to High(ButtonIncDecDefs) do
  begin
    if ButtonIncDecDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateButtonIncDec(ButtonIncDecDefs[c]);
    end;
  end;

  for c := 0 to High(KnobDefs) do
  begin
    if KnobDefs[c].ModuleID = aModule.TypeID then
    begin
      CreateKnob(KnobDefs[c]);
    end;
  end;
end;

end.
