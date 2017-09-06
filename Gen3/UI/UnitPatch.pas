unit UnitPatch;

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

// Patching control

interface

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  System.Variants,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Layouts,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Objects,
  FMX.Materials,
  FMX.MaterialSources,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2ControlsG2,
  BVE.NMG2ControlsGL,
  BVE.NMG2TexturesGL;

type
  TSelectionMode = (smMove, smCopy);

  TSelectionGL = class
  private
    FStartPosition, FPosition: TPoint3D;
    FWidth, FHeight: single;
    FOutlineList: TObjectList<TOutlineGL>;
    function GetDeltaUnitsCol: integer;
    function GetDeltaUnitsRow: integer;
    procedure SetHeight(const Value: single);
    procedure SetPosition(const Value: TPoint3D);
    procedure SetWidth(const Value: single);
  public
    constructor Create(aModuleList: TList<IG2Module>); overload;
    constructor Create(const aCol, aRow, aHeight: integer); overload;
    destructor Destroy; override;

    procedure Move(Dx, Dy: single);

    procedure Render(aContext: TContext3D);

    property DeltaUnitsCol: integer read GetDeltaUnitsCol;
    property DeltaUnitsRow: integer read GetDeltaUnitsRow;
    property Position: TPoint3D read FPosition write SetPosition;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
  end;

  TModuleCursorGL = class
  private
    FCol, FRow, FHeight: integer;
    FVisible: boolean;
    procedure SetHeight(const Value: integer);
    procedure SetVisible(const Value: boolean);
    procedure SetCol(const Value: integer);
    procedure SetRow(const Value: integer);
  public
    constructor Create(const aCol, aRow, aHeight: integer); overload;
    destructor Destroy; override;

    procedure Render(aContext: TContext3D);

    property Col: integer read FCol write SetCol;
    property Row: integer read FRow write SetRow;
    property Height: integer read FHeight write SetHeight;
    property Visible: boolean read FVisible write SetVisible;
  end;

  TPatchGL = class(TG2SlotPanelGL)
  private
    FLocation: integer;

    FCableBitmap: TTiledTexture;
    FCableMesh: TSegmentedMeshGL;
    FCableMat: TTextureMaterial;

    FBackgroundLayer: TTexMatrixControlGL;

    FRenderCables: Boolean;
    FRenderModules: Boolean;

    FModuleSelection: TSelectionGL;
    FModuleCursor: TModuleCursorGL;

    FCableCtrlList: TObjectDictionary<IG2Cable, TG2CableGL>;
    FModuleCtrlList: TObjectDictionary<IG2Module, TG2ModuleGL>;

    FActiveSelectorList: TG2SelectorListGL;
    FFromConnector: TG2ConnectorGL;

    FOperationMode: TOperationMode;
    FLongClick: boolean;

    function GetPatch: IG2Patch;

    procedure AddModuleFMX(Sender: TObject; const aLocationIndex: byte;
      aModule: IG2Module);
    procedure AddCableFMX(Sender: TObject; const aLocationIndex: byte;
      aCable: IG2Cable);
    procedure CreateG2Control(Sender: TObject; aModule: IG2Module;
      aControl: TControlGL);

    procedure ConnectorClk(Sender: TObject; Shift: TShiftState;
      const aBtnIndex: integer);
    procedure ModuleClk(Sender: TObject; Shift: TShiftState;
      const aBtnIndex: integer);

    procedure RenderCables;

    procedure SelectorDropDown(Sender: TObject);
    procedure SelectorCloseUp(Sender: TObject);

    procedure MoveSelectedPanels(const Dx, Dy: single);
    procedure SelectAll;
    procedure UnSelectAll;

    procedure SetActiveSelectorList(const Value: TG2SelectorListGL);
    procedure SetLocation(const Value: integer);
    procedure SetOperationMode(const Value: TOperationMode);
    function GetSelectedModuleCount: integer;
    function GetPatchPart: IG2PatchPart;
    //function GetConnection: IG2Connection;
    //function GetSlot: IG2Slot;
  protected
    procedure SetConnectionIndex(const Value: integer); override;
    procedure SetSlotIndex(const Value: integer); override;
    procedure SetTextureList(const Value: TTextureListGL); override;
    procedure SetMatrix3D(const Value: TMatrix3D); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: single); override;

    procedure DoLongClk(Shift: TShiftState; X, Y: Single); override;

    procedure RecreateContext; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ClearPatch;
    procedure InitPatch;

    procedure Render(aBitmap: TBitmap); override;

    function ControlAt(const aX, aY: single): TControlGL; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;

    //property Connection: IG2Connection read GetConnection;
    //property Slot: IG2Slot read GetSlot;
    property Patch: IG2Patch read GetPatch;
    property PatchPart: IG2PatchPart read GetPatchPart;
    property Location: integer read FLocation write SetLocation;
    property SelectedModuleCount: integer read GetSelectedModuleCount;
    property OperationMode: TOperationMode read FOperationMode
      write SetOperationMode;
    property ActiveSelectorList: TG2SelectorListGL read FActiveSelectorList
      write SetActiveSelectorList;
    property FromConnector: TG2ConnectorGL read FFromConnector
      write FFromConnector;
  end;

implementation

uses
  System.Math,
  BVE.NMG2Data;

// -----------------------------------------------------------------------------
//
//                                TPatchGL
//
// -----------------------------------------------------------------------------

constructor TPatchGL.Create(AOwner: TComponent);
begin
  inherited;

  Touch.InteractiveGestures := Touch.DefaultInteractiveGestures +
    [TInteractiveGesture.Zoom];
  AniCalculations.Animation := True;
  AniCalculations.BoundsAnimation := False;
  AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

  FCableCtrlList := TObjectDictionary<IG2Cable, TG2CableGL>.Create
    ([doOwnsValues]);
  FModuleCtrlList := TObjectDictionary<IG2Module, TG2ModuleGL>.Create
    ([doOwnsValues]);

  // FCableLayer := nil;
  FCableMesh := TSegmentedMeshGL.Create;
  FCableMat := TTextureMaterial.Create;

  FRenderCables := True;
  FRenderModules := True;

  AutoCapture := True;

  CanTranslate := True;

  FModuleSelection := nil;
  FModuleCursor := TModuleCursorGL.Create(0, 0, 1);

  FBackgroundLayer := TTexMatrixControlGL.Create(self);

  FLocation := LOCATION_VA;

  Scale3D := Point3D(0.5, 0.5, 1);
  // Scale3D := Point3D(1, 1, 1);
end;

destructor TPatchGL.Destroy;
begin
  FModuleCtrlList.Free;
  FCableCtrlList.Free;

  FreeAndNil(FCableMesh);
  FreeAndNil(FCableMat);
  FreeAndNil(FCableBitmap);

  if assigned(FModuleSelection) then
    FreeAndNil(FModuleSelection);

  FreeAndNil(FModuleCursor);

  inherited;
end;

procedure TPatchGL.RecreateContext;
begin
  inherited;

  FreeAndNil(FCableBitmap);

  if assigned(Context) then
  begin
    FCableBitmap := TTiledTexture.Create;
    FCableBitmap.SetSize(Context.Width, Context.Height);

    FRenderCables := True;
    FRenderModules := True;
  end;
end;

procedure TPatchGL.InitPatch;
var
  Module: IG2Module;
  Cable: IG2Cable;
begin
  ClearPatch;
  if assigned(Patch) then
  begin
    FRenderCables := True;
    FRenderModules := True;

    if assigned(Patch.PatchPart[Location]) then
    begin
      Patch.PatchPart[Location].OnAddModule := AddModuleFMX;
      Patch.PatchPart[Location].OnAddCable := AddCableFMX;

      for Module in Patch.PatchPart[Location].ModuleList do
      begin
        AddModuleFMX(self, Location, Module);
      end;

      for Cable in Patch.PatchPart[Location].CableList do
      begin
        AddCableFMX(self, Location, Cable);
      end;
    end;
    FullRepaint;
  end;
end;

procedure TPatchGL.ClearPatch;
begin
  if assigned(Patch) then
  begin
    ActiveSelectorList := nil;

    FModuleCtrlList.Clear;
    FCableCtrlList.Clear;

    if assigned(Patch.PatchPart[Location]) then
    begin
      Patch.PatchPart[Location].OnAddModule := nil;
      Patch.PatchPart[Location].OnAddCable := nil;
    end;
  end;
end;

procedure TPatchGL.AddCableFMX(Sender: TObject; const aLocationIndex: byte;
  aCable: IG2Cable);
var
  CableControl: TG2CableGL;
  ModulePanel: TG2ModuleGL;
  Module: IG2Module;
  Connector: TG2ConnectorGL;

  function FindConnector(const aModulePanel: TG2ModuleGL;
    const aConnectorIndex: integer; const aConnectorKind: TConnectorKind)
    : TG2ConnectorGL;
  var
    FMXObject: TFMXObject;
    Connector: IG2Connector;
  begin
    Result := nil;
    for FMXObject in aModulePanel.Children do
    begin
      if FMXObject is TG2ConnectorGL then
      begin
        Connector := (FMXObject as TG2ConnectorGL).Connector;
        if (Connector.ConnectorIndex = aConnectorIndex) and
          (Connector.ConnectorKind = aConnectorKind) then
        begin
          Result := (FMXObject as TG2ConnectorGL);
          exit;
        end;
      end;
    end;
  end;

begin
  if aCable.LocationIndex = Location then
  begin
    CableControl := TG2CableGL.Create(self);
    FCableCtrlList.Add(aCable, CableControl);

    CableControl.Cable := aCable;
    // CableControl.CableStyle := FWConMan.CableStyle;
    CableControl.Color := ConvertToAlpha(CableColors[ord(aCable.CableColor)]);

    Module := aCable.ModuleFrom;
    if assigned(Module) then
    begin
      if FModuleCtrlList.TryGetValue(Module, ModulePanel) then
      begin
        Connector := FindConnector(ModulePanel, aCable.ConnIndexFrom,
          aCable.ConnFromKind);
        CableControl.Connector1 := Connector;
      end;
    end;

    Module := aCable.ModuleTo;
    if assigned(Module) then
    begin
      if FModuleCtrlList.TryGetValue(Module, ModulePanel) then
      begin
        Connector := FindConnector(ModulePanel, aCable.ConnIndexTo,
          aCable.ConnToKind);
        CableControl.Connector2 := Connector;
      end;
    end;
    aCable.Invalidate;
  end;
end;

procedure TPatchGL.AddModuleFMX(Sender: TObject; const aLocationIndex: byte;
  aModule: IG2Module);
var
  ModulePanel: TG2ModuleGL;
begin
  if aModule.LocationIndex = Location then
  begin
    ModulePanel := TG2ModuleGL.Create(self);
    ModulePanel.TextureList := TextureList;
    ModulePanel.Parent := self;
    FModuleCtrlList.Add(aModule, ModulePanel);
    ModulePanel.Module := aModule;
    ModulePanel.CreateModuleControls(True, True, ConMan.KnobControl,
      CreateG2Control);

    ModulePanel.OnLongClk := ModuleClk;

    aModule.InvalidateControl;
  end;
end;

procedure TPatchGL.CreateG2Control(Sender: TObject; aModule: IG2Module;
  aControl: TControlGL);
begin
  if aControl is TG2SelectorGL then
  begin
    (aControl as TG2SelectorGL).OnDropDown := SelectorDropDown;
    (aControl as TG2SelectorGL).OnCloseUp := SelectorCloseUp;
  end;

  if aControl is TG2ConnectorGL then
  begin
    (aControl as TG2ConnectorGL).OnClk := ConnectorClk;
  end;
end;

procedure TPatchGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
var
  ModuleDef: TG2ModuleDef;
  R: TRectF;
  Rect: TRect;
  Cable: IG2Cable;
  Module: IG2Module;
begin
  case aG2Event of
    EvtUSBActiveChange:
      ;
    EvtUSBError:
      ;
    EvtBeforeSendMessage:
      ;
    EvtProcessResponseMsg:
      ;
    EvtNextInitStep:
      ;
    EvtAfterG2Init:
      begin
        InitPatch;
      end;
    EvtAfterPerfInit:
      ;
    EvtAfterSlotInit:
      begin
        InitPatch;
      end;
    EvtPerfsSettingsUpdate:
      ;
    EvtPerfUpdate:
      ;
    EvtSynthSettingsUpdate:
      ;
    EvtBeforePatchUpdate:
      begin
        // PreparePatchUpdate;
      end;
    EvtPatchUpdate:
      begin
        { CalcLayoutDimensions;

          SetCablesVisible(ccRed, Patch.Settings.RedVisible);
          SetCablesVisible(ccBlue, Patch.Settings.BlueVisible);
          SetCablesVisible(ccYellow, Patch.Settings.YellowVisible);
          SetCablesVisible(ccOrange, Patch.Settings.OrangeVisible);
          SetCablesVisible(ccGreen, Patch.Settings.GreenVisible);
          SetCablesVisible(ccPurple, Patch.Settings.PurpleVisible);
          SetCablesVisible(ccWhite, Patch.Settings.WhiteVisible);

          UpdateControls; }
        InitPatch;
      end;
    EvtVariationChange:
      begin
        // Variation := Patch.Settings.ActiveVariation;
        // Patch.InvalidateParameters;
      end;

    EvtCopyVariation:
      ;
    EvtMidiClockReceive:
      ;
    EvtClockRunChange:
      ;
    EvtClockBPMChange:
      ;
    EvtMidiCCRecieve:
      ;
    EvtAfterGetAssignedVoices:
      ;
    EvtPatchLoadChange:
      ;
    EvtSelectSlot:
      begin
        // UpdateControls;
      end;
    EvtSelectLocation:
      begin
        // if Patch.SelectedLocation in [LOCATION_FX, LOCATION_VA] then
        // SelectedLocationIndex := Patch.SelectedLocation;
        // UpdateControls;
        Location := Patch.SelectedLocation;
      end;
    EvtSelectModule:
      begin
      end;
    EvtSelectParam:
      ;
    EvtLabelValueChange:
      ;
    EvtMorphChange:
      ;
    EvtModuleAdd:
      begin
        //ModuleDef := GetDataModuleDef(ConMan.SelectedModuleType);
        //ConMan.SelectedRow := ConMan.SelectedRow + ModuleDef.Height;
        FModuleCursor.Row := ConMan.SelectedRow;

        //OperationMode := omNormal;

        FRenderModules := True;
        Invalidate;
      end;
    EvtModuleDelete:
      begin
        if Supports(aG2Object, IG2Module, Module) then
        begin
          FModuleCtrlList.Remove(Module);
        end;
      end;
    EvtCableDelete:
      begin
        if Supports(aG2Object, IG2Cable, Cable) then
        begin
          FCableCtrlList.Remove(Cable);
        end;
      end;
    EvtAfterRetreivePatch:
      begin
        ClearPatch;
      end;
    EvtAfterBankList:
      ;
    EvtAfterStore:
      ;
    EvtAfterClear:
      ;
    EvtAfterClearBank:
      ;
    EvtAfterBankDownload:
      ;
    EvtDeassignKnob:
      ;
    EvtAssignKnob:
      ;
    EvtDeassignGlobalKnob:
      ;
    EvtAssignGlobalKnob:
      ;
    EvtPianoRoll:
      begin
        // ActivatePianoroll;
      end;
    EvtCut:
      ;
    EvtCopy:
      ;
    EvtPaste:
      begin
        if assigned(Connection) and assigned(Connection.CopyPatch) then
        begin
          {Rect := Connection.CopyPatch.SelectedUnitsRect;

          ActivateSelection(
            Connection.CopyPatch,
            smCopy,
            ((Rect.Left + (Rect.Right - Rect.Left)/2) * (UNITS_COL+UNIT_MARGIN*2)),
            ((Rect.Top + (Rect.Bottom - Rect.Top)/2) * (UNITS_ROW+UNIT_MARGIN*2)));}

          //OperationMode := omPaste;
        end;
      end;
    EvtPasteParams:
      ;
    EvtDelete:
      ;
    EvtSelectModuleType:
      begin
        ModuleDef := GetDataModuleDef(ConMan.SelectedModuleType);
        //R := ModuleRect(0, 0, ModuleDef.Height);
        //OperationMode := omAddModule;
        FModuleCursor.Height := ModuleDef.Height;
      end;
    EvtDestroy:
      begin
        { if Supports(aG2Object, IG2Connection) then
          if assigned(Connection) then
          begin
          Connection.RemoveObserver(self);
          FConnectionIndex := -1;
          end;

          if Supports(aG2Object, IG2Slot) then
          if assigned(Slot) then
          begin
          Slot.RemoveObserver(self);
          FSlotIndex := -1;
          end;

          if Supports(aG2Object, IG2ConnectionManager) then
          RemoveAsObserver; }
      end;
    EvtOperationModeChange:
      begin
        if assigned(Slot) then
          OperationMode := Slot.OperationMode;
      end;
  end;
end;

procedure TPatchGL.ConnectorClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
var
  ToConnector: TG2ConnectorGL;
begin
  if OperationMode = omDrawCable then
  begin
    ToConnector := Sender as TG2ConnectorGL;

    Connection.PatchCableAdd(SlotIndex, Location,
      FFromConnector.Connector.ModuleIndex,
      FFromConnector.Connector.ConnectorIndex,
      FFromConnector.Connector.ConnectorKind,
      FFromConnector.Connector.ConnectorColor,
      ToConnector.Connector.ModuleIndex, ToConnector.Connector.ConnectorIndex,
      ToConnector.Connector.ConnectorKind,
      ToConnector.Connector.ConnectorColor);

    FFromConnector.Invalidate;
    ToConnector.Invalidate;

    FRenderCables := True;
    OperationMode := omNormal;

  end
  else
  begin
    FFromConnector := Sender as TG2ConnectorGL;
    OperationMode := omDrawCable;
  end;
end;

function TPatchGL.ControlAt(const aX, aY: single): TControlGL;
var
  Pair: TPair<IG2Module, TG2ModuleGL>;
  MP, CP: TPointF;
  P, P2: TPoint3D;
  ModulePanel: TG2ModuleGL;
  FMXObject: TFMXObject;
  ControlGL: TControlGL;
begin
  Result := nil;

  P := Point3D(aX, aY, 0);
  P2 := P * InvMatrix3D;
  MP := PointF(P2.X, P2.Y);

  for Pair in FModuleCtrlList do
  begin
    ModulePanel := Pair.Value;
    if ModulePanel.Visible and ModulePanel.BoundsRect.IntersectsWith(ViewRect2D)
    then
    begin
      if PtInRect(ModulePanel.BoundsRect, MP) then
      begin
        CP := PointF(MP.X - ModulePanel.Position.X,
          MP.Y - ModulePanel.Position.Y);

        if assigned(ModulePanel.Children) then
          for FMXObject in ModulePanel.Children do
          begin
            if FMXObject is TControlGL then
            begin
              ControlGL := FMXObject as TControlGL;
              if ControlGL.Visible and ControlGL.HitTest then
              begin
                Result := ControlGL.ControlAt(CP.X, CP.Y);
                if Result <> nil then
                  break;
              end;
            end;
          end;

        if not assigned(Result) then
        begin
          Result := ModulePanel;
          break;
        end;
      end;
    end;
  end;

  // Check active selector seperately because its overlaps the bounds
  // of the parent control
  if assigned(ActiveSelectorList) then
  begin
    if ActiveSelectorList.Visible and
      ActiveSelectorList.AbsBoundsRect.IntersectsWith(ViewRect2D) then
    begin

      if PtInRect(ActiveSelectorList.AbsBoundsRect, MP) then
      begin
        Result := ActiveSelectorList;
      end;
    end;
  end;
end;

function TPatchGL.GetPatch: IG2Patch;
begin
  if assigned(Connection) and (SlotIndex <> -1) then
    Result := Connection.Patch[SlotIndex]
  else
    Result := nil;
end;

function TPatchGL.GetPatchPart: IG2PatchPart;
begin
  if assigned(Patch) then
    Result := Patch.PatchPart[Location]
  else
    Result := nil;
end;

function TPatchGL.GetSelectedModuleCount: integer;
var
  SelectedList: TList<IG2Module>;
begin
  if assigned(Patch) then
  begin
    SelectedList := Patch.PatchPart[Location].CreateSelectedModuleList;
    try
      Result := SelectedList.Count;
    finally
      SelectedList.Free;
    end;
  end
  else
    Result := 0;
end;

procedure TPatchGL.ModuleClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  //OperationMode := omModule;
end;

procedure TPatchGL.DoLongClk(Shift: TShiftState; X, Y: Single);
begin
  FLongClick := True;

  inherited;

  OperationMode := omModule;

  if assigned(MouseDownCtrl) and (MouseDownCtrl is TG2ModuleGL) then
    (MouseDownCtrl as TG2ModuleGL).Selected :=
      not (MouseDownCtrl as TG2ModuleGL).Selected;

  {if assigned(MouseDownCtrl) and (MouseDownCtrl is TG2ModuleGL) then
  begin
    (MouseDownCtrl as TG2ModuleGL).Selected :=
      not (MouseDownCtrl as TG2ModuleGL).Selected;

    OperationMode := omModule;
  end;}

  //if OperationMode = omPaste then
  {if (OperationMode = omModule) and (not assigned(MouseDownCtrl)) and assigned(Connection.CopyPatch) then
  begin
    Connection.PatchModulesSelectedCopy(Patch.PatchPart[Location], Connection.CopyPatch);
  end;}
end;

procedure TPatchGL.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  //if not(ssShift in Shift) then
  //  UnSelectAll;
  FLongClick := False;

  inherited;

  case OperationMode of
    omNormal:
      begin
        ConMan.SelectedCol := Trunc(LocalMouseDownPoint.X / GRID_UNITS_X);
        ConMan.SelectedRow := Trunc(LocalMouseDownPoint.Y / GRID_UNITS_Y);
        FRenderModules := True;
        FullRepaint;
      end;
  end;

  if not assigned(MouseDownCtrl) then
  begin
    if ssDouble in Shift then
    begin
      OperationMode := omNormal;
      OperationMode := omAddModule;
    end;
  end;
end;

procedure TPatchGL.MouseMove(Shift: TShiftState; X, Y: single);
begin
  inherited;

  if assigned(MouseDownCtrl) and (MouseDownCtrl is TG2ModuleGL) then
  begin
    if not ((MouseDownCtrl as TG2ModuleGL).Selected) then
      UnSelectAll;

    (MouseDownCtrl as TG2ModuleGL).Selected := True;

    if ssLeft in Shift then
    begin
      MoveSelectedPanels(MouseDelta.X, MouseDelta.Y);
      FullRepaint;
    end;

  end;

  {if OperationMode = omPaste then
  begin
    MoveSelectedPanels(MouseDelta.X, MouseDelta.Y);
    FullRepaint;
  end;}
end;

procedure TPatchGL.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  if (not FLongClick) and assigned(MouseDownCtrl)
  and (MouseDownCtrl is TG2ModuleGL) then
  begin
    if not ((MouseDownCtrl as TG2ModuleGL).Selected) then
      UnSelectAll;

    (MouseDownCtrl as TG2ModuleGL).Selected := True;
  end;

  inherited;

  case OperationMode of
    omNormal:
      begin
        {ConMan.SelectedCol := Trunc(LocalMouseDownPoint.X / GRID_UNITS_X);
        ConMan.SelectedRow := Trunc(LocalMouseDownPoint.Y / GRID_UNITS_Y);
        FRenderModules := True;
        FullRepaint;}
      end;
    omSelection:
      begin
        OperationMode := omNormal;
      end;
    omDrawCable:
      ;
    omAddModule:
      ;
    omModule:
      ;
    omParam:
      ;
    omDragSelect:
      ;
    omPianoroll:
      ;
  end;
end;

procedure TPatchGL.MoveSelectedPanels(const Dx, Dy: single);
begin
  if (Dx <> 0) or (Dy <> 0) then
  begin
    OperationMode := omSelection;
    FModuleSelection.Move(Dx, Dy);
    FRenderModules := True;
  end;
end;

procedure TPatchGL.RenderCables;
var
  Pair: TPair<IG2Cable, TG2CableGL>;
  SaveMatrix, M: TMatrix3D;
  Cable: TG2CableGL;
begin
  // Draw cables on a texture

  if Context <> nil then
  begin
    Canvas.Flush;
    if Context.BeginScene then
      try
        SaveMatrix := Context.CurrentMatrix;

        Context.SetContextState(TContextState.csScissorOff);
        Context.Clear([TClearTarget.Color, TClearTarget.Depth], 0, 1.0, 0);

        M := Matrix3D * SaveMatrix;

        for Pair in FCableCtrlList do
        begin
          Cable := Pair.Value;

          if Cable.Visible and Cable.BoundsRect.IntersectsWith(ViewRect2D) then
          begin
            if FRenderCables then
              Cable.Invalidate;
            Context.SetMatrix(TMatrix3D.CreateTranslation(Cable.Position) * M);
            Cable.Render(Context, UpdateRectList)
          end;
        end;
      finally
        Context.EndScene;
        Context.SetMatrix(SaveMatrix);
      end;

    Context.CopyToBitmap(FCableBitmap, Rect(0, 0, FCableBitmap.Width,
      FCableBitmap.Height));

    FCableMesh.CreateMesh(Context.Width, Context.Height, 0, 1, 1);

    FRenderCables := False;
  end;
end;

procedure TPatchGL.Render(aBitmap: TBitmap);
var
  Pair: TPair<IG2Module, TG2ModuleGL>;
  SaveMatrix, PosM: TMatrix3D;
  ModulePanel: TG2ModuleGL;
  R: TRectF;
  PosBG: TPoint3D;
  bsw, bsh: single;
begin
  if FRenderCables then
    RenderCables;

  if Context <> nil then
  begin
    Canvas.Flush;

    if Context.BeginScene then
      try
        SaveMatrix := Context.CurrentMatrix;

        PosM := Matrix3D * SaveMatrix;

        if FRenderModules then
        begin
          Context.SetContextState(TContextState.csScissorOff);
          Context.Clear([TClearTarget.Color, TClearTarget.Depth],
            TAlphaColorRec.Black, 1.0, 0);

          // UpdateRectList.Add(RectF(0, 0, FModuleBitmap.Width,
          // FModuleBitmap.Height));

          bsw := FBackgroundLayer.TileWidth * Scale3D.X;
          bsh := FBackgroundLayer.TileHeight * Scale3D.Y;

          if (bsw > 0) and (bsh > 0) then
          begin
            FBackgroundLayer.ColCount := Trunc(Context.Width / bsw) + 3;
            FBackgroundLayer.RowCount := Trunc(Context.Height / bsh) + 3;

            PosBG.X := Trunc(Translation3D.X) Mod Trunc(bsw);
            PosBG.Y := Trunc(Translation3D.Y) Mod Trunc(bsh);
            PosBG.Z := 0;

            Context.SetMatrix(TMatrix3D.CreateScaling(Scale3D) *
              TMatrix3D.CreateTranslation(PosBG) * SaveMatrix);
          end;

          FBackgroundLayer.Invalidate;
          FBackgroundLayer.Render(Context, UpdateRectList);
        end;

        for Pair in FModuleCtrlList do
        begin
          ModulePanel := Pair.Value;
          if ModulePanel.Visible and ModulePanel.BoundsRect.IntersectsWith
            (ViewRect2D) then
          begin
            if FRenderModules then
              ModulePanel.Invalidate;

            Context.SetMatrix
              (TMatrix3D.CreateTranslation(ModulePanel.Position) * PosM);
            ModulePanel.Render(Context, UpdateRectList)
          end;
        end;

        // Render the acive selector list seperately so it's on top of
        // all modules
        if assigned(ActiveSelectorList) then
        begin
          R := ActiveSelectorList.AbsBoundsRect;
          Context.SetMatrix(TMatrix3D.CreateTranslation(
            Point3D(R.Left, R.Top, 10)) * PosM);
          ActiveSelectorList.Render(Context, UpdateRectList)
        end;

        Context.SetMatrix(SaveMatrix);

        FCableMat.Texture := FCableBitmap.Texture;
        Context.DrawTriangles(FCableMesh.VertexBuffer, FCableMesh.IndexBuffer,
          FCableMat, 1);

        Context.SetMatrix(PosM);

        if assigned(FModuleSelection) then
          FModuleSelection.Render(Context);

        FModuleCursor.Col := ConMan.SelectedCol;
        FModuleCursor.Row := ConMan.SelectedRow;
        FModuleCursor.Render(Context);

      finally
        Context.EndScene;
        Context.SetMatrix(SaveMatrix);
      end;

    Context.CopyToBitmap(aBitmap, Rect(0, 0, aBitmap.Width, aBitmap.Height));

    FRenderModules := False;
  end;
end;

procedure TPatchGL.SelectAll;
begin
  //
end;

procedure TPatchGL.SelectorCloseUp(Sender: TObject);
begin
  ActiveSelectorList := nil;
end;

procedure TPatchGL.SelectorDropDown(Sender: TObject);
begin
  if Sender is TG2SelectorGL then
  begin
    ActiveSelectorList := (Sender as TG2SelectorGL).List;
  end;
end;

procedure TPatchGL.SetActiveSelectorList(const Value: TG2SelectorListGL);
begin
  if FActiveSelectorList <> Value then
  begin
    FRenderModules := True;
    FullRepaint;
    FActiveSelectorList := Value;
  end;
end;

procedure TPatchGL.SetConnectionIndex(const Value: integer);
begin
  if Value <> ConnectionIndex then
  begin
    ClearPatch;
    inherited;
    InitPatch;
  end;
end;

procedure TPatchGL.SetLocation(const Value: integer);
begin
  if (FLocation <> Value) and (Value < LOCATION_PATCH) then
  begin
    ClearPatch;
    FLocation := Value;
    InitPatch;
  end;
end;

procedure TPatchGL.SetMatrix3D(const Value: TMatrix3D);
begin
  inherited;
  FRenderModules := True;
  FRenderCables := True;
end;

procedure TPatchGL.SetOperationMode(const Value: TOperationMode);
var
  ChangeList: TModuleChangeList;
  ModuleChange: TModuleChange;
  SelectedList: TList<IG2Module>;
  Module: IG2Module;
begin
  if FOperationMode <> Value then
  begin
    case FOperationMode of
      omNormal, omAddModule:
        begin
          if assigned(FModuleSelection) then
            FreeAndNil(FModuleSelection);

          FModuleCursor.Visible := False;
        end;
      omSelection:
        begin
          if assigned(FModuleSelection) then
          begin

            ChangeList := TModuleChangeList.Create;
            try
              SelectedList := Patch.PatchPart[Location].CreateSelectedModuleList;
              try
                for Module in SelectedList do
                begin
                  ModuleChange := TModuleChange.Create(
                    Module.ModuleIndex,
                    Module.ModuleName,
                    Module.ModuleFileName,
                    Module.Row,
                    Module.Col,
                    Module.HeightUnits);

                  ModuleChange.NewRow :=
                    Max(0, Module.Row + FModuleSelection.DeltaUnitsRow);
                  ModuleChange.NewCol :=
                    Max(0, Module.Col + FModuleSelection.DeltaUnitsCol);

                  ChangeList.Add(ModuleChange);
                end;

              finally
                SelectedList.Free;
              end;

              Connection.PatchModulesSelectedMove(
                Patch.PatchPart[Location],
                ChangeList);

            finally
              ChangeList.Free;
            end;

            FreeAndNil(FModuleSelection);
          end;
        end;
    end;

    FOperationMode := Value;
    Slot.OperationMode := FOperationMode;

    case FOperationMode of
      omNormal:
        begin
          FModuleCursor.Visible := MouseDownCtrl = nil;
        end;
      omAddModule:
        begin
          //ConMan.SelectedCol := Trunc(LocalMouseDownPoint.X / GRID_UNITS_X);
          //ConMan.SelectedRow := Trunc(LocalMouseDownPoint.Y / GRID_UNITS_Y);
        end;
      omSelection:
        begin
          if not assigned(FModuleSelection) then
          begin
            SelectedList := Patch.PatchPart[Location].CreateSelectedModuleList;
            try
              FModuleSelection := TSelectionGL.Create(SelectedList);
            finally
              SelectedList.Free;
            end;
          end;
        end;
      {omPaste:
        begin
          //MouseDownCtrl := nil;
          if assigned(Connection.CopyPatch) then
          begin
            SelectedList := Connection.CopyPatch.CreateSelectedModuleList;
            try
              FModuleSelection := TSelectionGL.Create(SelectedList);
            finally
              SelectedList.Free;
            end;
          end;
        end;}
    end;

    FRenderModules := True;
    FRenderCables := True;
    FullRepaint;
  end;
end;

procedure TPatchGL.SetSlotIndex(const Value: integer);
begin
  if Value <> SlotIndex then
  begin
    ClearPatch;
    inherited;
    InitPatch;
  end;
end;

procedure TPatchGL.SetTextureList(const Value: TTextureListGL);
begin
  inherited;

  if assigned(TextureList) then
  begin
    FBackgroundLayer.TextureList := TextureList;
    FBackgroundLayer.TextureID := 'RackRails';
  end;
end;

procedure TPatchGL.UnSelectAll;
var
  Pair: TPair<IG2Module, TG2ModuleGL>;
begin
  for Pair in FModuleCtrlList do
    Pair.Value.Selected := False;
end;

// -----------------------------------------------------------------------------
//
//                             TSelectionGL
//
// -----------------------------------------------------------------------------

constructor TSelectionGL.Create(aModuleList: TList<IG2Module>);
var
  Rect, MRect: TRectF;
  Module: IG2Module;
  Outline: TOutlineGL;
  First: Boolean;
begin
  First := True;

  for Module in aModuleList do
  begin
    MRect := ModuleRect(Module.Col, Module.Row, Module.HeightUnits);

    if First then
    begin
      First := False;
      Rect := MRect;
    end
    else
      Rect := TRectF.Union(Rect, MRect);
  end;

  FPosition := Point3D(Rect.Left, Rect.Top, 0);
  FStartPosition := FPosition;
  FWidth := Rect.Width;
  FHeight := Rect.Height;

  FOutlineList := TObjectList<TOutlineGL>.Create(True);

  for Module in aModuleList do
  begin
    MRect := ModuleRect(Module.Col, Module.Row, Module.HeightUnits);

    Outline := TOutlineGL.Create(
      Point3D(
        MRect.Left - Rect.Left,
        MRect.Top - Rect.Top,
        0),
      MRect.Width,
      MRect.Height);

    FOutlineList.Add(Outline)
  end;
end;

constructor TSelectionGL.Create(const aCol, aRow, aHeight: integer);
var
  Rect: TRectF;
  Outline: TOutlineGL;
begin
  Rect := ModuleRect(aCol, aRow, aHeight);

  FPosition := Point3D(Rect.Left, Rect.Top, 0);
  FStartPosition := FPosition;
  FWidth := Rect.Width;
  FHeight := Rect.Height;

  FOutlineList := TObjectList<TOutlineGL>.Create(True);

  Outline := TOutlineGL.Create(
    Point3D(
      Rect.Left - Rect.Left,
      Rect.Top - Rect.Top,
      0),
    Rect.Width,
    Rect.Height);

  FOutlineList.Add(Outline)
end;

destructor TSelectionGL.Destroy;
begin
  FOutlineList.Free;
  inherited;
end;

function TSelectionGL.GetDeltaUnitsCol: integer;
begin
  if FPosition.X - FStartPosition.X < 0 then
    Result := Trunc((FPosition.X - FStartPosition.X - (GRID_UNITS_X / 2)) /
      GRID_UNITS_X)
  else
    Result := Trunc((FPosition.X - FStartPosition.X) / GRID_UNITS_X);
end;

function TSelectionGL.GetDeltaUnitsRow: integer;
begin
  if FPosition.Y - FStartPosition.Y < 0 then
    Result := Trunc((FPosition.Y - FStartPosition.Y - (GRID_UNITS_Y / 2)) /
      GRID_UNITS_Y)
  else
    Result := Trunc((FPosition.Y - FStartPosition.Y) / GRID_UNITS_Y);
end;

procedure TSelectionGL.Move(Dx, Dy: single);
begin
  FPosition.X := FPosition.X + Dx;
  FPosition.Y := FPosition.Y + Dy;
end;

procedure TSelectionGL.Render(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  Outline: TOutlineGL;
begin
  SaveMatrix := aContext.CurrentMatrix;
  try
    aContext.SetMatrix(TMatrix3D.CreateTranslation(FPosition) * SaveMatrix);

    for Outline in FOutlineList do
      Outline.Render(aContext);

  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

procedure TSelectionGL.SetHeight(const Value: single);
begin
  FHeight := Value;
end;

procedure TSelectionGL.SetPosition(const Value: TPoint3D);
begin
  FPosition := Value;
  FStartPosition := FPosition;
end;

procedure TSelectionGL.SetWidth(const Value: single);
begin
  FWidth := Value;
end;

// -----------------------------------------------------------------------------
//
//                           TModuleCursorGL
//
// -----------------------------------------------------------------------------

constructor TModuleCursorGL.Create(const aCol, aRow, aHeight: integer);
var
  Rect: TRectF;
begin
  FCol := aCol;
  FRow := aRow;
  FHeight := aHeight;

  FVisible := True;
end;

destructor TModuleCursorGL.Destroy;
begin
  inherited;
end;

procedure TModuleCursorGL.Render(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
  Rect: TRectF;
  Cursor: TCursorGL;
begin
  if not FVisible then
    exit;

  Rect := ModuleRect(FCol, FRow, FHeight);

  Cursor := TCursorGL.Create(
    Point3D(Rect.Left, Rect.Top, 0),
    Rect.Width, Rect.Height);
  try
    Cursor.Render(aContext);
  finally
    Cursor.Free;
  end;
end;

procedure TModuleCursorGL.SetCol(const Value: integer);
begin
  FCol := Value;
end;

procedure TModuleCursorGL.SetHeight(const Value: integer);
begin
  FHeight := Value;
end;

procedure TModuleCursorGL.SetRow(const Value: integer);
begin
  FRow := Value;
end;

procedure TModuleCursorGL.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;


end.
