unit UnitSlot;

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
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.UIConsts, System.Generics.Collections, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Edit, FMX.Gestures, FMX.Objects, FMX.TabControl,
{$IF Defined(VER260) or Defined(VER270)}
  FMX.Graphics,
{$ENDIF}
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2GraphFMX, BVE.NMG2ControlsFMX,
  UnitAppSettings, UnitPianoRoll, UnitAddCable;

type
  TMiniVUClickEvent = procedure(Sender: TObject; aVUMeter : TG2MiniVU) of object;

  TOperationMode = (omNormal, omSelection, omDrawCable, omAddModule, omModule,
      omParam, omDragSelect, omPianoroll);
  TSelectionMode = (smMove, smCopy);

  TSelection = class(TControl)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FStartX, FStartY,
    FMouseX, FMouseY : single;
    FSelectionMode : TSelectionMode;
  protected
    function GetDeltaUnitsCol: integer;
    function GetDeltaUnitsRow: integer;
    function GetActive: boolean;
    procedure SetActive(const aValue : boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CalcNewPositions;

    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; AbsX, AbsY : single); virtual;

    procedure Paint; override;
    procedure PaintOn( aCanvas : TCanvas); //override;

    property Active : boolean read GetActive write SetActive;
    property DeltaUnitsCol : integer read GetDeltaUnitsCol;
    property DeltaUnitsRow : integer read GetDeltaUnitsRow;
  end;

  TModuleCursor = class(TBufferedControl)
  private
    FMargin : single;
    FRow, FCol, FUnitsHeight : integer;
    FActive : boolean;
    procedure SetActive(const Value: boolean);

    procedure SetCol(const Value: integer);
    procedure SetRow(const Value: integer);
    procedure SetUnitsHeight(const Value: integer);
    function GetCursorPos: TRect;
    procedure SetCursorPos(const Value: TRect);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetModuleLocation(const aModule : TG2FileModule);
    procedure ScrollInView;

    procedure PaintOn( aCanvas : TCanvas); override;

    property CursorPos : TRect read GetCursorPos write SetCursorPos;
    property Row : integer read FRow write SetRow;
    property Col : integer read FCol write SetCol;
    property UnitsHeight : integer read FUnitsHeight write SetUnitsHeight;
    property Active : boolean read FActive write SetActive;
  end;

  TDragSelector = class(TBufferedControl)
  private
    FMargin : single;
    FActive : boolean;
    FStartPoint: TPointF;
    FDragPoint: TPointF;
    procedure CalcBounds;
    procedure SetActive(const Value: boolean);
    procedure SetDragPoint(const Value: TPointF);
    procedure SetStartPoint(const Value: TPointF);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PaintOn( aCanvas : TCanvas); override;

    function InSelection(X, Y : single): boolean;

    property StartPoint : TPointF read FStartPoint write SetStartPoint;
    property DragPoint : TPointF read FDragPoint write SetDragPoint;
    property Active : boolean read FActive write SetActive;
  end;

  TframeSlot = class(TFrame, IG2Observer)
    sbVA: TScrollBox;
    sbFX: TScrollBox;
    lSizeVA: TLayout;
    lSizeFX: TLayout;
    Layout1: TLayout;
    lFX: TLayout;
    Image1: TImage;
    lVA: TLayout;
    Image2: TImage;
    Splitter1: TSplitter;
    Rectangle1: TRectangle;
    procedure btParamDeassignCCMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure tcPatchFunctionsChange(Sender: TObject);
  private
    [Weak] FModule : TG2GraphModuleFMX;
    [Weak] FMouseOverControl : TG2BaseControl;
    [Weak] FMouseOverModule : TG2GraphModuleFMX;

    FMouseOverLocation : TLocationType;

    FLastX, FLastY : single;

    [Weak] FframeAppSettings : TframeAppSettings;

    FframePianoroll : TframePianoRoll;
    FframeAddCable : TframeAddCable;

    [Weak] FStateStyleList : TG2StateStyleList;

    FZoom : single;
    FlZoomVA : TG2BufferedLayout;
    FlZoomFX : TG2BufferedLayout;

    [Weak] FPatch : TG2GraphPatchFMX;
    FCopyLocation : TLocationType;
    FVariation : integer;

    FSelectedLocation : TLocationType;

    FPrevOperationMode,
    FOperationMode : TOperationMode;

    FPanEnabled : boolean;

    FModuleCursor : TModuleCursor;
    FCursorVARect,
    FCursorFXRect : TRect;

    FDragSelector : TDragSelector;

    FSelectionVA,
    FSelectionFX : TSelection;
    FSelectionActive : boolean;
    FSelectionMode : TSelectionMode;

    FOnOperationModeChange : TNotifyEvent;
    FOnMiniVUClick : TMiniVUClickEvent;

    procedure SetPatch(const Value: TG2GraphPatchFMX);
    procedure SetSelectedModule(const Value: TG2GraphModuleFMX);
    procedure SetSelectedLocation(const Value : TLocationType);
    procedure SetMouseOverControl(const Value: TG2BaseControl);
    procedure SetMouseOverLocation(const Value: TLocationType);
    procedure SetMouseOverModule(const Value: TG2GraphModuleFMX);
    procedure SetOperationMode(const Value: TOperationMode);

    procedure SetframeAppSettings(const Value: TframeAppSettings);
    procedure SetVariation(const Value: integer);
    function GetSelectedControl: TG2BaseControl;

    procedure SetSelectedControl(const Value: TG2BaseControl);
    function GetSelectedLocation: TLocationType;
    function GetPatchPart: TG2GraphPatchPartFMX;

    procedure CreateModuleFMX(Sender : TObject; aLocation : TLocationType; aModule : TG2GraphModuleFMX);
    procedure CreateCableFMX(Sender : TObject; aLocation : TLocationType; aCable : TG2GraphCableFMX);
    procedure CreateG2Control(Sender : TObject; aControl : TG2BaseControl);

    procedure SetSelectMultiple(const Value: boolean);
    function GetSelectMultiple: boolean;  public

    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Update( aG2Event : TG2Event);
    procedure RemoveReference( aData : IG2Subject);

    procedure CalcLayoutDimensions;
    procedure SetZoomVA(const aValue : single);
    procedure SetZoomFX(const aValue : single);
    procedure FocusUnitsRect(const FocusRect : TRect);

    procedure UpdateZoom;
    procedure UpdateSplitter;
    procedure UpdateControls;
    procedure PreparePatchUpdate;
    procedure PatchUpdate;

    procedure UpdateModuleInfo( aModule : TG2GraphModuleFMX);

    procedure EnablePan( sb : TScrollbox);
    procedure DisablePan( sb : TScrollbox);

    procedure ActivateSelection( aPatchPart : TG2FilePatchPart; aSelectionMode : TSelectionMode; MouseX, MouseY : single);
    procedure DeactivateSelection;
    procedure ActivateDragSelect( sb : TScrollbox; X, Y : single);
    procedure DeactivateDragSelect;
    procedure ActivateDrawCable;
    procedure DeactivateDrawCable;
    procedure ActivateModule;
    procedure DeactivateModule;
    procedure ActivateAddModule;
    procedure DeactivateAddModule;
    procedure ActivateModuleCursor;
    procedure ActivateParameter;
    procedure ActivatePianoroll;
    procedure FinishOperation;

    procedure FocusModuleLeft;
    procedure FocusModuleRight;
    procedure FocusModuleAbove;
    procedure FocusModuleUnder;

    procedure SaveCursorPos;

    procedure ClosePianoroll( Sender : TObject);
    procedure SlotDeleteModule(Sender : TObject; SenderID : integer; Location: TLocationType; ModuleIndex : integer);

    procedure CloseDrawCable( Sender : TObject);
    procedure DeleteAllCables;

    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ControlGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);

    procedure ModuleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure ModuleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure ModuleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure sbMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure sbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure sbMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property frameAppSettings : TframeAppSettings read FframeAppSettings write SetframeAppSettings;

    property Patch : TG2GraphPatchFMX read FPatch write SetPatch;
    property PatchPart : TG2GraphPatchPartFMX read GetPatchPart;
    property Variation : integer read FVariation write SetVariation;

    property SelectedControl : TG2BaseControl read GetSelectedControl write SetSelectedControl;
    property SelectedModule : TG2GraphModuleFMX read FModule write SetSelectedModule;
    property SelectedLocation : TLocationType read GetSelectedLocation write SetSelectedLocation;
    property SelectMultiple : boolean read GetSelectMultiple write SetSelectMultiple;

    property ModuleCursor : TModuleCursor read FModuleCursor;
    property DragSelector : TDragSelector read FDragSelector;

    property MouseOverControl : TG2BaseControl read FMouseOverControl write SetMouseOverControl;
    property MouseOverModule : TG2GraphModuleFMX read FMouseOverModule write SetMouseOverModule;
    property MouseOverLocation : TLocationType read FMouseOverLocation write SetMouseOverLocation;

    property OperationMode : TOperationMode read FOperationMode write SetOperationMode;

    property OnOperationModeChange : TNotifyEvent read FOnOperationModeChange write FOnOperationModeChange;
    property OnMiniVUClick : TMiniVUClickEvent read FOnMiniVUClick write FOnMiniVUClick;
  end;

implementation

{$R *.fmx}

constructor TframeSlot.Create(AOwner: TComponent);
begin
  inherited;

  FlZoomVA := TG2BufferedLayout.Create(self);
  FlZoomVA.Parent := lSizeVA;

  FlZoomVA.Buffered := False;

  sbVA.AutoCapture := True;

  FlZoomVA.OnControlMouseDown := ControlMouseDown;
  FlZoomVA.OnControlMouseMOve := ControlMouseMove;
  FlZoomVA.OnControlMouseUp := ControlMouseUp;
  FlZoomVA.OnModuleMouseDown := ModuleMouseDown;
  FlZoomVA.OnModuleMouseMOve := ModuleMouseMove;
  FlZoomVA.OnModuleMouseUp := ModuleMouseUp;
  sbVA.OnMouseDown := sbMouseDown;
  sbVA.OnMouseMove := sbMouseMove;
  sbVA.OnMouseUp := sbMouseUp;

  sbVA.AniCalculations.Animation := True;
  sbVA.AniCalculations.BoundsAnimation := True;
  sbVA.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

  FlZoomFX := TG2BufferedLayout.Create(self);
  FlZoomFX.Parent := lSizeFX;

  FlZoomFX.Buffered := False;

  FlZoomFX.OnControlMouseDown := ControlMouseDown;
  FlZoomFX.OnControlMouseMOve := ControlMouseMove;
  FlZoomFX.OnControlMouseUp := ControlMouseUp;
  FlZoomFX.OnModuleMouseDown := ModuleMouseDown;
  FlZoomFX.OnModuleMouseMOve := ModuleMouseMove;
  FlZoomFX.OnModuleMouseUp := ModuleMouseUp;
  sbFX.OnMouseDown := sbMouseDown;
  sbFX.OnMouseMove := sbMouseMove;
  sbFX.OnMouseUp := sbMouseUp;

  sbFX.AniCalculations.Animation := True;
  sbFX.AniCalculations.BoundsAnimation := True;
  sbFX.AniCalculations.TouchTracking := [ttVertical, ttHorizontal];

  FPanEnabled := True;

  FPatch := nil;
  //FCopyPatch := nil;

  FSelectionVA := TSelection.Create(self);
  FSelectionVA.Parent := FlZoomVA;
  FSelectionVA.Active := False;

  FSelectionFX := TSelection.Create(self);
  FSelectionFX.Parent := FlZoomFX;
  FSelectionFX.Active := False;

  FModuleCursor := TModuleCursor.Create(self);
  FModuleCursor.Active := False;
  FModuleCursor.Parent := FlZoomVA;

  FCursorFXRect := Rect(0, 0, 1, 1);
  FCursorVARect := Rect(0, 0, 1, 1);

  FDragSelector := TDragSelector.Create(self);
  FDragSelector.Active := False;
  FDragSelector.Parent := FlZoomVA;

  FZoom := 1;

  FPrevOperationMode := omNormal;
  OperationMode := omNormal;
end;

destructor TframeSlot.Destroy;
begin
  inherited;
end;

procedure TframeSlot.SetPatch(const Value: TG2GraphPatchFMX);
var Module : TG2FileModule;
    Cable : TG2FileCable;
begin
  if Value <> FPatch then begin

    if assigned(FPatch) then begin

      for Module in FPatch.PatchPart[ord(ltFX)].ModuleList do begin
        (Module as TG2GraphModuleFMX).FreePanel;
      end;
      for Module in FPatch.PatchPart[ord(ltVA)].ModuleList do begin
        (Module as TG2GraphModuleFMX).FreePanel;
      end;
      for Cable in FPatch.PatchPart[ord(ltFX)].CableList do begin
        (Cable as TG2GraphCableFMX).G2Control.Free;
      end;
      for Cable in FPatch.PatchPart[ord(ltVA)].CableList do begin
        (Cable as TG2GraphCableFMX).G2Control.Free;
      end;

      FPatch.RemoveObserver(self);
      if assigned(FPatch.Slot) then begin
        FPatch.Slot.RemoveObserver(self);
        if assigned(FPatch.Slot.Performance) then
          FPatch.Slot.Performance.RemoveObserver(Self);
        if assigned(FPatch.G2) then
          FPatch.G2.RemoveObserver(Self);
      end;
      FPatch.OnCreateModuleFMX := nil;
      FPatch.OnCreateCableFMX := nil;

      FSelectionVA.FPatchPart := nil;
      FSelectionFX.FPatchPart := nil;

      (FPatch.Slot as TG2GraphSlotFMX).OnDeleteModule := nil;
    end;

    FPatch := Value;

    if assigned(FPatch) then begin
      FPatch.RegisterObserver(self);
      if assigned(FPatch.Slot) then begin
        FPatch.Slot.RegisterObserver(self);
        if assigned(FPatch.Slot.Performance) then
          FPatch.Slot.Performance.RegisterObserver(Self);
        if assigned(FPatch.G2) then
          FPatch.G2.RegisterObserver(Self);
      end;

      FPatch.OnCreateModuleFMX := CreateModuleFMX;
      FPatch.OnCreateCableFMX := CreateCableFMX;

      for Module in FPatch.PatchPart[ord(ltFX)].ModuleList do begin
        CreateModuleFMX( self, ltFX, (Module as TG2GraphModuleFMX));
      end;
      for Module in FPatch.PatchPart[ord(ltVA)].ModuleList do begin
        CreateModuleFMX( self, ltVA, (Module as TG2GraphModuleFMX));
      end;
      for Cable in FPatch.PatchPart[ord(ltFX)].CableList do begin
        CreateCableFMX( self, ltFX, (Cable as TG2GraphCableFMX));
      end;
      for Cable in FPatch.PatchPart[ord(ltVA)].CableList do begin
        CreateCableFMX( self, ltVA, (Cable as TG2GraphCableFMX));
      end;

      FSelectionVA.FPatchPart := FPatch.PatchPart[ ord(ltVA)];
      FSelectionFX.FPatchPart := FPatch.PatchPart[ ord(ltFX)];

      (FPatch.Slot as TG2GraphSlotFMX).OnDeleteModule := SlotDeleteModule;

      if FPatch.SelectedLocation in [ltFX, ltVA] then
        FSelectedLocation := FPatch.SelectedLocation
      else
        FSelectedLocation := ltVA;
    end;

    PatchUpdate;
  end;
end;

procedure TframeSlot.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange: ;
    EvtUSBError: ;
    EvtBeforeSendMessage: ;
    EvtReceiveResponseMessage: ;
    EvtNextInitStep: ;
    EvtAfterG2Init:
      begin
        CalcLayoutDimensions;
      end;
    EvtAfterPerfInit: ;
    EvtAfterSlotInit: ;
    EvtPerfsSettingsUpdate: ;
    EvtPerfUpdate: ;
    EvtSynthSettingsUpdate: ;
    EvtBeforePatchUpdate:
      begin
        PreparePatchUpdate;
      end;
    EvtPatchUpdate:
      begin
        Patch.SetCablesVisible( 0, Patch.PatchDescription.RedVisible = 1);
        Patch.SetCablesVisible( 1, Patch.PatchDescription.BlueVisible = 1);
        Patch.SetCablesVisible( 2, Patch.PatchDescription.YellowVisible = 1);
        Patch.SetCablesVisible( 3, Patch.PatchDescription.OrangeVisible = 1);
        Patch.SetCablesVisible( 4, Patch.PatchDescription.GreenVisible = 1);
        Patch.SetCablesVisible( 5, Patch.PatchDescription.PurpleVisible = 1);
        Patch.SetCablesVisible( 6, Patch.PatchDescription.WhiteVisible = 1);

        UpdateControls;
      end;
    EvtVariationChange:
      begin
        Variation := Patch.ActiveVariation;
      end;
    EvtCopyVariation: ;
    EvtMidiClockReceive: ;
    EvtClockRunChange: ;
    EvtClockBPMChange: ;
    EvtMidiCCRecieve: ;
    EvtAfterGetAssignedVoices: ;
    EvtPatchLoadChange: ;
    EvtSelectSlot:
      begin
        UpdateControls;
      end;
    EvtSelectLocation:
      begin
        if Patch.SelectedLocation in [ltFX, ltVA] then
          SelectedLocation := Patch.SelectedLocation;
        UpdateControls;
      end;
    EvtSelectModule: ;
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtDeleteModule: ;
    EvtAfterRetreivePatch: ;
    EvtAfterBankList: ;
    EvtAfterStore: ;
    EvtAfterClear: ;
    EvtAfterClearBank: ;
    EvtAfterBankDownload: ;
    EvtDeassignKnob: ;
    EvtAssignKnob: ;
    EvtDeassignGlobalKnob: ;
    EvtAssignGlobalKnob: ;
  end;
end;

procedure TframeSlot.CreateModuleFMX(Sender: TObject;
  aLocation : TLocationType; aModule: TG2GraphModuleFMX);
begin
  case aLocation of
  ltPatch:;
  ltFX:
    begin
       aModule.G2Module := TG2Module.Create(aModule);
       aModule.G2Module.Parent := FlZoomFX;
       aModule.InitPanel;
       aModule.G2Module.SendToBack;
       aModule.G2Module.OnMouseUp := FlZoomFX.OnModuleMouseUp;
       aModule.G2Module.OnMouseDown := FlZoomFX.OnModuleMouseDown;
       aModule.G2Module.OnMouseMove := FlZoomFX.OnModuleMouseMove;
       aModule.OnCreateG2Control := CreateG2Control;
       CreateModuleControls(aModule, aModule.G2Module, True, True);
       aModule.InvalidateControl;
    end;
  ltVA:
    begin
       aModule.G2Module := TG2Module.Create(aModule);
       aModule.G2Module.Parent := FlZoomVA;
       aModule.G2Module.SendToBack;
       aModule.InitPanel;
       aModule.G2Module.OnMouseUp := FlZoomVA.OnModuleMouseUp;
       aModule.G2Module.OnMouseDown := FlZoomVA.OnModuleMouseDown;
       aModule.G2Module.OnMouseMove := FlZoomVA.OnModuleMouseMove;
       aModule.OnCreateG2Control := CreateG2Control;
       CreateModuleControls(aModule, aModule.G2Module, True, True);
       aModule.InvalidateControl;
    end;
  end;
end;

procedure TframeSlot.CreateCableFMX(Sender: TObject; aLocation : TLocationType;
  aCable: TG2GraphCableFMX);
begin
  case aLocation of
  ltPatch:;
  ltFX:
    begin
      aCable.G2Control := TG2Cable.Create( aCable);
      aCable.G2Control.Parent := FlZoomFX;
      aCable.G2Control.CableStyle := FframeAppSettings.CableStyle;
      aCable.InitCable;
    end;
  ltVA:
    begin
      aCable.G2Control := TG2Cable.Create( aCable);
      aCable.G2Control.Parent := FlZoomVA;
      aCable.G2Control.CableStyle := FframeAppSettings.CableStyle;
      aCable.InitCable;
    end;
  end;
end;

procedure TframeSlot.CreateG2Control(Sender: TObject; aControl: TG2BaseControl);
begin
  if Sender is TG2GraphModuleFMX then begin
    case (Sender as TG2GraphModuleFMX).Location of
    ltPatch:;
    ltFX:
      begin
        aControl.OnGesture := FlZoomFX.OnControlGesture;
        aControl.OnMouseUp := FlZoomFX.OnControlMouseUp;
        aControl.OnMouseDown := FlZoomFX.OnControlMouseDown;
        aControl.OnMouseMove := FlZoomFX.OnControlMouseMove;
      end;
    ltVA:
      begin
        aControl.OnGesture := FlZoomVA.OnControlGesture;
        aControl.OnMouseUp := FlZoomVA.OnControlMouseUp;
        aControl.OnMouseDown := FlZoomVA.OnControlMouseDown;
        aControl.OnMouseMove := FlZoomVA.OnControlMouseMove;
      end;
    end;
  end;
end;

procedure TframeSlot.SetframeAppSettings(const Value: TframeAppSettings);
begin
  FframeAppSettings := Value;
  if assigned(FFrameAppSettings) then begin
    UpdateZoom;
  end;
end;

//==============================================================================
//
//                               Metrics
//
//==============================================================================

procedure TframeSlot.CalcLayoutDimensions;
var Rect : TRect;
begin
  if assigned(FPatch) then begin
    if assigned(FlZoomVA) then begin
      Rect := FPatch.ModuleList[ord(ltVA)].UnitsRect;
      FlZoomVA.SetBounds(0, 0, (Rect.Right + 4) * (UNITS_COL+UNIT_MARGIN*2), (Rect.Bottom + 20) * (UNITS_ROW+UNIT_MARGIN*2));
      lSizeVA.Width := FlZoomVA.Width * FZoom;
      lSizeVA.Height := FlZoomVA.Height * FZoom;
    end;
    if assigned(FlZoomFX) then begin
      Rect := FPatch.ModuleList[ord(ltFX)].UnitsRect;
      FlZoomFX.SetBounds(0, 0, (Rect.Right + 4) * (UNITS_COL+UNIT_MARGIN*2), (Rect.Bottom + 20) * (UNITS_ROW+UNIT_MARGIN*2));
      lSizeFX.Width := FlZoomFX.Width * FZoom;
      lSizeFX.Height := FlZoomFX.Height * FZoom;
    end;
  end;
end;

procedure TframeSlot.SetZoomVA(const aValue: single);
var mx, my : single;
begin
 // Calc absolute middle of currently visible rectangle of scrollbox

  mx := (sbVA.ViewportPosition.X + sbVA.Width/2) / FZoom;
  my := (sbVA.ViewportPosition.Y + sbVA.Height/2) / FZoom;

  FZoom := aValue;

  FlZoomVA.Zoom := FZoom;

  lSizeVA.Width := FlZoomVA.Width * FZoom;
  lSizeVA.Height := FlZoomVA.Height * FZoom;

  // Adjust scrollbox to keep centered on original mid point
  sbVA.ViewportPosition := PointF( mx * FZoom - (sbVA.Width/2), my * FZoom - (sbVA.Height/2));
end;

procedure TframeSlot.SetZoomFX(const aValue: single);
var mx, my : single;
begin
 // Calc absolute middle of currently visible rectangle of scrollbox

  mx := (sbFX.ViewportPosition.X + sbFX.Width/2) / FZoom;
  my := (sbFX.ViewportPosition.Y + sbFX.Height/2) / FZoom;

  FZoom := aValue;

  FlZoomFX.Zoom := FZoom;

  lSizeFX.Width := FlZoomFX.Width * FZoom;
  lSizeFX.Height := FlZoomFX.Height * FZoom;

  // Adjust scrollbox to keep centered on original mid point
  sbFX.ViewportPosition := PointF( mx * FZoom - (sbFX.Width/2), my * FZoom - (sbFX.Height/2));
end;

procedure TframeSlot.UpdateZoom;
begin
  if not assigned(FPatch) then
    exit;

  if not assigned(FFrameAppSettings) then
    exit;

  case FPatch.ZoomSetting of
  0 : begin
        SetZoomFX( FFrameAppSettings.ZoomSetting1);
        SetZoomVA( FFrameAppSettings.ZoomSetting1);
      end;
  1 : begin
        SetZoomFX( FFrameAppSettings.ZoomSetting2);
        SetZoomVA( FFrameAppSettings.ZoomSetting2);
      end;
  2 : begin
        SetZoomFX( FFrameAppSettings.ZoomSetting3);
        SetZoomVA( FFrameAppSettings.ZoomSetting3);
      end;
  end;
end;

procedure TframeSlot.FocusModuleAbove;
var Module : TG2FileModule;
begin
  if assigned(GetPatchPart.FocusedModule) then begin
    FModuleCursor.SetModuleLocation(GetPatchPart.FocusedModule);
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row - 1, FModuleCursor.Col);
  end else begin
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row - 1, FModuleCursor.Col);
  end;

  if FModuleCursor.Row > 0 then begin
    if assigned(Module) then begin
      SelectedModule := Module as TG2GraphModuleFMX;
      GetPatchPart.FocusedModule := SelectedModule;
      FModuleCursor.Row := Module.Row;
      FMOduleCursor.UnitsHeight := Module.HeightUnits;
      ActivateModule;
    end else begin
      ActivateAddModule;
      GetPatchPart.FocusedModule := nil;
      FModuleCursor.Row := FModuleCursor.Row - 1;
      FModuleCursor.UnitsHeight := 1;
    end;
  end;
  FModuleCursor.ScrollInView;
  SaveCursorPos;
end;

procedure TframeSlot.FocusModuleLeft;
var Module : TG2FileModule;
begin
  if assigned(GetPatchPart.FocusedModule) then begin
    FModuleCursor.SetModuleLocation(GetPatchPart.FocusedModule);
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row, FModuleCursor.Col - 1);
  end else begin
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row, FModuleCursor.Col - 1);
  end;

  if FModuleCursor.Col > 0 then begin
    if assigned(Module) then begin
      SelectedModule := Module as TG2GraphModuleFMX;
      GetPatchPart.FocusedModule := SelectedModule;
      FModuleCursor.Col := Module.Col;
      FMOduleCursor.UnitsHeight := Module.HeightUnits;
      ActivateModule;
    end else begin
      ActivateAddModule;
      GetPatchPart.FocusedModule := nil;
      FModuleCursor.Col := FModuleCursor.Col - 1;
      FModuleCursor.UnitsHeight := 1;
    end;
  end;
  FModuleCursor.ScrollInView;
  SaveCursorPos;
end;

procedure TframeSlot.FocusModuleRight;
var Module : TG2FileModule;
begin
  if assigned(GetPatchPart.FocusedModule) then begin
    FModuleCursor.SetModuleLocation(GetPatchPart.FocusedModule);
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row, FModuleCursor.Col + 1);
    FModuleCursor.Col := FModuleCursor.Col + 1;
  end else begin
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row, FModuleCursor.Col + 1);
    FModuleCursor.Col := FModuleCursor.Col + 1;
  end;

  if assigned(Module) then begin
    SelectedModule := Module as TG2GraphModuleFMX;
    GetPatchPart.FocusedModule := SelectedModule;
    FModuleCursor.Col := Module.Col;
    FModuleCursor.UnitsHeight := Module.HeightUnits;
    ActivateModule;
  end else begin
    ActivateAddModule;
    FModuleCursor.UnitsHeight := 1;
    GetPatchPart.FocusedModule := nil;
  end;
  FModuleCursor.ScrollInView;
  SaveCursorPos;
end;

procedure TframeSlot.FocusModuleUnder;
var Module : TG2FileModule;
begin
  if assigned(GetPatchPart.FocusedModule) then begin
    FModuleCursor.SetModuleLocation(GetPatchPart.FocusedModule);
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row + FModuleCursor.UnitsHeight, FModuleCursor.Col);
    FModuleCursor.Row := FModuleCursor.Row + FModuleCursor.UnitsHeight;
  end else begin
    Module := GetPatchPart.ModuleList.ModuleAtLocation(FModuleCursor.Row + 1, FModuleCursor.Col);
    FModuleCursor.Row := FModuleCursor.Row + 1;
  end;

  if assigned(Module) then begin
    SelectedModule := Module as TG2GraphModuleFMX;
    GetPatchPart.FocusedModule := SelectedModule;
    FModuleCursor.Row := Module.Row;
    FModuleCursor.UnitsHeight := Module.HeightUnits;
    ActivateModule;
  end else begin
    ActivateAddModule;
    FModuleCursor.UnitsHeight := 1;
    GetPatchPart.FocusedModule := nil;
  end;
  FModuleCursor.ScrollInView;
  SaveCursorPos;
end;

procedure TframeSlot.FocusUnitsRect(const FocusRect: TRect);
var mx, my, px, py : single;
begin
  mx := (FocusRect.Left + (FocusRect.Right - FocusRect.Left)/2) * (UNITS_COL+UNIT_MARGIN*2);
  my := (FocusRect.Top + (FocusRect.Bottom - FocusRect.Top)/2) * (UNITS_ROW+UNIT_MARGIN*2);

  px := System.Math.Max(0, mx * FZoom - (sbVA.Width/2));
  py := System.Math.Max(0, my * FZoom - (sbVA.Height/2));
  sbVA.ViewportPosition := PointF( px, py);
end;

procedure TframeSlot.UpdateControls;
begin
  if not FPatch.SplitterVisible then begin
    if SelectedLocation = ltVA then begin
      lVA.Visible := True;
      lFX.Visible := False;
      CalcLayoutDimensions;
    end else
      if SelectedLocation = ltFX then begin
        lVA.Visible := False;
        lFX.Visible := True;
        CalcLayoutDimensions;
      end;
  end else
    CalcLayoutDimensions;

  FPatch.InvalidateParameters;
  FPatch.InvalidateConnectors;
  UpdateZoom;

  if assigned(FframePianoRoll) then
    FframePianoRoll.UpdateControls;

  if assigned(FframeAddCable) then
    FframeAddCable.UpdateControls;
end;

procedure TframeSlot.PatchUpdate;
begin
  // Must be called if a patch is loaded
  SelectedModule := nil;
  //SelectedControl := nil;
  UpdateControls;
end;

procedure TframeSlot.PreparePatchUpdate;
begin
  FinishOperation;
end;

procedure TframeSlot.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeSlot.SaveCursorPos;
begin
  if SelectedLocation = ltFX then
    FCursorFXRect := FModuleCursor.CursorPos
  else
    FCursorVARect := FModuleCursor.CursorPos
end;

procedure TframeSlot.DisablePan(sb: TScrollbox);
begin
  sb.AniCalculations.Animation := False;
  sb.Touch.InteractiveGestures := [];
  FPanEnabled := False;
end;

procedure TframeSlot.EnablePan(sb: TScrollbox);
begin
  sb.AniCalculations.Animation := True;
  sb.Touch.InteractiveGestures := [TInteractiveGesture.igPan];
  FPanEnabled := True;
end;

//==============================================================================
//
//                                Module
//
//==============================================================================

procedure TframeSlot.ActivateAddModule;
begin
  OperationMode := omAddModule;

  if SelectedLocation = ltPatch then
    SelectedLocation := ltVA;

  ActivateModuleCursor;
end;

procedure TframeSlot.DeactivateAddModule;
begin
  FModuleCursor.Active := False;
end;

procedure TframeSlot.ActivateModuleCursor;
begin
  FModuleCursor.Active := True;

  if SelectedLocation = ltFX then begin
    if FModuleCursor.Parent <> FlZoomFX then begin
      FModuleCursor.Parent := FlZoomFX;
      FModuleCursor.CursorPos := FCursorFXRect;
    end;
  end else
    if SelectedLocation = ltVA then begin
      if FModuleCursor.Parent <> FlZoomVA then begin
        FModuleCursor.Parent := FlZoomVA;
        FModuleCursor.CursorPos := FCursorVARect;
      end;
    end else
      FModuleCursor.Parent := nil;
end;

procedure TframeSlot.ActivateModule;
begin
  //tcPatchFunctions.TabIndex := 2;
  OperationMode := omModule;
end;

procedure TframeSlot.DeactivateModule;
begin
end;

procedure TframeSlot.UpdateModuleInfo(aModule: TG2GraphModuleFMX);
begin
  if not assigned(aModule) then
    exit;
end;

//==============================================================================
//
//                              Parameter
//
//==============================================================================

procedure TframeSlot.ActivateParameter;
begin
  //tcPatchFunctions.TabIndex := 3;
  OperationMode := omParam;
  //frameParam.Parameter := GetSelectedParameter;
end;

//==============================================================================
//
//                                Pianoroll
//
//==============================================================================

procedure TframeSlot.ActivatePianoroll;
begin
  if not assigned(SelectedModule) then
    exit;

  OperationMode := omPianoroll;
  if not assigned(FFramePianoRoll) then begin
    FFramePianoRoll := TFramePianoRoll.Create(self);
    FFramePianoRoll.Parent := self;
    FFramePianoRoll.Align := TAlignLayout.alClient;
    FframePianoRoll.OnClose := ClosePianoRoll;
    ComponentSetStateStylesRecursive( FframePianoRoll, FStateStyleList);
  end;
  FFramePianoRoll.SeqModule := SelectedModule;
  FPatch.OnLed := FframePianoRoll.SeqLed;
  FFramePianoRoll.Visible := True;
  FFramePianoRoll.BringToFront;
  FFramePianoRoll.UpdateControls;
  FFramePianoRoll.ScrollInView;
end;

procedure TframeSlot.ClosePianoroll(Sender: TObject);
begin
  FinishOperation;
end;


//==============================================================================
//
//                                Cable
//
//==============================================================================

procedure TframeSlot.ActivateDrawCable;
var G2Connector : TG2Connector;
    Module : TG2FileModule;
    i : integer;
begin
  if not assigned(SelectedControl) or not(SelectedControl is TG2Connector) then
    exit;

  OperationMode := omDrawCable;

  if not assigned(FframeAddCable) then begin
    FframeAddCable := TframeAddCable.Create(self);
    FframeAddCable.Parent := self;
    ComponentSetStateStylesRecursive( FframeAddCable, FStateStyleList);
    FframeAddCable.Align := TAlignLayout.alBottom;
    FframeAddCable.OnClose := CloseDrawCable;
  end;

  FframeAddCable.Connector := nil;

  G2Connector := SelectedControl as TG2Connector;

  Module := FPatch.Modules[ord(G2Connector.Location), G2Connector.ModuleIndex];
  if assigned(Module) then begin
    if G2Connector.ConnectorKind = ckInput then
      FframeAddCable.Connector := Module.InConnector[ G2Connector.CodeRef] as TG2GraphConnectorFMX
    else
      FframeAddCable.Connector := Module.OutConnector[ G2Connector.CodeRef] as TG2GraphConnectorFMX;

    if assigned(FframeAddCable.Connector) then begin

      FframeAddCable.Visible := True;
      FframeAddCable.BringToFront;
      FframeAddCable.UpdateControls;
    end;
  end;
end;

procedure TframeSlot.DeactivateDrawCable;
begin
  FframeAddCable.Visible := False;
  FframeAddCable.Connector := nil;
end;

procedure TframeSlot.CloseDrawCable(Sender: TObject);
begin
  FinishOperation;
end;

procedure TframeSlot.DeleteAllCables;
var G2Connector : TG2Connector;
    Module : TG2FileModule;
    i : integer;
begin
  if OperationMode <> omDrawCable then
    exit;

  if assigned(FframeAddCable) and assigned(FframeAddCable.Connector) then begin
    if FframeAddCable.Connector.CableCount > 0 then begin
      for i := 0 to FframeAddCable.Connector.CableCount - 1 do begin
        Patch.MessDeleteConnection( FframeAddCable.Connector.Location, FframeAddCable.Connector.Cables[i]);
      end;
    end;
  end;

  FinishOperation;
end;

//==============================================================================
//
//                              Selection
//
//==============================================================================

procedure TframeSlot.ActivateSelection( aPatchPart : TG2FilePatchPart; aSelectionMode : TSelectionMode; MouseX, MouseY : single);
var x, y, w, h, b : single;
begin
  OperationMode := omSelection;

  b := 50;
  x := 0;
  y := 0;
  w := Width;
  h := Height;

  FSelectionVA.Position.X := x - b;
  FSelectionVA.Width := FlZoomVA.Width + 2*b;
  FSelectionVA.Height := FlZoomVA.Height + 2*b;
  FSelectionVA.FPatchPart := aPatchPart;

  FSelectionFX.Position.X := x - b;
  FSelectionFX.Width := FlZoomFX.Width + 2*b;
  FSelectionFX.Height := FlZoomFX.Height + 2*b;
  FSelectionFX.FPatchPart := aPatchPart;

  if aPatchPart.Location = ltVA then begin
    FSelectionVA.Position.Y := y - b;
    FSelectionFX.Position.Y := y  - sbVA.Height - b;
  end else begin
    FSelectionVA.Position.Y := y + sbVA.Height - b;
    FSelectionFX.Position.Y := y - b;
  end;

  {FSelectionVA.UnscaledLeft := x - b;
  FSelectionVA.UnscaledWidth := FlZoomVA.Width + 2*b;
  FSelectionVA.UnscaledHeight := FlZoomVA.Height + 2*b;
  FSelectionVA.FPatchPart := aPatchPart;

  FSelectionFX.UnscaledLeft := x - b;
  FSelectionFX.UnscaledWidth := FlZoomFX.Width + 2*b;
  FSelectionFX.UnscaledHeight := FlZoomFX.Height + 2*b;
  FSelectionFX.FPatchPart := aPatchPart;

  if aPatchPart.Location = ltVA then begin
    FSelectionVA.UnscaledTop := y - b;
    FSelectionFX.UnscaledTop := y  - sbVA.Height - b;
  end else begin
    FSelectionVA.UnscaledTop := y + sbVA.Height - b;
    FSelectionFX.UnscaledTop := y - b;
  end;}

  FSelectionVA.FStartX := FSelectionVA.Position.X;
  FSelectionVA.FStartY := FSelectionVA.Position.Y;
  // MouseXY, distance mouspointer to left top corner of selection
  FSelectionVA.FMouseX := MouseX + b;
  FSelectionVA.FMouseY := MouseY + b;

  FSelectionFX.FStartX := FSelectionFX.Position.X;
  FSelectionFX.FStartY := FSelectionFX.Position.Y;
  FSelectionFX.FMouseX := MouseX + b;
  FSelectionFX.FMouseY := MouseY + b;

  FSelectionMode := aSelectionMode;
  if FSelectionMode = smMove then begin
    if aPatchPart.Location = ltVA then begin
      // Disable panning
      DisablePan( sbVA);
      FSelectionVA.Active := True;
      FSelectionVA.FSelectionMode := aSelectionMode;
    end else begin
      DisablePan( sbFX);
      FSelectionFX.Active := True;
      FSelectionFX.FSelectionMode := aSelectionMode;
    end;
  end;
  if FSelectionMode = smCopy then begin
    DisablePan( sbVA);
    FSelectionVA.Active := True;
    FSelectionVA.FSelectionMode := aSelectionMode;
    DisablePan( sbFX);
    FSelectionFX.Active := True;
    FSelectionFX.FSelectionMode := aSelectionMode;
  end;
  FSelectionActive := True;
end;

procedure TframeSlot.DeactivateSelection;
var i : integer;
    Module : TG2FileModule;
    ModulesRect : TRect;
begin
  if FSelectionMode = smMove then begin
    if FSelectionVA.Active then begin
      EnablePan( sbVA);
      FSelectionVA.CalcNewPositions;
      FPatch.MessMoveModules( ltVA);
    end else begin
      if FSelectionFX.Active then begin
        EnablePan( sbFX);
        FSelectionFX.CalcNewPositions;
        FPatch.MessMoveModules( ltFX);
      end;
    end;
  end;

  if FSelectionMode = smCopy then begin
    if MouseOverLocation = ltVA then begin
      EnablePan( sbVA);
      FSelectionVA.CalcNewPositions;
      FPatch.MessCopyModules( FSelectionVA.FPatchPart, ltVA, MouseOverLocation);
    end else begin
      EnablePan( sbFX);
      FSelectionFX.CalcNewPositions;
      FPatch.MessCopyModules( FSelectionFX.FPatchPart, ltFX, MouseOverLocation);
    end;
  end;

  FSelectionActive := False;
  FSelectionVA.Active := False;
  FSelectionFX.Active := False;
end;

//==============================================================================
//
//                             Drag selection
//
//==============================================================================

procedure TframeSlot.ActivateDragSelect( sb : TScrollbox; X, Y : single);
begin
  OperationMode := omDragSelect;
  FDragSelector.Active := True;
  if SelectedLocation = ltFX then
    FDragSelector.Parent := FlZoomFX
  else
    if SelectedLocation = ltVA then
      FDragSelector.Parent := FlZoomVA
    else
      FDragSelector.Parent := nil;
  FDragSelector.StartPoint := PointF((sb.ViewportPosition.X + X)/FZoom, (sb.ViewportPosition.Y + Y)/FZoom);
end;

procedure TframeSlot.DeactivateDragSelect;
var i : integer;
begin
  if not assigned(PatchPart) then
    exit;

  for i := 0 to PatchPart.ModuleList.Count - 1 do begin
    PatchPart.ModuleList[i].SelectModule(
        FDragSelector.InSelection(PatchPart.ModuleList[i].Col * (UNITS_COL+UNIT_MARGIN*2),
                                  PatchPart.ModuleList[i].Row * (UNITS_ROW+UNIT_MARGIN*2)), False);
  end;
  FDragSelector.Active := False;
end;


//==============================================================================
//
//                              Get/Set
//
//==============================================================================

function TframeSlot.GetPatchPart: TG2GraphPatchPartFMX;
begin
  if assigned(FPatch) and (SelectedLocation in [ltFX, ltVA]) then
    Result := FPatch.PatchPart[ ord(SelectedLocation)] as TG2GraphPatchPartFMX
  else
    Result := nil;
end;

function TframeSlot.GetSelectedControl: TG2BaseControl;
var G2 : TG2GraphFMX;
begin
  Result := nil;
  if not assigned(FPatch) then
    exit;

  G2 := FPatch.G2 as TG2GraphFMX;
  Result := G2.SelectedControl;
end;

procedure TframeSlot.SetSelectedControl(const Value: TG2BaseControl);
var G2 : TG2GraphFMX;
begin
  G2 := FPatch.G2 as TG2GraphFMX;
  G2.SelectedControl := Value;
end;

function TframeSlot.GetSelectedLocation: TLocationType;
begin
  //Result := FPatch.SelectedLocation;
  Result := FSelectedLocation;
end;

procedure TframeSlot.SetSelectedLocation(const Value: TLocationType);
begin
  //if Value <> FPatch.SelectedLocation then begin
  //  FPatch.PatchPart[ord(FPatch.SelectedLocation)].UnselectModules;
  //  FPatch.SelectedLocation := Value;
  //  UpdateControls;
  //end;
  if Value <> FSelectedLocation then begin
    FPatch.PatchPart[ord(FSelectedLocation)].UnselectModules;
    FSelectedLocation := Value;
    FPatch.SelectedLocation := FSelectedLocation;
    case FOperationMode of
    omAddModule : ActivateModuleCursor;
    end;
    UpdateControls;
  end;
end;

procedure TframeSlot.SetSelectedModule(const Value: TG2GraphModuleFMX);
begin
  if Value <> FModule then begin
    FModule := Value;
    if assigned(FModule) then begin
      SelectedLocation := FModule.Location;
      UpdateModuleInfo(FModule);
    end;
  end;
end;

function TframeSlot.GetSelectMultiple: boolean;
begin
  if not assigned(FPatch) then
    Result := False
  else
    Result := FPatch.SelectMultiple;
end;

procedure TframeSlot.SetSelectMultiple(const Value: boolean);
begin
  if assigned(FPatch) then
    FPatch.SelectMultiple := Value;
end;

procedure TframeSlot.SetMouseOverControl(const Value: TG2BaseControl);
begin
  if Value <> FMouseOverControl then begin
    FMouseOverControl := Value;
  end;
end;

procedure TframeSlot.SetMouseOverLocation(const Value: TLocationType);
begin
  if Value <> FMouseOverLocation then begin
    FMouseOverLocation := Value;
  end;
end;

procedure TframeSlot.SetMouseOverModule(const Value: TG2GraphModuleFMX);
begin
  if Value <> FMouseOverModule then begin
    FMouseOverModule := Value;
  end;
end;

procedure TframeSlot.FinishOperation;
begin
  if FOperationMode = omSelection then
    DeactivateSelection;

  if FOperationMode = omDrawCable then
    DeactivateDrawCable;

  if FOperationMode = omAddModule then
    DeactivateAddModule;

  if FOperationMode = omDragSelect then
    DeactivateDragSelect;

  if FOperationMOde = omPianoRoll then begin
    FPatch.OnLed := nil;
    FframePianoRoll.SeqModule := nil;
    FframePianoRoll.Visible := False;
  end;

  case FPrevOperationMode of
    omAddModule : ActivateAddModule;
  else
    FOperationMode := omNormal;
  end;
end;

procedure TframeSlot.SetOperationMode(const Value: TOperationMode);
begin
  if FOperationMode <> Value then begin
    if (FOperationMode = omPianoRoll) and (FframePianoRoll.Visible) then begin
      FPatch.OnLed := nil;
      FframePianoRoll.SeqModule := nil;
      FframePianoRoll.Visible := False;
    end;


    if FOperationMode = omSelection then
      DeactivateSelection;

    if FOperationMode = omDrawCable then
      DeactivateDrawCable;

    if FOperationMode = omAddModule then
      DeactivateAddModule;

    FPrevOperationMode := FOperationMode;

    FOperationMode := Value;

    if assigned(FOnOperationModeChange) then
      FOnOperationModeChange(Self);
  end;
end;

procedure TframeSlot.SlotDeleteModule(Sender: TObject; SenderID: integer;
  Location: TLocationType; ModuleIndex: integer);
begin
  if assigned(FframePianoRoll) then begin
    FinishOperation;
    if assigned(FFramePianoRoll.SeqModule) then begin
      if (FFramePianoRoll.SeqModule.Location = Location) and (FFramePianoRoll.SeqModule.ModuleIndex = ModuleIndex) then
        FFramePianoRoll.SeqModule := nil;
    end;
  end;
end;

procedure TframeSlot.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  FStateStyleList := aStateStyleList;
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframeSlot.SetVariation(const Value: integer);
begin
  FVariation := Value;
  Patch.InvalidateParameters;
  if assigned(FframePianoRoll) then
    FframePianoRoll.UpdateControls;
end;

//==============================================================================
//
//                                   GUI
//
//==============================================================================

procedure TframeSlot.UpdateSplitter;
begin
  if not assigned(FPatch) then
    exit;

  if not FPatch.SplitterVisible then begin
    Splitter1.Visible := False;
    Splitter1.Align := TAlignLayout.alNone;
    lVA.Align := TAlignLayout.alClient;
    lVA.Visible := SelectedLocation = ltVA;
    lFX.Align := TAlignLayout.alClient;
    lFX.Visible := SelectedLocation = ltFX;
  end else begin
    lVA.Visible := True;
    lFX.Visible := True;
    Splitter1.Visible := True;
    lVA.Align := TAlignLayout.alNone;
    lFX.Align := TAlignLayout.alNone;
    Splitter1.Align := TAlignLayout.alNone;

    lVA.Position.Y := 0;
    Splitter1.Position.Y := 10;
    lFX.Position.Y := 20;

    lFX.Align := TAlignLayout.alBottom;
    //lFX.Height := FPatch.PatchDescription.BarPosition;
    lFX.Height := 210;
    Splitter1.Align := TAlignLayout.alBottom;
    lVA.Align := TAlignLayout.alClient;
  end;

  UpdateControls;
end;

procedure TframeSlot.ControlGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
end;

procedure TframeSlot.tcPatchFunctionsChange(Sender: TObject);
begin
  {if tcPatchFunctions.TabIndex = 0 then
    ActivateAddModule
  else begin
    FinishOperation;
  end;}
end;

procedure TframeSlot.btParamDeassignCCMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
end;

//==============================================================================
//
//                                   Mouse
//
//==============================================================================

procedure TframeSlot.ControlMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P : TPointF;
begin
  case OperationMode of
    omNormal,
    omModule,
    omParam,
    omAddModule:
      begin
        if Sender is TG2MiniVU then begin
          if assigned(FOnMiniVUClick) then
            FOnMiniVUClick(self, Sender as TG2MiniVU);
        end else
          if Sender is TG2BaseControl then begin

            SelectedControl := Sender as TG2BaseControl;

            P := SelectedControl.LocalToAbsolute(PointF(X, Y));

            FLastX := P.X;
            FLastY := P.Y;

            SelectedControl.ProcessMouseDown( Sender, Shift, 0, X, Y);
          end;
      end;
    omSelection:
      begin
      end;
    omDrawCable: ;
  end;

end;

procedure TframeSlot.ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  P, CP: TPointF;
  dx, dy : single;
  sb : TScrollbox;
begin
  if not(Sender is TG2BaseControl) then
    exit;

  MouseOverControl := Sender as TG2BaseControl;
  MouseOverModule := FPatch.Modules[ ord( FMouseOverControl.Location), FMouseOverControl.ModuleIndex] as TG2GraphModuleFMX;
  MouseOverLocation := MouseOverControl.Location;

  P := (Sender as TG2BaseControl).LocalToAbsolute(PointF(X, Y));
  dx := (P.X - FLastX);
  dy := (P.Y - FLastY);

  FLastX := P.X;
  FLastY := P.Y;

  case OperationMode of
    omNormal,
    omModule,
    omParam,
    omAddModule:
      begin
        if assigned(SelectedControl) then begin
          // Access violations!
          CP := SelectedControl.AbsoluteToLocal( P);
          SelectedControl.ProcessMouseMove( Sender, Shift, 0, CP.X, CP.Y);
        end;
      end;
    omDragSelect :
      begin
         if SelectedLocation = ltVA then
           sb := sbVA
         else
           sb := sbFX;
         P := sb.AbsoluteToLocal(P);

        FDragSelector.DragPoint := PointF((sb.ViewportPosition.X + P.X)/FZoom, (sb.ViewportPosition.Y + P.Y)/FZoom);
      end;
    omSelection:
      begin
        FSelectionVA.ProcessMouseMove(self, Shift, P.X, P.Y);
        FSelectionFX.ProcessMouseMove(self, Shift, P.X, P.Y);
      end;
    omDrawCable:
      begin
      end;
  end;
end;

procedure TframeSlot.ControlMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  case OperationMode of
    omNormal,
    omModule,
    omParam,
    omAddModule:
      begin
        if assigned(SelectedControl) then begin
          if SelectedControl is TG2Connector then begin
            ActivateDrawCable;
          end else begin
            ActivateParameter;
            SelectedControl.ProcessMouseUp( Sender, Shift, 0, X, Y);
          end;
        end;
      end;
    omSelection:
      begin
        // Deactivate the selection
        FinishOperation;
        CalcLayoutDimensions;
      end;
    omDragSelect:
      begin
        FinishOperation
      end;
    omDrawCable:
      begin
        if (Sender <> SelectedControl) and (Sender is TG2Connector) then begin
          if assigned(FframeAddCable) and assigned(FframeAddCable.Connector) then
            FframeAddCable.Connector.CreateCable( self, (Sender as TG2Connector));
        end;
        FinishOperation;
      end;
  end;
end;

procedure TframeSlot.ModuleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P : TPointF;
begin
  if (Sender is TG2Module) then begin

    case OperationMode of
    omNormal,
    omModule,
    omParam,
    omAddModule,
    omDrawCAble,
    omSelection:
      begin
        SelectedModule := FPatch.Modules[ ord((Sender as TG2Module).Location), (Sender as TG2Module).ModuleIndex] as TG2GraphModuleFMX;

        SelectedLocation := (Sender as TG2Module).Location;

        SelectedControl := nil;

        ActivateModule;

        P := SelectedModule.G2Module.LocalToAbsolute(PointF(X, Y));

        FLastX := P.X;
        FLastY := P.Y;
      end;
    end;
  end;
end;

procedure TframeSlot.ModuleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var P, CP: TPointF;
    dx, dy : single;
    sb : TScrollbox;
begin
  if not(Sender is TG2Module) then
    exit;

  MouseOverControl := nil;
  MouseOverModule := FPatch.Modules[ ord((Sender as TG2Module).Location), (Sender as TG2Module).ModuleIndex] as TG2GraphModuleFMX;
  MouseOverLocation := (Sender as TG2Module).Location;

  P := (Sender as TBufferedControl).LocalToAbsolute(PointF(X, Y));
  dx := (P.X - FLastX);
  dy := (P.Y - FLastY);
  FLastX := P.X;
  FLastY := P.Y;

  case OperationMode of
    omNormal,
    omModule,
    omParam,
    omAddModule:
      begin
        if assigned(SelectedControl) then begin
          CP := SelectedControl.AbsoluteToLocal( P);
          SelectedControl.ProcessMouseMove( Sender, Shift, 0, CP.X, CP.Y);
        end else
          if (ssLeft in Shift) or (ssTouch in Shift) then begin
            if assigned(SelectedModule) then begin
              if (abs(dx)>0.1) or (abs(dy)>0.1) then begin
                // Activate selection

                // If the module wasn't selected yet, then select it here
                if not SelectedModule.Selected then begin
                  SelectedModule.SelectModule( not SelectedModule.Selected, True);
                end;
                if SelectedLocation = ltVA then
                  P := lSizeVA.AbsoluteToLocal(P)
                else
                  P := lSizeFX.AbsoluteToLocal(P);

                ActivateSelection( Patch.PatchPart[ord(SelectedLocation)], smMove, P.X/FZoom, P.Y/FZoom);
              end;
            end;
          end;
      end;
    omDragSelect :
      begin
         if SelectedLocation = ltVA then
           sb := sbVA
         else
           sb := sbFX;
         P := sb.AbsoluteToLocal(P);

        FDragSelector.DragPoint := PointF((sb.ViewportPosition.X + P.X)/FZoom, (sb.ViewportPosition.Y + P.Y)/FZoom);
      end;
    omSelection:
      begin
        FSelectionVA.ProcessMouseMove(self, Shift, P.X, P.Y);
        FSelectionFX.ProcessMouseMove(self, Shift, P.X, P.Y);
      end;
    omDrawCable:
      begin
      end;
  end;
end;

procedure TframeSlot.ModuleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P, CP : TPointF;
begin
  P := (Sender as TG2Module).LocalToAbsolute(PointF(X, Y));

  case OperationMode of
    omNormal,
    omModule,
    omParam:
      begin
        if (Sender is TG2Module) then begin

          // Add the control to the selection
          SelectedModule := FPatch.Modules[ ord((Sender as TG2Module).Location), (Sender as TG2Module).ModuleIndex] as TG2GraphModuleFMX;;

          if assigned(SelectedControl) then begin
            CP := SelectedControl.AbsoluteToLocal( P);
            SelectedControl.ProcessMouseUp( Sender, Shift, 0, CP.X, CP.Y);
          end else begin
            if (ssShift in Shift) or (SelectMultiple) then begin
              SelectedModule.SelectModule( not SelectedModule.Selected, False);
            end else begin
              GetPatchPart.FocusedModule := SelectedModule;
              //SelectedModule.SelectModule( not SelectedModule.Selected, True);
            end;
          end;
        end;
      end;
    omDragSelect:
      begin
        FinishOperation
      end;
    omSelection:
      begin
        FinishOperation;
        Patch.MessMoveModules(SelectedLocation);
        CalcLayoutDimensions;
      end;
    omDrawCable:
      begin
      end;
  end;
end;

procedure TframeSlot.sbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var P: TPointF;
begin
  if ssCtrl in Shift then begin
    ActivateDragSelect( Sender as TScrollbox, X, Y);
    Abort;
  end;

  if not FPanEnabled then
    Abort;

  if (Sender as TScrollbox).Name = 'sbVA' then begin

    SelectedLocation := ltVA;

    if Button = TMouseButton.mbLeft then begin
      SelectedControl := nil;
      SelectedModule := nil;
      P := sbVA.LocalToAbsolute(P);
      FLastX := P.X;
      FLastY := P.Y;
    end;
  end else begin

    SelectedLocation := ltFX;

    if Button = TMouseButton.mbLeft then begin

      SelectedControl := nil;
      SelectedModule := nil;

      P := sbFX.LocalToAbsolute(P);

      FLastX := P.X;
      FLastY := P.Y;
    end;
  end;
end;

procedure TframeSlot.sbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var dx, dy : single;
    P, CP: TPointF;
    sb : TScrollbox;
begin
  P := PointF(X, Y);
  sb := Sender as TScrollbox;

  MouseOverControl := nil;
  MouseOverModule := nil;

  if (Sender as TScrollbox).Name = 'sbVA' then begin
    P := sbVA.LocalToAbsolute(P);
    MouseOverLocation := ltVA;
  end else begin
    P := sbFX.LocalToAbsolute(P);
    MouseOverLocation := ltFX;
  end;

  dx := (P.X - FLastX);
  dy := (P.Y - FLastY);
  FLastX := P.X;
  FLastY := P.Y;

  case OperationMode of
    omNormal,
    omModule,
    omParam:
      begin
        if assigned(SelectedControl) then begin
          // access violation!!
          CP := SelectedControl.AbsoluteToLocal( P);
          SelectedControl.ProcessMouseMove( Sender, Shift, 0, CP.X, CP.Y);
        end;
      end;
    omAddModule :
      begin
      end;
    omDragSelect :
      begin
        FDragSelector.DragPoint := PointF((sb.ViewportPosition.X + X)/FZoom, (sb.ViewportPosition.Y + Y)/FZoom);
      end;
    omSelection:
      begin
        FSelectionVA.ProcessMouseMove(self, Shift, P.X, P.Y);
        FSelectionFX.ProcessMouseMove(self, Shift, P.X, P.Y);
      end;
    omDrawCable:
      begin
      end;
  end;
end;

procedure TframeSlot.sbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var i : integer;
    P, CP: TPointF;
begin
  P := (Sender as TScrollbox).LocalToAbsolute(P);

  case OperationMode of
    omNormal,
    omModule,
    omParam:
      begin
        if assigned(SelectedControl) then begin
          // Value dragged beyond control bounds
          CP := SelectedControl.AbsoluteToLocal( P);
          SelectedControl.ProcessMouseUp( Sender, Shift, 0, X, Y);
        end else
          if assigned(FPatch) then begin
            FPatch.PatchPart[ ord(SelectedLocation)].UnselectModules;
            ActivateAddModule;
          end;
      end;
    omAddModule:
      begin
        if (Sender as TScrollbox).Name = 'sbVA' then begin
          FModuleCursor.Parent := FlZoomVA;
        end else
          if (Sender as TScrollbox).Name = 'sbFX' then begin
            FModuleCursor.Parent := FlZoomFX;
          end;
        FModuleCursor.Col := trunc((((Sender as TScrollbox).ViewportPosition.X + X) / FZoom) / (UNITS_COL+UNIT_MARGIN*2));
        FModuleCursor.Row := trunc((((Sender as TScrollbox).ViewportPosition.Y + Y) / FZoom) / (UNITS_ROW+UNIT_MARGIN*2));
        SaveCursorPos;
      end;
    omDragSelect:
      begin
        FinishOperation
      end;
    omSelection:
      begin
        FinishOperation;
        CalcLayoutDimensions;
      end;
    omDrawCable:
      begin
      end;
  end;
end;

//==============================================================================
//
//                              TSelectionLayout
//
//==============================================================================

constructor TSelection.Create(AOwner: TComponent);
begin
  inherited;

  HitTest := False;
  FPatchPart := nil;
  FStartX := Position.X;
  FStartY := Position.Y;
end;

destructor TSelection.Destroy;
begin
  inherited;
end;

function TSelection.GetActive: boolean;
begin
  Result := Visible;
end;

function TSelection.GetDeltaUnitsCol: integer;
begin
  if Position.X - FStartX < 0 then
    Result := trunc((Position.X - FStartX - (UNITS_COL+UNIT_MARGIN*2)/2)/(UNITS_COL+UNIT_MARGIN*2))
  else
    Result := trunc((Position.X - FStartX)/(UNITS_COL+UNIT_MARGIN*2));
end;

function TSelection.GetDeltaUnitsRow: integer;
begin
  if Position.Y - FStartY < 0 then
    Result := trunc((Position.Y - FStartY - (UNITS_ROW+UNIT_MARGIN*2)/2)/(UNITS_ROW+UNIT_MARGIN*2))
  else
    Result := trunc((Position.Y - FStartY)/(UNITS_ROW+UNIT_MARGIN*2));
end;

procedure TSelection.CalcNewPositions;
var i : integer;
    Module : TG2FileModule;
    ModulesRect : TRect;
begin
  if assigned(FPatchPart) then begin
    if FSelectionMode = smCopy then begin
      // After copy, mouse is in middle of modulesrect
      ModulesRect := FPatchPart.ModuleList.UnitsRect;
      for i := 0 to FPatchPart.ModuleList.Count - 1 do begin
        Module := FPatchPart.ModuleList[i];
        Module.NewCol := max(0, trunc((Module.Col -  ModulesRect.Left)
                                      + (Position.X + FMouseX
                                         - (UNITS_COL+UNIT_MARGIN*2) * (ModulesRect.Right - ModulesRect.Left)/2) / (UNITS_COL+UNIT_MARGIN*2)));
        Module.NewRow := max(0, trunc((Module.Row -  ModulesRect.Top)
                                      + (Position.Y + FMouseY
                                         - (UNITS_ROW+UNIT_MARGIN*2) * (ModulesRect.Bottom - ModulesRect.Top)/2) / (UNITS_ROW+UNIT_MARGIN*2)));
      end;
    end else begin
      // After move
      for i := 0 to FPatchPart.SelectedModuleList.Count - 1 do begin
        Module := FPatchPart.SelectedModuleList[i];
        Module.NewRow := max(0, Module.Row + DeltaUnitsRow);
        Module.NewCol := max(0, Module.Col + DeltaUnitsCol);
      end;
    end;
  end;
end;


procedure TSelection.Paint;
var Save: TCanvasSaveState;
begin
  inherited;
  Save := Canvas.SaveState;
  Canvas.BeginScene;
  try
    PaintOn(Canvas);
  finally
    Canvas.RestoreState(Save);
    Canvas.EndScene;
  end;
end;

procedure TSelection.PaintOn(aCanvas: TCanvas);
var Rect : TRectF;
    Module : TG2GraphModuleFMX;
    i : integer;
begin
  aCanvas.Fill.Kind := TBrushKind.bkNone;

  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claGreen;
  aCanvas.Stroke.Thickness := 3;

  if assigned(FPatchPart) then begin

    if FSelectionMode = smCopy then begin
      for i := 0 to FPatchPart.ModuleList.Count - 1 do begin
        Module := FPatchPart.ModuleList[i] as TG2GraphModuleFMX;
        Rect := Module.OutlineRect;
        Rect.Left := Rect.Left + 50 + UNIT_MARGIN;
        Rect.Top := Rect.Top + 50 + UNIT_MARGIN;
        Rect.Right := Rect.Right + 50 - UNIT_MARGIN;
        Rect.Bottom := Rect.Bottom + 50 - UNIT_MARGIN;
        aCanvas.DrawRect( Rect, 0, 0, [], AbsoluteOpacity);
      end;
    end else
      for i := 0 to FPatchPart.SelectedModuleList.Count - 1 do begin
        Module := FPatchPart.SelectedModuleList[i] as TG2GraphModuleFMX;
        Rect.Left := Module.G2Module.UnscaledLeft + 50 + UNIT_MARGIN;
        Rect.Top := Module.G2Module.UnscaledTop + 50 + UNIT_MARGIN;
        Rect.Width := Module.G2Module.UnscaledWidth - UNIT_MARGIN*2;
        Rect.Height := Module.G2Module.UnscaledHeight - UNIT_MARGIN * 2;
        aCanvas.DrawRect( Rect, 0, 0, [], AbsoluteOpacity);
      end;
  end;
end;

procedure TSelection.ProcessMouseMove(Sender: TObject; Shift: TShiftState; AbsX,
  AbsY: Single);
var P : TPointF;
begin
  P := (Parent as TControl).AbsoluteToLocal(PointF(AbsX, AbsY));

  Position.X := P.X - FMouseX;
  Position.Y := P.Y - FMouseY;
end;

procedure TSelection.SetActive(const aValue: boolean);
begin
  Visible := aValue;
  if Visible then
    BringToFront;
end;

//==============================================================================
//
//                              TModuleCursor
//
//==============================================================================

constructor TModuleCursor.Create(AOwner: TComponent);
begin
  inherited;
  FMargin := 0;
  Col := 0;
  Row := 0;
  UnitsHeight := 1;
  UnscaledLeft := FCol * (UNITS_COL+UNIT_MARGIN*2) - FMargin;
  UnscaledTop := FRow * (UNITS_ROW+UNIT_MARGIN*2) - FMargin;
  UnscaledHeight := (UNITS_ROW+UNIT_MARGIN*2) + FMargin * 2;
  UnscaledWidth := (UNITS_COL+UNIT_MARGIN*2) + FMargin * 2;
  FActive := False;
  Visible := FActive;
end;

destructor TModuleCursor.Destroy;
begin
  inherited;
end;

procedure TModuleCursor.PaintOn(aCanvas: TCanvas);
var R : TRectF;
begin
  aCanvas.Fill.Kind := TBrushKind.bkNone;

  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claGreen;
  aCanvas.Stroke.Thickness := 3;

  R := RectF( UnscaledBoundsRect.Left + UNIT_MARGIN,
              UnscaledBoundsRect.Top + UNIT_MARGIN,
              UnscaledBoundsRect.Right - UNIT_MARGIN,
              UnscaledBoundsRect.Bottom - UNIT_MARGIN);

  aCanvas.DrawRect( R, 0, 0, [], AbsoluteOpacity);
end;

procedure TModuleCursor.ScrollInView;
var NewViewportPosition : TPointF;
    R : TRectF;
    sb : TScrollbox;
    bl : TG2BufferedLayout;
    Zoom : single;
    FMXObject : TFMXObject;
begin
  FMXObject := Parent;
  while assigned(FMXObject) and not(FMXObject is TScrollbox) do begin
    if FMXObject is TG2BufferedLayout then
      bl := FMXObject as TG2BufferedLayout;

    FMXObject := FMXObject.Parent;
  end;

  if not assigned(FMXObject) then
    exit;

  Zoom := 1;
  if assigned(bl) then
    Zoom := bl.Zoom;

  R := RectF(Position.X * Zoom, Position.Y * Zoom, (Position.X + Width) * Zoom, (Position.Y + Height) * Zoom);

  sb := FMXObject as TScrollbox;

  if R.Right > sb.ViewportPosition.X + sb.Width then
    NewViewportPosition.X := R.Right - sb.Width
  else
    if R.Left < sb.ViewportPosition.X then
      NewViewportPosition.X := R.Left
    else
      NewViewportPosition.X := sb.ViewportPosition.X;

  if R.Bottom > sb.ViewportPosition.Y + sb.Height then
    NewViewportPosition.Y := R.Bottom - sb.Height
  else
    if R.Top < sb.ViewportPosition.Y then
      NewViewportPosition.Y := R.Top
    else
      NewViewportPosition.Y := sb.ViewportPosition.Y;
  sb.ViewportPosition := NewViewportPosition;
end;

procedure TModuleCursor.SetActive(const Value: boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    Visible := FActive;
    BringToFront;
    Redraw;
  end;
end;

procedure TModuleCursor.SetCol(const Value: integer);
begin
  if FCol <> Value then begin
    FCol := Value;
    UnscaledLeft := FCol * (UNITS_COL+UNIT_MARGIN*2) - FMargin;
  end;
end;

function TModuleCursor.GetCursorPos: TRect;
begin
  Result.Left := FCol;
  Result.Top := FRow;
  Result.Right := FCol + 1;
  Result.Bottom := FRow + FUnitsHeight;
end;

procedure TModuleCursor.SetCursorPos(const Value: TRect);
begin
  FRow := Value.Top;
  FUnitsHeight := Value.Bottom - Value.Top;
  FCol := Value.Left;
  UnscaledLeft := FCol * (UNITS_COL+UNIT_MARGIN*2) - FMargin;
  UnscaledTop := FRow * (UNITS_ROW+UNIT_MARGIN*2) - FMargin;
  UnscaledHeight := FUnitsHeight * (UNITS_ROW+UNIT_MARGIN*2) + FMargin * 2;
end;

procedure TModuleCursor.SetModuleLocation(const aModule: TG2FileModule);
begin
  Row := aModule.Row;
  Col := aModule.Col;
  UnitsHeight := aModule.HeightUnits;
end;

procedure TModuleCursor.SetRow(const Value: integer);
begin
  if FRow <> VAlue then begin
    FRow := Value;
    UnscaledTop := FRow * (UNITS_ROW+UNIT_MARGIN*2) - FMargin;
  end;
end;


procedure TModuleCursor.SetUnitsHeight(const Value: integer);
begin
  if FUnitsHeight <> Value then begin
    FUnitsHeight := Value;
    UnscaledHeight := FUnitsHeight * (UNITS_ROW+UNIT_MARGIN*2) + FMargin * 2;
  end;
end;

//==============================================================================
//
//                              TDragSelector
//
//==============================================================================

constructor TDragSelector.Create(AOwner: TComponent);
begin
  inherited;

  HitTest := False;

  FMargin := 4;
  FActive := False;
  Visible := FActive;
end;

destructor TDragSelector.Destroy;
begin
  inherited;
end;

function TDragSelector.InSelection(X, Y: single): boolean;
begin
  Result := ((X + (UNITS_COL+UNIT_MARGIN*2)/2) >= UnscaledRect.Left + FMargin)
        and (X <= UnscaledRect.Right - FMargin)
        and ((Y + (UNITS_ROW+UNIT_MARGIN*2)/2) >= UnscaledRect.Top + FMargin)
        and (Y <= UnscaledRect.Bottom - FMargin);
end;

procedure TDragSelector.PaintOn(aCanvas: TCanvas);
var R : TRectF;
begin
  aCanvas.Fill.Kind := TBrushKind.bkNone;

  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claGreen;
  aCanvas.Stroke.Thickness := 3;

  R := RectF( UnscaledBoundsRect.Left + FMargin,
              UnscaledBoundsRect.Top + FMargin,
              UnscaledBoundsRect.Right - FMargin,
              UnscaledBoundsRect.Bottom - FMargin);


  aCanvas.DrawRect( R, 0, 0, [], AbsoluteOpacity);
end;

procedure TDragSelector.SetActive(const Value: boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    Visible := FActive;
    BringToFront;
    Redraw;
  end;
end;

procedure TDragSelector.CalcBounds;
begin
  UnscaledLeft := System.Math.Min(FStartPoint.X, FDragPoint.X) - FMargin;
  UnscaledTop := System.Math.Min(FStartPoint.Y, FDragPoint.Y) - FMargin;
  UnscaledWidth := Abs(FStartPoint.X - FDragPoint.X) + FMargin * 2;
  UnscaledHeight := Abs(FStartPoint.Y - FDragPoint.Y) + FMargin * 2;
  Redraw;
end;


procedure TDragSelector.SetDragPoint(const Value: TPointF);
begin
  FDragPoint := Value;
  CalcBounds;
end;


procedure TDragSelector.SetStartPoint(const Value: TPointF);
begin
  FStartPoint := Value;
  FDragPoint := Value;
  CalcBounds;
end;

end.
