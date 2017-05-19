unit UnitMiniSlot;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  System.Generics.Collections,
  FMX.StdCtrls, FMX.Layouts,
{$IFDEF VER260}
  FMX.Graphics,
{$ENDIF}
  BVE.NMG2ControlsFMX,
  BVE.NMG2Data,
  BVE.NMG2Types,
  BVE.NMG2File,
  BVE.NMG2USB,
  BVE.NMG2GraphFMX;

type
   TMiniModule = class(TG2Module)
   private
     FModule : TG2GraphModuleFMX;
     FZoomed : boolean;
     procedure SetZoomed(const Value: boolean);
   protected
     procedure SetCol(const aValue: integer); override;
     procedure SetRow(const aValue: integer); override;
   public
     constructor Create(AOwner : TComponent); override;
     destructor Destroy; override;

     procedure PaintOn( aCanvas : TCanvas); override;

     property Zoomed : boolean read FZoomed write SetZoomed;
   end;

  TframeMiniSlot = class(TFrame)
    sbPatch: TScrollBox;
    blVA: TBufferedLayout;
    blFX: TBufferedLayout;
    procedure btInsertChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbLocationChangeValue(Sender: TObject; const aValue: Integer);
    procedure Panel1Click(Sender: TObject);
  private
    [Weak] FPatch : TG2GraphPatchFMX;

    FMiniModule : boolean;
    FSelectedLocation: TLocationType;

    procedure SetPatch(const Value: TG2GraphPatchFMX);
    procedure CreateModuleFMX( Sender : TObject; aLocation : TLocationType; aModule : TG2GraphModuleFMX);
    procedure CreateG2Control( Sender : TObject; aControl : TG2BaseControl);
    procedure CreateCableFMX(Sender: TObject; aLocation : TLocationType; aCable: TG2GraphCableFMX);
    procedure ModuleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    procedure SetSelectedLocation(const Value: TLocationType);  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure UpdatePatch;

    property SelectedLocation : TLocationType read FSelectedLocation write SetSelectedLocation;
    property Patch : TG2GraphPatchFMX read FPatch write SetPatch;
  end;

implementation

{$R *.fmx}

uses UnitModule, UnitAndroidUtils{, UnitG2MiniEditor};

const
  MODULE_WIDTH = 150;
  MODULE_HEIGHT = 40;

//==============================================================================
//
//                                 Utils
//
//==============================================================================

function ConvertToAlpha( aColor : integer): TAlphaColor;
begin
  Result := $ff000000
          + (Cardinal(aColor) and $000000ff) shl 16
          + (Cardinal(aColor) and $0000ff00)
          + (Cardinal(aColor) and $00ff0000) shr 16;
end;

{ TFrame1 }

procedure TframeMiniSlot.btInsertChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aVAlue = 0 then
    FPatch.MessAddModule(ltVA, 8, 1, 1);
end;

constructor TframeMiniSlot.Create(AOwner: TComponent);
begin
  inherited;
  FSelectedLocation := ltFX;
end;

procedure TframeMiniSlot.CreateG2Control(Sender: TObject; aControl: TG2BaseControl);
begin
  if aControl is TG2Knob then
    aControl.AutoCapture := True;

  if Sender is TG2GraphModuleFMX then begin
    case (Sender as TG2GraphModuleFMX).Location of
    ltPatch:;
    ltFX:
      begin
      end;
    ltVA:
      begin
      end;
    end;
  end;
end;

procedure CreateConnectors( aModule : TG2GraphModuleFMX; aControl : TControl);
var G2 : TG2GraphFMX;
    G2Control : TG2BaseControl;
    c : integer;

  procedure AssignToInConnector( aControl : TG2Connector);
  var Connector : TG2GraphConnectorFMX;
  begin
    Connector := aModule.InConnector[ aControl.CodeRef] as TG2GraphConnectorFMX;
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    Connector.G2Connector := aControl;
  end;

  procedure AssignToOutConnector( aControl : TG2Connector);
  var Connector : TG2GraphConnectorFMX;
  begin
    Connector := aModule.OutConnector[ aControl.CodeRef] as TG2GraphConnectorFMX;
    if not assigned(Connector) then
      raise Exception.Create('Connector ' + IntToStr(aControl.CodeRef) + ' not found in module ' + aModule.ModuleName);
    Connector.G2Connector := aControl as TG2Connector;
  end;

  procedure SetCommonProperties( aG2Control : TG2BaseControl; aID, aXPos, aYPos : integer);
  begin
    aG2Control.Parent := aControl;
    aG2Control.ModuleIndex := aModule.ModuleIndex;
    aG2Control.Location := aModule.Location;
    aG2Control.ID := aID;
    //aG2Control.Scale.X := MODULE_WIDTH/UNITS_COL;
    //aG2Control.Scale.Y := MODULE_WIDTH/UNITS_COL;
    //aG2Control.UnscaledLeft := aXPos * MODULE_WIDTH/UNITS_COL;
    //aG2Control.UnscaledTop := aYPos * MODULE_HEIGHT/(aModule.HeightUnits*UNITS_ROW);
    aG2Control.UnscaledLeft := aXPos + UNIT_MARGIN;
    aG2Control.UnscaledTop := aYPos + (1 + aYPos / UNITS_ROW) * UNIT_MARGIN;
    if assigned(aModule.OnCreateG2Control) then
      aModule.OnCreateG2Control(aModule, aG2Control);
  end;

  function CreateInput( aConnectorDef : TG2ConnectorDef): TG2Connector;
  begin
    Result := TG2Connector.Create(aControl);
    Result.BeginUpdate;
    SetCommonProperties( Result, aConnectorDef.ID, aConnectorDef.XPos, aConnectorDef.YPos);
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.Bandwidth;
    AssignToInConnector( Result);
    //SetEvents(Result);
    Result.EndUpdate;
  end;

  function CreateOutput( aConnectorDef : TG2ConnectorDef): TG2Connector;
  begin
    Result := TG2Connector.Create(aControl);
    Result.BeginUpdate;
    SetCommonProperties( Result, aConnectorDef.ID, aConnectorDef.XPos, aConnectorDef.YPos);
    Result.CodeRef := aConnectorDef.CodeRef;
    Result.ConnectorType := aConnectorDef.ConnectorType;
    Result.BandWidth := aConnectorDef.Bandwidth;
    AssignToOutConnector( Result);
    //SetEvents(Result);
    Result.EndUpdate;
  end;
begin
  for c := 0 to High(ModuleInputs) do begin
    if ModuleInputs[c].ModuleID = aModule.TypeID then begin
      G2Control := CreateInput( ModuleInputs[c]);
    end;
  end;

  for c := 0 to High(ModuleOutputs) do begin
    if ModuleOutputs[c].ModuleID = aModule.TypeID then begin
      G2Control := CreateOutput( ModuleOutputs[c]);
    end;
  end;
end;

procedure TframeMiniSlot.CreateModuleFMX(Sender: TObject; aLocation : TLocationType;
  aModule: TG2GraphModuleFMX);
begin
  case aLocation of
  ltPatch:;
  ltFX:
    begin
       aModule.G2Module := TG2Module.Create(aModule);
       aModule.G2Module.Parent := blFX;
       aModule.InitPanel;
       aModule.G2Module.BeginUpdate;
       aModule.G2Module.OnMouseUp := ModuleMouseUp;
       //aModule.G2Module.UnscaledTop := aModule.RowIndex * MODULE_HEIGHT;
       //aModule.G2Module.UnscaledLeft := aModule.Col * MODULE_WIDTH;
       //aModule.G2Module.UnscaledWidth := MODULE_WIDTH;
       //aModule.G2Module.UnscaledHeight := MODULE_HEIGHT;
       aModule.G2Module.EndUpdate;
       //aModule.G2Module.Visible := rbLocation.Value = 0;
       aModule.OnCreateG2Control := CreateG2Control;

       CreateConnectors(aModule, aModule.G2Module);
       //Patch.SortLeds;
    end;
  ltVA:
    begin
       aModule.G2Module := TG2Module.Create(aModule);
       aModule.G2Module.Parent := blVA;
       aModule.InitPanel;
       aModule.G2Module.BeginUpdate;
       aModule.G2Module.OnMouseUp := ModuleMouseUp;
       //aModule.G2Module.UnscaledTop := aModule.RowIndex * MODULE_HEIGHT;
       //aModule.G2Module.UnscaledLeft := aModule.Col * MODULE_WIDTH;
       //aModule.G2Module.UnscaledWidth := MODULE_WIDTH;
       //aModule.G2Module.UnscaledHeight := MODULE_HEIGHT;
       aModule.G2Module.EndUpdate;
       //aModule.G2Module.Visible := rbLocation.Value = 1;
       aModule.OnCreateG2Control := CreateG2Control;

       CreateConnectors(aModule, aModule.G2Module);
       //CreateModuleControls(aModule, aModule.G2Module);
       //Patch.SortLeds;
    end;
  end;
end;


procedure TframeMiniSlot.CreateCableFMX(Sender: TObject; aLocation : TLocationType;
  aCable: TG2GraphCableFMX);
begin
  case aLocation of
  ltPatch:;
  ltFX:
    begin
      aCable.G2Control := TG2Cable.Create( aCable);
      aCable.G2Control.Parent := blFX;
      aCable.G2Control.CableStyle := csFlat;
      aCable.G2Control.SegmentSize := 120;
    end;
  ltVA:
    begin
      aCable.G2Control := TG2Cable.Create( aCable);
      aCable.G2Control.Parent := blVA;
      aCable.G2Control.CableStyle := csFlat;
      aCable.G2Control.SegmentSize := 120;
    end;
  end;
end;

destructor TframeMiniSlot.Destroy;
begin
  inherited;
end;

procedure TframeMiniSlot.ModuleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  dlg: TfrmModule;
  Module : TG2GraphModuleFMX;
  Cable : TG2GraphCableFMX;
  i, j : integer;
begin
  //Module := (Sender as TMiniModule).FModule;
  Module := FPatch.Modules[ ord((Sender as TG2Module).Location), (Sender as TG2Module).ModuleIndex] as TG2GraphModuleFMX;

  dlg := TfrmModule.Create(nil);
  dlg.blModule.Zoom := dlg.ClientWidth / (UNITS_COL+UNIT_MARGIN*2);
  dlg.Module := TG2Module.Create(dlg);
  dlg.Module.Parent := dlg.blModule;
  dlg.Module.UnscaledWidth := UNITS_COL+UNIT_MARGIN*2;
  dlg.Module.UnscaledHeight := (UNITS_ROW+UNIT_MARGIN*2) * Module.HeightUnits;
  dlg.Module.ModuleLabel := Module.ModuleName;
  dlg.Module.StateStyleList.DefaultFill.Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);
  dlg.Module.StateStyleList.SelectedFill.Color := ConvertToAlpha(ModuleColors[Module.ModuleColor]);

  CreateModuleControls(Module, dlg.Module);

  for i := 0 to Module.InConnectorCount - 1 do begin
    if Module.InConnector[i].CableCount > 0 then
      (Module.InConnector[i] as TG2GraphConnectorFMX).G2Connector.Connected := True;
  end;

  for i := 0 to Module.OutConnectorCount - 1 do begin
    if Module.OutConnector[i].CableCount > 0 then
      (Module.OutConnector[i] as TG2GraphConnectorFMX).G2Connector.Connected := True;
  end;

  Module.InvalidateControl;

  {for i := 0 to Module.InConnectorCount - 1 do begin
    for j := 0 to Module.InConnector[i].CableCount - 1 do begin
      Cable := Module.InConnector[i].Cables[j] as TG2GraphCableFMX;
      Cable.G2Control.Parent := dlg.blModule;
      Cable.Invalidate;
    end;
  end;

  for i := 0 to Module.OutConnectorCount - 1 do begin
    for j := 0 to Module.OutConnector[i].CableCount - 1 do begin
      Cable := Module.OutConnector[i].Cables[j] as TG2GraphCableFMX;
      Cable.G2Control.Parent := dlg.blModule;
      Cable.Invalidate;
    end;
  end;}

  dlg.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult = mrOK then;
      dlg.DisposeOf;
    end);
end;

procedure TframeMiniSlot.Panel1Click(Sender: TObject);
begin
  SaveScreenShot(Parent as TForm, 'frmMiniSlot');
end;

procedure TframeMiniSlot.rbLocationChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdatePatch;
end;

procedure TframeMiniSlot.SetPatch(const Value: TG2GraphPatchFMX);
begin
  FPatch := Value;
  FPatch.OnCreateModuleFMX := CreateModuleFMX;
  FPatch.OnCreateCableFMX := CreateCableFMX;
end;

procedure TframeMiniSlot.SetSelectedLocation(const Value: TLocationType);
begin
  if FSelectedLocation <> Value then begin
    FSelectedLocation := Value;
    UpdatePatch;
  end;
end;

procedure TframeMiniSlot.UpdatePatch;
var i : integer;
    Module : TG2GraphModuleFMX;
    Cable : TG2GraphCableFMX;
    maxx, maxy : single;
begin
  if FSelectedLocation = ltFX then begin
    blFX.Visible := True;
    blVA.Visible := False;

    maxx := 0;
    maxy := 0;

    for i := 0 to FPatch.PatchPart[0].ModuleList.Count - 1 do begin
      Module := FPatch.PatchPart[0].ModuleList[i] as TG2GraphModuleFMX;
      //Module.G2Module.UnscaledTop := Module.RowIndex * MODULE_HEIGHT;

      if Module.G2Module.Position.X + Module.G2Module.Width > maxx then
        maxx := Module.G2Module.Position.X + Module.G2Module.Width;

      if Module.G2Module.Position.Y + Module.G2Module.Height > maxy then
        maxy := Module.G2Module.Position.Y + Module.G2Module.Height;

      Module.InvalidateControl;
      Module.InvalidateCables;
    end;
    blFX.Width := maxx;
    blFX.Height := maxy;
  end else begin
    blFX.Visible := False;
    blVA.Visible := True;

    for i := 0 to FPatch.PatchPart[1].ModuleList.Count - 1 do begin
      Module := FPatch.PatchPart[1].ModuleList[i] as TG2GraphModuleFMX;
      //Module.G2Module.UnscaledTop := Module.RowIndex * MODULE_HEIGHT;

      if Module.G2Module.Position.X + Module.G2Module.Width > maxx then
        maxx := Module.G2Module.Position.X + Module.G2Module.Width;

      if Module.G2Module.Position.Y + Module.G2Module.Height > maxy then
        maxy := Module.G2Module.Position.Y + Module.G2Module.Height;

      Module.InvalidateControl;
      Module.InvalidateCables;
    end;
    blVA.Width := maxx;
    blVA.Height := maxy;
  end;
end;

{ TMiniModule }

constructor TMiniModule.Create(AOwner: TComponent);
var StyleSet : TG2StyleSet;
begin
  inherited;
  FModule := AOwner as TG2GraphModuleFMX;

  StyleSet := StateStyleList.StateStyle[ csDefault];
  StyleSet.Font.Size := 16;
end;

destructor TMiniModule.Destroy;
begin
  inherited;
end;

procedure TMiniModule.SetCol(const aValue: integer);
begin
  UnscaledLeft := aValue * MODULE_WIDTH;
end;

procedure TMiniModule.SetRow(const aValue: integer);
begin
  UnscaledTop := FModule.RowIndex * MODULE_HEIGHT;
  //UnscaledTop := aValue * MODULE_HEIGHT;
end;

procedure TMiniModule.SetZoomed(const Value: boolean);
begin
  if Zoomed <> Value then begin
    FZoomed := Value;
  end;
end;

procedure TMiniModule.PaintOn( aCanvas : TCanvas);
var TextColor : TAlphaColor;
    StyleSet : TG2StyleSet;
    Rect : TRectF;
begin
  if Selected  then
    StyleSet := StateStyleList.StateStyle[ csSelected]
  else
    StyleSet := StateStyleList.StateStyle[ csDefault];

  StyleSet.ApplyStyleSet( aCanvas);

  Rect := UnscaledBoundsRect;
  Rect.Left := Rect.Left + UNIT_MARGIN;
  Rect.Top := Rect.Top + UNIT_MARGIN;
  Rect.Right := Rect.Right - UNIT_MARGIN;
  Rect.Bottom := Rect.Bottom - UNIT_MARGIN;

  aCanvas.FillRect( Rect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect( Rect, 0, 0, [], AbsoluteOpacity);

  DrawTextExt( aCanvas,
               StyleSet.Font,
               PointF(Rect.Left, Rect.Top),
               PointF(Rect.Width, Rect.Height),
               StyleSet.FontColor,
               ModuleLabel,
               TTextAlign.taLeading, TTextAlign.taLeading);
end;

end.
