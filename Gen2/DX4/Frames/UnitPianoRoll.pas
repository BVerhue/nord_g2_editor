unit UnitPianoRoll;
interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.UIConsts,
  FMX.Types,
{$IF Defined(VER260) or Defined(VER270) or Defined(VER340)}
  FMX.Graphics,
{$ENDIF}
  FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BVE.NMG2ControlsFMX, FMX.Layouts, BVE.NMG2File,
  BVE.NMG2GraphFMX, BVE.NMG2Types, FMX.Objects, FMX.Controls.Presentation;
type
  TframePianoRoll = class(TFrame)
    Layout1: TLayout;
    sbPianoRoll: TVertScrollBox;
    btNote1: TG2BtnText;
    idRotate: TG2BtnIncDec;
    idTranspose: TG2BtnIncDec;
    btRandom: TG2BtnText;
    btClose: TG2BtnText;
    G2Label1: TG2Label;
    G2Label2: TG2Label;
    btNote2: TG2BtnText;
    btNote3: TG2BtnText;
    BtNote4: TG2BtnText;
    btNote5: TG2BtnText;
    btNote6: TG2BtnText;
    btNote7: TG2BtnText;
    btNote8: TG2BtnText;
    btNote9: TG2BtnText;
    btNote10: TG2BtnText;
    btNote11: TG2BtnText;
    btNote12: TG2BtnText;
    btNote13: TG2BtnText;
    btNote14: TG2BtnText;
    btNote15: TG2BtnText;
    btNote16: TG2BtnText;
    Panel1: TPanel;
    btClear: TG2BtnText;
    lKeyboard: TLayout;
    Layout2: TLayout;
    Rectangle12: TRectangle;
    Rectangle11: TRectangle;
    Rectangle10: TRectangle;
    Rectangle9: TRectangle;
    Rectangle8: TRectangle;
    Rectangle7: TRectangle;
    Rectangle6: TRectangle;
    Rectangle5: TRectangle;
    Rectangle4: TRectangle;
    Rectangle3: TRectangle;
    Rectangle2: TRectangle;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Layout3: TLayout;
    Rectangle13: TRectangle;
    Rectangle14: TRectangle;
    Rectangle15: TRectangle;
    Rectangle16: TRectangle;
    Rectangle17: TRectangle;
    Rectangle18: TRectangle;
    Rectangle19: TRectangle;
    Rectangle20: TRectangle;
    Rectangle21: TRectangle;
    Rectangle22: TRectangle;
    Rectangle23: TRectangle;
    Rectangle24: TRectangle;
    Label2: TLabel;
    Layout4: TLayout;
    Rectangle25: TRectangle;
    Rectangle26: TRectangle;
    Rectangle27: TRectangle;
    Rectangle28: TRectangle;
    Rectangle29: TRectangle;
    Rectangle30: TRectangle;
    Rectangle31: TRectangle;
    Rectangle32: TRectangle;
    Rectangle33: TRectangle;
    Rectangle34: TRectangle;
    Rectangle35: TRectangle;
    Rectangle36: TRectangle;
    Label3: TLabel;
    Layout5: TLayout;
    Rectangle37: TRectangle;
    Rectangle38: TRectangle;
    Rectangle39: TRectangle;
    Rectangle40: TRectangle;
    Rectangle41: TRectangle;
    Rectangle42: TRectangle;
    Rectangle43: TRectangle;
    Rectangle44: TRectangle;
    Rectangle45: TRectangle;
    Rectangle46: TRectangle;
    Rectangle47: TRectangle;
    Rectangle48: TRectangle;
    Label4: TLabel;
    Layout6: TLayout;
    Rectangle49: TRectangle;
    Rectangle50: TRectangle;
    Rectangle51: TRectangle;
    Rectangle52: TRectangle;
    Rectangle53: TRectangle;
    Rectangle54: TRectangle;
    Rectangle55: TRectangle;
    Rectangle56: TRectangle;
    Rectangle57: TRectangle;
    Rectangle58: TRectangle;
    Rectangle59: TRectangle;
    Rectangle60: TRectangle;
    Label5: TLabel;
    Layout7: TLayout;
    Rectangle61: TRectangle;
    Rectangle62: TRectangle;
    Rectangle63: TRectangle;
    Rectangle64: TRectangle;
    Rectangle65: TRectangle;
    Rectangle66: TRectangle;
    Rectangle67: TRectangle;
    Rectangle68: TRectangle;
    Rectangle69: TRectangle;
    Rectangle70: TRectangle;
    Rectangle71: TRectangle;
    Rectangle72: TRectangle;
    Label6: TLabel;
    Layout8: TLayout;
    Rectangle73: TRectangle;
    Rectangle74: TRectangle;
    Rectangle75: TRectangle;
    Rectangle76: TRectangle;
    Rectangle77: TRectangle;
    Rectangle78: TRectangle;
    Rectangle79: TRectangle;
    Rectangle80: TRectangle;
    Rectangle81: TRectangle;
    Rectangle82: TRectangle;
    Rectangle83: TRectangle;
    Rectangle84: TRectangle;
    Label7: TLabel;
    Layout9: TLayout;
    Rectangle85: TRectangle;
    Rectangle86: TRectangle;
    Rectangle87: TRectangle;
    Rectangle88: TRectangle;
    Rectangle89: TRectangle;
    Rectangle90: TRectangle;
    Rectangle91: TRectangle;
    Rectangle92: TRectangle;
    Rectangle93: TRectangle;
    Rectangle94: TRectangle;
    Rectangle95: TRectangle;
    Rectangle96: TRectangle;
    Label8: TLabel;
    Line1: TLine;
    procedure btCloseChangeValue(Sender: TObject; const aValue: Integer);
    procedure sbPianoRollMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure NoteChangeValue(Sender: TObject; const aValue : integer);
    procedure btRandomChangeValue(Sender: TObject; const aValue: Integer);
    procedure btClearChangeValue(Sender: TObject; const aValue: Integer);
    procedure idTransposeButtonClick(Sender: TObject; const aIndex: Integer);
    procedure idRotateButtonClick(Sender: TObject; const aIndex: Integer);
    procedure FrameResize(Sender: TObject);
  private
    FSeqModule : TG2GraphModuleFMX;
    FNoteBtns : TObjectList<TG2BtnText>;
    FLedGroupIndex : integer;
    FLed : TRectangle;
    FOnClose : TNotifyEvent;
    FLedValue: integer;
    procedure SetSeqModule(const Value: TG2GraphModuleFMX);
    procedure SetLedValue(const Value: integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure UpdateControls;

    function GetNoteRect( aNoteIndex : integer): TRectF;

    procedure SeqLed(Sender : TObject; const aLocation : TLocationType; const aModuleIndex, aGroupIndex, aCodeRef, aValue : byte);

    procedure ScrollInView;
    property LedValue : integer read FLedValue write SetLedValue;
    property SeqModule : TG2GraphModuleFMX read FSeqModule write SetSeqModule;
    property OnClose : TNotifyEvent read FOnClose write FOnCLose;
  end;
implementation
{$R *.fmx}
{ TframePianoRoll }
procedure TframePianoRoll.btClearChangeValue(Sender: TObject;
  const aValue: Integer);
var i : integer;
begin
  if not assigned(FSeqModule) then
    exit;
    for i := 0 to 15 do
      FSeqModule.Parameter[ 0 + i].SetValue( 64);
  UpdateControls;
end;
procedure TframePianoRoll.btCloseChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 1 then begin
    if assigned(FOnClose) then
      FOnClose(self)
    else
      Visible := False;
  end;
end;
procedure TframePianoRoll.btRandomChangeValue(Sender: TObject;
  const aValue: Integer);
var i, j, k, d, nd : integer;
    Value, Oct, Note : byte;
begin
  if not assigned( FSeqModule) then
    exit;

  if aValue = 0 then begin
    for i := 0 to 15 do begin
      Value := FSeqModule.Parameter[ 0 + i].GetValue + (trunc(Random * 36) - 18);
      {if FNoteQuantCount > 0 then begin
        Oct := GetOctaaf( Value);
        Note := GetNoot( Value);
        d := 12;
        k := 0;
        for j := 0 to FNoteQuantCount - 1 do begin
          nd := abs(FNoteQuant[j] - Note);
          if nd<d then begin
            d := nd;
            k := j;
          end
        end;
        Value := GetValue(Oct, FNoteQuant[k]);
      end;}
      FSeqModule.Parameter[ 0 + i].SetValue( Value);
    end;
    UpdateControls;
  end;
end;
constructor TframePianoRoll.Create(AOwner: TComponent);
begin
  inherited;
  FNoteBtns := TObjectList<TG2BtnText>.Create(False);
  FNoteBtns.Add(btNote1);
  FNoteBtns.Add(btNote2);
  FNoteBtns.Add(btNote3);
  FNoteBtns.Add(btNote4);
  FNoteBtns.Add(btNote5);
  FNoteBtns.Add(btNote6);
  FNoteBtns.Add(btNote7);
  FNoteBtns.Add(btNote8);
  FNoteBtns.Add(btNote9);
  FNoteBtns.Add(btNote10);
  FNoteBtns.Add(btNote11);
  FNoteBtns.Add(btNote12);
  FNoteBtns.Add(btNote13);
  FNoteBtns.Add(btNote14);
  FNoteBtns.Add(btNote15);
  FNoteBtns.Add(btNote16);
  FLed := TRectangle.Create(self);
  FLed.Fill.Kind := TBrushKind.bkNone;
  FLed.Stroke.Thickness := 3;
  FLed.Stroke.Color := claLime;
  FLed.Parent := sbPianoRoll;
  FLed.HitTest := False;
  LedValue := 0;
  //sbPianoRoll.AniCalculations.Animation := True;
  //sbPianoRoll.AniCalculations.BoundsAnimation := True;
  //sbPianoRoll.AniCalculations.TouchTracking := [ttVertical];
end;
destructor TframePianoRoll.Destroy;
begin
  FNoteBtns.Free;
  inherited;
end;
procedure TframePianoRoll.FrameResize(Sender: TObject);
begin
  UpdateControls;
end;
function TframePianoRoll.GetNoteRect(aNoteIndex: integer): TRectF;
var Param : TG2FileParameter;
    KeyNo : integer;
    w, h : single;
begin
  if not assigned(FSeqModule) then
    exit;
  Param := FSeqModule.Parameter[aNoteIndex];
  if not assigned(Param) then
    exit;
  KeyNo := Param.GetValue;
  w := (Width - 16 - lKeyboard.Width) / 16;
  h := 25;
  Result.Left := lKeyboard.Width + aNoteIndex * w;
  Result.Top := lKeyBoard.Height - (KeyNo + 1) * 24;
  Result.Right := Result.Left + w;
  Result.Bottom := Result.Top + h;
end;
procedure TframePianoRoll.idRotateButtonClick(Sender: TObject;
  const aIndex: Integer);
var NoteValue, BtnValue : byte;
    i : integer;
begin
  if not assigned(FSeqModule) then
    exit;
  if aIndex = 0 then begin
    BtnValue  := FSeqModule.Parameter[16].GetValue;
    NoteValue := FSeqModule.Parameter[0].GetValue;
    for i := 1 to 15 do begin
      FSeqModule.Parameter[16 + i - 1].SetValue( FSeqModule.Parameter[16 + i].GetValue);
      FSeqModule.Parameter[ 0 + i - 1].SetValue( FSeqModule.Parameter[ 0 + i].GetValue);
    end;
    FSeqModule.Parameter[31].SetValue( BtnValue);
    FSeqModule.Parameter[15].SetValue( NoteValue);
  end else begin
    BtnValue  := FSeqModule.Parameter[31].GetValue;
    NoteValue := FSeqModule.Parameter[15].GetValue;
    for i := 14 downto 0 do begin
      FSeqModule.Parameter[16 + i + 1].SetValue( FSeqModule.Parameter[16 + i].GetValue);
      FSeqModule.Parameter[ 0 + i + 1].SetValue( FSeqModule.Parameter[ 0 + i].GetValue);
    end;
    FSeqModule.Parameter[16].SetValue( BtnValue);
    FSeqModule.Parameter[ 0].SetValue( NoteValue);
  end;
  UpdateControls;
end;
procedure TframePianoRoll.idTransposeButtonClick(Sender: TObject;
  const aIndex: Integer);
var i : integer;
    Value : integer;
begin
  if not assigned(FSeqModule) then
    exit;
  if aIndex = 0 then begin
    for i := 0 to 15 do begin
      Value := FSeqModule.Parameter[ 0 + i].GetValue;
      if Value > 0 then
        Value := Value - 1;
      FSeqModule.Parameter[ 0 + i].SetValue( Value);
    end;
  end else begin
    for i := 0 to 15 do begin
      Value := FSeqModule.Parameter[ 0 + i].GetValue;
      if Value < 127 then
        Value := Value + 1;
      FSeqModule.Parameter[ 0 + i].SetValue( Value);
    end;
  end;
  UpdateControls;
end;
procedure TframePianoRoll.NoteChangeValue(Sender: TObject;
  const aValue: integer);
var Param : TG2FileParameter;
begin
  if Sender is TG2BtnText then begin
    Param := FSeqModule.Parameter[16 + (Sender as TG2BtnText).Tag] as TG2FileParameter;
  // TODO  Param.SelectControl( Sender as TG2BtnText);
    Param.SetValue( aValue);
  end;
end;
procedure TframePianoRoll.sbPianoRollMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var NoteNo, KeyNo : integer;
begin
  NoteNo := trunc(16 * (X - lKeyboard.Width) / (Width - lKeyboard.Width));
  KeyNo := trunc(96 * (lKeyboard.Height - (Y + sbPianoRoll.ViewportPosition.Y)) / lKeyboard.Height);
  if assigned(FSeqModule) then begin
    FSeqModule.Parameter[NoteNo].SetValue( KeyNo);
    UpdateControls;
  end;
end;
procedure TframePianoRoll.ScrollInView;
var Posy : single;
begin
  if not assigned(FSeqModule) then
    exit;
  PosY := lKeyBoard.Height - (FSeqModule.Parameter[0].GetValue + 1) * 24;
  sbPianoRoll.ViewportPosition := PointF(0, (PosY - sbPianoRoll.Height/2));
end;
procedure TframePianoRoll.SeqLed(Sender : TObject; const aLocation : TLocationType;
  const aModuleIndex, aGroupIndex, aCodeRef, aValue : byte);
var i : integer;
begin
  if (FSeqModule.Location = aLocation) and (FSeqModule.ModuleIndex = aModuleIndex) then
    if aValue < 16 then
      LedValue := aValue;
end;
procedure TframePianoRoll.SetLedValue(const Value: integer);
var Rect : TRectF;
begin
  FLedValue := Value;
  Rect := GetNoteRect(Value);
  FLed.Position.X := Rect.Left-3;
  FLed.Position.Y := Rect.Top-3;
  FLed.Width := (Rect.Right - Rect.Left)+6;
  FLed.Height := (Rect.Bottom - Rect.Top)+6;
end;
procedure TframePianoRoll.SetSeqModule(const Value: TG2GraphModuleFMX);
var i : integer;
    ModulePanel : TG2Module;
begin
  if Value <> FSeqModule then begin
    FSeqModule := Value;
    if assigned(Value) then begin
      ModulePanel := FSeqModule.G2Module;
      FLedGroupIndex := -1;
      i := 0;
      while (i<ModulePanel.ChildrenCount) and not((ModulePanel.Children[i] is TG2Led) and ((ModulePanel.Children[i] as TG2Led).LedType = ltSequencer)) do
        inc(i);
      if (i<ModulePanel.ChildrenCount) then begin
        FLedGroupIndex := (ModulePanel.Children[i] as TG2Led).LedGroupID;
      end;
      LedValue := 0;
      UpdateControls;
    end;
  end;
end;
procedure TframePianoRoll.UpdateControls;
var i, keyno, o, k : integer;
    KeyType : TKeyType;
    BtnPos : single;
    ParamStep, ParamEvent : TG2FileParameter;
    KeyName : string;
begin
  if not assigned(FSeqModule) then
    exit;
  for i := 0 to 15 do begin
    ParamStep := FSeqModule.Parameter[i];
    ParamEvent := FSeqModule.Parameter[16 + i];
    if not(assigned(ParamStep) and assigned(ParamEvent)) then
      exit;

    KeyNo := ParamStep.GetValue;
    KeyName := GetKeyName(KeyNo);
    FNoteBtns[i].ButtonText.Clear;
    FNoteBtns[i].ButtonText.Add( KeyName);
    FNoteBtns[i].ButtonText.Add( KeyName);
    FNoteBtns[i].Value := ParamEvent.GetValue;
    FNoteBtns[i].UnscaledRect := GetNoteRect(i);
    LedValue := FLedValue;
  end;
end;
end.
