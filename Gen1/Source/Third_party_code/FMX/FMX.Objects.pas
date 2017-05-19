{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011 Embarcadero Technologies, Inc.      }
{                                                       }
{*******************************************************}

unit FMX.Objects;

{$I FMX.Defines.inc}
{$H+}

interface

uses
  System.Classes, System.Types, System.UITypes, FMX.Types, FMX.Video;

{$SCOPEDENUMS ON}

type

{ TShape }

  TShape = class(TControl)
  private
    FFill: TBrush;
    FStrokeThickness: Single;
    FStroke: TBrush;
    FStrokeCap: TStrokeCap;
    FStrokeJoin: TStrokeJoin;
    FStrokeDash: TStrokeDash;
    procedure SetFill(const Value: TBrush);
    procedure SetStroke(const Value: TBrush);
    procedure SetStrokeThickness(const Value: Single);
    function IsStrokeThicknessStored: Boolean;
    procedure SetStrokeCap(const Value: TStrokeCap);
    procedure SetStrokeJoin(const Value: TStrokeJoin);
    procedure SetStrokeDash(const Value: TStrokeDash);
  protected
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
    function GetShapeRect: TRectF;
    procedure Painting; override;
    procedure AfterPaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TBrush read FStroke write SetStroke;
    property StrokeThickness: Single read FStrokeThickness
      write SetStrokeThickness stored IsStrokeThicknessStored;
    property StrokeCap: TStrokeCap read FStrokeCap write SetStrokeCap default TStrokeCap.scFlat;
    property StrokeDash: TStrokeDash read FStrokeDash write SetStrokeDash default TStrokeDash.sdSolid;
    property StrokeJoin: TStrokeJoin read FStrokeJoin write SetStrokeJoin default TStrokeJoin.sjMiter;
    property ShapeRect: TRectF read GetShapeRect;
  end;

{ TLine }

  TLineType = (ltDiagonal, ltTop, ltLeft);

  TLine = class(TShape)
  private
    FLineType: TLineType;
    procedure SetLineType(const Value: TLineType);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property LineType: TLineType read FLineType write SetLineType;
  end;

{ TRectangle }

  TRectangle = class(TShape)
  private
    FYRadius: Single;
    FXRadius: Single;
    FCorners: TCorners;
    FCornerType: TCornerType;
    FSides: TSides;
    function IsCornersStored: Boolean;
    function IsSidesStored: Boolean;
  protected
    procedure SetXRadius(const Value: Single); virtual;
    procedure SetYRadius(const Value: Single); virtual;
    procedure SetCorners(const Value: TCorners); virtual;
    procedure SetCornerType(const Value: TCornerType); virtual;
    procedure SetSides(const Value: TSides); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners
      stored IsCornersStored;
    property CornerType: TCornerType read FCornerType write SetCornerType
      default TCornerType.ctRound;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
  end;

{ TRoundRect }

  TRoundRect = class(TShape)
  private
    FCorners: TCorners;
    function IsCornersStored: Boolean;
  protected
    procedure SetCorners(const Value: TCorners); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property Corners: TCorners read FCorners write SetCorners stored IsCornersStored;
  end;

{ TCalloutRectangle }

  TCalloutPosition = (cpTop, cpLeft, cpBottom, cpRight);

  TCalloutRectangle = class(TRectangle)
  private
    FPath: TPathData;
    FCalloutWidth: Single;
    FCalloutLength: Single;
    FCalloutPosition: TCalloutPosition;
    FCalloutOffset: Single;
    procedure SetCalloutWidth(const Value: Single);
    procedure SetCalloutLength(const Value: Single);
    procedure SetCalloutPosition(const Value: TCalloutPosition);
    procedure SetCalloutOffset(const Value: Single);
  protected
    procedure CreatePath;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fill;
    property CalloutWidth: Single read FCalloutWidth write SetCalloutWidth;
    property CalloutLength: Single read FCalloutLength write SetCalloutLength;
    property CalloutPosition: TCalloutPosition read FCalloutPosition  write SetCalloutPosition
      default TCalloutPosition.cpTop;
    property CalloutOffset: Single read FCalloutOffset write SetCalloutOffset;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

{ TEllipse }

  TEllipse = class(TShape)
  protected
    function PointInObject(X, Y: Single): Boolean; override;
    procedure Paint; override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

{ TCircle }

  TCircle = class(TEllipse)
  protected
    procedure Paint; override;
  end;

{ TPie }

  TPie = class(TEllipse)
  private
    FStartAngle: Single;
    FEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    procedure SetStartAngle(const Value: Single);
  protected
    function PointInObject(X, Y: Single): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property EndAngle: Single read FEndAngle write SetEndAngle;
  end;

{ TArc }

  TArc = class(TEllipse)
  private
    FStartAngle: Single;
    FEndAngle: Single;
    procedure SetEndAngle(const Value: Single);
    procedure SetStartAngle(const Value: Single);
  protected
    function PointInObject(X, Y: Single): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property EndAngle: Single read FEndAngle write SetEndAngle;
  end;

  TPathWrapMode = (pwOriginal, pwFit, pwStretch, pwTile);

{ TCustomPath }

  TCustomPath = class(TShape)
  private
    FData, FCurrent: TPathData;
    FWrapMode: TPathWrapMode;
    procedure SetData(const Value: TPathData);
    procedure SetWrapMode(const Value: TPathWrapMode);
  protected
    function PointInObject(X, Y: Single): Boolean; override;
    procedure DoChanged(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure UpdatePath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Data: TPathData read FData write SetData;
    property WrapMode: TPathWrapMode read FWrapMode write SetWrapMode default TPathWrapMode.pwStretch;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

{ TPath }

  TPath = class(TCustomPath)
  published
    property Data;
    property WrapMode;
  end;

{ TText }

  TText = class(TShape)
  private
    FText: string;
    FFont: TFont;
    FVertTextAlign: TTextAlign;
    FHorzTextAlign: TTextAlign;
    FWordWrap: Boolean;
    FAutoSize: Boolean;
    FStretch: Boolean;
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetHorzTextAlign(const Value: TTextAlign);
    procedure SetVertTextAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetAutoSize(const Value: Boolean);
    procedure SetStretch(const Value: Boolean);
  protected
    procedure FontChanged(Sender: TObject); virtual;
    procedure Paint; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure AdjustSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Fill;
    property Font: TFont read FFont write SetFont;
    property HorzTextAlign: TTextAlign read FHorzTextAlign write SetHorzTextAlign
      default TTextAlign.taCenter;
    property VertTextAlign: TTextAlign read FVertTextAlign write SetVertTextAlign
      default TTextAlign.taCenter;
    property Text: string read FText write SetText;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default True;
  end;

{ TImage }

  TImageWrapMode = (iwOriginal, iwFit, iwStretch, iwTile);

  TImage = class(TControl)
  private
    FBitmap: TBitmap;
    FBitmapMargins: TBounds;
    FWrapMode: TImageWrapMode;
    FDisableInterpolation: Boolean;
    procedure SetBitmap(const Value: TBitmap);
    procedure SetWrapMode(const Value: TImageWrapMode);
  protected
    procedure DoBitmapChanged(Sender: TObject); virtual;
    procedure Paint; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BitmapMargins: TBounds read FBitmapMargins write FBitmapMargins;
    property WrapMode: TImageWrapMode read FWrapMode write SetWrapMode default TImageWrapMode.iwFit;
    property DisableInterpolation: Boolean read FDisableInterpolation write FDisableInterpolation default False;
  end;

{ TPaintBox }

  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas) of object;

  TPaintBox = class(TControl)
  private
    FOnPaint: TPaintEvent;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
  end;

{ TSelection }

  TSelection = class(TControl)
  private
    FParentBounds: Boolean;
    FOnChange: TNotifyEvent;
    FHideSelection: Boolean;
    FMinSize: Integer;
    FOnTrack: TNotifyEvent;
    FProportional: Boolean;
    FGripSize: Single;
    procedure SetHideSelection(const Value: Boolean);
    procedure SetMinSize(const Value: Integer);
    procedure SetGripSize(const Value: Single);
  protected
    FRatio: Single;
    FMove, FLeftTop, FLeftBottom, FRightTop, FRightBottom: Boolean;
    FLeftTopHot, FLeftBottomHot, FRightTopHot, FRightBottomHot: Boolean;
    FDownPos, FMovePos: TPointF;
    function GetAbsoluteRect: TRectF; override;
    function PointInObject(X, Y: Single): Boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
  published
    property GripSize: Single read FGripSize write SetGripSize;
    property ParentBounds: Boolean read FParentBounds write FParentBounds default True;
    property HideSelection: Boolean read FHideSelection write SetHideSelection;
    property MinSize: Integer read FMinSize write SetMinSize default 15;
    property Proportional: Boolean read FProportional write FProportional;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
  end;

{ TSelectionPoint }

  TSelectionPoint = class(TControl)
  private
    FOnChange: TNotifyEvent;
    FOnTrack: TNotifyEvent;
    FParentBounds: Boolean;
    FGripSize: Single;
    procedure SetGripSize(const Value: Single);
  protected
    FPressed: Boolean;
    procedure Paint; override;
    procedure SetHeight(const Value: Single); override;
    procedure SetWidth(const Value: Single); override;
    function PointInObject(X, Y: Single): Boolean; override;
    function GetUpdateRect: TRectF; override;
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  published
    property GripSize: Single read FGripSize write SetGripSize;
    property ParentBounds: Boolean read FParentBounds write FParentBounds default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
  end;

implementation

uses System.SysUtils, System.Math, FMX.Effects;

{ TShape }

constructor TShape.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TBrush.Create(TBrushKind.bkSolid, $FF000000);
  FStroke.Color := $FF000000;
  FStroke.OnChanged := StrokeChanged;
  FStrokeThickness := 1;
  SetAcceptsControls(False);
end;

destructor TShape.Destroy;
begin
  FStroke.Free;
  FFill.Free;
  inherited;
end;

function TShape.GetShapeRect: TRectF;
begin
  Result := LocalRect;
  if FStroke.Kind <> TBrushKind.bkNone then
    if Odd(Round(FStrokeThickness)) then
    begin
      InflateRect(Result, -(FStrokeThickness / 2), -(FStrokeThickness / 2));
    end
    else
      InflateRect(Result, -(FStrokeThickness / 2), -(FStrokeThickness / 2));
end;

procedure TShape.FillChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

procedure TShape.StrokeChanged(Sender: TObject);
begin
  if FUpdating = 0 then
    Repaint;
end;

procedure TShape.Painting;
begin
  inherited;
  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);
  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.StrokeCap := FStrokeCap;
  Canvas.StrokeJoin := FStrokeJoin;
  Canvas.StrokeDash := FStrokeDash;
end;

procedure TShape.AfterPaint;
begin
  inherited AfterPaint;
  Canvas.StrokeDash := TStrokeDash.sdSolid;
  Canvas.StrokeThickness := 1;
end;

function TShape.IsStrokeThicknessStored: Boolean;
begin
  Result := StrokeThickness <> 1;
end;

procedure TShape.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;

procedure TShape.SetStroke(const Value: TBrush);
begin
  FStroke.Assign(Value);
end;

procedure TShape.SetStrokeThickness(const Value: Single);
begin
  if FStrokeThickness <> Value then
  begin
    FStrokeThickness := Value;
    Repaint;
  end;
end;

procedure TShape.SetStrokeCap(const Value: TStrokeCap);
begin
  if FStrokeCap <> Value then
  begin
    FStrokeCap := Value;
    Repaint;
  end;
end;

procedure TShape.SetStrokeJoin(const Value: TStrokeJoin);
begin
  if FStrokeJoin <> Value then
  begin
    FStrokeJoin := Value;
    Repaint;
  end;
end;

procedure TShape.SetStrokeDash(const Value: TStrokeDash);
begin
  if FStrokeDash <> Value then
  begin
    FStrokeDash := Value;
    Repaint;
  end;
end;

{ TLine }

constructor TLine.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TLine.Paint;
begin
  case FLineType of
    TLineType.ltTop:
      Canvas.DrawLine(GetShapeRect.TopLeft, PointF(GetShapeRect.Right,
        GetShapeRect.Top), AbsoluteOpacity);
    TLineType.ltLeft:
      Canvas.DrawLine(GetShapeRect.TopLeft, PointF(GetShapeRect.Left,
        GetShapeRect.Bottom), AbsoluteOpacity);
  else
    Canvas.DrawLine(GetShapeRect.TopLeft, GetShapeRect.BottomRight,
      AbsoluteOpacity);
  end;
end;

procedure TLine.SetLineType(const Value: TLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    Repaint;
  end;
end;

{ TEllipse }

procedure TEllipse.Paint;
begin
  Canvas.FillEllipse(GetShapeRect, AbsoluteOpacity);
  Canvas.DrawEllipse(GetShapeRect, AbsoluteOpacity);
end;

function TEllipse.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if Width * Height = 0 then
    Exit;
  if (Sqr((P.X * 2 - Width) / Width) + Sqr((P.Y * 2 - Height) / Height) <= 1)
  then
  begin
    Result := True;
  end;
end;

{ TCircle }

procedure TCircle.Paint;
var
  R: TRectF;
begin
  R := RectF(0, 0, Max(Width, Height), Max(Width, Height));
  FitRect(R, GetShapeRect);
  Canvas.FillEllipse(R, AbsoluteOpacity);
  Canvas.DrawEllipse(R, AbsoluteOpacity);
end;

{ TPie }

constructor TPie.Create(AOwner: TComponent);
begin
  inherited;
  FStartAngle := 0;
  FEndAngle := -90;
end;

procedure TPie.Paint;
var
  P: TPathData;
begin
  P := TPathData.Create;
  P.MoveTo(PointF(Width / 2, Height / 2));
  P.AddArc(PointF(Width / 2, Height / 2), PointF((Width - StrokeThickness) / 2,
    (Height - StrokeThickness) / 2), FStartAngle, FEndAngle - FStartAngle);
  P.LineTo(PointF(Width / 2, Height / 2));
  P.ClosePath;
  Canvas.FillPath(P, AbsoluteOpacity);
  Canvas.DrawPath(P, AbsoluteOpacity);
  P.Free;
end;

function TPie.PointInObject(X, Y: Single): Boolean;
var
  P: TPathData;
begin
  if (Canvas <> nil) then
  begin
    P := TPathData.Create;
    P.MoveTo(PointF(Width / 2, Height / 2));
    P.AddArc(PointF(Width / 2, Height / 2), PointF((Width - StrokeThickness) / 2,
      (Height - StrokeThickness) / 2), FStartAngle, FEndAngle - FStartAngle);
    P.LineTo(PointF(Width / 2, Height / 2));
    P.ClosePath;
    Result := Canvas.PtInPath(AbsoluteToLocal(PointF(X, Y)), P);
    P.Free;
  end
  else
    Result := inherited PointInObject(X, Y);
end;

procedure TPie.SetEndAngle(const Value: Single);
begin
  if FEndAngle <> Value then
  begin
    FEndAngle := Value;
    Repaint;
  end;
end;

procedure TPie.SetStartAngle(const Value: Single);
begin
  if FStartAngle <> Value then
  begin
    FStartAngle := Value;
    Repaint;
  end;
end;

{ TArc }

constructor TArc.Create(AOwner: TComponent);
begin
  inherited;
  Fill.Kind := TBrushKind.bkNone;
  Fill.DefaultKind := TBrushKind.bkNone;
  FStartAngle := 0;
  FEndAngle := -90;
end;

procedure TArc.Paint;
begin
  Canvas.FillArc(PointF(Width / 2, Height / 2),
    PointF((Width - StrokeThickness) / 2, ((Height - StrokeThickness) / 2)),
    FStartAngle, FEndAngle, AbsoluteOpacity);
  Canvas.DrawArc(PointF(Width / 2, Height / 2),
    PointF((Width - StrokeThickness) / 2, ((Height - StrokeThickness) / 2)),
    FStartAngle, FEndAngle, AbsoluteOpacity);
end;

function TArc.PointInObject(X, Y: Single): Boolean;
begin
  Result := inherited PointInObject(X, Y);
end;

procedure TArc.SetEndAngle(const Value: Single);
begin
  if FEndAngle <> Value then
  begin
    FEndAngle := Value;
    Repaint;
  end;
end;

procedure TArc.SetStartAngle(const Value: Single);
begin
  if FStartAngle <> Value then
  begin
    FStartAngle := Value;
    Repaint;
  end;
end;

{ TRectangle }

constructor TRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := AllCorners;
  FXRadius := 0;
  FYRadius := 0;
  FSides := AllSides;
end;

function TRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

function TRectangle.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TRectangle.Paint;
var
  R: TRectF;
  Off: Single;
begin
  R := GetShapeRect;
  
  if Fill.Kind = TBrushKind.bkGrab then
    OffsetRect(R, Position.X, Position.Y);
	
  if Sides <> AllSides then
  begin
    Off := R.Left;
    if not(TSide.sdTop in FSides) then
      R.Top := R.Top - Off;
    if not(TSide.sdLeft in FSides) then
      R.Left := R.Left - Off;
    if not(TSide.sdBottom in FSides) then
      R.Bottom := R.Bottom + Off;
    if not(TSide.sdRight in FSides) then
      R.Right := R.Right + Off;
    Canvas.FillRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
    Canvas.DrawRectSides(GetShapeRect, XRadius, YRadius, FCorners,
      AbsoluteOpacity, Sides, CornerType);
  end
  else
  begin
    Canvas.FillRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
    Canvas.DrawRect(R, XRadius, YRadius, FCorners, AbsoluteOpacity, CornerType);
  end;
end;

procedure TRectangle.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

procedure TRectangle.SetCornerType(const Value: TCornerType);
begin
  if FCornerType <> Value then
  begin
    FCornerType := Value;
    Repaint;
  end;
end;

procedure TRectangle.SetXRadius(const Value: Single);
begin
  if FXRadius <> Value then
  begin
    FXRadius := Value;
    Repaint;
  end;
end;

procedure TRectangle.SetYRadius(const Value: Single);
begin
  if FYRadius <> Value then
  begin
    FYRadius := Value;
    Repaint;
  end;
end;

procedure TRectangle.SetSides(const Value: TSides);
begin
  if FSides <> Value then
  begin
    FSides := Value;
    Repaint;
  end;
end;

{ TRoundRect }

constructor TRoundRect.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := AllCorners;
end;

function TRoundRect.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TRoundRect.Paint;
var
  Radius: Single;
begin
  if Height < Width then
    Radius := RectHeight(GetShapeRect) / 2
  else
    Radius := RectWidth(GetShapeRect) / 2;
  Canvas.FillRect(GetShapeRect, Radius, Radius, FCorners, AbsoluteOpacity);
  Canvas.DrawRect(GetShapeRect, Radius, Radius, FCorners, AbsoluteOpacity);
end;

procedure TRoundRect.SetCorners(const Value: TCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

{ TCalloutRectangle }

constructor TCalloutRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FCalloutWidth := 23;
  FCalloutLength := 11;
  FPath := TPathData.Create;
end;

destructor TCalloutRectangle.Destroy;
begin
  FreeAndNil(FPath);
  inherited;
end;

procedure TCalloutRectangle.CreatePath;
var
  x1, x2, y1, y2: Single;
  R: TRectF;
  Off: Single;
begin
  R := GetShapeRect;
  case CalloutPosition of
    TCalloutPosition.cpTop:
      R.Top := R.Top + FCalloutLength;
    TCalloutPosition.cpLeft:
      R.Left := R.Left + FCalloutLength;
    TCalloutPosition.cpBottom:
      R.Bottom := R.Bottom - FCalloutLength;
    TCalloutPosition.cpRight:
      R.Right := R.Right - FCalloutLength;
  end;
  if Sides <> AllSides then
  begin
    Off := R.Left;
    if not(TSide.sdTop in FSides) then
      R.Top := R.Top - Off;
    if not(TSide.sdLeft in FSides) then
      R.Left := R.Left - Off;
    if not(TSide.sdBottom in FSides) then
      R.Bottom := R.Bottom + Off;
    if not(TSide.sdRight in FSides) then
      R.Right := R.Right + Off;
  end;
  x1 := XRadius;
  if (RectWidth(R) - (x1 * 2) < 0) and (x1 > 0) then
    x1 := (XRadius * (RectWidth(R) / (x1 * 2)));
  x2 := x1 / 2;
  y1 := YRadius;
  if (RectHeight(R) - (y1 * 2) < 0) and (y1 > 0) then
    y1 := (YRadius * (RectHeight(R) / (y1 * 2)));
  y2 := y1 / 2;
  FPath.Clear;
  FPath.MoveTo(PointF(R.Left, R.Top + y1));
  if TCorner.crTopLeft in FCorners then
  begin
    case FCornerType of
      // ctRound - default
      TCornerType.ctBevel:
        FPath.LineTo(PointF(R.Left + x1, R.Top));
      TCornerType.ctInnerRound:
        FPath.CurveTo(PointF(R.Left + x2, R.Top + y1),
          PointF(R.Left + x1, R.Top + y2), PointF(R.Left + x1, R.Top));
      TCornerType.ctInnerLine:
        begin
          FPath.LineTo(PointF(R.Left + x2, R.Top + y1));
          FPath.LineTo(PointF(R.Left + x1, R.Top + y2));
          FPath.LineTo(PointF(R.Left + x1, R.Top));
        end;
    else
      FPath.CurveTo(PointF(R.Left, R.Top + (y2)), PointF(R.Left + x2, R.Top),
        PointF(R.Left + x1, R.Top))
    end;
  end
  else
  begin
    if TSide.sdLeft in FSides then
      FPath.LineTo(R.TopLeft)
    else
      FPath.MoveTo(R.TopLeft);
    if TSide.sdTop in FSides then
      FPath.LineTo(PointF(R.Left + x1, R.Top))
    else
      FPath.MoveTo(PointF(R.Left + x1, R.Top));
  end;
  if not(TSide.sdTop in FSides) then
    FPath.MoveTo(PointF(R.Right - x1, R.Top))
  else
  begin
    if (FCalloutPosition = TCalloutPosition.cpTop) then
    begin
      if CalloutOffset = 0 then
      begin
        FPath.LineTo(PointF((R.Right - R.Left) / 2 - (CalloutWidth / 2), R.Top));
        FPath.LineTo(PointF((R.Right - R.Left) / 2, R.Top - FCalloutLength));
        FPath.LineTo(PointF((R.Right - R.Left) / 2 + (CalloutWidth / 2), R.Top));
        FPath.LineTo(PointF(R.Right - x1, R.Top));
      end
      else if CalloutOffset > 0 then
      begin
        FPath.LineTo(PointF(R.Left + FCalloutOffset, R.Top));
        FPath.LineTo(PointF(R.Left + FCalloutOffset + (CalloutWidth / 2), R.Top - FCalloutLength));
        FPath.LineTo(PointF(R.Left + FCalloutOffset + CalloutWidth, R.Top));
        FPath.LineTo(PointF(R.Right - x1, R.Top));
      end else
      begin
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset) - CalloutWidth, R.Top));
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset) - (CalloutWidth / 2), R.Top - FCalloutLength));
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset), R.Top));
        FPath.LineTo(PointF(R.Right - x1, R.Top));
      end;
    end else
      FPath.LineTo(PointF(R.Right - x1, R.Top));
  end;
  if TCorner.crTopRight in FCorners then
  begin
    case FCornerType of
      // ctRound - default
      TCornerType.ctBevel:
        FPath.LineTo(PointF(R.Right, R.Top + y1));
      TCornerType.ctInnerRound:
        FPath.CurveTo(PointF(R.Right - x1, R.Top + y2),
          PointF(R.Right - x2, R.Top + y1), PointF(R.Right, R.Top + y1));
      TCornerType.ctInnerLine:
        begin
          FPath.LineTo(PointF(R.Right - x1, R.Top + y2));
          FPath.LineTo(PointF(R.Right - x2, R.Top + y1));
          FPath.LineTo(PointF(R.Right, R.Top + y1));
        end;
    else
      FPath.CurveTo(PointF(R.Right - x2, R.Top), PointF(R.Right, R.Top + (y2)),
        PointF(R.Right, R.Top + y1))
    end;
  end
  else
  begin
    if TSide.sdTop in FSides then
      FPath.LineTo(PointF(R.Right, R.Top))
    else
      FPath.MoveTo(PointF(R.Right, R.Top));
    if TSide.sdRight in FSides then
      FPath.LineTo(PointF(R.Right, R.Top + y1))
    else
      FPath.MoveTo(PointF(R.Right, R.Top + y1));
  end;
  if not(TSide.sdRight in FSides) then
    FPath.MoveTo(PointF(R.Right, R.Bottom - y1))
  else
  begin
    if (FCalloutPosition = TCalloutPosition.cpRight) then
    begin
      if CalloutOffset = 0 then
      begin
        FPath.LineTo(PointF(R.Right, (R.Bottom - R.Top) / 2 -
          (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Right + FCalloutLength, (R.Bottom - R.Top) / 2));
        FPath.LineTo(PointF(R.Right, (R.Bottom - R.Top) / 2 +
          (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Right, R.Bottom - y1));
      end
      else if CalloutOffset > 0 then
      begin
        FPath.LineTo(PointF(R.Right, R.Top + CalloutOffset));
        FPath.LineTo(PointF(R.Right + FCalloutLength, R.Top + CalloutOffset + (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Right, R.Top + CalloutOffset + CalloutWidth));
        FPath.LineTo(PointF(R.Right, R.Bottom - y1));
      end
      else
      begin
        FPath.LineTo(PointF(R.Right, R.Bottom + CalloutOffset));
        FPath.LineTo(PointF(R.Right + FCalloutLength, R.Bottom + CalloutOffset + (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Right, R.Bottom + CalloutOffset + CalloutWidth));
        FPath.LineTo(PointF(R.Right, R.Bottom - y1));
      end;
    end else
      FPath.LineTo(PointF(R.Right, R.Bottom - y1));
  end;
  if TCorner.crBottomRight in FCorners then
  begin
    case FCornerType of
      // ctRound - default
      TCornerType.ctBevel:
        FPath.LineTo(PointF(R.Right - x1, R.Bottom));
      TCornerType.ctInnerRound:
        FPath.CurveTo(PointF(R.Right - x2, R.Bottom - y1),
          PointF(R.Right - x1, R.Bottom - y2), PointF(R.Right - x1, R.Bottom));
      TCornerType.ctInnerLine:
        begin
          FPath.LineTo(PointF(R.Right - x2, R.Bottom - y1));
          FPath.LineTo(PointF(R.Right - x1, R.Bottom - y2));
          FPath.LineTo(PointF(R.Right - x1, R.Bottom));
        end;
    else
      FPath.CurveTo(PointF(R.Right, R.Bottom - (y2)),
        PointF(R.Right - x2, R.Bottom), PointF(R.Right - x1, R.Bottom))
    end;
  end
  else
  begin
    if TSide.sdRight in FSides then
      FPath.LineTo(PointF(R.Right, R.Bottom))
    else
      FPath.MoveTo(PointF(R.Right, R.Bottom));
    if TSide.sdBottom in FSides then
      FPath.LineTo(PointF(R.Right - x1, R.Bottom))
    else
      FPath.MoveTo(PointF(R.Right - x1, R.Bottom));
  end;
  if not(TSide.sdBottom in FSides) then
    FPath.MoveTo(PointF(R.Left + x1, R.Bottom))
  else
  begin
    if (FCalloutPosition = TCalloutPosition.cpBottom) then
    begin
      if CalloutOffset = 0 then
      begin
        FPath.LineTo(PointF((R.Right - R.Left) / 2 + (CalloutWidth / 2), R.Bottom));
        FPath.LineTo(PointF((R.Right - R.Left) / 2, R.Bottom + FCalloutLength));
        FPath.LineTo(PointF((R.Right - R.Left) / 2 - (CalloutWidth / 2), R.Bottom));
        FPath.LineTo(PointF(R.Left + x1, R.Bottom));
      end
      else if CalloutOffset > 0 then
      begin
        FPath.LineTo(PointF(R.Left + FCalloutOffset + CalloutWidth, R.Bottom));
        FPath.LineTo(PointF(R.Left + FCalloutOffset + (CalloutWidth / 2), R.Bottom + FCalloutLength));
        FPath.LineTo(PointF(R.Left + FCalloutOffset, R.Bottom));
        FPath.LineTo(PointF(R.Left + x1, R.Bottom));
      end else
      begin
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset), R.Bottom));
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset) - (CalloutWidth / 2), R.Bottom + FCalloutLength));
        FPath.LineTo(PointF(R.Right - Abs(FCalloutOffset) - CalloutWidth, R.Bottom));
        FPath.LineTo(PointF(R.Left + x1, R.Bottom));
      end;
    end else
      FPath.LineTo(PointF(R.Left + x1, R.Bottom));
  end;
  if TCorner.crBottomLeft in FCorners then
  begin
    case FCornerType of
      // ctRound - default
      TCornerType.ctBevel:
        FPath.LineTo(PointF(R.Left, R.Bottom - y1));
      TCornerType.ctInnerRound:
        FPath.CurveTo(PointF(R.Left + x1, R.Bottom - y2),
          PointF(R.Left + x2, R.Bottom - y1), PointF(R.Left, R.Bottom - y1));
      TCornerType.ctInnerLine:
        begin
          FPath.LineTo(PointF(R.Left + x1, R.Bottom - y2));
          FPath.LineTo(PointF(R.Left + x2, R.Bottom - y1));
          FPath.LineTo(PointF(R.Left, R.Bottom - y1));
        end;
    else
      FPath.CurveTo(PointF(R.Left + x2, R.Bottom), PointF(R.Left, R.Bottom - (y2)
        ), PointF(R.Left, R.Bottom - y1))
    end;
  end else
  begin
    if TSide.sdBottom in FSides then
      FPath.LineTo(PointF(R.Left, R.Bottom))
    else
      FPath.MoveTo(PointF(R.Left, R.Bottom));
    if TSide.sdLeft in FSides then
      FPath.LineTo(PointF(R.Left, R.Bottom - y1))
    else
      FPath.MoveTo(PointF(R.Left, R.Bottom - y1));
  end;
  if (TSide.sdLeft in FSides) then
  begin
    if (FCalloutPosition = TCalloutPosition.cpLeft) then
    begin
      if CalloutOffset = 0 then
      begin
        FPath.LineTo(PointF(R.Left, (R.Bottom - R.Top) / 2 + (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Left - FCalloutLength, (R.Bottom - R.Top) / 2));
        FPath.LineTo(PointF(R.Left, (R.Bottom - R.Top) / 2 - (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Left, R.Top + y1));
      end
      else if CalloutOffset > 0 then
      begin
        FPath.LineTo(PointF(R.Left, R.Top + CalloutOffset + CalloutWidth));
        FPath.LineTo(PointF(R.Left - FCalloutLength, R.Top + CalloutOffset + (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Left, R.Top + CalloutOffset));
        FPath.LineTo(PointF(R.Left, R.Top + y1));
      end else
      begin
        FPath.LineTo(PointF(R.Left, R.Bottom + CalloutOffset + CalloutWidth));
        FPath.LineTo(PointF(R.Left - FCalloutLength, R.Bottom + CalloutOffset + (CalloutWidth / 2)));
        FPath.LineTo(PointF(R.Left, R.Bottom + CalloutOffset));
        FPath.LineTo(PointF(R.Left, R.Top + y1));
      end;
    end else
      FPath.LineTo(PointF(R.Left, R.Top + y1));
  end;
end;

procedure TCalloutRectangle.Paint;
begin
  CreatePath;
  Canvas.FillPath(FPath, AbsoluteOpacity);
  Canvas.DrawPath(FPath, AbsoluteOpacity);
end;

procedure TCalloutRectangle.SetCalloutWidth(const Value: Single);
begin
  if FCalloutWidth <> Value then
  begin
    FCalloutWidth := Value;
    CreatePath;
    Repaint;
  end;
end;

procedure TCalloutRectangle.SetCalloutLength(const Value: Single);
begin
  if FCalloutLength <> Value then
  begin
    FCalloutLength := Value;
    CreatePath;
    Repaint;
  end;
end;

procedure TCalloutRectangle.SetCalloutPosition(const Value: TCalloutPosition);
begin
  if FCalloutPosition <> Value then
  begin
    FCalloutPosition := Value;
    CreatePath;
    Repaint;
  end;
end;

procedure TCalloutRectangle.SetCalloutOffset(const Value: Single);
begin
  if FCalloutOffset <> Value then
  begin
    FCalloutOffset := Value;
    CreatePath;
    Repaint;
  end;
end;

{ TText }

constructor TText.Create(AOwner: TComponent);
begin
  inherited;
  FHorzTextAlign := TTextAlign.taCenter;
  FVertTextAlign := TTextAlign.taCenter;
  FWordWrap := True;
  FFont := TFont.Create;
  FFont.OnChanged := FontChanged;
  Fill.DefaultColor := $FF000000;
  Fill.Color := $FF000000;
  Stroke.DefaultKind := TBrushKind.bkNone;
  Stroke.Kind := TBrushKind.bkNone;
end;

destructor TText.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TText.GetData: Variant;
begin
  Result := Text;
end;

procedure TText.SetData(const Value: Variant);
begin
  Text := Value;
end;

procedure TText.FontChanged(Sender: TObject);
begin
  if FAutoSize then
    AdjustSize;
  Repaint;
end;

procedure TText.Paint;
var
  R: TRectF;
  M: TMatrix;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;
  Canvas.Font.Assign(FFont);
  if FStretch then
  begin
    R := RectF(0, 0, 1000, 1000);
    Canvas.MeasureText(R, FText, False, [], FHorzTextAlign, FVertTextAlign);
    OffsetRect(R, -R.Left, -R.Top);
    M := IdentityMatrix;
    if not IsRectEmpty(R) then
    begin
      M.m11 := RectWidth(LocalRect) / RectWidth(R);
      M.m22 := RectHeight(LocalRect) / RectHeight(R);
    end;
    Canvas.MultyMatrix(M);
    InflateRect(R, Font.Size / 3, Font.Size / 3);
    Canvas.FillText(R, FText, False, AbsoluteOpacity,  FillTextFlags, TTextAlign.taCenter, TTextAlign.taCenter);
  end
  else
    Canvas.FillText(LocalRect, FText, FWordWrap, AbsoluteOpacity,
      FillTextFlags, FHorzTextAlign, FVertTextAlign);
end;

procedure TText.SetWordWrap(const Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    AdjustSize;
    Repaint;
  end;
end;

procedure TText.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

procedure TText.SetStretch(const Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    if Stretch then
      AutoSize := False;
    Repaint;
  end;
end;

procedure TText.AdjustSize;
var
  R: TRectF;
  C: TCanvas;
begin
  if FDisableAlign then
    Exit;
  FDisableAlign := True;
  if Canvas = nil then
  begin
    C := GetMeasureBitmap.Canvas;
  end
  else
    C := Canvas;
  if FAutoSize and (FText <> '') then
  begin
    C.Font.Assign(FFont);
    if WordWrap then
      R := RectF(0, 0, Width, MaxSingle)
    else
      R := RectF(0, 0, MaxSingle, MaxSingle);
    C.MeasureText(R, FText, WordWrap,  [], TTextAlign.taLeading, TTextAlign.taLeading);
    if not WordWrap then
      Width := R.Right;
    if VertTextAlign <> TTextAlign.taCenter then
      Height := R.Bottom;
  end;
  FDisableAlign := False;
  if FAutoSize and (FText <> '') then
  begin
    SetBounds(Position.X, Position.Y, R.Width + 4, R.Height + 4);
    if (Parent <> nil) and (Parent is TControl) then
      TControl(Parent).Realign;
  end;
end;

procedure TText.Realign;
begin
  inherited;
  if FAutoSize then
    AdjustSize;
end;

procedure TText.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if FAutoSize then
    AdjustSize;
end;

procedure TText.SetHorzTextAlign(const Value: TTextAlign);
begin
  if FHorzTextAlign <> Value then
  begin
    FHorzTextAlign := Value;
    Repaint;
  end;
end;

procedure TText.SetVertTextAlign(const Value: TTextAlign);
begin
  if FVertTextAlign <> Value then
  begin
    FVertTextAlign := Value;
    Repaint;
  end;
end;

procedure TText.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    if Length(Value) < Length(FText) then
      Repaint;
    FText := Value;
    if FAutoSize then
      AdjustSize;
    Repaint;
  end;
end;

{ TImage }

constructor TImage.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TBitmap.Create(0, 0);
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapMargins := TBounds.Create(RectF(0, 0, 0, 0));
  FWrapMode := TImageWrapMode.iwFit;
  SetAcceptsControls(False);
end;

destructor TImage.Destroy;
begin
  FreeAndNil(FBitmapMargins);
  FBitmap.Free;
  inherited;
end;

function TImage.GetData: Variant;
begin
  Result := ObjectToVariant(FBitmap);
end;

procedure TImage.SetData(const Value: Variant);
begin
  if VarIsObject(Value) then
  begin
    if VariantToObject(Value) is TPersistent then
      FBitmap.Assign(TPersistent(VariantToObject(Value)));
  end
  else
    FBitmap.LoadFromFile(Value)
end;

procedure TImage.DoBitmapChanged(Sender: TObject);
begin
  Repaint;
  UpdateEffects;
end;

procedure TImage.Paint;
var
  R: TRectF;
  State: TCanvasSaveState;
  i, j: Integer;
  B: TBitmap;
begin
  if (csDesigning in ComponentState) and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;

  B := FBitmap;
  if FBitmap.ResourceBitmap <> nil then
    B := FBitmap.ResourceBitmap;
  if B.IsEmpty then
    Exit;

  if not FBitmapMargins.MarginEmpty then
  begin
    { lefttop }
    R := RectF(0, 0, FBitmapMargins.Left, FBitmapMargins.Top);
    Canvas.DrawBitmap(B, RectF(0, 0, FBitmapMargins.Left, FBitmapMargins.Top), R,
      AbsoluteOpacity, True);
    { top }
    R := RectF(FBitmapMargins.Left, 0, Width - FBitmapMargins.Right,
      FBitmapMargins.Top);
    Canvas.DrawBitmap(B, RectF(FBitmapMargins.Left, 0,
      B.Width - FBitmapMargins.Right, FBitmapMargins.Top), R,
      AbsoluteOpacity, True);
    { righttop }
    R := RectF(Width - FBitmapMargins.Right, 0, Width, FBitmapMargins.Top);
    Canvas.DrawBitmap(B, RectF(B.Width - FBitmapMargins.Right, 0, B.Width,
      FBitmapMargins.Top), R, AbsoluteOpacity, True);
    { left }
    R := RectF(0, FBitmapMargins.Top, FBitmapMargins.Left,
      Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(B, RectF(0, FBitmapMargins.Top, FBitmapMargins.Left,
      B.Height - FBitmapMargins.Bottom), R, AbsoluteOpacity, True);
    { center }
    R := RectF(FBitmapMargins.Left, FBitmapMargins.Top,
      Width - FBitmapMargins.Right, Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(B, RectF(FBitmapMargins.Left, FBitmapMargins.Top,
      B.Width - FBitmapMargins.Right, B.Height - FBitmapMargins.Bottom), R,
      AbsoluteOpacity, True);
    { right }
    R := RectF(Width - FBitmapMargins.Right, FBitmapMargins.Top, Width,
      Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(B, RectF(B.Width - FBitmapMargins.Right,
      FBitmapMargins.Top, B.Width, B.Height - FBitmapMargins.Bottom), R,
      AbsoluteOpacity, True);
    { leftbottom }
    R := RectF(0, Height - FBitmapMargins.Bottom, FBitmapMargins.Left, Height);
    Canvas.DrawBitmap(B, RectF(0, B.Height - FBitmapMargins.Bottom,
      FBitmapMargins.Left, B.Height), R, AbsoluteOpacity, True);
    { bottom }
    R := RectF(FBitmapMargins.Left, Height - FBitmapMargins.Bottom,
      Width - FBitmapMargins.Right, Height);
    Canvas.DrawBitmap(B, RectF(FBitmapMargins.Left,
      B.Height - FBitmapMargins.Bottom, B.Width - FBitmapMargins.Right,
      B.Height), R, AbsoluteOpacity, True);
    { rightbottom }
    R := RectF(Width - FBitmapMargins.Right, Height - FBitmapMargins.Bottom,
      Width, Height);
    Canvas.DrawBitmap(B, RectF(B.Width - FBitmapMargins.Right,
      B.Height - FBitmapMargins.Bottom, B.Width, B.Height), R,
      AbsoluteOpacity, True);
  end
  else
  begin
    case FWrapMode of
      TImageWrapMode.iwOriginal:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);
            R := RectF(0, 0, B.Width, B.Height);
            Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
              AbsoluteOpacity, True);
          finally
            Canvas.RestoreState(State);
          end;
        end;
      TImageWrapMode.iwFit:
        begin
          R := RectF(0, 0, B.Width, B.Height);
          FitRect(R, LocalRect);
          Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
            AbsoluteOpacity, DisableInterpolation);
        end;
      TImageWrapMode.iwStretch:
        begin
          R := LocalRect;
          Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
            AbsoluteOpacity, DisableInterpolation)
        end;
      TImageWrapMode.iwTile:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);

            for i := 0 to Round(Width / B.Width) do
              for j := 0 to Round(Height / B.Height) do
              begin
                R := RectF(0, 0, B.Width, B.Height);
                OffsetRect(R, i * B.Width, j * B.Height);
                Canvas.DrawBitmap(B, RectF(0, 0, B.Width, B.Height), R,
                  AbsoluteOpacity, True);
              end;
          finally
            Canvas.RestoreState(State);
          end;
        end;
    end;
  end;
end;

procedure TImage.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TImage.SetWrapMode(const Value: TImageWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    Repaint;
  end;
end;

{ TPath }

constructor TCustomPath.Create(AOwner: TComponent);
begin
  inherited;
  FWrapMode := TPathWrapMode.pwStretch;
  FData := TPathData.Create;
  FData.OnChanged := DoChanged;
  FCurrent := TPathData.Create;
end;

destructor TCustomPath.Destroy;
begin
  FreeAndNil(FCurrent);
  FreeAndNil(FData);
  inherited;
end;

procedure TCustomPath.UpdatePath;
var
  B: TRectF;
  P: TPathData;
begin
  { Update path }
  P := FData;
  if FData.ResourcePath <> nil then
    P := FData.ResourcePath;

  if not P.IsEmpty then
  begin
    case FWrapMode of
      TPathWrapMode.pwOriginal:
        begin
          FCurrent.Assign(P);
        end;
      TPathWrapMode.pwFit:
        begin
          B := P.GetBounds;
          FitRect(B, ShapeRect);
          FCurrent.Assign(P);
          FCurrent.FitToRect(B);
        end;
      TPathWrapMode.pwStretch:
        begin
          B := P.GetBounds;
          FCurrent.Assign(P);
          FCurrent.Translate(-B.Left, -B.Top);
          FCurrent.Scale(RectWidth(ShapeRect) / RectWidth(B), RectHeight(ShapeRect) / RectHeight(B));
        end;
      TPathWrapMode.pwTile:
        begin
          B := P.GetBounds;
          FCurrent.Assign(P);
          FCurrent.Translate(-B.Left, -B.Top);
        end;
    end;
    if Stroke.Kind <> TBrushKind.bkNone then
      FCurrent.Translate(StrokeThickness / 2, StrokeThickness / 2);
  end
  else
    FCurrent.Clear;
end;

procedure TCustomPath.DoChanged(Sender: TObject);
begin
  UpdatePath;
  Repaint;
end;

procedure TCustomPath.Loaded;
begin
  inherited;
  UpdatePath;
end;

function TCustomPath.PointInObject(X, Y: Single): Boolean;
begin
  if (csDesigning in ComponentState) and not FLocked and not FInPaintTo then
  begin
    Result := inherited PointInObject(X, Y);
  end
  else if (Canvas <> nil) and not (FCurrent.IsEmpty) and (FWrapMode <> TPathWrapMode.pwTile) then
  begin
    Result := Canvas.PtInPath(AbsoluteToLocal(PointF(X, Y)), FCurrent)
  end
  else
    Result := inherited PointInObject(X, Y);
end;

procedure TCustomPath.Resize;
begin
  inherited;
  UpdatePath;
end;

procedure TCustomPath.Paint;
var
  B, R: TRectF;
  i, j: Integer;
  State: TCanvasSaveState;
  P1: TPathData;
begin
  if not FCurrent.IsEmpty then
  begin
    case FWrapMode of
      TPathWrapMode.pwOriginal:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);

            Canvas.FillPath(FCurrent, AbsoluteOpacity);
            Canvas.DrawPath(FCurrent, AbsoluteOpacity);

          finally
            Canvas.RestoreState(State);
          end;
        end;
      TPathWrapMode.pwFit:
        begin
          Canvas.FillPath(FCurrent, AbsoluteOpacity);
          Canvas.DrawPath(FCurrent, AbsoluteOpacity);
        end;
      TPathWrapMode.pwStretch:
        begin
          Canvas.FillPath(FCurrent, AbsoluteOpacity);
          Canvas.DrawPath(FCurrent, AbsoluteOpacity);
        end;
      TPathWrapMode.pwTile:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);

            B := FCurrent.GetBounds;
            R := B;
            P1 := TPathData.Create;
            for i := 0 to Round(Width / RectWidth(R)) do
              for j := 0 to Round(Height / RectHeight(R)) do
              begin
                P1.Assign(FCurrent);
                P1.Translate(ShapeRect.Left + i * (RectWidth(R) + ShapeRect.Left * 2),
                  ShapeRect.Top + j * (RectHeight(R) + ShapeRect.Top * 2));
                Canvas.FillPath(P1, AbsoluteOpacity);
                Canvas.DrawPath(P1, AbsoluteOpacity);
              end;
            P1.Free;
          finally
            Canvas.RestoreState(State);
          end;
        end;
    end;
  end;
  if (csDesigning in ComponentState) and not FLocked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;
end;

procedure TCustomPath.SetWrapMode(const Value: TPathWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    UpdatePath;
    Repaint;
  end;
end;

procedure TCustomPath.SetData(const Value: TPathData);
begin
  FData.Assign(Value);
end;

{ TPaintBox }

constructor TPaintBox.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TPaintBox.Destroy;
begin
  inherited;
end;

procedure TPaintBox.Paint;
var
  R: TRectF;
begin
  if (csDesigning in ComponentState) and not FLocked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := TStrokeDash.sdDash;
    Canvas.Stroke.Kind := TBrushKind.bkSolid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := TStrokeDash.sdSolid;
  end;
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

{ TSelection }

constructor TSelection.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := True;
  ParentBounds := True;
  FMinSize := 15;
  FGripSize := 3;
  SetAcceptsControls(False);
end;

destructor TSelection.Destroy;
begin
  inherited;
end;

function TSelection.GetAbsoluteRect: TRectF;
begin
  Result := inherited GetAbsoluteRect;
  InflateRect(Result, FGripSize + 4, FGripSize + 4);
end;

procedure TSelection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  R: TRectF;
begin
  // this line may be necessary because TSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  FDownPos := PointF(X, Y);
  if Button = TMouseButton.mbLeft then
  begin
    FRatio := Width / Height;
    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FDownPos, R) then
    begin
      FLeftTop := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FDownPos, R) then
    begin
      FRightTop := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FDownPos, R) then
    begin
      FRightBottom := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FDownPos, R) then
    begin
      FLeftBottom := True;
      Exit;
    end;
    FMove := True;
  end;
end;

procedure TSelection.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P, OldPos, Size: TPointF;
  S, SaveW: Single;
  R: TRectF;
begin
  // this line may be necessary because TSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  if Shift = [] then
  begin
    // handle painting for hotspot mouse hovering
    FMovePos := PointF(X, Y);
    P := LocalToAbsolute(FMovePos);
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);

    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FMovePos, R) xor FLeftTopHot then
    begin
      FLeftTopHot := not FLeftTopHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize),
      R.Top + (GripSize));
    if PointInRect(FMovePos, R) xor FRightTopHot then
    begin
      FRightTopHot := not FRightTopHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FMovePos, R) xor FRightBottomHot then
    begin
      FRightBottomHot := not FRightBottomHot;
      Repaint;
    end;

    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(FMovePos, R) xor FLeftBottomHot then
    begin
      FLeftBottomHot := not FLeftBottomHot;
      Repaint;
    end;
  end;

  if ssLeft in Shift then
  begin
    // handle the movement of the whole selection control by grabbing
    FMovePos := PointF(X, Y);
    if FMove then
    begin
      Position.X := Position.X + (FMovePos.X - FDownPos.X);
      Position.Y := Position.Y + (FMovePos.Y - FDownPos.Y);
      if ParentBounds then
      begin
        if Position.X < 0 then
          Position.X := 0;
        if Position.Y < 0 then
          Position.Y := 0;
        if (Parent <> nil) and (Parent is TControl) then
        begin
          if Position.X + Width > TControl(Parent).Width then
            Position.X := TControl(Parent).Width - Width;
          if Position.Y + Height > TControl(Parent).Height then
            Position.Y := TControl(Parent).Height - Height;
        end
        else
        if Canvas <> nil then
        begin
          if Position.X + Width > Canvas.Width then
            Position.X := Canvas.Width - Width;
          if Position.Y + Height > Canvas.Height then
            Position.Y := Canvas.Height - Height;
        end;
      end;
      if Assigned(FOnTrack) then
        FOnTrack(Self);
      Exit;
    end;

    OldPos := Position.Point;
    P := LocalToAbsolute(FMovePos);
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);
    if ParentBounds then
    begin
      if P.Y < 0 then
        P.Y := 0;
      if P.X < 0 then
        P.X := 0;
      if (Parent <> nil) and (Parent is TControl) then
      begin
        if P.X > TControl(Parent).Width then
          P.X := TControl(Parent).Width;
        if P.Y > TControl(Parent).Height then
          P.Y := TControl(Parent).Height;
      end
      else
      if Canvas <> nil then
      begin
        if P.X > Canvas.Width then
          P.X := Canvas.Width;
        if P.Y > Canvas.Height then
          P.Y := Canvas.Height;
      end;
    end;

   if FLeftTop then
    begin
      Repaint;
      Size := PointF((Position.X - (P.X + FDownPos.X)), (Position.Y - (P.Y + FDownPos.Y)));
      if (Parent <> nil) and (Parent is TControl) then
        Size := PointF(TControl(Parent).LocalToAbsoluteVector(Vector(Size)));
      Size := PointF(AbsoluteToLocalVector(Vector(Size)));

      if FProportional then
      begin
        Width := Width + Size.X;
        SaveW := Width;
        if Width < FMinSize then
          Width := FMinSize;
        if Width / FRatio < FMinSize then
          Width := Round(FMinSize * FRatio);
        Position.X := P.X + FDownPos.X - (Width - SaveW);
        Position.Y := Position.Y + (Height - Round(Width / FRatio));
        Height := Round(Width / FRatio);
      end
      else
      begin
        Width := Width + Size.X;
        Height := Height + Size.Y;
        Position.X := P.X + FDownPos.X;
        Position.Y := P.Y + FDownPos.Y;
        if Width < FMinSize then
          Width := FMinSize;
        if Height < FMinSize then
          Height := FMinSize;
      end;
      if Assigned(FOnTrack) then
        FOnTrack(Self);
      Repaint;
    end;

    if FRightTop then
    begin
      Repaint;
      Size := PointF((P.X { + FDownPos.X } ) - Position.X, (Position.Y - (P.Y + FDownPos.Y)));
      if (Parent <> nil) and (Parent is TControl) then
        Size := PointF(TControl(Parent).LocalToAbsoluteVector(Vector(Size)));
      Size := PointF(AbsoluteToLocalVector(Vector(Size)));

      Width := Size.X;
      if FProportional then
      begin
        if Width < FMinSize then
          Width := FMinSize;
        if Width / FRatio < FMinSize then
          Width := Round(FMinSize * FRatio);
        Position.Y := Position.Y + (Height - Round(Width / FRatio));
        Height := Round(Width / FRatio);
      end
      else
      begin
        Height := Height + Size.Y;
        Position.Y := P.Y + FDownPos.Y;
        if Width < FMinSize then
          Width := FMinSize;
        if Height < FMinSize then
          Height := FMinSize;
      end;
      if Assigned(FOnTrack) then
        FOnTrack(Self);
      Repaint;
    end;
    if FRightBottom then
    begin
      Repaint;
      Size := PointF((P.X { + FDownPos.X } ) - Position.X, (P.Y { + FDownPos.Y) } ) - Position.Y);
      if (Parent <> nil) and (Parent is TControl) then
        Size := PointF(TControl(Parent).LocalToAbsoluteVector(Vector(Size)));
      Size := PointF(AbsoluteToLocalVector(Vector(Size)));

      Width := Size.X;
      if FProportional then
      begin
        if Width < FMinSize then
          Width := FMinSize;
        if Width / FRatio < FMinSize then
          Width := Round(FMinSize * FRatio);
        Height := Round(Width / FRatio);
      end
      else
      begin
        Height := Size.Y;
        if Width < FMinSize then
          Width := FMinSize;
        if Height < FMinSize then
          Height := FMinSize;
      end;

      if Assigned(FOnTrack) then
        FOnTrack(Self);
      Repaint;
    end;
    if FLeftBottom then
    begin
      Repaint;
      Size := PointF((Position.X - (P.X + FDownPos.X)), (P.Y { + FDownPos.Y) } ) - Position.Y);
      if (Parent <> nil) and (Parent is TControl) then
        Size := PointF(TControl(Parent).LocalToAbsoluteVector(Vector(Size)));
      Size := PointF(AbsoluteToLocalVector(Vector(Size)));

      if FProportional then
      begin
        Width := Width + Size.X;
        SaveW := Width;
        if Width < FMinSize then
          Width := FMinSize;
        if Width / FRatio < FMinSize then
          Width := Round(FMinSize * FRatio);
        Position.X := P.X + FDownPos.X - (Width - SaveW);
        Height := Round(Width / FRatio);
      end
      else
      begin
        Width := Width + Size.X;
        Position.X := P.X + FDownPos.X;
        Height := Size.Y;
        if Width < FMinSize then
          Width := FMinSize;
        if Height < FMinSize then
          Height := FMinSize;
      end;
      if Assigned(FOnTrack) then
        FOnTrack(Self);
      Repaint;
    end;
  end;
end;

function TSelection.PointInObject(X, Y: Single): Boolean;
var
  R: TRectF;
  P: TPointF;
begin
  Result := inherited PointInObject(X, Y);
  if not Result then
  begin
    P := AbsoluteToLocal(PointF(X, Y));
    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize),
      R.Top + (GripSize));
    if PointInRect(P, R) then
    begin
      Result := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize),
      R.Top + (GripSize));
    if PointInRect(P, R) then
    begin
      Result := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(P, R) then
    begin
      Result := True;
      Exit;
    end;
    R := LocalRect;
    R := RectF(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize),
      R.Bottom + (GripSize));
    if PointInRect(P, R) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TSelection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  // this line may be necessary because TSelection is not a styled control;
  // must further investigate for a better fix
  if not Enabled then
    Exit;

  inherited;

  if FMove or FLeftTop or FLeftBottom or FRightTop or FRightBottom then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);

    FMove := False;
    FLeftTop := False;
    FLeftBottom := False;
    FRightTop := False;
    FRightBottom := False;
  end;
end;

procedure TSelection.Paint;
var
  DefColor: TAlphaColor;
  R: TRectF;

  // sets the canvas color depending if the control is enabled and if
  // we need to draw a zone being hot or not
  procedure SelectZoneColor(HotZone: Boolean);
  begin
    if Enabled then
      if HotZone then
        Canvas.Fill.Color := claRed
      else
        Canvas.Fill.Color := $FFFFFFFF
    else
      Canvas.Fill.Color := claGrey;
  end;

begin
  if FHideSelection then
    Exit;
  R := LocalRect;
  InflateRect(R, -0.5, -0.5);
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  Canvas.Fill.Color := $FFFFFFFF;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Color := $FF1072C5;
  Canvas.StrokeDash := TStrokeDash.sdDash;
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.StrokeDash := TStrokeDash.sdSolid;
  { angles }

  R := LocalRect;
  InflateRect(R, -0.5, -0.5);
  SelectZoneColor(FLeftTopHot);
  Canvas.FillEllipse(RectF(R.Left - (GripSize), R.Top - (GripSize),
    R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Left - (GripSize), R.Top - (GripSize),
    R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  SelectZoneColor(FRightTopHot);
  Canvas.FillEllipse(RectF(R.Right - (GripSize), R.Top - (GripSize),
    R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Right - (GripSize), R.Top - (GripSize),
    R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  SelectZoneColor(FLeftBottomHot);
  Canvas.FillEllipse(RectF(R.Left - (GripSize), R.Bottom - (GripSize),
    R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Left - (GripSize), R.Bottom - (GripSize),
    R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);

  R := LocalRect;
  SelectZoneColor(FRightBottomHot);
  Canvas.FillEllipse(RectF(R.Right - (GripSize), R.Bottom - (GripSize),
    R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(R.Right - (GripSize), R.Bottom - (GripSize),
    R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
end;

procedure TSelection.DoMouseLeave;
begin
  inherited;

  FLeftTopHot := False;
  FLeftBottomHot := False;
  FRightTopHot := False;
  FRightBottomHot := False;

  Repaint;
end;

procedure TSelection.SetHideSelection(const Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    Repaint;
  end;
end;

procedure TSelection.SetMinSize(const Value: Integer);
begin
  if FMinSize <> Value then
  begin
    FMinSize := Value;
    if FMinSize < 1 then
      FMinSize := 1;
  end;
end;

procedure TSelection.SetGripSize(const Value: Single);
begin
  if FGripSize <> Value then
  begin
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;
    if FGripSize < 1 then
      FGripSize := 1;
    Repaint;
  end;
end;

{ TSelectionPoint }

constructor TSelectionPoint.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := True;
  ParentBounds := True;
  FGripSize := 3;
  Width := FGripSize * 2;
  Height := FGripSize * 2;
  SetAcceptsControls(False);
end;

destructor TSelectionPoint.Destroy;
begin
  inherited;
end;

function TSelectionPoint.PointInObject(X, Y: Single): Boolean;
var
  P: TPointF;
begin
  Result := False;
  P := AbsoluteToLocal(PointF(X, Y));
  if (Abs(P.X) < GripSize) and (Abs(P.Y) < GripSize) then
  begin
    Result := True;
  end;
end;

procedure TSelectionPoint.Paint;
begin
  inherited;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.Stroke.Color := $FF1072C5;
  Canvas.Fill.Kind := TBrushKind.bkSolid;
  if IsMouseOver then
    Canvas.Fill.Color := $FFFF0000
  else
    Canvas.Fill.Color := $FFFFFFFF;

  Canvas.FillEllipse(RectF(-(GripSize), -(GripSize), (GripSize), (GripSize)),
    AbsoluteOpacity);
  Canvas.DrawEllipse(RectF(-(GripSize), -(GripSize), (GripSize), (GripSize)),
    AbsoluteOpacity);
end;

procedure TSelectionPoint.SetHeight(const Value: Single);
begin
  inherited SetHeight(FGripSize * 2);
end;

procedure TSelectionPoint.SetWidth(const Value: Single);
begin
  inherited SetWidth(FGripSize * 2);
end;

procedure TSelectionPoint.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if Button = TMouseButton.mbLeft then
    FPressed := True;
end;

procedure TSelectionPoint.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPointF;
begin
  inherited;
  if FPressed and (Parent <> nil) and (Parent is TControl) then
  begin
    P := LocalToAbsolute(PointF(X, Y));
    if (Parent <> nil) and (Parent is TControl) then
      P := TControl(Parent).AbsoluteToLocal(P);
    if ParentBounds then
    begin
      if P.X < 0 then
        P.X := 0;
      if P.Y < 0 then
        P.Y := 0;
      if (Parent <> nil) and (Parent is TControl) then
      begin
        if P.X > TControl(Parent).Width then
          P.X := TControl(Parent).Width;
        if P.Y > TControl(Parent).Height then
          P.Y := TControl(Parent).Height;
      end
      else
      if (Canvas <> nil) then
      begin
        if P.X > Canvas.Width then
          P.X := Canvas.Width;
        if P.Y > Canvas.Height then
          P.Y := Canvas.Height;
      end;
    end;
    Position.X := P.X;
    Position.Y := P.Y;
    if Assigned(FOnTrack) then
      FOnTrack(Self);
  end;
end;

procedure TSelectionPoint.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if FPressed then
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
  FPressed := False;
end;

procedure TSelectionPoint.DoMouseEnter;
begin
  inherited;
  Repaint;
end;

procedure TSelectionPoint.DoMouseLeave;
begin
  inherited;
  Repaint;
end;

function TSelectionPoint.GetUpdateRect: TRectF;
begin
  Result := inherited GetUpdateRect;
  InflateRect(Result, GripSize + 1, GripSize + 1);
end;

procedure TSelectionPoint.SetGripSize(const Value: Single);
begin
  if FGripSize <> Value then
  begin
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;
    if FGripSize < 1 then
      FGripSize := 1;
    Repaint;
  end;
end;

initialization
  RegisterFmxClasses([TLine, TRectangle, TRoundRect, TEllipse, TCircle,
    TArc, TPie, TText, TPath, TImage, TPaintBox,TCalloutRectangle,
    TSelection, TSelectionPoint]);
end.
