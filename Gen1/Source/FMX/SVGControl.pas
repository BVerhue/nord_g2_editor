unit SVGControl;

// http://www.angusj.com/delphitips/beziertext.php

interface

uses
  System.SysUtils, System.Classes, System.Contnrs, System.Types,
  System.Character, System.Math, FMX.Types,
  FMX.Objects, FMX.Controls, FMX.Layouts, DOM;

type
  TSVGShapeList = class(TObjectList)
  private
    function    GetShape( aIndex : integer) : TShape;
    procedure   SetShape( aIndex : integer; const aValue : TShape);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    function    AddShape( aValue : TShape): integer;
    property    Items[ aIndex : integer]: TShape read GetShape write SetShape; default;
  end;

  TPatchPointArray = array of TPathPoint;

  TSegmentLength = record
    ArcLength : single;
    PointSegmentIndex : integer; // Start index of segment in FTempPathData
    StartPointIndex : integer;
    ArcSegmentIndex : integer;   // Start index of segment in FArcLengths
    ArcSegmentCount : integer;   // Number of arclength entries for this segment
  end;

  TSGVPathData = class(TPathData)
  private
    FTempPathData: array of TPathPoint;
    FArcLengths : array of TSegmentLength;
    FArcLengthCount : integer;
    FPathLength : single;
    FRecalcBounds: Boolean;
    FStartPoint: TPointF;
    FOnChanged: TNotifyEvent;
    {FOnChanged: TNotifyEvent;
    FStyleResource: TObject;
    FStyleLookup: string;
    FStartPoint: TPointF;
    FPathData: array of TPathPoint;
    FRecalcBounds: Boolean;
    FBounds: TRectF;
    function GetPathString: string;}
    //procedure SetPathString(const Value: string);
    procedure AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    procedure AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean; const P2: TPointF);
    {function GetStyleLookup: string;
    procedure SetStyleLookup(const Value: string);
    function GetPath: TSGVPathData;}
    function GetTempCount: Integer; inline;
    function GetPoint(AIndex: Integer): TPathPoint; inline;
    function GetPointFromStr(const S: string; var Pos: Integer): TPointF;
    function GetNum(const S: string; var Pos: Integer): string;
    function GetTok(const S: string; var Pos: Integer): string;
    procedure ApplyPathData;
  public
    constructor Create; override;
    destructor Destroy; override;
    //procedure Assign(Source: TPersistent); override;
    //property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure ParseSGVPathString( const Value: string);
    procedure ParseSGVPolygonString( const Value: string);
    { creation }
    function LastPoint: TPointF;
    procedure MoveTo(const P: TPointF);
    procedure MoveToRel(const P: TPointF);
    procedure LineTo(const P: TPointF);
    procedure LineToRel(const P: TPointF);
    procedure HLineTo(const X: Single);
    procedure HLineToRel(const X: Single);
    procedure VLineTo(const Y: Single);
    procedure VLineToRel(const Y: Single);
    procedure CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
    procedure SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
    procedure QuadCurveTo(const ControlPoint, EndPoint: TPointF);
    procedure ClosePath;
    { shapes }
    procedure AddEllipse(const ARect: TRectF);
    procedure AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
      const ACornerType: TCornerType = TCornerType.ctRound);
    procedure AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
    { modification }
    procedure Clear;
    procedure Flatten(const Flatness: Single = 0.25);
    procedure Scale(const scaleX, scaleY: Single);
    procedure Translate(const dX, dY: Single);
    procedure FitToRect(const ARect: TRectF);
    procedure ApplyMatrix(const M: TMatrix);
    { params }
    //function GetBounds: TRectF;
    { convert }
    function FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = 0.25): TPointF;
    //function IsEmpty: Boolean;
    { access }
    procedure CalcArcLengths( steps : integer);
    procedure GetPointOnPath( u : single; var P : TPointf; var A: single);
    property TempCount: Integer read GetTempCount;
    property PathLength : single read FPathLength;
    //property Points[AIndex: Integer]: TPathPoint read GetPoint; default;
    { resoruces }
   // property ResourcePath: TSGVPathData read GetPath;
  published
    //property Data: string read GetPathString write SetPathString stored False;
    { This property allow to link path with PathObject by name. }
   // property StyleLookup: string read GetStyleLookup write SetStyleLookup;
  end;

  TSGVPath = class(TShape)
  private
    FData, FCurrent: TSGVPathData;
    FWrapMode: TPathWrapMode;
    procedure SetWrapMode(const Value: TPathWrapMode);
    procedure SetPathData(const Value: TSGVPathData);
    { IPathObject }
    function GetPath: TSGVPathData;
  protected
    procedure DoChanged(Sender: TObject);
    procedure Paint; override;
    procedure Resize; override;
    procedure Loaded; override;
    procedure UpdatePath;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PointInObject(X, Y: Single): Boolean; override;
    property Data: TSGVPathData read FData write SetPathData;
    property WrapMode: TPathWrapMode read FWrapMode write SetWrapMode default TPathWrapMode.pwStretch;
  end;

  TSVGControl = class(TControl)
  private
    { Private declarations }
    FShapeList : TSVGShapeList;
    FElementList : TStringList;

    function    ParseValue( aData : string; var i : integer): string;
    function    ParseHexValue( aData : string; var i : integer): integer;
    function    ParseFloatValue( aData : string; var i : integer): single;
    procedure   ParseStyle( aShape : TShape; aData : string);
    procedure   ParseTranslateMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseRotateMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseScaleMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseSkewXMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseSkewYMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
    procedure   ParseTransform( var aMatrix : TMatrix; aData : string);
    procedure   ParseNode( aNode : TDOMNode; aParent : TControl);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    //procedure   Paint; override;
    procedure   ParseXML( aSVGDoc : TXMLDocument);

  published
    { Published declarations }
  end;

procedure Register;

implementation

//==============================================================================
//
//                            Helper functions
//              http://www.angusj.com/delphitips/beziertext.php
//          http://steve.hollasch.net/cgindex/curves/cbezarclen.html
//
//==============================================================================

function DistanceBetween2Pts(pt1,pt2: TPointF): single;
begin
  result := sqrt((pt1.X - pt2.X)*(pt1.X - pt2.X) +
    (pt1.Y - pt2.Y)*(pt1.Y - pt2.Y));
end;

function GetPtAtDistAndAngleFromPt(pt: TPoint;
  dist: integer; angle: single): TPoint;
begin
  result.X := round(dist * cos(angle));
  result.Y := -round(dist * sin(angle)); //nb: Y axis is +ve down
  inc(result.X , pt.X);
  inc(result.Y , pt.Y);
end;

function PtBetween2Pts(pt1, pt2: TPoint;
  relativeDistFromPt1: single): TPoint;
begin
  //nb: 0 <= relativeDistFromPt1 <= 1
  if pt2.X = pt1.X then
    result.X := pt2.X else
    result.X := pt1.X + round((pt2.X - pt1.X)*relativeDistFromPt1);
  if pt2.Y = pt1.Y then
    result.Y := pt2.Y else
    result.Y := pt1.Y + round((pt2.Y - pt1.Y)*relativeDistFromPt1);
end;

function GetAnglePt2FromPt1(pt1, pt2: TPoint): single;
begin
  //nb: result is in radians
  dec(pt2.X,pt1.X);
  dec(pt2.Y,pt1.Y);
  with pt2 do
    if X = 0 then
    begin
      result := pi/2;
      if Y > 0 then result := 3*result; //nb: Y axis is +ve down
    end else
    begin
      result := arctan2(-Y,X);
      if result < 0 then result := result + pi * 2;
    end;
end;

function ArcTan2( V : TPointF): single;
begin
  Result := 0.0;
  if ABS(V.X) < 0.000000001 then //Close to zero! We declare it to be zero!
  begin //The small margin gives a slight error, but improves reliability.
    if V.y > 0 then
    begin
      Result := PI / 2; //90 degrees
    end
    else
    begin
      Result := PI / (-2); //-90 degrees
    end;
  end
  else
  begin
    if V.X > 0 then // 1. or 4. quadrant
    begin
      Result := ArcTan(V.Y / V.X); // Easy stuff. Normal ArcTan is valid for 1. and 4.
    end
    else // 2. or 3. quadrant
    begin
      Result := PI - ArcTan(-V.Y / V.X);
    end;
  end;
end;

procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
begin
  cx := 3.0 * (Bezier[1].X - Bezier[0].X);
  cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
  bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
  by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
  ax := Bezier[3].X - Bezier[0].X - cx - bx;
  ay := Bezier[3].Y - Bezier[0].Y - cy - by;
end;

function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
var
  tSqr: Single;
  tCube: Single;
begin
  tSqr := t * t;
  tCube := tSqr * t;
  Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
  Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
end;

function TangentOnBezier(const ax, bx, cx, ay, by, cy, t: Single): TPointF;
var
  tSqr: Single;
  M : single;
begin
  tSqr := t * t;
  Result.X := (3 * ax * tSqr) + (2 * bx * t) + cx;
  Result.Y := (3 * ay * tSqr) + (2 * by * t) + cy;

  if (Result.X <> 0) or (Result.Y <> 0) then begin
    M := sqrt(Result.X * Result.X + Result.Y * Result.Y);
    Result.X := Result.X / M;
    Result.Y := Result.Y / M;
  end;
end;

function NormalOnBezier(const ax, bx, cx, ay, by, cy, t: Single): TPointF;
var V : TPointF;
begin
  V := TangentOnBezier( ax, bx, cx, ay, by, cy, t);
  Result.X := V.Y;
  Result.Y := -V.X;
end;

function AngleOnBezier(const ax, bx, cx, ay, by, cy, t: Single): single;
var V : TPointF;
begin
  V := NormalOnBezier( ax, bx, cx, ay, by, cy, t);
  Result := ArcTan2(V);
end;

procedure BezierSplit( var Bezier, PLeft, PRight : TCubicBezier);
var i, j : integer;
    VTemp : array[0..3] of TCubicBezier;
begin
  // Copy control points
  for j := 0 to 3 do
    VTemp[0][j] := Bezier[j];

  // Triangle computation
  for i := 1 to 3 do begin
    for j := 0 to 3-i do begin
      VTemp[i][j].X := 0.5 * VTemp[i-1][j].X + 0.5 * VTemp[i-1][j+1].X;
      VTemp[i][j].Y := 0.5 * VTemp[i-1][j].Y + 0.5 * VTemp[i-1][j+1].Y;
    end;
  end;

  for j := 0 to 3 do
    PLeft[j] := VTemp[j][0];

  for j := 0 to 3 do
    PRight[j] := VTemp[3-j][j];
end;

procedure AddIfClose( var Bezier : TCubicBezier; var Length : double; Error : double);
var PLeft, PRight : TCubicBezier;
    len : double;
    chord : double;
    i : integer;
begin
  len := 0;
  for i := 0 to 2 do
    len := len + DistanceBetween2Pts( Bezier[i], Bezier[i+1]);

  chord := DistanceBetween2Pts( Bezier[0], Bezier[3]);

  if (len-chord) > Error then begin
    BezierSplit(Bezier, PLeft, PRight);
    AddIfClose( PLeft, Length, Error);
    AddIfClose( PRight, Length, Error);
    exit;
  end;

  Length := Length + len;
end;

function BezierLength( Bezier : TCubicBezier; Error : double): Double;
begin
  Result := 0;
  AddIfClose( Bezier, Result, Error);
end;


//==============================================================================
//
//                             TSVGShapeList
//
//==============================================================================

constructor TSVGShapeList.Create( AOwnsObjects: Boolean);
begin
  inherited Create( AOwnsObjects);
end;

destructor TSVGShapeList.Destroy;
begin
  inherited;
end;

function TSVGShapeList.GetShape( aIndex : integer) : TShape;
begin
  result := inherited Items[aindex] as TShape;
end;

procedure TSVGShapeList.SetShape( aIndex : integer; const aValue : TShape);
begin
  inherited Items[aindex] := aValue;
end;

function TSVGShapeList.AddShape( aValue : TShape): integer;
begin
  Result := inherited Add( aValue);
end;

//==============================================================================
//
//                               TSVGControl
//
//==============================================================================

constructor TSVGControl.Create( AOwner: TComponent);
begin
  inherited;

  FShapeList := TSVGShapeList.Create(True);
  FElementList := TStringList.Create;
end;

destructor TSVGControl.Destroy;
begin
  FElementList.Free;
  FShapeList.Free;

  inherited;
end;

function TSVGControl.ParseValue( aData : string; var i : integer): string;
begin
  Result := '';
  inc(i);
  while i <= Length(aData) do begin
    case aData[i] of
    ';', ',', ')' : exit;
    else
      Result := Result + aData[i];
    end;
    inc(i);
  end;
end;

function TSVGControl.ParseHexValue( aData : string; var i : integer): integer;
var HexValue : string;
begin
  inc(i);

  if aData[i] <> '#' then
    raise Exception.Create('# Expected');

  HexValue := ParseValue( aData, i);
  Result := strToInt64( '$ff' + HexValue);
end;

function TSVGControl.ParseFloatValue( aData : string; var i : integer): single;
var FloatValue : string;
begin
  FloatValue := ParseValue( aData, i);
  Result := StrToFloat( FloatValue);
end;

procedure TSVGControl.ParseMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
begin
  aMatrix.m11 := ParseFloatValue( aData, i);
  aMatrix.m12 := ParseFloatValue( aData, i);
  aMatrix.m13 := 0;
  aMatrix.m21 := ParseFloatValue( aData, i);
  aMatrix.m22 := ParseFloatValue( aData, i);
  aMatrix.m23 := 0;
  aMatrix.m31 := ParseFloatValue( aData, i);
  aMatrix.m32 := ParseFloatValue( aData, i);
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseTranslateMatrix( var aMatrix: TMatrix; aData: string; var i : integer);
begin
  aMatrix.m11 := 1;
  aMatrix.m12 := 0;
  aMatrix.m13 := 0;
  aMatrix.m21 := 0;
  aMatrix.m22 := 1;
  aMatrix.m23 := 0;
  aMatrix.m31 := ParseFloatValue( aData, i);
  aMatrix.m32 := ParseFloatValue( aData, i);
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseRotateMatrix(var aMatrix: TMatrix; aData: string;
  var i: integer);
var a : single;
begin
  a := ParseFloatValue( aData, i);
  aMatrix.m11 := cos(a);
  aMatrix.m12 := sin(a);
  aMatrix.m13 := 0;
  aMatrix.m21 := -sin(a);
  aMatrix.m22 := cos(a);
  aMatrix.m23 := 0;
  aMatrix.m31 := 0;
  aMatrix.m32 := 0;
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseScaleMatrix(var aMatrix: TMatrix; aData: string;
  var i: integer);
begin
  aMatrix.m11 := ParseFloatValue( aData, i);
  aMatrix.m12 := 0;
  aMatrix.m13 := 0;
  aMatrix.m21 := 0;
  aMatrix.m22 := ParseFloatValue( aData, i);
  aMatrix.m23 := 0;
  aMatrix.m31 := 0;
  aMatrix.m32 := 0;
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseSkewXMatrix(var aMatrix: TMatrix; aData: string;
  var i: integer);
var a : single;
begin
  a := ParseFloatValue( aData, i);
  aMatrix.m11 := 1;
  aMatrix.m12 := 0;
  aMatrix.m13 := 0;
  aMatrix.m21 := tan(a);
  aMatrix.m22 := 1;
  aMatrix.m23 := 0;
  aMatrix.m31 := 0;
  aMatrix.m32 := 0;
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseSkewYMatrix(var aMatrix: TMatrix; aData: string;
  var i: integer);
var a : single;
begin
  a := ParseFloatValue( aData, i);
  aMatrix.m11 := 1;
  aMatrix.m12 := tan(a);
  aMatrix.m13 := 0;
  aMatrix.m21 := 0;
  aMatrix.m22 := 1;
  aMatrix.m23 := 0;
  aMatrix.m31 := 0;
  aMatrix.m32 := 0;
  aMatrix.m33 := 1;
end;

procedure TSVGControl.ParseTransform( var aMatrix: TMatrix; aData: string);
var i : integer;
    Cmd : string;
begin
  Cmd := '';
  i := 1;
  while i <= Length(aData) do begin
    case aData[i] of
    '(' : begin
            if Cmd = 'matrix' then begin
              ParseMatrix( aMatrix, aData, i);
            end;
            if Cmd = 'translate' then begin
              ParseTranslateMatrix( aMatrix, aData, i);
            end;
            if Cmd = 'rotate' then begin
              ParseRotateMatrix( aMatrix, aData, i);
            end;
            if Cmd = 'scale' then begin
              ParseScaleMatrix( aMatrix, aData, i);
            end;
            if Cmd = 'skewX' then begin
              ParseSkewXMatrix( aMatrix, aData, i);
            end;
            if Cmd = 'skewY' then begin
              ParseSkewYMatrix( aMatrix, aData, i);
            end;

          end;
      else
        Cmd := Cmd + aData[i];
    end;

    inc(i);
  end;
end;

procedure TSVGControl.ParseStyle( aShape : TShape; aData : string);
var i, p : integer;
    Param, Value, HexValue : string;
begin
  Param := '';
  i := 1;
  while i <= Length(aData) do begin
    case aData[i] of
    ':' : begin
            if Param = 'fill' then begin
              Value := ParseValue( aData, i);
              if Value <> '' then begin
                if Value = 'none' then
                  aShape.Fill.Kind := TBrushKind.bkNone
                else
                  if Value.Chars[0] = '#' then begin
                    aShape.Fill.Color := strToInt64( '$ff' + copy(Value, 2, Length(Value)-1));
                  end;
              end;
            end;

            if Param = 'fill-opacity' then begin
              Value := ParseValue( aData, i);
              aShape.Opacity := StrToFloat(Value);
            end;

            if Param = 'fill-rule' then begin
              Value := ParseValue( aData, i);
              // TODO
            end;

            if Param = 'stroke' then begin
              Value := ParseValue( aData, i);
              if Value <> '' then begin
                if Value = 'none' then
                  aShape.Stroke.Kind := TBrushKind.bkNone
                else
                  if Value.Chars[0] = '#' then begin
                    aShape.Stroke.Color := strToInt64( '$ff' + copy(Value, 2, Length(Value)-1));
                  end;
              end;
            end;

            if Param = 'stroke-width' then begin
              Value := ParseValue( aData, i);
              if Value <> '' then begin
                p := pos('px', Value);
                if p > 0 then begin
                  aShape.StrokeThickness := StrToFloat( copy(Value, 1, p - 1));
                end else begin
                  aShape.StrokeThickness := StrToFloat( Value);
                end;
              end;
            end;

            if Param = 'stroke-linecap' then begin
              Value := ParseValue( aData, i);
              if Value = 'square' then
                aShape.Stroke.Cap := TStrokeCap.scFlat
              else
                aShape.Stroke.Cap := TStrokeCap.scRound;
            end;

            if Param = 'stroke-miterlimit' then begin
              Value := ParseValue( aData, i);
              // TODO
            end;

            Param := '';
          end;
      else
        Param := Param + aData[i];
    end;
    inc(i);
  end;
end;

procedure TSVGControl.ParseNode( aNode : TDOMNode; aParent : TControl);
var i, j : integer;
    id : string;
    R : TRectF;
    c : TDOMNodeList;
    n, ntspan, ntextPath : TDOMNode;
    Matrix : TMatrix;
    P1, P2 : TPointF;
    //Path : TSGVPath;
    Path : TPath;
    PathData : TSGVPathData;
    Layout : TLayout;
    Text : TText;

    procedure UpdateParentBounds( aControl : TControl; aRect : TRectF);
    begin
      if R.Width + R.Left > aControl.Width then
        aControl.Width := R.Width + R.Left;
      if R.Height + R.Top > aControl.Height then
        aControl.Height := R.Height + R.Top;
    end;

begin
  c := aNode.GetChildNodes;
  for i := 0 to c.Count - 1 do begin
    if c[i].NodeName = 'path' then begin
      Path := TPath.Create(self);
      Path.WrapMode := TPathWrapMode.pwOriginal;
      PathData := TSGVPathData.Create;

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        id := n.TextContent;
        j := FElementList.Add( id);
        FElementList.Objects[j] := Path;
      end;

      n := c[i].Attributes.GetNamedItem('d');
      if assigned(n) then begin
        PathData.ParseSGVPathString( n.TextContent);
        PathData.CalcArcLengths( 20);
      end;

      n := c[i].Attributes.GetNamedItem('transform');
      if assigned(n) then begin
        ParseTransform( Matrix, n.TextContent);
        PathData.ApplyMatrix( Matrix);
      end;

      R := PathData.GetBounds;
      Path.Data := PathData;
      UpdateParentBounds( Path, R);
      UpdateParentBounds( aParent, Path.BoundsRect);

      n := c[i].Attributes.GetNamedItem('style');
      if assigned(n) then begin
        ParseStyle( Path, n.TextContent);
      end;

      aParent.AddObject( Path);
    end;

    if c[i].NodeName = 'polygon' then begin
      Path := TPath.Create(self);
      Path.WrapMode := TPathWrapMode.pwOriginal;
      PathData := TSGVPathData.Create;

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        id := n.TextContent;
        j := FElementList.Add( id);
        FElementList.Objects[j] := Path;
      end;

      n := c[i].Attributes.GetNamedItem('points');
      if assigned(n) then begin
        PathData.ParseSGVPolygonString( n.TextContent);
        PathData.CalcArcLengths( 20);
      end;

      n := c[i].Attributes.GetNamedItem('transform');
      if assigned(n) then begin
        ParseTransform( Matrix, n.TextContent);
        PathData.ApplyMatrix( Matrix);
      end;

      R := PathData.GetBounds;
      Path.Data := PathData;
      UpdateParentBounds( Path, R);
      UpdateParentBounds( aParent, Path.BoundsRect);

      n := c[i].Attributes.GetNamedItem('style');
      if assigned(n) then begin
        ParseStyle( Path, n.TextContent);
      end;

      aParent.AddObject( Path);
    end;

    if c[i].NodeName = 'polyline' then begin
      Path := TPath.Create(self);
      Path.WrapMode := TPathWrapMode.pwOriginal;
      PathData := TSGVPathData.Create;

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        id := n.TextContent;
        j := FElementList.Add( id);
        FElementList.Objects[j] := Path;
      end;

      n := c[i].Attributes.GetNamedItem('points');
      if assigned(n) then begin
        PathData.ParseSGVPolygonString( n.TextContent);
        PathData.CalcArcLengths( 20);
      end;

      n := c[i].Attributes.GetNamedItem('transform');
      if assigned(n) then begin
        ParseTransform( Matrix, n.TextContent);
        PathData.ApplyMatrix( Matrix);
      end;

      R := PathData.GetBounds;
      Path.Data := PathData;
      UpdateParentBounds( Path, R);
      UpdateParentBounds( aParent, Path.BoundsRect);

      n := c[i].Attributes.GetNamedItem('style');
      if assigned(n) then begin
        ParseStyle( Path, n.TextContent);
      end;

      aParent.AddObject( Path);
    end;

    if c[i].NodeName = 'rect' then begin
      Path := TPath.Create(self);
      Path.WrapMode := TPathWrapMode.pwOriginal;
      PathData := TSGVPathData.Create;

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        j := FElementList.Add( n.TextContent);
        FElementList.Objects[j] := Path;
      end;

      n := c[i].Attributes.GetNamedItem('x');
      if assigned(n) then begin
        R.Left := StrToFloat( n.TextContent);
      end;

      n := c[i].Attributes.GetNamedItem('y');
      if assigned(n) then begin
        R.Top := StrToFloat( n.TextContent);
      end;

      n := c[i].Attributes.GetNamedItem('width');
      if assigned(n) then begin
        R.Width := StrToFloat( n.TextContent);
      end;

      n := c[i].Attributes.GetNamedItem('height');
      if assigned(n) then begin
        R.Height := StrToFloat( n.TextContent);
      end;

      PathData.AddRectangle( R, 1, 1, []);

      n := c[i].Attributes.GetNamedItem('transform');
      if assigned(n) then begin
        ParseTransform( Matrix, n.TextContent);
        PathData.ApplyMatrix( Matrix);
      end;

      R := PathData.GetBounds;
      Path.Data := PathData;
      UpdateParentBounds( Path, R);
      UpdateParentBounds( aParent, Path.BoundsRect);

      n := c[i].Attributes.GetNamedItem('style');
      if assigned(n) then begin
        ParseStyle( Path, n.TextContent);
      end;

      aParent.AddObject( Path);
    end;

    if c[i].NodeName = 'line' then begin
      Path := TPath.Create(self);
      Path.WrapMode := TPathWrapMode.pwOriginal;
      PathData := TSGVPathData.Create;

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        j := FElementList.Add( n.TextContent);
        FElementList.Objects[j] := Path;
      end;

      n := c[i].Attributes.GetNamedItem('x1');
      if assigned(n) then begin
        P1.X := StrToFloat( n.TextContent);
      end;

      n := c[i].Attributes.GetNamedItem('y1');
      if assigned(n) then begin
        P1.Y := StrToFloat( n.TextContent);
      end;

      PathData.MoveTo(P1);

      n := c[i].Attributes.GetNamedItem('x2');
      if assigned(n) then begin
        P2.X := StrToFloat( n.TextContent);
      end;

      n := c[i].Attributes.GetNamedItem('y2');
      if assigned(n) then begin
        P2.Y := StrToFloat( n.TextContent);
      end;

      PathData.LineTo(P2);

      n := c[i].Attributes.GetNamedItem('transform');
      if assigned(n) then begin
        ParseTransform( Matrix, n.TextContent);
        PathData.ApplyMatrix( Matrix);
      end;

      R := PathData.GetBounds;
      Path.Data := PathData;
      UpdateParentBounds( Path, R);
      UpdateParentBounds( aParent, Path.BoundsRect);

      n := c[i].Attributes.GetNamedItem('style');
      if assigned(n) then begin
        ParseStyle( Path, n.TextContent);
      end;

      aParent.AddObject( Path);
    end;

    if c[i].NodeName = 'text' then begin
      Text := TText.Create(self);

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        j := FElementList.Add( n.TextContent);
        FElementList.Objects[j] := Text;
      end;

      ntextPath := c[i].Attributes.GetNamedItem('textPath');
      if assigned(ntextPath) then begin
        ntspan := c[i].Attributes.GetNamedItem('tspan');
        if assigned(ntspan) then begin
          Text.Text := ntspan.NodeValue;
        end;
      end;
    end;

    if c[i].NodeName = 'g' then begin
      {Layout := TLayout.Create(self);

      n := c[i].Attributes.GetNamedItem('id');
      if assigned(n) then begin
        j := FElementList.Add( n.TextContent);
        FElementList.Objects[j] := Layout;
      end;

      aParent.AddObject(Layout);
      Layout.Align := TAlignLayout.alClient;
      Layout.SetBounds(0, 0, 400, 400 );

      ParseNode( c[i], Layout);}
      ParseNode( c[i], aParent);
    end;
  end;
end;

procedure TSVGControl.ParseXML( aSVGDoc : TXMLDocument);
var Root : TDomNode;
begin
  Root := aSVGDoc.FindNode('svg');
  if not assigned(Root) then
    raise Exception.Create('Root node "svg" not found.');
  ParseNode( Root, self);
end;

{procedure TSVGControl.Paint;
var i : integer;
    Path : TPath;
begin
  Canvas.BeginScene;
  for i := 0 to FShapeList.Count - 1 do begin
    //if FShapeList[i] is TPath then begin
    //  Path := FShapeList[i] as TPath;
    //  Canvas.DrawPath( Path.Data, Path.Opacity);
    //end;
    FShapeList[i].Scale.X := Scale.X;
    FShapeList[i].Scale.Y := Scale.Y;
    FShapeList[i].PaintTo( Canvas, BoundsRect);
  end;

  Canvas.EndScene;
end;}

procedure Register;
begin
  RegisterComponents('G2 FMX Controls', [TSVGControl]);
end;

{ TSGVPathData }

constructor TSGVPathData.Create;
begin
  inherited Create;
end;

destructor TSGVPathData.Destroy;
begin
  inherited;
end;

function TSGVPathData.GetTempCount: Integer;
begin
  Result := Length(FTempPathData);
end;

function TSGVPathData.GetPoint(AIndex: Integer): TPathPoint;
begin
  Result := FTempPathData[AIndex];
end;

{procedure TSGVPathData.ReadPath(Stream: TStream);
var
  S: Longint;
  I: Integer;
  k: Byte;
  P: TPointF;
begin
  Stream.Read(S, SizeOf(S));}
//{$IFDEF FPC_BIG_ENDIAN}
//  ReverseBytes(@S, 4);
//{$ENDIF}
{  SetLength(FTempPathData, S);
  if S > 0 then
  begin
    if (Stream.Size - 4) div S = 9 then
    begin
      for I := 0 to S - 1 do
      begin
        Stream.Read(k, 1);
        Stream.Read(P, SizeOf(P));}
//{$IFDEF FPC_BIG_ENDIAN}
{        ReverseBytes(@P.X, 4);
        ReverseBytes(@P.Y, 4);}
//{$ENDIF}
{        FTempPathData[I].Kind := TPathPointKind(k);
        FTempPathData[I].Point := P;
      end;
    end
    else
    begin
      Stream.Read(FTempPathData[0], S * SizeOf(TPathPoint));}
//{$IFDEF FPC_BIG_ENDIAN}
{      for I := 0 to S * 3 - 1 do
        ReverseBytes(@PColorArray(PathData)[I], 4);}
//{$ENDIF}
{    end;
  end;
  FRecalcBounds := True;
end;

procedure TSGVPathData.WritePath(Stream: TStream);
var
  S: Longint;
begin
  S := TempCount;
  Stream.Write(S, SizeOf(S));
  if S > 0 then
    Stream.Write(FTempPathData[0], S * SizeOf(TPathPoint));
end;}

procedure TSGVPathData.ApplyPathData;
var
  S: Longint;
  MemoryStream : TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    S := TempCount;
    MemoryStream.Write(S, SizeOf(S));
    if S > 0 then
      MemoryStream.Write(FTempPathData[0], S * SizeOf(TPathPoint));

    MemoryStream.Position := 0;
    inherited ReadPath(MemoryStream);
  finally
    MemoryStream.Free;
  end;
end;

function TSGVPathData.LastPoint: TPointF;
begin
  if TempCount > 0 then
    Result := FTempPathData[High(FTempPathData)].Point
  else
    Result := PointF(0, 0);
end;

procedure TSGVPathData.MoveTo(const P: TPointF);
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppMoveTo;
  FTempPathData[High(FTempPathData)].Point := P;
  FStartPoint := FTempPathData[High(FTempPathData)].Point;
  FRecalcBounds := True;
end;

procedure TSGVPathData.MoveToRel(const P: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppMoveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + P.X, LP.Y + P.Y);
  FStartPoint := FTempPathData[High(FTempPathData)].Point;
  FRecalcBounds := True;
end;

procedure TSGVPathData.LineTo(const P: TPointF);
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := P;
  FRecalcBounds := True;
end;

procedure TSGVPathData.LineToRel(const P: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + P.X, LP.Y + P.Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.HLineTo(const X: Single);
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := PointF(X, FTempPathData[High(FTempPathData) - 1].Point.Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.HLineToRel(const X: Single);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + X, LP.Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.VLineTo(const Y: Single);
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := PointF(FTempPathData[High(FTempPathData) - 1].Point.X, Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.VLineToRel(const Y: Single);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppLineTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X, LP.Y + Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.QuadCurveTo(const ControlPoint, EndPoint: TPointF);
var
  LP, CP1, CP2: TPointF;
begin
  LP := LastPoint;
  CP1.X := (1/3 * LP.X) + (2/3 * ControlPoint.X);
  CP1.Y := (1/3 * LP.Y) + (2/3 * ControlPoint.Y);
  CP2.X := (2/3 * ControlPoint.X) + (1/3 * EndPoint.X);
  CP2.Y := (2/3 * ControlPoint.Y) + (1/3 * EndPoint.Y);
  CurveTo(CP1, CP2, EndPoint);
end;

procedure TSGVPathData.CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := ControlPoint1;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := ControlPoint2;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := EndPoint;
  FRecalcBounds := True;
end;

procedure TSGVPathData.CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TPointF);
var
  LP: TPointF;
begin
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + ControlPoint1.X, LP.Y + ControlPoint1.Y);
  // bve controlpoints seem to be relative to first point
  //LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + ControlPoint2.X, LP.Y + ControlPoint2.Y);;
  //LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + EndPoint.X, LP.Y + EndPoint.Y);;
  FRecalcBounds := True;
end;

procedure TSGVPathData.SmoothCurveTo(const ControlPoint2, EndPoint: TPointF);
var
  ControlPoint1: TPointF;
begin
  if TempCount > 2 then
  begin
    ControlPoint1.X := LastPoint.X + (LastPoint.X - FTempPathData[High(FTempPathData) - 1].Point.X);
    ControlPoint1.Y := LastPoint.Y + (LastPoint.Y - FTempPathData[High(FTempPathData) - 1].Point.Y);
  end
  else
    ControlPoint1 := ControlPoint2;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := ControlPoint1;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := ControlPoint2;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := EndPoint;
  FRecalcBounds := True;
end;

procedure TSGVPathData.SmoothCurveToRel(const ControlPoint2, EndPoint: TPointF);
var
  LP, ControlPoint1: TPointF;
begin
  if TempCount > 2 then
  begin
    ControlPoint1.X := LastPoint.X + (LastPoint.X - FTempPathData[High(FTempPathData) - 1].Point.X);
    ControlPoint1.Y := LastPoint.Y + (LastPoint.Y - FTempPathData[High(FTempPathData) - 1].Point.Y);
  end
  else
    ControlPoint1 := ControlPoint2;

  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(ControlPoint1.X, ControlPoint1.Y);
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + ControlPoint2.X, LP.Y + ControlPoint2.Y);
  LP := LastPoint;
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppCurveTo;
  FTempPathData[High(FTempPathData)].Point := PointF(LP.X + EndPoint.X, LP.Y + EndPoint.Y);
  FRecalcBounds := True;
end;

procedure TSGVPathData.ClosePath;
begin
  SetLength(FTempPathData, TempCount + 1);
  FTempPathData[High(FTempPathData)].Kind := TPathPointKind.ppClose;
  FTempPathData[High(FTempPathData)].Point := FStartPoint;
  FRecalcBounds := True;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TSGVPathData.Clear;
begin
  FRecalcBounds := True;
  SetLength(FTempPathData, 0);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{function TSGVPathData.GetBounds: TRectF;
var
  I: Integer;
begin
  if Length(FTempPathData) = 0 then
  begin
    Result := RectF(0, 0, 0, 0);
    Exit;
  end;
  if FRecalcBounds then
  begin
    Result := RectF($FFFF, $FFFF, -$FFFF, -$FFFF);
    for I := 0 to High(FTempPathData) do
    begin
      if FTempPathData[I].Kind = TPathPointKind.ppClose then
        Continue;

      if FTempPathData[I].Point.X < Result.Left then
        Result.Left := FTempPathData[I].Point.X;
      if FTempPathData[I].Point.X > Result.Right then
        Result.Right := FTempPathData[I].Point.X;
      if FTempPathData[I].Point.Y < Result.Top then
        Result.Top := FTempPathData[I].Point.Y;
      if FTempPathData[I].Point.Y > Result.Bottom then
        Result.Bottom := FTempPathData[I].Point.Y;
    end;
    // add small amount
    if RectWidth(Result) = 0 then
      Result.Right := Result.Left + 0.001;
    if RectHeight(Result) = 0 then
      Result.Bottom := Result.Top + 0.001;
    FBounds := Result;
    FRecalcBounds := False;
  end
  else
    Result := FBounds;
end;}

procedure TSGVPathData.Scale(const scaleX, scaleY: Single);
var
  I: Integer;
begin
  if Length(FTempPathData) > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FTempPathData) do
      case FTempPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FTempPathData[I].Point.X := FTempPathData[I].Point.X * scaleX;
            FTempPathData[I].Point.Y := FTempPathData[I].Point.Y * scaleY;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
  end;
end;

procedure TSGVPathData.Translate(const dX, dY: Single);
var
  I: Integer;
begin
  if TempCount > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FTempPathData) do
      case FTempPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FTempPathData[I].Point.X := FTempPathData[I].Point.X + dX;
            FTempPathData[I].Point.Y := FTempPathData[I].Point.Y + dY;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
  end;
end;

procedure TSGVPathData.FitToRect(const ARect: TRectF);
var
  B: TRectF;
begin
  B := GetBounds;
  Translate(-B.Left, -B.Top);
  Scale(RectWidth(ARect) / RectWidth(B), RectHeight(ARect) / RectHeight(B));
  Translate(ARect.Left, ARect.Top);
end;

procedure TSGVPathData.ApplyMatrix(const M: TMatrix);
var
  I: Integer;
begin
  if Length(FTempPathData) > 0 then
  begin
    FRecalcBounds := True;
    for I := 0 to High(FTempPathData) do
      case FTempPathData[I].Kind of
        TPathPointKind.ppMoveTo, TPathPointKind.ppLineTo, TPathPointKind.ppCurveTo:
          begin
            FTempPathData[I].Point := M.Transform(Vector(FTempPathData[I].Point)).ToPointF;
          end;
        TPathPointKind.ppClose:
          begin
          end;
      end;
    ApplyPathData;
  end;
end;

procedure TSGVPathData.Flatten(const Flatness: Single = 0.25);

  {procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
  begin
    cx := 3.0 * (Bezier[1].X - Bezier[0].X);
    cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
    bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
    by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
    ax := Bezier[3].X - Bezier[0].X - cx - bx;
    ay := Bezier[3].Y - Bezier[0].Y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
  var
    tSqr: Single;
    tCube: Single;
  begin
    tSqr := t * t;
    tCube := tSqr * t;
    Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
    Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
  end;}

  function CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
  var
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    dT: Single;
    t: Single;
    I: Integer;
  begin
    if PointCount = 0 then
      Exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    t := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for I := 0 to PointCount - 1 do
    begin
      Result[I] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, t);
      t := t + dT;
    end;
  end;

var
  I, j: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  len: Single;
  SegCount: Integer;
  OldPathData: array of TPathPoint;
  CurPoint: TPointF;
  F, S: Single;
  Bounds, R: TRectF;
begin
  // scale
  if Length(FTempPathData) > 0 then
  begin
    FRecalcBounds := True;
    Bounds := GetBounds;
    R := Bounds;
    R.Fit(RectF(0, 0, 100, 100));
    S := Min(Extended(RectWidth(Bounds) / 100), Extended(RectHeight(Bounds) / 100));
    F := Flatness * S;
    if F < 0.05 then
      F := 0.05;

    // copy data
    SetLength(OldPathData, TempCount);
    System.Move(FTempPathData[0], OldPathData[0], TempCount * SizeOf(FTempPathData[0]));
    SetLength(FTempPathData, 0);

    I := 0;
    while I < Length(OldPathData) do
    begin
      case OldPathData[I].Kind of
        TPathPointKind.ppMoveTo:
          begin
            MoveTo(OldPathData[I].Point);
            CurPoint := OldPathData[I].Point;
          end;
        TPathPointKind.ppLineTo:
          begin
            LineTo(OldPathData[I].Point);
            CurPoint := OldPathData[I].Point;
          end;
        TPathPointKind.ppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := OldPathData[I].Point;
            inc(I);
            B[2] := OldPathData[I].Point;
            inc(I);
            B[3] := OldPathData[I].Point;
            len := (Vector(B[1]) - Vector(B[3])).Length;
            SegCount := round(len / F);
            if SegCount < 2 then
              SegCount := 2;
            BPts := CreateBezier(B, SegCount);
            for j := 0 to High(BPts) do
            begin
              LineTo(BPts[j]);
            end;
            CurPoint := OldPathData[I].Point;
          end;
        TPathPointKind.ppClose:
          begin
            ClosePath;
          end;
      end;
      inc(I);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

function TSGVPathData.FlattenToPolygon(var Polygon: TPolygon; const Flatness: Single = 0.25): TPointF;

  {procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
  begin
    cx := 3.0 * (Bezier[1].X - Bezier[0].X);
    cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
    bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
    by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
    ax := Bezier[3].X - Bezier[0].X - cx - bx;
    ay := Bezier[3].Y - Bezier[0].Y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
  var
    tSqr: Single;
    tCube: Single;
  begin
    tSqr := t * t;
    tCube := tSqr * t;
    Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
    Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
  end;}

  function CreateBezier(const Bezier: TCubicBezier; const PointCount: Integer): TPolygon;
  var
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    dT: Single;
    t: Single;
    I: Integer;
  begin
    if PointCount = 0 then
      Exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    t := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for I := 0 to PointCount - 1 do
    begin
      Result[I] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, t);
      t := t + dT;
    end;
  end;

  procedure AddPoint(const P: TPointF);
  begin
    if (Length(Polygon) > 0) and (SameValue(P.X, Polygon[High(Polygon)].X, Flatness / 10) and SameValue(P.Y, Polygon[High(Polygon)].Y, Flatness / 10)) then
      Exit;
    SetLength(Polygon, Length(Polygon) + 1);
    Polygon[High(Polygon)] := P;
  end;

var
  I, j: Integer;
  BPts: TPolygon;
  B: TCubicBezier;
  SP, CurPoint: TPointF;
  len: Single;
  SegCount: Integer;
  F, S: Single;
  Bounds, R: TRectF;
begin
  Result := PointF(0, 0);
  SetLength(Polygon, 0);
  if Length(FTempPathData) > 0 then
  begin
    FRecalcBounds := True;
    Bounds := GetBounds;
    R := Bounds;
    R.Fit(RectF(0, 0, 100, 100));
    S := Min(Extended(RectWidth(Bounds) / 100), Extended(RectHeight(Bounds) / 100));
    F := Flatness * S;
    if F < 0.05 then
      F := 0.05;

    I := 0;
    while I < TempCount do
    begin
      case FTempPathData[I].Kind of
        TPathPointKind.ppMoveTo:
          begin
            AddPoint(FTempPathData[I].Point);
            CurPoint := FTempPathData[I].Point;
            SP := CurPoint;
          end;
        TPathPointKind.ppLineTo:
          begin
            AddPoint(FTempPathData[I].Point);
            CurPoint := FTempPathData[I].Point;
          end;
        TPathPointKind.ppCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := FTempPathData[I].Point;
            inc(I);
            B[2] := FTempPathData[I].Point;
            inc(I);
            B[3] := FTempPathData[I].Point;
            len := (Vector(B[1])- Vector(B[3])).Length;
            SegCount := round(len / F);
            if SegCount < 2 then
              SegCount := 2;
            BPts := CreateBezier(B, SegCount);
            for j := 0 to High(BPts) do
              AddPoint(BPts[j]);
            CurPoint := FTempPathData[I].Point;
          end;
        TPathPointKind.ppClose:
          begin
            AddPoint(SP);
            AddPoint(ClosePolygon);
          end;
      end;
      inc(I);
    end;
    Result := PointF(Abs(GetBounds.Width), Abs(GetBounds.Height));
  end;
end;

procedure TSGVPathData.CalcArcLengths( steps : integer);
var i, psi, spi, asi : integer;
    dl : single;
    Sp, CurPoint : TPointF;
    B : TCubicBezier;

  procedure AddArcLength( aArcLength : single; aPointSegmentIndex, aStartPointIndex, aArcSegmentIndex, aArcSegmentCount : integer);
  begin
    if FArcLengthCount + 1 > Length(FArcLengths)  then
      SetLength( FArcLengths, Length(FArcLengths) + 50);
    FArcLengths[ FArcLengthCount].ArcLength := aArcLength;
    FArcLengths[ FArcLengthCount].PointSegmentIndex := aPointSegmentIndex;
    FArcLengths[ FArcLengthCount].StartPointIndex := aStartPointIndex;
    FArcLengths[ FArcLengthCount].ArcSegmentIndex := aArcSegmentIndex;
    FArcLengths[ FArcLengthCount].ArcSegmentCount := aArcSegmentCount;

    inc(FArcLengthCount);
  end;

  {procedure CalculateBezierCoefficients(const Bezier: TCubicBezier; out ax, bx, cx, ay, by, cy: Single);
  begin
    cx := 3.0 * (Bezier[1].X - Bezier[0].X);
    cy := 3.0 * (Bezier[1].Y - Bezier[0].Y);
    bx := 3.0 * (Bezier[2].X - Bezier[1].X) - cx;
    by := 3.0 * (Bezier[2].Y - Bezier[1].Y) - cy;
    ax := Bezier[3].X - Bezier[0].X - cx - bx;
    ay := Bezier[3].Y - Bezier[0].Y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TPointF; const ax, bx, cx, ay, by, cy, t: Single): TPointF;
  var
    tSqr: Single;
    tCube: Single;
  begin
    tSqr := t * t;
    tCube := tSqr * t;
    Result.X := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.X;
    Result.Y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.Y;
  end;}

  procedure AddBezier(const Bezier: TCubicBezier; const PointCount: Integer);
  var
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    dT: Single;
    t: Single;
    I: Integer;
    BP : TPointF;

  begin
    if PointCount = 0 then
      Exit;

    dT := 1.0 / (1.0 * PointCount - 1.0);
    t := dT;

    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);

    for I := 1 to PointCount - 1 do
    begin
      BP := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, t);

      dl := DistanceBetween2Pts(BP, CurPoint); //todo
      FPathLength := FPathLength + dl;
      AddArcLength( FPathLength, psi, spi, asi-1, PointCount-1);
      CurPoint := BP;

      t := t + dT;
    end;
  end;

begin
  FArcLengthCount := 0;
  FPathLength := 0;

  i := 0;
  psi := 0;
  spi := 0;
  asi := 0;
  while i < TempCount do
  begin
    case FTempPathData[i].Kind of
      TPathPointKind.ppMoveTo:
        begin
          CurPoint := FTempPathData[i].Point;
          AddArcLength( FPathLength, psi, spi, asi, 1);
          SP := CurPoint;
          asi := FArcLengthCount;
          inc(psi);
          spi := i;
        end;
      TPathPointKind.ppLineTo:
        begin
          dl := DistanceBetween2Pts(FTempPathData[i].Point, CurPoint);
          FPathLength := FPathLength + dl;
          AddArcLength( FPathLength, psi, spi, asi-1, 1);
          CurPoint := FTempPathData[i].Point;
          asi := FArcLengthCount;
          inc(psi);
          spi := i;
        end;
      TPathPointKind.ppCurveTo:
        begin
          B[0] := CurPoint;
          B[1] := FTempPathData[i].Point;
          inc(i);
          B[2] := FTempPathData[i].Point;
          inc(i);
          B[3] := FTempPathData[i].Point;
          AddBezier( B, steps);
          asi := FArcLengthCount;
          psi := psi + 3;
          spi := i;
          //dl := BezierLength( B, 0.001);
        end;
      TPathPointKind.ppClose:
        begin
          dl := DistanceBetween2Pts(SP, CurPoint);
          FPathLength := FPathLength + dl;
          AddArcLength( FPathLength, psi, spi, asi-1, 1);
        end;
    end;
    inc(i);
  end;
end;

// http://www.planetclegg.com/projects/WarpingTextToSplines.html

procedure TSGVPathData.GetPointOnPath( u : single; var P : TPointF; var A : single);
var targetArcLength, t, lBefore, lAfter, SegmentLength,
    SegmentFraction, M : single;
    i, psi, spi, asi, asc : integer;
    ax: Single;
    bx: Single;
    cx: Single;
    ay: Single;
    by: Single;
    cy: Single;
    B : TCubicBezier;
    p1, p2, N : TPointF;

  function BinSearch( Value : single): Integer;
  var
    First: Integer;
    Last: Integer;
    Pivot: Integer;
    Found: Boolean;
  begin
    First  := 0;                     //Sets the first item of the range
    Last   := FArcLengthCount - 1;   //Sets the last item of the range
    Found  := False;                 //Initializes the Found flag (Not found yet)
    Result := 0;                     //Initializes the Result

    if (FArcLengths[0].ArcLength >= Value) then begin
      Found  := True;
      exit;
    end;

    //If First > Last then the searched item doesn't exist
    //If the item is found the loop will stop
    while (First <= Last) and (not Found) do
    begin
      //Gets the middle of the selected range
      Pivot := (First + Last) div 2;
      //Compares the String in the middle with the searched one
      //if FArcLengths[Pivot].ArcLength = Value then
      if (Pivot = FArcLengthCount - 1) then begin
        if (FArcLengths[Pivot].ArcLength <= Value) then begin
          Found  := True;
          Result := Pivot;
        end;
      end else
        if (FArcLengths[Pivot].ArcLength <= Value) and ((FArcLengths[Pivot+1].ArcLength > Value)) then
        begin
          Found  := True;
          Result := Pivot;
        end
      //If the Item in the middle has a bigger value than
      //the searched item, then select the first half
      else if FArcLengths[Pivot].ArcLength > Value then
        Last := Pivot - 1
          //else select the second half
      else
        First := Pivot + 1;
    end;
  end;

begin
  if u >= 1 then
    exit;

  // get the target arcLength for curve for parameter u
  targetArcLength := u * FArcLengths[ FArcLengthCount -1].ArcLength;

  // the next function would be a binary search, for efficiency
  i := BinSearch(targetArcLength);

  if FArcLengths[i].ArcLength = targetArcLength then begin
    // if exact match, return t based on exact index

    asi := FArcLengths[i+1].ArcSegmentIndex;
    asc := FArcLengths[i+1].ArcSegmentCount;

    t := (i-asi) / asc;

  end else begin
    // need to interpolate between two points
    lBefore := FArcLengths[i].ArcLength;
    lAfter :=  FArcLengths[i+1].ArcLength;
    SegmentLength := lAfter - lBefore;

    // determine where we are between the 'before' and 'after' points.
    if SegmentLength <> 0 then
      SegmentFraction := (targetArcLength - lBefore) / SegmentLength
    else
      SegmentLength := 0;

    // add that fractional amount to t
    asi := FArcLengths[i+1].ArcSegmentIndex;
    asc := FArcLengths[i+1].ArcSegmentCount;
    t := ( (i - asi) + SegmentFraction) / asc;
  end;

  psi := FArcLengths[i+1].PointSegmentIndex;
  spi := FArcLengths[i+1].StartPointIndex;

  if FTempPathData[psi].Kind in [TPathPointKind.ppLineTo, TPathPointKind.ppClose] then begin
    p1 := FTempPathData[spi].Point;
    p2 := FTempPathData[psi].Point;

    P.X := p1.X + t*(p2.X - p1.X);
    P.Y := p1.Y + t*(p2.Y - p1.Y);

    //M := sqrt(P.X * P.X + P.Y * P.Y);

    N.X := P.Y;
    N.Y := -P.X;

    A := ArcTan2(N);

  end else
    if FTempPathData[psi].Kind = TPathPointKind.ppCurveTo then begin
      B[0] := FTempPathData[spi].Point;
      B[1] := FTempPathData[psi].Point;
      B[2] := FTempPathData[psi+1].Point;
      B[3] := FTempPathData[psi+2].Point;

      CalculateBezierCoefficients(B, ax, bx, cx, ay, by, cy);
      P := PointOnBezier(B[0], ax, bx, cx, ay, by, cy, t);
      N := TangentOnBezier(ax, bx, cx, ay, by, cy, t);
      A := ArcTan2(N);
      //A := AngleOnBezier(ax, bx, cx, ay, by, cy, t);
    end;
end;

procedure TSGVPathData.AddEllipse(const ARect: TRectF);
var
  cx, cy: Single;
  px, py: Single;
begin
  cx := (ARect.Left + ARect.Right) / 2;
  cy := (ARect.Top + ARect.Bottom) / 2;
  px := CurveKappa * (RectWidth(ARect) / 2);
  py := CurveKappa * (RectHeight(ARect) / 2);
  MoveTo(PointF(ARect.Left, cy));
  CurveTo(PointF(ARect.Left, cy - py), PointF(cx - px, ARect.Top), PointF(cx, ARect.Top));
  CurveTo(PointF(cx + px, ARect.Top), PointF(ARect.Right, cy - py), PointF(ARect.Right, cy));
  CurveTo(PointF(ARect.Right, cy + py), PointF(cx + px, ARect.Bottom), PointF(cx, ARect.Bottom));
  CurveTo(PointF(cx - px, ARect.Bottom), PointF(ARect.Left, cy + py), PointF(ARect.Left, cy));
end;

procedure TSGVPathData.AddRectangle(const ARect: TRectF; const XRadius, YRadius: Single; const ACorners: TCorners;
  const ACornerType: TCornerType = TCornerType.ctRound);
var
  R: TRectF;
  X1, X2, Y1, Y2: Single;
begin
  R := ARect;
  X1 := XRadius;
  if RectWidth(R) - (X1 * 2) < 0 then
    X1 := (XRadius * (RectWidth(R) / (X1 * 2)));
  X2 := X1 / 2;
  Y1 := YRadius;
  if RectHeight(R) - (Y1 * 2) < 0 then
    Y1 := (YRadius * (RectHeight(R) / (Y1 * 2)));
  Y2 := Y1 / 2;

  MoveTo(PointF(R.Left, R.Top + Y1));
  if TCorner.crTopLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Left + X1, R.Top));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Left + X2, R.Top + Y1), PointF(R.Left + X1, R.Top + Y2), PointF(R.Left + X1, R.Top));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Left + X2, R.Top + Y1));
          LineTo(PointF(R.Left + X1, R.Top + Y2));
          LineTo(PointF(R.Left + X1, R.Top));
        end;
    else
      CurveTo(PointF(R.Left, R.Top + (Y2)), PointF(R.Left + X2, R.Top), PointF(R.Left + X1, R.Top))
    end;
  end
  else
  begin
    LineTo(PointF(R.Left, R.Top));
    LineTo(PointF(R.Left + X1, R.Top));
  end;
  LineTo(PointF(R.Right - X1, R.Top));
  if TCorner.crTopRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Right, R.Top + Y1));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Right - X1, R.Top + Y2), PointF(R.Right - X2, R.Top + Y1), PointF(R.Right, R.Top + Y1));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Right - X1, R.Top + Y2));
          LineTo(PointF(R.Right - X2, R.Top + Y1));
          LineTo(PointF(R.Right, R.Top + Y1));
        end;
    else
      CurveTo(PointF(R.Right - X2, R.Top), PointF(R.Right, R.Top + (Y2)), PointF(R.Right, R.Top + Y1))
    end;
  end
  else
  begin
    LineTo(PointF(R.Right, R.Top));
    LineTo(PointF(R.Right, R.Top + Y1));
  end;
  LineTo(PointF(R.Right, R.Bottom - Y1));
  if TCorner.crBottomRight in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Right - X1, R.Bottom));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Right - X2, R.Bottom - Y1), PointF(R.Right - X1, R.Bottom - Y2), PointF(R.Right - X1, R.Bottom));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Right - X2, R.Bottom - Y1));
          LineTo(PointF(R.Right - X1, R.Bottom - Y2));
          LineTo(PointF(R.Right - X1, R.Bottom));
        end;
    else
      CurveTo(PointF(R.Right, R.Bottom - (Y2)), PointF(R.Right - X2, R.Bottom), PointF(R.Right - X1, R.Bottom))
    end;
  end
  else
  begin
    LineTo(PointF(R.Right, R.Bottom));
    LineTo(PointF(R.Right - X1, R.Bottom));
  end;
  LineTo(PointF(R.Left + X1, R.Bottom));
  if TCorner.crBottomLeft in ACorners then
  begin
    case ACornerType of
      // ctRound - default
      TCornerType.ctBevel:
        LineTo(PointF(R.Left, R.Bottom - Y1));
      TCornerType.ctInnerRound:
        CurveTo(PointF(R.Left + X1, R.Bottom - Y2), PointF(R.Left + X2, R.Bottom - Y1), PointF(R.Left, R.Bottom - Y1));
      TCornerType.ctInnerLine:
        begin
          LineTo(PointF(R.Left + X1, R.Bottom - Y2));
          LineTo(PointF(R.Left + X2, R.Bottom - Y1));
          LineTo(PointF(R.Left, R.Bottom - Y1));
        end;
    else
      CurveTo(PointF(R.Left + X2, R.Bottom), PointF(R.Left, R.Bottom - (Y2)), PointF(R.Left, R.Bottom - Y1))
    end;
  end
  else
  begin
    LineTo(PointF(R.Left, R.Bottom));
    LineTo(PointF(R.Left, R.Bottom - Y1));
  end;
  ClosePath;
end;

procedure DrawArcWithBezier(Path: TSGVPathData; CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: Single;
  UseMoveTo: Boolean);
var
  Coord: array of TPointF;
  Pts: array of TPointF;
  A, B, C, X, Y: Single;
  ss, cc: Single;
  I: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
    begin
      if (Length(Path.FTempPathData) = 0) then
        Path.MoveTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)))
      else
        Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    end;
    Path.LineTo(PointF(CenterX + RadiusX * Cos(StartAngle), CenterY - RadiusY * Sin(StartAngle)));
    Exit;
  end;
  B := Sin(SweepRange / 2);
  C := Cos(SweepRange / 2);
  A := 1 - C;
  X := A * 4.0 / 3.0;
  Y := B - X * C / B;
  ss := Sin(StartAngle + SweepRange / 2);
  cc := Cos(StartAngle + SweepRange / 2);
  SetLength(Coord, 4);
  Coord[0] := PointF(C, -B);
  Coord[1] := PointF(C + X, -Y);
  Coord[2] := PointF(C + X, Y);
  Coord[3] := PointF(C, B);
  SetLength(Pts, 4);
  for I := 0 to 3 do
  begin
    Pts[I] := PointF(CenterX + RadiusX * (Coord[I].X * cc - Coord[I].Y * ss),
      CenterY + RadiusY * (Coord[I].X * ss + Coord[I].Y * cc));
  end;
  if UseMoveTo then
  begin
    if (Length(Path.FTempPathData) = 0) then
      Path.MoveTo(Pts[0])
    else
      Path.LineTo(Pts[0]);
  end;
  Path.CurveTo(Pts[1], Pts[2], Pts[3]);
end;

procedure TSGVPathData.AddArc(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  total_sweep, local_sweep, prev_sweep: Single;
  done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);

  I := trunc(StartAngle / (2.0 * cPI));
  F := StartAngle - (I * 2.0 * cPI);

  StartAngle := F;

  if SweepAngle >= 2.0 * cPI then
    SweepAngle := 2.0 * cPI;
  if SweepAngle <= -2.0 * cPI then
    SweepAngle := -2.0 * cPI;

  if Abs(SweepAngle) < 1E-10 then
  begin
    Exit;
  end;

  total_sweep := 0.0;

  done := False;
  UseMoveTo := True;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep := -cPI * 0.5;
      total_sweep := total_sweep - (cPI * 0.5);
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep := cPI * 0.5;
      total_sweep := total_sweep + (Pi * 0.5);
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TSGVPathData.AddArcSvgPart(const Center, Radius: TPointF; StartAngle, SweepAngle: Single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: Boolean;
  I: Integer;
  F: Single;
  total_sweep, local_sweep, prev_sweep: Single;
  done: Boolean;
begin
  StartAngle := DegToRad(StartAngle);
  SweepAngle := DegToRad(SweepAngle);

  I := trunc(StartAngle / (2.0 * cPI));
  F := StartAngle - (I * 2.0 * cPI);

  StartAngle := F;

  if SweepAngle >= 2.0 * cPI then
    SweepAngle := 2.0 * cPI;
  if SweepAngle <= -2.0 * cPI then
    SweepAngle := -2.0 * cPI;

  if Abs(SweepAngle) < 1E-10 then
  begin
    Exit;
  end;

  total_sweep := 0.0;

  done := False;
  UseMoveTo := False;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep := -cPI * 0.5;
      total_sweep := total_sweep - (cPI * 0.5);
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep := cPI * 0.5;
      total_sweep := total_sweep + (Pi * 0.5);
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := True;
      end;
    end;
    DrawArcWithBezier(Self, Center.X, Center.Y, Radius.X, Radius.Y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := False;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TSGVPathData.AddArcSvg(const P1, Radius: TPointF; Angle: Single; const LargeFlag, SweepFlag: Boolean;
  const P2: TPointF);
var
  I: Integer;
  m_radii_ok: Boolean;
  V, P, N, sq, rx, ry, x0, y0, X1, Y1, X2, Y2, cx, cy, ux, uy, vx, vy,

    dx2, dy2, prx, pry, px1, py1, cx1, cy1, sx2, sy2,

    sign, coef,

    radii_check, start_angle, sweep_angle,

    cos_a, sin_a: Single;
  tm: TMatrix;
  len: Integer;
begin
  // Trivial case: Arc transformate to point
  if P1 = P2 then
    Exit;

  rx := Radius.X;
  ry := Radius.Y;
  x0 := P1.X;
  y0 := P1.Y;
  X2 := P2.X;
  Y2 := P2.Y;
  Angle := DegToRad(Angle);

  m_radii_ok := True;

  if rx < 0.0 then
    rx := -rx;

  if ry < 0.0 then
    ry := -rx;

  // Calculate the middle point between
  // the current and the final points
  dx2 := (x0 - X2) / 2.0;
  dy2 := (y0 - Y2) / 2.0;

  // Convert angle from degrees to radians
  cos_a := Cos(Angle);
  sin_a := Sin(Angle);

  // Calculate (x1, y1)
  X1 := cos_a * dx2 + sin_a * dy2;
  Y1 := -sin_a * dx2 + cos_a * dy2;

  // Ensure radii are large enough
  prx := rx * rx;
  pry := ry * ry;
  px1 := X1 * X1;
  py1 := Y1 * Y1;

  // Check that radii are large enough
  radii_check := px1 / prx + py1 / pry;

  if radii_check > 1.0 then
  begin
    rx := Sqrt(radii_check) * rx;
    ry := Sqrt(radii_check) * ry;
    prx := rx * rx;
    pry := ry * ry;

    if radii_check > 10.0 then
      m_radii_ok := False;

  end;

  // Calculate (cx1, cy1)
  if LargeFlag = SweepFlag then
    sign := -1.0
  else
    sign := 1.0;

  sq := (prx * pry - prx * py1 - pry * px1) / (prx * py1 + pry * px1);

  if sq < 0 then
    coef := sign * Sqrt(0)
  else
    coef := sign * Sqrt(sq);

  cx1 := coef * ((rx * Y1) / ry);
  cy1 := coef * -((ry * X1) / rx);

  // Calculate (cx, cy) from (cx1, cy1)
  sx2 := (x0 + X2) / 2.0;
  sy2 := (y0 + Y2) / 2.0;
  cx := sx2 + (cos_a * cx1 - sin_a * cy1);
  cy := sy2 + (sin_a * cx1 + cos_a * cy1);

  // Calculate the start_angle (angle1) and the sweep_angle (dangle)
  ux := (X1 - cx1) / rx;
  uy := (Y1 - cy1) / ry;
  vx := (-X1 - cx1) / rx;
  vy := (-Y1 - cy1) / ry;

  // Calculate the angle start
  N := Sqrt(ux * ux + uy * uy);
  P := ux; // (1 * ux ) + (0 * uy )

  if uy < 0 then
    sign := -1.0
  else
    sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  start_angle := sign * ArcCos(V);

  // Calculate the sweep angle
  N := Sqrt((ux * ux + uy * uy) * (vx * vx + vy * vy));
  P := ux * vx + uy * vy;

  if ux * vy - uy * vx < 0 then
    sign := -1.0
  else
    sign := 1.0;

  V := P / N;

  if V < -1.0 then
    V := -1.0;

  if V > 1.0 then
    V := 1.0;

  sweep_angle := sign * ArcCos(V);

  if (not SweepFlag) and (sweep_angle > 0) then
    sweep_angle := sweep_angle - Pi * 2.0
  else if SweepFlag and (sweep_angle < 0) then
    sweep_angle := sweep_angle + Pi * 2.0;

  len := TempCount;
  AddArcSvgPart(PointF(0, 0), PointF(rx, ry), RadToDeg(start_angle), RadToDeg(sweep_angle));

  tm := IdentityMatrix;
  tm.m31 := cx;
  tm.m32 := cy;
  tm := MatrixMultiply(CreateRotationMatrix(Angle), tm);

  I := len;
  while I < TempCount do
  begin
    FTempPathData[I].Point := tm.Transform(Vector(FTempPathData[I].Point)).ToPointF;
    Inc(I);
  end;
end;

function TSGVPathData.GetTok(const S: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  try
    Result := StringBuilder.ToString;
    if Pos > Length(S) then
      Exit;
    while (Pos < S.Length) and CharInSet(S.Chars[Pos],[' ']) do
      Inc(Pos);

    while Pos < S.Length do
    begin
      if not 'zmlchvsqtaZMLCHVSQTA'.Contains(S.Chars[Pos]) then
        Break;
      StringBuilder.Append(S.Chars[Pos]);
      Inc(Pos);
    end;
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

function TSGVPathData.GetNum(const S: string; var Pos: Integer): string;
var
  StringBuilder: TStringBuilder;
begin
  StringBuilder := TStringBuilder.Create;
  try
    Result := StringBuilder.ToString;
    if Pos > S.Length - 1 then
      Exit;
    while (Pos < S.Length) and CharInSet(S.Chars[Pos],[' ']) do
      Inc(Pos);
    while Pos < S.Length do
    begin
      if (S.Chars[Pos] = 'e') then
      begin
        StringBuilder.Append(S.Chars[Pos]);
        Inc(Pos);
        Continue;
      end;
      if (S.Chars[Pos] = '-') and (Result.Length > 0) and (Result.Chars[Result.Length - 1] = 'e') then
      begin
        StringBuilder.Append(S.Chars[Pos]);
        Inc(Pos);
        Continue;
      end;
      if (Result <> '') and (S.Chars[Pos] = '-') then
        Break;
     if not '0123456789.'.Contains(S.Chars[Pos]) and not((Pos = Pos) and (S.Chars[Pos] = '-')) then
        Break;
      StringBuilder.Append(S.Chars[Pos]);
      Inc(Pos);
    end;
    while CharInSet(S.Chars[Pos],[' ']) do
      Inc(Pos);
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

function TSGVPathData.GetPointFromStr(const S: string; var Pos: Integer): TPointF;
var
  X, Y: string;

  dummy : integer;
begin
  Result := PointF(0, 0);
  if Pos > S.Length then
    Exit;
  while (Pos < S.Length) and CharInSet(S.Chars[Pos], [',', ' ']) do
    Inc(Pos);
  X := GetNum(S, Pos);
  while (Pos < S.Length) and CharInSet(S.Chars[Pos], [',', ' ']) do
    Inc(Pos);
  Y := GetNum(S, Pos);
  while (Pos < S.Length) and CharInSet(S.Chars[Pos], [',', ' ']) do
    Inc(Pos);

  if (X = '-') or (Y = '-') then
    dummy := 1;



  Result := PointF(StrToFloat(X, USFormatSettings), StrToFloat(Y, USFormatSettings));
end;

procedure TSGVPathData.ParseSGVPathString(const Value: string);
var
  PathString, toks: string;
  Token: Char;
  R, CP1, CP2: TPointF;
  Angle: Single;
  large, sweet: Boolean;
  Pos, I: Integer;
  PointFTmp: TPointF;
  Builder: TStringBuilder;
  TokenBuilder: TStringBuilder;
  LastLength: Integer;
begin
  Builder := TStringBuilder.Create;
  TokenBuilder := TStringBuilder.Create;
  try
    { change every #10#13 to space }
    for I := 0 to Value.Length - 1 do
    begin
      if CharInSet(Value.Chars[I], [#9, #10, #13]) then
        Builder.Append(' ')
      else
        Builder.Append(Value.Chars[I]);
    end;
    PathString := Builder.ToString;

    { }
    SetLength(FTempPathData, 0);
    Pos := 0;
    LastLength := -1;
    while (Builder.Length > Pos) and (LastLength <> Pos) do
    begin
      LastLength := Pos;
      toks := GetTok(PathString, Pos);
      //TokenBuilder.Clear;
      //TokenBuilder.Append(toks);
      if (toks = '') and (Pos < PathString.Length) and (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) then begin
        // Repeat last token
        toks := Token;
      end;
      TokenBuilder.Clear;
      TokenBuilder.Append(toks);


      while TokenBuilder.Length > 0 do
      begin
        Token := TokenBuilder.Chars[0];
        TokenBuilder.Remove(0, 1);
        try
          if CharInSet(Token, ['z', 'Z']) then
          begin
            ClosePath;
          end;

          if CharInSet(Token, ['M']) then
          begin
            MoveTo(GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineTo(GetPointFromStr(PathString, Pos));
            end;
          end;
          if CharInSet(Token, ['m']) then
          begin
            MoveToRel(GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineToRel(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'L') then
          begin
            LineTo(GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineTo(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'l') then
          begin
            LineToRel(GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              LineToRel(GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'C') then
          begin
            CP1 := GetPointFromStr(PathString, Pos);
            CP2 := GetPointFromStr(PathString, Pos);
            CurveTo(CP1, CP2, GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP1 := GetPointFromStr(PathString, Pos);
              CP2 := GetPointFromStr(PathString, Pos);
              CurveTo(CP1, CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'c') then
          begin
            CP1 := GetPointFromStr(PathString, Pos);
            CP2 := GetPointFromStr(PathString, Pos);
            CurveToRel(CP1, CP2, GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP1 := GetPointFromStr(PathString, Pos);
              CP2 := GetPointFromStr(PathString, Pos);
              CurveToRel(CP1, CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'S') then
          begin
            CP2 := GetPointFromStr(PathString, Pos);
            SmoothCurveTo(CP2, GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP2 := GetPointFromStr(PathString, Pos);
              SmoothCurveTo(CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 's') then
          begin
            CP2 := GetPointFromStr(PathString, Pos);
            SmoothCurveToRel(CP2, GetPointFromStr(PathString, Pos));
            while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
            begin
              { next points }
              CP2 := GetPointFromStr(PathString, Pos);
              SmoothCurveToRel(CP2, GetPointFromStr(PathString, Pos));
            end;
          end;
          if (Token = 'H') then
          begin
            // skip horizontal line
            HLineTo(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'h') then
          begin
            // skip horizontal line
            HLineToRel(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'V') then
          begin
            // skip vertical line
            VLineTo(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'v') then
          begin
            // skip vertical line
            VLineToRel(StrToFloat(GetNum(PathString, Pos), USFormatSettings));
          end;
          if (Token = 'Q') then
          begin
            // skip quadratic bezier
            GetPointFromStr(PathString, Pos);
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'q') then
          begin
            // skip quadratic bezier
            GetPointFromStr(PathString, Pos);
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'T') then
          begin
            // skip show qudratic bezier
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 't') then
          begin
            // skip show qudratic bezier
            GetPointFromStr(PathString, Pos);
          end;
          if (Token = 'A') then
          begin
            // arc
            if TempCount > 0 then
              CP1 := FTempPathData[High(FTempPathData)].Point
            else
              CP1 := PointF(0, 0);
            R := GetPointFromStr(PathString, Pos);
            Angle := StrToFloat(GetNum(PathString, Pos), USFormatSettings);

            PointFTmp := GetPointFromStr(PathString, Pos);
            large := PointFTmp.X = 1;
            sweet := PointFTmp.Y = 1;
            CP2 := GetPointFromStr(PathString, Pos);
            AddArcSvg(CP1, R, Angle, large, sweet, CP2);
          end;
          if (Token = 'a') then
          begin
            // arc rel
            if TempCount > 0 then
              CP1 := FTempPathData[High(FTempPathData)].Point
            else
              CP1 := PointF(0, 0);
            R := GetPointFromStr(PathString, Pos);
            Angle := StrToFloat(GetNum(PathString, Pos), USFormatSettings);
            PointFTmp := GetPointFromStr(PathString, Pos);
            large := PointFTmp.X = 1;
            sweet := PointFTmp.Y = 1;
            CP2 := GetPointFromStr(PathString, Pos);
            CP2.X := CP1.X + CP2.X;
            CP2.Y := CP1.Y + CP2.Y;
            AddArcSvg(CP1, R, Angle, large, sweet, CP2);
          end;
        except
        end;
      end;
    end;
    ApplyPathData;
    FRecalcBounds := True;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    TokenBuilder.Free;
    Builder.Free;
  end;
end;

procedure TSGVPathData.ParseSGVPolygonString(const Value: string);
var
  PathString: string;
  R, CP1, CP2: TPointF;
  Pos, I: Integer;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    { change every #10#13 to space }
    for I := 0 to Value.Length - 1 do
    begin
      if CharInSet(Value.Chars[I], [#9, #10, #13]) then
        Builder.Append(' ')
      else
        Builder.Append(Value.Chars[I]);
    end;

    PathString := Builder.ToString;
    SetLength(FTempPathData, 0);
    Pos := 0;

    MoveTo(GetPointFromStr(PathString, Pos));
    while (IsDigit(PathString.Chars[Pos]) or (PathString.Chars[Pos] = '-')) do
    begin
      { next points }
      LineTo(GetPointFromStr(PathString, Pos));
    end;

    ApplyPathData;
    FRecalcBounds := True;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  finally
    Builder.Free;
  end;

end;

{ TSGVPath }

constructor TSGVPath.Create(AOwner: TComponent);
begin
  inherited;
  FWrapMode := TPathWrapMode.pwStretch;
  FData := TSGVPathData.Create;
  FData.OnChanged := DoChanged;
  FCurrent := TSGVPathData.Create;
end;

destructor TSGVPath.Destroy;
begin
  FreeAndNil(FCurrent);
  FreeAndNil(FData);
  inherited;
end;

procedure TSGVPath.UpdatePath;
var
  B: TRectF;
  P: TPathData;
begin
  { Update path }
  P := FData;
  if Assigned(FData.ResourcePath) then
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
          B.Fit(ShapeRect);
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
      FCurrent.Translate(Stroke.Thickness / 2, Stroke.Thickness / 2);
  end
  else
    FCurrent.Clear;
end;

procedure TSGVPath.DoChanged(Sender: TObject);
begin
  UpdatePath;
  Repaint;
end;

function TSGVPath.GetPath: TSGVPathData;
begin
  Result := FData;
end;

procedure TSGVPath.Loaded;
begin
  inherited;
  if not (csDestroying in ComponentState) then
    UpdatePath;
end;

function TSGVPath.PointInObject(X, Y: Single): Boolean;
begin
  if (csDesigning in ComponentState) and not FLocked and not FInPaintTo then
  begin
    Result := inherited PointInObject(X, Y);
  end
  else
  if Assigned(Canvas) and not (FCurrent.IsEmpty)
  and (FWrapMode <> TPathWrapMode.pwTile) then
  begin
    //Result := Canvas.PtInPath(AbsoluteToLocal(PointF(X, Y)), FCurrent)
    Result := False;
  end
  else
    Result := inherited PointInObject(X, Y);
end;

procedure TSGVPath.Resize;
begin
  inherited;
  UpdatePath;
end;

procedure TSGVPath.Paint;
var
  B, R: TRectF;
  i, j: Integer;
  a, t : single;
  State: TCanvasSaveState;
  P1: TSGVPathData;
  pa, pb : TPointF;
  Txt : string;

tw : single;
MSave: TMatrix;
M: TMatrix;
CenterPt: TPointF;

begin
  if not FCurrent.IsEmpty then
  begin
    case FWrapMode of
      TPathWrapMode.pwOriginal:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);
            Canvas.FillPath(FCurrent, AbsoluteOpacity, Fill);
            Canvas.DrawPath(FCurrent, AbsoluteOpacity, Stroke);
          finally
            Canvas.RestoreState(State);
          end;
        end;
      TPathWrapMode.pwFit:
        begin
          Canvas.FillPath(FCurrent, AbsoluteOpacity, Fill);
          Canvas.DrawPath(FCurrent, AbsoluteOpacity, Stroke);
        end;
      TPathWrapMode.pwStretch:
        begin
          Canvas.FillPath(FCurrent, AbsoluteOpacity, Fill);
          Canvas.DrawPath(FCurrent, AbsoluteOpacity, Stroke);
        end;
      TPathWrapMode.pwTile:
        begin
          State := Canvas.SaveState;
          try
            Canvas.IntersectClipRect(LocalRect);

            B := FCurrent.GetBounds;
            R := B;
            P1 := TSGVPathData.Create;
            try
              for i := 0 to Round(Width / RectWidth(R)) do
                for j := 0 to Round(Height / RectHeight(R)) do
                begin
                  P1.Assign(FCurrent);
                  P1.Translate(ShapeRect.Left + i * (RectWidth(R) + ShapeRect.Left * 2),
                    ShapeRect.Top + j * (RectHeight(R) + ShapeRect.Top * 2));
                  Canvas.FillPath(P1, AbsoluteOpacity, Fill);
                  Canvas.DrawPath(P1, AbsoluteOpacity, Stroke);
                end;
            finally
              if Assigned(P1) then
                P1.Free;
            end;
          finally
            Canvas.RestoreState(State);
          end;
        end;
    end;

    {FCurrent.CalcArcLengths(6);
    t := 0;
    for i := 0 to 19 do begin
      MSave := Canvas.Matrix;
      try
        FCurrent.GetPointOnPath(t, pa, a);

        txt := IntToStr(i);
        R := R.Empty;
        Canvas.MeasureText( R, txt, False, [], TTextAlign.taCenter);

        R.Left := R.Left + pa.X;
        R.Right := R.Right + pa.X;
        R.Top := R.Top + pa.Y;
        R.Bottom := R.Bottom + pa.Y;

        M := Canvas.Matrix;
        CenterPt.X := (((R.Right - R.Left) / 2 + R.Left) * Canvas.Matrix.m11) + Canvas.Matrix.m31;
        CenterPt.Y := (((R.Bottom - R.Top) / 2 + R.Top) * Canvas.Matrix.m22) + Canvas.Matrix.m32;
        M := MatrixMultiply(M, CreateTranslateMatrix(-CenterPt.X, -CenterPt.Y));
        M := MatrixMultiply(M, CreateRotationMatrix( a));
        M := MatrixMultiply(M, CreateTranslateMatrix(CenterPt.X, CenterPt.Y));
        Canvas.SetMatrix(M);
        Canvas.FillText( R, txt, False, 1, [], TTextAlign.taCenter);

      finally
        Canvas.SetMatrix( MSave);
      end;

      R.Left := pa.X - 1;
      R.Right := pa.X + 1;
      R.Top := pa.Y - 1;
      R.Bottom := pa.Y + 1;

      Canvas.DrawRect(R, 1, 1, [], 1);

      t := t + 1/20;
    end;}

  end;
  if (csDesigning in ComponentState) and not FLocked and not FInPaintTo then
  begin
    R := LocalRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.DrawDashRect(R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090);
  end;
end;

procedure TSGVPath.SetWrapMode(const Value: TPathWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    UpdatePath;
    Repaint;
  end;
end;

procedure TSGVPath.SetPathData(const Value: TSGVPathData);
begin
  //FData.Assign(Value);

  FreeAndNil(FData);
  FCurrent := Value;
  //UpdatePath;
end;



end.
