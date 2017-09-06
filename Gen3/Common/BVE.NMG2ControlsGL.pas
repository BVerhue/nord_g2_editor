unit BVE.NMG2ControlsGL;

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

// Base GPU controls

interface
uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.UIConsts,
  System.UITypes,
  System.RTLConsts,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Platform,
  FMX.Styles,
  FMX.Materials,
  FMX.MaterialSources,
  FMX.Layouts,
  FMX.Types3D,
  FMX.Layers3D,
  FMX.Controls3D,
  FMX.Textlayout,
  BVE.NMG2TexturesGL,
  BVE.NMG2CablePhysics;

type
  TControlGL = class;

  TGLClkEvent = procedure(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer) of object;
  TGLTexCreateEvent = procedure(Sender: TObject; var aTiledTexture: TTiledTexture) of object;

  /// <summary>A GPU scrollable viewport that can is a container of GPU objects.</summary>
  TViewPortGL = class(TScrollBox)
  private
    FTimer: TTimer;

    FContext: TContext3D;
    FTexture: TTexture;
    FMultisample: TMultisample;

    FUpdateRectList: TList<TRectF>;
    FNeedsRepaint: boolean;
    FNeedsFullRepaint: boolean;

    FMouseDownShift: TShiftState;
    FMouseDownPoint,
    FMouseDelta: TPoint3D;

    FLongClkTimer: TTimer;

    FGestureLocation: TPointF;
    FGestureDistance: single;

    FMouseOverCtrl,
    FMouseDownCtrl: TControlGL;

    FCanTranslate: boolean;

    FLayout: TLayout;

    FTranslation3D: TPoint3D;
    FScale3D: TPoint3D;
    FViewRect2D: TRectF;
    FMatrix3D,
    FInvMatrix3D: TMatrix3D;

    FColor: TAlphaColor;

    procedure DoTimer(Sender: TObject);
    function GetLocalMouseDownPoint: TPoint3D;
    function GetLayoutRect: TRectF;
    procedure SetLayoutRect(const Value: TRectF);
    procedure SetColor(const Value: TAlphaColor);
    procedure LongClick(Sender: TObject);
  protected
    function GetViewportScale: Single; virtual;
    procedure SetTranslation3D(const Value: TPoint3D); virtual;
    procedure SetScale3D(const Value: TPoint3D);
    procedure SetMatrix3D(const Value: TMatrix3D); virtual;
    procedure SetViewRect2D(const Value: TRectF);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;

    procedure CMGesture(var EventInfo: TGestureEventInfo); override;
    procedure DoGesture(const EventInfo: TGestureEventInfo; var Handled: Boolean); override;
    procedure DoLongClk(Shift: TShiftState; X, Y: Single); virtual;

    procedure RecreateContext; virtual;

    procedure Resize; override;
    procedure ViewportPositionChange(const OldViewportPosition, NewViewportPosition: TPointF;
                                     const ContentSizeChanged: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ControlAt(const aX, aY: single): TControlGL; virtual;

    function CalcLayoutRect: TRectF; virtual;

    procedure Invalidate;
    procedure InvalidateChildren;
    procedure FullRepaint;

    procedure UpdateViewRect2D;
    procedure UpdateMatrix3D;

    procedure Paint; override;
    procedure Render(aBitmap: TBitmap); virtual;

    property Context: TContext3D read FContext;
    property UpdateRectList: TList<TRectF> read FUpdateRectList;
    property ViewRect2D: TRectF read FViewRect2D write SetViewRect2D;
    property LayoutRect: TRectF read GetLayoutRect write SetLayoutRect;
    property Translation3D: TPoint3D read FTranslation3D write SetTranslation3D;
    property Matrix3D: TMatrix3D read FMatrix3D write SetMatrix3D;
    property InvMatrix3D: TMatrix3D read FInvMatrix3D;
    property Scale3D: TPoint3D read FScale3D write SetScale3D;
    property MouseDownCtrl: TControlGL read FMouseDownCtrl;
    property MouseOverCtrl: TControlGL read FMouseOverCtrl;
    property MouseDownPoint: TPoint3D read FMouseDownPoint;
    property LocalMouseDownPoint: TPoint3D read GetLocalMouseDownPoint;
    property MouseDelta: TPoint3D read FMouseDelta;
    property CanTranslate: boolean read FCanTranslate write FCanTranslate;
    property Color: TAlphaColor read FColor write SetColor;
  end;

  /// <summary>A class that can buffer the GPU viewport to a bitmap.</summary>
  TBufferedViewPortGL = class(TViewPortGL)
  private
    FBitmap: TBitmap;
  protected
    procedure SetVisible(const aValue: boolean); override;
    procedure RecreateContext; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;
  end;

  /// <summary>Base class for GPU controls.</summary>
  TControlGL = class(TFMXObject)
  private
    FAutoCapture: boolean;
    FProjection: TProjection;
    FPosition: TPosition3D;
    FVisible: boolean;
    FHitTest: boolean;
    FAbsoluteOpacity: single;
    FNeedRepaint: boolean;
    FChildNeedsRepaint: boolean;
    FMatrix: TMatrix3D;

    FMouseDownPoint: TPointF;
    FMouseDownShift: TShiftState;

    FOnClk: TGLClkEvent;
    FOnLongClk: TGLClkEvent;

    procedure SetAutoCapture(const Value: boolean);
    procedure SetProjection(const Value: TProjection);
    procedure SetPosition(const Value: TPoint3D);
    procedure SetVisible(const Value: boolean);
    procedure SetAbsoluteOpacity(const Value: single);
    function GetBoundsRect: TRectF;
    function GetAbsBoundsRect: TRectF;
    function GetPosition: TPoint3D;
    function GetAbsScale3D: TPoint3D;
    procedure SetMatrix(const Value: TMatrix3D);
  protected
    procedure SetWidth(const Value: single); virtual;
    procedure SetHeight(const Value: single); virtual;
    function GetHeight: single; virtual;
    function GetWidth: single; virtual;

    procedure Resize3D; virtual;
    procedure DoClk(Shift: TShiftState; X, Y: Single); virtual;
    procedure DoLongClk(Shift: TShiftState; X, Y: Single); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate;

    function AbsToLocal(const aPoint: TPoint3D): TPoint3D;
    function LocalToAbs(const aPoint: TPoint3D): TPoint3D;

    function ControlAt(const aX, aY: single): TControlGL;

    procedure CalcMatrix; virtual;

    procedure MouseEnter; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseLeave; virtual;

    procedure Render(aContext: TContext3D; aUpdateRectList: TList<TRectF>);
    procedure RenderControl(aContext: TContext3D); virtual;
    procedure RenderChildren(aContext: TContext3D; aUpdateRectList: TList<TRectF>); virtual;

    property Projection: TProjection read FProjection write SetProjection;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property AbsScale3D: TPoint3D read GetAbsScale3D;
    property BoundsRect: TRectF read GetBoundsRect;
    property AbsBoundsRect: TRectF read GetAbsBoundsRect;
    property NeedRepaint: boolean read FNeedRepaint;
    property ChildNeedsRepaint: boolean read FChildNeedsRepaint;
    property Matrix: TMatrix3D read FMatrix write SetMatrix;
  published
    property AbsoluteOpacity: single read FAbsoluteOpacity write SetAbsoluteOpacity;
    property AutoCapture: boolean read FAutoCapture write SetAutoCapture;
    property Position: TPoint3D read GetPosition write SetPosition;
    property Visible: boolean read FVisible write SetVisible;
    property HitTest: boolean read FHitTest write FHitTest;

    property OnClk: TGLClkEvent read FOnClk write FOnClk;
    property OnLongClk: TGLClkEvent read FOnLongClk write FOnLongClk;
  end;

  THalfSpace = (hsNegative, hsOnPlane, hsPositive);

  TPlane3D = record
    a, b, c, d : single;
    constructor Create(aA, aB, aC, aD: single);
    procedure Normalize;
    function Distance(const aPoint: TPoint3D): single;
    function ClassifyPoint(const aPoint: TPoint3D): THalfSpace;
  end;

  TFrustum = class
  private
    FPlanes: TList<TPlane3D>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalcPlanes( aModelViewMatrix: TMatrix3D; const aNormalize: boolean);
    function InFrustum(const aControl: TControl3D): boolean;
  end;

  TMeshGL = class(TPersistent)
  private
    FVer: TVertexBuffer;
    FIdx: TIndexBuffer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property VertexBuffer: TVertexBuffer read FVer;
    property IndexBuffer: TIndexBuffer read FIdx;
  end;

  /// <summary>A grid mesh made out of 2 triangles per cell.</summary>
  TSegmentedMeshGL = class(TMeshGL)
  private
    FSegmentWidth,
    FSegmentHeight: single;
    FPadding: single;
    FColCount,
    FRowCount: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CreateMesh(const aSegmentWidth, aSegmentHeight, aPadding: single;
      const aColCount, aRowCount: integer);

    function ColFromX(X: Single): integer;
    function RowFromY(Y: Single): integer;
  end;

  /// <summary>A pie slice mesh.</summary>
  TArcMeshGL = class(TMeshGL)
  private
    FCenter: TPointF;
    FRadius: TPointF;
    FStartAngle: single;
    FSweepAngle: single;
    FAngleStep: single;
  public
    constructor Create(const aCenter, aRadius: TPointF;
      const aStartAngle, aSweepAngle: single); reintroduce;
    destructor Destroy; override;

    procedure CreateMesh;

    property Center: TPointF read FCenter write FCenter;
    property Radius: TPointF read FRadius write FRadius;
    property StartAngle: single read FStartAngle write FStartAngle;
    property SweepAngle: single read FSweepAngle write FSweepAngle;
  end;

  /// <summary>A rectangle outline mesh.</summary>
  TRectOutlineMeshGL = class(TMeshGL)
  private
    FWidth: single;
    FHeight: single;
    FThickness: single;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    procedure CreateMesh(const aWidth, aHeight, aThickness: single);

    property Wdith: single read FWidth write FWidth;
    property Height: single read FHeight write FHeight;
    property Thickness: single read FThickness write FThickness;
  end;

  /// <summary>An outline (rectangle) to indicate a selected control.</summary>
  TOutlineGL = class
  private
    FPosition: TPoint3D;
    FWidth, FHeight: single;
    FMat: TColorMaterial;
    FMesh: TSegmentedMeshGL;
    FMatrix: TMatrix3D;
  public
    constructor Create(aPosition: TPoint3D; aWidth, aHeight: single);
    destructor Destroy; override;

    procedure Render(aContext: TContext3D);
  end;

  /// <summary>An outline (rectangle) to indicate the position of a control.</summary>
  TCursorGL = class
  private
    FPosition: TPoint3D;
    FWidth, FHeight: single;
    FMat: TColorMaterial;
    FMesh: TRectOutlineMeshGL;
    FMatrix: TMatrix3D;
  public
    constructor Create(aPosition: TPoint3D; aWidth, aHeight: single);
    destructor Destroy; override;

    procedure Render(aContext: TContext3D);
  end;

  /// <summary>A label control that draws its text on the fly. This doesn't
  /// seem to work on android.</summary>
  TLabelStaticGL = class
  private
    FFont: TFont;
    FTexture: TTextureBitmap;
    FMat: TTextureMaterial;
    FMesh: TSegmentedMeshGL;
    FText: string;
    FColor: TAlphaColor;
    FTextAlign: TTextAlign;
    FWidth, FHeight: single;
    FNeedsRepaint: boolean;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetFont(const Value: TFont);
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetHeight(const Value: single);
    procedure SetWidth(const Value: single);
  public
    constructor Create; overload;
    constructor Create(aFontFamily: string; aFontSize: integer;
      aColor: TAlphaColor; aTextAlign: TTextAlign; aText: string;
      const aWidth, aHeight: single); overload;

    destructor Destroy; override;

    procedure Render(aContext: TContext3D);
    procedure RenderTexture;

    property Font: TFont read FFont write SetFont;
    property Text: string read FText write SetText;
    property Color: TAlphaColor read FColor write SetColor;
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property NeedsRepaint: boolean read FNeedsRepaint;
  end;

  /// <summary>A colored rectangular shaped object.</summary>
  TColorObject = class
  private
    FPosition: TPoint3D;
    FMesh: TSegmentedMeshGL;
    FColorMat: TColorMaterial;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure SetPosition(const Value: TPoint3D);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Render(aContext: TContext3D); virtual;

    property Color: TAlphaColor read GetColor write SetColor;
    property Mesh: TSegmentedMeshGL read FMesh;
    property Position: TPoint3D read FPosition write SetPosition;
  end;

  /// <summary>A rectangular shaped textured object.</summary>
  TTexObject = class(TColorObject)
  private
    FTexMat: TTextureMaterial;
    FTexCols: integer;
    FTexRows: integer;
    FTileWidth: single;
    FTileHeight: single;
    FTextureID: string;
    FTextureList: TTextureListGL;
    FImageIndex: integer;
    FOnTexCreate: TGLTexCreateEvent;
    procedure SetTextureID(const Value: string);
    procedure SetTextureList(const Value: TTextureListGL);
    function GetTileHeight: single;
    function GetTileWidth: single;
    procedure SetTexCols(const Value: integer);
    procedure SetTexRows(const Value: integer);
    function GetSubRect(const aIndex: integer): TRectF;
    procedure SetImageIndex(const Value: integer);
  protected
    function GetTexCols: integer; virtual;
    function GetTexRows: integer; virtual;
    procedure UpdateTexCoords; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTexture: TTiledTexture;
    function CreateTexture: TTiledTexture; virtual;

    procedure Render(aContext: TContext3D); override;

    property TextureID: string read FTextureID write SetTextureID;
    property TexCols: integer read GetTexCols write SetTexCols;
    property TexRows: integer read GetTexRows write SetTexRows;
    property TileWidth: single read GetTileWidth;
    property TileHeight: single read GetTileHeight;
    property TextureList: TTextureListGL read FTextureList write SetTextureList;
    property ImageIndex: integer read FImageIndex write SetImageIndex;

    property OnTexCreate: TGLTexCreateEvent read FOnTexCreate write FOnTexCreate;
  end;

  /// <summary>A grid object that can show different parts of a texture in each cell.</summary>
  TTexMatrixObject = class(TTexObject)
  private
    FRowCount: integer;
    FColCount: integer;
    FPadding: single;
    FCellIndexArray: array of integer; // Index in texture
    function GetCellIndex(const aIndex: integer): integer;
    procedure SetCellIndex(const aIndex, Value: integer);
    procedure SetPadding(const Value: single);
  protected
    function GetHeight: single;
    function GetWidth: single;

    procedure SetColCount(const Value: integer); virtual;
    procedure SetRowCount(const Value: integer); virtual;

    procedure UpdateCellIndex; virtual;
    procedure UpdateTexCoords; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Render(aContext: TContext3D); override;

    function IndexFromPoint(X, Y: Single): integer;

    property CellIndex[const aIndex: integer]: integer read GetCellIndex write SetCellIndex;
    property ColCount: integer read FColCount write SetColCount;
    property RowCount: integer read FRowCount write SetRowCount;
    property Padding: single read FPadding write SetPadding;
    property Height: single read GetHeight;
    property Width: single read GetWidth;
  end;

  /// <summary>An object for displaying glyphs in a grid.</summary>
  TMatrixTextObject = class
  private
    FMesh: TSegmentedMeshGL;
    FTextureList: TTextureListGL;
    FTextureID: string;
    FMat: TTextureMaterial;
    FWidth: single;
    FTextAlign: TTextAlign;
    FText: string;
    FHeight: single;
    FTexCols: integer;
    FTexRows: integer;
    procedure SetHeight(const Value: single);
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TTextAlign);
    procedure SetWidth(const Value: single);
    procedure SetTextureID(const Value: string);
    procedure SetTextureList(const Value: TTextureListGL);
    function GetTextHeight: single;
    function GetTextWidth: single;
  protected
    procedure CreateVertices; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTexture: TTiledTexture;
    function CreateTexture: TTiledTexture; virtual;

    procedure UpdateTexCoords; virtual;
    procedure Render(aContext: TContext3D);

    property Text: string read FText write SetText;
    property TextAlign: TTextAlign read FTextAlign write SetTextAlign;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property TextWidth: single read GetTextWidth;
    property TextHeight: single read GetTextHeight;
    property TextureID: string read FTextureID write SetTextureID;
    property TextureList: TTextureListGL read FTextureList write SetTextureList;
  end;

  /// <summary>Base class of controls made out of a single texture.</summary>
  TTexControlGL = class(TControlGL)
  private
    FID: integer;
    FTexObject: TTexObject;

    procedure SetImageIndex(const Value: integer);
    procedure SetTextureID(const Value: string);
    function GetTileHeight: single;
    function GetTileWidth: single;
    procedure SetTexCols(const Value: integer);
    procedure SetTexRows(const Value: integer);
    function GetTexCols: integer;
    function GetTexRows: integer;
    function GetTextureID: string;
    function GetImageIndex: integer;
    function GetTextureList: TTextureListGL;
    function GetColor: TAlphaColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure TexCreate(Sender: TObject; var aTiledTexture: TTiledTexture);
  protected
    function GetHeight: single; override;
    function GetWidth: single; override;

    procedure SetTextureList(const Value: TTextureListGL); virtual;

    procedure DoBitmapChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateTexObjects; virtual;

    function GetTexture: TTiledTexture;

    function CreateTexture: TTiledTexture; virtual;

    function FindControl(aID: integer): TTexControlGL;
    procedure RenderControl(aContext: TContext3D); override;

    property TileWidth: single read GetTileWidth;
    property TileHeight: single read GetTileHeight;
  published
    property ID: integer read FID write FID;
    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property TextureList: TTextureListGL read GetTextureList write SetTextureList;
    property TextureID: string read GetTextureID write SetTextureID;
    property TexCols: integer read GetTexCols write SetTexCols;
    property TexRows: integer read GetTexRows write SetTexRows;
    property Color: TAlphaColor read GetColor write SetColor;
  end;

  /// <summary>Base class of controls made out of a grid of textures.</summary>
  TTexMatrixControlGL = class(TTexControlGL)
  private
    function GetCellIndex(const aIndex: integer): integer;
    procedure SetCellIndex(const aIndex, Value: integer);
    procedure SetPadding(const Value: single);
    function GetColCount: integer;
    function GetPadding: single;
    function GetRowCount: integer;
    function GetTexMatrixObject: TTexMatrixObject;
  protected
    function GetHeight: single; override;
    function GetWidth: single; override;

    procedure SetColCount(const Value: integer); virtual;
    procedure SetRowCount(const Value: integer); virtual;

    procedure UpdateCellIndex; virtual;
    procedure DoClk(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateTexObjects; override;

    procedure RenderControl(aContext: TContext3D); override;

    function IndexFromPoint(X, Y: Single): integer;

    property CellIndex[const aIndex: integer]: integer read GetCellIndex write SetCellIndex;
    property TexMatrixObject: TTexMatrixObject read GetTexMatrixObject;
  published
    property ColCount: integer read GetColCount write SetColCount;
    property RowCount: integer read GetRowCount write SetRowCount;
    property Padding: single read GetPadding write SetPadding;
  end;

  TCableNode = class
    X0,
    Y0,
    X1,
    Y1: single;
    VX,
    VY: single;
  end;

  /// <summary>A cable shaped control.</summary>
  TCableGL = class(TControlGL)
  private
    FMat: TColorMaterial;
    FVer: TVertexBuffer;
    FIdx: TIndexBuffer;
    FP0, FP1: TPointF;
    FCableThickness: single;
    FMargin: single;
    FCableNodeList : TCableNodeList;
    FTimer: TTimer;
    procedure SetCableThickness(const Value: single);
    procedure SetP0(const Value: TPointF);
    procedure SetP1(const Value: TPointF);
    procedure SetColor(const Value: TAlphaColor);
    function GetColor: TAlphaColor;
  protected
    procedure DoIterateCable(Sender: TObject);
    procedure CreateVertices;
    procedure CalcVertices;
    procedure CalcSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RenderControl(aContext: TContext3D); override;

    procedure InitCable;
    procedure IterateCable;

    property Color: TAlphaColor read GetColor write SetColor;
    property CableThickness: single read FCableThickness write SetCableThickness;
    property P0: TPointF read FP0 write SetP0;
    property P1: TPointF read FP1 write SetP1;
  end;

var
  GLObjectCount: integer;

implementation
uses
  System.Math;

{ TMeshGL }

constructor TMeshGL.Create;
begin
  inherited;

  FVer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 0);
  FIdx := TIndexBuffer.Create(0);
end;

destructor TMeshGL.Destroy;
begin
  if assigned(FVer) then
    FVer.Free;

  if assigned(FIdx) then
    FIdx.Free;

  inherited;
end;

{ TSegmentedMesh }

constructor TSegmentedMeshGL.Create;
begin
  inherited;
end;

destructor TSegmentedMeshGL.Destroy;
begin
  inherited;
end;

function TSegmentedMeshGL.RowFromY(Y: Single): integer;
begin
  if (FSegmentWidth + FPadding) <> 0 then
    Result := Trunc((Y - FPadding) / (FSegmentHeight + FPadding))
  else
    Result := 0;
end;

function TSegmentedMeshGL.ColFromX(X: Single): integer;
begin
  if (FSegmentWidth + FPadding) <> 0 then
    Result := Trunc((X - FPadding) / (FSegmentWidth + FPadding))
  else
    Result := 0;
end;

procedure TSegmentedMeshGL.CreateMesh(const aSegmentWidth, aSegmentHeight,
  aPadding: single; const aColCount, aRowCount: integer);
var
  LocalRect: TRectF;
  Left, Top: single;
  i, j, idx: integer;
begin
  if (FColCount <> aColCount)
  or (FRowCount <> aRowCount)
  or (FPadding <> aPadding)
  or (FSegmentWidth <> aSegmentWidth)
  or (FSegmentHeight <> aSegmentHeight)
  then
  begin

    FColCount := aColCount;
    FRowCount := aRowCount;
    FPadding := aPadding;
    FSegmentWidth := aSegmentWidth;
    FSegmentHeight := aSegmentHeight;

    if assigned(FVer) then
      FVer.Free;

    if assigned(FIdx) then
      FIdx.Free;

    FVer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 4 * FColCount * FRowCount);
    FIdx := TIndexBuffer.Create(6 * FColCount * FRowCount);

    for i := 0 to FRowCount - 1 do begin
      for j := 0 to FColCount - 1 do begin

        Left := FPadding + j * (FSegmentWidth + FPadding);
        Top := FPadding + i * (FSegmentHeight + FPadding);

        LocalRect := RectF(
          Left,
          Top,
          Left + FSegmentWidth,
          Top + FSegmentHeight);

        idx := i * FColCount + j;

        FVer.Vertices[idx * 4 + 0] := TPoint3D.Create(LocalRect.Left, LocalRect.Top, 0);
        FVer.TexCoord0[idx * 4 +0] := TPointF.Create(0, 0);
        FVer.Vertices[idx * 4 + 1] := TPoint3D.Create(LocalRect.Left, LocalRect.Bottom, 0);
        FVer.TexCoord0[idx * 4 +1] := TPointF.Create(0, 1);
        FVer.Vertices[idx * 4 + 2] := TPoint3D.Create(LocalRect.Right, LocalRect.Top, 0);
        FVer.TexCoord0[idx * 4 +2] := TPointF.Create(1, 0);
        FVer.Vertices[idx * 4 + 3] := TPoint3D.Create(LocalRect.Right, LocalRect.Bottom, 0);
        FVer.TexCoord0[idx * 4 +3] := TPointF.Create(1, 1);
      end;
    end;

    for i := 0 to FRowCount - 1 do begin
      for j := 0 to FColCount - 1 do begin
        idx := i * FColCount + j;

        FIdx[0 + 6 * idx] := 0 + idx * 4;
        FIdx[1 + 6 * idx] := 2 + idx * 4;
        FIdx[2 + 6 * idx] := 1 + idx * 4;
        FIdx[3 + 6 * idx] := 1 + idx * 4;
        FIdx[4 + 6 * idx] := 2 + idx * 4;
        FIdx[5 + 6 * idx] := 3 + idx * 4;
      end;
    end;
  end;
end;

{ TArcMeshGL }

constructor TArcMeshGL.Create(const aCenter, aRadius: TPointF;
  const aStartAngle, aSweepAngle: single);
begin
  inherited Create;

  FCenter := aCenter;
  FRadius := aRadius;
  FStartAngle := aStartAngle;
  FSweepAngle := aSweepAngle;
  FAngleStep := PI / 100;
end;

destructor TArcMeshGL.Destroy;
begin
  inherited;
end;

procedure TArcMeshGL.CreateMesh;
var
  a, x, y: single;
  i, SegmentCount: integer;
begin

  SegmentCount := abs(Trunc(FSweepAngle / FAngleStep));

  if assigned(FVer) then
    FVer.Free;

  if assigned(FIdx) then
    FIdx.Free;

  // Add Center and closing segment
  FVer := TVertexBuffer.Create([TVertexFormat.Vertex], SegMentCount + 1 + 1 + 1);
  FIdx := TIndexBuffer.Create(3 * (SegMentCount + 1));

  FVer.Vertices[0] := TPoint3D.Create(
    FCenter.X,
    FCenter.Y,
    0);

  if FSweepAngle < 0 then
    FAngleStep := + PI / 100
  else
    FAngleStep := - PI / 100;

  a := FStartAngle;
  for i := 0 to SegmentCount do begin
    x := FRadius.X * sin(a) + FCenter.X;
    y := FRadius.Y * cos(a) + FCenter.Y;

    FVer.Vertices[i+1] := TPoint3D.Create(x, y, 0);

    a := a + FAngleStep;
  end;

  x := FRadius.X * sin(FStartAngle + FSweepAngle) + FCenter.X;
  y := FRadius.Y * cos(FStartAngle + FSweepAngle) + FCenter.Y;
  FVer.Vertices[SegmentCount+2] := TPoint3D.Create(x, y, 0);

  for i := 0 to SegmentCount do begin

    FIdx[i * 3 + 0] := 0;
    FIdx[i * 3 + 1] := i + 1;
    FIdx[i * 3 + 2] := i + 2;
  end;
end;


{ TRectOutlineMeshGL }

constructor TRectOutlineMeshGL.Create;
begin
  inherited Create;
end;

procedure TRectOutlineMeshGL.CreateMesh(const aWidth, aHeight,
  aThickness: single);
var
  w, h, t: Single;
begin
  if assigned(FVer) then
    FVer.Free;

  if assigned(FIdx) then
    FIdx.Free;

  FVer := TVertexBuffer.Create([TVertexFormat.Vertex], 16);
  FIdx := TIndexBuffer.Create(6 * 8);

  FWidth := aWidth;
  FHeight := aHeight;
  FThickness := aThickness;

  w := FWidth;
  h := FHeight;
  t := FThickness;

  FVer.Vertices[0] := TPoint3D.Create(-t, -t, 0);
  FVer.Vertices[1] := TPoint3D.Create(t, -t, 0);
  FVer.Vertices[2] := TPoint3D.Create(w - t, -t, 0);
  FVer.Vertices[3] := TPoint3D.Create(w + t, -t, 0);

  FVer.Vertices[4] := TPoint3D.Create(-t, t, 0);
  FVer.Vertices[5] := TPoint3D.Create(t, t, 0);
  FVer.Vertices[6] := TPoint3D.Create(w - t, t, 0);
  FVer.Vertices[7] := TPoint3D.Create(w + t, t, 0);

  FVer.Vertices[8] := TPoint3D.Create(-t, h - t, 0);
  FVer.Vertices[9] := TPoint3D.Create(t, h - t, 0);
  FVer.Vertices[10] := TPoint3D.Create(w - t, h - t, 0);
  FVer.Vertices[11] := TPoint3D.Create(w + t, h - t, 0);

  FVer.Vertices[12] := TPoint3D.Create(-t, h + t, 0);
  FVer.Vertices[13] := TPoint3D.Create(t, h + t, 0);
  FVer.Vertices[14] := TPoint3D.Create(w - t, h + t, 0);
  FVer.Vertices[15] := TPoint3D.Create(w + t, h + t, 0);


  FIdx[ 0] := 0;  FIdx[ 3] := 4;
  FIdx[ 1] := 1;  FIdx[ 4] := 1;
  FIdx[ 2] := 4;  FIdx[ 5] := 5;

  FIdx[ 6] := 1;  FIdx[ 9] := 5;
  FIdx[ 7] := 2;  FIdx[10] := 2;
  FIdx[ 8] := 5;  FIdx[11] := 6;

  FIdx[12] := 2;  FIdx[15] := 6;
  FIdx[13] := 3;  FIdx[16] := 3;
  FIdx[14] := 6;  FIdx[17] := 7;

  FIdx[18] := 4;  FIdx[21] := 8;
  FIdx[19] := 5;  FIdx[22] := 5;
  FIdx[20] := 8;  FIdx[23] := 9;

  FIdx[24] := 6;  FIdx[27] := 10;
  FIdx[25] := 7;  FIdx[28] := 7;
  FIdx[26] := 10; FIdx[29] := 11;

  FIdx[30] := 8;  FIdx[33] := 12;
  FIdx[31] := 9;  FIdx[34] := 9;
  FIdx[32] := 12; FIdx[35] := 13;

  FIdx[36] := 9;  FIdx[39] := 13;
  FIdx[37] := 10; FIdx[40] := 10;
  FIdx[38] := 13; FIdx[41] := 14;

  FIdx[42] := 10; FIdx[45] := 14;
  FIdx[43] := 11; FIdx[46] := 11;
  FIdx[44] := 14; FIdx[47] := 15;
end;

destructor TRectOutlineMeshGL.Destroy;
begin
  inherited;
end;

{ TPlane3D }

// http://gamedevs.org/uploads/fast-extraction-viewing-frustum-planes-from-world-view-projection-matrix.pdf

function TPlane3D.ClassifyPoint(const aPoint: TPoint3D): THalfSpace;
var l: single;
begin
  l :=
      a * aPoint.X
    + b * aPoint.Y
    + c * aPoint.Z
    + d;

  if l < 0 then
    result := hsNegative
  else
    if l > 0 then
      Result := hsPositive
    else
      Result := hsOnPlane;
end;

constructor TPlane3D.Create(aA, aB, aC, aD: single);
begin
  a := aA;
  b := aB;
  c := aC;
  d := aD;
end;

function TPlane3D.Distance(const aPoint: TPoint3D): single;
begin
  Result :=
      a * aPoint.X
    + b * aPoint.Y
    + c * aPoint.Z
    + d;
end;

procedure TPlane3D.Normalize;
var l: single;
begin
  l := sqrt(a * a + b * b + c * c);
  if l <> 0 then begin
    a := a / l;
    b := b / l;
    c := c / l;
    d := d / l;
  end;
end;

{ TFrustum }

// Not used

constructor TFrustum.Create;
var i : integer;
begin
  FPlanes := TList<TPlane3D>.Create;
  for i := 0 to 5 do
    FPlanes.Add(TPlane3D.Create(0,0,0,0))
end;

destructor TFrustum.Destroy;
begin
  FPlanes.Free;
  inherited;
end;

function TFrustum.InFrustum(const aControl: TControl3D): boolean;
var P0, P1, P2, P3: TPoint3D;
begin
  P0 := aControl.Position.Point;
  P1 := P0;
  P1.X := P1.X + aControl.Width;
  P2 := P1;
  P2.Y := P2.Y + aControl.Height;
  P3 := P0;
  P3.Y := P3.Y + aControl.Height;

  Result :=
    (   (FPlanes[0].ClassifyPoint(P0) = hsPositive)
     or (FPlanes[0].ClassifyPoint(P1) = hsPositive)
     or (FPlanes[0].ClassifyPoint(P2) = hsPositive)
     or( FPlanes[0].ClassifyPoint(P3) = hsPositive))
    and
    (   (FPlanes[1].ClassifyPoint(P0) = hsPositive)
     or (FPlanes[1].ClassifyPoint(P1) = hsPositive)
     or (FPlanes[1].ClassifyPoint(P2) = hsPositive)
     or( FPlanes[1].ClassifyPoint(P3) = hsPositive))
    and
    (   (FPlanes[2].ClassifyPoint(P0) = hsPositive)
     or (FPlanes[2].ClassifyPoint(P1) = hsPositive)
     or (FPlanes[2].ClassifyPoint(P2) = hsPositive)
     or( FPlanes[2].ClassifyPoint(P3) = hsPositive))
    and
    (   (FPlanes[3].ClassifyPoint(P0) = hsPositive)
     or (FPlanes[3].ClassifyPoint(P1) = hsPositive)
     or (FPlanes[3].ClassifyPoint(P2) = hsPositive)
     or( FPlanes[3].ClassifyPoint(P3) = hsPositive))
    ;

end;

procedure TFrustum.CalcPlanes(aModelViewMatrix: TMatrix3D;
  const aNormalize: boolean);
var P: TPlane3D;
begin
  // Left clipping plane
  P.a := aModelViewMatrix.m14 + aModelViewMatrix.m11;
  P.b := aModelViewMatrix.m24 + aModelViewMatrix.m21;
  P.c := aModelViewMatrix.m34 + aModelViewMatrix.m31;
  P.d := aModelViewMatrix.m44 + aModelViewMatrix.m41;
  FPlanes[0] := P;
  // Right clipping plane
  P.a := aModelViewMatrix.m14 - aModelViewMatrix.m11;
  P.b := aModelViewMatrix.m24 - aModelViewMatrix.m21;
  P.c := aModelViewMatrix.m34 - aModelViewMatrix.m31;
  P.d := aModelViewMatrix.m44 - aModelViewMatrix.m41;
  FPlanes[1] := P;
  // Top clipping plane
  P.a := aModelViewMatrix.m14 - aModelViewMatrix.m12;
  P.b := aModelViewMatrix.m24 - aModelViewMatrix.m22;
  P.c := aModelViewMatrix.m34 - aModelViewMatrix.m32;
  P.d := aModelViewMatrix.m44 - aModelViewMatrix.m42;
  FPlanes[2] := P;
  // Bottom clipping plane
  P.a := aModelViewMatrix.m14 + aModelViewMatrix.m12;
  P.b := aModelViewMatrix.m24 + aModelViewMatrix.m22;
  P.c := aModelViewMatrix.m34 + aModelViewMatrix.m32;
  P.d := aModelViewMatrix.m44 + aModelViewMatrix.m42;
  FPlanes[3] := P;
  // Near clipping plane
  P.a := aModelViewMatrix.m13;
  P.b := aModelViewMatrix.m23;
  P.c := aModelViewMatrix.m33;
  P.d := aModelViewMatrix.m43;
  FPlanes[4] := P;
  // Far clipping plane
  P.a := aModelViewMatrix.m14 - aModelViewMatrix.m13;
  P.b := aModelViewMatrix.m24 - aModelViewMatrix.m23;
  P.c := aModelViewMatrix.m34 - aModelViewMatrix.m33;
  P.d := aModelViewMatrix.m44 - aModelViewMatrix.m43;
  FPlanes[5] := P;
  // Normalize the plane equations, if requested
  if aNormalize then begin
    FPlanes[0].Normalize;
    FPlanes[1].Normalize;
    FPlanes[2].Normalize;
    FPlanes[3].Normalize;
    FPlanes[4].Normalize;
    FPlanes[5].Normalize;
  end;
end;

{ TViewPortGL }

constructor TViewPortGL.Create(AOwner: TComponent);
begin
  inherited;

  StyleLookup := 'scrollboxstyle'; {!}
  FColor := TAlphaColorRec.LightGray;

  FMultisample := TMultisample.FourSamples;
  FUpdateRectList := TList<TRectF>.Create;

  FMouseDownPoint := Point3D(0.0, 0.0, 0.0);
  FMouseDelta := Point3D(0.0, 0.0, 0.0);

  FGestureLocation := PointF(0.0, 0.0);
  FGestureDistance := 0;

  FMouseOverCtrl := nil;
  FMouseDownCtrl := nil;

  FCanTranslate := True;
  FTranslation3D := Point3D(0.0, 0.0, 0.0);
  FScale3D := Point3D(1.0, 1.0, 1.0);
  FMatrix3D := TMatrix3D.Identity;
  FInvMatrix3D := TMatrix3D.Identity;

  //Touch.InteractiveGestures := Touch.DefaultInteractiveGestures + [TInteractiveGesture.Zoom];
  Touch.InteractiveGestures := [];

  FNeedsRepaint := False;
  FNeedsFullRepaint := True;

  FLayout := TLayout.Create(self);
  FLayout.Parent := self;
  AddObject(FLayout);
  FLayout.Position.Point := PointF(0,0);
  FLayout.Width := 5000;
  FLayout.Height := 2500;

  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := 25;
  FTimer.OnTimer := DoTimer;

  FLongClkTimer := TTimer.Create(nil);
  FLongClkTimer.Enabled := False;
  FLongClkTimer.Interval := 500;
  FLongClkTimer.OnTimer := LongClick;
end;

destructor TViewPortGL.Destroy;
begin
  FLongClkTimer.Free;
  FTimer.Enabled := False;

  FUpdateRectList.Free;

  FreeAndNil(FContext);
  FreeAndNil(FTexture);

  inherited;
end;

procedure TViewPortGL.DoTimer(Sender: TObject);
begin
  if FNeedsRepaint then
  begin
    RePaint;
  end;
end;

procedure TViewPortGL.FullRepaint;
begin
  InvalidateChildren;
  Invalidate;
  FNeedsFullRepaint := True;
end;

function TViewPortGL.GetLayoutRect: TRectF;
begin
  Result := FLayout.BoundsRect;
end;

function TViewPortGL.GetLocalMouseDownPoint: TPoint3D;
begin
  Result := FMouseDownPoint * FInvMatrix3D;
end;

function TViewPortGL.GetViewportScale: Single;
begin
  Result := 1.0;
end;

procedure TViewPortGL.Invalidate;
begin
  FNeedsRepaint := True;
end;

procedure TViewPortGL.InvalidateChildren;
var
  FMXObject: TFMXObject;
begin
  if assigned(Content) and assigned(Content.Children) then
  begin
    for FMXObject in Content.Children do
      if FMXObject is TControlGL then
        (FMXObject as TControlGL).Invalidate;
  end;
end;

procedure TViewPortGL.LongClick(Sender: TObject);
begin
  FLongCLkTimer.Enabled := False;
  DoLongClk(FMouseDownShift, FMouseDownPoint.X, FMouseDownPoint.Y);
end;

function TViewPortGL.CalcLayoutRect: TRectF;
var
  FMXObject: TFMXObject;
  ControlGL: TControlGL;
begin
  Result := TRectF.Empty;

  if assigned(Content) and assigned(Content.Children) then
  begin
    for FMXObject in Content.Children do
      if FMXObject is TControlGL then
      begin
        ControlGL := FMXObject as TControlGL;
        Result := TRectF.Union(Result, ControlGL.BoundsRect);
      end;
  end;
end;

procedure TViewPortGL.CMGesture(var EventInfo: TGestureEventInfo);
begin
  if not assigned(FMouseDownCtrl) then
    inherited;
end;

procedure TViewPortGL.DoGesture(const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
var
  d, l: single;
  NewTranslate: TPoint3D;
  NewScale: TPoint3D;
begin
  if (not HitTest) or (Not Visible) then
    exit;

  FLongClkTimer.Enabled := False;

  if EventInfo.GestureID = igiZoom then begin

    if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags))
      and (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then begin

      d := (EventInfo.Distance - FGestureDistance);

      //Text1.Text := Format('%0:3.1f %1:3.1f %2:3d', [ vpx, vpy, EventInfo.Distance]);
      l := sqrt(Width * Width + Height * Height);

      NewScale.X := FScale3D.X + d / l;
      NewScale.Y := FScale3D.Y + d / l;
      NewScale.Z := FScale3D.Z + d / l;

      NewTranslate.X := FTranslation3D.X - d /2;
      NewTranslate.Y := FTranslation3D.Y - d /2;
      NewTranslate.Z := FTranslation3D.Z - d /2;

      Scale3D := NewScale;
      Translation3D := NewTranslate;
    end;
    FGestureLocation := EventInfo.Location;
    FGestureDistance := EventInfo.Distance;
    Handled := True;
  end;

  {if EventInfo.GestureID = igiPan then
  begin
    if (not(TInteractiveGestureFlag.gfBegin in EventInfo.Flags))
      and (not(TInteractiveGestureFlag.gfEnd in EventInfo.Flags)) then
    begin

      NewTranslate.X := Translation3D.X + (EventInfo.Location.X - FGestureLocation.X) /2;
      NewTranslate.Y := Translation3D.Y + (EventInfo.Location.Y - FGestureLocation.Y) /2;
      NewTranslate.Z := 0;

      Translation3D := NewTranslate;
    end;
    FGestureLocation := EventInfo.Location;
    Handled := True;
  end;}
end;

procedure TViewPortGL.DoLongClk(Shift: TShiftState; X, Y: Single);
begin
  if assigned(FMouseDownCtrl) then
    FMouseDownCtrl.DoLongClk(FMouseDownShift, FMouseDownPoint.X, FMouseDownPoint.Y);
end;

procedure TViewPortGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPoint3D;
begin
  if (not HitTest) or (Not Visible) then
    exit;

  FLongClkTimer.Enabled := True;

  FMouseDownShift := Shift;
  FMouseDownCtrl := ControlAt(X, Y);
  FMouseDownPoint := Point3D(X, Y, 0.0);

  if assigned(FMouseDownCtrl) then
  begin
    P := FMouseDownCtrl.AbsToLocal(FMouseDownPoint * FInvMatrix3D);
    FMouseDownCtrl.MouseDown(Button, Shift, P.X, P.Y);
  end else
    inherited;
end;

procedure TViewPortGL.MouseMove(Shift: TShiftState; X, Y: Single);
var
  P: TPoint3D;
  NewMouseOverCtrl: TControlGL;
  NewMouseDownPoint: TPoint3D;
begin
  if (not HitTest) or (Not Visible) then
    exit;

  FLongClkTimer.Enabled := False;

  NewMouseDownPoint := Point3D(X, Y, 0.0);
  FMouseDelta := (NewMouseDownPoint * FInvMatrix3D) - (FMouseDownPoint * FInvMatrix3D);
  FMouseDownPoint := NewMouseDownPoint;

  NewMouseOverCtrl := ControlAt(X, Y);
  if NewMouseOverCtrl <> FMouseOverCtrl then
  begin
    if assigned(FMouseOverCtrl) then
      FMouseOverCtrl.MouseLeave;

    FMouseOverCtrl := NewMouseOverCtrl;

    if assigned(FMouseOverCtrl) then
      FMouseOverCtrl.MouseEnter;
  end;

  if assigned(FMouseDownCtrl) then
  begin
    if not FMouseDownCtrl.AutoCapture then
    begin
      if FMouseOverCtrl <> FMouseDownCtrl then
        FMouseDownCtrl := nil;
    end;

    if assigned(FMouseDownCtrl) then
    begin
      P := FMouseDownCtrl.AbsToLocal(FMouseDownPoint * FInvMatrix3D);
      FMouseDownCtrl.MouseMove(Shift, P.X, P.Y);
    end;
  end
  else
  {begin
    if ssLeft in Shift then
    begin
      if FCanTranslate then
      begin
        NewTranslation.X := Translation3D.X + FMouseDelta.X * FScale3D.X;
        NewTranslation.Y := Translation3D.Y + FMouseDelta.Y * FScale3D.Y;
        Translation3D := NewTranslation;
      end;
    end;
  end;}
    if ssLeft in Shift then
      inherited;

  if assigned(FMouseOverCtrl) then
  begin
    if not(assigned(FMouseDownCtrl) and (FMouseDownCtrl.AutoCapture)) then
    begin
      P := FMouseOverCtrl.AbsToLocal(NewMouseDownPoint * FInvMatrix3D);
      FMouseOverCtrl.MouseMove(Shift, P.X, P.Y);
    end;
  end;
end;

procedure TViewPortGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  P: TPoint3D;
begin
  if (not HitTest) or (Not Visible) then
    exit;

  FLongClkTimer.Enabled := False;

  if assigned(FMouseDownCtrl) then
  begin
    P := FMouseDownCtrl.AbsToLocal(FMouseDownPoint * FInvMatrix3D);

    FMouseDownCtrl.MouseUp(Button, Shift, P.X, P.Y);
    FMouseDownCtrl := nil;
  end else
    inherited;
end;

procedure TViewPortGL.Paint;
begin
  inherited;

  FUpdateRectList.Clear;
end;

function TViewPortGL.ControlAt(const aX, aY: single): TControlGL;
var
  FMXObject: TFMXObject;
  ControlGL: TControlGL;
  P: TPoint3D;
begin
  Result := nil;
  if assigned(Content) and assigned(Content.Children) then

    P := Point3D(aX, aY, 0.0) * InvMatrix3D;

    for FMXObject in Content.Children do
      if FMXObject is TControlGL then
      begin
        ControlGL := FMXObject as TControlGL;
        if ControlGL.Visible and ControlGL.HitTest then
        begin
          Result := ControlGL.ControlAt(p.X, p.Y);
          if Result <> nil then
            break;
        end;
      end;
end;

procedure TViewPortGL.RecreateContext;
begin
  FTimer.Enabled := False;

  FreeAndNil(FContext);
  FreeAndNil(FTexture);

  FTexture := TTexture.Create;
  FTexture.Style := [TTextureStyle.RenderTarget];
  FTexture.SetSize(Round(Width * GetViewportScale),
    Round(Height * GetViewportScale));
  ITextureAccess(FTexture).TextureScale := GetViewportScale;

  FContext := TContextManager.CreateFromTexture(FTexture, FMultisample, True);
  FContext.SetContextState(TContextState.cs2DScene);

  UpdateViewRect2D;
  FullRepaint;

  FTimer.Enabled := True;
end;

procedure TViewPortGL.Render(aBitmap: TBitmap);
var
  SaveMatrix, M: TMatrix3D;
  FMXObject: TFMXObject;
  ControlGL : TControlGL;
begin
  if not FNeedsRepaint then
    exit;

  if Context <> nil then
  begin
    if (not assigned(Content)) or (not assigned(Content.Children)) then
      exit;

    if Context.BeginScene then
    begin
      //aBitmap.Canvas.Flush;

      if FNeedsFullRepaint then
      begin
        Context.SetContextState(TContextState.csScissorOff);
        Context.Clear(
          [TClearTarget.Color, TClearTarget.Depth],
          FColor, 1.0, 0);
      end;

      SaveMatrix := Context.CurrentMatrix;
      try
        M := FMatrix3D * SaveMatrix;

        for FMXObject in Content.Children do
        begin
          if FMXObject is TControlGL then
          begin
            ControlGL := FMXObject as TControlGL;

            if ControlGL.Visible and (ControlGL.BoundsRect.IntersectsWith(ViewRect2D)) then
            begin

              if FNeedsFullRepaint then
                ControlGL.Invalidate;

              if ControlGL.NeedRepaint then
              begin
                Context.SetMatrix(TMatrix3D.CreateTranslation(ControlGL.Position) * M);
                ControlGL.Render(Context, UpdateRectList)
              end;
            end;
          end;
        end;

      finally
        Context.EndScene;
        Context.SetMatrix(SaveMatrix);
      end;
    end;

    aBitMap.Clear(FColor);
    Context.CopyToBitmap(aBitMap, Rect(0, 0, aBitmap.Width, aBitmap.Height));
  end;
  FNeedsRepaint := False;
  FNeedsFullRepaint := False;
end;

procedure TViewPortGL.Resize;
begin
  inherited;
  RecreateContext;
  InvalidateChildren;
end;

procedure TViewPortGL.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Invalidate;
  end;
end;

procedure TViewPortGL.SetLayoutRect(const Value: TRectF);
begin
  FLayout.BoundsRect := Value;
end;

procedure TViewPortGL.SetMatrix3D(const Value: TMatrix3D);
begin
  FMatrix3D := Value;
  FInvMatrix3D := FMatrix3D.Inverse;
  UpdateViewRect2D;
end;

procedure TViewPortGL.SetScale3D(const Value: TPoint3D);
begin
  FScale3D := Value;
  UpdateMatrix3D;
end;

procedure TViewPortGL.SetTranslation3D(const Value: TPoint3D);
begin
  FTranslation3D := Value;
  UpdateMatrix3D;
end;

procedure TViewPortGL.SetViewRect2D(const Value: TRectF);
begin
  FViewRect2D := Value;
  Invalidate;
  InvalidateChildren;
end;

procedure TViewPortGL.UpdateMatrix3D;
begin
  Matrix3D :=
      TMatrix3D.CreateScaling(FScale3D)
    * TMatrix3D.CreateTranslation(FTranslation3D);
end;

procedure TViewPortGL.UpdateViewRect2D;
var P1, P2: TPoint3D;
begin
  P1 := Point3D(0, 0, 0) * FInvMatrix3D;
  P2 := Point3D(Width, Height, 0) * FInvMatrix3D;

  ViewRect2D :=
    RectF(
      P1.X, P1.Y,
      P2.X, P2.Y);
end;

procedure TViewPortGL.ViewportPositionChange(const OldViewportPosition,
  NewViewportPosition: TPointF; const ContentSizeChanged: boolean);
begin
  inherited;

  FullRepaint;

  Translation3D := Point3D(
    -NewViewportPosition.X,
    -NewViewportPosition.Y,
    0.0);
end;

{ TBufferedViewPortGL }

constructor TBufferedViewPortGL.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TBufferedViewPortGL.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBufferedViewPortGL.Paint;
var
  R: TRectF;
begin
  inherited;

  if assigned(FBitmap) then
  begin
    Render(FBitmap);
    R := RectF(0, 0, FBitmap.Width, FBitmap.Height);
    Canvas.BeginScene;
    try
      Canvas.DrawBitmap(FBitmap, R, R, 1, True);
    finally
      Canvas.EndScene;
    end;
  end;
end;

procedure TBufferedViewPortGL.RecreateContext;
begin
  FreeAndNil(FBitmap);

  inherited;

  FBitmap := TBitmap.Create(Context.Width, Context.Height);
end;

procedure TBufferedViewPortGL.SetVisible(const aValue: boolean);
begin
  if aValue <> Visible then
  begin
    inherited;

    if not Visible then begin
      FreeAndNil(FBitmap)
    end else begin
      RecreateContext;
      FullRepaint;
    end;
  end;
end;

{ TControlGL }

procedure TControlGL.CalcMatrix;
begin
  Matrix := TMatrix3D.CreateTranslation(FPosition.Point);
end;

function TControlGL.ControlAt(const aX, aY: single): TControlGL;
var
  FMXObject: TFMXObject;
  ControlGL: TControlGL;
  CP: TPointF;
begin
  if assigned(Children) then
  begin
    CP := PointF(
      aX - Position.X,
      aY - Position.Y);

    for FMXObject in Children do begin
      ControlGL := FMXObject as TControlGL;
      if ControlGL.Visible and ControlGL.HitTest then
      begin
        Result := ControlGL.ControlAt(CP.X, CP.Y);
        if Result <> nil then
          exit;
      end;
    end;
  end;

  if PtInRect(BoundsRect, PointF(aX, aY)) then
    Result := Self
  else
    Result := nil;
end;

constructor TControlGL.Create(AOwner: TComponent);
begin
  inherited;

  FPosition := TPosition3D.Create(Point3D(0.0,0.0,0.0));;
  FAbsoluteOpacity := 1.0;
  FVisible := True;
  FHitTest := True;
  FAutoCapture := False;
  FProjection := TProjection.Camera;
  Matrix := TMatrix3D.Identity;
end;

destructor TControlGL.Destroy;
begin
  FPosition.Free;
  inherited;
end;

procedure TControlGL.DoClk(Shift: TShiftState; X, Y: Single);
begin
  if assigned(FOnClk) then
    FOnClk(Self, FMouseDownShift, 0);
end;

procedure TControlGL.DoLongClk(Shift: TShiftState; X, Y: Single);
begin
  if assigned(FOnLongClk) then
    FOnLongClk(Self, FMouseDownShift, 0);
end;

function TControlGL.AbsToLocal(const aPoint: TPoint3D): TPoint3D;
var FMXObject: TFMXObject;
begin
  Result := aPoint;

  Result.X := Result.X - Position.X;
  Result.Y := Result.Y - Position.Y;
  Result.Z := Result.Z - Position.Z;

  FMXObject := Parent;
  while assigned(FMXObject) and (FMXObject is TControlGL) do
  begin
    Result.X := Result.X - (FMXObject as TControlGL).Position.X;
    Result.Y := Result.Y - (FMXObject as TControlGL).Position.Y;
    Result.Z := Result.Z - (FMXObject as TControlGL).Position.Z;

    FMXObject := FMXObject.Parent;
  end;
end;

function TControlGL.LocalToAbs(const aPoint: TPoint3D): TPoint3D;
var FMXObject: TFMXObject;
begin
  Result := aPoint;

  Result.X := Result.X + Position.X;
  Result.Y := Result.Y + Position.Y;
  Result.Z := Result.Z + Position.Z;

  FMXObject := Parent;
  while assigned(FMXObject) and (FMXObject is TControlGL) do
  begin
    Result.X := Result.X + (FMXObject as TControlGL).Position.X;
    Result.Y := Result.Y + (FMXObject as TControlGL).Position.Y;
    Result.Z := Result.Z + (FMXObject as TControlGL).Position.Z;

    FMXObject := FMXObject.Parent;
  end;
end;

function TControlGL.GetAbsBoundsRect: TRectF;

  procedure AddParentPos(aParent: TFMXObject);
  var
    ControlGL: TControlGL;
  begin
    if assigned(aParent) and (aParent is TControlGL) then begin
      ControlGL := aParent as TControlGL;
      Result.Left := Result.Left + ControlGL.Position.X;
      Result.Top := Result.Top + ControlGL.Position.Y;
      Result.Right := Result.Right + ControlGL.Position.X;
      Result.Bottom := Result.Bottom + ControlGL.Position.Y;
      AddParentPos(aParent.Parent);
    end;
  end;

begin
  Result := BoundsRect;
  AddParentPos(Parent);
end;

function TControlGL.GetAbsScale3D: TPoint3D;

  function ParentScale3D: TPoint3D;
  begin
    if assigned(Parent) then
    begin
      if Parent is TControlGL then
        Result := (Parent as TControlGL).GetAbsScale3D
      else
        if Parent is TViewPortGL then
          Result := (Parent as TViewPortGL).Scale3D
        else
          Result := Point3D(1.0, 1.0, 1.0);
    end
    else
      Result := Point3D(1.0, 1.0, 1.0);
  end;

begin
  Result := ParentScale3D;
end;

function TControlGL.GetBoundsRect: TRectF;
begin
  Result.Left := FPosition.X;
  Result.Top := FPosition.Y;
  Result.Right := FPosition.X + Width;
  Result.Bottom := FPosition.Y + Height;
end;

function TControlGL.GetHeight: single;
begin
  Result := 0;
end;

function TControlGL.GetPosition: TPoint3D;
begin
  Result := FPosition.Point;
end;

function TControlGL.GetWidth: single;
begin
  Result := 0;
end;

procedure TControlGL.Invalidate;

  procedure SetParentChildNeedRepaint(aParent: TFMXObject);
  begin
    if assigned(aParent) then begin

      if (aParent is TControlGL) then begin
        (aParent as TControlGL).FChildNeedsRepaint := True;
        SetParentChildNeedRepaint(aParent.Parent);
      end;

      if (aParent is TScrollContent) then
        if ((aParent as TScrollContent).ScrollBox is TViewPortGL) then
        begin
          ((aParent as TScrollContent).ScrollBox as TViewPortGL).Invalidate;
        end;
    end;
  end;

begin
  if not FNeedRepaint then
  begin
    FNeedRepaint := True;
    SetParentChildNeedRepaint(Parent);
  end;
end;

procedure TControlGL.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  //DoClk(Shift, X, Y);
  FMouseDownShift := Shift;
  FMouseDownPoint := PointF(X, Y);
end;

procedure TControlGL.MouseEnter;
begin
//
end;

procedure TControlGL.MouseLeave;
begin
end;

procedure TControlGL.MouseMove(Shift: TShiftState; X, Y: Single);
begin
end;

procedure TControlGL.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  DoClk(Shift, X, Y);
end;

procedure TControlGL.Render(aContext: TContext3D; aUpdateRectList: TList<TRectF>);
begin
  if FNeedRepaint then begin
    RenderControl(aContext);

    aUpdateRectList.Add(AbsBoundsRect);
  end;

  RenderChildren(aContext, aUpdateRectList);

  FNeedRepaint := False;
  FChildNeedsRepaint := False;
end;

procedure TControlGL.RenderChildren(aContext: TContext3D; aUpdateRectList: TList<TRectF>);
var FMXObject: TFMXObject;
    ControlGL: TControlGL;
    SaveMatrix: TMatrix3D;
begin
  if (not FNeedRepaint) and (not FChildNeedsRepaint) then
    exit;

  if not assigned(Children) then
    exit;

  SaveMatrix := aContext.CurrentMatrix;
  try
    for FMXObject in Children do begin
      if FMXObject is TControlGL then begin
        ControlGL := FMXObject as TControlGL;
        if ControlGl.Visible then
          if FNeedRepaint or (ControlGL.NeedRepaint) or (ControlGL.ChildNeedsRepaint) then begin

            if FNeedRepaint then
              ControlGL.Invalidate;

            aContext.SetMatrix(ControlGL.Matrix * SaveMatrix);

            ControlGL.Render(aContext, aUpdateRectList);
          end;
      end;
    end;
  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

procedure TControlGL.RenderControl(aContext: TContext3D);
begin
  //
end;

procedure TControlGL.Resize3D;
begin
  //
end;

procedure TControlGL.SetAbsoluteOpacity(const Value: single);
begin
  FAbsoluteOpacity := Value;
end;

procedure TControlGL.SetAutoCapture(const Value: boolean);
begin
  FAutoCapture := Value;
end;

procedure TControlGL.SetHeight(const Value: single);
begin
//
end;

procedure TControlGL.SetMatrix(const Value: TMatrix3D);
begin
  FMatrix := Value;
  Invalidate;
end;

procedure TControlGL.SetPosition(const Value: TPoint3D);
begin
  FPosition.Point := Value;
  CalcMatrix;
end;

procedure TControlGL.SetProjection(const Value: TProjection);
begin
  FProjection := Value;
end;

procedure TControlGL.SetVisible(const Value: boolean);
begin
  FVisible := Value;
end;

procedure TControlGL.SetWidth(const Value: single);
begin
//
end;

{ TTexControlGL }

constructor TTexControlGL.Create(AOwner: TComponent);
begin
  inherited;

  CreateTexObjects;
  FTexObject.OnTexCreate := TexCreate;
end;

procedure TTexControlGL.CreateTexObjects;
begin
  FTexObject := TTexObject.Create;
end;

function TTexControlGL.CreateTexture: TTiledTexture;
begin
  Result := nil;
end;

destructor TTexControlGL.Destroy;
begin
  FTexObject.Free;
  inherited;
end;

procedure TTexControlGL.DoBitmapChanged(Sender: TObject);
begin
  //
end;

function TTexControlGL.FindControl(aID: integer): TTexControlGL;
var Control: TFMXObject;
begin
  Result := nil;
  for Control in Children do
    if Control is TTexControlGL then
      if (Control as TTexControlGL).ID = aID then begin
        Result := Control as TTexControlGL;
        exit;
      end;
end;

function TTexControlGL.GetColor: TAlphaColor;
begin
  Result := FTexObject.Color;
end;

function TTexControlGL.GetHeight: single;
begin
  Result := TileHeight;
end;

function TTexControlGL.GetImageIndex: integer;
begin
  Result := FTexObject.ImageIndex;
end;

function TTexControlGL.GetTexCols: integer;
begin
  Result := FTexObject.TexCols;
end;

function TTexControlGL.GetTexRows: integer;
begin
  Result := FTexObject.TexRows;
end;

function TTexControlGL.GetTexture: TTiledTexture;
begin
  Result := FTexObject.GetTexture;
end;

function TTexControlGL.GetTextureID: string;
begin
  Result := FTexObject.TextureID;
end;

function TTexControlGL.GetTextureList: TTextureListGL;
begin
  Result := FTexObject.TextureList;
end;

function TTexControlGL.GetTileHeight: single;
begin
  Result := FTexObject.TileHeight;
end;

function TTexControlGL.GetTileWidth: single;
begin
  Result := FTexObject.TileWidth;
end;

function TTexControlGL.GetWidth: single;
begin
  Result := TileWidth;
end;

procedure TTexControlGL.RenderControl(aContext: TContext3D);
begin
  inc(GLObjectCount);
  FTexObject.Render(aContext);
end;

procedure TTexControlGL.SetColor(const Value: TAlphaColor);
begin
  FTexObject.Color := Value;
  Invalidate;
end;

procedure TTexControlGL.SetImageIndex(const Value: integer);
begin
  FTexObject.ImageIndex := Value;
  Invalidate;
end;

procedure TTexControlGL.SetTexCols(const Value: integer);
begin
  FTexObject.TexCols := Value;
  Invalidate;
end;

procedure TTexControlGL.SetTexRows(const Value: integer);
begin
  FTexObject.TexRows := Value;
  Invalidate;
end;

procedure TTexControlGL.SetTextureID(const Value: string);
begin
  FTexObject.TextureID := Value;
  Invalidate;
end;

procedure TTexControlGL.SetTextureList(const Value: TTextureListGL);
begin
  FTexObject.TextureList := Value;
  Invalidate;
end;

procedure TTexControlGL.TexCreate(Sender: TObject;
  var aTiledTexture: TTiledTexture);
begin
  aTiledTexture := CreateTexture;
end;

{ TCableGL }

constructor TCableGL.Create(AOwner: TComponent);
begin
  inherited;

  //Projection := TProjection.pjScreen;

  FMat := TColorMaterial.Create;
  FMat.Color := TAlphaColorRec.Green;

  FCableThickness := 2.5;
  FMargin := 0;

  FCableNodeList := nil;

  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.Interval := 10;
  FTimer.OnTimer := DoIterateCable;

  //HitTest := False;
end;

destructor TCableGL.Destroy;
begin
  FTimer.Free;

  if assigned(FVer) then
    FVer.Free;

  if assigned(FIdx) then
    FIdx.Free;

  FMat.Free;

  if assigned(FCableNodeList) then
    FCableNodeList.Free;
  inherited;
end;

procedure TCableGL.DoIterateCable(Sender: TObject);
begin
  IterateCable;
end;

function TCableGL.GetColor: TAlphaColor;
begin
  Result := FMat.Color;
end;

procedure TCableGL.CreateVertices;
var i: integer;
begin
  if not assigned(FCableNodeList) then
    exit;

  if assigned(FVer) then
    FVer.Free;

  if assigned(FIdx) then
    FIdx.Free;

  FVer := TVertexBuffer.Create([TVertexFormat.Vertex, TVertexFormat.TexCoord0], 2*(FCableNodeList.Count + 1));
  FIdx := TIndexBuffer.Create(FCableNodeList.Count*6);

  for i := 0 to FCableNodeList.Count - 1 do begin
    FIdx[0 + 6*i] := 0 + i*2;
    FIdx[1 + 6*i] := 2 + i*2;
    FIdx[2 + 6*i] := 1 + i*2;
    FIdx[3 + 6*i] := 1 + i*2;
    FIdx[4 + 6*i] := 2 + i*2;
    FIdx[5 + 6*i] := 3 + i*2;
  end;
  //FTimer.Enabled := True;
end;

procedure TCableGL.CalcSize;
var i : integer;
    min_x, max_x, min_y, max_y: single;
begin
  if not assigned(FCableNodeList) then
    exit;

  min_x := Min(FP0.X, FP1.X);
  max_x := Max(FP0.X, FP1.X);
  min_y := Min(FP0.Y, FP1.Y);
  max_y := Max(FP0.Y, FP1.Y);

  for i := 0 to FCableNodeList.Count - 1 do begin

    if FCableNodeList[i].X < min_x then
      min_x := FCableNodeList[i].X;

    if FCableNodeList[i].X > max_x then
      max_x := FCableNodeList[i].X;

    if FCableNodeList[i].Y < min_y then
      min_y := FCableNodeList[i].Y;

    if FCableNodeList[i].Y > max_y then
      max_y := FCableNodeList[i].Y;
  end;

  Position := Point3D(
    min_x - FMargin,
    min_y - FMargin,
    0);

//  Width := max_x - Position.X + FMargin;
//  Height := max_y - Position.Y + FMargin;
end;

procedure TCableGL.CalcVertices;
var i: integer;
    d, SegLength: single;
    N0, N1, PT0, PB0, PT1, PB1: TPointF;

 function ImageXToReal(X: Single): Single;
 begin
   //if Projection = TProjection.Camera then
   //  Result := (X / 10)
   //else
     Result := X;
 end;

 function ImageYToReal(Y: Single): Single;
 begin
   //if Projection = TProjection.Camera then
   //  Result := (Y / 10)
   //else
     Result := Y;
 end;

 function CalcNormal(P0, P1: TPointF): TPointF;
 var l: single;
     V: TPointF;
 begin
   V.X := p1.X - p0.X;
   V.Y := p1.Y - p0.Y;
   Result.X := V.Y;
   Result.Y := -V.X;
   l := Sqrt(Result.X*Result.X + Result.Y*Result.Y);
   if l <> 0 then begin
     Result.X := Result.X / l;
     Result.Y := Result.Y / l;
   end else
     Result := PointF(0,0);
 end;

begin
  if not assigned(FCableNodeList) then
    exit;

  N0 := CalcNormal(PointF(FCableNodeList[0].X, FCableNodeList[0].Y),
                   PointF(FCableNodeList[1].X, FCableNodeList[1].Y));

  PT0.X := FCableNodeList[0].X + FCableThickness*N0.X - Position.X;
  PT0.Y := FCableNodeList[0].Y + FCableThickness*N0.Y - Position.Y;
  PB0.X := FCableNodeList[0].X - FCableThickness*N0.X - Position.X;
  PB0.Y := FCableNodeList[0].Y - FCableThickness*N0.Y - Position.Y;

  FVer.Vertices[0] := TPoint3D.Create(ImageXToReal(PT0.X), ImageYToReal(PT0.Y), 0);
  FVer.TexCoord0[0] := TPointF.Create(0, 0);
  FVer.Color0[0] := TAlphaColorRec.Green;
  FVer.Vertices[1] := TPoint3D.Create(ImageXToReal(PB0.X), ImageYToReal(PB0.Y), 0);
  FVer.TexCoord0[1] := TPointF.Create(0, 1);
  FVer.Color0[1] := TAlphaColorRec.Blue;

  for i := 1 to FCableNodeList.Count - 1 do begin
    PT0.X := FCableNodeList[i].X + FCableThickness*N0.X - Position.X;
    PT0.Y := FCableNodeList[i].Y + FCableThickness*N0.Y - Position.Y;
    PB0.X := FCableNodeList[i].X - FCableThickness*N0.X - Position.X;
    PB0.Y := FCableNodeList[i].Y - FCableThickness*N0.Y - Position.Y;

    if i < FCableNodeList.Count - 2 then
      N0 := CalcNormal(PointF(FCableNodeList[i].X, FCableNodeList[i].Y),
                       PointF(FCableNodeList[i+1].X, FCableNodeList[i+1].Y));

    PT1.X := FCableNodeList[i].X + FCableThickness*N0.X - Position.X;
    PT1.Y := FCableNodeList[i].Y + FCableThickness*N0.Y - Position.Y;
    PB1.X := FCableNodeList[i].X - FCableThickness*N0.X - Position.X;
    PB1.Y := FCableNodeList[i].Y - FCableThickness*N0.Y - Position.Y;

    FVer.Vertices[(i*2)] := TPoint3D.Create(ImageXToReal((PT0.X + PT1.X)/2), ImageYToReal((PT0.Y + PT1.Y)/2), 0);
    FVer.TexCoord0[(i*2)] := TPointF.Create(0, 0);
    FVer.Color0[(i*2)] := TAlphaColorRec.Green;
    FVer.Vertices[(i*2)+1] := TPoint3D.Create(ImageXToReal((PB0.X + PB1.X)/2), ImageYToReal((PB0.Y + PB1.Y)/2), 0);
    FVer.TexCoord0[(i*2)+1] := TPointF.Create(0, 1);
    FVer.Color0[(i*2)+1] := TAlphaColorRec.Blue;
  end;

  i := FCableNodeList.Count;

  FVer.Vertices[(i*2)] := TPoint3D.Create(ImageXToReal(PT1.X), ImageYToReal(PT1.Y), 0);
  FVer.TexCoord0[(i*2)] := TPointF.Create(0, 0);
  FVer.Vertices[(i*2)+1] := TPoint3D.Create(ImageXToReal(PB1.X), ImageYToReal(PB1.Y), 0);
  FVer.TexCoord0[(i*2)+1] := TPointF.Create(0, 1);
end;

procedure TCableGL.InitCable;
//var l, min_x, min_y, max_x, max_y : single;
begin
  {min_x := Min(FP0.X, FP1.X);
  max_x := Max(FP0.X, FP1.X);
  min_y := Min(FP0.Y, FP1.Y);
  max_y := Max(FP0.Y, FP1.Y);

  l := sqrt((max_x - min_x)*(max_x - min_x) + (max_y - min_y)*(max_y - min_y));}

  FCableNodeList := TCableNodeList.Create(FP0, FP1, 20);
  CreateVertices;
  IterateCable;
end;

procedure TCableGL.IterateCable;
begin
  if assigned(FCableNodeList) then begin
    FCableNodeList[0].X := P0.X;
    FCableNodeList[0].Y := P0.Y;
    FCableNodeList[FCableNodeList.Count - 1].X := P1.X;
    FCableNodeList[FCableNodeList.Count - 1].Y := P1.Y;

    FCableNodeList.Iterate;
    CalcSize;
    CalcVertices;
  end;
end;

procedure TCableGL.RenderControl(aContext: TContext3D);
begin
  inc(GLObjectCount);

  if assigned(FVer) and assigned(FIdx) then
    aContext.DrawTriangles(FVer, FIdx, FMat, AbsoluteOpacity);
end;

procedure TCableGL.SetCableThickness(const Value: single);
begin
  FCableThickness := Value;
  InitCable;
end;

procedure TCableGL.SetColor(const Value: TAlphaColor);
begin
  FMat.Color := Value;
end;

procedure TCableGL.SetP0(const Value: TPointF);
begin
  FP0 := Value;
end;

procedure TCableGL.SetP1(const Value: TPointF);
begin
  FP1 := Value;
end;

{ TColorObject }

constructor TColorObject.Create;
begin
  inherited Create;

  FMesh := TSegmentedMeshGL.Create;
  FColorMat := TColorMaterial.Create;
  FColorMat.Color := 0;
end;

destructor TColorObject.Destroy;
begin
  FreeAndNil(FMesh);

  inherited;
end;

function TColorObject.GetColor: TAlphaColor;
begin
  Result := FColorMat.Color;
end;

procedure TColorObject.Render(aContext: TContext3D);
begin
  if FColorMat.Color <> 0 then
    aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FColorMat, 1);
end;

procedure TColorObject.SetColor(const Value: TAlphaColor);
begin
  FColorMat.Color := Value;
end;

procedure TColorObject.SetPosition(const Value: TPoint3D);
begin
  FPosition := Value;
end;

{ TTexObject }

constructor TTexObject.Create;
begin
  inherited;

  FTexMat := TTextureMaterial.Create;
  FTexCols := 1;
  FTexRows := 1;
  FTileWidth := -1;
  FTileHeight := -1;
  FImageIndex := 0;
end;

function TTexObject.CreateTexture: TTiledTexture;
begin
  if assigned(FOnTexCreate) then
  begin
    FOnTexCreate(self, Result);
  end else
    Result := nil;
end;

destructor TTexObject.Destroy;
begin
  TextureID := '';
  FreeAndNil(FTexMat);

  inherited;
end;

function TTexObject.GetSubRect(const aIndex: integer): TRectF;
begin
  if(FTexCols <> 0) and (FTexRows <> 0) then
  begin
    Result.Left := (aIndex mod FTexCols) / FTexCols;
    Result.Top := (aIndex div FTexCols) / FTexRows;

    Result.Right := Result.Left + 1 / FTexCols;
    Result.Bottom :=  Result.Top + 1 / FTexRows;
  end else
    Result := TRectF.Empty;
end;

function TTexObject.GetTexCols: integer;
begin
  Result := FTexCols;
end;

function TTexObject.GetTexRows: integer;
begin
  Result := FTexRows;
end;

function TTexObject.GetTexture: TTiledTexture;
begin
  Result := nil;
  if assigned(FTextureList) and (FTextureID <> '') then
  begin
    if not FTextureList.Link(FTextureID, Result) then
    begin
      Result := CreateTexture;
      if assigned(Result) then
        FTextureList.Add(FTextureID, Result);
    end;
  end;
end;

function TTexObject.GetTileHeight: single;
var
  Texture: TTiledTexture;
begin
  if FTileHeight = -1 then
  begin
    Result := 0.0;
    if FTexRows <> 0 then
    begin
      Texture := GetTexture;
      if assigned(Texture) then
      begin
        FTileHeight := Texture.Height / FTexRows;
        Result := FTileHeight;
      end;
    end;
  end else
    Result := FTileHeight;
end;

function TTexObject.GetTileWidth: single;
var
  Texture: TTiledTexture;
begin
  if FTileWidth = -1 then
  begin
    Result := 0.0;
    if FTexCols <> 0 then
    begin
      Texture := GetTexture;
      if assigned(Texture) then
      begin
        FTileWidth := Texture.Width / FTexCols;
        Result := FTileWidth;
      end;
    end
  end else
    Result := FTileWidth;
end;

procedure TTexObject.Render(aContext: TContext3D);
var
  Texture: TTiledTexture;
  SegWidth, SegHeight: single;
begin
 if (FTexCols <> 0) and (FTexRows <> 0) then
  begin
    Texture := GetTexture;
    if assigned(Texture) then
    begin
      FTexMat.Texture := Texture.Texture;

      SegWidth := Texture.Width / FTexCols;
      SegHeight := Texture.Height / FTexRows;

      FMesh.CreateMesh(SegWidth, SegHeight, 0, 1, 1);

      inherited;

      UpdateTexCoords;
      aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FTexMat, 1);
    end;
  end;
end;

procedure TTexObject.SetImageIndex(const Value: integer);
begin
  if (Value >= 0) and (Value < FTexCols * FTexRows) and (Value <> FImageIndex) then begin
    FImageIndex := Value;
  end else
    FImageIndex := 0;
end;

procedure TTexObject.SetTexCols(const Value: integer);
begin
  FTexCols := Value;
end;

procedure TTexObject.SetTexRows(const Value: integer);
begin
  FTexRows := Value;
end;

procedure TTexObject.SetTextureID(const Value: string);
begin
  if FTextureID <> Value then
  begin
    FTextureID := Value;
    FTileWidth := -1;
    FTileHeight := -1;
  end;
end;

procedure TTexObject.SetTextureList(const Value: TTextureListGL);
begin
  if FTextureList <> Value then
  begin
    FTextureList := Value;
  end;
end;

procedure TTexObject.UpdateTexCoords;
var
  R: TRectF;
begin
  R := GetSubRect(FImageIndex);
  FMesh.VertexBuffer.TexCoord0[0] := TPointF.Create(R.Left, R.Top);
  FMesh.VertexBuffer.TexCoord0[1] := TPointF.Create(R.Left, R.Bottom);
  FMesh.VertexBuffer.TexCoord0[2] := TPointF.Create(R.Right, R.Top);
  FMesh.VertexBuffer.TexCoord0[3] := TPointF.Create(R.Right, R.Bottom);
end;

{ TTexMatrixObject }

constructor TTexMatrixObject.Create;
begin
  inherited;

  FRowCount := 0;
  FColCount := 0;
  FPadding := 0;
  SetLength(FCellIndexArray, 0);
end;

destructor TTexMatrixObject.Destroy;
begin
  Finalize(FCellIndexArray);

  inherited;
end;

function TTexMatrixObject.GetCellIndex(const aIndex: integer): integer;
begin
  Result := FCellIndexArray[aIndex];
end;

function TTexMatrixObject.GetHeight: single;
begin
  Result := (TileHeight + FPadding) * FRowCount + FPadding;
end;

function TTexMatrixObject.GetWidth: single;
begin
  Result := (TileWidth + FPadding) * FColCount + FPadding;
end;

function TTexMatrixObject.IndexFromPoint(X, Y: Single): integer;
var Col, Row: integer;
begin
  Col := FMesh.ColFromX(X);
  Row := FMesh.RowFromY(Y);

  Result := Row * ColCount + Col;
end;

procedure TTexMatrixObject.Render(aContext: TContext3D);
var
  Texture: TTiledTexture;
begin
  inc(GLObjectCount);

  if (FTexCols <> 0) and (FTexRows <> 0) then
  begin
    Texture := GetTexture;
    if assigned(Texture) then
    begin
      FMesh.CreateMesh(TileWidth, TileHeight, FPadding, ColCount, RowCount);

      if FColorMat.Color <> 0 then
        aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FColorMat, 1);

      FTexMat.Texture := Texture.Texture;
      UpdateTexCoords;
      aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FTexMat, 1);
    end;
  end;
end;

procedure TTexMatrixObject.SetCellIndex(const aIndex, Value: integer);
begin
  if FCellIndexArray[aIndex] <> Value then
  begin
    FCellIndexArray[aIndex] := Value;
  end;
end;

procedure TTexMatrixObject.SetColCount(const Value: integer);
var
  OldSize, i: integer;
begin
  if FColCount <> Value then
  begin
    OldSize := FColCount * FRowCount;
    FColCount := Value;
    SetLength(FCellIndexArray, FColCount * FRowCount);
    for i := OldSize to FColCount * FRowCount - 1 do
      FCellIndexArray[i] := 0;
    UpdateCellIndex;
  end;
end;

procedure TTexMatrixObject.SetPadding(const Value: single);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
  end;
end;

procedure TTexMatrixObject.SetRowCount(const Value: integer);
var
  OldSize, i: integer;
begin
  if RowCount <> Value then begin
    OldSize := FColCount * FRowCount;
    FRowCount := Value;
    SetLength(FCellIndexArray, FColCount * FRowCount);
    for i := OldSize to FColCount * FRowCount - 1 do
      FCellIndexArray[i] := 0;
    UpdateCellIndex;
  end;
end;

procedure TTexMatrixObject.UpdateCellIndex;
begin
 //
end;

procedure TTexMatrixObject.UpdateTexCoords;
var
  i, j, Idx, BtnState, TypeCount : integer;
  l, t, r, b: single;
  Texture: TTiledTexture;
begin
  // Button types must be on columns of texture
  // Button states must be on row of texture

  Texture := GetTexture;

  if assigned(Texture) then
  begin
    TypeCount := Round(Texture.Width / TileWidth);

    for i := 0 to FRowCount - 1 do begin

      for j := 0 to FColCount - 1 do begin

        Idx := i * FColCount + j;

        BtnState := FCellIndexArray[Idx];

        l := (Idx mod TypeCount) / TexCols;
        t := BtnState / TexRows;
        r := (Idx mod TypeCount) / TexCols + 1 / TexCols;
        b := BtnState / TexRows + 1 / TexRows;

        FMesh.VertexBuffer.TexCoord0[0 + Idx*4] := TPointF.Create(l, t);
        FMesh.VertexBuffer.TexCoord0[1 + Idx*4] := TPointF.Create(l, b);
        FMesh.VertexBuffer.TexCoord0[2 + Idx*4] := TPointF.Create(r, t);
        FMesh.VertexBuffer.TexCoord0[3 + Idx*4] := TPointF.Create(r, b);

      end;
    end;
  end;
end;

{ TMatrixTextObject }

constructor TMatrixTextObject.Create;
begin
  inherited Create;

  FMat := TTextureMaterial.Create;
  FMesh := TSegmentedMeshGL.Create;
  FText := '';
  FTextAlign := TTextAlign.Leading;
  FTexCols := 12;
  FTexRows := 8;
end;

function TMatrixTextObject.CreateTexture: TTiledTexture;
begin
  Result := nil; // TODO
end;

procedure TMatrixTextObject.CreateVertices;
var
  l: integer;
  SegmentWidth: single;
begin
  l := Length(FText);
  if l = 0 then
    SegmentWidth := 0
  else
    SegmentWidth := Width / l;

  if assigned(FMesh) then
    FMesh.CreateMesh(SegmentWidth, Height, 0, l, 1);

  UpdateTexCoords;
end;

destructor TMatrixTextObject.Destroy;
begin
  TextureID := '';
  FreeAndNil(FMesh);
  FreeAndNil(FMat);

  inherited;
end;

function TMatrixTextObject.GetTextHeight: single;
var
  Texture: TTiledTexture;
begin
  Texture := GetTexture;
  if assigned(Texture)  then
    Result := Texture.Height / FTexRows
  else
    Result := 0;
end;

function TMatrixTextObject.GetTexture: TTiledTexture;
begin
  Result := nil;
  if assigned(FTextureList) and (FTextureID <> '') then
  begin
    if not FTextureList.Link(FTextureID, Result) then
    begin
      Result := CreateTexture;
      if assigned(Result) then
        FTextureList.Add(FTextureID, Result);
    end;
  end;
end;

function TMatrixTextObject.GetTextWidth: single;
var
  Texture: TTiledTexture;
begin
  Texture := GetTexture;
  if assigned(Texture) then
    Result := Texture.Width / FTexCols * Length(FText)
  else
    Result := 0;
end;

procedure TMatrixTextObject.Render(aContext: TContext3D);
var
  Texture: TTiledTexture;
begin
  Texture := GetTexture;
  if assigned(Texture) then
  begin
    FMat.Texture := Texture.Texture;

    aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FMat, 1);
  end;
end;

procedure TMatrixTextObject.SetHeight(const Value: single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    CreateVertices;
  end;
end;

procedure TMatrixTextObject.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    CreateVertices;
  end;
end;

procedure TMatrixTextObject.SetTextAlign(const Value: TTextAlign);
begin
  if FTextAlign <> Value then
  begin
    FTextAlign := Value;
  end;
end;

procedure TMatrixTextObject.SetTextureID(const Value: string);
begin
  if FTextureID <> Value then
  begin
    FTextureID := Value;
  end;
end;

procedure TMatrixTextObject.SetTextureList(const Value: TTextureListGL);
begin
  if FTextureList <> Value then
  begin
    FTextureList := Value;
    TextureID := GetFontTexID;
  end;
end;

procedure TMatrixTextObject.SetWidth(const Value: single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    CreateVertices;
  end;
end;

procedure TMatrixTextObject.UpdateTexCoords;
var
  i, c : integer;
  l, t, w, h: single;
  Texture: TTiledTexture;
begin
  Texture := GetTexture;
  if assigned(Texture) then
  begin
    w := 1 / FTexCols;
    h := 1 / FTexRows;

    for i := 0 to Length(FText) - 1 do begin

      if i < FText.Length then
        c := ord(FText.Chars[i]) - 32
      else
        c := 0;

      if (c < 0) or (c >= FTexCols * FTexRows) then
        c := 0;
      l := (c mod FTexCols) / FTexCols;
      t := (c div FTexCols) / FTexRows;

      FMesh.VertexBuffer.TexCoord0[0 + i*4] := TPointF.Create(l, t);
      FMesh.VertexBuffer.TexCoord0[1 + i*4] := TPointF.Create(l, t + h);
      FMesh.VertexBuffer.TexCoord0[2 + i*4] := TPointF.Create(l + w, t);
      FMesh.VertexBuffer.TexCoord0[3 + i*4] := TPointF.Create(l + w, t + h);
    end;
  end;
end;

{ TTexMatrixControlGL }

constructor TTexMatrixControlGL.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TTexMatrixControlGL.CreateTexObjects;
begin
  FTexObject := TTexMatrixObject.Create;
end;

destructor TTexMatrixControlGL.Destroy;
begin
  inherited;
end;

procedure TTexMatrixControlGL.DoClk(Shift: TShiftState; X, Y: Single);
begin
  if assigned(FOnClk) then
    FOnClk(Self, Shift, IndexFromPoint(X, Y));
end;

function TTexMatrixControlGL.GetCellIndex(const aIndex: integer): integer;
begin
  Result := TexMatrixObject.CellIndex[aIndex];
end;

function TTexMatrixControlGL.GetColCount: integer;
begin
  Result := TexMatrixObject.ColCount;
end;

function TTexMatrixControlGL.GetHeight: single;
begin
  Result := TexMatrixObject.Height;
end;

function TTexMatrixControlGL.GetPadding: single;
begin
  Result := TexMatrixObject.Padding;
end;

function TTexMatrixControlGL.GetRowCount: integer;
begin
  Result := TexMatrixObject.RowCount;
end;

function TTexMatrixControlGL.GetTexMatrixObject: TTexMatrixObject;
begin
  Result := FTexObject as TTexMatrixObject;
end;

function TTexMatrixControlGL.GetWidth: single;
begin
  Result := TexMatrixObject.Width;
end;

function TTexMatrixControlGL.IndexFromPoint(X, Y: Single): integer;
begin
  Result := TexMatrixObject.IndexFromPoint(X, Y)
end;

procedure TTexMatrixControlGL.RenderControl(aContext: TContext3D);
begin
  inc(GLObjectCount);

  TexMatrixObject.Render(aContext);
end;

procedure TTexMatrixControlGL.SetCellIndex(const aIndex, Value: integer);
begin
  if TexMatrixObject.CellIndex[aIndex] <> Value then
  begin
    TexMatrixObject.CellIndex[aIndex] := Value;
    Invalidate;
  end;
end;

procedure TTexMatrixControlGL.SetColCount(const Value: integer);
begin
  if TexMatrixObject.ColCount <> Value then
  begin
    TexMatrixObject.ColCount := Value;
    UpdateCellIndex;
    Invalidate;
  end;
end;

procedure TTexMatrixControlGL.SetPadding(const Value: single);
begin
  if TexMatrixObject.Padding <> Value then
  begin
    TexMatrixObject.Padding := Value;
    Invalidate;
  end;
end;

procedure TTexMatrixControlGL.SetRowCount(const Value: integer);
begin
  if TexMatrixObject.RowCount <> Value then
  begin
    TexMatrixObject.RowCount := Value;
    UpdateCellIndex;
    Invalidate;
  end;
end;

procedure TTexMatrixControlGL.UpdateCellIndex;
begin
//
end;

{ TLabelStaticGL }

constructor TLabelStaticGL.Create;
begin
  inherited Create;

  FFont := TFont.Create;
  FTexture := TTextureBitmap.Create;
  FMat := TTextureMaterial.Create;
  FMesh := TSegmentedMeshGL.Create;
  FColor := TAlphaColorRec.Black;
  FText := '';
  FTextAlign := TTextAlign.Leading;
  FNeedsRepaint := False;
end;

constructor TLabelStaticGL.Create(aFontFamily: string; aFontSize: integer;
  aColor: TAlphaColor; aTextAlign: TTextAlign; aText: string;
  const aWidth, aHeight: single);
begin
  inherited Create;

  FFont := TFont.Create;
  FFont.Family := aFontFamily;
  FFont.Size := aFontSize;

  FTexture := TTextureBitmap.Create;
  FMat := TTextureMaterial.Create;
  FMesh := TSegmentedMeshGL.Create;

  FColor := aColor;
  FText := aText;
  FTextAlign := aTextAlign;

  FWidth := aWidth;
  FHeight := aHeight;

  FNeedsRepaint := True;
end;

destructor TLabelStaticGL.Destroy;
begin
  FreeAndNil(FMesh);
  FreeAndNil(FMat);
  FreeAndNil(FTexture);
  FreeAndNil(FFont);

  inherited;
end;

procedure TLabelStaticGL.Render(aContext: TContext3D);
begin
  //if FNeedsRepaint then
  //  RenderTexture;

{$IFDEF ANDROID}
  // Seems on Android the link to the texture is lost after the application
  // becomes inactive
  FMat.Texture := FTexture.Texture;
{$ENDIF}

  aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FMat, 1);
end;

procedure TLabelStaticGL.RenderTexture;
var
  w, h, sx, sy: single;
  SaveMatrix: TMatrix;
  TextLayout: TTextLayout;
begin
  if (FWidth = 0) or (FHeight = 0) then
    exit;

  FTexture.SetSize(10, 10);

  TextLayout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    TextLayout.TopLeft := PointF(0, 0);
    TextLayout.MaxSize := PointF(100, 100);
    TextLayout.Text := FText;
    TextLayout.Font.Assign(FFont);
    TextLayout.HorizontalAlign := TTextAlign.Leading;
    TextLayout.VerticalAlign := TTextAlign.Leading;
    TextLayout.WordWrap := False;
    TextLayout.EndUpdate;

    w := TextLayout.TextWidth;
    h := TextLayout.TextHeight;

  finally
    TextLayout.Free;
  end;

  {FTexture.Canvas.BeginScene;
  try
    FTexture.Canvas.Font.Assign(FFont);
    w := FTexture.Canvas.TextWidth(FText);
    h := FTexture.Canvas.TextHeight(FText);
  finally
    FTexture.Canvas.EndScene;
  end;}

  if w <= 0 then
    w := 10;

  if h <= 0 then
    h := 10;

  FTexture.SetSize(Round(FWidth), Round(FHeight));

  FTexture.Canvas.BeginScene;
  try
    SaveMatrix := FTexture.Canvas.Matrix;

    if w > FWidth then
      sx := FWidth / w
    else
      sx := 1;

    if h > FHeight then
      sy := FHeight / h
    else
      sy := 1;

    FTexture.Canvas.MultiplyMatrix(TMatrix.CreateScaling(sx, sy));

    FTexture.Canvas.Clear(0);
    FTexture.Canvas.Font.Assign(FFont);
    FTexture.Canvas.Fill.Color := FColor;
    FTexture.Canvas.FillText(RectF(0, 0, FWidth/sx, FHeight/sy), FText, False, 1, [],
      FTextAlign, TTextAlign.Center);

    FTexture.Canvas.SetMatrix(SaveMatrix);
  finally
    FTexture.Canvas.EndScene;
  end;

  FMesh.CreateMesh(FWidth, FHeight, 0, 1, 1);
  FMat.Texture := FTexture.Texture;

  FNeedsRepaint := False;
end;

procedure TLabelStaticGL.SetColor(const Value: TAlphaColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FNeedsRepaint := True;
  end;
end;

procedure TLabelStaticGL.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  FNeedsRepaint := True;
end;

procedure TLabelStaticGL.SetHeight(const Value: single);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    FNeedsRepaint := True;
  end;
end;

procedure TLabelStaticGL.SetText(const Value: string);
begin
  if Value <> FText then
  begin
    FText := Value;
    FNeedsRepaint := True;
  end;
end;

procedure TLabelStaticGL.SetTextAlign(const Value: TTextAlign);
begin
  if Value <> FTextAlign then
  begin
    FTextAlign := Value;
    FNeedsRepaint := True;
  end;
end;

procedure TLabelStaticGL.SetWidth(const Value: single);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    FNeedsRepaint := True;
  end;
end;

{ TOulineGL }

constructor TOutlineGL.Create(aPosition: TPoint3D; aWidth, aHeight: single);
begin
  inherited Create;

  FPosition := aPosition;
  FWidth := aWidth;
  FHeight := aHeight;

  FMesh := TSegmentedMeshGL.Create;
  FMat := TColorMaterial.Create;
  FMat.Color := TAlphaColorRec.White;

  FMesh.CreateMesh(FWidth, FHeight, 0, 1, 1);

  FMatrix := TMatrix3D.CreateTranslation(FPosition);
end;

destructor TOutlineGL.Destroy;
begin
  FMat.Free;
  FMesh.Free;

  inherited;
end;

procedure TOutlineGL.Render(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
begin
  SaveMatrix := aContext.CurrentMatrix;
  try
    aContext.SetMatrix(FMatrix * SaveMatrix);

    aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FMat, 0.8);

  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

{ TCursorGL }

constructor TCursorGL.Create(aPosition: TPoint3D; aWidth, aHeight: single);
begin
  inherited Create;

  FPosition := aPosition;
  FWidth := aWidth;
  FHeight := aHeight;

  FMesh := TRectOutlineMeshGL.Create;
  FMat := TColorMaterial.Create;
  FMat.Color := TAlphaColorRec.Green;

  FMesh.CreateMesh(FWidth, FHeight, 4);

  FMatrix := TMatrix3D.CreateTranslation(FPosition);
end;

destructor TCursorGL.Destroy;
begin
  FMat.Free;
  FMesh.Free;

  inherited;
end;

procedure TCursorGL.Render(aContext: TContext3D);
var
  SaveMatrix: TMatrix3D;
begin
  SaveMatrix := aContext.CurrentMatrix;
  try
    aContext.SetMatrix(FMatrix * SaveMatrix);

    aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FMat, 1.0);
  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

initialization
  RegisterFmxClasses([TBufferedViewPortGL]);

end.
