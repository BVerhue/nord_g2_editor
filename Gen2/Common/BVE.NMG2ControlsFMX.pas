unit BVE.NMG2ControlsFMX;
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
{$IFDEF IOS}
  {$DEFINE ANDROID_IOS}
{$ENDIF}
{$IFDEF ANDROID}
  {$DEFINE ANDROID_IOS}
{$ENDIF}
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
  System.Generics.Collections,
  {$IF Defined(VER270) or Defined(VER280) or Defined(VER340)}
  System.Math.Vectors,
  {$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.Textlayout,
  {$IF Defined(VER260) or Defined(VER270) or Defined(VER280) or Defined(VER340)}
  FMX.Graphics,
  {$ENDIF}
  BVE.NMG2Types,
  BVE.NMG2Pathdata;
type
  TKeyType = (ktWhite, ktBlack);
  TOctaveKey = record
    KeyID : integer;
    KeyType : TKeyType;
  end;
const
  KeyTypes : array[0..11] of TOctaveKey =
     ((KeyID : 0; KeyType : ktWhite),
      (KeyID : 0; KeyType : ktBlack),
      (KeyID : 1; KeyType : ktWhite),
      (KeyID : 1; KeyType : ktBlack),
      (KeyID : 2; KeyType : ktWhite),
      (KeyID : 3; KeyType : ktWhite),
      (KeyID : 3; KeyType : ktBlack),
      (KeyID : 4; KeyType : ktWhite),
      (KeyID : 4; KeyType : ktBlack),
      (KeyID : 5; KeyType : ktWhite),
      (KeyID : 5; KeyType : ktBlack),
      (KeyID : 6; KeyType : ktWhite));
   MAX_KEYS = 127;
type
  TG2Connector = class;
  TG2BtnIncDec = class;
  TG2StyleSet = class;
  TBtnKind = (bkToggle, bkMomentary);
  TKnobControl = (kcCircular, kcHorizontal, kcVertical);
  TControlElementType = (ceBackGround, ceText, ceSymbol, ceMorph, ceSliderKnob);
  TControlState = (csDefault, csFocused, csSelected, csFocusedSelected, csDisabled);
  TKeyboardOrientation = ( kbVertical, kbHorizontal);
  TChangeValueEvent = procedure(Sender: TObject; const aValue : integer) of object;
  TButtonClickEvent = procedure(Sender: TObject; const aIndex : integer) of object;
  TGetTextFuncEvent = procedure(Sender : TObject; var aTextFunc : string) of object;
  TCreateCableEvent = procedure(Sender : TObject; aToConnector : TG2Connector) of object;
  TPaintElementEvent = procedure(Sender : TObject; const aElementType : TControlElementType;
      aElementIndex : integer; aStyleSet : TG2StyleSet) of Object;
  TKeyboardKeyDownEvent = procedure(Sender : TObject; aKeyNo : integer) of object;
  TKeyboardKeyUpEvent = procedure(Sender : TObject; aKeyNo : integer) of object;
  TOctaveButtonChangeEvent = procedure(Sender : TObject; aOctave : integer; aValue : integer) of object;
  TDebugEvent = procedure(Sender : TObject; const aText : string) of object;
  TG2BufferedLayout = class;
  TBufferedControl = class(TControl)
  private
    FUnscaledPosition : TPosition;
    FUnscaledWidth : single;
    FUnscaledHeight : single;
    FMargin : single;
    FBuffered : boolean;
    FBitmap : TBitmap;
    FRedraw : boolean;
    FZoom : single;
    FG2Updating : boolean;
    FLog : TStrings;
  private
    procedure InitBitmap;
    procedure SetUnscaledPosition(const Value: TPosition);
  protected
    function GetUnscaledRect: TRectF;
    function GetUnscaledBoundsRect: TRectF;
    function GetUnscaledHeight: single;
    function GetUnscaledLeft: single;
    function GetUnscaledTop: single;
    function GetUnscaledWidth: single;
    procedure SetUnscaledHeight(const Value: single);
    procedure SetUnscaledLeft(const Value: single);
    procedure SetUnscaledTop(const Value: single);
    procedure SetUnscaledWdith(const Value: single);
    function GetBuffered: boolean; virtual;
    procedure SetBuffered(const Value: boolean); virtual;
    procedure SetZoom(const Value: single); virtual;
    procedure SetUnscaledRect(const Value: TRectF); virtual;
    procedure SetUnscaledBoundsRect(const Value: TRectF); virtual;
    procedure SetParent(const Value: TFmxObject); override;
    function GetControlRect: TRectF;
    procedure CalcZoomedBounds;
    procedure CalcUnscaledBounds(const X, Y, w, h : single);
    procedure CalcDimensions; virtual;
    procedure CalcMargin; virtual;
    procedure WriteLog(const aText : string);
    procedure SetLog(const Value: TStrings); virtual;
    {$IF Defined(VER280) or Defined(VER340)}
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    {$ELSE}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    {$ENDIF}
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure DrawTextExt( aCanvas : TCanvas; const aFont : TFont; const aTopLeft, aMaxSize : TPointF; const aColor : TAlphaColor; const aText : string; const aHorzAlign, aVertAlign : TTextAlign);
    procedure Paint; override;
    procedure PaintOn( aCanvas : TCanvas); virtual;
    //procedure PaintBuffered( aCanvas : TCanvas; aDraw : boolean);
    procedure Resize; override;
    procedure Redraw; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: Single); override;
    property Log : TStrings read FLog write SetLog;
    property UnscaledRect : TRectF read GetUnscaledRect write SetUnscaledRect;
    property UnscaledBoundsRect : TRectF read GetUnscaledBoundsRect write SetUnscaledBoundsRect;
    property UnscaledLeft : single read GetUnscaledLeft write SetUnscaledLeft;
    property UnscaledTop : single read GetUnscaledTop write SetUnscaledTop;
    property ControlRect : TRectF read GetControlRect;
    property Buffered: boolean read GetBuffered write SetBuffered;
    property Bitmap : TBitmap read FBitmap;
    property Zoom: single read FZoom write SetZoom;
    property UnscaledPosition : TPosition read FUnscaledPosition write SetUnscaledPosition;
    property UnscaledWidth : single read GetUnscaledWidth write SetUnscaledWdith;
    property UnscaledHeight : single read GetUnscaledHeight write SetUnscaledHeight;
  published
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    {$IF not Defined(VER340)}
    property DesignVisible default True;
    {$IFEND}
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    {$IF Defined(VER280) or Defined(VER340)}
    property Size;
    {$ENDIF}
    property Scale;
    property Visible default True;
    property Width;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BufferedLayout = class(TLayout)
  private
    FBuffered : boolean;
    FZoom : single;
    FOnControlGesture : TGestureEvent;
    FOnControlMouseUp: TMouseEvent;
    FOnControlMouseDown: TMouseEvent;
    FOnControlMouseMove: TMouseMoveEvent;
    FOnModuleGesture : TGestureEvent;
    FOnModuleMouseUp: TMouseEvent;
    FOnModuleMouseDown: TMouseEvent;
    FOnModuleMouseMove: TMouseMoveEvent;
  protected
    procedure SetBuffered(const Value: boolean);
    procedure SetZoom( aValue : single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnControlGesture: TGestureEvent read FOnControlGesture write FOnControlGesture;
    property OnControlMouseUp: TMouseEvent read FOnControlMouseUp write FOnControlMouseUp;
    property OnControlMouseDown: TMouseEvent read FOnControlMouseDown write FOnControlMouseDown;
    property OnControlMouseMove: TMouseMoveEvent read FOnControlMouseMove write FOnControlMouseMove;
    property OnModuleGeture : TGestureEvent read FOnModuleGesture write FOnModuleGesture;
    property OnModuleMouseUp: TMouseEvent read FOnModuleMouseUp write FOnModuleMouseUp;
    property OnModuleMouseDown: TMouseEvent read FOnModuleMouseDown write FOnModuleMouseDown;
    property OnModuleMouseMove: TMouseMoveEvent read FOnModuleMouseMove write FOnModuleMouseMove;
  published
    property Zoom : single read FZoom write SetZoom;
    property Buffered : boolean read FBuffered write SetBuffered;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TArrayLayout = class(TLayout)
  private
    FRows: integer;
    FColumns: integer;
    procedure RealignChildren;
    procedure SetColumns(const Value: integer);
    procedure SetRows(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
  published
    property Rows : integer read FRows write SetRows;
    property Columns : integer read FColumns write SetColumns;
  end;
  TG2StyleSet = class
  private
    FState : TControlState;
    FFill : TBrush;
    FStroke : TStrokeBrush;
    FFont  : TFont;
    FFontColor : TAlphaColor;
    FActiveMorphColor : TAlphaColor;
    FInactiveMorphColor : TAlphaColor;
    procedure SetFill(const Value: TBrush);
    procedure SetFont(const Value: TFont);
    procedure SetFontColor(const Value: TAlphaColor);
    procedure SetStroke(const Value: TStrokeBrush);
    procedure SetActiveMorphColor(const Value: TAlphaColor);
    procedure SetInactiveMorphColor(const Value: TAlphaColor);
  public
    constructor Create( aControlState : TControlState);
    destructor Destroy; override;
    procedure Assign( aStyleSet : TG2StyleSet);
    procedure ApplyStyleSet( aCanvas : TCanvas);
    procedure WriteToStrings( const aIndent : integer; sl : TStringList);
    property State : TControlState read FState write FState;
    property Fill: TBrush read FFill write SetFill;
    property Stroke: TStrokeBrush read FStroke write SetStroke;
    property Font: TFont read FFont write SetFont;
    property FontColor : TAlphaColor read FFontColor write SetFontColor;
    property ActiveMorphColor : TAlphaColor read FActiveMorphColor write SetActiveMorphColor;
    property InactiveMorphColor : TAlphaColor read FInactiveMorphColor write SetInactiveMorphColor;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2StateStyleList = class(TComponent)
  private
    FItems : TObjectList<TG2StyleSet>;
    function GetStateStyle( aControlState: TControlState): TG2StyleSet;
    function GetDefaultFill: TBrush;
    function GetDefaultFont: TFont;
    function GetDefaultFontColor: TAlphaColor;
    function GetDefaultStroke: TStrokeBrush;
    procedure SetDefaultFill(const Value: TBrush);
    procedure SetDefaultFont(const Value: TFont);
    procedure SetDefaultFontColor(const Value: TAlphaColor);
    procedure SetDefaultStroke(const Value: TStrokeBrush);
    function GetDisabledFill: TBrush;
    function GetDisabledFont: TFont;
    function GetDisabledFontColor: TAlphaColor;
    function GetDisabledStroke: TStrokeBrush;
    function GetFocusedFill: TBrush;
    function GetFocusedFont: TFont;
    function GetFocusedFontColor: TAlphaColor;
    function GetFocusedStroke: TStrokeBrush;
    function GetSelectedFill: TBrush;
    function GetSelectedFont: TFont;
    function GetSelectedFontColor: TAlphaColor;
    function GetSelectedStroke: TStrokeBrush;
    function GetFocusedSelectedFill: TBrush;
    function GetFocusedSelectedFont: TFont;
    function GetFocusedSelectedFontColor: TAlphaColor;
    function GetFocusedSelectedStroke: TStrokeBrush;
    procedure SetDisabledFill(const Value: TBrush);
    procedure SetDisabledFont(const Value: TFont);
    procedure SetDisabledFontColor(const Value: TAlphaColor);
    procedure SetDisabledStroke(const Value: TStrokeBrush);
    procedure SetFocusedFill(const Value: TBrush);
    procedure SetFocusedFont(const Value: TFont);
    procedure SetFocusedFontColor(const Value: TAlphaColor);
    procedure SetFocusedStroke(const Value: TStrokeBrush);
    procedure SetSelectedFill(const Value: TBrush);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetSelectedFontColor(const Value: TAlphaColor);
    procedure SetSlectedStroke(const Value: TStrokeBrush);
    procedure SetFocusedSelectedFill(const Value: TBrush);
    procedure SetFocusedSelectedFont(const Value: TFont);
    procedure SetFocusedSelectedFontColor(const Value: TAlphaColor);
    procedure SetFocusedSelectedStroke(const Value: TStrokeBrush);
    function GetMaxStrokeThickness: single;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    function AddStateStyle( aControlState : TControlState): TG2StyleSet;
    procedure WriteToStrings( const aIndent : integer; sl : TStringList);
    property StateStyle[ index : TControlState]: TG2StyleSet read GetStateStyle;
  published
    property DefaultFill: TBrush read GetDefaultFill write SetDefaultFill;
    property DefaultStroke: TStrokeBrush read GetDefaultStroke write SetDefaultStroke;
    property DefaultFont: TFont read GetDefaultFont write SetDefaultFont;
    property DefaultFontColor: TAlphaColor read GetDefaultFontColor write SetDefaultFontColor;
    property FocusedFill: TBrush read GetFocusedFill write SetFocusedFill;
    property FocusedStroke: TStrokeBrush read GetFocusedStroke write SetFocusedStroke;
    property FocusedFont: TFont read GetFocusedFont write SetFocusedFont;
    property FocusedFontColor: TAlphaColor read GetFocusedFontColor write SetFocusedFontColor;
    property SelectedFill: TBrush read GetSelectedFill write SetSelectedFill;
    property SelectedStroke: TStrokeBrush read GetSelectedStroke write SetSlectedStroke;
    property SelectedFont: TFont read GetSelectedFont write SetSelectedFont;
    property SelectedFontColor: TAlphaColor read GetSelectedFontColor write SetSelectedFontColor;
    property FocusedSelectedFill: TBrush read GetFocusedSelectedFill write SetFocusedSelectedFill;
    property FocusedSelectedStroke: TStrokeBrush read GetFocusedSelectedStroke write SetFocusedSelectedStroke;
    property FocusedSelectedFont: TFont read GetFocusedSelectedFont write SetFocusedSelectedFont;
    property FocusedSelectedFontColor: TAlphaColor read GetFocusedSelectedFontColor write SetFocusedSelectedFontColor;
    property DisabledFill: TBrush read GetDisabledFill write SetDisabledFill;
    property DisabledStroke: TStrokeBrush read GetDisabledStroke write SetDisabledStroke;
    property DisabledFont: TFont read GetDisabledFont write SetDisabledFont;
    property DisabledFontColor: TAlphaColor read GetDisabledFontColor write SetDisabledFontColor;
    property MaxStrokeThickness : single read GetMaxStrokeThickness;
  end;
  TG2Module = class;
  TG2StyleControl = class(TBufferedControl)
  private
    FDefaultStateStyles : TG2StateStyleList;
    [Weak] FStateStyleList : TG2StateStyleList;
    procedure SetStateStyleList(const aValue: TG2StateStyleList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CalcMargin; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property Buffered;

    property StateStyleList : TG2StateStyleList read FStateStyleList write SetStateStyleList stored True;
  end;
  TG2BaseControl = class(TG2StyleControl)
  private
    [Weak] FPathDataList : TG2PathDataList;
    FID,
    FModuleIndex,
    FZOrder,
    FCodeRef,
    FMasterRef,
    FInfoFunc,
    FTextFunc : integer;
    FDependencies : string;
    FSelected : boolean;
    FState : TControlState;
    FLowValue,
    FHighValue,
    FValue,
    FMorphValue : integer;
    FLocation : TLocationType;
    FNormValue,
    FNormMorphValue : single;
    FHasMorph : boolean;
    FMouseEnable : boolean;
    FImageIDS : TStrings;
    FOnGetTextFunc : TGetTextFuncEvent;
    FOnPaintElement : TPaintElementEvent;
  protected

    // Calculates position of shapes within the control, based on unscaled dimensions
    procedure SetUnscaledRect(const Value: TRectF); override;
    procedure SetUnscaledBoundsRect(const Value: TRectF); override;
    procedure SetHighValue(const aValue: integer);
    procedure SetLowValue(const aValue: integer);
    procedure SetNormMorphValue(const aValue: single);
    function GetNormMorphValue: single;
    procedure SetImageIDS(const aValue: TStrings);
    function GetCodeRef : integer;
    procedure SetCodeRef( const aValue : integer);
    function GetValueText( aIndex : integer): string; virtual;
    procedure SetValueText( aIndex : integer; const aValue : string); virtual;
    function GetValue: integer;
    procedure SetValue(const aValue: integer); virtual;
    function GetMorphValue: integer;
    procedure SetMorphValue(const aValue : integer);
    procedure SetNormValue(const aValue: single);
    function GetNormValue: single;
    function GetNormParamValue: single;
    procedure SetSelected(const aValue: boolean); virtual;
    procedure SetState(const aValue: TControlState); virtual;
    procedure SetHasMorph(const aValue: boolean);
    procedure SetButtonText(const aValue: TStrings); virtual;
    procedure SetMouseEnable(const Value: boolean); virtual;
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure ProcessMouseEnter( aTouchID : integer); virtual;
    procedure ProcessMouseLeave( aTouchID : integer); virtual;
    procedure ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); virtual;
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); virtual;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); virtual;
    procedure IncValue;
    procedure DecValue;
    procedure RotateValue;
    procedure IncMorphValue;
    procedure DecMorphValue;
    procedure ParsePanelData( fs : TModuleDefStream); virtual;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; virtual;
    property ID : integer read FID write FID;
    property Location : TLocationType read FLocation write FLocation;
    property ModuleIndex : integer read FModuleIndex write FModuleIndex;
    property CodeRef : integer read GetCodeRef write SetCodeRef;
    property MasterRef : integer read FMasterRef write FMasterRef;
    property InfoFunc : integer read FInfoFunc write FInfoFunc;
    property TextFunc : integer read FTextFunc write FTextFunc;
    property Dependencies : string read FDependencies write FDependencies;
    property Selected : boolean read FSelected write SetSelected;
    property State : TControlState read FState write SetState;
    property NormMorphValue : single read GetNormMorphValue write SetNormMorphValue;
    property LowValue : integer read FLowValue write SetLowValue;
    property HighValue : integer read FHighValue write SetHighValue;
    property NormValue : single read GetNormValue write SetNormValue;
    property Value : integer read GetValue write SetValue;
    property MorphValue : integer read GetMorphValue write SetMorphValue;
    property HasMorph : boolean read FHasMorph write SetHasMorph;
    property PathDataList : TG2PathDataList read FPathDataList write FPathDataList;
    property MouseEnable : boolean read FMouseEnable write SetMouseEnable;
    property OnGetTextFunc : TGetTextFuncEvent read FOnGetTextFunc write FOnGetTextFunc;
    property OnPaintElement : TPaintElementEvent read FOnPaintElement write FOnPaintElement;
  published
    property ImageIDS : TStrings read FImageIDS write SetImageIDS;
  end;
  TG2ReadControl = class( TG2BaseControl, IG2ParamObserver)
  private
    FDataReader : IG2DataParam;
    procedure Update( aG2Event : TG2Event);
  protected
    procedure SetDataReader(const aValue: IG2DataParam);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveReference( aData : IG2Subject);
    property DataReader : IG2DataParam read FDataReader write SetDataReader;
  end;
  TG2MultiReadControl = class(TG2BaseControl, IG2MultiParamObserver)
  private
    FDataDependencies : TList<IG2DataParam>;
    procedure Update( aG2Event : TG2Event);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveReference( aData : IG2Subject);
    procedure AddDataDependency( aData : IG2DataParam; aDeassignEvent : TNotifyEvent);
    procedure ClearDataDependencies;
  end;
  TG2WriteControl = class(TG2BaseControl, IG2ParamObserver)
  private
    FDataWriter : IG2DataParam;
    FOnSelectControl : TNotifyEvent;
    FOnChangeValue : TChangeValueEvent;
    FOnChangeMorphValue : TChangeValueEvent;
    procedure Update( aG2Event : TG2Event);
  protected
    procedure SetDataWriter(const aValue: IG2DataParam);
    procedure RemoveReference( aData : IG2Subject);
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure SetSelected(const aValue: boolean); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetValueDataDependencies( const aValue : byte);
    procedure SetMorphValueDataDependencies( const aValue : byte);
    property DataWriter : IG2DataParam read FDataWriter write SetDataWriter;
    property OnSelectControl : TNotifyEvent read FOnSelectControl write FOnSelectControl;
    property OnChangeValue : TChangeValueEvent read FOnChangeValue write FOnChangeValue;
    property OnChangeMorphValue : TChangeValueEvent read FOnChangeMorphValue write FOnChangeMorphValue;
  end;
  TG2Led = class(TG2BaseControl, IG2LedObserver)
  private
    FDataLed : IG2DataLed;
    FLedLevel : integer;
    FLedGroupId : integer;
    FLedType : TLedType;
    procedure Update( aG2Event : TG2Event);
  protected
    procedure SetDataLed(const aValue: IG2DataLed);
    procedure SetLedLevel( aValue : integer); virtual;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RemoveReference( aData : IG2Subject);
    property DataLed : IG2DataLed read FDataLed write SetDataLed;
    property LedGroupID : integer read FLedGroupID write FLedGroupID;
    property LedType : TLedType read FLedType write FLedType;
    property LedLevel : integer read FLedLevel write SetLedLevel;
    property LegGroupID : integer read FLedGroupID write FLedGroupID;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2LedGreen = class(TG2Led, IG2LedObserver)
  protected
    procedure SetLedLevel( aValue : integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure PaintOn( aCanvas : TCanvas); override;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Scope = class( TG2Led, IG2LedObserver)
  private
    FBuffer : array of single;
    FBufferIndex,
    FBufferSize,
    FMaxBufferSize : integer;
    //FMaxY, FMinY : single;
    FCycles : integer;
    FBufferingActive : boolean;
    procedure SetBufferingActive(const aValue: boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddValue( aValue : single);
    procedure PaintOn( aCanvas : TCanvas); override;
    property BufferingActive : boolean read FBufferingActive write SetBufferingActive;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2MiniVU = class( TG2Led, IG2LedObserver)
  private
    FMiniVUWidth,
    FMiniVUHeight : integer;
    FOrientation  : TOrientationType;
    FLevel        : integer;
    FScope        : TG2Scope;
    procedure SetScope(const Value: TG2Scope);
  protected
    procedure SetLedLevel( aValue : integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    property Scope : TG2Scope read FScope write SetScope;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Label = class(TG2ReadControl)
  private
    FTextAlign : TTextAlign;
    FLabelText : string;
    procedure SetLabelText(const aValue: string);
    procedure SetTextAlign(const aValue: TTextAlign);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property TextAlign : TTextAlign read FTextAlign write SetTextAlign;
    property LabelText : string read FLabelText write SetLabelText;
    property OnPaintElement;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2TextField = class(TG2MultiReadControl)
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property OnGetTextFunc;
    property OnPaintElement;
  end;

  TG2Button = class(TG2WriteControl)
  private
    FButtonText : TStrings;
    FBitmapData : TStringList;
    FOrientation : TOrientationType;
    FButtonKind : TBtnKind;
    FBtnSelected : integer;
    FBorderWidth : single;
    FButtonCount : integer;
    FButtonWidth : single;
    FButtonHeight : single;
    FImageCount : integer;
    FImageWidth : single;
  protected
    procedure SetButtonCount(const aValue: integer);
    procedure SetOrientation(const aValue: TOrientationType); virtual;
    procedure SetButtonHeight(const aValue: single);
    procedure SetButtonWidth(const aValue: single);
    procedure SetButtonKind(const aValue: TBtnKind);
    procedure SetButtonText(const aValue: TStrings); override;
    procedure CalcDimensions; override;
    procedure SetValueText( aIndex : integer; const aValue : string); override;
    procedure SetBtnSelected(const aValue: integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    property Orientation : TOrientationType read FOrientation write SetOrientation;
    property ButtonKind : TBtnKind read FButtonKind write SetButtonKind;
    property ButtonCount : integer read FButtonCount write SetButtonCount;
    property ButtonWidth : single read FButtonWidth write SetButtonWidth;
    property ButtonHeight : single read FButtonHeight write SetButtonHeight;
    property BtnSelected : integer read FBtnSelected write SetBtnSelected;
  published
    property ButtonText : TStrings read FButtonText write SetButtonText;
    property ImageCount : integer read FImageCount write FImageCount;
    property ImageWidth : single read FImageWidth write FImageWidth;
    property MouseEnable;
    property CanFocus;
    property TabOrder;
    property OnMouseUp;
    property OnChangeValue;
    property OnChangeMorphValue;
    property OnPaintElement;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnText = class(TG2Button)
  private
    FButtonType : TButtonTextType;
    procedure SetButtonType(const aValue: TButtonTextType);
  protected
    procedure DoMouseLeave; override;
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure ProcessMouseLeave( aTouchID : integer); override;
    procedure ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  published
    property ButtonType : TButtonTextType read FButtonType write SetButtonType;
    property ButtonKind;
    property OnMouseDown;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnTextEdit = class(TG2BtnText)
  private
    function GetButtonLabel: string;
    procedure SetButtonLabel(const aValue: string);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property ButtonLabel : string read GetButtonLabel write SetButtonLabel;
    property OnMouseDown;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnFlat = class(TG2Button)
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property HighValue;
    property LowValue;
    property Value;
  end;
  TG2BtnArray = class(TG2Button)
  private
    FRectArray : array of TRectF;
    FOnButtonClick : TButtonClickEvent;
  protected
    procedure SetValue(const aValue: integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcDimensionsFromBtnSize( aBtnWidth, aBtnHeight : single); virtual; abstract;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property Value;
    property OnButtonClick : TButtonClickEvent read FOnButtonClick write FOnButtonClick;
    property OnPaintElement;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnRadio = class(TG2BtnArray)
  private
    FUpsideDown : boolean;
  protected
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcDimensionsFromBtnSize( aBtnWidth, aBtnHeight : single); override;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
  published
    property ButtonCount;
    property Orientation;
    property UpsideDown : boolean read FUpsideDown write FUpsideDown;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnRadioEdit = class(TG2BtnArray)
  private
    FButtonColumns : integer;
    FButtonRows : integer;
    procedure SetButtonColumns(const aValue: integer);
    procedure SetButtonRows(const aValue: integer);
    function GetButtonLabel(aIndex: integer): string;
    procedure SetButtonLabel(aIndex: integer; const aValue: string);
  protected
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcDimensionsFromBtnSize( aBtnWidth, aBtnHeight : single); override;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    property ButtonLabel[ index : integer] : string read GetButtonLabel write SetButtonLabel;
  published
    property ButtonColumns : integer read FButtonColumns write SetButtonColumns;
    property ButtonRows : integer read FButtonRows write SetButtonRows;
    property OnDblClick;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2BtnIncDec = class(TG2BtnRadio)
  private
    FUpsideDown : boolean;
  protected
    procedure DoMouseLeave; override;
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ProcessMouseLeave( aTouchID : integer); override;
    procedure ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
  published
    property HighValue;
    property LowValue;
    property UpsideDown : boolean read FUpsideDown write FUpsideDown;
    property OnMouseDown;
  end;
  TG2PartSelector = class;
  TG2PartSelectorList = class( TG2WriteControl)
  private
    [Weak] FPartSelector : TG2PartSelector;
    FRectArray : array of TRectF;
    FOptionSelected : integer;
  protected
    procedure DoMouseLeave; override;
  public
    constructor Create( aPartSelector : TG2PartSelector); reintroduce;
    destructor  Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure ProcessMouseLeave( aTouchID : integer); override;
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2PartSelector = class( TG2WriteControl)
  private
    FBtnRect,
    FDisplayRect : TRectF;
    FOptionsText : TStrings;
    FBitmapData : TStringList;
    FImageWidth  : single;
    FItemCount  : integer;
    FOptionsVisible : boolean;
    FOptionList : TG2PartSelectorList;
    FOptionListEnabled : boolean;
    procedure SetOptionsVisible(const aValue: boolean);
    procedure SetOptionsText(const aValue: TStrings);
    procedure SetOptionListEnabled(const Value: boolean);
  protected
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure ParsePanelData( fs : TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    property OptionsText : TStrings read FOptionsText write SetOptionsText;
    property OptionsVisible : boolean read FOptionsVisible write SetOptionsVisible;
  published
    property MouseEnable;
    property CanFocus;
    property TabOrder;
    property HighValue;
    property LowValue;
    property OptionListEnabled : boolean read FOptionListEnabled write SetOptionListEnabled;
  end;
  TG2ResetButton = class(TBufferedControl)
  private
    FCentered : boolean;
  protected
    procedure CalcDimensions; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
  end;

  TKnobDownPart = (kdpNone, kdpBtnReset, kdpBtnIncDec, kdpKnob);

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Knob = class(TG2WriteControl, IG2ParamObserver)
  private
    FKnobDownPointAbs : TPointF;
    FKnobDownPoint : TPointF;
    FKnobDownNormValue,
    FKnobDownNormMorphValue : single;
    FKnobDownMorph : boolean;
    FKnobDownPart : TKnobDownPart;
    FCX, FCY, FR, FSliderKnobSize : single;
    FSliderRect, FSliderKnobRect : TRectF;
    FKnobType : TKnobType;
    FKnobIncDecBtns : TG2BtnIncDec;
    FDefaultSliderKnobStyles : TG2StateStyleList;
    [Weak] FSliderKnobStyles : TG2StateStyleList;
    FMorphColor : TAlphaColor;
    FKnobReset : TG2ResetButton;
    FOrientation : TOrientationType;
    FKnobControl : TKnobControl;
    procedure SetKnobType(const aValue: TKnobType);
    procedure SetOrientation(const aValue: TOrientationType);
    procedure SetSliderKnobSize(const aValue: single);
    procedure SetMorphColor(const aValue: TAlphaColor);
    function GetSliderKnobStyles: TG2StateStyleList;
    procedure SetSliderKnobStyles(const aValue: TG2StateStyleList);
    function CalcSliderKnobRect: TRectF;
  protected
    procedure DoMouseEnter; override;
    procedure DoMouseLeave; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure SetSelected(const aValue: boolean); override;
    procedure SetState(const aValue: TControlState); override;
    procedure SetBuffered(const Value: boolean); override;
    procedure SetZoom(const Value: single); override;
    procedure CalcDimensions; override;
{$IF Defined(VER280) or Defined(VER340)}
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
{$ELSE}
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
{$ENDIF}
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Redraw; override;
    procedure SetDefaultDimensions;
    procedure ParsePanelData(fs: TModuleDefStream); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure ProcessMouseLeave( aTouchID : integer); override;
    procedure ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
  published
    property Width;
    property Height;
    property Position;
    property CanFocus;
    property TabOrder;
    property OnPaintElement;
    property KnobType : TKnobType read FKnobType write SetKnobType;
    property SliderKnobSize : single read FSliderKnobSize write SetSliderKnobSize;
    property Orientation : TOrientationType read FOrientation write SetOrientation;
    property KnobControl : TKnobControl read FKnobControl write FKnobControl;
    property SliderKnobStyles : TG2StateStyleList read GetSliderKnobStyles write SetSliderKnobStyles;
    property IncDecBtns : TG2BtnIncDec read FKnobIncDecBtns;
    property MouseEnable;
    property HighValue;
    property LowValue;
    property Value;
    property MorphValue;
    property AutoCapture;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnChangeValue;
    property OnChangeMorphValue;
  end;

  TG2Cable = class;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Connector = class(TG2WriteControl)
  private
    FHoleRect : TRectF;
    FConnectorIndex : integer;
    FConnectorKind : TConnectorKind;
    FConnectorType : TConnectorType;
    FBandWidth : TBandWidthType;
    FConnectorColor : TAlphaColor;
    FConnected : boolean;
    FOnCreateCable : TCreateCableEvent;
    procedure SetConnected(const aValue: boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    function ParseProperties( fs: TModuleDefStream; aName : string): boolean; override;
    property ConnectorColor : TAlphaColor read FConnectorColor write FConnectorColor;
    property ConnectorKind : TConnectorKind read FConnectorKind write FConnectorKind;
    property ConnectorType : TConnectorType read FConnectorType write FConnectorTYpe;
    property BandWidth : TBandWidthType read FBandWidth write FBandWidth;
    property OnCreateCable : TCreateCableEvent read FOnCreateCable write FOnCreateCable;
  published
    property Connected : boolean read FConnected write SetConnected;
    property CanFocus;
    property TabOrder;
    property OnPaintElement;
  end;
  TCableStyle = ( csFlat, csGradient);
  TNode = class
    x  : single;
    y  : single;
    vx : single;
    vy : single;
  end;
  TG2CableNode = class(TBufferedControl)
  private
    [Weak] FCable : TG2Cable;
    x, x2  : single;
    y, y2  : single;
    vx : single;
    vy : single;
    PStart, PEnd : TPointF;
    FFill : TBrush;
    FPolygon : TPolygon;
    FStartNode,
    FEndNode : boolean;
  public
    constructor Create(aCable: TG2Cable); reintroduce;
    destructor Destroy; override;
    procedure CalcPath;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure SetVisible(const aValue : boolean); override;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Cable = class(TBufferedControl)
  private
    //FParent : TFmxObject;
    FP1, FP2 : TPointF;
    FColor : TAlphaColor;
    FCableStyle : TCableStyle;
    FNodeCount : integer;
    FSegmentSize : integer;
    FNodes : array of TG2CableNode;
    FMargin : single;
    FVisible : boolean;
    //FBuffered : boolean;
    FTimer : TTimer;
    FTimerCount : integer;
    procedure SetCableStyle(const Value: TCableStyle);
    procedure SetBuffered(const Value: boolean);
  protected
    procedure SetVisible(const aValue: boolean);
    //function GetParent: TFmxObject;
    //procedure SetParent(const Value: TFmxObject);
    procedure SetPoint1X(const Value : single);
    procedure SetPoint1Y(const Value : single);
    procedure SetPoint2X(const Value : single);
    procedure SetPoint2Y(const Value : single);
    procedure SetColor(const Value : TAlphaColor);
    procedure IterateCable;
    procedure OnTimer(Sender: TObject);
    procedure ClearNodes;
    procedure AddNodes( aNodeCount : integer);
    procedure CalcNodePaths;
    procedure SetZoom(const Value: single); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure StartTimer;
    procedure InitCable;
    procedure CalcCaterny;
  published
    //property Parent : TFmxObject read GetParent write SetParent;
    property Color : TAlphaColor read FColor write SetColor;
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
    property CableStyle : TCableStyle read FCableStyle write SetCableStyle;
    property SegmentSize : integer read FSegmentSize write FSegmentSize;
    property Visible : boolean read FVisible write SetVisible;
    property Buffered : boolean read FBuffered write SetBuffered;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2Module = class(TG2StyleControl)
  private
    FLocation : TLocationType;
    FModuleIndex : integer;
    FModuleLabel : string;
    FFont : TFont;
    FFill : TBrush;
    FStroke : TBrush;
    FSelected : boolean;
    FFocusedControl : IControl;
    FCol: integer;
    FRow: integer;
    FHeightUnits : integer;
    procedure SetHeightUnits(const Value: integer);
  protected
    procedure SetModuleLabel(const aValue: string);
    procedure SetCol(const aValue: integer); virtual;
    procedure SetRow(const aValue: integer); virtual;
    function GetSelected: boolean;
    procedure SetSelected( const aValue : boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure PaintOn( aCanvas : TCanvas); override;
  published
    property Visible;
    property ModuleIndex : integer read FModuleIndex write FModuleIndex;
    property Location : TLocationType read FLocation write FLocation;
    property Selected : boolean read GetSelected write SetSelected;
    property Fill : TBrush read FFill;
    property Stroke : TBrush read FStroke;
    property ModuleLabel : string read FModuleLabel write SetModuleLabel;
    property Row : integer read FRow write SetRow;
    property Col : integer read FCol write SetCol;
    property HeightUnits : integer read FHeightUnits write SetHeightUnits;
  end;

  {$IF Defined(VER340)}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or pidOSX64 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ELSE}
  [ComponentPlatformsAttribute( PidWin32 Or PidWin64 Or PidOSX32 Or PidiOSDevice Or PidiOSSimulator or pidAndroid)]
  {$ENDIF}
  TG2KeyBoard = class(TG2BaseControl)
  private
    FWhiteKeyWidth,
    FWhiteKeyLength,
    FBlackKeyWidth,
    FBlackKeyLength,
    FOctaveButtonWidth,
    FOctaveButtonHeight : single;
    FOrientation :  TKeyboardOrientation;
    FKeyTouchID : array[0..MAX_KEYS] of integer;
    FKeyUpdate : array[0..MAX_KEYS] of integer;
    FOctaveButtonValue : array[0..MAX_KEYS div 12 + 1] of integer;
    FOctaveUpdate : array[0..MAX_KEYS div 12 + 1] of integer;
    FKeyType1, FKeyType2, FKeyType3 : TPathData;
    FLowKey, FHighKey,
    FMonoKeyTouchID,
    FSelectedLowKey, FSelectedHighKey : integer;
    FState : TControlState;
    FRangeSelect : boolean;
    FOctaveButtons : boolean;
    FMouseEnable : boolean;
    FDownColor : TAlphaColor;
    FMonoKey : boolean;
    FOnKeyboardKeyDown : TKeyboardKeyDownEvent;
    FOnKeyboardKeyUp : TKeyboardKeyUpEvent;
    FOnOctaveButtonChange : TOctaveButtonChangeEvent;
    FOnDebug : TDebugEvent;
    procedure SetHighKey(const Value: integer);
    procedure SetLowKey(const Value: integer);
    procedure SetOrientation(const Value: TKeyboardOrientation);
    procedure SetSelectedHighKey(const Value: integer);
    procedure SetSelectedLowKey(const Value: integer);
    procedure SetState(const Value: TControlState);
    procedure SetOctaveButtons(const Value: boolean);
    procedure CalcKeyDimensions;
    procedure InitKeyTouchIDS;
    procedure InitOctaveButtonValues;
    procedure SetMouseEnable(const Value: boolean);
  protected
    procedure DoMouseLeave; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resize; override;
    procedure PaintOn( aCanvas : TCanvas); override;
    procedure GetKeyType(const aKey : integer; var aOctave, aKeyID : integer; var aKeyType : TKeyType);
    function  GetKeyRect(const aKey : integer): TRectF;
    function  GetOctaveRect(const aOctave : integer): TRectF;
    procedure PaintKey( aCanvas : TCanvas; const aKey : integer);
    procedure PaintOctaveButton( aCanvas : TCanvas; const aOctave : integer);
    procedure ProcessMouseLeave( aTouchID : integer); override;
    procedure ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseMove(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    property MonoKey : boolean read FMonoKey write FMonoKey;
  published
    property LowKey : integer read FLowKey write SetLowKey;
    property HighKey : integer read FHighKey write SetHighKey;
    property SelectedLowKey : integer read FSelectedLowKey write SetSelectedLowKey;
    property SelectedHighKey : integer read FSelectedHighKey write SetSelectedHighKey;
    property Orientation : TKeyboardOrientation read FOrientation write SetOrientation;
    property OctaveButtons : boolean read FOctaveButtons write SetOctaveButtons;
    property RangeSelect : boolean read FRangeSelect write FRangeSelect;
    property State : TControlState read FState write SetState;
    property DownColor : TAlphaColor read FDownColor write FDownColor;
    property MouseEnable : boolean read FMouseEnable write SetMouseEnable;
    property Align;
    property Position;
    property Width;
    property Height;
    property CanFocus;
    property Hittest;
    property TabOrder;
    property OnKeyboardKeyDown : TKeyboardKeyDownEvent read FOnKeyboardKeyDown write FOnKeyboardKeyDown;
    property OnKeyboardKeyUp : TKeyboardKeyUpEvent read FOnKeyboardKeyUp write FOnKeyboardKeyUp;
    property OnOctaveButtonChange : TOctaveButtonChangeEvent read FOnOctaveButtonChange write FOnOctaveButtonChange;
  end;
  function IndentSpaces(const aValue : integer): string;
  function ControlStateName(const aControlState : TControlState): string;
  function FontStyleName(const aFontStyles : TFontStyles): string;
  function BrushKindName(const aBrushKind : TBrushKind): string;
  function CssColorName(const aAlphaColor : TAlphaColor): string;
implementation
function ConvertToAlpha( aColor : integer): TAlphaColor;
begin
  Result := $ff000000
          + (Cardinal(aColor) and $000000ff) shl 16
          + (Cardinal(aColor) and $0000ff00)
          + (Cardinal(aColor) and $00ff0000) shr 16;
end;
function Darker(c : TAlphaColor; f : byte): TColor;
var A, R, G, B : byte;
  function sub( comp : byte): byte;
  begin
    if comp - f > 0 then
      result := comp - f
    else
      result := 0;
  end;
begin
  A := (c and $ff000000) shr 24;
  R := (c and $000000ff);
  G := (c and $0000ff00) shr 8;
  B := (c and $00ff0000) shr 16;
  result := A * 256*256*256
          + sub(B) * 256*256
          + sub(G) * 256
          + sub(R);
end;

function ArcTan2( V : TPointF): single;
begin
  if abs(V.X) < 0.000000001 then begin //Close to zero! Declare it to be zero!
    //The small margin gives a slight error, but improves reliability.
    if V.y > 0 then begin
      Result := PI / 2; //90 degrees
    end else begin
      Result := PI / (-2); //-90 degrees
    end;
  end else begin
    if V.X > 0 then begin // 1. or 4. quadrant
      Result := ArcTan(V.Y / V.X); // Easy stuff. Normal ArcTan is valid for 1. and 4.
    end else begin // 2. or 3. quadrant
      Result := PI - ArcTan(-V.Y / V.X);
    end;
  end;
end;
{$IF Defined(VER260) or Defined(VER270) or Defined(VER280)}
function CreateScaleMatrix( sx, sy : single): TMatrix;
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
function CreateTranslateMatrix( tx, ty : single): TMatrix;
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
function IndentSpaces(const aValue : integer): string;
var i : integer;
begin
  Result := '';
  for i := 1  to aValue do
    Result := Result + ' ';
end;
function ControlStateName(const aControlState : TControlState): string;
begin
  Result := '';
  case aControlState of
    csDefault : Result := 'Default';
    csFocused : Result := 'Focused';
    csSelected : Result := 'Selected';
    csFocusedSelected : Result := 'FocusedSelected';
    csDisabled : Result := 'Disabled';
  end;
end;
function FontStyleName(const aFontStyles : TFontStyles): string;
begin
  // TODO make set
  Result := '';
  if TFontStyle.fsBold in aFontStyles then
    Result := 'Bold'
  else
    if TFontStyle.fsItalic in aFontStyles then
      Result := 'Italic'
    else
      if TFontStyle.fsUnderline in aFontStyles then
        Result := 'Underline';
end;
function BrushKindName(const aBrushKind : TBrushKind): string;
begin
  Result := '';
  case aBrushKind of
    TBrushKind.bkNone : Result := 'None';
    TBrushKind.bkSolid: Result := 'Solid';
    TBrushKind.bkGradient: ;
    TBrushKind.bkBitmap: ;
    TBrushKind.bkResource: ;
  end;
end;
function CssColorName(const aAlphaColor : TAlphaColor): string;
var r, g, b, a : integer;
begin
  a := (aAlphaColor and $FF000000) shr 24;
  b := (aAlphaColor and $000000FF);
  g := (aAlphaColor and $0000FF00) shr 8;
  r := (aAlphaColor and $00FF0000) shr 16;
  Result := '#' + IntToHex(a, 2) + IntToHex(r, 2) + IntToHex(g, 2) + IntToHex(b, 2);
end;

//------------------------------------------------------------------------------
//
//                               TBufferedControl
//
//------------------------------------------------------------------------------
constructor TBufferedControl.Create(AOwner: TComponent);
var
  d: Integer;
begin
  inherited;

  if Name = 'rbSynth' then
    d := 1;

  FG2Updating := False;
  FBuffered := False;
  FBitmap := nil;
  FRedraw := True;
  FZoom := 1;
  FUnscaledPosition := TPosition.Create( PointF(Position.X, Position.Y));
  //FUnscaledWidth := Width;
  //FUnscaledHeight := Height;
end;
destructor TBufferedControl.Destroy;
begin
  if assigned(FBitmap) then
    FBitmap.Free;
  FUnscaledPosition.Free;
  inherited;
end;
procedure TBufferedControl.Loaded;
var i : integer;
  d: Integer;
begin
  inherited;

  if Name = 'rbSynth' then
    d := 1;

  CalcUnscaledBounds(Position.X, Position.Y, Width, Height);
  if FBuffered then begin
    CalcZoomedBounds;
    //Redraw;
  end;
end;
function TBufferedControl.GetBuffered: boolean;
begin
  Result := FBuffered;
end;
function TBufferedControl.GetControlRect: TRectF;
begin
  Result := RectF(Position.X, Position.Y, Position.X + Width, Position.Y + Height);
end;
function TBufferedControl.GetUnscaledBoundsRect: TRectF;
begin
  Result := RectF(0, 0, FUnscaledWidth, FUnscaledHeight);
end;
function TBufferedControl.GetUnscaledHeight: single;
begin
  Result := FUnscaledHeight;
end;
function TBufferedControl.GetUnscaledLeft: single;
begin
  Result := FUnscaledPosition.X;
end;
function TBufferedControl.GetUnscaledRect: TRectF;
begin
  Result := RectF( FUnscaledPosition.X,
                   FUnscaledPosition.Y,
                   FUnscaledPosition.X + FUnscaledWidth,
                   FUnscaledPosition.Y + FUnscaledHeight);
end;
function TBufferedControl.GetUnscaledTop: single;
begin
  Result := FUnscaledPosition.Y;
end;
function TBufferedControl.GetUnscaledWidth: single;
begin
  Result := FUnscaledWidth;
end;
procedure TBufferedControl.InitBitmap;
var ar : TRectF;
    w, h : integer;
begin
  CalcMargin;
  ar := AbsoluteRect;
  //w := trunc((UnscaledWidth+FMargin*2)*FZoom);
  //h := trunc((UnscaledHeight+FMargin*2)*FZoom);
  w := trunc((ar.Right - ar.Left) + FMargin * 2 * FZoom)+1;
  h := trunc((ar.Bottom - ar.Top) + FMargin * 2 * FZoom)+1;
  if not assigned(FBitmap) then begin
    FBitmap := TBitmap.Create( w, h);
    FBitmap.Clear( $000000FF);
    WriteLog('Create bitmap w:' + IntToStr(w) + ', h:' + IntToStr(h));
  end else
    if (FBitmap.Width <> w) or (FBitmap.Height <> h) then begin
      FBitmap.Resize( w, h);
      FBitmap.Clear( $000000FF);
      WriteLog('Resize bitmap w:' + IntToStr(w) + ', h:' + IntToStr(h));
    end;
end;
procedure TBufferedControl.Paint;
var SaveState: TCanvasSaveState;
    SaveMatrix, M : TMatrix;
    ScaledMargin, w, h : single;
    ar, sr, dr, ur, crs, crd, ics, icd : TRectF;
    ap : TPointF;
begin
  if FG2Updating then
    exit;
  WriteLog('Paint');
  if FBuffered then begin
    CalcMargin;
    ScaledMargin := FMargin*FZoom;
    InitBitmap;
    if FRedraw then begin
      FRedraw := False;
      FBitmap.Canvas.BeginScene;
      try
        // Draw scaled on bitmap
        {$IF Defined(VER340)}
        M := TMatrix.CreateTranslation( FMargin, FMargin)
           * TMatrix.CreateScaling( FZoom, FZoom);
        {$ELSE}
        M := Matrixmultiply(CreateTranslateMatrix( FMargin, FMargin),
                 CreateScaleMatrix( FZoom, FZoom));
        {$IFEND}
        FBitmap.Canvas.SetMatrix( M);
        PaintOn( FBitmap.Canvas);
        WriteLog('PaintOn');
      finally
        FBitmap.Canvas.EndScene;
      end;
    end;
    // Draw clipped bitmap on canvas
    SaveMatrix := Canvas.Matrix;
    Canvas.BeginScene;
    try
      // Update rect is in absolute coords
      ur := UpdateRect;

      ap := LocalToAbsolute(PointF( 0.0, 0.0));
      ar := AbsoluteRect;
      // Clipped source rect
      crs := RectF( - ap.X + ur.Left,
                    - ap.Y + ur.Top,
                    - ap.X + ur.Right + 2*ScaledMargin,
                    - ap.Y + ur.Bottom + 2*ScaledMargin);
      // Clipt destination rect
      crd := RectF( - ap.X - ScaledMargin + ur.Left,
                    - ap.Y - ScaledMargin + ur.Top,
                    - ap.X - ScaledMargin + ur.Right + 2*ScaledMargin,
                    - ap.Y - ScaledMargin + ur.Bottom + 2*ScaledMargin);
      w := (ar.Right - ar.Left) + 2*ScaledMargin;
      h := (ar.Bottom - ar.Top) + 2*ScaledMargin;
      sr := RectF(0, 0, w, h);
      dr := RectF(-ScaledMargin,
                  -ScaledMargin,
                  w - ScaledMargin,
                  h - ScaledMargin);

      // Eliminate zooming (rotation not supported)
      {$IF Defined(VER340)}
      M := TMatrix.CreateTranslation( ap.x, ap.y);
      {$ELSE}
      M := CreateTranslateMatrix( ap.x, ap.y);
      {$ENDIF}
      Canvas.SetMatrix( M);
      ics := TRectF.Intersect( crs, sr);
      icd := TRectF.Intersect( crd, dr);
      Canvas.DrawBitmap( FBitmap,
          ics, //TRectF.Intersect( crs, sr),
          icd, //TRectF.Intersect( crd, dr),
          AbsoluteOpacity,
          False);
      WriteLog('DrawBitmap');
    finally
      Canvas.SetMatrix( SaveMatrix);
      Canvas.EndScene;
    end;
  end else begin
    SaveState := Canvas.SaveState;
    Canvas.BeginScene;
    try
      PaintOn( Canvas);
    finally
      Canvas.RestoreState(SaveState);
      Canvas.EndScene;
    end;
  end;
end;
procedure TBufferedControl.PaintOn(aCanvas: TCanvas);
begin
  // Abstract
end;
procedure TBufferedControl.Redraw;
var i : integer;
    ar, sr : TRectF;
    SiblingControl : TBufferedControl;
begin
  if FBuffered then begin
    {if assigned(ParentControl) and (ParentControl is TBufferedControl)
      and (ParentControl as TBufferedControl).Buffered then begin
      // Child of buffered control
      if not FRedraw then begin
        FRedraw := True;
        WriteLog('Redraw!');
        // Redraw overlapping siblings also
        ar := AbsoluteRect;
        ar.Inflate(-FMargin, -FMargin);
        for i := 0 to ParentControl.ControlsCount - 1 do begin
          if (ParentControl.Controls[i] is TBufferedControl) then begin
            SiblingControl := ParentControl.Controls[i] as TBufferedControl;
            sr := SiblingControl.AbsoluteRect;
            sr.Inflate(-SiblingControl.FMargin, -SiblingControl.FMargin);
            if ar.IntersectsWith(sr) then begin
              if (ParentControl.Controls[i] is TBufferedControl) then
                (ParentControl.Controls[i] as TBufferedControl).Redraw;
            end;
          end;
        end;
        //ParentControl.RePaint;
        Repaint;
      end;
    end else}
      if not FRedraw then begin
        FRedraw := True;
        WriteLog('Redraw!');
        RePaint;
      end;
  end else begin
    // Not buffered
    FRedraw := True;
    WriteLog('Redraw!');
    RePaint;
  end;
end;
procedure TBufferedControl.Resize;
var ScaledMargin : single;
  d: Integer;
begin
  if Name = 'rbSynth' then
    d := 1;

  if FBuffered then begin
    InitBitmap;
    FRedraw := True;
  end;
  inherited;
  CalcUnscaledBounds( Position.X, Position.Y, Width, Height);
  WriteLog('Resize w:' + IntToStr(trunc(Width)) + ', h:' + IntToStr(trunc(Height)));
end;
procedure TBufferedControl.SetBounds(X, Y, AWidth, AHeight: Single);
var
  d: Integer;
begin
  if Name = 'rbSynth' then
    d := 1;

  CalcUnscaledBounds(X, Y, AWidth, AHeight);
  inherited;
end;
procedure TBufferedControl.DrawTextExt(aCanvas: TCanvas; const aFont: TFont;
  const aTopLeft, aMaxSize: TPointF; const aColor: TAlphaColor;
  const aText: string; const aHorzAlign, aVertAlign: TTextAlign);
var TextLayout : TTextLayout;
{$IFDEF ANDROID_IOS}
    SaveMatrix : TMatrix;
    f : single;
{$ENDIF}
begin
{$IFDEF ANDROID_IOS}
  // Fix to prevent ugly bitmap scaling of fonts in Android/iOS
  TextLayout :=  TTextLayoutManager.DefaultTextLayout.Create;
  SaveMatrix := aCanvas.Matrix;
  try
    f := SaveMatrix.m11;
    if f = 0 then
      exit;
    aCanvas.SetMatrix(MatrixMultiply(CreateScaleMatrix(1/f,1/f), SaveMatrix));
    TextLayout.BeginUpdate;
    TextLayout.Font.Assign( aFont);
    TextLayout.Font.Size := aFont.Size * f;
    TextLayout.TopLeft := PointF(aTopLeft.X*f, aTopLeft.Y*f);
    TextLayout.MaxSize := PointF(aMaxSize.X*f, aMaxSize.Y*f);
    TextLayout.Color := aColor;
    TextLayout.Text := aText;
    TextLayout.HorizontalAlign := aHorzAlign;
    TextLayout.VerticalAlign := aVertAlign;
    TextLayout.EndUpdate;
    TextLayout.RenderLayout( aCanvas);
  finally
    aCanvas.SetMatrix(SaveMatrix);
    TextLayout.Free;
  end;
{$ELSE}
  TextLayout :=  TTextLayoutManager.DefaultTextLayout.Create;
  try
    TextLayout.BeginUpdate;
    TextLayout.Font.Assign( aFont);
    TextLayout.TopLeft := aTopLeft;
    TextLayout.MaxSize := aMaxSize;
    TextLayout.Color := aColor;
    TextLayout.Text := aText;
    TextLayout.HorizontalAlign := aHorzAlign;
    TextLayout.VerticalAlign := aVertAlign;
    TextLayout.EndUpdate;
    TextLayout.RenderLayout( aCanvas);
  finally
    TextLayout.Free;
  end;
{$ENDIF}
end;
{$IF Defined(VER280) or Defined(VER340)}
procedure TBufferedControl.DoBeginUpdate;
begin
  inherited;
  CalcUnscaledBounds(Position.X, Position.Y, Width, Height);
  FG2Updating := True;
end;
procedure TBufferedControl.DoEndUpdate;
begin
  FG2Updating := False;
  CalcDimensions;
  CalcZoomedBounds;
  inherited;
end;
{$ELSE}
procedure TBufferedControl.BeginUpdate;
begin
  inherited;
  CalcUnscaledBounds(Position.X, Position.Y, Width, Height);
  FG2Updating := True;
end;
procedure TBufferedControl.EndUpdate;
begin
  FG2Updating := False;
  CalcDimensions;
  CalcZoomedBounds;
  inherited;
end;
{$ENDIF}
procedure TBufferedControl.SetParent(const Value: TFmxObject);
begin
  inherited;
  if Value is TG2BufferedLayout then begin
    Buffered := (Value as TG2BufferedLayout).Buffered;
    Zoom := (Value as TG2BufferedLayout).Zoom;
  end else
    if Value is TBufferedControl then begin
      Buffered := (Value as TBufferedControl).Buffered;
      Zoom := (Value as TBufferedControl).Zoom;
    end;
  CalcUnscaledBounds( Position.X, Position.Y, Width, Height);
end;
procedure TBufferedControl.SetUnscaledBoundsRect(const Value: TRectF);
begin
  if (FUnscaledWidth <> Value.Right - Value.Left) or
     (FUnscaledHeight <> Value.Bottom - Value.Top) then begin
    FUnscaledWidth := Value.Right - Value.Left;
    FUnscaledHeight := Value.Bottom - Value.Top;
    CalcZoomedBounds;
  end;
end;
procedure TBufferedControl.SetUnscaledHeight(const Value: single);
var z : single;
begin
  if Value <> FUnscaledHeight then begin
    FUnscaledHeight := Value;
    //z := FZoom;
    z := 1;
    Height := FUnscaledHeight * z;
  end;
end;
procedure TBufferedControl.SetUnscaledLeft(const Value: single);
var z : single;
begin
  if Value <> FUnscaledPosition.X then begin
    FUnscaledPosition.X := Value;
    //z := FZoom;
    z := 1;
    Position.X := FUnscaledPosition.X * z;
  end;
end;
procedure TBufferedControl.SetUnscaledPosition(const Value: TPosition);
begin
  FUnscaledPosition.Assign( Value);
end;
procedure TBufferedControl.CalcDimensions;
begin
  //
end;
procedure TBufferedControl.CalcMargin;
begin
  // Whole numbers otherwise canvas gets distorted
  {$IF Defined(VER340)}
  FMargin := (Canvas.Stroke.Thickness+1)*2;
  {$ELSE}
  FMargin := (Canvas.StrokeThickness+1)*2;
  {$ENDIF}
end;
procedure TBufferedControl.CalcUnscaledBounds(const X, Y, w, h : single);
var z : single;
begin
  if FZoom = 0 then
    z := 1
  else
    z := 1;//FZoom;
  FUnscaledPosition.X := X / z;
  FUnscaledPosition.Y := Y / z;
  FUnscaledWidth := w / z;
  FUnscaledHeight := h / z;
end;
procedure TBufferedControl.CalcZoomedBounds;
var z : single;
begin
  //z := FZoom;
  z := 1;
  BoundsRect := RectF(FUnscaledPosition.X * z, FUnscaledPosition.Y * z,
                      FUnscaledPosition.X * z + FUnscaledWidth * z,
                      FUnscaledPosition.Y * z + FUnscaledHeight * z);
end;

procedure TBufferedControl.SetUnscaledRect(const Value: TRectF);
begin
  FUnscaledPosition.X := Value.Left;
  FUnscaledPosition.Y := Value.Top;
  FUnscaledWidth := Value.Right - Value.Left;
  FUnscaledHeight := Value.Bottom - Value.Top;
  CalcZoomedBounds;
end;
procedure TBufferedControl.SetUnscaledTop(const Value: single);
var z : single;
begin
  if Value <> FUnscaledPosition.Y then begin
    FUnscaledPosition.Y := Value;
    //z := FZoom;
    z := 1;
    Position.Y := FUnscaledPosition.Y * z;
  end;
end;
procedure TBufferedControl.SetUnscaledWdith(const Value: single);
var z : single;
begin
  Exit;
  if Value <> FUnscaledWidth then begin
    FUnscaledWidth := Value;
    //z := FZoom;
    z := 1;
    Width := FUnscaledWidth * z;
  end;
end;
procedure TBufferedControl.SetBuffered(const Value: boolean);
var i : integer;
begin
  if FBuffered <> Value then begin
    FBuffered := Value;
    if FBuffered and not(csLoading in ComponentState) then
    begin
      CalcZoomedBounds;
      Redraw;
    end;
  end;
end;
procedure TBufferedControl.SetLog(const Value: TStrings);
begin
  if FLog <> Value then begin
    FLog := Value;
  end;
end;
procedure TBufferedControl.SetZoom(const Value: single);
var i : integer;
begin
  if FZoom <> Value then begin
    FZoom := Value;
    for i := 0 to ChildrenCount - 1 do begin
      if Children[i] is TBufferedControl then begin
        (Children[i] as TBufferedControl).Zoom := FZoom;
      end;
    end;
    if FBuffered then begin
      CalcZoomedBounds;
      Redraw;
    end;
  end;
end;
procedure TBufferedControl.WriteLog(const aText: string);
begin
  if assigned(FLog) then begin
    FLog.Add(Name + ' ' + aText);
  end;
end;
//------------------------------------------------------------------------------
//
//                            TG2BufferedLayout
//
//------------------------------------------------------------------------------
constructor TG2BufferedLayout.Create(AOwner: TComponent);
begin
  inherited;
  FBuffered := False;
  FZoom := 1;
  Hittest := False;
end;
destructor TG2BufferedLayout.Destroy;
begin
  inherited;
end;
procedure TG2BufferedLayout.SetBuffered(const Value: boolean);
var i : integer;
begin
  if FBuffered <> Value then begin
    FBuffered := Value;
  end;
end;
procedure TG2BufferedLayout.SetZoom(aValue: single);
var i : integer;
begin
  if aValue <> FZoom then begin
    FZoom := aValue;
    for i := 0 to ChildrenCount - 1 do begin
      if Children[i] is TBufferedControl then begin
        (Children[i] as TBufferedControl).Zoom := FZoom;
      end;
    end;
    if not FBuffered then begin
      Scale.X := FZoom;
      Scale.Y := FZoom;
    end;
  end;
end;
//------------------------------------------------------------------------------
//
//                             TArrayLayout
//
//------------------------------------------------------------------------------
constructor TArrayLayout.Create(AOwner: TComponent);
begin
  inherited;
  FRows := 1;
  FColumns := 1;
end;
destructor TArrayLayout.Destroy;
begin
  inherited;
end;
procedure TArrayLayout.RealignChildren;
var i, c, r : integer;
    w, h : single;
begin
  if (FRows = 0) or (FColumns = 0) then
    exit;
  r := 0;
  c := 0;
  i := 0;
  w := Width / FColumns;
  h := Height / FRows;
  for r := 0 to FRows - 1 do begin
    for c := 0 to FColumns - 1 do begin
      while (i < ChildrenCount) and not(Children[i] is TControl) do
        inc(i);
      if (i < ChildrenCount) then begin
        (Children[i] as TControl).SetBounds(c*w, r*h, w, h);
      end;
      inc(i);
    end;
  end;
end;
procedure TArrayLayout.Resize;
begin
  inherited;
  RealignChildren;
end;
procedure TArrayLayout.SetColumns(const Value: integer);
begin
  if FColumns <> Value then begin
    FColumns := Value;
    RealignChildren;
  end;
end;
procedure TArrayLayout.SetRows(const Value: integer);
begin
  if FRows <> Value then begin
    FRows := Value;
    RealignChildren;
  end;
end;
//------------------------------------------------------------------------------
//
//                             TG2StyleControl
//
//------------------------------------------------------------------------------
constructor TG2StyleControl.Create( AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultStateStyles := TG2StateStyleList.Create(self);
  FDefaultStateStyles.Name := 'DefaultStyles';
  FDefaultStateStyles.SetSubComponent(True);
  FStateStyleList := FDefaultStateStyles;
end;
destructor TG2StyleControl.Destroy;
begin
  FDefaultStateStyles.Free;
  inherited;
end;
procedure TG2StyleControl.CalcMargin;
begin
  if assigned(FStateStyleList) then
    // Whole numbers otherwise bitmap gets distorted
    //FMargin := FStateStyleList.MaxStrokeThickness*1.5
    FMargin := trunc(FStateStyleList.MaxStrokeThickness + 1)*2
  else
    inherited;
end;
procedure TG2StyleControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent is TG2StateStyleList)
    and (AComponent <> FDefaultStateStyles)  then
    StateStyleList := nil;
  inherited;
end;
procedure TG2StyleControl.SetStateStyleList(const aValue: TG2StateStyleList);
begin
  if aValue <> FStateStyleList then begin
    if assigned(aValue) then begin
      if assigned(FStateStyleList) and (FStateStyleList <> FDefaultStateStyles) then
        FStateStyleList.RemoveFreeNotification(self);
      FStateStyleList := aValue;
      if FStateStyleList <> FDefaultStateStyles then
        FStateStyleList.FreeNotification(self);
    end else begin
      if assigned(FStateStyleList) and (FStateStyleList <> FDefaultStateStyles) then
        FStateStyleList.RemoveFreeNotification(self);
      FStateStyleList := FDefaultStateStyles;
    end;
    Redraw;
  end;
end;

//------------------------------------------------------------------------------
//
//                               TG2BaseControl
//
//------------------------------------------------------------------------------
constructor TG2BaseControl.Create( AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hittest := False;
  FModuleIndex := -1;
  FID := -1;
  FZOrder := 0;
  FCodeRef := -1;
  FMasterRef := -1;
  FTextFunc := -1;
  FInfoFunc := -1;
  FSelected := False;
  FState := csDefault;
  FMouseEnable := True;
  FPathDataList := BVE.NMG2Pathdata.PathDataList;
  FImageIDS := TStringList.Create;
  FValue := 0;
  FNormValue := 0;
  FMorphValue := 0;
  FNormMorphValue := 0;
end;
destructor TG2BaseControl.Destroy;
begin
  //Selected := False; Problems when destroying app.
  FImageIDS.Free;
  inherited;
end;
procedure TG2BaseControl.DecMorphValue;
begin
end;
procedure TG2BaseControl.DecValue;
begin
  if FValue > FLowValue then begin
    FValue := FValue - 1;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.IncMorphValue;
begin
end;
procedure TG2BaseControl.IncValue;
begin
  if FValue < FHighValue then begin
    FValue := FValue + 1;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.CalcDimensions;
begin
  //
end;
procedure TG2BaseControl.SetUnscaledBoundsRect(const Value: TRectF);
begin
  inherited;
  CalcDimensions;
end;
procedure TG2BaseControl.SetUnscaledRect(const Value: TRectF);
begin
  inherited;
  CalcDimensions;
end;
procedure TG2BaseControl.Resize;
begin
  if FG2Updating then
    exit;
  inherited;
  CalcDimensions;
end;
procedure TG2BaseControl.RotateValue;
begin
  if FValue < FHighValue then begin
    FValue := FValue + 1;
    FNormValue := GetNormParamValue;
    Redraw;
  end else begin
    FValue := FLowValue;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
function TG2BaseControl.GetCodeRef: integer;
begin
  Result := FCodeRef;
end;
function TG2BaseControl.GetMorphValue: integer;
begin
  Result := FMorphValue;
end;
function TG2BaseControl.GetNormMorphValue: single;
var h, l, v : integer;
begin
  h := FHighValue;
  l := FLowValue;
  if FMorphValue > 127 then
    v := FMorphValue - 256
  else
    v := FMorphValue;
  if (h-l) <> 0 then
    Result := v/(h-l)
  else
    Result := 0;
end;
function TG2BaseControl.GetNormParamValue: single;
var h, l, v : integer;
begin
  h := FHighValue;
  l := FLowValue;
  v := FValue;
  if (h-l) <> 0 then
    Result := (v-l)/(h-l)
  else
    Result := 0;
end;
function TG2BaseControl.GetNormValue: single;
begin
  Result := FNormValue;
end;
function TG2BaseControl.GetValueText(aIndex: integer): string;
begin
  Result := '';
end;
function TG2BaseControl.GetValue: integer;
begin
  Result := FValue;
end;
procedure TG2BaseControl.SetSelected(const aValue: boolean);
begin
  if FSelected <> aValue then begin
    FSelected := aValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetState(const aValue: TControlState);
begin
  if State <> aValue then begin
    FState := aValue;
    if FState = csDisabled then  begin
      SendToBack
    end else begin
      BringToFront;
    end;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetValueText(aIndex: integer; const aValue: string);
begin
  //
end;
{procedure TG2BaseControl.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtValueChange: ;
    EvtStateChange: ;
    EvtInvalidate: ;
  end;
end;}
procedure TG2BaseControl.SetNormMorphValue(const aValue: single);
begin
  if aValue <> FNormMorphValue then begin
    FNormMorphValue := aValue;
    FMorphValue :=  trunc(aValue * (FHighValue - FLowValue));
    Redraw;
  end;
end;
procedure TG2BaseControl.SetNormValue(const aValue: single);
begin
  if aValue <> FNormValue then begin
    FNormValue := aValue;
    FValue :=  trunc(aValue * (FHighValue - FLowValue) + FLowValue);
    Redraw;
  end;
end;
procedure TG2BaseControl.SetValue(const aValue: integer);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetButtonText(const aValue: TStrings);
begin
  //
end;
procedure TG2BaseControl.SetCodeRef(const aValue: integer);
begin
 if aValue <> FCodeRef then
   FCodeRef := aValue;
end;
procedure TG2BaseControl.SetHasMorph(const aValue: boolean);
begin
  if FHasMorph <> aValue then begin
    FHasMorph := aValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetHighValue(const aValue: integer);
begin
  if aValue <> FHighValue then begin
    FHighValue := aValue;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetImageIDS(const aValue: TStrings);
var i, c, ImageID : integer;
begin
  for i := 0 to aValue.Count - 1 do begin
    val(aValue[i], ImageID, c);
    if c > 0 then
      exit;
    if ImageID > High(G2SymbolData) then
      exit;
  end;
  FImageIDS.Assign(aValue);
end;
procedure TG2BaseControl.SetLowValue(const aValue: integer);
begin
  if aValue <> FLowValue then begin
    FLowValue := aValue;
    FNormValue := GetNormParamValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetMorphValue(const aValue: integer);
begin
  if aValue <> FMorphValue then begin
    FMorphValue := aValue;
    FNormMorphValue := GetNormMorphValue;
    Redraw;
  end;
end;
procedure TG2BaseControl.SetMouseEnable(const Value: boolean);
begin
  FMouseEnable := Value;
end;
procedure TG2BaseControl.ParsePanelData(fs: TModuleDefStream);
var aName, aValue : string;
begin
  while (fs.Position < fs.Size) and (aName <> '#>') do begin
    fs.ReadSpaces;
    aName := fs.ReadUntil([':', #13]);
    if aName <> '#>' then begin
      if not ParseProperties( fs, aName) then begin
        // Unknown property
        aValue := fs.ReadUntil( [#13]);
      end;
    end;
  end;
end;
function TG2BaseControl.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var sValue : string;
    iValue : integer;
begin
  Result := True;
  if aName = 'ID' then begin
    sValue := fs.ReadUntil( [#13]);
    ID := StrToInt( string(sValue));
  end else
  if aName = 'XPos' then begin
    sValue := fs.ReadUntil( [#13]);
    iValue := StrToInt( string(sValue));
    UnscaledLeft := iValue + UNIT_MARGIN;
  end else
  if aName = 'YPos' then begin
    sValue := fs.ReadUntil( [#13]);
    iValue := StrToInt( string(sValue));
    UnscaledTop := iValue + (1 + iValue / UNITS_ROW) * UNIT_MARGIN;
  end else
  if aName = 'Width' then begin
    sValue := fs.ReadUntil( [#13]);
    if sValue.Chars[0] = '"' then begin
      fs.Position := fs.Position - Length(sValue) - 1;
      Result := False;
    end else
      FUnscaledWidth := StrToInt( string(sValue));
  end else
  if aName = 'Height' then begin
    sValue := fs.ReadUntil( [#13]);
    FUnscaledHeight := StrToInt( string(sValue));
  end else
  if aName = 'ZPos' then begin
    sValue := fs.ReadUntil( [#13]);
    FZOrder := StrToInt( string(sValue));
  end else
  if aName = 'InfoFunc' then begin
    sValue := fs.ReadUntil( [#13]);
    InfoFunc := StrToInt(sValue);
  end else
  if aName = 'ImageIDS' then begin
    fs.ReadConst( '"');
    FImageIDS.Delimiter := ',';
    FImageIDS.DelimitedText := fs.ReadUntil( ['"']);
  end else
    Result := False
end;
procedure TG2BaseControl.ProcessMouseDown(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single);
begin
  if CanFocus then
    SetFocus;
end;
procedure TG2BaseControl.ProcessMouseEnter( aTouchID : integer);
begin
  //
end;
procedure TG2BaseControl.ProcessMouseLeave( aTouchID : integer);
begin
  //
end;
procedure TG2BaseControl.ProcessMouseMove(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single);
begin
 //
end;
procedure TG2BaseControl.ProcessMouseUp(Sender: TObject; Shift: TShiftState; aTouchID : integer; X, Y: Single);
begin
 //
end;
//------------------------------------------------------------------------------
//
//                               TG2ReadControl
//
//------------------------------------------------------------------------------
procedure TG2ReadControl.RemoveReference(aData: IG2Subject);
begin
  if FDataReader = aData then
    FDataReader := nil;
end;
constructor TG2ReadControl.Create( AOwner: TComponent);
begin
  inherited;
  FDataReader := nil;
end;
destructor TG2ReadControl.Destroy;
begin
  if assigned(FDataReader) then begin
    //FDataReader.DeassignControl( self);
    FDataReader.RemoveObserver(self as IG2ParamObserver);
  end;
  inherited;
end;

procedure TG2ReadControl.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtValueChange:
      begin
        SetValue( FDataReader.GetValue);
        Redraw;
      end;
    EvtStateChange: ;
    EvtInvalidate:
      begin
        if assigned(FDataReader) then begin
          SetHighValue( FDataReader.GetHighValue);
          SetLowValue( FDataReader.GetLowValue);
          SetValue( FDataReader.GetValue);
          SetMorphValue( FDataReader.GetSelectedMorphValue);
          SetHasMorph( FDataReader.HasMorph);
          {if self is TG2Button then
            for i := LowValue to HighValue do
              SetValueText(i, FData.GetValueText(i));}
        end;
      end;
  end;
end;
procedure TG2ReadControl.SetDataReader(const aValue: IG2DataParam);
begin
  if FDataReader <> aValue then begin
    if assigned(FDataReader) then
      FDataReader.RemoveObserver( self as IG2ParamObserver);
    FDataReader := aValue;
    if assigned(FDataReader) then
      FDataReader.RegisterObserver( self as IG2ParamObserver);
  end;
end;
//------------------------------------------------------------------------------
//
//                               TG2MultiReadControl
//
//------------------------------------------------------------------------------
constructor TG2MultiReadControl.Create( AOwner: TComponent);
begin
  inherited;
  FDataDependencies := TList<IG2DataParam>.Create;
end;
destructor TG2MultiReadControl.Destroy;

var i : integer;

begin

  for i := 0 to FDataDependencies.Count - 1 do begin

    if assigned(FDataDependencies[i]) then

      FDataDependencies[i].RemoveObserver( self as IG2MultiParamObserver);

  end;

  ClearDataDependencies;

  FDataDependencies.Free;
  inherited;

end;
procedure TG2MultiReadControl.Update(aG2Event: TG2Event);
var i : integer;
begin
  case aG2Event of
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtValueChange: ;
    EvtStateChange: ;
    EvtInvalidate:
      begin
        Redraw
      end;
  end;
end;
procedure TG2MultiReadControl.AddDataDependency( aData : IG2DataParam; aDeassignEvent : TNotifyEvent);
begin
  FDataDependencies.Add( aData);
  aData.RegisterObserver( self as IG2MultiParamObserver);
  HighValue := aData.GetHighValue;
  LowValue := aData.GetLowValue;
  Value := aData.GetValue;
end;
{procedure TG2MultiReadControl.ClearDataDependency( aData : IG2DataParam);
var i : integer;
begin
  //aData.RemoveObserver( self as IG2MultiParamObserver);
  i := FDataDependencies.IndexOf( aData);
  if i <> -1 then begin
    FDataDependencies.Delete(i);
    Redraw;
  end;
end;}
procedure TG2MultiReadControl.RemoveReference(aData: IG2Subject);
var i : integer;
begin
  //aData.RemoveObserver( self as IG2MultiParamObserver);
  i := FDataDependencies.IndexOf( aData as IG2DataParam);
  if i <> -1 then begin
    FDataDependencies.Delete(i);
  end;
end;
procedure TG2MultiReadControl.ClearDataDependencies;
var i : integer;
begin
  FDataDependencies.Clear;
  FHasMorph := False;
  MorphValue := 0;
  Value := 0;
  Redraw;
end;
//------------------------------------------------------------------------------
//
//                               TG2WriteControl
//
//------------------------------------------------------------------------------
procedure TG2WriteControl.RemoveReference(aData: IG2Subject);
begin
  if FDataWriter = aData then
    FDataWriter := nil;
end;
constructor TG2WriteControl.Create( AOwner: TComponent);
begin
  inherited;
  FDataWriter := nil;
end;
destructor TG2WriteControl.Destroy;
begin
  if assigned(FDataWriter) then begin
    FDataWriter.RemoveObserver( self as IG2ParamObserver);
    //FDataWriter.DeassignControl( self);
  end;
  inherited;
end;
procedure TG2WriteControl.DoEnter;
begin
  inherited;
  if assigned(FDataWriter) then
    FDataWriter.SetSelected( True);
end;
procedure TG2WriteControl.DoExit;
begin
  inherited;
  if assigned(FDataWriter) then
    FDataWriter.SetSelected( False);
  Redraw;
end;
procedure TG2WriteControl.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  if IsFocused then begin
    case Key of
    vkUp :
      begin
        if not(ssShift in Shift) and not(ssCtrl in Shift) then begin
          IncValue;
          if assigned(FDataWriter) then
            FDataWriter.SetValue( FValue);
        end;
        //if ssCtrl in Shift then
        //  IncMorphValue;
      end;
    vkDown :
      begin
        if not(ssShift in Shift) and not(ssCtrl in Shift) then begin
          DecValue;
          if assigned(FDataWriter) then
            FDataWriter.SetValue( FValue);
        end;
        //if ssCtrl in Shift then
        //  DeccMorphValue;
      end;
    end;
  end;
  inherited;
end;
procedure TG2WriteControl.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin
  inherited;
end;
procedure TG2WriteControl.Update(aG2Event: TG2Event);
var i : integer;
begin
  case aG2Event of
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtValueChange: ;
    EvtStateChange: ;
    EvtInvalidate:
      begin
        if assigned(FDataWriter) then begin
          SetHighValue( FDataWriter.GetHighValue);
          SetLowValue( FDataWriter.GetLowValue);
          SetValue( FDataWriter.GetValue);
          SetMorphValue( FDataWriter.GetSelectedMorphValue);
          SetHasMorph( FDataWriter.HasMorph);
          if self is TG2Button then
            for i := LowValue to HighValue do
              SetValueText(i, FDataWriter.GetValueText(i));
        end;
      end;
  end;
end;
procedure TG2WriteControl.SetValueDataDependencies(const aValue: byte);
begin
  if assigned(FDataWriter) then begin
    if FDataWriter.GetValue <> aValue then
      FDataWriter.SetValue( aValue);
  end;
  if assigned(FOnChangeValue) then
    FOnChangeValue(self, FValue);
end;
procedure TG2WriteControl.SetDataWriter(const aValue: IG2DataParam);
begin
  if FDataWriter <> aValue then begin
    if assigned(FDataWriter) then
      FDataWriter.RemoveObserver( self as IG2ParamObserver);
    FDataWriter := aValue;
    Update(EvtInvalidate);
    if assigned(FDataWriter) then
      FDataWriter.RegisterObserver( self as IG2ParamObserver);
  end;
end;
procedure TG2WriteControl.SetMorphValueDataDependencies( const aValue : byte);
begin
  if assigned(FDataWriter) then
    FDataWriter.SetSelectedMorphValue( aValue);
  if assigned(FOnChangeMorphValue) then
    FOnChangeMorphValue(self, FValue);
end;
procedure TG2WriteControl.SetSelected(const aValue: boolean);
begin
  if FSelected <> aValue then begin
    inherited;
    if assigned(FDataWriter) then
      FDataWriter.SetSelected( aValue);
    if assigned(FOnSelectControl) then
      FOnSelectControl(self);
  end;
end;
//------------------------------------------------------------------------------
//
//                                 TG2Led
//
//------------------------------------------------------------------------------
constructor TG2Led.Create(AOwner: TComponent);
begin
  inherited;
  FDataLed := nil;
  HitTest := False;
end;
destructor TG2Led.Destroy;
begin
  if assigned(FDataLed) then
    FDataLed.RemoveObserver( self as IG2LedObserver);
  inherited;
end;
procedure TG2Led.RemoveReference(aData: IG2Subject);
begin
  if FDataLed = aData then
    FDataLed := nil;
end;
procedure TG2Led.SetDataLed(const aValue: IG2DataLed);
begin
  if FDataLed <> aValue then begin
    if assigned(FDataLed) then
      FDataLed.RemoveObserver( self as IG2LedObserver);
    FDataLed := aValue;
    if assigned(FDataLed) then
      FDataLed.RegisterObserver( self as IG2LedObserver);
  end;
end;
procedure TG2Led.SetLedLevel(aValue: integer);
begin
  // Abstract
end;
procedure TG2Led.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtValueChange:
      begin
        if FDataLed.GetGroupCount > 1 then begin
          if FDataLed.GetValue = CodeRef then
            SetValue(1)
          else
            SetValue(0);
        end else
          SetValue( FDataLed.GetValue);
        //Redraw;
      end;
    EvtStateChange: ;
    EvtInvalidate:;
  end;
end;
//------------------------------------------------------------------------------
//
//                                TG2LedGreen
//
//------------------------------------------------------------------------------
constructor TG2LedGreen.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LedType := ltGreen;
  //FUnscaledWidth := 11;
  //FUnscaledHeight := 6;
  Width := 11;
  Height := 6;
  FLedLevel := 0;
end;
destructor TG2LedGreen.Destroy;
begin
  inherited;
end;
procedure TG2LedGreen.PaintOn( aCanvas : TCanvas);
begin
  aCanvas.Fill.Kind := TBrushKind.bkSolid;
  //if FLedLevel = 0 then
  if FValue = 0 then
    aCanvas.Fill.Color := ModuleColors[0]
  else
    aCanvas.Fill.Color := claLime;
  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claBlue;
  aCanvas.FillRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  inherited;
end;
procedure TG2LedGreen.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcZoomedBounds;
end;
function TG2LedGreen.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      CodeRef := StrToInt(string(aValue));
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      InfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Green"' then begin
        LedType := ltGreen;
        FUnscaledWidth := 8;
        FUnscaledHeight := 8;
      end;
      if aValue = '"Sequencer"' then begin
        LedType := ltSequencer;
        FUnscaledWidth := 12;
        FUnscaledHeight := 7;
      end;
    end else
    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      LedGroupId := StrToInt(string(aValue));
    end else
      Result := False
  end;
end;
procedure TG2LedGreen.SetLedLevel(aValue: integer);
begin
  if aValue <> FLedLevel then begin
    FLedLevel := aValue;
    Redraw;
  end;
end;
{//------------------------------------------------------------------------------
//
//                               TG2LedGroup
//
//------------------------------------------------------------------------------
constructor TG2LedGroup.Create(AOwner: TComponent);
var LedSeqComparison : TComparison<TG2LedGreen>;
begin
  inherited Create(AOwner);
  LedSeqComparison :=
    function (const Led1, Led2 : TG2LedGreen): integer
    begin
      if led1.FCodeRef > led2.FCodeRef then
        Result := 1
      else
        if led1.FCodeRef = led2.FCodeRef then
          Result := 0
        else
          Result := -1;
    end;
  FLeds := TList<TG2LedGreen>.Create(TComparer<TG2LedGreen>.Construct(LedSeqComparison));
  FLedType := ltSequencer;
  FLedGroupId := 0;
  FLedOn := 0;
end;
destructor TG2LedGroup.Destroy;
begin
  FLeds.Free;
  inherited;
end;
procedure TG2LedGroup.SetLedLevel(aValue: integer);
begin
 inherited;
  if FLedOn < FLeds.Count then
    FLeds[ FLedOn].SetLedLevel( 0);
  if aValue < FLeds.Count then begin
    FLedOn := aValue;
    FLeds[ FLedOn].SetLEdLevel( 1);
  end;
end;}

//------------------------------------------------------------------------------
//
//                               TG2MiniVU
//
//------------------------------------------------------------------------------
constructor TG2MiniVU.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := True;
  FLedType := ltMiniVU;
  FMiniVUWidth := 8;
  FMiniVUHeight := 16;
  //UnscaledWidth := FMiniVUWidth;
  //UnscaledHeight := FMiniVUHeight;
  Width := FMiniVUWidth;
  Height := FMiniVUHeight;
  FLevel := 0;
end;
destructor TG2MiniVU.Destroy;
begin
  inherited;
end;
procedure TG2MiniVU.PaintOn( aCanvas : TCanvas);
var Rect : TRectF;
    i, level : integer;
    level_green,
    level_yellow,
    level_red : single;
begin
  level_green := 8 * Height / 16;
  level_yellow := 12 * Height / 16;
  level_red := Height;
  aCanvas.Fill.Kind := TBrushKind.bkSolid;
  Rect := UnscaledBoundsRect;
  Rect.Bottom := UnscaledBoundsRect.Bottom;
  Rect.Top := UnscaledBoundsRect.Bottom - level_green;
  aCanvas.Fill.Color := claGreen;
  aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
  Rect.Bottom := UnscaledBoundsRect.Bottom - level_green;
  Rect.Top := UnscaledBoundsRect.Bottom - level_yellow;
  aCanvas.Fill.Color := claOlive;
  aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
  Rect.Bottom := UnscaledBoundsRect.Bottom - level_yellow;
  Rect.Top := UnscaledBoundsRect.Bottom - Height;
  aCanvas.Fill.Color := claMaroon;
  aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
  Rect := Rect;
  if FValue = 0 then
    level := 0
  else
    level := round(FMiniVUHeight*system.math.log10(FValue)*0.45); // Something like this...
  i := 0;
  //while (i <= Height) and (i <= FLevel) do begin
  while (i <= Height) and (i <= level) do begin
    Rect.Top := UnscaledBoundsRect.Bottom - i - 1;
    Rect.Bottom := UnscaledBoundsRect.Bottom - i;
    if i > level_yellow then
      aCanvas.Fill.Color := claRed
    else
      if i > level_green then
        aCanvas.Fill.Color := claYellow
      else
        aCanvas.Fill.Color := claLime;
    aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
    inc(i);
  end;
  inherited;
end;
procedure TG2MiniVU.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcZOomedBounds;
end;
function TG2MiniVU.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      CodeRef := StrToInt(string(aValue));
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      InfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Vertical"' then begin
        FOrientation := otVertical;
        FUnscaledWidth := FMiniVUWidth;
        FUnscaledHeight := FMiniVUHeight;
      end;
      if aValue = '"Horizontal"' then begin
        FOrientation := otHorizontal;
        FUnscaledHeight := FMiniVUWidth;
        FUnscaledWidth := FMiniVUHeight;
      end;
    end else
    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      LedGroupId := StrToInt(string(aValue));
    end else
      Result := False
  end;
end;
procedure TG2MiniVU.SetLedLevel(aValue: integer);
begin
  // -40dB 0dB Green  aValue 0..7
  // 0dB 11dB Yellow         9..41/42
  //     >11dB Red           75/76
  if aValue = 0 then
    FLevel := 0
  else
    FLevel := round(FMiniVUHeight*system.math.log10(aValue)*0.45); // Something like this...
  if assigned(FScope) then
    FScope.AddValue( FLevel);
  Redraw;
end;
procedure TG2MiniVU.SetScope(const Value: TG2Scope);
begin
  FScope := Value;
end;
//------------------------------------------------------------------------------
//
//                                 TG2Scope
//
//------------------------------------------------------------------------------

constructor TG2Scope.Create(AOwner: TComponent);
begin
  inherited;
  FBufferingActive := False;
  FBufferIndex := 0;
  FMaxBufferSize := 0;
  FBufferSize := 0;
  FCycles := 0;
  SetLength(FBuffer, 0);
end;
destructor TG2Scope.Destroy;
begin
  Finalize(FBuffer);
  inherited;
end;
procedure TG2Scope.PaintOn(aCanvas: TCanvas);
var StyleSet : TG2StyleSet;
    sx, sy, ty : single;
    i : integer;
begin
  StyleSet := FStateStyleList.StateStyle[ FState];
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, 0, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  if FBufferSize = 0 then
    exit;

  ty := UnscaledHeight / 2;
  if FBufferSize = 0 then
    sx := 0
  else
    sx := UnscaledWidth/FBufferSize;
  sy := 5;
  for i := 1 to FBufferSize - 1 do begin
    aCanvas.DrawLine(PointF(sx*(i-1), ty + sy*FBuffer[i-1]), PointF(sx*i, ty +  sy*FBuffer[i]), AbsoluteOpacity);
  end;
  inherited;
end;
procedure TG2Scope.AddValue(aValue: single);
var LastValue : single;
begin
  if FBufferingActive then begin
    if (FBufferIndex >= 0) and (FBufferIndex < Length(FBuffer)) then
      LastValue := FBuffer[ FBufferIndex]
    else
      LastValue := 0;
    inc(FBufferIndex);
    if FBufferIndex >=  Length(FBuffer) then
      FBufferIndex := 0
    else
      if FBufferSize < FBufferIndex then
        FBufferSize := FBufferIndex;
    FBuffer[FBufferIndex] := aValue;
    Redraw;
  end;
end;
procedure TG2Scope.SetBufferingActive(const aValue: boolean);
begin
  if aValue <> FBufferingActive then begin
    if aValue then begin
      FMaxBufferSize := 50;
      SetLength(FBuffer, FMaxBufferSize);
      FBufferSize := 0;
      FBufferIndex := 0;
      FBufferingActive := True;
    end else begin
      FBufferingActive := False;
      SetLength(FBuffer, 0);
      FBufferIndex := 0;
      FBufferSize := 0;
    end;
  end;
end;

//------------------------------------------------------------------------------
//
//                                 TG2Label
//
//------------------------------------------------------------------------------
constructor TG2Label.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Hittest := False;
  //UnscaledHeight := 10;
  //UnscaledWidth := 30;
  Height := 10;
  Width := 30;
  FTextAlign := TTextAlign.taLeading;
  FLabelText := '';
end;
destructor TG2Label.Destroy;
begin
  inherited;
end;
procedure TG2Label.PaintOn( aCanvas : TCanvas);
var StyleSet : TG2StyleSet;
begin
  StyleSet := FStateStyleList.StateStyle[ csDefault];
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceText, 0, StyleSet);
  DrawTextExt( aCanvas,
               StyleSet.Font,
               PointF(0, 0),
               PointF(UnscaledBoundsRect.Width, UnscaledBoundsRect.Height),
               StyleSet.FontColor,
               FLabelText,
               FTextAlign,
               TTextAlign.taCenter);
  inherited;
end;
procedure TG2Label.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcZoomedBounds;
end;
function TG2Label.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
    StyleSet : TG2StyleSet;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'FontSize' then begin
      aValue := fs.ReadUntil( [#13]);
      StyleSet := FStateStyleList.StateStyle[ csDefault];
      StyleSet.Font.Size := StrToInt(string(aValue))-2
    end else
    if aName = 'Text' then begin
      aValue := fs.ReadUntil( [#13]);
      LabelText := string(fs.UnQuote(aValue));
    end else
      Result := False
  end;
end;
procedure TG2Label.SetLabelText(const aValue: string);
begin
  if FLabelText <> aValue then begin
    FLabelText := aValue;
    Redraw;
  end;
end;
procedure TG2Label.SetTextAlign(const aValue: TTextAlign);
begin
  if FTextAlign <> aValue then begin
    FTextAlign := aValue;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
//
//                             TG2TextField
//
//------------------------------------------------------------------------------
constructor TG2TextField.Create(AOwner: TComponent);
var StyleSet : TG2StyleSet;
begin
  inherited Create(AOwner);
  HitTest := False;
  StyleSet := FStateStyleList.StateStyle[ csDefault];
  StyleSet.Font.Size := 9;
  //UnscaledHeight := 10;
  //UnscaledWidth := 30;
  Height := 10;
  Width := 30;
end;
destructor TG2TextField.Destroy;
begin
  inherited;
end;
procedure TG2TextField.PaintOn( aCanvas : TCanvas);
var TextFunc : string;
    StyleSet : TG2StyleSet;
begin
  StyleSet := TG2StyleSet.Create( csDefault);
  try
    StyleSet.Assign( FStateStyleList.StateStyle[ csDefault]);
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceBackground, 0, StyleSet);
    StyleSet.ApplyStyleSet(aCanvas);
    aCanvas.FillRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
    TextFunc := '';
    if FState <> csDisabled then begin
      if assigned(FOnGetTextFunc) then
        FOnGetTextFunc(self, TextFunc)
      else
        if (FDataDependencies.Count > 0) and assigned(FDataDependencies[0]) then
          Textfunc := FDataDependencies[0].TextFunction;
    end;
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceText, 0, StyleSet);
    DrawTextExt( aCanvas,
                 StyleSet.Font,
                 PointF(0,0),
  {$IF Defined(Android)}
                 PointF(UnscaledWidth, UnscaledHeight*1.2),
  {$ELSE}
                 PointF(UnscaledWidth, UnscaledHeight),
  {$ENDIF}
                 StyleSet.FontColor,
                 TextFunc,
                 TTextAlign.taCenter, TTextAlign.taCenter);
  finally
    StyleSet.Free;
  end;
  inherited;
end;
procedure TG2TextField.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcZoomedBounds;
end;
function TG2TextField.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'MasterRef' then begin
      aValue := fs.ReadUntil( [#13]);
      MasterRef := StrToInt(string(aValue));
      // Parameter := ModuleData.Parameter[ MasterRef] as TG2GraphParameterFMX;
    end else
    if aName = 'Text Func' then begin
      aValue := fs.ReadUntil( [#13]);
      TextFunc := StrToInt(string(aValue));
    end else
    if aName = 'Dependencies' then begin
      fs.ReadConst( '"');
      Dependencies := fs.ReadUntil( ['"']);
      //if assigned(Parameter) then
      //  ModuleData.FSVGControl.ParseDependencies( self, Parameter.ParamIndex, aValue, FTextFunction);
    end else
      Result := False
  end;
end;
//------------------------------------------------------------------------------
//
//                               TG2Button
//
//------------------------------------------------------------------------------
constructor TG2Button.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  HitTest := True;
  CanFocus := True;
  FButtonText := TStringList.Create;
  FBitmapData := TStringList.Create;
  FDefaultStateStyles.StateStyle[csDefault].Font.Size := 7;
  FDefaultStateStyles.StateStyle[csFocused].Font.Size := 7;
  FDefaultStateStyles.StateStyle[csDisabled].Font.Size := 7;
  FDefaultStateStyles.StateStyle[csSelected].Font.Size := 7;
  FDefaultStateStyles.StateStyle[csFocusedSelected].Font.Size := 7;
  FOrientation := otHorizontal;
  FButtonKind := bkToggle;
  FButtonCount := 1;
  FBtnSelected := -1;
  FBorderWidth := 0;
  //UnscaledWidth := 13;
  //UnscaledHeight := 12;
  //FButtonWidth := UnscaledWidth;
  //FButtonHeight := UnscaledHeight;
  Width := 13;
  Height := 12;
  FButtonWidth := Width;
  FButtonHeight := Height;
  FImageCount := 0;
  FImageWidth := 0;
end;
destructor TG2Button.Destroy;
begin
  FButtonText.Free;
  FBitmapData.Free;
  inherited;
end;
procedure TG2Button.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseDown) then
    OnMouseDown( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseDown(Self, Shift, 0, X, Y);
end;
procedure TG2Button.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcZoomedBounds;
end;
function TG2Button.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'Text' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FButtonText, [','], ['"']);
    end else
    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FBitmapData, [':'], ['"']);
    end else
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      //TODO Parameter := ModuleData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
      CodeRef := StrToInt(string(aValue));
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      InfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Horizontal"' then
        FOrientation := otHorizontal
      else
        FOrientation := otVertical;
    end else
    if aName = 'ButtonCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonCount := StrToInt(string(aValue));
    end else
    if aName = 'ButtonWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonWidth := StrToInt(string(aValue)) - 1;
    end else
    {if aName = 'Type' then begin
      aValue := ReadUntil(fs, [#13]);
      //
    end else}
    if aName = 'Style' then begin
      aValue := fs.ReadUntil( [#13]);
      // TODO
    end else
    if aName = 'ImageWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageWidth := StrToInt(string(aValue));
    end else
    if aName = 'ImageCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageCount := StrToInt(string(aValue));
    end else
      Result := False
  end;
end;
procedure TG2Button.CalcDimensions;
var BtnCount : integer;
begin
  if FG2Updating then
    exit;
  if ButtonCount = 0 then
    BtnCount := 1
  else
    BtnCount := ButtonCount;
  if Orientation = otHorizontal then begin
    FButtonWidth := (UnscaledWidth - FBorderWidth) / BtnCount + FBorderWidth;
    FButtonHeight := UnscaledHeight;
  end else begin
    FButtonWidth := UnscaledWidth;
    FButtonHeight := (UnscaledHeight - FBorderWidth) / BtnCount + FBorderWidth;
  end;
end;
procedure TG2Button.SetBtnSelected(const aValue: integer);
begin
  if FBtnSelected <> aValue then begin
    FBtnSelected := aValue;
    Redraw;
  end;
end;
procedure TG2Button.SetButtonCount(const aValue: integer);
begin
  if FButtonCount <> aValue then begin
    FButtonCount := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Button.SetButtonHeight(const aValue: single);
begin
  if FButtonHeight <> aValue then begin
    FButtonHeight := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Button.SetButtonKind(const aValue: TBtnKind);
begin
  if FButtonKind <> aValue then begin
    FButtonKind := aValue;
  end;
end;
procedure TG2Button.SetButtonText(const aValue: TStrings);
begin
  if assigned(aValue) then begin
    FButtonText.Assign(aValue);
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Button.SetButtonWidth(const aValue: single);
begin
  if FButtonWidth <> aValue then begin
    FButtonWidth := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Button.SetOrientation(const aValue: TOrientationType);
begin
  if FOrientation <> aValue then begin
    FOrientation := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Button.SetValueText(aIndex: integer; const aValue: string);
begin
  while aIndex >= FButtonText.Count do
    FButtonText.Add('');
  FButtonText[aIndex] := aValue;
  Redraw;
end;
//------------------------------------------------------------------------------
//
//                                 TG2BtnText
//
//------------------------------------------------------------------------------
constructor TG2BtnText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonType := bttCheck;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnText.Destroy;
begin
  inherited;
end;
procedure TG2BtnText.PaintOn(aCanvas: TCanvas);
var LabelText : string;
    StyleSet : TG2StyleSet;
begin
  if FBtnSelected <> -1 then begin
    if IsFocused then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csSelected]
  end else
    if IsFocused then
      StyleSet := FStateStyleList.StateStyle[ csFocused]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, 0, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  if NormMorphValue <> 0 then begin
    aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
    aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
  end else
    if HasMorph then begin
      aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
      aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
    end;
  if FImageIDS.Count > 0 then begin
    if Value < FImageIDS.Count then begin
      if assigned(FOnPaintElement) then
        FOnPaintElement(self, ceSymbol, Value, StyleSet);
      aCanvas.Fill.Color := StyleSet.FontColor;
      FPathDataList.DrawSymbol( aCanvas, UnscaledBoundsRect, FImageIDS[Value]);
    end;
  end else begin
    LabelText := '';
    if Value < FButtonText.Count then begin
      LabelText := FButtonText[ Value];
    end;
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceText, 0, StyleSet);
    if trim(LabelText) <> '' then
      DrawTextExt( aCanvas,
                   StyleSet.Font,
                   PointF(UnscaledBoundsRect.Left, UnscaledBoundsRect.Top),
                   PointF(UnscaledBoundsRect.Width, UnscaledBoundsRect.Height),
                   StyleSet.FontColor,
                   LabelText,
                   TTextAlign.taCenter, TTextAlign.taCenter);
  end;
  inherited;
end;
procedure TG2BtnText.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  if FBitmapData.Count > 0 then begin
    //FImageList.BitmapWidth := FImageWidth;
    //FImageList.ParseImageData( 1, True)
  end;
  //FParameter.CanChangeLabel := True;
  CalcDimensions;
  CalcZoomedBounds;
end;
function TG2BtnText.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      {if aValue = '"Push"' then
        FButtonType := bttPush
      else
        FButtonType := bttNormal;}
      if aValue = '"Push"' then begin
        ButtonType := bttPush;
        ButtonKind := bkMomentary
      end else
        if aValue = '"Check"' then begin
          ButtonType := bttCheck;
          ButtonKind := bkToggle;
        end;
    end else
      Result := False
  end;
end;
procedure TG2BtnText.ProcessMouseDown(Sender: TObject; Shift: TShiftState;
     aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  if Sender = Self then begin
    if FButtonKind = bkMomentary then begin
      Value := 1;
      //FBtnSelected := 1;
      Redraw;
      SetValueDataDependencies( FValue);
      //if assigned(FOnChangeValue) then

      //  FOnChangeValue(self, FValue);
    end else begin

      //
    end;

  end;
  inherited;
end;
procedure TG2BtnText.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
     aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  //if not Selected then // prevent accidental click
  if not IsFocused then // prevent accidental click
    exit;
  if Sender = Self then begin
    if FButtonKind = bkMomentary then begin
      Value := 0;
    end else begin
      if Value = 0 then
        Value := 1
      else begin
        Value := 0;
      end;
    end;
    SetValueDataDependencies( FValue);
    //if assigned(FOnChangeValue) then
    //  FOnChangeValue(self, FValue);
  end;
end;
{procedure TG2BtnText.DoMouseLeave;
begin
  if FState = csDisabled then
    exit;
  if not IsFocused then // prevent accidental click
    exit;
  if FButtonKind = bkMomentary then begin
    if Value <> 0 then begin
      Value := 0;
      SetValueDataDependencies( FValue);
    end;
  end;
  inherited;
end;}
procedure TG2BtnText.DoMouseLeave;
begin
  if FMouseEnable then
    ProcessMouseLeave(0);
end;
procedure TG2BtnText.ProcessMouseLeave( aTouchID : integer);
begin
  if FState = csDisabled then
    exit;
  if not IsFocused then // prevent accidental click
    exit;
  if FButtonKind = bkMomentary then begin
    if Value <> 0 then begin
      Value := 0;
      SetValueDataDependencies( FValue);
    end;
  end;
  inherited;
end;
procedure TG2BtnText.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
procedure TG2BtnText.SetButtonType(const aValue: TButtonTextType);
begin
  if FButtonType <> aValue then begin
    FButtonType := aValue;
    case FButtonType of
      bttCheck : ButtonKind := bkToggle;
      bttPush : ButtonKind := bkMomentary;
    end;
  end;
end;
procedure TG2BtnText.SetValue(const aValue: integer);
begin
  if FButtonKind = bkMomentary then begin
    if aValue = 1 then
      FBtnSelected := 0
    else
      FBtnSelected := -1;
  end else begin
    if aValue = 1 then
      FBtnSelected := 0
    else
      FBtnSelected := -1;
  end;
  inherited;
end;
//------------------------------------------------------------------------------
//
//                                 TG2BtnTextEdit
//
//------------------------------------------------------------------------------
constructor TG2BtnTextEdit.Create(AOwner: TComponent);
begin
  inherited;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnTextEdit.Destroy;
begin
  inherited;
end;
function TG2BtnTextEdit.GetButtonLabel: string;
begin
  if FButtonText.Count > 0 then
    Result := FButtonText[0]
  else
    Result := '';
end;
procedure TG2BtnTextEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
procedure TG2BtnTextEdit.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  if not IsFocused then // prevent accidental click
    exit;
  if Sender = Self then begin
    if Value = 0 then
      Value := 1
    else begin
      Value := 0;
    end;
    SetValueDataDependencies( FValue);
    //if assigned(FOnChangeValue) then
    //  FOnChangeValue(self, FValue);
  end;
end;
procedure TG2BtnTextEdit.SetButtonLabel(const aValue: string);
begin
  if FButtonText.Count = 0 then
    FButtonText.Add(aValue)
  else
    FButtonText[0] := aValue;
  Redraw;
end;
procedure TG2BtnTextEdit.PaintOn(aCanvas: TCanvas);
var LabelText : string;
    StyleSet : TG2StyleSet;
begin
  if IsFocused then begin
    if Value = 1 then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csFocused];
  end else begin
    if Value = 1 then
      StyleSet := FStateStyleList.StateStyle[ csSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  end;
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, 0, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect( UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  if NormMorphValue <> 0 then begin
    aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
    aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
  end else
    if HasMorph then begin
      aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
      aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
    end;
  LabelText := '';
  if FButtonText.Count > 0 then begin
    LabelText := FButtonText[ 0];
  end;
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceText, 0, StyleSet);
  DrawTextExt( aCanvas,
               StyleSet.Font,
               PointF(UnscaledBoundsRect.Left, UnscaledBoundsRect.Top),
               PointF(UnscaledBoundsRect.Width, UnscaledBoundsRect.Height),
               StyleSet.FontColor,
               LabelText,
               TTextAlign.taCenter, TTextAlign.taCenter);
  inherited;
end;

//------------------------------------------------------------------------------
//
//                                 TG2BtnFlat
//
//------------------------------------------------------------------------------
constructor TG2BtnFlat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnFlat.Destroy;
begin
  inherited;
end;
procedure TG2BtnFlat.PaintOn( aCanvas : TCanvas);
var LabelText : string;
    StyleSet : TG2StyleSet;
begin
  if FBtnSelected <> -1 then begin
    if IsFocused then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csSelected];
  end else
    if IsFocused then
      StyleSet := FStateStyleList.StateStyle[ csFocused]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, 0, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.FillRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  if NormMorphValue <> 0 then begin
    aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
    aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
  end else
    if HasMorph then begin
      aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
      aCanvas.FillRect( UnscaledBoundsRect, 0, 0, [], 0.5);
    end;
  if FImageIDS.Count > 0 then begin
    if Value < FImageIDS.Count then begin
      if assigned(FOnPaintElement) then
        FOnPaintElement(self, ceSymbol, Value, StyleSet);
      aCanvas.Fill.Color := StyleSet.FontColor;
      PathDataList.DrawSymbol( aCanvas, UnscaledBoundsRect, FImageIDS[Value]);
    end;
  end else begin
    LabelText := '';
    if Value < FButtonText.Count then
      LabelText := FButtonText[ Value];
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceText, 0, StyleSet);
    DrawTextExt( aCanvas,
                 StyleSet.Font,
                 PointF(UnscaledBoundsRect.Left, UnscaledBoundsRect.Top),
                 PointF(UnscaledBoundsRect.Width, UnscaledBoundsRect.Height),
                 StyleSet.FontColor,
                 LabelText,
                 TTextAlign.taCenter, TTextAlign.taCenter);
  end;
  inherited;
end;
procedure TG2BtnFlat.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  //TODO if assigned(FParameter) then
  //  if FImageCount > 0 then begin
  //    //FImageList.BitmapWidth := FImageWidth;
  //    //FImageList.ParseImageData( FImageCount, True)
  //  end;
  CalcDimensions;
  CalcZoomedBounds;
end;
procedure TG2BtnFlat.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  //if not Selected then // prevent accidental click
  if not IsFocused then // prevent accidental click
    exit;
  if Sender = self then begin
    RotateValue;
    SetValueDataDependencies( FValue);
    //if assigned(FOnChangeValue) then
    //  FOnChangeValue(self, FValue);
  end;
end;
procedure TG2BtnFlat.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
//------------------------------------------------------------------------------
//
//                             TG2BtnArray
//
//------------------------------------------------------------------------------
constructor TG2BtnArray.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := True;
  SetLength(FRectArray,0);
end;
destructor TG2BtnArray.Destroy;
begin
  Finalize(FRectArray);
  inherited;
end;
procedure TG2BtnArray.PaintOn(aCanvas: TCanvas);
var i : integer;
    procedure PaintButton( aIndex : integer);
    var Rect : TRectF;
        LabelText : string;
        StyleSet : TG2StyleSet;
    begin
      StyleSet := TG2StyleSet.Create(csDefault);
      try
        Rect := FRectArray[aIndex];
        if aIndex = FBtnSelected then begin
          if IsFocused then
            StyleSet.Assign( FStateStyleList.StateStyle[ csFocusedSelected])
          else
            StyleSet.Assign( FStateStyleList.StateStyle[ csSelected]);
        end else begin
          if IsFocused then
            StyleSet.Assign( FStateStyleList.StateStyle[ csFocused])
          else
            StyleSet.Assign( FStateStyleList.StateStyle[ FState]);
        end;
        if assigned(FOnPaintElement) then
          FOnPaintElement(self, ceBackGround, aIndex, StyleSet);
        StyleSet.ApplyStyleSet( aCanvas);
        aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
        aCanvas.DrawRect(Rect, 0, 0, [], AbsoluteOpacity);
        if NormMorphValue <> 0 then begin
          aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
          aCanvas.FillRect( Rect, 0, 0, [], 0.5);
        end else
          if HasMorph then begin
            aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
            aCanvas.FillRect( Rect, 0, 0, [], 0.5);
          end;
        if FImageIDS.Count > 0 then begin
          if aIndex < FImageIDS.Count then begin
            if assigned(FOnPaintElement) then
              FOnPaintElement(self, ceSymbol, aIndex, StyleSet);
            aCanvas.Fill.Color := StyleSet.FontColor;
            PathDataList.DrawSymbol( aCanvas, Rect, FImageIDS[aIndex]);
          end;
        end else begin
          LabelText := '';
          if aIndex < FButtonText.Count then
            LabelText := FButtonText[aIndex];
          if assigned(FOnPaintElement) then
            FOnPaintElement(self, ceText, aIndex, StyleSet);
          DrawTextExt( aCanvas,
                       StyleSet.Font,
                       PointF(Rect.Left, Rect.Top),
                       PointF(Rect.Width, Rect.Height),
                       StyleSet.FontColor,
                       LabelText,
                       TTextAlign.taCenter, TTextAlign.taCenter);
        end;
      finally
        StyleSet.Free;
      end;
    end;
begin
  if Length(FRectArray) > 0 then begin
    for i := 0 to Length(FRectArray) - 1 do begin
      if i <> FBtnSelected then
        PaintButton( i);
    end;
    if (FBtnSelected <> -1) and (FBtnSelected < Length(FRectArray)) then begin
      PaintButton( FBtnSelected);
    end;
  end;
  inherited;
end;
procedure TG2BtnArray.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
var i : integer;
begin
  if FState = csDisabled then
    exit;
  //if not Selected then // prevent accidental click
  if not IsFocused then // prevent accidental click
    exit;
  if Sender = Self then begin
    i := 0;
    while (i<FButtonCount) and not(PtInRect(FRectArray[i], PointF(X{/Zoom}, Y{/Zoom}))) do
      inc(i);
    if (i<FButtonCount) then begin
      FBtnSelected := i;
      Value := i;
      SetValueDataDependencies( FValue);
      //if assigned(FOnChangeValue) then
      //  FOnChangeValue(self, FValue);
      if assigned(FOnButtonClick) then
        FOnButtonClick(self, i);
    end;
  end;
end;
procedure TG2BtnArray.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
procedure TG2BtnArray.SetValue(const aValue: integer);
begin
  if FButtonKind = bkToggle then begin
    if FBtnSelected <> aValue then begin
      FBtnSelected := aValue
    end;
  end;
  inherited;
end;
//------------------------------------------------------------------------------
//
//                             TG2BtnRadio
//
//------------------------------------------------------------------------------
constructor TG2BtnRadio.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpsideDown := False;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnRadio.Destroy;
begin
  inherited;
end;
procedure TG2BtnRadio.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  if Orientation = otHorizontal then begin
    UnscaledBoundsRect := RectF(0, 0, (ButtonWidth - FBorderWidth) * ButtonCount + FBorderWidth, ButtonHeight);
  end else begin
    UnscaledBoundsRect := RectF(0, 0, ButtonWidth, (ButtonHeight - FBorderWidth) * ButtonCount + FBorderWidth);
  end;
  CalcDImensions;
  if FBitmapData.Count > 0 then begin
    //FImageList.BitmapWidth := FImageWidth;
    //FImageList.ParseImageData( FButtonCount, True)
  end;
end;
function TG2BtnRadio.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    {if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Horizontal"' then
        Orientation := otHorizontal
      else
      Orientation := otVertical;
    end else
    if aName = 'ButtonCount' then begin
      aValue := fs.ReadUntil( [#13]);
      ButtonCount := StrToInt(string(aValue));
    end else
    if aName = 'ButtonWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      //ButtonWidth := StrToInt(string(aValue));
      UnscaledWidth := StrToInt(string(aValue));
    end else}
      Result := False
  end;
end;
procedure TG2BtnRadio.CalcDimensionsFromBtnSize(aBtnWidth, aBtnHeight: single);
begin
  if Orientation = otHorizontal then begin
    FUnscaledWidth := (ButtonWidth - FBorderWidth) * ButtonCount + FBorderWidth;
    FUnscaledHeight := ButtonHeight;
    //UnscaledBoundsRect := RectF(0, 0, (ButtonWidth - FBorderWidth) * ButtonCount + FBorderWidth, ButtonHeight);
  end else begin
    FUnscaledWidth := ButtonWidth;
    FUnscaledHeight := (ButtonHeight - FBorderWidth) * ButtonCount + FBorderWidth;
    //UnscaledBoundsRect := RectF(0, 0, ButtonWidth, (ButtonHeight - FBorderWidth) * ButtonCount + FBorderWidth);
  end;
end;
procedure TG2BtnRadio.CalcDimensions;
var Rect : TRectF;
    i : integer;
begin
  if FG2Updating then
    exit;
  inherited;
  SetLength(FRectArray, FButtonCount);
  if FBtnSelected >= FButtonCount then
    FBtnSelected := -1;
  if FButtonCount > 0 then begin
    if FOrientation = otHorizontal then begin
      Rect.Left := 0;
      Rect.Top := 0;
      Rect.Right := FButtonWidth;
      Rect.Bottom := FUnscaledHeight;
    end else begin
      Rect.Left := 0;
      Rect.Right := FUnscaledWidth;
      if FUpsideDown then begin
        Rect.Top := FUnscaledHeight - FButtonHeight;
        Rect.Bottom := FUnscaledHeight;
      end else begin
        Rect.Top := 0;
        Rect.Bottom := FButtonHeight;
      end;
    end;
    for i := 0 to FButtonCount - 1 do begin
       FRectArray[i] := Rect;
       if FOrientation = otHorizontal then begin
        Rect.Left := Rect.Left + FButtonWidth - FBorderWidth;
        Rect.Right := Rect.Right + FButtonWidth - FBorderWidth;
      end else begin
        if FUpsideDown then begin
          Rect.Top := Rect.Top - (FButtonHeight - FBorderWidth);
          Rect.Bottom := Rect.Bottom - (FButtonHeight - FBorderWidth);
        end else begin
          Rect.Top := Rect.Top + FButtonHeight - FBorderWidth;
          Rect.Bottom := Rect.Bottom + FButtonHeight - FBorderWidth;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------
//
//                             TG2BtnRadioEdit
//
//------------------------------------------------------------------------------
constructor TG2BtnRadioEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := True;
  FButtonWidth := 43;
  FButtonHeight := 12;
  //UnscaledWidth := FButtonWidth;
  //UnscaledHeight := FButtonHeight;
  Width := FButtonWidth;
  Height := FButtonHeight;
  FButtonRows := 1;
  FButtonColumns := 1;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnRadioEdit.Destroy;
begin
  inherited;
end;
procedure TG2BtnRadioEdit.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  UnscaledBoundsRect := RectF(0, 0, (ButtonWidth - FBorderWidth) * ButtonColumns + FBorderWidth, (ButtonHeight - FBorderWidth) * ButtonRows + FBorderWidth);
  CalcDimensions;
end;
function TG2BtnRadioEdit.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'ButtonColumns' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonColumns := StrToInt(string(aValue));
    end else
    if aName = 'ButtonRows' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonRows := StrToInt(string(aValue));
    end else
      Result := False
  end;
end;
procedure TG2BtnRadioEdit.SetButtonColumns(const aValue: integer);
begin
  if FButtonColumns <> aValue then begin
    FButtonColumns := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
function TG2BtnRadioEdit.GetButtonLabel(aIndex: integer): string;
begin
  if aIndex < FButtonText.Count then
    Result := FButtonText[ aIndex]
  else
    Result := '';
end;
procedure TG2BtnRadioEdit.SetButtonLabel(aIndex: integer; const aValue: string);
begin
  if aIndex < FButtonText.Count then
    FButtonText[aIndex] := aValue
  else begin
    while aIndex >= FButtonText.Count do
      FButtonText.Add('');
    FButtonText[aIndex] := aValue
  end;
  Redraw;
end;
procedure TG2BtnRadioEdit.SetButtonRows(const aValue: integer);
begin
  if FButtonRows <> aValue then begin
    FButtonRows := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2BtnRadioEdit.CalcDimensionsFromBtnSize(aBtnWidth,
  aBtnHeight: single);
begin
  FUnscaledWidth := (ButtonWidth - FBorderWidth) * ButtonColumns + FBorderWidth;
  FUnscaledHeight := (ButtonHeight - FBorderWidth) * ButtonRows + FBorderWidth;
  //UnscaledBoundsRect := RectF(0, 0, (ButtonWidth - FBorderWidth) * ButtonColumns + FBorderWidth, (ButtonHeight - FBorderWidth) * ButtonRows + FBorderWidth);
end;
procedure TG2BtnRadioEdit.CalcDimensions;
var Rect : TRectF;
    i, c, r : integer;
begin
  if FG2Updating then
    exit;
  if (FButtonColumns = 0) or (FButtonRows = 0) then
    exit;
  FButtonWidth := (UnscaledWidth - FBorderWidth)  / FButtonColumns + FBorderWidth;
  FButtonHeight := (UnscaledHeight - FBorderWidth) / FButtonRows + FBorderWidth;
  Rect.Top := 0;
  Rect.Bottom := FButtonHeight;
  FButtonCount := FButtonColumns * FButtonRows;
  SetLength(FRectArray, FButtonCount);
  i := 0;
  for r := 0 to FButtonRows - 1 do begin
    Rect.Left := 0;
    Rect.Right := FButtonWidth;
    for c := 0 to FButtonColumns - 1 do begin
      FRectArray[i] := Rect;
      inc(i);
      Rect.Left := Rect.Left + FButtonWidth - FBorderWidth;
      Rect.Right := Rect.Right + FButtonWidth;
    end;
    Rect.Top := Rect.Top + FButtonHeight - FBorderWidth;
    Rect.Bottom := Rect.Bottom + FButtonHeight;
  end;
end;

//------------------------------------------------------------------------------
//
//                              TG2BtnIncDec
//
//------------------------------------------------------------------------------
constructor TG2BtnIncDec.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTEst := True;
  FButtonWidth := 10;
  FButtonHeight := 11;
  FButtonCount := 2;
  FUpsideDown := False;
  FButtonKind := bkMomentary;
  //UnscaledWidth := FButtonWidth * FButtonCount;
  //UnscaledHeight := FButtonHeight;
  Width := FButtonWidth * FButtonCount;
  Height := FButtonHeight;
  FImageIDS.Add( IntToStr(ord(gsArrowLeft)));
  FImageIDS.Add( IntToStr(ord(gsArrowRight)));
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2BtnIncDec.Destroy;
begin
  inherited;
end;
{procedure TG2BtnIncDec.DoMouseLeave;
begin
  FBtnSelected := -1;
  Redraw;
  inherited;
end;}
procedure TG2BtnIncDec.DoMouseLeave;
begin
  if FMouseEnable then begin
    ProcessMouseLeave( 0);
    inherited;
  end;
end;
procedure TG2BtnIncDec.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseDown) then
    OnMouseDown( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseDown(Self, Shift, 0, X, Y);
end;
procedure TG2BtnIncDec.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
procedure TG2BtnIncDec.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcDimensions;
  CalcZoomedBounds;
end;
function TG2BtnIncDec.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Left/Right"' then begin
        FOrientation := otHorizontal;
      end else begin
        FOrientation := otVertical;
      end;
    end else
      Result := False
  end;
end;
procedure TG2BtnIncDec.ProcessMouseLeave( aTouchID : integer);
begin
  FBtnSelected := -1;
  Redraw;
end;
procedure TG2BtnIncDec.ProcessMouseDown(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
var i : integer;
begin
  if FState = csDisabled then
    exit;
  if Sender = Self then begin
    i := 0;
    while (i<Length(FRectArray)) and not(PtInRect(FRectArray[i], PointF(X{/Zoom},Y{/Zoom}))) do
      inc(i);
    if (i<Length(FRectArray)) then begin
      BtnSelected := i;
    end else
      BtnSelected := -1;
    inherited;
  end;
end;
procedure TG2BtnIncDec.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  //if not Selected then // prevent accidental click
  //if not IsFocused then // prevent accidental click
  //  exit;
  if FBtnSelected = -1 then
    exit;
  if Sender = Self then begin
    if FUpsideDown then begin
      if BtnSelected = 0 then
        IncValue
      else
        if BtnSelected = 1 then
          DecValue;
    end else begin
      if BtnSelected = 0 then
        DecValue
      else
        if BtnSelected = 1 then
          IncValue;
    end;
    SetValueDataDependencies( FValue);
    if assigned(FOnButtonClick) then
      FOnButtonClick(self, FBtnSelected);
    BtnSelected := -1;
  end;
end;
procedure TG2BtnIncDec.CalcDimensions;
begin
  if FG2Updating then
    exit;
  inherited;
  if FOrientation = otHorizontal then begin
    FImageIDS.Clear;
    FImageIDS.Add( IntToStr(ord(gsArrowLeft)));
    FImageIDS.Add( IntToStr(ord(gsArrowRight)));
  end else begin
    FImageIDS.Clear;
    FImageIDS.Add( IntToStr(ord(gsArrowUp)));
    FImageIDS.Add( IntToStr(ord(gsArrowDown)));
  end;
end;
//------------------------------------------------------------------------------
//
//                          TG2PartSelectorList
//
//------------------------------------------------------------------------------
constructor TG2PartSelectorList.Create(aPartSelector: TG2PartSelector);
begin
  inherited Create(aPartSelector);
  FPartSelector := aPartSelector;
  HitTest := True;
  Visible := aPartSelector.OptionsVisible;
  FOptionSelected := -1;
  SetLength(FRectArray,0);
end;
destructor TG2PartSelectorList.Destroy;
begin
  Finalize(FRectArray);
  inherited;
end;
procedure TG2PartSelectorList.PaintOn(aCanvas: TCanvas);
var i : integer;
    Rect : TRectF;
    StyleSet : TG2StyleSet;
    LabelText : String;
begin
  for i  := 0 to Length(FRectArray) - 1 do begin
    Rect := FRectArray[i];
    if i = FOptionSelected then
      StyleSet := FStateStyleList.StateStyle[ csSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceBackGround, i, StyleSet);
    StyleSet.ApplyStyleSet( aCanvas);
    aCanvas.FillRect(Rect, 0, 0, [], AbsoluteOpacity);
    if FPartSelector.FImageIDS.Count > 0 then begin
      if i < FPartSelector.FImageIDS.Count then begin
        if assigned(FOnPaintElement) then
          FOnPaintElement(self, ceSymbol, i, StyleSet);
        aCanvas.Fill.Color := StyleSet.FontColor;
        PathDataList.DrawSymbol( aCanvas, Rect, FPartSelector.FImageIDS[i]);
      end;
    end else begin
      LabelText := '';
      if i < FPartSelector.OptionsText.Count then
        LabelText := FPartSelector.OptionsText[i];
      if assigned(FOnPaintElement) then
        FOnPaintElement(self, ceText, i, StyleSet);
      DrawTextExt( aCanvas,
                   StyleSet.Font,
                   PointF(Rect.Left, Rect.Top),
                   PointF(Rect.Width, Rect.Height),
                   StyleSet.FontColor,
                   LabelText,
                   TTextAlign.taCenter, TTextAlign.taCenter);
    end;
  end;
  inherited;
end;
{procedure TG2PartSelectorList.DoMouseLeave;
begin
  FPartSelector.OptionsVisible := False;
  inherited;
end;}
procedure TG2PartSelectorList.DoMouseLeave;
begin
  if FMouseEnable then begin
    ProcessMouseLeave( 0);
    inherited;
  end;
end;
procedure TG2PartSelectorList.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if FMouseEnable then
    ProcessMouseMove( self, Shift, 0,  X, Y);
end;
procedure TG2PartSelectorList.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if FMouseEnable then
    ProcessMouseUp( self, shift, 0, X, Y);
end;
procedure TG2PartSelectorList.ProcessMouseLeave( aTouchID : integer);
begin
  FPartSelector.OptionsVisible := False;
end;
procedure TG2PartSelectorList.ProcessMouseMove(Sender: TObject;
  Shift: TShiftState; aTouchID : integer; X, Y: Single);
var i : integer;
begin
  i := 0;
  while (i<Length(FRectArray)) and not(PtInRect(FRectArray[i], PointF(X, Y))) do
    inc(i);
  if (i<Length(FRectArray)) then begin
    if (FOptionSelected <> i) then begin
      FOptionSelected := i;
      Redraw;
    end;
  end else begin
    i := -1;
    if (FOptionSelected <> i) then begin
      FOptionSelected := i;
      Redraw;
    end;
  end;
end;
procedure TG2PartSelectorList.ProcessMouseUp(Sender: TObject;
  Shift: TShiftState; aTouchID : integer; X, Y: Single);
begin
  FPartSelector.OptionsVisible := False;
  FPartSelector.Value := FOptionSelected;
  FPartSelector.SetValueDataDependencies( FPartSelector.FValue);
  //if assigned(FPartSelector.FOnChangeValue) then
  //  FPartSelector.FOnChangeValue(FPartSelector, FPartSelector.FValue);
end;
//------------------------------------------------------------------------------
//
//                             TG2PartSelector
//
//------------------------------------------------------------------------------
constructor TG2PartSelector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := True;
  CanFocus := True;
  FOptionsText := TStringList.Create;
  FBitmapData := TStringList.Create;
  FOptionListEnabled := True;
  FOptionsVisible := False;
  FOptionList := TG2PartSelectorList.Create(self);
  //UnscaledWidth := trunc(UNITS_ROW * 1.6);
  //UnscaledHeight := Width;
  Width := trunc(UNITS_ROW * 1.6);
  Height := Width;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2PartSelector.Destroy;
begin
  FBitmapData.Free;
  FOptionsText.Free;
  inherited;
end;
procedure TG2PartSelector.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 0, X, Y);
end;
procedure TG2PartSelector.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  if Sender = Self then begin
    if FOptionListEnabled then
      OptionsVisible := not OptionsVisible
    else begin
      RotateValue;
      SetValueDataDependencies( FValue);
    end;
  end;
end;
procedure TG2PartSelector.SetOptionListEnabled(const Value: boolean);
begin
  if FOptionListEnabled <> Value then begin
    FOptionListEnabled := Value;
  end;
end;
procedure TG2PartSelector.SetOptionsText(const aValue: TStrings);
begin
  if assigned(aValue) then begin
    FOptionsText.Assign(aValue);
    Redraw;
  end;
end;
procedure TG2PartSelector.SetOptionsVisible(const aValue: boolean);
var ImageHeight : single;
    ModuleLeft, ModuleTop : single;
begin
  if FOptionsVisible <> aValue then begin
    FOptionList.Visible := aValue;
    if aValue then begin
      if (Parent is TG2Module) and assigned(Parent.Parent) then begin
        FOptionList.Parent := Parent.Parent; // Scrollbox!
        ModuleLeft := (Parent as TG2Module).UnscaledLeft;
        ModuleTop := (Parent as TG2Module).UnscaledTop;
      end else begin
        FOptionList.Parent := Parent;
        ModuleLeft := 0.0;
        ModuleTop := 0.0;
      end;
      //ImageHeight := FBitmapData.Count / (FImageWidth * FImageCount);
      ImageHeight := Height;
      FOptionList.SetBoundsRect(RectF(ModuleLeft + UnscaledLeft,
                                      ModuleTop + UnscaledTop,
                                      ModuleLeft + UnscaledLeft + FImageWidth, //System.Math.Max(UnscaledWidth, FImageWidth),
                                      ModuleTop + UnscaledTop + UnscaledHeight + ImageHeight * FItemCount));
      FOptionList.BringToFront;
    end;
    FOptionsVisible := aValue;
  end;
end;
procedure TG2PartSelector.PaintOn( aCanvas : TCanvas);
var StyleSet : TG2StyleSet;
    LabelText : string;
begin
  if IsFocused then begin
    if FSelected then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csFocused];
  end else begin
    if FSelected then
      StyleSet := FStateStyleList.StateStyle[ csSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  end;
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, Value, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.FillRect(FDisplayRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect(FDisplayRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.FillRect(FBtnRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect(FBtnRect, 0, 0, [], AbsoluteOpacity);
  aCanvas.Fill.Color := StyleSet.FontColor;
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceSymbol, Value, StyleSet);
  PathDataList.DrawSymbol( aCanvas, FBtnRect, IntToStr(ord(gsArrowDown)));
  if FImageIDS.Count > 0 then begin
    if (Value >=0) and (Value < FImageIDS.Count) then begin
      PathDataList.DrawSymbol( aCanvas, FDisplayRect, FImageIDS[Value]);
    end;
  end else begin
    LabelText := '';
    if Value < FOptionsText.Count then
      LabelText := FOptionsText[Value];
    DrawTextExt( aCanvas,
                 StyleSet.Font,
                 PointF(FDisplayRect.Left, FDisplayRect.Top),
                 PointF(FDisplayRect.Width, FDisplayRect.Height),
                 StyleSet.FontColor,
                 LabelText,
                 TTextAlign.taCenter, TTextAlign.taCenter);
  end;
  inherited;
end;
procedure TG2PartSelector.ParsePanelData(fs: TModuleDefStream);
var Rect : TRectF;
    i, ItemCount : integer;
    ImageHeight, ImageWidth : single;
    BtnWidth : single;
begin
  inherited;
  CalcDimensions;
  CalcZoomedBounds;
end;
{procedure TG2PartSelector.EndUpdate;
var Rect : TRectF;
    i : integer;
    ImageHeight : single;
    BtnWidth : single;
begin
  inherited;
  ImageHeight := Height;
  BtnWidth := 8;
  FBtnRect     := RectF( UnscaledWidth-BtnWidth, 0, UnscaledWidth, UnscaledHeight);
  FDisplayRect := RectF( 0, 0, UnscaledWidth - BtnWidth, UnscaledHeight);
  SetLength(FOptionList.FRectArray, FImageCount);
  Rect := RectF(0, UnscaledHeight, FImageWidth, UnscaledHeight + ImageHeight);
  for i := 0 to Length(FOptionList.FRectArray) - 1 do begin
    FOptionList.FRectArray[i] := Rect;
    Rect.Top := Rect.Top + ImageHeight;
    Rect.Bottom := Rect.Bottom + ImageHeight;
  end;
end;}
procedure TG2PartSelector.CalcDimensions;
var Rect : TRectF;
    i : integer;
    ImageHeight : single;
    BtnWidth : single;
begin
  if FG2Updating then
    exit;
  BtnWidth := 8;
  ImageHeight := Height;
  FImageWidth := UnscaledWidth-BtnWidth;
  FBtnRect     := RectF( FImageWidth, 0, UnscaledWidth, UnscaledHeight);
  FDisplayRect := RectF( 0, 0, FImageWidth, UnscaledHeight);

  if FImageIDS.Count > 0 then
    FItemCount := FImageIDS.Count
  else
    FItemCount := FOptionsText.Count;
  SetLength(FOptionList.FRectArray, FItemCount);
  Rect := RectF(0, UnscaledHeight, FImageWidth, UnscaledHeight + ImageHeight);
  for i := 0 to Length(FOptionList.FRectArray) - 1 do begin
    FOptionList.FRectArray[i] := Rect;
    Rect.Top := Rect.Top + ImageHeight;
    Rect.Bottom := Rect.Bottom + ImageHeight;
  end;
  inherited;
end;
function TG2PartSelector.ParseProperties(fs: TModuleDefStream;
  aName: String): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'Text' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FOptionsText, [','], ['"']);
    end else
    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FBitmapData, [':'], ['"']);
    end else
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      //FParameter := ModuleData.Mode[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
      CodeRef := StrToInt(string(aValue));
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      InfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'ImageWidth' then begin
      aValue := fs.ReadUntil( [#13]);
      FImageWidth := StrToInt(string(aValue));
    end else
    if aName = 'ImageCount' then begin
      aValue := fs.ReadUntil( [#13]);
      FItemCount := StrToInt(string(aValue));
    end else
    if aName = 'MenuOffset' then begin
      aValue := fs.ReadUntil( [#13]);
      // TODO
    end else
      Result := False
  end;
end;
//------------------------------------------------------------------------------
//
//                           TG2ResetButton
//
//------------------------------------------------------------------------------
constructor TG2ResetButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CanFocus := False;
  //AutoCapture := True;
  HitTest := False;
  //FUnscaledWidth := 10;
  //FUnscaledHeight := 4;
  Width := 10;
  Height := 4;
  FCentered := True;
end;
destructor TG2ResetButton.Destroy;
begin
  inherited;
end;
procedure TG2ResetButton.CalcDimensions;
begin
  //
end;
procedure TG2ResetButton.PaintOn( aCanvas : TCanvas);
var poly : TPolygon;
begin
  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claBlue;
  aCanvas.Fill.Kind := TBrushKind.bkSolid;
  if FCentered then begin
    aCanvas.Fill.Color := claBlue;
  end else begin
    aCanvas.Fill.Color := claWhite;
  end;
  SetLength(poly, 4);
  poly[0] := PointF(0,0);
  poly[1] := PointF(UnscaledWidth,0);
  poly[2] := PointF(UnscaledWidth/2, UnscaledHeight);
  poly[3] := PointF(0,0);
  aCanvas.FillPolygon(poly, AbsoluteOpacity);
  aCanvas.DrawPolygon(poly, AbsoluteOpacity);
  inherited;
end;
//------------------------------------------------------------------------------
//
//                             TG2Knob
//
//------------------------------------------------------------------------------
constructor TG2Knob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HitTest := True;
  FKnobType := ktBig;
  FOrientation := otVertical;
  FSliderKnobSize := 8;
  CanFocus := True;
  FKnobDownPart := kdpNone;
  FDefaultSliderKnobStyles := TG2StateStyleList.Create(self);
  FDefaultSliderKnobStyles.Name := 'DefaultSliderKnobStyles';
  FDefaultSliderKnobStyles.SetSubComponent(True);
  FSliderKnobStyles := FDefaultSliderKnobStyles;
  FDefaultSliderKnobStyles.StateStyle[ csDefault].FFill.Color := claBlack;
  FDefaultSliderKnobStyles.StateStyle[ csFocused].FFill.Color := claBlue;
  FDefaultSliderKnobStyles.StateStyle[ csDisabled].FFill.Color := claGray;
  FDefaultSliderKnobStyles.StateStyle[ csSelected].FFill.Color := claBlue;
  FKnobReset := TG2ResetButton.Create(self);
  FKnobReset.Name := 'ResetBtn';
  FKnobReset.SetSubComponent(True);
  FKnobReset.Parent := Self;
  FKnobReset.Stored := False; // Otherwise there's a "argument out of range" error when droppen on a frame
  FKnobReset.Locked := True;
  FKnobReset.CanFocus := False;
  FKnobIncDecBtns := TG2BtnIncDec.Create(self);
  FKnobIncDecBtns.Name := 'IncDecBtns';
  FKnobIncDecBtns.SetSubComponent(True);
  FKnobIncDecBtns.Parent := Self;
  FKnobIncDecBtns.Stored := False; // Otherwise there's a "argument out of range" error when droppen on a frame
  FKnobIncDecBtns.Locked := True;
  FKnobIncDecBtns.HitTest := False;
  FKnobIncDecBtns.UpsideDown := (FOrientation = otVertical)
                            and (FKnobType in [ktSlider, ktSeqSlider]);
  FKnobIncDecBtns.CanFocus := False;
  FKnobControl := kcCircular;
  SetDefaultDimensions;
  CalcDimensions;
  CalcZoomedBounds;
end;
destructor TG2Knob.Destroy;
begin
  //FDefaultSliderKnobStyles.Free;
  inherited;
end;
procedure TG2Knob.DoEnter;
begin
  inherited;
  if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
    FKnobIncDecBtns.Visible := True;
  end;
  FKnobIncDecBtns.State := csFocused;
end;
procedure TG2Knob.DoExit;
begin
  inherited;
  if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
    FKnobIncDecBtns.Visible := False;
  end;
  FKnobIncDecBtns.State := csDefault;
end;
procedure TG2Knob.DoMouseEnter;
begin
  inherited;
  if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
    if assigned(FKnobIncDecBtns) then begin
      FKnobIncDecBtns.Visible := True;
      Redraw;
    end;
  end;
end;
{procedure TG2Knob.DoMouseLeave;
begin
  inherited;
  if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
    if assigned(FKnobIncDecBtns) then begin
      FKnobIncDecBtns.BtnSelected := -1;
      FKnobIncDecBtns.Visible := IsFocused;//Selected;
      Redraw;
    end;
  end;
end;}
procedure TG2Knob.DoMouseLeave;
begin
  if FMouseEnable then begin
    inherited;
    ProcessMouseLeave( 0);
  end;
end;
procedure TG2Knob.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseDown) then
    OnMouseDown( Self, Button, Shift, X, Y)
  else begin
    if FMouseEnable then
      ProcessMouseDown(Self, Shift, 0, X, Y);
    //inherited; // enable autocapture
  end;
end;
procedure TG2Knob.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if assigned(OnMouseMove) then
    OnMouseMove(Self, Shift, X, Y)
  else begin
    if FMouseEnable then
      ProcessMouseMove(Self, Shift, 0, X, Y);
  end;
end;
procedure TG2Knob.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp(Self, Button, Shift, X, Y)
  else begin
    if FMouseEnable then
      ProcessMouseUp( Self, Shift, 0, X, Y);
  end;
end;
procedure TG2Knob.ProcessMouseLeave( aTouchID : integer);
begin
  if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
    if assigned(FKnobIncDecBtns) then begin
      FKnobIncDecBtns.BtnSelected := -1;
      FKnobIncDecBtns.Visible := IsFocused;//Selected;
      Redraw;
    end;
  end;
end;
procedure TG2Knob.ProcessMouseDown(Sender: TObject; Shift: TShiftState;
   aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  //if Autocapture then
  //  Capture;
  if (ssLeft in Shift) or (ssTouch in Shift) then begin
    FKnobDownPart := kdpNone;
    if assigned(FKnobReset) and FKnobReset.Visible and PtInRect( FKnobReset.UnscaledRect, PointF(X{/Zoom}, Y{/Zoom})) then begin
      FKnobDownPart := kdpBtnReset;
      NormValue := 0.5;
      SetValueDataDependencies( FValue);
      //if assigned(FOnChangeValue) then
      //  FOnChangeValue(self, FValue);
    end else
      if assigned(FKnobIncDecBtns) and PtInRect( FKnobIncDecBtns.UnscaledRect, PointF(X{/Zoom}, Y{/Zoom})) then begin
        FKnobDownPart := kdpBtnIncDec;
        FKnobIncDecBtns.ProcessMouseDown(FKnobIncDecBtns, Shift, aTouchID, X - FKnobIncDecBtns.Position.X, Y - FKnobIncDecBtns.Position.Y);
      end else
        if (FKnobType = ktSlider) or (FKnobType = ktSeqSlider) then begin
          if PtInRect( CalcSLiderKnobRect, PointF(X{/Zoom}, Y{/Zoom})) then
            FKnobDownPart := kdpKnob;
        end else
          FKnobDownPart := kdpKnob;
    FKnobDownPointAbs := LocalToAbsolute(PointF(X, Y));
    FKnobDownPoint := PointF(X, Y);
    FKnobDownNormValue := NormValue;
    FKnobDownNormMorphValue := NormMorphValue;
    FKnobDownMorph := (ssCtrl in Shift);
    inherited;
  end;
end;
procedure TG2Knob.ProcessMouseMove(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
var d, angle, NewValue : single;
    C : TPointF;
begin
  if FState = csDisabled then
    exit;
  if (ssLeft in Shift) or (ssTouch in Shift) then begin
    if FKnobDownPart <> kdpKnob then
      exit;
    d := 0;
    if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
      case FKnobControl of
        kcCircular:
          begin
            if ((X - FCX)*(X - FCX) + (Y - FCY)*(Y - FCY))>FR*FR then begin
              C := PointF(FCX, FCY);
              angle := Arctan2(PointF(X - C.X, Y - C.Y)) - PI/2;
              if angle < 0 then
                angle := angle + 2*PI;
              if FKnobDownMorph then begin
                d := (Angle/(2*PI) - 0.1)/0.8 - (NormValue + NormMorphValue);
                if abs(d) > 0.75 then
                  exit;
                if NormMorphValue + d > 1 then
                  NormMorphValue := 1
                else
                  if NormMorphValue + d < -1 then
                    NormMorphValue := -1
                  else
                    NormMorphValue := NormMorphValue + d;

                SetMorphValueDataDependencies( FMorphValue);
              end else begin
                d := (Angle/(2*PI) - 0.1)/0.8 - NormValue;
                if abs(d) > 0.75 then
                  exit;
                if NormValue + d > 1 then
                  NormValue := 1
                else
                  if NormValue + d < 0 then
                    NormValue := 0
                  else
                    NormValue := NormValue + d;

                SetValueDataDependencies( FValue);

              end;

            end;

          end;
        kcHorizontal:
          begin
            if FKnobDownMorph then begin
              d := (X - FKnobDownPoint.X) / 100;
              if FKnobDownNormMorphValue + d > 1 then
                NormMorphValue := 1
              else
                if FKnobDownNormMorphValue + d < -1 then
                  NormMorphValue := -1
                else
                  NormMorphValue := FKnobDownNormMorphValue + d;
              SetMorphValueDataDependencies( FMorphValue)
            end else begin
              d := (X - FKnobDownPoint.X) / 100;
              if FKnobDownNormValue + d > 1 then
                NormValue := 1
              else
                if FKnobDownNormValue + d < 0 then
                  NormValue := 0
                else
                  NormValue := FKnobDownNormValue + d;
              SetValueDataDependencies( FValue);
            end;
          end;
        kcVertical:
          begin
            if FKnobDownMorph then begin
              d := (Y - FKnobDownPoint.Y) / 100;
              if FKnobDownNormMorphValue + d > 1 then
                NormMorphValue := 1
              else
                if FKnobDownNormMorphValue + d < -1 then
                  NormMorphValue := -1
                else
                  NormMorphValue := FKnobDownNormMorphValue + d;
              SetMorphValueDataDependencies( FMorphValue);
            end else begin
              d := (Y - FKnobDownPoint.Y) / 100;
              if FKnobDownNormValue + d > 1 then
                NormValue := 1
              else
                if FKnobDownNormValue + d < 0 then
                  NormValue := 0
                else
                  NormValue := FKnobDownNormValue + d;
              SetValueDataDependencies( FValue);
            end;
          end;
      end;
    end else begin
      if FKnobDownMorph then begin
        C := PointF(0, 0);
        if FOrientation = otVertical then begin
          if (FSliderRect.Bottom - FSLiderKnobRect.Bottom) <> 0 then begin
            NewValue := 1 - (Y - (FSLiderKnobRect.Bottom - FSLiderKnobRect.Top)/2 - C.Y) / (FSliderRect.Bottom - FSLiderKnobRect.Bottom);
            NormMorphValue := System.Math.Max(System.Math.Min(1, NewValue - NormValue),-1);
          end;
        end else begin
          if (FSliderRect.Right - FSLiderKnobRect.Right) <> 0 then begin
            NewValue := (X - (FSLiderKnobRect.Right - FSLiderKnobRect.Left)/2 - C.X) / (FSliderRect.Right - FSLiderKnobRect.Right);
            NormMorphValue := System.Math.Max(System.Math.Min(1, NewValue - NormValue),-1);
          end;
        end;
        SetMorphValueDataDependencies( FMorphValue);
      end else begin
        C := PointF(0, 0);
        if FOrientation = otVertical then begin
          if (FSliderRect.Bottom - FSLiderKnobRect.Bottom) <> 0 then begin
            NewValue := 1 - (Y - (FSLiderKnobRect.Bottom - FSLiderKnobRect.Top)/2 - C.Y) / (FSliderRect.Bottom - FSLiderKnobRect.Bottom);
            NormValue := System.Math.Max(System.Math.Min(1, NewValue),0);
          end;
        end else begin
          if (FSliderRect.Right - FSLiderKnobRect.Right) <> 0 then begin
            NewValue := (X - (FSLiderKnobRect.Right - FSLiderKnobRect.Left)/2 - C.X) / (FSliderRect.Right - FSLiderKnobRect.Right);
            NormValue := System.Math.Max(System.Math.Min(1, NewValue),0);
          end;
        end;
        SetValueDataDependencies( FValue);
      end;
    end;
  end;
end;
procedure TG2Knob.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
    aTouchID : integer; X, Y: Single);
begin
  if FState = csDisabled then
    exit;
  //if Autocapture then
  //  ReleaseCapture;
  FKnobDownPart := kdpNone;
  if Sender = Self then begin
    if assigned(FKnobReset) and (FKnobReset.Visible) and PtInRect( FKnobReset.UnscaledRect, PointF(X/Zoom, Y/Zoom)) then begin
      //NormValue := 0.5;
      //if assigned(FOnChangeValue) then
      //  FOnChangeValue(self, FValue);
    end else
      if assigned(FKnobIncDecBtns) then begin
        FKnobIncDecBtns.LowValue := LowValue;
        FKnobIncDecBtns.HighValue := HighValue;
        FKnobIncDecBtns.Value := Value;
        FKnobIncDecBtns.ProcessMouseUp(FKnobIncDecBtns, Shift, aTouchID, X - FKnobIncDecBtns.Position.X, Y - FKnobIncDecBtns.Position.Y);
        Value := FKnobIncDecBtns.Value;
        SetValueDataDependencies( FValue);
        //if assigned(FOnChangeValue) then
        //  FOnChangeValue(self, FValue);
      end;
  end;
end;
{$IF Defined(VER280) or Defined(VER340)}
procedure TG2Knob.DoBeginUpdate;
begin
  inherited;
  FKnobIncDecBtns.FG2Updating := True;
  FKnobReset.FG2Updating := True;
end;
procedure TG2Knob.DoEndUpdate;
begin
  inherited;
  FKnobIncDecBtns.FG2Updating := False;
  FKnobReset.FG2Updating := False;
end;
{$ELSE}
procedure TG2Knob.BeginUpdate;
begin
  inherited;
  FKnobIncDecBtns.FG2Updating := True;
  FKnobReset.FG2Updating := True;
end;
procedure TG2Knob.EndUpdate;
begin
  inherited;
  FKnobIncDecBtns.FG2Updating := False;
  FKnobReset.FG2Updating := False;
end;
{$ENDIF}
procedure TG2Knob.Redraw;
begin
  FKnobIncDecBtns.Redraw;
  FKnobReset.Redraw;
  inherited;
end;
function TG2Knob.GetSliderKnobStyles: TG2StateStyleList;
begin
  Result := FSliderKnobStyles;
end;
function TG2Knob.CalcSliderKnobRect: TRectF;
var f, h, range : single;
begin
  f :=  NormValue;
  if FOrientation = otVertical then begin
    h := FSliderKnobRect.Bottom;
    range := FSLiderRect.Bottom - h;
    Result := RectF(0, range - f * range, FSliderKnobRect.Right, range - f * range + h);
  end else begin
    h := FSliderKnobRect.Right;
    range := FSLiderRect.Right - h;
    Result := RectF(f * range, 0, f * range + h, FSliderKnobRect.Bottom);
  end;
end;
procedure TG2Knob.PaintOn( aCanvas : TCanvas);
var Angle, StartAngle, SweepAngle : single;
    f, KnobSize : single;
    PathData : TPathData;
    StyleSet : TG2StyleSet;
    MorphRect, R : TRectF;
begin
  //aCanvas.Clear( TAlphaColorRec.White);
  if FBuffered then
      aCanvas.ClearRect( UnscaledBoundsRect);
  if IsFocused then begin
    if Selected then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csFocused]
  end else
    if Selected then
      StyleSet := FStateStyleList.StateStyle[ csSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  if (FKnobType = ktSlider) or (FKnobType = ktSeqSlider) then begin

    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceBackGround, 0, StyleSet);
    StyleSet.ApplyStyleSet( aCanvas);
    aCanvas.FillRect( FSliderRect, 0, 0, [], AbsoluteOpacity);
    aCanvas.DrawRect( FSliderRect, 0, 0, [], AbsoluteOpacity);
    f := NormValue;
    if NormMorphValue <> 0 then begin
      MorphRect := FSliderRect;
      if FOrientation = otVertical then begin
        KnobSize := FSliderKnobRect.Height;
        MorphRect.Top := FSliderRect.Bottom - (FSliderRect.Bottom - KnobSize) * f - KnobSize/2;
        MorphRect.Bottom := System.Math.Max(MorphRect.Top - (FSliderRect.Bottom - KnobSize) * NormMorphValue , FSliderRect.Top);
      end else begin
        KnobSize := FSliderKnobRect.Width;
        MorphRect.Left := (FSliderRect.Right - KnobSize) * f + KnobSize/2;
        MorphRect.Right := System.Math.Min(MorphRect.Left + (FSliderRect.Right - KnobSize) * NormMorphValue, FSLiderRect.Right);
      end;
      aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
      aCanvas.FillRect( MorphRect, 0, 0, [], 0.5);
    end else
      if HasMorph then begin
        aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
        aCanvas.FillRect( FSliderRect, 0, 0, [], 0.5);
      end;
    StyleSet := FSliderKnobStyles.StateStyle[ FState];
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceSliderKnob, 0, StyleSet);
    StyleSet.ApplyStyleSet( aCanvas);
    R := CalcSliderKnobRect;
    aCanvas.FillRect( R , 0, 0, [], AbsoluteOpacity);
    aCanvas.DrawRect( R, 0, 0, [], AbsoluteOpacity);
  end else begin
    //StyleSet := FStateStyleList.StateStyle[ FState];
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceBackGround, 0, StyleSet);
    StyleSet.ApplyStyleSet( aCanvas);
    aCanvas.FillRect( FSliderRect, 0, 0, [], AbsoluteOpacity);
    aCanvas.DrawRect( FSliderRect, 0, 0, [], AbsoluteOpacity);
    if assigned(FOnPaintElement) then
      FOnPaintElement(self, ceBackground, 0, StyleSet);
    StyleSet.ApplyStyleSet(aCanvas);
    aCanvas.FillEllipse( RectF(FCX-FR, FCY-FR, FCX+FR, FCY+FR), AbsoluteOpacity);
    aCanvas.DrawEllipse( RectF(FCX-FR, FCY-FR, FCX+FR, FCY+FR), AbsoluteOpacity);
    Angle := 2*PI * ( 0.8 * NormValue + 0.1);
    if NormMorphValue <> 0 then begin
      StartAngle := Angle*180/PI;
      SweepAngle := 360 * ( 0.8 * NormMorphValue);
      if SweepAngle + StartAngle < 0.1*360 then
        SweepAngle := 0.1*360 - (StartAngle);
      if SweepAngle + StartAngle > 0.9*360 then
        SweepAngle := 0.9*360 - (StartAngle);
      aCanvas.Fill.Color := StyleSet.ActiveMorphColor;
      PathData := TPathData.Create;
      try
        PathData.MoveTo( PointF(FCX, FCY));
        PathData.LineTo( PointF(FCX-sin(Angle)*FR, FCY+cos(Angle)*FR));
        PathData.AddArc( PointF(FCX, FCY), PointF(FR, FR), StartAngle+90, SweepAngle);
        PathData.ClosePath;
        aCanvas.FillPath( PathData, 0.5);
      finally
        PathData.Free;
      end;
    end else
      if HasMorph then begin
        aCanvas.Fill.Color := StyleSet.InActiveMorphColor;
        aCanvas.FillEllipse( RectF(FCX-FR, FCY-FR, FCX+FR, FCY+FR), 0.5);
      end;
    aCanvas.DrawLine( PointF(FCX, FCY), PointF(FCX-sin(Angle)*FR, FCY+cos(Angle)*FR), AbsoluteOpacity);
{    if assigned(FKnobReset) then begin
      if Value = 64 then
        FKnobReset.FCentered := True
      else
        FKnobReset.FCentered := False;
      FKnobReset.Redraw;
    end;
}
  end;
  inherited;
end;
procedure TG2Knob.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
  CalcDimensions;
  CalcZoomedBounds;
end;
function TG2Knob.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      //FParameter := FModuleData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameterFMX;
      //(FParameter as TG2GraphParameterFMX).AssignControl(self);
      CodeRef := StrToInt(string(aValue));
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      InfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Reset"' then begin
        FKnobType := ktReset;
      end;
      if aValue = '"Reset/medium"' then begin
        FKnobType := ktResetMedium;
      end;
      if aValue = '"Small"' then begin
        FKnobType := ktSmall;
      end;
      if aValue = '"Medium"' then begin
        FKnobType := ktMedium;
      end;
      if aValue = '"Big"' then begin
        FKnobType := ktBig;
      end;
      if aValue = '"Slider"' then begin
        FKnobType := ktSlider;
      end;
      if aValue = '"SeqSlider"' then begin
        FKnobType := ktSeqSlider;
      end;
      SetDefaultDimensions;
      CalcZoomedBounds;
    end else
      Result := False
  end;
end;
procedure TG2Knob.CalcDimensions;
  procedure UpdateKnobBtns;
  begin
    if not assigned(FKnobIncDecBtns) then
      exit;
    FKnobIncDecBtns.Orientation := otHorizontal;
    FKnobIncDecBtns.FImageIDS.Clear;
    FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowDown)));
    FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowUp)));
    FKnobIncDecBtns.OnMouseMove := OnMouseMove;
    FKnobIncDecBtns.Visible := False;
  end;
  procedure UpdateSliderBtns;
  begin
    if not assigned(FKnobIncDecBtns) then
      exit;
    if FOrientation = otVertical then begin
      FKnobIncDecBtns.Orientation := otVertical;
      FKnobIncDecBtns.UpsideDown := True;
      FKnobIncDecBtns.FImageIDS.Clear;
      FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowUp)));
      FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowDown)));
      FKnobIncDecBtns.UnscaledRect := RectF(0, FSliderRect.Bottom + 1, FSliderRect.Right, FUnscaledHeight{ FSliderRect.Bottom + 17});
    end else begin
      FKnobIncDecBtns.Orientation := otHorizontal;
      FKnobIncDecBtns.UpsideDown := False;
      FKnobIncDecBtns.FImageIDS.Clear;
      FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowLeft)));
      FKnobIncDecBtns.FImageIDS.Add( IntToStr(ord(gsArrowRight)));
      FKnobIncDecBtns.UnscaledRect := RectF(FSliderRect.Right + 1, 0, FUnscaledWidth{FSliderRect.Right + 17}, FSliderRect.Bottom);
    end;
    FKnobIncDecBtns.OnMouseMove := OnMouseMove;
    FKnobIncDecBtns.Visible := True;
  end;
begin
  if FG2Updating then
    exit;
  inherited;
  case FKnobType of
    ktReset: // Width 18, Height 26, Radius 9, Reset button: Width 10, Height 4
      begin
        FR := FUnscaledWidth/2;
        FCX := FR;
        FCY := FR + FR*4/9 + 1;
        if assigned(FKnobReset) then begin
          FKnobReset.UnscaledRect := RectF(FR-FR*10/18, 0, FR+FR*10/18, FR*4/9);
          FKnobReset.OnMouseMove := OnMouseMove;
          FKnobReset.Visible := TRue;
        end;
        if assigned(FKnobIncDecBtns) then begin
          UpdateKnobBtns;
          FKnobIncDecBtns.UnscaledRect := RectF(FR-FR*10.5/9, UnscaledHeight-FR*9/9, FR+FR*10.5/9, UnscaledHeight);
        end;
      end;
    ktResetMedium: // Width 20, Height 30, Radius 10, Reset button: Width 10, Height 4
      begin
        FR := FUnscaledWidth / 2;
        FCX := FR;
        FCY := FR + FR*4/10 + 1;
        if assigned(FKnobReset) then begin
          FKnobReset.UnscaledRect := RectF(FR-FR*10/20, 0, FR+FR*10/20, FR*4/10);
          FKnobReset.OnMouseMove := OnMouseMove;
          FKnobReset.Visible := True;
        end;
        if assigned(FKnobIncDecBtns) then begin
          UpdateKnobBtns;
          FKnobIncDecBtns.UnscaledRect := RectF(FR-FR*10.5/10, UnscaledHeight-FR*9/10, FR+FR*10.5/10, UnscaledHeight);
        end;
      end;
    ktSmall: // Width 18, Height 22, Radius 9
      begin
        FR := UnscaledWidth / 2;
        FCX := FR;
        FCY := FR;
        if assigned(FKnobReset) then
          FKnobReset.Visible := False;
        if assigned(FKnobIncDecBtns) then begin
          UpdateKnobBtns;
          FKnobIncDecBtns.UnscaledRect := RectF(FR-FR*10.5/9, UnscaledHeight-FR*9/9, FR+FR*10.5/9, UnscaledHeight);
        end;
      end;
    ktMedium: // Width 20, Height 24, Radius 10
      begin
        FR := UnscaledWidth / 2;
        FCX := FR;
        FCY := FR;
        if assigned(FKnobReset) then
          FKnobReset.Visible := False;
        if assigned(FKnobIncDecBtns) then begin
          UpdateKnobBtns;
          FKnobIncDecBtns.UnscaledRect := RectF(FR-FR*10.5/10, UnscaledHeight-FR*9/10, FR+FR*10.5/10, UnscaledHeight);
        end;
      end;
    ktBig: // Width 22, Height 26, Radius 11
      begin
        FR := UnscaledWidth / 2;
        FCX := FR;
        FCY := FR;
        if assigned(FKnobReset) then
          FKnobReset.Visible := False;
        if assigned(FKnobIncDecBtns) then begin
          UpdateKnobBtns;
          FKnobIncDecBtns.UnscaledRect := RectF(FR-FR*11/11, UnscaledHeight-FR*9/11, FR+FR*11/11, UnscaledHeight);
        end;
      end;
    ktSlider: // width 11, height 45, buttons 17
      begin
        if FOrientation = otHorizontal then begin
          FSliderRect := RectF(0, 0, 45 * UnscaledWidth / (45+17), UnscaledHeight);
          FSliderKnobRect := RectF(0, 0, FSliderKnobSize, UnscaledHeight);
        end else begin
          FSliderRect := RectF(0, 0, UnscaledWidth, 45 * UnscaledHeight / (45+17));
          FSliderKnobRect := RectF(0, 0, UnscaledWidth, FSliderKnobSize);
        end;
        if assigned(FKnobIncDecBtns) then
          UpdateSliderBtns;
        if assigned(FKnobReset) then
          FKnobReset.Visible := False;
      end;
    ktSeqSlider: // width 11, height ?, buttons 17
      begin
        if FOrientation = otHorizontal then begin
          FSliderRect := RectF(0, 0, 62 * UnscaledWidth / (62+17), UnscaledHeight);
          FSliderKnobRect := RectF(0, 0, FSliderKnobSize, UnscaledHeight);
        end else begin
          FSliderRect := RectF(0, 0, UnscaledWidth, 62 * UnscaledHeight / (62+17));
          FSliderKnobRect := RectF(0, 0, UnscaledWidth, FSliderKnobSize);
        end;
        if assigned(FKnobIncDecBtns) then
          UpdateSliderBtns;
        if assigned(FKnobReset) then
          FKnobReset.Visible := False;
      end;
  end;
end;
procedure TG2Knob.SetBuffered(const Value: boolean);
begin
  inherited;
  FKnobIncDecBtns.Buffered := Value;
  FKnobReset.Buffered := Value;
end;
procedure TG2Knob.SetZoom(const Value: single);
begin
  inherited;
  FKnobIncDecBtns.Zoom := Value;
  FKnobReset.Zoom := Value;
end;
procedure TG2Knob.SetDefaultDimensions;
begin
  case FKnobType of
    ktBig:
      begin
        FUnscaledWidth := 22;
        FUnscaledHeight := 26;
      end;
    ktMedium:
      begin
        FUnscaledWidth := 20;
        FUnscaledHeight := 24;
      end;
    ktSmall:
      begin
        FUnscaledWidth := 18;
        FUnscaledHeight := 22;
      end;
    ktExtraSmall:
      begin
      end;
    ktReset:
      begin
        FUnscaledWidth := 18;
        FUnscaledHeight := 26;
      end;
    ktResetMedium:
      begin
        FUnscaledWidth := 20;
        FUnscaledHeight := 30;
      end;
    ktSlider:
      begin
        FUnscaledWidth := 11;
        FUnscaledHeight := 45 + 17;
      end;
    ktSeqSlider:
      begin
        FUnscaledWidth := 11;
        FUnscaledHeight := 62 + 17;
      end;
    ktNone:
      begin
      end;
  end;
  CalcDimensions;
end;
procedure TG2Knob.SetKnobType(const aValue: TKnobType);
begin
  if FKnobType <> aValue then begin
    FKnobType := aValue;
    CalcDimensions;
  end;
end;
procedure TG2Knob.SetMorphColor(const aValue: TAlphaColor);
begin
  if FMorphColor <> aValue then begin
    FMorphColor := aValue;
    Redraw;
  end;
end;
procedure TG2Knob.SetOrientation(const aValue: TOrientationType);
begin
  if FOrientation <> aValue then begin
    FOrientation := aValue;
    FKnobIncDecBtns.UpsideDown :=
          (FOrientation = otVertical)
      and (FKnobType in [ktSlider, ktSeqSlider]);
    CalcDimensions;
  end;
end;
procedure TG2Knob.SetSelected(const aValue: boolean);
begin
  inherited;
  if assigned(FKnobIncDecBtns) then begin
    FKnobIncDecBtns.Selected := Selected;
    if (FKnobType <> ktSlider) and (FKnobType <> ktSeqSlider) then begin
      FKnobIncDecBtns.Visible := Selected;
    end;
  end;
end;
procedure TG2Knob.SetSliderKnobSize(const aValue: single);
begin
  if FSLiderKnobSize <> aValue then begin
    FSliderKnobSize := aValue;
    CalcDimensions;
    Redraw;
  end;
end;
procedure TG2Knob.SetSliderKnobStyles(const aValue: TG2StateStyleList);
begin
  if FSliderKnobStyles <> aValue then begin
    if assigned(aValue) then
      FSliderKnobStyles := aValue
    else
      FSLiderKnobStyles := FDefaultSliderKnobStyles;
  end;
end;
procedure TG2Knob.SetState(const aValue: TControlState);
begin
  inherited;
  if assigned(FKnobIncDecBtns) then begin
    FKnobIncDecBtns.State := State;
  end;
end;
//------------------------------------------------------------------------------
//
//                             TSVGG2Connector
//
//------------------------------------------------------------------------------
constructor TG2Connector.Create(AOwner: TComponent);
var hw : single;
begin
  inherited;
  HitTest := True;
  //CanFocus := True;
  FConnectorIndex := -1;
  FConnectorKind := ckInput;
  FConnectorType := ctAudio;
  FBandWidth := btStatic;
  FConnectorColor := COLOR_WHITE;
  FConnected := False;
  //UnscaledWidth := 13;
  //UnscaledHeight := 13;
  //hw := UnscaledWidth/4;
  Width := 13;
  Height := 13;
  hw := Width/4;

  FHoleRect := RectF(hw, hw, UnscaledWidth-hw, UnscaledHeight-hw);
end;
destructor TG2Connector.Destroy;
begin
  inherited;
end;
procedure TG2Connector.PaintOn( aCanvas : TCanvas);
var StyleSet : TG2StyleSet;
begin
  if IsFocused then begin
    if Selected then
      StyleSet := FStateStyleList.StateStyle[ csFocusedSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ csFocused]
  end else
    if Selected then
      StyleSet := FStateStyleList.StateStyle[ csSelected]
    else
      StyleSet := FStateStyleList.StateStyle[ FState];
  StyleSet.Fill.Color := ConnectorColor;
  if assigned(FOnPaintElement) then
    FOnPaintElement(self, ceBackGround, 0, StyleSet);
  StyleSet.ApplyStyleSet( aCanvas);
  if FConnectorKind = ckInput then begin
    aCanvas.FillEllipse( UnscaledBoundsRect, AbsoluteOpacity);
    aCanvas.DrawEllipse( UnscaledBoundsRect, AbsoluteOpacity);
  end else begin
    aCanvas.FillRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
    aCanvas.DrawRect(UnscaledBoundsRect, 0, 0, [], AbsoluteOpacity);
  end;
  if not FConnected then begin
    aCanvas.Fill.Color := Darker(ConnectorColor, 128); //claBlue;
    aCanvas.FillEllipse( FHoleRect, AbsoluteOpacity);
    aCanvas.DrawEllipse( FHoleRect, AbsoluteOpacity);
  end else begin
    aCanvas.DrawEllipse( FHoleRect, AbsoluteOpacity);
  end;
  inherited;
end;
function TG2Connector.ParseProperties(fs: TModuleDefStream;
  aName: string): boolean;
var aValue : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FCodeRef := StrToInt(string(aValue));
      FConnectorIndex := FCodeRef;
    end else
    {if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      FInfoFunc := StrToInt(string(aValue));
    end else}
    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Audio"' then
        FConnectorType := ctAudio
      else
        if aValue = '"Logic"' then
          FConnectorType := ctLogic
        else
          if aValue = '"Control"' then
            FConnectorType := ctControl
          else
            raise Exception.Create('Unknown control type ' + string(aValue));
    end else
    if aName = 'Bandwidth' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Static"' then
        FBandwidth := btStatic
      else
        if aValue = '"Dynamic"' then
          FBandWidth := btDynamic
        else
          raise Exception.Create('Unknown bandwidth type ' + string(aValue));
    end else
      Result := False
  end;
end;
procedure TG2Connector.SetConnected(const aValue: boolean);
begin
  if FCOnnected <> aValue then begin
    FConnected := aValue;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
//
//                               TG2CableNode
//
//------------------------------------------------------------------------------
constructor TG2CableNode.Create(aCable : TG2Cable);
begin
  inherited Create(aCable);
  FCable := aCable;
  FFill := TBrush.Create( TBrushKind.bkSolid, claGreen);
  HitTest := False;
  x := 0;
  y := 0;
  x2 := 0;
  y2 := 0;
  vx := 0;
  vy := 0;
  PStart := PointF(0,0);
  PEnd := PointF(0,0);
  FStartNode := False;
  FEndNode := False;
  SetLength(FPolygon, 5);
end;
destructor TG2CableNode.Destroy;
begin
  Finalize(FPolygon);
  FFill.Free;
  inherited;
end;
procedure TG2CableNode.PaintOn( aCanvas : TCanvas);
var Rect : TRectF;
begin
  if assigned(FFill) then
    aCanvas.Fill.Assign(FFill);
  aCanvas.Stroke.Kind := TBrushKind.bkSolid;
  aCanvas.Stroke.Color := claGreen;
  //aCanvas.DrawRect(BoundsRect, 0, 0, [], AbsoluteOpacity);
  if FStartNode then begin
    Rect := RectF(PStart.X - 3, PStart.Y - 3, PStart.X + 3, PStart.Y + 3);
    aCanvas.FillEllipse( Rect, AbsoluteOpacity);
  end;
  if FEndNode then begin
    Rect := RectF(PEnd.X - 3, PEnd.Y - 3, PEnd.X + 3, PEnd.Y + 3);
    aCanvas.FillEllipse( Rect, AbsoluteOpacity);
  end;
  aCanvas.FillPolygon( FPolygon, AbsoluteOpacity);
  inherited;
end;
procedure TG2CableNode.SetVisible(const aValue: boolean);
var d : integer;
begin
  inherited;
  d := 1;
end;
procedure TG2CableNode.CalcPath;
var BB : TRectF;
    P1, P2, P3, P4, V, N, G1, G2 : TPointF;
    L, d, Margin : single;
begin
  FRedraw := True;
  d := 1;
  if FEndNode or FStartNode then begin
    Margin := 4;
  end else
    Margin := 0;
  // CalcNormal
  V.X := x2 - x;
  V.Y := y2 - y;
  N.X := V.Y;
  N.Y := -V.X;
  L := sqrt( N.X * N.X + N.Y * N.Y);
  if L <> 0 then begin
    N.X := N.X / L;
    N.Y := N.Y / L;
  end;
  // Calc bounding box
  P1.X := x + N.X*d;
  P1.Y := y + N.Y*d;
  P2.X := x2 + N.X*d;
  P2.Y := y2 + N.Y*d;
  P3.X := x2 - N.X*d;
  P3.Y := y2 - N.Y*d;
  P4.X := x - N.X*d;
  P4.Y := y - N.Y*d;
  BB.Left := System.Math.Min(System.Math.Min(System.Math.Min( p1.X, p2.X), p3.X), p4.X) - Margin;
  BB.Top := System.Math.Min(System.Math.Min(System.Math.Min( p1.Y, p2.Y), p3.Y), p4.Y) - Margin;
  BB.Right := System.Math.Max(System.Math.Max(System.Math.Max( p1.X, p2.X), p3.X), p4.X) + Margin;
  BB.Bottom := System.Math.Max(System.Math.Max(System.Math.Max( p1.Y, p2.Y), p3.Y), p4.Y) + Margin;
  //SetBoundsRect(BB);
  //UnscaledRect := BB;
  UnscaledRect := RectF(BB.Left - FCable.Position.X,
                        BB.Top - FCable.Position.Y,
                        BB.Right - FCable.Position.X,
                        BB.Bottom - FCable.Position.Y);
  if FCable.CableStyle = csGradient then begin
    G1.X := P1.X + (P2.X - P1.X)/2;
    G1.Y := P1.Y + (P2.Y - P1.Y)/2;
    G2.X := P4.X + (P3.X - P4.X)/2;
    G2.Y := P4.Y + (P3.Y - P4.Y)/2;
    if ((BB.Right - BB.Left) <> 0) and ((BB.Bottom - BB.Top) <> 0) then begin
      G1.X := (G1.X - BB.Left) / (BB.Right - BB.Left);
      G1.Y := (G1.Y - BB.Top) / (BB.Bottom - BB.Top);
      G2.X := (G2.X - BB.Left) / (BB.Right - BB.Left);
      G2.Y := (G2.Y - BB.Top) / (BB.Bottom - BB.Top);
    end;
    FFill.Kind := TBrushKind.bkGradient;
    FFill.Gradient.StartPosition.Point := PointF( G1.X, G1.Y);
    FFill.Gradient.StopPosition.Point  := PointF( G2.X, G2.Y);
    FFill.Gradient.Color := FCable.Color;
    FFill.Gradient.Color1 := Darker(FCable.Color, 128);
  end else begin
    FFill.Kind := TBrushKind.bkSolid;
    FFill.Color := FCable.Color;
  end;
  PStart.X := x - BB.Left;
  PStart.Y := y - BB.Top;
  PEnd.X := x2 - BB.Left;
  PEnd.Y := y2 - BB.Top;
  P1.X := P1.X - BB.Left;
  P1.Y := P1.Y - BB.Top;
  P2.X := P2.X - BB.Left;
  P2.Y := P2.Y - BB.Top;
  P3.X := P3.X - BB.Left;
  P3.Y := P3.Y - BB.Top;
  P4.X := P4.X - BB.Left;
  P4.Y := P4.Y - BB.Top;
  FPolygon[0] := P1;
  FPolygon[1] := P2;
  FPolygon[2] := P3;
  FPolygon[3] := P4;
  FPolygon[4] := P1;
end;

//------------------------------------------------------------------------------
//
//                               TG2Cable
//
//------------------------------------------------------------------------------
constructor TG2Cable.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := False;
  FBuffered := False;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FMargin := 10;
  FP1.X := Position.X + FMargin;
  FP1.Y := Position.Y + FMargin;
  FP2.X := Position.X + Width - FMargin * 2;
  FP2.Y := Position.Y + FMargin;
  FSegmentSize := 20;
  //AddNodes(2);
  InitCable;
  FColor := claRed;
  FCableStyle := csFlat;
  FVisible := True;
  FTimer.OnTimer := OnTimer;
end;
destructor TG2Cable.Destroy;
begin
  FTimer.Enabled := False;
  //FTimer.Free;
  //ClearNodes;
  Finalize(FNodes);
  inherited;
end;
{function TG2Cable.GetParent: TFmxObject;
begin
  Result := FParent;
end;}
procedure TG2Cable.SetBuffered(const Value: boolean);
var i : integer;
begin
  if FBuffered <> Value then begin
    //FBuffered := Value;
    for i := 0 to FNodeCount - 1 do begin
      FNodes[i].Buffered := Value;
    end;
  end;
end;
procedure TG2Cable.SetCableStyle(const Value: TCableStyle);
begin
  if FCableStyle <> Value then begin
    FCableStyle := Value;
    CalcNodePaths;
  end;
end;
procedure TG2Cable.SetColor(const Value: TAlphaColor);
begin
  if FColor <> VAlue then begin
    FColor := Value;
    CalcNodePaths;
  end;
end;
{procedure TG2Cable.SetParent(const Value: TFmxObject);
var i : integer;
begin
  FParent := Value;
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Parent := Value;
end;}
procedure TG2Cable.AddNodes(aNodeCount: integer);
var i : integer;
begin
  ClearNodes;
  FNodeCount := aNodeCount;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do begin
    FNodes[i] := TG2CableNode.Create(self);
    //FNodes[i].Parent := FParent;
    FNodes[i].Parent := Self;
    FNodes[i].SetSubComponent(True);
    FNodes[i].Stored := False; // Otherwise there's a "argument out of range" error when droppen on a frame
    FNodes[i].Buffered := FBuffered;
    FNodes[i].Zoom := Zoom;
    FNodes[i].FStartNode := i = 0;
    FNodes[i].FEndNode := i = (FNodeCount-1);
  end;
end;
procedure TG2Cable.ClearNodes;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].DisposeOf;
  FNodeCount := 0;
  SetLength(FNodes, 0);
end;
procedure TG2Cable.CalcNodePaths;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].CalcPath;
end;
procedure TG2Cable.CalcCaterny;
var n : integer;
    l, dx, dy : single;
    min_x, max_x, min_y, max_y : single;
    halfx, maxsag : single;
    function Caterny( x : single): single;
    var g, H : single;
    begin
    { z = -H/g (cosh (gx/H)-1)
      where
      z = vertical distance
      H = Horizontal component of cable force
      g = weight of cable / unit of lenght
      x = horizontal distance
      and
      dz/dx = -sinh(gx/H)
      V = - H sinh(gx/H)
      where
      V = vertical component of cable force
      N = H - gz
      where
      N = tension force in the cable
      Read more: http://wiki.answers.com/Q/What_is_the_formula_for_catenary_calculation#ixzz1iD2bBQpF}
      g := 1;
      H := 1000;
      Result := -H/g *(cosh (g*x/H)-1);
    end;
begin
  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);
  l := sqrt((max_x - min_x)*(max_x - min_x) + (max_y - min_y)*(max_y - min_y));
  n := FNodeCount - 1;
  dx := ( FP2.X - FP1.X) / FNodeCount;
  dy := ( FP2.Y - FP1.Y) / FNodeCount;
  halfx := ( FP2.X - FP1.X) / 2;
  maxsag := Caterny(-halfx);
  FNodes[n].x2 := FP2.X;
  FNodes[n].y2 := FP2.Y;
  while (n >= 0) do begin
    FNodes[n].x := FP1.X + dx * n;
    FNodes[n].y := FP1.Y + dy * n + Caterny(dx * n - halfx) - maxsag ;
    FNodes[n].vx := 0;
    FNodes[n].vy := 0;
    if FNodes[n].x < min_x then
      min_x := FNodes[n].x;
    if FNodes[n].x > max_x then
      max_x := FNodes[n].x;
    if FNodes[n].y < min_y then
      min_y := FNodes[n].y;
    if FNodes[n].y > max_y then
      max_y := FNodes[n].y;
    if n > 0 then begin
      FNodes[n-1].x2 := FNodes[n].x;
      FNodes[n-1].y2 := FNodes[n].y;
    end;
    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.y;
  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
  CalcNodePaths;
end;
procedure TG2Cable.InitCable;
var l, min_x, min_y, max_x, max_y : single;
begin
  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);
  l := sqrt((max_x - min_x)*(max_x - min_x) + (max_y - min_y)*(max_y - min_y));
  if FSegmentSize <> 0 then
    AddNodes(trunc(l / FSegmentSize)+2)
  else
    AddNodes(2);
  CalcCaterny;
end;
procedure TG2Cable.IterateCable;
var n : integer;
    min_x, min_y, max_x, max_y : single;
begin
  min_x := System.Math.Min(FP1.X, FP2.X);
  max_x := System.Math.Max(FP1.X, FP2.X);
  min_y := System.Math.Min(FP1.Y, FP2.Y);
  max_y := System.Math.Max(FP1.Y, FP2.Y);
  n := FNodeCount - 1;
  FNodes[n].x2 := FP2.X;
  FNodes[n].y2 := FP2.Y;
  while (n >= 0) do begin
    if n > 0 then begin
    	FNodes[n].vx := FNodes[n].vx + ( FNodes[n].x2 + FNodes[n - 1].x - FNodes[n].x * 2 ) / TENSION;
	  	FNodes[n].vy := FNodes[n].vy + ( FNodes[n].y2 + FNodes[n - 1].y - FNodes[n].y * 2 ) / TENSION;
    end else begin
    	FNodes[n].vx := FNodes[n].vx + ( FNodes[n].x2 + FNodes[n].x - FNodes[n].x * 2 ) / TENSION;
	  	FNodes[n].vy := FNodes[n].vy + ( FNodes[n].y2 + FNodes[n].y - FNodes[n].y * 2 ) / TENSION;
    end;
    FNodes[n].vy := FNodes[n].vy + GRAVITY;
    //-- Reibung
    FNodes[n].vx := FNodes[n].vx * DAMP;
    FNodes[n].vy := FNodes[n].vy * DAMP;
    //-- Addieren der neuen Vektoren
    FNodes[n].x := FNodes[n].x + FNodes[n].vx;
    FNodes[n].y := FNodes[n].y + FNodes[n].vy;
    if FNodes[n].x < min_x then
      min_x := FNodes[n].x;
    if FNodes[n].x > max_x then
      max_x := FNodes[n].x;
    if FNodes[n].y < min_y then
      min_y := FNodes[n].y;
    if FNodes[n].y > max_y then
      max_y := FNodes[n].y;
    if n > 0 then begin
      FNodes[n-1].x2 := FNodes[n].x;
      FNodes[n-1].y2 := FNodes[n].y;
    end;
    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.Y;
  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
  CalcNodePaths;
end;
procedure TG2Cable.OnTimer(Sender: TObject);
begin
//  if FTimerCount <= 0 then
//    FTimer.Enabled := False;
//  Dec(FTimerCount);
  IterateCable;
  //Repaint;
end;
procedure TG2Cable.StartTimer;
begin
  IterateCable;
  FTimerCount := 5;
  FTimer.Enabled := True;
end;
procedure TG2Cable.SetPoint1X(const Value: single);
begin
  if FP1.X <> Value then begin
    FP1.X := Value;
    InitCable;
  end;
end;
procedure TG2Cable.SetPoint1Y(const Value: single);
begin
  if FP1.Y <> Value then begin
    FP1.Y := Value;
    InitCable;
  end;
end;
procedure TG2Cable.SetPoint2X(const Value: single);
begin
  if FP2.X <> Value then begin
    FP2.X := Value;
    InitCable;
  end;
end;
procedure TG2Cable.SetPoint2Y(const Value: single);
begin
  if FP2.Y <> Value then begin
    FP2.Y := Value;
    InitCable
  end;
end;
procedure TG2Cable.SetVisible(const aValue: boolean);
var i : integer;
begin
  if FVisible <> aValue then begin
    FVisible := aValue;
    for i := 0 to Length(FNodes) - 1 do
      FNodes[i].Visible := aValue;
  end;
end;
procedure TG2Cable.SetZoom(const Value: single);
var i : integer;
begin
  inherited;
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Zoom := Zoom;
end;
//------------------------------------------------------------------------------
//
//                               TG2Module
//
//------------------------------------------------------------------------------
constructor TG2Module.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TFont.Create;
  FFont.Family := 'Roboto';
  FFont.Size := 10;
  FFill := TBrush.Create(TBrushKind.bkSolid, claWhite);
  FStroke := TBrush.Create( TBrushKind.bkSolid, claBlack);
  FDefaultStateStyles.StateStyle[csSelected].Stroke.Color := $FF505050;
  FDefaultStateStyles.StateStyle[csSelected].Stroke.Thickness := 2.5;
  FDefaultStateStyles.StateStyle[csDefault].Stroke.Color := $FF909090;
  FDefaultStateStyles.StateStyle[csDefault].Stroke.Thickness := 2;
  FSelected := False;
  UnscaledRect := RectF(0, 0, UNITS_COL+UNIT_MARGIN*2, UNITS_ROW+UNIT_MARGIN*2);
  FRow := 0;
  FCol := 0;
end;
destructor TG2Module.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FFont.Free;
  inherited;
end;
function TG2Module.GetSelected: boolean;
begin
  //TODO Result := ModuleData.Selected;
  Result := FSelected;
end;
procedure TG2Module.PaintOn( aCanvas : TCanvas);
var StyleSet : TG2StyleSet;
    Rect : TRectF;
begin
  if Selected  then
    StyleSet := FStateStyleList.StateStyle[ csSelected]
  else
    StyleSet := FStateStyleList.StateStyle[ csDefault];
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
               FModuleLabel,
               TTextAlign.taLeading, TTextAlign.taLeading);
  inherited;
end;
procedure TG2Module.SetCol(const aValue: integer);
begin
  if FCol <> aValue then begin
    FCol := aValue;
    UnscaledLeft := aValue * (UNITS_COL+UNIT_MARGIN*2);
  end;
end;
procedure TG2Module.SetHeightUnits(const Value: integer);
begin
  if FHeightUnits <> Value then begin
    FHeightUnits := Value;
    UnscaledHeight := (UNITS_ROW+UNIT_MARGIN*2) * FHeightUnits;
  end;
end;
procedure TG2Module.SetModuleLabel(const aValue: string);
begin
  if FModuleLabel <> aValue then begin;
    FModuleLabel := aValue;
    Redraw;
  end;
end;
procedure TG2Module.SetRow(const aValue: integer);
begin
  if FRow <> aValue then begin
    FRow := aValue;
    UnscaledTop := aValue * (UNITS_ROW+UNIT_MARGIN*2);
  end;
end;
procedure TG2Module.SetSelected(const aValue: boolean);
begin
  if aValue <> FSelected then begin
    FSelected := aValue;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
//
//                                 TG2KeyBoard
//
//------------------------------------------------------------------------------
constructor TG2KeyBoard.Create(AOwner: TComponent);
begin
  inherited;
  CanFocus := True;
  Hittest := True;
  FMouseEnable := True;
  FDownColor := TAlphaColorRec.Aqua;
  FOrientation := kbHorizontal;
  FLowKey := 0;
  FHighKey := 60;
  FSelectedLowKey := 0;
  FSelectedHighKey := 0;
  FRangeSelect := False;
  FOctaveButtons := False;
  FBuffered := False;
  FMouseEnable := True;
  FMonoKey := False;
  FMonoKeyTouchID := -1;
  FKeyType1 := TPathData.Create;
  FKeyType2 := TPathData.Create;
  FKeyType3 := TPathData.Create;
  InitKeyTouchIDS;
  InitOctaveButtonValues;
  CalcKeyDimensions;
end;
destructor TG2KeyBoard.Destroy;
begin
  FKeyType1.Free;
  FKeyType2.Free;
  FKeyType3.Free;
  inherited;
end;
procedure TG2KeyBoard.DoMouseLeave;
begin
  if FMouseEnable then
    ProcessMouseLeave( 1);
  inherited;
end;
procedure TG2KeyBoard.CalcKeyDimensions;
var lo, ho, lk, hk, range : integer;
    bl, wl, hbw, ww : single;
    t : TKeyType;
begin
  GetKeyType(FLowKey, lo, lk, t);
  GetKeyType(FHighKey, ho, hk, t);
  range := (7*ho + hk + 1) - (7*lo + lk);
  if FOrientation = kbHorizontal then begin
    if FOctaveButtons then
      FOctaveButtonHeight := UnscaledHeight * 0.15
    else
      FOctaveButtonHeight := 0;
    FWhiteKeyWidth := UnscaledWidth/range;
    FWhiteKeyLength := UnscaledHeight - FOctaveButtonHeight;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
    if FOctaveButtons then
      FOctaveButtonWidth := FWhiteKeyWidth * 7
    else
      FOctaveButtonHeight := 0;
  end else begin
    if FOctaveButtons then
      FOctaveButtonHeight := UnscaledWidth * 0.15
    else
      FOctaveButtonHeight := 0;
    FWhiteKeyWidth := UnscaledHeight/range;
    FWhiteKeyLength := UnscaledWidth - FOctaveButtonHeight;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
    if FOctaveButtons then
      FOctaveButtonWidth := FWhiteKeyWidth * 7
    else
      FOctaveButtonWidth := 0;
  end;
  FBlackKeyWidth := round(FWhiteKeyWidth * 0.7);
  bl := FBlackKeyLength;
  wl := FWhiteKeyLength;
  hbw :=  FBlackKeyWidth / 2;
  ww := FWhiteKeyWidth;
  if FOrientation = kbHorizontal then begin
    FKeyType1.Clear;
    FKeyType1.MoveTo( PointF( 0, 0));
    FKeyType1.LineToRel( PointF( 0, wl));
    FKeyType1.LineToRel( PointF( ww, 0));
    FKeyType1.LineToRel( PointF( 0, bl-wl));
    FKeyType1.LineToRel( PointF( -hbw, 0));
    FKeyType1.LineToRel( PointF( 0, -bl));
    FKeyType1.LineToRel( PointF( -ww + hbw, 0));
    FKeyType1.ClosePath;
    FKeyType2.Clear;
    FKeyType2.MoveTo( PointF( hbw, 0));
    FKeyType2.LineToRel( PointF( 0, bl));
    FKeyType2.LineToRel( PointF( -hbw, 0));
    FKeyType2.LineToRel( PointF( 0, wl-bl));
    FKeyType2.LineToRel( PointF( ww, 0));
    FKeyType2.LineToRel( PointF( 0, -wl+bl));
    FKeyType2.LineToRel( PointF( -hbw, 0));
    FKeyType2.LineToRel( PointF( 0, -bl));
    FKeyType2.LineToRel( PointF( -ww + hbw + hbw, 0));
    FKeyType2.ClosePath;
    FKeyType3.Clear;
    FKeyType3.MoveTo( PointF( hbw, 0));
    FKeyType3.LineToRel( PointF( 0, bl));
    FKeyType3.LineToRel( PointF( -hbw, 0));
    FKeyType3.LineToRel( PointF( 0, wl-bl));
    FKeyType3.LineToRel( PointF( ww, 0));
    FKeyType3.LineToRel( PointF( 0, -wl));
    FKeyType3.LineToRel( PointF( -ww + hbw, 0));
    FKeyType3.ClosePath;
  end else begin
    FKeyType1.Clear;
    FKeyType1.MoveTo( PointF( 0, ww));
    FKeyType1.LineToRel( PointF( wl, 0));
    FKeyType1.LineToRel( PointF( 0, -ww));
    FKeyType1.LineToRel( PointF( bl-wl, 0));
    FKeyType1.LineToRel( PointF( 0, hbw));
    FKeyType1.LineToRel( PointF( -bl, 0));
    FKeyType1.LineToRel( PointF( 0, ww - hbw));
    FKeyType1.ClosePath;
    FKeyType2.Clear;
    FKeyType2.MoveTo( PointF( 0, hbw));
    FKeyType2.LineToRel( PointF( bl, 0));
    FKeyType2.LineToRel( PointF( 0 , -hbw));
    FKeyType2.LineToRel( PointF( wl-bl, 0));
    FKeyType2.LineToRel( PointF( 0, ww));
    FKeyType2.LineToRel( PointF( -wl+bl, 0));
    FKeyType2.LineToRel( PointF( 0, -hbw));
    FKeyType2.LineToRel( PointF( -bl, 0));
    FKeyType2.LineToRel( PointF( 0, -ww + hbw + hbw));
    FKeyType2.ClosePath;
    FKeyType3.Clear;
    FKeyType3.MoveTo( PointF( 0, ww-hbw));
    FKeyType3.LineToRel( PointF( bl, 0));
    FKeyType3.LineToRel( PointF( 0, hbw));
    FKeyType3.LineToRel( PointF( wl-bl, 0));
    FKeyType3.LineToRel( PointF( 0, -ww));
    FKeyType3.LineToRel( PointF( -wl, 0));
    FKeyType3.LineToRel( PointF( 0, ww-hbw));
    FKeyType3.ClosePath;
  end;
end;
function TG2KeyBoard.GetKeyRect(const aKey: integer): TRectF;
var o, k, lo, lk : integer;
    t, lt : TKeyType;
    d : single;
begin
  GetKeyType(FLowKey, lo, lk, lt);
  GetKeyType(aKey, o, k, t);
  case t of
    ktWhite :
      begin
        d := (k - lk) * FWhiteKeyWidth + (o - lo) * (FWhiteKeyWidth * 7);
        if FOrientation = kbHorizontal then begin
          Result.Left := d;
          Result.Top := FOctaveButtonHeight;
          Result.Height := FWhiteKeyLength;
          Result.Width := FWhiteKeyWidth;
        end else begin
          Result.Top := Height - d;
          Result.Left := FOctaveButtonHeight;
          Result.Height := FWhiteKeyWidth;
          Result.Width := FWhiteKeyLength;
        end;
      end;
    ktBlack :
      begin
        d := FWhiteKeyWidth - FBlackKeyWidth/2
           + (k - lk) * FWhiteKeyWidth + (o - lo) * (FWhiteKeyWidth * 7);
        if FOrientation = kbHorizontal then begin
          Result.Top := FOctaveButtonHeight;
          Result.Left := d;
          Result.Height := FBlackKeyLength;
          Result.Width := FBlackKeyWidth;
        end else begin
          Result.Top := Height - d;
          Result.Left := FOctaveButtonHeight;
          Result.Height := FBlackKeyWidth;
          Result.Width := FBlackKeyLength;
        end;
      end;
  end;
end;
procedure TG2KeyBoard.GetKeyType(const aKey : integer; var aOctave, aKeyID : integer; var aKeyType : TKeyType);
var OctaveKey : integer;
begin
  aOctave := aKey div 12;
  OctaveKey := aKey mod 12;
  aKeyID := KeyTypes[OctaveKey].KeyID;
  aKeyType := KeyTypes[OctaveKey].KeyType;
end;
function TG2KeyBoard.GetOctaveRect(const aOctave: integer): TRectF;
var o, k, lo, lk : integer;
    t, lt : TKeyType;
    d, OctaveWidth : single;
begin
  GetKeyType(FLowKey, lo, lk, lt);
  OctaveWidth := (FWhiteKeyWidth * 7);
  d := lk * FWhiteKeyWidth;
  if FOrientation = kbHorizontal then begin
    Result.Left := (aOctave - lo) * OctaveWidth - d;
    Result.Top := 0;
    Result.Width := OctaveWidth;
    Result.Height := FOctaveButtonHeight;
  end else begin
    Result.Left := 0;
    Result.Top := Height - ((aOctave - lo) * OctaveWidth - d);
    Result.Width := FOctaveButtonHeight;
    Result.Height := OctaveWidth;
  end;
  Result := TRectF.Intersect( Result, BoundsRect);
end;
procedure TG2KeyBoard.InitKeyTouchIDS;
var i : integer;
begin
  for i := 0 to High(FKeyTouchID) do begin
    FKeyTouchID[i] := -1;
    FKeyUpdate[i] := 1;
  end;
end;
procedure TG2KeyBoard.InitOctaveButtonValues;
var i : integer;
begin
  for i := 0 to High(FOctaveButtonValue) do begin
    FOctaveButtonValue[i] := 0;
    FOctaveUpdate[i] := 1;
  end;
end;
procedure TG2KeyBoard.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseDown) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else begin
    if FMouseEnable then
      ProcessMouseDown(Self, Shift, 1, X, Y);
  end;
end;
procedure TG2KeyBoard.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if assigned(OnMouseMove) then
    OnMouseMove( Self, Shift, X, Y)
  else begin
    if FMouseEnable then
      ProcessMouseMove(Self, Shift, 1, X, Y);
  end;
end;
procedure TG2KeyBoard.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if assigned(OnMouseUp) then
    OnMouseUp( Self, Button, Shift, X, Y)
  else
    if FMouseEnable then
      ProcessMouseUp(Self, Shift, 1, X, Y);
end;
procedure TG2KeyBoard.PaintKey(aCanvas: TCanvas; const aKey : integer);
var R : TRectF;
    StyleSet : TG2StyleSet;
    o, k : integer;
    t : TKeyType;
    SaveMatrix : TMatrix;
begin
  if FBuffered then begin
    if FKeyUpdate[aKey] = 0 then
      exit
    else
      FKeyUpdate[aKey] := 0;
  end;
  R := GetKeyRect(aKey);
  if not R.IntersectsWith(BoundsRect) then
    exit;

  if FRangeSelect and (aKey >= FSelectedLowKey) and (aKey <= FSelectedHighKey)  then
    StyleSet := FStateStyleList.StateStyle[ csSelected]
  else
    StyleSet := FStateStyleList.StateStyle[ FState];
  StyleSet.ApplyStyleSet( aCanvas);
  GetKeyType(aKey, o, k, t);
  case t of
    ktWhite :
      begin
        if FKeyTouchID[aKey] <> -1 then
          aCanvas.Fill.Color := DownColor
        else
          aCanvas.Fill.Color := StyleSet.Fill.Color;
        aCanvas.Stroke.Color := StyleSet.Stroke.Color;
        SaveMatrix := aCanvas.Matrix;
        try
          {$IF Defined(VER340)}
          aCanvas.SetMatrix( SaveMatrix * TMatrix.CreateTranslation(R.Left, R.Top));
          {$ELSE}
          aCanvas.SetMatrix( MatrixMultiply(SaveMatrix, CreateTranslateMatrix(R.Left, R.Top)));
          {$ENDIF}
          case k of
            0,3 : begin
                    aCanvas.FillPath( FKeyType1, AbsoluteOpacity);
                    aCanvas.DrawPath( FKeyType1, AbsoluteOpacity);
                  end;
            1,4,5 : begin
                    aCanvas.FillPath( FKeyType2, AbsoluteOpacity);
                    aCanvas.DrawPath( FKeyType2, AbsoluteOpacity);
                  end;
            2,6 : begin
                    aCanvas.FillPath( FKeyType3, AbsoluteOpacity);
                    aCanvas.DrawPath( FKeyType3, AbsoluteOpacity);
                  end;
          end;
        finally
          aCanvas.SetMatrix( SaveMatrix);
        end;
      end;
    ktBlack :
      begin

        if FKeyTouchID[aKey] <> -1 then
          aCanvas.Fill.Color := DownColor
        else
          aCanvas.Fill.Color := StyleSet.Stroke.Color;
        aCanvas.Stroke.Color := StyleSet.Stroke.Color;
       aCanvas.FillRect(R, 0, 0, [], AbsoluteOpacity);
       aCanvas.DrawRect(R, 0, 0, [], AbsoluteOpacity);
      end;
  end;
end;
procedure TG2KeyBoard.PaintOctaveButton(aCanvas: TCanvas;const aOctave : integer);
var R : TRectF;
    StyleSet : TG2StyleSet;
begin
  if FBuffered then begin
    if FOctaveUpdate[aOctave] = 0 then
      exit
    else
      FOctaveUpdate[aOctave] := 0;
  end;
  R := GetOctaveRect(aOctave);
  if (FOctaveButtonValue[aOctave] = 1)  then
    StyleSet := FStateStyleList.StateStyle[ csSelected]
  else
    StyleSet := FStateStyleList.StateStyle[ FState];
  StyleSet.ApplyStyleSet( aCanvas);
  aCanvas.Fill.Color := StyleSet.Fill.Color;
  aCanvas.Stroke.Color := StyleSet.Stroke.Color;
  aCanvas.FillRect(R, 0, 0, [], AbsoluteOpacity);
  aCanvas.DrawRect(R, 0, 0, [], AbsoluteOpacity);
  DrawTextExt( aCanvas,
               StyleSet.Font,
               PointF(R.Left, R.Top),
               PointF(R.Width, R.Height),
               StyleSet.FontColor,
               IntToStr(aOctave),
               TTextAlign.taCenter, TTextAlign.taCenter);
end;
procedure TG2KeyBoard.PaintOn(aCanvas: TCanvas);
var i : integer;
    lo, ho : integer;
begin
  inherited;
  for i := FLowKey to FHighKey do begin
    if (KeyTypes[i mod 12].KeyType = ktWhite) then begin
      PaintKey(aCanvas, i);
    end;
  end;
  for i := FLowKey to FHighKey do begin
    if (KeyTypes[i mod 12].KeyType = ktBlack) then begin
      PaintKey(aCanvas, i);
    end;
  end;
  if FOctaveButtons then begin
    lo := FLowKey div 12;
    ho := FHighKey div 12;
    for i := lo to ho do
      PaintOctaveButton(aCanvas, i);
  end;
end;
procedure TG2KeyBoard.ProcessMouseLeave(aTouchID: integer);
var i : integer;
begin
  i := 0;
  for i := FLowKey to FHighKey do begin
    if FKeyTouchID[i] <> -1 then begin
      FKeyTouchID[i] := -1;
      FKeyUpdate[i] := 1;
      Redraw;
      if assigned(FOnKeyboardKeyUp) then
        FOnKeyboardKeyUp( self, i);
    end;
  end;
  FMonoKeyTouchID := -1;
end;
procedure TG2KeyBoard.ProcessMouseDown(Sender: TObject; Shift: TShiftState;
  aTouchID : integer; X, Y: Single);
var i, key, lo, ho : integer;
begin
  if FState = csDisabled then
    exit;
  if FMonoKey and (FMonoKeyTouchID <> -1) then
    exit;
  i := FLowKey;
  while (i <= FHighKey) and not((KeyTypes[i mod 12].KeyType = ktBlack) and PtInRect( GetKeyRect(i), PointF(X, Y))) do
    inc(i);
  if not(i <= FHighKey) then begin
    i := FLowKey;
    while (i <= FHighKey) and not((KeyTypes[i mod 12].KeyType = ktWhite) and PtInRect( GetKeyRect(i), PointF(X, Y))) do
      inc(i);
  end;
  if (i <= FHighKey) then begin
    key := i;
    if FRangeSelect then begin
      SelectedLowKey := key;
      SelectedHighKey := key;
    end else begin
      if FMonoKey then begin
        for i := FLowKey to FHighKey do begin
          if (i<>key) and (FKeyTouchID[i] <> -1) then begin
            FKeyTouchID[i] := -1;
            FKeyUpdate[i] := 1;
            Redraw;
            if assigned(FOnKeyboardKeyUp) then
              FOnKeyboardKeyUp( self, i);
          end;
        end;
      end;
      if FKeyTouchID[key] = -1 then begin
        FKeyTouchID[key] := aTouchID;
        FMonoKeyTouchID := aTouchID;
        FKeyUpdate[key] := 1;
        Redraw;
        if assigned(FOnKeyboardKeyDown) then
          FOnKeyboardKeyDown( self, key);
      end;
    end;
  end else begin
    if FOctaveButtons then begin
      lo := FLowKey div 12;
      ho := FHighKey div 12;
      i := lo;
      while (i<=ho) and not(PtInRect( GetOctaveRect(i), PointF(X, Y))) do
        inc(i);
      if (i<=ho) then begin
        FOctaveButtonValue[i] := 1 - FOctaveButtonValue[i];
        FOctaveUpdate[i] := 1;
        Redraw;
        if assigned(FOnOctaveButtonChange) then
          FOnOctaveButtonChange(self, i, FOctaveButtonValue[i]);
      end;
    end;
  end;
end;
procedure TG2KeyBoard.ProcessMouseMove(Sender: TObject; Shift: TShiftState;
  aTouchID : integer; X, Y: Single);
var i, key : integer;
begin
  if FState = csDisabled then
    exit;
  if FMonoKey and (FMonoKeyTouchID <> -1) then
    exit;
  if ssLeft in Shift then begin
    i := FLowKey;
    while (i <= FHighKey) and not((KeyTypes[i mod 12].KeyType = ktBlack) and PtInRect( GetKeyRect(i), PointF(X, Y))) do
      inc(i);
    if not(i <= FHighKey) then begin
      i := FLowKey;
      while (i <= FHighKey) and not((KeyTypes[i mod 12].KeyType = ktWhite) and PtInRect( GetKeyRect(i), PointF(X, Y))) do
        inc(i);
    end;
    if FRangeSelect then begin
      if (i <= FHighKey) then begin
        SelectedLowKey := System.Math.Min( i, SelectedLowKey);
        SelectedHighKey := i;
      end;
    end else begin
      key := i;
      for i := FLowKey to FHighKey do begin
        if (i <> key) and ((FKeyTouchID[i] = aTouchID)
                        or (FMonoKey and (FKeyTouchID[i] <> -1))) then begin
          FKeyTouchID[i] := -1;
          FKeyUpdate[i] := 1;
          Redraw;
          if assigned(FOnKeyboardKeyUp) then
            FOnKeyboardKeyUp( self, i);
        end;
      end;
       if (key <= FHighKey) then begin
         if FKeyTouchID[key] = -1 then begin
           FKeyTouchID[key] := aTouchID;
           FMonoKeyTouchID := aTouchID;
           FKeyUpdate[i] := 1;
           Redraw;
           if assigned(FOnKeyboardKeyDown) then
             FOnKeyboardKeyDown( self, key);
         end;
      end;
    end;
  end;
end;
procedure TG2KeyBoard.ProcessMouseUp(Sender: TObject; Shift: TShiftState;
  aTouchID : integer; X, Y: Single);
var i : integer;
begin
  if FState = csDisabled then
    exit;
  if FMonoKey then begin
    if aTouchID = FMonoKeyTouchID then begin
      i := FLowKey;
      while (i <= FHighKey) and not(FKeyTouchID[i] = aTouchID) do
        inc(i);
      if (i <= FHighKey) then begin
        FKeyTouchID[i] := -1;
        FKeyUpdate[i] := 1;
        FMonoKeyTouchID := -1;
        Redraw;
        if assigned(FOnKeyboardKeyUp) then
          FOnKeyboardKeyUp( self, i);
      end;
    end;
  end else begin
    i := FLowKey;
    while (i <= FHighKey) and not(FKeyTouchID[i] = aTouchID) do
      inc(i);
    if (i <= FHighKey) then begin
      FKeyTouchID[i] := -1;
      FKeyUpdate[i] := 1;
      Redraw;
      if assigned(FOnKeyboardKeyUp) then
        FOnKeyboardKeyUp( self, i);
    end;
  end;
end;
procedure TG2KeyBoard.Resize;
begin
  inherited;
  CalcKeyDimensions;
end;
procedure TG2KeyBoard.SetHighKey(const Value: integer);
begin
  if (Value < 0) or (Value > MAX_KEYS) then
    raise Exception.Create('High key out of range.');
  if (FHighKey <> Value) then begin
    FHighKey := Value;
    CalcKeyDimensions;
    InitKeyTouchIDS;
    SelectedLowKey := System.Math.Min( SelectedLowKey, FLowKey);
    SelectedHighKey := System.Math.Max( SelectedHighKey, FLowKey);
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetLowKey(const Value: integer);
begin
  if (Value < 0) or (Value > MAX_KEYS) then
    raise Exception.Create('Low key out of range.');
  if FLowKey <> Value then begin
    FLowKey := Value;
    InitKeyTouchIDS;
    CalcKeyDimensions;
    SelectedLowKey := System.Math.Min( SelectedLowKey, FLowKey);
    SelectedHighKey := System.Math.Max( SelectedHighKey, FLowKey);
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetMouseEnable(const Value: boolean);
begin
  if FMouseEnable <> Value then begin
    FMouseEnable := Value;
  end;
end;
procedure TG2KeyBoard.SetOctaveButtons(const Value: boolean);
begin
  if Value <> FOctaveButtons then begin
    FOctaveButtons := Value;
    InitKeyTouchIDS;
    InitOctaveButtonValues;
    CalcKeyDimensions;
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetOrientation(const Value: TKeyboardOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    CalcKeyDimensions;
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetSelectedHighKey(const Value: integer);
begin
  if FSelectedHighKey <> Value then begin
    FSelectedHighKey := Value;
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetSelectedLowKey(const Value: integer);
begin
  if FSelectedLowKey <> Value then begin
    FSelectedLowKey := Value;
    Redraw;
  end;
end;
procedure TG2KeyBoard.SetState(const Value: TControlState);
var i : integer;
begin
  if FState <> Value then begin
    FState := Value;
    Redraw;
  end;
end;
//------------------------------------------------------------------------------
//
//                                TG2StyleSet
//
//------------------------------------------------------------------------------
constructor TG2StyleSet.Create( aControlState : TControlState);
begin
  FState := aControlState;
  FFont := TFont.Create;
  FFont.Family := 'Roboto';
  FFont.Size := 10;
  case aControlState of
    csFocused:
      begin
        //FFill := TBrush.Create(TBrushKind.bkSolid, claLightGray);
        //FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claWhite);
        FFill := TBrush.Create(TBrushKind.bkSolid, claWhite);
        FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claGreen);
        FFontColor := $FF202020;
      end;
    csSelected:
      begin
        FFill := TBrush.Create(TBrushKind.bkSolid, claAqua);
        FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, $FF202020);
        FFontColor := $FF202020;
      end;
    csFocusedSelected:
      begin
        FFill := TBrush.Create(TBrushKind.bkSolid, claAqua);
        FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claGreen);
        FFontColor := $FF202020;
      end;
    csDisabled:
      begin
        FFill := TBrush.Create(TBrushKind.bkSolid, claLightGray);
        FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, claLightGray);
        FFontColor := claWhite;
      end;
    else begin
      FFill := TBrush.Create(TBrushKind.bkSolid, $FFE0E0E0);
      FStroke := TStrokeBrush.Create(TBrushKind.bkSolid, $FF202020);
      FFontColor := $FF202020;
    end;
  end;
  FActiveMorphColor := claRed;
  FInactiveMorphColor := claBlue;
end;
destructor TG2StyleSet.Destroy;
begin
  FFont.Free;
  FStroke.Free;
  FFill.Free;
  inherited;
end;
procedure TG2StyleSet.Assign(aStyleSet: TG2StyleSet);
begin
  FState := aStyleSet.State;
  FFill.Assign(aStyleSet.Fill);
  FFont.Assign(aStyleSet.Font);
  FFontColor := aStyleSet.FontColor;
  FStroke.Assign(aStyleSet.Stroke);
end;
procedure TG2StyleSet.ApplyStyleSet( aCanvas : TCanvas);
begin
  aCanvas.Fill.Assign(FFill);
  aCanvas.Font.Assign(FFont);
  aCanvas.Stroke.Assign(FStroke);
end;
procedure TG2StyleSet.SetActiveMorphColor(const Value: TAlphaColor);
begin
  FActiveMorphColor := Value;
end;
procedure TG2StyleSet.SetFill(const Value: TBrush);
begin
  FFill.Assign(Value);
end;
procedure TG2StyleSet.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;
procedure TG2StyleSet.SetFontColor(const Value: TAlphaColor);
begin
  FFontColor := Value;
end;
procedure TG2StyleSet.SetInactiveMorphColor(const Value: TAlphaColor);
begin
  FInactiveMorphColor := Value;
end;
procedure TG2StyleSet.SetStroke(const Value: TStrokeBrush);
begin
  FStroke.Assign(Value);
end;
procedure TG2StyleSet.WriteToStrings(const aIndent: integer; sl: TStringList);
begin
  sl.Add(IndentSpaces(aIndent) + '<' + ControlStateName(FState) + 'Fill');
  sl.Add(IndentSpaces(aIndent+2) + 'color="' + CSSColorName(FFill.Color) + '"');
  sl.Add(IndentSpaces(aIndent+2) + 'kind="' + BrushKindName(FFill.Kind) + '">');
  sl.Add(IndentSpaces(aIndent) + '</' + ControlStateName(FState) + 'Fill>');
  sl.Add(IndentSpaces(aIndent) + '<' + ControlStateName(FState) + 'Font');
  sl.Add(IndentSpaces(aIndent+2) + 'family="' + FFont.Family + '"');
  sl.Add(IndentSpaces(aIndent+2) + 'size="' + FloatToStr(FFont.Size) + '"');
  sl.Add(IndentSpaces(aIndent+2) + 'style="' + FontStyleName(FFont.Style) + '"');
  sl.Add(IndentSpaces(aIndent+2) + 'color="' + CSSColorName(FFontColor) + '">');
  sl.Add(IndentSpaces(aIndent) + '</' + ControlStateName(FState) + 'Font>');
  sl.Add(IndentSpaces(aIndent) + '<' + ControlStateName(FState) + 'Stroke');
  sl.Add(IndentSpaces(aIndent+2) + 'color="' + CSSColorName(FStroke.Color) + '"');
  sl.Add(IndentSpaces(aIndent+2) + 'thickness="' + FloatToStr(FStroke.Thickness) + '">');
  sl.Add(IndentSpaces(aIndent) + '</' + ControlStateName(FState) + 'Stroke>');
end;
//------------------------------------------------------------------------------
//
//                                TG2StateStyleList
//
//------------------------------------------------------------------------------
constructor TG2StateStyleList.Create(AOwner : TComponent);
begin
  inherited;
  FItems := TObjectlist<TG2StyleSet>.Create(True);
  AddStateStyle( csDefault);
  AddStateStyle( csFocused);
  AddStateStyle( csSelected);
  AddStateStyle( csFocusedSelected);
  AddStateStyle( csDisabled);
end;
destructor TG2StateStyleList.Destroy;
begin
  FItems.Free;
  inherited;
end;
function TG2StateStyleList.AddStateStyle(aControlState: TControlState): TG2StyleSet;
var i : integer;
begin
  i := 0;
  while (i<FItems.Count) and (FItems.Items[i].FState <> aControlState) do
    inc(i);
  if (i>=FItems.Count) then begin
    Result := TG2StyleSet.Create( aControlState);
    FItems.Add(Result);
  end else
    raise Exception.Create('Unknown control state.');
end;
function TG2StateStyleList.GetDefaultFill: TBrush;
begin
  Result := StateStyle[csDefault].FFill;
end;
function TG2StateStyleList.GetDefaultFont: TFont;
begin
  Result := StateStyle[csDefault].FFont;
end;
function TG2StateStyleList.GetDefaultFontColor: TAlphaColor;
begin
  Result := StateStyle[csDefault].FFontColor;
end;
function TG2StateStyleList.GetDefaultStroke: TStrokeBrush;
begin
  Result := StateStyle[csDefault].FStroke;
end;
function TG2StateStyleList.GetDisabledFill: TBrush;
begin
  Result := StateStyle[csDisabled].FFill;
end;
function TG2StateStyleList.GetDisabledFont: TFont;
begin
  Result := StateStyle[csDisabled].FFont;
end;
function TG2StateStyleList.GetDisabledFontColor: TAlphaColor;
begin
  Result := StateStyle[csDisabled].FFontColor;
end;
function TG2StateStyleList.GetDisabledStroke: TStrokeBrush;
begin
  Result := StateStyle[csDisabled].FStroke;
end;
function TG2StateStyleList.GetFocusedFill: TBrush;
begin
  Result := StateStyle[csFocused].FFill;
end;
function TG2StateStyleList.GetFocusedFont: TFont;
begin
  Result := StateStyle[csFocused].FFont;
end;
function TG2StateStyleList.GetFocusedFontColor: TAlphaColor;
begin
  Result := StateStyle[csFocused].FFontColor;
end;
function TG2StateStyleList.GetFocusedSelectedFill: TBrush;
begin
  Result := StateStyle[csFocusedSelected].FFill;
end;
function TG2StateStyleList.GetFocusedSelectedFont: TFont;
begin
  Result := StateStyle[csFocusedSelected].FFont;
end;
function TG2StateStyleList.GetFocusedSelectedFontColor: TAlphaColor;
begin
  Result := StateStyle[csFocusedSelected].FFontColor;
end;
function TG2StateStyleList.GetFocusedSelectedStroke: TStrokeBrush;
begin
  Result := StateStyle[csFocusedSelected].FStroke;
end;
function TG2StateStyleList.GetFocusedStroke: TStrokeBrush;
begin
  Result := StateStyle[csFocused].FStroke;
end;
function TG2StateStyleList.GetMaxStrokeThickness: single;
var StyleSet : TG2StyleSet;
begin
  Result := 0;
  for StyleSet in FItems do begin
    if StyleSet.FStroke.Thickness > Result then
      Result := StyleSet.FStroke.Thickness;
  end;
end;
function TG2StateStyleList.GetSelectedFill: TBrush;
begin
  Result := StateStyle[csSelected].FFill;
end;
function TG2StateStyleList.GetSelectedFont: TFont;
begin
  Result := StateStyle[csSelected].FFont;
end;
function TG2StateStyleList.GetSelectedFontColor: TAlphaColor;
begin
  Result := StateStyle[csSelected].FFontColor;
end;
function TG2StateStyleList.GetSelectedStroke: TStrokeBrush;
begin
  Result := StateStyle[csSelected].FStroke;
end;
function TG2StateStyleList.GetStateStyle(aControlState: TControlState): TG2StyleSet;
var i : integer;
begin
  i := 0;
  while (i<FItems.Count) and (FItems.Items[i].FState <> aControlState) do
    inc(i);
  if (i<FItems.Count) then
    Result := FItems.Items[i]
  else
    raise Exception.Create('Styleset ' +  IntToStr(integer(aControlState)) + ' not found.');
end;
procedure TG2StateStyleList.SetDefaultFill(const Value: TBrush);
begin
  StateStyle[csDefault].FFill.Assign(Value);
end;
procedure TG2StateStyleList.SetDefaultFont(const Value: TFont);
begin
  StateStyle[csDefault].FFont.Assign(Value);
end;
procedure TG2StateStyleList.SetDefaultFontColor(const Value: TAlphaColor);
begin
  StateStyle[csDefault].FFontColor := Value;
end;
procedure TG2StateStyleList.SetDefaultStroke(const Value: TStrokeBrush);
begin
  StateStyle[csDefault].FStroke.Assign(Value);
end;
procedure TG2StateStyleList.SetDisabledFill(const Value: TBrush);
begin
  StateStyle[csDisabled].FFill.Assign(Value);
end;
procedure TG2StateStyleList.SetDisabledFont(const Value: TFont);
begin
  StateStyle[csDisabled].FFont.Assign(Value);
end;
procedure TG2StateStyleList.SetDisabledFontColor(const Value: TAlphaColor);
begin
  StateStyle[csDisabled].FFontColor := Value;
end;
procedure TG2StateStyleList.SetDisabledStroke(const Value: TStrokeBrush);
begin
  StateStyle[csDisabled].FStroke.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedFill(const Value: TBrush);
begin
  StateStyle[csFocused].FFill.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedFont(const Value: TFont);
begin
  StateStyle[csFocused].FFont.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedFontColor(const Value: TAlphaColor);
begin
  StateStyle[csFocused].FFontColor := Value;
end;
procedure TG2StateStyleList.SetFocusedSelectedFill(const Value: TBrush);
begin
  StateStyle[csFocusedSelected].FFill.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedSelectedFont(const Value: TFont);
begin
  StateStyle[csFocusedSelected].FFont.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedSelectedFontColor(
  const Value: TAlphaColor);
begin
  StateStyle[csFocusedSelected].FFontColor := Value;
end;
procedure TG2StateStyleList.SetFocusedSelectedStroke(const Value: TStrokeBrush);
begin
   StateStyle[csFocusedSelected].FStroke.Assign(Value);
end;
procedure TG2StateStyleList.SetFocusedStroke(const Value: TStrokeBrush);
begin
  StateStyle[csFocused].FStroke.Assign(Value);
end;
procedure TG2StateStyleList.SetSelectedFill(const Value: TBrush);
begin
  StateStyle[csSelected].FFill.Assign(Value);
end;
procedure TG2StateStyleList.SetSelectedFont(const Value: TFont);
begin
  StateStyle[csSelected].FFont.Assign(Value);
end;
procedure TG2StateStyleList.SetSelectedFontColor(const Value: TAlphaColor);
begin
  StateStyle[csSelected].FFontColor := Value;
end;
procedure TG2StateStyleList.SetSlectedStroke(const Value: TStrokeBrush);
begin
  StateStyle[csSelected].FStroke.Assign(Value);
end;
procedure TG2StateStyleList.WriteToStrings(const aIndent: integer;
  sl: TStringList);
begin
  sl.Add(IndentSpaces(aIndent) + '<ControlStateStyle');
  sl.Add(IndentSpaces(aIndent+2) + 'name="' + Name + '">');
  StateStyle[csDefault].WriteToStrings(aIndent+2, sl);
  StateStyle[csDisabled].WriteToStrings(aIndent+2, sl);
  StateStyle[csFocused].WriteToStrings(aIndent+2, sl);
  StateStyle[csSelected].WriteToStrings(aIndent+2, sl);
  StateStyle[csFocusedSelected].WriteToStrings(aIndent+2, sl);
  sl.Add(IndentSpaces(aIndent) + '</ControlStateStyle>');
end;
initialization
  RegisterFmxClasses([TG2BufferedLayout,
                      TArrayLayout,
                      TG2Scope,
                      TG2LedGreen,
                      TG2TextField,
                      TG2BtnText,
                      TG2BtnTextEdit,
                      TG2BtnFlat,
                      TG2BtnRadio,
                      TG2BtnRadioEdit,
                      TG2BtnIncDec,
                      TG2ResetButton,
                      TG2PartSelectorList,
                      TG2PartSelector,
                      TG2Knob,
                      TG2Connector,
                      TG2CableNode,
                      TG2Cable,
                      TG2KeyBoard,
                      TG2Module]);

end.
