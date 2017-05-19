unit G2FMXGraph;

{$I includes\delphi_version.inc}

interface

uses
  System.SysUtils, System.Classes, System.UITypes, System.UIConsts, System.Types,
  System.Contnrs, FMX.Types, FMX.Controls, FMX.Forms, FMX.Layouts, FMX.Objects,
  G2_Types, G2_File, G2_usb, graph_util_fmx;

const
  TENSION = 10;
	DAMP    = 0.8;
	GRAVITY = 0.04;

type
  TClickEvent = procedure(Sender: TObject) of object;
  TChangeEvent = procedure(Sender : TObject) of Object;
  TModuleClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule) of Object;
  TParameterClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter) of Object;
  TConnectorClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileConnector) of Object;

  TG2GraphPatch = class;
  TG2FMXPatchArea = class;
  TG2FMXModule = class;
  TG2FMXControl = class;
  TButtonBevel = class;
  TG2FMXConnector = class;
  TG2FMXCable = class;

  TBevelType = (bvNone, bvRaised, bvLowered);

  TG2Graph = class( TG2USB)
  // Contains the control for the VA and FX patching
  private
    FLayoutVA : TG2FMXPatchArea;
    FLayoutFX : TG2FMXPatchArea;
    FClientType  : TClientType;
    procedure SetClientType( aValue: TClientType);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
    function    GetSelectedPatch : TG2GraphPatch; virtual;
    procedure   SetPatchAreaVA( aValue : TG2FMXPatchArea);
    procedure   SetPatchAreaFX( aValue : TG2FMXPatchArea);
    function    G2MessageDlg( tekst : string; code : integer): integer; override;
    procedure   G2ProcessWindowsMessages;
  published
    property    ClientType : TClientType read FClientType write SetClientType;
    property    PatchAreaVA : TG2FMXPatchArea read FLayoutVA write SetPatchAreaVA;
    property    PatchAreaFX : TG2FMXPatchArea read FLayoutFX write SetPatchAreaFX;
  end;

  TG2GraphSlot = class( TG2USBSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; override;
  end;

  TG2GraphPerformance = class( TG2USBPerformance)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; override;
  end;

  TG2GraphPatch = class( TG2USBPatch)
  // This represents the patch
  private
  // Lists for the leds
    FMiniVUList         : TList;
    FLedGroupList       : TList;
    FLed39List          : TList;
    FLed3AList          : TList;
    FVisible            : boolean;
    FSelectedControl    : TG2FMXControl;
    FSelectedMorphIndex : integer;
    procedure   SetVisible( aValue : boolean);
    procedure   SetSelectedControl( aValue : TG2FMXControl);
    procedure   SetSelectedMorphIndex( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Init; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;

    procedure   SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
    procedure   MoveOutlines( aLocation : TLocationType; dX, dY: single);
    function    MessMoveModules( aLocation : TLocationType): boolean;

    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; override;
    procedure   SetMiniVULevel( Index : integer; aValue : byte);
    procedure   SetLedLevel( Index : integer; aValue : byte);
    function    GetMiniVUListCount : integer;
    function    GetLedListCount : integer;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TG2FMXControl read FSelectedControl write SetSelectedControl;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;
  end;

  TG2FMXPatchArea = class(TPanel)
  private
    FNewCable : TG2FMXCable;
    FFromConnector : TG2FMXConnector;

    FMoveLayout : TLayout;
    FStartX, FStartY : single;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure   MoveLayoutMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure   MoveLayoutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   CablesToFront;
  end;

  TG2GraphModule = class( TG2FileModule)
  private
    FOutlineRect    : TRectF;
    FOutlineVisible : boolean;
    FFreePanel      : boolean;
    FPanel          : TG2FMXModule;
  protected
    function    GetParent : TFmxObject;
    procedure   SetParent( aValue : TFmxObject);
    function    GetVisible : boolean;
    procedure   SetVisible( aValue : boolean);
    function    GetScrollPosX : single;
    function    GetScrollPosY : single;
    procedure   SetSelected( aValue: boolean); override;
    function    GetNewCol : TBits7; override;
    function    GetNewRow : TBits7; override;
    function    GetOnModuleClick : TModuleClickEvent;
    procedure   SetOnModuleClick( aValue : TModuleClickEvent);
    function    GetOnParameterClick: TParameterClickEvent;
    procedure   SetOnParameterClick( aValue : TParameterClickEvent);
    function    GetOnConnectorClick: TConnectorClickEvent;
    procedure   SetOnConnectorClick( aValue : TConnectorClickEvent);
  public
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModule);
    destructor  Destroy; override;
    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; override;
    function    CreateParameter: TG2FileParameter; override;
    procedure   ParsePanelData;
    function    ClientToScreen( p : TPoint):  TPoint;
    procedure   SetCol( aValue : TBits7); override;
    procedure   SetRow( aValue : TBits7); override;
    procedure   SetModuleColor( aValue : TBits8); override;
    procedure   SetModuleName( aValue : AnsiString); override;
  published
    property    Parent: TFmxObject read GetParent write SetParent;
    property    Visible: boolean read GetVisible write SetVisible;
    property    ScrollPosX : single read GetScrollPosX;
    property    ScrollPosY : single read GetScrollPosY;
    property    OnModuleClick : TModuleClickEvent read GetOnModuleClick write SetOnModuleClick;
    property    OnParameterClick : TParameterClickEvent read GetOnParameterClick write SetOnParameterClick;
    property    OnConnectorClick : TConnectorClickEvent read GetOnConnectorClick write SetOnConnectorClick;
  end;

  TG2FMXModule = class( TRectangle)
  // This represents the module
  private
    FData :   TG2GraphModule;
    FOldX,
    FOldY : single;
    FStartX,
    FStartY : single;
    FWasAlreadySelected : boolean;

    FModuleLabel : TLabel;

    FChildControls : TList;

    FOnModuleClick : TModuleClickEvent;
    FOnParameterClick : TParameterClickEvent;
    FOnConnectorClick : TConnectorClickEvent;

    function    GetScrollBarX : single;
    function    GetScrollBarY : single;
    function    GetScrollPosX : single;
    function    GetScrollPosY : single;
    procedure   SetSelected( aValue : boolean);
    function    GetSelected: boolean;
    function    GetLocation : TLocationType;
    function    GetModuleIndex : TBits8;
    function    GetColor : TAlphaColor;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SetCol( aValue : TBits7);
    procedure   SetRow( aValue : TBits7);
    function    GetNewCol : TBits7;
    function    GetNewRow : TBits7;

    function    GetGraphChildControl( ChildControlIndex : integer): TG2FMXControl;
    function    GetChildControlsCount : integer;
    procedure   AddGraphControl( aGraphCtrl : TG2FMXControl);

    procedure   SelectModule;
    procedure   MoveOutline( dX, dY : single);
    procedure   MoveModule;

    property    ScrollBarX : single read GetScrollBarX;
    property    ScrollBarY : single read GetScrollBarY;
    property    ScrollPosX : single read GetScrollPosX;
    property    ScrollPosY : single read GetScrollPosY;
    property    GraphChildControls[ index : integer] : TG2FMXControl read GetGraphChildControl;
    property    ChildControlsCount : integer read GetChildControlsCount;

    function    GetPatch : TG2GraphPatch;
    function    GetControlType( aG2GraphChildControl: TG2FMXControl): string;
    function    NewG2GraphControl( aControlType : string) : TG2FMXControl;

    procedure   ParsePanelData;

  published
    property    OnMouseDown;
    property    OnMouseUp;
    property    OnMouseMove;
    property    OnClick;
    property    OnModuleClick : TModuleClickEvent read FOnModuleClick write FOnModuleClick;
    property    OnParameterClick : TParameterClickEvent read FOnParameterClick write FOnParameterClick;
    property    OnConnectorClick : TConnectorClickEvent read FOnConnectorClick write FOnConnectorClick;
    property    Selected : boolean read GetSelected write SetSelected;
    property    NewRow : TBits7 read GetNewRow;
    property    NewCol : TBits7 read GetNewCol;
    property    Data : TG2GraphModule read FData write FData;
    property    Location    : TLocationType read GetLocation;
    property    ModuleIndex : TBits8 read GetModuleIndex;
    //property    Color : TColor read GetColor;
  end;

  TG2ImageList = class( TObjectList)
  private
    // Class to hold the bitmaps from the module resource files
    FBitmapWidth  : integer;
    FBitmapHeight : integer;
    FImageData    : TStrings;
  protected
    function      GetBitmap( aIndex : integer) : TBitmap;
    procedure     SetBitmap( aIndex : integer; aValue : TBitmap);
    procedure     SetImageData( aValue : TStrings);
    function      GetBoundsRect: TRectF;
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    procedure   ParseImageData( ImageCount : integer; Hex : boolean);
    function    AddBitmap( aValue : TBitmap): integer;

    property    BoundsRect : TRectF read GetBoundsRect;
    property    ImageData : TStrings read FImageData write SetImageData;
    property    Items[ index : integer]: TBitmap read GetBitmap write SetBitmap;
    property    BitmapWidth : integer read FBitmapWidth write FBitmapWidth;
    property    BitmapHeight : integer read FBitmapHeight write FBitmapHeight;
  end;

  TG2FMXControl = class( TControl)
  // This represents a child graphic control of another graphic control (module)
  protected
    FSelected       : boolean;
    FModule         : TG2FMXModule;
    FParameter      : TG2FileParameter;
    FMouseInput     : boolean;
    FZOrder         : integer;
    FValue,
    FStartValue,
    FLowValue,
    FHighValue      : byte;

    FOnChange       : TChangeEvent;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure   Select;
    procedure   DeSelect; virtual;
    procedure   SetLowValue( aValue: byte);
    procedure   SetHighValue( aValue: byte);
    function    GetSelected : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetParameter( aParam : TG2FileParameter); virtual;
    procedure   SetModule( aValue : TG2FMXModule); virtual;
    procedure   SetValue( aValue : byte);
    function    GetValue : byte;
    procedure   SetParamLabel( aIndex : integer; aValue : AnsiString);
    function    GetParamLabel( aIndex : integer) : AnsiString;
    function    GetParamLabelIndex: integer;
    procedure   InitValue( aValue: integer);
    function    CheckValueBounds( aValue : integer): byte;
    procedure   SetMorphValue( aValue: byte);
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    function    GetGraphModule : TG2FMXModule;
    function    GetMorphValue: byte;
    function    HasMorph: boolean;
    function    GetMorph : TMorphParameter;
    function    GetParamIndex : integer;
    procedure   ParsePanelData( fs : TModuleDefStream); virtual;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; virtual;

    property    Parameter   : TG2FileParameter read FParameter write SetParameter;
    property    ParamIndex  : integer read GetParamIndex;
    property    ParamLabel[ Index : integer]: AnsiString read GetParamLabel write SetParamLabel;
    property    Module      : TG2FMXModule read FModule write SetModule;
    property    MouseInput  : boolean read FMouseInput write FMouseInput;
    property    Selected    : boolean read GetSelected;
  published
    property    Value       : byte read GetValue write SetValue;
    property    MorphValue  : byte read GetMorphValue;
    property    LowValue    : byte read GetLowValue write SetLowValue;
    property    HighValue   : byte read GetHighValue write SetHighValue;
    property    OnMouseUp;
    property    OnMouseDown;
    property    OnMouseMove;
  end;

  TG2GraphParameter = class(TG2FileParameter)
  private
    FControlList  : array of TG2FMXControl; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   AssignControl( aControl : TG2FMXControl);
    procedure   DeassignControl( aControl : TG2FMXControl);
    procedure   InvalidateControl; override;
  end;

  TG2FMXLabel = class( TG2FMXControl)
  private
    FCaption : string;
    FFontSize : integer;
    FOnClick : TClickEvent;
  protected
    procedure   SetCaption( aValue : string);
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Single); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    Caption : string read FCaption write SetCaption;
    property    OnClick : TClickEvent read FOnClick write FOnClick;
  end;

  TG2FMXDisplay = class( TG2FMXControl)
  private
    // TStrings gives problems in VST?
    FLine1,
    FLine2,
    FLine3,
    FLine4 : AnsiString;
    FLineCount : integer;
    FTextFunction : integer;
    FMasterRef : integer;
    FDisplayType : integer;
  protected
    function    GetLine( LineNo : integer): AnsiString;
    procedure   SetLine( LineNo : integer; aValue : AnsiString);
    procedure   SetLineCount( aValue : integer);
    procedure   SetTextFunction( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetParameter( aParam : TG2FileParameter); override;
    property    Line[ index : integer] : AnsiString read GetLine write SetLine;
  published
    property    LineCount : integer read FLineCount write SetLineCount;
    property    TextFunction : integer read FTextFunction write SetTextFunction;
    property    DisplayType : integer read FDisplayType write FDisplayType;
    //property    Font;
    //property    Color;
  end;

  TButtonBevel = class(TShape)
  private
    FInversed : boolean;
    FBevelThickness : single;
    FPointsUpperLeft,
    FPointsLowerRight: TPolygon;
    FDarkSideColor : TAlphaColor;
    FLightSideColor : TAlphaColor;
    FillDark : TBrush;
    FillLight : TBrush;
  protected
    procedure SetBevelThickness( aValue : single);
    procedure SetDarkSideColor( aValue : TAlphaColor);
    procedure SetLightSideColor( aValue : TAlphaColor);
    procedure SetInversed( aValue : boolean);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BevelThickness : single read FBevelThickness write SetBevelThickness;
    property DarkSideColor : TAlphaColor Read FDarkSideColor write SetDarkSideColor;
    property LightSideColor : TAlphaColor Read FLightSideColor write SetLightSideColor;
    property Inversed : boolean read FInversed write SetInversed;
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

  TG2FMXButton = class( TG2FMXControl)
  private
    FButtonText     : TStrings;
    FImageList      : TG2ImageList;
    FButtonWidth    : single;
    FButtonHeight   : single;
    FButtonCount    : integer;

    FOrientation    : TOrientationType;
    FHighlightColor : TAlphaColor;
    FColor : TAlphaColor;
    FBorderColor    : TAlphaColor;
    FDarkSideColor : TAlphaColor;
    FLightSideColor : TAlphaColor;

    FPointsUpperLeft,
    FPointsLowerRight: TPolygon;
    FBevelVisible   : boolean;

    FImageWidth     : integer;
    FImageCount     : integer;
    FCaption        : string;
    FIcon           : TIconType;
    FPressed        : boolean;


    FOnClick        : TClickEvent;

  protected
    procedure   MouseDown( Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   SetCaption( aValue : string);
    procedure   SetIcon( aValue : TIconType);
    procedure   SetButtonText( aValue : TStrings);
    procedure   SetColor( aValue : TAlphaColor);
    procedure   SetHighLightColor( aValue : TAlphaColor);
    procedure   SetBorderColor( aValue : TAlphaColor);
    procedure   SetDarkSideColor( aValue : TAlphaColor);
    procedure   SetLightSideColor( aValue : TAlphaColor);

    procedure   SetButtonCount( aValue : integer); virtual;
    procedure   SetBevelVisible( aValue : boolean);
    procedure   SetOrientation( aValue : TOrientationType); virtual;
    procedure   Paint; override;
    procedure   DrawBevel;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;


    property    Module : TG2FMXModule read FModule;
    property    Icon : TIconType read FIcon write SetIcon;
  published
    property    Caption : string read FCaption write SetCaption;
    property    OnClick : TClickEvent read FOnClick write FOnClick;
    //property    ButtonText : TStrings read FButtonText write SetButtonText;
    property    HightlightColor : TAlphaColor read FHighlightColor write SetHighlightColor;
    property    BorderColor : TAlphaColor read FBorderColor write SetBorderColor;
    property    DarkSideColor : TAlphaColor Read FDarkSideColor write SetDarkSideColor;
    property    LightSideColor : TAlphaColor Read FLightSideColor write SetLightSideColor;
    property    Color : TAlphaColor Read FColor write SetColor;
    property    BevelVisible : boolean read FBevelVisible write SetBevelVisible;
    //property    Orientation : TOrientationType read FOrientation write SetOrientation;
    //property    ButtonCount : integer read FButtonCount write SetButtonCount;
  end;

  TG2FMXButtonText = class( TG2FMXButton)
  private
    FButtonTextType : TButtonTextType;
    procedure   SetButtonTextType( aValue : TButtonTextType);
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   Paint; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    ButtonTextType : TButtonTextType read FButtonTextType write SetButtonTextType;
  end;

  TG2FMXTextEdit = class( TG2FMXButtonText)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TG2FMXButtonFlat = class( TG2FMXButton)
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   Paint; override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
  end;

  TG2FMXLevelShift = class( TG2FMXButtonFlat)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
  end;

  TG2FMXButtonRadio = class( TG2FMXButton)
  protected
    FUpsideDown : boolean;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Single; Y: Single); override;
    procedure   SetOrientation( aValue : TOrientationType); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
    procedure   SetButtonCount( aValue : integer); override;
    procedure   SetBounds(ALeft: Single; ATop: Single;  AWidth: Single; AHeight: Single); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    //procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); override;

    property    UpsideDown : boolean read FUpsideDown write FUpsideDown;
  end;

  TG2FMXKnob = class( TG2FMXControl)
  private
    FOrientation      : TOrientationType;
    FCenterButtonSize,
    FSliderSize       : integer;
    FSliderSelected   : boolean;
    FKnobRect,
    FCenterButtonRect,
    FIncBtnRect,
    FDecBtnRect       : TRectF;
    FNeedle           : TPolygon;
    FKnobRad          : integer;
    FType             : TKnobType;
    FStartX,
    FStartY           : single;
    FHighlightColor   : TAlphaColor;
    FFill,
    FFillMorph        : TBrush;
    procedure   SetOrientation( aValue : TOrientationType);
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure   SetKnobType( aValue : TKnobType);
    procedure   SetHighLightColor( aValue : TAlphaColor);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Paint; override;
    function    GetSliderRect: TRectF;
    function    GetMorphRect: TRectF;
    procedure   SetBounds(ALeft: single; ATop: single;  AWidth: single; AHeight: single); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
 published
    property    KnobType : TKnobType read FType write SetKnobType;
    property    Orientation : TOrientationType read FOrientation write SetOrientation;
    property    HightlightColor : TAlphaColor read FHighlightColor write SetHighlightColor;
  end;


  TG2FMXConnector = class(TG2FMXControl)
  private
    { Private declarations }
    FFillInner, FFillOuter : TBrush;
    FData   : TG2FileConnector;
  protected
    { Protected declarations }
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure   MouseMove(Shift: TShiftState; X, Y: Single); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  Paint; override;
    procedure  ParsePanelData(fs: TModuleDefStream);
    function   ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
    procedure  SetData(aConnectorData: TG2FileConnector);

    property Data : TG2FileConnector read FData write SetData;
  end;

  TNode = class
    x  : single;
    y  : single;
    vx : single;
    vy : single;
  end;

  TG2FMXCable = class(TStyledControl)
  private
    { Private declarations }
    FP1, FP2 : TPointF;
    FNodeCount : integer;
    FNodes : array of TNode;
    FMargin : single;
    FTimer : TTimer;
    FTimerCount : integer;
  protected
    { Protected declarations }
    procedure SetPoint1X( Value : single);
    procedure SetPoint1Y( Value : single);
    procedure SetPoint2X( Value : single);
    procedure SetPoint2Y( Value : single);
    procedure IterateCable;
    procedure OnTimer(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StartTimer;
    procedure InitCable;
  published
    { Published declarations }
    property Point1X : single read FP1.X write SetPoint1X;
    property Point1Y : single read FP1.Y write SetPoint1Y;
    property Point2X : single read FP2.X write SetPoint2X;
    property Point2Y : single read FP2.Y write SetPoint2Y;
  end;

  TG2GraphCable = class( TG2FileCable)
  protected
    FParent         : TFmxObject;
    FPatch          : TG2GraphPatch;
    FModule         : TG2FMXModule;
    FGraphControl   : TG2FMXCable;
    FSelected       : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   ConnectorMoved; override;
    procedure   SetParent( aValue : TFmxObject);

    property    GraphControl : TG2FMXCable read FGRaphControl write FGRaphControl;
    property    Parent : TFmxObject read FParent write SetParent;
  end;

function  Darker(c : TAlphaColor; f : byte): TAlphaColor;
function  Lighter(c : TAlphaColor; f : byte): TAlphaColor;

procedure Register;

implementation
uses
  Math;

function Darker(c : TAlphaColor; f : byte): TAlphaColor;
var R, G, B : byte;

  function sub( comp : byte): byte;
  begin
    if comp - f > 0 then
      result := comp - f
    else
      result := 0;
  end;

begin
  //if c < 0 then
  //  c := ColorToRGB(c);

  R := (c and $000000FF);
  G := (c and $0000FF00) shr 8;
  B := (c and $00FF0000) shr 16;

  result := sub(B) * 65536
          + sub(G) * 256
          + sub(R);
end;

function Lighter(c : TAlphaColor; f : byte): TAlphaColor;
var R, G, B : byte;

  function add( comp : byte): byte;
  begin
    if comp + f < 255 then
      result := comp + f
    else
      result := 255;
  end;

begin
  //if c < 0 then
  //  c := ColorToRGB(c);

  R := (c and $000000FF);
  G := (c and $0000FF00) shr 8;
  B := (c and $00FF0000) shr 16;

  result := add(B) * 65536
          + add(G) * 256
          + add(R);
end;


{function PointInRect( X, Y : single; Rect : TRect): boolean;
begin
  Result := PtInRect( Rect, PointF(X, Y))
end;}

{ ==== TG2Graph ================================================================}

constructor TG2Graph.Create(AOwner: TComponent);
begin
  inherited;
  FClientType := ctEditor;
end;

function TG2Graph.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2GraphPerformance.Create(self);
end;

destructor TG2Graph.Destroy;
begin
  inherited;
end;

function TG2Graph.GetSelectedPatch: TG2GraphPatch;
begin
  if assigned(Performance) then
    Result := Performance.Slot[ Performance.SelectedSlot].Patch as TG2GraphPatch
  else
    Result := nil;
end;

procedure TG2Graph.SetClientType( aValue: TClientType);
begin
  FClientType := aValue;
end;

procedure TG2Graph.SetPatchAreaVA( aValue : TG2FMXPatchArea);
begin
  FLayoutVA := aValue;
end;

procedure TG2Graph.SetPatchAreaFX( aValue : TG2FMXPatchArea);
begin
  FLayoutFX := aValue;
end;

function TG2Graph.G2MessageDlg( tekst : string; code : integer): integer;
begin
  //
end;

procedure TG2Graph.G2ProcessWindowsMessages;
begin
  Application.ProcessMessages;
end;

// ==== TG2GraphSlot ===========================================================

constructor TG2GraphSlot.Create(AOwner: TComponent);
begin
  inherited;

end;

function TG2GraphSlot.CreatePatch: TG2FilePatch;
begin
  Result := TG2GraphPatch.Create( self);
end;

destructor TG2GraphSlot.Destroy;
begin
  inherited;
end;

// ==== TG2GraphPerformance ====================================================

constructor TG2GraphPerformance.Create(AOwner: TComponent);
begin
  inherited;
end;

function TG2GraphPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2GraphSlot.Create( self);
end;

destructor TG2GraphPerformance.Destroy;
begin
  inherited;
end;

// ==== TG2GraphPatch ==========================================================

constructor TG2GraphPatch.Create(AOwner: TComponent);
begin
  FMiniVUList := TList.Create;
  FLedGroupList := TList.Create;
  FLed39List := TList.Create;
  FLed3AList := TList.Create;

  FSelectedMorphIndex := 0;

  inherited;
end;

destructor TG2GraphPatch.Destroy;
var i : integer;
begin
  FLed3AList.Free;
  FLed39List.Free;
  FLedGroupList.Free;
  FMiniVUList.Free;

  inherited;
end;

procedure TG2GraphPatch.Init;
var i : integer;
begin
  inherited;

  FSelectedControl := nil;

  FLed3AList.Clear;
  FLed39List.Clear;
  FLedGroupList.Clear;
  FMiniVUList.Clear;
end;

function TG2GraphPatch.CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule;
var i : integer;
    MOdule : TG2GraphModule;
begin
  // Create a module in a patch file

  Result := nil;
  Module := TG2GraphModule.Create( PatchPart[ ord(aLocation)]);
  Module.ModuleIndex := aModuleIndex;
  Module.TypeID := aModuleType;

  if assigned( G2) and assigned(G2.FModuleDefList) and assigned(G2.FParamDefList) then begin
    i := 0;
    while (i < G2.FModuleDefList.Count) and ( G2.FModuleDefList.ModuleDef[i].ModuleType <> aModuleType) do
      inc(i);

    if (i < G2.FModuleDefList.Count) then begin

      Module.InitModule( aLocation, G2.FModuleDefList.ModuleDef[i], G2.FParamDefList);

      if (G2.ClientType <> ctVST) then begin
        if aLocation = ltVA then begin
          Module.Parent := (G2 as TG2Graph).FLayoutVA;//FG2.FForm; //.ScrollboxVA;
          Module.ParsePanelData;
        end else
          if aLocation = ltFX then begin
            Module.Parent := (G2 as TG2Graph).FLayoutFX;//.ScrollboxFX;
            Module.ParsePanelData;
          end;
      end else
        Module.Parent := nil;

    end else
      raise Exception.Create('Unknown module type ' + IntToStr( aModuleType));;
  end;

  Module.Visible := Visible;
  Module.Location := aLocation;

//  if assigned(FG2USB) and assigned((FG2USB as TG2).FOnCreateModule) then
//    (FG2USB as TG2).FOnCreateModule(self, Module);

  Result := Module;
end;

function TG2GraphPatch.CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
var i : integer;
    FromConnKind : TConnectorKind;
    ModuleFrom, ModuleTo : TG2GraphModule;
    Cable : TG2GraphCable;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  // Create a cable connection in a patch file

  Result := nil;

  ModuleFrom := GetModule( ord(aLocation), aFromModule) as TG2GraphModule;
  if not assigned(ModuleFrom) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aFromModule) + ' not found.');

  ModuleTo := GetModule( ord(aLocation), aToModule) as TG2GraphModule;
  if not assigned(ModuleTo) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aToModule) + ' not found.');

  Cable               := TG2GraphCable.Create( self);
  Cable.CableColor    := aColor;
  Cable.ModuleFrom    := aFromModule;
  Cable.ConnectorFrom := aFromConnector;
  Cable.LinkType      := aLinkType;
  Cable.ModuleTo      := aToModule;
  Cable.ConnectorTo   := aToConnector;

  // If Linktype is 1 then the first connector is an output, else it's an input (i guess)
  if aLinkType = 1 then
    FromConnKind := ckOutput
  else
    FromConnKind := ckInput;

  if assigned( G2) and ( G2.ClientType <> ctVST) then begin

    // Link connectors to cable
    if FromConnKind = ckInput then
      Cable.FromConnector := ModuleFrom.InConnector[ aFromConnector]
    else
      Cable.FromConnector := ModuleFrom.OutConnector[ aFromConnector];

    Cable.ToConnector := ModuleTo.InConnector[ aToConnector];

    // Add cable to connectors
    Cable.FromConnector.AddCable(Cable);
    Cable.ToConnector.AddCable(Cable);

    if aLocation = ltFX then
      Cable.Parent := (G2 as TG2Graph).FLayoutFX
    else
      Cable.Parent := (G2 as TG2Graph).FLayoutVA;

    // Cable needs scrollbar coords
    ConnectorFrom := Cable.FromConnector.GraphControl as TG2FMXConnector;
    ConnectorTo := Cable.ToConnector.GraphControl as TG2FMXConnector;

//    Cable.GraphControl.Point1X := trunc(ModuleFrom.ScrollPosX +  ConnectorFrom.Position.X + ConnectorFrom.Width / 2);
//    Cable.GraphControl.Point1Y := trunc(ModuleTo.ScrollPosY +  ConnectorFrom.Position.Y + ConnectorFrom.Height / 2);
//    Cable.GraphControl.Point2X := Cable.GraphControl.Point1X;
//    Cable.GraphControl.Point2Y := Cable.GraphControl.Point1Y;
//    Cable.GraphControl.InitCable;
    Cable.InitCable;
  end;

  Result := Cable;
end;

procedure TG2GraphPatch.SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
var i, temp : integer;
begin
  if aRect.Left > aRect.Right then begin
    temp := aRect.Left;
    aRect.Left := aRect.Right;
    aRect.Right := temp;
  end;

  if aRect.Top > aRect.Bottom then begin
    temp := aRect.Top;
    aRect.Top := aRect.Bottom;
    aRect.Bottom := temp;
  end;

  UnselectModules( ltFX);
  UnselectModules( ltVA);
  for i := 0 to ModuleList[ ord(aLocation)].Count - 1 do begin
{    if PointInRect( (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Left,
                    (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Top,
                    aRect) then}
      ModuleList[ ord(aLocation)][i].Selected := True;
  end;
end;

procedure TG2GraphPatch.MoveOutlines( aLocation : TLocationType; dX, dY: single);
var i : integer;
    Module : TG2GraphModule;
begin
  {for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveOutline( dX, dY);}
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do begin
    Module := (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule);
    Module.FPanel.MoveOutline(dX, dY);
  end;

end;

function TG2GraphPatch.MessMoveModules( aLocation : TLocationType): boolean;
var i : integer;
begin
  Result := inherited MessMoveModules( aLocation);
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveModule;
end;

function TG2GraphPatch.CreateParameter( aModuleIndex : byte): TG2FileParameter;
begin
  Result := TG2GraphParameter.Create( self, ltPatch, aModuleIndex, nil);
end;

function TG2GraphPatch.GetLedListCount: integer;
begin
  Result := FLed39List.Count;
end;

function TG2GraphPatch.GetMiniVUListCount: integer;
begin
  Result := FLed3AList.Count;
end;

function CompareLedGreenOrder( Led1 : pointer; Led2 : pointer): integer;
begin
{  if TG2GraphLedGreen(Led1).Module.Location > TG2GraphLedGreen(Led2).Module.Location then
    Result := -1
  else
    if TG2GraphLedGreen(Led1).Module.Location =  TG2GraphLedGreen(Led2).Module.Location then begin
      if TG2GraphLedGreen(Led1).Module.ModuleIndex > TG2GraphLedGreen(Led2).Module.ModuleIndex then
        Result := 1
      else
        if TG2GraphLedGreen(Led1).Module.ModuleIndex = TG2GraphLedGreen(Led2).Module.ModuleIndex then begin
          if TG2GraphLedGreen(Led1).FGroupId > TG2GraphLedGreen(Led2).FGroupID then
            Result := 1
          else
            if TG2GraphLedGreen(Led1).FGroupId = TG2GraphLedGreen(Led2).FGroupID then begin
              if TG2GraphLedGreen(Led1).FCodeRef > TG2GraphLedGreen(Led2).FCodeRef then
                Result := 1
              else
                if TG2GraphLedGreen(Led1).FCodeRef = TG2GraphLedGreen(Led2).FCodeRef then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;}
end;

function CompareMiniVUOrder( Led1, Led2: pointer): integer;
begin
{  if TG2GraphLed(Led1).Module.Location > TG2GraphLed(Led2).Module.Location then
    Result := -1
  else
    if TG2GraphLed(Led1).Module.Location = TG2GraphLed(Led2).Module.Location then begin
      if TG2GraphLed(Led1).Module.ModuleIndex > TG2GraphLed(Led2).Module.ModuleIndex then
        Result := 1
      else
        if TG2GraphLed(Led1).Module.ModuleIndex = TG2GraphLed(Led2).Module.ModuleIndex then begin
          if TG2GraphLed(Led1).FGroupId > TG2GraphLed(Led2).FGroupID then
            Result := 1
          else
            if TG2GraphLed(Led1).FGroupId = TG2GraphLed(Led2).FGroupID then
              Result := 0
            else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;}
end;

procedure TG2GraphPatch.SetLedLevel( Index: integer; aValue: byte);
begin
//  TG2GraphLedGreen(FLed39List[Index]).SetLevel( aValue)
end;

procedure TG2GraphPatch.SetMiniVULevel( Index: integer; aValue: byte);
begin
//  TG2GraphLed(FLed3AList[Index]).SetLevel( aValue)
end;

procedure TG2GraphPatch.SetSelectedControl( aValue: TG2FMXControl);
begin
  if assigned(FSelectedControl) then
    FSelectedControl.DeSelect;

  FSelectedControl := aValue;
  if assigned(FSelectedControl) then
    FSelectedControl.Select;
end;

procedure TG2GraphPatch.SetSelectedMorphIndex( aValue: integer);

  procedure SetControlMorphUpdate;
  var m, l, p, Count : integer;
      Module : TG2FileModule;
      Param : TG2FileParameter;
  begin
    for l := 0 to 1 do begin
      Count := ModuleCount[ l];
      for m := 0 to Count - 1 do begin
        Module := Modules[ l, m];
        if assigned(Module) then begin
          for p := 0 to Module.ParameterCount - 1 do begin
            Param := Module.Parameter[p];
            if assigned(Param) then begin
               if Param.HasMorph then
                 Param.InvalidateControl;
            end;
          end;
        end;
      end;
    end;
  end;

begin
  if aValue <> FSelectedMorphIndex then begin
    FSelectedMorphIndex := aValue;
    //SetControlMorphUpdate;
  end;
end;

procedure TG2GraphPatch.SetVisible( aValue: boolean);
var i, j : integer;
begin
  if aValue <> FVisible then begin
    for i := 0 to 1 do
      for j := 0 to ModuleCount[i] - 1 do
        (ModuleList[i].Items[j] as TG2GraphModule).FPanel.Visible := aValue;
    FVisible := aValue;
  end;
end;

procedure TG2GraphPatch.SortLeds;
var i : integer;
begin
{  // Take leds that are in a led goup with only one led out of the group list
  // and put them in de led list.
  // These leds are addressed in message $39
  // VU-meters and ledgroups with more than 1? led are addressed in message $3A

  FLed39List.Clear;
  FLed3AList.Clear;

  i := 0;
  while i < FLedGroupList.Count do begin

    if TG2GraphLedGroup( FLedGroupList[i]).FLeds.Count = 1 then begin
      FLed39List.Add( TG2GraphLedGroup( FLedGroupList[i]).FLeds[0])
    end else
      FLed3AList.Add( FLedGroupList[i]);

    inc(i);
  end;

  for i := 0 to FMiniVUList.Count - 1 do
    FLed3AList.Add(FMiniVUList[i]);

  FLed3AList.Sort( CompareMiniVUOrder);
  FLed39List.Sort( CompareLedGreenOrder);}
end;

procedure TG2GraphPatch.RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);
var i : integer;
begin
{  // When a module is deleted, it's leds must be removed from the lists

  i := 0;
  while (i < FMiniVUList.Count) do begin
    if (TG2GraphLed(FMiniVUList.Items[i]).FModule.ModuleIndex = aModuleIndex) and
       (TG2GraphLed(FMiniVUList.Items[i]).FModule.Location = aLocation) then
      FMiniVUList.Delete(i)
    else
      inc(i);
  end;

  i := 0;
  while (i < FLedGroupList.Count) do begin
    if (TG2GraphLed(FLedGroupList.Items[i]).FModule.ModuleIndex = aModuleIndex) and
       (TG2GraphLed(FLedGroupList.Items[i]).FModule.Location = aLocation) then
      FLedGroupList.Delete(i)
    else
      inc(i);
  end;

  SortLeds;}
end;

// ==== TG2FMXPatchArea ========================================================

constructor TG2FMXPatchArea.Create( AOwner: TComponent);
begin
  inherited;
  FNewCable := nil;
end;

destructor TG2FMXPatchArea.Destroy;
begin
  inherited;
end;

procedure TG2FMXPatchArea.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
end;

procedure TG2FMXPatchArea.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if assigned(FNewCable) then begin
     FreeAndNil(FNewCable);
  end;
  inherited;
end;

procedure TG2FMXPatchArea.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if (ssLeft in Shift) and assigned(FNewCable) then begin
    FNewCable.Point2X := X;
    FNewCable.Point2Y := Y;
    FNewCable.StartTimer;
  end;
end;

procedure TG2FMXPatchArea.CablesToFront;
var i : integer;
    CableList : TList;
begin
  CableList := TList.Create;
  try
    for i := 0 to ChildrenCount-1 do
      if Children[i] is TG2FMXCable then
        CableList.Add(Children[i]);

    for i := 0 to CableList.Count-1 do
      TG2FMXCable(CableList[i]).BringToFront;
  finally
    CableList.Free;
  end;
end;

procedure TG2FMXPatchArea.MoveLayoutMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if (Sender is TLayout) then begin
    if assigned(FMoveLayout) then begin
      FMoveLayout.Free;
      //Patch.MessMoveModules( Data.Location);
    end;
  end;
end;

procedure TG2FMXPatchArea.MoveLayoutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var MoveLayout : TLayout;
    P: TPointF;
begin
  if (ssLeft in Shift) then
    if (Sender is TLayout) and assigned(FMoveLayout) then begin

      MoveLayout := Sender as TLayout;

      P := MoveLayout.LocalToAbsolute(PointF(X, Y));
      if Assigned(MoveLayout.Parent) and (MoveLayout.Parent is TControl) then
        P := TControl(MoveLayout.Parent).AbsoluteToLocal(P);

        MoveLayout.Position.X := P.X - FStartX;
        MoveLayout.Position.Y := P.Y - FStartY;
    end;
end;

// ==== G2GraphModule ==========================================================

constructor TG2GraphModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FPanel := TG2FMXModule.Create( aPatchPart);
  FPanel.FData := self;
  FFreePanel := True;
  FOutlineRect := FPanel.BoundsRect;
  FOutlineVisible := False;
end;

constructor TG2GraphModule.CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModule);
begin
  inherited Create( aPatchPart);
  Copy( aModule);
  FFreePanel := False;
  FPanel := aModule.FPanel;
  FOutlineRect := FPanel.BoundsRect;
  FOutlineVisible := False;
end;

destructor TG2GraphModule.Destroy;
begin
  if FFreePanel then
    FPanel.Free;
  inherited;
end;

function TG2GraphModule.CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule;
begin
  Result := TG2GraphModule.CopyCreate( aPatchPart, self);
end;


function TG2GraphModule.CreateParameter: TG2FileParameter;
begin
  Result := TG2GraphParameter.Create( PatchPart.Patch, TLocationType(Location), ModuleIndex, self);
end;

function TG2GraphModule.GetNewCol: TBits7;
begin
  if assigned(FPanel) then
    Result := FPanel.GetNewCol
  else
    Result := 0;
end;

function TG2GraphModule.GetNewRow: TBits7;
begin
  if assigned(FPanel) then
    Result := FPanel.GetNewRow
  else
    Result := 0;
end;

function TG2GraphModule.GetOnConnectorClick: TConnectorClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnConnectorClick
  else
    Result := nil;
end;

function TG2GraphModule.GetOnModuleClick: TModuleClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnModuleClick
  else
    Result := nil;
end;

function TG2GraphModule.GetOnParameterClick: TParameterClickEvent;
begin
  if assigned(FPanel) then
    Result := FPanel.OnParameterClick
  else
    Result := nil;
end;

function TG2GraphModule.GetParent : TFmxObject;
begin
  if assigned(FPanel) then
    Result := FPanel.Parent
  else
    Result := nil;
end;

function TG2GraphModule.GetScrollPosX: single;
begin
  if assigned(FPanel) then
    Result := FPanel.ScrollPosX
  else
    Result := 0;
end;

function TG2GraphModule.GetScrollPosY: single;
begin
  if assigned(FPanel) then
    Result := FPanel.ScrollPosY
  else
    Result := 0;
end;

function TG2GraphModule.GetVisible: boolean;
begin
  if assigned(FPanel) then
    Result := FPanel.Visible
  else
    Result := False;
end;

procedure TG2GraphModule.ParsePanelData;
begin
  if assigned(FPanel) then
    FPanel.ParsePanelData;
end;

procedure TG2GraphModule.SetCol(aValue: TBits7);
begin
  if assigned(FPanel) and (FFreePanel = True) then
    FPanel.SetCol( aValue);
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModule.SetModuleColor( aValue: TBits8);
begin
  inherited;
  if assigned(FPanel) then
    FPanel.Fill.Color := ModuleColors[ aValue] + $FF000000;
end;

procedure TG2GraphModule.SetModuleName(aValue: AnsiString);
begin
  inherited;
  if assigned(FPanel) then
    FPanel.FModuleLabel.Text := aValue;
end;

procedure TG2GraphModule.SetRow(aValue: TBits7);
begin
  if assigned(FPanel) and (FFreePanel = True) then
    FPanel.SetRow( aValue);
  inherited;
  InvalidateCables;
end;

procedure TG2GraphModule.SetOnConnectorClick(aValue: TConnectorClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnConnectorClick := aValue;
end;

procedure TG2GraphModule.SetOnModuleClick(aValue: TModuleClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnModuleClick := aValue;
end;

procedure TG2GraphModule.SetOnParameterClick(aValue: TParameterClickEvent);
begin
  if assigned(FPanel) then
    FPanel.OnParameterClick := aValue;
end;

procedure TG2GraphModule.SetParent( aValue : TFmxObject);
begin
  if assigned(FPanel) then
    FPanel.Parent := aValue;
end;

procedure TG2GraphModule.SetSelected( aValue: boolean);
begin
  inherited;
  FPanel.SetSelected( aValue);
end;

procedure TG2GraphModule.SetVisible(aValue: boolean);
begin
  if assigned(FPanel) then
    FPanel.Visible := aValue;
end;

function TG2GraphModule.ClientToScreen(p: TPoint): TPoint;
begin
{  if assigned(FPanel) then
    Result := FPanel.ClientToScreen(p)
  else
    Result := Point(0,0);}
end;


// ==== TG2FMXModule =================================================

constructor TG2FMXModule.Create(AOwner: TComponent);
begin
  inherited Create( AOWner);

  Opacity := 1;
  //CanClip := True;

  FChildControls := TList.Create;

  FOldX := ScrollPosX;
  FOldY := ScrollPosY;

  FWasAlreadySelected := False;

  FModuleLabel := TLabel.Create(self);
  FModuleLabel.Parent := self;
  FMOduleLabel.HitTest := False;
  FModuleLabel.Font.Family := 'Arial';
  FModuleLabel.Font.Size := 10;
  //FModuleLabel.Font.Style := fsBold;
  //FModuleLabel.Font.Color := $00000000;
  FModuleLabel.Position.X := 2;
  FModuleLabel.Position.Y := 2;
  FMOduleLabel.Text := 'Module';

  HitTest := True;


  Width := UNITS_COL;
  Height := UNITS_ROW;
end;

destructor TG2FMXModule.Destroy;
begin

  inherited;
end;

function TG2FMXModule.GetScrollBarX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).hscrollBar.Value
  else
    Result := 0;
end;

function TG2FMXModule.GetScrollBarY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).vscrollBar.Value
  else
    Result := 0;
end;

function TG2FMXModule.GetScrollPosX: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.X + (Parent as TScrollbox).hscrollBar.Value
  else
    Result := Position.X;
end;

function TG2FMXModule.GetScrollPosY: single;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Position.Y + (Parent as TScrollbox).vscrollBar.Value
  else
    Result := Position.Y;
end;

function TG2FMXModule.GetSelected: boolean;
begin
  Result := FData.Selected;
end;

procedure TG2FMXModule.SetSelected(aValue: boolean);
begin
  FOldX := Left;
  FOldY := Top;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2FMXModule.SetCol(aValue: TBits7);
begin
  //if aValue <> FData.Col then begin
    Position.X := aValue * UNITS_COL - ScrollBarX;
  //end;
end;

procedure TG2FMXModule.SetRow(aValue: TBits7);
begin
  //if aValue <> FData.Row then begin
    Position.Y := aValue * UNITS_ROW - ScrollbarY;
  //end;
end;

procedure TG2FMXModule.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var P : TPoint;
    i : integer;
    Patch : TG2GraphPatch;
    PatchArea : TG2FMXPatchArea;
begin
  {if assigned( FDropDownList) then begin
    FDropDownList.Free;
    FDropDownList := nil;
  end;}

  Patch := GetPatch;

    case Location of
      ltVA : Patch.UnselectModules(ltFX);
      ltFX : Patch.UnselectModules(ltVA);
    end;

    if not FData.Selected then begin
      if not(ssCtrl in Shift) then
        Patch.UnSelectModules( Location);
      FData.Selected := True;
      FWasAlreadySelected := False;
    end else
      FWasAlreadySelected := True;

    if Location <> Patch.SelectedLocation then
      Patch.SelectedLocation := Location;

    if ssLeft in Shift then begin
      FStartX := X;
      FStartY := Y;
      //Patch.SelectModules;

    {  BringToFront;
      PatchArea := Parent as TG2FMXPatchArea;
      if assigned(PatchArea) then
        PatchArea.CablesToFront;}


    end;

  inherited;
end;

procedure TG2FMXModule.MouseMove(Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Patch : TG2GraphPatch;
    Module : TG2GraphModule;
    BitMap : TBitmap;
    Image : TImage;
    i : integer;
    P: TPointF;
begin
  if assigned(Parent) then
    PatchArea := Parent as TG2FMXPatchArea;

  Patch := GetPatch;

  if assigned(PatchArea) and assigned(PatchArea.FNewCable) and (ssLeft in Shift) then
    PatchArea.MouseMove(Shift, Position.X + X, Position.Y + Y)
  else begin
    if (ssLeft in Shift) then
     { if FWasAlreadySelected then
        Patch.MoveOutlines( Data.Location, X - FStartX, Y - FStartY);}

      if ((abs(X-FStartX)>1) or (abs(Y-FStartY)>1)) then begin

        PatchArea.FMoveLayout := TLayout.Create(self);
        PatchArea.FMoveLayout.Parent := PatchArea;
        PatchArea.FMoveLayout.Position.X := 0;
        PatchArea.FMoveLayout.Position.Y := 0;
        PatchArea.FMoveLayout.Width := PatchArea.Width;
        PatchArea.FMoveLayout.Height := PatchArea.Height;
        PatchArea.FMoveLayout.HitTest := True;
        PatchArea.FMoveLayout.OnMouseMove := PatchArea.MoveLayoutMouseMove;
        PatchArea.FMoveLayout.OnMouseUp := PatchArea.MoveLayoutMouseUp;

        P := LocalToAbsolute(PointF(X, Y));
        P := PatchArea.AbsoluteToLocal(P);

        PatchArea.FStartX := P.X;
        PatchArea.FStartY := P.Y;

        for i := 0 to Patch.PatchPart[ ord(Location)].SelectedModuleList.Count - 1 do begin
          Module := (Patch.PatchPart[ ord(Location)].SelectedModuleList[i] as TG2GraphModule);
          Bitmap := Module.FPanel.MakeScreenshot;
          try
            Image := TImage.Create( PatchArea.FMoveLayout);
            Image.Bitmap.Assign( Bitmap);
            Image.Opacity := 0.6;
            Image.HitTest := False;
            Image.Position.Assign( Module.FPanel.Position);
            Image.Width := Module.FPanel.Width;
            Image.Height := Module.FPanel.Height;

            Image.Parent := PatchArea.FMoveLayout;
          finally
            BitMap.Free;
          end;
        end;
      end;

    inherited;
  end;
end;

procedure TG2FMXModule.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var NewCol, NewRow : byte;
    PatchArea : TG2FMXPatchArea;
    Patch : TG2GraphPatch;
begin
  if assigned(Parent) then
    PatchArea := Parent as TG2FMXPatchArea;

  Patch := GetPatch;

  if assigned(PatchArea) and assigned(PatchArea.FNewCable) then
    PatchArea.MouseUp(Button, Shift, Position.X + X, Position.Y + Y)
  else
    if FWasAlreadySelected then
     // if (FStartX <> X) or (FStartY <> Y) then
        Patch.MessMoveModules( Data.Location);


  if assigned( FOnModuleClick) then
    FOnModuleClick( self, Button, Shift, trunc(X), trunc(Y), FData);

  inherited;
end;

procedure TG2FMXModule.SelectModule;
begin
  FOldX := Position.X;
  FOldY := Position.Y;

  FData.FOutlineRect := BoundsRect;
end;

procedure TG2FMXModule.MoveOutline(dX, dY: single);
var i : integer;
    Module : TG2GraphModule;
begin
{  if assigned( Parent) and FData.FOutlineVisible then
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);

  FData.FOutlineRect.Left := FOldX + dX;
  FData.FOutlineRect.Top := FOldY + dY;
  FData.FOutlineRect.Right := FData.FOutlineRect.Left + Width;
  FData.FOutlineRect.Bottom := FData.FOutlineRect.Top + Height;

  if assigned( Parent) then begin
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
    FData.FOutlineVisible := True;
  end;}
  Position.X := Position.X + dX;
  Position.Y := Position.Y + dY;
  for i := 0 to FChildControls.Count - 1 do begin
    if TG2FMXControl(FChildControls[i]) is TG2FMXConnector then begin
      (TG2FMXControl(FChildControls[i]) as TG2FMXConnector).FData.InvalidateCables;
    end;

  end;

end;

procedure TG2FMXModule.MoveModule;
begin
  // TODO
{  if FData.FOutlineVisible then
    if assigned( Parent) then begin
      (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
      FData.FOutlineVisible := False;
    end;}
end;

function TG2FMXModule.GetNewCol: TBits7;
begin
  Result := trunc((ScrollPosX + FData.FOutlineRect.Left - FOldX) / UNITS_COL);
end;

function TG2FMXModule.GetNewRow: TBits7;
begin
  Result := trunc((ScrollPosY + FData.FOutlineRect.Top - FOldY) / UNITS_ROW);
end;

function TG2FMXModule.GetPatch: TG2GraphPatch;
begin
  if not assigned(FData.PatchPart.Patch) then
    raise Exception.Create('Patch not assigned to module.');

  Result := FData.PatchPart.Patch as TG2GraphPatch;
end;

procedure TG2FMXModule.ParsePanelData;
var //MemStream : TMemoryStream;
    ModuleStream : TModuleDefStream;
    CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2FMXControl;
    Connector : TG2FMXConnector;
    Param : TG2FileParameter;
    Patch : TG2GraphPatch;
begin
  aPath := ExtractFilePath(ParamStr(0));
  //aPath := GetCurrentDir;
{$IFDEF LINUX}
  aPath := aPath + 'Modules/';
{$ENDIF}

{$IFDEF MACOS}
  aPath := '/Applications/Delphi/PAServer/scratch-dir/Bruno-VBMac/Modules/';
{$ENDIF}

{$IFDEF MSWINDOWS}
  aPath := aPath + '\Modules\';
{$ENDIF}

  Patch := GetPatch;

  if FileExists( aPath + string(FData.ModuleFileName) + '.txt') then begin
    //MemStream := TMemoryStream.Create;
    //MemStream.LoadFromFile( aPath + FData.ModuleName + '.txt');
    ModuleStream := TModuleDefStream.Create(aPath + string(FData.ModuleFileName) + '.txt');
    try
      if ModuleStream.ReadConst('<#Module') then begin
        aName := 'Module';
        while (ModuleStream.Position < ModuleStream.Size) and (aName[1] <> '<') do begin
          ModuleStream.ReadSpaces;
          aName := ModuleStream.ReadUntil( [':', #13]);
          if aName[1] <> '<' then begin
            aValue := ModuleStream.ReadUntil( [#13]);

            if aName = 'Height' then
              Height := UNITS_ROW * StrToInt(string(aValue));
          end;
        end;

        while aName[1] = '<' do begin
          ControlType := copy(aName, 3, Length(aName) - 2);
          if ControlType = 'Input' then begin

            CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
            val( string(CodeRefStr), CodeRef, Err);
            if Err = 0 then begin
              Connector := TG2FMXConnector.Create(self);
              Connector.Module := self;
              Connector.Data := FData.InConnector[ CodeRef];
              if Connector.Data = nil then
                raise Exception.Create('Data for connector not found...');

              Connector.ParsePanelData( ModuleStream);
              AddGraphControl( Connector);
            end else
              raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' input connector CodeRef not found.' );

          end else
            if ControlType = 'Output' then begin

              CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
              val( string(CodeRefStr), CodeRef, Err);
              if Err = 0 then begin
                Connector := TG2FMXConnector.Create(self);
                Connector.Module := self;
                Connector.Data := FData.OutConnector[ CodeRef];
                if Connector.Data = nil then
                  raise Exception.Create('Data for connector not found...');

                Connector.ParsePanelData( ModuleStream);
                AddGraphControl( Connector);
              end else
                raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' output connector CodeRef not found.' );

            end else begin
              ChildControl := NewG2GraphControl(ControlType);
              if ChildControl <> nil then begin

                AddGraphControl( ChildControl);

                if ( ControlType = 'Knob') or
                   ( ControlType = 'ButtonIncDec') or
                   ( ControlType = 'ButtonRadio') or
                   ( ControlType = 'LevelShift') or
                   ( ControlType = 'ButtonFlat') or
                   ( ControlType = 'TextEdit') or
                   ( ControlType = 'PartSelector') or
                   ( ControlType = 'ButtonText') then begin

                  CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
                  val( string(CodeRefStr), CodeRef, Err);
                  if Err = 0 then begin
                    if ControlType = 'PartSelector' then begin
                      Param := FData.Mode[ CodeRef];
                    end else begin
                      Param := FData.Parameter[ CodeRef];
                    end;
                    ChildControl.ParsePanelData( ModuleStream);
                    ChildControl.Parameter := Param;
                  end else
                    raise Exception.Create('Parse error, module  ' + string(FData.ModuleName) + ' parameter CodeRef not found.' );

                end else
                  // No parameter associated
                  ChildControl.ParsePanelData( ModuleStream);

              end else begin
                while ( ModuleStream.Position < ModuleStream.Size) and (aName <> '#>') do begin
                  ModuleStream.ReadSpaces;
                  aName := ModuleStream.ReadUntil( [':', #13]);
                  if aName <> '#>' then begin
                    aValue := ModuleStream.ReadUntil( [#13]);
                  end;
                end;
              end;
            end;
          ModuleStream.ReadSpaces;
          aName := ModuleStream.ReadUntil( [':', #13]);
        end;

//        FChildControls.Sort(@CompareZOrder);
      end else
        raise Exception.Create('Unknown file type.');

      Patch.SortLeds;
    finally
      ModuleStream.Free;
    end;
  end;
end;


function TG2FMXModule.GetControlType( aG2GraphChildControl: TG2FMXControl): string;
begin
  if aG2GraphChildControl is TG2FMXControl then
    Result := '';

  if aG2GraphChildControl is TG2FMXConnector then
    Result := 'Connector';

  if aG2GraphChildControl is TG2FMXLabel then
    Result := 'Label';

  if aG2GraphChildControl is TG2FMXDisplay then
    Result := 'Display';

  {if aG2GraphChildControl is TG2GraphGraph then
    Result := 'Graph';

  if aG2GraphChildControl is TG2GraphLedGreen then
    Result := 'Led';

  if aG2GraphChildControl is TG2GraphMiniVU then
    Result := 'MiniVU';

  if aG2GraphChildControl is TG2GraphPartSelector then
    Result := 'PartSelector';}

  if aG2GraphChildControl is TG2FMXButtonText then
    Result := 'ButtonText';

  if aG2GraphChildControl is TG2FMXButtonFlat then
    Result := 'ButtonFlat';

  if aG2GraphChildControl is TG2FMXButtonRadio then
    Result := 'ButtonRadio';

  if aG2GraphChildControl is TG2FMXLevelShift then
    Result := 'LevelShift';

  if aG2GraphChildControl is  TG2FMXKnob then
    Result := 'Knob';

end;

function TG2FMXModule.NewG2GraphControl( aControlType: string): TG2FMXControl;
begin
  Result := nil;

  if (aControlType = 'Line') then begin
    //Result := TG2GraphLine.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Text') then begin
    //Result := TG2GraphLabel.Create(self);
    Result := TG2FMXLabel.Create(self);
    Result.Module := self;
  end;

  if aControlType = 'Connector' then begin
    Result := TG2FMXConnector.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextField') then begin
    //Result := TG2GraphDisplay.Create(self);
    Result := TG2FMXDisplay.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Graph') then begin
    //Result := TG2GraphGraph.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Led') then begin
    //Result := TG2GraphLedGreen.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'MiniVU') then begin
    //Result := TG2GraphMiniVU.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'PartSelector') then begin
    //Result := TG2GraphPartSelector.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonText') then begin
    Result := TG2FMXButtonText.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextEdit') then begin
    Result := TG2FMXTextEdit.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonFlat') then begin
    Result := TG2FMXButtonFlat.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'LevelShift') then begin
    Result := TG2FMXLevelShift.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonRadio') then begin
    Result := TG2FMXButtonRadio.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonIncDec') then begin
    //Result := TG2GraphButtonIncDec.Create(self);
    Result := TG2FMXControl.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Knob') or (aControlType = '') then begin
    Result := TG2FMXKnob.Create(self);
    Result.Module := self;
  end;

  //if not assigned( Result)  then
  //  raise Exception.Create('Unknown control type ' + aControlType);
end;


function TG2FMXModule.GetChildControlsCount: integer;
begin
  Result := FChildControls.Count;
end;

function TG2FMXModule.GetGraphChildControl( ChildControlIndex: integer): TG2FMXControl;
begin
  Result := TG2FMXControl( FChildControls[ChildControlIndex]);
end;

function TG2FMXModule.GetColor: TAlphaColor;
begin
  Result := FData.Color;
end;

function TG2FMXModule.GetLocation: TLocationType;
begin
  Result := FData.Location;
end;

function TG2FMXModule.GetModuleIndex: TBits8;
begin
  Result := FData.ModuleIndex;
end;

procedure TG2FMXModule.AddGraphControl(aGraphCtrl: TG2FMXControl);
begin
  FChildControls.Add( aGraphCtrl);
end;

// ==== TG2ImageList ===========================================================

constructor TG2ImageList.Create( AOwnsObjects : boolean);
begin
  inherited;

  FImageData := TStringList.Create;
end;

destructor TG2ImageList.Destroy;
begin
  FImageData.Free;

  inherited;
end;

function TG2ImageList.GetBitmap( aIndex: integer): TBitmap;
begin
  result := inherited Items[ aIndex] as TBitmap;
end;

function TG2ImageList.GetBoundsRect: TRectF;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := FBitmapWidth;
  Result.Height := FBitmapHeight;
end;

procedure TG2ImageList.SetBitmap( aIndex: integer; aValue: TBitmap);
begin
  inherited Items[ aIndex] := aValue;
end;

procedure TG2ImageList.SetImageData( aValue: TStrings);
begin
  FImageData.Assign( aValue);
end;

function TG2ImageList.AddBitmap( aValue : TBitmap): integer;
begin
  Result := inherited Add( aValue);
end;

{$IFDEF G2_VER240_up}
procedure TG2ImageList.ParseImageData( ImageCount : integer; Hex : boolean);
var Bitmap : TBitmap;
    i, j, k, b : integer;
    LNew : TAlphaColor;
    LScan : PAlphaColorArray;
    Rec: TBitmapData;
begin
  if (FImageData.Count = 0) or (FBitmapWidth = 0) then
    exit;

  FBitmapHeight := FImageData.Count div FBitmapWidth div ImageCount;

  b := 0;
  for i := 0 to ImageCount - 1 do begin
    Bitmap := TBitmap.Create( FBitmapWidth,
                              FBitmapHeight);

    Bitmap.Map( TMapAccess.maWrite, Rec);
    try
      for j := 0 to Bitmap.Height - 1 do begin
        //LScan := Bitmap.Scanline[j];
        for k := 0 to Bitmap.Width - 1 do begin
          if Hex then begin
            LNew      := $FF000000
                       + 256*256 *(16 * HexToByte( FImageData[b][1]) + 1 * HexToByte( FImageData[b][2]))
                       + 256 * (16 * HexToByte( FImageData[b][3]) + 1 * HexToByte( FImageData[b][4]))
                       + (16 * HexToByte( FImageData[b][5]) + 1 * HexToByte( FImageData[b][6]));
          end else begin
            LNew  := StrToInt(copy( FImageData[b], 1, 3))
                   + 256 * StrToInt(copy( FImageData[b], 4, 3))
                   + 256*256* StrToInt(copy( FImageData[b], 7, 3));
          end;
          if (j = 0) and (k = 0) then begin
            //Bitmap.TransparentColor := (LNew.rgbtBlue * 256 + LNew.rgbtGreen) * 256 + LNew.rgbtRed;
          end;

          //LScan[k] := LNew;
          Rec.SetPixel( k, j, LNew);
          inc(b);
        end;
      end;
    finally
      Bitmap.Unmap( Rec);
    end;

    Add( Bitmap);
  end;
  FImageData.Clear;
end;
{$ELSE}
procedure TG2ImageList.ParseImageData( ImageCount : integer; Hex : boolean);
var Bitmap : TBitmap;
    i, j, k, b : integer;
    LNew : TAlphaColor;
    LScan : PAlphaColorArray;
begin
  if (FImageData.Count = 0) or (FBitmapWidth = 0) then
    exit;

  FBitmapHeight := FImageData.Count div FBitmapWidth div ImageCount;

  b := 0;
  for i := 0 to ImageCount - 1 do begin
    Bitmap := TBitmap.Create( FBitmapWidth,
                              FBitmapHeight);

    //Bitmap.Height := FImageData.Count div FBitmapWidth div ImageCount;
    //Bitmap.Width :=  FBitmapWidth;
    //Bitmap.Pixelformat := pf24bit;

    //Bitmap.TransparentMode := tmFixed;
    //Bitmap.Transparent := True;

    for j := 0 to Bitmap.Height - 1 do begin
      LScan := Bitmap.Scanline[j];
      for k := 0 to Bitmap.Width - 1 do begin
        if Hex then begin
          LNew      := $FF000000
                     + 256*256 *(16 * HexToByte( FImageData[b][1]) + 1 * HexToByte( FImageData[b][2]))
                     + 256 * (16 * HexToByte( FImageData[b][3]) + 1 * HexToByte( FImageData[b][4]))
                     + (16 * HexToByte( FImageData[b][5]) + 1 * HexToByte( FImageData[b][6]));
        end else begin
          LNew  := StrToInt(copy( FImageData[b], 1, 3))
                 + 256 * StrToInt(copy( FImageData[b], 4, 3))
                 + 256*256* StrToInt(copy( FImageData[b], 7, 3));
        end;
        if (j = 0) and (k = 0) then begin
          //Bitmap.TransparentColor := (LNew.rgbtBlue * 256 + LNew.rgbtGreen) * 256 + LNew.rgbtRed;
        end;

        LScan[k] := LNew;
        inc(b);
      end;
    end;

    Add( Bitmap);
  end;
  FImageData.Clear;
end;
{$ENDIF}


// ==== TG2FMXControl ===================================================

constructor TG2FMXControl.Create( AOwner: TComponent);
begin
  inherited;

  FModule := nil;
  FParameter := nil;

  FMouseInput := False;
  FZOrder := 1;
  FSelected := False;

  FValue := 0;
  FLowValue := 0;
  FHighValue := 0;

  HitTest := False;
end;

destructor TG2FMXControl.Destroy;
var Patch : TG2GraphPatch;
begin
  if assigned(FModule) then begin
    Patch := FModule.GetPatch;
    if assigned(Patch) and (Patch.SelectedControl = self) then
      FModule.GetPatch.SelectedControl := nil;
  end;

  SetParameter(nil);
  inherited;
end;

{function TG2FMXControl.GetRelToParentRect : TRect;
begin
  if assigned( FModule) then
    Result := AddRect( BoundsRect, FModule.BoundsRect)
  else
    Result := BoundsRect;
end;}

{function TG2FMXControl.GetScreenCoordsRect: TRect;
begin
  if assigned(FModule) then begin
    Result.Left := FModule.ClientToScreen(Point(Left, Top)).X;
    Result.Right := FModule.ClientToScreen(Point(Left + Width, Top + Height)).X;
    Result.Top := FModule.ClientToScreen(Point(Left, Top)).Y;
    Result.Bottom := FModule.ClientToScreen(Point(Left + Width, Top + Height)).Y;
  end else begin
    Result.Left := Left;
    Result.Right := Left + Width;
    Result.Top := Top;
    Result.Bottom := Top + Height;
  end;
end;}

procedure TG2FMXControl.SetModule( aValue: TG2FMXModule);
begin
  FModule := aValue;
  Parent := FModule;
end;

procedure TG2FMXControl.SetParameter( aParam : TG2FileParameter);
var Param : TG2GraphParameter;
begin
  if assigned(aParam) and not( aParam is TG2FileParameter) then
    raise Exception.Create('Parameter must be of type TG2GraphParameter.');

  if aParam <> FParameter then begin
    Param := aParam as TG2GraphParameter;

    if assigned(FParameter) then begin
      (FParameter as TG2GraphParameter).DeassignControl(self);
      if assigned( Param) then begin
        Param.AssignControl( self);
      end;
      FParameter := Param;
    end else begin
      FParameter := Param;
      if assigned(FParameter) then
        (FParameter as TG2GraphParameter).AssignControl( self);
    end;
    Repaint;
  end;
end;

function TG2FMXControl.GetGraphModule: TG2FMXModule;
begin
  Result := FModule;
end;

function TG2FMXControl.GetHighValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.HighValue
  else
    Result := FHighValue;
end;

function TG2FMXControl.GetLowValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.LowValue
  else
    Result := FLowValue;
end;

function TG2FMXControl.HasMorph: boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.HasMorph
  else
    Result := False;
end;

function TG2FMXControl.GetMorph: TMorphParameter;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorph
  else
    Result := nil;
end;

function TG2FMXControl.GetMorphValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorphValue
  else
    Result := 0;
end;

function TG2FMXControl.GetParamIndex: integer;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamIndex
  else
    Result := -1;
end;

function TG2FMXControl.GetParamLabel( aIndex : integer): AnsiString;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamLabel[ aIndex]
  else
    Result := '';
end;

function TG2FMXControl.GetParamLabelIndex: integer;
begin
  Result := 0;
end;

function TG2FMXControl.GetValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetParameterValue
  else
    Result := FValue;
end;

function TG2FMXControl.GetSelected: boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.Selected
  else
    Result := False;
end;

procedure TG2FMXControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  if ssLeft in Shift then begin
    FStartValue := Value;
    //Select;
  end;
  inherited;
end;

procedure TG2FMXControl.MouseMove(Shift: TShiftState; X, Y: single);
begin
  inherited;
end;

procedure TG2FMXControl.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: single);
begin
  inherited;
end;

procedure TG2FMXControl.ParsePanelData(fs: TModuleDefStream);
var aName, aValue : string;
begin
  while (fs.Position < fs.Size) and (aName <> '#>') do begin
    fs.ReadSpaces;
    aName := fs.ReadUntil([':', #13]);
    if aName <> '#>' then begin
      //aValue := ReadUntil(fs, [#13]);
      if not ParseProperties( fs, aName) then begin
        // Unknown property
        aValue := fs.ReadUntil([#13]);
      end;
    end;
  end;
end;

function TG2FMXControl.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : string;
    G2 : TG2File;
    temp : string;
begin
  Result := True;

  if aName = 'XPos' then begin
    aValue := fs.ReadUntil([#13]);
    //Left := FModule.Left + StrToInt( aValue);
    Position.X := StrToInt( aValue);
  end else

  if aName = 'YPos' then begin
    aValue := fs.ReadUntil([#13]);
    //Top  := FModule.Top + StrToInt( aValue);
    Position.Y := StrToInt( aValue);
  end else

  if aName = 'Width' then begin
    aValue := fs.ReadUntil([#13]);
    if aValue[1] = '"' then begin
      fs.Position := fs.Position - Length(aValue) - 1;
      Result := False;
    end else
      Width := StrToInt( aValue);
  end else

  if aName = 'Height' then begin
    aValue := fs.ReadUntil([#13]);
    Height := StrToInt( aValue);
  end else

  if aName = 'ZPos' then begin
    aValue := fs.ReadUntil([#13]);
    FZOrder := StrToInt( aValue);
  end else

  if aName = 'InfoFunc' then begin
    aValue := fs.ReadUntil( [#13]);
    if assigned(FParameter) then
      FParameter.InfoFunctionIndex := StrToInt( string(aValue));

    if assigned(FParameter) and assigned(FParameter.Patch)
       and assigned(FParameter.Patch.G2) then begin
      G2 := FParameter.Patch.G2;
      temp := FParameter.InfoFunction( FParameter.InfoFunctionIndex);
      if pos('?', temp)>0 then
         G2.add_log_line( FParameter.ModuleName + ' ' + FParameter.ParamName + ' ' + IntToStr(FParameter.InfoFunctionIndex) + ' ' + temp, LOGCMD_NUL);
    end;
 end else

    Result := False
end;

function TG2FMXControl.CheckValueBounds( aValue: integer): byte;
begin
  if assigned( FParameter) then begin
    if aValue > FParameter.HighValue then
      Result := FParameter.HighValue
    else
      if aValue < FParameter.LowValue then
        Result := FParameter.LowValue
      else
        Result := aValue;
  end else begin
    if aValue > FHighValue then
      Result := FHighValue
    else
      if aValue < FLowValue then
        Result := FLowValue
      else
        Result := aValue;
  end;
end;

procedure TG2FMXControl.SetParamLabel( aIndex : integer; aValue: AnsiString);
begin
  if assigned( FParameter) then
    FParameter.ParamLabel[ aIndex] := aValue
  else begin
    Repaint;
  end;
end;

procedure TG2FMXControl.SetValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.SetParameterValue( aValue)
  else begin
    FValue := aValue;
    Repaint;
    if assigned( FOnChange) then
      FOnChange( self);
  end;
end;

procedure TG2FMXControl.SetLowValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.LowValue := aValue
  else begin
    FLowValue := aValue;
  end;
  Repaint;

end;

procedure TG2FMXControl.SetHighValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.HighValue := aValue
  else begin
    FHighValue := aValue;
  end;
  Repaint;
end;

procedure TG2FMXControl.InitValue( aValue: integer);
var Rect : TRect;
begin
  if (aValue >= FLowValue) and (aValue <= FHighValue) and ( aValue <> FValue) then begin
    FValue := aValue;
    //Rect := BoundsRect;
    //InvalidateRect( Parent.Handle, @Rect, True);
    Repaint;
  end;
end;

procedure TG2FMXControl.SetMorphValue( aValue: byte);
var MorphParameter : TMorphParameter;
    TempValue : integer;
begin
  if assigned( FParameter) then begin
    FParameter.SetSelectedMorphValue( aValue);
    Repaint;
  end;
end;

procedure TG2FMXControl.Select;
begin
  FSelected := True;
  Repaint;
end;

procedure TG2FMXControl.Deselect;
begin
  FSelected := False;
  Repaint;
end;

// ==== TG2GraphParameter ======================================================

constructor TG2GraphParameter.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
begin
  inherited Create( aPatch, aLocation, aModuleIndex, aModule);

  SetLength(FControlList, 0);
end;

destructor TG2GraphParameter.Destroy;
var i : integer;
begin
  for i := 0 to Length(FControlList) - 1 do
    FControlList[i].FParameter := nil;
  Finalize( FControlList);
  inherited;
end;

procedure TG2GraphParameter.AssignControl(aControl: TG2FMXControl);
var i : integer;
begin
  if not(aControl is TG2FMXControl) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl as TG2FMXControl;
  end;
end;

procedure TG2GraphParameter.DeassignControl(aControl: TG2FMXControl);
var i, j : integer;
begin
  if not(aControl is TG2FMXControl) then
    raise Exception.Create('Only a TG2GraphControlFMX can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if (i < Length(FControlList)) then begin
    FControlList[i].FParameter := nil;
    for j := i + 1 to Length(FControlList) - 1 do begin
      FControlList[j-1] := FControlList[j];
    end;
    SetLength(FControlList, Length(FControlList) - 1);
  end;
end;

procedure TG2GraphParameter.InvalidateControl;
var i : integer;
begin
  // Update all controls attached to the parameter
  for i := 0 to Length(FControlList) - 1 do begin
    FControlList[i].Repaint;
  end;
end;


// ==== TG2FMXLabel ==========================================================

constructor TG2FMXLabel.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;
  HitTest := False;

  Width := 30;
  Height := 10;
end;

destructor TG2FMXLabel.Destroy;
begin
  inherited;
end;

procedure TG2FMXLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Single);
begin
  inherited;
  if assigned(FOnClick) then FOnClick( self);
end;

procedure TG2FMXLabel.SetCaption( avalue: string);
begin
  FCaption := aValue;
  Repaint;
end;

procedure TG2FMXLabel.Paint;
var Rect : TRect;
begin
  //Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  //if assigned(FModule) then
  //  ExtCanvas.Brush.Color := FModule.Color;

  Canvas.Font.Size := FFontSize;
  Canvas.Fill.Color := claBlack;
  Canvas.Font.Style := [];
  Canvas.Fill.Kind := TBrushKind.bkSolid;

  Canvas.FillText( BoundsRect, FCaption, False, AbsoluteOpacity, [], TTextAlign.taLeading);

  {ExtCanvas.TextRect(
          Rect,
          Rect.Left + 1,
          Rect.Top +( Rect.Bottom - Rect.Top - ExtCanvas.TextHeight(FCaption)) div 2,
          FCaption);}
end;

function TG2FMXLabel.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'FontSize' then begin
      aValue := fs.ReadUntil( [#13]);
      FFontSize := StrToInt(string(aValue))-2
    end else

    if aName = 'Text' then begin
      aValue := fs.ReadUntil( [#13]);
      Caption := string(fs.UnQuote(aValue));
    end else

      Result := False
  end;
end;

// ==== TG2FMXDisplay ==========================================================

constructor TG2FMXDisplay.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;
  HitTest := True;

  {Font.Name := 'Arial';
  Font.Size := 8;
  Font.Style := [fsBold];
  Font.Color := clWhite;}

  FLine1 := '';
  FLine2 := '';
  FLine3 := '';
  FLine4 := '';
  FLineCount := 1;

  //Color := CL_DISPLAY_BACKGRND;

  Width := 38;
  Height := 13;

  FDisplayType := 0;
  //FTextFunction := 0;
  FMasterRef := -1;
end;

destructor TG2FMXDisplay.Destroy;
begin
  inherited;
end;

function TG2FMXDisplay.GetLine(LineNo: integer): AnsiString;
begin
  if assigned(FParameter) then
    Result := IntToStr(FParameter.GetParameterValue) // TODO Implement textfunction
  else
    case LineNo of
    0 : Result := FLine1;
    1 : Result := FLine2;
    2 : Result := FLine3;
    3 : Result := FLine4;
    else
      Result := '';
    end;
end;

procedure TG2FMXDisplay.SetLine(LineNo: integer; aValue: AnsiString);
begin
  case LineNo of
  0 : FLine1 := aValue;
  1 : FLine2 := aValue;
  2 : FLine3 := aValue;
  3 : FLine4 := aValue;
  end;
  Repaint;
end;

procedure TG2FMXDisplay.SetLineCount(aValue: integer);
begin
  if (aValue > 0) and (aValue <=4) then
    FLineCount := aValue
  else
    raise Exception.Create('Linecount must be between 1 and 4.');
end;

procedure TG2FMXDisplay.SetParameter(aParam: TG2FileParameter);
begin
  inherited;

  if assigned(aParam) then
    FTextFunction := aParam.TextFunctionIndex;
end;

procedure TG2FMXDisplay.SetTextFunction( aValue: integer);
begin
  FTextFunction := aValue;
  Repaint;
end;

procedure TG2FMXDisplay.Paint;
var Rect, LineRect : TRectF;
    i : integer;
    LineHeight : single;
    //BitMap : TBitmap;
begin
  if not Visible then
    exit;

  Rect := BoundsRect;

  if FLineCount > 0 then
    LineHeight := (BoundsRect.Bottom - BoundsRect.Top) / FLineCount
  else
    LineHeight := (BoundsRect.Bottom - BoundsRect.Top);

  {Bitmap := TBitmap.Create;
  try
    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Rect.Right - Rect.Left + 1;
    Bitmap.Height := Rect.Bottom - Rect.Top + 1;
    Bitmap.canvas.CopyRect(BoundsRect, ExtCanvas, Rect);

    Bitmap.Canvas.Font.Assign( Font);
    Bitmap.Canvas.Brush.Color := Color;}

    Canvas.Fill.Color := claGray;
    Canvas.Fill.Kind := TBrushKind.bkSolid;

    Canvas.Font.Family := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [TFontStyle.fsBold];

    LineRect.Left := 0;
    LineRect.Right := Width + 1;
    LineRect.Top := 0;
    LineRect.Bottom := LineRect.Top + LineHeight + 1;

    for i := 0 to FLineCount - 1 do begin
      Canvas.FillRect( LineRect, 0, 0, allCorners, AbsoluteOpacity);

      Canvas.Fill.Color := claWhite;
      if assigned(FParameter) then begin
        case FDisplayType of
        0 : Canvas.FillText( LineRect, FParameter.TextFunction, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
        1 : case i of
            0 : Canvas.FillText( LineRect, FParameter.ParamName, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
            1 : Canvas.FillText( LineRect, FParameter.TextFunction, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
            end;
        2 : case i of
            0 : Canvas.FillText( LineRect, FParameter.ParamName, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
            1 : Canvas.FillText( LineRect, IntToStr(FParameter.GetParameterValue), False, AbsoluteOpacity, [] , TTextAlign.taCenter);
            end;
        3 : Canvas.FillText( LineRect, FParameter.ParamName, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
        4 : Canvas.FillText( LineRect, IntToStr(FParameter.Patch.Slot.SlotIndex + 1) + ':' + string(FParameter.ModuleName), False, AbsoluteOpacity, [] , TTextAlign.taCenter);
        5 : if assigned(FParameter.Module) then Canvas.FillText( LineRect, FParameter.Module.ModuleName, False, AbsoluteOpacity, [] , TTextAlign.taCenter);
        end;
      end else
        Canvas.FillText( LineRect, Line[i], False, AbsoluteOpacity, [] , TTextAlign.taCenter);
      LineRect.Top := LineRect.Top + LineHeight;
      LineRect.Bottom := LineRect.Bottom + LineHeight + 1;
    end;

  {  ExtCanvas.Draw( Rect.Left, Rect.Top, Bitmap);
  finally
    Bitmap.Free;
  end;}
end;

function TG2FMXDisplay.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
    sl : TStringList;
    i, value, c : integer;
    G2 : TG2File;
    temp : string;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'MasterRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FMasterRef := StrToInt(string(aValue));
      FParameter := FModule.FData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameter;
      (FParameter as TG2GraphParameter).AssignControl(self);

    end else

    if aName = 'Text Func' then begin
      aValue := fs.ReadUntil( [#13]);
      FTextFunction := StrToInt(string(aValue));

    end else

    if aName = 'Dependencies' then begin
      fs.ReadConst( '"');
      sl := TStringList.Create;
      try
        fs.ReadOptions( sl, [','], ['"']);
        if sl.Count > 0 then
          if FMasterRef > -1 then begin
            //if FMasterRef < sl.Count then begin

              // Find ref to master parameter
              i := 0;
              while (i<sl.Count) and (sl[i]<>IntToStr(FMasterRef)) and (sl[i]<>'s'+IntToStr(FMasterRef)) do
                inc(i);

              if (i<sl.Count) then begin
                // Assign master parameter first
                if (Lowercase(sl[ i][1]) = 's') then begin // One of the static params
                  FParameter := FModule.FData.Mode[ FMasterRef] as TG2GraphParameter;
                  FParameter.TextFunctionIndex := FTextFunction;
                  (FParameter as TG2GraphParameter).AssignControl(self);
                end else begin
                  FParameter := FModule.FData.Parameter[ FMasterRef] as TG2GraphParameter;
                  FParameter.TextFunctionIndex := FTextFunction;
                  (FParameter as TG2GraphParameter).AssignControl(self);
                end;
              end else begin
                FParameter := FModule.FData.Parameter[ FMasterRef] as TG2GraphParameter;
                FParameter.TextFunctionIndex := FTextFunction;
                (FParameter as TG2GraphParameter).AssignControl(self);
              end;

              if assigned(FParameter) then begin

                for i := 0 to sl.Count - 1 do begin
                  if (length(sl[i])>0) and (Lowercase(sl[i][1]) = 's') then begin
                    val(copy(sl[i], 2, Length(sl[i]) - 1), value, c);
                    if c = 0 then begin
                      FParameter.AddTextDependency(ptMode, value);
                      (FModule.FData.Mode[ value] as TG2GraphParameter).AssignControl(self);
                    end;
                  end else begin
                    val(sl[i], value, c);
                    if c = 0 then begin
                      FParameter.AddTextDependency(ptParam, value);
                      (FModule.FData.Parameter[ value] as TG2GraphParameter).AssignControl(self);
                    end;
                  end;
                end;
              end;
          end else
            raise Exception.Create('Master param not assigned yet...');

      finally
        sl.Free;
      end;

      if assigned(FParameter) and assigned(FParameter.Patch)
         and assigned(FParameter.Patch.G2) then begin
        G2 := FParameter.Patch.G2;
        temp := FParameter.TextFunction;
        if pos('*', temp)>0 then
           G2.add_log_line( FParameter.ModuleName + ' ' + FParameter.ParamName + ' Textfunc ' + IntToStr(FParameter.TextFunctionIndex) + ' ' + temp + ' InfoFunc ' + IntToStr(FParameter.InfoFunctionIndex), LOGCMD_NUL);
      end;

    end else

      Result := False
  end;
end;

// ==== TG2FMXConnector ========================================================

constructor TG2FMXConnector.Create(AOwner: TComponent);
begin
  inherited;
  FFillOuter := TBrush.Create(TBrushKind.bkGradient, $FFBABABA);
  FFillOuter.Gradient.Color := $FFBABABA;
  FFillOuter.Gradient.Color1 := $FFFF0000;
  //FFillOuter.Gradient.Style := FMX.Types.TGradientStyle.gsRadial;
  FFillInner := TBrush.Create(TBrushKind.bkSolid, $FF550000);

  HitTest := True;

  Width := 13;
  Height := 13;
end;

destructor TG2FMXConnector.Destroy;
begin
  FFillInner.Free;
  FFillOuter.Free;
  inherited;
end;

procedure TG2FMXConnector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if (ssLeft in Shift) and assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;

      PatchArea.FFromConnector := self;

      PatchArea.FNewCable := TG2FMXCable.Create(PatchArea);
      PatchArea.FNewCable.Parent := PatchArea;

      PatchArea.FNewCable.Point1X := Module.Position.X + Position.X + Width / 2;
      PatchArea.FNewCable.Point1Y := Module.Position.Y + Position.Y + Width / 2;
      PatchArea.FNewCable.Point2X := PatchArea.FNewCable.Point1X;
      PatchArea.FNewCable.Point2Y := PatchArea.FNewCable.Point1Y;
      PatchArea.FNewCable.InitCable;
    end;
  end;
  inherited;
end;

procedure TG2FMXConnector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;

      if PatchArea.FFromConnector = self then begin
        FreeAndNil(PatchArea.FNewCable)
      end else begin
        FModule.FData.PatchPart.Patch.MessAddConnection( FModule.Location, PatchArea.FFromConnector.Data, Data);
        FreeAndNil(PatchArea.FNewCable);
      end;
    end;
  end;
  inherited;
end;

procedure TG2FMXConnector.MouseMove(Shift: TShiftState; X, Y: Single);
var PatchArea : TG2FMXPatchArea;
    Module : TG2FMXModule;
begin
  if (ssLeft in Shift) and assigned(FParent) then begin
    Module := FParent as TG2FMXModule;
    if assigned(Module.Parent) then begin
      PatchArea := Module.Parent as TG2FMXPatchArea;
      PatchArea.MouseMove(Shift, Module.Position.X + Position.X + X, Module.Position.Y + Position.Y + Y);
    end;
  end;
  inherited;
end;


procedure TG2FMXConnector.Paint;
var
  wSize, eSize: Single;
  rInner, rOuter : TRectF;
begin
  if Width < Height then begin
    wSize := Width / 2;
    rOuter.Left := 0;
    rOuter.Right := Width;
    rOuter.Top := Height / 2 - Width / 2;
    rOuter.Bottom := Height / 2 + Width / 2;
  end else begin
    wSize := Height / 2;
    rOuter.Left := Width / 2 - Height / 2;
    rOuter.Right := Width / 2 + Height / 2;
    rOuter.Top := 0;
    rOuter.Bottom := Height;
  end;
  eSize := wSize / 2.4;
  rInner.Left := rOuter.Left + eSize;
  rInner.Top := rOuter.Top + eSize;
  rInner.Width := rOuter.Width - eSize * 2;
  rInner.Height := rOuter.Height - eSize * 2;

  Canvas.Fill.Assign(FFillOuter);
  Canvas.FillEllipse(rOuter, Opacity);
  Canvas.Fill.Assign(FFillInner);
  Canvas.FillEllipse(rInner, Opacity);
end;

procedure TG2FMXConnector.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  FData.CalcDefColor;
end;

function TG2FMXConnector.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : string;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil([#13]);
      FData.ConnectorIndex := StrToInt(aValue);
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil([#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil([#13]);
      if aValue = '"Audio"' then
        FData.ConnectorType := ctAudio
      else
        if aValue = '"Logic"' then
          FData.ConnectorType := ctLogic
        else
          if aValue = '"Control"' then
            FData.ConnectorType := ctControl
          else
            raise Exception.Create('Unknown control type ' + aValue);
    end else

    if aName = 'Bandwidth' then begin
      aValue := fs.ReadUntil([#13]);
      if aValue = '"Static"' then
        FData.Bandwidth := btStatic
      else
        if aValue = '"Dynamic"' then
          FData.BandWidth := btDynamic
        else
          raise Exception.Create('Unknown bandwidth type ' + aValue);
    end else

      Result := False
  end;
end;

procedure TG2FMXConnector.SetData(aConnectorData: TG2FileConnector);
begin
  FData := aConnectorData;
  FData.GraphControl := self;
end;

// ==== TButtonBevel ===========================================================

constructor TButtonBevel.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FPointsUpperLeft, 6);
  SetLength(FPointsLowerRight, 6);
  FDarkSideColor := claGray;
  FLightSideColor := claLightGray;
  FInversed := False;

  FillDark := TBrush.Create(TBrushKind.bkSolid, claGray);
  FillLight := TBrush.Create(TBrushKind.bkSolid, claLightGray);
end;

destructor TButtonBevel.Destroy;
begin
  FillLight.Free;
  FillDark.Free;

  Finalize(FPointsUpperLeft);
  Finalize(FPointsLowerRight);
  inherited;
end;

procedure TButtonBevel.Paint;
var
  R, RInner: TRectF;
begin
  inherited;

  R := GetShapeRect;
  RInner.Left := R.Left + FBevelThickness;
  RInner.Right := R.Right + FBevelThickness;
  RInner.Top := R.Top + FBevelThickness;
  RInner.Bottom := R.Bottom + FBevelThickness;


  //if Canvas.BeginScene then
  //  try

      {canvas.Fill.Color := claDarkGray;}
      canvas.Fill.Kind := TBrushKind.bkSolid;

      {Canvas.FillRect( RInner, 0, 0, AllCorners, AbsoluteOpacity);}

      if Inversed then begin
        canvas.Fill.Color := claRed;//FDarkSideColor
        canvas.Stroke.Color := claRed;
      end else begin
        canvas.Fill.Color := FLightSideColor;
      end;

      FPointsUpperLeft[0] := PointF(R.Left, R.Top);
      FPointsUpperLeft[1] := PointF(R.Right, R.Top);
      FPointsUpperLeft[2] := PointF(R.Right, R.Bottom);
      FPointsUpperLeft[3] := PointF(R.Right - FBevelThickness, R.Bottom - FBevelThickness);
      FPointsUpperLeft[4] := PointF(R.Right - FBevelThickness, R.Top + FBevelThickness);
      FPointsUpperLeft[5] := PointF(R.Left + FBevelThickness, R.Top + FBevelThickness);

      Canvas.FillPolygon(FPointsUpperLeft, 1);
      //Canvas.DrawPolygon(FPoints, AbsoluteOpacity);

      if Inversed then begin
        canvas.Fill.Color := claBlue;//FDarkSideColor
        canvas.Stroke.Color := claBlue;
      end else
        canvas.Fill.Color := FDarkSideColor;

      FPointsLowerRight[0] := PointF(R.Left, R.Top);
      FPointsLowerRight[1] := PointF(R.Left + FBevelThickness, R.Top + FBevelThickness);
      FPointsLowerRight[2] := PointF(R.Left + FBevelThickness, R.Bottom - FBevelThickness);
      FPointsLowerRight[3] := PointF(R.Right - FBevelThickness, R.Bottom - FBevelThickness);
      FPointsLowerRight[4] := PointF(R.Right, R.Bottom);
      FPointsLowerRight[5] := PointF(R.Left, R.Bottom);

      Canvas.FillPolygon(FPointsLowerRight, 1);
    //finally
    //  Canvas.EndScene;
    //end;
end;

procedure TButtonBevel.SetBevelThickness(aValue: single);
begin
  FBevelThickness := aValue;
  Repaint;
end;

procedure TButtonBevel.SetDarkSideColor( aValue : TAlphaColor);
begin
  FDarkSideColor := aValue;
  Repaint;
end;

procedure TButtonBevel.SetInversed(aValue: boolean);
begin
  FInversed := aValue;
  Repaint;
end;

procedure TButtonBevel.SetLightSideColor( aValue : TAlphaColor);
begin
  FLightSideColor := aValue;
  Repaint;
end;

// ==== TG2FMXButton ===========================================================

constructor TG2FMXButton.Create(AOwner: TComponent);
begin
  inherited;

  FButtonText := TStringList.Create;
  FImageList := TG2ImageList.Create( True);

  FMouseInput := True;

  Width := 13;
  Height := 12;

  HitTest := True;

  FButtonWidth := 0;
  FButtonHeight := 0;
  FButtonCount := 0;
  FOrientation := otHorizontal;
  FImageWidth := 0;
  FImageCount := 0;

  FHighLightColor := claTurquoise;
  FColor := claDarkGray;
  FDarkSideColor := claGray;
  FLightSideColor := claLightGray;

  FCaption := '';
  FIcon := itNone;

  FPressed := False;

  SetLength(FPointsUpperLeft, 6);
  SetLength(FPointsLowerRight, 6);
end;

destructor TG2FMXButton.Destroy;
begin
  FImageList.Free;
  FButtonText.Free;

  Finalize(FPointsUpperLeft);
  Finalize(FPointsLowerRight);
  inherited;
end;

procedure TG2FMXButton.MouseDown( Button: TMouseButton; Shift: TShiftState; X,  Y: Single);
begin
 if (ssLeft in Shift) then begin
    FPressed := True;
    Repaint;
  end;
  inherited;
end;

procedure TG2FMXButton.MouseUp( Button: TMouseButton; Shift: TShiftState; X,  Y: Single);
begin
  if FPressed then begin
    FPressed := False;
    Repaint;
    if assigned(FOnClick) then FOnClick( self);
  end;
  inherited;
end;

procedure TG2FMXButton.SetCaption( aValue: string);
begin
  FCaption := aValue;
end;

procedure TG2FMXButton.SetColor(aValue: TAlphaColor);
begin
  FColor := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetDarkSideColor(aValue: TAlphaColor);
begin
  FDarkSideColor := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetOrientation( aValue : TOrientationType);
begin
  // Abstract
end;

procedure TG2FMXButton.SetHighLightColor( aValue: TAlphaColor);
begin
  FHighlightColor := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetBevelVisible( aValue: boolean);
begin
  FBevelVisible := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetBorderColor( aValue: TAlphaColor);
begin
  FBorderColor := aValue;
end;

procedure TG2FMXButton.SetIcon( aValue: TIconType);
begin
  FIcon := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetLightSideColor(aValue: TAlphaColor);
begin
  FLightSideColor := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetButtonCount( aValue: integer);
begin
  FButtonCount := aValue;
  Repaint;
end;

procedure TG2FMXButton.SetButtonText( aValue: TStrings);
begin
  FButtonText.Assign( aValue);
  Repaint;
end;

procedure TG2FMXButton.Paint;
var R : TRectF;
begin
  inherited;

  canvas.Fill.Color := FColor;
  canvas.Fill.Kind := TBrushKind.bkSolid;

  canvas.FillRect( BoundsRect, 0, 0, allCorners, 1);

  R := self.GetClipRect;

  if FBevelVisible and (not FPressed) then begin
    DrawBevel;
  end;

  Canvas.Fill.Color := claBlack;
  Canvas.Font.Family := 'Arial';
  Canvas.Font.Size := 8;
  Canvas.Font.Style := [];
  Canvas.FillText( R, FCaption, False, AbsoluteOpacity, [], TTextAlign.taCenter)
end;

procedure TG2FMXButton.DrawBevel;
var R : TRectF;
    FBevelThickness : single;
begin
  FBevelThickness := 1;

  R := BoundsRect;

  Canvas.Fill.Color := FLightSideColor;

  FPointsUpperLeft[0] := PointF(R.Left, R.Top);
  FPointsUpperLeft[1] := PointF(R.Right, R.Top);
  FPointsUpperLeft[2] := PointF(R.Right, R.Bottom);
  FPointsUpperLeft[3] := PointF(R.Right - FBevelThickness, R.Bottom - FBevelThickness);
  FPointsUpperLeft[4] := PointF(R.Right - FBevelThickness, R.Top + FBevelThickness);
  FPointsUpperLeft[5] := PointF(R.Left + FBevelThickness, R.Top + FBevelThickness);

  Canvas.FillPolygon(FPointsUpperLeft, AbsoluteOpacity);

  canvas.Fill.Color := FDarkSideColor;

  FPointsLowerRight[0] := PointF(R.Left, R.Top);
  FPointsLowerRight[1] := PointF(R.Left + FBevelThickness, R.Top + FBevelThickness);
  FPointsLowerRight[2] := PointF(R.Left + FBevelThickness, R.Bottom - FBevelThickness);
  FPointsLowerRight[3] := PointF(R.Right - FBevelThickness, R.Bottom - FBevelThickness);
  FPointsLowerRight[4] := PointF(R.Right, R.Bottom);
  FPointsLowerRight[5] := PointF(R.Left, R.Bottom);

  Canvas.FillPolygon(FPointsLowerRight, 1);
end;


function TG2FMXButton.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Text' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FButtonText, [','], ['"']);
    end else

    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FImageList.FImageData, [':'], ['"']);
    end else

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FParameter := FModule.FData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameter;
      (FParameter as TG2GraphParameter).AssignControl(self);
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

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
      FButtonWidth := StrToInt(string(aValue));
    end else

    {if aName = 'Type' then begin
      aValue := ReadUntil(fs, [#13]);
      //
    end else}

    if aName = 'Style' then begin
      aValue := fs.ReadUntil( [#13]);
      //
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

// ==== TG2FMXButtonText =======================================================

constructor TG2FMXButtonText.Create(AOwner: TComponent);
begin
  inherited;

  //FButtonTextType := bttNormal;
  FButtonTextType := bttPush;
end;

destructor TG2FMXButtonText.Destroy;
begin
  inherited;
end;

procedure TG2FMXButtonText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if  ( ssLeft in Shift) then begin
    if FButtonTextType = bttPush then
      Value := 1
    else
      Value := 1 - Value;
  end;
  inherited;
end;

procedure TG2FMXButtonText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if FButtonTextType = bttPush then
    Value := 0;
  inherited;
end;

procedure TG2FMXButtonText.Paint;
var LabelText : string;
begin
  if Value = 0 then
    Canvas.Fill.Color := FColor
  else
    Canvas.Fill.Color := FHighLightColor;
  canvas.Fill.Kind := TBrushKind.bkSolid;

  canvas.FillRect( BoundsRect, 0, 0, allCorners, AbsoluteOpacity);

  if FImageList.Count > 0 then begin
    //Canvas.DrawBitmap( FImageList.Items[0], RectF(0,0,FImageList.Items[0].Width, FImageList.Items[0].Height)
    //                                 ,BoundsRect, AbsoluteOpacity)
    Canvas.Fill.Color := claBlack;
    DrawImage( FParameter.ParamID, Value, Canvas, FImageList.Items[0],
               RectF(0,0,FImageList.Items[0].Width, FImageList.Items[0].Height),
               BoundsRect, 2, 2, AbsoluteOpacity);
  end else begin
    LabelText := '';
    if assigned(Parameter) then
      LabelText := Parameter.SelectedButtonText
    else
      if (FButtonText.Count > 0) then begin
        LabelText := string(ParamLabel[0]);
        if LabelText = '' then
          if  Value < FButtonText.Count then
            LabelText := FButtonText[ Value];
      end;
    Canvas.Fill.Color := claBlack;
    Canvas.Font.Family := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.FillText( BoundsRect, LabelText, False, AbsoluteOpacity, [], TTextAlign.taCenter)
  end;

  {if Selected then
    Canvas.Brush.Color := CL_SELECTED_PARAM
  else
    Canvas.Brush.Color := Color;}

  if Value = 0 then
    DrawBevel;
end;

procedure TG2FMXButtonText.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  if FImageList.FImageData.Count > 0 then begin
    FImageList.BitmapWidth := FImageWidth;
    FImageList.ParseImageData( 1, True)
  end;
end;

function TG2FMXButtonText.ParseProperties(fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);

      if aValue = '"Push"' then
        FButtonTextType := bttPush
      else
        if aValue = '"Check"' then
          FButtonTextType := bttCheck;

    end else
      Result := False

  end;
end;

procedure TG2FMXButtonText.SetButtonTextType( aValue: TButtonTextType);
begin
  FButtonTextType := aValue;
  Repaint;
end;

// ==== TG2GraphTextEdit =======================================================

constructor TG2FMXTextEdit.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2FMXTextEdit.Destroy;
begin
  inherited;
end;

// ==== TG2GraphButtonFlat =====================================================

constructor TG2FMXButtonFlat.Create(AOwner: TComponent);
begin
  inherited;

  FHighLightColor := claTurquoise;
  FColor := claDarkGray;
  FDarkSideColor := claGray;
  FLightSideColor := claGray;
end;

destructor TG2FMXButtonFlat.Destroy;
begin
  inherited;
end;

procedure TG2FMXButtonFlat.Paint;
var R : TRectF;
    LabelText : string;
begin
  Canvas.Fill.Color := FColor;
  Canvas.Fill.Kind := TBrushKind.bkSolid;

  Canvas.FillRect( BoundsRect, 0, 0, allCorners, 1);

  if (FImageList.Count > 0) and (Value < FImageList.Count) then begin
    //Canvas.DrawBitmap( FImageList.Items[Value], FImageList.BoundsRect, BoundsRect, AbsoluteOpacity);
    Canvas.Fill.Color := claBlack;
    DrawImage( FParameter.ParamID, Value, Canvas, FImageList.Items[Value],
               RectF(0,0,FImageList.Items[Value].Width, FImageList.Items[Value].Height),
               BoundsRect, 2, 2, AbsoluteOpacity);
  end else begin
    LabelText := '';
    if assigned(Parameter) then
      LabelText := Parameter.SelectedButtonText
    else
      if Value < FButtonText.Count then
        LabelText := FButtonText[Value];

    if assigned(Parameter) and (LabelText = '') then
      LabelText := Parameter.InfoFunction( Parameter.InfoFunctionIndex);

    Canvas.Fill.Color := claBlack;
    Canvas.Font.Family := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.FillText( BoundsRect, LabelText, False, AbsoluteOpacity, [], TTextAlign.taCenter)
  end;

  DrawBevel;

  {if MidiAware and ShowMidiBox then begin
    if assigned(FMidiEditorAssignmentList) then
      MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
    else
      MidiEditorAssignment := nil;
    DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
  end;}
end;

procedure TG2FMXButtonFlat.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  if assigned(FParameter) then
    if FImageCount > 0 then begin
      FImageList.BitmapWidth := FImageWidth;
      FImageList.ParseImageData( FImageCount, True)
    end;
end;

procedure TG2FMXButtonFlat.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ( ssLeft in Shift) then begin
    if Value + 1 > HighValue then
      Value := LowValue
    else
      Value := Value + 1;
  end;
  inherited;
end;

// ==== TG2FMXLevelShift =======================================================

constructor TG2FMXLevelShift.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2FMXLevelShift.Destroy;
begin
  inherited;
end;

procedure TG2FMXLevelShift.Paint;
var Rect, RectTop, RectBottom : TRectF;
    HorzCenter, VertCenter: single;
    P1, P2 : TPointF;
    //MidiEditorAssignment : TMidiEditorAssignment;

procedure DrawArrowHead( aPoint : TPointF; aSize : single; aOrientation : integer; aCanvas : TCanvas);
var pts : TPolygon;
begin
  SetLength( pts, 3);
  case aOrientation of
  0 : begin // Up
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y + aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  1 : begin // Down
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y - aSize;
      end;
  2 : begin // Left
        pts[0] := aPoint;
        pts[1].X := aPoint.X + aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  3 : begin // Right
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X - aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  end;

  aCanvas.FillPolygon(pts, AbsoluteOpacity);

  Finalize(pts);
end;

begin
  inherited;

  Rect := BoundsRect;

  Canvas.Fill.Color := claLightGray;
  Canvas.FillRect(Rect, 0, 0, allCorners, AbsoluteOpacity);

  HorzCenter := (Rect.Right + Rect.Left) / 2;
  VertCenter := (Rect.Top + Rect.Bottom) / 2;

  RectTop.Top  := Rect.Top;
  RectTop.Left := HorzCenter - 4;
  RectTop.Right := HorzCenter + 5;
  RectTop.Bottom := VertCenter;

  RectBottom.Top  := VertCenter;
  RectBottom.Left := HorzCenter - 4;
  RectBottom.Right := HorzCenter + 5;
  RectBottom.Bottom := Rect.Bottom;

  Canvas.Stroke.Color := claBlack;
  case Value of
  0 : begin
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := CL_BTN_FACE;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectTop.Top + 1;
        P2.X := HorzCenter; P2.Y := RectTop.Bottom;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 0, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  1 : begin
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := CL_BTN_FACE;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectTop.Bottom - 1;
        P2.X := HorzCenter; P2.Y := RectTop.Top;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 1, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  2 : begin
        Canvas.Fill.Color := claLightGray;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectBottom.Top;
        P2.X := HorzCenter; P2.Y := RectBottom.Bottom - 1;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 0, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  3 : begin
        Canvas.Fill.Color := claLightGray;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectBottom.Bottom - 2;
        P2.X := HorzCenter; P2.Y := RectBottom.Top - 1;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 1, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  4 : begin
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectTop.Top + 1;
        P2.X := HorzCenter; P2.Y := RectBottom.Bottom - 2;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 0, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
        Canvas.Stroke.Color := claLightGray;
        P1.X := Rect.Left; P1.Y := VertCenter;
        P2.X := Rect.Right; P2.Y := VertCenter;
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  5 : begin
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectTop, 0, 0, allCorners, AbsoluteOpacity);
        Canvas.Fill.Color := claLime;
        Canvas.FillRect(RectBottom, 0, 0, allCorners, AbsoluteOpacity);
        P1.X := HorzCenter; P1.Y := RectBottom.Bottom - 2;
        P2.X := HorzCenter; P2.Y := RectTop.Top + 1;
        Canvas.Fill.Color := claBlack;
        DrawArrowHead( P1, 2, 1, Canvas);
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
        Canvas.Stroke.Color := claLightGray;
        P1.X := Rect.Left; P1.Y := VertCenter - 1;
        P2.X := Rect.Right; P2.Y := VertCenter - 1;
        Canvas.DrawLine(P1, P2, AbsoluteOpacity);
      end;
  end;

  if Selected then begin
    FDarkSideColor := claWhite;
    FLightSideColor := claWhite;
  end else begin
    FDarkSideColor := claGray;
    FLightSideColor := claGray;
  end;

  DrawBevel;

  {if MidiAware and ShowMidiBox then begin
    if assigned(FMidiEditorAssignmentList) then
      MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
    else
      MidiEditorAssignment := nil;
    DrawMidiAwareBox( Canvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
  end;}
end;

// ==== TG2FMXButtonRadio ======================================================

constructor TG2FMXButtonRadio.Create(AOwner: TComponent);
begin
  inherited;
  FHighlightColor := claTurquoise;
  Color := claLightGray;
  FUpsideDown := False;
  Height := 14;
  //IndexedControl := True;
end;

destructor TG2FMXButtonRadio.Destroy;
begin
  inherited;
end;

procedure TG2FMXButtonRadio.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  if ssLeft in Shift then begin
    if FOrientation = otHorizontal then begin
      if (FButtonCount > 0) and (Width > 0) then begin
        Value := trunc(X * FButtonCount / Width);
      end;
    end else begin
      if (FButtonCount > 0) and (Height > 0) then begin
        if FUpsideDown then
          Value := trunc((Height - Y) * FButtonText.Count / Height)
        else
          Value := trunc(Y * FButtonText.Count / Height);
      end;
    end;
  end;
  inherited;
end;

procedure TG2FMXButtonRadio.Paint;
var Rect : TRectF;
    //Bitmap : TBitmap;
    i : integer;
    LabelText : string;
    //MidiEditorAssignment : TMidiEditorAssignment;
begin
  {Bitmap := TBitmap.Create;
  try

    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.Pen.Color := FBorderColor;
    Bitmap.Canvas.Font.Assign( Font);}

    Canvas.Font.Family := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];

    if FButtonCount > 0 then begin

      if FOrientation = otHorizontal then begin
        FButtonWidth := Width / FButtonCount + 1;
        Rect.Left := 0;
        Rect.Top := 0;
        Rect.Right := FButtonWidth;
        Rect.Bottom := Height;
      end else begin
        FButtonHeight := (Height / FButtonCount) + 1;
        Rect.Left := 0;
        Rect.Right := Width;
        if FUpsideDown then begin
          Rect.Top := Height - FButtonHeight;
          Rect.Bottom := Height;
        end else begin
          Rect.Top := 0;
          Rect.Bottom := FButtonHeight;
        end;
      end;

      for i := 0 to FButtonCount - 1 do begin

        if i = Value then
          Canvas.Fill.Color := FHighlightColor
        else
          Canvas.Fill.Color := Color;
        Canvas.FillRect( Rect, 0, 0, allCorners, AbsoluteOpacity);

        if FImageList.Count > 0 then begin
          if i < FImageList.Count then begin
            {Canvas.DrawBitmap( FImageList.Items[i],
                               RectF(0,0,FImageList.Items[i].Width, FImageList.Items[i].Height),
                               Rect, AbsoluteOpacity)}
            Canvas.Fill.Color := claBlack;
            DrawImage( FParameter.ParamID, i, Canvas, FImageList.Items[i],
                       RectF(0,0,FImageList.Items[i].Width, FImageList.Items[i].Height),
                       Rect, 2, 2, AbsoluteOpacity);
          end;
        end else begin
          LabelText := '';
          if assigned(Parameter) then
            LabelText := Parameter.ButtonText[i]
          else
            if i < FButtonText.Count then
              LabelText := FButtonText[i];

          Canvas.Fill.Color := claBlack;
          Canvas.FillText( Rect, LabelText, False, AbsoluteOpacity, [], TTextAlign.taCenter)
        end;

        {if FBevelVisible then begin
          FDarkSideColor := claGray;
          FLightSideColor := claLightGray;
          if i <> Value then begin
            DrawBevel;
          end;
        end else begin
          FDarkSideColor := claGray;
          FLightSideColor := claGray;

          DrawBevel;
        end;}

        {if MidiAware and ShowMidiBox then begin
          if assigned(FMidiEditorAssignmentList) then
            MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasIndex(self, i)
          else
            MidiEditorAssignment := nil;
          DrawMidiAwareBox( Bitmap.Canvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment)
        end;}

        if FOrientation = otHorizontal then begin
          Rect.Left := Rect.Left + FButtonWidth - 1;
          Rect.Right := Rect.Right + FButtonWidth - 1;
        end else begin
          if FUpsideDown then begin
            Rect.Top := Rect.Top - (FButtonHeight - 1);
            Rect.Bottom := Rect.Bottom - (FButtonHeight - 1);
          end else begin
            Rect.Top := Rect.Top + FButtonHeight - 1;
            Rect.Bottom := Rect.Bottom + FButtonHeight - 1;
          end;
        end;
      end;

    end {else
      Bitmap.Canvas.Rectangle( ClientRect)};

    FDarkSideColor := claGray;
    FLightSideColor := claGray;

    DrawBevel;

{    ExtCanvas.Draw( ExtRect.Left, ExtRect.Top, Bitmap);

  finally
    Bitmap.Free;
  end;}
end;

procedure TG2FMXButtonRadio.ParsePanelData( fs: TModuleDefStream);

    procedure CalcSize;
    begin
      if FButtonHeight <> 0 then
        Height := FButtonHeight
      else
        FButtonHeight := Height;

      if FButtonWidth <> 0 then
        Width := FButtonWidth
      else
        FButtonWidth := Width;

      if FButtonCount > 0 then begin
        if FOrientation = otHorizontal then
          Width := (Width - 1) * FButtonCount + 1
        else
          Height := (Height -1) * FButtonCount + 1;
      end;
    end;

begin
  inherited;

  CalcSize;
  if FImageList.FImageData.Count > 0 then begin
    FImageList.BitmapWidth := FImageWidth;
    FImageList.ParseImageData( FButtonCount, True)
  end;
end;

function TG2FMXButtonRadio.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

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
      FButtonWidth := StrToInt(string(aValue));
    end else

      Result := False

  end;
end;

procedure TG2FMXButtonRadio.SetBounds(ALeft, ATop, AWidth, AHeight: single);
begin
  inherited;

  if FOrientation = otHorizontal then begin
    if FButtonCount > 0 then
      FButtonWidth := (AWidth / FButtonCount) + 1
    else
      FButtonWidth := 0;
  end else begin
    if FButtonCount > 0 then
      FButtonHeight := AHeight / FButtonCount + 1
    else
      FButtonHeight := 0;
  end;
end;

procedure TG2FMXButtonRadio.SetButtonCount( aValue: integer);
begin
  FButtonCount := aValue;
  FLowValue := 0;
  FHighValue := aValue - 1;
  Repaint;
end;

procedure TG2FMXButtonRadio.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  Repaint;
end;

{procedure TG2FMXButtonRadio.SetValueByCtrlMidi( aMidiEditorAssignment: TMidiEditorAssignment; aMidiEvent: TMyMidiEvent);
var MidiMessage, MidiValue1, MidiValue2 : byte;
    CtrlRange, MidiRange : byte;
begin
  if assigned(aMidiEditorAssignment) then begin
    MidiMessage := aMidiEvent.MidiMessage shr 4;
    MidiValue1 := aMidiEvent.Data1;
    MidiValue2 := aMidiEvent.Data2;

    if Value <> aMidiEditorAssignment.ControlIndex then
      Value := aMidiEditorAssignment.ControlIndex;

    if assigned(FOnClick) then FOnClick(self)
  end;
end;}


// ==== TG2FMXKnob =============================================================

constructor TG2FMXKnob.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := True;
  HitTest := True;
  AutoCapture := True;

  //Color := clWhite;

  FOrientation := otVertical;

  Width := 22;
  Height := Width;
  FCenterButtonSize := 5;
  FSliderSize := 10;
  SetLength(FNeedle, 3);

  FHighLightColor := G_HighLightColor;

  FFill := TBrush.Create(TBrushKind.bkSolid, claWhite);
  //FFill := TBrush.Create(TBrushKind.bkGradient, $FFFFFFFF);
  //FFill.Gradient.Color := $FFFFFFFF;
  //FFill.Gradient.Color1 := $FF080808;
  //FFillOuter.Gradient.Style := FMX.Types.TGradientStyle.gsRadial;
  FFillMorph := TBrush.Create(TBrushKind.bkSolid, $FFCCCCCC);


  FValue := 0;
  FLowValue := 0;
  FHighValue := 127;
end;

destructor TG2FMXKnob.Destroy;
begin
  FFillMorph.Free;
  FFill.Free;
  Finalize(FNeedle);

  inherited;
end;

procedure TG2FMXKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Single);
begin
  FSliderSelected := False;
  if ( ssLeft in Shift) then begin

    FStartX := X;
    FStartY := Y;
    FStartValue := Value;

    if (FType in [ktSlider, ktSeqSlider]) then begin
      if (PtInRect( GetSliderRect, PointF(X, Y))) then
        FSLiderSelected := True;
    end else
      if (FType in [ktReset, ktResetMedium]) and PtInRect( FCenterButtonRect, PointF(X, Y)) then begin
        Value := (HighValue - LowValue + 1) div 2;
      end else
        if PtInRect( FIncBtnRect, PointF(X, Y)) then begin
          if Value < HighValue then
            Value := Value + 1;
        end else
          if PtInRect( FDecBtnRect, PointF(X, Y)) then begin
            if Value > LowValue then
              Value := Value - 1;
          end;
  end;
  inherited;
end;

procedure TG2FMXKnob.MouseMove(Shift: TShiftState; X, Y: Single);
var FdX, FdY : single;
begin
  if (ssLeft in Shift) then begin

    FdX := X - FStartX;
    FdY := Y - FStartY;

    if ( FType in [ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium]) then begin
      if ssCtrl in Shift then
        SetMorphValue( CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * FdX / 100)) - Value)
      else
        Value := CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * FdX / 100));
    end else
      if ( FType in [ktSlider, ktSeqSlider]) and FSliderSelected then begin
        if ssCtrl in Shift then begin
          if (FOrientation = otVertical) then begin
            if (Height - FSliderSize > 0) then
              SetMorphValue( CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * -FdY / (Height - FSliderSize))) - Value);
          end else
            if (Width - FSliderSize > 0) then
              SetMorphValue( CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * FdX / (Width - FSliderSize))) - Value);
        end else begin
          if (FOrientation = otVertical) then begin
            if (Height - FSliderSize > 0) then
              Value := CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * -FdY / (Height - FSliderSize)));
          end else
            if (Width - FSliderSize > 0) then
              Value := CheckValueBounds( FStartValue + trunc((FHighValue - FLowValue) * FdX / (Width - FSliderSize)));
        end;
      end;
    //Invalidate;
  end;
  inherited;
end;

procedure TG2FMXKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Single);
begin
  FSliderSelected := False;
  inherited;
end;

{procedure TG2FMXKnob.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var mx, my, r : integer;
    rad, mrad, X, Y : single;
    p1, p2, p3 : TPoint;
    Rect, IconRect, MorphRect : TRect;
    MorphParameter : TMorphParameter;
    FastBitmap : TFastbitmap;
    BitMap : TBitmap;
begin
  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  Bitmap := TBitmap.Create;
  Fastbitmap := TFastbitmap.Create;
  try
    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Rect.Right - Rect.Left;
    Bitmap.Height := Rect.Bottom - Rect.Top;
    Bitmap.canvas.CopyRect(ClientRect, ExtCanvas, Rect);
    if FType in [ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium] then begin

      r := (FKnobRect.Right - FKnobRect.Left) div 2;
      mx := FKnobRect.Left + (FKnobRect.Right - FKnobRect.Left) div 2;
      my := FKnobRect.Top + (FKnobRect.Bottom - FKnobRect.Top) div 2;

      if (HighValue - LowValue) <> 0 then
        rad := 2*PI * ( 0.8 * Value / (HighValue - LowValue) + 0.1)
      else
        rad := 0.1;

      X := sin(rad)*(r);
      Y := cos(rad)*(r);

      p1.x := trunc( mx);
      p1.y := trunc( my);
      p2.x := trunc( mx - X);
      p2.y := trunc( my + Y);

      // Draw Morph range
      if HasMorph then begin
        MorphParameter := GetMorph;
        if assigned(MorphParameter) then begin

          Bitmap.Canvas.Brush.Style := bsSolid;
          Bitmap.Canvas.Brush.Color := Color;
          Bitmap.Canvas.Pen.Color := Color;
          Bitmap.Canvas.Ellipse( FKnobRect.Left, FKnobRect.Top, FKnobRect.Right, FKnobRect.Bottom);

          if (HighValue - LowValue) <> 0 then
            mrad := 2*PI * ( 0.8 * MorphValue / (HighValue - LowValue) + 0.1)
          else
            mrad := 0.1;

          p3.x := trunc(mx - sin(mrad)*(r));
          p3.y := trunc(my + cos(mrad)*(r));

          if ( p3.x <> p2.x) or ( p3.y <> p2.y) then begin

            Bitmap.Canvas.Brush.Color := CL_KNOB_MORPH_SELECTED;
            Bitmap.Canvas.Pen.Color := CL_KNOB_MORPH_SELECTED;

            if MorphParameter.Range < 128 then
              Bitmap.Canvas.Pie(FKnobRect.Left, FKnobRect.Top, FKnobRect.Right, FKnobRect.Bottom,
                                p3.x, p3.y,
                                p2.x, p2.y)
            else
              Bitmap.Canvas.Pie(FKnobRect.Left, FKnobRect.Top, FKnobRect.Right, FKnobRect.Bottom,
                                p2.x, p2.y,
                                p3.x, p3.y);

          end;
          Fastbitmap.CopyFromBitmap(Bitmap);
        end else begin // Non selected morph
            Fastbitmap.CopyFromBitmap(Bitmap);
            Fastbitmap.DrawAntialisedDisk(mx, my, r, 3, CL_KNOB_MORPH, bsSolid);
        end;
      end else begin // No morph
        Fastbitmap.CopyFromBitmap(Bitmap);
        Fastbitmap.DrawAntialisedDisk(mx, my, r, 3, Color, bsSolid);
      end;

      Fastbitmap.DrawAntialisedLine(p1.x, p1.y, p2.x, p2.y, clBlack);
      Fastbitmap.DrawAntialisedDisk(mx, my, r, 2, clBlack, bsClear);
      Fastbitmap.CopyToBitmap(Bitmap);

      //DrawWuLine( BitMap, p1, p2, clBlack);
      //DrawDisk( Bitmap, mx, my, r, 2, clBlack, bsClear);

      if FType in [ktReset, ktResetMedium] then begin

        Bitmap.Canvas.Pen.Color := clBtnShadow;
        if Value = (HighValue - LowValue + 1) div 2 then
          Bitmap.Canvas.Brush.Color := FHighLightColor
        else
          Bitmap.Canvas.Brush.Color := clGray;

        with IconRect do begin
          Left := 0;
          Right := 8;
          Top := 0;
          Bottom := 4;
        end;
        DrawIcon( Bitmap.Canvas, FCenterButtonRect, IconRect, itDown);
      end;

      if Selected then begin
        Bitmap.Canvas.Pen.Color := clWhite;
        Bitmap.Canvas.Brush.Color := CL_BTN_FACE;
        Bitmap.Canvas.FillRect(FIncBtnRect);
        DrawRect( Bitmap.Canvas, FIncBtnRect);
        Bitmap.Canvas.FillRect(FDecBtnRect);
        DrawRect( Bitmap.Canvas, FDecBtnRect);

        with IconRect do begin
          Left := 0;
          Right := 4;
          Top := 0;
          Bottom := 2;
        end;
        Bitmap.Canvas.Brush.Color := clBlack;
        DrawIcon( Bitmap.Canvas, FIncBtnRect, IconRect, itUp);
        DrawIcon( Bitmap.Canvas, FDecBtnRect, IconRect, itDown);
      end;
    end else
      if ( FType = ktSlider) or (FType = ktSeqSlider) then begin
        if Selected then
          Bitmap.Canvas.Pen.Color := CL_SELECTED_PARAM
        else
          Bitmap.Canvas.Pen.Color := CL_DISPLAY_BACKGRND;

        if HasMorph then begin
          MorphParameter := GetMorph;
          if assigned(MorphParameter) then begin
             MorphRect := GetMorphRect;
            Bitmap.Canvas.Brush.Color := CL_BTN_FACE;
            Bitmap.Canvas.Rectangle( ClientRect);
            Bitmap.Canvas.Brush.Color := CL_KNOB_MORPH_SELECTED;
            Bitmap.Canvas.Rectangle( MorphRect);
          end else begin
            Bitmap.Canvas.Brush.Color := CL_KNOB_MORPH;
            Bitmap.Canvas.Rectangle( ClientRect);
          end;
        end else begin
          Bitmap.Canvas.Brush.Color := CL_BTN_FACE;
          Bitmap.Canvas.Rectangle( ClientRect);
        end;

        Bitmap.Canvas.Brush.Color := CL_DISPLAY_BACKGRND;
        Bitmap.Canvas.FillRect( GetSliderRect);
      end;
    ExtCanvas.Draw( Rect.Left, Rect.Top, Bitmap);
  finally
    Bitmap.Free;
    Fastbitmap.Free;
  end;
end;}

procedure TG2FMXKnob.Paint;
var R2 : TRectF;
    mx, my, r,
    rad, mrad, X, Y : single;
    p1, p2 : TPointF;
begin
  if FType in [ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium] then begin

    Canvas.Fill.Assign(FFill);
    Canvas.Fill.Color := claBlack;
    Canvas.FillEllipse(FKnobRect, AbsoluteOpacity);

    R2.Left := FKnobRect.Left + 2;
    R2.Top := FKnobRect.Top + 2;
    R2.Right := FKnobRect.Right - 2;
    R2.Bottom := FKnobRect.Bottom - 2;

    Canvas.Fill.Color := claLightGray;
    Canvas.FillEllipse(R2, AbsoluteOpacity);

   // Canvas.Fill.Assign(FFillMorph);
   // Canvas.FillEllipse(R2, Opacity);

    r := (FKnobRect.Right - FKnobRect.Left) / 2;
    mx := FKnobRect.Left + (FKnobRect.Right - FKnobRect.Left) / 2;
    my := FKnobRect.Top + (FKnobRect.Bottom - FKnobRect.Top) / 2;

    if (HighValue - LowValue) <> 0 then
      rad := 2*PI * ( 0.8 * Value / (HighValue - LowValue) + 0.1)
    else
      rad := 0.1;

    X := -sin(rad)*(r);
    Y := cos(rad)*(r);

    FNeedle[0] := PointF( mx - sin(rad)*r*0.9, my + cos(rad)*r*0.9);
    FNeedle[1] := PointF( mx - sin(rad-0.5)*r*0.5, my + cos(rad-0.5)*r*0.5);
    FNeedle[2] := PointF( mx - sin(rad+0.5)*r*0.5, my + cos(rad+0.5)*r*0.5);

    Canvas.Fill.Color := claBlack;
    Canvas.Fill.Kind := TBrushKind.bkSolid;
    Canvas.FillPolygon( FNeedle, AbsoluteOpacity);

    {p1.x := mx;
    p1.y := my;
    p2.x := mx - X;
    p2.y := my + Y;

    p1.x := FKnobRect.Left;
    p1.y := FKnobRect.top;
    p2.x := FKnobRect.right;
    p2.y := FKnobRect.bottom;


    Canvas.Stroke.Color := claBlack;
    Canvas.DrawLine(p1, p2, 1);}
  end;
end;

function TG2FMXKnob.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FParameter := FModule.FData.Parameter[ StrToInt(string(aValue))] as TG2GraphParameter;
      (FParameter as TG2GraphParameter).AssignControl(self);
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Reset"' then begin
        KnobType := ktReset;
        Width := 22;
        Height := 28;
      end;
      if aValue = '"Reset/medium"' then begin
        KnobType := ktResetMedium;
        Width := 21;
        Height := 31;
      end;
      if aValue = '"Small"' then begin
        KnobType := ktSmall;
        Width := 22;
        Height := 23;
      end;
      if aValue = '"Medium"' then begin
        KnobType := ktMedium;
        Width := 22;
        Height := 25;
      end;
      if aValue = '"Big"' then begin
        KnobType := ktBig;
        Width := 23;
        Height := 27;
      end;
      if aValue = '"Slider"' then begin
        KnobType := ktSlider;
        Width := 11;
        Height := 64;
      end;
      if aValue = '"SeqSlider"' then begin
        KnobType := ktSeqSlider;
        Width := 11;
        Height := 64;
      end;

    end else
      Result := False
  end;
end;

function TG2FMXKnob.GetSliderRect: TRectF;
var SliderPos : integer;
begin
  if FOrientation = otVertical then begin
    if (HighValue - LowValue) <> 0 then
      SliderPos := trunc((Height - FSliderSize) * Value / (HighValue - LowValue))
    else
      SliderPos := 0;

    Result.Top := Height - SliderPos - FSliderSize;
    Result.Bottom := Result.Top + FSliderSize;
    Result.Left := 0;
    Result.Right := Width;
  end else begin
    if (HighValue - LowValue) <> 0 then
      SliderPos := trunc((Width - FSliderSize) * Value / (HighValue - LowValue))
    else
      SliderPos := 0;

    Result.Top := 0;
    Result.Bottom := Height;
    Result.Left := SliderPos;
    Result.Right := Result.Left + FSLiderSize;
  end;
end;

function TG2FMXKnob.GetMorphRect: TRectF;
var ValuePos, MorphPos : integer;
begin
  ValuePos := 0;
  MorphPos := 0;
  if FOrientation = otVertical then begin
    if (HighValue - LowValue) <> 0 then begin
      ValuePos := trunc( Height * Value / (HighValue - LowValue));
      MorphPos := trunc( Height * MorphValue / (HighValue - LowValue));
    end;
    if MorphPos > ValuePos then begin
      Result.Top := Height - ValuePos;
      Result.Bottom := Height - MorphPos;
    end else begin
      Result.Top := Height - MorphPos;
      Result.Bottom := Height - ValuePos;
    end;
    Result.Left := 0;
    Result.Right := Width;
  end else begin
    if (HighValue - LowValue) <> 0 then begin
      ValuePos := trunc(Width * Value / (HighValue - LowValue));
      MorphPos := trunc(Width * MorphValue / (HighValue - LowValue));
    end;

    Result.Top := 0;
    Result.Bottom := Height;
    if MorphPos > ValuePos then begin
      Result.Left := ValuePos;
      Result.Right := MorphPos;
    end else begin
      Result.Left := MorphPos;
      Result.Right := ValuePos;
    end;
  end;
end;

procedure TG2FMXKnob.SetBounds(ALeft, ATop, AWidth, AHeight: single);
begin
  //Position.X := ALeft;
  //Position.Y := ATop;

  FKnobRect.Left := AWidth / 2 - FKnobRad;
  FKnobRect.Right := AWidth / 2 + FKnobRad;
  case FType of
    ktBig, ktMedium, ktSmall, ktExtraSmall:
      begin
        FKnobRect.Top := 0;
      end;
    ktReset:
      begin
        FKnobRect.Top := FCenterButtonSize;
      end;
    ktResetMedium:
      begin
        FKnobRect.Top := FCenterButtonSize + 1;
      end;
    ktSlider: ;
    ktSeqSlider: ;
  end;
  FKnobRect.Bottom := FKnobRect.Top + FKnobRad * 2;

  FCenterButtonRect.Left := (AWidth - FCenterButtonSize*2) / 2;
  FCenterButtonRect.Top := 0;
  FCenterButtonRect.Right := (AWidth + FCenterButtonSize*2) / 2;
  FCenterButtonRect.Bottom := FCenterButtonSize;

  FIncBtnRect.Left := AWidth / 2;
  FIncBtnRect.Top := AHeight - 9;
  FIncBtnRect.Right := AWidth / 2 + 11;
  FIncBtnRect.Bottom := AHeight;

  FDecBtnRect.Left := AWidth / 2 - 10;
  FDecBtnRect.Top := AHeight - 9;
  FDecBtnRect.Right := AWidth / 2 + 1;
  FDecBtnRect.Bottom := AHeight;

  inherited;
end;

procedure TG2FMXKnob.SetKnobType( aValue: TKnobType);
begin
  FType := aValue;
  case FType of
    ktBig         : FKnobRad := 11;
    ktMedium      : FKnobRad := 10;
    ktSmall       : FKnobRad := 9;
    ktExtraSmall  : FKnobRad := 8;
    ktReset       : FKnobRad := 9;
    ktResetMedium : FKnobRad := 10;
    ktSlider      :;
    ktSeqSlider   :;
  end;
  SetBounds(Position.X, Position.Y, Width, Height);
  //Invalidate;
end;

procedure TG2FMXKnob.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  //Invalidate;
end;

procedure TG2FMXKnob.SetHighLightColor( aValue: TAlphaColor);
begin
  FHighlightColor := aValue;
  //Invalidate;
end;


// ==== TG2GraphCable ==========================================================

constructor TG2GraphCable.Create( AOwner : TComponent);
var i : integer;
begin
  if not( AOwner is TG2GraphPatch) then
    raise Exception.Create('Owner must be a patch');

  FPatch := AOwner as TG2GraphPatch;
  FParent := nil;

  FromConnector := nil;
  ToConnector   := nil;

  FGraphControl := TG2FMXCable.Create(AOwner);

  InitCable;
end;

destructor TG2GraphCable.Destroy;
begin
  FGraphControl.Free;

  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);

  if assigned(ToConnector) then
    ToConnector.DelCable( self);

  inherited;
end;

procedure TG2GraphCable.SetParent(aValue: TFmxObject);
begin
  FParent := aValue;
  FGraphControl.Parent := FParent;
end;

procedure TG2GraphCable.InitCable;
var ModuleFrom, ModuleTo : TG2FMXModule;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) and (assigned((FromConnector.Module as TG2GraphModule).Parent)))) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2FMXConnector;
  ConnectorTo := ToConnector.GraphControl as TG2FMXConnector;

  FGraphControl.Point1X := ModuleFrom.ScrollPosX + ConnectorFrom.Position.X + ConnectorFrom.Width / 2;
  FGraphControl.Point1Y := ModuleFrom.ScrollPosY + ConnectorFrom.Position.Y + ConnectorFrom.Height / 2;
  FGraphControl.Point2X := ModuleTo.ScrollPosX + ConnectorTo.Position.X + ConnectorTo.Width / 2;
  FGraphControl.Point2Y := ModuleTo.ScrollPosY + ConnectorTo.Position.Y + ConnectorTo.Height / 2;
  FGraphControl.InitCable;
end;

procedure TG2GraphCable.ConnectorMoved;
var ModuleFrom, ModuleTo : TG2FMXModule;
    ConnectorFrom, ConnectorTo : TG2FMXConnector;
begin
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) and (assigned((FromConnector.Module as TG2GraphModule).Parent)))) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2FMXConnector;
  ConnectorTo := ToConnector.GraphControl as TG2FMXConnector;

  FGraphControl.Point1X := ModuleFrom.ScrollPosX + ConnectorFrom.Position.X + ConnectorFrom.Width / 2;
  FGraphControl.Point1Y := ModuleFrom.ScrollPosY + ConnectorFrom.Position.Y + ConnectorFrom.Height / 2;
  FGraphControl.Point2X := ModuleTo.ScrollPosX + ConnectorTo.Position.X + ConnectorTo.Width / 2;
  FGraphControl.Point2Y := ModuleTo.ScrollPosY + ConnectorTo.Position.Y + ConnectorTo.Height / 2;
  FGraphControl.StartTimer;
end;

// ==== TG2FMXCable ============================================================

constructor TG2FMXCable.Create(AOwner: TComponent);
var i : integer;
begin
  inherited;

  HitTest := False;
  FTimer := TTimer.Create(self);
  FTimer.Interval := 25;

  FMargin := 10;

  FP1.X := Position.X + FMargin;
  FP1.Y := Position.Y + FMargin;

  FP2.X := Position.X + Width - FMargin * 2;
  FP2.Y := Position.Y + FMargin;

  FNodeCount := 10;
  SetLength(FNodes, FNodeCount);
  for i := 0 to FNodeCount - 1 do
    FNodes[i] := TNode.Create;

  FTimer.OnTimer := OnTimer;
  InitCable;
end;

destructor TG2FMXCable.Destroy;
var i : integer;
begin
  for i := 0 to FNodeCount - 1 do
    FNodes[i].Free;
  Finalize(FNodes);

  FTimer.Enabled := False;
  FTimer.Free;

  inherited;
end;

procedure TG2FMXCable.InitCable;
var n : integer;
    dx, dy : single;
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
  min_x := min(FP1.X, FP2.X);
  max_x := max(FP1.X, FP2.X);
  min_y := min(FP1.Y, FP2.Y);
  max_y := max(FP1.Y, FP2.Y);

  n := FNodeCount - 1;

  dx := ( FP2.X - FP1.X) / (n + 1);
  dy := ( FP2.Y - FP1.Y) / (n + 1);

  halfx := ( FP2.X - FP1.X) / 2;
  maxsag := Caterny(-halfx);

  FNodes[n].x := FP2.X;
  FNodes[n].y := FP2.Y;

  dec(n);
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

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.y;

  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
end;

procedure TG2FMXCable.IterateCable;
var n : integer;
    min_x, min_y, max_x, max_y : single;
begin
  n := FNodeCount - 1;

  min_x := min(FP1.X, FP2.X);
  max_x := max(FP1.X, FP2.X);
  min_y := min(FP1.Y, FP2.Y);
  max_y := max(FP1.Y, FP2.Y);

  FNodes[n].x := FP2.X;
  FNodes[n].y := FP2.Y;

  dec(n);

  while (n > 0) do begin

  	FNodes[n].vx := FNodes[n].vx + ( FNodes[n + 1].x + FNodes[n - 1].x - FNodes[n].x * 2 ) / TENSION;
		FNodes[n].vy := FNodes[n].vy + ( FNodes[n + 1].y + FNodes[n - 1].y - FNodes[n].y * 2 ) / TENSION;

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

    dec(n);
  end;
  FNodes[0].x := FP1.X;
  FNodes[0].y := FP1.Y;

  Position.X := min_x - FMargin;
  Position.Y := min_y - FMargin;
  Width := max_x - Position.X + FMargin;
  Height := max_y - Position.Y + FMargin;
end;

procedure TG2FMXCable.OnTimer(Sender: TObject);
begin
  if FTimerCount <= 0 then
    FTimer.Enabled := False;
  Dec(FTimerCount);
  IterateCable;
  Repaint;
end;

procedure TG2FMXCable.StartTimer;
begin
  IterateCable;
  FTimerCount := 50;
  FTimer.Enabled := True;
end;

procedure TG2FMXCable.SetPoint1X( Value : single);
begin
  FP1.X := Value;
end;

procedure TG2FMXCable.SetPoint1Y( Value : single);
begin
  FP1.Y := Value;
end;

procedure TG2FMXCable.SetPoint2X( Value : single);
begin
  FP2.X := Value;
end;

procedure TG2FMXCable.SetPoint2Y( Value : single);
begin
  FP2.Y := Value;
end;

procedure TG2FMXCable.Paint;
var i : integer;
begin
  //Canvas.FillRect(RectF(0, 0, Width, Height), 0, 0, [], 1);
  Canvas.Stroke.Kind := TBrushKind.bkSolid;
  Canvas.StrokeThickness := 2;

  for i := 1 to FNodeCount - 1 do begin
    Canvas.DrawLine(PointF(FNodes[i-1].x - Position.X, FNodes[i-1].y - Position.Y),
                    PointF(FNodes[i].x - Position.X, FNodes[i].y - Position.Y), Opacity);
  end;
end;


procedure Register;
begin
  RegisterComponents('G2 FMX Controls', [TG2FMXPatchArea, TG2FMXControl,
      TButtonBevel, TG2FMXButton, TG2FMXKnob, TG2FMXCable, TG2FMXConnector,
      TG2FMXButtonText, TG2FMXButtonFlat]);
end;


initialization
  RegisterFMXClasses([TG2FMXPatchArea, TG2FMXControl, TButtonBevel,
      TG2FMXButton, TG2FMXKnob, TG2FMXCable, TG2FMXConnector, TG2FMXButtonText,
      TG2FMXButtonFlat]);
end.
