unit g2_graph;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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

//  ////////////////////////////////////////////////////////////////////////////
//
//  This unit contains the G2 objects with the methods to display and
//  interact with in Windows
//
//  ////////////////////////////////////////////////////////////////////////////

// For TBGRABitmap : it is important to check to store variables into registers (-Or)

// http://delphirss.com/graphic.html

{$I includes\delphi_version.inc}

interface

uses
{$IFDEF FPC}
  LclIntf, intfgraphics, FPImage,
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinApi.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}

{$IFDEF G2_VER220_up}
  WinApi.Messages, System.Classes, System.SysUtils, Vcl.Forms,StdCtrls,
  Vcl.ExtCtrls, Vcl.Controls, Vcl.Graphics, Vcl.Menus, VCL.Dialogs, System.math,
  System.Contnrs,
{$ELSE}
  Forms, Messages, Classes, SysUtils, StdCtrls, ExtCtrls, Controls,
  Graphics, Menus, Dialogs, math, Contnrs,
{$ENDIF}
  g2_database, MidiType,
  g2_file, g2_types, g2_usb, g2_midi, graph_util_vcl, fastbitmap;

type
  TG2GraphChildControl = class;
  TG2GraphLed = class;
  TG2GraphLedGroup = class;
  TG2GraphMiniVU = class;
  TG2GraphLedGreen = class;
  TG2GraphDropDownList = class;
  TG2GraphPartSelector = class;
  TG2GraphDisplay = class;
  TG2GraphModule = class;
  TG2GraphModulePanel = class;
  TG2GraphKnob = class;
  TG2GraphConnector = class;
  TG2GraphCable = class;
  TG2GraphPatch = class;
  TG2GraphParameter = class;
  TG2Graph = class;
  TG2GraphScrollBox = class;

  TClickEvent = procedure(Sender: TObject) of object;
  TChangeEvent = procedure(Sender : TObject) of Object;
  TModuleClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule) of Object;
  TParameterClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter) of Object;
  TConnectorClickEvent = procedure(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileConnector) of Object;

  TG2ImageList = class( TObjectList)
  private
    // Class to hold the bitmaps from the module resource files
    FBitmapWidth  : integer;
    FBitmapHeight : integer;
    FImageData    : TStrings;
    function      GetBitmap( aIndex : integer) : TBitmap;
    procedure     SetBitmap( aIndex : integer; aValue : TBitmap);
    procedure     SetImageData( aValue : TStrings);
  public
    constructor Create( AOwnsObjects : boolean);
    destructor  Destroy; override;
    procedure   ParseImageData( ImageCount : integer; Hex : boolean);
    function    AddBitmap( aValue : TBitmap): integer;

    property    ImageData : TStrings read FImageData write SetImageData;
    property    Items[ index : integer]: TBitmap read GetBitmap write SetBitmap;
    property    BitmapWidth : integer read FBitmapWidth write FBitmapWidth;
    property    BitmapHeight : integer read FBitmapHeight write FBitmapHeight;
  end;

  TG2Graph = class( TG2Midi)
  private
    FScrollboxVA    : TG2GraphScrollBox;
    FScrollboxFX    : TG2GraphScrollBox;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetSelectedPatch : TG2GraphPatch;
    procedure   SetModuleParent( aLocation : TLocationType; aValue: TG2GraphScrollbox);
    procedure   SetScrollboxVA( aValue : TG2GraphScrollbox);
    procedure   SetScrollboxFX( aValue : TG2GraphScrollbox);
    procedure   Invalidate;
    procedure   G2ProcessWindowsMessages; override;
    function    G2MessageDlg( tekst : string; code : integer): integer; override;
  published
    property    ScrollboxVA : TG2GraphScrollBox read FScrollboxVA write SetScrollboxVA;
    property    scrollboxFX : TG2GraphScrollBox read FScrollboxFX write SetScrollboxFX;
  end;

  TG2GraphPerformance = class( TG2USBPerformance)
  protected
    procedure   InitSelectedSlotIndex( aValue : TBits2); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TG2GraphPanel = class(TPanel)
  private
    FExtBitmap       : TBitmap;
{$IFDEF FPC}
    //procedure Paint; override;
{$ELSE}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
{$ENDIF}
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TG2GraphScrollBox = Class(TScrollBox)
  private
    FExtBitmap        : TBitmap;
    FBackBitmap       : TBitmap;
    FRackBitmap       : TBitmap;
    FCanvas           : TCanvas;
    FLocation         : TLocationType;
    FG2Graph          : TG2Graph;
    FBackgroundColor  : TColor;
    FRackColor        : TColor;
    FSelectRectActive : boolean;
    FOutlineRect      : TRect;
    FOutlineVisible   : boolean;
    FCopyPatch        : TG2FilePatchPart;
    FCopyOutLinesVisible : boolean;
    FStartX, FStartY : integer;
{$IFNDEF G2_VER210_up}
    FParentDoubleBuffered : boolean;
{$ENDIF}
{$IFDEF FPC}
    procedure Paint; override;
{$ELSE}
    procedure   WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure   WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
{$ENDIF}
    procedure   SetBitmap( aValue: TBitmap);
    procedure   SetBackgroundColor( aValue : TColor);
    procedure   SetRackColor( aValue : TColor);
    procedure   SetCopyPatch( aValue : TG2FilePatchPart);
  protected
    procedure   PaintBackGround( ExtCanvas: TCanvas; ExtBoundsRect: TRect);
    procedure   PaintCables( ExtBitmap : TBitmap; ExtBoundsRect : TRect);
    procedure   MouseEnter (var Msg: TMessage); message cm_mouseEnter;
    procedure   MouseLeave (var Msg: TMessage); message cm_mouseLeave;
    procedure   MouseDown( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   BuildRackBitmap;
    procedure   DrawOutlineCanvas( ExtCanvas : TCanvas; ExtBoundsRect: TRect; Rect : TRect);
    procedure   DrawOutline( Rect : TRect);
    procedure   DrawCopyPatchOutlines( ExtCanvas : TCanvas; ExtBoundsRect: TRect);
    procedure   DrawLine( X1, Y1, X2, Y2 : integer);
    procedure   SetPositionsInCopyPatch;
    function    GetPatchCoord( ClientPoint : TPoint): TPoint;

    property    G2 : TG2Graph read FG2Graph write FG2Graph;
    property    CopyPatch : TG2FilePatchPart read FCopyPatch write SetCopyPatch;
  published
    property    Location : TLocationType read FLocation write FLocation;
    property    BackBitmap: TBitmap read FBackBitmap write SetBitmap;
    property    BackgroundColor : TColor read FBackgroundColor write SetBackgroundColor;
    property    RackColor : TColor read FRackColor write SetRackColor;
    property    DoubleBuffered;
{$IFNDEF G2_VER210_up}
    // Probably introduced in Delphi2010? Added a dummy for older versions
    property    ParentDoubleBuffered : boolean read FParentDoubleBuffered write FParentDoubleBuffered;
{$ENDIF}
  end;

  TG2GraphParameter = class(TG2FileParameter)
  private
    FControlList  : array of TG2GraphChildControl; // array of controls the parameter is assigned to
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
    destructor  Destroy; override;
    procedure   AssignControl( aControl : TGraphicControl);
    procedure   DeassignControl( aControl : TGraphicControl);
    procedure   InvalidateControl; override;
  end;

  TMiniVUList = class(TObjectList)
  private
    function GetMiniVU(aindex: integer): TG2GraphMiniVU;
    procedure SetMiniVU(aindex: integer; const Value: TG2GraphMiniVU);
  public
    function Add( aMiniVU : TG2GraphMiniVU): integer;
    property Items[ aIndex: integer] : TG2GraphMiniVU read GetMiniVU write SetMiniVU; default;
  end;

  TLedGroupList = class(TObjectList)
  private
    function GetLedGroup(aindex: integer): TG2GraphLedGroup;
    procedure SetLedGroup(aindex: integer; const Value: TG2GraphLedGroup);
  public
    function Add( aLedGroup : TG2GraphLedGroup): integer;
    property Items[ aIndex: integer] : TG2GraphLedGroup read GetLedGroup write SetLedGroup; default;
  end;

  TLed39List = class(TObjectList)
  private
    function GetLedGreen(aindex: integer): TG2GraphLedGreen;
    procedure SetLedGreen(aindex: integer; const Value: TG2GraphLedGreen);
  public
    function Add( aLedGreen : TG2GraphLedGreen): integer;
    property Items[ aIndex: integer] : TG2GraphLedGreen read GetLedGreen write SetLedGreen; default;
  end;

  TLed3AList = class(TObjectList)
  private
    function GetLed(aindex: integer): TG2GraphLed;
    procedure SetLed(aindex: integer; const Value: TG2GraphLed);
  public
    function Add( aLed : TG2GraphLed): integer;
    property Items[ aIndex: integer] : TG2GraphLed read GetLed write SetLed; default;
  end;

  TG2GraphPatch = class( TG2USBPatch)
  private
    // Lists for the leds
    FMiniVUList         : TMiniVUList;
    FLedGroupList       : TLedGroupList;
    FLed39List          : TLed39List;
    FLed3AList          : TLed3AList;

    FVisible            : boolean;

    FSelectedControl    : TG2GraphChildControl; // needed to handle mouseevents

    procedure   SetVisible( aValue : boolean);
    procedure   SetSelectedControl( aValue : TG2GraphChildControl);
  protected
    procedure   SetSelectedMorphIndex( aValue : integer); override;
    procedure   SetSelectedLocation( aLocation : TLocationType); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Init; override;

    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; override;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; override;
    procedure   DeleteCableFromPatch( aLocation : TLocationType; aCable: TG2FileCable); override;
    procedure   ShakeCables;
    procedure   UpdateColorScheme;

    //procedure   SelectModules;
    procedure   SelectModulesInRect( aLocation : TLocationType; aRect : TRect);
    procedure   MoveOutlines( aLocation : TLocationType; dX, dY : integer);
    function    MessMoveModules( aLocation : TLocationType): boolean; override;

    function    GetG2 : TG2Graph;
    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; override;
    //procedure   UnselectModules( aLocation : TLocationType);
    procedure   SetMiniVULevel( Index : integer; aValue : byte); override;
    procedure   SetLedLevel( Index : integer; aValue : byte); override;
    function    GetMiniVUListCount : integer; override;
    function    GetLedListCount : integer; override;
    procedure   SortLeds; override;
    procedure   RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer); override;

    procedure   SelectNextModuleControl;
    procedure   SelectPrevModuleControl;

    property    Visible : boolean read FVisible write SetVisible;
    property    SelectedControl : TG2GraphChildControl read FSelectedControl write SetSelectedControl;
  end;

  TG2GraphModule = class( TG2FileModule)
  private
    FOutlineRect    : TRect;
    FOutlineVisible : boolean;
    FFreePanel      : boolean;
    FPanel          : TG2GraphModulePanel;
  protected
    function    GetParent : TWinControl;
    procedure   SetParent( aValue : TWinControl);
    function    GetVisible : boolean;
    procedure   SetVisible( aValue : boolean);
    function    GetScrollPosX : integer;
    function    GetScrollPosY : integer;
    procedure   SetSelected( aValue: boolean); override;
    //function    GetNewCol : TBits7; override;
    //function    GetNewRow : TBits7; override;
    procedure   SetModuleColor( Value : TBits8); override;
    function    GetOnModuleClick : TModuleClickEvent;
    procedure   SetOnModuleClick( aValue : TModuleClickEvent);
    function    GetOnParameterClick: TParameterClickEvent;
    procedure   SetOnParameterClick( aValue : TParameterClickEvent);
    function    GetOnConnectorClick: TConnectorClickEvent;
    procedure   SetOnConnectorClick( aValue : TConnectorClickEvent);
  public
    //constructor Create( AOwner: TComponent); override;
    //constructor CopyCreate( AOwner : TComponent; aModule : TG2GraphModule);
    constructor Create( aPatchPart : TG2FilePatchPart); override;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aModule : TG2GraphModule);
    destructor  Destroy; override;
    //function    CreateCopy( AOwner : TComponent) : TG2FileModule; override;
    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; override;
    function    CreateParameter: TG2FileParameter; override;
    procedure   ParsePanelData;
    procedure   Invalidate;
    function    ClientToScreen( p : TPoint):  TPoint;
    procedure   SetCol( aValue : TBits7); override;
    procedure   SetRow( aValue : TBits7); override;
    property    Panel: TG2GraphModulePanel read FPanel;
  published
    property    Parent: TWinControl read GetParent write SetParent;
    property    Visible: boolean read GetVisible write SetVisible;
    property    ScrollPosX : integer read GetScrollPosX;
    property    ScrollPosY : integer read GetScrollPosY;
    property    OnModuleClick : TModuleClickEvent read GetOnModuleClick write SetOnModuleClick;
    property    OnParameterClick : TParameterClickEvent read GetOnParameterClick write SetOnParameterClick;
    property    OnConnectorClick : TConnectorClickEvent read GetOnConnectorClick write SetOnConnectorClick;
  end;

  TG2GraphModulePanel = class( TGraphicControl)
  private
    FData : TG2GraphModule;
    FOldX,
    FOldY : integer;
    FStartX,
    FStartY : integer;

    FChildControls : TList;
    FWasAlreadySelected : boolean;

    FOnModuleClick : TModuleClickEvent;
    FOnParameterClick : TParameterClickEvent;
    FOnConnectorClick : TConnectorClickEvent;

    function    GetScrollBarX : integer;
    function    GetScrollBarY : integer;
    function    GetScrollPosX : integer;
    function    GetScrollPosY : integer;
    procedure   SetSelected( aValue : boolean);
    function    GetSelected: boolean;
    function    GetLocation : TLocationType;
    function    GetModuleIndex : TBits8;
    function    GetColor : TColor;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
    procedure   Paint; override;

  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SetCol( aValue : TBits7); //override;
    procedure   SetRow( aValue : TBits7); //override;
    function    CalcNewCol : TBits7;
    function    CalcNewRow : TBits7;

    function    GetGraphChildControl( ChildControlIndex : integer): TG2GraphChildControl;
    function    GetChildControlsCount : integer;
    procedure   AddGraphControl( aGraphCtrl : TG2GraphChildControl);

    //procedure   SelectModule;
    procedure   MoveOutline( dX, dY : integer);
    procedure   MoveModule;

    property    ScrollBarX : integer read GetScrollBarX;
    property    ScrollBarY : integer read GetScrollBarY;
    property    ScrollPosX : integer read GetScrollPosX;
    property    ScrollPosY : integer read GetScrollPosY;
    property    GraphChildControls[ index : integer] : TG2GraphChildControl read GetGraphChildControl;
    property    ChildControlsCount : integer read GetChildControlsCount;

    function    GetPatch : TG2GraphPatch;
    function    GetControlType( aG2GraphChildControl: TG2GraphChildControl): string;
    function    NewG2GraphControl( aControlType : AnsiString) : TG2GraphChildControl;

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
    //property    NewRow : TBits7 read GetNewRow;
    //property    NewCol : TBits7 read GetNewCol;
    property    Data : TG2GraphModule read FData write FData;
    property    Location    : TLocationType read GetLocation;
    property    ModuleIndex : TBits8 read GetModuleIndex;
    property    Color : TColor read GetColor;
  end;

  TG2GraphChildControl = class( TMidiAwareControl)
  // This represents a child graphic control of a module
  protected
    FSelected       : boolean;
    FID             : integer;
    FModule         : TG2GraphModulePanel;
    FParameter      : TG2FileParameter;
    FMouseInput     : boolean;
    FZOrder         : integer;
    FValue,
    FStartValue,
    FLowValue,
    FHighValue      : byte;
    FOnChange       : TChangeEvent;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   Paint; override;
    procedure   Select;
    procedure   DeSelect; virtual;
    procedure   SetLowValue( aValue: byte);
    procedure   SetHighValue( aValue: byte);
    function    GetSelected : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); virtual;
    procedure   Invalidate; override;
    procedure   Update; override;
    function    GetRelToParentRect : TRect;
    function    GetScreenCoordsRect : TRect; virtual;
    function    GetParamLabelIndex : integer; virtual;
    procedure   SetParameter( aParam : TG2FileParameter); virtual;
    procedure   SetModule( aValue : TG2GraphModulePanel);
    function    GetParameter : TG2FileParameter; override;
    procedure   SetValue( aValue : byte);
    function    GetValue : byte; override;
    procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); override;
    procedure   SetParamLabel( aIndex : integer; aValue : AnsiString);
    function    GetParamLabel( aIndex : integer) : AnsiString;
    procedure   InitValue( aValue: integer);
    function    CheckValueBounds( aValue : integer): byte;
    procedure   SetMorphValue( aValue: byte);
    function    GetHighValue : byte; override;
    function    GetLowValue : byte; override;
    function    GetGraphModule : TG2GraphModulePanel;
    function    GetMorphValue: byte;
    function    HasMorph: boolean;
    function    GetMorph : TMorphParameter;
    function    GetParamIndex : integer;
    procedure   ParsePanelData( fs : TModuleDefStream); virtual;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; virtual;

    property    Parameter   : TG2FileParameter read GetParameter write SetParameter;
    property    ParamIndex  : integer read GetParamIndex;
    property    Module      : TG2GraphModulePanel read FModule write SetModule;
    property    MouseInput  : boolean read FMouseInput write FMouseInput;
    property    Selected    : boolean read GetSelected;
    property    ParamLabel[ Index : integer]: AnsiString read GetParamLabel write SetParamLabel;
  published
    property    ID : integer read FID write FID;
    property    Font;
    property    Color;
    property    Value       : byte read GetValue write SetValue;
    property    MorphValue  : byte read GetMorphValue;
    property    LowValue    : byte read GetLowValue write SetLowValue;
    property    HighValue   : byte read GetHighValue write SetHighValue;
    property    OnMouseUp;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnChange    : TChangeEvent read FOnChange write FOnChange;
    property    ParentColor;
    property    ParentFont;
  end;

  TG2GraphBitmap = class( TG2GraphChildControl)
  private
    FImageList : TG2ImageList;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    property    ImageList : TG2ImageList read FImageList;
  end;

  TG2GraphLine = class( TG2GraphChildControl)
  private
    FOrientation  : TOrientationType;
    FLineWidth    : integer;
    FLength       : integer;
  protected
    procedure   SetOrientation( aValue : TOrientationType);
    procedure   SetLineWidth( aValue : integer);
    procedure   SetLength( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    Orientation : TOrientationType read FOrientation write SetOrientation;
    property    LineWidth : integer read FLineWidth write SetLineWidth;
    property    Length : integer read FLength write SetLength;
  end;

  TG2GraphLabel = class( TG2GraphChildControl)
  private
    FCaption : string;
    FOnClick : TClickEvent;
  protected
    procedure   SetCaption( aValue : string);
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    Caption : string read FCaption write SetCaption;
    property    OnClick : TClickEvent read FOnClick write FOnClick;
  end;

  TG2GraphDisplay = class( TG2GraphChildControl)
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
    FDependencies : TStringList;
  protected
    function    GetLine( LineNo : integer): AnsiString;
    procedure   SetLine( LineNo : integer; aValue : AnsiString);
    procedure   SetLineCount( aValue : integer);
    procedure   SetTextFunction( aValue : integer);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetParameter( aParam : TG2FileParameter); override;
    property    Line[ index : integer] : AnsiString read GetLine write SetLine;
  published
    property    LineCount : integer read FLineCount write SetLineCount;
    property    TextFunction : integer read FTextFunction write SetTextFunction;
    property    DisplayType : integer read FDisplayType write FDisplayType;
    property    MasterRef : integer read FMasterRef;
    property    Dependencies : TStringList read FDependencies;
    property    Font;
    property    Color;
  end;

  TG2GraphGraph = class( TG2GraphChildControl)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    Font;
    property    Color;
  end;

  TG2GraphLed = class( TG2GraphChildControl)
  public
    FGroupId : integer;
    FType    : TLedType;
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetLevel( aValue : byte); virtual;
  end;

  TG2GraphLedGreen = class( TG2GraphLed)
  private
    FCodeRef     : integer;
    FInfoFunc    : integer;
    FLevel       : byte;
    FLedColor    : TColor;
  protected
    procedure   SetLedColor( Value : TColor);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetLevel( aValue : byte); override;
  published
    property    LedColor : TColor read FLedColor write SetLedColor;
  end;

  TG2GraphLedGroup = class( TG2GraphLed)
  private
    FLeds : TList;
    FLedOn : integer;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetLevel( aValue : byte); override;
  end;

  TG2GraphMiniVU = class( TG2GraphLed)
  private
    FMiniVUWidth,
    FMiniVUHeight : integer;
    FCodeRef      : integer;
    FInfoFunc     : integer;
    FOrientation  : TOrientationType;
    FLevel        : byte;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetLevel( aValue : byte); override;
  end;

  TG2GraphButton = class( TG2GraphChildControl)
  private
    FButtonText     : TStrings;
    FImageList      : TG2ImageList;
    FButtonWidth    : integer;
    FButtonHeight   : integer;
    FButtonCount    : integer;
    FOrientation    : TOrientationType;
    FHighlightColor : TColor;
    FBorderColor    : TColor;
    FBevel          : boolean;
    FImageWidth     : integer;
    FImageCount     : integer;
    FCaption        : string;
    FIcon           : TIconType;
    FPressed        : boolean;
    FOnClick        : TClickEvent;
  protected
    procedure   MouseDown( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   SetCaption( aValue : string);
    procedure   SetIcon( aValue : TIconType);
    procedure   SetButtonText( aValue : TStrings);
    procedure   SetHighLightColor( aValue : TColor);
    procedure   SetBorderColor( aValue : TColor);
    procedure   SetButtonCount( aValue : integer); virtual;
    procedure   SetBevel( aValue : boolean);
    procedure   SetOrientation( aValue : TOrientationType); virtual;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    property    ImageList : TG2ImageList read FImageList;
    property    Module : TG2GraphModulePanel read FModule;
    property    Icon : TIconType read FIcon write SetIcon;
  published
    property    Caption : string read FCaption write SetCaption;
    property    OnClick : TClickEvent read FOnClick write FOnClick;
    property    ButtonText : TStrings read FButtonText write SetButtonText;
    property    HightlightColor : TColor read FHighlightColor write SetHighlightColor;
    property    BorderColor : TColor read FBorderColor write SetBorderColor;
    property    Bevel : boolean read FBevel write SetBevel;
    property    Orientation : TOrientationType read FOrientation write SetOrientation;
    property    ButtonCount : integer read FButtonCount write SetButtonCount;
  end;

  TG2GraphButtonText = class( TG2GraphButton)
  private
    FButtonTextType : TButtonTextType;
    procedure   SetButtonTextType( aValue : TButtonTextType);
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); override;
  published
    property    ButtonTextType : TButtonTextType read FButtonTextType write SetButtonTextType;
  end;

  TG2GraphTextEdit = class( TG2GraphButtonText)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
  end;

  TG2GraphButtonFlat = class( TG2GraphButton)
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); override;
  end;

  TG2GraphLevelShift = class( TG2GraphButtonFlat)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
  end;

  TG2GraphButtonRadio = class( TG2GraphButton)
  protected
    FUpsideDown : boolean;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   SetOrientation( aValue : TOrientationType); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   SetButtonCount( aValue : integer); override;
    procedure   SetBounds(ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); override;

    property    UpsideDown : boolean read FUpsideDown write FUpsideDown;
  end;

  TG2GraphButtonRadioEdit = class( TG2GraphButtonRadio)
  private
    FButtonColumns : integer;
    FButtonRows : integer;
    FClickedButton : integer;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   SetBounds(ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    function    GetScreenCoordsRect : TRect; override;
    function    GetParamLabelIndex : integer; override;
  published
    property    ButtonColumns : integer read FButtonColumns write FButtonColumns;
    property    ButtonRows : integer read FButtonRows write FButtonRows;
  end;

  TG2GraphButtonIncDec = class( TG2GraphButton)
  private
    FButtonPressed : integer;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   SetOrientation( aValue : TOrientationType); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetBounds(ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  end;

  TG2GraphDropDownList = class( TGraphicControl)
  private
    FPartSelector  : TG2GraphPartSelector;
    FMouseOverItem : integer;
  protected
    procedure   Paint; override;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
  end;

  TG2GraphPartSelector = class( TG2GraphChildControl)
  private
    FButton      : TG2GraphButton;
    FImageList   : TG2ImageList;
    FImageWidth  : integer;
    FImageCount  : integer;
    FButtonRect  : TRect;
    FButtonWidth : integer;
  protected
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   DropDown( Sender: TObject);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   SetBounds(ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
    property    ImageList : TG2ImageList read FImageList;
  end;

  TG2GraphKnob = class( TG2GraphChildControl)
  private
    FOrientation      : TOrientationType;
    FCenterButtonSize,
    FSliderSize       : integer;
    FSliderSelected   : boolean;
    FKnobRect,
    FCenterButtonRect,
    FIncBtnRect,
    FDecBtnRect       : TRect;
    FKnobRad          : integer;
    FType             : TKnobType;
    FStartX,
    FStartY           : integer;
    FHighlightColor   : TColor;
    procedure   SetOrientation( aValue : TOrientationType);
  protected
    procedure   MouseDown( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp( Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove( Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   SetKnobType( aValue : TKnobType);
    procedure   SetHighLightColor( aValue : TColor);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    function    GetSliderRect: TRect;
    function    GetMorphRect: TRect;
    procedure   SetBounds(ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;
  published
    property    KnobType : TKnobType read FType write SetKnobType;
    property    Orientation : TOrientationType read FOrientation write SetOrientation;
    property    HightlightColor : TColor read FHighlightColor write SetHighlightColor;
  end;

  TG2GraphConnector = class( TG2GraphChildControl)
  protected
    FNewCable : TG2GraphCable;
    FData     : TG2FileConnector;
    procedure   MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure   SetData( aConnectorData : TG2FileConnector);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect); override;
    procedure   SetBounds( ALeft: Integer; ATop: Integer;  AWidth: Integer; AHeight: Integer); override;
    procedure   ParsePanelData( fs : TModuleDefStream); override;
    function    ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean; override;

    property    Data : TG2FileConnector read FData write SetData;
  end;

  TCableElement = class
  public
    x           : single;
    y           : single;
    vx          : single;
    vy          : single;
    Left        : integer;
    Top         : integer;
    RelLeft     : integer; // Relative to Cable
    RelTop      : integer; // Relative to Cable
    Width       : integer;
    Height      : integer;
    BoundsRect  : TRect;
    ClientRect  : TRect;
    p           : array[0..3] of TPoint;
    p1, p2      : TPoint; // defining the midline
    Color       : TColor;
    ShadowColor : TColor;
    constructor Create;
    destructor  Destroy; override;
    procedure   Paint;
    procedure   PaintOn( ExtBitmap : TBitmap; ExtBoundsRect : TRect);
  end;

  TG2GraphCable = class( TG2FileCable)
  // http://www.charlespetzold.com/blog/2008/11/Simple-Cable-Simulation.html
  // http://lab.andre-michelle.com/cable-interface
  protected
    FParent          : TWinControl;
    FModule          : TG2GraphModulePanel;
    FNode            : array[ 0.. NCABLE_ELEMENTS] of TCableElement;
    Fx1, Fy1,
    Fx2, Fy2         : integer;
    FSelected        : boolean;
    min_x, min_y, max_x, max_y : integer;
    FLeft : integer;
    FTop : integer;
    FWidth : integer;
    FHeight : integer;
    //FInvalidate : boolean;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   InitCable;
    procedure   PaintElements;
    procedure   ConnectorMoved; override;
    function    CableResize( ax1, ay1, ax2, ay2 : integer): single;
    procedure   Invalidate; override;
    function    GetLeft : integer;
    function    GetTop : integer;
    procedure   SetLeft( aValue : integer);
    procedure   SetTop( aValue : integer);
    function    GetScrollBarX : integer;
    function    GetScrollBarY : integer;
    function    GetBoundsRect: TRect;
    function    GetClientRect : TRect;

    property    x1 : integer read Fx1 write Fx1;
    property    y1 : integer read Fy1 write Fy1;
    property    x2 : integer read Fx2 write Fx2;
    property    y2 : integer read Fy2 write Fy2;
    property    Left : integer read GetLeft write SetLeft;
    property    Top : integer read GetTop write SetTop;
    property    Width : integer read FWidth write FWidth;
    property    Height : integer read FHeight write FHeight;
    property    ScrollBarX : integer read GetScrollBarX;
    property    ScrollBarY : integer read GetScrollBarY;
    property    Parent : TWinControl read FParent write FParent;
    property    BoundsRect : TRect read GetBoundsRect;
    property    ClientRect : TRect read GetClientRect;
  end;

// TODO : not allowed when VST
var
  FDropDownList : TG2GraphDropDownList;

procedure Register;

implementation

function GetConnector( Parent : TWinControl): TG2GraphConnector; // TODO : put in scrollbox
var sr : TRect;
    i, j : integer;
    Module : TG2GraphModulePanel;
begin
  Result := nil;
  for i := 0 to Parent.ControlCount - 1 do
    if Parent.Controls[i] is TG2GraphModulePanel then begin
      Module := Parent.Controls[i] as TG2GraphModulePanel;
      if Module.Visible then begin

        for j := 0 to Module.ChildControlsCount - 1 do begin
          if Module.GraphChildControls[j] is TG2GraphConnector then begin

            sr.Left := Module.ClientToScreen(Point(Module.GraphChildControls[j].Left,
                                                   Module.GraphChildControls[j].Top)).X;
            sr.Top := Module.ClientToScreen(Point(Module.GraphChildControls[j].Left,
                                                   Module.GraphChildControls[j].Top)).Y;
            sr.Right := Module.ClientToScreen(Point(Module.GraphChildControls[j].Left + Module.GraphChildControls[j].Width,
                                                    Module.GraphChildControls[j].Top + Module.GraphChildControls[j].Height)).X;
            sr.Bottom := Module.ClientToScreen(Point(Module.GraphChildControls[j].Left + Module.GraphChildControls[j].Width,
                                                    Module.GraphChildControls[j].Top + Module.GraphChildControls[j].Height)).Y;

            if PtInRect(sr,Mouse.CursorPos) then begin
              Result := Module.GraphChildControls[j] as TG2GraphConnector;
              break;
            end;
          end;
        end;
      end;
    end;
end;

procedure DrawMidiAwareBox( aCanvas : TCanvas; aRect : TRect; aMidiEditorAssignment : TMidiEditorAssignment);
var OldFontSize : integer;
begin
  if not(assigned(aCanvas) and assigned(aCanvas.Font)) then
    exit;

  OldFontSize := aCanvas.Font.Size;
  try
    aCanvas.Font.Size := 6;
    if assigned(aMidiEditorAssignment) then begin
      if aMidiEditorAssignment.Note > 0 then begin
        aCanvas.Brush.Color := CL_MIDI_AWARE_BOX;
        aCanvas.FillRect( aRect);
        TextCenter( aCanvas, aRect, 'N' + IntToStr(aMidiEditorAssignment.Note));
      end;
      if aMidiEditorAssignment.CC > 0 then begin
        aCanvas.Brush.Color := CL_MIDI_AWARE_BOX;
        aCanvas.FillRect( aRect);
        TextCenter( aCanvas, aRect, 'C' + IntToStr(aMidiEditorAssignment.CC));
      end;
    end else begin
      // Just draw empty box
      aCanvas.Brush.Color := CL_MIDI_AWARE_BOX;
      aCanvas.FillRect( aRect);
    end;
  finally
    aCanvas.Font.Size := OldFontSize;
  end;
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

procedure TG2ImageList.ParseImageData( ImageCount : integer; Hex : boolean);
var Bitmap : TBitmap;
    i, j, k, b : integer;
{$IFDEF FPC}
    TempIntfImg: TLazIntfImage;
    CurColor, TransparentColor: TFPColor;
{$ELSE}
    LNew : TRGBTriple;
    LScan : PRGBTripleArray;
{$ENDIF}
begin
  if (FImageData.Count = 0) or (FBitmapWidth = 0) then
    exit;

  b := 0;
  for i := 0 to ImageCount - 1 do begin
    Bitmap := TBitmap.Create;

    Bitmap.Height := FImageData.Count div FBitmapWidth div ImageCount;
    Bitmap.Width :=  FBitmapWidth;
    Bitmap.Pixelformat := pf24bit;

{$IFDEF FPC}
    TempIntfImg := TLazIntfImage.Create(0,0);
    try
      TempIntfImg.LoadFromBitmap( Bitmap.Handle, Bitmap.MaskHandle);

      for j := 0 to Bitmap.Height - 1 do begin
        for k := 0 to Bitmap.Width - 1 do begin
          if Hex then begin
            CurColor.Blue  := 256 * (16 * HexToByte( FImageData[b][1]) + 1 * HexToByte( FImageData[b][2]));
            CurColor.Green := 256 * (16 * HexToByte( FImageData[b][3]) + 1 * HexToByte( FImageData[b][4]));
            CurColor.Red   := 256 * (16 * HexToByte( FImageData[b][5]) + 1 * HexToByte( FImageData[b][6]));
          end else begin
            CurColor.Blue  := StrToInt(copy( FImageData[b], 1, 3));
            CurColor.Green := StrToInt(copy( FImageData[b], 4, 3));
            CurColor.Red   := StrToInt(copy( FImageData[b], 7, 3));
          end;
          if (j = 0) and (k = 0) then begin
            TransparentColor := CurColor;
          end;
          TempIntfImg.Colors[k,j] := CurColor;
          inc(b);
        end;
      end;
      Bitmap.LoadFromIntfImage(TempIntfImg);
      Bitmap.TransparentColor := FPColorToTColor( TransparentColor);
      Bitmap.Transparent := True;
    finally
      TempIntfImg.Free;
    end;
{$ELSE}
    Bitmap.TransparentMode := tmFixed;
    Bitmap.Transparent := True;

    for j := 0 to Bitmap.Height - 1 do begin
      LScan := Bitmap.Scanline[j];
      for k := 0 to Bitmap.Width - 1 do begin
        if Hex then begin
          LNew.rgbtBlue  := 16 * HexToByte( FImageData[b][1]) + 1 * HexToByte( FImageData[b][2]);
          LNew.rgbtGreen := 16 * HexToByte( FImageData[b][3]) + 1 * HexToByte( FImageData[b][4]);
          LNew.rgbtRed   := 16 * HexToByte( FImageData[b][5]) + 1 * HexToByte( FImageData[b][6]);
        end else begin
          LNew.rgbtBlue  := StrToInt(copy( FImageData[b], 1, 3));
          LNew.rgbtGreen := StrToInt(copy( FImageData[b], 4, 3));
          LNew.rgbtRed   := StrToInt(copy( FImageData[b], 7, 3));
        end;
        if (j = 0) and (k = 0) then begin
          Bitmap.TransparentColor := (LNew.rgbtBlue * 256 + LNew.rgbtGreen) * 256 + LNew.rgbtRed;
        end;

        LScan[k] := LNew;
        inc(b);
      end;
    end;
{$ENDIF}

    Add( Bitmap);
  end;
  FImageData.Clear;
end;

// ==== TG2GraphPanel ==========================================================

constructor TG2GraphPanel.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);

  DoubleBuffered := False;
  FExtBitmap := TBitmap.Create;
  FExtBitMap.PixelFormat := pf24Bit;
end;

destructor TG2GraphPanel.Destroy;
begin
  FExtBitmap.Free;
  inherited;
end;

procedure TG2GraphPanel.WMPaint(var Msg: TWMPaint);
var PS        : TPaintStruct;
    i         : integer;
    Rect      : TRect;
    Control   : TG2GraphChildControl;
begin
  BeginPaint(Handle, PS);

  if (PS.rcPaint.Right - PS.rcPaint.Left) + 1 > FExtBitmap.Width then
    FExtBitmap.Width := PS.rcPaint.Right - PS.rcPaint.Left + 1;
  if (PS.rcPaint.Bottom - PS.rcPaint.Top) + 1 > FExtBitmap.Height then
    FExtBitmap.Height := PS.rcPaint.Bottom - PS.rcPaint.Top + 1;

  Rect := SubRect( ClientRect, PS.rcPaint);

  FExtBitmap.Canvas.Brush.Color := Color;
  FExtBitmap.Canvas.FillRect( Rect);
  DrawBevel( FExtBitmap.Canvas, Rect, bvRaised);
  FExtBitmap.Canvas.Pen.Color := Color;

  for i := 0 to ControlCount - 1 do begin
    if Controls[i] is TG2GraphChildControl then begin
      Control := Controls[i] as TG2GraphChildControl;
      if RectOverlap( Control.BoundsRect, PS.rcPaint) then
        Control.PaintOn( FExtBitmap.Canvas, PS.rcPaint);
      //Msg.Result := 0;
    end;
  end;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  Canvas.Lock;
  try
    Canvas.CopyRect( PS.rcPaint, FExtBitmap.Canvas, Rect);
  finally
    Canvas.UnLock;
  end;

  EndPaint(Handle, PS);
end;

procedure TG2GraphPanel.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1; // Do not
end;

  // ==== TG2GraphScrollbox ======================================================

constructor TG2GraphScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBackgroundColor := clWindow;
  FRackColor := clGray;

  FExtBitmap := TBitmap.Create;
  FExtBitMap.PixelFormat := pf24Bit;

  FRackBitmap := TBitmap.Create;
  FRackBitmap.Pixelformat := pf24bit;
  FRackBitmap.Width := UNITS_COL;
  FRackBitmap.Height := UNITS_ROW;
  FBackBitmap := TBitmap.Create;

{$IFDEF FPC}
  FCanvas := Canvas;
{$ELSE}
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
{$ENDIF}

  BuildRackBitmap;
  FBackBitmap.Assign( FRackBitmap);

  FSelectRectActive := False;
  FOutlineVisible := False;
end;

destructor TG2GraphScrollBox.Destroy;
var i : integer;
begin
  // Let the patch free the modules
  i := 0;
  while (i < ControlCount) do begin
    if Controls[i] is TG2GraphModulePanel then
      Controls[i].Parent := nil
    else
      inc(i);
  end;

  FBackBitmap.Free;
  FRackBitmap.Free;
  FExtBitmap.Free;
{$IFNDEF FPC}
  FCanvas.Free;
{$ENDIF}
  inherited;
end;

procedure TG2GraphScrollBox.DrawLine(X1, Y1, X2, Y2: integer);
begin
  FCanvas.Pen.Mode := pmXor;
  FCanvas.Pen.Color := clWhite;
  FCanvas.Pen.Width := 3;

  FCanvas.MoveTo( X1, Y1);
  FCanvas.LineTo( X2, Y2);
end;

procedure TG2GraphScrollBox.DrawOutlineCanvas( ExtCanvas : TCanvas; ExtBoundsRect: TRect; Rect: TRect);
var i : integer;
begin
  try
    ExtCanvas.Pen.Mode := pmXor;
    ExtCanvas.Pen.Color := clWhite;
    ExtCanvas.Pen.Width := 1;
    Rect := SubRect( Rect, ExtBoundsRect);
    for i := 0 to 3 do begin
      ExtCanvas.MoveTo( Rect.Left + i,  Rect.Top + i);
      ExtCanvas.LineTo( Rect.Left + i,  Rect.Bottom - i);
      ExtCanvas.LineTo( Rect.Right - i, Rect.Bottom - i);
      ExtCanvas.LineTo( Rect.Right - i, Rect.Top + i);
      ExtCanvas.LineTo( Rect.Left + i,  Rect.Top + i);
    end;
  finally
    ExtCanvas.Pen.Mode := pmCopy;
  end;
end;

procedure TG2GraphScrollBox.DrawCopyPatchOutlines( ExtCanvas : TCanvas; ExtBoundsRect: TRect);
var i : integer;
    Module : TG2GraphModule;
    Rect : TRect;
begin
  if assigned(FCopyPatch) then
    for i := 0 to FCopyPatch.ModuleList.Count - 1 do begin
      Module := FCopyPatch.ModuleList[i] as TG2GraphModule;
      Rect := Module.FOutlineRect;
      Rect.Left := Rect.Left + FStartX;
      Rect.Top := Rect.Top + FStartY;
      Rect.Right := Rect.Right + FStartX;
      Rect.Bottom := Rect.Bottom + FStartY;
      DrawOutlineCanvas( ExtCanvas, ExtBoundsRect, Rect);
    end;
end;

procedure TG2GraphScrollBox.DrawOutline( Rect: TRect);
begin
  DrawOutlineCanvas( FCanvas, ClientRect, Rect);
end;

procedure TG2GraphScrollBox.MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
begin
  if assigned( FDropDownList) then begin
    FDropDownList.Free;
    FDropDownList := nil;
  end;

  if assigned(FG2Graph) then begin
    FG2Graph.GetSelectedPatch.UnselectModules(ltFX);
    FG2Graph.GetSelectedPatch.UnselectModules(ltVA);
  end;

  FOutlineRect.Left := X;
  FOutlineRect.Right := X;
  FOutLineRect.Top := Y;
  FOutLineRect.Bottom := Y;

  inherited;
end;

procedure TG2GraphScrollBox.MouseEnter(var Msg: TMessage);
begin
  if not FCopyOutLinesVisible then begin
    DrawCopyPatchOutlines( FCanvas, ClientRect);
    FCopyOutLinesVisible := True;
  end;

  if FSelectRectActive and (not FOutlineVisible) then begin
    DrawOutline( FOutLineRect);
    FOutlineVisible := True;
  end;

  inherited;
end;

procedure TG2GraphScrollBox.MouseLeave(var Msg: TMessage);
begin
  if FCopyOutLinesVisible then begin
    DrawCopyPatchOutlines( FCanvas, ClientRect);
    FCopyOutLinesVisible := False;
  end;

  if FSelectRectActive and FOutlineVisible then begin
    DrawOutline( FOutLineRect);
    FOutlineVisible := False;
  end;

  inherited;
end;

procedure TG2GraphScrollBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if assigned( FCopyPatch) then begin
    if FCopyOutLinesVisible then begin
      DrawCopyPatchOutlines( FCanvas, ClientRect);
      FCopyOutLinesVisible := False;
    end;

    FSTartX := X;
    FStartY := Y;

    if not FCopyOutLinesVisible then begin
      DrawCopyPatchOutlines( FCanvas, ClientRect);
      FCopyOutLinesVisible := True;
    end;
  end else begin
    if ssLeft in Shift then begin

      FSelectRectActive := True;

      if FOutlineVisible then begin
        DrawOutline( FOutlineRect);
        FOutlineVisible := False;
      end;

      FOutlineRect.Right := X;
      FOutlineRect.Bottom := Y;

      if not FOutlineVisible then begin
        DrawOutline( FOutlineRect);
        FOutlineVisible := True;
      end;
    end;
  end;

  inherited;
end;

procedure TG2GraphScrollBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if assigned( FCopyPatch) then begin
    if FCopyOutLinesVisible then begin
      DrawCopyPatchOutlines( FCanvas, ClientRect);
      FCopyOutLinesVisible := False;
    end;
  end else begin
    if FSelectRectActive then begin
      if FOutlineVisible then begin
        DrawOutline( FOutlineRect);
        FOutlineVisible := False;
      end;
      FSelectRectActive := False;
      if assigned(FG2Graph) then
        FG2Graph.GetSelectedPatch.SelectModulesInRect( FLocation, FOutlineRect);
    end;
  end;

  inherited;
end;

procedure TG2GraphScrollBox.SetBitmap( aValue: TBitmap);
begin
  FBackBitmap.Assign( aValue);
  invalidate;
end;

procedure TG2GraphScrollBox.SetCopyPatch( aValue: TG2FilePatchPart);
var P : TPoint;
    i, MinX, MinY : integer;
    Module : TG2GraphModule;
begin
  //if assigned(FCopyPatch) then begin
    if FCopyOutLinesVisible then begin
      DrawCopyPatchOutlines( FCanvas, ClientRect);
      FCopyOutLinesVisible := False;
    end;
  //end;

  if assigned(aValue) then begin

    FCopyPatch := aValue;

    MinX := 1000;
    MinY := 1000;
    for i := 0 to FCopyPatch.ModuleList.Count - 1 do begin
      Module := FCopyPatch.ModuleList[i] as TG2GraphModule;
      if Module.FOutlineRect.Left < MinX then
        MinX := Module.FOutlineRect.Left;
      if Module.FOutlineRect.Top < MinY then
        MinY := Module.FOutlineRect.Top;
    end;

    for i := 0 to FCopyPatch.ModuleList.Count - 1 do begin
      Module := FCopyPatch.ModuleList[i] as TG2GraphModule;
      Module.FOutlineRect.Left :=  Module.FOutlineRect.Left - MinX;
      Module.FOutlineRect.Top := Module.FOutlineRect.Top - MinY;
      Module.FOutlineRect.Right :=  Module.FOutlineRect.Right - MinX;
      Module.FOutlineRect.Bottom := Module.FOutlineRect.Bottom - MinY;
    end;

    GetCursorPos( P);
    FStartX := ScreenToClient( P).X;
    FStartY := ScreenToClient( P).Y;

    if (FStartX >= 0) and (FStartX < Width) and (FStartY>=0) and (FStartY< Height) then begin
      if not FCopyOutLinesVisible then begin
        DrawCopyPatchOutlines( FCanvas, ClientRect);
        FCopyOutLinesVisible := True;
      end;
    end else
      FCopyOutLinesVisible := False;

  end else begin
    if FCopyOutLinesVisible then begin
      DrawCopyPatchOutlines( FCanvas, ClientRect);
      FCopyOutLinesVisible := False;
    end;
    FCopyPatch := aValue;
  end;
end;

procedure TG2GraphScrollBox.SetPositionsInCopyPatch;
var Module : TG2GraphModule;
    i : integer;
begin
  if assigned(FCopyPatch) then
    for i := 0 to FCopyPatch.ModuleList.Count - 1 do begin
      Module := FCopyPatch.ModuleList[i] as TG2GraphModule;
      Module.NewRow := (VertScrollbar.Position + FStartY + trunc(Module.FOutlineRect.Top)) div UNITS_ROW;
      Module.NewCol := (HorzScrollbar.Position + FStartX + trunc(Module.FOutlineRect.Left)) div UNITS_COL;
    end;
end;

function TG2GraphScrollBox.GetPatchCoord( ClientPoint : TPoint): TPoint;
begin
  Result.X := ClientPoint.X + HorzScrollbar.Position;
  Result.Y := ClientPoint.Y + VertScrollbar.Position;
end;

procedure TG2GraphScrollBox.SetRackColor( aValue: TColor);
begin
  FRackColor := aValue;
  BuildRackBitmap;
  FBackBitmap.Assign( FRackBitmap);
  Invalidate;
end;

procedure TG2GraphScrollBox.SetBackgroundColor( aValue: TColor);
begin
  FBackgroundColor := aValue;
  BuildRackBitmap;
  FBackBitmap.Assign( FRackBitmap);
  Invalidate;
end;

{$IFDEF FPC}
procedure TG2GraphScrollBox.Paint;
var dummy, i  : integer;
    FPatch    : TG2GraphPatch;
    Module    : TG2GraphModulePanel;
    DropDown  : TG2GraphDropDownList;
    ClipRect, Rect : TRect;
begin
  ClipRect := Canvas.ClipRect;

  if assigned(FG2Graph) then
    FPatch := FG2Graph.GetSelectedPatch
  else
    FPatch := nil;

  // Make the offscreen bitmap bigger if necessary
  if ClipRect.Right - ClipRect.Left > FExtBitmap.Width then
    FExtBitmap.Width := ClipRect.Right - ClipRect.Left;
  if ClipRect.Bottom - ClipRect.Top > FExtBitmap.Height then
    FExtBitmap.Height := ClipRect.Bottom - ClipRect.Top;

  PaintBackGround( FExtBitmap.Canvas, ClipRect);

  if assigned(FPatch) and FPatch.Visible then
    for i := 0 to ControlCount - 1 do begin
      if Controls[i] is TG2GraphModulePanel then begin
        Module := Controls[i] as TG2GraphModulePanel;
        if (Module.FPatch = FPatch) and (Module.Location = FLocation) then
          if RectOverlap( Module.BoundsRect, ClipRect) then
            Module.PaintOn( FExtBitmap.Canvas, ClipRect);
      end;
      if Controls[i] is TG2GraphDropDownList then begin
        DropDown := Controls[i] as TG2GraphDropDownList;
        Module := DropDown.FPartSelector.FModule;
        if (Module.FPatch = FPatch) and (Module.Location = FLocation) then
          if RectOverlap( DropDown.BoundsRect, ClipRect) then
            DropDown.PaintOn( FExtBitmap.Canvas, ClipRect);
      end;
    end;

  PaintCables( FExtBitmap, ClipRect);

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := ClipRect.Right - ClipRect.Left;
  Rect.Bottom := ClipRect.Bottom - ClipRect.Top;

  FCanvas.CopyRect( ClipRect, FExtBitmap.Canvas, Rect);
end;

{$ELSE}
procedure TG2GraphScrollBox.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1;
end;

procedure TG2GraphScrollBox.WMPaint(var Msg: TWMPaint);
//http://catch22.net/tuts/flicker
var
  PS          : TPaintStruct;
  i           : integer;
  FPatch      : TG2GraphPatch;
  FPatchPart  : TG2FilePatchPart;
  Module      : TG2GraphModule;
  ModulePanel : TG2GraphModulePanel;
  DropDown    : TG2GraphDropDownList;
  Rect        : TRect;
begin
  //AddLogLine('WMPaint ================================================== ');

  // The windows message contains the area of the scrollbox that has to
  // be redrawn, that is passed to the procedure in de windows message :
  // TPaintStruct.rcPaint

  // To keep this rectange as small as possible following method is used:
  // - when a controls has to be redrawn InvalidateRect of the control is send
  // - then the Update method of the parent is called.
  // That way the control is redrawn immediately.

  // When Update isn't called, but only invalidate, windows will combine paint messages.
  // If the area's are wide apart, this can result in a big area to be redrawn.
  // This is especially important for the led controls!

  // There are 3 layers to be drawn:
  // 1 : background
  // 2 : Modules and on top of the modules the controls
  // 3 : the cables

  if assigned(FG2Graph) then
    FPatch := FG2Graph.GetSelectedPatch
  else
    FPatch := nil;

  // PS.rcPaint is the arean that needs to be repainted
  BeginPaint(Handle, PS);

  //AddLogLine('Update region ' + IntToStr(PS.rcPaint.Left) + ' ' + IntToStr(PS.rcPaint.Top) + ' ' + IntToStr(PS.rcPaint.Right) + ' ' + IntToStr(PS.rcPaint.Bottom));

  // Make the offscreen bitmap bigger if necessary
  if PS.rcPaint.Right - PS.rcPaint.Left > FExtBitmap.Width then
    FExtBitmap.Width := PS.rcPaint.Right - PS.rcPaint.Left;
  if PS.rcPaint.Bottom - PS.rcPaint.Top > FExtBitmap.Height then
    FExtBitmap.Height := PS.rcPaint.Bottom - PS.rcPaint.Top;

  PaintBackGround( FExtBitmap.Canvas, PS.rcPaint);

  if assigned(FPatch) and FPatch.Visible then
    FPatchPart := FPatch.PatchPart[ ord(FLocation)];

    if assigned(FG2Graph)  then
      FG2Graph.Lock;
    try

    for i := 0 to ControlCount - 1 do begin
      if Controls[i] is TG2GraphModulePanel then begin
        ModulePanel := (Controls[i] as TG2GraphModulePanel);
        Module := ModulePanel.FData;
        if (Module.PatchPart = FPatchPart) and (Module.Location = FLocation) then
          if RectOverlap( ModulePanel.BoundsRect, PS.rcPaint) then
            ModulePanel.PaintOn( FExtBitmap.Canvas, PS.rcPaint);
      end;
      if Controls[i] is TG2GraphDropDownList then begin
        DropDown := Controls[i] as TG2GraphDropDownList;
        ModulePanel := DropDown.FPartSelector.FModule;
        Module := ModulePanel.FData;
        if (Module.PatchPart = FPatchPart) and (Module.Location = FLocation) then
          if RectOverlap( DropDown.BoundsRect, PS.rcPaint) then
            DropDown.PaintOn( FExtBitmap.Canvas, PS.rcPaint);
      end;
    end;
    finally
      if assigned(FG2Graph)  then
        FG2Graph.UnLock;
    end;

  PaintCables( FExtBitmap, PS.rcPaint);

  // Draw outlines of modules that are being dragged
  if assigned(FPatch) and FPatch.Visible then
    for i := 0 to ControlCount - 1 do begin
      if Controls[i] is TG2GraphModulePanel then begin
        ModulePanel := (Controls[i] as TG2GraphModulePanel);
        Module := ModulePanel.FData;
        if Module.FOutlineVisible then
          DrawOutlineCanvas( FExtBitmap.Canvas, PS.rcPaint, Module.FOutlineRect);
      end;
    end;

  // Draw outlines of modules that are being pasted
  if assigned(FCopyPatch) and FCopyOutLinesVisible then begin
    DrawCopyPatchOutlines( FExtBitmap.Canvas, PS.rcPaint);
  end;

  // Draw dragging rect
  if FSelectRectActive then begin
    DrawOutlineCanvas( FExtBitmap.Canvas, PS.rcPaint, FOutlineRect);
  end;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  FCanvas.CopyRect( PS.rcPaint, FExtBitmap.Canvas, Rect);

  EndPaint(Handle, PS);

  Msg.Result := 0;
end;
{$ENDIF}

procedure TG2GraphScrollBox.PaintBackGround( ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var FDrawHeight, FDrawWidth,
    Row, Column, xl, xt, xw, xh,
    xoffs, yoffs: Integer;
    Bitmap : TBitmap;
begin
  if assigned(FBackBitmap) and (FBackBitmap.Width <> 0) then
    Bitmap := FBackBitmap
  else
    Bitmap := FRackBitmap;

  if (Bitmap.width <> 0) and (Bitmap.Height <> 0) then begin

    FDrawHeight := ExtBoundsRect.Bottom - ExtBoundsRect.Top;
    FDrawWidth := ExtBoundsRect.Right - ExtBoundsRect.Left;

    xoffs := ExtBoundsRect.Left - ((ExtBoundsRect.Left + HorzScrollBar.Position) mod Bitmap.Width);
    yoffs := ExtBoundsRect.Top -  ((ExtBoundsRect.Top  + VertScrollBar.Position) mod Bitmap.Height);

    for Row := 0 to (FDrawHeight div Bitmap.Height) + 1 do begin
      for Column := 0 to (FDrawWidth div Bitmap.Width) + 1 do begin

        xl := xoffs + Column * Bitmap.Width - ExtBoundsRect.Left;
        xt := yoffs + Row * Bitmap.Height - ExtBoundsRect.Top;

        xw := Bitmap.Width;
        xh := Bitmap.Height;

        ExtCanvas.CopyRect(
          Rect(xl, xt, xl + xw, xt + xh),
          Bitmap.Canvas,
          Rect(0, 0, xw, xh));
      end;
    end;
  end;
end;

procedure TG2GraphScrollBox.PaintCables(ExtBitmap: TBitmap; ExtBoundsRect: TRect);
var Cable : TG2GraphCable;
    i, j : integer;
    FPatch : TG2GraphPatch;
begin
  if not assigned( FG2Graph) then
    exit;

  FPatch := FG2Graph.GetSelectedPatch as TG2GraphPatch;

  ExtBoundsRect.Left := ExtBoundsRect.Left + HorzScrollBar.Position;
  ExtBoundsRect.Top := ExtBoundsRect.Top + VertScrollBar.Position;
  ExtBoundsRect.Right := ExtBoundsRect.Right + HorzScrollBar.Position;
  ExtBoundsRect.Bottom := ExtBoundsRect.Bottom + VertScrollBar.Position;

  // Paint cable segments

  if FPatch.Visible then
    for i := 0 to FPatch.CableList[ord(FLocation)].Count - 1 do begin
      Cable := FPatch.CableList[ord(FLocation)].Items[i] as TG2GraphCable;

      if ((Cable.CableColor = COLOR_RED) and (FPatch.PatchDescription.RedVisible = 1)) or
         ((Cable.CableColor = COLOR_BLUE) and (FPatch.PatchDescription.BlueVisible = 1)) or
         ((Cable.CableColor = COLOR_YELLOW) and (FPatch.PatchDescription.YellowVisible = 1)) or
         ((Cable.CableColor = COLOR_ORANGE) and (FPatch.PatchDescription.OrangeVisible = 1)) or
         ((Cable.CableColor = COLOR_GREEN) and (FPatch.PatchDescription.GreenVisible = 1)) or
         ((Cable.CableColor = COLOR_PURPLE) and (FPatch.PatchDescription.PurpleVisible = 1)) or
         ((Cable.CableColor = COLOR_WHITE) and (FPatch.PatchDescription.WhiteVisible = 1)) then

        if RectOverlap( Cable.BoundsRect, ExtBoundsRect) then begin
          for j := 1 to NCABLE_ELEMENTS - 1 do begin

            if RectOverlap( Cable.FNode[j].BoundsRect, ExtBoundsRect) then
              Cable.FNode[j].PaintOn( ExtBitmap, ExtBoundsRect);
          end;
        end;
    end;
end;

procedure TG2GraphScrollBox.BuildRackBitmap;
var Rect : TRect;
begin
  // Rack rails!

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := FRackBitmap.Width;
  Rect.Bottom := FRackBitmap.Height;

  FRackBitmap.Canvas.Brush.Color := FRackColor;
  FRackBitmap.Canvas.Fillrect(Rect);

  FRackBitmap.Canvas.Pen.Color := Lighter(FRackColor, 30);
  FRackBitmap.Canvas.MoveTo(1, 0);
  FRackBitmap.Canvas.LineTo(1, FRackBitMap.Height);
  FRackBitmap.Canvas.Pen.Color := Darker(FRackColor, 50);
  FRackBitmap.Canvas.MoveTo(FRackBitmap.Width - 1, 0);
  FRackBitmap.Canvas.LineTo(FRackBitmap.Width - 1, FRackBitMap.Height);

  Rect.Left := Rect.Left + 20;
  Rect.Right := Rect.Right - 20;

  FRackBitmap.Canvas.Brush.Color := FBackgroundColor;
  FRackBitmap.Canvas.Fillrect(Rect);

  DrawDisk( FRackBitmap,  10, FRackBitmap.Height div 2, 5, 1, FBackgroundColor, bsSolid);
  DrawDisk( FRackBitmap, FRackBitmap.Width - 10, FRackBitmap.Height div 2, 5, 1, FBackgroundColor, bsSolid);

  FRackBitmap.Canvas.Pen.Color := clBlack;
  FRackBitmap.Canvas.MoveTo( 0, 0);
  FRackBitmap.Canvas.LineTo( 0, FRackBitmap.Height);
  FRackBitmap.Canvas.Pen.Color := clBlack;
  FRackBitmap.Canvas.MoveTo( FRackBitmap.Width, 0);
  FRackBitmap.Canvas.LineTo( FRackBitmap.Width, FRackBitmap.Height);
end;

// ==== TG2Graph ===============================================================

constructor TG2Graph.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2Graph.Destroy;
begin
  ScrollboxFX := nil;
  ScrollboxVA := nil;
  inherited;
end;

procedure TG2Graph.SetModuleParent( aLocation : TLocationType; aValue: TG2GraphScrollbox);
var p, m : integer;
    Slot : TG2FileSlot;
    Patch : TG2GraphPatch;
    ModuleList : TModuleList;
    Module : TG2GraphModule;
begin
  if not assigned(Performance) then
    exit;

  for p := 0 to 3 do begin
    Slot := Performance.Slot[ p];
    if assigned(Slot) and assigned(Slot.Patch) then begin
      Patch := Slot.Patch as TG2GraphPatch;
      if assigned(Patch) then begin

        if aValue = nil then begin

          //for l := 0 to 1 do begin
            ModuleList := Patch.ModuleList[ord(aLocation)];
            for m := 0 to ModuleList.Count - 1 do begin
              Module := ModuleList[m] as TG2GraphModule;
              if assigned(Module) then begin
                Module.Parent := aValue;
                Module.Visible := True;
              end;
            end;
          //end;

        end else begin
          ModuleList := Patch.ModuleList[ord(aValue.Location)];
          for m := 0 to ModuleList.Count - 1 do begin
            Module := ModuleList[m] as TG2GraphModule;
            if assigned(Module) then begin
              Module.Parent := aValue;
              Module.Visible := True;
            end;
          end;
        end;

      end;
    end;
  end;
end;

procedure TG2Graph.SetScrollboxFX( aValue: TG2GraphScrollbox);
begin
  if not assigned(aValue) then begin
    if assigned( FScrollboxFX) and assigned( FScrollboxFX.G2) then begin
      FScrollboxFX.G2.SetModuleParent( ltFX, nil);
      FScrollboxFX.G2 := nil;
    end;
    FScrollboxFX := aValue;
  end else begin
    if assigned( aValue) and assigned( aValue.G2) then begin
      aValue.G2.SetModuleParent( ltFX, nil);
    end;
    FScrollboxFX := aValue;
    FScrollboxFX.G2 := self;
    FScrollboxFX.Location := ltFX;
    SetModuleParent( ltFX, FScrollboxFX);
  end;
end;

procedure TG2Graph.SetScrollboxVA( aValue: TG2GraphScrollbox);
begin
  if not assigned(aValue) then begin
    if assigned( FScrollboxVA) and assigned( FScrollboxVA.G2) then begin
      FScrollboxVA.G2.SetModuleParent( ltVA, nil);
      FScrollboxVA.G2 := nil;
    end;
    FScrollboxVA := aValue;
  end else begin
    if assigned( aValue) and assigned( aValue.G2) then begin
      aValue.G2.SetModuleParent( ltVA, nil);
    end;
    FScrollboxVA := aValue;
    FScrollboxVA.G2 := self;
    FScrollboxVA.Location := ltVA;
    SetModuleParent( ltVA, FScrollboxVA);
  end;
end;

function TG2Graph.GetSelectedPatch: TG2GraphPatch;
begin
  if assigned(Performance) then
    Result := Performance.Slot[ Performance.SelectedSlot].Patch as TG2GraphPatch
  else
    Result := nil;
end;

procedure TG2Graph.Invalidate;
begin
  if assigned(FScrollboxVA) then
    FScrollboxVA.Invalidate;

  if assigned(FScrollboxFX) then
    FScrollboxFX.Invalidate;
end;

function TG2Graph.G2MessageDlg(tekst: string; code: integer): integer;
begin
  case code of
  0 :;
  1 : MessageDlg( tekst, mtError, [mbOK], 0);
  end;
end;

procedure TG2Graph.G2ProcessWindowsMessages;
begin
  Application.ProcessMessages;
end;

// ==== TG2GraphPerformance ====================================================

constructor TG2GraphPerformance.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TG2GraphPerformance.Destroy;
begin
  inherited;
end;

procedure TG2GraphPerformance.InitSelectedSlotIndex(aValue: TBits2);
var i : integer;
begin
  inherited;

  for i := 0 to 3 do
    (Slot[i].Patch as TG2GraphPatch).Visible := (i = aValue);

  if assigned(G2) then
    (G2 as TG2Graph).Invalidate;
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

procedure TG2GraphParameter.AssignControl(aControl: TGraphicControl);
var i : integer;
begin
  if not(aControl is TG2GraphChildControl) then
    raise Exception.Create('Only a TG2GraphChildControl can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if not(i < Length(FControlList)) then begin
    SetLength(FControlList, i + 1);
    FControlList[i] := aControl as TG2GraphChildControl;
  end;
end;

procedure TG2GraphParameter.DeassignControl(aControl: TGraphicControl);
var i, j : integer;
begin
  if not(aControl is TG2GraphChildControl) then
    raise Exception.Create('Only a TG2GraphChildControl can be assigned to a parameter.');

  i := 0;
  while (i < Length(FControlList)) and not(FControlList[i] = aControl) do
    inc(i);

  if (i < Length(FControlList)) then begin
    for j := i + 1 to Length(FControlList) - 1 do begin
      FControlList[j-1] := FControlList[j];
    end;
    SetLength(FControlList, Length(FControlList) - 1);
  end;
end;

procedure TG2GraphParameter.InvalidateControl;
var i, j : integer;
    ModulePanel : TG2GraphModulePanel;
    Display : TG2GraphDisplay;
begin
  // Update all controls attached to the parameter
  for i := 0 to Length(FControlList) - 1 do begin
    FControlList[i].Update;
  end;
end;


{ ==== TMiniVUList =============================================================}

function TMiniVUList.Add(aMiniVU: TG2GraphMiniVU): integer;
begin
  Result := inherited Add( aMiniVU);
end;

function TMiniVUList.GetMiniVU(aindex: integer): TG2GraphMiniVU;
begin
  result := inherited Items[aindex] as TG2GraphMiniVU;
end;

procedure TMiniVUList.SetMiniVU(aindex: integer; const Value: TG2GraphMiniVU);
begin
  inherited Items[aindex] := Value;
end;

{ ==== TLedGroupList ===========================================================}

function TLedGroupList.GetLedGroup(aindex: integer): TG2GraphLedGroup;
begin
  result := inherited Items[aindex] as TG2GraphLedGroup;
end;

procedure TLedGroupList.SetLedGroup(aindex: integer; const Value: TG2GraphLedGroup);
begin
  inherited Items[aindex] := Value;
end;

function TLedGroupList.Add( aLedGroup : TG2GraphLedGroup): integer;
begin
  Result := inherited Add( aLedGroup);
end;

{ ==== FLed39List ==============================================================}

function TLed39List.GetLedGreen(aindex: integer): TG2GraphLedGreen;
begin
  result := inherited Items[aindex] as TG2GraphLedGreen;
end;

procedure TLed39List.SetLedGreen(aindex: integer; const Value: TG2GraphLedGreen);
begin
  inherited Items[aindex] := Value;
end;

function TLed39List.Add( aLedGreen : TG2GraphLedGreen): integer;
begin
  Result := inherited Add( aLedGreen);
end;

function CompareLedGreenOrder( PLed1 : pointer; PLed2 : pointer): integer;
var Led1, Led2 : TG2GraphLedGreen;
begin
  Led1 := TG2GraphLedGreen(PLed1);
  Led2 := TG2GraphLedGreen(PLed2);

  if Led1.Module.Location > Led2.Module.Location then
    Result := -1
  else
    if Led1.Module.Location =  Led2.Module.Location then begin
      if Led1.Module.ModuleIndex > Led2.Module.ModuleIndex then
        Result := 1
      else
        if Led1.Module.ModuleIndex = Led2.Module.ModuleIndex then begin
          if Led1.FGroupId > Led2.FGroupID then
            Result := 1
          else
            if Led1.FGroupId = Led2.FGroupID then begin
              if Led1.FCodeRef > Led2.FCodeRef then
                Result := 1
              else
                if Led1.FCodeRef = Led2.FCodeRef then
                  Result := 0
                else
                  Result := -1;
            end else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;
end;

{ ==== FLed3AList ==============================================================}

function TLed3AList.GetLed(aindex: integer): TG2GraphLed;
begin
  result := inherited Items[aindex] as TG2GraphLed;
end;

procedure TLed3AList.SetLed(aindex: integer; const Value:TG2GraphLed);
begin
  inherited Items[aindex] := Value;
end;

function TLed3AList.Add( aLed : TG2GraphLed): integer;
begin
  Result := inherited Add( aLed);
end;

function CompareMiniVUOrder( PLed1, PLed2: pointer): integer;
var Led1, Led2 : TG2GraphLed;
begin
  Led1 := TG2GraphLed(PLed1);
  Led2 := TG2GraphLed(PLed2);

  if Led1.Module.Location > Led2.Module.Location then
    Result := -1
  else
    if Led1.Module.Location =Led2.Module.Location then begin
      if Led1.Module.ModuleIndex > Led2.Module.ModuleIndex then
        Result := 1
      else
        if Led1.Module.ModuleIndex = Led2.Module.ModuleIndex then begin
          if Led1.FGroupId > Led2.FGroupID then
            Result := 1
          else
            if Led1.FGroupId = Led2.FGroupID then
              Result := 0
            else
              Result := -1;
        end else
          Result := -1;
    end else
      Result := 1;
end;

// ==== TG2GraphPatch ==========================================================

constructor TG2GraphPatch.Create(AOwner: TComponent);
begin
  FMiniVUList := TMiniVUList.Create( False);
  FLedGroupList := TLedGroupList.Create( False);
  FLed39List := TLed39List.Create( False);
  FLed3AList := TLed3AList.Create( False);

  inherited;
end;

procedure TG2GraphPatch.DeleteCableFromPatch(aLocation: TLocationType; aCable: TG2FileCable);
begin
  inherited;

  if (aLocation = ltFX) and assigned(G2) and assigned((G2 as TG2Graph).ScrollboxFX) then
    (G2 as TG2Graph).ScrollboxFX.Invalidate;

  if (aLocation = ltVA) and assigned(G2) and assigned((G2 as TG2Graph).ScrollboxVA) then
    (G2 as TG2Graph).ScrollboxVA.Invalidate;
end;

destructor TG2GraphPatch.Destroy;
begin
  FLed3AList.Free;
  FLed39List.Free;
  FLedGroupList.Free;
  FMiniVUList.Free;

  inherited;
end;

procedure TG2GraphPatch.Init;
begin
  inherited;

  FSelectedControl := nil;

  FLed3AList.Clear;
  FLed39List.Clear;
  FLedGroupList.Clear;
  FMiniVUList.Clear;
end;

function TG2GraphPatch.CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule;
var i : LongWord;
    Module : TG2GraphModule;
begin
  // Create a module in a patch file

  Module := TG2GraphModule.Create( PatchPart[ ord( aLocation)]);
  Module.TypeID := aModuleType;
  Module.ModuleIndex := aModuleIndex;

  if assigned( G2) and assigned(G2.FModuleDefList) and assigned(G2.FParamDefList) then begin
    i := 0;
    while (i < G2.FModuleDefList.Count) and ( G2.FModuleDefList.ModuleDef[i].ModuleType <> aModuleType) do
      inc(i);

    if (i < G2.FModuleDefList.Count) then begin

      Module.InitModule( aLocation, G2.FModuleDefList.ModuleDef[i], G2.FParamDefList);

      if (G2.ClientType <> ctVST) then begin
        if aLocation = ltVA then begin
          Module.Parent := (G2 as TG2Graph).ScrollboxVA;
          Module.ParsePanelData;
        end else
          if aLocation = ltFX then begin
            Module.Parent := (G2 as TG2Graph).ScrollboxFX;
            Module.ParsePanelData;
          end;
      end else
        Module.Parent := nil;

    end else
      raise Exception.Create('Unknown module type ' + IntToStr( aModuleType));;
  end;

  Module.Visible := Visible;
  Module.Location := aLocation;
  Module.Invalidate;

  if assigned(G2) and assigned(G2.OnCreateModule) then
    G2.OnCreateModule(self, G2.ID, Module);

  Result := Module;
end;

function TG2GraphPatch.CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
var FromConnKind : TConnectorKind;
    ModuleFrom, ModuleTo : TG2GraphModule;
    Cable : TG2GraphCable;
    ConnectorFrom, ConnectorTo : TG2GraphConnector;
begin
  // Create a cable connection in a patch file

  ModuleFrom := GetModule( ord(aLocation), aFromModule) as TG2GraphModule;
  if not assigned(ModuleFrom) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aFromModule) + ' not found.');

  ModuleTo := GetModule( ord(aLocation), aToModule) as TG2GraphModule;
  if not assigned(ModuleTo) then
    raise Exception.Create('ModuleIndex ' + IntToStr( aToModule) + ' not found.');

  Cable                := TG2GraphCable.Create( self);
  Cable.CableColor     := aColor;
  Cable.ModuleFrom     := aFromModule;
  Cable.ConnectorFrom  := aFromConnector;
  Cable.LinkType       := aLinkType;
  Cable.ModuleTo       := aToModule;
  Cable.ConnectorTo    := aToConnector;

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
      Cable.Parent := (G2 as TG2Graph).ScrollboxFX
    else
      Cable.Parent := (G2 as TG2Graph).ScrollboxVA;

    // Cable needs scrollbar coords
    ConnectorFrom := Cable.FromConnector.GraphControl as TG2GraphConnector;
    ConnectorTo := Cable.ToConnector.GraphControl as TG2GraphConnector;

    Cable.x1 := ModuleFrom.ScrollPosX +  ConnectorFrom.Left + ConnectorFrom.Width div 2;
    Cable.y1 := ModuleFrom.ScrollPosY +  ConnectorFrom.Top + ConnectorFrom.Height div 2;
    Cable.x2 := ModuleTo.ScrollPosX +  ConnectorTo.Left + ConnectorTo.Width div 2;;
    Cable.y2 := ModuleTo.ScrollPosY +  ConnectorTo.Top + ConnectorTo.Height div 2;
    Cable.InitCable;
    Cable.ConnectorMoved;
  end;

  Result := Cable;
end;

{procedure TG2GraphPatch.SelectModules;
var i, j : integer;
begin
  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do
      if ModuleList[i].Items[j].Selected then
        (ModuleList[i].Items[j] as TG2GraphModule).FPanel.SelectModule;
end;}

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
    if PointInRect( (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Left,
                    (ModuleList[ ord(aLocation)][i] as TG2GraphModule).FPanel.Top,
                    aRect) then
      ModuleList[ ord(aLocation)][i].Selected := True;
  end;
end;

procedure TG2GraphPatch.MoveOutlines( aLocation : TLocationType; dX, dY: integer);
var i : integer;
begin
  {for i := 0 to SelectedModuleList.Count - 1 do begin
    (SelectedModuleList[i] as TG2GraphModule).FPanel.MoveOutline( dX, dY);
  end;}
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveOutline( dX, dY);

end;

function TG2GraphPatch.MessMoveModules( aLocation : TLocationType): boolean;
var i : integer;
begin
  Result := inherited MessMoveModules( aLocation);
  {for i := 0 to SelectedModuleList.Count - 1 do
    (SelectedModuleList[i] as TG2GraphModule).FPanel.MoveModule;}
  for i := 0 to PatchPart[ ord(aLocation)].SelectedModuleList.Count - 1 do
    (PatchPart[ ord(aLocation)].SelectedModuleList[i] as TG2GraphModule).FPanel.MoveModule;
end;

{procedure TG2GraphPatch.UnselectModules( aLocation : TLocationType);
var i : integer;
begin
  for i := 0 to ModuleCount[ord(aLocation)] - 1 do
    if ModuleList[ord(aLocation)].Items[i].Selected then
      (ModuleList[ord(aLocation)].Items[i] as TG2GraphModule).Selected := False;
end;}

function TG2GraphPatch.GetG2 : TG2Graph;
begin
  Result := nil;
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

procedure TG2GraphPatch.SetLedLevel( Index: integer; aValue: byte);
begin
  FLed39List[Index].SetLevel( aValue)
end;

procedure TG2GraphPatch.SetMiniVULevel( Index: integer; aValue: byte);
begin
  FLed3AList[Index].SetLevel( aValue)
end;

procedure TG2GraphPatch.SetSelectedControl( aValue: TG2GraphChildControl);
begin
  if assigned(FSelectedControl) then begin
    if assigned(FSelectedControl.FParameter) then
      FSelectedControl.FParameter.Selected := False;
    FSelectedControl.DeSelect;
  end;

  FSelectedControl := aValue;
  if assigned(FSelectedControl) then begin
    FSelectedControl.Select;
    if assigned(FSelectedControl.FParameter) then
      FSelectedControl.FParameter.Selected := True;
  end;
end;

procedure TG2GraphPatch.SelectNextModuleControl;
var Module : TG2FileModule;
    ModulePanel : TG2GraphModulePanel;
    i : integer;
begin
  if PatchPart[ ord(SelectedLocation)].SelectedParam = nil then
    exit;

  Module := Modules[ ord(SelectedLocation), PatchPart[ ord(SelectedLocation)].SelectedParam.ModuleIndex];
  if assigned(Module) then begin
    Module.SelectNextParam;
    ModulePanel := (Module as TG2GraphModule).Panel;
    i := 0;
    while (i< ModulePanel.ChildControlsCount) and not(ModulePanel.GraphChildControls[i].MouseInput
                                         and (ModulePanel.GraphChildControls[i].FParameter = Module.SelectedParam)) do
      inc(i);
    if (i< ModulePanel.ChildControlsCount) then
      SelectedControl := ModulePanel.GraphChildControls[i];
  end;

end;

procedure TG2GraphPatch.SelectPrevModuleControl;
var Module : TG2FileModule;
    ModulePanel : TG2GraphModulePanel;
    i : integer;
begin
  if PatchPart[ ord(SelectedLocation)].SelectedParam = nil then
    exit;

  Module := Modules[ ord(SelectedLocation), PatchPart[ ord(SelectedLocation)].SelectedParam.ModuleIndex];
  if assigned(Module) then begin
    Module.SelectPrevParam;
    ModulePanel := (Module as TG2GraphModule).Panel;
    i := 0;
    while (i< ModulePanel.ChildControlsCount) and not(ModulePanel.GraphChildControls[i].MouseInput
                                         and (ModulePanel.GraphChildControls[i].FParameter = Module.SelectedParam)) do
      inc(i);
    if (i< ModulePanel.ChildControlsCount) then
      SelectedControl := ModulePanel.GraphChildControls[i];
  end;
end;

procedure TG2GraphPatch.SetSelectedLocation(aLocation: TLocationType);
begin
  inherited;

  if assigned(G2) then begin
    (G2 as TG2Graph).FScrollboxVA.Invalidate;
    (G2 as TG2Graph).FScrollboxFX.Invalidate;
  end;
end;

procedure TG2GraphPatch.SetSelectedMorphIndex( aValue: integer);

  procedure SetControlMorphUpdate;
  var m, l, p : integer;
      Module : TG2FileModule;
      Param : TG2FileParameter;
  begin
    for l := 0 to 1 do begin
      for m := 0 to ModuleList[ l].Count - 1 do begin
        Module := ModuleList[ l].Items[m];
        for p := 0 to Module.ParameterCount - 1 do begin
            Param := Module.Parameter[p];
            if Param.HasMorph then
               Param.InvalidateControl;
        end;
      end;
    end;
  end;

begin
  if aValue <> SelectedMorphIndex then begin
    inherited;
    SetControlMorphUpdate;
  end;
end;

procedure TG2GraphPatch.SetVisible( aValue: boolean);
var i, j : integer;
begin
  for i := 0 to 1 do
    for j := 0 to ModuleCount[i] - 1 do
      (ModuleList[i].Items[j] as TG2GraphModule).FPanel.Visible := aValue;

  FVisible := aValue;
end;

procedure TG2GraphPatch.ShakeCables;
var p, c : integer;
    Cable : TG2GraphCable;
begin
  for p := 0 to 1 do begin
    for c := 0 to PatchPart[p].CableList.Count - 1 do begin
      Cable := PatchPart[p].CableList[c] as TG2GraphCable;
      Cable.ConnectorMoved;
    end;
  end;
end;

procedure TG2GraphPatch.UpdateColorScheme;
var p, m, c : integer;
    Module : TG2GraphModule;
begin
  for p := 0 to 1 do
    for m := 0 to ModuleCount[p] - 1 do begin
      Module := ModuleList[p].Items[m] as TG2GraphModule;
      if assigned(Module.FPanel) then begin
        for c := 0 to Module.FPanel.ChildControlsCount - 1 do begin
          if Module.FPanel.GraphChildControls[c] is TG2GraphButton then begin
            (Module.FPanel.GraphChildControls[c] as TG2GraphButton).HightlightColor := G_HighLightColor;
            Module.FPanel.GraphChildControls[c].Invalidate;
          end;
          if Module.FPanel.GraphChildControls[c] is TG2GraphKnob then begin
            (Module.FPanel.GraphChildControls[c] as TG2GraphKnob).HightlightColor := G_HighLightColor;
            Module.FPanel.GraphChildControls[c].Invalidate;
          end;
          if Module.FPanel.GraphChildControls[c] is TG2GraphLedGreen then begin
            (Module.FPanel.GraphChildControls[c] as TG2GraphLedGreen).LedColor := G_LedColor;
          end;
        end;
      end;
    end;
end;

procedure TG2GraphPatch.SortLeds;
var i : integer;
begin
  // Take leds that are in a led goup with only one led out of the group list
  // and put them in de led list.
  // These leds are addressed in message $39
  // VU-meters and ledgroups with more than 1? led are addressed in message $3A

  FLed39List.Clear;
  FLed3AList.Clear;

  i := 0;
  while i < FLedGroupList.Count do begin

    if FLedGroupList[i].FLeds.Count = 1 then begin
      // Add single Leds to the Led39List
      FLed39List.Add( FLedGroupList[i].FLeds[0])
    end else
      // Add grouped Leds to the Led3AList
      FLed3AList.Add( FLedGroupList[i]);

    inc(i);
  end;

  // Add the VU meters to the Led3AList
  for i := 0 to FMiniVUList.Count - 1 do
    FLed3AList.Add( FMiniVUList[i]);

  FLed3AList.Sort( CompareMiniVUOrder);
  FLed39List.Sort( CompareLedGreenOrder);
end;

procedure TG2GraphPatch.RemoveFromLedList( aLocation: TLocationType; aModuleIndex : integer);
var i : integer;
begin
  // When a module is deleted, it's leds must be removed from the lists

  i := 0;
  while (i < FMiniVUList.Count) do begin
     if (FMiniVUList.Items[i].FModule.ModuleIndex = aModuleIndex) and
        (FMiniVUList.Items[i].FModule.Location = aLocation) then
      FMiniVUList.Delete(i)
    else
      inc(i);
  end;

  i := 0;
  while (i < FLedGroupList.Count) do begin
    if (FLedGroupList.Items[i].FModule.ModuleIndex = aModuleIndex) and
       (FLedGroupList.Items[i].FModule.Location = aLocation) then
      FLedGroupList.Delete(i)
    else
      inc(i);
  end;

  SortLeds;
end;

// ==== G2GraphModule ==========================================================

constructor TG2GraphModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create( aPatchPart);
  FPanel := TG2GraphModulePanel.Create( aPatchPart);
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

{function TG2GraphModule.GetNewCol: TBits7;
begin
  if assigned(FPanel) then
    //Result := FPanel.GetNewCol
    Result := FPanel.CalcNewCol
  else
    Result := 0;
end;

function TG2GraphModule.GetNewRow: TBits7;
begin
  if assigned(FPanel) then
    //Result := FPanel.GetNewRow
    Result := FPanel.CalcNewRow
  else
    Result := 0;
end;}

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

function TG2GraphModule.GetParent : TWinControl;
begin
  if assigned(FPanel) then
    Result := FPanel.Parent
  else
    Result := nil;
end;

function TG2GraphModule.GetScrollPosX: integer;
begin
  if assigned(FPanel) then
    Result := FPanel.ScrollPosX
  else
    Result := 0;
end;

function TG2GraphModule.GetScrollPosY: integer;
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

procedure TG2GraphModule.Invalidate;
begin
  if assigned(FPanel) then
     FPanel.Invalidate;
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

procedure TG2GraphModule.SetModuleColor(Value: TBits8);
begin
  inherited;
  Invalidate;
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

procedure TG2GraphModule.SetParent( aValue : TWinControl);
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
  if assigned(FPanel) then
    Result := FPanel.ClientToScreen(p)
  else
    Result := Point(0,0);
end;

// ==== G2GraphModulePanel =====================================================

function CompareZOrder(Control1, Control2: pointer): integer;
begin
  // Sort children of module (controls) according to z-order
  if TG2GraphChildControl(Control1).FZOrder > TG2GraphChildControl(Control2).FZorder then
    Result := 1
  else
    if TG2GraphChildControl(Control1).FZOrder = TG2GraphChildControl(Control2).FZorder then
      Result := 0
    else
      Result := -1;
end;

constructor TG2GraphModulePanel.Create( AOwner: TComponent);
begin
  inherited Create( AOWner);

  FChildControls := TList.Create;

  FOldX := ScrollPosX;
  FOldY := ScrollPosY;

  Font.Name := 'Arial';
  Font.Size := 8;
  Font.Style := [fsbold];
  Font.Color := clBlack;

  Width := UNITS_COL;
  Height := UNITS_ROW;

  FWasAlreadySelected := False;
end;

destructor TG2GraphModulePanel.Destroy;
begin
  FChildControls.Free;

  inherited;
end;

function TG2GraphModulePanel.GetScrollBarX: integer;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).HorzScrollBar.Position
  else
    Result := 0;
end;

function TG2GraphModulePanel.GetScrollBarY: integer;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := (Parent as TScrollbox).VertScrollBar.Position
  else
    Result := 0;
end;

function TG2GraphModulePanel.GetScrollPosX: integer;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Left + (Parent as TScrollbox).HorzScrollBar.Position
  else
    Result := Left;
end;

function TG2GraphModulePanel.GetScrollPosY: integer;
begin
  if assigned(Parent) and (Parent is TScrollbox) then
    Result := Top + (Parent as TScrollbox).VertScrollBar.Position
  else
    Result := Top;
end;

function TG2GraphModulePanel.GetSelected: boolean;
begin
  Result := FData.Selected;
end;

procedure TG2GraphModulePanel.SetCol( aValue: TBits7);
begin
  if aValue <> FData.Col then
    Left := aValue * UNITS_COL - ScrollBarX;

  if FData.Selected then
    FOldX := Left;
end;

procedure TG2GraphModulePanel.SetRow( aValue: TBits7);
begin
  if aValue <> FData.Row then
    Top := aValue * UNITS_ROW - ScrollbarY;

  if FData.Selected then
    FOldY := Top;
end;

procedure TG2GraphModulePanel.SetSelected(aValue: boolean);
begin
  //FData.Selected := aValue;
  FOldX := Left;
  FOldY := Top;

  FData.FOutlineRect := BoundsRect;
  Invalidate;
end;

procedure TG2GraphModulePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
var i : integer;
    Patch : TG2GraphPatch;
begin
  if assigned( FDropDownList) then begin
    FDropDownList.Free;
    FDropDownList := nil;
  end;

  Patch := GetPatch;

  i := 0;
  while ( i < ChildControlsCount) and not( GraphChildControls[i].MouseInput and PointInRect( X, Y, GraphChildControls[i].BoundsRect)) do
    inc(i);

  if ( i < ChildControlsCount) then
    Patch.SelectedControl := GraphChildControls[i]
  else
    Patch.SelectedControl := nil;

  if assigned(Patch.SelectedControl) then begin
    Patch.SelectedControl.MouseDown( Button, Shift, X - Patch.SelectedControl.Left, Y - Patch.SelectedControl.Top);
  end else begin

    if not FData.Selected then begin
      if not(ssCtrl in Shift) then
        Patch.UnSelectModules( Location);
      FData.Selected := True;
      FWasAlreadySelected := False;
    end else
      FWasAlreadySelected := True;

    if Location <> Patch.SelectedLocation then
      Patch.SelectedLocation := Location;

    FStartX := X;
    FStartY := Y;
    FOldX := Left;
    FOldY := Top;
  end;

  inherited;
end;

procedure TG2GraphModulePanel.MouseMove( Shift: TShiftState; X, Y: Integer);
var Patch : TG2GraphPatch;
begin
  // Pass mousemovement to scrollbox when pasting a patch
  if assigned( Parent) and assigned((Parent as TG2GraphScrollbox).CopyPatch) then
    (Parent as TG2GraphScrollbox).MouseMove( Shift, Left + X, Top + Y);

  if ssLeft in Shift then begin
    Patch := GetPatch;

    if assigned(Patch.SelectedControl) then begin
      Patch.SelectedControl.MouseMove( Shift, X - Patch.SelectedControl.Left, Y - Patch.SelectedControl.Top);
    end else begin
      if FWasAlreadySelected then
        Patch.MoveOutlines( Data.Location, X - FStartX, Y - FStartY);
    end;
  end;

  inherited;
end;

procedure TG2GraphModulePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
var Patch : TG2GraphPatch;
begin
  Patch := GetPatch;

  if assigned(Patch.SelectedControl) then begin

    Patch.FSelectedControl.MouseUp( Button, Shift, X - Patch.FSelectedControl.Left, y - Patch.FSelectedControl.Top);

    if Patch.SelectedControl is TG2GraphConnector then begin
      if assigned(FOnConnectorClick) then
        FOnConnectorClick( self, Button, Shift, X, Y, (Patch.SelectedControl as TG2GraphConnector).FData);
    end else
      if assigned( Patch.FSelectedControl.FParameter) and assigned( FOnParameterClick) then
        FOnParameterClick( Patch.FSelectedControl, Button, Shift, X, Y, Patch.SelectedControl.FParameter);

  end else begin

    if FWasAlreadySelected then
      if (FStartX <> X) or (FStartY <> Y) then
        Patch.MessMoveModules( Data.Location);

    if assigned( FOnModuleClick) then
      FOnModuleClick( self, Button, Shift, X, Y, FData);

  end;
  inherited;
end;

procedure TG2GraphModulePanel.MoveOutline( dX, dY : integer);
begin
  if (dX = 0) and (dY = 0) then
    exit;

  if assigned( Parent) and FData.FOutlineVisible then
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);

  FData.FOutlineRect.Left := FOldX + dX;
  FData.FOutlineRect.Top := FOldY + dY;
  FData.FOutlineRect.Right := FData.FOutlineRect.Left + Width;
  FData.FOutlineRect.Bottom := FData.FOutlineRect.Top + Height;
  FData.NewRow := CalcNewRow;
  FData.NewCol := CalcNewCol;

  if assigned( Parent) then begin
    (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
    FData.FOutlineVisible := True;
  end;
end;

procedure TG2GraphModulePanel.MoveModule;
//var Patch : TG2GraphPatch;
begin
  // TODO
  if FData.FOutlineVisible then
    if assigned( Parent) then begin
      (Parent as TG2GraphScrollbox).DrawOutline(FData.FOutlineRect);
      FData.FOutlineVisible := False;
    end;

{  Patch := GetPatch;

  if (NewCol <> Col) or (NewRow <> Row) then
    Patch.MoveModule( Location, ModuleIndex, NewCol, NewRow);}
end;

function TG2GraphModulePanel.CalcNewCol: TBits7;
var NewCol : integer;
begin
  NewCol := (ScrollPosX + FData.FOutlineRect.Left - FOldX + FStartX) div UNITS_COL;
  if NewCol < 0 then
    Result := 0
  else
    Result := NewCol;
end;

function TG2GraphModulePanel.CalcNewRow: TBits7;
var NewRow : integer;
begin
  NewRow := (ScrollPosY + FData.FOutlineRect.Top - FOldY + FStartY) div UNITS_ROW;
  if NewRow < 0 then
    Result := 0
  else
    Result := NewRow;
end;

function TG2GraphModulePanel.GetPatch: TG2GraphPatch;
begin
  if not assigned(FData.PatchPart) then
    raise Exception.Create('Patch part not assigned to module.');

  Result := FData.PatchPart.Patch as TG2GraphPatch;
end;

procedure TG2GraphModulePanel.Paint;
begin
{$IFDEF FPC}
{$ELSE}
  PaintOn( Canvas, BoundsRect);
{$ENDIF}
end;

procedure TG2GraphModulePanel.PaintOn( ExtCanvas: TCanvas; ExtBoundsRect : TRect);
var Rect, Rect2 : TRect;
    i : integer;
    Title : string;
begin
  inherited;

  //AddLogLine('Paint module ' + FModuleName);

  Title := '';
  if assigned(FData.PatchPart) then
    Title := string(FData.PatchPart.GetModuleLabel( ModuleIndex))
  else
    Title := string(FData.ModuleName);

  Rect := SubRect( BoundsRect, ExtBoundsRect);

  ExtCanvas.Brush.Color := FData.Color;
  ExtCanvas.Brush.Style := bsSolid;
  ExtCanvas.FillRect( Rect);

  ExtCanvas.Font.Assign( Font);

  Rect2.Left   := 16;
  Rect2.Top    := 1;
  Rect2.Right  := Rect2.Left + ExtCanvas.TextWidth( Title) + 1;
  Rect2.Bottom := 14;

  Rect2 := AddRect( Rect2, BoundsRect);
  Rect2 := SubRect( Rect2, ExtBoundsRect);

  if (FData.Selected) and (Location = FData.PatchPart.Patch.SelectedLocation) then
    ExtCanvas.Brush.Color := clWhite;
  ExtCanvas.TextRect(Rect2, Rect2.Left + 1, Rect2.Top, Title);

  Rect := SubRect( BoundsRect, ExtBoundsRect);
  ExtCanvas.Brush.Color := FData.Color;
  DrawBevel( ExtCanvas, Rect, bvRaised);

  for i := 0 to FChildControls.Count - 1 do begin
    if RectOverlap( AddRect(TG2GraphChildControl(FChildControls[i]).BoundsRect, BoundsRect), ExtBoundsRect) then begin
      TG2GraphChildControl(FChildControls[i]).PaintOn( ExtCanvas, ExtBoundsRect);
    end;
  end;
end;

procedure TG2GraphModulePanel.ParsePanelData;
var //MemStream : TMemoryStream;
    ModuleStream : TModuleDefStream;
    CodeRef, Err : integer;
    aPath : string;
    aName, aValue, ControlType, CodeRefStr : AnsiString;
    ChildControl : TG2GraphChildControl;
    Connector : TG2GraphConnector;
    Param : TG2GraphParameter;
    Patch : TG2GraphPatch;
begin
  aPath := ExtractFilePath(ParamStr(0));
  //aPath := GetCurrentDir;
{$IFDEF FPC}
  aPath := aPath + 'Modules/';
{$ELSE}
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
              Connector := TG2GraphConnector.Create(self);
              Connector.FModule := self;
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
                Connector := TG2GraphConnector.Create(self);
                Connector.FModule := self;
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
                   ( ControlType = 'ButtonRadioEdit') or
                   ( ControlType = 'LevelShift') or
                   ( ControlType = 'ButtonFlat') or
                   ( ControlType = 'TextEdit') or
                   ( ControlType = 'PartSelector') or
                   ( ControlType = 'ButtonText') then begin

                  CodeRefStr := ModuleStream.PeekValue( 'CodeRef:', [#13, #10, '#'], ['#', '>']);
                  val( string(CodeRefStr), CodeRef, Err);
                  if Err = 0 then begin
                    if ControlType = 'PartSelector' then begin
                      Param := FData.Mode[ CodeRef] as TG2GraphParameter;
                    end else begin
                      Param := FData.Parameter[ CodeRef] as TG2GraphParameter;
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

        FChildControls.Sort(@CompareZOrder);
      end else
        raise Exception.Create('Unknown file type.');

      Patch.SortLeds;
    finally
      ModuleStream.Free;
    end;
  end;
end;

function TG2GraphModulePanel.GetColor: TColor;
begin
  Result := FData.Color;
end;

function TG2GraphModulePanel.GetControlType( aG2GraphChildControl: TG2GraphChildControl): string;
begin
  if aG2GraphChildControl is TG2GraphLabel then
    Result := 'Label';

  if aG2GraphChildControl is TG2GraphConnector then
    Result := 'Connector';

  if aG2GraphChildControl is TG2GraphDisplay then
    Result := 'Display';

  if aG2GraphChildControl is TG2GraphGraph then
    Result := 'Graph';

  if aG2GraphChildControl is TG2GraphLedGreen then
    Result := 'Led';

  if aG2GraphChildControl is TG2GraphMiniVU then
    Result := 'MiniVU';

  if aG2GraphChildControl is TG2GraphPartSelector then
    Result := 'PartSelector';

  if aG2GraphChildControl is TG2GraphButtonText then
    Result := 'ButtonText';

  if aG2GraphChildControl is TG2GraphTextEdit then
    Result := 'TextEdit';

  if aG2GraphChildControl is TG2GraphButtonFlat then
    Result := 'ButtonFlat';

  if aG2GraphChildControl is TG2GraphLevelShift then
    Result := 'LevelShift';

  if aG2GraphChildControl is  TG2GraphKnob then
    Result := 'Knob';

  if aG2GraphChildControl is  TG2GraphBitmap then
    Result := 'Bitmap';
end;

function TG2GraphModulePanel.NewG2GraphControl( aControlType : AnsiString) : TG2GraphChildControl;
begin
  Result := nil;

  if (aControlType = 'Line') then begin
    Result := TG2GraphLine.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Text') then begin
    Result := TG2GraphLabel.Create(self);
    Result.Module := self;
  end;

  if aControlType = 'Connector' then begin
    Result := TG2GraphConnector.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextField') then begin
    Result := TG2GraphDisplay.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Graph') then begin
    Result := TG2GraphGraph.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Led') then begin
    Result := TG2GraphLedGreen.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'MiniVU') then begin
    Result := TG2GraphMiniVU.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'PartSelector') then begin
    Result := TG2GraphPartSelector.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonText') then begin
    Result := TG2GraphButtonText.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'TextEdit') then begin
    Result := TG2GraphTextEdit.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonFlat') then begin
    Result := TG2GraphButtonFlat.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'LevelShift') then begin
    Result := TG2GraphLevelShift.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonRadio') then begin
    Result := TG2GraphButtonRadio.Create(self);
    (Result as TG2GraphButtonRadio).UpsideDown := True;
    Result.Module := self;
  end;

  if (aControlType = 'ButtonRadioEdit') then begin
    Result := TG2GraphButtonRadioEdit.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'ButtonIncDec') then begin
    Result := TG2GraphButtonIncDec.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Knob') or (aControlType = '') then begin
    Result := TG2GraphKnob.Create(self);
    Result.Module := self;
  end;

  if (aControlType = 'Bitmap') then begin
    Result := TG2GraphBitmap.Create(self);
    Result.Module := self;
  end;

  //if not assigned( Result)  then
  //  raise Exception.Create('Unknown control type ' + aControlType);
end;

function TG2GraphModulePanel.GetChildControlsCount: integer;
begin
  Result := FChildControls.Count;
end;

function TG2GraphModulePanel.GetGraphChildControl( ChildControlIndex: integer): TG2GraphChildControl;
begin
  Result := TG2GraphChildControl( FChildControls[ChildControlIndex]);
end;

function TG2GraphModulePanel.GetLocation: TLocationType;
begin
  Result := FData.Location;
end;

function TG2GraphModulePanel.GetModuleIndex: TBits8;
begin
  Result := FData.ModuleIndex;
end;

procedure TG2GraphModulePanel.AddGraphControl( aGraphCtrl: TG2GraphChildControl);
begin
  FChildControls.Add( aGraphCtrl);
end;

// ==== TG2GraphChildControl ===================================================

constructor TG2GraphChildControl.Create( AOwner: TComponent);
begin
  inherited;

  FModule := nil;
  FParameter := nil;

  Font.Name := 'Arial';
  Font.Size := 7;

  FMouseInput := False;
  FZOrder := 1;
  FSelected := False;

  FValue := 0;
  FLowValue := 0;
  FHighValue := 0;
end;

destructor TG2GraphChildControl.Destroy;
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

function TG2GraphChildControl.GetRelToParentRect : TRect;
begin
  if assigned( FModule) then
    Result := AddRect( BoundsRect, FModule.BoundsRect)
  else
    Result := BoundsRect;
end;

function TG2GraphChildControl.GetScreenCoordsRect: TRect;
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
end;

procedure TG2GraphChildControl.SetModule( aValue: TG2GraphModulePanel);
begin
  FModule := aValue;
end;

procedure TG2GraphChildControl.SetParameter( aParam : TG2FileParameter);
var Param : TG2GraphParameter;
begin
  if assigned(aParam) and not( aParam Is TG2GraphParameter) then
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
    Invalidate;

    //Update;
  end else begin
    Invalidate;
  end;
end;

function TG2GraphChildControl.GetGraphModule: TG2GraphModulePanel;
begin
  Result := FModule;
end;

function TG2GraphChildControl.GetHighValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.HighValue
  else
    Result := FHighValue;
end;

function TG2GraphChildControl.GetLowValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.LowValue
  else
    Result := FLowValue;
end;

function TG2GraphChildControl.HasMorph: boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.HasMorph
  else
    Result := False;
end;

function TG2GraphChildControl.GetMorph: TMorphParameter;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorph
  else
    Result := nil;
end;

function TG2GraphChildControl.GetMorphValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetSelectedMorphValue
  else
    Result := 0;
end;

function TG2GraphChildControl.GetParameter: TG2FileParameter;
begin
  Result := FParameter;
end;

function TG2GraphChildControl.GetParamIndex: integer;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamIndex
  else
    Result := -1;
end;

function TG2GraphChildControl.GetParamLabel( aIndex : integer): AnsiString;
begin
  if assigned( FParameter) then
    Result := FParameter.ParamLabel[ aIndex]
  else
    Result := '';
end;

function TG2GraphChildControl.GetParamLabelIndex: integer;
begin
  Result := 0;
end;

function TG2GraphChildControl.GetValue: byte;
begin
  if assigned( FParameter) then
    Result := FParameter.GetParameterValue
  else
    Result := FValue;
end;

function TG2GraphChildControl.GetSelected : boolean;
begin
  if assigned( FParameter) then
    Result := FParameter.Selected
  else
    Result := False;
end;

procedure TG2GraphChildControl.Invalidate;
var Rect : TRect;
begin
  if assigned( FModule) and assigned( FModule.Parent) then begin
    Rect := AddRect( BoundsRect, FModule.BoundsRect);
    InvalidateRect( FModule.Parent.Handle, @Rect, True);

    SendCtrlMidiValue;
  end else begin
    inherited Invalidate;
    SendCtrlMidiValue;
  end;
end;

procedure TG2GraphChildControl.Update;
var Rect : TRect;
begin
  if assigned( FModule) and assigned( FModule.Parent) then begin
    Rect := AddRect( BoundsRect, FModule.BoundsRect);
    InvalidateRect( FModule.Parent.Handle, @Rect, True);
    FModule.Parent.Update;

    SendCtrlMidiValue;
  end else begin
    inherited Invalidate;
    inherited Update;

    SendCtrlMidiValue;
  end;
end;

procedure TG2GraphChildControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    FStartValue := Value;
    //Select;
    Invalidate;
  end;
  inherited;
end;

procedure TG2GraphChildControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TG2GraphChildControl.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: Integer);
begin
  Invalidate;
  inherited;
end;

procedure TG2GraphChildControl.Paint;
begin
  PaintOn( Canvas, BoundsRect);
end;

procedure TG2GraphChildControl.PaintOn(ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var Rect : TRect;
begin
  // Abstract
end;

procedure TG2GraphChildControl.ParsePanelData(fs: TModuleDefStream);
var aName, aValue : AnsiString;
begin
  while (fs.Position < fs.Size) and (aName <> '#>') do begin
    fs.ReadSpaces;
    aName := fs.ReadUntil( [':', #13]);
    if aName <> '#>' then begin
      //aValue := ReadUntil(fs, [#13]);
      if not ParseProperties( fs, aName) then begin
        // Unknown property
        aValue := fs.ReadUntil( [#13]);
      end;
    end;
  end;
end;

function TG2GraphChildControl.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
    G2 : TG2File;
    temp : string;
begin
  Result := True;

  if aName = 'ID' then begin
    aValue := fs.ReadUntil( [#13]);
    FID := StrToInt( string(aValue));
  end else

  if aName = 'XPos' then begin
    aValue := fs.ReadUntil( [#13]);
    Left := FModule.Left + StrToInt( string(aValue));
  end else

  if aName = 'YPos' then begin
    aValue := fs.ReadUntil( [#13]);
    Top  := FModule.Top + StrToInt( string(aValue));
  end else

  if aName = 'Width' then begin
    aValue := fs.ReadUntil( [#13]);
    if aValue[1] = '"' then begin
      fs.Position := fs.Position - Length(aValue) - 1;
      Result := False;
    end else
      Width := StrToInt( string(aValue));
  end else

  if aName = 'Height' then begin
    aValue := fs.ReadUntil( [#13]);
    Height := StrToInt( string(aValue));
  end else

  if aName = 'ZPos' then begin
    aValue := fs.ReadUntil( [#13]);
    FZOrder := StrToInt( string(aValue));
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

function TG2GraphChildControl.CheckValueBounds( aValue: integer): byte;
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

procedure TG2GraphChildControl.SetParamLabel( aIndex : integer; aValue: AnsiString);
begin
  if assigned( FParameter) then
    FParameter.ParamLabel[ aIndex] := aValue
  else begin
    inherited Invalidate;
  end;
end;

procedure TG2GraphChildControl.SetValue( aValue: byte);
begin
  if assigned( FParameter) then begin
    FParameter.SetParameterValue( aValue);

//    SendMidiValue;

  end else begin
    FValue := aValue;
    Update;

//    SendMidiValue;

    if assigned( FOnChange) then
      FOnChange( self);
  end;
end;

procedure TG2GraphChildControl.SetValueByCtrlMidi(aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent);
var MidiMessage, MidiValue1, MidiValue2 : byte;
    CtrlRange, MidiRange : byte;
begin
  if assigned(aMidiEditorAssignment) then begin
    MidiMessage := aMidiEvent.MidiMessage shr 4;
    MidiValue1 := aMidiEvent.Data1;
    MidiValue2 := aMidiEvent.Data2;

    MidiReceiving := True;
    try

      if aMidiEditorAssignment.Note <> 0 then begin
        if MidiMessage = $09 then
          Value := HighValue
        else
          if MidiMessage = $08 then
            Value := LowValue;
      end else begin
        CtrlRange := (HighValue - LowValue);
        MidiRange := (aMidiEditorAssignment.MaxValue - aMidiEditorAssignment.MinValue);
        if MidiRange <> 0 then begin
          Value := trunc(((MidiValue2 - aMidiEditorAssignment.MinValue) / MidiRange) * CtrlRange + LowValue);
        end;
      end;
    finally
      MidiReceiving := False;
    end;
  end;
end;

procedure TG2GraphChildControl.SetLowValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.LowValue := aValue
  else begin
    FLowValue := aValue;
    inherited Invalidate;
  end;
end;

procedure TG2GraphChildControl.SetHighValue( aValue: byte);
begin
  if assigned( FParameter) then
    FParameter.HighValue := aValue
  else begin
    FHighValue := aValue;
    inherited Invalidate;
  end;
end;

procedure TG2GraphChildControl.InitValue( aValue: integer);
var Rect : TRect;
begin
  if (aValue >= FLowValue) and (aValue <= FHighValue) and ( aValue <> FValue) then begin
    FValue := aValue;
    Rect := BoundsRect;
    InvalidateRect( Parent.Handle, @Rect, True);
  end;
end;

procedure TG2GraphChildControl.SetMorphValue( aValue: byte);
begin
  if assigned( FParameter) then begin
    FParameter.SetSelectedMorphValue( aValue);
    Update;
  end;
end;

procedure TG2GraphChildControl.Select;
begin
  FSelected := True;
  Invalidate;
end;

procedure TG2GraphChildControl.Deselect;
begin
  FSelected := False;
  Invalidate;
end;


// ==== TG2GraphLine ===========================================================

constructor TG2GraphLine.Create( AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  FOrientation := otHorizontal;
  FLineWidth := 1;
  FLength := 0;
  Width := 0;
  Height := 0;

  Color := clGray;
end;

destructor TG2GraphLine.Destroy;
begin

  inherited;
end;

procedure TG2GraphLine.PaintOn(ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var Rect, Rect2 : TRect;
begin
  //AddLogLine('Paint line');

  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  ExtCanvas.Brush.Color := Color;
  case FOrientation of
  otHorizontal : begin
                   Rect2 := Rect;
                   Rect2.Right := Rect2.Left + FLength;
                   Rect2.Bottom := Rect2.Top + FLineWidth;

                 end;
  otVertical   : begin
                   Rect2 := Rect;
                   Rect2.Right := Rect2.Left + FLineWidth;
                   Rect2.Bottom := Rect2.Top + FLength;
                 end;
  end;
  ExtCanvas.FillRect(Rect2);
end;

procedure TG2GraphLine.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  case FOrientation of
  otHorizontal : SetBounds( Left, Top, FLength, Top + FLineWidth);
  otVertical   : SetBounds( Left, Top, Left + FLineWidth, Top + FLength);
  end;
end;

function TG2GraphLine.ParseProperties( fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Length' then begin
      aValue := fs.ReadUntil( [#13]);
      FLength := StrToInt(string(aValue));
    end else

    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Vertical"' then begin
        FOrientation := otVertical;
      end;
      if aValue = '"Horizontal"' then begin
        FOrientation := otHorizontal;
      end;
    end else

    if aName = 'Width' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Thin"' then begin
        FLineWidth := 1;
      end;
      if aValue = '"Thick"' then begin
        FLineWidth := 4;
      end;
    end else

      Result := False
  end;
end;

procedure TG2GraphLine.SetLength( aValue: integer);
begin
  FLength := aValue;
  Invalidate;
end;

procedure TG2GraphLine.SetLineWidth( aValue: integer);
begin
  FLineWidth := aValue;
  Invalidate;

end;

procedure TG2GraphLine.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  Invalidate;
end;

// ==== TG2GraphBitmap =========================================================

constructor TG2GraphBitmap.Create( AOwner: TComponent);
begin
  inherited;
  FImageList := TG2ImageList.Create( True);
end;

destructor TG2GraphBitmap.Destroy;
begin
  FImageList.Free;

  inherited;
end;

procedure TG2GraphBitmap.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect : TRect;
begin
  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);
  if FImageList.Count > 0 then
    DrawCenter( ExtCanvas, Rect, FImageList.Items[0]);
end;

procedure TG2GraphBitmap.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  FImageList.FBitmapWidth := Width;
  FImageList.FBitmapHeight := Height;

  FImageList.ParseImageData(1, False);
  //SetBounds(Left, Top, Width, Height);
end;

function TG2GraphBitmap.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Data' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FImageList.FImageData, [':'], ['"']);
    end else
      Result := False
  end;
end;

// ==== TG2GraphLabel ==========================================================

constructor TG2GraphLabel.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  Width := 30;
  Height := 10;
end;

destructor TG2GraphLabel.Destroy;
begin
  inherited;
end;

procedure TG2GraphLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  inherited;
  if assigned(FOnClick) then FOnClick( self);
end;

procedure TG2GraphLabel.SetCaption( avalue: string);
begin
  FCaption := aValue;
  Invalidate;
end;

procedure TG2GraphLabel.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect : TRect;
begin
 // AddLogLine('Paint label');

  inherited;

  ExtCanvas.Font.Assign( Font);

  Width := ExtCanvas.TextWidth(FCaption) + 1;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  if assigned(FModule) then
    ExtCanvas.Brush.Color := FModule.Color;

  ExtCanvas.TextRect(
          Rect,
          Rect.Left + 1,
          Rect.Top +( Rect.Bottom - Rect.Top - ExtCanvas.TextHeight(FCaption)) div 2,
          FCaption);
end;

function TG2GraphLabel.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'FontSize' then begin
      aValue := fs.ReadUntil( [#13]);
      Font.Size := StrToInt(string(aValue))-2
    end else

    if aName = 'Text' then begin
      aValue := fs.ReadUntil( [#13]);
      Caption := string(fs.UnQuote(aValue));
    end else

      Result := False
  end;
end;

// ==== TG2GraphLed ============================================================

constructor TG2GraphLed.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  FGroupId := 0;
end;


destructor TG2GraphLed.Destroy;
begin
  inherited;
end;

procedure TG2GraphLed.SetLevel( aValue: byte);
begin
  // Abstract
end;

// ==== TG2GraphLedGreen =======================================================

constructor TG2GraphLedGreen.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  Width := 11;
  Height := 6;
  FLevel := 0;

  FLedColor := G_LedColor;
end;

destructor TG2GraphLedGreen.Destroy;
begin
  inherited;
end;

procedure TG2GraphLedGreen.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect : TRect;
begin
  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  if FLevel = 0 then begin
    ExtCanvas.Pen.Color := clBlack;
    //ExtCanvas.Brush.Color := clGreen;
    ExtCanvas.Brush.Color := Darker(FLedColor, 100);
    ExtCanvas.Rectangle(Rect);
  end else begin
    ExtCanvas.Pen.Color := Darker(FLedColor, 100);
    ExtCanvas.Brush.Color := FLedColor;
    //ExtCanvas.Pen.Color := clGreen;
    //ExtCanvas.Brush.Color := clLime;

    ExtCanvas.Rectangle(Rect);
  end;
end;

function CompareSeqLedOrder( Led1 : pointer; Led2 : pointer): integer;
begin
  if TG2GraphLedGreen(led1).FCodeRef > TG2GraphLedGreen(led2).FCodeRef then
    Result := 1
  else
    if TG2GraphLedGreen(led1).FCodeRef = TG2GraphLedGreen(led2).FCodeRef then
      Result := 0
    else
      Result := -1;
end;

procedure TG2GraphLedGreen.ParsePanelData( fs: TModuleDefStream);
var i : integer;
    LedGroup : TG2GraphLedGroup;
    Patch : TG2GraphPatch;
begin
  inherited;

  Patch := FModule.GetPatch;

  i := 0;
  while (i < Patch.FLedGroupList.Count)
     and not( ( TG2GraphLed( Patch.FLedGroupList[i]).FGroupId = FGroupID)
          and ( TG2GraphLed( Patch.FLedGroupList[i]).FModule.ModuleIndex = FModule.ModuleIndex)
          and ( TG2GraphLed( Patch.FLedGroupList[i]).FModule.Location = FModule.Location)) do
    inc(i);

  if not(i < Patch.FLedGroupList.Count) then begin
    // No, add a new group
    LedGroup := TG2GraphLedGroup.Create( FModule);
    LedGroup.FModule := FModule;
    LedGroup.FType := FType;
    LedGroup.FGroupID := FGroupID;
    FModule.AddGraphControl( LedGroup);
    Patch.FLedGroupList.Add( LedGroup);
    Patch.FLedGroupList.Sort( CompareMiniVUOrder);
  end else begin
    // Yes, select the existing group
    LedGroup := TG2GraphLedGroup( Patch.FLedGroupList[i]);
  end;
  // Add the led
  LedGroup.FLeds.Add( self);
  LedGroup.FLeds.Sort( CompareSeqLedOrder);
end;

function TG2GraphLedGreen.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FCodeRef := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      FInfoFunc := StrToInt(string(aValue));
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Green"' then begin
        FType := ltGreen;
        Width := 8;
        Height := 8;
      end;
      if aValue = '"Sequencer"' then begin
        FType := ltSequencer;
        Width := 12;
        Height := 7;
      end;
    end else

    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      FGroupId := StrToInt(string(aValue));
    end else

      Result := False
  end;
end;

procedure TG2GraphLedGreen.SetLevel( aValue: byte);
begin
  if aValue <> FLevel then begin
    FLevel := aValue;
    Update;
  end;
end;

procedure TG2GraphLedGreen.SetLedColor( Value : TColor);
begin
  FLedColor := Value;
  Update;
end;

// ==== TG2GraphLedGroup =======================================================

constructor TG2GraphLedGroup.Create( AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  FLeds := TList.Create;
  FType := ltSequencer;
  FGroupId := 0;
  FLedOn := 0;
end;

destructor TG2GraphLedGroup.Destroy;
begin
  FLeds.Free;
  inherited;
end;

procedure TG2GraphLedGroup.SetLevel( aValue: byte);
begin
  inherited;

  if FLedOn < FLeds.Count then
    TG2GraphLedGreen(FLeds[ FLedOn]).SetLevel( 0);

  if aValue < FLeds.Count then begin
    FLedOn := aValue;
    TG2GraphLedGreen(FLeds[ FLedOn]).SetLevel( 1);
  end;
end;

// ==== TG2GraphMiniVU =========================================================

constructor TG2GraphMiniVU.Create( AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  FType := ltMiniVU;
  FMiniVUWidth := 8;
  FMiniVUHeight := 16;

  Width := FMiniVUWidth;
  Height := FMiniVUHeight;

  FLevel := 0;
end;

destructor TG2GraphMiniVU.Destroy;
begin
  inherited;
end;

procedure TG2GraphMiniVU.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, Rect2 : TRect;
    i : integer;
    level_green,
    level_yellow,
    level_red : integer;
begin
  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  level_green := 8 * Height div 16;
  level_yellow := 12 * Height div 16;
  level_red := Height;

  Rect2 := Rect;
  Rect2.Bottom := Rect.Bottom;
  Rect2.Top := Rect.Bottom - level_green;
  ExtCanvas.Brush.Color := clGreen;
  ExtCanvas.FillRect( Rect2);

  Rect2.Bottom := Rect.Bottom - level_green;
  Rect2.Top := Rect.Bottom - level_yellow;
  ExtCanvas.Brush.Color := clOlive;
  ExtCanvas.FillRect( Rect2);

  Rect2.Bottom := Rect.Bottom - level_yellow;
  Rect2.Top := Rect.Bottom - Height;
  ExtCanvas.Brush.Color := clMaroon;
  ExtCanvas.FillRect( Rect2);

  Rect2 := Rect;

  i := 0;
  while (i <= Height) and (i <= FLevel{(Height * Flevel) div 16}) do begin

    Rect2.Top := Rect.Bottom - i - 1;
    Rect2.Bottom := Rect.Bottom - i;

    if i > level_yellow then
      ExtCanvas.Brush.Color := clRed
    else
      if i > level_green then
        ExtCanvas.Brush.Color := clYellow
      else
        ExtCanvas.Brush.Color := clLime;

    ExtCanvas.FillRect( Rect2);
    inc(i);
  end;
end;

procedure TG2GraphMiniVU.ParsePanelData( fs: TModuleDefStream);
var Patch : TG2GraphPatch;
begin
  inherited;

  Patch := FModule.GetPatch;

  Patch.FMiniVUList.Add( self);
  Patch.FMiniVUList.Sort( CompareMiniVUOrder);
end;

function TG2GraphMiniVU.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FCodeRef := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      FInfoFunc := StrToInt(string(aValue));
    end else

    if aName = 'Orientation' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Vertical"' then begin
        FOrientation := otVertical;
        Width := FMiniVUWidth;
        Height := FMiniVUHeight;
      end;
      if aValue = '"Horizontal"' then begin
        FOrientation := otHorizontal;
        Height := FMiniVUWidth;
        Width := FMiniVUHeight;
      end;
    end else

    if aName = 'GroupId' then begin
      aValue := fs.ReadUntil( [#13]);
      FGroupId := StrToInt(string(aValue));
    end else

      Result := False
  end;
end;

procedure TG2GraphMiniVU.SetLevel(aValue: byte);
begin
  // -40dB 0dB Green  aValue 0..7
  // 0dB 11dB Yellow         9..41/42
  //     >11dB Red           75/76
  if aValue = 0 then
    FLevel := 0
  else
    FLevel := round(FMiniVUHeight*system.math.log10(aValue)*0.5); // Something like this...
  Update;
end;

// ==== TG2GraphDisplay ========================================================

constructor TG2GraphDisplay.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := False;

  Font.Name := 'Arial';
  Font.Size := 8;
  Font.Style := [fsBold];
  Font.Color := clWhite;

  FLine1 := '';
  FLine2 := '';
  FLine3 := '';
  FLine4 := '';
  FLineCount := 1;

  Color := CL_DISPLAY_BACKGRND;

  Width := 38;
  Height := 13;

  FDisplayType := 0;
  //FTextFunction := 0;
  FMasterRef := -1;
  FDependencies := TStringList.Create;
end;

destructor TG2GraphDisplay.Destroy;
begin
  FDependencies.Free;
  inherited;
end;

function TG2GraphDisplay.GetLine(LineNo: integer): AnsiString;
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

procedure TG2GraphDisplay.SetLine(LineNo: integer; aValue: AnsiString);
begin
  case LineNo of
  0 : FLine1 := aValue;
  1 : FLine2 := aValue;
  2 : FLine3 := aValue;
  3 : FLine4 := aValue;
  end;
  Invalidate;
end;

procedure TG2GraphDisplay.SetLineCount(aValue: integer);
begin
  if (aValue > 0) and (aValue <=4) then
    FLineCount := aValue
  else
    raise Exception.Create('Linecount must be between 1 and 4.');
end;

procedure TG2GraphDisplay.SetParameter(aParam: TG2FileParameter);
begin
  inherited;

  if assigned(aParam) then
    FTextFunction := aParam.TextFunctionIndex;
end;

procedure TG2GraphDisplay.SetTextFunction( aValue: integer);
begin
  FTextFunction := aValue;
  Invalidate;
end;

procedure TG2GraphDisplay.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, LineRect : TRect;
    i : integer;
    LineHeight : integer;
    BitMap : TBitmap;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  if not Visible then
    exit;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  if FLineCount > 0 then
    LineHeight := (ClientRect.Bottom - ClientRect.Top) div FLineCount
  else
    LineHeight := (ClientRect.Bottom - ClientRect.Top);

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;

  Bitmap := TBitmap.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Rect.Right - Rect.Left + 1;
    Bitmap.Height := Rect.Bottom - Rect.Top + 1;
    Bitmap.canvas.CopyRect(ClientRect, ExtCanvas, Rect);

    Bitmap.Canvas.Font.Assign( Font);
    Bitmap.Canvas.Brush.Color := Color;

    LineRect.Left := 0;
    LineRect.Right := Width + 1;
    LineRect.Top := 0;
    LineRect.Bottom := LineRect.Top + LineHeight + 1;

    for i := 0 to FLineCount - 1 do begin
      Bitmap.Canvas.FillRect( LineRect);
      if assigned(FParameter) then begin
        case FDisplayType of
        0 : TextCenter( Bitmap.Canvas, LineRect, FParameter.TextFunction);
        1 : case i of
            0 : TextCenter( Bitmap.Canvas, LineRect, FParameter.ParamName);
            1 : TextCenter( Bitmap.Canvas, LineRect, FParameter.TextFunction);
            end;
        2 : case i of
            0 : TextCenter( Bitmap.Canvas, LineRect, FParameter.ParamName);
            1 : TextCenter( Bitmap.Canvas, LineRect, IntToStr(FParameter.GetParameterValue));
            end;
        3 : TextCenter( Bitmap.Canvas, LineRect, FParameter.ParamName);
        4 : TextCenter( Bitmap.Canvas, LineRect, IntToStr(FParameter.Patch.Slot.SlotIndex + 1) + ':' + string(FParameter.ModuleName));
        5 : if assigned(FParameter.Module) then TextCenter( Bitmap.Canvas, LineRect, FParameter.Module.ModuleName);
        end;
      end else
        TextCenter( Bitmap.Canvas, LineRect, Line[i]);
      LineRect.Top := LineRect.Top + LineHeight;
      LineRect.Bottom := LineRect.Bottom + LineHeight + 1;
    end;

    ExtCanvas.Draw( Rect.Left, Rect.Top, Bitmap);
  finally
    Bitmap.Free;
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

function TG2GraphDisplay.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
    //sl : TStringList;
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
      //FDependencies := TStringList.Create;
      //try
        FDependencies.Clear;
        fs.ReadOptions( FDependencies, [','], ['"']);
        if FDependencies.Count > 0 then
          if FMasterRef > -1 then begin
            //if FMasterRef < FDependencies.Count then begin

              // Find ref to master parameter
              i := 0;
              while (i<FDependencies.Count) and (FDependencies[i]<>IntToStr(FMasterRef)) and (FDependencies[i]<>'s'+IntToStr(FMasterRef)) do
                inc(i);

              if (i<FDependencies.Count) then begin
                // Assign master parameter first
                if (Lowercase(FDependencies[ i][1]) = 's') then begin // One of the static params
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

                for i := 0 to FDependencies.Count - 1 do begin
                  if (length(FDependencies[i])>0) and (Lowercase(FDependencies[i][1]) = 's') then begin
                    val(copy(FDependencies[i], 2, Length(FDependencies[i]) - 1), value, c);
                    if c = 0 then begin
                      FParameter.AddTextDependency(ptMode, value);
                      (FModule.FData.Mode[ value] as TG2GraphParameter).AssignControl(self);
                    end;
                  end else begin
                    val(FDependencies[i], value, c);
                    if c = 0 then begin
                      FParameter.AddTextDependency(ptParam, value);
                      (FModule.FData.Parameter[ value] as TG2GraphParameter).AssignControl(self);
                    end;
                  end;
                end;
              end;
          end else
            raise Exception.Create('Master param not assigned yet...');
      //finally
      //  FDependencies.Free;
      //end;

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

// ===== TG2GraphGraph =========================================================

constructor TG2GraphGraph.Create(AOwner: TComponent);
begin
  inherited;

  Color := clGray;
end;

destructor TG2GraphGraph.Destroy;
begin

  inherited;
end;

procedure TG2GraphGraph.PaintOn(ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var Rect : TRect;
begin
  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  ExtCanvas.Pen.Color := clBtnFace;
  ExtCanvas.Brush.Color := clBtnFace;
  ExtCanvas.Brush.Style := bsClear;
  ExtCanvas.Rectangle(Rect);
end;

function TG2GraphGraph.ParseProperties( fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;
  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Graph Func' then begin
      aValue := fs.ReadUntil( [#13]);
    end else

    if aName = 'Dependencies' then begin
      aValue := fs.ReadUntil( [#13]);
    end else

      Result := False
  end;
end;

// ==== TG2GraphButton =========================================================

constructor TG2GraphButton.Create(AOwner: TComponent);
begin
  inherited;

  FButtonText := TStringList.Create;
  FImageList := TG2ImageList.Create( True);

  FMouseInput := True;

  Width := 13;
  Height := 12;

  FButtonWidth := 0;
  FButtonHeight := 0;
  FButtonCount := 0;
  FOrientation := otHorizontal;
  FImageWidth := 0;
  FImageCount := 0;

  FHighLightColor := G_HighLightColor;

  FCaption := '';
  FIcon := itNone;

  FPressed := False;
  //FHide := False;
  //FAutoHide := False;

  Color := CL_BTN_FACE;
end;

destructor TG2GraphButton.Destroy;
begin
  FImageList.Free;
  FButtonText.Free;
  inherited;
end;

procedure TG2GraphButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  if (ssLeft in Shift) then begin
    FPressed := True;
  end;
  inherited;
end;

procedure TG2GraphButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  if FPressed then begin
    FPressed := False;
    if assigned(FOnClick) then FOnClick( self);
  end;
  inherited;
end;

procedure TG2GraphButton.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, Rect2 : TRect;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  inherited;

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);
    //if FHide  then
    //  exit;

    //AddLogLine('Paint button');

    Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

    ExtCanvas.Brush.Color := Color;//clBtnFace;
    ExtCanvas.FillRect( Rect);

    if FImageList.Count = 0 then begin
      ExtCanvas.Font.Assign( Font);
      TextCenter( ExtCanvas, Rect, '');
    end else begin
      Rect2 := Rect;
      ShrinkRect( Rect2, 4);
      SquareRect( Rect2);
      ExtCanvas.Brush.Color := clBlack;
      DrawIcon( ExtCanvas, Rect2, Rect, FIcon);
    end;

    if Selected then
      ExtCanvas.Brush.Color := CL_SELECTED_PARAM
    else
      ExtCanvas.Brush.Color := Color;

    if FPressed then
      DrawBevel( ExtCanvas, Rect, bvNone)
    else
      DrawBevel( ExtCanvas, Rect, bvRaised);

    if MidiAware and ShowMidiBox then begin
      if assigned(FMidiEditorAssignmentList) then
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
      else
        MidiEditorAssignment := nil;
      DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
    end;
  finally
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

{procedure TG2GraphButton.SetAutoHide( aValue: boolean);
begin
  FAutoHide := aValue;
  if FAutoHide then
    Hide := True
  else
    Hide := False;
end;}

procedure TG2GraphButton.SetCaption( aValue: string);
begin
  FCaption := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetOrientation( aValue : TOrientationType);
begin
  // Abstract
end;

{procedure TG2GraphButton.SetHide( aValue: boolean);
begin
  FHide := aValue;
  Invalidate;
end;}

procedure TG2GraphButton.SetHighLightColor( aValue: TColor);
begin
  FHighlightColor := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetBevel( aValue: boolean);
begin
  FBevel := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetBorderColor( aValue: TColor);
begin
  FBorderColor := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetIcon( aValue: TIconType);
begin
  FIcon := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetButtonCount( aValue: integer);
begin
  FButtonCount := aValue;
  Invalidate;
end;

procedure TG2GraphButton.SetButtonText( aValue: TStrings);
begin
  FButtonText.Assign( aValue);
  Invalidate;
end;

function TG2GraphButton.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
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

// ==== TG2GraphButtonText =====================================================

constructor TG2GraphButtonText.Create(AOwner: TComponent);
begin
  inherited;

  //FButtonTextType := bttNormal;
  FButtonTextType := bttPush;
end;

destructor TG2GraphButtonText.Destroy;
begin
  inherited;
end;

procedure TG2GraphButtonText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if  ( ssLeft in Shift) then begin
    if FButtonTextType = bttPush then
      Value := 1
    else
      Value := 1 - Value;
  end;
  inherited;
end;

procedure TG2GraphButtonText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FButtonTextType = bttPush then
    Value := 0;
  inherited;
end;

procedure TG2GraphButtonText.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, IconRect : TRect;
    LabelText : string;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

    ExtCanvas.Font.Assign( Font);

    if FButtonTextType = bttCheckBox then begin

      ExtCanvas.Brush.Color := Color;
      ExtCanvas.FillRect( Rect);
      if Value = 1 then begin
        IconRect := Rect;
        ShrinkRect( IconRect ,1);
        ExtCanvas.Brush.Color := clBlack;
        ExtCanvas.Pen.Color := clBlack;
        DrawIcon( ExtCanvas, Rect, IconRect, itCheck);
      end;

    end else begin

      //if FButtonTextType = bttPush then
      //  ExtCanvas.Brush.Color := Color
      //else
        if Value = 0 then
          ExtCanvas.Brush.Color := Color
        else
          ExtCanvas.Brush.Color := FHighLightColor;
      ExtCanvas.FillRect( Rect);

      if FImageList.Count > 0 then
        DrawCenter( ExtCanvas, Rect, FImageList.Items[0])
      else begin
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
        TextCenter( ExtCanvas, Rect, LabelText);
      end;

      if Selected then
        ExtCanvas.Brush.Color := CL_SELECTED_PARAM
      else
        ExtCanvas.Brush.Color := Color;

      if Value = 0 then
        DrawBevel( ExtCanvas, Rect, bvRaised)
      else
        DrawBevel( ExtCanvas, Rect, bvNone);
    end;

    if MidiAware and ShowMidiBox then begin
      if assigned(FMidiEditorAssignmentList) then
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
      else
        MidiEditorAssignment := nil;
      DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
    end;

  finally
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

procedure TG2GraphButtonText.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;

  if FImageList.FImageData.Count > 0 then begin
    FImageList.BitmapWidth := FImageWidth;
    FImageList.ParseImageData( 1, True)
  end;

  //FParameter.CanChangeLabel := True;
end;

function TG2GraphButtonText.ParseProperties(fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);

      {if aValue = '"Push"' then
        FButtonTextType := bttPush
      else
        FButtonTextType := bttNormal;}
      if aValue = '"Push"' then
        FButtonTextType := bttPush
      else
        if aValue = '"Check"' then
          FButtonTextType := bttCheck;

    end else
      Result := False

  end;
end;

procedure TG2GraphButtonText.SetValueByCtrlMidi( aMidiEditorAssignment: TMidiEditorAssignment; aMidiEvent: TMyMidiEvent);
var MidiMessage, MidiValue1, MidiValue2 : byte;
    CtrlRange, MidiRange : byte;
begin
  if assigned(aMidiEditorAssignment) then begin
    MidiMessage := aMidiEvent.MidiMessage shr 4;
    MidiValue1 := aMidiEvent.Data1;
    MidiValue2 := aMidiEvent.Data2;

    MidiReceiving := True;
    try
      if aMidiEditorAssignment.Note <> 0 then begin
        if MidiMessage = $09 then begin
          if Value = HighValue then
            Value := LowValue
          else
            Value := Value + 1;
        end;
      end else begin
        if Value = HighValue then
          Value := LowValue
        else
          Value := Value + 1;
      end;
    finally
      MidiReceiving := False;
    end;

    if assigned(FOnClick) then FOnClick( self);
  end;
end;

procedure TG2GraphButtonText.SetButtonTextType( aValue: TButtonTextType);
begin
  FButtonTextType := aValue;
  Invalidate;
end;

////////////////////////////////////////////////////////////////////////////////
// TG2GraphTextEdit
////////////////////////////////////////////////////////////////////////////////

constructor TG2GraphTextEdit.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2GraphTextEdit.Destroy;
begin

  inherited;
end;


// ==== TG2GraphButtonFlat =====================================================

constructor TG2GraphButtonFlat.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2GraphButtonFlat.Destroy;
begin
  inherited;
end;

procedure TG2GraphButtonFlat.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect : TRect;
    LabelText : string;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  //AddLogLine('Paint button flat');

  inherited;

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

    ExtCanvas.Font.Assign( Font);

    ExtCanvas.Brush.Color := CL_BTN_FACE;
    ExtCanvas.FillRect(Rect);

    {if Value < FButtonText.Count then begin
      TextCenter( ExtCanvas, Rect, FButtonText[Value])
    end else
      if Value < FImageList.Count then begin
        DrawCenter( ExtCanvas, Rect, FImageList.Items[Value])
      end;}

    if (FImageList.Count > 0) and (Value < FImageList.Count) then begin
      DrawCenter( ExtCanvas, Rect, FImageList.Items[Value])
    end else begin
      LabelText := '';
      if assigned(Parameter) then
        LabelText := Parameter.SelectedButtonText
      else
        if Value < FButtonText.Count then
          LabelText := FButtonText[Value];

      if assigned(Parameter) and (LabelText = '') then
        LabelText := Parameter.InfoFunction( Parameter.InfoFunctionIndex);

      TextCenter( ExtCanvas, Rect, LabelText)
    end;

    if Selected then
      ExtCanvas.Pen.Color := CL_SELECTED_PARAM
    else
      ExtCanvas.Pen.Color := CL_BTN_BORDER;

    DrawRect( ExtCanvas, Rect);

    if MidiAware and ShowMidiBox then begin
      if assigned(FMidiEditorAssignmentList) then
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
      else
        MidiEditorAssignment := nil;
      DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
    end;
  finally
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

procedure TG2GraphButtonFlat.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  if assigned(FParameter) then
    if FImageCount > 0 then begin
      FImageList.BitmapWidth := FImageWidth;
      FImageList.ParseImageData( FImageCount, True)
    end;
end;

procedure TG2GraphButtonFlat.SetValueByCtrlMidi( aMidiEditorAssignment: TMidiEditorAssignment; aMidiEvent: TMyMidiEvent);
var MidiMessage, MidiValue1, MidiValue2 : byte;
    CtrlRange, MidiRange : byte;
begin
  if assigned(aMidiEditorAssignment) then begin
    MidiMessage := aMidiEvent.MidiMessage shr 4;
    MidiValue1 := aMidiEvent.Data1;
    MidiValue2 := aMidiEvent.Data2;

    MidiReceiving := True;
    try
      if aMidiEditorAssignment.Note <> 0 then begin
        if MidiMessage = $09 then begin
          if Value = HighValue then
            Value := LowValue
          else
            Value := Value + 1;
        end;
      end else begin
        if Value = HighValue then
          Value := LowValue
        else
          Value := Value + 1;
      end;
    finally
      MidiReceiving := False;
    end;

    if assigned(FOnClick) then FOnClick( self);
  end;
end;

procedure TG2GraphButtonFlat.MouseDown( Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ( ssLeft in Shift) then begin
    if Value + 1 > HighValue then
      Value := LowValue
    else
      Value := Value + 1;
  end;
  inherited;
end;

// ==== TG2GraphLevelShift =====================================================

constructor TG2GraphLevelShift.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2GraphLevelShift.Destroy;
begin
  inherited;
end;

procedure TG2GraphLevelShift.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, RectTop, RectBottom : TRect;
    HorzCenter, VertCenter: integer;
    P1, P2 : TPoint;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  inherited;

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

    ExtCanvas.Font.Assign( Font);

    ExtCanvas.Brush.Color := CL_BTN_FACE;
    ExtCanvas.FillRect(Rect);

    HorzCenter := (Rect.Right + Rect.Left) div 2;
    VertCenter := (Rect.Top + Rect.Bottom) div 2;

    RectTop.Top  := Rect.Top;
    RectTop.Left := HorzCenter - 4;
    RectTop.Right := HorzCenter + 5;
    RectTop.Bottom := VertCenter;

    RectBottom.Top  := VertCenter;
    RectBottom.Left := HorzCenter - 4;
    RectBottom.Right := HorzCenter + 5;
    RectBottom.Bottom := Rect.Bottom;

    ExtCanvas.Pen.Color := clBlack;
    case Value of
    0 : begin
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := CL_BTN_FACE;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectTop.Top + 1;
          P2.X := HorzCenter; P2.Y := RectTop.Bottom;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 0, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    1 : begin
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := CL_BTN_FACE;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectTop.Bottom - 1;
          P2.X := HorzCenter; P2.Y := RectTop.Top;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 1, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    2 : begin
          ExtCanvas.Brush.Color :=CL_BTN_FACE;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectBottom.Top;
          P2.X := HorzCenter; P2.Y := RectBottom.Bottom - 1;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 0, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    3 : begin
          ExtCanvas.Brush.Color := CL_BTN_FACE;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectBottom.Bottom - 2;
          P2.X := HorzCenter; P2.Y := RectBottom.Top - 1;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 1, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    4 : begin
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectTop.Top + 1;
          P2.X := HorzCenter; P2.Y := RectBottom.Bottom - 2;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 0, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
          ExtCanvas.Pen.Color := CL_BTN_FACE;
          P1.X := Rect.Left; P1.Y := VertCenter;
          P2.X := Rect.Right; P2.Y := VertCenter;
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    5 : begin
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectTop);
          ExtCanvas.Brush.Color := clLime;
          ExtCanvas.FillRect(RectBottom);
          P1.X := HorzCenter; P1.Y := RectBottom.Bottom - 2;
          P2.X := HorzCenter; P2.Y := RectTop.Top + 1;
          ExtCanvas.Brush.Color := clBlack;
          DrawArrowHead( P1, 2, 1, ExtCanvas);
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
          ExtCanvas.Pen.Color := CL_BTN_FACE;
          P1.X := Rect.Left; P1.Y := VertCenter - 1;
          P2.X := Rect.Right; P2.Y := VertCenter - 1;
          ExtCanvas.MoveTo(P1.X, P1.Y);
          ExtCanvas.LineTo(P2.X, P2.Y);
        end;
    end;

    if Selected then
      ExtCanvas.Pen.Color := CL_SELECTED_PARAM
    else
      ExtCanvas.Pen.Color := CL_BTN_BORDER;

    DrawRect( ExtCanvas, Rect);

    if MidiAware and ShowMidiBox then begin
      if assigned(FMidiEditorAssignmentList) then
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControl(self)
      else
        MidiEditorAssignment := nil;
      DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
    end;
  finally
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;


// ==== TG2GraphButtonRadio ====================================================

constructor TG2GraphButtonRadio.Create(AOwner: TComponent);
begin
  inherited;
  FHighlightColor := G_HighLightColor;
  Color := CL_BTN_FACE;
  FBevel := False;
  FUpsideDown := False;
  Height := 14;
  IndexedControl := True;
end;

destructor TG2GraphButtonRadio.Destroy;
begin
  inherited;
end;

procedure TG2GraphButtonRadio.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    if FOrientation = otHorizontal then begin
      if (FButtonCount > 0) and (Width > 0) then begin
        Value := X * FButtonCount div Width;
      end;
    end else begin
      if (FButtonCount > 0) and (Height > 0) then begin
        if FUpsideDown then
          Value := (Height - Y) * FButtonText.Count div Height
        else
          Value := Y * FButtonText.Count div Height;
      end;
    end;
  end;
  inherited;
end;

procedure TG2GraphButtonRadio.PaintOn(ExtCanvas: TCanvas;  ExtBoundsRect: TRect);
var ExtRect, Rect : TRect;
    Bitmap : TBitmap;
    i : integer;
    LabelText : string;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  ExtRect := SubRect( GetRelToParentRect, ExtBoundsRect);

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  Bitmap := TBitmap.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.Pen.Color := FBorderColor;
    Bitmap.Canvas.Font.Assign( Font);

    if FButtonCount > 0 then begin

      if FOrientation = otHorizontal then begin
        FButtonWidth := Width div FButtonCount + 1;
        Rect.Left := 0;
        Rect.Top := 0;
        Rect.Right := FButtonWidth;
        Rect.Bottom := Height;
      end else begin
        FButtonHeight := (Height div FButtonCount) + 1;
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
          Bitmap.Canvas.Brush.Color := FHighlightColor
        else
          Bitmap.Canvas.Brush.Color := Color;
        Bitmap.Canvas.FillRect( Rect);

        if FImageList.Count > 0 then begin
          Bitmap.Canvas.FillRect( Rect);
          if i < FImageList.Count then
            DrawCenter( Bitmap.Canvas, Rect, FImageList.Items[i]);
        end else begin
          LabelText := '';
          if assigned(Parameter) then
            LabelText := Parameter.ButtonText[i]
          else
            if i < FButtonText.Count then
              LabelText := FButtonText[i];
          TextCenter( Bitmap.Canvas, Rect, LabelText);
        end;

        if FBevel then begin
          if Selected then
            Bitmap.Canvas.Brush.Color := CL_SELECTED_PARAM
          else
            Bitmap.Canvas.Brush.Color := Color;

          if i <> Value then begin
            DrawBevel( Bitmap.Canvas, Rect, bvRaised);
          end else begin
            DrawBevel( Bitmap.Canvas, Rect, bvNone);
          end;
        end else begin
          if Selected then
            Bitmap.Canvas.Pen.Color := CL_SELECTED_PARAM
          else
            Bitmap.Canvas.Pen.Color := CL_BTN_BORDER;

          DrawRect( Bitmap.Canvas, Rect);
        end;

        if MidiAware and ShowMidiBox then begin
          if assigned(FMidiEditorAssignmentList) then
            MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasIndex(self, i)
          else
            MidiEditorAssignment := nil;
          DrawMidiAwareBox( Bitmap.Canvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment)
        end;

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

    end else
      Bitmap.Canvas.Rectangle( ClientRect);

    ExtCanvas.Draw( ExtRect.Left, ExtRect.Top, Bitmap);

  finally
    Bitmap.Free;

    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

procedure TG2GraphButtonRadio.ParsePanelData( fs: TModuleDefStream);

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

function TG2GraphButtonRadio.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
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

procedure TG2GraphButtonRadio.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if FOrientation = otHorizontal then begin
    if FButtonCount > 0 then
      FButtonWidth := (AWidth div FButtonCount) + 1
    else
      FButtonWidth := 0;
  end else begin
    if FButtonCount > 0 then
      FButtonHeight := AHeight div FButtonCount + 1
    else
      FButtonHeight := 0;
  end;
end;

procedure TG2GraphButtonRadio.SetButtonCount( aValue: integer);
begin
  FButtonCount := aValue;
  FLowValue := 0;
  FHighValue := aValue - 1;
  Invalidate;
end;

procedure TG2GraphButtonRadio.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  Invalidate;
end;

procedure TG2GraphButtonRadio.SetValueByCtrlMidi( aMidiEditorAssignment: TMidiEditorAssignment; aMidiEvent: TMyMidiEvent);
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
end;

// ==== TG2GraphButtonRadioEdit ================================================

constructor TG2GraphButtonRadioEdit.Create(AOwner: TComponent);
begin
  inherited;
  FButtonRows := 1;
  FButtonColumns := 1;
  FClickedButton := -1;
  FIndexedControl := True;
end;

destructor TG2GraphButtonRadioEdit.Destroy;
begin
  inherited;
end;

function TG2GraphButtonRadioEdit.GetParamLabelIndex: integer;
begin
  Result := FClickedButton;
end;

function TG2GraphButtonRadioEdit.GetScreenCoordsRect: TRect;
var r, c, l, t : integer;
begin
  if FClickedButton <> -1 then begin
    r := FClickedButton div FButtonColumns;
    c := FClickedButton mod FButtonColumns;
    FButtonWidth := Width div FButtonColumns;
    FButtonHeight := Height div FButtonRows;
    l := c * FButtonWidth + Left;
    t := r * FButtonHeight + Top;

    if assigned(FModule) then begin
      Result.Left := FModule.ClientToScreen(Point(l, t)).X;
      Result.Right := FModule.ClientToScreen(Point(l + FButtonWidth, t + FButtonHeight)).X;
      Result.Top := FModule.ClientToScreen(Point(l, t)).Y;
      Result.Bottom := FModule.ClientToScreen(Point(l + FButtonWidth, t + FButtonHeight)).Y;
    end else begin
      Result.Left := l;
      Result.Right := l + FButtonWidth;
      Result.Top := t;
      Result.Bottom := t + FButtonHeight;
    end;
  end else
    inherited GetScreenCoordsRect;
end;

procedure TG2GraphButtonRadioEdit.MouseDown(Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
var c, r : integer;
begin
  if (FButtonColumns > 0) and (Width > 0) and (FButtonRows > 0) and (Height > 0) then begin
    c := X * FButtonColumns div Width;
    r := Y * FButtonRows div Height;
    FClickedButton := r * FButtonColumns + c;
    if ssLeft in Shift then begin
      Value := FClickedButton;
    end else
      inherited;
  end else
    FClickedButton := -1;
end;

procedure TG2GraphButtonRadioEdit.PaintOn(ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var ExtRect, Rect : TRect;
    Bitmap : TBitmap;
    i, r, c : integer;
    LabelText : string;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  ExtRect := SubRect( GetRelToParentRect, ExtBoundsRect);

  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  Bitmap := TBitmap.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    Bitmap.Canvas.Brush.Color := Color;
    Bitmap.Canvas.Pen.Color := FBorderColor;
    Bitmap.Canvas.Font.Assign( Font);

    FButtonWidth := Width div FButtonColumns;
    FButtonHeight := Height div FButtonRows;
    Rect.Top := 0;
    Rect.Bottom := FButtonHeight;
    i := 0;
    for r := 0 to FButtonRows - 1 do begin
      Rect.Left := 0;
      Rect.Right := FButtonWidth;
      for c := 0 to FButtonColumns - 1 do begin

        if i = Value then
          Bitmap.Canvas.Brush.Color := FHighlightColor
        else
          Bitmap.Canvas.Brush.Color := Color;
        Bitmap.Canvas.FillRect( Rect);

        LabelText := '';
        if assigned(Parameter) then
          LabelText := ParamLabel[i]
        else
          if i < FButtonText.Count then
            LabelText := FButtonText[i];
        TextCenter( Bitmap.Canvas, Rect, LabelText);

        if FBevel then begin
          if Selected then
            Bitmap.Canvas.Brush.Color := CL_SELECTED_PARAM
          else
            Bitmap.Canvas.Brush.Color := Color;

          if i <> Value then begin
            DrawBevel( Bitmap.Canvas, Rect, bvRaised);
          end else begin
            DrawBevel( Bitmap.Canvas, Rect, bvNone);
          end;
        end else begin
          if Selected then
            Bitmap.Canvas.Pen.Color := CL_SELECTED_PARAM
          else
            Bitmap.Canvas.Pen.Color := CL_BTN_BORDER;

          DrawRect( Bitmap.Canvas, Rect);
        end;

        Rect.Left := Rect.Left + FButtonWidth - 1;
        Rect.Right := Rect.Right + FButtonWidth;

        inc(i);
      end;

      if MidiAware and ShowMidiBox then begin
        if (FMidiEditorAssignmentList <> nil) then
          MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasIndex( self, i)
        else
          MidiEditorAssignment := nil;
        DrawMidiAwareBox( Bitmap.Canvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
      end;

      Rect.Top := Rect.Top + FButtonHeight - 1;
      Rect.Bottom := Rect.Bottom + FButtonHeight;
    end;

    ExtCanvas.Draw( ExtRect.Left, ExtRect.Top, Bitmap);

  finally
    Bitmap.Free;

    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

procedure TG2GraphButtonRadioEdit.ParsePanelData(fs: TModuleDefStream);
begin
  inherited;
end;

function TG2GraphButtonRadioEdit.ParseProperties(fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin
    if aName = 'ButtonColumns' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonColumns := StrToInt(string(aValue));
      Width := FButtonColumns * 43;
    end else

    if aName = 'ButtonRows' then begin
      aValue := fs.ReadUntil( [#13]);
      FButtonRows := StrToInt(string(aValue));
      Height := FButtonRows * 12;
    end else
      Result := False

  end;
end;

procedure TG2GraphButtonRadioEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

// ==== TG2GraphButtonIncDec ===================================================

constructor TG2GraphButtonIncDec.Create(AOwner: TComponent);
begin
  inherited;

  FButtonCount := 2;
  FButtonWidth := 10;
  FButtonHeight := 11;
  FButtonPressed := -1;
  FBevel := True;
  Color := CL_BTN_FACE;
  FIndexedControl := True;
end;

destructor TG2GraphButtonIncDec.Destroy;
begin
  inherited;
end;

procedure TG2GraphButtonIncDec.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ( ssLeft in Shift) then begin
    if FOrientation = otHorizontal then begin
      FButtonPressed := X div FButtonWidth;
    end else begin
      FButtonPressed := (2 * FButtonHeight - Y) div FButtonHeight;
    end;

    case FButtonPressed of
    0 : if Value > LowValue then
          Value := Value - 1;
    1 : if Value < HighValue then
          Value := Value + 1;
    end;
  end;
  inherited;
end;

procedure TG2GraphButtonIncDec.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FButtonPressed := -1;

  inherited;
end;

procedure TG2GraphButtonIncDec.PaintOn(ExtCanvas: TCanvas; ExtBoundsRect: TRect);
var i : integer;
    Rect, Rect2, IconRect : TRect;
    MidiEditorAssignment : TMidiEditorAssignment;
    OrgBrush : TBrush;
    OrgPen : TPen;
    OrgFont : TFont;
begin
  OrgBrush := TBrush.Create;
  OrgPen := TPen.Create;
  OrgFont := TFont.Create;
  try
    OrgBrush.Assign(ExtCanvas.Brush);
    OrgPen.Assign(ExtCanvas.Pen);
    OrgFont.Assign(ExtCanvas.Font);

    Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

    Rect2.Left := 0;
    Rect2.Top := 0;
    Rect2.Right := FButtonWidth;
    Rect2.Bottom := FButtonHeight;

    Rect2 := AddRect( Rect2, Rect);

    ExtCanvas.Font.Assign( Font);

    for i := 0 to FButtonCount - 1 do begin
      ExtCanvas.Brush.Color := Color;

      ExtCanvas.FillRect( Rect2);

      ExtCanvas.Brush.Color := clBlack;
      case FOrientation of
        otHorizontal:
          begin;
            with IconRect do begin
              Left := 0;
              Right := 4;
              Top := 0;
              Bottom := 2;
            end;
            case i of
            0 : DrawIcon( ExtCanvas, Rect2, IconRect, itDown);
            1 : DrawIcon( ExtCanvas, Rect2, IconRect, itUp);
            end;
          end;
        otVertical:
          begin;
            with IconRect do begin
              Left := 0;
              Right := 4;
              Top := 0;
              Bottom := 2;
            end;
            case i of
            0 : DrawIcon( ExtCanvas, Rect2, IconRect, itUp);
            1 : DrawIcon( ExtCanvas, Rect2, IconRect, itDown);
            end;
          end;
      end;

      DrawRect( ExtCanvas, Rect2);

      if Selected then
        ExtCanvas.Brush.Color := CL_SELECTED_PARAM
      else
        ExtCanvas.Brush.Color := Color;

      if i <> FButtonPressed then
        DrawBevel( ExtCanvas, Rect2, bvRaised)
      else
        DrawBevel( ExtCanvas, Rect2, bvNone);

      if MidiAware and ShowMidiBox then begin
        if assigned(FMidiEditorAssignmentList) then
          MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasIndex( self, i)
        else
          MidiEditorAssignment := nil;
        DrawMidiAwareBox( ExtCanvas, MakeRect( Rect2.Left, Rect2.Top, 16, 8), MidiEditorAssignment);
      end;

      if FOrientation = otHorizontal then begin
        Rect2.Left := Rect2.Left + FButtonWidth;
        Rect2.Right := Rect2.Right + FButtonWidth;
      end else begin
        Rect2.Top := Rect2.Top + FButtonHeight;
        Rect2.Bottom := Rect2.Bottom + FButtonHeight;
      end;
    end;

    {if (FMidiEditorAssignmentList <> nil) then begin
      MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasCC( self);
      if assigned(MidiEditorAssignment) then
        DrawMidiAwareBox( ExtCanvas, MakeRect( Rect.Left, Rect.Top, 16, 8), MidiEditorAssignment);
    end;}
  finally
    ExtCanvas.Brush.Assign(OrgBrush);
    ExtCanvas.Pen.Assign(OrgPen);
    ExtCanvas.Font.Assign(OrgFont);
    OrgBrush.Free;
    OrgPen.Free;
    OrgFont.Free;
  end;
end;

procedure TG2GraphButtonIncDec.ParsePanelData( fs: TModuleDefStream);

    procedure CalcSize;
    begin
      if FButtonCount > 0 then begin
        if FOrientation = otHorizontal then begin
          SetBounds( Left, Top, FButtonWidth * FButtonCount, FButtonHeight);
        end else begin
          SetBounds( Left, Top, FButtonWidth, FButtonHeight * FButtonCount);
        end;
      end;
    end;

begin
  inherited;

  FButtonWidth := 10;
  FButtonHeight := 11;
  CalcSize;
end;

function TG2GraphButtonIncDec.ParseProperties( fs: TModuleDefStream; aName: AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Left/Right"' then
        FOrientation := otHorizontal
      else
        FOrientation := otVertical;
    end else
      Result := False

  end;
end;

procedure TG2GraphButtonIncDec.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if (FOrientation = otHorizontal) then begin
    if FButtonCount > 0 then
      FButtonWidth := AWidth div FButtonCount
    else
      FButtonWidth := 0;
    FButtonHeight := AHeight;
  end else begin
    if FButtonCount > 0 then
      FButtonHeight := AHeight div FButtonCount
    else
      FButtonHeight := 0;
    FButtonWidth := AWidth;
  end;

  inherited;
end;

procedure TG2GraphButtonIncDec.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  Invalidate;
end;

// ==== TG2GraphDropDownList ===================================================

constructor TG2GraphDropDownList.Create( AOwner: TComponent);
begin
  inherited;

  FPartSelector := nil;
  FMouseOverItem := 0;
end;

destructor TG2GraphDropDownList.Destroy;
begin
  inherited;
end;

procedure TG2GraphDropDownList.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //
end;

procedure TG2GraphDropDownList.MouseMove(Shift: TShiftState; X, Y: Integer);
var i : integer;
begin
  if Visible then begin
    i := Y div (Height div (FPartSelector.HighValue + 1));
    if (i >= 0) and (i <= FPartSelector.HighValue) then begin
      FMouseOverItem := i;
      Invalidate;
    end;
  end;
end;

procedure TG2GraphDropDownList.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : integer;
begin
  if Button = mbLeft then begin
    i := Y div (Height div (FPartSelector.HighValue + 1));
    if (i >= 0) and (i <= FPartSelector.HighValue) then begin
      FPartSelector.Value := i;
      FPartSelector.Invalidate;
    end;

    FDropDownList.Free;
    FDropDownList := nil;
  end;
end;

procedure TG2GraphDropDownList.Paint;
begin
end;

procedure TG2GraphDropDownList.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var i, h : integer;
    Rect, Rect2 : TRect;
begin
  //AddLogLine('Paint dropdown');

  inherited;

  Rect := SubRect( BoundsRect, ExtBoundsRect);

  h := (FPartSelector.ClientRect.Bottom - FPartSelector.ClientRect.Top);
  for i := 0 to FPartSelector.HighValue do begin
    Rect2.Left := 0;
    Rect2.Right := ClientRect.Right;
    Rect2.Top := i * h;
    Rect2.Bottom := i * h + h;
    Rect2 := AddRect( Rect2, Rect);

    if FMouseOverItem = i then begin
      ExtCanvas.Brush.Color := clBtnShadow;
    end else begin
      ExtCanvas.Brush.Color := clBtnFace;
    end;
    ExtCanvas.FillRect(Rect2);

    ExtCanvas.Draw( Rect2.Left + 1, Rect2.Top + 1, FPartSelector.FImageList.Items[ i]);
  end;
end;


// ==== TG2GraphPartSelector ===================================================

constructor TG2GraphPartSelector.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := True;

  FImageList := TG2ImageList.Create( True);

  Width := trunc(UNITS_ROW * 1.6);
  Height := Width;
  FButtonWidth := 9;
end;

destructor TG2GraphPartSelector.Destroy;
begin
  FImageList.Free;
  inherited;
end;

procedure TG2GraphPartSelector.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if ( ssLeft in Shift) then begin
    if PointInRect( X, Y, FButtonRect) then begin
      DropDown( self);
    end else
      if assigned(FDropDownList) then
        FDropDownList.Visible := False;
  end;
end;

procedure TG2GraphPartSelector.DropDown( Sender: TObject);
begin
  if assigned( FDropDownList) then
    FDropDownList.Free;

  FDropDownList := TG2GraphDropDownList.Create( self);
  FDropDownList.FPartSelector := self;
  if assigned(FModule) then begin
    FDropDownList.Parent := FModule.Parent;
    FDropDownList.SetBounds( FModule.Left + Left,
                             FModule.Top + Top + Height,
                             ClientRect.Right - ClientRect.Left - FButtonWidth,
                             Height * (HighValue + 1));
  end else begin
    FDropDownList.Parent := Parent;
    FDropDownList.SetBounds( Left,
                             Top + Height,
                             ClientRect.Right - ClientRect.Left - FButtonWidth,
                             Height * (HighValue + 1));

  end;
  //FDropDownList.Invalidate;
  FDropDownList.Parent.Invalidate;
end;

procedure TG2GraphPartSelector.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, Rect2, Rect3, IconRect : TRect;
begin
  //AddLogLine('Paint partselector');

  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  ExtCanvas.Brush.Color := clBtnFace;
  ExtCanvas.Pen.Color := clBlack;
  ExtCanvas.FillRect( Rect);
  ExtCanvas.Rectangle( Rect);

  Rect2 := AddRect( FButtonRect, Rect);
  ExtCanvas.Rectangle( Rect2);

  Rect3.Left := 0;
  Rect3.Top := 0;
  Rect3.Right := FButtonRect.Right - FButtonRect.Left;
  Rect3.Bottom := FButtonRect.Bottom - FButtonRect.Top;
  ShrinkRect( Rect3, 3);
  SquareRect( Rect3);
  ExtCanvas.Brush.Color := clBlack;
  with IconRect do begin
    Left := 0;
    Right := 4;
    Top := 0;
    Bottom := 2;
  end;
  DrawIcon( ExtCanvas, AddRect( FButtonRect, Rect), IconRect, itDown);
  //Canvas.Rectangle( Rect);

  Rect2 := Rect;
  ShrinkRect( Rect2, 1);
  SquareRect( Rect2);
  ExtCanvas.Brush.Color := clBlack;
  if ( Value >= LowValue) and ( Value <= HighValue) then
    ExtCanvas.Draw( Rect.Left + 1, Rect.Top + 2, FImageList.Items[ Value]);
end;

procedure TG2GraphPartSelector.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;


  if assigned( FButton) then begin
    FButton.SetBounds( ALeft + AWidth,
                       ATop,
                       FButtonWidth,
                       AHeight);
  end;

  FButtonRect.Left := Width - FButtonWidth;
  FButtonRect.Top := 0;
  FButtonRect.Right := Width;
  FButtonRect.Bottom := Height;
end;

procedure TG2GraphPartSelector.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  FImageList.BitmapWidth := FImageWidth;
  FImageList.ParseImageData( FImageCount, True);
  SetBounds(Left, Top, Width, Height);
end;

function TG2GraphPartSelector.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    {if aName = 'Text' then begin
      ReadConst( fs, '"');
      ReadOptions( fs, sl, [','], ['"']);
    end else}

    if aName = 'Image' then begin
      fs.ReadConst( '"');
      fs.ReadOptions( FImageList.FImageData, [':'], ['"']);
    end else

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FParameter := FModule.FData.Mode[ StrToInt(string(aValue))] as TG2GraphParameter;
    end else

    if aName = 'InfoFunc' then begin
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

    if aName = 'MenuOffset' then begin
      aValue := fs.ReadUntil( [#13]);
    end else

      Result := False

  end;
end;

// ==== TG2GraphKnob ===========================================================

constructor TG2GraphKnob.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := True;

  Color := clWhite;

  FOrientation := otVertical;

  Width := 22;
  Height := Width;
  FCenterButtonSize := 5;
  FSliderSize := 10;

  FHighLightColor := G_HighLightColor;


  FValue := 0;
  FLowValue := 0;
  FHighValue := 127;
end;

destructor TG2GraphKnob.Destroy;
begin
  inherited;
end;

procedure TG2GraphKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  FSliderSelected := False;
  if ( ssLeft in Shift) then begin

    FStartX := X;
    FStartY := Y;
    FStartValue := Value;

    if assigned(Parameter) and (Parameter.ParamType <> ptParam) then
      Parameter.SuspendUpdate := True;

    if (FType in [ktSlider, ktSeqSlider]) then begin
      if (PointInRect( X, Y, GetSliderRect)) then begin
        FSLiderSelected := True;
      end;
    end else
      if (FType in [ktReset, ktResetMedium]) and PointInRect( X, Y, FCenterButtonRect) then begin
        Value := (HighValue - LowValue + 1) div 2;
      end else
        if PointInRect( X, Y, FIncBtnRect) then begin
          if Value < HighValue then
            Value := Value + 1;
        end else
          if PointInRect( X, Y, FDecBtnRect) then begin
            if Value > LowValue then
              Value := Value - 1;
          end;
  end;
  inherited;
end;

procedure TG2GraphKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var FdX, FdY, ValueRange : integer;
begin
  if (ssLeft in Shift) then begin

    FdX := X - FStartX;
    FdY := Y - FStartY;
    ValueRange := HighValue - LowValue;

    if ( FType in [ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium]) then begin
      if ssCtrl in Shift then
        SetMorphValue( CheckValueBounds( FStartValue + trunc(ValueRange * FdX / 60)) - Value)
      else
        Value := CheckValueBounds( FStartValue + trunc(ValueRange * FdX / 60));
    end else
      if ( FType in [ktSlider, ktSeqSlider]) and FSliderSelected then begin
        if ssCtrl in Shift then begin
          if (FOrientation = otVertical) then begin
            if (Height - FSliderSize > 0) then
              SetMorphValue( CheckValueBounds( FStartValue + trunc(ValueRange * -FdY / (Height - FSliderSize))) - Value);
          end else
            if (Width - FSliderSize > 0) then
              SetMorphValue( CheckValueBounds( FStartValue + trunc(ValueRange * FdX / (Width - FSliderSize))) - Value);
        end else begin
          if (FOrientation = otVertical) then begin
            if (Height - FSliderSize > 0) then
              Value := CheckValueBounds( FStartValue + trunc(ValueRange * -FdY / (Height - FSliderSize)));
          end else
            if (Width - FSliderSize > 0) then
              Value := CheckValueBounds( FStartValue + trunc(ValueRange * FdX / (Width - FSliderSize)));
        end;
      end;
    Invalidate;
  end;
  inherited;
end;

procedure TG2GraphKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  if assigned(Parameter) and Parameter.SuspendUpdate then
    Parameter.SuspendUpdate := False;

  FSliderSelected := False;
  inherited;
end;

procedure TG2GraphKnob.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var mx, my, r : integer;
    rad, mrad, X, Y : single;
    p1, p2, p3 : TPoint;
    Rect, IconRect, MorphRect : TRect;
    MorphParameter : TMorphParameter;
    FastBitmap : TFastbitmap;
    BitMap : TBitmap;
    MidiEditorAssignment : TMidiEditorAssignment;
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

    if MidiAware and ShowMidiBox then begin
      if assigned(FMidiEditorAssignmentList) then
        MidiEditorAssignment := FMidiEditorAssignmentList.FindControlHasCC( self)
      else
        MidiEditorAssignment := nil;
      DrawMidiAwareBox( Bitmap.Canvas, MakeRect( FKnobRect.Left, FKnobRect.Top, 16, 8), MidiEditorAssignment);
    end;

    ExtCanvas.Draw( Rect.Left, Rect.Top, Bitmap);

  finally
    Bitmap.Free;
    Fastbitmap.Free;
  end;
end;

function TG2GraphKnob.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
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

function TG2GraphKnob.GetSliderRect: TRect;
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

function TG2GraphKnob.GetMorphRect: TRect;
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

procedure TG2GraphKnob.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FKnobRect.Left := AWidth div 2 - FKnobRad;
  FKnobRect.Right := AWidth div 2 + FKnobRad;
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

  FCenterButtonRect.Left := (AWidth - FCenterButtonSize*2) div 2;
  FCenterButtonRect.Top := 0;
  FCenterButtonRect.Right := (AWidth + FCenterButtonSize*2) div 2;
  FCenterButtonRect.Bottom := FCenterButtonSize;

  FIncBtnRect.Left := AWidth div 2;
  FIncBtnRect.Top := AHeight - 9;
  FIncBtnRect.Right := AWidth div 2 + 11;
  FIncBtnRect.Bottom := AHeight;

  FDecBtnRect.Left := AWidth div 2 - 10;
  FDecBtnRect.Top := AHeight - 9;
  FDecBtnRect.Right := AWidth div 2 + 1;
  FDecBtnRect.Bottom := AHeight;

  inherited;
end;

procedure TG2GraphKnob.SetKnobType( aValue: TKnobType);
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
  SetBounds(Left, Top, Width, Height);
  Invalidate;
end;

procedure TG2GraphKnob.SetOrientation( aValue: TOrientationType);
begin
  FOrientation := aValue;
  Invalidate;
end;

procedure TG2GraphKnob.SetHighLightColor( aValue: TColor);
begin
  FHighlightColor := aValue;
  Invalidate;
end;

// ==== TGraphConnector ========================================================

constructor TG2GraphConnector.Create(AOwner: TComponent);
begin
  inherited;

  FMouseInput := True;

  FData := nil;

  Width := 13;
  Height := 13;
end;

destructor TG2GraphConnector.Destroy;
begin
  inherited;
end;

procedure TG2GraphConnector.MouseDown(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
begin
  inherited;

  if (ssLeft in Shift) then begin
    FNewCable := TG2GraphCable.Create( FModule.FData.PatchPart);
    FNewCable.Fx1 := Left + FModule.ScrollPosX + Width div 2;
    FNewCable.Fy1 := Top  + FModule.ScrollPosY + Height div 2;
    FNewCable.Fx2 := Left + FModule.ScrollPosX + X;
    FNewCable.Fy2 := Top  + FModule.ScrollPosY + Y;
    FNewCable.InitCable;
    FNewCable.CableColor := Data.ConnectorColor;
    if assigned( FModule) and assigned( FModule.Parent) then
      (FModule.Parent as TG2GRaphScrollbox).DrawLine(FNewCable.Fx1 - FModule.ScrollBarX,
                                                     FNewCable.Fy1 - FModule.ScrollBarY,
                                                     FNewCable.Fx2 - FModule.ScrollBarX,
                                                     FNewCable.Fy2 - FModule.ScrollBarY);
    //FModule.Parent.Hint := 'Connector # ' + IntToStr(FConnectorIndex);
    //FModule.Parent.ShowHint := True;
  end;
end;

procedure TG2GraphConnector.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if ( ssLeft in Shift) and ( assigned(FNewCable)) then begin
    if assigned( FModule) and assigned( FModule.Parent) then
      (FModule.Parent as TG2GRaphScrollbox).DrawLine(FNewCable.Fx1 - FModule.ScrollBarX,
                                                     FNewCable.Fy1 - FModule.ScrollBarY,
                                                     FNewCable.Fx2 - FModule.ScrollBarX,
                                                     FNewCable.Fy2 - FModule.ScrollBarY);
    FNewCable.CableResize( FNewCable.Fx1,
                           FNewCable.Fy1,
                           Left + FModule.ScrollPosX + X,
                           Top + FModule.ScrollPosY + Y);
    if assigned( FModule) and assigned( FModule.Parent) then
      (FModule.Parent as TG2GRaphScrollbox).DrawLine(FNewCable.Fx1 - FModule.ScrollBarX,
                                                     FNewCable.Fy1 - FModule.ScrollBarY,
                                                     FNewCable.Fx2 - FModule.ScrollBarX,
                                                     FNewCable.Fy2 - FModule.ScrollBarY);
  end;
end;

procedure TG2GraphConnector.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ToConnector : TG2GraphConnector;
begin
  inherited;

  if assigned(FNewCable) then begin
    if assigned( FModule) and assigned( FModule.Parent) then
      (FModule.Parent as TG2GRaphScrollbox).DrawLine(FNewCable.Fx1 - FModule.ScrollBarX,
                                                     FNewCable.Fy1 - FModule.ScrollBarY,
                                                     FNewCable.Fx2 - FModule.ScrollBarX,
                                                     FNewCable.Fy2 - FModule.ScrollBarY);
    FNewCable.Free;
    FNewCable := nil;

    ToConnector := GetConnector( FModule.Parent);
    if ( ToConnector <> nil) and ( ToConnector <> self) then
      FModule.FData.PatchPart.Patch.MessAddConnection( FModule.Location, Data, ToCOnnector.Data);
  end;
end;

procedure TG2GraphConnector.PaintOn( ExtCanvas : TCanvas; ExtBoundsRect : TRect);
var Rect, Rect2 : TRect;
    r, hw : integer;
    InnerColor, BorderColor : TColor;
    Bitmap : TBitmap;
    Fastbitmap : TFastBitmap;
begin
  //AddLogLine('Paint connector');

  inherited;

  Rect := SubRect( GetRelToParentRect, ExtBoundsRect);

  Bitmap := TBitmap.Create;
  Fastbitmap := TFastbitmap.Create;
  try
    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Rect.Right - Rect.Left;
    Bitmap.Height := Rect.Bottom - Rect.Top;

    Rect2 := ClientRect;
    r := (Rect.Right - Rect.Left) div 2;
    hw := Width div 4;

    Bitmap.Canvas.Brush.Color := FModule.Color;
    Bitmap.Canvas.Brush.Style := bsSolid;
    Bitmap.Canvas.FillRect(ClientRect);

    InnerColor := CableColors[ Data.ConnectorColor];
    BorderColor := Darker( CableColors[ Data.ConnectorColor], 70);

    if Data.ConnectorKind = ckInput then begin

//      DrawDisk( Bitmap, r, r, r - 1, 2, InnerColor, bsSolid);
//      DrawDisk( Bitmap, r, r, r - 1, 2, BorderColor, bsClear);
      Fastbitmap.CopyFromBitmap( Bitmap);
      FastBitmap.DrawAntialisedDisk(r, r, r - 1, 2, InnerColor, bsSolid);
      FastBitmap.DrawAntialisedDisk(r, r, r - 1, 2, BorderColor, bsClear);
      // Don't draw a hole when the connector has cables
      if Data.CableCount = 0 then
        FastBitmap.DrawAntialisedDisk(r, r, r - hw, 1, clBlack, bsSolid);
      FastBitmap.CopyToBitmap(Bitmap);
    end else
      if Data.ConnectorKind = ckOutput then begin
        Bitmap.Canvas.Brush.Color := InnerColor;
        Bitmap.Canvas.Pen.Color := BorderColor;
        ShrinkRect(Rect2, 1);
        BitMap.Canvas.FillRect( Rect2);
        Bitmap.Canvas.Rectangle( Rect2);
        if Data.CableCount = 0 then begin
          Fastbitmap.CopyFromBitmap( Bitmap);
          FastBitmap.DrawAntialisedDisk(r, r, r - hw, 1, clBlack, bsSolid);
          FastBitmap.CopyToBitmap(Bitmap);
        end;
      end;

    ExtCanvas.Draw( Rect.Left, Rect.Top, Bitmap);
  finally
    FastBitmap.Free;
    Bitmap.Free;
  end;
end;

procedure TG2GraphConnector.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if assigned(FData) then
    FData.InvalidateCables;
end;

procedure TG2GraphConnector.ParsePanelData( fs: TModuleDefStream);
begin
  inherited;

  FData.CalcDefColor;
end;

function TG2GraphConnector.ParseProperties( fs: TModuleDefStream; aName : AnsiString): boolean;
var aValue : AnsiString;
begin
  Result := True;

  if not inherited ParseProperties( fs, aName) then begin

    if aName = 'CodeRef' then begin
      aValue := fs.ReadUntil( [#13]);
      FData.ConnectorIndex := StrToInt(string(aValue));
    end else

    if aName = 'InfoFunc' then begin
      aValue := fs.ReadUntil( [#13]);
      //
    end else

    if aName = 'Type' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Audio"' then
        FData.ConnectorType := ctAudio
      else
        if aValue = '"Logic"' then
          FData.ConnectorType := ctLogic
        else
          if aValue = '"Control"' then
            FData.ConnectorType := ctControl
          else
            raise Exception.Create('Unknown control type ' + string(aValue));
    end else

    if aName = 'Bandwidth' then begin
      aValue := fs.ReadUntil( [#13]);
      if aValue = '"Static"' then
        FData.Bandwidth := btStatic
      else
        if aValue = '"Dynamic"' then
          FData.BandWidth := btDynamic
        else
          raise Exception.Create('Unknown bandwidth type ' + string(aValue));
    end else

      Result := False

  end;
end;

procedure TG2GraphConnector.SetData(aConnectorData: TG2FileConnector);
begin
  FData := aConnectorData;
  FData.GraphControl := self;
end;

// ==== TCableElements =========================================================

constructor TCableElement.Create;
begin
  inherited;
end;

destructor TCableElement.Destroy;
begin
  inherited;
end;

procedure TCableElement.Paint;
begin
end;

procedure TCableElement.PaintOn(ExtBitmap: TBitmap; ExtBoundsRect: TRect);
var ep : array[0..3] of TPoint;
    pp : array of TPoint;
    ml1, ml2 : TPoint;
    i : integer;
    Fastbitmap : TFastbitmap;
    Rect, CRect : TRect;
begin
  ExtBitmap.Canvas.Pen.Color := Color;
  ExtBitmap.Canvas.Brush.Color := Color;
  ExtBitmap.Canvas.Brush.Style := bsSolid;

{$IFNDEF FPC}
  // Anti aliasing very slow on linux
  Fastbitmap := TFastbitmap.Create;
  try
    Rect := SubRect( BoundsRect, ExtBoundsRect);
    CRect := SubRect( BoundsRect, CrossRect( BoundsRect, ExtBoundsRect));

    SetLength(pp, 5);
    for i := 0 to 3 do begin
      ep[i].x := p[i].x + (Left - ExtBoundsRect.Left);
      ep[i].y := p[i].y + (Top - ExtBoundsRect.Top);
      pp[i].x := p[i].x + CRect.Left;
      pp[i].y := p[i].y + CRect.Top;
    end;
    pp[4] := pp[0];

    ml1.X := p1.x + CRect.Left;
    ml1.Y := p1.y + CRect.Top;
    ml2.X := p2.x + CRect.Left;
    ml2.Y := p2.y + CRect.Top;

    //ExtBitmap.Canvas.Polygon( ep);

    //ExtBitmap.Canvas.Pen.Color := clYellow;
    //ExtBitmap.Canvas.FrameRect( Rect);
    FastBitmap.CopyFromBitmapRect(ExtBitmap, Rect);

//    FastBitmap.DrawPolygon(pp, Color);
//    FastBitmap.DrawAntialisedLine(p[0].x + CRect.Left, p[0].y + CRect.Top, p[1].x + CRect.Left, p[1].y + CRect.Top, ShadowColor);
//    FastBitmap.DrawAntialisedLine(p[2].x + CRect.Left, p[2].y + CRect.Top, p[3].x + CRect.Left, p[3].y + CRect.Top, ShadowColor);
    FastBitmap.LineSeg(pp, 4, ml1, ml2, G_CableThickness/2 , Color, 255);
//    FastBitmap.DrawAntialisedLine(ml1.x, ml1.y, ml2.X, ml2.Y, clBlack);

    FastBitmap.CopyToBitmapRect(ExtBitmap, Rect);
  finally
    Fastbitmap.Free;
  end;
{$ELSE}
  for i := 0 to 3 do begin
    ep[i].x := p[i].x + (Left - ExtBoundsRect.Left);
    ep[i].y := p[i].y + (Top - ExtBoundsRect.Top);
  end;
  ExtBitmap.Canvas.Polygon( ep);
  DrawWuLine( ExtBitmap, ep[0], ep[1], Darker(ExtBitmap.Canvas.Pen.Color, 50));
  DrawWuLine( ExtBitmap, ep[2], ep[3], ShadowColor);
{$ENDIF}
end;

// ==== TG2GraphCable ==========================================================

constructor TG2GraphCable.Create( AOwner : TComponent);
var i : integer;
begin
  //if not( AOwner is TG2GraphPatch) then
  //  raise Exception.Create('Owner must be a patch');

  //FPatch := AOwner as TG2GraphPatch;
  FParent := nil;

  FromConnector := nil;
  ToConnector   := nil;
  for i := 0 to NCABLE_ELEMENTS - 1 do
    FNode[i] := TCableElement.Create;

  Fx1 := Left;
  Fy1 := Top;
  Fx2 := Left;
  Fy2 := Top;

  InitCable;
end;

destructor TG2GraphCable.Destroy;
var i : integer;
begin
  for i := 0 to NCABLE_ELEMENTS - 1 do
    FNode[i].Free;

  // Remove the cable from the connectors
  if assigned(FromConnector) then
    FromConnector.DelCable( self);

  if assigned(ToConnector) then
    ToConnector.DelCable( self);

  inherited;
end;

function TG2GraphCable.GetBoundsRect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := FLeft + FWidth - 1;
  Result.Bottom := FTop + FHeight - 1;
end;

function TG2GraphCable.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := FWidth - 1;
  Result.Bottom := FHeight - 1;
end;

function TG2GraphCable.GetLeft: integer;
begin
  Result := FLeft;
end;

procedure TG2GraphCable.SetLeft( aValue: integer);
begin
  FLeft := aValue;
end;

function TG2GraphCable.GetTop: integer;
begin
  Result := FTop;
end;

procedure TG2GraphCable.SetTop( aValue: integer);
begin
  FTop := aValue;
end;

function TG2GraphCable.GetScrollBarX: integer;
begin
  if FParent <> nil then
    Result := TScrollbox(FParent).HorzScrollBar.Position
  else
    Result := 0;
end;

function TG2GraphCable.GetScrollBarY: integer;
begin
  if FParent <> nil then
    Result := TScrollbox(FParent).VertScrollBar.Position
  else
    Result := 0;
end;

procedure Tg2GraphCable.PaintElements;
var i, n : integer;
    pt1, pt2, pt3, ptn : TPointFloat;
    pts : array[0..(NCABLE_ELEMENTS*2)-1] of TPoint;
    Color : TColor;

    function GetPoint( i : integer): TPointFloat;
    begin
      if i > 0 then begin
        if i < NCABLE_ELEMENTS then begin
          Result.x := FNode[i].x;
          Result.y := FNode[i].y;
        end else begin
          Result.x := FNode[NCABLE_ELEMENTS-1].x;
          Result.y := FNode[NCABLE_ELEMENTS-1].y;
        end;
      end else begin
        Result.x := FNode[0].x;
        Result.y := FNode[0].y;
      end;
    end;

    procedure CalcOulinePoints( aWidth : single);
    begin
      aWidth := aWidth / 2;
      n := 0;
      pt1.x := GetPoint(n + 0).x - Left;
      pt1.y := GetPoint(n + 0).y - Top;
      pt2.x := GetPoint(n + 1).x - Left;
      pt2.y := GetPoint(n + 1).y - Top;
      pt3.x := GetPoint(n + 2).x - Left;
      pt3.y := GetPoint(n + 2).y - Top;
      ptn := GetUnitNormal(pt3, pt1);

      while (n < NCABLE_ELEMENTS - 1) do begin
        pts[n].x := trunc(pt1.x - ptn.x * aWidth);
        pts[n].y := trunc(pt1.y - ptn.y * aWidth);

        inc(n);

        pt1 := pt2;
        pt2 := pt3;
        pt3.x := GetPoint(n + 2).x - Left;
        pt3.y := GetPoint(n + 2).y - Top;
        if n < NCABLE_ELEMENTS - 1 then
          ptn := GetUnitNormal(pt3, pt1);
      end;
      pts[NCABLE_ELEMENTS - 1].x := trunc(pt1.x - ptn.x * aWidth);
      pts[NCABLE_ELEMENTS - 1].y := trunc(pt1.y - ptn.y * aWidth);

      n := NCABLE_ELEMENTS - 1;
      pt1.x := GetPoint(n-0).x - Left;
      pt1.y := GetPoint(n-0).y - Top;
      pt2.x := GetPoint(n-1).x - Left;
      pt2.y := GetPoint(n-1).y - Top;
      pt3.x := GetPoint(n-2).x - Left;
      pt3.y := GetPoint(n-2).y - Top;
      ptn := GetUnitNormal(pt1, pt3);

      while (n > 0) do begin
        pts[(NCABLE_ELEMENTS * 2 - 1) - n].x := trunc(pt1.x + ptn.x * aWidth);
        pts[(NCABLE_ELEMENTS * 2 - 1) - n].y := trunc(pt1.y + ptn.y * aWidth);

        dec(n);

        pt1 := pt2;
        pt2 := pt3;
        pt3.x := GetPoint(n-2).x - Left;
        pt3.y := GetPoint(n-2).y - Top;
        if n > 1 then
          ptn := GetUnitNormal(pt1, pt3);
      end;
      pts[NCABLE_ELEMENTS * 2 - 1].x := trunc(pt1.x + ptn.x * aWidth);
      pts[NCABLE_ELEMENTS * 2 - 1].y := trunc(pt1.y + ptn.y * aWidth);
    end;

begin
  //AddLogLine('Paint cable');

  inherited;

  Color := CableColors[ CableColor];

  // Calc cable segments
  for i := 1 to NCABLE_ELEMENTS - 1 do begin
    // Cable segment bounds including control margin
    // RelLeft, RelTop : distance to cable left, cable top
    FNode[i].RelLeft:= min( trunc(FNode[i].x), trunc(FNode[i-1].x)) - Left - CABLE_CONTROL_MARGIN;
    FNode[i].RelTop := min( trunc(FNode[i].y), trunc(FNode[i-1].y)) - Top - CABLE_CONTROL_MARGIN;
    FNode[i].Left   := FNode[i].RelLeft + Left;
    FNode[i].Top    := FNode[i].RelTop + Top;
    FNode[i].Width  := trunc(abs( FNode[i].x - FNode[i-1].x) + 1) + CABLE_CONTROL_MARGIN * 2;
    FNode[i].Height := trunc(abs( FNode[i].y - FNode[i-1].y) + 1) + CABLE_CONTROL_MARGIN * 2;
    FNode[i].Color := Color;
    FNode[i].ShadowColor := Darker(Color, 80);

    FNode[i].ClientRect.Left := 0;
    FNode[i].ClientRect.Top := 0;
    FNode[i].ClientRect.Right := FNode[i].Width - 1;
    FNode[i].ClientRect.Bottom := FNode[i].Height - 1;

    Fnode[i].BoundsRect.Left := FNode[i].Left;
    FNode[i].BoundsRect.Top := FNode[i].Top;
    Fnode[i].BoundsRect.Right := FNode[i].Left + FNode[i].Width - 1;
    FNode[i].BoundsRect.Bottom := FNode[i].Top + FNode[i].Height - 1;
  end;

  // Calc midline in segment coords
  for n := 0 to NCABLE_ELEMENTS - 1 do begin
    FNode[n].p1.X := FNode[n].p1.X - Left - FNode[n].RelLeft;
    FNode[n].p1.Y := FNode[n].p1.Y - Top - FNode[n].RelTop;
    FNode[n].p2.X := FNode[n].p2.X - Left - FNode[n].RelLeft;
    FNode[n].p2.Y := FNode[n].p2.Y - Top - FNode[n].RelTop;
  end;

  CalcOulinePoints(G_CableThickness);

  for n := 1 to NCABLE_ELEMENTS - 1 do begin
    FNode[n].p[0].x := pts[n-1].x - FNode[n].RelLeft;
    FNode[n].p[0].y := pts[n-1].y - FNode[n].RelTop;
    FNode[n].p[1].x := pts[n].x   - FNode[n].RelLeft;
    FNode[n].p[1].y := pts[n].y   - FNode[n].RelTop;

    FNode[n].p[2].x := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 2].x - FNode[n].RelLeft;
    FNode[n].p[2].y := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 2].y - FNode[n].RelTop;
    FNode[n].p[3].x := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 1].x - FNode[n].RelLeft;
    FNode[n].p[3].y := pts[ NCABLE_ELEMENTS * 2 - (n-1) - 1].y - FNode[n].RelTop;
  end;
end;

procedure TG2GraphCable.InitCable;
var n : integer;
    dx, dy : single;
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
  min_x := min(Fx1, Fx2);
  max_x := max(Fx1, Fx2);
  min_y := min(Fy1, Fy2);
  max_y := max(Fy1, Fy2);

  n := NCABLE_ELEMENTS - 1;

  dx := ( Fx2 - Fx1) / (n + 1);
  dy := ( Fy2 - Fy1) / (n + 1);

  halfx := ( Fx2 - Fx1) / 2;
  maxsag := Caterny(-halfx);

  FNode[n].x := Fx2;
  FNode[n].y := Fy2;

  dec(n);
  while (n >= 0) do begin

    FNode[n].x := Fx1 + dx * n;
    FNode[n].y := Fy1 + dy * n + Caterny(dx * n - halfx) - maxsag ;
    FNode[n].vx := 0;
    FNode[n].vy := 0;

    if FNode[n].x < min_x then
      min_x := trunc(FNode[n].x);

    if FNode[n].x > max_x then
      max_x := trunc(FNode[n].x);

    if FNode[n].y < min_y then
      min_y := trunc(FNode[n].y);

    if FNode[n].y > max_y then
      max_y := trunc(FNode[n].y);

    dec(n);
  end;
  FNode[0].x := Fx1;
  FNode[0].y := Fy1;

  Left := min_x - CABLE_CONTROL_MARGIN;
  Top := min_y - CABLE_CONTROL_MARGIN;
  Width := max_x - min_x + CABLE_CONTROL_MARGIN * 2;
  Height := max_y - min_y + CABLE_CONTROL_MARGIN * 2;
end;

procedure TG2GraphCable.Invalidate;
begin
  ConnectorMoved;
end;

function TG2GraphCable.CableResize( ax1, ay1, ax2, ay2 : integer): single;
var n, min_x, min_y, max_x, max_y : integer;
    max_vx, max_vy : single;
begin
  n := NCABLE_ELEMENTS - 1;

  Fx1 := ax1;
  Fy1 := ay1;
  Fx2 := ax2;
  Fy2 := ay2;

  min_x := min(Fx1, Fx2);
  max_x := max(Fx1, Fx2);
  min_y := min(Fy1, Fy2);
  max_y := max(Fy1, Fy2);

  max_vx := 0;
  max_vy := 0;

  FNode[n].x := Fx2;
  FNode[n].y := Fy2;

  FNode[n].p2.X := Fx2;
  FNode[n].p2.Y := Fy2;

  dec(n);

  while (n > 0) do begin

  	FNode[n].vx := FNode[n].vx + ( FNode[n + 1].x + FNode[n - 1].x - FNode[n].x * 2 ) / TENSION;
		FNode[n].vy := FNode[n].vy + ( FNode[n + 1].y + FNode[n - 1].y - FNode[n].y * 2 ) / TENSION;

    FNode[n].vy := FNode[n].vy + GRAVITY;

    //-- Reibung
    FNode[n].vx := FNode[n].vx * DAMP;
    FNode[n].vy := FNode[n].vy * DAMP;

    //-- Addieren der neuen Vektoren
    FNode[n].x := FNode[n].x + FNode[n].vx;
    FNode[n].y := FNode[n].y + FNode[n].vy;

    FNode[n+1].p1.X := trunc(FNode[n].x);
    FNode[n+1].p1.Y := trunc(FNode[n].y);
    FNode[n].p2.X := trunc(FNode[n].x);
    FNode[n].p2.Y := trunc(FNode[n].y);

    if FNode[n].x < min_x then
      min_x := trunc(FNode[n].x);

    if FNode[n].x > max_x then
      max_x := trunc(FNode[n].x);

    if FNode[n].y < min_y then
      min_y := trunc(FNode[n].y);

    if FNode[n].y > max_y then
      max_y := trunc(FNode[n].y);

    if FNode[n].vx > max_vx then
      max_vx := FNode[n].vx;

    if FNode[n].vy > max_vy then
      max_vy := FNode[n].vy;

    dec(n);
  end;
  FNode[0].x := Fx1;
  FNode[0].y := Fy1;

  FNode[1].p1.X := trunc(FNode[0].x);
  FNode[1].p1.Y := trunc(FNode[0].y);
  FNode[0].p2.X := trunc(FNode[0].x);
  FNode[0].p2.Y := trunc(FNode[0].y);
  FNode[0].p1.X := Fx1;
  FNode[0].p1.Y := Fy1;

  Left := min_x - CABLE_CONTROL_MARGIN;
  Top := min_y - CABLE_CONTROL_MARGIN;
  Width := max_x - min_x + CABLE_CONTROL_MARGIN * 2;
  Height := max_y - min_y + CABLE_CONTROL_MARGIN * 2;

  Result := sqrt( max_vx * max_vx + max_vy * max_vy);
end;

procedure TG2GraphCable.ConnectorMoved;
var i : integer;
    ModuleFrom, ModuleTo : TG2GraphModulePanel;
    ConnectorFrom, ConnectorTo : TG2GraphConnector;
begin
  // TODO : make this better
  if not(assigned(FromConnector) and (assigned(ToConnector)
      and (assigned(FromConnector.Module)) {and (assigned((FromConnector.Module as TG2GraphModule).Parent))})) then
    exit;

  ModuleFrom := (FromConnector.Module as TG2GraphModule).FPanel;
  ModuleTo := (ToConnector.Module as TG2GraphModule).FPanel;

  ConnectorFrom := FromConnector.GraphControl as TG2GraphConnector;
  ConnectorTo := ToConnector.GraphControl as TG2GraphConnector;

  Fx1 := ModuleFrom.ScrollPosX + ConnectorFrom.Left + ConnectorFrom.Width div 2;
  Fy1 := ModuleFrom.ScrollPosY + ConnectorFrom.Top + ConnectorFrom.Height div 2;
  Fx2 := ModuleTo.ScrollPosX + ConnectorTo.Left + ConnectorTo.Width div 2;
  Fy2 := ModuleTo.ScrollPosY + ConnectorTo.Top + ConnectorTo.Height div 2;
  InitCable;

  i := 50;
  while (i > 0) and (CableResize( ModuleFrom.ScrollPosX + ConnectorFrom.Left + ConnectorFrom.Width div 2,
                                  ModuleFrom.ScrollPosY + ConnectorFrom.Top + ConnectorFrom.Height div 2,
                                  ModuleTo.ScrollPosX + ConnectorTo.Left + ConnectorTo.Width div 2,
                                  ModuleTo.ScrollPosY + ConnectorTo.Top + ConnectorTo.Height div 2) > 0.01) do
    dec(i);
  PaintElements;

  if assigned((FromConnector.Module as TG2GraphModule).Parent) then
    (FromConnector.Module as TG2GraphModule).Parent.Invalidate;
end;

procedure Register;
begin
  RegisterComponents('NM G2', [TG2GraphScrollBox, TG2GraphDisplay, TG2GraphButtonRadio,
      TG2GraphButtonText, TG2GraphButtonFlat, TG2GraphButtonIncDec, TG2GraphKnob, TG2GraphLine,
      TG2GraphLabel, TG2GraphPanel]);
end;


Initialization
  begin
    FDropDownList := nil; // TODO : put in scrollbox
  end;
end.
