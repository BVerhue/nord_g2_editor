unit UnitG2Editor;

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
//  The libusb-win32 usb driver is needed:
//
//  Download libusb-win32 snapshot from
//    http://sourceforge.net/projects/libusb-win32/files/
//
//  Make a system restore point
//  Install as filter driver on the clavia usb driver
//  Do NOT install as device driver (because it then permanently replaces the clavia driver!)
//
//  This unit contains the demo G2 editor
//
//  ////////////////////////////////////////////////////////////////////////////

// v2.0
// ====
// Select, cut, copy past part of patches
// Undo
// Edit name of modules/parameters
// Choose module color
// Show/hide cables
// Sysex functions
// drag select
// replace socket components by Indy
// assign morphs/patch params to knobs
// short cut keys (keymap + osc map)
// Modules with textedit: initialize parameter label when adding module
// assign volume/patch settings to knob
// Bank/patch manager
// Uprate calculation bug
// Undo delete modules
// Clean up global assign on init/load patch
// ini-file (xml) with some preferences

// v2.1
// ====

// Static binding of libusb.dll (optional loading)
// Buttons on parameter pages
// Assign whole module to parameter page
// Patch parameter button labels
// Version control paramdef.xml, moduledef.xml
// doubling bug in patch manager
// Midi, send receieve sysex
// Copy patch->Undo->Cables are not deleted
// Set parameter functions to g2_file

// v2.2
// ==============

// Multiple g2 support
// Improve shut down of usb connections
// Customization of colors
// Some of the module display functions
// Added a "Control midi" port, but this hasn't yet any functionality.

// v2.3
// ==============

// Fixed a bug in the loading of partch banks from the G2
// Added "Save log file" button on the View log form
// Menu-driven patching (for use with Jaws)
// Select parameter messaging between G2 and editor
// Patch notes dialog window
// Changes to the moduledef.xml (parameter mappings to parameter pages knobs, still some modules todo)// Auto adjust module positions on module insert (no overlapping modules)
// Numbering modules when adding modules, check change module names
// Close dialogs with ESC
// Variation init function:
   // Copy from var 1 to init 00 0A 01 28 00 44 00 08 0F 5C
   // Init on var 2           00 0A 01 28 00 44 08 01 17 DC

// v2.4
// ===============

// Added selection buttons on switches
// Fixed error in filter modules, shows filtertype didn't correspond with actual filtertype
// Added unit conversions of many parameters
// Added uni/polar direction buttons on envelopes/lfo's
// Fixed editing parameter labels on switches and mixer knobs
// Fixed a number of bugs
// Added two modules "Resonator" and "Driver" that where hidden in the editor

// v2.5
// ===============

// Detection of modules incomaptible with original clavia edtor, will be saved with extensions prf2x en pch2x
// Bug placing modules when adding modules in scrolled scrollbox
// Delay parameter where not shown in units in param pages.
// Added some other missing unit conversions.
// Added help to application settings
// Added menu option to run g2ools from editor
// Added option to change led colors
// Added option to control editor ui elements with midi (ctrl midi input) ableton style


// Still todo:
// PatchSettings: add popup menu for patch parameters
// Add Slot settings dialog
// Make dialogs Jaws compatible
// Patch notes screen
   // h     : 00 0B 01 28 00 6F 00 01 68 D3 88
   // ha    : 00 0C 01 28 00 6F 00 02 68 61 56 C9
   // hal   : 00 0D 01 28 00 6F 00 03 68 61 6C 28 AD
   // enz.
// Finish module param mappings to parameter pages in xml def file
// CTRL E, parameter paste:
   // Osc B to Osc B : 00 19 01 28 00 4D 00 0F 40 40 40 45 80 D9 80 04 00 00 00 00 02 02 00 53 D3
   // Osc B incl morphs : 00 31 01 28 00 43 01 03 00 00 2B 00 00 43 01 03 06
   //                     00 1E 00 00 43 01 03 08 00 04 00 00 4D 00 0F 40 40
   //                     40 C5 80 55 4A 04 00 00 00 00 02 02 00 91 00
// Reclass controls for use with Jaws : TEdit : DEdit, TListView : DListView etc.

// TODO List next
// ==============

// Solve compiler warnings
// Add ini file for VST
// store/retrieve functions patch manager also with midi only connection
// Module editor in seperate app.
// VST clear knob assignmenst on patch change
// Delete Cable too...->Message not send?
// Cable colors (uprate)

// Morph value showing on sliders and buttons
// Make Set..InPatch functions private
// Auto assign midi
// some of the text functions
// some of the graphic screens
// osc/client server connections screen
// finish basic osc

// TODO List later
// ===============
// Virtual keyboard
// Module text functions
// Module graphic screens
// Memory used indicator
// Patch mutator
// Patch adjuster

// Mystery Modules
//
// ModuleType ShortName
// ========== =========
//         - AR-Env
//           AudioIn
//           Blue2Red  -> Testmodule
//           BusIn
//        77 ClkDivFix -> Doesn't seem to work
//        35 Driver    -> Works!
//           EnvDX
//           LfoD
//           Mixer6-1A
//           Mixer6-1B
//           OutBusA
//           OutBusB
//           PeakFollow
//       101 PolarFade -> Doesn't seem to work
//       120 PolarPan  -> Doesn't seem to work
//        10 PulseOsc  -> Doesn't seem to work
//           Red2Blue  -> Testmodule
//        56 Resonator -> Works!
//       207 RndChaos  -> Doesn't seem to work, but shows up with id 6,95,136,138
//           RndDistr
//           RndState
//           RndStep
//           SeqA
//       104 ShelvEQ   -> Doesn't seem to work
//        14 SyncOsc   -> Doesn't seem to work

// http://www.delphicorner.f9.co.uk/articles/op1.htm

{$I delphi_version.inc}

interface

uses
{$IFDEF FPC}
  FileUtil, LclIntf,
{$ELSE}
  Windows, XPStyleActnCtrls, ActnMan, ScktComp,
  {$IFDEF G2_VER200_up}
    // Don't know exactly in what version this style was introduced
    Vcl.PlatformDefaultStyleActnCtrls,
  {$ENDIF}
{$ENDIF}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,  ActnList, ImgList, Contnrs,
  g2_types, g2_database, g2_file, g2_mess, g2_usb, g2_graph, g2_midi, g2_classes,
  graph_util_vcl, LibUSBWinDyn, Menus, Buttons, DOM, XMLRead, XMLWrite,
  {MMSystem, MidiType, MidiIn, MidiOut,} ComCtrls;

type
  TEditorSettings = class
    FCableThickness : integer;
    FSlotStripColor : TColor;
    FSlotStripInverseColor : TColor;
    FSlotStripDisabledColor : TColor;
    FHighlightColor : TColor;
    FLedColor : TColor;
  end;

  TSlotPanel = class(TG2GraphPanel)
  private
    FDisableControls : boolean;
    FSlot            : TG2Slot;
    FlbSlotName      : TG2GraphLabel;
    FePatchName      : TEdit;
    FlbVariation     : TG2GraphLabel;
    FG2btInitVar     : TG2GraphButtonText;
    FG2btEditAllVars : TG2GraphButtonText;
    FG2rbVariation   : TG2GraphButtonRadio;
    FlbVolume        : TG2GraphLabel;
    FG2kVolume       : TG2GraphKnob;
    FG2btMute        : TG2GraphButtonText;
    FG2dVoices       : TG2GraphDisplay;
    FG2idVoiceMode   : TG2GraphButtonIncDec;
    FG2kMorphArray   : array[0..7] of TG2GraphKnob;
    FG2btMorphArray  : array[0..7] of TG2GraphButtonFlat;
    FpuVariationMenu : TPopupMenu;
    procedure ChangeAlleVariationsClick( Sender: TObject);
    procedure InitVariationClick( Sender: TObject);
  protected
    procedure VariationClick(Sender: TObject);
    procedure VariationMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure VoiceModeChange(Sender: TObject);
    procedure MorphMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
    procedure SetSlotCaption( aValue : string);
    function  GetSlotCaption: string;
    function  GetSlotIndex: integer;
    procedure SetSlot( aSlot : TG2Slot);
  public
    constructor Create( AOwner : TComponent; aSlot : TG2Slot);
    destructor Destroy; override;
    procedure  UpdateControls;
    procedure  SetSlotStripColors( aSlotStripColor, aSlotStripInverseColor, aHighLightColor : TColor);

    property VariationMenu : TPopupMenu read FpuVariationMenu write FPuVariationMenu;
    property SlotCaption : string read GetSlotCaption write SetSlotCaption;
    property SlotIndex : integer read GetSlotIndex;
    property Slot : TG2Slot read FSlot write SetSlot;
  end;

  TfrmG2Main = class(TForm)
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Uploadpatch1: TMenuItem;
    View1: TMenuItem;
    Patchsettings1: TMenuItem;
    cbMode: TCheckBox;
    Splitter1: TSplitter;
    puAddModule: TPopupMenu;
    cbOnline: TCheckBox;
    PerfPanel: TPanel;
    sbFX: TG2GraphScrollBox;
    sbVA: TG2GraphScrollBox;
    StartupTimer: TTimer;
    Initpatch1: TMenuItem;
    Parameterpages1: TMenuItem;
    puParamMenu: TPopupMenu;
    Defaultvalue1: TMenuItem;
    miMorphAssign: TMenuItem;
    miEditParamName: TMenuItem;
    N1: TMenuItem;
    miAssignKnob: TMenuItem;
    miAssignGlobalKnob: TMenuItem;
    MIDIController1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    puModuleMenu: TPopupMenu;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    puVariationMenu: TPopupMenu;
    Copyto11: TMenuItem;
    Copyto21: TMenuItem;
    Copyto31: TMenuItem;
    Copyto41: TMenuItem;
    Copyto51: TMenuItem;
    Copyto61: TMenuItem;
    Copyto71: TMenuItem;
    Copyto81: TMenuItem;
    N3: TMenuItem;
    Initvariation1: TMenuItem;
    N4: TMenuItem;
    Properties1: TMenuItem;
    miMidiCC: TMenuItem;
    miAssignMidiCC: TMenuItem;
    Deassign1: TMenuItem;
    Loadperformance1: TMenuItem;
    Saveperformance1: TMenuItem;
    Edit1: TMenuItem;
    Copy2: TMenuItem;
    miModuleRename: TMenuItem;
    Undo1: TMenuItem;
    Cut2: TMenuItem;
    Paste2: TMenuItem;
    Delete2: TMenuItem;
    SavePerformanceAsFxb1: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    Exit1: TMenuItem;
    puConnectorMenu: TPopupMenu;
    miDeleteAllCables: TMenuItem;
    miDeleteCable: TMenuItem;
    Editortools1: TMenuItem;
    Comm1: TMenuItem;
    SavePatchAsSysEx: TMenuItem;
    SavePerfAsSysEx: TMenuItem;
    N11: TMenuItem;
    ilModules: TImageList;
    ActionManager1: TActionManager;
    aDelete: TAction;
    aUndo: TAction;
    aRedo: TAction;
    aCut: TAction;
    aCopy: TAction;
    aPaste: TAction;
    aSelectAll: TAction;
    N12: TMenuItem;
    N13: TMenuItem;
    aPatchManager: TAction;
    Patchmanager1: TMenuItem;
    aParameterPages: TAction;
    aSynthSettings: TAction;
    aPerformanceSettings: TAction;
    aPatchSettings: TAction;
    aSettings: TAction;
    aEditTools: TAction;
    aInitPatch: TAction;
    aLoadPatch: TAction;
    aSavePatch: TAction;
    aSavePatchAsFXP: TAction;
    aSavePatchAsSysex: TAction;
    aLoadPerformance: TAction;
    aSavePerformance: TAction;
    aSavePerformanceAsSysEx: TAction;
    aSavePerformanceAsFXB: TAction;
    aLoadIniXML: TAction;
    aSaveIniXML: TAction;
    aExit: TAction;
    Loadini1: TMenuItem;
    Saveini1: TMenuItem;
    N14: TMenuItem;
    ResponseTimer: TTimer;
    Settings2: TMenuItem;
    Settings3: TMenuItem;
    Synthsettings2: TMenuItem;
    Performancesettings2: TMenuItem;
    aMidiDump: TAction;
    aSendControllerSnapshot: TAction;
    Mididump1: TMenuItem;
    Sendcontrolersnapshot1: TMenuItem;
    aViewLog: TAction;
    Viewlog1: TMenuItem;
    N7: TMenuItem;
    Def1: TMenuItem;
    aSendPartchSysex: TAction;
    aSendPerfSysex: TAction;
    Sendpatchsysex1: TMenuItem;
    Sendperformancesysex1: TMenuItem;
    N8: TMenuItem;
    miModuleAssignKnobs: TMenuItem;
    miModuleAssignGlobalKnobs: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    aGetPatchSysexFromBank: TAction;
    aGetPerfSysexFromBank: TAction;
    aGetActivePatchSysex: TAction;
    aGetActivePerfSysex: TAction;
    Getpatchsysexfrombank1: TMenuItem;
    Getperfsysexfrombank1: TMenuItem;
    Getactivepatchsysex1: TMenuItem;
    Getactiveperfsysex1: TMenuItem;
    N15: TMenuItem;
    aAnalyzePatch: TAction;
    rbSynth: TG2GraphButtonRadio;
    StatusBar1: TStatusBar;
    miSelect: TMenuItem;
    puSelectSlot: TPopupMenu;
    puSelectLocation: TPopupMenu;
    puSelectModule: TPopupMenu;
    puSelectParam: TPopupMenu;
    puSelectConnector: TPopupMenu;
    puSelectCable: TPopupMenu;
    aShowSelectSlot: TAction;
    aShowSelectLocation: TAction;
    aShowSelectModule: TAction;
    aShowSelectParam: TAction;
    aShowSelectCable: TAction;
    miSelectSlot: TMenuItem;
    miSelectlocation: TMenuItem;
    miSelectmodule: TMenuItem;
    miSelectparameter: TMenuItem;
    miSelectcable: TMenuItem;
    miAdd: TMenuItem;
    miAddModule: TMenuItem;
    aShowAddModule: TAction;
    aShowAddCable: TAction;
    miAddcable: TMenuItem;
    pPatchArea: TPanel;
    N16: TMenuItem;
    Selectall1: TMenuItem;
    aEditModuleProperties: TAction;
    aEditParamProperties: TAction;
    Moduleproperties1: TMenuItem;
    Parameterproperties1: TMenuItem;
    StaticText1: TStaticText;
    lbClientsConnected: TStaticText;
    Initvar1: TMenuItem;
    aPatchNotes: TAction;
    Patchnotes1: TMenuItem;
    N17: TMenuItem;
    aShowCopyVariation: TAction;
    Copyvariation1: TMenuItem;
    aSaveLogFile: TAction;
    aG2oolsDX2G2: TAction;
    aG2oolsNM1toG2: TAction;
    ools1: TMenuItem;
    G2oolsNM1toG21: TMenuItem;
    G2oolsDXtoG21: TMenuItem;
    N18: TMenuItem;
    miModuleHelp: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure cbModeClick(Sender: TObject);
    procedure aPatchSettingsExecute(Sender: TObject);
    procedure aViewLogExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aInitPatchExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StartupTimerTimer(Sender: TObject);
    procedure aParameterPagesExecute(Sender: TObject);
    procedure aDownloadPatchExecute(Sender: TObject);
    procedure sbFXMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sbVAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure VariaionCopytoClick(Sender: TObject);
    procedure Properties1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AssignMidiCC( Sender: TObject);
    procedure DeAssignMidiCC( Sender: TObject);
    procedure AssignMorph( Sender: TObject);
    procedure aSynthSettingsExecute(Sender: TObject);
    procedure miEditParamNameClick(Sender: TObject);
    procedure miModuleRenameClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure G2SelectSlot(Sender: TObject; SenderID: Integer; Slot: Integer);
    procedure Splitter1Moved(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aUndoExecute(Sender: TObject);
    procedure aRedoExecute(Sender: TObject);
    procedure aCutExecute(Sender: TObject);
    procedure aCopyExecute(Sender: TObject);
    procedure aPasteExecute(Sender: TObject);
    procedure aDeleteExecute(Sender: TObject);
    procedure aSelectAllExecute(Sender: TObject);
    procedure aPatchManagerExecute(Sender: TObject);
    procedure aPerformanceSettingsExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure aEditToolsExecute(Sender: TObject);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure aSavePatchExecute(Sender: TObject);
    procedure aSavePatchAsFXPExecute(Sender: TObject);
    procedure aSavePatchAsSysexExecute(Sender: TObject);
    procedure aLoadPerformanceExecute(Sender: TObject);
    procedure aSavePerformanceExecute(Sender: TObject);
    procedure aSavePerformanceAsSysExExecute(Sender: TObject);
    procedure aSavePerformanceAsFXBExecute(Sender: TObject);
    procedure aExitExecute(Sender: TObject);
    procedure cbOnlineClick(Sender: TObject);
    procedure ResponseTimerTimer(Sender: TObject);
    procedure aSendControllerSnapshotExecute(Sender: TObject);
    procedure aMidiDumpExecute(Sender: TObject);
    procedure Def1Click(Sender: TObject);
    procedure aSendPartchSysexExecute(Sender: TObject);
    procedure aSendPerfSysexExecute(Sender: TObject);
    procedure miModuleAssignKnobsClick(Sender: TObject);
    procedure aGetPatchSysexFromBankExecute(Sender: TObject);
    procedure aGetPerfSysexFromBankExecute(Sender: TObject);
    procedure aGetActivePatchSysexExecute(Sender: TObject);
    procedure aGetActivePerfSysexExecute(Sender: TObject);
    procedure aAnalyzePatchExecute(Sender: TObject);
    procedure rbSynthChange(Sender: TObject);
    procedure aShowSelectSlotExecute(Sender: TObject);
    procedure aShowSelectLocationExecute(Sender: TObject);
    procedure aShowSelectModuleExecute(Sender: TObject);
    procedure aShowSelectParamExecute(Sender: TObject);
    procedure miSelectClick(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure aShowAddModuleExecute(Sender: TObject);
    procedure aShowAddCableExecute(Sender: TObject);
    procedure aShowSelectCableExecute(Sender: TObject);
    procedure aEditModulePropertiesExecute(Sender: TObject);
    procedure aEditParamPropertiesExecute(Sender: TObject);
    procedure aPatchNotesExecute(Sender: TObject);
    procedure aShowCopyVariationExecute(Sender: TObject);
    procedure aSaveLogFileExecute(Sender: TObject);

    procedure G2VariationChange(Sender: TObject; SenderID: Integer; Slot, Variation: Integer);
    procedure G2USBActiveChange(Sender: TObject; Active: Boolean);
    procedure G2PatchUpdate(Sender: TObject; SenderID: Integer; PatchIndex: Integer);
    procedure G2PatchNameChange(Sender: TObject; SenderID: Integer; PatchIndex: Integer; PatchName : AnsiString);
    procedure G2CreateModule(Sender: TObject; SenderID: Integer; Module: TG2FileModule);
    procedure G2DeassignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    procedure G2AssignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    procedure G2AddClient(Sender: TObject; ClientIndex: Integer);
    procedure G2DeleteClient(Sender: TObject; ClientIndex: Integer);
    procedure G2MidiCCReceive(Sender: TObject; SenderID: Integer; MidiCC: Byte);
    procedure G2PerfSettingsUpdate(Sender: TObject; SenderID: Integer; PerfMode: Boolean);
    procedure G2SynthSettingsUpdate(Sender: TObject; SenderID: Integer);
    procedure G2AfterG2Init(Sender: TObject);
    procedure G2AfterRetrievePatch(Sender: TObject; SenderID: Integer; aSlot, aBank, aPatch: Byte);
    procedure G2BeforeSendMessage(Sender: TObject; SenderID: Integer; SendMessage: TG2SendMessage);
    procedure G2ReceiveResponseMessage(Sender: TObject; ResponseMessage: TMemoryStream);
    procedure G2DeassignGlobalKnob(Sender: TObject; SenderID, KnobIndex: Integer);
    procedure G2AssignGlobalKnob(Sender: TObject; SenderID, KnobIndex: Integer);
    procedure G2DeleteModule(Sender : TObject; SenderID : integer; Location: TLocationType; ModuleIndex : integer);
    procedure G2AddModule(Sender: TObject; SenderID : integer; Module : TG2FileModule);
    procedure G2SetModuleLabel(Sender: TObject; SenderID : integer; PatchIndex : byte; Location : TLocationType;  ModuleIndex : byte);
    procedure G2SetParamLabel(Sender: TObject; SenderID : integer; PatchIndex : byte; Location : TLocationType;  ModuleIndex : byte);
    procedure G2CopyVariation( Sender: TObject; SenderID : integer; SlotIndex, FromVariation, ToVariation : integer);
    procedure G2AddCable(Sender: TObject; SenderID : integer; Module : TG2FileCable);
    procedure G2DeleteCable(Sender : TObject; SenderID : integer; Location: TLocationType; FromModuleIndex, FromConnectorIndex, ToModuleIndex, ToConnectorIndex : integer);
    procedure G2SelectModule(Sender : TObject; SenderID : integer; Module : TG2FileModule);
    procedure G2SelectParam(Sender : TObject; SenderID : integer; Param : TG2FileParameter);
    procedure aG2oolsDX2G2Execute(Sender: TObject);
    procedure aG2oolsNM1toG2Execute(Sender: TObject);
    procedure miModuleHelpClick(Sender: TObject);

  private
    { Private declarations }
    //FCtrlMidiEnabled : boolean;
    //FCtrlMidiInput : TMidiInput;
    procedure DialogKey(var Msg: TWMKey); message CM_DIALOGKEY;
    //procedure SetCtrlMidiEnabled( aValue : boolean);
  public
    { Public declarations }
    FDisableControls      : boolean;
    FSlotPanel            : array[0..3] of TSlotPanel;
    FAddPoint             : TPoint;
    //FLocation             : TLocationType;
    FOldSplitterPos       : integer;
    FLastReceivedMidiCC   : byte;
    FCopyPatch            : TG2FilePatchPart;
    FG2List               : TObjectList;
    FG2Index              : integer;
    FEditorSettingsList   : TObjectList;
    FOnlyTextMenus        : boolean;

    procedure AddG2( G2USBDevice : pusb_device);
    function  SelectedG2: TG2;
    procedure SelectG2( G2Index : integer);

    procedure AddModule( aModuleType : byte);
    procedure AddTestModule( aModuleType, aAlternativeModuleType : byte);
    procedure DoAddModule( Sender: TObject);
    procedure DoAddTestModule( Sender: TObject);
    procedure AssignKnob( Sender: TObject);
    procedure DeAssignKnob( Sender: TObject);
    procedure AssignGlobalKnob( Sender: TObject);
    procedure DeAssignGlobalKnob( Sender: TObject);
    procedure ModuleAssignKnobs( Sender: TObject);
    procedure ModuleAssignGlobalKnobs( Sender: TObject);
    procedure GetPatchversion;
    procedure UpdateControls;
    procedure SetSelectedModuleColor( aColor : byte);
    //function  GetPatchWindowHeight: integer;

    procedure LoadImageMap( aBitmap : TBitmap;aCols, aRows: integer; aImageList : TImageList);
    procedure SetEditorSettings( aSlotStripColor, aSlotStripInverseColor, aSlotStripDisabledColor, aHighLightColor, aLedColor : TColor;
                                 aCableThickness : integer);
    procedure ShakeCables;
    procedure UpdateColorSchema;

    function IsShortCut(var Message: TWMKey): Boolean; override;
    procedure UpdateMainFormActions;
    procedure CreateAddModuleMenu;
    procedure CreateModuleMenu;
    procedure CreateParamMenu;

    function  AddMenuItem( aName, aCaption : string): TMenuItem;
    function  FindMenuItem( aParentMenuItem : TPopupMenu; aName : string): TMenuItem;
    function  FindOrAddMenuItem( aParentMenuItem : TPopupMenu; aName, aCaption : string): TMenuItem;
    procedure AddSelectModuleMenu( aMenuItem : TMenuItem; CableDestSelect : boolean);
    procedure UpdateSelectSlot;
    procedure UpdateSelectLocation;
    procedure UpdateSelectModule( aPopupMenu : TPopupMenu; CableDestSelect : boolean);
    procedure UpdateSelectParameter;
    procedure UpdateSelectConnector;
    procedure AddCableMenuItem( aMenuItem : TMenuItem; Connector : TG2FileConnector);
    procedure UpdateSelectCable;
    procedure UpdateSelectMenu;
    procedure UpdateAddMenu;
    procedure UpdateEditMenu;
    procedure UpdateParamMenu( aParam : TG2FileParameter);
    procedure UpdateVariationMenu;
    procedure CurrentlySelected;

    procedure DoSelectSlot( Sender: TObject);
    procedure DoSelectLocation( Sender: TObject);
    procedure DoSelectModule( Sender: TObject);
    procedure DoSelectParam( Sender: TObject);
    procedure DoDeleteCable( Sender: TObject);
    procedure DoDeleteAllCablesFromConnector(Sender: TObject);
    procedure DoAddCable( Sender: TObject);

    function  GetSelectedModule: TG2FileModule;
    function  GetSelectedParam : TG2FileParameter;
    function  GetSelectedConnector : TG2FileConnector;
    function  GetSelectedCable : TG2FileCable;
    function  GetSlotName( aSlot : TG2FileSlot): string;
    function  GetLocationName( aLocation : TLocationType) : string;
    function  GetModuleName( aModule : TG2FileModule) : string;
    function  GetParameterName( aParam : TG2FileParameter) : string;
    function  GetParameterValue( aParam : TG2FileParameter) : string;
    function  GetConnectorName( aConnector : TG2FileConnector) : string;
    function  GetCableName( aCable : TG2FileCable) : string;
    procedure SetOnlyTextMenus( aValue : boolean);

    procedure CopyPatchSelection;
    procedure DeletePatchSelection;
    procedure PastePatchSelection;
    procedure Undo;
    procedure SelectSlot( aSlotIndex : byte);
    procedure SelectVariation( aSlotIndex, aVariationIndex : byte);
    procedure SelectPatchLocation( aLocation : TLocationType);

    procedure SaveIniXML;
    procedure LoadIniXML;

    //procedure DoCtrlMidiInput(Sender: TObject);

    procedure PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ModuleClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module : TG2FileModule);
    procedure ParameterClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter : TG2FileParameter);
    procedure ConnectorClick( Sender : TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Connector : TG2FileConnector);
    procedure PanelClick(Sender: TObject);

    //property CtrlMidiEnabled : boolean read FCtrlMidiEnabled write SetCtrlMidiEnabled;
    //property CtrlMidiInput : TMidiInput read FCtrlMidiInput;
    property OnlyTextMenus : boolean read FOnlyTextMenus write SetOnlyTextMenus;
  end;

  procedure SetFormPosition( aForm : TForm; aLeft, aTop, aWidth, aHeight : integer);

var
  frmG2Main: TfrmG2Main;
  Initialized : boolean;

implementation

uses ShellApi, Vcl.HtmlHelpViewer,
  UnitLog, UnitPatchSettings, UnitParameterPages, UnitSeqGrid,
  UnitSynthSettings, UnitPerfSettings, UnitEditLabel, UnitSettings,
  UnitEditorTools, UnitPatchManager, UnitModuleDef, UnitPatchNotes,
  UnitMidiMapping, UnitTestModule;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure SetFormPosition( aForm : TForm; aLeft, aTop, aWidth, aHeight : integer);
begin
  // Check if form doesn;t fall of the screen
  Screen.Width;

  if aLeft < 0 then
    aForm.Left := 0
  else
    if aLeft > Screen.Width then
      aForm.Left := Screen.Width - 32
    else
      aForm.Left := aLeft;

  if aTop < 0 then
    aForm.Top := 0
  else
    if aTop > Screen.Height then
      aForm.Top := Screen.Height - 32
    else
      aForm.Top := aTop;

  if aWidth > 0 then
    aForm.Width := aWidth;

  if aHeight > 0 then
    aForm.Height := aHeight;
end;

// ==== TSlotPanel =============================================================

constructor TSlotPanel.Create(AOwner: TComponent; aSlot : TG2Slot);
var i : integer;
begin
  inherited Create( AOwner);

  FDisableControls := True;
  FSlot := aSlot;
  Font.Name := 'Arial';

  Height := 33;

  FlbSlotName := TG2GraphLabel.Create(self);
  FlbSlotName.Parent := self;
  FlbSlotName.Font.Size := 12;
  FlbSlotName.Font.Style := [fsbold];
  FlbSlotName.Font.Color := G_SlotStripInverseColor;
  FlbSlotName.Caption := Caption;
  FlbSlotName.SetBounds( 15, 8, 11, 19);
  FlbSlotName.OnClick := frmG2Main.PanelClick;

  FePatchName := TEdit.Create(self);
  FePatchName.ReadOnly := True;
  FePatchName.Parent := self;
  FePatchName.SetBounds( 36, 6, 101, 21);

  FlbVariation := TG2GraphLabel.Create(self);
  FlbVariation.Parent := self;
  FlbVariation.Font.Size := 8;
  FlbVariation.Font.Style := [fsBold];
  FlbVariation.Font.Color := G_SlotStripInverseColor;
  FlbVariation.Caption := 'Var';
  FlbVariation.SetBounds( 145, 10, 25, 13);
  FlbVariation.OnClick := frmG2Main.PanelClick;

  FG2rbVariation := TG2GraphButtonRadio.Create(self);
  FG2rbVariation.Parent := self;
  FG2rbVariation.SetBounds( 172, 7, 177, 19);
  FG2rbVariation.HightlightColor := G_HighlightColor;
  FG2rbVariation.Color := clActiveBorder;
  FG2rbVariation.BorderColor := clBtnShadow;
  FG2rbVariation.Bevel := False;
  FG2rbVariation.Orientation := otHorizontal;
  FG2rbVariation.Font.Style := [fsBold];
  for i := 1 to 8 do
    FG2rbVariation.ButtonText.Add( IntToStr(i));
  FG2rbVariation.ButtonCount := 8;
  FG2rbVariation.OnClick := VariationClick;
  FG2rbVariation.OnMouseUp := VariationMouseUp;

  FG2btInitVar := TG2GraphButtonText.Create(self);
  FG2btInitVar.Parent := Self;
  FG2btInitVar.SetBounds( 355, 8, 35, 18);
  FG2btInitVar.ParentColor := False;
  FG2btInitVar.BorderColor := clBlack;
  FG2btInitVar.Color := clBtnFace;
  FG2btInitVar.HightlightColor := G_HighlightColor;
  FG2btInitVar.ButtonTextType := bttPush;
  FG2btInitVar.ButtonText.Add('Init');
  FG2btInitVar.ButtonText.Add('Init');
  FG2btInitVar.OnClick := InitVariationClick;

  FG2btEditAllVars := TG2GraphButtonText.Create(self);
  FG2btEditAllVars.Parent := Self;
  FG2btEditAllVars.SetBounds( 396, 8, 35, 18);
  FG2btEditAllVars.ParentColor := False;
  FG2btEditAllVars.BorderColor := clBlack;
  FG2btEditAllVars.Color := clBtnFace;
  FG2btEditAllVars.HightlightColor := G_HighlightColor;
  FG2btEditAllVars.ButtonTextType := bttCheck;
  FG2btEditAllVars.ButtonText.Add('Edit all');
  FG2btEditAllVars.ButtonText.Add('Edit all');
  FG2btEditAllVars.OnClick := ChangeAlleVariationsClick;

  FlbVolume := TG2GraphLabel.Create(self);
  FlbVolume.Parent := self;
  FlbVolume.ParentColor := True;
  FlbVolume.Font.Size := 8;
  FlbVolume.Font.Style := [fsBold];

  FlbVolume.Font.Color := G_SlotStripInverseColor;
  FlbVolume.Caption := 'Vol';
  FlbVolume.SetBounds( 437, 10, 36, 13);
  FlbVolume.OnClick := frmG2Main.PanelClick;

  FG2kVolume := TG2GraphKnob.Create(self);
  FG2kVolume.Parent := Self;
  FG2kVolume.Orientation := otHorizontal;
  FG2kVolume.KnobType := ktSlider;
  FG2kVolume.SetBounds( 462, 10, 123, 15);
  FG2kVolume.OnMouseUp := frmG2Main.PatchCtrlMouseUp;

  FG2btMute := TG2GraphButtonText.Create(self);
  FG2btMute.Parent := Self;
  FG2btMute.SetBounds( 589, 8, 37, 18);
  FG2btMute.ParentColor := False;
  FG2btMute.BorderColor := clBlack;
  FG2btMute.Color := clBtnFace;
  FG2btMute.ButtonTextType := bttCheck;
  FG2btMute.HightlightColor := G_HighlightColor;
  FG2btMute.ButtonText.Add('Mute');
  FG2btMute.ButtonText.Add('On');
  FG2btMute.OnMouseUp := frmG2Main.PatchCtrlMouseUp;

  FG2dVoices := TG2GraphDisplay.Create(self);
  FG2dVoices.Parent := Self;
  FG2dVoices.SetBounds( 639, 8, 45, 17);
  FG2dVoices.Line[0] := '0';
  FG2dVoices.Color := CL_DISPLAY_BACKGRND;

  FG2idVoiceMode := TG2GraphButtonIncDec.Create(self);
  FG2idVoiceMode.Parent := Self;
  FG2idVoiceMode.SetBounds( 684, 8, 28, 17);
  FG2idVoiceMode.HighValue := 32;
  FG2idVoiceMode.LowValue := 0;
  FG2idVoiceMode.Orientation := otHorizontal;
  FG2idVoiceMode.Value := 1;
  FG2idVoiceMode.Bevel := True;
  FG2idVoiceMode.ParentColor := False;
  FG2idVoiceMode.BorderColor := clBlack;
  FG2idVoiceMode.Color := clBtnFace;
  FG2idVoiceMode.HightlightColor := G_HighlightColor;
  FG2idVoiceMode.OnChange := VoiceModeChange;

  for i := 0 to 7 do begin
    FG2kMorphArray[i] := TG2GraphKnob.Create(self);
    FG2kMorphArray[i].Parent := Self;
    FG2kMorphArray[i].KnobType := ktExtraSmall;
    FG2kMorphArray[i].SetBounds( 737 + i * 40 + 10, 2, 18, 18);
    FG2kMorphArray[i].Tag := i;
    if i = (FSlot.Patch as TG2Patch).SelectedMorphIndex then
      FG2kMorphArray[i].Color := CL_KNOB_MORPH_SELECTED
    else
      FG2kMorphArray[i].Color := CL_KNOB_MORPH;
    FG2kMorphArray[i].HighValue := 127;
    FG2kMorphArray[i].LowValue := 0;
    FG2kMorphArray[i].Value := 0;
    FG2kMorphArray[i].OnMouseUp := MorphMouseUp;

    FG2btMorphArray[i] := TG2GraphButtonFlat.Create(self);
    FG2btMorphArray[i].Parent := Self;
    FG2btMorphArray[i].SetBounds( 737 + i * 40, 20, 38, 11);
    FG2btMorphArray[i].ParentColor := False;
    FG2btMorphArray[i].Bevel := True;
    FG2btMorphArray[i].Color := clBtnFace;
    FG2btMorphArray[i].Tag := i + 8;
    FG2btMorphArray[i].ButtonText.Add('knob');
    FG2btMorphArray[i].ButtonText.Add(string(STD_MORPH_NAMES[i]));
    FG2btMorphArray[i].ButtonText.Add('');
  end;

  ParentColor := False;
{$IFDEF FPC}
{$ELSE}
  ParentBackground := False;
{$ENDIF}
  Color := G_SlotStripColor;

  FDisableControls := False;
end;

destructor TSlotPanel.Destroy;
begin
  inherited;
end;

function TSlotPanel.GetSlotCaption: string;
begin
  Result := FlbSlotName.Caption;
end;

procedure TSlotPanel.SetSlot(aSlot: TG2Slot);
begin
  FSlot := aSlot;
  UpdateControls;
end;

procedure TSlotPanel.SetSlotCaption( aValue: string);
begin
  FlbSlotName.Caption := aValue;
end;

function TSlotPanel.GetSlotIndex: integer;
begin
  Result := FSlot.SlotIndex;
end;

procedure TSlotPanel.UpdateControls;
var Variation : TVariation;
    i : integer;
begin
  FDisableControls := True;
  try
    FePatchName.Text     := string(FSlot.PatchName);
    FG2rbVariation.InitValue( FSlot.Patch.ActiveVariation);
    Variation            := FSlot.Patch.PatchSettings.Variations[ FSlot.Patch.ActiveVariation];
    if FSlot.Patch.EditAllVariations then
      FG2btEditAllVars.Value := 1
    else
      FG2btEditAllVars.Value := 0;

    FG2btMute.Parameter  := FSlot.Patch.Parameter[ PATCH_VOLUME, VOLUME_MUTE];
    FG2kVolume.Parameter := FSlot.Patch.Parameter[ PATCH_VOLUME, VOLUME_LEVEL];

    FG2idVoiceMode.InitValue( FSlot.Patch.PatchDescription.VoiceCount + 2 - FSlot.Patch.PatchDescription.MonoPoly - 1);
    FG2dVoices.Line[ 0] := FSlot.G2.TextFunction(0, FG2idVoiceMode.Value, 0, 0);
    for i := 0 to 7 do begin
      FG2kMorphArray[i].Parameter := FSlot.Patch.Parameter[ PATCH_MORPH, i];
      FG2btMorphArray[i].Parameter := FSlot.Patch.Parameter[ PATCH_MORPH, 8 + i];
    end;

    frmPatchSettings.UpdateControls;
    frmParameterPages.UpdateControls;
    frmPatchNotes.UpdateControls;

    Invalidate;
  finally
    FDisableControls := False;
  end;
end;

procedure TSlotPanel.SetSlotStripColors( aSlotStripColor, aSlotStripInverseColor, aHighLightColor : TColor);
begin
  FlbSlotName.Font.Color := G_SlotStripInverseColor;
  FlbVariation.Font.Color := G_SlotStripInverseColor;
  FG2rbVariation.HightlightColor := G_HighlightColor;
  FG2btInitVar.HightlightColor := G_HighlightColor;
  FG2btEditAllVars.HightlightColor := G_HighlightColor;
  FlbVolume.Font.Color := G_SlotStripInverseColor;
  FG2btMute.HightlightColor := G_HighlightColor;
  FG2idVoiceMode.HightlightColor := G_HighlightColor;
  Color := G_SlotStripColor;
  Invalidate;
end;

procedure TSlotPanel.VariationClick(Sender: TObject);
begin
  if FDisableControls then exit;

  frmG2Main.SelectVariation( FSlot.SlotIndex, FG2rbVariation.Value);
end;

procedure TSlotPanel.ChangeAlleVariationsClick(Sender: TObject);
begin
  if FDisableControls then exit;

  with (Sender as TG2GraphButtonText).Parent as TSlotPanel do begin
    FSlot.Patch.EditAllVariations := (Sender as TG2GraphButtonText).Value = 1;
  end;
end;

procedure TSlotPanel.InitVariationClick(Sender: TObject);
begin
  FSlot.SendCopyVariationMessage( 8, FSlot.Patch.ActiveVariation);
end;

procedure TSlotPanel.VariationMouseUp(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
var P : TPoint;
    i : integer;
begin
  GetCursorPos(P);
  if (Button = mbRight) then begin
    frmG2Main.UpdateVariationMenu;
    FpuVariationMenu.Popup( P.X, P.Y);
  end;
end;

procedure TfrmG2Main.VariaionCopytoClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  case (Sender as TMenuItem).Tag of
  0 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 0);
  1 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 1);
  2 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 2);
  3 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 3);
  4 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 4);
  5 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 5);
  6 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 6);
  7 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 7);
  8 : G2.SelectedSlot.SendCopyVariationMessage( G2.SelectedPatch.ActiveVariation, 8);
  9 : G2.SelectedSlot.SendCopyVariationMessage( 8, G2.SelectedPatch.ActiveVariation);
  end;
end;

procedure TSlotPanel.MorphMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); // TODO
begin
  if Sender is TG2GraphKnob then begin
    FG2kMorphArray[ (FSlot.Patch as TG2Patch).SelectedMorphIndex].Color := CL_KNOB_MORPH;
    (FSlot.Patch as TG2Patch).SelectedMorphIndex := (Sender as TG2GraphKnob).Tag;
    FG2kMorphArray[ (FSlot.Patch as TG2Patch).SelectedMorphIndex].Color := CL_KNOB_MORPH_SELECTED;

    frmG2Main.ParameterClick( Sender, Button, Shift, X, Y, FG2kMorphArray[(FSlot.Patch as TG2Patch).SelectedMorphIndex].Parameter as TG2GraphParameter);
  end;
end;

procedure TSlotPanel.VoiceModeChange(Sender: TObject);
var FPatchDescription : TPatchDescription;
begin
  FPatchDescription := FSlot.Patch.PatchDescription;
  case FG2idVoiceMode.Value of
  0 : begin // Legato
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 2;
      end;
  1 : begin // Mono
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 1;
      end;
  2..32 :
      begin
        FPatchDescription.VoiceCount := FG2idVoiceMode.Value - 1;
        FPatchDescription.MonoPoly := 0;
      end;
  end;
  (FSlot.Patch as TG2Patch).MessSetPatchDescription( FPatchDescription);
end;

// ==== Main app ===============================================================
// =============================================================================

procedure TfrmG2Main.FormCreate(Sender: TObject);
var ModuleMap : TBitmap;
    G2 : TG2;
    G2DeviceList : TList;
    i : integer;
begin
  FG2List := TObjectList.Create( True);
  FEditorSettingsList := TObjectList.Create( True);

  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);

    if G2DeviceList.Count > 0 then begin
      for i := 0 to G2DeviceList.Count - 1 do begin
        AddG2( pusb_device(G2DeviceList[i]));
      end;
    end else begin
      AddG2( nil);
      MessageBox(0, 'No g2 device found', '', MB_OK)
    end;
  finally
    G2DeviceList.Free;
  end;
  rbSynth.Width := FG2List.Count * 120 + 1;
  rbSynth.ButtonCount := FG2List.Count;
  rbSynth.ButtonText.Clear;
  for i := 0 to FG2List.Count - 1 do
    rbSynth.ButtonText.Add('G2 Synth ' + IntToStr(i+ 1));
  FG2Index := -1;
  SelectG2(0);

  Caption := 'NMG2 Editor ' + NMG2_VERSION;

  FDisableControls := False;
  FOldSplitterPos := Splitter1.Height;

  //FPatchManagerVisible := False;
  G2 := SelectedG2;

  ModuleMap := TBitmap.Create;
  try
    ModuleMap.LoadFromFile('Img\module_map.bmp');
    LoadImageMap( ModuleMap, 10, 22, ilModules);
  finally
    ModuleMap.Free;
  end;

  //FCtrlMidiInput := TMidiInput.Create(self);
  //FCtrlMidiInput.OnMidiInput := DoCtrlMidiInput;

  LoadIniXML;

  FSlotPanel[3] := TSlotPanel.Create( self, G2.SlotD);
  FSlotPanel[3].Align := alTop;
  FSlotPanel[3].VariationMenu := puVariationMenu;
  FSlotPanel[3].SlotCaption := 'D';
  FSlotPanel[3].OnClick := PanelClick;
  FSlotPanel[3].Parent := self;

  FSlotPanel[2] := TSlotPanel.Create( self, G2.SlotC);
  FSlotPanel[2].Align := alTop;
  FSlotPanel[2].VariationMenu := puVariationMenu;
  FSlotPanel[2].SlotCaption := 'C';
  FSlotPanel[2].OnClick := PanelClick;
  FSlotPanel[2].Parent := self;

  FSlotPanel[1] := TSlotPanel.Create( self, G2.SlotB);
  FSlotPanel[1].Align := alTop;
  FSlotPanel[1].VariationMenu := puVariationMenu;
  FSlotPanel[1].SlotCaption := 'B';
  FSlotPanel[1].OnClick := PanelClick;
  FSlotPanel[1].Parent := self;

  FSlotPanel[0] := TSlotPanel.Create( self, G2.SlotA);
  FSlotPanel[0].Align := alTop;
  FSlotPanel[0].VariationMenu := puVariationMenu;
  FSlotPanel[0].SlotCaption := 'A';
  FSlotPanel[0].OnClick := PanelClick;
  FSlotPanel[0].Parent := self;

  PerfPanel.Top := -1;
  PerfPanel.Align := alTop;
end;

procedure TfrmG2Main.AddG2( G2USBDevice : pusb_device);
var G2 : TG2;
    EditorSettings : TEditorSettings;
begin
  G2 := TG2.Create(self);
  G2.ClientType := ctEditor;
  G2.Host := '127.0.0.1';
  G2.Port := 2501 + FG2List.Count;
  G2.IsServer := True;
  G2.G2USBDevice := G2USBDevice;

  G2.OnAddClient := G2AddClient;
  G2.OnAfterG2Init := G2AfterG2Init;
  G2.OnAfterRetrievePatch := G2AfterRetrievePatch;
  G2.OnAssignGlobalKnob := G2AssignGlobalKnob;
  G2.OnAssignKnob := G2AssignKnob;
  G2.OnBeforeSendMessage := G2BeforeSendMessage;
  G2.OnCreateModule := G2CreateModule;
  G2.OnDeassignGlobalKnob := G2DeassignGlobalKnob;
  G2.OnDeassignKnob := G2DeassignKnob;
  G2.OnDeleteClient := G2DeleteClient;
  G2.OnMidiCCReceive := G2MidiCCReceive;
  G2.OnPatchUpdate := G2PatchUpdate;
  G2.OnPatchNameChange := G2PatchNameChange;
  G2.OnPerfSettingsUpdate := G2PerfSettingsUpdate;
  G2.OnReceiveResponseMessage := G2ReceiveResponseMessage;
  G2.OnSelectSlot := G2SelectSlot;
  G2.OnSynthSettingsUpdate := G2SynthSettingsUpdate;
  G2.OnUSBActiveChange := G2USBActiveChange;
  G2.OnVariationChange := G2VariationChange;
  G2.OnCopyVariation := G2CopyVariation;
  G2.OnAddModule := G2AddModule;
  G2.OnDeleteModule := G2DeleteModule;
  G2.OnAddCable := G2AddCable;
  G2.OnDeleteCable := G2DeleteCable;
  G2.OnSetModuleLabel := G2SetModuleLabel;
  G2.OnSetParamLabel := G2SetParamLabel;
  G2.OnSelectModule := G2SelectModule;
  G2.OnSelectParam := G2SelectParam;

  FG2List.Add(G2);

  EditorSettings := TEditorSettings.Create;
  EditorSettings.FCableThickness := G_CableThickness;
  EditorSettings.FSlotStripColor := G_SlotStripColor;
  EditorSettings.FSlotStripInverseColor := G_SlotStripInverseColor;
  EditorSettings.FSlotStripDisabledColor := G_SlotStripDisabledColor;
  EditorSettings.FHighlightColor := G_HighLightColor;
  EditorSettings.FLedColor := G_LedColor;

  FEditorSettingsList.Add( EditorSettings);
end;

function TfrmG2Main.SelectedG2: TG2;
begin
  if FG2Index <> -1 then
    SelectedG2 := FG2List[FG2Index] as TG2
  else
    SelectedG2 := nil;
end;

procedure TfrmG2Main.SelectG2(G2Index: integer);
var i : integer;
begin
  if (G2Index >= FG2List.Count) or (G2Index < 0) then
    raise Exception.Create( 'G2Index (' + IntToStr(G2Index) + ') out of bounds');

  if G2Index = FG2Index then
    exit;

  FG2Index := G2Index;
  (FG2List[FG2Index] as TG2).ScrollboxVA := sbVA;
  (FG2List[FG2Index] as TG2).ScrollboxFX := sbFX;

  with FEditorSettingsList[FG2Index] as TEditorSettings do begin
    G_SlotStripColor := FSlotStripColor;
    G_SlotStripInverseColor := FSlotStripInverseColor;
    G_SlotStripDisabledColor := FSlotStripDisabledColor;
    G_HighLightColor := FHighLightColor;
    G_LedColor := FLedColor;
    G_CableThickness := FCableThickness;
  end;

  for i := 0 to 3 do
    if assigned(FSlotPanel[i]) then
      FSlotPanel[i].Slot := (FG2List[FG2Index] as TG2).Slot[i];

  ShakeCables;
  UpdateColorSchema;
  SelectSlot( (FG2List[FG2Index] as TG2).SelectedSlotIndex);

  UpdateControls;
  sbVA.Invalidate;
  sbFX.Invalidate;

  if assigned(frmParameterPages) then
    frmParameterPages.UpdateControls;

  if assigned(frmPatchSettings) then
    frmPatchSettings.UpdateControls;

  if assigned(frmSettings) then
    frmSettings.UpdateControls;

  if assigned(frmSynthSettings) then
    frmSynthSettings.updateDialog;

  if assigned(frmPerfSettings) then
    frmPerfSettings.updateDialog;

  if assigned(frmEditorTools) then
    frmEditorTools.UpdateControls;

  if assigned(frmPatchManager) then begin
    frmPatchManager.TabControl1Change(self);
  end;

end;

procedure TfrmG2Main.FormShow(Sender: TObject);
begin
  {cbLogMessages.Checked := True;
  cbLogMessagesClick(self);}

  StartupTimer.Enabled := True;
end;

procedure TfrmG2Main.StartupTimerTimer(Sender: TObject);
var G2 : TG2;
    i : integer;
begin
  StartupTimer.Enabled := False;

  // Load module and parameter xml database
  for i := 0 to FG2List.Count - 1 do begin
    G2 := FG2List[i] as TG2;
    G2.LoadModuleDefs('');

    if G2.FModuleDefList.FileVersion <> NMG2_VERSION then
      ShowMessage( 'Warning, ModuleDef.xml version differs from application.');

    if G2.FParamDefList.FileVersion <> NMG2_VERSION then
      ShowMessage( 'Warning, ParamDef.xml version differs from application.');

    G2.USBActive := True;
  end;

  CreateAddModuleMenu;
  CreateModuleMenu;
  CreateParamMenu;

  Initialized := True;

  UpdateControls;
end;

procedure TfrmG2Main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var G2 : TG2;
    i : integer;
    ClientsConnected : boolean;
begin
  ClientsConnected := False;
  for i := 0 to FG2List.Count - 1 do begin
    G2 := FG2List[i] as TG2;
    if G2.IsServer and (G2.GetClientCount > 0) then
      ClientsConnected := True;
  end;

  if ClientsConnected then
   CanClose := MessageDlg('There are ' + IntToStr( G2.GetClientCount) + ' G2 clients connected to this server, do you really want to close the connection?',
           mtWarning, mbOKCancel, 0) = mrOk;
end;

procedure TfrmG2Main.FormClose(Sender: TObject; var Action: TCloseAction);
var G2 : TG2;
    i : integer;
begin
  SaveIniXML; // At this moment all the windows still are visible
  for i := 0 to FG2List.Count - 1 do begin
    G2 := FG2List[i] as TG2;
    G2.USBActive := False;
  end;
end;

procedure TfrmG2Main.FormDestroy(Sender: TObject);
begin
  //FCtrlMidiInput.Free;

  if assigned(FCopyPatch) then
    FCopyPatch.Free;

  FEditorSettingsList.Free;
  FG2List.Free;
end;

procedure TfrmG2Main.LoadImageMap(aBitmap: TBitmap; aCols, aRows: integer; aImageList: TImageList);
var SubBitmap : TBitmap;
    i, j : integer;
    Rect, DestRect : TRect;

begin
  SubBitmap := TBitmap.Create;
  SubBitmap.Width := aBitmap.Width div aCols + 1;
  SubBitmap.Height := aBitmap.Height div aRows + 1;
  SubBitmap.PixelFormat := aBitmap.PixelFormat;
  DestRect.Left := 0;
  DestRect.Top := 0;
  DestRect.Right := SubBitmap.Width;
  DestRect.Bottom := SubBitmap.Height;
  aImageList.Masked := False;
  aImageList.Width := SubBitmap.Width;
  aImageList.Height := SubBitmap.Height;
  try
    for i := 0 to aRows - 1 do
      for j := 0 to aCols - 1 do begin
        Rect.Left := j * SubBitmap.Width;
        Rect.Top := i * SubBitmap.Height;
        Rect.Right := Rect.Left + SubBitmap.Width;
        Rect.Bottom := Rect.Top + SubBitmap.Height;

        SubBitmap.Canvas.CopyRect( DestRect, aBitmap.Canvas, Rect);
        aImageList.Add( SubBitmap, nil);
      end;
  finally
    SubBitmap.Free;
  end;
end;

procedure TfrmG2Main.LoadIniXML;
var Doc : TXMLDocument;
    RootNode, SynthNode : TDOMNode;
    TCPSettingsNode : TXMLTCPSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
    EditorSettingsNode : TXMLEditorSettingsType;
    G2 : TG2;
    i : integer;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      for i := 0 to FG2List.Count - 1 do begin

        SynthNode := RootNode.FindNode('G2_Synth_' + IntToStr(i+1));
        if assigned(SynthNode) then begin

          G2 := FG2List[i] as TG2;
          if assigned(G2) then begin
            TCPSettingsNode := TXMLTCPSettingsType(SynthNode.FindNode('TCP_settings'));
            if assigned(TCPSettingsNode) then begin
              G2.IsServer := TCPSettingsNode.IsServer;
              G2.Host := TCPSettingsNode.IP;
              G2.Port := TCPSettingsNode.Port;
              G2.TimerBroadcastLedMessages := TCPSettingsNode.TimerBroadcastLedMessages;
            end;

            EditorSettingsNode := TXMLEditorSettingsType( SynthNode.FindNode('Editor_settings'));
            if assigned(EditorSettingsNode) then begin
              if EditorSettingsNode.LogEnabled then
                G2.LogLevel := 1
              else
                G2.LogLevel := 0;

              OnlyTextMenus := EditorSettingsNode.OnlyTextMenus;

              with FEditorSettingsList[i] as TEditorSettings do begin
                FCableThickness := EditorSettingsNode.CableThickness;
                FHighLightColor := EditorSettingsNode.HighlightColor;
                FLedColor := EditorSettingsNode.LedColor;
                FSlotStripColor := EditorSettingsNode.SlotStripColor;
                FSlotStripInverseColor := EditorSettingsNode.SlotStripInverseColor;
                FSlotStripDisabledColor := EditorSettingsNode.SlotStripDisabledColor;
                if i = 0 then begin
                  G_CableThickness := FCableThickness;
                  G_HighlightColor := FHighLightColor;
                  G_LedColor := FLedColor;
                  G_SlotStripColor := FSlotStripColor;
                  G_SlotStripInverseColor := FSlotStripInverseColor;
                  G_SlotStripDisabledColor := FSlotStripDisabledColor;
                  UpdateColorSchema;
                end;
              end;
            end;
          end;
        end;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('MainForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := True;
      end;
    end;

  finally
    Doc.Free;
  end;
end;

procedure TfrmG2Main.SaveIniXML;
var Doc : TXMLDocument;
    RootNode, SynthNode : TDOMNode;
    TCPSettingsNode : TXMLTCPSettingsType;
    MidiSettingsNode : TXMLMidiSettingsType;
    EditorSettingsNode : TXMLEditorSettingsType;
    CtrlMidiSettingsNode : TXMLCtrlMidiSettingsType;
    EditorCtrlMidiassignmentNode : TXMLEditorCtrlMidiassignmentType;
    PatchManagerSettingsNode : TXMLPatchManagerSettingsType;
    DirSettingsNode : TXMLDirectorySettingsType;
    FormSettingsNode : TXMLFormSettingsType;
    G2 : TG2;
    i : integer;
begin
  Doc := TXMLDocument.Create;
  try
    if FileExists('G2_editor_ini.xml') then
      ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if not assigned(RootNode) then begin
      RootNode := Doc.CreateElement('G2_Editor_settings');
      Doc.AppendChild(RootNode);
    end;

    for i := 0 to FG2List.Count - 1 do begin

      SynthNode := RootNode.FindNode('G2_Synth_' + IntToStr(i+1));
      if not assigned(SynthNode) then begin
        SynthNode := Doc.CreateElement('G2_Synth_' + IntToStr(i+1));
        RootNode.AppendChild(SynthNode);
      end;

      G2 := FG2List[i] as TG2;

      EditorSettingsNode := TXMLEditorSettingsType(SynthNode.FindNode('Editor_settings'));
      if not assigned(EditorSettingsNode) then begin
        EditorSettingsNode := TXMLEditorSettingsType( Doc.CreateElement('Editor_settings'));
        SynthNode.AppendChild( EditorSettingsNode);
      end;
      EditorSettingsNode.LogEnabled := G2.LogLevel = 1;
      EditorSettingsNode.OnlyTextMenus := OnlyTextMenus;
      with FEditorSettingsList[i] as TEditorSettings do begin
        EditorSettingsNode.CableThickness := FCableThickness;
        EditorSettingsNode.SlotStripColor := FSlotStripColor;
        EditorSettingsNode.SlotStripInverseColor := FSlotStripInverseColor;
        EditorSettingsNode.SlotStripDisabledColor := FSlotStripDisabledColor;
        EditorSettingsNode.HighlightColor := FHighLightColor;
        EditorSettingsNode.LedColor := FLedColor;
      end;

      TCPSettingsNode := TXMLTCPSettingsType( SynthNode.FindNode('TCP_settings'));
      if not assigned( TCPSettingsNode) then begin
        TCPSettingsNode := TXMLTCPSettingsType( Doc.CreateElement('TCP_settings'));
        SynthNode.AppendChild(TCPSettingsNode);
      end;
      TCPSettingsNode.IsServer := G2.IsServer;
      TCPSettingsNode.IP := G2.Host;
      TCPSettingsNode.Port := G2.Port;
      TCPSettingsNode.TimerBroadcastLedMessages := G2.TimerBroadcastLedMessages;

      MidiSettingsNode := TXMLMidiSettingsType( SynthNode.FindNode('MIDI_settings'));
      if not assigned( MidiSettingsNode) then begin
        MidiSettingsNode := TXMLMidiSettingsType( Doc.CreateElement('MIDI_settings'));
        SynthNode.AppendChild( MidiSettingsNode);
      end;
      MidiSettingsNode.MidiEnabled := G2.MidiEnabled;
      MidiSettingsNode.MidiInDevice := G2.MidiInput.ProductName;
      MidiSettingsNode.MidiOutDevice := G2.MidiOutput.ProductName;
    end;

    CtrlMidiSettingsNode := TXMLCtrlMidiSettingsType( RootNode.FindNode('CTRL_MIDI_settings'));
    if not assigned( CtrlMidiSettingsNode) then begin
      CtrlMidiSettingsNode := TXMLCtrlMidiSettingsType( Doc.CreateElement('CTRL_MIDI_settings'));
      RootNode.AppendChild( CtrlMidiSettingsNode);
    end;
    CtrlMidiSettingsNode.CtrlMidiEnabled := frmMidiMapping.CtrlMidiEnabled;
    CtrlMidiSettingsNode.CtrlMidiInDevice := frmMidiMapping.CtrlMidiInput.ProductName;
    CtrlMidiSettingsNode.CtrlMidiOutDevice := frmMidiMapping.CtrlMidiOutput.ProductName;

    for i := 0 to MAX_CTRL_MIDI_ASSIGNMENTS - 1 do begin
      EditorCtrlMidiassignmentNode := TXMLEditorCtrlMidiassignmentType( RootNode.FindNode('EDITOR_CTRL_MIDI_ASSIGNMENT_' + IntToStr(i)));
      if i < frmMidiMapping.MidiEditorAssignmentList.Count then begin
        if not assigned( EditorCtrlMidiassignmentNode) then begin
          EditorCtrlMidiassignmentNode :=TXMLEditorCtrlMidiassignmentType( Doc.CreateElement('EDITOR_CTRL_MIDI_ASSIGNMENT_' + IntToStr(i)));
          RootNode.AppendChild( EditorCtrlMidiassignmentNode);
        end;
        EditorCtrlMidiassignmentNode.ID := i;
        EditorCtrlMidiassignmentNode.Channel := frmMidiMapping.MidiEditorAssignmentList[i].Channel;
        EditorCtrlMidiassignmentNode.Note := frmMidiMapping.MidiEditorAssignmentList[i].Note;
        EditorCtrlMidiassignmentNode.ControlIndex := frmMidiMapping.MidiEditorAssignmentList[i].ControlIndex;
        EditorCtrlMidiassignmentNode.CC := frmMidiMapping.MidiEditorAssignmentList[i].CC;
        EditorCtrlMidiassignmentNode.MinValue := frmMidiMapping.MidiEditorAssignmentList[i].MinValue;
        EditorCtrlMidiassignmentNode.MaxValue := frmMidiMapping.MidiEditorAssignmentList[i].MaxValue;
        EditorCtrlMidiassignmentNode.ControlPath := frmMidiMapping.MidiEditorAssignmentList[i].ControlPath;
      end else begin
        // Deleted
        if assigned( EditorCtrlMidiassignmentNode) then begin
          EditorCtrlMidiassignmentNode.Free;
        end;
      end;
    end;

    FormSettingsNode := TXMLFormSettingsType( RootNode.FindNode('MainForm'));
    if not assigned( FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe( Doc.CreateElement('MainForm'));
      RootNode.AppendChild( FormSettingsNode);
    end;
    FormSettingsNode.PosX := Left;
    FormSettingsNode.PosY := Top;
    FormSettingsNode.SizeX := Width;
    FormSettingsNode.SizeY := Height;
    FormSettingsNode.Visible := True;

    DirSettingsNode := TXMLDirectorySettingsType(RootNode.FindNode('DirectorySettings'));
    if not assigned(DirSettingsNode) then begin
      DirSettingsNode := TXMLDirectorySettingsType(Doc.CreateElement('DirectorySettings'));
      RootNode.AppendChild(DirSettingsNode);
    end;
    DirSettingsNode.G2oolsFolder := AnsiString(frmSettings.eG2oolsFolder.Text);
    DirSettingsNode.ModuleHelpFile := AnsiString(frmSettings.eModuleHelpFile.Text);

    PatchManagerSettingsNode := TXMLPatchManagerSettingsType(RootNode.FindNode('PatchManagerSettings'));
    if not assigned(PatchManagerSettingsNode) then begin
      PatchManagerSettingsNode := TXMLPatchManagerSettingsType(Doc.CreateElement('PatchManagerSettings'));
      RootNode.AppendChild(PatchManagerSettingsNode);
    end;
    PatchManagerSettingsNode.BaseFolder := AnsiString(frmSettings.ePatchRootFolder.Text);
    PatchManagerSettingsNode.SelectedTab := frmPatchManager.TabControl1.TabIndex;
    PatchManagerSettingsNode.ExternalSortCol := frmPatchManager.FExternalSortCol;
    PatchManagerSettingsNode.InternalSortCol := frmPatchManager.FInternalSortCol;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchManagerForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('PatchManagerForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;

    FormSettingsNode.PosX := frmPatchManager.Left;
    FormSettingsNode.PosY := frmPatchManager.Top;
    FormSettingsNode.SizeX := frmPatchManager.Width;
    FormSettingsNode.SizeY := frmPatchManager.Height;
    FormSettingsNode.Visible := frmPatchManager.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('SettingsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('SettingsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmSettings.Left;
    FormSettingsNode.PosY := frmSettings.Top;
    FormSettingsNode.SizeX := frmSettings.Width;
    FormSettingsNode.SizeY := frmSettings.Height;
    FormSettingsNode.Visible := frmSettings.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('ParameterPagesForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('ParameterPagesForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmParameterPages.Left;
    FormSettingsNode.PosY := frmParameterPages.Top;
    FormSettingsNode.SizeX := frmParameterPages.Width;
    FormSettingsNode.SizeY := frmParameterPages.Height;
    FormSettingsNode.Visible := frmParameterPages.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchSettingsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('PatchSettingsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmPatchSettings.Left;
    FormSettingsNode.PosY := frmPatchSettings.Top;
    FormSettingsNode.SizeX := frmPatchSettings.Width;
    FormSettingsNode.SizeY := frmPatchSettings.Height;
    FormSettingsNode.Visible := frmPatchSettings.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('EditorToolsForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('EditorToolsForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmEditorTools.Left;
    FormSettingsNode.PosY := frmEditorTools.Top;
    FormSettingsNode.SizeX := frmEditorTools.Width;
    FormSettingsNode.SizeY := frmEditorTools.Height;
    FormSettingsNode.Visible := frmEditorTools.Visible;

    FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchNotesForm'));
    if not assigned(FormSettingsNode) then begin
      FormSettingsNode := TXMLFormSettingsTYpe(Doc.CreateElement('PatchNotesForm'));
      RootNode.AppendChild(FormSettingsNode);
    end;
    FormSettingsNode.PosX := frmPatchNotes.Left;
    FormSettingsNode.PosY := frmPatchNotes.Top;
    FormSettingsNode.SizeX := frmPatchNotes.Width;
    FormSettingsNode.SizeY := frmPatchNotes.Height;
    FormSettingsNode.Visible := frmPatchNotes.Visible;

    WriteXMLFile( Doc, 'G2_editor_ini.xml');
  finally
    Doc.Free;
  end;
end;

procedure TfrmG2Main.SetEditorSettings( aSlotStripColor,
                                        aSlotStripInverseColor,
                                        aSlotStripDisabledColor,
                                        aHighLightColor : TColor;
                                        aLedColor : TColor;
                                        aCableThickness : integer);
var i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if FG2Index <> -1 then
    with FEditorSettingsList[ FG2Index] as TEditorSettings do begin
      FSlotStripColor := aSlotStripColor;
      FSlotStripInverseColor := aSlotStripInverseColor;
      FSlotStripDisabledColor := aSlotStripDisabledColor;
      FHighLightColor := aHighLightColor;
      FLedColor := aLedColor;
      FCableThickness := aCableThickness;
    end;

  UpdateColorSchema;
end;

procedure TfrmG2Main.ShakeCables;
var i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for i := 0 to 3 do begin
    (G2.Slot[i].Patch as TG2Patch).ShakeCables;
  end;
end;

procedure TfrmG2Main.UpdateColorSchema;
var i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for i := 0 to 3 do begin
    (G2.Slot[i].Patch as TG2Patch).UpdateColorScheme;
    if assigned(FSlotPanel[i]) then
      if G2.SelectedSlotIndex = i then
        FSlotPanel[i].SetSlotStripColors( G_SlotStripInverseColor, G_SlotStripColor, G_HighLightColor)
      else
        FSlotPanel[i].SetSlotStripColors( G_SlotStripColor, G_SlotStripInverseColor, G_HighLightColor);
  end;
  rbSynth.HightlightColor := G_HighLightColor;

  if assigned(frmParameterPages) then
    frmParameterPages.UpdateColorScema;

  if assigned(frmPatchSettings) then
    frmPatchSettings.UpdateColorSchema;
end;

{procedure TfrmG2Main.SetCtrlMidiEnabled( aValue : boolean);
begin
  if aValue then begin
    if FCtrlMidiInput.State = misClosed then
      FCtrlMidiInput.OpenAndStart;
    FCtrlMidiEnabled := aValue;
  end else begin
    FCtrlMidiEnabled := aValue;
    if FCtrlMidiInput.State = misOpen then
      FCtrlMidiInput.StopAndClose;
  end;
end;

procedure TfrmG2Main.DoCtrlMidiInput(Sender: TObject);
var G2 : TG2;
  	thisEvent : TMyMidiEvent;
    midistring : string;

  function MidiToString(MidiEvent: TMyMidiEvent): string;
  var channel, parameter, i : integer;
  begin
    if MidiEvent.Sysex = nil then begin
      channel := CHANNEL_MASK and MidiEvent.MidiMessage;
      parameter := MidiEvent.MidiMessage - channel;
      Result := IntToHex(parameter, 2)
              + IntToHex(MidiEvent.Data1, 2)
              + IntToHex(MidiEvent.Data2, 2);
    end else begin
      channel := CHANNEL_MASK and ord(MidiEvent.Sysex[2]);
      parameter := ord(MidiEvent.Sysex[5]);
      Result := IntToHex(ord(MidiEvent.Sysex[0]), 2);
      for i := 1 to MidiEvent.SysexLength - 1 do
        Result := Result + IntToHex(ord(MidiEvent.Sysex[i]), 2);
    end;
  end;

begin
  while (FCtrlMidiInput.MessageCount > 0) do begin

    // Get the event as an object
    thisEvent := FCtrlMidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex = nil then begin

        midistring := MidiToString(thisEvent);

        case thisEvent.MidiMessage of
        143 : begin
                G2 := SelectedG2;
                if not assigned(G2) then
                  exit;
                G2.SendNoteMessage(thisEvent.Data1, $01);
              end;
        144 : begin
                G2 := SelectedG2;
                if not assigned(G2) then
                  exit;

                case thisEvent.Data2 of
                0 : G2.SendNoteMessage(thisEvent.Data1, $01);
                else
                  G2.SendNoteMessage(thisEvent.Data1, $00);
                end;
              end;
        end;


      end;
    finally
      thisEvent.Free;
    end;
  end;
end;}

procedure TfrmG2Main.UpdateMainFormActions;
var G2 : TG2;
    Module : TG2FileModule;
    SelectedModuleText,
    SelectedParamText : string;
begin
  if not Initialized then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if G2.USBActive then begin
    if G2.IsServer then begin
      // USB only
      aSynthSettings.Enabled := True;
      aMidiDump.Enabled := True;
      // Midi
      aSendPartchSysex.Enabled := True;
      aSendPerfSysex.Enabled := True;
      aGetPatchSysexFromBank.Enabled := True;
      aGetPerfSysexFromBank.Enabled := True;
      aGetActivePatchSysex.Enabled := True;
      aGetActivePerfSysex.Enabled := True;
    end else begin
      aSynthSettings.Enabled := True;
      aMidiDump.Enabled := True;
      // Midi
      aSendPartchSysex.Enabled := False;
      aSendPerfSysex.Enabled := False;
      aGetPatchSysexFromBank.Enabled := False;
      aGetPerfSysexFromBank.Enabled := False;
      aGetActivePatchSysex.Enabled := False;
      aGetActivePerfSysex.Enabled := False;
    end;
  end else
    if G2.IsServer then begin
      aSynthSettings.Enabled := False;
      aMidiDump.Enabled := False;
      // Midi
      aSendPartchSysex.Enabled := True;
      aSendPerfSysex.Enabled := True;
      aGetPatchSysexFromBank.Enabled := True;
      aGetPerfSysexFromBank.Enabled := True;
      aGetActivePatchSysex.Enabled := True;
      aGetActivePerfSysex.Enabled := True;
    end else begin
      aSynthSettings.Enabled := True;
      aMidiDump.Enabled := True;
      // Midi
      aSendPartchSysex.Enabled := False;
      aSendPerfSysex.Enabled := False;
      aGetPatchSysexFromBank.Enabled := False;
      aGetPerfSysexFromBank.Enabled := False;
      aGetActivePatchSysex.Enabled := False;
      aGetActivePerfSysex.Enabled := False;
    end;

  if assigned(G2.SelectedPatch) then begin
    if assigned(G2.SelectedPatchPart.SelectedParam) then begin
      SelectedParamText := G2.SelectedPatchPart.SelectedParam.ParamName;
      aEditParamProperties.Caption := 'Parameter proprties ' + SelectedParamText;
      aEditParamProperties.Enabled := True;
    end else begin
      aEditParamProperties.Caption := 'Parameter proprties';
      aEditParamProperties.Enabled := False;
    end;

    if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then begin
      if G2.SelectedPatchPart.SelectedModuleList.Count = 1 then begin
        Module := G2.SelectedPatchPart.SelectedModuleList[0];
        SelectedModuleText := 'module ' + Module.ModuleName;
        aCut.Caption := 'Cut ' + SelectedModuleText;
        aCut.Enabled := True;
        aCopy.Caption := 'Copy ' + SelectedModuleText;
        aCopy.Enabled := True;
        aDelete.Caption := 'Delete ' + SelectedModuleText;
        aDelete.Enabled := True;
        aEditModuleProperties.Caption := 'Module proprties ' + SelectedModuleText;
        aEditModuleProperties.Enabled := True;
        aShowSelectCable.Enabled := Module.CableCount > 0;
        aShowSelectParam.Enabled := Module.ParameterCount > 0;
      end else begin
        SelectedModuleText := IntToStr(G2.SelectedPatchPart.SelectedModuleList.Count) + ' modules';
        aCut.Caption := 'Cut ' + SelectedModuleText;
        aCut.Enabled := True;
        aCopy.Caption := 'Copy ' + SelectedModuleText;
        aCopy.Enabled := True;
        aDelete.Caption := 'Delete ' + SelectedModuleText;
        aDelete.Enabled := True;
        aEditModuleProperties.Caption := 'Module proprties';
        aEditModuleProperties.Enabled := False;
        aShowSelectCable.Enabled := False;
        aShowSelectParam.Enabled := False;
      end;
    end else begin
      aCut.Caption := 'Cut';
      aCut.Enabled := False;
      aCopy.Caption := 'Copy';
      aCopy.Enabled := False;
      aDelete.Caption := 'Delete';
      aDelete.Enabled := False;
      aEditModuleProperties.Caption := 'Module proprties';
      aEditModuleProperties.Enabled := False;
      aShowSelectCable.Enabled := False;
      aShowSelectParam.Enabled := False;
    end;

    if assigned(FCopyPatch) then begin
      aPaste.Enabled := True;
    end else begin
      aPaste.Enabled := False;
    end;

    if FileExists(CompletePath(frmSettings.eG2oolsFolder.Text) + 'nm2g2.exe') then
      aG2oolsNM1toG2.Enabled := True
    else
      aG2oolsNM1toG2.Enabled := False;

    if FileExists(CompletePath(frmSettings.eG2oolsFolder.Text) + 'dx2g2.exe') then
      aG2oolsDX2G2.Enabled := True
    else
      aG2oolsDX2G2.Enabled := False;


    if G2.SelectedPatch.UndoCount > 0 then
      aUndo.Enabled := True
    else
      aUndo.Enabled := False;
  end;
end;

procedure TfrmG2Main.UpdateControls;
var i : integer;
    G2 : TG2;
begin
  if not Initialized then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for i := 0 to 3 do begin
    if assigned(FSlotPanel[i]) then begin

      if G2.SelectedSlotIndex = i then begin
        FSlotPanel[i].Color := G_SlotStripInverseColor;
        FSlotPanel[i].FlbSlotName.Font.Color := G_SlotStripColor;
        FSlotPanel[i].FlbVariation.Font.Color := G_SlotStripColor;
        FSlotPanel[i].FlbVolume.Font.Color := G_SlotStripColor;
      end else begin
        if G2.Slot[i].Enabled = 1 then
          FSlotPanel[i].Color := G_SlotStripColor
        else
          FSlotPanel[i].Color := clGray;
        FSlotPanel[i].FlbSlotName.Font.Color := G_SlotStripInverseColor;
        FSlotPanel[i].FlbVariation.Font.Color := G_SlotStripInverseColor;
        FSlotPanel[i].FlbVolume.Font.Color := G_SlotStripInverseColor;
      end;

      FSlotPanel[i].UpdateControls;
    end;
  end;

  if assigned(frmEditorTools) and frmEditorTools.Visible then
    frmEditorTools.UpdateControls;

  if assigned(frmSeqGrid) and frmSeqGrid.Visible then
    frmSeqGrid.Update;

  UpdateMainFormActions;
end;

{function TfrmG2Main.GetPatchWindowHeight: integer;
var i : integer;
begin
  Result := ClientHeight - PerfPanel.Height;
  for i := 0 to 3 do
    Result := Result - FSlotPanel[i].Height;
end;}

procedure TfrmG2Main.DialogKey(var Msg: TWMKey);
begin
  case Msg.CharCode of
    VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT:
      if Assigned(OnKeyDown) then begin
        OnKeyDown(Self, Msg.CharCode, KeyDataToShiftState(Msg.KeyData));
        inherited;
      end
    else
      inherited
  end;
end;

function TfrmG2Main.IsShortCut(var Message: TWMKey): Boolean;
begin
  if Active then
    Result := inherited IsShortCut( Message)
  else
    Result := False;
end;

procedure TfrmG2Main.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  case Key of
    ord('1') : SelectVariation( G2.SelectedSlotIndex, 0);
    ord('2') : SelectVariation( G2.SelectedSlotIndex, 1);
    ord('3') : SelectVariation( G2.SelectedSlotIndex, 2);
    ord('4') : SelectVariation( G2.SelectedSlotIndex, 3);
    ord('5') : SelectVariation( G2.SelectedSlotIndex, 4);
    ord('6') : SelectVariation( G2.SelectedSlotIndex, 5);
    ord('7') : SelectVariation( G2.SelectedSlotIndex, 6);
    ord('8') : SelectVariation( G2.SelectedSlotIndex, 7);
    ord('F') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectPatchLocation( ltFX);
    ord('V') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectPatchLocation( ltVA);
    ord('A') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectSlot(0);
    ord('B') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectSlot(1);
    ord('C') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectSlot(2);
    ord('D') : if not(ssShift in Shift) and not(ssAlt in Shift) and not(ssCtrl in Shift) then SelectSlot(3);
    VK_LEFT  : begin
                 if ssShift in Shift then
                   G2.SelectedPatchPart.SelectModuleLeft;
                 if not(ssShift in Shift) then
                   //G2.SelectedPatchPart.SelectPrevModuleParam;
                   (G2.SelectedPatch as TG2GraphPatch).SelectPrevModuleControl;
               end;
    VK_RIGHT : begin
                 if ssShift in Shift then
                   G2.SelectedPatchPart.SelectModuleRight;
                 if not(ssShift in Shift) then
                   //G2.SelectedPatchPart.SelectNextModuleParam;
                   (G2.SelectedPatch as TG2GraphPatch).SelectNextModuleControl;
               end;
    VK_UP    : begin
                 if ssShift in Shift then
                   G2.SelectedPatchPart.SelectModuleAbove;
                 if not(ssShift in Shift) and not(ssCtrl in Shift) then
                   if assigned(G2.SelectedPatchPart.SelectedParam) then
                     G2.SelectedPatchPart.SelectedParam.IncValue;
                 if ssCtrl in Shift then
                   if assigned(G2.SelectedPatchPart.SelectedParam) then
                     G2.SelectedPatchPart.SelectedParam.IncMorphValue;
               end;
    VK_DOWN  : begin
                 if ssShift in Shift then
                   G2.SelectedPatchPart.SelectModuleUnder;
                 if not(ssShift in Shift) and not(ssCtrl in Shift) then
                   if assigned(G2.SelectedPatchPart.SelectedParam) then
                     G2.SelectedPatchPart.SelectedParam.DecValue;
                 if ssCtrl in Shift then
                   if assigned(G2.SelectedPatchPart.SelectedParam) then
                     G2.SelectedPatchPart.SelectedParam.DecMorphValue;
               end;
  end;
end;

procedure TfrmG2Main.cbModeClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if cbMode.Checked then
    G2.SendSetModeMessage(1)  // Performance mode
  else
    G2.SendSetModeMessage(0); // Patch mode

  G2.Performance.USBStartInit( True);

  UpdateControls;
end;

procedure TfrmG2Main.GetPatchversion;
var G2 : TG2;
begin
  // Get current patch version from G2
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedSlot.SendGetPatchVersionMessage;
end;

procedure TfrmG2Main.sbFXMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.SelectedLocation := ltFX;

  if assigned( FCopyPatch) and assigned(sbFX.CopyPatch) then begin
    sbFX.SetPositionsInCopyPatch;
    sbFX.CopyPatch := nil;
    sbVA.CopyPatch := nil;
    G2.SelectedPatch.MessCopyModules( FCopyPatch, ltVA, ltFX);
  end;

  if Button = mbRight then begin
    GetCursorPos( P);
    FAddPoint := sbFX.GetPatchCoord( sbFX.ScreenToClient(P));

    puAddModule.Popup(P.X, P.Y);
  end;
  UpdateMainFormActions;
end;

procedure TfrmG2Main.sbVAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.SelectedLocation := ltVA;

  if assigned( FCopyPatch) and assigned(sbFX.CopyPatch) then begin
    sbVA.SetPositionsInCopyPatch;
    sbFX.CopyPatch := nil;
    sbVA.CopyPatch := nil;
    G2.SelectedPatch.MessCopyModules( FCopyPatch, ltVA, ltVA);
  end;

  if Button = mbRight then begin
    GetCursorPos( P);
    FAddPoint := sbVA.GetPatchCoord( sbVA.ScreenToClient(P));

    puAddModule.Popup(P.X, P.Y);
  end;
  UpdateMainFormActions;
end;

procedure TfrmG2Main.SetSelectedModuleColor(aColor: byte);
var l, m : integer;
    Module : TG2FileModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for l := 0 to 1 do
    for m := 0 to G2.SelectedPatch.ModuleList[ l].Count - 1 do begin
      Module := G2.SelectedPatch.ModuleList[ l].Items[m];
     if Module.Selected then
       G2.SelectedPatch.MessSetModuleColor( TLocationType(l), Module.ModuleIndex, aColor);
    end;
end;

procedure TfrmG2Main.Splitter1Moved(Sender: TObject);
begin
  if FDisableControls then exit;

  FOldSplitterPos := Splitter1.Height;
end;

// ==== File menu ==============================================================

procedure TfrmG2Main.aDownloadPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedSlot.SendGetPatchMessage;
end;

procedure TfrmG2Main.aInitPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  // New patch
  G2.SelectedPatch.Init;
  G2.SelectedSlot.SendSetPatchMessage('No name', G2.SelectedPatch);
end;

procedure TfrmG2Main.aLoadPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  OpenDialog1.Filter := 'patch files (*.pch2)|*.pch2|extended patch files (*.pch2x)|*.pch2x|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if OpenDialog1.Execute then begin
    G2.LoadFileStream( OpenDialog1.FileName);
  end;
end;

procedure TfrmG2Main.aLoadPerformanceExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  OpenDialog1.Filter := 'performance files (*.prf2)|*.prf2|extended performance files (*.prf2x)|*.prf2x|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';

  if OpenDialog1.Execute then begin
    G2.LoadFileStream( OpenDialog1.FileName);
  end;
end;

procedure TfrmG2Main.aSaveLogFileExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SaveDialog1.Filter := 'txt files (*.txt)|*.txt';
  if SaveDialog1.Execute then begin
    G2.LogLines.SaveToFile( SaveDialog1.FileName);
  end;
end;

procedure TfrmG2Main.aSavePatchAsFXPExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SaveDialog1.Filter := 'fxp files (*.fxp)|*.fxp';
  if SaveDialog1.Execute then begin

{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveAsFXP(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePatchAsSysexExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SaveDialog1.Filter := 'sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if SaveDialog1.Execute then begin
    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveMidiToStream(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePatchExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if G2.SelectedPatch.GetNoOffExtendedModules > 0 then begin
    SaveDialog1.Filter := 'extended patch files (*.pch2x)|*.pch2x';
    SaveDialog1.DefaultExt := 'pch2x';
  end else begin
    SaveDialog1.Filter := 'patch files (*.pch2)|*.pch2';
    SaveDialog1.DefaultExt := 'pch2';
  end;
  SaveDialog1.FileName := G2.SelectedPatch.PatchName + '.' + SaveDialog1.DefaultExt;

  if SaveDialog1.Execute then begin
{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.SelectedPatch.SaveToFile(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;


procedure TfrmG2Main.aSavePerformanceAsFXBExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SaveDialog1.Filter := 'fxb files (*.fxb)|*.fxb';
  if SaveDialog1.Execute then begin

{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;
{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveAsFXB(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePerformanceAsSysExExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SaveDialog1.Filter := 'sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if SaveDialog1.Execute then begin
    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveMidiToStream(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aSavePerformanceExecute(Sender: TObject);
var WriteStream : TFileStream;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if G2.Performance.GetNoOffExtendedModules > 0 then begin
    SaveDialog1.Filter := 'extended performance files (*.prf2x)|*.prf2x';
    SaveDialog1.DefaultExt := 'prf2x';
  end else begin
    SaveDialog1.Filter := 'performance files (*.prf2)|*.prf2';
    SaveDialog1.DefaultExt := 'prf2';
  end;
  SaveDialog1.FileName := G2.Performance.PerformanceName + '.' + SaveDialog1.DefaultExt;

  if SaveDialog1.Execute then begin
{$IFDEF FPC}
    if FileExistsUTF8(SaveDialog1.FileName) { *Converted from FileExists*  } then begin
{$ELSE}
    if FileExists(SaveDialog1.FileName) then begin
{$ENDIF}

      if MessageDlg('Are you sure you want to replace the existing file? Because this is demo software there is a change the patch will be corrupted!', mtConfirmation, [mbYes, mbNo], 0) = mrNo then
        exit;

{$IFDEF FPC}
      DeleteFileUTF8(SaveDialog1.FileName); { *Converted from DeleteFile*  }
{$ELSE}
      DeleteFile(SaveDialog1.FileName);
{$ENDIF}
    end;

    WriteStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    try
      G2.Performance.SaveToFile(WriteStream);
    finally
      WriteStream.Free;
    end;
  end;
end;

procedure TfrmG2Main.aExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmG2Main.aG2oolsDX2G2Execute(Sender: TObject);
begin
  OpenDialog1.Filter := 'sysex files (*.syx)|*.syx';
  if OpenDialog1.Execute then begin
    ShellExecute( self.Handle, 'Open', 'Cmd', PChar('/K C:\Users\Bruno\Downloads\g2ools_15_190\g2ools-1.5\dx2g2.exe -d "' + OpenDialog1.FileName + '"'), PChar('C:\Users\Bruno\Downloads\g2ools_15_190\g2ools-1.5\'), SW_SHOWNORMAL);
  end;
end;

procedure TfrmG2Main.aG2oolsNM1toG2Execute(Sender: TObject);
begin
  OpenDialog1.Filter := 'patch files (*.pch)|*.pch';
  if OpenDialog1.Execute then begin
    ShellExecute( self.Handle, 'Open', 'Cmd', PChar('/K C:\Users\Bruno\Downloads\g2ools_15_190\g2ools-1.5\nm2g2.exe -d "' + OpenDialog1.FileName + '"'), PChar('C:\Users\Bruno\Downloads\g2ools_15_190\g2ools-1.5\'), SW_SHOWNORMAL);
  end;
end;

procedure TfrmG2Main.aGetActivePatchSysexExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SysExPatchRequestBySlot( G2.SelectedSlotIndex);
end;

procedure TfrmG2Main.aGetActivePerfSysexExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SysExPerformanceRequest;
end;

procedure TfrmG2Main.aGetPatchSysexFromBankExecute(Sender: TObject);
var Bank, Slot, c : integer;
    sBank, sSlot : string;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if InputQuery('Sysex req.', 'Bank', sBank) then begin
    val(sBank, Bank, C);
    if C = 0 then begin
      if InputQuery('Sysex req.', 'Slot', sSlot) then begin
        val(sSlot, Slot, C);
        if C = 0 then begin
          G2.SysExPatchRequestByFileIndex( Bank, Slot);
        end else
          raise Exception.Create('Invalid input');
      end;
    end else
     raise Exception.Create('Invalid input');
  end;
end;

procedure TfrmG2Main.aGetPerfSysexFromBankExecute(Sender: TObject);
var Bank, Slot, c : integer;
    sBank, sSlot : string;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if InputQuery('Sysex req.', 'Bank', sBank) then begin
    val(sBank, Bank, C);
    if C = 0 then begin
      if InputQuery('Sysex req.', 'Slot', sSlot) then begin
        val(sSlot, Slot, C);
        if C = 0 then begin
          G2.SysExPerformanceRequestByFileIndex( Bank, Slot);
        end else
          raise Exception.Create('Invalid input');
      end;
    end else
     raise Exception.Create('Invalid input');
  end;
end;

// ==== Edit menu ==============================================================

procedure TfrmG2Main.aAnalyzePatchExecute(Sender: TObject);
begin
//
end;

procedure TfrmG2Main.aCopyExecute(Sender: TObject);
begin
  CopyPatchSelection;
end;

procedure TfrmG2Main.aCutExecute(Sender: TObject);
begin
  CopyPatchSelection;
  DeletePatchSelection;
end;

procedure TfrmG2Main.aPasteExecute(Sender: TObject);
begin
  PastePatchSelection;
end;

procedure TfrmG2Main.aDeleteExecute(Sender: TObject);
begin
  DeletePatchSelection;
end;

procedure TfrmG2Main.aUndoExecute(Sender: TObject);
begin
  Undo;
end;

procedure TfrmG2Main.aRedoExecute(Sender: TObject);
begin
//
end;

procedure TfrmG2Main.aSelectAllExecute(Sender: TObject);
var i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for i := 0 to G2.SelectedPatchPart.ModuleList.Count - 1 do
    G2.SelectedPatchPart.ModuleList[i].Selected := True;

  UpdateMainFormActions;
end;

procedure TfrmG2Main.aEditModulePropertiesExecute(Sender: TObject);
var P : TPoint;
    G2 : TG2;
    Module : TG2FileModule;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := nil;
  if G2.SelectedPatchPart.SelectedModuleList.Count = 1 then
    Module := G2.SelectedPatchPart.SelectedModuleList[0];

  if assigned(Module) then begin
    GetCursorPos(P);
    puModuleMenu.Tag := integer(Module);
    puModuleMenu.Popup( P.X, P.Y);
  end;
end;

procedure TfrmG2Main.aEditParamPropertiesExecute(Sender: TObject);
var P : TPoint;
    G2 : TG2;
    Param : TG2FileParameter;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Param := G2.SelectedPatchPart.SelectedParam;

  if assigned(Param) then begin
    UpdateParamMenu( Param);
    GetCursorPos(P);

    miEditParamName.Enabled := Param.CanChangeLabel;

    puParamMenu.Popup( P.X, P.Y);
    puParamMenu.Tag := integer( Param);
  end;
end;

// ==== Selection menu =========================================================

procedure TfrmG2Main.SetOnlyTextMenus( aValue : boolean);
begin
  FOnlyTextMenus := aValue;
  if not FOnlyTextMenus then begin
    puAddModule.Images := ilModules;
    puSelectModule.Images := ilModules;
  end else begin
    puAddModule.Images := nil;
    puSelectModule.Images := nil;
  end;
end;

procedure TfrmG2Main.DoSelectLocation(Sender: TObject);
begin
  if Sender is TMenuItem then begin
    case (Sender as TMenuItem).Tag of
    0 : SelectPatchLocation(ltVA);
    1 : SelectPatchLocation(ltFX);
    end;
    UpdateMainFormActions;
  end;
end;

procedure TfrmG2Main.DOSelectModule(Sender: TObject);
var ModuleIndex : integer;
    Module : TG2FileModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Sender is TMenuItem then begin
     Module := TG2FileModule((Sender as TMenuItem).Tag);
     if assigned(Module) then begin
       G2.SelectedPatchPart.UnSelectModules;
       Module.Selected := True;
       UpdateMainFormActions;
     end;
  end;
end;

procedure TfrmG2Main.DoSelectParam(Sender: TObject);
var Param : TG2FileParameter;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Sender is TMenuItem then begin
    Param := TG2FileParameter( (Sender as TMenuItem).Tag);
    Param.Selected := True;
    G2.SelectedSlot.SendSelParamMessage( ord(Param.Location), Param.ModuleIndex, Param.ParamIndex);
    UpdateMainFormActions;
  end;
end;

procedure TfrmG2Main.DoDeleteCable( Sender: TObject);
var G2 : TG2;
   Cable : TG2FileCable;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Sender is TMenuItem then begin
    Cable := TG2FileCable((Sender as TMenuItem).tag);
    if assigned(Cable) then
      G2.SelectedPatch.MessDeleteConnection( G2.SelectedPatch.SelectedLocation, Cable);
  end;
end;

procedure TfrmG2Main.DoDeleteAllCablesFromConnector(Sender: TObject);
var Connector : TG2FileConnector;
   i : integer;
   G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Sender is TMenuItem then begin
    Connector := TG2FileConnector((Sender as TMenuItem).tag);
    for i := 0 to Connector.CableCount - 1 do // TODO : Could be made more efficient (all in one message)
      G2.SelectedPatch.MessDeleteConnection( G2.SelectedPatch.SelectedLocation, Connector.Cables[0]);
  end;
end;

procedure TfrmG2Main.DoAddCable(Sender: TObject);
var ConnectorToMenuItem, ModuleMenuItem : TMenuItem;
    FromConnector, ToCOnnector : TG2FileConnector;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if (Sender is TMenuItem) then begin
    ConnectorToMenuItem := Sender as TMenuItem;
    if assigned(ConnectorToMenuItem.Parent) then begin
      ModuleMenuItem := ConnectorToMenuItem.Parent;

      if assigned(ModuleMenuItem) and assigned(ModuleMenuItem.Parent) and assigned(ModuleMenuItem.Parent.Parent) then begin

        FromConnector := TG2FileConnector( ModuleMenuItem.Parent.Parent.Tag);
        ToConnector := TG2FileConnector( ConnectorToMenuItem.Tag);
        if assigned(FromConnector) and assigned(ToConnector) then begin
          G2.SelectedPatch.MessAddConnection( G2.SelectedPatch.SelectedLocation, FromConnector, ToCOnnector);
        end;

      end;
    end;
  end;
end;

procedure TfrmG2Main.DoSelectSlot(Sender: TObject);
begin
  if Sender is TMenuItem then begin
    case (Sender as TMenuItem).Tag of
    0 : SelectSlot(0);
    1 : SelectSlot(1);
    2 : SelectSlot(2);
    3 : SelectSlot(3);
    end;
  end;
end;

procedure TfrmG2Main.CurrentlySelected;
begin
//
end;

function TfrmG2Main.AddMenuItem( aName, aCaption : string): TMenuItem;
begin
  Result := TMenuItem.Create(self);
  Result.Name := aName;
  Result.Caption := aCaption;
end;

function TfrmG2Main.FindMenuItem( aParentMenuItem : TPopupMenu; aName : string): TMenuItem;
var i : integer;
begin
  i := 0;
  while (i<aParentMenuItem.Items.Count) and (aParentMenuItem.Items[i].Name <> aName) do
    inc(i);

  if (i<aParentMenuItem.Items.Count) then
    Result := aParentMenuItem.Items[i]
  else
    Result := nil
end;

function TfrmG2Main.FindOrAddMenuItem( aParentMenuItem : TPopupMenu; aName, aCaption : string): TMenuItem;
begin
  Result := FindMenuItem( aParentMenuItem, aName);
  if Result = nil then begin
    Result := AddMenuItem( aName, aCaption);
    aParentMenuItem.Items.Add(Result);
  end;
end;

function TfrmG2Main.GetSelectedModule: TG2FileModule;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Result := nil;
  if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then begin
    Result := G2.SelectedPatchPart.SelectedModuleList[0];
  end;
end;

function TfrmG2Main.GetSelectedParam : TG2FileParameter;
var G2 : TG2;
    Module : TG2FileModule;
    i : integer;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Result := nil;
  Module := GetSelectedModule;
  if assigned(Module) then begin
    i := 0;
    while (i < Module.ParameterCount) and not(Module.Parameter[i].Selected) do
      inc(i);

    if (i < Module.ParameterCount) then
      Result := Module.Parameter[i];
  end;
end;

function TfrmG2Main.GetSelectedConnector : TG2FileConnector;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;
end;

function TfrmG2Main.GetSelectedCable : TG2FileCable;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;
end;

function TfrmG2Main.GetSlotName( aSlot : TG2FileSlot): string;
var patchname : string;
begin
  if assigned( aSlot) then begin
    if trim(aSlot.PatchName) = '' then
      patchname := 'no patch loaded'
    else
      patchName := aSlot.PatchName;

    case aSlot.SlotIndex of
    0 : Result := 'Slot A, ' + patchname;
    1 : Result := 'Slot B, ' + patchname;
    2 : Result := 'Slot C, ' + patchname;
    3 : Result := 'Slot D, ' + patchname;
    end;
  end else
    Result := '';
end;

function TfrmG2Main.GetLocationName( aLocation : TLocationType) : string;
begin
  case aLocation of
    ltFX: Result := 'FX';
    ltVA: Result := 'VA';
    ltPatch: Result := 'Patch';
  end;
end;

function TfrmG2Main.GetModuleName( aModule : TG2FileModule) : string;
begin
  if assigned(aModule) then
    Result := aModule.ModuleName
  else
    Result := '';
end;

function TfrmG2Main.GetParameterName( aParam : TG2FileParameter) : string;
begin
  if assigned(aParam) then
    Result := aParam.ParamName
  else
    Result := '';
end;

function TfrmG2Main.GetParameterValue( aParam : TG2FileParameter) : string;
begin
  if assigned(aParam) then
    Result := IntToStr(aParam.GetParameterValue)
  else
    Result := '';
end;

function TfrmG2Main.GetConnectorName( aConnector : TG2FileConnector) : string;
begin
  if aConnector.ConnectorKind = ckInput then
    Result := 'Input ' + aConnector.Name
  else
    Result := 'Output ' + aConnector.Name;
end;

function TfrmG2Main.GetCableName( aCable : TG2FileCable) : string;
begin
  Result := '';
end;

procedure TfrmG2Main.UpdateSelectSlot;
var MenuItem : TMenuItem;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  MenuItem := FindOrAddMenuItem( puSelectSlot, 'miSlotA', GetSlotName(G2.SlotA));
  MenuItem.Tag := 0;
  MenuItem.OnClick := DoSelectSlot;
  MenuItem := FindOrAddMenuItem( puSelectSlot, 'miSlotB', GetSlotName(G2.SlotB));
  MenuItem.Tag := 1;
  MenuItem.OnClick := DoSelectSlot;
  MenuItem := FindOrAddMenuItem( puSelectSlot, 'miSlotC', GetSlotName(G2.SlotC));
  MenuItem.Tag := 2;
  MenuItem.OnClick := DoSelectSlot;
  MenuItem := FindOrAddMenuItem( puSelectSlot, 'miSlotD', GetSlotName(G2.SlotD));
  MenuItem.Tag := 3;
  MenuItem.OnClick := DoSelectSlot;
end;

procedure TfrmG2Main.UpdateVariationMenu;
var i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  for i := 0 to puVariationMenu.Items.Count - 1 do begin
    if puVariationMenu.Items[i].Tag = G2.SelectedPatch.ActiveVariation then
      puVariationMenu.Items[i].Enabled := False
    else
      puVariationMenu.Items[i].Enabled := True;
  end;
end;

procedure TfrmG2Main.UpdateSelectLocation;
var MenuItem : TMenuItem;
begin
  MenuItem := FindOrAddMenuItem( puSelectLocation, 'miLocationVA', '&VA');
  MenuItem.Tag := 0;
  MenuItem.OnClick := DoSelectLocation;
  MenuItem := FindOrAddMenuItem( puSelectLocation, 'miLocationFX', '&FX');
  MenuItem.Tag := 1;
  MenuItem.OnClick := DoSelectLocation;
end;

procedure TfrmG2Main.UpdateSelectMenu;
var G2 : TG2;
    Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  miSelectSlot.Caption := 'Select &slot, current ' + GetSlotName(G2.SelectedSlot);
  miSelectLocation.Caption := 'Select l&ocation, current ' + GetLocationName( G2.SelectedPatch.SelectedLocation);
  Module := GetSelectedModule;
  if assigned(Module) then begin
    miSelectModule.Caption := 'Select &module, current ' + GetModuleName(Module);
    Param := GetSelectedParam;
    if assigned(Param) then
      miSelectParameter.Caption := 'Select &parameter, current ' + GetParameterName(Param) + ', value ' + {GetParameterValue(Param)} Param.InfoFunction
    else
      miSelectParameter.Caption := 'Select &parameter';
    miSelectCable.Caption := 'Select cabl&e';
  end else begin
    miSelectModule.Caption := 'Select &module';
    miSelectParameter.Caption := 'Select &parameter';
    miSelectCable.Caption := 'Select cabl&e';
  end;
end;

procedure TfrmG2Main.UpdateAddMenu;
var G2 : TG2;
    Module : TG2FileModule;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  miAddModule.Caption := 'Add &module to slot ' + GetSlotName(G2.SelectedSlot) + ', location ' + GetLocationName( G2.SelectedPatch.SelectedLocation);

  Module := GetSelectedModule;
  if assigned(Module) then begin
    miAddCable.Caption := 'Add cabl&e to ' + GetModuleName(Module)
  end else begin
    miAddCable.Caption := 'Add cabl&e (no module selected)';
  end;
end;

procedure TfrmG2Main.UpdateSelectModule( aPopupMenu : TPopupMenu; CableDestSelect : boolean);
begin
  AddSelectModuleMenu( aPopupMenu.Items, CableDestSelect);
end;

procedure TfrmG2Main.AddSelectModuleMenu( aMenuItem : TMenuItem; CableDestSelect : boolean);
var G2 : TG2;
    i, j, c, p, m, mc : integer;
    FromModule, Module : TG2FileModule;
    Connector : TG2FileConnector;
    PageName : string;
    CategorieMenuItem, ModuleMenuItem, ConnectorMenuItem : TMenuItem;
    Allowed : boolean;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if CableDestSelect then begin
    FromModule := GetSelectedModule;
    if not assigned(FromModule) then
      exit;
  end;

  mc := 0;
  aMenuItem.Clear;
  for i := 0 to G2.SelectedPatchPart.ModuleList.Count - 1 do begin
    Module := G2.SelectedPatchPart.ModuleList[i];

    if CableDestSelect then begin
      // Only select modules with a free in-connector
      c := 0;
      while (c < Module.InConnectorCount) and (Module.InConnector[c].CableCount <> 0) do
        inc(c);

      Allowed := (c < Module.InConnectorCount);
    end else
      Allowed := True;

    if Allowed then begin

      // Find the page for the module type
      m := 0;
      while ( m < G2.FModuleDefList.Count) and (G2.FModuleDefList.ModuleDef[ m].ModuleType <> Module.TypeID) do
        inc(m);

      if ( m >= G2.FModuleDefList.Count) then
        raise Exception.Create('Module type ' + IntToStr(Module.TypeID) + ' not found.');

      PageName := G2.FModuleDefList.ModuleDef[ m].Page;

      p := 0;
      while (p<16) and (MODULECATEGORIES[p] <> PageName) do
        inc(p);

      if (p>=16) then
        raise Exception.Create( 'Module categorie ' + PageName + ' not found.');


      // Add module page name on the first level sorted bij page number
      j := 0;
      while (j < aMenuItem.Count) and (aMenuItem[j].Tag < p) do
        inc(j);

      if (j < aMenuItem.Count) then begin

        if aMenuItem[j].Tag <> p then begin
          CategorieMenuItem := TMenuItem.Create(self);
          CategorieMenuItem.Caption := PageName;

          // Jaws doesn't work on menuitems with pictures
          if not OnlyTextMenus then
            CategorieMenuItem.ImageIndex := Module.TypeID;
          CategorieMenuItem.Tag := p;

          aMenuItem.Insert( j, CategorieMenuItem);
        end else
          CategorieMenuItem := aMenuItem[j];

      end else begin
        CategorieMenuItem := TMenuItem.Create(self);
        CategorieMenuItem.Caption := PageName;
          // Jaws doesn't work on menuitems with pictures
        if not OnlyTextMenus then
          CategorieMenuItem.ImageIndex := Module.TypeID;
        CategorieMenuItem.Tag := p;

        aMenuItem.Add( CategorieMenuItem);
      end;

      // Now add the module type name on the second level, sorted by
      ModuleMenuItem := TMenuItem.Create(self);
      ModuleMenuItem.Caption := G2.FModuleDefList.ModuleDef[ m].ShortName + ' - ' + G2.SelectedPatchPart.GetModuleLabel( Module.ModuleIndex);
      // Jaws doesn't work on menuitems with pictures
      if not OnlyTextMenus then
        ModuleMenuItem.ImageIndex := Module.TypeID;
      ModuleMenuItem.Tag := integer(Module);
      if not CableDestSelect then
        ModuleMenuItem.OnClick := DoSelectModule;

      j := 0;
      while (j<CategorieMenuItem.Count) and (TG2FileModule(CategorieMenuItem.Items[j].Tag).PageIndex < Module.PageIndex) do
        inc(j);

      if j<CategorieMenuItem.Count then begin
        CategorieMenuItem.Insert( j, ModuleMenuItem);
      end else
        CategorieMenuItem.Add( ModuleMenuItem);

      if CableDestSelect then begin
        // Add allowed in-connectors
        for c := 0 to Module.InConnectorCount - 1 do begin
          Connector := Module.InConnector[c];
          if Connector.CableCount = 0 then begin
            ConnectorMenuItem := TMenuItem.Create(self);
            inc(mc);
            ConnectorMenuItem.Caption := GetConnectorName( Connector);
            ConnectorMenuItem.Tag := integer(Connector);
            ConnectorMenuItem.OnClick := DoAddCable;
            ModuleMenuItem.Add(ConnectorMenuItem);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmG2Main.UpdateEditMenu;
begin
//
end;

procedure TfrmG2Main.UpdateSelectParameter;
var G2 : TG2;
    i : integer;
    Module : TG2FileModule;
    Param : TG2FileParameter;
    MenuItem : TMenuItem;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  puSelectParam.Items.Clear;
  if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then begin
    Module := G2.SelectedPatchPart.SelectedModuleList[0];
    for i := 0 to Module.ParameterCount - 1 do begin
      Param := Module.Parameter[i];
      MenuItem := FindOrAddMenuItem( puSelectParam, 'mi' + ConvertToObjectName(Param.ParamName), Param.ParamName + ', value ' + Param.InfoFunction{ + ' (' + IntToStr(Param.GetParameterValue) + ')'});
      MenuItem.Tag := integer(Param);
      MenuItem.OnClick := DoSelectParam;
    end;
  end;
end;

procedure TfrmG2Main.UpdateSelectConnector;
var G2 : TG2;
    i : integer;
    Module : TG2FileModule;
    Connector : TG2FileConnector;
    MenuItem : TMenuItem;
begin
  // Add all output connectors of selected module
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  puSelectConnector.Items.Clear;
  if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then begin
    Module := G2.SelectedPatchPart.SelectedModuleList[0];
    for i := 0 to Module.OutConnectorCount - 1 do begin
      Connector := Module.OutConnector[i];
      MenuItem := FindOrAddMenuItem( puSelectConnector, 'miOut' + IntToStr(Integer(Connector)), GetConnectorName(Connector));
      MenuItem.Tag := integer(Connector);
      AddSelectModuleMenu( MenuItem, True);
    end;
    for i := 0 to Module.InConnectorCount - 1 do begin
      Connector := Module.InConnector[i];
      MenuItem := FindOrAddMenuItem( puSelectConnector, 'miIn' + IntToStr(Integer(Connector)), GetConnectorName(Connector));
      MenuItem.Tag := integer(Connector);
      AddSelectModuleMenu( MenuItem, True);
    end;
  end;
end;


procedure TfrmG2Main.AddCableMenuItem( aMenuItem : TMenuItem; Connector : TG2FileConnector);
var i : integer;
    G2 : TG2;
    CableMenuItem, CableOptionsMenu, ConnectorOptionsMenu : TMenuItem;
    Module, OtherModule : TG2FileModule;
    OtherConnector : TG2FileConnector;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := Connector.Module;
  for i := 0 to Connector.CableCount - 1 do begin

    OtherModule := nil;
    OtherConnector := nil;

    if Module.ModuleIndex = Connector.Cables[i].ModuleFrom then begin
      OtherModule := G2.SelectedPatch.Modules[ ord( G2.SelectedPatch.SelectedLocation), Connector.Cables[i].ModuleTo];
      if assigned(OtherModule) then begin
        if Connector.Cables[i].LinkType = 0 then // input to input
          OtherConnector := OtherModule.InConnector[ Connector.Cables[i].ToConnector.ConnectorIndex]
        else
          if Connector.ConnectorKind = ckOutput then
            OtherConnector := OtherModule.InConnector[ Connector.Cables[i].ToConnector.ConnectorIndex]
          else
            OtherConnector := OtherModule.OutConnector[ Connector.Cables[i].ToConnector.ConnectorIndex];
      end;
    end else begin
      OtherModule := G2.SelectedPatch.Modules[ ord( G2.SelectedPatch.SelectedLocation), Connector.Cables[i].ModuleFrom];
      if assigned(OtherModule) then begin
        if Connector.Cables[i].LinkType = 0 then // input to input
          OtherConnector := OtherModule.InConnector[ Connector.Cables[i].FromConnector.ConnectorIndex]
        else
          if Connector.ConnectorKind = ckOutput then
            OtherConnector := OtherModule.InConnector[ Connector.Cables[i].FromConnector.ConnectorIndex]
          else
            OtherConnector := OtherModule.OutConnector[ Connector.Cables[i].FromConnector.ConnectorIndex];
      end;
    end;

    if assigned(OtherConnector) then begin
      CableMenuItem := TMenuItem.Create(self);
      if OtherConnector.ConnectorKind = ckInput then
        CableMenuItem.Caption := 'Connected to ' + OtherModule.ModuleName + ', input ' + OtherConnector.Name
      else
        CableMenuItem.Caption := 'Connected to ' + OtherModule.ModuleName + ', output ' + OtherConnector.Name;
      CableMenuItem.Tag := integer(Connector.Cables[i]);
      aMenuItem.Add(CableMenuItem);

      CableOptionsMenu := TMenuItem.Create(self);
      CableOptionsMenu.Caption := 'Select module ' + OtherModule.ModuleName;
      CableOptionsMenu.Tag := integer(OtherModule);
      CableOptionsMenu.OnClick := DoSelectModule;
      CableMenuItem.Add( CableOptionsMenu);

      CableOptionsMenu := TMenuItem.Create(self);
      CableOptionsMenu.Caption := 'Delete cable';
      CableOptionsMenu.Tag := integer(Connector.Cables[i]);
      CableOptionsMenu.OnClick := DoDeleteCable;
      CableMenuItem.Add( CableOptionsMenu);
    end;

    ConnectorOptionsMenu := TMenuItem.Create( self);
    ConnectorOptionsMenu.Caption := '-';
    aMenuItem.Add(ConnectorOptionsMenu);

    ConnectorOptionsMenu := TMenuItem.Create( self);
    ConnectorOptionsMenu.Caption := 'Delete all cables from connector';
    ConnectorOptionsMenu.Tag := integer(Connector);
    ConnectorOptionsMenu.OnClick := DoDeleteAllCablesFromConnector;
    aMenuItem.Add(ConnectorOptionsMenu);
  end;
end;

procedure TfrmG2Main.UpdateSelectCable;
var G2 : TG2;
    i : integer;
    Module : TG2FileModule;
    Connector : TG2FileConnector;
    ConnectorMenuItem : TMenuItem;
begin
  // Add all output connectors of selected module
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  puSelectCable.Items.Clear;
  if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then begin
    Module := G2.SelectedPatchPart.SelectedModuleList[0];

    for i := 0 to Module.OutConnectorCount - 1 do begin
      Connector := Module.OutConnector[i];
      if Connector.CableCount > 0 then begin
        ConnectorMenuItem := FindOrAddMenuItem( puSelectCable, 'miOut' + ConvertToObjectName(Connector.Name), GetConnectorName( Connector));
        ConnectorMenuItem.Tag := Connector.ConnectorIndex;
        AddCableMenuItem( ConnectorMenuItem, Connector);
      end;
    end;

    for i := 0 to Module.InConnectorCount - 1 do begin
      Connector := Module.InConnector[i];
      if Connector.CableCount > 0 then begin
        ConnectorMenuItem := FindOrAddMenuItem( puSelectCable, 'miIn' + ConvertToObjectName(Connector.Name), GetConnectorName( Connector));
        ConnectorMenuItem.Tag := Connector.ConnectorIndex;
        AddCableMenuItem( ConnectorMenuItem, Connector);
      end;
    end;

  end;
end;

procedure TfrmG2Main.aShowAddCableExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectConnector;
  puSelectConnector.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowAddModuleExecute(Sender: TObject);
var P : TPoint;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  GetCursorPos( P);
  if G2.SelectedPatch.SelectedLocation = ltVA then begin
    FAddPoint := sbVA.GetPatchCoord( sbVA.ScreenToClient(P));
    puAddModule.Popup(P.X, P.Y);
  end else
    if G2.SelectedPatch.SelectedLocation = ltFX then begin
      FAddPoint := sbFX.GetPatchCoord( sbFX.ScreenToClient(P));
      puAddModule.Popup(P.X, P.Y);
    end;
end;

procedure TfrmG2Main.aShowCopyVariationExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateVariationMenu;
  puVariationMenu.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowSelectCableExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectCable;
  puSelectCable.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowSelectLocationExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectLocation;
  puSelectLocation.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowSelectModuleExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectModule( puSelectModule, False);
  puSelectModule.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowSelectParamExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectParameter;
  puSelectParam.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.aShowSelectSlotExecute(Sender: TObject);
var P : TPoint;
begin
  GetCursorPos( P);
  UpdateSelectSlot;
  puSelectSlot.Popup( P.X, P.Y);
end;

procedure TfrmG2Main.miSelectClick(Sender: TObject);
begin
  UpdateSelectMenu;
end;

procedure TfrmG2Main.miAddClick(Sender: TObject);
begin
  UpdateAddMenu;
end;

// ==== View menu ==============================================================

procedure TfrmG2Main.aSynthSettingsExecute(Sender: TObject);
begin
  frmSynthSettings.Show;
  //G2.USBSetSynthSettings;
end;

procedure TfrmG2Main.aViewLogExecute(Sender: TObject);
begin
  frmLog.Show;
end;

procedure TfrmG2Main.aParameterPagesExecute(Sender: TObject);
begin
  frmParameterPages.Show;
end;

procedure TfrmG2Main.aPatchManagerExecute(Sender: TObject);
begin
  frmPatchManager.Show;
end;

procedure TfrmG2Main.aPatchNotesExecute(Sender: TObject);
begin
  frmPatchNotes.Show;
end;

procedure TfrmG2Main.aPatchSettingsExecute(Sender: TObject);
begin
  frmPatchSettings.Show;
end;

procedure TfrmG2Main.aPerformanceSettingsExecute(Sender: TObject);
begin
  frmPerfSettings.Show;
end;

procedure TfrmG2Main.aSettingsExecute(Sender: TObject);
begin
  frmSettings.Show;
end;

procedure TfrmG2Main.aEditToolsExecute(Sender: TObject);
begin
  frmEditorTools.Show;
end;

// ==== Module menu ============================================================

procedure TfrmG2Main.CreateAddModuleMenu;
var i, j, k : integer;
    aMenuItem, aSubMenuItem : TMenuItem;
    dummy : integer;
    G2 : TG2;

    function GetPageIndex( aModuleType : integer): integer;
    var m : integer;
    begin
      m := 0;
      while (m < G2.FModuleDefList.Count) and (G2.FModuleDefList.ModuleDef[m].ModuleType <> aModuleType) do
        inc(m);

      if (m < G2.FModuleDefList.Count) then
        Result := G2.FModuleDefList.ModuleDef[m].PageIndex
      else
        Result := 0;
    end;

begin
  for i := 0 to 16 do begin

    aMenuItem := TMenuItem.Create( puAddModule);
    aMenuItem.Caption := MODULECATEGORIES[i];
    {case i of
     1 : aMenuItem.Caption := 'In/Out';
     2 : aMenuItem.Caption := 'Note';
     3 : aMenuItem.Caption := 'Osc';
     4 : aMenuItem.Caption := 'LFO';
     5 : aMenuItem.Caption := 'Rnd';
     6 : aMenuItem.Caption := 'Env';
     7 : aMenuItem.Caption := 'Filter';
     8 : aMenuItem.Caption := 'FX';
     9 : aMenuItem.Caption := 'Delay';
    10 : aMenuItem.Caption := 'Shaper';
    11 : aMenuItem.Caption := 'Level';
    12 : aMenuItem.Caption := 'Mixer';
    13 : aMenuItem.Caption := 'Switch';
    14 : aMenuItem.Caption := 'Logic';
    15 : aMenuItem.Caption := 'Seq';
    16 : aMenuItem.Caption := 'MIDI';
    end;}

    puAddModule.Items.Add( aMenuItem);

    G2 := SelectedG2;
    if not assigned(G2) then
      exit;

    for j := 0 to G2.FModuleDefList.Count - 1 do begin
      if G2.FModuleDefList.ModuleDef[j].ModuleType = 164 then
        dummy := 1;

      if string(G2.FModuleDefList.ModuleDef[j].Page) = aMenuItem.Caption then begin
        if aMenuItem.ImageIndex = -1 then
          aMenuItem.ImageIndex := G2.FModuleDefList.ModuleDef[j].ModuleType;

        aSubMenuItem := TMenuItem.Create( puAddModule);
        aSubMenuItem.Caption := string(G2.FModuleDefList.ModuleDef[j].ShortName);
        aSubMenuItem.Tag := G2.FModuleDefList.ModuleDef[j].ModuleType;
        aSubMenuItem.ImageIndex := G2.FModuleDefList.ModuleDef[j].ModuleType;

        if aMenuItem.Caption = 'Test' then
          aSubMenuItem.OnClick := DoAddTestModule
        else
          aSubMenuItem.OnClick := DoAddModule;

        k := 0;
        while (k<aMenuItem.Count) and (GetPageIndex(aMenuItem.Items[k].Tag) < G2.FModuleDefList.ModuleDef[j].PageIndex) do
          inc(k);

        if k<aMenuItem.Count then begin
          aMenuItem.Insert( k, aSubMenuItem);
        end else
          aMenuItem.Add( aSubMenuItem);
      end;
    end;
  end;
end;

procedure TfrmG2Main.CreateModuleMenu;
var i, j, Param : integer;
    aMenuItem, aSubMenuItem : TMenuItem;
begin
  // Assign knobs
  for i := 0 to 4 do begin

    aMenuItem := TMenuItem.Create( puModuleMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + i);
    miModuleAssignKnobs.Add(aMenuItem);
    for j := 0 to 2 do begin
      aSubMenuItem := TMenuItem.Create( puModuleMenu);
      aSubMenuItem.Caption := 'Column ' + IntToStr(j+1);
      aSubMenuItem.Tag := i * 3 + j;
      aSubMenuItem.OnClick := ModuleAssignKnobs;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;

  // Assign global knobs
  for i := 0 to 4 do begin
    aMenuItem := TMenuItem.Create( puModuleMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + i);
    miModuleAssignGlobalKnobs.Add(aMenuItem);
    for j := 0 to 2 do begin
      aSubMenuItem := TMenuItem.Create( puModuleMenu);
      aSubMenuItem.Caption := 'Column ' + IntToStr(j+1);
      aSubMenuItem.Tag := i * 3 + j;
      aSubMenuItem.OnClick := ModuleAssignGlobalKnobs;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;
end;

procedure TfrmG2Main.ModuleAssignGlobalKnobs(Sender: TObject);
var Module : TG2GraphModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := TG2GraphModule(puModuleMenu.Tag);

  if Module.AssignableKnobCount > 0 then
    G2.SelectedPatch.MessModuleAssignGlobalKnobs( Module, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.ModuleAssignKnobs(Sender: TObject);
var Module : TG2GraphModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := TG2GraphModule(puModuleMenu.Tag);

  if Module.AssignableKnobCount > 0 then
    G2.SelectedPatch.MessModuleAssignKnobs( Module, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.ModuleClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Module: TG2FileModule);
var P : TPoint;
begin
  UpdateMainFormActions;

  if FileExists(frmSettings.eModuleHelpFile.Text) then
    miModuleHelp.Enabled := True
  else
    miModuleHelp.Enabled := False;

  if Button = mbRight then begin
    GetCursorPos(P);
    puModuleMenu.Popup( P.X, P.Y);
    puModuleMenu.Tag := integer(Module);
  end;
end;

procedure TfrmG2Main.miModuleHelpClick(Sender: TObject);
var Module : TG2GraphModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := TG2GraphModule(puModuleMenu.Tag);
  if FileExists(frmSettings.eModuleHelpFile.Text) then begin
    Application.HelpFile := frmSettings.eModuleHelpFile.Text;
    Application.HelpJump( 'nmg2_ref_-_' + Module.ModuleFileName);
  end;
end;

procedure TfrmG2Main.miModuleRenameClick(Sender: TObject);
var Module : TG2GraphModule;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Module := TG2GraphModule(puModuleMenu.Tag);
  frmEditLabel.Left := Module.ClientToScreen(Point(0, 0)).X;
  frmEditLabel.Top := Module.ClientToScreen(Point(0, 0)).Y;
  if frmEditLabel.ShowModal = mrOk then begin
    G2.SelectedPatch.MessSetModuleLabel( Module.Location,
                                         Module.ModuleIndex,
                                         AnsiString(frmEditLabel.eLabel.Text));
  end;
end;

procedure TfrmG2Main.Properties1Click(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);
  if Module.TypeID = 121 then begin
    frmSeqGrid.SetModule( Module);
    frmSeqGrid.Show;
  end;
end;

procedure TfrmG2Main.Def1Click(Sender: TObject);
var Module : TG2GraphModule;
begin
  Module := TG2GraphModule(puModuleMenu.Tag);
  frmModuleDef.FModuleType := Module.TypeID;
  frmModuleDef.Show;
end;

procedure TfrmG2Main.AddModule( aModuleType : byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if FAddPoint.X < 0 then
    FAddPoint.X := 0;

  if FAddPoint.Y < 0 then
    FAddPoint.Y := 0;

  G2.SelectedPatch.MessAddModule( G2.SelectedPatch.SelectedLocation, aModuleType, aModuleType, FAddPoint.X div UNITS_COL, FAddPoint.y div UNITS_ROW );
end;

procedure TfrmG2Main.AddTestModule( aModuleType, aAlternativeModuleType : byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if FAddPoint.X < 0 then
    FAddPoint.X := 0;

  if FAddPoint.Y < 0 then
    FAddPoint.Y := 0;

  G2.SelectedPatch.MessAddModule( G2.SelectedPatch.SelectedLocation, aModuleType, aAlternativeModuleType, FAddPoint.X div UNITS_COL, FAddPoint.y div UNITS_ROW );
end;

procedure TfrmG2Main.DoAddModule( Sender: TObject);
begin
  with Sender as TMenuItem do
    AddModule( Tag);
end;

procedure TfrmG2Main.DoAddTestModule( Sender: TObject);
var ModuleTypeID, AlternativeModuleTypeID : byte;
begin
  with Sender as TMenuItem do
    ModuleTypeID := Tag;

  frmTestModule.eModuleTypeID.Text := IntToStr(ModuleTypeID);
  if frmTestModule.ShowModal = mrOk then begin
    AlternativeModuleTypeID := StrToInt(frmTestModule.eModuleTypeID.Text);
    AddTestModule( ModuleTypeID, AlternativeModuleTypeID);
  end;
end;

procedure TfrmG2Main.miModuleAssignKnobsClick(Sender: TObject);
begin
end;

// ==== Parameter menu =========================================================

procedure TfrmG2Main.CreateParamMenu;
var i, j, Page, PageColumn, Param : integer;
    aMenuItem, aSubMenuItem : TMenuItem;
begin
  // Assign knobs
  for i := 0 to 14 do begin
    Page := ( i div 3);
    PageColumn := ( i mod 3);

    aMenuItem := TMenuItem.Create( puParamMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + Page) + IntToStr(PageColumn);
    miAssignKnob.Add(aMenuItem);
    for j := 0 to 7 do begin
      aSubMenuItem := TMenuItem.Create( puParamMenu);
      aSubMenuItem.Caption := 'Knob ' + IntToStr(j);
      aSubMenuItem.Tag := i * 8 + j;
      aSubMenuItem.OnClick := AssignKnob;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;
  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := '-';
  miAssignKnob.Add(aMenuItem);

  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := 'Deassign';
  aMenuItem.OnClick := DeassignKnob;
  miAssignKnob.Add(aMenuItem);

  // Assign global knobs
  for i := 0 to 14 do begin
    Page := ( i div 3);
    PageColumn := ( i mod 3);

    aMenuItem := TMenuItem.Create( puParamMenu);
    aMenuItem.Caption := 'Page ' + chr(65 + Page) + IntToStr(PageColumn);
    miAssignGlobalKnob.Add(aMenuItem);
    for j := 0 to 7 do begin
      aSubMenuItem := TMenuItem.Create( puParamMenu);
      aSubMenuItem.Caption := 'Knob ' + IntToStr(j);
      aSubMenuItem.Tag := i * 8 + j;
      aSubMenuItem.OnClick := AssignGlobalKnob;
      aMenuItem.Add(aSubMenuItem);
    end;
  end;
  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := '-';
  miAssignGlobalKnob.Add(aMenuItem);

  aMenuItem := TMenuItem.Create( puParamMenu);
  aMenuItem.Caption := 'Deassign';
  aMenuItem.OnClick := DeassignGlobalKnob;
  miAssignGlobalKnob.Add(aMenuItem);
end;

procedure TfrmG2Main.miEditParamNameClick(Sender: TObject);
var Parameter : TG2FileParameter;
    Rect : TRect;
    G2 : TG2;
    ParamLabelIndex : integer;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  ParamLabelIndex := G2.SelectedPatch.SelectedControl.GetParamLabelIndex;
  if ParamLabelIndex <> -1 then begin
    Rect := G2.SelectedPatch.SelectedControl.GetScreenCoordsRect;


    frmEditLabel.Left := Rect.Left;
    frmEditLabel.Top := Rect.Top;
    if frmEditLabel.ShowModal = mrOk then begin
      Parameter := G2.SelectedPatch.SelectedControl.Parameter;
      G2.SelectedPatch.MessSetModuleParamLabels( Parameter.Location,
                                                 Parameter.ModuleIndex,
                                                 Parameter.ParamIndex,
                                                 ParamLabelIndex,
                                                 AnsiString(frmEditLabel.eLabel.Text));
    end;
  end;
end;

procedure TfrmG2Main.AssignKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.SelectedPatch.MessAssignKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.DeAssignKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    KnobIndex : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  KnobIndex := G2.SelectedPatch.FindKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);

  if KnobIndex <> -1 then
    G2.SelectedPatch.MessDeassignKnob( KnobIndex);
end;

procedure TfrmG2Main.AssignGlobalKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.Patch[ Parameter.Patch.Slot.SlotIndex].MessAssignGlobalKnob( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, (Sender as TMenuItem).Tag);
end;

procedure TfrmG2Main.DeAssignGlobalKnob(Sender: TObject);
var Parameter : TG2GraphParameter;
    KnobIndex : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  KnobIndex := G2.Performance.GlobalKnobList.FindGlobalKnobIndex( Parameter.Patch.Slot.SlotIndex, Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);

  if KnobIndex <> -1 then
    G2.SelectedPatch.MessDeassignGlobalKnob( KnobIndex);
end;

procedure TfrmG2Main.AssignMidiCC(Sender: TObject);
var Parameter : TG2GraphParameter;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  G2.SelectedPatch.MessAssignMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, FLastReceivedMidiCC);
end;

procedure TfrmG2Main.DeAssignMidiCC(Sender: TObject);
var Parameter : TG2GraphParameter;
    MidiCC : byte;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Parameter := TG2GraphParameter( puParamMenu.Tag);
  MidiCC := G2.SelectedPatch.FindMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex);
  if MidiCC <> 0 then
    G2.SelectedPatch.MessDeassignMidiCC( MidiCC);
end;

procedure TfrmG2Main.AssignMorph(Sender: TObject);
var Parameter : TG2GraphParameter;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Sender is TMenuItem then begin
    if (Sender as TMenuItem).Checked then begin
      // Deassign Morph
      Parameter := TG2GraphParameter( puParamMenu.Tag);
      G2.SelectedPatch.SetMorphValue(  Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, 0, G2.SelectedPatch.ActiveVariation);
      Parameter.InvalidateControl;
    end else begin
      Parameter := TG2GraphParameter( puParamMenu.Tag);
      G2.SelectedPatch.SetMorphValue(  Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, 128 - Parameter.GetParameterValue , G2.SelectedPatch.ActiveVariation);
      Parameter.InvalidateControl;
    end;
  end;
end;

procedure TfrmG2Main.UpdateParamMenu( aParam : TG2FileParameter);
var Knob : TKnob;
    G2 : TG2;

  procedure SetKnobCheck( index : integer);
  var i, j : integer;
  begin
    for i := 0 to 14 do begin
      for j := 0 to 7 do begin
        if miAssignKnob.Items[i].Items[j].Tag = index then
          miAssignKnob.Items[i].Items[j].Checked := True
        else
          miAssignKnob.Items[i].Items[j].Checked := False;

        Knob := G2.SelectedPatch.GetKnob( i * 8 + j);
        if (Knob <> nil) and (Knob.IsAssigned = 1) then
          miAssignKnob.Items[i].Items[j].Enabled := False
        else
          miAssignKnob.Items[i].Items[j].Enabled := True;
      end;
    end;
  end;

  procedure SetGlobalKnobCheck( index : integer);
  var i, j : integer;
  begin
    for i := 0 to 14 do begin
      for j := 0 to 7 do begin
        if miAssignGlobalKnob.Items[i].Items[j].Tag = index then
          miAssignGlobalKnob.Items[i].Items[j].Checked := True
        else
          miAssignGlobalKnob.Items[i].Items[j].Checked := False;

        Knob := G2.Performance.GetGlobalKnob( i * 8 + j);
        if (Knob <> nil) and (Knob.IsAssigned = 1) then
          miAssignGlobalKnob.Items[i].Items[j].Enabled := False
        else
          miAssignGlobalKnob.Items[i].Items[j].Enabled := True;
      end;
    end;
  end;

  procedure SetMidiCCCheck( MidiCC : byte);
  begin
    miMidiCC.Caption :=  'Midi CC ' + IntToStr(FLastReceivedMidiCC);
    if MidiCC = 0 then begin
      miAssignMidiCC.Checked := False;
      miAssignMidiCC.Caption := 'Assign...';
    end else begin
      miAssignMidiCC.Checked := True;
      miAssignMidiCC.Caption := 'Assign';
    end;
  end;

  procedure SetMorphCheck( MorphAssigned : boolean);
  begin
    miMorphAssign.Checked := MorphAssigned;
  end;

begin
  if not assigned(aParam) then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  SetKnobCheck( G2.SelectedPatch.FindKnob( aParam.Location, aParam.ModuleIndex, aParam.ParamIndex));
  SetGlobalKnobCheck( G2.Performance.GlobalKnobList.FindGlobalKnobIndex( G2.SelectedSlotIndex, aParam.Location, aParam.ModuleIndex, aParam.ParamIndex));
  SetMidiCCCheck( G2.SelectedPatch.FindMidiCC( aParam.Location, aParam.ModuleIndex, aParam.ParamIndex));
  SetMorphCheck( G2.SelectedPatch.GetMorph( aParam.Location, aParam.ModuleIndex, aParam.ParamIndex, G2.SelectedPatch.SelectedMorphIndex, G2.SelectedPatch.ActiveVariation) <> nil);
end;

procedure TfrmG2Main.ParameterClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Parameter: TG2FileParameter);
var P : TPoint;
    G2 : TG2;
begin
  if not assigned(Parameter) then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedSlot.SendSelParamMessage( ord(Parameter.Location), Parameter.ModuleIndex, Parameter.ParamIndex);

  if Button = mbRight then begin
    UpdateParamMenu( Parameter);
    GetCursorPos(P);

    //miEditParamName.Enabled := sender is TG2GraphButtonText;
    miEditParamName.Enabled := Parameter.CanChangeLabel;

    puParamMenu.Popup( P.X, P.Y);
    puParamMenu.Tag := integer( Parameter);
  end else
    if frmSeqGrid.Visible and ((frmSeqGrid.FModuleIndex = Parameter.ModuleIndex) and
                               (frmSeqGrid.FLocation = Parameter.Location)) then
      frmSeqGrid.Update;
end;


// ==== Connector menu =========================================================

procedure TfrmG2Main.ConnectorClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,  Y: Integer; Connector: TG2FileConnector);
var P : TPoint;
    aMenuItem : TMenuItem;
    i : integer;
    G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if Button = mbRight then begin
    GetCursorPos(P);

    {while miDeleteCable.Count > 0 do begin
      miDeleteCable.Items[0].Free;
    end;

    for i := 0 to Connector.CableCount - 1 do begin
      aMenuItem := TMenuItem.Create( puConnectorMenu);
      if Connector.Cables[i].FromConnector = Connector then
        aMenuItem.Caption := 'Cable to ' + string(G2.SelectedPatch.GetModuleLabel( Connector.Module.Location, Connector.Cables[i].ModuleTo))
      else
        aMenuItem.Caption := 'Cable to ' + string(G2.SelectedPatch.GetModuleLabel( Connector.Module.Location, Connector.Cables[i].ModuleFrom));
      aMenuItem.Tag := integer(Connector.Cables[i]);
      //aMenuItem.OnClick := miDeleteCablesClick;
      aMenuItem.OnClick := DoDeleteCable;
      miDeleteCable.Add(aMenuItem);
    end;

    puConnectorMenu.Popup( P.X, P.Y);
    puConnectorMenu.Tag := integer(Connector);}

    puConnectorMenu.Items.Clear;
    AddCableMenuItem( puConnectorMenu.Items, Connector);
    puConnectorMenu.Popup( P.X, P.Y);
    puConnectorMenu.Tag := integer(Connector);
  end;
end;

// ==== Communication menu =====================================================

procedure TfrmG2Main.aSendControllerSnapshotExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedSlot.SendControllerSnapshotMessage;
end;

procedure TfrmG2Main.aSendPartchSysexExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SysExSendPatch( G2.SelectedSlotIndex);
end;

procedure TfrmG2Main.aSendPerfSysexExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SysExSendPerformance;
end;

procedure TfrmG2Main.aMidiDumpExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SendDumpMidiMessage;
end;

// ==== Functions ==============================================================

procedure TfrmG2Main.CopyPatchSelection;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if assigned( FCopyPatch) then
    FCopyPatch.Free;

  FCopyPatch := TG2FilePatchPart.CopyModules( G2.SelectedPatch, G2.SelectedPatchPart, G2.SelectedPatchPart.SelectedModuleList);

  UpdateMainFormActions;
end;

procedure TfrmG2Main.DeletePatchSelection;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  if G2.SelectedPatchPart.SelectedModuleList.Count > 0 then
    G2.SelectedPatch.MessDeleteModules( G2.SelectedPatch.SelectedLocation);

  UpdateMainFormActions;
end;

procedure TfrmG2Main.PastePatchSelection;
begin
  if assigned( FCopyPatch) then begin
    sbVA.CopyPatch := FCopyPatch;
    sbFX.CopyPatch := FCopyPatch;

    UpdateMainFormActions;
  end;
end;

procedure TfrmG2Main.PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Sender is TG2GraphChildControl then
    ParameterClick( Sender, Button, Shift, X, Y, (Sender as TG2GraphChildControl).Parameter);
end;

procedure TfrmG2Main.Undo;
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.SendUndoMessage;
end;

procedure TfrmG2Main.SelectSlot( aSlotIndex : byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.SelectedSlotIndex := aSlotIndex;
  UpdateControls;
end;

procedure TfrmG2Main.SelectVariation( aSlotIndex, aVariationIndex : byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  G2.Slot[ aSlotIndex].SendSelectVariationMessage( aVariationIndex);
end;

procedure TfrmG2Main.SelectPatchLocation(aLocation: TLocationType);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    if (aLocation = ltVA) then begin
      if (G2.SelectedPatch.SelectedLocation = ltFX) then
        sbFX.Height := 10
      else
        sbFX.Height := {GetPatchWindowHeight}pPatchArea.Height - FOldSplitterPos - Splitter1.Height;
    end else
      if G2.SelectedPatch.SelectedLocation = ltVA then
        sbFX.Height := {GetPatchWindowHeight}pPatchArea.Height - Splitter1.Height - 10
      else
        sbFX.Height := {GetPatchWindowHeight}pPatchArea.Height - FOldSplitterPos - Splitter1.Height;
    G2.SelectedPatch.SelectedLocation := aLocation;
    Invalidate;
  finally
    FDisableControls := False;
  end;
end;

// ==== Event handling =========================================================

procedure TfrmG2Main.G2BeforeSendMessage(Sender: TObject; SenderID: Integer; SendMessage: TG2SendMessage);
var SubCmd : byte;
    SynthName : string;
begin
  // Waint for the response
  Screen.Cursor := crHourglass;
  // Set the responsetimer, in case of error
  ResponseTimer.Enabled := True;

  SubCmd := 0;
  if SendMessage.Size >= 4 then
    SubCmd := PStaticByteBuffer(SendMessage.Memory)^[5]; // Normally the 5 byte;

  SynthName := 'Unknown synth : ';
  if Sender is tG2 then
    SynthName := (Sender as TG2).SynthName + ' : ';

  case SubCmd of
  S_LOAD  : Statusbar1.SimpleText := SynthName + 'Loading patch...';
  Q_PATCH : Statusbar1.SimpleText := SynthName + 'Request patch...';
  Q_PATCH_NAME : Statusbar1.SimpleText := SynthName + 'Request patch name...';
  Q_PERF_SETTINGS : Statusbar1.SimpleText := SynthName + 'Request performance settings...';
  Q_SYNTH_SETTINGS : Statusbar1.SimpleText := SynthName + 'Request synth settings...';
  Q_LIST_NAMES : Statusbar1.SimpleText := SynthName + 'Request patch bank...';
  S_START_STOP_COM : Statusbar1.SimpleText := SynthName + 'Start/stop message stream...';
  Q_RESOURCES_USED : Statusbar1.SimpleText := SynthName + 'Request recourses used...';
  Q_VERSION_CNT : Statusbar1.SimpleText := SynthName + 'Request version counter...';
  else
    Statusbar1.SimpleText := SynthName + 'Sending command ' + IntToHex(SubCmd,2);
  end;
end;

procedure TfrmG2Main.G2ReceiveResponseMessage(Sender: TObject; ResponseMessage: TMemoryStream);
var G2 : TG2;
begin
  Statusbar1.SimpleText := '';
  ResponseTimer.Enabled := False;
  Screen.Cursor := crDefault;

  G2 := SelectedG2;
  if assigned(G2) then begin
    if G2.ErrorMessage then
       MessageDlg('G2 returned error message ' + IntToStr(G2.ErrorMessageNo), mtError, [mbOK], 0);
  end;
end;

procedure TfrmG2Main.ResponseTimerTimer(Sender: TObject);
begin
  // Waited 5s for the response, probably lost contact now
  ResponseTimer.Enabled := False;
  Screen.Cursor := crDefault;
end;

procedure TfrmG2Main.PanelClick(Sender: TObject);
begin
  if FDisableControls then exit;

  if Sender is TSlotPanel then begin
     SelectSlot( (Sender as TSlotPanel).SlotIndex);
    // TODO
    //sbVA.Height := G2.Slot[FSelectedSlot].Patch.FPatchDescription.FBarPosition;
  end else
    if (Sender is TG2GraphLabel) and ((Sender as TG2GraphLabel).Parent is TSlotPanel) then
      SelectSlot( ((Sender as TG2GraphLabel).Parent as TSlotPanel).SlotIndex);

  UpdateControls;
end;

procedure TfrmG2Main.G2PatchNameChange(Sender: TObject; SenderID, PatchIndex: Integer; PatchName: AnsiString);
begin
  FSlotPanel[ PatchIndex].FePatchName.Text := PatchName;
end;

procedure TfrmG2Main.G2PatchUpdate(Sender: TObject; SenderID: Integer; PatchIndex: Integer);
begin
  FSlotPanel[ PatchIndex].UpdateControls;
end;

procedure TfrmG2Main.G2DeleteModule(Sender: TObject; SenderID: integer; Location: TLocationType; ModuleIndex: integer);
begin
  UpdateMainFormActions;
end;

procedure TfrmG2Main.G2PerfSettingsUpdate(Sender: TObject; SenderID: Integer; PerfMode: Boolean);
begin
  frmPerfSettings.updateDialog;
  UpdateControls;
end;

procedure TfrmG2Main.G2SelectSlot(Sender: TObject; SenderID: Integer; Slot: Integer);
begin
  UpdateControls;
end;

procedure TfrmG2Main.G2SetModuleLabel(Sender: TObject; SenderID: integer;
  PatchIndex: byte; Location: TLocationType; ModuleIndex: byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  if G2.SelectedSlotIndex = PatchIndex then begin
    case Location of
      ltFX: sbFX.Invalidate;
      ltVA: sbVA.Invalidate;
    end;
  end;
end;

procedure TfrmG2Main.G2SetParamLabel(Sender: TObject; SenderID: integer;
  PatchIndex: byte; Location: TLocationType; ModuleIndex: byte);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  if G2.SelectedSlotIndex = PatchIndex then begin
    case Location of
      ltFX: sbFX.Invalidate;
      ltVA: sbVA.Invalidate;
    end;
  end;
end;

procedure TfrmG2Main.G2SynthSettingsUpdate(Sender: TObject;  SenderID: Integer);
var i : integer;
begin
  i := 0;
  while (i<FG2List.Count) and (FG2List[i] <> Sender) do
    inc(i);

  if (i<FG2List.Count) then begin
    rbSynth.ButtonText[i] := (FG2List[i] as TG2).SynthName;
    rbSynth.Invalidate;
  end;

  frmSynthSettings.updateDialog;
end;

procedure TfrmG2Main.G2USBActiveChange(Sender: TObject; Active: Boolean);
begin
  FDisableControls := True;
  try
    cbOnline.Checked := Active;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmG2Main.cbOnlineClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  // Disabled to prevent accidents
  //G2.USBActive := cbOnline.Checked;
  cbOnline.Checked := G2.USBActive;
end;

procedure TfrmG2Main.G2VariationChange(Sender: TObject; SenderID: Integer; Slot, Variation: Integer);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  if G2.SelectedSlotIndex = Slot then begin
    sbVA.Invalidate;
    sbFX.Invalidate;
  end;
  UpdateControls;
end;

procedure TfrmG2Main.G2CopyVariation( Sender: TObject; SenderID : integer; SlotIndex, FromVariation, ToVariation : integer);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  if (G2.SelectedSlotIndex = SlotIndex) and (G2.SelectedPatch.ActiveVariation = ToVariation) then begin
    sbVA.Invalidate;
    sbFX.Invalidate;
  end;
  UpdateControls;
end;


procedure TfrmG2Main.G2AddCable(Sender: TObject; SenderID: integer; Module: TG2FileCable);
begin
  UpdateMainFormActions;
end;

procedure TfrmG2Main.G2AddClient(Sender: TObject; ClientIndex: Integer);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  lbClientsConnected.Caption := IntToStr( G2.GetClientCount);
end;

procedure TfrmG2Main.G2AddModule(Sender: TObject; SenderID: integer; Module: TG2FileModule);
begin
  UpdateMainFormActions;
end;

procedure TfrmG2Main.G2DeleteCable(Sender: TObject; SenderID: integer;
  Location: TLocationType; FromModuleIndex, FromConnectorIndex, ToModuleIndex,
  ToConnectorIndex: integer);
begin
  UpdateMainFormActions;
end;

procedure TfrmG2Main.G2DeleteClient(Sender: TObject; ClientIndex: Integer);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) or (G2 <> Sender) then
    exit;

  lbClientsConnected.Caption := IntToStr( G2.GetClientCount);
end;


procedure TfrmG2Main.rbSynthChange(Sender: TObject);
begin
  SelectG2( rbSynth.Value);
end;

procedure TfrmG2Main.G2AfterG2Init(Sender: TObject);
begin
  UpdateControls;
  frmPatchManager.TabControl1Change(Self);
end;

procedure TfrmG2Main.G2AfterRetrievePatch(Sender: TObject; SenderID: Integer;  aSlot, aBank, aPatch: Byte);
var G2 : TG2;
begin
  if Sender is TG2 then
    G2 := Sender as TG2
  else
    exit;

  if aSlot = 4 then
    G2.Performance.USBStartInit( True)
  else
    G2.Slot[ aSlot].USBStartInit( True);
end;

procedure TfrmG2Main.G2MidiCCReceive(Sender: TObject; SenderID: Integer; MidiCC: Byte);
begin
  FLastReceivedMidiCC := MidiCC;
end;

procedure TfrmG2Main.G2AssignGlobalKnob(Sender: TObject; SenderID, KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

procedure TfrmG2Main.G2AssignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

procedure TfrmG2Main.G2CreateModule(Sender: TObject; SenderID: Integer; Module: TG2FileModule);
var G2 : TG2;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  (Module as TG2GraphModule).OnModuleClick := ModuleClick;
  (Module as TG2GraphModule).OnParameterClick := ParameterClick;
  (Module as TG2GraphModule).OnConnectorClick := ConnectorClick;
  G2.SelectedPatch.SelectedLocation := Module.Location;
end;

procedure TfrmG2Main.G2DeassignGlobalKnob(Sender: TObject; SenderID,  KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

procedure TfrmG2Main.G2DeassignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
begin
  frmParameterPages.UpdateControls;
end;

procedure TfrmG2Main.G2SelectModule(Sender : TObject; SenderID : integer; Module : TG2FileModule);
begin
  UpdateMainFormActions;
end;

procedure TfrmG2Main.G2SelectParam(Sender : TObject; SenderID : integer; Param : TG2FileParameter);
begin
  UpdateMainFormActions;
end;


initialization
  Initialized := False;

end.
