unit BVE.NMG2File;

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
//  This unit contains the G2 classes to read or write G2 objects
//  to filestream or memory stream.
//
//  Parts ported from https://github.com/msg/g2ools
//  Parts used from http://www.iaf.nl/Users/BlueHell/
//
//  ////////////////////////////////////////////////////////////////////////////

// 2011-11-14 bve : Performance chunk, first G2 gives Slotno 1..4, second G2 gives 5..8..?

// 2013-09-23 bve : modified for Xe4

interface

uses
  System.Types, System.Classes, System.SysUtils, System.Generics.Collections,
  System.math, System.SyncObjs,
  Generics.Defaults,
  //Spring.Collections,
  idSync,
  BVE.NMG2Types, BVE.NMG2Data;

const
  MIDI_BLOCK_SIZE = 479;//419; // The number of whole octets in  a full size MIDI packet

type
  TG2FilePatch = class;
  TG2FilePatchPart = class;
  TCableList = class;
  TModuleParameterList = class;
  TModuleVariationList = class;
  TParameterLabelList = class;
  TModuleLabelList = class;
  TGlobalKnob = class;
  TG2FileSlot = class;
  TG2File = class;
  TG2FileModule = class;
  TG2FileCable = class;
  TG2FileParameter = class;
  TG2FileConnector = class;
  TG2FilePerformance = class;
  TG2FileDataStream = class;

  TCreateModuleEvent = procedure(Sender: TObject; SenderID : integer; Module : TG2FileModule) of object;
  TAddModuleEvent = procedure(Sender: TObject; SenderID : integer; Module : TG2FileModule) of object;
  TDeleteModuleEvent = procedure(Sender : TObject; SenderID : integer; Location: TLocationType; ModuleIndex : integer) of object;
  TAddCableEvent = procedure(Sender: TObject; SenderID : integer; Module : TG2FileCable) of object;
  TDeleteCableEvent = procedure(Sender : TObject; SenderID : integer; Location: TLocationType; FromModuleIndex, FromConnectorIndex, ToModuleIndex, ToConnectorIndex : integer) of object;
  TAssignKnobEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; KnobIndex : integer) of object;
  TAssignGlobalKnobEvent = procedure(Sender: TObject; SenderID : integer; KnobIndex : integer) of object;
  TDeassignKnobEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; KnobIndex : integer) of object;
  TDeassignGlobalKnobEvent = procedure(Sender: TObject; SenderID : integer; KnobIndex : integer) of object;
  TParameterChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot, Variation : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; aValue : byte) of object;
  TMorphChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; Location : TLocationType; ModuleIndex, ParamIndex, aMorphIndex, aVariationIndex, aValue : byte) of object;
  TLabelValueChangeEvent = procedure(Sender: TObject; SenderID : integer; Param : TG2FileParameter) of object;
  TModuleModeChangeEvent = procedure(Sender: TObject; SenderID : integer; Slot : byte; Location : TLocationType; ModuleIndex, ParamIndex : byte; aValue : byte) of object;
  TSelectSlotEvent = procedure(Sender : TObject; SenderID : integer; Slot : integer) of object;
  TSelectModuleEvent = procedure(Sender : TObject; SenderID : integer; Module : TG2FileModule) of object;
  TSelectParamEvent = procedure(Sender : TObject; SenderID : integer; Param : TG2FileParameter) of object;
  TLogLineEvent = procedure(Sender : TObject; const LogLine : string; LogCmd : integer) of object;
  TSelectLocationEvent = procedure(Sender : TObject; SenderID : integer; Slot : byte; Location : TLocationType) of object;
  TLedEvent = procedure(Sender : TObject; const aLocation : TLocationType; const aModuleIndex, aGroupIndex, aCodeRef, aValue : byte) of Object;

  // Header for VST patch chunk file
  TFXPHeader = record
    chunkMagic  : packed array[0..3] of byte;        // 'CcnK'
    byteSize    : Cardinal;                          // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of byte;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of byte;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    name        : packed array[0..27] of byte;
    chunkSize   : Cardinal;
  end;

  // Header for VST bank chunk file
  TFXBHeader = record
    chunkMagic  : packed array[0..3] of byte;        // 'CcnK'
    byteSize    : Cardinal;                          // of this chunk, excl. magic + byteSize
    fxMagic     : packed array[0..3] of byte;        // 'FxCh', 'FPCh', or 'FBCh'
    version     : Cardinal;
    fxID        : packed array[0..3] of byte;        // fx unique id
    fxVersion   : Cardinal;
    numPrograms : Cardinal;
    future      : packed array[0..127] of byte;
    chunkSize   : Cardinal;
  end;

  // Base class for loading/saving performances and patches from/to stream
  TG2FileDataStream = class( TComponent)
  private
    FChecksum         : Integer; // MIDI checksum septet
    FPatchVersion     : byte;
    FPatchType        : byte;
  protected
    //function GetMaxVariations : byte; virtual; abstract;
    //procedure SetMaxVariations( aValue : byte); virtual; abstract;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
    procedure   Read( aChunk : TPatchChunk); virtual;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); virtual;

    function    ReadClaviaString( aStream : TStream): string;
    procedure   WriteClaviaString( aStream : TStream; aValue : string);

    function    CreateFromMidiStream(const aStream: TStream; aLogLines : TStrings): TMemoryStream;
    procedure   CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
    procedure   SaveMidiToStream( const aStream : TStream);
    function    ParseMidiConst  ( const aStream : TStream; aValue: Byte): Byte;
    function    ParseMidiSeptet ( const aStream : TStream): Byte;
    function    ParseMidiInt    ( const aStream : TStream): Integer;
    function    ParseMidiString ( const aStream : TStream): string;
    procedure   AddMidiByte( aStream : TStream; aByte: Byte);
    procedure   AddMidiInt( aStream : TStream; anInteger: Integer);
    procedure   AddMidiName( aStream : TStream; aName : string);

    class function LoadFileData( AOwner: TComponent; aStream : TStream; aLogLines : TStrings): TG2FileDataStream; virtual;
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; virtual;
    class function LoadMidiData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
    class function LoadMidiStream( AOwner : TComponent; aStream: TStream): TG2FileDataStream; virtual;

    property PatchVersion : byte read FPatchVersion write FPatchVersion;
    property PatchType : byte read FPatchType write FPatchType;
    //property MaxVariations : byte read GetMaxVariations write SetMaxVariations;
  end;

  TG2FileModule = class(TComponent)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    // File storage
    FTypeID         : TBits8;
    FModuleIndex    : TBits8;
    FCol            : TBits7;
    FRow            : TBits7;
    FModuleColor    : TBits8;
    FUprate         : TBits1; // processing rate increased due to audio modulation
    FIsLed          : TBits1; // unknown function
    //FUnknown1     : TBits8;
    FUnknown1       : TBits6;
    //FUnknown2     : TBits4;
    FModeCount      : TBits4;
    FModeInfoArray  : array of TBits6;

    // Work properties
    FHeightUnits    : integer;
    FNewCol         : integer;
    FNewRow         : integer;

    FPage           : TModulePage;
    FPageIndex      : integer;

    // Interface
    //FSelected       : Boolean;
    FSelectedParam  : integer;
    FLocation       : TLocationType;
    FModuleName     : string;
    FNewUprate      : TBits1; // for calculating uprate changes
    FInConnArray    : array of TG2FileConnector;
    FOutConnArray   : array of TG2FileConnector;
    FParamArray     : array of TG2FileParameter;
    FModeArray      : array of TG2FileParameter;

    FModuleFileName : string;

    function    GetInputConnector( ConnectorIndex : integer): TG2FileConnector;
    function    GetOutputConnector( ConnectorIndex : integer): TG2FileConnector;
    function    GetInputConnectorCount : integer;
    function    GetOutputConnectorCount : integer;
    function    GetParameter( ParamIndex : integer): TG2FileParameter;
    function    GetMode( ModeIndex : integer): TG2FileParameter;
    function    GetParameterCount : integer;
    function    GetModeCount : integer;
    function    GetColor: integer;
    procedure   SetUprate( Value : TBits1);
    procedure   SetModeInfo( aModeIndex : integer; aValue : TBits6);
    function    GetModeInfo( aModeIndex : integer): TBits6;
    function    GetSelectedParam : TG2FileParameter;
    procedure   SetSelectedParam( aValue : TG2FileParameter);
    function    GetSelected: boolean;
  protected
    procedure   SetCol( Value : TBits7); virtual;
    procedure   SetRow( Value : TBits7); virtual;
    procedure   SetModuleColor( Value : TBits8); virtual;
    procedure   SetModuleName( aValue : string); virtual;
    procedure   SetModuleIndex( const aValue : TBits8); virtual;
    procedure   SetLocation( const aValue : TLocationType); virtual;
    //procedure   SetSelected( aValue: boolean); virtual;
    procedure   SetNewCol( aValue : TBits7);
    procedure   SetNewRow( aValue : TBits7);
    function    GetNewCol : TBits7; virtual;
    function    GetNewRow : TBits7; virtual;
    function    GetRowIndex : integer;
    function    GetAssignableKnobCount : integer;
  public
    constructor Create( aPatchPart : TG2FilePatchPart); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    procedure   InitModule( aLocation : TLocationType; aModuleType : integer); virtual;

    function    CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule; virtual;
    procedure   Copy( aModule : TG2FileModule); virtual;

    function    CreateParameter: TG2FileParameter; virtual;
    function    CreateConnector: TG2FileConnector; virtual;

    procedure   InvalidateControl; virtual;
    procedure   InvalidateCables;
    procedure   InvalidateConnectors;
    procedure   InvalidateParameters;

    procedure   SelectModule(const aValue, aExclusive : boolean); virtual;

    function    GetNextParam : TG2FileParameter;
    function    GetPrevParam : TG2FileParameter;
    procedure   SelectNextParam;
    procedure   SelectPrevParam;

    function    CableCount : integer;

    property    TypeID      : TBits8 read FTypeID write FTypeID;
    property    ModuleIndex : TBits8 read FModuleIndex write SetModuleIndex;
    property    Col         : TBits7 read FCol write SetCol;
    property    Row         : TBits7 read FRow write SetRow;
    property    ModuleColor : TBits8 read FModuleColor write SetModuleColor;
    property    Uprate      : TBits1 read FUprate write SetUprate;
    property    IsLed       : TBits1 read FIsLed write FIsLed;
    property    ModeInfo    [ Index : integer]: TBits6 read GetModeInfo write SetModeInfo;
    property    PatchPart : TG2FilePatchPart read FPatchPart write FPatchPart;
    property    ModuleName  : string read FModuleName write SetModuleName;
    property    InConnector [ Index : integer]: TG2FileConnector read GetInputConnector;
    property    OutConnector[ Index : integer]: TG2FileConnector read GetOutputConnector;
    property    Parameter   [ Index : integer]: TG2FileParameter read GetParameter;
    property    Mode        [ Index : integer]: TG2FileParameter read GetMode;
    property    HeightUnits : integer read FHeightUnits write FHeightUnits;
    property    InConnectorCount : integer read GetInputConnectorCount;
    property    OutConnectorCount : integer read GetOutputConnectorCount;
    property    ParameterCount : integer read GetParameterCount;
    property    ModeCount : integer read GetModeCount;
    property    Location : TLocationType read FLocation write Setlocation;
    property    Color : integer read GetColor;
    property    Selected    : boolean read GetSelected{ write SetSelected};
    property    NewRow : TBits7 read GetNewRow write SetNewRow;
    property    NewCol : TBits7 read GetNewCol write SetNewCol;
    property    NewUprate : TBits1 read FNewUprate write FNewUprate;
    property    RowIndex    : integer read GetRowIndex;
    property    SelectedParam : TG2FileParameter read GetSelectedParam write SetSelectedParam;
    property    AssignableKnobCount : integer read GetAssignableKnobCount;
    property    ModuleFileName : string read FModuleFileName write FModuleFileName;
    property    Page : TModulePage read FPage;
    property    PageIndex : integer read FPageIndex;
  end;

  TModuleList = class( TObjectList<TG2FileModule>)
  private
    [Weak] FPatchPart   : TG2FilePatchPart;

    FLocation    : TBits2;
    FModuleCount : TBits8;
    function    GetModule( aIndex : integer) : TG2FileModule;
    procedure   SetModule( aIndex : integer; const aValue : TG2FileModule);
    function    GetUnitsRect: TRect;
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList);
    destructor  Destroy; override;
    procedure   Init;
    function    FindModule( aModuleIndex : integer) : TG2FileModule;
    function    ModuleAtLocation(const aRow, aCol : integer): TG2FileModule;
    function    GetMaxModuleIndex : integer;
    function    GetNoOffModuleType( aModuleType : byte) : integer;
    function    GetNoOffExtendedModules : integer;
    function    GetSelectedCount: integer;
    procedure   DeleteModule( aModuleIndex : byte);
    function    GetModuleAbove( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleUnder( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleLeft( aModuleIndex : byte) : TG2FileModule;
    function    GetModuleRight( aModuleIndex : byte) : TG2FileModule;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddModule( aValue : TG2FileModule): integer;

    property    Items[ aIndex : integer]: TG2FileModule read GetModule write SetModule; default;
    property    UnitsRect : TRect read GetUnitsRect;
  end;

  TG2FileCable = class(TComponent)
  private
    [Weak] FFromConnector : TG2FileConnector;
    [Weak] FToConnector   : TG2FileConnector;

    FCableColor    : TBits3;
    FModuleFrom    : TBits8;
    FConnectorFrom : TBits6;
    FLinkType      : TBits1; // Guess 0 - input to input; 1 - output to input
    FModuleTo      : TBits8;
    FConnectorTo   : TBits6;

  protected
    procedure   SetCableColor( Value : byte); virtual;
  public
    constructor Create( aPatchPart : TG2FilePatchPart); reintroduce; virtual;
    constructor CopyCreate( aPatchPart : TG2FilePatchPart; aCable : TG2FileCable);
    destructor  Destroy; override;
    procedure   Init;
    procedure   ConnectorMoved; virtual;
    procedure   Invalidate; virtual;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    property    ModuleFrom : TBits8 read FModuleFrom write FModuleFrom;
    property    ConnectorFrom : TBits6 read FConnectorFrom write FConnectorFrom;
    property    LinkType : TBits1 read FLinkType write FLinkType;
    property    ModuleTo : TBits8 read FModuleTo write FModuleTo;
    property    ConnectorTo : TBits6 read FConnectorTo write FConnectorTo;
    property    FromConnector : TG2FileConnector read FFromConnector write FFromConnector;
    property    ToConnector : TG2FileConnector read FToConnector write FToConnector;
    property    CableColor : Byte read FCableColor write SetCableColor;
  end;

  TCableList = class( TObjectList<TG2FileCable>)
  private
    [Weak] FPatchPart  : TG2FilePatchPart;

    FLocation   : TBits2;
    FUnknown    : TBits12;
    FCableCount : TBits10;

    function    GetCable( aIndex : integer) : TG2FileCable;
    procedure   SetCable( aIndex : integer; const aValue : TG2FileCable);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aCableList : TCableList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteCable( aCable : TG2FileCable);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindCable( FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;
    function    AddCable( aValue : TG2FileCable): integer;
    property    Items[ aIndex : integer]: TG2FileCable read GetCable write SetCable; default;
    property    Unknown : TBits12 read FUnknown write FUnknown;
  end;

  TCurrentNote = class
  private
    FNote    : TBits7;
    FAttack  : TBits7;
    FRelease : TBits7;
  public
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
  end;

  TCurrentNoteList = class( TObjectList<TCurrentNote>)
  private
    [Weak] FPatch : TG2FilePatch;

    FLastNote  : TCurrentNote;

    FNoteCount : TBits5;

    function    GetNote( aIndex : integer) : TCurrentNote;
    procedure   SetNote( aIndex : integer; const aValue : TCurrentNote);
  public
    constructor Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddNote( aValue : TCurrentNote): integer;
    property    Items[ aIndex : integer]: TCurrentNote read GetNote write SetNote; default;
  end;

  TModuleParamValues = class
  private
    // Set of parameter values for one module one variation

    FVariation           : TBits8;
    FParamValueArray     : array of TBits7;

    function    GetParamValue( aIndex : integer): TBits7;
    procedure   SetParamValue( aIndex : integer; aValue : TBits7);
    function    GetParamCount: integer;
    procedure   SetParamCount(const Value: integer);
  public
    constructor Create;
    constructor Copy( aParamValues : TModuleParamValues);
    destructor  Destroy; override;
    procedure   CopyValues( aParamValues : TModuleParamValues);
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk; aParamCount : integer);
    procedure   Write( aChunk : TPatchChunk);

    property    Variation : TBits8 read FVariation write FVariation;
    property    ParamValues[ aIndex : integer]: TBits7 read GetParamValue write SetParamValue;
    property    ParamCount : integer read GetParamCount write SetParamCount;
  end;

  TModuleVariationList = class( TObjectList<TModuleParamValues>)
  private
    // Set of parameter values for a module all variations
    // 0 .. 7 : variations 1 .. 8, 8 : init variation
    [Weak] FPatchPart    : TG2FilePatchPart;

    FModuleIndex         : TBits8;
    FParameterCount      : Integer;

    function    GetVariation( aIndex : integer) : TModuleParamValues;
    procedure   SetVariation( aIndex : integer; const aValue : TModuleParamValues);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor Copy( AOwnsObjects : boolean; aModuleVariationList : TModuleVariationList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   InitModuleVariationList( aModuleIndex : byte; aModuleType : integer);
    procedure   Read( aChunk : TPatchChunk; aVariationCount : integer);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : integer);
    procedure   AddNewVariation( aVariation : byte);
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindVariation( aVariation : integer): TModuleParamValues;
    function    AddVariation( aValue : TModuleParamValues): integer;
    property    Items[ aIndex : integer]: TModuleParamValues read GetVariation write SetVariation; default;
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property    ParameterCount : Integer read FParameterCount write FParameterCount;
  end;

  TModuleParameterList = class( TObjectList<TModuleVariationList>)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FLocation       : Tbits2;

    function    GetModuleVariationList( aIndex : integer) : TModuleVariationList;
    procedure   SetModuleVariationList( aIndex : integer; const aValue : TModuleVariationList);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aModuleParameterList : TModuleParameterList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   DeleteModule( aModuleIndex : Byte);
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindModuleVariationList( aModuleIndex : integer): TModuleVariationList;
    function    FindParamValue( aModuleIndex, aVariation, aParamIndex : integer): TBits7;
    function    AddModuleVariationList( aValue : TModuleVariationList): integer;
    property    Items[ aIndex : integer]: TModuleVariationList read GetModuleVariationList write SetModuleVariationList; default;
    property    Location : TBits2 read FLocation write FLocation;
  end;

  TMorphSetting = class
  private
    FDialArray : array of TBits7;
    FModeArray : array of TBits7;

    function   GetDial( aVariation : integer): TBits7;
    procedure  SetDial( aVariation : integer; aValue : TBits7);
    function   GetMode( aVariation : integer): TBits7;
    procedure  SetMode( aVariation : integer; aValue : TBits7);
  public
    constructor Create;
    destructor  Destroy; override;

    property   Dials[ Index : integer]: TBits7 read GetDial write SetDial;
    property   Modes[ Index : integer]: TBits7 read GetMode write SetMode;
  end;

  TMorphParameter = class
  private
    // Morph value of a parameter
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FParamIndex  : TBits7;
    FMorph       : TBits4;
    FRange       : TBits8;
  public
    procedure   Init;
    procedure   Write( aChunk : TPatchChunk);
    property    Range : TBits8 read FRange write Frange;
  end;

  TMorphParameters = class
  private
    [Weak] FPatch   : TG2FilePatch;

    FVariationCount : TBits8;
    Unknown1        : TBits4;
    Unknown2,
    Unknown3,
    Unknown4,
    Unknown5,
    Unknown6,
    Unknown7        : TBits8; // 0..5
    Unknown8        : TBits4;
    FMorphCount     : TBits4;
    FReserved1      : integer;
    FReserved2      : TBits4;
  public
    constructor Create( aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
  end;

  TPatchParamList = class( TObjectList<TMorphParameter>)
  private
    // Keeps the patch parameter values and a list of the parameter morph values

    [Weak] FPatch : TG2FilePatch;

    FPatchVol    : TBits7;
    FActiveMuted : TBits7;
    FGlide       : TBits7;
    FGlideTime   : TBits7;
    FBend        : TBits7;
    FSemi        : TBits7;
    FVibrato     : TBits7;
    FCents       : TBits7;
    FRate        : TBits7;
    FArpeggiator : TBits7;
    FArpTime     : TBits7;
    FArpType     : TBits7;
    FOctaveShift : TBits7;
    FSustain     : TBits7;
    FOctaves     : TBits7;

    function    GetMorphParam( aIndex : integer) : TMorphParameter;
    procedure   SetMorphParam( aIndex : integer; const aValue : TMorphParameter);
  public
    constructor Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Copy( aFromVariation : TPatchParamList);

    procedure   AddNewMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aRange: byte);
    procedure   DelMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex: byte);
    function    FindMorphParam( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
    procedure   DeleteModule( aLocation, aModuleIndex : byte);

    function    AddMorphParam( aValue : TMorphParameter): integer;
    property    Items[ aIndex : integer]: TMorphParameter read GetMorphParam write SetMorphParam; default;
  end;

  TPatchSettings = class
  private
    [Weak] FPatch       : TG2FilePatch;

    FLocation           : Tbits2;
    FSectionCnt         : TBits8;
    FMorphArray         : array of TMorphSetting; // [0..NMORPHS-1]
    FVariationListArray : array of TPatchParamList;
    function    GetPatchParamList( aVariation : integer): TPatchParamList;
    procedure   SetPatchParamList( aVariation : integer; aValue : TPatchParamList);
    function    GetVariationCount: TBits8;
  public
    constructor Create( aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    property    VariationCount : TBits8 read GetVariationCount;
    property    PatchParamList[ Index : integer]: TPatchParamList read GetPatchParamList write SetPatchParamList;
  end;

  TController = class;

  TKnob = class
  private
    [Weak] FParameter : TG2FileParameter;

    FAssigned    : TBits1;
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FIsLed       : TBits2;
    FParamIndex  : TBits7;

    FKnobIndex   : integer;
    function     GetKnobValue : byte;
    procedure    SetKnobValue( aValue : byte);
    function     GetKnobFloatValue : single;
    procedure    SetKnobFloatValue( aValue : single);
    function     GetKnobHighValue : byte;
    function     GetKnobLowValue : byte;
    function     GetKnobButtonValue : byte;
    procedure    SetKnobButtonValue( aValue : byte);
    function     GetKnobButtonFloatValue : single;
    procedure    SetKnobButtonFloatValue( aValue : single);
  public
    procedure    Init; virtual;
    procedure    Read( aChunk : TPatchChunk); virtual;
    procedure    Write( aChunk : TPatchChunk); virtual;

    property     IsAssigned : TBits1 read FAssigned write FAssigned;
    property     Location : TBits2 read FLocation write FLocation;
    property     ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property     ParamIndex : TBits7 read FParamIndex write FParamIndex;
    property     Parameter : TG2FileParameter read FParameter write FParameter;
    property     KnobValue : byte read GetKnobValue write SetKnobValue;
    property     KnobFloatValue : single read GetKnobFloatValue write SetKnobFloatValue;
    property     KnobButtonValue : byte read GetKnobButtonValue write SetKnobButtonValue;
    property     KnobButtonFloatValue : single read GetKnobButtonFloatValue write SetKnobButtonFloatValue;
    property     KnobHighValue : byte read GetKnobHighValue;
    property     KnobLowValue : byte read GetKnobLowValue;
    property     KnobIndex : integer read FKnobIndex;
  end;

  TKnobList = class( TObjectList<TKnob>)
  private
    [Weak] FPatch : TG2FilePatch;

    FKnobCount : TBits16;

    function    GetKnob( aIndex : integer) : TKnob;
    procedure   SetKnob( aIndex : integer; const aValue : TKnob);
  public
    constructor Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteModule( aLocation, aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    function    AddKnob( aValue : TKnob): integer;
    property    Items[ aIndex : integer]: TKnob read GetKnob write SetKnob; default;
  end;

  TController = class
  private
    [Weak] FKnob      : TKnob;
    [Weak] FParameter : TG2FileParameter;

    FMidiCC      : TBits7;
    FLocation    : TBits2;
    FModuleIndex : TBits8;
    FParamIndex  : TBits7;

  public
    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);
    property  MidiCC : TBits7 read FMidiCC write FMidiCC;
    property  Location : TBits2 read FLocation write FLocation;
    property  ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property  ParamIndex : TBits7 read FParamIndex write FParamIndex;
    property  Parameter : TG2FileParameter read FParameter write FParameter;
    property  Knob : TKnob read FKnob write FKnob;
  end;

  TControllerList = class( TObjectList<TController>)
  private
    [Weak] FPatch : TG2FilePatch;

    FControllerCount : TBits7;

    function    GetController( aIndex : integer) : TController;
    procedure   SetController( aIndex : integer; const aValue : TController);
  public
    constructor Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
    destructor  Destroy; override;
    procedure   Init;
    procedure   DeleteModule( aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
    function    AddController( aValue : TController): integer;
    property    Items[ aIndex : integer]: TController read Getcontroller write SetController; default;
  end;

  TParamLabel = class(TObject)
  private
    FNameArray  : array of byte; // [0..6]
    function    GetName : string;
    procedure   SetName( aValue : string); reintroduce;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    property    ParamLabel : string read GetName write SetName;
  end;

  TParamLabelParamList = class( TObjectList<TParamLabel>)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FIsString   : TBits8;
    FParamLen   : TBits8;
    FParamIndex : TBits8;

    function    GetParamLabel( aIndex : integer) : TParamLabel;
    procedure   SetParamLabel( aIndex : integer; const aValue : TParamLabel);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopyCreate( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aParamLabelParam : TParamLabelParamList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabel( aValue : TParamLabel): integer;

    property    Items[ aIndex : integer]: TParamLabel read GetParamLabel write SetParamLabel; default;
    property    IsString : TBits8 read FIsString write FIsString;
    property    ParamLen : TBits8 read FParamLen write FParamLen;
    property    ParamIndex : TBits8 read FParamIndex write FParamIndex;
  end;

  TParamLabelModuleList = class( TObjectList<TParamLabelParamList>)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FModuleIndex : TBits8;
    FModuleLen   : TBits8;

    function    GetParamLabelParam( aIndex : integer) : TParamLabelParamList;
    procedure   SetParamLabelParam( aIndex : integer; const aValue : TParamLabelParamList);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopyCreate( AOwnsObjects  : boolean; aPatchPart : TG2FilePatchPart; aParamLabelModule : TParamLabelModuleList);
    destructor  Destroy; override;
    procedure   Init;
    procedure   InitParamLabelModuleList( aModuleIndex : byte; aModuleType : integer);
    function    FindParamLabelParam( aParamIndex : integer): TParamLabelParamList;
    function    GetModuleLabelsLength: integer;
    procedure   AddParamLabel( aParamIndex, aLabelIndex : byte; aName : string);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabelParam( aValue : TParamLabelParamList): integer;
    function    GetParameterLabelCount( aParamIndex: byte): integer;

    property    Items[ aIndex : integer]: TParamLabelParamList read GetParamLabelParam write SetParamLabelParam; default;
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
  end;

  TParameterLabelList = class( TObjectList<TParamLabelModuleList>)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FLocation    : Tbits2;

    function    GetParamLabelModule( aIndex : integer) : TParamLabelModuleList;
    procedure   SetParamLabelModule( aIndex : integer; const aValue : TParamLabelModuleList);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aParameterLabels : TParameterLabelList);
    destructor  Destroy; override;
    function    FindParamLabelModule( aModuleIndex : integer): TParamLabelModuleList;
    function    FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
    procedure   AddParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte; aName : string);
    procedure   AddParamLabels( aModuleIndex, aParamIndex : byte; aNames : string);
    function    GetParameterLabelCount( aModuleIndex, aParamIndex: byte): integer;
    procedure   Init;
    procedure   DeleteParamLabel( aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk; aModuleCount : integer);
    procedure   Write( aChunk : TPatchChunk);
    function    AddParamLabelModule( aValue : TParamLabelModuleList): integer;

    property    Items[ aIndex : integer]: TParamLabelModuleList read GetParamLabelModule write SetParamLabelModule; default;
  end;

  TModuleLabel = class
  private

    FModuleIndex : TBits8;
    FName        : string;

    function GetName: string;
    procedure SetName(const Value: string); reintroduce;
  public
    constructor Copy( aModuleLabel : TModuleLabel);
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    property    ModuleIndex : TBits8 read FModuleIndex write FModuleIndex;
    property    ModuleLabel : string read GetName write SetName;
  end;

  TModuleLabelList = class( TObjectList<TModuleLabel>)
  private
    [Weak] FPatchPart : TG2FilePatchPart;

    FLocation  : TBits2;
    FUnknown   : TBits6;
    FNameCount : TBits8;

    function    GetModuleLabel( aIndex : integer) : TModuleLabel;
    procedure   SetModuleLabel( aIndex : integer; const aValue : TModuleLabel);
  public
    constructor Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
    constructor CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aModuleLabels : TModuleLabelList);
    destructor  Destroy; override;
    procedure   Init;
    function    FindModuleLabel( aModuleIndex : byte): string;
    procedure   DeleteModuleLabel( aModuleIndex : byte);
    procedure   AddNewModuleLabel( aModuleIndex : byte; aName : string);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddModuleLabel( aValue : TModuleLabel): integer;

    property    Items[ aIndex : integer]: TModuleLabel read GetModuleLabel write SetModuleLabel; default;
    property    Unknown : TBits6 read FUnknown write FUnknown;
  end;

  TPatchDescription = class
  private
    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4,
    FUnknown5,
    FUnknown6,
    FUnknown7        : TBits8; // array[0..6] of Tbits8;
    FUnknown8        : TBits5;
    FVoiceCount      : TBits5;
    FBarPosition     : TBits14;
    FUnknown9        : TBits3;
    FRedVisible      : TBits1;
    FBlueVisible     : TBits1;
    FYellowVisible   : TBits1;
    FOrangeVisible   : TBits1;
    FGreenVisible    : TBits1;
    FPurpleVisible   : TBits1;
    FWhiteVisible    : TBits1;
    FMonoPoly        : TBits2;
    FActiveVariation : Byte;
    FCategory        : Byte;
    FUnknown10       : TBits4;
  public
    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    property  VoiceCount : TBits5 read FVoiceCount write FVoiceCount;
    property  MonoPoly : Tbits2 read FMonoPoly write FMonoPoly;
    property  ActiveVariation : Byte read FActiveVariation write FActiveVariation;
    property  BarPosition : TBits14 read FBarPosition write FBarPosition;
    property  RedVisible : TBits1 read FRedVisible write FRedVisible;
    property  BlueVisible : TBits1 read FBlueVisible write FBlueVisible;
    property  YellowVisible : TBits1 read FYellowVisible write FYellowVisible;
    property  OrangeVisible : TBits1 read FOrangeVisible write FOrangeVisible;
    property  GreenVisible : TBits1 read FGreenVisible write FGreenVisible;
    property  PurpleVisible : TBits1 read FPurpleVisible write FPurpleVisible;
    property  WhiteVisible : TBits1 read FWhiteVisible write FWhiteVisible;
    property  Categorie : Byte read FCategory write FCategory;
  end;

  TPatchNotes = class
  private
    FText : packed array of byte;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   SetLines( aLines : TStrings);
    procedure   GetLines( aLines : TStrings);
    function    GetText: string;
    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    property Text : string read GetText;
  end;

  TG2FilePatchPart = class( TComponent)
  private
    [Weak] FPatch         : TG2FilePatch;
    //[Weak] FSelectedParam : TG2FileParameter;
    [Weak] FFocusedModule : TG2FileModule;

    FLocation           : TLocationType;
    FModuleList         : TModuleList;
    FSelectedModuleList : TModuleList;
    FCableList          : TCableList;
    FParameterList      : TModuleParameterList;
    FModuleLabelList    : TModuleLabelList;
    FParameterLabelList : TParameterLabelList;
    function GetSelectedParam: TG2FileParameter;

  protected
    procedure   SetLocation( aValue : TLocationType);
    function    GetSelectedModuleList : TModuleList;
    //procedure   SetSelectedParam( aValue : TG2FileParameter); virtual;
    function    GetVariationCount: integer;
    procedure   SetFocusedModule( const aModule : TG2FileModule);
  public
    constructor Create( aPatch : TG2FilePatch); reintroduce; virtual;
    constructor CopyModules( aPatch : TG2FilePatch; aSrcePatchPart : TG2FilePatchPart; aModuleList : TModuleList);
    destructor  Destroy; override;

    procedure   Init;
    procedure   Read( aChunk : TPatchChunk);
    function    ReadChunk( aChunk : TPatchChunk): boolean;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);

    procedure   DeleteModule( aModuleIndex : integer);
    procedure   AddCable( aCable : TG2FileCable);
    procedure   DeleteCable( aCable : TG2FileCable);
    function    GetMaxModuleIndex : integer;
    function    GetUniqueModuleNameSeqNr( aModuleFileName : string): integer;
    function    GetNoOffModuleType( aModuleType : byte): integer;
    function    FindModuleLabel( aModuleIndex : byte): string;
    function    FindParamValue( aModuleIndex, aVariation, aParamIndex : byte): integer;
    function    FindModuleVariationList( aModuleIndex: byte): TModuleVariationList;
    function    FindModule( aModuleIndex : byte): TG2FileModule;
    function    GetMorphValue( aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aVariation : byte): byte;
    function    GetParameterLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
    procedure   SetParameterLabel( aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
    function    GetParameterLabelCount( aModuleIndex, aParamIndex : byte) : integer;
    function    GetModuleLabel( aModuleIndex : byte): string;
    procedure   SetModuleLabel( aModuleIndex : byte; aValue : string);
    procedure   CopyVariation( aFromVariation, aToVariation : byte);
    function    FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
    procedure   AddParamLabel(aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
    procedure   AddNewModuleLabel( aModuleIndex : byte; aValue : string);
    procedure   AddModuleToSelection( aModule : TG2FileModule);
    procedure   RemoveModuleFromSelection( aModule : TG2FileModule);
    //procedure   FocusModuleAbove;
    //procedure   FocusModuleUnder;
    //procedure   FocusModuleLeft;
    //procedure   FocusModuleRight;
    procedure   SelectAll;
    procedure   UnselectModules;
    procedure   SelectNextModuleParam;
    procedure   SelectPrevModuleParam;
    function    CreateModule( aModuleIndex : byte; aModuleType : byte): TG2FileModule;
    function    CreateCable( aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; virtual;

    procedure   InvalidateParameters;
    procedure   InvalidateConnectors;

    property    Patch : TG2FilePatch read FPatch write FPatch;
    property    Location : TLocationType read FLocation write SetLocation;
    property    ModuleList : TModuleList read FModuleList;
    property    SelectedModuleList : TModuleList read GetSelectedModuleList;
    //property    SelectedParam : TG2FileParameter read FSelectedParam write SetSelectedParam;
    property    SelectedParam : TG2FileParameter read GetSelectedParam;
    property    FocusedModule : TG2FileModule read FFocusedModule write SetFocusedModule;
    property    CableList : TCableList read FCableList;
    property    ParameterList : TModuleParameterList read FParameterList;
    property    ModuleLabelList : TModuleLabelList read FModuleLabelList;
    property    ParameterLabelList : TParameterLabelList read FParameterLabelList;
    property    VariationCount : integer read GetVariationCount;
  end;

  TG2FilePatch = class( TG2FileDataStream, IG2Subject)
  private
    [Weak] FG2          : TG2File;
    [Weak] FPerformance : TG2FilePerformance;
    [Weak] FSlot        : TG2FileSlot;

    FObserverList       : TList<IG2Observer>;

    FPatchPartArray     : array of TG2FilePatchPart; // [0..2]
    FPatchSettings      : TPatchSettings;
    FPatchDescription   : TPatchDescription;
    FMorphParameters    : TMorphParameters;
    FKnobList           : TKnobList;
    FControllerList     : TControllerList;
    FCurrentNoteList    : TCurrentNoteList;
    FPatchNotes         : TPatchNotes;
    // Not found it in the file so far
    FSelectedMorphIndex : integer;
    FSelectedLocation   : TLocationType;
    FSelectedParamPage  : integer;
    FSelectedParamPageColumn : integer;

    FEditAllVariations  : boolean;

    FLedList        : TList<IG2DataLed>; // single leds
    FLedStripList   : TList<IG2DataLed>; // mini-vu's and sequencer leds
    FLedComparison  : IComparer<IG2DataLed>;

    FOnLedEvent : TLedEvent;

    procedure   DoSelectLocation;
    procedure   DoFocusModule;
    procedure   DoSelectParam;
    procedure   DoLabelValueChange( aParam : TG2FileParameter);
    procedure   DoMorphChange( Slot: byte; Location: TLocationType; ModuleIndex, ParamIndex, aMorphIndex, aVariationIndex, aValue: byte);

    procedure   SetG2( aValue : TG2File);
    procedure   SetPerformance( aValue : TG2FilePerformance);
    procedure   SetSlot( aValue : TG2FileSlot);
    function    GetPatchPart( aIndex : integer): TG2FilePatchPart;
    function    GetModuleList( aIndex : integer): TModuleList;
    function    GetCableList( aIndex : integer): TCableList;
    function    GetParameterList( aIndex : integer): TModuleParameterList;
    function    GetModuleLabelList( aIndex : integer): TModuleLabelList;
    function    GetParameterLabelList( aIndex : integer): TParameterLabelList;
    function    GetPatchName : string;
    procedure   SetPatchName( aValue : string);
    function    GetVariation : byte;
    procedure   SetVariation( aValue : byte);
    function    GetVariationCount: integer;
  protected
    procedure   SetSelectedMorphIndex( aValue : integer); virtual;
    procedure   SetSelectedLocation( aLocation : TLocationType); virtual;
  public

    constructor Create( AOwner: TComponent); override;
    constructor CopyModules( AOwner: TComponent; aPatch : TG2FilePatch; aModuleList : TModuleList);
    destructor  Destroy; override;

    procedure   Init; virtual;
    function    ReadChunk( aChunk : TPatchChunk): boolean;
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); override;

    procedure   RegisterObserver( aObserver : IG2Observer);
    procedure   RemoveObserver( aObserver : IG2Observer);
    procedure   NotifyObservers( aG2Event : TG2Event);

    function    GetModule( LocationIndex, ModuleIndex: integer): TG2FileModule; //virtual;
    function    GetModuleCount( LocationIndex : integer): integer;

    procedure   InitParameters;
    procedure   InitNames;
    procedure   InitKnobs;

    function    GetMorphKnobParameter( aIndex : integer): TG2FileParameter;
    function    GetPatchKnobParameter( aIndex : integer): TG2FileParameter;

    procedure   InvalidateParameters;
    procedure   InvalidateConnectors;

    function    GetMaxModuleIndex( aLocation : TLocationType) : integer;
    function    GetNoOffModuleType( aLocation : TLocationType; aModuleType : byte) : integer;
    function    GetNoOffExtendedModules : integer; // Number of modules that are not compatible with original editor
    function    GetActiveVariation : byte;

    function    GetModuleName( aLocation : TLocationType; aModuleIndex : byte): string;

    function    GetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte): byte;
    procedure   SetParamInPatch( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte; aValue: byte); virtual;
    procedure   SetParamValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation, aValue : byte); virtual;

    procedure   SelectParamInPatch( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte);
    procedure   SelectParam( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte); virtual;

    procedure   SetModeInPatch( aLocation : TLocationType; aModuleIndex : byte; aParamIndex, aValue : byte); virtual;
    function    GetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte): byte;
    procedure   SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aValue: byte); virtual;
    procedure   SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte); virtual;
    function    GetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aVariation : byte): byte;

    procedure   SetVoiceCount( aValue : byte); virtual;
    function    GetVoiceCount : byte;
    procedure   SetVoiceMode( aValue : byte); virtual;
    function    GetVoiceMode : byte;
    procedure   SetMasterClock( aValue : byte);
    function    GetMasterClock : byte;
    procedure   SetMasterClockRun( aValue : byte);
    function    GetMasterClockRun : byte;

    procedure   SaveToFile( aStream : TStream);
{$IFDEF MSWINDOWS}
    procedure   SaveAsFXP( aStream : TStream);
{$ENDIF}
    function    LoadFromFile( aStream : TStream; aLogLines : TStrings): Boolean;
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; override;

    function    CreatePatchPart( aLocation : TLocationType): TG2FilePatchPart; virtual;
    function    CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule; virtual;
    function    CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable; virtual;
    function    CreateParameter( aModuleIndex : byte): TG2FileParameter; virtual;
    function    CreateLed( const aLedType  : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID : byte; const aGroupCount : integer): IG2DataLed; virtual;
    procedure   CreateLeds( const aLocation : TLocationType; const aModuleIndex, aModuleType : byte);

    procedure   AddModuleToPatch( aLocation : TLocationType; aModule: TG2FileModule);
    procedure   DeleteModuleFromPatch( aLocation : TLocationType; aModule: TG2FileModule);

    procedure   AddCableToPatch( aLocation : TLocationType; aCable: TG2FileCable);
    procedure   DeleteCableFromPatch( aLocation : TLocationType; aCable: TG2FileCable); virtual;

    function    FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    function    FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;

    function    AssignKnobInPatch( aKnobIndex : integer; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TKnob;
    procedure   DeassignKnobInPatch( aKnobIndex : integer);
    function    GetKnob(KnobIndex: integer): TKnob;

    function    GetMidiCC( aMidiCC : byte): TController;
    function    AssignMidiCCInPatchX(aMidiCC: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TController;
    procedure   DeassignMidiCCInPatchX(aMidiCC: byte);

    function    GetMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
    function    HasMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation : byte): boolean;

    procedure   CopyVariation( aFromVariation, aToVariation : byte);

    function    GetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex, alabelIndex : byte): string;
    procedure   SetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
    function    GetParameterLabelCount( aLocation : TLocationType; aModuleIndex, aParamIndex : byte) : integer;
    function    GetModuleLabel( aLocation : TLocationType; aModuleIndex : byte): string;
    procedure   SetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aValue : string);

    procedure   UnselectModules( aLocation : TLocationType);

    function    MessAddModule( aLocation : TLocationType; aModuleTypeID, aCol, aRow: byte): boolean; virtual;
    function    MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean; virtual;
    // Abstract functions
    function    MessCopyParameters( aSrcePatch : TG2FilePatchPart; aToLocation : TLocationType; aFromVariation, aToVariation : integer): boolean; virtual;
    function    MessAddConnection( aLocation : TLocationType; aFromConnection, aToConnection : TG2FileConnector): boolean; virtual;
    function    MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean; virtual;
    function    MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean; virtual;
    function    MessDeleteModules( aLocation : TLocationType): boolean; virtual;
    function    MessMoveModule( aLocation : TLocationType; aModuleIndex, aCol, aRow : byte): boolean; virtual;
    function    MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean; virtual;
    function    MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex, aLabelIndex: byte; aName: string): boolean; virtual;
    function    MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : string): boolean; virtual;

    property    PatchName : string read GetPatchName write SetPatchName;
    property    ActiveVariation : byte read GetActiveVariation; // TODO Don't remember what....
    property    Variation : byte read GetVariation write SetVariation;
    property    VariationCount : integer read GetVariationCount;
    property    SelectedMorphIndex : integer read FSelectedMorphIndex write SetSelectedMorphIndex;

    property    Modules[ LocationIndex, ModuleIndex : integer]: TG2FileModule read GetModule;
    property    ModuleCount[ LocationIndex : integer]: integer read GetModuleCount;
    property    EditAllVariations : boolean read FEditAllVariations write FEditAllVariations;

    property    PatchPart[ Index : integer]: TG2FilePatchPart read GetPatchPart;
    property    ModuleList[ Index : integer]: TModuleList read GetModuleList;
    property    CableList[ Index : integer]: TCableList read GetCableList;
    property    ParameterList[ Index : integer]: TModuleParameterList read GetParameterList;
    property    ModuleLabelList[ Index : integer]: TModuleLabelList read GetModuleLabelList;
    property    ParameterLabelList[ Index : integer]: TParameterLabelList read GetParameterLabelList;
    property    PatchSettings : TPatchSettings read FPatchSettings;
    property    KnobList : TKnobList read FKnobList;
    property    PatchDescription : TPatchDescription read FPatchDescription;
    property    PatchNotes : TPatchNotes read FPatchNotes;
    property    CurrentNoteList : TCurrentNoteList read FCurrentNoteList;

    property    SelectedLocation : TLocationType read FSelectedLocation write SetSelectedLocation;
    property    SelectedParamPage : integer read FSelectedParamPage write FSelectedParamPage;
    property    SelectedParamPageColumn : integer read FSelectedParamPageColumn write FSelectedParamPageColumn;

    property    LedList : TList<IG2DataLed> read FLedList;
    property    LedStripList : TList<IG2DataLed> read FLedStripList;

    property    G2 : TG2File read FG2 write SetG2;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
    property    Slot : TG2FileSlot read FSlot write SetSlot;
    property    OnLed : TLedEvent read FOnLedEvent write FOnLedEvent;
  end;

  // Interface class between file and application

  TG2FileConnector = class
  private
    [Weak] FPatch      : TG2FilePatch;
    [Weak] FModule     : TG2FileModule;

    FLocation          : TLocationType;
    FConnectorKind     : TConnectorKind;
    FConnectorType     : TConnectorType;
    FBandWidth         : TBandWidthType;
    FConnectorDefColor : Byte;
    FConnectorIndex    : Byte;
    FName              : string;

    FCables            : TList<TG2FileCable>;

    procedure   SetConnectorKind( aValue : TConnectorKind);
    function    GetConnectorColor : byte;
    function    GetNewConnectorColor: byte;
    function    GetCable( aIndex : integer) : TG2FileCable;
    function    GetCableCount : integer;
    function    GetDefRate: TBits1;
    function    GetRate: TBits1;
    function    GetNewRate: TBits1;
  protected
    procedure   SetConnectorDefColor( aValue : Byte); virtual;
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModule : TG2FileModule); reintroduce; virtual;
    destructor  Destroy; override;
    procedure   InitConnector( aModuleType, aConnectorIndex : Byte; aConnectorKind : TConnectorKind);
    procedure   AddCable( aCable : TG2FileCable);
    procedure   DelCable( aCable : TG2FileCable);
    procedure   InvalidateCables;
    procedure   InvalidateControl; virtual;
    procedure   CalcDefColor;

    property    Module : TG2FileModule read FModule;
    property    Location : TLocationType read FLocation write FLocation;
    property    ConnectorKind : TConnectorKind read FConnectorKind write SetConnectorKind;
    property    ConnectorColor : byte read GetConnectorColor;
    property    ConnectorDefColor : byte read FConnectorDefColor write SetConnectorDefColor;
    property    ConnectorIndex : byte read FConnectorIndex write FConnectorIndex;
    property    ConnectorType : TConnectorType read FConnectorType write FConnectorType;
    property    BandWidth : TBandWidthType read FBandWidth write FBandWidth;
    property    Name : string read FName write FName;
    property    Cables[ Index : integer]: TG2FileCable read GetCable;
    property    CableCount : integer read GetCableCount;
    property    NewConnectorColor : Byte read GetNewConnectorColor;
    property    DefRate : TBits1 read GetDefRate;
    property    Rate : TBits1 read GetRate;
    property    NewRate : TBits1 read GetNewRate;
  end;

  TParamRef = record
    ParamType : TParamType;
    ParamIndex : byte;
  end;

  // Interface class between file and application

  TG2FileParameter = class(TComponent, IG2DataParam)
  private
    [Weak] FPatch      : TG2FilePatch;
    [Weak] FModule     : TG2FileModule;
    [Weak] FKnob       : TKnob;
    [Weak] FGlobalKnob : TGlobalKnob;
    [Weak] FController : TController;

    FObserverList      : TList<IG2Observer>;

    FLocation          : TLocationType;
    FModuleIndex       : integer;
    FID                : integer;
    FParamType         : TParamType;
    FParamIndex        : integer;
    FLowValue          : byte;
    FHighValue         : byte;
    FDefaultValue      : byte;
    FParamName         : string;
    FModuleName        : string;
    FDefaultParamLabel : string;
    FCanChangeLabel    : boolean;
    FDefaultKnob       : integer; // The knob nr, when a module is assigned to a parameter page
    FButtonParamIndex  : integer; // The parameter that is assigned to the button below the knob on the param page
    FButtonText        : TStrings;
    FInfoFunctionIndex : integer;
    FTextFunctionIndex : integer;
    FTextDependencieArray  : array of TParamRef;
    FGraphDependencieArray : array of TParamRef;

    FSuspendUpdate     : boolean; // Prevent large amount of updates for none-responseless updates (virtual params: Masterclock/Voices)
    FSuspendedValue    : byte; // Value to show when suspended

    function    GetLabelIndex: integer;
    function    GetLabelOnValue: boolean;
    function    GetMorphAssignmentsAllowed: boolean;
    function    GetKnobAssignmentsAllowed: boolean;
    function    GetMidiAssignmentsAllowed: boolean;
  protected
    procedure   SetSelected( const aValue : boolean); virtual;
    function    GetSelected : boolean;
    function    GetButtonText( Index : integer): string;
    procedure   SetButtonText( Index : integer; aValue : string);
    function    GetSelectedButtonText: string;
    function    GetButtonTextCount : integer;
    function    GetButtonParam : TG2FileParameter;
    procedure   SetSuspendUpdate( aValue : boolean);
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    function    GetValueText( aIndex : integer): string;
  public
    constructor Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule); reintroduce;
    destructor  Destroy; override;

    procedure   RegisterObserver( aObserver : IG2Observer);
    procedure   RemoveObserver( aObserver : IG2Observer);
    procedure   NotifyObservers( aG2Event : TG2Event);

    procedure   InitParam( aID : integer; aModuleName : string; aParamIndex : byte; aParamType : TParamType;
                           aParamName, aDefaultParamLabel : string;
                           aLowValue, aHighValue, aDefaultValue : byte;
                           aDefaultKnob, aButtonParamIndex : integer; aButtonText : string);

    procedure   AssignKnob( aKnobIndex : integer);
    procedure   DeassignKnob( aKnobIndex : integer);
    procedure   AssignGlobalKnob( aPerf : TG2FilePerformance; aSlotIndex : byte; aKnobIndex : integer);
    procedure   DeassignGlobalKnob(  aPerf : TG2FilePerformance; aKnobIndex: integer);
    procedure   AssignController( aMidiCC : byte);
    procedure   DeassignController;
    procedure   DeassignControl( const aControl : IG2ParamObserver); virtual;

    procedure   SetValue( const aValue : byte); virtual;
    function    GetValue: byte; virtual;
    procedure   IncValue;
    procedure   DecValue;
    procedure   IncMorphValue;
    procedure   DecMorphValue;

    function    GetParameterLabelCount: integer;
    function    GetParamLabel( aIndex : integer): string;
    procedure   SetParamLabel( aIndex : integer; aValue : string);
    function    GetSelectedMorph : TMorphParameter;
    function    GetMorph( aMorphIndex, aVariation : integer) : TMorphParameter;
    function    HasMorph: boolean;
    function    GetSelectedMorphValue : byte;
    function    GetMorphValue( const aMorphIndex, aVariation : integer) : byte;
    procedure   SetMorphValue( const aMorphIndex, aVariation : integer; const aValue : byte);
    procedure   SetSelectedMorphValue( const Value: byte);
    procedure   AddTextDependency( aParamType : TParamType; aParamIndex : byte);
    function    GetTextDependendParamValue( aIndex : integer) : byte;
    procedure   AddGraphDependency( aParamType : TParamType; aParamIndex : byte);
    function    GetGraphDependendParamValue( aIndex : integer) : byte;
    //function    FreqDispValue( aMode : integer; aFreqCourse : integer; aFreqFine : integer): string;
    function    DelayDispValue( aType : integer; aRange : integer; aValue : integer): string;
    function    InfoFunction( aIndex : integer): string;
    function    TextFunction: string;
    procedure   InvalidateControl; virtual;

    property    ParamID : integer read FID write FID;
    property    ParamType : TParamType read FParamType write FParamType;
    property    ParamIndex : integer read FParamIndex write FParamIndex;
    property    LowValue : byte read GetLowValue write FLowValue;
    property    HighValue : byte read GetHighValue write FHighValue;
    property    DefaultValue : byte read FDefaultValue write FDefaultValue;
    property    Location : TLocationType read FLocation write FLocation;
    property    ModuleIndex : integer read FModuleIndex write FModuleIndex;
    property    ParamName : string read FParamName write FParamName;
    property    ModuleName : string read FModuleName write FModuleName;
    property    LabelIndex : integer read GetLabelIndex;
    property    ParamLabel[ index : integer] : string read GetParamLabel write SetParamLabel;
    property    ParamLabelCount : integer read GetParameterLabelCount;
    property    CanChangeLabel : boolean read FCanChangeLabel write FCanChangeLabel;
    property    LabelOnValue : boolean read GetLabelOnValue; // Switches for example
    property    Selected : boolean read GetSelected write SetSelected;
    property    Knob : TKnob read FKnob write FKnob;
    property    GlobalKnob : TGlobalKnob read FGlobalKnob write FGlobalKnob;
    property    Controller : TController read FController write FController;
    property    InfoFunctionIndex : integer read FInfoFunctionIndex write FInfoFunctionIndex;
    property    TextFunctionIndex : integer read FTextFunctionIndex write FTextFunctionIndex;
    property    ButtonParamIndex : integer read FButtonParamIndex write FButtonParamIndex;
    property    DefaultKnob : integer read FDefaultKnob write FDefaultKnob;
    property    ButtonTextList : TStrings read FButtonText;
    property    ButtonText[ index : integer]: string read GetButtonText write SetButtonText;
    property    ButtonTextCount : integer read GetButtonTextCount;
    property    SelectedButtonText: string read GetSelectedButtonText;
    property    ButtonParam : TG2FileParameter read GetButtonParam;
    property    Patch : TG2FilePatch read FPatch;
    property    Module : TG2FileModule read FModule;
    property    MorphAssignmentsAllowed : boolean read GetMorphAssignmentsAllowed;
    property    KnobAssignmentsAllowed : boolean read GetKnobAssignmentsAllowed;
    property    MidiAssignmentsAllowed : boolean read GetMidiAssignmentsAllowed;

    property    SuspendUpdate : boolean read FSuspendUpdate write SetSuspendUpdate;
  end;

  TG2FileLed = class(TInterfacedObject, IG2DataLed)
  private
    FObserverList : TList<IG2Observer>;
    FLocation : TLocationType;
    FModuleIndex : byte;
    FLedType : TLedType;
    FGroupID : byte;
    FGroupCount : integer;
    FValue : byte;
  protected
    function GetValue : byte;
    function GetHighValue : byte;
    function GetLowValue : byte;
    function GetLocation : TLocationType;
    function GetGroupID : byte;
    function GetModuleIndex : byte;
    function GetLedType : TLedType;
    function GetGroupCount : integer;
    function HasMorph: boolean;
    function GetSelectedMorphValue: byte;
    function GetValueText( aIndex : integer): string;
    procedure SetValue( const aValue : byte); virtual;
    procedure DeassignControl( const aControl : IG2ParamObserver);
  public
    constructor Create( const aLedType : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID : byte; const aGroupCount : integer);
    destructor Destroy; override;

    procedure RegisterObserver( aObserver : IG2Observer);
    procedure RemoveObserver( aObserver : IG2Observer);
    procedure NotifyObservers( aG2Event : TG2Event);
  end;

  TG2FileSlot = class( TComponent, IG2Subject)
  private
    [Weak] FPatch       : TG2FilePatch;
    [Weak] FPerformance : TG2FilePerformance;
    [Weak] FG2          : TG2File;

    FObserverList       : TList<IG2Observer>;

    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4          : TBits8; // array[0..4] of
    FEnabled           : TBits8;
    FKeyboard          : TBits8;
    FHold              : TBits8;
    FKeyboardRangeFrom : TBits8;
    FKeyboardRangeTo   : TBits8;
    FSlot              : TBits8;
    FBankIndex         : TBits8;
    FPatchIndex        : TBits8;

    FPatchName         : string;

    function    GetSlotIndex: byte;
    procedure   SetSlotIndex( aSlotIndex : byte);
    procedure   SetEnabled( aValue : TBits8);
    procedure   SetHold( aValue : TBits8);
    procedure   SetKeyboard( aValue : TBits8);
    procedure   SetKeyboardRangeTo( aValue : TBits8);
    procedure   SetKeyboardRangeFrom( aValue : TBits8);
    procedure   SetG2( aValue : TG2File);

    procedure   SetPerformance( aValue : TG2FilePerformance);
    procedure   SetPatch( aValue : TG2FilePatch);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; virtual;

    procedure   RegisterObserver( aObserver : IG2Observer);
    procedure   RemoveObserver( aObserver : IG2Observer);
    procedure   NotifyObservers( aG2Event : TG2Event);

    procedure   Init; virtual;
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    GetPatch: TG2FilePatch;

    property    SlotIndex : byte read GetSlotIndex write SetSlotIndex;
    property    PatchName : string read FPatchname write FPatchname;
    property    Enabled : TBits8 read FEnabled write SetEnabled;
    property    Hold : TBits8 read FHold write SetHold;
    property    Keyboard : TBits8 read FKeyboard write SetKeyboard;
    property    BankIndex : byte read FBankIndex;
    property    PatchIndex : byte read FPatchIndex;
    property    Upper : TBits8 read FKeyboardRangeTo write SetKeyboardRangeTo;
    property    Lower : TBits8 read FKeyboardRangeFrom write SetKeyboardRangeFrom;
    property    Patch : TG2FilePatch read FPatch write SetPatch;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
    property    G2 : TG2File read FG2 write SetG2;
  end;

  TGlobalKnob = class( TKnob)
  private
    FSlotIndex  : TBits2;
  public
    procedure   Init; override;
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk); override;
    property    SlotIndex : TBits2 read FSlotIndex write FSlotIndex;
  end;

  TGlobalKnobList = class( TObjectList<TGlobalKnob>)
  private
    [Weak] FPerf : TG2FilePerformance;

    FKnobCount  : TBits16;

    function    GetGlobalKnob( aIndex : integer) : TGlobalKnob;
    procedure   SetGlobalKnob( aIndex : integer; const aValue : TGlobalKnob);
  public
    constructor Create( AOwnsObjects : boolean; aPerf : TG2FilePerformance);
    destructor  Destroy; override;
    procedure   Init;
    function    FindGlobalKnobIndex( aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
    procedure   DeleteModule( aSlotIndex, aLocation, aModuleIndex : Byte);
    procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);
    function    AddGlobalKnob( aValue : TGlobalKnob): integer;
    property    Items[ aIndex : integer]: TGlobalKnob read GetGlobalKnob write SetGlobalKnob; default;
  end;

  TG2FilePerformance = class( TG2FileDataStream, IG2Subject)
  private
    [Weak] FG2            : TG2File;

    FObserverList       : TList<IG2Observer>;

    FPerformanceName      : string;
    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4,
    FUnknown5,
    FUnknown6,
    FUnknown7,
    FUnknown8,
    FUnknown9             : TBits8; //array[0..8] of
    FSelectedSlot         : TBits2;
    FKeyboardRangeEnabled : TBits8;
    FMasterClock          : TBits8;
    FMasterClockRun       : TBits8;

    FSlotArray            : array of TG2FileSlot; // [0..NSLOTS-1]

    FGlobalKnobList       : TGlobalKnobList;

    // Probably somewhere in the unknown bytes
    FSelectedParamPage  : integer;
    FSelectedParamPageColumn : integer;

  protected
    procedure   DoSelectSlot;
    procedure   SetG2( aValue : TG2File);
    procedure   SetPerformanceName( aValue : string);
    procedure   SetKeyboardRangeEnabled( aValue : TBits8);
    function    GetSlot( aSlot : byte): TG2FileSlot;
    procedure   InitSelectedSlotIndex( aValue : TBits2); virtual;
    procedure   SetSelectedSlotIndex( aValue : TBits2); virtual;
    procedure   SetMasterClock( aValue : TBits8); virtual;
    procedure   SetMasterClockRun( aValue : TBits8); virtual;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; virtual;

    procedure   RegisterObserver( aObserver : IG2Observer);
    procedure   RemoveObserver( aObserver : IG2Observer);
    procedure   NotifyObservers( aG2Event : TG2Event);

    procedure   Init;
    procedure   InitKnobs;
    procedure   WriteSettings( aChunk : TPatchChunk);
    procedure   Read( aChunk : TPatchChunk); override;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte); override;
    procedure   SaveToFile( aStream : TStream);
    function    LoadFromFile( aStream : TStream; aLogLines : TStrings): Boolean;
{$IFDEF MSWINDOWS}
    procedure   SaveAsFXB( aStream : TStream);
    function    LoadFromFXB( aStream : TStream): Boolean;
{$ENDIF}
    class function LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; override;

    function    GetNoOffExtendedModules : integer; // Number of modules that are not compatible with original editor

    procedure   DeleteModuleFromPerf( aSlotIndex : Byte; aLocation : TLocationType; aModule: TG2FileModule);
    function    AssignGlobalKnobInPerf( aKnobIndex : integer; aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TGlobalKnob;
    procedure   DeassignGlobalKnobInPerf( aKnobIndex : integer);
    function    GetGlobalKnob(KnobIndex: integer): TGlobalKnob;

    property    PerformanceName : string read FPerformanceName write SetPerformanceName;
    property    SelectedSlot : TBits2 read FSelectedSlot write SetSelectedSlotIndex;
    property    MasterClock : TBits8 read FMasterClock write SetMasterClock;
    property    MasterClockRun : TBits8 read FMasterClockRun write SetMasterClockRun;
    property    KeyboardRangeEnabled : TBits8 read FKeyboardRangeEnabled write SetKeyboardRangeEnabled;
    property    GlobalKnobList : TGlobalKnobList read FGlobalKnobList;
    property    SelectedParamPage : integer read FSelectedParamPage write FSelectedParamPage;
    property    SelectedParamPageColumn : integer read FSelectedParamPageColumn write FSelectedParamPageColumn;
    property    G2 : TG2File read FG2 write SetG2;
    property    Slot[ Index : byte]: TG2FileSlot read GetSlot;
  end;

  TBankItem = class(TComponent)
  public
    Cmd       : byte;
    PatchFileType : TPatchFileType;
    Bank      : byte;
    Patch     : byte;
    Category  : byte;
    PatchName : string;
  end;

  TBankList = class( TObjectList<TBankItem>)
  private
    [Weak] FG2 : TG2File;
    function    GetBankItem( aIndex : integer) : TBankItem;
    procedure   SetBankItem( aIndex : integer; const aValue : TBankItem);
  public
    constructor Create( AOwnsObjects : boolean; aG2 : TG2File);
    destructor  Destroy; override;
    function    FindIndex( aPatchFileType : TPatchFileType; aBank, aPatch : byte): integer;
    function    Find( aPatchFileType : TPatchFileType; aBank, aPatch : byte): TBankItem;
    function    FindNext( aPatchFileType : TPatchFileType; aBank, aPatch : byte): TBankItem;
    function    FindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;
    function    AddBankItem( aValue : TBankItem): integer;
    property    Items[ aIndex : integer]: TBankItem read GetBankItem write SetBankItem; default;
  end;

  TMidiToKnob = class
    FSource : TG2ParamSource; // 0=Param 1=Global 2=Morph 3=Patch
    FControlType : TG2ControlType; // 0=Knob 1=Button
    FKnob : integer;
    FMidiCC : integer;
  end;

  TG2File = class( TComponent, IG2Subject)
  private
    FClientType           : TClientType;

    FBankList             : TBankList;

    FObserverList         : TList<IG2Observer>;

    FOnCreateModule       : TCreateModuleEvent;
    FOnAddModule          : TAddModuleEvent;
    FOnDeleteModule       : TDeleteModuleEvent;
    FOnAddCable           : TAddCableEvent;
    FOnDeleteCable        : TDeleteCableEvent;
    FOnAssignKnob         : TAssignKnobEvent;
    FOnAssignGlobalKnob   : TAssignGlobalKnobEvent;
    FOnDeassignKnob       : TDeassignKnobEvent;
    FOnDeassignGlobalKnob : TDeassignGlobalKnobEvent;
    FOnParameterChange    : TParameterChangeEvent;
    FOnMorphChange        : TMorphChangeEvent;
    FOnLabelValueChange   : TLabelValueChangeEvent;
    FOnModuleModeChange   : TModuleModeChangeEvent;
    FOnSelectSlot         : TSelectSlotEvent;
    FOnSelectLocation     : TSelectLocationEvent;
    FOnSelectModule       : TSelectModuleEvent;
    FOnSelectParam        : TSelectParamEvent;
    FOnLogLine            : TLogLineEvent;

    FPerformance   : TG2FilePerformance;

    FAutoAssignMidi : boolean; // Automatically assigns midi on initialisation
    FMidiToKnobList : TObjectList<TMidiToKnob>;

    FLogLevel    : integer;
    FLogLines    : TStringList;

    FLogLock     : TCriticalSection; // for multi threaded access to log

    FLastError   : string;

    procedure SetPerformance( aValue : TG2FilePerformance);
    procedure SetLogLevel( aValue : integer);
    function  GetSelectedPatchPart : TG2FilePatchPart;
  protected
    function  GetID : integer; virtual;
  public
    // TODO make these private
    FslBanks       : TStringList;

    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   RegisterObserver( aObserver : IG2Observer);
    procedure   RemoveObserver( aObserver : IG2Observer);
    procedure   NotifyObservers( aG2Event : TG2Event);

    function    CreatePerformance : TG2FilePerformance; virtual;

    // For auto assign midi to knobs
    procedure   AddMidiToKnob( aSource : TG2ParamSource; aControlType : TG2ControlType; aKnob : integer; aMidiCC : byte);
    procedure   DeleteMidiToKnob( aSource : TG2ParamSource; aControlType : TG2ControlType; aKnob : integer);
    function    FindMidiToKnob( aSource : TG2ParamSource; aControlType : TG2ControlType; aKnob : integer): integer;
    function    MidiToKnobCheckCC( aMidiCC : byte): boolean;

    function    TextFunction( No : integer; Param1, Param2, Param3 : byte): string;

    procedure   add_log_line( tekst: string; log_cmd : integer);
    procedure   dump_buffer( var buffer; max_size : integer);
    procedure   SaveLog( const filename : string);
    procedure   AssignLog( Lines : TStrings);
    procedure   ClearLog;
    function    G2MessageDlg( tekst : string; code : integer): integer; virtual; // For sending messages to UI

    property    BankList : TBankList read FBankList write FBankList;
    property    Performance : TG2FilePerformance read FPerformance write SetPerformance;
    property    LogLines : TStringList read FLogLines;
    property    LastError : string read FLastError write FLastError;
    property    SelectedPatchPart : TG2FilePatchPart read GetSelectedPatchPart;

  published
    property    ID : integer read GetID;
    property    ClientType : TClientType read FClientType write FClientType;
    property    LogLevel : integer read FLogLevel write SetLogLevel;
    property    AutoAssignMidi : boolean read FAutoAssignMidi write FAutoAssignMidi;
    property    MidiToKnobList : TObjectList<TMidiToKnob> read FMidiToKnobList;
    property    OnCreateModule : TCreateModuleEvent read FOnCreateModule write FOnCreateModule;
    property    OnAddModule : TAddModuleEvent read FOnAddModule write FOnAddModule;
    property    OnDeleteModule : TDeleteModuleEvent read FOnDeleteModule write FOnDeleteModule;
    property    OnAddCable : TAddCableEvent read FOnAddCable write FOnAddCable;
    property    OnDeleteCable : TDeleteCableEvent read FOnDeleteCable write FOnDeleteCable;
    property    OnAssignKnob : TAssignKnobEvent read FOnAssignKnob write FOnAssignKnob;
    property    OnDeassignKnob : TDeassignKnobEvent read FOnDeassignKnob write FOnDeassignKnob;
    property    OnAssignGlobalKnob : TAssignGlobalKnobEvent read FOnAssignGlobalKnob write FOnAssignGlobalKnob;
    property    OnDeassignGlobalKnob : TDeassignGlobalKnobEvent read FOnDeassignGlobalKnob write FOnDeassignGlobalKnob;
    property    OnParameterChange : TParameterChangeEvent read FOnParameterChange write FOnParameterChange;
    property    OnMorphChange : TMorphChangeEvent read FOnMorphChange write FOnMorphChange;
    property    OnLabelValueChange : TLabelValueChangeEvent read FOnLabelValueChange write FOnLabelValueChange;
    property    OnModuleModeChange : TModuleModeChangeEvent read FOnModuleModeChange write FOnModuleModeChange;
    property    OnSelectSlot : TSelectSlotEvent read FOnSelectSlot write FOnSelectSlot;
    property    OnSelectLocation : TSelectLocationEvent read FOnSelectLocation write FOnSelectLocation;
    property    OnSelectModule : TSelectModuleEvent read FOnSelectModule write FOnSelectModule;
    property    OnSelectParam : TSelectParamEvent read FOnSelectParam write FOnSelectParam;
    property    OnLogLine : TLogLineEvent read FOnLogLine write FOnLogLine;
  end;

procedure AddHeaderInfo( aPatchFileType : TPatchFileType; aStream : TStream);

implementation

//------------------------------------------------------------------------------
//
//                              TG2FileModule
//
//------------------------------------------------------------------------------

constructor TG2FileModule.Create( aPatchPart : TG2FilePatchPart);
begin
  inherited Create(nil); // Owned by ModuleList

  FPatchPart := aPatchPart;

  SetLength(FModeInfoArray, 0);
  FSelectedParam := -1;
end;

function TG2FileModule.CreateCopy( aPatchPart : TG2FilePatchPart) : TG2FileModule;
begin
  Result := TG2FileModule.Create( nil);
  Result.Copy( self);
end;

procedure TG2FileModule.Copy( aModule : TG2FileModule);
var i : integer;
begin
  FTypeID         := aModule.FTypeID;
  FModuleIndex    := aModule.FModuleIndex;
  FModuleName     := aModule.FModuleName;
  //FModuleName     := aModule.PatchPart.GetUniqueModuleName(aModule.FModuleName);
  FModuleFileName := aModule.FModuleFileName;
  FCol            := aModule.Col;
  FRow            := aModule.Row;
  FModuleColor    := aModule.ModuleColor;
  FUprate         := aModule.Uprate;
  FIsLed          := aModule.IsLed;
  FUnknown1       := aModule.FUnknown1;
  FModeCount      := aModule.ModeCount;
  FHeightUnits    := aModule.HeightUnits;

  SetLength(FModeInfoArray, FModeCount);
  for i := 0 to Length(FModeInfoArray) - 1 do
    FModeInfoArray[i] := aModule.FModeInfoArray[i];
end;

destructor TG2FileModule.Destroy;
var i : integer;
begin
  if assigned(FPatchPart) then begin
    FPatchPart.RemoveModuleFromSelection( self);

    {if assigned(FPatchPart.FPatch) then
      FPatchPart.FPatch.DeleteModule( FPatchPart.FLocation, FModuleIndex)
    else
      FPatchPart.DeleteModule( FModuleIndex);}
  end;

  for i := 0 to Length(FModeArray) - 1 do
    FModeArray[i].DisposeOf;
  Finalize(FModeArray);

  for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].DisposeOf;
  Finalize(FParamArray);

  for i := 0 to Length(FOutConnArray) - 1 do
    FOutConnArray[i].Free;
  Finalize( FOutConnArray);

  for i := 0 to Length(FInConnArray) - 1 do
    FInConnArray[i].Free;
  Finalize( FInConnArray);

  Finalize(FModeInfoArray);

  inherited;
end;

procedure TG2FileModule.Init;
begin
  TypeID       := 0;
  ModuleIndex  := 0;
  Row          := 0;
  Col          := 0;
  MOduleColor  := 0;
  FUnknown1    := 0;
  FModeCount   := 0;
  SetLength(FModeInfoArray, 0);

  ModuleName := '';
  FModuleFileName := '';
end;

function TG2FileModule.CreateParameter: TG2FileParameter;
var Patch : TG2FilePatch;
begin
  Patch := nil;
  if assigned(PatchPart) then
    Patch := PatchPart.Patch;

  Result := TG2FileParameter.Create( Patch, TLocationType(FLocation), FModuleIndex, self);
end;

function TG2FileModule.CreateConnector: TG2FileConnector;
begin
  Result := TG2FileConnector.Create( FPatchPart.FPatch, TLocationType(FLocation), self);
end;

{procedure TG2FileModule.InitModule( aLocation : TLocationType; aModuleDef: IXMLModuleDefType; aParamDefList : IXMLParamDefListType);
var i : integer;
    dummy : integer;
    InputDefList : IXMLInputsType;
    ConnectorDef : IXMLConnectorType;
    OutputDefList : IXMLOutputsType;
    ParamDefList : IXMLParamsType;
    ParamDef : IXMLParamDefType;
    ModeDefList : IXMLModesType;
begin
  FLocation      := aLocation;
  FTypeID        := aModuleDef.ModuleType;
  FUprate        := aModuleDef.Uprate;
  FIsLed         := aModuleDef.IsLed;
  FHeightUnits   := aModuleDef.Height;
  ModuleName     := aModuleDef.ShortName;
  //ModuleName     := FPatchPart.GetUniqueModuleName(aModuleDef.ShortName);
  ModuleFileName := aModuleDef.ShortName;
  FModeCount     := 0;

  if aModuleDef.ModuleType = 64 then
    dummy := 1;

  FPage := aModuleDef.Page;
  FPageIndex := aModuleDef.PageIndex;

  InputDefList := aModuleDef.Inputs;
  SetLength( FInConnArray, InputDefList.Count);
  for i := 0 to InputDefList.Count - 1 do begin
    FInConnArray[i] := CreateConnector;
    FInConnArray[i].InitConnector( i, ckInput, InputDefList.Connector[i]);
  end;

  OutputDefList := aModuleDef.Outputs;
  SetLength( FOutConnArray, aModuleDef.Outputs.Count);
  for i := 0 to OutputDefList.Count - 1 do begin
    FOutConnArray[i] := CreateConnector;
    FOutConnArray[i].InitConnector( i, ckOutput, OutputDefList.Connector[i]);
  end;

  ParamDefList := aModuleDef.Params;
  SetLength( FParamArray, ParamDefList.Count);
  for i := 0 to ParamDefList.Count - 1 do begin
    FParamArray[i] := CreateParameter;
    ParamDef := aParamDefList.ParamDef[ aModuleDef.Params[i].Id];
    FParamArray[i].InitParam( aModuleDef.Params[i].Id, aModuleDef.ShortName, i, ptParam, aModuleDef.Params[i].Name, aModuleDef.Params[i].ParamLabel,
                          ParamDef.LowValue, ParamDef.HighValue, aModuleDef.Params[i].DefaultValue, //ParamDef.DefaultValue,
                          aModuleDef.Params[i].DefaultKnob, aModuleDef.Params[i].ButtonParam,
                          ParamDef.ButtonText);
  end;

  ModeDefList := aModuleDef.Modes;
  FModeCount  := aModuleDef.Modes.Count;
  SetLength( FModeInfoArray, FModeCount);
  SetLength( FModeArray, ModeDefList.Count);
  for i := 0 to ModeDefList.Count - 1 do begin
    FModeArray[i] := CreateParameter;
    ParamDef := aParamDefList.ParamDef[ aModuleDef.Modes[i].Id];
    FModeArray[i].InitParam( aModuleDef.Modes[i].Id, aModuleDef.ShortName, i, ptMode, aModuleDef.Modes[i].Name, aModuleDef.Modes[i].ParamLabel,
                         ParamDef.LowValue, ParamDef.HighValue, aModuleDef.Modes[i].DefaultValue, // ParamDef.DefaultValue,
                         -1, -1,
                          ParamDef.ButtonText);
    FModeInfoArray[i] := ParamDef.DefaultValue;
  end;
end;
}

procedure TG2FileModule.InitModule( aLocation : TLocationType; aModuleType : integer);
var i, mi, pi, pdi, count : integer;
begin
  mi := GetDataModuleIndex(aModuleType);

  FLocation      := aLocation;
  FTypeID        := aModuleType;
  FUprate        := ModuleDefs[mi].Uprate;
  FIsLed         := ModuleDefs[mi].IsLed;
  FHeightUnits   := ModuleDefs[mi].Height;
  ModuleName     := ModuleDefs[mi].ModuleName;
  //ModuleName     := FPatchPart.GetUniqueModuleName(aModuleDef.ShortName);
  ModuleFileName := ModuleDefs[mi].ModuleName;
  FModeCount     := 0;

  FPage := ModuleDefs[mi].Page;
  FPageIndex := ModuleDefs[mi].PageIndex;

  count := GetDataInputCount(aModuleType);
  SetLength( FInConnArray, count);
  for i := 0 to count - 1 do begin
    FInConnArray[i] := CreateConnector;
    FInConnArray[i].InitConnector( aModuleType, i, ckInput);
  end;

  count := GetDataOutputCount(aModuleType);
  SetLength( FOutConnArray, Count);
  for i := 0 to count - 1 do begin
    FOutConnArray[i] := CreateConnector;
    FOutConnArray[i].InitConnector( aModuleType, i, ckOutput);
  end;

  count := GetDataParamCount(aModuleType);
  SetLength( FParamArray, count);
  for i := 0 to count - 1 do begin
    pi := GetDataModuleParamIndex( aModuleType, i);
    pdi := GetDataParamDefIndex( ModuleParams[pi].ParamID);

    FParamArray[i] := CreateParameter;
    FParamArray[i].InitParam( ModuleParams[pi].ParamID,
                              ModuleDefs[mi].ModuleName,
                              i,
                              ptParam,
                              ModuleParams[pi].ParamName,
                              ModuleParams[pi].slParamLabel,
                              ParamDefs[pdi].LowValue,
                              ParamDefs[pdi].HighValue,
                              ModuleParams[pi].DefaultValue,
                              ModuleParams[pi].DefaultKnob,
                              ModuleParams[pi].ButtonParam,
                              ParamDefs[pdi].slButtonText);
  end;
  if count > 0 then
    FSelectedParam := 0;

  count := GetDataModeCount(aModuleType);
  FModeCount  := count;
  SetLength( FModeInfoArray, count);
  SetLength( FModeArray, count);
  for i := 0 to count - 1 do begin
    FModeArray[i] := CreateParameter;
    pi := GetDataModuleModeIndex( aModuleType, i);
    pdi := GetDataParamDefIndex( ModuleModes[pi].ParamID);

    FModeArray[i].InitParam( ModuleParams[pi].ParamID,
                             ModuleDefs[mi].ModuleName,
                             i,
                             ptMode,
                             ModuleParams[pi].ParamName,
                             ModuleParams[pi].slParamLabel,
                             ParamDefs[pdi].LowValue,
                             ParamDefs[pdi].HighValue,
                             ModuleParams[pi].DefaultValue,
                             -1,
                             -1,
                             ParamDefs[pdi].slButtonText);
    FModeInfoArray[i] := ModuleParams[pi].DefaultValue;
  end;
end;

procedure TG2FileModule.InvalidateCables;
var i : integer;
begin
  for i := 0 to Length(FInConnArray) - 1 do
    FInConnArray[i].InvalidateCables;

  for i := 0 to Length(FOutConnArray) - 1 do
    FOutConnArray[i].InvalidateCables;
end;

procedure TG2FileModule.InvalidateConnectors;
var i : integer;
begin
  for i := 0 to Length(FInConnArray) - 1 do
    FInConnArray[i].InvalidateControl;

  for i := 0 to Length(FOutConnArray) - 1 do
    FOutConnArray[i].InvalidateControl;
end;

procedure TG2FileModule.InvalidateControl;
begin
  // override to update an associated control
end;

procedure TG2FileModule.InvalidateParameters;
var i : integer;
begin
  for i := 0 to Length(FModeArray) - 1 do
    FModeArray[i].InvalidateControl;

  for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].InvalidateControl;
end;

function TG2FileModule.GetInputConnector( ConnectorIndex: integer): TG2FileConnector;
var i : integer;
begin
  i := 0;
  while ( i < Length(FInConnArray)) and ( ConnectorIndex <> FInConnArray[i].ConnectorIndex) do
    inc(i);

  if ( i < Length(FInConnArray)) then
    Result := FInConnArray[i]
  else
    raise Exception.Create('Input connector with index ' + IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetInputConnectorCount: integer;
begin
  Result := Length(FInConnArray);
end;

function TG2FileModule.GetOutputConnector( ConnectorIndex: integer): TG2FileConnector;
var i : integer;
begin
  i := 0;
  while ( i < Length(FOutConnArray)) and ( ConnectorIndex <> FOutConnArray[i].ConnectorIndex) do
    inc(i);

  if ( i < Length(FOutConnArray)) then
    Result := FOutConnArray[i]
  else
    raise Exception.Create('Output connector with index ' + IntToStr(ConnectorIndex) + ' not found.');
end;

function TG2FileModule.GetOutputConnectorCount: integer;
begin
  Result := Length(FOutConnArray);
end;

function TG2FileModule.GetMode( ModeIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FModeArray)) and ( ModeIndex <> FModeArray[i].ParamIndex) do
    inc(i);

  if ( i < Length(FModeArray)) then
    Result := FModeArray[i]
  else
    //raise Exception.Create('Mode parameter with index ' + IntToStr(ModeIndex) + ' not found.');
    Result := nil;
end;

function TG2FileModule.GetParameter( ParamIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FParamArray)) and ( ParamIndex <> FParamArray[i].ParamIndex) do
    inc(i);

  if ( i < Length(FParamArray)) then
    Result := FParamArray[i]
  else
    //raise Exception.Create('Parameter with index ' + IntToStr(ParamIndex) + ' not found.');
    Result := nil;
end;

function TG2FileModule.GetModeCount: integer;
begin
  Result := Length( FModeInfoArray);
end;

function TG2FileModule.GetParameterCount: integer;
begin
  Result := Length( FParamArray);
end;

procedure TG2FileModule.SetModeInfo( aModeIndex : integer; aValue : TBits6);
begin
  if aModeIndex < Length(FModeInfoArray) then
    FModeInfoArray[aModeIndex] := aValue
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) + ' out of range.');
end;

function TG2FileModule.GetModeInfo( aModeIndex : integer): TBits6;
begin
  if aModeIndex < Length(FModeInfoArray) then
    Result := FModeInfoArray[aModeIndex]
  else
    raise Exception.Create('Set ModeInfo, index ' + IntToStr(aModeIndex) + ' out of range.');
end;

procedure TG2FileModule.SetNewCol( aValue : TBits7);
begin
  FNewCol := aValue;
end;

function TG2FileModule.GetNewCol : TBits7;
begin
  Result := FNewCol;
end;

procedure TG2FileModule.SetNewRow( aValue : TBits7);
begin
  FNewRow := aValue;
end;

function TG2FileModule.GetNewRow : TBits7;
begin
  Result := FNewRow;
end;

function TG2FileModule.GetRowIndex : integer;
var i : integer;
begin
  // Volgorde van module in de kolom, tel aantal modules in de kolom
  // met een kleinere row-waarde

  Result := 0;
  if assigned(FPatchPart) then begin
    for i := 0 to FPatchPart.ModuleList.Count - 1 do begin
      if (FPatchPart.ModuleList.Items[i].FCol = FCol) and (FPatchPart.ModuleList.Items[i].FRow < FRow) then
        inc(Result);
    end;
  end;
end;

function TG2FileModule.GetSelectedParam : TG2FileParameter;
begin
  if FSelectedParam = -1 then
    Result := nil
  else
    Result := FParamArray[ FSelectedParam];
end;

procedure TG2FileModule.SetSelectedParam( aValue : TG2FileParameter);
var i : integer;
begin
  if Length(FParamArray) = 0 then
    exit;

  i := 0;
  while (i < Length(FParamArray)) and (FParamArray[i] <> aValue) do
    inc(i);

  if (i < Length(FParamArray)) then begin
    //Selected := True; // Select the module
    FSelectedParam := i;
  end;
end;

function TG2FileModule.GetNextParam : TG2FileParameter;
begin
  Result := nil;
  if Length( FParamArray) = 0 then
    exit;

  inc(FSelectedParam);
  if FSelectedParam >= Length(FParamArray) then
     FSelectedParam := 0;

  Result := FParamArray[FSelectedParam];
end;

function TG2FileModule.GetPrevParam : TG2FileParameter;
begin
  Result := nil;
  if Length( FParamArray) = 0 then
    exit;

  dec(FSelectedParam);
  if FSelectedParam < 0  then
     FSelectedParam := Length(FParamArray) - 1;

  Result := FParamArray[FSelectedParam];
end;

procedure TG2FileModule.SelectNextParam;
var Param : TG2FileParameter;
begin
  if FSelectedParam <> -1 then
    FParamArray[FSelectedParam].Selected := False;
  Param := GetNextParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParam := -1;
end;

procedure TG2FileModule.SelectPrevParam;
var Param : TG2FileParameter;
begin
  if FSelectedParam <> -1 then
    FParamArray[FSelectedParam].Selected := False;
  Param := GetPrevParam;
  if assigned(Param) then
    Param.Selected := True
  else
    FSelectedParam := -1;
end;

function TG2FileModule.CableCount : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to InConnectorCount - 1 do
    Result := Result + InConnector[i].CableCount;
  for i := 0 to OutConnectorCount - 1 do
    Result := Result + OutConnector[i].CableCount;
end;

procedure TG2FileModule.Read( aChunk : TPatchChunk);
var i : integer;
begin
  // FModuleIndex : TBits8;
  // FRow         : TBits7;
  // FCol         : TBits7;
  // FColor       : TBits8;
  // FUnknown1    : TBits8;
  // FUnknown2    : TBits4; => Seems to be the ModeCount
  // FModeInfoArray    : array of TBits6;

  //FTypeID      := Chunk.ReadBits(aStream, 8);
  //ModuleIndex  := aChunk.ReadBits( 8);
  Col          := aChunk.ReadBits( 7);
  Row          := aChunk.ReadBits( 7);
  ModuleColor  := aChunk.ReadBits( 8);
  Uprate       := aChunk.ReadBits( 1);
  IsLed        := aChunk.ReadBits( 1);
  FUnknown1    := aChunk.ReadBits( 6);
  FModeCount   := aChunk.ReadBits( 4);

  if assigned(aChunk.FLogLines) then
    aChunk.FLogLines.Add(Format('%3d %3d %3d %3d %3d %3d %3d %3d %3d', [TypeID, ModuleIndex, Row, Col, ModuleColor, Uprate, IsLed, FUnknown1, FModeCount]));

  SetLength(FModeInfoArray, FModeCount);
  for i := 0 to Length(FModeInfoArray) - 1 do
    FModeInfoArray[i] := aChunk.ReadBits( 6);
end;

procedure TG2FileModule.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(TypeID, 8);
  aChunk.WriteBits(ModuleIndex, 8);
  aChunk.WriteBits(Col, 7);
  aChunk.WriteBits(Row, 7);
  aChunk.WriteBits(ModuleColor, 8);
  aChunk.WriteBits(Uprate, 1);
  aChunk.WriteBits(IsLed, 1);
  aChunk.WriteBits(FUnknown1, 6);
  aChunk.WriteBits(FModeCount, 4);

  for i := 0 to Length(FModeInfoArray) - 1 do
    aChunk.WriteBits(FModeInfoArray[i], 6);
end;


procedure TG2FileModule.SetCol(Value: TBits7);
begin
  FCol := Value;
end;

procedure TG2FileModule.SetLocation(const aValue: TLocationType);
begin
  FLocation := aValue;
end;

procedure TG2FileModule.SetRow(Value: TBits7);
begin
  FRow := Value;
end;

procedure TG2FileModule.SetModuleColor(Value: TBits8);
begin
  if FModuleColor <> Value then begin
    FModuleColor := Value;
    InvalidateControl;
  end;
end;

procedure TG2FileModule.SetModuleIndex(const aValue: TBits8);
begin
  FModuleIndex := aValue;
end;

procedure TG2FileModule.SetUprate(Value: TBits1);
begin
  if FUprate <> Value then begin
    FUprate := Value;
    InvalidateConnectors;
  end;
end;

function TG2FileModule.GetSelected: boolean;
begin
  Result := False;
  if not assigned(FPatchPart) then
    exit;

  Result := FPatchPart.SelectedModuleList.FindModule( FModuleIndex) <> nil;
end;

{procedure TG2FileModule.SetSelected( aValue: Boolean);
begin
  if aValue <> Selected then begin

    // The patch keeps a list of selected modules
    if assigned(FPatchPart) then
      if aValue then begin
        FPatchPart.AddModuleToSelection( self);
    //    if assigned(FPatchPart.Patch) then
    //      FPatchPart.Patch.DoFocusModule;
      end else
        FPatchPart.RemoveModuleFromSelection( self);
  end;
end;}

procedure TG2FileModule.SelectModule(const aValue, aExclusive : boolean);
begin
  if aValue <> Selected then begin

    // The patch keeps a list of selected modules
    if assigned(FPatchPart) then
      if aValue then begin
        if aExclusive then
          FPatchPart.UnselectModules;
        FPatchPart.AddModuleToSelection( self);
      end else
        FPatchPart.RemoveModuleFromSelection( self);
  end;
end;

procedure TG2FileModule.SetModuleName( aValue: string);
begin
  FModuleName := aValue;
//FMX  Caption := FModuleName;
//  Invalidate;
end;

function TG2FileModule.GetAssignableKnobCount: integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Length(FParamArray) - 1 do
    if FParamArray[i].FDefaultKnob >= 0 then
      inc(Result);
end;

function TG2FileModule.GetColor: integer;
begin
  if FModuleColor <= 24 then
    Result := ModuleColors[ FModuleColor]
  else
    Result := ModuleColors[ 0];
end;

//------------------------------------------------------------------------------
//
//                              TG2ModuleList
//
//------------------------------------------------------------------------------

constructor TModuleList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

constructor TModuleList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList);
var Module : TG2FileModule;
    i : integer;
begin
  inherited Create( AOwnsObjects);

  FPatchPart := aPatchPart;
  FModuleCount := aModuleList.GetSelectedCount;

  for i := 0 to aModuleList.Count - 1 do begin
    Module := aModuleList.Items[i];
    if assigned(Module) {and (Module.FSelected)} then begin
      Add(Module.CreateCopy( aPatchPart));
    end;
  end;
end;

function TModuleList.GetModule( aIndex : integer) : TG2FileModule;
begin
  result := TG2FileModule(inherited Items[aindex]);
end;

procedure TModuleList.SetModule( aIndex : integer; const aValue : TG2FileModule);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleList.AddModule( aValue : TG2FileModule): integer;
begin
  Result := inherited Add( aValue);
end;

function TModuleList.GetSelectedCount: integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].Selected then
      inc(Result);
end;

function TModuleList.GetUnitsRect: TRect;
var i : integer;
begin
  if Count = 0 then begin
    Result := Rect(0,0,0,0);
  end else begin
    Result := Rect(10000, 10000, -10000, -10000);
    for i := 0 to Count - 1 do begin
      if Items[i].Row < Result.Top then
        Result.Top := Items[i].Row;
      if Items[i].Row + Items[i].HeightUnits > Result.Bottom then
        Result.Bottom := Items[i].Row + Items[i].HeightUnits;
      if Items[i].Col < Result.Left then
        Result.Left := Items[i].Col;
      if Items[i].Col + 1 > Result.Right then
        Result.Right := Items[i].Col + 1;
    end;
  end;
end;

destructor TModuleList.Destroy;
begin
  inherited;
end;

procedure TModuleList.Init;
begin
  Clear;

  FLocation       := 0;
  FModuleCount    := 0;
end;

function TModuleList.ModuleAtLocation(const aRow, aCol: integer): TG2FileModule;
var i : integer;
begin
  Result := nil;

  i := 0;
  while (i<Count) and not((Items[i].Col = aCol) and (aRow >= Items[i].Row) and (aRow < (Items[i].Row + Items[i].HeightUnits))) do
    inc(i);
  if (i<Count) then
    Result := Items[i];
end;

function TModuleList.FindModule( aModuleIndex : integer) : TG2FileModule;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TModuleList.DeleteModule( aModuleIndex : byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
     Delete(i);
  end;
end;

function TModuleList.GetModuleAbove( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Row < Module.Row then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleUnder( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Row > Module.Row then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleLeft( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Col < Module.Col then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

function TModuleList.GetModuleRight( aModuleIndex : byte) : TG2FileModule;
var Module : TG2FileModule;
    i : integer;
    dx, dy, ds : single;
begin
  Module := FindModule(aModuleIndex);
  Result := Module;
  if not(assigned(Result)) then
    exit;

  ds := 0;
  for i := 0 to Count - 1 do begin
    if Items[ i].ParameterCount > 0 then begin
      if Items[ i].Col > Module.Col then begin
        dx := Items[i].Col * UNITS_COL - Module.Col * UNITS_COL;
        dy := Items[i].Row * UNITS_ROW - Module.Row * UNITS_ROW;
        if (ds = 0) or (sqrt(dx*dx+dy*dy) < ds) then begin
          Result := Items[i];
          ds := sqrt(dx*dx+dy*dy);
        end;
      end;
    end;
  end;
end;

procedure TModuleList.Read( aChunk : TPatchChunk);
var i, j : integer;
    ModuleIndex, ModuleType : byte;
    Module : TG2FileModule;
begin
  // FLocation       : TBits2;
  // FModuleCount    : TBits8;
  // FModules        : array of TModule;

  FModuleCount    := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module list, count = ' + IntToStr(FModuleCount));
    aChunk.FLogLines.Add('Typ Idx Row Col Clr Upr Isl Un1 Mde');
  end;

  for i := 0 to FModuleCount - 1 do begin
    ModuleType  := aChunk.ReadBits( 8);
    ModuleIndex := aChunk.ReadBits( 8);
    j := 0;
    while (j<Count) and not(Items[j].ModuleIndex = ModuleIndex) do
      inc(j);

    if j<Count then begin
      Module := Items[j]
    end else begin
      Module := FPatchPart.CreateModule( ModuleIndex, ModuleType);
      Add(Module);
    end;
    Module.Read( aChunk);
  end;
end;

procedure TModuleList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FModuleCount := Count;
  aChunk.WriteBits(FModuleCount, 8);
  for i := 0 to Count - 1 do begin
    Items[i].Write( aChunk);
  end;
end;

function TModuleList.GetMaxModuleIndex : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].ModuleIndex > Result then
      Result := Items[i].ModuleIndex;
end;

function TModuleList.GetNoOffModuleType( aModuleType : byte) : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].TypeID = aModuleType then
      Result := Result + 1;
end;

function TModuleList.GetNoOffExtendedModules : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    if Items[i].TypeID in  EXTENDED_MODULE_IDS then
      Result := Result + 1;
end;

//------------------------------------------------------------------------------
//
//                              TG2FileCable
//
//------------------------------------------------------------------------------

constructor TG2FileCable.Create( aPatchPart: TG2FilePatchPart);
begin
  inherited Create( nil); // Owned by cablelist

  Init;
end;

constructor TG2FileCable.CopyCreate( aPatchPart : TG2FilePatchPart; aCable : TG2FileCable);
begin
  inherited Create( nil);

  FCableColor    := aCable.FCableColor;
  FModuleFrom    := aCable.FModuleFrom;
  FConnectorFrom := aCable.FConnectorFrom;
  FLinkType      := aCable.FLinkType;
  FModuleTo      := aCable.FModuleTo;
  FConnectorTo   := aCable.FConnectorTo;
end;

destructor TG2FileCable.Destroy;
begin
  inherited;
end;

procedure TG2FileCable.Init;
begin
  FCableColor    := 0;
  FModuleFrom    := 0;
  FConnectorFrom := 0;
  FLinkType      := 0;
  FModuleTo      := 0;
  FConnectorTo   := 0;
end;

procedure TG2FileCable.Read( aChunk : TPatchChunk);
begin
 // FColor         : TBits3;
 // FModuleFrom    : TBits8;
 // FConnectorFrom : TBits6;
 // FLinkType      : TBits1;
 // FModuleTo      : TBits8;
 // FConnectorTo   : TBits6;

 FCableColor    := aChunk.ReadBits( 3);
 FModuleFrom    := aChunk.ReadBits( 8);
 FConnectorFrom := aChunk.ReadBits( 6);
 FLinkType      := aChunk.ReadBits( 1);
 FModuleTo      := aChunk.ReadBits( 8);
 FConnectorTo   := aChunk.ReadBits( 6);
end;

procedure TG2FileCable.Invalidate;
begin
  // abstract
end;

procedure TG2FileCable.SetCableColor(Value: byte);
begin
  if FCableColor <> Value then begin
    FCableColor := Value;
    Invalidate;
  end;
end;

procedure TG2FileCable.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits( FCableColor, 3);
  aChunk.WriteBits( FModuleFrom, 8);
  aChunk.WriteBits( FConnectorFrom, 6);
  aChunk.WriteBits( FLinkType, 1);
  aChunk.WriteBits( FModuleTo, 8);
  aChunk.WriteBits( FConnectorTo, 6);
end;

procedure TG2FileCable.ConnectorMoved;
begin
  // Abstract
end;

//------------------------------------------------------------------------------
//
//                              TG2CableList
//
//------------------------------------------------------------------------------

constructor TCableList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);

  Init;
  FPatchPart := aPatchPart;
end;

constructor TCableList.CopySelected(  AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aCableList : TCableList);
var i : integer;
    ModuleFrom, ModuleTo : TG2FileModule;
    NumCables : integer;
begin
  inherited Create( AOwnsObjects);

  Init;
  FPatchPart := aPatchPart;

  FLocation   := aCableList.FLocation;
  FUnknown    := aCableList.FUnknown;
  NumCables   := aCableList.Count;

  for i := 0 to NumCables - 1 do begin
    ModuleFrom := aModuleList.FindModule( aCableList.Items[i].FModuleFrom);
    ModuleTo := aModuleList.FindModule( aCableList.Items[i].FModuleTo);

    if assigned(ModuleFrom) and assigned(ModuleTo) {and (ModuleFrom.FSelected) and (ModuleTo.FSelected)} then begin
      Add(TG2FileCable.CopyCreate(nil, aCableList.Items[i]));
    end;
  end;
end;

destructor TCableList.Destroy;
begin
  inherited;
end;

function TCableList.GetCable( aIndex : integer) : TG2FileCable;
begin
  result := TG2FileCable(inherited Items[aindex]);
end;

procedure TCableList.SetCable( aIndex : integer; const aValue : TG2FileCable);
begin
  inherited Items[aindex] := aValue;
end;

function TCableList.AddCable( aValue : TG2FileCable): integer;
var ModuleFrom, ModuleTo : TG2FileModule;
begin
  Result := inherited Add( aValue);

  ModuleFrom := FPatchPart.ModuleList.FindModule( aValue.FModuleFrom);
  ModuleTo := FPatchPart.ModuleList.FindModule( aValue.FModuleTo);

  if assigned(ModuleFrom) then begin
    ModuleFrom.InvalidateControl;
    ModuleFrom.InvalidateConnectors;
  end;

  if assigned(ModuleTo) then begin
    ModuleTo.InvalidateControl;
    ModuleTo.InvalidateConnectors;
  end;
end;

procedure TCableList.Init;
begin
  Clear;

  FLocation   := 0;
  FUnknown    := 0;
  FCableCount := 0;
end;

procedure TCableList.DeleteCable( aCable : TG2FileCable);
var i : integer;
    ModuleFrom, ModuleTo : TG2FileModule;
begin
  i := 0;
  while (i < Count) and ( aCable <> Items[i]) do
    inc(i);
  if (i < Count) then begin

    ModuleFrom := FPatchPart.ModuleList.FindModule( Items[i].FModuleFrom);
    ModuleTo := FPatchPart.ModuleList.FindModule( Items[i].FModuleTo);

    Delete(i);

    if assigned(ModuleFrom) then begin
      ModuleFrom.InvalidateControl;
      ModuleFrom.InvalidateConnectors;
    end;

    if assigned(ModuleTo) then begin
      ModuleTo.InvalidateControl;
      ModuleTo.InvalidateConnectors;
    end;
  end;
end;

procedure TCableList.Read( aChunk : TPatchChunk);
var i, j : integer;
    Color           : TBits3;
    ModuleFrom      : TBits8;
    ConnectorFrom   : TBits6;
    LinkType        : TBits1;
    ModuleTo        : TBits8;
    ConnectorTo     : TBits6;
    NumCables        : TBits10;
begin
  // FLocation   : TBits2;
  // FUnknown    : TBits12;
  // FCableCount : TBits10;
  // FCables     : array of TCable;

  FUnknown    := aChunk.ReadBits( 12);
  NumCables   := aChunk.ReadBits( 10);

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Num Cables = ' + IntToStr( NumCables));
    aChunk.FLogLines.Add('Col MFr CFr Lkp MTo CTo');
  end;

  for i := 0 to NumCables - 1 do begin
    Color         := aChunk.ReadBits( 3);
    ModuleFrom    := aChunk.ReadBits( 8);
    ConnectorFrom := aChunk.ReadBits( 6);
    LinkType      := aChunk.ReadBits( 1);
    ModuleTo      := aChunk.ReadBits( 8);
    ConnectorTo   := aChunk.ReadBits( 6);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d %3d', [Color, ModuleFrom, ConnectorFrom, LinkType, ModuleTo, ConnectorTo]));

    j := 0;
    while (j<Count) and not((Items[j].ModuleFrom = ModuleFrom)
                        and (Items[j].ConnectorFrom = ConnectorFrom)
                        and (Items[j].ModuleTo = ModuleTo)
                        and (Items[j].ConnectorTo = ConnectorTo)) do
      inc(j);
    if j<Count then begin
      Items[j].CableColor := Color;
      Items[j].LinkType := LinkType;
    end else
      Add(FPatchPart.CreateCable( Color, ModuleFrom, ConnectorFrom, LinkType, ModuleTo, ConnectorTo));
  end;
  FCableCount := Count;
end;

procedure TCableList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FUnknown, 12);
  FCableCount := Count;
  aChunk.WriteBits(FCableCount, 10);
  for i := 0 to Count - 1 do begin
    Items[i].Write( aChunk);
  end;
end;

function TCableList.FindCable( FromModule : byte; FromConnector : byte; ToModule : byte; ToConnector : byte): TG2FileCable;
var i : integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FModuleFrom = FromModule)
                         and (Items[i].FConnectorFrom = FromConnector)
                         and (Items[i].FModuleTo = ToModule)
                         and (Items[i].FConnectorTo = ToConnector)) do
    inc(i);
  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
//
//                              TCurrentNote
//
//------------------------------------------------------------------------------

procedure TCurrentNote.Init;
begin
  FNote    := 64;
  FAttack  := 0;
  FRelease := 0;
end;

procedure TCurrentNote.Read( aChunk : TPatchChunk);
begin
  FNote    := aChunk.ReadBits( 7);
  FAttack  := aChunk.ReadBits( 7);
  FRelease := aChunk.ReadBits( 7);
end;

procedure TCurrentNote.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FNote, 7);
  aChunk.WriteBits(FAttack, 7);
  aChunk.WriteBits(FRelease, 7);
end;

//------------------------------------------------------------------------------
//
//                            TCurrentNoteList
//
//------------------------------------------------------------------------------

constructor TCurrentNoteList.Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
begin
  inherited Create( AOwnsObjects);

  FPatch := aPatch;

  FNoteCount := 0;
  FLastNote := TCurrentNote.Create;
end;

destructor TCurrentNoteList.Destroy;
begin
  FNoteCount := 0;
  FLastNote.Free;

  inherited;
end;

function TCurrentNoteList.GetNote( aIndex : integer) : TCurrentNote;
begin
  result := TCurrentNote(inherited Items[aindex]);
end;

procedure TCurrentNoteList.SetNote( aIndex : integer; const aValue : TCurrentNote);
begin
  inherited Items[aindex] := aValue;
end;

function TCurrentNoteList.AddNote( aValue : TCurrentNote): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TCurrentNoteList.Init;
var Note : TCurrentNote;
begin
  FLastNote.Init;

  Clear;
  Note := TCurrentNote.Create;
  Note.Init;
  Add(Note);
end;

procedure TCurrentNoteList.Read( aChunk : TPatchChunk);
var i : integer;
    Note : TCurrentNote;
begin
  //  FLastNote  : TCurrentNote;
  //  FNoteCount : TBits5;
  //  FNotes     : array of TCurrentNote;
  clear;

  FLastNote.Read( aChunk);
  FNoteCount := aChunk.ReadBits( 5) + 1; // ?

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Num Notes = ' + IntToStr( FNoteCount));
    aChunk.FLogLines.Add('Nte Att Rel');
  end;

  for i := 0 to FNoteCount - 1 do begin
    Note := TCurrentNote.Create;
    Note.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d', [Note.FNote, Note.FAttack, Note.FRelease]));

    Add(Note);
  end;
end;

procedure TCurrentNoteList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FLastNote.Write( aChunk);
  FNoteCount := Count - 1;
  aChunk.WriteBits(FNoteCount, 5);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                           TModuleParamValues
//
//------------------------------------------------------------------------------

constructor TModuleParamValues.Create{( aModulevariationList : TModuleVariationList)};
begin
  inherited Create;
  //FModuleVariationList := aModulevariationList;
end;

constructor TModuleParamValues.Copy( {aModuleVariationList : TModuleVariationList;} aParamValues : TModuleParamValues);
//var i : integer;
begin
  //FVariation := aParamValues.FVariation;
  //FModuleVariationList := aModulevariationList;
  {SetLength(FParamValueArray, Length(aParamValues.FParamValueArray));
  for i := 0 to Length(aParamValues.FParamValueArray) - 1 do
    FParamValueArray[i] := aParamValues.FParamValueArray[i];}
  CopyValues( aParamValues);
end;

procedure TModuleParamValues.CopyValues( aParamValues : TModuleParamValues);
var i : integer;
begin
  FVariation := aParamValues.FVariation;
  SetLength(FParamValueArray, Length(aParamValues.FParamValueArray));
  for i := 0 to Length(aParamValues.FParamValueArray) - 1 do
    FParamValueArray[i] := aParamValues.FParamValueArray[i];
end;

procedure TModuleParamValues.Init;
begin
  SetLength(FParamValueArray, 0);
  FVariation := 0;
end;

destructor TModuleParamValues.Destroy;
begin
  Finalize(FParamValueArray);

  inherited;
end;

function TModuleParamValues.GetParamCount: integer;
begin
  Result := Length(FParamValueArray);
end;

function TModuleParamValues.GetParamValue( aIndex : integer): TBits7;
begin
  if aIndex < Length(FParamValueArray) then
    Result := FParamValueArray[ aIndex]
  else
    raise Exception.Create('Param value index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TModuleParamValues.SetParamCount(const Value: integer);
begin
  SetLength(FParamValueArray, Value);
end;

procedure TModuleParamValues.SetParamValue( aIndex : integer; aValue : TBits7);
begin
  if aIndex < Length(FParamValueArray) then
    FParamValueArray[ aIndex] := aValue
  else
    raise Exception.Create('Param value index ' + IntToStr( aIndex) + ' out of range.');
end;

procedure TModuleParamValues.Read( aChunk : TPatchChunk; aParamCount : integer);
var i : integer;
begin
  // FVariation   : Byte;
  // FParamValueArray : array of TBits7;

  //FVariation := aChunk.ReadBits( 8);
  //SetLength(FParamValueArray, FModuleVariationList.FParameterCount);
  SetLength(FParamValueArray, aParamCount);
  for i := 0 to Length(FParamValueArray) -1 do
    FParamValueArray[i] := aChunk.ReadBits( 7);
end;

procedure TModuleParamValues.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FVariation, 8);
  for i := 0 to Length(FParamValueArray) -1 do
    aChunk.WriteBits(FParamValueArray[i], 7);
end;

//------------------------------------------------------------------------------
//
//                           TModuleVariationList
//
//------------------------------------------------------------------------------

constructor TModuleVariationList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart{; aModuleParameterList : TModuleParameterList});
begin
  inherited Create( AOwnsObjects);

  FPatchPart := aPatchPart;

  //FModuleParameterList := aModuleParameterList;
  Init;
end;

destructor TModuleVariationList.Destroy;
begin
  inherited;
end;

procedure TModuleVariationList.Init;
begin
  Clear;

  FModuleIndex := 0;
  FParameterCount := 0;
end;

procedure TModuleVariationList.InitModuleVariationList(aModuleIndex : byte;
  aModuleType : integer);
var i, j, k, pi : integer;
    ModuleParamValues : TModuleParamValues;
begin
  FModuleIndex := aModuleIndex;
  FParameterCount := GetDataParamCount(aModuleType);

  //for i := 0 to FModuleParameterList.FVariationCount - 1 do begin
  for i := 0 to FPatchPart.VariationCount - 1 do begin
    j := 0;
    while (j<Count) and not(Items[j].FVariation = i) do
      inc(j);

    if (j<Count) then
      ModuleParamValues := Items[j]
    else begin
      ModuleParamValues := TModuleParamValues.Create{(self)};
      ModuleParamValues.Variation := i;
      AddVariation(ModuleParamValues);
    end;
    SetLength(ModuleParamValues.FParamValueArray, FParameterCount);
    for k := 0 to Length(ModuleParamValues.FParamValueArray) - 1 do begin
      pi := GetDataModuleParamIndex( aModuleType, k);
      ModuleParamValues.FParamValueArray[k] := ModuleParams[pi].DefaultValue;
    end;
  end;
end;

function TModuleVariationList.GetVariation( aIndex : integer) : TModuleParamValues;
begin
  result := TModuleParamValues(inherited Items[aindex]);
end;

procedure TModuleVariationList.SetVariation( aIndex : integer; const aValue : TModuleParamValues);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleVariationList.AddVariation( aValue : TModuleParamValues): integer;
begin
  Result := inherited Add( aValue);
end;

constructor TModuleVariationList.Copy( AOwnsObjects : boolean{; aModuleParameterList : TModuleParameterList}; aModuleVariationList : TModuleVariationList);
var i, j : integer;
begin
  inherited Create( AOwnsObjects);

  //FModuleParameterList := aModuleParameterList;
  FModuleIndex := aModuleVariationList.FModuleIndex;
  FParameterCount := aModuleVariationList.FParameterCount;

  for i := 0 to aModuleVariationList.Count - 1 do begin
    j := 0;
    while (j<Count) and not(Items[j].FVariation = aModuleVariationList[i].FVariation) do
      inc(j);

    if j<Count then begin
      Items[j].CopyValues( aModuleVariationList[i])
    end else
      Add( TModuleParamValues.Copy({self, }aModuleVariationList.Items[i]));
  end;
end;

procedure TModuleVariationList.AddNewVariation( aVariation : byte);
var ModuleParamValues : TModuleParamValues;
    i, j : integer;
begin
  i := 0;
  while (i<Count) and not(Items[i].FVariation = aVariation) do
    inc(i);

  if i>=Count then begin
    ModuleParamValues := TModuleParamValues.Create{(self)};
    //ModuleParamValues.FVariation := FModuleParameterList.FVariationCount - 1; // FModuleParameters.FVariationCount must already be increased
    ModuleParamValues.FVariation := aVariation;
    SetLength(ModuleParamValues.FParamValueArray, FParameterCount);
    for j := 0 to FParameterCount - 1 do
      ModuleParamValues.FParamValueArray[j] := 0;
    Add(ModuleParamValues);
  end;
end;

procedure TModuleVariationList.CopyVariation( aFromVariation, aToVariation : byte);
var i : integer;
begin
  for i := 0 to Length(Items[aFromVariation].FParamValueArray) - 1 do
    Items[aToVariation].FParamValueArray[i] := Items[aFromVariation].FParamValueArray[i];
end;

function TModuleVariationList.FindVariation( aVariation : integer): TModuleParamValues;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FVariation <> aVariation) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

procedure TModuleVariationList.Read( aChunk : TPatchChunk; aVariationCount : integer);
var i, j : integer;
    ModuleParamValues : TModuleParamValues;
    param_str : string;
    Module : TG2FileModule;
    Variation : byte;
begin
  // Set of parameter values for one module all variations
  // 0 .. 7 : variations 1 .. 8, 8 : init variation
  // FModuleIndex    : TBits8;
  // FParameterCount : TBits7;
  // FVariations     : array[ 0 .. 8] of TModuleParamValues;

  //FModuleIndex    := aChunk.ReadBits( 8);
  FParameterCount := aChunk.ReadBits( 7);

  if assigned( aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module id = ' + IntToStr( FModuleIndex) + ', param cnt = ' + IntToStr(FParameterCount));
    aChunk.FLogLines.Add('Var Params');
  end;

  //for i := 0 to FModuleParameterList.FVariationCount - 1 do begin
  for i := 0 to aVariationCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    j := 0;
    while (j < Count) and (Items[j].FVariation <> Variation) do
      inc(j);

    if (j < Count) then
      ModuleParamValues := Items[j]
    else begin
      ModuleParamValues := TModuleParamValues.Create{(self)};
      ModuleParamValues.FVariation := Variation;
      Add(ModuleParamValues);
    end;
    ModuleParamValues.Read( aChunk, FParameterCount);

    if assigned( aChunk.FLogLines) then begin
      param_str := '';
      for j := 0 to Length(ModuleParamValues.FParamValueArray) - 1 do
        param_str := param_str + IntToStr(ModuleParamValues.FParamValueArray[j]) + ' ';

      aChunk.FLogLines.Add( Format('%3d', [ModuleParamValues.FVariation]) + ' ' + param_str);
    end;
  end;

  i := aVariationCount;
  while (i<N_USB_VARIATIONS) do begin
    AddNewVariation(i);
    inc(i);
  end;


  if assigned(FPatchPart) then begin
    Module := FPatchPart.FindModule( FModuleIndex);
    if assigned(Module) then begin

      // Correction parameter count!
      case Module.FTypeID of
      140 : begin // Mix1-4S
              if FParameterCount = 8 then begin
                FParameterCount := 9;
                for i := 0 to Count - 1 do begin
                  SetLength( Items[i].FParamValueArray, 9);
                  Items[i].FParamValueArray[8] := 0;
                end;
              end;
            end;
      end;

      Module.InvalidateParameters;
    end;
  end;
end;

procedure TModuleVariationList.Write( aChunk : TPatchChunk; aVariationCount : integer);
var i : integer;
begin
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParameterCount, 7);
  //for i := 0 to min( Count, FModuleParameters.FPatch.FMaxVariations) - 1 do
  for i := 0 to aVariationCount - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                           TModuleParameterList
//
//------------------------------------------------------------------------------

constructor TModuleParameterList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;
end;

constructor TModuleParameterList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aModuleParameterList : TModuleParameterList);
var i, j : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;
  //FModuleVariationListCount := aModuleParameterList.Count;
  //FVariationCount := aModuleParameterList.FVariationCount;
  //for i := 0 to FModuleVariationListCount - 1 do begin
  for i := 0 to aModuleParameterList.Count - 1 do begin
    Module := aModuleList.FindModule( aModuleParameterList.Items[ i].ModuleIndex);

    if assigned(Module) then begin
      j := 0;
      while (j<Count) and not(Items[j].ModuleIndex = Module.ModuleIndex) do
        inc(j);

      if j>=Count then
        Add( TModuleVariationList.Copy( True, {self,} aModuleParameterList.Items[ i]));
    end;
  end;
end;

destructor TModuleParameterList.Destroy;
begin
  inherited;
end;

procedure TModuleParameterList.Init;
begin
  Clear;

  //FModuleVariationListCount := 0;
  //FVariationCount := 0;
end;

function TModuleParameterList.GetModuleVariationList( aIndex : integer) : TModuleVariationList;
begin
  result := TModuleVariationList(inherited Items[aindex]);
end;

procedure TModuleParameterList.SetModuleVariationList( aIndex : integer; const aValue : TModuleVariationList);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleParameterList.AddModuleVariationList( aValue : TModuleVariationList): integer;
begin
  Result := inherited Add( aValue);
  //FModuleVariationListCount := Count;
end;

procedure TModuleParameterList.DeleteModule( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    //FModuleVariationListCount := Count;
  end;
end;

procedure TModuleParameterList.Read( aChunk : TPatchChunk);
var i, j : integer;
    ModuleVariationList : TModuleVariationList;
    ModuleCount, ModuleIndex, VariationCount : integer;
begin
  //  FLocation  : Tbits2;
  //  FSetCount  : TBits8;
  //  FUnknown   : TBits8  <= this must be variationcount, there seem to be 10 variations in patches that come from usb
  //  FParamSets : array of TModuleVariationList;

  //FModuleVariationListCount := aChunk.ReadBits( 8); // Number of modules in the list
  ModuleCount := aChunk.ReadBits( 8); // Number of modules in the list
  VariationCount := aChunk.ReadBits( 8); // Number of variations in the list

  if assigned( aChunk.FLogLines) then begin
    //aChunk.FLogLines.Add('Num sets = ' + IntToStr(FModuleVariationListCount) + ', var cnt = ' + IntToStr(FVariationCount));
    aChunk.FLogLines.Add('Num sets = ' + IntToStr(ModuleCount) + ', var cnt = ' + IntToStr(VariationCount));
    aChunk.FLogLines.Add('Var Params');
  end;

  //for i := 0 to FModuleVariationListCount - 1 do begin
  for i := 0 to ModuleCount - 1 do begin
    ModuleIndex := aChunk.ReadBits( 8);
    j := 0;
    while (j<Count) and (Items[j].ModuleIndex <> ModuleIndex) do
      inc(j);

    if (j<Count) then
      ModuleVariationList := Items[j]
    else begin
      ModuleVariationList := TModuleVariationList.Create( True, FPatchPart{, self});
      ModuleVariationList.ModuleIndex := ModuleIndex;
      Add(ModuleVariationList);
    end;
    ModuleVariationList.Read( achunk, VariationCount);
  end;
  //FModuleVariationListCount := Count;
end;

procedure TModuleParameterList.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  //FModuleVariationListCount := Count;
  //aChunk.WriteBits(FModuleVariationListCount, 8);
  aChunk.WriteBits(Count, 8);
  //if FModuleVariationListCount = 0 then
  if Count = 0 then
    aChunk.WriteBits( 0, 8)
  else
    aChunk.WriteBits( aVariationCount, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk, aVariationCount);
end;

{procedure TModuleParameterList.AddNewVariation( aVariation : byte);
var i : integer;
begin
  //inc(FVariationCount);
  for i := 0 to Count - 1 do
    //Items[i].AddNewVariation(FVariationCount - 1);
    Items[i].AddNewVariation( aVariation);
end;}

procedure TModuleParameterList.CopyVariation( aFromVariation, aToVariation : byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].CopyVariation( aFromVariation, aToVariation);
end;

function TModuleParameterList.FindModuleVariationList( aModuleIndex : integer): TModuleVariationList;
var i : integer;
begin
  i := 0;
  while (i < Count) and ( Items[i].ModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TModuleParameterList.FindParamValue( aModuleIndex, aVariation, aParamIndex : integer): TBits7;
var ModuleVariationList : TModuleVariationList;
    ModuleParamValues : TModuleParamValues;
begin
  ModuleVariationList := FindModuleVariationList( aModuleIndex);
  if assigned(ModuleVariationList) then begin
    ModuleParamValues := ModuleVariationList.FindVariation( aVariation);
    if assigned(ModuleParamValues) then
      Result := ModuleParamValues.ParamValues[ aParamIndex]
    else
      Result := 0;
  end else
    Result := 0;
end;

//------------------------------------------------------------------------------
//
//                              TMorphSetting
//
//------------------------------------------------------------------------------

constructor TMorphSetting.Create{(aPatch: TG2FilePatch)};
begin
  //inherited Create( aPatch); // Owned by patch CHECK
  SetLength( FModeArray, 0);
  SetLength( FDialArray, 0);
  //FMorphName := '';
end;

destructor TMorphSetting.Destroy;
begin
  Finalize(FModeArray);
  Finalize(FDialArray);
  inherited;
end;

function TMorphSetting.GetDial( aVariation : integer): TBits7;
begin
  if aVariation < Length(FDialArray) then
    Result := FDialArray[ aVariation]
  else
    raise Exception.Create('Dial variation ' + IntToStr( aVariation) + ' out of range.');
end;

procedure TMorphSetting.SetDial( aVariation : integer; aValue : TBits7);
begin
  if aVariation < Length(FDialArray) then
    FDialArray[ aVariation] := aValue
  else
    raise Exception.Create('Dial variation ' + IntToStr( aVariation) + ' out of range.');
end;

function TMorphSetting.GetMode( aVariation : integer): TBits7;
begin
  if aVariation < Length(FModeArray) then
    Result := FModeArray[ aVariation]
  else
    raise Exception.Create('Mode variation ' + IntToStr( aVariation) + ' out of range.');
end;

{function TMorphSetting.GetMorphName: string;
var i : integer;
begin
  Result := FMorphName;
end;

procedure TMorphSetting.SetMorphName(const Value: string);
begin
  FMorphName := Value;
end;}

procedure TMorphSetting.SetMode( aVariation : integer; aValue : TBits7);
begin
  if aVariation < Length(FDialArray) then
    FModeArray[ aVariation] := aValue
  else
    raise Exception.Create('Mode variation ' + IntToStr( aVariation) + ' out of range.');
end;

//------------------------------------------------------------------------------
//
//                              TMorphParameter
//
//------------------------------------------------------------------------------

procedure TMorphParameter.Init;
begin
  FLocation    := 0;
  FModuleIndex := 0;
  FParamIndex  := 0;
  FMorph       := 0;
  FRange       := 0;
end;

procedure TMorphParameter.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FLocation, 2);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParamIndex, 7);
  aChunk.WriteBits(FMorph, 4);
  aChunk.WriteBits(FRange, 8);
end;

//------------------------------------------------------------------------------
//
//                             TMorphParameters
//
//------------------------------------------------------------------------------

constructor TMorphParameters.Create( aPatch : TG2FilePatch);
begin
  inherited Create;

  FPatch := aPatch;
end;

destructor TMorphParameters.Destroy;
begin
  inherited;
end;

procedure TMorphParameters.Init;
var i : integer;
begin
  // TVariations in TPatchsettings should already be created and initialized

  FVariationCount := Length(FPatch.FPatchSettings.FVariationListArray);
  FMorphCount := NMORPHS;
  FReserved1 := 0;
  FReserved2 := 0;

  for i := 0 to FVariationCount - 1 do begin
    Unknown1 := 0;
    //for j := 0 to 5 do
    //  Unknown2[j] := 0;
    Unknown2 := 0;
    Unknown3 := 0;
    Unknown4 := 0;
    Unknown5 := 0;
    Unknown6 := 0;
    Unknown7 := 0;

    Unknown8 := 0;

    FPatch.FPatchSettings.FVariationListArray[i].Clear;

    FReserved2      := 0;
  end;
end;

procedure TMorphParameters.Read( aChunk : TPatchChunk);
var i, j, k, BitsLeft : integer;
    PatchParamList : TPatchParamList;
    VariationIndex : integer;
    MorphParamCount : integer;
    MorphParam : TMorphParameter;
    Location : TBits2;
    ModuleIndex : TBits8;
    ParamIndex : TBits7;
    Morph : TBits4;
    Range : TBits8;
begin
  // FVariationCount : TBits8;
  // FMorphCount     : TBits4;
  // FReserved1      : integer;
  // FReserved2      : TBits4;

  FVariationCount := aChunk.ReadBits( 8);
  FMorphCount     := aChunk.ReadBits( 4);
  FReserved1      := aChunk.ReadBits( 20);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Morph param list, varcount : ' + IntToStr(FVariationCount) + ', morphcount : ' + IntToStr(FMorphCount));
  end;

  for i := 0 to FVariationCount - 1 do begin
    VariationIndex := aChunk.ReadBits( 4);

    // These should be fields in TPatchParamList, otherwise there overwritten here for each PatchParamList, but maybe they are always 0
    Unknown1 := aChunk.ReadBits( 4);
    //for j := 0 to 5 do
    //  Unknown2[j] := aChunk.ReadBits( 8);
    Unknown2 := aChunk.ReadBits( 8);
    Unknown3 := aChunk.ReadBits( 8);
    Unknown4 := aChunk.ReadBits( 8);
    Unknown5 := aChunk.ReadBits( 8);
    Unknown6 := aChunk.ReadBits( 8);
    Unknown7 := aChunk.ReadBits( 8);

    Unknown8 := aChunk.ReadBits( 4);

    MorphParamCount := aChunk.ReadBits( 8);

    PatchParamList := FPatch.FPatchSettings.FVariationListArray[VariationIndex];

    if assigned(aChunk.FLogLines) then begin
      aChunk.FLogLines.Add('Morph params, variation : ' + IntToStr(VariationIndex) + ', count = ' + IntToStr(MorphParamCount));
      aChunk.FLogLines.Add('Loc Mod Par Mor Rng');
    end;

    for j := 0 to MorphParamCount - 1 do begin

      Location    := aChunk.ReadBits( 2);
      ModuleIndex := aChunk.ReadBits( 8);
      ParamIndex  := aChunk.ReadBits( 7);
      Morph       := aChunk.ReadBits( 4);
      Range       := aChunk.ReadBits( 8);

      if assigned(aChunk.FLogLines) then
        aChunk.FLogLines.Add(Format('%3d %3d %3d %3d %3d', [Location, ModuleIndex, ParamIndex, Morph, Range]));

      k := 0;
      while (k<PatchParamList.Count) and not((Location = PatchParamList[k].FLocation)
                                         and (ModuleIndex = PatchParamList[k].FModuleIndex)
                                         and (ParamIndex = PatchParamList[k].FParamIndex)
                                         and (Morph = PatchParamList[k].FMorph)) do
        inc(k);

      if k<PatchParamList.Count then begin
        MorphParam := PatchParamList[k];
        MorphParam.FRange := Range;
      end else begin
        MorphParam := TMorphParameter.Create;
        //MorphParam.Read( aChunk);
        MorphParam.FLocation := Location;
        MorphParam.FModuleIndex := ModuleIndex;
        MorphParam.FParamIndex := ParamIndex;
        MorphParam.FMorph := Morph;
        MorphParam.FRange := Range;
        PatchParamList.Add( MorphParam);
      end;
    end;

    BitsLeft := aChunk.GetReadBufferBitsLeft;
    if BitsLeft < 4 then
      // At the end of the chunk we need to read the bits that are left over to align to byte
      FReserved2 := aChunk.ReadBits( BitsLeft)
    else
      FReserved2 := aChunk.ReadBits( 4);
  end;
end;

procedure TMorphParameters.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i, j : integer;
    PatchParamList : TPatchParamList;
begin
  //NumVariations := min( FVariationCount, FPatch.FMaxVariations);
  //aChunk.WriteBits(FVariationCount, 8);
  aChunk.WriteBits(aVariationCount, 8);
  aChunk.WriteBits(FMorphCount,     4);
  aChunk.WriteBits(FReserved1,     20);

  for i := 0 to aVariationCount - 1 do begin

    PatchParamList := FPatch.FPatchSettings.FVariationListArray[i];

    aChunk.WriteBits(i, 4);

    aChunk.WriteBits(Unknown1, 4);
    //for j := 0 to 5 do
    //  aChunk.WriteBits(Unknown2[j], 8);
    aChunk.WriteBits(Unknown2, 8);
    aChunk.WriteBits(Unknown3, 8);
    aChunk.WriteBits(Unknown4, 8);
    aChunk.WriteBits(Unknown5, 8);
    aChunk.WriteBits(Unknown6, 8);
    aChunk.WriteBits(Unknown7, 8);

    aChunk.WriteBits(Unknown8, 4);

    aChunk.WriteBits( PatchParamList.Count,  8);
    for j := 0 to PatchParamList.Count - 1 do begin
      PatchParamList.Items[j].Write( aChunk);
    end;

    if i < (aVariationCount - 1) then
      aChunk.WriteBits(FReserved2, 4)
    else
    // At the end of the chunk we need to write the bits that are left over to align to byte
      if aChunk.FBitWriter.GetWriteBufferBitsLeft < 8 then
        // It seems that in case of performances when this is <> 0, the perf. failes to load! This one only took one evening ;)
        aChunk.WriteBits({FReserved2}0, aChunk.FBitWriter.GetWriteBufferBitsLeft);
  end;
end;

//------------------------------------------------------------------------------
//
//                             TPatchParamList
//
//------------------------------------------------------------------------------

constructor TPatchParamList.Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
begin
  inherited Create( AOwnsObjects);
  FPatch := aPatch;
end;

destructor TPatchParamList.Destroy;
begin
  inherited;
end;

procedure TPatchParamList.Copy( aFromVariation : TPatchParamList);
begin
  FPatchVol    := aFromVariation.FPatchVol;
  FGlide       := aFromVariation.FGlide;
  FGlideTime   := aFromVariation.FGlideTime;
  FBend        := aFromVariation.FBend;
  FSemi        := aFromVariation.FSemi;
  FVibrato     := aFromVariation.FVibrato;
  FCents       := aFromVariation.FCents;
  FRate        := aFromVariation.FRate;
  FArpeggiator := aFromVariation.FArpeggiator;
  FArpTime     := aFromVariation.FArpTime;
  FArpType     := aFromVariation.FArpType;
  FOctaves     := aFromVariation.FOctaves;
  FSustain     := aFromVariation.FSustain;
  FOctaveShift := aFromVariation.FOctaveShift;
end;

function TPatchParamList.GetMorphParam( aIndex : integer) : TMorphParameter;
begin
  result := TMorphParameter(inherited Items[aindex]);
end;

procedure TPatchParamList.SetMorphParam( aIndex : integer; const aValue : TMorphParameter);
begin
  inherited Items[aindex] := aValue;
end;

function TPatchParamList.AddMorphParam( aValue : TMorphParameter): integer;
begin
  Result := inherited Add( aValue);
end;

function TPatchParamList.FindMorphParam( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
var i : integer;
begin
  Result := nil;
  i := 0;
  while (i < Count) and
    not((Items[i].FLocation = TBits2(aLocation)) and
        (Items[i].FModuleIndex = aModuleIndex) and
        (Items[i].FParamIndex = aParamIndex) and
        (Items[i].FMorph = aMorphIndex)) do
    inc(i);

  if (i < Count) then
    Result := Items[i];
end;

procedure TPatchParamList.AddNewMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aRange: byte);
var MorphParam : TMorphParameter;
begin
  MorphParam := TMorphParameter.Create;
  MorphParam.FLocation := aLocation;
  MorphParam.FModuleIndex := aModuleIndex;
  MorphParam.FParamIndex := aParamIndex;
  MorphParam.FMorph := aMorphIndex;
  MorphParam.FRange := aRange;
  Add( MorphParam);
end;

procedure TPatchParamList.DelMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex: byte);
var i: integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FLocation = aLocation) and
                            (Items[i].FModuleIndex = aModuleIndex) and
                            (Items[i].FParamIndex = aParamIndex) and
                            (Items[i].FMorph = aMorphIndex)) do
    inc(i);

  if (i < Count) then begin
    Delete( i);
  end;
end;

procedure TPatchParamList.DeleteModule( aLocation, aModuleIndex : byte);
var i: integer;
begin
  i := 0;
  while (i < Count) do begin
    if ((Items[i].FLocation = aLocation) and
        (Items[i].FModuleIndex = aModuleIndex)) then
      Delete( i)
    else
      inc(i);
  end;
end;

//------------------------------------------------------------------------------
//
//                             TPatchSettings
//
//------------------------------------------------------------------------------

constructor TPatchSettings.Create( aPatch : TG2FilePatch);
var i : integer;
begin
  inherited Create;

  FPatch := aPatch;

  // Start to create 9 Variations
  SetLength(FVariationListArray, N_USB_VARIATIONS);
  for i := 0 to N_USB_VARIATIONS - 1 do
    FVariationListArray[i] := TPatchParamList.Create( True, FPatch);

  SetLength(FMorphArray, NMORPHS);
  for i := 0 to NMORPHS - 1 do begin
    FMorphArray[i] := TMorphSetting.Create{( FPatch)};
    SetLength(FMorphArray[i].FDialArray, N_USB_VARIATIONS);
    SetLength(FMorphArray[i].FModeArray, N_USB_VARIATIONS);
  end;
end;

procedure TPatchSettings.Init;
var i , j : integer;
begin
  FLocation       := 2;
  FSectionCnt     := 7;
  //FVariationCount := N_USB_VARIATIONS;

  //SetLength(FVariationListArray, FVariationCount);
  //SetLength(FMorphArray, NMORPHS);

  for i := 0 to VariationCount - 1 do begin
    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FDialArray[i] := 0;

    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FModeArray[i] := 1;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FPatchVol    := 100;
    FVariationListArray[i].FActiveMuted := 1;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FGlide      := 0;
    FVariationListArray[i].FGlideTime  := 28;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FBend       := 1;
    FVariationListArray[i].FSemi       := 1;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FVibrato     := 0;
    FVariationListArray[i].FCents       := 50;
    FVariationListArray[i].FRate        := 64;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FArpeggiator := 0;
    FVariationListArray[i].FArpTime     := 3;
    FVariationListArray[i].FArpType     := 0;
    FVariationListArray[i].FOctaves     := 0;
  end;

  for i := 0 to VariationCount - 1 do begin
    FVariationListArray[i].FOctaveShift := 2;
    FVariationListArray[i].FSustain     := 1;
  end;
end;

destructor TPatchSettings.Destroy;
var i : integer;
begin
  for i := 0 to NMORPHS - 1 do begin
    Finalize(FMorphArray[i].FDialArray);
    Finalize(FMorphArray[i].FModeArray);
    FMorphArray[i].Free;
  end;
  Finalize(FMorphArray);

  for i := 0 to Length(FVariationListArray) -1 do
    FVariationListArray[i].Free;
  Finalize(FVariationListArray);

  inherited;
end;

function TPatchSettings.GetPatchParamList( aVariation : integer): TPatchParamList;
begin
  if aVariation < VariationCount then
    Result := FVariationListArray[ aVariation]
  else
    raise Exception.Create('Variation index ' + IntToStr( aVariation) + ' out of range.');
end;

function TPatchSettings.GetVariationCount: TBits8;
begin
  Result := Length(FVariationListArray);
end;

procedure TPatchSettings.SetPatchParamList( aVariation : integer; aValue : TPatchParamList);
begin
  if aVariation < VariationCount then
    FVariationListArray[ aVariation] := aValue
  else
    raise Exception.Create('Variation index ' + IntToStr( aVariation) + ' out of range.');
end;

procedure TPatchSettings.Read( aChunk : TPatchChunk);
var i, j, Variation, SrceVarCount : integer;
begin
  // FLocation       : Tbits2;
  // FSectionCnt     : TBits8;
  // FVariationCount : TBits8;
  // FMorphArray         : array of TMorph;
  // FVariationListArray     : array of TPatchParamList;

  FSectionCnt    := aChunk.ReadBits( 8);
  SrceVarCount   := aChunk.ReadBits( 8);

{  i := Length(FVariationListArray);
  SetLength(FVariationListArray, FVariationCount); // Can be more then 9?
  while (i<FVariationCount) do begin
    FVariationListArray[i] := TPatchParamList.Create( True, FPatch);  // Create more when necessary (usb)
    inc(i);
  end;}

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FDialArray[Variation] := aChunk.ReadBits( 7);

    for j := 0 to NMORPHS - 1 do
      FMorphArray[j].FModeArray[Variation] := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FPatchVol    := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FActiveMuted := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FGlide     := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FGlideTime := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FBend := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FSemi := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FVibrato     := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FCents       := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FRate        := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FArpeggiator := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FArpTime     := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FArpType     := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FOctaves     := aChunk.ReadBits( 7);
  end;

  aChunk.ReadBits( 8); // Sections
  aChunk.ReadBits( 7); // Entries

  for i := 0 to SrceVarCount - 1 do begin
    Variation := aChunk.ReadBits( 8);
    FVariationListArray[Variation].FOctaveShift := aChunk.ReadBits( 7);
    FVariationListArray[Variation].FSustain     := aChunk.ReadBits( 7);
  end;
end;


procedure TPatchSettings.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i , j : integer;
begin
  //NumVariations := min(FVariationCount, FPatch.FMaxVariations);

  aChunk.WriteBits( FSectionCnt, 8);
  aChunk.WriteBits( aVariationCount, 8);

  aChunk.WriteBits(1, 8);
  aChunk.WriteBits(16, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphArray[j].FDialArray[i], 7);

    for j := 0 to NMORPHS - 1 do
      aChunk.WriteBits(FMorphArray[j].FModeArray[i], 7);
  end;

  aChunk.WriteBits(2, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FPatchVol, 7);
    aChunk.WriteBits(FVariationListArray[i].FActiveMuted, 7);
  end;

  aChunk.WriteBits(3, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits(i, 8);
    aChunk.WriteBits(FVariationListArray[i].FGlide, 7);
    aChunk.WriteBits(FVariationListArray[i].FGlideTime, 7);
  end;

  aChunk.WriteBits(4, 8);
  aChunk.WriteBits(2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariationListArray[i].FBend, 7);
    aChunk.WriteBits( FVariationListArray[i].FSemi, 7);
  end;

  aChunk.WriteBits( 5, 8);
  aChunk.WriteBits( 3, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariationListArray[i].FVibrato, 7);
    aChunk.WriteBits( FVariationListArray[i].FCents, 7);
    aChunk.WriteBits( FVariationListArray[i].FRate, 7);
  end;

  aChunk.WriteBits( 6, 8);
  aChunk.WriteBits( 4, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariationListArray[i].FArpeggiator, 7);
    aChunk.WriteBits( FVariationListArray[i].FArpTime, 7);
    aChunk.WriteBits( FVariationListArray[i].FArpType, 7);
    aChunk.WriteBits( FVariationListArray[i].FOctaves, 7);
  end;

  aChunk.WriteBits( 7, 8);
  aChunk.WriteBits( 2, 7);

  for i := 0 to aVariationCount - 1 do begin
    aChunk.WriteBits( i, 8);
    aChunk.WriteBits( FVariationListArray[i].FOctaveShift, 7);
    aChunk.WriteBits( FVariationListArray[i].FSustain, 7);
  end;
end;

//------------------------------------------------------------------------------
//
//                                  TKnob
//
//------------------------------------------------------------------------------

procedure TKnob.Init;
begin
 FAssigned := 0;
 FLocation := 0;
 FModuleIndex := 0;
 FIsLed := 0;
 FParamIndex := 0;

 FParameter := nil;
end;

procedure TKnob.Read( aChunk : TPatchChunk);
begin
  // Fassigned : TBits1;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FIsLed : TBits2;
  // FParamIndex: TBits7;


  FAssigned    := aChunk.ReadBits( 1);
  if FAssigned = 1 then begin

    FLocation    := aChunk.ReadBits( 2);
    FModuleIndex := aChunk.ReadBits( 8);
    FIsLed       := aChunk.ReadBits( 2);
    FParamIndex  := aChunk.ReadBits( 7);
  end;
end;

procedure TKnob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(FLocation, 2);
    aChunk.WriteBits(FModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(FParamIndex, 7);
  end;
end;

function TKnob.GetKnobValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.GetValue
  else
    Result := 0;
end;

procedure TKnob.SetKnobValue( aValue : byte);
begin
  if (FAssigned = 1) and assigned(FParameter) then
    FParameter.SetValue( aValue);
end;

function TKnob.GetKnobButtonValue : byte;
var ButtonParam : TG2FileParameter;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      Result := ButtonParam.GetValue;
  end;
end;

procedure TKnob.SetKnobButtonValue( aValue : byte);
var ButtonParam : TG2FileParameter;
begin
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetValue( aValue);
  end;
end;

function TKnob.GetKnobFloatValue : single;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then
    if FParameter.HighValue - FParameter.LowValue <> 0 then
      Result := FParameter.GetValue / (FParameter.HighValue - FParameter.LowValue);
end;

function TKnob.GetKnobButtonFloatValue : single;
var ButtonParam : TG2FileParameter;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then begin
      if ButtonParam.HighValue - ButtonParam.LowValue <> 0 then
        Result := ButtonParam.GetValue / (ButtonParam.HighValue - ButtonParam.LowValue);
    end;
  end;
end;

procedure TKnob.SetKnobFloatValue( aValue : single);
begin
  if (FAssigned = 1) and assigned(FParameter) then
    FParameter.SetValue( trunc(aValue * 127));
end;

procedure TKnob.SetKnobButtonFloatValue( aValue : single);
var ButtonParam : TG2FileParameter;
begin
  if (FAssigned = 1) and assigned(FParameter) then begin
    ButtonParam := FParameter.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetValue( trunc(aValue * 127));
  end;
end;

function TKnob.GetKnobHighValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.HighValue
  else
    Result := 0;
end;

function TKnob.GetKnobLowValue : byte;
begin
  if (FAssigned = 1) and assigned(FParameter) then
    Result := FParameter.LowValue
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
//
//                               TKnobList
//
//------------------------------------------------------------------------------

constructor TKnobList.Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
var i : integer;
    Knob : TKnob;
begin
  inherited Create( AOwnsObjects);

  FPatch := aPatch;

  FKnobCount := 120;
  for i := 0 to FKnobCount - 1 do begin
    Knob := TKnob.Create;
    Knob.FKnobIndex := i;
    Add( Knob);
  end;
end;

destructor TKnobList.Destroy;
begin
  inherited;
end;

function TKnobList.GetKnob( aIndex : integer) : TKnob;
begin
  result := TKnob(inherited Items[aindex]);
end;

procedure TKnobList.SetKnob( aIndex : integer; const aValue : TKnob);
begin
  inherited Items[aindex] := aValue;
end;

function TKnobList.AddKnob( aValue : TKnob): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TKnobList.Init;
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Init;
end;

procedure TKnobList.DeleteModule( aLocation, aModuleIndex : Byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].FModuleIndex = aModuleIndex) and (Items[i].FLocation = aLocation) then
      Items[i].Fassigned := 0;
end;

procedure TKnobList.Read( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := aChunk.ReadBits( 16);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('KnobList, count = ' + IntToStr(FKnobCount));
    aChunk.FLogLines.Add('Knb Loc Mod Isl Par');
  end;

  for i := 0 to FKnobCount - 1 do begin
    Items[i].Read( aChunk);

    if assigned( aChunk.FLogLines) and (Items[i].FAssigned = 1) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d', [i, Items[i].FLocation, Items[i].FModuleIndex, Items[i].FIsLed, Items[i].FParamIndex]));
  end;
end;

procedure TKnobList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := Count;
  aChunk.WriteBits(FKnobCount, 16);
  for i := 0 to FKnobCount - 1 do
    Items[i].Write( aChunk);
end;

function TKnobList.FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(( Items[i].FAssigned = 1)
                        and ( Items[i].FLocation = TBits2(aLocation))
                        and ( Items[i].FModuleIndex = aModuleIndex)
                        and ( Items[i].FParamIndex = aParamIndex) ) do
    inc(i);

  if (i < Count) then begin
    Result := i
  end else
    Result := -1;
end;

//------------------------------------------------------------------------------
//
//                               TController
//
//------------------------------------------------------------------------------

procedure TController.Init;
begin
  FMidiCC := 0;
  FLocation := 0;
  FModuleIndex := 0;
  FParamIndex := 0;
  FKnob := nil;
 end;

procedure TController.Read( aChunk : TPatchChunk);
begin
  // FMidiCC : TBits7;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FParamIndex : TBits7;

  FMidiCC      := aChunk.ReadBits( 7);
  FLocation    := aChunk.ReadBits( 2);
  FModuleIndex := aChunk.ReadBits( 8);
  FParamIndex  := aChunk.ReadBits( 7);
  FKnob := nil;
end;

procedure TController.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FMidiCC, 7);
  aChunk.WriteBits(FLocation, 2);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParamIndex, 7);
end;

//------------------------------------------------------------------------------
//
//                               TControllerList
//
//------------------------------------------------------------------------------

constructor TControllerList.Create( AOwnsObjects : boolean; aPatch : TG2FilePatch);
begin
  inherited Create( AOwnsObjects);
  FPatch := aPatch;
end;

destructor TControllerList.Destroy;
begin
  inherited;
end;

function TControllerList.GetController( aIndex : integer) : TController;
begin
  result := TController(inherited Items[aindex]);
end;

procedure TControllerList.SetController( aIndex : integer; const aValue : TController);
begin
  inherited Items[aindex] := aValue;
end;

function TControllerList.AddController( aValue : TController): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TControllerList.Init;
var Controller : TController;
begin
  Clear;

  Controller := TController.Create;
  Controller.FMidiCC := 7;
  Controller.FLocation := 2;
  Controller.FModuleIndex := 2;
  Controller.FParamIndex := 0;
  Add( Controller);

  Controller := TController.Create;
  Controller.FMidiCC := 17;
  Controller.FLocation := 2;
  Controller.FModuleIndex := 7;
  Controller.FParamIndex := 0;
  Add( Controller);
end;

procedure TControllerList.DeleteModule( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FControllerCount := Count;
  end;

end;

procedure TControllerList.Read( aChunk : TPatchChunk);
var i : integer;
    Controller : TController;
begin
  // FControllerCount : TBits7;
  Clear;

  FControllerCount := aChunk.ReadBits( 7);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Controller list, count = ' + IntToStr(FControllerCount));
    aChunk.FLogLines.Add('CC# Loc Mod Par');
  end;

  for i := 0 to FControllerCount - 1 do begin
    Controller := TController.Create;
    Controller.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d', [Controller.FMidiCC, Controller.FLocation, Controller.FModuleIndex, Controller.FParamIndex]));
    Add( Controller);
  end;
end;

procedure TControllerList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FControllerCount := Count;
  aChunk.WriteBits(FControllerCount, 7);
  for i := 0 to FControllerCount - 1 do begin
    Items[i].Write(aChunk);
  end;
end;

function TControllerList.FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
var i : integer;
begin
  i := 0;
  while (i < Count)
           and not(( Items[i].FLocation = TBits2(aLocation))
               and ( Items[i].FModuleIndex = aModuleIndex)
               and ( Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then begin
    Result :=  Items[i].FMidiCC
  end else
    Result := 0;
end;

//------------------------------------------------------------------------------
//
//                                TParamLabel
//
//------------------------------------------------------------------------------

constructor TParamLabel.Create;
begin
  inherited;
  SetLength(FNameArray, 7);
end;

destructor TParamLabel.Destroy;
begin
  Finalize(FNameArray);
  inherited;
end;

procedure TParamLabel.Init;
var i : integer;
begin
  SetLength(FNameArray, 7);
  for i := 0  to 6 do
    FNameArray[i] := 0;
end;

function TParamLabel.GetName : string;
var i : integer;
begin
  Result := '';
  i := 0;
  while (i<7) and (FNameArray[i] <> 0) do begin
    Result := Result + Char(FNameArray[i]);
    inc(i);
  end;
end;

procedure TParamLabel.SetName( aValue : string);
var i : integer;
begin
  i := 0;
  while (i<7) and (i < aValue.Length) do begin
    FNameArray[i] := byte(aValue.Chars[i]);
    inc(i);
  end;
  while (i<7) do begin
    FNameArray[i] := 0;
    inc(i);
  end;
end;

procedure TParamLabel.Read( aChunk : TPatchChunk);
var i : integer;
begin
  for i := 0 to 6 do
    FNameArray[i] := aChunk.ReadBits( 8);
end;

procedure TParamLabel.Write( aChunk : TPatchChunk);
var i : integer;
begin
  for i := 0 to 6 do
    aChunk.WriteBits(FNameArray[i], 8);
end;

//------------------------------------------------------------------------------
//
//                           TParamLabelParamList
//
//------------------------------------------------------------------------------

constructor TParamLabelParamList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

constructor TParamLabelParamList.CopyCreate( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aParamLabelParam : TParamLabelParamList);
var i, j : integer;
    ParamLabel : TParamLabel;
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;
  FIsString   := aParamLabelParam.FIsString;
  FParamLen   := aParamLabelParam.FParamLen;
  FParamIndex := aParamLabelParam.FParamIndex;

  for i := 0 to aParamLabelParam.Count - 1 do begin
    ParamLabel := TParamLabel.Create;
    for j := 0 to 6 do
      ParamLabel.FNameArray[j] := aParamLabelParam.Items[i].FNameArray[j];
    Add( ParamLabel);
  end;
end;

destructor TParamLabelParamList.Destroy;
begin
  inherited;
end;

procedure TParamLabelParamList.Init;
begin
  FIsString   := 0;
  FParamLen   := 0;
  FParamIndex := 0;
  Clear;
end;

function TParamLabelParamList.GetParamLabel( aIndex : integer) : TParamLabel;
begin
  result := TParamLabel(inherited Items[aindex]);
end;

procedure TParamLabelParamList.SetParamLabel( aIndex : integer; const aValue : TParamLabel);
begin
  inherited Items[aindex] := aValue;
end;

function TParamLabelParamList.AddParamLabel( aValue : TParamLabel): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParamLabelParamList.Read( aChunk : TPatchChunk);
begin
  // FIsString   : TBits8; 0 Extra parameter value, 1 : Label
  // FParamLen   : TBits8; if label then label length + parameterindex length else 1
  // FParamIndex : TBits8; if label then parameterindex else extra parameter value
  // FParamNames : array of TParamName;
end;

procedure TParamLabelParamList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  if FIsString = 1 then
    FParamLen := 1 + Count * 7;

  aChunk.WriteBits(FIsString,   8);
  aChunk.WriteBits(FParamLen,   8);
  aChunk.WriteBits(FParamIndex, 8);

  if FIsString = 1 then
    for i := 0 to Count - 1 do
      Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                           TParamLabelModuleList
//
//------------------------------------------------------------------------------

constructor TParamLabelModuleList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

constructor TParamLabelModuleList.CopyCreate( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aParamLabelModule : TParamLabelModuleList);
var i : integer;
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;
  FModuleIndex := aParamLabelModule.FModuleIndex;
  FModuleLen   := aParamLabelModule.FModuleLen;
  for i := 0 to aParamLabelModule.Count - 1 do begin
    Add( TParamLabelParamList.CopyCreate( True, aPatchPart, aParamLabelModule.Items[i]));
  end;
end;

destructor TParamLabelModuleList.Destroy;
begin
  inherited;
end;

function TParamLabelModuleList.GetParameterLabelCount( aParamIndex: byte): integer;
begin
  Result := Items[aParamIndex].Count;
end;

function TParamLabelModuleList.GetParamLabelParam( aIndex : integer) : TParamLabelParamList;
begin
  result := TParamLabelParamList(inherited Items[aindex]);
end;

procedure TParamLabelModuleList.SetParamLabelParam( aIndex : integer; const aValue : TParamLabelParamList);
begin
  inherited Items[aindex] := aValue;
end;

function TParamLabelModuleList.AddParamLabelParam( aValue : TParamLabelParamList): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParamLabelModuleList.Init;
begin
  FModuleIndex := 0;
  FModuleLen   := 0;
  Clear;
end;

procedure TParamLabelModuleList.InitParamLabelModuleList(aModuleIndex: byte;
  aModuleType : integer);
var i, j, pi, ParamCount : integer;
    sl : TStringList;
begin
  FModuleIndex := aModuleIndex;
  ParamCount := GetDataParamCount( aModuleType);
  sl := TStringList.Create;
  try
    for i := 0 to ParamCount - 1 do begin
      pi := GetDataModuleParamIndex( aModuleIndex, i);

      if ModuleParams[pi].slParamLabel <> '' then begin
        sl.Clear;
        sl.Delimiter := ';';
        sl.StrictDelimiter := True;
        sl.DelimitedText :=  ModuleParams[pi].slParamLabel;
        for j := 0 to sl.Count - 1 do
          AddParamLabel( i, j, sl[j]);
      end;
    end;
  finally
    sl.Free;
  end;
end;

function TParamLabelModuleList.FindParamLabelParam( aParamIndex : integer): TParamLabelParamList;
var i : integer;
begin
  i := 0;
  while (i < Count) and not((Items[i].FIsString = 1) and (Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TParamLabelModuleList.GetModuleLabelsLength: integer;
var i : integer;
begin
  Result := 0;

  for i := 0 to Count - 1 do begin
    if Items[i].FIsString = 1 then begin
      Result := Result + 3 + 7 * Items[i].Count;
    end else
      Result := Result + 3;
  end;
end;

procedure TParamLabelModuleList.AddParamLabel( aParamIndex, aLabelIndex : byte; aName : string);
var ParamLabelParam : TParamLabelParamList;
    ParamLabel : TParamLabel;
begin
  ParamLabelParam := FindParamLabelParam( aParamIndex);
  if not assigned(ParamLabelParam) then begin
    ParamLabelParam := TParamLabelParamList.Create( True, FPatchPart);
    ParamLabelParam.FIsString   := 1;
    ParamLabelParam.FParamLen   := 8;
    ParamLabelParam.FParamIndex := aParamIndex;
    Add( ParamLabelParam);
  end;

  if (aLabelIndex < ParamLabelParam.Count) then begin
    ParamLabelParam.Items[aLabelIndex].SetName( aName);
  end else begin
    ParamLabel := TParamLabel.Create;
    ParamLabel.SetName( aName);
    ParamLabelParam.Add( ParamLabel);
    FModuleLen := GetModuleLabelsLength;
  end;
end;

procedure TParamLabelModuleList.Read( aChunk : TPatchChunk);
var i, j : integer;
    //aParamLen : integer;
    ParamLabelParam : TParamLabelParamList;
    ParamLabel : TParamLabel;
    parlabels_str : string;
    Module : TG2FileModule;
begin
  // FModuleIndex : TBits8;
  // FModuleLen   : byte;
  // FParams      : array of TParamLabelParamList;

  // FIsString   : TBits8; 0 Extra parameter value, 1 : Label
  // FParamLen   : TBits8; if label then label length + parameterindex length else 1
  // FParamIndex : TBits8; if label then parameterindex else extra parameter value
  // FParamNames : array of TParamName;

  // This section is also used to store some extra params for SeqNote
  // extra editor parameters
  // [0, 1, mag, 0, 1, octave]
  // mag   : 0=3-octaves,1=2-octaves,2=1-octave
  // octave: 0-9 (c0-c9)

  //FModuleIndex := aChunk.ReadBits( 8);
  FModuleLen   := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add( Format('%3d %3d', [FModuleIndex, FModuleLen]));
    aChunk.FLogLines.Add( '    Str Len Par Labels');
  end;

  // Parameter labels are updated per Module, so first the list of parameter
  // labes of current module will be cleared
  Clear;

  i := FModuleLen;
  while (i > 0) do begin
    ParamLabelParam := TParamLabelParamList.Create( True, FPatchPart);
    ParamLabelParam.FIsString   := aChunk.ReadBits( 8);
    ParamLabelParam.FParamLen   := aChunk.ReadBits( 8);
    ParamLabelParam.FParamIndex := aChunk.ReadBits( 8);

    if assigned(aChunk.FLogLines) then begin
      parlabels_str := Format('    %3d %3d %3d', [ParamLabelParam.FIsString, ParamLabelParam.FParamLen, ParamLabelParam.FParamIndex]);
    end;

    Add( ParamLabelParam);

    i := i - 3; //bytes

    if ParamLabelParam.FParamLen - 1 > 0 then begin // It's a string
      for j := 0 to ((ParamLabelParam.FParamLen - 1) div 7) - 1 do begin
        ParamLabel := TParamLabel.Create;
        ParamLabel.Read( aChunk);
        ParamLabelParam.Add(ParamLabel);

        i := i - 7; //bytes
      end;
      if assigned(aChunk.FLogLines) then begin
        for j := 0 to ParamLabelParam.Count - 1 do
          parlabels_str := parlabels_str + ' ' + ParamLabelParam.Items[j].GetName;
      end;
    end;
    if assigned(aChunk.FLogLines) then
      aChunk.FLogLines.Add( parlabels_str);
  end;


  if assigned(FPatchPart) then begin
    Module := FPatchPart.FindModule( FModuleIndex);
    if assigned(Module) then begin
      Module.InvalidateParameters;
    end;
  end;
end;


procedure TParamLabelModuleList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FModuleLen := GetModuleLabelsLength;

  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FModuleLen, 8);

  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                             TParameterLabelList
//
//------------------------------------------------------------------------------

constructor TParameterLabelList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

constructor TParameterLabelList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aParameterLabels : TParameterLabelList);
var i : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;

  FLocation       := aParameterLabels.FLocation;
  //FModuleCount    := 0;
  for i := 0 to aParameterLabels.Count - 1 do begin
    Module := aModuleList.FindModule( aParameterLabels.Items[i].FModuleIndex);

    if assigned(Module) {and (Module.FSelected)} then
      Add( TParamLabelModuleList.CopyCreate( True, aPatchPart, aParameterLabels.Items[i]));
  end;
  //FModuleCount := Count;
end;

destructor TParameterLabelList.Destroy;
begin
  inherited;
end;

function TParameterLabelList.GetParameterLabelCount( aModuleIndex,  aParamIndex: byte): integer;
begin
  if aModuleIndex < Count then
    Result := Items[ aModuleIndex].GetParameterLabelCount( aParamIndex)
  else
    Result := 0;
end;

function TParameterLabelList.GetParamLabelModule( aIndex : integer) : TParamLabelModuleList;
begin
  result := TParamLabelModuleList(inherited Items[aindex]);
end;

procedure TParameterLabelList.SetParamLabelModule( aIndex : integer; const aValue : TParamLabelModuleList);
begin
  inherited Items[aindex] := aValue;
end;

function TParameterLabelList.AddParamLabelModule( aValue : TParamLabelModuleList): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TParameterLabelList.AddParamLabels(aModuleIndex, aParamIndex: byte; aNames: string);
var sl : TStringList;
    i : integer;
begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := aNames;
    for i := 0 to sl.Count - 1 do
      AddParamLabel( aModuleIndex, aParamIndex, i, sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TParameterLabelList.Init;
begin
  FLocation       := 0;
  //FModuleCount    := 0;
  Clear;
end;

function TParameterLabelList.FindParamLabelModule( aModuleIndex : integer): TParamLabelModuleList;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);
  if (i < Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TParameterLabelList.FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
var ParamLabelModule : TParamLabelModuleList;
    ParamLabelParam : TParamLabelParamList;
begin
  ParamLabelModule := FindParamLabelModule( aModuleIndex);
  if assigned(ParamLabelModule) then begin
    ParamLabelParam := ParamLabelModule.FindParamLabelParam( aParamIndex);
    if assigned(ParamLabelParam) and (ParamLabelParam.FIsString = 1) then begin
      if aLabelIndex < ParamLabelParam.Count then
        Result := ParamLabelParam.Items[ aLabelIndex].GetName
      else
        Result := ParamLabelParam.Items[0].GetName;
    end else
      Result := '';
  end else
    Result := '';
end;

procedure TParameterLabelList.AddParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte; aName : string);
var ParamLabelModule : TParamLabelModuleList;
begin
  ParamLabelModule := FindParamLabelModule( aModuleIndex);
  if assigned(ParamLabelModule) then
    ParamLabelModule.AddParamLabel( aParamIndex, aLabelIndex, aName)
  else begin
    ParamLabelModule := TParamLabelModuleList.Create( True, FPatchPart);
    ParamLabelModule.FModuleIndex := aModuleIndex;
    ParamLabelModule.AddParamLabel( aParamIndex, aLabelIndex, aName);
    Add( ParamLabelModule);
  end;
  //FModuleCount := Count;
end;

procedure TParameterLabelList.DeleteParamLabel( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    //FModuleCount := Count;
  end;
end;

procedure TParameterLabelList.Read( aChunk : TPatchChunk; aModuleCount : integer);
var i, j, ModuleIndex : integer;
    ParamLabelModule : TParamLabelModuleList;
begin
  // FLocation       : Tbits2;
  // FModuleCount    : TBits8
  // FModules        : array of TParamLabelModuleList;

  //NumModules := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Param labels, count = ' + IntToStr(aModuleCount));
    aChunk.FLogLines.Add('Mod Len');
  end;

  for i := 0 to aModuleCount - 1 do begin
    ModuleIndex := aChunk.ReadBits( 8);
    j := 0;
    while (j<Count) and not(Items[j].FModuleIndex = ModuleIndex) do
      inc(j);

    if j<Count then
      ParamLabelModule := Items[j]
    else begin
      ParamLabelModule := TParamLabelModuleList.Create( True, FPatchPart);
      ParamLabelModule.FModuleIndex := ModuleIndex;
      Add( ParamLabelModule);
    end;
    ParamLabelModule.Read( aChunk);
  end;

  //FModuleCount := Count;
end;

procedure TParameterLabelList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  //FModuleCount := Count;
  aChunk.WriteBits(Count, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                              TModuleLabel
//
//------------------------------------------------------------------------------

constructor TModuleLabel.Copy(aModuleLabel : TModuleLabel);
begin
  FModuleIndex := aModuleLabel.FModuleIndex;
  FName := aModuleLabel.FName;
end;

function TModuleLabel.GetName: string;
begin
  Result := FName;
end;

procedure TModuleLabel.Init;
begin
  FModuleIndex := 0;
  FName := ''
end;

procedure TModuleLabel.Read( aChunk : TPatchChunk);
begin
  // FModuleIndex : TBits8;
  // FName        : AnsiString;

  //FModuleIndex := aChunk.ReadBits( 8);
  FName        := aChunk.ReadName;
end;

procedure TModuleLabel.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TModuleLabel.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteName(FName);
end;

//------------------------------------------------------------------------------
//
//                             TModuleLabelList
//
//------------------------------------------------------------------------------

constructor TModuleLabelList.Create( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart);
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;
end;

destructor TModuleLabelList.Destroy;
begin
  inherited;
end;

constructor TModuleLabelList.CopySelected( AOwnsObjects : boolean; aPatchPart : TG2FilePatchPart; aModuleList : TModuleList; aModuleLabels : TModuleLabelList);
var i : integer;
    Module : TG2FileModule;
begin
  inherited Create( AOwnsObjects);
  FPatchPart := aPatchPart;

  Init;
  FLocation := aModuleLabels.FLocation;
  FUnknown := aModuleLabels.FUnknown;
  FNameCount := 0;
  for i := 0 to aModuleLabels.FNameCount - 1 do begin
    Module := aModuleList.FindModule( aModuleLabels.Items[i].FModuleIndex);

    if assigned(Module) {and ( Module.FSelected)} then begin
      Add( TModuleLabel.Copy( aModuleLabels.Items[i]));
    end;
  end;
  FNameCount := Count;
end;

function TModuleLabelList.GetModuleLabel( aIndex : integer) : TModuleLabel;
begin
  result := TModuleLabel(inherited Items[aindex]);
end;

procedure TModuleLabelList.SetModuleLabel( aIndex : integer; const aValue : TModuleLabel);
begin
  inherited Items[aindex] := aValue;
end;

function TModuleLabelList.AddModuleLabel( aValue : TModuleLabel): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TModuleLabelList.Init;
begin
  FLocation  := 0;
  FUnknown   := 0;
  FNameCount := 0;
  Clear;
end;

function TModuleLabelList.FindModuleLabel(aModuleIndex: Byte): string;
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Result := Items[i].FName
  end else
    Result := '';
end;

procedure TModuleLabelList.AddNewModuleLabel( aModuleIndex : byte; aName : string);
var i : integer;
    ModuleLabel : TModuleLabel;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Items[i].FName := aName;
  end else begin
    ModuleLabel:= TModuleLabel.Create;
    ModuleLabel.FModuleIndex := aModuleIndex;
    ModuleLabel.FName := aName;
    Add( ModuleLabel);
  end;
  FNameCount := Count;
end;

procedure TModuleLabelList.DeleteModuleLabel( aModuleIndex : Byte);
var i : integer;
begin
  i := 0;
  while (i < Count) and (Items[i].FModuleIndex <> aModuleIndex) do
    inc(i);

  if (i < Count) then begin
    Delete(i);
    FNameCount := Count;
  end;
end;

procedure TModuleLabelList.Read( aChunk : TPatchChunk);
var NumNames, i, j, ModuleIndex : integer;
    ModuleLabel : TModuleLabel;
begin
  // FLocation    : TBits2;
  // FUnknown     : TBits6;
  // FNameCount   : TBits8
  // FModuleNames : array of TModuleName;

  //FUnknown     := aChunk.ReadBits( 6);
  NumNames     := aChunk.ReadBits( 8);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('Module labels, count = ' + IntToStr(NumNames));
    aChunk.FLogLines.Add('Mod Label');
  end;

  for i := 0 to NumNames - 1 do begin

    ModuleIndex := aChunk.ReadBits( 8);

    j := 0;
    while (j<Count) and not(Items[j].FModuleIndex = ModuleIndex) do
      inc(j);

    if j<Count then
      ModuleLabel := Items[j]
    else begin
      ModuleLabel := TModuleLabel.Create;
      ModuleLabel.FModuleIndex := ModuleIndex;
      Add( ModuleLabel);
    end;

    ModuleLabel.Read( aChunk);

    if assigned( aChunk.FLogLines) then
      aChunk.FLogLines.Add( Format('%3d', [ModuleLabel.FModuleIndex]) + ' ' + ModuleLabel.FName);

  end;
  FNameCount := Count;
end;

procedure TModuleLabelList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  FNameCount := Count;
  //aChunk.WriteBits({FUnknown}0, 6);
  aChunk.WriteBits(FNameCount, 8);
  for i := 0 to Count - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                             TPatchDescription
//
//------------------------------------------------------------------------------

procedure TPatchDescription.Init;
begin
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;
  FUnknown5 := 0;
  FUnknown6 := 0;
  FUnknown7 := 0;

  FUnknown8         := 0;
  FVoiceCount       := 1;
  FBarPosition      := 600;
  FUnknown9         := 2;
  FRedVisible       := 1;
  FBlueVisible      := 1;
  FYellowVisible    := 1;
  FOrangeVisible    := 1;
  FGreenVisible     := 1;
  FPurpleVisible    := 1;
  FWhiteVisible     := 1;
  FMonoPoly         := 1;
  FActiveVariation  := 0;
  FCategory         := 0;
  FUnknown10        := 0;
end;

procedure TPatchDescription.Read( aChunk : TPatchChunk);
begin
  // FUnknown1        : TArray7;
  // FUnknown2        : TBits5;
  // FVoiceCount      : TBits5;
  // FBarPosition     : TBits14;
  // FUnknown3        : TBits3;
  // FRedVisible      : TBits1;
  // FBlueVisible     : TBits1;
  // FYellowVisible   : TBits1;
  // FOrangeVisible   : TBits1;
  // FGreenVisible    : TBits1;
  // FPurpleVisible   : TBits1;
  // FWhiteVisible    : TBits1;
  // FMonoPoly        : TBits2;
  // FActiveVariation : Byte;
  // FCategory        : Byte;
  //for i := 0 to 6 do
  //  FUnknown1[i] := aChunk.ReadBits( 8);
  FUnknown1        := aChunk.ReadBits( 8);
  FUnknown2        := aChunk.ReadBits( 8);
  FUnknown3        := aChunk.ReadBits( 8);
  FUnknown4        := aChunk.ReadBits( 8);
  FUnknown5        := aChunk.ReadBits( 8);
  FUnknown6        := aChunk.ReadBits( 8);
  FUnknown7        := aChunk.ReadBits( 8);
  FUnknown8        := aChunk.ReadBits( 5);
  FVoiceCount      := aChunk.ReadBits( 5);
  FBarPosition     := aChunk.ReadBits( 14);
  FUnknown9        := aChunk.ReadBits( 3);
  FRedVisible      := aChunk.ReadBits( 1);
  FBlueVisible     := aChunk.ReadBits( 1);
  FYellowVisible   := aChunk.ReadBits( 1);
  FOrangeVisible   := aChunk.ReadBits( 1);
  FGreenVisible    := aChunk.ReadBits( 1);
  FPurpleVisible   := aChunk.ReadBits( 1);
  FWhiteVisible    := aChunk.ReadBits( 1);
  FMonoPoly        := aChunk.ReadBits( 2);
  FActiveVariation := aChunk.ReadBits( 8);
  FCategory        := aChunk.ReadBits( 8);
  FUnknown10       := aChunk.ReadBits( 12); // To byte align again
end;

procedure TPatchDescription.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 5);
  aChunk.WriteBits(FVoiceCount, 5);
  aChunk.WriteBits(FBarPosition, 14);
  aChunk.WriteBits(FUnknown9, 3);
  aChunk.WriteBits(FRedVisible, 1);
  aChunk.WriteBits(FBlueVisible, 1);
  aChunk.WriteBits(FYellowVisible, 1);
  aChunk.WriteBits(FOrangeVisible, 1);
  aChunk.WriteBits(FGreenVisible, 1);
  aChunk.WriteBits(FPurpleVisible, 1);
  aChunk.WriteBits(FWhiteVisible, 1);
  aChunk.WriteBits(FMonoPoly, 2);
  aChunk.WriteBits(FActiveVariation, 8);
  aChunk.WriteBits(FCategory, 8);
  aChunk.WriteBits(FUnknown10, 12); // To byte align again
end;

//------------------------------------------------------------------------------
//
//                               TPatchNotes
//
//------------------------------------------------------------------------------

constructor TPatchNotes.Create;
begin
  SetLength(FText, 0);
end;

destructor TPatchNotes.Destroy;
begin
  Finalize(FText);

  inherited;
end;

procedure TPatchNotes.GetLines(aLines: TStrings);
var i : integer;
begin
  aLines.Text := '';
  for i := 0 to Length(FText) - 1 do
    aLines.Text := aLines.Text + Char(FText[i]);
end;

procedure TPatchNotes.SetLines(aLines: TStrings);
var i : integer;
    b : byte;
    notes : string;
begin
  notes := aLines.Text;
  SetLength(FText, notes.Length);
  for i := 0 to Length(FText) - 1 do begin
    b := byte(notes.Chars[i]);
    FText[i] := b;
  end;
end;

procedure TPatchNotes.Init;
begin
  SetLength(FText, 0);
end;

procedure TPatchNotes.Read( aChunk : TPatchChunk);
begin
  aChunk.FReadBuffer.Read(FText[0], Length(FText));
end;


procedure TPatchNotes.Write( aChunk : TPatchChunk);
begin
  aChunk.FWriteBuffer.Write(FText[0], Length(FText));
end;

function TPatchNotes.GetText: string;
var i : integer;
begin
  Result := '';
  for i := 0 to Length(FText) - 1 do begin
    Result := Result + Char(FText[i]);
  end;
end;

//------------------------------------------------------------------------------
//
//                             TG2FilePatchPart
//
//------------------------------------------------------------------------------

constructor TG2FilePatchPart.Create( aPatch : TG2FilePatch);
begin
  inherited Create( aPatch);

  FPatch := aPatch;
  FSelectedModuleList := TModuleList.Create( False, nil);

  //FSelectedParam := nil;
  FFocusedModule := nil;

  FModuleList := TModuleList.Create( True, self);
  FCableList := TCableList.Create( True,  self);
  FParameterList := TModuleParameterList.Create( True, self);
  FModuleLabelList := TModuleLabelList.Create( True, self);
  FParameterLabelList := TParameterLabelList.Create( True, self);
end;

{constructor TG2FilePatchPart.CopyModules( aPatch : TG2FilePatch; aSrcePatchPart : TG2FilePatchPart; aModuleList : TModuleList);
begin
  inherited Create( aPatch);

  FPatch := aPatch;
  FModuleList := TModuleList.CopySelected( True, self, aModuleList);
  FCableList := TCableList.CopySelected( True, self, aModuleList, aSrcePatchPart.FCableList);
  FParameterList := TModuleParameterList.CopySelected( True, self, aModuleList, aSrcePatchPart.FParameterList);
  FModuleLabelList := TModuleLabelList.CopySelected( True, self, aModuleList, aSrcePatchPart.FModuleLabelList);
  FParameterLabelList := TParameterLabelList.CopySelected( True, self, aModuleList, aSrcePatchPart.FParameterLabelList);

  FSelectedModuleList := TModuleList.Create( False, nil);
end;}

constructor TG2FilePatchPart.CopyModules( aPatch : TG2FilePatch; aSrcePatchPart : TG2FilePatchPart; aModuleList : TModuleList);
var MemStream : TMemoryStream;
    Chunk : TPatchChunk;
begin
  inherited Create( aPatch);

  FPatch := aPatch;
  FFocusedModule := nil;

  FLocation := aSrcePatchPart.Location;

  FModuleList := TModuleList.Create( True, self);
  FCableList := TCableList.Create( True,  self);
  FParameterList := TModuleParameterList.Create( True, self);
  FModuleLabelList := TModuleLabelList.Create( True, self);
  FParameterLabelList := TParameterLabelList.Create( True, self);

  MemStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create( MemStream);
  try
    Chunk.FLogLines := FPatch.FG2.LogLines;
    aSrcePatchPart.Write( Chunk, aSrcePatchPart.VariationCount);
    MemStream.Position := 0;
    Chunk.ReadChunk;
    Read( Chunk);

    FSelectedModuleList := TModuleList.Create( False, nil);
  finally
    Chunk.Free;
    MemStream.Free;
  end;
end;


destructor TG2FilePatchPart.Destroy;
begin
  FParameterLabelList.Free;
  FModuleLabelList.Free;
  FParameterList.Free;
  FCableList.Free;
  FModuleList.Free;
  FreeAndNil(FSelectedModuleList);
  inherited;
end;

procedure TG2FilePatchPart.Init;
begin
  // TODO : Remove params from Global knob list!

  FModuleList.Init;
  FCableList.Init;
  FParameterList.Init;
  FModuleLabelList.Init;
  FParameterLabelList.Init;

  FSelectedModuleList.Clear;
  //FSelectedParam := nil;
  FFocusedModule := nil;
end;

procedure TG2FilePatchPart.InvalidateParameters;
var m : integer;
begin
  for m := 0 to FModuleList.Count - 1 do begin
    FModuleList[m].InvalidateControl;
    FModuleList[m].InvalidateParameters;
  end;
end;

procedure TG2FilePatchPart.Read( aChunk : TPatchChunk);
var einde : boolean;
begin
  einde := False;
  repeat
    if not ReadChunk( aChunk) then begin
      einde := True;

      raise Exception.Create('Error parsing patch data.');
    end else
      if aChunk.FId = C_MODULE_NAMES then
        einde := true; // assuming this is always the last chunk in a patch part

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;
  until einde;
end;

function TG2FilePatchPart.ReadChunk(aChunk: TPatchChunk): boolean;
var ModuleCount : byte;
begin
  Result := True;
  case aChunk.FId of
  C_MODULE_LIST :     // $4a
        begin // Module list
          FModuleList.Read( aChunk);
        end ;
  C_CABLE_LIST :      // $52
        begin // Cable list
          FCableList.Read( aChunk)
        end ;
  C_PARAM_LIST :      // $4d
        begin // Parameter list
          FParameterList.Read( aChunk);
        end ;
  C_PARAM_NAMES :     // $5b
        begin // Parmeter names
          ModuleCount := aChunk.ReadBits( 8);
          FParameterLabelList.Read( aChunk, ModuleCount);
        end ;
  C_MODULE_NAMES :    // $5a
        begin // Module names
          FModuleLabelList.Read( aChunk);
          FPatch.FG2.dump_buffer( (aChunk.FStream as TMemoryStream).Memory^, aChunk.FStream.Size);
        end ;
    else begin
      Result := False;
    end;
  end;
end;

procedure TG2FilePatchPart.Write(aChunk: TPatchChunk; aVariationCount: byte);
begin
  FModuleList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_LIST);

  FCableList.Write( aChunk);
  aChunk.WriteChunk( C_CABLE_LIST);

  FParameterList.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  FParameterLabelList.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  FModuleLabelList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_NAMES);

  FPatch.FG2.dump_buffer( (aChunk.FStream as TMemoryStream).Memory^, aChunk.FStream.Size);
end;

procedure TG2FilePatchPart.InvalidateConnectors;
var m : integer;
begin
  for m := 0 to FModuleList.Count - 1 do
    FModuleList[m].InvalidateConnectors;
end;

function TG2FilePatchPart.GetSelectedModuleList: TModuleList;
begin
  Result := FSelectedModuleList;
end;

function TG2FilePatchPart.GetSelectedParam: TG2FileParameter;
begin
  Result := nil;
  if assigned(FFocusedModule) then
    Result := FFocusedModule.SelectedParam;
end;

function TG2FilePatchPart.GetUniqueModuleNameSeqNr( aModuleFileName: string): integer;
var m : integer;
    found : boolean;
begin
  found := false;
  Result := 1;
  repeat
    m := 0;
    while (m < FModuleList.Count) and (FModuleList[m].ModuleName <> (aModuleFileName + IntToStr(Result))) do
      inc(m);

    found := m < FModuleList.Count;
    if found then begin
      inc(Result);
    end;
  until not found;
end;

function TG2FilePatchPart.GetVariationCount: integer;
begin
  if assigned(FPatch) then
    Result := FPatch.VariationCount
  else
    raise Exception.Create('Patch not assigned, cannot determine VariationCount'); // TODO Temporary
end;

procedure TG2FilePatchPart.SetFocusedModule(const aModule: TG2FileModule);
var OldFocusedModule : TG2FileModule;
begin
  if aModule <> FFocusedModule then begin
    OldFocusedModule := FFocusedModule;
    FFocusedModule := aModule;

    if assigned(aModule) then
      aModule.SelectModule( True, True);

    if assigned(OldFocusedModule) then begin
      OldFocusedModule.InvalidateControl;
      OldFocusedModule.InvalidateParameters;
    end;

    if assigned(FFocusedModule) then begin
      FFocusedModule.InvalidateControl;
      FFocusedModule.InvalidateParameters;
    end;

    if assigned(FPatch) then
      FPatch.DoFocusModule;
  end;
end;

procedure TG2FilePatchPart.SetLocation( aValue : TLocationType);
begin
  FLocation := aValue;

  FModuleList.FLocation := ord(FLocation);
  FCableList.FLocation   := ord(FLocation);
  FParameterList.FLocation := ord(FLocation);
  FModuleLabelList.FLocation := ord(FLocation);
  FParameterLabelList.FLocation := ord(FLocation);
end;

procedure TG2FilePatchPart.DeleteModule( aModuleIndex : integer);
begin
  if assigned(FocusedModule) and (FocusedModule.ModuleIndex = aModuleIndex) then
    FocusedModule := nil;

  FParameterLabelList.DeleteParamLabel( aModuleIndex);
  FModuleLabelList.DeleteModuleLabel( aModuleIndex);
  FParameterList.DeleteModule( aModuleIndex);
  FModuleList.DeleteModule( aModuleIndex);
end;

procedure TG2FilePatchPart.AddCable( aCable : TG2FileCable);
begin
  FCableList.AddCable( aCable);
end;

procedure TG2FilePatchPart.DeleteCable( aCable : TG2FileCable);
begin
  FCableList.DeleteCable( aCable);
end;

function TG2FilePatchPart.GetMaxModuleIndex : integer;
begin
  Result := FModuleList.GetMaxModuleIndex;
end;

function TG2FilePatchPart.GetNoOffModuleType( aModuleType : byte): integer;
begin
  Result := FModuleList.GetNoOffModuleType( aModuleType);
end;

function TG2FilePatchPart.FindModuleLabel( aModuleIndex : byte): string;
begin
  Result := FModuleLabelList.FindModuleLabel( aModuleIndex);
end;

function TG2FilePatchPart.FindParamValue( aModuleIndex, aVariation, aParamIndex : byte): integer;
begin
  Result := FParameterList.FindParamValue( aModuleIndex, aVariation, aParamIndex);
end;

function TG2FilePatchPart.FindModuleVariationList( aModuleIndex: byte): TModuleVariationList;
begin
  Result := FParameterList.FindModuleVariationList( aModuleIndex);
end;

function TG2FilePatchPart.FindModule( aModuleIndex : byte): TG2FileModule;
begin
  Result := FModuleList.FindModule( aModuleIndex);
end;

{procedure TG2FilePatchPart.AddNewVariation( aVariation : byte);
begin
  FParameterList.AddNewVariation( aVariation);
end;}

procedure TG2FilePatchPart.CopyVariation( aFromVariation, aToVariation : byte);
begin
  FParameterList.CopyVariation( aFromVariation, aToVariation);
end;

function TG2FilePatchPart.FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
begin
  Result := FParameterLabelList.FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex);
end;

procedure TG2FilePatchPart.AddParamLabel(aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
begin
  FParameterLabelList.AddParamLabel(aModuleIndex, aParamIndex, aLabelIndex, aValue);
end;

procedure TG2FilePatchPart.AddNewModuleLabel(aModuleIndex : byte; aValue : string);
begin
  FModuleLabelList.AddNewModuleLabel( aModuleIndex, aValue);
end;

function TG2FilePatchPart.GetParameterLabel( aModuleIndex, aParamIndex, aLabelIndex : byte): string;
begin
  Result := FindParamLabel( aModuleIndex, aParamIndex, aLabelIndex)
end;

function TG2FilePatchPart.GetParameterLabelCount( aModuleIndex,  aParamIndex: byte): integer;
begin
  Result := FParameterLabelList.GetParameterLabelCount( aModuleIndex, aParamIndex);
end;

procedure TG2FilePatchPart.SetParameterLabel( aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
var Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  AddParamLabel(aModuleIndex, aParamIndex, aLabelIndex, aValue);

  Module := FindModule( aModuleIndex);
  if assigned(Module) then begin
    Param := Module.Parameter[ aParamIndex];
    if assigned(Param) then
      Param.InvalidateControl;
  end;
end;

function TG2FilePatchPart.GetModuleLabel( aModuleIndex : byte): string;
var Module : TG2FileModule;
begin
  Result := FindModuleLabel( aModuleIndex);
  if Result = '' then begin
    Module := FindModule( aModuleIndex);
    if Module <> nil then
      Result := Module.ModuleName
    else
      Result := '';
  end;
end;

function TG2FilePatchPart.GetMorphValue(aModuleIndex, aParamIndex, aMorphIndex,
  aVariation: byte): byte;
var MorphParameter : TMorphParameter;
begin
  Result := 0;
  MorphParameter := FPatch.GetMorph( FLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
  if assigned(MorphParameter) then
    Result := MorphParameter.Range;
end;

procedure TG2FilePatchPart.SetModuleLabel( aModuleIndex : byte; aValue : string);
var Module : TG2FileModule;
begin
  AddNewModuleLabel( aModuleIndex, aValue);

  Module := FindModule( aModuleIndex);
  if assigned(Module) then
    Module.InvalidateControl;
end;

procedure TG2FilePatchPart.AddModuleToSelection(aModule: TG2FileModule);
begin
  if not assigned(aModule) then
    exit;

  if SelectedModuleList.FindModule( aModule.ModuleIndex) = nil then
    SelectedModuleList.AddModule( aModule);

  {if (SelectedModuleList.Count = 1) and (aModule.ParameterCount > 0) then begin
    if (aModule.FSelectedParam = -1) then
      aModule.FSelectedParam := 0;
    if aModule.FSelectedParam >= aModule.ParameterCount then
      aModule.FSelectedParam := 0;

    aModule.Parameter[ aModule.FSelectedParam].Selected := True;
  end;}
end;

procedure TG2FilePatchPart.RemoveModuleFromSelection(aModule: TG2FileModule);
//var Param : TG2FileParameter;
begin
  //Param := aModule.GetSelectedParam;
  //if Param = FSelectedParam then
  //  FSelectedParam := nil;
  FSelectedModuleList.DeleteModule( aModule.ModuleIndex);
end;

procedure TG2FilePatchPart.SelectAll;
var i : integer;
begin
  for i := 0 to FModuleList.Count - 1 do
    if not FModuleList.Items[i].Selected then
      FModuleList.Items[i].SelectModule( True, False);
end;

procedure TG2FilePatchPart.UnselectModules;
var i : integer;
begin
  for i := 0 to FModuleList.Count - 1 do
    if FModuleList.Items[i].Selected then
      FModuleList.Items[i].SelectModule( False, True);
end;

{procedure TG2FilePatchPart.FocusModuleAbove;
var Module : TG2FileModule;
begin
  if FFocusedModule = nil then
    exit;

  Module := FModuleList.GetModuleAbove( FFocusedModule.FModuleIndex);
  if assigned(Module) then begin
    //UnSelectModules;
    //Module.Selected := True;
    FocusedModule := Module;
  end;
end;

procedure TG2FilePatchPart.FocusModuleUnder;
var Module : TG2FileModule;
begin
  if FFocusedModule = nil then
    exit;

  Module := FModuleList.GetModuleUnder( FFocusedModule.FModuleIndex);
  if assigned(Module) then begin
    //UnSelectModules;
    //Module.Selected := True;
    FocusedModule := Module;
  end;
end;

procedure TG2FilePatchPart.FocusModuleLeft;
var Module : TG2FileModule;
begin
  if FFocusedModule = nil then
    exit;

  Module := FModuleList.GetModuleLeft( FFocusedModule.FModuleIndex);
  if assigned(Module) then begin
    //UnSelectModules;
    //Module.Selected := True;
    FocusedModule := Module;
  end;
end;

procedure TG2FilePatchPart.FocusModuleRight;
var Module : TG2FileModule;
begin
  if FFocusedModule = nil then
    exit;

  Module := FModuleList.GetModuleRight( FFocusedModule.FModuleIndex);
  if assigned(Module) then begin
    //UnSelectModules;
    //Module.Selected := True;
    FocusedModule := Module;
  end;
end;}

{procedure TG2FilePatchPart.SetSelectedParam(aValue: TG2FileParameter);
var OldSelectedParam : TG2FileParameter;
begin
  OldSelectedParam := SelectedParam;
  FSelectedParam := aValue;
  if assigned(SelectedParam) then
    SelectedParam.InvalidateControl;

  if assigned(OldSelectedParam) then
    OldSelectedParam.InvalidateControl;
end;}

procedure TG2FilePatchPart.SelectNextModuleParam;
//var Module : TG2FileModule;
begin
  //if FSelectedParam = nil then
  //  exit;

  //Module := FModuleList.FindModule( FSelectedParam.FModuleIndex);
  //if assigned(Module) then begin
  //  Module.SelectNextParam;
  //end;

  if assigned(FFocusedModule) then
    FFocusedModule.SelectNextParam;
end;

procedure TG2FilePatchPart.SelectPrevModuleParam;
//var Module : TG2FileModule;
begin
  //if FSelectedParam = nil then
  //  exit;

  //Module := FModuleList.FindModule( FSelectedParam.FModuleIndex);
  //if assigned(Module) then begin
  //  Module.SelectPrevParam;
  //end;

  if assigned(FFocusedModule) then
    FFocusedModule.SelectPrevParam;
end;

function TG2FilePatchPart.CreateModule( aModuleIndex : byte; aModuleType : byte): TG2FileModule;
begin
  if assigned(FPatch) then
    Result := FPatch.CreateModule( FLocation, aModuleIndex, aModuleType)
  else
    raise Exception.Create('Patch not assigned to patchpart');
end;

{function TG2FilePatchPart.CreateModuleFromDef( aModuleIndex : byte;
  aModuleType : integer): TG2FileModule;
begin
  Result := TG2FileModule.Create(self);
  Result.ModuleIndex := 0;
  Result.Location := Location;
  Result.TypeID := aModuleType;
  Result.InitModule( Location, aModuleType);
end;}

{procedure TG2FilePatchPart.AddModuleFromDef( aModuleIndex : byte;
  aModuleType : integer);
var Module : TG2FileModule;
    ModuleVariationList : TModuleVariationList;
    ParamLabelModuleList : TParamLabelModuleList;
begin
  // Creates te module data directly in the patchpart, without messaging. For
  // creation in copy buffer

  Module := CreateModuleFromDef( aModuleIndex, aModuleType);
  ModuleList.AddModule(Module);

  ModuleVariationList := TModuleVariationList.Create(True, self);
  //ParameterList.VariationCount := N_VARIATIONS;
  ModuleVariationList.InitModuleVariationList( Module.ModuleIndex, aModuleType);
  ParameterList.AddModuleVariationList( ModuleVariationList);

  ParamLabelModuleList := TParamLabelModuleList.Create(True, self);
  ParamLabelModuleList.InitParamLabelModuleList( Module.ModuleIndex, aModuleType);
  ParameterLabelList.AddParamLabelModule( ParamLabelModuleList);

  // TODO Modulelabel!
end;}

function TG2FilePatchPart.CreateCable( aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
begin
  if assigned(FPatch) then
    Result := FPatch.CreateCable( FLocation, aColor, aFromModule, aFromConnector, aLinkType, aToModule, aToConnector)
  else
    raise Exception.Create('Patch not assigned to patchpart');
end;

//------------------------------------------------------------------------------
//
//                                TG2FilePatch
//
//------------------------------------------------------------------------------

constructor TG2FilePatch.Create( AOwner: TComponent);
var i : integer;
begin
  inherited Create( AOwner);

  FG2 := nil;

  FObserverList := TList<IG2Observer>.Create;

  //FMaxVariations := 9; // The amount of variations in the patch: file 9, usb 10!

  SetLength(FPatchPartArray, 3);
  for i := 0 to 2 do // FX, VA and Patch
    FPatchPartArray[i] := CreatePatchPart(TLocationType(i));

  FCurrentNoteList := TCurrentNoteList.Create( True, self);
  FPatchSettings := TPatchSettings.Create( self);
  FPatchDescription := TPatchDescription.Create;
  FMorphParameters := TMorphParameters.Create( self);
  FKnobList := TKnobList.Create( True, self);
  FControllerList := TControllerList.Create( True, self);
  //FMorphNames := TMorphNames.Create( self);
  FPatchNotes := TPatchNotes.Create;

  //FSelectedModuleList := TModuleList.Create( False, nil);

  FLedComparison := TComparer<IG2DataLed>.Construct(
    function (const Led1, Led2 : IG2DataLed): integer
    begin
      if Led1.Location < Led2.Location then
        Result := 1
      else
        if Led1.Location = Led2.Location then begin
          if Led1.ModuleIndex > Led2.ModuleIndex then
            Result := 1
          else begin
            if Led1.ModuleIndex = Led2.ModuleIndex then begin
              if Led1.GroupId > Led2.GroupID then
                Result := 1
              else begin
                if Led1.GroupId = Led2.GroupID then
                  Result := 0
                else
                  Result := -1;
              end;
            end else
              Result := -1;
          end;
        end else
          Result := -1;
    end);

  FLedList := TList<IG2DataLed>.Create(FLedComparison);
  FLedStripList := TList<IG2DataLed>.Create(FLedComparison);

  Init;
end;

constructor TG2FilePatch.CopyModules( AOwner: TComponent; aPatch : TG2FilePatch; aModuleList : TModuleList);
var i : integer;
begin
  inherited Create( AOwner);

  //FMaxVariations := aPatch.FMaxVariations;

  //FSelectedModuleList := TModuleList.Create( False, nil);

  SetLength(FPatchPartArray, 3);
  for i := 0 to 2 do
    FPatchPartArray[i] := TG2FilePatchPart.CopyModules( self, aPatch.FPatchPartArray[i],  aModuleList);

  FCurrentNoteList := TCurrentNoteList.Create( True, self);
  FPatchSettings := TPatchSettings.Create( self);
  FPatchDescription := TPatchDescription.Create;
  FMorphParameters := TMorphParameters.Create( self);
  FKnobList := TKnobList.Create( True, self);
  FControllerList := TControllerList.Create( True, self);
  //FMorphNames := TMorphNames.Create( self);
  FPatchNotes := TPatchNotes.Create;

end;

destructor TG2FilePatch.Destroy;
begin
  //Finalize(FParamArray);

  FLedList.Free;
  FLedStripList.Free;


  FPatchNotes.Free;
  //FMorphNames.Free;
  FControllerList.Free;
  FKnobList.Free;
  FPatchDescription.Free;
  FPatchSettings.Free;
  //FMorphParameters.Free;
  FCurrentNoteList.Free;
  //for i := 0 to 1 do
  //  FPatchPart[i].Free;
  Finalize(FPatchPartArray);

  //FSelectedModuleList.Free;

  FObserverList.Free;

  inherited;
end;

procedure TG2FilePatch.DoLabelValueChange(aParam: TG2FileParameter);
begin
  NotifyObservers( EvtLabelValueChange);

  if assigned(aParam) then begin
    if (aParam.LabelOnValue) and assigned(G2.OnLabelValueChange) then
      G2.OnLabelValueChange( self, FG2.ID, aParam);

    aParam.InvalidateControl;
  end;
end;

procedure TG2FilePatch.DoMorphChange(Slot: byte; Location: TLocationType;
  ModuleIndex, ParamIndex, aMorphIndex, aVariationIndex, aValue: byte);
begin
  NotifyObservers( EvtMorphChange);

  if assigned(G2) then
    if assigned(G2.FOnMorphChange) then
      G2.FOnMorphChange( G2, G2.ID, Slot, TLocationType(Location), ModuleIndex,
          ParamIndex, aMorphIndex, aVariationIndex, aValue);
end;

procedure TG2FilePatch.DoSelectLocation;
begin
  NotifyObservers( EvtSelectLocation);

  if assigned(G2) then begin
    if assigned(G2.FOnSelectLocation) then
      G2.FOnSelectLocation( G2, G2.ID, Slot.SlotIndex, SelectedLocation);
  end;
end;

procedure TG2FilePatch.DoFocusModule;
begin
  NotifyObservers( EvtSelectModule);

  if assigned(G2) and assigned(G2.OnSelectModule) then
    G2.OnSelectModule( G2, G2.ID, PatchPart[ord(SelectedLocation)].FocusedModule);
end;

procedure TG2FilePatch.DoSelectParam;
begin
  NotifyObservers( EvtSelectParam);

  if assigned(G2) and assigned(G2.OnSelectParam) then begin
    G2.OnSelectParam( G2, G2.ID, PatchPart[ord(SelectedLocation)].SelectedParam);
  end;
end;

procedure TG2FilePatch.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2FilePatch.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

procedure TG2FilePatch.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event);
end;

procedure TG2FilePatch.Init;
var i : integer;
begin
  FPatchVersion := PATCH_VERSION;
  FPatchType := 0;

  if assigned(FPerformance) and assigned(FSlot) then begin
    for i := 0 to FPerformance.FGlobalKnobList.Count - 1 do
      if FPerformance.FGlobalKnobList[i].SlotIndex = FSlot.SlotIndex then begin
        FPerformance.FGlobalKnobList[i].Init;
      end;
  end;

  FPatchDescription.Init;

  for i := 0 to 2 do begin
    FPatchPartArray[i].Init;
    FPatchPartArray[i].Location := TLocationType(i);
  end;

  FCurrentNoteList.Init;

  FPatchSettings.Init;
  FPatchSettings.FLocation := 2;

  FMorphParameters.Init;

  FKnobList.Init;

  FControllerList.Init;

  //FMorphNames.Init;
  //FMorphNames.FLocation := 2;

  // Init morph labels
  for i := 0 to 7 do
    PatchPart[ord(ltPatch)].ParameterLabelList.AddParamLabel( PATCH_MORPH, 8+i, 0, STD_MORPH_NAMES[i]);

  FPatchNotes.Init;

  //for i := 0 to Length(FParamArray) - 1 do
  //  FParamArray[i].Free;
  //SetLength(FParamArray, 0);

  InitParameters;
  //FSelectedModuleList.Clear;

  FLedList.Clear;
  FLedStripList.Clear;

  FEditAllVariations := False;
  //FSelectedParam := nil;
  FSelectedParamPage := 0;
  FSelectedParamPageColumn := 0;
  FSelectedLocation := ltVA;
end;


procedure TG2FilePatch.SetPatchName( aValue : string);
begin
  if assigned(FSlot) then
    FSlot.FPatchName := aValue;
end;

procedure TG2FilePatch.SelectParamInPatch(aLocation: TLocationType; aModuleIndex,
  aParameterIndex: byte);
var Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  {case aLocation of
  ltVA, ltFX, ltPatch :
    begin
      Module := Modules[ ord(aLocation), aModuleIndex];
      if assigned(Module) then begin
        Param := Module.Parameter[ aParameterIndex];
        if assigned(Param) then
          Param.Selected := True;
      end;
    end;
  //ltPatch :
  //  begin
  //     Param := Parameter[ ModuleIndex, ParamIndex];
  //     if assigned(Param) then
  //       Param.Selected := True;
  //  end;
  end;}


  Module := nil;
  SelectedLocation := aLocation;

  Module := PatchPart[ ord(aLocation)].FindModule( aModuleIndex);
  if assigned(Module) then begin

    //PatchPart[ ord(aLocation)].UnSelectModules;
    //Module.Selected := True;

    Param := Module.Parameter[ aParameterIndex];

    if assigned(Param) then begin
      Module.SelectedParam := Param;
      //if PatchPart[ ord(aLocation)].SelectedParam <> Param then
      //   PatchPart[ ord(aLocation)].SelectedParam := Param;
    end;

    PatchPart[ ord(aLocation)].FocusedModule := Module;
  end;

  DoSelectParam;
end;

procedure TG2FilePatch.SelectParam(aLocation: TLocationType; aModuleIndex,
  aParameterIndex: byte);
begin
  SelectParamInPatch( aLocation, aModuleIndex, aParameterIndex);
end;

procedure TG2FilePatch.SetG2( aValue : TG2File);
begin
  FG2 := aValue;
end;

procedure TG2FilePatch.SetPerformance( aValue : TG2FilePerformance);
begin
  FPerformance := aValue;
end;

procedure TG2FilePatch.SetSlot( aValue : TG2FileSlot);
begin
  FSlot := aValue;
end;

{procedure TG2FilePatch.SetSelectedParam(aValue: TG2FileParameter);
var OldSelectedParam : TG2FileParameter;
begin
  OldSelectedParam := SelectedParam;
  FSelectedParam := aValue;
  if assigned(SelectedParam) then
    SelectedParam.InvalidateControl;

  if assigned(OldSelectedParam) then
    OldSelectedParam.InvalidateControl;
end;}

procedure TG2FilePatch.InitKnobs;
var i : integer;
    Knob : TKnob;
    Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  for i := 0 to FKnobList.Count - 1 do begin
    Knob := FKnobList.Items[i];
    if Knob.Fassigned = 1 then begin
      case TLocationType(Knob.FLocation) of
      {ltPatch : begin
                  Param := Parameter[ Knob.ModuleIndex, Knob.FParamIndex];
                  if assigned(Param) then
                    Param.AssignKnob( i);
                end;}
      ltPatch,
      ltFX,
      ltVA    : begin
                  Module := GetModule( Knob.FLocation, Knob.FModuleIndex);
                  if assigned(Module) then begin
                    Param := Module.Parameter[ Knob.FParamIndex];
                    if assigned(Param) then
                      Param.AssignKnob( i);
                  end;
                end;
      end;
    end;
  end;
end;

procedure TG2FilePatch.InitNames;
var m, l, Count : integer;
    aName : string;
    Module : TG2FileModule;
begin
  for l := 0 to 1 do begin
    Count := ModuleCount[ l];
    for m := 0 to Count - 1 do begin
      Module := Modules[ l, m];
      if assigned(Module) then begin
        aName := GetModuleName( TLocationType(l), Module.ModuleIndex);
        if aName <> '' then
          Module.FModuleName := aName;
      end;
    end;
  end;
end;

procedure TG2FilePatch.InitParameters;

  {procedure AddParam( ParamType : TParamType; ModuleIndex : byte; ModuleName : string; ParamIndex : byte; ParamName : string; LowValue, HighValue, DefaultValue: byte; ButtonText : string; InfoFunc : integer);
  begin
    i := Length(FParamArray);
    SetLength(FParamArray, i + 1);
    FParamArray[i] := CreateParameter( ModuleIndex);
    FParamArray[i].InitParam( 0, ModuleName, ParamIndex, ParamType, ParamName, '', LowValue, HighValue, DefaultValue, -1, -1, ButtonText);
    FParamArray[i].InfoFunctionIndex := InfoFunc;
  end;}

  procedure AddParam( ParamType : TParamType; ModuleIndex : byte; ModuleName : string;
      ParamIndex : byte; ParamName, DefaultLabel : string; LowValue, HighValue, DefaultValue: byte;
      ButtonText : string; InfoFunc : integer);
  var Module : TG2FileModule;
      Param : TG2FileParameter;
  begin
    Module := PatchPart[ord(ltPatch)].FindModule( ModuleIndex);
    if not assigned(Module) then begin
      Module := CreateModule( ltPatch, ModuleIndex, ModuleIndex);
      AddModuleToPatch( ltPatch, Module);
      Module.ModuleName := ModuleName;
    end;

    Param := Module.CreateParameter;
    if Length(Module.FParamArray) < ParamIndex + 1 then
      SetLength(Module.FParamArray, ParamIndex + 1);
    Module.FParamArray[ ParamIndex] := Param;

    Param.InitParam( 0, ModuleName, ParamIndex, ParamType, ParamName, DefaultLabel, LowValue, HighValue, DefaultValue, -1, -1, ButtonText);
    Param.InfoFunctionIndex := InfoFunc;
  end;

begin
  {for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].Free;
  SetLength(FParamArray, 0);}

  AddParam( ptParam, PATCH_VOLUME,      'Vol',      VOLUME_LEVEL,  'VolLevel',   '', 0, 127, 100, '', 515);
  AddParam( ptParam, PATCH_VOLUME,      'Vol',      VOLUME_MUTE,   'VolMute',    '', 0,   1,   0, 'Off;On', 514);
  AddParam( ptParam, PATCH_GLIDE,       'Glide',    GLIDE_TYPE,    'GlideType',  '', 0,   2,   0, 'Auto;Normal;Off', 512);
  AddParam( ptParam, PATCH_GLIDE,       'Glide',    GLIDE_SPEED,   'GlideSpeed', '', 0, 127,   0, '', 511);
  AddParam( ptParam, PATCH_BEND,        'Bend',     BEND_ON_OFF,   'BendOnOff',  '', 0,   1,   0, 'Off;On', 514);
  AddParam( ptParam, PATCH_BEND,        'Bend',     BEND_RANGE,    'BendRange',  '', 0,  23,   0, '', 513);
  AddParam( ptParam, PATCH_VIBRATO,     'Vibrato',  VIBRATO_MOD,   'VibrMod',    '', 0,   2,   0, 'Wheel;AfTouch;Off', 510);
  AddParam( ptParam, PATCH_VIBRATO,     'Vibrato',  VIBRATO_DEPTH, 'VibrDepth',  '', 0, 127,   0, '', 509);
  AddParam( ptParam, PATCH_VIBRATO,     'Vibrato',  VIBRATO_RATE,  'VibrRate',   '', 0, 127,   0, '', 0);
  AddParam( ptParam, PATCH_ARPEGGIATOR, 'Arp',      ARP_ON_OFF,    'ArpOnOff',   '', 0,   1,   0, 'Off;On', 506);
  AddParam( ptParam, PATCH_ARPEGGIATOR, 'Arp',      ARP_SPEED,     'ArpSpeed',   '', 0,   3,   0, '1/8;1/8T;1/16;1/16T', 505);
  AddParam( ptParam, PATCH_ARPEGGIATOR, 'Arp',      ARP_DIRECTION, 'ArpDir',     '', 0,   3,   0, 'Up;Dn;UpDn;Rnd', 507);
  AddParam( ptParam, PATCH_ARPEGGIATOR, 'Arp',      ARP_OCTAVES,   'ArpOct',     '', 0,   3,   0, '1;2;3;4', 508);
  AddParam( ptParam, PATCH_SUSTAIN,     'Sustain',  SUSTAIN_PEDAL, 'Sustain',    '', 0,   1,   0, 'Off;On', 517);
  AddParam( ptParam, PATCH_SUSTAIN,     'Oct Shft', OCTAVE_SHIFT,  'Oct.Shft',   '', 0,   0,   0, '-2;-1;0;1;2', 518);

  AddParam( ptParam, PATCH_MORPH,       'Morph',    0,             'Wheel', 'Wheel',     0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    1,             'Vel', 'Vel',         0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    2,             'Keyb', 'Keyb',       0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    3,             'Aft.Tch', 'Aft.Tch', 0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    4,             'Sust.Pd', 'Sust.Pd', 0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    5,             'Ctrl.Pd', 'Ctrl.Pd', 0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    6,             'P.Stick', 'P.Stick', 0,   127,   0, '', 179);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    7,             'G.Wh2', 'G.Wh2',     0,   127,   0, '', 179);

  AddParam( ptParam, PATCH_MORPH,       'Morph',    8,             'Wheel',      '', 0,   1,   0, 'Knob;Wheel', 519);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    9,             'Vel',        '', 0,   1,   0, 'Knob;Vel', 520);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    10,            'Keyb',       '', 0,   1,   0, 'Knob;Keyb', 521);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    11,            'Aft.Tch',    '', 0,   1,   0, 'Knob;Aft.Tch', 522);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    12,            'Sust.Pd',    '', 0,   2,   0, 'Knob;Sust.Pd;G Wh 1', 523);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    13,            'Ctrl.Pd',    '', 0,   1,   0, 'Knob;Ctrl.Pd', 524);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    14,            'P.Stick',    '', 0,   1,   0, 'Knob;P.Stick', 525);
  AddParam( ptParam, PATCH_MORPH,       'Morph',    15,            'G.Wh2',      '', 0,   1,   0, 'Knob;G.Wh2', 526);

  // "Virtual" paramaters for use in parameter pages
  AddParam( ptMasterClock, PATCH_MASTERCLOCK, 'Mast Clk',  0,      'M.Clk',    '', 30,  240,  0, '', 501);
  AddParam( ptMasterClock, PATCH_MASTERCLOCK, 'Clk Run',   1,      'Clk.Rn',   '', 0,   1,    0, 'Off;On', 502);

  AddParam( ptVoiceMode, PATCH_VOICES,      'Voice Cnt',  0,       'Voices',     '', 0,   31,   0, '', 503);
  AddParam( ptVoiceMode, PATCH_VOICES,      'Voice Mod',  1,       'Vce.Mde',    '', 0,   2,    0, 'Poly;Mono;Legato', 504);

  // Module params should already be initialized;
  InitKnobs;
end;

function TG2FilePatch.GetMorphKnobParameter( aIndex: integer): TG2FileParameter;
begin
  Result := nil;
  case aIndex of
  0 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[0];
  1 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[1];
  2 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[2];
  3 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[3];
  4 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[4];
  5 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[5];
  6 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[6];
  7 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[7];
  8 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[8];
  9 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[9];
  10 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[10];
  11 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[11];
  12 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[12];
  13 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[13];
  14 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[14];
  15 : Result := Modules[ord(ltPatch), PATCH_MORPH].Parameter[15];
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;

function TG2FilePatch.GetPatchKnobParameter(  aIndex: integer): TG2FileParameter;
begin
  Result := nil;
  case aIndex of
  0 : Result := Modules[ord(ltPatch), PATCH_MASTERCLOCK].Parameter[0];
  1 : Result := Modules[ord(ltPatch), PATCH_VOICES].Parameter[0];
  2 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_SPEED];
  3 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_DIRECTION];
  4 : Result := Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_DEPTH];
  5 : Result := Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_SPEED];
  6 : Result := Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_RANGE];
  7 : Result := Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_LEVEL];
  8 : Result := Modules[ord(ltPatch), PATCH_MASTERCLOCK].Parameter[1];
  9 : Result := Modules[ord(ltPatch), PATCH_VOICES].Parameter[1];
  10 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_ON_OFF];
  11 : Result := Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_OCTAVES];
  12 : Result := Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_MOD];
  13 : Result := Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_TYPE];
  14 : Result := Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_ON_OFF];
  15 : Result := Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_MUTE];
  else
    raise Exception.Create('Knob index out of range.');
  end;
end;


procedure TG2FilePatch.InvalidateParameters;
var i : integer;
begin
  for i := 0 to 2 do begin
    FPatchPartArray[i].InvalidateParameters;
  end;
  {for i := 0 to Length(FParamArray) - 1 do
    FParamArray[i].InvalidateControl;}
end;

procedure TG2FilePatch.InvalidateConnectors;
var i : integer;
begin
  for i := 0 to 1 do begin
    FPatchPartArray[i].InvalidateConnectors;
  end;
end;

procedure TG2FilePatch.Read( aChunk : TPatchChunk);
var einde : boolean;
begin
  einde := False;
  repeat
    if not ReadChunk( aChunk) then begin
      einde := True;

      raise Exception.Create('Error parsing patch data.');
    end else
      if aChunk.FId = C_PATCH_NOTES then
        einde := true; // assuming this is always the last chunk in a patch

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;
  until einde;

  InitParameters;
  InitNames;
  //SortLeds;
end;

function TG2FilePatch.ReadChunk( aChunk : TPatchChunk): boolean;
var  Location, ModuleCount, dummy : byte;
begin
  Result := True;
  case aChunk.FId of
  C_MODULE_LIST :     // $4a
        begin // Module list
          location := aChunk.ReadBits( 2);
          FPatchPartArray[ location].FModuleList.Read( aChunk);
        end ;
  C_CABLE_LIST :      // $52
        begin // Cable list
          location := aChunk.ReadBits( 2);
          FPatchPartArray[ location].FCableList.Read( aChunk)
        end ;
  C_MODULE_NAMES :    // $5a
        begin // Module names
          location := aChunk.ReadBits( 2);
          dummy := aChunk.ReadBits( 6);
          FPatchPartArray[ location].FModuleLabelList.Read( aChunk);
        end ;
  C_PARAM_LIST :      // $4d
        begin // Parameter list
          location := aChunk.ReadBits( 2);
          case location of
          0..1 : FPatchPartArray[ location].FParameterList.Read( aChunk);
          2    : FPatchsettings.read( aChunk);
          end;
        end ;
  C_PARAM_NAMES :     // $5b
        begin // Parmeter names
          location := aChunk.ReadBits( 2);
          ModuleCount := aChunk.ReadBits( 8);
          case location of
          0..1 : FPatchPartArray[ location].FParameterLabelList.Read( aChunk, ModuleCount);
          //2    : FMorphNames.Read( aChunk);
          2 : FPatchPartArray[ location].FParameterLabelList.Read( aChunk, ModuleCount);
          end;
        end ;
  C_PATCH_DESCR :     // $21
        begin // Patch description
          FPatchDescription.read( aChunk);
        end ;
  C_CURRENT_NOTE_2 :  // $69
        begin // Current note
          FCurrentNoteList.Read( aChunk);
        end ;
  C_MORPH_PARAM :     // $65
        begin // Morph parameter list
          FMorphParameters.Read( aChunk);
        end ;
  C_KNOBS :           // $62
        begin // Knob assignments
          FKnobList.Read( aChunk);
        end ;
  C_CONTROLLERS :     // $60
        begin // Controller assignments
          FControllerList.Read( aChunk);
        end ;
  C_PATCH_NOTES :     // $6f
        begin // Patch notes
          SetLength(FPatchNotes.FText, aChunk.FSize);
          FPatchNotes.Read( aChunk);
        end ;
  S_SEL_PARAM_PAGE :  // $2d
        begin // Sel parameter page
          aChunk.FStream.Position := aChunk.FStream.Position - 1; // This seems to be there only in patches that come from usb
        end;
    else begin
      Result := False;
    end;
  end;
end;

procedure TG2FilePatch.Write( aChunk : TPatchChunk; aVariationCount : byte);
begin
  {for i := 0 to aVariationCount - 1 do
    if (VariationCount - 1) < i then
      AddNewVariation(i);}

  FPatchDescription.Write( aChunk);
  aChunk.WriteChunk( C_PATCH_DESCR);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPartArray[ ord(ltVA)].FModuleList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPartArray[ ord(ltFX)].FModuleList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_LIST);

  FCurrentNoteList.Write( aChunk);
  aChunk.WriteChunk( C_CURRENT_NOTE_2);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPartArray[ ord(ltVA)].FCableList.Write( aChunk);
  aChunk.WriteChunk( C_CABLE_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPartArray[ ord(ltFX)].FCableList.Write( aChunk);
  aChunk.WriteChunk( C_CABLE_LIST);

  aChunk.WriteBits( LOCATION_PATCH_SETTINGS, 2);
  FPatchsettings.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPartArray[ ord(ltVA)].FParameterList.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPartArray[ ord(ltFX)].FParameterList.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_PARAM_LIST);

  FMorphParameters.Write( aChunk, aVariationCount);
  aChunk.WriteChunk( C_MORPH_PARAM);

  FKnobList.Write( aChunk);
  aChunk.WriteChunk( C_KNOBS);

  FControllerList.Write( aChunk);
  aChunk.WriteChunk( C_CONTROLLERS);

  aChunk.WriteBits( LOCATION_PATCH_SETTINGS, 2);
  //FMorphNames.Write( aChunk);
  FPatchPartArray[ ord(ltPatch)].FParameterLabelList.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_VA, 2);
  FPatchPartArray[ ord(ltVA)].FParameterLabelList.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_FX, 2);
  FPatchPartArray[ ord(ltFX)].FParameterLabelList.Write( aChunk);
  aChunk.WriteChunk( C_PARAM_NAMES);

  aChunk.WriteBits( LOCATION_VA, 2);
  aChunk.WriteBits( 0, 6);
  FPatchPartArray[ ord(ltVA)].FModuleLabelList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_NAMES);

  aChunk.WriteBits( LOCATION_FX, 2);
  aChunk.WriteBits( 0, 6);
  FPatchPartArray[ ord(ltFX)].FModuleLabelList.Write( aChunk);
  aChunk.WriteChunk( C_MODULE_NAMES);

  FPatchNotes.Write( aChunk);
  aChunk.WriteChunk( C_PATCH_NOTES);
end;

function TG2FilePatch.LoadFromFile(aStream : TStream; aLogLines : TStrings): Boolean;
var
  sl : TStringList;
  Chunk : TPatchChunk;
  s  : string;
  b  : byte;
  bm, bl : Byte;
  Crc : Word;
begin
  Result := False;

  Chunk := TPatchChunk.Create( aStream);
  sl := TStringList.Create;
  try
    if assigned( aLogLines) then
      Chunk.FLogLines := aLogLines;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( b, SizeOf( b));
      case b of
      $0d :; // just skip it
      $0a : begin
              sl.Add(s);
              s := '';
            end;
      $00 : Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    Init;
    Chunk.ReadBuffer( 2);
    FPatchVersion := Chunk.ReadBits( 8);
    FPatchType := Chunk.ReadBits( 8);
    Chunk.ReadChunk;
    Read( Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    Result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

class function TG2FilePatch.LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  Result := TG2FilePatch.Create( AOwner);
  aChunk.ReadChunk;
  Result.Read( aChunk);
end;

procedure TG2FilePatch.SaveToFile( aStream : TStream);
var //sl : TStringList;
    //i : integer;
    //c : AnsiChar;
    //s : AnsiString;
    Chunk : TPatchChunk;
begin
  //sl := TStringList.Create;
  Chunk := TPatchChunk.Create( aStream);
  try
    {sl.Clear;
    sl.Add('Version=Nord Modular G2 File Format 1');
    sl.Add('Type=Patch');
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := AnsiString(sl[i]);
      aStream.Write( s[1], Length( s));
      c := #$0d;
      aStream.Write( c, SizeOf( c));
      c := #$0a;
      aStream.Write( c, SizeOf( c));
    end;

    c := #0;
    aStream.Write( c, SizeOf( c));}

    AddHeaderInfo( pftPatch, aStream);

    Chunk.WriteBits(FPatchVersion, 8);
    Chunk.WriteBits(FPatchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;
    Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
    //sl.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TG2FilePatch.SaveAsFXP( aStream : TStream);
var FXPHeader : TFXPHeader;
    MemStream : TMemoryStream;
    i : integer;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
{    begin
      // TDOD
    end;}

begin
  MemStream := TMemoryStream.Create;
  try
    SaveToFile(MemStream);

    StringToByteArray( 'CcnK', FXPHeader.chunkMagic, 4);
    FXPHeader.byteSize    := SwapBytes(52 + MemStream.Size);
    StringToByteArray( 'FPCh', FXPHeader.fxMagic, 4);
    FXPHeader.version     := SwapBytes(1);
    StringToByteArray( 'NMG2', FXPHeader.fxID, 4);
    FXPHeader.fxVersion   := SwapBytes(1);
    FXPHeader.numPrograms := SwapBytes(1);
    Fillchar(FXPHeader.Name, 28, #0);
    for i := 0 to PatchName.Length - 1 do
      FXPHeader.name[i] := byte(PatchName.Chars[i]);

    FXPHeader.chunkSize   := SwapBytes(MemStream.Size);

    aStream.Write(FXPHeader, SizeOf(FXPHeader));
    aStream.Write(MemStream.Memory^, MemStream.Size);

  finally
    MemStream.Free;
  end;
end;
{$ENDIF}

function TG2FilePatch.CreatePatchPart( aLocation : TLocationType): TG2FilePatchPart;
begin
  Result := TG2FilePatchPart.Create( Self);
  Result.Location := aLocation;
end;

function TG2FilePatch.CreateModule( aLocation : TLocationType; aModuleIndex : byte; aModuleType : byte): TG2FileModule;
var i : integer;
begin
  Result := TG2FileModule.Create( FPatchPartArray[ ord(aLocation)]);
  Result.FModuleIndex := aModuleIndex;
  Result.FTypeID := aModuleType;

  if assigned( G2) and (aLocation <> ltPatch) then begin
    i := GetDataModuleIndex(aModuleType);
    Result.InitModule( aLocation, aModuleType);
    CreateLeds( aLocation, aModuleIndex, aModuleType);
  end;

  Result.Location := aLocation;

  if assigned(G2) and assigned(G2.OnCreateModule) then
    G2.OnCreateModule(self, G2.ID, Result);
end;

function TG2FilePatch.CreateCable( aLocation : TLocationType; aColor : byte; aFromModule : byte; aFromConnector : byte; aLinkType : byte; aToModule : byte; aToConnector : byte): TG2FileCable;
begin
  Result                := TG2FileCable.Create( FPatchPartArray[ord(aLocation)]);
  Result.FCableColor    := aColor;
  Result.FModuleFrom    := aFromModule;
  Result.FConnectorFrom := aFromConnector;
  Result.FLinkType      := aLinkType;
  Result.FModuleTo      := aToModule;
  Result.FConnectorTo   := aToConnector;
end;

function TG2FilePatch.CreateParameter( aModuleIndex : byte): TG2FileParameter;
begin
  Result := TG2FileParameter.Create( self, ltPatch, aModuleIndex, nil);
end;

function TG2FilePatch.CreateLed( const aLedType  : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID : byte; const aGroupCount : integer): IG2DataLed;
begin
  Result := TG2FileLed.Create( aLedType, aLocation, aModuleIndex, aGroupID, aGroupCount);
end;

{function TG2FilePatch.CreateLedStrip( const aLedType  : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID : byte; const aGroupCount : integer): IG2Led;
begin
  Result := TG2FileLed.Create( aLedType, aLocation, aModuleIndex, aGroupID, aGroupCount);
end;}

procedure TG2FilePatch.CreateLeds( const aLocation : TLocationType; const aModuleIndex, aModuleType: byte);
var i, j, GroupCount : integer;
    Led : IG2DataLed;
begin
  for i := 0 to High(LedDefs) do begin
    if LedDefs[i].ModuleID = aModuleType then begin
      GroupCount := GetDataLedsInGroup( aModuleType, LedDefs[i].GroupID);
      if GroupCount = 1 then begin
        Led := CreateLed( LedDefs[i].LedType, aLocation, aModuleIndex, LedDefs[i].GroupID, GroupCount);
        FLedList.Add(Led);
      end else begin
        //Led := FLedStripList.Where( function ( const aLed : IG2DataLed) : boolean
        //                            begin
        //                              Result := (aLed.Location = aLocation)
        //                                    and (aLed.ModuleIndex = aModuleIndex)
        //                                    and (aLed.GroupID = LedDefs[i].GroupID);
        //                            end).FirstOrDefault;
        j := 0;
        while (j < FLedStripList.Count) and not((FLedStripList[j].Location = aLocation)
                                            and (FLedStripList[j].ModuleIndex = aModuleIndex)
                                            and (FLedStripList[j].GroupID = LedDefs[i].GroupID)) do
          inc(j);

        if not (j < FLedStripList.Count) then begin
          Led := CreateLed( LedDefs[i].LedType, aLocation, aModuleIndex, LedDefs[i].GroupID, GroupCount);
          FLedStripList.Add(Led);
        end;
      end;
    end;
  end;

  for i := 0 to High(MiniVUDefs) do begin
    if MiniVUDefs[i].ModuleID = aModuleType then begin
      Led := CreateLed( ltMiniVU, aLocation, aModuleIndex, MiniVUDefs[i].GroupID, 1);
      FLedStripList.Add(Led);
    end;
  end;

  FLedList.Sort;
  FLedStripList.Sort;
end;

procedure TG2FilePatch.AddModuleToPatch( aLocation : TLocationType; aModule: TG2FileModule);
begin
  FPatchPartArray[ ord(aLocation)].FModuleList.AddModule( aModule);
  //if aLocation <> ltPatch then begin
    FPatchPartArray[ ord(aLocation)].UnselectModules;
    FPatchPartArray[ ord(aLocation)].FocusedModule := aModule;
  //end;
end;

procedure TG2FilePatch.DeleteModuleFromPatch( aLocation : TLocationType; aModule: TG2FileModule);
var aModuleIndex : Byte;
    i : integer;
begin
  //FPatchPartArray[ ord(aLocation)].DeselectModule( aModule);
  aModule.SelectMOdule( False, True);

  if assigned(FPerformance) and assigned(FSlot) then
    FPerformance.DeleteModuleFromPerf( FSlot.SlotIndex, aLocation, aModule);

  aModuleIndex := aModule.ModuleIndex;

  FPatchPartArray[ ord(aLocation)].DeleteModule( aModuleIndex);
  FControllerList.DeleteModule( aModuleIndex);
  FKnobList.DeleteModule( aModuleIndex, ord(aLocation));
  for i := 0 to Length(FPatchSettings.FVariationListArray) - 1 do
    FPatchSettings.PatchParamList[i].DeleteModule( ord(aLocation), aModuleIndex);

  for i := FLedList.Count - 1 downto 0 do
    if (FLedList[i].Location = aLocation)
   and (FLedList[i].ModuleIndex = aModuleIndex) then
      FLedList.Delete(i);

  for i := FLedStripList.Count - 1 downto 0 do
    if (FLedStripList[i].Location = aLocation)
   and (FLedStripList[i].ModuleIndex = aModuleIndex) then
      FLedStripList.Delete(i);

  FLedList.Sort;
  FLedStripList.Sort;
end;

{procedure TG2FilePatch.SelectModule(aModule: TG2FileModule);
begin
  if not assigned(aModule) then
    exit;

  if SelectedModuleList.FindModule( aModule.ModuleIndex) = nil then
    SelectedModuleList.AddModule( aModule);

  if (SelectedModuleList.Count = 1) and (aModule.ParameterCount > 0) then begin
    if (aModule.FSelectedParam = -1) then
      aModule.FSelectedParam := 0;
    if aModule.FSelectedParam >= aModule.ParameterCount then
      aModule.FSelectedParam := 0;

    aModule.Parameter[ aModule.FSelectedParam].Selected := True;
  end;
end;

procedure TG2FilePatch.DeselectModule(aModule: TG2FileModule);
var Param : TG2FileParameter;
begin
  Param := aModule.GetSelectedParam;
  if Param = FSelectedParam then
    FSelectedParam := nil;

  FSelectedModuleList.DeleteModule( aModule.ModuleIndex);
end;}

procedure TG2FilePatch.AddCableToPatch( aLocation : TLocationType; aCable: TG2FileCable);
begin
  FPatchPartArray[ ord(aLocation)].AddCable( aCable);
end;

procedure TG2FilePatch.DeleteCableFromPatch(aLocation : TLocationType; aCable: TG2FileCable);
begin
  FPatchPartArray[ ord(aLocation)].DeleteCable( aCable);
end;

function TG2FilePatch.FindKnob(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
begin
  Result := FKnobList.FindKnob( aLocation, aModuleIndex, aParamIndex);
end;

function TG2FilePatch.FindMidiCC(aLocation : TLocationType; aModuleIndex, aParamIndex : byte): byte;
begin
  Result := FControllerList.FindMidiCC( aLocation, aModuleIndex, aParamIndex);
end;

function TG2FilePatch.AssignKnobInPatch(aKnobIndex: integer; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TKnob;
var Knob : TKnob;
begin
  Result := nil;

  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FKnobList.Count then begin
    Knob := FKnobList.Items[ aKnobIndex];
    Knob.Fassigned := 1;
    Knob.FLocation := ord(aLocation);
    Knob.FModuleIndex := aModuleIndex;
    Knob.FIsLed := 0;
    Knob.FParamIndex := aParamIndex;

    Result := Knob;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

function TG2FilePatch.AssignMidiCCInPatchX(aMidiCC: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TController;
var Controller : TController;
begin
  Result := nil;
  Controller := GetMidiCC( aMidiCC);
  if Controller = nil then begin

    Controller := TController.Create;
    Controller.FLocation := ord(aLocation);
    Controller.FModuleIndex := aModuleIndex;
    Controller.FParamIndex := aParamIndex;
    Controller.FMidiCC := aMidiCC;
    FControllerList.Add( Controller);

    Result := Controller;
  end else begin
    Controller.FLocation := ord(aLocation);
    Controller.FModuleIndex := aModuleIndex;
    Controller.FParamIndex := aParamIndex;

    Result := Controller;
  end;
end;

procedure TG2FilePatch.DeassignKnobInPatch(aKnobIndex: integer);
var Knob : TKnob;
begin
  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FKnobList.Count then begin
    Knob := FKnobList.Items[ aKnobIndex];
    Knob.Fassigned := 0;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

procedure TG2FilePatch.DeassignMidiCCInPatchX(aMidiCC: byte);
var i : integer;
begin
  i := 0;
  while (i < FControllerList.Count) and
    not(FControllerList.Items[i].FMidiCC = aMidiCC) do
    inc(i);

  if (i < FControllerList.Count) then
    FControllerList.Delete(i);
end;

function TG2FilePatch.GetMaxModuleIndex( aLocation : TLocationType) : integer;
begin
  Result := FPatchPartArray[ord(aLocation)].GetMaxModuleIndex;
end;

function TG2FilePatch.GetNoOffModuleType( aLocation : TLocationType; aModuleType : byte) : integer;
begin
  Result := FPatchPartArray[ord(aLocation)].GetNoOffModuleType( aModuleType);
end;

function TG2FilePatch.GetNoOffExtendedModules : integer;
begin
  Result := FPatchPartArray[ ord(ltVA)].FModuleList.GetNoOffExtendedModules
          + FPatchPartArray[ ord(ltFX)].FModuleList.GetNoOffExtendedModules;
end;

function TG2FilePatch.GetKnob( KnobIndex: integer): TKnob;
begin
  if (KnobIndex >= 0) and (KnobIndex < FKnobList.Count) then begin
    Result := FKnobList.Items[ KnobIndex]
  end else
    Result := nil;
end;

function TG2FilePatch.GetMidiCC( aMidiCC : byte): TController;
var i : integer;
begin
  Result := nil;
  i := 0;
  while (i < FControllerList.Count) and
    not(FControllerList.Items[i].FMidiCC = aMidiCC) do
    inc(i);

  if (i < FControllerList.Count) then
    Result := FControllerList.Items[i];
end;

function TG2FilePatch.GetModuleName( aLocation : TLocationType; aModuleIndex : byte): string;
begin
  Result := FPatchPartArray[ord(aLocation)].FindModuleLabel( aModuleIndex);
end;

function TG2FilePatch.GetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte): byte;
begin
  Result := 0;

  if ( aLocation = ltFX) or ( aLocation = ltVA) then begin
    Result := FPatchPartArray[ord(aLocation)].FindParamValue( aModuleIndex, aVariation, aParamIndex);
  end else
    if aLocation = ltPatch then begin

      case aModuleIndex of
        PATCH_MORPH :
          begin
            case aParamIndex of
              0..7  : Result := FPatchSettings.FMorphArray[aParamIndex].FDialArray[ aVariation];
              8..15 : Result := FPatchSettings.FMorphArray[aParamIndex - 8].FModeArray[ aVariation];
            end;
          end;
        PATCH_VOLUME :
          begin
            case aParamIndex of
              VOLUME_LEVEL : Result := FPatchSettings.FVariationListArray[ aVariation].FPatchVol;
              VOLUME_MUTE  : Result := FPatchSettings.FVariationListArray[ aVariation].FActiveMuted;
            end;
          end;
        PATCH_GLIDE :
          begin
            case aParamIndex of
              GLIDE_TYPE  : Result := FPatchSettings.FVariationListArray[ aVariation].FGlide;
              GLIDE_SPEED : Result := FPatchSettings.FVariationListArray[ aVariation].FGlideTime;
            end;
          end;
        PATCH_BEND :
          begin
            case aParamIndex of
              BEND_ON_OFF : Result := FPatchSettings.FVariationListArray[ aVariation].FBend;
              BEND_RANGE  : Result := FPatchSettings.FVariationListArray[ aVariation].FSemi;
            end;
          end;
        PATCH_VIBRATO :
          begin
            case aParamIndex of
              VIBRATO_MOD   : Result := FPatchSettings.FVariationListArray[ aVariation].FVibrato;
              VIBRATO_DEPTH : Result := FPatchSettings.FVariationListArray[ aVariation].FCents;
              VIBRATO_RATE  : Result := FPatchSettings.FVariationListArray[ aVariation].FRate;
            end;
          end;
        PATCH_ARPEGGIATOR :
          begin
            case aParamIndex of
              ARP_ON_OFF    : Result := FPatchSettings.FVariationListArray[ aVariation].FArpeggiator;
              ARP_SPEED     : Result := FPatchSettings.FVariationListArray[ aVariation].FArpTime;
              ARP_DIRECTION : Result := FPatchSettings.FVariationListArray[ aVariation].FArpType;
              ARP_OCTAVES   : Result := FPatchSettings.FVariationListArray[ aVariation].FOctaves;
            end;
          end;
        PATCH_SUSTAIN :
          begin
            case aParamIndex of
              SUSTAIN_PEDAL : Result := FPatchSettings.FVariationListArray[ aVariation].FSustain;
              OCTAVE_SHIFT : Result := FPatchSettings.FVariationListArray[ aVariation].FOctaveShift;
            end;
          end;
      end;
    end;
end;

procedure TG2FilePatch.SetParamInPatch( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aVariation : byte; aValue: byte);
var ModuleVariationList : TModuleVariationList;
    ModuleParamValues : TModuleParamValues;
    Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  if ( aLocation = ltFX) or ( aLocation = ltVA) then begin
    ModuleVariationList := FPatchPartArray[ord(aLocation)].FindModuleVariationList( aModuleIndex);
    if assigned(ModuleVariationList) then begin
      ModuleParamValues := ModuleVariationList.FindVariation(aVariation);
      if assigned(ModuleParamValues) then begin
        ModuleParamValues.FParamValueArray[ aParamIndex] := aValue;
      end;
    end;
  end else
    if aLocation = ltPatch then begin

      case aModuleIndex of
        PATCH_MORPH :
          begin
            case aParamIndex of
              0..7  : FPatchSettings.FMorphArray[aParamIndex].FDialArray[ aVariation] := aValue;
              8..15 : FPatchSettings.FMorphArray[aParamIndex - 8].FModeArray[ aVariation] := aValue;
            end;
          end;
        PATCH_VOLUME :
          begin
            case aParamIndex of
              VOLUME_LEVEL : FPatchSettings.FVariationListArray[ aVariation].FPatchVol := aValue;
              VOLUME_MUTE  : FPatchSettings.FVariationListArray[ aVariation].FActiveMuted := aValue;
            end;
          end;
        PATCH_GLIDE :
          begin
            case aParamIndex of
              GLIDE_TYPE  : FPatchSettings.FVariationListArray[ aVariation].FGlide := aValue;
              GLIDE_SPEED : FPatchSettings.FVariationListArray[ aVariation].FGlideTime := aValue;
            end;
          end;
        PATCH_BEND :
          begin
            case aParamIndex of
              BEND_ON_OFF : FPatchSettings.FVariationListArray[ aVariation].FBend := aValue;
              BEND_RANGE : FPatchSettings.FVariationListArray[ aVariation].FSemi := aValue;
            end;
          end;
        PATCH_VIBRATO :
          begin
            case aParamIndex of
              VIBRATO_MOD   : FPatchSettings.FVariationListArray[ aVariation].FVibrato := aValue;
              VIBRATO_DEPTH : FPatchSettings.FVariationListArray[ aVariation].FCents := aValue;
              VIBRATO_RATE  : FPatchSettings.FVariationListArray[ aVariation].FRate := aValue;
            end;
          end;
        PATCH_ARPEGGIATOR :
          begin
            case aParamIndex of
              ARP_ON_OFF    : FPatchSettings.FVariationListArray[ aVariation].FArpeggiator := aValue;
              ARP_SPEED     : FPatchSettings.FVariationListArray[ aVariation].FArpTime := aValue;
              ARP_DIRECTION : FPatchSettings.FVariationListArray[ aVariation].FArpType := aValue;
              ARP_OCTAVES   : FPatchSettings.FVariationListArray[ aVariation].FOctaves := aValue;
            end;
          end;
        PATCH_SUSTAIN :
          begin
            case aParamIndex of
              SUSTAIN_PEDAL : FPatchSettings.FVariationListArray[ aVariation].FSustain := aValue;
              OCTAVE_SHIFT : FPatchSettings.FVariationListArray[ aVariation].FOctaveShift := aValue;
            end;
          end;
      end;
    end;

  if assigned(G2) then begin
    if (G2.CLientType <> ctVST) then begin

      Param := nil;
      Module := nil;
      {if (aLocation = ltFX) or (aLocation = ltVA)  then}
        Module := Modules[ord(aLocation), aModuleIndex];

      {if aLocation = ltPatch then
        Param := Parameter[aModuleIndex, aParamIndex]
      else}
        if assigned(Module) then
          Param := Module.Parameter[aParamIndex];

      DoLabelValueChange( Param);

      //if assigned(Param) then begin
      //  if (Param.LabelOnValue) and assigned(G2.OnLabelValueChange) then
      //    G2.OnLabelValueChange( self, FG2.ID, Param);

      //  Param.InvalidateControl;
      //end;
    end;

    if assigned(G2.OnParameterChange) then
      G2.OnParameterChange( self, FG2.ID, Slot.SlotIndex, aVariation, aLocation, aModuleIndex, aParamIndex, aValue);
  end;

end;

procedure TG2FilePatch.SetParamValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation, aValue : byte);
begin
  //raise Exception.Create('Call of abstract function.');
  SetParamInPatch( aLocation, aModuleIndex, aParamIndex, aVariation, aValue);

  {if assigned(G2.OnParameterChange) then
    G2.OnParameterChange( self, G2.ID, Slot.SlotIndex, aVariation, aLocation, aModuleIndex, aParamIndex, aValue);}
end;

{procedure TG2FilePatch.InitModeValue( aLocation : TLocationType; aModuleIndex, aParamIndex, aValue : byte);
begin
  raise Exception.Create('Call of abstract function.');
end;}

function TG2FilePatch.GetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte): byte;
var Module : TG2FileModule;
begin
  Result := 0;

  Module := FPatchPartArray[ ord(aLocation)].FindModule( aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.FModeCount then
      Result := Module.FModeInfoArray[ aParamIndex];
end;

procedure TG2FilePatch.SetModeInPatch(aLocation: TLocationType; aModuleIndex,
  aParamIndex, aValue: byte);
var Module : TG2FileModule;
begin
  Module := FPatchPartArray[ ord(aLocation)].FindModule( aModuleIndex);
  if assigned(Module) then
    if aParamIndex < Module.FModeCount then begin
      Module.FModeInfoArray[ aParamIndex] := aValue;

      if assigned(G2.OnModuleModeChange) then
        G2.OnModuleModeChange( self, G2.ID, SLot.SlotIndex, aLocation, aModuleIndex, aParamIndex, aValue);
    end;
end;

procedure TG2FilePatch.SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aValue: byte);
begin
  SetModeInPatch( aLocation, aModuleIndex, aParamIndex, aValue);
end;

function TG2FilePatch.HasMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aVariation : byte): boolean;
var i : integer;
    PatchParamList : TPatchParamList;
begin
  PatchParamList := FPatchSettings.FVariationListArray[aVariation];
  i := 0;
  while (i < PatchParamList.Count) and
    not((PatchParamList.Items[i].FLocation = TBits2(aLocation)) and
        (PatchParamList.Items[i].FModuleIndex = aModuleIndex) and
        (PatchParamList.Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  Result := (i < PatchParamList.Count);
end;

function TG2FilePatch.GetMorph( aLocation : TLocationType; aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): TMorphParameter;
begin
  Result := FPatchSettings.FVariationListArray[aVariation].FindMorphParam( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
end;

function TG2FilePatch.GetMorphValue(aLocation: TLocationType; aModuleIndex,
  aParamIndex, aMorphIndex, aVariation: byte): byte;
var MorphParameter : TMorphParameter;
begin
  Result := 0;
  if aLocation = ltPatch then begin
    MorphParameter := GetMorph( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
    if assigned(MorphParameter) then
      Result := MorphParameter.Range;
  end else
    Result := FPatchPartArray[ord(aLocation)].GetMorphValue( aModuleIndex, aParamIndex, aMorphIndex, aVariation);
end;

procedure TG2FilePatch.SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte);
var MorphParameter : TMorphParameter;
begin
  MorphParameter := GetMorph( aLocation, aModuleIndex, aParamIndex, aMorphIndex, aVariation);
  if not assigned(MorphParameter) then begin
    if aValue <> 0 then
      FPatchSettings.FVariationListArray[aVariation].AddNewMorphParam( ord(aLocation), aModuleIndex, aParamIndex, aMorphIndex, aValue);
  end else begin
    if aValue = 0 then
      FPatchSettings.FVariationListArray[aVariation].DelMorphParam( ord(aLocation), aModuleIndex, aParamIndex, aMorphIndex)
    else
      MorphParameter.FRange := aValue;
  end;

  DoMorphChange( Slot.SlotIndex, TLocationType(aLocation), aModuleIndex, aParamIndex, aMorphIndex, aVariation, aValue);
  //if assigned(G2) then
  //  if assigned(G2.FOnMorphChange) then
  //    G2.FOnMorphChange( G2, G2.ID, Slot.SlotIndex, TLocationType(aLocation), aModuleIndex, aParamIndex, aMorphIndex, aVariation, aValue);
end;

procedure TG2FilePatch.SetVoiceCount( aValue : byte);
begin
  FPatchDescription.VoiceCount := aValue;
end;

function TG2FilePatch.GetVoiceCount : byte;
begin
  Result := FPatchDescription.VoiceCount;
end;

procedure TG2FilePatch.SetVoiceMode( aValue : byte);
begin
  FPatchDescription.MonoPoly := aValue;
end;

function TG2FilePatch.GetVoiceMode : byte;
begin
  Result := FPatchDescription.MonoPoly;
end;

procedure TG2FilePatch.SetMasterClock( aValue : byte);
begin
  if assigned(FPerformance) then
    FPerformance.SetMasterClock( aValue);
end;

function TG2FilePatch.GetMasterClock : byte;
begin
  if assigned(FPerformance) then
    Result := FPerformance.FMasterClock
  else
    Result := 0;
end;

procedure TG2FilePatch.SetMasterClockRun( aValue : byte);
begin
  if assigned(FPerformance) then
    FPerformance.SetMasterClockRun( aValue);
end;

function TG2FilePatch.GetMasterClockRun : byte;
begin
  if assigned(FPerformance) then
    Result := FPerformance.FMasterClockRun
  else
    Result := 0;
end;

procedure TG2FilePatch.SetVariation( aValue : byte);
begin
  FPatchDescription.ActiveVariation := aValue;
end;

function TG2FilePatch.GetVariation : byte;
begin
  Result := FPatchDescription.ActiveVariation;
end;

function TG2FilePatch.GetVariationCount: integer;
begin
  Result := FPatchSettings.VariationCount;
end;

{procedure TG2FilePatch.AddNewVariation( aVariation : byte);
var i : integer;
begin
  //for i := 0 to 2 do
  //  FPatchPartArray[i].AddNewVariation( aVariation);
  FPatchSettings.AddNewVariation( aVariation);
  FMorphParameters.AddNewVariation( aVariation);
end;}

procedure TG2FilePatch.CopyVariation( aFromVariation, aToVariation : byte);
var i : integer;
begin
  for i := 0 to 2 do
    FPatchPartArray[i].CopyVariation( aFromVariation, aToVariation);
  FPatchSettings.FVariationListArray[ aToVariation].Copy( FPatchSettings.FVariationListArray[ aFromVariation]);
end;

function TG2FilePatch.GetPatchPart( aIndex : integer): TG2FilePatchPart;
begin
  if (aIndex < Length(FPatchPartArray)) then
    Result := FPatchPartArray[ aIndex]
  else
    raise Exception.Create('Patch part index out of range.');
end;

function TG2FilePatch.GetModuleList( aIndex : integer): TModuleList;
begin
  if aIndex < Length(FPatchPartArray) then
    Result := FPatchPartArray[ aIndex].FModuleList
  else
    raise Exception.Create('Module list index ' + IntToStr( aIndex) + ' out of range.');
end;

{function TG2FilePatch.GetSelectedModuleList: TModuleList;
begin
  Result := FSelectedModuleList;
end;}

function TG2FilePatch.GetCableList( aIndex : integer): TCableList;
begin
  if aIndex < Length(FPatchPartArray) then
    Result := FPatchPartArray[ aIndex].FCableList
  else
    raise Exception.Create('Cable list index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetParameterList( aIndex : integer): TModuleParameterList;
begin
  if aIndex < Length(FPatchPartArray) then
    Result := FPatchPartArray[ aIndex].FParameterList
  else
    raise Exception.Create('Parameter list index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetModuleLabelList( aIndex : integer): TModuleLabelList;
begin
  if aIndex < Length(FPatchPartArray) then
    Result := FPatchPartArray[ aIndex].FModuleLabelList
  else
    raise Exception.Create('ModuleLabels index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FilePatch.GetParameterLabelList( aIndex : integer): TParameterLabelList;
begin
  if aIndex < Length(FPatchPartArray) then
    Result := FPatchPartArray[ aIndex].FParameterLabelList
  else
    raise Exception.Create('ParameterLabels index ' + IntToStr( aIndex) + ' out of range.');
end;

{procedure TG2FilePatch.SetMaxVariations( aValue : Byte);
begin
  FMaxVariations := aValue;
end;

function TG2FilePatch.GetMaxVariations : Byte;
begin
  Result := FMaxVariations;
end;}

function TG2FilePatch.GetPatchName : string;
begin
  if assigned(FSlot) then
    Result := FSlot.FPatchName
  else
    Result := '';
end;

function TG2FilePatch.GetModule( LocationIndex, ModuleIndex: integer): TG2FileModule;
begin
  Result := FPatchPartArray[LocationIndex].FModuleList.FindModule( ModuleIndex);
end;

function TG2FilePatch.GetModuleCount( LocationIndex: integer): integer;
begin
  Result := FPatchPartArray[LocationIndex].FModuleList.Count;
end;

{function TG2FilePatch.GetParameter( ModuleIndex, ParamIndex: integer): TG2FileParameter;
var i : integer;
begin
  i := 0;
  while ( i < Length(FParamArray)) and not( (ModuleIndex = FParamArray[i].ModuleIndex)
                                    and (ParamIndex = FParamArray[i].ParamIndex)) do
    inc(i);

  if ( i < Length(FParamArray)) then
    Result := FParamArray[i]
  else
    Result := nil;
end;

function TG2FilePatch.GetParameterCount: integer;
begin
  Result := Length(FParamArray);
end;}

function TG2FilePatch.GetActiveVariation : byte;
begin
  if Assigned(FPatchDescription) then
    Result := FPatchDescription.FActiveVariation
  else
    Result := 0;
end;


function TG2FilePatch.GetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex, aLabelIndex : byte): string;
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then begin
    Result := FPatchPartArray[ ord(aLocation)].GetParameterLabel( aModuleIndex, aParamIndex, aLabelIndex)
  end else
    Result := '';
end;

function TG2FilePatch.GetParameterLabelCount(aLocation: TLocationType;  aModuleIndex, aParamIndex: byte): integer;
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then begin
    Result := FPatchPartArray[ ord(aLocation)].GetParameterLabelCount( aModuleIndex, aParamIndex)
  end else
    Result := 0;
end;

procedure TG2FilePatch.SetParameterLabel( aLocation : TLocationType; aModuleIndex, aParamIndex, aLabelIndex : byte; aValue : string);
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then
    FPatchPartArray[ ord(aLocation)].SetParameterLabel(aModuleIndex, aParamIndex, aLabelIndex, aValue);
end;

function TG2FilePatch.GetModuleLabel( aLocation : TLocationType; aModuleIndex : byte): string;
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then begin
    Result := FPatchPartArray[ ord(aLocation)].GetModuleLabel( aModuleIndex);
  end else
    Result := '';
end;

procedure TG2FilePatch.SetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aValue : string);
begin
  if (aLocation = ltFX) or (aLocation = ltVA) then
    FPatchPartArray[ ord(aLocation)].SetModuleLabel( aModuleIndex, aValue);
end;

procedure TG2FilePatch.UnselectModules( aLocation : TLocationType);
var i : integer;
begin
  for i := 0 to ModuleCount[ord(aLocation)] - 1 do
    if ModuleList[ord(aLocation)].Items[i].Selected then
      ModuleList[ord(aLocation)].Items[i].SelectModule( False, True);
end;

{procedure TG2FilePatch.SelectModuleAbove;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleAbove( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleUnder;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleUnder( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleLeft;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleLeft( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;

procedure TG2FilePatch.SelectModuleRight;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].GetModuleRight( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    UnSelectModules( FSelectedParam.FLocation);
    Module.Selected := True;
  end;
end;}

{procedure TG2FilePatch.SelectNextModuleParam;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].FindModule( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    Module.SelectNextParam;
  end;
end;

procedure TG2FilePatch.SelectPrevModuleParam;
var Module : TG2FileModule;
begin
  if FSelectedParam = nil then
    exit;

  Module := ModuleList[ ord(FSelectedParam.FLocation)].FindModule( FSelectedParam.FModuleIndex);
  if assigned(Module) then begin
    Module.SelectPrevParam;
  end;
end;}

function TG2FilePatch.MessAddModule( aLocation : TLocationType; aModuleTypeID, aCol, aRow: byte): boolean;
begin
  Result := False;
  //UnselectModules( ltVA);
  //UnselectModules( ltFX);
end;

function TG2FilePatch.MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean;
begin
  Result := False;
  //UnselectModules( ltVA);
  //UnselectModules( ltFX);
end;

function TG2FilePatch.MessCopyParameters(aSrcePatch: TG2FilePatchPart;
  aToLocation: TLocationType; aFromVariation, aToVariation: integer): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessAddConnection( aLocation : TLocationType; aFromConnection, aToConnection : TG2FileConnector): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessDeleteModules( aLocation : TLocationType): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessMoveModule( aLocation : TLocationType; aModuleIndex, aCol, aRow : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex, aLabelIndex: byte; aName: string): boolean;
begin
  // Abstract
  Result := False;
end;

function TG2FilePatch.MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : string): boolean;
begin
  // Abstract
  Result := False;
end;

//procedure TG2FilePatch.SortLeds;
//begin
  // Abstract
//end;

procedure TG2FilePatch.SetSelectedLocation(aLocation: TLocationType);
begin
  if FSelectedLocation <> aLocation then begin

    FSelectedLocation := aLocation;

    DoSelectLocation;
    //if assigned(G2) then begin
    //  if assigned(G2.FOnSelectLocation) then
    //    G2.FOnSelectLocation( G2, G2.ID, Slot.SlotIndex, aLocation);
    //end;
  end;
end;

procedure TG2FilePatch.SetSelectedMorphIndex( aValue : integer);
begin
  FSelectedMorphIndex := aValue;
end;

//------------------------------------------------------------------------------
//
//                              TG2FileConnector
//
//------------------------------------------------------------------------------

constructor TG2FileConnector.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModule : TG2FileModule);
begin
  FPatch := aPatch;
  FLocation := aLocation;
  FModule := aModule;

  FConnectorKind := ckInput;
  FConnectorDefColor := COLOR_RED;
  FConnectorIndex := 0;

  //FControl := nil;
  FCables := TList<TG2FileCable>.Create;
end;

destructor TG2FileConnector.Destroy;
var i : integer;
begin
  // Remove the connector from any cable that isn't freed yet
  for i := 0 to FCables.Count - 1 do begin
    if FCables[i].FFromConnector = self then
      FCables[i].FFromConnector := nil;

    if FCables[i].FToConnector = self then
      FCables[i].FToConnector := nil;
  end;
  FCables.Free;

  inherited;
end;

procedure TG2FileConnector.InitConnector( aModuleType, aConnectorIndex: Byte; aConnectorKind: TConnectorKind);
var i : integer;
begin
  FConnectorIndex := aConnectorIndex;
  FConnectorKind := aConnectorKind;
  case FConnectorKind of
    ckInput:
      begin
        i := GetDataModuleInputIndex( aModuleType, aConnectorIndex);
        FName := ModuleInputs[i].ConnectorName;
        FConnectorType := ModuleInputs[i].ConnectorType;
        FBandWidth := ModuleInputs[i].BandWidth;
      end;
    ckOutput:
      begin
        i := GetDataModuleOutputIndex( aModuleType, aConnectorIndex);
        FName := ModuleOutputs[i].ConnectorName;
        FConnectorType := ModuleOutputs[i].ConnectorType;
        FBandWidth := ModuleOutputs[i].BandWidth;
      end;
  end;
end;

procedure TG2FileConnector.AddCable( aCable: TG2FileCable);
begin
  FCables.Add( aCable);
end;

procedure TG2FileConnector.DelCable(aCable: TG2FileCable);
var i : integer;
begin
  i := 0;
  while ( i < FCables.Count) and (FCables[i] <> aCable) do
    inc(i);

  if ( i < FCables.Count) then
    FCables.Delete(i);

{FMX  if assigned( FModule) and assigned( FModule.Parent) then
    FModule.Parent.Invalidate;}
end;

procedure TG2FileConnector.InvalidateCables;
var i : integer;
begin
  for i := 0 to FCables.Count - 1 do
    FCables[i].ConnectorMoved;
end;

procedure TG2FileConnector.InvalidateControl;
begin
  //
end;

procedure TG2FileConnector.SetConnectorDefColor( aValue: byte);
begin
  FConnectorDefColor := aValue;
end;

procedure TG2FileConnector.SetConnectorKind( aValue : TConnectorKind);
begin
  FConnectorKind := aValue;
end;

function TG2FileConnector.GetConnectorColor: byte;
begin
  if (FConnectorDefColor = COLOR_BLUE) and (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
    Result := COLOR_RED
  else
    if (FConnectorDefColor = COLOR_YELLOW) and (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
      Result := COLOR_ORANGE
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetNewConnectorColor: byte;
begin
  if (FConnectorDefColor = COLOR_BLUE) and (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := COLOR_RED
  else
    if (FConnectorDefColor = COLOR_YELLOW) and (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
      Result := COLOR_ORANGE
    else
      Result := FConnectorDefColor;
end;

function TG2FileConnector.GetCable( aIndex : integer) : TG2FileCable;
begin
  if aIndex < FCables.Count then
    Result := FCables[ aIndex]
  else
    raise Exception.Create('Cable index ' + IntToStr( aIndex) + ' out of range.');
end;

function TG2FileConnector.GetCableCount : integer;
begin
  Result := FCables.Count;
end;

procedure TG2FileConnector.CalcDefColor;
begin
  if FConnectorType = ctLogic then begin
    ConnectorDefColor := COLOR_YELLOW;
  end else
    if FConnectorType = ctControl then
      ConnectorDefColor := COLOR_BLUE
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then ConnectorDefColor := COLOR_RED;
        if FBandWidth = btDynamic then ConnectorDefColor := COLOR_BLUE;
      end;
end;

function TG2FileConnector.GetDefRate: TBits1;
begin
  Result := 0;
  // Return signal frequency indicator comming from this connector : 0 - 24kHz, 1 - 96kHz
  if FConnectorType = ctLogic then
    Result := 0
  else
    if FConnectorType = ctControl then
      Result := 0
    else
      if FConnectorType = ctAudio then begin
        if FBandWidth = btStatic then Result := 1;
        if FBandWidth = btDynamic then Result := 0;
      end else
        Result := 0;
end;

function TG2FileConnector.GetRate: TBits1;
begin
  if (FModule.Uprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

function TG2FileConnector.GetNewRate: TBits1;
begin
  if (FModule.NewUprate = 1) and (FBandWidth = btDynamic) then
    Result := 1
  else
    Result := GetDefRate;
end;

//------------------------------------------------------------------------------
//
//                               TG2FileParameter
//
//------------------------------------------------------------------------------

constructor TG2FileParameter.Create( aPatch : TG2FilePatch; aLocation : TLocationType; aModuleIndex : integer; aModule : TG2FileModule);
begin
  inherited Create(nil);

  FObserverList := TList<IG2Observer>.Create;

  FPatch := aPatch;
  FLocation := aLocation;
  FModuleIndex := aModuleIndex;
  FModule := aModule; // nil if location is ltPatch
  FKnob := nil;
  FCanChangeLabel := False;
  FDefaultKnob := -1;
  FButtonParamIndex := -1;
  FButtonText := TStringList.Create;
  FButtonText.Delimiter := ';';
  FButtonText.StrictDelimiter := True;
  FInfoFunctionIndex := 0;
  FTextFunctionIndex := 0;
  SetLength( FTextDependencieArray, 0);
  SetLength( FGraphDependencieArray, 0);

  FSuspendUpdate := False;
  FSuspendedValue := 0;
end;

destructor TG2FileParameter.Destroy;
var Observer : IG2Observer;
    Subject : IG2Subject;
    G2ParamObserver : IG2ParamObserver;
    G2MultiParamObserver : IG2MultiParamObserver;
begin
  Finalize( FGraphDependencieArray);
  Finalize( FTextDependencieArray);
  FButtonText.Free;
  {for Observer in FObserverList do begin
    if Supports( Observer, IG2MultiParamObserver, G2MultiParamObserver) then
      G2MultiParamObserver.ClearDataDependency(self);
    if Supports( Observer, IG2ParamObserver, G2ParamObserver) then
      G2ParamObserver.ClearDataDependency(self);
  end;}
  for Observer in FObserverList do begin
    //if Supports(self, IG2Subject, Subject) then
      Observer.RemoveReference( Self as IG2DataParam);
    {if Supports( Observer, IG2MultiParamObserver, G2MultiParamObserver) then
      G2MultiParamObserver.ClearDataDependency(self);
    if Supports( Observer, IG2ParamObserver, G2ParamObserver) then
      G2ParamObserver.ClearDataDependency(self);}
  end;
  FObserverList.Free;
  inherited;
end;

procedure TG2FileParameter.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event);
end;

procedure TG2FileParameter.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2FileParameter.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

function TG2FileParameter.GetButtonText(Index: integer): string;
begin
  if Index < FButtonText.Count then
    Result := FButtonText[Index]
  else
    Result := '';
end;

procedure TG2FileParameter.SetButtonText(Index: integer; aValue: string);
begin
  if Index < FButtonText.Count then
    FButtonText[Index] := aValue;
end;

function TG2FileParameter.GetParamLabel( aIndex : integer): string;
begin
  if assigned(FPatch) then
    Result := FPatch.GetParameterLabel( FLocation, FModuleIndex, FParamIndex, aIndex)
  else
    Result := '';
end;

procedure TG2FileParameter.SetParamLabel( aIndex : integer; aValue : string);
begin
  if assigned(FPatch) then
    FPatch.SetParameterLabel( FLocation, FModuleIndex, FParamIndex, aIndex, aValue);
end;


function TG2FileParameter.GetValueText(aIndex: integer): string;
begin
  if assigned(FPatch) then
    Result := FPatch.GetParameterLabel( FLocation, FModuleIndex, FParamIndex, aIndex)
  else
    Result := '';
  if Result = '' then
    if aIndex < FButtonText.Count then
      Result := FButtonText[aIndex];
end;


function TG2FileParameter.GetButtonTextCount: integer;
begin
  Result := FButtonText.Count;
end;

function TG2FileParameter.GetMorph( aMorphIndex, aVariation : integer) : TMorphParameter;
begin
  if assigned(FPatch) then
    Result := FPatch.GetMorph( FLocation, FModuleIndex, FParamIndex, aMorphIndex, aVariation)
  else
    Result := nil;
end;

function TG2FileParameter.GetSelectedMorph: TMorphParameter;
begin
  Result := GetMorph( FPatch.FSelectedMorphIndex, FPatch.ActiveVariation);
end;

function TG2FileParameter.HasMorph: boolean;
begin
  if assigned(FPatch) then
    Result := FPatch.HasMorph( FLocation, FModuleIndex, FParamIndex, FPatch.ActiveVariation)
  else
    Result := False;
end;

function TG2FileParameter.GetMorphValue( const aMorphIndex, aVariation : integer) : byte;
var MorphParameter : TMorphParameter;
begin
  MorphParameter := GetMorph( aMorphIndex, aVariation);
  if assigned(MorphParameter) then begin
    {if MorphParameter.FRange >= 128 then
      Result := GetParameterValue + ( MorphParameter.FRange - 256)
    else
      Result := GetParameterValue + MorphParameter.FRange;

    if Result > FHighValue then
      Result := FHighValue
    else
      if Result < FLowValue then
        Result := FLowValue;}
    Result := MorphParameter.FRange
  end else
    Result := 0;
end;

procedure TG2FileParameter.SetMorphValue( const aMorphIndex, aVariation: integer;
  const aValue: byte);
begin
  if assigned(FPatch) then begin
    FPatch.SetMorphValue( FLocation, FModuleIndex, FParamIndex, aMorphIndex, aValue, aVariation);
  end;
end;

function TG2FileParameter.GetSelectedMorphValue : byte;
begin
  if assigned(FPatch) then
    Result := GetMorphValue( FPatch.FSelectedMorphIndex, FPatch.ActiveVariation)
  else
    Result := 0;
end;

procedure TG2FileParameter.SetSelectedMorphValue( const Value: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if FPatch.EditAllVariations then begin
    FromVariation := 0;
    ToVariation := N_VARIATIONS - 1;
  end else begin
    FromVariation := FPatch.ActiveVariation;
    ToVariation := FPatch.ActiveVariation;
  end;

  for Variation := FromVariation to ToVariation do
    SetMorphValue( FPatch.FSelectedMorphIndex, Variation, Value);
  InvalidateControl;
end;

procedure TG2FileParameter.SetSuspendUpdate(aValue: boolean);
var Variation, FromVariation, ToVariation : byte;
begin
  if aValue <> FSuspendUpdate then begin
    FSuspendUpdate := aValue;
    if FSuspendUpdate then begin
      if assigned(FPatch) then begin
        case FParamType of
          ptParam:
            FSuspendedValue := FPatch.GetParameterValue( FLocation, FModuleIndex, FParamIndex, FPatch.ActiveVariation);
          ptMode:
            FSuspendedValue := FPatch.GetModeValue( FLocation, FModuleIndex, FParamIndex);
          ptMasterClock:
            case FParamIndex of
            0 : FSuspendedValue := FPatch.GetMasterClock;
            1 : FSuspendedValue := FPatch.GetMasterClockRun;
            end;
          ptVoiceMode:
            case FParamIndex of
            0 : FSuspendedValue := FPatch.GetVoiceCount;
            1 : FSuspendedValue := FPatch.GetVoiceMode;
            end;
        end;
      end else
        FSuspendedValue := 0;
    end else begin
      if assigned(FPatch) then begin
        case FParamType of
          ptParam:
            begin;
              if FPatch.EditAllVariations then begin
                FromVariation := 0;
                ToVariation := N_VARIATIONS - 1;
              end else begin
                FromVariation := FPatch.ActiveVariation;
                ToVariation := FPatch.ActiveVariation;
              end;

              for Variation := FromVariation to ToVariation do begin
                FPatch.SetParamValue( FLocation, FModuleIndex, FParamIndex, Variation, FSuspendedValue)
              end;
            end;
          ptMode: FPatch.SetModeValue( FLocation, FModuleIndex, FParamIndex, FSuspendedValue);
          ptMasterClock:
            begin
              case FParamIndex of
              0: FPatch.SetMasterClock( FSuspendedValue);
              1: FPatch.SetMasterClockRun( FSuspendedValue);
              end;
            end;
          ptVoiceMode:
            begin
              case FParamIndex of
              0: FPatch.SetVoiceCount( FSuspendedValue);
              1: FPatch.SetVoiceMode( FSuspendedValue);
              end;
            end;
        end;
      end;
    end;
  end;
end;

function TG2FileParameter.GetValue: byte;
begin
  Result := 0;
  if FSuspendUpdate then begin
    Result := FSuspendedValue
  end else
    if assigned(FPatch) then begin
      case FParamType of
        ptParam:
          Result := FPatch.GetParameterValue( FLocation, FModuleIndex, FParamIndex, FPatch.ActiveVariation);
        ptMode:
          Result := FPatch.GetModeValue( FLocation, FModuleIndex, FParamIndex);
        ptMasterClock:
          case FParamIndex of
          0 : Result := FPatch.GetMasterClock;
          1 : Result := FPatch.GetMasterClockRun;
          end;
        ptVoiceMode:
          case FParamIndex of
          0 : Result := FPatch.GetVoiceCount;
          1 : Result := FPatch.GetVoiceMode;
          end;
      end;
    end;
end;

procedure TG2FileParameter.SetValue( const aValue: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if FSuspendUpdate then begin
    FSuspendedValue := aValue;
    InvalidateControl;
  end else
    if assigned(FPatch) then begin
      case FParamType of
        ptParam:
          begin;
            if FPatch.EditAllVariations then begin
              FromVariation := 0;
              ToVariation := N_VARIATIONS - 1;
            end else begin
              FromVariation := FPatch.ActiveVariation;
              ToVariation := FPatch.ActiveVariation;
            end;

            for Variation := FromVariation to ToVariation do begin
              FPatch.SetParamValue( FLocation, FModuleIndex, FParamIndex, Variation, aValue)
            end;
          end;
        ptMode: FPatch.SetModeValue( FLocation, FModuleIndex, FParamIndex, aValue);
        ptMasterClock:
          begin
            case FParamIndex of
            0: FPatch.SetMasterClock( aValue);
            1: FPatch.SetMasterClockRun( aValue);
            end;
          end;
        ptVoiceMode:
          begin
            case FParamIndex of
            0: FPatch.SetVoiceCount( aValue);
            1: FPatch.SetVoiceMode( aValue);
            end;
          end;
      end;
    end;
end;

procedure TG2FileParameter.IncValue;
var Value : byte;
begin
  Value := GetValue;
  if Value < FHighValue then
    SetValue( Value + 1)
end;

procedure TG2FileParameter.DecValue;
var Value : byte;
begin
  Value := GetValue;
  if Value > FLowValue then
    SetValue( Value - 1)
end;

procedure TG2FileParameter.IncMorphValue;
var Value: byte;
    Range : integer;
    MorphParameter : TMorphParameter;
begin
  Value := GetValue;
  MorphParameter := GetSelectedMorph;
  if not(assigned(MorphParameter)) then
    Range := 0
  else
    Range := MorphParameter.FRange;

  if Range >= 128 then
    Range := Range - 256;

  inc(Range);

  if (Value + Range) <= FHighValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

procedure TG2FileParameter.DecMorphValue;
var Value: byte;
    Range : integer;
    MorphParameter : TMorphParameter;
begin
  Value := GetValue;
  MorphParameter := GetSelectedMorph;
  if not(assigned(MorphParameter)) then
    Range := 0
  else
    Range := MorphParameter.FRange;

  if Range >= 128 then
    Range := Range - 256;

  dec(Range);

  if (Value + Range) >= FLowValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

procedure TG2FileParameter.InitParam( aID : integer; aModuleName : string; aParamIndex : byte; aParamType : TParamType;
                                      aParamName, aDefaultParamLabel : string;
                                      aLowValue, aHighValue, aDefaultValue : byte;
                                      aDefaultKnob, aButtonParamIndex : integer; aButtonText : string);
var i : integer;
begin
  FID := aID;
  FParamType := aParamType;
  FParamIndex := aParamIndex;
  FParamName := aParamName;
  FDefaultParamLabel := aDefaultParamLabel;
  FCanChangeLabel := aDefaultParamLabel <> '';
  FLowValue := aLowValue;
  FHighValue := aHighValue;
  FDefaultValue := aDefaultValue;
  FModuleName := aModuleName;
  FDefaultKnob := aDefaultKnob;
  FButtonParamIndex := aButtonParamIndex;
  FButtonText.DelimitedText := aButtonText;

  if assigned(FPatch) then begin
    // Check for associated knob
    i := FPatch.FindKnob( FLocation, FModuleIndex, FParamIndex);

    if i <> -1 then begin
      FKnob := FPatch.FKnobList.Items[i];
      FKnob.FParameter := self;
    end else
      FKnob := nil;
  end;
end;

procedure TG2FileParameter.AssignKnob( aKnobIndex : integer);
begin
  FKnob := FPatch.AssignKnobInPatch( aKnobIndex, FLocation, FModuleIndex, FParamIndex);
  FKnob.FParameter := self;
end;

procedure TG2FileParameter.DeassignKnob( aKnobIndex: integer);
begin
  FPatch.DeassignKnobInPatch( aKnobIndex);
  FKnob := nil;
end;

procedure TG2FileParameter.AssignGlobalKnob( aPerf : TG2FilePerformance; aSlotIndex : byte; aKnobIndex : integer);
begin
  FGlobalKnob := aPerf.AssignGlobalKnobInPerf( aKnobIndex, aSlotIndex, FLocation, FModuleIndex, FParamIndex);
  FGlobalKnob.FParameter := self;
end;

procedure TG2FileParameter.DeassignGlobalKnob( aPerf : TG2FilePerformance; aKnobIndex: integer);
begin
  aPerf.DeassignGlobalKnobInPerf( aKnobIndex);
  FGlobalKnob := nil;
end;

procedure TG2FileParameter.AssignController( aMidiCC : byte);
begin
  FController := FPatch.AssignMidiCCInPatchX( aMidiCC, FLocation, FModuleIndex, FParamIndex);
  FCOntroller.Parameter := self;
end;

procedure TG2FileParameter.DeassignControl(const aControl: IG2ParamObserver);
begin
  //
end;

procedure TG2FileParameter.DeassignController;
begin
  if assigned(FController) then begin
    FController.Parameter := nil;
    FPatch.DeassignMidiCCInPatchX( FController.MidiCC);
    FController := nil;
  end;
end;

function TG2FileParameter.GetParameterLabelCount: integer;
begin
  if assigned(FPatch) then
    Result := FPatch.GetParameterLabelCount( FLocation, FModuleIndex, FParamIndex)
  else
    Result := 0;
end;

function TG2FileParameter.GetSelectedButtonText: string;
var ParamValue : byte;
begin
  if CanChangeLabel then
    Result := ParamLabel[0]
  else begin
    ParamValue := GetValue;
    if ParamValue < FButtonText.Count then
      Result := FButtonText[ParamValue]
    else
      Result := InfoFunction(FInfoFunctionIndex);
  end;
end;

function TG2FileParameter.GetKnobAssignmentsAllowed: boolean;
begin
  if Location = ltPatch then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

function TG2FileParameter.GetMorphAssignmentsAllowed: boolean;
begin
  if Location = ltPatch then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

function TG2FileParameter.GetMidiAssignmentsAllowed: boolean;
begin
  if Location = ltPatch then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
      PATCH_VOLUME : Result := ParamIndex <> VOLUME_LEVEL;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;


function TG2FileParameter.GetButtonParam : TG2FileParameter;
var Module : TG2FileModule;
begin
  if FButtonParamIndex <> -1 then begin
    Module := FPatch.Modules[ ord(FLocation), FModuleIndex];
    if assigned(Module) then
      Result := Module.Parameter[FButtonParamIndex]
    else
      Result := nil;
  end else
    Result := nil;
end;

function TG2FileParameter.GetSelected : boolean;
begin
  Result := False;
  if assigned(FPatch) then begin
    {case FLocation of
      ltFX: Result := FPatch.PatchPart[ ord(ltFX)].SelectedParam = self;
      ltVA: Result := FPatch.PatchPart[ ord(ltVA)].SelectedParam = self;
      ltPatch: Result := False;
    end;}
    if assigned(FPatch.PatchPart[ ord(FPatch.SelectedLocation)].FocusedModule) then
      Result := FPatch.PatchPart[ ord(FPatch.SelectedLocation)].FocusedModule.SelectedParam = self;
  end;
end;

procedure TG2FileParameter.SetSelected( const aValue : boolean);
//var Module : TG2FileModule;
begin
  {Module := nil;
  if aValue then begin
    if assigned(FPatch) then begin
      FPatch.SelectedLocation := FLocation;

      case FLocation of
        ltFX, ltVA, ltPatch :
          begin
            if FPatch.PatchPart[ ord(FLocation)].SelectedParam <> self then
              FPatch.PatchPart[ ord(FLocation)].SelectedParam := self;
            Module := FPatch.PatchPart[ ord(FLocation)].FindModule( FModuleIndex);
          end;
        //ltPatch: Module := nil;
      end;
      if assigned(Module) then
        Module.SelectedParam := self;

      Patch.DoSelectParam;
      //if assigned(Patch.G2) and assigned(Patch.G2.OnSelectParam) then
      //  Patch.G2.OnSelectParam( Patch.G2, Patch.G2.ID, self);
    end;
  end;}
  if aValue then
    FPatch.SelectParam( FLocation, FModuleIndex, FParamIndex);
end;

procedure TG2FileParameter.AddTextDependency( aParamType : TParamType; aParamIndex : byte);
var i : integer;
begin
  i := Length(FTextDependencieArray);
  SetLength( FTextDependencieArray, i + 1);
  FTextDependencieArray[i].ParamType := aParamType;
  FTextDependencieArray[i].ParamIndex := aParamIndex;
end;

function TG2FileParameter.GetTextDependendParamValue( aIndex : integer) : byte;
begin
  if aIndex < Length(FTextDependencieArray) then begin
    if FTextDependencieArray[aIndex].ParamType = ptParam then
      Result := Module.Parameter[FTextDependencieArray[aIndex].ParamIndex].GetValue
    else
      Result := Module.Mode[FTextDependencieArray[aIndex].ParamIndex].GetValue;
  end else
    Result := 0;
end;

procedure TG2FileParameter.AddGraphDependency( aParamType : TParamType; aParamIndex : byte);
var i : integer;
begin
  i := Length(FGraphDependencieArray);
  SetLength( FGraphDependencieArray, i + 1);
  FGraphDependencieArray[i].ParamType := aParamType;
  FGraphDependencieArray[i].ParamIndex := aParamIndex;
end;

function TG2FileParameter.GetGraphDependendParamValue( aIndex : integer) : byte;
begin
  if aIndex < Length(FGraphDependencieArray) then begin
    if FGraphDependencieArray[aIndex].ParamType = ptParam then
      Result := Module.Parameter[FGraphDependencieArray[aIndex].ParamIndex].GetValue
    else
      Result := Module.Mode[FGraphDependencieArray[aIndex].ParamIndex].GetValue;
  end else
    Result := 0;
end;

function TG2FileParameter.GetHighValue :  byte;
begin
  Result := FHighValue;
end;

function TG2FileParameter.GetLabelIndex: integer;
begin
  if GetLabelOnValue then // Switch
    Result := GetValue
  else
    Result := 0;
end;

function TG2FileParameter.GetLabelOnValue: boolean;
begin
  Result := FID in [167, 168, 169];
end;

function TG2FileParameter.GetLowValue: byte;
begin
  Result := FLowValue;
end;

{
function TG2FileParameter.FreqDispValue( aMode : integer; aFreqCourse : integer; aFreqFine : integer): string;
var iValue1, iValue2 : integer;
    Exponent, Freq, Fact : single;
begin
  case aMode of
  0 : begin // Semi
       iValue1 := aFreqCourse - 64;
       Result := '';
       if iValue1 < 0 then
         Result := Result + IntToStr(iValue1)
       else
         Result := Result + '+' + IntToStr(iValue1);
       Result := Result + '  ';
       iValue2 := (aFreqFine-64)*100 div 128;
       if iValue2 < 0 then
         Result := Result + IntToStr(iValue2)
       else
         Result := Result + '+' + IntToStr(iValue2);
     end;
  1 : begin // Freq
       // http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
       Exponent := ((aFreqCourse - 69) + (aFreqFine - 64) / 128) / 12;
       Freq := 440.0 * power(2, Exponent);
       if Freq >= 1000 then
         Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
       else
         Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
     end;
  2 : begin // Fac
       Exponent := ((aFreqCourse - 64) + (aFreqFine - 64) / 128) / 12;
       Fact := power(2, Exponent);
       Result :=  'x' + G2FloatToStrFixed( Fact, 6)
     end;
  3 : begin // Part
       if aFreqCourse <= 32 then begin
         Exponent := -(((32 - aFreqCourse ) * 4) + 77 - (aFreqFine - 64) / 128) / 12;
         Freq := 440.0 * power(2, Exponent);
         Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end else begin
         if (aFreqCourse > 32) and (aFreqCourse <=64) then begin
           iValue1 := 64 - aFreqCourse + 1;
           Result := '1:' + IntToStr(iValue1);
         end else begin
           iValue1 := aFreqCourse - 64 + 1;
           Result := IntToStr(iValue1) + ':1';
         end;
         Result := Result + '  ';
         iValue2 := (aFreqFine-64)*100 div 128;
         if iValue2 < 0 then
           Result := Result + IntToStr(iValue2)
         else
           Result := Result + '+' + IntToStr(iValue2);
       end;
     end;
  4 : begin // Semi PShift
       iValue1 := aFreqCourse - 64;
       Result := '';
       if iValue1 < 0 then
         Result := Result + G2FloatToStrFixed(iValue1 / 4, 4)
       else
         Result := Result + '+' + G2FloatToStrFixed(iValue1 / 4, 4);
       Result := Result + '  ';
       iValue2 := (aFreqFine-64)*100 div 128;
       if iValue2 < 0 then
         Result := Result + IntToStr(iValue2)
       else
         Result := Result + '+' + IntToStr(iValue2);
     end;
  end;
end;}

function TG2FileParameter.DelayDispValue( aType : integer; aRange : integer; aValue : integer): string;
var DlyRange, DlyMin, DlyMax : single;
begin
  case aType of
  0 : begin // ranges 5m,25m,100m,500m,1sm,2s,2.7s
         case aRange of
         0 : begin
               DlyMin := 0.05;
               DlyMax := 5.3;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         1 : begin
               DlyMin := 0.21;
               DlyMax := 25.1;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         2 : begin
               DlyMin := 0.8;
               DlyMax := 100;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         3 : begin
               DlyMin := 3.95;
               DlyMax := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         4 : begin
               DlyMin := 7.89;
               DlyMax := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,000s'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         5 : begin
               DlyMin := 15.8;
               DlyMax := 2000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 64 then
                   Result := G2FloatToStr((DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126)/1000, 5) + 's'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         6 : begin
               DlyMin := 21.3;
               DlyMax := 2700;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 48 then
                   Result := G2FloatToStr((DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126)/1000, 5) + 's'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 5) + 'm';
             end;
         end;
      end;
  1 : begin // ranges 5m,25m,100m,500m,1sm,2s,2.7s for DelayEight
         DlyMin := 0;
         DlyMax := 0;
         case aRange of
         0 : begin
               DlyMin := 0;
               DlyMax := 0.66;
             end;
         1 : begin
               DlyMin := 0;
               DlyMax := 3.14;
             end;
         2 : begin
               DlyMin := 0;
               DlyMax := 12.6;
             end;
         3 : begin
               DlyMin := 0;
               DlyMax := 62.5;
             end;
         4 : begin
               DlyMin := 0;
               DlyMax := 125;
             end;
         5 : begin
               DlyMin := 0;
               DlyMax := 250;
             end;
         6 : begin
               DlyMin := 0;
               DlyMax := 338;
             end;
         end;
         Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * aValue /127, 4) + 'm';
      end;
  2 : begin // ranges 500m,1s,2s,2.7s
         case aRange of
         0 : begin
               DlyRange := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         1 : begin
               DlyRange := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,00s'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         2 : begin
               DlyRange := 2000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 64 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         3 : begin
               DlyRange := 2700;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 48 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         end;
      end;
  3 : begin // ranges 500m,1s,1.351s
         case aRange of
         0 : begin
               DlyRange := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         1 : begin
               DlyRange := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,00s'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         2 : begin
               DlyRange := 1351;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 95 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         end;
      end;
  4 : begin // Clock synced
        case aValue of
            0..3 : Result := '1/64T';
            4..7 : Result := '1/64';
           8..11 : Result := '1/32T';
          12..15 : Result := '1/64D';
          16..19 : Result := '1/32';
          20..23 : Result := '1/16T';
          24..27 : Result := '1/32D';
          28..31 : Result := '1/16';
          32..35 : Result := '1/16';
          36..39 : Result := '1/8T';
          40..43 : Result := '1/8T';
          44..47 : Result := '1/16D';
          48..51 : Result := '1/16D';
          52..55 : Result := '1/8';
          56..59 : Result := '1/8';
          60..63 : Result := '1/4T';
          64..67 : Result := '1/4T';
          68..71 : Result := '1/8D';
          72..75 : Result := '1/8D';
          76..79 : Result := '1/4';
          80..83 : Result := '1/4';
          84..87 : Result := '1/2T';
          88..91 : Result := '1/2T';
          92..95 : Result := '1/4D';
          96..99 : Result := '1/4D';
        100..103 : Result := '1/2';
        104..107 : Result := '1/2';
        108..111 : Result := '1/1T';
        112..115 : Result := '1/2D';
        116..119 : Result := '1/1';
        120..123 : Result := '1/1D';
        124..127 : Result := '2/1';
        end;
      end;
  end;
end;

function TG2FileParameter.InfoFunction( aIndex : integer): string;
var t : single;
    FreqCourse, FreqFine, FreqMode, LevelShiftValue : Byte;
    Exponent, Freq, Fact : single;
    aValue, iValue1, iValue2 : integer;
    TempValue : integer;
    Param : TG2FileParameter;
begin
  if (TextFunctionIndex <> 0) and assigned(Module) and (Length(FTextDependencieArray)>0) then
    aValue := GetTextDependendParamValue( 0)
  else
    aValue := GetValue;

  case aIndex of
  0  : begin
         if aValue = 127 then
           Result := '100.0'
         else
           Result := FloatToStr( Round(1000 * aValue / 128) / 10);
       end;
  2  : begin // Seq Length, Dly Clk
         Result := IntToStr(aValue + 1);
       end;
  3  : begin // KB
         case aValue of
         0 : Result := 'Off';
         1 : Result := 'On';
         end;
       end;
  4  : begin // Mode
         case aValue of
         0 : Result := 'Poly';
         1 : Result := 'Mono';
         end;
       end;
  5  : begin // Mixer Inv
         case aValue of
         0 : Result := 'Normal';
         1 : Result := 'Inverted';
         end;
       end;
  7  : begin // On/Off
         case aValue of
         0 : Result := 'Muted';
         1 : Result := 'Active';
         end;
       end;
  8  : begin // Dly,Flt On/Off
         case aValue of
         0 : Result := 'Bypass';
         1 : Result := 'Active';
         end;
       end;
  9 : begin // Seq, cycles
         case aValue of
         0 : Result := '1-Cycle';
         1 : Result := 'Loop';
         end;
      end;
  11 : begin // Note quant, Notes
         TempValue := aValue;
         if TempValue = 0 then
           Result := 'Off'
         else
           Result := IntToStr(TempValue);
       end;
  12 : begin // Mystery modules PolarFade/PolarPan...
         Result := IntToStr(aValue);
       end;
  13 : begin // Note detect
         Result := GetKeyName(aValue);
       end;
  16 : begin // Env Sustain
         if (Length(FTextDependencieArray)>1) then
           LevelShiftValue := GetTextDependendParamValue(1)
         else
           LevelShiftValue := 0;

         case LevelShiftValue of
         0 : begin // Pos
               Result := Format('%.1g', [1.0 * round(aValue/2)])
             end;
         1 : begin // PosInv
               Result := Format('%.1g', [1.0 * round(aValue/2)])
             end;
         2 : begin // Neg
               Result := Format('%.1g', [1.0 * round(aValue/2)])
             end;
         3 : begin // NegInv
               Result := Format('%.1g', [1.0 * round(aValue/2)])
             end;
         4 : begin // Bip
               Result := Format('%.1g', [1.0 * round(aValue) - 64])
             end;
         5 : begin // BipInv
               Result := Format('%.1g', [1.0 * round(aValue) - 64])
             end;
         end;
       end;
  17 : begin // RndPattern PatternA / FltPhase FB / NoteZone SendTrans
         case aValue of
         127 : Result := IntToStr(64);
         else
           Result := IntToStr(aValue - 64);
         end;
       end;
  18 : begin // Seq, Pol
         case aValue of
         0 : Result := 'Bipol';
         1 : Result := 'Unipol';
         end;
       end;
  19 : begin // MixStereo Pan
         TempValue := aValue - 64;
         if TempValue = 0 then
           Result := '0'
         else
           if TempValue < 0 then
             Result := IntToStr(TempValue)
           else
             Result := '+' + IntToStr(TempValue);
       end;
  21 : begin //DrumSynth NoiseFltFreq
         Freq := 440 * power(2, (aValue - 65) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  22 : begin // Drumsynth, Masterfreq
         Result := G2FloatToStr(20.02 * power(2, aValue / 24), 4) + 'Hz';
       end;
  23 : begin // Drumsynth, SlaveRatio
         TempValue := aValue;
         case TempValue of
         0  : Result := '1:1';
         48 : Result := '2:1';
         96 : Result := '4:1';
           else begin
             Exponent := (aValue / 48);
             Fact := power(2, Exponent);
             Result :=  'x' + G2FloatToStrFixed( Fact, 4)
           end;
         end;
       end;
  26 : begin // Osc KB
         case aValue of
         0 : Result := 'KBT Off';
         1 : Result := 'KBT On';
         end;
       end;
  28 : begin // Env Attack, Decay, Release
         t := ENV_TIMES[aValue];
         if ENV_TIMES[aValue] < 1.0 then
           Result := Format('%.3g', [t*1000]) + 'm'
         else
           Result := Format('%.3g', [t]) + 's';
       end;
  30 : begin // FreqShift Range
         case aValue of
         0 : Result := 'Sub';
         1 : Result := 'Lo';
         2 : Result := 'Hi';
         end;
       end;
  35 : begin // Note Quant, Range
         TempValue := aValue;
         if TempValue = 0 then
           Result := '0'
         else
           if TempValue = 127 then
             Result := '+-64'
           else
             Result := '+-' + Format('%.1f', [TempValue / 2]);
       end;
  36 : begin // EqPeak Gain
         case aValue of
         127 : Result := G2FloatToStrFixed( 64/3.55555, 4) + 'dB';
         else
           Result := G2FloatToStrFixed( (aValue - 64)/3.55555, 4) + 'dB';
         end;
       end;
  38 : begin // EqPeak Freq
         Freq := 20 * power(2, (aValue) / 13.169);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  39 : begin // FltPhase Freq
         //TextFunction;
         Freq := 100 * power(2, (aValue) / 17.34515804);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  43 : begin // Env, Seq Gate/Trigger
         case aValue of
         0 : Result := 'Trig';
         1 : Result := 'Gate';
         end;
       end;
  45 : begin // ClkGen Tempo
         if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
           iValue1 := GetTextDependendParamValue(1);
           iValue2 := GetTextDependendParamValue(2);
           if iValue1 = 0 then
             Result := '--'
           else
             if iValue2 = 1 then
               Result := 'MASTER'
             else
               Result := G2BPM( aValue) + ' BPM';
         end;
       end;
  46 : begin // LevelShift
         case aValue of
         0 : Result := 'Pos';
         1 : Result := 'PosInv';
         2 : Result := 'Neg';
         3 : Result := 'NegInv';
         4 : Result := 'Bip';
         5 : Result := 'BipInv';
         end;
       end;
  47 : begin // General On/Off
         case aValue of
         0 : Result := 'Off';
         1 : Result := 'On';
         end;
       end;
  49 : begin // Env L1/L2
         case aValue of
         0 : Result := 'L1';
         1 : Result := 'L2';
         end;
       end;
  50 : begin // Env L1/L2/L3/Trig
         case aValue of
         0 : Result := 'L1';
         1 : Result := 'L2';
         2 : Result := 'L3';
         3 : Result := 'Trg';
         end;
       end;
  52 : begin // Fade2-1 Mix
         TempValue := aValue - 64;
         if TempValue = 0 then
           Result := 'Mute'
         else
           if TempValue = -64 then
             Result := 'I1:127'
           else
             if TempValue < 0 then
               Result := 'I1:' + IntToStr(-TempValue*2)
             else
               if TempValue = 63 then
                 Result := 'I2:127'
               else
                 Result := 'I2:' + IntToStr(TempValue*2);
       end;
  53 : begin // Fade1-2 Mix
         TempValue := aValue - 64;
         if TempValue = 0 then
           Result := 'Mute'
         else
           if TempValue = -64 then
             Result := 'O1:127'
           else
             if TempValue < 0 then
               Result := 'O1:' + IntToStr(-TempValue*2)
             else
               if TempValue = 63 then
                 Result := 'O2:127'
               else
                 Result := 'O2:' + IntToStr(TempValue*2);
       end;
  55 : begin // LevMod ModType
         TempValue := aValue;
         case TempValue of
         0 : Result := 'None';
         64 : Result := 'Am';
         127 : Result := 'Rm';
         else
           Result := IntToStr(TempValue);
         end;
       end;
  57 : begin // Mix8-1A Pad
         case aValue of
         0 : Result := '0dB';
         1 : Result := '-6dB';
         2 : Result := '-12dB';
         end;
       end;
  58 : begin // Osc Waveform
         case aValue of
         0 : Result := 'Sine';
         1 : Result := 'Tri';
         2 : Result := 'Saw';
         3 : Result := 'Sqr50';
         4 : Result := 'Sqr25';
         5 : Result := 'Sqr10';
         end;
       end;
  59 : begin // Osc Fine
         TempValue := (aValue-64)*100 div 128;
         if TempValue < 0 then
           Result := IntToStr(TempValue)
         else
           Result := '+' + IntToStr(TempValue);
       end;
  61 : begin // Osc freq
         if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
           FreqCourse := aValue;
           FreqFine := GetTextDependendParamValue(1);
           FreqMode := GetTextDependendParamValue(2);
           Result := FreqDispValue( FreqMode, FreqCourse, FreqFine);
         end;
       end;
  63 : begin // Osc Freq mode
          case aValue of
          0 : Result := 'Semi';
          1 : Result := 'Freq';
          2 : Result := 'Factor';
          3 : Result := 'Partial';
          end;
       end;
  64 : begin // Sw2-1 Sel
          Result := IntToStr( aValue + 1);
       end;
  68 : begin // PartQuant, range
          TempValue := aValue;
          if TempValue > 64 then
            Result := '+-' + IntToStr(Trunc(TempValue / 2)) + '*'
          else
            Result := '+-' + IntToStr(Trunc(TempValue / 2));
       end;
  69 : begin // NoteScaler, range
          TempValue := aValue;
          if TempValue = 127 then
            Result := '+-64.0'
          else
            Result := '+-' + Format('%.1f',[TempValue / 2]);
            if (TempValue mod 24 = 0) then
              Result := Result + '-Oct'
            else
              if ((TempValue mod 24) mod 20 = 0) then
                Result := Result + '-7th'
              else
                if ((TempValue mod 24) mod 14 = 0) then
                  Result := Result + '-5th';
       end;
  70 : begin // Level scaler, Gain
         TempValue := aValue;
         if TempValue < 64  then
           Result := G2FloatToStrFixed( -8.0 + 16.0 * TempValue / 127, 3) + 'dB'
         else
           if TempValue > 64 then
             Result := G2FloatToStrFixed( -8.0 + 16.0 * TempValue / 127, 3) + 'dB'
           else
             Result := '0.0dB';

       end;
  71 : begin // Flt KBT
          case aValue of
          0 : Result := 'KBT Off';
          1 : Result := 'KBT 25%';
          2 : Result := 'KBT 50%';
          3 : Result := 'KBT75%';
          4 : Result := 'KBT 100%';
          end;
       end;
  72 : begin // Flt GC
          case aValue of
          0 : Result := 'GC Off';
          1 : Result := 'GC On';
          end;
       end;
  73 : begin // Flt Slope
          case aValue of
          0 : Result := '6dB';
          1 : Result := '12dB';
          end;
       end;
  74 : begin // FltNord dB/Oct
          case aValue of
          0 : Result := '12dB';
          1 : Result := '24dB';
          end;
       end;
  75 : begin // FltNord FilterType
          case aValue of
          0 : Result := 'LP';
          1 : Result := 'BP';
          2 : Result := 'HP';
          3 : Result := 'BR';
          end;
       end;
  76 : begin // EqPeak BandWidth
           Result := G2FloatToStrFixed( (128 - aValue) / 64, 4) + 'Oct'
       end;
  78 : begin // FltVoice Vowel
          case aValue of
          0 : Result := 'A';
          1 : Result := 'E';
          2 : Result := 'I';
          3 : Result := 'O';
          4 : Result := 'U';
          5 : Result := 'Y';
          6 : Result := 'AA';
          7 : Result := 'AE';
          8 : Result := 'OE';
          end;
       end;
  79 : begin // Vocoder BandSel
         TempValue := aValue;
         case TempValue of
         0 : Result := 'Off';
         else
           Result := IntToStr(TempValue);
         end;
       end;
  80 : begin // Vocoder Emphasis
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
       end;
  81 : begin // Vocoder Monitor
          case aValue of
          0 : Result := 'Active';
          1 : Result := 'Monitor';
          end;
       end;
  82 : begin // Clip, shape
          case aValue of
          0 : Result := 'Asym';
          1 : Result := 'Sym';
          end;
       end;
  83 : begin // Rect Mode
          case aValue of
          0 : Result := 'HalfPos';
          1 : Result := 'HalfNeg';
          2 : Result := 'FullPos';
          3 : Result := 'FullNeg';
          end;
       end;
  84 : begin // ShpStatic Mode
          case aValue of
          0 : Result := 'Inv x3';
          1 : Result := 'Inv x2';
          2 : Result := 'x2';
          3 : Result := 'x3';
          end;
       end;
  88 : begin // Digitizer Rate
         Freq := 440 * power(2, (aValue - 45) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  90 : begin // Reverb RoomType
          case aValue of
          0 : Result := 'Small';
          1 : Result := 'Medium';
          2 : Result := 'Large';
          3 : Result := 'Hall';
          end;
       end;
  93 : begin // FltClassic dB/Oct
          case aValue of
          0 : Result := '12dB';
          1 : Result := '18dB';
          2 : Result := '24dB';
          end;
       end;
  104 : begin // LFO Range
          case aValue of
          0 : Result := 'Sub';
          1 : Result := 'Lo';
          2 : Result := 'Hi';
          3 : Result := 'BPM';
          4 : Result := 'Clk';
          end;
        end;
  105 : begin // LfoA KBT
          case aValue of
          0 : Result := 'KBT off';
          1 : Result := 'KBT 25%';
          2 : Result := 'KBT 50%';
          3 : Result := 'KBT 75%';
          4 : Result := 'KBT 100';
          end;
        end;
  106 : begin // LfoA Wave
          case aValue of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          4 : Result := 'RndStep';
          5 : Result := 'Rnd';
          end;
        end;
  108 : begin // Midi Channel Send
           TempValue := aValue;
           case TempValue of
           0..15 : Result := IntToStr(TempValue + 1);
           16 : Result := 'This';
           17 : Result := 'Slot A';
           18 : Result := 'Slot B';
           19 : Result := 'Slot C';
           20 : Result := 'Slot D';
           end;
        end;
  109 : begin // Midi Channel Receive
           TempValue := aValue;
           case TempValue of
           0..15 : Result := IntToStr(TempValue + 1);
           16 : Result := 'This';
           end;
        end;
  114 : begin // ValSw2-1 Val
          TempValue := aValue;
          if TempValue = 63 then
            Result := '64'
          else
            Result := IntToStr( aValue);
        end;
  122 : begin // logic Pulse/Delay Range
         if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
           t := PULSE_DELAY_RANGE[ aValue];
           iValue1 := GetTextDependendParamValue(1);
           case iValue1 of
           0 : begin // Sub
                 t := t/100;
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           1 : begin // Lo
                 t := t/10;
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           2 : begin // Hi
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           end;
         end;
       end;
  123 : begin // Flt Freq
          FreqCourse := aValue;
          Exponent := (FreqCourse - 60) / 12;
          Freq := 440.0 * power(2, Exponent);
          if Freq >= 1000 then
            //Result := Format('%.4g', [Freq / 1000]) + 'kHz'
            Result := G2FloatToStrFixed( Freq / 1000, 4) + 'kHz'
          else
            //Result := Format('%.4g', [Freq]) + 'Hz';
            Result := G2FloatToStrFixed( Freq, 5) + 'Hz'
        end;
  124 : begin // Flt Resonance
          t := FILTER_RESONANCE[ aValue];
          Result := G2FloatToStrFixed( t, 4);
        end;
  125 : begin // Osc FM Mod
          case aValue of
          0 : Result := 'FM Lin';
          1 : Result := 'FM Trk';
          end;
        end;
  126 : begin // Osc Shape
          Result := IntToStr(trunc(50 + 50.0 * aValue / 128)) + '%';
        end;
  127 : begin // LevConv InputType
          case aValue of
          0 : Result := 'Bipol';
          1 : Result := 'Pos';
          2 : Result := 'Neg';
          end;
        end;
  128 : begin // LfoShpA Shape
          Result := IntToStr(trunc(1 + 97.0 * aValue / 127)) + '%';
        end;
  129 : begin // EnvFollow Attack
          t := ENV_FOLLOW_ATTACK[ aValue];
          if t = 0 then
            Result := 'Fast'
          else
            if t < 1000 then
              Result := G2FloatToStrFixed( t, 4) + 'm'
            else
              Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  130 : begin //EnvFollow release
          t := ENV_FOLLOW_RELEASE[ aValue];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  132 : begin // Key Quant, Range
          TempValue := aValue;
          if TempValue = 127 then
            Result := '+-64.0'
          else
            Result := '+-' + Format('%.1f', [TempValue / 2]);
        end;
  134 : begin // SeqCtrl, XFade
          case aValue of
          0 : Result := '0%';
          1 : Result := '25%';
          2 : Result := '50%';
          3 : Result := '100%';
          end;
        end;
  136 : begin // Env Shape
          case aValue of
          0 : Result := 'LogExp';
          1 : Result := 'LinExp';
          2 : Result := 'ExpExp';
          3 : Result := 'LinLin';
          end;
        end;
  138 : begin // Env NR Button
          case aValue of
          0 : Result := 'Normal';
          1 : Result := 'Reset';
          end;
        end;
  139 : begin // Env Decy/Release
          case aValue of
          0 : Result := 'AD';
          1 : Result := 'AR';
          end;
        end;
  142 : begin // Freq Shift
          if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
            iValue1 := GetTextDependendParamValue(1);
            case iValue1 of
            0 : begin // Sub
                  t := FREQ_SHIFT_SUB[ aValue];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            1 : begin // Lo
                  t := FREQ_SHIFT_LO[ aValue];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            2 : begin // High
                  t := FREQ_SHIFT_HI[ aValue];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            end;
          end;
        end;
  144 : begin // Dly Time/Clk
          case aValue of
          0 : Result := 'Time';
          1 : Result := 'ClkSync';
          end;
        end;
  148 : begin // LevAmp Type
          case aValue of
          0 : Result := 'Lin';
          1 : Result := 'dB';
          end;
        end;
  149 : begin // FX In, pad
          case aValue of
          0 : Result := '+6dB';
          1 : Result := '0dB';
          2 : Result := '-6dB';
          3 : Result := '-12dB';
          end;
        end;
  150 : begin // 2 in source
          case aValue of
          0 : Result := 'In 1/2';
          1 : Result := 'In 3/4';
          2 : Result := 'Bus 1/2';
          3 : Result := 'Bus 3/4';
          end;
        end;
  152 : begin // FX In, source
          case aValue of
          0 : Result := 'Fx 1/2';
          1 : Result := 'Fx 3/4';
          end;
        end;
  155 : begin // OscA Waveform
          case aValue of
          0 : Result := 'Sine1';
          1 : Result := 'Sine2';
          2 : Result := 'Sine3';
          3 : Result := 'Sine4';
          4 : Result := 'TriSaw';
          5 : Result := 'Pulse';
          end;
       end;
  156 : begin // OscB Waveform
          case aValue of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          4 : Result := 'DualSaw';
          end;
       end;
  157 : begin // Glide, shape
          case aValue of
          0 : Result := 'Log';
          1 : Result := 'Lin';
          end;
        end;
  159 : begin // NoiseGate Attack
          t := NOISE_GATE_ATTACK[ aValue];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  160 : begin // NoiseGate Release
          t := NOISE_GATE_RELEASE[ aValue];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  163 : begin // LFO, phase
          Result := IntToStr(round(aValue / 128 * 360));
        end;
  164 : begin // LFO, wave
          case aValue of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          end;
        end;
  165 : begin // LfoShpA Wave
          case aValue of
          0 : Result := 'Sine';
          1 : Result := 'CosBell';
          2 : Result := 'TriBell';
          3 : Result := 'Saw2Tri';
          4 : Result := 'Sqr2Tri';
          5 : Result := 'Sqr';
          end;
        end;
  166 : begin // ClkGen BeatSync
           case aValue of
           0 : Result := '1';
           1 : Result := '2';
           2 : Result := '4';
           3 : Result := '8';
           4 : Result := '16';
           5 : Result := '32';
           end;
        end;
  167 : begin // Overdrive Type
          case aValue of
          0 : Result := 'Soft';
          1 : Result := 'Hard';
          2 : Result := 'Fat';
          3 : Result := 'Heavy';
          end;
        end;
  168 : begin // Overdrive Shape
          case aValue of
          0 : Result := 'Asym';
          1 : Result := 'Sym';
          end;
        end;
  169 : begin // ModAmt ExpLin
          case aValue of
          0 : Result := 'Exp';
          1 : Result := 'Lin';
          end;
        end;
  170 : begin // Phaser Type
          case aValue of
          0 : Result := 'Type I';
          1 : Result := 'Type II';
          end;
        end;
  172 : begin // FltPhase Type
          case aValue of
          0 : Result := '1';
          1 : Result := '2';
          2 : Result := '3';
          3 : Result := '4';
          4 : Result := '5';
          5 : Result := '6';
          end;
        end;
  173 : begin // FltComb Freq
         Freq := 440 * power(2, (aValue - 69) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
        end;
  174 : begin // Compressor Attack
          Result := COMPR_ATTACK_TIMES[ aValue];
        end;
  175 : begin // Compressor Release
          Result := COMPR_RELEASE_TIMES[ aValue];
        end;
  176 : begin // Compressor Treshold
          case aValue of
          42 : Result := 'Off';
          else
            Result := IntToStr(aValue - 30) + 'dB';
          end;
        end;
  177 : begin // Compressor Ratio
          TempValue := aValue;
          if (TempValue >= 0) and (TempValue < 10) then
            Result := G2FloatToStrFixed( 1 + TempValue/10, 3) + ':1'
          else
            if TempValue < 25  then
              Result := G2FloatToStrFixed( TempValue/5, 3) + ':1'
            else
              if TempValue < 35  then
                Result := G2FloatToStrFixed( 5 + (TempValue-25)/2, 3) + ':1'
              else
                if TempValue < 45  then
                  Result := G2FloatToStrFixed( 10 + (TempValue-35), 3) + ':1'
                else
                  if TempValue < 60  then
                    Result := G2FloatToStrFixed( 20 + (TempValue-45)*2, 3) + ':1'
                  else
                    if TempValue <= 66  then
                      Result := G2FloatToStrFixed( 50 + (TempValue-60)*5, 3) + ':1'
        end;
  178 : begin // Compressor RefLevel
          Result := IntToStr( aValue - 30) + 'dB';
        end;
  179 : begin // Midi values & operator
          Result := IntToStr( aValue);
        end;
  180 : begin // Out Pad
          case aValue of
          0 : Result := '0dB';
          1 : Result := '+6dB';
          2 : Result := '+12dB';
          3 : Result := '+18dB';
          end;
        end;
  181 : begin // MonoKey, priority
          case aValue of
          0 : Result := 'Last';
          1 : Result := 'Low';
          2 : Result := 'High';
          end;
        end;
  182 : begin // Out dest
          case aValue of
          0 : Result := 'Out 1/2';
          1 : Result := 'Out 3/4';
          2 : Result := 'Fx 1/2';
          3 : Result := 'Fx 3/4';
          4 : Result := 'Bus 1/2';
          5 : Result := 'Bus 3/4';
          end;
        end;
  183 : begin // 4 Out dest
          case aValue of
          0 : Result := 'Out';
          1 : Result := 'Fx';
          2 : Result := 'Bus';
          end;
        end;
  184 : begin // Eq3Band MidFreq
         Freq := 100 * power(2, aValue / 20.089);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
        end;
  185 : begin // Osc dual Saw phase (0..357)
          Result := IntToStr(round(aValue / 128 * 360));
        end;
  187 : begin // ClkGen Source
          case aValue of
          0 : Result := 'Intern';
          1 : Result := 'Master';
          end;
        end;
  188 : begin // Mix4-1C Pad
          case aValue of
          0 : Result := '0dB';
          1 : Result := '-6dB';
          end;
        end;
  189 : begin // ModAmt InvertMode
          case aValue of
          0 : Result := 'm';
          1 : Result := '1-m';
          end;
        end;
  190 : begin // ClkGen Swing
          Result := Format('%.1f',[50 + 25 * aValue / 127]) + '%';
        end;
  191 : begin // Flt Freq Mod
          Result := Format('%.1f',[200.0 * aValue / 127]) + '%';
        end;
  192 : begin // ShpExp Curve
          case aValue of
          0 : Result := 'x2';
          1 : Result := 'x3';
          2 : Result := 'x4';
          3 : Result := 'x5';
          end;
        end;
  194 : begin // Key Quant, capture
          case aValue of
          0 : Result := 'Closest';
          1 : Result := 'Evenly';
          end;
        end;
  195 : begin // Digitizer Bits
          case aValue of
          12 : Result := 'Off'
          else
            Result := IntToStr(aValue + 1)
          end;
        end;
  196 : begin // Operator FreqDetune
          Result := IntToStr(aValue - 7);
        end;
  197 : begin // Operator BrPoint
          Result := GetKeyName(aValue + 9);
        end;
  199 : begin // Operator RDepthMode
          Result := IntToStr(aValue);
        end;
  200 : begin // Operator freq mode
          case aValue of
          0 : Result := 'Ratio';
          1 : Result := 'Fixed';
          end;
        end;
  202 : begin // PShift Delay
          case aValue of
          0 : Result := '12.5m';
          1 : Result := '25m';
          2 : Result := '50m';
          3 : Result := '100m';
          end;
        end;
  203 : begin // Random Step
          case aValue of
          0 : Result := '0%';
          1 : Result := '25%';
          2 : Result := '50%';
          3 : Result := '75%';
          4 : Result := '100%';
          end;
        end;
  204 : begin // RandomA OutType
          case aValue of
          0 : Result := 'Bip';
          1 : Result := 'Pos';
          2 : Result := 'Neg';
          end;
        end;
  205 : begin // Random Clk A, Step
          Result := IntToStr(100 * aValue div 127) + '%';
        end;
  207 : begin // RandomA Step
          case aValue of
          0 : Result := '25%';
          1 : Result := '50%';
          2 : Result := '75%';
          3 : Result := '100%';
          end;
        end;
  209 : begin // Scratch Ratio
          case aValue of
          64  : Result := 'x0';
          127 : Result := 'x' + G2FloatToStrFixed(4, 4);
          else
            Result := 'x' + G2FloatToStrFixed((aValue - 64)/16, 4);
          end;
        end;
  210 : begin // NoteZone ThruMode
          case aValue of
          0 : Result := 'Notes';
          1 : Result := 'Note+CC';
          end;
        end;
  211 : begin // Automate Echo
          case aValue of
          0 : Result := 'EchoOff';
          1 : Result := 'EchoOn';
          end;
        end;
  212 : begin // RndTrig Step
          Result := IntToStr(100 * aValue div 127) + '%';
        end;
  213 : begin // Eq2Band LoFreq
          case aValue of
          0 : Result := '80 Hz';
          1 : Result := '110 Hz';
          2 : Result := '160 Hz';
          end;
        end;
  214 : begin // Eq2Band HiFreq
          case aValue of
          0 : Result := '6 kHz';
          1 : Result := '8 kHz';
          2 : Result := '12 kHz';
          end;
        end;
  215 : begin // Flanger Rate
          //Result := G2FloatToStr(440 * power(2, ((298.210634 - aValue) / -32.64072819)), 4) + 'Hz';
          t := FLANGER_RATE[ aValue];
          Result := G2FloatToStrFixed(t, 4) + 'Hz';
        end;
  216 : begin // Phaser Freq
          //Result := G2FloatToStr(440 * power(2, ((298.210634 - aValue) / -32.64072819)), 4) + 'Hz';
          t := PHASER_FREQ[ aValue];
          Result := G2FloatToStrFixed(t, 4) + 'Hz';
        end;
  217  : begin // Glide time
           t := GLIDE_TIME[ aValue];
           if t < 1000 then
             Result := G2FloatToStrFixed(t, 4) + 'm'
           else
             Result := G2FloatToStrFixed(t/1000, 4) + 's';
         end;
  218 : begin // DrumSynth NoiseFltMode
          case aValue of
          0 : Result := 'LP';
          1 : Result := 'BP';
          2 : Result := 'HP';
          end;
        end;
  220 : begin // Pitch track and Noise gate threshhold
          t := NOISEGATE_PITCHTRACK_THRESHHOLD[ aValue];
          if aValue = 0 then
            Result := 'Inf.'
          else
            Result := G2FloatToStr(t, 4) + 'dB';
        end;
  500 : begin // SeqCtrl (Not found in original moduledef)
          if assigned(Module) then begin
            TempValue := aValue;
            Param := Module.Parameter[ 33];
            if assigned(Param) then begin // Polarity
              case Param.GetValue of
              0 : Result := IntToStr( TempValue - 64);
              1 : if TempValue = 127 then
                    Result := '64.0'
                  else
                    Result := FloatToStr( Round(TempValue / 2 * 10)/10);
              end;
            end;
          end;
        end;
  501 : begin // Master clock
          Result := IntToStr( aValue);
        end;
  502 : begin // Master clock run
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  503 : begin // Voices
          case aValue of
          0 : Result := 'Legato';
          1 : Result := 'Mono';
          else
            Result := IntToStr(aValue-1);
          end;
        end;
  504 : begin // voice mode
          case aValue of
          0 : Result := 'Poly';
          1 : Result := 'Mono';
          2 : Result := 'Mono';
          end;
        end;
  505 : begin // Arp speed
          case aValue of
          0 : Result := '1/8';
          1 : Result := '1/8T';
          2 : Result := '1/16';
          3 : Result := '1/16T';
          end;
        end;
  506 : begin // Arp on/off
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  507 : begin // Arp dir
          case aValue of
          0 : Result := 'Up';
          1 : Result := 'Down';
          2 : Result := 'Up/Down';
          3 : Result := 'Rnd';
          end;
        end;
  508 : begin // Arp Octaves
          case aValue of
          0 : Result := '1 Oct';
          1 : Result := '2 Oct';
          2 : Result := '3 Oct';
          3 : Result := '4 Oct';
          end;
        end;
  509 : begin // Vibrato Depth
         if aValue = 127 then
           Result := '100 cnt'
         else
           Result := IntToStr(Round(aValue / 128)) + ' cnt';
        end;
  510 : begin // Vibrato Mod Srce
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'AfTouch';
          2 : Result := 'Wheel';
          end;
        end;
  511 : begin // Glide Speed
          t := PATCH_SETTINGS_GLIDE[ aValue];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 3) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  512 : begin // Glide Type
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'Normal';
          2 : Result := 'Auto';
          end;
        end;
  513 : begin // Bend range
          Result := IntToStr(aValue + 1) + ' semi';
        end;
  514 : begin // Bend On/Off
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  515 : begin // Volume
          t := PATCH_SETTINGS_VOLUME[ aValue];
          Result := G2FloatToStr(t, 3) + ' dB';
        end;
  516 : begin // Mute On/Off
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  517 : begin // Sustain
          case aValue of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  518 : begin // Octave Shift
          case aValue of
          0 : Result := '-2';
          1 : Result := '-1';
          2 : Result := '0';
          3 : Result := '1';
          4 : Result := '2';
          end;
        end;
  519 : begin // Morph 1
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Wheel';
          end;
        end;
  520 : begin // Morph 2
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Vel';
          end;
        end;
  521 : begin // Morph 3
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Keyb';
          end;
        end;
  522 : begin // Morph 4
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Aft.Tch';
          end;
        end;
  523 : begin // Morph 5
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Sust.Pd';
          end;
        end;
  524 : begin // Morph 6
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'Ctrl.Pd';
          end;
        end;
  525 : begin // Morph 7
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'P.Stick';
          end;
        end;
  526 : begin // Morph 8
          case aValue of
          0 : Result := 'Knob';
          1 : Result := 'G.Wh2';
          end;
        end;
  else
    //Result := IntToStr(aValue);
    Result := '??? ' + IntToStr(FInfoFunctionIndex) + ' ???';
  end;
end;

// TODO Parameter unit conversion:
// ===============================
// Phaser Freq 216 ??? 216 ???
// Flanger Rate 215 ??? 215 ???
// EnvFollow Attack 129 ??? 129 ???
// EnvFollow Release 130 ??? 130 ???
// NoiseGate Attack 159 ??? 159 ???
// NoiseGate Release 160 ??? 160 ???
// Pulse Range 30 ??? 30 ???
// Delay Range 30 ??? 30 ???
// Patch Volume
// Patch Glide

// Pitch track
// Filter Resonance


function TG2FileParameter.TextFunction: string;
var aValue : byte;
    FreqCourse, FreqFine : Byte;
    Freq, Fact, Temp : single;
    iValue1, iValue2 : integer;
begin
  if (FTextFunctionIndex <> 0) and assigned(Module) and (Length(FTextDependencieArray)>0) then
    aValue := GetTextDependendParamValue( 0)
  else
    aValue := GetValue;

  case FTextFunctionIndex of
  // Zero: displays that are dependend on one parameter only
  0  : begin // Filter freq Nord
         Result := InfoFunction( FInfoFunctionIndex);
       end;
  13 : begin // Filter freq Nord
         Result := InfoFunction( FTextFunctionIndex);
       end;
  2    : begin // DlyClock
           Result := InfoFunction( FTextFunctionIndex);
         end;
  17 : begin // NoteZone SendTrans
         Result := InfoFunction( 17);
       end;
  27 : begin // OscShpB, shape Mod
         Result := InfoFunction( 126);
       end;
  122 : begin // logic Pulse/Delay Range
         Result := InfoFunction( 122);
         {if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
           Temp := PULSE_DELAY_RANGE[ aValue];
           iValue1 := GetTextDependendParamValue(1);
           case iValue1 of
           0 : begin // Sub
                 if Temp < 100000 then
                   Result := G2FloatToStrFixed( Temp / 100, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( Temp / 100000, 4) + 's';
               end;
           1 : begin // Lo
                 if Temp < 100000 then
                   Result := G2FloatToStrFixed( Temp / 10, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( Temp / 10000, 4) + 's';
               end;
           2 : begin // Hi
                 if Temp < 100000 then
                   Result := G2FloatToStrFixed( Temp / 1, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( Temp / 1000, 4) + 's';
               end;
           end;
         end;}
       end;
  39 : begin // FltPhase Freq
         Result := InfoFunction( 39);
       end;
  60   : begin // Osc freq
           Result := InfoFunction( 61);
         end;
  96 : begin // Constant Level
         Result := InfoFunction( 17);
       end;
  99 : begin // Sw1-4 Sel CHECK
          Result := IntToStr( aValue * 4);
        end;
  100 : begin // SwOnOffM On, SwOnOffT On, Sw2-1M Sel, Sw2-1 Sel, Sw1-2M Sel, Sw1-2 Sel CHECK
          Result := IntToStr( aValue * 4);
        end;
  101 : begin // Sw4-1 Sel, Sw8-1 Sel, Sw1-8 Sel CHECK
          Result := IntToStr( aValue * 4);
        end;
  102 : begin // MixFader Lev1
          Result := InfoFunction( 0);
        end;
  103  : begin // LFO Freq
           if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
             iValue1 := GetTextDependendParamValue(1);
             case iValue1 of
             0 : Result := G2FloatToStr( 699 / (aValue+1), 4) + 's';
             1 : if aValue < 32 then
                   Result := G2FloatToStr(1/(0.0159 * power(2, aValue / 12)), 4) + 's'
                 else
                   Result := G2FloatToStr(0.0159 * power(2, aValue / 12), 4) + 'Hz';
             2 : Result := G2FloatToStr(0.2555 * power(2, aValue / 12), 4) + 'Hz';
             3 : Result := G2BPM( aValue);
             4 : begin; // Clock
                   case aValue of
                   0..3 : Result := '64/1';
                   4..7 : Result := '48/1';
                   8..11 : Result := '32/1';
                   12..15 : Result := '24/1';
                   16..19 : Result := '16/1';
                   20..23 : Result := '12/1';
                   24..27 : Result := '8/1';
                   28..31 : Result := '6/1';
                   32..35 : Result := '4/1';
                   36..39 : Result := '3/1';
                   40..43 : Result := '2/1';
                   44..47 : Result := '1/1D';
                   48..51 : Result := '1/1';
                   52..55 : Result := '1/2D';
                   56..59 : Result := '1/1T';
                   60..63 : Result := '1/2';
                   64..67 : Result := '1/4D';
                   68..71 : Result := '1/2T';
                   72..75 : Result := '1/4';
                   76..79 : Result := '1/8D';
                   80..83 : Result := '1/4T';
                   84..87 : Result := '1/8';
                   88..91 : Result := '1/16D';
                   92..95 : Result := '1/8T';
                   96..99 : Result := '1/16';
                   100..103 : Result := '1/32D';
                   104..107 : Result := '1/16T';
                   108..111 : Result := '1/32';
                   112..115 : Result := '1/64D';
                   116..119 : Result := '1/32T';
                   120..123 : Result := '1/64';
                   124..127 : Result := '1/64T';
                   end;
                 end;
             end;
           end;
         end;
  107 : begin // Reverb time
          if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
            Temp := 0;
            iValue1 := GetTextDependendParamValue(1);
            case iValue1 of
            0 : Temp := 3/127 * aValue;
            1 : Temp := 6/127 * aValue;
            2 : Temp := 9/127 * aValue;
            3 : Temp := 12/127 * aValue;
            end;
            if Temp < 1 then
              Result := G2FloatToStrFixed( Temp* 1000, 4) + 'ms'
            else
              Result := G2FloatToStrFixed( Temp, 5) + 's';
          end;
        end;
  108  : begin // Midi dest
           Result := InfoFunction( 108);
         end;
  109 : begin // CtrlRcv Ch, NoteRcv Ch, NoteZone RcvCh
           Result := InfoFunction( 109);
        end;
  110  : begin // ClkGen Tempo
           Result := InfoFunction( 45);
         end;
  133  : begin // SeqVal
           if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
             iValue1 := GetTextDependendParamValue(1);
             case iValue1 of
             0 : begin // Bipol
                   Result := IntToStr(aValue - 64);
                 end;
             1 : begin // Unipol
                   Result := IntToStr(aValue);
                 end;
             end;
           end;
         end;
  137 : begin // Env Sustain
          Result := InfoFunction( 16);
        end;
  140 : begin // Dly time
          if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
            iValue1 := GetTextDependendParamValue(1);
            iValue2 := GetTextDependendParamValue(2);
            if iValue1 = 0 then
              Result := DelayDispValue(0, iValue2, aValue)
            else
              Result := DelayDispValue(4, iValue2, aValue);
          end;
        end;
  141 : begin // Dly time
          if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
            iValue1 := GetTextDependendParamValue(1);
            Result := DelayDispValue(0, iValue1, aValue);
          end;
        end;
  142 : begin // Freq Shift Freq
          Result := InfoFunction( 142);
        end;
  143 : begin // Dly time
          if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
            iValue1 := GetTextDependendParamValue(1);
            iValue2 := GetTextDependendParamValue(2);
            if iValue1 = 0 then
              Result := DelayDispValue(2, iValue2, aValue)
            else
              Result := DelayDispValue(4, iValue2, aValue);
          end;
        end;
  145 : begin // Dly8 time
          if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
            iValue1 := GetTextDependendParamValue(1);
            Result := DelayDispValue(1, iValue1, aValue);
          end;
        end;
  146 : begin // Dly time
          if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
            iValue1 := GetTextDependendParamValue(1);
            iValue2 := GetTextDependendParamValue(2);
            if iValue1 = 0 then
              Result := DelayDispValue(3, iValue2, aValue)
            else
              Result := DelayDispValue(4, iValue2, aValue);
          end;
        end;
  147 : begin // LevAmp Gain TODO
          Result := IntToStr( aValue);
        end;
  166 : begin // ClkGen Beatsync
          Result := InfoFunction( 166);
        end;
  173 : begin // FltComb Freq
          Result := InfoFunction( 173);
        end;
  179 : begin // Midi Values / Operator
          Result := InfoFunction( 179);
        end;
  198 : begin // Operator freq
          if assigned(Module) and (Length(FTextDependencieArray)=3) then begin
            iValue1 := GetTextDependendParamValue(1);
            iValue2 := GetTextDependendParamValue(2);
            case iValue2 of
            0 : begin // Ratio  x0.50...x16.00..x31.00
                  if aValue = 0 then begin
                    Fact := 0.5;
                  end else begin
                    Fact := aValue;
                  end;
                 Fact := Fact + Fact * iValue1 / 100;
                 Result :=  'x' + G2FloatToStrFixed( Fact, 4)
                end;
            1 : begin // Fixed 1Hz,10Hz,100Hz,1000Hz etc
                 Freq := trunc(power(10, (aValue mod 4)));
                  if Freq >= 1000 then
                    Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
                  else
                    Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
                end;
            end;
          end;
        end;
  201 : begin // PShift ShiftSemi
          if assigned(Module) and (Length(FTextDependencieArray)=2) then begin
            FreqCourse := aValue;
            FreqFine := GetTextDependendParamValue(1);
            Result := FreqDispValue( 4, FreqCourse, FreqFine);
          end;
        end;
  217 : begin // Glide time
          Result := InfoFunction( 217);
        end;
  // For VST:
{ 1000 : Result := string(FModuleName);
  1001 : begin
           case LineNo of
           0 : Result := string(FParamName);
           1 : Result := InfoFunction;
           end;
         end;
  1002 : begin
           Result := IntToStr(FPatch.Slot.SlotIndex + 1) + ':' + string(FModuleName);
         end;
  1003 : begin
           case LineNo of
           0 : Result := string(FParamName);
           1 : Result := IntToStr(GetParameterValue);
           end;
         end;}
  else begin
      //Result := IntToStr(aValue);
      RESULT := '*** ' + IntToStr(FTextFunctionIndex) + ' ***';
    end;
  end;
end;

procedure TG2FileParameter.InvalidateControl;
begin
  NotifyObservers( EvtInvalidate);
end;

//------------------------------------------------------------------------------
//
//                                  TG2FileLed
//
//------------------------------------------------------------------------------

constructor TG2FileLed.Create( const aLedType : TLedType; const aLocation : TLocationType; const aModuleIndex, aGroupID: byte; const aGroupCount : integer);
begin
  FObserverList := TList<IG2Observer>.Create;

  FLocation := aLocation;
  FModuleIndex := aModuleIndex;
  FLedType := aLedType;
  FGroupID := aGroupID;
  FGroupCount := aGroupCount;
end;

destructor TG2FileLed.Destroy;
begin
  FObserverList.Free;
  inherited;
end;

procedure TG2FileLed.DeassignControl( const aControl : IG2ParamObserver);
begin
  //
end;

function TG2FileLed.GetGroupCount: integer;
begin
  Result := FGroupCount;
end;

function TG2FileLed.GetGroupID: byte;
begin
  Result := FGroupID;
end;

function TG2FileLed.GetHighValue: byte;
begin
  Result := 127;
end;

function TG2FileLed.GetLedType: TLedType;
begin
  Result := FLedType;
end;

function TG2FileLed.GetLocation: TLocationType;
begin
  Result := FLocation;
end;

function TG2FileLed.GetLowValue: byte;
begin
  Result := 0;
end;

function TG2FileLed.GetModuleIndex: byte;
begin
  Result := FModuleIndex;
end;

function TG2FileLed.GetSelectedMorphValue: byte;
begin
  Result := 0;
end;

function TG2FileLed.GetValue: byte;
begin
  Result := FValue;
end;

function TG2FileLed.GetValueText(aIndex: integer): string;
begin
  Result := '';
end;

function TG2FileLed.HasMorph: boolean;
begin
  Result := False;
end;

procedure TG2FileLed.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
    i : integer;
begin
  //for Observer in FObserverList do
  //  Observer.Update(aG2Event);
  for i := 0 to FObserverlist.count - 1 do begin
    FObserverlist[i].Update(aG2Event);
  end;
end;

procedure TG2FileLed.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2FileLed.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

procedure TG2FileLed.SetValue(const aValue: byte);
begin
  if aValue <> FValue then begin
    FValue := aValue;
    NotifyObservers( EvtValueChange);
  end;
end;

//------------------------------------------------------------------------------
//
//                               TG2FileSlotSettings
//
//------------------------------------------------------------------------------

constructor TG2FileSlot.Create( AOwner: TComponent);
begin
  inherited;

  FG2 := nil;
  FPerformance := nil;
  FPatch := CreatePatch;
  FPatch.Slot := self;
  FPatch.G2 := FG2;

  FObserverList := TList<IG2Observer>.Create;

  Init;
end;

destructor TG2FileSlot.Destroy;
begin
  FObserverList.Free;
  inherited;
end;

function TG2FileSlot.CreatePatch : TG2FilePatch;
begin
  Result := TG2FilePatch.Create( self);
end;

procedure TG2FileSlot.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2FileSlot.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

procedure TG2FileSlot.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event);
end;

procedure TG2FileSlot.Init;
begin
  //for i := 0 to 3 do
  //  FUnknown[i] := 0;
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;

  FPatchName         := 'No name';
  FEnabled           := $01;
  FKeyboard          := $00;
  FHold              := $00;
  FKeyboardRangeFrom := $00;
  FKeyboardRangeTo   := $7f;
  FSlot              := $00;
  FBankIndex         := $00;
  FPatchIndex        := $00;

  FPatch.Init;
end;

procedure TG2FileSlot.SetG2( aValue : TG2File);
begin
  FG2 := aValue;
  if assigned( FPatch) then
    FPatch.G2 := aValue;
end;

procedure TG2FileSlot.SetPerformance( aValue : TG2FilePerformance);
begin
  FPerformance := aValue;
  if assigned( FPatch) then begin
    FPatch.Performance := aValue;
  end;
end;

procedure TG2FileSlot.SetPatch( aValue : TG2FilePatch);
begin
  FPatch := aValue;
  if assigned( FPatch) then begin
    FPatch.G2 := FG2;
  end;
end;

procedure TG2FileSlot.Read( aChunk : TPatchChunk);
begin
  FPatchName         := aChunk.ReadName;
  FEnabled           := aChunk.ReadBits( 8);
  FKeyboard          := aChunk.ReadBits( 8);
  FHold              := aChunk.ReadBits( 8);
  FBankIndex         := aChunk.ReadBits( 8);
  FPatchIndex        := aChunk.ReadBits( 8);
  FKeyboardRangeFrom := aChunk.ReadBits( 8);
  FKeyboardRangeTo   := aChunk.ReadBits( 8);
  //FSlot              := aChunk.ReadBits( 8); // 2011-11-14 bve : First G2 gives 1..4, second G2 gives 5..8..?
  FUnknown2          := aChunk.ReadBits( 8);
  FUnknown3          := aChunk.ReadBits( 8);
  FUnknown4          := aChunk.ReadBits( 8);
end;

procedure TG2FileSlot.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteName( FPatchName);
  aChunk.WriteBits( FEnabled,           8);
  aChunk.WriteBits( FKeyboard,          8);
  aChunk.WriteBits( FHold,              8);
  aChunk.WriteBits( FBankIndex,         8);
  aChunk.WriteBits( FPatchIndex,        8);
  aChunk.WriteBits( FKeyboardRangeFrom, 8);
  aChunk.WriteBits( FKeyboardRangeTo,   8);
  //aChunk.WriteBits( FSlot,              8);
  aChunk.WriteBits( FUnknown2,          8);
  aChunk.WriteBits( FUnknown3,          8);
  aChunk.WriteBits( FUnknown4,          8);
end;

function TG2FileSlot.GetSlotIndex: byte;
begin
  Result := FSlot - 1;
end;

procedure TG2FileSlot.SetSlotIndex( aSlotIndex : byte);
begin
  FSlot := aSlotIndex + 1;
end;

function TG2FileSlot.GetPatch: TG2FilePatch;
begin
  Result := FPatch;
end;

procedure TG2FileSlot.SetEnabled( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot enabled must be 0 or 1.');
  FEnabled := aValue;
end;

procedure TG2FileSlot.SetHold( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot hold must be 0 or 1.');
  FHold := aValue;
end;

procedure TG2FileSlot.SetKeyboard( aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot keyboard must be 0 or 1.');
  FKeyboard := aValue;
end;

procedure TG2FileSlot.SetKeyboardRangeTo( aValue : TBits8);
begin
  FKeyboardRangeTo := aValue; // can be from 0 to 255
end;

procedure TG2FileSlot.SetKeyboardRangeFrom( aValue : TBits8);
begin
  FKeyboardRangeFrom := aValue;
end;

//------------------------------------------------------------------------------
//
//                             TGlobalKnob
//
//------------------------------------------------------------------------------

procedure TGlobalKnob.Init;
begin
 FAssigned := 0;
 FLocation := 0;
 FModuleIndex := 0;
 FIsLed := 0;
 FParamIndex := 0;
 FSlotIndex := 0;

 FParameter := nil;
end;

procedure TGlobalKnob.Read( aChunk : TPatchChunk);
begin
  // Fassigned : TBits1;
  // FLocation : TBits2;
  // FModuleIndex : TBits8;
  // FIsLed : TBits2;
  // FParamIndex: TBits7;
  // FSlotIndex : TBits2

  FAssigned    := aChunk.ReadBits( 1);
  if FAssigned = 1 then begin

    FLocation    := aChunk.ReadBits( 2);
    FModuleIndex := aChunk.ReadBits( 8);
    FIsLed       := aChunk.ReadBits( 2);
    FParamIndex  := aChunk.ReadBits( 7);
    FSlotIndex   := aChunk.ReadBits( 2);
  end;
end;

procedure TGlobalKnob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(FLocation, 2);
    aChunk.WriteBits(FModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(FParamIndex, 7);
    aChunk.WriteBits(FSlotIndex, 2);
  end;
end;

//------------------------------------------------------------------------------
//
//                             TGlobalKnobList
//
//------------------------------------------------------------------------------

constructor TGlobalKnobList.Create( AOwnsObjects : boolean; aPerf : TG2FilePerformance);
var i : integer;
    GlobalKnob : TGlobalKnob;
begin
  inherited Create( AOwnsObjects);
  FPerf := aPerf;

  FKnobCount := 120;
  for i := 0 to FKnobCount - 1 do begin
    GlobalKnob := TGlobalKnob.Create;
    GlobalKnob.FKnobIndex := i;
    Add( GlobalKnob);
  end;
end;

destructor TGlobalKnobList.Destroy;
begin
  inherited;
end;

function TGlobalKnobList.GetGlobalKnob( aIndex : integer) : TGlobalKnob;
begin
  result := TGlobalKnob(inherited Items[aindex]);
end;

procedure TGlobalKnobList.SetGlobalKnob( aIndex : integer; const aValue : TGlobalKnob);
begin
  inherited Items[aindex] := aValue;
end;

function TGlobalKnobList.AddGlobalKnob( aValue : TGlobalKnob): integer;
begin
  Result := inherited Add( aValue);
end;

procedure TGlobalKnobList.Init;
var i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Init;
end;

function TGlobalKnobList.FindGlobalKnobIndex( aSlotIndex : byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(( Items[i].FAssigned = 1)
                        and ( Items[i].FSlotIndex = aSlotIndex)
                        and ( Items[i].FLocation = TBits2(aLocation))
                        and ( Items[i].FModuleIndex = aModuleIndex)
                        and ( Items[i].FParamIndex = aParamIndex)) do
    inc(i);

  if (i < Count) then begin
    Result := i
  end else
    Result := -1;
end;

procedure TGlobalKnobList.DeleteModule( aSlotIndex, aLocation, aModuleIndex : Byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    if (Items[i].FModuleIndex = aModuleIndex)  and (Items[i].FLocation = aLocation) and (Items[i].FSlotIndex = aSlotIndex) then
      Items[i].FAssigned := 0;
end;

procedure TGlobalKnobList.Read( aChunk : TPatchChunk);
var i : integer;
begin
  FKnobCount := aChunk.ReadBits( 16);

  if assigned(aChunk.FLogLines) then begin
    aChunk.FLogLines.Add('KnobList, count = ' + IntToStr(FKnobCount));
    aChunk.FLogLines.Add('Knb Loc Mod Isl Par Slt');
  end;

  for i := 0 to FKnobCount - 1 do begin
    Items[i].Read( aChunk);

    if assigned( aChunk.FLogLines) and (Items[i].FAssigned = 1) then
      aChunk.FLogLines.Add( Format('%3d %3d %3d %3d %3d %3d', [i, Items[i].FLocation, Items[i].FModuleIndex, Items[i].FIsLed, Items[i].FParamIndex, Items[i].FSlotIndex]));
  end;
end;

procedure TGlobalKnobList.Write( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits(FKnobCount, 16);
  for i := 0 to FKnobCount - 1 do
    Items[i].Write( aChunk);
end;

//------------------------------------------------------------------------------
//
//                            TG2FilePerformance
//
//------------------------------------------------------------------------------

constructor TG2FilePerformance.Create( AOwner: TComponent);
var i : integer;
begin
  inherited;

  FG2 := nil;

  SetLength(FSLotArray, NSLOTS);
  for i := 0 to NSLOTS - 1 do begin
    FSlotArray[i] := CreateSlot;
    FSlotArray[i].G2 := FG2;
    FSlotArray[i].Performance := self;
    FSlotArray[i].SlotIndex := i;
  end;

  FGlobalKnobList := TGlobalKnobList.Create( True, self);

  FObserverList := TList<IG2Observer>.Create;

  init;
end;

function TG2FilePerformance.CreateSlot : TG2FileSlot;
begin
  Result := TG2FileSlot.Create( self);
end;

procedure TG2FilePerformance.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2FilePerformance.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

procedure TG2FilePerformance.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event);
end;


function TG2FilePerformance.GetSlot( aSlot : byte): TG2FileSlot;
begin
  Result := FSlotArray[ aSlot];
end;

procedure TG2FilePerformance.SetG2(aValue: TG2File);
var i : integer;
begin
  FG2 := aValue;
  if assigned(FG2) then begin
    for i := 0 to NSLOTS - 1 do
      FSlotArray[i].G2 := FG2;
  end;
end;

procedure TG2FilePerformance.Init;
var i : integer;
begin
  FPatchVersion   := PATCH_VERSION;
  FPatchType := 1;

  //for i := 0 to 8 do
  //  FUnknown[i] := 0;
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;
  FUnknown5 := 0;
  FUnknown6 := 0;
  FUnknown7 := 0;
  FUnknown8 := 0;
  FUnknown9 := 0;

  FKeyboardRangeEnabled := 0;
  FMasterClock          := $78; // 120 BPM
  FMasterClockRun       := 0;

  for i := 0 to NSLOTS - 1 do begin
    FSlotArray[i].Init;
    if i = 0 then
      FSlotArray[i].FKeyboard := $01;
    FSlotArray[i].SlotIndex := i;
  end;

  FGlobalKnobList.Init;
  FSelectedParamPage := 0;
  FSelectedParamPageColumn := 0;
end;

procedure TG2FilePerformance.InitKnobs;
var i : integer;
    Knob : TGlobalKnob;
    Module : TG2FileModule;
    Param : TG2FileParameter;
begin
  for i  := 0 to FGlobalKnobList.Count - 1 do begin
    Knob := FGlobalKnobList.Items[i];
    if Knob.Fassigned = 1 then begin
      case TLocationType(Knob.FLocation) of
      {ltPatch : begin
                  Param := FSlotArray[ Knob.FSlotIndex].GetPatch.Parameter[ Knob.ModuleIndex, Knob.FParamIndex];
                  if assigned(Param) then
                    Param.AssignGlobalKnob( self, Knob.FSlotIndex, i);
                end;}
      ltPatch,
      ltFX,
      ltVA    : begin
                  Module := FSlotArray[ Knob.FSlotIndex].GetPatch.GetModule( Knob.FLocation, Knob.FModuleIndex);
                  if assigned(Module) then begin
                    Param := Module.Parameter[ Knob.FParamIndex];
                    if assigned(Param) then
                      Param.AssignGlobalKnob( self, Knob.FSlotIndex, i);
                  end;
                end;
      end;
    end;
  end;
end;

destructor TG2FilePerformance.Destroy;
begin
  FObserverList.Free;

  FGlobalKnobList.Free;
  Finalize(FSlotArray);
  inherited;
end;

procedure TG2FilePerformance.DoSelectSlot;
begin
  NotifyObservers( EvtSelectSlot);

  if assigned(G2.OnSelectSlot) then
    G2.OnSelectSlot( self, G2.ID, FSelectedSlot);
end;

procedure TG2FilePerformance.InitSelectedSlotIndex(aValue: TBits2);
begin
  FSelectedSlot := aValue;
end;

procedure TG2FilePerformance.SetSelectedSlotIndex( aValue : TBits2);
begin
  InitSelectedSlotIndex( aValue);
  //FSelectedSlot := aValue;
  //if assigned(FG2) and assigned(FG2.FOnSelectSlot) then
  //  FG2.FOnSelectSlot( self, FG2.ID, aValue);
end;

{procedure TG2FilePerformance.SetMaxVariations( aValue : Byte);
var i : integer;
begin
  for i := 0 to NSLOTS-1 do
    FSlotArray[i].Patch.MaxVariations := aValue;
end;

function TG2FilePerformance.GetMaxVariations : Byte;
begin
  Result := 0
end;}

procedure TG2FilePerformance.SetPerformanceName( aValue : string);
begin
  if aValue.Length > 16 then
    raise Exception.Create('Name length must be 16 max.');

  if aValue = '' then
    aValue := 'No name';
  //  raise Exception.Create('Invalid name');

  FPerformanceName := aValue;
end;

procedure TG2FilePerformance.SetMasterClock( aValue : TBits8);
begin
  if (aValue < 30) or (aValue > 240) then
    raise Exception.Create('Master clock must be between 30 and 240.');

  FMasterClock := aValue;
end;

procedure TG2FilePerformance.SetMasterClockRun( aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Master clock run must be 0 or 1.');

  FMasterClockRun := aValue;
end;

procedure TG2FilePerformance.SetKeyboardRangeEnabled( aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Keyboard range enable must be 0 or 1.');

  FKeyboardRangeEnabled := aValue;
end;

procedure TG2FilePerformance.Read( aChunk : TPatchChunk);
var einde : boolean;
    b : byte;
    i, PatchCount : integer;
begin
  // FUnknown              : TBits8;
  // FPerformanceName      : 16TBits8 or ends with #0
  // FKeyboardRangeEnabled : TBits7;
  // FMasterClock          : TBits7;
  // FMasterClockRun       : TBits7;
  // FSettings             : array of TPerformanceSetting;

  PatchCount := 0;
  einde := False;
  repeat
    case aChunk.FId of
      C_PERF_NAME :
        begin
          FPerformanceName := aChunk.ReadName;
        end;
      C_PERF_SETTINGS :
        begin // performance description
          FUnknown2             := aChunk.ReadBits( 8);
          FUnknown3             := aChunk.ReadBits( 4);
          b                     := aChunk.ReadBits( 2);
          InitSelectedSlotIndex(b);
          FUnknown4             := aChunk.ReadBits( 2);

          FKeyboardRangeEnabled := aChunk.ReadBits( 8);
          FMasterClock          := aChunk.ReadBits( 8);
          FUnknown5             := aChunk.ReadBits( 8);
          FMasterClockRun       := aChunk.ReadBits( 8);
          FUnknown6             := aChunk.ReadBits( 8);
          FUnknown7             := aChunk.ReadBits( 8);

          if assigned(aChunk.FLogLines) then begin
            aChunk.FLogLines.Add('Perf settings:');
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown2));
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown3));
            aChunk.FLogLines.Add('Selected slot : ' + IntToStr(FSelectedSlot));
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown4));
            aChunk.FLogLines.Add('Keyb range : ' + IntToStr(FKeyboardRangeEnabled));
            aChunk.FLogLines.Add('Materclock : ' + IntToStr(FMasterClock));
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown5));
            aChunk.FLogLines.Add('MClock run : ' + IntToStr(FMasterClockRun));
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown6));
            aChunk.FLogLines.Add('Unknown : ' + IntToStr(FUnknown7));
          end;

          for i := 0 to NSLOTS - 1 do
            GetSlot( i).Read( aChunk);
          PatchCount := 0;
        end;
      C_PATCH_DESCR :
        begin // Patch description
          GetSlot( PatchCount).GetPatch.Read( aChunk);
          inc(PatchCount);
          //FPatchDescription.read( aChunk);
        end ;
      C_KNOBS_GLOBAL :
        begin // Unknown
          FGlobalKnobList.Read( aChunk);
        end ;
    end;

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;

  until einde;
  InitKnobs;
end;

procedure TG2FilePerformance.WriteSettings( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits( FUnknown2,             8);
  aChunk.WriteBits( FUnknown3,             4);
  aChunk.WriteBits( FSelectedSlot,         2);
  aChunk.WriteBits( FUnknown4,             2);


  aChunk.WriteBits( FKeyboardRangeEnabled, 8);
  aChunk.WriteBits( FMasterClock,          8);
  aChunk.WriteBits( FUnknown5,             8);
  aChunk.WriteBits( FMasterClockRun,       8);
  aChunk.WriteBits( FUnknown6,             8);
  aChunk.WriteBits( FUnknown7,             8);

  for i := 0 to NSLOTS - 1 do
    GetSlot( i).Write( aChunk);
  aChunk.WriteChunk( C_PERF_SETTINGS);
end;


procedure TG2FilePerformance.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  // Write patch data
  for i := 0 to NSLOTS - 1 do begin
    GetSlot( i).GetPatch.Write( aChunk, aVariationCount);
  end;

  FGlobalKnobList.Write( aChunk);
  aChunk.WriteChunk( C_KNOBS_GLOBAL);
end;

function TG2FilePerformance.LoadFromFile(aStream : TStream; aLogLines : TStrings): Boolean;
var
  sl : TStringList;
  Chunk : TPatchChunk;
  b  : byte;
  bm, bl : Byte;
  s  : string;
  Crc : Word;
begin
  Result := False;

  Chunk := TPatchChunk.Create( aStream);
  sl := TStringList.Create;
  try
    if assigned(aLogLines) then
      Chunk.FLogLines := aLogLines;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( b, SizeOf( b));
      case b of
      $0d :; // just skip it
      $0a : begin
               sl.Add(s);
               s := '';
             end;
      $00 : Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    Init;
    Chunk.ReadBuffer( 2);
    FPatchVersion := Chunk.ReadBits( 8);
    FPatchType := Chunk.ReadBits( 8);
    Chunk.ReadChunk;
    Read( Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    Result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

class function TG2FilePerformance.LoadFileStream( AOwner : TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  Result := TG2FilePerformance.Create( AOwner);
  aChunk.ReadChunk;
  Result.Read( aChunk);
end;

procedure TG2FilePerformance.SaveToFile( aStream : TStream);
var //sl : TStringList;
    //i : integer;
    //c : AnsiChar;
    //s : AnsiString;
    Chunk : TPatchChunk;
begin
  //sl := TStringList.Create;
  Chunk := TPatchChunk.Create( aStream);
  try
    {sl.Clear;
    sl.Add('Version=Nord Modular G2 File Format 1');
    sl.Add('Type=Performance');
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := AnsiString(sl[i]);
      aStream.Write( s[1], Length( s));
      c := #$0d;
      aStream.Write( c, SizeOf( c));
      c := #$0a;
      aStream.Write( c, SizeOf( c));
    end;

    c := #0;
    aStream.Write( c, SizeOf( c));}

    AddHeaderInfo( pftPerf, aStream);

    Chunk.WriteBits(FPatchVersion, 8);
    Chunk.WriteBits(FPatchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;
    WriteSettings(Chunk);
    Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
    //sl.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TG2FilePerformance.SaveAsFXB( aStream : TStream);
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  MemStream := TMemoryStream.Create;
  try
    SaveToFile(MemStream);

    StringToByteArray( 'CcnK', FXBHeader.chunkMagic, 4);
    FXBHeader.byteSize    := SwapBytes(152 + MemStream.Size);
    StringToByteArray( 'FBCh', FXBHeader.fxMagic, 4);
    FXBHeader.version     := SwapBytes(1);
    StringToByteArray( 'NMG2', FXBHeader.fxID, 4);
    FXBHeader.fxVersion   := SwapBytes(1);
    FXBHeader.numPrograms := SwapBytes(1);
    Fillchar(FXBHeader.Future, 128, #0);

    FXBHeader.chunkSize   := SwapBytes(MemStream.Size);

    aStream.Write(FXBHeader, SizeOf(FXBHeader));
    aStream.Write(MemStream.Memory^, MemStream.Size);

  finally
    MemStream.Free;
  end;
end;

function TG2FilePerformance.LoadFromFXB( aStream : TStream): boolean;
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  Result := False;
  MemStream := TMemoryStream.Create;
  try
    aStream.Read( FXBHeader.chunkMagic, SizeOf( FXBHeader.chunkMagic));
    if ByteArrayToString( FXBHeader.chunkMagic, 4) <> 'CcnK' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.byteSize, SizeOf( FXBHeader.byteSize));
    FXBHeader.byteSize := SwapBytes(FXBHeader.byteSize);
    aStream.Read( FXBHeader.fxMagic, SizeOf( FXBHeader.fxMagic));
    if ByteArrayToString( FXBHeader.chunkMagic, 4) <> 'FBCh' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.version, SizeOf( FXBHeader.version));
    FXBHeader.version := SwapBytes(FXBHeader.version);
    aStream.Read( FXBHeader.fxID, SizeOf( FXBHeader.fxID));
    if ByteArrayToString( FXBHeader.fxID, 4) <> 'NMG2' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.fxVersion, SizeOf( FXBHeader.fxVersion));
    FXBHeader.fxVersion := SwapBytes(FXBHeader.fxVersion);
    aStream.Read( FXBHeader.numPrograms, SizeOf( FXBHeader.numPrograms));
    FXBHeader.numPrograms := SwapBytes(FXBHeader.numPrograms);
    aStream.Read( FXBHeader.Future, 128);
    aStream.Read( FXBHeader.chunkSize, SizeOf(FXBHeader.chunkSize));
    FXBHeader.chunkSize := SwapBytes(FXBHeader.chunkSize);

    LoadFromFile( aStream, nil);

    Result := True;
  finally
    MemStream.Free;
  end;
end;
{$ENDIF}

function TG2FilePerformance.GetNoOffExtendedModules : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to NSLOTS - 1 do
    Result := Result + GetSlot( i).GetPatch.GetNoOffExtendedModules;
end;


procedure TG2FilePerformance.DeleteModuleFromPerf( aSlotIndex : Byte; aLocation : TLocationType; aModule: TG2FileModule);
var aModuleIndex : Byte;
begin
  aModuleIndex := aModule.ModuleIndex;
  FGlobalKnobList.DeleteModule( aSlotIndex, ord(aLocation), aModuleIndex);
end;

function TG2FilePerformance.AssignGlobalKnobInPerf(aKnobIndex: integer; aSlotIndex: byte; aLocation : TLocationType; aModuleIndex, aParamIndex : byte): TGlobalKnob;
var Knob : TGlobalKnob;
begin
  Result := nil;

  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FGlobalKnobList.Count then begin
    Knob := FGlobalKnobList.Items[ aKnobIndex];
    Knob.FAssigned := 1;
    Knob.FLocation := ord(aLocation);
    Knob.FModuleIndex := aModuleIndex;
    Knob.FIsLed := 0;
    Knob.FParamIndex := aParamIndex;
    Knob.FSlotIndex := aSlotIndex;

    Result := Knob;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

procedure TG2FilePerformance.DeassignGlobalKnobInPerf( aKnobIndex: integer);
var Knob : TGlobalKnob;
begin
  if aKnobIndex < 0 then
    exit;

  if aKnobIndex < FGlobalKnobList.Count then begin
    Knob := FGlobalKnobList.Items[ aKnobIndex];
    Knob.FAssigned := 0;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

function TG2FilePerformance.GetGlobalKnob( KnobIndex: integer): TGlobalKnob;
begin
  if (KnobIndex >= 0) and (KnobIndex < FGlobalKnobList.Count) then begin
    Result := FGlobalKnobList.Items[ KnobIndex]
  end else
    Result := nil;
end;

//------------------------------------------------------------------------------
//
//                               TG2DataStream
//
//------------------------------------------------------------------------------

procedure AddHeaderInfo( aPatchFileType : TPatchFileType; aStream : TStream);
var sl : TStringList;
    i, j : integer;
    b : byte;
    s : string;
begin
  sl := TStringList.Create;
  try
    sl.Clear;

    sl.Add('Version=Nord Modular G2 File Format 1');
    case aPatchFileType of
      pftPatch: sl.Add('Type=Patch');
      pftPerf: sl.Add('Type=Performance');
    end;
    sl.Add('Version=23');
    sl.Add('Info=BUILD 320');

    aStream.Position := 0;

    for i := 0 to sl.Count - 1 do
    begin
      s := sl[i];
      for j := 0 to s.Length - 1 do begin
        b := byte(s.Chars[j]);
        aStream.Write( b, 1);
      end;
      b := $0d;
      aStream.Write( b, SizeOf( b));
      b := $0a;
      aStream.Write( b, SizeOf( b));
    end;

    b := 0;
    aStream.Write( b, SizeOf( b));
  finally
    sl.Free;
  end;
end;

constructor TG2FileDataStream.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor  TG2FileDataStream.Destroy;
begin
  inherited;
end;

procedure TG2FileDataStream.Read( aChunk : TPatchChunk);
begin
  // Abstract
end;

procedure TG2FileDataStream.Write( aChunk : TPatchChunk; aVariationCount : byte);
begin
  // Abstract
end;


function TG2FileDataStream.LoadFileHeader( aStream : TStream; aChunk : TPatchChunk): boolean;
var sl : TStringList;
    s : string;
    b : byte;
begin
  Result := False;

  sl := TStringList.Create;
  try
    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( b, SizeOf( b));
      case b of
      $0d :; // just skip it
      $0a : begin
              sl.Add(s);
              s := '';
             end;
      $00 : Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    aChunk.ReadBuffer( 2);
    FPatchVersion := aChunk.ReadBits( 8);
    FPatchType := aChunk.ReadBits( 8);
  finally
    sl.Free;
  end;
end;

function TG2FileDataStream.ReadClaviaString( aStream : TStream): string;
var b : byte;
begin
  Result := '';
  aStream.Read(b, 1);
  while (b <> 0) do begin
    Result := Result + Char(b);
    if Result.Length = 16 then
      break;
    aStream.Read(b, 1);
  end;
end;

procedure TG2FileDataStream.WriteClaviaString( aStream : TStream; aValue : string);
var i : integer;
    b : Byte;
begin
  i := 0;
  while (i < aValue.Length) and (i<16) do begin
    b := byte(aValue.Chars[i]);
    if aStream.Write( b, 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i<16) then begin
    b := 0;
    if aStream.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

function TG2FileDataStream.CreateFromMidiStream(const aStream: TStream; aLogLines : TStrings): TMemoryStream;
var MData, i, SlotIndex, Offs : integer;
    b : byte;
    FName        : string;
    BlockCount   : integer;
    CurrentBlock : integer;
    BitWriter    : TBitWriter;
    Line         : string;
    DeviceID     : byte;
begin
  BlockCount     := 0;
  CurrentBlock   := 0;
  Offs := 0;

  Result := TMemoryStream.Create;

  BitWriter := TBitWriter.Create;
  try
    aStream.Position := 0;
    while aStream.Position < aStream.Size do Begin
      Line := '';
      FChecksum := 0;
      ParseMidiConst( aStream, $f0);            // Start sysex
      ParseMidiConst( aStream, $33);            // Clavia
      //ParseMidiConst( aStream, $7f);            // ?
      DeviceID := ParseMidiSeptet( aStream);
      ParseMidiConst( aStream, $0a);            // G2

      b := ParseMidiSeptet( aStream);
      case b of
        $20 : PatchType := PATCH_DATA;
        $28 : PatchType := PERF_DATA;
        else
          raise Exception.Create( 'Invalid MIDI, data type changed during load');
      end;

      ParseMidiConst( aStream, $00);            // ?
      SlotIndex := ParseMidiSeptet(aStream);
      ParseMidiConst( aStream, $00);            // ?
      MData := ParseMidiInt( aStream);          // Block Number

      Line := Line + 'Block : ' + IntToStr(MData);

      if MData <> CurrentBlock then
        raise Exception.Create(Format( 'Invalid MIDI, block %d expected but block %d seen', [ CurrentBlock, MData]));

      MData := ParseMidiInt( aStream);          // Block count

      Line := Line + ' Count : ' + IntToStr(MData);

      if BlockCount = 0 then
        BlockCount := MData
      else
        if BlockCount <> MData then
          raise Exception.Create(Format( 'Block count changed during MIDI load (%d -> %d)', [ BlockCount, MData]));


      FName := ParseMidiString( aStream);       // Patch name

      Line := Line + ' Name : ' + string(FName) + ' Data : ';

      MData := ParseMidiInt( aStream);          // Septet count
      for i := 0 to MData - 2 do begin
        aStream.Read( b, 1);
        FChecksum := ( FChecksum + b) and $7f;
        BitWriter.WriteBits( Result, b, 7);
      end;
      aStream.Read( b, 1);
      FChecksum := ( FChecksum + b) and $7f;
      b := b shr (7-BitWriter.GetWriteBufferBitsLeft); {!}
      BitWriter.WriteBits( Result, b, BitWriter.GetWriteBufferBitsLeft);

      for i := Offs to Result.Size - 1 do
        Line := Line + IntToHex(PStaticByteBuffer(Result.Memory)^[i], 2) + ' ';
      Offs := Result.Size;


      if assigned(aLogLines) then
        aLogLines.Add(Line);

      MData := FChecksum;

      if ParseMidiSeptet( aStream) <> MData then // Checksum
        raise Exception.Create(Format( 'Invalid MIDI data, checksum error in block %d', [ CurrentBlock]));

      ParseMidiConst( aStream, $f7);            // End of sysex
      inc( CurrentBlock);
    end;
  finally
    BitWriter.Free;
  end;
end;

procedure TG2FileDataStream.CreateMidiBlock( aBlockNr, aBlockCount, anOffset, aSize: Integer; aChunk : TPatchChunk; aMidiStream : TStream);
var
  i, bits_left : integer;
  b         : Byte;
Begin
  // Writes one MIDI data block to Result

  FChecksum := 0;                                // Cheksum starts at 0
  AddMidiByte( aMidiStream, $f0);                // Header
  AddMidiByte( aMidiStream, $33);                // Header
  AddMidiByte( aMidiStream, $7f);                // Header
  AddMidiByte( aMidiStream, $0a);                // Header
  case PatchType of
    PATCH_DATA : AddMidiByte( aMidiStream, $20);
    PERF_DATA  : AddMidiByte( aMidiStream, $28);
  else
    raise Exception.Create('Unknown data type.');
  end;
  //AddMidiByte( aMidiStream, PatchType);
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiByte( aMidiStream, $00        );        // ?
  AddMidiInt ( aMidiStream, aBlockNr   );        // Block number
  AddMidiInt ( aMidiStream, aBlockCount);        // Block count
  AddMidiName( aMidiStream, 'No name'  );        // Patch name

  aSize := Min( (aChunk.GetReadBufferBitsLeft + 6) div 7, MIDI_BLOCK_SIZE);
  AddMidiInt( aMidiStream, aSize);   // Septet count

  i := 0;
  while i < aSize do begin
    if i = aSize - 1 then begin
      bits_left := aChunk.GetReadBufferBitsLeft mod 8;
      b := aChunk.ReadBits( bits_left);
      b := b shl (7 - bits_left); {! Tricky business}
    end else
      b := aChunk.ReadBits( 7) and $7f;
    AddMidiByte( aMidiStream, b);                // Data septets
    inc(i);
  end;

  b := FChecksum and $7f;
  AddMidiByte( aMidiStream, b);                  // Checksum
  AddMidiByte( aMidiStream, $f7);                // End of sysex
End;

procedure TG2FileDataStream.SaveMidiToStream( const aStream: TStream);
// Saves all data as a set of MIDI blocks to a MIDI file
var
  Offset     : Integer;     // Current septet start offset
  BlockNr    : Integer;     // Current MIDI block number
  BlockCount : Integer;     // Total block count
  Remaining  : Integer;     // Total of remaining septets tp handle
  SaveSize   : Integer;     // Save size for current block, in septets
  FileStream : TMemoryStream;
  Chunk      : TPatchChunk;
  b          : byte;
begin
  FileStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create( FileStream);
  try
    b := $00;
    FileStream.Write( b, 1);

    Chunk.WriteBits( PatchVersion, 8);
    Chunk.WriteBits( Patchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;

    if PatchType = PERF_DATA then // TODO
      (self as TG2FilePerformance).WriteSettings( Chunk);

    Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc( FileStream);

    FileStream.Position := 0;
    Chunk.ReadBuffer( FileStream.Size);
    Remaining  := FileStream.Size;
    //BlockCount := ( DataSize + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockCount :=  (Chunk.GetReadBufferBitsLeft div 7 + MIDI_BLOCK_SIZE - 1) div MIDI_BLOCK_SIZE;
    BlockNr    :=  0;
    Offset     :=  0;
    SaveSize   := Min( Remaining, MIDI_BLOCK_SIZE);
    //while Remaining > 0 do begin
    while Chunk.GetReadBufferBitsLeft > 0 do begin

      CreateMidiBlock( BlockNr, BlockCount, Offset, SaveSize, Chunk, aStream);
      Inc( BlockNr);
      Inc( Offset, MIDI_BLOCK_SIZE);
      Remaining := Remaining - SaveSize;
      SaveSize  := Min( Remaining, MIDI_BLOCK_SIZE);
    end;
  finally
    Chunk.Free;
    FileStream.Free;
  end;
end;

function TG2FileDataStream.ParseMidiConst( const aStream : TStream; aValue: Byte): Byte;
var b : Byte;
Begin
// Reads a constant value from a MIDI file

  aStream.Read(b, 1);
  Result := b;
  if b <> aValue then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x expected but %.2x read', [ aValue, b]));
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiInt( const aStream : TStream): Integer;
Begin
// Reads an Integer (0 .. 16383) value from a MIDI file

  Result := 128 * ParseMidiSeptet( aStream);
  Inc( Result, ParseMidiSeptet( aStream));
end;

function TG2FileDataStream.ParseMidiSeptet( const aStream : TStream): Byte;
var b : Byte;
Begin
// Reads an arbitrary data value from a MIDI file

  aStream.Read(b, 1);
  if b > $7f then
    raise Exception.Create(Format( 'Invalid MIDI dump, %.2x is not MIDI data', [ b]));
  Result := b;
  FChecksum := ( FChecksum + Result) And $7f;
end;

function TG2FileDataStream.ParseMidiString( const aStream : TStream): string;
var b : Byte;
    i : Integer;
Begin
// Reads a String[ 16]\0 from a MIDI file

  Result := '';
  for i := 1 to 16 do begin // Read the 16 characters
    b := parseMidiSeptet( aStream);
    if b <> 0 then // Don't add \0's to pascal string
      Result := Result + Char( b);
  end;
  ParseMidiConst( aStream, 0); // Read the closing \0
end;

procedure TG2FileDataStream.AddMidiByte( aStream : TStream; aByte: Byte);
begin
// Adds a MIDI byte to aData

  aStream.Write( aByte, 1);
  FChecksum := ( FChecksum + aByte) And $7f;
end;

procedure TG2FileDataStream.AddMidiInt( aStream : TStream; anInteger: Integer);
begin
// Adds an integer (0 .. 16383) to aData

  AddMidiByte( aStream, (anInteger shr 7) and $7f); // Write high septet
  AddMidiByte( aStream, anInteger and $7f);         // then  low  septet
end;

procedure TG2FileDataStream.AddMidiName( aStream : TStream; aName : string);
var i : Integer;
Begin
  // Adds the patch name as a String[ 16]\0 to aData

  for i := 0 to 15 do                            // Max 16 septets
    if i >= aName.Length then
      AddMidiByte( aStream, 0)
    else
      AddMidiByte( aStream, Byte( aName.Chars[i]) And $7f);
  AddMidiByte( aStream, $00);                    // and finalizing \0
End;


class function TG2FileDataStream.LoadFileData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
var Chunk : TPatchChunk;
    bm, bl : Byte;
    Crc : Word;
    G2FileDataStream : TG2FileDataStream;
begin
  Chunk := TPatchChunk.Create( aStream);
  G2FileDataStream := TG2FileDataStream.Create( AOwner);
  try
    if assigned( aLogLines) then
      Chunk.FLogLines := aLogLines;

    G2FileDataStream.LoadFileHeader( aStream, Chunk);

    if G2FileDataStream.FPatchType = PATCH_DATA then
      Result := TG2FilePatch.LoadFileStream( AOwner, aStream, Chunk)
    else
      Result := TG2FilePerformance.LoadFileStream( AOwner, aStream, Chunk);

    Result.FPatchVersion := G2FileDataStream.FPatchVersion;
    Result.FPatchType := G2FileDataStream.FPatchType;

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

  finally
    G2FileDataStream.Free;
    Chunk.Free;
  end;
end;

class function TG2FileDataStream.LoadMidiData( AOwner : TComponent; aStream: TStream; aLogLines : TStrings): TG2FileDataStream;
var Chunk            : TPatchChunk;
    b                : Byte;
    MemStream        : TMemoryStream;
    G2FileDataStream : TG2FileDataStream;
begin
  G2FileDataStream := TG2FileDataStream.Create( AOwner);
  try
    MemStream := G2FileDataStream.CreateFromMidiStream( aStream, aLogLines);

    MemStream.Position := 0;
    //MemStream.SaveToFile('TestSysEx_converted.bin');
    Chunk := TPatchChunk.Create( MemStream);
    try
      if assigned( aLogLines) then
        Chunk.FLogLines := aLogLines;

      Chunk.ReadBuffer( 3);

      b := Chunk.ReadBits( 8);
      b := Chunk.ReadBits( 8);
      b := Chunk.ReadBits( 8);

      if G2FileDataStream.PatchType = PATCH_DATA then
        Result := TG2FilePatch.LoadFileStream( AOwner, aStream, Chunk)
      else
        Result := TG2FilePerformance.LoadFileStream( AOwner, aStream, Chunk);

      Result.PatchVersion := G2FileDataStream.PatchVersion;
      Result.PatchType := G2FileDataStream.PatchType;

    finally
      Chunk.Free;
      MemStream.Free;
    end;

  finally
    G2FileDataStream.Free;
  end;
end;

class function TG2FileDataStream.LoadFileStream( AOwner: TComponent; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  // Abstract
  Result := nil;
end;

class function TG2FileDataStream.LoadMidiStream( AOwner : TComponent; aStream: TStream): TG2FileDataStream;
begin
  // Abstract
  Result := nil;
end;


//------------------------------------------------------------------------------
//
//                                TBankList
//
//------------------------------------------------------------------------------

constructor TBankList.Create( AOwnsObjects : boolean; aG2 : TG2File);
begin
  inherited Create( AOwnsObjects);
  FG2 := aG2;
end;

destructor TBankList.Destroy;
begin
  inherited;
end;

function TBankList.GetBankItem( aIndex : integer) : TBankItem;
begin
  result := TBankItem(inherited Items[aindex]);
end;

procedure TBankList.SetBankItem( aIndex : integer; const aValue : TBankItem);
begin
  inherited Items[aindex] := aValue;
end;

function TBankList.AddBankItem( aValue : TBankItem): integer;
begin
  Result := inherited Add( aValue);
end;

function TBankList.FindIndex( aPatchFileType : TPatchFileType; aBank, aPatch : byte): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not((aPatchFileType = GetBankItem(i).PatchFileType)
                        and (aBank = GetBankItem(i).Bank)
                        and (aPatch = GetBankItem(i).Patch)) do
    inc(i);

  if (i < Count) then
    Result := i
  else
    Result := -1;
end;

function TBankList.FindNext(aPatchFileType: TPatchFileType; aBank, aPatch: byte): TBankItem;
var i : integer;
begin
  // Find bank
  i := 0;
  while (i < Count) and not((Items[i].PatchFileType = aPatchFileType)
                        and (Items[i].Bank = aBank)) do
    inc(i);

  if (i < Count) then begin
    while (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)
                       and (Items[i].Patch <= aPatch)) do
      inc(i);

    if (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)) then
      Result := Items[i]
    else
      Result := nil;

  end else
    Result := nil;
end;

function TBankList.FindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;
var i : integer;
begin
  Result := False;

  // Find bank
  i := 0;
  while (i < Count) and not((Items[i].PatchFileType = aPatchFileType)
                        and (Items[i].Bank = aBank)) do
    inc(i);

  if (i < Count) then begin
    aFirstLocation := Items[i].Patch;
    aLastLocation := aFirstLocation;

    inc(i);
    while (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)) do begin
      aLastLocation := Items[i].Patch;
      inc(i);
    end;
    Result := True;
  end;
end;

function TBankList.Find( aPatchFileType : TPatchFileType; aBank, aPatch : byte): TBankItem;
var i : integer;
begin
  i := FindIndex( aPatchFileType, aBank, aPatch);
  if (i <> -1) then
    Result := GetBankItem(i)
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
//
//                                 TG2File
//
//------------------------------------------------------------------------------

constructor TG2File.Create( AOwner: TComponent);
begin
  inherited;

  FLogLock := TCriticalSection.Create;

  FObserverList := TList<IG2Observer>.Create;

  FLogLines := nil;
  FLogLevel := 0;

  FslBanks := TStringList.Create;

  FPerformance := CreatePerformance;
  FPerformance.G2 := self;

  FBankList := TBankList.Create( True, self);
  FMidiToKnobList := TObjectList<TMidiToKnob>.Create(True);
end;

destructor TG2File.Destroy;
begin
  FMidiToKnobList.Free;
  FBankList.Free;
  FslBanks.Free;

  if assigned(FLogLines) then begin
    FLogLock.Acquire;
    try
      FreeAndNil(FLogLines);
    finally
      FLogLock.Release;
    end;
  end;

  FObserverList.Free;

  FLogLock.Free;

  inherited;
end;

procedure TG2File.RegisterObserver(aObserver: IG2Observer);
begin
  FObserverList.Add( aObserver);
end;

procedure TG2File.RemoveObserver(aObserver: IG2Observer);
begin
  FObserverList.Remove( aObserver);
end;

procedure TG2File.NotifyObservers(aG2Event: TG2Event);
var Observer : IG2Observer;
begin
  for Observer in FObserverList do
    Observer.Update(aG2Event);
end;

function TG2File.CreatePerformance : TG2FilePerformance;
begin
  Result := TG2FilePerformance.Create(self);
end;

procedure TG2File.SetPerformance(aValue: TG2FilePerformance);
begin
  FPerformance := aValue;
  if assigned(FPerformance) then
    FPerformance.G2 := self;
end;

procedure TG2File.AddMidiToKnob(aSource : TG2ParamSource;
  aControlType : TG2ControlType; aKnob: integer; aMidiCC: byte);
var i : integer;
    MidiToKnob : TMidiToKnob;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].FSource = aSource)
                                      and (FMidiToKnobList[i].FControlType = aControlType)
                                      and (FMidiToKnobList[i].FKnob = aKnob)) do
    inc(i);

  if not(i<FMidiToKnobList.Count) then begin
    MidiToKnob := TMidiToKnob.Create;
    MidiToKnob.FSource := aSource;
    MidiToKnob.FControlType := aControlType;
    MidiToKnob.FKnob := aKnob;
    MidiToKnob.FMidiCC := aMidiCC;
    FMidiToKnobList.Add(MidiToKnob);
  end else
    FMidiToKnobList[i].FMidiCC := aMidiCC;
end;

procedure TG2File.DeleteMidiToKnob(aSource : TG2ParamSource;
  aControlType : TG2ControlType; aKnob: integer);
var i : integer;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].FSource = aSource)
                                      and (FMidiToKnobList[i].FControlType = aControlType)
                                      and (FMidiToKnobList[i].FKnob = aKnob)) do
    inc(i);

  if (i<FMidiToKnobList.Count) then
    FMidiToKnobList.Delete(i);
end;

function TG2File.FindMidiToKnob(aSource : TG2ParamSource;
  aControlType : TG2ControlType; aKnob: integer): integer;
var i : integer;
begin
  Result := 0;

  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].FSource = aSource)
                                      and (FMidiToKnobList[i].FControlType = aControlType)
                                      and (FMidiToKnobList[i].FKnob = aKnob)) do
    inc(i);

  if (i<FMidiToKnobList.Count) then
    Result := FMidiToKnobList[i].FMidiCC;
end;

function TG2File.MidiToKnobCheckCC(aMidiCC: byte): boolean;
var i : integer;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not(FMidiToKnobList[i].FMidiCC = aMidiCC) do
    inc(i);

  Result := (i<FMidiToKnobList.Count);
end;

function TG2File.TextFunction( No : integer; Param1, Param2, Param3 : byte): string;
begin
  case No of
  0 : begin
        case Param1 of
        0 : Result := 'Legato';
        1 : Result := 'Mono';
        else
          Result := IntToStr(Param1);
        end;
      end;
  end;
end;

procedure TG2File.SetLogLevel( aValue : integer);
begin
  if aValue > 0 then begin

    FLogLock.Acquire;
    try
      if not assigned(FLogLines) then
        FLogLines := TStringList.Create;
    finally
      FLogLock.Release;
    end;

  end else begin

    if assigned(FLogLines) then begin
      FLogLock.Acquire;
      try
        FreeAndNil(FLogLines);
      finally
        FLogLock.Release;
      end;
    end;

  end;
  FLogLevel := aValue;
end;

function TG2File.GetID : integer;
begin
  Result := 0;
end;

function TG2File.GetSelectedPatchPart: TG2FilePatchPart;
var Patch : TG2FilePatch;
    Slot : TG2FileSlot;
begin
  Result := nil;
  if assigned(FPerformance) then begin
    Slot := FPerformance.Slot[ FPerformance.SelectedSlot];
    if assigned(Slot) then begin
      Patch := Slot.Patch;
      if assigned(Patch) then
        Result := Patch.PatchPart[ ord(Patch.SelectedLocation)];
    end;
  end;
end;

function TG2File.G2MessageDlg( tekst : string; code : integer): integer;
begin
  Result := 0;
end;

procedure TG2File.add_log_line( tekst: string; log_cmd : integer);
begin
  if (FLogLevel = 0) then
    exit;

  if assigned(FOnLogLine) then begin
    // Use event if assigned in stead of internal log
    FOnLogLine(self, tekst, log_cmd);
    exit;
  end;

  if (not assigned(FLogLines)) then
    exit;

  FLogLock.Acquire;
  try
    if (FLogLevel = 0) or (not assigned(FLogLines)) then
      exit;

    case log_cmd of
    LOGCMD_NUL: FLogLines.Add(tekst);
    LOGCMD_SAV:
      begin
        FLogLines.Add(tekst);
        FLogLines.SaveToFile('G2_log.txt');
      end;
    LOGCMD_HDR:
      begin
        FLogLines.Add('');
        FLogLines.Add(stringofchar('=', Length(tekst)));
        FLogLines.Add(tekst);
      end;
    LOGCMD_TAB:
      FLogLines.Add('    ' + tekst);
    LOGCMD_ERR:
      FLogLines.Add('ERROR : ' + tekst);
    end;

  finally
    FLogLock.Release;
  end;
end;

procedure TG2File.dump_buffer( var buffer; max_size : integer);
type buf_type = packed array[0..65536 - 1] of byte;
var p, i, c : integer;
    char_line, line : string;
begin
  if (FLogLevel < 2) or ((not assigned(FLogLines)) and (not assigned(FOnLogLine))) then
    exit;

  FLogLock.Acquire;
  try

    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i<max_size) do begin
      if c < 16 then begin
        line := line + IntToHex(buf_type(buffer)[i], 2) + ' ';
        if buf_type(buffer)[i] >= 32 then
          char_line := char_line + chr(buf_type(buffer)[i])
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end else begin
        if assigned(FOnLogLine) then
          FOnLogLine( self, IntToHex(p, 6) + ' - ' + line + ' ' + char_line, LOGCMD_NUL)
        else
          FLogLines.Add(IntToHex(p, 6) + ' - ' + line + ' ' + char_line);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
     if assigned(FOnLogLine) then
       FOnLogLine( self, IntToHex(p, 6) + ' - ' + line + stringofchar(' ', 16*3 - Length(line) + 1) + char_line, LOGCMD_NUL)
     else
       FLogLines.Add(IntToHex(p, 6) + ' - ' + line + stringofchar(' ', 16*3 - Length(line) + 1) + char_line);
  finally
    FLogLock.Release;
  end;
end;

procedure TG2File.SaveLog(const filename : string);
var i : integer;
begin
  if assigned(FLogLines) then begin
    FLogLock.Acquire;
    try
      FLogLines.SaveToFile(filename);
    finally
      FLogLock.Release;
    end;
  end;
end;

procedure TG2File.AssignLog( Lines : TStrings);
begin
  if assigned(FLogLines) then begin
    FLogLock.Acquire;
    try
      Lines.Assign(FLogLines);
    finally
      FLogLock.Release;
    end;
  end;
end;

procedure TG2File.ClearLog;
begin
  if assigned(FLogLines) then begin
    FLogLock.Acquire;
    try
      FLogLines.Clear;
    finally
      FLogLock.Release;
    end;
  end;
end;

end.



