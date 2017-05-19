unit BVE.NMG2FileIntf;

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

interface
uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  BVE.NMG2Types,
  BVE.NMG2Classes;

type
  IG2Synth = interface;
  IG2Perf = interface;
  IG2Slot = interface;
  IG2Patch = interface;
  IG2PatchPart = interface;
  IG2Module = interface;
  IG2Cable = interface;
  IG2Function = interface;
  IG2Connection = interface;
  IG2Subject = interface;

  IG2Object = interface
    ['{44D80023-DA76-4690-B7DD-1E433C0BCD9D}']
  end;

  IG2Observer = interface
    ['{44FB1F31-448E-43FB-9BF9-5E9938B0EDBB}']
    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
    // Because remove reference is called in Destroy of an interfaced
    // object, the parameter must be weak otherwise the reference count will
    // again be nill when the call returns and Destroy will be triggered
    // again (and again and again...)
    // Can't get it to work...
    //procedure RemoveReference( aData : pointer);
    //procedure RemoveReference( const aData : IG2Subject);
  end;

  IG2Subject = interface(IG2Object)
    ['{4ACA1A45-7A5D-4AA7-A1F6-75F51E8F5F3A}']
    procedure RegisterObserver(aObserver: IG2Observer);
    procedure RemoveObserver(aObserver: IG2Observer);
    procedure NotifyObservers(aG2Event: TG2Event; const aG2Object: IG2Object);
    procedure NotifyDestroy;
  end;

  //IG2ParamObserver = interface;

  IG2Led = interface(IG2Subject)
    ['{3CE71DAA-F061-40A4-80BF-348A5AEDC6E6}']
    procedure SetValue( const aValue : byte);
    function GetValue : byte;
    function GetLocationIndex : byte;
    function GetGroupID : byte;
    function GetModuleIndex : byte;
    //function GetLedType : TLedType;
    function GetGroupCount : integer;

    property LocationIndex : byte read GetLocationIndex;
    property ModuleIndex : byte read GetModuleIndex;
    property GroupID : byte read GetGroupID;
    //property LedType : TLedType read GetLedType;
    property GroupCount : integer read GetGroupCount;
  end;

  {IG2DataParam = interface(IG2Subject)
    4C28F845-210F-4CDD-82EC-8F1CCFEE9F9D
    procedure SetValue( const aValue : byte);
    function GetValue : byte;
    function GetHighValue : byte;
    function GetLowValue : byte;
    function GetValueText( aIndex : integer): string;
    function HasMorph : boolean;
    procedure SetSelectedMorphValue( const aValue : byte);
    function GetSelectedMorphValue : byte;
    procedure SetSelected( const aValue : boolean);
    function GetSelected : boolean;
  end;}

  IG2Function = interface(IG2Subject)
    ['{8433E9C6-942D-44F7-918F-B4229AE14E8E}']
    function GetAsText : string;

    property AsText : string read GetAsText;
  end;

  {IG2LedObserver = interface(IG2Observer)
    B044DDF3-B423-4517-B010-062B7F62280C
    function GetCodeRef : integer;
    procedure SetValue(const aValue : integer);
    procedure Redraw;
  end;}

  {IG2ParamObserver = interface(IG2Observer)
    FA9A0B2E-960F-44DC-B47D-32876D9A50E9
    procedure SetValue(const aValue : integer);
    procedure SetButtonText(const aValue : TStrings);
    procedure SetMorphValue(const aValue : integer);
    procedure SetHasMorph(const aValue : boolean);
    procedure SetSelected(const aValue : boolean);
    function GetValueText( aIndex : integer): string;
    procedure SetValueText( aIndex : integer; const aValue : string);
    function GetCodeRef : integer;
    procedure Redraw;

    property ValueText[aIndex : integer] : string read GetValueText write SetValueText;
    property CodeRef : integer read GetCodeRef;
  end;}

  {IG2MultiParamObserver = interface(IG2ParamObserver)
   // ['A6F880E1-8C16-49A2-9ED6-3A7296678B02']
  {  procedure AddDataDependency( aData : IG2DataParam; aDeassignEvent : TNotifyEvent);
    procedure ClearDataDependencies;
  end;}

  IG2Connector = interface(IG2Subject)
  ['{1920FCA6-894B-4ACC-AA06-9F5CA99B4A4D}']
    function    GetConnectorColor: TCableColor;
    function    GetNewConnectorColor: TCableColor;
    function    GetDefRate: TBits1;
    function    GetRate: TBits1;
    function    GetNewRate: TBits1;
    function    GetConnectorKind: TConnectorKind;
    function    GetConnectorDefColor : TCableColor;
    function    GetConnectorIndex: byte;
    function    GetConnectorType: TConnectorType;
    function    GetBandWidth: TBandWidthType;
    function    GetConnectorName: string;
    function    GetModule: IG2Module;
    function    GetSlotIndex: byte;
    function    GetModuleIndex: byte;
    function    GetLocationIndex: byte;
    function    GetConnected: boolean;

    procedure   SetConnectorKind( aValue : TConnectorKind);
    procedure   SetConnectorDefColor( aValue : TCableColor);
    procedure   SetConnectorIndex( const aValue : byte);
    procedure   SetConnectorType( const aValue : TConnectorType);
    procedure   SetBandWidth( const aValue : TBandWidthType);
    procedure   SetConnectorName( const aValue : string);
    //procedure   SetLocationIndex( const aValue : byte);

    procedure   InitConnector( aModuleType, aConnectorIndex : Byte; aConnectorKind : TConnectorKind);

    function    CreateCableList: TList<IG2Cable>;

    procedure   InvalidateCables;
    procedure   InvalidateControl;

    procedure   CalcDefColor;

    property    Module : IG2Module read GetModule;
    property    SlotIndex: byte read GetSlotIndex;
    property    ModuleIndex: byte read GetModuleIndex;
    property    LocationIndex : byte read GetLocationIndex;
    property    ConnectorKind : TConnectorKind read GetConnectorKind write SetConnectorKind;
    property    ConnectorColor : TCableColor read GetConnectorColor;
    property    ConnectorDefColor : TCableColor read GetConnectorDefColor write SetConnectorDefColor;
    property    ConnectorIndex : byte read GetConnectorIndex write SetConnectorIndex;
    property    ConnectorType : TConnectorType read GetConnectorType write SetConnectorType;
    property    BandWidth : TBandWidthType read GetBandWidth write SetBandWidth;
    property    ConnectorName : string read GetConnectorName write SetConnectorName;
    property    NewConnectorColor : TCableColor read GetNewConnectorColor;
    property    DefRate : TBits1 read GetDefRate;
    property    Rate : TBits1 read GetRate;
    property    NewRate : TBits1 read GetNewRate;
    property    Connected: boolean read GetConnected;
  end;

  IG2Knob = interface;
  IG2Controller = interface;

  IG2ParamMorph = interface(IG2Object)
  ['{ABCAEBE6-9E7B-491D-8502-8B247BD5028F}']
    function    GetRange: byte;
    function    GetAbsRange: byte;
    function    GetNegative: byte;
    function    GetSlotIndex: byte;
    function    GetLocationIndex : byte;
    function    GetModuleIndex : byte;
    function    GetParamIndex : byte;
    function    GetMorphIndex : byte;

    procedure   SetRange( const aValue : byte);
    procedure   SetSlotIndex(const aValue: byte);
    procedure   SetLocationIndex( const aValue : byte);
    procedure   SetModuleIndex( const aValue : byte);
    procedure   SetParamIndex( const aValue : byte);
    procedure   SetMorphIndex( const aValue : byte);

    procedure   Init;
    procedure   Write( aChunk : TPatchChunk);

    property    Range : byte read GetRange write SetRange;
    property    AbsRange : byte read GetAbsRange;
    property    Negative : byte read GetNegative;
    property    SlotIndex: byte read GetSlotIndex write SetSlotIndex;
    property    LocationIndex : byte read GetLocationIndex write SetLocationIndex;
    property    ModuleIndex : byte read GetModuleIndex write SetModuleIndex;
    property    ParamIndex : byte read GetParamIndex write SetParamIndex;
    property    MorphIndex : byte read GetMorphIndex write SetMorphIndex;
  end;

  IG2Param = interface(IG2Subject)
  ['{A7CCF074-C9C6-4C01-83C7-D95B65CFA103}']
    procedure   SetValue( const aValue : byte);
    function    GetValue : byte;
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    procedure   SetSelected( const aValue : boolean);
    function    GetSelected : boolean;
    function    GetLabelIndex: integer;
    function    GetLabelOnValue: boolean;
    function    GetMorphAssignmentsAllowed: boolean;
    function    GetKnobAssignmentsAllowed: boolean;
    function    GetMidiAssignmentsAllowed: boolean;
    function    GetButtonText( Index : integer): string;
    function    GetSelectedButtonText: string;
    function    GetButtonTextCount : integer;
    function    GetButtonParam : IG2Param;
    function    GetSuspendUpdate: boolean;

    procedure   SetButtonText( Index : integer; aValue : string);
    procedure   SetSuspendUpdate( aValue : boolean);

    function    GetParamID : integer;
    function    GetParamType : TParamType;
    function    GetParamIndex : integer;
    function    GetDefaultValue : byte;
    function    GetValueText(const aIndex : integer): string;
    function    GetSlotIndex: integer;
    function    GetLocationIndex : integer;
    function    GetModuleIndex : integer;
    function    GetParamName : string;
    function    GetModuleName : string;
    function    GetCanChangeLabel : boolean;
    function    GetInfoFunctionIndex : integer;
    function    GetButtonParamIndex : integer;
    function    GetDefaultKnobIndex : integer;
    function    GetButtonTextList : TStrings;
    function    GetModule : IG2Module;
    function    GetKnob : IG2Knob;
    function    GetController : IG2Controller;
    function    GetGlobalKnob : IG2Knob;
    function    GetMorph( const aMorphIndex, aVariation : byte) : IG2ParamMorph;
    procedure   SetSelectedMorphValue( const aValue : byte);
    function    GetSelectedMorphValue : byte;

    procedure   SetHighValue( const aValue : byte);
    procedure   SetLowValue( const aValue : byte);
    procedure   SetParamID( const aValue : integer);
    procedure   SetParamType( const aValue : TParamType);
    procedure   SetDefaultValue( const aValue : byte);
    //procedure   SetModuleIndex( const aValue : integer);
    procedure   SetParamName( const aValue : string);
    procedure   SetModuleName( const aValue : string);
    procedure   SetCanChangeLabel( const aValue : boolean);
    procedure   SetInfoFunctionIndex( const aValue : integer);
    procedure   SetButtonParamIndex( const aValue : integer);
    procedure   SetDefaultKnobIndex( const aValue : integer);

    function    CreateMorph( const aMorphIndex, aVariationIndex, aRange : byte): IG2ParamMorph;

    function    CreateMessage: TG2SendMessage;
    function    CreateUndoMessage: TG2SendMessage;

    procedure   AddMessAssignKnob( const aKnob : IG2Knob; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignKnob( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAssignController( const aMidiCC : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignController( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAssignGlobalKnob( const aGlobalKnob : IG2Knob; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignGlobalKnob( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessSetMorph( const aMorph : IG2ParamMorph; const aVariation : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessClearMorph( const aMorph : IG2ParamMorph; const aVariation : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    //procedure   DeassignControl( const aControl : IG2ParamObserver);

    procedure   IncValue;
    procedure   DecValue;
    function    HasMorph : boolean;
    procedure   IncMorphValue;
    procedure   DecMorphValue;

    function    GetParamLabel(const aIndex : integer): string;
    function    GetMorphValue(const aMorphIndex, aVariation : integer) : byte;
    procedure   SetMorphValue(const aMorphIndex, aVariation : integer; const aValue : byte);
    procedure   InvalidateControl;

    property    ParamID : integer read GetParamID write SetParamID;
    property    ParamType : TParamType read GetParamType write SetParamType;
    property    ParamIndex : integer read GetParamIndex;
    property    LowValue : byte read GetLowValue write SetLowValue;
    property    HighValue : byte read GetHighValue write SetHighValue;
    property    Value : byte read GetValue write SetValue;
    property    DefaultValue : byte read GetDefaultValue write SetDefaultValue;
    property    Morph[ const aMorphIndex, aVariation : byte]: IG2ParamMorph read GetMorph;
    property    SlotIndex: integer read GetSlotIndex;
    property    LocationIndex : integer read GetLocationIndex;
    property    ModuleIndex : integer read GetModuleIndex;
    property    ParamName : string read GetParamName write SetParamName;
    property    ModuleName : string read GetModuleName write SetModuleName;
    property    LabelIndex : integer read GetLabelIndex;
    property    ParamLabel[const index : integer] : string read GetParamLabel;
    property    CanChangeLabel : boolean read GetCanChangeLabel write SetCanChangeLabel;
    property    LabelOnValue : boolean read GetLabelOnValue; // Switches for example
    property    Selected : boolean read GetSelected write SetSelected;
    property    InfoFunctionIndex : integer read GetInfoFunctionIndex write SetInfoFunctionIndex;
    property    ButtonParamIndex : integer read GetButtonParamIndex write SetButtonParamIndex;
    property    DefaultKnobIndex : integer read GetDefaultKnobIndex write SetDefaultKnobIndex;
    property    ButtonTextList : TStrings read GetButtonTextList;
    property    ButtonText[ index : integer]: string read GetButtonText write SetButtonText;
    property    ButtonTextCount : integer read GetButtonTextCount;
    property    SelectedButtonText: string read GetSelectedButtonText;
    property    ButtonParam : IG2Param read GetButtonParam;
    property    Module : IG2Module read GetModule;
    property    Knob : IG2Knob read GetKnob;
    property    Controller : IG2Controller read GetController;
    property    GlobalKnob : IG2Knob read GetGlobalKnob;
    property    MorphAssignmentsAllowed : boolean read GetMorphAssignmentsAllowed;
    property    KnobAssignmentsAllowed : boolean read GetKnobAssignmentsAllowed;
    property    MidiAssignmentsAllowed : boolean read GetMidiAssignmentsAllowed;

    property    SuspendUpdate : boolean read GetSuspendUpdate write SetSuspendUpdate;
  end;

  IG2Knob = interface(IG2Object)
  ['{F2BF77CD-508A-4847-B47A-B160D940125B}']
    function     GetKnobValue : byte;
    procedure    SetKnobValue(const aValue : byte);
    function     GetKnobFloatValue : single;
    procedure    SetKnobFloatValue(const aValue : single);
    function     GetKnobHighValue : byte;
    function     GetKnobLowValue : byte;
    function     GetKnobButtonValue : byte;
    procedure    SetKnobButtonValue(const aValue : byte);
    function     GetKnobButtonFloatValue : single;
    procedure    SetKnobButtonFloatValue(const aValue : single);
    //procedure    SetKnobIndex(const aValue : byte);

    function     GetKnobIndex : byte;
    function     GetIsAssigned : TBits1;
    function     GetIsLed : TBits2;
    function     GetSlotIndex: TBits2;
    function     GetLocationIndex : TBits3;
    function     GetModuleIndex : TBits8;
    function     GetParamIndex : TBits7;
    function     GetParam : IG2Param;

    procedure    SetIsAssigned( const aValue : TBits1);
    procedure    SetIsLed( const aValue : TBits2);
    //procedure    SetLocationIndex( const aValue : TBits2);
    //procedure    SetModuleIndex( const aValue : TBits8);
    //procedure    SetParamIndex( const aValue : TBits7);
    procedure    SetParam( const aValue : IG2Param);

    procedure    Init;
    //procedure    Read( aChunk : TPatchChunk);
    procedure    Write( aChunk : TPatchChunk);

    property     IsAssigned : TBits1 read GetIsAssigned write SetIsAssigned;
    property     IsLed : TBits2 read GetIsLed write SetIsLed;
    property     SlotIndex : byte read GetSlotIndex;
    property     LocationIndex : TBits2 read GetLocationIndex;
    property     ModuleIndex : TBits8 read GetModuleIndex;
    property     ParamIndex : TBits7 read GetParamIndex;
    property     Param : IG2Param read GetParam write SetParam;
    property     KnobValue : byte read GetKnobValue write SetKnobValue;
    property     KnobFloatValue : single read GetKnobFloatValue write SetKnobFloatValue;
    property     KnobButtonValue : byte read GetKnobButtonValue write SetKnobButtonValue;
    property     KnobButtonFloatValue : single read GetKnobButtonFloatValue write SetKnobButtonFloatValue;
    property     KnobHighValue : byte read GetKnobHighValue;
    property     KnobLowValue : byte read GetKnobLowValue;
    property     KnobIndex : byte read GetKnobIndex;
  end;

  IG2Controller = interface(IG2Object)
  ['{6417174A-16DD-45AC-A35D-1C9A6BA6F28F}']
    function  GetMidiCC: TBits7;
    function  GetLocationIndex: TBits2;
    function  GetModuleIndex: TBits8;
    function  GetParamIndex: TBits7;
    function  GetParam: IG2Param;

    procedure SetMidiCC(const aValue: TBits7);
    procedure SetParam(const aValue: IG2Param);

    procedure Write(aChunk : TPatchChunk);

    property  MidiCC : TBits7 read GetMidiCC write SetMidiCC;
    property  LocationIndex : TBits2 read GetLocationIndex;
    property  ModuleIndex : TBits8 read GetModuleIndex;
    property  ParamIndex : TBits7 read GetParamIndex;
    property  Param : IG2Param read GetParam write SetParam;
  end;

  TModeInfoArray = array of TBits6;

  IG2Module = interface(IG2Subject)
  ['{1A7AA2EC-1984-464E-9BA5-51CF72FC81AE}']
    function    GetInputConnector(const ConnectorIndex : integer): IG2Connector;
    function    GetOutputConnector(const ConnectorIndex : integer): IG2Connector;
    function    GetInputConnectorCount : integer;
    function    GetOutputConnectorCount : integer;
    function    GetParam(const ParamIndex : integer): IG2Param;
    function    GetMode(const ModeIndex : integer): IG2Param;
    function    GetParameterCount : integer;
    function    GetModeCount : integer;
    function    GetColor: integer;
    function    GetModeInfo(const aModeIndex : integer): TBits6;
    function    GetSelectedParam : IG2Param;
    function    GetSelected: boolean;
    function    GetFocussed: boolean;
    function    GetRowIndex : integer;
    //function    GetInfoFunction(const aIndex: integer): IG2Function;

    function    GetTypeID        : TBits8;
    function    GetSlotIndex     : byte;
    function    GetLocationIndex : byte;
    function    GetModuleIndex   : TBits8;
    function    GetCol         : TBits7;
    function    GetRow         : TBits7;
    function    GetModuleColor : TBits8;
    function    GetUnknown1    : TBits6;
    function    GetUprate      : TBits1;
    function    GetIsLed       : TBits1;
    function    GetPatchPart : IG2PatchPart;
    function    GetModuleName  : string;
    function    GetHeightUnits : integer;
    function    GetNewUprate : TBits1;
    function    GetModuleFileName : string;
    function    GetPage : TModulePage;
    function    GetPageIndex : integer;
    function    GetModeInfoArray : TModeInfoArray;
    function    GetNewCol : byte;
    function    GetNewRow : byte;
    function    GetNewModuleIndex : byte;
    function    GetNewModuleName : string;

    procedure   SetUprate(const Value : TBits1);
    procedure   SetModeInfo(const aModeIndex : integer; const aValue : TBits6);
    procedure   SetSelected(const aValue : boolean);
    procedure   SetSelectedParam(const aValue : IG2Param);
    procedure   SetCol(const Value : TBits7);
    procedure   SetRow(const Value : TBits7);
    procedure   SetModuleColor(const Value : TBits8);
    procedure   SetModuleName(const aValue : string);
    procedure   SetModuleIndex(const aValue : TBits8);
    procedure   SetTypeID(const aValue : TBits8);
    procedure   SetIsLed(const aValue : TBits1);
    procedure   SetHeightUnits(const aValue : integer);
    procedure   SetUnknown1(const aValue : TBits6);
    procedure   SetNewUprate(const aValue : TBits1);
    procedure   SetModuleFileName(const aValue : string);
    procedure   SetNewCol(const aValue : byte);
    procedure   SetNewRow(const aValue : byte);
    procedure   SetNewModuleIndex(const aValue : byte);
    procedure   SetNewModuleName(const aValue : string);

    function    GetParamValue(const aVariation, aParamIndex : byte): integer;
    procedure   SetParamValue(const aVariation, aParamIndex, aValue : byte);

    function    GetParamLabel(const aParamIndex, aLabelIndex : byte): string;
    procedure   SetParamLabel(const aParamIndex, aLabelIndex : byte; const aName : string);
    procedure   SetParamLabels(const aParamIndex : byte; const aNames : string);
    function    GetParamLabelCount: integer;

    procedure   Init;

    procedure   ResetUprate;

    procedure   Read( aChunk : TPatchChunk);
    procedure   ReadParamList( aChunk : TPatchChunk; aVariationCount : integer);
    procedure   ReadParamLabelList( aChunk : TPatchChunk);

    procedure   Write( aChunk : TPatchChunk);
    procedure   WriteParamList( aChunk : TPatchChunk; aVariationCount : integer);
    procedure   WriteParamLabelList( aChunk : TPatchChunk);

    procedure   InitModule;

    procedure   AddParameter( aParameter : IG2Param);
    function    AddInfoFunction( const aFunctionID : integer; const aParamIndex: integer): IG2Function;
    function    AddTextFunction( const aFunctionID : integer; const aMasterParamIndex: integer; const aDependencies : string): IG2Function;

    procedure   CopyVariation( aFromVariation, aToVariation : byte);

    procedure   InvalidateControl;
    procedure   InvalidateCables;
    procedure   InvalidateConnectors;
    procedure   InvalidateParameters;

    function    CreateAssignableParamList: TList<IG2Param>;

    procedure   AddMessModuleAdd( const aLocationIndex : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessModuleDelete( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessModuleColor( const aModuleColor: byte; aSendMessage, aUndoMessage: TG2SendMessage);
    procedure   AddMessModuleMove( aSendMessage, aUndoMessage: TG2SendMessage);
    procedure   AddMessModuleLabel( const aLabel : string; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessParamLabel( const aParam : IG2Param; const aIndex : integer; const aLabel : string; aSendMessage,  aUndoMessage : TG2SendMessage);

    function    GetNextParam : IG2Param;
    function    GetPrevParam : IG2Param;
    procedure   SelectNextParam;
    procedure   SelectPrevParam;

    property    TypeID      : TBits8 read GetTypeID write SetTypeID;
    property    SlotIndex: byte read GetSlotIndex;
    property    LocationIndex : byte read GetLocationIndex;
    property    ModuleIndex : TBits8 read GetModuleIndex write SetModuleIndex;
    property    Col         : TBits7 read GetCol write SetCol;
    property    Row         : TBits7 read GetRow write SetRow;
    property    ModuleColor : TBits8 read GetModuleColor write SetModuleColor;
    property    Unknown1    : TBits6 read GetUnknown1 write SetUnknown1;
    property    Uprate      : TBits1 read GetUprate write SetUprate;
    property    IsLed       : TBits1 read GetIsLed write SetIsLed;
    property    ModeInfo    [const Index : integer]: TBits6 read GetModeInfo write SetModeInfo;
    property    PatchPart : IG2PatchPart read GetPatchPart;
    property    ModuleName  : string read GetModuleName write SetModuleName;
    property    InConnector [const Index : integer]: IG2Connector read GetInputConnector;
    property    OutConnector[const Index : integer]: IG2Connector read GetOutputConnector;
    property    Param       [const Index : integer]: IG2Param read GetParam;
    property    Mode        [const Index : integer]: IG2Param read GetMode;
    property    HeightUnits : integer read GetHeightUnits write SetHeightUnits;
    property    InConnectorCount : integer read GetInputConnectorCount;
    property    OutConnectorCount : integer read GetOutputConnectorCount;
    property    ParameterCount : integer read GetParameterCount;
    property    ModeCount : integer read GetModeCount;
    property    Color : integer read GetColor;
    property    Focussed: boolean read GetFocussed;
    property    Selected : boolean read GetSelected write SetSelected;
    property    NewRow : byte read GetNewRow write SetNewRow;
    property    NewCol : byte read GetNewCol write SetNewCol;
    property    NewModuleIndex : byte read GetNewModuleIndex write SetNewModuleIndex;
    property    NewModuleName : string read GetNewModuleName write SetNewModuleName;
    property    NewUprate : TBits1 read GetNewUprate write SetNewUprate;
    property    RowIndex    : integer read GetRowIndex;
    property    SelectedParam : IG2Param read GetSelectedParam write SetSelectedParam;
    property    ModuleFileName : string read GetModuleFileName write SetModuleFileName;
    property    Page : TModulePage read GetPage;
    property    PageIndex : integer read GetPageIndex;
    property    ModeInfoArray : TModeInfoArray read GetModeInfoArray;
    //property    InfoFunction[const aIndex: integer]: IG2Function read GetInfoFunction;
    property    ParamLabelCount: integer read GetParamLabelCount;
  end;

  IG2Cable = interface(IG2Subject)
  ['{837BAF2B-667F-41EF-85EC-DF444AF06663}']
    function    GetModuleIndexFrom : TBits8;
    function    GetConnIndexFrom : TBits6;
    function    GetLinkType : TLinkType;
    function    GetModuleIndexTo : TBits8;
    function    GetConnIndexTo : TBits6;
    function    GetConnFrom : IG2Connector;
    function    GetConnTo : IG2Connector;
    function    GetModuleFrom: IG2Module;
    function    GetModuleTo: IG2Module;
    function    GetCableColor : TCableColor;
    function    GetSlotIndex: byte;
    function    GetLocationIndex : byte;
    function    GetConnFromKind : TConnectorKind;
    function    GetConnToKind : TConnectorKind;
    function    GetToDelete : boolean;

    procedure   SetConnFrom( const aValue : IG2Connector);
    procedure   SetConnTo( const aValue : IG2Connector);
    procedure   SetCableColor( const aValue : TCableColor);
    procedure   SetToDelete( const aValue : boolean);

    procedure   Init;
    procedure   ConnectorMoved;
    procedure   Invalidate;
    //procedure   Read( aChunk : TPatchChunk);
    procedure   Write( aChunk : TPatchChunk);

    procedure   AddMessCableAdd( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessCableDelete( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessCableColor(const aColor: byte; aSendMessage, aUndoMessage: TG2SendMessage);

    property    SlotIndex: byte read GetSlotIndex;
    property    LocationIndex : byte read GetLocationIndex;
    property    ModuleIndexFrom : TBits8 read GetModuleIndexFrom;
    property    ConnIndexFrom : TBits6 read GetConnIndexFrom;
    property    ConnFromKind : TConnectorKind read GetConnFromKind;
    property    LinkType : TLinkType read GetLinkType;
    property    ModuleIndexTo : TBits8 read GetModuleIndexTo;
    property    ConnIndexTo : TBits6 read GetConnIndexTo;
    property    ConnToKind : TConnectorKind read GetConnToKind;
    property    ConnFrom : IG2Connector read GetConnFrom write SetConnFrom;
    property    ConnTo : IG2Connector read GetConnTo write SetConnTo;
    property    ModuleFrom: IG2Module read GetModuleFrom;
    property    ModuleTo: IG2Module read GetModuleTo;
    property    CableColor : TCableColor read GetCableColor write SetCableColor;
    property    ToDelete : boolean read GetToDelete write SetToDelete;
  end;

  TAddModuleEvent = procedure(Sender : TObject; const aLocationIndex : byte; aModule : IG2Module) of Object;
  TAddCableEvent = procedure(Sender : TObject; const aLocationIndex : byte; aCable : IG2Cable) of Object;
  TLedEvent = procedure(Sender : TObject; const aLocationIndex, aModuleIndex, aGroupIndex, aCodeRef, aValue : byte) of Object;

  IG2PatchPart = interface(IG2Object)
  ['{657A92C6-D266-4687-81FE-04FBDAC2DEE9}']
    function    GetVariationCount: integer;
    procedure   SetFocusedModuleIndex(const aModuleIndex: integer);

    function    GetSelectedParam : IG2Param;
    function    GetFocusedModuleIndex : integer;
    function    GetModuleList : TList<IG2Module>;
    function    GetCableList : TList<IG2Cable>;
    function    GetLedList : TList<IG2Led>;
    function    GetLedStripList : TList<IG2Led>;
    function    GetUnitsRect: TRect;
    function    GetSelectedUnitsRect: TRect;
    function    GetLocationIndex: byte;
    function    GetSlotIndex: byte;
    function    GetPatch: IG2Patch;

    procedure   SetOnAddModule(const aValue : TAddModuleEvent);
    procedure   SetOnAddCable(const aValue : TAddCableEvent);
    procedure   SetOnLedEvent(const aValue : TLedEvent);

    function    GetOnAddModule: TAddModuleEvent;
    function    GetOnAddCable: TAddCableEvent;
    function    GetOnLedEvent: TLedEvent;

    procedure   Init;
    procedure   InitNames;
    procedure   InitParams;

    procedure   Read(aChunk: TPatchChunk);
    function    ReadChunk(aChunk: TPatchChunk): boolean;
    procedure   Write(aChunk: TPatchChunk; aVariationCount : byte);
    procedure   WriteChunk(aID: byte; aChunk : TPatchChunk; aVariationCount : byte);
    procedure   WriteChunkSelectedModules(aModuleList: TList<IG2Module>; const aID : byte; const aVariationCount : byte; aChunk : TPatchChunk);

    procedure   AddMessAddModule( const aModuleTypeID, aCol, aRow: byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAddCable( const aFromModuleIndex : byte;
      aFromConnectorKind : TConnectorKind; aFromConnectorIndex : byte;
      aFromConnectorColor : byte; aToModuleIndex : byte;
      aToConnectorKind : TConnectorKind; aToConnectorIndex : byte;
      aToConnectorColor : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeleteCable( aCable : IG2Cable; aSendMessage, aUndoMessage: TG2SendMessage);
    procedure   AddMessAddPatchPart( aDestPatchPart : IG2PatchPart; aSendMessage, aUndoMessage: TG2SendMessage);
    procedure   AddMessDeleteModules( aSendMessage, aUndoMessage: TG2SendMessage);
    procedure   AddMessPasteParams( aSrcePatchPart : IG2PatchPart; const aFromVariation, aToVariation : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    procedure   AddModule( aModule : IG2Module);
    procedure   DeleteModule( aModuleIndex : integer);
    procedure   AddCable( aCable : IG2Cable);
    procedure   DeleteCable( aCable : IG2Cable);

    function    GetMaxModuleIndex : integer;
    function    GetUniqueModuleNameSeqNr( aModuleFileName : string): integer;
    function    GetNoOffModuleType( aModuleType : byte): integer;
    function    GetNoOffExtendedModules : integer;

    procedure   RenumberModules(aFromIndex: byte);

    function    ModuleAtLocation(const aRow, aCol : integer): IG2Module;

    function    FindModule( const aModuleIndex : byte): IG2Module;
    function    FindCable(const aFromModuleIndex, aFromConnIndex, aToModuleIndex, aToConnIndex: byte): IG2Cable;
    function    FindParam( const aModuleIndex, aParamIndex : byte): IG2Param;

    function    GetModuleRow( const aModule : IG2Module): byte;
    function    GetMorphValue( aModuleIndex : byte; aParamIndex : byte; aMorphIndex : byte; aVariation : byte): byte;

    function    CheckParametersFit( aSrcePatch: IG2PatchPart): boolean;

    procedure   ResetModuleUprates;
    procedure   CreateModuleUprateList( const aConnector : IG2Connector; const aNewRate : byte; aModuleList : TList<IG2Module>);

    function    CreateCopy(aPatch: IG2Patch; const aLocationIndex : integer): IG2PatchPart;
    function    CreateCopySelected(aPatch: IG2Patch; const aLocationIndex : integer): IG2PatchPart;

    function    CreateSelectedModuleList: TList<IG2Module>;

    procedure   CopyVariation( aFromVariation, aToVariation : byte);

    procedure   SelectAll;
    procedure   UnselectModules;
    procedure   SelectModule(const aModuleIndex : byte; const aValue, aExclusive : boolean);

    function    ProcessResponseMsg( aSender : TObject; aMsg : TG2Message): boolean;
    procedure   ProcessMsgModuleParamLabel( aMsg: TG2Message);

    procedure   SelectNextModuleParam;
    procedure   SelectPrevModuleParam;

    function    CreateModule(const aModuleIndex, aModuleType : byte): IG2Module;

    procedure   InvalidateParameters;
    procedure   InvalidateConnectors;
    procedure   InvalidateCables;

    property    Patch: IG2Patch read GetPatch;
    property    SlotIndex: byte read GetSlotIndex;
    property    LocationIndex: byte read GetLocationIndex;

    property    ModuleList : TList<IG2Module> read GetModuleList;
    property    CableList : TList<IG2Cable> read GetCableList;
    property    LedList : TList<IG2Led> read GetLedList;
    property    LedStripList : TList<IG2Led> read GetLedStripList;

    property    SelectedParam : IG2Param read GetSelectedParam;
    property    FocusedModuleIndex : integer read GetFocusedModuleIndex write SetFocusedModuleIndex;
    property    VariationCount : integer read GetVariationCount;
    property    UnitsRect : TRect read GetUnitsRect;
    property    SelectedUnitsRect : TRect read GetSelectedUnitsRect;

    property    OnAddModule : TAddModuleEvent read GetOnAddModule write SetOnAddModule;
    property    OnAddCable : TAddCableEvent read GetOnAddCable write SetOnAddCable;
    property    OnLed : TLedEvent read GetOnLedEvent write SetOnLedEvent;
  end;

  IG2PatchSettings = interface
    ['{16D1F3EB-70EA-4A52-A2C0-63A4FE0230B2}']
    function GetBarPosition : word;
    function GetRedVisible : boolean;
    function GetBlueVisible : boolean;
    function GetYellowVisible : boolean;
    function GetOrangeVisible : boolean;
    function GetGreenVisible : boolean;
    function GetPurpleVisible : boolean;
    function GetWhiteVisible : boolean;
    function GetCategory : byte;
    function GetVoiceCount : byte;
    function GetVoiceMode : byte;
    function GetActiveVariation : byte;

    procedure SetBarPosition( const aValue : word);
    procedure SetRedVisible( const aValue : boolean);
    procedure SetBlueVisible( const aValue : boolean);
    procedure SetYellowVisible( const aValue : boolean);
    procedure SetOrangeVisible( const aValue : boolean);
    procedure SetGreenVisible( const aValue : boolean);
    procedure SetPurpleVisible( const aValue : boolean);
    procedure SetWhiteVisible( const aValue : boolean);
    procedure SetCategory( const aValue : byte);
    procedure SetActiveVariation( const aValue : byte);
    procedure SetVoiceCount( const aValue : byte);
    procedure SetVoiceMode( const aValue : byte);

    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    procedure WriteChunk( aStream : TStream);

    property BarPosition : word read GetBarPosition write SetBarPosition;
    property RedVisible : boolean read GetRedVisible write SetRedVisible;
    property BlueVisible : boolean read GetBlueVisible write SetBlueVisible;
    property YellowVisible : boolean read GetYellowVisible write SetYellowVisible;
    property OrangeVisible : boolean read GetOrangeVisible write SetOrangeVisible;
    property GreenVisible : boolean read GetGreenVisible write SetGreenVisible;
    property PurpleVisible : boolean read GetPurpleVisible write SetPurpleVisible;
    property WhiteVisible : boolean read GetWhiteVisible write SetWhiteVisible;
    property Categorie : Byte read GetCategory write SetCategory;
    property ActiveVariation : byte read GetActiveVariation write SetActiveVariation;
    property VoiceCount : byte read GetVoiceCount write SetVoiceCount;
    property VoiceMode : Tbits2 read GetVoiceMode write SetVoiceMode;
  end;

  IG2Patch = interface(IG2Object)
  ['{1C655F08-551E-41E5-A314-1855604B05DF}']
    function    GetPatchPart( const aIndex : integer): IG2PatchPart;
    function    GetPatchName : string;
    procedure   SetPatchName(const aValue : string);
    function    GetVariationCount: integer;
    procedure   SetSelectedMorphIndex(const aValue : integer);
    procedure   SetSelectedLocation(const aLocation : integer);

    function    GetSelectedMorphIndex : integer;
    function    GetEditAllVariations : boolean;
    function    GetSelectedLocation : integer;
    function    GetSelectedParamPage : integer;
    function    GetSelectedParamPageColumn : integer;
    function    GetPerf : IG2Perf;
    function    GetSlot : IG2Slot;
    function    GetKnobList: TList<IG2Knob>;
    function    GetControllerList : TList<IG2Controller>;
    function    GetPatchSettings : IG2PatchSettings;
    function    GetSlotIndex: integer;

    procedure   SetEditAllVariations(const aValue: boolean);
    procedure   SetSelectedParamPage(const aValue: integer);
    procedure   SetSelectedParamPageColumn(const aValue: integer);

    procedure   NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure   Init;
    procedure   InitNames;

    procedure   Read( aChunk : TPatchChunk);
    function    ReadChunk( aChunk : TPatchChunk): boolean;
    function    ReadLedData( aStream : TStream): boolean;
    function    ReadVolumeData( aStream : TStream): boolean;
    procedure   Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure   WriteChunk( aID : byte; aChunk : TPatchChunk; aVariationCount : byte);

    procedure   WriteMessSelectParamPage( const aPageIndex : byte; aStream : TStream);

    procedure   AddMessPatchSettings( aPatchSettings : IG2PatchSettings; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessPatchNotes( aLines : TStringList; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAssignKnobs(aModule: IG2Module; const aPageIndex: byte; aSendMessage, aUndoMessage: TG2SendMessage);

    function    ProcessMsg( aSender : TObject; aMsg : TG2Message): boolean;

    procedure   ProcessMsgKnobAssign( aMsg: TG2Message);
    procedure   ProcessMsgKnobDeassign( aMsg: TG2Message);
    procedure   ProcessMsgMidiCCAssign( aMsg: TG2Message);
    procedure   ProcessMsgMidiCCDeassign( aMsg: TG2Message);
    procedure   ProcessMsgPatchDescrChunk( aMsg : TG2Message);

    function    GetModule( LocationIndex, ModuleIndex: integer): IG2Module;

    function    GetMorphKnobParameter(const aIndex : integer): IG2Param;
    function    GetPatchKnobParameter(const aIndex : integer): IG2Param;

    procedure   InvalidateParameters;
    procedure   InvalidateConnectors;
    procedure   InvalidateCables;

    function    GetMaxModuleIndex(const aLocationIndex: integer) : integer;
    function    GetNoOffModuleType(const aLocationIndex, aModuleType : byte) : integer;
    function    GetNoOffExtendedModules : integer; // Number of modules that are not compatible with original editor

    function    GetParameterValue(const aLocationIndex, aModuleIndex, aParamIndex, aVariation : byte): byte;
    procedure   SetParamInPatch(const aLocationIndex, aModuleIndex, aParamIndex, aVariation, aValue: byte);
    procedure   SetParamValue(const aLocationIndex, aModuleIndex, aParamIndex, aVariation, aValue : byte);

    procedure   SelectParamInPatch(const aLocationIndex, aModuleIndex, aParamIndex : byte);
    procedure   SelectParam(const aLocationIndex, aModuleIndex, aParamIndex : byte);

    procedure   SetModeInPatch(const aLocationIndex, aModuleIndex, aParamIndex, aValue : byte);
    function    GetModeValue(const aLocationIndex, aModuleIndex, aParamIndex : byte): byte;
    procedure   SetModeValue(const aLocationIndex, aModuleIndex, aParamIndex, aValue: byte);
    procedure   SetMorphValue(const aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex, aValue, aVariation : byte);
    function    GetMorphValue(const aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): byte;

    procedure   SetMasterClock(const aValue : byte);
    function    GetMasterClock : byte;
    procedure   SetMasterClockRun(const aValue : byte);
    function    GetMasterClockRun : byte;

    procedure   SaveToFile( aStream : TStream);
{$IFDEF MSWINDOWS}
    procedure   SaveAsFXP( aStream : TStream);
{$ENDIF}
    function    LoadFromFile( aStream : TStream): Boolean;

    procedure   DeleteModuleFromPatch(const aLocationIndex: integer; aModule: IG2Module);
    procedure   DeleteCableFromPatch(const aLocationIndex: integer; aCable: IG2Cable);

    function    FindParam(const aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Param;
    function    FindKnob(const aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Knob;
    function    FindController(const aMidiCC : byte): IG2Controller; overload;
    function    FindController(const aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Controller; overload;

    function    FindMorph(const aLocationIndex, aModuleIndex, aParamIndex, aMorphIndex, aVariation : byte): IG2ParamMorph;
    function    HasMorph(const aLocationIndex, aModuleIndex, aParamIndex, aVariation : byte): boolean;

    procedure   CopyVariation(const aFromVariation, aToVariation : byte);

    procedure   UnselectModules(const aLocationIndex : integer);

    property    PatchName : string read GetPatchName write SetPatchName;
    property    VariationCount : integer read GetVariationCount;
    property    SelectedMorphIndex : integer read GetSelectedMorphIndex write SetSelectedMorphIndex;
    property    Perf: IG2Perf read GetPerf;
    property    Slot: IG2Slot read GetSlot;
    property    PatchPart[ const aIndex : integer]: IG2PatchPart read GetPatchPart;

    property    Modules[ LocationIndex, ModuleIndex : integer]: IG2Module read GetModule;
    property    KnobList : TList<IG2Knob> read GetKnobList;
    property    ControllerList : TList<IG2Controller> read GetControllerList;

    property    EditAllVariations : boolean read GetEditAllVariations write SetEditAllVariations;

    property    SelectedLocation : integer read GetSelectedLocation write SetSelectedLocation;
    property    SelectedParamPage : integer read GetSelectedParamPage write SetSelectedParamPage;
    property    SelectedParamPageColumn : integer read GetSelectedParamPageColumn write SetSelectedParamPageColumn;

    property    Settings : IG2PatchSettings read GetPatchSettings;
    property    SlotIndex: integer read GetSlotIndex;
  end;

  IG2Slot = interface(IG2Subject)
  ['{1A21628B-F274-4818-AB49-CDCCAB97AFF3}']
    procedure SetPatchVersion(const aValue : byte);
    function GetSlotIndex: byte;
    function GetPatch: IG2Patch;
    //procedure SetSlotIndex(const aValue : byte);
    procedure SetEnabled(const aValue : TBits8);
    procedure SetHold(const aValue : TBits8);
    procedure SetKeyboard(const aValue : TBits8);
    procedure SetKeyboardRangeTo(const aValue : TBits8);
    procedure SetKeyboardRangeFrom(const aValue : TBits8);
    procedure SetAssignedVoices(const aValue : byte);
    procedure SetInitializing(const aValue : boolean);
    procedure SetOperationMode(const Value: TOperationMode);

    function GetConnection: IG2Connection;
    function GetLog: IG2Log;
    function GetPatchVersion : byte;
    function GetPatchName : string;
    function GetEnabled : TBits8;
    function GetHold : TBits8;
    function GetKeyboard : TBits8;
    function GetBankIndex : byte;
    function GetPatchIndex : byte;
    function GetKeyboardRangeTo : TBits8;
    function GetKeyboardRangeFrom : TBits8;
    function GetPerf : IG2Perf;
    function GetAssignedVoices : byte;
    function GetPatchloadMemVA : single;
    function GetPatchloadMemFX : single;
    function GetPatchloadCyclesVA : single;
    function GetPatchloadCyclesFX : single;
    function GetInitializing : boolean;
    function GetOperationMode: TOperationMode;

    procedure SetPatchName( const aValue : string);
    procedure SetBankIndex( const aValue : byte);
    procedure SetPatchIndex( const aValue : byte);

    function CreatePatch : IG2Patch;

    procedure DoPatchUpdate;
    procedure DoAfterRetreivePatch( Slot, Bank, Patch : byte);

    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure StartInit(aConnection: IG2Connection; const aStartCommAfterInit : boolean);

    procedure AddMsgGetPatchVersion(aMsg: TG2SendMessage);
    procedure AddMsgGetPatch(aMsg: TG2SendMessage);
    procedure AddMsgGetPatchName( aMsg : TG2SendMessage);
    procedure AddMsgGetCurrentNote( aMsg : TG2Message);
    procedure AddMsgGetPatchNotes( aMsg : TG2Message);
    procedure AddMsgGetResourceTable( aMsg : TG2Message; const aLocationIndex: byte);
    procedure AddMsgUnknown6( aMsg: TG2Message);
    procedure AddMsgGetSelectedParam( aMsg: TG2Message);
    procedure AddMsgControllerSnapshot( aMsg: TG2SendMessage);
    procedure AddMsgCopyVariation( const aFromVariation, aToVariation : byte; aMsg: TG2SendMessage);
    procedure AddMsgSelectVariation( const aVariation : byte; aMsg: TG2SendMessage);
    procedure AddMsgPatchLoad(aMsg: TG2Message; const aPatchName: string; aPatch: IG2Patch);

    function  ProcessResponseMsg( aSender : TObject; aMsg : TG2Message): boolean;
    function  ProcessSendMsg( aSender : TObject; aMsg : TG2Message): boolean;
    procedure ProcessMsgVersionChange( aMsg: TG2Message);

    property Connection: IG2Connection read GetConnection;
    property Perf: IG2Perf read GetPerf;
    property Log: IG2Log read GetLog;
    property PatchVersion : byte read GetPatchVersion write SetPatchVersion;
    property SlotIndex : byte read GetSlotIndex;
    property PatchName : string read GetPatchname write SetPatchname;
    property Enabled : TBits8 read GetEnabled write SetEnabled;
    property Hold : TBits8 read GetHold write SetHold;
    property Keyboard : TBits8 read GetKeyboard write SetKeyboard;
    property BankIndex : byte read GetBankIndex;
    property PatchIndex : byte read GetPatchIndex;
    property Upper : TBits8 read GetKeyboardRangeTo write SetKeyboardRangeTo;
    property Lower : TBits8 read GetKeyboardRangeFrom write SetKeyboardRangeFrom;
    property Patch : IG2Patch read GetPatch;
    property AssignedVoices : byte read GetAssignedVoices write SetAssignedVoices;
    property PatchloadMemVA : single read GetPatchloadMemVA;
    property PatchloadMemFX : single read GetPatchloadMemFX;
    property PatchloadCyclesVA : single read GetPatchloadCyclesVA;
    property PatchloadCyclesFX : single read GetPatchloadCyclesFX;
    property Initializing : boolean read GetInitializing write SetInitializing;
    property OperationMode: TOperationMode read GetOperationMode write SetOperationMode;
  end;

  IG2Perf = interface(IG2Object)
  ['{0F65C3F5-E1D6-4A2F-A66D-D91420ECBF30}']
    procedure SetPerfVersion(const aValue : byte);
    procedure SetPerformanceName(const aValue : string);
    procedure SetKeyboardRangeEnabled(const aValue : TBits8);
    procedure SetSelectedSlotIndex(const aValue : TBits2);
    procedure SetMasterClock(const aValue : TBits8);
    procedure SetMasterClockRun(const aValue : TBits8);

    function GetPerfVersion : Byte;
    function GetPerformanceName : string;
    function GetSlot(const aIndex : byte): IG2Slot;
    function GetSelectedSlotIndex : TBits2;
    function GetMasterClock : TBits8;
    function GetMasterClockRun : TBits8;
    function GetKeyboardRangeEnabled : TBits8;
    function GetSelectedParamPage : integer;
    function GetSelectedParamPageColumn : integer;
    function GetGlobalKnobList : TList<IG2Knob>;
    function GetSynth : IG2Synth;
    function GetLastMidiCC : byte;
    function GetLastMidiClock : integer;

    procedure SetSelectedParamPage( const aValue : integer);
    procedure SetSelectedParamPageColumn( const aValue : integer);

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure DoAfterRetreivePatch( Slot, Bank, Patch : byte);

    procedure Init;
    procedure ClearKnobs( const aSlotIndex : integer);

    function FindKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Knob;

    procedure WriteSettings( aChunk : TPatchChunk);

    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk; aVariationCount : byte);
    procedure WriteChunk( aID : byte; aChunk : TPatchChunk);

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure StartInit(aConnection: IG2Connection; const aStartCommAfterInit : boolean);

    procedure AddMsgGetPerfSettings( aMsg : TG2SendMessage);
    procedure AddMsgUnknown2( aMsg : TG2SendMessage);
    procedure AddMsgSelectSlot( aMsg : TG2SendMessage; aSlotIndex: byte);
    procedure AddMsgSetMasterClockBPM( aMsg : TG2SendMessage; aBPM: byte);
    procedure AddMsgSetMasterClockRun( aMsg : TG2SendMessage; aStart: boolean);
    procedure AddMsgSetPerfSettings( aMsg : TG2SendMessage);
    procedure AddMsgSetPerfName( aMsg : TG2SendMessage; aPerfName : string);
    procedure AddMsgGetGlobalKnobs( aMsg : TG2SendMessage);
    procedure AddMsgSelectGlobalParamPage( aMsg : TG2SendMessage; aPageIndex : byte);

    procedure AddMessAssignGlobalKnobs(aModule: IG2Module; const aPageIndex : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    function  ProcessResponseMsg( aSender : TObject; aMsg : TG2Message): boolean;
    function  ProcessSendMsg( aSender : TObject; aMsg : TG2Message): boolean;

    procedure ProcessMsgVersionChange( aMsg: TG2Message);
    procedure ProcessMsgPerfChunk( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobsChunk( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobAssign( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobDeassign( aMsg: TG2Message);

    procedure SaveToFile( aStream : TStream);
    function LoadFromFile( aStream : TStream): Boolean;
{$IFDEF MSWINDOWS}
    procedure SaveAsFXB( aStream : TStream);
    function LoadFromFXB( aStream : TStream): Boolean;
{$ENDIF}
    function GetNoOffExtendedModules : integer; // Number of modules that are not compatible with original editor

    procedure DeleteModuleFromPerf(const aSlotIndex, aLocationIndex: byte; aModule: IG2Module);

    property PerfVersion : Byte read GetPerfVersion write SetPerfVersion;
    property PerformanceName : string read GetPerformanceName write SetPerformanceName;
    property SelectedSlotIndex : TBits2 read GetSelectedSlotIndex write SetSelectedSlotIndex;
    property MasterClock : TBits8 read GetMasterClock write SetMasterClock;
    property MasterClockRun : TBits8 read GetMasterClockRun write SetMasterClockRun;
    property KeyboardRangeEnabled : TBits8 read GetKeyboardRangeEnabled write SetKeyboardRangeEnabled;
    property SelectedParamPage : integer read GetSelectedParamPage write SetSelectedParamPage;
    property SelectedParamPageColumn : integer read GetSelectedParamPageColumn write SetSelectedParamPageColumn;
    property LastMidiCC : byte read GetLastMidiCC;
    property LastMidiClock : integer read GetLastMidiClock;
    property Synth : IG2Synth read GetSynth;
    property Slot[ const aIndex : byte]: IG2Slot read GetSlot;
    property GlobalKnobList : TList<IG2Knob> read GetGlobalKnobList;
  end;

  IG2BankItem = interface
  ['{060F6B1B-8AAE-4AE1-BE6D-36E0298FA2C7}']
    function GetCmd : byte;
    function GetPatchFileType : TPatchFileType;
    function GetBank : byte;
    function GetPatch : byte;
    function GetCategory : byte;
    function GetPatchName : string;

    procedure SetCmd( const aValue : byte);
    procedure SetPatchFileType( const aValue : TPatchFileType);
    procedure SetBank( const aValue : byte);
    procedure SetPatch( const aValue : byte);
    procedure SetCategory( const aValue : byte);
    procedure SetPatchName( const aValue : string);

    property Cmd : byte read GetCmd write SetCmd;
    property PatchFileType : TPatchFileType read GetPatchFileType write SetPatchFileType;
    property Bank : byte read Getbank write SetBank;
    property Patch : byte Read GetPatch write SetPatch;
    property Category : byte read GetCategory write SetCategory;
    property PatchName : string read GetPatchName write SetPatchName;
  end;

  IG2MidiToKnob = interface
  ['{25CC0FEA-E121-42D2-A91A-7370374D8D7C}']
    function GetController: integer;
    function GetControlType: TG2ControlType;
    function GetKnob: integer;
    function GetSource: TG2ParamSource;
    procedure SetController(const Value: integer);
    procedure SetControlType(const Value: TG2ControlType);
    procedure SetKnob(const Value: integer);
    procedure SetSource(const Value: TG2ParamSource);

    property Source: TG2ParamSource read GetSource write SetSource;
    property ControlType: TG2ControlType read GetControlType write SetControlType;
    property Knob: integer read GetKnob write SetKnob;
    property Controller: integer read GetController Write SetController;
  end;

  TNextBankListCmd = record
    Cmd            : Byte;
    PatchFileType  : TPatchFileType;
    Bank           : Byte;
    Patch          : Byte;
  end;

  IG2Synth = interface(IG2Object)
  ['{4E94E893-E28D-4B14-923D-E7BB9F1E11CB}']
    function GetID : integer;

    function GetBankItem( const aIndex : integer) : IG2BankItem;
    function GetBankItemCount: integer;
    function GetPerf : IG2Perf;
    function GetClientType : TClientType;
    function GetAutoAssignMidi : boolean;

    function GetSynthName : string;
    function GetPerfMode : TBits1;
    function GetMemoryProtect : TBits1;
    function GetMidiChannelA : TBits8;
    function GetMidiChannelB : TBits8;
    function GetMidiChannelC : TBits8;
    function GetMidiChannelD : TBits8;
    function GetMidiGlobalChannel : TBits8;
    function GetSysExID : TBits8;
    function GetLocalOn :  TBits1;
    function GetProgramChangeReceive : TBits1;
    function GetProgramChangeSend : TBits1;
    function GetControllersReceive : TBits1;
    function GetControllersSend : TBits1;
    function GetSendClock : TBits1;
    function GetIgnoreExternalClock : TBits1;
    function GetTuneCent : TBits8;
    function GetGlobalOctaveShiftActive : TBits1;
    function GetGlobalOctaveShift : TBits8;
    function GetTuneSemi : TBits8;
    function GetPedalPolarity : TBits1;
    function GetControlPedalGain : TBits8;

    function GetSelectedSlot : IG2Slot;
    function GetSelectedSlotIndex : TBits2;

    function GetBankDumpFolder : string;
    function GetBankDumpFileName : string;
    function GetBankDumpList : TStringList;
    function GetBankDumpListIndex : integer;
    function GetBankDumpDestBank : byte;

    function GetMidiToKnobList : TList<IG2MidiToKnob>;

    function GetNextBankListCmd : TNextBankListCmd;

    procedure SetSynthName( aValue : string);
    procedure SetPerfMode( aValue : TBits1);
    procedure SetMemoryProtect( aValue : TBits1);
    procedure SetMidiChannelA( aValue : TBits8);
    procedure SetMidiChannelB( aValue : TBits8);
    procedure SetMidiChannelC( aValue : TBits8);
    procedure SetMidiChannelD( aValue : TBits8);
    procedure SetMidiGlobalChannel( aValue : TBits8);
    procedure SetSysExID( aValue : TBits8);
    procedure SetLocalOn( aValue :  TBits1);
    procedure SetProgramChangeReceive( aValue : TBits1);
    procedure SetProgramChangeSend( aValue : TBits1);
    procedure SetControllersReceive( aValue : TBits1);
    procedure SetControllersSend( aValue : TBits1);
    procedure SetSendClock( aValue : TBits1);
    procedure SetIgnoreExternalClock( aValue : TBits1);
    procedure SetTuneCent( aValue : TBits8);
    procedure SetGlobalOctaveShiftActive( aValue : TBits1);
    procedure SetGlobalOctaveShift( aValue : TBits8);
    procedure SetTuneSemi( aValue : TBits8);
    procedure SetPedalPolarity( aValue : TBits1);
    procedure SetControlPedalGain( aValue : TBits8);

    procedure SetBankItem( const aIndex : integer; const aValue : IG2BankItem);
    procedure SetClientType( const aValue : TClientType);
    procedure SetAutoAssignMidi( const aValue : boolean);

    procedure SetBankDumpFolder( aValue : string);
    procedure SetBankDumpFileName( aValue : string);
    procedure SetBankDumpListIndex( aValue : integer);
    procedure SetBankDumpDestBank( aValue : byte);

    procedure SetNextBankListCmd(const aValue : TNextBankListCmd);
    procedure SetSelectedSlotIndex(const aValue : TBits2);

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure Init;

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure AddMsgInit(aMsg: TG2SendMessage);
    procedure AddMsgStartStopCommunication(aMsg: TG2SendMessage; const Stop : byte);
    procedure AddMsgGetPatchVersion(aMsg: TG2SendMessage);
    procedure AddMsgGetAssignedVoices(aMsg: TG2SendMessage);
    procedure AddMsgGetSynthSettings(aMsg: TG2SendMessage);
    procedure AddMsgUnknown1Message(aMsg: TG2SendMessage);
    procedure AddMsgMidiDumpMessage(aMsg: TG2SendMessage);
    procedure AddMsgListMessage(aMsg: TG2SendMessage; const aPatchFileType: TPatchFileType; const aBank, aPatch : byte);
    procedure AddMsgSetModeMessage(aMsg: TG2SendMessage; aMode : byte);
    procedure AddMsgNoteMessage(aMsg: TG2SendMessage; aNote : byte; aOnoff : byte);
    procedure AddMsgGetMasterClockMessage(aMsg: TG2SendMessage);
    procedure AddMsgUploadBankMessage(aMsg: TG2SendMessage; aPatchFileType: TPatchFileType; aBank, aLocation: byte);
    procedure AddMsgDownloadPatchBankMessage(aMsg: TG2SendMessage; aBank, aLocation: byte; aPatchName: string; aPatch: IG2Patch);
    procedure AddMsgDownloadPerfBankMessage(aMsg: TG2SendMessage; aBank, aLocation: byte; aPerfName: string; aPerf: IG2Perf);
    procedure AddMsgRetrieveMessage(aMsg: TG2SendMessage; aSlot, aBank, aPatch : byte);
    procedure AddMsgStoreMessage(aMsg: TG2SendMessage; aSlot, aBank, aPatch : byte);
    procedure AddMsgClearBankMessage(aMsg: TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aFromLocation, aToLocation : byte);
    procedure AddMsgClearMessage(aMsg: TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aPatch : byte);
    procedure AddMsgSetSynthSettings(aMsg: TG2Message);
    procedure AddMsgSelGlobalParamPage(aMsg: TG2Message; aPageIndex : byte);
    procedure AddMsgSetPerformance(aMsg: TG2SendMessage; aPerfName : string; aPerf : IG2Perf);

    function ProcessResponseMsg( aSender : TObject; aMsg : TG2Message): boolean;
    function ProcessSendMsg( aSender : TObject; aMsg : TG2Message): boolean;

    procedure BankListClear;
    procedure AddBankItem( aBankItem : IG2BankItem);
    function BankFindNext: IG2BankItem;
    function BankFindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;

    procedure StartInit(aConnection: IG2Connection);

    procedure AddMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer; const aMidiCC : byte);
    procedure DeleteMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer);
    function FindMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer): integer;
    function MidiToKnobCheckCC(const aMidiCC: byte): boolean;

    function TextFunction( No : integer; Param1, Param2, Param3 : byte): string;

    function G2MessageDlg( tekst : string; code : integer): integer; // For sending messages to UI

    property SynthName : string read GetSynthName write SetSynthName;
    property PerfMode : TBits1 read GetPerfMode write SetPerfMode;
    property MemoryProtect : TBits1 read GetMemoryProtect write SetMemoryProtect;
    property MidiChannelA : TBits8 read GetMidiChannelA write SetMidiChannelA;
    property MidiChannelB : TBits8 read GetMidiChannelB write SetMidiChannelB;
    property MidiChannelC : TBits8 read GetMidiChannelC write SetMidiChannelC;
    property MidiChannelD : TBits8 read GetMidiChannelD write SetMidiChannelD;
    property MidiGlobalChannel : TBits8 read GetMidiGlobalChannel write SetMidiGlobalChannel;
    property SysExID : TBits8 read GetSysExID write SetSysExID;
    property LocalOn : TBits1 read GetLocalOn write SetLocalOn;
    property ProgramChangeReceive : TBits1 read GetProgramChangeReceive write SetProgramChangeReceive;
    property ProgramChangeSend : TBits1 read GetProgramChangeSend write SetProgramChangeSend;
    property ControllersReceive : TBits1 read GetControllersReceive write SetControllersReceive;
    property ControllersSend : TBits1 read GetControllersSend write SetControllersSend;
    property SendClock : TBits1 read GetSendClock write SetSendClock;
    property IgnoreExternalClock : TBits1 read GetIgnoreExternalClock write SetIgnoreExternalClock;
    property TuneCent : TBits8 read GetTuneCent write SetTuneCent;
    property GlobalOctaveShiftActive : TBits1 read GetGlobalOctaveShiftActive write SetGlobalOctaveShiftActive;
    property GlobalOctaveShift : TBits8 read GetGlobalOctaveShift write SetGlobalOctaveShift;
    property TuneSemi : TBits8 read GetTuneSemi write SetTuneSemi;
    property PedalPolarity : TBits1 read GetPedalPolarity write SetPedalPolarity;
    property ControlPedalGain : TBits8 read GetControlPedalGain write SetControlPedalGain;

    property BankDumpFolder : string read GetBankDumpFolder write SetBankDumpFolder;
    property BankDumpFileName : string read GetBankDumpFileName write SetBankDumpFileName;
    property BankDumpList : TStringList read GetBankDumpList;
    property BankDumpListIndex : integer read GetBankDumpListIndex write SetBankDumpListIndex;
    property BankDumpDestBank : byte read GetBankDumpDestBank write SetBankDumpDestBank;

    property NextBankListCmd : TNextBankListCmd read GetNextBankListCmd write SetNextBankListCmd;

    property ID : integer read GetID;
    property ClientType : TClientType read GetClientType write SetClientType;
    property Perf : IG2Perf read GetPerf;
    property SelectedSlotIndex : TBits2 read GetSelectedSlotIndex write SetSelectedSlotIndex;
    property AutoAssignMidi : boolean read GetAutoAssignMidi write SetAutoAssignMidi;
    property BankItems[ const aIndex : integer] : IG2BankItem read GetBankItem write SetBankItem;
    property BankItemCount : integer read GetBankItemCount;
    property MidiToKnobList : TList<IG2MidiToKnob> read GetMidiToKnobList;
  end;

  TG2CommandState = (cmdReady, cmdDone, cmdUndone, cmdStuck);

  IG2Command = interface
  ['{627E48D5-4DA8-49CC-B843-782EA9CB9C54}']
    function GetState : TG2CommandState;
    procedure Execute;

    property State : TG2CommandState read GetState;
  end;

  IG2UndoableCommand = interface(IG2Command)
  ['{9D3ACF01-5589-4E70-87AF-A134BA4C4478}']
    procedure Rollback; // Reverse effect of Execute
    procedure Redo;
  end;

  IG2Client = interface
  ['{5967182A-2D7C-4D51-80F8-5AD17889F5EF}']
    function GetConnected: boolean;

    procedure Connect;
    procedure Disconnect;

    procedure SendMsg(aMsg: TG2SendMessage);
    procedure AddParamUpdRec(const aSlot, aVersion, aSubCmd, aLocation, aModule,
      aParam, aMorph, aValue, aNegative, aVariation: byte);

    property Connected: boolean read GetConnected;
  end;

  TOnNextMsgEvent = procedure(Sender: TObject; aConnection: IG2Connection) of object;

  IG2Connection = interface(IG2Subject)
  ['{D1AB996F-4BBF-40EB-A985-DEA63CB662F2}']

    function GetOnNextMsg : TOnNextMsgEvent;
    procedure SetOnNextMsg( const aValue : TOnNextMsgEvent);
    function GetLog : IG2Log;
    procedure SetLog( const aValue : IG2Log);
    function GetConnected : boolean;
    function GetOnConnect : TNotifyEvent;
    procedure SetOnConnect( aValue : TNotifyEvent);
    function GetOnDisconnect : TNotifyEvent;
    procedure SetOnDisconnect(aValue : TNotifyEvent);
    function GetProcessResponseMsg: TG2MessageEvent;
    procedure SetProcessResponseMsg(aValue : TG2MessageEvent);
    function GetProcessSendMsg: TG2MessageEvent;
    procedure SetProcessSendMsg(aValue : TG2MessageEvent);

    function GetSynth: IG2Synth;
    function GetPerf: IG2Perf;
    function GetSlot(const aSlotIndex : integer): IG2Slot;
    function GetPatch(const aSlotIndex : integer): IG2Patch;
    function GetPatchPart(const aIndex, aLocationIndex : integer): IG2PatchPart;
    function GetModule(const aSlotIndex, aLocationIndex, aModuleIndex: integer): IG2Module;
    function GetParam(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Param;
    function GetInConnector(const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer): IG2Connector;
    function GetOutConnector(const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer): IG2Connector;
    function GetController(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Controller;
    function GetKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Knob;
    function GetGlobalKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Knob;
    function GetCopyPatch: IG2PatchPart;
    function GetCopyVariation: byte;
    procedure SetCopyPatch(const Value: IG2PatchPart);
    procedure SetCopyVariation(const Value: byte);
    function GetSelectMultiple: boolean;
    procedure SetSelectMultiple(const Value: boolean);
    function GetClient: IG2Client;
    procedure SetClient(aValue: IG2Client);

    function GetSelectedSlot: IG2Slot;
    function GetSelectedSlotIndex: integer;
    function GetSelectedPatch: IG2Patch;
    function GetSelectedLocationIndex: integer;
    procedure SetSelectedLocationIndex(const aValue: integer);
    function GetSelectedPatchPart: IG2PatchPart;
    //function GetSelectedModuleList: TList<IG2Module>;
    function GetSelectedModuleCount: integer;
    function GetFocusedModule: IG2Module;
    function GetSelectedParam: IG2Param;
    function GetSelectedVariation: byte;
    function GetSelectedMorphIndex: byte;

    function GetLogLevel: integer;
    procedure SetLogLevel(const Value: integer);
    function GetLastG2Error: string;
    procedure SetLastG2Error(const aValue : string);
    function GetLastResponsMessage: byte;
    procedure SetLastResponsMessage(const Value: byte);

    procedure ExecuteCommand(aCommand : IG2Command);

    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure PasteParams;
    procedure Delete;
    procedure Undo;
    procedure Redo;

    procedure LoadBank(const aPatchType : TPatchFileType; const aFileName: string; const aDest: byte);
    procedure SaveBank(const aPatchType : TPatchFileType; const aFileName: string; const aSource: byte);

    procedure Push(aCommand : IG2UndoableCommand);

    procedure LoadSettings;
    procedure SaveSettings;

    procedure Connect;
    procedure Disconnect;

    procedure AddLog(const aLogLine: string; const aLogCmd : integer);
    procedure DumpBuffer(var buffer; max_size : integer);

    procedure SendMsg(aMsg : TG2SendMessage);

    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoBeforeSendMsg( aMsg : TG2Message);
    procedure DoProcessResponseMsg( aMsg : TG2Message);
    procedure DoProcessSendMsg( aMsg : TG2Message);
    procedure DoNextMsg;

    procedure ParamControllerAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMidiCC: byte);
    procedure ParamKnobAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte);
    procedure ParamGlobalKnobAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte);
    procedure ParamGlobalKnobDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
    procedure ParamKnobDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
    procedure ParamControllerDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
    procedure ParamLabel(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aLabelIndex: byte; aLabel : string);
    procedure ParamSetValue(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aVariation, aValue: byte);
    procedure ParamSelect(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: byte);
    procedure ParamSetMorph(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aMorph, aValue, aNegative, aVariation: byte);

    procedure ModuleLabel(const aSlotIndex, aLocationIndex, aModuleIndex: byte; const aLabel : string);
    procedure ModuleColor(const aSlotIndex, aLocationIndex, aModuleIndex, aModuleColor : byte);
    procedure ModuleGlobalKnobsAssign(const aSlotIndex, aLocationIndex, aModuleIndex, aPageIndex : byte);
    procedure ModuleKnobsAssign(const aSlotIndex, aLocationIndex, aModuleIndex, aPageIndex : byte);

    procedure PatchSettings( aPatch : IG2Patch; aPatchSettings : IG2PatchSettings);
    procedure PatchModuleAdd( aPatchPart : IG2PatchPart; aModuleTypeID, aCol, aRow: byte);
    procedure PatchCableAdd(const aSlotIndex, aLocationIndex, aFromModuleIndex,
      aFromConnectorIndex: byte; const aFromConnectorKind : TConnectorKind;
      const aFromConnectorColor: TCableColor; const aToModuleIndex,
      aToConnectorIndex: byte; const aToConnectorKind : TConnectorKind;
      const aToConnectorColor: TCableColor);
    procedure PatchCableDelete(const aSlotIndex, aLocationIndex,
      aFromModuleIndex, aFromConnectorIndex: byte;
      const aFromConnectorKind : TConnectorKind; const aToModuleIndex,
      aToConnectorIndex: byte; const aToConnectorKind : TConnectorKind);
    procedure PatchModulesSelectedMove( aPatchPart: IG2PatchPart; aModuleChangeList : TModuleChangeList);
    procedure PatchModulesSelectedCopy( aDestPatchPart, aSrcePatchPart: IG2PatchPart);
    procedure PatchModulesSelectedDelete( aPatchPart: IG2PatchPart);
    procedure PatchParamsCopy( aDestPatchPart, aSrcePatchPart: IG2PatchPart; aFromVariation, aToVariation: byte);

    procedure SlotGetPatchVersion(const aSlotIndex: byte);
    procedure SlotGetPatch(const aSlotIndex: byte);
    procedure SlotGetPatchName(const aSlotIndex: byte);
    procedure SlotGetCurrentNote(const aSlotIndex: byte);
    procedure SlotGetPatchNotes(const aSlotIndex: byte);
    procedure SlotGetResourceTable(const aSlotIndex, aLocation: byte);
    procedure SlotUnknown6(const aSlotIndex: byte);
    procedure SlotGetSelectedParam(const aSlotIndex: byte);
    procedure SlotSetPatchName(const aSlotIndex: byte; aPatchName : string);
    procedure SlotSetPatchCategory(const aSlotIndex, aCategory : byte);
    procedure SlotSetVoices(const aSlotIndex, aVoiceCount, aVoiceMode : byte);
    procedure SlotSelectVariation(const aSlotIndex, aVariation : byte);
    procedure SlotCopyVariation(const aSlotIndex, aFromVariation, aToVariation : byte);
    procedure SlotPatchLoad(const aSlotIndex: byte; const aPatchName : string; aPatch : IG2Patch);
    procedure SlotControllerSnapshot(const aSlotIndex: byte);
    procedure SlotVariationSelect(const aSlotIndex, aVariation : byte);

    procedure PerfGetSettings;
    procedure PerfSetSettings;
    procedure PerfSelectSlot(const aSlotIndex : byte);
    procedure PerfName(const aPerfName : string);
    procedure PerfUnknown2Message;
    procedure PerfGetGlobalKnobs;

    procedure SynthInit;
    procedure SynthStartStopCommunication(const aStop: byte);
    procedure SynthGetPatchVersion;
    procedure SynthUnknown1Message;
    procedure SynthGetAssignedVoices;
    procedure SynthGetMasterClock;
    procedure SynthGetList(const aPatchFileType: TPatchFileType; const aBank, aPatch : byte);
    procedure SynthRetreivePatch(const aSlotIndex, aBankIndex, aPatchIndex : byte);
    procedure SynthStorePatch(const aSlotIndex, aBankIndex, aPatchIndex : byte);
    procedure SynthBankClear(const PatchFileType : TPatchFileType; const aBank, aFromLocation, aToLocation: byte);
    procedure SynthPatchClear(const aPatchFileType : TPatchFileType; const aBank, aPatch : byte);
    procedure SynthBankUpload(const aPatchFileType: TPatchFileType; const aBank, aLocation: byte);
    procedure SynthBankPatchDownload(const aBank, aLocation: byte; const aPatchName: string; aPatch: IG2Patch);
    procedure SynthPerfLoad(const aPerfName : string; aPerf : IG2Perf);
    procedure SynthPerfMode(const aPerfMode: byte);
    procedure SynthNoteOnOff(const aNote, aOnOff: byte);
    procedure SynthMidiDump;
    procedure SynthSettings;

    property Connected : boolean read GetConnected;
    property Synth : IG2Synth read GetSynth;
    property Perf : IG2Perf read GetPerf;
    property Slot[const aSlotIndex : integer] : IG2Slot read GetSlot;
    property Patch[const aSlotIndex : integer] : IG2Patch read GetPatch;
    property PatchPart[const aSlotIndex, aLocationIndex : integer] : IG2PatchPart read GetPatchPart;
    property Module[const aSlotIndex, aLocationIndex, aModuleIndex: integer]: IG2Module read GetModule;
    property Param[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Param read GetParam;
    property InConnector[const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer]: IG2Connector read GetInConnector;
    property OutConnector[const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer]: IG2Connector read GetOutConnector;
    property Controller[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Controller read GetController;
    property Knob[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Knob read GetKnob;
    property GlobalKnob[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Knob read GetGlobalKnob;
    property CopyPatch: IG2PatchPart read GetCopyPatch write SetCopyPatch;
    property CopyVariation: byte read GetCopyVariation write SetCopyVariation;

    property SelectedSlot: IG2Slot read GetSelectedSlot;
    property SelectedSlotIndex: integer read GetSelectedSlotIndex;
    property SelectedPatch: IG2Patch read GetSelectedPatch;
    property SelectedLocationIndex: integer read GetSelectedLocationIndex write SetSelectedLocationIndex;
    property SelectedPatchPart: IG2PatchPart read GetSelectedPatchPart;
    //property SelectedModuleList: TList<IG2Module> read GetSelectedModuleList;
    property SelectedModuleCount: integer read GetSelectedModuleCount;
    property FocusedModule: IG2Module read GetFocusedModule;
    property SelectedParam: IG2Param read GetSelectedParam;
    property SelectedVariation: byte read GetSelectedVariation;
    property SelectedMorphIndex: byte read GetSelectedMorphIndex;

    property SelectMultiple: boolean read GetSelectMultiple write SetSelectMultiple;

    property Log : IG2Log read GetLog write SetLog;
    property LogLevel: integer read GetLogLevel write SetLogLevel;

    property LastG2Error: string read GetLastG2Error write SetLastG2Error;
    property LastResponsMessage: byte read GetLastResponsMessage write SetLastResponsMessage;

    property Client: IG2Client read GetClient write SetClient;

    property OnConnect : TNotifyEvent read GetOnConnect write SetOnConnect;
    property OnDisconnect : TNotifyEvent read GetOnDisconnect write SetOnDisconnect;
    property OnProcessResponseMsg : TG2MessageEvent read GetProcessResponseMsg write SetProcessResponseMsg;
    property OnProcessSendMsg : TG2MessageEvent read GetProcessSendMsg write SetProcessSendMsg;
    property OnNextmsg : TOnNextMsgEvent read GetOnNextMsg write SetOnNextMsg;
  end;

  IG2ConnectionManager = interface(IG2Subject)
    ['{A6CB1AA1-4684-4F7E-A60F-BAA0E89202F4}']
    function GetConnectionList: TList<IG2Connection>;
    function GetSelectedConnection: IG2Connection;
    function GetSelectedConnectionIndex: integer;
    procedure SetSelectedConnectionIndex(const Value: integer);
    function GetSelectedSlot: IG2Slot;
    function GetSelectedSlotIndex: integer;
    function GetSelectedPatch: IG2Patch;
    function GetSelectedLocationIndex: integer;
    procedure SetSelectedLocationIndex(const aValue: integer);
    function GetSelectedPatchPart: IG2PatchPart;
    //function GetSelectedModuleList: TList<IG2Module>;
    //function GetSelectedModuleCount: integer;
    function GetFocusedModule: IG2Module;
    function GetSelectedParam: IG2Param;
    function GetAutoAssignMidi: integer;
    function GetPatchDir: string;
    function GetCableStyle: TCableStyle;
    function GetKnobControl: TKnobControl;
    function GetSplitterVisible: integer;
    function GetZoomSetting1: single;
    function GetZoomSetting2: single;
    function GetZoomSetting3: single;
    procedure SetAutoAssignMidi(const aValue: integer);
    procedure SetPatchDir(const aValue: string);
    procedure SetCableStyle(const Value: TCableStyle);
    procedure SetKnobControl(const Value: TKnobControl);
    procedure SetSplitterVisible(const Value: integer);
    procedure SetZoomSetting1(const Value: single);
    procedure SetZoomSetting2(const Value: single);
    procedure SetZoomSetting3(const Value: single);
    function GetSelectedModuleType: byte;
    procedure SetSelectedModuleType(const Value: byte);
    function GetSelectedCol: byte;
    function GetSelectedRow: byte;
    procedure SetSelectedCol(const Value: byte);
    procedure SetSelectedRow(const Value: byte);

    procedure DeviceDiscovery;
    procedure DisconnectAll;

    procedure InitDefaultSettings;
    procedure LoadSettings;
    procedure SaveSettings;

    procedure LoadFromFile(const aFileName: string);

    property PatchDir : string read GetPatchDir write SetPatchDir;
    property ZoomSetting1 : single read GetZoomSetting1 write SetZoomSetting1;
    property ZoomSetting2 : single read GetZoomSetting2 write SetZoomSetting2;
    property ZoomSetting3 : single read GetZoomSetting3 write SetZoomSetting3;
    property AutoAssignMidi : integer read GetAutoAssignMidi write SetAutoAssignMidi;
    property KnobControl : TKnobControl read GetKnobControl write SetKnobControl;
    property CableStyle : TCableStyle read GetCableStyle write SetCableStyle;
    property SplitterVisible : integer read GetSplitterVisible write SetSplitterVisible;

    property ConnectionList: TList<IG2Connection> read GetConnectionList;

    property SelectedConnection: IG2Connection read GetSelectedConnection;
    property SelectedConnectionIndex: integer read GetSelectedConnectionIndex write SetSelectedConnectionIndex;
    property SelectedSlot: IG2Slot read GetSelectedSlot;
    property SelectedSlotIndex: integer read GetSelectedSlotIndex;
    property SelectedPatch: IG2Patch read GetSelectedPatch;
    property SelectedLocationIndex: integer read GetSelectedLocationIndex write SetSelectedLocationIndex;
    property SelectedPatchPart: IG2PatchPart read GetSelectedPatchPart;
    //property SelectedModuleList: TList<IG2Module> read GetSelectedModuleList;
    //property SelectedModuleCount: integer read GetSelectedModuleCount;
    property SelectedModuleType: byte read GetSelectedModuleType write SetSelectedModuleType;
    property FocusedModule: IG2Module read GetFocusedModule;
    property SelectedRow: byte read GetSelectedRow write SetSelectedRow;
    property SelectedCol: byte read GetSelectedCol write SetSelectedCol;
    property SelectedParam: IG2Param read GetSelectedParam;
  end;

implementation

end.
