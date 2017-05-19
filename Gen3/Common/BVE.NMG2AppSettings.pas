
{******************************************************************************************************************}
{                                                                                                                  }
{                                                 XML Data Binding                                                 }
{                                                                                                                  }
{         Generated on: 26-1-2014 10:14:09                                                                         }
{       Generated from: C:\Users\Bruno Verhue\Documents\Delphi\nmg2editor\nmg2_editor_FMX\Deploy\g2editorfmx.xml   }
{   Settings stored in: C:\Users\Bruno Verhue\Documents\Delphi\nmg2editor\nmg2_editor_FMX\Deploy\g2editorfmx.xdb   }
{                                                                                                                  }
{******************************************************************************************************************}

unit BVE.NMG2AppSettings;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLAppSettingsType = interface;
  IXMLGeneralType = interface;
  IXMLG2SynthsType = interface;
  IXMLG2SynthType = interface;
  IXMLMidiToKnobsType = interface;
  IXMLMidiToKnobType = interface;

{ IXMLAppSettingsType }

  IXMLAppSettingsType = interface(IXMLNode)
    ['{457D7685-F100-411F-BAB3-10CE206B2746}']
    { Property Accessors }
    function Get_Xmlns: Integer;
    function Get_General: IXMLGeneralType;
    function Get_G2Synths: IXMLG2SynthsType;
    procedure Set_Xmlns(Value: Integer);
    { Methods & Properties }
    property Xmlns: Integer read Get_Xmlns write Set_Xmlns;
    property General: IXMLGeneralType read Get_General;
    property G2Synths: IXMLG2SynthsType read Get_G2Synths;
  end;

{ IXMLGeneralType }

  IXMLGeneralType = interface(IXMLNode)
    ['{BBC1C1A2-9950-4BE0-A425-5E0E920D2495}']
    { Property Accessors }
    function Get_KnobControl: Integer;
    function Get_CableStyle: Integer;
    function Get_AutoAssignMidiToKnob: Integer;
    function Get_PatchDir: string;
    function Get_SplitterVisible: Integer;
    function Get_Zoom1: string;
    function Get_Zoom2: string;
    function Get_Zoom3: string;
    procedure Set_KnobControl(Value: Integer);
    procedure Set_CableStyle(Value: Integer);
    procedure Set_AutoAssignMidiToKnob(Value: Integer);
    procedure Set_PatchDir(Value: string);
    procedure Set_SplitterVisible(Value: Integer);
    procedure Set_Zoom1(Value: string);
    procedure Set_Zoom2(Value: string);
    procedure Set_Zoom3(Value: string);
    { Methods & Properties }
    property KnobControl: Integer read Get_KnobControl write Set_KnobControl;
    property CableStyle: Integer read Get_CableStyle write Set_CableStyle;
    property AutoAssignMidiToKnob: Integer read Get_AutoAssignMidiToKnob write Set_AutoAssignMidiToKnob;
    property PatchDir: string read Get_PatchDir write Set_PatchDir;
    property SplitterVisible: Integer read Get_SplitterVisible write Set_SplitterVisible;
    property Zoom1: string read Get_Zoom1 write Set_Zoom1;
    property Zoom2: string read Get_Zoom2 write Set_Zoom2;
    property Zoom3: string read Get_Zoom3 write Set_Zoom3;
  end;

{ IXMLG2SynthsType }

  IXMLG2SynthsType = interface(IXMLNodeCollection)
    ['{407C5E23-3D85-4CD7-AE9C-2A352A6BA9DA}']
    { Property Accessors }
    function Get_G2Synth(Index: Integer): IXMLG2SynthType;
    { Methods & Properties }
    function Add: IXMLG2SynthType;
    function Insert(const Index: Integer): IXMLG2SynthType;
    property G2Synth[Index: Integer]: IXMLG2SynthType read Get_G2Synth; default;
  end;

{ IXMLG2SynthType }

  IXMLG2SynthType = interface(IXMLNode)
    ['{D2A5C3F9-AB1A-4440-AF99-DB5ECB009F13}']
    { Property Accessors }
    function Get_Name: string;
    function Get_UsbConnection: Boolean;
    function Get_Host: string;
    function Get_Port: Integer;
    function Get_MidiToKnobs: IXMLMidiToKnobsType;
    procedure Set_Name(Value: string);
    procedure Set_UsbConnection(Value: Boolean);
    procedure Set_Host(Value: string);
    procedure Set_Port(Value: Integer);
    { Methods & Properties }
    property Name: string read Get_Name write Set_Name;
    property UsbConnection: Boolean read Get_UsbConnection write Set_UsbConnection;
    property Host: string read Get_Host write Set_Host;
    property Port: Integer read Get_Port write Set_Port;
    property MidiToKnobs: IXMLMidiToKnobsType read Get_MidiToKnobs;
  end;

{ IXMLMidiToKnobsType }

  IXMLMidiToKnobsType = interface(IXMLNodeCollection)
    ['{251634F7-6966-4EE8-BC0D-A7C1A021A0D6}']
    { Property Accessors }
    function Get_MidiToKnob(Index: Integer): IXMLMidiToKnobType;
    { Methods & Properties }
    function Add: IXMLMidiToKnobType;
    function Insert(const Index: Integer): IXMLMidiToKnobType;
    property MidiToKnob[Index: Integer]: IXMLMidiToKnobType read Get_MidiToKnob; default;
  end;

{ IXMLMidiToKnobType }

  IXMLMidiToKnobType = interface(IXMLNode)
    ['{C36ECF49-0EC0-41ED-8EAB-7D621DC42C60}']
    { Property Accessors }
    function Get_Source: string;
    function Get_ControlType: Integer;
    function Get_Knob: Integer;
    function Get_MidiCC: Integer;
    procedure Set_Source(Value: string);
    procedure Set_ControlType(Value: Integer);
    procedure Set_Knob(Value: Integer);
    procedure Set_MidiCC(Value: Integer);
    { Methods & Properties }
    property Source: string read Get_Source write Set_Source;
    property ControlType: Integer read Get_ControlType write Set_ControlType;
    property Knob: Integer read Get_Knob write Set_Knob;
    property MidiCC: Integer read Get_MidiCC write Set_MidiCC;
  end;

{ Forward Decls }

  TXMLAppSettingsType = class;
  TXMLGeneralType = class;
  TXMLG2SynthsType = class;
  TXMLG2SynthType = class;
  TXMLMidiToKnobsType = class;
  TXMLMidiToKnobType = class;

{ TXMLAppSettingsType }

  TXMLAppSettingsType = class(TXMLNode, IXMLAppSettingsType)
  protected
    { IXMLAppSettingsType }
    function Get_Xmlns: Integer;
    function Get_General: IXMLGeneralType;
    function Get_G2Synths: IXMLG2SynthsType;
    procedure Set_Xmlns(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGeneralType }

  TXMLGeneralType = class(TXMLNode, IXMLGeneralType)
  protected
    { IXMLGeneralType }
    function Get_KnobControl: Integer;
    function Get_CableStyle: Integer;
    function Get_AutoAssignMidiToKnob: Integer;
    function Get_PatchDir: string;
    function Get_SplitterVisible: Integer;
    function Get_Zoom1: string;
    function Get_Zoom2: string;
    function Get_Zoom3: string;
    procedure Set_KnobControl(Value: Integer);
    procedure Set_CableStyle(Value: Integer);
    procedure Set_AutoAssignMidiToKnob(Value: Integer);
    procedure Set_PatchDir(Value: string);
    procedure Set_SplitterVisible(Value: Integer);
    procedure Set_Zoom1(Value: string);
    procedure Set_Zoom2(Value: string);
    procedure Set_Zoom3(Value: string);
  end;

{ TXMLG2SynthsType }

  TXMLG2SynthsType = class(TXMLNodeCollection, IXMLG2SynthsType)
  protected
    { IXMLG2SynthsType }
    function Get_G2Synth(Index: Integer): IXMLG2SynthType;
    function Add: IXMLG2SynthType;
    function Insert(const Index: Integer): IXMLG2SynthType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLG2SynthType }

  TXMLG2SynthType = class(TXMLNode, IXMLG2SynthType)
  protected
    { IXMLG2SynthType }
    function Get_Name: string;
    function Get_UsbConnection: Boolean;
    function Get_Host: string;
    function Get_Port: Integer;
    function Get_MidiToKnobs: IXMLMidiToKnobsType;
    procedure Set_Name(Value: string);
    procedure Set_UsbConnection(Value: Boolean);
    procedure Set_Host(Value: string);
    procedure Set_Port(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMidiToKnobsType }

  TXMLMidiToKnobsType = class(TXMLNodeCollection, IXMLMidiToKnobsType)
  protected
    { IXMLMidiToKnobsType }
    function Get_MidiToKnob(Index: Integer): IXMLMidiToKnobType;
    function Add: IXMLMidiToKnobType;
    function Insert(const Index: Integer): IXMLMidiToKnobType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLMidiToKnobType }

  TXMLMidiToKnobType = class(TXMLNode, IXMLMidiToKnobType)
  protected
    { IXMLMidiToKnobType }
    function Get_Source: string;
    function Get_ControlType: Integer;
    function Get_Knob: Integer;
    function Get_MidiCC: Integer;
    procedure Set_Source(Value: string);
    procedure Set_ControlType(Value: Integer);
    procedure Set_Knob(Value: Integer);
    procedure Set_MidiCC(Value: Integer);
  end;

{ Global Functions }

function GetAppSettings(Doc: IXMLDocument): IXMLAppSettingsType;
function LoadAppSettings(const FileName: string): IXMLAppSettingsType;
function NewAppSettings: IXMLAppSettingsType;

const
  TargetNamespace = 'http://www.yourtargetnamespace.com';

implementation

{ Global Functions }

function GetAppSettings(Doc: IXMLDocument): IXMLAppSettingsType;
begin
  Result := Doc.GetDocBinding('AppSettings', TXMLAppSettingsType, TargetNamespace) as IXMLAppSettingsType;
end;

function LoadAppSettings(const FileName: string): IXMLAppSettingsType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('AppSettings', TXMLAppSettingsType, TargetNamespace) as IXMLAppSettingsType;
end;

function NewAppSettings: IXMLAppSettingsType;
begin
  Result := NewXMLDocument.GetDocBinding('AppSettings', TXMLAppSettingsType, TargetNamespace) as IXMLAppSettingsType;
end;

{ TXMLAppSettingsType }

procedure TXMLAppSettingsType.AfterConstruction;
begin
  RegisterChildNode('General', TXMLGeneralType);
  RegisterChildNode('G2Synths', TXMLG2SynthsType);
  inherited;
end;

function TXMLAppSettingsType.Get_Xmlns: Integer;
begin
  Result := AttributeNodes['xmlns'].NodeValue;
end;

procedure TXMLAppSettingsType.Set_Xmlns(Value: Integer);
begin
  SetAttribute('xmlns', Value);
end;

function TXMLAppSettingsType.Get_General: IXMLGeneralType;
begin
  Result := ChildNodes['General'] as IXMLGeneralType;
end;

function TXMLAppSettingsType.Get_G2Synths: IXMLG2SynthsType;
begin
  Result := ChildNodes['G2Synths'] as IXMLG2SynthsType;
end;

{ TXMLGeneralType }

function TXMLGeneralType.Get_KnobControl: Integer;
begin
  Result := AttributeNodes['knobControl'].NodeValue;
end;

procedure TXMLGeneralType.Set_KnobControl(Value: Integer);
begin
  SetAttribute('knobControl', Value);
end;

function TXMLGeneralType.Get_CableStyle: Integer;
begin
  Result := AttributeNodes['cableStyle'].NodeValue;
end;

procedure TXMLGeneralType.Set_CableStyle(Value: Integer);
begin
  SetAttribute('cableStyle', Value);
end;

function TXMLGeneralType.Get_AutoAssignMidiToKnob: Integer;
begin
  Result := AttributeNodes['autoAssignMidiToKnob'].NodeValue;
end;

procedure TXMLGeneralType.Set_AutoAssignMidiToKnob(Value: Integer);
begin
  SetAttribute('autoAssignMidiToKnob', Value);
end;

function TXMLGeneralType.Get_PatchDir: string;
begin
  Result := AttributeNodes['patchDir'].Text;
end;

procedure TXMLGeneralType.Set_PatchDir(Value: string);
begin
  SetAttribute('patchDir', Value);
end;

function TXMLGeneralType.Get_SplitterVisible: Integer;
begin
  Result := AttributeNodes['splitterVisible'].NodeValue;
end;

procedure TXMLGeneralType.Set_SplitterVisible(Value: Integer);
begin
  SetAttribute('splitterVisible', Value);
end;

function TXMLGeneralType.Get_Zoom1: string;
begin
  Result := AttributeNodes['Zoom1'].Text;
end;

procedure TXMLGeneralType.Set_Zoom1(Value: string);
begin
  SetAttribute('Zoom1', Value);
end;

function TXMLGeneralType.Get_Zoom2: string;
begin
  Result := AttributeNodes['Zoom2'].Text;
end;

procedure TXMLGeneralType.Set_Zoom2(Value: string);
begin
  SetAttribute('Zoom2', Value);
end;

function TXMLGeneralType.Get_Zoom3: string;
begin
  Result := AttributeNodes['Zoom3'].Text;
end;

procedure TXMLGeneralType.Set_Zoom3(Value: string);
begin
  SetAttribute('Zoom3', Value);
end;

{ TXMLG2SynthsType }

procedure TXMLG2SynthsType.AfterConstruction;
begin
  RegisterChildNode('G2Synth', TXMLG2SynthType);
  ItemTag := 'G2Synth';
  ItemInterface := IXMLG2SynthType;
  inherited;
end;

function TXMLG2SynthsType.Get_G2Synth(Index: Integer): IXMLG2SynthType;
begin
  Result := List[Index] as IXMLG2SynthType;
end;

function TXMLG2SynthsType.Add: IXMLG2SynthType;
begin
  Result := AddItem(-1) as IXMLG2SynthType;
end;

function TXMLG2SynthsType.Insert(const Index: Integer): IXMLG2SynthType;
begin
  Result := AddItem(Index) as IXMLG2SynthType;
end;

{ TXMLG2SynthType }

procedure TXMLG2SynthType.AfterConstruction;
begin
  RegisterChildNode('MidiToKnobs', TXMLMidiToKnobsType);
  inherited;
end;

function TXMLG2SynthType.Get_Name: string;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLG2SynthType.Set_Name(Value: string);
begin
  SetAttribute('name', Value);
end;

function TXMLG2SynthType.Get_UsbConnection: Boolean;
begin
  Result := AttributeNodes['usbConnection'].NodeValue;
end;

procedure TXMLG2SynthType.Set_UsbConnection(Value: Boolean);
begin
  SetAttribute('usbConnection', Value);
end;

function TXMLG2SynthType.Get_Host: string;
begin
  Result := AttributeNodes['Host'].Text;
end;

procedure TXMLG2SynthType.Set_Host(Value: string);
begin
  SetAttribute('Host', Value);
end;

function TXMLG2SynthType.Get_Port: Integer;
begin
  Result := AttributeNodes['Port'].NodeValue;
end;

procedure TXMLG2SynthType.Set_Port(Value: Integer);
begin
  SetAttribute('Port', Value);
end;

function TXMLG2SynthType.Get_MidiToKnobs: IXMLMidiToKnobsType;
begin
  Result := ChildNodes['MidiToKnobs'] as IXMLMidiToKnobsType;
end;

{ TXMLMidiToKnobsType }

procedure TXMLMidiToKnobsType.AfterConstruction;
begin
  RegisterChildNode('MidiToKnob', TXMLMidiToKnobType);
  ItemTag := 'MidiToKnob';
  ItemInterface := IXMLMidiToKnobType;
  inherited;
end;

function TXMLMidiToKnobsType.Get_MidiToKnob(Index: Integer): IXMLMidiToKnobType;
begin
  Result := List[Index] as IXMLMidiToKnobType;
end;

function TXMLMidiToKnobsType.Add: IXMLMidiToKnobType;
begin
  Result := AddItem(-1) as IXMLMidiToKnobType;
end;

function TXMLMidiToKnobsType.Insert(const Index: Integer): IXMLMidiToKnobType;
begin
  Result := AddItem(Index) as IXMLMidiToKnobType;
end;

{ TXMLMidiToKnobType }

function TXMLMidiToKnobType.Get_Source: string;
begin
  Result := AttributeNodes['source'].Text;
end;

procedure TXMLMidiToKnobType.Set_Source(Value: string);
begin
  SetAttribute('source', Value);
end;

function TXMLMidiToKnobType.Get_ControlType: Integer;
begin
  Result := AttributeNodes['controlType'].NodeValue;
end;

procedure TXMLMidiToKnobType.Set_ControlType(Value: Integer);
begin
  SetAttribute('controlType', Value);
end;

function TXMLMidiToKnobType.Get_Knob: Integer;
begin
  Result := AttributeNodes['knob'].NodeValue;
end;

procedure TXMLMidiToKnobType.Set_Knob(Value: Integer);
begin
  SetAttribute('knob', Value);
end;

function TXMLMidiToKnobType.Get_MidiCC: Integer;
begin
  Result := AttributeNodes['midiCC'].NodeValue;
end;

procedure TXMLMidiToKnobType.Set_MidiCC(Value: Integer);
begin
  SetAttribute('midiCC', Value);
end;

end.
