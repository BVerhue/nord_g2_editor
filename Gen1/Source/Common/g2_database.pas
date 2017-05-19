unit g2_database;

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
{$IFDEF G2_VER220_up}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  DOM, XMLRead, XMLWrite;

type
  TXMLModuleDefListType = class;
  TXMLModuleDefType = class;
  TXMLConnectorListType = class;
  TXMLConnectorType = class;
  TXMLParamListType = class;
  TXMLParamType = class;
  TXMLParamDefListType = class;
  TXMLParamDefType = class;

  TXMLTCPSettingsType = class;

  TXMLModuleDefListType = class(TDOMElementList)
  protected
    function Get_ModuleDef(Index: Integer): TXMLModuleDefType;
    function Get_FileVersion : string;
  public
    constructor Create(ANode: TDOMNode); overload;
    property ModuleDef[Index: Integer]: TXMLModuleDefType read Get_ModuleDef;
    property FileVersion : string read Get_FileVersion;
  end;

  TXMLModuleDefType = class(TDOMElement)
  protected
    function Get_ModuleType: Integer;
    function Get_IsLed: Integer;
    function Get_Uprate: Integer;
    function Get_Page: AnsiString;
    function Get_PageIndex: Integer;
    function Get_ShortName: AnsiString;
    function Get_LongName: AnsiString;
    function Get_Height: Integer;
    function Get_Inputs: TXMLConnectorListType;
    function Get_Outputs: TXMLConnectorListType;
    function Get_Params: TXMLParamListType;
    function Get_Modes: TXMLParamListType;
  public
    property ModuleType: Integer read Get_ModuleType;
    property IsLed: Integer read Get_IsLed;
    property Uprate: Integer read Get_Uprate;
    property Page: AnsiString read Get_Page;
    property PageIndex: Integer read Get_PageIndex;
    property ShortName: AnsiString read Get_ShortName;
    property LongName: AnsiString read Get_LongName;
    property Height: Integer read Get_Height;
    property Inputs: TXMLConnectorListType read Get_Inputs;
    property Outputs: TXMLConnectorListType read Get_Outputs;
    property Params: TXMLParamListType read Get_Params;
    property Modes: TXMLParamListType read Get_Modes;
  end;

  TXMLConnectorListType = class(TDOMElementList)
  protected
    function Get_Connector(Index: Integer): TXMLConnectorType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property Connector[Index: Integer]: TXMLConnectorType read Get_Connector; default;
  end;

  TXMLConnectorType = class(TDOMElement)
    function Get_Name: AnsiString;
    function Get_Type_: AnsiString;
    property Name: AnsiString read Get_Name;
    property Type_: AnsiString read Get_Type_;
  end;

  TXMLParamListType = class(TDOMElementList)
  protected
    function Get_Param(Index: Integer): TXMLParamType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property Param[Index: Integer]: TXMLParamType read Get_Param; default;
  end;

  TXMLParamType = class(TDOMElement)
    function Get_Id: Integer;
    function Get_Name: AnsiString;
    function Get_DefaultValue: Integer;
    function Get_ParamLabel: AnsiString;
    function Get_DefaultKnob : Integer;
    function Get_ButtonParamIndex : Integer;
    property Id: Integer read Get_Id;
    property Name: AnsiString read Get_Name;
    property DefaultValue: Integer read Get_DefaultValue;
    property ParamLabel: AnsiString read Get_ParamLabel;
    property DefaultKnob : Integer read Get_DefaultKnob;
    property ButtonParamIndex : Integer read Get_ButtonParamIndex;
  end;

  TXMLParamDefListType = class(TDOMElementList)
  protected
    function Get_ParamDef(Index: Integer): TXMLParamDefType;
    function Get_FileVersion : string;
  public
    constructor Create(ANode: TDOMNode); overload;
    property ParamDef[Index: Integer]: TXMLParamDefType read Get_ParamDef; default;
    property FileVersion : string read Get_FileVersion;
  end;

  TXMLParamDefType = class(TDOMElement)
  protected
    function Get_Id: Integer;
    function Get_ParamType: Integer;
    function Get_RangeType: AnsiString;
    function Get_LowValue: Integer;
    function Get_HighValue: Integer;
    function Get_DefaultValue: Integer;
    function Get_Definitions: AnsiString;
    function Get_Comments: AnsiString;
    function Get_ButtonText: AnsiString;
  public
    property Id: Integer read Get_Id;
    property ParamType: Integer read Get_ParamType;
    property RangeType: AnsiString read Get_RangeType;
    property LowValue: Integer read Get_LowValue;
    property HighValue: Integer read Get_HighValue;
    property DefaultValue: Integer read Get_DefaultValue;
    property Definitions: AnsiString read Get_Definitions;
    property Comments: AnsiString read Get_Comments;
    property ButtonText: AnsiString read Get_ButtonText;
  end;

  TXMLTCPSettingsListType = class(TDOMElementList)
  protected
    function Get_TCPSettings(Index: Integer): TXMLTCPSettingsType;
   public
     constructor Create(ANode: TDOMNode); overload;
     property TCPSettings[Index: Integer]: TXMLTCPSettingsType read Get_TCPSettings;
  end;

  TXMLTCPSettingsType = class(TDOMElement)
  protected
    function Get_IsServer : boolean;
    procedure Set_IsServer( aValue : boolean);
    function Get_IP: string;
    function Get_Port: integer;
    procedure Set_IP( aValue : string);
    procedure Set_Port( aValue : integer);
    function Get_TimerBroadcastLedMessages : integer;
    procedure Set_TimerBroadcastLedMessages( aValue : integer);
  public
    property IsServer: boolean read Get_IsServer write Set_IsServer;
    property IP: string read Get_IP write Set_IP;
    property Port: integer read Get_Port write Set_Port;
    property TimerBroadcastLedMessages : integer read Get_TimerBroadcastLedMessages write Set_TimerBroadcastLedMessages;
  end;

  TXMLVSTTCPSettingsType = class(TDOMElement)
  protected
    function Get_IP: string;
    function Get_Port: integer;
    procedure Set_IP( aValue : string);
    procedure Set_Port( aValue : integer);
  public
    property IP: string read Get_IP write Set_IP;
    property Port: integer read Get_Port write Set_Port;
  end;

  TXMLFormSettingsType = class(TDOMElement)
  protected
    function Get_PosX: integer;
    function Get_PosY: integer;
    function Get_SizeX: integer;
    function Get_SizeY: integer;
    function Get_Visible : boolean;
    procedure Set_PosX( aValue : integer);
    procedure Set_PosY( aValue : integer);
    procedure Set_SizeX( aValue : integer);
    procedure Set_SizeY( aValue : integer);
    procedure Set_Visible( aValue : boolean);
  public
    property PosX: integer read Get_PosX write Set_PosX;
    property PosY: integer read Get_PosY write Set_PosY;
    property SizeX: integer read Get_SizeX write Set_SizeX;
    property SizeY: integer read Get_SizeY write Set_SizeY;
    property Visible: boolean read Get_Visible write Set_Visible;
  end;

  TXMLPatchBrowserSettingsType = class(TDOMElement)
  protected
    function Get_BaseFolder: AnsiString;
    procedure Set_BaseFolder( aValue : AnsiString);
    function Get_ExternalSortCol: integer;
    procedure Set_ExternalSortCol( aValue : integer);
    function Get_InternalSortCol: integer;
    procedure Set_InternalSortCol( aValue : integer);
    function Get_SelectedTab: integer;
    procedure Set_SelectedTab( aValue : integer);
  public
    property BaseFolder: AnsiString read Get_BaseFolder write Set_BaseFolder;
    property ExternalSortCol : integer read Get_ExternalSortCol write Set_ExternalSortCol;
    property InternalSortCol : integer read Get_InternalSortCol write Set_InternalSortCol;
    property SelectedTab : integer read Get_SelectedTab write Set_SelectedTab;
  end;

  TXMLPatchBufferSettingsType = class(TDOMElement)
  protected
    function Get_Folder: AnsiString;
    procedure Set_Folder( aValue : AnsiString);
  public
    property Folder: AnsiString read Get_Folder write Set_Folder;
  end;

  TXMLDirectorySettingsType = class(TDOMElement)
  protected
    function Get_G2oolsFolder: AnsiString;
    procedure Set_G2oolsFolder( aValue : AnsiString);
    function Get_ModuleHelpFile: AnsiString;
    procedure Set_ModuleHelpFile( aValue : AnsiString);
  public
    property G2oolsFolder: AnsiString read Get_G2oolsFolder write Set_G2oolsFolder;
    property ModuleHelpFile : AnsiString read Get_ModuleHelpFile write Set_ModuleHelpFile;
  end;

  TXMLMidiDeviceType = class(TDOMElement)
  protected
    function Get_MidiEnabled: boolean;
    procedure Set_MidiEnabled( aValue : boolean);
    function Get_MidiInDevice: string;
    procedure Set_MidiInDevice( aValue : string);
    function Get_MidiOutDevice: string;
    procedure Set_MidiOutDevice( aValue : string);
  public
    property MidiEnabled: boolean read Get_MidiEnabled write Set_MidiEnabled;
    property MidiInDevice : string read Get_MidiInDevice write Set_MidiInDevice;
    property MidiOutDevice : string read Get_MidiOutDevice write Set_MidiOutDevice;
  end;

  TXMLCtrlMidiDeviceType = class(TDOMElement)
  protected
    function Get_CtrlMidiEnabled: boolean;
    procedure Set_CtrlMidiEnabled( aValue : boolean);
    function Get_CtrlMidiDevice: string;
    procedure Set_CtrlMidiDevice( aValue : string);
  public
    property CtrlMidiEnabled: boolean read Get_CtrlMidiEnabled write Set_CtrlMidiEnabled;
    property CtrlMidiDevice : string read Get_CtrlMidiDevice write Set_CtrlMidiDevice;
  end;

  TXMLCtrlMidiDeviceListType = class(TDOMElementList)
  protected
    function Get_CtrlMidiDevice(Index: Integer): TXMLCtrlMidiDeviceType;
  public
    constructor Create(ANode: TDOMNode); overload;
    function GetMidiDeviceNodeByName( aName : string): TXMLCtrlMidiDeviceType;
    property CtrlMidiDevice[Index: Integer]: TXMLCtrlMidiDeviceType read Get_CtrlMidiDevice; default;
  end;

  TXMLCtrlMidiAssignmentType = class(TDOMElement)
  protected
    function Get_ID: integer;
    procedure Set_ID( aValue : integer);
    function Get_Channel: byte;
    procedure Set_Channel( aValue : byte);
    function Get_Note: byte;
    procedure Set_Note( aValue : byte);
    function Get_ControlIndex: byte;
    procedure Set_ControlIndex( aValue : byte);
    function Get_CC: byte;
    procedure Set_CC( aValue : byte);
    function Get_MinValue: byte;
    procedure Set_MinValue( aValue : byte);
    function Get_MaxValue: byte;
    procedure Set_MaxValue( aValue : byte);
    function Get_ControlPath: string;
    procedure Set_ControlPath( aValue : string);
  public
    property ID: integer read Get_ID write Set_ID;
    property Channel: byte read Get_Channel write Set_Channel;
    property Note: byte read Get_Note write Set_Note;
    property CC: byte read Get_CC write Set_CC;
    property MinValue: byte read Get_MinValue write Set_MinValue;
    property MaxValue: byte read Get_MaxValue write Set_MaxValue;
    property ControlPath : string read Get_ControlPath write Set_ControlPath;
    property ControlIndex: byte read Get_ControlIndex write Set_ControlIndex;
  end;

   TXMLCtrlMidiassignmentListType = class(TDOMElementList)
  protected
    function Get_EditorCtrlMidiassignment(Index: Integer): TXMLCtrlMidiAssignmentType;
  public
    constructor Create(ANode: TDOMNode); overload;
    property EditorCtrlMidiassignmente[Index: Integer]: TXMLCtrlMidiAssignmentType
                            read Get_EditorCtrlMidiassignment; default;
  end;

  TXMLEditorSettingsType = class(TDOMElement)
  protected
    function Get_LogEnabled: boolean;
    procedure Set_LogEnabled( aValue : boolean);
    function Get_CableThickness: integer;
    procedure Set_CableThickness( aValue : integer);
    function Get_SlotStripColor: integer;
    procedure Set_SlotStripColor( aValue : integer);
    function Get_SlotStripInverseColor: integer;
    procedure Set_SlotStripInverseColor( aValue : integer);
    function Get_SlotStripDisabledColor: integer;
    procedure Set_SlotStripDisabledColor( aValue : integer);
    function Get_HighlightColor: integer;
    procedure Set_HighlightColor( aValue : integer);
    function Get_LedColor: integer;
    procedure Set_LedColor( aValue : integer);
    function Get_OnlyTextMenus: boolean;
    procedure Set_OnlyTextMenus( aValue : boolean);
  public
    property LogEnabled: boolean read Get_LogEnabled write Set_LogEnabled;
    property CableThickness : integer read Get_CableThickness write Set_CableThickness;
    property SlotStripColor : integer read Get_SlotStripColor write Set_SlotStripColor;
    property SlotStripInverseColor : integer read Get_SlotStripInverseColor write Set_SlotStripInverseColor;
    property SlotStripDisabledColor : integer read Get_SlotStripDisabledColor write Set_SlotStripDisabledColor;
    property HighlightColor : integer read Get_HighlightColor write Set_HighlightColor;
    property LedColor : integer read Get_LedColor write Set_LedColor;
    property OnlyTextMenus : boolean read Get_OnlyTextMenus write Set_OnlyTextMenus;
  end;

implementation
uses
  g2_types;

function GetInt( aValue : string): integer;
var Code : integer;
begin
  val( aValue, Result, Code);
  if Code <> 0 then
    Result := 0;
end;

function GetByte( aValue : string): byte;
var Code : integer;
begin
  val( aValue, Result, Code);
  if Code <> 0 then
    Result := 0;
end;

{ TXMLModuleDefListType }

constructor TXMLModuleDefListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'ModuleDef');
end;

function TXMLModuleDefListType.Get_ModuleDef(Index: Integer): TXMLModuleDefType;
begin
  Result := TXMLModuleDefType(Item[Index]);
end;

function TXMLModuleDefListType.Get_FileVersion: string;
var n : TDOMNode;
begin
  n := FNode.Attributes.GetNamedItem('version');
  if n <> nil then
    Result := n.NodeValue
  else
    Result := '';
end;

{ TXMLModuleDefType }

function TXMLModuleDefType.Get_ModuleType: Integer;
begin
  Result := GetInt(FindNode('ModuleType').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_IsLed: Integer;
begin
  Result := GetInt(FindNode('IsLed').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Uprate: Integer;
begin
  Result := GetInt(FindNode('Uprate').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Page: AnsiString;
begin
  Result := AnsiString(FindNode('Page').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_PageIndex: Integer;
begin
  Result := GetInt(FindNode('PageIndex').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_ShortName: AnsiString;
begin
  Result := AnsiString(FindNode('ShortName').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_LongName: AnsiString;
begin
  Result := AnsiString(FindNode('LongName').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Height: Integer;
begin
  Result := GetInt(FindNode('Height').FirstChild.NodeValue);
end;

function TXMLModuleDefType.Get_Inputs: TXMLConnectorListType;
var n : TDOMNode;
begin
  n := FindNode('Inputs');
  if assigned(n) then
    Result := TXMLConnectorListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Outputs: TXMLConnectorListType;
var n : TDOMNode;
begin
  n := FindNode('Outputs');
  if assigned(n) then
    Result := TXMLConnectorListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Params: TXMLParamListType;
var n : TDOMNode;
begin
  n := FindNode('Params');
  if assigned(n) then
    Result := TXMLParamListType.Create( n)
  else
    Result := nil;
end;

function TXMLModuleDefType.Get_Modes: TXMLParamListType;
var n : TDOMNode;
begin
  n := FindNode('Modes');
  if assigned(n) then
    Result := TXMLParamListType.Create( n)
  else
    Result := nil;
end;

{ TXMLConnectorListType }

constructor TXMLConnectorListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'Connector');
end;

function TXMLConnectorListType.Get_Connector(Index: Integer): TXMLConnectorType;
begin
  Result := TXMLConnectorType(Item[Index]);
end;

{ TXMLConnectorType }

function TXMLConnectorType.Get_Name: AnsiString;
begin
  Result := AnsiString(FindNode('Name').FirstChild.NodeValue);
end;

function TXMLConnectorType.Get_Type_: AnsiString;
begin
  Result := AnsiString(FindNode('Type').FirstChild.NodeValue);
end;

{ TXMLParamListType }

constructor TXMLParamListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'Param');
end;

function TXMLParamListType.Get_Param(Index: Integer): TXMLParamType;
begin
  Result := TXMLParamType(Item[Index]);
end;

{ TXMLParamType }

function TXMLParamType.Get_DefaultKnob: Integer;
var Node : TDOMNode;
begin
  Node := FindNode('DefaultKnob');
  if Node <> nil then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamType.Get_ButtonParamIndex: Integer;
var Node : TDOMNode;
begin
  Node := FindNode('ButtonParam');
  if Node <> nil then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamType.Get_DefaultValue: Integer;
begin
  Result := GetInt(FindNode('DefaultValue').FirstChild.NodeValue);
end;

function TXMLParamType.Get_Id: Integer;
begin
  Result := GetInt(FindNode('Id').FirstChild.NodeValue);
end;

function TXMLParamType.Get_Name: AnsiString;
begin
  Result := AnsiString(FindNode('Name').FirstChild.NodeValue);
end;

function TXMLParamType.Get_ParamLabel: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('ParamLabel');
  if assigned(Node) then
    Result := AnsiString(Node.FirstChild.NodeValue)
  else
    Result := '';
end;

{procedure TXMLParamType.Set_DefaultKnob(aValue: integer);
var Node : TDOMElement;
begin
  Node := TDOMElement(FindNode('DefaultKnob'));
  if assigned(Node) then begin
    Node.TextContent := IntToStr(aValue);
  end else begin
    Node := TDOMDocument(OwnerDocument).CreateElement('DefaultKnob');
    AppendChild( Node);
    Node.TextContent := IntToStr(aValue);
  end;
end;}

{ TXMLParamDefListType }

constructor TXMLParamDefListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'ParamDef');
end;

function TXMLParamDefListType.Get_ParamDef(Index: Integer): TXMLParamDefType;
begin
  Result := TXMLParamDefType(Item[Index]);
end;

function TXMLParamDefListType.Get_FileVersion: string;
var n : TDOMNode;
begin
  n := FNode.Attributes.GetNamedItem('version');
  if n <> nil then
    Result := n.NodeValue
  else
    Result := '';
end;

{ TXMLParamDefType }

function TXMLParamDefType.Get_ButtonText: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('ButtonText');
  if assigned(Node) then
    Result := AnsiString(Node.TextContent)
  else
    Result := '';
end;

function TXMLParamDefType.Get_Comments: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('Comments');
  if assigned(Node) then
    Result := AnsiString(Node.TextContent)
  else
    Result := '';
end;

function TXMLParamDefType.Get_DefaultValue: Integer;
var Node : TDomNode;
begin
  Node := FindNode('DefaultValue');
  if assigned(Node) and (Node.TextContent <> '') then
    Result := GetInt(Node.TextContent)
  else
    Result := 0;
end;

function TXMLParamDefType.Get_Definitions: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('Definitions');
  if assigned(Node) then
    Result := AnsiString(Node.TextContent)
  else
    Result := '';
end;

function TXMLParamDefType.Get_HighValue: Integer;
var Node : TDomNode;
begin
  Node := FindNode('HighValue');
  if assigned(Node) and (Node.TextContent <> '') then
    Result := GetInt(Node.TextContent)
  else
    Result := 0;
end;

function TXMLParamDefType.Get_Id: Integer;
var Node : TDomNode;
begin
  Node := FindNode('Id');
  if assigned(Node) and (Node.TextContent <> '') then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamDefType.Get_LowValue: Integer;
var Node : TDomNode;
begin
  Node := FindNode('LowValue');
  if assigned(Node) and (Node.TextContent <> '') then
    Result := GetInt(Node.TextContent)
  else
    Result := 0;
end;

function TXMLParamDefType.Get_ParamType: Integer;
var Node : TDomNode;
begin
  Node := FindNode('ParamType');
  if assigned(Node) and (Node.TextContent <> '') then
    Result := GetInt(Node.TextContent)
  else
    Result := -1;
end;

function TXMLParamDefType.Get_RangeType: AnsiString;
var Node : TDomNode;
begin
  Node := FindNode('RangeType');
  if assigned(Node) then
    Result := AnsiString(Node.TextContent)
  else
    Result := '';
end;

{ TXMLTCPSettings }

function TXMLTCPSettingsType.Get_IP: string;
begin
  if GetAttribute('IP') = '' then
    Result := '127.0.0.1'
  else
    Result := GetAttribute('IP');
end;

function TXMLTCPSettingsType.Get_IsServer: boolean;
begin
  if GetAttribute('IsServer') = '' then
    Result := True
  else
    Result := StrToBool(GetAttribute('IsServer'));
end;

function TXMLTCPSettingsType.Get_Port: integer;
begin
  if GetAttribute('Port') = '' then
    Result := 2501
  else
    Result := GetInt(GetAttribute('Port'));
end;

function TXMLTCPSettingsType.Get_TimerBroadcastLedMessages: integer;
begin
  if GetAttribute('TimerBroadcastLedMessages') = '' then
    Result := 500
  else
    Result := GetInt(GetAttribute('TimerBroadcastLedMessages'));
end;

procedure TXMLTCPSettingsType.Set_IP(aValue: string);
begin
  SetAttribute('IP', aValue);
end;

procedure TXMLTCPSettingsType.Set_IsServer(aValue: boolean);
begin
  SetAttribute('IsServer', BoolToStr(aValue));
end;

procedure TXMLTCPSettingsType.Set_Port(aValue: integer);
begin
  SetAttribute('Port', IntToStr(aValue));
end;

procedure TXMLTCPSettingsType.Set_TimerBroadcastLedMessages(aValue: integer);
begin
  SetAttribute('TimerBroadcastLedMessages', IntToStr(aValue));
end;

{ TXMLVSTTCPSettingsType }

function TXMLVSTTCPSettingsType.Get_IP: string;
begin
  if GetAttribute('IP') = '' then
    Result := '127.0.0.1'
  else
    Result := GetAttribute('IP');
end;

function TXMLVSTTCPSettingsType.Get_Port: integer;
begin
  if GetAttribute('Port') = '' then
    Result := 2501
  else
    Result := GetInt(GetAttribute('Port'));
end;

procedure TXMLVSTTCPSettingsType.Set_IP( aValue : string);
begin
  SetAttribute('IP', aValue);
end;

procedure TXMLVSTTCPSettingsType.Set_Port( aValue : integer);
begin
  SetAttribute('Port', IntToStr(aValue));
end;

{ TXMLTCPSettingsListType }

constructor TXMLTCPSettingsListType.Create(ANode: TDOMNode);
begin
  inherited Create( aNode,'TCP_Settings');
end;

function TXMLTCPSettingsListType.Get_TCPSettings( Index: Integer): TXMLTCPSettingsType;
begin
  Result := TXMLTCPSettingsType(Item[Index]);
end;

{ TXMLFormSettingsType }

function TXMLFormSettingsType.Get_PosX: integer;
begin
  Result := GetInt(GetAttribute('PosX'));
end;

function TXMLFormSettingsType.Get_PosY: integer;
begin
  Result := GetInt(GetAttribute('PosY'));
end;

function TXMLFormSettingsType.Get_SizeX: integer;
begin
  Result := GetInt(GetAttribute('SizeX'));
end;

function TXMLFormSettingsType.Get_SizeY: integer;
begin
  Result := GetInt(GetAttribute('SizeY'));
end;

function TXMLFormSettingsType.Get_Visible: boolean;
begin
  Result := StrToBool(GetAttribute('Visible'));
end;

procedure TXMLFormSettingsType.Set_PosX(aValue: integer);
begin
  SetAttribute('PosX', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_PosY(aValue: integer);
begin
  SetAttribute('PosY', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_SizeX(aValue: integer);
begin
  SetAttribute('SizeX', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_SizeY(aValue: integer);
begin
  SetAttribute('SizeY', IntToStr(aValue));
end;

procedure TXMLFormSettingsType.Set_Visible(aValue: boolean);
begin
  SetAttribute('Visible', BoolToStr(aValue));
end;

{ TXMLDirectorySettingsType }

function TXMLDirectorySettingsType.Get_G2oolsFolder: AnsiString;
begin
  Result := GetAttribute('G2oolsFolder');
end;

procedure TXMLDirectorySettingsType.Set_G2oolsFolder( aValue : AnsiString);
begin
  SetAttribute('G2oolsFolder', aValue);
end;

function TXMLDirectorySettingsType.Get_ModuleHelpFile: AnsiString;
begin
  Result := GetAttribute('ModuleHelpFile');
end;

procedure TXMLDirectorySettingsType.Set_ModuleHelpFile( aValue : AnsiString);
begin
  SetAttribute('ModuleHelpFile', aValue);
end;

{ TXMLPatchManagerSettingsType }

function TXMLPatchBrowserSettingsType.Get_BaseFolder: AnsiString;
begin
  Result := GetAttribute('BaseFolder');
end;

function TXMLPatchBrowserSettingsType.Get_ExternalSortCol: integer;
begin
  Result := GetInt(GetAttribute('ExternalSortCol'));
end;

function TXMLPatchBrowserSettingsType.Get_InternalSortCol: integer;
begin
  Result := GetInt(GetAttribute('InternalSortCol'));
end;

function TXMLPatchBrowserSettingsType.Get_SelectedTab: integer;
begin
  Result := GetInt(GetAttribute('SelectedTab'));
end;

procedure TXMLPatchBrowserSettingsType.Set_BaseFolder(aValue: AnsiString);
begin
  SetAttribute('BaseFolder', aValue);
end;

procedure TXMLPatchBrowserSettingsType.Set_ExternalSortCol(aValue: integer);
begin
  SetAttribute('ExternalSortCol', IntToStr(aValue));
end;

procedure TXMLPatchBrowserSettingsType.Set_InternalSortCol(aValue: integer);
begin
  SetAttribute('InternalSortCol', IntToStr(aValue));
end;

procedure TXMLPatchBrowserSettingsType.Set_SelectedTab(aValue: integer);
begin
  SetAttribute('SelectedTab', IntToStr(aValue));
end;

{ TXMLMidiSettingsType }

function TXMLMidiDeviceType.Get_MidiEnabled: boolean;
begin
  if GetAttribute('MidiEnabled') = '' then
    Result := False
  else
    Result := StrToBool(GetAttribute('MidiEnabled'));
end;

function TXMLMidiDeviceType.Get_MidiInDevice: string;
begin
  Result := GetAttribute('MidiInDevice');
end;

function TXMLMidiDeviceType.Get_MidiOutDevice: string;
begin
  Result := GetAttribute('MidiOutDevice');
end;

procedure TXMLMidiDeviceType.Set_MidiEnabled(aValue: boolean);
begin
  SetAttribute('MidiEnabled', BoolToStr(aValue));
end;

procedure TXMLMidiDeviceType.Set_MidiInDevice(aValue: string);
begin
  SetAttribute('MidiInDevice', aValue);
end;

procedure TXMLMidiDeviceType.Set_MidiOutDevice(aValue: string);
begin
  SetAttribute('MidiOutDevice', aValue);
end;

{ TXMLMidiDeviceListType }

constructor TXMLCtrlMidiDeviceListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode, 'CtrlMidiDevice');
end;

function TXMLCtrlMidiDeviceListType.GetMidiDeviceNodeByName( aName: string): TXMLCtrlMidiDeviceType;
var i : integer;
begin
  i := 0;
  while (i<Count) and (aName <> CtrlMidiDevice[i].CtrlMidiDevice) do
    inc(i);

  if (i<Count) then
    Result := CtrlMidiDevice[i]
  else
    Result := nil;
end;

function TXMLCtrlMidiDeviceListType.Get_CtrlMidiDevice( Index: Integer): TXMLCtrlMidiDeviceType;
begin
  Result := TXMLCtrlMidiDeviceType(Item[Index]);
end;

{ TXMLCtrlMidiDeviceType }

function TXMLCtrlMidiDeviceType.Get_CtrlMidiEnabled: boolean;
begin
  if GetAttribute('CtrlMidiEnabled') = '' then
    Result := False
  else
    Result := StrToBool(GetAttribute('CtrlMidiEnabled'));
end;

procedure TXMLCtrlMidiDeviceType.Set_CtrlMidiEnabled( aValue : boolean);
begin
  SetAttribute('CtrlMidiEnabled', BoolToStr(aValue));
end;

function TXMLCtrlMidiDeviceType.Get_CtrlMidiDevice: string;
begin
  Result := GetAttribute('CtrlMidiDevice');
end;

procedure TXMLCtrlMidiDeviceType.Set_CtrlMidiDevice( aValue : string);
begin
  SetAttribute('CtrlMidiDevice', aValue);
end;

{ TXMLEditorCtrlMidiassignmentType }

function TXMLCtrlMidiassignmentType.Get_ID: integer;
begin
  Result := GetInt(GetAttribute('Channel'));
end;

procedure TXMLCtrlMidiassignmentType.Set_ID( aValue : integer);
begin
  SetAttribute('Channel', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_Channel: byte;
begin
  Result := GetByte(GetAttribute('Channel'));
end;

procedure TXMLCtrlMidiassignmentType.Set_Channel( aValue : byte);
begin
  SetAttribute('Channel', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_Note: byte;
begin
  Result := GetByte(GetAttribute('Note'));
end;

procedure TXMLCtrlMidiassignmentType.Set_Note( aValue : byte);
begin
  SetAttribute('Note', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_ControlIndex: byte;
begin
  Result := GetByte(GetAttribute('ControlIndex'));
end;

procedure TXMLCtrlMidiassignmentType.Set_ControlIndex( aValue : byte);
begin
  SetAttribute('ControlIndex', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_CC: byte;
begin
  Result := GetByte(GetAttribute('CC'));
end;

procedure TXMLCtrlMidiassignmentType.Set_CC( aValue : byte);
begin
  SetAttribute('CC', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_MinValue: byte;
begin
  Result := GetByte(GetAttribute('MinValue'));
end;

procedure TXMLCtrlMidiassignmentType.Set_MinValue( aValue : byte);
begin
  SetAttribute('MinValue', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_MaxValue: byte;
begin
  Result := GetByte(GetAttribute('MaxValue'));
end;

procedure TXMLCtrlMidiassignmentType.Set_MaxValue( aValue : byte);
begin
  SetAttribute('MaxValue', IntToStr(aValue));
end;

function TXMLCtrlMidiassignmentType.Get_ControlPath: string;
begin
  Result := GetAttribute('ControlPath');
end;

procedure TXMLCtrlMidiassignmentType.Set_ControlPath( aValue : string);
begin
  SetAttribute('ControlPath', aValue);
end;

{ TXMLEditorCtrlMidiassignmentListType }

constructor TXMLCtrlMidiassignmentListType.Create(ANode: TDOMNode);
begin
  inherited Create(aNode,'CTRL_MIDI_ASSIGNMENT');
end;

function TXMLCtrlMidiassignmentListType.Get_EditorCtrlMidiassignment(
  Index: Integer): TXMLCtrlMidiAssignmentType;
begin
  Result := TXMLCtrlMidiAssignmentType(Item[Index]);
end;

{ TXMLEditorSettingsType}

function TXMLEditorSettingsType.Get_LogEnabled: boolean;
begin
  if GetAttribute('LogEnabled') = '' then
    Result := False
  else
    Result := StrToBool(GetAttribute('LogEnabled'));
end;

procedure TXMLEditorSettingsType.Set_LogEnabled( aValue : boolean);
begin
  SetAttribute('LogEnabled', BoolToStr(aValue));
end;

function TXMLEditorSettingsType.Get_CableThickness: integer;
begin
  if GetAttribute('CableThickness') = '' then
    Result := 2
  else
    Result := GetInt(GetAttribute('CableThickness'));
end;

procedure TXMLEditorSettingsType.Set_CableThickness( aValue : integer);
begin
  SetAttribute('CableThickness', IntToStr(aValue));
end;

function TXMLEditorSettingsType.Get_SlotStripColor: integer;
begin
  if GetAttribute('SlotStripColor') = '' then
    Result := XCL_CLAVIA_RED
  else
    Result := GetInt(GetAttribute('SlotStripColor'));
end;

procedure TXMLEditorSettingsType.Set_SlotStripColor( aValue : integer);
begin
  SetAttribute('SlotStripColor', IntToStr(aValue));
end;

function TXMLEditorSettingsType.Get_SlotStripInverseColor: integer;
begin
  if GetAttribute('SlotStripInverseColor') = '' then
    Result := XCL_CLAVIA_BLUE
  else
    Result := GetInt(GetAttribute('SlotStripInverseColor'));
end;

procedure TXMLEditorSettingsType.Set_SlotStripInverseColor( aValue : integer);
begin
  SetAttribute('SlotStripInverseColor', IntToStr(aValue));
end;

function TXMLEditorSettingsType.Get_SlotStripDisabledColor: integer;
begin
  if GetAttribute('SlotStripDisabledColor') = '' then
    Result := CL_BTN_FACE
  else
    Result := GetInt(GetAttribute('SlotStripDisabledColor'));
end;

procedure TXMLEditorSettingsType.Set_SlotStripDisabledColor( aValue : integer);
begin
  SetAttribute('SlotStripDisabledColor', IntToStr(aValue));
end;

function TXMLEditorSettingsType.Get_HighlightColor: integer;
begin
  if GetAttribute('HighlightColor') = '' then
    Result := XCL_CONTROL_HIGHLIGHT
  else
    Result := GetInt(GetAttribute('HighlightColor'));
end;

procedure TXMLEditorSettingsType.Set_HighlightColor( aValue : integer);
begin
  SetAttribute('HighlightColor', IntToStr(aValue));
end;

function TXMLEditorSettingsType.Get_LedColor: integer;
begin
  if GetAttribute('LedColor') = '' then
    Result := XCL_LED
  else
    Result := GetInt(GetAttribute('LedColor'));
end;

procedure TXMLEditorSettingsType.Set_LedColor( aValue : integer);
begin
  SetAttribute('LedColor', IntToStr(aValue));
end;


function TXMLEditorSettingsType.Get_OnlyTextMenus: boolean;
begin
  if GetAttribute('OnlyTextMenus') = '' then
    Result := False
  else
    Result := StrToBool(GetAttribute('OnlyTextMenus'));
end;

procedure TXMLEditorSettingsType.Set_OnlyTextMenus( aValue : boolean);
begin
  SetAttribute('OnlyTextMenus', BoolToStr(aValue));
end;

{ TXMLPatchBufferSettingsType }

function TXMLPatchBufferSettingsType.Get_Folder: AnsiString;
begin
  Result := GetAttribute('Folder');
end;

procedure TXMLPatchBufferSettingsType.Set_Folder(aValue: AnsiString);
begin
  SetAttribute('Folder', aValue);
end;

end.
