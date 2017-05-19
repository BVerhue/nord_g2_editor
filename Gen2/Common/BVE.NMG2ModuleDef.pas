
{********************************************************************************************************************************}
{                                                                                                                                }
{                                                        XML Data Binding                                                        }
{                                                                                                                                }
{         Generated on: 24-9-2013 11:00:23                                                                                       }
{       Generated from: C:\Documents and Settings\BVerhue\Mijn documenten\RAD Studio\Projects\nmg2_editor_FMX\ModuleDef_v3.xdb   }
{   Settings stored in: C:\Documents and Settings\BVerhue\Mijn documenten\RAD Studio\Projects\nmg2_editor_FMX\ModuleDef_v3.xdb   }
{                                                                                                                                }
{********************************************************************************************************************************}

unit BVE.NMG2ModuleDef;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLModuleDefListType = interface;
  IXMLModuleDefType = interface;
  IXMLInputsType = interface;
  IXMLConnectorType = interface;
  IXMLOutputsType = interface;
  IXMLParamsType = interface;
  IXMLParamType = interface;
  IXMLModesType = interface;

{ IXMLModuleDefListType }

  IXMLModuleDefListType = interface(IXMLNodeCollection)
    ['{FBB3D442-B28F-4478-A637-269C48FEFD2D}']
    { Property Accessors }
    function Get_Xmlns: string;
    function Get_Version: string;
    function Get_ModuleDef(Index: Integer): IXMLModuleDefType;
    procedure Set_Xmlns(Value: string);
    procedure Set_Version(Value: string);
    { Methods & Properties }
    function Add: IXMLModuleDefType;
    function Insert(const Index: Integer): IXMLModuleDefType;
    property Xmlns: string read Get_Xmlns write Set_Xmlns;
    property Version: string read Get_Version write Set_Version;
    property ModuleDef[Index: Integer]: IXMLModuleDefType read Get_ModuleDef; default;
  end;

{ IXMLModuleDefType }

  IXMLModuleDefType = interface(IXMLNode)
    ['{F1D06D88-D9BC-4645-8DAF-BA0246AB7DCD}']
    { Property Accessors }
    function Get_ModuleType: Integer;
    function Get_IsLed: Integer;
    function Get_Uprate: Integer;
    function Get_Page: string;
    function Get_PageIndex: Integer;
    function Get_ShortName: string;
    function Get_LongName: string;
    function Get_Height: Integer;
    function Get_Inputs: IXMLInputsType;
    function Get_Outputs: IXMLOutputsType;
    function Get_Params: IXMLParamsType;
    function Get_Modes: IXMLModesType;
    procedure Set_ModuleType(Value: Integer);
    procedure Set_IsLed(Value: Integer);
    procedure Set_Uprate(Value: Integer);
    procedure Set_Page(Value: string);
    procedure Set_PageIndex(Value: Integer);
    procedure Set_ShortName(Value: string);
    procedure Set_LongName(Value: string);
    procedure Set_Height(Value: Integer);
    { Methods & Properties }
    property ModuleType: Integer read Get_ModuleType write Set_ModuleType;
    property IsLed: Integer read Get_IsLed write Set_IsLed;
    property Uprate: Integer read Get_Uprate write Set_Uprate;
    property Page: string read Get_Page write Set_Page;
    property PageIndex: Integer read Get_PageIndex write Set_PageIndex;
    property ShortName: string read Get_ShortName write Set_ShortName;
    property LongName: string read Get_LongName write Set_LongName;
    property Height: Integer read Get_Height write Set_Height;
    property Inputs: IXMLInputsType read Get_Inputs;
    property Outputs: IXMLOutputsType read Get_Outputs;
    property Params: IXMLParamsType read Get_Params;
    property Modes: IXMLModesType read Get_Modes;
  end;

{ IXMLInputsType }

  IXMLInputsType = interface(IXMLNodeCollection)
    ['{65ED7B6B-BBE1-498B-98C5-652B7B80272C}']
    { Property Accessors }
    function Get_Connector(Index: Integer): IXMLConnectorType;
    { Methods & Properties }
    function Add: IXMLConnectorType;
    function Insert(const Index: Integer): IXMLConnectorType;
    property Connector[Index: Integer]: IXMLConnectorType read Get_Connector; default;
  end;

{ IXMLConnectorType }

  IXMLConnectorType = interface(IXMLNode)
    ['{B0D91F5C-D0AE-44A2-A370-CD891E34305A}']
    { Property Accessors }
    function Get_Name: string;
    function Get_Type_: string;
    procedure Set_Name(Value: string);
    procedure Set_Type_(Value: string);
    { Methods & Properties }
    property Name: string read Get_Name write Set_Name;
    property Type_: string read Get_Type_ write Set_Type_;
  end;

{ IXMLOutputsType }

  IXMLOutputsType = interface(IXMLNodeCollection)
    ['{3A6DFA82-22D1-446D-9525-7537063D24C9}']
    { Property Accessors }
    function Get_Connector(Index: Integer): IXMLConnectorType;
    { Methods & Properties }
    function Add: IXMLConnectorType;
    function Insert(const Index: Integer): IXMLConnectorType;
    property Connector[Index: Integer]: IXMLConnectorType read Get_Connector; default;
  end;

{ IXMLParamsType }

  IXMLParamsType = interface(IXMLNodeCollection)
    ['{07F6590F-E717-4A10-B252-24097D3A1A95}']
    { Property Accessors }
    function Get_Param(Index: Integer): IXMLParamType;
    { Methods & Properties }
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
    property Param[Index: Integer]: IXMLParamType read Get_Param; default;
  end;

{ IXMLParamType }

  IXMLParamType = interface(IXMLNode)
    ['{79653135-3557-454A-A9F7-B4E93B294F54}']
    { Property Accessors }
    function Get_Name: string;
    function Get_DefaultValue: Integer;
    function Get_Id: Integer;
    function Get_DefaultKnob: Integer;
    function Get_ButtonParam: Integer;
    function Get_ParamLabel: string;
    procedure Set_Name(Value: string);
    procedure Set_DefaultValue(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_DefaultKnob(Value: Integer);
    procedure Set_ButtonParam(Value: Integer);
    procedure Set_ParamLabel(Value: string);
    { Methods & Properties }
    property Name: string read Get_Name write Set_Name;
    property DefaultValue: Integer read Get_DefaultValue write Set_DefaultValue;
    property Id: Integer read Get_Id write Set_Id;
    property DefaultKnob: Integer read Get_DefaultKnob write Set_DefaultKnob;
    property ButtonParam: Integer read Get_ButtonParam write Set_ButtonParam;
    property ParamLabel: string read Get_ParamLabel write Set_ParamLabel;
  end;

{ IXMLModesType }

  IXMLModesType = interface(IXMLNodeCollection)
    ['{E336C3EB-BCAC-4987-9739-4129E360CA19}']
    { Property Accessors }
    function Get_Param(Index: Integer): IXMLParamType;
    { Methods & Properties }
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
    property Param[Index: Integer]: IXMLParamType read Get_Param; default;
  end;

{ Forward Decls }

  TXMLModuleDefListType = class;
  TXMLModuleDefType = class;
  TXMLInputsType = class;
  TXMLConnectorType = class;
  TXMLOutputsType = class;
  TXMLParamsType = class;
  TXMLParamType = class;
  TXMLModesType = class;

{ TXMLModuleDefListType }

  TXMLModuleDefListType = class(TXMLNodeCollection, IXMLModuleDefListType)
  protected
    { IXMLModuleDefListType }
    function Get_Xmlns: string;
    function Get_Version: string;
    function Get_ModuleDef(Index: Integer): IXMLModuleDefType;
    procedure Set_Xmlns(Value: string);
    procedure Set_Version(Value: string);
    function Add: IXMLModuleDefType;
    function Insert(const Index: Integer): IXMLModuleDefType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLModuleDefType }

  TXMLModuleDefType = class(TXMLNode, IXMLModuleDefType)
  protected
    { IXMLModuleDefType }
    function Get_ModuleType: Integer;
    function Get_IsLed: Integer;
    function Get_Uprate: Integer;
    function Get_Page: string;
    function Get_PageIndex: Integer;
    function Get_ShortName: string;
    function Get_LongName: string;
    function Get_Height: Integer;
    function Get_Inputs: IXMLInputsType;
    function Get_Outputs: IXMLOutputsType;
    function Get_Params: IXMLParamsType;
    function Get_Modes: IXMLModesType;
    procedure Set_ModuleType(Value: Integer);
    procedure Set_IsLed(Value: Integer);
    procedure Set_Uprate(Value: Integer);
    procedure Set_Page(Value: string);
    procedure Set_PageIndex(Value: Integer);
    procedure Set_ShortName(Value: string);
    procedure Set_LongName(Value: string);
    procedure Set_Height(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLInputsType }

  TXMLInputsType = class(TXMLNodeCollection, IXMLInputsType)
  protected
    { IXMLInputsType }
    function Get_Connector(Index: Integer): IXMLConnectorType;
    function Add: IXMLConnectorType;
    function Insert(const Index: Integer): IXMLConnectorType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLConnectorType }

  TXMLConnectorType = class(TXMLNode, IXMLConnectorType)
  protected
    { IXMLConnectorType }
    function Get_Name: string;
    function Get_Type_: string;
    procedure Set_Name(Value: string);
    procedure Set_Type_(Value: string);
  end;

{ TXMLOutputsType }

  TXMLOutputsType = class(TXMLNodeCollection, IXMLOutputsType)
  protected
    { IXMLOutputsType }
    function Get_Connector(Index: Integer): IXMLConnectorType;
    function Add: IXMLConnectorType;
    function Insert(const Index: Integer): IXMLConnectorType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLParamsType }

  TXMLParamsType = class(TXMLNodeCollection, IXMLParamsType)
  protected
    { IXMLParamsType }
    function Get_Param(Index: Integer): IXMLParamType;
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLParamType }

  TXMLParamType = class(TXMLNode, IXMLParamType)
  protected
    { IXMLParamType }
    function Get_Name: string;
    function Get_DefaultValue: Integer;
    function Get_Id: Integer;
    function Get_DefaultKnob: Integer;
    function Get_ButtonParam: Integer;
    function Get_ParamLabel: string;
    procedure Set_Name(Value: string);
    procedure Set_DefaultValue(Value: Integer);
    procedure Set_Id(Value: Integer);
    procedure Set_DefaultKnob(Value: Integer);
    procedure Set_ButtonParam(Value: Integer);
    procedure Set_ParamLabel(Value: string);
  end;

{ TXMLModesType }

  TXMLModesType = class(TXMLNodeCollection, IXMLModesType)
  protected
    { IXMLModesType }
    function Get_Param(Index: Integer): IXMLParamType;
    function Add: IXMLParamType;
    function Insert(const Index: Integer): IXMLParamType;
  public
    procedure AfterConstruction; override;
  end;

{ Global Functions }

function GetModuleDefList(Doc: IXMLDocument): IXMLModuleDefListType;
function LoadModuleDefList(const FileName: string): IXMLModuleDefListType;
function NewModuleDefList: IXMLModuleDefListType;

const
  TargetNamespace = 'http://www.yourtargetnamespace.com';

implementation

{ Global Functions }

function GetModuleDefList(Doc: IXMLDocument): IXMLModuleDefListType;
begin
  Result := Doc.GetDocBinding('ModuleDefList', TXMLModuleDefListType, TargetNamespace) as IXMLModuleDefListType;
end;

function LoadModuleDefList(const FileName: string): IXMLModuleDefListType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ModuleDefList', TXMLModuleDefListType, TargetNamespace) as IXMLModuleDefListType;
end;

function NewModuleDefList: IXMLModuleDefListType;
begin
  Result := NewXMLDocument.GetDocBinding('ModuleDefList', TXMLModuleDefListType, TargetNamespace) as IXMLModuleDefListType;
end;

{ TXMLModuleDefListType }

procedure TXMLModuleDefListType.AfterConstruction;
begin
  RegisterChildNode('ModuleDef', TXMLModuleDefType);
  ItemTag := 'ModuleDef';
  ItemInterface := IXMLModuleDefType;
  inherited;
end;

function TXMLModuleDefListType.Get_Xmlns: string;
begin
  Result := AttributeNodes['xmlns'].Text;
end;

procedure TXMLModuleDefListType.Set_Xmlns(Value: string);
begin
  SetAttribute('xmlns', Value);
end;

function TXMLModuleDefListType.Get_Version: string;
begin
  Result := AttributeNodes['version'].Text;
end;

procedure TXMLModuleDefListType.Set_Version(Value: string);
begin
  SetAttribute('version', Value);
end;

function TXMLModuleDefListType.Get_ModuleDef(Index: Integer): IXMLModuleDefType;
begin
  Result := List[Index] as IXMLModuleDefType;
end;

function TXMLModuleDefListType.Add: IXMLModuleDefType;
begin
  Result := AddItem(-1) as IXMLModuleDefType;
end;

function TXMLModuleDefListType.Insert(const Index: Integer): IXMLModuleDefType;
begin
  Result := AddItem(Index) as IXMLModuleDefType;
end;

{ TXMLModuleDefType }

procedure TXMLModuleDefType.AfterConstruction;
begin
  RegisterChildNode('Inputs', TXMLInputsType);
  RegisterChildNode('Outputs', TXMLOutputsType);
  RegisterChildNode('Params', TXMLParamsType);
  RegisterChildNode('Modes', TXMLModesType);
  inherited;
end;

function TXMLModuleDefType.Get_ModuleType: Integer;
begin
  Result := AttributeNodes['moduleType'].NodeValue;
end;

procedure TXMLModuleDefType.Set_ModuleType(Value: Integer);
begin
  SetAttribute('moduleType', Value);
end;

function TXMLModuleDefType.Get_IsLed: Integer;
begin
  Result := AttributeNodes['isLed'].NodeValue;
end;

procedure TXMLModuleDefType.Set_IsLed(Value: Integer);
begin
  SetAttribute('isLed', Value);
end;

function TXMLModuleDefType.Get_Uprate: Integer;
begin
  Result := AttributeNodes['uprate'].NodeValue;
end;

procedure TXMLModuleDefType.Set_Uprate(Value: Integer);
begin
  SetAttribute('uprate', Value);
end;

function TXMLModuleDefType.Get_Page: string;
begin
  Result := AttributeNodes['page'].Text;
end;

procedure TXMLModuleDefType.Set_Page(Value: string);
begin
  SetAttribute('page', Value);
end;

function TXMLModuleDefType.Get_PageIndex: Integer;
begin
  Result := AttributeNodes['pageIndex'].NodeValue;
end;

procedure TXMLModuleDefType.Set_PageIndex(Value: Integer);
begin
  SetAttribute('pageIndex', Value);
end;

function TXMLModuleDefType.Get_ShortName: string;
begin
  Result := AttributeNodes['shortName'].Text;
end;

procedure TXMLModuleDefType.Set_ShortName(Value: string);
begin
  SetAttribute('shortName', Value);
end;

function TXMLModuleDefType.Get_LongName: string;
begin
  Result := AttributeNodes['longName'].Text;
end;

procedure TXMLModuleDefType.Set_LongName(Value: string);
begin
  SetAttribute('longName', Value);
end;

function TXMLModuleDefType.Get_Height: Integer;
begin
  Result := AttributeNodes['height'].NodeValue;
end;

procedure TXMLModuleDefType.Set_Height(Value: Integer);
begin
  SetAttribute('height', Value);
end;

function TXMLModuleDefType.Get_Inputs: IXMLInputsType;
begin
  Result := ChildNodes['Inputs'] as IXMLInputsType;
end;

function TXMLModuleDefType.Get_Outputs: IXMLOutputsType;
begin
  Result := ChildNodes['Outputs'] as IXMLOutputsType;
end;

function TXMLModuleDefType.Get_Params: IXMLParamsType;
begin
  Result := ChildNodes['Params'] as IXMLParamsType;
end;

function TXMLModuleDefType.Get_Modes: IXMLModesType;
begin
  Result := ChildNodes['Modes'] as IXMLModesType;
end;

{ TXMLInputsType }

procedure TXMLInputsType.AfterConstruction;
begin
  RegisterChildNode('Connector', TXMLConnectorType);
  ItemTag := 'Connector';
  ItemInterface := IXMLConnectorType;
  inherited;
end;

function TXMLInputsType.Get_Connector(Index: Integer): IXMLConnectorType;
begin
  Result := List[Index] as IXMLConnectorType;
end;

function TXMLInputsType.Add: IXMLConnectorType;
begin
  Result := AddItem(-1) as IXMLConnectorType;
end;

function TXMLInputsType.Insert(const Index: Integer): IXMLConnectorType;
begin
  Result := AddItem(Index) as IXMLConnectorType;
end;

{ TXMLConnectorType }

function TXMLConnectorType.Get_Name: string;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLConnectorType.Set_Name(Value: string);
begin
  SetAttribute('name', Value);
end;

function TXMLConnectorType.Get_Type_: string;
begin
  Result := AttributeNodes['type'].Text;
end;

procedure TXMLConnectorType.Set_Type_(Value: string);
begin
  SetAttribute('type', Value);
end;

{ TXMLOutputsType }

procedure TXMLOutputsType.AfterConstruction;
begin
  RegisterChildNode('Connector', TXMLConnectorType);
  ItemTag := 'Connector';
  ItemInterface := IXMLConnectorType;
  inherited;
end;

function TXMLOutputsType.Get_Connector(Index: Integer): IXMLConnectorType;
begin
  Result := List[Index] as IXMLConnectorType;
end;

function TXMLOutputsType.Add: IXMLConnectorType;
begin
  Result := AddItem(-1) as IXMLConnectorType;
end;

function TXMLOutputsType.Insert(const Index: Integer): IXMLConnectorType;
begin
  Result := AddItem(Index) as IXMLConnectorType;
end;

{ TXMLParamsType }

procedure TXMLParamsType.AfterConstruction;
begin
  RegisterChildNode('Param', TXMLParamType);
  ItemTag := 'Param';
  ItemInterface := IXMLParamType;
  inherited;
end;

function TXMLParamsType.Get_Param(Index: Integer): IXMLParamType;
begin
  Result := List[Index] as IXMLParamType;
end;

function TXMLParamsType.Add: IXMLParamType;
begin
  Result := AddItem(-1) as IXMLParamType;
end;

function TXMLParamsType.Insert(const Index: Integer): IXMLParamType;
begin
  Result := AddItem(Index) as IXMLParamType;
end;

{ TXMLParamType }

function TXMLParamType.Get_Name: string;
begin
  Result := AttributeNodes['name'].Text;
end;

procedure TXMLParamType.Set_Name(Value: string);
begin
  SetAttribute('name', Value);
end;

function TXMLParamType.Get_DefaultValue: Integer;
begin
  Result := AttributeNodes['defaultValue'].NodeValue;
end;

procedure TXMLParamType.Set_DefaultValue(Value: Integer);
begin
  SetAttribute('defaultValue', Value);
end;

function TXMLParamType.Get_Id: Integer;
begin
  Result := AttributeNodes['id'].NodeValue;
end;

procedure TXMLParamType.Set_Id(Value: Integer);
begin
  SetAttribute('id', Value);
end;

function TXMLParamType.Get_DefaultKnob: Integer;
begin
  Result := AttributeNodes['defaultKnob'].NodeValue;
end;

procedure TXMLParamType.Set_DefaultKnob(Value: Integer);
begin
  SetAttribute('defaultKnob', Value);
end;

function TXMLParamType.Get_ButtonParam: Integer;
begin
  Result := AttributeNodes['buttonParam'].NodeValue;
end;

procedure TXMLParamType.Set_ButtonParam(Value: Integer);
begin
  SetAttribute('buttonParam', Value);
end;

function TXMLParamType.Get_ParamLabel: string;
begin
  Result := AttributeNodes['paramLabel'].Text;
end;

procedure TXMLParamType.Set_ParamLabel(Value: string);
begin
  SetAttribute('paramLabel', Value);
end;

{ TXMLModesType }

procedure TXMLModesType.AfterConstruction;
begin
  RegisterChildNode('Param', TXMLParamType);
  ItemTag := 'Param';
  ItemInterface := IXMLParamType;
  inherited;
end;

function TXMLModesType.Get_Param(Index: Integer): IXMLParamType;
begin
  Result := List[Index] as IXMLParamType;
end;

function TXMLModesType.Add: IXMLParamType;
begin
  Result := AddItem(-1) as IXMLParamType;
end;

function TXMLModesType.Insert(const Index: Integer): IXMLParamType;
begin
  Result := AddItem(Index) as IXMLParamType;
end;

end.