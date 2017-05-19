
{*******************************************************************************************************************************}
{                                                                                                                               }
{                                                       XML Data Binding                                                        }
{                                                                                                                               }
{         Generated on: 24-9-2013 11:01:25                                                                                      }
{       Generated from: C:\Documents and Settings\BVerhue\Mijn documenten\RAD Studio\Projects\nmg2_editor_FMX\ParamDef_v3.xdb   }
{   Settings stored in: C:\Documents and Settings\BVerhue\Mijn documenten\RAD Studio\Projects\nmg2_editor_FMX\ParamDef_v3.xdb   }
{                                                                                                                               }
{*******************************************************************************************************************************}

unit BVE.NMG2ParamDef;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLParamDefListType = interface;
  IXMLParamDefType = interface;

{ IXMLParamDefListType }

  IXMLParamDefListType = interface(IXMLNodeCollection)
    ['{20D3A854-3C87-49F9-8965-0FBFEE3FBEF5}']
    { Property Accessors }
    function Get_Xmlns: Integer;
    function Get_Version: Integer;
    function Get_ParamDef(Index: Integer): IXMLParamDefType;
    procedure Set_Xmlns(Value: Integer);
    procedure Set_Version(Value: Integer);
    { Methods & Properties }
    function Add: IXMLParamDefType;
    function Insert(const Index: Integer): IXMLParamDefType;
    property Xmlns: Integer read Get_Xmlns write Set_Xmlns;
    property Version: Integer read Get_Version write Set_Version;
    property ParamDef[Index: Integer]: IXMLParamDefType read Get_ParamDef; default;
  end;

{ IXMLParamDefType }

  IXMLParamDefType = interface(IXMLNode)
    ['{A1F54104-FAD5-45A3-B890-C41DC2A331C5}']
    { Property Accessors }
    function Get_Id: Integer;
    function Get_ParamType: Integer;
    function Get_RangeType: string;
    function Get_LowValue: Integer;
    function Get_HighValue: Integer;
    function Get_DefaultValue: Integer;
    function Get_Definitions: string;
    function Get_Comments: string;
    function Get_ButtonText: string;
    procedure Set_Id(Value: Integer);
    procedure Set_ParamType(Value: Integer);
    procedure Set_RangeType(Value: string);
    procedure Set_LowValue(Value: Integer);
    procedure Set_HighValue(Value: Integer);
    procedure Set_DefaultValue(Value: Integer);
    procedure Set_Definitions(Value: string);
    procedure Set_Comments(Value: string);
    procedure Set_ButtonText(Value: string);
    { Methods & Properties }
    property Id: Integer read Get_Id write Set_Id;
    property ParamType: Integer read Get_ParamType write Set_ParamType;
    property RangeType: string read Get_RangeType write Set_RangeType;
    property LowValue: Integer read Get_LowValue write Set_LowValue;
    property HighValue: Integer read Get_HighValue write Set_HighValue;
    property DefaultValue: Integer read Get_DefaultValue write Set_DefaultValue;
    property Definitions: string read Get_Definitions write Set_Definitions;
    property Comments: string read Get_Comments write Set_Comments;
    property ButtonText: string read Get_ButtonText write Set_ButtonText;
  end;

{ Forward Decls }

  TXMLParamDefListType = class;
  TXMLParamDefType = class;

{ TXMLParamDefListType }

  TXMLParamDefListType = class(TXMLNodeCollection, IXMLParamDefListType)
  protected
    { IXMLParamDefListType }
    function Get_Xmlns: Integer;
    function Get_Version: Integer;
    function Get_ParamDef(Index: Integer): IXMLParamDefType;
    procedure Set_Xmlns(Value: Integer);
    procedure Set_Version(Value: Integer);
    function Add: IXMLParamDefType;
    function Insert(const Index: Integer): IXMLParamDefType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLParamDefType }

  TXMLParamDefType = class(TXMLNode, IXMLParamDefType)
  protected
    { IXMLParamDefType }
    function Get_Id: Integer;
    function Get_ParamType: Integer;
    function Get_RangeType: string;
    function Get_LowValue: Integer;
    function Get_HighValue: Integer;
    function Get_DefaultValue: Integer;
    function Get_Definitions: string;
    function Get_Comments: string;
    function Get_ButtonText: string;
    procedure Set_Id(Value: Integer);
    procedure Set_ParamType(Value: Integer);
    procedure Set_RangeType(Value: string);
    procedure Set_LowValue(Value: Integer);
    procedure Set_HighValue(Value: Integer);
    procedure Set_DefaultValue(Value: Integer);
    procedure Set_Definitions(Value: string);
    procedure Set_Comments(Value: string);
    procedure Set_ButtonText(Value: string);
  end;

{ Global Functions }

function GetParamDefList(Doc: IXMLDocument): IXMLParamDefListType;
function LoadParamDefList(const FileName: string): IXMLParamDefListType;
function NewParamDefList: IXMLParamDefListType;

const
  TargetNamespace = 'http://www.yourtargetnamespace.com';

implementation

{ Global Functions }

function GetParamDefList(Doc: IXMLDocument): IXMLParamDefListType;
begin
  Result := Doc.GetDocBinding('ParamDefList', TXMLParamDefListType, TargetNamespace) as IXMLParamDefListType;
end;

function LoadParamDefList(const FileName: string): IXMLParamDefListType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ParamDefList', TXMLParamDefListType, TargetNamespace) as IXMLParamDefListType;
end;

function NewParamDefList: IXMLParamDefListType;
begin
  Result := NewXMLDocument.GetDocBinding('ParamDefList', TXMLParamDefListType, TargetNamespace) as IXMLParamDefListType;
end;

{ TXMLParamDefListType }

procedure TXMLParamDefListType.AfterConstruction;
begin
  RegisterChildNode('ParamDef', TXMLParamDefType);
  ItemTag := 'ParamDef';
  ItemInterface := IXMLParamDefType;
  inherited;
end;

function TXMLParamDefListType.Get_Xmlns: Integer;
begin
  Result := AttributeNodes['xmlns'].NodeValue;
end;

procedure TXMLParamDefListType.Set_Xmlns(Value: Integer);
begin
  SetAttribute('xmlns', Value);
end;

function TXMLParamDefListType.Get_Version: Integer;
begin
  Result := AttributeNodes['version'].NodeValue;
end;

procedure TXMLParamDefListType.Set_Version(Value: Integer);
begin
  SetAttribute('version', Value);
end;

function TXMLParamDefListType.Get_ParamDef(Index: Integer): IXMLParamDefType;
begin
  Result := List[Index] as IXMLParamDefType;
end;

function TXMLParamDefListType.Add: IXMLParamDefType;
begin
  Result := AddItem(-1) as IXMLParamDefType;
end;

function TXMLParamDefListType.Insert(const Index: Integer): IXMLParamDefType;
begin
  Result := AddItem(Index) as IXMLParamDefType;
end;

{ TXMLParamDefType }

function TXMLParamDefType.Get_Id: Integer;
begin
  Result := AttributeNodes['id'].NodeValue;
end;

procedure TXMLParamDefType.Set_Id(Value: Integer);
begin
  SetAttribute('id', Value);
end;

function TXMLParamDefType.Get_ParamType: Integer;
begin
  Result := AttributeNodes['paramType'].NodeValue;
end;

procedure TXMLParamDefType.Set_ParamType(Value: Integer);
begin
  SetAttribute('paramType', Value);
end;

function TXMLParamDefType.Get_RangeType: string;
begin
  Result := AttributeNodes['rangeType'].Text;
end;

procedure TXMLParamDefType.Set_RangeType(Value: string);
begin
  SetAttribute('rangeType', Value);
end;

function TXMLParamDefType.Get_LowValue: Integer;
begin
  Result := AttributeNodes['lowValue'].NodeValue;
end;

procedure TXMLParamDefType.Set_LowValue(Value: Integer);
begin
  SetAttribute('lowValue', Value);
end;

function TXMLParamDefType.Get_HighValue: Integer;
begin
  Result := AttributeNodes['highValue'].NodeValue;
end;

procedure TXMLParamDefType.Set_HighValue(Value: Integer);
begin
  SetAttribute('highValue', Value);
end;

function TXMLParamDefType.Get_DefaultValue: Integer;
begin
  Result := AttributeNodes['defaultValue'].NodeValue;
end;

procedure TXMLParamDefType.Set_DefaultValue(Value: Integer);
begin
  SetAttribute('defaultValue', Value);
end;

function TXMLParamDefType.Get_Definitions: string;
begin
  Result := AttributeNodes['definitions'].Text;
end;

procedure TXMLParamDefType.Set_Definitions(Value: string);
begin
  SetAttribute('definitions', Value);
end;

function TXMLParamDefType.Get_Comments: string;
begin
  Result := AttributeNodes['comments'].Text;
end;

procedure TXMLParamDefType.Set_Comments(Value: string);
begin
  SetAttribute('comments', Value);
end;

function TXMLParamDefType.Get_ButtonText: string;
begin
  Result := AttributeNodes['buttonText'].Text;
end;

procedure TXMLParamDefType.Set_ButtonText(Value: string);
begin
  SetAttribute('buttonText', Value);
end;

end.