
{************************************************************************************************************}
{                                                                                                            }
{                                              XML Data Binding                                              }
{                                                                                                            }
{         Generated on: 26-12-2013 09:32:31                                                                  }
{       Generated from: C:\Users\Bruno Verhue\Documents\Delphi\nmg2editor\nmg2_editor_FMX\SVGStructure.xml   }
{   Settings stored in: C:\Users\Bruno Verhue\Documents\Delphi\nmg2editor\nmg2_editor_FMX\SVGStructure.xdb   }
{                                                                                                            }
{************************************************************************************************************}

unit BVE.NMG2SymbolDef;

interface

uses
  XML.xmldom,
  XML.XMLDoc,
  XML.XMLIntf;

type

{ Forward Decls }

  IXMLSvgType = interface;
  IXMLDefsType = interface;
  IXMLGType = interface;
  IXMLGTypeList = interface;
  IXMLTextType = interface;
  IXMLTextTypeList = interface;
  IXMLPathType = interface;
  IXMLPathTypeList = interface;
  IXMLRectType = interface;
  IXMLRectTypeList = interface;
  IXMLPolylineType = interface;
  IXMLPolylineTypeList = interface;
  IXMLPolygonType = interface;
  IXMLPolygonTypeList = interface;
  IXMLUseType = interface;
  IXMLUseTypeList = interface;
  IXMLLineType = interface;
  IXMLCircleType = interface;
  IXMLCircleTypeList = interface;

{ IXMLSvgType }

  IXMLSvgType = interface(IXMLNode)
    ['{B0FA8135-C2E3-4585-965C-D840ABBF3D9F}']
    { Property Accessors }
    function Get_Xmlns: string;
    function Get_Version: string;
    function Get_Height: Double;
    function Get_Width: Double;
    function Get_ViewBox: string;
    function Get_Defs: IXMLDefsType;
    function Get_G: IXMLGType;
    procedure Set_Xmlns(Value: string);
    procedure Set_Version(Value: string);
    procedure Set_Height(Value: Double);
    procedure Set_Width(Value: Double);
    procedure Set_ViewBox(Value: string);
    { Methods & Properties }
    property Xmlns: string read Get_Xmlns write Set_Xmlns;
    property Version: string read Get_Version write Set_Version;
    property Height: Double read Get_Height write Set_Height;
    property Width: Double read Get_Width write Set_Width;
    property ViewBox: string read Get_ViewBox write Set_ViewBox;
    property Defs: IXMLDefsType read Get_Defs;
    property G: IXMLGType read Get_G;
  end;

{ IXMLDefsType }

  IXMLDefsType = interface(IXMLNode)
    ['{86D1AB8E-B1E7-4853-AA23-27506BE7BAEC}']
    { Property Accessors }
    function Get_G: IXMLGTypeList;
    function Get_Path: IXMLPathTypeList;
    { Methods & Properties }
    property G: IXMLGTypeList read Get_G;
    property Path: IXMLPathTypeList read Get_Path;
  end;

{ IXMLGType }

  IXMLGType = interface(IXMLNode)
    ['{CDFD9271-C3E2-4E70-AC16-F232A1A06733}']
    { Property Accessors }
    function Get_Id: string;
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Transform: string;
    function Get_Style: string;
    function Get_Fill: string;
    function Get_Stroke: string;
    function Get_Strokewidth: Double;
    function Get_Text: IXMLTextTypeList;
    function Get_Path: IXMLPathTypeList;
    function Get_Rect: IXMLRectTypeList;
    function Get_Polyline: IXMLPolylineTypeList;
    function Get_G: IXMLGTypeList;
    function Get_Polygon: IXMLPolygonTypeList;
    function Get_Use: IXMLUseTypeList;
    function Get_Line: IXMLLineType;
    function Get_Circle: IXMLCircleTypeList;
    procedure Set_Id(Value: string);
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Transform(Value: string);
    procedure Set_Style(Value: string);
    procedure Set_Fill(Value: string);
    procedure Set_Stroke(Value: string);
    procedure Set_Strokewidth(Value: Double);
    { Methods & Properties }
    property Id: string read Get_Id write Set_Id;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Transform: string read Get_Transform write Set_Transform;
    property Style: string read Get_Style write Set_Style;
    property Fill: string read Get_Fill write Set_Fill;
    property Stroke: string read Get_Stroke write Set_Stroke;
    property Strokewidth: Double read Get_Strokewidth write Set_Strokewidth;
    property Text: IXMLTextTypeList read Get_Text;
    property Path: IXMLPathTypeList read Get_Path;
    property Rect: IXMLRectTypeList read Get_Rect;
    property Polyline: IXMLPolylineTypeList read Get_Polyline;
    property G: IXMLGTypeList read Get_G;
    property Polygon: IXMLPolygonTypeList read Get_Polygon;
    property Use: IXMLUseTypeList read Get_Use;
    property Line: IXMLLineType read Get_Line;
    property Circle: IXMLCircleTypeList read Get_Circle;
  end;

{ IXMLGTypeList }

  IXMLGTypeList = interface(IXMLNodeCollection)
    ['{A94C0958-8F15-400B-9820-8C328AC372A8}']
    { Methods & Properties }
    function Add: IXMLGType;
    function Insert(const Index: Integer): IXMLGType;

    function Get_Item(Index: Integer): IXMLGType;
    property Items[Index: Integer]: IXMLGType read Get_Item; default;
  end;

{ IXMLTextType }

  IXMLTextType = interface(IXMLNode)
    ['{1403FCB8-1AA6-49F7-8598-A43D42887B90}']
    { Property Accessors }
    function Get_Transform: string;
    function Get_Fontfamily: string;
    function Get_Fontweight: string;
    function Get_Fontsize: Double;
    function Get_Style: string;
    procedure Set_Transform(Value: string);
    procedure Set_Fontfamily(Value: string);
    procedure Set_Fontweight(Value: string);
    procedure Set_Fontsize(Value: Double);
    procedure Set_Style(Value: string);
    { Methods & Properties }
    property Transform: string read Get_Transform write Set_Transform;
    property Fontfamily: string read Get_Fontfamily write Set_Fontfamily;
    property Fontweight: string read Get_Fontweight write Set_Fontweight;
    property Fontsize: Double read Get_Fontsize write Set_Fontsize;
    property Style: string read Get_Style write Set_Style;
  end;

{ IXMLTextTypeList }

  IXMLTextTypeList = interface(IXMLNodeCollection)
    ['{F691597B-E69A-4629-81EB-27DEAA2B1921}']
    { Methods & Properties }
    function Add: IXMLTextType;
    function Insert(const Index: Integer): IXMLTextType;

    function Get_Item(Index: Integer): IXMLTextType;
    property Items[Index: Integer]: IXMLTextType read Get_Item; default;
  end;

{ IXMLPathType }

  IXMLPathType = interface(IXMLNode)
    ['{38283220-7125-49DE-8EFE-6BB80B7C68FF}']
    { Property Accessors }
    function Get_Style: string;
    function Get_D: string;
    function Get_Transform: string;
    function Get_Id: string;
    procedure Set_Style(Value: string);
    procedure Set_D(Value: string);
    procedure Set_Transform(Value: string);
    procedure Set_Id(Value: string);
    { Methods & Properties }
    property Style: string read Get_Style write Set_Style;
    property D: string read Get_D write Set_D;
    property Transform: string read Get_Transform write Set_Transform;
    property Id: string read Get_Id write Set_Id;
  end;

{ IXMLPathTypeList }

  IXMLPathTypeList = interface(IXMLNodeCollection)
    ['{159DCCE3-71F2-4C68-8057-4064A82D77A9}']
    { Methods & Properties }
    function Add: IXMLPathType;
    function Insert(const Index: Integer): IXMLPathType;

    function Get_Item(Index: Integer): IXMLPathType;
    property Items[Index: Integer]: IXMLPathType read Get_Item; default;
  end;

{ IXMLRectType }

  IXMLRectType = interface(IXMLNode)
    ['{420EA918-CBC8-42D9-AD8D-A5EF3640947F}']
    { Property Accessors }
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Style: string;
    function Get_Width: Double;
    function Get_Height: Double;
    function Get_Transform: string;
    function Get_Fill: string;
    function Get_Ry: Double;
    function Get_Rx: Double;
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Style(Value: string);
    procedure Set_Width(Value: Double);
    procedure Set_Height(Value: Double);
    procedure Set_Transform(Value: string);
    procedure Set_Fill(Value: string);
    procedure Set_Ry(Value: Double);
    procedure Set_Rx(Value: Double);
    { Methods & Properties }
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Style: string read Get_Style write Set_Style;
    property Width: Double read Get_Width write Set_Width;
    property Height: Double read Get_Height write Set_Height;
    property Transform: string read Get_Transform write Set_Transform;
    property Fill: string read Get_Fill write Set_Fill;
    property Ry: Double read Get_Ry write Set_Ry;
    property Rx: Double read Get_Rx write Set_Rx;
  end;

{ IXMLRectTypeList }

  IXMLRectTypeList = interface(IXMLNodeCollection)
    ['{B2356D77-4F9F-4541-B658-44125C031802}']
    { Methods & Properties }
    function Add: IXMLRectType;
    function Insert(const Index: Integer): IXMLRectType;

    function Get_Item(Index: Integer): IXMLRectType;
    property Items[Index: Integer]: IXMLRectType read Get_Item; default;
  end;

{ IXMLPolylineType }

  IXMLPolylineType = interface(IXMLNode)
    ['{733A2C57-644E-4DD8-B958-BACD0B120E46}']
    { Property Accessors }
    function Get_Style: string;
    function Get_Points: string;
    procedure Set_Style(Value: string);
    procedure Set_Points(Value: string);
    { Methods & Properties }
    property Style: string read Get_Style write Set_Style;
    property Points: string read Get_Points write Set_Points;
  end;

{ IXMLPolylineTypeList }

  IXMLPolylineTypeList = interface(IXMLNodeCollection)
    ['{8DC32A6B-EC92-48ED-A0AF-5AA4EB04F745}']
    { Methods & Properties }
    function Add: IXMLPolylineType;
    function Insert(const Index: Integer): IXMLPolylineType;

    function Get_Item(Index: Integer): IXMLPolylineType;
    property Items[Index: Integer]: IXMLPolylineType read Get_Item; default;
  end;

{ IXMLPolygonType }

  IXMLPolygonType = interface(IXMLNode)
    ['{A8E0D2E3-F472-4B8A-8837-3227153DE332}']
    { Property Accessors }
    function Get_Style: string;
    function Get_Points: string;
    procedure Set_Style(Value: string);
    procedure Set_Points(Value: string);
    { Methods & Properties }
    property Style: string read Get_Style write Set_Style;
    property Points: string read Get_Points write Set_Points;
  end;

{ IXMLPolygonTypeList }

  IXMLPolygonTypeList = interface(IXMLNodeCollection)
    ['{F75D9154-AEE7-4E3C-A7F9-8725B4C62099}']
    { Methods & Properties }
    function Add: IXMLPolygonType;
    function Insert(const Index: Integer): IXMLPolygonType;

    function Get_Item(Index: Integer): IXMLPolygonType;
    property Items[Index: Integer]: IXMLPolygonType read Get_Item; default;
  end;

{ IXMLUseType }

  IXMLUseType = interface(IXMLNode)
    ['{CD828F2C-83D1-4264-9134-EACAA9D1847B}']
    { Property Accessors }
    function Get_Transform: string;
    function Get_Href: string;
    function Get_Style: string;
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Id: string;
    function Get_Width: Double;
    function Get_Height: Double;
    procedure Set_Transform(Value: string);
    procedure Set_Href(Value: string);
    procedure Set_Style(Value: string);
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Id(Value: string);
    procedure Set_Width(Value: Double);
    procedure Set_Height(Value: Double);
    { Methods & Properties }
    property Transform: string read Get_Transform write Set_Transform;
    property Href: string read Get_Href write Set_Href;
    property Style: string read Get_Style write Set_Style;
    property X: Double read Get_X write Set_X;
    property Y: Double read Get_Y write Set_Y;
    property Id: string read Get_Id write Set_Id;
    property Width: Double read Get_Width write Set_Width;
    property Height: Double read Get_Height write Set_Height;
  end;

{ IXMLUseTypeList }

  IXMLUseTypeList = interface(IXMLNodeCollection)
    ['{86639BE5-03A4-4854-A37E-83E99C563DEF}']
    { Methods & Properties }
    function Add: IXMLUseType;
    function Insert(const Index: Integer): IXMLUseType;

    function Get_Item(Index: Integer): IXMLUseType;
    property Items[Index: Integer]: IXMLUseType read Get_Item; default;
  end;

{ IXMLLineType }

  IXMLLineType = interface(IXMLNode)
    ['{B9B14007-5A24-4E6C-8241-33E0AFCCCBD4}']
    { Property Accessors }
    function Get_X1: Double;
    function Get_Y1: Double;
    function Get_X2: Double;
    function Get_Y2: Double;
    function Get_Stroke: string;
    function Get_Strokewidth: Double;
    procedure Set_X1(Value: Double);
    procedure Set_Y1(Value: Double);
    procedure Set_X2(Value: Double);
    procedure Set_Y2(Value: Double);
    procedure Set_Stroke(Value: string);
    procedure Set_Strokewidth(Value: Double);
    { Methods & Properties }
    property X1: Double read Get_X1 write Set_X1;
    property Y1: Double read Get_Y1 write Set_Y1;
    property X2: Double read Get_X2 write Set_X2;
    property Y2: Double read Get_Y2 write Set_Y2;
    property Stroke: string read Get_Stroke write Set_Stroke;
    property Strokewidth: Double read Get_Strokewidth write Set_Strokewidth;
  end;

{ IXMLCircleType }

  IXMLCircleType = interface(IXMLNode)
    ['{DC683F77-C42C-4157-AE5D-32788247468A}']
    { Property Accessors }
    function Get_Cx: Double;
    function Get_Cy: Double;
    function Get_R: Double;
    function Get_Fill: string;
    procedure Set_Cx(Value: Double);
    procedure Set_Cy(Value: Double);
    procedure Set_R(Value: Double);
    procedure Set_Fill(Value: string);
    { Methods & Properties }
    property Cx: Double read Get_Cx write Set_Cx;
    property Cy: Double read Get_Cy write Set_Cy;
    property R: Double read Get_R write Set_R;
    property Fill: string read Get_Fill write Set_Fill;
  end;

{ IXMLCircleTypeList }

  IXMLCircleTypeList = interface(IXMLNodeCollection)
    ['{9FD12D1B-3A04-46D4-A3FC-878ABC096974}']
    { Methods & Properties }
    function Add: IXMLCircleType;
    function Insert(const Index: Integer): IXMLCircleType;

    function Get_Item(Index: Integer): IXMLCircleType;
    property Items[Index: Integer]: IXMLCircleType read Get_Item; default;
  end;

{ Forward Decls }

  TXMLSvgType = class;
  TXMLDefsType = class;
  TXMLGType = class;
  TXMLGTypeList = class;
  TXMLTextType = class;
  TXMLTextTypeList = class;
  TXMLPathType = class;
  TXMLPathTypeList = class;
  TXMLRectType = class;
  TXMLRectTypeList = class;
  TXMLPolylineType = class;
  TXMLPolylineTypeList = class;
  TXMLPolygonType = class;
  TXMLPolygonTypeList = class;
  TXMLUseType = class;
  TXMLUseTypeList = class;
  TXMLLineType = class;
  TXMLCircleType = class;
  TXMLCircleTypeList = class;

{ TXMLSvgType }

  TXMLSvgType = class(TXMLNode, IXMLSvgType)
  protected
    { IXMLSvgType }
    function Get_Xmlns: string;
    function Get_Version: string;
    function Get_Height: Double;
    function Get_Width: Double;
    function Get_ViewBox: string;
    function Get_Defs: IXMLDefsType;
    function Get_G: IXMLGType;
    procedure Set_Xmlns(Value: string);
    procedure Set_Version(Value: string);
    procedure Set_Height(Value: Double);
    procedure Set_Width(Value: Double);
    procedure Set_ViewBox(Value: string);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefsType }

  TXMLDefsType = class(TXMLNode, IXMLDefsType)
  private
    FG: IXMLGTypeList;
    FPath: IXMLPathTypeList;
  protected
    { IXMLDefsType }
    function Get_G: IXMLGTypeList;
    function Get_Path: IXMLPathTypeList;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGType }

  TXMLGType = class(TXMLNode, IXMLGType)
  private
    FText: IXMLTextTypeList;
    FPath: IXMLPathTypeList;
    FRect: IXMLRectTypeList;
    FPolyline: IXMLPolylineTypeList;
    FG: IXMLGTypeList;
    FPolygon: IXMLPolygonTypeList;
    FUse: IXMLUseTypeList;
    FCircle: IXMLCircleTypeList;
  protected
    { IXMLGType }
    function Get_Id: string;
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Transform: string;
    function Get_Style: string;
    function Get_Fill: string;
    function Get_Stroke: string;
    function Get_Strokewidth: Double;
    function Get_Text: IXMLTextTypeList;
    function Get_Path: IXMLPathTypeList;
    function Get_Rect: IXMLRectTypeList;
    function Get_Polyline: IXMLPolylineTypeList;
    function Get_G: IXMLGTypeList;
    function Get_Polygon: IXMLPolygonTypeList;
    function Get_Use: IXMLUseTypeList;
    function Get_Line: IXMLLineType;
    function Get_Circle: IXMLCircleTypeList;
    procedure Set_Id(Value: string);
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Transform(Value: string);
    procedure Set_Style(Value: string);
    procedure Set_Fill(Value: string);
    procedure Set_Stroke(Value: string);
    procedure Set_Strokewidth(Value: Double);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGTypeList }

  TXMLGTypeList = class(TXMLNodeCollection, IXMLGTypeList)
  protected
    { IXMLGTypeList }
    function Add: IXMLGType;
    function Insert(const Index: Integer): IXMLGType;

    function Get_Item(Index: Integer): IXMLGType;
  end;

{ TXMLTextType }

  TXMLTextType = class(TXMLNode, IXMLTextType)
  protected
    { IXMLTextType }
    function Get_Transform: string;
    function Get_Fontfamily: string;
    function Get_Fontweight: string;
    function Get_Fontsize: Double;
    function Get_Style: string;
    procedure Set_Transform(Value: string);
    procedure Set_Fontfamily(Value: string);
    procedure Set_Fontweight(Value: string);
    procedure Set_Fontsize(Value: Double);
    procedure Set_Style(Value: string);
  end;

{ TXMLTextTypeList }

  TXMLTextTypeList = class(TXMLNodeCollection, IXMLTextTypeList)
  protected
    { IXMLTextTypeList }
    function Add: IXMLTextType;
    function Insert(const Index: Integer): IXMLTextType;

    function Get_Item(Index: Integer): IXMLTextType;
  end;

{ TXMLPathType }

  TXMLPathType = class(TXMLNode, IXMLPathType)
  protected
    { IXMLPathType }
    function Get_Style: string;
    function Get_D: string;
    function Get_Transform: string;
    function Get_Id: string;
    procedure Set_Style(Value: string);
    procedure Set_D(Value: string);
    procedure Set_Transform(Value: string);
    procedure Set_Id(Value: string);
  end;

{ TXMLPathTypeList }

  TXMLPathTypeList = class(TXMLNodeCollection, IXMLPathTypeList)
  protected
    { IXMLPathTypeList }
    function Add: IXMLPathType;
    function Insert(const Index: Integer): IXMLPathType;

    function Get_Item(Index: Integer): IXMLPathType;
  end;

{ TXMLRectType }

  TXMLRectType = class(TXMLNode, IXMLRectType)
  protected
    { IXMLRectType }
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Style: string;
    function Get_Width: Double;
    function Get_Height: Double;
    function Get_Transform: string;
    function Get_Fill: string;
    function Get_Ry: Double;
    function Get_Rx: Double;
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Style(Value: string);
    procedure Set_Width(Value: Double);
    procedure Set_Height(Value: Double);
    procedure Set_Transform(Value: string);
    procedure Set_Fill(Value: string);
    procedure Set_Ry(Value: Double);
    procedure Set_Rx(Value: Double);
  end;

{ TXMLRectTypeList }

  TXMLRectTypeList = class(TXMLNodeCollection, IXMLRectTypeList)
  protected
    { IXMLRectTypeList }
    function Add: IXMLRectType;
    function Insert(const Index: Integer): IXMLRectType;

    function Get_Item(Index: Integer): IXMLRectType;
  end;

{ TXMLPolylineType }

  TXMLPolylineType = class(TXMLNode, IXMLPolylineType)
  protected
    { IXMLPolylineType }
    function Get_Style: string;
    function Get_Points: string;
    procedure Set_Style(Value: string);
    procedure Set_Points(Value: string);
  end;

{ TXMLPolylineTypeList }

  TXMLPolylineTypeList = class(TXMLNodeCollection, IXMLPolylineTypeList)
  protected
    { IXMLPolylineTypeList }
    function Add: IXMLPolylineType;
    function Insert(const Index: Integer): IXMLPolylineType;

    function Get_Item(Index: Integer): IXMLPolylineType;
  end;

{ TXMLPolygonType }

  TXMLPolygonType = class(TXMLNode, IXMLPolygonType)
  protected
    { IXMLPolygonType }
    function Get_Style: string;
    function Get_Points: string;
    procedure Set_Style(Value: string);
    procedure Set_Points(Value: string);
  end;

{ TXMLPolygonTypeList }

  TXMLPolygonTypeList = class(TXMLNodeCollection, IXMLPolygonTypeList)
  protected
    { IXMLPolygonTypeList }
    function Add: IXMLPolygonType;
    function Insert(const Index: Integer): IXMLPolygonType;

    function Get_Item(Index: Integer): IXMLPolygonType;
  end;

{ TXMLUseType }

  TXMLUseType = class(TXMLNode, IXMLUseType)
  protected
    { IXMLUseType }
    function Get_Transform: string;
    function Get_Href: string;
    function Get_Style: string;
    function Get_X: Double;
    function Get_Y: Double;
    function Get_Id: string;
    function Get_Width: Double;
    function Get_Height: Double;
    procedure Set_Transform(Value: string);
    procedure Set_Href(Value: string);
    procedure Set_Style(Value: string);
    procedure Set_X(Value: Double);
    procedure Set_Y(Value: Double);
    procedure Set_Id(Value: string);
    procedure Set_Width(Value: Double);
    procedure Set_Height(Value: Double);
  end;

{ TXMLUseTypeList }

  TXMLUseTypeList = class(TXMLNodeCollection, IXMLUseTypeList)
  protected
    { IXMLUseTypeList }
    function Add: IXMLUseType;
    function Insert(const Index: Integer): IXMLUseType;

    function Get_Item(Index: Integer): IXMLUseType;
  end;

{ TXMLLineType }

  TXMLLineType = class(TXMLNode, IXMLLineType)
  protected
    { IXMLLineType }
    function Get_X1: Double;
    function Get_Y1: Double;
    function Get_X2: Double;
    function Get_Y2: Double;
    function Get_Stroke: string;
    function Get_Strokewidth: Double;
    procedure Set_X1(Value: Double);
    procedure Set_Y1(Value: Double);
    procedure Set_X2(Value: Double);
    procedure Set_Y2(Value: Double);
    procedure Set_Stroke(Value: string);
    procedure Set_Strokewidth(Value: Double);
  end;

{ TXMLCircleType }

  TXMLCircleType = class(TXMLNode, IXMLCircleType)
  protected
    { IXMLCircleType }
    function Get_Cx: Double;
    function Get_Cy: Double;
    function Get_R: Double;
    function Get_Fill: string;
    procedure Set_Cx(Value: Double);
    procedure Set_Cy(Value: Double);
    procedure Set_R(Value: Double);
    procedure Set_Fill(Value: string);
  end;

{ TXMLCircleTypeList }

  TXMLCircleTypeList = class(TXMLNodeCollection, IXMLCircleTypeList)
  protected
    { IXMLCircleTypeList }
    function Add: IXMLCircleType;
    function Insert(const Index: Integer): IXMLCircleType;

    function Get_Item(Index: Integer): IXMLCircleType;
  end;

{ Global Functions }

function Getsvg(Doc: IXMLDocument): IXMLSvgType;
function Loadsvg(const FileName: string): IXMLSvgType;
function Newsvg: IXMLSvgType;

const
  TargetNamespace = 'http://www.w3.org/2000/svg';

implementation

{ Global Functions }

function Getsvg(Doc: IXMLDocument): IXMLSvgType;
begin
  Result := Doc.GetDocBinding('svg', TXMLSvgType, TargetNamespace) as IXMLSvgType;
end;

function Loadsvg(const FileName: string): IXMLSvgType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('svg', TXMLSvgType, TargetNamespace) as IXMLSvgType;
end;

function Newsvg: IXMLSvgType;
begin
  Result := NewXMLDocument.GetDocBinding('svg', TXMLSvgType, TargetNamespace) as IXMLSvgType;
end;

{ TXMLSvgType }

procedure TXMLSvgType.AfterConstruction;
begin
  RegisterChildNode('defs', TXMLDefsType);
  RegisterChildNode('g', TXMLGType);
  inherited;
end;

function TXMLSvgType.Get_Xmlns: string;
begin
  Result := AttributeNodes['xmlns'].Text;
end;

procedure TXMLSvgType.Set_Xmlns(Value: string);
begin
  SetAttribute('xmlns', Value);
end;

function TXMLSvgType.Get_Version: string;
begin
  Result := AttributeNodes['version'].Text;
end;

procedure TXMLSvgType.Set_Version(Value: string);
begin
  SetAttribute('version', Value);
end;

function TXMLSvgType.Get_Height: Double;
begin
  Result := AttributeNodes['height'].NodeValue;
end;

procedure TXMLSvgType.Set_Height(Value: Double);
begin
  SetAttribute('height', Value);
end;

function TXMLSvgType.Get_Width: Double;
begin
  Result := AttributeNodes['width'].NodeValue;
end;

procedure TXMLSvgType.Set_Width(Value: Double);
begin
  SetAttribute('width', Value);
end;

function TXMLSvgType.Get_ViewBox: string;
begin
  Result := AttributeNodes['viewBox'].Text;
end;

procedure TXMLSvgType.Set_ViewBox(Value: string);
begin
  SetAttribute('viewBox', Value);
end;

function TXMLSvgType.Get_Defs: IXMLDefsType;
begin
  Result := ChildNodes['defs'] as IXMLDefsType;
end;

function TXMLSvgType.Get_G: IXMLGType;
begin
  Result := ChildNodes[string('g')] as IXMLGType;
end;

{ TXMLDefsType }

procedure TXMLDefsType.AfterConstruction;
begin
  RegisterChildNode('g', TXMLGType);
  RegisterChildNode('path', TXMLPathType);
  FG := CreateCollection(TXMLGTypeList, IXMLGType, 'g') as IXMLGTypeList;
  FPath := CreateCollection(TXMLPathTypeList, IXMLPathType, 'path') as IXMLPathTypeList;
  inherited;
end;

function TXMLDefsType.Get_G: IXMLGTypeList;
begin
  Result := FG;
end;

function TXMLDefsType.Get_Path: IXMLPathTypeList;
begin
  Result := FPath;
end;

{ TXMLGType }

procedure TXMLGType.AfterConstruction;
begin
  RegisterChildNode('text', TXMLTextType);
  RegisterChildNode('path', TXMLPathType);
  RegisterChildNode('rect', TXMLRectType);
  RegisterChildNode('polyline', TXMLPolylineType);
  RegisterChildNode('g', TXMLGType);
  RegisterChildNode('polygon', TXMLPolygonType);
  RegisterChildNode('use', TXMLUseType);
  RegisterChildNode('line', TXMLLineType);
  RegisterChildNode('circle', TXMLCircleType);
  FText := CreateCollection(TXMLTextTypeList, IXMLTextType, 'text') as IXMLTextTypeList;
  FPath := CreateCollection(TXMLPathTypeList, IXMLPathType, 'path') as IXMLPathTypeList;
  FRect := CreateCollection(TXMLRectTypeList, IXMLRectType, 'rect') as IXMLRectTypeList;
  FPolyline := CreateCollection(TXMLPolylineTypeList, IXMLPolylineType, 'polyline') as IXMLPolylineTypeList;
  FG := CreateCollection(TXMLGTypeList, IXMLGType, 'g') as IXMLGTypeList;
  FPolygon := CreateCollection(TXMLPolygonTypeList, IXMLPolygonType, 'polygon') as IXMLPolygonTypeList;
  FUse := CreateCollection(TXMLUseTypeList, IXMLUseType, 'use') as IXMLUseTypeList;
  FCircle := CreateCollection(TXMLCircleTypeList, IXMLCircleType, 'circle') as IXMLCircleTypeList;
  inherited;
end;

function TXMLGType.Get_Id: string;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLGType.Set_Id(Value: string);
begin
  SetAttribute('id', Value);
end;

function TXMLGType.Get_X: Double;
begin
  Result := AttributeNodes[string('x')].NodeValue;
end;

procedure TXMLGType.Set_X(Value: Double);
begin
  SetAttribute(string('x'), Value);
end;

function TXMLGType.Get_Y: Double;
begin
  Result := AttributeNodes[string('y')].NodeValue;
end;

procedure TXMLGType.Set_Y(Value: Double);
begin
  SetAttribute(string('y'), Value);
end;

function TXMLGType.Get_Transform: string;
begin
  Result := AttributeNodes['transform'].Text;
end;

procedure TXMLGType.Set_Transform(Value: string);
begin
  SetAttribute('transform', Value);
end;

function TXMLGType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLGType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLGType.Get_Fill: string;
begin
  Result := AttributeNodes['fill'].Text;
end;

procedure TXMLGType.Set_Fill(Value: string);
begin
  SetAttribute('fill', Value);
end;

function TXMLGType.Get_Stroke: string;
begin
  Result := AttributeNodes['stroke'].Text;
end;

procedure TXMLGType.Set_Stroke(Value: string);
begin
  SetAttribute('stroke', Value);
end;

function TXMLGType.Get_Strokewidth: Double;
begin
  Result := AttributeNodes['stroke-width'].NodeValue;
end;

procedure TXMLGType.Set_Strokewidth(Value: Double);
begin
  SetAttribute('stroke-width', Value);
end;

function TXMLGType.Get_Text: IXMLTextTypeList;
begin
  Result := FText;
end;

function TXMLGType.Get_Path: IXMLPathTypeList;
begin
  Result := FPath;
end;

function TXMLGType.Get_Rect: IXMLRectTypeList;
begin
  Result := FRect;
end;

function TXMLGType.Get_Polyline: IXMLPolylineTypeList;
begin
  Result := FPolyline;
end;

function TXMLGType.Get_G: IXMLGTypeList;
begin
  Result := FG;
end;

function TXMLGType.Get_Polygon: IXMLPolygonTypeList;
begin
  Result := FPolygon;
end;

function TXMLGType.Get_Use: IXMLUseTypeList;
begin
  Result := FUse;
end;

function TXMLGType.Get_Line: IXMLLineType;
begin
  Result := ChildNodes['line'] as IXMLLineType;
end;

function TXMLGType.Get_Circle: IXMLCircleTypeList;
begin
  Result := FCircle;
end;

{ TXMLGTypeList }

function TXMLGTypeList.Add: IXMLGType;
begin
  Result := AddItem(-1) as IXMLGType;
end;

function TXMLGTypeList.Insert(const Index: Integer): IXMLGType;
begin
  Result := AddItem(Index) as IXMLGType;
end;

function TXMLGTypeList.Get_Item(Index: Integer): IXMLGType;
begin
  Result := List[Index] as IXMLGType;
end;

{ TXMLTextType }

function TXMLTextType.Get_Transform: string;
begin
  Result := AttributeNodes['transform'].Text;
end;

procedure TXMLTextType.Set_Transform(Value: string);
begin
  SetAttribute('transform', Value);
end;

function TXMLTextType.Get_Fontfamily: string;
begin
  Result := AttributeNodes['font-family'].Text;
end;

procedure TXMLTextType.Set_Fontfamily(Value: string);
begin
  SetAttribute('font-family', Value);
end;

function TXMLTextType.Get_Fontweight: string;
begin
  Result := AttributeNodes['font-weight'].Text;
end;

procedure TXMLTextType.Set_Fontweight(Value: string);
begin
  SetAttribute('font-weight', Value);
end;

function TXMLTextType.Get_Fontsize: Double;
begin
  Result := AttributeNodes['font-size'].NodeValue;
end;

procedure TXMLTextType.Set_Fontsize(Value: Double);
begin
  SetAttribute('font-size', Value);
end;

function TXMLTextType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLTextType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

{ TXMLTextTypeList }

function TXMLTextTypeList.Add: IXMLTextType;
begin
  Result := AddItem(-1) as IXMLTextType;
end;

function TXMLTextTypeList.Insert(const Index: Integer): IXMLTextType;
begin
  Result := AddItem(Index) as IXMLTextType;
end;

function TXMLTextTypeList.Get_Item(Index: Integer): IXMLTextType;
begin
  Result := List[Index] as IXMLTextType;
end;

{ TXMLPathType }

function TXMLPathType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPathType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLPathType.Get_D: string;
begin
  Result := AttributeNodes[string('d')].Text;
end;

procedure TXMLPathType.Set_D(Value: string);
begin
  SetAttribute(string('d'), Value);
end;

function TXMLPathType.Get_Transform: string;
begin
  Result := AttributeNodes['transform'].Text;
end;

procedure TXMLPathType.Set_Transform(Value: string);
begin
  SetAttribute('transform', Value);
end;

function TXMLPathType.Get_Id: string;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLPathType.Set_Id(Value: string);
begin
  SetAttribute('id', Value);
end;

{ TXMLPathTypeList }

function TXMLPathTypeList.Add: IXMLPathType;
begin
  Result := AddItem(-1) as IXMLPathType;
end;

function TXMLPathTypeList.Insert(const Index: Integer): IXMLPathType;
begin
  Result := AddItem(Index) as IXMLPathType;
end;

function TXMLPathTypeList.Get_Item(Index: Integer): IXMLPathType;
begin
  Result := List[Index] as IXMLPathType;
end;

{ TXMLRectType }

function TXMLRectType.Get_X: Double;
begin
  Result := AttributeNodes[string('x')].NodeValue;
end;

procedure TXMLRectType.Set_X(Value: Double);
begin
  SetAttribute(string('x'), Value);
end;

function TXMLRectType.Get_Y: Double;
begin
  Result := AttributeNodes[string('y')].NodeValue;
end;

procedure TXMLRectType.Set_Y(Value: Double);
begin
  SetAttribute(string('y'), Value);
end;

function TXMLRectType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLRectType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLRectType.Get_Width: Double;
begin
  Result := AttributeNodes['width'].NodeValue;
end;

procedure TXMLRectType.Set_Width(Value: Double);
begin
  SetAttribute('width', Value);
end;

function TXMLRectType.Get_Height: Double;
begin
  Result := AttributeNodes['height'].NodeValue;
end;

procedure TXMLRectType.Set_Height(Value: Double);
begin
  SetAttribute('height', Value);
end;

function TXMLRectType.Get_Transform: string;
begin
  Result := AttributeNodes['transform'].Text;
end;

procedure TXMLRectType.Set_Transform(Value: string);
begin
  SetAttribute('transform', Value);
end;

function TXMLRectType.Get_Fill: string;
begin
  Result := AttributeNodes['fill'].Text;
end;

procedure TXMLRectType.Set_Fill(Value: string);
begin
  SetAttribute('fill', Value);
end;

function TXMLRectType.Get_Ry: Double;
begin
  Result := AttributeNodes['ry'].NodeValue;
end;

procedure TXMLRectType.Set_Ry(Value: Double);
begin
  SetAttribute('ry', Value);
end;

function TXMLRectType.Get_Rx: Double;
begin
  Result := AttributeNodes['rx'].NodeValue;
end;

procedure TXMLRectType.Set_Rx(Value: Double);
begin
  SetAttribute('rx', Value);
end;

{ TXMLRectTypeList }

function TXMLRectTypeList.Add: IXMLRectType;
begin
  Result := AddItem(-1) as IXMLRectType;
end;

function TXMLRectTypeList.Insert(const Index: Integer): IXMLRectType;
begin
  Result := AddItem(Index) as IXMLRectType;
end;

function TXMLRectTypeList.Get_Item(Index: Integer): IXMLRectType;
begin
  Result := List[Index] as IXMLRectType;
end;

{ TXMLPolylineType }

function TXMLPolylineType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPolylineType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLPolylineType.Get_Points: string;
begin
  Result := AttributeNodes['points'].Text;
end;

procedure TXMLPolylineType.Set_Points(Value: string);
begin
  SetAttribute('points', Value);
end;

{ TXMLPolylineTypeList }

function TXMLPolylineTypeList.Add: IXMLPolylineType;
begin
  Result := AddItem(-1) as IXMLPolylineType;
end;

function TXMLPolylineTypeList.Insert(const Index: Integer): IXMLPolylineType;
begin
  Result := AddItem(Index) as IXMLPolylineType;
end;

function TXMLPolylineTypeList.Get_Item(Index: Integer): IXMLPolylineType;
begin
  Result := List[Index] as IXMLPolylineType;
end;

{ TXMLPolygonType }

function TXMLPolygonType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLPolygonType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLPolygonType.Get_Points: string;
begin
  Result := AttributeNodes['points'].Text;
end;

procedure TXMLPolygonType.Set_Points(Value: string);
begin
  SetAttribute('points', Value);
end;

{ TXMLPolygonTypeList }

function TXMLPolygonTypeList.Add: IXMLPolygonType;
begin
  Result := AddItem(-1) as IXMLPolygonType;
end;

function TXMLPolygonTypeList.Insert(const Index: Integer): IXMLPolygonType;
begin
  Result := AddItem(Index) as IXMLPolygonType;
end;

function TXMLPolygonTypeList.Get_Item(Index: Integer): IXMLPolygonType;
begin
  Result := List[Index] as IXMLPolygonType;
end;

{ TXMLUseType }

function TXMLUseType.Get_Transform: string;
begin
  Result := AttributeNodes['transform'].Text;
end;

procedure TXMLUseType.Set_Transform(Value: string);
begin
  SetAttribute('transform', Value);
end;

function TXMLUseType.Get_Href: string;
begin
  Result := AttributeNodes['xlink:href'].Text;
end;

procedure TXMLUseType.Set_Href(Value: string);
begin
  SetAttribute('xlink:href', Value);
end;

function TXMLUseType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLUseType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLUseType.Get_X: Double;
begin
  Result := AttributeNodes[string('x')].NodeValue;
end;

procedure TXMLUseType.Set_X(Value: Double);
begin
  SetAttribute(string('x'), Value);
end;

function TXMLUseType.Get_Y: Double;
begin
  Result := AttributeNodes[string('y')].NodeValue;
end;

procedure TXMLUseType.Set_Y(Value: Double);
begin
  SetAttribute(string('y'), Value);
end;

function TXMLUseType.Get_Id: string;
begin
  Result := AttributeNodes['id'].Text;
end;

procedure TXMLUseType.Set_Id(Value: string);
begin
  SetAttribute('id', Value);
end;

function TXMLUseType.Get_Width: Double;
begin
  Result := AttributeNodes['width'].NodeValue;
end;

procedure TXMLUseType.Set_Width(Value: Double);
begin
  SetAttribute('width', Value);
end;

function TXMLUseType.Get_Height: Double;
begin
  Result := AttributeNodes['height'].NodeValue;
end;

procedure TXMLUseType.Set_Height(Value: Double);
begin
  SetAttribute('height', Value);
end;

{ TXMLUseTypeList }

function TXMLUseTypeList.Add: IXMLUseType;
begin
  Result := AddItem(-1) as IXMLUseType;
end;

function TXMLUseTypeList.Insert(const Index: Integer): IXMLUseType;
begin
  Result := AddItem(Index) as IXMLUseType;
end;

function TXMLUseTypeList.Get_Item(Index: Integer): IXMLUseType;
begin
  Result := List[Index] as IXMLUseType;
end;

{ TXMLLineType }

function TXMLLineType.Get_X1: Double;
begin
  Result := AttributeNodes['x1'].NodeValue;
end;

procedure TXMLLineType.Set_X1(Value: Double);
begin
  SetAttribute('x1', Value);
end;

function TXMLLineType.Get_Y1: Double;
begin
  Result := AttributeNodes['y1'].NodeValue;
end;

procedure TXMLLineType.Set_Y1(Value: Double);
begin
  SetAttribute('y1', Value);
end;

function TXMLLineType.Get_X2: Double;
begin
  Result := AttributeNodes['x2'].NodeValue;
end;

procedure TXMLLineType.Set_X2(Value: Double);
begin
  SetAttribute('x2', Value);
end;

function TXMLLineType.Get_Y2: Double;
begin
  Result := AttributeNodes['y2'].NodeValue;
end;

procedure TXMLLineType.Set_Y2(Value: Double);
begin
  SetAttribute('y2', Value);
end;

function TXMLLineType.Get_Stroke: string;
begin
  Result := AttributeNodes['stroke'].Text;
end;

procedure TXMLLineType.Set_Stroke(Value: string);
begin
  SetAttribute('stroke', Value);
end;

function TXMLLineType.Get_Strokewidth: Double;
begin
  Result := AttributeNodes['stroke-width'].NodeValue;
end;

procedure TXMLLineType.Set_Strokewidth(Value: Double);
begin
  SetAttribute('stroke-width', Value);
end;

{ TXMLCircleType }

function TXMLCircleType.Get_Cx: Double;
begin
  Result := AttributeNodes['cx'].NodeValue;
end;

procedure TXMLCircleType.Set_Cx(Value: Double);
begin
  SetAttribute('cx', Value);
end;

function TXMLCircleType.Get_Cy: Double;
begin
  Result := AttributeNodes['cy'].NodeValue;
end;

procedure TXMLCircleType.Set_Cy(Value: Double);
begin
  SetAttribute('cy', Value);
end;

function TXMLCircleType.Get_R: Double;
begin
  Result := AttributeNodes[string('r')].NodeValue;
end;

procedure TXMLCircleType.Set_R(Value: Double);
begin
  SetAttribute(string('r'), Value);
end;

function TXMLCircleType.Get_Fill: string;
begin
  Result := AttributeNodes['fill'].Text;
end;

procedure TXMLCircleType.Set_Fill(Value: string);
begin
  SetAttribute('fill', Value);
end;

{ TXMLCircleTypeList }

function TXMLCircleTypeList.Add: IXMLCircleType;
begin
  Result := AddItem(-1) as IXMLCircleType;
end;

function TXMLCircleTypeList.Insert(const Index: Integer): IXMLCircleType;
begin
  Result := AddItem(Index) as IXMLCircleType;
end;

function TXMLCircleTypeList.Get_Item(Index: Integer): IXMLCircleType;
begin
  Result := List[Index] as IXMLCircleType;
end;

end.