
{**********************************************************************************************************************}
{                                                                                                                      }
{                                                   XML Data Binding                                                   }
{                                                                                                                      }
{         Generated on: 4-5-2014 10:42:50                                                                              }
{       Generated from: D:\Crash20140412\Documenten\Delphi\nmg2editor\nmg2_editor_FMX\Deploy\g2EditorColorScheme.xml   }
{   Settings stored in: D:\Crash20140412\Documenten\Delphi\nmg2editor\nmg2_editor_FMX\Deploy\g2EditorColorScheme.xdb   }
{                                                                                                                      }
{**********************************************************************************************************************}

unit BVE.NMG2ColorScheme;

interface

uses xmldom, XMLDoc, XMLIntf;

type

{ Forward Decls }

  IXMLColorSchemeType = interface;
  IXMLSlotStripType = interface;
  IXMLControlStateStylesType = interface;
  IXMLControlStateStyleType = interface;
  IXMLDefaultFillType = interface;
  IXMLDefaultFontType = interface;
  IXMLDefaultStrokeType = interface;
  IXMLDisabledFillType = interface;
  IXMLDisabledFontType = interface;
  IXMLDisabledStrokeType = interface;
  IXMLFocusedFillType = interface;
  IXMLFocusedFontType = interface;
  IXMLFocusedStrokeType = interface;
  IXMLSelectedFillType = interface;
  IXMLSelectedFontType = interface;
  IXMLSelectedStrokeType = interface;
  IXMLFocusedSelectedFillType = interface;
  IXMLFocusedSelectedFontType = interface;
  IXMLFocusedSelectedStrokeType = interface;

{ IXMLColorSchemeType }

  IXMLColorSchemeType = interface(IXMLNode)
    ['{39B93ADD-DBCC-4611-B6C7-048F7027F866}']
    { Property Accessors }
    function Get_Xmlns: Integer;
    function Get_SlotStrip: IXMLSlotStripType;
    function Get_ControlStateStyles: IXMLControlStateStylesType;
    procedure Set_Xmlns(Value: Integer);
    { Methods & Properties }
    property Xmlns: Integer read Get_Xmlns write Set_Xmlns;
    property SlotStrip: IXMLSlotStripType read Get_SlotStrip;
    property ControlStateStyles: IXMLControlStateStylesType read Get_ControlStateStyles;
  end;

{ IXMLSlotStripType }

  IXMLSlotStripType = interface(IXMLNode)
    ['{59D8394B-6FB5-434B-A01B-E788F2EE67C2}']
    { Property Accessors }
    function Get_BackColor: string;
    function Get_SelectedColor: string;
    procedure Set_BackColor(Value: string);
    procedure Set_SelectedColor(Value: string);
    { Methods & Properties }
    property BackColor: string read Get_BackColor write Set_BackColor;
    property SelectedColor: string read Get_SelectedColor write Set_SelectedColor;
  end;

{ IXMLControlStateStylesType }

  IXMLControlStateStylesType = interface(IXMLNodeCollection)
    ['{F436ED44-CF58-40EF-A60D-55DCF1D3D1E9}']
    { Property Accessors }
    function Get_ControlStateStyle(Index: Integer): IXMLControlStateStyleType;
    { Methods & Properties }
    function Add: IXMLControlStateStyleType;
    function Insert(const Index: Integer): IXMLControlStateStyleType;
    property ControlStateStyle[Index: Integer]: IXMLControlStateStyleType read Get_ControlStateStyle; default;
  end;

{ IXMLControlStateStyleType }

  IXMLControlStateStyleType = interface(IXMLNode)
    ['{82E8EA8D-5705-4D9C-8C9A-2C2EDF53243B}']
    { Property Accessors }
    function Get_Type_: Integer;
    function Get_DefaultFill: IXMLDefaultFillType;
    function Get_DefaultFont: IXMLDefaultFontType;
    function Get_DefaultStroke: IXMLDefaultStrokeType;
    function Get_DisabledFill: IXMLDisabledFillType;
    function Get_DisabledFont: IXMLDisabledFontType;
    function Get_DisabledStroke: IXMLDisabledStrokeType;
    function Get_FocusedFill: IXMLFocusedFillType;
    function Get_FocusedFont: IXMLFocusedFontType;
    function Get_FocusedStroke: IXMLFocusedStrokeType;
    function Get_SelectedFill: IXMLSelectedFillType;
    function Get_SelectedFont: IXMLSelectedFontType;
    function Get_SelectedStroke: IXMLSelectedStrokeType;
    function Get_FocusedSelectedFill: IXMLFocusedSelectedFillType;
    function Get_FocusedSelectedFont: IXMLFocusedSelectedFontType;
    function Get_FocusedSelectedStroke: IXMLFocusedSelectedStrokeType;
    procedure Set_Type_(Value: Integer);
    { Methods & Properties }
    property Type_: Integer read Get_Type_ write Set_Type_;
    property DefaultFill: IXMLDefaultFillType read Get_DefaultFill;
    property DefaultFont: IXMLDefaultFontType read Get_DefaultFont;
    property DefaultStroke: IXMLDefaultStrokeType read Get_DefaultStroke;
    property DisabledFill: IXMLDisabledFillType read Get_DisabledFill;
    property DisabledFont: IXMLDisabledFontType read Get_DisabledFont;
    property DisabledStroke: IXMLDisabledStrokeType read Get_DisabledStroke;
    property FocusedFill: IXMLFocusedFillType read Get_FocusedFill;
    property FocusedFont: IXMLFocusedFontType read Get_FocusedFont;
    property FocusedStroke: IXMLFocusedStrokeType read Get_FocusedStroke;
    property SelectedFill: IXMLSelectedFillType read Get_SelectedFill;
    property SelectedFont: IXMLSelectedFontType read Get_SelectedFont;
    property SelectedStroke: IXMLSelectedStrokeType read Get_SelectedStroke;
    property FocusedSelectedFill: IXMLFocusedSelectedFillType read Get_FocusedSelectedFill;
    property FocusedSelectedFont: IXMLFocusedSelectedFontType read Get_FocusedSelectedFont;
    property FocusedSelectedStroke: IXMLFocusedSelectedStrokeType read Get_FocusedSelectedStroke;
  end;

{ IXMLDefaultFillType }

  IXMLDefaultFillType = interface(IXMLNode)
    ['{08928F67-BF95-4B12-95C7-22044687CBA1}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Kind: string read Get_Kind write Set_Kind;
  end;

{ IXMLDefaultFontType }

  IXMLDefaultFontType = interface(IXMLNode)
    ['{68794BCD-5E53-4007-937A-98C0B7F0820D}']
    { Property Accessors }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
    { Methods & Properties }
    property Family: string read Get_Family write Set_Family;
    property Size: Integer read Get_Size write Set_Size;
    property Style: string read Get_Style write Set_Style;
    property Color: string read Get_Color write Set_Color;
  end;

{ IXMLDefaultStrokeType }

  IXMLDefaultStrokeType = interface(IXMLNode)
    ['{0675C733-5B90-4FC2-8F21-E9C3803A03B8}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Thickness: Integer read Get_Thickness write Set_Thickness;
  end;

{ IXMLDisabledFillType }

  IXMLDisabledFillType = interface(IXMLNode)
    ['{26323C94-7E4B-4E38-938D-350118CFD6A0}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Kind: string read Get_Kind write Set_Kind;
  end;

{ IXMLDisabledFontType }

  IXMLDisabledFontType = interface(IXMLNode)
    ['{2BDBCAD1-1574-4D57-B7A2-0400574E10DF}']
    { Property Accessors }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
    { Methods & Properties }
    property Family: string read Get_Family write Set_Family;
    property Size: Integer read Get_Size write Set_Size;
    property Style: string read Get_Style write Set_Style;
    property Color: string read Get_Color write Set_Color;
  end;

{ IXMLDisabledStrokeType }

  IXMLDisabledStrokeType = interface(IXMLNode)
    ['{6A94687C-F849-4A6D-9A6C-3CCC2F1030B6}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Thickness: Integer read Get_Thickness write Set_Thickness;
  end;

{ IXMLFocusedFillType }

  IXMLFocusedFillType = interface(IXMLNode)
    ['{F9F2C7D0-40E5-49E2-BA72-A99295B65A98}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Kind: string read Get_Kind write Set_Kind;
  end;

{ IXMLFocusedFontType }

  IXMLFocusedFontType = interface(IXMLNode)
    ['{6C3FA224-D36E-4B96-A2A2-45049F60A629}']
    { Property Accessors }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
    { Methods & Properties }
    property Family: string read Get_Family write Set_Family;
    property Size: Integer read Get_Size write Set_Size;
    property Style: string read Get_Style write Set_Style;
    property Color: string read Get_Color write Set_Color;
  end;

{ IXMLFocusedStrokeType }

  IXMLFocusedStrokeType = interface(IXMLNode)
    ['{57028861-30C8-4810-BBA9-A80E0BC60E45}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Thickness: Integer read Get_Thickness write Set_Thickness;
  end;

{ IXMLSelectedFillType }

  IXMLSelectedFillType = interface(IXMLNode)
    ['{C51FCF81-1EE0-461F-9DA5-EBA2F5F7237B}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Kind: string read Get_Kind write Set_Kind;
  end;

{ IXMLSelectedFontType }

  IXMLSelectedFontType = interface(IXMLNode)
    ['{7A897346-7365-429F-9B68-5D05CE7FD15F}']
    { Property Accessors }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
    { Methods & Properties }
    property Family: string read Get_Family write Set_Family;
    property Size: Integer read Get_Size write Set_Size;
    property Style: string read Get_Style write Set_Style;
    property Color: string read Get_Color write Set_Color;
  end;

{ IXMLSelectedStrokeType }

  IXMLSelectedStrokeType = interface(IXMLNode)
    ['{4CF3A28A-2C92-4CFA-A206-F24145BFB7ED}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Thickness: Integer read Get_Thickness write Set_Thickness;
  end;

{ IXMLFocusedSelectedFillType }

  IXMLFocusedSelectedFillType = interface(IXMLNode)
    ['{6AB26425-94E5-4E17-A4A1-BA3B3829C447}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Kind: string read Get_Kind write Set_Kind;
  end;

{ IXMLFocusedSelectedFontType }

  IXMLFocusedSelectedFontType = interface(IXMLNode)
    ['{8A54DC7A-E579-48D1-BF1A-BAF083FB6FA9}']
    { Property Accessors }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
    { Methods & Properties }
    property Family: string read Get_Family write Set_Family;
    property Size: Integer read Get_Size write Set_Size;
    property Style: string read Get_Style write Set_Style;
    property Color: string read Get_Color write Set_Color;
  end;

{ IXMLFocusedSelectedStrokeType }

  IXMLFocusedSelectedStrokeType = interface(IXMLNode)
    ['{0D505501-5703-4120-8894-9F011E718636}']
    { Property Accessors }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
    { Methods & Properties }
    property Color: string read Get_Color write Set_Color;
    property Thickness: Integer read Get_Thickness write Set_Thickness;
  end;

{ Forward Decls }

  TXMLColorSchemeType = class;
  TXMLSlotStripType = class;
  TXMLControlStateStylesType = class;
  TXMLControlStateStyleType = class;
  TXMLDefaultFillType = class;
  TXMLDefaultFontType = class;
  TXMLDefaultStrokeType = class;
  TXMLDisabledFillType = class;
  TXMLDisabledFontType = class;
  TXMLDisabledStrokeType = class;
  TXMLFocusedFillType = class;
  TXMLFocusedFontType = class;
  TXMLFocusedStrokeType = class;
  TXMLSelectedFillType = class;
  TXMLSelectedFontType = class;
  TXMLSelectedStrokeType = class;
  TXMLFocusedSelectedFillType = class;
  TXMLFocusedSelectedFontType = class;
  TXMLFocusedSelectedStrokeType = class;

{ TXMLColorSchemeType }

  TXMLColorSchemeType = class(TXMLNode, IXMLColorSchemeType)
  protected
    { IXMLColorSchemeType }
    function Get_Xmlns: Integer;
    function Get_SlotStrip: IXMLSlotStripType;
    function Get_ControlStateStyles: IXMLControlStateStylesType;
    procedure Set_Xmlns(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLSlotStripType }

  TXMLSlotStripType = class(TXMLNode, IXMLSlotStripType)
  protected
    { IXMLSlotStripType }
    function Get_BackColor: string;
    function Get_SelectedColor: string;
    procedure Set_BackColor(Value: string);
    procedure Set_SelectedColor(Value: string);
  end;

{ TXMLControlStateStylesType }

  TXMLControlStateStylesType = class(TXMLNodeCollection, IXMLControlStateStylesType)
  protected
    { IXMLControlStateStylesType }
    function Get_ControlStateStyle(Index: Integer): IXMLControlStateStyleType;
    function Add: IXMLControlStateStyleType;
    function Insert(const Index: Integer): IXMLControlStateStyleType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLControlStateStyleType }

  TXMLControlStateStyleType = class(TXMLNode, IXMLControlStateStyleType)
  protected
    { IXMLControlStateStyleType }
    function Get_Type_: Integer;
    function Get_DefaultFill: IXMLDefaultFillType;
    function Get_DefaultFont: IXMLDefaultFontType;
    function Get_DefaultStroke: IXMLDefaultStrokeType;
    function Get_DisabledFill: IXMLDisabledFillType;
    function Get_DisabledFont: IXMLDisabledFontType;
    function Get_DisabledStroke: IXMLDisabledStrokeType;
    function Get_FocusedFill: IXMLFocusedFillType;
    function Get_FocusedFont: IXMLFocusedFontType;
    function Get_FocusedStroke: IXMLFocusedStrokeType;
    function Get_SelectedFill: IXMLSelectedFillType;
    function Get_SelectedFont: IXMLSelectedFontType;
    function Get_SelectedStroke: IXMLSelectedStrokeType;
    function Get_FocusedSelectedFill: IXMLFocusedSelectedFillType;
    function Get_FocusedSelectedFont: IXMLFocusedSelectedFontType;
    function Get_FocusedSelectedStroke: IXMLFocusedSelectedStrokeType;
    procedure Set_Type_(Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLDefaultFillType }

  TXMLDefaultFillType = class(TXMLNode, IXMLDefaultFillType)
  protected
    { IXMLDefaultFillType }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
  end;

{ TXMLDefaultFontType }

  TXMLDefaultFontType = class(TXMLNode, IXMLDefaultFontType)
  protected
    { IXMLDefaultFontType }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
  end;

{ TXMLDefaultStrokeType }

  TXMLDefaultStrokeType = class(TXMLNode, IXMLDefaultStrokeType)
  protected
    { IXMLDefaultStrokeType }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
  end;

{ TXMLDisabledFillType }

  TXMLDisabledFillType = class(TXMLNode, IXMLDisabledFillType)
  protected
    { IXMLDisabledFillType }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
  end;

{ TXMLDisabledFontType }

  TXMLDisabledFontType = class(TXMLNode, IXMLDisabledFontType)
  protected
    { IXMLDisabledFontType }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
  end;

{ TXMLDisabledStrokeType }

  TXMLDisabledStrokeType = class(TXMLNode, IXMLDisabledStrokeType)
  protected
    { IXMLDisabledStrokeType }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
  end;

{ TXMLFocusedFillType }

  TXMLFocusedFillType = class(TXMLNode, IXMLFocusedFillType)
  protected
    { IXMLFocusedFillType }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
  end;

{ TXMLFocusedFontType }

  TXMLFocusedFontType = class(TXMLNode, IXMLFocusedFontType)
  protected
    { IXMLFocusedFontType }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
  end;

{ TXMLFocusedStrokeType }

  TXMLFocusedStrokeType = class(TXMLNode, IXMLFocusedStrokeType)
  protected
    { IXMLFocusedStrokeType }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
  end;

{ TXMLSelectedFillType }

  TXMLSelectedFillType = class(TXMLNode, IXMLSelectedFillType)
  protected
    { IXMLSelectedFillType }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
  end;

{ TXMLSelectedFontType }

  TXMLSelectedFontType = class(TXMLNode, IXMLSelectedFontType)
  protected
    { IXMLSelectedFontType }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
  end;

{ TXMLSelectedStrokeType }

  TXMLSelectedStrokeType = class(TXMLNode, IXMLSelectedStrokeType)
  protected
    { IXMLSelectedStrokeType }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
  end;

{ TXMLFocusedSelectedFillType }

  TXMLFocusedSelectedFillType = class(TXMLNode, IXMLFocusedSelectedFillType)
  protected
    { IXMLFocusedSelectedFillType }
    function Get_Color: string;
    function Get_Kind: string;
    procedure Set_Color(Value: string);
    procedure Set_Kind(Value: string);
  end;

{ TXMLFocusedSelectedFontType }

  TXMLFocusedSelectedFontType = class(TXMLNode, IXMLFocusedSelectedFontType)
  protected
    { IXMLFocusedSelectedFontType }
    function Get_Family: string;
    function Get_Size: Integer;
    function Get_Style: string;
    function Get_Color: string;
    procedure Set_Family(Value: string);
    procedure Set_Size(Value: Integer);
    procedure Set_Style(Value: string);
    procedure Set_Color(Value: string);
  end;

{ TXMLFocusedSelectedStrokeType }

  TXMLFocusedSelectedStrokeType = class(TXMLNode, IXMLFocusedSelectedStrokeType)
  protected
    { IXMLFocusedSelectedStrokeType }
    function Get_Color: string;
    function Get_Thickness: Integer;
    procedure Set_Color(Value: string);
    procedure Set_Thickness(Value: Integer);
  end;

{ Global Functions }

function GetColorScheme(Doc: IXMLDocument): IXMLColorSchemeType;
function LoadColorScheme(const FileName: string): IXMLColorSchemeType;
function NewColorScheme: IXMLColorSchemeType;

const
  TargetNamespace = 'http://www.yourtargetnamespace.com';

implementation

{ Global Functions }

function GetColorScheme(Doc: IXMLDocument): IXMLColorSchemeType;
begin
  Result := Doc.GetDocBinding('ColorScheme', TXMLColorSchemeType, TargetNamespace) as IXMLColorSchemeType;
end;

function LoadColorScheme(const FileName: string): IXMLColorSchemeType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ColorScheme', TXMLColorSchemeType, TargetNamespace) as IXMLColorSchemeType;
end;

function NewColorScheme: IXMLColorSchemeType;
begin
  Result := NewXMLDocument.GetDocBinding('ColorScheme', TXMLColorSchemeType, TargetNamespace) as IXMLColorSchemeType;
end;

{ TXMLColorSchemeType }

procedure TXMLColorSchemeType.AfterConstruction;
begin
  RegisterChildNode('SlotStrip', TXMLSlotStripType);
  RegisterChildNode('ControlStateStyles', TXMLControlStateStylesType);
  inherited;
end;

function TXMLColorSchemeType.Get_Xmlns: Integer;
begin
  Result := AttributeNodes['xmlns'].NodeValue;
end;

procedure TXMLColorSchemeType.Set_Xmlns(Value: Integer);
begin
  SetAttribute('xmlns', Value);
end;

function TXMLColorSchemeType.Get_SlotStrip: IXMLSlotStripType;
begin
  Result := ChildNodes['SlotStrip'] as IXMLSlotStripType;
end;

function TXMLColorSchemeType.Get_ControlStateStyles: IXMLControlStateStylesType;
begin
  Result := ChildNodes['ControlStateStyles'] as IXMLControlStateStylesType;
end;

{ TXMLSlotStripType }

function TXMLSlotStripType.Get_BackColor: string;
begin
  Result := AttributeNodes['backColor'].Text;
end;

procedure TXMLSlotStripType.Set_BackColor(Value: string);
begin
  SetAttribute('backColor', Value);
end;

function TXMLSlotStripType.Get_SelectedColor: string;
begin
  Result := AttributeNodes['selectedColor'].Text;
end;

procedure TXMLSlotStripType.Set_SelectedColor(Value: string);
begin
  SetAttribute('selectedColor', Value);
end;

{ TXMLControlStateStylesType }

procedure TXMLControlStateStylesType.AfterConstruction;
begin
  RegisterChildNode('ControlStateStyle', TXMLControlStateStyleType);
  ItemTag := 'ControlStateStyle';
  ItemInterface := IXMLControlStateStyleType;
  inherited;
end;

function TXMLControlStateStylesType.Get_ControlStateStyle(Index: Integer): IXMLControlStateStyleType;
begin
  Result := List[Index] as IXMLControlStateStyleType;
end;

function TXMLControlStateStylesType.Add: IXMLControlStateStyleType;
begin
  Result := AddItem(-1) as IXMLControlStateStyleType;
end;

function TXMLControlStateStylesType.Insert(const Index: Integer): IXMLControlStateStyleType;
begin
  Result := AddItem(Index) as IXMLControlStateStyleType;
end;

{ TXMLControlStateStyleType }

procedure TXMLControlStateStyleType.AfterConstruction;
begin
  RegisterChildNode('DefaultFill', TXMLDefaultFillType);
  RegisterChildNode('DefaultFont', TXMLDefaultFontType);
  RegisterChildNode('DefaultStroke', TXMLDefaultStrokeType);
  RegisterChildNode('DisabledFill', TXMLDisabledFillType);
  RegisterChildNode('DisabledFont', TXMLDisabledFontType);
  RegisterChildNode('DisabledStroke', TXMLDisabledStrokeType);
  RegisterChildNode('FocusedFill', TXMLFocusedFillType);
  RegisterChildNode('FocusedFont', TXMLFocusedFontType);
  RegisterChildNode('FocusedStroke', TXMLFocusedStrokeType);
  RegisterChildNode('SelectedFill', TXMLSelectedFillType);
  RegisterChildNode('SelectedFont', TXMLSelectedFontType);
  RegisterChildNode('SelectedStroke', TXMLSelectedStrokeType);
  RegisterChildNode('FocusedSelectedFill', TXMLFocusedSelectedFillType);
  RegisterChildNode('FocusedSelectedFont', TXMLFocusedSelectedFontType);
  RegisterChildNode('FocusedSelectedStroke', TXMLFocusedSelectedStrokeType);
  inherited;
end;

function TXMLControlStateStyleType.Get_Type_: Integer;
begin
  Result := AttributeNodes['type'].NodeValue;
end;

procedure TXMLControlStateStyleType.Set_Type_(Value: Integer);
begin
  SetAttribute('type', Value);
end;

function TXMLControlStateStyleType.Get_DefaultFill: IXMLDefaultFillType;
begin
  Result := ChildNodes['DefaultFill'] as IXMLDefaultFillType;
end;

function TXMLControlStateStyleType.Get_DefaultFont: IXMLDefaultFontType;
begin
  Result := ChildNodes['DefaultFont'] as IXMLDefaultFontType;
end;

function TXMLControlStateStyleType.Get_DefaultStroke: IXMLDefaultStrokeType;
begin
  Result := ChildNodes['DefaultStroke'] as IXMLDefaultStrokeType;
end;

function TXMLControlStateStyleType.Get_DisabledFill: IXMLDisabledFillType;
begin
  Result := ChildNodes['DisabledFill'] as IXMLDisabledFillType;
end;

function TXMLControlStateStyleType.Get_DisabledFont: IXMLDisabledFontType;
begin
  Result := ChildNodes['DisabledFont'] as IXMLDisabledFontType;
end;

function TXMLControlStateStyleType.Get_DisabledStroke: IXMLDisabledStrokeType;
begin
  Result := ChildNodes['DisabledStroke'] as IXMLDisabledStrokeType;
end;

function TXMLControlStateStyleType.Get_FocusedFill: IXMLFocusedFillType;
begin
  Result := ChildNodes['FocusedFill'] as IXMLFocusedFillType;
end;

function TXMLControlStateStyleType.Get_FocusedFont: IXMLFocusedFontType;
begin
  Result := ChildNodes['FocusedFont'] as IXMLFocusedFontType;
end;

function TXMLControlStateStyleType.Get_FocusedStroke: IXMLFocusedStrokeType;
begin
  Result := ChildNodes['FocusedStroke'] as IXMLFocusedStrokeType;
end;

function TXMLControlStateStyleType.Get_SelectedFill: IXMLSelectedFillType;
begin
  Result := ChildNodes['SelectedFill'] as IXMLSelectedFillType;
end;

function TXMLControlStateStyleType.Get_SelectedFont: IXMLSelectedFontType;
begin
  Result := ChildNodes['SelectedFont'] as IXMLSelectedFontType;
end;

function TXMLControlStateStyleType.Get_SelectedStroke: IXMLSelectedStrokeType;
begin
  Result := ChildNodes['SelectedStroke'] as IXMLSelectedStrokeType;
end;

function TXMLControlStateStyleType.Get_FocusedSelectedFill: IXMLFocusedSelectedFillType;
begin
  Result := ChildNodes['FocusedSelectedFill'] as IXMLFocusedSelectedFillType;
end;

function TXMLControlStateStyleType.Get_FocusedSelectedFont: IXMLFocusedSelectedFontType;
begin
  Result := ChildNodes['FocusedSelectedFont'] as IXMLFocusedSelectedFontType;
end;

function TXMLControlStateStyleType.Get_FocusedSelectedStroke: IXMLFocusedSelectedStrokeType;
begin
  Result := ChildNodes['FocusedSelectedStroke'] as IXMLFocusedSelectedStrokeType;
end;

{ TXMLDefaultFillType }

function TXMLDefaultFillType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDefaultFillType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLDefaultFillType.Get_Kind: string;
begin
  Result := AttributeNodes['kind'].Text;
end;

procedure TXMLDefaultFillType.Set_Kind(Value: string);
begin
  SetAttribute('kind', Value);
end;

{ TXMLDefaultFontType }

function TXMLDefaultFontType.Get_Family: string;
begin
  Result := AttributeNodes['family'].Text;
end;

procedure TXMLDefaultFontType.Set_Family(Value: string);
begin
  SetAttribute('family', Value);
end;

function TXMLDefaultFontType.Get_Size: Integer;
begin
  Result := AttributeNodes['size'].NodeValue;
end;

procedure TXMLDefaultFontType.Set_Size(Value: Integer);
begin
  SetAttribute('size', Value);
end;

function TXMLDefaultFontType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDefaultFontType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLDefaultFontType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDefaultFontType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

{ TXMLDefaultStrokeType }

function TXMLDefaultStrokeType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDefaultStrokeType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLDefaultStrokeType.Get_Thickness: Integer;
begin
  Result := AttributeNodes['thickness'].NodeValue;
end;

procedure TXMLDefaultStrokeType.Set_Thickness(Value: Integer);
begin
  SetAttribute('thickness', Value);
end;

{ TXMLDisabledFillType }

function TXMLDisabledFillType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDisabledFillType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLDisabledFillType.Get_Kind: string;
begin
  Result := AttributeNodes['kind'].Text;
end;

procedure TXMLDisabledFillType.Set_Kind(Value: string);
begin
  SetAttribute('kind', Value);
end;

{ TXMLDisabledFontType }

function TXMLDisabledFontType.Get_Family: string;
begin
  Result := AttributeNodes['family'].Text;
end;

procedure TXMLDisabledFontType.Set_Family(Value: string);
begin
  SetAttribute('family', Value);
end;

function TXMLDisabledFontType.Get_Size: Integer;
begin
  Result := AttributeNodes['size'].NodeValue;
end;

procedure TXMLDisabledFontType.Set_Size(Value: Integer);
begin
  SetAttribute('size', Value);
end;

function TXMLDisabledFontType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLDisabledFontType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLDisabledFontType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDisabledFontType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

{ TXMLDisabledStrokeType }

function TXMLDisabledStrokeType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLDisabledStrokeType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLDisabledStrokeType.Get_Thickness: Integer;
begin
  Result := AttributeNodes['thickness'].NodeValue;
end;

procedure TXMLDisabledStrokeType.Set_Thickness(Value: Integer);
begin
  SetAttribute('thickness', Value);
end;

{ TXMLFocusedFillType }

function TXMLFocusedFillType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedFillType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLFocusedFillType.Get_Kind: string;
begin
  Result := AttributeNodes['kind'].Text;
end;

procedure TXMLFocusedFillType.Set_Kind(Value: string);
begin
  SetAttribute('kind', Value);
end;

{ TXMLFocusedFontType }

function TXMLFocusedFontType.Get_Family: string;
begin
  Result := AttributeNodes['family'].Text;
end;

procedure TXMLFocusedFontType.Set_Family(Value: string);
begin
  SetAttribute('family', Value);
end;

function TXMLFocusedFontType.Get_Size: Integer;
begin
  Result := AttributeNodes['size'].NodeValue;
end;

procedure TXMLFocusedFontType.Set_Size(Value: Integer);
begin
  SetAttribute('size', Value);
end;

function TXMLFocusedFontType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLFocusedFontType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLFocusedFontType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedFontType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

{ TXMLFocusedStrokeType }

function TXMLFocusedStrokeType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedStrokeType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLFocusedStrokeType.Get_Thickness: Integer;
begin
  Result := AttributeNodes['thickness'].NodeValue;
end;

procedure TXMLFocusedStrokeType.Set_Thickness(Value: Integer);
begin
  SetAttribute('thickness', Value);
end;

{ TXMLSelectedFillType }

function TXMLSelectedFillType.Get_Color: string;
begin
  Result := AttributeNodes['color'].NodeValue;
end;

procedure TXMLSelectedFillType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLSelectedFillType.Get_Kind: string;
begin
  Result := AttributeNodes['kind'].Text;
end;

procedure TXMLSelectedFillType.Set_Kind(Value: string);
begin
  SetAttribute('kind', Value);
end;

{ TXMLSelectedFontType }

function TXMLSelectedFontType.Get_Family: string;
begin
  Result := AttributeNodes['family'].Text;
end;

procedure TXMLSelectedFontType.Set_Family(Value: string);
begin
  SetAttribute('family', Value);
end;

function TXMLSelectedFontType.Get_Size: Integer;
begin
  Result := AttributeNodes['size'].NodeValue;
end;

procedure TXMLSelectedFontType.Set_Size(Value: Integer);
begin
  SetAttribute('size', Value);
end;

function TXMLSelectedFontType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLSelectedFontType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLSelectedFontType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLSelectedFontType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

{ TXMLSelectedStrokeType }

function TXMLSelectedStrokeType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLSelectedStrokeType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLSelectedStrokeType.Get_Thickness: Integer;
begin
  Result := AttributeNodes['thickness'].NodeValue;
end;

procedure TXMLSelectedStrokeType.Set_Thickness(Value: Integer);
begin
  SetAttribute('thickness', Value);
end;

{ TXMLFocusedSelectedFillType }

function TXMLFocusedSelectedFillType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedSelectedFillType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLFocusedSelectedFillType.Get_Kind: string;
begin
  Result := AttributeNodes['kind'].Text;
end;

procedure TXMLFocusedSelectedFillType.Set_Kind(Value: string);
begin
  SetAttribute('kind', Value);
end;

{ TXMLFocusedSelectedFontType }

function TXMLFocusedSelectedFontType.Get_Family: string;
begin
  Result := AttributeNodes['family'].Text;
end;

procedure TXMLFocusedSelectedFontType.Set_Family(Value: string);
begin
  SetAttribute('family', Value);
end;

function TXMLFocusedSelectedFontType.Get_Size: Integer;
begin
  Result := AttributeNodes['size'].NodeValue;
end;

procedure TXMLFocusedSelectedFontType.Set_Size(Value: Integer);
begin
  SetAttribute('size', Value);
end;

function TXMLFocusedSelectedFontType.Get_Style: string;
begin
  Result := AttributeNodes['style'].Text;
end;

procedure TXMLFocusedSelectedFontType.Set_Style(Value: string);
begin
  SetAttribute('style', Value);
end;

function TXMLFocusedSelectedFontType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedSelectedFontType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

{ TXMLFocusedSelectedStrokeType }

function TXMLFocusedSelectedStrokeType.Get_Color: string;
begin
  Result := AttributeNodes['color'].Text;
end;

procedure TXMLFocusedSelectedStrokeType.Set_Color(Value: string);
begin
  SetAttribute('color', Value);
end;

function TXMLFocusedSelectedStrokeType.Get_Thickness: Integer;
begin
  Result := AttributeNodes['thickness'].NodeValue;
end;

procedure TXMLFocusedSelectedStrokeType.Set_Thickness(Value: Integer);
begin
  SetAttribute('thickness', Value);
end;

end.