unit UnitModuleDef;

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
//  Tool for editing the ModuleDef.XML file
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Contnrs, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, VCL.Grids, VCL.ExtCtrls, VCL.ActnList, VCL.ActnMan,
  VCL.XPStyleActnCtrls, VCL.PlatformDefaultStyleActnCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Grids, ExtCtrls,
  ActnList, ActnMan, XPStyleActnCtrls, Contnrs,
{$ENDIF}
  DOM, XMLWrite, XMLRead, g2_types, g2_database, g2_graph, g2_file, g2_mess, g2_usb,
  g2_classes, g2_midi;

type
  TSVGSkin = class;
  TSVGSkinSection = class;
  TSVGSkinSectionPlaceHolder = class;

  TSVGObject = class(TObject)
  private
    FID : string;
    FSkin : TSVGSkin;
    FPlaceholder : TSVGSkinSectionPlaceHolder;
    FNode : TDomNode;
  public
    constructor Create( aPlaceholder : TSVGSkinSectionPlaceHolder; aID : string); virtual;
    destructor Destroy; override;
  end;

  TSVGG2Module = class(TSVGObject)
    constructor Create( aPlaceholder : TSVGSkinSectionPlaceHolder; aID : string;
        aModule : TG2GraphModule);
    destructor Destroy; override;
  end;

  TSVGSkinSectionPlaceHolder = class(TObject)
  private
    FID : string;
    FSection : TSVGSkinSection;
    FSkin : TSVGSkin;
    FNode : TDomNode;
  public
    constructor Create( aSection : TSVGSkinSection; aID : string; x, y : integer);
    destructor Destroy; override;
  end;

  TSVGSkinSection = class(TObject)
  private
    FID : string;
    FComment : string;
    FSkin : TSVGSkin;
    FRows, FCols : integer;
    FPlaceholderWidth, FPlaceholderHeight : integer;
    FNode : TDomNode;
    FPlaceholders : TObjectList;
  public
    constructor Create( aSkin : TSVGSkin; aID, aComment : string);
    destructor  Destroy; override;
    function    CommentText( aID : string; aText : string): string;
    procedure   CreateComment( aNode : TDomNode);
    function    CreatePlaceholder( aID : string): TSVGSkinSectionPlaceholder;
  end;

  TSVGSkin = class(TComponent)
  private
    Doc : TXMLDocument;
    BitMapList : TObjectList;
    TextBitmap : TBitmap;

    FDefList : TStringList;

    FSections : TObjectList;
    FRootNode : TDomNode;
    FDefsNode : TDomNode;
    FMainNode : TDomNode;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure SaveToFile;

    function  GetID2D( aBaseName : string; aWidth, aHeight : single): string;
    function  GetID3D( aBaseName : string; aWidth, aHeight, aDepth : single): string;
    function  GetID4D( aBaseName : string; aWidth, aHeight, aDepth : single; aSymbolID: string): string;
    function  GetLabelID( aBaseName: string; aText : string): string;

    function  FindDef( aID : string): TDomNode;
    function  DefExists( aID : string): boolean;

    function  FindSection( aID : string): TSVGSkinSection;
    function  AddToSection( aSectionID : string; aID : string): TSVGSkinSectionPlaceholder;

    function  FindBitmap( aBitmap : TBitmap): integer;
    function  AddSymbolFromBitmap( aBitmap : TBitmap): integer;

    function  RoundedBevelObject( aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
    function  BevelObject( aID : string; aWidth, aHeight, aBevelWidth : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
    function  RoundedButtonObject( aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single;
        aColSideLeft, aColSideTop, aColSideRight, aColSideBottom, aColBG, aColFace : string ): string;

    function  CreateChild( aNode : TDomNode; aElementName, aID : string): TDomNode;
    function  CreateG( aNode : TDomNode; aID : string): TDomNode;
    function  CreateDesc( aNode : TDomNode; aID, aDescription : string): TDomNode;
    function  CreateUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer): TDomNode;

    function  CreateRect( aNode : TDomNode; aID : string; aFill, aStroke : string; x, y, width, height : integer): TDomNode;
    procedure CreateGradientPoint( aNode : TDomNode; aID : string; aOffset : single; aStopColor : string);
    function  CreateLinearGradient( aNode : TDomNode; aID : string): TDomNode;

    procedure CreateModulePanelGradients;
    procedure CreateModulePanelGradient( aNode : TDomNode; aID : string; aColor1, aColor2 : string);

    function  CreateParamLink( aNode : TDomNode; aID : string; aCodeRef, aMasterRef, aInfoFunc, aTextFunc : integer; aCtrlType, aDependencies : string): TDomNode;
    function  CreateConnLink(aNode: TDomNode; aID: string; aCodeRef: integer): TDomNode;
    function  CreateParamLinkedUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer;
                 aCodeRef, aMasterRef, aInfoFunc, aTextFunc: integer; aCtrlType, aDependencies: string): TDomNode;
    function  CreateConnectorLinkedUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer;
                 aCodeRef: integer; aCtrlStyle : string): TDomNode;
    function  CreateSymbol( aNode : TDomNode; aID : string; aWidth, aHeight : integer): TDomNode;
    function  CreateSymbolSmallArrowUp: TDomNode;
    function  CreateSymbolSmallArrowDown: TDomNode;
    function  CreateSymbolSmallArrowLeft: TDomNode;
    function  CreateSymbolSmallArrowRight: TDomNode;
    function  CreateLabel( aNode : TDomNode; aID : string; x, y : integer; aText : string; aFontSize : integer): TDomNode;
    function  CreateTextField( aNode : TDomNode; aID : string; x, y, aWidth, aHeight : single): TDomNode;
    function  CreateRoundedButtonTextUp( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth : single): TDomNode;
    function  CreateRoundedButtonTextDown( aNode : TDomNode; aID : String; aWidth, aHeight, aBevelWidth : single): TDomNode;
    function  CreateRoundedButton( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth : single; aSymbolID : string; aSymbolWidth, aSymbolHeight : single): TDomNode;
    function  CreateButtonFlatButton( aNode : TDomNode; aID : string; aWidth, aHeight : single; aSymbolID : string; aSymbolWidth, aSymbolHeight : single): TDomNode;
    function  CreateButtonFlat( aNode : TDomNode; aID : string; aBtns : TStringList): TDomNode;
    function  CreateButtonRadio( aNode : TDomNode; aID : string; dx, dy : integer; aBtns : TStringList): TDomNode;

    function  CreateBtnIncDecVert( aNode : TDomNode; aID : string): TDomNode;
    function  CreateBtnIncDecHorz( aNode : TDomNode; aID : string): TDomNode;
    function  CreateInConnector( aNode : TDomNode; aID : string; aColor : string): TDomNode;
    function  CreateOutConnector( aNode : TDomNode; aID : string; aColor : string): TDomNode;

    function CreateKnobSideGradient( aNode : TDomNode; aID : string; r : single): TDomNode;
    function CreateCenterBtnOff( aNode : TDomNode): TDomNode;
    function CreateCenterBtnOn( aNode : TDomNode): TDomNode;
    function CreateCenterBtn( aNode : TDomNode): TDomNode;
    function CreateKnobButtons( aNode : TDomNode): TDomNode;
    function CreateKnob( aNode : TDomNode; aID : string; w, h, c_x, c_y, r_side, r_face : single; reset : boolean): TDomNode;
    function CreateKnobBig( aNode : TDomNode): TDomNode;
    function CreateKnobMedium( aNode : TDomNode): TDomNode;
    function CreateKnobResetMedium( aNode : TDomNode): TDomNode;
    function CreateKnobReset( aNode : TDomNode): TDomNode;
    function CreateKnobSmall( aNode : TDomNode): TDomNode;
    function CreateKnobSliderKnob( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single): TDomNode;
    function CreateKnobSlider( aNode : TDomNode; aID : string): TDomNode;

    function CreateBackground( aNode : TDomNode; aID : string): TDomNode;
    procedure SetAttribute( aNode : TDomNode; aName, aValue : string);

    function CreateSection( aID : string; aPlaceholderWidth, aPlaceholderHeight, aCols, aRows : integer; aText : string): TSVGSkinSection;
  end;


  TfrmModuleDef = class(TForm)
    Panel1: TPanel;
    sgParams: TStringGrid;
    Panel2: TPanel;
    Label1: TLabel;
    eModuleName: TEdit;
    Button1: TButton;
    Button2: TButton;
    ActionManager1: TActionManager;
    aExtractModuleInfo: TAction;
    lbModules: TListBox;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Button3: TButton;
    G2_module_def: TG2;
    Button4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure aNextModuleExecute(Sender: TObject);
    procedure aPrevModuleExecute(Sender: TObject);
    procedure aExtractModuleInfoExecute(Sender: TObject);
    procedure lbModulesClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    slParamAttr : TStringList;
    FModuleType,
    FModuleIndex : integer;
    FModule : TG2GraphModule;
    procedure LoadDatabase;
    procedure UpdateControls;
    procedure UpdateModuleDef;
    procedure ExtractTextEdit;
    procedure CreateSVG;
    procedure LoadModule;
    procedure UnloadModule;
  end;

var
  frmModuleDef: TfrmModuleDef;

implementation
uses UnitG2Editor, graph_util_vcl;

{$R *.dfm}

const
  nl = #10+#13;

  cBtnFace = '#c8b7b7';
  cBtnSideDark = '#6c5353';
  cBtnSideMedium = '#ac9393';
  cBtnSideLight = '#e3dbdb';

  cBtnFlatFace = '#ececec';

  cKnobFace = '#ececec';
  cKnobBorder = '#000000';

  cSldrKnobFace = '#333333';
  cSldrKnobSideDark = '#000000';
  cSldrKnobSideMedium = '#4d4d4d';
  cSldrKnobSideLight = '#e6e6e6';

  cPartSelWindow = '#e9c6af';
  cPartselSelected = '#dfdbaa';
  cPartSelBtn = '#deaa87';

  idSymbol = 'symbol';
  idLabel = 'label';
  idLedOffGreen = 'ledGreenOff';
  idLedOffSequencer = 'ledSeqOff';
  idLedOnGreen = 'ledgreen';
  idLedOnSequencer = 'ledseq';
  idMiniVU = 'minivU';
  idTxtField = 'txtField';
  idBtnText = 'btntext';
  idBtnFlat = 'btnflat';
  idBtnIncDecHorz = 'btnincdec_horz';
  idBtnIncDecVert = 'btnincdec_vert';
  idLevelShift = 'levelshift';
  idBtnRadio = 'btnradio';
  idSlider = 'slider';
  idKnobBig = 'knobbig';
  idKnobMedium = 'knobmedium';
  idKnobResetMedium = 'knobresetmedium';
  idKnobReset = 'knobreset';
  idKnobSmall = 'knobsmall';
  idKnobBtns = 'knobbtns';
  idKnobCenterBtn = 'knobresetbtn';
  idConnectorIn = 'conn_in';
  idConnectorOut = 'conn_out';
  idPartSel = 'partsel';
  idGraph = 'graph';
  idModule = 'module';
  idPanelBackground = 'patchbackground';

{ TForm1 }

procedure TfrmModuleDef.aExtractModuleInfoExecute(Sender: TObject);
begin
  ExtractTextEdit;
end;

procedure TfrmModuleDef.aNextModuleExecute(Sender: TObject);
begin
//
end;

procedure TfrmModuleDef.aPrevModuleExecute(Sender: TObject);
begin
//
end;

procedure TfrmModuleDef.Button1Click(Sender: TObject);
var G2 : TG2;
    i : integer;
begin
  UpdateModuleDef;
  for i := 0 to frmG2Main.FG2List.Count - 1 do begin
    G2 := frmG2Main.FG2List[i] as TG2;
    G2.LoadModuleDefs('');
  end;
end;

procedure TfrmModuleDef.Button3Click(Sender: TObject);
begin
  CreateSVG;
end;

procedure TfrmModuleDef.Button4Click(Sender: TObject);
var sr : TSearchRec;
    Path : string;
    sl, slModuleDef : TStringList;
    i : integer;
begin
  sl := TStringList.Create;
  slModuleDef := TStringList.Create;
  try
    Path := ExtractFilePath(ParamStr(0)) + '\Modules\';
    if FindFirst( Path + '*.txt', faAnyFile, sr) = 0 then begin
      repeat
        sl.Add('<#File:' + ExtractFilename(sr.Name) + '#>');
        slModuleDef.LoadFromFile(Path + sr.Name);
        for i := 0 to slModuleDef.Count - 1 do
          sl.Add(slModuleDef[i]);
      until (FindNext(sr) <> 0);
      FindClose(sr);

      sl.SaveToFile('ModulePanelDefs.txt');
    end;
  finally
    slModuleDef.Free;
    sl.Free;
  end;
end;

procedure TfrmModuleDef.FormCreate(Sender: TObject);
begin
  LoadDatabase;
end;

procedure TfrmModuleDef.FormDestroy(Sender: TObject);
begin
  slParamAttr.Free;
end;

procedure TfrmModuleDef.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmModuleDef.lbModulesClick(Sender: TObject);
begin
  if lbModules.ItemIndex = -1 then
    exit;

  FModuleType := TXMLModuleDefType(lbModules.Items.Objects[lbModules.ItemIndex]).ModuleType;
  UpdateControls;
end;

procedure TfrmModuleDef.LoadDatabase;
var m, p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
begin
  lbModules.Clear;
  G2_module_def.LoadModuleDefs('');
  slParamAttr := TStringList.Create;
  FModuleIndex := -1;
  // Make a list of all posible parameter attributes
  slParamAttr.Clear;
  for m := 0 to G2_module_def.FModuleDefList.Count - 1 do begin
    lbModules.Items.Add(string(G2_module_def.FModuleDefList.ModuleDef[m].LongName));
    lbModules.Items.Objects[m] := G2_module_def.FModuleDefList.ModuleDef[m];
    if G2_module_def.FModuleDefList.ModuleDef[m].Params <> nil then
      for p := 0 to G2_module_def.FModuleDefList.ModuleDef[m].Params.Count - 1 do begin
        ParamNode := G2_module_def.FModuleDefList.ModuleDef[m].Params[p];
        Node := ParamNode.FirstChild;
        while Node <> nil do begin
          if slParamAttr.IndexOf( Node.NodeName) = -1 then
            slParamAttr.Add( Node.NodeName);
          Node := Node.NextSibling;
        end;
      end;
  end;
end;

procedure TfrmModuleDef.LoadModule;
begin
  FModule := G2_module_def.SelectedPatch.CreateModule( ltVA, 1, FModuleType) as TG2GraphModule;
  if assigned(FModule) then begin
    G2_module_def.SelectedPatch.AddModuleToPatch( ltVA, FModule);
    G2_module_def.SelectedPatch.SortLeds;
  end;
end;

procedure TfrmModuleDef.UnloadModule;
begin
  try
    G2_module_def.SelectedPatch.RemoveFromLedList( ltVA, FModule.ModuleIndex);
    G2_module_def.SelectedPatch.DeleteModuleFromPatch(ltVA, FModule);
  finally
    FModule := nil;
  end;
end;

procedure TfrmModuleDef.UpdateControls;
var m, p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
begin
  if FModule <> nil then
    UnloadModule;

  FModuleIndex := -1;
  for m := 0 to G2_module_def.FModuleDefList.Count - 1 do begin
    if FModuleType = G2_module_def.FModuleDefList.ModuleDef[m].ModuleType then
      FModuleIndex := m;
  end;

  if FModuleIndex <> -1 then begin
    LoadModule;
    eModuleName.Text := string(G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].LongName);
    if G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params <> nil then begin
      sgParams.ColCount := slParamAttr.Count + 1;
      sgParams.RowCount := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count + 1;
      for p := 0 to slParamAttr.Count - 1 do
        sgParams.Cells[p + 1, 0] := slParamAttr[p];
      for p := 0 to G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
        sgParams.Cells[0, p + 1] := IntToStr(p);
        ParamNode := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
        for i := 0 to slParamAttr.Count - 1 do begin
          Node := ParamNode.FindNode( slParamAttr[i]);
          if Node <> nil then
            sgParams.Cells[i + 1, p + 1] := Node.TextContent
          else
            sgParams.Cells[i + 1, p + 1] := '';
        end;
      end;
    end;
  end else
    raise Exception.Create('Moduletype ' + IntToStr(FModuleType) + ' not found.');
end;

procedure TfrmModuleDef.UpdateModuleDef;
var p, i : integer;
    ParamNode : TDOMElement;
    Node : TDOMNode;
    new_filename : string;
begin
  for p := 0 to G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params.Count - 1 do begin
    ParamNode := G2_module_def.FModuleDefList.ModuleDef[FModuleIndex].Params[p];
    for i := 0 to slParamAttr.Count - 1 do begin
      Node := ParamNode.FindNode( slParamAttr[i]);
      if Node <> nil then begin
        if sgParams.Cells[i + 1, p + 1] = '' then begin
          ParamNode.DetachChild(Node);
          Node.Free;
        end else
          Node.TextContent := sgParams.Cells[i + 1, p + 1];
      end else
        if sgParams.Cells[i + 1, p + 1] <> '' then begin
          Node := TDOMDocument(ParamNode.OwnerDocument).CreateElement(slParamAttr[i]);
          ParamNode.AppendChild( Node);
          Node.TextContent := sgParams.Cells[i + 1, p + 1];
        end;
    end;
  end;

  new_filename := 'ModuleDef.xml';
  WriteXMLFile( G2_module_def.FXMLModuleDefs, new_filename);
end;

procedure TfrmModuleDef.ExtractTextEdit;
var Control : TG2GraphChildControl;
    i, j, k, l, p, ParamIndex : Integer;
    ButtonText : string;
    ParamNode : TXMLParamType;
    ParamDefNode : TXMLParamDefType;
    Node : TDOMNode;
    new_filename : string;
begin
  // I used this to get some info out of the paneld definitions
  for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin

    FModuleType := G2_module_def.FModuleDefList.ModuleDef[i].ModuleType;
    LoadModule;

    try

      for j := 0 to FModule.ParameterCount - 1 do begin
        k := 0;
        while (k < FModule.Panel.ChildControlsCount) and (FModule.Panel.GraphChildControls[k].Parameter <> FModule.Parameter[j]) do
          inc(k);

        if (k < FModule.Panel.ChildControlsCount) then begin
          Control := FModule.Panel.GraphChildControls[k];

          // Add buttontext to xml database
          if (control is TG2GraphButton) then begin
            if (Control as TG2GraphButton).ButtonText.Count > 0 then begin

              ButtonText := '';
              for l := 0 to (Control as TG2GraphButton).ButtonText.Count - 1 do
                ButtonText := ButtonText + (Control as TG2GraphButton).ButtonText[l] + ';';

              ParamIndex := (Control as TG2GraphButton).Parameter.ParamIndex;
              ParamNode := G2_module_def.FModuleDefList.ModuleDef[i].Params[ParamIndex];

              // Zoek parameter type op
              p := 0;
              while (p < G2_module_def.FParamDefList.Count) and (G2_module_def.FParamDefList.ParamDef[p].Id  <>  ParamNode.Get_Id) do
                inc(p);

              if (p < G2_module_def.FParamDefList.Count) then begin
                ParamDefNode := G2_module_def.FParamDefList.ParamDef[p];

                Node := ParamDefNode.FindNode( 'ButtonText');
                if Node <> nil then begin
                  if ButtonText = '' then begin
                    ParamDefNode.DetachChild(Node);
                    Node.Free;
                  end else
                    Node.TextContent := ButtonText;
                end else
                  if ButtonText <> '' then begin
                     Node := TDOMDocument(ParamDefNode.OwnerDocument).CreateElement('ButtonText');
                     ParamDefNode.AppendChild( Node);
                     Node.TextContent := ButtonText;
                  end;
              end;
            end;
          end;

        end;
      end;
    finally
      UnloadModule
    end;
  end;
  new_filename := 'ParamDef_new.xml';
  WriteXMLFile( G2_module_def.FXMLParamDefs, new_filename);
end;

procedure TfrmModuleDef.CreateSVG;

var Control : TG2GraphChildControl;
    BitMapList : TObjectList;
    Bitmap, TextBitmap : TBitmap;
    i, j, k : integer;
    symbol_section, knob_section, buttontext_up_section, buttontext_down_section, buttontext_section,
    buttonflat_section, textfield_section, led_off_section, led_on_section, btnIncDec_section,
    partsel_section, connector_section, graph_section, minivu_section, btnRadioUp_section,
    btnRadioDown_section, levelshift_section, module_section, uipanel_section,
    btnflatoptions_section, partseloptions_section,
    mr, mc, btc, bfc, kc, tfc, sc, loffc, lonc, lsc, bidc, psc, cc, gc, minivu_c,
    rbc, uir, bfoc, psoc: integer;
    new_filename : string;
    Doc : TXMLDocument;
    RootNode, DefsNode, GMainNode,
    GSymbolSectionNode, GSymbolDefNode, SymbolNode,
    GTextFieldSectionNode, GTextFieldDefNode, TextFieldNode,
    GButtonTextUpSectionNode, GButtonTextUpDefNode, ButtonTextUpNode,
    GButtonTextDownSectionNode, GButtonTextDownDefNode, ButtonTextDownNode,
    GButtonTextSectionNode, GButtonTextDefNode, ButtonTextNode,
    GButtonFlatSectionNode, GButtonFlatDefNode, ButtonFlatNode,
    GKnobSectionNode, GKnobDefNode, KnobNode,
    GLedOffSectionNode, GLedOffDefNode, LedOffNode,
    GLedOnSectionNode, GLedOnDefNode, LedOnNode,
    GMiniVUSectionNode, GMiniVUDefNode, MiniVUNode,
    GGraphSectionNode, GGraphDefNode, GraphNode,
    GBtnIncDecSectionNode, GBtnIncDecDefNode, BtnIncDecNode,
    GLevelShiftSectionNode, GLevelShiftDefNode, LevelShiftNode,
    GBtnRadioUpSectionNode, GBtnRadioUpDefNode, BtnRadioUpNode,
    GBtnRadioDownSectionNode, GBtnRadioDownDefNode, BtnRadioDownNode,
    GPartSelSectionNode, GPartSelDefNode, PartSelNode,
    GConnectorSectionNode, GConnectorDefNode, ConnectorNode,
    GButtonFlatOptionsSectionNode, GButtonFlatOptionsDefNode, ButtonFlatOptionsNode,
    GPartSelOptionsSectionNode, GPartSelOptionsDefNode, PartSelOptionsNode,
    GModuleSectionNode, GModuleDefNode, ModuleNode,
    GModuleColorSectionNode, GModuleColorDefNode, ModuleColorNode,
    GUIPanelSectionNode, GUIPanelDefNode, UIPanelNode : TDomNode;
    id_smallarrow_up, id_smallarrow_down, id_smallarrow_left, id_smallarrow_right : integer;
    SVGSkin : TSVGSkin;

    function Darker(c : integer; f : byte): integer;
    var R, G, B : byte;

      function sub( comp : byte): byte;
      begin
        if comp - f > 0 then
          result := comp - f
        else
          result := 0;
      end;

    begin
      //if c < 0 then
      //  c := ColorToRGB(c);

      R := (c and $000000FF);
      G := (c and $0000FF00) shr 8;
      B := (c and $00FF0000) shr 16;

      result := sub(B) * 65536
              + sub(G) * 256
              + sub(R);
    end;

    function Lighter(c : integer; f : byte): integer;
    var R, G, B : byte;

      function add( comp : byte): byte;
      begin
        if comp + f < 255 then
          result := comp + f
        else
          result := 255;
      end;

    begin
      //if c < 0 then
      //  c := ColorToRGB(c);

      R := (c and $000000FF);
      G := (c and $0000FF00) shr 8;
      B := (c and $00FF0000) shr 16;

      result := add(B) * 65536
              + add(G) * 256
              + add(R);
    end;

    function FindBitmap( aBitmap : TBitmap): integer;
    begin
      Result := 0;
      while (Result < BitMapList.Count) and not(CompareBitmap( aBitmap, BitMapList.Items[Result] as TBitmap)) do
        inc(Result);

      if (Result >= BitMapList.Count) then
        Result := -1;
    end;

    function NiceBevel( aBaseName : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
    var dw, dh : single;
    begin
      dw := aBevelWidth / 1.5;
      dh := aBevelHeight / 1.5;

      Result :=
         '<path id="' + aBaseName + '_bevel_l" fill="' + aColSideLeft + '" stroke="none"'
       + '  d="M 0,0'
           + ' c 0,0 ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
             + ' 0,' + FloatToStr(aBevelWidth + dw) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2 - dw - aBevelWidth) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2)
             + ' 0,' + FloatToStr(dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_t" fill="' + aColSideTop + '" stroke="none"'
       + '  d="M ' + FloatToStr(aWidth) + ',0'
           + ' c 0,0 ' + FloatToStr(-dh) + ','  + FloatToStr(aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(aBevelHeight)
             + ' ' + FloatToStr(-aBevelHeight - dh) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2 + dh + aBevelHeight) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2) + ',0'
             + ' ' + FloatToStr(-dh) + ',0 ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_r" fill="' + aColSideRight + '" stroke="none"'
       + '  d="M ' + FloatToStr(aWidth) + ',' + FloatToStr(aHeight)
           + ' c 0,0 ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(-dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
             + ' 0,' + FloatToStr(-aBevelWidth - dw) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2 + dw + aBevelWidth) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2)
             + ' 0,' + FloatToStr(-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
           + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_b" fill="' + aColSideBottom + '" stroke="none"'
       + '  d="M 0,' + FloatToStr(aHeight)
           + ' c 0,0 ' + FloatToStr(dh) + ','  + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(-aBevelHeight)
             + ' ' + FloatToStr(aBevelHeight + dh) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2 - dh - aBevelHeight) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2) + ',0'
             + ' ' + FloatToStr(dh) + ',0 ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight)
           + ' z">'
       + '</path>';
    end;

    function Bevel( aBaseName : string; aWidth, aHeight, aBevelWidth : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
    begin
       Result :=
         '<path id="' + aBaseName + '_bevel_l" fill="' + aColSideLeft + '" stroke="none"'
       + '  d="M 0,0 l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' v ' + FloatToStr((aHeight - aBevelWidth*2)) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_t" fill="' + aColSideTop + '" stroke="none"'
       + '  d="M 0,0 h ' + FloatToStr(aWidth) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' h ' + FloatToStr(-(aWidth - aBevelWidth*2)) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_r" fill="' + aColSideRight + '" stroke="none"'
       + '  d="M ' + FloatToStr(aWidth) + ',0 v ' + FloatToStr(aHeight) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(-aBevelWidth) + ' v ' + FloatToStr(-(aHeight - aBevelWidth*2)) + ' z">'
       + '</path>'
       + '<path id="' + aBaseName + '_bevel_b" fill="' + aColSideBottom + '" stroke="none"'
       + '  d="M 0, ' + FloatToStr(aHeight) + ' l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(-aBevelWidth) + ' h ' + FloatToStr((aWidth - aBevelWidth*2)) + ' l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(aBevelWidth) +' z">'
       + '</path>';
    end;

    function CommentText( aID : string; aText : string): string;
    var sl : TStringList;
        i : integer;
    begin
      sl := TStringList.Create;
      try
        sl.Text := aText;

        Result := '<text id="' + aID + '">';

        for i := 0 to sl.Count - 1 do begin
          Result := Result
                  + '<tspan id="' + aID + '_' + IntTostr(i) + '" x="0" dy="' + IntTostr(10) +'">'
                  + sl[i]
                  + '</tspan>';
        end;

        Result := Result + '</text>'

      finally
        sl.Free;
      end;
    end;

    procedure CreateComment( aNode : TDomNode; aID: string; aText : string);
    var S : TStringStream;
    begin
      S := TStringStream.Create( CommentText( aID, aText));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function CreateSection( aNode : TDomNode; aID : string; y : integer; aText : string): TDomNode;
    var CommentNode : TDomNode;
    begin
      Result := Doc.CreateElement('g');
      aNode.AppendChild(Result);
      TDOMElement(Result).SetAttribute('id', aID);
      TDOMElement(Result).SetAttribute('transform', 'translate(' + IntToStr(0) + ',' + IntToStr(y) + ')');

      CommentNode := Doc.CreateElement('g');
      Result.AppendChild(CommentNode);
      TDOMElement(CommentNode).SetAttribute('id', aID + '_comment');
      CreateComment( CommentNode, aID + '_comment_text', aText);
    end;

    function CreateSectionPlaceholder( aNode : TDomNode; aID : string; x, y : integer): TDomNode;
    begin
      Result := Doc.CreateElement('g');
      aNode.AppendChild(Result);
      TDOMElement(Result).SetAttribute('id', aID);
      TDOMElement(Result).SetAttribute('transform', 'translate(' + IntToStr(x) + ',' + IntToStr(y) + ')');
    end;

    procedure CreateGradientPoint( aNode : TDomNode; aOffset : single; aStopColor : string);
    var S : TStringStream;
    begin
      S := TStringStream.Create(
         '<stop offset="' + FloatToStr(aOffset) + '%" stop-color="' + aStopColor + '" />');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function CreateGradientNodeForStopPoints( aNode : TDomNode; aID : string): TDomNode;
    begin
      Result := Doc.CreateElement('linearGradient');
      aNode.AppendChild(Result);
      TDOMElement(Result).SetAttribute('id', aID);
    end;

    procedure CreateModulePanelGradient( aNode : TDomNode; aID : string; aColor1, aColor2 : string);
    var StopsNode : TDomNode;
        S : TStringStream;
    begin
      StopsNode := CreateGradientNodeForStopPoints( aNode, aID + '_stops');
      CreateGradientPoint( StopsNode, 0, aColor1);
      CreateGradientPoint( StopsNode, 100, aColor2);

      S := TStringStream.Create(
         '<linearGradient'
       + ' id="' + aID + '"'
       + ' gradientUnits="objectBoundingBox"'
       + ' x1="0"'
       + ' x2="1"'
       + ' y1="0"'
       + ' y2="1"'
       + ' xlink:href="#' +aID + '_stops' + '">'
       + '</linearGradient>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateModulePanelGradients;
    var j : integer;
        color : integer;
        svg_color1, svg_color2 : string;
    begin
      for j := 0 to 24 do begin
        color := (ModuleColors[j] and $00FF0000) shr 32
               + (ModuleColors[j] and $0000FF00)
               + (ModuleColors[j] and $000000FF) shl 32;
        svg_color1 := '#' +  IntToHex( Lighter(Color,40), 6);
        svg_color2 := '#' +  IntToHex( Color, 6);
        CreateModulePanelGradient( DefsNode, 'PanelGradient_' + IntToStr(j), svg_color1{'#F0F0F0'} , svg_color2);
      end;
    end;

    procedure CreateLabel( aNode : TDomNode; x, y : integer; aText : string; aFontSize : integer);
    var S : TStringStream;
        id, FontFamily : string;
        p : integer;
    begin
      id := idLabel;

      p := pos('&', aText);
      if p>0 then
        aText[p]:= '+';

      FontFamily := 'arial';

      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Label for G2 editor</desc>'
       + '  <text id="' + id + '_text' + '" x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '"'
       + '        style="font-size:' + IntToStr(aFontSize) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:' + FontFamily + '"'
       + '        xml:space="preserve">'
       + '     <tspan id="' + id + '_span"  x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '">'
       + aText
       + '     </tspan>'
       + '  </text>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function Use( aID, aRef : string; dx, dy : single): string;
    begin
      Result :=
         ' <use'
       + ' id="' + aID + '"'
       + ' xlink:href="#' + aRef + '"'
       + ' transform="translate(' + FloatToStr(dx) + ',' + FloatToStr(dy) + ')"/>'
    end;

    function CreateUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer): TDomNode;
    begin
      Result := Doc.CreateElement('use');
      aNode.AppendChild(Result);
      TDOMElement(Result).SetAttribute('id', aID);
      TDOMElement(Result).SetAttribute('xlink:href', '#' + aRef);
      TDOMElement(Result).SetAttribute('transform', 'translate(' + IntToStr(dx) + ',' + IntToStr(dy) + ')');
      TDOMElement(Result).SetAttribute('x', '0');
      TDOMElement(Result).SetAttribute('y', '0');
    end;

    procedure CreateSymbol( aNode : TDomNode; index : string; aWidth, aHeight : integer);
    var S : TStringStream;
        id : string;
    begin
      id := idSymbol + '_' + index;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Symbol for G2 editor</desc>'
       + '  <g id="' + id + '_sel">'
       + '     <rect id="' + id + '_rect" fill="none" stroke="blue" stroke-width="0.2" x="0" y="0" width="' + IntToStr(aWidth)+ '" height="' + IntToStr(aHeight) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function AddSymbolFromBitmap( aBitmap : TBitmap): integer;
    var id : string;
    begin
      Result := FindBitmap(aBitmap);
      if Result = - 1 then begin

        Bitmap := TBitmap.Create;
        Bitmap.Assign(aBitmap);
        BitMapList.Add(Bitmap);
        Result := BitmapList.Count -1;

        id := idSymbol + '_' + IntToStr(Result) + '_def';

        SymbolNode := GSymbolSectionNode.FirstChild;
        while (SymbolNode <> nil) and (TDomELement(SymbolNode).GetAttribute('id') <>  id) do
          SymbolNode := SymbolNode.NextSibling;

        if not assigned(SymbolNode) then begin
          GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, id, sc, 0);
          CreateSymbol( GSymbolDefNode, IntToStr(Result), (BitmapList[Result] as TBitmap).Width, (BitmapList[Result] as TBitmap).Height);
          sc := sc + (BitmapList[Result] as TBitmap).Width + 20;
        end;
      end;
    end;

    function AddSymbol( aId : integer): integer;
    var id : string;
    begin

      GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, id, sc, 0);
      CreateSymbol( GSymbolDefNode, IntToStr(Result), (BitmapList[Result] as TBitmap).Width, (BitmapList[Result] as TBitmap).Height);
      sc := sc + (BitmapList[Result] as TBitmap).Width + 20;
    end;

    procedure CreateSymbolSmallArrowUp;
    var S : TStringStream;
        def_id : string;
    begin
      BitMapList.Add(nil);
      id_smallarrow_up := BitmapList.Count -1;

      def_id := idSymbol + '_' + IntToStr(id_smallarrow_up) + '_def';

      GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, def_id, sc, 0);
      sc := sc + 20;

      S := TStringStream.Create(
        '  <path id="' + idSymbol + '_' + IntToStr(id_smallarrow_up) + '" fill="black" stroke="none"'
        + ' d="M0,3 h6 l-3,-3 z" />');
      try
        ReadXMLFragment( GSymbolDefNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateSymbolSmallArrowDown;
    var S : TStringStream;
        def_id : string;
    begin
      BitMapList.Add(nil);
      id_smallarrow_down := BitmapList.Count -1;

      def_id := idSymbol + '_' + IntToStr(id_smallarrow_down) + '_def';

      GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, def_id, sc, 0);
      sc := sc + 20;

      S := TStringStream.Create(
        '  <path id="' + idSymbol + '_' + IntToStr(id_smallarrow_down) + '" fill="black" stroke="none"'
        + ' d="M0,0 h6 l-3,3 z" />');
      try
        ReadXMLFragment( GSymbolDefNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateSymbolSmallArrowLeft;
    var S : TStringStream;
        def_id : string;
    begin
      BitMapList.Add(nil);
      id_smallarrow_left := BitmapList.Count -1;

      def_id := idSymbol + '_' + IntToStr(id_smallarrow_left) + '_def';

      GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, def_id, sc, 0);
      sc := sc + 20;

      S := TStringStream.Create(
        '  <path id="' + idSymbol + '_' + IntToStr(id_smallarrow_left) + '" fill="black" stroke="none"'
        + ' d="M3,0 v6 l-3,-3 z" />');
      try
        ReadXMLFragment( GSymbolDefNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateSymbolSmallArrowRight;
    var S : TStringStream;
        def_id : string;
    begin
      BitMapList.Add(nil);
      id_smallarrow_right := BitmapList.Count -1;

      def_id := idSymbol + '_' + IntToStr(id_smallarrow_right) + '_def';

      GSymbolDefNode := CreateSectionPlaceholder( GSymbolSectionNode, def_id, sc, 0);
      sc := sc + 20;

      S := TStringStream.Create(
        '  <path id="' + idSymbol + '_' + IntToStr(id_smallarrow_right) + '" fill="black" stroke="none"'
        + ' d="M0,0 v6 l3,-3 z" />');
      try
        ReadXMLFragment( GSymbolDefNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateLedSeq( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := idLedOffSequencer;

      bw := 1;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Led sequencer for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="black" stroke="none" x="0" y="0" width="12" height="6" />'
       + '     <rect id="' + id + '_window" fill="green" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(12 - bw*2) + '" height="' + IntToStr(6 - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateLedGreen( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := idLedOffGreen;

      bw := 1;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Led green for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="black" stroke="none" x="0" y="0" width="7" height="7" />'
       + '     <rect id="' + id + '_window" fill="green" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(7 - bw*2) + '" height="' + IntToStr(7 - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function GetIDTxtField( aWidth, aHeight : single): string;
    begin
      Result := idTxtField + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateTextField( aNode : TDomNode; x, y, Width, Height : single);
    var S : TStringStream;
        id : string;
        bw : integer;
    begin
      bw := 1;
      id := GetIDTxtField( Width, Height);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>TextField for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bevel" fill="gray" stroke="none" x="' + FloatToStr(x) + '" y="' + FloatToStr(y) + '" width="' + FloatToStr(Width)+ '" height="' + FloatToStr(Height) + '" />'
       + '     <rect id="' + id + '_window" fill="black" stroke="none" x="' + FloatToStr(x+bw) + '" y="' + FloatToStr(y+bw) + '" width="' + FloatToStr(Width - bw*2) + '" height="' + FloatToStr(Height - bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddTextField( aWidth, aHeight : single);
    var id : string;
    begin
      id := idTxtField + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);

      TextFieldNode := GTextFieldSectionNode.FirstChild;
      while (TextFieldNode <> nil) and (TDomELement(TextFieldNode).GetAttribute('id') <>  id) do
        TextFieldNode := TextFieldNode.NextSibling;

      if not assigned(TextFieldNode) then begin
        GTextFieldDefNode := CreateSectionPlaceholder( GTextFieldSectionNode, id, tfc, 0);
        CreateTextField( GTextFieldDefNode, 0, 0, Control.Width, Control.Height);
        tfc := tfc + Control.Width + 30;
      end;
    end;

    function GetIDBtnText( aBaseName : string; aWidth, aHeight, aBevelWidth : single): string;
    begin
      Result :=  aBaseName + '_' + FloatToStr(trunc(aWidth*10)/10) + 'x' + FloatToStr(trunc(aHeight*10)/10) + 'x' + FloatToStr(trunc(aBevelWidth*10)/10);
    end;

    function NiceButton( aBaseName : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single;
                         aColSideLeft, aColSideTop, aColSideRight, aColSideBottom, aColBG, aColFace : string ): string;
    var id : string;
    begin
       id := GetIDBtnText( aBaseName, aWidth, aHeight, aBevelWidth);
       Result :=
         '<g id="' + id + '">'

       + '  <desc>Button text for G2 editor</desc>'

       + '  <rect id="' + id + '_sel" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="none" stroke="blue" stroke-width="0.2"/>'

       + '  <g id="' + id + '_parts">'

       + '    <rect id="' + id + '_bg" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="' + aColBG + '" stroke="black" stroke-width="0.5"/>'

       + '    <g id="' + id + '_bevel">'
       + NiceBevel( id, aWidth, aHeight, aBevelWidth, aBevelHeight, aColSideLeft, aColSideTop, aColSideRight, aColSideBottom)
       + '    </g>'

       + '    <rect id="' + id + '_face" x="' + FloatToStr(aBevelWidth) + '" y="' + FloatToStr(aBevelHeight) + '" width="' + FloatToStr(aWidth-aBevelWidth*2) + '" height="' + FloatToStr(aHeight-aBevelHeight*2) + '" rx="0.75" fill="' + aColFace + '" stroke="none"/>'

       + '  </g>'
       + '</g>';
    end;

    procedure CreateBtnTextSideGradient( aNode : TDomNode; aID : string; x1, x2, y1, y2 : single; aColor1, aColor2 : string);
    var StopsNode : TDomNode;
        S : TStringStream;
    begin
      StopsNode := CreateGradientNodeForStopPoints( aNode, aID + '_stops');
      CreateGradientPoint( StopsNode, 0, aColor1);
      CreateGradientPoint( StopsNode, 100, aColor2);

      S := TStringStream.Create(
         '<linearGradient'
       + ' id="' + aID + '"'
       + ' gradientUnits="objectBoundingBox"'
       + ' x1="' + FloatToStr(x1) + '"'
       + ' x2="' + FloatToStr(x2) + '"'
       + ' y1="' + FloatToStr(y1) + '"'
       + ' y2="' + FloatToStr(y2) + '"'
       + ' xlink:href="#' +aID + '_stops' + '">'
       + '</linearGradient>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateBtFaceGradient( aNode : TDomNode; aID : string; cx, cy, fx, fy, r : single; aColor1, aColor2 : string);
    var StopsNode : TDomNode;
        S : TStringStream;
    begin
      StopsNode := CreateGradientNodeForStopPoints( aNode, aID + '_stops');
      CreateGradientPoint( StopsNode, 0, aColor1);
      CreateGradientPoint( StopsNode, 100, aColor2);

      S := TStringStream.Create(
         '<radialGradient'
       + ' id="' + aID + '"'
       + ' gradientUnits="objectBoundingBox"'
       + ' cx="' + FloatToStr(cx) + '"'
       + ' cy="' + FloatToStr(cy) + '"'
       + ' fx="' + FloatToStr(fx) + '"'
       + ' fy="' + FloatToStr(fy) + '"'
       + ' r="' + FloatToStr(r) + '"'
       + ' xlink:href="#' +aID + '_stops' + '">'
       + '</radialGradient>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateButtonTextUp( aNode : TDomNode; aWidth, aHeight, aBevelWidth : single);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      {S := TStringStream.Create(
         '<g id="g2_buttontext_' + IntToStr(Width) + 'x' + IntToStr(Height) + '">'
       + '  <desc>Button text for G2 editor</desc>'
       + '  <rect id="g2_buttontext_sel" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '  <g id="g2_buttontext_parts">'
       + '    <rect id="g2_buttontext_face" x="0" y="0" width="' + IntTostr(Width) + '" height="' + IntToStr(Height) + '" fill="lightgray" stroke="none"/>'
       + '    <g id="g2_buttontext_bevel">'
       + Bevel( 'g2_buttontext', Width, Height, bw, 'white', 'darkgray')
       + '    </g>'
       + '  </g>'
       + '</g>');}
       S := TStringStream.Create( NiceButton( idBtnText + '_up', aWidth, aHeight, aBevelWidth, aBevelWidth,
                  cBtnSideLight, cBtnSideLight, cBtnSideDark, cBtnSideDark, cBtnSideMedium, cBtnFace));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateButtonTextDown( aNode : TDomNode; aWidth, aHeight, aBevelWidth : single);
    var S : TStringStream;
    begin
       S := TStringStream.Create( NiceButton( idBtnText + '_down', aWidth, aHeight, aBevelWidth, aBevelWidth,
                  'url(#btnTextGradienSideLeft)', 'url(#btnTextGradienSideTop)', 'url(#btnTextGradienSideRight)', 'url(#btnTextGradienSideBottom)',
                  '#00ffcc', 'url(#btnTextGradienFace)'));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    {procedure FindOrAddButtonTextUp( aWidth, aHeight, aBevelWidth : single);
    var id : string;
    begin
      id := idBtnText + '_up' + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth);

      ButtonTextUpNode := GButtonTextUpSectionNode.FirstChild;
      while (ButtonTextUpNode <> nil) and (TDomELement(ButtonTextUpNode).GetAttribute('id') <> id) do
        ButtonTextUpNode := ButtonTextUpNode.NextSibling;

      if not assigned(ButtonTextUpNode) then begin
        GButtonTextUpDefNode := CreateSectionPlaceholder( GButtonTextUpSectionNode, id, btc, 0);
        CreateButtonTextUp( GButtonTextUpDefNode, aWidth, aHeight, aBevelWidth);


        GButtonTextDownDefNode := CreateSectionPlaceholder( GButtonTextDownSectionNode, id, btc, 0);
        CreateButtonTextDown( GButtonTextDownDefNode, aWidth, aHeight, aBevelWidth);

        btc := btc + trunc(aWidth) + 20;
      end;
    end;}

    procedure FindOrAddButtonText( aWidth, aHeight, aBevelWidth : single);
    var id : string;
        GroupNode : TDomNode;
    begin
      id := idBtnText + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth);

      ButtonTextNode := GButtonTextSectionNode.FirstChild;
      while (ButtonTextNode <> nil) and (TDomELement(ButtonTextNode).GetAttribute('id') <> id) do
        ButtonTextNode := ButtonTextNode.NextSibling;

      if not assigned(ButtonTextNode) then begin
        GButtonTextUpDefNode := CreateSectionPlaceholder( GButtonTextUpSectionNode, id, btc, 0);
        CreateButtonTextUp( GButtonTextUpDefNode, aWidth, aHeight, aBevelWidth);

        GButtonTextDownDefNode := CreateSectionPlaceholder( GButtonTextDownSectionNode, id, btc, 0);
        CreateButtonTextDown( GButtonTextDownDefNode, aWidth, aHeight, aBevelWidth);

        GButtonTextDefNode := CreateSectionPlaceholder( GButtonTextSectionNode, id, btc, 0);

        GroupNode := Doc.CreateElement('g');
        TDOMElement(GroupNode).SetAttribute('id', idBtnText + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth));
        GButtonTextDefNode.AppendChild(GroupNode);
        CreateUse( GroupNode,
                   id + '_el_1',
                   idBtnText + '_up' + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth),
                   0, 0);
        CreateUse( GroupNode,
                   id + '_el_2',
                   idBtnText + '_down' + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth),
                   0, 0);

        btc := btc + trunc(aWidth) + 20;
      end;
    end;

    {procedure FindOrAddButtonTextDown( aWidth, aHeight, aBevelWidth : single);
    var id : string;
    begin
      id := idBtnText + '_down' + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight) + 'x' + FloatToStr(aBevelWidth);

      ButtonTextDownNode := GButtonTextDownSectionNode.FirstChild;
      while (ButtonTextDownNode <> nil) and (TDomELement(ButtonTextDownNode).GetAttribute('id') <> id) do
        ButtonTextDownNode := ButtonTextDownNode.NextSibling;

      if not assigned(ButtonTextDownNode) then begin
        GButtonTextDownDefNode := CreateSectionPlaceholder( GButtonTextDownSectionNode, id, btc, 0);
        CreateButtonTextDown( GButtonTextDownDefNode, aWidth, aHeight, aBevelWidth);
        btc := btc + trunc(aWidth) + 20;
      end;
    end;}

    procedure CreateBtnIncDecHorz( aNode : TDomNode);
    var S : TStringStream;
        w, h, d : single;
    begin
      w := 11;
      h := 11;
      d := 1;
      FindOrAddButtonText(w, h, d);

      S := TStringStream.Create(
         '<g id="' + idBtnIncDecHorz + '">'
        + ' <desc>Button inc-dec horizontal for G2 editor</desc>'
          + ' <g id="' + idBtnIncDecHorz + '_parts">'

               + Use( idBtnIncDecHorz + '_dec', GetIDBtnText( idBtnText {+ '_up'}, w, h, d), 0, 0)

               + ' <use id="' + idBtnIncDecHorz + '_' + IntToStr(id_smallarrow_left) + '_symbol' + '"'
                  + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_left) + '"'
                  + ' transform="translate(' + FloatToStr(w/2 - 3/2) + ',' + FloatToStr(h/2 - 6/2) + ')"'
                  + ' x="0" y="0" />'

              + Use( idBtnIncDecHorz + '_inc', GetIDBtnText( idBtnText {+ '_up'}, w, h, d), w, 0)

               + ' <use id="' + idBtnIncDecHorz + '_' + IntToStr(id_smallarrow_right) + '_symbol' + '"'
                  + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_right) + '"'
                  + ' transform="translate(' + FloatToStr(w + w/2 - 3/2) + ',' + FloatToStr(h/2 - 6/2) + ')"'
                  + ' x="0" y="0" />'
       + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateBtnIncDecVert( aNode : TDomNode);
    var S : TStringStream;
        w, h, d : single;
    begin
      w := 11;
      h := 9;
      d := 1;
      FindOrAddButtonText(w, h, d);

      S := TStringStream.Create(
         '<g id="' + idBtnIncDecVert + '">'
       + ' <desc>Button inc-dec vertical for G2 editor</desc>'
         + ' <g id="g2_btnIncDecVert_parts">'

               + Use( idBtnIncDecVert + '_dec', GetIDBtnText( idBtnText {+ '_up'}, w, h, d), 0, 0)

               + ' <use id="' + idBtnIncDecHorz + '_' + IntToStr(id_smallarrow_up) + '_symbol' + '"'
                  + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_up) + '"'
                  + ' transform="translate(' + FloatToStr(w/2 - 6/2) + ',' + FloatToStr(h/2 - 3/2) + ')"'
                  + ' x="0" y="0" />'

               + Use( idBtnIncDecVert + '_inc', GetIDBtnText( idBtnText {+'_up'}, w, h, d), 0, h)

               + ' <use id="' + idBtnIncDecHorz + '_' + IntToStr(id_smallarrow_down) + '_symbol' + '"'
                  + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_down) + '"'
                  + ' transform="translate(' + FloatToStr(w/2 - 6/2) + ',' + FloatToStr(h + h/2 - 3/2) + ')"'
                  + ' x="0" y="0" />'

       + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function GetIDBtnFlat( aWidth, aHeight : single): string;
    begin
      Result := idBtnFlat + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateButtonFlat( aNode : TDomNode; aWidth, aHeight : integer);
    var S : TStringStream;
        rx, bw : single;
        id : string;
    begin
      bw := 1;
      rx := 1;
      id := GetIDBtnFlat( aWidth, aHeight);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Button flat for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bg" fill="black" stroke="none" x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'
       + '     <rect id="' + id + '_face" fill="' + cBtnFlatFace + '" stroke="none" rx="' + FloatToStr(rx) + '" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddButtonFlat( aWidth, aHeight : integer);
    var id : string;
    begin
      id := idBtnFlat + '_def_'  + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

       ButtonFlatNode := GButtonFlatSectionNode.FirstChild;
       while (ButtonFlatNode <> nil) and (TDomELement(ButtonFlatNode).GetAttribute('id') <>  id) do
         ButtonFlatNode := ButtonFlatNode.NextSibling;

       if not assigned(ButtonFlatNode) then begin
         GButtonFlatDefNode := CreateSectionPlaceholder( GButtonFlatSectionNode, id, bfc, 0);
         CreateButtonFlat( GButtonFlatDefNode, aWidth, aHeight);
         bfc := bfc + aWidth + 30;
       end;
    end;

    function GetIDBtnFlatOption( aWidth, aHeight : single; aParamID : integer): string;
    begin
      Result := idBtnFlat + 'Options_' + IntToStr(aParamID) + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateButtonFlatOptions( aNode : TDomNode; aWidth, aHeight : integer; aBtnFlat : TG2GraphButtonFlat);
    var S : TStringStream;
        SVGText, txt : string;
        ParamID, symbol_id, o, tw, th : integer;
        id : string;
        ParamDef : TXMLParamDefType;
        dx, dy, w, h : single;
    begin
      {ParamDef := G2_module_def.FParamDefList.ParamDef[ aParamID];
      if not assigned(ParamDef) then
        exit;}
      ParamID := aBtnFlat.Parameter.ParamID;
      id := GetIDBtnFlatOption( aWidth, aHeight, ParamID);

      SVGText :=
         '<g id="' + id + '">'
       + '  <desc>Button flat options for G2 editor</desc>'
       + '  <g id="' + id + '_parts">';

      dx := 0;
      dy := 0;
      if (aBtnFlat.ImageList.Count > 0) then begin
        for o := 0 to aBtnFlat.ImageList.Count - 1 do begin

          symbol_id := AddSymbolFromBitmap( aBtnFlat.ImageList.Items[o]);

          w := (BitmapList[symbol_id] as TBitmap).Width;
          h := (BitmapList[symbol_id] as TBitmap).Height;

          SVGText := SVGText
          + '<g id="' + id + '_' + IntToStr(o) + '">'
            + '<use id="' + id + '_' + IntToStr(o) + '_btn' + '"'
              + ' xlink:href="#' + GetIDBtnFlat( Control.Width, Control.Height) + '"'
              + ' transform="translate(' + FloatToStr(dx) + ',' + FloatToStr(dy) + ')"'
              + ' x="0" y="0" />'

            + '<use id="' + id + '_' + IntToStr(o) + '_symbol' + '"'
              + ' xlink:href="#' + idSymbol + '_' + IntToStr(symbol_id) + '"'
              + ' transform="translate(' + FloatToStr( dx + aWidth/2 - w/2) + ',' + FloatToStr( dy + aHeight/2 - h/2) + ')"'
              + ' x="0" y="0" />'
          + '</g>';

          dy := dy + aHeight;

        end;
      end else begin
        for o := 0 to aBtnFlat.ButtonText.Count - 1 do begin


          txt := aBtnFlat.ButtonText[o];


          TextBitmap.Canvas.Font.Name := 'Arial';

          TextBitmap.Canvas.Font.Size := 7;

          tw := TextBitmap.Canvas.TextWidth( trim(txt)) - 9;

          //th := TextBitmap.Canvas.TextHeight( txt);

          th := 5;


          SVGText := SVGText
          + '<g id="' + id + '_' + IntToStr(o) + '">'
            + '<use id="' + id + '_' + IntToStr(o) + '_btn' + '"'
              + ' xlink:href="#' + GetIDBtnFlat( aWidth, aHeight) + '"'
              + ' transform="translate(' + FloatToStr(dx) + ',' + FloatToStr(dy) + ')"'
              + ' x="0" y="0" />'

            + '<text id="' + id + '_' + IntToStr(o) + '_text' + '"'
              + ' style="font-size:7px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Arial" xml:space="preserve"'
              + ' transform="translate(' + FloatToStr( aWidth/2 - tw/2) + ',' + FloatToStr( dy + aHeight/2 + th/2) + ')"'
              + ' x="0" y="0">'
            + txt
            + '</text>'

          + '</g>';

          dy := dy + aHeight;

        end;
      end;

      SVGText := SVGText
       + '  </g>'
       + '</g>';

      S := TStringStream.Create( SVGText);
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddButtonFlatOptions( aWidth, aHeight : integer; aBtnFlat : TG2GraphButtonFlat);
    var id : string;
        ParamID : integer;
    begin
      FindOrAddButtonFlat( Control.Width, Control.Height);

      //for m := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
      //  AddSymbolFromBitmap( (Control as TG2GraphButton).ImageList.Items[m]);
      //end;

      ParamID := aBtnFlat.Parameter.ParamID;

      id := idBtnFlat + 'Options_def_' + IntToStr(ParamID) + '_' + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

      ButtonFlatOptionsNode := GButtonFlatOptionsSectionNode.FirstChild;
      while (ButtonFlatOptionsNode <> nil) and (TDomELement(ButtonFlatOptionsNode).GetAttribute('id') <>  id) do
        ButtonFlatOptionsNode := ButtonFlatOptionsNode.NextSibling;

      if not assigned(ButtonFlatOptionsNode) then begin
        GButtonFlatOptionsDefNode := CreateSectionPlaceholder( GButtonFlatOptionsSectionNode, id, bfoc, 0);
        CreateButtonFlatOptions( GButtonFlatOptionsDefNode, aWidth, aHeight, aBtnFlat);
        bfoc := bfoc + aWidth + 30;
      end;
    end;


    function GetIDBtnRadio( aBaseID : string; aWidth, aHeight : single): string;
    begin
      Result := aBaseID + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateBtnRadioUp( aNode : TDomNode; aWidth, aHeight : single);
    var S : TStringStream;
        bw : single;
        id : string;
    begin
      bw := 2;
      id := GetIDBtnRadio( idBtnRadio + '_up', aWidth, aHeight);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Button radio up for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bg" fill="black" stroke="none" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" />'
       + Bevel( id, aWidth, aHeight, bw, '#e6e6e6', '#e6e6e6', '#e6e6e6', '#e6e6e6')
       + '     <rect id="' + id + '_face" fill="url(#btnRadioUpGradientFace)" stroke="none" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateBtnRadioDown( aNode : TDomNode; aWidth, aHeight : single);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      bw := 2;
      id := GetIDBtnRadio( idBtnRadio + '_down', aWidth, aHeight);
      {S := TStringStream.Create(
         '<g id="' + id + '">'
        + ' <desc>Button radio down for G2 editor</desc>'
          + ' <g id="' + id + '_parts">'
            + ' <rect id="' + id + '_bg" fill="black" stroke="none" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" />'
                  + Bevel( id, aWidth, aHeight, bw, 'url(#btnRadioGradienSideLeft)', 'url(#btnRadioGradienSideTop)', 'url(#btnRadioGradienSideRight)', 'url(#btnRadioGradienSideBottom)')
            + ' <rect id="' + id + '_face" fill="url(#btnRadioGradienFace)" stroke="none" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" />'
       + ' </g>'
       + '</g>');}
      S := TStringStream.Create(
         '<g id="' + id + '">'
        + ' <desc>Button radio down for G2 editor</desc>'
          + ' <g id="' + id + '_parts">'
            + ' <rect id="' + id + '_bg" fill="black" stroke="none" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" />'
                  + Bevel( id, aWidth, aHeight, bw, 'url(#btnTextGradienSideLeft)', 'url(#btnTextGradienSideTop)', 'url(#btnTextGradienSideRight)', 'url(#btnTextGradienSideBottom)')
            + ' <rect id="' + id + '_face" fill="url(#btnTextGradienFace)" stroke="none" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" />'
       + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddBtnRadio( aWidth, aHeight : single);
    var id : string;
    begin
      id := idBtnRadio + '_up' + '_def_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);

      btnRadioUpNode := GBtnRadioUpSectionNode.FirstChild;
      while (btnRadioUpNode <> nil) and (TDomELement(btnRadioUpNode).GetAttribute('id') <>  id) do
        btnRadioUpNode := btnRadioUpNode.NextSibling;

      if not assigned(btnRadioUpNode) then begin
        GBtnRadioUpDefNode := CreateSectionPlaceholder( GBtnRadioUpSectionNode, id, rbc, 0);
        CreateBtnRadioUp( GBtnRadioUpDefNode, aWidth, aHeight);

        GBtnRadioDownDefNode := CreateSectionPlaceholder( GBtnRadioDownSectionNode, id, rbc, 0);
        CreateBtnRadioDown( GBtnRadioDownDefNode, aWidth, aHeight);

        rbc := rbc + trunc(aWidth) + 30;
      end;
    end;

    procedure CreateLevelShift( aNode : TDomNode; aWidth, aHeight : single);
    var S : TStringStream;
        bw : single;
        id : string;
    begin
      bw := 1;
      id := idLevelShift;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Level shiftfor G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bg" fill="black" stroke="none" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" />'
       + '     <rect id="' + id + '_face" fill="white" stroke="none" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" rx="' + FloatToStr(bw) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    function GetIDPartSel( aWidth, aHeight : single): string;
    begin
      Result := idPartSel + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreatePartSelector( aNode : TDomNode; aWidth, aHeight : integer);
    var S : TStringStream;
        bw : integer;
        id : string;
    begin
      id := GetIDPartSel( aWidth, aHeight);
      bw := 9;
      S := TStringStream.Create(
         '<g id="' + id + '">'
        + ' <desc>Part selector for G2 editor</desc>'
          + ' <g id="' + id + '_parts">'
            + ' <rect id="' + id + '_window" fill="' + cPartselWindow + '" stroke="black" x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'
            + ' <rect id="' + id + '_button" fill="' + cPartselBtn + '" stroke="black" x="' +  IntToStr(aWidth - bw) + '" y="0" width="' + IntToStr(bw) + '" height="' + IntToStr(aHeight) + '" />'
            + ' <use id="' + id + '_' + IntToStr(id_smallarrow_down) + '_symbol' + '"'
              + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_down) + '"'
              + ' transform="translate(' + FloatToStr((aWidth-bw) + bw/2 - 6/2) + ',' + FloatToStr( aHeight/2 - 3/2) + ')"'
              + ' x="0" y="0" />'

        + ' </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddPartSel( aWidth, aHeight : integer);
    var id : string;
    begin
      id := idPartsel + '_def_'  + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

       PartSelNode := GPartSelSectionNode.FirstChild;
       while (PartSelNode <> nil) and (TDomELement(PartSelNode).GetAttribute('id') <>  id) do
         PartSelNode := PartSelNode.NextSibling;

       if not assigned(PartSelNode) then begin
         GPartSelDefNode := CreateSectionPlaceholder( GPartSelSectionNode, id, psc, 0);
         CreatePartSelector( GPartSelDefNode, aWidth, aHeight);
         psc := psc + aWidth + 30;
       end;
    end;

    function GetIDPartSelOption( aWidth, aHeight : single; aParamID : integer): string;
    begin
      Result := idPartSel + 'Options_' + IntToStr(aParamID) + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreatePartSelOptions( aNode : TDomNode; aWidth, aHeight : integer; aPartSel : TG2GraphPartSelector);
    var S : TStringStream;
        SVGText : string;
        ParamID, symbol_id, o : integer;
        id : string;
        dx, dy, w, h : single;
    begin
      ParamID := aPartSel.Parameter.ParamID;
      id := GetIDBtnFlatOption( aWidth, aHeight, ParamID);

      SVGText :=
         '<g id="' + id + '">'
       + '  <desc>Button flat options for G2 editor</desc>'
       + '  <g id="' + id + '_parts">';

      dx := 0;
      dy := 0;
      if (aPartSel.ImageList.Count > 0) then begin
        for o := 0 to aPartSel.HighValue do begin

          symbol_id := AddSymbolFromBitmap( aPartSel.ImageList.Items[o]);

          w := (BitmapList[symbol_id] as TBitmap).Width;
          h := (BitmapList[symbol_id] as TBitmap).Height;

          SVGText := SVGText
          + '<g id="' + id + '_' + IntToStr(o) + '">'
            + '<rect id="' + id + '_window"'
              + ' transform="translate(' + FloatToStr(dx) + ',' + FloatToStr(dy) + ')"';

          if o = 0 then
            SVGText := SVGText
              + ' fill="' + cPartselSelected + '"'
          else
            SVGText := SVGText
              + ' fill="' + cPartselWindow + '"';

          SVGText := SVGText
              + ' stroke="black"'
              + ' x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'

            + '<use id="' + id + '_' + IntToStr(o) + '_symbol' + '"'
              + ' xlink:href="#' + idSymbol + '_' + IntToStr(symbol_id) + '"'
              + ' transform="translate(' + FloatToStr( dx + aWidth/2 - w/2) + ',' + FloatToStr( dy + aHeight/2 - h/2) + ')"'
              + ' x="0" y="0" />'
          + '</g>';

          dy := dy + aHeight;

        end;
      end;

      SVGText := SVGText
       + '  </g>'
       + '</g>';

      S := TStringStream.Create( SVGText);
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddPartSelOptions( aWidth, aHeight : integer; aPartSel : TG2GraphPartSelector);
    var id : string;
        ParamID : integer;
    begin
      ParamID := aPartSel.Parameter.ParamID;

      id := idPartSel + 'Options_def_' + IntToStr(ParamID) + '_' + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

      PartSelOptionsNode := GPartSelOptionsSectionNode.FirstChild;
      while (PartSelOptionsNode <> nil) and (TDomELement(PartSelOptionsNode).GetAttribute('id') <>  id) do
        PartSelOptionsNode := PartSelOptionsNode.NextSibling;

      if not assigned(PartSelOptionsNode) then begin
        GPartSelOptionsDefNode := CreateSectionPlaceholder( GPartSelOptionsSectionNode, id, psoc, 0);
        CreatePartSelOptions( GPartSelOptionsDefNode, aWidth, aHeight, aPartSel);
        psoc := psoc + aWidth + 30;
      end;
    end;

    function GetIDGraph( aWidth, aHeight : single): string;
    begin
      Result := idGraph + '_' + FloatToStr(aWidth) + 'x' + FloatToStr(aHeight);
    end;

    procedure CreateGraph( aNode : TDomNode; aWidth, aHeight : integer);
    var S : TStringStream;
        id : string;
    begin
      id := GetIDGraph( aWidth, aHeight);
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Graph for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_window" fill="blue" stroke="black" x="0" y="0" width="' + IntToStr(aWidth) + '" height="' + IntToStr(aHeight) + '" />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure FindOrAddGraph( aWidth, aHeight : integer);
    var id : string;
    begin
      id := idGraph + '_def_'  + IntToStr(aWidth) + 'x' + IntToStr(aHeight);

       GraphNode := GGraphSectionNode.FirstChild;
       while (GraphNode <> nil) and (TDomELement(GraphNode).GetAttribute('id') <>  id) do
         GraphNode := GraphNode.NextSibling;

       if not assigned(GraphNode) then begin
         GGraphDefNode := CreateSectionPlaceholder( GGraphSectionNode, id, gc, 0);
         CreateGraph( GGraphDefNode, aWidth, aHeight);
         gc := gc + aWidth + 30;
       end;
    end;

    function Gradient( id : string; x1, y1, x2, y2 : single; Color1, Color2 : string ): string;
    begin
      Result :=
        '<linearGradient id="' + id + '" gradientUnits="userSpaceOnUse" x1="' + FloatToStr(x1) + '" y1="' + FloatToStr(y1) + '" x2="' + FloatToStr(x2) + '" y2="' + FloatToStr(y2) + '" >'
      + ' <stop style="stop-color:' + Color1 + ';stop-opacity:1" offset="0" id="' + id + 'stop0" />'
      + ' <stop style="stop-color:' + Color2 + ';stop-opacity:1" offset="1" id="' + id + 'stop1" />'
      + ' </linearGradient>';
    end;

    procedure CreateKnobSideGradient( aNode : TDomNode; r : single);
    var id : string;
        S : TStringStream;
    begin
      id := 'knobSideGradient';
      S := TStringStream.Create( Gradient( id, r/2, r/2, -r/2, -r/2, '#333333', '#ffffff'));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnob( aNode : TDomNode; id : string; w, h, c_x, c_y, r_side, r_face : single; reset : boolean);
    var S : TStringStream;
        svg : string;
    begin
      svg :=
         '<g id="' + id + '">'
       + '  <desc>Knob for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="' + FLoatToStr(w) + '" height="' + FloatToStr(h) + '" fill="none" stroke="blue" stroke-width="0.2"/>';

       if reset then begin
         svg := svg
       {+ '  <g>'
       + '    <path id="' + id + '_cntrBtn" d="M' + FloatToStr((w - 10)/2) + ',0 h10 l -5,4 z" fill="lime" stroke="black" opacity="1" />'
       + '  </g>';}
         + '<use id="' + idKnobCenterBtn + '_use"'
              + ' xlink:href="#' + idKnobCenterBtn + '_on"'
              + ' transform="translate(' + FloatToStr( c_x - 5) + ',' + FloatToStr(0) + ')"'
              + ' x="0" y="0" />'
       end;

       svg := svg
       + '  <g transform="translate(' + FloatToStr(c_x) + ',' + FloatToStr(c_y) + ')">'
       + '    <g id="g' + id + '_face">'
       + '      <circle id="' + id + '_side" cx="0" cy="0" r="' + FloatToStr(r_side) + '" fill="url(#knobSideGradient)" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '      <circle id="' + id + '_face" cx="0" cy="0" r="' + FloatToStr(r_face) + '" fill="' + cKnobFace + '" stroke="none"/>'
       + '    </g>'
       + '    <g id="g' + id + '_needle">'
       + '      <rect id="' + id + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="' + FloatToStr(-(r_face-1)) + '" width="0.2" height="' + FloatToStr(r_face-1) + '" />'
       + '    </g>'
       + '    <g id="g' + id + '_morph">'
       + '      <path id="' + id + '_morph" d="M0,0 v' + FLoatToStr(-r_side) + ' a' + FloatToStr(r_side) + ',' + FLoatToStr(r_side) + ' 0 0,0 ' + FloatToStr(-r_side) + ',' + FloatToStr(r_side) + ' z" fill="red" stroke="none" opacity="0.5" />'
       + '    </g>'
       + '  </g>'

         + '<use id="' + idKnobBtns + '_use"'
              + ' xlink:href="#' + idKnobBtns + '"'
              + ' transform="translate(' + FloatToStr( c_x - 10.5) + ',' + FloatToStr(h - 9) + ')"'
              + ' x="0" y="0" />'

       + '</g>';

      S := TStringStream.Create(svg);
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobBig( aNode : TDomNode);
    var id : string;
        w, h, c_x, c_y, r_side, r_face : single;
    begin
      // url(#linearGradient26483)

      w := 22;
      h := 26;
      r_side := 11;
      r_face := 8;
      c_x := 11;
      c_y := 11;

      id := idKnobBig;

      CreateKnob( aNode, id, w, h, c_x, c_y, r_side, r_face, false);
    end;

    procedure CreateKnobMedium( aNode : TDomNode);
    var id : string;
    begin
      id := idKnobMedium;
      CreateKnob( aNode, id, 20, 24, 10, 10, 10, 7, false);
    end;

    procedure CreateKnobResetMedium( aNode : TDomNode);
    var id : string;
    begin
      id := idKnobResetMedium;
      CreateKnob( aNode, id, 20, 30, 10, 15, 10, 7, true);
    end;

    procedure CreateKnobReset( aNode : TDomNode);
    var id : string;
    begin
      id := idKnobReset;
      CreateKnob( aNode, id, 18, 26, 9, 14, 9, 6, true);
    end;

    procedure CreateKnobSmall( aNode : TDomNode);
    var id : string;
    begin
      id := idKnobSmall;
      CreateKnob( aNode, id, 18, 22, 9, 9, 9, 6, false);
    end;

    procedure CreateKnobButtons( aNode : TDomNode);
    var S : TStringStream;
        id, temp : string;
        w, h, bw : single;
    begin
      w := 11;
      h := 9;
      bw := 1;
      temp :=
      '<g id="' + idKnobBtns + '">'
        + '<rect id="' + idKnobBtns + '_bg" fill="white" stroke="none"'
             + ' x="0" y="0"  width="' + FloatToStr(w*2-bw) + '" height="' + FloatToStr(h) + '"/>'

        + '<rect id="' + idKnobBtns + '_left" fill="' + cBtnFlatFace + '" stroke="none"'
             + ' x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(w - bw*2) + '" height="' + FloatToStr(h - bw*2) + '"/>'

        + ' <use id="' + idKnobBtns + '_' + IntToStr(id_smallarrow_left) + '_symbol' + '"'
             + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_down) + '"'
             + ' transform="translate(' + FloatToStr(w/2 - 6/2) + ',' + FloatToStr(h/2 - 3/2) + ')"'
             + ' x="0" y="0" />'

        +  '<rect id="' + idKnobBtns + '_right" fill="' + cBtnFlatFace + '" stroke="none"'
             + ' x="' + FloatToStr(w) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(w - bw*2) + '" height="' + FloatToStr(h - bw*2) + '"/>'

        + ' <use id="' + idKnobBtns + '_' + IntToStr(id_smallarrow_up) + '_symbol' + '"'
             + ' xlink:href="#' + idSymbol + '_' + IntToStr(id_smallarrow_up) + '"'
             + ' transform="translate(' + FloatToStr(w - bw + w/2 - 6/2) + ',' + FloatToStr(h/2 - 3/2) + ')"'
             + ' x="0" y="0" />'
       + '</g>';
      S := TStringStream.Create( temp);
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateCenterBtnOff( aNode : TDomNode);
    var S : TStringStream;
        id : string;
        w : single;
    begin
      w := 10;
      S := TStringStream.Create(
         '<g id="' + idKnobCenterBtn + '_off">'
       + '  <path id="' + idKnobCenterBtn + '_off_bg" d="M' + FloatToStr((w - 10)/2) + ',0 h10 l -5,4 z" fill="gray" stroke="black" opacity="1" />'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateCenterBtnOn( aNode : TDomNode);
    var S : TStringStream;
        id : string;
        w : single;
    begin
      w := 10;
      S := TStringStream.Create(
         '<g id="' + idKnobCenterBtn + '_on">'
       + '  <path id="' + idKnobCenterBtn + '_on_bg" d="M' + FloatToStr((w - 10)/2) + ',0 h10 l -5,4 z" fill="lime" stroke="black" opacity="1" />'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobSliderKnob( aNode : TDomNode; aWidth, aHeight, aBevelWidth, aBevelHeight : single);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create( NiceButton( idSlider + '_knob', aWidth, aHeight, aBevelWidth, aBevelHeight,
                  cSldrKnobSideLight, cSldrKnobSideLight, cSldrKnobSideDark, cSldrKnobSideDark, cSldrKnobSideMedium, cSldrKnobFace));
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateKnobSlider( aNode : TDomNode);
    var S : TStringStream;
        bw : integer;
    begin
      bw := 1;
      S := TStringStream.Create(
         '<g id="' + idSlider + '">'
       + '  <desc>Knob slider for G2 editor</desc>'
       + '  <g id="' + idSlider + '_parts">'
       + '     <rect id="' + idSlider + '_bevel" fill="gray" stroke="none" x="0" y="0" width="11" height="45" />'
       + '     <rect id="' + idSlider + '_face" fill="lightgray" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(11 - bw*2) + '" height="' + IntToStr(45 - bw*2) + '" />'
       //+ '     <rect id="' + idSlider + '_btn" fill="gray" stroke="none" x="0" y="40" width="11" height="5" />'
       + Use( idSlider + '_knob_use', idSlider + '_knob', 0, 0)
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateInConnector( aNode : TDomNode; aID : string; aColor : string);
    var S : TStringStream;
        id : string;
    begin
      id := aID;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Connector for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="10" height="10" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '   <g transform="translate(5,5)">'
       + '     <g id="' + id + '_parts">'
       + '       <circle id="' + id + '_border" cx="0" cy="0" r="5" fill="' + aColor + '" stroke="none" opacity="1"  />'
       + '       <circle id="' + id + '_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
       + '     </g>'
       + '   </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateOutConnector( aNode : TDomNode; aID : string; aColor : string);
    var S : TStringStream;
        id : string;
    begin
      id := aID;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Connector for G2 editor</desc>'
       + '  <rect id="' + id + '_sel" x="0" y="0" width="10" height="10" fill="none" stroke="blue" stroke-width="0.2"/>'
       + '   <g transform="translate(5,5)">'
       + '     <g id="' + id + '_parts">'
       + '       <rect id="' + id + '_border" x="-5" y="-5" width="10" height="10" fill="' + aColor + '" stroke="none" opacity="1" />'
       + '       <circle id="' + id + '_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
       + '     </g>'
       + '   </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

    procedure CreateModule( aNode : TDomNode);
    var GModuleNode, ParamLinkNode, ConnLinkNode, DescNode, PanelNode, UseNode : TDOMNode;
        l, m, n, x, y, w, h, dx, dy, symbol_id, ParamID : integer;
        module_id, control_id, ref_id : string;
        RadioButton : TG2GraphButtonRadio;
        RadioButtonEdit : TG2GraphButtonRadioEdit;
        Connector : TG2GraphConnector;
    begin
      module_id := idModule + '_' + IntToStr(FModule.TypeID);

      GModuleNode := Doc.CreateElement('g');
      aNode.AppendChild(GModuleNode);
      TDOMElement(GModuleNode).SetAttribute('id', module_id);

      DescNode := Doc.CreateElement('desc');
      GModuleNode.AppendChild(DescNode);
      DescNode.AppendChild(Doc.CreateTextNode('Module ' + IntToStr(FModule.TypeID) + ', ' + FModule.ModuleName));

      PanelNode := Doc.CreateElement('rect');
      GModuleNode.AppendChild(PanelNode);
      TDOMElement(PanelNode).SetAttribute('id', module_id + '_panel_' + IntToStr(FModule.TypeID));
      TDOMElement(PanelNode).SetAttribute('fill', 'url(#PanelGradient_0)');
      TDOMElement(PanelNode).SetAttribute('stroke', 'black');
      TDOMElement(PanelNode).SetAttribute('x', '0');
      TDOMElement(PanelNode).SetAttribute('y', '0');
      TDOMElement(PanelNode).SetAttribute('width', IntToStr(FModule.Panel.Width));
      TDOMElement(PanelNode).SetAttribute('height', IntToStr(FModule.Panel.Height));

      CreateLabel( GModuleNode, 1, 1, FModule.ModuleName, 10);

      for l := 0 to FModule.Panel.ChildControlsCount - 1 do begin

        control_id := module_id + '_ctrl_' + IntToStr(l);

        Control :=  FModule.Panel.GraphChildControls[l];

        if Control is TG2GraphLabel then begin
          CreateLabel( GModuleNode, Control.Left, Control.Top, (Control as TG2GraphLabel).Caption, (Control as TG2GraphLabel).Font.Size);
        end;

        if Control is TG2GraphDisplay then begin
          FindOrAddTextField( Control.Width, Control.Height);

          CreateUse( GModuleNode, control_id, GetIDTxtField(Control.Width, Control.Height), Control.Left, Control.Top);
        end;

        if Control is TG2GraphButtonText then begin

          FindOrAddButtonText( Control.Width, Control.Height, 2);

          ParamLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ParamLinkNode);
          TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnText');

          UseNode := CreateUse( ParamLinkNode, control_id, GetIDBtnText( idBtnText {+ '_up'}, Control.Width, Control.Height, 2), Control.Left, Control.Top);

          for m := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
            symbol_id := AddSymbolFromBitmap( (Control as TG2GraphButton).ImageList.Items[m]);
            w := (BitmapList[symbol_id] as TBitmap).Width;
            h := (BitmapList[symbol_id] as TBitmap).Height;

            CreateUse( ParamLinkNode, control_id + '_symbol_' + IntToStr(m), idSymbol + '_' + IntToStr(symbol_id), Control.Left + Control.Width div 2 - w div 2, Control.Top + Control.Height div 2 - h div 2);
          end;
        end;

        if Control is TG2GraphButtonIncDec then begin

          if (Control as TG2GraphButtonIncDec).Orientation = otHorizontal then begin

            ParamLinkNode := Doc.CreateElement('g');
            GModuleNode.AppendChild(ParamLinkNode);
            TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnIncDec');

            CreateUse( ParamLinkNode, control_id, idBtnIncDecHorz, Control.Left, Control.Top);
          end else begin
            ParamLinkNode := Doc.CreateElement('g');
            GModuleNode.AppendChild(ParamLinkNode);
            TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnIncDec');

            CreateUse(ParamLinkNode, control_id, idBtnIncDecVert, Control.Left, Control.Top);
          end;
        end;

        if Control is TG2GraphButtonFlat then begin

          if Control is TG2GraphLevelShift then begin
            CreateUse( GModuleNode, control_id, idLevelShift, Control.Left, Control.Top);
          end else begin
            ParamID := Control.Parameter.ParamID;

            FindOrAddButtonFlatOptions( Control.Width, Control.Height, Control as TG2GraphButtonFlat);

            ParamLinkNode := Doc.CreateElement('g');
            GModuleNode.AppendChild(ParamLinkNode);
            TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnFlat');

            CreateUse( ParamLinkNode, control_id, GetIDBtnFlatOption( Control.Width, Control.Height, ParamID) + '_0' , Control.Left, Control.Top);
          end;
        end;

        if Control is TG2GraphButtonRadio then begin
          RadioButton := Control as TG2GraphButtonRadio;

          ParamLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ParamLinkNode);
          TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnRadio');

          if RadioButton.ButtonCount > 0 then begin

            if RadioButton.Orientation = otHorizontal then begin
              w := RadioButton.Width div RadioButton.ButtonCount - 1;
              h := RadioButton.Height;
              if RadioButton.UpsideDown then begin
                dx := w;
                dy := 0;
              end else begin
                dx := -w;
                dy := 0;
              end;
            end else begin
              w := RadioButton.Width;
              h := RadioButton.Height div RadioButton.ButtonCount - 1;
              if RadioButton.UpsideDown then begin
                dx := 0;
                dy := h;
              end else begin
                dx := 0;
                dy := -h;
              end;
            end;

            FindOrAddBtnRadio( w, h);

            x := RadioButton.Left;
            y := RadioButton.Top;
            for m := 0 to RadioButton.ButtonCount - 1 do begin
              if m = 0 then
                CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_down', w, h), x, y)
              else
                CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_up', w, h), x, y);
              x := x + dx;
              y := y + dy;
            end;
          end;
        end;

        if Control is TG2GraphButtonRadioEdit then begin
          RadioButtonEdit := Control as TG2GraphButtonRadioEdit;

          ParamLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ParamLinkNode);
          TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'btnRadioEdit');

          w := RadioButtonEdit.Width div RadioButtonEdit.ButtonColumns;
          h := RadioButtonEdit.Height div RadioButtonEdit.ButtonRows;
          y := RadioButtonEdit.Top;
          for m := 0 to RadioButtonEdit.ButtonRows - 1 do begin
            x := RadioButtonEdit.Left;
            for n := 0 to RadioButtonEdit.ButtonColumns - 1 do begin

              FindOrAddBtnRadio( w, h);
              if (m = 0) and (n=0) then
                CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_down', w, h), x, y)
              else
                CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_up', w, h), x, y);

              x := x + w;
            end;
            y := y + h;
          end;
        end;

        if Control is TG2GraphKnob then begin

          ParamLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ParamLinkNode);
          TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'Knob');

          if (Control as TG2GraphKnob).KnobType = ktSlider then begin
            TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'slider');

            CreateUse( ParamLinkNode, control_id + '_el_1', idSlider, Control.Left, Control.Top);
            CreateUse( ParamLinkNode, control_id + '_el_2', idBtnIncDecVert, Control.Left, Control.Top + 46);
          end else begin
            case (Control as TG2GraphKnob).KnobType of
              ktBig :
                begin
              	  ref_id := idKnobBig;
                  TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'big');
              	end;
              ktMedium :
                begin
                  ref_id := idKnobMedium;
                  TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'medium');
                end;
              ktResetMedium :
                begin
                  ref_id := idKnobResetMedium;
                  TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'resetMedium');
                end;
              ktReset :
                begin
                  ref_id := idKnobReset;
                  TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'reset');
                end;
              ktSmall :
                begin
                  ref_id := idKnobSmall;
                  TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'small');
                end;
            end;
            CreateUse( ParamLinkNode, control_id, ref_id, Control.Left, Control.Top);
          end;
        end;

        if Control is TG2GraphPartSelector then begin

          FindOrAddPartSel( Control.Width, Control.Height);

          ParamLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ParamLinkNode);
          TDOMElement(ParamLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_paramlink_' + IntToStr(Control.ID));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Control.Parameter.ParamIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.InfoFunc', IntTostr(Control.Parameter.InfoFunctionIndex));
          TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlType', 'partSel');

          CreateUse( ParamLinkNode, control_id, GetIDPartSel( Control.Width, Control.Height), Control.Left, Control.Top);

          FindOrAddPartSelOptions( Control.Width, Control.Height, (Control as TG2GraphPartSelector));
        end;

        if Control is TG2GraphGraph then begin

          FindOrAddGraph( Control.Width, Control.Height);

          CreateUse( GModuleNode, control_id, GetIDGraph( Control.Width, Control.Height), Control.Left, Control.Top);
        end;

        if Control is TG2GraphLedGreen then begin
          case (Control as TG2GraphLedGreen).FType of
            ltGreen : ref_id := idLedOffGreen;
            ltSequencer : ref_id := idLedOffSequencer;
          end;
          CreateUse( GModuleNode, control_id, ref_id, Control.Left, Control.Top);
        end;

        if Control is TG2GraphConnector then begin
          Connector := Control as TG2GraphConnector;

          ConnLinkNode := Doc.CreateElement('g');
          GModuleNode.AppendChild(ConnLinkNode);
          TDOMElement(ConnLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_connlink_' + IntToStr(Control.ID));
          TDOMElement(ConnLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Connector.Data.ConnectorIndex));

          UseNode := nil;
          if Connector.Data.ConnectorKind = ckInput then begin
            case Connector.Data.ConnectorDefColor of
            COLOR_YELLOW : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorIn + '_yellow', Control.Left, Control.Top);
            COLOR_BLUE : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorIn + '_blue', Control.Left, Control.Top);
            COLOR_RED : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorIn + '_red', Control.Left, Control.Top);
            end;
          end;

          if Connector.Data.ConnectorKind = ckOutput then begin
            case Connector.Data.ConnectorDefColor of
            COLOR_YELLOW : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorOut + '_yellow', Control.Left, Control.Top);
            COLOR_BLUE : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorOut + '_blue', Control.Left, Control.Top);
            COLOR_RED : UseNode := CreateUse( ConnLinkNode, control_id, idConnectorOut + '_red', Control.Left, Control.Top);
            end;
          end;

        end;
      end;
    end;

    procedure CreateBackground( aNode : TDomNode);
    var S : TStringStream;
        rx, bw : single;
        id : string;
    begin
      bw := 1;
      rx := 1;
      id := idPanelBackground;
      S := TStringStream.Create(
         '<g id="' + id + '">'
       + '  <desc>Patch background for G2 editor</desc>'
       + '  <g id="' + id + '_parts">'
       + '     <rect id="' + id + '_bg" fill="#483737" stroke="none" x="0" y="0" width="' + IntToStr(UNITS_COL) + '" height="' + IntToStr(UNITS_ROW) + '" />'
       + '     <rect id="' + id + '_leftrail" fill="lightgray" stroke="none" x="' + IntToStr(0) + '" y="' + IntToStr(0) + '" width="' + IntToStr(UNITS_ROW) + '" height="' + IntToStr(UNITS_ROW) + '" />'
       + '     <rect id="' + id + '_rightrail" fill="lightgray" stroke="none" x="' + IntToStr(UNITS_COL - UNITS_ROW) + '" y="' + IntToStr(0) + '" width="' + IntToStr(UNITS_ROW) + '" height="' + IntToStr(UNITS_ROW) + '" />'
       + '     <circle id="' + id + '_lefthole" cx="' + FloatToStr(UNITS_ROW/2) + '" cy="' + FloatToStr(UNITS_ROW/2) + '" r="' + FloatToStr(UNITS_ROW * 0.3) + '" fill="#483737" stroke="none"  />'
       + '     <circle id="' + id + '_rightthole" cx="' + FloatToStr(UNITS_COL - UNITS_ROW/2) + '" cy="' + FloatToStr(UNITS_ROW/2) + '" r="' + FloatToStr(UNITS_ROW * 0.3) + '" fill="#483737" stroke="none"  />'
       + '  </g>'
       + '</g>');
      try
        ReadXMLFragment( aNode, S);
      finally
        S.Free;
      end;
    end;

begin
  Doc := TXMLDocument.Create;
  BitMapList := TObjectList.Create(True);
  TextBitmap := TBitmap.Create;
  try
    RootNode := Doc.CreateElement('svg');
    Doc.AppendChild(RootNode);
    TDOMElement(RootNode).SetAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
    TDOMElement(RootNode).SetAttribute('xmlns:cc','http://creativecommons.org/ns#');
    TDOMElement(RootNode).SetAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');
    TDOMElement(RootNode).SetAttribute('xmlns:svg','http://www.w3.org/2000/svg');
    TDOMElement(RootNode).SetAttribute('xmlns:nmg2','http://www.bverhue/g2dev');
    TDOMElement(RootNode).SetAttribute('xmlns','http://www.w3.org/2000/svg');
    TDOMElement(RootNode).SetAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');

    DefsNode := Doc.CreateElement('defs');
    RootNode.AppendChild( DefsNode);
    TDOMElement(DefsNode).SetAttribute('id', 'g2_defs');

    symbol_section := 0;
    knob_section := 50;
    led_off_section := 100;
    led_on_section := 150;
    minivu_section := 200;
    buttontext_up_section := 250;
    buttontext_down_section := 300;
    buttontext_section := 350;
    buttonflat_section := 400;
    btnradioup_section := 450;
    btnradiodown_section := 500;
    textfield_section := 550;
    btnIncDec_section := 600;
    levelshift_section := 650;
    partsel_section := 700;
    graph_section := 750;
    connector_section := 800;
    btnflatoptions_section := 850;
    partseloptions_section := 1000;
    module_section := 1100;
    uipanel_section := module_section + 3500;

    mr := 0;
    mc := 0;
    btc := 0;
    bfc := 0;
    kc := 0;
    tfc := 0;
    sc := 0;
    loffc := 0;
    lonc := 0;
    bidc := 0;
    psc := 0;
    cc := 0;
    minivu_c := 0;
    gc := 0;
    rbc := 0;
    lsc := 0;
    uir := 0;
    bfoc := 0;
    psoc := 0;

    CreateModulePanelGradients;
    CreateKnobSideGradient( DefsNode, 11);
    CreateBtnTextSideGradient( DefsNode, 'btnTextGradienSideLeft', 0.5, 1, 0.5, 0.5, '#005544', '#80ffe6');
    CreateBtnTextSideGradient( DefsNode, 'btnTextGradienSideTop', 0.5, 0.5, 0.5, 1, '#005544', '#80ffe6');
    CreateBtnTextSideGradient( DefsNode, 'btnTextGradienSideRight', 0.5, 0.0, 0.5, 0.5, '#005544', '#80ffe6');
    CreateBtnTextSideGradient( DefsNode, 'btnTextGradienSideBottom', 0.5, 0.5, 0.5, 0.0, '#005544', '#80ffe6');
    CreateBtFaceGradient( DefsNode, 'btnTextGradienFace', 0.5, 0.5, 0.5, 0.5, 1, '#aaffee', '#80ffe6');

    CreateBtnTextSideGradient( DefsNode, 'btnRadioGradienSideLeft', 0.25, 1, 0.5, 0.5, '#4d4d4d', '#ffdd55');
    CreateBtnTextSideGradient( DefsNode, 'btnRadioGradienSideTop', 0.5, 0.5, 0.25, 1, '#4d4d4d', '#ffdd55');
    CreateBtnTextSideGradient( DefsNode, 'btnRadioGradienSideRight', 0.75, 0.0, 0.5, 0.5, '#4d4d4d', '#ffdd55');
    CreateBtnTextSideGradient( DefsNode, 'btnRadioGradienSideBottom', 0.5, 0.5, 0.75, 0.0, '#4d4d4d', '#ffdd55');
    CreateBtFaceGradient( DefsNode, 'btnRadioGradienFace', 0.5, 0.5, 0.5, 0.5, 1, '#ffffff', '#ffeeaa');

    CreateBtFaceGradient( DefsNode, 'btnRadioUpGradientFace', 0.5, 0.5, 0.5, 0.5, 1, '#ffffff', '#f2f2f2');

    GMainNode := Doc.CreateElement('g');
    RootNode.AppendChild(GMainNode);
    TDOMElement(GMainNode).SetAttribute('id', 'g2_main');

    GSymbolSectionNode := CreateSection( GMainNode, 'symbolSection', symbol_section, 'Symbol section');
    sc := sc + 200;

    CreateSymbolSmallArrowUp;
    CreateSymbolSmallArrowDown;
    CreateSymbolSmallArrowLeft;
    CreateSymbolSmallArrowRight;

    GKnobSectionNode := CreateSection( GMainNode, 'knobSection', knob_section, 'Knob section');
    kc := kc + 200;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobBtns + '_def', kc, 0);
    CreateKnobButtons( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobCenterBtn + '_off_def', kc, 0);
    CreateCenterBtnOff( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobCenterBtn + '_on_def', kc, 0);
    CreateCenterBtnOn( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobBig + '_def', kc, 0);
    CreateKnobBig( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobMedium + '_def', kc, 0);
    CreateKnobMedium( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobResetMedium + '_def', kc, 0);
    CreateKnobResetMedium( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobReset + '_def', kc, 0);
    CreateKnobReset( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idKnobSmall + '_def', kc, 0);
    CreateKnobSmall( GKnobDefNode);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idSlider + 'knob_def', kc, 0);
    CreateKnobSliderKnob( GKnobDefNode, 11, 6, 0.3, 1.0);
    kc := kc + 40;

    GKnobDefNode := CreateSectionPlaceholder( GKnobSectionNode, idSlider + '_def', kc, 0);
    CreateKnobSlider( GKnobDefNode);
    kc := kc + 40;

    GLedOffSectionNode := CreateSection( GMainNode, 'ledOffSection', led_off_section, 'Led off section');
    loffc := loffc + 200;

    GLedOffDefNode := CreateSectionPlaceholder( GLedOffSectionNode, idLedOffSequencer + '_def', loffc, 0);
    CreateLedSeq( GLedOffDefNode);
    loffc := loffc + 40;

    GLedOffDefNode := CreateSectionPlaceholder( GLedOffSectionNode, idLedOffGreen + '_def', loffc, 0);
    CreateLedGreen( GLedOffDefNode);
    loffc := loffc + 40;

    GLedOnSectionNode := CreateSection( GMainNode, 'ledOffSection', led_on_section, 'Led on section');
    lonc := lonc + 200;

    GLedOnDefNode := CreateSectionPlaceholder( GLedOnSectionNode, idLedOnSequencer + '_def', lonc, 0);
    CreateLedSeq( GLedOnDefNode);
    lonc := lonc + 40;

    GLedOnDefNode := CreateSectionPlaceholder( GLedOnSectionNode, idLedOnGreen + '_def', lonc, 0);
    CreateLedGreen( GLedOnDefNode);
    lonc := lonc + 40;

    GMiniVUSectionNode := CreateSection( GMainNode, 'minivuSection', minivu_section, 'Mini VU section');
    minivu_c := minivu_c + 200;

    GTextFieldSectionNode := CreateSection( GMainNode, 'textfieldSection', textfield_section, 'Text field section');
    tfc := tfc + 200;

    GButtonTextUpSectionNode := CreateSection( GMainNode, 'btnTextUpSection', buttontext_up_section, 'Button text up section');
    GButtonTextDownSectionNode := CreateSection( GMainNode, 'btnTextDownSection', buttontext_down_section, 'Button text down section');
    GButtonTextSectionNode := CreateSection( GMainNode, 'btnTextSection', buttontext_section, 'Button text section');
    btc := btc + 200;

    GButtonFlatSectionNode := CreateSection( GMainNode, 'btnFlatSection', buttonflat_section, 'Button flat section');
    bfc := bfc + 200;

    GBtnIncDecSectionNode := CreateSection( GMainNode, 'btnIncDecSection', btnincdec_section, 'Button inc dec section');
    bidc := bidc + 200;

    GBtnIncDecDefNode := CreateSectionPlaceholder( GBtnIncDecSectionNode, idBtnIncDecHorz + '_def', bidc, 0);
    CreateBtnIncDecHorz( GBtnIncDecDefNode);
    bidc := bidc + 40;

    GBtnIncDecDefNode := CreateSectionPlaceholder( GBtnIncDecSectionNode, idBtnIncDecVert + '_def', bidc, 0);
    CreateBtnIncDecVert( GBtnIncDecDefNode);
    bidc := bidc + 40;

    GLevelShiftSectionNode := CreateSection( GMainNode, 'levelShiftSection', levelshift_section, 'Level shift section');
    lsc := lsc + 200;

    GLevelShiftDefNode := CreateSectionPlaceholder( GLevelShiftSectionNode, idLevelShift + '_def', lsc, 0);
    CreateLevelShift( GLevelShiftDefNode, 13, 12);
    lsc := lsc + 40;

    GBtnRadioUpSectionNode := CreateSection( GMainNode, 'btnRadionUpSection', btnRadioUp_section, 'Button radio up section');
    GBtnRadioDownSectionNode := CreateSection( GMainNode, 'btnRadionDownSection', btnRadioDown_section, 'Button radio down section');
    rbc := rbc + 200;

    GPartSelSectionNode := CreateSection( GMainNode, 'partselSection', partsel_section, 'Part selector section');
    psc := psc + 200;

    GGraphSectionNode := CreateSection( GMainNode, 'graphSection', graph_section, 'Graph section');
    gc := gc + 200;

    GConnectorSectionNode := CreateSection( GMainNode, 'connectorSection', connector_section, 'Connector section');
    cc := cc + 200;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorIn + '_blue_def', cc, 0);
    CreateInConnector( GConnectorDefNode, idConnectorIn + '_blue', '#6464FF');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorIn + '_red_def', cc, 0);
    CreateInConnector( GConnectorDefNode, idConnectorIn + '_red', '#FF5A5A');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorIn + '_yellow_def', cc, 0);
    CreateInConnector( GConnectorDefNode, idConnectorIn + '_yellow', '#E6E650');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorIn + '_orange_def', cc, 0);
    CreateInConnector( GConnectorDefNode, idConnectorIn + '_orange', '#FFC050');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorOut + '_blue_def', cc, 0);
    CreateOutConnector( GConnectorDefNode, idConnectorOut + '_blue', '#6464FF');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorOut + '_red_def', cc, 0);
    CreateOutConnector( GConnectorDefNode, idConnectorOut + '_red', '#FF5A5A');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorOut + '_yellow_def', cc, 0);
    CreateOutConnector( GConnectorDefNode, idConnectorOut + '_yellow', '#E6E650');
    cc := cc + 40;

    GConnectorDefNode := CreateSectionPlaceholder( GConnectorSectionNode, idConnectorOut + '_orange_def', cc, 0);
    CreateOutConnector( GConnectorDefNode, idConnectorOut + '_orange', '#FFC050');
    cc := cc + 40;

    GButtonFlatOptionsSectionNode := CreateSection( GMainNode, 'btnFlatOptionsSection', btnflatoptions_section, 'Btn flat options section');
    bfoc := bfoc + 200;

    GPartSelOptionsSectionNode := CreateSection( GMainNode, 'partSelOptionsSection', partseloptions_section, 'Part selector options section');
    psoc := psoc + 200;

    GModuleSectionNode := CreateSection( GMainNode, 'moduleSection', module_section, 'Module section');

    for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin

      GModuleDefNode := CreateSectionPlaceholder( GModuleSectionNode, idModule + '_' + IntToStr(i) + '_def', mc * 300, mr*160);

      inc(mr);
      if mr > 21 then begin
        inc(mc);
        mr := 0;
      end;

      FModuleType := G2_module_def.FModuleDefList.ModuleDef[i].ModuleType;
      LoadModule;
      try
        CreateModule( GModuleDefNode);

        for j := 0 to FModule.Panel.ChildControlsCount - 1 do begin
          Control :=  FModule.Panel.GraphChildControls[j];

          // Extract bitmaps
          if Control is TG2GraphBitmap then begin
            for k := 0 to (Control as TG2GraphBitmap).ImageList.Count - 1 do begin
              if FindBitmap((Control as TG2GraphBitmap).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphBitmap).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphBitmap).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphButton then begin
            for k := 0 to (Control as TG2GraphButton).ImageList.Count - 1 do begin
              if FindBitmap((Control as TG2GraphButton).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphButton).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphButton).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;

          if Control is TG2GraphPartSelector then begin
            for k := 0 to (Control as TG2GraphPartSelector).ImageList.Count - 1 do begin
              if FindBitmap((Control as TG2GraphPartSelector).ImageList.Items[k]) = -1 then begin
                Bitmap := TBitmap.Create;
                Bitmap.Assign((Control as TG2GraphPartSelector).ImageList.Items[k]);
                BitMapList.Add(Bitmap);
              end;
              //(Control as TG2GraphPartSelector).ImageList.Items[k].SaveToFile('C:\Users\Bruno\Delphi\nmg2editor\v0.25\ExtrImg\Module_' + IntToStr(FModule.TypeID) + '_ControlID_' + IntToStr(Control.ID) + '_No_' +  IntToStr(k) + '.bmp');
            end;
          end;
        end;

      finally
        UnloadModule
      end;
    end;


    GUIPanelSectionNode := CreateSection( GMainNode, 'UIPanelSection', uipanel_section, 'User interf. panel section');
    uir := uir + 60;

    GUIPanelDefNode := CreateSectionPlaceholder( GUIPanelSectionNode, idPanelBackground + '_def', 0, uir);
    CreateBackground( GUIPanelDefNode);
    uir := uir + 40;

    for i := 0 to BitmapList.Count - 1 do begin
      if assigned(BitmapList.Items[i]) then
        (BitmapList.Items[i] as TBitmap).SaveToFile('Skin\Symbol_' + IntToStr(i) + '.bmp');
    end;

    WriteXMLFile( Doc, 'Skin\g2_graphics.svg');

    SVGSkin := TSVGSkin.Create(self);
    try
      SVGSkin.SaveToFile;
    finally
      SVGSkin.Free;
    end;

  finally
    TextBitmap.Free;
    BitMapList.Free;
    Doc.Free;
  end;
end;


//------------------------------------------------------------------------------
//
//                                  TSVGSkin
//
//------------------------------------------------------------------------------

//http://stackoverflow.com/questions/16865979/delphi-exception-when-preserving-whitespace-in-txmldocument
//FragNode := storedXMLObj.DocumentElement.ChildNodes[1];
//FragNode.ChildNodes.Nodes[1].ChildNodes.Nodes[1].ChildNodes.Add(LoadXMLData(XMLFragment.Text).DocumentElement);
//FragNode.ChildNodes.Nodes[1].ChildNodes.Nodes[1].ChildNodes.Add(LoadXMLData(XMLFragment.Text).DocumentElement);

constructor TSVGSkin.Create(AOwner: TComponent);
begin
  inherited;
  Doc := TXMLDocument.Create;
  BitMapList := TObjectList.Create(True);
  TextBitmap := TBitmap.Create;

  FDefList := TStringList.Create;

  FSections := TObjectList.Create(True);

  FRootNode := Doc.CreateElement('svg');
  Doc.AppendChild(FRootNode);
  TDOMElement(FRootNode).SetAttribute('xmlns:dc','http://purl.org/dc/elements/1.1/');
  TDOMElement(FRootNode).SetAttribute('xmlns:cc','http://creativecommons.org/ns#');
  TDOMElement(FRootNode).SetAttribute('xmlns:rdf','http://www.w3.org/1999/02/22-rdf-syntax-ns#');
  TDOMElement(FRootNode).SetAttribute('xmlns:svg','http://www.w3.org/2000/svg');
  TDOMElement(FRootNode).SetAttribute('xmlns:nmg2','http://www.bverhue/g2dev');
  TDOMElement(FRootNode).SetAttribute('xmlns','http://www.w3.org/2000/svg');
  TDOMElement(FRootNode).SetAttribute('xmlns:xlink','http://www.w3.org/1999/xlink');

  FDefsNode := CreateChild( FRootNode, 'defs', 'g2_defs');
  FMainNode := CreateG( FRootNode, 'g2_main');
end;

destructor TSVGSkin.Destroy;
begin
  FSections.Free;

  FDefList.Free;

  TextBitmap.Free;
  BitMapList.Free;
  Doc.Free;
  inherited;
end;

function TSVGSkin.GetID2D( aBaseName : string; aWidth, aHeight : single): string;
begin
  Result :=  aBaseName + '_' + FloatToStr(trunc(aWidth*10)/10) + 'x' + FloatToStr(trunc(aHeight*10)/10);
end;

function TSVGSkin.GetID3D( aBaseName : string; aWidth, aHeight, aDepth : single): string;
begin
  Result :=  aBaseName + '_' + FloatToStr(trunc(aWidth*10)/10) + 'x' + FloatToStr(trunc(aHeight*10)/10) + 'x' + FloatToStr(trunc(aDepth*10)/10);
end;

function TSVGSkin.GetID4D(aBaseName: string; aWidth, aHeight, aDepth: single;
  aSymbolID: string): string;
begin
  Result :=  aBaseName + '_' + FloatToStr(trunc(aWidth*10)/10) + 'x' + FloatToStr(trunc(aHeight*10)/10) + 'x' + FloatToStr(trunc(aDepth*10)/10)+ '_' + aSymbolID;
end;

function TSVGSkin.GetLabelID( aBaseName: string; aText : string): string;
var i : integer;
    c : char;
begin
  Result := '';
  for i := 1 to Length(aText) do begin
    c := aText[i];
    case c of
      ' ' : c := '_';
      '&' : c := '+';
    end;
    Result := Result + c;
  end;

  Result := aBaseName + '_' + Result;
end;

function TSVGSkin.FindDef(aID: string): TDomNode;
var index : integer;
begin
  index := FDefList.IndexOf(aID);
  if index = -1 then
    Result := nil
  else begin
    Result := FDefList.Objects[index] as TDomNode;
  end;
end;

function TSVGSkin.FindSection(aID: string): TSVGSkinSection;
var i : integer;
begin
  Result := nil;

  i := 0;
  while (i < FSections.Count) and not((FSections[i] as TSVGSkinSection).FID = aID) do
    inc(i);

  if (i >= FSections.Count) then
    raise Exception.Create('Section ' + aID + ' not found.');

  Result := (FSections[i] as TSVGSkinSection);
end;

function TSVGSkin.DefExists(aID: string): boolean;
begin
  Result := FDefList.IndexOf(aID) <> -1;
end;

function TSVGSkin.AddToSection(aSectionID, aID: string): TSVGSkinSectionPlaceholder;
var Section : TSVGSkinSection;
begin
  Section := FindSection(aSectionID);
  Result := Section.CreatePlaceholder('def_' + aID);
end;

procedure TSVGSkin.SaveToFile;
var g2_module_def : TG2;
    i : integer;
    Module : TG2GraphModule;
    ModuleType : integer;
    ModuleID : string;
    ModuleSection, Section  : TSVGSkinSection;
    Placeholder : TSVGSkinSectionPlaceholder;
    SVGObject : TSVGObject;
begin
  g2_module_def := TG2.Create(self);
  try
    g2_module_def.LoadModuleDefs('');

    CreateModulePanelGradients;

    CreateSection( 'module_names_section',  60,  50, 100, 3, 'Module names section');
    CreateSection( 'module_labels_section', 40,  50, 100, 3, 'Module labels section');

    CreateSection( 'symbol_section',        20,  50, 100, 1, 'Symbol section');
    CreateSymbolSmallArrowUp;
    CreateSymbolSmallArrowDown;
    CreateSymbolSmallArrowLeft;
    CreateSymbolSmallArrowRight;


    CreateSection( 'textfield_section',    100,  50, 50, 1, 'Text field section');

    CreateSection( 'btntext_up_section', 100,  30, 50, 3, 'Button text up section');
    CreateSection( 'btntext_down_section', 100,  30, 50, 3, 'Button text down section');
    CreateSection( 'btntext_labels_section', 40,  50, 100, 2, 'Button text labels section');
    CreateSection( 'btntext_section', 100,  30, 50, 3, 'Button text section');

    CreateSection( 'btnincdec_section', 50,  50, 50, 1, 'Button inc dec section');

    CreateSection( 'btnflat_labels_section', 40,  50, 100, 2, 'Button flat labels section');
    CreateSection( 'btnflat_btns_section', 80,  30, 50, 4, 'Button flat section');

    CreateSection( 'btnflat_section', 80,  30, 50, 1, 'Button flat section');
    CreateSection( 'btnradio_section', 100,  100, 50, 1, 'Button radio section');

    CreateSection( 'knob_section', 50, 50, 50, 1, 'Knob section');
    CreateCenterBtnOff( AddToSection('knob_section', '').FNode);
    CreateCenterBtnOn( AddToSection('knob_section', '').FNode);
    CreateCenterBtn( AddToSection('knob_section', '').FNode);
    CreateKnobButtons( AddToSection('knob_section', '').FNode);
    CreateKnobBig( AddToSection('knob_section', '').FNode);
    CreateKnobMedium( AddToSection('knob_section', '').FNode);
    CreateKnobResetMedium( AddToSection('knob_section', '').FNode);
    CreateKnobReset( AddToSection('knob_section', '').FNode);
    CreateKnobSmall( AddToSection('knob_section', '').FNode);
    CreateKnobSliderKnob( AddToSection('knob_section', '').FNode, idSlider + '_knob', 11, 6, 0.3, 1.0);
    CreateKnobSlider( AddToSection('knob_section', '').FNode, idSlider);

    CreateSection( 'connector_section', 50, 50, 20, 1, 'Connector section');
    CreateInConnector( AddToSection('connector_section', '').FNode, 'conn_in' + '_blue', '#6464FF');
    CreateInConnector( AddToSection('connector_section', '').FNode, 'conn_in' + '_red', '#FF5A5A');
    CreateInConnector( AddToSection('connector_section', '').FNode, 'conn_in' + '_yellow', '#E6E650');
    CreateInConnector( AddToSection('connector_section', '').FNode, 'conn_in' + '_orange', '#FFC050');
    CreateOutConnector( AddToSection('connector_section', '').FNode, 'conn_out' + '_blue', '#6464FF');
    CreateOutConnector( AddToSection('connector_section', '').FNode, 'conn_out' + '_red', '#FF5A5A');
    CreateOutConnector( AddToSection('connector_section', '').FNode, 'conn_out' + '_yellow', '#E6E650');
    CreateOutConnector( AddToSection('connector_section', '').FNode, 'conn_out' + '_orange', '#FFC050');

    ModuleSection := CreateSection( 'moduleSection', 300, 300, 10, 20, 'Module section');

    CreateSection( 'UIPanelSection', 300, 300, 1, 4, 'User interf. panel section');
    Placeholder := AddToSection( 'UIPanelSection', 'patchbackground');
    CreateBackground( Placeholder.FNode, 'patchbackground');

    for i := 0 to G2_module_def.FModuleDefList.Count - 1 do begin
      ModuleType := G2_module_def.FModuleDefList.ModuleDef[i].ModuleType;
      Module := G2_module_def.SelectedPatch.CreateModule( ltVA, 1, ModuleType) as TG2GraphModule;

      if assigned(Module) then begin

        ModuleID := 'module' + '_' + IntToStr(ModuleType);
        G2_module_def.SelectedPatch.AddModuleToPatch( ltVA, Module);
        G2_module_def.SelectedPatch.SortLeds;
        try
          Placeholder := ModuleSection.CreatePlaceholder( 'def_' + ModuleID);
          TSVGG2Module.Create( Placeholder, ModuleID, Module);
        finally
          G2_module_def.SelectedPatch.RemoveFromLedList( ltVA, Module.ModuleIndex);
          G2_module_def.SelectedPatch.DeleteModuleFromPatch(ltVA, Module);
          Module := nil;
        end;
      end;
    end;

    WriteXMLFile( Doc, 'Skin\g2_graphics_2.svg');
  finally
    g2_module_def.Free;
  end;
end;

function TSVGSkin.FindBitmap( aBitmap : TBitmap): integer;
begin
  Result := 0;
  while (Result < BitMapList.Count) and not(CompareBitmap( aBitmap, BitMapList.Items[Result] as TBitmap)) do
    inc(Result);

  if (Result >= BitMapList.Count) then
    Result := -1;
end;

function TSVGSkin.AddSymbolFromBitmap( aBitmap : TBitmap): integer;
var ObjectID : string;
    Bitmap : TBitmap;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  Result := FindBitmap(aBitmap);
  if Result = - 1 then begin

    Bitmap := TBitmap.Create;
    Bitmap.Assign(aBitmap);
    BitMapList.Add(Bitmap);
    Result := BitmapList.Count -1;

    ObjectID := 'symbol' + '_' + IntToStr(Result);

    Placeholder := AddToSection( 'symbol_section', ObjectID);
    CreateSymbol( Placeholder.FNode, ObjectID, (BitmapList[Result] as TBitmap).Width, (BitmapList[Result] as TBitmap).Height);
  end;
end;

function TSVGSkin.RoundedBevelObject( aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
var dw, dh : single;
begin
  dw := aBevelWidth / 1.5;
  dh := aBevelHeight / 1.5;

  Result :=
     '<path id="' + aID + '_bevel_l" fill="' + aColSideLeft + '" stroke="none"'
   + '  d="M 0,0'
       + ' c 0,0 ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
         + ' 0,' + FloatToStr(aBevelWidth + dw) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2 - dw - aBevelWidth) + ' 0,' + FloatToStr(aHeight - (aBevelWidth+dw)*2)
         + ' 0,' + FloatToStr(dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(aBevelWidth+dw)
       + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_t" fill="' + aColSideTop + '" stroke="none"'
   + '  d="M ' + FloatToStr(aWidth) + ',0'
       + ' c 0,0 ' + FloatToStr(-dh) + ','  + FloatToStr(aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(aBevelHeight)
         + ' ' + FloatToStr(-aBevelHeight - dh) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2 + dh + aBevelHeight) + ',0 ' + FloatToStr(-aWidth + (aBevelHeight+dh)*2) + ',0'
         + ' ' + FloatToStr(-dh) + ',0 ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(-aBevelHeight-dh) + ',' + FloatToStr(-aBevelHeight)
       + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_r" fill="' + aColSideRight + '" stroke="none"'
   + '  d="M ' + FloatToStr(aWidth) + ',' + FloatToStr(aHeight)
       + ' c 0,0 ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(-dw) + ' ' + FloatToStr(-aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
         + ' 0,' + FloatToStr(-aBevelWidth - dw) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2 + dw + aBevelWidth) + ' 0,' + FloatToStr(-aHeight + (aBevelWidth+dw)*2)
         + ' 0,' + FloatToStr(-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw) + ' ' + FloatToStr(aBevelWidth) + ',' + FloatToStr(-aBevelWidth-dw)
       + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_b" fill="' + aColSideBottom + '" stroke="none"'
   + '  d="M 0,' + FloatToStr(aHeight)
       + ' c 0,0 ' + FloatToStr(dh) + ','  + FloatToStr(-aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(-aBevelHeight)
         + ' ' + FloatToStr(aBevelHeight + dh) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2 - dh - aBevelHeight) + ',0 ' + FloatToStr(aWidth - (aBevelHeight+dh)*2) + ',0'
         + ' ' + FloatToStr(dh) + ',0 ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight) + ' ' + FloatToStr(aBevelHeight+dh) + ',' + FloatToStr(aBevelHeight)
       + ' z">'
   + '</path>';
end;

function TSVGSkin.BevelObject( aID : string; aWidth, aHeight, aBevelWidth : single; aColSideLeft, aColSideTop, aColSideRight, aColSideBottom : string): string;
begin
   Result :=
     '<path id="' + aID + '_bevel_l" fill="' + aColSideLeft + '" stroke="none"'
   + '  d="M 0,0 l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' v ' + FloatToStr((aHeight - aBevelWidth*2)) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_t" fill="' + aColSideTop + '" stroke="none"'
   + '  d="M 0,0 h ' + FloatToStr(aWidth) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(aBevelWidth) + ' h ' + FloatToStr(-(aWidth - aBevelWidth*2)) + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_r" fill="' + aColSideRight + '" stroke="none"'
   + '  d="M ' + FloatToStr(aWidth) + ',0 v ' + FloatToStr(aHeight) + ' l ' + FloatToStr(-aBevelWidth) + ','  + FloatToStr(-aBevelWidth) + ' v ' + FloatToStr(-(aHeight - aBevelWidth*2)) + ' z">'
   + '</path>'
   + '<path id="' + aID + '_bevel_b" fill="' + aColSideBottom + '" stroke="none"'
   + '  d="M 0, ' + FloatToStr(aHeight) + ' l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(-aBevelWidth) + ' h ' + FloatToStr((aWidth - aBevelWidth*2)) + ' l ' + FloatToStr(aBevelWidth) + ','  + FloatToStr(aBevelWidth) +' z">'
   + '</path>';
end;

function TSVGSkin.RoundedButtonObject( aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single;
                                       aColSideLeft, aColSideTop, aColSideRight, aColSideBottom, aColBG, aColFace : string ): string;
begin
   Result :=
     '<rect id="' + aID + '_sel" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="none" stroke="blue" stroke-width="0.2"/>'
   + '<g id="' + aID + '_parts">'
   + '  <rect id="' + aID + '_bg" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" fill="' + aColBG + '" stroke="black" stroke-width="0.5"/>'
   + '  <g id="' + aID + '_bevel">'
         + RoundedBevelObject( aID, aWidth, aHeight, aBevelWidth, aBevelHeight, aColSideLeft, aColSideTop, aColSideRight, aColSideBottom)
   + '  </g>'
   + '  <rect id="' + aID + '_face" x="' + FloatToStr(aBevelWidth) + '" y="' + FloatToStr(aBevelHeight) + '" width="' + FloatToStr(aWidth-aBevelWidth*2) + '" height="' + FloatToStr(aHeight-aBevelHeight*2) + '" rx="0.75" fill="' + aColFace + '" stroke="none"/>'
   + '</g>';
end;

function TSVGSkin.CreateChild( aNode : TDomNode; aElementName, aID : string): TDomNode;
begin
  Result := Doc.CreateElement(aElementName);
  aNode.AppendChild(Result);
  SetAttribute(Result, 'id', aID);
  FDefList.AddObject( aID, Result);
end;

function TSVGSkin.CreateG(aNode: TDomNode; aID: string): TDomNode;
begin
  Result := CreateChild( aNode, 'g', aID);
end;

function TSVGSkin.CreateDesc(aNode: TDomNode; aID,
  aDescription: string): TDomNode;
begin
  Result := CreateChild( aNode, 'desc', aID);
  Result.AppendChild( Doc.CreateTextNode(aDescription));
end;

function TSVGSkin.CreateUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer): TDomNode;
begin
  Result := CreateChild( aNode, 'use', aID);
  SetAttribute( Result, 'id', aID);
  SetAttribute( Result, 'xlink:href', '#' + aRef);
  SetAttribute( Result, 'transform', 'translate(' + IntToStr(dx) + ',' + IntToStr(dy) + ')');
  SetAttribute( Result, 'x', '0');
  SetAttribute( Result, 'y', '0');
end;

function TSVGSkin.CreateParamLink(aNode: TDomNode; aID: string; aCodeRef,
  aMasterRef, aInfoFunc, aTextFunc: integer; aCtrlType, aDependencies: string): TDomNode;
begin
  Result := CreateG( aNode, aID);
  SetAttribute( Result, 'nmg2.CodeRef', IntToStr(aCodeRef));
  SetAttribute( Result, 'nmg2.MasterRef', IntToStr(aMasterRef));
  SetAttribute( Result, 'nmg2.InfoFunc', IntTostr(aInfoFunc));
  SetAttribute( Result, 'nmg2.TextFunc', IntTostr(aTextFunc));
  SetAttribute( Result, 'nmg2.CtrlType', aCtrlType);
  SetAttribute( Result, 'nmg2.Dependencies', aDependencies);
end;

function TSVGSkin.CreateConnLink(aNode: TDomNode; aID: string; aCodeRef: integer): TDomNode;
begin
  Result := CreateG( aNode, aID);
  SetAttribute( Result, 'nmg2.CodeRef', IntToStr(aCodeRef));
end;

function TSVGSkin.CreateParamLinkedUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer;
  aCodeRef, aMasterRef, aInfoFunc, aTextFunc: integer; aCtrlType, aDependencies: string): TDomNode;
begin
  Result := CreateChild( aNode, 'use', aID);
  SetAttribute( Result, 'id', aID);
  SetAttribute( Result, 'xlink:href', '#' + aRef);
  SetAttribute( Result, 'transform', 'translate(' + IntToStr(dx) + ',' + IntToStr(dy) + ')');
  SetAttribute( Result, 'x', '0');
  SetAttribute( Result, 'y', '0');
  SetAttribute( Result, 'nmg2.CodeRef', IntToStr(aCodeRef));
  SetAttribute( Result, 'nmg2.MasterRef', IntToStr(aMasterRef));
  SetAttribute( Result, 'nmg2.InfoFunc', IntTostr(aInfoFunc));
  SetAttribute( Result, 'nmg2.TextFunc', IntTostr(aTextFunc));
  SetAttribute( Result, 'nmg2.CtrlType', aCtrlType);
  SetAttribute( Result, 'nmg2.Dependencies', aDependencies);
end;

function TSVGSkin.CreateConnectorLinkedUse( aNode : TDomNode; aID, aRef : string; dx, dy : integer;
    aCodeRef: integer; aCtrlStyle : string): TDomNode;
begin
  Result := CreateChild( aNode, 'use', aID);
  SetAttribute( Result, 'id', aID);
  SetAttribute( Result, 'xlink:href', '#' + aRef);
  SetAttribute( Result, 'transform', 'translate(' + IntToStr(dx) + ',' + IntToStr(dy) + ')');
  SetAttribute( Result, 'x', '0');
  SetAttribute( Result, 'y', '0');
  SetAttribute( Result, 'nmg2.CodeRef', IntToStr(aCodeRef));
  SetAttribute( Result, 'nmg2.CtrlType', 'Connector');
  SetAttribute( Result, 'nmg2.CtrlStyle', aCtrlStyle);
end;

function TSVGSkin.CreateRect(aNode: TDomNode; aID, aFill,
  aStroke: string; x, y, width, height: integer): TDomNode;
begin
  Result := CreateChild( aNode, 'rect', aID);

  SetAttribute( Result, 'fill', aFill);
  SetAttribute( Result, 'stroke', aStroke);
  SetAttribute( Result, 'x', IntToStr(x));
  SetAttribute( Result, 'y', IntToStr(y));
  SetAttribute( Result, 'width', IntToStr(width));
  SetAttribute( Result, 'height', IntToStr(height));
end;

procedure TSVGSkin.CreateGradientPoint( aNode : TDomNode; aID : string; aOffset : single; aStopColor : string);
var S : TStringStream;
begin
  S := TStringStream.Create(
     '<stop id="' + aID + '" offset="' + FloatToStr(aOffset) + '%" stop-color="' + aStopColor + '" />');
  try
    ReadXMLFragment( aNode, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateLinearGradient( aNode : TDomNode; aID : string): TDomNode;
begin
  Result := CreateChild( aNode, 'linearGradient', aID);
end;

procedure TSVGSkin.CreateModulePanelGradient( aNode : TDomNode; aID : string; aColor1, aColor2 : string);
var StopsNode : TDomNode;
    S : TStringStream;
begin
  StopsNode := CreateLinearGradient( aNode, aID + '_stops');
  CreateGradientPoint( StopsNode, aID + '_start', 0, aColor1);
  CreateGradientPoint( StopsNode, aID + '_stop',  100, aColor2);

  S := TStringStream.Create(
     '<linearGradient'
   + ' id="' + aID + '"'
   + ' gradientUnits="objectBoundingBox"'
   + ' x1="0"'
   + ' x2="1"'
   + ' y1="0"'
   + ' y2="1"'
   + ' xlink:href="#' +aID + '_stops' + '">'
   + '</linearGradient>');
  try
    ReadXMLFragment( aNode, S);
  finally
    S.Free;
  end;
end;

procedure TSVGSkin.CreateModulePanelGradients;
var j : integer;
    color : integer;
    svg_color1, svg_color2 : string;
begin
  for j := 0 to 24 do begin
    color := (ModuleColors[j] and $00FF0000) shr 32
           + (ModuleColors[j] and $0000FF00)
           + (ModuleColors[j] and $000000FF) shl 32;
    svg_color1 := '#' +  IntToHex( Lighter(Color,40), 6);
    svg_color2 := '#' +  IntToHex( Color, 6);
    CreateModulePanelGradient( FDefsNode, 'PanelGradient_' + IntToStr(j), svg_color1{'#F0F0F0'} , svg_color2);
  end;
end;

function TSVGSkin.CreateSymbol( aNode : TDomNode; aID : string; aWidth, aHeight : integer): TDomNode;
var S : TStringStream;
begin
  Result := CreateChild( aNode, 'rect', aID);
  SetAttribute( Result, 'fill', 'none');
  SetAttribute( Result, 'stroke', 'blue');
  SetAttribute( Result, 'x', '0');
  SetAttribute( Result, 'y', '0');
  SetAttribute( Result, 'width', IntToStr(aWidth));
  SetAttribute( Result, 'height', IntToStr(aHeight));

  {  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
   '<rect id="' + aID + '_rect" fill="none" stroke="blue" stroke-width="0.2" x="0" y="0" width="' + IntToStr(aWidth)+ '" height="' + IntToStr(aHeight) + '" />');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;}
end;

function TSVGSkin.CreateSymbolSmallArrowUp : TDomNode;
var S : TStringStream;
    ObjectiD : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  BitMapList.Add(nil);
  ObjectID := 'symbol' + '_' + IntToStr(BitmapList.Count - 1);
  Placeholder := AddToSection( 'symbol_section', ObjectID);

  Result := CreateChild( Placeholder.FNode, 'path', ObjectID);
  SetAttribute( Result, 'fill', 'black');
  SetAttribute( Result, 'stroke', 'none');
  SetAttribute( Result, 'd', 'M0,3 h6 l-3,-3 z');
  SetAttribute( Result, 'width', '6');
  SetAttribute( Result, 'height', '3');
end;

function TSVGSkin.CreateSymbolSmallArrowDown : TDomNode;
var S : TStringStream;
    ObjectiD : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  BitMapList.Add(nil);
  ObjectID := 'symbol' + '_' + IntToStr(BitmapList.Count - 1);
  Placeholder := AddToSection( 'symbol_section', ObjectID);

  Result := CreateChild( Placeholder.FNode, 'path', ObjectID);
  SetAttribute( Result, 'fill', 'black');
  SetAttribute( Result, 'stroke', 'none');
  SetAttribute( Result, 'd', 'M0,0 h6 l-3,3 z');
  SetAttribute( Result, 'width', '6');
  SetAttribute( Result, 'height', '3');
end;

function TSVGSkin.CreateSymbolSmallArrowLeft : TDomNode;
var S : TStringStream;
    ObjectiD : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  BitMapList.Add(nil);
  ObjectID := 'symbol' + '_' + IntToStr(BitmapList.Count - 1);
  Placeholder := AddToSection( 'symbol_section', ObjectID);

  Result := CreateChild( Placeholder.FNode, 'path', ObjectID);
  SetAttribute( Result, 'fill', 'black');
  SetAttribute( Result, 'stroke', 'none');
  SetAttribute( Result, 'd', 'M3,0 v6 l-3,-3 z');
  SetAttribute( Result, 'width', '3');
  SetAttribute( Result, 'height', '6');
end;

function TSVGSkin.CreateSymbolSmallArrowRight : TDomNode;
var S : TStringStream;
    ObjectiD : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  BitMapList.Add(nil);
  ObjectID := 'symbol' + '_' + IntToStr(BitmapList.Count - 1);
  Placeholder := AddToSection( 'symbol_section', ObjectID);

  Result := CreateChild( Placeholder.FNode, 'path', ObjectID);
  SetAttribute( Result, 'fill', 'black');
  SetAttribute( Result, 'stroke', 'none');
  SetAttribute( Result, 'd', 'M0,0 v6 l3,-3 z');
  SetAttribute( Result, 'width', '3');
  SetAttribute( Result, 'height', '6');
end;


function TSVGSkin.CreateLabel( aNode : TDomNode; aID : string; x, y : integer; aText : string; aFontSize : integer): TDomNode;
var S : TStringStream;
    p : integer;
    ObjectID, FontFamily : string;
    TextObject, SpanObject : TDomNode;
begin
  //Result := CreateG( aNode, aID);

  p := pos('&', aText);
  if p>0 then
    aText[p]:= '+';

  FontFamily := 'arial';

  Result := CreateChild( aNode, 'text', aID);
  SetAttribute( Result, 'x', IntToStr(x));
  SetAttribute( Result, 'y', IntToStr(y+aFontSize));
  SetAttribute( Result, 'style', 'font-size:' + IntToStr(aFontSize) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:' + FontFamily);
  //SetAttribute( Result, 'xml:space', 'preserve');

  SpanObject := CreateChild( Result, 'tspan', aID + '_span');
  SetAttribute( SpanObject, 'x', IntToStr(x));
  SetAttribute( SpanObject, 'y', IntToStr(y+aFontSize));
  SpanObject.TextContent := aText;

  {S := TStringStream.Create(
     '<text id="' + aID + '_text' + '" x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '"'
   + ' style="font-size:' + IntToStr(aFontSize) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#000000;fill-opacity:1;stroke:none;font-family:Sans"'
   //+ ' xml:space="preserve">'
   + ' xml:space="default">'
     + '<tspan id="' + aID + '_span"  x="' + IntToStr(x) + '" y="' + IntToStr(y+aFontSize) + '">'
          + aText
     + '</tspan>'
   + '</text>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;}
end;

function TSVGSkin.CreateTextField( aNode : TDomNode; aID : string; x, y, aWidth, aHeight : single): TDomNode;
var S : TStringStream;
    bw, th : integer;
begin
  bw := 1;
  th := 8;
  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
     '<g id="' + aID + '_parts">'
    + ' <rect id="' + aID + '_bevel" fill="gray" stroke="none" x="' + FloatToStr(x) + '" y="' + FloatToStr(y) + '" width="' + FloatToStr(aWidth)+ '" height="' + FloatToStr(aHeight) + '" />'
    + ' <rect id="' + aID + '_window" fill="black" stroke="none" x="' + FloatToStr(x+bw) + '" y="' + FloatToStr(y+bw) + '" width="' + FloatToStr(aWidth - bw*2) + '" height="' + FloatToStr(aHeight - bw*2) + '" />'
     + '<text id="' + aID + '_text' + '"'
       + ' style="font-size:' + IntToStr(th) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:#ffffff;fill-opacity:1;stroke:none;font-family:Arial" xml:space="preserve"'
       + ' x="0" y="' + FloatToStr( aHeight/2 + th/2) + '">'
      + '12Ab'
    + '</text>'
   + '</g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateRoundedButtonTextUp( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth : single): TDomNode;
var S : TStringStream;
begin
  Result := CreateG( aNode, aID);
  SetAttribute( Result, 'nmg2.CtrlType', 'btnstate_off');
  S := TStringStream.Create( RoundedButtonObject( aID, aWidth, aHeight, aBevelWidth, aBevelWidth,
              cBtnSideLight, cBtnSideLight, cBtnSideDark, cBtnSideDark, cBtnSideMedium, cBtnFace));
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateRoundedButtonTextDown( aNode : TDomNode; aID : String; aWidth, aHeight, aBevelWidth : single): TDomNode;
var S : TStringStream;
begin
  Result := CreateG( aNode, aID);
  SetAttribute( Result, 'nmg2.CtrlType', 'btnstate_on');
  S := TStringStream.Create( RoundedButtonObject( aID, aWidth, aHeight, aBevelWidth, aBevelWidth,
              'url(#btnTextGradienSideLeft)', 'url(#btnTextGradienSideTop)', 'url(#btnTextGradienSideRight)', 'url(#btnTextGradienSideBottom)',
              '#00ffcc', 'url(#btnTextGradienFace)'));
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateRoundedButton( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth : single; aSymbolID : string; aSymbolWidth, aSymbolHeight : single): TDomNode;
var Placeholder : TSVGSkinSectionPlaceholder;
    Node : TDomNode;
begin
  if not DefExists(aID + '_up') then begin
    Placeholder := AddToSection( 'btntext_up_section', aID + '_up');
    CreateRoundedButtonTextUp( Placeholder.FNode, aID + '_up', aWidth, aHeight, aBevelWidth);
  end;

  if not DefExists(aID + '_down') then begin
    Placeholder := AddToSection( 'btntext_down_section', aID + '_down');
    CreateRoundedButtonTextDown( Placeholder.FNode, aID + '_down', aWidth, aHeight, aBevelWidth);
  end;

  Result := CreateG( aNode, aID + '_' + aSymbolID);

  CreateUse( Result, aID + '_up_use', aID + '_up', 0, 0);
  CreateUse( Result, aID + '_down_use', aID + '_down', 0, 0);

  if aSymbolID <> '' then begin
    CreateUse( Result,
               aID + '_' + aSymbolID + '_use',
               aSymbolID,
               trunc(aWidth / 2 - aSymbolWidth / 2), trunc(aHeight / 2 - aSymbolHeight / 2));
  end;
end;

function TSVGSkin.CreateButtonFlatButton( aNode : TDomNode; aID : string; aWidth, aHeight : single; aSymbolID : string; aSymbolWidth, aSymbolHeight : single): TDomNode;
var S : TStringStream;
    rx, bw : single;
    w, h : integer;
    th : integer;
    Node : TDomNode;
    svg_text : string;
    lc, fc : string;
begin
  Result := CreateG( aNode, aID);

  bw := 1.2;
  rx := 2;
  th := 7;

  lc := '#e6e6e6';
  fc := '#b3b3b3'; //'#666666'; // cBtnFlatFace

  svg_text :=
      '<rect id="' + aID + '_bg" fill="black" stroke="none" x="0" y="0" width="' + FloatToStr(aWidth) + '" height="' + FloatToStr(aHeight) + '" />'
     + '<rect id="' + aID + '_face" fill="' + fc + '" stroke="none" rx="' + FloatToStr(rx) + '" x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(aWidth-bw*2) + '" height="' + FloatToStr(aHeight-bw*2) + '" />';

    svg_text := svg_text
       + '<use id="' + aID + '_symbol' + '"'
         + ' xlink:href="#' + aSymbolID + '"'
         + ' transform="translate(' + FloatToStr( aWidth / 2 - aSymbolWidth / 2) + ',' + FloatToStr( aHeight / 2 - aSymbolHeight / 2) + ')"'
         + ' x="0" y="0" />';

         {  if aSymbolID = '' then begin
    svg_text := svg_text
      + '<text id="' + aID + '_text' + '"'
        + ' style="font-size:' + IntToStr(th) + 'px;font-style:normal;font-weight:normal;line-height:125%;letter-spacing:0px;word-spacing:0px;fill:' + lc + ';fill-opacity:1;stroke:none;font-family:Arial" xml:space="preserve"'
        + ' x="0" y="' + FloatToStr( aHeight/2 + th/2) + '">'
        + aText
      + '</text>';

  end else begin
    Node := FindDef( aSymbolID);
    w := StrToInt(TDomElement(Node).GetAttribute('width'));
    h := StrToInt(TDomElement(Node).GetAttribute('height'));

    svg_text := svg_text
       + '<use id="' + aID + '_symbol' + '"'
         + ' xlink:href="#' + aSymbolID + '"'
         + ' transform="translate(' + FloatToStr( aWidth / 2 - w div 2) + ',' + FloatToStr( aHeight / 2 - h div 2) + ')"'
         + ' x="0" y="0" />';
  end;}

  S := TStringStream.Create(svg_text);
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateButtonRadio(aNode: TDomNode; aID: string; dx,
  dy: integer; aBtns: TStringList): TDomNode;
var i : integer;
    ObjectNode : TDomNode;
    x, y : integer;
begin
  Result := CreateG( aNode, aID);

  x := 0;
  y := 0;

  for i := 0 to aBtns.Count - 1 do begin
    ObjectNode := CreateUse( Result, aBtns[i] + '_use', aBtns[i], x, y);
    SetAttribute( ObjectNode, 'nmg2.CtrlType', 'option');
    x := x + dx;
    y := y + dy;
  end;
end;

function TSVGSkin.CreateButtonFlat( aNode : TDomNode; aID : string; aBtns : TStringList): TDomNode;
var i : integer;
    ObjectNode : TDomNode;
begin
  Result := CreateG( aNode, aID);

  for i := 0 to aBtns.Count - 1 do begin
    ObjectNode := CreateUse( Result, aBtns[i] + '_use', aBtns[i], 0, 0);
    SetAttribute( ObjectNode, 'nmg2.CtrlType', 'option');
  end;
end;


function TSVGSkin.CreateBtnIncDecHorz( aNode : TDomNode; aID : string): TDomNode;
var S : TStringStream;
    w, h, d : single;
    Object1ID, Object2ID : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  w := 11;
  h := 11;
  d := 1;

  Result := CreateG( aNode, aID);

  Object1ID := GetID3D('btntext', w, h, d);
  if not DefExists(Object1ID + '_symbol_0') then begin
    Placeholder := AddToSection( 'btntext_section', Object1ID + '_symbol_0');
    CreateRoundedButton( Placeholder.FNode, Object1ID, w, h, d, 'symbol_0', 6, 3);
  end;

  Object2ID := GetID3D('btntext', w, h, d);
  if not DefExists(Object2ID + '_symbol_1') then begin
    Placeholder := AddToSection( 'btntext_section', Object2ID + '_symbol_1');
    CreateRoundedButton( Placeholder.FNode, Object2ID, w, h, d, 'symbol_1', 6, 3);
  end;

  S := TStringStream.Create(
        ' <g id="' + aID + '_parts">'
           + ' <use id="' + aID + '_dec' + '"'
              + ' xlink:href="#' + Object1ID + '_symbol_0' + '"'
              + ' nmg2.CtrlType="btn_dec"'
              + ' x="0" y="0" />'

           + ' <use id="' + aID + '_inc' + '"'
              + ' xlink:href="#' + Object2ID + '_symbol_1' + '"'
              + ' nmg2.CtrlType="btn_inc"'
              + ' x="' + FloatToStr(w) + '" y="0" />'
      + ' </g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateBtnIncDecVert( aNode : TDomNode; aID : string): TDomNode;
var S : TStringStream;
    w, h, d : single;
    Object1ID, Object2ID : string;
    Placeholder : TSVGSkinSectionPlaceholder;
begin
  w := 11;
  h := 9;
  d := 1;

  Result := CreateG( aNode, aID);

  Object1ID := GetID3D('btntext', w, h, d);
  if not DefExists(Object1ID + '_symbol_0') then begin
    Placeholder := AddToSection( 'btntext_section', Object1ID + '_symbol_0');
    CreateRoundedButton( Placeholder.FNode, Object1ID, w, h, d, 'symbol_0', 6, 3);
  end;

  Object2ID := GetID3D('btntext', w, h, d);
  if not DefExists(Object2ID + '_symbol_1') then begin
    Placeholder := AddToSection( 'btntext_section', Object2ID + '_symbol_1');
    CreateRoundedButton( Placeholder.FNode, Object2ID, w, h, d, 'symbol_1', 6, 3);
  end;

  S := TStringStream.Create(
        ' <g id="' + aID + '_parts">'
           + ' <use id="' + aID + '_dec' + '"'
              + ' xlink:href="#' + Object1ID + '_symbol_0' + '"'
              + ' nmg2.CtrlType="btn_dec"'
              + ' x="0" y="0" />'

           + ' <use id="' + aID + '_inc' + '"'
              + ' xlink:href="#' + Object2ID + '_symbol_1' + '"'
              + ' nmg2.CtrlType="btn_inc"'
              + ' x="0" y="' + FloatToStr(h) + '" />'
      + ' </g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateKnobSideGradient( aNode : TDomNode; aID : string; r : single): TDomNode;
var S : TStringStream;
begin
{  StopsNode := CreateLinearGradient( aNode, aID + '_stops');
  CreateGradientPoint( StopsNode, aID + '_start', 0, aColor1);
  CreateGradientPoint( StopsNode, aID + '_stop',  100, aColor2);

  S := TStringStream.Create( Gradient( id, r/2, r/2, -r/2, -r/2, '#333333', '#ffffff'));
  try
    ReadXMLFragment( aNode, S);
  finally
    S.Free;
  end;}
end;

function TSVGSkin.CreateCenterBtnOff( aNode : TDomNode): TDomNode;
var S : TStringStream;
    id : string;
    w : single;
begin
  w := 10;

  Result := CreateG(aNode, idKnobCenterBtn + '_off');
  SetAttribute( Result, 'nmg2.CtrlType', 'btnstate_off');

  S := TStringStream.Create(
     '<path id="' + idKnobCenterBtn + '_off_bg" d="M' + FloatToStr((w - 10)/2) + ',0 h10 l -5,4 z" fill="gray" stroke="black" opacity="1" />');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateCenterBtnOn( aNode : TDomNode): TDomNode;
var S : TStringStream;
    id : string;
    w : single;
begin
  w := 10;

  Result := CreateG(aNode, idKnobCenterBtn + '_on');
  SetAttribute( Result, 'nmg2.CtrlType',  'btnstate_on');

  S := TStringStream.Create(
     '<path id="' + idKnobCenterBtn + '_on_bg" d="M' + FloatToStr((w - 10)/2) + ',0 h10 l -5,4 z" fill="lime" stroke="black" opacity="1" />');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateCenterBtn( aNode : TDomNode): TDomNode;
begin
  Result := CreateG(aNode, idKnobCenterBtn);
  CreateUse( Result, idKnobCenterBtn + '_el_1', idKnobCenterBtn + '_off', 0, 0);
  CreateUse( Result, idKnobCenterBtn + '_el_2', idKnobCenterBtn + '_on', 0, 0);
end;

function TSVGSkin.CreateKnobButtons( aNode : TDomNode): TDomNode;
var S : TStringStream;
    id, temp, id_smallarrow_up, id_smallarrow_down : string;
    w, h, bw : single;
begin
  w := 11;
  h := 9;
  bw := 1;

  id_smallarrow_up := 'symbol_0';
  id_smallarrow_down := 'symbol_1';

  Result := CreateG( aNode, idKnobBtns);

  temp :=
      '<rect id="' + idKnobBtns + '_bg" fill="white" stroke="none"'
         + ' x="0" y="0"  width="' + FloatToStr(w*2-bw) + '" height="' + FloatToStr(h) + '"/>'

    + '<rect id="' + idKnobBtns + '_left" fill="' + cBtnFlatFace + '" stroke="none"'
         + ' x="' + FloatToStr(bw) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(w - bw*2) + '" height="' + FloatToStr(h - bw*2) + '"/>'

    + ' <use id="' + idKnobBtns + '_' + id_smallarrow_down + '"'
         + ' xlink:href="#' + id_smallarrow_down + '"'
         + ' transform="translate(' + FloatToStr(w/2 - 6/2) + ',' + FloatToStr(h/2 - 3/2) + ')"'
         + ' x="0" y="0" />'

    +  '<rect id="' + idKnobBtns + '_right" fill="' + cBtnFlatFace + '" stroke="none"'
         + ' x="' + FloatToStr(w) + '" y="' + FloatToStr(bw) + '" width="' + FloatToStr(w - bw*2) + '" height="' + FloatToStr(h - bw*2) + '"/>'

    + ' <use id="' + idKnobBtns + '_' + id_smallarrow_up + '"'
         + ' xlink:href="#' + id_smallarrow_up + '"'
         + ' transform="translate(' + FloatToStr(w - bw + w/2 - 6/2) + ',' + FloatToStr(h/2 - 3/2) + ')"'
         + ' x="0" y="0" />';
  S := TStringStream.Create( temp);
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateKnob( aNode : TDomNode; aID : string; w, h, c_x, c_y, r_side, r_face : single; reset : boolean): TDomNode;
var S : TStringStream;
    svg : string;
begin
  Result := CreateG( aNode, aID);

  svg :=
     '<rect id="' + aID + '_sel" nmg2.CtrlType="selection" x="0" y="0" width="' + FLoatToStr(w) + '" height="' + FloatToStr(h) + '" fill="none" stroke="blue" stroke-width="0.2"/>';

   if reset then begin
     svg := svg
     + '<use id="' + aID + '_reset' + '"'
          + ' xlink:href="#' + idKnobCenterBtn + '"'
          + ' nmg2.CtrlType="reset"'
          + ' transform="translate(' + FloatToStr( c_x - 5) + ',' + FloatToStr(0) + ')"'
          + ' x="0" y="0" />'
   end;

   svg := svg
   + '<g transform="translate(' + FloatToStr(c_x) + ',' + FloatToStr(c_y) + ')">'
     + '<g id="g' + aID + '_face">'
       + '<circle id="' + aID + '_side" nmg2.CtrlType="side" cx="0" cy="0" r="' + FloatToStr(r_side) + '" fill="url(#knobSideGradient)" stroke="' + cKnobBorder + '" stroke-width="1"  />'
       + '<circle id="' + aID + '_face" nmg2.CtrlType="face" cx="0" cy="0" r="' + FloatToStr(r_face) + '" fill="' + cKnobFace + '" stroke="none"/>'
     + '</g>'
     + '<g id="g' + aID + '_needle" nmg2.CtrlType="needle">'
       + '<rect id="' + aID + '_needle" fill="' + cKnobBorder + '" stroke="' + cKnobBorder + '" x="-0.1" y="' + FloatToStr(-(r_face-1)) + '" width="0.2" height="' + FloatToStr(r_face-1) + '" />'
     + '</g>'
     + '<g id="g' + aID + '_morph">'
       + '<path id="' + aID + '_morph" nmg2.CtrlType="morph" d="M0,0 v' + FLoatToStr(-r_side) + ' a' + FloatToStr(r_side) + ',' + FLoatToStr(r_side) + ' 0 0,0 ' + FloatToStr(-r_side) + ',' + FloatToStr(r_side) + ' z" fill="red" stroke="none" opacity="0.5" />'
     + '</g>'
   + '</g>'

   + '<use id="' + aID + '_btns' + '"'
        + ' xlink:href="#' + idKnobBtns + '"'
        + ' nmg2.CtrlType="buttons"'
        + ' transform="translate(' + FloatToStr( c_x - 10.5) + ',' + FloatToStr(h - 9) + ')"'
        + ' x="0" y="0" />';

  S := TStringStream.Create(svg);
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateKnobBig( aNode : TDomNode): TDomNode;
var id : string;
    w, h, c_x, c_y, r_side, r_face : single;
begin
  // url(#linearGradient26483)

  w := 22;
  h := 26;
  r_side := 11;
  r_face := 8;
  c_x := 11;
  c_y := 11;

  id := idKnobBig;

  Result := CreateKnob( aNode, idKnobBig, 22, 26, 11, 11, 11, 8, false);
end;

function TSVGSkin.CreateKnobMedium( aNode : TDomNode): TDomNode;
var id : string;
begin
  id := idKnobMedium;
  Result := CreateKnob( aNode, idKnobMedium, 20, 24, 10, 10, 10, 7, false);
end;

function TSVGSkin.CreateKnobResetMedium( aNode : TDomNode): TDomNode;
var id : string;
begin
  id := idKnobResetMedium;
  Result := CreateKnob( aNode, idKnobResetMedium, 20, 30, 10, 15, 10, 7, true);
end;

function TSVGSkin.CreateKnobReset( aNode : TDomNode): TDomNode;
var id : string;
begin
  id := idKnobReset;
  Result := CreateKnob( aNode, idKnobReset, 18, 26, 9, 14, 9, 6, true);
end;

function TSVGSkin.CreateKnobSmall( aNode : TDomNode): TDomNode;
var id : string;
begin
  id := idKnobSmall;
  Result := CreateKnob( aNode, idKnobSmall, 18, 22, 9, 9, 9, 6, false);
end;

function TSVGSkin.CreateKnobSliderKnob( aNode : TDomNode; aID : string; aWidth, aHeight, aBevelWidth, aBevelHeight : single): TDomNode;
var S : TStringStream;
begin
  Result := CreateG( aNode, aID);
  S := TStringStream.Create( RoundedButtonObject( aID, aWidth, aHeight, aBevelWidth, aBevelWidth,
              cSldrKnobSideLight, cSldrKnobSideLight, cSldrKnobSideDark, cSldrKnobSideDark, cSldrKnobSideMedium, cSldrKnobFace));
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
  {S := TStringStream.Create( NiceButton( idSlider + '_knob', aWidth, aHeight, aBevelWidth, aBevelHeight,
              cSldrKnobSideLight, cSldrKnobSideLight, cSldrKnobSideDark, cSldrKnobSideDark, cSldrKnobSideMedium, cSldrKnobFace));
  try
    ReadXMLFragment( aNode, S);
  finally
    S.Free;
  end;}
end;

function TSVGSkin.CreateKnobSlider( aNode : TDomNode; aID : string): TDomNode;
var S : TStringStream;
    bw : integer;
begin
  bw := 1;

  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
     '<g id="' + idSlider + '_parts">'
     + '<rect id="' + idSlider + '_bevel" fill="gray" stroke="none" x="0" y="0" width="11" height="45" />'
     + '<rect id="' + idSlider + '_face" fill="lightgray" stroke="none" x="' + IntToStr(bw) + '" y="' + IntToStr(bw) + '" width="' + IntToStr(11 - bw*2) + '" height="' + IntToStr(45 - bw*2) + '" />'
     + '<use id="' + idSlider + '_knob_use"'
          + ' xlink:href="#' + idSlider + '_knob' + '"'
          + ' nmg2.CtrlType="knob"'
          + ' x="0" y="0" />'
     + '<use id="' + idSlider + '_btnincdec_use"'
          + ' xlink:href="#' + idBtnIncDecVert + '"'
          + ' nmg2.CtrlType="btns"'
          + ' x="0" y="46" />'
     + '</g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateInConnector( aNode : TDomNode; aID : string; aColor : string): TDomNode;
var S : TStringStream;
begin
  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
     '<rect id="' + aID + '_sel" x="0" y="0" width="10" height="10" fill="none" stroke="blue" stroke-width="0.2"/>'
     + '<g transform="translate(5,5)">'
     + '<g id="' + aID + '_parts">'
       + '<circle id="' + aID + '_border" cx="0" cy="0" r="5" fill="' + aColor + '" stroke="none" opacity="1"  />'
       + '<circle id="' + aID + '_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
     + '</g>'
   + '</g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateOutConnector( aNode : TDomNode; aID : string; aColor : string): TDomNode;
var S : TStringStream;
begin
  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
     '<rect id="' + aID + '_sel" x="0" y="0" width="10" height="10" fill="none" stroke="blue" stroke-width="0.2"/>'
     + '<g transform="translate(5,5)">'
     + '<g id="' + aID + '_parts">'
       + '<rect id="' + aID + '_border" x="-5" y="-5" width="10" height="10" fill="' + aColor + '" stroke="none" opacity="1" />'
       + '<circle id="' + aID + '_hole" cx="0" cy="0" r="3" fill="black" stroke="none" opacity="1"  />'
     + '</g>'
   + '</g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

function TSVGSkin.CreateBackground( aNode : TDomNode; aID : string): TDomNode;
var S : TStringStream;
    rx, bw : single;
begin
  bw := 1;
  rx := 1;
  Result := CreateG( aNode, aID);

  S := TStringStream.Create(
     '<g id="' + aID + '_parts">'
     + '<rect id="' + aID + '_bg" fill="#483737" stroke="none" x="0" y="0" width="' + IntToStr(UNITS_COL) + '" height="' + IntToStr(UNITS_ROW) + '" />'
     + '<rect id="' + aID + '_leftrail" fill="lightgray" stroke="none" x="' + IntToStr(0) + '" y="' + IntToStr(0) + '" width="' + IntToStr(UNITS_ROW) + '" height="' + IntToStr(UNITS_ROW) + '" />'
     + '<rect id="' + aID + '_rightrail" fill="lightgray" stroke="none" x="' + IntToStr(UNITS_COL - UNITS_ROW) + '" y="' + IntToStr(0) + '" width="' + IntToStr(UNITS_ROW) + '" height="' + IntToStr(UNITS_ROW) + '" />'
     + '<circle id="' + aID + '_lefthole" cx="' + FloatToStr(UNITS_ROW/2) + '" cy="' + FloatToStr(UNITS_ROW/2) + '" r="' + FloatToStr(UNITS_ROW * 0.3) + '" fill="#483737" stroke="none"  />'
     + '<circle id="' + aID + '_rightthole" cx="' + FloatToStr(UNITS_COL - UNITS_ROW/2) + '" cy="' + FloatToStr(UNITS_ROW/2) + '" r="' + FloatToStr(UNITS_ROW * 0.3) + '" fill="#483737" stroke="none"  />'
   + '</g>');
  try
    ReadXMLFragment( Result, S);
  finally
    S.Free;
  end;
end;

procedure TSVGSkin.SetAttribute(aNode: TDomNode; aName, aValue: string);
begin
  TDOMElement(aNode).SetAttribute(aName, aValue);
end;

function TSVGSkin.CreateSection( aID : string; aPlaceholderWidth, aPlaceholderHeight, aCols, aRows : integer; aText : string): TSVGSkinSection;
var CommentNode : TDomNode;
begin
  Result := TSVGSkinSection.Create(self, aID, aText);
  Result.FID := aID;
  Result.FPlaceholderWidth := aPlaceholderWidth;
  Result.FPlaceholderHeight := aPlaceholderHeight;
  Result.FRows := aRows;
  Result.FCols := aCols;

  FSections.Add(Result);
end;

//------------------------------------------------------------------------------
//
//                                TSVGSkinSection
//
//------------------------------------------------------------------------------

constructor TSVGSkinSection.Create( aSkin : TSVGSkin; aID, aComment : string);
var CommentNode : TDomNode;
    i, y : integer;
begin
  FSkin := aSkin;
  FID := aID;
  FComment := aComment;

  FPlaceHolders := TObjectList.Create(True);

  y := 0;
  for i := 0 to FSkin.FSections.Count - 1 do begin
    y := y + (FSkin.FSections[i] as TSVGSkinSection).FPlaceholderHeight
           * (FSkin.FSections[i] as TSVGSkinSection).FRows;
  end;

  FNode := FSkin.CreateG( FSkin.FMainNode, FID);
  FSkin.SetAttribute(FNode, 'transform', 'translate(' + IntToStr(0) + ',' + IntToStr(y) + ')');
end;

destructor TSVGSkinSection.Destroy;
begin
  FPlaceHolders.Free;
  inherited;
end;

function TSVGSkinSection.CommentText( aID : string; aText : string): string;
var sl : TStringList;
    i : integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := aText;

    Result := '<text id="' + aID + '">';

    for i := 0 to sl.Count - 1 do begin
      Result := Result
              + '<tspan id="' + aID + '_' + IntTostr(i) + '" x="0" dy="' + IntTostr(10) +'">'
              + sl[i]
              + '</tspan>';
    end;

    Result := Result + '</text>'

  finally
    sl.Free;
  end;
end;

procedure TSVGSkinSection.CreateComment( aNode : TDomNode);
var S : TStringStream;
    CommentNode : TDomNode;
begin
  CommentNode := FSkin.CreateChild(FNode, 'g', FID + '_comment');
  S := TStringStream.Create( CommentText( FID + '_comment_text', FComment));
  try
    ReadXMLFragment( CommentNode, S);
  finally
    S.Free;
  end;
end;

function TSVGSkinSection.CreatePlaceholder( aID : string): TSVGSkinSectionPlaceholder;
var index, x, y : integer;
begin
  index := FPlaceHolders.Count;
  x := (index mod FCols) * FPlaceholderWidth;
  y := (index div FCols) * FPlaceholderHeight;

  Result := TSVGSkinSectionPlaceHolder.Create( self, aID, x, y);
  FPlaceHolders.Add( Result);
end;

//------------------------------------------------------------------------------
//
//                          TSVGSkinSectionPlaceHolder
//
//------------------------------------------------------------------------------

constructor TSVGSkinSectionPlaceHolder.Create( aSection : TSVGSkinSection; aID : string; x, y : integer);
begin
  FID := aID;
  FSection := aSection;
  FSkin := FSection.FSkin;

  FNode := FSKin.CreateG(FSection.FNode, FID);
  FSkin.SetAttribute( FNode, 'transform', 'translate(' + IntToStr(x) + ',' + IntToStr(y) + ')');
end;

destructor TSVGSkinSectionPlaceHolder.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
//
//                                  TSVGObject
//
//------------------------------------------------------------------------------

constructor TSVGObject.Create(aPlaceholder: TSVGSkinSectionPlaceHolder; aID: string);
begin
  FPlaceholder := aPlaceholder;
  FSkin := aPlaceholder.FSkin;
  FID := aID;
  FNode := nil;
end;

destructor TSVGObject.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------
//
//                                  TSVGG2Module
//
//------------------------------------------------------------------------------

constructor TSVGG2Module.Create(aPlaceholder : TSVGSkinSectionPlaceHolder; aID: string; aModule : TG2GraphModule);
var ModuleNode, DescNode, PanelNode : TDomNode;
    Control : TG2GraphChildControl;
    TextField : TG2GraphDisplay;
    BtnText : TG2GraphButtonText;
    BtnFlat : TG2GraphButtonFlat;
    BtnRadio : TG2GraphButtonRadio;
    Connector : TG2GraphConnector;
    i, j, t, w, h, dx, dy, x, y, symbol_width, symbol_height : integer;
    Placeholder : TSVGSkinSectionPlaceholder;
    ControlID, ObjectID, ChildObjectID, ButtonID, SymbolID, CtrlStyle : string;
    slObjectID : TStringlist;
    ConnLinkNode, ParamLinkNode, ObjectNode : TDomNode;
begin
  inherited Create( aPlaceHolder, aID);

  ModuleNode := FSkin.CreateG( FPlaceholder.FNode, aID);
  DescNode := FSkin.CreateDesc( ModuleNode, aID + '_desc', 'Module ' + IntToStr(aModule.TypeID) + ', ' + aModule.ModuleName);

  PanelNode := FSkin.CreateRect( ModuleNode,
                                 aID + '_panel',
                                 'url(#PanelGradient_0)', 'black',
                                 0, 0, aModule.Panel.Width, aModule.Panel.Height);

  ObjectID := FSKin.GetLabelID('module_name', aModule.ModuleName);
  if not FSkin.DefExists(ObjectID) then begin
    Placeholder := FSkin.AddToSection( 'module_names_section', ObjectID);
    FSkin.CreateLabel( Placeholder.FNode, ObjectID, 0, 0, aModule.ModuleName, 10);
  end;
  FSkin.CreateUse( ModuleNode, ObjectID + '_use', ObjectID, 1, 1);


  for i := 0 to aModule.Panel.ChildControlsCount - 1 do begin
    ControlID := FID + '_ctrl_' + IntToStr(i);


    Control :=  aModule.Panel.GraphChildControls[i];

    if Control is TG2GraphLabel then begin

      ObjectID := FSKin.GetLabelID('module_label', (Control as TG2GraphLabel).Caption);
      if not FSkin.DefExists(ObjectID) then begin
        Placeholder := FSkin.AddToSection( 'module_labels_section', ObjectID);
        FSkin.CreateLabel( Placeholder.FNode, ObjectID, 0, 0, (Control as TG2GraphLabel).Caption, (Control as TG2GraphLabel).Font.Size);
      end;
      FSkin.CreateUse( ModuleNode, ControlID, ObjectID, Control.Left, Control.Top);
    end;

    if Control is TG2GraphDisplay then begin
      TextField := Control as TG2GraphDisplay;
      ObjectID := FSkin.GetID2D('textfield', Control.Width, Control.Height);
      if not FSkin.DefExists(ObjectID) then begin
        Placeholder := FSkin.AddToSection( 'textfield_section', ObjectID);
        FSKin.CreateTextField( Placeholder.FNode, ObjectID, 0, 0, Control.Width, Control.Height);
      end;
      {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                              FID + '_paramlink_' + IntToStr(Control.ID),
                                              0,
                                              TextField.MasterRef,
                                              0,
                                              TextField.TextFunction,
                                              'textfield',
                                              TextField.Dependencies.DelimitedText);
      FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, Control.Left, Control.Top);}
      ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                   ControlID,
                                                   ObjectID,
                                                   Control.Left,
                                                   Control.Top,
                                                   0,
                                                   TextField.MasterRef,
                                                   0,
                                                   TextField.TextFunction,
                                                   'textfield',
                                                   TextField.Dependencies.DelimitedText);
    end;

    if Control is TG2GraphButtonText then begin

      BtnText := Control as TG2GraphButtonText;

      if BtnText.ImageList.Count > 0 then begin

        symbol_width := BtnText.ImageList.Items[0].Width;
        symbol_height := BtnText.ImageList.Items[0].Height;

        SymbolID := '';
        for j := 0 to BtnText.ImageList.Count - 1 do begin
          SymbolID := 'symbol_' + IntToStr(FSkin.AddSymbolFromBitmap( BtnText.ImageList.Items[j]));
        end;

      end else begin
        symbol_width := BtnText.Width;
        symbol_height := BtnText.Height;

        SymbolID := FSKin.GetLabelID('btntext_label', BtnText.Parameter.SelectedButtonText);
        if not FSkin.DefExists(SymbolID) then begin
          Placeholder := FSkin.AddToSection( 'btntext_labels_section', SymbolID);
          FSkin.CreateLabel( Placeholder.FNode, SymbolID, 0, 0, BtnText.Parameter.SelectedButtonText, 7);
        end;
      end;

      //ObjectID := FSKin.GetID4D('btntext', BtnText.Width, BtnText.Height, 2, SymbolID);
      ButtonID := FSKin.GetID3D('btntext', BtnText.Width, BtnText.Height, 2);
      ObjectID := ButtonID + '_' + SymbolID;
      if not FSkin.DefExists(ObjectID) then begin
        Placeholder := FSkin.AddToSection( 'btntext_section', ObjectID);
        FSKin.CreateRoundedButton( Placeholder.FNode, ButtonID, BtnText.Width, BtnText.Height, 2, SymbolID, symbol_width, symbol_height);
      end;
      {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                              FID + '_paramlink_' + IntToStr(Control.ID),
                                              Control.Parameter.ParamIndex,
                                              0,
                                              Control.Parameter.InfoFunctionIndex,
                                              0,
                                              'btntext',
                                              '');
      FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, BtnText.Left, BtnText.Top);}
      ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                   ControlID,
                                                   ObjectID,
                                                   BtnText.Left,
                                                   BtnText.Top,
                                                   Control.Parameter.ParamIndex,
                                                   0,
                                                   Control.Parameter.InfoFunctionIndex,
                                                   0,
                                                   'btntext',
                                                   '');

    end;

    if Control is TG2GraphButtonIncDec then begin
      if (Control as TG2GraphButtonIncDec).Orientation = otHorizontal then begin

        ObjectID := idBtnIncDecHorz;
        if not FSkin.DefExists(ObjectID) then begin
          Placeholder := FSkin.AddToSection( 'btnincdec_section', ObjectID);
          FSKin.CreateBtnIncDecHorz( Placeholder.FNode, ObjectID);
        end;
        {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                                FID + '_paramlink_' + IntToStr(Control.ID),
                                                Control.Parameter.ParamIndex,
                                                0,
                                                Control.Parameter.InfoFunctionIndex,
                                                0,
                                                'btnincdec',
                                                '');
        FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, Control.Left, Control.Top);}
        ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                     ControlID,
                                                     ObjectID,
                                                     Control.Left,
                                                     Control.Top,
                                                     Control.Parameter.ParamIndex,
                                                     0,
                                                     Control.Parameter.InfoFunctionIndex,
                                                     0,
                                                     'btnincdec',
                                                     '');

      end else begin
        ObjectID := idBtnIncDecVert;
        if not FSkin.DefExists(ObjectID) then begin
          Placeholder := FSkin.AddToSection( 'btnincdec_section', ObjectID);
          FSKin.CreateBtnIncDecVert( Placeholder.FNode, ObjectID);
        end;
        {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                                FID + '_paramlink_' + IntToStr(Control.ID),
                                                Control.Parameter.ParamIndex,
                                                0,
                                                Control.Parameter.InfoFunctionIndex,
                                                0,
                                                'btnincdec',
                                                '');
         FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, Control.Left, Control.Top);}
        ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                     ControlID,
                                                     ObjectID,
                                                     Control.Left,
                                                     Control.Top,
                                                     Control.Parameter.ParamIndex,
                                                     0,
                                                     Control.Parameter.InfoFunctionIndex,
                                                     0,
                                                     'btnincdec',
                                                     '');

      end;
    end;

    if Control is TG2GraphButtonFlat then begin

      slObjectID := TStringList.Create;
      try

        if Control is TG2GraphLevelShift then begin
          //CreateUse( GModuleNode, control_id, idLevelShift, Control.Left, Control.Top);
        end else begin
          BtnFlat := Control as TG2GraphButtonFlat;

          ObjectID := 'btnflat_' + IntToStr(BtnFlat.Parameter.ParamID);

          if (BtnFlat.ImageList.Count > 0) then begin
            for j := 0 to BtnFlat.ImageList.Count - 1 do begin

              symbol_width := BtnFlat.ImageList.Items[j].Width;
              symbol_height := BtnFlat.ImageList.Items[j].Height;
              SymbolID := 'symbol_' + IntToStr(FSkin.AddSymbolFromBitmap(  BtnFlat.ImageList.Items[j]));

              ChildObjectID := ObjectID + '_' + IntToStr(j);

              slObjectID.Add( ChildObjectID);
              if not FSkin.DefExists( ChildObjectID) then begin
                Placeholder := FSkin.AddToSection( 'btnflat_btns_section', ChildObjectID);
                FSKin.CreateButtonFlatButton( Placeholder.FNode, ChildObjectID, BtnFlat.Width, BtnFlat.Height, SymbolID, symbol_width, symbol_height);
              end;
            end;
          end else begin
            for j := 0 to BtnFlat.ButtonText.Count - 1 do begin

              SymbolID := FSKin.GetLabelID('btnflat_label', BtnFlat.Parameter.ButtonText[j]);
              if not FSkin.DefExists(SymbolID) then begin
                Placeholder := FSkin.AddToSection( 'btnflat_labels_section', SymbolID);
                FSkin.CreateLabel( Placeholder.FNode, SymbolID, 0, 0, BtnFlat.Parameter.ButtonText[j], 7);
              end;

              ChildObjectID := ObjectID + '_' + IntToStr(j);
              slObjectID.Add(ChildObjectID);
              if not FSkin.DefExists(ChildObjectID) then begin
                Placeholder := FSkin.AddToSection( 'btnflat_btns_section', ChildObjectID);
                FSKin.CreateButtonFlatButton( Placeholder.FNode, ChildObjectID, BtnFlat.Width, BtnFlat.Height, SymbolID, BtnFlat.Width, BtnFlat.Height);
              end;
            end;
          end;

        end;

        if not FSkin.DefExists(ObjectID) then begin
          Placeholder := FSkin.AddToSection( 'btnflat_section', ObjectID);
          FSKin.CreateButtonFlat( Placeholder.FNode, ObjectID, slObjectID);
        end;

        {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                                FID + '_paramlink_' + IntToStr(Control.ID),
                                                Control.Parameter.ParamIndex,
                                                0,
                                                Control.Parameter.InfoFunctionIndex,
                                                0,
                                                'btnflat',
                                                '');
        FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, Control.Left, Control.Top);}
        ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                     ControlID,
                                                     ObjectID,
                                                     Control.Left,
                                                     Control.Top,
                                                     Control.Parameter.ParamIndex,
                                                     0,
                                                     Control.Parameter.InfoFunctionIndex,
                                                     0,
                                                     'btnflat',
                                                     '');

      finally
        slObjectID.Free;
      end;
    end;

    if Control is TG2GraphButtonRadio then begin
      slObjectID := TStringList.Create;
      try
        BtnRadio := Control as TG2GraphButtonRadio;

        ObjectID := 'btnradio_' + IntToStr(BtnRadio.Parameter.ParamID);

        if BtnRadio.ButtonCount > 0 then begin

          if BtnRadio.Orientation = otHorizontal then begin
            w := BtnRadio.Width div BtnRadio.ButtonCount - 1;
            h := BtnRadio.Height;
            if BtnRadio.UpsideDown then begin
              dx := w;
              dy := 0;
            end else begin
              dx := -w;
              dy := 0;
            end;
          end else begin
            w := BtnRadio.Width;
            h := BtnRadio.Height div BtnRadio.ButtonCount - 1;
            if BtnRadio.UpsideDown then begin
              dx := 0;
              dy := h;
            end else begin
              dx := 0;
              dy := -h;
            end;
          end;

          if (BtnRadio.ImageList.Count > 0) then begin
            for j := 0 to BtnRadio.ImageList.Count - 1 do begin

              symbol_width := BtnRadio.ImageList.Items[j].Width;
              symbol_height := BtnRadio.ImageList.Items[j].Height;
              SymbolID := 'symbol_' + IntToStr(FSkin.AddSymbolFromBitmap(  BtnRadio.ImageList.Items[j]));

              ButtonID := FSKin.GetID3D('btntext', w, h, 2);
              //ChildObjectID := FSKin.GetID4D('btntext', w, h, 2, SymbolID);
              ChildObjectID := ButtonID + '_' + SymbolID;

              slObjectID.Add( ChildObjectID);
              if not FSkin.DefExists( ChildObjectID) then begin
                Placeholder := FSkin.AddToSection( 'btntext_section', ChildObjectID);
                FSKin.CreateRoundedButton( Placeholder.FNode, ButtonID, w, h, 2, SymbolID, symbol_width, symbol_height);
              end;
            end;
          end else begin
            for j := 0 to BtnRadio.ButtonText.Count - 1 do begin

              SymbolID := FSKin.GetLabelID('btntext_label', BtnRadio.Parameter.ButtonText[j]);
              if not FSkin.DefExists(SymbolID) then begin
                Placeholder := FSkin.AddToSection( 'btntext_labels_section', SymbolID);
                FSkin.CreateLabel( Placeholder.FNode, SymbolID, 0, 0, BtnRadio.Parameter.ButtonText[j], 7);
              end;

              ButtonID := FSKin.GetID3D('btntext', w, h, 2);
              //ChildObjectID := FSKin.GetID4D('btntext', w, h, 2, SymbolID);
              ChildObjectID := ButtonID + '_' + SymbolID;

              slObjectID.Add( ChildObjectID);
              if not FSkin.DefExists( ChildObjectID) then begin
                Placeholder := FSkin.AddToSection( 'btntext_section', ChildObjectID);
                FSKin.CreateRoundedButton( Placeholder.FNode, ButtonID, w, h, 2, SymbolID, w, h);
              end;
            end;
          end;

          if not FSkin.DefExists(ObjectID) then begin
            Placeholder := FSkin.AddToSection( 'btnradio_section', ObjectID);
            FSKin.CreateButtonRadio( Placeholder.FNode, ObjectID, dx, dy, slObjectID);
          end;

          {FindOrAddBtnRadio( w, h);

          x := BtnRadio.Left;
          y := BtnRadio.Top;
          for m := 0 to BtnRadio.ButtonCount - 1 do begin
            if m = 0 then
              CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_down', w, h), x, y)
            else
              CreateUse( ParamLinkNode, control_id + '_el_'+ IntToStr(m), GetIDBtnRadio( idBtnRadio + '_up', w, h), x, y);
            x := x + dx;
            y := y + dy;
          end;}
        end;

        {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                                FID + '_paramlink_' + IntToStr(Control.ID),
                                                Control.Parameter.ParamIndex,
                                                0,
                                                Control.Parameter.InfoFunctionIndex,
                                                0,
                                                'btnradio',
                                                '');
        FSkin.CreateUse( ParamLinkNode, ControlID, ObjectID, Control.Left, Control.Top);}
        ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                     ControlID,
                                                     ObjectID,
                                                     Control.Left,
                                                     Control.Top,
                                                     Control.Parameter.ParamIndex,
                                                     0,
                                                     Control.Parameter.InfoFunctionIndex,
                                                     0,
                                                     'btnradio',
                                                     '');
      finally
        slObjectID.Free;
      end;
    end;

    if Control is TG2GraphKnob then begin

      {ParamLinkNode := FSkin.CreateParamLink( ModuleNode,
                                              FID + '_paramlink_' + IntToStr(Control.ID),
                                              Control.Parameter.ParamIndex,
                                              0,
                                              Control.Parameter.InfoFunctionIndex,
                                              0,
                                              'knob',
                                              '');}

      if (Control as TG2GraphKnob).KnobType = ktSlider then begin
        //TDOMElement(ParamLinkNode).SetAttribute('nmg2.CtrlStyle', 'slider');
        //FSkin.CreateUse( ParamLinkNode, ControlID, idSlider, Control.Left, Control.Top);
        ObjectID := idSlider;
        CtrlStyle := 'slider';
      end else begin
        case (Control as TG2GraphKnob).KnobType of
          ktBig :
            begin
              //FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', 'big');
              //FSkin.CreateUse( ParamLinkNode, ControlID, idKnobBig, Control.Left, Control.Top);
              ObjectID := idKnobBig;
              CtrlStyle := 'big';
            end;
          ktMedium :
            begin
              //FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', 'medium');
              //FSkin.CreateUse( ParamLinkNode, ControlID, idKnobMedium, Control.Left, Control.Top);
              ObjectID := idKnobMedium;
              CtrlStyle := 'medium';
            end;
          ktResetMedium :
            begin
              //FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', 'resetmedium');
              //FSkin.CreateUse( ParamLinkNode, ControlID, idKnobResetMedium, Control.Left, Control.Top);
              ObjectID := idKnobResetMedium;
              CtrlStyle := 'resetmedium';
            end;
          ktReset :
            begin
              //FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', 'reset');
              //FSkin.CreateUse( ParamLinkNode, ControlID, idKnobReset, Control.Left, Control.Top);
              ObjectID := idKnobReset;
              CtrlStyle := 'reset';
            end;
          ktSmall :
            begin
              //FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', 'small');
              //FSkin.CreateUse( ParamLinkNode, ControlID, idKnobSmall, Control.Left, Control.Top);
              ObjectID := idKnobSmall;
              CtrlStyle := 'small';
            end;
        end;
      end;
      ParamLinkNode := FSkin.CreateParamLinkedUse( ModuleNode,
                                                   ControlID,
                                                   ObjectID,
                                                   Control.Left,
                                                   Control.Top,
                                                   Control.Parameter.ParamIndex,
                                                   0,
                                                   Control.Parameter.InfoFunctionIndex,
                                                   0,
                                                   'knob',
                                                   '');
       FSkin.SetAttribute(ParamLinkNode, 'nmg2.CtrlStyle', CtrlStyle);

    end;

    if Control is TG2GraphConnector then begin
      Connector := Control as TG2GraphConnector;

      //ConnLinkNode := FSkin.CreateConnLink( ModuleNode,
      //                                      FID + '_connlink_' + IntToStr(Control.ID),
      //                                      Connector.Data.ConnectorIndex);

      if Connector.Data.ConnectorKind = ckInput then begin
        case Connector.Data.ConnectorDefColor of
        COLOR_YELLOW :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorIn + '_yellow', Control.Left, Control.Top);
            ObjectID := idConnectorIn + '_yellow';
            CtrlStyle := 'In';
          end;
        COLOR_BLUE :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorIn + '_blue', Control.Left, Control.Top);
            ObjectID := idConnectorIn + '_blue';
            CtrlStyle := 'In';
          end;
        COLOR_RED :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorIn + '_red', Control.Left, Control.Top);
            ObjectID := idConnectorIn + '_red';
            CtrlStyle := 'In';
          end;
        end;
      end;

      if Connector.Data.ConnectorKind = ckOutput then begin
        case Connector.Data.ConnectorDefColor of
        COLOR_YELLOW :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorOut + '_yellow', Control.Left, Control.Top);
            ObjectID := idConnectorOut + '_yellow';
            CtrlStyle := 'Out';
          end;
        COLOR_BLUE :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorOut + '_blue', Control.Left, Control.Top);
            ObjectID := idConnectorOut + '_blue';
            CtrlStyle := 'Out';
          end;
        COLOR_RED :
          begin
            //FSkin.CreateUse( ConnLinkNode, ControlID, idConnectorOut + '_red', Control.Left, Control.Top);
            ObjectID := idConnectorOut + '_red';
            CtrlStyle := 'Out';
          end;
        end;
      end;

      {ConnLinkNode := Doc.CreateElement('g');
      GModuleNode.AppendChild(ConnLinkNode);
      TDOMElement(ConnLinkNode).SetAttribute('id', 'g2_module_' + IntToStr(FModule.TypeID) + '_connlink_' + IntToStr(Control.ID));
      TDOMElement(ConnLinkNode).SetAttribute('nmg2.CodeRef', IntToStr(Connector.Data.ConnectorIndex));}

      ConnLinkNode := FSkin.CreateConnectorLinkedUse( ModuleNode,
                                                      ControlID,
                                                      ObjectID,
                                                      Control.Left,
                                                      Control.Top,
                                                      Connector.Data.ConnectorIndex,
                                                      CtrlStyle);

    end;


  end;
end;

destructor TSVGG2Module.Destroy;
begin
  inherited;
end;

end.
