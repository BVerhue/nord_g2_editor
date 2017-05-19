unit UnitSelectModule;

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

// Select module control

interface
uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Materials,
  FMX.Types3D,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2ControlsGL,
  BVE.NMG2TexturesGL,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2;

type
  TSelectModuleGL = class;

  TSelectionItem = class(TControlGL)
  private
    FSelectModule: TSelectModuleGL;

    FModuleID: integer;

    FBorder: single;

    FColorMat: TColorMaterial;
    FMesh: TSegmentedMeshGL;

    FLabel: TTexControlGL;
    FImg: TTexControlGL;

    FLabelMatrix: TMatrix3D;
    FSelected: boolean;

    procedure CreateVertices;
    function GetImgTextureID: string;
    procedure SetImgTextureID(const Value: string);
    procedure SetSelected(const Value: boolean);
    procedure SetImgTextureList(const Value: TTextureListGL);
    function GetImgTextureList: TTextureListGL;
    function GetLabelTextureID: string;
    function GetLabelTextureList: TTextureListGL;
    procedure SetLabelTextureID(const Value: string);
    procedure SetLabelTextureList(const Value: TTextureListGL);protected
    function GetWidth: single; override;
    function GetHeight: single; override;

    procedure Resize3D; override;
    procedure DoClk(Shift: TShiftState; X, Y: Single); override;
  public
    constructor Create(aSelectModule: TSelectModuleGL); reintroduce;
    destructor Destroy; override;

    procedure RenderControl(aContext: TContext3D); override;

    property ModuleID: integer read FModuleID write FModuleID;
    property ItemLabel: TTexControlGL read FLabel;
    property Border: single read FBorder write FBorder;
    property LabelTextureID: string read GetLabelTextureID write SetLabelTextureID;
    property LabelTextureList: TTextureListGL read GetLabelTextureList write SetLabelTextureList;
    property ImgTextureID: string read GetImgTextureID write SetImgTextureID;
    property ImgTextureList: TTextureListGL read GetImgTextureList write SetImgTextureList;
    property Selected: boolean read FSelected write SetSelected;
  end;

  TSelectModuleGL = class(TG2PanelGL)
  private
    FItems: TList<TSelectionItem>;
    FPage: integer;
    FBtnPages: TBtnRadioEditGL;

    FOnAddModule: TNotifyEvent;

    procedure SetPage(const Value: integer);
    procedure BtnPagesClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure CreateItems;
  protected
    procedure SetTextureList(const Value: TTextureListGL); override;

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CalcLayout;

    procedure AddModule(const aModuleID: byte);

    procedure UnselectAll;

    property Page: integer read FPage write SetPage;
    property OnAddModule: TNotifyEvent read FOnAddModule write FOnAddModule;
  end;

implementation
uses
  FMX.Forms,
  BVE.NMG2Module,
  BVE.NMG2GraphTypes,
  BVE.NMG2Types;

{ TSelectModuleGL }

constructor TSelectModuleGL.Create(AOwner: TComponent);
begin
  inherited;

  FBtnPages := TBtnRadioEditGL.Create(self);
  FBtnPages.Parent := Self;
  FBtnPages.TextureID := GetBtnTexID(32, 12);
  FBtnPages.TextureList := TextureList;
  FBtnPages.Padding := 1.5;
  FBtnPages.ColCount := 8;
  FBtnPages.RowCount := 2;
  FBtnPages.Texts := 'In-Out;Note;Osc;LFO;Rnd;Env;Filter;FX;Delay;Shaper;Level;Mixer;Switch;Logic;Seq;Midi';
  FBtnPages.OnClk := BtnPagesClk;

  FItems := TList<TSelectionItem>.Create;
  FPage := -1;

  FBtnPages.Value := 0;
  Page := 0;
end;

procedure TSelectModuleGL.CreateItems;
var
  i: integer;
  Item: TSelectionItem;
  TexName: string;
begin
  if assigned(TextureList) then
  begin

    for i := 0 to FItems.Count - 1 do
    begin
      FItems[i].Parent := nil;
      FItems[i].DisposeOf;
    end;
    FItems.Clear;

    for i := 0 to High(ModuleDefs) do
    begin
      if ord(ModuleDefs[i].Page) = FPage then
      begin
        Item := TSelectionItem.Create(self);
        Item.ModuleID := ModuleDefs[i].ModuleID;
        Item.Parent := self;

        TexName := GetLabelTexID(12, ModuleDefs[i].ModuleName);
        Item.LabelTextureList := TextureList;
        Item.LabelTextureID := TexName;

        TexName := 'prev_' + IntToStr(ModuleDefs[i].ModuleID);
        Item.ImgTextureList := TextureList;
        Item.ImgTextureID := TexName;

        FItems.Add(Item);
      end;
    end;

    CalcLayout;
  end;
end;

destructor TSelectModuleGL.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TSelectModuleGL.AddModule(const aModuleID: byte);
var
  ModuleDef: TG2ModuleDef;
begin
  ConMan.SelectedModuleType := aModuleID;

  ModuleDef := GetDataModuleDef(aModuleID);

  ConMan.SelectedConnection.PatchModuleAdd(
    ConMan.SelectedPatchPart,
    aModuleID,
    ConMan.SelectedCol,
    ConMan.SelectedRow);

  ConMan.SelectedRow := ConMan.SelectedRow + ModuleDef.Height;

  if assigned(FOnAddModule) then
    FOnAddModule(Self);
end;

procedure TSelectModuleGL.BtnPagesClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
  Page := aValue;
end;

procedure TSelectModuleGL.Resize;
begin
  inherited;

  CalcLayout;
end;

procedure TSelectModuleGL.CalcLayout;
var
  Col, Row: single;
  Item: TSelectionItem;
  Margin: single;
begin
  Margin := 10;

  FBtnPages.Position :=
    Point3D((Width - FBtnPages.Width) / 2, Margin, 0);

  Col := Margin;
  Row := Margin + FBtnPages.Height + Margin;

  for Item in FItems do
  begin
    if Row + Item.Height > Height then
    begin
      Row := Margin + FBtnPages.Height + Margin;
      Col := Col + Item.Width + Margin;
    end;

    Item.Position := Point3D(Col, Row, 0);

    Row := Row + Item.Height + Margin;
  end;
end;

procedure TSelectModuleGL.SetPage(const Value: integer);
begin
  if Value <> FPage then
  begin
    FPage := Value;
    CreateItems;
  end;
end;

procedure TSelectModuleGL.SetTextureList(const Value: TTextureListGL);
begin
  inherited;
end;

procedure TSelectModuleGL.UnselectAll;
var
  Item: TSelectionItem;
begin
  for Item in FItems do
    Item.Selected := False;
end;

{ TSelectionItem }

constructor TSelectionItem.Create(aSelectModule: TSelectModuleGL);
begin
  inherited Create(aSelectModule);

  FSelectModule := aSelectModule;

  FMesh := TSegmentedMeshGL.Create;
  FColorMat := TColorMaterial.Create;
  FColorMat.Color := TAlphaColorRec.White;

  FLabel := TTexControlGL.Create(Self);
  FImg := TTexControlGL.Create(self);

  Position := Point3D(0.0, 0.0, 0.0);
  FBorder := 4;

  FLabelMatrix := TMatrix3D.Identity;
end;

procedure TSelectionItem.CreateVertices;
var
  dx, dy: single;
begin
  dx := (Width - FLabel.Width) / 2;
  dy := FBorder;
  FLabelMatrix := TMatrix3D.CreateTranslation(Point3D(dx, dy, 0));

  dx := (Width - FImg.TileWidth) / 2;
  dy := ((Height - FLabel.Height - 2 * FBorder) - FImg.TileHeight) / 2;
  dy := dy + FLabel.Height + 2 * FBorder;
  FImg.Position := Point3D(dx, dy, 0);

  FMesh.CreateMesh(Width, Height, 0, 1, 1);
end;

destructor TSelectionItem.Destroy;
begin
  FImg.Free;
  FLabel.Free;

  FColorMat.Free;
  FMesh.Free;

  inherited;
end;

procedure TSelectionItem.DoClk(Shift: TShiftState; X, Y: Single);
begin
  Selected := True;
  inherited;

  FSelectModule.AddModule(FModuleID);
end;

function TSelectionItem.GetHeight: single;
begin
  Result := Border * 4 + FImg.TileHeight + ItemLabel.Height;
end;

function TSelectionItem.GetImgTextureID: string;
begin
  Result := FImg.TextureID;
end;

function TSelectionItem.GetImgTextureList: TTextureListGL;
begin
  Result := FImg.TextureList;
end;

function TSelectionItem.GetLabelTextureID: string;
begin
  Result := FLabel.TextureID;
end;

function TSelectionItem.GetLabelTextureList: TTextureListGL;
begin
  Result := FLabel.TextureList;
end;

function TSelectionItem.GetWidth: single;
begin
  Result := Border * 2 + FImg.TileWidth;;
end;

procedure TSelectionItem.RenderControl(aContext: TContext3D);
var
  SaveMatrix, M: TMatrix3D;
begin
  SaveMatrix := aContext.CurrentMatrix;
  try
    CreateVertices;

    M := SaveMatrix;
    aContext.SetMatrix(M);
    aContext.DrawTriangles(FMesh.VertexBuffer, FMesh.IndexBuffer, FColorMat, 0.8);

    aContext.SetMatrix(M * FLabelMatrix);
    FLabel.RenderControl(aContext);

    aContext.SetMatrix(M * FImg.Matrix);
    FImg.RenderControl(aContext);

  finally
    aContext.SetMatrix(SaveMatrix);
  end;
end;

procedure TSelectionItem.Resize3D;
begin
  inherited;
end;

procedure TSelectionItem.SetImgTextureID(const Value: string);
begin
  if FImg.TextureID <> Value then
  begin
    FImg.TextureID := Value;
    Invalidate;
  end;
end;

procedure TSelectionItem.SetImgTextureList(const Value: TTextureListGL);
begin
  FImg.TextureList := Value;
end;

procedure TSelectionItem.SetLabelTextureID(const Value: string);
begin
  if FLabel.TextureID <> Value then
  begin
    FLabel.TextureID := Value;
    Invalidate;
  end;
end;

procedure TSelectionItem.SetLabelTextureList(const Value: TTextureListGL);
begin
  FLabel.TextureList := Value;
end;

procedure TSelectionItem.SetSelected(const Value: boolean);
begin
  if FSelected <> Value then
  begin
    if Value then
      FSelectModule.UnSelectAll;

    FSelected := Value;
    if FSelected then
      FColorMat.Color := TAlphaColorRec.Cyan
    else
      FColorMat.Color := TAlphaColorRec.White;
    Invalidate;
  end;
end;

end.
