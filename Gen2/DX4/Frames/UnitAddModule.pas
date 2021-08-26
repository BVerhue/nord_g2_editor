unit UnitAddModule;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

{$I ..\Common\CompilerSettings.Inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Rtti,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.Generics.Defaults,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.StdCtrls,
  {$IFDEF VER260Up}
  FMX.Graphics,
  {$ENDIF}
  BVE.NMG2Data,
  BVE.NMG2ControlsFMX,
  BVE.NMG2Types,
  BVE.NMG2GraphFMX,
  UnitSlot;

type
  TFrameAddModule = class(TFrame)
    rbeModulePages: TG2BtnRadioEdit;
    sbAddModule: TVertScrollBox;
    ArrayLayout1: TArrayLayout;
    Layout1: TLayout;
    btInsertModule: TG2BtnText;
    procedure rbeModulePagesPaintElement(Sender: TObject;
      const aElementType: TControlElementType; aElementIndex: Integer;
      aStyleSet: TG2StyleSet);
    procedure sbAddModuleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure sbAddModuleDblClick(Sender: TObject);
    procedure rbeModulePagesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btInsertModuleChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FframeSlot: TframeSlot;
    [Weak] FModuleImageList: TObjectDictionary<Integer, TBitmap>;

    FAddModuleList: TModuleSelectionList;
    procedure SetframeSlot(const Value: TframeSlot);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateControls;

    procedure InsertModule;

    procedure NextModulePage;
    procedure PrevModulePage;
    procedure NextTemplateModule;
    procedure PrevTemplateModule;

    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);

    property ModuleImageList: TObjectDictionary<Integer, TBitmap>
      read FModuleImageList write FModuleImageList;
    property frameSlot: TframeSlot read FframeSlot write SetframeSlot;
  end;

implementation

{$R *.fmx}

procedure TFrameAddModule.btInsertModuleChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    InsertModule;
end;

constructor TFrameAddModule.Create(AOwner: TComponent);
begin
  inherited;

  FAddModuleList := TModuleSelectionList.Create(False);
  FAddModuleList.Scrollbox := sbAddModule;
  sbAddModule.AniCalculations.Animation := True;
  sbAddModule.AniCalculations.BoundsAnimation := True;
  sbAddModule.AniCalculations.TouchTracking := [ttVertical];
end;

destructor TFrameAddModule.Destroy;
begin
  FAddModuleList.Free;
  inherited;
end;

procedure TFrameAddModule.InsertModule;
var
  TypeID: Byte;
  ModuleDef: TG2ModuleDef;
begin
  if FAddModuleList.ItemIndex = -1 then
    Exit;

  if FframeSlot.SelectedLocation = ltPatch then
    Exit;

  TypeID := FAddModuleList[FAddModuleList.ItemIndex].TypeID;
  ModuleDef := GetDataModuleDef(TypeID);

  FframeSlot.Patch.MessAddModule(FframeSlot.SelectedLocation, TypeID,
    FframeSlot.ModuleCursor.Col, FframeSlot.ModuleCursor.Row);
  FframeSlot.ModuleCursor.Row := FframeSlot.ModuleCursor.Row + ModuleDef.Height;
end;

procedure TFrameAddModule.NextModulePage;
begin
  if rbeModulePages.Value = 15 then
    rbeModulePages.Value := 0
  else
    rbeModulePages.Value := rbeModulePages.Value + 1;

  UpdateControls;
end;

procedure TFrameAddModule.NextTemplateModule;
begin
  if (FAddModuleList.ItemIndex + 1) < FAddModuleList.Count then
    FAddModuleList.ItemIndex := FAddModuleList.ItemIndex + 1;
end;

procedure TFrameAddModule.PrevModulePage;
begin
  if rbeModulePages.Value = 0 then
    rbeModulePages.Value := 15
  else
    rbeModulePages.Value := rbeModulePages.Value - 1;

  UpdateControls;
end;

procedure TFrameAddModule.PrevTemplateModule;
begin
  if (FAddModuleList.ItemIndex - 1) >= 0 then
    FAddModuleList.ItemIndex := FAddModuleList.ItemIndex - 1;
end;

procedure TFrameAddModule.rbeModulePagesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  rbeModulePages.ProcessMouseUp(Sender, Shift, 0, X, Y);
  UpdateControls;
end;

procedure TFrameAddModule.rbeModulePagesPaintElement(Sender: TObject;
  const aElementType: TControlElementType; aElementIndex: Integer;
  aStyleSet: TG2StyleSet);
begin
  if aStyleSet.State <> csSelected then
  begin
    case aElementType of
      ceBackGround:
        aStyleSet.Fill.Color :=
          ConvertToAlpha(ModulePageColors[aElementIndex, 0]);
      ceText:
        aStyleSet.FontColor :=
          ConvertToAlpha(ModulePageColors[aElementIndex, 1]);
    end;
  end;
end;

procedure TFrameAddModule.sbAddModuleDblClick(Sender: TObject);
begin
  InsertModule;
end;

procedure TFrameAddModule.sbAddModuleMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  i: Integer;
  P: TPointF;
  ModuleDef: TG2ModuleDef;
begin
  FAddModuleList.ItemIndex := -1;
  P := sbAddModule.LocalToAbsolute(PointF(X, Y));
  for i := 0 to FAddModuleList.Count - 1 do
  begin
    if FAddModuleList[i].PointInObject(P.X, P.Y) then
    begin
      FAddModuleList.ItemIndex := i;

      ModuleDef := GetDataModuleDef
        (FAddModuleList[FAddModuleList.ItemIndex].TypeID);

      frameSlot.ModuleCursor.UnitsHeight := ModuleDef.Height;
      Exit;
    end;
  end;
end;

procedure TFrameAddModule.SetframeSlot(const Value: TframeSlot);
begin
  FframeSlot := Value;
end;

procedure TFrameAddModule.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  rbeModulePages.StateStyleList := aStateStyleList;
  btInsertModule.StateStyleList := aStateStyleList;
end;

procedure TFrameAddModule.UpdateControls;
var
  Module: TG2GraphModuleFMX;
  ModuleDefStream: TModuleDefStream;
  Synth: TG2GraphFMX;
  Item: TModuleListItem;
  Pos: Single;
  PageNo, i: Integer;
  ModuleDef: TG2ModuleDef;
begin
  sbAddModule.BeginUpdate;
  try
    PageNo := rbeModulePages.Value;

    FAddModuleList.ItemIndex := -1;
    for i := 0 to FAddModuleList.Count - 1 do
      FAddModuleList[i].DisposeOf;
    FAddModuleList.Clear;

    FAddModuleList.FillColor := ConvertToAlpha(ModulePageColors[PageNo, 0]);

    Pos := 0;
    i := 0;
    for i in FModuleImageList.keys do
    begin
      ModuleDef := GetDataModuleDef(i);
      if ord(ModuleDef.Page) = rbeModulePages.Value then
      begin
        Item := TModuleListItem.Create(sbAddModule);
        Item.Parent := sbAddModule;

        FAddModuleList.Add(Item);

        Item.TypeID := i;
        Item.PageIndex := ModuleDef.PageIndex;
        Item.Fill.Color := FAddModuleList.FillColor;
        Item.ImageLabel.LabelText := ModuleDef.ModuleName;
        Item.ImageLabel.StateStyleList.StateStyle[csDefault].FontColor :=
          ConvertToAlpha(ModulePageColors[PageNo, 1]);
        Item.Position.X := 0;
        Item.Position.Y := Pos;
        Item.Width := sbAddModule.Width;
        Item.Height := 20 + ModuleDef.Height * (UNITS_ROW + UNIT_MARGIN * 2) *
          Item.Width / (UNITS_COL + UNIT_MARGIN * 2);
        Item.HitTest := False;
        Pos := Pos + Item.Height;
        Item.Image.Bitmap := FModuleImageList[i];
      end;
    end;

    FAddModuleList.Sort(TComparer<TModuleListItem>.Construct(
      function(const L, R: TModuleListItem): Integer
      begin
        if L.PageIndex < R.PageIndex then
          Result := -1
        else if L.PageIndex > R.PageIndex then
          Result := +1
        else
          Result := 0;
      end));

    Pos := 0;
    for Item in FAddModuleList do
    begin
      Item.Position.Y := Pos;
      Pos := Pos + Item.Height;
    end;

    if FAddModuleList.Count > 0 then
      FAddModuleList.ItemIndex := 0;
  finally
    sbAddModule.EndUpdate;
  end;
end;

end.
