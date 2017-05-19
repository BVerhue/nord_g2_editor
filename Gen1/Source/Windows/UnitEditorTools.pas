unit UnitEditorTools;

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
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.ExtCtrls, VCL.StdCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
{$ENDIF}
  ImgList, g2_types, g2_database, g2_graph, g2_classes, DOM, XMLRead, XMLWrite, g2_midi;

type
  TfrmEditorTools = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    btCablesRed: TG2GraphButtonText;
    btCablesBlue: TG2GraphButtonText;
    btCablesYellow: TG2GraphButtonText;
    btCablesOrange: TG2GraphButtonText;
    btCablesGreen: TG2GraphButtonText;
    btCablesPurple: TG2GraphButtonText;
    btCablesWhite: TG2GraphButtonText;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure PanelClick(Sender: TObject);
    procedure btCablesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FDisableControls : boolean;
    procedure LoadIniXML;
    procedure UpdateControls;
  end;

var
  frmEditorTools: TfrmEditorTools;

implementation

{$R *.dfm}

uses UnitG2Editor;

procedure TfrmEditorTools.FormCreate(Sender: TObject);
begin
  LoadIniXML;
end;

procedure TfrmEditorTools.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmEditorTools.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('EditorToolsForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;


procedure TfrmEditorTools.btCablesClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedEditG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.PatchDescription.RedVisible := btCablesRed.Value;
  G2.SelectedPatch.PatchDescription.BlueVisible := btCablesBlue.Value;
  G2.SelectedPatch.PatchDescription.YellowVisible := btCablesYellow.Value;
  G2.SelectedPatch.PatchDescription.OrangeVisible := btCablesOrange.Value;
  G2.SelectedPatch.PatchDescription.GreenVisible := btCablesGreen.Value;
  G2.SelectedPatch.PatchDescription.PurpleVisible := btCablesPurple.Value;
  G2.SelectedPatch.PatchDescription.WhiteVisible := btCablesWhite.Value;
  frmG2Main.sbFX.Invalidate;
  frmG2Main.sbVA.Invalidate;
end;

procedure TfrmEditorTools.FormShow(Sender: TObject);
var i : integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TPanel then
      (Components[i] as TPanel).Color := ModuleColors[(Components[i] as TPanel).Tag];


end;

procedure TfrmEditorTools.PanelClick(Sender: TObject);
begin
  with Sender as TPanel do
    frmG2Main.SetSelectedModuleColor( (Sender as TPanel).Tag);
end;

procedure TfrmEditorTools.UpdateControls;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedEditG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    btCablesRed.Value := G2.SelectedPatch.PatchDescription.RedVisible;
    btCablesBlue.Value := G2.SelectedPatch.PatchDescription.BlueVisible;
    btCablesYellow.Value := G2.SelectedPatch.PatchDescription.YellowVisible;
    btCablesOrange.Value := G2.SelectedPatch.PatchDescription.OrangeVisible;
    btCablesGreen.Value := G2.SelectedPatch.PatchDescription.GreenVisible;
    btCablesPurple.Value := G2.SelectedPatch.PatchDescription.PurpleVisible;
    btCablesWhite.Value := G2.SelectedPatch.PatchDescription.WhiteVisible;

  finally
    FDisableControls := False;
  end;
end;

end.
