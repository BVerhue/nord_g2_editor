unit UnitPatchNotes;

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

{$I ..\..\Source\Common\Includes\delphi_version.inc}

interface

uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls,
{$ENDIF}
  JawsCtrls, g2_classes, g2_usb, g2_database, DOM, XMLRead, XMLWrite;

type
  TfrmPatchNotes = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AEdit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure UpdateControls;
  end;

var
  frmPatchNotes: TfrmPatchNotes;

implementation
uses
  UnitG2Editor;

{$R *.dfm}

{ TfrmPatchNotes }

procedure TfrmPatchNotes.AEdit1Change(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedEditG2;
  if not assigned(G2) then
    exit;

  G2.SelectedPatch.MessSetPatchNotes( Memo1.Lines);
end;

procedure TfrmPatchNotes.FormCreate(Sender: TObject);
begin
  FDisableControls := False;

  LoadIniXML;
end;

procedure TfrmPatchNotes.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmPatchNotes.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmPatchNotes.LoadIniXML;
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
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchNotesForm'));
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

procedure TfrmPatchNotes.UpdateControls;
var G2 : TG2;
    OldSelStart : integer;
begin
  //exit; // Doesn't work yet, something goes wrong with carriage returns...

  FDisableControls := True;
  try
    G2 := frmG2Main.SelectedEditG2;
    if not assigned(G2) then
      exit;

    OldSelStart := Memo1.SelStart;
    G2.SelectedPatch.PatchNotes.GetLines( Memo1.Lines);
    Memo1.SelStart := OldSelStart;
  finally
    FDisableControls := False;
  end;

end;

end.
