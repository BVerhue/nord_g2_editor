unit UnitPatchSettings;

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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinAPI.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}
{$IFDEF G2_VER220_up}
  WinAPI.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls, VCL.ExtCtrls,
{$ELSE}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
  g2_types, g2_file, g2_graph, g2_classes, g2_midi,
  UnitG2Editor, g2_database, DOM, XMLRead, XMLWrite;

type
  TfrmPatchSettings = class(TForm)
    G2GraphPanel1: TG2GraphPanel;
    G2GraphLabel1: TG2GraphLabel;
    G2GraphLabel2: TG2GraphLabel;
    G2GraphLabel3: TG2GraphLabel;
    G2GraphLabel4: TG2GraphLabel;
    G2GraphLabel5: TG2GraphLabel;
    G2GraphLabel6: TG2GraphLabel;
    kBendRange: TG2GraphKnob;
    kGlideSpeed: TG2GraphKnob;
    kVibratoDepth: TG2GraphKnob;
    kVibratoRate: TG2GraphKnob;
    obArpeggiatorOnOff: TG2GraphButtonRadio;
    obArpOctaves: TG2GraphButtonRadio;
    obArpSpeed: TG2GraphButtonRadio;
    obBendOnOff: TG2GraphButtonRadio;
    obGlideType: TG2GraphButtonRadio;
    obOctaveShift: TG2GraphButtonRadio;
    obSustainPedalOnOff: TG2GraphButtonRadio;
    obVibratoMod: TG2GraphButtonRadio;
    obArpDir: TG2GraphButtonRadio;
    G2GraphLabel7: TG2GraphLabel;
    procedure PatchCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure UpdateControls;
    procedure UpdateColorSchema;
  end;

var
  frmPatchSettings: TfrmPatchSettings;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TfrmPatchSettings.FormCreate(Sender: TObject);
begin
  LoadIniXML;
end;

procedure TfrmPatchSettings.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmPatchSettings.LoadIniXML;
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
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchSettingsForm'));
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

procedure TfrmPatchSettings.PatchCtrlMouseUp(Sender: TObject;  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(frmG2Main) and (Sender is TG2GraphChildControl) then
    frmG2Main.ParameterClick( Sender, Button, Shift, X, Y, (Sender as TG2GraphChildControl).Parameter);
end;

procedure TfrmPatchSettings.UpdateControls;
var Patch : TG2Patch;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin

    UpdateColorSchema;

    Patch := G2.SelectedPatch;

    obSustainPedalOnOff.Parameter := Patch.Parameter[ PATCH_SUSTAIN, SUSTAIN_PEDAL];
    obOctaveShift.Parameter       := Patch.Parameter[ PATCH_SUSTAIN, OCTAVE_SHIFT];
    obBendOnOff.Parameter         := Patch.Parameter[ PATCH_BEND, BEND_ON_OFF];
    kBendRange.Parameter          := Patch.Parameter[ PATCH_BEND, BEND_RANGE];
    obArpeggiatorOnOff.Parameter  := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_ON_OFF];
    obArpOctaves.Parameter        := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_OCTAVES];
    obArpDir.Parameter            := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_DIRECTION];
    obArpSpeed.Parameter          := Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED];
    obGlideType.Parameter         := Patch.Parameter[ PATCH_GLIDE, GLIDE_TYPE];
    kGlideSpeed.Parameter         := Patch.Parameter[ PATCH_GLIDE, GLIDE_SPEED];
    obVibratoMod.Parameter        := Patch.Parameter[ PATCH_VIBRATO, VIBRATO_MOD];
    kVibratoRate.Parameter        := Patch.Parameter[ PATCH_VIBRATO, VIBRATO_RATE];
    kVibratoDepth.Parameter       := Patch.Parameter[ PATCH_VIBRATO, VIBRATO_DEPTH];
  end;
end;

procedure TfrmPatchSettings.UpdateColorSchema;
begin
  obSustainPedalOnOff.HightlightColor := G_HighlightColor;
  obOctaveShift.HightlightColor := G_HighlightColor;
  obBendOnOff.HightlightColor := G_HighlightColor;
  obArpeggiatorOnOff.HightlightColor := G_HighlightColor;
  obArpOctaves.HightlightColor := G_HighlightColor;
  obArpDir.HightlightColor := G_HighlightColor;
  obArpSpeed.HightlightColor := G_HighlightColor;
  obGlideType.HightlightColor := G_HighlightColor;
  obVibratoMod.HightlightColor := G_HighlightColor;
end;

end.
