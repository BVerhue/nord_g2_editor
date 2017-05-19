unit UnitSynthSettings;

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
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.StdCtrls, VCL.ComCtrls, VCL.ExtCtrls,
{$ELSE}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,
{$ENDIF}
  g2_types, g2_classes;

type
  TfrmSynthSettings = class(TForm)
    eSynthName: TEdit;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    udMidiChannelA: TUpDown;
    eMidiChannelA: TEdit;
    cbMidiActiveA: TCheckBox;
    cbMidiActiveB: TCheckBox;
    cbMidiActiveC: TCheckBox;
    cbMidiActiveD: TCheckBox;
    eMidiChannelB: TEdit;
    eMidiChannelC: TEdit;
    eMidiChannelD: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    udMidiChannelB: TUpDown;
    udMidiChannelC: TUpDown;
    udMidiChannelD: TUpDown;
    GroupBox2: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    lbFileFreeMem: TLabel;
    cbMemoryProtect: TCheckBox;
    rgPedalPolarity: TRadioGroup;
    Label6: TLabel;
    Label7: TLabel;
    eGlobalChannel: TEdit;
    eSysexID: TEdit;
    udGlobalChannel: TUpDown;
    udSysexID: TUpDown;
    cbSendClock: TCheckBox;
    cbIgnoreExternalClock: TCheckBox;
    Label8: TLabel;
    cbControllersReceive: TCheckBox;
    cbControllersSend: TCheckBox;
    Label9: TLabel;
    cbProgramChangeSend: TCheckBox;
    cbProgramChangeReceive: TCheckBox;
    lbMasterTune: TLabel;
    eTuneSemi: TEdit;
    eTuneCent: TEdit;
    udTuneSemi: TUpDown;
    udTuneCent: TUpDown;
    Label10: TLabel;
    Label11: TLabel;
    cbGlobaleOctaveShift: TCheckBox;
    rbOctShift1: TRadioButton;
    rbOctShift2: TRadioButton;
    rbOctShift3: TRadioButton;
    rbOctShift4: TRadioButton;
    rbOctShift5: TRadioButton;
    lblControlPedalGain: TLabel;
    udControlPedalGain: TUpDown;
    cbLocalOn: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure SynthChange(Sender: TObject);
    procedure cbMidiActiveAClick(Sender: TObject);
    procedure cbMidiActiveBClick(Sender: TObject);
    procedure cbMidiActiveCClick(Sender: TObject);
    procedure cbMidiActiveDClick(Sender: TObject);
    procedure udTuneCentChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure udTuneSemiChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure udSysexIDChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
    procedure udGlobalChannelChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udMidiChannelDChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udMidiChannelCChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udMidiChannelBChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udMidiChannelAChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure udControlPedalGainChangingEx(Sender: TObject;
      var AllowChange: Boolean; NewValue: SmallInt;
      Direction: TUpDownDirection);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure updateDialog;
  end;

var
  frmSynthSettings: TfrmSynthSettings;

implementation

uses UnitG2Editor;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TfrmSynthSettings }

procedure TfrmSynthSettings.cbMidiActiveAClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if cbMidiActiveA.Checked then
    eMidiChannelA.Text := '1'
  else
    eMidiChannelA.Text := '17';
  udMidiChannelA.Position := StrToInt(eMidiChannelA.Text) - 1;
  SynthChange(self);
end;

procedure TfrmSynthSettings.cbMidiActiveBClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if cbMidiActiveB.Checked then
    eMidiChannelB.Text := '2'
  else
    eMidiChannelB.Text := '17';
  udMidiChannelB.Position := StrToInt(eMidiChannelB.Text) - 1;
  SynthChange(self);
end;

procedure TfrmSynthSettings.cbMidiActiveCClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if cbMidiActiveC.Checked then
    eMidiChannelC.Text := '3'
  else
    eMidiChannelC.Text := '17';
  udMidiChannelC.Position := StrToInt(eMidiChannelC.Text) - 1;
  SynthChange(self);
end;

procedure TfrmSynthSettings.cbMidiActiveDClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if cbMidiActiveD.Checked then
    eMidiChannelD.Text := '4'
  else
    eMidiChannelD.Text := '17';
  udMidiChannelD.Position := StrToInt(eMidiChannelD.Text) - 1;
  SynthChange(self);
end;

procedure TfrmSynthSettings.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmSynthSettings.FormShow(Sender: TObject);
begin
  updateDialog;
end;

procedure TfrmSynthSettings.SynthChange(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    G2.SynthName := AnsiString(eSynthName.Text);

    G2.MidiChannelA := StrToInt(eMidiChannelA.Text) - 1;
    udMidiChannelA.Position := G2.MidiChannelA;

    G2.MidiChannelB := StrToInt(eMidiChannelB.Text) - 1;
    udMidiChannelB.Position := G2.MidiChannelB;

    G2.MidiChannelC := StrToInt(eMidiChannelC.Text) - 1;
    udMidiChannelC.Position := G2.MidiChannelC;

    G2.MidiChannelD := StrToInt(eMidiChannelD.Text) - 1;
    udMidiChannelD.Position := G2.MidiChannelD;

    if eGlobalChannel.Text = 'Off' then
      G2.MidiGlobalChannel := 16
    else
      G2.MidiGlobalChannel := StrToInt( eGlobalChannel.Text) - 1;
    udGlobalChannel.Position := G2.MidiGlobalChannel;

    if eSysExId.Text = 'All' then
      G2.SysExID := 16
    else
      G2.SysExID := StrToInt(eSysExID.Text) - 1;
    udSysExID.Position := G2.SysExID;

    G2.SendClock := BoolToByte( cbSendClock.Checked);
    G2.IgnoreExternalClock := BoolToByte( cbIgnoreExternalClock.Checked);

    G2.ControllersReceive := BoolToByte( cbControllersReceive.Checked);
    G2.ControllersSend := BoolToByte( cbControllersSend.Checked);

    G2.ProgramChangeSend := BoolToByte( cbProgramChangeSend.Checked);
    G2.ProgramChangeReceive := BoolToByte( cbProgramChangeReceive.Checked);

    G2.TuneSemi := IntToByte(StrToInt( eTuneSemi.Text));
    udTuneSemi.Position := StrToInt( eTuneSemi.Text);

    G2.TuneCent := IntToByte(StrToInt( eTuneCent.Text));
    udTuneCent.Position := StrToInt( eTuneCent.Text);

    G2.GlobalOctaveShiftActive := BoolToByte( cbGlobaleOctaveShift.Checked);

    if rbOctShift1.Checked then G2.GlobalOctaveShift := $FE;
    if rbOctShift2.Checked then G2.GlobalOctaveShift := $FF;
    if rbOctShift3.Checked then G2.GlobalOctaveShift := $00;
    if rbOctShift4.Checked then G2.GlobalOctaveShift := $01;
    if rbOctShift5.Checked then G2.GlobalOctaveShift := $02;

    G2.ControlPedalGain := udControlPedalGain.Position;

    G2.LocalOn := BoolToByte( cbLocalOn.Checked);

    G2.PedalPolarity := rgPedalPolarity.ItemIndex;

    G2.MemoryProtect := BoolToByte( cbMemoryProtect.Checked);

    G2.SendSetSynthSettingsMessage;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmSynthSettings.udControlPedalGainChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or ( NewValue < 0) or (NewValue > 32) then begin
    AllowChange := False;
    exit;
  end;

  SynthChange(self);
  AllowChange := True;
end;

procedure TfrmSynthSettings.udGlobalChannelChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eGlobalChannel.Text := IntToStr(NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udMidiChannelAChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eMidiChannelA.Text := IntToStr( NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udMidiChannelBChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eMidiChannelB.Text := IntToStr( NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udMidiChannelCChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eMidiChannelC.Text := IntToStr( NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udMidiChannelDChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eMidiChannelD.Text := IntToStr( NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udSysexIDChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 0) or (NewValue > 16) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eSysExID.Text := IntToStr( NewValue + 1);
  SynthChange(self);
end;

procedure TfrmSynthSettings.udTuneCentChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < -100) or (NewValue > 100) then begin
    AllowChange := False;
    exit;
  end;

  eTuneCent.Text := IntToStr(NewValue);
  SynthChange(self);
  AllowChange := True;
end;

procedure TfrmSynthSettings.udTuneSemiChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < -6) or (NewValue > 6) then begin
    AllowChange := False;
    exit;
  end;

  eTuneSemi.Text := IntToStr(NewValue);
  SynthChange(self);
  AllowChange := True;
end;

procedure TfrmSynthSettings.updateDialog;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    eSynthName.Text := string(G2.SynthName);

    eMidiChannelA.Text := IntToStr(G2.MidiChannelA + 1);
    cbMidiActiveA.Checked := G2.MidiChannelA < 16;
    udMidiChannelA.Position := G2.MidiChannelA;

    eMidiChannelB.Text := IntToStr(G2.MidiChannelB + 1);
    cbMidiActiveB.Checked := G2.MidiChannelB < 16;
    udMidiChannelB.Position := G2.MidiChannelB;

    eMidiChannelC.Text := IntToStr(G2.MidiChannelC + 1);
    cbMidiActiveC.Checked := G2.MidiChannelC < 16;
    udMidiChannelC.Position := G2.MidiChannelC;

    eMidiChannelD.Text := IntToStr(G2.MidiChannelD + 1);
    cbMidiActiveD.Checked := G2.MidiChannelD < 16;
    udMidiChannelD.Position := G2.MidiChannelD;

    if G2.MidiGlobalChannel = 16 then
      eGlobalChannel.Text := 'Off'
    else
      eGlobalChannel.Text := IntToStr(G2.MidiGlobalChannel + 1);
    udGlobalChannel.Position := G2.MidiGlobalChannel;

    if G2.SysExID = 16 then
      eSysExID.Text := 'All'
    else
      eSysExID.Text := IntToStr(G2.SysExID + 1);
    udSysExID.Position := G2.SysExID;

    cbSendClock.Checked := G2.SendClock = 1;
    cbIgnoreExternalClock.Checked := G2.IgnoreExternalClock = 1;

    cbControllersReceive.Checked := G2.ControllersReceive = 1;
    cbControllersSend.Checked := G2.ControllersSend = 1;

    cbProgramChangeSend.Checked := G2.ProgramChangeSend = 1;
    cbProgramChangeReceive.Checked := G2.ProgramChangeReceive = 1;

    eTuneSemi.Text := IntToStr(ByteToInt(G2.TuneSemi));
    udTuneSemi.Position := StrToInt(eTuneSemi.Text);
    eTuneCent.Text := IntToStr(ByteToInt(G2.TuneCent));
    udTuneCent.Position := StrToInt(eTuneCent.Text);

    cbGlobaleOctaveShift.Checked := G2.GlobalOctaveShift = 1;

    case G2.GlobalOctaveShift of
    $FE : rbOctShift1.Checked := True;
    $FF : rbOctShift2.Checked := True;
    $00 : rbOctShift3.Checked := True;
    $01 : rbOctShift4.Checked := True;
    $02 : rbOctShift5.Checked := True;
    end;

    rbOctShift1.Enabled := cbGlobaleOctaveShift.Checked;
    rbOctShift2.Enabled := cbGlobaleOctaveShift.Checked;
    rbOctShift3.Enabled := cbGlobaleOctaveShift.Checked;
    rbOctShift4.Enabled := cbGlobaleOctaveShift.Checked;
    rbOctShift5.Enabled := cbGlobaleOctaveShift.Checked;

    lblControlPedalGain.Caption := 'Control pedal gain: x1.' + IntToStr(trunc( 50 * G2.ControlPedalGain / 32));
    udControlPedalGain.Position := G2.ControlPedalGain;

    cbLocalOn.Checked := G2.LocalOn = 1;

    rgPedalPolarity.ItemIndex := G2.PedalPolarity;

    cbMemoryProtect.Checked := G2.MemoryProtect = 1;
  finally
    FDisableControls := False;
  end;
end;

end.
