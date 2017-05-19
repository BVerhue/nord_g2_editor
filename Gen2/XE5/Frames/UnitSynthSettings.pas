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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit,
  BVE.NMG2ControlsFMX, BVE.NMG2Types, BVE.NMG2GraphFMX, FMX.Objects;

type
  TframeSynthSettings = class(TFrame, IG2Observer)
    Panel1: TPanel;
    eSynthName: TEdit;
    Panel2: TPanel;
    eMidiChannelA: TEdit;
    eMidiChannelB: TEdit;
    eMidiChannelC: TEdit;
    eMidiChannelD: TEdit;
    Panel3: TPanel;
    eMidiGlobal: TEdit;
    eSysExID: TEdit;
    Panel4: TPanel;
    eTuneCent: TEdit;
    eTuneSemi: TEdit;
    G2Label1: TG2Label;
    G2Label2: TG2Label;
    G2Label3: TG2Label;
    G2Label4: TG2Label;
    G2Label5: TG2Label;
    G2Label6: TG2Label;
    G2Label7: TG2Label;
    G2Label8: TG2Label;
    G2Label9: TG2Label;
    bidMidiCHannelA: TG2BtnIncDec;
    bidMidiChannelB: TG2BtnIncDec;
    bidMidiChannelC: TG2BtnIncDec;
    BidMidiChannelD: TG2BtnIncDec;
    btMidiChannelAEnabled: TG2BtnText;
    btMidiChannelBEnabled: TG2BtnText;
    btMidiChannelCEnabled: TG2BtnText;
    btMidiChannelDEnabled: TG2BtnText;
    bidMidiGlobal: TG2BtnIncDec;
    bidSysExID: TG2BtnIncDec;
    btSendClock: TG2BtnText;
    btIgnoreExtClock: TG2BtnText;
    btCCReceive: TG2BtnText;
    btCCSend: TG2BtnText;
    btProgChangeReceive: TG2BtnText;
    btProgChangeSend: TG2BtnText;
    G2Label10: TG2Label;
    G2Label11: TG2Label;
    tfMasterTune: TG2TextField;
    G2Label12: TG2Label;
    G2Label13: TG2Label;
    bidTuneSemi: TG2BtnIncDec;
    bidTuneCent: TG2BtnIncDec;
    btGlobalOctaveShiftEnable: TG2BtnText;
    rbGlobalOctaveShift: TG2BtnRadio;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    G2Label14: TG2Label;
    G2Label15: TG2Label;
    tfFreeMem: TG2TextField;
    btMemoryProtect: TG2BtnText;
    G2Label16: TG2Label;
    rbPedalPolarity: TG2BtnRadio;
    G2Label17: TG2Label;
    bidCtrlPedalGain: TG2BtnIncDec;
    tfCtrlPedalGain: TG2TextField;
    btLocal: TG2BtnText;
    btMidiDump: TG2BtnText;
    btCCSnapshot: TG2BtnText;
    btMidiGlobalEnabled: TG2BtnText;
    btSysExEnabled: TG2BtnText;
    Rectangle1: TRectangle;
    procedure eSynthNameExit(Sender: TObject);
    procedure eMidiChannelDExit(Sender: TObject);
    procedure eMidiChannelCExit(Sender: TObject);
    procedure eMidiChannelBExit(Sender: TObject);
    procedure eMidiChannelAExit(Sender: TObject);
    procedure bidMidiCHannelAChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure bidMidiChannelBChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure bidMidiChannelCChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure BidMidiChannelDChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btMidiChannelAEnabledChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btMidiChannelBEnabledChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btMidiChannelCEnabledChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btMidiChannelDEnabledChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure eSysExIDExit(Sender: TObject);
    procedure eMidiGlobalExit(Sender: TObject);
    procedure bidSysExIDChangeValue(Sender: TObject; const aValue: Integer);
    procedure bidMidiGlobalChangeValue(Sender: TObject; const aValue: Integer);
    procedure btMidiGlobalEnabledChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btSysExEnabledChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSendClockChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCCReceiveChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCCSendChangeValue(Sender: TObject; const aValue: Integer);
    procedure btIgnoreExtClockChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btProgChangeReceiveChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btProgChangeSendChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure eTuneSemiExit(Sender: TObject);
    procedure eTuneCentExit(Sender: TObject);
    procedure bidTuneSemiChangeValue(Sender: TObject; const aValue: Integer);
    procedure bidTuneCentChangeValue(Sender: TObject; const aValue: Integer);
    procedure tfMasterTuneGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure btGlobalOctaveShiftEnableChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure rbGlobalOctaveShiftChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btLocalChangeValue(Sender: TObject; const aValue: Integer);
    procedure bidCtrlPedalGainChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure tfCtrlPedalGainGetTextFunc(Sender: TObject;
      var aTextFunc: string);
    procedure rbPedalPolarityChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure tfFreeMemGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure btMemoryProtectChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btCCSnapshotChangeValue(Sender: TObject; const aValue: Integer);
    procedure btMidiDumpChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FSynth : TG2GraphFMX;

    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference( aData : IG2Subject);

    procedure SetSynth(const Value: TG2GraphFMX);
  public
    procedure UpdateControls;
    procedure UpdateSynth;

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property Synth : TG2GraphFMX read FSynth write SetSynth;
  end;

implementation

{$R *.fmx}

procedure TframeSynthSettings.bidCtrlPedalGainChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.ControlPedalGain := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidMidiCHannelAChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MidiChannelA := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidMidiChannelBChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MidiChannelB := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidMidiChannelCChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MidiChannelC := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.BidMidiChannelDChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MidiChannelD := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidMidiGlobalChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MidiGlobalChannel := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidSysExIDChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.SysExID := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidTuneCentChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.TuneCent := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.bidTuneSemiChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.TuneSemi := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btCCReceiveChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.ControllersReceive := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btCCSendChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.ControllersSend := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btCCSnapshotChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Synth.SelectedSlot.SendControllerSnapshotMessage;
end;

procedure TframeSynthSettings.btGlobalOctaveShiftEnableChangeValue(
  Sender: TObject; const aValue: Integer);
begin
  Synth.GlobalOctaveShiftActive := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btIgnoreExtClockChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.IgnoreExternalClock := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btLocalChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.LocalOn := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMemoryProtectChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.MemoryProtect := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMidiChannelAEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.MidiChannelA := 16;
  1 : Synth.MidiChannelA := 0;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMidiChannelBEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.MidiChannelA := 16;
  1 : Synth.MidiChannelA := 1;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMidiChannelCEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.MidiChannelA := 16;
  1 : Synth.MidiChannelA := 2;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMidiChannelDEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.MidiChannelA := 16;
  1 : Synth.MidiChannelA := 3;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.btMidiDumpChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Synth.SendDumpMidiMessage;
end;

procedure TframeSynthSettings.btMidiGlobalEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.MidiGlobalChannel := 16;
  1 : Synth.MidiGlobalChannel := 0;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.btProgChangeReceiveChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.ProgramChangeReceive := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btProgChangeSendChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.ProgramChangeSend := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btSendClockChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.SendClock := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.btSysExEnabledChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.SysExID := 16;
  1 : Synth.SysExID := 0;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.eMidiChannelAExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eMidiChannelA.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.MidiChannelA := channel - 1;
  UpdateSynth;
end;

procedure TframeSynthSettings.eMidiChannelBExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eMidiChannelB.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.MidiChannelB := channel;
  UpdateSynth;
end;

procedure TframeSynthSettings.eMidiChannelCExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eMidiChannelC.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.MidiChannelC := channel;
  UpdateSynth;
end;

procedure TframeSynthSettings.eMidiChannelDExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eMidiChannelD.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.MidiChannelD := channel;
  UpdateSynth;
end;

procedure TframeSynthSettings.eMidiGlobalExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eMidiGlobal.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.MidiGlobalChannel := channel;
  UpdateSynth;
end;

procedure TframeSynthSettings.eSynthNameExit(Sender: TObject);
begin
  Synth.SynthName := eSynthName.Text;
  UpdateSynth;
end;

procedure TframeSynthSettings.eSysExIDExit(Sender: TObject);
var channel : integer;
begin
  channel := StrToInt(eSysExID.Text);
  if (channel > 0) and (channel <= 16) then
    Synth.SysExID := channel;
  UpdateSynth;
end;

procedure TframeSynthSettings.eTuneCentExit(Sender: TObject);
var TuneCent : integer;
begin
  TuneCent := StrToInt(eTuneCent.Text);
  if (TuneCent >= -100) and (TuneCent <= 100) then
    Synth.TuneCent := IntToSignedByte(TuneCent);
  UpdateSynth;
end;

procedure TframeSynthSettings.eTuneSemiExit(Sender: TObject);
var TuneSemi : integer;
begin
  TuneSemi := StrToInt(eTuneSemi.Text);
  if (TuneSemi >= -6) and (TuneSemi <= 6) then
    Synth.TuneSemi := IntToSignedByte(TuneSemi);
  UpdateSynth;
end;

procedure TframeSynthSettings.rbGlobalOctaveShiftChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : Synth.GlobalOctaveShift := $FE;
  1 : Synth.GlobalOctaveShift := $FF;
  2 : Synth.GlobalOctaveShift := $00;
  3 : Synth.GlobalOctaveShift := $01;
  4 : Synth.GlobalOctaveShift := $02;
  end;
  UpdateSynth;
end;

procedure TframeSynthSettings.rbPedalPolarityChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  Synth.PedalPolarity := aValue;
  UpdateSynth;
end;

procedure TframeSynthSettings.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeSynthSettings.SetStateStyles(
  aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframeSynthSettings.SetSynth(const Value: TG2GraphFMX);
begin
  if FSynth <> Value then begin
    if assigned(FSynth) then
      FSynth.RemoveObserver(self);

    FSynth := Value;

    if assigned(FSynth) then
      FSynth.RegisterObserver(self);

    UpdateControls;
  end;
end;

procedure TframeSynthSettings.tfCtrlPedalGainGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSynth) then
    aTextFunc := '-'
  else begin
    aTextFunc := 'x1.' + IntToStr(trunc( 50 * Synth.ControlPedalGain / 32));
  end;
end;

procedure TframeSynthSettings.tfFreeMemGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSynth) then
    aTextFunc := '-'
  else begin
    aTextFunc := '-';
  end;
end;

procedure TframeSynthSettings.tfMasterTuneGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSynth) then
    aTextFunc := '-'
  else begin
    aTextFunc := FreqDispValue(1, 69 + Synth.TuneSemi, 64 + Synth.TuneCent);
  end;
end;

procedure TframeSynthSettings.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange: ;
    EvtUSBError: ;
    EvtBeforeSendMessage: ;
    EvtReceiveResponseMessage: ;
    EvtNextInitStep: ;
    EvtAfterG2Init: ;
    EvtAfterPerfInit: ;
    EvtAfterSlotInit: ;
    EvtPerfsSettingsUpdate: ;
    EvtPerfUpdate: ;
    EvtSynthSettingsUpdate:
      begin
        UpdateControls;
      end;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate: ;
    EvtVariationChange: ;
    EvtCopyVariation: ;
    EvtMidiClockReceive: ;
    EvtClockRunChange: ;
    EvtClockBPMChange: ;
    EvtMidiCCRecieve: ;
    EvtAfterGetAssignedVoices: ;
    EvtPatchLoadChange: ;
    EvtSelectSlot: ;
    EvtSelectLocation: ;
    EvtSelectModule: ;
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtDeleteModule: ;
    EvtAfterRetreivePatch: ;
    EvtAfterBankList: ;
    EvtAfterStore: ;
    EvtAfterClear: ;
    EvtAfterClearBank: ;
    EvtAfterBankDownload: ;
    EvtDeassignKnob: ;
    EvtAssignKnob: ;
    EvtDeassignGlobalKnob: ;
    EvtAssignGlobalKnob: ;
  end;
end;

procedure TframeSynthSettings.UpdateControls;
begin
  eSynthName.Text := Synth.SynthName;

  if Synth.MidiChannelA = 16 then begin
    eMidiChannelA.Text := 'Off';
    eMidiChannelA.Enabled := False;
    bidMidiChannelA.State := csDisabled;
    btMidiChannelAEnabled.Value := 0;
  end else begin
    eMidiChannelA.Text := IntToStr(Synth.MidiChannelA+1);
    eMidiChannelA.Enabled := True;
    bidMidiChannelA.LowValue := 0;
    bidMidiCHannelA.HighValue := 15;
    bidMidiChannelA.Value := Synth.MidiChannelA;
    bidMidiChannelA.State := csDefault;
    btMidiChannelAEnabled.Value := 1;
  end;

  if Synth.MidiChannelB = 16 then begin
    eMidiChannelB.Text := 'Off';
    eMidiChannelB.Enabled := False;
    bidMidiChannelB.State := csDisabled;
    btMidiChannelBEnabled.Value := 0;
  end else begin
    eMidiChannelB.Text := IntToStr(Synth.MidiChannelB+1);
    bidMidiChannelB.LowValue := 0;
    bidMidiChannelB.HighValue := 15;
    bidMidiChannelB.Value := Synth.MidiChannelB;
    bidMidiChannelB.State := csDefault;
    btMidiChannelBEnabled.Value := 1;
  end;

  if Synth.MidiChannelC = 16 then begin
    eMidiChannelC.Text := 'Off';
    eMidiChannelC.Enabled := False;
    bidMidiChannelC.State := csDisabled;
    btMidiChannelCEnabled.Value := 0;
  end else begin
    eMidiChannelC.Text := IntToStr(Synth.MidiChannelC+1);
    bidMidiChannelC.LowValue := 0;
    bidMidiChannelC.HighValue := 15;
    bidMidiChannelC.Value := Synth.MidiChannelC;
    bidMidiChannelC.State := csDefault;
    btMidiChannelCEnabled.Value := 1;
  end;

  if Synth.MidiChannelD = 16 then begin
    eMidiChannelD.Text := 'Off';
    eMidiChannelD.Enabled := False;
    bidMidiChannelD.State := csDisabled;
    btMidiChannelDEnabled.Value := 0;
  end else begin
    eMidiChannelD.Text := IntToStr(Synth.MidiChannelD+1);
    bidMidiChannelD.LowValue := 0;
    bidMidiChannelD.HighValue := 15;
    bidMidiChannelD.Value := Synth.MidiChannelD;
    bidMidiChannelD.State := csDefault;
    btMidiChannelDEnabled.Value := 1;
  end;

  if Synth.MidiGlobalChannel = 16 then begin
    eMidiGlobal.Text := 'Off';
    eMidiGlobal.Enabled := False;
    bidMidiGlobal.State := csDisabled;
    btMidiGlobalEnabled.Value := 0;
  end else begin
    eMidiGlobal.Text := IntToStr(Synth.MidiGlobalChannel+1);
    bidMidiGlobal.LowValue := 0;
    bidMidiGlobal.HighValue := 15;
    bidMidiGlobal.Value := Synth.MidiGlobalChannel;
    bidMidiGlobal.State := csDefault;
    btMidiGlobalEnabled.Value := 1;
  end;

  if Synth.SysExID = 16 then begin
    eSysExID.Text := 'All';
    eSysExID.Enabled := False;
    bidSysExID.State := csDisabled;
    btSysExEnabled.Value := 0;
  end else begin
    eSysExID.Text := IntToStr(Synth.SysExID + 1);
    bidSysExID.LowValue := 0;
    bidSysExID.HighValue := 15;
    bidSysExID.Value := Synth.SysexID;
    bidSysExID.State := csDefault;
    btSysExEnabled.Value := 1;
  end;

  btSendClock.Value := Synth.SendClock;
  btIgnoreExtClock.Value := Synth.IgnoreExternalClock;
  btCCReceive.Value := Synth.ControllersReceive;
  btCCSend.Value := Synth.ControllersSend;
  btProgChangeReceive.Value := Synth.ProgramChangeReceive;
  btProgChangeSend.Value := Synth.ProgramChangeSend;

  bidTuneCent.LowValue := -100;
  bidTuneCent.HighValue := 100;
  bidTuneCent.Value := SignedByteToInt(Synth.TuneCent);
  eTuneCent.Text := IntToStr(bidTuneCent.Value);

  bidTuneSemi.LowValue := -6;
  bidTuneSemi.HighValue := 6;
  bidTuneSemi.Value := SignedByteToInt(Synth.TuneSemi);
  eTuneSemi.Text := IntToStr(bidTuneSemi.Value);

  btGlobalOctaveShiftEnable.Value := Synth.GlobalOctaveShiftActive;
  case Synth.GlobalOctaveShift of
  $FE : rbGlobalOctaveShift.Value := 0;
  $FF : rbGlobalOctaveShift.Value := 1;
  $00 : rbGlobalOctaveShift.Value := 2;
  $01 : rbGlobalOctaveShift.Value := 3;
  $02 : rbGlobalOctaveShift.Value := 4;
  end;

  btLocal.Value := Synth.LocalOn;

  bidCtrlPedalGain.LowValue := 0;
  bidCtrlPedalGain.HighValue := 32;
  bidCtrlPedalGain.Value := Synth.ControlPedalGain;

  rbPedalPolarity.Value := Synth.PedalPolarity;

  btMemoryProtect.Value := Synth.MemoryProtect;

  tfMasterTune.Redraw;
  tfCtrlPedalGain.Redraw;
  tfFreeMem.Redraw;
end;

procedure TframeSynthSettings.UpdateSynth;
begin
  Synth.SendSetSynthSettingsMessage;
  //UpdateControls;
end;

end.
