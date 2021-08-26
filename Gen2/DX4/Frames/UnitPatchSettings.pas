unit UnitPatchSettings;

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
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Controls.Presentation,
  BVE.NMG2Types,
  BVE.NMG2File,
  BVE.NMG2GraphFMX,
  BVE.NMG2ControlsFMX;

type
  TframePatchSettings = class(TFrame, IG2Observer)
    rbOctaveShift: TG2BtnRadio;
    rbArpRate: TG2BtnRadio;
    rbArpDirection: TG2BtnRadio;
    rbArpRange: TG2BtnRadio;
    rbVibratoMod: TG2BtnRadio;
    rbGlideType: TG2BtnRadio;
    btArp: TG2BtnText;
    btPitchBend: TG2BtnText;
    btSustainPedal: TG2BtnText;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    G2Label1: TG2Label;
    G2Label2: TG2Label;
    G2Label3: TG2Label;
    G2Label4: TG2Label;
    G2Label5: TG2Label;
    G2Label6: TG2Label;
    G2Label7: TG2Label;
    G2Label8: TG2Label;
    G2Label9: TG2Label;
    G2Label10: TG2Label;
    G2Label11: TG2Label;
    G2Label12: TG2Label;
    G2Label13: TG2Label;
    G2Label14: TG2Label;
    Rectangle1: TRectangle;
    slVibratoRange: TG2Knob;
    slVibratoRate: TG2Knob;
    slGlideTime: TG2Knob;
    slBendRAnge: TG2Knob;
    procedure Panel2Click(Sender: TObject);
  private
    [Weak] FPatch: TG2GraphPatchFMX;
    procedure ConnectControls;
    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference(aData: IG2Subject);
    procedure SetPatch(const Value: TG2GraphPatchFMX);
  public
    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);
    property Patch: TG2GraphPatchFMX read FPatch write SetPatch;
  end;

implementation

{$R *.fmx}

{ TframePatchSettings }

procedure TframePatchSettings.ConnectControls;
var
  Param: TG2FileParameter;
begin
  Param := FPatch.Modules[ord(ltPatch), PATCH_SUSTAIN].Parameter[SUSTAIN_PEDAL];
  btSustainPedal.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_SUSTAIN].Parameter[OCTAVE_SHIFT];
  rbOctaveShift.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_ON_OFF];
  btPitchBend.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_BEND].Parameter[BEND_RANGE];
  slBendRAnge.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_ON_OFF];
  btArp.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_OCTAVES];
  rbArpRange.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_DIRECTION];
  rbArpDirection.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_ARPEGGIATOR].Parameter[ARP_SPEED];
  rbArpRate.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_TYPE];
  rbGlideType.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_GLIDE].Parameter[GLIDE_SPEED];
  slGlideTime.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_MOD];
  rbVibratoMod.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_RATE];
  slVibratoRate.DataWriter := Param;
  Param := FPatch.Modules[ord(ltPatch), PATCH_VIBRATO].Parameter[VIBRATO_DEPTH];
  slVibratoRange.DataWriter := Param;
end;

procedure TframePatchSettings.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframePatchSettings.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange:
      ;
    EvtUSBError:
      ;
    EvtBeforeSendMessage:
      ;
    EvtReceiveResponseMessage:
      ;
    EvtNextInitStep:
      ;
    EvtAfterG2Init:
      ;
    EvtAfterPerfInit:
      ;
    EvtAfterSlotInit:
      ;
    EvtPerfsSettingsUpdate:
      ;
    EvtPerfUpdate:
      ;
    EvtSynthSettingsUpdate:
      ;
    EvtBeforePatchUpdate:
      ;
    EvtPatchUpdate:
      begin
        if assigned(FPatch) then
        begin
          ConnectControls;
          // FPatch.InvalidateParameters;
        end;
      end;
    EvtVariationChange:
      ;
    EvtCopyVariation:
      ;
    EvtMidiClockReceive:
      ;
    EvtClockRunChange:
      ;
    EvtClockBPMChange:
      ;
    EvtMidiCCRecieve:
      ;
    EvtAfterGetAssignedVoices:
      ;
    EvtPatchLoadChange:
      ;
    EvtSelectSlot:
      ;
    EvtSelectLocation:
      ;
    EvtSelectModule:
      ;
    EvtSelectParam:
      ;
    EvtLabelValueChange:
      ;
    EvtMorphChange:
      ;
    EvtDeleteModule:
      ;
    EvtAfterRetreivePatch:
      ;
    EvtAfterBankList:
      ;
    EvtAfterStore:
      ;
    EvtAfterClear:
      ;
    EvtAfterClearBank:
      ;
    EvtAfterBankDownload:
      ;
    EvtDeassignKnob:
      ;
    EvtAssignKnob:
      ;
    EvtDeassignGlobalKnob:
      ;
    EvtAssignGlobalKnob:
      ;
  end;
end;

procedure TframePatchSettings.Panel2Click(Sender: TObject);
begin
  FPatch.PatchPart[2].InvalidateParameters;
end;

procedure TframePatchSettings.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframePatchSettings.SetPatch(const Value: TG2GraphPatchFMX);
begin
  if FPatch <> Value then
  begin
    if assigned(FPatch) then
    begin
      FPatch.RemoveObserver(self);
      if assigned(FPatch.Slot) then
        FPatch.Slot.RemoveObserver(self);
    end;

    FPatch := Value;
    if assigned(FPatch) then
    begin
      FPatch.RegisterObserver(self);
      if assigned(FPatch.Slot) then
        FPatch.Slot.RegisterObserver(self);
    end;
    ConnectControls;
  end;
end;

end.
