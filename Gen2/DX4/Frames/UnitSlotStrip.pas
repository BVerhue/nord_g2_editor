unit UnitSlotStrip;
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
  FMX.StdCtrls, FMX.Edit, FMX.Objects,
  BVE.NMG2ControlsFMX, BVE.NMG2Types, BVE.NMG2File, BVE.NMG2GraphFMX,
  FMX.Layouts, FMX.Controls.Presentation;
type
  TframeSlotStrip = class(TFrame, IG2Observer)
    lbSlot: TLabel;
    btEnable: TG2BtnText;
    btKeyb: TG2BtnText;
    btHold: TG2BtnText;
    rbVariation: TG2BtnRadio;
    btEditAllVars: TG2BtnText;
    bgRectangle: TRectangle;
    kVolume: TG2Knob;
    btMute: TG2BtnText;
    tfVoices: TG2TextField;
    bidVoices: TG2BtnIncDec;
    ePatchName: TEdit;
    Layout1: TLayout;
    tfVACycles: TG2TextField;
    tfFXCycles: TG2TextField;
    Layout3: TLayout;
    tfVAMem: TG2TextField;
    tfFXMem: TG2TextField;
    lbCPU: TLabel;
    lbMem: TLabel;
    procedure bgRectangleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure tfVoicesGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure bidVoicesChangeValue(Sender: TObject; const aValue: Integer);
    procedure btEnableChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKeybChangeValue(Sender: TObject; const aValue: Integer);
    procedure btHoldChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbVariationChangeValue(Sender: TObject; const aValue: Integer);
    procedure ePatchNameExit(Sender: TObject);
    procedure tfFXMemGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfVAMemGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfVACyclesGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfFXCyclesGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure btEditAllVarsChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FSlot : TG2GraphSlotFMX;
    FBackColor,
    FSelectedColor : TAlphaColor;
    FVACycles : single;
    FVAMem : single;
    FFXCycles : single;
    FFXMem : single;
    procedure ConnectControls;
    procedure Update( aG2Event : TG2Event);
    procedure RemoveReference( aData : IG2Subject);
    function GetSelected: boolean;
    procedure SetSelected(const Value: boolean);
    procedure SetSlot(const Value: TG2GraphSlotFMX);
    procedure SetFXCycles(const Value: single);
    procedure SetFXMem(const Value: single);
    procedure SetVACycles(const Value: single);
    procedure SetVAMem(const Value: single);
    procedure SetBackColor(const Value: TAlphaColor);
    procedure SetSelectedColor(const Value: TAlphaColor);  public
    constructor Create(AOwner : TComponent); override;
    procedure UpdateControls;
    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);
    property BackColor : TAlphaColor read FBackColor write SetBackColor;
    property SelectedColor : TAlphaColor read FSelectedColor write SetSelectedColor;
    property Slot : TG2GraphSlotFMX read FSlot write SetSlot;
    property Selected : boolean read GetSelected write SetSelected;
    property VACycles : single read FVACycles write SetVACycles;
    property VAMem : single read FVAMem write SetVAMem;
    property FXCycles : single read FFXCycles write SetFXCycles;
    property FXMem : single read FFXMem write SetFXMem;
  end;
implementation
{$R *.fmx}
procedure TframeSlotStrip.Update(aG2Event: TG2Event);
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
    EvtSynthSettingsUpdate: ;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate:
      begin
        if assigned(Slot) then begin
          ConnectControls;
          if (not Slot.Initializing) and (Slot.G2.AutoAssignMidi) then begin
            Slot.AutoAssignMidiToKnobs;
          end;
        end;
        UpdateControls;
      end;
    EvtVariationChange:
      begin
        UpdateControls;
      end;
    EvtCopyVariation: ;
    EvtMidiClockReceive: ;
    EvtClockRunChange: ;
    EvtClockBPMChange: ;
    EvtMidiCCRecieve: ;
    EvtAfterGetAssignedVoices:
      begin
        bidVoices.HighValue := 32;
        bidVoices.LowValue := 0;
        bidVoices.Value := FSlot.Patch.PatchDescription.VoiceCount + 2 - FSlot.Patch.PatchDescription.MonoPoly - 1;
        tfVoices.Redraw;
      end;
    EvtPatchLoadChange:
      begin
        VACycles := FSLot.PatchloadCyclesVA;
        VAMem := FSLot.PatchloadMemVA;
        FXCycles := FSLot.PatchloadCyclesFX;
        FXMem := FSLot.PatchloadMemFX;
        tfVACycles.Redraw;
        tfVAMem.Redraw;
        tfFXCycles.Redraw;
        tfFXMem.Redraw;
      end;
    EvtSelectSlot:
      begin
        UpdateControls;
      end;
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

procedure TframeSlotStrip.UpdateControls;
begin
  if Selected then begin
    lbSlot.FontColor := FBackColor;
    lbCPU.FontColor := FBackColor;
    lbMem.FontColor := FBackColor;
    bgRectangle.Fill.Color := FSelectedColor;
  end else begin
    lbSlot.FontColor := FSelectedColor;
    lbCPU.FontColor := FSelectedColor;
    lbMem.FontColor := FSelectedColor;
    bgRectangle.Fill.Color := FBackColor;
  end;
  if assigned(FSlot) then begin
    case FSlot.SlotIndex of
      0 : lbSlot.Text := 'A';
      1 : lbSlot.Text := 'B';
      2 : lbSlot.Text := 'C';
      3 : lbSlot.Text := 'D';
    end;
    ePatchName.Text := string(FSlot.PatchName);
    rbVariation.Value := FSlot.Patch.ActiveVariation;
    if FSlot.Patch.EditAllVariations then
      btEditAllVars.Value := 1
    else
      btEditAllVars.Value := 0;
    btEnable.Value := FSlot.Enabled;
    btKeyb.Value := FSlot.Keyboard;
    btHold.Value := FSlot.Hold;
  end;
end;
procedure TframeSlotStrip.bgRectangleMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FSLot.Performance.SelectedSlot := FSLot.SlotIndex;
end;
procedure TframeSlotStrip.bidVoicesChangeValue(Sender: TObject;
  const aValue: Integer);
var FPatchDescription : TPatchDescription;
begin
  FPatchDescription := FSlot.Patch.PatchDescription;
  case bidVoices.Value of
  0 : begin // Legato
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 2;
      end;
  1 : begin // Mono
        FPatchDescription.VoiceCount := 1;
        FPatchDescription.MonoPoly := 1;
      end;
  2..32 :
      begin
        FPatchDescription.VoiceCount := bidVoices.Value - 1;
        FPatchDescription.MonoPoly := 0;
      end;
  end;
  (FSlot.Patch as TG2GraphPatchFMX).MessSetPatchDescription( FPatchDescription);
  tfVoices.Redraw;
end;
procedure TframeSlotStrip.btEditAllVarsChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSlot.Patch.EditAllVariations := aValue = 1;
end;
procedure TframeSlotStrip.btEnableChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSlot.Enabled := btEnable.Value;
  (FSlot.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;
procedure TframeSlotStrip.btKeybChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSlot.Keyboard := btKeyb.Value;
  (FSlot.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;
procedure TframeSlotStrip.btHoldChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSlot.Hold := btHold.Value;
  (FSlot.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;
procedure TframeSlotStrip.rbVariationChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSlot.SendSelectVariationMessage( aValue);
end;
procedure TframeSlotStrip.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeSlotStrip.ConnectControls;
var VolumeParam, MuteParam : TG2FileParameter;
begin
  VolumeParam := FSlot.Patch.Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_LEVEL];
  kVolume.DataWriter := VolumeParam;
  MuteParam := FSlot.Patch.Modules[ord(ltPatch), PATCH_VOLUME].Parameter[VOLUME_MUTE];
  btMute.DataWriter := MuteParam;
end;
constructor TframeSlotStrip.Create(AOwner: TComponent);
begin
  inherited;
  FBackColor := TAlphaColorRec.Silver;
  FSelectedColor := TAlphaColorRec.Seagreen;
end;

procedure TframeSlotStrip.ePatchNameExit(Sender: TObject);
begin
  FSlot.SendSetPatchName( ePatchName.Text);
end;
procedure TframeSlotStrip.tfFXCyclesGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSlot) then
    exit;
  aTextFunc := Format('%.1f', [FSlot.PatchloadCyclesFX]);
end;
procedure TframeSlotStrip.tfFXMemGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSlot) then
    exit;
  aTextFunc := Format('%.1f', [FSlot.PatchloadMemFX]);
end;
procedure TframeSlotStrip.tfVACyclesGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSlot) then
    exit;
  aTextFunc := Format('%.1f', [FSlot.PatchloadCyclesVA]);
end;
procedure TframeSlotStrip.tfVAMemGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if not assigned(FSlot) then
    exit;
  aTextFunc := Format('%.1f', [FSlot.PatchloadMemVA]);
end;
procedure TframeSlotStrip.tfVoicesGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  aTextFunc := '';
  if not assigned(FSlot) then
    exit;
  if not assigned(FSlot.G2) then
    exit;
  if bidVoices.Value <= 1 then begin
    aTextFunc := FSlot.G2.TextFunction(0, bidVoices.Value, 0, 0);
  end else begin
    aTextFunc := IntToStr(FSlot.AssignedVoices) + ' (' + FSlot.G2.TextFunction(0, bidVoices.Value, 0, 0) + ')';
  end;
end;
function TframeSlotStrip.GetSelected: boolean;
begin
  if assigned(FSlot) then
    Result := FSlot.GetPerformance.SelectedSlot = FSlot.SlotIndex;
end;
procedure TframeSlotStrip.SetBackColor(const Value: TAlphaColor);
begin
  FBackColor := Value;
  UpdateControls;
end;

procedure TframeSlotStrip.SetSelectedColor(const Value: TAlphaColor);
begin
  FSelectedColor := Value;
  UpdateControls;
end;

procedure TframeSlotStrip.SetFXCycles(const Value: single);
begin
  FFXCycles := Value;
end;
procedure TframeSlotStrip.SetFXMem(const Value: single);
begin
  FFXMem := Value;
end;
procedure TframeSlotStrip.SetSelected(const Value: boolean);
begin
  if not assigned(FSlot) then
    exit;
  if Value then begin
    (FSlot.Performance as TG2GraphPerformanceFMX).SendSelectSlotMessage(FSlot.SlotIndex);
  end;
end;
procedure TframeSlotStrip.SetSlot(const Value: TG2GraphSlotFMX);
begin
  if FSlot <> VAlue then begin
    if assigned(FSLot) then begin
      FSlot.RemoveObserver( self);
      if assigned(FSlot.Performance) then
        FSlot.Performance.RemoveObserver(self);
    end;
    FSlot := Value;
    if assigned(FSlot) then begin
      FSlot.RegisterObserver( self);
      if assigned(FSLot.Performance) then
        FSlot.Performance.RegisterObserver(self);
    end;
    ConnectControls;
    UpdateControls;
  end;
end;
procedure TframeSlotStrip.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;
procedure TframeSlotStrip.SetVACycles(const Value: single);
begin
  FVACycles := Value;
end;
procedure TframeSlotStrip.SetVAMem(const Value: single);
begin
  FVAMem := Value;
end;
end.
