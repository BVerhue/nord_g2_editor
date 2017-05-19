unit UnitPerfSlot;

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
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Viewport3D,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Objects,
  FMX.MaterialSources,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2TexturesGL,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2,
  UnitFormSlot;

type
  TPerfGL = class(TG2PanelGL)
  private
    FLbSynth: TTextFieldGL;
    FTePerf: TTextEditGL;
    FLbClock: TTextFieldGL;
    FBtnClock: TBtnIncDecGL;
    FBtnRun: TG2BtnTextGL;
    FBtnPerf: TG2BtnTextGL;

    procedure UpdateClock;
    procedure SynthNameClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PerfNameClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure ClockClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure RunClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PerfClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
    procedure UpdateControls;
  end;

  TPerfSlotGL = class(TG2SlotPanelGL)
  private
    FLbSlot: TLabelGL;
    FTePatch: TTextEditGL;
    FBtnEdit: TBtnTextGL;
    FBtnVariation: TBtnRadioGL;
    FKnobVolume: TG2KnobGL;
    FBtnMute: TG2BtnTextGL;
    FBtnKeyb: TG2BtnTextGL;
    FBtnHold: TG2BtnTextGL;
    FBtnEnable: TG2BtnTextGL;

    FfrmSlot: TfrmSlot;
  private
    procedure VariationClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure KeybClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure HoldClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure EnableClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PatchClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure EditClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure ConnectParams;
  protected
    procedure SetConnectionManager(const Value: IG2ConnectionManager); override;
    procedure SetConnectionIndex(const Value: integer); override;
    procedure SetSlotIndex(const Value: integer); override;
    procedure SetTextureList(const Value: TTextureListGL); override;

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
    procedure UpdateControls;
  end;

implementation
uses
  UnitFormBanks;

// -----------------------------------------------------------------------------
//
//                                TPerfSlotGL
//
// -----------------------------------------------------------------------------

procedure TPerfSlotGL.ConnectParams;
begin
  if assigned(Slot) and assigned(Slot.Patch) then
  begin
    FKnobVolume.Param := Slot.Patch.Modules[LOCATION_PATCH, PATCH_VOLUME].Param[VOLUME_LEVEL];
    FBtnMute.Param := Slot.Patch.Modules[LOCATION_PATCH, PATCH_VOLUME].Param[VOLUME_MUTE];
  end else begin
    FKnobVolume.Param := nil;
    FBtnMute.Param := nil;
  end;
end;

constructor TPerfSlotGL.Create(AOwner: TComponent);
begin
  inherited;

  FLbSlot := TLabelGL.Create(Self);
  FLbSlot.Parent := Self;
  FLbSlot.Font.Size := 12 * SCALE_X;
  FLbSlot.Font.Style := [TFontStyle.fsBold];
  FLbSlot.Caption := 'A';

  FTePatch := TTextEditGL.Create(Self);
  FTePatch.Parent := Self;
  FTePatch.TextureID := GetBtnTexID(43, 12);
  FTePatch.TextureList := TextureList;
  FTePatch.BtnType := TButtonTextType.bttPush;
  FTePatch.Caption := '';
  FTePatch.OnClk := PatchClk;

  FBtnEdit := TBtnTextGL.Create(Self);
  FBtnEdit.Parent := Self;
  FBtnEdit.TextureID := GetBtnTextTexID(24, 12, '', 'Edit;Edit');
  FBtnEdit.TextureList := TextureList;
  FBtnEdit.BtnType := TButtonTextType.bttPush;
  FBtnEdit.OnClk := EditClk;

  FBtnVariation := TBtnRadioGL.Create(Self);
  FBtnVariation.Parent := Self;
  FBtnVariation.TextureID := GetBtnRadioTexID(10, 12, '', '1;2;3;4;5;6;7;8');
  FBtnVariation.TextureList := TextureList;
  FBtnVariation.TexCols := 8;
  FBtnVariation.TexRows := 2;
  FBtnVariation.Horizontal := True;
  FBtnVariation.ColCount := 8;
  FBtnVariation.RowCount := 1;
  FBtnVariation.OnClk := VariationClk;

  FKnobVolume := TG2KnobGL.Create(Self);
  FKnobVolume.Parent := Self;
  FKnobVolume.TextureID := GetKnobTexID(ktHSlider);
  FKnobVolume.TextureList := TextureList;
  FKnobVolume.KnobType := ktHSlider;
  FKnobVolume.MinValue := 0;
  FKnobVolume.MaxValue := 127;

  FBtnMute := TG2BtnTextGL.Create(Self);
  FBtnMute.TextureID := GetBtnTextTexID(24, 12, '', 'Mute;On');
  FBtnMute.TextureList := TextureList;
  FBtnMute.Parent := Self;
  FBtnMute.BtnType := TButtonTextType.bttCheck;

  FBtnKeyb := TG2BtnTextGL.Create(Self);
  FBtnKeyb.Parent := Self;
  FBtnKeyb.TextureID := GetBtnTextTexID(24, 12, '', 'Keyb;Keyb');
  FBtnKeyb.TextureList := TextureList;
  FBtnKeyb.BtnType := TButtonTextType.bttCheck;
  FBtnKeyb.OnClk := KeybClk;

  FBtnHold := TG2BtnTextGL.Create(Self);
  FBtnHold.Parent := Self;
  FBtnHold.TextureID := GetBtnTextTexID(24, 12, '', 'Hold;Hold');
  FBtnHold.TextureList := TextureList;
  FBtnHold.BtnType := TButtonTextType.bttCheck;
  FBtnHold.OnClk := HoldClk;

  FBtnEnable := TG2BtnTextGL.Create(Self);
  FBtnEnable.Parent := Self;
  FBtnEnable.TextureID := GetBtnTextTexID(24, 12, '', 'Enab;Enab');
  FBtnEnable.TextureList := TextureList;
  FBtnEnable.BtnType := TButtonTextType.bttCheck;
  FBtnEnable.OnClk := EnableClk;

  Application.CreateForm(TfrmSlot, FfrmSlot);
  FfrmSlot.PatchCtrl.TextureList := TextureList;
  FfrmSlot.ConMan := ConMan;
end;

destructor TPerfSlotGL.Destroy;
begin
  inherited;
end;

procedure TPerfSlotGL.EditClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  FfrmSlot.Show;
end;

procedure TPerfSlotGL.EnableClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
  begin
    Slot.Enabled := FBtnEnable.Value;
    Connection.PerfSetSettings;
  end;
end;

procedure TPerfSlotGL.HoldClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
  begin
    Slot.Hold := FBtnHold.Value;
    Connection.PerfSetSettings;
  end;
end;

procedure TPerfSlotGL.KeybClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
  begin
    Slot.Keyboard := FBtnKeyb.Value;
    Connection.PerfSetSettings;
  end;
end;

procedure TPerfSlotGL.PatchClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.PerfSelectSlot(Slot.SlotIndex);

  frmBanks.Show;
end;

procedure TPerfSlotGL.Resize;
begin
  inherited;
  LayoutControls;
end;

procedure TPerfSlotGL.SetConnectionIndex(const Value: integer);
begin
  inherited;
  FfrmSlot.ConnectionIndex := Value;
end;

procedure TPerfSlotGL.SetConnectionManager(const Value: IG2ConnectionManager);
begin
  inherited;
  FfrmSlot.ConMan := Value;
end;

procedure TPerfSlotGL.SetSlotIndex(const Value: integer);
begin
  inherited;
  FfrmSlot.SlotIndex := Value;
  UpdateControls;
end;

procedure TPerfSlotGL.SetTextureList(const Value: TTextureListGL);
begin
  inherited;
  FfrmSlot.TextureList := Value;
end;

procedure TPerfSlotGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtUSBActiveChange:
      ;
    EvtUSBError:
      ;
    EvtBeforeSendMessage:
      ;
    EvtProcessResponseMsg:
      ;
    EvtNextInitStep:
      ;
    EvtAfterG2Init:
      begin
        UpdateControls;
      end;
    EvtAfterPerfInit:
      ;
    EvtAfterSlotInit:
      begin
        UpdateControls;
      end;
    EvtPerfsSettingsUpdate:
      ;
    EvtPerfUpdate:
      ;
    EvtSynthSettingsUpdate:
      ;
    EvtBeforePatchUpdate:
      begin
      end;
    EvtPatchUpdate:
      begin
        UpdateControls;
      end;
    EvtVariationChange:
      begin
        if assigned(Slot) and assigned(Slot.Patch) then
          FBtnVariation.Value := Slot.Patch.Settings.ActiveVariation;
      end;
  end;
end;

procedure TPerfSlotGL.UpdateControls;
begin
  case SlotIndex of
  0: FLbSlot.Caption := 'A';
  1: FLbSlot.Caption := 'B';
  2: FLbSlot.Caption := 'C';
  3: FLbSlot.Caption := 'D';
  end;
  ConnectParams;

  if assigned(Slot) then
  begin
    if assigned(Slot.Patch) then
    begin
      FBtnVariation.Value := Slot.Patch.Settings.ActiveVariation;
      FTePatch.Caption := Slot.Patch.PatchName;
    end;
    FBtnKeyb.Value := Slot.Keyboard;
    FBtnHold.Value := Slot.Hold;
    FBtnEnable.Value := Slot.Enabled;
  end;

  FullRepaint;
end;

procedure TPerfSlotGL.VariationClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.SlotSelectVariation(SlotIndex, aBtnIndex);
end;

// -----------------------------------------------------------------------------
//
//                                 TPerfGL
//
// -----------------------------------------------------------------------------

constructor TPerfGL.Create(AOwner: TComponent);
begin
  inherited;

  FLbSynth := TTextFieldGL.Create(Self);
  FLbSynth.Parent := Self;
  FLbSynth.FieldWidth := 50;
  FLbSynth.FieldHeight := 12;
  FLbSynth.Caption := 'Syntname';

  FTePerf := TTextEditGL.Create(Self);
  FTePerf.Parent := Self;
  FTePerf.TextureID := GetBtnTexID(43, 12);
  FTePerf.TextureList := TextureList;
  FTePerf.BtnType := TButtonTextType.bttCheck;
  FTePerf.Caption := '';
  FTePerf.OnClk := PerfNameClk;

  FLbClock := TTextFieldGL.Create(Self);
  FLbClock.Parent := Self;
  FLbClock.TextureList := TextureList;
  FLbClock.FieldWidth := 24;
  FLbClock.FieldHeight := 12;
  FLbClock.Caption := '120';

  FBtnClock := TBtnIncDecGL.Create(Self);
  FBtnClock.Parent := Self;
  FBtnClock.ColCount := 2;
  FBtnClock.RowCount := 1;
  FBtnClock.TextureID := GetBtnIncDecTexID(13, 12, True);
  FBtnClock.TextureList := TextureList;
  FBtnClock.MinValue := 30;
  FBtnClock.MaxValue := 240;
  FBtnClock.Value := 120;
  FBtnClock.OnClk := ClockClk;

  FBtnRun := TG2BtnTextGL.Create(Self);
  FBtnRun.Parent := Self;
  FBtnRun.TextureID := GetBtnTextTexID(24, 12, '', 'Run;Run');
  FBtnRun.TextureList := TextureList;
  FBtnRun.BtnType := TButtonTextType.bttCheck;
  FBtnRun.OnClk := RunClk;

  FBtnPerf := TG2BtnTextGL.Create(Self);
  FBtnPerf.Parent := Self;
  FBtnPerf.TextureID := GetBtnTextTexID(24, 12, '', 'Perf;Perf');
  FBtnPerf.TextureList := TextureList;
  FBtnPerf.BtnType := TButtonTextType.bttCheck;
  FBtnPerf.OnClk := PerfClk;
end;

destructor TPerfGL.Destroy;
begin
  inherited;
end;

procedure TPerfGL.ClockClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Perf.MasterClock := FBtnClock.Value;
  COnnection.PerfSetSettings;
end;

procedure TPerfGL.PerfClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Synth.PerfMode := FBtnPerf.Value;
  Connection.SynthPerfMode(FBtnPerf.Value);
end;

procedure TPerfGL.PerfNameClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  frmBanks.Show;
end;

procedure TPerfGL.RunClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Perf.MasterClockRun := FBtnRun.Value;
  COnnection.PerfSetSettings;
end;

procedure TPerfGL.SynthNameClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  //
end;

procedure TPerfGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    {vtBeforeSendMessage:
      begin
        if rLed.Fill.Color <> claRed then
         rLed.Fill.Color := claLime;
      end;
    EvtReceiveResponseMessage:
      begin
        rLed.Fill.Color := $FFE0E0E0;
      end;}
    EvtAfterG2Init:
      begin
      end;
    EvtAfterPerfInit:
      begin
      end;
    EvtAfterSlotInit:
      begin
      end;
    EvtPerfsSettingsUpdate:
      begin
        UpdateControls;
      end;
    EvtPerfUpdate:
      begin
        UpdateControls;
      end;
    EvtSynthSettingsUpdate:
      begin
        UpdateControls;
      end;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate:
      begin
        UpdateControls;
      end;
    EvtVariationChange: ;
    EvtCopyVariation: ;
    EvtMidiClockReceive:
      begin
        UpdateClock;
      end;
    EvtClockRunChange:
      begin
        if assigned(Perf) then
        begin
          if Perf.MasterClockRun = 0 then
            FBtnRun.Value := 0
          else
            FBtnRun.Value := 1;
        end;
        UpdateClock;
      end;
    EvtClockBPMChange:
      begin
        UpdateClock;
      end;
    EvtMidiCCRecieve:
      begin
        //
      end;
    EvtAfterGetAssignedVoices: ;
    EvtPatchLoadChange: ;
    EvtSelectSlot:
      begin
        //
      end;
    EvtSelectLocation:
      begin
        //
      end;
    EvtSelectModule: ;
    EvtSelectParam:
      begin
        //
      end;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
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

procedure TPerfGL.UpdateClock;
begin
  if assigned(Synth)
  and assigned(Perf)
  then
  begin
    if Synth.IgnoreExternalClock = 0 then
    begin
      FLbClock.Caption := IntToStr(Perf.LastMidiClock);
      FLbClock.Color := TAlphaColorRec.Red;
    end else begin
      FLbClock.Caption := IntToStr(Perf.MasterClock);
      FLbClock.Color := TAlphaColorRec.LightGray;
    end;
  end;
end;

procedure TPerfGL.UpdateControls;
begin
  if assigned(Synth) then
  begin
    FLbSynth.Caption := Synth.SynthName;
    FBtnPerf.Value := Synth.PerfMode;
  end else begin
    FLbSynth.Caption := 'No synth';
  end;

  if assigned(Perf) then
  begin
    FTePerf.Caption := Perf.PerformanceName;
    FBtnClock.Value := Perf.MasterClock;
    FBtnRun.Value := Perf.MasterClockRun;
  end;

  UpdateClock;

  FullRepaint;
end;

end.
