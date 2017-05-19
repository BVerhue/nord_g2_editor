unit UnitSynthStrip;

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
  System.Variants, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls,
{$IFDEF VER260}
  FMX.Graphics,
{$ENDIF}
  BVE.NMG2USB, BVE.NMG2Mess, BVE.NMG2File, BVE.NMG2ControlsFMX,
  BVE.NMG2GraphFMX, BVE.NMG2Types, FMX.Edit, FMX.Layouts, FMX.Objects;

type
  TFrameSynthStrip = class(TFrame, IG2Observer)
    lbSynthName: TLabel;
    btClockRun: TG2BtnText;
    tfMasterClock: TG2TextField;
    idClockBPM: TG2BtnIncDec;
    ePerfName: TEdit;
    btKeyboardSplit: TG2BtnText;
    btPerfMode: TG2BtnText;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    rLed: TRectangle;
    Layout1: TLayout;
    procedure tfMasterClockGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfMidiCCGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure idClockBPMChangeValue(Sender: TObject; const aValue: Integer);
    procedure btClockRunChangeValue(Sender: TObject; const aValue: Integer);
    procedure ePerfNameExit(Sender: TObject);
    procedure btKeyboardSplitChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btPerfModeChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FSynth : TG2GraphFMX;

    FResponseTimeOut : TTimer;

    FSelected : boolean;
    FLastReceivedMidiCC : byte;
    FMasterClockBPM : integer;
    FMasterClock: integer;

    procedure Update( aG2Event : TG2Event);
    procedure SetSynth(const Value: TG2GraphFMX);
    procedure SetSelected(const Value: boolean);
    procedure G2MessageTimeOut(Sender : TObject);
  protected
    procedure SetMasterClockBPM(const Value: integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure StartStopClock;

    procedure UpdateControls;

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property Synth : TG2GraphFMX read FSynth write SetSynth;

    property MasterClockBPM : integer read FMasterClockBPM write SetMasterClockBPM;
    property LastReceivedMidiCC : byte read FLastReceivedMidiCC write FLastReceivedMidiCC;
    property Selected : boolean read FSelected write SetSelected;
  end;

implementation

{$R *.fmx}

{ TFrameSynthStrip }

procedure TFrameSynthStrip.btKeyboardSplitChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.KeyboardRangeEnabled := aValue;
  (FSynth.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;

procedure TFrameSynthStrip.btPerfModeChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.PerfMode := aValue;
end;

constructor TFrameSynthStrip.Create(AOwner: TComponent);
begin
  inherited;

  FResponseTimeOut := TTimer.Create(nil);
  FResponseTimeOut.Enabled := False;
  FResponseTimeOut.Interval := 4000;
  FResponseTimeOut.OnTimer := G2MessageTimeOut;
end;

destructor TFrameSynthStrip.Destroy;
begin
  FResponseTimeOut.Free;
  inherited;
end;

procedure TFrameSynthStrip.ePerfNameExit(Sender: TObject);
begin
  (FSynth.Performance as TG2USBPerformance).SendSetPerfNameMessage( ePerfName.Text);
end;

procedure TFrameSynthStrip.G2MessageTimeOut(Sender: TObject);
begin
  FResponseTimeOut.Enabled := False;
  rLed.Fill.Color := claRed;
end;

procedure TFrameSynthStrip.btClockRunChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.MasterClockRun := btClockRun.Value;
end;

procedure TFrameSynthStrip.idClockBPMChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.MasterClock := idClockBPM.Value;
  MasterClockBPM := idClockBPM.Value;
end;

procedure TFrameSynthStrip.SetSynth(const Value: TG2GraphFMX);
begin
  if FSynth <> Value then begin
    if assigned(FSynth) then
      FSynth.RemoveObserver( self);

    FSynth := Value;

    if assigned(FSynth) then
      FSynth.RegisterObserver( self);
  end;
end;

procedure TFrameSynthStrip.SetMasterClockBPM(const Value: integer);
begin
  if FMasterClockBPM <> Value then begin
    FMasterClockBPM := Value;
    tfMasterClock.Redraw;
  end;
end;

procedure TFrameSynthStrip.SetSelected(const Value: boolean);
begin
  if Value <> FSelected then begin
    FSelected := Value;

    if Selected then begin
      Rectangle2.Fill.Color := claAqua;
      Rectangle1.Fill.Color := $FFF0F0F0;
      lbSynthName.Font.Style := [TFontStyle.fsBold];
      lbSynthName.Repaint;
    end else begin
      Rectangle1.Fill.Color := $FFF0F0F0;
      Rectangle2.Fill.Color := $FFE0E0E0;
      lbSynthName.Font.Style := [];
      lbSynthName.Repaint;
    end;
  end;
end;

procedure TFrameSynthStrip.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TFrameSynthStrip.StartStopClock;
begin
  btClockRun.Value := 1 - btClockRun.Value;
  FSYnth.Performance.MasterClockRun := btClockRun.Value;
end;

procedure TFrameSynthStrip.tfMasterClockGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  aTextFunc := IntToStr(FMasterClockBPM);
end;

procedure TFrameSynthStrip.tfMidiCCGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  aTextFunc := 'CC ' + IntToHex(FLastReceivedMidiCC,2);
end;

procedure TFrameSynthStrip.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange: ;
    EvtUSBError: ;
    EvtBeforeSendMessage:
      begin
       if rLed.Fill.Color <> claRed then
         rLed.Fill.Color := claLime;
        FResponseTimeOut.Enabled := True;
      end;
    EvtReceiveResponseMessage:
      begin
        FResponseTimeOut.Enabled := False;
        rLed.Fill.Color := $FFE0E0E0;
      end;
    EvtNextInitStep: ;
    EvtAfterG2Init:
      begin
        UpdateControls;
      end;
    EvtAfterPerfInit: ;
    EvtAfterSlotInit: ;
    EvtPerfsSettingsUpdate:
      begin
      end;
    EvtPerfUpdate: ;
    EvtSynthSettingsUpdate: ;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate: ;
    EvtVariationChange: ;
    EvtCopyVariation: ;
    EvtMidiClockReceive:
      begin
        if FSynth.Performance.MasterClockRun = 1 then begin
          MasterClockBPM := (FSynth.Performance as TG2MessPerformance).LastMidiClock;
        end;
      end;
    EvtClockRunChange:
      begin
        if Synth.Performance.MasterClockRun = 0 then begin
          btClockRun.Value := 0;
          MasterClockBPM := Synth.Performance.MasterClock;
        end else begin
          btClockRun.Value := 1;
        end;
        btClockRun.Redraw;
      end;
    EvtClockBPMChange:
      begin
        MasterClockBPM := FSynth.Performance.MasterClock;
      end;
    EvtMidiCCRecieve:
      begin
        LastReceivedMidiCC := (Synth.Performance as TG2MessPerformance).LastMidiCC;
      end;
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

procedure TFrameSynthStrip.UpdateControls;
begin
  lbSynthName.Text := FSynth.SynthName;
  ePerfName.Text := FSynth.Performance.PerformanceName;
  idClockBPM.LowValue := 30;
  idClockBPM.HighValue := 240;
  idClockBPM.Value := FSynth.Performance.MasterClock;
  idClockBPM.Redraw;
  btClockRun.Value := FSynth.Performance.MasterClockRun;
  MasterClockBPM := idClockBPM.Value;
  btClockRun.Redraw;
  btPerfMode.Value := FSynth.PerfMode;
  btKeyboardSplit.Value := FSynth.Performance.KeyboardRangeEnabled;
  if FSynth.PerfMode = 1 then
    ePerfName.Enabled := True
  else
    ePerfname.Enabled := False;
end;

end.

