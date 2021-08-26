unit UnitParam;

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
  FMX.Edit,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Controls.Presentation,
  BVE.NMG2ControlsFMX,
  BVE.NMG2File,
  BVE.NMG2USB,
  BVE.NMG2GraphFMX,
  BVE.NMG2Types,
  UnitSlot,
  UnitAppSettings;

type
  TframeParam = class(TFrame, IG2Observer)
    tcParam: TTabControl;
    TabItem6: TTabItem;
    eParamLabel: TEdit;
    btParamAssignCC: TG2BtnText;
    TabItem7: TTabItem;
    kMorphWheel: TG2Knob;
    kMorphVelocity: TG2Knob;
    kMorphKeyboard: TG2Knob;
    kMorphAftertouch: TG2Knob;
    kMorphSustainPedal: TG2Knob;
    kMorphControlPedal: TG2Knob;
    kMorphPitchStick: TG2Knob;
    kMorphGWheel2: TG2Knob;
    G2Label1: TG2Label;
    G2Label2: TG2Label;
    G2Label3: TG2Label;
    G2Label4: TG2Label;
    G2Label5: TG2Label;
    G2Label6: TG2Label;
    G2Label7: TG2Label;
    G2Label8: TG2Label;
    eParamName: TEdit;
    Layout1: TLayout;
    TabItem1: TTabItem;
    G2Label9: TG2Label;
    G2Label10: TG2Label;
    Panel1: TPanel;
    G2Label11: TG2Label;
    rbPage: TG2BtnRadio;
    rbPageIndex: TG2BtnRadio;
    btKnob1: TG2BtnText;
    btKnob2: TG2BtnText;
    btKnob3: TG2BtnText;
    btKnob4: TG2BtnText;
    btKnob5: TG2BtnText;
    btKnob6: TG2BtnText;
    btKnob7: TG2BtnText;
    btKnob8: TG2BtnText;
    Panel2: TPanel;
    G2Label12: TG2Label;
    rbGlobalPage: TG2BtnRadio;
    rbGlobalPageIndex: TG2BtnRadio;
    btGlobalKnob1: TG2BtnText;
    btGlobalKnob2: TG2BtnText;
    btGlobalKnob3: TG2BtnText;
    btGlobalKnob4: TG2BtnText;
    btGlobalKnob5: TG2BtnText;
    btGlobalKnob6: TG2BtnText;
    btGlobalKnob7: TG2BtnText;
    btGlobalKnob8: TG2BtnText;
    ArrayLayout1: TArrayLayout;
    ArrayLayout2: TArrayLayout;
    Panel3: TPanel;
    G2Label13: TG2Label;
    ArrayLayout3: TArrayLayout;
    btCopyVarTo1: TG2BtnText;
    btCopyVarTo2: TG2BtnText;
    btCopyVarTo3: TG2BtnText;
    btCopyVarTo4: TG2BtnText;
    btCopyVarTo5: TG2BtnText;
    btCopyVarTo6: TG2BtnText;
    btCopyVarTo7: TG2BtnText;
    btCopyVarTo8: TG2BtnText;
    btInitVariation: TG2BtnText;
    eMidiCC: TEdit;
    G2Label14: TG2Label;
    btCopyToInit: TG2BtnText;
    btDefaultValue: TG2BtnText;
    procedure btParamAssignCCChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure kMorphWheelChangeValue(Sender: TObject; const aValue: Integer);
    procedure kMorphVelocityChangeValue(Sender: TObject; const aValue: Integer);
    procedure kMorphKeyboardChangeValue(Sender: TObject; const aValue: Integer);
    procedure kMorphAftertouchChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure kMorphSustainPedalChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure kMorphControlPedalChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure kMorphPitchStickChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure kMorphGWheel2ChangeValue(Sender: TObject; const aValue: Integer);
    procedure kMorphWheelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphVelocityMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphKeyboardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphAftertouchMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphSustainPedalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphControlPedalMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphPitchStickMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure kMorphGWheel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btKnob1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob2ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob3ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob4ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob5ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob6ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob7ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btKnob8ChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbPageChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbPageIndexChangeValue(Sender: TObject; const aValue: Integer);
    procedure eParamLabelExit(Sender: TObject);
    procedure btGlobalKnob1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob2ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob3ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob4ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob5ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob6ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob7ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalKnob8ChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbGlobalPageChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbGlobalPageIndexChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btInitVariationChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btCopyVarTo1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo2ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo3ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo4ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo5ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo6ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo7ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCopyVarTo8ChangeValue(Sender: TObject; const aValue: Integer);
    procedure eMidiCCExit(Sender: TObject);
    procedure btCopyToInitChangeValue(Sender: TObject; const aValue: Integer);
    procedure btDefaultValueChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FSynth: TG2GraphFMX;
    [Weak] FParameter: TG2FileParameter;
    [Weak] FframeAppSettings: TframeAppSettings;

    FLastReceivedMidiCC: byte;
    FMorphIndex: Integer;

    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference(aData: IG2Subject);

    function GetKnobIndexOffset: Integer;
    function GetGlobalKnobIndexOffset: Integer;
    procedure SetParameter(const Value: TG2FileParameter);
    procedure SetParamMorphValue(const aMorphIndex, aValue: Integer);
    procedure SetMorphIndex(const Value: Integer);
    procedure SetLabelIndex(const Value: Integer);
    procedure SetSynth(const Value: TG2GraphFMX);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // TODO Implement this...
    { procedure SetValue(const aValue : integer);
      procedure SetButtonText(const aValue : TStrings);
      procedure SetMorphValue(const aValue : integer);
      procedure SetHasMorph(const aValue : boolean);
      procedure SetSelected(const aValue : boolean);
      function GetValueText( aIndex : integer): string;
      procedure SetValueText( aIndex : integer; const aValue : string);
      function GetCodeRef : integer;
      procedure Redraw;
      procedure ClearDataDependency( aData : IG2DataParam); }

    procedure AssignKnob(aKnobIndex: Integer);
    procedure DeassignKnob;
    procedure AssignGlobalKnob(aKnobIndex: Integer);
    procedure DeassignGlobalKnob;
    procedure AssignMidiCC(aMidiCC: byte);
    procedure DeassignMidiCC;
    procedure CopyVariation(aToVariationIndex: Integer);
    procedure InitVariation;
    procedure DefaultValue;
    procedure UpdateControls;
    procedure UpdateAssignCCButton(aCC: byte);

    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);

    property frameAppSettings: TframeAppSettings read FframeAppSettings
      write FframeAppSettings;
    // property frameSynthStrip : TframeSynthStrip read FframeSynthStrip write FframeSynthStrip;
    property Synth: TG2GraphFMX read FSynth write SetSynth;
    property Parameter: TG2FileParameter read FParameter write SetParameter;
    property MorphIndex: Integer read FMorphIndex write SetMorphIndex;
  end;

implementation

{$R *.fmx}

function IntToMorphRange(aValue: Integer): byte;
begin
  if aValue < 0 then
  begin
    if aValue >= -128 then
      Result := 256 + aValue
    else
      Result := 128;
  end
  else
  begin
    if aValue <= 127 then
      Result := aValue
    else
      Result := 127;
  end;
end;

function MorphRangeToInt(aValue: byte): Integer;
begin
  if aValue > 127 then
    Result := aValue - 256
  else
    Result := aValue;
end;

{ TframeParam }

procedure TframeParam.CopyVariation(aToVariationIndex: Integer);
begin
  (FSynth.SelectedSlot as TG2USBSlot).SendCopyVariationMessage(
    FSynth.SelectedSlot.Patch.ActiveVariation, aToVariationIndex);
end;

procedure TframeParam.InitVariation;
begin
  (FSynth.SelectedSlot as TG2USBSlot).SendCopyVariationMessage(8,
    FSynth.SelectedSlot.Patch.ActiveVariation)
end;

constructor TframeParam.Create(AOwner: TComponent);
begin
  inherited;
  rbPage.Value := 0;
  rbPageIndex.Value := 0;
  rbGlobalPage.Value := 0;
  rbGlobalPageIndex.Value := 0;
end;

destructor TframeParam.Destroy;
begin
  inherited;
end;

// TODO Implement this...
{ procedure TframeParam.SetValue(const aValue : integer);
  begin
  //
  end;
  procedure TframeParam.SetButtonText(const aValue : TStrings);
  begin
  //
  end;
  procedure TframeParam.SetMorphValue(const aValue : integer);
  begin
  //
  end;
  procedure TframeParam.SetHasMorph(const aValue : boolean);
  begin
  //
  end;
  procedure TframeParam.SetSelected(const aValue : boolean);
  begin
  //
  end;
  function TframeParam.GetValueText( aIndex : integer): string;
  begin
  //
  end;
  procedure TframeParam.SetValueText( aIndex : integer; const aValue : string);
  begin
  //
  end;
  function TframeParam.GetCodeRef : integer;
  begin
  //
  end;
  procedure TframeParam.Redraw;
  begin
  //
  end;
  procedure TframeParam.ClearDataDependency( aData : IG2DataParam);
  begin
  //if aData = FParameter as IG2DataPAram then
  //  FParameter := nil;
  end; }

procedure TframeParam.RemoveReference(aData: IG2Subject);
begin
  if aData = FParameter as IG2DataPAram then
    FParameter := nil;
end;

procedure TframeParam.btCopyToInitChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(8);
end;

procedure TframeParam.btCopyVarTo1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(0);
end;

procedure TframeParam.btCopyVarTo2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(1);
end;

procedure TframeParam.btCopyVarTo3ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(2);
end;

procedure TframeParam.btCopyVarTo4ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(3);
end;

procedure TframeParam.btCopyVarTo5ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(4);
end;

procedure TframeParam.btCopyVarTo6ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(5);
end;

procedure TframeParam.btCopyVarTo7ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(6);
end;

procedure TframeParam.btCopyVarTo8ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    CopyVariation(7);
end;

procedure TframeParam.btDefaultValueChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    DefaultValue;
end;

procedure TframeParam.btGlobalKnob1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 0);
  end;
end;

procedure TframeParam.btGlobalKnob2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 1);
  end;
end;

procedure TframeParam.btGlobalKnob3ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 2);
  end;
end;

procedure TframeParam.btGlobalKnob4ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 3);
  end;
end;

procedure TframeParam.btGlobalKnob5ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 4);
  end;
end;

procedure TframeParam.btGlobalKnob6ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 5);
  end;
end;

procedure TframeParam.btGlobalKnob7ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 6);
  end;
end;

procedure TframeParam.btGlobalKnob8ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignGlobalKnob;
    1:
      AssignGlobalKnob(GetGlobalKnobIndexOffset + 7);
  end;
end;

procedure TframeParam.btInitVariationChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    InitVariation;
end;

procedure TframeParam.btKnob1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 0);
  end;
end;

procedure TframeParam.btKnob2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 1);
  end;
end;

procedure TframeParam.btKnob3ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 2);
  end;
end;

procedure TframeParam.btKnob4ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 3);
  end;
end;

procedure TframeParam.btKnob5ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 4);
  end;
end;

procedure TframeParam.btKnob6ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 5);
  end;
end;

procedure TframeParam.btKnob7ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 6);
  end;
end;

procedure TframeParam.btKnob8ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0:
      DeassignKnob;
    1:
      AssignKnob(GetKnobIndexOffset + 7);
  end;
end;

procedure TframeParam.btParamAssignCCChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if assigned(FParameter) then
  begin
    case aValue of
      0:
        begin
          DeassignMidiCC;
        end;
      1:
        begin
          if FLastReceivedMidiCC <> 0 then
            AssignMidiCC(FLastReceivedMidiCC);
        end;
    end;
  end;
end;

procedure TframeParam.SetLabelIndex(const Value: Integer);
begin
end;

procedure TframeParam.SetMorphIndex(const Value: Integer);

  procedure SetSelectKnob;
  begin
    kMorphWheel.Selected := Value = 0;
    kMorphVelocity.Selected := Value = 1;
    kMorphKeyboard.Selected := Value = 2;
    kMorphAftertouch.Selected := Value = 3;
    kMorphSustainPedal.Selected := Value = 4;
    kMorphControlPedal.Selected := Value = 5;
    kMorphPitchStick.Selected := Value = 6;
    kMorphGWheel2.Selected := Value = 7;
  end;

begin
  if FMorphIndex <> Value then
  begin
    FMorphIndex := Value;
    if assigned(FParameter) then
    begin
      FParameter.Patch.SelectedMorphIndex := FMorphIndex;
      FParameter.Patch.InvalidateParameters;
    end;
    SetSelectKnob;
  end;
end;

procedure TframeParam.SetParamMorphValue(const aMorphIndex, aValue: Integer);
begin
  if assigned(FParameter) then
  begin
    FParameter.SetMorphValue(aMorphIndex,
      FParameter.Patch.ActiveVariation, aValue);
    FParameter.InvalidateControl;
  end;
end;

procedure TframeParam.SetParameter(const Value: TG2FileParameter);
begin
  if Value <> FParameter then
  begin
    if assigned(FParameter) then
    begin
      FParameter.RemoveObserver(self as IG2Observer);
      FParameter.Patch.RemoveObserver(self as IG2Observer);
    end;

    FParameter := Value;

    if assigned(FParameter) then
    begin
      FParameter.RegisterObserver(self as IG2Observer);
      FParameter.Patch.RegisterObserver(self as IG2Observer);
    end;
  end;
  UpdateControls;
end;

procedure TframeParam.SetSynth(const Value: TG2GraphFMX);
begin
  if FSynth <> Value then
  begin
    if assigned(FSynth) then
      FSynth.RemoveObserver(self as IG2Observer);

    FSynth := Value;

    if assigned(FSynth) then
      FSynth.RegisterObserver(self);
  end;
end;

procedure TframeParam.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframeParam.Update(aG2Event: TG2Event);
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
      ;
    EvtVariationChange:
      begin
        UpdateControls;
      end;
    EvtCopyVariation:
      begin
        UpdateControls;
      end;
    EvtMidiClockReceive:
      ;
    EvtClockRunChange:
      ;
    EvtClockBPMChange:
      ;
    EvtMidiCCRecieve:
      begin
        FLastReceivedMidiCC := (Synth.Performance as TG2USBPerformance).LastMidiCC;
      end;
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
      begin
        //
      end;
    EvtLabelValueChange:
      begin
        UpdateControls;
      end;
    EvtMorphChange:
      begin
        UpdateControls;
      end;
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
      begin
        UpdateControls;
      end;
    EvtAssignKnob:
      begin
        UpdateControls;
      end;
    EvtDeassignGlobalKnob:
      begin
        UpdateControls;
      end;
    EvtAssignGlobalKnob:
      begin
        UpdateControls;
      end;
  end;
end;

procedure TframeParam.UpdateAssignCCButton(aCC: byte);
begin
  eMidiCC.Text := IntToStr(aCC);
  if assigned(FParameter) then
  begin
    if assigned(FParameter.Controller) then
    begin
      btParamAssignCC.State := csDefault;
      btParamAssignCC.Value := 1;
      btParamAssignCC.ButtonText[1] := 'Deassign CC ' + IntToStr(FParameter.Controller.MidiCC);
      btParamAssignCC.ButtonText[0] := 'Assign CC ' + IntToStr(FLastReceivedMidiCC);
    end
    else
    begin
      if FLastReceivedMidiCC = 0 then
        btParamAssignCC.State := csDisabled
      else
        btParamAssignCC.State := csDefault;

      btParamAssignCC.Value := 0;
      btParamAssignCC.ButtonText[1] := 'Deassign CC';
      btParamAssignCC.ButtonText[0] := 'Assign CC ' + IntToStr(FLastReceivedMidiCC);
    end;
  end
  else
  begin
    btParamAssignCC.Value := 0;
    btParamAssignCC.State := csDisabled;
  end;
  btParamAssignCC.Redraw;
end;

procedure TframeParam.UpdateControls;
var
  i: Integer;
  Knob: TKnob;
  GlobalKnob: TGlobalKnob;
  KnobIndex: Integer;
  Synth: TG2GraphFMX;
  Patch: TG2GraphPatchFMX;
  Perf: TG2GraphPerformanceFMX;
  Variation: Integer;

  function BtnAssignKnob(aIndex: Integer): TG2BtnText;
  begin
    case aIndex of
      0:
        Result := btKnob1;
      1:
        Result := btKnob2;
      2:
        Result := btKnob3;
      3:
        Result := btKnob4;
      4:
        Result := btKnob5;
      5:
        Result := btKnob6;
      6:
        Result := btKnob7;
      7:
        Result := btKnob8;
    else
      raise Exception.Create('Index out of range');
    end;
  end;

  function BtnAssignGlobalKnob(aIndex: Integer): TG2BtnText;
  begin
    case aIndex of
      0:
        Result := btGlobalKnob1;
      1:
        Result := btGlobalKnob2;
      2:
        Result := btGlobalKnob3;
      3:
        Result := btGlobalKnob4;
      4:
        Result := btGlobalKnob5;
      5:
        Result := btGlobalKnob6;
      6:
        Result := btGlobalKnob7;
      7:
        Result := btGlobalKnob8;
    else
      raise Exception.Create('Index out of range');
    end;
  end;

  function CopyVariationKnob(aIndex: Integer): TG2BtnText;
  begin
    case aIndex of
      0:
        Result := btCopyVarTo1;
      1:
        Result := btCopyVarTo2;
      2:
        Result := btCopyVarTo3;
      3:
        Result := btCopyVarTo4;
      4:
        Result := btCopyVarTo5;
      5:
        Result := btCopyVarTo6;
      6:
        Result := btCopyVarTo7;
      7:
        Result := btCopyVarTo8;
    else
      raise Exception.Create('Index out of range');
    end;
  end;

begin
  if not assigned(FSynth) then
    Exit;

  Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;
  Perf := FSynth.Performance as TG2GraphPerformanceFMX;

  for i := 0 to 7 do
  begin
    if Patch.ActiveVariation = i then
      CopyVariationKnob(i).State := csDisabled
    else
      CopyVariationKnob(i).State := csDefault;
  end;

  if assigned(FParameter) then
  begin
    eParamName.Text := FParameter.ParamName;

    if (FParameter.Location = ltPatch) and (FParameter.ModuleIndex = PATCH_MORPH)
    then
    begin
      // The morph parameter labels seems to be connected to the button not the knob!
      if FParameter.ParamIndex < 8 then
        eParamLabel.Text := Patch.PatchPart[ord(ltPatch)].ParameterLabelList.FindParamLabel(
          PATCH_MORPH, FParameter.ParamIndex + 8, 0)
      else
        eParamLabel.Text := Patch.PatchPart[ord(ltPatch)].ParameterLabelList.FindParamLabel(
          PATCH_MORPH, FParameter.ParamIndex, 0);
      eParamLabel.Enabled := True;
    end
    else
    begin
      eParamLabel.Text := FParameter.ParamLabel[FParameter.LabelIndex];
      eParamLabel.Enabled := FParameter.CanChangeLabel;
    end;

    if (FParameter.KnobAssignmentsAllowed) then
    begin

      if assigned(FParameter.Knob) and (FParameter.Knob.IsAssigned = 1) then
      begin
        KnobIndex := FParameter.Knob.KnobIndex;
        rbPage.Value := trunc(KnobIndex / 24);
        rbPageIndex.Value := trunc((KnobIndex mod 24) / 8);
      end
      else
        KnobIndex := -1;

      for i := 0 to 7 do
      begin
        Knob := Patch.GetKnob(GetKnobIndexOffset + i);
        if (Knob <> nil) and (Knob.IsAssigned = 1) then
        begin
          if Knob.KnobIndex = KnobIndex then
          begin
            BtnAssignKnob(i).Value := 1;
            BtnAssignKnob(i).State := csDefault;
          end
          else
          begin
            BtnAssignKnob(i).Value := 0;
            BtnAssignKnob(i).State := csDisabled;
          end;
        end
        else
        begin
          BtnAssignKnob(i).Value := 0;
          BtnAssignKnob(i).State := csDefault;
        end;
        BtnAssignKnob(i).Redraw;
      end;

      if assigned(FParameter.GlobalKnob)
      and (FParameter.GlobalKnob.IsAssigned = 1) then
      begin
        KnobIndex := FParameter.GlobalKnob.KnobIndex;
        rbGlobalPage.Value := trunc(KnobIndex / 24);
        rbGlobalPageIndex.Value := trunc((KnobIndex mod 24) / 8);
      end
      else
        KnobIndex := -1;

      for i := 0 to 7 do
      begin
        GlobalKnob := Perf.GetGlobalKnob(GetGlobalKnobIndexOffset + i);
        if (GlobalKnob <> nil) and (GlobalKnob.IsAssigned = 1) then
        begin
          if GlobalKnob.KnobIndex = KnobIndex then
          begin
            BtnAssignGlobalKnob(i).Value := 1;
            BtnAssignGlobalKnob(i).State := csDefault;
          end
          else
          begin
            BtnAssignGlobalKnob(i).Value := 0;
            BtnAssignGlobalKnob(i).State := csDisabled;
          end;
        end
        else
        begin
          BtnAssignGlobalKnob(i).Value := 0;
          BtnAssignGlobalKnob(i).State := csDefault;
        end;
        BtnAssignGlobalKnob(i).Redraw;
      end;
    end
    else
    begin
      for i := 0 to 7 do
      begin
        BtnAssignKnob(i).Value := 0;
        BtnAssignKnob(i).State := csDisabled;
        BtnAssignGlobalKnob(i).Value := 0;
        BtnAssignGlobalKnob(i).State := csDisabled;
      end;
    end;

    if (FParameter.MorphAssignmentsAllowed) then
    begin
      if (FParameter.Location = ltPatch)
      and (FParameter.ModuleIndex = PATCH_MORPH) then
      begin
        kMorphWheel.State := csDisabled;
        kMorphVelocity.State := csDisabled;
        kMorphKeyboard.State := csDisabled;
        kMorphAftertouch.State := csDisabled;
        kMorphSustainPedal.State := csDisabled;
        kMorphControlPedal.State := csDisabled;
        kMorphPitchStick.State := csDisabled;
        kMorphGWheel2.State := csDisabled;
      end
      else
      begin
        kMorphWheel.State := csDefault;
        kMorphVelocity.State := csDefault;
        kMorphKeyboard.State := csDefault;
        kMorphAftertouch.State := csDefault;
        kMorphSustainPedal.State := csDefault;
        kMorphControlPedal.State := csDefault;
        kMorphPitchStick.State := csDefault;
        kMorphGWheel2.State := csDefault;
      end;
      Variation := FParameter.Patch.ActiveVariation;
      kMorphWheel.Value := MorphRangeToInt(FParameter.GetMorphValue(0,
        Variation));
      kMorphVelocity.Value := MorphRangeToInt(FParameter.GetMorphValue(1,
        Variation));
      kMorphKeyboard.Value := MorphRangeToInt(FParameter.GetMorphValue(2,
        Variation));
      kMorphAftertouch.Value :=
        MorphRangeToInt(FParameter.GetMorphValue(3, Variation));
      kMorphSustainPedal.Value :=
        MorphRangeToInt(FParameter.GetMorphValue(4, Variation));
      kMorphControlPedal.Value :=
        MorphRangeToInt(FParameter.GetMorphValue(5, Variation));
      kMorphPitchStick.Value :=
        MorphRangeToInt(FParameter.GetMorphValue(6, Variation));
      kMorphGWheel2.Value := MorphRangeToInt(FParameter.GetMorphValue(7,
        Variation));
    end
    else
    begin
      kMorphWheel.State := csDisabled;
      kMorphVelocity.State := csDisabled;
      kMorphKeyboard.State := csDisabled;
      kMorphAftertouch.State := csDisabled;
      kMorphSustainPedal.State := csDisabled;
      kMorphControlPedal.State := csDisabled;
      kMorphPitchStick.State := csDisabled;
      kMorphGWheel2.State := csDisabled;
    end;

    if (FParameter.MidiAssignmentsAllowed) then
    begin
      UpdateAssignCCButton(FLastReceivedMidiCC);
    end
    else
    begin
      btParamAssignCC.State := csDisabled;
    end;

  end
  else
  begin
    eParamName.Text := '';
    eParamLabel.Enabled := False;

    btParamAssignCC.State := csDisabled;

    for i := 0 to 7 do
    begin
      BtnAssignKnob(i).Value := 0;
      BtnAssignKnob(i).State := csDisabled;
      BtnAssignGlobalKnob(i).Value := 0;
      BtnAssignGlobalKnob(i).State := csDisabled;
    end;

    kMorphWheel.State := csDisabled;
    kMorphVelocity.State := csDisabled;
    kMorphKeyboard.State := csDisabled;
    kMorphAftertouch.State := csDisabled;
    kMorphSustainPedal.State := csDisabled;
    kMorphControlPedal.State := csDisabled;
    kMorphPitchStick.State := csDisabled;
    kMorphGWheel2.State := csDisabled;
  end;
end;

procedure TframeParam.AssignGlobalKnob(aKnobIndex: Integer);
var
  Patch: TG2USBPatch;
  MidiCC: byte;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.KnobAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  Patch.MessAssignGlobalKnob(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex, aKnobIndex);

  if (FframeAppSettings.AutoAssignMidi = 1) then
  begin
    MidiCC := FframeAppSettings.FindMidiToKnob(FSynth, g2psGlobal, g2ctKnob,
      aKnobIndex);
    if MidiCC <> 0 then
      Patch.MessAssignMidiCC(FParameter.Location, FParameter.ModuleIndex,
        FParameter.ParamIndex, MidiCC);
  end;
end;

procedure TframeParam.AssignKnob(aKnobIndex: Integer);
var
  Patch: TG2USBPatch;
  MidiCC: byte;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.KnobAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  Patch.MessAssignKnob(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex, aKnobIndex);

  if (FframeAppSettings.AutoAssignMidi = 1) then
  begin
    MidiCC := FframeAppSettings.FindMidiToKnob(FSynth, g2psParam, g2ctKnob,
      aKnobIndex);
    if MidiCC <> 0 then
      Patch.MessAssignMidiCC(FParameter.Location, FParameter.ModuleIndex,
        FParameter.ParamIndex, MidiCC);
  end;
end;

procedure TframeParam.AssignMidiCC(aMidiCC: byte);
var
  Patch: TG2USBPatch;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.MidiAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  Patch.MessAssignMidiCC(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex, aMidiCC);
end;

procedure TframeParam.DeassignGlobalKnob;
var
  KnobIndex: Integer;
  Patch: TG2USBPatch;
  MidiCC: byte;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.KnobAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  KnobIndex := Patch.Performance.GlobalKnobList.FindGlobalKnobIndex
    (Patch.Slot.SlotIndex, FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex);

  if KnobIndex <> -1 then
  begin
    if (FframeAppSettings.AutoAssignMidi = 1)
    and assigned(FParameter.Controller) then
    begin
      MidiCC := FframeAppSettings.FindMidiToKnob(FSynth, g2psGlobal, g2ctKnob,
        KnobIndex);
      if (MidiCC <> 0) and (MidiCC = FParameter.Controller.MidiCC) then
        DeassignMidiCC;
    end;

    Patch.MessDeassignGlobalKnob(KnobIndex);
  end;
end;

procedure TframeParam.DeassignKnob;
var
  KnobIndex: Integer;
  Patch: TG2USBPatch;
  MidiCC: byte;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.KnobAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  KnobIndex := Patch.FindKnob(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex);

  if KnobIndex <> -1 then
  begin
    if (FframeAppSettings.AutoAssignMidi = 1) and assigned(FParameter.Controller)
    then
    begin
      MidiCC := FframeAppSettings.FindMidiToKnob(FSynth, g2psParam, g2ctKnob,
        KnobIndex);
      if (MidiCC <> 0) and (MidiCC = FParameter.Controller.MidiCC) then
        DeassignMidiCC;
    end;

    Patch.MessDeassignKnob(KnobIndex);
  end;
end;

procedure TframeParam.DeassignMidiCC;
var
  MidiCC: byte;
  Patch: TG2USBPatch;
begin
  if not assigned(FParameter) then
    Exit;

  if not FParameter.MidiAssignmentsAllowed then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  MidiCC := Patch.FindMidiCC(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex);
  if MidiCC <> 0 then
    Patch.MessDeassignMidiCC(MidiCC);
end;

procedure TframeParam.DefaultValue;
var
  Patch: TG2USBPatch;
begin
  if not assigned(FParameter) then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  Patch.SetParamValue(FParameter.Location, FParameter.ModuleIndex,
    FParameter.ParamIndex, Patch.ActiveVariation, FParameter.DefaultValue);
end;

procedure TframeParam.eMidiCCExit(Sender: TObject);
var
  Value, c: Integer;
begin
  Val(eMidiCC.Text, Value, c);
  if c = 0 then
  begin
    FLastReceivedMidiCC := Value;
    UpdateControls;
  end;
end;

procedure TframeParam.eParamLabelExit(Sender: TObject);
var
  Patch: TG2USBPatch;
  ParamIndex: Integer;
begin
  if not assigned(FParameter) then
    Exit;

  Patch := FParameter.Patch as TG2USBPatch;

  if (FParameter.Location = ltPatch)
  and (FParameter.ModuleIndex = PATCH_MORPH) then
  begin
    // The morph parameterer labels seems to be connected to the button, not the knob
    ParamIndex := FParameter.ParamIndex;
    if ParamIndex < 8 then
      ParamIndex := ParamIndex + 8;

    Patch.MessSetModuleParamLabels(FParameter.Location, FParameter.ModuleIndex,
      ParamIndex, FParameter.LabelIndex, eParamLabel.Text);
  end
  else
  begin
    if not FParameter.CanChangeLabel then
      Exit;

    Patch.MessSetModuleParamLabels(FParameter.Location, FParameter.ModuleIndex,
      FParameter.ParamIndex, FParameter.LabelIndex, eParamLabel.Text);
  end;
end;

function TframeParam.GetGlobalKnobIndexOffset: Integer;
begin
  Result := rbGlobalPageIndex.Value * 8 + rbGlobalPage.Value * 8 * 3;
end;

function TframeParam.GetKnobIndexOffset: Integer;
begin
  Result := rbPageIndex.Value * 8 + rbPage.Value * 8 * 3;
end;

procedure TframeParam.kMorphAftertouchChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(3, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphAftertouchMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 3;
  if assigned(FParameter) then
  begin
    kMorphAftertouch.ProcessMouseDown(kMorphAftertouch, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphControlPedalChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(5, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphControlPedalMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 5;
  if assigned(FParameter) then
  begin
    kMorphControlPedal.ProcessMouseDown(kMorphControlPedal, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphGWheel2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(7, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphGWheel2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 7;
  if assigned(FParameter) then
  begin
    kMorphGWheel2.ProcessMouseDown(kMorphGWheel2, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphKeyboardChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(2, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphKeyboardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 2;
  if assigned(FParameter) then
  begin
    kMorphKeyboard.ProcessMouseDown(kMorphKeyboard, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphPitchStickChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(6, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphPitchStickMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 6;
  if assigned(FParameter) then
  begin
    kMorphPitchStick.ProcessMouseDown(kMorphPitchStick, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphSustainPedalChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(4, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphSustainPedalMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 4;
  if assigned(FParameter) then
  begin
    kMorphSustainPedal.ProcessMouseDown(kMorphSustainPedal, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphVelocityChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(1, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphVelocityMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 1;
  if assigned(FParameter) then
  begin
    kMorphVelocity.ProcessMouseDown(kMorphVelocity, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.kMorphWheelChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SetParamMorphValue(0, IntToMorphRange(aValue));
end;

procedure TframeParam.kMorphWheelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  MorphIndex := 0;
  if assigned(FParameter) then
  begin
    kMorphWheel.ProcessMouseDown(kMorphWheel, Shift, 0, X, Y);
  end;
end;

procedure TframeParam.rbGlobalPageChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateControls;
end;

procedure TframeParam.rbGlobalPageIndexChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateControls;
end;

procedure TframeParam.rbPageChangeValue(Sender: TObject; const aValue: Integer);
begin
  UpdateControls;
end;

procedure TframeParam.rbPageIndexChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateControls;
end;

end.
