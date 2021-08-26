unit UnitKnobs;

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
  FMX.Objects,
  FMX.Controls.Presentation,
  BVE.NMG2Types,
  BVE.NMG2File,
  BVE.NMG2ControlsFMX,
  BVE.NMG2GraphFMX,
  UnitAppSettings;

type
  TframeKnobs = class(TFrame, IG2Observer)
    kKnob1: TG2Knob;
    kKnob2: TG2Knob;
    kKnob3: TG2Knob;
    kKnob4: TG2Knob;
    kKnob5: TG2Knob;
    kKnob6: TG2Knob;
    kKnob7: TG2Knob;
    kKnob8: TG2Knob;
    tfDisplayB1: TG2TextField;
    tfDisplayB2: TG2TextField;
    tfDisplayB3: TG2TextField;
    tfDisplayB4: TG2TextField;
    tfDisplayB5: TG2TextField;
    tfDisplayB6: TG2TextField;
    tfDisplayB7: TG2TextField;
    tfDisplayB8: TG2TextField;
    bfButton1: TG2BtnFlat;
    bfButton2: TG2BtnFlat;
    bfButton3: TG2BtnFlat;
    bfButton4: TG2BtnFlat;
    bfButton5: TG2BtnFlat;
    bfButton6: TG2BtnFlat;
    bfButton7: TG2BtnFlat;
    bfButton8: TG2BtnFlat;
    rbPage: TG2BtnRadio;
    rbSource: TG2BtnRadio;
    rbPageIndex: TG2BtnRadio;
    tfDisplayC1: TG2TextField;
    tfDisplayC2: TG2TextField;
    tfDisplayC3: TG2TextField;
    tfDisplayC4: TG2TextField;
    tfDisplayC5: TG2TextField;
    tfDisplayC6: TG2TextField;
    tfDisplayC7: TG2TextField;
    tfDisplayC8: TG2TextField;
    tfDisplayA1: TG2TextField;
    tfDisplayA2: TG2TextField;
    tfDisplayA3: TG2TextField;
    tfDisplayA4: TG2TextField;
    tfDisplayA5: TG2TextField;
    tfDisplayA6: TG2TextField;
    tfDisplayA7: TG2TextField;
    tfDisplayA8: TG2TextField;
    tfDisplayD1: TG2TextField;
    tfDisplayD2: TG2TextField;
    tfDisplayD3: TG2TextField;
    tfDisplayD4: TG2TextField;
    tfDisplayD5: TG2TextField;
    tfDisplayD6: TG2TextField;
    tfDisplayD7: TG2TextField;
    tfDisplayD8: TG2TextField;
    eMidiCC1: TEdit;
    eMidiCC2: TEdit;
    eMidiCC3: TEdit;
    eMidiCC4: TEdit;
    eMidiCC5: TEdit;
    eMidiCC6: TEdit;
    eMidiCC7: TEdit;
    eMidiCC8: TEdit;
    eMidiCC9: TEdit;
    eMidiCC10: TEdit;
    eMidiCC11: TEdit;
    eMidiCC12: TEdit;
    eMidiCC13: TEdit;
    eMidiCC14: TEdit;
    eMidiCC15: TEdit;
    eMidiCC16: TEdit;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    procedure rbPageChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbPageIndexChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbSourceChangeValue(Sender: TObject; const aValue: Integer);
    procedure tfDisplayAGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfDisplayCGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfDisplayDGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure eMidiCCExit(Sender: TObject);
    procedure eMidiCCEnter(Sender: TObject);
  private
    [Weak] FSynth: TG2GraphFMX;
    [Weak] FframeAppSettings: TframeAppSettings;
    procedure SetSynth(const Value: TG2GraphFMX);
    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference(aData: IG2Subject);
    function GetKnobIndexOffset: Integer;
    function GetButton(aIndex: Integer): TG2BtnFlat;
    function GetKnob(aIndex: Integer): TG2Knob;
    function GetDisplayA(aIndex: Integer): TG2TextField;
    function GetDisplayB(aIndex: Integer): TG2TextField;
    function GetDisplayC(aIndex: Integer): TG2TextField;
    function GetDisplayD(aIndex: Integer): TG2TextField;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AssignParamToKnob(aIndex: Integer; aParam: TG2FileParameter);
    procedure AssignParamToButton(aIndex: Integer; aParam: TG2FileParameter);
    procedure DeassignControls;
    procedure AssignMorphParameters;
    procedure AssignKnobs;
    procedure AssignGlobalKnobs;
    procedure AssignPatchParams;
    procedure SetEditMidiCC(aIndex: Integer; aValue: string; aVisible: boolean);
    procedure UpdateControls;
    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);
    property frameAppSettings: TframeAppSettings read FframeAppSettings
      write FframeAppSettings;
    property Synth: TG2GraphFMX read FSynth write SetSynth;
    property DisplayA[index: Integer]: TG2TextField read GetDisplayA;
    property DisplayB[index: Integer]: TG2TextField read GetDisplayB;
    property DisplayC[index: Integer]: TG2TextField read GetDisplayC;
    property DisplayD[index: Integer]: TG2TextField read GetDisplayD;
    property Knob[index: Integer]: TG2Knob read GetKnob;
    property Button[index: Integer]: TG2BtnFlat read GetButton;
  end;

implementation

{$R *.fmx}

{ TframeKnobs }

procedure TframeKnobs.AssignGlobalKnobs;
var
  Knob: TKnob;
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    Knob := FSynth.Performance.GetGlobalKnob(GetKnobIndexOffset + i);
    if assigned(Knob) and (Knob.IsAssigned = 1) then
    begin
      AssignParamToKnob(i, Knob.Parameter);
      if assigned(Knob.Parameter.ButtonParam) then
        AssignParamToButton(i, Knob.Parameter.ButtonParam)
      else
        AssignParamToButton(i, nil);
    end
    else
    begin
      AssignParamToKnob(i, nil);
      AssignParamToButton(i, nil);
    end;
  end;
end;

procedure TframeKnobs.AssignKnobs;
var
  Knob: TKnob;
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    Knob := FSynth.SelectedSlot.Patch.GetKnob(GetKnobIndexOffset + i);
    if assigned(Knob) and (Knob.IsAssigned = 1) then
    begin
      AssignParamToKnob(i, Knob.Parameter);
      if assigned(Knob.Parameter.ButtonParam) then
        AssignParamToButton(i, Knob.Parameter.ButtonParam)
      else
        AssignParamToButton(i, nil);
    end
    else
    begin
      AssignParamToKnob(i, nil);
      AssignParamToButton(i, nil);
    end;
  end;
end;

procedure TframeKnobs.AssignMorphParameters;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    AssignParamToKnob(i, FSynth.SelectedSlot.Patch.GetMorphKnobParameter(i));
    AssignParamToButton(i,
      FSynth.SelectedSlot.Patch.GetMorphKnobParameter(i + 8));
  end;
end;

procedure TframeKnobs.AssignParamToKnob(aIndex: Integer;
  aParam: TG2FileParameter);
begin
  DisplayB[aIndex].OnGetTextFunc := nil;
  DisplayA[aIndex].OnGetTextFunc := tfDisplayAGetTextFunc;
  DisplayD[aIndex].OnGetTextFunc := tfDisplayDGetTextFunc;
  if assigned(aParam) then
  begin
    Knob[aIndex].DataWriter := aParam;
    DisplayA[aIndex].AddDataDependency(aParam, nil);
    DisplayB[aIndex].AddDataDependency(aParam, nil);
    DisplayC[aIndex].AddDataDependency(aParam, nil);
    Knob[aIndex].State := csDefault;
    DisplayA[aIndex].State := csDefault;
    DisplayB[aIndex].State := csDefault;
    DisplayD[aIndex].State := csDefault;
    aParam.InvalidateControl;
  end
  else
  begin
    Knob[aIndex].DataWriter := nil;
    DisplayA[aIndex].ClearDataDependencies;
    DisplayB[aIndex].ClearDataDependencies;
    DisplayD[aIndex].ClearDataDependencies;
    Knob[aIndex].State := csDisabled;
    DisplayA[aIndex].State := csDisabled;
    DisplayB[aIndex].State := csDisabled;
    DisplayD[aIndex].State := csDisabled;
    Knob[aIndex].Selected := False;
    Knob[aIndex].Value := 0;
    Knob[aIndex].MorphValue := 0;
    Knob[aIndex].Redraw;
  end;
end;

procedure TframeKnobs.AssignPatchParams;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    AssignParamToKnob(i, FSynth.SelectedSlot.Patch.GetPatchKnobParameter(i));
    AssignParamToButton(i, FSynth.SelectedSlot.Patch.GetPatchKnobParameter(i + 8));
  end;
end;

constructor TframeKnobs.Create(AOwner: TComponent);
begin
  inherited;
  rbSource.Value := 0;
  rbPage.Value := 0;
  rbPageIndex.Value := 0;
end;

procedure TframeKnobs.AssignParamToButton(aIndex: Integer;
  aParam: TG2FileParameter);
begin
  DisplayC[aIndex].OnGetTextFunc := tfDisplayCGetTextFunc;
  if assigned(aParam) then
  begin
    Button[aIndex].DataWriter := aParam;
    DisplayC[aIndex].AddDataDependency(aParam, nil);
    Button[aIndex].State := csDefault;
    DisplayC[aIndex].State := csDefault;
    aParam.InvalidateControl;
  end
  else
  begin
    DisplayC[aIndex].ClearDataDependencies;
    Button[aIndex].DataWriter := nil;
    Button[aIndex].State := csDisabled;
    DisplayC[aIndex].State := csDisabled;
    Button[aIndex].Selected := False;
    Button[aIndex].ButtonText.Clear;
    Button[aIndex].Value := 0;
    Button[aIndex].MorphValue := 0;
    Button[aIndex].Redraw;
  end;
end;

procedure TframeKnobs.DeassignControls;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    Knob[i].DataWriter := nil;
    Button[i].DataWriter := nil;
    DisplayB[i].ClearDataDependencies;
    DisplayC[i].ClearDataDependencies;
    DisplayB[i].OnGetTextFunc := nil;
  end;
end;

procedure TframeKnobs.eMidiCCEnter(Sender: TObject);
begin
  //
end;

procedure TframeKnobs.eMidiCCExit(Sender: TObject);
var
  KnobIndex: Integer;
  eMidiCC: TEdit;
  Value, CurrentValue, c, KnobNo: Integer;
  ControlType: TG2ControlType;
  Parameter: TG2FileParameter;
  Knob: TKnob;
begin
  if not(FframeAppSettings.AutoAssignMidi = 1) then
    Exit;

  eMidiCC := Sender as TEdit;

  case rbSource.Value of
    0, 1:
      begin
        if eMidiCC.Tag < 8 then
        begin
          ControlType := g2ctKnob;
          KnobNo := eMidiCC.Tag;
        end
        else
        begin
          ControlType := g2ctButton;
          KnobNo := eMidiCC.Tag - 8;
        end;
        KnobIndex := GetKnobIndexOffset + KnobNo;
        Val(eMidiCC.Text, Value, c);

        if (eMidiCC.Text = '') or (c > 0) or (Value = 0)
        or (not CheckCCAllowed(Value)) then
        begin
          // Auto Deassign Midi
          Knob := nil;
          if rbSource.Value = 0 then
            Knob := FSynth.SelectedSlot.Patch.GetKnob(KnobIndex)
          else
            Knob := FSynth.Performance.GetGlobalKnob(KnobIndex);
          if assigned(Knob) and (Knob.IsAssigned = 1) then
          begin
            if ControlType = g2ctKnob then
            begin
              if assigned(Knob.Parameter) and assigned(Knob.Parameter.Controller)
              then
                (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessDeassignMidiCC(
                  Knob.Parameter.Controller.MidiCC);
            end
            else
            begin
              if assigned(Knob.Parameter) and
                assigned(Knob.Parameter.ButtonParam) and
                assigned(Knob.Parameter.ButtonParam.Controller) then
                (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessDeassignMidiCC(
                  Knob.Parameter.ButtonParam.Controller.MidiCC);
            end;
          end;
          eMidiCC.Text := '';
          FframeAppSettings.DeleteMidiToKnob(FSynth,
            TG2ParamSource(rbSource.Value), ControlType, KnobIndex);
        end
        else
        begin
          if FframeAppSettings.MidiToKnobCheckCC(FSynth, Value) then
            raise Exception.Create('Midi CC ' + IntToStr(Value) +
              ' is already used.');

          FframeAppSettings.AddMidiToKnob(FSynth,
            TG2ParamSource(rbSource.Value), ControlType, KnobIndex, Value);

          // Auto assign Midi
          Knob := nil;
          if rbSource.Value = 0 then
            Knob := FSynth.SelectedSlot.Patch.GetKnob(KnobIndex)
          else
            Knob := FSynth.Performance.GetGlobalKnob(KnobIndex);

          if assigned(Knob) and (Knob.IsAssigned = 1) then
          begin
            if ControlType = g2ctKnob then
            begin
              if assigned(Knob.Parameter) then
                (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessAssignMidiCC(
                  Knob.Parameter.Location,
                  Knob.Parameter.ModuleIndex,
                  Knob.Parameter.ParamIndex,
                  Value);
            end
            else
            begin
              if assigned(Knob.Parameter) and
                assigned(Knob.Parameter.ButtonParam) then
                (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessAssignMidiCC(
                  Knob.Parameter.ButtonParam.Location,
                  Knob.Parameter.ButtonParam.ModuleIndex,
                  Knob.Parameter.ButtonParam.ParamIndex,
                  Value);
            end;
          end;
        end;
        FframeAppSettings.SaveSettings;
      end;

    2, 3:
      begin
        if eMidiCC.Tag < 8 then
        begin
          ControlType := g2ctKnob;
          KnobNo := eMidiCC.Tag;
        end
        else
        begin
          ControlType := g2ctButton;
          KnobNo := eMidiCC.Tag - 8;
        end;

        KnobIndex := eMidiCC.Tag;
        if (rbSource.Value = 3) and ((KnobNo = 0) or (KnobNo = 1)) then
        begin
          // No CC allowed on Clock and Voices params!
          eMidiCC.Text := '';
        end;

        Val(eMidiCC.Text, Value, c);
        if (eMidiCC.Text = '') or (c > 0) or (Value = 0)
        or (not CheckCCAllowed(Value)) then
        begin
          eMidiCC.Text := '';
          FframeAppSettings.DeleteMidiToKnob(FSynth,
            TG2ParamSource(rbSource.Value), ControlType, KnobNo);

          // Auto deassign Midi
          if rbSource.Value = 2 then
            Parameter := FSynth.SelectedSlot.Patch.GetMorphKnobParameter(KnobIndex)
          else
            Parameter := FSynth.SelectedSlot.Patch.GetPatchKnobParameter(KnobIndex);

          if assigned(Parameter) and assigned(Parameter.Controller) then
            (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessDeassignMidiCC(
              Parameter.Controller.MidiCC);
        end
        else
        begin
          if FframeAppSettings.MidiToKnobCheckCC(FSynth, Value) then
            raise Exception.Create('Midi CC ' + IntToStr(Value) +
              ' is already used.');

          FframeAppSettings.AddMidiToKnob(FSynth,
            TG2ParamSource(rbSource.Value), ControlType, KnobNo, Value);

          // Auto Assign Midi
          if rbSource.Value = 2 then
            Parameter := FSynth.SelectedSlot.Patch.GetMorphKnobParameter(KnobIndex)
          else
            Parameter := FSynth.SelectedSlot.Patch.GetPatchKnobParameter(KnobIndex);

          if assigned(Parameter) then
            (FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).MessAssignMidiCC(
              Parameter.Location,
              Parameter.ModuleIndex,
              Parameter.ParamIndex, Value);
        end;
        FframeAppSettings.SaveSettings;
      end;
  end;
end;

function TframeKnobs.GetButton(aIndex: Integer): TG2BtnFlat;
begin
  case aIndex of
    0:
      Result := bfButton1;
    1:
      Result := bfButton2;
    2:
      Result := bfButton3;
    3:
      Result := bfButton4;
    4:
      Result := bfButton5;
    5:
      Result := bfButton6;
    6:
      Result := bfButton7;
    7:
      Result := bfButton8;
  else
    raise Exception.Create('Button index out of range');
  end;
end;

function TframeKnobs.GetDisplayA(aIndex: Integer): TG2TextField;
begin
  case aIndex of
    0:
      Result := tfDisplayA1;
    1:
      Result := tfDisplayA2;
    2:
      Result := tfDisplayA3;
    3:
      Result := tfDisplayA4;
    4:
      Result := tfDisplayA5;
    5:
      Result := tfDisplayA6;
    6:
      Result := tfDisplayA7;
    7:
      Result := tfDisplayA8;
  else
    raise Exception.Create('DisplayA index out of range');
  end;
end;

function TframeKnobs.GetDisplayB(aIndex: Integer): TG2TextField;
begin
  case aIndex of
    0:
      Result := tfDisplayB1;
    1:
      Result := tfDisplayB2;
    2:
      Result := tfDisplayB3;
    3:
      Result := tfDisplayB4;
    4:
      Result := tfDisplayB5;
    5:
      Result := tfDisplayB6;
    6:
      Result := tfDisplayB7;
    7:
      Result := tfDisplayB8;
  else
    raise Exception.Create('DisplayB index out of range');
  end;
end;

function TframeKnobs.GetDisplayC(aIndex: Integer): TG2TextField;
begin
  case aIndex of
    0:
      Result := tfDisplayC1;
    1:
      Result := tfDisplayC2;
    2:
      Result := tfDisplayC3;
    3:
      Result := tfDisplayC4;
    4:
      Result := tfDisplayC5;
    5:
      Result := tfDisplayC6;
    6:
      Result := tfDisplayC7;
    7:
      Result := tfDisplayC8;
  else
    raise Exception.Create('DisplayC index out of range');
  end;
end;

function TframeKnobs.GetDisplayD(aIndex: Integer): TG2TextField;
begin
  case aIndex of
    0:
      Result := tfDisplayD1;
    1:
      Result := tfDisplayD2;
    2:
      Result := tfDisplayD3;
    3:
      Result := tfDisplayD4;
    4:
      Result := tfDisplayD5;
    5:
      Result := tfDisplayD6;
    6:
      Result := tfDisplayD7;
    7:
      Result := tfDisplayD8;
  else
    raise Exception.Create('DisplayD index out of range');
  end;
end;

function TframeKnobs.GetKnob(aIndex: Integer): TG2Knob;
begin
  case aIndex of
    0:
      Result := kKnob1;
    1:
      Result := kKnob2;
    2:
      Result := kKnob3;
    3:
      Result := kKnob4;
    4:
      Result := kKnob5;
    5:
      Result := kKnob6;
    6:
      Result := kKnob7;
    7:
      Result := kKnob8;
  else
    raise Exception.Create('Knob index out of range');
  end;
end;

function TframeKnobs.GetKnobIndexOffset: Integer;
begin
  Result := rbPageIndex.Value * 8 + rbPage.Value * 8 * 3;
end;

procedure TframeKnobs.rbPageChangeValue(Sender: TObject; const aValue: Integer);
begin
  if assigned(FSynth) then
    case rbSource.Value of
      0:
        FSynth.SelectedSlot.Patch.SelectedParamPage := aValue;
      1:
        FSynth.Performance.SelectedParamPage := aValue;
    end;
  UpdateControls;
end;

procedure TframeKnobs.rbPageIndexChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if assigned(FSynth) then
    case rbSource.Value of
      0:
        FSynth.SelectedSlot.Patch.SelectedParamPageColumn := aValue;
      1:
        FSynth.Performance.SelectedParamPageColumn := aValue;
    end;
  UpdateControls;
end;

procedure TframeKnobs.rbSourceChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateControls;
end;

procedure TframeKnobs.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeKnobs.SetEditMidiCC(aIndex: Integer; aValue: string;
  aVisible: boolean);
begin
  case aIndex of
    0:
      begin
        eMidiCC1.Text := aValue;
        eMidiCC1.Visible := aVisible;
      end;
    1:
      begin
        eMidiCC2.Text := aValue;
        eMidiCC2.Visible := aVisible;
      end;
    2:
      begin
        eMidiCC3.Text := aValue;
        eMidiCC3.Visible := aVisible;
      end;
    3:
      begin
        eMidiCC4.Text := aValue;
        eMidiCC4.Visible := aVisible;
      end;
    4:
      begin
        eMidiCC5.Text := aValue;
        eMidiCC5.Visible := aVisible;
      end;
    5:
      begin
        eMidiCC6.Text := aValue;
        eMidiCC6.Visible := aVisible;
      end;
    6:
      begin
        eMidiCC7.Text := aValue;
        eMidiCC7.Visible := aVisible;
      end;
    7:
      begin
        eMidiCC8.Text := aValue;
        eMidiCC8.Visible := aVisible;
      end;
    8:
      begin
        eMidiCC9.Text := aValue;
        eMidiCC9.Visible := aVisible;
      end;
    9:
      begin
        eMidiCC10.Text := aValue;
        eMidiCC10.Visible := aVisible;
      end;
    10:
      begin
        eMidiCC11.Text := aValue;
        eMidiCC11.Visible := aVisible;
      end;
    11:
      begin
        eMidiCC12.Text := aValue;
        eMidiCC12.Visible := aVisible;
      end;
    12:
      begin
        eMidiCC13.Text := aValue;
        eMidiCC13.Visible := aVisible;
      end;
    13:
      begin
        eMidiCC14.Text := aValue;
        eMidiCC14.Visible := aVisible;
      end;
    14:
      begin
        eMidiCC15.Text := aValue;
        eMidiCC15.Visible := aVisible;
      end;
    15:
      begin
        eMidiCC16.Text := aValue;
        eMidiCC16.Visible := aVisible;
      end;
  end;
end;

procedure TframeKnobs.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(Self, aStateStyleList);
end;

procedure TframeKnobs.SetSynth(const Value: TG2GraphFMX);
var
  i: Integer;
begin
  if FSynth <> Value then
  begin
    if assigned(FSynth) then
    begin
      for i := 0 to 3 do
      begin
        FSynth.GetSlot(i).Patch.RemoveObserver(Self);
        FSynth.GetSlot(i).RemoveObserver(Self);
        FSynth.Performance.RemoveObserver(Self);
      end;
    end;
    FSynth := Value;
    if assigned(FSynth) then
    begin
      for i := 0 to 3 do
      begin
        FSynth.GetSlot(i).Patch.RegisterObserver(Self);
        FSynth.GetSlot(i).RegisterObserver(Self);
        FSynth.Performance.RegisterObserver(Self);
      end;
    end;
  end;
end;

procedure TframeKnobs.Update(aG2Event: TG2Event);
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
        UpdateControls;
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
      begin
        UpdateControls;
      end;
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

procedure TframeKnobs.tfDisplayAGetTextFunc(Sender: TObject;
  var aTextFunc: string);
var
  TextField: TG2TextField;
  KnobIndex: Integer;
  Knob: TKnob;
  GlobalKnob: TGlobalKnob;
  Module: TG2FileModule;
begin
  aTextFunc := '';

  if not(Sender is TG2TextField) then
    Exit;

  TextField := Sender as TG2TextField;
  if not assigned(FSynth) then
    Exit;

  case rbSource.Value of
    0:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        Knob := FSynth.SelectedSlot.Patch.GetKnob(KnobIndex);
        if assigned(Knob) and (Knob.IsAssigned = 1) then
        begin
          aTextFunc := FSynth.SelectedSlot.Patch.GetModuleLabel(
            TLocationType(Knob.Location), Knob.ModuleIndex);
        end;
      end;

    1:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        GlobalKnob := FSynth.Performance.GetGlobalKnob(KnobIndex);
        if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then
        begin
          case GlobalKnob.SlotIndex of
            0:
              aTextFunc := 'A:';
            1:
              aTextFunc := 'B:';
            2:
              aTextFunc := 'C:';
            3:
              aTextFunc := 'D:';
          end;
          aTextFunc := aTextFunc + FSynth.Performance.Slot[GlobalKnob.SlotIndex].Patch.GetModuleLabel(
            TLocationType(GlobalKnob.Location),
            GlobalKnob.ModuleIndex);
        end;
      end;

    2:
      begin
        aTextFunc := STD_MORPH_NAMES[TextField.Tag];
      end;

    3:
      begin
        Module := nil;
        case TextField.Tag of
          0:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_MASTERCLOCK];
          1:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOICES];
          2:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR];
          3:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR];
          4:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VIBRATO];
          5:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_GLIDE];
          6:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_BEND];
          7:
            Module := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOLUME];
        end;

        if assigned(Module) then
          aTextFunc := Module.ModuleName;
      end;
  end;
end;

procedure TframeKnobs.tfDisplayCGetTextFunc(Sender: TObject;
  var aTextFunc: string);
var
  TextField: TG2TextField;
  KnobIndex: Integer;
  Knob: TKnob;
  GlobalKnob: TGlobalKnob;
  Param: TG2FileParameter;
begin
  aTextFunc := '';
  if not(Sender is TG2TextField) then
    Exit;

  TextField := Sender as TG2TextField;
  if not assigned(FSynth) then
    Exit;

  case rbSource.Value of
    0:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        Knob := FSynth.SelectedSlot.Patch.GetKnob(KnobIndex);
        if assigned(Knob) and (Knob.IsAssigned = 1) then
        begin
          Param := Knob.Parameter;
          if assigned(Param.ButtonParam) then
          begin
            Param := Param.ButtonParam;
            if Param.LabelOnValue then
              aTextFunc := Param.GetParamLabel(Param.GetValue)
            else
              aTextFunc := Param.GetParamLabel(0);
            if aTextFunc = '' then
              aTextFunc := Param.ParamName;
          end;
        end;
      end;

    1:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        GlobalKnob := FSynth.Performance.GetGlobalKnob(KnobIndex);
        if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then
        begin
          Param := GlobalKnob.Parameter;
          if assigned(Param.ButtonParam) then
          begin
            Param := Param.ButtonParam;
            if Param.LabelOnValue then
              aTextFunc := Param.GetParamLabel(Param.GetValue)
            else
              aTextFunc := Param.GetParamLabel(0);
            if aTextFunc = '' then
              aTextFunc := Param.ParamName;
          end;
        end;
      end;

    2:
      begin
        aTextFunc := STD_MORPH_NAMES[TextField.Tag];
      end;

    3:
      begin
        Param := nil;
        case TextField.Tag of
          0:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_MASTERCLOCK].Parameter[1];
          1:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOICES].Parameter[1];
          2:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR].Parameter[ARP_ON_OFF];
          3:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR].Parameter[ARP_OCTAVES];
          4:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VIBRATO].Parameter[VIBRATO_MOD];
          5:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_GLIDE].Parameter[GLIDE_TYPE];
          6:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch), PATCH_BEND]
              .Parameter[BEND_ON_OFF];
          7:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOLUME].Parameter[VOLUME_MUTE];
        end;

        if assigned(Param) then
          aTextFunc := Param.ModuleName;
      end;
  end;
end;

procedure TframeKnobs.tfDisplayDGetTextFunc(Sender: TObject;
  var aTextFunc: string);
var
  TextField: TG2TextField;
  KnobIndex: Integer;
  Knob: TKnob;
  GlobalKnob: TGlobalKnob;
  Patch: TG2FilePatch;
  Param: TG2FileParameter;
begin
  aTextFunc := '';
  if not(Sender is TG2TextField) then
    Exit;

  TextField := Sender as TG2TextField;
  if not assigned(FSynth) then
    Exit;

  Patch := FSynth.SelectedSlot.Patch;

  case rbSource.Value of
    0:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        Knob := Patch.GetKnob(KnobIndex);
        if assigned(Knob) and (Knob.IsAssigned = 1) then
        begin
          Param := Knob.Parameter;
          if Param.LabelOnValue then
            aTextFunc := Param.GetParamLabel(Param.GetValue)
          else
            aTextFunc := Param.GetParamLabel(0);
          if aTextFunc = '' then
            aTextFunc := Param.ParamName;
        end;
      end;

    1:
      begin
        KnobIndex := GetKnobIndexOffset + TextField.Tag;
        GlobalKnob := FSynth.Performance.GetGlobalKnob(KnobIndex);
        if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then
        begin
          Param := GlobalKnob.Parameter;
          if Param.LabelOnValue then
            aTextFunc := Param.GetParamLabel(Param.GetValue)
          else
            aTextFunc := Param.GetParamLabel(0);
          if aTextFunc = '' then
            aTextFunc := Param.ParamName;
        end;
      end;

    2:
      begin
        // aTextFunc := STD_MORPH_NAMES[TextField.Tag];
        aTextFunc := Patch.PatchPart[ord(ltPatch)
          ].ParameterLabelList.FindParamLabel(PATCH_MORPH,
          TextField.Tag + 8, 0);
      end;

    3:
      begin
        Param := nil;
        case TextField.Tag of
          0:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_MASTERCLOCK].Parameter[0];
          1:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOICES].Parameter[0];
          2:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR].Parameter[ARP_SPEED];
          3:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_ARPEGGIATOR].Parameter[ARP_DIRECTION];
          4:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VIBRATO].Parameter[VIBRATO_DEPTH];
          5:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_GLIDE].Parameter[GLIDE_SPEED];
          6:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_BEND].Parameter[BEND_RANGE];
          7:
            Param := FSynth.SelectedSlot.Patch.Modules[ord(ltPatch),
              PATCH_VOLUME].Parameter[VOLUME_LEVEL];
        end;

        if assigned(Param) then
          aTextFunc := Param.ParamName;
      end;
  end;
end;

procedure TframeKnobs.UpdateControls;
var
  i: Integer;
  KnobOffset: Integer;
  MidiCC: Integer;
  Visible: boolean;
begin
  KnobOffset := 0;
  case rbSource.Value of
    0:
      begin
        if assigned(FSynth) then
        begin
          rbPage.Value := FSynth.SelectedSlot.Patch.SelectedParamPage;
          rbPageIndex.Value :=
            FSynth.SelectedSlot.Patch.SelectedParamPageColumn;
        end;
        AssignKnobs;
        KnobOffset := GetKnobIndexOffset;
      end;

    1:
      begin
        if assigned(FSynth) then
        begin
          rbPage.Value := FSynth.Performance.SelectedParamPage;
          rbPageIndex.Value := FSynth.Performance.SelectedParamPageColumn;
        end;
        AssignGlobalKnobs;
        KnobOffset := GetKnobIndexOffset;
      end;

    2:
      begin
        AssignMorphParameters;
        KnobOffset := 0;
      end;

    3:
      begin
        AssignPatchParams;
        KnobOffset := 0;
      end;

    else
      raise Exception.Create('Source index out of range');
  end;

  for i := 0 to 7 do
  begin
    DisplayA[i].Redraw;
    DisplayC[i].Redraw;
    Visible := not((rbSource.Value = 3) and (i in [0, 1, 7]));

    // No assignments allowed for clock, voices and volume level
    MidiCC := FframeAppSettings.FindMidiToKnob(FSynth,
      TG2ParamSource(rbSource.Value), g2ctKnob, KnobOffset + i);
    if MidiCC = 0 then
      SetEditMidiCC(i, '', Visible)
    else
      SetEditMidiCC(i, IntToStr(MidiCC), Visible);
    Visible := not((rbSource.Value = 3) and (i in [0, 1]));

    // No assignments allowed for clock, voices
    MidiCC := FframeAppSettings.FindMidiToKnob(FSynth,
      TG2ParamSource(rbSource.Value), g2ctButton, KnobOffset + i);
    if MidiCC = 0 then
      SetEditMidiCC(i + 8, '', Visible)
    else
      SetEditMidiCC(i + 8, IntToStr(MidiCC), Visible);
  end;
end;

end.
