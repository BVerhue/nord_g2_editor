unit UnitAppSettings;

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
  System.Rtti, System.Classes,
  System.Variants,
  System.Generics.Collections,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Controls.Presentation,
  FMX.TabControl,
  XML.xmldom,
  XML.XMLDoc,
  XML.XMLIntf,
  BVE.NMG2Types,
  BVE.NMG2GraphFMX,
  BVE.NMG2File,
  BVE.NMG2ControlsFMX,
  BVE.NMG2AppSettings,
  UnitAppSynthSettings;

type
  TframeAppSettings = class(TFrame)
    eZoom1: TEdit;
    eZoom2: TEdit;
    eZoom3: TEdit;
    bfKnobControl: TG2BtnFlat;
    bfCableStyle: TG2BtnFlat;
    bfSplitterVisible: TG2BtnFlat;
    G2Label4: TG2Label;
    G2Label5: TG2Label;
    G2Label7: TG2Label;
    G2Label8: TG2Label;
    G2Label9: TG2Label;
    G2Label10: TG2Label;
    Panel2: TPanel;
    G2Label12: TG2Label;
    ePatchDir: TEdit;
    G2Label6: TG2Label;
    btInitDefaults: TG2BtnText;
    tcSynthSettings: TTabControl;
    Panel1: TPanel;
    G2Label1: TG2Label;
    bfAutoAssignMidi: TG2BtnFlat;
    Panel3: TPanel;
    lblWeblink: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure eZoom1Exit(Sender: TObject);
    procedure eZoom2Exit(Sender: TObject);
    procedure eZoom3Exit(Sender: TObject);
    procedure ePatchDirExit(Sender: TObject);
    procedure bfKnobControlChangeValue(Sender: TObject; const aValue: Integer);
    procedure bfCableStyleChangeValue(Sender: TObject; const aValue: Integer);
    procedure bfSplitterVisibleChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btInitDefaultsChangeValue(Sender: TObject; const aValue: Integer);
    procedure bfAutoAssignMidiChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure lblWeblinkClick(Sender: TObject);
    procedure lblWeblinkMouseEnter(Sender: TObject);
    procedure lblWeblinkMouseLeave(Sender: TObject);
  private
    FZoom1Str: string;
    FZoom2Str: string;
    FZoom3Str: string;
    FKnobControl: TKnobControl;
    FCableStyle: TCableStyle;
    FPatchDir: string;
    FSplitterVisible: Integer;
    FAutoAssignMidi: Integer;
    [Weak] FStateStyleList: TG2StateStyleList;
    FframeAppSynthSettingsList: TObjectList<TframeAppSynthSettings>;
    FOnKnobControlChange: TNotifyEvent;

    procedure SetPatchDir(const Value: string);
    function GetZoomSetting1: single;
    function GetZoomSetting2: single;
    function GetZoomSetting3: single;
    procedure SetKnobControl(const Value: TKnobControl);
    procedure SetCableStyle(const Value: TCableStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitDefaults;
    procedure AddSynth(aID: string; aSynth: TG2GraphFMX);
    function FindOrAddframeAppSynthSettings(aID, aHost: string; aPort: Integer;
      aUSBCOnnection: boolean; aSynth: TG2GraphFMX): TframeAppSynthSettings;
    procedure UpdateControls;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure AddMidiToKnob(aSynth: TG2GraphFMX; aSource: TG2ParamSource;
      aControlType: TG2ControlType; aKnob: Integer; aMidiCC: byte);
    procedure DeleteMidiToKnob(aSynth: TG2GraphFMX; aSource: TG2ParamSource;
      aControlType: TG2ControlType; aKnob: Integer);
    function FindMidiToKnob(aSynth: TG2GraphFMX; aSource: TG2ParamSource;
      aControlType: TG2ControlType; aKnob: Integer): Integer;
    function MidiToKnobCheckCC(aSynth: TG2GraphFMX; aMidiCC: byte): boolean;
    procedure AutoAssignMidiToKnobs(aSynth: TG2GraphFMX; aSlotIndex: Integer);
    procedure SynthSettingsChange(Sender: TObject);
    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);
    property PatchDir: string read FPatchDir write SetPatchDir;
    property ZoomSetting1: single read GetZoomSetting1;
    property ZoomSetting2: single read GetZoomSetting2;
    property ZoomSetting3: single read GetZoomSetting3;
    property SplitterVisible: Integer read FSplitterVisible;
    property AutoAssignMidi: Integer read FAutoAssignMidi;
    property KnobControl: TKnobControl read FKnobControl write SetKnobControl;
    property CableStyle: TCableStyle read FCableStyle write SetCableStyle;
    property OnKnobControlChange: TNotifyEvent read FOnKnobControlChange
      write FOnKnobControlChange;
  end;

implementation

{$IFnDEF ANDROID}
uses
  UnitUtils;
{$ENDIF}

{$R *.fmx}

{ TframeAppSettings }

constructor TframeAppSettings.Create(AOwner: TComponent);
begin
  inherited;

  FframeAppSynthSettingsList := TObjectList<TframeAppSynthSettings>.Create(False);

  InitDefaults;
end;

destructor TframeAppSettings.Destroy;
begin
  FframeAppSynthSettingsList.Free;

  inherited;
end;

function TframeAppSettings.FindOrAddframeAppSynthSettings(aID, aHost: string;
  aPort: Integer; aUSBCOnnection: boolean; aSynth: TG2GraphFMX): TframeAppSynthSettings;
var
  i: Integer;
  ti: TTabItem;
begin
  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].ID = aID) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
  begin
    Result := FframeAppSynthSettingsList[i]
  end
  else
  begin
    ti := TTabItem.Create(tcSynthSettings);
    ti.Parent := tcSynthSettings;
    ti.Text := aID;

    tcSynthSettings.BeginUpdate;
    try
      tcSynthSettings.AddObject(ti);
      tcSynthSettings.ActiveTab := ti;
    finally
      tcSynthSettings.EndUpdate;
    end;

    Result := TframeAppSynthSettings.Create(self);
    Result.Name := 'frameAppSynthSettings' +
      IntToStr(FframeAppSynthSettingsList.Count);
    Result.Parent := ti;
    Result.Align := TAlignLayout.alCenter;
    ComponentSetStateStylesRecursive(Result, FStateStyleList);
    Result.ID := aID;
    Result.USBConnection := aUSBCOnnection;
    Result.Host := aHost;
    Result.Port := aPort;
    Result.OnChange := SynthSettingsChange;

    FframeAppSynthSettingsList.Add(Result);
  end;
end;

procedure TframeAppSettings.AddMidiToKnob(aSynth: TG2GraphFMX;
  aSource: TG2ParamSource; aControlType: TG2ControlType; aKnob: Integer;
  aMidiCC: Byte);
var
  i: Integer;
begin
  if not CheckCCAllowed(aMidiCC) then
    Exit;

  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].Synth = aSynth) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
  begin
    FframeAppSynthSettingsList[i].Synth.AddMidiToKnob(aSource, aControlType,
      aKnob, aMidiCC);
    SaveSettings;
  end;
end;

procedure TframeAppSettings.DeleteMidiToKnob(aSynth: TG2GraphFMX;
  aSource: TG2ParamSource; aControlType: TG2ControlType; aKnob: Integer);
var
  i: Integer;
begin
  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].Synth = aSynth) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
  begin
    FframeAppSynthSettingsList[i].Synth.DeleteMidiToKnob(aSource,
      aControlType, aKnob);
    SaveSettings;
  end;
end;

function TframeAppSettings.FindMidiToKnob(aSynth: TG2GraphFMX;
  aSource: TG2ParamSource; aControlType: TG2ControlType;
  aKnob: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;

  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].Synth = aSynth) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
    Result := FframeAppSynthSettingsList[i].Synth.FindMidiToKnob(aSource,
      aControlType, aKnob);
end;

function TframeAppSettings.MidiToKnobCheckCC(aSynth: TG2GraphFMX;
  aMidiCC: Byte): Boolean;
var
  i: Integer;
begin
  // Check if already in use
  Result := False;

  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].Synth = aSynth) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
    Result := FframeAppSynthSettingsList[i].Synth.MidiToKnobCheckCC(aMidiCC);
end;

procedure TframeAppSettings.AutoAssignMidiToKnobs(aSynth: TG2GraphFMX;
  aSlotIndex: Integer);
var
  Patch: TG2GraphPatchFMX;
  Knob: TKnob;
  GlobalKnob: TGlobalKnob;
  Parameter: TG2FileParameter;
  MidiCC: Byte;
  i: Integer;
begin
  case aSlotIndex of
    0, 1, 2, 3: // Slots
      begin
        Patch := aSynth.GetSlot(aSlotIndex).Patch as TG2GraphPatchFMX;

        for i := 0 to Patch.KnobList.Count - 1 do
        begin
          MidiCC := FindMidiToKnob(aSynth, g2psParam, g2ctKnob, i);
          if MidiCC <> 0 then
          begin
            Knob := Patch.KnobList[i];
            if assigned(Knob) and (Knob.IsAssigned = 1) and
              assigned(Knob.Parameter) then
            begin
              if (not assigned(Knob.Parameter.Controller)) or
                (Knob.Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Knob.Parameter.Location,
                  Knob.Parameter.ModuleIndex,
                  Knob.Parameter.ParamIndex, MidiCC);
            end;
          end;

          MidiCC := FindMidiToKnob(aSynth, g2psParam, g2ctButton, i);
          if MidiCC <> 0 then
          begin
            Knob := Patch.KnobList[i];
            if assigned(Knob) and (Knob.IsAssigned = 1) and
              assigned(Knob.Parameter) and assigned(Knob.Parameter.ButtonParam)
            then
            begin
              if (not assigned(Knob.Parameter.ButtonParam.Controller)) or
                (Knob.Parameter.ButtonParam.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Knob.Parameter.ButtonParam.Location,
                  Knob.Parameter.ButtonParam.ModuleIndex,
                  Knob.Parameter.ButtonParam.ParamIndex, MidiCC);
            end;
          end;
        end;

        for i := 0 to 7 do
        begin
          MidiCC := FindMidiToKnob(aSynth, g2psMorph, g2ctKnob, i);
          if MidiCC <> 0 then
          begin
            Parameter := aSynth.GetSlot(aSlotIndex).Patch.GetMorphKnobParameter(i);
            if assigned(Parameter) then
              if (not assigned(Parameter.Controller)) or
                (Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Parameter.Location,
                  Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
          end;

          MidiCC := FindMidiToKnob(aSynth, g2psMorph, g2ctButton, i);
          if MidiCC <> 0 then
          begin
            Parameter := aSynth.GetSlot(aSlotIndex).Patch.GetMorphKnobParameter(i + 8);
            if assigned(Parameter) then
              if (not assigned(Parameter.Controller)) or
                (Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Parameter.Location,
                  Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
          end;
        end;

        for i := 0 to 7 do
        begin
          MidiCC := FindMidiToKnob(aSynth, g2psPatch, g2ctKnob, i);
          if MidiCC <> 0 then
          begin
            Parameter := aSynth.GetSlot(aSlotIndex).Patch.GetPatchKnobParameter(i);
            if assigned(Parameter) and (Parameter.MidiAssignmentsAllowed) then
              if (not assigned(Parameter.Controller)) or
                (Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Parameter.Location,
                  Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
          end;

          MidiCC := FindMidiToKnob(aSynth, g2psPatch, g2ctButton, i);
          if MidiCC <> 0 then
          begin
            Parameter := aSynth.GetSlot(aSlotIndex)
              .Patch.GetPatchKnobParameter(i + 8);
            if assigned(Parameter) and (Parameter.MidiAssignmentsAllowed) then
              if (not assigned(Parameter.Controller)) or
                (Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(Parameter.Location,
                  Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
          end;
        end;
      end;

    4: // Performance
      begin
        for i := 0 to aSynth.Performance.GlobalKnobList.Count - 1 do
        begin
          MidiCC := FindMidiToKnob(aSynth, g2psGlobal, g2ctKnob, i);
          if MidiCC <> 0 then
          begin
            GlobalKnob := aSynth.Performance.GlobalKnobList[i];
            if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) and
              assigned(GlobalKnob.Parameter) then
            begin
              Patch := aSynth.GetSlot(GlobalKnob.SlotIndex).Patch as TG2GraphPatchFMX;
              if (not assigned(GlobalKnob.Parameter.Controller)) or
                (GlobalKnob.Parameter.Controller.MidiCC <> MidiCC) then
                Patch.MessAssignMidiCC(GlobalKnob.Parameter.Location,
                  GlobalKnob.Parameter.ModuleIndex,
                  GlobalKnob.Parameter.ParamIndex, MidiCC);
            end;
          end;
        end;
      end;
  end;
end;

procedure TframeAppSettings.AddSynth(aID: string; aSynth: TG2GraphFMX);
var
  i, j: Integer;
  ti: TTabItem;
  frameAppSynthSettings: TframeAppSynthSettings;
begin
  if not assigned(aSynth) then
    Exit;

  i := 0;
  while (i < FframeAppSynthSettingsList.Count) and
    not(FframeAppSynthSettingsList[i].ID = aID) do
    inc(i);

  if (i < FframeAppSynthSettingsList.Count) then
  begin
    FframeAppSynthSettingsList[i].Synth := aSynth;
    aSynth.SynthName := aSynth.SynthName;
    for j := 0 to 3 do
      (aSynth.GetSlot(j).Patch as TG2GraphPatchFMX).SplitterVisible :=
        FSplitterVisible = 1;
  end
  else
  begin
    ti := TTabItem.Create(tcSynthSettings);
    ti.Parent := tcSynthSettings;
    ti.Text := aID;

    tcSynthSettings.BeginUpdate;
    try
      tcSynthSettings.AddObject(ti);
      tcSynthSettings.ActiveTab := ti;
    finally
      tcSynthSettings.EndUpdate;
    end;

    frameAppSynthSettings := TframeAppSynthSettings.Create(self);
    frameAppSynthSettings.Name := 'frameAppSynthSettings' +
      IntToStr(FframeAppSynthSettingsList.Count);
    frameAppSynthSettings.Parent := ti;
    frameAppSynthSettings.Align := TAlignLayout.alCenter;
    ComponentSetStateStylesRecursive(frameAppSynthSettings, FStateStyleList);
    frameAppSynthSettings.ID := aID;
    frameAppSynthSettings.Synth := aSynth;
    frameAppSynthSettings.SynthName := aSynth.SynthName;
    frameAppSynthSettings.OnChange := SynthSettingsChange;
    FframeAppSynthSettingsList.Add(frameAppSynthSettings);
  end;
end;

procedure TframeAppSettings.bfAutoAssignMidiChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FAutoAssignMidi := aValue;
  SaveSettings;
end;

procedure TframeAppSettings.bfCableStyleChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  CableStyle := TCableStyle(aValue);
  SaveSettings;
end;

procedure TframeAppSettings.bfKnobControlChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  KnobControl := TKnobControl(aValue);
  SaveSettings;
  if assigned(FOnKnobControlChange) then
    FOnKnobControlChange(self);
end;

procedure TframeAppSettings.bfSplitterVisibleChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSplitterVisible := aValue;
  SaveSettings;
end;

procedure TframeAppSettings.btInitDefaultsChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
  begin
    InitDefaults;
    SaveSettings;
  end;
end;

procedure TframeAppSettings.ePatchDirExit(Sender: TObject);
begin
  FPatchDir := ePatchDir.Text;
  SaveSettings;
end;

procedure TframeAppSettings.eZoom1Exit(Sender: TObject);
var
  Value: single;
  Code: Integer;
begin
  val(eZoom1.Text, Value, Code);
  if Code <> 0 then
    raise Exception.Create('Must enter a number.');
  FZoom1Str := eZoom1.Text;
  SaveSettings;
end;

procedure TframeAppSettings.eZoom2Exit(Sender: TObject);
var
  Value: single;
  Code: Integer;
begin
  val(eZoom2.Text, Value, Code);
  if Code <> 0 then
    raise Exception.Create('Must enter a number.');
  FZoom2Str := eZoom2.Text;
  SaveSettings;
end;

procedure TframeAppSettings.eZoom3Exit(Sender: TObject);
var
  Value: single;
  Code: Integer;
begin
  val(eZoom3.Text, Value, Code);
  if Code <> 0 then
    raise Exception.Create('Must enter a number.');
  FZoom3Str := eZoom3.Text;
  SaveSettings;
end;

function TframeAppSettings.GetZoomSetting1: single;
var
  Code: Integer;
begin
  val(FZoom1Str, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Invalid zoom setting 1.');
end;

function TframeAppSettings.GetZoomSetting2: single;
var
  Code: Integer;
begin
  val(FZoom2Str, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Invalid zoom setting 2.');
end;

function TframeAppSettings.GetZoomSetting3: single;
var
  Code: Integer;
begin
  val(FZoom3Str, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Invalid zoom setting 3.');
end;

procedure TframeAppSettings.InitDefaults;
var
  i: Integer;
begin
  FZoom1Str := '1';
  FZoom2Str := '1.5';
  FZoom3Str := '2';
  KnobControl := kcCircular;
  CableStyle := csFlat;
  FPatchDir := '';
  FSplitterVisible := 1;
  { for i := 0 to FframeAppSynthSettingsList.Count - 1 do
    FframeAppSynthSettingsList[i].Free;
    FframeAppSynthSettingsList.Clear;
    while tcSynthSettings.TabCount > 0 do
    tcSynthSettings.Tabs[0].Free; }
  UpdateControls;
end;

procedure TframeAppSettings.lblWeblinkClick(Sender: TObject);
begin
  {$IFnDEF ANDROID}
  TUtils.Open('http://www.bverhue.nl/g2dev');
  {$ENDIF}
end;

procedure TframeAppSettings.lblWeblinkMouseEnter(Sender: TObject);
begin
  lblWeblink.Font.Style := [TFontStyle.fsUnderline];
end;

procedure TframeAppSettings.lblWeblinkMouseLeave(Sender: TObject);
begin
  lblWeblink.Font.Style := [];
end;

procedure TframeAppSettings.LoadSettings;
var
  XMLAppSettings: TXMLDocument;
  XMLAppSettingsType: IXMLAppSettingsType;
  frameAppSynthSettings: TframeAppSynthSettings;
  i, j: Integer;
  Source: TG2ParamSource;
  SynthName, path: string;
  Synth: TG2GraphFMX;
begin
  path := '';
  {$IFDEF MSWINDOWS}
  path := '';
  {$ELSE}
  path := TPath.GetDocumentsPath + PathDelim;
  {$ENDIF}

  if not FileExists(path + 'g2editorfmx.xml') then
  begin
    InitDefaults;
    Exit;
  end;

  XMLAppSettings := TXMLDocument.Create(self);
  try
    XMLAppSettings.FileName := path + 'g2editorfmx.xml';
    XMLAppSettings.Active := True;
    try
      XMLAppSettingsType := GetAppSettings(XMLAppSettings);
      KnobControl := TKnobControl(XMLAppSettingsType.General.KnobControl);
      CableStyle := TCableStyle(XMLAppSettingsType.General.CableStyle);
      FPatchDir := XMLAppSettingsType.General.PatchDir;
      FSplitterVisible := XMLAppSettingsType.General.SplitterVisible;
      FZoom1Str := XMLAppSettingsType.General.Zoom1;
      FZoom2Str := XMLAppSettingsType.General.Zoom2;
      FZoom3Str := XMLAppSettingsType.General.Zoom3;
      FAutoAssignMidi := XMLAppSettingsType.General.AutoAssignMidiToKnob;

      for i := 0 to XMLAppSettingsType.G2Synths.Count - 1 do
      begin
        SynthName := XMLAppSettingsType.G2Synths[i].Name;
        frameAppSynthSettings := FindOrAddframeAppSynthSettings(
          'Synth' + IntToStr(i + 1),
          XMLAppSettingsType.G2Synths[i].Host,
          XMLAppSettingsType.G2Synths[i].Port,
          XMLAppSettingsType.G2Synths[i].USBConnection,
          nil);

        for j := 0 to XMLAppSettingsType.G2Synths[i].MidiToKnobs.Count - 1 do
        begin
          Source := g2psParam;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Params' then
            Source := g2psParam;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Global' then
            Source := g2psGlobal;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Morphs' then
            Source := g2psMorph;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Patch' then
            Source := g2psPatch;

          Synth := frameAppSynthSettings.Synth;
          if assigned(Synth) then
          begin
            Synth.AutoAssignMidi := FAutoAssignMidi = 1;
            Synth.AddMidiToKnob(
              Source,
              TG2ControlType(XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].ControlType),
              XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Knob,
              XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].MidiCC);
          end;
        end;
      end;
    finally
      XMLAppSettings.Active := False;
    end;
  finally
    XMLAppSettings.Free;
  end;
  UpdateControls;
end;

procedure TframeAppSettings.SaveSettings;
var
  XMLAppSettings: TXMLDocument;
  XMLAppSettingsType: IXMLAppSettingsType;
  XMLG2SynthType: IXMLG2SynthType;
  XMLMidiToKnobType: IXMLMidiToKnobType;
  i, j: Integer;
  path: string;
  Synth: TG2GraphFMX;
begin
  path := '';
  {$IFDEF MSWINDOWS}
  path := '';
  {$ELSE}
  path := TPath.GetDocumentsPath + PathDelim;
  {$ENDIF}

  XMLAppSettingsType := NewAppSettings;
  XMLAppSettingsType.General.KnobControl := Integer(FKnobControl);
  XMLAppSettingsType.General.CableStyle := Integer(FCableStyle);
  XMLAppSettingsType.General.PatchDir := FPatchDir;
  XMLAppSettingsType.General.SplitterVisible := FSplitterVisible;
  XMLAppSettingsType.General.Zoom1 := FZoom1Str;
  XMLAppSettingsType.General.Zoom2 := FZoom2Str;
  XMLAppSettingsType.General.Zoom3 := FZoom3Str;
  XMLAppSettingsType.General.AutoAssignMidiToKnob := FAutoAssignMidi;

  for i := 0 to FframeAppSynthSettingsList.Count - 1 do
  begin
    XMLG2SynthType := XMLAppSettingsType.G2Synths.Add;
    XMLG2SynthType.Name := FframeAppSynthSettingsList[i].ID;
    XMLG2SynthType.USBConnection := FframeAppSynthSettingsList[i].USBConnection;
    XMLG2SynthType.Host := FframeAppSynthSettingsList[i].Host;
    XMLG2SynthType.Port := FframeAppSynthSettingsList[i].Port;

    Synth := FframeAppSynthSettingsList[i].Synth;
    if assigned(Synth) then
    begin
      for j := 0 to Synth.MidiToKnobList.Count - 1 do
      begin

        XMLMidiToKnobType := XMLG2SynthType.MidiToKnobs.Add;
        case Synth.MidiToKnobList[j].FSource of
          g2psParam:
            XMLMidiToKnobType.Source := 'Params';
          g2psGlobal:
            XMLMidiToKnobType.Source := 'Global';
          g2psMorph:
            XMLMidiToKnobType.Source := 'Morphs';
          g2psPatch:
            XMLMidiToKnobType.Source := 'Patch';
        end;

        XMLMidiToKnobType.ControlType :=
          Integer(Synth.MidiToKnobList[j].FControlType);
        XMLMidiToKnobType.Knob := Synth.MidiToKnobList[j].FKnob;
        XMLMidiToKnobType.MidiCC := Synth.MidiToKnobList[j].FMidiCC;
      end;
    end;
  end;
  XMLAppSettingsType.OwnerDocument.SaveToFile(path + 'g2editorfmx.xml');
end;

procedure TframeAppSettings.SetCableStyle(const Value: TCableStyle);
var
  i: Integer;
begin
  FCableStyle := Value;
  for i := 0 to FframeAppSynthSettingsList.Count - 1 do
    if assigned(FframeAppSynthSettingsList[i].Synth) then
      FframeAppSynthSettingsList[i].Synth.CableStyle := Value;
end;

procedure TframeAppSettings.SetKnobControl(const Value: TKnobControl);
var
  i: Integer;
begin
  FKnobControl := Value;
  for i := 0 to FframeAppSynthSettingsList.Count - 1 do
    if assigned(FframeAppSynthSettingsList[i].Synth) then
      FframeAppSynthSettingsList[i].Synth.KnobControl := Value;
end;

procedure TframeAppSettings.SetPatchDir(const Value: string);
begin
  FPatchDir := Value;
end;

procedure TframeAppSettings.SetStateStyles(aStateStyleList: TG2StateStyleList);
var
  i: Integer;
begin
  FStateStyleList := aStateStyleList;
  ComponentSetStateStylesRecursive(self, aStateStyleList);
  for i := 0 to FframeAppSynthSettingsList.Count - 1 do
    ComponentSetStateStylesRecursive(FframeAppSynthSettingsList[i],
      aStateStyleList);
end;

procedure TframeAppSettings.SynthSettingsChange(Sender: TObject);
begin
  SaveSettings;
end;

procedure TframeAppSettings.UpdateControls;
var
  i: Integer;
begin
  bfKnobControl.Value := Integer(FKnobControl);
  bfCableStyle.Value := Integer(FCableStyle);
  ePatchDir.Text := FPatchDir;
  bfSplitterVisible.Value := FSplitterVisible;
  bfAutoAssignMidi.Value := FAutoAssignMidi;
  eZoom1.Text := FZoom1Str;
  eZoom2.Text := FZoom2Str;
  eZoom3.Text := FZoom3Str;
  for i := 0 to FframeAppSynthSettingsList.Count - 1 do
    FframeAppSynthSettingsList[i].UpdateControls;
end;

end.
