unit BVE.NMG2ComManager;

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
  System.Generics.Collections,
  System.UIConsts,
  System.IOUtils,
  BVE.NMG2Types,
  BVE.NMG2FileIntf,
  BVE.NMG2Object,
  BVE.NMG2Com,
  BVE.NMG2ComUSB;

type
  TG2ConnectionManager = class(TG2ObservedObject, IG2ConnectionManager)
  private
    FConnectionList: TList<IG2Connection>;

    FSelectedConnectionIndex: integer;
    FSelectedModuleType: byte;
    FSelectedRow: byte;
    FSelectedCol: byte;

    FZoomSetting1 : single;
    FZoomSetting2 : single;
    FZoomSetting3 : single;
    FKnobControl : TKnobControl;
    FCableStyle : TCableStyle;
    FPatchDir : string;
    FSplitterVisible : integer;
    FAutoAssignMidi : integer;

    function GetSelectedConnection: IG2Connection;
    function GetConnectionList: TList<IG2Connection>;
    function GetSelectedConnectionIndex: integer;
    procedure SetSelectedConnectionIndex(const Value: integer);
    function GetSelectedSlot: IG2Slot;
    function GetSelectedSlotIndex: integer;
    function GetSelectedPatch: IG2Patch;
    function GetSelectedLocationIndex: integer;
    procedure SetSelectedLocationIndex(const aValue: integer);
    function GetSelectedPatchPart: IG2PatchPart;
    function GetFocusedModule: IG2Module;
    function GetSelectedParam: IG2Param;

    function GetAutoAssignMidi: integer;
    function GetPatchDir: string;
    function GetCableStyle: TCableStyle;
    function GetKnobControl: TKnobControl;
    function GetSplitterVisible: integer;
    function GetZoomSetting1: single;
    function GetZoomSetting2: single;
    function GetZoomSetting3: single;
    procedure SetAutoAssignMidi(const aValue: integer);
    procedure SetPatchDir(const aValue: string);
    procedure SetCableStyle(const Value: TCableStyle);
    procedure SetKnobControl(const Value: TKnobControl);
    procedure SetSplitterVisible(const Value: integer);
    procedure SetZoomSetting1(const Value: single);
    procedure SetZoomSetting2(const Value: single);
    procedure SetZoomSetting3(const Value: single);
    function GetSelectedModuleType: byte;
    procedure SetSelectedModuleType(const Value: byte);
    function GetSelectedCol: byte;
    function GetSelectedRow: byte;
    procedure SetSelectedCol(const Value: byte);
    procedure SetSelectedRow(const Value: byte);
  public
    constructor Create;
    destructor Destroy; override;

    procedure DeviceDiscovery;
    procedure DisconnectAll;

    procedure InitDefaultSettings;
    procedure LoadSettings;
    procedure SaveSettings;

    procedure LoadFromFile(const aFileName: string);

    property ConnectionList : TList<IG2Connection> read GetConnectionList;
    property SelectedConnection: IG2Connection read GetSelectedConnection;
    property SelectedConnectionIndex: integer read GetSelectedConnectionIndex write SetSelectedConnectionIndex;
    property SelectedSlot: IG2Slot read GetSelectedSlot;
    property SelectedModuleType: byte read GetSelectedModuleType write SetSelectedModuleType;
    property SelectedRow: byte read GetSelectedRow write SetSelectedRow;
    property SelectedCol: byte read GetSelectedCol write SetSelectedCol;

    property PatchDir : string read GetPatchDir write SetPatchDir;
    property ZoomSetting1 : single read GetZoomSetting1 write SetZoomSetting1;
    property ZoomSetting2 : single read GetZoomSetting2 write SetZoomSetting2;
    property ZoomSetting3 : single read GetZoomSetting3 write SetZoomSetting3;
    property AutoAssignMidi : integer read GetAutoAssignMidi write SetAutoAssignMidi;
    property KnobControl : TKnobControl read GetKnobControl write SetKnobControl;
    property CableStyle : TCableStyle read GetCableStyle write SetCableStyle;
    property SplitterVisible : integer read GetSplitterVisible write SetSplitterVisible;
  end;

implementation
uses
  Xmldom,
  XMLDoc,
  XMLIntf,
{$IF Defined(MSWINDOWS)}
  BVE.NMG2USBWin,
{$ENDIF}
{$IF Defined(MACOS)}
  BVE.NMG2USBunix,
{$ENDIF}
{$IF Defined(Android)}
  BVE.NMG2USBAndroid,
{$ENDIF}
  BVE.NMG2AppSettings,
  BVE.NMG2Stream,
  BVE.NMG2Patch,
  BVE.NMG2Perf;

{ TG2ConnectionManager }

constructor TG2ConnectionManager.Create;
begin
  inherited Create;

  FConnectionList := TList<IG2Connection>.Create;
  FSelectedConnectionIndex := -1;
  InitDefaultSettings;
end;

destructor TG2ConnectionManager.Destroy;
var Connection: IG2Connection;
begin
  DisconnectAll;

  for Connection in FConnectionList do
    Connection.NotifyDestroy;
  FConnectionList.Free;

  inherited;
end;

procedure TG2ConnectionManager.DeviceDiscovery;
var Connection: IG2Connection;
begin
{$IF Defined(Android)}
  GetUSBDeviceList(self);
{$ELSE}
  GetUSBDeviceList(self);
{$ENDIF}
  if FConnectionList.Count = 0 then begin
    Connection := TG2USBConnection.Create(self);
    FConnectionList.Add(Connection);
  end;
end;

procedure TG2ConnectionManager.DisconnectAll;
var i : integer;
begin
  for i := 0 to FCOnnectionList.Count - 1 do
    FConnectionList[i].Disconnect;
end;

function TG2ConnectionManager.GetAutoAssignMidi: integer;
begin
  Result := FAutoAssignMidi;
end;

function TG2ConnectionManager.GetCableStyle: TCableStyle;
begin
  Result := FCableStyle;
end;

function TG2ConnectionManager.GetConnectionList: TList<IG2Connection>;
begin
  Result := FConnectionList;
end;

function TG2ConnectionManager.GetFocusedModule: IG2Module;
begin
  Result := SelectedConnection.FocusedModule;
end;

function TG2ConnectionManager.GetKnobControl: TKnobControl;
begin
  Result := FKnobControl;
end;

function TG2ConnectionManager.GetPatchDir: string;
begin
  Result := FPatchDir;
end;

function TG2ConnectionManager.GetSelectedCol: byte;
begin
  Result := FSelectedCol;
end;

function TG2ConnectionManager.GetSelectedConnection: IG2Connection;
begin
  if (FSelectedConnectionIndex >= 0) and (FSelectedConnectionIndex < FConnectionList.Count) then
    Result := FConnectionList[FSelectedConnectionIndex]
  else
    Result := nil;
end;

function TG2ConnectionManager.GetSelectedConnectionIndex: integer;
begin
  Result := FSelectedConnectionIndex;
end;

function TG2ConnectionManager.GetSelectedLocationIndex: integer;
begin
  Result := SelectedConnection.SelectedLocationIndex;
end;

function TG2ConnectionManager.GetSelectedModuleType: byte;
begin
  Result := FSelectedModuleType;
end;

function TG2ConnectionManager.GetSelectedParam: IG2Param;
begin
  Result := SelectedConnection.SelectedParam;
end;

function TG2ConnectionManager.GetSelectedPatch: IG2Patch;
begin
  Result := SelectedConnection.SelectedPatch;
end;

function TG2ConnectionManager.GetSelectedPatchPart: IG2PatchPart;
begin
  Result := SelectedConnection.SelectedPatchPart;
end;

function TG2ConnectionManager.GetSelectedRow: byte;
begin
  Result := FSelectedRow;
end;

function TG2ConnectionManager.GetSelectedSlot: IG2Slot;
begin
  if assigned(SelectedConnection) then
    Result := SelectedConnection.SelectedSlot
  else
    Result := nil;;
end;

function TG2ConnectionManager.GetSelectedSlotIndex: integer;
begin
  Result := SelectedConnection.SelectedSlotIndex;
end;

function TG2ConnectionManager.GetSplitterVisible: integer;
begin
  Result := FSplitterVisible;
end;

function TG2ConnectionManager.GetZoomSetting1: single;
begin
  Result := FZoomSetting1;
end;

function TG2ConnectionManager.GetZoomSetting2: single;
begin
  Result := FZoomSetting2;
end;

function TG2ConnectionManager.GetZoomSetting3: single;
begin
  Result := FZoomSetting3;
end;

procedure TG2ConnectionManager.InitDefaultSettings;
begin
  FZoomSetting1 := 1;
  FZoomSetting1 := 1.5;
  FZoomSetting1 := 2;
  KnobControl := kcCircular;
  CableStyle := csFlat;
  FPatchDir := '';
  FSplitterVisible := 1;
end;

procedure TG2ConnectionManager.LoadFromFile(const aFileName: string);
var G2FileDataStream: TG2FileDataStream;
    FileName, DataName: string;
    i: integer;
    FileStream: TFileStream;
    b: byte;
begin
  if not assigned(SelectedConnection) then
    exit;

  FileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    Filename := ExtractFilename(aFileName);

    // Name patch max size = 16, if shorter end with 0
    DataName := '';
    i := 1;
    while (i<=Length(FileName)) and (i<=16) and (FileName[i] <> '.') do begin
      DataName := DataName + Char(byte(FileName[i]));
      inc(i);
    end;

    // Check first byte
    FileStream.Read( b, 1);
    FileStream.Position := 0;
    case b of
      $56 : G2FileDataStream := TG2FileDataStream.LoadFileData(SelectedConnection, FileStream);
      $F0 : G2FileDataStream := TG2FileDataStream.LoadMidiData(SelectedConnection, FileStream);
      else
        raise Exception.Create('Unknown file data.');
    end;

    if G2FileDataStream is TG2Perf then begin
      SelectedConnection.SynthPerfLoad( DataName, G2FileDataStream as IG2Perf);
    end else
      if G2FileDataStream is TG2Patch then
        SelectedConnection.SlotPatchLoad(
          SelectedConnection.SelectedSlotIndex,
          DataName,
          G2FileDataStream as IG2Patch)
      else
        raise Exception.Create('Unknown data type');

  finally
    FileStream.Free;
  end;
end;

procedure TG2ConnectionManager.LoadSettings;
var XMLAppSettings : IXMLDocument;
    XMLAppSettingsType : IXMLAppSettingsType;
    //frameAppSynthSettings : TframeAppSynthSettings;
    //i, j : integer;
    //Source : TG2ParamSource;
    //ZoomStr, Code, SynthName,
    path : string;
begin
  path := '';
{$IF Defined(MSWINDOWS)}
  path := '';
{$ELSE}
  path := TPath.GetDocumentsPath + PathDelim;
{$ENDIF}

  if not FileExists(path + 'g2editorfmx.xml') then begin
    InitDefaultSettings;
    exit;
  end;

  XMLAppSettings := TXMLDocument.Create(nil);
  try
    XMLAppSettings.FileName := path + 'g2editorfmx.xml';

    XMLAppSettings.Active := True;
    try
      XMLAppSettingsType := GetAppSettings(XMLAppSettings);
      KnobControl := TKnobControl(XMLAppSettingsType.General.KnobControl);
      CableStyle := TCableStyle(XMLAppSettingsType.General.CableStyle);
      FPatchDir := XMLAppSettingsType.General.PatchDir;
      FSplitterVisible := XMLAppSettingsType.General.SplitterVisible;
      if not TryStrToFloat(XMLAppSettingsType.General.Zoom1, FZoomSetting1) then
        FZoomSetting1 := 1;
      if not TryStrToFloat(XMLAppSettingsType.General.Zoom2, FZoomSetting2) then
        FZoomSetting2 := 1.5;
      if not TryStrToFloat(XMLAppSettingsType.General.Zoom3, FZoomSetting3) then
        FZoomSetting3 := 2;
      FAutoAssignMidi := XMLAppSettingsType.General.AutoAssignMidiToKnob;

      {for i := 0 to XMLAppSettingsType.G2Synths.Count - 1 do begin
        SynthName := XMLAppSettingsType.G2Synths[i].Name;
        frameAppSynthSettings := FindOrAddframeAppSynthSettings( 'Synth' + IntToStr(i+1),
                                                                 XMLAppSettingsType.G2Synths[i].Host,
                                                                 XMLAppSettingsType.G2Synths[i].Port,
                                                                 XMLAppSettingsType.G2Synths[i].UsbConnection);
        for j := 0 to XMLAppSettingsType.G2Synths[i].MidiToKnobs.Count - 1 do begin
          {Source := g2psParam;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Params' then
            Source := g2psParam;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Global' then
            Source := g2psGlobal;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Morphs' then
            Source := g2psMorph;
          if XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Source = 'Patch' then
            Source := g2psPatch;}

          //frameAppSynthSettings.Synth.AutoAssignMidi := FAutoAssignMidi=1;
          {frameAppSynthSettings.AddMidiToKnob( Source,
              TG2ControlType(XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].ControlType),
              XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].Knob,
              XMLAppSettingsType.G2Synths[i].MidiToKnobs[j].MidiCC);}
       { end;
      end;}
    finally
      XMLAppSettings.Active := False;
    end;
  finally
    //XMLAppSettings.Free;
  end;
end;

procedure TG2ConnectionManager.SaveSettings;
var XMLAppSettingsType : IXMLAppSettingsType;
    //XMLAppSettings : TXMLDocument;
    //XMLG2SynthType : IXMLG2SynthType;
    //XMLMidiToKnobType : IXMLMidiToKnobType;
    //AppSynthSett : TframeAppSynthSettings;
    //i, j : integer;
    path : string;
begin
  path := '';
{$IF Defined(MSWINDOWS)}
  path := '';
{$ELSE}
  path := TPath.GetDocumentsPath + PathDelim;
{$ENDIF}

  XMLAppSettingsType := NewAppSettings;
  XMLAppSettingsType.General.KnobControl := integer(FKnobControl);
  XMLAppSettingsType.General.CableStyle := integer(FCableStyle);
  XMLAppSettingsType.General.PatchDir := FPatchDir;
  XMLAppSettingsType.General.SplitterVisible := FSplitterVisible;
  XMLAppSettingsType.General.Zoom1 := FloatToStr(FZoomSetting1);
  XMLAppSettingsType.General.Zoom2 := FloatToStr(FZoomSetting2);
  XMLAppSettingsType.General.Zoom3 := FloatToStr(FZoomSetting3);
  XMLAppSettingsType.General.AutoAssignMidiToKnob := FAutoAssignMidi;

  {for i := 0 to FframeAppSynthSettingsList.Count - 1 do begin
    XMLG2SynthType := XMLAppSettingsType.G2Synths.Add;
    XMLG2SynthType.Name := FframeAppSynthSettingsList[i].ID;
    XMLG2SynthType.UsbConnection := FframeAppSynthSettingsList[i].USBConnection;
    XMLG2SynthType.Host := FframeAppSynthSettingsList[i].Host;
    XMLG2SynthType.Port := FframeAppSynthSettingsList[i].Port;

    AppSynthSett := FframeAppSynthSettingsList[i];
    {for j := 0 to AppSynthSett.MidiToKnobList.Count - 1 do begin

      XMLMidiToKnobType := XMLG2SynthType.MidiToKnobs.Add;
      case AppSynthSett.MidiToKnobList[j].FSource of
        g2psParam  : XMLMidiToKnobType.Source := 'Params';
        g2psGlobal : XMLMidiToKnobType.Source := 'Global';
        g2psMorph  : XMLMidiToKnobType.Source := 'Morphs';
        g2psPatch  : XMLMidiToKnobType.Source := 'Patch';
      end;

      XMLMidiToKnobType.ControlType := integer(AppSynthSett.MidiToKnobList[j].FControlType);
      XMLMidiToKnobType.Knob := AppSynthSett.MidiToKnobList[j].FKnob;
      XMLMidiToKnobType.MidiCC := AppSynthSett.MidiToKnobList[j].FMidiCC;
    end;}
  {end;}

  XMLAppSettingsType.OwnerDocument.SaveToFile(path + 'g2editorfmx.xml');
end;

procedure TG2ConnectionManager.SetAutoAssignMidi(const aValue: integer);
begin
  FAutoAssignMidi := aValue;
end;

procedure TG2ConnectionManager.SetCableStyle(const Value: TCableStyle);
begin
  FCableStyle := Value;
end;

procedure TG2ConnectionManager.SetKnobControl(const Value: TKnobControl);
begin
  FKnobControl := Value;
end;

procedure TG2ConnectionManager.SetPatchDir(const aValue: string);
begin
  FPatchDir := aValue;
  NotifyObservers(EvtPatchDirChange, nil);
end;

procedure TG2ConnectionManager.SetSelectedCol(const Value: byte);
begin
  FSelectedCol := Value;
end;

procedure TG2ConnectionManager.SetSelectedConnectionIndex(const Value: integer);
begin
  if Value <> FSelectedConnectionIndex then begin
    FSelectedConnectionIndex := Value;
    NotifyObservers(EvtConnectionChange, self as IG2ConnectionManager);
  end;
end;

procedure TG2ConnectionManager.SetSelectedLocationIndex(const aValue: integer);
begin
  SelectedConnection.SelectedLocationIndex := aValue;
end;

procedure TG2ConnectionManager.SetSelectedModuleType(const Value: byte);
begin
  FSelectedModuleType := Value;
  if assigned(SelectedSlot) then
    SelectedSlot.NotifyObservers(evtSelectModuleType, SelectedSlot);
end;

procedure TG2ConnectionManager.SetSelectedRow(const Value: byte);
begin
  FSelectedRow := Value;
end;

procedure TG2ConnectionManager.SetSplitterVisible(const Value: integer);
begin
  FSplitterVisible := Value;
end;

procedure TG2ConnectionManager.SetZoomSetting1(const Value: single);
begin
  FZoomSetting1 := Value;
end;

procedure TG2ConnectionManager.SetZoomSetting2(const Value: single);
begin
  FZoomSetting2 := Value;
end;

procedure TG2ConnectionManager.SetZoomSetting3(const Value: single);
begin
  FZoomSetting3 := Value;
end;

end.

