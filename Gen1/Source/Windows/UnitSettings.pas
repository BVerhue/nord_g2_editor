unit UnitSettings;

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

//  ////////////////////////////////////////////////////////////////////////////
//
//  This unit needs the Delphi MIDI I/O components, download at https://bitbucket.org/h4ndy/midiio-dev/overview
//
//  ////////////////////////////////////////////////////////////////////////////

{$I ..\..\Source\Common\Includes\delphi_version.inc}

interface
uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants, System.Classes,
  VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,  VCL.StdCtrls, VCL.ExtCtrls,
  VCL.ComCtrls, Vcl.CheckLst, VCL.FileCtrl,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, CheckLst,
  FileCtrl,
{$ENDIF}
  IdBaseComponent, IdComponent, IdUDPBase,
  IdUDPClient, IdSocketHandle, IdUDPServer, IdGlobal,
  DOM, XMLRead, XMLWrite,
  JawsCtrls,
  OSCUtils, MMSystem, MidiIn, MidiOut,
  g2_types, g2_database, g2_file, g2_classes, g2_midi;

const
   EXPLENATIONS : array[0..21] of string =
{ 0}      ('Is Server : If checked, the G2 VST or other OS G2 editors can connect to this editor using the TCP-IP settings. Only the server editor can have a direct USB connection to the G2 synth, so in a normal setup this one should be checked.',
{ 1}       'Port : The port number for the server editor.',
{ 2}       'Host : TCP-IP address of the PC the server editor is running on.',
{ 3}       'Timer broadcast led messages : The timer interval the server is sending led update messages to the clients (set higher to prevent network overload).',
{ 4}       'Midi enabled : Check to enable the midi ports of the editor to send and receive sysex messages. With tis you can upload and download patches to and from the G2 synth using sysex.',
{ 5}       'Sysex midi in : The midi port to receive sysex messages into the editor.',
{ 6}       'Sysex midi out : The midi port to send sysex messages.',
{ 7}       'Ctrl midi out : The midi out port to send controller midi messages from the editor to the controller.',
{ 8}       'Ctrl midi in : The midi in port to recieve controller midi messages for the editor.',
{ 9}       'Log enabled : Check to enable the log. Please note: this will slowdown the loading of patches.',
{10}       'Cable thickness : Enter a number to control the cable thickness in the patch windows.',
{11}       'Slot strip color : Select a color for the slot strips.',
{12}       'Slot strip inverse color : Select a color for the slot strip when selected.',
{13}       'Slot strip disabled color : Select a color for the slot strip when disabled.',
{14}       'Highlight color : Select a color for the module controls that are highlighted.',
{15}       'Led color : Select a color for the leds.',
{16}       'Only text menus : For use with JAWS (screenreading software for visualy disabled users) this should be checked, otherwise JAWS will not read out the menu options',
{17}       'Patch root folder : Set the root folder of your g2 patch library on disk for use in the patch browser.',
{18}       'Module help file : Set to "Nord Modular G2 Editor v1.62.chm".',
{19}       'G2ools folder : Set to the folder containing the g2ools executables.',
{20}       'Ctrl midi out : The midi port to send controller midi messages from the editor (parameter feedback).',
{21}       'Here you can set de folder containing the patches that are initially loaded in the patch buffer.');

type
  TfrmSettings = class(TForm)
    Memo1: TMemo;
    IdUDPServer1: TIdUDPServer;
    Button2: TButton;
    Panel1: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    cbIsServer: TCheckBox;
    TabSheet5: TTabSheet;
    cbSlotStripColor: TColorBox;
    cbSlotStripInverseColor: TColorBox;
    cbSlotStripDisabledColor: TColorBox;
    cbHighLightColor: TColorBox;
    cbOnlyTextMenus: TCheckBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    ePort: DEdit;
    eHost: DEdit;
    eTimerBroadcastLedMessages: DEdit;
    eCableThickness: DEdit;
    eOSCServerIP: DEdit;
    eOSCHostPort: DEdit;
    Panel2: TPanel;
    cbLedColor: TColorBox;
    StaticText15: TStaticText;
    OpenDialog1: TOpenDialog;
    TabSheet6: TTabSheet;
    StaticText6: TStaticText;
    bMidiMapping: TButton;
    StaticText18: TStaticText;
    lExplenation: TStaticText;
    clbCtrlMidiOutDevices: TCheckListBox;
    clbCtrlMidiInDevices: TCheckListBox;
    clbSysexMidiInDevices: TCheckListBox;
    clbSysExMidiOutDevices: TCheckListBox;
    Panel3: TPanel;
    GroupBox1: TGroupBox;
    bSelectPtachRootFolder: TButton;
    StaticText12: TStaticText;
    ePatchRootFolder: DEdit;
    StaticText16: TStaticText;
    StaticText17: TStaticText;
    eModuleHelpFile: TEdit;
    eG2oolsFolder: TEdit;
    bSelectModuleHelpFile: TButton;
    bSelectG2oolsFolder: TButton;
    GroupBox2: TGroupBox;
    cbLogEnabled: TCheckBox;
    Panel4: TPanel;
    Panel5: TPanel;
    bCreateG2VSTIni: TButton;
    ePatchBufferFolder: TEdit;
    bSelectPatchBufferFolder: TButton;
    StaticText19: TStaticText;
    procedure Button2Click(Sender: TObject);
    procedure IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbIsServerClick(Sender: TObject);
    procedure cbMidiEnabledClick(Sender: TObject);
    procedure bSelectPtachRootFolderClick(Sender: TObject);
    procedure cbLogEnabledClick(Sender: TObject);
    procedure cbSlotStripColorChange(Sender: TObject);
    procedure cbSlotStripInverseColorChange(Sender: TObject);
    procedure cbSlotStripDisabledColorChange(Sender: TObject);
    procedure cbHighLightColorChange(Sender: TObject);
    procedure cbOnlyTextMenusClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cbIsServerEnter(Sender: TObject);
    procedure ePortEnter(Sender: TObject);
    procedure eHostEnter(Sender: TObject);
    procedure eTimerBroadcastLedMessagesEnter(Sender: TObject);
    procedure cbCtrlMidiEnabledEnter(Sender: TObject);
    procedure cbCtrlMidiInDevicesEnter(Sender: TObject);
    procedure cbLogEnabledEnter(Sender: TObject);
    procedure eCableThicknessEnter(Sender: TObject);
    procedure cbSlotStripColorEnter(Sender: TObject);
    procedure cbSlotStripInverseColorEnter(Sender: TObject);
    procedure cbSlotStripDisabledColorEnter(Sender: TObject);
    procedure cbHighLightColorEnter(Sender: TObject);
    procedure cbLedColorEnter(Sender: TObject);
    procedure cbOnlyTextMenusEnter(Sender: TObject);
    procedure ePatchRootFolderEnter(Sender: TObject);
    procedure eModuleHelpFileEnter(Sender: TObject);
    procedure eG2oolsFolderEnter(Sender: TObject);
    procedure bSelectModuleHelpFileClick(Sender: TObject);
    procedure bSelectG2oolsFolderClick(Sender: TObject);
    procedure cbLedColorChange(Sender: TObject);
    procedure bMidiMappingClick(Sender: TObject);
    procedure cbCtrlMidiOutDevicesEnter(Sender: TObject);
    procedure clbCtrlMidiOutDevicesClickCheck(Sender: TObject);
    procedure clbCtrlMidiInDevicesClickCheck(Sender: TObject);
    procedure clbSysexMidiInDevicesClickCheck(Sender: TObject);
    procedure clbSysExMidiOutDevicesClickCheck(Sender: TObject);
    procedure clbSysexMidiInDevicesEnter(Sender: TObject);
    procedure clbSysExMidiOutDevicesEnter(Sender: TObject);
    procedure clbCtrlMidiInDevicesEnter(Sender: TObject);
    procedure clbCtrlMidiOutDevicesEnter(Sender: TObject);
    procedure bCreateG2VSTIniClick(Sender: TObject);
    procedure bSelectPatchBufferFolderClick(Sender: TObject);
    procedure ePatchBufferFolderEnter(Sender: TObject);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure UpdateControls;
    procedure InitMidiDevices;
    procedure ApplyCtrlMidiDevices;
    procedure UpdateSysExMidiDevices;
    procedure UpdateCtrlMidiDevices;
    procedure ApplySysExMidiDevices;
    procedure MidiDeviceStateChange( Sender : TObject);
  end;

var
  frmSettings: TfrmSettings;

implementation

{$R *.dfm}

uses UnitG2Editor, UnitMidiMapping;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  InitMidiDevices;
  LoadIniXML;
  ApplyCtrlMidiDevices;
  frmMidiMapping.UpdateCtrlMidiOutputList;
  UpdateSysExMidiDevices;
  ApplySysExMidiDevices;
end;

procedure TfrmSettings.FormClose(Sender: TObject; var Action: TCloseAction);
var G2 : TG2;
    i, c, ledtimer : integer;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Host := eHost.Text;
    G2.Port := StrToInt(ePort.Text);
    val( eTimerBroadcastLedMessages.Text, ledtimer, c);
    if c = 0 then
      G2.TimerBroadcastLedMessages := ledtimer;

    val( eCableThickness.Text, i, c);
    if c = 0 then begin
      if G_CableThickness <> i then begin
        G_CableThickness := i;
        frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
        frmG2Main.ShakeCables;
      end;
    end;
  end;
end;

procedure TfrmSettings.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmSettings.UpdateControls;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    // TCP-IP
    cbIsServer.Checked := G2.IsServer;
    eHost.Text := G2.Host;
    ePort.Text := IntTostr(G2.Port);
    if cbIsServer.Checked then begin
      eHost.Enabled := False;
    end else
      eHost.Enabled := True;
    eTimerBroadcastLedMessages.Text := IntToStr(G2.TimerBroadcastLedMessages);

    UpdateSysExMidiDevices;
    UpdateCtrlMidiDevices;

    cbLogEnabled.Checked := G2.LogLevel = 1;
    eCableThickness.Text := IntToStr(G_CableThickness);
    cbSlotStripColor.Selected := G_SlotStripColor;
    cbSlotStripInverseColor.Selected := G_SlotStripInverseColor;
    cbSlotStripDisabledColor.Selected := G_SlotStripDisabledColor;
    cbHighLightColor.Selected := G_HighLightColor;
    cbLedColor.Selected := G_LedColor;
    cbOnlyTextMenus.Checked := frmG2Main.OnlyTextMenus;

    Panel3.Caption := 'For selected G2 synth: ' + G2.SynthName;
    Panel4.Caption := 'For selected G2 synth: ' + G2.SynthName;
    Panel5.Caption := 'For selected G2 synth: ' + G2.SynthName;
  finally
    FDisableControls := False;
  end;
end;

procedure TfrmSettings.LoadIniXML;
var Doc : TXMLDocument;
    RootNode, SynthNode : TDOMNode;
    mi, mo : integer;
    PatchBrowserSettingsNode : TXMLPatchBrowserSettingsType;
    PatchBufferSettingsNode : TXMLPatchBufferSettingsType;
    DirSettingsNode : TXMLDirectorySettingsType;
    MidiSettingsNode : TXMLMidiDeviceType;
    CtrlMidiDeviceListNode : TDOMNode;
    CtrlMidiDeviceList : TXMLCtrlMidiDeviceListType;
    CtrlMidiDevice : TXMLCtrlMidiDeviceType;
    FormSettingsNode : TXMLFormSettingsType;
    G2 : TG2;
    i, j, d : integer;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      for i := 0 to frmG2Main.FG2List.Count - 1 do begin

        SynthNode := RootNode.FindNode('G2_Synth_' + IntToStr(i+1));
        if assigned(SynthNode) then begin

          G2 := frmG2Main.FG2List[i] as TG2;

          if assigned(G2) then begin

            MidiSettingsNode := TXMLMidiDeviceType( SynthNode.FindNode('MIDI_settings'));
            if assigned( MidiSettingsNode) then begin
              G2.MidiInDeviceName := MidiSettingsNode.MidiInDevice;
              G2.MidiOutDeviceName := MidiSettingsNode.MidiOutDevice;
            end;
          end;
        end;
      end;

      CtrlMidiDeviceListNode := RootNode.FindNode('CTRL_MIDI_in_device_list');
      if assigned(CtrlMidiDeviceListNode) then begin
        CtrlMidiDeviceList := TXMLCtrlMidiDeviceListType.Create( CtrlMidiDeviceListNode);
        if assigned(CtrlMidiDeviceList) then begin
          try
            for i := 0 to clbCtrlMidiInDevices.Items.Count - 1 do begin
              j := 0;
              while (j<CtrlMidiDeviceList.Count) and (clbCtrlMidiInDevices.Items[i] <> CtrlMidiDeviceList[j].CtrlMidiDevice) do
                inc(j);

              if (j<CtrlMidiDeviceList.Count) then begin
                clbCtrlMidiInDevices.Checked[i] := True;
              end;
            end;
            for i := 0 to CtrlMidiDeviceList.Count - 1 do begin
              j := 0;
              while (j<clbCtrlMidiInDevices.Items.Count) and (clbCtrlMidiInDevices.Items[j] <> CtrlMidiDeviceList[i].CtrlMidiDevice) do
                inc(j);

              if not(j<clbCtrlMidiInDevices.Items.Count) then begin
                // Not found anymore in current device list, add to list but disabled
                d := clbCtrlMidiInDevices.Items.Add( CtrlMidiDeviceList[i].CtrlMidiDevice);
                clbCtrlMidiInDevices.Checked[d] := True;
                clbCtrlMidiInDevices.ItemEnabled[d] := False;
              end;
            end;
          finally
            CtrlMidiDeviceList.Free;
          end;
        end;
      end;

      CtrlMidiDeviceListNode := RootNode.FindNode('CTRL_MIDI_out_device_list');
      if assigned(CtrlMidiDeviceListNode) then begin
        CtrlMidiDeviceList := TXMLCtrlMidiDeviceListType.Create( CtrlMidiDeviceListNode);
        if assigned(CtrlMidiDeviceList) then begin
          try
            for i := 0 to clbCtrlMidiOutDevices.Items.Count - 1 do begin
              j := 0;
              while (j<CtrlMidiDeviceList.Count) and (clbCtrlMidiOutDevices.Items[i] <> CtrlMidiDeviceList[j].CtrlMidiDevice) do
                inc(j);

              if (j<CtrlMidiDeviceList.Count) then begin
                clbCtrlMidiOutDevices.Checked[i] := True;
              end;
            end;
            for i := 0 to CtrlMidiDeviceList.Count - 1 do begin
              j := 0;
              while (j<clbCtrlMidiOutDevices.Items.Count) and (clbCtrlMidiOutDevices.Items[j] <> CtrlMidiDeviceList[i].CtrlMidiDevice) do
                inc(j);

              if not(j<clbCtrlMidiOutDevices.Items.Count) then begin
                // Not found anymore in current device list, add to list but disabled
                d := clbCtrlMidiOutDevices.Items.Add( CtrlMidiDeviceList[i].CtrlMidiDevice);
                clbCtrlMidiOutDevices.Checked[d] := True;
                clbCtrlMidiOutDevices.ItemEnabled[d] := False;
              end;
            end;
          finally
            CtrlMidiDeviceList.Free;
          end;
        end;
      end;

      DirSettingsNode := TXMLDirectorySettingsType(RootNode.FindNode('DirectorySettings'));
      if assigned(DirSettingsNode) then begin
        eG2oolsFolder.Text := String(DirSettingsNode.G2oolsFolder);
        eModuleHelpFile.Text := String(DirSettingsNode.ModuleHelpFile);
      end;

      PatchBrowserSettingsNode := TXMLPatchBrowserSettingsType(RootNode.FindNode('PatchBrowserSettings'));
      if assigned(PatchBrowserSettingsNode) then begin
        ePatchRootFolder.Text := String(PatchBrowserSettingsNode.BaseFolder);
      end;

      PatchBufferSettingsNode := TXMLPatchBufferSettingsType(RootNode.FindNode('PatchBufferSettings'));
      if assigned(PatchBufferSettingsNode) then begin
        ePatchBufferFolder.Text := String(PatchBufferSettingsNode.Folder);
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('SettingsForm'));
      if assigned(FormSettingsNode) then begin
        Left := FormSettingsNode.PosX;
        Top := FormSettingsNode.PosY;
        {Width := FormSettingsNode.SizeX;
        Height := FormSettingsNode.SizeY;}
        Visible := FormSettingsNode.Visible;
      end;

    end;
  finally
    Doc.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  General settings
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.cbLogEnabledClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  if cbLogEnabled.Checked then
    G2.LogLevel := 1
  else
    G2.LogLevel := 0;
end;

procedure TfrmSettings.bSelectG2oolsFolderClick(Sender: TObject);
var FDir : string;
begin
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := 'Select Directory';
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
        OkButtonLabel := 'Select';
        DefaultFolder := FDir;
        FileName := FDir;
        if Execute then
          eG2oolsFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      eG2oolsFolder.Text := FDir;
  frmG2Main.UpdateControls;
end;

procedure TfrmSettings.bSelectModuleHelpFileClick(Sender: TObject);
var FDir : string;
begin
  OpenDialog1.Filter := 'compiled help files (*.chm)|*.chm';
  if OpenDialog1.Execute then
      eModuleHelpFile.Text := OpenDialog1.FileName;;
  frmG2Main.UpdateControls;
end;

procedure TfrmSettings.bSelectPatchBufferFolderClick(Sender: TObject);
var FDir : string;
begin
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := 'Select Directory';
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
        OkButtonLabel := 'Select';
        DefaultFolder := FDir;
        FileName := FDir;
        if Execute then
          ePatchBufferFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      ePatchBufferFolder.Text := FDir;
  frmG2Main.UpdateControls;
end;

procedure TfrmSettings.bSelectPtachRootFolderClick(Sender: TObject);
var FDir : string;
begin
  if Win32MajorVersion >= 6 then
    with TFileOpenDialog.Create(nil) do
      try
        Title := 'Select Directory';
        Options := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem]; // YMMV
        OkButtonLabel := 'Select';
        DefaultFolder := FDir;
        FileName := FDir;
        if Execute then
          ePatchRootFolder.Text := FileName;
      finally
        Free;
      end
  else
    if SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
               [sdNewUI, sdNewFolder]) then
      ePatchRootFolder.Text := FDir;
  frmG2Main.UpdateControls;
end;

////////////////////////////////////////////////////////////////////////////////
//  Midi settings
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.InitMidiDevices;
var i, d : integer;
begin
  // Add all devices to the midi checkbox lists

  for i := 0 to frmG2Main.FMidiOutDevices.Count - 1 do begin

    d := clbCtrlMidiOutDevices.Items.Add(frmG2Main.FMidiOutDevices[i].Name);
    clbCtrlMidiOutDevices.Items.Objects[d] := frmG2Main.FMidiOutDevices[i];

    d := clbSysExMidiOutDevices.Items.Add(frmG2Main.FMidiOutDevices[i].Name);
    clbSysExMidiOutDevices.Items.Objects[d] := frmG2Main.FMidiOutDevices[i];

    frmG2Main.FMidiOutDevices[i].OnMidiDeviceStateChange := MidiDeviceStateChange;
  end;

  for i := 0 to frmG2Main.FMidiInDevices.Count - 1 do begin

    d := clbCtrlMidiInDevices.Items.Add(frmG2Main.FMidiInDevices[i].Name);
    clbCtrlMidiInDevices.Items.Objects[d] := frmG2Main.FMidiInDevices[i];

    d := clbSysExMidiInDevices.Items.Add(frmG2Main.FMidiInDevices[i].Name);
    clbSysExMidiInDevices.Items.Objects[d] := frmG2Main.FMidiInDevices[i];

    frmG2Main.FMidiInDevices[i].OnMidiDeviceStateChange := MidiDeviceStateChange;
  end;
end;

procedure TfrmSettings.UpdateSysExMidiDevices;
var i : integer;
    G2 : TG2;
    found : boolean;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  Found := false;
  i := 0;
  while (i<clbSysExMidiInDevices.Items.Count) do begin

    if (clbSysExMidiInDevices.Items[i] = G2.MidiInDeviceName) then begin
      found := True;
      clbSysExMidiInDevices.Checked[i] := True;
    end else
      clbSysExMidiInDevices.Checked[i] := False;

    MidiInDevice := clbSysExMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then
      clbSysExMidiInDevices.ItemEnabled[i] := MidiInDevice.Assignment in [mdatNone, mdatSysEx]
    else
      clbSysExMidiInDevices.ItemEnabled[i] := False;

    inc(i);
  end;

  if (not found) and (G2.MidiInDeviceName<>'') then begin
    // Not found anymore in current device list, add to list but disabled
    i := clbSysExMidiInDevices.Items.Add( G2.MidiInDeviceName);
    clbSysExMidiInDevices.Checked[i] := True;
    clbSysExMidiInDevices.ItemEnabled[i] := False;
  end;

  Found := false;
  i := 0;
  while (i<clbSysExMidiOutDevices.Items.Count) do begin

    if (clbSysExMidiOutDevices.Items[i] = G2.MidiOutDeviceName) then begin
      found := True;
      clbSysExMidiOutDevices.Checked[i] := True;
    end else
      clbSysExMidiOutDevices.Checked[i] := False;

    MidiOutDevice := clbSysExMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then
      clbSysExMidiOutDevices.ItemEnabled[i] := MidiOutDevice.Assignment in [mdatNone, mdatSysEx]
    else
      clbSysExMidiOutDevices.ItemEnabled[i] := False;

    inc(i);
  end;

  if (not found) and (G2.MidiOutDeviceName<>'') then begin
    // Not found anymore in current device list, add to list but disabled
    i := clbSysExMidiOutDevices.Items.Add( G2.MidiOutDeviceName);
    clbSysExMidiOutDevices.Checked[i] := True;
    clbSysExMidiOutDevices.ItemEnabled[i] := False;
  end;
end;

procedure TfrmSettings.ApplySysExMidiDevices;
var i : integer;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  for i := 0 to clbSysExMidiInDevices.Count - 1 do begin
    MidiInDevice := clbSysExMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then begin
      if (not clbSysExMidiInDevices.Checked[i]) and (MidiInDevice.Assignment = mdatSysEx) and MidiInDevice.Open then begin
        if G2.MidiInDevice = MidiInDevice then begin
          MidiInDevice.MidiInput.OnMidiInput := nil;
          G2.MidiInDevice := nil;
          G2.MidiInDeviceName := '';
        end;
        MidiInDevice.Assignment := mdatNone;
      end;
    end;
  end;

  for i := 0 to clbSysExMidiInDevices.Count - 1 do begin
    MidiInDevice := clbSysExMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then begin
      if clbSysExMidiInDevices.Checked[i] and not(MidiInDevice.Open) then begin
        G2.MidiInDevice := MidiInDevice;
        MidiInDevice.MidiInput.OnMidiInput := G2.DoMidiInput;
        MidiInDevice.Assignment := mdatSysEx;
      end;
    end;
  end;

  for i := 0 to clbSysExMidiOutDevices.Count - 1 do begin
    MidiOutDevice := clbSysExMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then begin
      if (not clbSysExMidiOutDevices.Checked[i]) and (MidiOutDevice.Assignment = mdatSysEx) and MidiOutDevice.Open then begin
        if G2.MidiOutDevice = MidiOutDevice then begin
          G2.MidiOutDevice := nil;
          G2.MidiOutDeviceName := '';
        end;
        MidiOutDevice.Assignment := mdatNone;
      end;
    end;
  end;

  for i := 0 to clbSysExMidiOutDevices.Count - 1 do begin
    MidiOutDevice := clbSysExMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then begin
      if clbSysExMidiOutDevices.Checked[i] then begin
        G2.MidiOutDevice := MidiOutDevice;
        MidiOutDevice.Open := True;
        MidiOutDevice.Assignment := mdatSysEx;
      end;
    end;
  end;
end;

procedure TfrmSettings.UpdateCtrlMidiDevices;
var i : integer;
    G2 : TG2;
    found : boolean;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
begin
  for i := 0 to clbCtrlMidiInDevices.Count - 1 do begin
    MidiInDevice := clbCtrlMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then
      clbCtrlMidiInDevices.ItemEnabled[i] := MidiInDevice.Assignment in [mdatNone, mdatCtrl]
    else
      clbCtrlMidiInDevices.ItemEnabled[i] := False;
  end;

  for i := 0 to clbCtrlMidiOutDevices.Count - 1 do begin
    MidiOutDevice := clbCtrlMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then
      clbCtrlMidiOutDevices.ItemEnabled[i] := MidiOutDevice.Assignment in [mdatNone, mdatCtrl]
    else
      clbCtrlMidiOutDevices.ItemEnabled[i] := False;
  end;
end;

procedure TfrmSettings.ApplyCtrlMidiDevices;
var i : integer;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
begin
  for i := 0 to clbCtrlMidiInDevices.Count - 1 do begin
    MidiInDevice := clbCtrlMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) and (MidiInDevice.Assignment in [mdatNone, mdatCtrl]) then begin
      if clbCtrlMidiInDevices.Checked[i] then begin
        MidiInDevice.Assignment := mdatCtrl;
        MidiInDevice.MidiInput.OnMidiInput := frmMidiMapping.DoCtrlMidiInput;
      end;
    end;
  end;

  for i := 0 to clbCtrlMidiOutDevices.Count - 1 do begin
    MidiOutDevice := clbCtrlMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) and (MidiOutDevice.Assignment in [mdatNone, mdatCtrl]) then begin
      if clbCtrlMidiOutDevices.Checked[i] then begin
        MidiOutDevice.Assignment := mdatCtrl;
      end;
    end;
  end;
end;

procedure TfrmSettings.MidiDeviceStateChange(Sender: TObject);
var i : integer;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
begin
  for i := 0 to clbCtrlMidiInDevices.Items.Count - 1 do begin
    MidiInDevice := clbCtrlMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then begin
      if MidiInDevice.Assignment = mdatCtrl then begin
        clbCtrlMidiInDevices.Checked[i] := MidiInDevice.Open;
      end else begin
        clbCtrlMidiInDevices.Checked[i] := False;
        clbCtrlMidiInDevices.ItemEnabled[i] := MidiIndevice.Assignment = mdatNone;
      end;
    end else
      clbCtrlMidiInDevices.ItemEnabled[i] := False;
  end;

  for i := 0 to clbCtrlMidiOutDevices.Items.Count - 1 do begin
    MidiOutDevice := clbCtrlMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then begin
      if MidiOutDevice.Assignment = mdatCtrl then begin
        clbCtrlMidiOutDevices.Checked[i] := MidiOutDevice.Open;
      end else begin
        clbCtrlMidiOutDevices.Checked[i] := False;
        clbCtrlMidiOutDevices.ItemEnabled[i] := MidiOutDevice.Assignment = mdatNone;
      end;
    end else
      clbCtrlMidiOutDevices.ItemEnabled[i] := False;
  end;


  for i := 0 to clbSysExMidiInDevices.Items.Count - 1 do begin
    MidiInDevice := clbSysExMidiInDevices.Items.Objects[i] as TMidiInDevice;
    if assigned(MidiInDevice) then begin
      if MidiInDevice.Assignment = mdatSysEx then begin
        clbSysExMidiInDevices.Checked[i] := MidiInDevice.Open;
      end else begin
        clbSysExMidiInDevices.Checked[i] := False;
        clbSysExMidiInDevices.ItemEnabled[i] := MidiIndevice.Assignment = mdatNone;
      end;
    end else
      clbSysExMidiInDevices.ItemEnabled[i] := False;
  end;

  for i := 0 to clbSysExMidiOutDevices.Items.Count - 1 do begin
    MidiOutDevice := clbSysExMidiOutDevices.Items.Objects[i] as TMidiOutDevice;
    if assigned(MidiOutDevice) then begin
      if MidiOutDevice.Assignment = mdatSysEx then begin
        clbSysExMidiOutDevices.Checked[i] := MidiOutDevice.Open;
      end else begin
        clbSysExMidiOutDevices.Checked[i] := False;
        clbSysExMidiOutDevices.ItemEnabled[i] := MidiOutdevice.Assignment = mdatNone;
      end;
    end else
      clbSysExMidiOutDevices.ItemEnabled[i] := False;
  end;
end;

procedure TfrmSettings.clbCtrlMidiInDevicesClickCheck(Sender: TObject);
begin
  ApplyCtrlMidiDevices;
end;

procedure TfrmSettings.clbCtrlMidiOutDevicesClickCheck(Sender: TObject);
begin
  ApplyCtrlMidiDevices;
  frmMidiMapping.UpdateCtrlMidiOutputList;
end;

procedure TfrmSettings.clbSysexMidiInDevicesClickCheck(Sender: TObject);
var i, j : integer;
begin
  // Make sure only one is selected
  i := clbSysexMidiInDevices.ItemIndex;
  if clbSysexMidiInDevices.Checked[i] then begin
    for j := 0 to clbSysExMidiIndevices.Count - 1 do
      if (j<>i) and clbSysexMidiInDevices.Checked[j] then
        clbSysexMidiInDevices.Checked[j] := False;
  end;
  ApplySysexMidiDevices;
end;

procedure TfrmSettings.clbSysExMidiOutDevicesClickCheck(Sender: TObject);
var i, j : integer;
begin
  // Make sure only one is selected
  i := clbSysexMidiOutDevices.ItemIndex;
  if clbSysexMidiOutDevices.Checked[i] then begin
    for j := 0 to clbSysexMidiOutDevices.Count - 1 do
      if (j<>i) and clbSysexMidiOutDevices.Checked[j] then
        clbSysexMidiOutDevices.Checked[j] := False;
  end;
  ApplySysexMidiDevices;
end;

procedure TfrmSettings.bCreateG2VSTIniClick(Sender: TObject);
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    VSTTCPSettingsNode : TXMLVSTTCPSettingsType;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  Doc := TXMLDocument.Create;
  try
    RootNode := Doc.CreateElement('G2_VST_settings');
    Doc.AppendChild(RootNode);

    VSTTCPSettingsNode := TXMLVSTTCPSettingsType( Doc.CreateElement('TCP_settings'));
    RootNode.AppendChild(VSTTCPSettingsNode);
    VSTTCPSettingsNode.IP := G2.Host;
    VSTTCPSettingsNode.Port := G2.Port;

    WriteXMLFile( Doc, 'G2_VST_ini.xml');
  finally
    Doc.Free;
  end;
end;

procedure TfrmSettings.bMidiMappingClick(Sender: TObject);
begin
  frmMidiMapping.Show;
end;

procedure TfrmSettings.cbMidiEnabledClick(Sender: TObject);
begin
end;

////////////////////////////////////////////////////////////////////////////////
//  Editor settings
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.cbOnlyTextMenusClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if cbOnlyTextMenus.Checked then
    frmG2Main.OnlyTextMenus := True
  else
    frmG2Main.OnlyTextMenus := False;
end;

procedure TfrmSettings.cbSlotStripDisabledColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripDisabledColor := cbSlotStripDisabledColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripInverseColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripInverseColor := cbSlotStripInverseColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbLedColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_LedColor := cbLedColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbHighLightColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_HighLightColor := cbHighLightColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

procedure TfrmSettings.cbSlotStripColorChange(Sender: TObject);
begin
  if FDisableControls then
    exit;

  G_SlotStripColor := cbSlotStripColor.Selected;

  frmG2Main.SetEditorSettings( G_SlotStripColor, G_SlotStripInverseColor, G_SlotStripDisabledColor, G_HighLightColor, G_LedColor, G_CableThickness);
end;

////////////////////////////////////////////////////////////////////////////////
//  Server / TCP-IP settings
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.cbIsServerClick(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    try
      G2.IsServer := cbIsServer.Checked;
    finally
      UpdateControls;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  Help
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.cbCtrlMidiEnabledEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[7];
end;

procedure TfrmSettings.clbSysexMidiInDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[5];
end;

procedure TfrmSettings.clbSysExMidiOutDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[6];
end;

procedure TfrmSettings.cbCtrlMidiInDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[8];
end;

procedure TfrmSettings.cbCtrlMidiOutDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[20];
end;

procedure TfrmSettings.cbSlotStripColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[11];
end;

procedure TfrmSettings.cbSlotStripDisabledColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[13];
end;

procedure TfrmSettings.cbSlotStripInverseColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[12];
end;

procedure TfrmSettings.cbLedColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[15];
end;

procedure TfrmSettings.eCableThicknessEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[10];
end;

procedure TfrmSettings.eG2oolsFolderEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[19];
end;

procedure TfrmSettings.eHostEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[2];
end;

procedure TfrmSettings.eModuleHelpFileEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[18];
end;

procedure TfrmSettings.ePatchBufferFolderEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[21];
end;

procedure TfrmSettings.ePatchRootFolderEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[17];
end;

procedure TfrmSettings.ePortEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[1];
end;

procedure TfrmSettings.eTimerBroadcastLedMessagesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[3];
end;

procedure TfrmSettings.cbHighLightColorEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[14];
end;

procedure TfrmSettings.cbIsServerEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[0];
end;

procedure TfrmSettings.cbLogEnabledEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[9];
end;

procedure TfrmSettings.cbOnlyTextMenusEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[16];
end;

procedure TfrmSettings.clbCtrlMidiInDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[8];
end;

procedure TfrmSettings.clbCtrlMidiOutDevicesEnter(Sender: TObject);
begin
  lExplenation.Caption := EXPLENATIONS[7];
end;



////////////////////////////////////////////////////////////////////////////////
// OSC interface, someting for later...
////////////////////////////////////////////////////////////////////////////////

procedure TfrmSettings.udpServerDeviceUDPRead(AThread: TIdUDPListenerThread; AData: TIdBytes; ABinding: TIdSocketHandle);
var i, p, c, b, KnobIndex : integer;
    address : AnsiString;
    Knob : TKnob;
    OscBundle : TOSCBundle;
    OscMessage : TOSCMessage;
    OscPacket : TOSCPacket;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  OscPacket := TOSCPacket.Unpack(@AData[0], Length(AData));
  try
    if OscPacket is TOSCBundle then begin
      //dump_buffer( Memo1, AData, Length(AData));

      for p := 0 to 4 do
        for c := 0 to 2 do
          for b := 0 to 7 do begin
            address := AnsiString('/' + PARAM_PAGE_NAMES[p] + '/' + IntToStr(c) + '/' + IntToStr(b));
            OscMessage := (OscPacket as TOSCBundle).MatchAddress(Address);
              if assigned(OscMessage) then begin
                OscMessage.Decode;
                KnobIndex := (c-1) * 8 + p * 8 * 3 + (b-1);
                Knob := G2.SelectedPatch.GetKnob( KnobIndex);
                if assigned(Knob) and (Knob.IsAssigned = 1) then
                for i := 0 to OscMessage.ArgumentCount - 1 do begin
                  Memo1.Lines.Add(string(Address + ':' + OscMessage.Argument[i]));
                  Knob.Parameter.SetParameterValue( trunc(127 * StrToFloat(string(OscMessage.Argument[i]))));
                end;
              end;
          end;
    end;
    if OscPacket is TOSCMessage then begin
      G2.dump_buffer( AData, Length(AData));
    end;
  finally
     OSCPacket.Free;
  end;
end;

procedure TfrmSettings.Button2Click(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if IdUDPServer1.Active then
    IdUDPServer1.Active := False
  else
    IdUDPServer1.Active := True;
end;

procedure TfrmSettings.IdUDPServer1Status(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  Memo1.Lines.Add( AStatusText)
end;

end.
