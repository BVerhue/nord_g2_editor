unit UnitG2EditorFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Contnrs,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Math,
  FMX.Layouts, FMX.Objects, FMX.Menus, FMX.ExtCtrls,
{$IFDEF MSWINDOWS}
  LibUSBWinDyn,
{$ENDIF}
{$IFDEF MACOS}
  libusb_dyn,
{$ENDIF}
  g2_types, g2_file, g2_usb, G2FMXGraph, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components;

type
  TEditorSettings = class
    FCableThickness : integer;
    FSlotStripColor : TColor;
    FSlotStripInverseColor : TColor;
    FSlotStripDisabledColor : TColor;
    FHighlightColor : TColor;
    FLedColor : TColor;
  end;

  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    MenuBar1: TMenuBar;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    LayoutZoomFX: TLayout;
    Splitter1: TSplitter;
    ScrollboxVA: TScrollBox;
    LayoutZoomVA: TLayout;
    ScrollBoxFX: TScrollBox;
    LayoutVA: TG2FMXPatchArea;
    LayoutFX: TG2FMXPatchArea;
    cbUSBActive: TCheckBox;
    Label1: TLabel;
    timerStartUp: TTimer;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure timerStartUpTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    procedure G2AfterG2Init(Sender: TObject);
  public
    { Public declarations }
    FG2List : TObjectList;
    FG2Index : integer;
    FEditorSettingsList : TObjectList;
    FCopyPatch : TG2FilePatchPart;

    procedure InitPatchLocations;
    procedure AddG2( G2USBDevice : pusb_device);
    procedure SelectG2(G2Index: integer);
    function SelectedG2: TG2Graph;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


// === Form ====================================================================

procedure TForm1.FormCreate(Sender: TObject);
var G2DeviceList : TList;
    i : integer;
begin
  FormatSettings.DecimalSeparator := '.';
  //Application.UpdateFormatSettings := False;

  FG2List := TObjectList.Create( True);
  FEditorSettingsList := TObjectList.Create( True);

{$IFDEF MSWINDOWS}
  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);

    if G2DeviceList.Count > 0 then begin
      for i := 0 to G2DeviceList.Count - 1 do begin
        AddG2( pusb_device(G2DeviceList[i]));
      end;
    end else begin
      AddG2( nil);
      ShowMessage('No g2 device found')
    end;
  finally
    G2DeviceList.Free;
  end;
{$ENDIF}
{$IFDEF MACOS}
  LoadLibUSB;
{$ENDIF}
  FG2Index := -1;
  SelectG2(0);

  Caption := 'NMG2 Editor ' + NMG2_VERSION;

  timerStartup.Enabled := True;
end;

procedure TForm1.timerStartUpTimer(Sender: TObject);
var G2 : TG2Graph;
    i : integer;
begin
  timerStartup.Enabled := False;

  // Load module and parameter xml database
  for i := 0 to FG2List.Count - 1 do begin
    G2 := FG2List[i] as TG2Graph;

{$IFDEF MSWINDOWS}
    G2.LoadModuleDefs('');
{$ENDIF}
{$IFDEF MACOS}
    G2.LoadModuleDefs('/Applications/Delphi/PAServer/scratch-dir/Bruno-VBMac/');
{$ENDIF}

    if G2.FModuleDefList.FileVersion <> NMG2_VERSION then
      ShowMessage( 'Warning, ModuleDef.xml version differs from application.');

    if G2.FParamDefList.FileVersion <> NMG2_VERSION then
      ShowMessage( 'Warning, ParamDef.xml version differs from application.');

    G2.USBActive := True;
  end;

  G2 := Form1.SelectedG2;
  {if assigned(G2) then
    frmPatchBrowserModuleFilter.UpdateModules(G2.FModuleDefList);

  CreateAddModuleMenu;
  CreateModuleMenu;
  CreateParamMenu;

  Initialized := True;

  UpdateControls;}
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  LayoutVa.Scale.X := TrackBar1.Value;
  LayoutVa.Scale.Y := TrackBar1.Value;
  LayoutZoomVA.Width := LayoutVa.Width * TrackBar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * TrackBar1.Value;

  ScrollboxVa.VScrollBar.Value := (LayoutVa.Height/2 * TrackBar1.Value - LayoutVa.Height/2);
  ScrollboxVa.HScrollBar.Value := (LayoutVa.Width/2 * TrackBar1.Value - LayoutVa.Width/2);

  LayoutFX.Scale.X := TrackBar1.Value;
  LayoutFX.Scale.Y := TrackBar1.Value;
  LayoutZoomFX.Width := LayoutVa.Width * TrackBar1.Value;
  LayoutZoomFX.Height := LayoutVa.Height * TrackBar1.Value;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var G2 : TG2Graph;
    i : integer;
begin
  for i := 0 to FG2List.Count - 1 do begin
    G2 := FG2List[i] as TG2Graph;
    if G2.USBActive then
      G2.USBActive := False;
  end;
{$IFDEF MACOS}
  UnloadLIBUSB;
{$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if assigned(FCopyPatch) then
    FCopyPatch.Free;

  FEditorSettingsList.Free;
  FG2List.Free;
end;

procedure TForm1.AddG2( G2USBDevice : pusb_device);
var G2 : TG2Graph;
    EditorSettings : TEditorSettings;
begin
  G2 := TG2Graph.Create(self);
  G2.ClientType := ctEditor;
  G2.Host := '127.0.0.1';
  G2.Port := 2501 + FG2List.Count;
  G2.PatchAreaVA := LayoutVA;
  G2.PatchAreaFX := LayoutFX;
  G2.IsServer := True;
  G2.G2USBDevice := G2USBDevice;

  {G2.OnAddClient := G2AddClient;
  G2.OnAfterG2Init := G2AfterG2Init;
  G2.OnAfterRetrievePatch := G2AfterRetrievePatch;
  G2.OnAssignGlobalKnob := G2AssignGlobalKnob;
  G2.OnAssignKnob := G2AssignKnob;
  G2.OnBeforeSendMessage := G2BeforeSendMessage;
  G2.OnCreateModule := G2CreateModule;
  G2.OnDeassignGlobalKnob := G2DeassignGlobalKnob;
  G2.OnDeassignKnob := G2DeassignKnob;
  G2.OnDeleteClient := G2DeleteClient;
  G2.OnMidiCCReceive := G2MidiCCReceive;
  G2.OnPatchUpdate := G2PatchUpdate;
  G2.OnPatchNameChange := G2PatchNameChange;
  G2.OnPerfNameChange := G2PerfNameChange;
  G2.OnPerfSettingsUpdate := G2PerfSettingsUpdate;
  G2.OnReceiveResponseMessage := G2ReceiveResponseMessage;
  G2.OnSelectSlot := G2SelectSlot;
  G2.OnSynthSettingsUpdate := G2SynthSettingsUpdate;
  G2.OnUSBActiveChange := G2USBActiveChange;
  G2.OnVariationChange := G2VariationChange;
  G2.OnCopyVariation := G2CopyVariation;
  G2.OnAddModule := G2AddModule;
  G2.OnDeleteModule := G2DeleteModule;
  G2.OnAddCable := G2AddCable;
  G2.OnDeleteCable := G2DeleteCable;
  G2.OnSetModuleLabel := G2SetModuleLabel;
  G2.OnSetParamLabel := G2SetParamLabel;
  G2.OnSelectModule := G2SelectModule;
  G2.OnSelectParam := G2SelectParam;
  G2.OnAfterBankList := G2AfterBankList;
  G2.OnAfterStore := G2AfterStore;
  G2.OnAfterClear := G2AfterClear;
  G2.OnAfterClearBank := G2AfterClearBank;
  G2.OnMidiClockReceive := G2MidiClockReceive;
  G2.OnClockRunChange := G2ClockRunChange;
  G2.OnClockBPMChange := G2ClockBPMChange;
  G2.OnAfterBankDownload := G2AfterBankDownload;
  G2.OnAfterGetAssignedVoices := G2AfterGetAssignedVoices;}

  G2.OnAfterG2Init := G2AfterG2Init;

  FG2List.Add(G2);

  EditorSettings := TEditorSettings.Create;
  EditorSettings.FCableThickness := G_CableThickness;
  EditorSettings.FSlotStripColor := G_SlotStripColor;
  EditorSettings.FSlotStripInverseColor := G_SlotStripInverseColor;
  EditorSettings.FSlotStripDisabledColor := G_SlotStripDisabledColor;
  EditorSettings.FHighlightColor := G_HighLightColor;
  EditorSettings.FLedColor := G_LedColor;

  FEditorSettingsList.Add( EditorSettings);
end;

procedure TForm1.SelectG2(G2Index: integer);
var i : integer;
begin
  if (G2Index >= FG2List.Count) or (G2Index < 0) then
    raise Exception.Create( 'G2Index (' + IntToStr(G2Index) + ') out of bounds');

  if G2Index = FG2Index then
    exit;

  FG2Index := G2Index;
  (FG2List[FG2Index] as TG2Graph).PatchAreaVA := LayoutVA;
  (FG2List[FG2Index] as TG2Graph).PatchAreaFX := LayoutFX;

  with FEditorSettingsList[FG2Index] as TEditorSettings do begin
    G_SlotStripColor := FSlotStripColor;
    G_SlotStripInverseColor := FSlotStripInverseColor;
    G_SlotStripDisabledColor := FSlotStripDisabledColor;
    G_HighLightColor := FHighLightColor;
    G_LedColor := FLedColor;
    G_CableThickness := FCableThickness;
  end;

  {for i := 0 to 3 do
    if assigned(FSlotPanel[i]) then
      FSlotPanel[i].Slot := (FG2List[FG2Index] as TG2).Slot[i];

  ShakeCables;
  UpdateColorSchema;
  SelectSlot( (FG2List[FG2Index] as TG2).SelectedSlotIndex);

  UpdateControls;}
end;


function TForm1.SelectedG2: TG2Graph;
begin
  if FG2Index <> -1 then
    Result := FG2List[FG2Index] as TG2Graph
  else
    Result := nil;
end;

procedure TForm1.G2AfterG2Init(Sender: TObject);
var G2 : TG2Graph;
    Patch : TG2GraphPatch;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Patch := G2.GetSelectedPatch as TG2GraphPatch;
  if assigned(Patch) then
    Patch.Visible := True;

  InitPatchLocations;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var G2 : TG2Graph;
    Patch : TG2GraphPatch;
    PatchName : string;
    i : integer;
    FileStream : TFileStream;
    aFileName : string;
begin
  if OpenDialog1.Execute then begin
    aFileName := OpenDialog1.FileName;

    FileStream := TFileStream.Create( aFileName, fmOpenRead);
    try
      // Take the patchname from the filename
      aFilename := ExtractFilename( aFileName);

      // Name patch max size = 16, if shorter end with 0
      PatchName := '';
      i := 1;
      while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
        PatchName := PatchName + aFileName[i];
        inc(i);
      end;

      G2 := SelectedG2;
      if not assigned(G2) then
        exit;

      Patch := G2.GetSelectedPatch as TG2GraphPatch;
      if not assigned(Patch) then
        exit;

      Patch.Init;
      Patch.LoadFromFile(FileStream, nil);
      Patch.Visible := True;

      InitPatchLocations;
    finally
      FileStream.Free;
    end;
  end;
end;


procedure TForm1.InitPatchLocations;
var G2 : TG2Graph;
    Patch : TG2GraphPatch;
    i, max_col, max_row : integer;
begin
  G2 := SelectedG2;
  if not assigned(G2) then
    exit;

  Patch := G2.GetSelectedPatch as TG2GraphPatch;
  if not assigned(Patch) then
    exit;

  max_col := 0;
  max_row := 0;
  for i := 0 to Patch.ModuleCount[ord(ltVA)] - 1 do begin
    if Patch.ModuleList[ord(ltVA)][i].Row > max_row then
      max_row := Patch.ModuleList[ord(ltVA)][i].Row;

    if Patch.ModuleList[ord(ltVA)][i].Col > max_col then
      max_col := Patch.ModuleList[ord(ltVA)][i].Col;
  end;

  LayoutZoomVA.Position.X := 0;
  LayoutZoomVA.Position.Y := 0;
  LayoutVA.Position.X := 0;
  LayoutVA.Position.Y := 0;
  LayoutVA.Width := (max_col + 2) * UNITS_COL;
  LayoutVA.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomVA.Width := LayoutVa.Width * TrackBar1.Value;
  LayoutZoomVA.Height := LayoutVa.Height * TrackBar1.Value;

  max_col := 0;
  max_row := 0;
  for i := 0 to Patch.ModuleCount[ord(ltFX)] - 1 do begin
    if Patch.ModuleList[ord(ltFX)][i].Row > max_row then
      max_row := Patch.ModuleList[ord(ltFX)][i].Row;

    if Patch.ModuleList[ord(ltFX)][i].Col > max_col then
      max_col := Patch.ModuleList[ord(ltFX)][i].Col;
  end;

  LayoutZoomFX.Position.X := 0;
  LayoutZoomFX.Position.Y := 0;
  LayoutFX.Position.X := 0;
  LayoutFX.Position.Y := 0;
  LayoutFX.Width := (max_col + 2) * UNITS_COL;
  LayoutFX.Height := (max_row + 6) * UNITS_ROW;
  LayoutZoomFX.Width := LayoutFX.Width * TrackBar1.Value;
  LayoutZoomFX.Height := LayoutFX.Height * TrackBar1.Value;

end;

end.
