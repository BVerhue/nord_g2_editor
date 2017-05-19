unit UnitG2CentralStrip;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.UIConsts,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit, FMX.Objects,
{$IFDEF MSWINDOWS}
  LibUSBWinDyn,
{$ENDIF}
{$IFDEF MACOS}
  libusb_dyn,
{$ENDIF}
  BVE.NMG2Types, BVE.NMG2USB, BVE.NMG2Mess, BVE.NMG2GraphFMX, BVE.NMG2ControlsFMX,
  UnitAppSettings, UnitPatch, UnitAddModule,
  FMX.TabControl, UnitSlotStrip, UnitSlot, UnitBanks, UnitPatchSettings,
  UnitKnobs, UnitParam, UnitModule, UnitSynthSettings;

const
  RBWidth = 24;
  SPACER = 2;


type
  TFrameTabs = (ftSlotA, ftSlotB, ftSlotC, ftSlotD, ftBanks, ftPatchSettings,
                ftKnobs, ftSynthSettings, ftAppSettings);
  TPatchTabs = (ptAddModule, ptModule, ptParam, ptPatch);

  TAppShowMessageEvent = procedure(Sender: TObject; aMessage : string) of object;
  TAppCloseMessageEvent = procedure(Sender: TObject) of object;

  TframeEditorCentralStrip = class(TFrame, IG2Observer)
    lSynth: TLayout;
    tcPatchFunctions: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    Layout1: TLayout;
    lSlotStrips: TLayout;
    tcFrame: TTabControl;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    TabItem7: TTabItem;
    TabItem8: TTabItem;
    TabItem9: TTabItem;
    TabItem10: TTabItem;
    rbSynth: TG2BtnRadio;
    rLed: TRectangle;
    tfSynthName: TG2TextField;
    ePerfName: TEdit;
    idClockBPM: TG2BtnIncDec;
    tfMasterClock: TG2TextField;
    btSplitter: TG2BtnText;
    btKeyboardSplit: TG2BtnText;
    btClockRun: TG2BtnText;
    btPerfMode: TG2BtnText;
    rbZoom: TG2BtnRadio;
    frameSlotStrip1: TframeSlotStrip;
    frameSlotStrip2: TframeSlotStrip;
    frameSlotStrip3: TframeSlotStrip;
    frameSlotStrip4: TframeSlotStrip;
    rbSlotStrip: TG2BtnRadio;
    frameBanks: TframeBanks;
    framePatchSettings: TframePatchSettings;
    frameKnobs: TframeKnobs;
    frameSynthSettings: TframeSynthSettings;
    frameAppSettings: TframeAppSettings;
    FrameAddModule: TFrameAddModule;
    frameModule: TframeModule;
    frameParam: TframeParam;
    G2Keyboard: TG2Keyboard;
    rbLocation: TG2BtnRadio;
    framePatch: TframePatch;
    ArrayLayout1: TArrayLayout;
    btRed: TG2BtnText;
    btBlue: TG2BtnText;
    btYellow: TG2BtnText;
    btOrange: TG2BtnText;
    btGreen: TG2BtnText;
    btPurple: TG2BtnText;
    btWhite: TG2BtnText;
    btInfo: TG2BtnText;
    tcPatch: TTabControl;
    TabItem11: TTabItem;
    TabItem12: TTabItem;
    TabItem13: TTabItem;
    TabItem14: TTabItem;
    frameSlot1: TframeSlot;
    frameSlot2: TframeSlot;
    frameSlot3: TframeSlot;
    frameSlot4: TframeSlot;
    lSynths: TLayout;
    procedure btKeyboardSplitChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btPerfModeChangeValue(Sender: TObject; const aValue: Integer);
    procedure ePerfNameExit(Sender: TObject);
    procedure btClockRunChangeValue(Sender: TObject; const aValue: Integer);
    procedure idClockBPMChangeValue(Sender: TObject; const aValue: Integer);
    procedure tfMasterClockGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure tfSynthNameGetTextFunc(Sender: TObject; var aTextFunc: string);
    procedure rbSlotStripChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSplitterChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbLocationChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbZoomChangeValue(Sender: TObject; const aValue: Integer);
    procedure tcPatchFunctionsChange(Sender: TObject);
    procedure btRedChangeValue(Sender: TObject; const aValue: Integer);
    procedure btBlueChangeValue(Sender: TObject; const aValue: Integer);
    procedure btYellowChangeValue(Sender: TObject; const aValue: Integer);
    procedure btOrangeChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGreenChangeValue(Sender: TObject; const aValue: Integer);
    procedure btPurpleChangeValue(Sender: TObject; const aValue: Integer);
    procedure btWhiteChangeValue(Sender: TObject; const aValue: Integer);
    procedure btInfoChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbSynthChangeValue(Sender: TObject; const aValue: Integer);
  private
    // The patch, appsettings and add module frame will move to the selected synth

    [Weak] FSynth : TG2GraphFMX;

    [Weak] FStateStyleList : TG2StateStyleList;

    FG2List : TObjectList<TG2GraphFMX>;
    FSlotList : TList<TframeSlot>;

    FModuleImageList : TObjectDictionary<integer, TBitmap>;

    frameEditorCentralStrip2 : TframeEditorCentralStrip;

    FInitialized : boolean;

    FResponseTimeOut : TTimer;

    FOnShowAppMessage : TAppShowMessageEvent;
    FOnCloseAppMessage : TAppCloseMessageEvent;

    procedure Update(aG2Event: TG2Event);
    procedure SlotOperationModeChange(Sender: TObject);

    procedure DoAfterG2Init;
    procedure DoPerfUpdate;
    procedure DoSelectSlot;

    procedure G2MessageTimeOut(Sender : TObject);

    function GetSelectedSynthIndex: integer;
    procedure SetSelectedSynthIndex(const Value: integer);
    procedure SetOnCloseAppMessage(const Value: TAppCloseMessageEvent);
    procedure SetOnShowAppMessage(const Value: TAppShowMessageEvent);

    procedure AddSynth(const aSynth : TG2GraphFMX);
    function GetSynth: TG2GraphFMX;
    procedure SetSynth(const Value: TG2GraphFMX);
    procedure SelectSynthIndex(aIndex: integer);

    //function GetFrameSlot(const aIndex : integer): TframeSlot;
    function GetSelectedFrameSlot: TFrameSlot;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure DeviceDiscovery;

    procedure Init;

    procedure SelectSlot(const aSlotIndex : integer);
    procedure SelectVariation(const aVariation : integer);
    procedure SelectLocation(const aLocation: TLocationType);

    procedure StartStopClock;

    procedure UpdateControls;
    procedure SlotSetZoomSetting(const aValue: integer);


    procedure ProcessKeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property Synth : TG2GraphFMX read GetSynth write SetSynth;
    property G2List : TObjectList<TG2GraphFMX> read FG2List write FG2List;
    property SelectedSynthIndex : integer read GetSelectedSynthIndex write SetSelectedSynthIndex;
    property OnShowAppMessage : TAppShowMessageEvent read FOnShowAppMessage write SetOnShowAppMessage;
    property OnCloseAppMessage : TAppCloseMessageEvent read FOnCloseAppMessage write SetOnCloseAppMessage;
  end;

implementation

{$R *.fmx}

{ TframeG2EditorCentralStrip }

constructor TframeEditorCentralStrip.Create(AOwner: TComponent);
begin
  inherited;

  FInitialized := False;

  FG2List := TObjectList<TG2GraphFMX>.Create(False);
  FSlotList := TList<TframeSlot>.Create;

  FSlotList.Add( frameSlot1);
  FSlotList.Add( frameSlot2);
  FSlotList.Add( frameSlot3);
  FSlotList.Add( frameSlot4);

  FModuleImageList := TObjectDictionary<integer, TBitmap>.Create([doOwnsValues], 170);

  framePatch.frameAppSettings := frameAppSettings;
  frameBanks.frameAppSettings := frameAppSettings;
  frameKnobs.frameAppSettings := frameAppSettings;

  frameSlot1.frameAppSettings := frameAppSettings;
  frameSlot2.frameAppSettings := frameAppSettings;
  frameSlot3.frameAppSettings := frameAppSettings;
  frameSlot4.frameAppSettings := frameAppSettings;

  frameParam.frameAppSettings := frameAppSettings;

  frameSlot1.OnOperationModeChange := SlotOperationModeChange;
  frameAddModule.frameSlot := frameSlot1;
  tcPatch.TabIndex := 0;

  FResponseTimeOut := TTimer.Create(nil);
  FResponseTimeOut.Enabled := False;
  FResponseTimeOut.Interval := 4000;
  FResponseTimeOut.OnTimer := G2MessageTimeOut;
end;

destructor TframeEditorCentralStrip.Destroy;
var i : integer;
begin
  for i := 0 to FG2List.Count - 1 do begin
    FG2List[i].USBActive := False;
  end;
  FSlotList.Free;
  FG2List.Free;

  FResponseTimeOut.Free;

  FModuleImageList.Free;

  inherited;
end;

procedure TframeEditorCentralStrip.DeviceDiscovery;
var i : integer;
    G2DeviceList : TList;
    G2 : TG2GraphFMX;
begin
  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);
    if G2DeviceList.Count > 0 then begin
      for i := 0 to min(G2DeviceList.Count,3) - 1 do begin
        G2 := TG2GraphFMX.Create(self);
        G2.ClientType := ctEditor;
        G2.Host := '127.0.0.1';
        G2.Port := 2501 + FG2List.Count;
        G2.IsServer := True;
        //G2.LogLevel := 1;
        AddSynth( G2);
{$IFDEF MSWINDOWS}
        G2.G2USBDevice := pusb_device(G2DeviceList[i]);
{$ELSE}
        G2.G2USBDevice := Plibusb_device(G2DeviceList[i]));
{$ENDIF}
        SelectSynthIndex(i);
      end;
    end else begin
      G2 := TG2GraphFMX.Create(self);
      G2.ClientType := ctEditor;
      G2.Host := '127.0.0.1';
      G2.Port := 2501;
      G2.IsServer := True;
      AddSynth( G2);
      G2.G2USBDevice := nil;

      {G2 := TG2GraphFMX.Create(self);
      G2.ClientType := ctEditor;
      G2.Host := '127.0.0.1';
      G2.Port := 2502;
      G2.IsServer := True;
      AddSynth( G2);
      G2.G2USBDevice := nil;}

      ShowMessage('No g2 device found');
      SelectSynthIndex(0);
    end;

    // Multiple synth radio button

    rbSynth.ButtonCount := FG2List.Count;
    rbSynth.ButtonText.Clear;

    for i := 0 to FG2List.Count - 1 do
      rbSynth.ButtonText.Add(IntToStr(i+1));
    rbSynth.Width := RBWIDTH *FG2List.Count + 1;

    if FG2List.Count > 1 then begin
      rbSynth.Visible := True;
      lSynth.Position.X := rbSynth.Position.X + rbSynth.Width + SPACER;
    end else begin
      rbSynth.Visible := False;
      lSynth.Position.X := rLed.Position.X + rLed.Width + SPACER;
    end;

  finally
    G2DeviceList.Free;
  end;
end;

procedure TframeEditorCentralStrip.AddSynth(const aSynth: TG2GraphFMX);
var ti : TTabItem;
    frameSlot : TframeSlot;
    i, offs : integer;
begin
  FG2List.Add( aSynth);

  if (FG2List.Count - 1) * 4 >= tcPatch.TabCount then begin

    for i := 0 to 3 do begin

      // Add frameslots

      ti := tcPatch.Add(TTabItem);
      ti.Parent := tcPatch;
      ti.Text := 's' + IntToStr(FG2List.Count - 1) + 't' + IntToStr(i);
      ti.StyleLookup := 'tabdotstyle';

      tcPatch.BeginUpdate;
      try
        frameSlot := TFrameSlot.Create(ti);
        frameSlot.frameAppSettings := frameAppSettings;
        frameSlot.Parent := ti;
        frameSlot.Align := TAlignLayout.alClient;
        frameSlot.Patch := aSynth.Performance.Slot[i].Patch as TG2GraphPatchFMX;
        FSlotList.Add(frameSlot);
      finally
        tcPatch.EndUpdate;
      end;
    end;
  end else begin
    offs := (FG2List.Count - 1) * 4;
    for i := 0 to 3 do begin
      FSlotList[i].Patch := aSynth.Performance.Slot[i].Patch as TG2GraphPatchFMX;
    end;
  end;


  //frameSlot1.Patch := aSynth.Performance.Slot[0].Patch as TG2GraphPatchFMX;
  //frameSlot2.Patch := aSynth.Performance.Slot[1].Patch as TG2GraphPatchFMX;
  //frameSlot3.Patch := aSynth.Performance.Slot[2].Patch as TG2GraphPatchFMX;
  //frameSlot4.Patch := aSynth.Performance.Slot[3].Patch as TG2GraphPatchFMX;
end;

function TframeEditorCentralStrip.GetSynth: TG2GraphFMX;
begin
  Result := FSynth;
end;

procedure TframeEditorCentralStrip.SetSynth(const Value: TG2GraphFMX);
var i : integer;
begin
  if not assigned(Value) then
    exit;

  if Value <> FSynth then begin

    if assigned(FSynth) then begin
      FSynth.RemoveObserver( self);
      FSynth.Performance.RemoveObserver( self);
      for i := 0 to 3 do begin
        FSynth.GetSlot(i).RemoveObserver(self);
        FSynth.GetSlot(i).Patch.RemoveObserver(self);
      end;
    end;

    FSynth := Value;

    if assigned(FSynth) then begin
      FSynth.RegisterObserver( self);
      FSynth.Performance.RegisterObserver( self);
      for i := 0 to 3 do begin
        FSynth.GetSlot(i).RegisterObserver(self);
        FSynth.GetSlot(i).Patch.RegisterObserver(self);
      end;

    end;

    //FSynth.OnLogLine := G2LogLine;

    //frameSlot1.Patch := FSynth.Performance.Slot[0].Patch as TG2GraphPatchFMX;
    //frameSlot2.Patch := FSynth.Performance.Slot[1].Patch as TG2GraphPatchFMX;
    //frameSlot3.Patch := FSynth.Performance.Slot[2].Patch as TG2GraphPatchFMX;
    //frameSlot4.Patch := FSynth.Performance.Slot[3].Patch as TG2GraphPatchFMX;

    frameSlotStrip1.Slot := FSynth.Performance.Slot[0] as TG2GraphSlotFMX;
    frameSlotStrip2.Slot := FSynth.Performance.Slot[1] as TG2GraphSlotFMX;
    frameSlotStrip3.Slot := FSynth.Performance.Slot[2] as TG2GraphSlotFMX;
    frameSlotStrip4.Slot := FSynth.Performance.Slot[3] as TG2GraphSlotFMX;

    framePatchSettings.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;

    frameSynthSettings.Synth := FSynth;
    framePatch.Synth := FSynth;
    frameBanks.Synth := FSynth;
    frameKnobs.Synth := FSynth;
    frameParam.Synth := FSynth;
    //frameLog.Synth := FSYnth;
  end;
end;

procedure TframeEditorCentralStrip.DoAfterG2Init;
var i : integer;
begin
  if not assigned(FSynth) then
    exit;

  UpdateControls;

  for i := 0 to 3 do begin
    FSynth.Performance.Slot[i].Patch.InvalidateParameters;
  end;

  //if tcFrame.TabIndex < 4 then
  //  tcFrame.TabIndex := FSynth.Performance.SelectedSlot;

  frameAppSettings.UpdateControls;
  frameBanks.UpdateControls;
  frameKnobs.UpdateControls;
  frameModule.UpdateControls;
end;

procedure TframeEditorCentralStrip.DoPerfUpdate;
begin
  if FSynth.Performance.KeyboardRangeEnabled  = 1 then
    G2Keyboard.State := csDefault
  else
    G2Keyboard.State := csDisabled;

  DoSelectSlot;
end;

procedure TframeEditorCentralStrip.DoSelectSlot;
var i : integer;
    frameSlot : TFrameSlot;
begin
  //if tcFrame.TabIndex < 4 then
  //  tcFrame.TabIndex := Synth.SelectedSlot.SlotIndex;

  for i := 0 to (4*FG2List.Count)-1 do begin
    frameSlot := FSlotList[ i];
    if (rbSynth.Value = i div 4) and  (i mod 4 = FSynth.SelectedSlot.SlotIndex) then begin
      frameSlot.OnOperationModeChange := SlotOperationModeChange;
      frameAddModule.frameSlot := frameSlot;
      frameModule.frameSlot := frameSlot;
      tcPatch.TabIndex := i;
    end else begin
      frameSlot.OnOperationModeChange := nil;
    end;
  end;

  //frameSlot1.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;
  //frameAddModule.frameSlot := frameSlot1;
  //frameModule.frameSlot := frameSlot1;

  framePatchSettings.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;
  frameParam.Parameter := FSynth.SelectedPatchPart.SelectedParam as TG2GraphParameterFMX;

  UpdateControls;
end;

procedure TframeEditorCentralStrip.ePerfNameExit(Sender: TObject);
begin
  (FSynth.Performance as TG2USBPerformance).SendSetPerfNameMessage( ePerfName.Text);
end;

procedure TframeEditorCentralStrip.Init;
var G2Synth : TG2GraphFMX;
begin
  if assigned(FOnShowAppMessage) then
    FOnShowAppMessage(Self, 'Loading app settings...');

  if assigned(FOnShowAppMessage) then
     FOnShowAppMessage(Self, 'Creating module images...');

{$IF not Defined(Android)}
  CreateModuleImages(FModuleImageList);
{$ENDIF}

  // Only one patch file frame needed
  for G2Synth in FG2List do
    frameAppSettings.AddSynth( G2Synth.SynthName, G2Synth);

  frameAppSettings.LoadSettings;

  //frameSynth.Init;

  frameAddModule.ModuleImageList := FModuleImageList;
  frameAddModule.UpdateControls;

  if assigned(FOnShowAppMessage) then
    FOnShowAppMessage(Self, 'Scanning patch folder...');

  try
    framePatch.UpdateFiles;
  except on E:Exception do
    ShowMessage(E.Message);
  end;

  FInitialized := True;

  SelectSynthIndex( rbSynth.Value);
end;

procedure TframeEditorCentralStrip.G2MessageTimeOut(Sender: TObject);
begin
  FResponseTimeOut.Enabled := False;
  rLed.Fill.Color := claRed;
end;

{function TframeEditorCentralStrip.GetFrameSlot(
  const aIndex: integer): TframeSlot;
begin
  Result := nil;
  case aIndex of
  0 : Result := frameSlot1;
  1 : Result := frameSlot2;
  2 : Result := frameSlot3;
  3 : Result := frameSlot4;
  end;
end;}

function TframeEditorCentralStrip.GetSelectedFrameSlot: TFrameSlot;
begin
  Result := nil;

  if FG2List.Count > 0 then
    Result := FSlotList[rbSynth.Value * 4 + FSynth.SelectedSlot.SlotIndex];
end;

function TframeEditorCentralStrip.GetSelectedSynthIndex: integer;
begin
  Result := rbSynth.Value;
end;

procedure TframeEditorCentralStrip.SetSelectedSynthIndex(const Value: integer);
begin
  if Value <> rbSynth.Value then begin
    rbSynth.Value := Value;
  end;
end;

procedure TframeEditorCentralStrip.SlotSetZoomSetting(const aValue: integer);
var frameSlot : TFrameSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  if not assigned(frameSlot) then
    exit;

  frameSlot.Patch.ZoomSetting := aValue;
  frameSlot.UpdateControls;
end;

procedure TframeEditorCentralStrip.Update(aG2Event: TG2Event);
begin
  case aG2Event of
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
    EvtAfterG2Init:
      begin
        DoAfterG2Init;
      end;
    EvtAfterPerfInit:
      begin
        //DoAfterPerfInit;
      end;
    EvtAfterSlotInit:
      begin
        //DoAfterSlotInit;
      end;
    EvtPerfsSettingsUpdate:
      begin
        DoPerfUpdate;
      end;
    EvtPerfUpdate:
      begin
        DoPerfUpdate;
      end;
    EvtSynthSettingsUpdate: ;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate:
      begin
        UpdateControls;
      end;
    EvtVariationChange: ;
    EvtCopyVariation: ;
    EvtMidiClockReceive: ;
    EvtClockRunChange:
      begin
        if Synth.Performance.MasterClockRun = 0 then begin
          btClockRun.Value := 0;
          //MasterClockBPM := Synth.Performance.MasterClock;
        end else begin
          btClockRun.Value := 1;
        end;
        btClockRun.Redraw;
      end;
    EvtClockBPMChange:
      begin
      end;
    EvtMidiCCRecieve:
      begin
        frameParam.UpdateAssignCCButton( (Synth.Performance as TG2MessPerformance).LastMidiCC);
      end;
    EvtAfterGetAssignedVoices: ;
    EvtPatchLoadChange: ;
    EvtSelectSlot:
      begin
        DoSelectSlot;
      end;
    EvtSelectLocation:
      begin
        if Synth.SelectedSlot.Patch.SelectedLocation in [ltFX, ltVA] then
          rbLocation.Value := ord(Synth.SelectedSlot.Patch.SelectedLocation);
      end;
    EvtSelectModule: ;
    EvtSelectParam:
      begin
        //frameParam.Parameter := Synth.SelectedSlot.Patch.PatchPart[ ord(Synth.SelectedSlot.Patch.SelectedLocation)].SelectedParam as TG2GraphParameterFMX;
        frameParam.Parameter := Synth.SelectedPatchPart.SelectedParam as TG2GraphParameterFMX;
      end;
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

procedure TframeEditorCentralStrip.SlotOperationModeChange(Sender: TObject);
var frameSlot : TFrameSlot;
begin
  if not (Sender is TFrameSlot) then
    exit;

  frameSlot := Sender as TFrameSlot;

  case frameSlot.OperationMode of
    omModule :
      begin
        tcPatchFunctions.TabIndex := 1;
        frameModule.UpdateControls;
      end;
    omParam :
      begin
        tcPatchFunctions.TabIndex := 2;
        FrameParam.UpdateControls;
      end;
  end;
end;

procedure TframeEditorCentralStrip.UpdateControls;
var frameSlot : TFrameSlot;
begin
  tfSynthName.Redraw;
  ePerfName.Text := FSynth.Performance.PerformanceName;
  idClockBPM.LowValue := 30;
  idClockBPM.HighValue := 240;
  idClockBPM.Value := FSynth.Performance.MasterClock;
  idClockBPM.Redraw;
  btClockRun.Value := FSynth.Performance.MasterClockRun;
  //MasterClockBPM := idClockBPM.Value;
  btClockRun.Redraw;
  btPerfMode.Value := FSynth.PerfMode;
  btKeyboardSplit.Value := FSynth.Performance.KeyboardRangeEnabled;
  if FSynth.PerfMode = 1 then
    ePerfName.Enabled := True
  else
    ePerfname.Enabled := False;

  rbSlotStrip.Value := FSynth.Performance.SelectedSlot;

  G2Keyboard.SelectedLowKey := FSynth.SelectedSlot.Lower - 24;
  G2Keyboard.SelectedHighKey := FSynth.SelectedSlot.Upper - 24;

  frameSlot := GetSelectedFrameSlot;

  if assigned(frameSlot) and assigned(frameSlot.Patch) then begin
    rbZoom.Value := frameSlot.Patch.ZoomSetting;

    if frameSlot.Patch.SplitterVisible then
      btSplitter.Value := 1
    else
      btSplitter.Value := 0;

    case frameSlot.Patch.SelectedLocation of
    ltFX : rbLocation.Value := 0;
    ltVA : rbLocation.Value := 1;
    end;

    btRed.Value := frameSlot.Patch.PatchDescription.RedVisible;
    btBlue.Value := frameSlot.Patch.PatchDescription.BlueVisible;
    btYellow.Value := frameSlot.Patch.PatchDescription.YellowVisible;
    btOrange.Value := frameSlot.Patch.PatchDescription.OrangeVisible;
    btGreen.Value := frameSlot.Patch.PatchDescription.GreenVisible;
    btPurple.Value := frameSlot.Patch.PatchDescription.PurpleVisible;
    btWhite.Value := frameSlot.Patch.PatchDescription.WhiteVisible;
  end;
end;

procedure TframeEditorCentralStrip.ProcessKeyDown(var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var C : Char;
    frameSlot : TFrameSlot;
begin
  if not assigned(FSynth) then
    exit;

  frameSlot := GetSelectedFrameSlot;

  case Key of
  vkDelete :
    begin
      if frameSlot.OperationMode = omDrawCable then
        frameSlot.DeleteAllCables
      else
        frameModule.Delete;
    end;
  vkReturn :
    if ssCtrl in Shift then
      frameAddModule.InsertModule;
  vkTab :
    if ssShift in Shift then
      frameAddModule.PrevModulePage
    else
      frameAddModule.NextModulePage;
  vkLeft :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleLeft;
      if not(ssShift in Shift) then
        AdvanceFocus( GetFocusedControl(self), False);
    end;
  vkRight :
     begin
       if ssShift in Shift then
         FSynth.SelectedPatchPart.SelectModuleRight;
       if not(ssShift in Shift) then
         AdvanceFocus( GetFocusedControl(self), True);
     end;
  vkUp :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleAbove;
      if ssCtrl in Shift then
        if assigned(FSynth.SelectedPatchPart.SelectedParam) then
          FSynth.SelectedPatchPart.SelectedParam.IncMorphValue;
    end;
  vkDown :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleUnder;
      if ssCtrl in Shift then
        if assigned(FSynth.SelectedPatchPart.SelectedParam) then
          FSynth.SelectedPatchPart.SelectedParam.DecMorphValue;
    end;
  vkControl :
    begin
      if assigned(frameSlot) then begin
        frameSlot.DisablePan(frameSlot.sbVA);
        frameSlot.DisablePan(frameSlot.sbFX);
       end;
       KeyChar := Char(#0);
     end;
  else begin
    if Key = 0 then
      C := Upcase(KeyChar)
    else begin
      C := Char(Key);
    end;

    case C of
    'A' : begin
            if ssCtrl in Shift then
              FSynth.SelectedPatchPart.SelectAll
            else
              SelectSlot(0);
          end;
    'B' : begin
            if Shift = [ssCtrl]  then
              tcFrame.TabIndex := ord(ftBanks)
            else
              SelectSlot(1);
          end;
    'C' : begin
            if ssCtrl in Shift then
              frameModule.Copy
            else
              SelectSlot(2);
          end;
    'D' : SelectSlot(3);
    'E' : begin
            if ssCtrl in Shift then
              frameModule.PasteParams;
          end;
    'F' : begin
            if ssCtrl in Shift then
              tcFrame.TabIndex := ord(ftKnobs)
            else
              SelectLocation(ltFX);
          end;
    'G' : begin
            if ssCtrl in Shift then
              tcFrame.TabIndex := ord(ftSynthSettings);
          end;
    'N' : begin
            if ssCtrl in Shift then begin
              FSynth.SelectedSlot.Patch.Init;
              FSynth.SelectedSlot.SendSetPatchMessage('No name', FSynth.SelectedSlot.Patch);
              tcFrame.TabIndex := FSynth.SelectedSlot.SlotIndex;
            end;
          end;
    'O' : begin
            if ssCtrl in Shift then
              tcPatchFunctions.TabIndex := ord(ptPatch);
          end;
    'P' : begin
            if ssCtrl in Shift then
              tcFrame.TabIndex := ord(ftPatchSettings);
          end;
    'Q' : begin
            if ssCtrl in Shift then
              Application.Terminate;
          end;
    'R' : StartStopClock;
    'S' : begin
            if ssCtrl in Shift then
              tcPatchFunctions.TabIndex := ord(ptPatch);
          end;
    'V' : begin
            if ssCtrl in Shift then
              frameModule.Paste
            else
              SelectLocation(ltVA);
          end;
    'X' : begin
            if ssCtrl in Shift then
              frameModule.Cut
            else
              SelectLocation(ltVA);
          end;
    'Z' : begin
            if ssCtrl in Shift then
              frameModule.Undo;
          end;
    '1' : SelectVariation(0);
    '2' : SelectVariation(1);
    '3' : SelectVariation(2);
    '4' : SelectVariation(3);
    '5' : SelectVariation(4);
    '6' : SelectVariation(5);
    '7' : SelectVariation(6);
    '8' : SelectVariation(7);
    '>', '.' : frameAddModule.NextTemplateModule;
    '<', ',' : frameAddModule.PrevTemplateModule;
    '}', ']' : frameAddModule.NextModulePage;
    '{', '[' : frameAddModule.PrevModulePage;
    end;
    end;
  end;
end;

procedure TframeEditorCentralStrip.btPerfModeChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.PerfMode := aValue;
end;

procedure TframeEditorCentralStrip.btRedChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.RedVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btBlueChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.BlueVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btYellowChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.YellowVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btOrangeChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.OrangeVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btGreenChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.GreenVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btPurpleChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.PurpleVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btWhiteChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.WhiteVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeEditorCentralStrip.btSplitterChangeValue(Sender: TObject;
  const aValue: Integer);
var frameSlot : TFrameSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  if not assigned(frameSlot) then
    exit;

  frameSlot.Patch.SplitterVisible := aValue = 1;
  frameSlot.UpdateSplitter;
end;

procedure TframeEditorCentralStrip.btClockRunChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.MasterClockRun := btClockRun.Value;
end;

procedure TframeEditorCentralStrip.btKeyboardSplitChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.KeyboardRangeEnabled := aValue;
  (FSynth.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;

procedure TframeEditorCentralStrip.idClockBPMChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  FSynth.Performance.MasterClock := idClockBPM.Value;
  tfMasterClock.Redraw;
end;

procedure TframeEditorCentralStrip.btInfoChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 1 then begin
    if assigned(FOnShowAppMessage) then
      FOnShowAppMessage(self, 'An alternative editor for the Nord G2')
  end else
    if assigned(FOnCloseAppMessage) then
      FOnCloseAppMessage(self);
end;

procedure TframeEditorCentralStrip.tcPatchFunctionsChange(Sender: TObject);
var frameSlot : TFrameSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  if not assigned(frameSlot) then
    exit;

  if tcPatchFunctions.TabIndex = 0 then
    frameSlot.ActivateAddModule
  else begin
    //
  end;
end;

procedure TframeEditorCentralStrip.tfMasterClockGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if assigned(FSynth) then
    aTextFunc := IntToStr(FSynth.Performance.MasterClock)
  else
    aTextFunc := '';
end;

procedure TframeEditorCentralStrip.tfSynthNameGetTextFunc(Sender: TObject;
  var aTextFunc: string);
begin
  if assigned(FSynth) then
    aTextFunc := FSynth.SynthName
  else
    aTextFunc := 'No synth';
end;

procedure TframeEditorCentralStrip.rbLocationChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0 : SelectLocation(ltFX);
    1 : SelectLocation(ltVA);
  end;
end;

procedure TframeEditorCentralStrip.rbSlotStripChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : begin
        frameSlotStrip1.Visible := True;
        frameSlotStrip2.Visible := False;
        frameSlotStrip3.Visible := False;
        frameSlotStrip4.Visible := False;
        lSlotStrips.Height := 35;
        FSynth.Performance.SelectedSlot := 0;
      end;
  1 : begin
        frameSlotStrip1.Visible := False;
        frameSlotStrip2.Visible := True;
        frameSlotStrip3.Visible := False;
        frameSlotStrip4.Visible := False;
        lSlotStrips.Height := 35;
        FSynth.Performance.SelectedSlot := 1;
      end;
  2 : begin
        frameSlotStrip1.Visible := False;
        frameSlotStrip2.Visible := False;
        frameSlotStrip3.Visible := True;
        frameSlotStrip4.Visible := False;
        lSlotStrips.Height := 35;
        FSynth.Performance.SelectedSlot := 2;
      end;
  3 : begin
        frameSlotStrip1.Visible := False;
        frameSlotStrip2.Visible := False;
        frameSlotStrip3.Visible := False;
        frameSlotStrip4.Visible := True;
        lSlotStrips.Height := 35;
        FSynth.Performance.SelectedSlot := 3;
      end;
  4 : begin
        lSlotStrips.Height := 137;
        frameSlotStrip1.Visible := True;
        frameSlotStrip2.Visible := True;
        frameSlotStrip3.Visible := True;
        frameSlotStrip4.Visible := True;
      end;
  end;
end;

procedure TframeEditorCentralStrip.rbSynthChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  SelectSynthIndex( aValue);
end;

procedure TframeEditorCentralStrip.rbZoomChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : SlotSetZoomSetting(0);
  1 : SlotSetZoomSetting(1);
  2 : SlotSetZoomSetting(2);
  end;
end;

procedure TframeEditorCentralStrip.ProcessKeyUp(var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var frameSlot : TFrameSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  if not assigned(frameSlot) then
    exit;

  case Key of
    vkControl :
      begin
        if assigned(frameSlot) then begin
          frameSlot.EnablePan(frameSlot.sbVA);
          frameSlot.EnablePan(frameSlot.sbFX);
         end;
       end;
    end;
end;

procedure TframeEditorCentralStrip.StartStopClock;
begin
  btClockRun.Value := 1 - btClockRun.Value;
  FSynth.Performance.MasterClockRun := btClockRun.Value;
end;

procedure TframeEditorCentralStrip.SelectLocation(
  const aLocation: TLocationType);
begin
  if not assigned(FSynth) then
    exit;

  FSynth.SelectedSlot.Patch.SelectedLocation := aLocation;
end;

procedure TframeEditorCentralStrip.SelectSlot(const aSlotIndex: integer);
begin
  if not assigned(FSynth) then
    exit;

  FSynth.Performance.SelectedSlot := aSlotIndex;
end;

procedure TframeEditorCentralStrip.SelectSynthIndex(aIndex: integer);
begin
  Synth := FG2List[aIndex];
  DoSelectSlot;
end;

procedure TframeEditorCentralStrip.SelectVariation(const aVariation: integer);
begin
  if not assigned(FSynth) then
    exit;

  FSynth.SelectedSlot.SendSelectVariationMessage( aVariation);
end;

procedure TframeEditorCentralStrip.SetOnCloseAppMessage(
  const Value: TAppCloseMessageEvent);
begin
  FOnCloseAppMessage := Value;
end;

procedure TframeEditorCentralStrip.SetOnShowAppMessage(
  const Value: TAppShowMessageEvent);
begin
  FOnShowAppMessage := Value;
end;

procedure TframeEditorCentralStrip.SetStateStyles(
  aStateStyleList: TG2StateStyleList);
begin
  FStateStyleList := aStateStyleList;

  ComponentSetStateStylesRecursive(self, aStateStyleList);

  // Back to default for cable buttons
  btRed.StateStyleList := nil;
  btBlue.StateStyleList := nil;
  btYellow.StateStyleList := nil;
  btOrange.StateStyleList := nil;
  btGreen.StateStyleList := nil;
  btPurple.StateStyleList := nil;
  btWhite.StateStyleList := nil;

  framePatch.SetStateStyles( FStateStyleList);
  frameAddModule.SetStateStyles( FstateStyleList);
  frameAppSettings.SetStateStyles( FStateStyleList);

  frameSlot1.SetStateStyles( aStateStyleList);
  frameSlot2.SetStateStyles( aStateStyleList);
  frameSlot3.SetStateStyles( aStateStyleList);
  frameSlot4.SetStateStyles( aStateStyleList);

  framePatchSettings.SetStateStyles( aStateStyleList);
  frameBanks.SetStateStyles( aStateStyleList);
  frameModule.SetStateStyles( aStateStyleList);
  FrameParam.SetStateStyles( aStateStyleList);
  frameKnobs.SetStateStyles(  aStateStyleList);
  frameSynthSettings.SetStateStyles(  aStateStyleList);
end;

end.
