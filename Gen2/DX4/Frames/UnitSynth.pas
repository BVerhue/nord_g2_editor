unit UnitSynth;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, System.Generics.Collections, System.UIConsts,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts,
  FMX.TabControl, FMX.Edit, FMX.StdCtrls,
  BVE.NMG2Types, BVE.NMG2USB, BVE.NMG2File, BVE.NMG2Mess,
  BVE.NMG2ControlsFMX, BVE.NMG2GraphFMX,
  XMLDoc, XMLIntf,
  UnitParam, UnitModule, UnitAddModule, UnitAppSettings,
  UnitPatchSettings, UnitBanks, UnitSlot, UnitSynthStrip,
  UnitSlotStrip, UnitPatch, UnitKnobs, UnitSynthSettings, UnitMessageDlg,
  UnitLog;

type
  TFrameTabs = (ftSlotA, ftSlotB, ftSlotC, ftSlotD, ftBanks, ftPatchSettings,
                ftKnobs, ftSynthSettings, ftAppSettings);
  TPatchTabs = (ptAddModule, ptModule, ptParam, ptPatch);
  TInitType = (itNone, itAll, itPerf, itSlotA, itSlotB, itSlotC, itSlotD);

  TAppShowMessageEvent = procedure(Sender: TObject; aMessage : string) of object;
  TAppCloseMessageEvent = procedure(Sender: TObject) of object;

  TframeSynth = class(TFrame, IG2Observer)
    lSlotStrips: TLayout;
    Layout1: TLayout;
    rbZoom: TG2BtnRadio;
    rbLocation: TG2BtnRadio;
    btSplitter: TG2BtnText;
    Layout2: TLayout;
    tcFrame: TTabControl;
    tiSlotA: TTabItem;
    Panel1: TPanel;
    frameSlot1: TframeSlot;
    tiSlotB: TTabItem;
    Panel2: TPanel;
    frameSlot2: TframeSlot;
    tiSlotC: TTabItem;
    Panel3: TPanel;
    frameSlot3: TframeSlot;
    tiSlotD: TTabItem;
    Panel4: TPanel;
    frameSlot4: TframeSlot;
    tiBanks: TTabItem;
    tiPatchSettings: TTabItem;
    tiSynthSettings: TTabItem;
    tcPatchFunctions: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    OpenDialog1: TOpenDialog;
    frameSlotStrip1: TframeSlotStrip;
    frameSlotStrip2: TframeSlotStrip;
    frameSlotStrip3: TframeSlotStrip;
    frameSlotStrip4: TframeSlotStrip;
    TabItem4: TTabItem;
    frameBanks1: TframeBanks;
    TabItem5: TTabItem;
    frameKnobs: TframeKnobs;
    frameSynthSettings: TframeSynthSettings;
    framePatchSettings: TframePatchSettings;
    frameModule: TframeModule;
    frameParam: TframeParam;
    G2Keyboard: TG2Keyboard;
    btInfo: TG2BtnText;
    ArrayLayout1: TArrayLayout;
    btCablesRed: TG2BtnText;
    btCablesBlue: TG2BtnText;
    btCablesYellow: TG2BtnText;
    btCablesOrange: TG2BtnText;
    btCablesGreen: TG2BtnText;
    btCablesPurple: TG2BtnText;
    btCablesWhite: TG2BtnText;
    tcLog: TTabItem;
    frameLog: TframeLog;
    procedure rbZoomChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSplitterChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbLocationChangeValue(Sender: TObject; const aValue: Integer);
    procedure tcPatchFunctionsChange(Sender: TObject);
    procedure tcFrameChange(Sender: TObject);
    procedure G2KeyboardKeyboardKeyUp(Sender: TObject; aKeyNo: Integer);
    procedure btInfoChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesRedChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesBlueChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesYellowChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesOrangeChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesGreenChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesPurpleChangeValue(Sender: TObject; const aValue: Integer);
    procedure btCablesWhiteChangeValue(Sender: TObject; const aValue: Integer);
  private
    FSlotStripList : TObjectList<TframeSlotStrip>;
    FSlotList : TObjectList<TframeSlot>;

    [Weak] FSynth : TG2GraphFMX;
    [Weak] FframeSynthStrip : TframeSynthStrip;
    [Weak] FframePatchSettings : TframePatchSettings;
    [Weak] FframePatch : TframePatch;
    [Weak] FframeBanks : TframeBanks;
    [Weak] FframeAppSettings : TframeAppSettings;
    [Weak] FframeAddModule : TframeAddModule;
    [Weak] FframeModule : TframeModule;
    [Weak] FFrameParam : TframeParam;

    //FInitType : TInitType;

    [Weak] FStateStyleList : TG2StateStyleList;

    FOnShowAppMessage : TAppShowMessageEvent;
    FOnCloseAppMessage : TAppCloseMessageEvent;

    procedure Update( aG2Event : TG2Event);

    //procedure G2BeforeSendMessage(Sender: TObject; SenderID: Integer; SendMessage: TG2SendMessage);
    //procedure G2ReceiveResponseMessage(Sender: TObject; ResponseMessage: TMemoryStream);

    procedure G2LogLine(Sender : TObject; const LogLine : string; LogCmd : integer);

    procedure DoAfterG2Init;
    procedure DoPerfUpdate;
    procedure DoSelectSlot;
    //procedure DoAfterSlotInit;
    //procedure DoAfterPerfInit;
    //procedure G2PerfSettingsUpdate(Sender: TObject; SenderID: Integer; PerfMode: Boolean);
    //procedure G2PerfUpdate(Sender: TObject; SenderID: Integer);
    //procedure G2SynthSettingsUpdate(Sender: TObject; SenderID: Integer);
    //procedure G2BeforePatchUpdate(Sender: TObject; SenderID : integer; PatchIndex : integer);
    //procedure G2PatchUpdate(Sender: TObject; SenderID : integer; PatchIndex : integer);
    //procedure G2VariationChange(Sender: TObject; SenderID: Integer; Slot, Variation: Integer);
    //procedure G2CopyVariation(Sender: TObject; SenderID : integer; Slot, FromVariation, ToVariation : integer);

    //procedure G2MidiClockReceive(Sender : TObject; SenderID : integer; BPM : integer);
    //procedure G2ClockRunChange( Sender : TObject; SenderID : integer; Status : boolean);
    //procedure G2ClockBPMChange( Sender : TObject; SenderID : integer; BPM : integer);
    //procedure G2MidiCCReceive(Sender: TObject; SenderID: Integer; MidiCC: Byte);
    //procedure G2AfterGetAssignedVoices(Sender : TObject);
    //procedure G2PatchLoadChange( Sender : TObject; SenderID : integer; Slot : byte);

    //procedure G2SelectSlot(Sender: TObject; SenderID : integer; Slot : integer);
    //procedure G2SelectLocation(Sender : TObject; SenderID : integer; Slot : byte; Location : TLocationType);
    //procedure G2SelectModule(Sender : TObject; SenderID : integer; Module : TG2FileModule);
    //procedure G2SelectParam(Sender : TObject; SenderID : integer; Param : TG2FileParameter);
    //procedure G2LabelValueChange(Sender : TObject; SenderID : integer; Param : TG2FileParameter);
    //procedure G2MorphChange(Sender: TObject; SenderID : integer; Slot : byte; Location : TLocationType; ModuleIndex, ParamIndex, aMorphIndex, aVariationIndex, aValue : byte);

    //procedure G2DeleteModule(Sender : TObject; SenderID : integer; Location: TLocationType; ModuleIndex : integer);

    //procedure G2AfterRetrievePatch(Sender: TObject; SenderID: Integer; aSlot, aBank, aPatch: Byte);
    //procedure G2AfterBankList(Sender: TObject; SenderID: Integer);
    //procedure G2AfterStore(Sender: TObject; SenderID: Integer; SlotIndex, BankIndex, PatchIndex : byte);
    //procedure G2AfterClear(Sender: TObject; SenderID: Integer; PatchFileType : TPatchFileType; BankIndex, PatchIndex : byte);
    //procedure G2AfterClearBank(Sender: TObject; SenderID: Integer; PatchFileType : TPatchFileType; BankIndex : byte);
    //procedure G2AfterBankDownload( Sender : TObject; SenderID : integer; PatchFileType : TPatchFileType);

    //procedure G2DeassignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    //procedure G2AssignKnob(Sender: TObject; SenderID: Integer; Slot : byte; KnobIndex: Integer);
    //procedure G2DeassignGlobalKnob(Sender: TObject; SenderID, KnobIndex: Integer);
    //procedure G2AssignGlobalKnob(Sender: TObject; SenderID, KnobIndex: Integer);

    procedure SlotUpdateSettings(Sender : TObject);
    procedure SlotOperationModeChange(Sender : TObject);

    function GetSelectedFrameSlot: TframeSlot;
    function GetSynth: TG2GraphFMX;
    procedure SetFrameSynthStrip(const Value: TframeSynthStrip);
    procedure SetSynth(const Value: TG2GraphFMX);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Init;

    //procedure CalcLayoutDimensions;

    procedure SetFrameAppSettings( aValue : TframeAppSettings);
    procedure SetFramePatch( aValue : TframePatch);
    procedure SetFrameAddModule( aValue : TframeAddModule);

    procedure UpdateControls;

    procedure SlotSetZoomSetting(const aValue : integer);

    procedure LoadPatch;

    procedure SelectSlot(const aSlotIndex : integer);
    procedure SelectVariation(const aVariation : integer);
    procedure SelectLocation(const aLocation: TLocationType);

    procedure StartStopClock;

    procedure ProcessKeyDown(var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ProcessKeyUp(var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);
    procedure AppSettingsUpdate;

    property Synth : TG2GraphFMX read GetSynth write SetSynth;
    property FrameSynthStrip : TframeSynthStrip read FframeSynthStrip write SetFrameSynthStrip;
    property SelectedFrameSlot : TframeSlot read GetSelectedFrameSlot;

    property OnShowAppMessage : TAppShowMessageEvent read FOnShowAppMessage write FOnShowAppMessage;
    property OnCloseAppMessage : TAppCloseMessageEvent read FOnCloseAppMessage write FOnCloseAppMessage;
  end;

implementation

{$R *.fmx}

{procedure TframeSynth.CalcLayoutDimensions;
var i : integer;
begin
  for i := 0 to FSlotList.Count - 1 do
    FSlotList[i].CalcLayoutDimensions;
end;}

constructor TframeSynth.Create(AOwner: TComponent);
var i : integer;
begin
  inherited;

  //FInitType := itNone;

  FSlotStripList := TObjectList<TframeSlotStrip>.Create(False);
  FSlotList := TObjectList<TframeSlot>.Create(False);

  FSlotStripList.Add( frameSlotStrip1);
  FSlotStripList.Add( frameSlotStrip2);
  FSlotStripList.Add( frameSlotStrip3);
  FSlotStripList.Add( frameSlotStrip4);

  FSlotList.Add( frameSlot1);
  FSlotList.Add( frameSlot2);
  FSlotList.Add( frameSlot3);
  FSlotList.Add( frameSlot4);

  // Can be reassigned (shared among synths)
  //SetframeAppSettings( frameAppSettings1);
  //SetframePatch( framePatch);
  //SetframeAddModule( frameAddModule);
  FframeAppSettings := nil;
  FframePatch := nil;
  FframeAddModule := nil;

  FframeBanks := frameBanks1;
  FframePatchSettings := framePatchSettings;

  FframeModule := frameModule;
  FFrameParam := frameParam;

  for i := 0 to FSlotList.Count - 1 do begin
    FSlotList[i].OnOperationModeChange := SlotOperationModeChange;
  end;

end;

destructor TframeSynth.Destroy;
begin
  FSlotStripList.Free;
  FSlotList.Free;

  inherited;
end;

function TframeSynth.GetSynth: TG2GraphFMX;
begin
  Result := FSynth;
end;

procedure TframeSynth.Init;
var i : integer;
begin
  // After loading ini
  for i := 0 to FSlotList.Count - 1 do
    FSlotList[i].UpdateSplitter;
  SlotUpdateSettings(self);

  if assigned(FSynth) then begin
    FSynth.KnobControl := FframeAppSettings.KnobControl;
    FSynth.CableStyle := FframeAppSettings.CableStyle;
  end;
end;

procedure TframeSynth.SetStateStyles(aStateStyleList: TG2StateStyleList);
var i : integer;
begin
  FStateStyleList := aStateStyleList;

  rbZoom.StateStyleList := aStateStyleList;
  btSplitter.StateStyleList := aStateStyleList;
  rbLocation.StateStyleList := aStateStyleList;
  btInfo.StateStyleList := aStateStyleList;

  for i := 0 to FSlotStripList.Count - 1 do
    FSlotStripList[i].SetStateStyles( aStateStyleList);

  for i := 0 to FSlotList.Count - 1 do
    FSlotList[i].SetStateStyles( aStateStyleList);

  G2Keyboard.StateStyleList := aStateStyleList;

  //FframeAddModule.SetStateStyles( aStateStyleList);
  //FframeAppSettings.SetStateStyles( aStateStyleList);
  //FframePatch.SetStateStyles( aStateStyleList);
  if assigned(FframeSynthStrip) then
    FframeSynthStrip.SetStateStyles( aStateStyleList);
  FframePatchSettings.SetStateStyles( aStateStyleList);
  FframeBanks.SetStateStyles( aStateStyleList);
  FframeModule.SetStateStyles( aStateStyleList);
  FFrameParam.SetStateStyles( aStateStyleList);
  frameKnobs.SetStateStyles(  aStateStyleList);
  frameSynthSettings.SetStateStyles(  aStateStyleList);
end;

procedure TframeSynth.SetSynth(const Value: TG2GraphFMX);
var i : integer;
begin
  if not assigned(Value) then
    exit;

  if Value <> FSynth then begin

    if assigned(FSynth) then begin
      FSynth.RemoveObserver( self);
      FSynth.RemoveObserver( self);
      for i := 0 to 3 do
        FSynth.GetSlot(i).Patch.RemoveObserver(self);
    end;

    FSynth := Value;

    if assigned(FSynth) then begin
      FSynth.RegisterObserver( self);
      FSynth.Performance.RegisterObserver( self);
      for i := 0 to 3 do
        FSynth.GetSlot(i).Patch.RegisterObserver(self);

    end;

    //FSynth.OnBeforeSendMessage := G2BeforeSendMessage;
    //FSynth.OnReceiveResponseMessage := G2ReceiveResponseMessage;
    FSynth.OnLogLine := G2LogLine;

    //FSynth.OnAfterG2Init := G2AfterG2Init;
    //FSynth.OnPerfSettingsUpdate := G2PerfSettingsUpdate;
    //FSynth.OnPerfUpdate := G2PerfUpdate;
    //FSynth.OnSynthSettingsUpdate := G2SynthSettingsUpdate;
    //FSynth.OnBeforePatchUpdate := G2BeforePatchUpdate;
    //FSynth.OnPatchUpdate := G2PatchUpdate;

    //FSynth.OnSelectSlot := G2SelectSlot;
    //FSynth.OnSelectLocation := G2SelectLocation;
    //FSynth.OnSelectModule := G2SelectModule;
    //FSynth.OnSelectParam := G2SelectParam;
    //FSynth.OnLabelValueChange := G2LabelValueChange;
    //FSynth.OnMorphChange := G2MorphChange;

    //FSynth.OnDeleteModule := G2DeleteModule;

    //FSynth.OnAfterGetAssignedVoices := G2AfterGetAssignedVoices;
    //FSynth.OnPatchLoadChange := G2PatchLoadChange;

    //FSynth.OnVariationChange := G2VariationChange;
    //FSynth.OnCopyVariation := G2CopyVariation;

    //FSynth.OnMidiClockReceive := G2MidiClockReceive;
    //FSynth.OnClockRunChange := G2ClockRunChange;
    //FSynth.OnClockBPMChange := G2ClockBPMChange;
    //FSynth.OnMidiCCReceive := G2MidiCCReceive;

    //FSynth.OnAfterBankList := G2AfterBankList;
    //FSynth.OnAfterStore := G2AfterStore;
    //FSynth.OnAfterClear := G2AfterClear;
    //FSynth.OnAfterClearBank := G2AfterClearBank;
    //FSynth.OnAfterBankDownload := G2AfterBankDownload;
    //FSynth.OnAfterRetrievePatch := G2AfterRetrievePatch;

    //FSynth.OnAssignKnob := G2AssignKnob;
    //FSynth.OnDeassignKnob := G2DeassignKnob;
    //FSynth.OnAssignGlobalKnob := G2AssignGlobalKnob;
    //FSynth.OnDeassignGlobalKnob := G2DeassignGlobalKnob;

    frameSlot1.Patch := FSynth.Performance.Slot[0].Patch as TG2GraphPatchFMX;
    frameSlot2.Patch := FSynth.Performance.Slot[1].Patch as TG2GraphPatchFMX;
    frameSlot3.Patch := FSynth.Performance.Slot[2].Patch as TG2GraphPatchFMX;
    frameSlot4.Patch := FSynth.Performance.Slot[3].Patch as TG2GraphPatchFMX;

    frameSlotStrip1.Slot := FSynth.Performance.Slot[0] as TG2GraphSlotFMX;
    frameSlotStrip2.Slot := FSynth.Performance.Slot[1] as TG2GraphSlotFMX;
    frameSlotStrip3.Slot := FSynth.Performance.Slot[2] as TG2GraphSlotFMX;
    frameSlotStrip4.Slot := FSynth.Performance.Slot[3] as TG2GraphSlotFMX;

    framePatchSettings.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;

    frameSynthSettings.Synth := FSynth;
    frameBanks1.Synth := FSynth;
    frameKnobs.Synth := FSynth;
    frameLog.Synth := FSYnth;
    //framePatch.Synth := FSynth;

    //G2SelectSlot(FSynth, FSynth.ID, 0);
  end;
end;

procedure TframeSynth.SetFrameAddModule(aValue: TframeAddModule);
begin
  if assigned(FframeAddModule) then
    if aValue <> FframeAddModule then begin
      //FreeAndNil(FFrameAddModule);
      FFrameAddModule.DisposeOf;
    end;

  FFrameAddModule := aValue;
end;

procedure TframeSynth.SetFrameAppSettings(aValue: TframeAppSettings);
var i : integer;
begin
  if assigned(FframeAppSettings) then
    if aValue <> FframeAppSettings then begin
      //FreeAndNil(FframeAppSettings);
      FframeAppSettings.DisposeOf;
    end;

  FframeAppSettings := aValue;

  for i := 0 to FSlotList.Count - 1 do begin
    FSlotList[i].frameAppSettings := FframeAppSettings;
  end;
  if assigned(FframeBanks) then
    FframeBanks.frameAppSettings := FframeAppSettings;

  if assigned(FframePatch) then
    FframePatch.frameAppSettings := FframeAppSettings;

  FrameKnobs.frameAppSettings := FFrameAppSettings;
  FrameParam.frameAppSettings := FFrameAppSettings;
end;

procedure TframeSynth.AppSettingsUpdate;
var i : integer;
begin
  btSplitter.Value := FframeAppSettings.SplitterVisible;
  for i := 0 to FSlotList.Count - 1 do begin
    FSlotList[i].Patch.SplitterVisible := btSplitter.Value = 1;
    FSlotList[i].UpdateSplitter;
  end;
end;

procedure TframeSynth.SetFramePatch(aValue: TframePatch);
begin
  if assigned(FframePatch) then
    if aValue <> FframePatch then begin
      //FreeAndNil(FframePatch);
      FframePatch.DisposeOf;
    end;

  FframePatch := aValue;
end;

procedure TframeSynth.SetFrameSynthStrip(const Value: TframeSynthStrip);
begin
  if FframeSynthStrip <> Value then begin
    if assigned(Value) then begin
      FframeSynthStrip := Value;
      //frameSlot1.frameSynthStrip := FframeSynthStrip;
      //frameSlot2.frameSynthStrip := FframeSynthStrip;
      //frameSlot3.frameSynthStrip := FframeSynthStrip;
      //frameSlot4.frameSynthStrip := FframeSynthStrip;

      //frameParam.frameSynthStrip := FframeSynthStrip;
    end;
  end;
end;

function TframeSynth.GetSelectedFrameSlot: TframeSlot;
begin
  if assigned(FSynth) then
    Result := FSlotList[ FSynth.Performance.SelectedSlot]
  else
    Result := nil;;
end;

procedure TframeSynth.ProcessKeyDown(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var frameSlot : TframeSlot;
    C : Char;
begin
  if not assigned(FSynth) then
    exit;

  case Key of
  vkDelete :
    begin
      if SelectedFrameSlot.OperationMode = omDrawCable then
        SelectedFrameSlot.DeleteAllCables
      else
        frameModule.Delete;
    end;
  vkReturn :
    if ssCtrl in Shift then
      FframeAddModule.InsertModule;
  vkTab :
    if ssShift in Shift then
      FframeAddModule.PrevModulePage
    else
      FframeAddModule.NextModulePage;
  vkLeft :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleLeft;
      if not(ssShift in Shift) then
        //(FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).SelectPrevModuleControl;
        AdvanceFocus( GetFocusedControl(self), False);
    end;
  vkRight :
     begin
       if ssShift in Shift then
         FSynth.SelectedPatchPart.SelectModuleRight;
       if not(ssShift in Shift) then
         //(FSynth.SelectedSlot.Patch as TG2GraphPatchFMX).SelectNextModuleControl;
         AdvanceFocus( GetFocusedControl(self), True);
     end;
  vkUp :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleAbove;
      //if not(ssShift in Shift) and not(ssCtrl in Shift) then
      //  if assigned(FSynth.SelectedPatchPart.SelectedParam) then
      //    FSynth.SelectedPatchPart.SelectedParam.IncValue;
      if ssCtrl in Shift then
        if assigned(FSynth.SelectedPatchPart.SelectedParam) then
          FSynth.SelectedPatchPart.SelectedParam.IncMorphValue;
    end;
  vkDown :
    begin
      if ssShift in Shift then
        FSynth.SelectedPatchPart.SelectModuleUnder;
      //if not(ssShift in Shift) and not(ssCtrl in Shift) then
      //  if assigned(FSynth.SelectedPatchPart.SelectedParam) then
      //    FSynth.SelectedPatchPart.SelectedParam.DecValue;
      if ssCtrl in Shift then
        if assigned(FSynth.SelectedPatchPart.SelectedParam) then
          FSynth.SelectedPatchPart.SelectedParam.DecMorphValue;
    end;
  vkControl :
    begin
      frameslot := SelectedFrameSlot;

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
    '>', '.' : FframeAddModule.NextTemplateModule;
    '<', ',' : FframeAddModule.PrevTemplateModule;
    '}', ']' : FframeAddModule.NextModulePage;
    '{', '[' : FframeAddModule.PrevModulePage;
    end;
  end;
  end;
end;

procedure TframeSynth.ProcessKeyUp(var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var frameSlot : TframeSlot;
begin
  if not assigned(SelectedFrameSlot) then
    exit;

  case Key of
    vkControl :
      begin
        frameslot := SelectedFrameSlot;

        if assigned(frameSlot) then begin
          frameSlot.EnablePan(frameSlot.sbVA);
          frameSlot.EnablePan(frameSlot.sbFX);
         end;
       end;
    end;
end;

//==============================================================================
//
//                                 GUI
//
//==============================================================================

procedure TframeSynth.rbLocationChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
    0 : SelectLocation(ltFX);
    1 : SelectLocation(ltVA);
  end;
end;

procedure TframeSynth.rbZoomChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  case aValue of
  0 : SlotSetZoomSetting(0);
  1 : SlotSetZoomSetting(1);
  2 : SlotSetZoomSetting(2);
  end;
end;

procedure TframeSynth.btCablesBlueChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.BlueVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesGreenChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.GreenVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesOrangeChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.OrangeVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesPurpleChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.PurpleVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesRedChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.RedVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesWhiteChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.WhiteVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btCablesYellowChangeValue(Sender: TObject;
  const aValue: Integer);
var Patch : TG2USBPatch;
begin
  if not assigned(FSynth) then
    exit;

  Patch := FSynth.SelectedSlot.Patch as TG2USBPatch;

  Patch.PatchDescription.YellowVisible := aValue;
  (Patch as TG2USBPatch).MessSetPatchDescription( Patch.PatchDescription);
end;

procedure TframeSynth.btInfoChangeValue(Sender: TObject; const aValue: Integer);
begin
  if aValue = 1 then begin
    if assigned(FOnShowAppMessage) then
      FOnShowAppMessage(self, 'An alternative editor for the Nord G2')
  end else
    if assigned(FOnCloseAppMessage) then
      FOnCloseAppMessage(self);
end;

procedure TframeSynth.btSplitterChangeValue(Sender: TObject;
  const aValue: Integer);
var frameSlot : TframeSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  frameSlot.Patch.SplitterVisible := aValue = 1;
  frameSlot.UpdateSplitter;
end;

//==============================================================================
//
//                                Functions
//
//==============================================================================

procedure TframeSynth.LoadPatch;
var aFileName : string;
begin
  if OpenDialog1.Execute then begin
    aFileName := OpenDialog1.FileName;
    FframePatch.LoadFileStream( aFileName);
  end;
  //MemoryUsed;
end;

procedure TframeSynth.SelectVariation(const aVariation: integer);
begin
  if not assigned(FSynth) then
    exit;

  FSynth.SelectedSlot.SendSelectVariationMessage( aVariation);
end;

procedure TframeSynth.SelectLocation(const aLocation: TLocationType);
var frameSlot : TframeSlot;
begin
  if not assigned(FSynth) then
    exit;

  FSynth.SelectedSlot.Patch.SelectedLocation := aLocation;
end;

procedure TframeSynth.SelectSlot(const aSlotIndex: integer);
begin
  if not assigned(FSynth) then
    exit;

  FSynth.Performance.SelectedSlot := aSlotIndex;
end;

procedure TframeSynth.StartStopClock;
begin
  if not assigned(FframeSynthStrip) then
    exit;

  FframeSynthStrip.StartStopClock;
end;

procedure TframeSynth.tcFrameChange(Sender: TObject);
begin
  if not assigned(FSynth) then
    exit;

  if tcFrame.TabIndex < 4 then
    FSynth.Performance.SelectedSlot := tcFrame.TabIndex;
end;

procedure TframeSynth.tcPatchFunctionsChange(Sender: TObject);
begin
  if not assigned(SelectedFrameSlot) then
    exit;

  if tcPatchFunctions.TabIndex = 0 then
    SelectedFrameSlot.ActivateAddModule
  else begin
    //SelectedFrameSlot.FinishOperation;
  end;
end;


procedure TframeSynth.UpdateControls;
begin
  if not assigned(FSynth) then
    exit;

  G2Keyboard.SelectedLowKey := FSynth.SelectedSlot.Lower - 24;
  G2Keyboard.SelectedHighKey := FSynth.SelectedSlot.Upper - 24;

  btCablesRed.Value := FSynth.SelectedSlot.Patch.PatchDescription.RedVisible;
  btCablesBlue.Value := FSynth.SelectedSlot.Patch.PatchDescription.BlueVisible;
  btCablesYellow.Value := FSynth.SelectedSlot.Patch.PatchDescription.YellowVisible;
  btCablesOrange.Value := FSynth.SelectedSlot.Patch.PatchDescription.OrangeVisible;
  btCablesGreen.Value := FSynth.SelectedSlot.Patch.PatchDescription.GreenVisible;
  btCablesPurple.Value := FSynth.SelectedSlot.Patch.PatchDescription.PurpleVisible;
  btCablesWhite.Value := FSynth.SelectedSlot.Patch.PatchDescription.WhiteVisible;
end;

procedure TframeSynth.SlotOperationModeChange(Sender: TObject);
begin
  if assigned(Sender) and (Sender is TframeSlot) and
      (Sender as TframeSlot = SelectedframeSlot) then begin
    case SelectedframeSlot.OperationMode of
      omModule :
        begin
          tcPatchFunctions.TabIndex := 1;
          FframeModule.UpdateControls;
        end;
      omParam :
        begin
          tcPatchFunctions.TabIndex := 2;
          FFrameParam.UpdateControls;
        end;
    end;
  end;
end;

procedure TframeSynth.SlotSetZoomSetting(const aValue: integer);
var frameSlot : TframeSlot;
begin
  frameSlot := GetSelectedFrameSlot;
  if assigned(frameSlot) then begin
    frameSlot.Patch.ZoomSetting := aValue;
    frameSlot.UpdateControls;
    //frameSlot.ZoomSetting[ord(frameSlot.SelectedLocation)] := aValue;
  end;
end;

procedure TframeSynth.SlotUpdateSettings(Sender: TObject);
var frameSlot : TframeSlot;
begin
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
  end;
end;

procedure TframeSynth.G2KeyboardKeyboardKeyUp(Sender: TObject;
  aKeyNo: Integer);
begin
  FSynth.SelectedSlot.Lower := G2Keyboard.SelectedLowKey + 24;
  FSynth.SelectedSlot.Upper := G2Keyboard.SelectedHighKey + 24;
  (FSynth.Performance as TG2GraphPerformanceFMX).SendSetPerfSettingsMessage;
end;

//==============================================================================
//
//                                 Events
//
//==============================================================================

procedure TframeSynth.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtBeforeSendMessage:
      begin

      end;
    EvtReceiveResponseMessage:
      begin

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
    EvtClockRunChange: ;
    EvtClockBPMChange: ;
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

{procedure TframeSynth.G2BeforeSendMessage(Sender: TObject; SenderID: Integer;
  SendMessage: TG2SendMessage);
begin
  if assigned(FframeSynthStrip) then begin
    if FframeSynthStrip.rLed.Fill.Color <> claRed then
      FframeSynthStrip.rLed.Fill.Color := claLime;
  end;
  FResponseTimeOut.Enabled := True;
end;

procedure TframeSynth.G2ReceiveResponseMessage(Sender: TObject;
  ResponseMessage: TMemoryStream);
begin
  FResponseTimeOut.Enabled := False;
  if assigned(FframeSynthStrip) then begin
    FframeSynthStrip.rLed.Fill.Color := $FFE0E0E0;
  end;
end;

procedure TframeSynth.G2MessageTimeOut(Sender: TObject);
begin
  FResponseTimeOut.Enabled := False;
  if assigned(FframeSynthStrip) then begin
    FframeSynthStrip.rLed.Fill.Color := claRed;
  end;
end;}

procedure TframeSynth.DoAfterG2Init;
var i : integer;
begin
  if not assigned(FSynth) then
    exit;

 // if not assigned(FframeSynthStrip)  then
 //   exit;

  {if (FframeAppSettings.AutoAssignMidi = 1) then begin
    for i := 0 to 4 do
      FframeAppSettings.AutoAssignMidiToKnobs( FSynth, i);
  end;}

  //CalcLayoutDimensions;

  UpdateControls;
  //FframeSynthStrip.UpdateControls;

  for i := 0 to 3 do begin
    //FSynth.Performance.Slot[i].Patch.SelectedLocation := ltVA;
    FSynth.Performance.Slot[i].Patch.InvalidateParameters;
  end;

  if tcFrame.TabIndex < 4 then
    tcFrame.TabIndex := FSynth.Performance.SelectedSlot;

  FframeAppSettings.UpdateControls;
  frameBanks1.UpdateControls;
  frameKnobs.UpdateControls;
  frameModule.UpdateControls;
end;

{
procedure TframeSynth.DoAfterPerfInit;
var Synth : TG2USB;
    Perf : TG2USBPerformance;
    i : integer;
begin
  // After retrieving a performance...
  //if Sender is TG2USBPerformance then begin
  //  Perf := Sender as TG2USBPerformance;
  //  Synth := Perf.G2 as TG2USB;

    //Synth.OnAfterPerfInit := nil;
  if FInitType = itPerf then begin

    FInitType := itNone;
    if (FframeAppSettings.AutoAssignMidi = 1) then begin
      for i := 0 to 4 do
        FframeAppSettings.AutoAssignMidiToKnobs( FSynth, i);
    end;
  end;
end;

procedure TframeSynth.DoAfterSlotInit;
var Synth : TG2USB;
    Slot : TG2USBSlot;
begin
  // After retrieving a patch...
  //if Sender is TG2USBSlot then begin
  //  Slot := Sender as TG2USBSlot;
  //  Synth := Slot.G2 as TG2USB;

  //  Synth.OnAfterSlotInit := nil;
  if FInitType in [itSlotA, itSlotB, itSlotC, itSlotD] then begin

    if (FframeAppSettings.AutoAssignMidi = 1) then begin
      case FInitType of
        itSlotA : FframeAppSettings.AutoAssignMidiToKnobs( FSynth, 0);
        itSlotB : FframeAppSettings.AutoAssignMidiToKnobs( FSynth, 1);
        itSlotC : FframeAppSettings.AutoAssignMidiToKnobs( FSynth, 2);
        itSlotD : FframeAppSettings.AutoAssignMidiToKnobs( FSynth, 3);
      end;

    end;
    FInitType := itNone;
  end;
  //end;
end;}

{procedure TframeSynth.G2PatchLoadChange(Sender: TObject; SenderID: integer;
  Slot: byte);
begin
  FSlotStripList[ Slot].UpdateControls;
end;}

{procedure TframeSynth.G2BeforePatchUpdate(Sender: TObject; SenderID,
  PatchIndex: integer);
var i : integer;
begin
  if PatchIndex = 4 then begin
    for i := 0 to 3 do
      FSlotList[ i].PreparePatchUpdate;
  end else begin
    FSlotList[ PatchIndex].PreparePatchUpdate;
  end;
end;}

{procedure TframeSynth.G2PatchUpdate(Sender: TObject; SenderID,
  PatchIndex: integer);
var Patch : TG2GraphPatchFMX;
begin
  UpdateControls;
  //FSlotList[ PatchIndex].UpdateControls;
  //FSlotStripList[ PatchIndex].UpdateControls;
  //framePatchSettings.UpdateControls;
  //frameKnobs.UpdateControls;

  FSlotList[ PatchIndex].SelectedLocation := TLocationType(rbLocation.Value);
  //Patch := FSlotList[ PatchIndex].Patch;
  //Patch.SetCablesVisible( 0, Patch.PatchDescription.RedVisible = 1);
  //Patch.SetCablesVisible( 1, Patch.PatchDescription.BlueVisible = 1);
  //Patch.SetCablesVisible( 2, Patch.PatchDescription.YellowVisible = 1);
  //Patch.SetCablesVisible( 3, Patch.PatchDescription.OrangeVisible = 1);
  //Patch.SetCablesVisible( 4, Patch.PatchDescription.GreenVisible = 1);
  //Patch.SetCablesVisible( 5, Patch.PatchDescription.PurpleVisible = 1);
  //Patch.SetCablesVisible( 6, Patch.PatchDescription.WhiteVisible = 1);

  if (not FSynth.GetSlot(PatchIndex).Initializing) and (FframeAppSettings.AutoAssignMidi = 1) then begin
    FframeAppSettings.AutoAssignMidiToKnobs( FSynth, PatchIndex);
  end;
end;}

{procedure TframeSynth.G2PerfSettingsUpdate(Sender: TObject;
  SenderID: Integer; PerfMode: Boolean);
begin
  if FSynth.Performance.KeyboardRangeEnabled  = 1 then
    G2Keyboard.State := csDefault
  else
    G2Keyboard.State := csDisabled;

  //if assigned(FframeSynthStrip)  then
   // FframeSynthStrip.UpdateControls;

  G2SelectSlot(FSynth, FSynth.ID, FSynth.Performance.SelectedSlot);
end;

procedure TframeSynth.G2PerfUpdate(Sender: TObject; SenderID: Integer);
begin
  G2PerfSettingsUpdate(FSynth, FSynth.ID, True);
  //G2SelectSlot(FSynth, FSynth.ID, FSynth.Performance.SelectedSlot);
end;}

procedure TframeSynth.DoPerfUpdate;
begin
  if FSynth.Performance.KeyboardRangeEnabled  = 1 then
    G2Keyboard.State := csDefault
  else
    G2Keyboard.State := csDisabled;

  DoSelectSlot;
end;

{procedure TframeSynth.G2SelectLocation(Sender: TObject; SenderID: integer;
  Slot: byte; Location: TLocationType);
var frameSlot : TframeSlot;
begin
  case Location of
    ltFX, ltVA :
      begin
        rbLocation.Value := ord(Location);
        //frameSlot := GetSelectedFrameSlot;
        //if assigned(frameSlot) then
        //  frameSlot.SelectedLocation := Location;
        //frameSlot.UpdateControls;
        //frameModule.UpdateControls;
      end;
  end;
end;}

{procedure TframeSynth.G2SelectModule(Sender: TObject; SenderID: integer;
  Module: TG2FileModule);
begin
  frameModule.UpdateControls;
end;}

{procedure TframeSynth.G2SelectParam(Sender: TObject; SenderID: integer;
  Param: TG2FileParameter);
begin
  frameParam.Parameter := Param as TG2GraphParameterFMX;
end;}

{procedure TframeSynth.G2SelectSlot(Sender: TObject; SenderID, Slot: Integer);
var frameSlot : TframeSlot;
    i : integer;
begin
  if tcFrame.TabIndex < 4 then
    tcFrame.TabIndex := Slot;

  //for i := 0 to FSlotStripList.Count - 1 do
  //  FSlotStripList[i].UpdateControls;

  //for i := 0 to FSlotList.Count - 1 do
  //  FSlotList[i].UpdateControls;

  UpdateControls;

  FframeAddModule.frameSlot := SelectedFrameSlot;
  frameModule.frameSlot := SelectedFrameSlot;
  framePatchSettings.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;
  //frameKnobs.UpdateControls;
  frameParam.Parameter := FSynth.SelectedSlot.Patch.PatchPart[ord(FSynth.SelectedSlot.Patch.SelectedLocation)].SelectedParam as TG2GraphParameterFMX;
  frameModule.UpdateControls;
  SlotUpdateSettings(self);
end;}

procedure TframeSynth.DoSelectSlot;
var frameSlot : TframeSlot;
    i : integer;
begin
  if tcFrame.TabIndex < 4 then
    tcFrame.TabIndex := Synth.SelectedSlot.SlotIndex;

  UpdateControls;

  FframeAddModule.frameSlot := SelectedFrameSlot;
  frameModule.frameSlot := SelectedFrameSlot;
  framePatchSettings.Patch := FSynth.SelectedSlot.Patch as TG2GraphPatchFMX;
  frameParam.Parameter := FSynth.SelectedPatchPart.SelectedParam as TG2GraphParameterFMX;
  SlotUpdateSettings(self);
end;

{procedure TframeSynth.G2SynthSettingsUpdate(Sender: TObject;
  SenderID: Integer);
begin
  frameSynthSettings.UpdateControls;
end;}

{procedure TframeSynth.G2VariationChange(Sender: TObject; SenderID, Slot,
  Variation: Integer);
begin
  FSlotStripList[Slot].UpdateControls;
  FSlotList[Slot].Variation := Variation;
  FrameParam.UpdateControls;
end;}

{procedure TframeSynth.G2CopyVariation(Sender: TObject; SenderID, Slot,
  FromVariation, ToVariation: integer);
begin
  if FSlotList[Slot].Patch.ActiveVariation = ToVariation then
    FSlotList[Slot].Patch.InvalidateParameters;
  frameParam.UpdateControls;
end;}

{procedure TframeSynth.G2AfterBankList(Sender: TObject; SenderID: Integer);
begin
  frameBanks1.UpdateControls;
end;}

{procedure TframeSynth.G2AfterStore(Sender: TObject; SenderID: Integer; SlotIndex,
  BankIndex, PatchIndex : byte);
begin
  frameBanks1.UpdateControls;
end;}

{procedure TframeSynth.G2AfterClear(Sender: TObject; SenderID: Integer;
  PatchFileType : TPatchFileType; BankIndex, PatchIndex : byte);
begin
  frameBanks1.UpdateControls;
end;}

{procedure TframeSynth.G2AfterBankDownload( Sender : TObject; SenderID : integer; PatchFileType : TPatchFileType);
begin
  frameBanks1.UpdateControls;
end;}

{procedure TframeSynth.G2AfterClearBank(Sender: TObject; SenderID: Integer;
  PatchFileType: TPatchFileType; BankIndex: byte);
begin
  frameBanks1.UpdateControls;
end;}

{procedure TframeSynth.G2AssignGlobalKnob(Sender: TObject; SenderID,
  KnobIndex: Integer);
begin
  frameKnobs.UpdateControls;
  frameParam.UpdateControls;
end;

procedure TframeSynth.G2AssignKnob(Sender: TObject; SenderID: Integer;
  Slot: byte; KnobIndex: Integer);
begin
  frameKnobs.UpdateControls;
  frameParam.UpdateControls;
end;}

{procedure TframeSynth.G2AfterRetrievePatch(Sender: TObject;
  SenderID: Integer; aSlot, aBank, aPatch: Byte);
begin
  if aSlot = 4 then begin
    //FSynth.OnAfterPerfInit := G2AfterPerfInit;
    FInitType := itPerf;
    (FSynth.Performance as TG2USBPerformance).USBStartInit( True)
  end else begin
    //FSynth.OnAfterSlotInit := G2AfterSlotInit;
    case aSlot of
    0 : FInitType := itSlotA;
    1 : FInitType := itSlotB;
    2 : FInitType := itSlotC;
    3 : FInitType := itSlotD;
    end;
    (FSynth.Performance.Slot[ aSlot] as TG2USBSlot).USBStartInit( True);
  end;
end;}

{procedure TframeSynth.G2AfterGetAssignedVoices(Sender: TObject);
var i : integer;
begin
  for i := 0 to FSlotStripList.Count - 1 do
    FSlotStripList[i].UpdateControls;
end;}

{procedure TframeSynth.G2ClockBPMChange(Sender: TObject; SenderID,
  BPM: integer);
begin
  FframeSynthStrip.MasterClockBPM := BPM;
end;}

{procedure TframeSynth.G2ClockRunChange(Sender: TObject; SenderID: integer;
  Status: boolean);
begin
  if Status = False then begin
    //gdMasterClock.Font.Color := clWhite;
    FframeSynthStrip.MasterClockBPM := FSynth.Performance.MasterClock;
  end;
  if Status then
    FframeSynthStrip.btClockRun.Value := 1
  else
    FframeSynthStrip.btClockRun.Value := 0;
  FframeSynthStrip.btClockRun.Redraw;
end;}

{procedure TframeSynth.G2DeassignGlobalKnob(Sender: TObject; SenderID,
  KnobIndex: Integer);
begin
  frameKnobs.UpdateControls;
  frameParam.UpdateControls;
end;

procedure TframeSynth.G2DeassignKnob(Sender: TObject; SenderID: Integer;
  Slot: byte; KnobIndex: Integer);
begin
  frameKnobs.UpdateControls;
  frameParam.UpdateControls;
end;}

{procedure TframeSynth.G2DeleteModule(Sender: TObject; SenderID: integer;
  Location: TLocationType; ModuleIndex: integer);
begin
  frameModule.UpdateControls;
end;}

{procedure TframeSynth.G2LabelValueChange(Sender: TObject; SenderID: integer;
  Param: TG2FileParameter);
begin
  frameParam.UpdateControls;
end;}

procedure TframeSynth.G2LogLine(Sender: TObject; const LogLine: string;
  LogCmd: integer);
begin
  frameLog.mLog.Lines.Add(LogLine);
end;

{procedure TframeSynth.G2MidiCCReceive(Sender: TObject; SenderID: Integer;
  MidiCC: Byte);
begin
  FframeSynthStrip.LastReceivedMidiCC := MidiCC;
  frameParam.UpdateAssignCCButton(MidiCC);
end;}

{procedure TframeSynth.G2MidiClockReceive(Sender: TObject; SenderID,
  BPM: integer);
begin
  if FSynth.Performance.MasterClockRun = 1 then begin
    frameSynthStrip.MasterClockBPM := BPM;
  end;
end;}

{procedure TframeSynth.G2MorphChange(Sender: TObject; SenderID: integer;
  Slot: byte; Location: TLocationType; ModuleIndex, ParamIndex, aMorphIndex,
  aVariationIndex, aValue: byte);
begin
  if assigned(FframeParam) and assigned(FframeParam.Parameter) then begin
    if (FframeParam.Parameter.Patch.Slot.SlotIndex = Slot) and
       (FframeParam.Parameter.Location = Location) and
       (FframeParam.Parameter.ModuleIndex = ModuleIndex) and
       (FframeParam.Parameter.ParamIndex = ParamIndex) then
      FframeParam.UpdateControls;
  end;
end;}

end.

