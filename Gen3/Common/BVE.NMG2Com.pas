unit BVE.NMG2Com;

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

interface

uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.UITypes,
  FMX.Dialogs,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  TG2Command = class(TInterfacedObject, IG2Command)
  private
    FState: TG2CommandState;
    function GetState: TG2CommandState;
    // function GetCanRollback: boolean;
  protected
    procedure DoExecute; virtual; abstract;

  public
    constructor Create;

    procedure Execute;
  end;

  TG2UndoableCommand = class(TG2Command, IG2UndoableCommand)
  private
    // function GetCanRollback: boolean;
  protected
    procedure DoRollback; virtual;
    procedure DoRedo; virtual;

  public
    constructor Create;

    procedure Rollback; // Reverse effect of Execute
    procedure Redo;
  end;

  TG2Connection = class(TG2ObservedObject, IG2Connection)
  private
    [Weak]
    FWConMan: IG2ConnectionManager;

    FClient: IG2Client;

    FSynth: IG2Synth;

    FCopyPatch: IG2PatchPart;
    FCopyVariation: integer;

    FSelectMultiple: boolean;

    FUndoStack, FRedoStack: TStack<IG2UndoableCommand>;

    FLog: IG2Log;
    FLogLevel: integer;

    FLastResponseMessage: Byte;
    FLastG2Error: string;

    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnNextMsg: TOnNextMsgEvent;
    FOnBeforeSendMsg: TG2MessageEvent;
    FOnProcessResponseMsg: TG2MessageEvent;
    FOnProcessSendMsg: TG2MessageEvent;

  protected
    function GetClient: IG2Client;
    procedure SetClient(aValue: IG2Client);
    function GetCopyPatch: IG2PatchPart;
    function GetCopyVariation: Byte;
    procedure SetCopyPatch(const Value: IG2PatchPart);
    procedure SetCopyVariation(const Value: Byte);
    function GetOnNextMsg: TOnNextMsgEvent;
    procedure SetOnNextMsg(const aValue: TOnNextMsgEvent);
    function GetLog: IG2Log;
    procedure SetLog(const aValue: IG2Log);
    function GetConnected: boolean; virtual;
    function GetOnConnect: TNotifyEvent;
    procedure SetOnConnect(aValue: TNotifyEvent);
    function GetOnDisconnect: TNotifyEvent;
    procedure SetOnDisconnect(aValue: TNotifyEvent);
    function GetProcessResponseMsg: TG2MessageEvent;
    procedure SetProcessResponseMsg(aValue: TG2MessageEvent);
    function GetProcessSendMsg: TG2MessageEvent;
    procedure SetProcessSendMsg(aValue: TG2MessageEvent);
    function GetBeforeSendMsg: TG2MessageEvent;
    procedure SetBeforeSendMsg(aValue: TG2MessageEvent);
    function GetSynth: IG2Synth;
    function GetPerf: IG2Perf;
    function GetSlot(const aSlotIndex: integer): IG2Slot;
    function GetPatch(const aSlotIndex: integer): IG2Patch;
    function GetPatchPart(const aSlotIndex, aLocationIndex: integer): IG2PatchPart;
    function GetModule(const aSlotIndex, aLocationIndex, aModuleIndex: integer): IG2Module;
    function GetParam(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Param;
    function GetInConnector(const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer): IG2Connector;
    function GetOutConnector(const aSlotIndex, aLocationIndex, aModuleIndex, aConnectorIndex: integer): IG2Connector;
    function GetController(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Controller;
    function GetKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Knob;
    function GetGlobalKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer): IG2Knob;
    function GetSelectMultiple: boolean;
    procedure SetSelectMultiple(const Value: boolean);

    function GetSelectedSlot: IG2Slot;
    function GetSelectedSlotIndex: integer;
    function GetSelectedPatch: IG2Patch;
    function GetSelectedLocationIndex: integer;
    procedure SetSelectedLocationIndex(const aValue: integer);
    function GetSelectedPatchPart: IG2PatchPart;
    // function GetSelectedModuleList: TList<IG2Module>;
    function GetSelectedModuleCount: integer;
    function GetFocusedModule: IG2Module;
    function GetSelectedParam: IG2Param;
    function GetSelectedVariation: Byte;
    function GetSelectedMorphIndex: Byte;

    function GetLogLevel: integer;
    procedure SetLogLevel(const Value: integer);

    function GetLastG2Error: string;
    procedure SetLastG2Error(const aValue: string);
    function GetLastResponsMessage: Byte;
    procedure SetLastResponsMessage(const Value: Byte);

    procedure NextPatchBankDownloadMessage(Sender: TObject; aConnection: IG2Connection);
    procedure NextPerfBankDownloadMessage(Sender: TObject; aConnection: IG2Connection);
    procedure DownloadPatchBankMessage(const aBank, aLocation: Byte; const aFileName: string);
    procedure DownloadPerfBankMessage(const aBank, aLocation: Byte; const aFileName: string);
    procedure NextBankUploadMessage(Sender: TObject; aConnection: IG2Connection);
  public
    constructor Create(aConMan: IG2ConnectionManager);
    destructor Destroy; override;

    procedure ExecuteCommand(aCommand: IG2Command); virtual;
    procedure Push(aCommand: IG2UndoableCommand);

    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure PasteParams;
    procedure Delete;
    procedure Undo; virtual;
    procedure Redo; virtual;

    procedure LoadBank(const aPatchType: TPatchFileType; const aFileName: string; const aDest: Byte);
    procedure SaveBank(const aPatchType: TPatchFileType; const aFileName: string; const aSrce: Byte);

    procedure LoadSettings; virtual;
    procedure SaveSettings; virtual;

    procedure Connect; virtual;
    procedure Disconnect; virtual;

    procedure AddLog(const aLogLine: string; const aLogCmd: integer);
    procedure DumpBuffer(var buffer; max_size: integer);

    procedure SendMsg(aMsg: TG2SendMessage); virtual; abstract;

    procedure DoConnect;
    procedure DoDisconnect;
    procedure DoBeforeSendMsg(aMsg: TG2Message);
    procedure DoProcessResponseMsg(aMsg: TG2Message);
    procedure DoProcessSendMsg(aMsg: TG2Message);
    procedure DoNextMsg;

    procedure ParamControllerAssign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aParamIndex, aMidiCC: Byte); virtual; abstract;
    procedure ParamKnobAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnob: Byte); virtual; abstract;
    procedure ParamGlobalKnobAssign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aParamIndex, aKnob: Byte); virtual; abstract;
    procedure ParamGlobalKnobDeassign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aParamIndex: Byte); virtual; abstract;
    procedure ParamKnobDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: Byte); virtual; abstract;
    procedure ParamControllerDeassign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aParamIndex: Byte); virtual; abstract;
    procedure ParamLabel(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aLabelIndex: Byte; aLabel: string); virtual; abstract;
    procedure ParamSetValue(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aVariation, aValue: Byte); virtual; abstract;
    procedure ParamSelect(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: Byte); virtual; abstract;
    procedure ParamSetMorph(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMorph, aValue, aNegative, aVariation: Byte);
      virtual; abstract;

    procedure ModuleLabel(const aSlotIndex, aLocationIndex, aModuleIndex: Byte;
      const aLabel: string); virtual; abstract;
    procedure ModuleColor(const aSlotIndex, aLocationIndex, aModuleIndex,
      aModuleColor: Byte); virtual; abstract;
    procedure ModuleGlobalKnobsAssign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aPageIndex: Byte); virtual; abstract;
    procedure ModuleKnobsAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aPageIndex: Byte); virtual; abstract;

    procedure PatchSettings(aPatch: IG2Patch; aPatchSettings: IG2PatchSettings); virtual; abstract;
    procedure PatchModuleAdd(aPatchPart: IG2PatchPart; aModuleTypeID, aCol, aRow: Byte); virtual; abstract;
    procedure PatchCableAdd(const aSlotIndex, aLocationIndex, aFromModuleIndex,
      aFromConnectorIndex: Byte; const aFromConnectorKind: TConnectorKind;
      const aFromConnectorColor: TCableColor;
      const aToModuleIndex, aToConnectorIndex: Byte;
      const aToConnectorKind: TConnectorKind;
      const aToConnectorColor: TCableColor); virtual; abstract;
    procedure PatchCableDelete(const aSlotIndex, aLocationIndex,
      aFromModuleIndex, aFromConnectorIndex: Byte;
      const aFromConnectorKind: TConnectorKind;
      const aToModuleIndex, aToConnectorIndex: Byte;
      const aToConnectorKind: TConnectorKind); virtual; abstract;
    procedure PatchModulesSelectedMove(aPatchPart: IG2PatchPart; aModuleChangeList: TModuleChangeList); virtual; abstract;
    procedure PatchModulesSelectedCopy(aDestPatchPart, aSrcePatchPart: IG2PatchPart); virtual; abstract;
    procedure PatchModulesSelectedDelete(aPatchPart: IG2PatchPart); virtual; abstract;
    procedure PatchParamsCopy(aDestPatchPart, aSrcePatchPart: IG2PatchPart; aFromVariation, aToVariation: Byte); virtual; abstract;

    procedure SlotGetPatchVersion(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetPatch(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetPatchName(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetCurrentNote(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetPatchNotes(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetResourceTable(const aSlotIndex, aLocationIndex: Byte); virtual; abstract;
    procedure SlotUnknown6(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotGetSelectedParam(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotSetPatchName(const aSlotIndex: Byte; aPatchName: string); virtual; abstract;
    procedure SlotSetPatchCategory(const aSlotIndex, aCategory: Byte); virtual; abstract;
    procedure SlotSetVoices(const aSlotIndex, aVoiceCount, aVoiceMode: Byte); virtual; abstract;
    procedure SlotSelectVariation(const aSlotIndex, aVariation: Byte); virtual; abstract;
    procedure SlotCopyVariation(const aSlotIndex, aFromVariation, aToVariation: Byte); virtual; abstract;
    procedure SlotPatchLoad(const aSlotIndex: Byte; const aPatchName: string; aPatch: IG2Patch); virtual; abstract;
    procedure SlotControllerSnapshot(const aSlotIndex: Byte); virtual; abstract;
    procedure SlotVariationSelect(const aSlotIndex, aVariation: Byte); virtual; abstract;

    procedure PerfGetSettings; virtual; abstract;
    procedure PerfSetSettings; virtual; abstract;
    procedure PerfSelectSlot(const aSlotIndex: Byte); virtual; abstract;
    procedure PerfName(const aPerfName: string); virtual; abstract;
    procedure PerfUnknown2Message; virtual; abstract;
    procedure PerfGetGlobalKnobs; virtual; abstract;

    procedure SynthInit; virtual; abstract;
    procedure SynthStartStopCommunication(const aStop: Byte); virtual; abstract;
    procedure SynthGetPatchVersion; virtual; abstract;
    procedure SynthGetMasterClock; virtual; abstract;
    procedure SynthUnknown1Message; virtual; abstract;
    procedure SynthGetAssignedVoices; virtual; abstract;
    procedure SynthGetList(const aPatchFileType: TPatchFileType; const aBank, aPatch: Byte); virtual; abstract;
    procedure SynthRetreivePatch(const aSlotIndex, aBankIndex, aPatchIndex: Byte); virtual; abstract;
    procedure SynthStorePatch(const aSlotIndex, aBankIndex, aPatchIndex: Byte); virtual; abstract;
    procedure SynthBankClear(const PatchFileType: TPatchFileType; const aBank, aFromLocation, aToLocation: Byte); virtual; abstract;
    procedure SynthPatchClear(const aPatchFileType: TPatchFileType; const aBank, aPatch: Byte); virtual; abstract;
    procedure SynthBankUpload(const aPatchFileType: TPatchFileType; const aBank, aLocation: Byte); virtual; abstract;
    procedure SynthBankPatchDownload(const aBank, aLocation: Byte; const aPatchName: string; aPatch: IG2Patch); virtual; abstract;
    procedure SynthBankPerfDownload(const aBank, aLocation: Byte; const aPerfName: string; aPerf: IG2Perf); virtual; abstract;
    procedure SynthPerfLoad(const aPerfName: string; aPerf: IG2Perf); virtual; abstract;
    procedure SynthPerfMode(const aPerfMode: byte); virtual; abstract;
    procedure SynthNoteOnOff(const aNote, aOnOff: Byte); virtual; abstract;
    procedure SynthMidiDump; virtual; abstract;
    procedure SynthSettings; virtual; abstract;

    property Synth: IG2Synth read GetSynth;
    property Perf: IG2Perf read GetPerf;
    property Slot[const aSlotIndex: integer]: IG2Slot read GetSlot;
    property Patch[const aSlotIndex: integer]: IG2Patch read GetPatch;
    property PatchPart[const aSlotIndex, aLocationIndex: integer]: IG2PatchPart read GetPatchPart;
    property Module[const aSlotIndex, aLocationIndex, aModuleIndex: integer]: IG2Module read GetModule;
    property Param[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Param read GetParam;
    property Controller[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Controller read GetController;
    property Knob[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Knob read GetKnob;
    property GlobalKnob[const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: integer]: IG2Knob read GetGlobalKnob;
    property CopyPatch: IG2PatchPart read GetCopyPatch write SetCopyPatch;
    property CopyVariation: Byte read GetCopyVariation write SetCopyVariation;

    property SelectedSlot: IG2Slot read GetSelectedSlot;
    property SelectedSlotIndex: integer read GetSelectedSlotIndex;
    property SelectedPatch: IG2Patch read GetSelectedPatch;
    property SelectedPatchPart: IG2PatchPart read GetSelectedPatchPart;
    // property SelectedModuleList: TList<IG2Module> read GetSelectedModuleList;
    property SelectedModuleCount: integer read GetSelectedModuleCount;
    property SelectedLocationIndex: integer read GetSelectedLocationIndex write SetSelectedLocationIndex;
    property SelectedVariation: Byte read GetSelectedVariation;
    property SelectedMorphIndex: Byte read GetSelectedMorphIndex;

    property Client: IG2Client read GetClient write SetClient;
    property Connected: boolean read GetConnected;
    property Log: IG2Log read GetLog write SetLog;

    property OnConnect: TNotifyEvent read GetOnConnect write SetOnConnect;
    property OnDisconnect: TNotifyEvent read GetOnDisconnect write SetOnDisconnect;
    property OnBeforeSendMsg: TG2MessageEvent read GetBeforeSendMsg write SetBeforeSendMsg;
    property OnProcessResponseMsg: TG2MessageEvent read GetProcessResponseMsg write SetProcessResponseMsg;
    property OnProcessSendMsg: TG2MessageEvent read GetProcessSendMsg write SetProcessSendMsg;
    property OnNextMsg: TOnNextMsgEvent read GetOnNextMsg write SetOnNextMsg;
  end;

implementation

uses
  BVE.NMG2PatchPart,
  BVE.NMG2Patch,
  BVE.NMG2Perf,
  BVE.NMG2Synth;

{ TG2Command }

constructor TG2Command.Create;
begin
  inherited Create;
  FState := cmdReady;
end;

procedure TG2Command.Execute;
begin
  DoExecute;
end;

{ function TG2Command.GetCanRollback: boolean;
  begin
  Result := False;
  end; }

function TG2Command.GetState: TG2CommandState;
begin
  Result := FState;
end;

{ TG2UndoableCommand }

constructor TG2UndoableCommand.Create;
begin
  inherited;
end;

procedure TG2UndoableCommand.DoRedo;
begin
  DoExecute;
end;

procedure TG2UndoableCommand.DoRollback;
begin
  //
end;

{ function TG2UndoableCommand.GetCanRollback: boolean;
  begin
  Result := True;
  end; }

procedure TG2UndoableCommand.Redo;
begin
  DoRedo;
end;

procedure TG2UndoableCommand.Rollback;
begin
  DoRollback;
end;

{ TG2Connection }

procedure TG2Connection.AddLog(const aLogLine: string; const aLogCmd: integer);
begin
  if assigned(FLog) then
    FLog.add_log_line(aLogLine, aLogCmd);
end;

constructor TG2Connection.Create(aConMan: IG2ConnectionManager);
begin
  inherited Create;

  SetWeak(@FWConMan, aConMan);

  FClient := nil;

  FSynth := TG2Synth.Create(self);
  FCopyPatch := nil;
  FCopyVariation := 0;

  FOnProcessResponseMsg := (FSynth as TG2Synth).ProcessResponseMsg;
  FOnProcessSendMsg := (FSynth as TG2Synth).ProcessSendMsg;

  FLogLevel := 0;

  FUndoStack := TStack<IG2UndoableCommand>.Create;
  FRedoStack := TStack<IG2UndoableCommand>.Create;

  // FSynth.Init;
end;

destructor TG2Connection.Destroy;
begin
  // NotifyObservers(evtDestroy, self as IG2Connection);

  FRedoStack.Free;
  FUndoStack.Free;

  FClient := nil;

  SetWeak(@FWConMan, nil);
  inherited;
end;

procedure TG2Connection.Connect;
begin
  if assigned(Client) and (not Client.Connected) then
  begin
    // ShowMessage('Connect over USB...');

    Client.Connect;

    if assigned(OnConnect) then
      OnConnect(self);

    // Start the G2 initialization for the server
    Synth.StartInit(self);
  end
  else
  begin
    // No USB connection

    // ShowMessage('No usb connection.');

    FWConMan.NotifyObservers(EvtAfterG2Init, Synth);
  end;
end;

procedure TG2Connection.Disconnect;
begin
  if assigned(OnDisconnect) then
    OnDisconnect(self);

  if assigned(Client) then
    Client.Disconnect;
end;

procedure TG2Connection.Copy;
begin
  if assigned(FCopyPatch) then
    FCopyPatch := nil;

  FCopyPatch := SelectedPatchPart.CreateCopy(nil, SelectedLocationIndex);

  FCopyVariation := SelectedVariation;
  NotifyObservers(evtCopy, FCopyPatch);
end;

procedure TG2Connection.Cut;
begin
  Copy;
  Delete;
  NotifyObservers(evtCut, FCopyPatch);
end;

procedure TG2Connection.Paste;
begin
  NotifyObservers(evtPaste, FCopyPatch);
end;

procedure TG2Connection.PasteParams;
begin
  if assigned(FCopyPatch) and assigned(SelectedPatchPart) and
    SelectedPatchPart.CheckParametersFit(FCopyPatch) then
  begin
    PatchParamsCopy(SelectedPatchPart, FCopyPatch, FCopyVariation,
      SelectedVariation);

    NotifyObservers(evtPasteParams, FCopyPatch);
  end;
end;

procedure TG2Connection.Delete;
begin
  if assigned(SelectedPatchPart) then
  { if SelectedPatchPart.SelectedModuleCount > 0 then } begin
    PatchModulesSelectedDelete(SelectedPatchPart);
    NotifyObservers(evtDelete, SelectedPatchPart);
  end;
end;

procedure TG2Connection.Undo;
var
  Command: IG2UndoableCommand;
begin
  if FUndoStack.Count > 0 then
  begin
    Command := FUndoStack.Pop;
    Command.Rollback;
    FRedoStack.Push(Command);
  end;
end;

procedure TG2Connection.LoadSettings;
begin

end;

procedure TG2Connection.SaveSettings;
begin

end;

procedure TG2Connection.LoadBank(const aPatchType: TPatchFileType;
  const aFileName: string; const aDest: Byte);
begin
  if aPatchType = pftPatch then
  begin
    Synth.BankDumpFolder := ExtractFilePath(aFileName);
    Synth.BankDumpFileName := aFileName;
    Synth.BankDumpList.LoadFromFile(aFileName);

    Synth.BankDumpDestBank := aDest;

    // Read bank dump list
    Synth.BankDumpListIndex := 0;
    if Synth.BankDumpList[Synth.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump'
    then
      raise Exception.Create
        ('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

    if MessageDlg('Patch bank ' + IntToStr(Synth.BankDumpDestBank + 1) +
      ' will be overwritten with patches from patch list ' +
      ExtractFileName(aFileName) + ', sure?', TMsgDlgType.mtConfirmation,
      mbOKCancel, 0) = mrOk then
      NextPatchBankDownloadMessage(self, self);
  end
  else
  begin
    Synth.BankDumpFolder := ExtractFilePath(aFileName);
    Synth.BankDumpFileName := aFileName;
    Synth.BankDumpList.LoadFromFile(aFileName);

    Synth.BankDumpDestBank := aDest;

    // Read bank dump list
    Synth.BankDumpListIndex := 0;
    if Synth.BankDumpList[Synth.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump'
    then
      raise Exception.Create
        ('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

    if MessageDlg('Perf bank ' + IntToStr(Synth.BankDumpDestBank + 1) +
      ' will be overwritten with performances from patch list ' +
      ExtractFileName(aFileName) + ', sure?', TMsgDlgType.mtConfirmation,
      mbOKCancel, 0) = mrOk then
      NextPerfBankDownloadMessage(self, self);
  end;
end;

procedure TG2Connection.NextPatchBankDownloadMessage(Sender: TObject;
  aConnection: IG2Connection);
var
  OriginalBank, OriginalLocation: Byte;
  PatchFileName: string;
begin
  Synth.BankDumpListIndex := Synth.BankDumpListIndex + 1;
  if Synth.BankDumpListIndex < Synth.BankDumpList.Count then
  begin
    FOnNextMsg := NextPatchBankDownloadMessage;
    if ParseBankDumpListLine(Synth.BankDumpList[Synth.BankDumpListIndex],
      OriginalBank, OriginalLocation, PatchFileName) then
    begin
      DownloadPatchBankMessage(Synth.BankDumpDestBank, Synth.BankDumpListIndex -
        1, Synth.BankDumpFolder + PatchFileName);
    end
    else
    begin
      FOnNextMsg := nil;
      raise Exception.Create('Error reading patch dump file.');
    end
  end
  else
  begin
    aConnection.OnNextMsg := nil;
  end;
end;

procedure TG2Connection.NextPerfBankDownloadMessage(Sender: TObject;
  aConnection: IG2Connection);
var
  OriginalBank, OriginalLocation: Byte;
  PatchFileName: string;
begin
  Synth.BankDumpListIndex := Synth.BankDumpListIndex + 1;
  if Synth.BankDumpListIndex < Synth.BankDumpList.Count then
  begin
    FOnNextMsg := NextPerfBankDownloadMessage;
    if ParseBankDumpListLine(Synth.BankDumpList[Synth.BankDumpListIndex],
      OriginalBank, OriginalLocation, PatchFileName) then
    begin
      DownloadPerfBankMessage(Synth.BankDumpDestBank, Synth.BankDumpListIndex -
        1, Synth.BankDumpFolder + PatchFileName);
    end
    else
    begin
      FOnNextMsg := nil;
      raise Exception.Create('Error reading patch dump file.');
    end
  end
  else
  begin
    aConnection.OnNextMsg := nil;
  end;
end;

procedure TG2Connection.DownloadPatchBankMessage(const aBank, aLocation: Byte;
  const aFileName: string);
var
  Patch: IG2Patch;
  PatchName: string;
  FileStream: TFileStream;
begin
  Patch := TG2Patch.Create(nil);
  FileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    try
      Patch.LoadFromFile(FileStream);
      PatchName := PatchNameFromFileName(aFileName);
      SynthBankPatchDownload(aBank, aLocation, PatchName, Patch);
    except
      on E: Exception do
      begin
        FOnNextMsg := nil;
        raise;
      end;
    end;
  finally
    FileStream.DisposeOf;
  end;
end;

procedure TG2Connection.DownloadPerfBankMessage(const aBank, aLocation: Byte;
  const aFileName: string);
var
  Perf: IG2Perf;
  PerfName: string;
  FileStream: TFileStream;
begin
  Perf := TG2Perf.Create(nil);
  FileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    try
      Perf.LoadFromFile(FileStream);
      PerfName := PatchNameFromFileName(aFileName);
      SynthBankPerfDownload(aBank, aLocation, PerfName, Perf);
    except
      on E: Exception do
      begin
        FOnNextMsg := nil;
        raise;
      end;
    end;
  finally
    FileStream.DisposeOf;
  end;
end;

procedure TG2Connection.DumpBuffer(var buffer; max_size: integer);
begin
  FLog.dump_buffer(buffer, max_size);
end;

procedure TG2Connection.SaveBank(const aPatchType: TPatchFileType;
  const aFileName: string; const aSrce: Byte);
begin
  Synth.BankDumpList.Clear;
  Synth.BankDumpList.Add('Version=Nord Modular G2 Bank Dump');
  case aPatchType of
    pftPatch:
      begin
        Synth.BankDumpFileName := 'PatchBank' + IntToStr(aSrce + 1) +
          '.pchList';
      end;
    pftPerf:
      begin
        Synth.BankDumpFileName := 'PerfBank' + IntToStr(aSrce + 1) + '.pchList';
      end;
  else
    raise Exception.Create('Unknown patch file type.');
  end;

  Synth.BankDumpFolder := ExtractFilePath(aFileName);

  FOnNextMsg := NextBankUploadMessage;
  SynthBankUpload(aPatchType, aSrce, 0);
end;

procedure TG2Connection.NextBankUploadMessage(Sender: TObject;
  aConnection: IG2Connection);
var
  BankItem: IG2BankItem;
begin
  BankItem := Synth.BankFindNext;
  if assigned(BankItem) then
    SynthBankUpload(BankItem.PatchFileType, BankItem.Bank, BankItem.Patch)
  else
  begin
    // Last one send, save the bank dump list
    Synth.BankDumpList.SaveToFile(Synth.BankDumpFolder +
      Synth.BankDumpFileName);
    FOnNextMsg := nil;
  end;
end;

procedure TG2Connection.DoBeforeSendMsg(aMsg: TG2Message);
begin
  if assigned(FOnBeforeSendMsg) then
    FOnBeforeSendMsg(self, aMsg);
end;

procedure TG2Connection.DoConnect;
begin
  if assigned(FOnConnect) then
    FOnConnect(self);
end;

procedure TG2Connection.DoDisconnect;
begin
  if assigned(FOnDisconnect) then
    FOnDisconnect(self);
end;

procedure TG2Connection.DoNextMsg;
begin
  if assigned(FOnNextMsg) then
    FOnNextMsg(self, self);
end;

procedure TG2Connection.DoProcessResponseMsg(aMsg: TG2Message);
begin
  if assigned(FOnProcessResponseMsg) then
    FOnProcessResponseMsg(self, aMsg);
end;

procedure TG2Connection.DoProcessSendMsg(aMsg: TG2Message);
begin
  if assigned(FOnProcessSendMsg) then
    FOnProcessSendMsg(self, aMsg);

  if assigned(FOnNextMsg) then
    FOnNextMsg(self, self);
end;

procedure TG2Connection.ExecuteCommand(aCommand: IG2Command);
var
  UndoableCommand: IG2UndoableCommand;
begin
  aCommand.Execute;
  if Supports(aCommand, IG2UndoableCommand, UndoableCommand) then
    Push(UndoableCommand);
end;

function TG2Connection.GetBeforeSendMsg: TG2MessageEvent;
begin
  Result := FOnBeforeSendMsg;
end;

function TG2Connection.GetClient: IG2Client;
begin
  Result := FClient;
end;

function TG2Connection.GetConnected: boolean;
begin
  if assigned(FClient) then
    Result := FClient.Connected
  else
    Result := False;
end;

function TG2Connection.GetController(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex: integer): IG2Controller;
begin
  Result := Patch[aSlotIndex].FindController(aLocationIndex, aModuleIndex,
    aParamIndex);
end;

function TG2Connection.GetCopyPatch: IG2PatchPart;
begin
  Result := FCopyPatch;
end;

function TG2Connection.GetCopyVariation: Byte;
begin
  Result := FCopyVariation;
end;

function TG2Connection.GetFocusedModule: IG2Module;
begin
  Result := SelectedPatchPart.FindModule(SelectedPatchPart.FocusedModuleIndex);
end;

function TG2Connection.GetGlobalKnob(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex: integer): IG2Knob;
begin
  Result := Perf.FindKnob(aSlotIndex, aLocationIndex, aModuleIndex,
    aParamIndex);
end;

function TG2Connection.GetInConnector(const aSlotIndex, aLocationIndex,
  aModuleIndex, aConnectorIndex: integer): IG2Connector;
var
  M: IG2Module;
begin
  M := Module[aSlotIndex, aLocationIndex, aModuleIndex];
  if assigned(M) then
    Result := M.InConnector[aConnectorIndex]
  else
    Result := nil;
end;

function TG2Connection.GetKnob(const aSlotIndex, aLocationIndex, aModuleIndex,
  aParamIndex: integer): IG2Knob;
begin
  Result := Patch[aSlotIndex].FindKnob(aLocationIndex, aModuleIndex,
    aParamIndex);
end;

function TG2Connection.GetLog: IG2Log;
begin
  Result := FLog;
end;

function TG2Connection.GetLogLevel: integer;
begin
  Result := FLogLevel;
end;

function TG2Connection.GetModule(const aSlotIndex, aLocationIndex,
  aModuleIndex: integer): IG2Module;
begin
  if assigned(PatchPart[aSlotIndex, aLocationIndex]) then
    Result := PatchPart[aSlotIndex, aLocationIndex].FindModule(aModuleIndex)
  else
    Result := nil;
end;

function TG2Connection.GetOnConnect: TNotifyEvent;
begin
  Result := FOnConnect;
end;

function TG2Connection.GetOnDisconnect: TNotifyEvent;
begin
  Result := FOnDisconnect;
end;

function TG2Connection.GetOnNextMsg: TOnNextMsgEvent;
begin
  Result := FOnNextMsg;
end;

function TG2Connection.GetOutConnector(const aSlotIndex, aLocationIndex,
  aModuleIndex, aConnectorIndex: integer): IG2Connector;
var
  M: IG2Module;
begin
  M := Module[aSlotIndex, aLocationIndex, aModuleIndex];
  if assigned(M) then
    Result := M.OutConnector[aConnectorIndex]
  else
    Result := nil;
end;

function TG2Connection.GetParam(const aSlotIndex, aLocationIndex, aModuleIndex,
  aParamIndex: integer): IG2Param;
var
  M: IG2Module;
begin
  M := Module[aSlotIndex, aLocationIndex, aModuleIndex];
  if assigned(M) then
    Result := M.Param[aParamIndex]
  else
    Result := nil;
end;

function TG2Connection.GetPatch(const aSlotIndex: integer): IG2Patch;
begin
  if assigned(Slot[aSlotIndex]) then
    Result := Slot[aSlotIndex].Patch
  else
    Result := nil;
end;

function TG2Connection.GetPatchPart(const aSlotIndex, aLocationIndex: integer)
  : IG2PatchPart;
begin
  if assigned(Patch[aSlotIndex]) then
    Result := Patch[aSlotIndex].PatchPart[aLocationIndex]
  else
    Result := nil;
end;

function TG2Connection.GetPerf: IG2Perf;
begin
  if assigned(FSynth) then
    Result := FSynth.Perf
  else
    Result := nil;
end;

function TG2Connection.GetProcessResponseMsg: TG2MessageEvent;
begin
  Result := FOnProcessResponseMsg;
end;

function TG2Connection.GetProcessSendMsg: TG2MessageEvent;
begin
  Result := FOnProcessSendMsg;
end;

function TG2Connection.GetSelectedLocationIndex: integer;
begin
  Result := SelectedPatch.SelectedLocation;
end;

function TG2Connection.GetSelectedModuleCount: integer;
var
  SelectedModuleList: TList<IG2Module>;
begin
  if assigned(SelectedPatchPart) then
  begin

    SelectedModuleList := SelectedPatchPart.CreateSelectedModuleList;
    try
      Result := SelectedModuleList.Count;
    finally
      SelectedModuleList.Free;
    end;
  end
  else
    Result := 0;
end;

{ function TG2Connection.GetSelectedModuleList: TList<IG2Module>;
  begin
  Result := SelectedPatchPart.SelectedModuleList;
  end; }

function TG2Connection.GetSelectedMorphIndex: Byte;
begin
  Result := SelectedPatch.SelectedMorphIndex;
end;

function TG2Connection.GetSelectedParam: IG2Param;
begin
  Result := SelectedPatchPart.SelectedParam;
end;

function TG2Connection.GetSelectedPatch: IG2Patch;
begin
  Result := Patch[SelectedSlotIndex];
end;

function TG2Connection.GetSelectedPatchPart: IG2PatchPart;
begin
  Result := SelectedPatch.PatchPart[SelectedPatch.SelectedLocation];
end;

function TG2Connection.GetSelectedSlot: IG2Slot;
begin
  Result := Slot[SelectedSlotIndex];
end;

function TG2Connection.GetSelectedSlotIndex: integer;
begin
  if assigned(Perf) then
    Result := Perf.SelectedSlotIndex
  else
    Result := -1;
end;

function TG2Connection.GetSelectedVariation: Byte;
begin
  Result := SelectedPatch.Settings.ActiveVariation;
end;

function TG2Connection.GetSelectMultiple: boolean;
begin
  Result := FSelectMultiple;
end;

function TG2Connection.GetSlot(const aSlotIndex: integer): IG2Slot;
begin
  if assigned(Perf) then
    Result := Perf.Slot[aSlotIndex]
  else
    Result := nil;
end;

function TG2Connection.GetSynth: IG2Synth;
begin
  Result := FSynth;
end;

function TG2Connection.GetLastG2Error: string;
begin
  Result := FLastG2Error;
end;

function TG2Connection.GetLastResponsMessage: Byte;
begin
  Result := FLastResponseMessage;
end;

procedure TG2Connection.SetLastG2Error(const aValue: string);
begin
  FLastG2Error := aValue;

  // DoError;
end;

procedure TG2Connection.SetLastResponsMessage(const Value: Byte);
begin
  FLastResponseMessage := Value;
end;

procedure TG2Connection.Push(aCommand: IG2UndoableCommand);
begin
  FUndoStack.Push(aCommand);
end;

procedure TG2Connection.Redo;
var
  Command: IG2UndoableCommand;
begin
  if FRedoStack.Count > 0 then
  begin
    Command := FRedoStack.Pop;
    Command.Redo;
    FUndoStack.Push(Command);
  end;
end;

procedure TG2Connection.SetBeforeSendMsg(aValue: TG2MessageEvent);
begin
  FOnBeforeSendMsg := aValue;
end;

procedure TG2Connection.SetClient(aValue: IG2Client);
begin
  FClient := aValue;
end;

procedure TG2Connection.SetCopyPatch(const Value: IG2PatchPart);
begin
  FCopyPatch := Value;
end;

procedure TG2Connection.SetCopyVariation(const Value: Byte);
begin
  FCopyVariation := Value;
end;

procedure TG2Connection.SetLog(const aValue: IG2Log);
begin
  FLog := aValue;
end;

procedure TG2Connection.SetLogLevel(const Value: integer);
begin
  FLogLevel := Value;
end;

procedure TG2Connection.SetOnConnect(aValue: TNotifyEvent);
begin
  FOnConnect := aValue;
end;

procedure TG2Connection.SetOnDisconnect(aValue: TNotifyEvent);
begin
  FOnDisconnect := aValue;
end;

procedure TG2Connection.SetOnNextMsg(const aValue: TOnNextMsgEvent);
begin
  FOnNextMsg := aValue;
end;

procedure TG2Connection.SetProcessResponseMsg(aValue: TG2MessageEvent);
begin
  FOnProcessResponseMsg := aValue;
end;

procedure TG2Connection.SetProcessSendMsg(aValue: TG2MessageEvent);
begin
  FOnProcessSendMsg := aValue;
end;

procedure TG2Connection.SetSelectedLocationIndex(const aValue: integer);
begin
  SelectedPatch.SelectedLocation := aValue;
end;

procedure TG2Connection.SetSelectMultiple(const Value: boolean);
begin
  FSelectMultiple := Value;
end;

end.
