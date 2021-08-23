unit BVE.NMG2USB;

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
// ////////////////////////////////////////////////////////////////////////////
//
// The libusb-win32 usb driver is needed:
//
// Download libusb-win32 snapshot from
// http://sourceforge.net/projects/libusb-win32/files/
//
// Make a system restore point
// Install as filter driver on the clavia usb driver
// Do NOT install as device driver (because it then permanently replaces the clavia driver!)
//
// This unit contains the USB interface and the client/server functionality
//
// The usb interface is based on the libusb-win32 (for windows) and the
// libusb1.0 library for unix
//
// The client/server is based on the Indy10 library
//
// Parts ported from https://github.com/msg/g2ools
//
// ////////////////////////////////////////////////////////////////////////////

{$I CompilerSettings.Inc}

// {$DEFINE TCPIPNETWORK}
{$DEFINE SENDTHREAD}
{$DEFINE RECIEVEQUEUE}

interface

uses
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  {$IFDEF MSWINDOWS}
  LibUSBWinDyn,
  {$ENDIF}
  {$IFDEF MACOS}
  libusb_dyn,
  {$ENDIF}
  {$IFDEF LINUX}
  libusb_dyn,
  {$ENDIF}
  {$IFDEF Android}
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.USB,
  {$ENDIF}
  FMX.Types,
  BVE.NMG2Types,
  BVE.NMG2File,
  BVE.NMG2Mess;

const
  {$IFNDEF LINUX}
  LIBUSB_ERROR_TIMEOUT = -116;
  LIBUSB_ERROR_OTHER = -99;
  {$ENDIF}

  // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2
  TIME_OUT = 0;
  // tcpip buffer size, long enough to hold the longest message from the G2
  MAX_READ_BUFFER = 65536;

type
  TCommand = (cmdConnect, cmdDisconnect, cmdGetPerformance, cmdSendPerformance,
    cmdGetGlobalKnobs, cmdSendGlobalKnobs, cmdGetVariationSettings,
    cmdSendVariationSettings, cmdSendMessage);

type
  TUSBErrorEvent = procedure(Sender: TObject; ErrNo: Integer;
    ErrText, LastCmd: string) of object;
  TUSBActiveChangeEvent = procedure(Sender: TObject; Active: Boolean) of object;
  TAfterG2InitEvent = procedure(Sender: TObject) of Object;
  TInitStepEvent = procedure(Sender: TObject) of Object;
  TOnAddClient = procedure(Sender: TObject; ClientIndex: Integer) of Object;
  TOnDeleteClient = procedure(Sender: TObject; ClientIndex: Integer) of Object;
  TBeforeSendMessage = procedure(Sender: TObject; SenderID: Integer;
    SendMessage: TG2SendMessage) of Object;
  TReceiveResponseMessage = procedure(Sender: TObject;
    ResponseMessage: TMemoryStream) of Object;
  TG2USB = class;
  TG2USBPerformance = class;
  TG2USBSlot = class;
  TG2USBPatch = class;

  // Thread for receiving USB messages from the G2
  TReceiveMessageThread = class(TThread)
  private
    [Weak]
    FG2USB: TG2USB;
    // Next is all for logging
    FLogMessage: string;
    FBuffer: pointer;
    FMaxSize: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; aG2: TG2USB);
    procedure ProcessMessage;
    procedure ProcessMessageQueued;
    procedure WriteLog;
    procedure DumpMessage;
  end;

  {$IFDEF SENDTHREAD}
  // Thread for sending USB messages to the G2
  TSendMessageThread = class(TThread)
  private
    [Weak]
    FG2USB: TG2USB;
    // Next is all for logging
    FLogMessage: string;
    FBuffer: pointer;
    FMaxSize: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; aG2: TG2USB);
    procedure WriteLog;
    procedure DumpMessage;
  end;
  {$ENDIF}

  TG2USB = class(TG2Mess)
  private
    {$IFDEF MSWINDOWS}
    bus: pusb_bus;
    g2dev: pusb_device;
    g2udev: pusb_dev_handle;
    g2conf: usb_config_descriptor;
    g2intf: usb_interface;
    g2eps: PArray_usb_endpoint_descriptor;
    g2iin: Byte;
    g2bin: Byte;
    g2bout: Byte;
    {$ENDIF}
    {-$IFDEF MACOS}
    {$IFDEF LIBUSB_UNIX}
    g2dev: Plibusb_device;
    g2udev: Plibusb_device_handle; // a device handle
    g2iin: Byte;
    g2bin: Byte;
    g2bout: Byte;
    {$ENDIF}
    {$IFDEF Android}
    [Weak] FUsbManager: JUSBManager;
    FUsbDevice: JUSBDevice;
    FUsbInterface: JUSBInterface;
    FUsbEPII, FUsbEPBI, FUsbEPBO: JUSBEndPoint;
    FUsbDeviceConnection: JUSBDeviceConnection;
    FIIBuffer, FBIBuffer: TJavaArray<Byte>;
    FBOBuffer: TJavaArray<Byte>;
    {$ENDIF}
    g_bin_buf: TG2ResponseMessage;
    g_bout_buf: TG2SendMessage;
    FParamOutBuf: array [0 .. 20] of Byte;
    {$IFDEF SENDTHREAD}
    // Threadsafe queue for sending USB messages to the G2
    FSendMessageQueue: TThreadList<TG2SendMessage>;
    FSendMessageCount: Integer;
    {$ENDIF}
    {$IFDEF RECIEVEQUEUE}
    // In stead of synchronize...
    FRecieveMessageQueue: TThreadList<TG2ResponseMessage>;
    FRecieveMessageCount: Integer;
    {$ENDIF}
    FInitStep: Integer;
    FInitialized: Boolean;
    FProcessLedData: Boolean;
    FOnUSBError: TUSBErrorEvent;
    FOnUSBActiveChange: TUSBActiveChangeEvent;
    FOnNextInitStep: TInitStepEvent;
    FOnAfterG2Init: TAfterG2InitEvent;
    FOnAfterSlotInit: TNotifyEvent;
    FOnAfterPerfInit: TNotifyEvent;
    FOnBeforeSendMessage: TBeforeSendMessage;
    FOnReceiveResponseMessage: TReceiveResponseMessage;
    // Vars for communication between server and G2
    FWaitforCmd: Byte;
    FReceiveMessageThread: TReceiveMessageThread;
    {$IFDEF SENDTHREAD}
    FSendMessageThread: TSendMessageThread;
    {$ENDIF}
    {$IFDEF UNIX}
    function iread(var buffer; size, timeout: longword): Integer;
    function bread(var buffer; size, timeout: longword): Integer;
    function bwrite(var buffer; size, timeout: longword): Integer;
    {$ENDIF}
    {-$IFDEF MACOS}
    {$IFDEF LIBUSB_UNIX}
    function iread(var buffer; size, timeout: longword): Integer;
    function bread(var buffer; size, timeout: longword): Integer;
    function bwrite(var buffer; size, timeout: longword): Integer;
   {$ENDIF}
    {$IFDEF Android}
    function iread(var buffer; size, timeout: Integer): Integer;
    function bread(var buffer; size, timeout: Integer): Integer;
    function bwrite(var buffer; size, timeout: Integer): Integer;
   {$ENDIF}
    {$IFDEF MSWINDOWS}
    function bwrite(var buffer; size, timeout: longword): Integer;
    function bread(var buffer; size, timeout: longword): Integer;
    function iread(var buffer; size, timeout: longword): Integer;
    {$ENDIF}
    function extended_message(var iin_buf: TByteBuffer; var err_msg: string): Integer;
    function embedded_message(var iin_buf: TByteBuffer): Integer;
    function isextended(buf_in: TByteBuffer): Boolean;
    function isembedded(buf_in: TByteBuffer): Boolean;
  protected
    procedure DoUSBError(ErrNo: Integer; ErrText, LastCmd: string);
    procedure DoUSBActiveChange(Active: Boolean);
    procedure DoNextInitStep;
    procedure DoAfterG2Init;
    procedure DoAfterSlotInit;
    procedure DoAfterPerfInit;
    procedure DoBeforeSendMessage(SenderID: Integer; SendMessage: TG2SendMessage);
    procedure DoReceiveResponseMessage(ResponseMessage: TMemoryStream);
    procedure DoAfterBankDownload(aPatchFileType: TPatchFileType);
    function GetUSBActive: Boolean; virtual;
    procedure SetUSBActive(const value: Boolean); virtual;
    procedure SetPerfMode(aValue: TBits1); override;
  public
    FLogLedDataMessages: Boolean;
    FRecieveAlive,
    FSendAlive,
    FTotalRecieved,
    FTotalRecievedInterrupt: Integer;
    FTotalSend: Integer;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreatePerformance: TG2FilePerformance; override;
    function GetPerformance: TG2USBPerformance;
    function GetSlot(aSlot: Byte): TG2USBSlot;
    // Initialization USB interface
    procedure LibUSBInit;
    procedure LibUSBDone;
    function get_error: string;
    function SendCmdMessage(aSendMessage: TG2SendMessage): Boolean; virtual;
    // USB communication
    {$IFDEF SENDTHREAD}
    procedure SendMessFromQueue(var err_msg: string);
    // procedure   SendMessFromQueue2( Mess : TG2SendMessage);
    procedure SendResponslessTable;
    {$ENDIF}
    procedure USBSendMessage(aSendMessage: TG2SendMessage);
    procedure USBProcessMessage(ResponseMessage: TG2ResponseMessage);
    procedure USBProcessMessageQueued;
    procedure USBProcessResponseMessage(ResponseMessage: TG2ResponseMessage); virtual;
    procedure USBProcessSendMessage(aSendMessage: TG2SendMessage); virtual;
    procedure USBStartInit;
    procedure USBInitSeq(Sender: TObject);
    procedure USBSentStartComm(Sender: TObject);
    procedure ReportState;
    procedure NextListMessage(Sender: TObject);
    procedure RefreshBanks;
    procedure SendInitMessage;
    procedure SendStartStopCommunicationMessage(Stop: Byte);
    procedure SendGetPatchVersionMessage(var patch_version: Byte);
    procedure SendGetSynthSettingsMessage;
    procedure SendSetSynthSettingsMessage;
    procedure SendUnknown1Message;
    procedure SendDumpMidiMessage;
    procedure SendListMessage(aPatchFileType: TPatchFileType; aBank, aPatch: Byte; names: TStrings);
    procedure SendSetModeMessage(aMode: Byte);
    procedure SendNoteMessage(aNote: Byte; aOnoff: Byte);
    procedure SendGetMasterClockMessage;
    procedure SendGetAssignedVoicesMessage;
    procedure SendRetrieveMessage(aSlot, aBank, aPatch: Byte);
    procedure SendStoreMessage(aSlot, aBank, aPatch: Byte);
    procedure SendClearMessage(aPatchFileType: TPatchFileType; aBank, aPatch: Byte);
    procedure SendClearBankMessage(aPatchFileType: TPatchFileType; aBank, aFromLocation, aToLocation: Byte);
    procedure SendUploadBankMessage(aPatchFileType: TPatchFileType; aBank, aLocation: Byte);
    procedure NextBankUploadMessage(Sender: TObject);
    procedure SendDownloadPatchBankMessage(aBank, aLocation: Byte; aFileName: string);
    procedure NextPatchBankDownloadMessage(Sender: TObject);
    procedure SendDownloadPerfBankMessage(aBank, aLocation: Byte; aFileName: string);
    procedure NextPerfBankDownloadMessage(Sender: TObject);

    property Initialized: Boolean read FInitialized;
    {$IFDEF MSWINDOWS}
    property G2USBDevice: pusb_device read g2dev write g2dev;
    {$ENDIF}
    {-$IFDEF MACOS}
    {$IFDEF LIBUSB_UNIX}
    property G2USBDevice: Plibusb_device read g2dev write g2dev;
    {$ENDIF}
    {$IFDEF Android}
    property G2USBDevice: JUSBDevice read FUsbDevice write FUsbDevice;
    property G2USBManager: JUSBManager read FUsbManager write FUsbManager;
    {$ENDIF}
  published
    property USBActive: Boolean read GetUSBActive write SetUSBActive;
    property ProcessLedData: Boolean read FProcessLedData write FProcessLedData;
    property WaitForCmd: Byte read FWaitforCmd write FWaitforCmd;
    property LastSendMessage: TG2SendMessage read g_bout_buf write g_bout_buf;
    property OnUSBError: TUSBErrorEvent read FOnUSBError write FOnUSBError;
    property OnUSBActiveChange: TUSBActiveChangeEvent read FOnUSBActiveChange write FOnUSBActiveChange;
    property OnAfterG2Init: TAfterG2InitEvent read FOnAfterG2Init write FOnAfterG2Init;
    property OnAfterSlotInit: TNotifyEvent read FOnAfterSlotInit write FOnAfterSlotInit;
    property OnAfterPerfInit: TNotifyEvent read FOnAfterPerfInit write FOnAfterPerfInit;
    property OnBeforeSendMessage: TBeforeSendMessage read FOnBeforeSendMessage write FOnBeforeSendMessage;
    property OnReceiveResponseMessage: TReceiveResponseMessage read FOnReceiveResponseMessage write FOnReceiveResponseMessage;
    property OnNextInitStep: TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  end;

  TG2USBPerformance = class(TG2MessPerformance)
  private
    FOnNextInitStep: TInitStepEvent;
    FInitStep: Integer;
    FInitMidiStep: Integer;
    FStartCommAfterInit: Boolean;

    function GetInitializing: Boolean;
  protected
    procedure DoNextInitStep;
    procedure DoAfterRetreivePatch(Slot, Bank, Patch: Byte); override;
    procedure SetMasterClock(aValue: TBits8); override;
    procedure SetMasterClockRun(aValue: TBits8); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateSlot: TG2FileSlot; override;
    function GetSlot(aSlot: Byte): TG2USBSlot;
    function SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
    procedure USBStartInit(aStartCommAfterInit: Boolean);
    procedure USBStartInitWithName;
    procedure USBInitSeq(Sender: TObject);
    function USBInitMidiStep: Boolean;
    // procedure   USBStartInitPerf;
    // procedure   USBInitPerfSeq(Sender: TObject);
    procedure SendGetPerfSettingsMessage;
    procedure SendUnknown2Message;
    procedure SendSelectSlotMessage(aSlot: Byte);
    procedure SendSetPerformanceMessage(aPerfName: string; aPerf: TG2FilePerformance);
    procedure SendSetPerfSettingsMessage;
    procedure SendSetPerfNameMessage(aPerfName: string);
    procedure SendGetGlobalKnobsMessage;
    procedure SendSetMasterClockBPMMessage(BPM: Byte);
    procedure SendSetMasterClockRunMessage(Start: Boolean);
    procedure AutoAssignMidiToKnobs;

    property Initializing: Boolean read GetInitializing;
    property OnNextInitStep: TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  end;

  // Record def. for table sending responseless messages
  TParamUpdRec = record
    SubCmd: Byte;
    Location: Byte;
    Module: Byte;
    Param: Byte;
    Morph: Byte;
    value: Byte;
    Negative: Byte;
    Variation: Byte;
    Changed: Boolean;
  end;

  TG2USBSlot = class(TG2MessSlot)
  private
    FParamUpdBuf: array of TParamUpdRec;
    FParamUpdBufCount: Integer;
    FInitStep: Integer;
    FInitMidiStep: Integer;
    FOnNextInitStep: TInitStepEvent;
    FStartCommAfterInit: Boolean;
  protected
    procedure DoNextInitStep;
    procedure DoAfterRetreivePatch(Slot, Bank, Patch: Byte); override;
    function GetInitializing: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init; override;
    function CreatePatch: TG2FilePatch; override;
    function GetPatch: TG2USBPatch;
    function GetPerformance: TG2USBPerformance;
    function SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
    // All responseless messages must be send through the ParamUpdBuf
    function AddParamUpdRec(aSubCmd, aLocation, aModule, aParam, aMorph, aValue,
      aNegative, aVariation: Byte): Boolean;
    procedure USBStartInit(aStartCommAfterInit: Boolean);
    procedure USBInitSeq(Sender: TObject);
    function USBInitMidiStep: Boolean;
    procedure SendGetPatchVersionMessage;
    procedure SendPatchNotesMessage;
    procedure SendControllerSnapshotMessage;
    procedure SendResourceTableMessage(aLocation: Byte);
    procedure SendGetPatchNameMessage;
    procedure SendCurrentNoteMessage;
    procedure SendUnknown6Message;
    procedure SendGetSelectedParameterMessage;
    procedure SendSetPatchName(aPatchName: string);
    procedure SendSetPatchMessage(aPatchName: string; aPatch: TG2FilePatch);
    procedure SendGetPatchMessage;
    procedure SendSelectVariationMessage(aVariationIndex: Byte);
    procedure SendSetParamMessage(aLocation, aModule, aParam, aValue, aVariation: Byte); virtual;
    procedure SendSelParamMessage(aLocation, aModule, aParam: Integer); virtual;
    procedure SendSetMorphMessage(aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: Byte); virtual;
    procedure SendSetModeMessage(aLocation, aModule, aParam, aValue: Integer);
    procedure SendCopyVariationMessage(aFromVariation, aToVariation: Byte);
    procedure SendGetParamNamesMessage(aLocation: Byte);
    procedure SendGetParamsMessage(aLocation: Byte);
    procedure AutoAssignMidiToKnobs;

    property Initializing: Boolean read GetInitializing;
    property ParamUpdBufCount: Integer read FParamUpdBufCount;
    property OnNextInitStep: TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  end;

  TG2USBPatch = class(TG2MessPatch)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetPerformance: TG2USBPerformance;
    procedure SetVoiceCount(aValue: Byte); override;
    procedure SetVoiceMode(aValue: Byte); override;
    function SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
    procedure SendUndoMessage;
    procedure SetParamValue(aLocation: TLocationType; aModuleIndex: Byte; aParameterIndex: Byte; aVariation: Byte; aValue: Byte); override;
    procedure SetModeValue(aLocation: TLocationType; aModuleIndex: Byte; aParameterIndex: Byte; aValue: Byte); override;
    procedure SetMorphValue(aLocation: TLocationType; aModuleIndex: Byte; aParameterIndex: Byte; aMorphIndex: Byte; aValue: Byte; aVariation: Byte); override;
    procedure SelectParam(aLocation: TLocationType; aModuleIndex: Byte; aParameterIndex: Byte); override;
    function MessSetPatchDescription(FPatchDescription: TPatchDescription): Boolean;
    function MessSetPatchNotes(aLines: TStrings): Boolean;
    function MessAddModule(aLocation: TLocationType; aModuleTypeID, aCol, aRow: Byte): Boolean; override;
    function MessCopyModules(aSrcePatch: TG2FilePatchPart; aFromLocation, aToLocation: TLocationType): Boolean; override;
    function MessCopyParameters(aSrcePatch: TG2FilePatchPart; aToLocation: TLocationType; aFromVariation, aToVariation: Integer): Boolean; override;
    function MessAddConnection(aLocation: TLocationType; aFromConnector, aToConnector: TG2FileConnector): Boolean; override;
    function MessDeleteConnection(aLocation: TLocationType; aCable: TG2FileCable): Boolean; override;
    function MessDeleteModule(aLocation: TLocationType; aModuleIndex: Byte): Boolean; override;
    function MessDeleteModules(aLocation: TLocationType): Boolean; override;
    function MessMoveModules(aLocation: TLocationType): Boolean; virtual;
    function MessSetModuleColor(aLocation: TLocationType; aModuleIndex, aColor: Byte): Boolean; override;
    function MessAssignKnob(aLocation: TLocationType; aModule, aParam, aKnob: Integer): Boolean;
    function MessDeAssignKnob(aKnob: Integer): Boolean;
    function MessModuleAssignKnobs(aModule: TG2FileModule; aPageIndex: Integer): Boolean;
    function MessModuleAssignGlobalKnobs(aModule: TG2FileModule; aPageIndex: Integer): Boolean;
    function MessAssignMidiCC(aLocation: TLocationType; aModule, aParam, aMidiCC: Integer): Boolean;
    function MessDeassignMidiCC(aMidiCC: Integer): Boolean;
    function MessAssignGlobalKnob(aLocation: TLocationType; aModule, aParam, aKnob: Integer): Boolean;
    function MessDeassignGlobalKnob(aKnob: Integer): Boolean;
    function MessSetModuleParamLabels(aLocation: TLocationType; aModuleIndex, aParamIndex, aLabelIndex: Byte; aName: string): Boolean; override;
    function MessSetModuleLabel(aLocation: TLocationType; aModuleIndex: Byte; aName: string): Boolean; override;
  end;

{$IFDEF MSWINDOWS}
  procedure GetUSBDeviceList(aList: TList);
{$ENDIF}
{-$IFDEF MACOS}
{$IFDEF LIBUSB_UNIX}
  procedure GetUSBDeviceList(aList: TList);
{$ENDIF}

implementation

//==============================================================================
//
//                            TG2USB  (G2 + USB)
//
//==============================================================================

constructor TG2USB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInitialized := False; // G2 Initialization succesfull
  FProcessLedData := False;
  {$IFDEF MSWINDOWS}
  g2dev := nil;
  g2udev := nil;
  {$ENDIF}
  {-$IFDEF MACOS}
  {$IFDEF LIBUSB_UNIX}
  g2dev := nil;
  g2udev := nil;
  {$ENDIF}
  {$IFDEF Android}
  FUsbManager := nil;
  FUsbDevice := nil;
  FIIBuffer := TJavaArray<Byte>.Create(8192);
  FBIBuffer := TJavaArray<Byte>.Create(8192);
  FBOBuffer := TJavaArray<Byte>.Create(8192);
  {$ENDIF}
  g_bin_buf := TG2ResponseMessage.Create;
  g_bout_buf := CreateSendMessage;
  {$IFDEF SENDTHREAD}
  FSendMessageQueue := TThreadList<TG2SendMessage>.Create;
  FSendMessageCount := 0;
  {$ENDIF}
  {$IFDEF RECIEVEQUEUE}
  FRecieveMessageQueue := TThreadList<TG2ResponseMessage>.Create;
  FRecieveMessageCount := 0;
  {$ENDIF}
  FLogLedDataMessages := False;
end;

destructor TG2USB.Destroy;
var
  i: Integer;
  SendMessageList: TList<TG2SendMessage>;
  RecieveMessageList: TList<TG2ResponseMessage>;
begin
  {$IFDEF SENDTHREAD}
  // Free the send message queue
  SendMessageList := FSendMessageQueue.LockList;
  try
    for i := 0 to SendMessageList.Count - 1 do
    begin
      SendMessageList[i].DisposeOf;
    end;
    FSendMessageCount := SendMessageList.Count;
  finally
    FSendMessageQueue.UnlockList;
  end;
  FSendMessageQueue.DisposeOf;
  {$ENDIF}

  {$IFDEF RECIEVEQUEUE}
  RecieveMessageList := FRecieveMessageQueue.LockList;
  try
    for i := 0 to RecieveMessageList.Count - 1 do
    begin
      RecieveMessageList[i].DisposeOf;
    end;
    FRecieveMessageCount := RecieveMessageList.Count;
  finally
    FRecieveMessageQueue.UnlockList;
  end;
  FRecieveMessageQueue.DisposeOf;
  {$ENDIF}

  g_bout_buf.DisposeOf;
  g_bin_buf.DisposeOf;

  {$IFDEF Android}
  FBOBuffer.DisposeOf;
  FIIBuffer.DisposeOf;
  FBIBuffer.DisposeOf;
  {$ENDIF}

  inherited;
end;

procedure TG2USB.DoAfterBankDownload(aPatchFileType: TPatchFileType);
begin
  NotifyObservers(EvtAfterBankDownload);

  if assigned(OnAfterBankDownload) then
    OnAfterBankDownload(Self, ID, aPatchFileType);
end;

procedure TG2USB.DoAfterG2Init;
begin
  NotifyObservers(EvtAfterG2Init);

  if assigned(FOnAfterG2Init) then
    FOnAfterG2Init(Self);
end;

procedure TG2USB.DoAfterPerfInit;
begin
  NotifyObservers(EvtAfterPerfInit);

  if assigned(FOnAfterPerfInit) then
   FOnAfterPerfInit(Self);
end;

procedure TG2USB.DoAfterSlotInit;
begin
  NotifyObservers(EvtAfterSlotInit);
  if assigned(FOnAfterSlotInit) then
    FOnAfterSlotInit(self);
end;

procedure TG2USB.DoBeforeSendMessage(SenderID: Integer;
  SendMessage: TG2SendMessage);
begin
  if assigned(FOnBeforeSendMessage) then
    FOnBeforeSendMessage(Self, SenderID, SendMessage);

  NotifyObservers(EvtBeforeSendMessage);
end;

procedure TG2USB.DoNextInitStep;
begin
  NotifyObservers(EvtNextInitStep);

  if assigned(FOnNextInitStep) then
    FOnNextInitStep(Self);
end;

procedure TG2USB.DoReceiveResponseMessage(ResponseMessage: TMemoryStream);
begin
  NotifyObservers(EvtReceiveResponseMessage);

  if assigned(FOnReceiveResponseMessage) then
    FOnReceiveResponseMessage(Self, ResponseMessage);
end;

procedure TG2USB.DoUSBActiveChange(Active: Boolean);
begin
  NotifyObservers(EvtUSBActiveChange);
  if assigned(FOnUSBActiveChange) then
    FOnUSBActiveChange(Self, Active);
end;

procedure TG2USB.DoUSBError(ErrNo: Integer; ErrText, LastCmd: string);
begin
  NotifyObservers(EvtUSBError);
  if assigned(FOnUSBError) then
    FOnUSBError(Self, ErrNo, ErrText, LastCmd);
end;

function TG2USB.get_error: string;
var
  i: Integer;
  err: PByteArray;
begin
//{$IFDEF UNIX}
//  Result := 'error...';
//{$ENDIF}
//{$IFDEF MACOS}
//  Result := 'error...';
//{$ENDIF}
//{$IFDEF ANDROID}
//  Result := 'error...';
//{$ENDIF}
{err := usb_strerror;
  Result := '';
  i := 0;
  while (i<255) and (err[i]<>#0) do begin
    Result := Result + string(err[i]);
    inc(i);
  end;
}
  Result := 'Libusb error...';
end;

function TG2USB.GetPerformance: TG2USBPerformance;
begin
  Result := Performance as TG2USBPErformance;
end;

function TG2USB.GetSlot(aSlot: Byte): TG2USBSlot;
begin
  Result := Performance.Slot[aSlot] as TG2USBSlot;
end;

//------------------------------------------------------------------------------
//
//                           Device discovery
//
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure GetUSBDeviceList(aList: TList);
var
  bus: pusb_bus;
  dev: pusb_device;
begin
  aList.Clear;

  if not assigned(usb_init) then
    Exit; // libusb dll not loaded

  // LibUSB-Win32 Initialization
  usb_init;              // Initialize libusb
  usb_find_busses;       // Finds all USB busses on system
  usb_find_devices;      // Find all devices on all USB devices
  bus := usb_get_busses; // Return the list of USB busses found

  while assigned(bus) do
  begin
    dev := bus^.devices;

    while assigned(dev) do
    begin
      if (dev^.descriptor.idVendor = VENDOR_ID) and (dev^.descriptor.idProduct = PRODUCT_ID) then
        aList.Add(dev);

      dev := dev^.next;
    end;
    bus := bus^.next;
  end;

  usb_set_debug(255);
end;
{$ENDIF}
{-$IFDEF MACOS}
{$IFDEF LIBUSB_UNIX}
procedure GetUSBDeviceList(aList : TList);
type
  DevsArray = array[0..255] of pointer;
  PDevsArray = ^DevsArray;
var
  err: Integer;
  devs: PPlibusb_device;
  pdev: Plibusb_device;
  descr: libusb_device_descriptor;
  i, cnt: Integer;
begin
  aList.Clear;

  if not assigned(ctx) then
    Exit; // libusb dll not loaded

  cnt := libusb_get_device_list(ctx, @devs);
  try
    for i := 0 to cnt - 1 do
    begin
      pdev := PLibusb_device(PDevsArray(devs)^[i]);
      err := libusb_get_device_descriptor( pdev, @descr );

      if err = 0 then
      begin
        //Log.d('Vendor: ' + IntToStr(descr.idVendor) + ' Product: ' + IntToStr(descr.idProduct));

        if (descr.idVendor = VENDOR_ID) and (descr.idProduct = PRODUCT_ID) then
          aList.Add(pdev)
        else
          libusb_unref_device(pdev);
      end;
    end;

  finally
    //free the list, don't unref the devices in it
    libusb_free_device_list(devs, 0);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//                           Init LibUSB/USB
//
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure TG2USB.LibUSBInit;
var
  dev: pusb_device;
  version: pusb_version;
begin
  // Initialization of the USB interface windows
  try
    g2udev := nil;
    if not assigned(g2dev) then
      Exit;

    add_log_line('USB Get endpoints', LOGCMD_NUL);

    // get 3 endpoints
    g2conf := g2dev^.config[0];
    g2intf := g2conf.iinterface[0];
    g2eps := g2intf.altsetting[0].endpoint;
    g2iin := g2eps[0].bEndpointAddress;
    g2bin := g2eps[1].bEndpointAddress;
    g2bout := g2eps[2].bEndpointAddress;

    add_log_line('g2iin = ' + IntToStr(g2iin) + ', g2bin = ' + IntToStr(g2bin) + ', g2bout = ' + IntToStr(g2bout), LOGCMD_NUL);
    add_log_line('Opening USB Device', LOGCMD_NUL);

    g2udev := usb_open(g2dev);

    if not Assigned(g2udev) then
      raise Exception.Create('Unable to open device.');

    version := usb_get_version;
    add_log_line('LibUSB-win32 driver version : ' + IntToStr(version^.drivermajor) + '.' + IntToStr(version.driverminor) + '.' + IntToStr(version.drivermicro) + '.' + IntToStr(version.drivernano), LOGCMD_NUL);
    add_log_line('Set USB Configuration', LOGCMD_NUL);

    if usb_set_configuration(g2udev, g2conf.bConfigurationValue) < 0 then
      raise Exception.Create('Unable to set configuration.');

    add_log_line('Claim USB Interface', LOGCMD_NUL);

    if usb_claim_interface(g2udev, 0) < 0 then
      raise Exception.Create('Unable to claim the interface.');

    except on E:Exception do
    begin
      add_log_line( E.Message, LOGCMD_ERR);
      G2MessageDlg( E.Message, 1);
    end;
  end;
end;
{$ENDIF}

{-$IFDEF MACOS}
{$IFDEF LIBUSB_UNIX}
procedure TG2USB.LibUSBInit;
var
  err: Integer;
  dev: libusb_device;
  devs: PPlibusb_device;
  cnt: Integer;
begin
  // Initialization of the USB interface unix
  if not assigned(g2dev) then
    Exit;

  err := libusb_open(g2dev, @g2udev);

  //Log.d('Libusb open: ' + IntToStr(err));

  if err = 0 then
  begin

    //kernel driver attaching problem
    if (libusb_kernel_driver_active(g2udev, 0) = 1) then  //find out if kernel driver is attached
    begin
      add_log_line('Kernel Driver Active', LOGCMD_HDR);
      if(libusb_detach_kernel_driver(g2udev, 0) = 0) then //detach it
        add_log_line('Kernel Driver Detached!', LOGCMD_HDR);
    end;

    //dev := libusb_get_device(g2udev);
    // get 3 endpoints
    g2iin := $81;
    g2bin := $82;
    g2bout := $03;

    err := libusb_claim_interface(g2udev, 0);   //claim usb interface

    if err < 0 then
      add_log_line('Error claiming interface ' + IntToStr(err), LOGCMD_HDR);

  end else
    raise Exception.Create('Error opening device ' + IntToStr(err));
end;
{$ENDIF}

{$IFDEF Android}
procedure TG2USB.LibUSBInit;
begin
  //add_log_line('# Interfaces ' + IntToStr(FUSBDevice.getInterfaceCount), LOGCMD_HDR);
  FUsbInterface := FUSBDevice.getInterface(0);
  //add_log_line('# Endpoints ' + IntToStr(FUsbInterface.getEndpointCount), LOGCMD_HDR);

  FUsbEPII := FUsbInterface.getEndpoint(0);
  //add_log_line('Endpoint ' + IntToStr(FUsbEPII.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBI := FUsbInterface.getEndpoint(1);
  //add_log_line('Endpoint ' + IntToStr(FUsbEPBI.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBO := FUsbInterface.getEndpoint(2);
  //add_log_line('Endpoint ' + IntToStr(FUsbEPBO.getEndpointNumber), LOGCMD_HDR);

   if FUsbManager.hasPermission(FUSBDevice) then begin
     add_log_line('Permissions o.k.', LOGCMD_HDR);
   end else begin
     FUSBDevice := nil;
     raise Exception.Create('No permission...');
   end;

  FUsbDeviceConnection := FUsbManager.openDevice(FUSBDevice);
  if not assigned(FUsbDeviceConnection) then
    raise Exception.Create('Failed to open device.');

  if not FUsbDeviceConnection.claimInterface(FUsbInterface, True) then
    raise Exception.Create('Failed to claim interface.');
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//                           Clean up LibUSB/USB
//
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}
procedure TG2USB.LibUSBDone;
begin
  // Disconnect from the USB interface windows
  if Assigned(g2udev) then
  begin
    // Needed to break out the infinite interupt wait
    add_log_line('Reset device', LOGCMD_NUL);
    usb_reset(g2udev);
    add_log_line('Release USB Interface', LOGCMD_NUL);
    usb_release_interface(g2udev, 0);
    add_log_line('Closing USB Device', LOGCMD_NUL);
    usb_close(g2udev);

    g2udev := nil;
  end;
end;
{$ENDIF}

{-$IFDEF MACOS}
{$IFDEF LIBUSB_UNIX}
procedure TG2USB.LIBUSBDone;
var
  err: Integer;
begin
  // Disconnect from the USB interface unix
  // Needed to break out the infinite interupt wait
  libusb_reset_device(g2udev);
  err := libusb_release_interface(g2udev, 0); //release the claimed interface
  if (err <> 0) then
    add_log_line('Error releasing interface', LOGCMD_HDR) //result handler
  else
    add_log_line('Released Interface', LOGCMD_HDR);
  // G2 is never attached to the kernel, so following not needed
  // if (libusb_attach_kernel_driver(g2udev, 0) = 0) then //attach kernel again it
  //   add_log_line('Kernel Driver Attached!', LOGCMD_HDR);
  libusb_close( g2udev); //close the device we opened
  g2udev := nil;
  libusb_unref_device(g2dev);
end;
{$ENDIF}

{$IFDEF Android}
procedure TG2USB.LibUSBDone;
begin
  if assigned(FUsbDeviceConnection) then
  begin
    if not FUsbDeviceConnection.releaseInterface(FUsbInterface) then
      add_log_line('Failed to release the interface', LOGCMD_HDR);

    FUsbDeviceConnection.close;

    add_log_line('Device is closed', LOGCMD_HDR);
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//                       LibUSB/USB send and recieve
//
//------------------------------------------------------------------------------

{$IFDEF UNIX}
function TG2USB.iread(var buffer; size, timeout: longword): Integer;
var
  requested: Integer;
  bytes_read: longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(g2udev, g2iin, PChar(@buffer), size, @bytes_read, timeout);
  if Result >= 0 then
    Result := bytes_read;
end;

function TG2USB.bread(var buffer; size, timeout: longword): Integer;
var
  requested: Integer;
  bytes_read: longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(g2udev, g2bin, PChar(@buffer), size, @bytes_read, timeout);
  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): Integer;
var
  bytes_written: longint;
begin
  // Write a bulk message over USB unix
  try
    Result := libusb_bulk_transfer(g2udev, g2bout, PChar(@buffer), size, @bytes_written, timeout);
  finally
    if Result < 0 then
      add_log_line(get_error, LOGCMD_ERR)
    else
      Result := size;
  end;
end;
{$ENDIF}

{-$IFDEF MACOS}
{$IFDEF LIBUSB_UNIX}
function TG2USB.iread(var buffer; size, timeout: longword): Integer;
var
  requested: Integer;
  bytes_read: longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(g2udev, g2iin, PChar(@buffer), size, @bytes_read, timeout);
  if Result >= 0 then
    Result := bytes_read;
end;

function TG2USB.bread(var buffer; size, timeout: longword): Integer;
var
  requested: Integer;
  bytes_read: longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(g2udev, g2bin, PChar(@buffer), size, @bytes_read, timeout);
  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): Integer;
var
  bytes_written: longint;
begin
  // Write a bulk message over USB unix
  try
    Result := libusb_bulk_transfer(g2udev, g2bout, PChar(@buffer), size, @bytes_written, timeout);
  finally
    if Result < 0 then
      add_log_line(get_error, LOGCMD_ERR)
    else
      Result := size;
  end;
end;
{$ENDIF}

{$IFDEF ANDROID}
function TG2USB.iread(var buffer; size, timeout: Integer): Integer;
var
  requested: Integer;
begin
  // Read an interrupt message from USB unix
  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPII, FIIBuffer, size, timeout);
  if Result > 0 then
     Move(FIIBuffer.Data^, buffer, Result);
end;

function TG2USB.bread(var buffer; size, timeout: Integer): Integer;
var
  requested: Integer;
begin
  // Read a bulk message from USB unix
  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBI, FBIBuffer, size, timeout);
  if Result > 0 then
    Move(FBIBuffer.Data^, buffer, Result);
  //if Result < 0 then
  //  add_log_line(get_error, LOGCMD_ERR);
end;

function TG2USB.bwrite(var buffer; size, timeout: Integer): Integer;
var
  TempBuffer: TJavaArray<Byte>;
begin
  // Write a bulk message over USB unix
  Move(buffer, FBOBuffer.Data^, size);
  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBO, FBOBuffer, Size, 0);
  if Result < 0 then
    add_log_line('Error sending message ' + IntToStr(Result), LOGCMD_ERR);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TG2USB.iread(var buffer; size, timeout: longword): Integer;
begin
  // Read an interrupt message from USB windows
  Result := usb_interrupt_read(g2udev, g2iin, buffer, size, timeout);
end;

function TG2USB.bread(var buffer; size, timeout: longword): Integer;
begin
  Result := 0;
  // Read a bulk message from USB windows
  if assigned(g2udev) then
    Result := usb_bulk_read(g2udev, g2bin, buffer, size, timeout);
  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): Integer;
begin
  // Write a bulk message over USB windows
  Result := usb_bulk_write(g2udev, g2bout, buffer, size, timeout);
  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//                      G2 Check response message CRC
//
//------------------------------------------------------------------------------

function TG2USB.extended_message(var iin_buf: TByteBuffer; var err_msg: string): Integer;
var
  i, retries, bytes_read, buf_position: Integer;
  ecrc, acrc: Word;
  size: Word;
begin
  // Read an G2 extended message from USB, interrupt messages contains length,
  // bulk message contains the data
  Result := -1;
  err_msg := '';
  size := (iin_buf[1] shl 8) or iin_buf[2];
  g_bin_buf.Position := 0;
  g_bin_buf.Size := Size;
  retries := 5; // the message has to return within 5 tries
  bytes_read := 0;
  buf_position := 0;

  while (retries > 0) and (buf_position < size) {(size <> bytes_read)} and (bytes_read >= 0) do
  begin
    bytes_read := bread(PStaticByteBuffer(g_bin_buf.Memory)^[buf_position], size, 500{TIME_OUT});
    if bytes_read = LIBUSB_ERROR_TIMEOUT then
    begin
      bytes_read := 0; //time out
      err_msg := err_msg + 'Bulk read timeout! ';
    end else begin
      if bytes_read < 0 then
      begin
        err_msg := err_msg + 'Bulk read error ' + IntToStr(bytes_read) + ' ';
      end else begin
        //if size <> bytes_read then
        //  err_msg := err_msg + 'bulk read ' + IntToStr(bytes_read)+ '/' + IntToStr(size) + ' ';
        if bytes_read <> 0 then
        begin
          buf_position := buf_position + bytes_read;
          inc(FTotalRecieved);
        end;

      end;
    end;
    dec(retries);
  end;

  if retries = 0 then
  begin
    err_msg := err_msg + 'Timeout reading extended message. ';
    Exit;
  end;

  if buf_position <> size then
  begin
    err_msg := err_msg + 'bulk read size mismatch, read ' + IntToStr(buf_position)+ ' of ' + IntToStr(size) + ' ';
  end else begin

    ecrc := 0; // expected crc
    for i := 0 to size - 2 - 1 do
      ecrc := CrcClavia(ecrc, PStaticByteBuffer(g_bin_buf.Memory)^[i]);
    acrc := PStaticByteBuffer(g_bin_buf.Memory)^[ size-1] // actual crc
          + PStaticByteBuffer(g_bin_buf.Memory)^[ size-2] * 256;
    if ecrc <> acrc then
      err_msg := err_msg + 'Bad crc exp: ' + IntToHex(ecrc,2) + ' act: ' + IntToHex(acrc,2) + ' ';
  end;

  if bytes_read < 0 then
    Result := bytes_read
  else
    Result := buf_position;
end;

function TG2USB.embedded_message(var iin_buf: TByteBuffer): Integer;
var
  i, dil: Integer;
  ecrc, acrc: Word;
begin
  // Read an embedded G2 message, interrupt message contains length and data
  Result := -1;
  g_bin_buf.Position := 0;
  g_bin_buf.Size := Length(iin_buf);
  move( iin_buf[0], g_bin_buf.Memory^, Length(iin_buf));
  dil := PStaticByteBuffer(g_bin_buf.Memory)^[0] shr 4;
  ecrc := 0;
  for i := 1 to dil - 2 do
    ecrc := CrcClavia(ecrc, PStaticByteBuffer(g_bin_buf.Memory)^[i]);
  acrc := PStaticByteBuffer(g_bin_buf.Memory)^[dil-0] // actual crc
        + PStaticByteBuffer(g_bin_buf.Memory)^[dil-1] * 256;
  if ecrc <> acrc then
    add_log_line('Bad crc exp: ' + IntToHex(ecrc,2) + ' act: ' + IntToHex(acrc,2), LOGCMD_ERR);
  Result := g_bin_buf.Size;
end;

function TG2USB.isextended(buf_in: TByteBuffer): Boolean;
begin
  Result := buf_in[0] and $f = 1;
end;

function TG2USB.isembedded(buf_in: TByteBuffer): Boolean;
begin
  Result := buf_in[0] and $f = 2;
end;

//------------------------------------------------------------------------------
//
//                        ACTIVATE/DEACTIVATE COMMUNICATION
//
//------------------------------------------------------------------------------

function TG2USB.GetUSBActive: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := Assigned(g2udev)
  {$ENDIF}
  {-$IFDEF MACOS}
  {$IFDEF LIBUSB_UNIX}
  Result := Assigned(g2udev)
  {$ENDIF}
  {$IFDEF Android}
  Result := Assigned(FUsbDevice)
  {$ENDIF}
end;

procedure TG2USB.SetUSBActive(const Value: Boolean);
var
  iin_buf: TByteBuffer;
  bytes_read: Integer;
  timer_start: Cardinal;
begin
  if Value then
  begin
    add_log_line('Initializing connection.', LOGCMD_NUL);

   {$IFDEF MSWINDOWS}
    add_log_line( 'g2udev handle = ' + IntToHex(Integer(g2udev), 4), LOGCMD_NUL);
    if assigned(g2udev) then
      LibUSBDone;
   {$ENDIF}
   {-$IFDEF MACOS}
   {$IFDEF LIBUSB_UNIX}
    add_log_line('g2udev handle = ' + IntToHex(Integer(g2udev), 4), LOGCMD_NUL);
    if assigned(g2udev) then
      LibUSBDone;
   {$ENDIF}

    add_log_line('initializing usb.', LOGCMD_NUL);

    // Start the message thread only if there's a USB connection

    {$IFDEF Android}
    if USBActive then
    begin
    {$ELSE}
    if assigned(g2dev) then
    begin
    {$ENDIF}
      LibUSBInit;

      add_log_line('starting message threads.', LOGCMD_NUL);

      FReceiveMessageThread := TReceiveMessageThread.Create(False, Self);
      {$IFDEF SENDTHREAD}
      FSendMessageThread := TSendMessageThread.Create(False, Self);
      {$ENDIF}

      Sleep(1000);
      DoUSBActiveChange(True);

      //Log.d('Start the G2 initialization for the server');

      // Start the G2 initialization for the server
      USBStartInit;
    end else begin
      // No USB connection
      Performance.SelectedSlot := 0; // CHECK
      DoAfterG2Init;
    end;

  end else begin
    add_log_line('Closing connection.', LOGCMD_NUL);

    // Disconnect
    if USBActive then
    begin
      try
       {$IFDEF SENDTHREAD}
        if assigned(FSendMessageThread) then
        begin
          add_log_line('Stop send message thread.', LOGCMD_NUL);

          FSendMessageThread.Terminate;
          FreeAndNil(FSendMessageThread);
        end;
       {$ENDIF}
        if assigned(FReceiveMessageThread) then
        begin
          add_log_line('Stop receive message thread.', LOGCMD_NUL);
          FReceiveMessageThread.Terminate;
          // Close usb connection
          LibUSBDone;
          FreeAndNil(FReceiveMessageThread);
        end;

      except on E:Exception do
        add_log_line(E.Message, LOgCMD_ERR);
      end;

      FInitialized := False;
    end;

    DoUSBActiveChange(False);
  end;
end;
//------------------------------------------------------------------------------
//
//                           TSendMessageThread
//                    Send messages to USB interface
//
//   This is done in a seperate thread to better control the speed messages
//   are send, because Android crashes when too many are send at once
//
//------------------------------------------------------------------------------
{$IFDEF SENDTHREAD}
constructor TSendMessageThread.Create(CreateSuspended: Boolean; aG2: TG2USB);
begin
  FG2USB := aG2;

  FreeOnTerminate := False;

  inherited Create(CreateSuspended);
end;

procedure TSendMessageThread.Execute;
var
  Error : Boolean;
  //mess : TG2SendMessage;
  //MessageList : TList<TG2SendMessage>;
begin
  Error := False;
  FG2USB.FWaitForCmd := 0;
  FG2USB.FSendAlive := 0;
  FG2USB.FTotalSend := 0;

  {$IFDEF MACOS}
  {-$IFDEF LIBUSB_UNIX}
  Priority := 1;
  {$ENDIF}

  FLogMessage := 'Send thread startet.';
  Synchronize(WriteLog);

  repeat
    // Waiting for a response? No, send a new command
    if (FG2USB.FWaitForCmd = 0) then
    begin
      FG2USB.SendResponslessTable;

      // Send new message if any
      if (FG2USB.FSendMessageCount > 0) then
      begin
        FG2USB.SendMessFromQueue( FLogMessage);
        Synchronize(WriteLog);
      end;

  {MessageList := FG2USB.FSendMessageQueue.LockList;
  try
    if MessageList.Count > 0 then begin

      Mess := MessageList[0];
      FG2USB.FWaitForCmd := Mess.Command;

      MessageList.Delete(0);
      FG2USB.SendMessFromQueue2(Mess);

      FG2USB.FSendMessageCount := MessageList.Count;
      FLogMessage := 'SEND: ' + MidiDump(FG2USB.g_bout_buf, 6);
      synchronize(WriteLog);
    end;

  finally
    FG2USB.FSendMessageQueue.UnlockList;
  end;}

     {$IFDEF Android}
      Sleep(50);
     {$ELSE}
      Sleep(1);
     {$ENDIF}
    end else
      {$IFDEF Android}
      Sleep(50);
      {$ELSE}
      Sleep(10);
      {$ENDIF}

    Inc(FG2USB.FSendAlive);

    if Error then
      Terminate;

  until Terminated;

  FLogMessage := 'Send thread terminated.';
  Synchronize(WriteLog);
end;

procedure TSendMessageThread.WriteLog;
begin
  FG2USB.add_log_line( FLogMessage, LOGCMD_NUL);
end;

procedure TSendMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer( FBuffer^, FMaxSize);
end;

procedure TG2USB.SendMessFromQueue;
var
  mess: TG2SendMessage;
  MessageList: TList<TG2SendMessage>;
  bytes_send: Integer;
begin
  MessageList := FSendMessageQueue.LockList;
  try
    Mess := MessageList[0];
    FWaitForCmd := Mess.Command;
    g_bout_buf.Assign(Mess);
    {$IFDEF ANDROID}
    bytes_send := bwrite(g_bout_buf.Memory^, g_bout_buf.size, 500);
    {$ELSE}
    bytes_send := bwrite(g_bout_buf.Memory^, g_bout_buf.size, TIME_OUT);
    {$ENDIF}
    MessageList.Delete(0);
    Mess.DisposeOf;

    if bytes_send = g_bout_buf.size then
    begin
      FSendMessageCount := MessageList.Count;
      err_msg := 'SEND: ' + MidiDump(g_bout_buf, 6);
      inc(FTotalSend);
    end else begin
      if bytes_send < 0 then
        err_msg := 'SEND ERROR: ' + IntToStr(bytes_send);
      FWaitForCmd := 0;
    end;

  finally
    FSendMessageQueue.UnlockList;
  end;

  {try
    g_bout_buf.Assign(Mess);
    bwrite(g_bout_buf.Memory^, g_bout_buf.size, TIME_OUT);

  finally
    Mess.DisposeOf;
  end;}
end;

{procedure TG2USB.SendMessFromQueue2( Mess : TG2SendMessage);
begin
  try
    g_bout_buf.Assign(Mess);
    bwrite(g_bout_buf.Memory^, g_bout_buf.size, TIME_OUT);
  finally
    Mess.DisposeOf;
  end;
end;}

procedure TG2USB.SendResponslessTable;
var
  i, j: Integer;
  SlotIndex, Size: Byte;
  Crc: Word;
  b: Byte;
  Slot: TG2USBSlot;
begin
  // Send all responseless messages with this function
  for SlotIndex := 0 to 3 do
  begin
    Slot := GetSlot(SlotIndex) as TG2USBSLot;

    for i := 0 to Slot.FParamUpdBufCount - 1 do
    begin
      if (Slot.FParamUpdBuf[i].Changed) and (Slot.FParamUpdBuf[i].Value < 128) then
      begin
        case Slot.FParamUpdBuf[i].SubCmd of
          S_SET_PARAM :
            begin
              Size := 13;
              FParamOutBuf[ 0] := 0;
              FParamOutBuf[ 1] := Size;
              FParamOutBuf[ 2] := $01;
              FParamOutBuf[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
              FParamOutBuf[ 4] := Slot.PatchVersion; // Current patch version!
              FParamOutBuf[ 5] := S_SET_PARAM;
              FParamOutBuf[ 6] := Slot.FParamUpdBuf[i].Location;
              FParamOutBuf[ 7] := Slot.FParamUpdBuf[i].Module;
              FParamOutBuf[ 8] := Slot.FParamUpdBuf[i].Param;
              FParamOutBuf[ 9] := Slot.FParamUpdBuf[i].Value;
              FParamOutBuf[10] := Slot.FParamUpdBuf[i].Variation;
            end;
          S_SEL_PARAM :
            begin
              Size := 12;
              FParamOutBuf[ 0] := 0;
              FParamOutBuf[ 1] := Size;
              FParamOutBuf[ 2] := $01;
              FParamOutBuf[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
              FParamOutBuf[ 4] := Slot.PatchVersion; // Current patch version!
              FParamOutBuf[ 5] := S_SEL_PARAM;
              FParamOutBuf[ 6] := 00;
              FParamOutBuf[ 7] := Slot.FParamUpdBuf[i].Location;
              FParamOutBuf[ 8] := Slot.FParamUpdBuf[i].Module;
              FParamOutBuf[ 9] := Slot.FParamUpdBuf[i].Param;
            end;
          S_SET_MORPH_RANGE :
            begin
              Size := 15;
              FParamOutBuf[ 0] := 0;
              FParamOutBuf[ 1] := Size;
              FParamOutBuf[ 2] := $01;
              FParamOutBuf[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
              FParamOutBuf[ 4] := Slot.PatchVersion; // Current patch version!
              FParamOutBuf[ 5] := S_SET_MORPH_RANGE;
              FParamOutBuf[ 6] := Slot.FParamUpdBuf[i].Location;
              FParamOutBuf[ 7] := Slot.FParamUpdBuf[i].Module;
              FParamOutBuf[ 8] := Slot.FParamUpdBuf[i].Param;
              FParamOutBuf[ 9] := Slot.FParamUpdBuf[i].Morph;
              FParamOutBuf[10] := Slot.FParamUpdBuf[i].Value;
              FParamOutBuf[11] := Slot.FParamUpdBuf[i].Negative;
              FParamOutBuf[12] := Slot.FParamUpdBuf[i].Variation;
            end;
          else begin
            Size := 0;
          end;
        end;

        if Size > 3 then
        begin
          // Calc CRC
          Crc := 0;
          for j := 2 to Size - 3 do
          begin
            b := FParamOutBuf[j];
            Crc := CrcClavia(Crc, b);
          end;
          // Write CRC
          b := Crc div 256;
          FParamOutBuf[Size - 2] := b;
          b := Crc mod 256;
          FParamOutBuf[Size - 1] := b;
         {$IFDEF ANDROID}
          bwrite(FParamOutBuf[0], Size, 200);
         {$ELSE}
          bwrite(FParamOutBuf[0], Size, TIME_OUT);
         {$ENDIF}
         {$IFDEF Android}
          Sleep(100);
          {$ENDIF}
          Inc(FTotalSend);
        end;
      end;

      Slot.FParamUpdBuf[i].Changed := False;
    end;
  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------
//
//                           TReceiveMessageThread
//                    Receive messages from USB interface
//
//------------------------------------------------------------------------------

constructor TReceiveMessageThread.Create(CreateSuspended: Boolean; aG2: TG2USB);
begin
  FG2USB := aG2;
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);
  {$IFDEF MSWINDOWS}
  Priority := tpHigher;
  {$ENDIF}
end;

procedure TReceiveMessageThread.Execute;
var
  iin_buf: TByteBuffer;
  bytes_read: Integer;
  error: Boolean;
  RecieveMessageList: TList<TG2ResponseMessage>;
  Mess: TG2ResponseMessage;
begin
  SetLength(iin_buf, 16);
  error := False;
  FG2USB.FRecieveAlive := 0;
  FG2USB.FTotalRecieved := 0;
  FG2USB.FTotalRecievedInterrupt := 0;

  {$IFDEF MACOS}
  {-$IFDEF LIBUSB_UNIX}
  Priority := 4;
  {$ENDIF}

  repeat
    // Is USB Active?
    if FG2USB.USBActive then
    begin
      // Read the interrupt message
      // Time out must be long enough, otherwise we lose contact with the G2!
      // TIME_OUT = 0 : wait forever
      bytes_read := FG2USB.iread( iin_buf[0], 16, TIME_OUT);

      Inc(FG2USB.FTotalRecievedInterrupt);

      if bytes_read <> 16 then
      begin
        FLogMessage := 'USB interrupt recieved ' + IntToStr(bytes_read) + ' in stead of 16!';
        Synchronize(WriteLog);
      end;

      if bytes_read > 0 then
      begin
        //FLogMessage := 'Usb interupt message recieved ' + IntToStr(bytes_read);
        //Synchronize( WriteLog);
        if FG2USB.isextended(iin_buf) then
        begin
          bytes_read := FG2USB.extended_message(iin_buf, FLogMessage);

          if FLogMessage <> '' then
            Synchronize( WriteLog);

          {$IFDEF RECIEVEQUEUE}
          RecieveMessageList := FG2USB.FRecieveMessageQueue.LockList;
          try
            Mess := TG2ResponseMessage.Create;
            Mess.Assign(FG2USB.g_bin_buf);
            RecieveMessageList.Add(Mess);
            FG2USB.FRecieveMessageCount := RecieveMessageList.Count;
          finally
            FG2USB.FRecieveMessageQueue.UnlockList;
          end;

          Queue(ProcessMessageQueued); //Messages need to be queued also...
          {$ELSE}
          Synchronize(ProcessMessage);
          {$ENDIF}
        end else
          if FG2USB.isembedded( iin_buf) then
          begin
            bytes_read := FG2USB.embedded_message(iin_buf);

            {$IFDEF RECIEVEQUEUE}
            RecieveMessageList := FG2USB.FRecieveMessageQueue.LockList;
            try
              Mess := TG2ResponseMessage.Create;
              Mess.Assign(FG2USB.g_bin_buf);
              RecieveMessageList.Add(Mess);
              FG2USB.FRecieveMessageCount := RecieveMessageList.Count;
            finally
              FG2USB.FRecieveMessageQueue.UnlockList;
            end;

            Queue(ProcessMessageQueued); //Messages need to be queued also...
            {$ELSE}
            Synchronize(ProcessMessage);
            {$ENDIF}
          end else begin
            FLogMessage := 'RecThread: Message received is not embedded nor extended.';
            Synchronize(WriteLog);
            Error := True;
          end;

      end else begin
        if bytes_read < 0 then
        begin
          FLogMessage := 'RecThread: Interrupt read error ' + IntToStr(bytes_read);
          Synchronize( WriteLog);

          case bytes_read of
            LIBUSB_ERROR_TIMEOUT:
              begin
                // could not happen with infinite timeout
              end;

            LIBUSB_ERROR_OTHER:
              begin
                // On reset device, Exit;
                Terminate;
              end;

            else begin
               Error := True;
               FLogMessage := 'RecThread: Interrupt read error ' + IntToStr(bytes_read);
               Synchronize(WriteLog);
               //FBuffer := FG2USB.g_bout_buf.Memory;
              //FMaxSize :=  FG2USB.g_bout_buf.Size;
              //Synchronize( DumpMessage);
            end;
          end;
        end;
      end;

    end else begin
      FLogMessage := 'USB not active...';
      Synchronize( WriteLog);
      Terminate;
    end;

    if Error then
      Terminate;

    Inc(FG2USB.FRecieveAlive);
  until Terminated;

  FLogMessage := 'Usb recieve thread terminated.';
  Synchronize(WriteLog);
end;

procedure TReceiveMessageThread.ProcessMessage;
begin
  FG2USB.USBProcessMessage(FG2USB.g_bin_buf);
end;

procedure TReceiveMessageThread.ProcessMessageQueued;
begin
  FG2USB.USBProcessMessageQueued;
end;

procedure TReceiveMessageThread.WriteLog;
begin
  FG2USB.add_log_line(FLogMessage, LOGCMD_NUL);
end;

procedure TReceiveMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer(FBuffer^, FMaxSize);
end;

//------------------------------------------------------------------------------
//
//                         Processing USB Messages
//
//------------------------------------------------------------------------------

procedure TG2USB.USBSendMessage( aSendMessage : TG2SendMessage);
var
  bytes_send: Integer;
begin
  // Send a message to the G2 over the USB interface
  bytes_send := bwrite(aSendMessage.Memory^, aSendMessage.size, TIME_OUT);
  if bytes_send < 0 then
    add_log_line( 'Error sending message ' + IntToStr(bytes_send), LOGCMD_ERR);
end;

{procedure TG2USB.USBProcessMessage( ResponseMessage : TG2ResponseMessage);
var Cmd : Byte;
    tempMessage : TG2SendMessage;
begin
  // Process a response message received over USB
  Cmd := ResponseMessage.Command;
  ErrorMessage := False;
  USBProcessResponseMessage( ResponseMessage);
  // Led data, volume data...
  if ResponseMessage.IsLedData then
    Exit;
  // Are we waiting for a response?
  if (FWaitForCmd <> 0) then begin
    // Is this the responsemessage we have been waiting for

    if (Cmd = FWaitForCmd) or
       ((Cmd = $04) and (FWaitForCmd = $0c)) or
       ErrorMessage then begin
      if not ErrorMessage then begin
        // If g2 did not respond with error message, then process the send message
        tempMessage := CreateSendMessage; // Necessary?
        try
          tempMessage.Assign( g_bout_buf);
          //FWaitForCmd := 0; // Send buffer can send next message
          USBProcessSendMessage( tempMessage);
          //add_log_line( 'PROCESSED: ' + MidiDump(tempmessage, 6), LOGCMD_NUL);
        finally
          tempMessage.Free;
        end;
      end else begin
        FWaitForCmd := 0; // Try to go on... CHECK
      end;
      DoReceiveResponseMessage( ResponseMessage);
      FWaitForCmd := 0; // Send buffer can send next message
      // Send next message in a sequence, if assigned
      if assigned(GetSlot(0)) and assigned(GetSlot(0).OnNextInitStep)  then
        GetSlot(0).DoNextInitStep
      else
      if assigned(GetSlot(1)) and assigned(GetSlot(1).OnNextInitStep)  then
        GetSlot(1).DoNextInitStep
      else
      if assigned(GetSlot(2)) and assigned(GetSlot(2).OnNextInitStep)  then
        GetSlot(2).DoNextInitStep
      else
      if assigned(GetSlot(3)) and assigned(GetSlot(3).OnNextInitStep)  then
        GetSlot(3).DoNextInitStep
      else
      if assigned(Performance) and assigned(GetPerformance.OnNextInitStep) then
        GetPerformance.DoNextInitStep
      else
      DoNextInitStep;
    end;
  end;
end;}

procedure TG2USB.USBProcessMessage(ResponseMessage : TG2ResponseMessage);
var
  Cmd: Byte;
  tempMessage: TG2SendMessage;
begin
  // Process a response message received over USB
  Cmd := ResponseMessage.Command;
  ErrorMessage := False;
  // Led data, volume data...
  if ResponseMessage.IsLedData then
  begin
    USBProcessResponseMessage(ResponseMessage);
    Exit;
  end else begin
    // Are we waiting for a response?
    if (FWaitForCmd <> 0) then
    begin
      // Is this the responsemessage we have been waiting for

      if (Cmd = FWaitForCmd) or
         ((Cmd = $04) and (FWaitForCmd = $0c)) or
         ErrorMessage then
      begin
        // If g2 did not respond with error message, then process the send message
        tempMessage := CreateSendMessage; // Necessary?
        try
          tempMessage.Assign( g_bout_buf);
          DoReceiveResponseMessage( ResponseMessage);
          USBProcessResponseMessage( ResponseMessage);
          if not ErrorMessage then
            USBProcessSendMessage( tempMessage);
            //add_log_line( 'PROCESSED: ' + MidiDump(tempmessage, 6), LOGCMD_NUL);
          FWaitForCmd := 0; // Send buffer can send next message
        finally
          tempMessage.DisposeOf;
        end;

        // Send next message in a sequence, if assigned
        if assigned(GetSlot(0)) and assigned(GetSlot(0).OnNextInitStep) then
          GetSlot(0).DoNextInitStep
        else
        if assigned(GetSlot(1)) and assigned(GetSlot(1).OnNextInitStep)  then
          GetSlot(1).DoNextInitStep
        else
        if assigned(GetSlot(2)) and assigned(GetSlot(2).OnNextInitStep)  then
          GetSlot(2).DoNextInitStep
        else
        if assigned(GetSlot(3)) and assigned(GetSlot(3).OnNextInitStep)  then
          GetSlot(3).DoNextInitStep
        else
        if assigned(Performance) and assigned(GetPerformance.OnNextInitStep) then
          GetPerformance.DoNextInitStep
        else
          DoNextInitStep;

      end else begin
        add_log_line(DateTimeToStr(Now) + ' Message received out of sync (1). Size = ' + IntToStr(ResponseMessage.Size) + ' ' + MidiDump(ResponseMessage, ResponseMessage.Size), LOGCMD_ERR);
      end;

    end else begin
      //add_log_line( DateTimeToStr(Now) + ' Message received out of sync (2). Size = ' + IntToStr(ResponseMessage.Size) + ' ' + MidiDump(ResponseMessage, ResponseMessage.Size), LOGCMD_ERR);
      // Resource messages are received here...
      USBProcessResponseMessage(ResponseMessage);
    end;
  end;
end;

procedure TG2USB.USBProcessMessageQueued;
var
  Cmd: Byte;
  tempMessage: TG2SendMessage;
  RecieveMessageList: TList<TG2ResponseMessage>;
  ResponseMessage: TG2ResponseMessage;
begin
  // Process a response message received over USB
  RecieveMessageList := FRecieveMessageQueue.LockList;
  try
    if RecieveMessageList.Count = 0 then
      Exit;

    ResponseMessage := RecieveMessageList[0];
    RecieveMessageList.Delete(0);

    FRecieveMessageCount := RecieveMessageList.Count;
  finally
    FRecieveMessageQueue.UnlockList;
  end;

  try
    Cmd := ResponseMessage.Command;
    ErrorMessage := False;
    // Led data, volume data...
    if ResponseMessage.IsLedData then
    begin
      USBProcessResponseMessage(ResponseMessage);
      Exit;
    end else begin
      // Are we waiting for a response?
      if (FWaitForCmd <> 0) then
      begin
        // Is this the responsemessage we have been waiting for

        if (Cmd = FWaitForCmd) or
           ((Cmd = $04) and (FWaitForCmd = $0c)) or
           ErrorMessage then
        begin
          // If g2 did not respond with error message, then process the send message
          tempMessage := CreateSendMessage; // Necessary?
          try
            tempMessage.Assign( g_bout_buf);
            DoReceiveResponseMessage( ResponseMessage);
            USBProcessResponseMessage( ResponseMessage);

            if not ErrorMessage then
              USBProcessSendMessage( tempMessage);
              //add_log_line( 'PROCESSED: ' + MidiDump(tempmessage, 6), LOGCMD_NUL);
            FWaitForCmd := 0; // Send buffer can send next message
          finally
            tempMessage.DisposeOf;
          end;

          // Send next message in a sequence, if assigned
          if assigned(GetSlot(0)) and assigned(GetSlot(0).OnNextInitStep) then
            GetSlot(0).DoNextInitStep
          else
          if assigned(GetSlot(1)) and assigned(GetSlot(1).OnNextInitStep)  then
            GetSlot(1).DoNextInitStep
          else
          if assigned(GetSlot(2)) and assigned(GetSlot(2).OnNextInitStep)  then
            GetSlot(2).DoNextInitStep
          else
          if assigned(GetSlot(3)) and assigned(GetSlot(3).OnNextInitStep)  then
            GetSlot(3).DoNextInitStep
          else
          if assigned(Performance) and assigned(GetPerformance.OnNextInitStep) then
            GetPerformance.DoNextInitStep
          else
            DoNextInitStep;

        end else begin
          add_log_line( DateTimeToStr(Now) + ' Message received out of sync (1). Size = ' + IntToStr(ResponseMessage.Size) + ' ' + MidiDump(ResponseMessage, ResponseMessage.Size), LOGCMD_ERR);
        end;

      end else begin
        //add_log_line( DateTimeToStr(Now) + ' Message received out of sync (2). Size = ' + IntToStr(ResponseMessage.Size) + ' ' + MidiDump(ResponseMessage, ResponseMessage.Size), LOGCMD_ERR);
        // Resource messages are received here...
        USBProcessResponseMessage(ResponseMessage);
      end;
    end;
  finally
    ResponseMessage.DisposeOf;
  end;
end;

procedure TG2USB.USBProcessSendMessage(aSendMessage : TG2SendMessage);
begin
  // Responseless messages that originate from the server are already processed in patch
  if (aSendMessage.HasResponse) then
  begin
    aSendMessage.Position := 0;
    if not ProcessSendMessage( aSendMessage, ID) then
       add_log_line( DateTimeToStr(Now) + ' Send message not processed.', LOGCMD_ERR);
  end;
end;

procedure TG2USB.USBProcessResponseMessage(ResponseMessage : TG2ResponseMessage);
var
  Size: Byte;
begin
  if ResponseMessage.IsEmbedded then
  begin
    ResponseMessage.Position := 1; // Skip first Byte if embedded
    Size := PStaticByteBuffer(ResponseMessage.Memory)^[0] shr 4;
    ResponseMessage.Size := Size;
  end else
    ResponseMessage.Position := 0;

  if not ProcessResponseMessage( ResponseMessage, 0) then
    add_log_line( DateTimeToStr(Now) + ' Response message not processed.', LOGCMD_ERR);

  if not( ResponseMessage.IsLedData) or FLogLedDataMessages then
  begin
    add_log_line( '', LOGCMD_NUL);
    add_log_line( 'Broadcast : ', LOGCMD_NUL);
    dump_buffer( PStaticByteBuffer( ResponseMessage.Memory)^, ResponseMessage.Size);
  end;
end;

procedure TG2USB.ReportState;
var
  RecieveMessageList: TList<TG2ResponseMessage>;
  MessageList: TList<TG2SendMessage>;
begin
  add_log_line( 'WaitForCmd:' + IntToStr(FWaitForCmd), LOGCMD_NUL);

  {$IFDEF RECIEVEQUEUE}
  RecieveMessageList := FRecieveMessageQueue.LockList;
  try
    FRecieveMessageCount := RecieveMessageList.Count;
  finally
    FRecieveMessageQueue.UnlockList;
  end;

  add_log_line('Recieve queue holds ' + IntToStr(FRecieveMessageCount), LOGCMD_NUL);
  {$ENDIF}

  {$IFDEF SENDTHREAD}
  MessageList := FSendMessageQueue.LockList;
  try
    FSendMessageCount := MessageList.Count;
  finally
    FSendMessageQueue.UnlockList;
  end;

  add_log_line('Send queue holds ' + IntToStr(FSendMessageCount), LOGCMD_NUL);
  {$ENDIF}

  if FReceiveMessageThread.Terminated then
    add_log_line('Recieve thread stopped.', LOGCMD_NUL)
  else
    add_log_line('Recieve thread running.', LOGCMD_NUL);

  {$IFDEF SENDTHREAD}
  if FSendMessageThread.Terminated then
    add_log_line('Send thread stopped.', LOGCMD_NUL)
  else
    add_log_line('Send thread running.', LOGCMD_NUL);
  {$ENDIF}

  add_log_line('Recieve alive ' + IntToStr(FRecieveAlive), LOGCMD_NUL);
  add_log_line('Total recieved interrupt ' + IntToStr(FTotalRecievedInterrupt), LOGCMD_NUL);
  add_log_line('Total recieved ' + IntToStr(FTotalRecieved), LOGCMD_NUL);
  add_log_line('Total send ' + IntToStr(FTotalSend), LOGCMD_NUL);
end;

//------------------------------------------------------------------------------
//
//                            TG2USB Messages
//
//------------------------------------------------------------------------------

{$IFDEF SENDTHREAD}
function TG2USB.SendCmdMessage(aSendMessage: TG2SendMessage): Boolean;
var
  MessageList: TList<TG2SendMessage>;
begin
   // Send message from server to G2 or from client to server
  // add_log_line( 'Sending message...', LOGCMD_NUL);
   // Return True if message was send
   Result := False;
   ErrorMessage := False;
   ErrorMessageNo := 0;
   //add_log_line( '', LOGCMD_NUL);
   //add_log_line( 'Send message, size = ' + IntToStr(aSendMessage.Size) , LOGCMD_NUL);
   //dump_buffer( aSendMessage.Memory^, aSendMessage.Size);
   // Online?
   if USBActive then
   begin
     aSendMessage.PrepareForSend;
     DoBeforeSendMessage( ID, aSendMessage);
     MessageList := FSendMessageQueue.LockList;
     try
       MessageList.Add(aSendMessage);
       FSendMessageCount := MessageList.Count;
     finally
       FSendMessageQueue.UnlockList;
     end;

   end else begin
     // Not online
     USBProcessSendMessage( aSendMessage);
     aSendMessage.DisposeOf;
     aSendMessage := nil;
     Result := True;
   end;
end;
{$ELSE}
function TG2USB.SendCmdMessage(aSendMessage: TG2SendMessage): Boolean;
begin
   // Send message from server to G2 or from client to server
   try
     if FWaitForCmd <> 0 then
     begin
       aSendMessage.DisposeOf;
       aSendMessage := nil;
       Exit; // Waiting for response, can't send a new command now
     end;

     // Return True if message was send
     Result := False;
     ErrorMessage := False;
     ErrorMessageNo := 0;
     //add_log_line( '', LOGCMD_NUL);
     //add_log_line( 'Send message, size = ' + IntToStr(aSendMessage.Size) , LOGCMD_NUL);
     //dump_buffer( aSendMessage.Memory^, aSendMessage.Size);
     // Online?
     if USBActive then
     begin
       aSendMessage.PrepareForSend;
       FWaitForCmd := aSendMessage.Command;
       g_bout_buf.Assign(aSendMessage);

       if FWaitForCmd = 0 then
       begin
         USBSendMessage(aSendMessage);
       end else begin
         if assigned(FOnBeforeSendMessage) then
           FOnBeforeSendMessage( self, ID, aSendMessage);
         // The message is freed after a response is received
         USBSendMessage( aSendMessage);
       end;

       Result := True;
     end else begin
       // Not online
       USBProcessSendMessage(aSendMessage);
       Result := True;
     end;

  finally
    aSendMessage.DisposeOf;
    aSendMessage := nil;
  end;
end;
{$ENDIF}

function TG2USB.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2USBPerformance.Create(self);
end;

procedure TG2USB.USBStartInit;
begin
  FInitStep := 1;
  NextBankListCmd.PatchFileType := pftPatch;
  NextBankListCmd.Bank := 0;
  NextBankListCmd.Patch := 0;
  FOnNextInitStep := USBInitSeq;
  SendInitMessage;
end;

procedure TG2USB.USBInitSeq(Sender: TObject);
var
  do_next_step: Boolean;
begin
  do_next_step := true;
  try
    case FInitStep of
    0 : SendInitMessage;
    1 : SendStartStopCommunicationMessage( STOP_COMM);
    2 : SendGetPatchVersionMessage( GetPerformance.FPerfVersion);
    3 : SendGetSynthSettingsMessage;
    4 : SendUnknown1Message;
    5 : GetPerformance.USBStartInit( False);
    6 : if NextBankListCmd.PatchFileType <> pftEnd then
        begin
          SendListMessage( NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
          do_next_step := false;
        end else begin
          SendStartStopCommunicationMessage( START_COMM);
          OnNextInitStep := nil;
          FInitialized := True;
          DoAfterG2Init;
        end;
    end;

    if do_next_step then
      inc(FInitStep);

  except on E:Exception do
  begin
      FInitialized := False;
      OnNextInitStep := nil;
    end;
  end;
end;

procedure TG2USB.NextListMessage(Sender: TObject);
begin
  if NextBankListCmd.PatchFileType <> pftEnd then
  begin
    SendListMessage(NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
  end else
    OnNextInitStep := nil;
end;

procedure TG2USB.NextBankUploadMessage(Sender: TObject);
var
  BankItem: TBankItem;
begin
  BankItem := BankList.FindNext( NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch);

  if assigned(BankItem) then
    SendUploadBankMessage( BankItem.PatchFileType, BankItem.Bank, BankItem.Patch)
  else begin
    // Last one send, save the bank dump list
    BankDumpList.SaveToFile( BankDumpFolder + BankDumpFileName);
    OnNextInitStep := nil;
  end;
end;

procedure TG2USB.RefreshBanks;
begin
  NextBankListCmd.PatchFileType := pftPatch;
  NextBankListCmd.Bank := 0;
  NextBankListCmd.Patch := 0;
  SendListMessage(NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
  OnNextInitStep := NextListMessage;
end;

procedure TG2USB.USBSentStartComm(Sender: TObject);
begin
  // Usually the last step
  SendStartStopCommunicationMessage(START_COMM);
end;

procedure TG2USB.SendInitMessage;
begin
  SendCmdMessage(CreateInitMessage);
end;

procedure TG2USB.SetPerfMode(aValue: TBits1);
begin
  inherited;

  SendSetModeMessage(aValue); // 0:Patch mode 1:Performance mode
  (Performance as TG2USBPerformance).USBStartInit( True);
end;

procedure TG2USB.SendStartStopCommunicationMessage( Stop : Byte);
begin
  SendCmdMessage(CreateStartStopCommunicationMessage(Stop));
end;

procedure TG2USB.SendGetMasterClockMessage;
begin
  SendCmdMessage(CreateGetMasterClockMessage);
end;

procedure TG2USB.SendGetPatchVersionMessage;
begin
  SendCmdMessage(CreateGetPatchVersionMessage);
end;

procedure TG2USB.SendGetAssignedVoicesMessage;
begin
  SendCmdMessage(CreateGetAssignedVoicesMessage);
end;

procedure TG2USB.SendGetSynthSettingsMessage;
begin
  SendCmdMessage(CreateGetSynthSettingsMessage);
end;

procedure TG2USB.SendSetSynthSettingsMessage;
begin
  SendCmdMessage(CreateSetSynthSettingsMessage);
end;

procedure TG2USB.SendUnknown1Message;
begin
  SendCmdMessage(CreateUnknown1Message);
end;

procedure TG2USB.SendUploadBankMessage(aPatchFileType: TPatchFileType; aBank,
  aLocation: Byte);
begin
  {if aLocation = 0 then begin
    // First one, init bank dump list
    BankDumpList.Clear;
    BankDumpList.Add('Version=Nord Modular G2 Bank Dump');
    case aPatchFileType of
      pftPatch :
        begin
          BankDumpFileName := 'PatchBank' + IntToStr(aBank + 1) + '.pchList';
        end;
      pftPerf :
        begin
          BankDumpFileName := 'PerfBank' + IntToStr(aBank + 1) + '.pchList';
        end;
      else
        raise Exception.Create('Unknown patch file type.');
    end;
  end;}
  OnNextInitStep := NextBankUploadMessage;
  SendCmdMessage(CreateUploadBankMessage(aPatchFileType, aBank, aLocation));
end;

procedure TG2USB.NextPatchBankDownloadMessage(Sender: TObject);
var
  OriginalBank, OriginalLocation: Byte;
  PatchFileName: string;
begin
  inc(BankDumpListIndex);
  if BankDumpListIndex < BankDumpList.Count then
  begin
    OnNextInitStep := NextPatchBankDownloadMessage;

    if ParseBankDumpListLine( BankDumpList[BankDumpListIndex], OriginalBank, OriginalLocation, PatchFileName) then
    begin
      SendDownLoadPatchBankMessage( BankDumpDestBank, BankDumpListIndex - 1, BankDumpFolder + PatchFileName);
    end else begin
      OnNextInitStep := nil;
      raise Exception.Create('Error reading patch dump file.');
    end

  end else begin
    OnNextInitStep := nil;
    DoAfterBankDownload( pftPatch);
    //if assigned(OnAfterBankDownload) then
    //  OnAfterBankDownload( Self, ID, pftPatch);
  end;
end;

procedure TG2USB.SendDownloadPatchBankMessage(aBank, aLocation: Byte; aFileName: string);
var
  Patch: TG2FilePatch;
  PatchName: string;
  FileStream : TFileStream;
begin
  Patch := TG2FilePatch.Create(nil);

  FileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    try
      Patch.LoadFromFile(FileStream, LogLines);
      PatchName := PatchNameFromFileName(aFileName);
      SendCmdMessage( CreateDownloadPatchBankMessage(aBank, aLocation, PatchName, Patch));

    except on E:Exception do
    begin
        add_log_line('Error loading patch ' + PatchName + ', patch download aborted.', LOGCMD_ERR);
        OnNextInitStep := nil;
      end;
    end;
  finally
    FileStream.DisposeOf;
    Patch.DisposeOf;
  end;
end;

procedure TG2USB.NextPerfBankDownloadMessage(Sender: TObject);
var
  OriginalBank, OriginalLocation: Byte;
  PatchFileName: string;
begin
  inc(BankDumpListIndex);

  if BankDumpListIndex < BankDumpList.Count then
  begin
    OnNextInitStep := NextPerfBankDownloadMessage;
    if ParseBankDumpListLine( BankDumpList[BankDumpListIndex], OriginalBank, OriginalLocation, PatchFileName) then
    begin
      SendDownLoadPerfBankMessage( BankDumpDestBank, BankDumpListIndex - 1, BankDumpFolder + PatchFileName);
    end else begin
      OnNextInitStep := nil;
      raise Exception.Create('Error reading patch dump file.');
    end
  end else begin
    OnNextInitStep := nil;
    DoAfterBankDownload( pftPerf);
    //if assigned(OnAfterBankDownload) then
    //  OnAfterBankDownload( Self, ID, pftPerf);
  end;
end;

procedure TG2USB.SendDownloadPerfBankMessage(aBank, aLocation: Byte; aFileName : string);
var
  Perf: TG2FilePerformance;
  PerfName: string;
  FileStream: TFileStream;
begin
  Perf := TG2FilePerformance.Create(nil);
  FileStream := TFileStream.Create(aFileName, fmOpenRead);
  try
    try
      Perf.LoadFromFile(FileStream, LogLines);
      PerfName := PatchNameFromFileName(aFileName);
      SendCmdMessage(CreateDownloadPerfBankMessage(aBank, aLocation, PerfName, Perf));

      except on E:Exception do
      begin
        add_log_line('Error loading patch ' + PerfName + ', patch download aborted.', LOGCMD_ERR);
        OnNextInitStep := nil;
      end;
    end;
  finally
    FileStream.DisposeOf;
    Perf.DisposeOf;
  end;
end;

procedure TG2USB.SendDumpMidiMessage;
begin
  SendCmdMessage(CreateMidiDumpMessage);
end;

procedure TG2USB.SendListMessage(aPatchFileType : TPatchFileType; aBank, aPatch : Byte; names : TStrings);
begin
  SendCmdMessage(CreateListMessage(aPatchFileType, aBank, aPatch, Names));
end;

procedure TG2USB.SendSetModeMessage(aMode : Byte);
begin
  SendCmdMessage( CreateSetModeMessage( aMode));
end;

procedure TG2USB.SendNoteMessage(aNote: Byte; aOnoff: Byte);
begin
  SendCmdMessage(CreateNoteMessage( aNote, aOnOff));
end;

procedure TG2USB.SendRetrieveMessage(aSlot, aBank, aPatch: Byte);
begin
  if assigned(OnBeforePatchUpdate) then
    OnBeforePatchUpdate(Self, ID, aSlot);

  SendCmdMessage(CreateRetrieveMessage(aSlot, aBank, aPatch));
end;

procedure TG2USB.SendStoreMessage(aSlot, aBank, aPatch: Byte);
begin
  SendCmdMessage(CreateStoreMessage(aSlot, aBank, aPatch));
end;

procedure TG2USB.SendClearBankMessage(aPatchFileType: TPatchFileType; aBank, aFromLocation, aToLocation: Byte);
begin
  SendCmdMessage(CreateClearBankMessage(aPatchFileType, aBank, aFromLocation, aToLocation));
end;

procedure TG2USB.SendClearMessage(aPatchFileType: TPatchFileType; aBank, aPatch: Byte);
begin
  SendCmdMessage(CreateClearMessage(aPatchFileType, aBank, aPatch));
end;

//------------------------------------------------------------------------------
//
//                            TG2USBPerformance
//
//------------------------------------------------------------------------------

constructor TG2USBPerformance.Create(AOwner: TComponent);
begin
  inherited;
  FPerfVersion := 0;
  FOnNextInitStep := nil;
end;

destructor TG2USBPerformance.Destroy;
begin
  inherited;
end;

procedure TG2USBPerformance.DoAfterRetreivePatch(Slot, Bank, Patch: Byte);
begin
  inherited;
  USBStartInit( True);
end;

procedure TG2USBPerformance.DoNextInitStep;
begin
  if assigned(FOnNextInitStep) then
    FOnNextInitStep(self);
end;

function TG2USBPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2USBSlot.Create( self);
end;

function TG2USBPerformance.GetInitializing: Boolean;
begin
  Result := assigned(FOnNextInitStep);
end;

function TG2USBPerformance.GetSlot(aSlot: Byte): TG2USBSlot;
begin
  Result := Slot[aSlot] as TG2USBSlot;
end;

function TG2USBPerformance.SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage(SendMessage)
  else
    Result := False;
end;

procedure TG2USBPerformance.USBStartInit(aStartCommAfterInit: Boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPerfSettingsMessage;
end;

procedure TG2USBPerformance.USBStartInitWithName;
begin
  FInitStep := 0;
  FStartCommAfterInit := False;
  FOnNextInitStep := USBInitSeq;
  SendGetPerfSettingsMessage;
end;

procedure TG2USBPerformance.USBInitSeq(Sender: TObject);
begin
  FInitMidiStep := 0;

  while (FInitStep <= 10) do
  begin
    case FInitStep of
     0 : begin
           SendGetPerfSettingsMessage;
           inc(FInitStep);
           Exit;
         end;
     1 : begin
           SendUnknown2Message;
           inc(FInitStep);
           Exit;
         end;
     2 : begin
           GetSlot(0).USBStartInit( False);
           inc(FInitStep);
           Exit;
         end;
     3 : begin
           GetSlot(1).USBStartInit( False);
           inc(FInitStep);
           Exit;
         end;
     4 : begin
           GetSlot(2).USBStartInit( False);
           inc(FInitStep);
           Exit;
         end;
     5 : begin
           GetSlot(3).USBStartInit( False);
           inc(FInitStep);
           Exit;
         end;
     6 : begin
           (G2 as TG2USB).SendGetAssignedVoicesMessage;
           inc(FInitStep);
           Exit;
         end;
     7 : begin
           //(G2 as TG2USB).SendGetMasterClockMessage;
           SendGetGlobalKnobsMessage;
           inc(FInitStep);
           Exit;
         end;
     8 : begin
           if G2.AutoAssignMidi then begin
             if USBInitMidiStep then
               Exit;
           end else
             inc(FInitStep);
         end;
     9 : begin
           //SendGetGlobalKnobsMessage;
           (G2 as TG2USB).SendGetMasterClockMessage;
           inc(FInitStep);
           if not FStartCommAfterInit then
           begin
             OnNextInitStep := nil;
             (G2 as TG2USB).DoAfterPerfInit;
           end;
           Exit;
         end;
     10 : begin
           (G2 as TG2USB).SendStartStopCommunicationMessage(START_COMM);
           inc(FInitStep);
           OnNextInitStep := nil;
           (G2 as TG2USB).DoAfterPerfInit;
           Exit;
         end;
    end;
  end;
end;

function TG2USBPerformance.USBInitMidiStep: Boolean;
var
  Knob: TGlobalKnob;
  MidiCC: Byte;
  KnobIndex: Byte;
  Param: TG2FileParameter;
  Patch: TG2USBPatch;

  function AssignMidiCC(aPatch : TG2USBPatch; aParam: TG2FileParameter; aMidiCC: Byte): Boolean;
  begin
    // Returns True if as message is send
    Result := False;
    if assigned( aParam.Controller) then
    begin
      if aParam.Controller.MidiCC <> aMidiCC then
      begin
        aPatch.MessDeassignMidiCC(aParam.Controller.MidiCC);
        Result := True;
      end else
        inc(FInitMidiStep); // Already connected to the CC
    end else begin
      aPatch.MessAssignMidiCC(
           aParam.Location,
           aParam.ModuleIndex,
           aParam.ParamIndex,
           aMidiCC);
      Result := True;
      inc(FInitMidiStep);
    end;
  end;

begin
  // FControlType : TG2ControlType; // 0=Knob 1=Button
  // FKnob : Integer;
  // FMidiCC : Integer;

  // Return True if a message is send
  Result := False;

  if FInitMidiStep < G2.MidiToKnobList.Count then
  begin
    MidiCC := G2.MidiToKnobList[FInitMidiStep].FMidiCC;
    KnobIndex := G2.MidiToKnobList[FInitMidiStep].FKnob;

    case G2.MidiToKnobList[FInitMidiStep].FSource of
      g2psGlobal :
        begin
          Knob := GlobalKnobList[ KnobIndex];
          case G2.MidiToKnobList[FInitMidiStep].FControlType of
            g2ctKnob :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) then
                begin
                  Patch := Slot[ Knob.SlotIndex].Patch as TG2USBPatch;
                  if AssignMidiCC( Patch, Knob.Parameter, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) and assigned(Knob.Parameter.ButtonParam) then
                begin
                  Patch := Slot[ Knob.SlotIndex].Patch as TG2USBPatch;
                  if AssignMidiCC( Patch, Knob.Parameter.ButtonParam, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            else
              inc(FInitMidiStep);
          end;
        end;
      else
       inc(FInitMidiStep);
    end;
  end else
    inc(FInitStep);
end;

procedure TG2USBPerformance.AutoAssignMidiToKnobs;
var
  i: Integer;
  MidiCC: Byte;
  GlobalKnob: TGlobalKnob;
  Patch: TG2USBPatch;
begin
  for i := 0 to GlobalKnobList.Count - 1 do
  begin
    MidiCC := G2.FindMidiToKnob( g2psGlobal, g2ctKnob, i);
    if MidiCC <> 0 then
    begin
      GlobalKnob :=  GlobalKnobList[i];
      if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) and assigned(GlobalKnob.Parameter) then
      begin
        Patch :=  Slot[ GlobalKnob.SlotIndex].Patch as TG2USBPatch;
        if (not assigned(GlobalKnob.Parameter.Controller)) or (GlobalKnob.Parameter.Controller.MidiCC <> MidiCC) then
          Patch.MessAssignMidiCC(GlobalKnob.Parameter.Location, GlobalKnob.Parameter.ModuleIndex, GlobalKnob.Parameter.ParamIndex, MidiCC);
      end;
    end;
  end;
end;

{procedure TG2USBPerformance.USBStartInitPerf;
begin
  FInitStep := 1;
  FOnNextInitStep := USBInitPerfSeq;
  (G2 as TG2USB).SendGetPatchVersionMessage( FPerfVersion);
end;

procedure TG2USBPerformance.USBInitPerfSeq(Sender: TObject);
begin
  case FInitStep of
   0  : (G2 as TG2USB).SendGetPatchVersionMessage( FPerfVersion);
   1  : SendSetPerformanceMessage( 'Empty perf', self);
   2  : SendUnknown2Message;
   3  : GetSlot(0).SendGetParamsMessage( ord(ltPatch));
   4  : GetSlot(0).SendGetParamNamesMessage( ord(ltPatch));
   5  : GetSlot(0).SendUnknown6Message;
   6  : GetSlot(0).SendGetSelectedParameterMessage;
   7  : GetSlot(1).SendGetParamsMessage( ord(ltPatch));
   8  : GetSlot(1).SendGetParamNamesMessage( ord(ltPatch));
   9  : GetSlot(1).SendUnknown6Message;
   10 : GetSlot(1).SendGetSelectedParameterMessage;
   11 : GetSlot(2).SendGetParamsMessage( ord(ltPatch));
   12 : GetSlot(2).SendGetParamNamesMessage( ord(ltPatch));
   13 : GetSlot(2).SendUnknown6Message;
   14 : GetSlot(2).SendGetSelectedParameterMessage;
   15 : GetSlot(3).SendGetParamsMessage( ord(ltPatch));
   16 : GetSlot(3).SendGetParamNamesMessage( ord(ltPatch));
   17 : GetSlot(3).SendUnknown6Message;
   18 : GetSlot(3).SendGetSelectedParameterMessage;
   19 : (G2 as TG2USB).SendGetAssignedVoicesMessage;
   20 : (G2 as TG2USB).SendGetMasterClockMessage;
   21 : begin
          SendGetGlobalKnobsMessage;
          OnNextInitStep := nil;
        end;
  end;
  inc(FInitStep);
end;}

procedure TG2USBPerformance.SendGetPerfSettingsMessage;
begin
  SendCmdMessage(CreateGetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendUnknown2Message;
begin
  SendCmdMessage(CreateUnknown2Message);
end;

procedure TG2USBPerformance.SetMasterClock(aValue: TBits8);
begin
  inherited;
  SendSetPerfSettingsMessage;
end;

procedure TG2USBPerformance.SetMasterClockRun(aValue: TBits8);
begin
  inherited;
  SendSetPerfSettingsMessage;
end;

procedure TG2USBPerformance.SendSelectSlotMessage(aSlot: Byte);
begin
  SendCmdMessage(CreateSelectSlotMessage(aSlot));
end;

procedure TG2USBPerformance.SendSetPerformanceMessage(aPerfName: string; aPerf: TG2FilePerformance);
begin
  if assigned(G2) and assigned((G2 as TG2Mess).OnBeforePatchUpdate) then
    (G2 as TG2Mess).OnBeforePatchUpdate(Self, G2.ID, 4);

  SendCmdMessage( CreateSetPerformanceMessage( aPerfName, aPerf));
end;

procedure TG2USBPerformance.SendSetPerfSettingsMessage;
begin
  SendCmdMessage(CreateSetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendSetMasterClockBPMMessage;
begin
  SendCmdMessage(CreateSetMasterClockBPMMessage(BPM));
end;

procedure TG2USBPerformance.SendSetMasterClockRunMessage;
begin
  SendCmdMessage(CreateSetMasterClockRunMessage(Start));
end;

procedure TG2USBPerformance.SendSetPerfNameMessage(aPerfName: string);
begin
  SendCmdMessage(CreateSetPerfNameMessage(aPerfName));
end;

procedure TG2USBPerformance.SendGetGlobalKnobsMessage;
begin
  SendCmdMessage(CreateGetGlobalKnobsMessage);
end;

//------------------------------------------------------------------------------
//
//                              TG2USBSlot
//
//------------------------------------------------------------------------------

constructor TG2USBSlot.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FParamUpdBuf, 100);
  FParamUpdBufCount := 0;
end;

destructor TG2USBSlot.Destroy;
begin
  Finalize(FParamUpdBuf);
  inherited;
end;

procedure TG2USBSlot.DoAfterRetreivePatch(Slot, Bank, Patch: Byte);
begin
  inherited;
  USBStartInit(True);
end;

procedure TG2USBSlot.DoNextInitStep;
begin
  if assigned(FOnNextInitStep) then
    FOnNextInitStep(self);
end;

function TG2USBSlot.CreatePatch: TG2FilePatch;
begin
  Result := TG2USBPatch.Create( self);
end;

function TG2USBSlot.GetInitializing: Boolean;
begin
  Result := assigned(FOnNextInitStep);
end;

function TG2USBSlot.GetPatch: TG2USBPatch;
begin
  if not assigned(Patch) then
    raise Exception.Create('Patch in slot unassigned!');
  Result := Patch as TG2USBPatch;
end;

function TG2USBSlot.GetPerformance: TG2USBPerformance;
begin
  if not assigned(Performance) then
    raise Exception.Create('Performance not assigned to slot.');
  Result := Performance as TG2USBPerformance;
end;

procedure TG2USBSlot.Init;
begin
  inherited;
  FParamUpdBufCount := 0;
end;

procedure TG2USBSlot.USBStartInit(aStartCommAfterInit: Boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPatchVersionMessage;
end;

procedure TG2USBSlot.USBInitSeq(Sender: TObject);
begin
  FInitMidiStep := 0;
  while FInitStep <= 10 do
  begin
    case FInitStep of
     0 : begin
           SendGetPatchVersionMessage;
           inc(FInitStep);
           Exit;
         end;
     1 : begin
           SendGetPatchMessage;
           inc(FInitStep);
           Exit;
         end;
     2 : begin
           SendGetPatchNameMessage;
           inc(FInitStep);
           Exit;
         end;
     3 : begin
           SendCurrentNoteMessage;
           inc(FInitStep);
           Exit;
         end;
     4 : begin
           SendPatchNotesMessage;
           inc(FInitStep);
           Exit;
         end;
     5 : begin
           SendResourceTableMessage(LOCATION_VA);
           inc(FInitStep);
           Exit;
         end;
     6 : begin
           SendResourceTableMessage(LOCATION_FX);
           inc(FInitStep);
           Exit;
         end;
     7 : begin
           if G2.AutoAssignMidi then
           begin
             if USBInitMidiStep then
               Exit;
           end else
             inc(FInitStep);
         end;
     8 : begin
           SendUnknown6Message;
           inc(FInitStep);
           Exit;
         end;
     9 : begin
           SendGetSelectedParameterMessage;
           inc(FInitStep);
           if not FStartCommAfterInit then
           begin
             OnNextInitStep := nil;
             (G2 as TG2USB).DoAfterSLotInit;
           end;
           Exit;
         end;
     10 : begin
           (G2 as TG2USB).SendStartStopCommunicationMessage(START_COMM);
           inc(FInitStep);
           OnNextInitStep := nil;
           (G2 as TG2USB).DoAfterSlotInit;
           Exit;
         end;
    end;
    //inc(FInitStep);
  end;
end;

function TG2USBSlot.USBInitMidiStep: Boolean;
var
  Knob: TKnob;
  MidiCC: Byte;
  KnobIndex: Byte;
  Param: TG2FileParameter;

  function AssignMidiCC(aParam : TG2FileParameter; aMidiCC: Byte): Boolean;
  begin
    // Returns True if a messages is send
    Result := False;
    if assigned( aParam.Controller) then
    begin
      if aParam.Controller.MidiCC <> aMidiCC then
      begin
        (Patch as TG2USBPatch).MessDeassignMidiCC(aParam.Controller.MidiCC);
        Result := True;
      end else
        inc(FInitMidiStep); // Already connected to the CC
    end else begin
      (Patch as TG2USBPatch).MessAssignMidiCC(
           aParam.Location,
           aParam.ModuleIndex,
           aParam.ParamIndex,
           aMidiCC);
      Result := True;
      inc(FInitMidiStep);
    end;
  end;

begin
  // FControlType : TG2ControlType; // 0=Knob 1=Button
  // FKnob : Integer;
  // FMidiCC : Integer;

  // Return True if a message is send
  Result := False;

  if FInitMidiStep < G2.MidiToKnobList.Count then
  begin
    MidiCC := G2.MidiToKnobList[FInitMidiStep].FMidiCC;
    KnobIndex := G2.MidiToKnobList[FInitMidiStep].FKnob;

    case G2.MidiToKnobList[FInitMidiStep].FSource of
      g2psParam :
        begin
          Knob := Patch.KnobList[ KnobIndex];
          case G2.MidiToKnobList[FInitMidiStep].FControlType of
            g2ctKnob :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) then
                begin
                  if AssignMidiCC(Knob.Parameter, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) and assigned(Knob.Parameter.ButtonParam) then
                begin
                  if AssignMidiCC(Knob.Parameter.ButtonParam, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
          end;
        end;
      g2psMorph :
        begin
          case G2.MidiToKnobList[FInitMidiStep].FControlType of
            g2ctKnob :
              begin
                Param := Patch.GetMorphKnobParameter(KnobIndex);
                if assigned(Param) then
                begin
                  if AssignMidiCC(Param, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                Param := Patch.GetMorphKnobParameter(KnobIndex+8);
                if assigned(Param) then
                begin
                  if AssignMidiCC(Param, MidiCC) then
                  begin
                    Result := True;
                    Exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
          end;
        end;
      g2psPatch :
        begin
          case G2.MidiToKnobList[FInitMidiStep].FControlType of
            g2ctKnob :
              begin
                Param := Patch.GetPatchKnobParameter(KnobIndex);
                if AssignMidiCC(Param, MidiCC) then
                begin
                  Result := True;
                  Exit;
                end;
              end;
            g2ctButton :
              begin
                Param := Patch.GetPatchKnobParameter(KnobIndex+8);
                if AssignMidiCC(Param, MidiCC) then
                begin
                  Result := True;
                  Exit;
                end;
              end;
            else
              inc(FInitMidiStep);
          end;
        end;
    end;
  end else
    inc(FInitStep);
end;

procedure TG2USBSlot.AutoAssignMidiToKnobs;
var
  i: Integer;
  MidiCC: Byte;
  Knob: TKnob;
  Parameter: TG2FileParameter;
begin
  for i := 0 to Patch.KnobList.Count - 1 do
  begin
    MidiCC := G2.FindMidiToKnob( g2psParam, g2ctKnob, i);
    if MidiCC <> 0 then
    begin
      Knob := Patch.KnobList[i];
      if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) then
      begin
        if (not assigned(Knob.Parameter.Controller)) or (Knob.Parameter.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC( Knob.Parameter.Location, Knob.Parameter.ModuleIndex, Knob.Parameter.ParamIndex, MidiCC);
      end;
    end;
    MidiCC := G2.FindMidiToKnob( g2psParam, g2ctButton, i);
    if MidiCC <> 0 then
    begin
      Knob := Patch.KnobList[i];
      if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) and assigned(Knob.Parameter.ButtonParam) then
      begin
        if (not assigned(Knob.Parameter.ButtonParam.Controller)) or (Knob.Parameter.ButtonParam.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC( Knob.Parameter.ButtonParam.Location, Knob.Parameter.ButtonParam.ModuleIndex, Knob.Parameter.ButtonParam.ParamIndex, MidiCC);
      end;
    end;
  end;

  for i := 0 to 7 do
  begin
    MidiCC := G2.FindMidiToKnob(g2psMorph, g2ctKnob, i);
    if MidiCC <> 0 then
    begin
      Parameter := Patch.GetMorphKnobParameter(i);
      if assigned(Parameter) then
        if (not assigned(Parameter.Controller)) or (Parameter.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC(Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
    end;

    MidiCC := G2.FindMidiToKnob(g2psMorph, g2ctButton, i);

    if MidiCC <> 0 then
    begin
      Parameter := Patch.GetMorphKnobParameter(i+8);
      if assigned(Parameter) then
        if (not assigned(Parameter.Controller)) or (Parameter.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
    end;
  end;

  for i := 0 to 7 do
  begin
    MidiCC := G2.FindMidiToKnob(g2psPatch, g2ctKnob, i);
    if MidiCC <> 0 then
    begin
      Parameter := Patch.GetPatchKnobParameter(i);
      if assigned(Parameter) and (Parameter.MidiAssignmentsAllowed) then
        if (not assigned(Parameter.Controller)) or (Parameter.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC(Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
    end;

    MidiCC := G2.FindMidiToKnob(g2psPatch, g2ctButton, i);
    if MidiCC <> 0 then
    begin
      Parameter := Patch.GetPatchKnobParameter(i+8);
      if assigned(Parameter) and (Parameter.MidiAssignmentsAllowed) then
        if (not assigned(Parameter.Controller)) or (Parameter.Controller.MidiCC <> MidiCC) then
          (Patch as TG2USBPatch).MessAssignMidiCC( Parameter.Location, Parameter.ModuleIndex, Parameter.ParamIndex, MidiCC);
    end;
  end;
end;

function TG2USBSlot.SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage(SendMessage)
  else
    Result := False;
end;

procedure TG2USBSlot.SendGetPatchVersionMessage;
begin
  SendCmdMessage(CreateGetPatchVersionMessage);
end;

procedure TG2USBSlot.SendPatchNotesMessage;
begin
  SendCmdMessage(CreatePatchNotesMessage);
end;

procedure TG2USBSlot.SendControllerSnapshotMessage;
begin
  SendCmdMessage(CreateSendControllerSnapshotMessage);
end;

procedure TG2USBSlot.SendResourceTableMessage(aLocation : Byte);
begin
  SendCmdMessage(CreateResourceTableMessage(aLocation));
end;

procedure TG2USBSlot.SendGetPatchNameMessage;
begin
  SendCmdMessage(CreateGetPatchNameMessage);
end;

procedure TG2USBSlot.SendCurrentNoteMessage;
begin
  SendCmdMessage(CreateCurrentNoteMessage);
end;

procedure TG2USBSlot.SendUnknown6Message;
begin
  SendCmdMessage(CreateUnknown6Message);
end;

procedure TG2USBSlot.SendGetSelectedParameterMessage;
begin
  SendCmdMessage(CreateGetSelectedParameterMessage);
end;

procedure TG2USBSlot.SendSetPatchMessage(aPatchName: string; aPatch: TG2FilePatch);
begin
  if assigned(G2) and assigned((G2 as TG2Mess).OnBeforePatchUpdate) then
    (G2 as TG2Mess).OnBeforePatchUpdate(Self, G2.ID, SlotIndex);
  SendCmdMessage(CreateSetPatchMessage(aPatchName, aPatch));
end;

procedure TG2USBSlot.SendSetPatchName(aPatchName: string);
begin
  SendCmdMessage(CreateSetPatchName(aPatchName));
end;

procedure TG2USBSlot.SendGetParamNamesMessage(aLocation: Byte);
begin
  SendCmdMessage(CreateGetParamNamesMessage(aLocation));
end;

procedure TG2USBSlot.SendGetParamsMessage(aLocation: Byte);
begin
  SendCmdMessage(CreateGetParamsMessage(aLocation));
end;

procedure TG2USBSlot.SendGetPatchMessage;
begin
  SendCmdMessage(CreateGetPatchMessage);
end;

procedure TG2USBSlot.SendSelectVariationMessage(aVariationIndex: Byte);
begin
  SendCmdMessage(CreateSelectVariationMessage(aVariationIndex));
end;

procedure TG2USBSlot.SendSetModeMessage(aLocation, aModule, aParam, aValue: Integer);
begin
  SendCmdMessage(CreateSetModeMessage(aLocation, aModule, aParam, aValue));
end;

procedure TG2USBSlot.SendCopyVariationMessage(aFromVariation, aToVariation: Byte);
begin
  SendCmdMessage(CreateCopyVariationMessage(aFromVariation, aToVariation));
end;

function TG2USBSlot.AddParamUpdRec(aSubCmd, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: Byte): Boolean;
var
  i: Integer;
begin
  // Return true if param value in table was changed
  Result := False;
  // Todo : replace by fast search and add sorted
  i := 0;
  while (i < FParamUpdBufCount) and not(( FParamUpdBuf[i].SubCmd = aSubCmd)
                                    and ( FParamUpdBuf[i].Location = aLocation)
                                    and ( FParamUpdBuf[i].Module = aModule)
                                    and ( FParamUpdBuf[i].Param = aParam)
                                    and ( FParamUpdBuf[i].Morph = aMorph)
                                    and ( FParamUpdBuf[i].Variation = aVariation)) do
    inc(i);

  if not(i < FParamUpdBufCount) then
  begin
    // Not found, add
    if (FParamUpdBufCount + 1) >= Length( FParamUpdBuf) then
      SetLength(FParamUpdBuf, Length( FParamUpdBuf) + 100); // Increase buffersize

    FParamUpdBuf[FParamUpdBufCount].SubCmd := aSubCmd;
    FParamUpdBuf[FParamUpdBufCount].Location := aLocation;
    FParamUpdBuf[FParamUpdBufCount].Module := aModule;
    FParamUpdBuf[FParamUpdBufCount].Param := aParam;
    FParamUpdBuf[FParamUpdBufCount].Morph := aMorph;
    FParamUpdBuf[FParamUpdBufCount].Value := 128; // Clavia max is 127, so this means "unitialized"
    FParamUpdBuf[FParamUpdBufCount].Negative := 0;
    FParamUpdBuf[FParamUpdBufCount].Variation := aVariation;

    Inc(FParamUpdBufCount);
  end;

  if (FParamUpdBuf[i].Value <> aValue) or (FParamUpdBuf[i].Negative <> aNegative) then
  begin
    FParamUpdBuf[i].Value := aValue;
    FParamUpdBuf[i].Negative := aNegative;
    FParamUpdBuf[i].Changed := True;
    Result := True;
  end;
end;

procedure TG2USBSlot.SendSetParamMessage(aLocation, aModule, aParam, aValue,
  aVariation: Byte);
begin
  AddParamUpdRec(S_SET_PARAM, aLocation, aModule, aParam, 0, aValue, 0, aVariation);
end;

procedure TG2USBSlot.SendSelParamMessage(aLocation, aModule, aParam: Integer);
begin
  AddParamUpdRec(S_SEL_PARAM, aLocation, aModule, aParam, 0, 0, 0, 0);
end;

procedure TG2USBSlot.SendSetMorphMessage(aLocation, aModule, aParam, aMorph,
  aValue, aNegative, aVariation: Byte);
begin
  AddParamUpdRec(S_SET_MORPH_RANGE, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation);
end;

//------------------------------------------------------------------------------
//
//                               TG2USBPatch
//
//------------------------------------------------------------------------------

constructor TG2USBPatch.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TG2USBPatch.Destroy;
begin
  inherited;
end;

function TG2USBPatch.GetPerformance: TG2USBPerformance;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');
  Result := (Slot as TG2USBSlot).GetPerformance;
end;

function TG2USBPatch.SendCmdMessage(SendMessage: TG2SendMessage): Boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBPatch.SendUndoMessage;
var
  MemStream: TG2SendMessage;
begin
  if FUndoStack.Count > 0 then
  begin
    // Send over usb
    MemStream := PopUndoStack;
    (G2 as TG2USB).SendCmdMessage(MemStream);
  end;
end;

procedure TG2USBPatch.SetModeValue(aLocation: TLocationType; aModuleIndex,
  aParameterIndex, aValue: Byte);
begin
  (Slot as TG2USBSlot).SendSetModeMessage(Ord(aLocation), aModuleIndex, aParameterIndex, aValue);
end;

procedure TG2USBPatch.SetMorphValue(aLocation: TLocationType; aModuleIndex,
  aParameterIndex, aMorphIndex, aValue, aVariation: Byte);
begin
  if aValue >= 128 then
    (Slot as TG2USBSlot).SendSetMorphMessage(Ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, abs(aValue - 256), 1, aVariation)
  else
    (Slot as TG2USBSlot).SendSetMorphMessage(Ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, aValue, 0, aVariation);

  inherited SetMorphValue(aLocation, aModuleIndex, aParameterIndex, aMorphIndex, aValue, aVariation);
end;

procedure TG2USBPatch.SetParamValue(aLocation: TLocationType; aModuleIndex, aParameterIndex, aVariation : Byte; aValue: Byte);
begin
  (Slot as TG2USBSlot).SendSetParamMessage(Ord(aLocation), aModuleIndex, aParameterIndex, aValue, aVariation);

  inherited SetParamValue(aLocation, aModuleIndex, aParameterIndex, aVariation, aValue);
end;

procedure TG2USBPatch.SelectParam(aLocation: TLocationType; aModuleIndex,
  aParameterIndex: Byte);
begin
  (Slot as TG2USBSlot).SendSelParamMessage(Ord(aLocation), aModuleIndex, aParameterIndex);

  inherited SelectParam(aLocation, aModuleIndex, aParameterIndex);
end;

procedure TG2USBPatch.SetVoiceCount(aValue: Byte);
var
  FPatchDescription: TPatchDescription;
begin
  if aValue > 32 then
    Exit;

  FPatchDescription := PatchDescription;
  if aValue > 0 then
  begin
    FPatchDescription.MonoPoly := 0; // Poly
    FPatchDescription.VoiceCount := aValue;
  end else begin
    if FPatchDescription.MonoPoly = 0 then
      FPatchDescription.MonoPoly := 1; // Mono
    FPatchDescription.VoiceCount := aValue;
  end;

  MessSetPatchDescription( FPatchDescription);
end;

procedure TG2USBPatch.SetVoiceMode(aValue: Byte);
var
  FPatchDescription: TPatchDescription;
begin
  if aValue > 2 then
    Exit;

  FPatchDescription := PatchDescription;
  if FPatchDescription.VoiceCount > 1 then
    FPatchDescription.MonoPoly := 0 // Poly
  else begin
    if FPatchDescription.MonoPoly <> 1 then
      FPatchDescription.MonoPoly := 1 // Mono
    else
      if FPatchDescription.MonoPoly <> 2 then
        FPatchDescription.MonoPoly := 2 // Legato
  end;

  MessSetPatchDescription( FPatchDescription);
end;

function TG2USBPatch.MessSetPatchDescription(FPatchDescription: TPatchDescription): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateSetPatchDescriptionMessage(FPatchDescription);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetPatchNotes(aLines: TStrings): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateSetPatchNotesMessage(aLines);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessAddModule(aLocation: TLocationType; aModuleTypeID,
  aCol, aRow: Byte): Boolean;
var
  MemStream: TG2SendMessage;
  aModuleIndex: Byte;
begin
  inherited MessAddModule(aLocation, aModuleTypeID, aCol, aRow);
  // Get a new module index
  aModuleIndex := GetMaxModuleIndex(aLocation) + 1;
  // Send over usb
  MemStream := CreateAddNewModuleMessage(aLocation, aModuleIndex, aModuleTypeID, aCol, ARow);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessCopyModules(aSrcePatch: TG2FilePatchPart; aFromLocation, aToLocation: TLocationType): Boolean;
var
  MemStream : TG2SendMessage;
begin
  inherited MessCopyModules(aSrcePatch, aFromLocation, aToLocation);

  MemStream := CreateCopyModulesMessage(aSrcePatch, aFromLocation, aToLocation, True);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessCopyParameters(aSrcePatch: TG2FilePatchPart;
  aToLocation: TLocationType; aFromVariation, aToVariation: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateCopyParametersMessage(aSrcePatch, aToLocation, aFromVariation, aToVariation);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessModuleAssignGlobalKnobs(aModule: TG2FileModule; aPageIndex: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateModuleAssignGlobalKnobs(aModule, aPageIndex);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessModuleAssignKnobs(aModule: TG2FileModule; aPageIndex: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateModuleAssignKnobs(aModule, aPageIndex);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessMoveModules(aLocation : TLocationType): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateMoveModulesMessage(aLocation);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteModule(aLocation: TLocationType; aModuleIndex: Byte): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateDeleteModuleMessage(aLocation, aModuleIndex);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessDeleteModules(aLocation : TLocationType): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateDeleteModulesMessage(aLocation);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessSetModuleColor(aLocation: TLocationType; aModuleIndex, aColor: Byte): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateSetModuleColorMessage(aLocation, aModuleIndex, aColor);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessAddConnection(aLocation: TLocationType; aFromConnector, aToConnector: TG2FileConnector): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateAddConnectionMessage(aFromConnector, aToConnector);
  if assigned(MemStream) then
    Result := SendCmdMessage(MemStream)
  else
    Result := False;
end;

function TG2USBPatch.MessDeleteConnection(aLocation: TLocationType; aCable: TG2FileCable): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateDeleteConnectionMessage(aCable);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessAssignKnob(aLocation: TLocationType; aModule, aParam, aKnob: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateAssignKnobMessage(aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessDeassignKnob(aKnob: Integer): Boolean;
var
  MemStream: TG2SendMessage;
  Knob: TKnob;
begin
  Knob := GetKnob(aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');
  MemStream := CreateDeassignKnobMessage(aKnob);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessAssignMidiCC(aLocation: TLocationType; aModule, aParam, aMidiCC: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateAssignMidiCCMessage(aLocation, aModule, aParam, aMidiCC);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessDeassignMidiCC(aMidiCC: Integer): Boolean;
var
  MemStream: TG2SendMessage;
  Controller: TController;
begin
  Controller := GetMidiCC(aMidiCC);
  if not assigned(Controller) then
    raise Exception.Create('Controller ' + IntToHex(aMidiCC,2) + ' not found.');
  MemStream := CreateDeassignMidiCCMessage(aMidiCC);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessAssignGlobalKnob(aLocation: TLocationType; aModule, aParam, aKnob: Integer): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateAssignGlobalKnobMessage(aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessDeassignGlobalKnob(aKnob: Integer): Boolean;
var
  MemStream: TG2SendMessage;
  Knob: TGlobalKnob;
  Perf: TG2USBPerformance;
begin
  Perf := GetPerformance;
  if not assigned(Perf) then
    raise Exception.Create('Performance not assigned to patch.');
  Knob := Perf.GetGlobalKnob(aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');
  MemStream := CreateDeassignGlobalKnobMessage(aKnob);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessSetModuleParamLabels(aLocation: TLocationType; aModuleIndex, aParamIndex, aLabelIndex: Byte; aName: string): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateSetModuleParamLabelsMessage(aLocation, aModuleIndex, aParamIndex, aLabelIndex, aName);
  Result := SendCmdMessage(MemStream);
end;

function TG2USBPatch.MessSetModuleLabel(aLocation: TLocationType; aModuleIndex: Byte; aName: string): Boolean;
var
  MemStream: TG2SendMessage;
begin
  MemStream := CreateSetModuleLabelMessage(aLocation, aModuleIndex, aName);
  Result := SendCmdMessage(MemStream);
end;

end.
