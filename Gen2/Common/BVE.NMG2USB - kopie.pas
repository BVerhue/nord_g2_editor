unit BVE.NMG2USB;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

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
//  The libusb-win32 usb driver is needed:
//
//  Download libusb-win32 snapshot from
//    http://sourceforge.net/projects/libusb-win32/files/
//
//  Make a system restore point
//  Install as filter driver on the clavia usb driver
//  Do NOT install as device driver (because it then permanently replaces the clavia driver!)
//
//  This unit contains the USB interface and the client/server functionality
//
//  The usb interface is based on the libusb-win32 (for windows) and the
//  libusb1.0 library for unix
//
//  The client/server is based on the Indy10 library
//
//  Parts ported from https://github.com/msg/g2ools
//
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
{$IF Defined(FPC)}
  LCLIntf,
  LCLType,
{$ELSE}
  {$IF Defined(MSWINDOWS)}
    WinApi.Windows, WinApi.Messages,
  {$ENDIF}
  System.SyncObjs, System.Classes, System.SysUtils, System.Generics.Collections,
{$ENDIF}
{$IF Defined(MSWINDOWS)}
  LibUSBWinDyn,
{$ENDIF}
{$IF Defined(MACOS)}
  libusb_dyn,
{$ENDIF}
{$IF Defined(Android)}
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.USB,
{$ENDIF}
  IdCustomTCPServer, idTCPConnection, IdYarn, IdThread, idSync, IdTCPServer,
  idTCPClient, IdContext,
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2Mess;

const
{$IFNDEF UNIX}
  LIBUSB_ERROR_TIMEOUT = -116;
  LIBUSB_ERROR_OTHER = -99;
{$ENDIF}
  TIME_OUT             = 0;     // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2

  DEFAULT_PORT         = 2501;  // The default tcpip port for the communication
  MAX_READ_BUFFER      = 65536; // tcpip buffer size, long enough to hold the longest message from the G2

type
  TCommand = (
    cmdConnect,
    cmdDisconnect,
    cmdGetPerformance,
    cmdSendPerformance,
    cmdGetGlobalKnobs,
    cmdSendGlobalKnobs,
    cmdGetVariationSettings,
    cmdSendVariationSettings,
    cmdSendMessage);

  // Client identification
  TClient = record
    ID         : integer;     // Unique ID
    ClientType : TClientType; // Editor or VST
  end;

  // The protocol for communication between server and client
  TProtocol = record
    Command     : TCommand;
    Sender      : TClient;
    MessageType : TMessageDataType;
    DataSize    : Integer;
  end;

const
  szProtocol = SizeOf(TProtocol);

type
  TUSBErrorEvent = procedure(Sender: TObject; ErrNo : integer; ErrText, LastCmd : string) of object;
  TUSBActiveChangeEvent = procedure(Sender: TObject; Active : boolean) of object;
  TAfterG2InitEvent = procedure(Sender: TObject) of Object;
  TInitStepEvent = procedure(Sender: TObject) of Object;
  TOnAddClient = procedure(Sender: TObject; ClientIndex : integer) of Object;
  TOnDeleteClient = procedure(Sender: TObject; ClientIndex : integer) of Object;
  TBeforeSendMessage = procedure(Sender: TObject; SenderID: integer; SendMessage : TG2SendMessage) of Object;
  TReceiveResponseMessage = procedure(Sender: TObject; ResponseMessage : TMemoryStream) of Object;

  TClientSendMessage = class;

  // Custom client context for the server
  TClientContext = class(TIdServerContext)
  private
    // Critical section to ensure a single access on the connection at a time
    FClientContextCriticalSection: TCriticalSection;
    FClient: TClient;
    FInitialized : boolean;
    FLastWrite : integer;
  public
{$IF Defined(VER230)}
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList = nil); override;
{$ELSE}
{$IF Defined(VER240)}
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList = nil); override;
{$ELSE}
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList = nil); override;
{$ENDIF}
{$ENDIF}
    destructor Destroy; override;
  public
    procedure Lock;
    procedure Unlock;
    procedure ServerSendClientSendMessage( ClientSendMessage : TClientSendMessage);
    procedure ServerSendClientResponseMessage( MemStream : TMemoryStream);
  public
    property Client: TClient read FClient write FClient;
    property Initialized : boolean read FInitialized write FInitialized;
    property LastWrite : integer read FLastWrite write FLastWrite;
  end;

  TG2USB = class;
  TG2USBPerformance = class;
  TG2USBSlot = class;
  TG2USBPatch = class;

  // Listening thread for a client
  TIdClientReadThread = class(TIdThread)
  protected
    [Weak] FG2: TG2USB;
    LMessage : TMemoryStream;
    LProtocol: TProtocol;
    procedure Run; override;
  public
    constructor Create(AG2: TG2USB); reintroduce;
    destructor Destroy; override;
    procedure DoClientProcessServerSendMessage;
    procedure DoClientProcessServerResponseMessage;
  End;

  // Thread for receiving USB messages from the G2
  TReceiveMessageThread = class(TThread)
  private
    [Weak] FG2USB : TG2USB;
    // Next is all for logging
    FLogMessage  : string;
    FBuffer      : pointer;
    FMaxSize     : integer;
  protected
    procedure   Execute; override;
  public
    constructor Create( CreateSuspended: Boolean; aG2 : TG2USB);
    procedure   ProcessMessage;
    procedure   WriteLog;
    procedure   DumpMessage;
  end;

  // Thread for sending USB messages to the G2
  TSendMessageThread = class( TThread)
  private
    [Weak] FG2USB : TG2USB;
    // Next is all for logging
    FLogMessage  : string;
    FBuffer      : pointer;
    FMaxSize     : integer;
  protected
    procedure   Execute; override;
  public
    constructor Create( CreateSuspended: Boolean; aG2 : TG2USB);
    procedure   SendUSBMessage;
    procedure   WriteLog;
    procedure   DumpMessage;
  end;

  // Notification from the server listening thread to the main thread
  TG2ProcessClientSendMessage = class( TIdNotify)
  private
    [Weak] FG2 : TG2USB;
    [Weak] FClientContext : TClientContext;

    FBuffer : TMemoryStream;
    FClient : TClient;
  protected
    procedure DoNotify; override;
    destructor Destroy; override;
  end;

  // Record def. for table sending responseless messages
  TParamUpdRec = record
    [Weak] ClientContext : TClientContext;

    SubCmd    : byte;
    Location  : byte;
    Module    : byte;
    Param     : byte;
    Morph     : byte;
    Value     : byte;
    Negative  : byte;
    Variation : byte;
    Changed   : boolean;
  end;

  // Message object for the send queue
  TClientSendMessage = class
    [Weak] FClientContext  : TClientContext; // nil : server, else a client
    [Weak] FMessage        : TG2SendMessage;

    FMessageSender  : TClient;
  end;

  TG2USB = class( TG2Mess)
    private
{$IF Defined(MSWINDOWS)}
      bus    : pusb_bus;
      g2dev  : pusb_device;
      g2udev : pusb_dev_handle;
      g2conf : usb_config_descriptor;
      g2intf : usb_interface;
      g2eps  : PArray_usb_endpoint_descriptor;
      g2iin  : Byte;
      g2bin  : Byte;
      g2bout : Byte;
{$ENDIF}
{$IF Defined(MACOS)}
      g2dev  : Plibusb_device;
      g2udev : Plibusb_device_handle; // a device handle
      g2iin  : Byte;
      g2bin  : Byte;
      g2bout : Byte;
{$ENDIF}
{$IF Defined(Android)}
      [Weak] FUsbManager : JUSBManager;
      FUsbDevice : JUSBDevice;
      FUsbInterface : JUSBInterface;
      FUsbEPII, FUsbEPBI, FUsbEPBO : JUSBEndPoint;
      FUsbDeviceConnection : JUSBDeviceConnection;
      FIBuffer,
      FBBuffer : TJavaArray<Byte>;
{$ENDIF}
      g_bin_buf : TG2ResponseMessage;
      g_bout_buf : TG2SendMessage;

      // Threadsafe queue for sending USB messages to the G2
      FSendMessageQueue    : TThreadList;
      FSendMessageCount    : integer;

      FInitStep : integer;
      FInitialized : boolean;
      FProcessLedData : boolean;

      FOnUSBError           : TUSBErrorEvent;
      FOnUSBActiveChange    : TUSBActiveChangeEvent;
      FOnNextInitStep       : TInitStepEvent;
      FOnAfterG2Init        : TAfterG2InitEvent;
      FOnAfterSlotInit      : TNotifyEvent;
      FOnAfterPerfInit      : TNotifyEvent;
      FOnAddClient          : TOnAddClient;
      FOnDeleteClient       : TOnDeleteClient;
      FOnBeforeSendMessage  : TBeforeSendMessage;
      FOnReceiveResponseMessage : TReceiveResponseMessage;

      // Vars for communication between server and G2
      FWaitforCmd           : byte;
      FMessageSendStart     : integer;
      FReceiveMessageThread : TReceiveMessageThread;
      FSendMessageThread    : TSendMessageThread;
      FReceiveMessageThreadHandle : THandle;
      FSendMessageThreadHandle : THandle;

      // Vars for communication between server and client
      FTCPIPEnabled         : boolean;
      FHost                 : String;
      FPort                 : integer;
      FIsServer             : boolean;
      FClientID             : TClient;       // user name & ID

      // Indy server object
      FIdTCPServer          : TIdTCPServer;

      // Indy client object
      FIdTCPClient          : TIdTCPClient;

      // Listening thread for a client
      FidClientReadThread    : TIdClientReadThread;
      FClientCriticalSection : TCriticalSection;

      FTimerBroadcastLedMessages : integer;

      function    GetUSBActive : boolean;
      procedure   SetUSBActive(const value : boolean);
      //procedure   SetUSBActive2(const value : boolean);
{$IFDEF UNIX}
      function    iread(var buffer; size, timeout: longword): integer;
      function    bread(var buffer; size, timeout: longword): integer;
      function    bwritevar buffer; size, timeout: longword): integer;
{$ENDIF}
{$IFDEF MACOS}
      function    iread(var buffer; size, timeout: longword): integer;
      function    bread(var buffer; size, timeout: longword): integer;
      function    bwrite(var buffer; size, timeout: longword): integer;
{$ENDIF}
{$IFDEF ANDROID}
      function    iread(var buffer; size, timeout: longword): integer;
      function    bread(var buffer; size, timeout: longword): integer;
      function    bwrite(var buffer; size, timeout: longword): integer;
{$ENDIF}
{$IFDEF MSWINDOWS}
      function    bwrite(var buffer; size, timeout : longword): integer;
      function    bread(var buffer; size, timeout : longword): integer;
      function    iread(var buffer; size, timeout : longword): integer;
{$ENDIF}
      function    extended_message(var iin_buf : TByteBuffer): integer;
      function    embedded_message(var iin_buf : TByteBuffer): integer;
      function    isextended(buf_in : TByteBuffer): boolean;
      function    isembedded(buf_in : TByteBuffer): boolean;
    protected
      procedure   SetPerfMode( aValue : TBits1); override;
      function    GetID : integer; override;
      property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
    public
      FLogLedDataMessages  : boolean;
      //FErrorMessage        : boolean;
      //FErrorMessageNo      : integer;
      FTCPErrorMessage     : string;

      constructor Create( AOwner: TComponent); override;
      destructor  Destroy; override;

      function    CreatePerformance: TG2FilePerformance; override;

      function    GetPerformance : TG2USBPerformance;
      function    GetSlot( aSlot : byte) : TG2USBSlot;

      procedure   G2ProcessWindowsMessages; virtual; // Override on windows platform

      // Initialization USB interface
      procedure   LibUSBInit;
      procedure   LibUSBDone;

      function    get_error: string;

      procedure   SendOnClientMessage( SendMessage : TMemoryStream);
      function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

      // Client/server communication

      procedure   IdTCPServerConnect( AContext: TIdContext);
      procedure   IdTCPServerDisconnect( AContext: TIdContext);
      procedure   IdTCPServerExecute( AContext: TIdContext);

      procedure   ServerProcessClientMessage( ClientMessage : TClientSendMessage);
      procedure   ServerBroadcastResponseMessage( ResponseMessage : TG2ResponseMessage);
      procedure   ServerBroadcastSendMessage( ClientSendMessage : TClientSendMessage);
      function    ServerProcessClientResponselessMessage( ClientMessage : TClientSendMessage): boolean;

      procedure   ClientProcessServerSendMessage( MemStream : TMemoryStream; SenderID : integer);
      procedure   ClientProcessServerResponseMessage( MemStream : TMemoryStream);
      procedure   ClientSendMessageToServer( MemStream : TMemoryStream);
      procedure   ClientSendConnectedToServer;

      function    GetClientCount: integer;

      procedure   Lock;
      procedure   Unlock;

      // USB communication
      procedure   USBSendMessage;
      procedure   USBSendParamUpd;
      procedure   USBProcessMessage( ResponseMessage : TG2ResponseMessage);
      procedure   USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage);
      procedure   USBProcessSendMessage( ClientSendMessage : TClientSendMessage);

      procedure   USBStartInit;
      procedure   USBInitSeq(Sender: TObject);
      procedure   USBSentStartComm(Sender: TObject);

      procedure   NextListMessage(Sender: TObject);
      procedure   RefreshBanks;

      procedure   SendInitMessage;
      procedure   SendStartStopCommunicationMessage( Stop : byte);
      procedure   SendGetPatchVersionMessage(var patch_version: byte);
      procedure   SendGetSynthSettingsMessage;
      procedure   SendSetSynthSettingsMessage;
      procedure   SendUnknown1Message;
      procedure   SendDumpMidiMessage;
      procedure   SendListMessage( aPatchFileType : TPatchFileType; aBank, aPatch : byte; names : TStrings);
      procedure   SendSetModeMessage( aMode : byte);
      procedure   SendNoteMessage( aNote : byte; aOnoff : byte);
      procedure   SendGetMasterClockMessage;
      procedure   SendGetAssignedVoicesMessage;
      procedure   SendRetrieveMessage( aSlot, aBank, aPatch : byte);
      procedure   SendStoreMessage( aSlot, aBank, aPatch : byte);
      procedure   SendClearMessage( aPatchFileType : TPatchFileType; aBank, aPatch : byte);
      procedure   SendClearBankMessage( aPatchFileType : TPatchFileType; aBank, aFromLocation, aToLocation : byte);
      procedure   SendUploadBankMessage( aPatchFileType: TPatchFileType; aBank, aLocation: byte);
      procedure   NextBankUploadMessage( Sender: TObject);
      procedure   SendDownloadPatchBankMessage( aBank, aLocation: byte; aFileName : string);
      procedure   NextPatchBankDownloadMessage( Sender: TObject);
      procedure   SendDownloadPerfBankMessage( aBank, aLocation: byte; aFileName : string);
      procedure   NextPerfBankDownloadMessage( Sender: TObject);

      property    TCPIPEnabled : boolean read FTCPIPEnabled write FTCPIPEnabled;
      property    IdTCPClient : TIdTCPClient read FIdTCPClient;
      property    Initialized : boolean read FInitialized;
{$IF Defined(MSWINDOWS)}
      property    G2USBDevice : pusb_device read g2dev write g2dev;
{$ENDIF}
{$IF Defined(MACOS)}
      property    G2USBDevice : Plibusb_device read g2dev write g2dev;
{$ENDIF}
{$IF Defined(Android)}
      property    G2USBDevice : JUsbDevice read FUsbDevice write FUsbDevice;
      property    G2USBManager : JUsbManager read FUsbManager write FUsbManager;
{$ENDIF}
    published
      property    IsServer : boolean read FIsServer write FIsServer;
      property    Port : integer read FPort write FPort;
      property    Host : string read FHost write FHost;
      property    USBActive : boolean read GetUSBActive write SetUSBActive;
      property    ProcessLedData : boolean read FProcessLedData write FProcessLedData;
      property    TimerBroadcastLedMessages : integer read FTimerBroadcastLedMessages write FTimerBroadcastLedMessages default 500;
      property    OnUSBError : TUSBErrorEvent read FOnUSBError write FOnUSBError;
      property    OnUSBActiveChange : TUSBActiveChangeEvent read FOnUSBActiveChange write FOnUSBActiveChange;
      property    OnAfterG2Init : TAfterG2InitEvent read FOnAfterG2Init write FOnAfterG2Init;
      property    OnAfterSlotInit : TNotifyEvent read FOnAfterSlotInit write FOnAfterSlotInit;
      property    OnAfterPerfInit : TNotifyEvent read FOnAfterPerfInit write FOnAfterPerfInit;
      property    OnAddClient : TOnAddClient read FOnAddClient write FOnAddClient;
      property    OnDeleteClient : TOnDeleteClient read FOnDeleteClient write FOnDeleteClient;
      property    OnBeforeSendMessage : TBeforeSendMessage read FOnBeforeSendMessage write FOnBeforeSendMessage;
      property    OnReceiveResponseMessage : TReceiveResponseMessage read FOnReceiveResponseMessage write FOnReceiveResponseMessage;
  end;

  TG2USBPerformance = class( TG2MessPerformance)
  private
    FOnNextInitStep     : TInitStepEvent;
    FInitStep           : integer;
    FStartCommAfterInit : boolean;
    function GetInitializing: boolean;
  protected
    procedure   SetMasterClock( aValue : TBits8); override;
    procedure   SetMasterClockRun( aValue : TBits8); override;
    procedure   SetSelectedSlotIndex( aValue : TBits2); override;

    property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreateSlot : TG2FileSlot; override;

    function    GetSlot( aSlot : byte) : TG2USBSlot;

    function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

    procedure   USBStartInit( aStartCommAfterInit : boolean);
    procedure   USBInitSeq(Sender: TObject);

    procedure   USBStartInitPerf;
    procedure   USBInitPerfSeq(Sender: TObject);

    procedure   SendGetPerfSettingsMessage;
    procedure   SendUnknown2Message;
    procedure   SendSelectSlotMessage( aSlot: byte);
    procedure   SendSetPerformanceMessage( aPerfName : string; aPerf : TG2FilePerformance);
    procedure   SendSetPerfSettingsMessage;
    procedure   SendSetPerfNameMessage( aPerfName : string);
    procedure   SendGetGlobalKnobsMessage;
    procedure   SendSetMasterClockBPMMessage( BPM : byte);
    procedure   SendSetMasterClockRunMessage( Start : boolean);

    property    Initializing : boolean read GetInitializing;
  end;

  TG2USBSlot = class( TG2MessSlot)
  private
    FParamUpdBuf        : array of TParamUpdRec;
    FParamUpdBufCount   : integer;
    FInitStep           : integer;
    FOnNextInitStep     : TInitStepEvent;
    FStartCommAfterInit : boolean;
    function GetInitializing: boolean;
  protected
    property    OnNextInitStep : TInitStepEvent read FOnNextInitStep write FOnNextInitStep;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    CreatePatch : TG2FilePatch; override;

    function    GetPatch : TG2USBPatch;
    function    GetPerformance : TG2USBPerformance;

    function    SendCmdMessage(  SendMessage : TG2SendMessage): boolean;

    procedure   USBStartInit( aStartCommAfterInit : boolean);
    procedure   USBInitSeq(Sender: TObject);

    procedure   SendGetPatchVersionMessage;
    procedure   SendPatchNotesMessage;
    procedure   SendControllerSnapshotMessage;
    procedure   SendResourceTableMessage( aLocation : Byte);
    procedure   SendGetPatchNameMessage;
    procedure   SendCurrentNoteMessage;
    procedure   SendUnknown6Message;
    procedure   SendGetSelectedParameterMessage;
    procedure   SendSetPatchName( aPatchName : string);
    procedure   SendSetPatchMessage( aPatchName : string; aPatch : TG2FilePatch);
    procedure   SendGetPatchMessage;
    procedure   SendSelectVariationMessage( aVariationIndex: byte);
    procedure   SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte);
    procedure   SendSelParamMessage( aLocation, aModule, aParam: integer);
    procedure   SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
    procedure   SendSetModeMessage( aLocation, aModule, aParam, aValue: integer);
    procedure   SendCopyVariationMessage( aFromVariation, aToVariation : byte);
    procedure   SendGetParamNamesMessage( aLocation : byte);
    procedure   SendGetParamsMessage( aLocation : byte);
    // All responseless messages must be send through the ParamUpdBuf
    procedure   AddParamUpdRec( aSubCmd, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation : byte; aClientContext : TClientContext);

    property    ParamUpdBufCount : integer read FParamUpdBufCount;
    property    Initializing : boolean read GetInitializing;
  end;

  TG2USBPatch = class( TG2MessPatch)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    GetPerformance : TG2USBPerformance;

    procedure   SetVoiceCount( aValue : byte); override;
    procedure   SetVoiceMode( aValue : byte); override;

    function    SendCmdMessage( SendMessage : TG2SendMessage): boolean;

    procedure   SendUndoMessage;

    procedure   SetParamValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aVariation : byte; aValue: byte); override;
    procedure   SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aValue: byte); override;
    procedure   SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte); override;

    function    MessSetPatchDescription( FPatchDescription : TPatchDescription): boolean;
    function    MessSetPatchNotes( aLines : TStrings): boolean;
    function    MessAddModule( aLocation : TLocationType; aModuleTypeID, aCol, aRow: byte): boolean; override;
    function    MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean; override;
    function    MessCopyParameters( aSrcePatch : TG2FilePatchPart; aToLocation : TLocationType; aFromVariation, aToVariation : integer): boolean; override;
    function    MessAddConnection( aLocation : TLocationType; aFromConnector, aToConnector : TG2FileConnector): boolean; override;
    function    MessDeleteConnection( aLocation : TLocationType; aCable : TG2FileCable): boolean; override;
    function    MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean; override;
    function    MessDeleteModules( aLocation : TLocationType): boolean; override;
    function    MessMoveModules( aLocation : TLocationType): boolean; virtual;
    function    MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean; override;
    function    MessAssignKnob( aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
    function    MessDeAssignKnob( aKnob: integer): boolean;
    function    MessModuleAssignKnobs( aModule : TG2FileModule; aPageIndex : integer): boolean;
    function    MessModuleAssignGlobalKnobs( aModule : TG2FileModule; aPageIndex : integer): boolean;
    function    MessAssignMidiCC( aLocation: TLocationType; aModule, aParam, aMidiCC: integer): boolean;
    function    MessDeassignMidiCC( aMidiCC: integer): boolean;
    function    MessAssignGlobalKnob( aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
    function    MessDeassignGlobalKnob( aKnob: integer): boolean;
    function    MessSetModuleParamLabels( aLocation : TLocationType; aModuleIndex, aParamIndex, aLabelIndex : byte; aName : string): boolean; override;
    function    MessSetModuleLabel( aLocation : TLocationType; aModuleIndex : byte; aName : string): boolean; override;
  end;

{$IF Defined(MSWindows) or Defined(MACOS)}
  procedure GetUSBDeviceList( aList : TList);
{$ENDIF}

implementation

function ProtocolToStream(const AProtocol: TProtocol): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  // set the length of result to the length of the protocol
  Result.Size := szProtocol;
  // move a block of memory from AProtocol to Result
  Move(AProtocol, Result.Memory^, szProtocol);
end;

function StreamToProtocol( MemStream : TMemoryStream): TProtocol;
begin
  if MemStream.Size < szProtocol then
    MemStream.Size := szProtocol;
  Move( MemStream.Memory^, Result, szProtocol);
end;

procedure InitProtocol(var AProtocol: TProtocol);
begin
  FillChar(AProtocol, szProtocol, 0);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USB
////////////////////////////////////////////////////////////////////////////////

constructor TG2USB.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);

  FClientCriticalSection := TCriticalSection.Create;

  FInitialized := False; // G2 Initialization succesfull
  FProcessLedData := False;

  FSendMessageQueue := TThreadList.Create;
  FSendMessageQueue.Duplicates := dupAccept;
  FSendMessageCount := 0;

{$IF Defined(MSWINDOWS)}
  g2dev := nil;
  g2udev := nil;
{$ENDIF}
{$IF Defined(MACOS)}
  g2dev := nil;
  g2udev := nil;
{$ENDIF}
{$IF Defined(Android)}
  FUsbManager := nil;
  FUsbDevice := nil;
  FIBuffer := TJavaArray<Byte>.Create(8192);
  FBBuffer := TJavaArray<Byte>.Create(8192);
{$ENDIF}
  g_bin_buf := TG2ResponseMessage.Create;
  g_bout_buf := TG2SendMessage.Create;

  FTCPIPEnabled := True;

  FHost := '127.0.0.1';
  FPort := DEFAULT_PORT;

  FLogLedDataMessages := False;
end;

destructor TG2USB.Destroy;
var i : integer;
    MessageList : TList;
begin
  // Free the send message queue
  MessageList := FSendMessageQueue.LockList;
  try
    for i := 0 to MessageList.Count - 1 do begin
      TClientSendMessage(MessageList[i]).FMessage.Free;
      TClientSendMessage(MessageList[i]).Free;
    end;
    FSendMessageCount := MessageList.Count;
  finally
    FSendMessageQueue.UnlockList;
  end;
  FSendMessageQueue.Free;

  // Free the usb message buffers
  g_bout_buf.Free;
  g_bin_buf.Free;
{$IF Defined(Android)}
  FBBuffer.Free;
  FIBuffer.Free;
{$ENDIF}

  FClientCriticalSection.Free;

  inherited;
end;

function TG2USB.get_error: string;
var i : integer;
    err : PByteArray;
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

procedure TG2USB.G2ProcessWindowsMessages; // Override on windows platform
begin
  // For windows : Application.ProcessMessages;
end;

function TG2USB.GetPerformance: TG2USBPerformance;
begin
  Result := Performance as TG2USBPErformance;
end;

function TG2USB.GetSlot( aSlot: byte): TG2USBSlot;
begin
  Result := Performance.Slot[ aSlot] as TG2USBSlot;
end;

{$IF Defined(MSWINDOWS)}
procedure GetUSBDeviceList( aList : TList);
var bus : pusb_bus;
    dev : pusb_device;
begin
  aList.Clear;

  if not assigned(usb_init) then
    exit; // libusb dll not loaded

  // LibUSB-Win32 Initialization
  usb_init;              // Initialize libusb
  usb_find_busses;       // Finds all USB busses on system
  usb_find_devices;      // Find all devices on all USB devices
  bus := usb_get_busses; // Return the list of USB busses found

  while assigned(bus) do begin
    dev := bus^.devices;
    while assigned(dev) do begin
      if (dev^.descriptor.idVendor = VENDOR_ID) and (dev^.descriptor.idProduct = PRODUCT_ID) then
        aList.Add(dev);
      dev := dev^.next;
    end;
    bus := bus^.next;
  end;
  usb_set_debug(255);
end;
{$ENDIF}
{$IF Defined(MACOS)}
procedure GetUSBDeviceList( aList : TList);
type DevsArray = array[0..255] of pointer;
     PDevsArray = ^DevsArray;
var err : integer;
    devs : PPlibusb_device;
    pdev : Plibusb_device;
    descr : libusb_device_descriptor;
    i, cnt : integer;
begin
  aList.Clear;

  if not assigned(ctx) then
    exit; // libusb dll not loaded

  cnt := libusb_get_device_list(ctx, @devs);
  try
    for i := 0 to cnt - 1 do begin
      pdev := PLibusb_device(PDevsArray(devs)^[i]);
      err := libusb_get_device_descriptor( pdev, @descr );
      if err = 0 then begin
        if (descr.idVendor = VENDOR_ID) and (descr.idProduct = PRODUCT_ID) then begin
          aList.Add(pdev);
        end else
          libusb_unref_device(pdev);
      end;
    end;
  finally
    //free the list, don't unref the devices in it
    libusb_free_device_list(devs, 0);
  end;
end;
{$ENDIF}

{$IF Defined(MSWINDOWS)}
procedure TG2USB.LibUSBInit;
var dev: pusb_device;
    version : pusb_version;
begin
  // Initialization of the USB interface windows
  try
    g2udev := nil;

    if not assigned(g2dev) then
      exit;

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

    except on E:Exception do begin
      add_log_line( E.Message, LOGCMD_ERR);
      G2MessageDlg( E.Message, 1);
    end;
  end;
end;
{$ENDIF}
{$IF Defined(MACOS)}
procedure TG2USB.LibUSBInit;
var err : integer;
    dev : libusb_device;
    devs : PPlibusb_device;
    cnt : integer;
begin
  // Initialization of the USB interface unix

  if not assigned(g2dev) then
    exit;

  err := libusb_open( g2dev, @g2udev);

  if err = 0 then begin
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
{$IF Defined(Android)}
procedure TG2USB.LibUSBInit;
begin
  add_log_line('# Interfaces ' + IntToStr(FUSBDevice.getInterfaceCount), LOGCMD_HDR);

  FUsbInterface := FUSBDevice.getInterface(0);
  add_log_line('# Endpoints ' + IntToStr(FUsbInterface.getEndpointCount), LOGCMD_HDR);

  FUsbEPII := FUsbInterface.getEndpoint(0);
  add_log_line('Endpoint ' + IntToStr(FUsbEPII.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBI := FUsbInterface.getEndpoint(1);
  add_log_line('Endpoint ' + IntToStr(FUsbEPBI.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBO := FUsbInterface.getEndpoint(2);
  add_log_line('Endpoint ' + IntToStr(FUsbEPBO.getEndpointNumber), LOGCMD_HDR);

   if FUsbManager.hasPermission(FUSBDevice) then begin
     add_log_line('Permissions o.k.', LOGCMD_HDR);
   end else begin
     FUSBDevice := nil;
     raise Exception.Create('No permission...');
   end;

  FUsbDeviceConnection := FUsbManager.openDevice(FUSBDevice);
  if not assigned( FUsbDeviceConnection) then
    raise Exception.Create('Failed to open device.');

  if not FUsbDeviceConnection.claimInterface(FUsbInterface, True) then
    raise Exception.Create('Failed to claim interface.');
end;
{$ENDIF}

{$IF Defined(MSWINDOWS)}
procedure TG2USB.LibUSBDone;
begin
  // Disconnect from the USB interface windows

  if Assigned(g2udev) then begin
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
{$IF Defined(MACOS)}
procedure TG2USB.LIBUSBDone;
var err : integer;
begin
  // Disconnect from the USB interface unix

  // Needed to break out the infinite interupt wait
  libusb_reset_device( g2udev);

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

  libusb_exit( ctx);
end;
{$ENDIF}
{$IF Defined(Android)}
procedure TG2USB.LibUSBDone;
begin
  if assigned(FUsbDeviceConnection) then begin

    if not FUsbDeviceConnection.releaseInterface(FUsbInterface) then
      add_log_line('Failed to release the interface', LOGCMD_HDR);
    FUsbDeviceConnection.close;

    add_log_line('Device is closed', LOGCMD_HDR);
  end;
end;
{$ENDIF}

{$IF Defined(UNIX)}
function TG2USB.iread(var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(g2udev, g2iin, PChar(@buffer), size, @bytes_read, timeout);
  if Result >= 0 then
    Result := bytes_read;
end;

function TG2USB.bread(var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(g2udev, g2bin, PChar(@buffer), size, @bytes_read, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): integer;
var bytes_written : longint;
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
{$IF Defined(MACOS)}
function TG2USB.iread(var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(g2udev, g2iin, PChar(@buffer), size, @bytes_read, timeout);
  if Result >= 0 then
    Result := bytes_read;
end;

function TG2USB.bread(var buffer; size, timeout: longword): integer;
var requested : integer;
    bytes_read : longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(g2udev, g2bin, PChar(@buffer), size, @bytes_read, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): integer;
var bytes_written : longint;
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
{$IF Defined(ANDROID)}
function TG2USB.iread(var buffer; size, timeout: longword): integer;
var requested : integer;
begin
  // Read an interrupt message from USB unix
  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPII, FIBuffer, size, timeout);
  if Result > 0 then
     Move(FIBuffer.Data^, buffer, Result);
end;

function TG2USB.bread(var buffer; size, timeout: longword): integer;
var requested : integer;
begin
  // Read a bulk message from USB unix

  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBI, FBBuffer, size, timeout);
  if Result > 0 then
    Move(FBBuffer.Data^, buffer, Result);

  //if Result < 0 then
  //  add_log_line(get_error, LOGCMD_ERR);
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): integer;
begin
  // Write a bulk message over USB unix
  try
    Move(buffer, FBBuffer.Data^, size);

    Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBO, FBBuffer, Size, timeout);
  finally
    if Result < 0 then
      add_log_line(get_error, LOGCMD_ERR)
    else
      Result := size;
  end;
end;
{$ENDIF}
{$IF Defined(MSWINDOWS)}
function TG2USB.iread(var buffer; size, timeout: longword): integer;
begin
  // Read an interrupt message from USB windows
  Result := usb_interrupt_read(g2udev, g2iin, buffer, size, timeout);
end;

function TG2USB.bread(var buffer; size, timeout: longword): integer;
begin
  Result := 0;
  // Read a bulk message from USB windows
  if assigned(g2udev) then
    Result := usb_bulk_read(g2udev, g2bin, buffer, size, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;

function TG2USB.bwrite(var buffer; size, timeout: longword): integer;
begin
  // Write a bulk message over USB windows
  Result := usb_bulk_write(g2udev, g2bout, buffer, size, timeout);

  if Result < 0 then
    add_log_line(get_error, LOGCMD_ERR);
end;
{$ENDIF}
function TG2USB.extended_message(var iin_buf : TByteBuffer): integer;
var i, retries, bytes_read, buf_position : integer;
    ecrc, acrc : word;
    size : word;
begin
  // Read an G2 extended message from USB, interrupt messages contains length,
  // bulk message contains the data

  Result := -1;

  size := (iin_buf[1] shl 8) or iin_buf[2];

  g_bin_buf.Position := 0;
  g_bin_buf.Size := Size;

  retries := 5; // the message has to return within 5 tries
  bytes_read := 0;
  buf_position := 0;
  while (retries > 0) and (size <> bytes_read) and (bytes_read >= 0) do begin
    bytes_read := bread(PStaticByteBuffer(g_bin_buf.Memory)^[buf_position], size, TIME_OUT);

    if bytes_read = LIBUSB_ERROR_TIMEOUT then
      bytes_read := 0 //time out
    else
      buf_position := buf_position + bytes_read;
    dec(retries);
  end;

  if retries = 0 then begin
    add_log_line('Timeout reading extended message.', LOGCMD_ERR);
    exit;
  end;

  if buf_position > 0 then begin

    ecrc := 0; // expected crc
    for i := 0 to size - 2 - 1 do
      ecrc := CrcClavia(ecrc, PStaticByteBuffer(g_bin_buf.Memory)^[i]);

    acrc := PStaticByteBuffer(g_bin_buf.Memory)^[ size-1] // actual crc
          + PStaticByteBuffer(g_bin_buf.Memory)^[ size-2] * 256;

    if ecrc <> acrc then
      add_log_line('Bad crc exp: ' + IntToHex(ecrc,2) + ' act: ' + IntToHex(acrc,2), LOGCMD_ERR);
  end;
  Result := bytes_read;
end;

function TG2USB.embedded_message(var iin_buf : TByteBuffer): integer;
var i, dil : integer;
    ecrc, acrc : word;
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

function TG2USB.isextended( buf_in : TByteBuffer): boolean;
begin
  Result := buf_in[0] and $f = 1;
end;

function TG2USB.isembedded( buf_in : TByteBuffer): boolean;
begin
  Result := buf_in[0] and $f = 2;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//                  ACTIVATE/DEACTIVATE COMMUNICATION
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

function TG2USB.GetUSBActive: boolean;
begin
  if FIsServer then
{$IF Defined(MSWINDOWS) or Defined(MACOS)}
    Result := Assigned(g2udev)
{$ENDIF}
{$IF Defined(Android)}
    Result := Assigned(FUsbDevice)
{$ENDIF}
  else
    if assigned(FIdTCPClient) then
      Result := FIdTCPClient.Connected
    else
      Result := False;
end;

procedure TG2USB.SetUSBActive(const Value: boolean);
var iin_buf : TByteBuffer;
    bytes_read : integer;
    timer_start : Cardinal;
begin
  if Value then begin
    add_log_line( 'Initializing connection.', LOGCMD_NUL);

    // Only a server can have an USB connection
    if FIsServer then begin
      add_log_line( 'Started as server.', LOGCMD_NUL);

{$IF Defined(MSWINDOWS) or Defined(MACOS)}
      add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);

      if assigned(g2udev) then
        LibUSBDone;
{$ENDIF}

      if assigned(FIdTCPClient) then begin
        FIdTCPClient.Disconnect;
        FIdTCPClient.Free;
        FIdTCPClient := nil;
      end;


      if FTCPIPEnabled then begin
        add_log_line( 'Starting server tcp-ip thread.', LOGCMD_NUL);

        // Create server object
        FIdTCPServer := TIdTCPServer.Create( Self);
        FIdTCPServer.OnExecute := IdTCPServerExecute;
        FIdTCPServer.Bindings.DefaultPort := FPort;
        // set the context class of the server to our custom TClientContext
        // the server will create automatically our class instance
        // when a client is connected and free it when it disconnects
        FIdTCPServer.ContextClass := TClientContext;
        FIdTCPServer.OnConnect := IdTCPServerConnect;
        FIdTCPServer.OnDisconnect := IdTCPServerDisconnect;
        FIdTCPServer.Active := True;
      end;

      add_log_line( 'initializing usb.', LOGCMD_NUL);

      // Start the message thread only if there's a USB connection
{$IF Defined(Andoid)}
      if USBActive then begin
{$ELSE}
      if assigned(g2dev) then begin
{$ENDIF}
        LibUSBInit;
        //add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);

        add_log_line( 'starting message threads.', LOGCMD_NUL);
        FReceiveMessageThread := TReceiveMessageThread.Create(False, self);
        FSendMessageThread := TSendMessageThread.Create(False, self);

        if assigned( FOnUSBActiveChange) then
          FOnUSBActiveChange( self, True);

        // Start the G2 initialization for the server
        USBStartInit;
      end else
        if assigned( FOnAfterG2Init) then begin
          Performance.SelectedSlot := 0;
          FOnAfterG2Init( self);
        end;

    end else begin
      // Client connects
      add_log_line( 'Started as client.', LOGCMD_NUL);

      if assigned( FIdTCPServer) then begin
        FIdTCPServer.Active := False;
        FIdTCPServer.Free;
        FIdTCPServer := nil;
      end;

      FClientID.ID := GetTickCount; // set the user ID to connected time
      FClientID.ClientType := ClientType;

      FIdTCPClient := TIdTCPClient.Create( self);

      FIdTCPClient.Port := FPort;
      FIdTCPClient.Host := FHost;
      FTCPErrorMessage := '';
      try
        add_log_line( 'Connect to server, ip = ' + FHost + ', port = ' + IntToStr(FPort), LOGCMD_NUL);

        FIdTCPClient.Connect; // attempt connection


        if FIdTCPClient.Connected then begin
          // if we are connected, create a listener thread instance
          FIdClientReadThread := TIdClientReadThread.Create(self);

          sleep(250); // Wait for listening thread to start

          add_log_line( 'Connect to server ' + FHost + '.', LOGCMD_NUL);
          ClientSendConnectedToServer;

          if assigned( FOnUSBActiveChange) then
            FOnUSBActiveChange( self, True);

          // Start the G2 initialization for the client
          USBStartInit;
        end;
      except on E:Exception do begin
          FTCPErrorMessage := E.Message;
          LastError := E.Message;
          add_log_line( E.Message, LOGCMD_ERR);
        end;

      end;
    end;
  end else begin
    add_log_line( 'Closing connection.', LOGCMD_NUL);

    // Disconnect
    if USBActive then begin
      // Terminate the USB message thread
      try
        if assigned( FSendMessagethread) then begin
          add_log_line( 'Stop send message thread.', LOGCMD_NUL);
          FSendMessageThread.Terminate;
          FreeAndNil(FSendMessageThread);
        end;
      except on E:Exception do
        add_log_line( E.Message, LOgCMD_ERR);
      end;

      try
        if assigned( FReceiveMessageThread) then begin
          add_log_line( 'Stop receive message thread.', LOGCMD_NUL);
          FReceiveMessageThread.Terminate;
          // Close usb connection
          LibUSBDone;
          FreeAndNil(FReceiveMessageThread);
        end;
      except on E:Exception do
        add_log_line( E.Message, LOgCMD_ERR);
      end;

      FInitialized := False;
    end;

    if (not FIsServer) and assigned(FIdTCPCLient) and FIdTCPCLient.Connected then begin
      add_log_line( 'Disconnect client.', LOGCMD_NUL);
      FIdTCPCLient.Disconnect;  // disconnect client
      FreeAndNil(FIdClientReadThread);
    end;

    if assigned(FIdTCPServer) then begin
      add_log_line( 'Stop server thread.', LOGCMD_NUL);
      if FIdTCPServer.Active then
        FIdTCPServer.Active := False;
      FIdTCPServer.Free;
      FIdTCPServer := nil;
    end;

    if assigned(FIdTCPClient) then begin
      FIdTCPCLient.Free;
      FIdTCPCLient := nil;
    end;

    if assigned( FOnUSBActiveChange) then
      FOnUSBActiveChange( self, False);
  end;
end;

{procedure TG2USB.SetUSBActive2(const Value: boolean);
var iin_buf : TByteBuffer;
    bytes_read : integer;
    timer_start : Cardinal;
begin
  if Value then begin
    add_log_line( 'Initializing connection.', LOGCMD_NUL);

    // Only a server can have an USB connection
    if FIsServer then begin
      add_log_line( 'Started as server.', LOGCMD_NUL);

      add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);
      if assigned(g2udev) then
        LibUSBDone;

      if assigned(FIdTCPClient) then begin
        FIdTCPClient.Disconnect;
        FIdTCPClient.Free;
        FIdTCPClient := nil;
      end;

      add_log_line( 'Starting server tcp-ip thread.', LOGCMD_NUL);

      // Create server object
      FIdTCPServer := TIdTCPServer.Create( Self);
      FIdTCPServer.OnExecute := IdTCPServerExecute;
      FIdTCPServer.Bindings.DefaultPort := FPort;
      // set the context class of the server to our custom TClientContext
      // the server will create automatically our class instance
      // when a client is connected and free it when it disconnects
      FIdTCPServer.ContextClass := TClientContext;
      FIdTCPServer.OnConnect := IdTCPServerConnect;
      FIdTCPServer.OnDisconnect := IdTCPServerDisconnect;
      if FTCPIPEnabled then
        FIdTCPServer.Active := True;

      add_log_line( 'initializing usb.', LOGCMD_NUL);
      LibUSBInit;
      add_log_line( 'g2udev handle = ' + IntToHex(integer(g2udev), 4), LOGCMD_NUL);

      // Start the message thread only if there's a USB connection
      if assigned(g2udev) then begin
        add_log_line( 'starting message threads.', LOGCMD_NUL);
        FReceiveMessageThread := TReceiveMessageThread.Create(False, self);
$IFDEF MSWINDOWS
        FReceiveMessageThreadHandle := FReceiveMessageThread.Handle;
$ENDIF

        FSendMessageThread := TSendMessageThread.Create(False, self);
$IFDEF MSWINDOWS
        FSendMessageThreadHandle := FSendMessageThread.Handle;
$ENDIF
      end;

      if assigned(g2udev) then begin
        if assigned( FOnUSBActiveChange) then
          FOnUSBActiveChange( self, True);

        // Start the G2 initialization for the server
        USBStartInit;
      end else
        if assigned( FOnAfterG2Init) then begin
          Performance.SelectedSlot := 0;
          FOnAfterG2Init( self);
        end;

    end else begin
      // Client connects
      add_log_line( 'Started as client.', LOGCMD_NUL);

      if assigned( FIdTCPServer) then begin
        FIdTCPServer.Active := False;
        FIdTCPServer.Free;
        FIdTCPServer := nil;
      end;

      FClientID.ID := GetTickCount; // set the user ID to connected time
      FClientID.ClientType := ClientType;

      FIdTCPClient := TIdTCPClient.Create( self);

      FIdTCPClient.Port := FPort;
      FIdTCPClient.Host := FHost;
      FTCPErrorMessage := '';
      try
        add_log_line( 'Connect to server, ip = ' + FHost + ', port = ' + IntToStr(FPort), LOGCMD_NUL);

        FIdTCPClient.Connect; // attempt connection


        if FIdTCPClient.Connected then begin
          // if we are connected, create a listener thread instance
          FIdClientReadThread := TIdClientReadThread.Create(self);

          sleep(250); // Wait for listening thread to start

          add_log_line( 'Connect to server ' + FHost + '.', LOGCMD_NUL);
          ClientSendConnectedToServer;

          if assigned( FOnUSBActiveChange) then
            FOnUSBActiveChange( self, True);

          // Start the G2 initialization for the client
          USBStartInit;
        end;
      except on E:Exception do begin
          FTCPErrorMessage := E.Message;
          LastError := E.Message;
          add_log_line( E.Message, LOGCMD_ERR);
        end;

      end;
    end;
  end else begin
    add_log_line( 'Closing connection.', LOGCMD_NUL);

    // Disconnect
    if assigned(g2udev) then begin

      if FInitialized then begin
        // Try to stop the led messages stream
        add_log_line( 'Stop G2 message stream.', LOGCMD_NUL);

        LastResponseMessage := 00;
        SendStartStopCommunicationMessage( 1);
        timer_start := GetTickCount;
        repeat
          sleep(100);
          G2ProcessWindowsMessages;
          //Application.ProcessMessages;
        until (LastResponseMessage = R_OK) or (GetTickCount - timer_start > 3000);
      end;
      FInitialized := False;
    end;

    if (not FIsServer) and assigned(FIdTCPCLient) and FIdTCPCLient.Connected then begin
      add_log_line( 'Disconnect client.', LOGCMD_NUL);
      FIdTCPCLient.Disconnect;  // disconnect client
      FIdClientReadThread.WaitFor;
    end;

    // Wait some time to empty the read buffer

    // Terminate the USB message thread
    try
      if assigned( FSendMessagethread) then begin
        add_log_line( 'Stop send message thread.', LOGCMD_NUL);
        FSendMessageThread.Terminate;
$IFDEF MSWINDOWS
        WaitForSingleObject( FSendMessageThreadHandle, 3000);
$ENDIF
        FSendMessageThread := nil;
      end;
    except on E:Exception do
      add_log_line( E.Message, LOgCMD_ERR);
    end;

    try
      if assigned( FReceiveMessageThread) then begin
        add_log_line( 'Stop receive message thread.', LOGCMD_NUL);
        FReceiveMessageThread.Terminate;
      end;
    except on E:Exception do
      add_log_line( E.Message, LOgCMD_ERR);
    end;

    if assigned(g2udev) then begin
      // Read remaining messages until timeout
$IFDEF MSWINDOWS
      add_log_line( 'Empty recieve message buffer.', LOGCMD_NUL);
      timer_start := GetTickCount;
      SetLength(iin_buf, 16);
      bytes_read := iread( g2iin, iin_buf[0], 16, 1000);
      while (bytes_read > 0) and (GetTickCount - timer_start > 3000) do begin
        if isextended(iin_buf) then
          bytes_read := extended_message(iin_buf);
        bytes_read := iread( g2iin, iin_buf[0], 16, 1000);
      end;
$ENDIF
      // Clean up usb connection
      LibUSBDone;
    end;

    if assigned( FReceiveMessageThread) and not(FReceiveMessageThread.Terminated) then begin
$IFDEF MSWINDOWS
      WaitForSingleObject( FReceiveMessageThreadHandle, 3000);
$ENDIF
      FReceiveMessageThread := nil;
    end;

    if assigned(FIdTCPServer) then begin
      add_log_line( 'Stop server thread.', LOGCMD_NUL);
      if FIdTCPServer.Active then
        FIdTCPServer.Active := False;
      FIdTCPServer.Free;
      FIdTCPServer := nil;
    end;

    if assigned(FIdTCPClient) then begin
      FIdTCPCLient.Free;
      FIdTCPCLient := nil;
    end;

    if assigned( FOnUSBActiveChange) then
      FOnUSBActiveChange( self, False);
  end;
end;}

////////////////////////////////////////////////////////////////////////////////
//  TSendMessageThread
//
//  Send messages over USB interface to G2
////////////////////////////////////////////////////////////////////////////////

constructor TSendMessageThread.Create(CreateSuspended: Boolean; aG2: TG2USB);
begin
  FG2USB := aG2;
  // FG2USB.add_log_line('Creating send thread...', LOGCMD_NUL);

  //FreeOnTerminate := True;
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);

{$IFDEF UNIX}
  Priority := tpNormal;
{$ENDIF}
{$IFDEF MACOS}
  //Priority := tpNormal;
{$ENDIF}
{$IFDEF ANDROID}
  //Priority := tpNormal;
{$ENDIF}
{$IFDEF MSWINDOWS}
  Priority := tpHigher;
{$ENDIF}
end;

procedure TSendMessageThread.Execute;
var Error : boolean;
begin
  Error := False;
  FG2USB.FWaitForCmd := 0;

  repeat
    // Waiting for a response? No, send a new command
    if (FG2USB.FWaitForCmd = 0) then begin
      // Send updated parameters (no response)
      FG2USB.USBSendParamUpd;
      // Send new message if any
      if (FG2USB.FSendMessageCount > 0) then begin
        FG2USB.USBSendMessage;
        FLogMessage := 'Usb message send...';
        Synchronize(WriteLog);

      end;
      sleep(1);
    end else
      sleep(10);

    if Error then
      Terminate;

  until Terminated;

  FLogMessage := 'Usb send thread terminated.';
  Synchronize(WriteLog);
end;

procedure TSendMessageThread.SendUSBMessage;
begin
  FG2USB.USBSendMessage;
end;

procedure TSendMessageThread.WriteLog;
begin
  FG2USB.add_log_line( FLogMessage, LOGCMD_ERR);
end;

procedure TSendMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer( FBuffer^, FMaxSize);
end;

////////////////////////////////////////////////////////////////////////////////
//  TReceiveMessageThread
//
//  Receive messages from USB interface
////////////////////////////////////////////////////////////////////////////////

constructor TReceiveMessageThread.Create(CreateSuspended: Boolean; aG2 : TG2USB);
begin
  FG2USB := aG2;
//  FG2USB.add_log_line('Creating thread...', LOGCMD_NUL);

  //FreeOnTerminate := True;
  FreeOnTerminate := False;
  inherited Create(CreateSuspended);

{$IFDEF MSWINDOWS}
// VCL :
  Priority := tpHigher;
// FMX :
//  Priority := tpNormal;
{$ENDIF}
end;

procedure TReceiveMessageThread.Execute;
var iin_buf : TByteBuffer;
    bytes_read : integer;
    error : boolean;
begin
  SetLength(iin_buf, 16);

  error := False;

  repeat
    // Is USB Active?
    if FG2USB.USBActive then begin

      // Read the interrupt message
      // Time out must be long enough, otherwise we lose contact with the G2!
      // TIME_OUT = 0 : wait forever
      bytes_read := FG2USB.iread( iin_buf[0], 16, TIME_OUT);

      if bytes_read > 0 then begin
        FLogMessage := 'Usb interupt message recieved ' + IntToStr(bytes_read);
        Synchronize( WriteLog);

        if FG2USB.isextended(iin_buf) then begin
          bytes_read := FG2USB.extended_message(iin_buf);
          FLogMessage := 'Usb bulk message recieved ' + IntToStr(bytes_read);
          Synchronize( WriteLog);
          Synchronize(ProcessMessage);
        end else
          if FG2USB.isembedded( iin_buf) then begin
            bytes_read := FG2USB.embedded_message(iin_buf);
            Synchronize(ProcessMessage);
          end else begin
            FLogMessage := 'RecThread: Message received is not embedded nor extended.';
            Synchronize( WriteLog);
            Error := True;
          end;
      end else begin
        if bytes_read < 0 then begin
          case bytes_read of
          LIBUSB_ERROR_TIMEOUT :
            begin
              // could not happen with infinite timeout
            end;
          LIBUSB_ERROR_OTHER :
            begin
              // On reset device, exit;
              Terminate;
            end;
          else begin
              Error := True;
              FLogMessage := 'RecThread: Interrupt read error ' + IntToStr(bytes_read);
              Synchronize( WriteLog);
              FBuffer := FG2USB.g_bout_buf.Memory;
              FMaxSize :=  FG2USB.g_bout_buf.Size;
              Synchronize( DumpMessage);
            end;
          end;
        end;
      end;
    end;

    if Error then
      Terminate;

  until Terminated;

  FLogMessage := 'Usb send thread terminated.';
  Synchronize(WriteLog);
end;

procedure TReceiveMessageThread.ProcessMessage;
begin
  FG2USB.USBProcessMessage( FG2USB.g_bin_buf);
end;

procedure TReceiveMessageThread.WriteLog;
begin
  FG2USB.add_log_line( FLogMessage, LOGCMD_NUL);
end;

procedure TReceiveMessageThread.DumpMessage;
begin
  FG2USB.dump_buffer( FBuffer^, FMaxSize);
end;

////////////////////////////////////////////////////////////////////////////////
//  Processing USB Messages
////////////////////////////////////////////////////////////////////////////////

procedure TG2USB.USBSendMessage;
var retries, total_bytes_send, bytes_send, packet : integer;
    MessageList : TList;
    ClientSendMessage : TClientSendMessage;
begin
  // Send a message to the G2 over the USB interface

  if FSendMessageCount = 0 then
    exit;

  // Read top message in queue
  MessageList := FSendMessageQueue.LockList;
  try
    ClientSendMessage := TClientSendMessage( MessageList[0]);
    ClientSendMessage.FMessage.PrepareForSend;
    g_bout_buf.Clear;
    g_bout_buf.Write( ClientSendMessage.FMessage.Memory^,
                      ClientSendMessage.FMessage.Size);
  finally
    FSendMessageQueue.UnlockList;
  end;

  // Init expected answer to command
  FWaitForCmd := g_bout_buf.Command;
  FMessageSendStart := GetTickCount;

  if USBActive then begin

    if g_bout_buf.size > 0  then begin
    // Break the message up in packets, but for the G2 the messages aren't very big thus maybe unecessary
      retries := 5;
      total_bytes_send := 0;
      bytes_send := 0;
      while (total_bytes_send < g_bout_buf.size) and ((bytes_send >= 0) or (bytes_send = LIBUSB_ERROR_TIMEOUT)) and (retries > 0) do begin

        if (g_bout_buf.size - total_bytes_send) > MAX_BULK_DATA_SIZE then
          packet := MAX_BULK_DATA_SIZE
        else
          packet := ( g_bout_buf.size - total_bytes_send);

        bytes_send := bwrite(PStaticByteBuffer(g_bout_buf.Memory)^[total_bytes_send], packet, TIME_OUT);
        if bytes_send > 0 then
          total_bytes_send := total_bytes_send + bytes_send
        else
          if bytes_send = LIBUSB_ERROR_TIMEOUT then
            dec(retries);
      end;

      if (retries = 0) and (total_bytes_send < g_bout_buf.size) then begin
        add_log_line( 'Timeout sending message', LOGCMD_ERR);
      end;

    end else
      exit; // nothing to send
  end;
end;

procedure TG2USB.USBSendParamUpd;
var i, j : integer;
    SlotIndex , Size : byte;
    Slot : TG2USBSlot;
    crc : word;
    ClientSendMessage : TClientSendMessage;
begin
  // Send all responseless messages with this function

  ClientSendMessage := TClientSendMessage.Create;
  ClientSendMessage.FMessage := g_bout_buf;
  try
    g_bout_buf.Size := 20; // long enough for the longest message

    for SlotIndex := 0 to 3 do begin

      Slot := GetSlot(SlotIndex);

      for i := 0 to Slot.FParamUpdBufCount - 1 do begin
        if Slot.FParamUpdBuf[i].Changed then begin
          case Slot.FParamUpdBuf[i].SubCmd of
            S_SET_PARAM :
              begin
                Size := 13;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SET_PARAM;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Param;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Value;
                PStaticByteBuffer(g_bout_buf.Memory)^[10] := Slot.FParamUpdBuf[i].Variation;
              end;
            S_SEL_PARAM :
              begin
                Size := 12;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SEL_PARAM;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := 00;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Param;
              end;
            S_SET_MORPH_RANGE :
              begin
                Size := 15;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 0] := 0;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 1] := Size;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 2] := $01;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 3] := CMD_NO_RESP + CMD_SLOT + SlotIndex;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 4] := Slot.PatchVersion; // Current patch version!
                PStaticByteBuffer(g_bout_buf.Memory)^[ 5] := S_SET_MORPH_RANGE;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 6] := Slot.FParamUpdBuf[i].Location;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 7] := Slot.FParamUpdBuf[i].Module;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 8] := Slot.FParamUpdBuf[i].Param;
                PStaticByteBuffer(g_bout_buf.Memory)^[ 9] := Slot.FParamUpdBuf[i].Morph;
                PStaticByteBuffer(g_bout_buf.Memory)^[10] := Slot.FParamUpdBuf[i].Value;
                PStaticByteBuffer(g_bout_buf.Memory)^[11] := Slot.FParamUpdBuf[i].Negative;
                PStaticByteBuffer(g_bout_buf.Memory)^[12] := Slot.FParamUpdBuf[i].Variation;
              end;
            else begin
              Size := 0;
            end;
          end;

          if Size > 3 then begin

            // Calc CRC
            crc := 0;
            for j := 2 to Size - 3 do
              crc := CrcClavia(crc, PStaticByteBuffer(g_bout_buf.Memory)^[j]);

            // Write CRC
            PStaticByteBuffer(g_bout_buf.Memory)^[Size - 2] := CRC div 256;
            PStaticByteBuffer(g_bout_buf.Memory)^[Size - 1] := CRC mod 256;;

            // Online?
            if USBActive then
              bwrite( PStaticByteBuffer(g_bout_buf.Memory)^, Size, TIME_OUT);

          end;

          Slot.FParamUpdBuf[i].Changed := False;

          ClientSendMessage.FClientContext := Slot.FParamUpdBuf[i].ClientContext;
          USBProcessSendMessage( ClientSendMessage);
        end;
      end;
    end;
  finally
    ClientSendMessage.Free;
  end;
end;

procedure TG2USB.USBProcessMessage( ResponseMessage : TG2ResponseMessage);
var Cmd : byte;
    MessageList : TList;
    ClientSendMessage : TClientSendMessage;
begin
  // Process a response message received over USB

  add_log_line( 'USBProcessMessage ' + IntToStr(ResponseMessage.Size), LOGCMD_NUL);

  Cmd := ResponseMessage.Command;

  add_log_line( 'Process response message CMD ' + IntToStr(Cmd), LOGCMD_NUL);

  ErrorMessage := False;
  USBProcessResponseMessage( ResponseMessage);

  if (FWaitForCmd <> 0) and ((Cmd = FWaitForCmd) or ((Cmd = $04) and (FWaitForCmd = $0c)) or ErrorMessage) then begin

     add_log_line( 'Get send message from list.', LOGCMD_NUL);

    // Get the send message from the list
    MessageList := FSendMessageQueue.LockList;
    try
      ClientSendMessage := TClientSendMessage( MessageList[0]);
      MessageList.Delete(0);
      FSendMessageCount := MessageList.Count;
    finally
      FSendMessageQueue.UnlockList;
    end;

    try
      add_log_line( 'Process send message.', LOGCMD_NUL);
      if not ErrorMessage then
        USBProcessSendMessage( ClientSendMessage);

      if assigned(FOnReceiveResponseMessage) then
        FOnReceiveResponseMessage( Self, ResponseMessage);

    finally
      ClientSendMessage.FMessage.Free;
      ClientSendMessage.Free;
      FWaitForCmd := 0;
    end;

    // Send next message in a sequence, if assigned
    if assigned(GetSlot(0)) and assigned(GetSlot(0).FOnNextInitStep)  then
      GetSlot(0).FOnNextInitStep(self)
    else

    if assigned(GetSlot(1)) and assigned(GetSlot(1).FOnNextInitStep)  then
      GetSlot(1).FOnNextInitStep(self)
    else

    if assigned(GetSlot(2)) and assigned(GetSlot(2).FOnNextInitStep)  then
      GetSlot(2).FOnNextInitStep(self)
    else

    if assigned(GetSlot(3)) and assigned(GetSlot(3).FOnNextInitStep)  then
      GetSlot(3).FOnNextInitStep(self)
    else

    if assigned(Performance) and assigned(GetPerformance.FOnNextInitStep) then
      GetPerformance.FOnNextInitStep(self)
    else

    if assigned(FOnNextInitStep) then
      FOnNextInitStep(self);
  end;
end;

procedure TG2USB.USBProcessSendMessage( ClientSendMessage : TClientSendMessage);
begin
  // Responseless messages that originate from the server are already processed in patch
  if not((ClientSendMessage.FClientContext = nil) and not(ClientSendMessage.FMessage.HasResponse)) then begin
    ClientSendMessage.FMessage.Position := 0;


    if not ProcessSendMessage( ClientSendMessage.FMessage, ClientSendMessage.FMessageSender.ID) then
       add_log_line( DateTimeToStr(Now) + ' Send message not processed.', LOGCMD_ERR);
  end;

  ServerBroadCastSendMessage( ClientSendMessage);
end;

procedure TG2USB.USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage);
var Size : byte;
begin
  add_log_line( 'USBProcessResponseMessage ' + IntToStr(ResponseMessage.Size), LOGCMD_NUL);

  if ResponseMessage.IsEmbedded then begin
    ResponseMessage.Position := 1; // Skip first byte if embedded
    Size := PStaticByteBuffer(ResponseMessage.Memory)^[0] shr 4;
    ResponseMessage.Size := Size;
  end else
    ResponseMessage.Position := 0;

  if not ProcessResponseMessage( ResponseMessage, 0) then
    add_log_line( DateTimeToStr(Now) + ' Response message not processed.', LOGCMD_ERR);

  if not( ResponseMessage.IsLedData) or FLogLedDataMessages then begin
    add_log_line( '', LOGCMD_NUL);
    add_log_line( 'Broadcast : ', LOGCMD_NUL);
    dump_buffer( PStaticByteBuffer( ResponseMessage.Memory)^, ResponseMessage.Size);
  end;

  ServerBroadCastResponseMessage( ResponseMessage);
end;

////////////////////////////////////////////////////////////////////////////////
// Communication between Server and client
////////////////////////////////////////////////////////////////////////////////

function TG2USB.GetID : integer;
begin
  Result := FClientID.ID;
end;

procedure TG2USB.IdTCPServerConnect( AContext: TIdContext);
var LClientContext: TClientContext;
begin
  LClientContext := TClientContext(AContext);

  add_log_line( 'Client connect : ' + AContext.Binding.PeerIP
              + ', port : ' + IntToStr( AContext.Binding.PeerPort)
              + ', remote host : ' {+ GStack.WSGetHostByAddr(AContext.Binding.PeerIP)}
              + ', type : ' + IntToStr(ord( LClientContext.Client.ClientType)), LOGCMD_NUL); // <-- here
  if assigned(FOnAddClient) then
    FOnAddClient(self, 0);
end;

procedure TG2USB.IdTCPServerDisconnect( AContext: TIdContext);
begin
  add_log_line( 'Client disconnect : ' + AContext.Binding.PeerIP, LOGCMD_NUL);
  if assigned(FOnDeleteClient) then
    FOnDeleteClient(self, 0);
end;

////////////////////////////////////////////////////////////////////////////////
// Client context for server
////////////////////////////////////////////////////////////////////////////////

{$IFDEF VER230}
constructor TClientContext.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList);
{$ELSE}
{$IFDEF VER230}
constructor TClientContext.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TThreadList);
{$ELSE}
constructor TClientContext.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList);
{$ENDIF}
{$ENDIF}
begin
  inherited Create(AConnection, AYarn, AList);
  // create the critical section
  FClientContextCriticalSection := TCriticalSection.Create;
  FLastWrite := 0;
end;

destructor TClientContext.Destroy;
begin
  // free and nil critical section
  FreeAndNil(FClientContextCriticalSection);
  inherited;
end;

procedure TClientContext.Lock;
begin
  if assigned(FClientContextCriticalSection) then
    FClientContextCriticalSection.Enter;
end;

procedure TClientContext.ServerSendClientSendMessage( ClientSendMessage : TClientSendMessage);
var LProtocol: TProtocol;
    LBuffer: TMemoryStream;
begin
  // fill protocol variable with zero's
  FillChar( LProtocol, SizeOf(LProtocol), 0);
  LProtocol.Command := cmdSendMessage;
  LProtocol.Sender := ClientSendMessage.FMessageSender;
  LProtocol.MessageType := mdtSendMessage;
  LProtocol.DataSize := ClientSendMessage.FMessage.Size;

  LBuffer := ProtocolToStream(LProtocol);
  // set the length of the buffer to <size of protocol structure> + <message length>
  LBuffer.Size := szProtocol + LProtocol.DataSize;
  // move message to buffer
  Move( ClientSendMessage.FMessage.Memory^, PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
  Lock;
  try
    // write the buffer
    Connection.IOHandler.Write(LBuffer);
  finally
    // unlock client
    Unlock;
    LBuffer.Free;
  end;
end;

procedure TClientContext.ServerSendClientResponseMessage( MemStream : TMemoryStream);
var LProtocol: TProtocol;
    LBuffer: TMemoryStream;
begin
  // fill protocol variable with zero's
  InitProtocol(LProtocol);
  LProtocol.Command := cmdSendMessage;
  LProtocol.Sender := FClient;
  LProtocol.MessageType := mdtResponseMessage;
  LProtocol.DataSize := MemStream.Size;
  //LBuffer := ProtocolToBytes(LProtocol);
  LBuffer := ProtocolToStream(LProtocol);
  // set the length of the buffer to <size of protocol structure> + <message length>
  LBuffer.Size := szProtocol + LProtocol.DataSize;
  // move message to buffer
  Move( MemStream.Memory^, PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
  Lock;
  try
    // write the buffer
    Connection.IOHandler.Write(LBuffer);
  finally
    // unlock client
    Unlock;
    LBuffer.Free;
  end;
end;

procedure TClientContext.Unlock;
begin
  if assigned(FClientContextCriticalSection) then
    FClientContextCriticalSection.Leave;
end;

////////////////////////////////////////////////////////////////////////////////
// Server listening thread
//
// Listening en responding to messages received from clients
////////////////////////////////////////////////////////////////////////////////

procedure TG2USB.IdTCPServerExecute( AContext: TIdContext);
var
  LBuffer: TMemoryStream;
  LDataSize: Integer;
  LProtocol: TProtocol;
  LClientContext: TClientContext; // we need to HARD CAST AContext to TClientContext in order to access our custom methods(procedures)
  MyNotify: TG2ProcessClientSendMessage;
begin
  // hard cast AContext to TClientContext
  LClientContext := TClientContext(AContext);
  LDataSize := LClientContext.Connection.IOHandler.InputBuffer.Size;
  if LDataSize >= szProtocol then begin
    LBuffer := TMemoryStream.Create;
    try
      LClientContext.Connection.IOHandler.ReadStream( LBuffer, szProtocol);
      LProtocol := StreamToProtocol(LBuffer);
      // check client command and act accordingly
      case LProtocol.Command of
        cmdConnect:
          begin
            add_log_line('<connected> : ' + IntToStr(LProtocol.Sender.ID), LOGCMD_NUL);
            LClientContext.Client := LProtocol.Sender;
          end;
        cmdDisconnect:
          begin
            add_log_line('<disconnected> %s' + IntToStr(LProtocol.Sender.ID), LOGCMD_NUL);
          end;
        cmdSendMessage :
          begin
            MyNotify := TG2ProcessClientSendMessage.Create;
            MyNotify.FG2 := self;
            MyNotify.FBuffer := TMemoryStream.Create;
            LClientContext.Connection.IOHandler.ReadStream(MyNotify.FBuffer, LProtocol.DataSize);
            MyNotify.FClient := LProtocol.Sender;
            MyNotify.FClientContext := LClientContext;
            MyNotify.Notify;
          end;
      end;
    finally
      LBuffer.Free;
    end;
  end else
    sleep(5);
end;

destructor TG2ProcessClientSendMessage.Destroy;
begin
  if assigned(FBuffer) then
    FBuffer.Free;

  inherited;
end;

procedure TG2ProcessClientSendMessage.DoNotify;
var ClientSendMessage : TClientSendMessage;
begin
  ClientSendMessage := TClientSendMessage.Create;
  ClientSendMessage.FClientContext := FClientContext;
  ClientSendMessage.FMessageSender := FClient;
  ClientSendMessage.FMessage := TG2SendMessage.Create;
  ClientSendMessage.FMessage.Clear;
  ClientSendMessage.FMessage.Size := FBuffer.Size;
  Move( FBuffer.Memory^, ClientSendMessage.FMessage.Memory^, FBuffer.Size);
  FBuffer.Free;
  FBuffer := nil;
  FG2.ServerProcessClientMessage( ClientSendMessage);
end;

procedure TG2USB.ServerBroadcastResponseMessage( ResponseMessage : TG2ResponseMessage);
var i : integer;
{$IFDEF IOS}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
{$IFDEF ANDROID}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
    LClients: TList;
{$ENDIF}
{$ENDIF}
    LClientContext: TClientContext;
begin
  if not assigned(FIdTCPServer) then
    exit;

  // Send message to the connected clients
  LClients := FIdTCPServer.Contexts.LockList;
  try
    for i := 0 to LClients.Count -1 do begin
      LClientContext := TClientContext(LClients[i]);
      if ResponseMessage.IsLedData then begin
        // Don't send led data to vst's or client editors that are not initialized yet.
        if (LClientContext.Client.ClientType <> ctVST) and (LClientContext.Initialized)
           and (GetTickCount - Cardinal(LClientContext.LastWrite) > Cardinal(FTimerBroadcastLedMessages)) then begin

          LClientContext.Lock;
          try
            LClientContext.LastWrite := GetTickCount;
            LClientContext.ServerSendClientResponseMessage( ResponseMessage);
          finally
            LClientContext.Unlock;
          end;
        end;
      end else begin

        LClientContext.Lock;
        try
          LClientContext.ServerSendClientResponseMessage( ResponseMessage);
        finally
          LClientContext.Unlock;
        end;
      end;
    end;
  finally
    // unlock client list
    FIdTCPServer.Contexts.UnlockList;
  end;
end;

procedure TG2USB.ServerBroadcastSendMessage( ClientSendMessage : TClientSendMessage);
var i : integer;
{$IFDEF IOS}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
{$IFDEF ANDROID}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
    LClients: TList;
{$ENDIF}
{$ENDIF}
    LClientContext: TClientContext;
    SenderIsThisClient : boolean;
begin
  if not assigned(FIdTCPServer) then
    exit;

  // Send message to the connected clients
  LClients := FIdTCPServer.Contexts.LockList;
  try
    for i := 0 to LClients.Count -1 do begin
      LClientContext := TClientContext(LClients[i]);

      SenderIsThisClient := (ClientSendMessage.FClientContext <> nil)
                        and (LClientContext.FClient.ID = ClientSendMessage.FClientContext.FClient.ID);
      // Don't send responseless messages back to sender
      if not(SenderIsThisClient and not(ClientSendMessage.FMessage.HasResponse)) then begin
        LClientContext.Lock;
        try
          LClientContext.ServerSendClientSendMessage( ClientSendMessage);
        finally
          LClientContext.Unlock;
        end;
      end;
    end;
  finally
    // unlock client list
    FIdTCPServer.Contexts.UnlockList;
  end;
end;

function TG2USB.GetClientCount: integer;
var i : integer;
{$IFDEF IOS}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
{$IFDEF ANDROID}
    LClients: TList<idContext.TIdContext>;
{$ELSE}
    LClients: TList;
{$ENDIF}
{$ENDIF}
begin
  Result := 0;

  if not assigned(FIdTCPServer) then
    exit;

  if assigned(FIdTCPServer) then begin
    LClients := FIdTCPServer.Contexts.LockList;
    try
      for i := 0 to LClients.Count - 1 do
        if TClientContext(LClients[i]).Connection.Connected then
          inc(Result);
    finally
      FIdTCPServer.Contexts.UnlockList;
    end;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Client listening thread
//
// Listening and responding to messages send from server
////////////////////////////////////////////////////////////////////////////////

constructor TIdClientReadThread.Create(AG2: TG2USB);
begin
  inherited Create(False);
  FG2 := AG2;
  FreeOnTerminate := False;
  LMessage := TMemoryStream.Create;
end;

destructor TIdClientReadThread.Destroy;
begin
  LMessage.Free;
  inherited;
end;

procedure TG2USB.Lock;
begin
  FClientCriticalSection.Enter;
end;

procedure TG2USB.Unlock;
begin
  FClientCriticalSection.Leave;
end;

procedure TIdClientReadThread.Run;
var
  LBuffer : TMemoryStream;
  LDataSize: Integer;
begin
  FG2.idTCPClient.IOHandler.CheckForDataOnSource(50);
  if not FG2.idTCPClient.IOHandler.InputBufferIsEmpty then begin
    LBuffer := TMemoryStream.Create;
    try
      LDataSize := FG2.idTCPClient.IOHandler.InputBuffer.Size;

      if LDataSize >= szProtocol then begin
        FG2.Lock;
        try
          FG2.idTCPClient.IOHandler.ReadStream( LBuffer, szProtocol);
        finally
          FG2.Unlock;
        end;

        move( LBuffer.Memory^, LProtocol, szProtocol);
        // check the command, only one at the moment
        case LProtocol.Command of
          cmdSendMessage :
            begin
              LMessage.Clear;
              // Process the message received from the server
              FG2.Lock;
              try
                FG2.idTCPClient.IOHandler.ReadStream(LMessage, LProtocol.DataSize);
              finally
                FG2.Unlock;
              end;
              // Synchronize doesn't work with dll's, so a critical section is used (lock) for VST
{$IFDEF G2_VST}
              // Don't us the critical section around these procedures, because
              // they can send messages back to the server in the same critical section.
              case LProtocol.MessageType of
                mdtSendMessage     : DoClientProcessServerSendMessage;
                mdtResponseMessage : DoClientProcessServerResponseMessage;
              end;
{$ELSE}
              case LProtocol.MessageType of
                mdtSendMessage     : Synchronize(DoClientProcessServerSendMessage);
                mdtResponseMessage : Synchronize(DoClientProcessServerResponseMessage);
              end;
{$ENDIF}
            end;
        end;
      end;
    finally
      LBuffer.Free;
    end;
  end else
    sleep(100);
  FG2.idTCPClient.IOHandler.CheckForDisconnect;
end;

procedure TIdClientReadThread.DoClientProcessServerResponseMessage;
begin
  FG2.ClientProcessServerResponseMessage( LMessage)
end;

procedure TIdClientReadThread.DoClientProcessServerSendMessage;
begin
  FG2.ClientProcessServerSendMessage( LMessage, LProtocol.Sender.ID)
end;

procedure TG2USB.ClientProcessServerResponseMessage( MemStream : TMemoryStream);
var Cmd, b : byte;
begin
  // Client receives a response message from server
  MemStream.Position := 0;

  if PStaticByteBuffer(MemStream.Memory)^[0] = CMD_INIT then
    CMD := CMD_INIT
  else
    if (PStaticByteBuffer(MemStream.Memory)^[0] and $f) = 2 then begin
      CMD := PStaticByteBuffer(MemStream.Memory)^[2];  // Embedded
      MemStream.Read(b, 1); // skip first byte;
    end else
      CMD := PStaticByteBuffer(MemStream.Memory)^[1]; // Extended

  //add_log_line( 'Client received message ' + IntToHex( Cmd, 2), LOGCMD_NUL);

  try
    if not ProcessResponseMessage( MemStream, 0) then
      add_log_line( DateTimeToStr(Now) + ' Response message not processed.', LOGCMD_ERR);
  except on E:Exception do
    add_log_line( DateTimeToStr(Now) + ' ' + E.Message, LOGCMD_ERR);
  end;

  // Is this what we've been waiting for?
  if Cmd = FWaitforCmd then begin
    FWaitforCmd := 0;

    if assigned(FOnReceiveResponseMessage) then
      FOnReceiveResponseMessage( Self, MemStream);

    // Still connected?
    if FidTCPClient.Connected then begin

      // Send next message in a sequence, if assigned
      if assigned(GetSlot(0)) and assigned(GetSlot(0).FOnNextInitStep)  then
        GetSlot(0).FOnNextInitStep(self)
      else

      if assigned(GetSlot(1)) and assigned(GetSlot(1).FOnNextInitStep)  then
        GetSlot(1).FOnNextInitStep(self)
      else

      if assigned(GetSlot(2)) and assigned(GetSlot(2).FOnNextInitStep)  then
        GetSlot(2).FOnNextInitStep(self)
      else

      if assigned(GetSlot(3)) and assigned(GetSlot(3).FOnNextInitStep)  then
        GetSlot(3).FOnNextInitStep(self)
      else

      if assigned(Performance) and assigned(GetPerformance.FOnNextInitStep) then
        GetPerformance.FOnNextInitStep(self)
      else

      if assigned(FOnNextInitStep) then
        FOnNextInitStep(self);
    end;
  end;
end;

procedure TG2USB.ClientProcessServerSendMessage( MemStream : TMemoryStream; SenderID : integer);
begin
  // Client receives a send message
  MemStream.Position := 0;
  try
    if not ProcessSendMessage( MemStream, SenderID) then
      add_log_line( DateTimeToStr(now) + ' Send message not processed.', LOGCMD_ERR);
  except on E:Exception do
    add_log_line( DateTimeToStr(now) + E.Message, LOGCMD_ERR);
  end;
end;

procedure TG2USB.ClientSendConnectedToServer;
var
  LBuffer: TMemoryStream;
  LProtocol: TProtocol;
begin
  InitProtocol(LProtocol);
  LProtocol.Command := cmdConnect;
  LProtocol.Sender := FClientID;
  LBuffer := ProtocolToStream(LProtocol);
  FClientCriticalSection.Enter;
  try
    FidTCPClient.IOHandler.Write(LBuffer);
  finally
    LBuffer.Free;
    FClientCriticalSection.Leave;
  end;
end;

procedure TG2USB.ClientSendMessageToServer( MemStream : TMemoryStream);
var
  LBuffer: TMemoryStream;
  LProtocol: TProtocol;
begin
  // Client sending message to server

  if not( assigned(FIdTCPClient) and FIdTCPClient.Connected) then
    exit;

  InitProtocol(LProtocol);
  LProtocol.Command := cmdSendMessage;
  LProtocol.MessageType := mdtSendMessage;
  LProtocol.Sender := FClientID;
  LProtocol.DataSize := MemStream.Size;
  //LBuffer := ProtocolToBytes(LProtocol);
  LBuffer := ProtocolToStream(LProtocol);
  Lock;
  try
    LBuffer.Size := szProtocol + LProtocol.DataSize;
    Move(PStaticByteBuffer(MemStream.Memory)^[0], PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
    FIdTCPClient.IOHandler.Write(LBuffer);
  finally
    LBuffer.Free;
    Unlock;
  end;
end;

function TG2USB.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
var ClientSendMessage : TClientSendMessage;
    MessageList : TList;
begin
   // Send message from server to G2 or from client to server

   // Return True if message was send
   Result := False;
   ErrorMessage := False;
   ErrorMessageNo := 0;

   add_log_line( '', LOGCMD_NUL);
   add_log_line( 'Send message, size = ' + IntToStr(SendMessage.Size) , LOGCMD_NUL);
   dump_buffer( SendMessage.Memory^, SendMessage.Size);

  if FIsServer then begin

    // Online?
    if USBActive then begin

      // Server, send message over USB to G2

      if assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage( self, ID, SendMessage);

      add_log_line('Send buffer count was ' + IntTostr(FSendMessageCount), LOGCMD_NUL);

      // Add message to send queue. The message is removed after the message is
      // fully processed in broadcast message by the ReceiveMessage thread.
      ClientSendMessage := TClientSendMessage.Create;
      ClientSendMessage.FMessageSender := FClientID;
      ClientSendMessage.FClientContext := nil;
      ClientSendMessage.FMessage := SendMessage;

      MessageList := FSendMessageQueue.LockList;
      try
        MessageList.Add( ClientSendMessage);
        FSendMessageCount := MessageList.Count;
      finally
        FSendMessageQueue.UnlockList;
      end;
      Result := True;


      add_log_line('Send buffer count is ' + IntTostr(FSendMessageCount), LOGCMD_NUL);

    end else begin
      // Not online
      ClientSendMessage := TClientSendMessage.Create;
      ClientSendMessage.FMessageSender := FClientID;
      ClientSendMessage.FClientContext := nil;
      ClientSendMessage.FMessage := SendMessage;
      try
        USBProcessSendMessage( ClientSendMessage);
        Result := True;
      finally
        SendMessage.Free;
        ClientSendMessage.Free;
      end;
    end;

  end else begin
    // Client

    if not( assigned(FIdTCPClient) and FIdTCPClient.Connected) then begin
      SendMessage.Free;
      exit;
    end;

    FWaitforCmd := SendMessage.Command;

    // Client, send message to server
    if assigned(FIdTCPClient) and FIdTCPClient.Connected then begin

      if assigned(FOnBeforeSendMessage) then
        FOnBeforeSendMessage( self, ID, SendMessage);

      PStaticByteBuffer( SendMessage.Memory)^[0] := SendMessage.Size div 256;
      PStaticByteBuffer( SendMessage.Memory)^[1] := SendMessage.Size mod 256;
      ClientSendMessageToServer( SendMessage);
      Result := True;
    end;

    SendMessage.Free;
  end;
end;

procedure TG2USB.ServerProcessClientMessage( ClientMessage : TClientSendMessage);
var ResponseMessage : TG2ResponseMessage;
    MessageList : TList;
begin
  // If it's a message the server can answer, then send the responsemessage here
  // only to the client that asked for it, else send it through to the G2.

  ResponseMessage := CreateResponseMessage( ClientMessage.FMessage, ClientMessage.FClientContext.FInitialized);

  if ResponseMessage <> nil then begin
    try
      ResponseMessage.CalcCRC;
      ClientMessage.FClientContext.ServerSendClientResponseMessage(ResponseMessage);
    finally
      ResponseMessage.Free;
      ClientMessage.FMessage.Free;
      ClientMessage.Free;
      ClientMessage := nil;
    end;
  end else begin
    if USBActive then begin
      // Online

      if ClientMessage.FMessage.HasResponse then begin
        MessageList := FSendMessageQueue.LockList;
        try
          // Add message to send queue for G2
          ClientMessage.FMessage.Position := 0;
          MessageList.Add( ClientMessage);
          FSendMessageCount := MessageList.Count;
        finally
          FSendMessageQueue.UnlockList;
        end;
      end else begin
        // Put in parameter update table
        try
          ClientMessage.FMessage.Position := 0;
          ServerProcessClientResponselessMessage( ClientMessage);
        finally
          ClientMessage.FMessage.Free;
          ClientMessage.Free;
          ClientMessage := nil;
        end;
      end;

    end else begin
      // Offline
      try
        USBProcessSendMessage( ClientMessage);
      finally
        ClientMessage.FMessage.Free;
        ClientMessage.Free;
        ClientMessage := nil;
      end;
    end;
  end;
end;

function TG2USB.ServerProcessClientResponselessMessage( ClientMessage : TClientSendMessage): boolean;
var Cmd, SubCmd, aVariation, aLocation, aModuleIndex, aParameterIndex,
    aValue, b, bh, bl : byte;
    Size : integer;
begin
  Result := False;

  if ( ClientMessage.FMessage.Size - ClientMessage.FMessage.Position) < 6 then begin
    ClientMessage.FMessage.Position := ClientMessage.FMessage.Size;
    exit;
  end;

  // Read size
  ClientMessage.FMessage.Read( bh, 1);
  ClientMessage.FMessage.Read( bl, 1);
  Size := bh * 256 + bl;

  ClientMessage.FMessage.Read( b, 1); // $01
  ClientMessage.FMessage.Read( Cmd, 1);

  case (Cmd and $0f)  of
  CMD_SYS : exit;
         else
            begin
              ClientMessage.FMessage.Read( b, 1); // Version
              ClientMessage.FMessage.Read( SubCmd, 1);
              case SubCmd of
              S_SET_PARAM :
                    begin
                      ClientMessage.FMessage.Read( aLocation, 1);
                      ClientMessage.FMessage.Read( aModuleIndex, 1);
                      ClientMessage.FMessage.Read( aParameterIndex, 1);
                      ClientMessage.FMessage.Read( aValue, 1);
                      ClientMessage.FMessage.Read( aVariation, 1);
                      ClientMessage.FMessage.Position := ClientMessage.FMessage.Size;
                      case Cmd and $0f of
                      $08 : GetSlot(0).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $09 : GetSlot(1).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $0a : GetSlot(2).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      $0b : GetSlot(3).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation, ClientMessage.FClientContext);
                      end;
                      Result := True;
                    end;
              end;
            end;
  end;
end;

procedure TG2USB.SendOnClientMessage( SendMessage : TMemoryStream);
begin
  PStaticByteBuffer(SendMessage.Memory)^[0] := SendMessage.Size div 256;
  PStaticByteBuffer(SendMessage.Memory)^[1] := SendMessage.Size mod 256;
  ClientSendMessageToServer( SendMessage);
end;

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
var do_next_step : boolean;
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
    6 : if NextBankListCmd.PatchFileType <> pftEnd then begin
          SendListMessage( NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
          do_next_step := false;
        end else begin
          SendStartStopCommunicationMessage( START_COMM);
          OnNextInitStep := nil;
          FInitialized := True;
          if assigned(FOnAfterG2Init) then
            FOnAfterG2Init(self);
        end;
    end;
    if do_next_step then
      inc(FInitStep);
  except on E:Exception do begin
      FInitialized := False;
      OnNextInitStep := nil;
    end;
  end;
end;

procedure TG2USB.NextListMessage(Sender: TObject);
begin
  if NextBankListCmd.PatchFileType <> pftEnd then begin
    SendListMessage( NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
  end else
    OnNextInitStep := nil;
end;

procedure TG2USB.NextBankUploadMessage(Sender: TObject);
var BankItem : TBankItem;
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
  SendListMessage( NextBankListCmd.PatchFileType, NextBankListCmd.Bank, NextBankListCmd.Patch, FslBanks);
  OnNextInitStep := NextListMessage;
end;

procedure TG2USB.USBSentStartComm(Sender: TObject);
begin
  // Usually the last step
  SendStartStopCommunicationMessage( START_COMM);
end;

procedure TG2USB.SendInitMessage;
begin
  SendCmdMessage( CreateInitMessage);
end;

procedure TG2USB.SetPerfMode(aValue: TBits1);
begin
  inherited;

  SendSetModeMessage( aValue); // 0:Patch mode 1:Performance mode
  (Performance as TG2USBPerformance).USBStartInit( True);
end;

procedure TG2USB.SendStartStopCommunicationMessage( Stop : byte);
begin
  SendCmdMessage( CreateStartStopCommunicationMessage(Stop));
end;

procedure TG2USB.SendGetMasterClockMessage;
begin
  SendCmdMessage( CreateGetMasterClockMessage);
end;

procedure TG2USB.SendGetPatchVersionMessage;
begin
  SendCmdMessage( CreateGetPatchVersionMessage);
end;

procedure TG2USB.SendGetAssignedVoicesMessage;
begin
  SendCmdMessage( CreateGetAssignedVoicesMessage);
end;

procedure TG2USB.SendGetSynthSettingsMessage;
begin
  SendCmdMessage( CreateGetSynthSettingsMessage);
end;

procedure TG2USB.SendSetSynthSettingsMessage;
begin
  SendCmdMessage( CreateSetSynthSettingsMessage);
end;

procedure TG2USB.SendUnknown1Message;
begin
  SendCmdMessage( CreateUnknown1Message);
end;

procedure TG2USB.SendUploadBankMessage(aPatchFileType: TPatchFileType; aBank,
  aLocation: byte);
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
  SendCmdMessage( CreateUploadBankMessage( aPatchFileType, aBank, aLocation));
end;

procedure TG2USB.NextPatchBankDownloadMessage( Sender: TObject);
var OriginalBank, OriginalLocation : byte;
    PatchFileName : string;
begin
  inc(BankDumpListIndex);
  if BankDumpListIndex < BankDumpList.Count then begin
    OnNextInitStep := NextPatchBankDownloadMessage;
    if ParseBankDumpListLine( BankDumpList[BankDumpListIndex], OriginalBank, OriginalLocation, PatchFileName) then begin
      SendDownLoadPatchBankMessage( BankDumpDestBank, BankDumpListIndex - 1, BankDumpFolder + PatchFileName);
    end else begin
      OnNextInitStep := nil;
      raise Exception.Create('Error reading patch dump file.');
    end
  end else begin
    OnNextInitStep := nil;
    if assigned(OnAfterBankDownload) then
      OnAfterBankDownload( Self, ID, pftPatch);
  end;
end;

procedure TG2USB.SendDownloadPatchBankMessage( aBank, aLocation: byte; aFileName : string);
var Patch: TG2FilePatch;
    PatchName: string;
    FileStream : TFileStream;
begin
  Patch := TG2FilePatch.Create(nil);
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    try
      Patch.LoadFromFile( FileStream, LogLines);
      PatchName := PatchNameFromFileName( aFileName);
      SendCmdMessage( CreateDownloadPatchBankMessage( aBank, aLocation, PatchName, Patch));
    except on E:Exception do begin
        add_log_line('Error loading patch ' + PatchName + ', patch download aborted.', LOGCMD_ERR);
        OnNextInitStep := nil;
      end;
    end;
  finally
    FileStream.Free;
    Patch.Free;
  end;
end;

procedure TG2USB.NextPerfBankDownloadMessage( Sender: TObject);
var OriginalBank, OriginalLocation : byte;
    PatchFileName : string;
begin
  inc(BankDumpListIndex);
  if BankDumpListIndex < BankDumpList.Count then begin
    OnNextInitStep := NextPerfBankDownloadMessage;
    if ParseBankDumpListLine( BankDumpList[BankDumpListIndex], OriginalBank, OriginalLocation, PatchFileName) then begin
      SendDownLoadPerfBankMessage( BankDumpDestBank, BankDumpListIndex - 1, BankDumpFolder + PatchFileName);
    end else begin
      OnNextInitStep := nil;
      raise Exception.Create('Error reading patch dump file.');
    end
  end else begin
    OnNextInitStep := nil;

    if assigned(OnAfterBankDownload) then
      OnAfterBankDownload( Self, ID, pftPerf);
  end;
end;

procedure TG2USB.SendDownloadPerfBankMessage( aBank, aLocation: byte; aFileName : string);
var Perf: TG2FilePerformance;
    PerfName: string;
    FileStream : TFileStream;
begin
  Perf := TG2FilePerformance.Create(nil);
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    try
      Perf.LoadFromFile( FileStream, LogLines);
      PerfName := PatchNameFromFileName( aFileName);
      SendCmdMessage( CreateDownloadPerfBankMessage( aBank, aLocation, PerfName, Perf));
    except on E:Exception do begin
        add_log_line('Error loading patch ' + PerfName + ', patch download aborted.', LOGCMD_ERR);
        OnNextInitStep := nil;
      end;
    end;
  finally
    FileStream.Free;
    Perf.Free;
  end;
end;

procedure TG2USB.SendDumpMidiMessage;
begin
  SendCmdMessage( CreateMidiDumpMessage);
end;

procedure TG2USB.SendListMessage( aPatchFileType : TPatchFileType; aBank, aPatch : byte; names : TStrings);
begin
  SendCmdMessage( CreateListMessage( aPatchFileType, aBank, aPatch, Names));
end;

procedure TG2USB.SendSetModeMessage( aMode : byte);
begin
  SendCmdMessage( CreateSetModeMessage( aMode));
end;

procedure TG2USB.SendNoteMessage( aNote : byte; aOnoff : byte);
begin
  SendCmdMessage( CreateNoteMessage( aNote, aOnOff));
end;

procedure TG2USB.SendRetrieveMessage( aSlot, aBank, aPatch : byte);
begin
  if assigned(OnBeforePatchUpdate) then
    OnBeforePatchUpdate(Self, ID, aSlot);

  SendCmdMessage( CreateRetrieveMessage( aSlot, aBank, aPatch));
end;

procedure TG2USB.SendStoreMessage( aSlot, aBank, aPatch : byte);
begin
  SendCmdMessage( CreateStoreMessage( aSlot, aBank, aPatch));
end;

procedure TG2USB.SendClearBankMessage(aPatchFileType : TPatchFileType; aBank, aFromLocation, aToLocation: byte);
begin
  SendCmdMessage( CreateClearBankMessage( aPatchFileType, aBank, aFromLocation, aToLocation));
end;

procedure TG2USB.SendClearMessage( aPatchFileType : TPatchFileType; aBank, aPatch : byte);
begin
  SendCmdMessage( CreateClearMessage( aPatchFileType, aBank, aPatch));
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBPerformance
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBPerformance.Create( AOwner: TComponent);
begin
  inherited;

  FPerfVersion := 0;
  FOnNextInitStep := nil;
end;

destructor TG2USBPerformance.Destroy;
begin
  inherited;
end;

function TG2USBPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2USBSlot.Create( self);
end;

function TG2USBPerformance.GetInitializing: boolean;
begin
  Result := assigned(FOnNextInitStep);
end;

function TG2USBPerformance.GetSlot( aSlot : byte): TG2USBSlot;
begin
  Result := Slot[aSlot] as TG2USBSlot;
end;

function TG2USBPerformance.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBPerformance.USBStartInit( aStartCommAfterInit : boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPerfSettingsMessage;
end;

procedure TG2USBPerformance.USBInitSeq(Sender: TObject);
begin
  case FInitStep of
   0 : SendGetPerfSettingsMessage;
   1 : SendUnknown2Message;
   2 : GetSlot(0).USBStartInit( False);
   3 : GetSlot(1).USBStartInit( False);
   4 : GetSlot(2).USBStartInit( False);
   5 : GetSlot(3).USBStartInit( False);
   6 : (G2 as TG2USB).SendGetAssignedVoicesMessage;
   7 : (G2 as TG2USB).SendGetMasterClockMessage;
   8 : begin
         SendGetGlobalKnobsMessage;
         if not FStartCommAfterInit then begin
           OnNextInitStep := nil;
           if assigned((G2 as TG2USB).FOnAfterPerfInit) then begin
             (G2 as TG2USB).FOnAfterPerfInit(self);
           end;
         end;
       end;
   9 : begin
         (G2 as TG2USB).SendStartStopCommunicationMessage( START_COMM);
         OnNextInitStep := nil;
         if assigned((G2 as TG2USB).FOnAfterPerfInit) then
           (G2 as TG2USB).FOnAfterPerfInit(self);
       end;
  end;
  inc(FInitStep);
end;

procedure TG2USBPerformance.USBStartInitPerf;
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
end;

procedure TG2USBPerformance.SendGetPerfSettingsMessage;
begin
  SendCmdMessage( CreateGetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendUnknown2Message;
begin
  SendCmdMessage( CreateUnknown2Message);
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

procedure TG2USBPerformance.SetSelectedSlotIndex(aValue: TBits2);
begin
  inherited;
  if (G2 as TG2USB).IsServer then begin
    // Only when server, so that clients can have another slot selected
    SendSelectSlotMessage( aValue);
  end;
end;

procedure TG2USBPerformance.SendSelectSlotMessage( aSlot: byte);
begin
  SendCmdMessage( CreateSelectSlotMessage( aSlot));
end;

procedure TG2USBPerformance.SendSetPerformanceMessage( aPerfName : string; aPerf : TG2FilePerformance);
begin
  if assigned(G2) and assigned((G2 as TG2Mess).OnBeforePatchUpdate) then
    (G2 as TG2Mess).OnBeforePatchUpdate(Self, G2.ID, 4);

  SendCmdMessage( CreateSetPerformanceMessage( aPerfName, aPerf));
end;

procedure TG2USBPerformance.SendSetPerfSettingsMessage;
begin
  SendCmdMessage( CreateSetPerfSettingsMessage);
end;

procedure TG2USBPerformance.SendSetMasterClockBPMMessage;
begin
  SendCmdMessage( CreateSetMasterClockBPMMessage( BPM));
end;

procedure TG2USBPerformance.SendSetMasterClockRunMessage;
begin
  SendCmdMessage( CreateSetMasterClockRunMessage( Start));
end;

procedure TG2USBPerformance.SendSetPerfNameMessage( aPerfName : string);
begin
  SendCmdMessage( CreateSetPerfNameMessage( aPerfName));
end;

procedure TG2USBPerformance.SendGetGlobalKnobsMessage;
begin
  SendCmdMessage( CreateGetGlobalKnobsMessage);
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBSlot
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBSlot.Create( AOwner: TComponent);
begin
  inherited;

  SetLength( FParamUpdBuf, 100);
  FParamUpdBufCount := 0;
end;

destructor TG2USBSlot.Destroy;
begin
  Finalize( FParamUpdBuf);

  inherited;
end;

function TG2USBSlot.CreatePatch: TG2FilePatch;
begin
  Result := TG2USBPatch.Create( self);
end;

function TG2USBSlot.GetInitializing: boolean;
begin
  Result := assigned(FOnNextInitStep);
end;

function TG2USBSlot.GetPatch : TG2USBPatch;
begin
  if not assigned(Patch) then
    raise Exception.Create('Patch in slot unassigned!');

  Result := Patch as TG2USBPatch;
end;

function TG2USBSlot.GetPerformance : TG2USBPerformance;
begin
  if not assigned(Performance) then
    raise Exception.Create('Performance not assigned to slot.');

  Result := Performance as TG2USBPerformance;
end;


procedure TG2USBSlot.USBStartInit( aStartCommAfterInit : boolean);
begin
  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FOnNextInitStep := USBInitSeq;
  SendGetPatchVersionMessage;
end;

procedure TG2USBSlot.USBInitSeq(Sender: TObject);
begin
  case FInitStep of
   0 : SendGetPatchVersionMessage;
   1 : SendGetPatchMessage;
   2 : SendGetPatchNameMessage;
   3 : SendCurrentNoteMessage;
   4 : SendPatchNotesMessage;
   5 : SendResourceTableMessage( LOCATION_VA);
   6 : SendResourceTableMessage( LOCATION_FX);
   7 : SendUnknown6Message;
   8 : begin
         SendGetSelectedParameterMessage;
         if not FStartCommAfterInit then begin
           OnNextInitStep := nil;
           if assigned(G2) and assigned((G2 as TG2USB).FOnAfterSlotInit) then
             (G2 as TG2USB).FOnAfterSlotInit(self);
         end;
       end;
   9 : begin
         (G2 as TG2USB).SendStartStopCommunicationMessage( START_COMM);
         OnNextInitStep := nil;
         if assigned(G2) and assigned((G2 as TG2USB).FOnAfterSlotInit) then
           (G2 as TG2USB).FOnAfterSlotInit(self);
       end;
  end;
  inc(FInitStep);
end;

function TG2USBSlot.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBSlot.SendGetPatchVersionMessage;
begin
  SendCmdMessage( CreateGetPatchVersionMessage);
end;

procedure TG2USBSlot.SendPatchNotesMessage;
begin
  SendCmdMessage( CreatePatchNotesMessage);
end;

procedure TG2USBSlot.SendControllerSnapshotMessage;
begin
  SendCmdMessage( CreateSendControllerSnapshotMessage);
end;

procedure TG2USBSlot.SendResourceTableMessage( aLocation : Byte);
begin
  SendCmdMessage( CreateResourceTableMessage( aLocation));
end;

procedure TG2USBSlot.SendGetPatchNameMessage;
begin
  SendCmdMessage( CreateGetPatchNameMessage);
end;

procedure TG2USBSlot.SendCurrentNoteMessage;
begin
  SendCmdMessage( CreateCurrentNoteMessage);
end;

procedure TG2USBSlot.SendUnknown6Message;
begin
  SendCmdMessage( CreateUnknown6Message);
end;

procedure TG2USBSlot.SendGetSelectedParameterMessage;
begin
  SendCmdMessage( CreateGetSelectedParameterMessage);
end;

procedure TG2USBSlot.SendSetPatchMessage( aPatchName : string; aPatch : TG2FilePatch);
begin
  if assigned(G2) and assigned((G2 as TG2Mess).OnBeforePatchUpdate) then
    (G2 as TG2Mess).OnBeforePatchUpdate(Self, G2.ID, SlotIndex);

  SendCmdMessage( CreateSetPatchMessage( aPatchName, aPatch));
end;

procedure TG2USBSlot.SendSetPatchName( aPatchName : string);
begin
  SendCmdMessage( CreateSetPatchName( aPatchName));
end;

procedure TG2USBSlot.SendGetParamNamesMessage(aLocation: byte);
begin
  SendCmdMessage( CreateGetParamNamesMessage(aLocation));
end;

procedure TG2USBSlot.SendGetParamsMessage(aLocation: byte);
begin
  SendCmdMessage( CreateGetParamsMessage(aLocation));
end;

procedure TG2USBSlot.SendGetPatchMessage;
begin
  SendCmdMessage( CreateGetPatchMessage);
end;

procedure TG2USBSlot.SendSelectVariationMessage( aVariationIndex: byte);
begin
  SendCmdMessage( CreateSelectVariationMessage( aVariationIndex));
end;

procedure TG2USBSlot.AddParamUpdRec( aSubCmd, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation : byte; aClientContext : TClientContext);
var i : integer;
    SendStream : TG2SendMessage;
begin
  try
    if not assigned(G2) then
      exit;

    // Todo : replace by fast search and add sorted
    i := 0;
    while (i < FParamUpdBufCount) and not(( FParamUpdBuf[i].SubCmd = aSubCmd)
                                      and ( FParamUpdBuf[i].Location = aLocation)
                                      and ( FParamUpdBuf[i].Module = aModule)
                                      and ( FParamUpdBuf[i].Param = aParam)
                                      and ( FParamUpdBuf[i].Morph = aMorph)
                                      and ( FParamUpdBuf[i].Variation = aVariation)
                                      {and ( FParamUpdBuf[i].Sender = aSender)}
                                      and ( FParamUpdBuf[i].ClientContext = aClientContext)) do
      inc(i);

    if not(i < FParamUpdBufCount) then begin
      // Not found, add
      if (FParamUpdBufCount + 1) >= Length( FParamUpdBuf) then
        SetLength( FParamUpdBuf, Length( FParamUpdBuf) + 100); // Increase buffersize

      FParamUpdBuf[ FParamUpdBufCount].SubCmd := aSubCmd;
      FParamUpdBuf[ FParamUpdBufCount].Location := aLocation;
      FParamUpdBuf[ FParamUpdBufCount].Module := aModule;
      FParamUpdBuf[ FParamUpdBufCount].Param := aParam;
      FParamUpdBuf[ FParamUpdBufCount].Morph := aMorph;
      FParamUpdBuf[ FParamUpdBufCount].Value := 128; // Clavia max is 127, so this means "unitialized"
      FParamUpdBuf[ FParamUpdBufCount].Negative := 0;
      FParamUpdBuf[ FParamUpdBufCount].Variation := aVariation;
      //FParamUpdBuf[ FParamUpdBufCount].Sender := aSender;
      FParamUpdBuf[ FParamUpdBufCount].ClientContext := aClientContext;
      inc( FParamUpdBufCount);
    end;

    if ((G2 as TG2USB).FIsServer) then begin
      // Server, mark what has to be updated to the G2
      if (FParamUpdBuf[ i].Value <> aValue) or (FParamUpdBuf[ i].Negative <> aNegative) then begin
        FParamUpdBuf[ i].Value := aValue;
        FParamUpdBuf[ i].Negative := aNegative;
        FParamUpdBuf[ i].Changed := True;
      end;
    end else begin
      // Client, send only if value is changed
      if (FParamUpdBuf[ i].Value <> aValue) or (FParamUpdBuf[ i].Negative <> aNegative) then begin
        FParamUpdBuf[ i].Value := aValue;
        FParamUpdBuf[ i].Negative := aNegative;

        case aSubCmd of
          S_SEL_PARAM :
            begin
              SendStream := CreateSelParamMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param);
              try
                (G2 as TG2USB).SendOnClientMessage( SendStream);
              finally
                SendStream.Free;
              end;
            end;
          S_SET_PARAM :
            begin
              SendStream := CreateSetParamMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param, FParamUpdBuf[ i].Value, FParamUpdBuf[ i].Variation);
              try
                (G2 as TG2USB).SendOnClientMessage( SendStream);
              finally
                SendStream.Free;
              end;
            end;
          S_SET_MORPH_RANGE :
            begin
              SendStream := CreateSetMorphMessage( FParamUpdBuf[ i].Location, FParamUpdBuf[ i].Module, FParamUpdBuf[ i].Param, FParamUpdBuf[ i].Morph, FParamUpdBuf[ i].Value, FParamUpdBuf[ i].Negative, FParamUpdBuf[ i].Variation);
              try
                (G2 as TG2USB).SendOnClientMessage( SendStream);
              finally
                SendStream.Free;
              end;
            end;
        end;

      end;
    end;
  except on E:Exception do begin
      G2.add_log_line( 'AddParamUpdRec, FParamUpdBufCount ' + IntToStr(FParamUpdBufCount) + ', Length buf ' + IntToStr(Length(FParamUpdBuf)), LOGCMD_NUL);
      G2.save_log;
    end;
  end;
end;

procedure TG2USBSlot.SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte);
begin
  AddParamUpdRec( S_SET_PARAM, aLocation, aModule, aParam, 0, aValue, 0, aVariation, nil);
end;

procedure TG2USBSlot.SendSelParamMessage( aLocation, aModule, aParam: integer);
begin
  AddParamUpdRec( S_SEL_PARAM, aLocation, aModule, aParam, 0, 0, 0, 0, nil);
end;

procedure TG2USBSlot.SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
begin
  AddParamUpdRec( S_SET_MORPH_RANGE, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation, nil);
end;

procedure TG2USBSlot.SendSetModeMessage( aLocation, aModule, aParam, aValue: integer);
begin
  SendCmdMessage( CreateSetModeMessage( aLocation, aModule, aParam, aValue));
end;

procedure TG2USBSlot.SendCopyVariationMessage( aFromVariation, aToVariation : byte);
begin
  SendCmdMessage( CreateCopyVariationMessage( aFromVariation, aToVariation));
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2USBPatch
////////////////////////////////////////////////////////////////////////////////

constructor TG2USBPatch.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2USBPatch.Destroy;
begin
  inherited;
end;

function TG2USBPatch.GetPerformance : TG2USBPerformance;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := (Slot as TG2USBSlot).GetPerformance;
end;

function TG2USBPatch.SendCmdMessage( SendMessage : TG2SendMessage): boolean;
begin
  if assigned(G2) then
    Result := (G2 as TG2USB).SendCmdMessage( SendMessage)
  else
    Result := False;
end;

procedure TG2USBPatch.SendUndoMessage;
var MemStream : TG2SendMessage;
begin
  if FUndoStack.Count > 0 then begin
    // Send over usb
    MemStream := PopUndoStack;
    (G2 as TG2USB).SendCmdMessage( MemStream);
  end;
end;

procedure TG2USBPatch.SetModeValue(aLocation: TLocationType; aModuleIndex,
  aParameterIndex, aValue: byte);
begin
  (Slot as TG2USBSlot).SendSetModeMessage( ord(aLocation), aModuleIndex, aParameterIndex, aValue);
end;

procedure TG2USBPatch.SetMorphValue(aLocation: TLocationType; aModuleIndex,
  aParameterIndex, aMorphIndex, aValue, aVariation: byte);
begin
  if aValue >= 128 then
    (Slot as TG2USBSlot).SendSetMorphMessage( ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, abs(aValue - 256), 1, aVariation)
  else
    (Slot as TG2USBSlot).SendSetMorphMessage( ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, aValue, 0, aVariation);
  inherited SetMorphValue( aLocation, aModuleIndex, aParameterIndex, aMorphIndex, aValue, aVariation);
end;

procedure TG2USBPatch.SetParamValue( aLocation: TLocationType; aModuleIndex, aParameterIndex, aVariation : byte; aValue: byte);
begin
  (Slot as TG2USBSlot).SendSetParamMessage( ord(aLocation), aModuleIndex, aParameterIndex, aValue, aVariation);
  inherited SetParamValue( aLocation, aModuleIndex, aParameterIndex, aVariation, aValue);
end;

procedure TG2USBPatch.SetVoiceCount(aValue: byte);
var FPatchDescription : TPatchDescription;
begin
  if aValue > 32 then
    exit;

  FPatchDescription := PatchDescription;
  if aValue > 0 then begin
    FPatchDescription.MonoPoly := 0; // Poly
    FPatchDescription.VoiceCount := aValue;
  end else begin
    if FPatchDescription.MonoPoly = 0 then
      FPatchDescription.MonoPoly := 1; // Mono
    FPatchDescription.VoiceCount := aValue;
  end;
  MessSetPatchDescription( FPatchDescription);
end;

procedure TG2USBPatch.SetVoiceMode(aValue: byte);
var FPatchDescription : TPatchDescription;
begin
  if aValue > 2 then
    exit;

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

function TG2USBPatch.MessSetPatchDescription( FPatchDescription : TPatchDescription): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetPatchDescriptionMessage( FPatchDescription);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetPatchNotes( aLines: TStrings): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetPatchNotesMessage( aLines);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAddModule( aLocation : TLocationType; aModuleTypeID,
  aCol, aRow: byte): boolean;
var MemStream : TG2SendMessage;
    aModuleIndex : Byte;
begin
  inherited MessAddModule( aLocation, aModuleTypeID, aCol, aRow);

  // Get a new module index
  aModuleIndex := GetMaxModuleIndex( aLocation) + 1;

  // Send over usb
  MemStream := CreateAddNewModuleMessage( aLocation, aModuleIndex, aModuleTypeID, aCol, ARow);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessCopyModules( aSrcePatch : TG2FilePatchPart; aFromLocation, aToLocation : TLocationType): boolean;
var MemStream : TG2SendMessage;
begin
  inherited MessCopyModules( aSrcePatch, aFromLocation, aToLocation);

  MemStream := CreateCopyModulesMessage( aSrcePatch, aFromLocation, aToLocation, True);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessCopyParameters(aSrcePatch: TG2FilePatchPart;
  aToLocation: TLocationType; aFromVariation, aToVariation: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateCopyParametersMessage( aSrcePatch, aToLocation, aFromVariation, aToVariation);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessModuleAssignGlobalKnobs(aModule: TG2FileModule; aPageIndex: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateModuleAssignGlobalKnobs( aModule, aPageIndex);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessModuleAssignKnobs( aModule: TG2FileModule; aPageIndex: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateModuleAssignKnobs( aModule, aPageIndex);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessMoveModules( aLocation : TLocationType): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateMoveModulesMessage( aLocation);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteModule( aLocation : TLocationType; aModuleIndex : byte): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteModuleMessage( aLocation, aModuleIndex);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeleteModules( aLocation : TLocationType): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteModulesMessage( aLocation);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleColor( aLocation: TLocationType; aModuleIndex, aColor : byte): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleColorMessage( aLocation, aModuleIndex, aColor);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAddConnection( aLocation : TLocationType; aFromConnector, aToConnector : TG2FileConnector): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAddConnectionMessage( aFromConnector, aToConnector);
  if assigned(MemStream) then
    Result := SendCmdMessage( MemStream)
  else
    Result := False;
end;

function TG2USBPatch.MessDeleteConnection( aLocation : TLocationType; aCable: TG2FileCable): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateDeleteConnectionMessage( aCable);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignKnob(aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignKnobMessage( aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignKnob(aKnob: integer): boolean;
var MemStream : TG2SendMessage;
    Knob : TKnob;
begin
  Knob := GetKnob( aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');

  MemStream := CreateDeassignKnobMessage( aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignMidiCC(aLocation: TLocationType; aModule, aParam, aMidiCC: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignMidiCCMessage( aLocation, aModule, aParam, aMidiCC);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignMidiCC( aMidiCC: integer): boolean;
var MemStream : TG2SendMessage;
    Controller : TController;
begin
  Controller := GetMidiCC( aMidiCC);
  if not assigned(Controller) then
    raise Exception.Create('Controller ' + IntToHex(aMidiCC,2) + ' not found.');

  MemStream := CreateDeassignMidiCCMessage( aMidiCC);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessAssignGlobalKnob( aLocation : TLocationType; aModule, aParam, aKnob: integer): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateAssignGlobalKnobMessage( aLocation, aModule, aParam, aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessDeassignGlobalKnob(aKnob: integer): boolean;
var MemStream : TG2SendMessage;
    Knob : TGlobalKnob;
    Perf : TG2USBPerformance;
begin
  Perf := GetPerformance;
  if not assigned(Perf) then
    raise Exception.Create('Performance not assigned to patch.');

  Knob := Perf.GetGlobalKnob( aKnob);
  if not assigned(Knob) then
    raise Exception.Create('Knob ' + IntToStr(aKnob) + ' not found.');

  MemStream := CreateDeassignGlobalKnobMessage( aKnob);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleParamLabels( aLocation: TLocationType; aModuleIndex, aParamIndex, aLabelIndex: byte; aName: string): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleParamLabelsMessage( aLocation, aModuleIndex, aParamIndex, aLabelIndex, aName);
  Result := SendCmdMessage( MemStream);
end;

function TG2USBPatch.MessSetModuleLabel( aLocation: TLocationType; aModuleIndex: byte; aName: string): boolean;
var MemStream : TG2SendMessage;
begin
  MemStream := CreateSetModuleLabelMessage( aLocation, aModuleIndex, aName);
  Result := SendCmdMessage( MemStream);
end;

end.
