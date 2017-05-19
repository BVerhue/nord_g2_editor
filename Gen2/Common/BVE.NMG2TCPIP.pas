unit BVE.NMG2TCPIP;

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
  System.SyncObjs, System.Classes, System.SysUtils, System.Generics.Collections,
  IdCustomTCPServer, idTCPConnection, IdYarn, IdThread, idSync, IdTCPServer,
  idTCPClient, IdContext,
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2Mess, BVE.NMG2USB;

const
  DEFAULT_PORT = 2501;  // The default tcpip port for the communication

type
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
  TClientSendMessage = class;
  TG2TCPIP = class;

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

  // Send message to keep track of sender
  TClientSendMessage = class(TG2SendMessage)
    [Weak] FClientContext  : TClientContext; // nil : server, else a client
    FMessageSender  : TClient;
    procedure Assign( aSource : TG2SendMessage); override;
  end;

  // Listening thread for a client
  TIdClientReadThread = class(TIdThread)
  protected
    [Weak] FG2: TG2TCPIP;
    LMessage : TMemoryStream;
    LProtocol: TProtocol;
    procedure Run; override;
  public
    constructor Create(AG2: TG2TCPIP); reintroduce;
    destructor Destroy; override;
    procedure DoClientProcessServerSendMessage;
    procedure DoClientProcessServerResponseMessage;
  End;

  // Notification from the server listening thread to the main thread
  TG2ProcessClientSendMessage = class( TIdNotify)
  private
    [Weak] FG2 : TG2TCPIP;
    [Weak] FClientContext : TClientContext;

    FBuffer : TMemoryStream;
    FClient : TClient;
  protected
    procedure DoNotify; override;
    destructor Destroy; override;
  end;

  TG2TCPIP = class( TG2USB)
    private
      FProcessLedData : boolean;

      FOnAddClient          : TOnAddClient;
      FOnDeleteClient       : TOnDeleteClient;

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

    protected
      function    GetID: integer; override;
      function    GetUSBActive : boolean; override;
      procedure   SetUSBActive(const value : boolean); override;
    public
      FLogLedDataMessages  : boolean;
      FTCPErrorMessage     : string;

      constructor Create( AOwner: TComponent); override;
      destructor  Destroy; override;

      function    CreatePerformance: TG2FilePerformance; override;

      function    CreateSendMessage: TG2SendMessage; override;

      function    SendCmdMessage( aSendMessage : TG2SendMessage): boolean; override;
      procedure   SendOnClientMessage( SendMessage : TG2SendMessage);

      procedure   USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage); override;
      procedure   USBProcessSendMessage(aSendMessage : TG2SendMessage); override;

      // Client/server communication
      procedure   Lock;
      procedure   Unlock;

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

      property    TCPIPEnabled : boolean read FTCPIPEnabled write FTCPIPEnabled;
      property    IdTCPClient : TIdTCPClient read FIdTCPClient;
    published
      property    IsServer : boolean read FIsServer write FIsServer;
      property    Port : integer read FPort write FPort;
      property    Host : string read FHost write FHost;
      property    ProcessLedData : boolean read FProcessLedData write FProcessLedData;
      property    TimerBroadcastLedMessages : integer read FTimerBroadcastLedMessages write FTimerBroadcastLedMessages default 500;

      property    OnAddClient : TOnAddClient read FOnAddClient write FOnAddClient;
      property    OnDeleteClient : TOnDeleteClient read FOnDeleteClient write FOnDeleteClient;
  end;

  TG2TCPIPPerformance = class( TG2USBPerformance)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SetSelectedSlotIndex( aValue : TBits2); override;

    function    CreateSlot : TG2FileSlot; override;
  end;

  TG2TCPIPSlot = class( TG2USBSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    procedure   SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte); override;
    procedure   SendSelParamMessage( aLocation, aModule, aParam: integer); override;
    procedure   SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte); override;
  end;


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

procedure TClientSendMessage.Assign(aSource: TG2SendMessage);
begin
  inherited;
   FClientContext := (aSource as TCLientSendMessage).FClientContext;
   FMessageSender := (aSource as TCLientSendMessage).FMessageSender;
end;

//==============================================================================
//
//                            TG2TCPIP  (G2 + TCPIP)
//
//==============================================================================

constructor TG2TCPIP.Create( AOwner: TComponent);
begin
  inherited Create( AOwner);

  FClientCriticalSection := TCriticalSection.Create;

  FProcessLedData := False;

  FTCPIPEnabled := True;

  FHost := '127.0.0.1';
  FPort := DEFAULT_PORT;

  FLogLedDataMessages := False;
end;

function TG2TCPIP.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2TCPIPPerformance.Create(self);
end;

destructor TG2TCPIP.Destroy;
begin
  FClientCriticalSection.Free;

  inherited;
end;

function TG2TCPIP.CreateSendMessage: TG2SendMessage;
begin
  Result := TClientSendMessage.Create;
end;

//==============================================================================
//
//                        ACTIVATE/DEACTIVATE COMMUNICATION
//
//==============================================================================

function TG2TCPIP.GetUSBActive: boolean;
begin
  if FIsServer then
    Result := inherited
  else
    if assigned(FIdTCPClient) then
      Result := FIdTCPClient.Connected
    else
      Result := False;
end;

procedure TG2TCPIP.SetUSBActive(const Value: boolean);
var iin_buf : TByteBuffer;
    bytes_read : integer;
    timer_start : Cardinal;
begin
  if Value then begin
    add_log_line( 'Initializing connection.', LOGCMD_NUL);

    // Only a server can have an USB connection
    if FIsServer then begin
      add_log_line( 'Started as server.', LOGCMD_NUL);

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

      inherited;

    end else begin
      // Client connects
      add_log_line( 'Started as client.', LOGCMD_NUL);

      if assigned( FIdTCPServer) then begin
        FIdTCPServer.Active := False;
        FIdTCPServer.Free;
        FIdTCPServer := nil;
      end;

      FIdTCPClient := TIdTCPClient.Create( self);

      FClientID.ID := integer(FIdTCPClient); // set the user ID to some number
      FClientID.ClientType := ClientType;

      FIdTCPClient.Port := FPort;
      FIdTCPClient.Host := FHost;
      FTCPErrorMessage := '';
      try
        add_log_line( 'Connect to server, ip = ' + FHost + ', port = ' + IntToStr(FPort), LOGCMD_NUL);

        FIdTCPClient.Connect; // attempt connection


        if FIdTCPClient.Connected then begin
          // if we are connected, create a listener thread instance
          FIdClientReadThread := TIdClientReadThread.Create(self);

          FIdClientReadThread.Start;

          add_log_line( 'Connect to server ' + FHost + '.', LOGCMD_NUL);
          ClientSendConnectedToServer;

          if assigned( OnUSBActiveChange) then
            OnUSBActiveChange( self, True);

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
    inherited;

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

    if assigned( OnUSBActiveChange) then
      OnUSBActiveChange( self, False);
  end;
end;

procedure TG2TCPIP.USBProcessSendMessage( aSendMessage : TG2SendMessage);
var ClientSendMessage : TClientSendMessage;
begin
  ClientSendMessage := aSendMessage as TClientSendMessage;

  // Responseless messages that originate from the server are already processed in patch
  if not((ClientSendMessage.FClientContext = nil) and not(ClientSendMessage.HasResponse)) then begin
    //inherited;
    aSendMessage.Position := 0;
    ProcessSendMessage( aSendMessage, ID);
  end;

  ServerBroadCastSendMessage( ClientSendMessage);
end;

procedure TG2TCPIP.USBProcessResponseMessage( ResponseMessage : TG2ResponseMessage);
begin
  inherited;

  ServerBroadCastResponseMessage( ResponseMessage);
end;

function TG2TCPIP.SendCmdMessage( aSendMessage : TG2SendMessage): boolean;
var ClientSendMessage : TClientSendMessage;
begin
   // Send message from server to G2 or from client to server
   ClientSendMessage := aSendMessage as TClientSendMessage;

  if FIsServer then begin

    Result := inherited;

  end else begin
    // Client TODO CHECK

    // Return True if message was send
    Result := False;
    ErrorMessage := False;
    ErrorMessageNo := 0;

    add_log_line( '', LOGCMD_NUL);
    add_log_line( 'Send message, size = ' + IntToStr(aSendMessage.Size) , LOGCMD_NUL);
    dump_buffer( aSendMessage.Memory^, aSendMessage.Size);

   //if WaitForCmd <> 0 then begin
   //  aSendMessage.DisposeOf;
   //  aSendMessage := nil;
   //  exit; // Waiting for response, can't send a new command now
   //end;

    // Client, send message to server
    if assigned(FIdTCPClient) and FIdTCPClient.Connected then begin

      WaitforCmd := aSendMessage.Command;

      if assigned(OnBeforeSendMessage) then
        OnBeforeSendMessage( self, ID, ClientSendMessage);

      PStaticByteBuffer( aSendMessage.Memory)^[0] := aSendMessage.Size div 256;
      PStaticByteBuffer( aSendMessage.Memory)^[1] := aSendMessage.Size mod 256;

      if WaitForCmd = 0 then begin
        ClientSendMessageToServer( ClientSendMessage);
        aSendMessage.DisposeOf;
        aSendMessage := nil;
      end else begin
        LastSendMessage := ClientSendMessage;
        ClientSendMessageToServer( ClientSendMessage);
      end;

      Result := True;
    end else begin
      aSendMessage.DisposeOf;
      aSendMessage := nil;
    end;
  end;
end;

//==============================================================================
//
//                 Communication between server and client
//
//==============================================================================

function TG2TCPIP.GetID : integer;
begin
  Result := FClientID.ID;
end;

procedure TG2TCPIP.IdTCPServerConnect( AContext: TIdContext);
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

procedure TG2TCPIP.IdTCPServerDisconnect( AContext: TIdContext);
begin
  add_log_line( 'Client disconnect : ' + AContext.Binding.PeerIP, LOGCMD_NUL);
  if assigned(FOnDeleteClient) then
    FOnDeleteClient(self, 0);
end;

//==============================================================================
//
//                          Client context for server
//
//==============================================================================

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

procedure TClientContext.Unlock;
begin
  if assigned(FClientContextCriticalSection) then
    FClientContextCriticalSection.Leave;
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
  LProtocol.DataSize := ClientSendMessage.Size;

  LBuffer := ProtocolToStream(LProtocol);
  // set the length of the buffer to <size of protocol structure> + <message length>
  LBuffer.Size := szProtocol + LProtocol.DataSize;
  // move message to buffer
  Move( ClientSendMessage.Memory^, PStaticByteBuffer(LBuffer.Memory)^[szProtocol], LProtocol.DataSize);
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

//==============================================================================
//
//                          Server listening thread
//
//==============================================================================

procedure TG2TCPIP.IdTCPServerExecute( AContext: TIdContext);
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
  ClientSendMessage.Size := FBuffer.Size;
  Move( FBuffer.Memory^, ClientSendMessage.Memory^, FBuffer.Size);
  FBuffer.Free;
  FBuffer := nil;

  //ClientSendMessage.Prepared := True; // Already prepared by client

  ClientSendMessage.Position := 0;
  if ClientSendMessage.HasResponse then
    (FG2 as TG2TCPIP).ServerProcessClientMessage( ClientSendMessage)
  else
    (FG2 as TG2TCPIP).ServerProcessClientResponselessMessage( ClientSendMessage);
end;

procedure TG2TCPIP.ServerBroadcastResponseMessage( ResponseMessage : TG2ResponseMessage);
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
           and (TThread.GetTickCount - Cardinal(LClientContext.LastWrite) > Cardinal(FTimerBroadcastLedMessages)) then begin

          LClientContext.Lock;
          try
            LClientContext.LastWrite := TThread.GetTickCount;
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

procedure TG2TCPIP.ServerBroadcastSendMessage( ClientSendMessage : TClientSendMessage);
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
      if not(SenderIsThisClient and not(ClientSendMessage.HasResponse)) then begin
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

function TG2TCPIP.GetClientCount: integer;
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

//==============================================================================
//
//                         Client listening thread
//
//==============================================================================

constructor TIdClientReadThread.Create(AG2: TG2TCPIP);
begin
  inherited Create(True);
  FG2 := AG2;
  FreeOnTerminate := False;
  LMessage := TMemoryStream.Create;
end;

destructor TIdClientReadThread.Destroy;
begin
  LMessage.Free;
  inherited;
end;

procedure TG2TCPIP.Lock;
begin
  FClientCriticalSection.Enter;
end;

procedure TG2TCPIP.Unlock;
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

procedure TG2TCPIP.ClientProcessServerResponseMessage( MemStream : TMemoryStream);
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
  if Cmd = WaitforCmd then begin
    WaitforCmd := 0;

    if assigned(OnReceiveResponseMessage) then
      OnReceiveResponseMessage( Self, MemStream);

    // Still connected?
    if FidTCPClient.Connected then begin

      // Send next message in a sequence, if assigned
      if assigned(GetSlot(0)) and assigned(GetSlot(0).OnNextInitStep)  then
        GetSlot(0).OnNextInitStep(self)
      else

      if assigned(GetSlot(1)) and assigned(GetSlot(1).OnNextInitStep)  then
        GetSlot(1).OnNextInitStep(self)
      else

      if assigned(GetSlot(2)) and assigned(GetSlot(2).OnNextInitStep)  then
        GetSlot(2).OnNextInitStep(self)
      else

      if assigned(GetSlot(3)) and assigned(GetSlot(3).OnNextInitStep)  then
        GetSlot(3).OnNextInitStep(self)
      else

      if assigned(Performance) and assigned(GetPerformance.OnNextInitStep) then
        GetPerformance.OnNextInitStep(self)
      else

      if assigned(OnNextInitStep) then
        OnNextInitStep(self);
    end;
  end;
end;

procedure TG2TCPIP.ClientProcessServerSendMessage( MemStream : TMemoryStream; SenderID : integer);
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

procedure TG2TCPIP.ClientSendConnectedToServer;
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

procedure TG2TCPIP.ClientSendMessageToServer( MemStream : TMemoryStream);
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

procedure TG2TCPIP.ServerProcessClientMessage( ClientMessage : TClientSendMessage);
var ResponseMessage : TG2ResponseMessage;
    MessageList : TList;
begin
  // If it's a message the server can answer, then send the responsemessage here
  // only to the client that asked for it, else send it through to the G2.

  ResponseMessage := CreateResponseMessage( ClientMessage, ClientMessage.FClientContext.FInitialized);

  if ResponseMessage <> nil then begin
    try
      ResponseMessage.CalcCRC;
      ClientMessage.FClientContext.ServerSendClientResponseMessage(ResponseMessage);
    finally
      ResponseMessage.Free;
      ClientMessage.DisposeOf;
    end;
  end else
    SendCmdMessage( ClientMessage);
end;

function TG2TCPIP.ServerProcessClientResponselessMessage( ClientMessage : TClientSendMessage): boolean;
var Cmd, SubCmd, aVariation, aLocation, aModuleIndex, aParameterIndex,
    aValue, aMorph, aNegative, b, bh, bl : byte;
    Size : integer;
begin
  Result := False;
  try
    // ?
    if ( ClientMessage.Size - ClientMessage.Position) < 6 then begin
      ClientMessage.Position := ClientMessage.Size;
      exit;
    end;

    // Read size
    ClientMessage.Read( bh, 1);
    ClientMessage.Read( bl, 1);
    Size := bh * 256 + bl;

    ClientMessage.Read( b, 1); // $01
    ClientMessage.Read( Cmd, 1);

    case (Cmd and $0f)  of
    CMD_SYS : exit;
           else
              begin
                ClientMessage.Read( b, 1); // Version
                ClientMessage.Read( SubCmd, 1);
                case SubCmd of
                S_SET_PARAM :
                      begin
                        ClientMessage.Read( aLocation, 1);
                        ClientMessage.Read( aModuleIndex, 1);
                        ClientMessage.Read( aParameterIndex, 1);
                        ClientMessage.Read( aValue, 1);
                        ClientMessage.Read( aVariation, 1);
                        ClientMessage.Position := ClientMessage.Size;
                        case Cmd and $0f of
                        $08 : (GetSlot(0) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation);
                        $09 : (GetSlot(1) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation);
                        $0a : (GetSlot(2) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation);
                        $0b : (GetSlot(3) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, aValue, 0, aVariation);
                        end;
                        Result := True;
                      end;
                S_SEL_PARAM :
                      begin
                        ClientMessage.Read( b, 1); // Unknown
                        ClientMessage.Read( aLocation, 1);
                        ClientMessage.Read( aModuleIndex, 1);
                        ClientMessage.Read( aParameterIndex, 1);
                        ClientMessage.Position := ClientMessage.Size;
                        case Cmd and $0f of
                        $08 : (GetSlot(0) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, 0, 0, 0);
                        $09 : (GetSlot(1) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, 0, 0, 0);
                        $0a : (GetSlot(2) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, 0, 0, 0);
                        $0b : (GetSlot(3) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, 0, 0, 0, 0);
                        end;
                        Result := True;
                      end;
                S_SET_MORPH_RANGE :
                      begin
                        ClientMessage.Read( aLocation, 1);
                        ClientMessage.Read( aModuleIndex, 1);
                        ClientMessage.Read( aParameterIndex, 1);
                        ClientMessage.Read( aMorph, 1);
                        ClientMessage.Read( aValue, 1);
                        ClientMessage.Read( aNegative, 1);
                        ClientMessage.Read( aVariation, 1);
                        ClientMessage.Position := ClientMessage.Size;
                        case Cmd and $0f of
                        $08 : (GetSlot(0) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, aMorph, aValue, aNegative, aVariation);
                        $09 : (GetSlot(1) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, aMorph, aValue, aNegative, aVariation);
                        $0a : (GetSlot(2) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, aMorph, aValue, aNegative, aVariation);
                        $0b : (GetSlot(3) as TG2TCPIPSlot).AddParamUpdRec( SubCmd, aLocation, aModuleIndex, aParameterIndex, aMorph, aValue, aNegative, aVariation);
                        end;
                        Result := True;
                      end;
                end;

                USBProcessSendMessage(ClientMessage);
              end;
    end;
  finally
    ClientMessage.DisposeOf;
    ClientMessage := nil;
  end;
end;

procedure TG2TCPIP.SendOnClientMessage( SendMessage : TG2SendMessage);
begin
  PStaticByteBuffer(SendMessage.Memory)^[0] := SendMessage.Size div 256;
  PStaticByteBuffer(SendMessage.Memory)^[1] := SendMessage.Size mod 256;
  ClientSendMessageToServer( SendMessage);
end;

// =============================================================================
//
//                            TG2TCPIPPerformance
//
// =============================================================================

constructor TG2TCPIPPerformance.Create(AOwner: TComponent);
begin
  inherited;
end;

function TG2TCPIPPerformance.CreateSlot: TG2FileSlot;
begin
  Result := TG2TCPIPSlot.Create(self);
end;

destructor TG2TCPIPPerformance.Destroy;
begin

  inherited;
end;

procedure TG2TCPIPPerformance.SetSelectedSlotIndex(aValue: TBits2);
begin
  inherited;
  if (G2 as TG2TCPIP).IsServer then begin
    // Only when server, so that clients can have another slot selected
    SendSelectSlotMessage( aValue);
  end;
end;

// =============================================================================
//
//                             TG2TCPIPSlot
//
// =============================================================================

constructor TG2TCPIPSlot.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2TCPIPSlot.Destroy;
begin
  inherited;
end;

procedure TG2TCPIPSlot.SendSetParamMessage( aLocation, aModule, aParam, aValue, aVariation: byte);
var SendStream : TG2SendMessage;
begin
  if AddParamUpdRec( S_SET_PARAM, aLocation, aModule, aParam, 0, aValue, 0, aVariation) then begin
    SendStream := CreateSetParamMessage( aLocation,aModule, aParam, aValue, aVariation);
    try
      if not (G2 as TG2TCPIP).IsServer then
        (G2 as TG2TCPIP).SendOnClientMessage( SendStream)
      else
        (G2 as TG2TCPIP).USBProcessSendMessage( SendStream);
    finally
      SendStream.Free;
    end;
  end;
end;

procedure TG2TCPIPSlot.SendSelParamMessage( aLocation, aModule, aParam: integer);
var SendStream : TG2SendMessage;
begin
  if AddParamUpdRec( S_SEL_PARAM, aLocation, aModule, aParam, 0, 0, 0, 0) then begin
    SendStream := CreateSelParamMessage( aLocation, aModule, aParam);
    try
      if not (G2 as TG2TCPIP).IsServer then
        (G2 as TG2TCPIP).SendOnClientMessage( SendStream)
      else
        (G2 as TG2TCPIP).USBProcessSendMessage( SendStream);
    finally
      SendStream.Free;
    end;
  end;
end;

procedure TG2TCPIPSlot.SendSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
var SendStream : TG2SendMessage;
begin
  if AddParamUpdRec( S_SET_MORPH_RANGE, aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation) then begin
    SendStream := CreateSetMorphMessage( aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation);
    try
      if not (G2 as TG2TCPIP).IsServer then
        (G2 as TG2TCPIP).SendOnClientMessage( SendStream)
      else
        (G2 as TG2TCPIP).USBProcessSendMessage( SendStream);
    finally
      SendStream.Free;
    end;
  end;
end;




end.
