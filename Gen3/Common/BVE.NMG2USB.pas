unit BVE.NMG2USB;

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
  System.SyncObjs,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  BVE.NMG2FileIntf,
  BVE.NMG2Types,
  BVE.NMG2Classes;

const
  LIBUSB_ERROR_TIMEOUT = -116;
  LIBUSB_ERROR_OTHER = -99;
  MAX_READ_BUFFER = 65536; // tcpip buffer size, long enough to hold the longest message from the G2

type
  TG2USB = class;

  // Thread for sending and receiving USB messages
  TListeningThread = class(TThread)
  private
    [Weak] FUSB: TG2USB;
    FRecieveMessageQueue: TThreadList<TG2ResponseMessage>;
    FRecieveMessageCount: integer;
    FInMessBuf: TG2ResponseMessage;

    FRecieveAlive,
    FTotalRecieved,
    FTotalRecievedInterrupt: integer;

    FLogMessage: string;
    FBuffer: pointer;
    FMaxSize: integer;
  private
    function IsExtended(buf_in: TByteBuffer): Boolean;
    function IsEmbedded(buf_in: TByteBuffer): Boolean;
    function ExtendedMessage(var iin_buf: TByteBuffer;
      var err_msg: string): integer;
    function EmbeddedMessage(var iin_buf: TByteBuffer): integer;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; aUSB: TG2USB);
    destructor Destroy; override;
    procedure QueueMessage;
    procedure ProcessMessageQueued;
    procedure WriteLog;
    procedure DumpMessage;
  end;

  // Record def. for table sending responseless messages
  TParamUpdKey = record
    SlotIndex: byte;
    SubCmd: byte;
    LocationIndex: byte;
    ModuleIndex: byte;
    ParamIndex: byte;
    MorphIndex: byte;
    Variation: byte;
  end;

  TParamUpdValue = record
    PatchVersion: byte;
    Value: byte;
    Negative: byte;
    Changed: Boolean;
  end;

  // Thread for sending responseless (parameter) messages
  TSendParamThread = class(TThread)
  private
    [Weak] FUSB: TG2USB;
    FParamUpdList: TDictionary<TParamUpdKey, TParamUpdValue>;
    FLock : TCriticalSection;
    FParamEvent : TEvent;

    FSendAlive,
    FTotalSend: integer;
    FLogMessage: string;
    FBuffer: pointer;
    FMaxSize: integer;
  private
     procedure Lock;
     procedure Unlock;
     procedure SendTable;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean; aUSB: TG2USB);
    destructor Destroy; override;

    procedure AddParamUpdRec(const aSlot, aVersion, aSubCmd, aLocation,
      aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
    procedure WriteLog;
    procedure DumpMessage;
  end;

  TG2USB = class(TInterfacedObject, IG2Client)
  private
    [Weak] FWConnection: IG2Connection;

    FErrorMessage: boolean;
    FErrorMessageNo: integer;

    FSendMessageQueue : TThreadList<TG2SendMessage>;
    FSendMessageCount : integer;

    FListeningThread: TListeningThread;
    FSendParamThread: TSendParamThread;

    procedure DoResponseMessageQueued;
    procedure ProcessSendMessage(aSendMsg: TG2SendMessage);
    procedure ProcessResponseMessage(aResponseMsg: TG2ResponseMessage);
  protected
    procedure USBInit; virtual; abstract;
    procedure USBDone; virtual; abstract;

    function InterruptRead(var aBuffer; const aSize: longword): integer; virtual; abstract;
    function BulkRead(var aBuffer; const aSize: longword): integer; virtual; abstract;
    function BulkWrite(var aBuffer; const aSize: longword): integer; virtual; abstract;

    function GetConnected: boolean; virtual; abstract;
    function GetError: string; virtual; abstract;

    procedure AddLog(const aLogLine: string; const aLogCmd : integer);
    procedure DumpBuffer(var buffer; max_size : integer);
  public
    constructor Create(aConnection: IG2Connection);
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;

    procedure SendMsg(aMsg: TG2SendMessage); virtual;
    procedure AddParamUpdRec(const aSlot, aVersion, aSubCmd, aLocation, aModule,
      aParam, aMorph, aValue, aNegative, aVariation: byte);

    property Connected: boolean read GetConnected;
  end;

implementation

// ------------------------------------------------------------------------------
//
// TSendThread
// Send messages to USB interface
//
// This is done in a seperate thread to better control the speed messages
// are send, because Android crashes when too many are send at once
//
// ------------------------------------------------------------------------------

procedure TSendParamThread.AddParamUpdRec(const aSlot, aVersion, aSubCmd,
  aLocation, aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
var Key: TParamUpdKey;
    Value: TParamUpdValue;
begin
  Lock;
  try
    Key.SlotIndex := aSlot;
    Key.LocationIndex := aLocation;
    Key.SubCmd := aSubCmd;
    Key.ModuleIndex := aModule;
    Key.ParamIndex := aParam;
    Key.MorphIndex := aMorph;
    Key.Variation := aVariation;

    if not FParamUpdList.TryGetValue(Key, Value) then begin
      Value.PatchVersion := aVersion;
      Value.Value := aValue;
      Value.Negative := aNegative;
      Value.Changed := True;

      FParamUpdList.Add(Key, Value);
    end else begin
      Value.PatchVersion := aVersion;
      Value.Value := aValue;
      Value.Negative := aNegative;
      Value.Changed := True;

      FParamUpdList[Key] := Value;
    end;
  finally
    Unlock;
  end;
  FParamEvent.SetEvent;
end;

constructor TSendParamThread.Create(CreateSuspended: Boolean;
  aUSB: TG2USB);
begin
  FUSB := aUSB;

  FParamUpdList := TDictionary<TParamUpdKey, TParamUpdValue>.Create;
  FLock := TCriticalSection.Create;
  FParamEvent := TEvent.Create(nil, False, False, '');
  FParamEvent.ResetEvent;

  FreeOnTerminate := False;

  inherited Create(CreateSuspended);
end;

destructor TSendParamThread.Destroy;
begin
  FParamEvent.Free;
  FLock.Free;
  FParamUpdList.Free;

  inherited;
end;

procedure TSendParamThread.Execute;
var
  Error: Boolean;
begin
  Error := False;
  FSendAlive := 0;
  FTotalSend := 0;

{$IF Defined(MACOS)}
  Priority := 1;
{$ENDIF}
  FLogMessage := 'Send param thread startet.';
  synchronize(WriteLog);
  repeat
    // Waiting for a response? No, send a new command
    FParamEvent.ResetEvent;
    FParamEvent.WaitFor;

    if not Terminated then begin

      SendTable;
{$IF Defined(Android)}
      sleep(50);
{$ELSE}
      sleep(1);
{$ENDIF}
      inc(FSendAlive);

      if Error then
        Terminate;
    end;

  until Terminated;
  FLogMessage := 'Send param thread terminated.';
  synchronize(WriteLog);
end;

procedure TSendParamThread.Lock;
begin
  FLock.Enter;
end;

procedure TSendParamThread.Unlock;
begin
  FLock.Leave;
end;

procedure TSendParamThread.SendTable;
var
  i: integer;
  Size: byte;
  Crc: word;
  b: byte;
  Pair : TPair<TParamUpdKey, TParamUpdValue>;
  Buf: array[0..20] of byte;
begin
  // Send all responseless messages with this function
  Lock;
  try
    for Pair in FParamUpdList do
    begin
      case Pair.Key.SubCmd of
        S_SET_PARAM:
          begin
            Size := 13;
            Buf[0] := 0;
            Buf[1] := Size;
            Buf[2] := $01;
            Buf[3] := CMD_NO_RESP + CMD_SLOT + Pair.Key.SlotIndex;
            Buf[4] := Pair.Value.PatchVersion;
            Buf[5] := S_SET_PARAM;
            Buf[6] := Pair.Key.Locationindex;
            Buf[7] := Pair.Key.ModuleIndex;
            Buf[8] := Pair.Key.ParamIndex;
            Buf[9] := Pair.Value.Value;
            Buf[10] := Pair.Key.Variation;
          end;
        S_SEL_PARAM:
          begin
            Size := 12;
            Buf[0] := 0;
            Buf[1] := Size;
            Buf[2] := $01;
            Buf[3] := CMD_NO_RESP + CMD_SLOT + Pair.Key.Slotindex;
            Buf[4] := Pair.Value.PatchVersion;
            Buf[5] := S_SEL_PARAM;
            Buf[6] := 00;
            Buf[7] := Pair.Key.LocationIndex;
            Buf[8] := Pair.Key.ModuleIndex;
            Buf[9] := Pair.Key.ParamIndex;
          end;
        S_SET_MORPH_RANGE:
          begin
            Size := 15;
            Buf[0] := 0;
            Buf[1] := Size;
            Buf[2] := $01;
            Buf[3] := CMD_NO_RESP + CMD_SLOT + Pair.Key.SlotIndex;
            Buf[4] := Pair.Value.PatchVersion;
            Buf[5] := S_SET_MORPH_RANGE;
            Buf[6] := Pair.Key.LocationIndex;
            Buf[7] := Pair.Key.ModuleIndex;
            Buf[8] := Pair.Key.ParamIndex;
            Buf[9] := Pair.Key.MorphIndex;
            Buf[10] := Pair.Value.Value;
            Buf[11] := Pair.Value.Negative;
            Buf[12] := Pair.Key.Variation;
          end;
        else
          Size := 0;
      end;

      if Size > 3 then
      begin
        // Calc CRC
        Crc := 0;
        for i := 2 to Size - 3 do
        begin
          b := Buf[i];
          Crc := CrcClavia(Crc, b);
        end;

        // Write CRC
        b := Crc div 256;
        Buf[Size - 2] := b;
        b := Crc mod 256;
        Buf[Size - 1] := b;

        FUSB.BulkWrite(Buf[0], Size);

        inc(FTotalSend);
      end;
    end;

    FParamUpdList.Clear;
  finally
    Unlock;
  end;
end;

procedure TSendParamThread.WriteLog;
begin
  FUSB.AddLog(FLogMessage, LOGCMD_NUL);
end;

procedure TSendParamThread.DumpMessage;
begin
  FUSB.DumpBuffer(FBuffer^, FMaxSize);
end;

// ------------------------------------------------------------------------------
//
// TReceiveMessageThread
//
// ------------------------------------------------------------------------------

constructor TListeningThread.Create(CreateSuspended: Boolean;
  aUSB: TG2USB);
begin
  FUSB := aUSB;

  FInMessBuf := TG2ResponseMessage.Create;

  FRecieveMessageQueue := TThreadList<TG2ResponseMessage>.Create;
  FRecieveMessageCount := 0;

  FreeOnTerminate := False;
  inherited Create(CreateSuspended);

{$IFDEF MSWINDOWS}
  Priority := tpHigher;
{$ENDIF}
end;

destructor TListeningThread.Destroy;
var i : integer;
    RecieveMessageList: TList<TG2ResponseMessage>;
begin
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

  FInMessBuf.DisposeOf;

  inherited;
end;

procedure TListeningThread.Execute;
var
  iin_buf: TByteBuffer;
  bytes_read: integer;
  Error: Boolean;
begin
  SetLength(iin_buf, 16);

  Error := False;
  FRecieveAlive := 0;
  FTotalRecieved := 0;
  FTotalRecievedInterrupt := 0;
{$IF Defined(MACOS)}
  Priority := 4;
{$ENDIF}
  FLogMessage := 'Listening thread startet.';
  synchronize(WriteLog);
  repeat
    // Is USB Active?
    if FUSB.Connected then
    begin

      // Read the interrupt message
      // Time out must be long enough, otherwise we lose contact with the G2!
      // TIME_OUT = 0 : wait forever
      bytes_read := FUSB.InterruptRead(iin_buf[0], 16);
      inc(FTotalRecievedInterrupt);

      if bytes_read <> 16 then
      begin
        FLogMessage := 'USB interrupt recieved ' + IntToStr(bytes_read) +
          ' in stead of 16!';
        Synchronize(WriteLog);
      end;

      if bytes_read > 0 then
      begin
        // FLogMessage := 'Usb interupt message recieved ' + IntToStr(bytes_read);
        // Synchronize( WriteLog);

        if IsExtended(iin_buf) then
        begin
          ExtendedMessage(iin_buf, FLogMessage);
          if FLogMessage <> '' then
            Synchronize(WriteLog);
          QueueMessage;
        end
        else if IsEmbedded(iin_buf) then
        begin
          EmbeddedMessage(iin_buf);
          QueueMessage;
        end
        else
        begin
          FLogMessage :=
            'RecThread: Message received is not embedded nor extended.';
          Synchronize(WriteLog);
          Error := True;
        end;
      end
      else
      begin
        if bytes_read < 0 then
        begin
          FLogMessage := 'RecThread: Interrupt read error ' +
            IntToStr(bytes_read);
          Synchronize(WriteLog);
          case bytes_read of
            LIBUSB_ERROR_TIMEOUT:
              begin
                // could not happen with infinite timeout
              end;
            LIBUSB_ERROR_OTHER:
              begin
                // On reset device, exit;
                Terminate;
              end;
          else
            begin
              Error := True;
              FLogMessage := 'RecThread: Interrupt read error ' +
                IntToStr(bytes_read);
              Synchronize(WriteLog);
              // FBuffer := FConnection.g_bout_buf.Memory;
              // FMaxSize :=  FConnection.g_bout_buf.Size;
              // Synchronize( DumpMessage);
            end;
          end;
        end;
      end;
    end
    else
    begin
      FLogMessage := 'USB not active...';
      Synchronize(WriteLog);
      Terminate;
    end;

    if Error then
      Terminate;

    inc(FRecieveAlive);
  until Terminated;

  FLogMessage := 'USB recieve thread terminated.';
  Synchronize(WriteLog);
end;

function TListeningThread.EmbeddedMessage(var iin_buf: TByteBuffer): integer;
var
  i, dil: integer;
  ecrc, acrc: word;
begin
  // Read an embedded G2 message, interrupt message contains length and data

  FInMessBuf.Position := 0;
  FInMessBuf.size := Length(iin_buf);
  Move(iin_buf[0], FInMessBuf.Memory^, Length(iin_buf));

  dil := PStaticByteBuffer(FInMessBuf.Memory)^[0] shr 4;
  ecrc := 0;
  for i := 1 to dil - 2 do
    ecrc := CrcClavia(ecrc, PStaticByteBuffer(FInMessBuf.Memory)^[i]);

  acrc := PStaticByteBuffer(FInMessBuf.Memory)^[dil - 0] // actual crc
    + PStaticByteBuffer(FInMessBuf.Memory)^[dil - 1] * 256;

  if ecrc <> acrc then
    FUSB.AddLog('Bad crc exp: ' + IntToHex(ecrc, 2) + ' act: ' + IntToHex(acrc,
      2), LOGCMD_ERR);

  Result := FInMessBuf.size;
end;

function TListeningThread.ExtendedMessage(var iin_buf: TByteBuffer;
  var err_msg: string): integer;
var
  i, retries, bytes_read, buf_position: integer;
  ecrc, acrc: word;
  size: word;
begin
  // Read an G2 extended message from USB, interrupt messages contains length,
  // bulk message contains the data

  Result := -1;
  err_msg := '';

  size := (iin_buf[1] shl 8) or iin_buf[2];

  FInMessBuf.Position := 0;
  FInMessBuf.size := size;

  retries := 5; // the message has to return within 5 tries
  bytes_read := 0;
  buf_position := 0;
  while (retries > 0) and (buf_position < size) { (size <> bytes_read) } and
    (bytes_read >= 0) do
  begin
    bytes_read := FUSB.BulkRead(PStaticByteBuffer(FInMessBuf.Memory)^[buf_position],
      size);

    if bytes_read = LIBUSB_ERROR_TIMEOUT then
    begin
      bytes_read := 0; // time out
      err_msg := err_msg + 'Bulk read timeout! ';
    end
    else
    begin
      if bytes_read < 0 then
      begin
        err_msg := err_msg + 'Bulk read error ' + IntToStr(bytes_read) + ' ';
      end
      else
      begin
        // if size <> bytes_read then
        // err_msg := err_msg + 'bulk read ' + IntToStr(bytes_read)+ '/' + IntToStr(size) + ' ';
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
    exit;
  end;

  if buf_position <> size then
  begin
    err_msg := err_msg + 'bulk read size mismatch, read ' +
      IntToStr(buf_position) + ' of ' + IntToStr(size) + ' ';
  end
  else
  begin

    ecrc := 0; // expected crc
    for i := 0 to size - 2 - 1 do
      ecrc := CrcClavia(ecrc, PStaticByteBuffer(FInMessBuf.Memory)^[i]);

    acrc := PStaticByteBuffer(FInMessBuf.Memory)^[size - 1] // actual crc
      + PStaticByteBuffer(FInMessBuf.Memory)^[size - 2] * 256;

    if ecrc <> acrc then
      err_msg := err_msg + 'Bad crc exp: ' + IntToHex(ecrc, 2) + ' act: ' +
        IntToHex(acrc, 2) + ' ';
  end;

  if bytes_read < 0 then
    Result := bytes_read
  else
    Result := buf_position;
end;

function TListeningThread.IsEmbedded(buf_in: TByteBuffer): Boolean;
begin
  Result := buf_in[0] and $F = 2;
end;

function TListeningThread.IsExtended(buf_in: TByteBuffer): Boolean;
begin
  Result := buf_in[0] and $F = 1;
end;

procedure TListeningThread.ProcessMessageQueued;
begin
  FUSB.DoResponseMessageQueued;
end;

procedure TListeningThread.QueueMessage;
var RecieveMessageList: TList<TG2ResponseMessage>;
    Mess: TG2ResponseMessage;
begin
  RecieveMessageList := FRecieveMessageQueue.LockList;
  try
    Mess := TG2ResponseMessage.Create;
    Mess.Assign(FInMessBuf);
    RecieveMessageList.Add(Mess);
    FRecieveMessageCount := RecieveMessageList.Count;
  finally
    FRecieveMessageQueue.UnlockList;
  end;
  Queue(ProcessMessageQueued); // Messages need to be queued also...
end;

procedure TListeningThread.WriteLog;
begin
  FUSB.AddLog(FLogMessage, LOGCMD_NUL);
end;

procedure TListeningThread.DumpMessage;
begin
  FUSB.DumpBuffer(FBuffer^, FMaxSize);
end;

{ TG2USB }

procedure TG2USB.AddLog(const aLogLine: string; const aLogCmd: integer);
begin
  FWConnection.AddLog(aLogLine, aLogCmd);
end;

constructor TG2USB.Create(aConnection: IG2Connection);
begin
  inherited Create;

  SetWeak(@FWConnection, aConnection);

  FSendMessageQueue := TThreadList<TG2SendMessage>.Create;
  FSendMessageCount := 0;
end;

destructor TG2USB.Destroy;
var i : integer;
    SendMessageList : TList<TG2SendMessage>;
begin
// Free the send message queue
  SendMessageList := FSendMessageQueue.LockList;
  try
    for i := 0 to SendMessageList.Count - 1 do begin
      SendMessageList[i].DisposeOf;
    end;
    FSendMessageCount := SendMessageList.Count;
  finally
    FSendMessageQueue.UnlockList;
  end;
  FSendMessageQueue.DisposeOf;

  SetWeak(@FWConnection, nil);

  inherited;
end;

procedure TG2USB.Connect;
begin
  AddLog('Initializing connection.', LOGCMD_NUL);

{$IF Defined(MSWINDOWS) or Defined(MACOS)}
  if Connected then
    USBDone;
{$ENDIF}
  AddLog('initializing usb.', LOGCMD_NUL);

  USBInit;

  AddLog('starting message threads.', LOGCMD_NUL);
  FListeningThread := TListeningThread.Create(False, self);
  FSendParamThread := TSendParamThread.Create(False, self);
  sleep(1000);
end;

procedure TG2USB.Disconnect;
begin
  AddLog('Closing connection.', LOGCMD_NUL);

  // Disconnect
  if Connected then
  begin
    try
      if assigned(FSendParamThread) then
      begin
        AddLog('Stop send message thread.', LOGCMD_NUL);
        FSendParamThread.Terminate;
        FSendParamThread.FParamEvent.SetEvent;
        FreeAndNil(FSendParamThread);
      end;
      if assigned(FListeningThread) then
      begin
        AddLog('Stop receive message thread.', LOGCMD_NUL);
        FListeningThread.Terminate;
        // Close usb connection
        USBDone;
        FreeAndNil(FListeningThread);
      end;
    except
      on E: Exception do
        AddLog(E.Message, LOGCMD_ERR);
    end;
  end;
end;

procedure TG2USB.DoResponseMessageQueued;
var
  Cmd, SendMsgCmd: byte;
  SendMsg: TG2SendMessage;
  RecieveMessageList: TList<TG2ResponseMessage>;
  ResponseMessage: TG2ResponseMessage;
  SendMessageList: TList<TG2SendMessage>;
begin
  // Process a response message received over USB

  if not assigned(FListeningThread) then
    exit;

  RecieveMessageList := FListeningThread.FRecieveMessageQueue.LockList;
  try
    if RecieveMessageList.Count = 0 then
      exit;

    ResponseMessage := RecieveMessageList[0];
    RecieveMessageList.Delete(0);

    FListeningThread.FRecieveMessageCount := RecieveMessageList.Count;
  finally
    FListeningThread.FRecieveMessageQueue.UnlockList;
  end;

  try
    Cmd := ResponseMessage.Command;

    FErrorMessage := False;

    // Led data, volume data...
    if ResponseMessage.IsLedData then
    begin
      ProcessResponseMessage(ResponseMessage);
      exit;
    end
    else
    begin
      // Are we waiting for a response?
      if (FSendMessageCount > 0) then
      begin

        SendMessageList := FSendMessageQueue.LockList;
        try
          SendMsg := SendMessageList[0];
          SendMsgCmd := SendMsg.Command;

          // Is this the responsemessage we have been waiting for

          if (Cmd = SendMsgCmd) or ((Cmd = $04) and (SendMsgCmd = $0C)) or FErrorMessage
          then
          begin
            SendMessageList.Delete(0);
            FSendMessageCount := SendMessageList.Count;
            try
              ProcessResponseMessage(ResponseMessage);

              if not FErrorMessage then
                ProcessSendMessage(SendMsg);
            finally
              SendMsg.DisposeOf;
            end;
          end
          else
          begin
            AddLog(DateTimeToStr(Now) +
              ' Message received out of sync (1). Size = ' +
              IntToStr(ResponseMessage.size) + ' ' + MiniDump(ResponseMessage,
              ResponseMessage.size), LOGCMD_ERR);
          end;

        finally
          FSendMessageQueue.UnlockList;
        end;

      end
      else
      begin
        // Resource messages are received here...
        ProcessResponseMessage(ResponseMessage);
      end;
    end;
  finally
    ResponseMessage.DisposeOf;
  end;
end;

procedure TG2USB.ProcessResponseMessage(
  aResponseMsg: TG2ResponseMessage);
var Size : byte;
begin
  if aResponseMsg.IsEmbedded then begin
    aResponseMsg.Position := 1; // Skip first byte if embedded
    Size := PStaticByteBuffer(aResponseMsg.Memory)^[0] shr 4;
    aResponseMsg.Size := Size;
  end else
    aResponseMsg.Position := 0;

  FWConnection.DoProcessResponseMsg( aResponseMsg);
end;

procedure TG2USB.ProcessSendMessage(aSendMsg: TG2SendMessage);
begin
  // Responseless messages that originate from the server are already processed in patch
  if (aSendMsg.HasResponse) then begin
    aSendMsg.Position := 0;

    FWConnection.DoProcessSendMsg(aSendMsg);
  end;
end;

procedure TG2USB.DumpBuffer(var buffer; max_size: integer);
begin
  FWConnection.DumpBuffer(buffer, max_size);
end;

procedure TG2USB.SendMsg(aMsg: TG2SendMessage);
var SendMessageList : TList<TG2SendMessage>;
    SendMsg: TG2SendMessage;
begin
  FErrorMessage := False;
  FErrorMessageNo := 0;

  // Online?
  if Connected then
  begin
    SendMsg := TG2SendMessage.Create;
    SendMsg.Assign(aMsg);
    SendMsg.PrepareForSend;

    SendMessageList := FSendMessageQueue.LockList;
    try
      SendMessageList.Add(SendMsg);
      FSendMessageCount := SendMessageList.Count;
      BulkWrite(SendMsg.Memory^, SendMsg.size);
    finally
      FSendMessageQueue.UnlockList;
    end;
  end
  else
  begin
    // Not online
    ProcessSendMessage(aMsg);
  end;
end;

procedure TG2USB.AddParamUpdRec(const aSlot, aVersion, aSubCmd, aLocation,
  aModule, aParam, aMorph, aValue, aNegative, aVariation: byte);
begin
  FSendParamThread.AddParamUpdRec(aSlot, aVersion, aSubcmd, aLocation, aModule,
    aParam, aMorph, aValue, aNegative, aVariation);
end;

end.
