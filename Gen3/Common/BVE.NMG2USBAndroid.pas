unit BVE.NMG2USBAndroid;

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
  System.Classes,
  System.Generics.Collections,
  Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.USB,
  BVE.NMG2Types,
  BVE.NMG2FileIntf,
  BVE.NMG2USB;

const
  TIME_OUT = 0;     // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2


type
  TUSBAndroid = class(TG2USB)
  private
    FUsbManager: JUSBManager;
    FUsbDevice: JUSBDevice;
    FUsbInterface: JUSBInterface;
    FUsbEPII, FUsbEPBI, FUsbEPBO: JUSBEndPoint;
    FUsbDeviceConnection: JUSBDeviceConnection;
    FIIBuffer, FBIBuffer: TJavaArray<byte>;
    FBOBuffer: TJavaArray<byte>;
  protected
    procedure USBInit; override;
    procedure USBDone; override;

    function InterruptRead(var aBuffer; const aSize: longword): integer; override;
    function BulkRead(var aBuffer; const aSize: longword): integer; override;
    function BulkWrite(var aBuffer; const aSize: longword): integer; override;

    function GetConnected: boolean; override;
    function GetError: string; override;
  public
    constructor Create(aConnection: IG2Connection; aUSBDevice: JUSBDevice; aUSBManager: JUSBManager);
    destructor Destroy; override;
  end;

procedure GetUSBDeviceList(aConMan: IG2ConnectionManager);

implementation
uses
  //FMX.Helpers.Android,
  Androidapi.Helpers,
  FMX.Dialogs,
  BVE.NMG2ComUSB;

procedure GetUSBDeviceList(aConMan: IG2ConnectionManager);
var
  DeviceList : JHashMap;
  Device : JUsbDevice;
  JavaObject : JObject;
  t : Jiterator;
  i : integer;
  Connection: IG2Connection;
  USBAndroid: TUSBAndroid;
  USBManager: JUSBManager;
begin
  JavaObject := SharedActivityContext.getSystemService(TJContext.JavaClass.USB_SERVICE);
  USBManager := TJUSBManager.Wrap((JavaObject as ILocalObject).GetObjectID);

  DeviceList := USBManager.getDeviceList;

  if DeviceList.Size > 0  then begin

    //ShowMessage('Devices: ' + IntToStr(DeviceList.Size));

    t := DeviceList.values.iterator;
    i := 0;
    while t.hasNext do begin
      Device := TJUSBDevice.Wrap((t.next as ILocalObject).GetObjectID);
      if (Device.getVendorId = VENDOR_ID) and (Device.getProductId = PRODUCT_ID) then begin

        Connection := TG2USBConnection.Create(aConMan);

        //ShowMessage('Creating android USB connection...');

        USBAndroid := TUSBAndroid.Create(Connection, Device, USBManager);
        Connection.Client := USBAndroid;

        aConMan.ConnectionList.Add(Connection);

        inc(i);
      end;
    end;
  end;
  //ShowMessage('Connections: ' + IntToStr(aConMan.ConnectionList.Count));
end;

{ TUSBAndroid }

constructor TUSBAndroid.Create(aConnection: IG2Connection; aUSBDevice: JUSBDevice;
  aUSBManager: JUSBManager);
begin
  inherited Create(aConnection);

  FUsbManager := aUSBManager;
  FUsbDevice := aUSBDevice;
  FUsbDeviceConnection := nil;

  FIIBuffer := TJavaArray<byte>.Create(8192);
  FBIBuffer := TJavaArray<byte>.Create(8192);
  FBOBuffer := TJavaArray<byte>.Create(8192);
end;

destructor TUSBAndroid.Destroy;
begin
  FBOBuffer.DisposeOf;
  FIIBuffer.DisposeOf;
  FBIBuffer.DisposeOf;

  inherited;
end;

function TUSBAndroid.GetConnected: boolean;
begin
  Result := assigned(FUsbDeviceConnection)
end;

function TUSBAndroid.GetError: string;
begin
  Result := '';
end;

procedure TUSBAndroid.USBInit;
begin
  //ShowMessage('USBInit...');

  // add_log_line('# Interfaces ' + IntToStr(FUSBDevice.getInterfaceCount), LOGCMD_HDR);

  FUsbInterface := FUsbDevice.getInterface(0);
  // add_log_line('# Endpoints ' + IntToStr(FUsbInterface.getEndpointCount), LOGCMD_HDR);

  FUsbEPII := FUsbInterface.getEndpoint(0);
  // add_log_line('Endpoint ' + IntToStr(FUsbEPII.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBI := FUsbInterface.getEndpoint(1);
  // add_log_line('Endpoint ' + IntToStr(FUsbEPBI.getEndpointNumber), LOGCMD_HDR);

  FUsbEPBO := FUsbInterface.getEndpoint(2);
  // add_log_line('Endpoint ' + IntToStr(FUsbEPBO.getEndpointNumber), LOGCMD_HDR);

  if FUsbManager.hasPermission(FUsbDevice) then
  begin
    AddLog('Permissions o.k.', LOGCMD_HDR);
  end
  else
  begin
    FUsbDevice := nil;
    raise Exception.Create('No permission...');
  end;

  FUsbDeviceConnection := FUsbManager.openDevice(FUsbDevice);
  if not assigned(FUsbDeviceConnection) then
    raise Exception.Create('Failed to open device.');

  if not FUsbDeviceConnection.claimInterface(FUsbInterface, True) then
    raise Exception.Create('Failed to claim interface.');
end;

procedure TUSBAndroid.USBDone;
begin
  if assigned(FUsbDeviceConnection) then
  begin
    if not FUsbDeviceConnection.releaseInterface(FUsbInterface) then
      AddLog('Failed to release the interface', LOGCMD_HDR);
    FUsbDeviceConnection.close;

    FUsbDeviceConnection := nil;

    AddLog('Device is closed', LOGCMD_HDR);
  end;
end;

function TUSBAndroid.InterruptRead(var aBuffer; const aSize: longword): integer;
begin
  // Read an interrupt message from USB unix

  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPII, FIIBuffer,
    aSize, TIME_OUT);
  if Result > 0 then
    Move(FIIBuffer.Data^, aBuffer, Result);
end;

function TUSBAndroid.BulkRead(var aBuffer; const aSize: longword): integer;
begin
  // Read a bulk message from USB unix

  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBI, FBIBuffer,
    aSize, TIME_OUT);
  if Result > 0 then
    Move(FBIBuffer.Data^, aBuffer, Result);
end;

function TUSBAndroid.BulkWrite(var aBuffer; const aSize: longword): integer;
begin
  // Write a bulk message over USB unix
  Move(aBuffer, FBOBuffer.Data^, aSize);
  Result := FUsbDeviceConnection.bulkTransfer(FUsbEPBO, FBOBuffer, aSize, 0);

  if Result < 0 then
    AddLog('Error sending message ' + IntToStr(Result), LOGCMD_ERR);
end;

end.
