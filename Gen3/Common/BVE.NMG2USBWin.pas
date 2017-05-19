unit BVE.NMG2USBWin;

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
//  This unit contains the USB interface and the client/server functionality
//
//  The usb interface is based on the libusb-win32 (for windows) and the
//  libusb1.0 library for unix
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  LibUSBWinDyn,
  BVE.NMG2Types,
  BVE.NMG2FileIntf,
  BVE.NMG2USB;

const
  TIME_OUT = 0;     // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2

type
  TUSBWin = class(TG2USB)
  private
    FDev: pusb_device;
    FUDev: pusb_dev_handle;
    FConf: usb_config_descriptor;
    FIntf: usb_interface;
    FEps: PArray_usb_endpoint_descriptor;
    FIin: byte;
    FBin: byte;
    FBout: byte;
  protected
    procedure USBInit; override;
    procedure USBDone; override;

    function InterruptRead(var aBuffer; const aSize: longword): integer; override;
    function BulkRead(var aBuffer; const aSize: longword): integer; override;
    function BulkWrite(var aBuffer; const aSize: longword): integer; override;

    function GetConnected: boolean; override;
    function GetError: string; override;
  public
    constructor Create(aConnection: IG2Connection; aUSBDevice: pusb_device);
    destructor Destroy; override;
  end;

procedure GetUSBDeviceList(aConMan: IG2ConnectionManager);

implementation
uses
  BVE.NMG2ComUSB;

// ------------------------------------------------------------------------------
//
// Device discovery
//
// ------------------------------------------------------------------------------

procedure GetUSBDeviceList(aConMan: IG2ConnectionManager);
var
  bus: pusb_bus;
  dev: pusb_device;
  Connection: IG2Connection;
  USBWin: TUSBWin;
begin
  if not assigned(usb_init) then
    exit; // libusb dll not loaded

  // LibUSB-Win32 Initialization
  usb_init; // Initialize libusb
  usb_find_busses; // Finds all USB busses on system
  usb_find_devices; // Find all devices on all USB devices
  bus := usb_get_busses; // Return the list of USB busses found

  while assigned(bus) do
  begin
    dev := bus^.devices;
    while assigned(dev) do
    begin
      if (dev^.descriptor.idVendor = VENDOR_ID) and
        (dev^.descriptor.idProduct = PRODUCT_ID) then begin

        Connection := TG2USBConnection.Create(aConMan);
        USBWin := TUSBWin.Create(Connection, dev);
        Connection.Client := USBWin;

        aConMan.ConnectionList.Add(Connection);

      end;
      dev := dev^.next;
    end;
    bus := bus^.next;
  end;
  // usb_set_debug(255);
end;

{ TUSBWin }

constructor TUSBWin.Create(aConnection: IG2Connection; aUSBDevice: pusb_device);
begin
  inherited Create(aConnection);

  FDev := aUSBDevice;
  FUDev := nil;
end;

destructor TUSBWin.Destroy;
begin
  inherited;
end;

function TUSBWin.GetConnected: boolean;
begin
  Result := assigned(FUDev)
end;

function TUSBWin.GetError: string;
begin
  Result := ''; // Don't know yet
end;

procedure TUSBWin.USBInit;
var
  version: pusb_version;
begin
  // Initialization of the USB interface windows
  try
    FUDev := nil;

    if not assigned(FDev) then
      exit;

    AddLog('USB Get endpoints', LOGCMD_NUL);

    // get 3 endpoints
    FConf := FDev^.config[0];
    FIntf := FConf.iinterface[0];
    FEps := FIntf.altsetting[0].endpoint;

    FIin := FEps[0].bEndpointAddress;
    FBin := FEps[1].bEndpointAddress;
    FBout := FEps[2].bEndpointAddress;

    AddLog('g2iin = ' + IntToStr(FIin) + ', g2bin = ' + IntToStr(FBin) +
      ', g2bout = ' + IntToStr(FBout), LOGCMD_NUL);

    AddLog('Opening USB Device', LOGCMD_NUL);
    FUDev := usb_open(FDev);
    if not assigned(FUDev) then
      raise Exception.Create('Unable to open device.');

    version := usb_get_version;
    AddLog('LibUSB-win32 driver version : ' +
      IntToStr(version^.drivermajor) + '.' + IntToStr(version.driverminor) + '.'
      + IntToStr(version.drivermicro) + '.' + IntToStr(version.drivernano),
      LOGCMD_NUL);

    AddLog('Set USB Configuration', LOGCMD_NUL);
    if usb_set_configuration(FUDev, FConf.bConfigurationValue) < 0 then
      raise Exception.Create('Unable to set configuration.');

    AddLog('Claim USB Interface', LOGCMD_NUL);
    if usb_claim_interface(FUDev, 0) < 0 then
      raise Exception.Create('Unable to claim the interface.');

  except
    on E: Exception do
    begin
      AddLog(E.Message, LOGCMD_ERR);
      raise;
    end;
  end;
end;

procedure TUSBWin.USBDone;
begin
  // Disconnect from the USB interface windows

  if assigned(FUDev) then
  begin
    // Needed to break out the infinite interupt wait
    AddLog('Reset device', LOGCMD_NUL);
    usb_reset(FUDev);

    AddLog('Release USB Interface', LOGCMD_NUL);
    usb_release_interface(FUDev, 0);

    AddLog('Closing USB Device', LOGCMD_NUL);
    usb_close(FUDev);

    FUDev := nil;
  end;
end;

function TUSBWin.InterruptRead(var aBuffer; const aSize: longword): integer;
begin
  // Read an interrupt message from USB windows
  Result := usb_interrupt_read(FUDev, FIin, aBuffer, aSize, TIME_OUT);
end;

function TUSBWin.BulkRead(var aBuffer; const aSize: longword): integer;
begin
  // Read a bulk message over USB windows
  Result := usb_bulk_read(FUDev, FBin, aBuffer, aSize, TIME_OUT);
end;

function TUSBWin.BulkWrite(var aBuffer; const aSize: longword): integer;
begin
  // Write a bulk message over USB windows
  Result := usb_bulk_write(FUDev, FBout, aBuffer, aSize, TIME_OUT);
end;


end.
