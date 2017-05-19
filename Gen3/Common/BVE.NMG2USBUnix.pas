unit BVE.NMG2USBUnix;

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
//  The libusb1.0 library is needed:
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  libusb_dyn,
  BVE.NMG2Types,
  BVE.NMG2FileIntf,
  BVE.NMG2USB;

const
  LIBUSB_ERROR_TIMEOUT = -116;
  LIBUSB_ERROR_OTHER = -99;
  TIME_OUT = 0;     // Timeout wait for G2 interrupt message, must be long enough, otherwise there is the risk of losing contact with the G2

type
  TUSBUnix = class(TG2USB)
  private
    FDev: Plibusb_device;
    FUDev: Plibusb_device_handle;
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
    constructor Create(aConnection: IG2Connection; aUSBDevice: Plibusb_device); override;
    destructor Destroy; override;
  end;

procedure GetUSBDeviceList(aList: TList);

implementation

// ------------------------------------------------------------------------------
//
// Device discovery
//
// ------------------------------------------------------------------------------

procedure GetUSBDeviceList(aList: TList);
type
  DevsArray = array [0 .. 255] of pointer;
  PDevsArray = ^DevsArray;
var
  err: integer;
  devs: PPlibusb_device;
  pdev: Plibusb_device;
  descr: libusb_device_descriptor;
  i, cnt: integer;
begin
  aList.Clear;

  if not assigned(ctx) then
    exit; // libusb dll not loaded

  cnt := libusb_get_device_list(ctx, @devs);
  try
    for i := 0 to cnt - 1 do
    begin
      pdev := Plibusb_device(PDevsArray(devs)^[i]);
      err := libusb_get_device_descriptor(pdev, @descr);
      if err = 0 then
      begin
        if (descr.idVendor = VENDOR_ID) and (descr.idProduct = PRODUCT_ID) then
        begin
          aList.Add(pdev);
        end
        else
          libusb_unref_device(pdev);
      end;
    end;
  finally
    // free the list, don't unref the devices in it
    libusb_free_device_list(devs, 0);
  end;
end;


{ TUSBUnix }

constructor TUSBUnix.Create(aConnection: IG2Connection; aUSBDevice: Plibusb_device);
begin
  inherited Create(aConnection);

  FDev := aUSBDevice;
  FUDev := nil;
end;

destructor TUSBUnix.Destroy;
begin

  inherited;
end;

function TUSBUnix.GetConnected: boolean;
begin
  Result := assigned(FUDev)
end;

function TUSBUnix.GetError: string;
begin
  Result := ''; // Don't know yet
end;

procedure TUSBUnix.USBInit;
var
  err: integer;
  dev: libusb_device;
  devs: PPlibusb_device;
  cnt: integer;
begin
  // Initialization of the USB interface unix

  if not assigned(FDev) then
    exit;

  err := libusb_open(FDev, @FUDev);

  if err = 0 then
  begin
    // kernel driver attaching problem
    if (libusb_kernel_driver_active(FUDev, 0) = 1) then
    // find out if kernel driver is attached
    begin
      AddLog('Kernel Driver Active', LOGCMD_HDR);
      if (libusb_detach_kernel_driver(FUDev, 0) = 0) then // detach it
        AddLog('Kernel Driver Detached!', LOGCMD_HDR);
    end;
    // dev := libusb_get_device(FUDev);

    // get 3 endpoints
    FIin := $81;
    FBin := $82;
    FBout := $03;

    err := libusb_claim_interface(FUDev, 0); // claim usb interface
    if err < 0 then
      AddLog('Error claiming interface ' + IntToStr(err), LOGCMD_HDR);

  end
  else
    raise Exception.Create('Error opening device ' + IntToStr(err));
end;

procedure TUSBUnix.USBDone;
var
  err: integer;
begin
  // Disconnect from the USB interface unix

  // Needed to break out the infinite interupt wait
  libusb_reset_device(FUDev);

  err := libusb_release_interface(FUDev, 0); // release the claimed interface
  if (err <> 0) then
    AddLog('Error releasing interface', LOGCMD_HDR) // result handler
  else
    AddLog('Released Interface', LOGCMD_HDR);

  // G2 is never attached to the kernel, so following not needed
  // if (libusb_attach_kernel_driver(FUDev, 0) = 0) then //attach kernel again it
  // AddLogLine('Kernel Driver Attached!', LOGCMD_HDR);

  libusb_close(FUDev); // close the device we opened
  FUDev := nil;

  libusb_unref_device(FDev);
end;

function TUSBUnix.InterruptRead(var aBuffer; const aSize: longword): integer;
var
  bytes_read: longint;
begin
  // Read an interrupt message from USB unix
  Result := libusb_interrupt_transfer(FUDev, FIin, PChar(@aBuffer), aSize,
    @bytes_read, TIME_OUT);

  if Result < 0 then
    AddLog(GetError, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TUSBUnix.BulkRead(var aBuffer; const aSize: longword): integer;
var
  bytes_read: longint;
begin
  // Read a bulk message from USB unix
  Result := libusb_bulk_transfer(FUDev, FBin, PChar(@aBuffer), aSize,
    @bytes_read, TIME_OUT);

  if Result < 0 then
    AddLog(GetError, LOGCMD_ERR)
  else
    Result := bytes_read;
end;

function TUSBUnix.BulkWrite(var aBuffer; const aSize: longword): integer;
var
  bytes_written: longint;
begin
  // Write a bulk message over USB unix
  try
    Result := libusb_bulk_transfer(FUDev, FBout, PChar(@aBuffer), aSize,
      @bytes_written, TIME_OUT);
  finally
    if Result < 0 then
      AddLog(GetError, LOGCMD_ERR)
    else
      Result := aSize;
  end;

end;



end.
