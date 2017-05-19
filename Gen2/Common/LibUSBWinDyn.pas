(* LIBUSB-WIN32, Generic Windows USB Library
 * Copyright (c) 2002-2004 Stephan Meyer <ste_meyer@web.de>
 * Copyright (c) 2000-2004 Johannes Erdfelt <johannes@erdfelt.com>
 *
 * Pascal translation
 * Copyright (c) 2004 Yvo Nelemans <ynlmns@xs4all.nl>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * 17-2-2012 Bve modified it for dynamic loading
 *)


unit LibUSBWinDyn;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

const
  LIBUSB_PATH_MAX = 512;
  LIBUSB_DLL_NAME =  'libusb0.dll';

  USB_OK = 0; // 0 = success from functions < 0 is failed


  { Device and/or Interface Class codes }
  USB_CLASS_PER_INTERFACE   =   0; { for DeviceClass }
  USB_CLASS_AUDIO           =   1;
  USB_CLASS_COMM            =   2;
  USB_CLASS_HID             =   3;
  USB_CLASS_PRINTER         =   7;
  USB_CLASS_MASS_STORAGE    =   8; 
  USB_CLASS_HUB             =   9; 
  USB_CLASS_DATA            =  10; 
  USB_CLASS_VENDOR_SPEC     = $ff; 
 
  // Descriptor types
  USB_DT_DEVICE     = $01; 
  USB_DT_CONFIG     = $02;
  USB_DT_STRING     = $03; 
  USB_DT_INTERFACE  = $04;
  USB_DT_ENDPOINT   = $05; 
 
  USB_DT_HID        = $21; 
  USB_DT_REPORT     = $22; 
  USB_DT_PHYSICAL   = $23; 
  USB_DT_HUB        = $29; 
 
  // Descriptor sizes per descriptor type 
  USB_DT_DEVICE_SIZE          = 18; 
  USB_DT_CONFIG_SIZE          =  9; 
  USB_DT_INTERFACE_SIZE       =  9; 
  USB_DT_ENDPOINT_SIZE        =  7; 
  USB_DT_ENDPOINT_AUDIO_SIZE  =  9; // Audio extension 
  USB_DT_HUB_NONVAR_SIZE      =  7; 
 
 
type 
  // All standard descriptors have these 2 fields in common 
  usb_descriptor_header = packed record 
    bLength, 
    bDescriptorType: byte; 
  end;

  // String descriptor 
  usb_string_descriptor = packed record 
    bLength, 
    bDescriptorType: byte;
    wData: packed array [0..0] of word; 
  end; 
 
  usb_hid_descriptor = packed record 
    bLength, 
    bDescriptorType: byte; 
    bcdHID: word; 
    bCountryCode, 
    bNumDescriptors: byte; 
  end; 
 
const 
  // Endpoint descriptor 
  USB_MAXENDPOINTS = 32; 
 
type 
  // Endpoint descriptor 
  pusb_endpoint_descriptor = ^usb_endpoint_descriptor;
  usb_endpoint_descriptor = packed record 
    bLength, 
    bDescriptorType, 
    bEndpointAddress,
    bmAttributes: byte; 

    wMaxPacketSize: word; 
 
    bInterval, 
    bRefresh, 
    bSynchAddress: byte; 
 
    extra: PByte; // Extra descriptors 
    extralen: longword; 
  end; 
  // pascal translation of C++ struc 
  TArray_usb_endpoint_descriptor =
   packed array [0..65535] of usb_endpoint_descriptor; 
  PArray_usb_endpoint_descriptor = ^TArray_usb_endpoint_descriptor; 
 

const 
  USB_ENDPOINT_ADDRESS_MASK      = $0f; // in bEndpointAddress 
  USB_ENDPOINT_DIR_MASK          = $80; 
 
  USB_ENDPOINT_TYPE_MASK         = $03; // in bmAttributes
  USB_ENDPOINT_TYPE_CONTROL      = 0; 
  USB_ENDPOINT_TYPE_ISOCHRONOUS  = 1; 
  USB_ENDPOINT_TYPE_BULK         = 2; 
  USB_ENDPOINT_TYPE_INTERRUPT    = 3; 
 
  // Interface descriptor 
  USB_MAXINTERFACES = 32; 
 
type 
  // Interface descriptor 
  pusb_interface_descriptor = ^usb_interface_descriptor; 
  usb_interface_descriptor = packed record 
    bLength, 
    bDescriptorType, 
    bInterfaceNumber, 
    bAlternateSetting, 
    bNumEndpoints, 
    bInterfaceClass, 
    bInterfaceSubClass, 
    bInterfaceProtocol, 
    iInterface: byte; 
 
    endpoint: PArray_usb_endpoint_descriptor;
 
    extra: PByte; // Extra descriptors 
    extralen: longword; 
  end; 
  // pascal translation of C++ struc 
  TArray_usb_interface_descriptor =
   packed array [0..65535] of usb_interface_descriptor;
  PArray_usb_interface_descriptor = ^TArray_usb_interface_descriptor;
 
 
const
  USB_MAXALTSETTING = 128; // Hard limit 
 
type
  pusb_interface = ^usb_interface; 
  usb_interface = packed record 
    altsetting: PArray_usb_interface_descriptor; 
    num_altsetting: longword; 
  end; 
  // pascal translation of C++ struc 
  TArray_usb_interface = packed array [0..65535] of usb_interface; 
  PArray_usb_interface = ^TArray_usb_interface; 
 
 
const 
  // Configuration descriptor information..
  USB_MAXCONFIG = 8; 
 
type 
  // Configuration descriptor information.. 
  pusb_config_descriptor = ^usb_config_descriptor; 
  usb_config_descriptor = packed record 
    bLength, 
    bDescriptorType: byte; 
 
    wTotalLength: word; 
 
    bNumInterfaces, 
    bConfigurationValue, 
    iConfiguration, 
    bmAttributes, 
    MaxPower: byte; 
 
    iinterface: PArray_usb_interface; 
 
    extra: PByte; // Extra descriptors
    extralen: longword; 
  end; 
  // pascal translation of C++ struc
  TArray_usb_config_descriptor =
   packed array [0..65535] of usb_config_descriptor; 
  PArray_usb_config_descriptor = ^TArray_usb_config_descriptor; 
 
 
  // Device descriptor 
  usb_device_descriptor = packed record 
    bLength,
    bDescriptorType: byte; 
    bcdUSB: word;
 
    bDeviceClass, 
    bDeviceSubClass, 
    bDeviceProtocol,
    bMaxPacketSize0: byte; 
 
    idVendor, 
    idProduct, 
    bcdDevice: word; 
 
    iManufacturer, 
    iProduct, 
    iSerialNumber, 
    bNumConfigurations: byte; 
  end; 

  usb_ctrl_setup = packed record 
    bRequestType, 
    bRequest: byte; 
    wValue, 
    wIndex, 
    wLength: word; 
  end; 
 
const 
  // Standard requests
  USB_REQ_GET_STATUS         = $00; 
  USB_REQ_CLEAR_FEATURE      = $01; 
  // $02 is reserved 
  USB_REQ_SET_FEATURE        = $03;
  // $04 is reserved 
  USB_REQ_SET_ADDRESS        = $05; 
  USB_REQ_GET_DESCRIPTOR     = $06; 
  USB_REQ_SET_DESCRIPTOR     = $07; 
  USB_REQ_GET_CONFIGURATION  = $08; 
  USB_REQ_SET_CONFIGURATION  = $09; 
  USB_REQ_GET_INTERFACE      = $0A; 
  USB_REQ_SET_INTERFACE      = $0B; 
  USB_REQ_SYNCH_FRAME        = $0C;

  USB_TYPE_STANDARD   = ($00 shl 5); 
  USB_TYPE_CLASS      = ($01 shl 5); 
  USB_TYPE_VENDOR     = ($02 shl 5); 
  USB_TYPE_RESERVED   = ($03 shl 5); 
 
  USB_RECIP_DEVICE     = $00; 
  USB_RECIP_INTERFACE  = $01; 
  USB_RECIP_ENDPOINT   = $02; 
  USB_RECIP_OTHER      = $03;

  // Various libusb API related stuff
  USB_ENDPOINT_IN   = $80; 
  USB_ENDPOINT_OUT  = $00;

  // Error codes 
  USB_ERROR_BEGIN  = 500000; 
 
type 
  pusb_device = ^usb_device; 
  pusb_bus = ^usb_bus; 
 
  usb_device = packed record 
    next, 
    prev: pusb_device; 
    filename: packed array [0..LIBUSB_PATH_MAX-1] of AnsiChar;
    bus: pusb_bus; 
    descriptor: usb_device_descriptor; 
    config: PArray_usb_config_descriptor; 
    dev: pointer; // Darwin support
    devnum, 
     num_children: byte; 
     children : ^pusb_device; 
  end; 
 
  usb_bus = packed record 
    next, 
    prev: pusb_bus; 
    dirname: packed array [0..LIBUSB_PATH_MAX-1] of AnsiChar;
    devices: pusb_device;
    location: longint;
     root_dev: pusb_device;
   end;

  // Version information, Windows specific
  pusb_version = ^usb_version;
  usb_version = packed record
    dllmajor,
    dllminor,
    dllmicro,
    dllnano: longint;
    drivermajor,
    driverminor,
    drivermicro,
    drivernano: longint;
  end;

  pusb_dev_handle = pointer; // struct usb_dev_handle;


{ Function prototypes }

var
  // usb.c
  usb_open : function( dev: pusb_device): pusb_dev_handle; cdecl = nil;
  usb_close : function( dev: pusb_dev_handle): longword; cdecl = nil;
  usb_get_string : function( dev: pusb_dev_handle;index, langid: longword; var buf;buflen: longword): longword; cdecl = nil;
  usb_get_string_simple : function( dev: pusb_dev_handle;index: longword; var buf;buflen: longword): longword; cdecl = nil;

  // descriptors.c
  usb_get_descriptor_by_endpoint : function( udev: pusb_dev_handle;ep: longword;ttype: byte;index: byte;var buf;size: longword): longword; cdecl = nil;
  usb_get_descriptor : function( udev: pusb_dev_handle;ttype: byte;index: byte;var buf;size: longword): longword; cdecl = nil;

// <arch>.c
  usb_bulk_write : function( dev: pusb_dev_handle;ep : longword; var bytes;size,timeout:longword): longword; cdecl = nil;
  usb_bulk_read : function( dev: pusb_dev_handle;ep: longword; var bytes; size,timeout:longword): longword; cdecl = nil;

  usb_interrupt_write : function( dev: pusb_dev_handle;ep : longword; var bytes; size, timeout: longword): longword; cdecl = nil;
  usb_interrupt_read : function( dev: pusb_dev_handle;ep : longword; var bytes; size, timeout: longword): longword; cdecl = nil;
  usb_control_msg : function( dev: pusb_dev_handle;requesttype, request, value, index: longword;var bytes;size, timeout: longword): longword; cdecl = nil;
  usb_set_configuration : function( dev: pusb_dev_handle;configuration: longword): integer; cdecl = nil;
  usb_claim_interface : function( dev: pusb_dev_handle;iinterface: longword): integer;  cdecl = nil; // was interface, a pascal reserved word
  usb_release_interface : function( dev: pusb_dev_handle;iinterface: longword): longword; cdecl = nil;
  usb_set_altinterface : function( dev: pusb_dev_handle;alternate: longword): longword; cdecl = nil;
  usb_resetep : function( dev: pusb_dev_handle;ep: longword): longword; cdecl = nil;
  usb_clear_halt : function( dev: pusb_dev_handle;ep: longword): longword; cdecl = nil;
  usb_reset : function( dev: pusb_dev_handle): longword; cdecl = nil;

  usb_strerror : function : PAnsiChar; cdecl = nil;

  usb_init : procedure; cdecl = nil;
  usb_set_debug : procedure( level: longword); cdecl = nil;
  usb_find_busses : function : longword; cdecl = nil;
  usb_find_devices : function : longword; cdecl = nil;
  usb_get_device : function( dev: pusb_dev_handle): pusb_device; cdecl = nil; // renamed from usb_device because of same named record
  usb_get_busses : function : pusb_bus; cdecl = nil;

  usb_install_service_np : function : integer; cdecl = nil;
  usb_uninstall_service_np : function : integer; cdecl = nil;
  usb_install_driver_np : function( inf_file: PAnsiChar): integer; cdecl = nil;
  usb_get_version : function : pusb_version; cdecl = nil;
  usb_isochronous_setup_async : function( dev: pusb_dev_handle; var context: pointer; ep: PAnsiChar; pktsize: integer): integer; cdecl = nil;
  usb_bulk_setup_async : function( dev: pusb_dev_handle; var context: pointer; ep: PAnsiChar): integer; cdecl = nil;
  usb_interrupt_setup_async : function( dev: pusb_dev_handle; var context: pointer; ep: PAnsiChar): integer; cdecl = nil;
  usb_submit_async : function( context: pointer; bytes: PByte; size: integer): integer; cdecl = nil;
  usb_reap_async : function( context: pointer; timeout: integer): integer; cdecl = nil;
  usb_reap_async_nocancel : function( context: pointer; timeout: integer): integer; cdecl = nil;
  usb_cancel_async : function( context: pointer): integer; cdecl = nil;
  usb_free_async : function( var context: pointer): integer; cdecl = nil;


implementation
uses
  Windows;

var
  LibUSBHandle: Cardinal;


{$IFNDEF G2_VST}
initialization
  LibUSBHandle := LoadLibrary( LIBUSB_DLL_NAME);
  if LibUSBHandle <= 32 then
    MessageBox(0, LIBUSB_DLL_NAME + ' not found, USB functions are disabled',  '', MB_OK)
  else
  begin
    //Add := GetProcAddress(eBob42Handle, 'Add');
    //AddFloat := GetProcAddress(eBob42Handle, 'AddFloat')
    usb_open := GetProcAddress( LibUSBHandle, 'usb_open');
    usb_close := GetProcAddress( LibUSBHandle, 'usb_close');
    usb_get_string := GetProcAddress( LibUSBHandle, 'usb_get_string');
    usb_get_string_simple := GetProcAddress( LibUSBHandle, 'usb_get_string_simple');

    usb_get_descriptor_by_endpoint := GetProcAddress( LibUSBHandle, 'usb_get_descriptor_by_endpoint');
    usb_get_descriptor := GetProcAddress( LibUSBHandle, 'usb_get_descriptor');

    usb_bulk_write := GetProcAddress( LibUSBHandle, 'usb_bulk_write');
    usb_bulk_read := GetProcAddress( LibUSBHandle, 'usb_bulk_read');

    usb_interrupt_write := GetProcAddress( LibUSBHandle, 'usb_interrupt_write');
    usb_interrupt_read := GetProcAddress( LibUSBHandle, 'usb_interrupt_read');
    usb_control_msg := GetProcAddress( LibUSBHandle, 'usb_control_msg');
    usb_set_configuration := GetProcAddress( LibUSBHandle, 'usb_set_configuration');
    usb_claim_interface := GetProcAddress( LibUSBHandle, 'usb_claim_interface');// was interface, a pascal reserved word
    usb_release_interface := GetProcAddress( LibUSBHandle, 'usb_release_interface');
    usb_set_altinterface := GetProcAddress( LibUSBHandle, 'usb_set_altinterface');
    usb_resetep := GetProcAddress( LibUSBHandle, 'usb_resetep');
    usb_clear_halt := GetProcAddress( LibUSBHandle, 'usb_clear_halt');
    usb_reset := GetProcAddress( LibUSBHandle, 'usb_reset');

    usb_strerror := GetProcAddress( LibUSBHandle, 'usb_strerror');

    usb_init := GetProcAddress( LibUSBHandle, 'usb_init');
    usb_set_debug := GetProcAddress( LibUSBHandle, 'usb_set_debug');
    usb_find_busses := GetProcAddress( LibUSBHandle, 'usb_find_busses');
    usb_find_devices := GetProcAddress( LibUSBHandle, 'usb_find_devices');
    usb_get_device := GetProcAddress( LibUSBHandle, 'usb_device'); // renamed from usb_device because of same named record
    usb_get_busses := GetProcAddress( LibUSBHandle, 'usb_get_busses');

    usb_install_service_np := GetProcAddress( LibUSBHandle, 'usb_install_service_np');
    usb_uninstall_service_np := GetProcAddress( LibUSBHandle, 'usb_uninstall_service_np');
    usb_install_driver_np := GetProcAddress( LibUSBHandle, 'usb_install_driver_np');
    usb_get_version := GetProcAddress( LibUSBHandle, 'usb_get_version');
    usb_isochronous_setup_async := GetProcAddress( LibUSBHandle, 'usb_isochronous_setup_async');
    usb_bulk_setup_async := GetProcAddress( LibUSBHandle, 'usb_bulk_setup_async');
    usb_interrupt_setup_async := GetProcAddress( LibUSBHandle, 'usb_interrupt_setup_async');
    usb_submit_async := GetProcAddress( LibUSBHandle, 'usb_submit_async');
    usb_reap_async := GetProcAddress( LibUSBHandle, 'usb_reap_async');
    usb_reap_async_nocancel := GetProcAddress( LibUSBHandle, 'usb_reap_async_nocancel');
    usb_cancel_async := GetProcAddress( LibUSBHandle, 'usb_cancel_async');
    usb_free_async := GetProcAddress( LibUSBHandle, 'usb_free_async');
  end

finalization
  FreeLibrary(LibUSBHandle)
{$ENDIF}
end.