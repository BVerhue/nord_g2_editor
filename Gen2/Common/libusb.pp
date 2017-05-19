unit libusb;
interface

{$LINKLIB /usr/local/lib/libusb-1.0.a}

//******************************************************************************
//
//  * Project name:
//       Translation of libusb 1.0 header for pascal
//  * Copyright:
//       (C) 2010 Marko Medic <medamarko@gmail.com>
//  * Revision History:
//       20101126:
//        - initial release;
//  * Test Configuration:
//       - Lazarus IDE 0.9.28.2
//       - fpc 2.4
//       - few different USB devices based on PIC MCU
//
//******************************************************************************
    { Pointers to basic pascal types}
    Type
      PLongint  = ^Longint;
      PSmallInt = ^SmallInt;
      PByte     = ^Byte;
      PWord     = ^Word;
      PDWord    = ^DWord;
      PDouble   = ^Double;
      uint8_t  = byte;
      uint16_t = word;
      uint32_t = dword;
      size_t    = uint32_t;

//------------------------------------------------------------------------------
// Constant
//------------------------------------------------------------------------------
{(* standard USB stuff *)}
{(* Device and/or Interface Class codes *)}
const LIBUSB_CLASS_PER_INTERFACE = 0;
const LIBUSB_CLASS_AUDIO = 1;
const LIBUSB_CLASS_COMM = 2;
const LIBUSB_CLASS_HID = 3;
const LIBUSB_CLASS_PRINTER = 7;
const LIBUSB_CLASS_PTP = 6;
const LIBUSB_CLASS_MASS_STORAGE = 8;
const LIBUSB_CLASS_HUB = 9;
const LIBUSB_CLASS_DATA = 10;
const LIBUSB_CLASS_WIRELESS = $e0;
const LIBUSB_CLASS_APPLICATION = $fe;
const LIBUSB_CLASS_VENDOR_SPEC = $ff;

{(* Descriptor types as defined by the USB specification. *)}
const LIBUSB_DT_DEVICE = $01;
const LIBUSB_DT_CONFIG = $02;
const LIBUSB_DT_STRING = $03;
const LIBUSB_DT_INTERFACE = $04;
const LIBUSB_DT_ENDPOINT = $05;
const LIBUSB_DT_HID = $21;
const LIBUSB_DT_REPORT = $22;
const LIBUSB_DT_PHYSICAL = $23;
const LIBUSB_DT_HUB = $29;

{(* Descriptor sizes per descriptor type *)}
const LIBUSB_DT_DEVICE_SIZE = 			18;
const LIBUSB_DT_CONFIG_SIZE = 			9;
const LIBUSB_DT_INTERFACE_SIZE = 		9;
const LIBUSB_DT_ENDPOINT_SIZE = 		7;
const LIBUSB_DT_ENDPOINT_AUDIO_SIZE = 	9;	{* Audio extension *}
const LIBUSB_DT_HUB_NONVAR_SIZE = 		7;
const LIBUSB_ENDPOINT_ADDRESS_MASK = 	$0f;    {* in bEndpointAddress *}
const LIBUSB_ENDPOINT_DIR_MASK = 		$80;

{* Endpoint direction. Values for bit 7 of the ref libusb_endpoint_descriptor.bEndpointAddress 'endpoint address' scheme. *}
const LIBUSB_ENDPOINT_IN = $80;
const LIBUSB_ENDPOINT_OUT = $00;

const LIBUSB_TRANSFER_TYPE_MASK = 			$03;    {* in bmAttributes *}

{* Endpoint transfer cType. Values for bits 0:1 of the ref libusb_endpoint_descriptor.bmAttributes 'endpoint attributes' field.*}
const LIBUSB_TRANSFER_TYPE_CONTROL = 0;
const LIBUSB_TRANSFER_TYPE_ISOCHRONOUS = 1;
const LIBUSB_TRANSFER_TYPE_BULK = 2;
const LIBUSB_TRANSFER_TYPE_INTERRUPT = 3;

{* Standard requests, as defined in table 9-3 of the USB2 specifications *}
const LIBUSB_REQUEST_GET_STATUS = $00;
const LIBUSB_REQUEST_CLEAR_FEATURE = $01;
const LIBUSB_REQUEST_SET_FEATURE = $03;
const LIBUSB_REQUEST_SET_ADDRESS = $05;
const LIBUSB_REQUEST_GET_DESCRIPTOR = $06;
const LIBUSB_REQUEST_SET_DESCRIPTOR = $07;
const LIBUSB_REQUEST_GET_CONFIGURATION = $08;
const LIBUSB_REQUEST_SET_CONFIGURATION = $09;
const LIBUSB_REQUEST_GET_INTERFACE = $0A;
const LIBUSB_REQUEST_SET_INTERFACE = $0B;
const LIBUSB_REQUEST_SYNCH_FRAME = $0C;

{* Request cType bits of the ref libusb_control_setup.bmRequestType 'bmRequestType' field in control transfers. *}
const LIBUSB_REQUEST_TYPE_STANDARD = $00 shl 5;
const LIBUSB_REQUEST_TYPE_CLASS = $01 shl 5;
const LIBUSB_REQUEST_TYPE_VENDOR = $02 shl 5;
const LIBUSB_REQUEST_TYPE_RESERVED = $03 shl 5;

{* Recipient bits of the ref libusb_control_setup.bmRequestType 'bmRequestType' field in control transfers. Values 4 through 31 are reserved. *}
const LIBUSB_RECIPIENT_DEVICE = $00;
const LIBUSB_RECIPIENT_INTERFACE = $01;
const LIBUSB_RECIPIENT_ENDPOINT = $02;
const LIBUSB_RECIPIENT_OTHER = $03;

const LIBUSB_ISO_SYNC_TYPE_MASK = 		$0C;

{* Synchronization cType for isochronous endpoints. Values for bits 2:3 of the ref libusb_endpoint_descriptor.bmAttributes 'bmAttributes' field in libusb_endpoint_descriptor. *}
const LIBUSB_ISO_SYNC_TYPE_NONE = 0;
const LIBUSB_ISO_SYNC_TYPE_ASYNC = 1;
const LIBUSB_ISO_SYNC_TYPE_ADAPTIVE = 2;
const LIBUSB_ISO_SYNC_TYPE_SYNC = 3;

const LIBUSB_ISO_USAGE_TYPE_MASK =  $30;

{* Usage cType for isochronous endpoints. Values for bits 4:5 of the ref libusb_endpoint_descriptor.bmAttributes 'bmAttributes' field in libusb_endpoint_descriptor. *}
const LIBUSB_ISO_USAGE_TYPE_DATA = 0;
const LIBUSB_ISO_USAGE_TYPE_FEEDBACK = 1;
const LIBUSB_ISO_USAGE_TYPE_IMPLICIT = 2;

{* Error codes. Most libusb functions result:= 0 on success or one of these codes on failure.*}
const LIBUSB_SUCCESS = 0;
const LIBUSB_ERROR_IO = -1;
const LIBUSB_ERROR_INVALID_PARAM = -2;
const LIBUSB_ERROR_ACCESS = -3;
const LIBUSB_ERROR_NO_DEVICE = -4;
const LIBUSB_ERROR_NOT_FOUND = -5;
const LIBUSB_ERROR_BUSY = -6;
const LIBUSB_ERROR_TIMEOUT = -7;
const LIBUSB_ERROR_OVERFLOW = -8;
const LIBUSB_ERROR_PIPE = -9;
const LIBUSB_ERROR_INTERRUPTED = -10;
const LIBUSB_ERROR_NO_MEM = -11;
const LIBUSB_ERROR_NOT_SUPPORTED = -12;
const LIBUSB_ERROR_OTHER = -99;

//------------------------------------------------------------------------------
// Structures
//------------------------------------------------------------------------------
{ * Transfer status codes */}
type libusb_transfer_status = (
	//** Transfer completed without error. Note that this does not indicate that the entire amount of requested data was transferred. */
	LIBUSB_TRANSFER_COMPLETED,
	//** Transfer failed */
	LIBUSB_TRANSFER_ERROR,
	//** Transfer timed out */
	LIBUSB_TRANSFER_TIMED_OUT,
	//** Transfer was cancelled */
	LIBUSB_TRANSFER_CANCELLED,
	//** For bulk/interrupt endpoints: halt condition detected (endpoint stalled). For control endpoints: control request not supported. */
	LIBUSB_TRANSFER_STALL,
	//** Device was disconnected */
	LIBUSB_TRANSFER_NO_DEVICE,
	//** Device sent more data than requested */
	LIBUSB_TRANSFER_OVERFLOW
);

{* A structure representing the standard USB device descriptor. This descriptor is documented in section 9.6.1 of the USB 2.0 specification. All multiple-byte fields are represented in host-endian format.*}
type
   Plibusb_device_descriptor = ^libusb_device_descriptor;
   PPlibusb_device_descriptor = ^Plibusb_device_descriptor;
   PPPlibusb_device_descriptor = ^PPlibusb_device_descriptor;
   libusb_device_descriptor = record
		bLength: uint8_t;
		bDescriptorType: uint8_t;
		bcdUSB: uint16_t;
		bDeviceClass: uint8_t;
		bDeviceSubClass: uint8_t;
		bDeviceProtocol: uint8_t;
		bMaxPacketSize0: uint8_t;
		idVendor: uint16_t;
		idProduct: uint16_t;
		bcdDevice: uint16_t;
		iManufacturer: uint8_t;
		iProduct: uint8_t;
		iSerialNumber: uint8_t;
		bNumConfigurations: uint8_t;
	end;

{* A structure representing the standard USB endpoint descriptor. This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All multiple-byte fields are represented in host-endian format.*}
type
    Plibusb_endpoint_descriptor = ^libusb_endpoint_descriptor;
	libusb_endpoint_descriptor = record
		bLength: uint8_t;
		bDescriptorType: uint8_t;
		bEndpointAddress: uint8_t;
		bmAttributes: uint8_t;
		wMaxPacketSize: uint16_t;
		bInterval: uint8_t;
		bRefresh: uint8_t;
		bSynchAddress: uint8_t;
		extra: PByte;
		extra_length: Integer;
	end;

{* A structure representing the standard USB interface descriptor. This descriptor is documented in section 9.6.5 of the USB 2.0 specification. All multiple-byte fields are represented in host-endian format.*}
type
    Plibusb_interface_descriptor = ^libusb_interface_descriptor;
	libusb_interface_descriptor = record
		bLength: uint8_t;
		bDescriptorType: uint8_t;
		bInterfaceNumber: uint8_t;
		bAlternateSetting: uint8_t;
		bNumEndpoints: uint8_t;
		bInterfaceClass: uint8_t;
		bInterfaceSubClass: uint8_t;
		bInterfaceProtocol: uint8_t;
		iInterface: uint8_t;
		endpoint: Plibusb_endpoint_descriptor;
		extra: PByte;
		extra_length: Integer;
	end;

{* A collection of alternate settings for a particular USB interface.*}
type
    Plibusb_interface = ^libusb_interface;
	libusb_interface = record
		altsetting: Plibusb_interface_descriptor;
		num_altsetting: Integer;
	end;

{* A structure representing the standard USB configuration descriptor. This descriptor is documented in section 9.6.3 of the USB 2.0 specification. All multiple-byte fields are represented in host-endian format.*}
type
    Plibusb_config_descriptor = ^libusb_config_descriptor;
    PPlibusb_config_descriptor = ^Plibusb_config_descriptor;
	libusb_config_descriptor = record
		bLength: uint8_t;
		bDescriptorType: uint8_t;
		wTotalLength: uint16_t;
		bNumInterfaces: uint8_t;
		bConfigurationValue: uint8_t;
		iConfiguration: uint8_t;
		bmAttributes: uint8_t;
		MaxPower: uint8_t;
		theinterface: Plibusb_interface;// theInterface (Interface is reserved word)
		extra: PByte;
		extra_length: Integer;
	end;

{ * Setup packet for control transfers. *}
type
	Plibusb_control_setup = ^libusb_control_setup;
	libusb_control_setup = record
		bmRequestType: uint8_t;
		bRequest: uint8_t;
		wValue: uint16_t;
		wIndex: uint16_t;
		wLength: uint16_t;
	end;

const LIBUSB_CONTROL_SETUP_SIZE =  (SizeOf( libusb_control_setup));

{* libusb *}
type
	 Plibusb_context = ^libusb_context;
         PPlibusb_context = Plibusb_context;
	 libusb_context = record
	 {undefined structure}
	 end;
type
	Plibusb_device = ^libusb_device;
        PPlibusb_device = ^Plibusb_device;
        PPPlibusb_device = ^PPlibusb_device;
	libusb_device = record
	{undefined structure}
	end;
type
	Plibusb_device_handle = ^libusb_device_handle;
        PPlibusb_device_handle = ^Plibusb_device_handle;
	libusb_device_handle = record
	{undefined structure}
	end;

{* libusb_transfer.flags values *}
const LIBUSB_TRANSFER_SHORT_NOT_OK = 1 shl 0;
const LIBUSB_TRANSFER_FREE_BUFFER = 1 shl 1;
const LIBUSB_TRANSFER_FREE_TRANSFER = 1 shl 2;

{*Isochronous packet descriptor. *}
type
    Plibusb_iso_packet_descriptor = ^libusb_iso_packet_descriptor;
	 libusb_iso_packet_descriptor = record
	 length: DWORD;
	 actual_length: DWORD;
	 status: libusb_transfer_status;
    end;

type
    Pstructlibusb_transfer = ^structlibusb_transfer;
    libusb_transfer_cb_fn= procedure(transfer : Pstructlibusb_transfer);
    Plibusb_transfer_cb_fn = ^libusb_transfer_cb_fn;
    structlibusb_transfer = record
         dev_handle: ^libusb_device_handle;
	 flags: uint8_t;
	 endpoint:byte;
	 ctype: byte; // type is reserved word
	 timeout: DWORD ;
	 status: libusb_transfer_status;
	 length: integer;
	 actual_length: integer ;
	 callback: Plibusb_transfer_cb_fn;
         user_data: pointer;
	 buffer:PChar;
	 num_iso_packets: integer ;
	 iso_packet_desc: plibusb_iso_packet_descriptor;
    end;

//******************************************************************************
//------------------------------------------------------------------------------
// Library functions and procedures
//------------------------------------------------------------------------------
//******************************************************************************
function libusb_init(ctx:PPlibusb_context):longint;cdecl;external;
procedure libusb_exit(ctx:Plibusb_context);cdecl;external;
procedure libusb_set_debug(ctx:Plibusb_context; level:integer);cdecl;external;

function libusb_get_device_list(ctx:Plibusb_context; list: PPPlibusb_device): size_t;cdecl;external;
procedure libusb_free_device_list(list:PPlibusb_device; unref_devices:integer);cdecl;external;
function libusb_get_configuration(dev: Plibusb_device_handle; config: PLongint): Integer;cdecl;external;

function libusb_get_device_descriptor(dev: Plibusb_device; desc:Plibusb_device_descriptor): Integer;cdecl;external;
function libusb_get_active_config_descriptor(dev: Plibusb_device; config:PPlibusb_config_descriptor): Integer;cdecl;external;

function libusb_get_config_descriptor(dev: Plibusb_device;config_index:uint8_t; config: PPlibusb_config_descriptor):Integer;cdecl;external;

function libusb_get_max_packet_size(dev: Plibusb_device; endpoint: Byte): Integer;cdecl;external;
function libusb_get_max_iso_packet_size(dev: Plibusb_device; endpoint: Byte): Integer;cdecl;external;

function libusb_open(dev: Plibusb_device; handle: PPlibusb_device_handle): Integer;cdecl;external;
procedure libusb_close(dev_handle:Plibusb_device_handle);cdecl;external;
function libusb_get_device(dev_handle: Plibusb_device_handle): libusb_device;cdecl;external;

function libusb_set_configuration(dev: Plibusb_device_handle; configuration: Integer): Integer;cdecl;external;

function libusb_claim_interface(dev: Plibusb_device_handle; iface: Integer): Integer;cdecl;external;
function libusb_release_interface(dev: Plibusb_device_handle; iface: Integer): Integer;cdecl;external;

function libusb_open_device_with_vid_pid(ctx: Plibusb_context; vendor_id: uint16_t; product_id: uint16_t): Plibusb_device_handle;cdecl;external;

function libusb_set_interface_alt_setting(dev: Plibusb_device_handle;interface_number: Integer;alternate_setting: Integer): Integer;cdecl;external;
function libusb_clear_halt(dev: Plibusb_device_handle; endpoint: BYTE): Integer;cdecl;external;
function libusb_reset_device(dev: Plibusb_device_handle): Integer;cdecl;external;

function libusb_kernel_driver_active(dev: Plibusb_device_handle; theinterface: Integer): Integer;cdecl;external;
function libusb_detach_kernel_driver(dev: Plibusb_device_handle; theinterface: Integer): Integer;cdecl;external;
function libusb_attach_kernel_driver(dev: Plibusb_device_handle; theinterface: Integer): Integer;cdecl;external;
//------------------------------------------------------------------------------
// Async I/O
//------------------------------------------------------------------------------
function libusb_submit_transfer(transfer: Pstructlibusb_transfer):Integer;cdecl;external;
function libusb_cancel_transfer(transfer: Pstructlibusb_transfer):Integer;cdecl;external;
procedure libusb_free_transfer(transfer: Pstructlibusb_transfer);cdecl;external;
function libusb_alloc_transfer(iso_packets:integer):Pstructlibusb_transfer;cdecl;external;
//------------------------------------------------------------------------------
// Sync I/O
//------------------------------------------------------------------------------
function libusb_control_transfer(dev_handle: Plibusb_device_handle; request_type: uint8_t; request: uint8_t; value: uint16_t; index: uint16_t; data: PChar; length:uint16_t;timeout: DWORD):Integer;cdecl;external;
function libusb_bulk_transfer(dev_handle: Plibusb_device_handle; endpoint:BYTE; data: Pchar; length:integer;actual_length:Plongint; timeout:DWORD):Integer;cdecl;external; //tested
function libusb_interrupt_transfer(dev_handle: Plibusb_device_handle; endpoint:byte; data:Pchar; length:integer; actual_length:Plongint; timeout:DWORD):Integer;cdecl;external;    //tested

function libusb_get_string_descriptor_ascii(dev:plibusb_device_handle ;index:uint8_t; data:Pchar; length:integer):Integer;cdecl;external;

{/* polling and timeouts */}
function libusb_try_lock_events(ctx:Plibusb_context):integer;cdecl;external;
procedure libusb_lock_events(ctx:Plibusb_context);cdecl;external;
procedure libusb_unlock_events(ctx:Plibusb_context);cdecl;external;
function libusb_event_handling_ok(ctx:Plibusb_context):integer;cdecl;external;
function libusb_event_handler_active(ctx:Plibusb_context):integer;cdecl;external;
procedure libusb_lock_event_waiters(ctx:Plibusb_context); cdecl;external;
procedure libusb_unlock_event_waiters(ctx:Plibusb_context); cdecl;external;

implementation



end.



