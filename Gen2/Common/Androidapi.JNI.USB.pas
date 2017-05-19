unit Androidapi.JNI.USB;

interface
uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Classes;

type
// =============================================================================
//
//                                 UsbEndPoint
//
// =============================================================================

  JUsbEndPoint = interface;
  JUsbEndPointClass = interface(JObjectClass)
  ['{4B9757DB-9DF8-4E8A-B981-E318D099B0A1}']
  end;

  [JavaSignature('android/hardware/usb/UsbEndpoint')]
  JUsbEndPoint = interface(JObject)
  ['{2D1BCC63-C184-41D0-A23A-78E256F8E1D4}']
    function init:JUsbEndPoint; cdecl;
    function getAddress:integer; cdecl;
    function getAttributes:integer; cdecl;
    function getDirection:integer; cdecl;
    function getEndpointNumber:integer; cdecl;
    function getInterval:integer; cdecl;
    function getMaxPacketSize:integer; cdecl;
    function getType:integer; cdecl;
  end;

  TJUsbEndPoint = class(TJavaGenericImport<JUsbEndPointClass, JUsbEndPoint>) end;

// =============================================================================
//
//                                 UsbInterface
//
// =============================================================================

  JUsbInterface = interface;
  JUsbInterfaceClass = interface(JObjectClass)
  ['{245DC801-9BF6-4014-B84A-62F44A8C3DB9}']
  end;

  [JavaSignature('android/hardware/usb/UsbInterface')]
  JUsbInterface = interface(JObject)
  ['{82D91C14-A4AA-47D7-BA6A-5B05B86F6856}']
    function init:JUsbInterface; cdecl;
    function getEndpoint(i : integer): JUsbEndPoint; cdecl;
    function getEndpointCount: integer; cdecl;
    function getId: integer; cdecl;
    function getInterfaceClass: integer; cdecl;
    function getInterfaceProtocol: integer; cdecl;
    function getInterfaceSubclass: integer; cdecl;
  end;

  TJUsbInterface = class(TJavaGenericImport<JUsbInterfaceClass, JUsbInterface>) end;

// =============================================================================
//
//                               UsbDeviceConnection
//
// =============================================================================

  JUsbDeviceConnection = interface;
  JUsbDeviceConnectionClass = interface(JObjectClass)
  ['{447D85BC-BA61-4BBA-A803-563071D90D85}']
  end;

  [JavaSignature('android/hardware/usb/UsbDeviceConnection')]
  JUsbDeviceConnection = interface(JObject)
  ['{D613CA69-DD0E-404A-A064-828E09429145}']
    function init:JUsbDeviceConnection; cdecl;
    function bulkTransfer(UsbEndpoint: JUsbEndpoint; data: TJavaArray<Byte>; length : integer; timeout : integer): integer; cdecl;
    function claimInterface(UsbInterface: JUsbInterface; ForceClaim: boolean): boolean; cdecl;
    procedure close; cdecl;
    function releaseInterface(UsbInterface: JUsbInterface): boolean; cdecl;
  end;

  TJUsbDeviceConnection = class(TJavaGenericImport<JUsbDeviceConnectionClass, JUsbDeviceConnection>) end;

// =============================================================================
//
//                                   UsbDevice
//
// =============================================================================

  JUsbDevice = interface;
  JUsbDeviceClass = interface(JObjectClass)
  ['{38F968EC-5B0B-4018-A302-4DC469509254}']
  end;

  [JavaSignature('android/hardware/usb/UsbDevice')]
  JUsbDevice = interface(JObject)
  ['{35B16245-52F3-409B-86BF-259F3A8F4845}']
    function getProductId:integer; cdecl;
    function getVendorId:integer; cdecl;
    function getInterface(i: integer):JUSBInterface; cdecl;
    function getInterfaceCount: integer; cdecl;
  end;

  TJUsbDevice = class(TJavaGenericImport<JUsbDeviceClass, JUsbDevice>) end;

// =============================================================================
//
//                                 UsbManager
//
// =============================================================================

  JUsbManager = interface;
  JUsbManagerClass = interface(JObjectClass)
  ['{D4A4DDAC-EE30-4123-A0BE-76F8E95FAC55}']
  end;

  [JavaSignature('android/hardware/usb/UsbManager')]
  JUsbManager = interface(JObject)
  ['{5E8A5FA6-64DA-4C90-9D52-988D66E6728E}']
    function getDeviceList:JHashMap; cdecl;
    function hasPermission(UsbDevice:JUsbDevice):boolean; cdecl;
    function openDevice(UsbDevice:JUsbDevice):JUsbDeviceConnection; cdecl;
  end;

  TJUsbManager = class(TJavaGenericImport<JUsbManagerClass, JUsbManager>) end;


implementation

end.

