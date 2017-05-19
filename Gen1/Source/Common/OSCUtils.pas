/////project name
//OSCUtils

//////description
//Utility library to encode/decode osc-packets
//inspired by original OSC reference implementation (OSC-Kit)
//and OSC.Net library as shipped with the TUIO-CSharp sample
//from http://reactable.iua.upf.edu/?software

//////licence
//GNU Lesser General Public License (LGPL)
//english: http://www.gnu.org/licenses/lgpl.html
//german: http://www.gnu.de/lgpl-ger.html

//////language/ide
//delphi

//////initial author
//joreg -> joreg@vvvv.org

//additions for FreePascal
//simon -> simonmoscrop@googlemail.com

//////instructions
////for use with FreePascal
//define: FPC

////encoding a single message:
//first create a message: msg := TOSCMessage.Create(address)
//then call msg.AddAsString(typetag, value) to add any number of arguments
//with msg.ToOSCString you get its osc-string representation
////encoding a bundle:
//first create a bundle: bundle := TOSCBundle.Create
//then add any number of packets (i.e. message, bundle) via bundle.Add(packet)
//with bundle.ToOSCString you get its osc-string representation

////decoding a string
//use TOSCPacket.Unpack(PByte(osc-string), Length(osc-string)) to create
//TOSCPackets of your osc-strings (those can be either bundles or single
//messages. if you want to decode several packets at once you can create
//a container bundle first and add the packets you create like this.
//then use msg := FPacket.MatchAddress(address) to find a message with the
//according address in your packet-structure.
//before you now can access the arguments and typetags of a message you have
//to call msg.Decode
//voila.


unit OSCUtils;

interface

uses Classes, Contnrs;

type
  TOSCPacket = class;
  TOSCMessage = class;

  TOSCPacket = class (TObject)
  private
  protected
    FBytes: PByte;
    function MatchBrackets(pMessage, pAddress: PAnsiChar): Boolean;
    function MatchList(pMessage, pAddress: PAnsiChar): Boolean;
    function MatchPattern(pMessage, pAddress: PAnsiChar): Boolean;
  public
    constructor Create(Bytes: PByte);
    function MatchAddress(Address: AnsiString): TOSCMessage; virtual; abstract;
    function ToOSCString: Ansistring; virtual; abstract;
    procedure Unmatch; virtual; abstract;
    class function Unpack(Bytes: PByte; Count: Integer): TOSCPacket; overload;
    class function Unpack(Bytes: PByte; Offset, Count: Integer; TimeTag: Extended =
        0): TOSCPacket; overload; virtual;
  end;

  TOSCMessage = class(TOSCPacket)
  private
    FAddress: Ansistring;
    FArguments: TStringList;
    FIsDecoded: Boolean;
    FMatched: Boolean;
    FTimeTag: Extended;
    FTypeTagOffset: Integer;
    FTypeTags: Ansistring;
    function GetArgument(Index: Integer): Ansistring;
    function GetArgumentCount: Integer;
    function GetTypeTag(Index: Integer): Ansistring;
  public
    constructor Create(Address: Ansistring); overload;
    constructor Create(Bytes: PByte); overload;
    destructor Destroy; override;
    function AddAsString(TypeTag: AnsiChar; Value: AnsiString): HResult;
    procedure AddFloat(Value: Single);
    procedure AddInteger(Value: Integer);
    procedure AddString(Value: AnsiString);
    procedure Decode;
    function MatchAddress(Address: AnsiString): TOSCMessage; override;
    function ToOSCString: Ansistring; override;
    procedure Unmatch; override;
    class function Unpack(Bytes: PByte; PacketOffset, Count: Integer; TimeTag:
        Extended = 0): TOSCPacket; overload; override;
    property Address: Ansistring read FAddress write FAddress;
    property Argument[Index: Integer]: AnsiString read GetArgument;
    property ArgumentCount: Integer read GetArgumentCount;
    property IsDecoded: Boolean read FIsDecoded write FIsDecoded;
    property Matched: Boolean read FMatched write FMatched;
    property TimeTag: Extended read FTimeTag write FTimeTag;
    property TypeTag[Index: Integer]: AnsiString read GetTypeTag;
    property TypeTagOffset: Integer read FTypeTagOffset write FTypeTagOffset;
  end;

  TOSCBundle = class(TOSCPacket)
  private
    FPackets: TObjectList;
  public
    constructor Create(Bytes: PByte);
    destructor Destroy; override;
    procedure Add(const Packet: TOSCPacket);
    function MatchAddress(Address: AnsiString): TOSCMessage; override;
    function ToOSCString: AnsiString; override;
    procedure Unmatch; override;
    class function Unpack(Bytes: PByte; PacketOffset, Count: Integer; TimeTag:
        Extended = 0): TOSCPacket; overload; override;
  end;

  function MakeOSCFloat(value: Single): AnsiString;

  function MakeOSCInt(value: Integer): AnsiString;

  function MakeOSCString(value: AnsiString): AnsiString;

  function UnpackFloat(Bytes: PByte; var Offset: Integer): Single;

  function UnpackInt(Bytes: PByte; var Offset: Integer): Integer;

  function UnpackString(Bytes: PByte; var Offset: Integer): Ansistring;

  const
    OSC_OK = 0;
    OSC_UNRECOGNIZED_TYPETAG = 1;
    OSC_CONVERT_ERROR = 2;


implementation

uses
  SysUtils, Math {$IFNDEF FPC}, WinSock {$ENDIF};

function MakeOSCFloat(value: Single): AnsiString;
var
  tmp: Byte;
  intg, i: Integer;
begin
  result := '';
  intg := Integer(Pointer(value));
  {$IFDEF FPC}
  intg := BEtoN(intg);
  {$ELSE}
  intg := htonl(intg);
  {$ENDIF}
  for i := 0 to 3 do
  begin
    tmp := intg and $ff;
    result := result + AnsiChar(tmp);
    intg := intg shr 8;
  end;
end;

function MakeOSCInt(value: Integer): AnsiString;
var
  tmp: Byte;
  i, val: Integer;
begin
  result := '';
  {$IFDEF FPC}
  val := BEtoN(value);
  {$ELSE}
  val := htonl(value);
  {$ENDIF}
  for i := 0 to 3 do
  begin
    tmp := val and $ff;
    result := result + AnsiChar(tmp);
    val := val shr 8;
  end;
end;

function MakeOSCString(value: AnsiString): AnsiString;
var i, ln: Integer;
begin
  result := value;

  ln := 4 - (length(value)) mod 4;
  for i := 0 to ln - 1 do
    result := result + #0;
end;

function UnpackFloat(Bytes: PByte; var Offset: Integer): Single;
var
  i, value: Integer;
  tmp: PByte;
begin
  value := 0;
  tmp := Bytes;
  Inc(tmp, Offset);

  for i := 0 to 3 do
  begin
    value := value + tmp^ shl (i * 8);
    Inc(tmp);
  end;

  Inc(Offset, 4);

  {$IFDEF FPC}
  value := NtoBE(value);
  {$ELSE}
  value := ntohl(value);
  {$ENDIF}
  Result := Single(Pointer(value));
end;

function UnpackInt(Bytes: PByte; var Offset: Integer): Integer;
var
  i, value: Integer;
  tmp: PByte;
begin
  value := 0;
  tmp := Bytes;
  Inc(tmp, Offset);

  for i := 0 to 3 do
  begin
    value := value + tmp^ shl (i * 8);
    Inc(tmp);
  end;

  Inc(Offset, 4);
  {$IFDEF FPC}
  Result := NtoBE(value);
  {$ELSE}
  Result := ntohl(value);
  {$ENDIF}
end;

function UnpackString(Bytes: PByte; var Offset: Integer): AnsiString;
var
  tmp: PByte;
  off: Integer;
begin
  tmp := Bytes;
  Inc(tmp, Offset);

  Result := PAnsiChar(tmp);
  off := Length(PAnsiChar(tmp));
  off := off + (4 - off mod 4);
  Inc(Offset, off)
end;


constructor TOSCMessage.Create(Address: AnsiString);
begin
  FAddress := Address;
  Create(nil);
end;

constructor TOSCMessage.Create(Bytes: PByte);
begin
  inherited;

  FTypeTags := ',';
  FArguments := TStringList.Create;
  FIsDecoded := false;
end;

destructor TOSCMessage.Destroy;
begin
  FArguments.Free;
  inherited;
end;

function TOSCMessage.AddAsString(TypeTag: AnsiChar; Value: AnsiString): HResult;
begin
  Result := OSC_OK;

  try
    if TypeTag = 'f' then
      FArguments.Add(string(MakeOSCFloat(StrToFloat(string(Value)))))
    else if TypeTag = 'i' then
      FArguments.Add(string(MakeOSCInt(StrToInt(string(Value)))))
    else if TypeTag = 's' then
      FArguments.Add(string(MakeOSCString(Value)))
    else
      Result := OSC_UNRECOGNIZED_TYPETAG;
  except on EConvertError do
    Result := OSC_CONVERT_ERROR;
  end;

  if Result = OSC_OK then
    FTypeTags := FTypeTags + TypeTag;
end;

procedure TOSCMessage.AddFloat(Value: Single);
begin
  FTypeTags := FTypeTags + 'f';
  FArguments.Add(string(MakeOSCFloat(Value)));
end;

procedure TOSCMessage.AddInteger(Value: Integer);
begin
  FTypeTags := FTypeTags + 'i';
  FArguments.Add(string(MakeOSCInt(Value)));
end;

procedure TOSCMessage.AddString(Value: AnsiString);
begin
  FTypeTags := FTypeTags + 's';
  FArguments.Add(string(MakeOSCString(Value)));
end;

procedure TOSCMessage.Decode;
var
  i, offset: Integer;
begin
  if FIsDecoded then
    exit;

  offset := FTypeTagOffset;
  FTypeTags := UnpackString(FBytes, offset);

  for i := 1 to Length(FTypeTags) - 1 do
  begin
    if FTypeTags[i+1] = 's' then
      FArguments.Add(string(UnpackString(FBytes, offset)))
    else if FTypeTags[i+1] = 'i' then
      FArguments.Add(IntToStr(UnpackInt(FBytes, offset)))
    else if FTypeTags[i+1] = 'f' then
      FArguments.Add(FloatToStr(UnpackFloat(FBytes, offset)));
  end;

  FIsDecoded := true;
end;

function TOSCMessage.GetArgument(Index: Integer): AnsiString;
begin
  Result := AnsiString(FArguments[Index]);
end;

function TOSCMessage.GetArgumentCount: Integer;
begin
  Result := FArguments.Count;
end;

function TOSCMessage.GetTypeTag(Index: Integer): AnsiString;
begin
  Result := FTypeTags[Index + 2];
end;

function TOSCMessage.MatchAddress(Address: AnsiString): TOSCMessage;
begin
  if not FMatched
  and MatchPattern(PAnsiChar(FAddress), PAnsiChar(Address)) then
  begin
    FMatched := true;
    Result := Self
  end
  else
    Result := nil;
end;

function TOSCMessage.ToOSCString: AnsiString;
var
  i: Integer;
begin
  Result := MakeOSCString(FAddress) + MakeOSCString(FTypeTags);

  for i := 0 to FArguments.Count - 1 do
    Result := Result + AnsiString(FArguments[i]);
end;

procedure TOSCMessage.Unmatch;
begin
  FMatched := false;
end;

class function TOSCMessage.Unpack(Bytes: PByte; PacketOffset, Count: Integer;
    TimeTag: Extended = 0): TOSCPacket;
begin
  Result := TOSCMessage.Create(Bytes);
  //for now decode address only
  (Result as TOSCMessage).Address := UnpackString(Bytes, PacketOffset);
  (Result as TOSCMessage).TimeTag := TimeTag;

  //save offset for later decoding on demand
 (Result as TOSCMessage).TypeTagOffset := PacketOffset;
 (Result as TOSCMessage).IsDecoded := false;
end;

constructor TOSCBundle.Create(Bytes: PByte);
begin
  inherited;
  FPackets := TObjectList.Create;
  FPackets.OwnsObjects := true;
end;

destructor TOSCBundle.Destroy;
begin
  FPackets.Free;
  inherited;
end;

procedure TOSCBundle.Add(const Packet: TOSCPacket);
begin
  FPackets.Add(Packet);
end;

function TOSCBundle.MatchAddress(Address: AnsiString): TOSCMessage;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to FPackets.Count - 1 do
  begin
    Result := (FPackets[i] as TOSCPacket).MatchAddress(Address);
    if Assigned(Result) then
      break;
  end;
end;

function TOSCBundle.ToOSCString: AnsiString;
var
  timeTag: AnsiString;
  i: Integer;
  packet: AnsiString;
begin
  timeTag := #0#0#0#0#0#0#0#1; //immediately
  Result := MakeOSCString('#bundle') + timeTag;

  for i := 0 to FPackets.Count - 1 do
  begin
    packet := (FPackets[i] as TOSCPacket).ToOSCString;
    Result := Result + MakeOSCInt(Length(packet)) + packet;
  end;
end;

procedure TOSCBundle.Unmatch;
var
  i: Integer;
begin
  for i := 0 to FPackets.Count - 1 do
    (FPackets[i] as TOSCPacket).UnMatch;
end;

class function TOSCBundle.Unpack(Bytes: PByte; PacketOffset, Count: Integer;
    TimeTag: Extended = 0): TOSCPacket;
var
  packetLength: Integer;
  tt1, tt2: Cardinal;
begin
  Result := TOSCBundle.Create(Bytes);

  //advance the '#bundle' string
  UnpackString(Bytes, PacketOffset);

  //advance the timestamp
  tt1 := Cardinal(UnpackInt(Bytes, PacketOffset));
  tt2 := Cardinal(UnpackInt(Bytes, PacketOffset));

  TimeTag := tt1 + tt2 / power(2, 32);

  while PacketOffset < Count do
  begin
    packetLength := UnpackInt(Bytes, PacketOffset);
    //note: PacketOffset is always from the very beginning of Bytes!
    //not the beginning of the current packet.
    (Result as TOSCBundle).Add(TOSCPacket.Unpack(Bytes, PacketOffset, PacketOffset + packetLength, TimeTag));
    Inc(PacketOffset, packetLength);
  end;
end;

constructor TOSCPacket.Create(Bytes: PByte);
begin
  FBytes := Bytes;
end;

// we know that pattern[0] == '[' and test[0] != 0 */
function TOSCPacket.MatchBrackets(pMessage, pAddress: PAnsiChar): Boolean;
var
  negated: Boolean;
  p, p1, p2: PAnsiChar;
begin
  p := pMessage;
  Result := false;
  negated := false;

  Inc(pMessage);
  if pMessage^ = #0 then
  begin
    //LogWarningFMT('Unterminated [ in message: %s', [FInput[0]]);
    Dec(pMessage);
    exit;
  end;

  if pMessage^ = '!' then
  begin
    negated := true;
    Inc(p);
  end;

  Dec(pMessage);

  Result := negated;

  while p^ <> ']' do
  begin
    if p^ = #0 then
    begin
      //LogWarningFMT('Unterminated [ in message: %s', [FInput[0]]);
      exit;
    end;

    p1 := p + 1; // sizeOf(PChar);
    p2 := p1 + 1; //sizeOf(PChar);

    if (p1^ = '-')
    and (p2^ <> #0) then
      if (Ord(pAddress^) >= Ord(p^))
      and (Ord(pAddress^) <= Ord(p2^)) then
      begin
        Result := not negated;
        break;
      end;

    if p^ = pAddress^ then
    begin
      Result := not negated;
      break;
    end;

    Inc(p);
  end;

  if Result = false then
    exit;

  while p^ <> ']' do
  begin
    if p^ = #0 then
    begin
      //LogWarningFMT('Unterminated [ in message: %s', [FInput[0]]);
      exit;
    end;

    Inc(p);
  end;

  Inc(p);
  pMessage := p;
  Inc(pAddress);
  Result := MatchPattern(p, pAddress);
end;

function TOSCPacket.MatchList(pMessage, pAddress: PAnsiChar): Boolean;
var
  p, tp: PAnsiChar;
begin
  Result := false;

  p := pMessage;
  tp := pAddress;

  while p^ <> '}' do
  begin
    if p^ = #0 then
    begin
      //LogWarningFMT('Unterminated { in message: %s', [FInput[0]]);
      exit;
    end;

    Inc(p);
  end;


// for(restOfPattern = pattern; *restOfPattern != '}'; restOfPattern++) {
// if (*restOfPattern == 0) {
// OSCWarning("Unterminated { in pattern \".../%s/...\"", theWholePattern);
// return FALSE;
// }
//}

  Inc(p); // skip close curly brace
  Inc(pMessage); // skip open curly brace

  while true do
  begin
    if pMessage^ = ',' then
    begin
      if MatchPattern(p, tp) then
      begin
        Result := true;
        pMessage := p;
        pAddress := tp;
        exit;
      end
      else
      begin
        tp := pAddress;
        Inc(pMessage);
      end;
    end
    else if pMessage^ = '}' then
    begin
      Result := MatchPattern(p, tp);
      pMessage := p;
      pAddress := tp;
      exit;
    end
    else if pMessage^ = tp^ then
    begin
      Inc(pMessage);
      Inc(tp);
    end
    else
    begin
      tp := pAddress;
      while (pMessage^ <> ',')
        and (pMessage^ <> '}') do
          Inc(pMessage);

      if pMessage^ = ',' then
        Inc(pMessage);
    end;
  end;
end;

function TOSCPacket.MatchPattern(pMessage, pAddress: PAnsiChar): Boolean;
begin
  if (pMessage = nil)
  or (pMessage^ = #0) then
  begin
    Result := pAddress^ = #0;
    exit;
  end;

  if pAddress^ = #0 then
  begin
    if pMessage^ = '*' then
    begin
      Result := MatchPattern(pMessage + 1, pAddress);
      exit;
    end
    else
    begin
      Result := false;
      exit;
    end;
  end;

  case pMessage^ of
  #0 : Result := pAddress^ = #0;
  '?': Result := MatchPattern(pMessage + 1, pAddress + 1);
  '*':
  begin
      if MatchPattern(pMessage + 1, pAddress) then
        Result := true
      else
        Result := MatchPattern(pMessage, pAddress + 1);
  end;
  ']','}':
  begin
    //LogWarningFMT('Spurious %s in message: %s', [pMessage^, FInput[0]]);
    Result := false;
  end;
  '[': Result := MatchBrackets(pMessage, pAddress);
  '{': Result := MatchList(pMessage, pAddress);
  {'\\':
begin
if pMessage^ + 1 = #0 then
Result := pAddress^ = #0
else if pMessage^ + 1 = pAddress^
Result := MatchPattern(pMessage + 2, pAddress + 1)
else
Result := false;
end; }
  else
  if pMessage^ = pAddress^ then
    Result := MatchPattern(pMessage + 1,pAddress + 1)
  else
    Result := false;
  end;
end;

class function TOSCPacket.Unpack(Bytes: PByte; Count: Integer): TOSCPacket;
begin
  Result := UnPack(Bytes, 0, Count);
end;

class function TOSCPacket.Unpack(Bytes: PByte; Offset, Count: Integer; TimeTag:
    Extended = 0): TOSCPacket;
var
  tmp: PByte;
begin
  tmp := Bytes;
  Inc(tmp, Offset);

  if AnsiChar(tmp^) = '#' then
    Result := TOSCBundle.UnPack(Bytes, Offset, Count)
  else
    Result := TOSCMessage.UnPack(Bytes, Offset, Count, TimeTag);
end;

end.
