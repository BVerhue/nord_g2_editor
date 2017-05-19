unit g2_midi;

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
//  This unit needs the Delphi MIDI I/O components, download at https://bitbucket.org/h4ndy/midiio-dev/overview
//
//  Most of it disabled for the VST
//
//  ////////////////////////////////////////////////////////////////////////////


interface
uses
{$IFDEF G2_VER220_up}
  System.Classes, System.SysUtils, System.Controls, System.Contnrs,
{$ELSE}
  Classes, SysUtils, Controls, Contnrs,
{$ENDIF}
  MidiType,
{$IFNDEF G2_VST}
  MMSystem, MidiIn, MidiOut,
{$ENDIF}
  g2_types, g2_file, g2_usb;

const
  CHANNEL_MASK = $f;
  MAX_CTRL_MIDI_ASSIGNMENTS = 255;

type
  TMidiDeviceStateChangeEvent = procedure(Sender : TObject) of object;

  TG2Midi = class;
  TMidiAwareControl = class;
  TMidiEditorAssignmentList = class;

{$IFNDEF G2_VST}
  TMidiDevice = class
  private
    FName : string;
    FOpen : boolean;
    FAssignment : TMidiDeviceAssignmentType;
    FOnMidiDeviceStateChange : TMidiDeviceStateChangeEvent;

  protected
    procedure SetOpen( aValue : Boolean); virtual;
    procedure SetAssignment( aValue : TMidiDeviceAssignmentType);
  public
    constructor Create;
    destructor Destroy; override;

    property Name : string read FName write FName;
    property Open : boolean read FOpen write SetOpen;
    property Assignment : TMidiDeviceAssignmentType read FAssignment write SetAssignment;
    property OnMidiDeviceStateChange : TMidiDeviceStateChangeEvent read FOnMidiDeviceStateChange write FOnMidiDeviceStateChange;
  end;

  TMidiInDevice = class( TMidiDevice)
  private
    FMidiInput     : TMidiInput;
  protected

    procedure SetOpen( Value : Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;

    property MidiInput : TMidiInput read FMidiInput;
  end;

  TMidiOutDevice = class( TMidiDevice)
  private
    FMidiOutput     : TMidiOutput;
  protected
    procedure SetOpen( Value : Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;

    property MidiOutput : TMidiOutput read FMidiOutput;
  end;

  TMidiDeviceList = class(TObjectList)
  protected
    function    GetMidiDevice( aIndex : integer) : TMidiDevice;
    procedure   SetMidiDevice( aIndex : integer; const aValue : TMidiDevice);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    function    Find( aMidiDevice : TMidiDevice): integer;
    property    Items[ aIndex : integer]: TMidiDevice read GetMidiDevice write SetMidiDevice; default;
  end;


  TMidiOutputList = class( TObjectList)
  protected
    function    GetMidiOutput( aIndex : integer) : TMidiOutput;
    procedure   SetMidiOutput( aIndex : integer; const aValue : TMidiOutput);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    function    Find( aMidiOutput : TMidiOutput): integer;
    procedure   SendValue( aMidiMessage, aData1, aData2 : byte);

    property    Items[ aIndex : integer]: TMidiOutput read GetMidiOutput write SetMidiOutput; default;
  end;
{$ENDIF}

  // Midi assignment to editor UI control
  TMidiEditorAssignment = class
  private
    FChannel        : byte;
    FNote           : byte; // Note event (on/off)
    FCC             : byte; // CC value, range of 0..127
    FControlIndex   : byte; // For indexed controls (radiobuttons/onoff buttons)
    FMinValue       : byte;
    FMaxValue       : byte;
{$IFNDEF G2_VST}
    FMidiOutputList : TMidiOutputList;
{$ENDIF}
    FControl        : TMidiAwareControl; // Pointer to UI controls see g2_graph
    FControlPath    : string; // Osc style adressing of the control within the application
    FG2             : TG2Midi; // To access log
  public
    constructor Create( aG2 : TG2Midi);
    destructor Destroy; override;

    procedure WriteLog( aMessage, aData1, aData2 : byte);
    procedure SendValue( aValue : byte);

    property Channel : byte read FChannel write FChannel;
    property Note : byte read FNote write FNote;
    property ControlIndex : byte read FControlIndex write FControlIndex;
    property CC : byte read FCC write FCC;
    property MinValue : byte read FMinValue write FMinValue;
    property MaxValue : byte read FMaxValue write FMaxValue;
    property Control : TMidiAwareControl read FControl write FControl;
    property ControlPath : string read FControlPath write FControlPath;
{$IFNDEF G2_VST}
    property MidiOutputList : TMidiOutputList read FMidiOutputList write FMidiOutputList;
{$ENDIF}
  end;

  TMidiEditorAssignmentList = class(TObjectList)
  protected
    function    GetMidiEditorAssignment( aIndex : integer) : TMidiEditorAssignment;
    procedure   SetMidiEditorAssignment( aIndex : integer; const aValue : TMidiEditorAssignment);
  public
    constructor Create( AOwnsObjects: Boolean);
    destructor  Destroy; override;
    function    FindControl( aControl : TMidiAwareControl): TMidiEditorAssignment;
    function    FindControlNote( aControl : TMidiAwareControl; aChannel, aNote : byte): TMidiEditorAssignment;
    function    FindControlCC( aControl : TMidiAwareControl; aChannel, aCC : byte): TMidiEditorAssignment;
    function    FindControlHasCC( aControl : TMidiAwareControl): TMidiEditorAssignment;
    function    FindControlHasIndex( aControl: TMidiAwareControl; aIndex : byte): TMidiEditorAssignment;

    property    Items[ aIndex : integer]: TMidiEditorAssignment read GetMidiEditorAssignment write SetMidiEditorAssignment; default;
  end;

  TMidiAwareControl = class( TGraphicControl)
  protected
    FIndexedControl : boolean;
    FMidiEditorAssignmentList : TMidiEditorAssignmentList;
    FMidiReceiving : boolean;
    FMidiAware : boolean;
    FShowMidiBox : boolean;
    procedure SetShowMidiBox( aValue : boolean);
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetValueByCtrlMidi( aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent); virtual;
    procedure   SendCtrlMidiValue;
    function    GetParameter : TG2FileParameter; virtual;
    function    GetValue : byte; virtual;
    function    GetHighValue : byte; virtual;
    function    GetLowValue : byte; virtual;
    function    GetCntrlPath : string;

    property MidiEditorAssignmentList : TMidiEditorAssignmentList read FMidiEditorAssignmentList write FMidiEditorAssignmentList;
    property MidiReceiving : boolean read FMidiReceiving write FMidiReceiving;
    property IndexedControl : boolean read FIndexedControl write FIndexedControl;
    property ShowMidiBox : boolean read FShowMidiBox write SetShowMidiBox;
  published
    property MidiAware : boolean read FMidiAware write FMidiAware;
  end;


  // Midi functionality for G2 class
  TG2Midi = class( TG2USB)
    private
      FMidiEnabled : boolean;
      FSysExStream : TMemoryStream; // Buffer for sysex
      FMidiInDeviceName : string;
      FMidiOutDeviceName : string;
{$IFNDEF G2_VST}
      FMidiInDevice  : TMidiInDevice;
      FMidiOutDevice : TMidiOutDevice;
      FLastMidiEvent : TMyMidiEvent;
    //protected
    //  procedure   SetMidiEnabled( aValue : boolean);
    protected
      procedure SetMidiInDevice( aValue : TMidiInDevice);
      procedure SetMidiOutDevice( aValue : TMidiOutDevice);
    public
{$ENDIF}
      constructor Create( AOwner: TComponent); override;
      destructor  Destroy; override;
{$IFNDEF G2_VST}
      //procedure   OpenMidi;
      //procedure   CloseMidi;
      //procedure   GetInDevices( sl : TStrings);
      //procedure   GetOutDevices( sl : TStrings);
      procedure   ProcessMidiMessage;
      procedure   SysExSend( aSlot: byte);
      procedure   SendMidiShort( aMidiMessage, aData1, aData2: Byte);
      procedure   SendMidiLong( aSysex: Pointer; aMsgLength: Word);
      procedure   DoMidiInput(Sender: TObject);
      procedure   SysExSendPatch( aSlot : byte);
      procedure   SysExSendPerformance;
      procedure   SysExAllControllersRequest;
      procedure   SysExPatchRequestBySlot( aSlot : byte);
      procedure   SysExPerformanceRequest;
      procedure   SysExPatchRequestByFileIndex( aBank, aPatch: byte);
      procedure   SysExPerformanceRequestByFileIndex( aBank, aPerf: byte);

      //property    MidiEnabled : boolean read FMidiEnabled write SetMidiEnabled;
      property    MidiInDevice : TMidiInDevice read FMidiInDevice write SetMidiInDevice;
      property    MidiOutDevice : TMidiOutDevice read FMidiOutDevice write SetMidiOutDevice;
      property    MidiInDeviceName : string read FMidiInDeviceName write FMidiInDeviceName;
      property    MidiOutDeviceName : string read FMidiOutDeviceName write FMidiOutDeviceName;
{$ENDIF}
   end;

{$IFNDEF G2_VST}
procedure InitMidiDevices( MidiInDevices, MidiOutDevices : TMidiDeviceList);
{$ENDIF}

implementation

constructor TG2Midi.Create(AOwner: TComponent);
begin
  inherited;

  FMidiEnabled := False;
{$IFNDEF G2_VST}
  FMidiInDevice := nil;
  FMidiOutDevice := nil;

  FSysExStream := TMemoryStream.Create;

  //FxMidiInput.OnMidiInput := DoMidiInput;
{$ENDIF}
end;

destructor TG2Midi.Destroy;
begin
{$IFNDEF G2_VST}
  FSYsExStream.Free;
{$ENDIF}
  inherited;
end;

{$IFNDEF G2_VST}
function MidiToString(MidiEvent: TMyMidiEvent): string;
var channel, parameter, i : integer;
begin
  if MidiEvent.Sysex = nil then begin
    channel := CHANNEL_MASK and MidiEvent.MidiMessage;
    parameter := MidiEvent.MidiMessage - channel;
    Result := IntToHex(parameter, 2)
            + IntToHex(MidiEvent.Data1, 2)
            + IntToHex(MidiEvent.Data2, 2);
  end else begin
    channel := CHANNEL_MASK and ord(MidiEvent.Sysex[2]);
    parameter := ord(MidiEvent.Sysex[5]);
    Result := IntToHex(ord(MidiEvent.Sysex[0]), 2);
    for i := 1 to MidiEvent.SysexLength - 1 do
      Result := Result + IntToHex(ord(MidiEvent.Sysex[i]), 2);
  end;
end;

procedure TG2Midi.SetMidiInDevice( aValue : TMidiInDevice);
begin
  if aValue <> FMidiInDevice then begin
    if assigned(FMidiInDevice) then begin
      if assigned(FMidiInDevice.MidiInput) then
        FMidiInDevice.MidiInput.OnMidiInput := nil;
      FMidiInDevice.Open := False;
      FMidiInDevice.Assignment := mdatNone;
    end;

    if assigned(aValue) then begin
      aValue.Assignment := mdatSysEx;
      FMidiInDeviceName := aValue.FName;
    end;
    FMidiInDevice := aValue;
  end;
end;

procedure TG2Midi.SetMidiOutDevice( aValue : TMidiOutDevice);
begin
  if aValue <> FMidiOutDevice then begin
    if assigned(FMidiOutDevice) then begin
      FMidiOutDevice.Open := False;
      FMidiOutDevice.Assignment := mdatNone;
    end;

    if assigned(aValue) then begin
      aValue.Assignment := mdatSysEx;
      FMidiOutDeviceName := aValue.FName;
    end;
    FMidiOutDevice := aValue;
  end;
end;

procedure TG2Midi.SysExAllControllersRequest;
var
  SysEx : packed array of byte;
begin
  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $40;
  SysEx[5] := $04;
  SysEx[6] := $F7;

  SendMidiLong(@SysEx[0], 7);
end;

procedure TG2Midi.SysExPatchRequestByFileIndex( aBank, aPatch: byte);
var
  SysEx : packed array of byte;
begin
  if (aBank < 1) or (aBank > 32) then
    raise Exception.Create('Bank must be in the range 1-32.');

  if (aPatch < 1) or (aBank > 128) then
    raise Exception.Create('Patch must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $31;
  SysEx[5] := aBank;
  SysEx[6] := aPatch;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPatchRequestBySlot( aSlot: byte);
var
  SysEx : packed array of byte;
begin
  if (aSlot<0) or (aSlot>3) then
    raise Exception.Create('Slot must be in the range 0-3.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $30;
  SysEx[5] := aSlot;
  SysEx[6] := $00;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPerformanceRequestByFileIndex( aBank, aPerf : byte);
var
  SysEx : packed array of byte;
begin
  if (aBank < 1) or (aBank > 8) then
    raise Exception.Create('Bank must be in the range 1-8.');

  if (aPerf < 1) or (aPerf > 128) then
    raise Exception.Create('Perf must be in the range 1-128.');

  SetLength(SysEx, 10);
  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $39;
  SysEx[5] := aBank;
  SysEx[6] := aPerf;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SysExPerformanceRequest;
var
  SysEx : packed array of byte;
begin
  SetLength(SysEx, 10);

  SysEx[0] := $F0;
  SysEx[1] := $33;
  SysEx[2] := $7F;
  SysEx[3] := $0A;
  SysEx[4] := $38;
  SysEx[5] := $00;
  SysEx[6] := $00;
  SysEx[7] := $F7;

  SendMidiLong(@SysEx[0], 8);
end;

procedure TG2Midi.SendMidiLong( aSysex: Pointer; aMsgLength: Word);
begin
  if not (assigned(FMidiOutDevice) and assigned(FMidiOutDevice.FMidiOutput)) then
    exit;

  with FMidiOutDevice.MidiOutput do begin
    PutLong( aSysex, aMsgLength);
    add_log_line( ProductName + ' <= ' + 'Sysex', LOGCMD_NUL);
  end;
end;

procedure TG2Midi.SendMidiShort( aMidiMessage, aData1, aData2: Byte);
begin
  if not (assigned(FMidiOutDevice) and assigned(FMidiOutDevice.FMidiOutput)) then
    exit;

  with FMidiOutDevice.MidiOutput do begin
    PutShort( aMidiMessage, aData1, aData2);
    add_log_line( ProductName + ' <= ' + IntToHex( aMidiMessage, 2)
                                       + ' ' + IntToHex( aData1, 2)
                                       + ' ' + IntToHex( aData2, 2), LOGCMD_NUL);
  end;
end;

{procedure TG2Midi.SetMidiEnabled(aValue: boolean);
begin
  if aValue then begin
    if FMidiInput.State = misClosed then
      FMidiInput.OpenAndStart;
    if FMidiOutput.State = mosClosed then
      FMidiOutput.Open;
    FMidiEnabled := aValue;
  end else begin
    FMidiEnabled := aValue;
    if FMidiInput.State = misOpen then
      FMidiInput.StopAndClose;
    if FMidiOutput.State = mosOpen then
      FMidiOutput.Close;
  end;
end;}

procedure TG2Midi.SysExSend( aSlot: byte);
var
  SysEx : packed array of byte;
  Buffer : packed array[0..3] of Byte;
  i, j, blok, FCheckSum : integer;
  C : Byte;
  line : string;

begin
  SetLength(SysEx, 4096);
  blok := 0;
  i := 0;

  FSysExStream.Position := 0;
  while (FSysExStream.Position < FSysExStream.Size) do begin
    FSysExStream.Read(C, 1);

    if (i<4) then begin
      Buffer[i] := C;
    end else begin
      if (Buffer[0] = $F0) and (Buffer[1] = $33) and (Buffer[2] = $7F) and (Buffer[3] = $0A) then begin
        // Begin nieuw blok

        if blok <> 0 then begin
          // Send Sysex

          if SysEx[4] = $20 then
            SysEx[6] := aSlot;


          FCheckSum := 0;
          line := '';
          for j := 0 to i - 4 - 1 do begin
            line := line + IntToHex(SysEx[j],2);
            if j = i - 4 - 1 - 1 then
              SysEx[j] := FChecksum
            else
              FChecksum := ( FChecksum + SysEx[j]) And $7f;
          end;

          add_log_line( 'Block : ' + IntToStr(blok) + ' ' + Line, LOGCMD_NUL);

          SendMidiLong( @SysEx[0], i-4);
        end;

        inc(blok);
        i := 4;
      end;

      SysEx[i-4] := Buffer[0];
      for j := 0 to 2 do
        Buffer[j] := Buffer[j+1];
      Buffer[3] := C;
    end;

    inc(i);
  end;

  // Rest buffer

  for j := 0 to 3 do begin
    SysEx[i-4] := Buffer[j];
    inc(i);
  end;

  if SysEx[4] = $20 then
    SysEx[6] := aSlot;

  FCheckSum := 0;
  line := '';
  for j := 0 to i - 4 - 1 do begin
    line := line + IntToHex(SysEx[j],2);
    if j = i - 4 - 1 - 1 then
      SysEx[j] := FChecksum
    else
      FChecksum := ( FChecksum + SysEx[j]) And $7f;
  end;

  add_log_line('Block : ' + IntToStr(blok) + ' ' + Line, LOGCMD_NUL);

  SendMidiLong( @SysEx[0], i-4);
end;

procedure TG2Midi.SysExSendPatch( aSlot : byte);
begin
  FSysExStream.Clear;
  GetSlot( aSlot).Patch.SaveMidiToStream( FSysExStream);
  SysExSend( aSlot);
end;

procedure TG2Midi.SysExSendPerformance;
begin
  FSysExStream.Clear;
  Performance.SaveMidiToStream( FSysExStream);
  SysExSend(0);
end;

procedure TG2Midi.DoMidiInput(Sender: TObject);
begin
  ProcessMidiMessage;
end;

{procedure TG2Midi.GetInDevices(sl: TStrings);
var DevMidiIn : TMidiInput;
    lInCaps: TMidiInCaps;
    i : integer;
begin
  DevMidiIn := TMidiInput.Create(self);
  try
    if DevMidiIn.NumDevs > 0 then begin
      for i := 0 To (DevMidiIn.NumDevs-1) do begin
        midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));
        sl.Add(lInCaps.szPname)
      end;
    end;

  finally
    DevMidiIn.Free;
  end;
end;

procedure TG2Midi.GetOutDevices(sl: TStrings);
var lOutCaps: TMidiOutCaps;
    DevMidiOut : TMidiOutput;
    i : integer;
begin
  DevMidiOut := TMidiOutput.Create(self);
  try
    if DevMidiOut.NumDevs > 0 then begin
      for i := 0 To (DevMidiOut.NumDevs-1) do begin
        midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));
        sl.Add(lOutCaps.szPname);
      end;
    end;
  finally
    DevMidiOut.Free;
  end;
end;}

procedure TG2Midi.ProcessMidiMessage;
var	thisEvent : TMyMidiEvent;
    midistring : string;
    C : Byte;
    block, total, i : integer;
    G2FileDataStream : TG2FileDataStream;
    Lines : TStrings;
begin
  if not (assigned(FMidiInDevice) and assigned(FMidiInDevice.MidiInput)) then
    exit;

  while (FMidiInDevice.MidiInput.MessageCount > 0) do begin

    { Get the event as an object }
    thisEvent := FMidiInDevice.MidiInput.GetMidiEvent;
    try
      if thisEvent.Sysex <> nil then begin

        midistring := MidiToString(thisEvent);
        add_log_line( midistring, LOGCMD_NUL);

        if copy(midistring, 1, 4) = 'F033' then begin

          if (copy(midistring, 7, 6) = '0A2000')   // Patch dump
            or (copy(midistring, 7, 6) = '0A2800') // Perf dump
              then begin

            // Welk blok?
            block := ord(thisEvent.Sysex[8])*256 + ord(thisEvent.Sysex[9]);
            total := ord(thisEvent.Sysex[10])*256 + ord(thisEvent.Sysex[11]);

            add_log_line('Receiving sysex patch, block ' + IntToStr(block) + ' of ' + IntToStr(total), LOGCMD_NUL);

            if block = 0 then begin
              // Eerste blok
              FSysExStream.Clear;
            end;

            for i := 0 to thisEvent.SysexLength - 1 do begin
              C := ord(thisEvent.Sysex[i]);
              FSysExStream.Write(C, 1);
            end;

            if block = total - 1 then begin
              // Laatste blok
              FSysExStream.Position := 0;

              Lines := nil;
              if assigned(LogLines) then
                Lines := LogLines;

              //FSysExStream.SaveToFile('TestSysEx.bin');
              G2FileDataStream := TG2FileDataStream.LoadMidiData( self, FSysExStream, Lines);

              if G2FileDataStream is TG2FilePerformance then
                (Performance as TG2USBPerformance).SendSetPerformanceMessage( '', G2FileDataStream as TG2FilePerformance)
              else
                if G2FileDataStream is TG2FilePatch then
                  (Performance.Slot[ Performance.SelectedSlot] as TG2USBSlot).SendSetPatchMessage( '', G2FileDataStream as TG2FilePatch)
                else
                  raise Exception.Create('Unknown data type');
            end;
          end;
        end;
      end;
    finally
      thisEvent.Free;
    end;
  end;
end;

{ Maybe something for later
function TG2Midi.TranslateMidi(MidiEventIn: TMyMidiEvent): integer;
var channel : byte;
    c : integer;
begin
  if MidiEventIn.Sysex <> nil then
    SendMidiLong(MidiEventIn.Sysex, MidiEventIn.SysexLength)
  else
    if not frmG2EngineControl.ProcessCC( MidiEventIn.MidiMessage, MidiEventIn.Data1, MidiEventIn.Data2) then begin
      if frmG2EngineControl.sbPerf.Down then begin
        val(eGlobalChannel.Text, channel, c);
        channel := channel - 1;
      end else
        channel := GetChannel(frmG2EngineControl.CurrentSlot);
      SendMidiShort((MidiEventIn.MidiMessage and $F0) + channel, MidiEventIn.Data1, MidiEventIn.Data2);
    end;
end;}


{procedure TG2Midi.InitMidi;
var lOutCaps: TMidiOutCaps;
    lInCaps: TMidiInCaps;
    i : integer;
    MidiIn : TMidiInput;
    MidiOut : TMidiOutput;
begin
  MidiIn := TMidiInput.Create(self);
  MidiOut := TMidiOutput.Create(self);
  try

    if MidiOut.NumDevs > 0 then begin
      for i := 0 To (MidiOut.NumDevs-1) do begin
        midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));

        with lvMidiOut.Items.Add do begin
          Caption:= lOutCaps.szPname;
          Data := TMidiOutput.Create(self);
        end;
      end;
    end;

    if MidiIn.NumDevs > 0 then begin
      for i := 0 To (MidiIn.NumDevs-1) do begin
        midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));

        with lvMidiIn.Items.Add do begin
          Caption:= lInCaps.szPname;
          Data := TMidiInput.Create(self);
          TMidiInput(Data).OnMidiInput := MidiInput;
        end;
      end;
    end;

  finally
    MidiOut.Free;
    MidiIn.Free;
  end;
end;}

{procedure TG2Midi.OpenMidi;
begin
  with FMidiInput do begin
     Open;
     Start;
  end;

  with FMidiOutput do begin
    Open;
  end;
end;

procedure TG2Midi.CloseMidi;
begin
 with FMidiInput do begin
   Stop;
   Close;
  end;

  with FMidiOutput do begin
    Close;
  end;
end;}

{$ENDIF}

{ TMidiEditorAssignment }

constructor TMidiEditorAssignment.Create( aG2 : TG2Midi);
begin
  FChannel        := 0;
  FNote           := 0;
  FControlIndex   := 0;
  FCC             := 0;
  FMinValue       := 0;
  FMaxValue       := 0;
  FControl        := nil;
{$IFNDEF G2_VST}
  FMidiOutputList := nil;
{$ENDIF}
  FControlPath    := '';
  FG2 := aG2;
end;

destructor TMidiEditorAssignment.Destroy;
begin
  inherited;
end;

procedure TMidiEditorAssignment.SendValue( aValue : byte);
begin
{$IFNDEF G2_VST}
  if assigned(FMidiOutputList) then begin
    if FNote <> 0 then begin
      if aValue > 0 then begin
        FMidiOutputList.SendValue( $90 + FChannel, FNote, aValue);
        WriteLog( $90 + FChannel, FNote, aValue);
      end else begin
        FMidiOutputList.SendValue( $80 + FChannel, FNote, 0);
        WriteLog( $80 + FChannel, FNote, 0);
      end;
    end else
      if FCC <> 0 then begin
        FMidiOutputList.SendValue( $B0 + FChannel, FCC, aValue);
        WriteLog( $B0 + FChannel, FCC, aValue);
      end;
  end;
{$ENDIF}
end;

procedure TMidiEditorAssignment.WriteLog(aMessage, aData1, aData2: byte);
begin
  if assigned(FG2) then
    FG2.add_log_line('Write midi out : ' + IntToHex(aMessage, 2) + ' ' + IntToHex(aData1,2) + ' ' + IntToHex(aData2,2), LOGCMD_NUL);
end;

{ TMidiEditorAssignmentList }

constructor TMidiEditorAssignmentList.Create(AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TMidiEditorAssignmentList.Destroy;
begin
  inherited;
end;

function TMidiEditorAssignmentList.FindControl( aControl: TMidiAwareControl): TMidiEditorAssignment;
var i : integer;
begin
  i := 0;
  while (i<Count) and (Items[i].FControl <> aControl) do
    inc(i);

  if (i<Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TMidiEditorAssignmentList.FindControlCC(aControl: TMidiAwareControl;
  aChannel, aCC: byte): TMidiEditorAssignment;
var i : integer;
begin
  i := 0;
  while (i<Count) and not((Items[i].FControl = aControl) and (Items[i].FChannel = aChannel) and (Items[i].FCC = aCC)) do
    inc(i);

  if (i<Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TMidiEditorAssignmentList.FindControlHasCC(aControl: TMidiAwareControl): TMidiEditorAssignment;
var i : integer;
begin
  i := 0;
  while (i<Count) and not((Items[i].FControl = aControl) and (Items[i].FCC <> 0)) do
    inc(i);

  if (i<Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TMidiEditorAssignmentList.FindControlHasIndex( aControl: TMidiAwareControl; aIndex : byte): TMidiEditorAssignment;
var i : integer;
begin
  i := 0;
  while (i<Count) and not((Items[i].FControl = aControl) and (Items[i].FControlIndex = aIndex)) do
    inc(i);

  if (i<Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TMidiEditorAssignmentList.FindControlNote(aControl: TMidiAwareControl;  aChannel, aNote: byte): TMidiEditorAssignment;
var i : integer;
begin
  i := 0;
  while (i<Count) and not((Items[i].FControl = aControl) and (Items[i].FChannel = aChannel) and (Items[i].FNote = aNote)) do
    inc(i);

  if (i<Count) then
    Result := Items[i]
  else
    Result := nil;
end;

function TMidiEditorAssignmentList.GetMidiEditorAssignment( aIndex: integer): TMidiEditorAssignment;
begin
  Result := inherited items[aIndex] as  TMidiEditorAssignment;
end;

procedure TMidiEditorAssignmentList.SetMidiEditorAssignment(aIndex: integer; const aValue: TMidiEditorAssignment);
begin
  inherited items[aIndex] := aValue;
end;

{ TMidiAwareControl }

constructor TMidiAwareControl.Create(AOwner: TComponent);
begin
  inherited;

  FMidiEditorAssignmentList := nil;
  FIndexedControl := False;
  FMidiReceiving := False;
  FMidiAware := False;
  FShowMidiBox := False;
end;

destructor TMidiAwareControl.Destroy;
begin

  inherited;
end;

procedure TMidiAwareControl.SetShowMidiBox(aValue: boolean);
begin
  FShowMidiBox := aValue;
  Invalidate;
end;

procedure TMidiAwareControl.SetValueByCtrlMidi(aMidiEditorAssignment : TMidiEditorAssignment; aMidiEvent : TMyMidiEvent);
begin
  // abstract
end;

function TMidiAwareControl.GetValue: byte;
begin
  // abstract
  Result := 0;
end;

function TMidiAwareControl.GetHighValue: byte;
begin
  // abstract
  Result := 0;
end;

function TMidiAwareControl.GetLowValue: byte;
begin
  // abstract
  Result := 0;
end;

function TMidiAwareControl.GetParameter: TG2FileParameter;
begin
  // Abstract
  Result := nil;
end;

function TMidiAwareControl.GetCntrlPath : string;

  function GetParentName( aControl : TControl): string;
  begin
    if assigned(aControl.Parent) then
      Result := GetParentName(aControl.Parent) + '/' + aControl.Parent.Name;
  end;

begin
  // Recursively construct a path traveling up the parent tree
  Result := Name;

  Result := GetParentName(self) + '/' + Result;
end;

procedure TMidiAwareControl.SendCtrlMidiValue;
var i : integer;
    CtrlRange : byte;
    MidiRange : byte;
    MidiValue : byte;
begin
  if FMidiReceiving then
    exit;

  if assigned(FMidiEditorAssignmentList) then
    for i := 0 to FMidiEditorAssignmentList.Count - 1 do begin
      if FMidiEditorAssignmentList[i].FControl = self then begin
        if FIndexedControl then begin
          if FMidiEditorAssignmentList[i].FControlIndex = GetValue then begin
            // Send max value
            FMidiEditorAssignmentList[i].SendValue( FMidiEditorAssignmentList[i].MaxValue);
          end else begin
            // Send min value
            FMidiEditorAssignmentList[i].SendValue( FMidiEditorAssignmentList[i].MinValue);
          end;
        end else begin

          CtrlRange := GetHighValue - GetLowValue;
          MidiRange := FMidiEditorAssignmentList[i].MaxValue - FMidiEditorAssignmentList[i].MinValue;

          if CtrlRange <> 0 then begin
            MidiValue := trunc(((GetValue - GetLowValue)/CtrlRange) * MidiRange + FMidiEditorAssignmentList[i].MinValue);
            FMidiEditorAssignmentList[i].SendValue( MidiValue);
          end else
            FMidiEditorAssignmentList[i].SendValue( 0);
        end;
      end;
    end;
end;

{$IFNDEF G2_VST}

{ TMidiDevice }

constructor TMidiDevice.Create;
begin
  FOpen := False;
  FAssignment := mdatNone;
end;

destructor TMidiDevice.Destroy;
begin
  inherited;
end;

procedure TMidiDevice.SetOpen( aValue: Boolean);
begin
  FOpen := aValue;
end;

procedure TMidiDevice.SetAssignment( aValue : TMidiDeviceAssignmentType);
begin
  if aValue <> FAssignment then begin
    case aValue of
    mdatNone :
      begin
        FAssignMent := aValue;
        Open := False;
      end;
    mdatSysex,
    mdatCtrl :
      if not Open then begin
        FAssignment := aValue;
        Open := True;
        if not Open then
          FAssignment := mdatNone;
      end else
        FAssignment := aValue;
    end;
  end;
end;


{ TMidiInDevice }

constructor TMidiInDevice.Create;
begin
  inherited;
  FMidiInput := TMidiInput.Create(nil);
end;

destructor TMidiInDevice.Destroy;
begin
  FMidiInput.Free;
  inherited;
end;

procedure TMidiInDevice.SetOpen(Value: Boolean);
begin
  if assigned(FMidiInput) then begin
    if Value = True then begin
      if FMidiInput.State = misClosed then begin
        try
          FMidiInput.Open;
          FMidiInput.Start;
          FOpen := True;
        except
          FOpen := False;
        end;
      end;
    end else begin
      FMidiInput.StopAndClose;
      FOpen := False;
    end;
  end else
    FOpen := False;
end;

{ TMidiOutDevice }

constructor TMidiOutDevice.Create;
begin
  inherited;
  FMidiOutput := TMidiOutput.Create(nil);
end;

destructor TMidiOutDevice.Destroy;
begin
  FMidiOutput.Free;
  inherited;
end;

procedure TMidiOutDevice.SetOpen(Value: Boolean);
begin
  if assigned(FMidiOutput) then begin
    if Value = True then begin
      if FMidiOutput.State = mosClosed then begin
        try
          FMidiOutput.Open;
          FOpen := True;
        except
          FOpen := False;
        end;
      end;
    end else begin
      FMidiOutput.Close;
      FOpen := False;
    end;
  end else
    FOpen := False;

  if assigned(FOnMidiDeviceStateChange) then
    FOnMidiDeviceStateChange(self);
end;

{ TMidiDeviceList }

constructor TMidiDeviceList.Create(AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TMidiDeviceList.Destroy;
begin

  inherited;
end;

function TMidiDeviceList.Find(aMidiDevice: TMidiDevice): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(Items[i] = aMidiDevice) do
    inc(i);

  if (i < Count) then
    Result := i
  else
    Result := -1;
end;

function TMidiDeviceList.GetMidiDevice(aIndex: integer): TMidiDevice;
begin
  Result := inherited items[aIndex] as TMidiDevice
end;

procedure TMidiDeviceList.SetMidiDevice(aIndex: integer;
  const aValue: TMidiDevice);
begin
  inherited items[aIndex] := aValue;
end;

{ TMidiOutputList }

constructor TMidiOutputList.Create(AOwnsObjects: Boolean);
begin
  inherited;
end;

destructor TMidiOutputList.Destroy;
begin
  inherited;
end;

function TMidiOutputList.Find(aMidiOutput: TMidiOutput): integer;
var i : integer;
begin
  i := 0;
  while (i < Count) and not(Items[i] = aMidiOutput) do
    inc(i);

  if (i < Count) then
    Result := i
  else
    Result := -1;
end;

function TMidiOutputList.GetMidiOutput(aIndex: integer): TMidiOutput;
begin
  Result := inherited items[aIndex] as TMidiOutput
end;

procedure TMidiOutputList.SetMidiOutput(aIndex: integer; const aValue: TMidiOutput);
begin
  inherited items[aIndex] := aValue;
end;

procedure TMidiOutputList.SendValue( aMidiMessage, aData1, aData2 : byte);
var i : integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].State = mosOpen then
      Items[i].PutShort( aMidiMessage, aData1, aData2);
end;

procedure InitMidiDevices( MidiInDevices, MidiOutDevices : TMidiDeviceList);
var lOutCaps: TMidiOutCaps;
    lInCaps: TMidiInCaps;
    i, d : integer;
    MidiInput : TMidiInput;
    MidiOutput : TMidiOutput;
    MidiInDevice : TMidiInDevice;
    MidiOutDevice : TMidiOutDevice;
begin
  MidiInput := TMidiInput.Create(nil);
  midiOutput := TMidiOutput.Create(nil);
  try
    MidiOutDevices.Clear;
    if midiOutput.NumDevs > 0 then begin
      for i := 0 To (midiOutput.NumDevs-1) do begin
        midiOutGetDevCaps(i, @lOutCaps, SizeOf(TMidiOutCaps));

        MidiOutDevice := TMidiOutDevice.Create;
        MidiOutDevice.Name := lOutCaps.szPname;
        MidiOutDevice.MidiOutput.DeviceID := i;
        MidiOutDevices.Add(MidiOutDevice);
      end;
    end;

    MidiInDevices.Clear;
    if MidiInput.NumDevs > 0 then begin
      for i := 0 To (MidiInput.NumDevs-1) do begin
        midiInGetDevCaps(i, @lInCaps, SizeOf(TMidiInCaps));

        MidiInDevice := TMidiInDevice.Create;
        MidiInDevice.Name := lInCaps.szPname;
        MidiInDevice.MidiInput.DeviceID := i;
        //TODO !MidiInDevice.MidiInput.OnMidiInput := frmMidiMapping.DoCtrlMidiInput;
        MidiInDevices.Add(MidiInDevice);
      end;
    end;

  finally
    midiOutput.Free;
    MidiInput.Free;
  end;
end;
{$ENDIF}

end.
