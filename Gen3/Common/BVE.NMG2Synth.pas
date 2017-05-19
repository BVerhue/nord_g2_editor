unit BVE.NMG2Synth;

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
//  Parts ported from https://github.com/msg/g2ools
//  Parts used from http://www.iaf.nl/Users/BlueHell/
//
//  ////////////////////////////////////////////////////////////////////////////

// 2011-11-14 bve : Performance chunk, first G2 gives Slotno 1..4, second G2 gives 5..8..?

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.math,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2Stream;

type
  TG2Synth = class;

  TBankItem = class(TInterfacedObject, IG2BankItem)
  private
    FCmd       : byte;
    FPatchFileType : TPatchFileType;
    FBank      : byte;
    FPatch     : byte;
    FCategory  : byte;
    FPatchName : string;

    function GetCmd : byte;
    function GetPatchFileType : TPatchFileType;
    function GetBank : byte;
    function GetPatch : byte;
    function GetCategory : byte;
    function GetPatchName : string;

    procedure SetCmd( const aValue : byte);
    procedure SetPatchFileType( const aValue : TPatchFileType);
    procedure SetBank( const aValue : byte);
    procedure SetPatch( const aValue : byte);
    procedure SetCategory( const aValue : byte);
    procedure SetPatchName( const aValue : string);
  end;

  TBankList = class(TList<IG2BankItem>)
  public
    constructor Create( AOwnsObjects : boolean{; aG2 : TG2Synth});
    destructor  Destroy; override;
    function    FindNext( aPatchFileType : TPatchFileType; aBank, aPatch : byte): IG2BankItem;
    function    FindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;
  end;

  TG2MidiToKnob = class(TInterfacedObject, IG2MidiToKnob)
  private
    FSource : TG2ParamSource; // 0=Param 1=Global 2=Morph 3=Patch
    FControlType : TG2ControlType; // 0=Knob 1=Button
    FKnob : integer;
    FController : integer;
  protected
    function GetController: integer;
    function GetControlType: TG2ControlType;
    function GetKnob: integer;
    function GetSource: TG2ParamSource;
    procedure SetController(const Value: integer);
    procedure SetControlType(const Value: TG2ControlType);
    procedure SetKnob(const Value: integer);
    procedure SetSource(const Value: TG2ParamSource);
  public
    constructor Create(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob, aController: integer);
  end;

  TG2Synth = class(TInterfacedObject, IG2Synth)
  private
    [Weak] FWConnection   : IG2Connection;

    FClientType           : TClientType;

    FBankList             : TBankList;
    FInitStep             : integer;
    FNextBankListCmd      : TNextBankListCmd;

    FInitialized          : boolean;

    FBankDumpFolder       : string;
    FBankDumpFileName     : string;
    FBankDumpList         : TStringList;
    FBankDumpListIndex    : integer;
    FBankDumpDestBank     : byte;

    FslBanks : TStringList;

    // Synth settings
    FSynthName            : string;
    FPerfMode             : TBits1;
    FPerfBank             : TBits8;
    FPerfLocation         : Tbits8;
    FMemoryProtect        : TBits1;
    FMidiChannelA         : TBits8; // 16 : Inactive
    FMidiChannelB         : TBits8; // 16 : Inactive
    FMidiChannelC         : TBits8; // 16 : Inactive
    FMidiChannelD         : TBits8; // 16 : Inactive
    FMidiGlobalChannel    : TBits8; // 16 : Off
    FSysExID              : TBits8; // 16 : All
    FLocalOn              : TBits1;
    FProgramChangeReceive : TBits1;
    FProgramChangeSend    : TBits1;
    FControllersReceive   : TBits1;
    FControllersSend      : TBits1;
    FSendClock            : TBits1;
    FIgnoreExternalClock  : TBits1;
    FTuneCent             : TBits8; // -100..100 (9C..64)
    FGlobalOctaveShiftActive : TBits1;
    FGlobalOctaveShift    : TBits8; // -2..2 (FE..02)
    FTuneSemi             : TBits8; // -6..6
    FPedalPolarity        : TBits1; // Open 0, Closed 1
    FControlPedalGain     : TBits8; // 0..32

    FUnknown              : byte;   // Unknown data

    FPerf : IG2Perf;

    FAutoAssignMidi : boolean; // Automatically assigns midi on initialisation
    FMidiToKnobList : TList<IG2MidiToKnob>;

    procedure SetSynthName( aValue : string);
    procedure SetPerfMode( aValue : TBits1); virtual;
    procedure SetMemoryProtect( aValue : TBits1);
    procedure SetMidiChannelA( aValue : TBits8);
    procedure SetMidiChannelB( aValue : TBits8);
    procedure SetMidiChannelC( aValue : TBits8);
    procedure SetMidiChannelD( aValue : TBits8);
    procedure SetMidiGlobalChannel( aValue : TBits8);
    procedure SetSysExID( aValue : TBits8);
    procedure SetLocalOn( aValue :  TBits1);
    procedure SetProgramChangeReceive( aValue : TBits1);
    procedure SetProgramChangeSend( aValue : TBits1);
    procedure SetControllersReceive( aValue : TBits1);
    procedure SetControllersSend( aValue : TBits1);
    procedure SetSendClock( aValue : TBits1);
    procedure SetIgnoreExternalClock( aValue : TBits1);
    procedure SetTuneCent( aValue : TBits8);
    procedure SetGlobalOctaveShiftActive( aValue : TBits1);
    procedure SetGlobalOctaveShift( aValue : TBits8);
    procedure SetTuneSemi( aValue : TBits8);
    procedure SetPedalPolarity( aValue : TBits1);
    procedure SetControlPedalGain( aValue : TBits8);

    procedure SetSelectedSlotIndex(const aValue : TBits2);

    function GetSynthName : string;
    function GetPerfMode : TBits1;
    function GetMemoryProtect : TBits1;
    function GetMidiChannelA : TBits8;
    function GetMidiChannelB : TBits8;
    function GetMidiChannelC : TBits8;
    function GetMidiChannelD : TBits8;
    function GetMidiGlobalChannel : TBits8;
    function GetSysExID : TBits8;
    function GetLocalOn :  TBits1;
    function GetProgramChangeReceive : TBits1;
    function GetProgramChangeSend : TBits1;
    function GetControllersReceive : TBits1;
    function GetControllersSend : TBits1;
    function GetSendClock : TBits1;
    function GetIgnoreExternalClock : TBits1;
    function GetTuneCent : TBits8;
    function GetGlobalOctaveShiftActive : TBits1;
    function GetGlobalOctaveShift : TBits8;
    function GetTuneSemi : TBits8;
    function GetPedalPolarity : TBits1;
    function GetControlPedalGain : TBits8;

    function GetSelectedSlot : IG2Slot;
    function GetSelectedSlotIndex : TBits2;

    function GetBankItem( const aIndex : integer) : IG2BankItem;
    function GetBankItemCount: integer;
    function GetPerf : IG2Perf;
    function GetClientType : TClientType;
    function GetAutoAssignMidi : boolean;
    function GetMidiToKnobList : TList<IG2MidiToKnob>;

    function GetBankDumpFolder : string;
    function GetBankDumpFileName : string;
    function GetBankDumpList : TStringList;
    function GetBankDumpListIndex : integer;
    function GetBankDumpDestBank : byte;

    function GetNextBankListCmd : TNextBankListCmd;
    procedure SetNextBankListCmd( const aValue : TNextBankListCmd);

    procedure SetBankDumpFolder( aValue : string);
    procedure SetBankDumpFileName( aValue : string);
    procedure SetBankDumpListIndex( aValue : integer);
    procedure SetBankDumpDestBank( aValue : byte);

    procedure SetBankItem( const aIndex : integer; const aValue : IG2BankItem);
    procedure SetClientType( const aValue : TClientType);
    procedure SetAutoAssignMidi( const aValue : boolean);
  protected
    function GetID : integer; virtual;
  public
    constructor Create(aConnection: IG2Connection);
    destructor Destroy; override;

    procedure Init;

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure add_log_line(tekst: string; log_cmd: integer);
    procedure DoError;

    // For auto assign midi to knobs
    procedure AddMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer; const aMidiCC : byte);
    procedure DeleteMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer);
    function FindMidiToKnob(const aSource: TG2ParamSource; const aControlType: TG2ControlType; const aKnob: integer): integer;
    function MidiToKnobCheckCC(const aMidiCC: byte): boolean;

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure AddMsgInit( aMsg : TG2SendMessage);
    procedure AddMsgStartStopCommunication( aMsg : TG2SendMessage; const Stop : byte);
    procedure AddMsgGetPatchVersion( aMsg : TG2SendMessage);
    procedure AddMsgGetAssignedVoices( aMsg : TG2SendMessage);
    procedure AddMsgGetSynthSettings( aMsg : TG2SendMessage);
    procedure AddMsgUnknown1Message( aMsg : TG2SendMessage);
    procedure AddMsgMidiDumpMessage( aMsg : TG2SendMessage);
    procedure AddMsgListMessage( aMsg : TG2SendMessage; const aPatchFileType : TPatchFileType; const aBank, aPatch : byte);
    procedure AddMsgSetModeMessage( aMsg : TG2SendMessage; aMode : byte);
    procedure AddMsgNoteMessage( aMsg : TG2SendMessage; aNote : byte; aOnoff : byte);
    procedure AddMsgGetMasterClockMessage( aMsg : TG2SendMessage);
    procedure AddMsgUploadBankMessage( aMsg : TG2SendMessage; aPatchFileType: TPatchFileType; aBank, aLocation: byte);
    procedure AddMsgDownloadPatchBankMessage( aMsg : TG2SendMessage; aBank, aLocation: byte; aPatchName: string; aPatch: IG2Patch);
    procedure AddMsgDownloadPerfBankMessage( aMsg : TG2SendMessage; aBank, aLocation: byte; aPerfName: string; aPerf: IG2Perf);
    procedure AddMsgRetrieveMessage( aMsg : TG2SendMessage; aSlot, aBank, aPatch : byte);
    procedure AddMsgStoreMessage( aMsg : TG2SendMessage; aSlot, aBank, aPatch : byte);
    procedure AddMsgClearBankMessage( aMsg : TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aFromLocation, aToLocation : byte);
    procedure AddMsgClearMessage( aMsg : TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aPatch : byte);
    procedure AddMsgSetSynthSettings( aMsg : TG2Message);
    procedure AddMsgSelGlobalParamPage( aMsg : TG2Message; aPageIndex : byte);
    procedure AddMsgSetPerformance( aMsg : TG2SendMessage; aPerfName : string; aPerf : IG2Perf);

    procedure DoSynthSettingsUpdate;
    procedure DoAfterBankList;
    procedure DoAfterPerfNameChange;
    procedure DoAfterStartStopCommunication;
    procedure DoAfterStore( SlotIndex, BankIndex, PatchIndex : byte);
    procedure DoAfterClear( PatchFileType : TPatchFileType; BankIndex, PatchIndex : byte);
    procedure DoAfterClearBank( PatchFileType : TPatchFileType; BankIndex : byte);
    procedure DoAfterG2Init;

    function ProcessResponseMsg( aSender : TObject; aMsg: TG2Message): boolean;
    function ProcessSendMsg( aSender : TObject; aMsg: TG2Message): boolean;

    procedure ProcessMsgSynthSettings( aMsg: TG2Message);
    procedure ProcessMsgListAddNames( aMsg: TG2Message);
    procedure ProcessMsgAfterStore( aMsg: TG2Message);
    procedure ProcessMsgAfterClear( aMsg: TG2Message);
    procedure ProcessMsgAfterClearBank( aMsg: TG2Message);
    procedure ProcessMsgAfterBankUpload( aMsg: TG2Message);
    procedure ProcessMsgPatchBankData( aMsg: TG2Message);

    procedure BankListClear;
    procedure AddBankItem( aBankItem : IG2BankItem);
    function FindBankItem( aPatchFileType : TPatchFileType; aBank, aPatch : byte): IG2BankItem;
    procedure DeleteBankItem( aPatchFileType : TPatchFileType; aBank, aPatch : byte);
    function BankFindNext: IG2BankItem;
    function BankFindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;

    procedure StartInit(aConnection: IG2Connection);
    procedure InitSeq(Sender: TObject; aConnection: IG2Connection);

    function TextFunction( No : integer; Param1, Param2, Param3 : byte): string;

    function G2MessageDlg( tekst : string; code : integer): integer; virtual; // For sending messages to UI

    property Perf: IG2Perf read GetPerf;
  end;

implementation
uses
  BVE.NMG2Patch,
  BVE.NMG2Perf,
  BVE.NMG2Slot;


//------------------------------------------------------------------------------
//
//                                TBankItem
//
//------------------------------------------------------------------------------

function TBankItem.GetBank: byte;
begin
  Result := FBank;
end;

function TBankItem.GetCategory: byte;
begin
  Result := FCategory;
end;

function TBankItem.GetCmd: byte;
begin
  Result := FCmd;
end;

function TBankItem.GetPatch: byte;
begin
  Result := FPatch;
end;

function TBankItem.GetPatchFileType: TPatchFileType;
begin
  Result := FPatchFileType;
end;

function TBankItem.GetPatchName: string;
begin
  Result := FPatchName;
end;

procedure TBankItem.SetBank(const aValue: byte);
begin
  FBank := aValue;
end;

procedure TBankItem.SetCategory(const aValue: byte);
begin
  FCategory := aValue;
end;

procedure TBankItem.SetCmd(const aValue: byte);
begin
  FCmd := aValue;
end;

procedure TBankItem.SetPatch(const aValue: byte);
begin
  FPatch := aValue;
end;

procedure TBankItem.SetPatchFileType(const aValue: TPatchFileType);
begin
  FPatchFileType := aValue;
end;

procedure TBankItem.SetPatchName(const aValue: string);
begin
  FPatchName := aValue;
end;

//------------------------------------------------------------------------------
//
//                                TBankList
//
//------------------------------------------------------------------------------

constructor TBankList.Create( AOwnsObjects : boolean{; aG2 : TG2Synth});
begin
  inherited Create;
end;

destructor TBankList.Destroy;
begin
  inherited;
end;

function TBankList.FindNext(aPatchFileType: TPatchFileType; aBank, aPatch: byte): IG2BankItem;
var i : integer;
begin
  // Find bank
  i := 0;
  while (i < Count) and not((Items[i].PatchFileType = aPatchFileType)
                        and (Items[i].Bank = aBank)) do
    inc(i);

  if (i < Count) then begin
    while (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)
                       and (Items[i].Patch <= aPatch)) do
      inc(i);

    if (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)) then
      Result := Items[i]
    else
      Result := nil;

  end else
    Result := nil;
end;

function TBankList.FindFirstLast( aPatchFileType : TPatchFileType; aBank : byte; var aFirstLocation, aLastLocation : byte): boolean;
var i : integer;
begin
  Result := False;

  // Find bank
  i := 0;
  while (i < Count) and not((Items[i].PatchFileType = aPatchFileType)
                        and (Items[i].Bank = aBank)) do
    inc(i);

  if (i < Count) then begin
    aFirstLocation := Items[i].Patch;
    aLastLocation := aFirstLocation;

    inc(i);
    while (i < Count) and ((Items[i].PatchFileType = aPatchFileType)
                       and (Items[i].Bank = aBank)) do begin
      aLastLocation := Items[i].Patch;
      inc(i);
    end;
    Result := True;
  end;
end;

//------------------------------------------------------------------------------
//
//                                 TG2Synth
//
//------------------------------------------------------------------------------

constructor TG2Synth.Create(aConnection: IG2Connection);
begin
  inherited Create;

  SetWeak(@FWConnection, aConnection);

  FslBanks := TStringList.Create;

  FPerf := TG2Perf.Create(FWConnection);

  FBankList := TBankList.Create( True);
  FBankDumpFolder := '';
  FBankDumpFileName := '';
  FBankDumpList := TStringList.Create;

  FMidiToKnobList := TList<IG2MidiToKnob>.Create;

  FInitialized := False;
end;

destructor TG2Synth.Destroy;
begin
  FMidiToKnobList.Free;
  FBankDumpList.Free;

  FBankList.Free;
  FslBanks.Free;

  SetWeak(@FWConnection, nil);

  inherited;
end;

procedure TG2Synth.Init;
begin
  FSynthName := 'G2 Engine';
  FMemoryProtect := 0;
  FMidiChannelA := 0;
  FMidiChannelB := 1;
  FMidiChannelC := 2;
  FMidiChannelD := 3;
  FMidiGlobalChannel := 15;
  FSysExID := 16;
  FLocalOn := 0;
  FProgramChangeReceive := 1;
  FProgramChangeSend := 0;
  FControllersReceive := 1;
  FControllersSend := 1;
  FSendClock := 0;
  FIgnoreExternalClock := 0;
  FTuneCent := 0;
  FGlobalOctaveShiftActive := 0;
  FGlobalOctaveShift := 0;
  FTuneSemi := 0;
  FPedalPolarity := 0;
  FControlPedalGain := 0;

  FPerf.Init;
end;

procedure TG2Synth.DoError;
begin
  NotifyObservers(EvtError, self as IG2Synth);
end;

procedure TG2Synth.DoAfterBankList;
begin
  NotifyObservers(EvtAfterBankList, self as IG2Synth);
end;

procedure TG2Synth.DoAfterClear(PatchFileType : TPatchFileType; BankIndex, PatchIndex : byte);
begin
  NotifyObservers(EvtAfterClear, self as IG2Synth);
end;

procedure TG2Synth.DoAfterClearBank(PatchFileType: TPatchFileType;
  BankIndex: byte);
begin
  NotifyObservers(EvtAfterClearBank, self as IG2Synth);
end;

procedure TG2Synth.DoAfterG2Init;
begin
  NotifyObservers(EvtAfterG2Init, self as IG2Synth);
end;

procedure TG2Synth.DoAfterPerfNameChange;
begin
  NotifyObservers(EvtAfterPerfNameChange, self as IG2Synth);
end;

procedure TG2Synth.DoAfterStartStopCommunication;
begin
  NotifyObservers(EvtAfterStartStopCommunication, self as IG2Synth);
end;

procedure TG2Synth.DoAfterStore( SlotIndex, BankIndex, PatchIndex : byte);
begin
  NotifyObservers(EvtAfterStore, self as IG2Synth);
end;

procedure TG2Synth.DoSynthSettingsUpdate;
begin
  NotifyObservers(EvtSynthSettingsUpdate, self as IG2Synth);
end;

procedure TG2Synth.NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);
begin
  FWConnection.NotifyObservers(aG2Event, aG2Object);
end;

procedure TG2Synth.InitMsg( aMsg : TG2SendMessage);
begin
  aMsg.Init;

  aMsg.WriteMessage( $01);
  aMsg.WriteMessage( CMD_REQ + CMD_SYS );
  aMsg.WriteMessage( $41);
end;

procedure TG2Synth.InitUndoMsg( aUndoMsg : TG2SendMessage);
begin
  aUndoMsg.Init;

  aUndoMsg.AddReversed := True;
  aUndoMsg.Offset := 5;

  aUndoMsg.WriteMessage( $01);
  aUndoMsg.WriteMessage( CMD_REQ + CMD_SYS );
  aUndoMsg.WriteMessage( $41);
end;

procedure TG2Synth.AddMsgInit( aMsg : TG2SendMessage);
begin
  add_log_line('Init', LOGCMD_HDR);

  aMsg.Init;
  aMsg.WriteMessage( CMD_INIT);
end;

procedure TG2Synth.AddMsgStartStopCommunication( aMsg : TG2SendMessage; const Stop : byte);
begin
  if stop = 0 then
    add_log_line('Start communication', LOGCMD_HDR)
  else
    add_log_line('Stop communication', LOGCMD_HDR);

  // Start or stop the message stream comming from the G2

  aMsg.WriteMessage( S_START_STOP_COM);
  aMsg.WriteMessage( Stop); //h00 = start, h01 = stop
end;

procedure TG2Synth.AddMsgGetPatchVersion( aMsg : TG2SendMessage);
begin
  add_log_line('Get patch version, slot $04', LOGCMD_HDR);

  aMsg.WriteMessage( Q_VERSION_CNT);
  aMsg.WriteMessage( $04);
end;

procedure TG2Synth.AddMsgGetAssignedVoices( aMsg : TG2SendMessage);
begin
  add_log_line('Get assigned voices', LOGCMD_HDR);

  aMsg.WriteMessage( Q_ASSIGNED_VOICES);
end;

procedure TG2Synth.AddMsgGetSynthSettings( aMsg : TG2SendMessage);
begin
  add_log_line('Synth settings', LOGCMD_HDR);

  aMsg.WriteMessage( Q_SYNTH_SETTINGS);
end;

procedure TG2Synth.AddMsgUnknown1Message( aMsg : TG2SendMessage);
begin
  add_log_line('Unknown 1', LOGCMD_HDR);

  aMsg.WriteMessage( M_UNKNOWN_1);
end;

procedure TG2Synth.AddMsgMidiDumpMessage( aMsg : TG2SendMessage);
begin
  add_log_line('Midi dump', LOGCMD_HDR);

  aMsg.WriteMessage( S_MIDI_DUMP);
end;

procedure TG2Synth.AddMsgListMessage( aMsg : TG2SendMessage; const aPatchFileType : TPatchFileType; const aBank, aPatch : byte);
begin
  add_log_line('List', LOGCMD_HDR);

  aMsg.WriteMessage( Q_LIST_NAMES);
  aMsg.WriteMessage( ord(aPatchFileType));
  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aPatch);
end;

procedure TG2Synth.AddMsgSetModeMessage( aMsg : TG2SendMessage; aMode : byte);
begin
  add_log_line('Set mode', LOGCMD_HDR);

  aMsg.WriteMessage( S_SET_PARAM_MODE);
  aMsg.WriteMessage( aMode); // $00 perf $01 patch
  aMsg.WriteMessage( $00);
end;

procedure TG2Synth.AddMsgNoteMessage( aMsg : TG2SendMessage; aNote : byte; aOnoff : byte);
begin
  add_log_line('Play note', LOGCMD_HDR);

  aMsg.WriteMessage( S_PLAY_NOTE);
  aMsg.WriteMessage( aOnOff); // $00 on $01 off
  aMsg.WriteMessage( aNote);
end;

procedure TG2Synth.AddMsgGetMasterClockMessage( aMsg : TG2SendMessage);
begin
  add_log_line('Get master clock', LOGCMD_HDR);

  aMsg.WriteMessage( Q_MASTER_CLOCK);
end;

procedure TG2Synth.AddMsgUploadBankMessage( aMsg : TG2SendMessage; aPatchFileType: TPatchFileType; aBank, aLocation: byte);
begin
  add_log_line('Upload bank patch from G2 synth to disk', LOGCMD_HDR);

  aMsg.WriteMessage( S_PATCH_BANK_UPLOAD);
  aMsg.WriteMessage( ord(aPatchFileType));
  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aLocation);
end;

procedure TG2Synth.AddMsgDownloadPatchBankMessage( aMsg : TG2SendMessage; aBank,
  aLocation: byte; aPatchName: string; aPatch: IG2Patch);
var Chunk : TPatchChunk;
    MemStream : TMemoryStream;
    Size : integer;
begin
  add_log_line('Download patch from disk to G2 synth bank', LOGCMD_HDR);

  MemStream := TMemoryStream.Create;
  try
    Chunk := TPatchChunk.Create( MemStream);
    try
      aMsg.WriteMessage( S_PATCH_BANK_DATA);
      aMsg.WriteMessage( ord(pftPatch));
      aMsg.WriteMessage( aBank);
      aMsg.WriteMessage( aLocation);

      aMsg.WriteClaviaString( aPatchName);

      Chunk.WriteBits( FWConnection.Slot[aPatch.SlotIndex].PatchVersion, 8);
      Chunk.WriteBits(ord(pftPatch), 8);
      Chunk.Flush;
      aPatch.Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
      Chunk.WriteCrc(MemStream);

      Size := MemStream.Size + 1; // ?
      aMsg.WriteMessage( Size div 256);
      aMsg.WriteMessage( Size mod 256);
      aMsg.WriteMessage( PATCH_VERSION);
      aMsg.WriteMessage( ord(pftPatch));  // Always 0?
      aMsg.Write( MemStream.Memory^, MemStream.Size);

    finally
      Chunk.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TG2Synth.AddMsgDownloadPerfBankMessage( aMsg : TG2SendMessage; aBank,
  aLocation: byte; aPerfName: string; aPerf: IG2Perf);
var Chunk : TPatchChunk;
    MemStream : TMemoryStream;
    Size : integer;
begin
  add_log_line('Download performance from disk to G2 synth bank', LOGCMD_HDR);

  MemStream := TMemoryStream.Create;
  try
    Chunk := TPatchChunk.Create( MemStream);
    try
      aMsg.WriteMessage( S_PATCH_BANK_DATA);
      aMsg.WriteMessage( ord(pftPerf));
      aMsg.WriteMessage( aBank);
      aMsg.WriteMessage( aLocation);

      aMsg.WriteClaviaString( aPerfName);

      Chunk.WriteBits( aPerf.PerfVersion, 8);
      Chunk.WriteBits(ord(pftPerf), 8);
      Chunk.Flush;
      aPerf.WriteSettings(Chunk);
      aPerf.Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
      Chunk.WriteCrc( MemStream);

      Size := MemStream.Size + 1; // ?
      aMsg.WriteMessage( Size div 256);
      aMsg.WriteMessage( Size mod 256);
      aMsg.WriteMessage( PATCH_VERSION);
      //aMsg.WriteMessage( ord(pftPerf));
      aMsg.WriteMessage( ord(pftPatch)); // Always 0?
      aMsg.Write( MemStream.Memory^, MemStream.Size);

    finally
      Chunk.Free;
    end;
  finally
    MemStream.Free;
  end;
end;

procedure TG2Synth.AddMsgRetrieveMessage( aMsg : TG2SendMessage; aSlot, aBank, aPatch : byte);
begin
  if aSlot = 4 then
    add_log_line('Retrieve performance, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR)
  else
    add_log_line('Retreve patch, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR);

  aMsg.WriteMessage( S_RETREIVE);
  aMsg.WriteMessage( aSlot);
  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aPatch);
end;

procedure TG2Synth.AddMsgStoreMessage( aMsg : TG2SendMessage; aSlot, aBank, aPatch : byte);
begin
  if aSlot = 4 then
    add_log_line('Save performance, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR)
  else
    add_log_line('Save patch, slot ' + IntToStr(aSlot) + ', bank ' + IntToStr(aBank) + ', patch ' + IntToStr(aPatch), LOGCMD_HDR);

  aMsg.WriteMessage( S_STORE);
  aMsg.WriteMessage( aSlot);
  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aPatch);
end;

procedure TG2Synth.AddMsgClearBankMessage( aMsg : TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aFromLocation, aToLocation : byte);
begin
   add_log_line('Clear bank', LOGCMD_HDR);

  aMsg.WriteMessage( S_CLEAR_BANK);
  aMsg.WriteMessage( ord(aPatchFileType)); // $00 - patch - $01 - perf

  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aFromLocation);

  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aToLocation);

  aMsg.WriteMessage( $00); // Unknown
end;

procedure TG2Synth.AddMsgClearMessage( aMsg : TG2SendMessage; aPatchFileType : TPatchFileType; aBank, aPatch : byte);
begin
  add_log_line('Clear bank location', LOGCMD_HDR);

  aMsg.WriteMessage( S_CLEAR);
  aMsg.WriteMessage( ord(aPatchFileType)); // $00 - patch - $01 - perf
  aMsg.WriteMessage( aBank);
  aMsg.WriteMessage( aPatch);
  aMsg.WriteMessage( $00); // Unknown
end;

procedure TG2Synth.AddMsgSelGlobalParamPage( aMsg : TG2Message; aPageIndex: byte);
begin
  add_log_line('Select global parameter page', LOGCMD_HDR);

  aMsg.WriteMessage( S_SEL_GLOBAL_PAGE);
  aMsg.WriteMessage( aPageIndex);
end;

procedure TG2Synth.AddMsgSetSynthSettings( aMsg : TG2Message);
var BitWriter : TBitWriter;
begin
  add_log_line('Set synth settings', LOGCMD_HDR);

  aMsg.WriteMessage( S_SYNTH_SETTINGS);

  BitWriter := TBitWriter.Create;
  try
    WriteClaviaString( aMsg, FSynthName);
    aMsg.WriteMessage( $80);
    aMsg.WriteMessage( $00);
    //aMsg.WriteMessage( $00);
    BitWriter.WriteBits( aMsg, FPerfBank, 8);
    //aMsg.WriteMessage( $00);
    BitWriter.WriteBits( aMsg, FPerfLocation, 8);
    BitWriter.WriteBits( aMsg, FMemoryProtect, 1);
    BitWriter.WriteBits( aMsg, $00,            7);
    BitWriter.WriteBits( aMsg, FMidiChannelA, 8);
    BitWriter.WriteBits( aMsg, FMidiChannelB, 8);
    BitWriter.WriteBits( aMsg, FMidiChannelC, 8);
    BitWriter.WriteBits( aMsg, FMidiChannelD, 8);
    BitWriter.WriteBits( aMsg, FMidiGlobalChannel, 8);
    BitWriter.WriteBits( aMsg, FSysExID, 8);
    BitWriter.WriteBits( aMsg, FLocalOn, 1);
    BitWriter.WriteBits( aMsg, $00,      7);
    BitWriter.WriteBits( aMsg, $00,                   6);
    BitWriter.WriteBits( aMsg, FProgramChangeReceive, 1);
    BitWriter.WriteBits( aMsg, FProgramChangeSend,    1);
    BitWriter.WriteBits( aMsg, $00,                 6);
    BitWriter.WriteBits( aMsg, FControllersReceive, 1);
    BitWriter.WriteBits( aMsg, FControllersSend,    1);
    BitWriter.WriteBits( aMsg, $00,                  1);
    BitWriter.WriteBits( aMsg, FSendClock,           1);
    BitWriter.WriteBits( aMsg, FIgnoreExternalClock, 1);
    BitWriter.WriteBits( aMsg, $00,                  5);
    BitWriter.WriteBits( aMsg, FTuneCent, 8);
    BitWriter.WriteBits( aMsg, FGlobalOctaveShiftActive, 1);
    BitWriter.WriteBits( aMsg, $00,                      7);
    BitWriter.WriteBits( aMsg, FGlobalOctaveShift, 8);
    BitWriter.WriteBits( aMsg, FTuneSemi, 8);
    aMsg.WriteMessage( $00);
    BitWriter.WriteBits( aMsg, FPedalPolarity, 1);
    BitWriter.WriteBits( aMsg, $40,            7);
    BitWriter.WriteBits( aMsg, FControlPedalGain, 8);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
    aMsg.WriteMessage( $00);
  finally
    BitWriter.Free;
  end;
end;

procedure TG2Synth.AddMsgSetPerformance( aMsg : TG2SendMessage; aPerfName : string; aPerf : IG2Perf);
var Chunk : TPatchChunk;
    i : integer;
begin
  add_log_line( 'Upload performance', LOGCMD_HDR);

  PByteArray(aMsg.Memory)^[4] := $42; // Overrule perf version
  aMsg.WriteMessage( S_SET_PATCH);
  aMsg.WriteMessage( $00);
  aMsg.WriteMessage( $00);
  aMsg.WriteMessage( $00);


  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := FWConnection.Log;

    Chunk.WriteName( aPerfName);
    Chunk.Flush;

    aPerf.PerformanceName := aPerfName;

    Chunk.WriteBits( $1a,         8);
    Chunk.WriteBits( C_PERF_NAME, 8);
    Chunk.WriteName( aPerfName);
    Chunk.Flush;

    aPerf.WriteSettings( Chunk);

    for i := 0 to NSLOTS - 1 do begin
      aPerf.Slot[ i].GetPatch.Write( Chunk, N_USB_VARIATIONS); // Write 10 variations in patch for USB
      Chunk.Flush;
    end;

    //CO aPerf.GlobalKnobList.Write( Chunk);
    //CO Chunk.WriteChunk( C_KNOBS_GLOBAL);
    aPerf.WriteChunk( C_KNOBS_GLOBAL, Chunk);

  finally
    Chunk.Free;
  end;
end;

function TG2Synth.ProcessResponseMsg( aSender : TObject; aMsg: TG2Message): boolean;
var R, Cmd, Version, SubCmd, Slot, b : byte;
    i : integer;
begin
  // Return True if message was processed
  Result := False;

  aMsg.Read( R, 1);
  case R of
  $80 : begin
          Result := True;
        end;
  $01 : begin
          aMsg.Read( Cmd, 1);
          case Cmd of
          $0c : begin
                  aMsg.Read( Version, 1);

                  case Version of
                  $40 : begin
                          Result := True;
                          repeat
                            aMsg.Read( SubCmd, 1);
                            case SubCmd of
                            $1f : begin
                                    aMsg.Read(b, 1); // Performance version?
                                    Perf.PerfVersion := b;
                                    i := 0;
                                    while (i < 4) and Result do begin

                                      aMsg.Read( b,    1); // $36
                                      aMsg.Read( Slot, 1);
                                      aMsg.Read( b,    1);
                                      Perf.Slot[ Slot].PatchVersion := b;

                                      inc(i);
                                    end;
                                    break;
                                  end;
                            $36 : begin
                                    aMsg.Read( Slot, 1);
                                    case Slot of
                                    $00..$03 :
                                      begin
                                        Perf.Slot[ Slot].ProcessMsgVersionChange( aMsg);
                                        //aMsg.Read( b, 1);
                                        //Performance.Slot[ Slot].PatchVersion := b;
                                      end;
                                    $04 :
                                      begin
                                        //aMsg.Read( b, 1);
                                        //Performance.PerfVersion := b;
                                        Perf.ProcessMsgVersionChange( aMsg);
                                      end;
                                    end;
                                  end;
                            $38 : begin
                                    // TODO : Follows after retrieving a patch, don't know the meaning
                                    // of the complete message
                                    aMsg.Read( Slot, 1);
                                    case Slot of
                                    $00..$03 :
                                      begin
                                        aMsg.Read( b, 1);
                                        Perf.Slot[ Slot].PatchVersion := b;
                                      end;
                                    $04 :
                                      begin
                                        aMsg.Read( b, 1);
                                        Perf.PerfVersion := b;
                                      end;
                                    end;
                                  end;
                              else begin
                                add_log_line('Unknown subcommand ' + IntToHex( R, 2) + ' ' + IntToHex( Cmd, 2) + ' ' + IntToHex( Version, 2) + ' ' + IntToHex( SubCmd, 2), LOGCMD_ERR);
                                Result := False;
                                break;
                              end;
                            end;
                          until aMsg.Position >= aMsg.Size - 2;
                        end;
                    else begin
                      if Version = Perf.GetPerfVersion then begin

                          repeat
                            aMsg.Read( SubCmd, 1);
                            case SubCmd of
                            S_SYNTH_SETTINGS :
                                  begin // synth settings
                                    ProcessMsgSynthSettings( aMsg);
                                    Result := True;
                                  end;
                            R_LIST_NAMES :
                                  begin
                                    aMsg.Read( b, 1); // $30 unknown
                                    aMsg.Read( b, 1); // $02 Unknown
                                  end;
                            R_ADD_NAMES :
                                  begin // List names
                                    ProcessMsgListAddNames( aMsg);
                                    Result := True;
                                  end;
                            R_STORE : // Folows after store (behind list message), but also after clear
                                  begin
                                     ProcessMsgAfterStore( aMsg);
                                     Result := True;
                                  end;
                            R_CLEAR :
                                  begin
                                     ProcessMsgAfterClear( aMsg);
                                     Result := True;
                                  end;
                            R_CLEAR_BANK :
                                  begin
                                     ProcessMsgAfterClearBank( aMsg);
                                     Result := True;
                                  end;
                            R_PATCH_BANK_UPDLOAD :
                                  begin
                                     ProcessMsgAfterBankUpload( aMsg);
                                     Result := True;
                                  end;
                            S_PATCH_BANK_DATA :
                                  begin
                                    ProcessMsgPatchBankData( aMsg);
                                    Result := True;
                                  end;
                            $1e : begin // unknown_2
                                    Result := True;
                                  end;
                            C_PERF_NAME,
                            C_PERF_SETTINGS :
                                  begin // Performance settings
                                    Perf.ProcessMsgPerfChunk( aMsg);
                                    Result := True;
                                  end;
                            C_KNOBS_GLOBAL :
                                  begin // Global knobs
                                    Perf.ProcessMsgGlobalKnobsChunk( aMsg);
                                    Result := True;
                                  end;
                            R_ASSIGNED_VOICES,
                            R_EXT_MASTER_CLOCK :
                                  begin
                                    aMsg.Position := aMsg.Position - 2;
                                    Result := Perf.ProcessResponseMsg( aSender, aMsg)
                                  end;
                            R_OK : begin
                                     FWConnection.LastResponsMessage := R_OK;
                                     Result := True;
                                   end;
                            R_ERROR :
                                  begin
                                    aMsg.Read(b, 1); // error no?
                                    //FErrorMessage := True;
                                    //FErrorMessageNo := b;
                                    //SetLastG2Error( IntToHex(b, 2));
                                    FWConnection.LastG2Error := IntToHex(b, 2);

                                    add_log_line('G2 returns error ' + IntToHex(b, 2) + '!', LOGCMD_ERR);

                                    FWConnection.LastResponsMessage := R_ERROR;
                                    Result := True;
                                  end;
                            $80 : begin // Unknown 1
                                    Result := True;
                                  end;
                              else begin
                                add_log_line('Unknown subcommand ' + IntToHex( R, 2) + ' ' + IntToHex( Cmd, 2) + ' ' + IntToHex( Version, 2) + ' ' + IntToHex( SubCmd, 2), LOGCMD_ERR);
                                Result := False;
                                exit;
                              end;
                            end;

                          until aMsg.Position >= aMsg.Size - 2;
                      end else begin
                        add_log_line('Performance version differs ' + IntToHex( R, 2) + ' ' + IntToHex( Cmd, 2) + ' ' + IntToHex( Version, 2), LOGCMD_ERR);
                        Result := False;
                      end;
                    end;
                  end;
                end;
          $00, $08 :
                begin
                  Result := Perf.Slot[ 0].ProcessResponseMsg( aSender, aMsg);
                end;
          $01, $09 :
                begin
                  Result := Perf.Slot[ 1].ProcessResponseMsg( aSender, aMsg);
                end;
          $02, $0a :
                begin
                  Result := Perf.Slot[ 2].ProcessResponseMsg( aSender, aMsg);
                end;
          $03, $0b :
                begin
                  Result := Perf.Slot[ 3].ProcessResponseMsg( aSender, aMsg);
                end;
          $04 : begin
                  Result := Perf.ProcessResponseMsg( aSender, aMsg);
                end;
            else begin
              add_log_line('Unknown command ' + IntToHex( R, 2) + ' ' + IntToHex( Cmd, 2), LOGCMD_ERR);
              Result := False;
            end;
          end;
        end;
    else begin
      add_log_line('Unknown response byte ' + IntToHex( R, 2), LOGCMD_ERR);
      Result := False;
    end;
  end;
end;

function TG2Synth.ProcessSendMsg( aSender : TObject; aMsg: TG2Message): boolean;
var Cmd, SubCmd, R, b, bh, bl, Stop, Slot, Mode, Bank, Patch : byte;
    i, Size : integer;
    perf_name : string;
begin
  // Return True if processed
  Result := False;

  if (aMsg.Size - aMsg.Position) < 4 then begin
    aMsg.Position := aMsg.Size;
    exit;
  end;

  // Read size
  aMsg.Read( bh, 1);
  aMsg.Read( bl, 1);
  Size := bh * 256 + bl;

  aMsg.Read( R, 1);
  case R of
  $80 : begin
          aMsg.Position := aMsg.Size; // Todo
          Result := True;
        end;
  $01 : begin
          aMsg.Read( Cmd, 1);

          case (Cmd and $0f)  of
          CMD_SYS :
            begin
              aMsg.Read( b, 1); // Version
              aMsg.Read( SubCmd, 1);
              case SubCmd of
                Q_VERSION_CNT :
                  begin
                    Result := True;
                  end;
                Q_SYNTH_SETTINGS :
                  begin
                    Result := True;
                  end;
                S_SYNTH_SETTINGS :
                  begin
                    Result := True;
                    DoSynthSettingsUpdate;
                  end;
                M_UNKNOWN_1 :
                  begin
                    Result := True;
                  end;
                M_UNKNOWN_2 :
                  begin
                    Result := True;
                  end;
                S_SEL_SLOT,
                Q_PERF_SETTINGS,
                C_PERF_SETTINGS :
                  begin
                    aMsg.Position := aMsg.Position - 1;
                    Result := Perf.ProcessSendMsg( aSender, aMsg);
                  end;
                C_PERF_NAME :
                     begin
                        perf_name := '';
                        i := 1;
                        aMsg.Read(b, 1);
                        while (aMsg.Position < aMsg.Size - 1) and (i<=16) and (b<>0) do begin
                          perf_name := perf_name + Char(b);
                          aMsg.Read(b, 1);
                          inc(i);
                        end;

                        aMsg.Position := aMsg.Size; // Todo
                        Result := True;

                        Perf.PerformanceName := perf_name;

                        DoAfterPerfNameChange;
                     end;
                S_START_STOP_COM :
                  begin
                    aMsg.Read( Stop, 1);
                    Result := True;

                    DoAfterStartStopCommunication;
                  end;
                S_SET_PATCH :
                  begin
                    aMsg.Position := aMsg.Position - 1;
                    Result := Perf.ProcessSendMsg( aSender, aMsg);
                  end;
               S_STORE :
                  begin
                    aMsg.Read( Slot, 1);
                    aMsg.Read( Bank, 1);
                    aMsg.Read( Patch, 1);
                    Result := True;

                    DoAfterStore( Slot, Bank, Patch);
                  end;
               S_CLEAR :
                  begin
                    aMsg.Read( Mode, 1);
                    aMsg.Read( Bank, 1);
                    aMsg.Read( Patch, 1);
                    Result := True;

                    DoAfterClear( TPatchFileType(Mode), Bank, Patch);
                  end;
               S_CLEAR_BANK :
                  begin
                    aMsg.Read( Mode, 1);
                    aMsg.Read( Bank, 1);
                    aMsg.Read( Patch, 1);
                    aMsg.Position := aMsg.Size; // Skip the rest

                    Result := True;

                    DoAfterClearBank( TPatchFileType(Mode), Bank);
                  end;
                S_RETREIVE :
                  begin
                    aMsg.Read( Slot, 1);
                    aMsg.Read( Bank, 1);
                    aMsg.Read( Patch, 1);
                    Result := True;

                    if Slot = 4 then
                    begin
                      //Perf.DoAfterRetreivePatch( Slot, Bank, Patch)
                      Perf.Init;
                      Perf.StartInit(FWConnection, True)
                    end
                    else
                    begin
                      //Perf.Slot[ Slot].DoAfterRetreivePatch( SLot, Bank, Patch);
                      Perf.Slot[ Slot].Patch.Init;
                      Perf.Slot[ Slot].StartInit(FWConnection, True);
                    end;
                  end;
                Q_GLOBAL_KNOBS :
                  begin
                    Result := True;
                  end;
                Q_LIST_NAMES :
                  begin
                    aMsg.Read( b, 1);
                    Result := True;

                    if (FNextBankListCmd.PatchFileType = pftEnd) then
                      DoAfterBankList;
                  end;
                Q_ASSIGNED_VOICES :
                  begin
                    aMsg.Read( b, 1);
                    Result := True;

                  end
                else
                  aMsg.Position := aMsg.Size; // Todo
              end;
            end;
          $08 : begin
                  aMsg.Position := 0;
                  Result := Perf.Slot[ 0].ProcessSendMsg( aSender, aMsg);
                end;
          $09 : begin
                  aMsg.Position := 0;
                  Result := Perf.Slot[ 1].ProcessSendMsg( aSender, aMsg);
                end;
          $0a : begin
                  aMsg.Position := 0;
                  Result := Perf.Slot[ 2].ProcessSendMsg( aSender, aMsg);
                end;
          $0b : begin
                  aMsg.Position := 0;
                  Result := Perf.Slot[ 3].ProcessSendMsg( aSender, aMsg);
                end;
          end;
        end;
  end;
end;

procedure TG2Synth.ProcessMsgSynthSettings( aMsg: TG2Message);
var BitReader : TBitReader;
begin
  BitReader := TBitReader.Create;
  try
    FSynthName := ReadClaviaString( aMsg);
    //while (length(FSynthName) < 16) and (aMsg.Read(FUnknown, 1) = 1) and (FUnknown <> 0) do begin
    //  FSynthName := FSynthName + char(FUnknown);
    //end;
    FPerfMode := BitReader.ReadBits( aMsg, 1); // Perf mode? Check!
    FUnknown := BitReader.ReadBits( aMsg, 7);
    FUnknown := BitReader.ReadBits( aMsg, 8); // $00
    FPerfBank := BitReader.ReadBits( aMsg, 8);
    FPerfLocation := BitReader.ReadBits( aMsg, 8);
    FMemoryProtect := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 7);
    FMidiChannelA := BitReader.ReadBits( aMsg, 8);
    FMidiChannelB := BitReader.ReadBits( aMsg, 8);
    FMidiChannelC := BitReader.ReadBits( aMsg, 8);
    FMidiChannelD := BitReader.ReadBits( aMsg, 8);
    FMidiGlobalChannel := BitReader.ReadBits( aMsg, 8);
    FSysExID := BitReader.ReadBits( aMsg, 8);
    FLocalOn := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 7);
    FUnknown := BitReader.ReadBits( aMsg, 6);
    FProgramChangeReceive := BitReader.ReadBits( aMsg, 1);
    FProgramChangeSend := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 6);
    FControllersReceive := BitReader.ReadBits( aMsg, 1);
    FControllersSend := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 1);
    FSendClock := BitReader.ReadBits( aMsg, 1);
    FIgnoreExternalClock := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 5);
    FTuneCent := BitReader.ReadBits( aMsg, 8);
    FGlobalOctaveShiftActive := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 7);
    FGlobalOctaveShift := BitReader.ReadBits( aMsg, 8);
    FTuneSemi := BitReader.ReadBits( aMsg, 8);
    FUnknown := BitReader.ReadBits( aMsg, 8);
    FPedalPolarity := BitReader.ReadBits( aMsg, 1);
    FUnknown := BitReader.ReadBits( aMsg, 7);
    FControlPedalGain := BitReader.ReadBits( aMsg, 8);

  finally
    BitReader.Free;
  end;

  DoSynthSettingsUpdate;
end;


procedure TG2Synth.ProcessMsgListAddNames( aMsg: TG2Message);
var b, Bank, Patch, Category : byte;
    PatchFileType : TPatchFileType;
    patch_name : string;
    BankItem : IG2BankItem;
begin
  //for i := 0 to 3 do
  //  aMsg.Read( b, 1); // read 4 unknown bytes
  aMsg.Read( b, 1); // $00 - After store $01 - After list

  aMsg.Read( PatchFileType, 1);

  patch_name := '';

  while aMsg.Read(b, 1) = 1 do begin
    case b of
    $01 : begin // read patch location
            aMsg.Read( Patch, 1);
            FNextBankListCmd.Cmd   := b;
            FNextBankListCmd.PatchFileType  := PatchFileType;
            FNextBankListCmd.Bank  := Bank;
            FNextBankListCmd.Patch := Patch;
          end;
    $02 : begin // ????
            FNextBankListCmd.Cmd   := b;
            FNextBankListCmd.PatchFileType := PatchFileType;
            FNextBankListCmd.Bank  := Bank;
            FNextBankListCmd.Patch := Patch;
            break;
          end;
    $03 : begin // read bank
            aMsg.Read( Bank, 1);
            aMsg.Read( Patch, 1);
            FNextBankListCmd.Cmd   := b;
            FNextBankListCmd.PatchFileType := PatchFileType;
            FNextBankListCmd.Bank  := Bank;
            FNextBankListCmd.Patch := Patch;
          end;
    $04 : begin // next mode  0 = patch, 1 = perf, 2 = finished
            inc( PatchFileType);
            Bank := 0;
            Patch := 0;
            FNextBankListCmd.Cmd   := b;
            FNextBankListCmd.PatchFileType := PatchFileType;
            FNextBankListCmd.Bank  := Bank;
            FNextBankListCmd.Patch := Patch;

            break;
          end;
    $05 : begin; // last patch in message read
            //inc( aPatch);
            FNextBankListCmd.Cmd   := b;
            FNextBankListCmd.PatchFileType := PatchFileType;
            FNextBankListCmd.Bank  := Bank;
            FNextBankListCmd.Patch := Patch;

            break;
          end
    else
      if b = $00 then begin
        // End of string
        aMsg.Read( Category, 1);
        FslBanks.Add( 'Category : ' + IntToStr( Category) + ' Mode:' + IntToStr( ord(PatchFileType)) + ' Bank: ' + IntToStr(Bank)
                     + ' Patch: ' + IntTostr( Patch) + ' ' + string( patch_name));

        if (ord(PatchFileType) = 0) and (Bank = 0) and (Patch = 0) then
          BankListClear;

        BankItem := FindBankItem( PatchFileType, Bank, Patch);
        if not assigned(BankItem) then begin
          BankItem := TBankItem.Create;
          BankItem.PatchFileType := PatchFileType;
          BankItem.Bank := Bank;
          BankItem.Patch := Patch;
          BankItem.PatchName := patch_name;
          BankItem.Category := Category;
          AddBankItem( BankItem);
        end else begin
          BankItem.PatchName := patch_name;
          BankItem.Category := Category;
        end;

        patch_name := '';
        inc(Patch);
      end else begin
        patch_name := patch_name + Char(b);
        if patch_name.Length = 16 then begin
          // String reached 16 chars
          aMsg.Read( Category, 1);
          FslBanks.Add( 'Category : ' + IntToStr( Category) + ' Mode:' + IntToStr( ord(PatchFileType)) + ' Bank: ' + IntToStr(Bank)
                       + ' Patch: ' + IntTostr( Patch) + ' ' + string( patch_name));

          if ( ord(PatchFileType) = 0) and ( Bank = 0) and ( Patch = 0) then
            BankListClear;

          BankItem := FindBankItem( PatchFileType, Bank, Patch);
          if not assigned(BankItem) then begin
            BankItem := TBankItem.Create;
            BankItem.PatchFileType := PatchFileType;
            BankItem.Bank := Bank;
            BankItem.Patch := Patch;
            BankItem.PatchName := patch_name;
            BankItem.Category := Category;
            AddBankItem( BankItem);
          end else begin
            BankItem.PatchName := patch_name;
            BankItem.Category := Category;
          end;

          patch_name := '';
          inc(Patch);
        end;
      end;
    end;
  end;
end;

procedure TG2Synth.ProcessMsgAfterStore( aMsg: TG2Message);
var Slot, Bank, Patch, b : byte;
begin
  aMsg.Read( Slot, 1);
  aMsg.Read( Bank, 1);
  aMsg.Read( Patch, 1);
  aMsg.Read( b, 1);  // Unknown
  aMsg.Read( b, 1);  // Unknown
end;

procedure TG2Synth.ProcessMsgAfterClear( aMsg: TG2Message);
var Bank, Patch : byte;
    PatchFileType : TPatchFileType;
begin
  aMsg.Read( PatchFileType, 1); // 0 - patch, 1 - perf
  aMsg.Read( Bank, 1);
  aMsg.Read( Patch, 1);
  DeleteBankItem( PatchFileType, Bank, Patch);
  {i := BankList.FindIndex( PatchFileType, Bank, Patch);
  if i <> -1 then
   BankList.Delete(i);}
end;

procedure TG2Synth.ProcessMsgAfterClearBank( aMsg: TG2Message);
var FromBank, ToBank, FromLocation, ToLocation : byte;
    PatchFileType : TPatchFileType;
begin
  aMsg.Read( PatchFileType, 1); // 0 - patch, 1 - perf
  aMsg.Read( FromBank, 1);
  aMsg.Read( FromLocation, 1);
  aMsg.Read( ToBank, 1);
  aMsg.Read( ToLocation, 1);
  while (FromLocation <= ToLocation) and (FromBank = ToBank) do begin
    DeleteBankItem( PatchFileType, FromBank, FromLocation);
    {i := BankList.FindIndex( PatchFileType, FromBank, FromLocation);
    if i <> -1 then
      BankList.Delete(i);}
    inc( FromLocation);
  end;
end;

procedure TG2Synth.ProcessMsgAfterBankUpload( aMsg: TG2Message);
var Bank, Patch, b : byte;
    PatchFileType : TPatchFileType;
begin
  aMsg.Read( b, 1);  // Unknown $04
  aMsg.Read( PatchFileType, 1); // 0 - patch, 1 - perf
  aMsg.Read( Bank, 1);
  aMsg.Read( Patch, 1);

  FNextBankListCmd.Cmd   := b;
  FNextBankListCmd.PatchFileType  := PatchFileType;
  FNextBankListCmd.Bank  := Bank;
  FNextBankListCmd.Patch := Patch;
end;

procedure TG2Synth.ProcessMsgPatchBankData( aMsg: TG2Message);
var Bank, Patch, b, hb, lb, PatchVersion, PatchType : byte;
    PatchFileType : TPatchFileType;
    patch_name, file_name, NameExt, FileExt : string;
    PatchData : TMemoryStream;
    i : integer;
begin
  aMsg.Read( PatchFileType, 1); // 0 - patch, 1 - perf
  aMsg.Read( Bank, 1);
  aMsg.Read( Patch, 1);

  FNextBankListCmd.Cmd   := b;
  FNextBankListCmd.PatchFileType  := PatchFileType;
  FNextBankListCmd.Bank  := Bank;
  FNextBankListCmd.Patch := Patch;

  patch_name := '';
  while (Patch_name.Length < 16) and (aMsg.Read(b, 1) = 1) and (b <> 0) do begin
    Patch_name := Patch_name + Char(b);
  end;

  aMsg.Read( hb, 1); // Size
  aMsg.Read( lb, 1);
  aMsg.Read( PatchVersion, 1); // Version
  aMsg.Read( PatchType, 1); // PatchType, always 0?

  PatchData := TMemoryStream.Create;
  try
    // Save to file
    i := 0;
    NameExt := '';
    if TPatchFileType(PatchFileType) = pftPatch then begin
      FileExt := '.pch2';
      AddHeaderInfo( pftPatch, PatchData);
    end else begin
      FileExt := '.prf2';
      AddHeaderInfo( pftPerf, PatchData);
    end;
    // Check: Crc??

    while FileExists( FBankDumpFolder + patch_name + NameExt + FileExt) do begin
      inc(i);
      NameExt := IntToStr(i);
    end;
    file_name := patch_name + NameExt + FileExt;

    PatchData.Size := PatchData.Size + hb*256+lb - 1;
    aMsg.Read( PStaticByteBuffer(PatchData.Memory)^[PatchData.Position], hb*256+lb);
    PatchData.SaveToFile( FBankDumpFolder + file_name);

    // Seems clavia software starts first file entry with 2????
    FBankDumpList.Add( IntToStr(Bank+1) + ':' + IntToStr(FBankDumpList.Count + 1) + ': ' + file_name);
  finally
    PatchData.Free;
  end;
end;

procedure TG2Synth.AddBankItem(aBankItem: IG2BankItem);
begin
  FBankList.Add( aBankItem);
end;

procedure TG2Synth.DeleteBankItem(aPatchFileType: TPatchFileType; aBank,
  aPatch: byte);
var i : integer;
begin
  i := 0;
  while (i<FBankList.Count)
      and not( (FBankList[i].PatchFileType = aPatchFileType)
           and (FBankList[i].Bank = aBank)
           and (FBankList[i].Patch = aPatch)) do
    inc(i);
  if i<FBankList.Count then
    FBankList.Delete(i);
end;

function TG2Synth.FindBankItem( aPatchFileType: TPatchFileType; aBank,
  aPatch: byte): IG2BankItem;
var i : integer;
begin
  i := 0;
  while (i<FBankList.Count)
      and not( (FBankList[i].PatchFileType = aPatchFileType)
           and (FBankList[i].Bank = aBank)
           and (FBankList[i].Patch = aPatch)) do
    inc(i);
  if i<FBankList.Count then
    Result := FBankList[i]
  else
    Result := nil;
end;

procedure TG2Synth.AddMidiToKnob(const aSource: TG2ParamSource;
  const  aControlType: TG2ControlType; const aKnob: integer;
  const aMidiCC: byte);
var i : integer;
    MidiToKnob : IG2MidiToKnob;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].Source = aSource)
                                      and (FMidiToKnobList[i].ControlType = aControlType)
                                      and (FMidiToKnobList[i].Knob = aKnob)) do
    inc(i);

  if not(i<FMidiToKnobList.Count) then begin
    MidiToKnob := TG2MidiToKnob.Create(aSource, aControlType, aKnob, aMidiCC);
    FMidiToKnobList.Add(MidiToKnob);
  end else
    FMidiToKnobList[i].Controller := aMidiCC;
end;

procedure TG2Synth.DeleteMidiToKnob(const aSource : TG2ParamSource;
  const aControlType : TG2ControlType; const aKnob: integer);
var i : integer;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].Source = aSource)
                                      and (FMidiToKnobList[i].ControlType = aControlType)
                                      and (FMidiToKnobList[i].Knob = aKnob)) do
    inc(i);

  if (i<FMidiToKnobList.Count) then
    FMidiToKnobList.Delete(i);
end;

function TG2Synth.FindMidiToKnob(const aSource : TG2ParamSource;
  const aControlType : TG2ControlType; const aKnob: integer): integer;
var i : integer;
begin
  Result := 0;

  i := 0;
  while (i<FMidiToKnobList.Count) and not((FMidiToKnobList[i].Source = aSource)
                                      and (FMidiToKnobList[i].ControlType = aControlType)
                                      and (FMidiToKnobList[i].Knob = aKnob)) do
    inc(i);

  if (i<FMidiToKnobList.Count) then
    Result := FMidiToKnobList[i].Controller;
end;

function TG2Synth.MidiToKnobCheckCC(const aMidiCC: byte): boolean;
var i : integer;
begin
  i := 0;
  while (i<FMidiToKnobList.Count) and not(FMidiToKnobList[i].Controller = aMidiCC) do
    inc(i);

  Result := (i<FMidiToKnobList.Count);
end;

function TG2Synth.TextFunction( No : integer; Param1, Param2, Param3 : byte): string;
begin
  case No of
  0 : begin
        case Param1 of
        0 : Result := 'Legato';
        1 : Result := 'Mono';
        else
          Result := IntToStr(Param1);
        end;
      end;
  end;
end;

function TG2Synth.GetSynthName : string;
begin
  Result := FSynthName;
end;

function TG2Synth.GetPerfMode : TBits1;
begin
  Result := FPerfMode;
end;

function TG2Synth.GetMemoryProtect : TBits1;
begin
  Result := FMemoryProtect;
end;

function TG2Synth.GetMidiChannelA : TBits8;
begin
  Result := FMidiChannelA;
end;

function TG2Synth.GetMidiChannelB : TBits8;
begin
  Result := FMidiChannelB;
end;

function TG2Synth.GetMidiChannelC : TBits8;
begin
  Result := FMidiChannelC;
end;

function TG2Synth.GetMidiChannelD : TBits8;
begin
  Result := FMidiChannelD;
end;

function TG2Synth.GetMidiGlobalChannel : TBits8;
begin
  Result := FMidiGlobalChannel;
end;

function TG2Synth.GetMidiToKnobList: TList<IG2MidiToKnob>;
begin
  Result := FMidiToKnobList;
end;

function TG2Synth.GetNextBankListCmd: TNextBankListCmd;
begin
  Result := FNextBankListCmd;
end;

function TG2Synth.GetSysExID : TBits8;
begin
  Result := FSysExID;
end;

function TG2Synth.GetLocalOn :  TBits1;
begin
  Result := FLocalOn;
end;

function TG2Synth.GetProgramChangeReceive : TBits1;
begin
  Result := FProgramChangeReceive;
end;

function TG2Synth.GetProgramChangeSend : TBits1;
begin
  Result := FProgramChangeSend;
end;

function TG2Synth.GetControllersReceive : TBits1;
begin
  Result := FControllersReceive;
end;

function TG2Synth.GetControllersSend : TBits1;
begin
  Result := FControllersSend;
end;

function TG2Synth.GetSendClock : TBits1;
begin
  Result := FSendClock;
end;

function TG2Synth.GetIgnoreExternalClock : TBits1;
begin
  Result := FIgnoreExternalClock;
end;

function TG2Synth.GetTuneCent : TBits8;
begin
  Result := FTuneCent;
end;

function TG2Synth.GetGlobalOctaveShiftActive : TBits1;
begin
  Result := FGlobalOctaveShiftActive;
end;

function TG2Synth.GetGlobalOctaveShift : TBits8;
begin
  Result := FGlobalOctaveShift;
end;

function TG2Synth.GetTuneSemi : TBits8;
begin
  Result := FTuneSemi;
end;

function TG2Synth.GetPedalPolarity : TBits1;
begin
  Result := FPedalPolarity;
end;

function TG2Synth.GetControlPedalGain : TBits8;
begin
  Result := FControlPedalGain;
end;

procedure TG2Synth.SetSynthName( aValue : string);
begin
  if Length(aValue) > 16 then
    raise Exception.Create('Synth name length max 16.');

  if aValue = '' then
    raise Exception.Create('Invalid synth name.');

  FSynthName := aValue;
end;

procedure TG2Synth.SetMemoryProtect( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Memory protect must be 0 or 1.');

  FMemoryProtect := aValue;
end;

procedure TG2Synth.SetMidiChannelA( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelA := aValue;
end;

procedure TG2Synth.SetMidiChannelB( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelB := aValue;
end;

procedure TG2Synth.SetMidiChannelC( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelC := aValue;
end;

procedure TG2Synth.SetMidiChannelD( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Midi channel must be between 0 and 16.');

  FMidiChannelD := aValue;
end;

procedure TG2Synth.SetMidiGlobalChannel( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Global midi channel must be between 0 and 16.');

  FMidiGlobalChannel := aValue;
end;

procedure TG2Synth.SetNextBankListCmd(const aValue: TNextBankListCmd);
begin
  FNextBankListCmd := aValue;
end;

procedure TG2Synth.SetSysExID( aValue : TBits8);
begin
  if aValue > 16 then
    raise Exception.Create('Sysex ID must be between 0 and 16.');

  FSysExID := aValue;
end;

procedure TG2Synth.SetLocalOn( aValue :  TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Local on must be 0 or 1.');

  FLocalOn := aValue;
end;

procedure TG2Synth.SetProgramChangeReceive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Program change receive must be 0 or 1.');

  FProgramChangeReceive := aValue;
end;

procedure TG2Synth.SetProgramChangeSend( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Program change send must be 0 or 1.');

  FProgramChangeSend := aValue;
end;

procedure TG2Synth.SetControllersReceive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Controllers receive must be 0 or 1.');

  FControllersReceive := aValue;
end;

procedure TG2Synth.SetControllersSend( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Controllers send must be 0 or 1.');

  FControllersSend := aValue;
end;

procedure TG2Synth.SetSelectedSlotIndex(const aValue: TBits2);
begin
  Perf.SelectedSlotIndex := aValue;
end;

procedure TG2Synth.SetSendClock( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Send clock must be 0 or 1.');

  FSendClock := aValue;
end;

procedure TG2Synth.SetIgnoreExternalClock( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Ignore external clock must be 0 or 1.');

  FIgnoreExternalClock := aValue;
end;

procedure TG2Synth.SetTuneCent( aValue : TBits8);
begin
  if (aValue > $64) and (aValue < $9c) then
    raise Exception.Create('Tune cent must be between -100 ($9c) and 100 ($64).');
  FTuneCent := aValue;
end;

procedure TG2Synth.SetGlobalOctaveShiftActive( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Global shift active must be 0 or 1.');

  FGlobalOctaveShiftActive := aValue;
end;

procedure TG2Synth.SetGlobalOctaveShift( aValue : TBits8);
begin
 if (aValue > $02) and (aValue < $FE) then
    raise Exception.Create('Global shift active must be between -2 ($FE) or 2 ($02).');

  FGlobalOctaveShift := aValue;
end;

procedure TG2Synth.SetTuneSemi( aValue : TBits8);
begin
  if (aValue > $06) and (aValue < $FA) then
    raise Exception.Create('Tune semi must be between -6 ($FA) and 6 ($06).');

  FTuneSemi := aValue;
end;

procedure TG2Synth.StartInit(aConnection: IG2Connection);
begin
  FInitStep := 1;
  FNextBankListCmd.PatchFileType := pftPatch;
  FNextBankListCmd.Bank := 0;
  FNextBankListCmd.Patch := 0;

  aConnection.OnNextMsg := InitSeq;
  aConnection.SynthInit;
end;

procedure TG2Synth.InitSeq(Sender: TObject; aConnection: IG2Connection);
var do_next_step : boolean;
begin
  do_next_step := true;
  try
    case FInitStep of
    0 : aConnection.SynthInit;
    1 : aConnection.SynthStartStopCommunication(STOP_COMM);
    2 : aConnection.SynthGetPatchVersion;
    3 : aConnection.SynthSettings;
    4 : aConnection.SynthUnknown1Message;
    5 : Perf.StartInit(aConnection, False);
    6 : if FNextBankListCmd.PatchFileType <> pftEnd then begin
          aConnection.SynthGetList(FNextBankListCmd.PatchFileType, FNextBankListCmd.Bank, FNextBankListCmd.Patch);
          do_next_step := false;
        end else begin
          aConnection.SynthStartStopCommunication(START_COMM);
          aConnection.OnNextMsg := nil;
          FInitialized := True;
          DoAfterG2Init;
        end;
    else
      begin
        aConnection.OnNextMsg := nil;
      end;
    end;
    if do_next_step then
      inc(FInitStep);
  except on E:Exception do begin
      FInitialized := False;
      aConnection.OnNextMsg := nil;
    end;
  end;
end;

procedure TG2Synth.SetPedalPolarity( aValue : TBits1);
begin
  if aValue > 1 then
    raise Exception.Create('Pedal polarity must be 0 or 1.');

  FPedalPolarity := aValue;
end;

procedure TG2Synth.SetPerfMode(aValue: TBits1);
begin
  FPerfMode := aValue;
end;

procedure TG2Synth.SetControlPedalGain( aValue : TBits8);
begin
  if aValue > 32 then
    raise Exception.Create('Control pedal gain must be between 0 and 32.');
   FControlPedalGain := aValue;
end;

procedure TG2Synth.SetAutoAssignMidi(const aValue: boolean);
begin
  FAutoAssignMidi := aValue;
end;

procedure TG2Synth.SetBankDumpDestBank(aValue: byte);
begin
  FBankDumpDestBank := aValue;
end;

procedure TG2Synth.SetBankDumpFileName(aValue: string);
begin
  FBankDumpFileName := aValue;
end;

procedure TG2Synth.SetBankDumpFolder(aValue: string);
begin
  FBankDumpFolder := aValue;
end;

procedure TG2Synth.SetBankDumpListIndex(aValue: integer);
begin
  FBankDumpListIndex := aValue;
end;

procedure TG2Synth.SetBankItem( const aIndex : integer; const aValue: IG2BankItem);
begin
  FBankList[ aIndex] := aValue;
end;

procedure TG2Synth.SetClientType( const aValue: TClientType);
begin
  FClientType := aValue;
end;

function TG2Synth.GetAutoAssignMidi: boolean;
begin
  Result := FAutoAssignMidi;
end;

function TG2Synth.GetBankDumpDestBank: byte;
begin
  Result := FBankDumpDestBank;
end;

function TG2Synth.GetBankDumpFileName: string;
begin
  Result := FBankDumpFileName;
end;

function TG2Synth.GetBankDumpFolder: string;
begin
  Result := FBankDumpFolder;
end;

function TG2Synth.GetBankDumpList: TStringList;
begin
  Result := FBankDumpList;
end;

function TG2Synth.GetBankDumpListIndex: integer;
begin
  Result := FBankDumpListIndex;
end;

function TG2Synth.GetBankItem( const aIndex : integer): IG2BankItem;
begin
  Result := FBankList[ aIndex];
end;

function TG2Synth.GetBankItemCount: integer;
begin
  Result := FBankList.Count;
end;

function TG2Synth.GetClientType: TClientType;
begin
  Result := FClientType;
end;

function TG2Synth.GetID : integer;
begin
  Result := 0;
end;

function TG2Synth.GetPerf: IG2Perf;
begin
  Result := FPerf;
end;

function TG2Synth.GetSelectedSlot: IG2Slot;
begin
  Result := FPerf.Slot[ FPerf.SelectedSlotIndex];
end;

function TG2Synth.GetSelectedSlotIndex: TBits2;
begin
  Result := FPerf.SelectedSlotIndex;
end;

function TG2Synth.G2MessageDlg( tekst : string; code : integer): integer;
begin
  Result := 0;
end;

procedure TG2Synth.add_log_line( tekst: string; log_cmd : integer);
begin
  FWConnection.AddLog( tekst, log_cmd);
end;

function TG2Synth.BankFindFirstLast(aPatchFileType: TPatchFileType; aBank: byte;
  var aFirstLocation, aLastLocation: byte): boolean;
begin
  Result := FBankList.FindFirstLast( aPatchFileType, aBank, aFirstLocation, aLastLocation);
end;

function TG2Synth.BankFindNext: IG2BankItem;
begin
  Result := FBankList.FindNext( FNextBankListCmd.PatchFileType, FNextBankListCmd.Bank, FNextBankListCmd.Patch);
end;

procedure TG2Synth.BankListClear;
begin
  FBankList.Clear;
end;

{ TG2MidiToKnob }

constructor TG2MidiToKnob.Create(const aSource: TG2ParamSource;
  const aControlType: TG2ControlType; const aKnob, aController: integer);
begin
  FSource := aSource;
  FControlType := aControlType;
  FKnob := aKnob;
  FController := aController;
end;

function TG2MidiToKnob.GetController: integer;
begin
  Result := FController;
end;

function TG2MidiToKnob.GetControlType: TG2ControlType;
begin
  Result := FControlType;
end;

function TG2MidiToKnob.GetKnob: integer;
begin
  Result := FKnob;
end;

function TG2MidiToKnob.GetSource: TG2ParamSource;
begin
  Result := FSource;
end;

procedure TG2MidiToKnob.SetController(const Value: integer);
begin
  FController := Value;
end;

procedure TG2MidiToKnob.SetControlType(const Value: TG2ControlType);
begin
  FControlType := Value;
end;

procedure TG2MidiToKnob.SetKnob(const Value: integer);
begin
  FKnob := Value;
end;

procedure TG2MidiToKnob.SetSource(const Value: TG2ParamSource);
begin
  FSource := Value;
end;

end.

