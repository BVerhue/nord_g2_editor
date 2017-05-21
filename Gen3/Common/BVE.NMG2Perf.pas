unit BVE.NMG2Perf;

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
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  System.Math,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2Stream,
  BVE.NMG2FileIntf;

type
  TG2Perf = class(TG2FileDataStream, IG2Perf)
  private
    [Weak] FWConnection : IG2Connection;

    FInitStep : integer;
    FInitMidiStep : integer;
    FStartCommAfterInit : boolean;
    FFinishMsg : TOnNextMsgEvent;

    FPerfVersion : Byte;
    FPerformanceName : string;
    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4,
    FUnknown5,
    FUnknown6,
    FUnknown7,
    FUnknown8,
    FUnknown9 : TBits8; //array[0..8] of
    FSelectedSlotIndex : TBits2;
    FKeyboardRangeEnabled : TBits8;
    FMasterClock : TBits8;
    FMasterClockRun : TBits8;

    FLastMidiCC : byte;
    FLastMidiClock : integer;

    FSlotArray : array of IG2Slot; // [0..NSLOTS-1]

    FGlobalKnobList : TList<IG2Knob>;

    // Probably somewhere in the unknown bytes
    FSelectedParamPage  : integer;
    FSelectedParamPageColumn : integer;

    function GetPerfVersion : Byte;
    function GetPerformanceName : string;
    function GetSelectedSlotIndex : TBits2;
    function GetMasterClock : TBits8;
    function GetMasterClockRun : TBits8;
    function GetKeyboardRangeEnabled : TBits8;
    function GetSelectedParamPage : integer;
    function GetSelectedParamPageColumn : integer;
    function GetSynth : IG2Synth;
    function GetGlobalKnobList : TList<IG2Knob>;
    function GetLastMidiCC : byte;
    function GetLastMidiClock : integer;

    procedure SetPerfVersion(const aValue : byte);
    procedure SetSelectedParamPage(const aValue : integer);
    procedure SetSelectedParamPageColumn(const aValue : integer);

    function ProcessResponseMsg(aSender : TObject; aMsg: TG2Message): boolean;
    function ProcessSendMsg(aSender : TObject; aMsg : TG2Message): boolean;

    procedure ProcessMsgAssignedVoices( aMsg: TG2Message);
    procedure ProcessMsgExtMasterClock( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobAssign( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobDeassign( aMsg: TG2Message);
    procedure ProcessMsgGlobalKnobsChunk( aMsg: TG2Message);
    procedure ProcessMsgMasterClock( aMsg: TG2Message);
    procedure ProcessMsgMidiCC( aMsg: TG2Message);
    procedure ProcessMsgPerfChunk( aMsg: TG2Message);
    procedure ProcessMsgVersionChange( aMsg: TG2Message);
  protected
    procedure DoSelectSlot;
    procedure SetPerformanceName(const aValue : string);
    procedure SetKeyboardRangeEnabled(const aValue : TBits8);
    function GetSlot(const aIndex : byte): IG2Slot;
    procedure InitSelectedSlotIndex(const aValue : TBits2); virtual;
    procedure SetSelectedSlotIndex(const aValue : TBits2); virtual;
    procedure SetMasterClock(const aValue : TBits8); virtual;
    procedure SetMasterClockRun(const aValue : TBits8); virtual;
  public
    constructor Create(aConnection: IG2Connection);
    destructor Destroy; override;

    class function LoadStreamPerf(aConnection: IG2Connection; aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream; override;

    procedure NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);

    procedure add_log_line(tekst: string; log_cmd: integer);

    procedure Init;
    procedure ClearKnobs(const aSlotIndex: integer);

    procedure WriteSettings(aChunk: TPatchChunk);

    procedure Read( aChunk : TPatchChunk); override;
    procedure ReadKnobList( aKnobList : TList<IG2Knob>; aChunk : TPatchChunk);

    procedure Write( aChunk : TPatchChunk; aVariationCount : byte); override;
    procedure WriteChunk( aID : byte; aChunk : TPatchChunk);
    procedure WriteKnobList( aKnobList : TList<IG2Knob>; aChunk : TPatchChunk);

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure StartInit(aConnection: IG2Connection; const aStartCommAfterInit : boolean);
    procedure InitSeq(Sender: TObject; aConnection: IG2Connection);
    function StartInitMidi(aConnection: IG2Connection): boolean;

    procedure AddMsgGetPerfSettings(aMsg : TG2SendMessage);
    procedure AddMsgUnknown2(aMsg : TG2SendMessage);
    procedure AddMsgSelectSlot(aMsg : TG2SendMessage; aSlotIndex: byte);
    procedure AddMsgSetMasterClockBPM(aMsg : TG2SendMessage; aBPM: byte);
    procedure AddMsgSetMasterClockRun(aMsg : TG2SendMessage; aStart: boolean);
    procedure AddMsgSetPerfSettings(aMsg : TG2SendMessage);
    procedure AddMsgSetPerfName(aMsg : TG2SendMessage; aPerfName : string);
    procedure AddMsgGetGlobalKnobs(aMsg : TG2SendMessage);
    procedure AddMsgSelectGlobalParamPage(aMsg : TG2SendMessage; aPageIndex : byte);
    procedure AddMessAssignGlobalKnobs(aModule: IG2Module; const aPageIndex : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    procedure DoAfterGetAssignedVoices;
    procedure DoAfterPerfInit;
    procedure DoPerfSettingsUpdate;
    procedure DoPerfUpdate;
    procedure DoAfterRetreivePatch( Slot, Bank, Patch : byte); virtual;
    procedure DoClockRunChange( aRun : boolean);
    procedure DoClockBPMChange( aBPM : byte);
    procedure DoMidiClockReceive( aValue : integer);
    procedure DoMidiCCReceive( aMidiCC : byte);

    procedure SaveToFile( aStream : TStream);
    function LoadFromFile( aStream : TStream): Boolean;
{$IFDEF MSWINDOWS}
    procedure SaveAsFXB( aStream : TStream);
    function LoadFromFXB( aStream : TStream): Boolean;
{$ENDIF}
    function GetNoOffExtendedModules : integer; // Number of modules that are not compatible with original editor

    procedure DeleteModuleFromPerf(const aSlotIndex, aLocationIndex : byte; aModule: IG2Module);
    function AssignGlobalKnobInPerf(const aKnobIndex, aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Knob;
    procedure DeassignGlobalKnobInPerf(const aKnobIndex : byte);

    function FindKnob(const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Knob;

    property Synth: IG2Synth read GetSynth;
    property Slot[ const aIndex : byte]: IG2Slot read GetSlot;
    property PerfVersion : Byte read GetPerfVersion write SetPerfVersion;
    property PerformanceName : string read GetPerformanceName write SetPerformanceName;
  end;


implementation
uses
  BVE.NMG2Knob,
  BVE.NMG2Slot;

//------------------------------------------------------------------------------
//
//                            TG2Perf
//
//------------------------------------------------------------------------------

constructor TG2Perf.Create(aConnection: IG2Connection);
var i, KnobCount : integer;
    GlobalKnob : IG2Knob;
begin
  inherited Create;

  SetWeak(@FWConnection, aConnection);

  FGlobalKnobList := TList<IG2Knob>.Create;
  KnobCount := 120;
  for i := 0 to KnobCount - 1 do begin
    GlobalKnob := TG2GlobalKnob.Create(i, nil);
    FGlobalKnobList.Add( GlobalKnob);
  end;

  SetLength(FSLotArray, NSLOTS);
  for i := 0 to NSLOTS - 1 do begin
    FSlotArray[i] := TG2Slot.Create(FWConnection, i);
  end;
end;

destructor TG2Perf.Destroy;
var i : integer;
begin
  FGlobalKnobList.Free;

  for i := 0 to High(FSlotArray) do
    FSlotArray[i].NotifyDestroy;
  Finalize(FSlotArray);

  SetWeak(@FWConnection, nil);

  inherited;
end;

class function TG2Perf.LoadStreamPerf(aConnection: IG2Connection;
  aStream : TStream; aChunk : TPatchChunk): TG2FileDataStream;
begin
  Result := TG2Perf.Create(aConnection);
  aChunk.ReadChunk;
  Result.Read( aChunk);
end;

procedure TG2Perf.Init;
var i : integer;
    GlobalKnob : IG2Knob;
begin
  PatchVersion   := PATCH_VERSION;
  PatchType := 1;

  //for i := 0 to 8 do
  //  FUnknown[i] := 0;
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;
  FUnknown5 := 0;
  FUnknown6 := 0;
  FUnknown7 := 0;
  FUnknown8 := 0;
  FUnknown9 := 0;

  FKeyboardRangeEnabled := 0;
  FMasterClock          := $78; // 120 BPM
  FMasterClockRun       := 0;

  for GlobalKnob in FGlobalKnobList do
    GlobalKnob.Init;

  for i := 0 to NSLOTS - 1 do
    FSlotArray[i].Init;

  FSelectedParamPage := 0;
  FSelectedParamPageColumn := 0;
end;

procedure TG2Perf.InitMsg( aMsg : TG2SendMessage);
begin
  aMsg.Init;

  aMsg.WriteMessage( $01);
  aMsg.WriteMessage( CMD_REQ + CMD_SYS );
  aMsg.WriteMessage( PerfVersion);
end;

procedure TG2Perf.InitUndoMsg( aUndoMsg : TG2SendMessage);
begin
  aUndoMsg.Init;

  aUndoMsg.AddReversed := True;
  aUndoMsg.Offset := 5;

  aUndoMsg.WriteMessage( $01);
  aUndoMsg.WriteMessage( CMD_REQ + CMD_SYS );
  aUndoMsg.WriteMessage( PerfVersion);
end;

procedure TG2Perf.AddMsgGetPerfSettings( aMsg : TG2SendMessage);
begin
  add_log_line('Performance, settings patch_version ' + IntToStr( PerfVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_PERF_SETTINGS);
end;

procedure TG2Perf.AddMsgUnknown2( aMsg : TG2SendMessage);
begin
  add_log_line('Unknown 2, patch_version ' + IntToStr( PerfVersion), LOGCMD_HDR);

  aMsg.WriteMessage( M_UNKNOWN_2);
end;

procedure TG2Perf.AddMsgSelectSlot( aMsg : TG2SendMessage; aSlotIndex: byte);
begin
  add_log_line('Select slot patch_version ' + IntToStr( PerfVersion) + ', Slot ' + IntToStr(aSlotIndex), LOGCMD_HDR);

  aMsg.WriteMessage( S_SEL_SLOT);
  aMsg.WriteMessage( aSlotIndex);
end;

procedure TG2Perf.AddMsgSetMasterClockBPM( aMsg : TG2SendMessage; aBPM: byte);
begin
  add_log_line('Set master clock BPM message', LOGCMD_HDR);

  aMsg.WriteMessage( S_SET_MASTER_CLOCK);
  aMsg.WriteMessage( $FF); // Unknown
  aMsg.WriteMessage( $01); // $00 Set clock run $01 Set clock BPM
  aMsg.WriteMessage( aBPM);
end;

procedure TG2Perf.AddMsgSetMasterClockRun( aMsg : TG2SendMessage; aStart: boolean);
begin
  add_log_line('Set master clock run message', LOGCMD_HDR);

  aMsg.WriteMessage( S_SET_MASTER_CLOCK);
  aMsg.WriteMessage( $FF); // Unknown
  aMsg.WriteMessage( $00); // $00 Set clock run $01 Set clock BPM
  if aStart then
    aMsg.WriteMessage( $01)
  else
    aMsg.WriteMessage( $00);
end;

procedure TG2Perf.AddMsgSetPerfSettings( aMsg : TG2SendMessage);
var Chunk : TPatchChunk;
begin
  add_log_line('Upload performance settings', LOGCMD_HDR);

  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := FWConnection.Log;

    WriteSettings( Chunk);
  finally
    Chunk.Free;
  end;
end;

procedure TG2Perf.AddMsgSetPerfName( aMsg : TG2SendMessage; aPerfName : string);
begin
  add_log_line('Performance name', LOGCMD_HDR);

  aMsg.WriteMessage( C_PERF_NAME);
  aMsg.WriteClaviaString( aPerfName);
end;

procedure TG2Perf.AddMessAssignGlobalKnobs(aModule: IG2Module;
  const aPageIndex: byte; aSendMessage, aUndoMessage: TG2SendMessage);
var
  NewKnobIndex: integer;
  Param: IG2Param;
  ParamList: TList<IG2Param>;
begin
  ParamList := aModule.CreateAssignableParamList;
  try
    for Param in ParamList do
    begin
      NewKnobIndex := Param.DefaultKnobIndex;
      if NewKnobIndex >= 0 then
      begin
        NewKnobIndex := NewKnobIndex + aPageIndex * 8;
        Param.AddMessAssignGlobalKnob(FGlobalKnobList[NewKnobIndex], aSendMessage,
          aUndoMessage);
      end;
    end;
  finally
    ParamList.Free;
  end;
end;

procedure TG2Perf.AddMsgGetGlobalKnobs( aMsg : TG2SendMessage);
begin
  add_log_line('Global knobs', LOGCMD_HDR);

  aMsg.WriteMessage( Q_GLOBAL_KNOBS);
end;

procedure TG2Perf.AddMsgSelectGlobalParamPage( aMsg : TG2SendMessage;
  aPageIndex: byte);
begin
  add_log_line('Select global parameter page', LOGCMD_HDR);

  aMsg.WriteMessage( S_SEL_GLOBAL_PAGE);
  aMsg.WriteMessage( aPageIndex);
end;

function TG2Perf.ProcessResponseMsg( aSender : TObject; aMsg: TG2Message): boolean;
var Version, SubCmd, SlotIndex : byte;
begin
  // Return True if processed
  Result := False;

  aMsg.Read(Version, 1);

  if Version = $40 then begin
    aMsg.Read( SubCmd, 1);
    case SubCmd of
    R_PATCH_VERSION_CHANGE :
          begin
            // Slot patch version changed. Reload slot
            aMsg.Read( SlotIndex, 1);
            if SlotIndex < 4 then
              Slot[ SlotIndex].ProcessMsgVersionChange( aMsg)
            else
              ProcessMsgVersionChange( aMsg);
          end;
      else begin
             add_log_line('Performance unknown command ' + IntToHex(SubCmd, 2), LOGCMD_ERR);
             Result := False;
           end;
    end;
  end else

    if Version = FPerfVersion then begin

      aMsg.Read( SubCmd, 1);
      case SubCmd of
      R_ASSIGNED_VOICES :
            begin
              ProcessMsgAssignedVoices( aMsg);
              Result := True;
            end;
      R_MIDI_CC :
            begin
              ProcessMsgMidiCC( aMsg);
              Result := True;
            end;
      S_SET_MASTER_CLOCK :
            begin
              ProcessMsgMasterClock( aMsg);
              Result := True;
            end;
      R_EXT_MASTER_CLOCK :
            begin
              ProcessMsgExtMasterClock( aMsg);
              Result := True;
            end;
      C_PERF_NAME,
      C_PERF_SETTINGS : // Strange, sometimes received after store : because patch bank location is in slot settings...
            begin // Performance settings
              ProcessMsgPerfChunk( aMsg);
              Result := True;
            end;
      else begin
             add_log_line('Performance unknown command ' + IntToHex(SubCmd, 2), LOGCMD_ERR);
             Result := False;
           end;
      end;
    end;
end;

function TG2Perf.ProcessSendMsg(aSender: TObject;
  aMsg: TG2Message): boolean;
var SubCmd, aSlot, b : byte;
    perf_name : string;
    Chunk : TPatchChunk;
    i : integer;
begin
  // Return True if processed
  Result := False;
  aMsg.Read( SubCmd, 1);
  case SubCmd of
    Q_PERF_SETTINGS :
         begin
           aMsg.Position := aMsg.Size;
           Result := True;

           DoPerfSettingsUpdate;
         end;
    S_SEL_SLOT :
         begin
           aMsg.Read( aSlot, 1);
           InitSelectedSlotIndex( aSlot);
           Result := True;
           DoSelectSlot;
         end;
    C_PERF_SETTINGS :
         begin
            Result := True;
            DoPerfSettingsUpdate;
         end;
    S_SET_PATCH :
          begin
            aMsg.Read(b, 1);
            aMsg.Read(b, 1);
            aMsg.Read(b, 1);
            perf_name := '';
            while (length(perf_name) < 16) and (aMsg.Read(b, 1) = 1) and (b <> 0) do begin
              perf_name := perf_name + Char(b);
            end;
            aMsg.Read(b, 1); // $1a
            aMsg.Read(b, 1); // $29

            if b <> C_PERF_NAME then
              raise Exception.Create('Performance name chunk expected.');

            perf_name := '';
            while (perf_name.Length < 16) and (aMsg.Read(b, 1) = 1) and (b <> 0) do begin
              perf_name := perf_name + Char(b);
            end;
            SetPerformanceName( perf_name);

            Chunk := TPatchChunk.Create(aMsg);
            try
              Chunk.Log := FWConnection.Log;

              Chunk.ReadChunk;
              if Chunk.FId = C_PERF_SETTINGS then begin

                Init;
                Read( Chunk); // Parse patch

                Result := True;
              end else
                raise Exception.Create('Performance chunk expected');

            finally
              Chunk.Free;
            end;

            // Must be called AFTER moduleindex is known
            for i := 0 to NSLOTS - 1 do begin
              //FWConnection.PatchPart[i, LOCATION_PATCH].InitParameters;
              FWConnection.Patch[i].InitNames;
              FWConnection.Slot[i].DoPatchUpdate;
            end;

            DoPerfUpdate;
          end;
    {S_ASS_GLOBAL_KNOB : // Assign knob in global page
            begin
              BitReader := TBitReader.Create;
              try
                aSlotIndex     := BitReader.ReadBits( aMsg, 4);
                aLocation      := BitReader.ReadBits( aMsg, 2);
                aUnknown       := BitReader.ReadBits( aMsg, 2);
                aModuleIndex   := BitReader.ReadBits( aMsg, 8);
                aParamIndex    := BitReader.ReadBits( aMsg, 8);
                aUnknown       := BitReader.ReadBits( aMsg, 8);
                aKnobIndex     := BitReader.ReadBits( aMsg, 8);

                if TLocationType(aLocation) = ltPatch then begin
                  Slot[aSlotIndex].Patch.Parameter[ aModuleIndex, aParamIndex].AssignGlobalKnob( self, aSlotIndex, aKnobIndex);
                end else begin
                  Module := Slot[aSlotIndex].Patch.GetModule( aLocation, aModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ aParamIndex].AssignGlobalKnob( self, aSlotIndex, aKnobIndex);
                  end else
                    add_log_line( 'ModuleIndex ' + IntToStr( aModuleIndex) + ' not found.', LOGCMD_ERR);
                end;

                if assigned(G2.OnAssignGlobalKnob) then
                  G2.OnAssignGlobalKnob(self, G2.ID, aKnobIndex);
              finally
                BitReader.Free;
              end;
            end;
    S_DEASS_GLOB_KNOB : // Deassign global knob
            begin
              aMsg.Read( aUnknown, 1);
              aMsg.Read( aKnobIndex, 1);

              GlobalKnob := GlobalKnobList.Items[ aKnobIndex];

              if GlobalKnob.IsAssigned = 1 then begin
                if GlobalKnob.Location = TBits2(ltPatch) then begin
                  Param := Slot[aSlotIndex].Patch.Parameter[ GlobalKnob.ModuleIndex, GlobalKnob.ParamIndex];
                  if assigned(Param) then
                    Param.DeassignGlobalKnob( self, aKnobIndex);
                end else begin
                  Module := Slot[aSlotIndex].Patch.GetModule( GlobalKnob.Location, GlobalKnob.ModuleIndex);
                  if assigned( Module) then begin
                    Module.Parameter[ GlobalKnob.ParamIndex].DeassignGlobalKnob( self, aKnobIndex);
                  end;
                end;
              end;

              if assigned(G2.OnDeassignGlobalKnob) then
                G2.OnDeassignGlobalKnob(self, G2.ID, aKnobIndex);
            end;
    S_SEL_GLOBAL_PAGE : // Select global parameter page
            begin
              // TODO
            end;}
  end;
end;

procedure TG2Perf.ProcessMsgAssignedVoices(aMsg: TG2Message);
var b : byte;
begin
  aMsg.Read(b, 1);
  Slot[0].AssignedVoices := b;
  aMsg.Read(b, 1);
  Slot[1].AssignedVoices := b;
  aMsg.Read(b, 1);
  Slot[2].AssignedVoices := b;
  aMsg.Read(b, 1);
  Slot[3].AssignedVoices := b;

  DoAfterGetAssignedVoices;
end;

procedure TG2Perf.ProcessMsgExtMasterClock(aMsg: TG2Message);
var b, hb, lb : byte;
begin
  // External clock
  aMsg.Read( b, 1);
  aMsg.Read( hb, 1);
  aMsg.Read( lb, 1);

  FLastMidiClock := hb*256+lb;

  DoMidiClockReceive( FLastMidiClock);
end;

procedure TG2Perf.ProcessMsgVersionChange( aMsg: TG2Message);
var b : byte;
begin
  aMsg.Read( b, 1);
  PatchVersion := b;
  DoAfterRetreivePatch( 4, 0, 0)
end;

procedure TG2Perf.ProcessMsgPerfChunk( aMsg: TG2Message);
var b : byte;
    perf_name : string;
    Chunk : TPatchChunk;
begin
  // its actially first perf name and then the chunk
  perf_name := '';
  while (length(perf_name) < 16) and (aMsg.Read(b, 1) = 1) and (b <> 0) do begin
    perf_name := perf_name + Char(b);
  end;
  PerformanceName := perf_name;

  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := FWConnection.Log;

    Chunk.ReadChunk;
    // Parse the performance settings
    Read( Chunk);

  finally
    Chunk.Free;
  end;
end;

procedure TG2Perf.ProcessMsgGlobalKnobsChunk( aMsg: TG2Message);
var Chunk : TPatchChunk;
begin
  aMsg.Position := aMsg.Position - 1;
  Chunk := TPatchChunk.Create( aMsg);
  try
    Chunk.Log := FWConnection.Log;

    Chunk.ReadChunk;
    // Parse the performance settings
    Read( Chunk);
  finally
    Chunk.Free;
  end;
end;

procedure TG2Perf.ProcessMsgMasterClock(aMsg: TG2Message);
var b : byte;
begin
  aMsg.Read(b, 1); // $FF Unknown
  aMsg.Read(b, 1); // $00 : Clock run $01 : Clock BPM
  case b of
  $00 : begin
          aMsg.Read(b, 1); // $00 : Off, $01 : On
          SetMasterClockRun( b);
          DoClockRunChange( b=1);
        end;
  $01 : begin
          aMsg.Read(b, 1);
          SetMasterClock( b);
          DoClockBPMChange( b);
        end;
  end;
end;

procedure TG2Perf.ProcessMsgMidiCC(aMsg: TG2Message);
var b : byte;
begin
  // Midi CC
  // 82 01 04 00 80 00 3f 30 40 00 00 00 00 00 00 00  = CC #63
  aMsg.Read( b, 1);
  aMsg.Read( FLastMidiCC, 1);

  DoMidiCCReceive( FLastMidiCC);
end;

procedure TG2Perf.ProcessMsgGlobalKnobAssign(aMsg: TG2Message);
var SlotIndex, LocationIndex, ModuleIndex, ParamIndex, KnobIndex : byte;
    BitReader: TBitReader;
    GlobalKnob: IG2Knob;
    Param: IG2Param;
begin
  BitReader := TBitReader.Create;
  try
    SlotIndex     := BitReader.ReadBits( aMsg, 4);
    LocationIndex := BitReader.ReadBits( aMsg, 2);
    BitReader.ReadBits( aMsg, 2); // Unknown1
    ModuleIndex   := BitReader.ReadBits( aMsg, 8);
    ParamIndex    := BitReader.ReadBits( aMsg, 8);
    BitReader.ReadBits( aMsg, 8); // Unknown2
    KnobIndex     := BitReader.ReadBits( aMsg, 8);

    Param := Slot[SlotIndex].Patch.FindParam(LocationIndex, ModuleIndex, ParamIndex);

    GlobalKnob := FGlobalKnobList[KnobIndex];
    //GlobalKnob.SlotIndex := SlotIndex;
    //GlobalKnob.LocationIndex := LocationIndex;
    //GlobalKnob.ModuleIndex := ModuleIndex;
    //GlobalKnob.ParamIndex := ParamIndex;
    //GlobalKnob.IsAssigned := 1;
    GlobalKnob.Param := Param;

    NotifyObservers(EvtAssignGlobalKnob, GlobalKnob);
  finally
    BitReader.Free;
  end;
end;

procedure TG2Perf.ProcessMsgGlobalKnobDeassign(
  aMsg: TG2Message);
var Unknown, KnobIndex : byte;
    GlobalKnob : IG2Knob;
begin
  aMsg.Read( Unknown, 1);
  aMsg.Read( KnobIndex, 1);

  GlobalKnob := FGlobalKnobList[ KnobIndex];
  if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then begin
    GlobalKnob.IsAssigned := 0;

    NotifyObservers(EvtDeassignGlobalKnob, GlobalKnob);
  end;
end;

function TG2Perf.GetSelectedParamPage: integer;
begin
  Result := FSelectedParamPage;
end;

function TG2Perf.GetSelectedParamPageColumn: integer;
begin
  Result := FSelectedParamPageColumn;
end;

function TG2Perf.GetSelectedSlotIndex: TBits2;
begin
  Result := FSelectedSlotIndex;
end;

function TG2Perf.GetSlot( const aIndex : byte): IG2Slot;
begin
  Result := FSlotArray[ aIndex]
end;

procedure TG2Perf.ClearKnobs( const aSlotIndex : integer);
var i : integer;
begin
  for i := 0 to FGlobalKnobList.Count - 1 do
    if FGlobalKnobList[i].SlotIndex = aSlotIndex then begin
      FGlobalKnobList[i].Init;
  end;
end;

procedure TG2Perf.DoSelectSlot;
begin
  NotifyObservers(EvtSelectSlot, self as IG2Perf);
end;

function TG2Perf.FindKnob(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex: byte): IG2Knob;
var i : integer;
begin
  i := 0;
  while (i < FGlobalKnobList.Count) and not(( FGlobalKnobList[i].IsAssigned = 1)
                        and ( FGlobalKnobList[i].SlotIndex = aSlotIndex)
                        and ( FGlobalKnobList[i].LocationIndex = aLocationIndex)
                        and ( FGlobalKnobList[i].ModuleIndex = aModuleIndex)
                        and ( FGlobalKnobList[i].ParamIndex = aParamIndex) ) do
    inc(i);

  if (i < FGlobalKnobList.Count) then begin
    Result := FGlobalKnobList.Items[i]
  end else
    Result := nil;
end;

procedure TG2Perf.DoAfterGetAssignedVoices;
begin
  NotifyObservers(EvtAfterGetAssignedVoices, self as IG2Perf);
end;

procedure TG2Perf.DoAfterPerfInit;
begin
  NotifyObservers(EvtAfterPerfInit, self as IG2Perf);
end;

procedure TG2Perf.DoAfterRetreivePatch(Slot, Bank, Patch: byte);
begin
  NotifyObservers(EvtAfterRetreivePatch, self as IG2Perf);
end;

procedure TG2Perf.DoClockBPMChange(aBPM: byte);
begin
  NotifyObservers(EvtClockBPMChange, self as IG2Perf);
end;

procedure TG2Perf.DoClockRunChange(aRun: boolean);
begin
  NotifyObservers(EvtClockRunChange, self as IG2Perf);
end;

procedure TG2Perf.DoMidiCCReceive(aMidiCC: byte);
begin
  NotifyObservers(EvtMidiCCRecieve, self as IG2Perf);
end;

procedure TG2Perf.DoMidiClockReceive(aValue: integer);
begin
  NotifyObservers(EvtMidiClockReceive, self as IG2Perf);
end;

procedure TG2Perf.DoPerfSettingsUpdate;
begin
  NotifyObservers(EvtPerfsSettingsUpdate, self as IG2Perf);
end;

procedure TG2Perf.DoPerfUpdate;
begin
  NotifyObservers(EvtPerfUpdate, self as IG2Perf);
end;

procedure TG2Perf.InitSelectedSlotIndex(const aValue: TBits2);
begin
  FSelectedSlotIndex := aValue;
end;

procedure TG2Perf.StartInit(aConnection: IG2Connection;
  const aStartCommAfterInit: boolean);
begin
  if not aConnection.Connected then
    exit;

  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FFinishMsg := aConnection.OnNextMsg;
  aConnection.OnNextMsg := InitSeq;

  Init;

  aConnection.PerfGetSettings;
end;

procedure TG2Perf.InitSeq(Sender: TObject;
  aConnection: IG2Connection);
begin
  FInitMidiStep := 0;
  while (FInitStep <= 10) do begin
    case FInitStep of
     0 : begin
           aConnection.PerfGetSettings;
           inc(FInitStep);
           exit;
         end;
     1 : begin
           aConnection.PerfUnknown2Message;
           inc(FInitStep);
           exit;
         end;
     2 : begin
           Slot[0].StartInit(aConnection, False);
           inc(FInitStep);
           exit;
         end;
     3 : begin
           Slot[1].StartInit(aConnection, False);
           inc(FInitStep);
           exit;
         end;
     4 : begin
           Slot[2].StartInit(aConnection, False);
           inc(FInitStep);
           exit;
         end;
     5 : begin
           Slot[3].StartInit(aConnection, False);
           inc(FInitStep);
           exit;
         end;
     6 : begin
           aConnection.SynthGetAssignedVoices;
           inc(FInitStep);
           exit;
         end;
     7 : begin
           //(G2 as TG2USB).SendGetMasterClockMessage;
           aConnection.PerfGetGlobalKnobs;
           inc(FInitStep);
           exit;
         end;
     8 : begin
           if FWConnection.Synth.AutoAssignMidi then begin
             if StartInitMidi(aConnection) then
               exit;
           end else
             inc(FInitStep);
         end;
     9 : begin
           //SendGetGlobalKnobsMessage;
           aConnection.SynthGetMasterClock;
           inc(FInitStep);
           if not FStartCommAfterInit then begin
             aConnection.OnNextMsg := FFinishMsg;
             DoAfterPerfInit;
           end;
           exit;
         end;
     10 : begin
           aConnection.SynthStartStopCommunication(START_COMM);
           inc(FInitStep);
           aConnection.OnNextMsg := FFinishMsg;
           DoAfterPerfInit;
           exit;
         end;
    end;
  end;
end;

function TG2Perf.StartInitMidi(aConnection: IG2Connection): boolean;
var
  Knob: IG2Knob;
  MidiCC: byte;
  KnobIndex: byte;
  Patch: IG2Patch;

  function AssignMidiCC(aPatch : IG2Patch; aParam : IG2Param; aMidiCC : byte): boolean;
  begin
    // Returns True if as message is send
    Result := False;
    if assigned( aParam.Controller) then begin
      if aParam.Controller.MidiCC <> aMidiCC then begin
        aConnection.ParamControllerDeassign(
          aParam.SlotIndex,
          aParam.LocationIndex,
          aParam.ModuleIndex,
          aParam.ParamIndex);
        Result := True;
      end else
        inc(FInitMidiStep); // Already connected to the CC
    end else begin
      aConnection.ParamControllerAssign(
        aParam.SlotIndex,
        aParam.LocationIndex,
        aParam.ModuleIndex,
        aParam.ParamIndex,
        aMidiCC);
      Result := True;
      inc(FInitMidiStep);
    end;
  end;

begin
  // FControlType : TG2ControlType; // 0=Knob 1=Button
  // FKnob : integer;
  // FMidiCC : integer;

  // Return True if a message is send
  Result := False;

  if FInitMidiStep < Synth.MidiToKnobList.Count then begin
    MidiCC := Synth.MidiToKnobList[FInitMidiStep].Controller;
    KnobIndex := Synth.MidiToKnobList[FInitMidiStep].Knob;

    case Synth.MidiToKnobList[FInitMidiStep].Source of
      g2psGlobal :
        begin
          Knob := FGlobalKnobList[ KnobIndex];
          case Synth.MidiToKnobList[FInitMidiStep].ControlType of
            g2ctKnob :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Param) then begin
                  Patch := FWConnection.Patch[Knob.SlotIndex];
                  if AssignMidiCC( Patch, Knob.Param, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Param) and assigned(Knob.Param.ButtonParam) then begin
                  Patch := FWConnection.Patch[Knob.SlotIndex];
                  if AssignMidiCC( Patch, Knob.Param.ButtonParam, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            else
              inc(FInitMidiStep);
          end;
        end;
      else
       inc(FInitMidiStep);
    end;
  end else
    inc(FInitStep);
end;

procedure TG2Perf.SetSelectedParamPage(const aValue: integer);
begin
  FSelectedParamPage := aValue;
end;

procedure TG2Perf.SetSelectedParamPageColumn(const aValue: integer);
begin
  FSelectedParamPageColumn := aValue;
end;

procedure TG2Perf.SetSelectedSlotIndex(const aValue : TBits2);
begin
  InitSelectedSlotIndex( aValue);
end;

procedure TG2Perf.SetPerformanceName(const aValue : string);
begin
  if aValue.Length > 16 then
    raise Exception.Create('Name length must be 16 max.');

  if aValue = '' then
    FPerformanceName := 'No name'
  else
    FPerformanceName := aValue;
end;

procedure TG2Perf.SetPerfVersion(const aValue: byte);
begin
  FPerfVersion := aValue;
end;

procedure TG2Perf.SetMasterClock(const aValue : TBits8);
begin
  if (aValue < 30) or (aValue > 240) then
    raise Exception.Create('Master clock must be between 30 and 240.');

  FMasterClock := aValue;
end;

procedure TG2Perf.SetMasterClockRun(const aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Master clock run must be 0 or 1.');

  FMasterClockRun := aValue;
end;

procedure TG2Perf.SetKeyboardRangeEnabled(const aValue : TBits8);
begin
  if (aValue > 1) then
    raise Exception.Create('Keyboard range enable must be 0 or 1.');

  FKeyboardRangeEnabled := aValue;
end;

procedure TG2Perf.Read( aChunk : TPatchChunk);
var einde : boolean;
    b : byte;
    i, PatchCount : integer;
begin
  // FUnknown              : TBits8;
  // FPerformanceName      : 16TBits8 or ends with #0
  // FKeyboardRangeEnabled : TBits7;
  // FMasterClock          : TBits7;
  // FMasterClockRun       : TBits7;
  // FSettings             : array of TPerformanceSetting;

  PatchCount := 0;
  einde := False;
  repeat
    case aChunk.FId of
      C_PERF_NAME :
        begin
          FPerformanceName := aChunk.ReadName;
        end;
      C_PERF_SETTINGS :
        begin // performance description
          FUnknown2             := aChunk.ReadBits( 8);
          FUnknown3             := aChunk.ReadBits( 4);
          b                     := aChunk.ReadBits( 2);
          InitSelectedSlotIndex(b);
          FUnknown4             := aChunk.ReadBits( 2);

          FKeyboardRangeEnabled := aChunk.ReadBits( 8);
          FMasterClock          := aChunk.ReadBits( 8);
          FUnknown5             := aChunk.ReadBits( 8);
          FMasterClockRun       := aChunk.ReadBits( 8);
          FUnknown6             := aChunk.ReadBits( 8);
          FUnknown7             := aChunk.ReadBits( 8);

          if assigned(aChunk.Log) then begin
            aChunk.Log.add_log_line('Perf settings:', LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown2), LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown3), LOGCMD_NUL);
            aChunk.Log.add_log_line('Selected slot : ' + IntToStr(FSelectedSlotIndex), LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown4), LOGCMD_NUL);
            aChunk.Log.add_log_line('Keyb range : ' + IntToStr(FKeyboardRangeEnabled), LOGCMD_NUL);
            aChunk.Log.add_log_line('Materclock : ' + IntToStr(FMasterClock), LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown5), LOGCMD_NUL);
            aChunk.Log.add_log_line('MClock run : ' + IntToStr(FMasterClockRun), LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown6), LOGCMD_NUL);
            aChunk.Log.add_log_line('Unknown : ' + IntToStr(FUnknown7), LOGCMD_NUL);
          end;

          for i := 0 to NSLOTS - 1 do
            GetSlot( i).Read( aChunk);
          PatchCount := 0;
        end;
      C_PATCH_DESCR :
        begin // Patch description
          GetSlot( PatchCount).GetPatch.Read( aChunk);
          inc(PatchCount);
          //FPatchDescription.read( aChunk);
        end ;
      C_KNOBS_GLOBAL :
        begin // Unknown
          ReadKnobList( FGlobalKnobList, aChunk);
        end ;
    end;

    if not einde then begin
      if aChunk.FStream.Position < aChunk.FStream.Size - 2 then
        aChunk.ReadChunk
      else
        einde := true;
    end;

  until einde;
  //InitKnobs;
end;

procedure TG2Perf.ReadKnobList(aKnobList: TList<IG2Knob>;
  aChunk: TPatchChunk);
var i, KnobCount: integer;
    Param: IG2Param;
    IsAssigned    : TBits1;
    LocationIndex : TBits2;
    ModuleIndex   : TBits8;
    ParamIndex    : TBits7;
    SlotIndex     : TBits2;
begin
  KnobCount := aChunk.ReadBits( 16);

  if assigned(aChunk.Log) then begin
    aChunk.Log.add_log_line('KnobList, count = ' + IntToStr(KnobCount), LOGCMD_NUL);
    aChunk.Log.add_log_line('Knb Loc Mod Isl Par Slt', LOGCMD_NUL);
  end;

  for i := 0 to KnobCount - 1 do begin
    IsAssigned    := aChunk.ReadBits( 1);
    if IsAssigned = 1 then begin

      LocationIndex := aChunk.ReadBits( 2);
      ModuleIndex := aChunk.ReadBits( 8);
      aChunk.ReadBits( 2); // IsLed
      ParamIndex  := aChunk.ReadBits( 7);
      SlotIndex   := aChunk.ReadBits( 2);

      Param := Slot[SlotIndex].Patch.FindParam(LocationIndex, ModuleIndex, ParamIndex);
      FGlobalKnobList[i].Param := Param;
    end else
      FGlobalKnobList[i].Param := nil;

    if assigned( aChunk.Log) and (FGlobalKnobList[i].IsAssigned = 1) then
      aChunk.Log.add_log_line( Format('%3d %3d %3d %3d %3d %3d', [i, ord(FGlobalKnobList[i].LocationIndex), FGlobalKnobList[i].ModuleIndex, FGlobalKnobList[i].IsLed, FGlobalKnobList[i].ParamIndex, FGlobalKnobList[i].SlotIndex]), LOGCMD_NUL);
  end;
end;

procedure TG2Perf.WriteSettings( aChunk : TPatchChunk);
var i : integer;
begin
  aChunk.WriteBits( FUnknown2,             8);
  aChunk.WriteBits( FUnknown3,             4);
  aChunk.WriteBits( FSelectedSlotIndex,    2);
  aChunk.WriteBits( FUnknown4,             2);


  aChunk.WriteBits( FKeyboardRangeEnabled, 8);
  aChunk.WriteBits( FMasterClock,          8);
  aChunk.WriteBits( FUnknown5,             8);
  aChunk.WriteBits( FMasterClockRun,       8);
  aChunk.WriteBits( FUnknown6,             8);
  aChunk.WriteBits( FUnknown7,             8);

  for i := 0 to NSLOTS - 1 do
    GetSlot( i).Write( aChunk);
  aChunk.WriteChunk( C_PERF_SETTINGS);
end;


procedure TG2Perf.Write( aChunk : TPatchChunk; aVariationCount : byte);
var i : integer;
begin
  // Write patch data
  for i := 0 to NSLOTS - 1 do begin
    GetSlot( i).GetPatch.Write( aChunk, aVariationCount);
  end;

  WriteChunk( C_KNOBS_GLOBAL, aChunk);
end;

procedure TG2Perf.WriteChunk(aID: byte; aChunk: TPatchChunk);
begin
  case aID of
    C_KNOBS_GLOBAL :
      begin
        WriteKnobList( FGlobalKnobList, aChunk);
        aChunk.WriteChunk( C_KNOBS_GLOBAL);
      end;
  end;
end;

procedure TG2Perf.WriteKnobList(aKnobList: TList<IG2Knob>;
  aChunk: TPatchChunk);
var GlobalKnob : IG2Knob;
begin
  aChunk.WriteBits(FGlobalKnobList.Count, 16);
  for GlobalKnob in FGlobalKnobList do
    GlobalKnob.Write( aChunk);
end;

function TG2Perf.LoadFromFile(aStream : TStream): Boolean;
var
  sl : TStringList;
  Chunk : TPatchChunk;
  b  : byte;
  bm, bl : Byte;
  s  : string;
  Crc : Word;
begin
  Result := False;

  Chunk := TPatchChunk.Create( aStream);
  sl := TStringList.Create;
  try
    Chunk.Log := FWConnection.Log;

    s := '';
    aStream.Position := 0;
    while aStream.Position < aStream.Size do begin
      aStream.Read( b, SizeOf( b));
      case b of
      $0d :; // just skip it
      $0a : begin
               sl.Add(s);
               s := '';
             end;
      $00 : Break;
      else
        s := s + Char(b);
      end;
    end;
    if s <> '' then
      sl.Add(s);

    Init;
    Chunk.ReadBuffer( 2);
    PatchVersion := Chunk.ReadBits( 8);
    PatchType := Chunk.ReadBits( 8);
    Chunk.ReadChunk;
    Read( Chunk);

    aStream.Read(bm, 1);
    aStream.Read(bl, 1);
    Crc := bm * 256 + bl;

    if Crc <> Chunk.FReadCrc then
      raise Exception.Create('Crc error.');

    Result := True;

  finally
    Chunk.Free;
    sl.Free;
  end;
end;

procedure TG2Perf.SaveToFile( aStream : TStream);
var Chunk : TPatchChunk;
begin
  Chunk := TPatchChunk.Create( aStream);
  try
    AddHeaderInfo( pftPerf, aStream);

    Chunk.WriteBits(PatchVersion, 8);
    Chunk.WriteBits(Patchtype, 8);
    Chunk.Flush;

    //MaxVariations := 9;
    WriteSettings(Chunk);
    Write( Chunk, N_VARIATIONS); // 9 Variations are written to file
    Chunk.WriteCrc(aStream);

  finally
    Chunk.Free;
  end;
end;

{$IFDEF MSWINDOWS}
procedure TG2Perf.SaveAsFXB( aStream : TStream);
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  MemStream := TMemoryStream.Create;
  try
    SaveToFile(MemStream);

    StringToByteArray( 'CcnK', FXBHeader.chunkMagic, 4);
    FXBHeader.byteSize    := SwapBytes(152 + MemStream.Size);
    StringToByteArray( 'FBCh', FXBHeader.fxMagic, 4);
    FXBHeader.version     := SwapBytes(1);
    StringToByteArray( 'NMG2', FXBHeader.fxID, 4);
    FXBHeader.fxVersion   := SwapBytes(1);
    FXBHeader.numPrograms := SwapBytes(1);
    Fillchar(FXBHeader.Future, 128, #0);

    FXBHeader.chunkSize   := SwapBytes(MemStream.Size);

    aStream.Write(FXBHeader, SizeOf(FXBHeader));
    aStream.Write(MemStream.Memory^, MemStream.Size);

  finally
    MemStream.Free;
  end;
end;

function TG2Perf.LoadFromFXB( aStream : TStream): boolean;
var FXBHeader : TFXBHeader;
    MemStream : TMemoryStream;

    function SwapBytes(Value: Cardinal): Cardinal; register;
    asm
      BSWAP  EAX
    end;
    {begin
      // TDOD
    end;}

begin
  Result := False;
  MemStream := TMemoryStream.Create;
  try
    aStream.Read( FXBHeader.chunkMagic, SizeOf( FXBHeader.chunkMagic));
    if ByteArrayToString( FXBHeader.chunkMagic, 4) <> 'CcnK' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.byteSize, SizeOf( FXBHeader.byteSize));
    FXBHeader.byteSize := SwapBytes(FXBHeader.byteSize);
    aStream.Read( FXBHeader.fxMagic, SizeOf( FXBHeader.fxMagic));
    if ByteArrayToString( FXBHeader.chunkMagic, 4) <> 'FBCh' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.version, SizeOf( FXBHeader.version));
    FXBHeader.version := SwapBytes(FXBHeader.version);
    aStream.Read( FXBHeader.fxID, SizeOf( FXBHeader.fxID));
    if ByteArrayToString( FXBHeader.fxID, 4) <> 'NMG2' then
      raise Exception.Create('Unknown filetype');
    aStream.Read( FXBHeader.fxVersion, SizeOf( FXBHeader.fxVersion));
    FXBHeader.fxVersion := SwapBytes(FXBHeader.fxVersion);
    aStream.Read( FXBHeader.numPrograms, SizeOf( FXBHeader.numPrograms));
    FXBHeader.numPrograms := SwapBytes(FXBHeader.numPrograms);
    aStream.Read( FXBHeader.Future, 128);
    aStream.Read( FXBHeader.chunkSize, SizeOf(FXBHeader.chunkSize));
    FXBHeader.chunkSize := SwapBytes(FXBHeader.chunkSize);

    LoadFromFile( aStream);

    Result := True;
  finally
    MemStream.Free;
  end;
end;
{$ENDIF}

function TG2Perf.GetNoOffExtendedModules : integer;
var i : integer;
begin
  Result := 0;
  for i := 0 to NSLOTS - 1 do
    Result := Result + GetSlot( i).GetPatch.GetNoOffExtendedModules;
end;


function TG2Perf.GetPerformanceName: string;
begin
  Result := FPerformanceName;
end;

function TG2Perf.GetPerfVersion: Byte;
begin
  Result := FPerfVersion;
end;

procedure TG2Perf.DeleteModuleFromPerf(const aSlotIndex,
   aLocationIndex : byte; aModule: IG2Module);
var aModuleIndex : Byte;
    GlobalKnob : IG2Knob;
begin
  aModuleIndex := aModule.ModuleIndex;

  //FGlobalKnobList.DeleteModule( aSlotIndex, ord(aLocation), aModuleIndex);

  for GlobalKnob in FGlobalKnobList do
    if (GlobalKnob.ModuleIndex = aModuleIndex) and (GlobalKnob.LocationIndex = aLocationIndex) and (GlobalKnob.SlotIndex = aSlotIndex) then
      GlobalKnob.IsAssigned := 0;
end;

procedure TG2Perf.add_log_line(tekst: string; log_cmd: integer);
begin
  FWConnection.AddLog( tekst, log_cmd);
end;

function TG2Perf.AssignGlobalKnobInPerf(const aKnobIndex, aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex : byte): IG2Knob;
var Knob: IG2Knob;
    Param: IG2Param;
begin
  Result := nil;

  if aKnobIndex < FGlobalKnobList.Count then begin

    Param := Slot[aSlotIndex].Patch.FindParam(aLocationIndex, aModuleIndex, aParamIndex);
    Knob := FGlobalKnobList.Items[aKnobIndex];
    //Knob.IsAssigned := 1;
    //Knob.LocationIndex := aLocationIndex;
    //Knob.ModuleIndex := aModuleIndex;
    //Knob.IsLed := 0;
    //Knob.ParamIndex := aParamIndex;
    //Knob.SlotIndex := aSlotIndex;
    Knob.Param := Param;

    Result := Knob;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

procedure TG2Perf.DeassignGlobalKnobInPerf(const aKnobIndex: byte);
var Knob : IG2Knob;
begin
  if aKnobIndex < FGlobalKnobList.Count then begin
    Knob := FGlobalKnobList.Items[ aKnobIndex];
    Knob.IsAssigned := 0;
  end else
    raise Exception.Create('Knob index ' + IntToStr(aKnobIndex) + ' out of range.');
end;

function TG2Perf.GetSynth: IG2Synth;
begin
  if assigned(FWConnection) then
    Result := FWConnection.Synth
  else
    Result := nil;
end;

function TG2Perf.GetGlobalKnobList: TList<IG2Knob>;
begin
  Result := FGlobalKnobList;
end;

function TG2Perf.GetKeyboardRangeEnabled: TBits8;
begin
  Result := FKeyboardRangeEnabled;
end;

function TG2Perf.GetLastMidiCC: byte;
begin
  Result := FLastMidiCC;
end;

function TG2Perf.GetLastMidiClock: integer;
begin
  Result := FLastMidiClock;
end;

function TG2Perf.GetMasterClock: TBits8;
begin
  Result := FMasterClock;
end;

function TG2Perf.GetMasterClockRun: TBits8;
begin
  Result := FMasterClockRun;
end;

procedure TG2Perf.NotifyObservers(aG2Event: TG2Event; aG2Object: IG2Object);
begin
  FWConnection.NotifyObservers(aG2Event, aG2Object);
end;

end.
