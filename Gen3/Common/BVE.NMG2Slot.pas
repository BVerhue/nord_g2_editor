unit BVE.NMG2Slot;

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
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  TPatchLoadData = packed array[0..26] of byte;

  TG2Slot = class(TG2ObservedObject, IG2Slot)
  private
    [Weak] FWConnection : IG2Connection;

    FSlotIndex          : byte;

    FPatch              : IG2Patch;

    FInitStep           : integer;
    FInitMidiStep       : integer;
    FStartCommAfterInit : boolean;
    FFinishMsg          : TOnNextMsgEvent;

    FPatchVersion       : Byte;

    FAssignedVoices     : Byte;
    FPatchLoadVAData    : TPatchLoadData;
    FPatchLoadFXData    : TPatchLoadData;

    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4           : TBits8; // array[0..4] of
    FEnabled            : TBits8;
    FKeyboard           : TBits8;
    FHold               : TBits8;
    FKeyboardRangeFrom  : TBits8;
    FKeyboardRangeTo    : TBits8;
    FSlot               : TBits8;
    FBankIndex          : TBits8;
    FPatchIndex         : TBits8;

    FPatchName          : string;

    FInitializing       : boolean;

    FOperationMode      : TOperationMode; // for ui control

    procedure   SetPatchVersion(const aValue : byte);
    procedure   SetSlotIndex(const aValue : byte);
    procedure   SetEnabled(const aValue : TBits8);
    procedure   SetHold(const aValue : TBits8);
    procedure   SetKeyboard(const aValue : TBits8);
    procedure   SetKeyboardRangeTo(const aValue : TBits8);
    procedure   SetKeyboardRangeFrom(const aValue : TBits8);
    procedure   SetAssignedVoices(const aValue : byte);
    procedure   SetInitializing(const aValue : boolean);

    function    GetConnection: IG2Connection;
    function    GetLog: IG2Log;
    function    GetPatchVersion : byte;
    function    GetPatch: IG2Patch;
    function    GetEnabled : TBits8;
    function    GetHold : TBits8;
    function    GetKeyboard : TBits8;
    function    GetBankIndex : byte;
    function    GetPatchIndex : byte;
    function    GetKeyboardRangeTo : TBits8;
    function    GetKeyboardRangeFrom : TBits8;
    function    GetPerf : IG2Perf;
    function    GetSynth : IG2Synth;
    function    GetAssignedVoices : byte;
    function    GetPatchloadMemVA : single;
    function    GetPatchloadMemFX : single;
    function    GetPatchloadCyclesVA : single;
    function    GetPatchloadCyclesFX : single;
    function    GetInitializing : boolean;
    function    GetOperationMode: TOperationMode;

    procedure   SetBankIndex(const aValue : byte);
    procedure   SetPatchIndex(const aValue : byte);

    function    CalcPatchLoadMem(aPatchLoadData: TPatchLoadData): single;
    function    CalcPatchLoadCycles(aPatchLoadData: TPatchLoadData): single;

  protected
    procedure   SetOperationMode(const Value: TOperationMode);
    function    GetSlotIndex: byte;
    function    GetPatchName : string;
    procedure   SetPatchName(const aValue : string);
  public
    constructor Create(aConnection: IG2Connection; const aSlotIndex: integer);
    destructor Destroy; override;
    function CreatePatch : IG2Patch; virtual;

    procedure add_log_line(tekst: string; log_cmd: integer);

    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    procedure InitMsg( aMsg : TG2SendMessage);
    procedure InitUndoMsg( aUndoMsg : TG2SendMessage);

    procedure StartInit(aConnection: IG2Connection; const aStartCommAfterInit : boolean);
    procedure InitSeq(Sender: TObject; aConnection: IG2Connection);
    function StartInitMidi(aConnection: IG2Connection): boolean;

    procedure AddMsgGetPatchVersion( aMsg : TG2SendMessage);
    procedure AddMsgGetPatch( aMsg : TG2SendMessage);
    procedure AddMsgGetPatchName( aMsg : TG2SendMessage);
    procedure AddMsgGetCurrentNote( aMsg : TG2Message);
    procedure AddMsgGetPatchNotes( aMsg : TG2Message);
    procedure AddMsgGetResourceTable( aMsg : TG2Message; const aLocationIndex: byte);
    procedure AddMsgUnknown6( aMsg: TG2Message);
    procedure AddMsgGetSelectedParam( aMsg: TG2Message);
    procedure AddMsgControllerSnapshot( aMsg : TG2SendMessage);
    procedure AddMsgCopyVariation( const aFromVariation, aToVariation : byte; aMsg: TG2SendMessage);
    procedure AddMsgSelectVariation( const aVariation : byte; aMsg: TG2SendMessage);
    procedure AddMsgPatchLoad(aMsg: TG2Message; const aPatchName: string; aPatch: IG2Patch);

    function ProcessResponseMsg(aSender : TObject; aMsg: TG2Message): boolean;
    function ProcessSendMsg(aSender : TObject; aMsg : TG2Message): boolean;

    procedure ProcessMsgPatchLoad( aMsg: TG2Message);
    procedure ProcessMsgPatchRequest( aMsg: TG2Message);
    procedure ProcessMsgVariationSel( aMsg : TG2Message);
    procedure ProcessMsgVariationCopy( aMsg: TG2Message);
    procedure ProcessMsgVersionChange( aMsg: TG2Message);
    procedure ProcessMsgPatchName( aMsg : TG2Message);
    procedure ProcessMsgVolumeData( aMsg : TG2Message);
    procedure ProcessMsgLedData( aMsg : TG2Message);
    procedure ProcessMsgParamSel( aMsg : TG2Message);
    procedure ProcessMsgParamChange( aMsg : TG2Message);
    procedure ProcessMsgModeChange( aMsg: TG2Message);
    procedure ProcessMsgResourcesUsed( aMsg : TG2Message);

    procedure DoAfterSlotInit;
    procedure DoPatchUpdate;
    procedure DoPatchLoadChange;
    procedure DoVariationChange;
    procedure DoCopyVariation;
    procedure DoAfterRetreivePatch( Slot, Bank, Patch : byte); virtual;
    procedure DoOperationModeChange;

    property PatchVersion : byte read GetPatchVersion write SetPatchVersion;
    property Synth: IG2Synth read GetSynth;
    property Patch : IG2Patch read GetPatch;
    property SlotIndex : byte read GetSlotIndex write SetSlotIndex;
    property AssignedVoices : byte read GetAssignedVoices;
    property OperationMode: TOperationMode read GetOperationMode write SetOperationMode;
  end;

implementation
uses
  System.Math,
  BVE.NMG2Patch;

//------------------------------------------------------------------------------
//
//                               TG2FileSlotSettings
//
//------------------------------------------------------------------------------

procedure TG2Slot.AddMsgControllerSnapshot(aMsg: TG2SendMessage);
begin
  add_log_line('Controller snapshot', LOGCMD_HDR);

  aMsg.WriteMessage( S_CTRL_SNAPSHOT);
end;

procedure TG2Slot.AddMsgCopyVariation(const aFromVariation,
  aToVariation: byte; aMsg: TG2SendMessage);
begin
  add_log_line('Copy variation, from variation ' + IntToStr(aFromVariation) + ', to variation ' + IntToStr(aToVariation), LOGCMD_HDR);

  aMsg.WriteMessage( S_COPY_VARIATION);
  aMsg.WriteMessage( aFromVariation);
  aMsg.WriteMessage( aToVariation);
end;

procedure TG2Slot.AddMsgGetCurrentNote(aMsg: TG2Message);
begin
  add_log_line('Current note, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_CURRENT_NOTE);
end;

procedure TG2Slot.AddMsgGetPatch(aMsg: TG2SendMessage);
begin
  add_log_line('Download, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_PATCH);
end;

procedure TG2Slot.AddMsgGetPatchName(aMsg: TG2SendMessage);
begin
  add_log_line('Patch name, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_PATCH_NAME);
end;

procedure TG2Slot.AddMsgGetPatchNotes(aMsg: TG2Message);
begin
  add_log_line('Patch notes, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_PATCH_TEXT);
end;

procedure TG2Slot.AddMsgGetPatchVersion(aMsg: TG2SendMessage);
begin
  add_log_line('Get patch version, slot ' + IntToStr(SlotIndex), LOGCMD_HDR);

  aMsg.Init;
  aMsg.WriteMessage( $01);
  aMsg.WriteMessage( CMD_REQ + CMD_SYS );
  aMsg.WriteMessage( $41);
  aMsg.WriteMessage( Q_VERSION_CNT);
  aMsg.WriteMessage( SlotIndex);
end;

procedure TG2Slot.AddMsgGetResourceTable(aMsg: TG2Message;
  const aLocationIndex: byte);
begin
  add_log_line('Resources used ' + IntToStr(aLocationIndex) + ', slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_RESOURCES_USED);
  aMsg.WriteMessage( aLocationIndex);
end;

procedure TG2Slot.AddMsgGetSelectedParam(aMsg: TG2Message);
begin
  add_log_line('Get selected parameter, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( Q_SELECTED_PARAM);
end;

procedure TG2Slot.AddMsgPatchLoad(aMsg: TG2Message;
  const aPatchName: string; aPatch: IG2Patch);
var Chunk : TPatchChunk;
begin
  add_log_line('Upload, slot ' + IntToStr( SlotIndex), LOGCMD_HDR);

  aMsg.WriteMessage( $01);
  aMsg.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex );
  aMsg.WriteMessage( $53); // ?
  aMsg.WriteMessage( S_SET_PATCH);
  aMsg.WriteMessage( $00);
  aMsg.WriteMessage( $00);
  aMsg.WriteMessage( $00);

  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := FWConnection.Log;

    Chunk.WriteName(aPatchName);
    Chunk.Flush;

    // 10 Variations must be in a patch for USB, unclear what #10 holds
    aPatch.Write(Chunk, N_USB_VARIATIONS);
  finally
    Chunk.Free;
  end;
end;

procedure TG2Slot.AddMsgSelectVariation(const aVariation: byte;
  aMsg: TG2SendMessage);
begin
  add_log_line('Select variation ' + IntToStr(aVariation) + ', slot ' + IntToStr(GetSlotIndex) + ', patch_version ' + IntToStr(PatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( S_SEL_VARIATION);
  aMsg.WriteMessage( aVariation);
end;

procedure TG2Slot.AddMsgUnknown6(aMsg: TG2Message);
begin
  add_log_line('Unknown 6, slot ' + IntToStr(SlotIndex) + ', patch_version ' + IntToStr(FPatchVersion), LOGCMD_HDR);

  aMsg.WriteMessage( M_UNKNOWN_6);
end;

procedure TG2Slot.add_log_line(tekst: string; log_cmd: integer);
begin
  FWConnection.AddLog( tekst, log_cmd);
end;

constructor TG2Slot.Create(aConnection: IG2Connection;
  const aSlotIndex: integer);
begin
  inherited Create;

  SetWeak(@FWConnection, aConnection);
  SlotIndex := aSlotIndex;

  FPatch := CreatePatch;
end;

destructor TG2Slot.Destroy;
begin
  SetWeak(@FWConnection, nil);

  inherited;
end;

procedure TG2Slot.Init;
begin
  //for i := 0 to 3 do
  //  FUnknown[i] := 0;
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;

  FPatchName         := 'No name';
  FEnabled           := $01;
  if FSLotIndex = 0 then
    FKeyboard          := $01
  else
    FKeyboard          := $00;
  FHold              := $00;
  FKeyboardRangeFrom := $00;
  FKeyboardRangeTo   := $7f;
  //FSlot              := $00;
  FBankIndex         := $00;
  FPatchIndex        := $00;

  FInitializing := False;

  FPatch.Init;
end;


procedure TG2Slot.DoPatchUpdate;
begin
  NotifyObservers(EvtPatchUpdate, self as IG2Slot);
end;

procedure TG2Slot.DoCopyVariation;
begin
  NotifyObservers(EvtCopyVariation, self as IG2Slot);
end;

procedure TG2Slot.DoOperationModeChange;
begin
  NotifyObservers(EvtOperationModeChange, self as IG2Slot);
end;

procedure TG2Slot.DoPatchLoadChange;
begin
  NotifyObservers(EvtPatchLoadChange, self as IG2Slot);
end;

procedure TG2Slot.DoVariationChange;
begin
  NotifyObservers( EvtVariationChange, self as IG2Slot);
end;

procedure TG2Slot.DoAfterRetreivePatch(Slot, Bank, Patch: byte);
begin
  NotifyObservers(EvtAfterRetreivePatch, self as IG2Slot);
end;

procedure TG2Slot.DoAfterSlotInit;
begin
  NotifyObservers(EvtAfterSlotInit, self as IG2Slot);
end;

function TG2Slot.CreatePatch : IG2Patch;
begin
  Result := TG2Patch.Create(self as IG2Slot);
end;

procedure TG2Slot.InitMsg( aMsg : TG2SendMessage);
begin
  aMsg.Init;

  aMsg.WriteMessage( $01);
  aMsg.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex);
  aMsg.WriteMessage( PatchVersion);
end;

procedure TG2Slot.InitUndoMsg( aUndoMsg : TG2SendMessage);
begin
  aUndoMsg.Init;

  aUndoMsg.AddReversed := True;
  aUndoMsg.Offset := 5;

  aUndoMsg.WriteMessage( $01);
  aUndoMsg.WriteMessage( CMD_REQ + CMD_SLOT + SlotIndex);
  aUndoMsg.WriteMessage( PatchVersion);
end;

function TG2Slot.ProcessResponseMsg( aSender : TObject; aMsg: TG2Message): boolean;
var Version, SubCmd, b : byte;
begin
  // Return true if message was processed

  //Patch := GetPatch;

  aMsg.Read(Version, 1);

  if Version = $40 then begin

    aMsg.Read( SubCmd, 1);
    case SubCmd of
    $36 : begin // patch version
            ProcessMsgVersionChange( aMsg);
            Result := True;
          end;
      else begin
         add_log_line('Unknown subcommand ' + IntToHex(Version, 2) + ' ' + IntToHex(SubCmd, 2), LOGCMD_ERR);
         Result := False;
      end;
    end;

  end else
    if Version = PatchVersion then begin

      aMsg.Read( SubCmd, 1);
      case SubCmd of
      C_PATCH_DESCR,
      C_CURRENT_NOTE_2,
      C_PATCH_NOTES,
      C_PARAM_LIST,
      C_PARAM_NAMES :
            begin
              aMsg.Position := aMsg.Position - 1;
              Result := Patch.ProcessMsg( aSender, aMsg);
            end;
      S_PATCH_NAME :
            begin // patch name
              ProcessMsgPatchName( aMsg);
              Result := True;
            end;
      S_SEL_PARAM :
            begin // selected parameter
              ProcessMsgParamSel( aMsg);
              Result := True;
            end;
      S_SEL_VARIATION :
            begin
              ProcessMsgVariationSel( aMsg);
              result := True;
            end;
      R_VOLUME_DATA :
            begin // Volume indicator data
              Result := Patch.ReadVolumeData( aMsg);
            end;
      R_LED_DATA :
            begin // Led data
              Result := Patch.ReadLedData( aMsg)
            end;
      S_SET_PARAM :
            begin // ParamChange
              ProcessMsgParamChange( aMsg);
              Result := True;
            end;
      R_RESOURCES_USED :
            begin // Resources in use
              ProcessMsgResourcesUsed( aMsg);
              Result := True;
            end;
      R_OK :
            begin // ok
              Result := True;
            end;
      R_ERROR :
            begin // NOT ok
              aMsg.Read(b, 1); // error no?
              FWConnection.LastG2Error := IntToHex(b, 2);
              add_log_line('G2 returns error ' + IntToHex(b, 2) + '!', LOGCMD_ERR);
              Result := True;
            end;
        else begin
          add_log_line('Unknown subcommand ' + IntToHex(Version, 2) + ' ' + IntToHex(SubCmd, 2), LOGCMD_ERR);
          Result := False;
        end;
      end;

    end else begin
      add_log_line('Patch version differs, received version ' + IntToHex(Version, 2) + ', current patch version is ' +  IntToHex(FPatchVersion, 2), LOGCMD_ERR);
      Result := False;
    end;
end;

function TG2Slot.ProcessSendMsg(aSender: TObject;
  aMsg: TG2Message): boolean;
var Cmd, SubCmd, b, bh, bl : byte;
    Size : integer;
begin
  // Return True if proceed
  Result := False;

  if (aMsg.Size - aMsg.Position) < 6 then begin
    aMsg.Position := aMsg.Size;
    exit;
  end;

  // Read size
  aMsg.Read( bh, 1);
  aMsg.Read( bl, 1);
  Size := bh * 256 + bl;

  aMsg.Read( b, 1); // $01
  aMsg.Read( Cmd, 1);

  case (Cmd and $0f)  of
  CMD_SYS : begin
              aMsg.Position := aMsg.Size; // Todo
              Result := True;
            end;
         else
            begin
              aMsg.Read( b, 1); // Version

              Result := Patch.ProcessMsg( aSender, aMsg);

              if not Result then begin
                aMsg.Position := aMsg.Position - 1;

                aMsg.Read( SubCmd, 1);

                case SubCmd of
                S_SET_PATCH :
                  begin
                    ProcessMsgPatchLoad( aMsg);
                    Result := True;
                  end;
                Q_PATCH :
                  begin
                    ProcessMsgPatchRequest( aMsg);
                    Result := True;
                  end;
                S_SEL_VARIATION :
                  begin
                    ProcessMsgVariationSel( aMsg);
                    aMsg.Position := aMsg.Size;
                    Result := True;
                  end;
                Q_SELECTED_PARAM :
                  begin
                    aMsg.Position := aMsg.Size; // Todo
                    Result := True;
                  end;
                M_UNKNOWN_6 :
                  begin
                    aMsg.Position := aMsg.Size; // Todo
                    Result := True;
                  end;
                Q_CURRENT_NOTE :
                  begin
                    aMsg.Position := aMsg.Size; // Todo
                    Result := True;
                  end;
                S_PATCH_NAME :
                  begin
                    ProcessMsgPatchName( aMsg);
                    aMsg.Position := aMsg.Size; // Todo
                    Result := True;
                  end;
                Q_PATCH_NAME :
                   begin
                     aMsg.Position := aMsg.Size; // Todo
                     Result := True;
                    end;
                Q_RESOURCES_USED :
                    begin
                      aMsg.Position := aMsg.Size; // Todo
                      Result := True;
                    end;
                Q_PATCH_TEXT :
                    begin
                      aMsg.Position := aMsg.Size; // Todo
                      Result := True;
                    end;
                S_SET_PARAM :
                    begin
                      ProcessMsgParamChange( aMsg);
                      aMsg.Position := aMsg.Size;
                      Result := True;
                    end;
                S_SET_MODE :
                    begin
                      ProcessMsgModeChange( aMsg);
                      aMsg.Position := aMsg.Size;
                      Result := True;
                    end;
                S_COPY_VARIATION :
                    begin
                      ProcessMsgVariationCopy( aMsg);
                      Result := True;
                    end;
                end;
              end;
            end;
  end;
end;

procedure TG2Slot.ProcessMsgPatchLoad( aMsg: TG2Message);
var b : byte;
    PatchName : string;
    Chunk : TPatchChunk;
begin
  aMsg.Read(b, 1);
  aMsg.Read(b, 1);
  aMsg.Read(b, 1);
  PatchName := '';
  while (length(PatchName) < 16) and (aMsg.Read(b, 1) = 1) and (b <> 0) do begin
    PatchName := PatchName + Char(b);
  end;
  Patch.PatchName := PatchName;
  SetPatchName( PatchName);

  Chunk := TPatchChunk.Create(aMsg);
  try
    Chunk.Log := FWConnection.Log;

    Chunk.ReadChunk;
    if Chunk.FId = $21 then begin
      Patch.Init;
      Patch.Read( Chunk); // Parse patch
    end;

  finally
    Chunk.Free;
  end;

  DoPatchUpdate;
end;

procedure TG2Slot.ProcessMsgPatchRequest( aMsg: TG2Message);
begin
  aMsg.Position := aMsg.Size;

  // Must be called AFTER moduleindex is known
  //Patch.PatchPart[LOCATION_PATCH].InitParameters;
  Patch.InitNames;

  //DoPatchUpdate;
end;

procedure TG2Slot.ProcessMsgPatchName( aMsg: TG2Message);
var PatchName : string;
    b : byte;
begin
  PatchName := '';
  while (aMsg.Read(b, 1) = 1) and (length(PatchName) < 16) and (b <> 0) do begin
    PatchName := PatchName + Char(b);
  end;
  SetPatchName( PatchName);
end;

procedure TG2Slot.ProcessMsgVariationSel(aMsg: TG2Message);
var Variation : byte;
begin
  aMsg.Read(Variation, 1);
  Patch.Settings.ActiveVariation := Variation;

  DoVariationChange;
end;

procedure TG2Slot.ProcessMsgVariationCopy( aMsg: TG2Message);
var FromVariation, ToVariation : byte;
begin
  aMsg.Read( FromVariation, 1);
  aMsg.Read( ToVariation, 1);
  Patch.CopyVariation( FromVariation, ToVariation);

  DoCopyVariation;
  if ToVariation = Patch.Settings.ActiveVariation then begin
    Patch.InvalidateParameters;
  end;
end;

procedure TG2Slot.ProcessMsgLedData( aMsg: TG2Message);
begin

end;

procedure TG2Slot.ProcessMsgParamChange( aMsg: TG2Message);
var LocationIndex, ModuleIndex, ParamIndex, Value, Variation : byte;
begin
  aMsg.Read( LocationIndex, 1);
  aMsg.Read( ModuleIndex, 1);
  aMsg.Read( ParamIndex, 1);
  aMsg.Read( Value, 1);
  aMsg.Read( Variation, 1);

  if ModuleIndex <> 0 then
    Patch.SetParamInPatch(LocationIndex, ModuleIndex, ParamIndex, Variation, Value);
end;

procedure TG2Slot.ProcessMsgModeChange( aMsg: TG2Message);
var LocationIndex, ModuleIndex, ParamIndex, Value : byte;
begin
  aMsg.Read( LocationIndex, 1);
  aMsg.Read( ModuleIndex, 1);
  aMsg.Read( ParamIndex, 1);
  aMsg.Read( Value, 1);

  Patch.SetModeInPatch(LocationIndex, ModuleIndex, ParamIndex, Value);
end;

procedure TG2Slot.ProcessMsgParamSel(aMsg: TG2Message);
var b, LocationIndex, ModuleIndex, ParamIndex : byte;
begin
  aMsg.Read(b, 1); // Unknown
  aMsg.Read(LocationIndex, 1);
  aMsg.Read(ModuleIndex, 1);
  aMsg.Read(ParamIndex, 1);

  Patch.SelectParamInPatch(LocationIndex, ModuleIndex, ParamIndex);
end;

procedure TG2Slot.ProcessMsgResourcesUsed(aMsg: TG2Message);
var Location, b : byte;
begin
  aMsg.Read(Location, 1);
  case Location of
  $00 : aMsg.Read(FPatchLoadFXData, 27);
  $01 : aMsg.Read(FPatchLoadVAData, 27);
  end;

  if aMsg.Position < aMsg.Size - 2 then begin
    aMsg.Read(b, 1);
    if b = R_RESOURCES_USED then begin
      aMsg.Read(Location, 1);
      case Location of
      $00 : aMsg.Read(FPatchLoadFXData, 27);
      $01 : aMsg.Read(FPatchLoadVAData, 27);
      end;
    end;
  end;

  DoPatchLoadChange;
end;

procedure TG2Slot.ProcessMsgVersionChange(aMsg: TG2Message);
var b : byte;
begin
  //aMsg.Read(b, 1); // Slot?
  aMsg.Read(b, 1); // Patch Version

  PatchVersion := b;

  DoAfterRetreivePatch( SlotIndex, 0, 0);
end;

procedure TG2Slot.ProcessMsgVolumeData(aMsg: TG2Message);
begin

end;

procedure TG2Slot.SetPatchIndex(const aValue: byte);
begin
  FPatchIndex := aValue;
end;

procedure TG2Slot.SetPatchName(const aValue: string);
begin
  FPatchName := aValue;
end;

procedure TG2Slot.SetPatchVersion(const aValue: byte);
begin
  FPatchVersion := aValue;
end;

procedure TG2Slot.Read( aChunk : TPatchChunk);
begin
  FPatchName         := aChunk.ReadName;
  FEnabled           := aChunk.ReadBits( 8);
  FKeyboard          := aChunk.ReadBits( 8);
  FHold              := aChunk.ReadBits( 8);
  FBankIndex         := aChunk.ReadBits( 8);
  FPatchIndex        := aChunk.ReadBits( 8);
  FKeyboardRangeFrom := aChunk.ReadBits( 8);
  FKeyboardRangeTo   := aChunk.ReadBits( 8);
  //FSlot              := aChunk.ReadBits( 8); // 2011-11-14 bve : First G2 gives 1..4, second G2 gives 5..8..?
  FUnknown2          := aChunk.ReadBits( 8);
  FUnknown3          := aChunk.ReadBits( 8);
  FUnknown4          := aChunk.ReadBits( 8);
end;

procedure TG2Slot.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteName( FPatchName);
  aChunk.WriteBits( FEnabled,           8);
  aChunk.WriteBits( FKeyboard,          8);
  aChunk.WriteBits( FHold,              8);
  aChunk.WriteBits( FBankIndex,         8);
  aChunk.WriteBits( FPatchIndex,        8);
  aChunk.WriteBits( FKeyboardRangeFrom, 8);
  aChunk.WriteBits( FKeyboardRangeTo,   8);
  //aChunk.WriteBits( FSlot,              8);
  aChunk.WriteBits( FUnknown2,          8);
  aChunk.WriteBits( FUnknown3,          8);
  aChunk.WriteBits( FUnknown4,          8);
end;

function TG2Slot.GetSlotIndex: byte;
begin
  Result := FSlotIndex;
end;

procedure TG2Slot.SetSlotIndex(const aValue : byte);
begin
  FSlotIndex := aValue;
end;

procedure TG2Slot.StartInit(aConnection: IG2Connection;
  const aStartCommAfterInit : boolean);
begin
  if not aConnection.Connected then
    exit;

  FInitStep := 1;
  FStartCommAfterInit := aStartCommAfterInit;
  FFinishMsg := aConnection.OnNextMsg;
  aConnection.OnNextMsg := InitSeq;

  Patch.Init;

  aConnection.SlotGetPatchVersion(SlotIndex);
end;

procedure TG2Slot.InitSeq(Sender: TObject; aConnection: IG2Connection);
begin
  FInitMidiStep := 0;
  while FInitStep <= 10 do begin
    case FInitStep of
     0 : begin
           aConnection.SlotGetPatchVersion(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     1 : begin
           aConnection.SlotGetPatch(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     2 : begin
           aConnection.SlotGetPatchName(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     3 : begin
           aConnection.SlotGetCurrentNote(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     4 : begin
           aConnection.SlotGetPatchNotes(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     5 : begin
           aConnection.SlotGetResourceTable(SlotIndex, LOCATION_VA);
           inc(FInitStep);
           exit;
         end;
     6 : begin
           aConnection.SlotGetResourceTable(SlotIndex, LOCATION_FX);
           inc(FInitStep);
           exit;
         end;
     7 : begin
           if FWConnection.Synth.AutoAssignMidi then begin
             if StartInitMidi(aConnection) then
               exit;
           end else
             inc(FInitStep);
         end;
     8 : begin
           aConnection.SlotUnknown6(SlotIndex);
           inc(FInitStep);
           exit;
         end;
     9 : begin
           aConnection.SlotGetSelectedParam(SlotIndex);
           inc(FInitStep);
           if not FStartCommAfterInit then begin
             aConnection.OnNextMsg := FFinishMsg;
             DoAfterSLotInit;
           end;
           exit;
         end;
     10 : begin
           aConnection.SynthStartStopCommunication(START_COMM);
           inc(FInitStep);
           aConnection.OnNextMsg := FFinishMsg;
           DoAfterSlotInit;
           exit;
         end;
    end;
    //inc(FInitStep);
  end;
end;

function TG2Slot.StartInitMidi(aConnection: IG2Connection): boolean;
var Knob : IG2Knob;
    MidiCC : byte;
    KnobIndex : byte;
    Param : IG2Param;

  function AssignMidiCC(aParam : IG2Param; const aMidiCC : byte): boolean;
  begin
    // Returns True if a messages is send
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

  if FInitMidiStep < FWConnection.Synth.MidiToKnobList.Count then begin

    MidiCC := Synth.MidiToKnobList[FInitMidiStep].Controller;
    KnobIndex := Synth.MidiToKnobList[FInitMidiStep].Knob;

    case Synth.MidiToKnobList[FInitMidiStep].Source of
      g2psParam :
        begin
          Knob := Patch.KnobList[ KnobIndex];
          case Synth.MidiToKnobList[FInitMidiStep].ControlType of
            g2ctKnob :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Param) then begin
                  if AssignMidiCC(Knob.Param, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Param) and assigned(Knob.Param.ButtonParam) then begin
                  if AssignMidiCC(Knob.Param.ButtonParam, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
          end;
        end;
      g2psMorph :
        begin
          case Synth.MidiToKnobList[FInitMidiStep].ControlType of
            g2ctKnob :
              begin
                Param := Patch.GetMorphKnobParameter(KnobIndex);
                if assigned(Param) then begin
                  if AssignMidiCC(Param, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
            g2ctButton :
              begin
                Param := Patch.GetMorphKnobParameter(KnobIndex+8);
                if assigned(Param) then begin
                  if AssignMidiCC(Param, MidiCC) then begin
                    Result := True;
                    exit;
                  end;
                end else
                  inc(FInitMidiStep);
              end;
          end;
        end;
      g2psPatch :
        begin
          case FWConnection.Synth.MidiToKnobList[FInitMidiStep].ControlType of
            g2ctKnob :
              begin
                Param := Patch.GetPatchKnobParameter(KnobIndex);
                if AssignMidiCC(Param, MidiCC) then begin
                  Result := True;
                  exit;
                end;
              end;
            g2ctButton :
              begin
                Param := Patch.GetPatchKnobParameter(KnobIndex+8);
                if AssignMidiCC(Param, MidiCC) then begin
                  Result := True;
                  exit;
                end;
              end;
            else
              inc(FInitMidiStep);
          end;
        end;
    end;
  end else
    inc(FInitStep);
end;

function TG2Slot.GetAssignedVoices: byte;
begin
  Result := FAssignedVoices;
end;

function TG2Slot.GetBankIndex: byte;
begin
  Result := FBankIndex;
end;

function TG2Slot.GetConnection: IG2Connection;
begin
  Result := FWConnection;
end;

function TG2Slot.GetEnabled: TBits8;
begin
  Result := FEnabled;
end;

function TG2Slot.GetSynth: IG2Synth;
begin
  Result := FWConnection.Synth;
end;

function TG2Slot.GetHold: TBits8;
begin
  Result := FHold;
end;

function TG2Slot.GetInitializing: boolean;
begin
  Result := FInitializing;
end;

function TG2Slot.GetKeyboard: TBits8;
begin
  Result := FKeyboard;
end;

function TG2Slot.GetKeyboardRangeFrom: TBits8;
begin
  Result := FKeyboardRangeFrom;
end;

function TG2Slot.GetKeyboardRangeTo: TBits8;
begin
  Result := FKeyboardRangeTo;
end;

function TG2Slot.GetLog: IG2Log;
begin
  Result := FWConnection.Log;
end;

function TG2Slot.GetOperationMode: TOperationMode;
begin
  Result := FOperationMode;
end;

function TG2Slot.GetPatch: IG2Patch;
begin
  Result := FPatch;
end;

function TG2Slot.GetPatchIndex: byte;
begin
  Result := FPatchIndex;
end;

function TG2Slot.GetPatchName: string;
begin
  Result := FPatchName;
end;

function TG2Slot.GetPatchVersion: byte;
begin
  Result := FPatchVersion;
end;

function TG2Slot.GetPerf: IG2Perf;
begin
  Result := FWConnection.Perf;
end;

function TG2Slot.CalcPatchLoadMem(aPatchLoadData: TPatchLoadData): single;
var Resource4 : word;
    InternalMem : byte;
    RAM : smallint;
begin
  //CyclesRed1  := aPatchLoadData[1] + aPatchLoadData[0] * 256;
  //CyclesBlue1 := aPatchLoadData[3] + aPatchLoadData[2] * 256;

  InternalMem := aPatchLoadData[4];

  //Unknown1 := aPatchLoadData[6] + aPatchLoadData[5] * 256;
  Resource4 := aPatchLoadData[8] + aPatchLoadData[7] * 128;

  //Resource5 := aPatchLoadData[10] + aPatchLoadData[9] * 256;

  //CyclesRed2  := aPatchLoadData[12] + aPatchLoadData[11] * 256;

  //Unknown3 := aPatchLoadData[14] + aPatchLoadData[13] * 256;
  //Resource8 := aPatchLoadData[16] + aPatchLoadData[15] * 256;

  //CyclesBlue2 := aPatchLoadData[18] + aPatchLoadData[17] * 256;

  //Unknown4 := aPatchLoadData[20] + aPatchLoadData[19] * 256;

  RAM := aPatchLoadData[24] + aPatchLoadData[23] * 256 + aPatchLoadData[22] * 256*256 + aPatchLoadData[21] * 256*256*256;

  //Unknown5 := aPatchLoadData[26] + aPatchLoadData[25] * 256;

  Result := Max(Max( 100 * InternalMem / 128, 100 * RAM / 260000), 100*Resource4 / 4315);
end;

function TG2Slot.CalcPatchLoadCycles(aPatchLoadData: TPatchLoadData): single;
var CyclesRed1, CyclesBlue1 : word;
begin
  CyclesRed1  := aPatchLoadData[1] + aPatchLoadData[0] * 128;
  CyclesBlue1 := aPatchLoadData[3] + aPatchLoadData[2] * 128;

  //InternalMem := aPatchLoadData[4];

  //Unknown1 := aPatchLoadData[6] + aPatchLoadData[5] * 256;
  //Unknown2 := aPatchLoadData[8] + aPatchLoadData[7] * 256;

  //Resource5 := aPatchLoadData[10] + aPatchLoadData[9] * 256;

  //CyclesRed2  := aPatchLoadData[12] + aPatchLoadData[11] * 128;

  //Unknown3 := aPatchLoadData[14] + aPatchLoadData[13] * 256;
  //Resource8 := aPatchLoadData[16] + aPatchLoadData[15] * 256;

  //CyclesBlue2 := aPatchLoadData[18] + aPatchLoadData[17] * 128;

  //Unknown4 := aPatchLoadData[20] + aPatchLoadData[19] * 256;

  //RAM := aPatchLoadData[24] + aPatchLoadData[23] * 256 + aPatchLoadData[22] * 256*256 + aPatchLoadData[21] * 256*256*256;

  //Unknown5 := aPatchLoadData[26] + aPatchLoadData[25] * 256;

  //Result := fmax( 100 * CyclesRed1 / 1352 + 100 * CyclesBlue1 / 5000, 100 * Resource5 / 258);
  Result := Max( 100 * CyclesRed1 / 1372 + 100 * CyclesBlue1 / 5000, 0);
end;

function TG2Slot.GetPatchLoadCyclesFX: single;
begin
  Result := CalcPatchLoadCycles( FPatchLoadFXData);
end;

function TG2Slot.GetPatchLoadCyclesVA: single;
begin
  Result := CalcPatchLoadCycles( FPatchLoadVAData);
end;

function TG2Slot.GetPatchLoadMemFX: single;
begin
  Result := CalcPatchLoadMem( FPatchLoadFXData);
end;

function TG2Slot.GetPatchLoadMemVA: single;
begin
  Result := CalcPatchLoadMem( FPatchLoadVAData);
end;

procedure TG2Slot.SetAssignedVoices(const aValue: byte);
begin
  FAssignedVoices := aValue;
end;

procedure TG2Slot.SetBankIndex(const aValue: byte);
begin
  FBankIndex := aValue;
end;

procedure TG2Slot.SetEnabled(const aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot enabled must be 0 or 1.');
  FEnabled := aValue;
end;

procedure TG2Slot.SetHold(const aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot hold must be 0 or 1.');
  FHold := aValue;
end;

procedure TG2Slot.SetInitializing(const aValue: boolean);
begin
  FInitializing := aValue;
end;

procedure TG2Slot.SetKeyboard(const aValue : TBits8);
begin
  if aValue > 1 then
    raise Exception.Create('Slot keyboard must be 0 or 1.');
  FKeyboard := aValue;
end;

procedure TG2Slot.SetKeyboardRangeTo(const aValue : TBits8);
begin
  FKeyboardRangeTo := aValue; // can be from 0 to 255
end;

procedure TG2Slot.SetOperationMode(const Value: TOperationMode);
begin
  if Value <> FOperationMode then
  begin
    FOperationMode := Value;
    DoOperationModeChange;
  end;
end;

procedure TG2Slot.SetKeyboardRangeFrom(const aValue : TBits8);
begin
  FKeyboardRangeFrom := aValue;
end;

end.
