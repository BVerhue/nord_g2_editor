unit g2_classes;

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
//  This unit contains all the objects on the highest level. These should be
//  used to build an application for the G2
//
//  ////////////////////////////////////////////////////////////////////////////

interface

uses
{$IFDEF FPC}
{$ELSE}
  Windows, Messages, Forms,
{$ENDIF}
  Classes, SysUtils, g2_types, g2_file, g2_mess, g2_usb, g2_graph,
  g2_database;

type
  TG2 = class;
  TG2Patch = class;
  TG2Slot = class;
  TG2Performance = class;

  TG2Patch = class( TG2GraphPatch)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;

    function    GetG2: TG2;
    function    GetSlot : TG2Slot;
    function    GetPerformance : TG2Performance;

    //procedure   SetParameterValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aVariation : byte; aValue: byte); override;
    //procedure   SetParameterAutomated( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aVariation : byte; aValue: byte);
    procedure   SetModeValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aValue: byte); override;
    procedure   SetMorphValue( aLocation : TLocationType; aModuleIndex : byte; aParameterIndex : byte; aMorphIndex : byte; aValue: byte; aVariation : byte); override;

    //procedure   InitParameterValue( aLocation : TLocationType; aModuleIndex, aParameterIndex, aVariation, aValue : byte); override;
    procedure   InitModeValue( aLocation : TLocationType; aModuleIndex, aParameterIndex, aValue: byte); override;
  end;

  TG2Slot = class( TG2USBSlot)
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePatch : TG2FilePatch; override;
    function    GetPatch: TG2Patch;
    function    GetPerformance : TG2Performance;
    procedure   UploadPatch( aFileName : string);
  end;

  TG2Performance = class(TG2GraphPerformance)
  private
    function    GetSlot( aSlot : byte): TG2Slot;
    function    GetSlotA: TG2Slot;
    function    GetSlotB: TG2Slot;
    function    GetSlotC: TG2Slot;
    function    GetSlotD: TG2Slot;
    function    GetG2 : TG2;
  protected
    //procedure   SetSelectedSlotIndex( aValue : TBits2); override;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreateSlot : TG2FileSlot; override;
    procedure   UploadPerf( aFileName : string);

    property    Slot[ Index : byte]: TG2Slot read GetSlot;

    property    SlotA : TG2Slot read GetSlotA;
    property    SlotB : TG2Slot read GetSlotB;
    property    SlotC : TG2Slot read GetSlotC;
    property    SlotD : TG2Slot read GetSlotD;
  end;

  TG2 = class( TG2Graph)
  private
    function    GetSlot( aSlot : byte): TG2Slot;
    function    GetSlotA : TG2Slot;
    function    GetSlotB : TG2Slot;
    function    GetSlotC : TG2Slot;
    function    GetSlotD : TG2Slot;
    function    GetPatch( aSlot : byte): TG2Patch;
    function    GetPatchA : TG2Patch;
    function    GetPatchB : TG2Patch;
    function    GetPatchC : TG2Patch;
    function    GetPatchD : TG2Patch;
    function    GetPerformance : TG2Performance;
    function    GetSelectedSlotIndex: TBits2;
    procedure   SetSelectedSlotIndex( aValue : TBits2);
    function    GetSelectedSlot : TG2Slot;
    function    GetSelectedPatch : TG2Patch;
  public
    constructor Create( AOwner: TComponent); override;
    destructor  Destroy; override;
    function    CreatePerformance : TG2FilePerformance; override;
    procedure   LoadFileStream( aFilename : string);
    procedure   LoadMidiStream( aFilename : string);
    property    Slot[ Index : byte]: TG2Slot read GetSlot;
    property    Patch[ Index : byte]: TG2Patch read GetPatch;
    property    SlotA : TG2Slot read GetSlotA;
    property    SlotB : TG2Slot read GetSlotB;
    property    SlotC : TG2Slot read GetSlotC;
    property    SlotD : TG2Slot read GetSlotD;
    property    PatchA : TG2Patch read GetPatchA;
    property    PatchB : TG2Patch read GetPatchB;
    property    PatchC : TG2Patch read GetPatchC;
    property    PatchD : TG2Patch read GetPatchD;
    property    SelectedSlot : TG2Slot read GetSelectedSlot;
    property    SelectedPatch : TG2Patch read GetSelectedPatch;
    property    Performance : TG2Performance read GetPerformance;
  published
    property    SelectedSlotIndex : TBits2 read GetSelectedSlotIndex write SetSelectedSlotIndex;
  end;

procedure Register;

implementation

////////////////////////////////////////////////////////////////////////////////
//  TG2Patch
////////////////////////////////////////////////////////////////////////////////

constructor TG2Patch.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2Patch.Destroy;
begin
  inherited;
end;

function TG2Patch.GetG2: TG2;
begin
  Result := G2 as TG2;
end;

function TG2Patch.GetSlot: TG2Slot;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := Slot as TG2Slot;
end;

function TG2Patch.GetPerformance: TG2Performance;
begin
  if not assigned(Slot) then
    raise Exception.Create('Slot not assigned to patch.');

  Result := (Slot as TG2Slot).GetPerformance;
end;

procedure TG2Patch.SetModeValue( aLocation: TLocationType; aModuleIndex, aParameterIndex, aValue: byte);
var Slot : TG2Slot;
begin
  inherited;

  Slot := GetSlot;

  Slot.SendSetModeMessage( ord(aLocation), aModuleIndex, aParameterIndex, aValue);
end;

procedure TG2Patch.SetMorphValue(aLocation: TLocationType; aModuleIndex, aParameterIndex, aMorphIndex, aValue, aVariation: byte);
var Slot : TG2Slot;
begin
  inherited;

  Slot := GetSlot;

  if aValue >= 128 then
    Slot.SendSetMorphMessage( ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, abs(aValue - 256), 1, aVariation)
  else
    Slot.SendSetMorphMessage( ord(aLocation), aModuleIndex, aParameterIndex, aMorphIndex, aValue, 0, aVariation);
end;

{procedure TG2Patch.SetParameterAutomated(aLocation: TLocationType; aModuleIndex, aParameterIndex, aVariation, aValue: byte);
var Slot : TG2Slot;
begin
  // Parameter sent from VST host to G2 Vst client
  inherited SetParameterValue( aLocation, aModuleIndex, aParameterIndex, aVariation, aValue);

  Slot := GetSlot;
  try
    if assigned(Slot) then
      Slot.SendSetParamMessage( ord(aLocation), aModuleIndex, aParameterIndex, aValue, aVariation);
    except on E:Exception do begin
        G2.add_log_line( E.Message + ' SetParameterAutomated, Slot = '+ IntToStr(Int64(Slot))
                         + ', Location = ' + IntToStr(ord(aLocation))
                         + ', ModuleIdnex = ' + IntToStr(aModuleIndex)
                         + ', ParamIndex = ' + IntToStr(aParameterIndex)
                         + ', Value = ' + IntToStr(aValue)
                         + ', Variation = ' + IntToStr(aVariation), LOGCMD_NUL);
        G2.save_log;
      end;

  end;
  // No event calling to prevent feedback loops
  // Knob is repainted with timer in VST to reduce cpu load
end;}

{procedure TG2Patch.SetParameterValue( aLocation: TLocationType; aModuleIndex, aParameterIndex, aVariation : byte; aValue: byte);
var Slot : TG2Slot;
begin
  Slot := GetSlot;

  Slot.SendSetParamMessage( ord(aLocation), aModuleIndex, aParameterIndex, aValue, aVariation);
  InitParameterValue( aLocation, aModuleIndex, aParameterIndex, aVariation, aValue);
end;}

{procedure TG2Patch.InitParameterValue( aLocation : TLocationType; aModuleIndex,  aParameterIndex, aVariation, aValue: byte);
var Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  // Don't send over USB! That's already been done
  inherited SetParameterValue( aLocation, aModuleIndex, aParameterIndex, aVariation, aValue);

  if assigned(G2) then begin
    if (G2.CLientType <> ctVST) then begin

      Param := nil;
      Module := nil;
      if (aLocation = ltFX) or (aLocation = ltVA)  then
        Module := Modules[ord(aLocation), aModuleIndex];

      if aLocation = ltPatch then
        Param := Parameter[aModuleIndex, aParameterIndex]
      else
        if assigned(Module) then
          Param := Module.Parameter[aParameterIndex];

      if assigned(Param) then
        Param.InvalidateControl;
    end;

    if assigned(G2.OnParameterChange) then
      G2.OnParameterChange( self, G2.ID, Slot.SlotIndex, aVariation, aLocation, aModuleIndex, aParameterIndex, aValue);
  end;
end;}

procedure TG2Patch.InitModeValue( aLocation : TLocationType; aModuleIndex, aParameterIndex, aValue: byte);
var Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  // Don't send over USB! That's already been done
  inherited SetModeValue( aLocation, aModuleIndex, aParameterIndex, aValue);

  Param := nil;
  Module := nil;
  if (aLocation = ltFX) or (aLocation = ltVA)  then
    Module := Modules[ord(aLocation), aModuleIndex];

  if assigned(Module) then
    Param := Module.Mode[aParameterIndex];

  if assigned(Param) then
    Param.InvalidateControl;

  if assigned(G2.OnModuleModeChange) then
    G2.OnModuleModeChange( self, G2.ID, SLot.SlotIndex, aLocation, aModuleIndex, aParameterIndex, aValue);
end;


////////////////////////////////////////////////////////////////////////////////
//  TG2Slot
////////////////////////////////////////////////////////////////////////////////

constructor TG2Slot.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2Slot.Destroy;
begin
  // Patch is freed automatically
  inherited;
end;

function TG2Slot.CreatePatch : TG2FilePatch;
begin
  Result := TG2Patch.Create( self);
end;

function TG2Slot.GetPatch: TG2Patch;
begin
  if not assigned(Patch) then
    raise Exception.Create('Patch not assigned to slot.');
  Result := Patch as TG2Patch;
end;

function TG2Slot.GetPerformance: TG2Performance;
begin
  if not assigned(Performance) then
    raise Exception.Create('Performance not assigned to slot.');

  Result := Performance as TG2Performance;
end;

procedure TG2Slot.UploadPatch( aFileName: string);
var aPatch : TG2FilePatch;
    PatchName : AnsiString;
    i : integer;
    FileStream : TFileStream;
begin
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    // Take the patchname from the filename
    aFilename := ExtractFilename( aFileName);

    // Name patch max size = 16, if shorter end with 0
    PatchName := '';
    i := 1;
    while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
      PatchName := PatchName + AnsiChar(aFileName[i]);
      inc(i);
    end;

    aPatch := TG2FilePatch.Create(G2);
    try
      if aPatch.LoadFromFile(FileStream, (G2 as TG2USB).LogLines) then
        SendSetPatchMessage(PatchName, aPatch);
    finally
      aPatch.Free;
    end;

  finally
    FileStream.Free;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
//  TG2Performance
////////////////////////////////////////////////////////////////////////////////

constructor TG2Performance.Create( AOwner: TComponent);
begin
  inherited;
end;

destructor TG2Performance.Destroy;
begin
  // Slots are freed automatically
  inherited;
end;

function TG2Performance.CreateSlot : TG2FileSlot;
begin
  Result := TG2Slot.Create( self);
end;

function TG2Performance.GetG2: TG2;
begin
  Result := G2 as TG2;
end;

function TG2Performance.GetSlot( aSlot : byte): TG2Slot;
begin
  Result := inherited GetSlot(aSlot) as TG2Slot;
end;

function TG2Performance.GetSlotA: TG2Slot;
begin
  Result := Slot[0] as TG2Slot;
end;

function TG2Performance.GetSlotB: TG2Slot;
begin
  Result := Slot[1] as TG2Slot;
end;

function TG2Performance.GetSlotC: TG2Slot;
begin
  Result := Slot[2] as TG2Slot;
end;

function TG2Performance.GetSlotD: TG2Slot;
begin
  Result := Slot[3] as TG2Slot;
end;

procedure TG2Performance.UploadPerf( aFileName: string);
var aPerf : TG2FilePerformance;
    PerfName : AnsiString;
    i : integer;
    FileStream : TFileStream;
    LogLines : TStrings;
begin
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    aFilename := ExtractFilename( aFileName);

    // Name patch max size = 16, if shorter end with 0
    PerfName := '';
    i := 1;
    while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
      PerfName := PerfName + AnsiChar(aFileName[i]);
      inc(i);
    end;

    aPerf := TG2FilePerformance.Create(G2);
    try
      LogLines := nil;
      if assigned((G2 as TG2USB).LogLines) then
        LogLines := (G2 as TG2USB).LogLines;

      if aPerf.LoadFromFile(FileStream, LogLines) then
        SendSetPerformanceMessage( PerfName, aPerf);
    finally
      aPerf.Free;
    end;

  finally
    FileStream.Free;
  end;
end;

{procedure TG2Performance.SetSelectedSlotIndex(aValue: TBits2);
var i : integer;
begin
  inherited;

  // Disabled sending select slot, so that clients (vst) can have a different slot
  // selected than the editor.
  //USBSelectSlot( aValue);

  for i := 0 to 3 do
    (Slot[i].Patch as TG2Patch).Visible := (i = aValue);

  if assigned((G2 as TG2Graph).ScrollboxVA)  then
    (G2 as TG2Graph).ScrollboxVA.Invalidate;

  if assigned((G2 as TG2Graph).ScrollboxFX) then
    (G2 as TG2Graph).ScrollboxFX.Invalidate;

//  if assigned((G2 as TG2).OnSelectSlot) then
//    (G2 as TG2).OnSelectSlot( self, G2.ID, aValue);
end;}

////////////////////////////////////////////////////////////////////////////////
//  TG2
////////////////////////////////////////////////////////////////////////////////

constructor TG2.Create( AOwner: TComponent);
begin
  inherited;

end;

function TG2.CreatePerformance: TG2FilePerformance;
begin
  Result := TG2Performance.Create(self);
end;

destructor TG2.Destroy;
begin
  // Performance is freed automatically
  inherited;
end;

function TG2.GetPatch(aSlot: byte): TG2Patch;
var Perf : TG2Performance;
begin
  Result := nil;
  Perf := Performance;
  if assigned( Perf) and assigned( Perf.GetSlot(aSlot)) then
    Result := Perf.GetSlot(aSlot).Patch as TG2Patch;
end;

function TG2.GetPatchA: TG2Patch;
begin
  Result := GetPatch(0);
end;

function TG2.GetPatchB: TG2Patch;
begin
  Result := GetPatch(1);
end;

function TG2.GetPatchC: TG2Patch;
begin
  Result := GetPatch(2);
end;

function TG2.GetPatchD: TG2Patch;
begin
  Result := GetPatch(3);
end;

function TG2.GetPerformance: TG2Performance;
begin
  Result := inherited GetPerformance as TG2Performance;
end;

{function TG2.GetSelectedPatch: TG2GraphPatch;
begin
  Result := nil;
  if assigned( Performance) then
    if assigned( Performance.GetSlot( SelectedSlot)) then
      Result := Performance.GetSlot( SelectedSlot).GetPatch as TG2GraphPatch;
end;}

function TG2.GetSelectedPatch: TG2Patch;
begin
  Result := GetPatch( SelectedSlotIndex);
end;

function TG2.GetSelectedSlot: TG2Slot;
begin
  Result := GetSlot( SelectedSlotIndex);
end;

function TG2.GetSelectedSlotIndex: TBits2;
begin
  Result := 0;
  if assigned( Performance) then
    Result := Performance.SelectedSlot;
end;

function TG2.GetSlot( aSlot: byte): TG2Slot;
begin
  Result := nil;
  if assigned( Performance) then
    Result := Performance.GetSlot( aSlot) as TG2Slot;
end;

function TG2.GetSlotA: TG2Slot;
begin
  Result := GetSlot(0);
end;

function TG2.GetSlotB: TG2Slot;
begin
  Result := GetSlot(1);
end;

function TG2.GetSlotC: TG2Slot;
begin
  Result := GetSlot(2);
end;

function TG2.GetSlotD: TG2Slot;
begin
  Result := GetSlot(3);
end;

procedure TG2.LoadFileStream( aFilename : string);
var G2FileDataStream : TG2FileDataStream;
    DataName : AnsiString;
    i : integer;
    FileStream : TFileStream;
    Lines : TStrings;
    b : byte;
begin
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    aFilename := ExtractFilename( aFileName);

    // Name patch max size = 16, if shorter end with 0
    DataName := '';
    i := 1;
    while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
      DataName := DataName + AnsiChar(aFileName[i]);
      inc(i);
    end;

    Lines := nil;
    if assigned(LogLines) then
      Lines := LogLines;

    // Check first byte
    FileStream.Read( b, 1);
    FileStream.Position := 0;
    case b of
      $56 : G2FileDataStream := TG2FileDataStream.LoadFileData( self, FileStream, Lines);
      $F0 : G2FileDataStream := TG2FileDataStream.LoadMidiData( self, FileStream, Lines);
      else
        raise Exception.Create('Unknown file data.');
    end;

    if G2FileDataStream is TG2FilePerformance then
      Performance.SendSetPerformanceMessage( DataName, G2FileDataStream as TG2FilePerformance)
    else
      if G2FileDataStream is TG2FilePatch then
        SelectedSlot.SendSetPatchMessage( DataName, G2FileDataStream as TG2FilePatch)
      else
        raise Exception.Create('Unknown data type');

  finally
    FileStream.Free;
  end;
end;

procedure TG2.LoadMidiStream( aFilename: string);
var G2FileDataStream : TG2FileDataStream;
    DataName : AnsiString;
    i : integer;
    FileStream : TFileStream;
    Lines : TStrings;
begin
  FileStream := TFileStream.Create( aFileName, fmOpenRead);
  try
    aFilename := ExtractFilename( aFileName);

    // Name patch max size = 16, if shorter end with 0
    DataName := '';
    i := 1;
    while (i<=Length( aFileName)) and (i<=16) and ( aFileName[i] <> '.') do begin
      DataName := DataName + AnsiChar(aFileName[i]);
      inc(i);
    end;

    Lines := nil;
    if assigned(LogLines) then
      Lines := LogLines;

    G2FileDataStream := TG2FileDataStream.LoadMidiData( self, FileStream, Lines);
    if G2FileDataStream is TG2FilePerformance then
      Performance.SendSetPerformanceMessage( DataName, G2FileDataStream as TG2FilePerformance)
    else
      if G2FileDataStream is TG2FilePatch then
        SelectedSlot.SendSetPatchMessage( DataName, G2FileDataStream as TG2FilePatch)
      else
        raise Exception.Create('Unknown data type');

  finally
    FileStream.Free;
  end;
end;


procedure TG2.SetSelectedSlotIndex(aValue: TBits2);
begin
  if assigned(Performance) then
    Performance.SelectedSlot := aValue;
end;

procedure Register;
begin
  RegisterComponents('NM G2', [TG2]);
end;

end.
