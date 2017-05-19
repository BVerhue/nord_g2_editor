unit BVE.NMG2Param;

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
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  TG2ParamMorph = class( TInterfacedObject, IG2ParamMorph)
  private
    FSlotIndex     : byte;
    // Morph value of a parameter
    FLocationIndex : TBits2;
    FModuleIndex   : TBits8;
    FParamIndex    : TBits7;
    FMorphIndex    : TBits4;
    FRange         : TBits8;
  public
    function    GetRange: byte;
    function    GetAbsRange: byte;
    function    GetNegative: byte;
    function    GetSlotIndex: byte;
    function    GetLocationIndex : byte;
    function    GetModuleIndex : byte;
    function    GetParamIndex : byte;
    function    GetMorphIndex : byte;

    procedure   SetRange( const aValue : byte);
    procedure   SetSlotIndex(const aValue: byte);
    procedure   SetLocationIndex( const aValue : byte);
    procedure   SetModuleIndex( const aValue : byte);
    procedure   SetParamIndex( const aValue : byte);
    procedure   SetMorphIndex( const aValue : byte);

    procedure   Init;
    procedure   Write( aChunk : TPatchChunk);
  end;

  // Parameter class

  TG2Param = class(TG2ObservedObject, IG2Param)
  private
    [Weak] FWModule : IG2Module;

    //FSlotIndex         : byte;
    //FLocationIndex     : byte;
    //FModuleIndex       : byte;

    FID                : integer;

    FParamType         : TParamType;
    FParamIndex        : integer;
    FLowValue          : byte;
    FHighValue         : byte;
    FDefaultValue      : byte;
    FParamName         : string;
    FModuleName        : string;
    FDefaultParamLabel : string;
    FCanChangeLabel    : boolean;
    FDefaultKnobIndex  : integer; // The knob nr, when a module is assigned to a parameter page
    FButtonParamIndex  : integer; // The parameter that is assigned to the button below the knob on the param page
    FButtonText        : TStrings;
    FInfoFunctionIndex : integer;

    FSuspendUpdate     : boolean; // Prevent large amount of updates for none-responseless updates (virtual params: Masterclock/Voices)
    FSuspendedValue    : byte; // Value to show when suspended

    function    GetLabelIndex: integer;
    function    GetLabelOnValue: boolean;
    function    GetMorphAssignmentsAllowed: boolean;
    function    GetKnobAssignmentsAllowed: boolean;
    function    GetMidiAssignmentsAllowed: boolean;

    function    GetParamID : integer;
    function    GetParamType : TParamType;
    function    GetParamIndex : integer;
    function    GetDefaultValue : byte;
    function    GetSlotIndex: integer;
    function    GetLocationIndex : integer;
    function    GetModuleIndex : integer;
    function    GetParamName : string;
    function    GetModuleName : string;
    function    GetCanChangeLabel : boolean;
    function    GetInfoFunctionIndex : integer;
    function    GetButtonParamIndex : integer;
    function    GetDefaultKnobIndex : integer;
    function    GetButtonTextList : TStrings;
    function    GetPatch: IG2Patch;
    function    GetModule : IG2Module;
    function    GetSuspendUpdate: boolean;
    function    GetKnob : IG2Knob;
    function    GetController : IG2Controller;
    function    GetGlobalKnob : IG2Knob;

    procedure   SetHighValue( const aValue : byte);
    procedure   SetLowValue( const aValue : byte);
    procedure   SetParamID( const aValue : integer);
    procedure   SetParamType( const aValue : TParamType);
    //procedure   SetParamIndex( const aValue : integer);
    procedure   SetDefaultValue( const aValue : byte);
    //procedure   SetModuleIndex( const aValue : integer);
    procedure   SetParamName( const aValue : string);
    procedure   SetModuleName( const aValue : string);
    procedure   SetCanChangeLabel( const aValue : boolean);
    procedure   SetInfoFunctionIndex( const aValue : integer);
    procedure   SetButtonParamIndex( const aValue : integer);
    procedure   SetDefaultKnobIndex( const aValue : integer);
  protected
    procedure   SetSelected( const aValue : boolean); virtual;
    function    GetSelected : boolean;
    function    GetButtonText( Index : integer): string;
    procedure   SetButtonText( Index : integer; aValue : string);
    function    GetSelectedButtonText: string;
    function    GetButtonTextCount : integer;
    function    GetButtonParam : IG2Param;
    procedure   SetSuspendUpdate( aValue : boolean);
    function    GetHighValue : byte;
    function    GetLowValue : byte;
    function    GetValueText(const aIndex : integer): string;
  public
    constructor Create(aModule: IG2Module; const aParamIndex: integer;
      const aParamType: TParamType; const aParamID: integer;
      const aDefaultValue: byte; const aDefaultKnobIndex,
      aButtonParamIndex : integer; const aModuleName, aParamName,
      aDefaultParamLabel: string); reintroduce;
    destructor  Destroy; override;

    procedure   DoAssignGlobalKnob( aKnobIndex: byte);
    procedure   DoDeassignGlobalKnob( aKnobIndex : byte);
    procedure   DoAssignKnob( aSlotIndex, aKnobIndex : byte);
    procedure   DoDeassignKnob( aSlotIndex, aKnobIndex : byte);

    function    CreateMessage: TG2SendMessage;
    function    CreateUndoMessage: TG2SendMessage;

    procedure   AddMessAssignKnob( const aKnob : IG2Knob; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignKnob( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAssignController( const aMidiCC : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignController( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessAssignGlobalKnob( const aGlobalKnob : IG2Knob; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessDeassignGlobalKnob( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessSetMorph( const aMorph : IG2ParamMorph; const aVariation : byte; aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessClearMorph( const aMorph : IG2ParamMorph; const aVariation : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    procedure   SetValue( const aValue : byte); virtual;
    function    GetValue: byte; virtual;
    procedure   IncValue;
    procedure   DecValue;
    procedure   IncMorphValue;
    procedure   DecMorphValue;

    function    GetParamLabel(const aIndex : integer): string;
    function    GetMorph( const aMorphIndex, aVariation : byte) : IG2ParamMorph;
    function    HasMorph: boolean;
    function    CreateMorph( const aMorphIndex, aVariationIndex, aRange : byte): IG2ParamMorph;
    function    GetSelectedMorphValue : byte;
    function    GetMorphValue( const aMorphIndex, aVariation : integer) : byte;
    procedure   SetMorphValue( const aMorphIndex, aVariation : integer; const aValue : byte);
    procedure   SetSelectedMorphValue( const Value: byte);
    procedure   InvalidateControl; virtual;

    property    Patch: IG2Patch read GetPatch;
    property    Module: IG2Module read GetModule;
    property    Knob : IG2Knob read GetKnob;
    property    GlobalKnob : IG2Knob read GetGlobalKnob;
    property    Controller : IG2Controller read GetController;
    property    SlotIndex: integer read GetSlotIndex;
    property    LocationIndex : integer read GetLocationIndex;
    property    ModuleIndex : integer read GetModuleIndex;
  end;

implementation

//------------------------------------------------------------------------------
//
//                              TMorphParameter
//
//------------------------------------------------------------------------------

function TG2ParamMorph.GetAbsRange: byte;
begin
  if FRange >= 128 then begin
    Result := abs(FRange - 256);
 end else
    Result := FRange;
end;

function TG2ParamMorph.GetLocationIndex: byte;
begin
  Result := FLocationIndex;
end;

function TG2ParamMorph.GetModuleIndex: byte;
begin
  Result := FModuleIndex;
end;

function TG2ParamMorph.GetMorphIndex: byte;
begin
  Result := FMorphIndex;
end;

function TG2ParamMorph.GetNegative: byte;
begin
  if FRange >= 128 then begin
    Result := 1
 end else
    Result := 0;
end;

function TG2ParamMorph.GetParamIndex: byte;
begin
  Result := FParamIndex;
end;

function TG2ParamMorph.GetRange: byte;
begin
  Result := FRange;
end;

function TG2ParamMorph.GetSlotIndex: byte;
begin
  Result := FSlotIndex;
end;

procedure TG2ParamMorph.Init;
begin
  FSlotIndex     := 0;
  FLocationIndex := 0;
  FModuleIndex   := 0;
  FParamIndex    := 0;
  FMorphIndex    := 0;
  FRange         := 0;
end;

procedure TG2ParamMorph.SetLocationIndex(const aValue: byte);
begin
  FLocationIndex := aValue;
end;

procedure TG2ParamMorph.SetModuleIndex(const aValue: byte);
begin
  FModuleIndex := aValue;
end;

procedure TG2ParamMorph.SetMorphIndex(const aValue: byte);
begin
  FMorphIndex := aValue;
end;

procedure TG2ParamMorph.SetParamIndex(const aValue: byte);
begin
  FParamIndex := aValue;
end;

procedure TG2ParamMorph.SetRange(const aValue: byte);
begin
  FRange := aValue;
end;

procedure TG2ParamMorph.SetSlotIndex(const aValue: byte);
begin
  FSlotIndex := aValue;
end;

procedure TG2ParamMorph.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FLocationIndex, 2);
  aChunk.WriteBits(FModuleIndex, 8);
  aChunk.WriteBits(FParamIndex, 7);
  aChunk.WriteBits(FMorphIndex, 4);
  aChunk.WriteBits(FRange, 8);
end;

//------------------------------------------------------------------------------
//
//                               TG2Param
//
//------------------------------------------------------------------------------

constructor TG2Param.Create(aModule: IG2Module; const aParamIndex: integer;
  const aParamType: TParamType; const aParamID: integer;
  const aDefaultValue: byte; const aDefaultKnobIndex,
  aButtonParamIndex : integer; const aModuleName, aParamName,
  aDefaultParamLabel: string);
begin
  inherited Create;

  SetWeak(@FWModule, aModule);

  FParamIndex := aParamIndex;

  FCanChangeLabel := False;
  FInfoFunctionIndex := -1;
  FSuspendUpdate := False;
  FSuspendedValue := 0;

  FID := aParamID;
  FParamType := aParamType;
  FParamName := aParamName;
  FDefaultParamLabel := aDefaultParamLabel;
  FCanChangeLabel := aDefaultParamLabel <> '';
  FDefaultValue := aDefaultValue;
  FModuleName := aModuleName;
  FDefaultKnobIndex := aDefaultKnobIndex;
  FButtonParamIndex := aButtonParamIndex;
  FButtonText := TStringList.Create;

  FLowValue := ParamDefs[FID].LowValue;
  FHighValue := ParamDefs[FID].HighValue;

  FButtonText.Delimiter := ';';
  FButtonText.StrictDelimiter := True;
  FButtonText.DelimitedText := ParamDefs[FID].slButtonText;
end;

destructor TG2Param.Destroy;
begin
  //NotifyObservers(evtDestroy, self as IG2Param);

  FButtonText.Free;

  SetWeak(@FWModule, nil);

  inherited;
end;

function TG2Param.CreateMorph( const aMorphIndex, aVariationIndex,
  aRange: byte): IG2ParamMorph;
begin
  Result := TG2ParamMorph.Create;
  Result.SlotIndex := SlotIndex;
  Result.LocationIndex := LocationIndex;
  Result.ModuleIndex := ModuleIndex;
  Result.ParamIndex := FParamIndex;
  Result.MorphIndex := aMorphIndex;
  Result.Range := aRange;
end;

procedure TG2Param.DoAssignGlobalKnob(aKnobIndex : byte);
begin
  NotifyObservers(EvtAssignGlobalKnob, self as IG2Param);
end;

procedure TG2Param.DoAssignKnob(aSlotIndex, aKnobIndex: byte);
begin
  NotifyObservers(EvtAssignKnob, self as IG2Param);
end;

procedure TG2Param.DoDeassignGlobalKnob(aKnobIndex: byte);
begin
  NotifyObservers(EvtDeassignGlobalKnob, self as IG2Param);
end;

procedure TG2Param.DoDeassignKnob(aSlotIndex, aKnobIndex: byte);
begin
  NotifyObservers(EvtDeassignKnob, self as IG2Param);
end;

function TG2Param.GetButtonText(Index: integer): string;
begin
  if Index < FButtonText.Count then
    Result := FButtonText[Index]
  else
    Result := '';
end;

procedure TG2Param.SetButtonParamIndex(const aValue: integer);
begin
  FButtonParamIndex := aValue;
end;

procedure TG2Param.SetButtonText(Index: integer; aValue: string);
begin
  if Index < FButtonText.Count then
    FButtonText[Index] := aValue;
end;

procedure TG2Param.SetCanChangeLabel(const aValue: boolean);
begin
  FCanChangeLabel := aValue;
end;

procedure TG2Param.SetDefaultKnobIndex(const aValue: integer);
begin
  FDefaultKnobIndex := aValue;
end;

procedure TG2Param.SetDefaultValue(const aValue: byte);
begin
  FDefaultValue := aValue;
end;

procedure TG2Param.SetHighValue(const aValue: byte);
begin
  FHighValue := aValue;
end;

procedure TG2Param.SetInfoFunctionIndex(const aValue: integer);
begin
  FInfoFunctionIndex := aValue;

  if assigned(Module) then
    Module.AddInfoFunction(
      FInfoFunctionIndex,
      FParamIndex);
end;

procedure TG2Param.SetLowValue(const aValue: byte);
begin
  FLowValue := aValue;
end;

function TG2Param.GetParamLabel(const aIndex : integer): string;
begin
  if assigned(Module) then
    Result := Module.GetParamLabel( FParamIndex, aIndex)
  else
    Result := '';
end;

function TG2Param.GetParamName: string;
begin
  Result := FParamName;
end;

function TG2Param.GetParamType: TParamType;
begin
  Result := FParamType;
end;

function TG2Param.GetPatch: IG2Patch;
begin
  if assigned(FWModule.PatchPart) then
    Result := FWModule.PatchPart.Patch
  else
    Result := nil;
end;

procedure TG2Param.SetParamID(const aValue: integer);
begin
  FID := aValue;
end;

{procedure TG2Param.SetParamIndex(const aValue: integer);
begin
  FParamIndex := aValue;
end;}

procedure TG2Param.SetParamName(const aValue: string);
begin
  FParamName := aValue;
end;

procedure TG2Param.SetParamType(const aValue: TParamType);
begin
  FParamType := aValue;
end;

function TG2Param.GetValueText(const aIndex: integer): string;
begin
  Result := GetParamLabel(aIndex);
  if Result = '' then
    if aIndex < FButtonText.Count then
      Result := FButtonText[aIndex];
end;

function TG2Param.GetButtonTextCount: integer;
begin
  Result := FButtonText.Count;
end;

function TG2Param.GetButtonTextList: TStrings;
begin
  Result := FButtonText;
end;

function TG2Param.GetCanChangeLabel: boolean;
begin
  Result := FCanChangeLabel;
end;

function TG2Param.GetController: IG2Controller;
begin
  if assigned(Patch) then
    Result := Patch.FindController(LocationIndex, ModuleIndex, FParamIndex)
  else
    Result := nil;
end;

function TG2Param.GetDefaultKnobIndex: integer;
begin
  Result := FDefaultKnobIndex;
end;

function TG2Param.GetDefaultValue: byte;
begin
  Result := FDefaultValue;
end;

function TG2Param.GetMorph(const aMorphIndex, aVariation : byte) : IG2ParamMorph;
begin
  if assigned(Patch) then
    Result := Patch.FindMorph(LocationIndex, ModuleIndex, FParamIndex, aMorphIndex, aVariation)
  else
    Result := nil;
end;

function TG2Param.HasMorph: boolean;
begin
  if assigned(Patch) then
    Result := Patch.HasMorph(LocationIndex, ModuleIndex, FParamIndex, Patch.Settings.ActiveVariation)
  else
    Result := False;
end;

function TG2Param.GetMorphValue(const aMorphIndex, aVariation : integer) : byte;
begin
  if assigned(Patch) then
    Result := Patch.GetMorphValue(LocationIndex, ModuleIndex, FParamIndex, aMorphIndex, aVariation)
  else
    Result := 0;
end;

{procedure TG2Param.SetModuleIndex(const aValue: integer);
begin
  FModuleIndex := aValue;
end;}

procedure TG2Param.SetModuleName(const aValue: string);
begin
  FModuleName := aValue;
end;

procedure TG2Param.SetMorphValue(const aMorphIndex, aVariation: integer;
  const aValue: byte);
begin
  if not assigned(Patch) then
    exit;

  Patch.SetMorphValue(LocationIndex, ModuleIndex, FParamIndex, aMorphIndex, aValue, aVariation);
  {if aValue >= 128 then
    FWConnection.ParamSetMorph(FSlotIndex, FLocationIndex, FModuleIndex, FParamIndex, aMorphIndex, aValue, 1, 256 - aVariation)
  else
    FWConnection.ParamSetMorph(FSlotIndex, FLocationIndex, FModuleIndex, FParamIndex, aMorphIndex, aValue, 0, aVariation);}
end;

function TG2Param.GetSelectedMorphValue : byte;
begin
  if assigned(Patch) then
    Result :=  GetMorphValue(Patch.SelectedMorphIndex, Patch.Settings.ActiveVariation)
  else
    Result := 0;
end;

function TG2Param.GetSlotIndex: integer;
begin
  if assigned(Patch) then
    Result := Patch.SlotIndex
  else
    Result := -1;
end;

function TG2Param.GetSuspendUpdate: boolean;
begin
  Result := FSuspendUpdate;
end;

procedure TG2Param.SetSelectedMorphValue( const Value: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if not assigned(Patch) then
    exit;

  if Patch.EditAllVariations then begin
    FromVariation := 0;
    ToVariation := N_VARIATIONS - 1;
  end else begin
    FromVariation := Patch.Settings.ActiveVariation;
    ToVariation := Patch.Settings.ActiveVariation;
  end;

  for Variation := FromVariation to ToVariation do
    SetMorphValue(Patch.SelectedMorphIndex, Variation, Value);
  InvalidateControl;
end;

procedure TG2Param.SetSuspendUpdate(aValue: boolean);
var Variation, FromVariation, ToVariation : byte;
begin
  if aValue <> FSuspendUpdate then begin
    FSuspendUpdate := aValue;
    if FSuspendUpdate then begin
      if assigned(Patch) then begin
        case FParamType of
          ptParam:
            FSuspendedValue := Patch.GetParameterValue(LocationIndex, ModuleIndex, FParamIndex, Patch.Settings.ActiveVariation);
          ptMode:
            FSuspendedValue := Patch.GetModeValue(LocationIndex, ModuleIndex, FParamIndex);
          ptMasterClock:
            case FParamIndex of
            0 : FSuspendedValue := Patch.GetMasterClock;
            1 : FSuspendedValue := Patch.GetMasterClockRun;
            end;
          ptVoiceMode:
            case FParamIndex of
            0 : FSuspendedValue := Patch.Settings.VoiceCount;
            1 : FSuspendedValue := Patch.Settings.VoiceMode;
            end;
        end;
      end else
        FSuspendedValue := 0;
    end else begin
      if assigned(Patch) then begin
        case FParamType of
          ptParam:
            begin;
              if Patch.EditAllVariations then begin
                FromVariation := 0;
                ToVariation := N_VARIATIONS - 1;
              end else begin
                FromVariation := Patch.Settings.ActiveVariation;
                ToVariation := Patch.Settings.ActiveVariation;
              end;

              for Variation := FromVariation to ToVariation do begin
                Patch.SetParamValue(LocationIndex, ModuleIndex, FParamIndex, Variation, FSuspendedValue)
              end;
            end;
          ptMode: Patch.SetModeValue(LocationIndex, ModuleIndex, FParamIndex, FSuspendedValue);
          ptMasterClock:
            begin
              case FParamIndex of
              0: Patch.SetMasterClock( FSuspendedValue);
              1: Patch.SetMasterClockRun( FSuspendedValue);
              end;
            end;
          ptVoiceMode:
            begin
              case FParamIndex of
              0: Patch.Settings.VoiceCount := FSuspendedValue;
              1: Patch.Settings.VoiceMode := FSuspendedValue;
              end;
            end;
        end;
      end;
    end;
  end;
end;

function TG2Param.GetValue: byte;
begin
  Result := 0;
  if FSuspendUpdate then begin
    Result := FSuspendedValue
  end else
    if assigned(Patch) then begin
      case FParamType of
        ptParam:
          Result := Patch.GetParameterValue(LocationIndex, ModuleIndex, FParamIndex, Patch.Settings.ActiveVariation);
        ptMode:
          Result := Patch.GetModeValue(LocationIndex, ModuleIndex, FParamIndex);
        ptMasterClock:
          case FParamIndex of
          0 : Result := Patch.GetMasterClock;
          1 : Result := Patch.GetMasterClockRun;
          end;
        ptVoiceMode:
          case FParamIndex of
          0 : Result := Patch.Settings.VoiceCount;
          1 : Result := Patch.Settings.VoiceMode;
          end;
      end;
    end;
end;

procedure TG2Param.SetValue( const aValue: byte);
var Variation, FromVariation, ToVariation : byte;
begin
  if FSuspendUpdate then begin
    FSuspendedValue := aValue;
    InvalidateControl;
  end else
    if assigned(Patch) then begin
      case FParamType of
        ptParam:
          begin;
            if Patch.EditAllVariations then begin
              FromVariation := 0;
              ToVariation := N_VARIATIONS - 1;
            end else begin
              FromVariation := Patch.Settings.ActiveVariation;
              ToVariation := Patch.Settings.ActiveVariation;
            end;

            for Variation := FromVariation to ToVariation do begin
              Patch.SetParamValue(LocationIndex, ModuleIndex, FParamIndex, Variation, aValue);
              //FWConnection.ParamSetValue(SlotIndex, LocationIndex, ModuleIndex, FParamIndex, Variation, aValue);
            end;
          end;
        ptMode: Patch.SetModeValue(LocationIndex, ModuleIndex, FParamIndex, aValue);
        ptMasterClock:
          begin
            case FParamIndex of
            0: Patch.SetMasterClock( aValue);
            1: Patch.SetMasterClockRun( aValue);
            end;
          end;
        ptVoiceMode:
          begin
            case FParamIndex of
            0: Patch.Settings.VoiceCount := aValue;
            1: Patch.Settings.VoiceMode := aValue;
            end;
          end;
      end;
    end;
end;

procedure TG2Param.IncValue;
var Value : byte;
begin
  Value := GetValue;
  if Value < FHighValue then
    SetValue( Value + 1)
end;

procedure TG2Param.DecValue;
var Value : byte;
begin
  Value := GetValue;
  if Value > FLowValue then
    SetValue( Value - 1)
end;

procedure TG2Param.IncMorphValue;
var Value: byte;
    Range : integer;
begin
  Value := GetValue;
  Range := GetSelectedMorphValue;

  if Range >= 128 then
    Range := Range - 256;

  inc(Range);

  if (Value + Range) <= FHighValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

procedure TG2Param.DecMorphValue;
var Value: byte;
    Range : integer;
begin
  Value := GetValue;
  Range := GetSelectedMorphValue;

  if Range >= 128 then
    Range := Range - 256;

  dec(Range);

  if (Value + Range) >= FLowValue then begin
    if Range < 0 then
      SetSelectedMorphValue( 256 + Range)
    else
      SetSelectedMorphValue( Range);
  end;
end;

function TG2Param.CreateMessage: TG2SendMessage;
begin
  Result := TG2SendMessage.Create;
  Result.WriteMessage($01);
  Result.WriteMessage(CMD_REQ + CMD_SLOT + SlotIndex);
  Result.WriteMessage(Patch.Slot.PatchVersion);
end;

function TG2Param.CreateUndoMessage: TG2SendMessage;
begin
  Result := TG2SendMessage.Create;
  Result.AddReversed := True;
  Result.Offset := 5;

  Result.WriteMessage($01);
  Result.WriteMessage(CMD_REQ + CMD_SLOT + SlotIndex);
  Result.WriteMessage(Patch.Slot.PatchVersion);
end;

procedure TG2Param.AddMessAssignKnob(const aKnob : IG2Knob;
  aSendMessage, aUndoMessage : TG2SendMessage);
begin
  // Is the parameter already assigned to a knob?
  if assigned(Knob) and (Knob.IsAssigned = 1) then begin
    // Send a deassign message first
    AddMessDeassignKnob(aSendMessage, aUndoMessage);
  end;

  // Is the knob already assigned to another parameter?
  if assigned(aKnob) and (aKnob.IsAssigned = 1) and (aKnob.Param <> self as IG2Param) then begin
    // Send a deassign message first
    aKnob.Param.AddMessDeassignKnob( aSendMessage, aUndoMessage);
  end;

  // Asign knob to parameter
  aSendMessage.AddMessKnobAssign(LocationIndex, ModuleIndex, FParamIndex, aKnob.KnobIndex);
  // Add undo
  aUndoMessage.AddMessKnobDeassign(aKnob.KnobIndex);
end;

procedure TG2Param.AddMessClearMorph( const aMorph: IG2ParamMorph;
  const aVariation: byte; aSendMessage, aUndoMessage: TG2SendMessage);
begin
  aSendMessage.AddMessMorphSet(LocationIndex, ModuleIndex, FParamIndex,
        aMorph.MorphIndex, 0, 0, aVariation);

  aUndoMessage.AddMessMorphSet(LocationIndex, ModuleIndex, FParamIndex,
        aMorph.MorphIndex, aMorph.AbsRange, aMorph.Negative,
        aVariation);
end;

procedure TG2Param.AddMessDeassignKnob(aSendMessage,
  aUndoMessage : TG2SendMessage);
begin
  if assigned(Knob) and (Knob.IsAssigned = 1) then begin
    aSendMessage.AddMessKnobDeassign(Knob.KnobIndex);

    // Create undo
    aUndoMessage.AddMessKnobAssign(LocationIndex, ModuleIndex, FParamIndex, Knob.KnobIndex);
  end;
end;

procedure TG2Param.AddMessSetMorph(const aMorph: IG2ParamMorph;
  const aVariation : byte; aSendMessage, aUndoMessage: TG2SendMessage);
var CurrentMorph : IG2ParamMorph;
begin
  CurrentMorph := Patch.FindMorph(LocationIndex, ModuleIndex, FParamIndex, aMorph.MorphIndex, aVariation);
  if assigned(CurrentMorph) then
    aUndoMessage.AddMessMorphSet(LocationIndex, ModuleIndex, FParamIndex,
        CurrentMorph.MorphIndex, CurrentMorph.AbsRange, CurrentMorph.Negative,
        aVariation);
  aSendMessage.AddMessMorphSet(LocationIndex, ModuleIndex, FParamIndex,
        aMorph.MorphIndex, aMorph.AbsRange, aMorph.Negative,
        aVariation);
end;

procedure TG2Param.AddMessAssignGlobalKnob(
  const aGlobalKnob : IG2Knob; aSendMessage,
  aUndoMessage : TG2SendMessage);
begin
  // Is the parameter already assigned to a knob?
  if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then begin
    // Send a deassign message first
    AddMessDeassignGlobalKnob(aSendMessage, aUndoMessage);
  end;

  // Is the knob already assigned to another parameter?
  if assigned(aGlobalKnob) and (aGlobalKnob.IsAssigned = 1) and (aGlobalKnob.Param <> self as IG2Param) then begin
    // Send a deassign message first
    aGlobalKnob.Param.AddMessDeassignGlobalKnob( aSendMessage, aUndoMessage);
  end;

  // Asign knob to parameter
  aSendMessage.AddMessGlobalKnobAssign(SlotIndex, LocationIndex, ModuleIndex, FParamIndex, aGlobalKnob.KnobIndex);
  // Add undo
  aUndoMessage.AddMessGlobalKnobDeassign(aGlobalKnob.KnobIndex);
end;

procedure TG2Param.AddMessDeassignGlobalKnob(aSendMessage, aUndoMessage : TG2SendMessage);
begin
  if assigned(GlobalKnob) and (GlobalKnob.IsAssigned = 1) then begin
    aSendMessage.AddMessGlobalKnobDeassign(GlobalKnob.KnobIndex);

    // Create undo
    aUndoMessage.AddMessGlobalKnobAssign(SlotIndex, LocationIndex, ModuleIndex, FParamIndex, GlobalKnob.KnobIndex);
  end;
end;

procedure TG2Param.AddMessAssignController(const aMidiCC : byte; aSendMessage, aUndoMessage : TG2SendMessage);
var CurrentController : IG2Controller;
begin
  // Is CC already assigned?
  CurrentController := Patch.FindController(aMidiCC);
  if assigned(CurrentController) then begin
    if assigned(CurrentController.Param) then
      CurrentController.Param.AddMessDeassignController(aSendMessage, aUndoMessage);
  end;
  aSendMessage.AddMessMidiCCAssign(LocationIndex, ModuleIndex, FParamIndex, aMidiCC);
  // Create undo
  aUndoMessage.AddMessMidiCCDeassign( aMidiCC);
end;

procedure TG2Param.AddMessDeassignController(aSendMessage, aUndoMessage : TG2SendMessage);
begin
  if assigned(Controller) then begin

    aSendMessage.AddMessMidiCCDeassign(Controller.MidiCC);

    // Create undo
    aUndoMessage.AddMessMidiCCAssign(LocationIndex, ModuleIndex, FParamIndex, Controller.MidiCC);
  end;
end;

function TG2Param.GetParamID: integer;
begin
  Result := FID;
end;

function TG2Param.GetParamIndex: integer;
begin
  Result := FParamIndex;
end;

function TG2Param.GetSelectedButtonText: string;
var ParamValue : byte;
begin
  if FCanChangeLabel then
    Result := GetParamLabel(0)
  else begin
    ParamValue := GetValue;
    if ParamValue < FButtonText.Count then
      Result := FButtonText[ParamValue]
    else
{      if assigned(Module) then
        Result := Module.InfoFunction[FInfoFunctionIndex].AsText
      else}
        Result := '';
  end;
end;

function TG2Param.GetKnob: IG2Knob;
begin
  if assigned(Patch) then
    Result := Patch.FindKnob(LocationIndex, ModuleIndex, FParamIndex)
  else
    Result := nil;
end;

function TG2Param.GetKnobAssignmentsAllowed: boolean;
begin
  if LocationIndex = LOCATION_PATCH then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

function TG2Param.GetModule: IG2Module;
begin
  Result := FWModule;
end;

function TG2Param.GetModuleIndex: integer;
begin
  Result := FWModule.ModuleIndex;
end;

function TG2Param.GetModuleName: string;
begin
  Result := FWModule.ModuleName;
end;

function TG2Param.GetMorphAssignmentsAllowed: boolean;
begin
  if LocationIndex = LOCATION_PATCH then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

function TG2Param.GetMidiAssignmentsAllowed: boolean;
begin
  if LocationIndex = LOCATION_PATCH then begin
    case ModuleIndex of
      PATCH_MASTERCLOCK : Result := False;
      PATCH_VOICES : Result := False;
      PATCH_VOLUME : Result := FParamIndex <> VOLUME_LEVEL;
    else
      Result := True;
    end;
  end else begin
    Result := True;
  end;
end;

function TG2Param.GetButtonParam : IG2Param;
begin
  if assigned(Module) and (FButtonParamIndex <> -1) then begin
    Result := Module.Param[FButtonParamIndex]
  end else
    Result := nil;
end;

function TG2Param.GetButtonParamIndex: integer;
begin
  Result := FButtonParamIndex;
end;

function TG2Param.GetSelected : boolean;
begin
  Result := Module.SelectedParam = self as IG2Param
end;

procedure TG2Param.SetSelected( const aValue : boolean);
begin
  if not assigned(Patch) then
    exit;

  if aValue then  begin
    Patch.SelectParam(Locationindex, ModuleIndex, FParamIndex);
  end;
end;

function TG2Param.GetGlobalKnob: IG2Knob;
begin
  if assigned(Patch) then
    Result := Patch.Perf.FindKnob(SlotIndex, LocationIndex, ModuleIndex, FParamIndex)
  else
    Result := nil;
end;

function TG2Param.GetHighValue :  byte;
begin
  Result := FHighValue;
end;

function TG2Param.GetInfoFunctionIndex: integer;
begin
  Result := FInfoFunctionIndex;
end;

function TG2Param.GetLabelIndex: integer;
begin
  if GetLabelOnValue then // Switch
    Result := GetValue
  else
    Result := 0;
end;

function TG2Param.GetLabelOnValue: boolean;
begin
  Result := FID in [167, 168, 169];
end;

function TG2Param.GetLocationIndex: integer;
begin
  Result := FWModule.LocationIndex;
end;

function TG2Param.GetLowValue: byte;
begin
  Result := FLowValue;
end;

procedure TG2Param.InvalidateControl;
begin
  NotifyObservers(EvtInvalidate, self as IG2Param);
end;

end.
