unit BVE.NMG2Knob;

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
  BVE.NMG2FileIntf;

type
  TG2Knob = class(TInterfacedObject, IG2Knob)
  private
    [Weak] FWParam: IG2Param;

    FAssigned      : TBits1;
    FIsLed         : TBits2;
    FKnobIndex     : byte;

    function     GetKnobValue : byte;
    procedure    SetKnobValue(const aValue : byte);
    function     GetKnobFloatValue : single;
    procedure    SetKnobFloatValue(const aValue : single);
    function     GetKnobHighValue : byte;
    function     GetKnobLowValue : byte;
    function     GetKnobButtonValue : byte;
    procedure    SetKnobButtonValue(const aValue : byte);
    function     GetKnobButtonFloatValue : single;
    procedure    SetKnobButtonFloatValue(const aValue : single);

    function     GetKnobIndex : byte;
    function     GetIsAssigned : TBits1;
    function     GetIsLed : TBits2;
    function     GetSlotIndex: TBits2;
    function     GetLocationIndex : byte;
    function     GetModuleIndex : TBits8;
    function     GetParamIndex : TBits7;
    function     GetParam : IG2Param;

    procedure    SetIsAssigned( const aValue : TBits1);
    procedure    SetIsLed( const aValue : TBits2);
    procedure    SetParam( const aValue : IG2Param);
  public
    constructor Create(const aKnobIndex: byte; aParam: IG2Param);
    destructor  Destroy; override;

    procedure    Init; virtual;
    procedure    Write( aChunk : TPatchChunk); virtual;

    property     SlotIndex : byte read GetSlotIndex;
    property     LocationIndex : TBits2 read GetLocationIndex;
    property     ModuleIndex : TBits8 read GetModuleIndex;
    property     ParamIndex : TBits7 read GetParamIndex;
    property     Param: IG2Param read GetParam write SetParam;
  end;

  TG2GlobalKnob = class( TG2Knob, IG2Knob)
  public
    constructor Create(const aKnobIndex: byte; aParam: IG2Param);
    destructor  Destroy; override;

    procedure   Write( aChunk : TPatchChunk); override;
  end;

implementation

//------------------------------------------------------------------------------
//
//                                  TKnob
//
//------------------------------------------------------------------------------

procedure TG2Knob.Init;
begin
 FAssigned := 0;
 FIsLed := 0;
 SetWeak(@FWParam, nil);
end;

procedure TG2Knob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(LocationIndex, 2);
    aChunk.WriteBits(ModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(ParamIndex, 7);
  end;
end;

function TG2Knob.GetKnobValue : byte;
begin
  if (FAssigned = 1) and assigned(Param) then
    Result := Param.GetValue
  else
    Result := 0;
end;

function TG2Knob.GetLocationIndex: TBits2;
begin
  if assigned(Param) then
    Result := Param.Locationindex
  else
    Result := 0;
end;

function TG2Knob.GetModuleIndex: TBits8;
begin
  if assigned(Param) then
    Result := Param.ModuleIndex
  else
    Result := 0;
end;

function TG2Knob.GetParam: IG2Param;
begin
  Result := FWParam;
end;

function TG2Knob.GetParamIndex: TBits7;
begin
  if assigned(Param) then
    Result := Param.ParamIndex
  else
    result := 0;
end;

function TG2Knob.GetSlotIndex: byte;
begin
  if assigned(Param) then
    Result := Param.SlotIndex
  else
    Result := 0;
end;

procedure TG2Knob.SetKnobValue(const aValue : byte);
begin
  if (FAssigned = 1) and assigned(Param) then
    Param.SetValue( aValue);
end;

procedure TG2Knob.SetParam(const aValue: IG2Param);
begin
  SetWeak(@FWParam, aValue);

  if assigned(FWParam) then begin
    FAssigned := 1;
    FIsLed := 0;
  end else begin
    FAssigned := 0;
  end;
end;

function TG2Knob.GetKnobButtonValue : byte;
var ButtonParam : IG2Param;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(Param) then begin
    ButtonParam := Param.ButtonParam;
    if assigned(ButtonParam) then
      Result := ButtonParam.GetValue;
  end;
end;

procedure TG2Knob.SetKnobButtonValue(const aValue : byte);
var ButtonParam : IG2Param;
begin
  if (FAssigned = 1) and assigned(Param) then begin
    ButtonParam := Param.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetValue( aValue);
  end;
end;

function TG2Knob.GetKnobFloatValue : single;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(Param) then
    if Param.HighValue - Param.LowValue <> 0 then
      Result := Param.GetValue / (Param.HighValue - Param.LowValue);
end;

constructor TG2Knob.Create(const aKnobIndex: byte; aParam: IG2Param);
begin
  inherited Create;

  SetWeak(@FWParam, aParam);
  FKnobIndex := aKnobIndex;

  Init;
end;

destructor TG2Knob.Destroy;
begin
  SetWeak(@FWParam, nil);

  inherited;
end;

function TG2Knob.GetIsAssigned: TBits1;
begin
  Result := FAssigned;
end;

function TG2Knob.GetIsLed: TBits2;
begin
  Result := FIsLed;
end;

function TG2Knob.GetKnobButtonFloatValue : single;
var ButtonParam : IG2Param;
begin
  Result := 0;
  if (FAssigned = 1) and assigned(Param) then begin
    ButtonParam := Param.ButtonParam;
    if assigned(ButtonParam) then begin
      if ButtonParam.HighValue - ButtonParam.LowValue <> 0 then
        Result := ButtonParam.GetValue / (ButtonParam.HighValue - ButtonParam.LowValue);
    end;
  end;
end;

procedure TG2Knob.SetKnobFloatValue(const aValue : single);
begin
  if (FAssigned = 1) and assigned(Param) then
    Param.SetValue( trunc(aValue * 127));
end;

procedure TG2Knob.SetIsAssigned(const aValue: TBits1);
begin
  FAssigned := aValue;
end;

procedure TG2Knob.SetIsLed(const aValue: TBits2);
begin
  FIsLed := aValue;
end;

procedure TG2Knob.SetKnobButtonFloatValue(const aValue : single);
var ButtonParam : IG2Param;
begin
  if (FAssigned = 1) and assigned(Param) then begin
    ButtonParam := Param.ButtonParam;
    if assigned(ButtonParam) then
      ButtonParam.SetValue( trunc(aValue * 127));
  end;
end;

function TG2Knob.GetKnobHighValue : byte;
begin
  if (FAssigned = 1) and assigned(Param) then
    Result := Param.HighValue
  else
    Result := 0;
end;

function TG2Knob.GetKnobIndex: byte;
begin
  Result := FKnobIndex;
end;

function TG2Knob.GetKnobLowValue : byte;
begin
  if (FAssigned = 1) and assigned(Param) then
    Result := Param.LowValue
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
//
//                             TG2GlobalKnob
//
//------------------------------------------------------------------------------

constructor TG2GlobalKnob.Create(const aKnobIndex: byte; aParam: IG2Param);
begin
  inherited;

  Init;
end;

destructor TG2GlobalKnob.Destroy;
begin

  inherited;
end;

procedure TG2GlobalKnob.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FAssigned, 1);
  if (FAssigned = 1) then begin
    aChunk.WriteBits(LocationIndex, 2);
    aChunk.WriteBits(ModuleIndex, 8);
    aChunk.WriteBits(FIsLed, 2);
    aChunk.WriteBits(ParamIndex, 7);
    aChunk.WriteBits(SlotIndex, 2);
  end;
end;

end.
