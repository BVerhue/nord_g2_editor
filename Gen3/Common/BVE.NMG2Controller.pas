unit BVE.NMG2Controller;

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
  TController = class( TInterfacedObject, IG2Controller)
  private
    [Weak] FWParam : IG2Param;

    FMidiCC        : TBits7;

    function  GetMidiCC : TBits7;
    function  GetLocationIndex : TBits2;
    function  GetModuleIndex : TBits8;
    function  GetParamIndex : TBits7;
    function  GetParam : IG2Param;

    procedure SetMidiCC(const aValue : TBits7);
    procedure SetParam( const aValue : IG2Param);
  public
    constructor Create(const aMidiCC: TBits7; aParam: IG2Param);
    destructor  Destroy; override;

    procedure Write(aChunk : TPatchChunk);

    property  MidiCC : TBits7 read GetMidiCC write SetMidiCC;
    property  LocationIndex : TBits2 read GetLocationIndex;
    property  ModuleIndex : TBits8 read GetModuleIndex;
    property  ParamIndex : TBits7 read GetParamIndex;
    property  Param : IG2Param read GetParam write SetParam;
  end;

implementation

//------------------------------------------------------------------------------
//
//                               TController
//
//------------------------------------------------------------------------------

constructor TController.Create(const aMidiCC: TBits7; aParam: IG2Param);
begin
  inherited Create;

  SetWeak(@FWParam, aParam);
  FMidiCC := aMidiCC;
end;

destructor TController.Destroy;
begin
  SetWeak(@FWParam, nil);

  inherited;
end;

function TController.GetLocationIndex: TBits2;
begin
  if assigned(Param) then
    Result := Param.LocationIndex
  else
    Result := 0;
end;

function TController.GetMidiCC: TBits7;
begin
  Result := FMidiCC;
end;

function TController.GetModuleIndex: TBits8;
begin
  if assigned(Param) then
    Result := Param.ModuleIndex
  else
    Result := 0;
end;

function TController.GetParam: IG2Param;
begin
  Result := FWParam;
end;

function TController.GetParamIndex: TBits7;
begin
  if assigned(Param) then
    Result := Param.ParamIndex
  else
    Result := 0;
end;

procedure TController.SetMidiCC(const aValue: TBits7);
begin
  FMidiCC := aValue;
end;

procedure TController.SetParam(const aValue: IG2Param);
begin
  SetWeak(@FWParam, aValue);
end;

procedure TController.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(FMidiCC, 7);
  aChunk.WriteBits(LocationIndex, 2);
  aChunk.WriteBits(ModuleIndex, 8);
  aChunk.WriteBits(ParamIndex, 7);
end;

end.
