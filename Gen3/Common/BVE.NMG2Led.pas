unit BVE.NMG2Led;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

interface

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Generics.Defaults,
  BVE.NMG2Types,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  TG2FileLed = class(TG2ObservedObject, IG2Led)
  private
    FLocationIndex: byte;
    FModuleIndex: byte;
    FLedType: TLedType;
    FGroupID: byte;
    FGroupCount: integer;
    FValue: byte;
  protected
    function GetValue: byte;
    function GetHighValue: byte;
    function GetLowValue: byte;
    function GetLocationIndex: byte;
    function GetGroupID: byte;
    function GetModuleIndex: byte;
    function GetLedType: TLedType;
    function GetGroupCount: integer;
    function HasMorph: boolean;
    function GetSelectedMorphValue: byte;
    function GetValueText(aIndex: integer): string;
    procedure SetValue(const aValue: byte); virtual;
  public
    constructor Create(const aLedType: TLedType;
      const aLocationIndex, aModuleIndex, aGroupID: byte;
      const aGroupCount: integer);
    destructor Destroy; override;
  end;

implementation

// ------------------------------------------------------------------------------
//
// TG2FileLed
//
// ------------------------------------------------------------------------------

constructor TG2FileLed.Create(const aLedType: TLedType;
  const aLocationIndex, aModuleIndex, aGroupID: byte;
  const aGroupCount: integer);
begin
  inherited Create;

  FLocationIndex := aLocationIndex;
  FModuleIndex := aModuleIndex;
  FLedType := aLedType;
  FGroupID := aGroupID;
  FGroupCount := aGroupCount;
end;

destructor TG2FileLed.Destroy;
begin
  inherited;
end;

function TG2FileLed.GetGroupCount: integer;
begin
  Result := FGroupCount;
end;

function TG2FileLed.GetGroupID: byte;
begin
  Result := FGroupID;
end;

function TG2FileLed.GetHighValue: byte;
begin
  Result := 127;
end;

function TG2FileLed.GetLedType: TLedType;
begin
  Result := FLedType;
end;

function TG2FileLed.GetLocationIndex: byte;
begin
  Result := FLocationIndex;
end;

function TG2FileLed.GetLowValue: byte;
begin
  Result := 0;
end;

function TG2FileLed.GetModuleIndex: byte;
begin
  Result := FModuleIndex;
end;

function TG2FileLed.GetSelectedMorphValue: byte;
begin
  Result := 0;
end;

function TG2FileLed.GetValue: byte;
begin
  Result := FValue;
end;

function TG2FileLed.GetValueText(aIndex: integer): string;
begin
  Result := '';
end;

function TG2FileLed.HasMorph: boolean;
begin
  Result := False;
end;

procedure TG2FileLed.SetValue(const aValue: byte);
begin
  if aValue <> FValue then
  begin
    FValue := aValue;
    NotifyObservers(EvtValueChange, self as IG2Led);
  end;
end;

end.
