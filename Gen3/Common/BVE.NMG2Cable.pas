unit BVE.NMG2Cable;

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
  TG2FileCable = class;

  TG2FileCable = class(TG2ObservedObject, IG2Cable)
  private
    [weak] FWConnFrom: IG2Connector;
    [weak] FWConnTo: IG2Connector;

    FCableColor: TBits3;
    FLinkType: TBits1; // 0 : input to input; 1 : output to input

    FToDelete: boolean; // Used for uprate calculations

    function    GetModuleIndexFrom : TBits8;
    function    GetConnIndexFrom : TBits6;
    function    GetLinkType : TLinkType;
    function    GetModuleIndexTo : TBits8;
    function    GetConnIndexTo : TBits6;
    function    GetConnFrom : IG2Connector;
    function    GetConnTo : IG2Connector;
    function    GetModuleFrom: IG2Module;
    function    GetModuleTo: IG2Module;
    function    GetCableColor : TCableColor;
    function    GetSlotIndex: byte;
    function    GetLocationIndex : byte;
    function    GetConnFromKind : TConnectorKind;
    function    GetConnToKind : TConnectorKind;
    function    GetToDelete : boolean;

    procedure   SetConnFrom( const aValue : IG2Connector);
    procedure   SetConnTo( const aValue : IG2Connector);
    procedure   SetToDelete( const aValue : boolean);
  protected
    procedure   SetCableColor( const aValue : TCableColor); virtual;
  public
    constructor Create(aConnFrom, aConnTo: IG2Connector; const aColorIndex: byte);
    destructor  Destroy; override;

    procedure   Init;

    procedure   ConnectorMoved; virtual;
    procedure   Invalidate; virtual;
    procedure   Write(aChunk : TPatchChunk);

    procedure   AddMessCableAdd( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessCableDelete( aSendMessage, aUndoMessage : TG2SendMessage);
    procedure   AddMessCableColor( const aColor : byte; aSendMessage, aUndoMessage : TG2SendMessage);

    property    LocationIndex : byte read GetLocationIndex;
    property    ModuleIndexFrom : TBits8 read GetModuleIndexFrom;
    property    ConnIndexFrom : TBits6 read GetConnIndexFrom;
    property    ConnFromKind : TConnectorKind read GetConnFromKind;
    property    LinkType : TLinkType read GetLinkType;
    property    ModuleIndexTo : TBits8 read GetModuleIndexTo;
    property    ConnIndexTo : TBits6 read GetConnIndexTo;
    property    ConnToKind : TConnectorKind read GetConnToKind;
    property    ConnFrom : IG2Connector read GetConnFrom write SetConnFrom;
    property    ConnTo : IG2Connector read GetConnTo write SetConnTo;
    property    ModuleFrom: IG2Module read GetModuleFrom;
    property    ModuleTo: IG2Module read GetModuleTo;
    property    CableColor : TCableColor read GetCableColor write SetCableColor;
  end;

implementation

//------------------------------------------------------------------------------
//
//                              TG2FileCable
//
//------------------------------------------------------------------------------

constructor TG2FileCable.Create(aConnFrom, aConnTo: IG2Connector;
  const aColorIndex: byte);
begin
  inherited Create;

  SetWeak(@FWConnFrom, aConnFrom);
  SetWeak(@FWConnTo, aConnTo);

  FCableColor      := aColorIndex;
  if aConnFrom.ConnectorKind = ckInput  then
    FLinkType := 0
  else
    FLinkType := 1;

  FToDelete := False;
end;

destructor TG2FileCable.Destroy;
begin
  SetWeak(@FWConnFrom, nil);
  SetWeak(@FWConnTo, nil);

  inherited;
end;

function TG2FileCable.GetCableColor: TCableColor;
begin
  Result := TCableColor(FCableColor);
end;

function TG2FileCable.GetConnIndexFrom: TBits6;
begin
  Result := FWConnFrom.ConnectorIndex;
end;

function TG2FileCable.GetConnIndexTo: TBits6;
begin
  Result := FWConnTo.ConnectorIndex;
end;

function TG2FileCable.GetConnFromKind: TConnectorKind;
begin
  Result := FWConnFrom.ConnectorKind;
end;

function TG2FileCable.GetConnToKind: TConnectorKind;
begin
  Result := FWConnTo.ConnectorKind;
end;

function TG2FileCable.GetConnFrom: IG2Connector;
begin
  Result := FWConnFrom;
end;

function TG2FileCable.GetLinkType: TLinkType;
begin
  Result := TLinkType(FLinkType);
end;

function TG2FileCable.GetLocationIndex: byte;
begin
  Result := FWConnFrom.LocationIndex;
end;

function TG2FileCable.GetModuleFrom: IG2Module;
begin
  Result := FWConnFrom.Module;
end;

function TG2FileCable.GetModuleIndexFrom: TBits8;
begin
  Result := FWConnFrom.ModuleIndex;
end;

function TG2FileCable.GetModuleIndexTo: TBits8;
begin
  Result := FWConnTo.ModuleIndex;
end;

function TG2FileCable.GetModuleTo: IG2Module;
begin
  Result := FWConnTo.Module;
end;

function TG2FileCable.GetSlotIndex: byte;
begin
  Result := FWConnFrom.Module.SlotIndex;
end;

function TG2FileCable.GetConnTo: IG2Connector;
begin
  Result :=  FWConnTo;
end;

function TG2FileCable.GetToDelete: boolean;
begin
  Result := FToDelete;
end;

procedure TG2FileCable.Init;
begin
  FCableColor      := 0;
  FLinkType        := 0;
end;

procedure TG2FileCable.Invalidate;
begin
  NotifyObservers(EvtInvalidate, self as IG2Cable);
end;

procedure TG2FileCable.SetCableColor(const aValue: TCableColor);
begin
  if FCableColor <> byte(ord(aValue)) then begin
    FCableColor := ord(aValue);
    Invalidate;
  end;
end;

procedure TG2FileCable.SetConnFrom(const aValue: IG2Connector);
begin
  SetWeak(@FWConnFrom, aValue);
  if assigned(FWConnFrom) then
    if FWConnFrom.ConnectorKind = ckInput then
      FLinkType := 0
    else
      FLinkType := 1;
end;

procedure TG2FileCable.SetConnTo(const aValue: IG2Connector);
begin
  SetWeak(@FWConnTo, aValue);
end;

procedure TG2FileCable.SetToDelete(const aValue: boolean);
begin
  FToDelete := aValue;
end;

procedure TG2FileCable.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits( FCableColor, 3);
  aChunk.WriteBits( ModuleIndexFrom, 8);
  aChunk.WriteBits( ConnIndexFrom, 6);
  aChunk.WriteBits( FLinkType, 1);
  aChunk.WriteBits( ModuleIndexTo, 8);
  aChunk.WriteBits( ConnIndexTo, 6);
end;

procedure TG2FileCable.AddMessCableAdd(aSendMessage,
  aUndoMessage: TG2SendMessage);
begin
  aSendMessage.AddMessCableAdd(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind), FCableColor);

  aUndoMessage.AddMessCableDelete(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind));
end;

procedure TG2FileCable.AddMessCableColor(const aColor: byte; aSendMessage,
  aUndoMessage: TG2SendMessage);
begin
  aSendMessage.AddMessCableColor(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind), aColor);

  aUndoMessage.AddMessCableColor(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind), FCableColor);
end;

procedure TG2FileCable.AddMessCableDelete(aSendMessage,
  aUndoMessage: TG2SendMessage);
begin
  aSendMessage.AddMessCableDelete(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind));

  aUndoMessage.AddMessCableAdd(LocationIndex, ModuleIndexFrom, ConnIndexFrom,
      ord(GetConnFromKind), ModuleIndexTo, ConnIndexTo, ord(GetConnToKind), FCableColor);
end;

procedure TG2FileCable.ConnectorMoved;
begin
  // Abstract
end;

end.
