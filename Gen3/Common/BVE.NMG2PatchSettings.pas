unit BVE.NMG2PatchSettings;

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
  TG2PatchSettings = class(TInterfacedObject, IG2PatchSettings)
  private
    FUnknown1,
    FUnknown2,
    FUnknown3,
    FUnknown4,
    FUnknown5,
    FUnknown6,
    FUnknown7        : TBits8; // array[0..6] of Tbits8;
    FUnknown8        : TBits5;
    FVoiceCount      : TBits5;
    FBarPosition     : TBits14;
    FUnknown9        : TBits3;
    FRedVisible      : TBits1;
    FBlueVisible     : TBits1;
    FYellowVisible   : TBits1;
    FOrangeVisible   : TBits1;
    FGreenVisible    : TBits1;
    FPurpleVisible   : TBits1;
    FWhiteVisible    : TBits1;
    FMonoPoly        : TBits2;
    FActiveVariation : Byte;
    FCategory        : Byte;
    FUnknown10       : TBits4;
  protected
    function GetBarPosition : word;
    function GetRedVisible : boolean;
    function GetBlueVisible : boolean;
    function GetYellowVisible : boolean;
    function GetOrangeVisible : boolean;
    function GetGreenVisible : boolean;
    function GetPurpleVisible : boolean;
    function GetWhiteVisible : boolean;
    function GetCategory : byte;
    function GetActiveVariation : byte;
    function GetVoiceCount : byte;
    function GetVoiceMode : byte;

    procedure SetBarPosition( const aValue : word);
    procedure SetRedVisible( const aValue : boolean);
    procedure SetBlueVisible( const aValue : boolean);
    procedure SetYellowVisible( const aValue : boolean);
    procedure SetOrangeVisible( const aValue : boolean);
    procedure SetGreenVisible( const aValue : boolean);
    procedure SetPurpleVisible( const aValue : boolean);
    procedure SetWhiteVisible( const aValue : boolean);
    procedure SetCategory( const aValue : byte);
    procedure SetActiveVariation( const aValue : byte);
    procedure SetVoiceCount( const aValue : byte);
    procedure SetVoiceMode( const aValue : byte);

  public
    constructor Create;
    constructor CopyCreate( aSource : IG2PatchSettings);

    procedure Init;
    procedure Read( aChunk : TPatchChunk);
    procedure Write( aChunk : TPatchChunk);

    procedure WriteChunk( aStream : TStream);

    property  VoiceCount : TBits5 read FVoiceCount write FVoiceCount;
    property  MonoPoly : Tbits2 read FMonoPoly write FMonoPoly;
    property  ActiveVariation : Byte read FActiveVariation write FActiveVariation;
    property  BarPosition : TBits14 read FBarPosition write FBarPosition;
    property  RedVisible : TBits1 read FRedVisible write FRedVisible;
    property  BlueVisible : TBits1 read FBlueVisible write FBlueVisible;
    property  YellowVisible : TBits1 read FYellowVisible write FYellowVisible;
    property  OrangeVisible : TBits1 read FOrangeVisible write FOrangeVisible;
    property  GreenVisible : TBits1 read FGreenVisible write FGreenVisible;
    property  PurpleVisible : TBits1 read FPurpleVisible write FPurpleVisible;
    property  WhiteVisible : TBits1 read FWhiteVisible write FWhiteVisible;
    property  Categorie : Byte read FCategory write FCategory;
  end;


implementation

//------------------------------------------------------------------------------
//
//                             TG2PatchSettings
//
//------------------------------------------------------------------------------

constructor TG2PatchSettings.Create;
begin
  inherited;

  init;
end;

constructor TG2PatchSettings.CopyCreate( aSource: IG2PatchSettings);
var Chunk : TPatchChunk;
    MemStream : TMemoryStream;
begin
  inherited Create;

  MemStream := TMemoryStream.Create;
  Chunk := TPatchChunk.Create( MemStream);
  try
    aSource.Write( Chunk);
    Chunk.WriteChunk( C_PATCH_DESCR);

    MemStream.Position := 0;

    Chunk.ReadChunk;
    Read( Chunk);
  finally
    MemStream.Free;
  end;
end;

procedure TG2PatchSettings.Init;
begin
  FUnknown1 := 0;
  FUnknown2 := 0;
  FUnknown3 := 0;
  FUnknown4 := 0;
  FUnknown5 := 0;
  FUnknown6 := 0;
  FUnknown7 := 0;

  FUnknown8         := 0;
  FVoiceCount       := 1;
  FBarPosition      := 600;
  FUnknown9         := 2;
  FRedVisible       := 1;
  FBlueVisible      := 1;
  FYellowVisible    := 1;
  FOrangeVisible    := 1;
  FGreenVisible     := 1;
  FPurpleVisible    := 1;
  FWhiteVisible     := 1;
  FMonoPoly         := 1;
  FActiveVariation  := 0;
  FCategory         := 0;
  FUnknown10        := 0;
end;

procedure TG2PatchSettings.Read( aChunk : TPatchChunk);
begin
  // FUnknown1        : TArray7;
  // FUnknown2        : TBits5;
  // FVoiceCount      : TBits5;
  // FBarPosition     : TBits14;
  // FUnknown3        : TBits3;
  // FRedVisible      : TBits1;
  // FBlueVisible     : TBits1;
  // FYellowVisible   : TBits1;
  // FOrangeVisible   : TBits1;
  // FGreenVisible    : TBits1;
  // FPurpleVisible   : TBits1;
  // FWhiteVisible    : TBits1;
  // FMonoPoly        : TBits2;
  // FActiveVariation : Byte;
  // FCategory        : Byte;
  FUnknown1        := aChunk.ReadBits( 8);
  FUnknown2        := aChunk.ReadBits( 8);
  FUnknown3        := aChunk.ReadBits( 8);
  FUnknown4        := aChunk.ReadBits( 8);
  FUnknown5        := aChunk.ReadBits( 8);
  FUnknown6        := aChunk.ReadBits( 8);
  FUnknown7        := aChunk.ReadBits( 8);
  FUnknown8        := aChunk.ReadBits( 5);
  FVoiceCount      := aChunk.ReadBits( 5);
  FBarPosition     := aChunk.ReadBits( 14);
  FUnknown9        := aChunk.ReadBits( 3);
  FRedVisible      := aChunk.ReadBits( 1);
  FBlueVisible     := aChunk.ReadBits( 1);
  FYellowVisible   := aChunk.ReadBits( 1);
  FOrangeVisible   := aChunk.ReadBits( 1);
  FGreenVisible    := aChunk.ReadBits( 1);
  FPurpleVisible   := aChunk.ReadBits( 1);
  FWhiteVisible    := aChunk.ReadBits( 1);
  FMonoPoly        := aChunk.ReadBits( 2);
  FActiveVariation := aChunk.ReadBits( 8);
  FCategory        := aChunk.ReadBits( 8);
  FUnknown10       := aChunk.ReadBits( 12); // To byte align again
end;

procedure TG2PatchSettings.Write( aChunk : TPatchChunk);
begin
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 8);
  aChunk.WriteBits(0, 5);
  aChunk.WriteBits(FVoiceCount, 5);
  aChunk.WriteBits(FBarPosition, 14);
  aChunk.WriteBits(FUnknown9, 3);
  aChunk.WriteBits(FRedVisible, 1);
  aChunk.WriteBits(FBlueVisible, 1);
  aChunk.WriteBits(FYellowVisible, 1);
  aChunk.WriteBits(FOrangeVisible, 1);
  aChunk.WriteBits(FGreenVisible, 1);
  aChunk.WriteBits(FPurpleVisible, 1);
  aChunk.WriteBits(FWhiteVisible, 1);
  aChunk.WriteBits(FMonoPoly, 2);
  aChunk.WriteBits(FActiveVariation, 8);
  aChunk.WriteBits(FCategory, 8);
  aChunk.WriteBits(FUnknown10, 12); // To byte align again
end;

procedure TG2PatchSettings.WriteChunk( aStream: TStream);
var Chunk : TPatchChunk;
begin
  Chunk := TPatchChunk.Create( aStream);
  try
    Write( Chunk);
    Chunk.WriteChunk( C_PATCH_DESCR);
    Chunk.Flush;
  finally
    Chunk.Free;
  end;
end;

procedure TG2PatchSettings.SetBarPosition(const aValue: word);
begin
  FBarPosition := aValue;
end;

procedure TG2PatchSettings.SetBlueVisible(const aValue: boolean);
begin
  FBlueVisible := BoolToByte( aValue);
end;

procedure TG2PatchSettings.SetCategory(const aValue: byte);
begin
  FCategory := aValue;
end;

procedure TG2PatchSettings.SetGreenVisible(const aValue: boolean);
begin
  FGreenVisible := BoolToByte( aValue);
end;

procedure TG2PatchSettings.SetPurpleVisible(const aValue: boolean);
begin
  FPurpleVisible := BoolToByte( aValue);
end;

procedure TG2PatchSettings.SetRedVisible(const aValue: boolean);
begin
  FRedVisible := BoolToByte( aValue);
end;

function TG2PatchSettings.GetOrangeVisible: boolean;
begin
  Result := FOrangeVisible = 1;
end;

procedure TG2PatchSettings.SetOrangeVisible(const aValue: boolean);
begin
  FOrangeVisible := BoolToByte( aValue);
end;

procedure TG2PatchSettings.SetVoiceCount( const aValue : byte);
begin
  FVoiceCount := aValue;
end;

function TG2PatchSettings.GetVoiceCount : byte;
begin
  Result := FVoiceCount;
end;

procedure TG2PatchSettings.SetVoiceMode( const aValue : byte);
begin
  FMonoPoly := aValue;
end;

procedure TG2PatchSettings.SetWhiteVisible(const aValue: boolean);
begin
  FWhiteVisible := BoolToByte( aValue);
end;

procedure TG2PatchSettings.SetYellowVisible(const aValue: boolean);
begin
  FYellowVisible := BoolToByte( aValue);
end;

function TG2PatchSettings.GetVoiceMode : byte;
begin
  Result := FMonoPoly;
end;

function TG2PatchSettings.GetWhiteVisible: boolean;
begin
  Result := FWhiteVisible = 1;
end;

function TG2PatchSettings.GetYellowVisible: boolean;
begin
  Result := FYellowVisible = 1;
end;

procedure TG2PatchSettings.SetActiveVariation( const aValue : byte);
begin
  FActiveVariation := aValue;
end;

function TG2PatchSettings.GetActiveVariation : byte;
begin
  Result := FActiveVariation
end;

function TG2PatchSettings.GetBarPosition: word;
begin
  Result := FBarPosition;
end;

function TG2PatchSettings.GetBlueVisible: boolean;
begin
  Result := FBlueVisible = 1;
end;

function TG2PatchSettings.GetCategory: byte;
begin
  Result := FCategory;
end;

function TG2PatchSettings.GetGreenVisible: boolean;
begin
  Result := FGreenVisible = 1;
end;

function TG2PatchSettings.GetPurpleVisible: boolean;
begin
  Result := FPurpleVisible = 1;
end;

function TG2PatchSettings.GetRedVisible: boolean;
begin
  Result := FRedVisible = 1;
end;

end.
