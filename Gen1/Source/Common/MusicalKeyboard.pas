unit MusicalKeyboard;

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

{$I includes\delphi_version.inc}

interface

uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, System.SysUtils, System.Classes, Vcl.Controls, VCL.Graphics;
{$ELSE}
  Windows, SysUtils, Classes, Controls, Graphics;
{$ENDIF}

type
  TKeyboardOrientation = ( kbVertical, kbHorizontal);

  TOnNoteOnEvent = procedure( Sender : TObject; aOcatve, aKey: integer) of object;
  TOnNoteOffEvent = procedure( Sender : TObject; aOcatve, aKey: integer) of object;
  TOnSetRangeEvent = procedure( Sender : TObject; aLowKey, aHighKey: integer) of object;

  TKeyboard = class(TGraphicControl)
  private
    { Private declarations }
    FOrientation : TKeyboardOrientation;
    FOctaves : integer;
    FBaseOctave : integer;
    FStartOctave, FStartKey,
    FEndOctave, FEndKey,
    FOctave, FKey : integer;
    FWhiteKeyWidth,
    FWhiteKeyLength,
    FBlackKeyWidth,
    FBlackKeyLength : integer;
    FHighLightColorBlackKeys : TColor;
    FHighLightColorWhiteKeys : TColor;

    FOnNoteOn : TOnNoteOnEvent;
    FOnNoteOff : TOnNoteOffEvent;
    FOnSetRange : TOnSetRangeEvent;

    procedure CalcKeyDimensions;
    function BlackKeyHitTest( o, k, x, y : integer): boolean;
    function BlackKeyRect( o, k : integer): TRect;
    function GetKeyNo( k : integer; black : boolean): integer;
    procedure GetOctaveKeyFromPoint( x, y : integer; var aOctave, aKey : integer);
    function GetKeyIsSelected( o, k : integer): boolean;
    function GetKey: integer;
    function GetLowKey : integer;
    procedure SetLowKey( aValue : integer);
    function GetHighKey : integer;
    procedure SetHighKey( aValue : integer);
  protected
    { Protected declarations }
    procedure SetOrientation( aValue : TKeyboardOrientation);
    procedure SetOctaves( aValue : integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    procedure paint; override;
  published
    { Published declarations }
    constructor Create( AOwner : TComponent); override;
    property Orientation : TKeyboardOrientation read FOrientation write SetOrientation;
    property Octaves : integer read FOctaves write SetOctaves;
    property BaseOctave : integer read FBaseOctave write FBaseOctave;
    property LowKey : integer read GetLowKey write SetLowKey;
    property HighKey : integer read GetHighKey write SetHighKey;
    property HighLightColorBlackKeys : TColor read FHighLightColorBlackKeys write FHighLightColorBlackKeys;
    property HighLightColorWhiteKeys : TColor read FHighLightColorWhiteKeys write FHighLightColorWhiteKeys;

    property OnNoteOn : TOnNoteOnEvent read FOnNoteOn write FOnNoteOn;
    property OnNoteOff : TOnNoteOffEvent read FOnNoteOff write FOnNoteOff;
    property OnSetRange : TOnSetRangeEvent read FOnSetRange write FOnSetRange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('NM G2', [TKeyboard]);
end;

{ TKeyboard }

procedure TKeyboard.CalcKeyDimensions;
begin
  if FOctaves = 0 then
    exit;

  if FOrientation = kbHorizontal then begin
    FWhiteKeyWidth := ClientWidth div (7*FOctaves);
    FWhiteKeyLength := ClientHeight;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
  end else begin
    FWhiteKeyWidth := ClientHeight div (7*FOctaves);
    FWhiteKeyLength := ClientWidth;
    FBlackKeyLength := round( FWhiteKeyLength * 0.6);
  end;
  FBlackKeyWidth := round(FWhiteKeyWidth * 0.7);
end;

function TKeyboard.BlackKeyRect( o, k : integer): TRect;
begin
  case FOrientation of
    kbVertical :
      begin
        Result.Top := ClientHeight - FWhiteKeyWidth - FBlackKeyWidth div 2
                    - o * (FWhiteKeyWidth * 7)
                    - k * FWhiteKeyWidth;
        Result.Left := 0;
        Result.Bottom := ClientHeight - FWhiteKeyWidth + FBlackKeyWidth div 2
                    - o * (FWhiteKeyWidth * 7)
                    - k * FWhiteKeyWidth;
        Result.Right := FBlackKeyLength;
      end;
    kbHorizontal :
      begin
        Result.Top := 0;
        Result.Left := FWhiteKeyWidth - FBlackKeyWidth div 2
                     + o * (FWhiteKeyWidth * 7)
                     + k * FWhiteKeyWidth;
        Result.Bottom := FBlackKeyLength;
        Result.Right := FWhiteKeyWidth + FBlackKeyWidth div 2
                      + o * (FWhiteKeyWidth * 7)
                      + k * FWhiteKeyWidth;
      end;
  end;
end;

function TKeyboard.BlackKeyHitTest( o, k, x, y : integer): boolean;
var Rect : TRect;
begin
  Rect := BlackKeyRect( o, k);
  Result := PtInRect( Rect, Point(x, y));
end;

function TKeyboard.GetKeyNo( k : integer; black : boolean): integer;
begin
  if black then begin
    case k of
    0 : Result := 1;
    1 : Result := 3;
    3 : Result := 6;
    4 : Result := 8;
    5 : Result := 10;
    end;
  end else
    case k of
    0 : Result := 0;
    1 : Result := 2;
    2 : Result := 4;
    3 : Result := 5;
    4 : Result := 7;
    5 : Result := 9;
    6 : Result := 11;
    end;
end;

function TKeyboard.GetKey: integer;
begin
  Result := FOctave * 12 + FKey
end;

function TKeyboard.GetLowKey : integer;
begin
  Result := FStartOctave * 12 + FStartKey;
end;

procedure TKeyboard.SetLowKey( aValue : integer);
begin
  FStartOctave := aValue div 12;
  FStartKey := aValue mod 12;
  Invalidate;
end;

function TKeyboard.GetHighKey : integer;
begin
  Result := FEndOctave * 12 + FEndKey
end;

procedure TKeyboard.SetHighKey( aValue : integer);
begin
  FEndOctave := aValue div 12;
  FEndKey := aValue mod 12;
  Invalidate;
end;

function TKeyboard.GetKeyIsSelected( o, k : integer): boolean;
var StartKeyNo, EndKeyNo, KeyNo : integer;
begin
  Result := False;
  if (FStartOctave > -1) and (FStartKey > -1) then begin

    StartKeyNo := GetLowKey;
    EndKeyNo := GetHighKey;
    KeyNo := o * 12 + k;

    Result := (KeyNo >= StartKeyNo) and ( KeyNo <= EndKeyNo);
  end;
end;

constructor TKeyboard.Create(AOwner: TComponent);
begin
  inherited;

  FOctaves := 2;
  FOrientation := kbHorizontal;
  FBaseOctave := 0;
end;

procedure TKeyboard.paint;
var i, j : integer;
    Rect : TRect;
    Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Pixelformat := pf24bit;
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    if assigned(Parent) then
      Bitmap.Canvas.Brush.Color := Parent.Brush.Color
    else
      Bitmap.Canvas.Brush.Color := clBtnFace;
    Bitmap.Canvas.FillRect(ClientRect);

    CalcKeyDimensions;

    if FOrientation = kbHorizontal then begin
      Rect.Top := 0;
      Rect.Left := 0;
      Rect.Bottom := ClientHeight;
      Rect.Right := FWhiteKeyWidth;
    end else begin
      Rect.Top := ClientHeight - FWhiteKeyWidth;
      Rect.Left := 0;
      Rect.Bottom := ClientHeight;
      Rect.Right := ClientWidth;
    end;

    Bitmap.Canvas.Pen.Color := clBlack;

    for i := 0 to FOctaves - 1 do begin
      for j := 0 to 6 do begin
        if GetKeyIsSelected( i + FBaseOctave, GetKeyNo( j, False)) then
          Bitmap.Canvas.Brush.Color := FHighLightColorWhiteKeys
        else
          Bitmap.Canvas.Brush.Color := clWhite;
        Bitmap.Canvas.FillRect( Rect);
        Bitmap.Canvas.Brush.Color := clBlack;
        Bitmap.Canvas.FrameRect(Rect);

        if FOrientation = kbHorizontal then begin
          Rect.Left := Rect.Right;
          Rect.Right := Rect.Right + FWhiteKeyWidth;
        end else begin
          Rect.Bottom := Rect.Top;
          Rect.Top := Rect.Top - FWhiteKeyWidth;
        end;
      end;
    end;

    Bitmap.Canvas.Pen.Color := clBlack;

    for i := 0 to FOctaves - 1 do begin
      for j := 0 to 6 do begin
        if GetKeyIsSelected( i + FBaseOctave, GetKeyNo( j, True)) then
          Bitmap.Canvas.Brush.Color := FHighLightColorBlackKeys
        else
          Bitmap.Canvas.Brush.Color := clBlack;

        Rect := BlackKeyRect( i, j);
        if (j <> 2) and (j<>6) then
          Bitmap.Canvas.FillRect( Rect);
      end;
    end;

    Canvas.Draw( 0, 0, Bitmap);

  finally
    BitMap.Free;
  end;

//  inherited;
end;

procedure TKeyboard.GetOctaveKeyFromPoint( x, y : integer; var aOctave, aKey : integer);
var i : integer;
begin
  CalcKeyDimensions;
  case FOrientation of
    kbVertical:
      begin;
        aOctave := (ClientHeight - Y) div (FWhiteKeyWidth * 7);
        i := 0;
        while (i < 6) and (not BlackKeyHitTest( aOctave, i, X, Y)) do
          inc(i);

        if (i < 6) and (i<>2) then
          aKey := GetKeyNo(i, True)
        else
          aKey := GetKeyNo(((ClientHeight - Y) mod (FWhiteKeyWidth * 7)) div FWhiteKeyWidth, False);

        aOctave := aOctave + FBaseOctave;
      end;
    kbHorizontal:
      begin
        aOctave := X div (FWhiteKeyWidth * 7);
        i := 0;
        while (i < 6) and (not BlackKeyHitTest( aOctave, i, X, Y)) do
          inc(i);

        if (i < 6) and (i<>2) then
          aKey := GetKeyNo(i, True)
        else
          aKey := GetKeyNo((X mod (FWhiteKeyWidth * 7)) div FWhiteKeyWidth, False);

        aOctave := aOctave + FBaseOctave;
      end;
  end;
end;

procedure TKeyboard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    GetOctaveKeyFromPoint( X, Y, FOctave, FKey);
    if ssShift in Shift then begin
      FStartOctave := FOctave;
      FStartKey := FKey;
    end else begin
      if assigned(FOnNoteOn) then
        FOnNoteOn( self, FOctave, FKey);
    end;
  end;
end;

procedure TKeyboard.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    if ssShift in Shift then begin
      GetOctaveKeyFromPoint( X, Y, FEndOctave, FEndKey);

      FOctave := FEndOctave;
      FKey := FendKey;

      Paint;
    end;
  end;
end;

procedure TKeyboard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetOctaveKeyFromPoint( X, Y, FOctave, FKey);
  Paint;

  if ssShift in Shift then begin
   FEndOctave := FOctave;
   FEndKey := FKey;

    if assigned(FOnSetRange) then
      FOnSetRange( self, FStartOctave * 12 + FStartKey, FEndOctave * 12 + FEndKey);
  end else
    if assigned(FOnNoteOff) then
      FOnNoteOff( self, FOctave, FKey);
end;

procedure TKeyboard.SetOctaves(aValue: integer);
begin
  FOctaves := aValue;
  Invalidate;
end;

procedure TKeyboard.SetOrientation(aValue: TKeyboardOrientation);
begin
  FOrientation := aValue;
  Invalidate;
end;

end.
