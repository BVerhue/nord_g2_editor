unit graph_util_vcl;

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
//  Some graphical support functions
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses
{$IFDEF FPC}
  LclIntf, intfgraphics, FPImage,
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinAPI.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}
{$IFDEF G2_VER220_up}
  WinAPI.Messages, System.SysUtils, System.Classes, System.Variants, System.Math,
  VCL.Forms, VCL.Graphics, VCL.StdCtrls, VCL.ExtCtrls, VCL.Controls,
{$ELSE}
  Forms, Messages, SysUtils, Classes, Variants, Graphics, StdCtrls, ExtCtrls,
  Math, Controls,
{$ENDIF}
  Fastbitmap, g2_types;

type
{$IFDEF FPC}
  TPointFloat = Record
    X : single;
    Y : single;
  end;
{$ELSE}
  TRGBTripleArray = array[0..MaxPixelCount-1] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;
{$ENDIF}

// TODO : Check what's safe for the vst

{$IFDEF FPC}
procedure AlphaBlendPixel(aIntfImg : TLazIntfImage; X, Y : integer ; R, G, B : Word ; ARatio : Real);
{$ELSE}
procedure AlphaBlendPixel(ABitmap : TBitmap ; X, Y : integer ; R, G, B : byte ; ARatio : Real);
{$ENDIF}
procedure DrawWuLine(ABitmap : TBitmap ; Point1, Point2 : TPoint ; AColor : TColor);
//procedure DrawWuCircle( aBitmap : TBitmap; x, y, r : integer; AColor : TColor);
procedure DrawDisk( Bitmap: TBitmap; CenterX, CenterY, Radius, Feather: single; aColor : TColor; aBrushStyle : TBrushStyle);
procedure DrawBevel( aCanvas : TCanvas; aRect : TRect; aBevelType : TBevelCut);
procedure DrawArrowHead( aPoint : TPoint; aSize : integer; aOrientation : integer; aCanvas : TCanvas);
procedure DrawIcon( aCanvas : TCanvas; aBoundsRect, aIconRect : TRect; aIcon : TIconType);

procedure DrawRect( aCanvas : TCanvas; aRect : TRect);
procedure TextCenter( aCanvas : TCanvas; aRect : TRect; aText : string);
procedure DrawCenter( aCanvas : TCanvas; aRect : TRect; aBitmap : TBitmap);

function  CompareBitmap( Bitmap1, Bitmap2 : TBitmap): boolean;

//procedure CreateFlatPen(Pen: TPen; Width: Integer; Color: TColor);

procedure ShrinkRect(var aRect : TRect; aAmount : integer);
procedure SquareRect(var aRect : TRect);
function  PointInRect( X, Y : integer; Rect : TRect): boolean;
function  RectOverlap( Rect1, Rect2 : TRect): boolean;
function  AddRect( Rect, RefRect : TRect): TRect;
function  SubRect( Rect, RefRect : TRect): TRect;
function  MakeRect( aLeft, aTop, aWidth, aHeight: integer): TRect;

function  Darker(c : TColor; f : byte): TColor;
function  Lighter(c : TColor; f : byte): TColor;

//function  GetG2Color( G2Color : byte): TColor;
function  GetUnitNormal(const pt1, pt2 : TPointFloat): TPointFloat;

implementation

// ==== Rectangle functions ====================================================

function PointInRect( X, Y : integer; Rect : TRect): boolean;
begin
  Result := PtInRect( Rect, Point(X, Y))
end;

function RectOverlap( Rect1, Rect2 : TRect): boolean;
var Rect : TRect;
begin
  Result := IntersectRect( Rect, Rect1, Rect2);
end;

function CenterRect( Rect, ClientRect : TRect): TRect;
begin
  Result.Left := Rect.Left + (Rect.Right - Rect.Left) div 2 - (ClientRect.Right - ClientRect.Left) div 2;
  Result.Top := Rect.Top + (Rect.Bottom - Rect.Top) div 2 - (ClientRect.Bottom - ClientRect.Top) div 2;
  Result.Right := Result.Left + (Rect.Right - Rect.Left);
  Result.Bottom := Result.Top + (Rect.Bottom - Rect.Top);
end;

procedure ShrinkRect(var aRect : TRect; aAmount : integer);
begin
  aRect.Left   := aRect.Left   + aAmount;
  aRect.Right  := aRect.Right  - aAmount;
  aRect.Top    := aRect.Top    + aAmount;
  aRect.Bottom := aRect.Bottom - aAmount;
end;

procedure SquareRect(var aRect : TRect);
var Width, Height, Side : integer;
begin
  Width := aRect.Right - aRect.Left;
  Height := aRect.Bottom - aRect.Top;

  Side := min( Width, Height);

  aRect.Right  := aRect.Left + (Width + Side) div 2;
  aRect.Left   := aRect.Left + (Width - Side) div 2;
  aRect.Bottom := aRect.Top  + (Height + Side) div 2;
  aRect.Top    := aRect.Top  + (Height - Side) div 2;
end;

function AddRect( Rect, RefRect : TRect): TRect;
begin
  Result.Left   := Rect.Left   + RefRect.Left;
  Result.Top    := Rect.Top    + RefRect.Top;
  Result.Right  := Rect.Right  + RefRect.Left;
  Result.Bottom := Rect.Bottom + RefRect.Top;
end;

function SubRect( Rect, RefRect : TRect): TRect;
begin
  Result.Left   := Rect.Left   - RefRect.Left;
  Result.Top    := Rect.Top    - RefRect.Top;
  Result.Right  := Rect.Right  - RefRect.Left;
  Result.Bottom := Rect.Bottom - RefRect.Top;
end;

function MakeRect( aLeft, aTop, aWidth, aHeight: integer): TRect;
begin
  Result.Left := aLeft;
  Result.Top := aTop;
  Result.Right := aLeft + aWidth;
  Result.Bottom := aTop + aHeight;
end;

// ==== Some graphic utility functions =========================================

// Drawing an Antiailiased Ellipse or Circle using Delphi5 Graphics

// http://www.experts-exchange.com/Programming/Languages/Pascal/Delphi/Graphics/Q_23502480.html

// blend a pixel with the current colour
{$IFDEF FPC}
procedure AlphaBlendPixel(aIntfImg : TLazIntfImage; X, Y : integer ; R, G, B : Word ; ARatio : Real);
{$ELSE}
procedure AlphaBlendPixel(ABitmap : TBitmap ; X, Y : integer ; R, G, B : byte ; ARatio : Real);
{$ENDIF}
Var
{$IFDEF FPC}
  CurColor: TFPColor;
{$ELSE}
  LBack, LNew : TRGBTriple;
  LScan : PRGBTripleArray;
{$ENDIF}
  LMinusRatio : Real;
begin

  LMinusRatio := 1 - ARatio;

{$IFDEF FPC}
  if (X < 0) or (X > aIntfImg.Width - 1) or (Y < 0) or (Y > aIntfImg.Height - 1) then
    Exit; // clipping

  CurColor := aIntfImg.Colors[ x, y];
  CurColor.Blue  := round( B * ARatio + CurColor.Blue * LMinusRatio);
  CurColor.Green := round( G * ARatio + CurColor.Green * LMinusRatio);
  CurColor.Red   := round( R * ARatio + CurColor.Red * LMinusRatio);
  aIntfImg.Colors[x,y] := CurColor;
{$ELSE}
  if (X < 0) or (X > ABitmap.Width - 1) or (Y < 0) or (Y > ABitmap.Height - 1) then
    Exit; // clipping

  LScan := ABitmap.Scanline[Y];
  LBack := LScan[X];
  LNew.rgbtBlue  := round(B * ARatio + LBack.rgbtBlue  * LMinusRatio);
  LNew.rgbtGreen := round(G * ARatio + LBack.rgbtGreen * LMinusRatio);
  LNew.rgbtRed   := round(R * ARatio + LBack.rgbtRed   * LMinusRatio);
  LScan[X] := LNew;
{$ENDIF}

end;

procedure DrawWuLine(ABitmap : TBitmap ; Point1, Point2 : TPoint ; AColor : TColor);
var
  deltax, deltay, loop, start, finish : integer;
  dx, dy, dydx : single; // fractional parts
{$IFDEF FPC}
  LR, LG, LB : Word;
  CurColor: TFPColor;
  TempIntfImg: TLazIntfImage;
{$ELSE}
  LR, LG, LB : byte;
{$ENDIF}
  x1, x2, y1, y2 : integer;
begin
{$IFDEF FPC}
  TempIntfImg := TLazIntfImage.Create(0,0);
  try
    TempIntfImg.LoadFromBitmap( aBitmap.Handle, aBitmap.MaskHandle);
{$ENDIF}

    x1 := Point1.X; y1 := Point1.Y;
    x2 := Point2.X; y2 := Point2.Y;
    deltax := abs(x2 - x1); // Calculate deltax and deltay for initialisation
    deltay := abs(y2 - y1);

    if aColor < 0 then
      aColor := ColorToRGB(aColor);

{$IFDEF FPC}
    CurColor := TColorToFPColor(aColor);
    LR := CurColor.Red;
    LG := CurColor.Green;
    LB := CurColor.Blue;
{$ELSE}
    LR := (AColor and $000000FF);
    LG := (AColor and $0000FF00) shr 8;
    LB := (AColor and $00FF0000) shr 16;
{$ENDIF}

    if deltax > deltay then begin // horizontal or vertical
      if deltax = 0 then
        deltax := 1;

      if y2 > y1 then // determine rise and run
        dydx := -(deltay / deltax)
      else
        dydx := deltay / deltax;

      if x2 < x1 then begin
        start := x2; // right to left
        finish := x1;
        dy := y2;
      end else begin
        start := x1; // left to right
        finish := x2;
        dy := y1;
        dydx := -dydx; // inverse slope
      end;
      for loop := start to finish do begin
{$IFDEF FPC}
        AlphaBlendPixel( TempIntfImg, loop, trunc(dy), LR, LG, LB, 1 - frac(dy));
        AlphaBlendPixel( TempIntfImg, loop, trunc(dy) + 1, LR, LG, LB, frac(dy));
{$ELSE}
        AlphaBlendPixel(ABitmap, loop, trunc(dy), LR, LG, LB, 1 - frac(dy));
        AlphaBlendPixel(ABitmap, loop, trunc(dy) + 1, LR, LG, LB, frac(dy));
{$ENDIF}
        dy := dy + dydx; // next point
      end;
    end else begin
      if deltay = 0 then
        deltay := 1;

      if x2 > x1 then // determine rise and run
        dydx := -(deltax / deltay)
      else
        dydx := deltax / deltay;

      if y2 < y1 then begin
        start := y2; // right to left
        finish := y1;
        dx := x2;
      end else begin
        start := y1; // left to right
        finish := y2;
        dx := x1;
        dydx := -dydx; // inverse slope
      end;

      for loop := start to finish do begin
{$IFDEF FPC}
        AlphaBlendPixel( TempIntfImg, trunc(dx), loop, LR, LG, LB, 1 - frac(dx));
        AlphaBlendPixel( TempIntfImg, trunc(dx) + 1, loop, LR, LG, LB, frac(dx));
{$ELSE}
        AlphaBlendPixel(ABitmap, trunc(dx), loop, LR, LG, LB, 1 - frac(dx));
        AlphaBlendPixel(ABitmap, trunc(dx) + 1, loop, LR, LG, LB, frac(dx));
{$ENDIF}
        dx := dx + dydx; // next point
      end;
    end;
{$IFDEF FPC}
    aBitmap.LoadFromIntfImage(TempIntfImg);
  finally
    TempIntfImg.Free;
  end;
{$ENDIF}
end;

procedure DrawDisk( Bitmap: TBitmap; CenterX, CenterY, Radius, Feather: single; aColor : TColor; aBrushStyle : TBrushStyle);
// Draw a disk on Bitmap. Bitmap must be a 256 color (pf8bit)
// palette bitmap, and parts outside the disk will get palette
// index 0, parts inside will get palette index 255, and in the
// antialiased area (feather), the pixels will get values
// inbetween.
// ***Parameters***
// Bitmap:
//   The bitmap to draw on
// CenterX, CenterY:
//   The center of the disk (float precision). Note that [0, 0]
//   would be the center of the first pixel. To draw in the
//   exact middle of a 100x100 bitmap, use CenterX = 49.5 and
//   CenterY = 49.5
// Radius:
//   The radius of the drawn disk in pixels (float precision)
// Feather:
//   The feather area. Use 1 pixel for a 1-pixel antialiased
//   area. Pixel centers outside 'Radius + Feather / 2' become
//   0, pixel centers inside 'Radius - Feather/2' become 255.
//   Using a value of 0 will yield a bilevel image.
// Copyright (c) 2003 Nils Haeck M.Sc. www.simdesign.nl
var
  x, y: integer;
  LX, RX, LY, RY: integer;
  RPF2, RMF2: single;
  //P: PByteArray;
  SqY, SqDist: single;
  sqX: array of single;
{$IFDEF FPC}
  LG, LB, LR : word;
  TempIntfImg: TLazIntfImage;
  CurColor: TFPColor;
{$ELSE}
  LG, LB, LR : byte;
  LBack, LNew : TRGBTriple;
  LScan : PRGBTripleArray;
{$ENDIF}
  aRatio : Real;
  LMinusRatio : Real;
begin
{$IFDEF FPC}
  TempIntfImg := TLazIntfImage.Create(0,0);
  try
    TempIntfImg.LoadFromBitmap( Bitmap.Handle, Bitmap.MaskHandle);
{$ENDIF}

    if aColor < 0 then
      aColor := ColorToRGB(aColor);

{$IFDEF FPC}
    CurColor := TColorToFPColor(aColor);

    LR := CurColor.Red;
    LG := CurColor.Green;
    LB := CurColor.Blue;
{$ELSE}
    LR := (aColor and $000000FF);
    LG := (aColor and $0000FF00) shr 8;
    LB := (aColor and $00FF0000) shr 16;
{$ENDIF}

    // Determine some helpful values (singles)
    RPF2 := sqr(Radius + Feather/2);
    RMF2 := sqr(Radius - Feather/2);

    // Determine bounds:
    LX := Max(floor(CenterX - RPF2), 0);
    RX := Min(ceil (CenterX + RPF2), Bitmap.Width - 1);
    LY := Max(floor(CenterY - RPF2), 0);
    RY := Min(ceil (CenterY + RPF2), Bitmap.Height - 1);

    // Optimization run: find squares of X first
    SetLength(SqX, RX - LX + 1);
    for x := LX to RX do
      SqX[x - LX] := sqr(x - CenterX);

    // Loop through Y values
    for y := LY to RY do begin
      //P := Bitmap.Scanline[y];
{$IFDEF FPC}
{$ELSE}
      LScan := Bitmap.Scanline[Y];
{$ENDIF}

      SqY := Sqr(y - CenterY);
      // Loop through X values
      for x := LX to RX do begin

        // determine squared distance from center for this pixel
        SqDist := SqY + SqX[x - LX];

        // inside inner circle? Most often..
        if sqdist < RMF2 then begin
          // inside the inner circle.. just give the scanline the
          // new color
          //P[x] := 255
          if aBrushStyle = bsSolid then begin
{$IFDEF FPC}
            CurColor.Red := LR;
            CurColor.Green := LG;
            CurColor.Blue := LB;
            TempIntfImg.Colors[x,y] := CurColor;
{$ELSE}
            LNew.rgbtBlue  := LB;
            LNew.rgbtGreen := LG;
            LNew.rgbtRed   := LR;
            LScan[x] := LNew;
{$ENDIF}

          end;
        end else begin
          // inside outer circle?
          if sqdist < RPF2 then begin
            // We are inbetween the inner and outer bound, now mix the color

            //Fact := round(((Radius - sqrt(sqdist)) * 2 / Feather) * 127.5 + 127.5);
            if aBrushStyle = bsSolid then
              ARatio :=  ((Radius - sqrt(sqdist)) * 2 / Feather) * 0.5 + 0.5 // disk
            else
              ARatio :=  1 - abs((Radius - sqrt(sqdist)) * 2 / Feather); // line

            // just in case limit to [0, 255]
            //P[x] := Max(0, Min(Fact, 255));

            LMinusRatio := 1 - ARatio;
{$IFDEF FPC}
            CurColor := TempIntfImg.Colors[ x, y];
            CurColor.Blue  := round( LB * ARatio + CurColor.Blue * LMinusRatio);
            CurColor.Green := round( LG * ARatio + CurColor.Green * LMinusRatio);
            CurColor.Red   := round( LR * ARatio + CurColor.Red * LMinusRatio);
            TempIntfImg.Colors[x,y] := CurColor;
{$ELSE}
            LBack := LScan[X];
            LNew.rgbtBlue  := round( LB * ARatio + LBack.rgbtBlue  * LMinusRatio);
            LNew.rgbtGreen := round( LG * ARatio + LBack.rgbtGreen * LMinusRatio);
            LNew.rgbtRed   := round( LR * ARatio + LBack.rgbtRed   * LMinusRatio);
            LScan[X] := LNew;
{$ENDIF}

          end else begin
            //P[x] := 0;
          end;
        end;
      end;
    end;
{$IFDEF FPC}
    Bitmap.LoadFromIntfImage(TempIntfImg);
  finally
     TempIntfImg.Free;
  end;
{$ENDIF}
end;

function CompareBitmap( Bitmap1, Bitmap2 : TBitmap): boolean;
var i, j : integer;
    LScan1, LScan2 : PRGBTripleArray;
begin
  if (not assigned(Bitmap1)) or (not assigned(Bitmap2)) then begin
    Result := False;
    exit;
  end;

  // Only 24bit valid!
  if Bitmap1.Pixelformat <> pf24bit then
    raise Exception.Create('Only 24bit bmp valid.');

  if (Bitmap1.Pixelformat = Bitmap2.PixelFormat) and
     (Bitmap1.Width = Bitmap2.Width) and
     (Bitmap1.Height = Bitmap2.Height) then begin
    Result := True;
    for i := 0 to Bitmap1.Height - 1 do begin
      LScan1 := Bitmap1.ScanLine[i];
      LScan2 := Bitmap2.ScanLine[i];
      for j := 0 to BitMap1.Width do begin
        if not((LScan1[j].rgbtBlue = LScan2[j].rgbtBlue) and
               (LScan1[j].rgbtGreen = LScan2[j].rgbtGreen) and
               (LScan1[j].rgbtRed = LScan2[j].rgbtRed)) then begin
          Result := False;
          exit;
        end;
      end;
    end;
  end else
    Result := false;
end;

procedure DrawBevel( aCanvas : TCanvas; aRect : TRect; aBevelType : TBevelCut);
begin
  aCanvas.Pen.Width := 1;

  case aBevelType of
    bvRaised  : aCanvas.Pen.Color := Lighter(aCanvas.Brush.Color, 50);
    bvNone    : aCanvas.Pen.Color := aCanvas.Brush.Color;
    bvLowered : aCanvas.Pen.Color := Darker(aCanvas.Brush.Color, 50);
  end;

  aCanvas.MoveTo( aRect.Left, aRect.Bottom - 1);
  aCanvas.LineTo( aRect.Left, aRect.Top);
  aCanvas.LineTo( aRect.Right - 1, aRect.Top);

  case aBevelType of
    bvRaised  : aCanvas.Pen.Color := Darker(aCanvas.Brush.Color, 50);
    bvNone    : aCanvas.Pen.Color := aCanvas.Brush.Color;
    bvLowered : aCanvas.Pen.Color := Lighter(aCanvas.Brush.Color, 50);
  end;

  aCanvas.LineTo( aRect.Right - 1, aRect.Bottom - 1);
  aCanvas.LineTo( aRect.Left, aRect.Bottom - 1);
end;

procedure DrawRect( aCanvas : TCanvas; aRect : TRect);
begin
  aCanvas.MoveTo( aRect.Left,  aRect.Bottom - 1);
  aCanvas.LineTo( aRect.Left,  aRect.Top);
  aCanvas.LineTo( aRect.Right - 1, aRect.Top);
  aCanvas.LineTo( aRect.Right - 1, aRect.Bottom - 1);
  aCanvas.LineTo( aRect.Left,  aRect.Bottom - 1);
end;

{procedure CreateFlatPen(Pen: TPen; Width: Integer; Color: TColor);
var
  HP: HPen;
  LB: TLOGBRUSH;
begin
  LB.lbStyle := BS_SOLID;
  LB.lbColor := ColorToRGB(Color);
  LB.lbHatch := 0;
  HP := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT, Width, LB, 0, nil);
  if HP = 0 then
  begin
    Pen.Width := Width;
    Pen.Color := Color;
  end
  else
    Pen.Handle := HP;
end;}

function GetUnitNormal(const pt1, pt2 : TPointFloat): TPointFloat;
var
  dx, dy, f: single;
begin
  dx := (pt2.X - pt1.X);
  dy := (pt2.Y - pt1.Y);

  if (dx = 0) and (dy = 0) then begin
    result.x := 0;
    result.y := 0;
  end else begin
    f := 1 / Hypot(dx, dy);
    dx := dx * f;
    dy := dy * f;
  end;
  Result.X := dy;  //ie perpendicular to
  Result.Y := -dx; //the unit vector
end;

procedure TextCenter( aCanvas : TCanvas; aRect : TRect; aText : string);
begin
  aCanvas.TextRect( aRect,
                    aRect.Left + (aRect.Right - aRect.Left - aCanvas.TextWidth(aText)) div 2,
                    aRect.Top + (aRect.Bottom - aRect.Top - aCanvas.TextHeight(aText)) div 2,
                    aText);
end;

procedure DrawCenter( aCanvas : TCanvas; aRect : TRect; aBitmap : TBitmap);
begin
  aCanvas.Draw( aRect.Left + (aRect.Right - aRect.Left) div 2 - aBitmap.Width div 2,
                aRect.Top  + (aRect.Bottom - aRect.Top) div 2 - aBitmap.Height div 2,
                aBitmap);

end;


procedure DrawArrowHead( aPoint : TPoint; aSize : integer; aOrientation : integer; aCanvas : TCanvas);
var pts : array of TPoint;
begin
  SetLength( pts, 3);
  case aOrientation of
  0 : begin // Up
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y + aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  1 : begin // Down
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y - aSize;
      end;
  2 : begin // Left
        pts[0] := aPoint;
        pts[1].X := aPoint.X + aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X + aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  3 : begin // Right
        pts[0] := aPoint;
        pts[1].X := aPoint.X - aSize; pts[1].Y := aPoint.Y - aSize;
        pts[2].X := aPoint.X - aSize; pts[2].Y := aPoint.Y + aSize;
      end;
  end;

  aCanvas.Polygon(pts);
end;

procedure DrawIcon( aCanvas : TCanvas; aBoundsRect, aIconRect : TRect; aIcon : TIconType);
var pts : array of TPoint;
    hx, hy, offsx, offsy, Width, Height : integer;
    Bitmap : TBitmap;
    i : integer;
    ClientRect : TRect;

  procedure DrawPolygon;
  var i : integer;
  begin
    Bitmap.Canvas.Polygon(pts);
    for i := 0 to Length(pts) - 2 do
      DrawWuLine(Bitmap, pts[i], pts[i + 1], Bitmap.Canvas.Pen.Color);
    DrawWuLine(Bitmap, pts[ Length(pts) - 1], pts[0], Bitmap.Canvas.Pen.Color);

    {DrawWuLine(Bitmap, pts[0], pts[1], Bitmap.Canvas.Pen.Color);
    DrawWuLine(Bitmap, pts[1], pts[2], Bitmap.Canvas.Pen.Color);
    DrawWuLine(Bitmap, pts[2], pts[0], Bitmap.Canvas.Pen.Color);}
  end;

  procedure DrawPolygonFBM;
  var Fastbitmap : TFastbitmap;
  begin
    Fastbitmap := TFastbitmap.Create;
    try
      FastBitmap.CopyFromBitmap(Bitmap);
      FastBitmap.DrawPolygon(pts, Bitmap.Canvas.Brush.Color);
      FastBitmap.CopyToBitmap(Bitmap);
    finally
      Fastbitmap.Free;
    end;
  end;

  procedure DrawLine;
  var p : integer;
  begin
    p := 1;
    while (p < Length(pts)) do begin
      DrawWuLine(Bitmap, pts[p-1], pts[p], Bitmap.Canvas.Pen.Color);
      inc(p);
    end;
  end;

begin
  BitMap := TBitmap.Create;
  try
    BitMap.PixelFormat := pf24Bit;
    BitMap.Width  := aBoundsRect.Right - aBoundsRect.Left;
    BitMap.Height := aBoundsRect.Bottom - aBoundsRect.Top;

    ClientRect.Left := 0;
    ClientRect.Top := 0;
    ClientRect.Right := BitMap.Width;
    ClientRect.Bottom := BitMap.Height;

    Width := aIconRect.Right - aIconRect.Left;
    Height := aIconRect.Bottom - aIconRect.Top;
    hx := Width div 2;
    hy := Height div 2;
    offsx := (Bitmap.Width - Width) div 2;
    offsy := (Bitmap.Height - Height) div 2;

    Bitmap.Canvas.Brush.Color := aCanvas.Brush.Color;
    Bitmap.Canvas.Pen.Color := aCanvas.Pen.Color;

    Bitmap.Canvas.CopyRect( ClientRect, aCanvas, aBoundsRect);

    case aIcon of
    itUp : begin // up arrow
          SetLength( pts, 3);
          //inc(hx);

          pts[0].x := offsx + aIconRect.Left;        pts[0].y := offsy + aIconRect.Bottom;
          pts[1].x := offsx + aIconRect.Left + hx;   pts[1].y := offsy + aIconRect.Top;
          pts[2].x := offsx + aIconRect.Right;       pts[2].y := offsy + aIconRect.Bottom;

          //DrawPolygon;
          DrawPolygonFBM;
        end;
    itDown : begin // down arrow
          SetLength( pts, 3);
          //inc(hx);

          pts[0].x := offsx + aIconRect.Left;         pts[0].y := offsy + aIconRect.Top;
          pts[1].x := offsx + aIconRect.Left + hx;    pts[1].y := offsy + aIconRect.Bottom;
          pts[2].x := offsx + aIconRect.Right;        pts[2].y := offsy + aIconRect.Top;

          //DrawPolygon;
          DrawPolygonFBM;
          //Bitmap.Canvas.Rectangle(aIconRect);
        end;
    itLeft : begin // left arrow
          SetLength( pts, 3);
          pts[0].x := aIconRect.Right;      pts[0].y := aIconRect.Top;
          pts[1].x := aIconRect.Right;      pts[1].y := aIconRect.Bottom;
          pts[1].x := aIconRect.Left;       pts[1].y := aIconRect.Top + hy;

          DrawPolygon;
        end;
    itRight : begin // right arrow
          SetLength( pts, 3);
          pts[0].x := aIconRect.Left;       pts[0].y := aIconRect.Bottom;
          pts[1].x := aIconRect.Left;       pts[1].y := aIconRect.Top;
          pts[1].x := aIconRect.Right;      pts[1].y := aIconRect.Bottom - hy;

          DrawPolygon;
        end;
    itCheck : begin // an ugly checkmark
          SetLength( pts, 6);
          pts[0].x := aIconRect.Left + Width * 1 div 7;  pts[0].y := aIconRect.Top;
          pts[1].x := aIconRect.Left + Width * 4 div 7;  pts[1].y := aIconRect.Top + Height * 4 div 7;
          pts[2].x := aIconRect.Left + Width * 6 div 7;  pts[2].y := aIconRect.Top + Height * 2 div 7;
          pts[3].x := aIconRect.Left + Width * 7 div 7;  pts[3].y := aIconRect.Top + Height * 3 div 7;
          pts[4].x := aIconRect.Left + pts[1].x;         pts[4].y := aIconRect.Top + Height * 6 div 7;
          pts[5].x := aIconRect.Left + Width * 0 div 7;  pts[5].y := aIconRect.Top + Height * 1 div 7;
          DrawPolygon;
        end;
    itSine : begin // sine
          SetLength( pts, 10);
          for i := 0 to 9 do begin
            pts[i].x := aIconRect.Left + trunc(Width * i/9);
            pts[i].y := trunc(aIconRect.Top + hx + hx * sin(2*PI * i/9));
          end;
          DrawLine;
        end;
    itSaw : begin // saw
          SetLength( pts, 3);
          pts[0].x := aIconRect.Left;
          pts[0].y := aIconRect.Bottom;

          pts[1].x := aIconRect.Left;
          pts[1].y := aIconRect.Top;

          pts[2].x := aIconRect.Right;
          pts[2].y := aIconRect.Bottom;

          DrawLine;
        end;
    itPulse : begin // Pulse
          SetLength( pts, 6);
          pts[0].x := aIconRect.Left;
          pts[0].y := aIconRect.Top + hy;

          pts[1].x := aIconRect.Left;
          pts[1].y := aIconRect.Top;

          pts[2].x := aIconRect.Left + hx;
          pts[2].y := aIconRect.Top;

          pts[3].x := aIconRect.Left + hx;
          pts[3].y := aIconRect.Bottom;

          pts[4].x := aIconRect.Right;
          pts[4].y := aIconRect.Bottom;

          pts[5].x := aIconRect.Right;
          pts[5].y := aIconRect.Bottom - hy;
          DrawLine;
        end;
    end;
    aCanvas.Draw( aBoundsRect.Left, aBoundsRect.Top, Bitmap);
  finally
    BitMap.Free;
  end;
end;

function Darker(c : TColor; f : byte): TColor;
var R, G, B : byte;

  function sub( comp : byte): byte;
  begin
    if comp - f > 0 then
      result := comp - f
    else
      result := 0;
  end;

begin
  if c < 0 then
    c := ColorToRGB(c);

  R := (c and $000000FF);
  G := (c and $0000FF00) shr 8;
  B := (c and $00FF0000) shr 16;

  result := sub(B) * 65536
          + sub(G) * 256
          + sub(R);
end;

function Lighter(c : TColor; f : byte): TColor;
var R, G, B : byte;

  function add( comp : byte): byte;
  begin
    if comp + f < 255 then
      result := comp + f
    else
      result := 255;
  end;

begin
  if c < 0 then
    c := ColorToRGB(c);

  R := (c and $000000FF);
  G := (c and $0000FF00) shr 8;
  B := (c and $00FF0000) shr 16;

  result := add(B) * 65536
          + add(G) * 256
          + add(R);
end;

{function GetG2Color( G2Color : byte): TColor;
begin
  case G2Color of
  COLOR_RED    : Result := clRed;
  COLOR_BLUE   : Result := clBlue;
  COLOR_YELLOW : Result := clYellow;
  COLOR_ORANGE : Result := $00A5FF;
  COLOR_GREEN  : Result := clGreen;
  COLOR_PURPLE : Result := clPurple;
  COLOR_WHITE  : Result := clWhite;
  end;
end;}



end.
