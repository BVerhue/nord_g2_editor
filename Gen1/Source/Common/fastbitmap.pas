unit fastbitmap;

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
//  This unit contains the fastbitmap class for drawing on an ofscreen
//  bitmap, for windows and unix (FPC)
//
//  ////////////////////////////////////////////////////////////////////////////

// Sources:

// Polygon filling
// http://www.sunshine2k.de/stuff/Java/Polygon/Filling/FillPolygon.htm
// http://www.cs.rit.edu/~icss571/filling/index.html

// Anti aliased disc:
// Nils Haeck M.Sc. www.simdesign.nl

// FPC Scanline alternatives
// http://wiki.lazarus.freepascal.org/Developing_with_Graphics

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I Includes\delphi_version.inc}

interface
uses
{$IFDEF FPC}
  GraphType,
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinAPI.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}

{$IFDEF G2_VER220_up}
  System.Classes, System.SysUtils, System.math, VCL.Graphics;
{$ELSE}
  Classes, SysUtils, math, Graphics;
{$ENDIF}


type
{$IFDEF FPC}
  TFastBitmapPixel = Integer;
  PFastBitmapPixel = ^TFastBitmapPixel;

  TFastBitmapPixelComponents = packed record
    B, G, R, A: Byte;
  end;
{$ELSE}
  TFastBitmapPixel = packed record
    B, G, R: Byte;
  end;
  PFastBitmapPixel = ^TFastBitmapPixel;

  TFastBitmapPixelComponents = TFastBitmapPixel;
{$ENDIF}

  { TFastBitmap }

  TFastBitmap = class
  private
    FPixelsData: PByte;
    FSize: TPoint;
    function GetPixel(X, Y: Integer): TFastBitmapPixel; inline;
    procedure SetPixel(X, Y: Integer; const AValue: TFastBitmapPixel); inline;
    procedure SetSize(const AValue: TPoint);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RandomImage;
    procedure CopyToBitmap( Bitmap: TBitmap);
    procedure CopyFromBitmap( Bitmap: TBitmap);
    procedure CopyToBitmapRect( Bitmap: TBitmap; Rect : TRect);
    procedure CopyFromBitmapRect( Bitmap: TBitmap; Rect : TRect);
    procedure DrawAntialisedLine( const AX1, AY1, AX2, AY2: real; const LineColor: TColor);
    procedure DrawAntialisedDisk( CenterX, CenterY, Radius, Feather: single; aColor : TColor; aBrushStyle : TBrushStyle);
    //function  PolygonIsConvex( var polygon : array of TPoint) : boolean;
    procedure DrawPolygon( var polygon : array of TPoint; color : TColor);
    procedure DrawHorizontalLine( x1, x2, y : integer; pixel : TFastBitmapPixelComponents);
    function  LineSeg( var polygon : array of TPoint; N : integer; p1, p2 : TPoint; w : single; aColor : TColor; aAlpha : byte): integer;


    property Size: TPoint read FSize write SetSize;
    property Pixels[X, Y: Integer]: TFastBitmapPixel read GetPixel write SetPixel;
    property PixelData : PByte read FPixelsData;
  end;

//  function SwapBRComponent(Value: Integer): Integer; inline;
//  function NoSwapBRComponent(Value: Integer): Integer; inline;

function CrossRect( Rect1, Rect2 : TRect): TRect;

implementation

type
  TEdge = class
     p1, p2 : TPoint;
     min_x : integer;
     m, CurX : single;
     constructor Create( aP1, aP2 : TPoint);
     procedure Activate;
     procedure Update;
     procedure Deactivate;
   end;

constructor TEdge.Create( aP1, aP2 : TPoint);
begin
  // Create on edge out of two vertices
  p1 := aP1;
  p2 := aP2;

  if p1.x < p2.x then
    min_x := p1.x
  else
    min_x := p2.x;

  CurX := p1.X;

  // m = dy / dx
  if (p1.x - p2.x) <> 0 then
    m := (p1.y - p2.y) / (p1.x - p2.x)
  else
    m := 0;
end;

procedure TEdge.Activate;
begin
  // Called when scanline intersects the first vertice of this edge.
  // That simply means that the intersection point is this vertice.
  curX := p1.x;
end;

procedure TEdge.Update;
begin
  // Update the intersection point from the scanline and this edge.
  // Instead of explicitly calculate it we just increment with 1/m every time
  // it is intersected by the scanline.
  if m <> 0 then
    curX := curX + 1/m;
end;

procedure TEdge.Deactivate;
begin
  // Called when scanline intersects the second vertice,
  // so the intersection point is exactly this vertice and from now on
  // we are done with this edge
  curX := p2.x;
end;

function CrossRect( Rect1, Rect2 : TRect): TRect;
begin
  Result.Left := max(Rect1.Left, Rect2.Left);
  Result.Top := max(Rect1.Top, Rect2.Top);
  Result.Right := min(Rect1.Right, Rect2.Right);
  Result.Bottom := min(Rect1.Bottom, Rect2.Bottom);
end;

function GetRValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb);
end;

function GetGValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 8);
end;

function GetBValue(rgb: DWORD): Byte;
begin
  Result := Byte(rgb shr 16);
end;

{function SwapBRComponent(Value: Integer): Integer;
begin
//  Result := (Value and $00ff00) or ((Value shr 16) and $ff) or ((Value and $ff) shl 16);
  Result := Value;
  TFastBitmapPixelComponents(Result).R := TFastBitmapPixelComponents(Value).B;
  TFastBitmapPixelComponents(Result).B := TFastBitmapPixelComponents(Value).R;
end;

function NoSwapBRComponent(Value: Integer): Integer;
begin
//  Result := (Value and $00ff00) or ((Value shr 16) and $ff) or ((Value and $ff) shl 16);
  Result := Value;
  TFastBitmapPixelComponents(Result).B := TFastBitmapPixelComponents(Value).B;
  TFastBitmapPixelComponents(Result).R := TFastBitmapPixelComponents(Value).R;
end;}

{ TFastBitmap }

function TFastBitmap.GetPixel(X, Y: Integer): TFastBitmapPixel;
begin
{$IFDEF FPC}
  Result := PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) * SizeOf(TFastBitmapPixel))^;
{$ELSE}
  Result := PFastBitmapPixel(Pointer(Int64(FPixelsData) + (Y * FSize.X + X) * SizeOf(TFastBitmapPixel)))^;
{$ENDIF}
end;

procedure TFastBitmap.SetPixel(X, Y: Integer; const AValue: TFastBitmapPixel);
begin
{$IFDEF FPC}
  PFastBitmapPixel(FPixelsData + (Y * FSize.X + X) * SizeOf(TFastBitmapPixel))^ := AValue;
{$ELSE}
  PFastBitmapPixel(Pointer(int64(FPixelsData) + (Y * FSize.X + X) * SizeOf(TFastBitmapPixel)))^ := AValue;
{$ENDIF}
end;

procedure TFastBitmap.SetSize(const AValue: TPoint);
begin
  if (FSize.X = AValue.X) and (FSize.Y = AValue.X) then Exit;
  FSize := AValue;
{$IFDEF FPC}
  FPixelsData := ReAllocMem(FPixelsData, FSize.X * FSize.Y * SizeOf(TFastBitmapPixel));
{$ELSE}
  ReAllocMem(FPixelsData, FSize.X * FSize.Y * SizeOf(TFastBitmapPixel));
{$ENDIF}
end;

constructor TFastBitmap.Create;
begin
  Size := Point(0, 0);
end;

destructor TFastBitmap.Destroy;
begin
  FreeMem(FPixelsData);
  inherited Destroy;
end;

procedure TFastBitmap.RandomImage;
var
  I, X, Y: Integer;
  fp : TFastBitmapPixelComponents;
begin
  for I := 0 to 2 do
    for Y := 0 to (Size.Y div 2) - 1 do
      for X := 0 to (Size.X div 3) - 1 do begin
        fp.R := 255;
        fp.G := 0;
        fp.B := 0;
        Pixels[X + (I * (Size.X div 3)), Y] := fp;//TFastBitmapPixel(255 shl (I * 8));
      end;

  for Y := (Size.Y div 2) to Size.Y - 1 do
    for X := 0 to Size.X - 1 do begin
      fp.R := Random(256);
      fp.G := Random(256);
      fp.B := Random(256);
      Pixels[X, Y] := fp;
    end;
end;

procedure TFastBitmap.DrawAntialisedLine( const AX1, AY1, AX2, AY2: real; const LineColor: TColor);
var
  swapped: boolean;

  procedure plot(const x, y, c: real);
  var
    resclr: TFastBitmapPixelComponents;
  begin
    if swapped then
    begin
      if (x < 0) or (y < 0) or (y >= FSize.X) or (x >= FSize.Y) then
        Exit;
      resclr := TFastBitmapPixelComponents(Pixels[round(y), round(x)])
    end
    else
    begin
      if (y < 0) or (x < 0) or (x >= FSize.X) or (y >= FSize.Y) then
        Exit;
      resclr := TFastBitmapPixelComponents(Pixels[round(x), round(y)]);
    end;
    resclr.R := round(resclr.R * (1-c) + GetRValue(LineColor) * c);
    resclr.G := round(resclr.G * (1-c) + GetGValue(LineColor) * c);
    resclr.B := round(resclr.B * (1-c) + GetBValue(LineColor) * c);
    if swapped then
      Pixels[round(y), round(x)] := TFastBitmapPixel(resclr)
    else
      Pixels[round(x), round(y)] := TFastBitmapPixel(resclr);
  end;

  function rfrac(const x: real): real; inline;
  begin
    rfrac := 1 - frac(x);
  end;

  procedure swap(var a, b: real);
  var
    tmp: real;
  begin
    tmp := a;
    a := b;
    b := tmp;
  end;

var
  x1, x2, y1, y2, dx, dy, gradient, xend, yend, xgap, xpxl1, ypxl1,
  xpxl2, ypxl2, intery: real;
  x: integer;

begin

  x1 := AX1;
  x2 := AX2;
  y1 := AY1;
  y2 := AY2;

  dx := x2 - x1;
  dy := y2 - y1;
  swapped := abs(dx) < abs(dy);
  if swapped then
  begin
    swap(x1, y1);
    swap(x2, y2);
    swap(dx, dy);
  end;
  if x2 < x1 then
  begin
    swap(x1, x2);
    swap(y1, y2);
  end;

  if dx <> 0 then
    gradient := dy / dx
  else
    gradient := 0;

  xend := round(x1);
  yend := y1 + gradient * (xend - x1);
  xgap := rfrac(x1 + 0.5);
  xpxl1 := xend;
  ypxl1 := floor(yend);
  plot(xpxl1, ypxl1, rfrac(yend) * xgap);
  plot(xpxl1, ypxl1 + 1, frac(yend) * xgap);
  intery := yend + gradient;

  xend := round(x2);
  yend := y2 + gradient * (xend - x2);
  xgap := frac(x2 + 0.5);
  xpxl2 := xend;
  ypxl2 := floor(yend);
  plot(xpxl2, ypxl2, rfrac(yend) * xgap);
  plot(xpxl2, ypxl2 + 1, frac(yend) * xgap);

  for x := round(xpxl1) + 1 to round(xpxl2) - 1 do
  begin
    plot(x, floor(intery), rfrac(intery));
    plot(x, floor(intery) + 1, frac(intery));
    intery := intery + gradient;
  end;
end;

procedure TFastBitmap.DrawAntialisedDisk( CenterX, CenterY, Radius, Feather: single; aColor : TColor; aBrushStyle : TBrushStyle);
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
  x, y        : integer;
  LX, RX, LY, RY : integer;
  Fact        : integer;
  RPF2, RMF2  : single;
  SqY, SqDist : single;
  sqX         : array of single;
  LG, LB, LR  : byte;
  LBack, LNew : TFastBitmapPixelComponents;
  aRatio      : Real;
  LMinusRatio : Real;
  R           : TRect;
begin
  if aColor < 0 then
    aColor := ColorToRGB(aColor);

  LR := (aColor and $000000FF);
  LG := (aColor and $0000FF00) shr 8;
  LB := (aColor and $00FF0000) shr 16;

  // Determine some helpful values (singles)
  RPF2 := sqr(Radius + Feather/2);
  RMF2 := sqr(Radius - Feather/2);

  // Determine bounds:
  LX := Max(floor(CenterX - RPF2), 0);
  RX := Min(ceil (CenterX + RPF2), FSize.X - 1);
  LY := Max(floor(CenterY - RPF2), 0);
  RY := Min(ceil (CenterY + RPF2), FSize.Y - 1);

  // Optimization run: find squares of X first
  SetLength(SqX, RX - LX + 1);
  for x := LX to RX do
    SqX[x - LX] := sqr(x - CenterX);

  // Loop through Y values
  for y := LY to RY do begin

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

          LNew.B := LB;
          LNew.G := LG;
          LNew.R := LR;
          Pixels[x, y] := TFastBitmapPixel(LNew);

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
          LBack := TFastBitmapPixelComponents(Pixels[x,y]);
          LNew.B := round( LB * ARatio + LBack.B * LMinusRatio);
          LNew.G := round( LG * ARatio + LBack.G * LMinusRatio);
          LNew.R := round( LR * ARatio + LBack.R * LMinusRatio);
          Pixels[x,y] := TFastBitmapPixel(LNew);

        end else begin
          //P[x] := 0;
        end;
      end;
    end;
  end;
end;

{Fills a block of memory with the given dword}
procedure FillDWord(AAddress: Pointer; ADWordValue: integer; ADWordCount: integer);
  asm
  {On Entry: eax = AAddress
  edx = DWordValue
  ecx = ADWordCount}
  neg ecx
  jns @Done
  shl ecx, 2
  sub eax, ecx
  @FillLoop:
  mov [eax + ecx], edx
  add ecx, 4
  js @FillLoop
  @Done:
end;

procedure TFastBitmap.DrawHorizontalLine( x1, x2, y : integer; pixel : TFastBitmapPixelComponents);
var ptr : pointer;
    i, l, s : integer;
    //redpixel, bluepixel : TFastBitmapPixel;
begin
  if (y < 0) or (y > Size.Y - 1) then
    exit;

  {redpixel.B := 0;
  redpixel.G := 0;
  redpixel.R := $ff;

  bluepixel.B := $ff;
  bluepixel.G := $ff;
  bluepixel.R := 00;}

  if (x1 < 0) then begin
    x1 := 0;
    //pixel := redpixel;
  end;

  if (x1 > Size.X - 1) then begin
    x1 := Size.X - 1;
    //pixel := redpixel;
  end;

  if (x2 < x1) then
    exit;

  if (x2 > Size.X - 1) then begin
    x2 := Size.X - 1;
    //pixel := redpixel;
  end;

  l := x2 - x1;
  i := 0;
  s := SizeOf(TFastBitmapPixel);
  ptr := pointer(int64(FPixelsData) + y * s * Size.X + x1 * s);
  while (i<=l) do begin
    PFastBitmapPixel(ptr)^ := pixel;
    ptr := pointer(int64(ptr) + s);
    inc(i);
  end;
end;

{function TFastBitmap.PolygonIsConvex( var polygon : array of TPoint) : boolean;
var p, v, u, tmp : TPoint;
    i, res, newres : integer;
begin
  // check if polygon is convex

  Result := False;

  if Length(polygon) < 3 then
    exit;

  for i := 0 to length(polygon) - 3 do begin
    p := polygon[i];
    tmp := polygon[i+1];
    v.x := tmp.x - p.x;
    v.y := tmp.y - p.y;
    u := polygon[i+2];
    if i = 0 then
      res := u.x * v.y - u.y * v.x + v.x * p.y - v.y * p.x
    else begin
      newres := u.x * v.y - u.y * v.x + v.x * p.y - v.y * p.x;
      if ((newres > 0) and (res < 0)) or ((newres < 0) and (res > 0)) then
        exit;
    end;
  end;

  Result := True;
end;}

function CompareEdges( Edge1, Edge2 : pointer): integer;
begin
  Result := 0;
  if TEdge(Edge1).p1.y < TEdge(Edge2).p1.y then
    Result := -1
  else
    if TEdge(Edge1).p1.y > TEdge(Edge2).p1.y then
      Result := 1
    else
      if TEdge(Edge1).p1.y = TEdge(Edge2).p1.y then begin
        if TEdge(Edge1).curx < TEdge(Edge2).curx then
          Result := -1
        else
          if TEdge(Edge1).curx > TEdge(Edge2).curx then
            Result := 1;
      end;
end;

procedure TFastBitmap.DrawPolygon( var polygon : array of TPoint; color : TColor);
var GlobalEdgeList, ActiveEdgeList : TList;
    x1, x2, i, ScanlineEnd, scanline : integer;
    fp : TFastBitmapPixel;

    // http://www.cs.rit.edu/~icss571/filling/index.html

    // Seems to be a small bug in here somewhere

   // Create from the polygon vertices an array of edges.
   // Note that the first vertice of an edge is always the one with the smaller y coordinate one of both
    procedure createEdges;
    var i : integer;
    begin
      for i := 0 to Length(polygon) - 2 do begin
        if (polygon[i].y < polygon[i+1].y) then
          GlobalEdgeList.Add( TEdge.Create( polygon[i], polygon[i+1]))
        else
          if (polygon[i].y > polygon[i+1].y) then
            GlobalEdgeList.Add( TEdge.Create( polygon[i+1], polygon[i]));
      end;
    end;

begin
  GlobalEdgeList := TList.Create;
  ActiveEdgeList := TList.Create;
  try

    {if not PolygonIsConvex( polygon) then begin
      raise Exception.Create('Polygon is not convex.');
      exit;
    end;}

    if Color < 0 then
      Color := ColorToRGB(Color);

    fp.R := (Color and $000000FF);
    fp.G := (Color and $0000FF00) shr 8;
    fp.B := (Color and $00FF0000) shr 16;

    // create edges array from polygon vertice vector
    // make sure that first vertice of an edge is the smaller one
    createEdges;

    if GlobalEdgeList.Count = 0 then
      exit;

    GlobalEdgeList.Sort(@CompareEdges);

    // find biggest y-coord of all vertices
    ScanLineEnd := 0;
    for i := 0 to GlobalEdgeList.Count - 1 do
      if scanlineEnd < TEdge(GlobalEdgeList[i]).p2.y then
        scanlineEnd := TEdge(GlobalEdgeList[i]).p2.y;

    scanline := TEdge(GlobalEdgeList[0]).p1.Y;

    i := 0;
    while (i<GlobalEdgeList.Count) do begin
      if TEdge(GlobalEdgeList[i]).p1.Y = scanline then begin
        TEdge(GlobalEdgeList[i]).Activate;
        ActiveEdgeList.Add(GlobalEdgeList[i]);
        GlobalEdgeList.Delete(i);
      end else
        if TEdge(GlobalEdgeList[i]).p1.Y > scanline then
          break
        else
          inc(i);
    end;

    // move scanline step by step down to biggest one
    while (scanline <= scanlineEnd) and (ActiveEdgeList.Count > 0) do begin

      i := 0;
      while (i<ActiveEdgeList.Count-1) do begin
        x1 := trunc(TEdge(ActiveEdgelist[i]).CurX);
        x2 := trunc(TEdge(ActiveEdgelist[i+1]).CurX);
        if x1 > x2 then
          DrawHorizontalLine( x2, x1, scanline, fp)
        else
          DrawHorizontalLine( x1, x2, scanline, fp);
        inc(i);
        inc(i);
      end;

      inc(scanline);

      i := 0;
      while (i<ActiveEdgeList.Count) do begin
        if TEdge(ActiveEdgeList[i]).p2.y < scanline then
          ActiveEdgeList.Delete(i)
        else begin
          TEdge(ActiveEdgeList[i]).Update;
          inc(i);
        end;
      end;

      i := 0;
      while (i<GlobalEdgeList.Count) do begin
        if TEdge(GlobalEdgeList[i]).p1.Y = scanline then begin
          TEdge(GlobalEdgeList[i]).Activate;
          ActiveEdgeList.Add(GlobalEdgeList[i]);
          ActiveEdgeList.Sort(CompareEdges);
          GlobalEdgeList.Delete(i);
        end else
          {if TEdge(GlobalEdgeList[i]).p1.Y > scanline then
            break;}
        inc(i);
      end;

    end;

  finally
    ActiveEdgeList.Free;
    GlobalEdgeList.Free;
  end;
end;

//  get point at which line intersects raster line
//  Params: y - the raster line / y value
//          x1, y1 - line point 1 xy coords
//          x2, y2 - line point2 xy coords
//  Returns: x cordiante of intersect
//  Notes: do not call for parallel line to x axis.

function intersect( y, x1, y1, x2, y2: single) : single;
var g, c : single;
begin
  g := (x1 - x2) / (y1 - y2);
  c := x1 - y1 * g;

  Result := y * g + c;
end;

//  bubble sort a vector
//  Params: x - list of values
//          N - N values

procedure bubblesort(var x : array of single; N : integer);
var
  flag : integer;
  temp : single;
  i : integer;
begin
  repeat
    flag := 0;
   	for i:=0 to N-2 do begin
  	  if x[i] > x[i+1] then begin
  	    temp := x[i];
	     	x[i] := x[i+1];
		    x[i+1] := temp;
    		flag := 1;
      end;
	  end;
  until flag = 0;
end;

function TFastBitmap.LineSeg( var polygon : array of TPoint; N : integer; p1, p2 : TPoint; w : single;  aColor : TColor; aAlpha : byte): integer;
// http://www.malcolmmclean.site11.com/www/Bioinformatics/polygons.html
var
  i, ii, iii, line : integer;
  miny, maxy : single;
  minx, maxx, Nsegs : integer;
  x1, x2,
  y1, y2, suby : single;
  sub : integer;
  segs : array[0..255] of single;
  aabuff : array of byte;
  alpha : integer;
  LR, LG, LB : byte;
  fp : TFastBitmapPixel;
  p : TPoint;
  a,b,c,d, DT,r : single;
  s : integer;
  ptr : pointer;
  RoundnessFactor : single;
  Darker : single;
begin
  if N > 255 then begin
    Result := -1;
    exit;
  end;

  RoundnessFactor := w*w*4;
  // p1 p2 define midline through cable segment

  s := SizeOf(TFastBitmapPixel);
  b := p2.y-p1.y;
  a := p2.x-p1.x;
  DT := (a*a + b*b);
  // pt1 and pt2 are the same. More efficient would be
  // a divide by zero exception trap
  if DT <=0 then
    exit;

  LR := (AColor and $000000FF);
  LG := (AColor and $0000FF00) shr 8;
  LB := (AColor and $00FF0000) shr 16;

  SetLength( aabuff, FSize.X*16);

  miny := polygon[0].Y;
  maxy := polygon[0].Y;

  for i := 1 to N-1 do begin
    if miny > polygon[i].Y then
      miny := polygon[i].Y;
    if maxy < polygon[i].Y then
      maxy := polygon[i].Y;
  end;

  if(miny < 0) then
    miny := 0;
  if maxy > FSize.y - 1 then
    maxy := FSize.y - 1;

  for line := trunc(miny) to trunc(maxy) do begin

    minx := FSize.X;
    maxx := 0;

    suby := line;
    for sub := 0 to 3 do begin

      Nsegs := 0;
      for i:=0 to N-1 do begin

        if (i = N-1) then begin
          x1 := polygon[i].x;
          x2 := polygon[0].x;
          y1 := polygon[i].y;
          y2 := polygon[0].y;
        end else begin
          x1 := polygon[i].x;
          x2 := polygon[i+1].x;
          y1 := polygon[i].y;
          y2 := polygon[i+1].y;
        end;

        if ((y1 > suby) and (y2 <= suby)) or ((y1 <= suby) and (y2 > suby)) then begin
          segs[Nsegs] := intersect(suby, x1, y1, x2, y2);
          inc(NSegs);
        end;

      end;
      if (Nsegs = 0) then
        continue;

      bubblesort(segs, Nsegs);

      i := 0;
      while i<Nsegs do begin
        if segs[i] < 0 then
          segs[i] := 0;
        if segs[i+1] > FSize.X then
          segs[i+1] := FSize.X;
        for ii := trunc(segs[i] * 4) to trunc(segs[i+1] *4) do
          aabuff[ sub * FSize.X * 4 + ii] := 1;
        i := i + 3;
      end;

      if minx > segs[0] then
        minx := trunc(segs[0]);
      if maxx < segs[Nsegs-1] then
        maxx := trunc(segs[Nsegs-1]);

      suby := suby + 1/3.0;
    end;

    ptr := pointer(int64(FPixelsData) + line * s * FSize.X + minx * s);

    for i := minx to maxx do begin

      alpha := 0;
      for ii :=0 to 3 do begin
        for iii :=0 to 3 do begin
          alpha := alpha + aabuff[ii * 4 * FSize.X + i*4 + iii];
          aabuff[ii *4 * FSize.X + i *4 + iii] := 0;
        end;
      end;
      alpha := trunc((aAlpha * alpha)/16);

      p.X := i;
      p.Y := line;

      c := p.y-p1.y;
      d := p.x-p1.x;
      r := (c*b + d*a)/DT;
      r := c*a - d*b;
      Darker := (RoundnessFactor - (r*r/DT)) / RoundNessFactor;

      fp := PFastBitmapPixel(ptr)^;

      fp.R := trunc(( LR * Darker * alpha + fp.R * (255 - alpha)) / 255);
      fp.G := trunc(( LG * Darker * alpha + fp.G * (255 - alpha)) / 255);
      fp.B := trunc(( LB * Darker * alpha + fp.B * (255 - alpha)) / 255);

      PFastBitmapPixel(ptr)^ := fp;

      ptr := pointer(int64(ptr) + s);
    end;
  end;

  Finalize(aabuff);
  Result := 0;
end;


procedure TFastBitmap.CopyToBitmap(Bitmap : TBitmap);
var Y: Integer;
    PixelPtr: PByte;
    RowPtr: Pointer;
{$IFDEF FPC}
    RawImage: TRawImage;
{$ENDIF}
    BytePerPixel: Integer;
    BytePerRow, FBMBytePerRow: Integer;
begin
{$IFDEF FPC}
  try
    Bitmap.BeginUpdate(False);
    RawImage := Bitmap.RawImage;
    RowPtr := RawImage.Data;
    BytePerPixel := RawImage.Description.BitsPerPixel div 8;
    BytePerRow := RawImage.Description.BytesPerLine;
{$ELSE}
    RowPtr := Bitmap.Scanline[0];
    BytePerPixel := SizeOf(TFastBitmapPixel);
    BytePerRow := FSize.X * BytePerPixel;
{$ENDIF}
    FBMBytePerRow := FSize.X * SizeOf(TFastBitmapPixel);
    PixelPtr := FPixelsData;
    for Y := 0 to Size.Y - 1 do begin
{$IFNDEF FPC}
      RowPtr := Bitmap.Scanline[Y];
{$ENDIF}
      Move(PixelPtr^, RowPtr^, BytePerRow);
      Inc(PByte(PixelPtr), FBMBytePerRow);
{$IFDEF FPC}
      Inc(PByte(RowPtr), BytePerRow);
{$ENDIF}
    end;
{$IFDEF FPC}
  finally
    Bitmap.EndUpdate(False);
  end;
{$ENDIF}
end;

procedure TFastBitmap.CopyFromBitmap(Bitmap: TBitmap);
var Y: Integer;
    PixelPtr: PByte;
    RowPtr: Pointer;
{$IFDEF FPC}
    RawImage: TRawImage;
{$ENDIF}
    BytePerPixel: Integer;
    BytePerRow, FBMBytePerRow: Integer;
begin
  Size := Point(Bitmap.Width, Bitmap.Height);

{$IFDEF FPC}
  try
    Bitmap.BeginUpdate(False);
    RawImage := Bitmap.RawImage;
    RowPtr := RawImage.Data;
    BytePerPixel := RawImage.Description.BitsPerPixel div 8;
    BytePerRow := RawImage.Description.BytesPerLine;
{$ELSE}
    RowPtr := Bitmap.Scanline[0];
    BytePerPixel := SizeOf(TFastBitmapPixel);
    BytePerRow := FSize.X * BytePerPixel;
{$ENDIF}
    FBMBytePerRow := FSize.X * SizeOf(TFastBitmapPixel);
    PixelPtr := FPixelsData;
    for Y := 0 to Size.Y - 1 do begin
{$IFNDEF FPC}
      RowPtr := Bitmap.Scanline[Y];
{$ENDIF}
      Move(RowPtr^, PixelPtr^, BytePerRow);
      Inc(PByte(PixelPtr), FBMBytePerRow);
{$IFDEF FPC}
      Inc(PByte(RowPtr), BytePerRow);
{$ENDIF}
    end;
{$IFDEF FPC}
  finally
    Bitmap.EndUpdate(False);
  end;
{$ENDIF}
end;

procedure TFastBitmap.CopyToBitmapRect(Bitmap: TBitmap; Rect: TRect);
var Y: Integer;
    PixelPtr: PByte;
    RowPtr: Pointer;
{$IFDEF FPC}
    RawImage: TRawImage;
{$ENDIF}
    BytePerPixel: Integer;
    BytePerRow, FBMBytePerRow: Integer;
begin
  if Rect.Left < 0 then
    Rect.Left := 0;
  if Rect.Right < 0 then
    Rect.Right := 0;
  if Rect.Top < 0 then
    Rect.Top := 0;
  if Rect.Bottom < 0 then
    Rect.Bottom := 0;
  if Rect.Right > Bitmap.Width - 1 then
    Rect.Right := Bitmap.Width - 1;
  if Rect.Bottom > Bitmap.Height - 1 then
    Rect.Bottom := Bitmap.Height - 1;
    
{$IFDEF FPC}
  try
    Bitmap.BeginUpdate(False);
    RawImage := Bitmap.RawImage;
    BytePerPixel := RawImage.Description.BitsPerPixel div 8;
    BytePerRow := RawImage.Description.BytesPerLine;
    RowPtr := RawImage.Data + Rect.Left * BytePerPixel  + BytePerRow * Rect.Top;
{$ELSE}
//    RowPtr := Pointer(int64(Bitmap.Scanline[0]) + BytePerPixel * Rect.Left);
    BytePerPixel := SizeOf( TFastBitmapPixel);
    BytePerRow := FSize.X * BytePerPixel;
{$ENDIF}
    FBMBytePerRow := FSize.X * SizeOf(TFastBitmapPixel);
    PixelPtr := FPixelsData;
    for Y := Rect.Top to Rect.Bottom - 1 do begin
{$IFNDEF FPC}
      RowPtr := Pointer(int64(Bitmap.Scanline[Y]) + BytePerPixel * Rect.Left);
{$ENDIF}
      Move(PixelPtr^, RowPtr^, FBMBytePerRow);
      Inc(PByte(PixelPtr), FBMBytePerRow);
{$IFDEF FPC}
      Inc(PByte(RowPtr), BytePerRow);
{$ENDIF}
    end;
{$IFDEF FPC}
  finally
    Bitmap.EndUpdate(False);
  end;
{$ENDIF}
end;

procedure TFastBitmap.CopyFromBitmapRect(Bitmap: TBitmap; Rect: TRect);
var Y: Integer;
    PixelPtr: PByte;
    RowPtr: Pointer;
{$IFDEF FPC}
    RawImage: TRawImage;
{$ENDIF}
    BytePerPixel: Integer;
    BytePerRow, FBMBytePerRow: Integer;
begin
  if Rect.Left < 0 then
    Rect.Left := 0;
  if Rect.Right < 0 then
    Rect.Right := 0;
  if Rect.Top < 0 then
    Rect.Top := 0;
  if Rect.Bottom < 0 then
    Rect.Bottom := 0;
  if Rect.Right > Bitmap.Width - 1 then
    Rect.Right := Bitmap.Width - 1;
  if Rect.Bottom > Bitmap.Height - 1 then
    Rect.Bottom := Bitmap.Height - 1;

  Size := Point(Rect.Right - Rect.Left + 1, Rect.Bottom - Rect.Top + 1);

{$IFDEF FPC}
  try
    Bitmap.BeginUpdate(False);
    RawImage := Bitmap.RawImage;
    BytePerPixel := RawImage.Description.BitsPerPixel div 8;
    BytePerRow := RawImage.Description.BytesPerLine;
    RowPtr := RawImage.Data + BytePerPixel * Rect.Left + BytePerRow * Rect.Top;
{$ELSE}
//    RowPtr := Pointer(int64(Bitmap.Scanline[0]) + BytePerPixel * Rect.Left);
    BytePerPixel := SizeOf(TFastBitmapPixel);
    BytePerRow := FSize.X * BytePerPixel;
{$ENDIF}
    FBMBytePerRow := FSize.X * SizeOf(TFastBitmapPixel);
    PixelPtr := FPixelsData;
    for Y := Rect.Top to Rect.Bottom - 1 do begin
{$IFNDEF FPC}
      RowPtr := Pointer(int64(Bitmap.Scanline[Y]) + BytePerPixel * Rect.Left);
{$ENDIF}
      Move(RowPtr^, PixelPtr^, FBMBytePerRow);
      Inc(PByte(PixelPtr), FBMBytePerRow);
{$IFDEF FPC}
      Inc(PByte(RowPtr), BytePerRow);
{$ENDIF}
    end;
{$IFDEF FPC}
  finally
    Bitmap.EndUpdate(False);
  end;
{$ENDIF}
end;

end.
