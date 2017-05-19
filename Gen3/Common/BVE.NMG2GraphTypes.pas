unit BVE.NMG2GraphTypes;

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
  System.SysUtils,
  System.Types,
  System.UIConsts,
  System.UITypes,
  System.RTLConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Graphics;

const
  // Module dimensions
  UNITS_COL = 255;
  UNITS_ROW = 15;
  UNIT_MARGIN = 2;

  SCALE_X = 3;
  SCALE_Y = 3;

  GRID_UNITS_X = (UNITS_COL + UNIT_MARGIN) * SCALE_X;
  GRID_UNITS_Y = (UNITS_ROW + UNIT_MARGIN) * SCALE_Y;

  CableColors : array[0..6] of integer = ($005A5AFF,
                                          $00FF6464,
                                          $0050E6E6,
                                          $0050C0FF,
                                          $0050D250,
                                          $00E600C8,
                                          $00FFFFFF);

type
  TKnobType = (ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium,
    ktSlider, ktSeqSlider, ktHSlider, ktNone);
  TLineWidthType = (lwThick, lwThin);
  TOrientationType = (otHorizontal, otVertical);
  TButtonTextType = (bttNormal, bttPush, bttCheck, bttCheckBox);

function Darker(c: TAlphaColor; f: byte): TColor;
function ConvertToAlpha(aColor: integer): TAlphaColor;
procedure DrawSymbol(aCanvas: TCanvas; aDestRect: TRectF; aPathData: string);
function ReplaceSpecialChars(aValue: string): string;

implementation

function ConvertToAlpha(aColor: integer): TAlphaColor;
begin
  Result := $FF000000 + (Cardinal(aColor) and $000000FF) shl 16 +
    (Cardinal(aColor) and $0000FF00) + (Cardinal(aColor) and $00FF0000) shr 16;
end;

function Darker(c: TAlphaColor; f: byte): TColor;
var
  A, R, G, B: byte;

  function sub(comp: byte): byte;
  begin
    if comp - f > 0 then
      Result := comp - f
    else
      Result := 0;
  end;

begin
  A := (c and $FF000000) shr 24;
  R := (c and $000000FF);
  G := (c and $0000FF00) shr 8;
  B := (c and $00FF0000) shr 16;

  Result := A * 256 * 256 * 256 + sub(B) * 256 * 256 + sub(G) * 256 + sub(R);
end;

procedure DrawSymbol(aCanvas: TCanvas; aDestRect: TRectF; aPathData: string);
var
  Rect: TRectF;
  PathData: TPathData;
  dx, dy, s, dx2, dy2: single;
  SaveMatrix: TMatrix;
begin
  PathData := TPathData.Create;
  try
    PathData.Data := aPathData;

    Rect := PathData.GetBounds;
    if (Rect.Width = 0) or (Rect.Height = 0) then
      exit;

    dx := (Rect.Left + Rect.Width / 2);
    dy := (Rect.Top + Rect.Height / 2);

    dx2 := (aDestRect.Left + (aDestRect.Right - aDestRect.Left) / 2);
    dy2 := (aDestRect.Top + (aDestRect.Bottom - aDestRect.Top) / 2);

    s := 0.7 * System.Math.Min(aDestRect.Width / Rect.Width,
      aDestRect.Height / Rect.Height);

    SaveMatrix := aCanvas.Matrix;
    try
      aCanvas.SetMatrix(TMatrix.CreateTranslation(-dx, -dy) *
        TMatrix.CreateScaling(s, s) * TMatrix.CreateTranslation(dx2, dy2) *
        SaveMatrix);

      aCanvas.FillPath(PathData, 1);
    finally
      aCanvas.SetMatrix(SaveMatrix);
    end;
  finally
    PathData.Free;
  end;
end;

function ReplaceSpecialChars(aValue: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to aValue.Length - 1 do
    if CharInSet(aValue.Chars[i], ['/','\','.','>','<']) then
      Result := Result + '_'
    else
      Result := Result + aValue.Chars[i];
end;

end.
