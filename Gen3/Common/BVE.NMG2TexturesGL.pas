unit BVE.NMG2TexturesGL;

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
  System.Classes,
  System.Generics.Collections,
  System.UIConsts,
  System.UITypes,
  System.RTLConsts,
  System.Math,
  System.Math.Vectors,
  FMX.Graphics,
  FMX.Types,
  FMX.Materials,
  FMX.MaterialSources,
  FMX.Types3D,
  BVE.NMG2GraphTypes;

const
  FONT_FAMILY = 'Roboto';

  // http://stackoverflow.com/questions/17701006/why-are-condensed-fonts-not-displayed-properly-in-firemonkey

  //FONT_FAMILY = 'Roboto Cn';
  //FONT_FAMILY = 'Arial Narrow';
  //FONT_FAMILY = 'Roboto Mono';

  FONTSIZE_X = 8;
  FONTSIZE_Y = 12;

type
  TTiledTexture = class(TTextureBitmap)
  private
    FRefCount: integer;
    FID: string;
    function GetSize: integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure SaveToCache(const aFileName: string);
    function LoadFromCache(const aFileName: string): boolean;

    procedure CreateModuleSegment;
    procedure CreateModuleSegmentTop;
    procedure CreateModuleSegmentMiddle;
    procedure CreateModuleSegmentBottom;

    procedure CreateFont(aFont: TFont);
    procedure CreateLabel(const aFontFamily: string; const aFontSize: integer; const aFontStyle: TFontStyles; const aCaption: string);
    procedure CreateConnectorIn;
    procedure CreateConnectorOut;
    procedure CreateBtn(const aWidth, aHeight: single);
    //procedure CreateBtnRadioEdit(const aWidth, aHeight: single);
    //procedure CreateTextEdit(const aWidth, aHeight: single);
    procedure CreateBtnText(const aWidth, aHeight: single; const aImageIDs, aTexts: string);
    procedure CreateBtnFlat(const aWidth, aHeight: single; const aImageIDs, aTexts: string);
    procedure CreateBtnRadio(const aWidth, aHeight: single; const aImageIDs, aTexts: string);
    procedure CreateBtnIncDec(const aWidth, aHeight: single; const aHorizontal: boolean);
    procedure CreateGraph(const aWidth, aHeight: single);
    procedure CreateTextField(const aWidth, aHeight: single);
    procedure CreateBtnLevelShift(const aWidth, aHeight: single; const aImageIDs: string);
    procedure CreatePartSelectorBtn(const aWidth, aHeight: single);
    procedure CreatePartSelectorList(const aWidth, aHeight: single; const aImageIDs, aTexts: string);

    procedure CreateKnobIndicator(const aRDial, aRKnob: TRectF);
    procedure CreateKnob(const aRDial, aRKnob: TRectF);
    procedure CreateKnobBtns(const aRBtns: TRectF);
    procedure CreateCenterBtn(const aRCntr: TRectF);

    procedure CreateSliderIndicator(const aRDial, aRKnob, aRBtns: TRectF; const aHorizontal: boolean);
    procedure CreateSlider(const aRDial, aRKnob, aRBtns: TRectF; const aHorizontal: boolean);
    procedure CreateSliderBtns(const aRBtns: TRectF; const aHorizontal: boolean);

    procedure CreateLedGreen;
    procedure CreateLedSequencer;
    procedure CreateMiniVU;
    procedure CreateRackRails;
    procedure CreateWhiteKeys(aWidth, aHeight, aBlackWidth, aBlackHeight: single);
    procedure CreateBlackKey(aWidth, aHeight: single);

    procedure CreateMenuLeft;
    procedure CreateMenuRight;

    property ID: string read FID write FID;
    property Size: integer read GetSize;
  end;

  TTextureListGL = class(TComponent)
  private
    FSize: integer;
    FMaxSize: integer;
    FCacheDir: string;
    FMemItems: TObjectDictionary<string, TTiledTexture>;
    FUsedList: TStringList; // Least used has index 0
    function GetItem(const aKey: string): TTiledTexture;
    procedure SetItem(const aKey: string; const Value: TTiledTexture);
    procedure SaveToCache(const aTexture: TTiledTexture);
    function LoadFromCache(const aTextureID: string; var aTexture: TTiledTexture): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function TextureFileName(const aTextureID: string): string;

    procedure UnLink(const aTextureID: string);
    function Link(const aKey: string; var aTexture: TTiledTexture): boolean;
    procedure Add(const aKey: string; aTexture : TTiledTexture);

    function ContainsKey(const aKey: string): boolean;

    procedure SaveTextures(const aPath: string);
    procedure LoadFromZip(const aFileName: string);

    property Items[const aKey: string]: TTiledTexture read GetItem write SetItem; default;
    property CacheDir: string read FCacheDir;
    property MaxSize: integer read FMaxSize write FMaxSize;
  end;

  procedure GetKnobMetrics(const aKnobType: TKnobType; var aDialRect, aKnobRect, aBtnsRect, aCntrRect: TRectF);

  function GetBtnTextTexID(const aWidth, aHeight: single; const aImageIDs, aTexts: string): string;
  function GetBtnFlatTexID(const aWidth, aHeight: single; const aImageIDs, aTexts: string): string;
  function GetBtnLevelShiftTexID(const aWidth, aHeight: single; const aImageIDs: string): string;
  function GetBtnIncDecTexID(const aWidth, aHeight: single; const aHorizontal: boolean): string;
  function GetBtnRadioTexID(const aWidth, aHeight: single; const aImageIDs, aTexts: string): string;
  function GetBtnTexID(const aWidth, aHeight: single): string;
  //function GetBtnRadioEditTexID(const aWidth, aHeight: single): string;
  //function GetTextEditTexID(const aWidth, aHeight: single): string;
  function GetPartSelectorBtnTexID(const aWidth, aHeight: single): string;
  function GetPartSelectorListTexID(const aWidth, aHeight: single; const aImageIDs, aTexts: string): string;
  function GetTextFieldTexID(const aWidth, aHeight: single): string;
  function GetGraphTexID(const aWidth, aHeight: single): string;
  function GetKnobTexID(const aKnobType: TKnobType): string;
  function GetKnobIndicatorTexID(const aKnobType: TKnobType): string;
  function GetConnectorInTexID: string;
  function GetConnectorOutTexID: string;
  function GetLedTexID(const aSeqLedType: boolean): string;
  function GetMiniVUTexID: string;
  function GetFontTexID: string;
  function GetLabelTexID(const aFontSize: integer; const aCaption: string): string;

implementation
uses
  System.Zip,
  System.IOUtils,
  BVE.NMG2PathData;

type
  TKeyType = (ktWhite, ktBlack);

  TOctaveKey = record
    KeyID : integer;
    KeyType : TKeyType;
  end;

const
  KeyTypes : array[0..11] of TOctaveKey =
     ((KeyID : 0; KeyType : ktWhite),
      (KeyID : 0; KeyType : ktBlack),
      (KeyID : 1; KeyType : ktWhite),
      (KeyID : 1; KeyType : ktBlack),
      (KeyID : 2; KeyType : ktWhite),
      (KeyID : 3; KeyType : ktWhite),
      (KeyID : 3; KeyType : ktBlack),
      (KeyID : 4; KeyType : ktWhite),
      (KeyID : 4; KeyType : ktBlack),
      (KeyID : 5; KeyType : ktWhite),
      (KeyID : 5; KeyType : ktBlack),
      (KeyID : 6; KeyType : ktWhite));

   MAX_KEYS = 127;

procedure GetKnobMetrics(const aKnobType: TKnobType;
  var aDialRect, aKnobRect, aBtnsRect, aCntrRect: TRectF);

  function CalcRect(aLeft, aTop, aWidth, aHeight: single): TRectF;
  begin
    Result := RectF(
      aLeft * SCALE_X,
      aTop * SCALE_Y,
      (aLeft + aWidth) * SCALE_X,
      (aTop + aHeight) * SCALE_Y);
  end;

begin
  case aKnobType of
    ktBig:
      begin;
        // d = 22
        aDialRect := CalcRect(0,  0, 22, 27);
        aKnobRect := CalcRect(0,  0, 22, 22);
        aBtnsRect := CalcRect(0, 18, 22,  9);
      end;
    ktMedium:
      begin;
        // d = 20
        aDialRect := CalcRect(0,  0, 22, 25);
        aKnobRect := CalcRect(1,  0, 20, 20);
        aBtnsRect := CalcRect(0, 16, 22,  9);
      end;
    ktResetMedium:
      begin
        // d = 20
        aDialRect := CalcRect(0,  0, 22, 31);
        aKnobRect := CalcRect(1,  6, 20, 20);
        aBtnsRect := CalcRect(0, 22, 22,  9);
        aCntrRect := CalcRect(6,  0, 10,  4);
      end;
    ktSmall:
      begin;
        // d = 18
        aDialRect := CalcRect(0,  0, 22, 23);
        aKnobRect := CalcRect(2,  0, 18, 18);
        aBtnsRect := CalcRect(0, 14, 22,  9);
      end;
    ktReset:
      begin
        // d = 18
        aDialRect := CalcRect(0,  0, 22, 29);
        aKnobRect := CalcRect(2,  6, 18, 18);
        aBtnsRect := CalcRect(0, 20, 22,  9);
        aCntrRect := CalcRect(6,  0, 10,  4);
      end;
    ktSlider:
      begin;
        aDialRect := CalcRect(0,  0, 11, 61);
        aKnobRect := CalcRect(0,  0, 11,  8);
        aBtnsRect := CalcRect(0, 45, 11, 16);
      end;
    ktSeqSlider:
      begin;
        aDialRect := CalcRect(0,  0, 11, 78);
        aKnobRect := CalcRect(0,  0, 11,  8);
        aBtnsRect := CalcRect(0, 62, 11, 16);
      end;
    ktHSlider:
      begin
        aDialRect := CalcRect(0,  0, 61, 11);
        aKnobRect := CalcRect(0,  0,  8, 11);
        aBtnsRect := CalcRect(45, 0, 16, 11);
      end;
  end;
end;

function GetBtnFlatTexID(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string): string;
var
  Temp: string;
begin
  Temp :=
      'BtnFlat'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
  if aImageIDs <> '' then
    Temp := Temp + ';' + aImageIDs
  else
    Temp := Temp + ';' + aTexts;

  Result := ReplaceSpecialChars(Temp);
end;

function GetBtnIncDecTexID(const aWidth, aHeight: single;
  const aHorizontal: boolean): string;
var
  Temp: string;
begin
  if aHorizontal then
    Temp := 'BtnIncDecHorz'
  else
    Temp := 'BtnIncDecVert';

  Temp := Temp
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;

function GetBtnLevelShiftTexID(const aWidth, aHeight: single;
  const aImageIDs: string): string;
var
  Temp: string;
begin
  Temp :=
      'LvlShift'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
  Temp := Temp + ';' + aImageIDs;

  Result := ReplaceSpecialChars(Temp);
end;

function GetBtnTexID(const aWidth, aHeight: single): string;
begin
  Result :=
      'Btn'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
end;

{function GetBtnRadioEditTexID(const aWidth,
  aHeight: single): string;
var
  Temp: string;
begin
  Temp :=
      'BtnRadioEdit'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;}

function GetBtnRadioTexID(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string): string;
var
  Temp: string;
begin
  Temp :=
      'BtnRadio'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
  if aImageIDs <> '' then
    Temp := Temp + ';' + aImageIDs
  else
    Temp := Temp + ';' + aTexts;

  Result := ReplaceSpecialChars(Temp);
end;

function GetBtnTextTexID(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string): string;
var
  Temp: string;
begin
  Temp :=
      'BtnTxt'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
  if aImageIDs <> '' then
    Temp := Temp + ';' + aImageIDs
  else
    Temp := Temp + ';' + aTexts;

  Result := ReplaceSpecialChars(Temp);
end;

function GetConnectorInTexID: string;
begin
  Result := 'ConnIn';
end;

function GetConnectorOutTexID: string;
begin
  Result := 'ConnOut';
end;

function GetKnobIndicatorTexID(const aKnobType: TKnobType): string;
begin
  case aKnobType of
    ktBig:
      begin;
        Result := 'KIBig';
      end;
    ktMedium:
      begin;
        Result := 'KIMed';
      end;
    ktResetMedium:
      begin
        Result := 'KIResMed';
      end;
    ktSmall:
      begin;
        Result := 'KISmall';
      end;
    ktReset:
      begin
        Result := 'KIReset';
      end;
    ktSlider:
      begin;
        Result := 'SldI';
      end;
    ktSeqSlider:
      begin;
        Result := 'SeqSldI';
      end;
    ktHSlider:
      begin
        Result := 'SldHI';
      end;
  end;
end;

function GetKnobTexID(const aKnobType: TKnobType): string;
begin
  case aKnobType of
    ktBig:
      begin;
        Result := 'KBig';
      end;
    ktMedium, ktResetMedium:
      begin;
        Result := 'KMed';
      end;
    ktSmall, ktReset:
      begin;
        Result := 'KSmall';
      end;
    ktSlider:
      begin;
        Result := 'Sld';
      end;
    ktSeqSlider:
      begin;
        Result := 'SeqSld';
      end;
    ktHSlider:
      begin
        Result := 'SldH';
      end;
  end;
end;

function GetLedTexID(const aSeqLedType: boolean): string;
begin
  if aSeqLedType then
    Result := 'LedSeq'
  else
    Result := 'LedGreen';
end;

function GetMiniVUTexID: string;
begin
  Result := 'MiniVU';
end;

function GetPartSelectorBtnTexID(const aWidth,
  aHeight: single): string;
var
  Temp: string;
begin
  Temp :=
      'SelBtn'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;

function GetPartSelectorListTexID(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string): string;
var
  Temp: string;
begin
  Temp :=
      'SelList'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));
  if aImageIDs <> '' then
    Temp := Temp + ';' + aImageIDs
  else
    Temp := Temp + ';' + aTexts;

  Result := ReplaceSpecialChars(Temp);
end;

{function GetTextEditTexID(const aWidth, aHeight: single): string;
var
  Temp: string;
begin
  Temp :=
      'TxtEdit'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;}

function GetTextFieldTexID(const aWidth, aHeight: single): string;
var
  Temp: string;
begin
  Temp :=
      'TxtFld'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;

function GetGraphTexID(const aWidth, aHeight: single): string;
var
  Temp: string;
begin
  Temp :=
      'Graph'
      + IntToStr(Trunc(aWidth)) + 'x'
      + IntToStr(Trunc(aHeight));

  Result := ReplaceSpecialChars(Temp);
end;

function GetFontTexID: string;
begin
  Result := 'RobotoMono';
end;

function GetLabelTexID(const aFontSize: integer; const aCaption: string): string;
begin
  Result := 'Lb' + IntToStr(aFontSize) + ';' + ReplaceSpecialChars(aCaption);
end;

{ TTiledTexture }

constructor TTiledTexture.Create;
begin
  inherited;
  FID := '';
  FRefCount := 0;
end;

procedure TTiledTexture.CreateBtnFlat(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string);
var
  i: integer;
  sl: TStringList;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      Canvas.Fill.Color := TAlphaColorRec.White;

      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.FillRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      Canvas.DrawRect(R2, 1*SCALE_Y, 1*SCALE_Y, AllCorners, 1);

      if aImageIDs <> '' then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Black;
        DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aRow])]);
      end
      else
      begin
        Canvas.Font.Family := FONT_FAMILY;
        Canvas.Font.Size := 8 * SCALE_Y;
        Canvas.Fill.Color := TAlphaColorRec.Black;
        Canvas.FillText(R, sl[aRow], False, 1, [], TTextAlign.Center,
          TTextAlign.Center);
      end;

    finally
      Canvas.EndScene;
    end;
  end;

begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    if aImageIDs <> '' then
    begin
      sl.DelimitedText := aImageIDs;
    end
    else
    begin
      sl.DelimitedText := aTexts;
    end;

    Cols := 1;
    Rows := sl.Count;

    TileWidth := Trunc(SCALE_X * aWidth);
    TileHeight := Trunc(SCALE_Y * aHeight);
    SetSize(TileWidth * Cols, TileHeight * Rows);

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateBtnIncDec(const aWidth, aHeight: single;
  const aHorizontal: boolean);
var
  i: integer;
  sl: TStringList;
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aCol, aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(aCol * TileWidth, dy, aCol * TileWidth + TileWidth,
        dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      Canvas.FillRect(R, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Black;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Blue;
           end;
      end;
      DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aCol])]);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 2;
  Rows := 2;
  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  d := SCALE_X * 1;
  SetSize(TileWidth * Cols, TileHeight * Rows);

  sl := TStringList.Create;
  try
    if aHorizontal then
    begin
      sl.Add('2');
      sl.Add('3');
    end else begin
      sl.Add('0');
      sl.Add('1');
    end;

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i, 0);
      DrawBtn(i, 1);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateBtnLevelShift(const aWidth, aHeight: single;
  const aImageIDs: string);
var
  i: integer;
  sl: TStringList;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      Canvas.Fill.Color := TAlphaColorRec.White;

      Canvas.FillRect(R, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      R2 := R;
      R2.Inflate(-d/2, -d/2);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);

      Canvas.Fill.Color := TAlphaColorRec.Black;
      DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aRow])]);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aheight);

  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    sl.DelimitedText := aImageIDs;

    Cols := 1;
    Rows := sl.Count;

    SetSize(TileWidth * Cols, TileHeight * Rows);

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateBtnRadio(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string);
var
  i: integer;
  sl: TStringList;
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aCol, aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(aCol * TileWidth, dy, aCol * TileWidth + TileWidth,
        dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      Canvas.FillRect(R, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);

      case aRow of
        0:
          Canvas.Fill.Color := TAlphaColorRec.Black;
        1:
          Canvas.Fill.Color := TAlphaColorRec.Blue;
      end;

      if aImageIDs <> '' then
      begin
        DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aCol])]);
      end
      else
      begin
        Canvas.Font.Family := FONT_FAMILY;
        Canvas.Font.Size := 8 * SCALE_Y;
        Canvas.FillText(R, sl[aCol], False, 1, [], TTextAlign.Center,
          TTextAlign.Center);
      end;

    finally
      Canvas.EndScene;
    end;
  end;

begin
  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    if aImageIDs <> '' then
      sl.DelimitedText := aImageIDs
    else
      sl.DelimitedText := aTexts;

    Cols := sl.Count;
    Rows := 2;

    d := SCALE_X * 1;
    TileWidth := Trunc(SCALE_X * aWidth);
    TileHeight := Trunc(SCALE_Y * aHeight);
    SetSize(TileWidth * Cols, TileHeight * Rows);

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i, 0);
      DrawBtn(i, 1);
    end;
  finally
    sl.Free;
  end;
end;

{procedure TTiledTexture.CreateBtnRadioEdit(const aWidth, aHeight: single);
var
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.FillRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  d := SCALE_X * 1;
  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBtn(0);
  DrawBtn(1);
end;

procedure TTiledTexture.CreateTextEdit(const aWidth, aHeight: single);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.Black;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      Canvas.FillRect(R, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBtn(0);
  DrawBtn(1);
end;}

procedure TTiledTexture.CreateBtn(const aWidth, aHeight: single);
var
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.FillRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      Canvas.DrawRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  d := SCALE_X * 1;
  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBtn(0);
  DrawBtn(1);
end;

procedure TTiledTexture.CreateBtnText(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.Black;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.FillRect(R2, 1*SCALE_X, 1*SCALE_X, AllCorners, 1);
      Canvas.DrawRect(R2, 1*SCALE_Y, 1*SCALE_Y, AllCorners, 1);

      case aRow of
        0:
          Canvas.Fill.Color := TAlphaColorRec.Black;
        1:
          Canvas.Fill.Color := TAlphaColorRec.Blue;
      end;

      if aImageIDs <> '' then
      begin
        sl.Delimiter := ';';
        sl.StrictDelimiter := True;
        sl.DelimitedText := aImageIDs;
        if sl.Count > 0 then
        begin
          DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[0])]);
        end;
      end
      else
      begin
        sl.Delimiter := ';';
        sl.StrictDelimiter := True;
        sl.DelimitedText := aTexts;
        if sl.Count > 0 then
        begin
          Canvas.Font.Family := FONT_FAMILY;
          Canvas.Font.Size := 8 * SCALE_Y;
          Canvas.FillText(R, sl[0], False, 1, [], TTextAlign.Center,
            TTextAlign.Center);
        end;
      end;

    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBtn(0);
  DrawBtn(1);
end;

procedure TTiledTexture.CreateCenterBtn(const aRCntr: TRectF);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aRow: integer);
  var
    dy, d: single;
    poly: TPolygon;
  begin
    Canvas.BeginScene;
    try
      dy := TileHeight * aRow;
      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      case aRow of
        0: begin
            Canvas.Fill.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
            Canvas.Fill.Color := TAlphaColorRec.Blue;
           end;
      end;

      SetLength(poly, 4);
      poly[0] := PointF(0 , dy);
      poly[1] := PointF(TileWidth, dy);
      poly[2] := PointF(TileWidth/2, TileHeight + dy);
      poly[3] := PointF(0 , dy);

      Canvas.FillPolygon(poly, 1);
    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  TileWidth := Trunc(SCALE_X * aRCntr.Width);
  TileHeight := Trunc(SCALE_Y * aRCntr.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBtn(0);
  DrawBtn(1);
end;

procedure TTiledTexture.CreateConnectorIn;
var
  i: integer;
  d: single;
  OuterSize, InnerSize: single;
  OuterRect, InnerRect, R2: TRectF;
  Cols, Rows, TileWidth, TileHeight: integer;
begin
  Cols := 4;
  Rows := 2;
  OuterSize := SCALE_X * 11;
  InnerSize := OuterSize / 4;

  d := SCALE_X * 1;

  TileWidth := Round(OuterSize);
  TileHeight := Round(OuterSize);
  SetSize(TileWidth * Cols, TileHeight * Rows);
  Clear(0);

  Canvas.BeginScene;
  try
    OuterRect := RectF(0, 0, OuterSize, OuterSize);
    InnerRect := RectF((OuterSize - InnerSize), (OuterSize - InnerSize),
      InnerSize, InnerSize);

    for i := 0 to 3 do
    begin
      Canvas.Fill.Color := ConvertToAlpha(CableColors[i]);
      Canvas.FillEllipse(OuterRect, 1);
      Canvas.Fill.Color := Darker(ConvertToAlpha(CableColors[i]), 128);
      Canvas.Stroke.Color := Canvas.Fill.Color;
      R2 := OuterRect;
      R2.Inflate(-d/2, -d/2);
      Canvas.DrawEllipse(R2, 1);
      Canvas.FillEllipse(InnerRect, 1);

      OuterRect.Left := OuterRect.Left + OuterSize;
      OuterRect.Right := OuterRect.Right + OuterSize;

      InnerRect.Left := InnerRect.Left + OuterSize;
      InnerRect.Right := InnerRect.Right + OuterSize;
    end;

    OuterRect := RectF(0, OuterSize, OuterSize, OuterSize * 2);
    InnerRect := RectF((OuterSize - InnerSize), (OuterSize - InnerSize) +
      OuterSize, InnerSize, InnerSize + OuterSize);

    for i := 0 to 3 do
    begin
      Canvas.Fill.Color := ConvertToAlpha(CableColors[i]);
      Canvas.FillEllipse(OuterRect, 1);
      Canvas.Stroke.Color := Darker(ConvertToAlpha(CableColors[i]), 128);
      R2 := OuterRect;
      R2.Inflate(-d/2, -d/2);
      Canvas.DrawEllipse(R2, 1);
      Canvas.DrawEllipse(InnerRect, 1);

      OuterRect.Left := OuterRect.Left + OuterSize;
      OuterRect.Right := OuterRect.Right + OuterSize;

      InnerRect.Left := InnerRect.Left + OuterSize;
      InnerRect.Right := InnerRect.Right + OuterSize;
    end;

  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateConnectorOut;
var
  i: integer;
  OuterSize, InnerSize, d: single;
  OuterRect, InnerRect, R2: TRectF;
  Cols, Rows, TileWidth, TileHeight: integer;
begin
  Cols := 4;
  Rows := 2;
  OuterSize := SCALE_X * 11;
  InnerSize := OuterSize / 4;

  d := SCALE_X * 1;

  TileWidth := Round(OuterSize);
  TileHeight := Round(OuterSize);
  SetSize(TileWidth * Cols, TileHeight * Rows);
  Clear(0);

  Canvas.BeginScene;
  try
    OuterRect := RectF(0, 0, OuterSize, OuterSize);
    InnerRect := RectF((OuterSize - InnerSize), (OuterSize - InnerSize),
      InnerSize, InnerSize);

    for i := 0 to 3 do
    begin
      Canvas.Fill.Color := ConvertToAlpha(CableColors[i]);
      Canvas.FillRect(OuterRect, 0, 0, [], 1);
      Canvas.Fill.Color := Darker(ConvertToAlpha(CableColors[i]), 128);
      Canvas.Stroke.Color := Canvas.Fill.Color;
      R2 := OuterRect;
      R2.Inflate(-d/2, -d/2);
      Canvas.DrawRect(R2, 0, 0, [], 1);
      Canvas.FillEllipse(InnerRect, 1);

      OuterRect.Left := OuterRect.Left + OuterSize;
      OuterRect.Right := OuterRect.Right + OuterSize;

      InnerRect.Left := InnerRect.Left + OuterSize;
      InnerRect.Right := InnerRect.Right + OuterSize;
    end;

    OuterRect := RectF(0, OuterSize, OuterSize, OuterSize * 2);
    InnerRect := RectF((OuterSize - InnerSize), (OuterSize - InnerSize) +
      OuterSize, InnerSize, InnerSize + OuterSize);

    for i := 0 to 3 do
    begin
      Canvas.Fill.Color := ConvertToAlpha(CableColors[i]);
      Canvas.FillRect(OuterRect, 0, 0, [], 1);
      Canvas.Stroke.Color := Darker(ConvertToAlpha(CableColors[i]), 128);
      R2 := OuterRect;
      R2.Inflate(-d/2, -d/2);
      Canvas.DrawRect(R2, 0, 0, [], 1);
      Canvas.DrawEllipse(InnerRect, 1);

      OuterRect.Left := OuterRect.Left + OuterSize;
      OuterRect.Right := OuterRect.Right + OuterSize;

      InnerRect.Left := InnerRect.Left + OuterSize;
      InnerRect.Right := InnerRect.Right + OuterSize;
    end;

  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateFont(aFont: TFont);
var
  i: integer;
  R: TRectF;
  Cols, Rows, TileWidth, TileHeight: integer;
begin
  SetSize(10, 10);
  Canvas.Font.Assign(aFont);
  Cols := 12;
  Rows := 8;
  TileWidth := Round(Canvas.TextWidth('W'));
  TileHeight := Round(Canvas.TextHeight('|P2j'));
  SetSize(TileWidth * Cols, TileHeight * Rows);

  Canvas.Font.Assign(aFont);

  Canvas.BeginScene;
  try
    Clear(0);

    Canvas.Fill.Color := TAlphaColorRec.Black;
    Canvas.Fill.Kind := TBrushKind.Solid;

    for i := 32 to 128 do
    begin
      R.Left := ((i - 32) mod 12) * TileWidth;
      R.Top := ((i - 32) div 12) * TileHeight;
      R.Width := TileWidth;
      R.Height := TileHeight;

      Canvas.FillText(R, Char(i), False, 1, [], TTextAlign.Center);
    end;
  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateGraph(const aWidth, aHeight: single);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBack(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      Canvas.Fill.Color := TAlphaColorRec.Blue;
      Canvas.Stroke.Color := TAlphaColorRec.DarkGray;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBack(0);
end;

procedure TTiledTexture.CreateKnobIndicator(const aRDial, aRKnob: TRectF);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawKnobIndicator(aRow: integer);
  var
    dy, d, AngleStart, AngleEnd, Range,
    MidX, MidY, RxStart, RxEnd, RyStart, RyEnd: single;
    R: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      Canvas.Fill.Color := 0;
      Canvas.FillRect(R, 0, 0, [], 1);

      R := RectF(
        aRKnob.Left,
        dy + aRKnob.Top,
        aRKnob.Right,
        dy + aRKnob.Bottom);

      d := SCALE_X * 1;
      R.Inflate(-d / 2, -d / 2);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;
      Canvas.Fill.Color := 0;

      Canvas.FillEllipse(R, 1);
      Canvas.DrawEllipse(R, 1);

      Range := 2*PI - 55/180*PI;

      AngleStart := -(2*PI - Range) / 2;
      AngleEnd := -(2*PI - Range) / 2 -  Range;

      MidX := R.Left + R.Width / 2;
      MidY := R.Top + R.Height / 2;

      RxStart := R.Width / 2;
      RyStart := R.Width / 2;

      RxEnd := RxStart + 2 * SCALE_X;
      RyEnd := RyStart + 2 * SCALE_Y;

      Canvas.DrawLine(
        PointF(MidX - RxStart * Sin(AngleStart), MidY + RyStart * Cos(AngleStart)),
        PointF(MidX - RxEnd * Sin(AngleStart), MidY + RyEnd * Cos(AngleStart)), 1);

      Canvas.DrawLine(
        PointF(MidX - RxStart * Sin(AngleEnd), MidY + dy + RyStart * Cos(AngleEnd)),
        PointF(MidX - RxEnd * Sin(AngleEnd), MidY + dy + RyEnd * Cos(AngleEnd)), 1);
    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Round(aRDial.Width);
  TileHeight := Round(aRDial.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawKnobIndicator(0);
end;

procedure TTiledTexture.CreateKnob(const aRDial, aRKnob: TRectF);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawKnob(aRow: integer);
  var
    dy, d, Rx, Ry: single;
    R: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;
      R.Inflate(-d / 2, -d / 2);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;
      Canvas.Fill.Color := TAlphaColorRec.White;

      Canvas.FillEllipse(R, 1);
      Canvas.DrawEllipse(R, 1);

      Rx := TileWidth / 2;
      Ry := TileHeight / 2;

      Canvas.DrawLine(
        PointF(Rx, Ry + dy),
        PointF(Rx, Ry + dy + Ry), 1);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Round(aRKnob.Width);
  TileHeight := Round(aRKnob.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);
  DrawKnob(0);
end;

procedure TTiledTexture.CreateKnobBtns(const aRBtns: TRectF);
var
  i: integer;
  sl: TStringList;
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aCol, aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      d := SCALE_X * 1;

      R := RectF(aCol * TileWidth, dy, aCol * TileWidth + TileWidth,
        dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Black;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Blue;
           end;
      end;
      DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aCol])]);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 2;
  Rows := 2;

  d := SCALE_X * 1;
  TileWidth := Trunc(SCALE_X * aRBtns.Width / 2);
  TileHeight := Trunc(SCALE_Y * aRBtns.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  sl := TStringList.Create;
  try
    sl.Add('2');
    sl.Add('3');

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i, 0);
      DrawBtn(i, 1);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateSlider(const aRDial, aRKnob, aRBtns: TRectF;
  const aHorizontal: boolean);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawSlider(aRow: integer);
  var
    d: single;
    R: TRectF;
  begin
    Canvas.BeginScene;
    try
      d := SCALE_X * 1;

      Canvas.Stroke.Color := TAlphaColorRec.Darkgray;
      Canvas.Fill.Color := TAlphaColorRec.Black;

      {if aHorizontal then
      begin
        h := TileWidth;
        Range := (aRDial.Width - aRBtns.Width) - h;
        f := aRow / Rows;
      end else begin
        h := TileHeight;
        Range := (aRDial.Height - aRBtns.Height) - h;
        f := aRow / Rows;
        R := RectF(0, 0, TileWidth, h);
      end;}

      R := RectF(0, 0, TileWidth, TileHeight);
      R.Inflate(-d / 2, -d / 2);

      Canvas.FillRect(R, 0, 0, [], 1);
    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Round(aRKnob.Width);
  TileHeight := Round(aRKnob.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);
  DrawSlider(0);
end;

procedure TTiledTexture.CreateSliderBtns(const aRBtns: TRectF; const aHorizontal: boolean);
var
  i: integer;
  sl: TStringList;
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aCol, aRow: integer);
  var
    dy: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(aCol * TileWidth, dy, aCol * TileWidth + TileWidth,
        dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Lightgray;
             Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Cyan;
             Canvas.Stroke.Color := TAlphaColorRec.Blue;
           end;
      end;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);

      case aRow of
        0: begin
             Canvas.Fill.Color := TAlphaColorRec.Black;
           end;
        1: begin
             Canvas.Fill.Color := TAlphaColorRec.Blue;
           end;
      end;

      DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aCol])]);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 2;
  Rows := 2;

  d := SCALE_X * 1;
  sl := TStringList.Create;
  try

    if aHorizontal then
    begin
      TileWidth := Trunc(SCALE_X * aRBtns.Width / 2);
      TileHeight := Trunc(SCALE_Y * aRBtns.Height);
      sl.Add('2');
      sl.Add('3');
    end else begin
      TileWidth := Trunc(SCALE_X * aRBtns.Width);
      TileHeight := Trunc(SCALE_Y * aRBtns.Height / 2);
      sl.Add('0');
      sl.Add('1');
    end;
    SetSize(TileWidth * Cols, TileHeight * Rows);

    for i := 0 to sl.Count - 1 do
    begin
      DrawBtn(i, 0);
      DrawBtn(i, 1);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateSliderIndicator(const aRDial, aRKnob,
  aRBtns: TRectF; const aHorizontal: boolean);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawSliderIndicator(aRow: integer);
  var
    dy, d: single;
    R: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      Canvas.Fill.Color := 0;
      Canvas.FillRect(R, 0, 0, [], 1);

      if aHorizontal then
        R := RectF(0, dy, TileWidth - aRBtns.Width, dy + TileHeight)
      else
        R := RectF(0, dy, TileWidth, dy + TileHeight - aRBtns.Width);

      Canvas.Fill.Color := TAlphaColorRec.Lightgray;
      Canvas.FillRect(R, 0, 0, [], 1);

      d := SCALE_X * 1;
      R.Inflate(-d / 2, -d / 2);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Darkgray;

      Canvas.DrawRect(R, 0, 0, [], 1);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Round(aRDial.Width);
  TileHeight := Round(aRDial.Height);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawSliderIndicator(0)
end;

procedure TTiledTexture.CreateLabel(const aFontFamily: string;
  const aFontSize: integer; const aFontStyle: TFontStyles;
  const aCaption: string);
var
  w, h: single;
begin
  SetSize(10, 10);

  Canvas.BeginScene;
  try
    Canvas.Font.Family := aFontFamily;
    Canvas.Font.Size := (aFontSize - 2) * SCALE_Y;
    Canvas.Font.Style := aFontStyle;
    w := Canvas.TextWidth(aCaption);
    h := Canvas.TextHeight(aCaption);
  finally
    Canvas.EndScene;
  end;

  if w <= 0 then
    w := 10;

  if h <= 0 then
    h := 10;

  SetSize(Round(w), Round(h));

  Canvas.BeginScene;
  try
    Canvas.Font.Family := FONT_FAMILY;
    Canvas.Font.Size := (aFontSize - 2) * SCALE_Y;
    Canvas.Font.Style := aFontStyle;
    Canvas.Clear(0);
    Canvas.Fill.Color := TAlphaColorRec.Black;
    Canvas.FillText(
      RectF(0, 0, w, h),
      aCaption, False, 1, [],
      TTextAlign.Leading,
      TTextAlign.Center);
  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateLedGreen;
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawLed(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      case aRow of
        0:
          begin
            Canvas.Fill.Color := TAlphaColorRec.Black;
            Canvas.Stroke.Color := TAlphaColorRec.Gray;
          end;
        1:
          begin
            Canvas.Fill.Color := TAlphaColorRec.Lime;
            Canvas.Stroke.Color := TAlphaColorRec.Green;
          end;
      end;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);

    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * 8;
  TileHeight := SCALE_Y * 8;
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawLed(0);
  DrawLed(1);
end;

procedure TTiledTexture.CreateLedSequencer;
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawLed(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
  begin
    dy := TileHeight * aRow;

    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      case aRow of
        0:
          begin
            Canvas.Fill.Color := TAlphaColorRec.Black;
            Canvas.Stroke.Color := TAlphaColorRec.Gray;
          end;
        1:
          begin
            Canvas.Fill.Color := TAlphaColorRec.Lime;
            Canvas.Stroke.Color := TAlphaColorRec.Green;
          end;
      end;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * 12;
  TileHeight := SCALE_Y * 7;
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawLed(0);
  DrawLed(1);
end;

procedure TTiledTexture.CreateMenuLeft;
var
  Points: TPolygon;
  TileWidth, TileHeight: integer;
  d: Single;
begin
  TileWidth := SCALE_X * 12;
  TileHeight := SCALE_Y * 16;
  SetSize(TileWidth, TileHeight);

  Canvas.BeginScene;
  try
    d := SCALE_X * 1;

    Canvas.StrokeThickness := d;
    Canvas.Clear(0);
    Canvas.Stroke.Color := TAlphaColorRec.Black;

    SetLength(Points, 3);
    Points[0].X := TileWidth;
    Points[0].Y := 0;

    Points[1].X := 0;
    Points[1].Y := TileHeight / 2;

    Points[2].X := TileWidth;
    Points[2].Y := TileHeight;

    Canvas.DrawPolygon(Points, 1);
  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateMenuRight;
var
  Points: TPolygon;
  TileWidth, TileHeight: integer;
  d: Single;
begin
  TileWidth := SCALE_X * 12;
  TileHeight := SCALE_Y * 16;
  SetSize(TileWidth, TileHeight);

  Canvas.BeginScene;
  try
    d := SCALE_X * 1;

    Canvas.StrokeThickness := d;
    Canvas.Clear(0);
    Canvas.Stroke.Color := TAlphaColorRec.Black;

    SetLength(Points, 3);
    Points[0].X := 0;
    Points[0].Y := 0;

    Points[1].X := TileWidth;
    Points[1].Y := TileHeight / 2;

    Points[2].X := 0;
    Points[2].Y := TileHeight;

    Canvas.DrawPolygon(Points, 1);
  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreateMiniVU;
var
  i: integer;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawMiniVU(aRow: integer);
  var
    dy, d: single;
    R: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      // Level green
      if aRow < 8 then
      begin
        R := RectF(0, dy + TileHeight - (aRow * SCALE_Y), TileWidth,
          dy + TileHeight);
        Canvas.Fill.Color := TAlphaColorRec.Lime;
        Canvas.FillRect(R, 0, 0, [], 1);

        R := RectF(0, dy + TileHeight - (8 * SCALE_Y), TileWidth,
          dy + TileHeight - (aRow * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Green;
        Canvas.FillRect(R, 0, 0, [], 1);
      end
      else
      begin
        R := RectF(0, dy + TileHeight - (8 * SCALE_Y), TileWidth,
          dy + TileHeight);
        if aRow >= 8 then
          Canvas.Fill.Color := TAlphaColorRec.Lime
        else
          Canvas.Fill.Color := TAlphaColorRec.Green;
        Canvas.FillRect(R, 0, 0, [], 1);
      end;

      // Level yellow

      if (aRow > 8) and (aRow < 12) then
      begin
        R := RectF(0, dy + TileHeight - (aRow * SCALE_Y), TileWidth,
          dy + TileHeight - (8 * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Yellow;
        Canvas.FillRect(R, 0, 0, [], 1);

        R := RectF(0, dy + TileHeight - (12 * SCALE_Y), TileWidth,
          dy + TileHeight - (aRow * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Olive;
        Canvas.FillRect(R, 0, 0, [], 1);
      end
      else
      begin
        R := RectF(0, dy + TileHeight - (12 * SCALE_Y), TileWidth,
          dy + TileHeight - (8 * SCALE_Y));
        if aRow >= 12 then
          Canvas.Fill.Color := TAlphaColorRec.Yellow
        else
          Canvas.Fill.Color := TAlphaColorRec.Olive;
        Canvas.FillRect(R, 0, 0, [], 1);
      end;

      if (aRow > 12) and (aRow < 16) then
      begin
        R := RectF(0, dy + TileHeight - (aRow * SCALE_Y), TileWidth,
          dy + TileHeight - (12 * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(R, 0, 0, [], 1);

        R := RectF(0, dy + TileHeight - (15 * SCALE_Y), TileWidth,
          dy + TileHeight - (aRow * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Maroon;
        Canvas.FillRect(R, 0, 0, [], 1);
      end
      else
      begin
        R := RectF(0, dy + TileHeight - (15 * SCALE_Y), TileWidth,
          dy + TileHeight - (12 * SCALE_Y));
        Canvas.Fill.Color := TAlphaColorRec.Maroon;
        Canvas.FillRect(R, 0, 0, [], 1);
      end;
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 16;
  TileWidth := SCALE_X * 8;
  TileHeight := SCALE_Y * 15;
  SetSize(TileWidth * Cols, TileHeight * Rows);

  for i := 0 to 15 do
    DrawMiniVU(i);
end;

procedure TTiledTexture.CreateModuleSegment;
var
  Rows, Cols, TileWidth, TileHeight: integer;

  procedure DrawModule(aRow: integer; aColor: TAlphaColor);
  var
    R: TRectF;
    dy, d: single;
  begin
    dy := TileHeight * aRow;
    R := RectF(0, dy, TileWidth, dy + TileHeight);

    Canvas.BeginScene;
    try
      Canvas.Stroke.Color := aColor;
      Canvas.Fill.Color := aColor;
      d := SCALE_X * 2;
      Canvas.StrokeThickness := d;
      R.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R, 0, 0, [], 1);
    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * UNITS_COL;
  TileHeight := SCALE_Y * UNITS_ROW * 2;
  SetSize(
    TileWidth * Cols,
    TileHeight * Rows);

  Clear(0);
  DrawModule(0, TAlphaColorRec.Black); // Normal
  DrawModule(1, TAlphaColorRec.White); // Selected
end;

procedure TTiledTexture.CreateModuleSegmentBottom;
var
  Rows, Cols, TileWidth, TileHeight: integer;

  procedure DrawModule(aRow: integer; aColor: TAlphaColor);
  var
    R: TRectF;
    dy, d: single;
    PathData: TPathData;
  begin
    d := SCALE_X * 2;
    dy := TileHeight * aRow;
    R := RectF(0, dy, TileWidth, dy + TileHeight);

    PathData := TPathData.Create;
    Canvas.BeginScene;
    try
      Canvas.Stroke.Color := aColor;
      Canvas.Fill.Color := aColor;
      Canvas.StrokeThickness := d;
      R.Inflate(-d / 2, -d / 2);
      PathData.MoveTo(PointF(R.Left, R.Top));
      PathData.LineTo(PointF(R.Left, R.Bottom));
      PathData.LineTo(PointF(R.Right, R.Bottom));
      PathData.LineTo(PointF(R.Right, R.Top));

      Canvas.DrawPath(PathData, 1);
    finally
      PathData.Free;
      Canvas.EndScene;
    end;
  end;

begin
  // For modules one unit high

  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * UNITS_COL;
  TileHeight := SCALE_Y * UNITS_ROW;
  SetSize(
    TileWidth * Cols,
    TileHeight * Rows);

  Clear(0);
  DrawModule(0, TAlphaColorRec.Black); // Normal
  DrawModule(1, TAlphaColorRec.White); // Selected
end;

procedure TTiledTexture.CreateModuleSegmentMiddle;
var
  Rows, Cols, TileWidth, TileHeight: integer;

  procedure DrawModule(aRow: integer; aColor: TAlphaColor);
  var
    R: TRectF;
    dy, d: single;
  begin
    d := SCALE_X * 2;
    dy := TileHeight * aRow;
    R := RectF(0, dy, TileWidth, dy + TileHeight);

    Canvas.BeginScene;
    try
      Canvas.Stroke.Color := aColor;
      Canvas.Fill.Color := aColor;
      Canvas.StrokeThickness := d;
      R.Inflate(-d / 2, -d / 2);
      Canvas.DrawLine(
        PointF(R.Left, R.Top),
        PointF(R.Left, R.Bottom),
        1);
      Canvas.DrawLine(
        PointF(R.Right, R.Top),
        PointF(R.Right, R.Bottom),
        1);
    finally
      Canvas.EndScene;
    end;
  end;

begin
  // For modules one unit high

  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * UNITS_COL;
  TileHeight := SCALE_Y * UNITS_ROW;
  SetSize(
    TileWidth * Cols,
    TileHeight * Rows);

  Clear(0);
  DrawModule(0, TAlphaColorRec.Black); // Normal
  DrawModule(1, TAlphaColorRec.White); // Selected
end;

procedure TTiledTexture.CreateModuleSegmentTop;
var
  Rows, Cols, TileWidth, TileHeight: integer;

  procedure DrawModule(aRow: integer; aColor: TAlphaColor);
  var
    R: TRectF;
    dy, d: single;
    PathData: TPathData;
  begin
    d := SCALE_X * 2;
    dy := TileHeight * aRow;
    R := RectF(0, dy, TileWidth, dy + TileHeight);

    PathData := TPathData.Create;
    Canvas.BeginScene;
    try
      Canvas.Stroke.Color := aColor;
      Canvas.Fill.Color := aColor;
      Canvas.StrokeThickness := d;
      R.Inflate(-d / 2, -d / 2);
      PathData.MoveTo(PointF(R.Left, R.Bottom));
      PathData.LineTo(PointF(R.Left, R.Top));
      PathData.LineTo(PointF(R.Right, R.Top));
      PathData.LineTo(PointF(R.Right, R.Bottom));

      Canvas.DrawPath(PathData, 1);
    finally
      PathData.Free;
      Canvas.EndScene;
    end;
  end;

begin
  // For modules one unit high

  Cols := 1;
  Rows := 2;
  TileWidth := SCALE_X * UNITS_COL;
  TileHeight := SCALE_Y * UNITS_ROW;
  SetSize(
    TileWidth * Cols,
    TileHeight * Rows);

  Clear(0);
  DrawModule(0, TAlphaColorRec.Black); // Normal
  DrawModule(1, TAlphaColorRec.White); // Selected
end;

procedure TTiledTexture.CreatePartSelectorBtn(const aWidth, aHeight: single);
var
  d: single;
  BtnWidth: single;
  RBtn, R2: TRectF;
  Cols, Rows, TileWidth, TileHeight: integer;
begin
  Cols := 1;
  Rows := 1;

  d := SCALE_X * 1;
  BtnWidth := aWidth;
  TileWidth := Trunc(SCALE_X * BtnWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  RBtn := RectF(0, 0, TileWidth, TileHeight);

  Canvas.BeginScene;
  try
    Canvas.StrokeThickness := d;
    Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
    Canvas.Fill.Color := TAlphaColorRec.Lightgray;

    Canvas.FillRect(RBtn, 0, 0, [], 1);
    R2 := RBtn;
    R2.Inflate(-d / 2, -d / 2);
    Canvas.DrawRect(R2, 0, 0, [], 1);

    Canvas.Fill.Color := TAlphaColorRec.Black;
    DrawSymbol(Canvas, RBtn, G2SymbolData[1]);
  finally
    Canvas.EndScene;
  end;
end;

procedure TTiledTexture.CreatePartSelectorList(const aWidth, aHeight: single;
  const aImageIDs, aTexts: string);
var
  i: integer;
  sl: TStringList;
  d: single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBtn(aCol, aRow: integer);
  var
    R, R2: TRectF;
    dy: single;
  begin
    Canvas.BeginScene;
    try
      dy := TileHeight * aRow;

      R := RectF(aCol * TileWidth, dy, aCol * TileWidth + TileWidth,
        dy + TileHeight);

      Canvas.StrokeThickness := d;
      Canvas.Stroke.Color := TAlphaColorRec.Black;

      case aRow of
      0: begin
           Canvas.Stroke.Color := TAlphaColorRec.DarkGray;
           Canvas.Fill.Color := TAlphaColorRec.Lightgray;
         end;
      1: begin
           Canvas.Stroke.Color := TAlphaColorRec.Blue;
           Canvas.Fill.Color := TAlphaColorRec.Cyan;
         end;
      end;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);

      case aRow of
      0: begin
           Canvas.Fill.Color := TAlphaColorRec.Black;
         end;
      1: begin
           Canvas.Fill.Color := TAlphaColorRec.Blue;
         end;
      end;

      if aImageIDs <> '' then
      begin
        DrawSymbol(Canvas, R, G2SymbolData[StrToInt(sl[aCol])]);
      end
      else
      begin
        Canvas.Font.Family := FONT_FAMILY;
        Canvas.Font.Size := 8 * SCALE_Y;
        Canvas.FillText(R, sl[aCol], False, 1, [], TTextAlign.Center,
          TTextAlign.Center);
      end;

    finally
      Canvas.EndScene;
    end;
  end;

begin
  d := SCALE_X * 0;
  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);

  sl := TStringList.Create;
  try
    sl.Delimiter := ';';
    sl.StrictDelimiter := True;
    if aImageIDs <> '' then
      sl.DelimitedText := aImageIDs
    else
      sl.DelimitedText := aTexts;

    Cols := sl.Count;
    Rows := 2;

    SetSize(TileWidth * Cols, TileHeight * Rows);

    for i := 0 to sl.Count - 1 do begin
      DrawBtn(i, 0);
      DrawBtn(i, 1);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTiledTexture.CreateRackRails;
var
  R: TRectF;
  w, d: single;
  Cols, Rows, TileWidth, TileHeight: integer;
begin
  Cols := 1;
  Rows := 1;
  TileWidth := GRID_UNITS_X;
  TileHeight := GRID_UNITS_Y;
  SetSize(TileWidth * Cols, TileHeight * Rows);

  Canvas.BeginScene;
  try

    R := RectF(0, 0, TileWidth, TileHeight);
    Canvas.Fill.Color := $FF483E37;//$FF141C1C;
    Canvas.FillRect(R, 0, 0, [], 1);

    w := TileHeight;

    R := RectF(1, 0, w + 1, TileHeight);
    Canvas.Fill.Color := TAlphaColorRec.Silver;
    Canvas.FillRect(R, 0, 0, [], 1);

    R := RectF(TileWidth - w, 0, TileWidth, TileHeight);
    Canvas.Fill.Color := TAlphaColorRec.Silver;
    Canvas.FillRect(R, 0, 0, [], 1);

    d := w * 0.25;

    R := RectF(1 + w/2 - d, TileHeight / 2 - d, 1 + w/2 + d, TileHeight / 2 + d);
    Canvas.Fill.Color := $FF483E37;
    Canvas.FillEllipse(R, 1);

    R := RectF(TileWidth - w/2 - d, TileHeight / 2 - d, TileWidth - w/2 + d, TileHeight / 2 + d);
    Canvas.FillEllipse(R, 1);
  finally
    Canvas.EndScene;
  end;
end;


procedure TTiledTexture.CreateTextField(const aWidth, aHeight: single);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawBack(aRow: integer);
  var
    dy, d: single;
    R, R2: TRectF;
    sl: TStringList;
  begin
    dy := TileHeight * aRow;

    sl := TStringList.Create;
    Canvas.BeginScene;
    try
      R := RectF(0, dy, TileWidth, dy + TileHeight);

      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      Canvas.Fill.Color := TAlphaColorRec.Lightgray;
      Canvas.Stroke.Color := TAlphaColorRec.DarkGray;

      Canvas.FillRect(R, 0, 0, [], 1);
      R2 := R;
      R2.Inflate(-d / 2, -d / 2);
      Canvas.DrawRect(R2, 0, 0, [], 1);
    finally
      Canvas.EndScene;
      sl.Free;
    end;
  end;

begin
  Cols := 1;
  Rows := 1;

  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawBack(0);
end;

procedure TTiledTexture.CreateBlackKey(aWidth, aHeight: single);
var
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawKey(aRow: integer);
  var
    dy, d: single;
    R: TRectF;
  begin
    Canvas.BeginScene;
    try
      dy := TileHeight * aRow;
      d := SCALE_X * 1;

      Canvas.StrokeThickness := d;

      case aRow of
        0: begin
            Canvas.Fill.Color := TAlphaColorRec.Black;
           end;
        1: begin
            Canvas.Fill.Color := TAlphaColorRec.Cyan;
           end;
      end;

      R := RectF(0, dy, TileWidth, dy + TileHeight);

      Canvas.FillRect(R, 0, 0, [], 1);

    finally
      Canvas.EndScene;
    end;
  end;

begin
  Cols := 1;
  Rows := 2;

  TileWidth := Trunc(SCALE_X * aWidth);
  TileHeight := Trunc(SCALE_Y * aHeight);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  DrawKey(0);
  DrawKey(1);
end;

procedure TTiledTexture.CreateWhiteKeys(aWidth, aHeight, aBlackWidth,
  aBlackHeight: single);
var
  KeyPath1, KeyPath2, KeyPath3 : TPathData;
  bl, wl, hbw, ww : single;
  Cols, Rows, TileWidth, TileHeight: integer;

  procedure DrawKey(aPath: TPathData; aCol: integer);

    procedure DrawKeyState(aRow: integer);
    var
      d: single;
      R: TRectF;
      SaveMatrix: TMatrix;
    begin
      R := aPath.GetBounds;

      Canvas.BeginScene;
      try
        SaveMatrix := Canvas.Matrix;

        Canvas.MultiplyMatrix(TMatrix.CreateTranslation(aCol * TileWidth, aRow * TileHeight));

        d := SCALE_X * 1/2;
        Canvas.StrokeThickness := d;

        case aRow of
          0: begin
              Canvas.Fill.Color := TAlphaColorRec.White;
              Canvas.Stroke.Color := TAlphaColorRec.Black;
             end;
          1: begin
              Canvas.Fill.Color := TAlphaColorRec.Cyan;
              Canvas.Stroke.Color := TAlphaColorRec.Black;
             end;
        end;

        Canvas.FillPath(aPath, 1);
        //aPath.Scale((R.Width - d/2)/R.Width, (R.Height - d/2)/R.Height);
        //aPath.Translate(d/2, d/2);
        Canvas.DrawPath(aPath, 1);

        Canvas.SetMatrix(SaveMatrix);
      finally
        Canvas.EndScene;
      end;
    end;

  begin
    DrawKeyState(0);
    DrawKeySTate(1);
  end;

begin
  Cols := 7;
  Rows := 2;

  bl := aBlackHeight * SCALE_X;
  wl := aHeight * SCALE_Y;
  hbw :=  aBlackWidth * SCALE_Y / 2;
  ww := aWidth * SCALE_X;

  TileWidth := Round(ww);
  TileHeight := Round(wl);
  SetSize(TileWidth * Cols, TileHeight * Rows);

  Canvas.BeginScene;
  try
    Canvas.Clear(0);
  finally
    Canvas.EndScene;
  end;

  KeyPath1 := TPathData.Create;
  KeyPath2 := TPathData.Create;
  KeyPath3 := TPathData.Create;
  try

    KeyPath1.Clear;
    KeyPath1.MoveTo( PointF( 0, 0));
    KeyPath1.LineToRel( PointF( 0, wl));
    KeyPath1.LineToRel( PointF( ww, 0));
    KeyPath1.LineToRel( PointF( 0, bl-wl));
    KeyPath1.LineToRel( PointF( -hbw, 0));
    KeyPath1.LineToRel( PointF( 0, -bl));
    KeyPath1.LineToRel( PointF( -ww + hbw, 0));
    KeyPath1.ClosePath;

    KeyPath2.Clear;
    KeyPath2.MoveTo( PointF( hbw, 0));
    KeyPath2.LineToRel( PointF( 0, bl));
    KeyPath2.LineToRel( PointF( -hbw, 0));
    KeyPath2.LineToRel( PointF( 0, wl-bl));
    KeyPath2.LineToRel( PointF( ww, 0));
    KeyPath2.LineToRel( PointF( 0, -wl+bl));
    KeyPath2.LineToRel( PointF( -hbw, 0));
    KeyPath2.LineToRel( PointF( 0, -bl));
    KeyPath2.LineToRel( PointF( -ww + hbw + hbw, 0));
    KeyPath2.ClosePath;

    KeyPath3.Clear;
    KeyPath3.MoveTo( PointF( hbw, 0));
    KeyPath3.LineToRel( PointF( 0, bl));
    KeyPath3.LineToRel( PointF( -hbw, 0));
    KeyPath3.LineToRel( PointF( 0, wl-bl));
    KeyPath3.LineToRel( PointF( ww, 0));
    KeyPath3.LineToRel( PointF( 0, -wl));
    KeyPath3.LineToRel( PointF( -ww + hbw, 0));
    KeyPath3.ClosePath;

    DrawKey(KeyPath1, 0);
    DrawKey(KeyPath2, 1);
    DrawKey(KeyPath3, 2);
    DrawKey(KeyPath1, 3);
    DrawKey(KeyPath2, 4);
    DrawKey(KeyPath2, 5);
    DrawKey(KeyPath3, 6);
  finally
    KeyPath3.Free;
    KeyPath2.Free;
    KeyPath1.Free;
  end;
end;

destructor TTiledTexture.Destroy;
begin
  inherited;
end;

function TTiledTexture.GetSize: integer;
begin
  Result := Width * Height * BytesPerPixel;
end;

function TTiledTexture.LoadFromCache(const aFileName: string): boolean;
begin
  if FileExists(aFileName) then
  begin
    LoadFromFile(aFileName);
    Result := True;
  end else
    Result := False;
end;

procedure TTiledTexture.SaveToCache(const aFileName: string);
begin
  SaveToFile(aFileName);
end;

{ TTextureListGL }

procedure TTextureListGL.Add(const aKey: string; aTexture: TTiledTexture);
var
  OldTexture: TTiledTexture;
  OldKey: string;
begin
  while (FUsedList.Count > 0) and (FSize + aTexture.Size > FMaxSize) do
  begin
    OldKey := FUsedList[0];
    if FMemItems.TryGetValue(OldKey, OldTexture) then
    begin
      FSize := FSize - OldTexture.Size;
      FMemItems.Remove(OldKey);
    end;
    FUsedList.Delete(0);
  end;

  FMemItems.Add(aKey, aTexture);
  FSize := FSize + aTexture.Size;
  aTexture.ID := aKey;
  FUsedList.Append(aKey);
end;

function TTextureListGL.ContainsKey(const aKey: string): boolean;
begin
  Result := FMemItems.ContainsKey(aKey);
end;

constructor TTextureListGL.Create(AOwner: TComponent);
begin
  inherited;

  FSize := 0;
  FMaxSize := 1000000;

{$IFDEF ANDROID}
  FCacheDir := System.IOUtils.TPath.GetCachePath;
{$ELSE}
  FCacheDir := 'Cache';
{$ENDIF}

  FMemItems := TObjectDictionary<string, TTiledTexture>.Create([doOwnsValues]);
  FUsedList := TStringList.Create;
end;

destructor TTextureListGL.Destroy;
begin
  FUsedList.Free;
  FMemItems.Free;
  inherited;
end;

function TTextureListGL.GetItem(const aKey: string): TTiledTexture;
begin
  Result := FMemItems[aKey];
end;

function TTextureListGL.Link(const aKey: string;
  var aTexture: TTiledTexture): boolean;
var
  i: integer;
begin
  Result := FMemItems.TryGetValue(aKey, aTexture);
  if not Result then
  begin
    Result := LoadFromCache(aKey, aTexture);
    if Result then
    begin
      Add(aKey, aTexture);
    end;
  end else begin
    // Move to end of used list
    i := FUsedList.IndexOf(aKey);
    if i > -1 then begin
      FUsedList.Delete(i);
    end;
    FUsedList.Append(aKey);
  end;

  if Result then
  begin
    aTexture.FRefCount := aTexture.FRefCount + 1;
  end;
end;

function TTextureListGL.LoadFromCache(const aTextureID: string;
  var aTexture: TTiledTexture): boolean;
var
  FileName: string;
begin
  FileName := TPath.Combine(FCacheDir, aTextureID + '.png');
  if FileExists(FileName) then
  begin
    aTexture := TTiledTexture.Create;
    aTexture.LoadFromCache(FileName);
    Result := True;
  end else
    Result := False;
end;

procedure TTextureListGL.SaveToCache(const aTexture: TTiledTexture);
var
  FileName: string;
begin
  FileName := TPath.Combine(FCacheDir, aTexture.ID + '.png');
  aTexture.SaveToCache(FileName);
end;

procedure TTextureListGL.LoadFromZip(const aFileName: string);
var
  Zip: TZipFile;
  i: integer;
  Texture: TTiledTexture;
  Stream: TStream;
  LocalHeader: TZipHeader;
  FileName: string;
  p: integer;
begin
  Zip := TZipFile.Create;
  try
    Zip.Open(aFileName, zmRead);
    for i := 0 to Zip.FileCount - 1 do
    begin
      Zip.Read(i, Stream, LocalHeader);
      try
        FileName := Zip.FileName[i];
        p := Pos('.', FileName);
        FileName := Copy(FileName, 1, p - 1);

        Texture := TTiledTexture.Create;

        Texture.LoadFromStream(Stream);

        FMemItems.Add(FileName, Texture);
      finally
        Stream.Free;
      end;
    end;
  finally
    Zip.Free;
  end;
end;

procedure TTextureListGL.SaveTextures(const aPath: string);
var
  Pair: TPair<string, TTiledTexture>;
begin
  for Pair in FMemItems do
    Pair.Value.SaveToFile(aPath + Pair.Key + '.png');
end;

procedure TTextureListGL.SetItem(const aKey: string;
  const Value: TTiledTexture);
begin
  FSize := FSize - FMemItems[aKey].Size;
  FMemItems[aKey] := Value;
  FSize := FSize + FMemItems[aKey].Size;
end;

function TTextureListGL.TextureFileName(const aTextureID: string): string;
begin
  Result := aTextureID + '.png';
end;

procedure TTextureListGL.UnLink(const aTextureID: string);
var
  Texture: TTiledTexture;
begin
  if FMemItems.TryGetValue(aTextureID, Texture) then
  begin
    Texture.FRefCount := Texture.FRefCount - 1;
    if Texture.FRefCount <= 0 then
    begin
      FSize := FSize - SizeOf(Texture);
      FMemItems.Remove(aTextureID);
    end;
  end;
end;

end.
