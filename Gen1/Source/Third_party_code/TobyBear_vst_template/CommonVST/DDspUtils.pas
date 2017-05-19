{
********************************************************************
DDspUtils.pas
A unit with some useful functions for DSP/audio processing!

(C)opyright 2001-2004 by Tobybear (tobybear@web.de)
Get the latest version at my site: www.tobybear.de

I AM NOT RESPONSIBLE FOR ANYTHING! USE AT YOUR OWN RISK!

VST is a registered trademark of Steinberg Hard- and Software GmbH
********************************************************************
}

unit DDspUtils;
interface
type
 TSingleArray = array of single;
 PSingleArray = ^TSingleArray;
 TArrayOfSingleArray = array of TSingleArray;
 PArrayOfSingleArray = ^TArrayOfSingleArray;
 TStrArray = array of AnsiString;

function GetApplicationFilename: String;
function GetApplicationDirectory: String;
function ms2smp(ms, srate: single): single;
function smp2ms(smp, srate: single): single;
function getSyncFactor(base_factor: single; dotted, triads: boolean): single;
function sync2smp(syncfactor, bpm, srate: single): longint;
function f_limit(v: Single; l: Single = -1; u: Single = 1): single;
function dB_to_Amp(g: single): single;
function Amp_to_dB(v: single): single;
function linear_interpolation(f, a, b: single): single;
function cubic_interpolation(fr, inm1, inp, inp1, inp2: single): single;
function f_frac(x: single): single;
function f_int(x: single): single;
function f_trunc(f: single): integer;
function f_round(f: single): integer;
function f_exp(x: single): single;
function f_ln2(f: single): single;
function f_floorln2(f: single): longint;
function f_abs(f: single): single;
function f_neg(f: single): single;
function f_root(i: single; n: integer): single;
function f_power(i: single; n: integer): single;
function f_log2(val: single): single;
function f_arctan(fValue: single): single;
function f_sin(fAngle: single): single;
function f_cos(fAngle: single): single;
function f_sgn(f: single): longint;
function f_clip(x, l, h: single): single;
function f_cliplo(x, l: single): single;
function f_cliphi(x, h: single): single;

// scale logarithmically from 20 Hz to 20 kHz
function FreqLinearToLog(value: single): single;
function FreqLogToLinear(value: single): single;

function OnOff(fvalue: single): boolean;
function undenormalize(fvalue: single): single;

function Saturate(input, fMax: single): single;

procedure Msg(b: boolean); overload;
procedure Msg(m: String; m2: String = ''); overload;
procedure Msg(i: integer); overload;
procedure Msg(s: single); overload;
procedure Msg(m: String; i: integer); overload;

function SplitString(S: AnsiString; Delimiter: Ansichar): TStrArray;
function MakeGoodFileName(s: AnsiString; replchar: Ansichar = '_'): AnsiString;
function FillStr(s: AnsiString; len: integer; FillChar: AnsiChar): AnsiString;

const
 LN2R = 1.442695041;
 kDenorm = 1.0e-24;
 _pi=3.1415926536;

implementation

uses Math, SysUtils, Windows;

function GetApplicationFilename:String;
var s: array[0..1500] of Char;
    st: String;
begin
 GetModuleFilename(hinstance, s, sizeof(s));
 st := StrPas(s);
 st := ExtractFilename(st);
 result := st;
end;

function GetApplicationDirectory:String;
var s: array[0..1500] of Char;
    st: String;
begin
 GetModuleFilename(hinstance, s, sizeof(s));
 st := StrPas(s);
 st := ExtractFilePath(st);
 result := st;
end;

// Limit a value to be l<=v<=u
function f_limit(v: Single; l: Single = -1; u: Single = 1): single;
begin
 if v < l then result := l else
 if v > u then result := u else
 result := v;
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g: single): single;
begin
 if (g > -90.0) then result := power(10, g * 0.05)
 else result := 0;
end;

function Amp_to_dB(v:single):single;
begin
 result := (20 * log10(v));
end;

function linear_interpolation(f, a, b: single): single;
begin
 result := (1 - f) * a + f * b;
end;

function cubic_interpolation(fr, inm1, inp, inp1, inp2: single): single;
begin
 result := inp + 0.5 * fr * (inp1 - inm1 + fr * (4 * inp1 +
  2 * inm1 - 5 * inp - inp2 + fr * (3 * (inp - inp1) - inm1 +
  inp2)));
end;

const half: double = 0.5 - 1.0E-16;
function f_trunc(f: single): integer;
var i: longint;
begin
 asm
  fld f
  fsub half
  fistp i
 end;
 result := i;
end;

function f_frac(x: single): single;
begin
 result := x - f_trunc(x);
end;

function f_int(x: single): single;
begin
 result := f_trunc(x);
end;

function f_round(f: single): integer;
begin
 result := round(f);
end;

function f_exp(x: single): single;
begin
 result := power(2, x * LN2R);
end;

function f_Sin(fAngle: single): single;
var fASqr, fResult: single;
begin
 fASqr := fAngle * fAngle;
 fResult := 7.61e-03;
 fResult := fResult * fASqr;
 fResult := fResult - 1.6605e-01;
 fResult := fResult * fASqr;
 fResult := fResult + 1;
 fResult := fResult * fAngle;
 result := fResult;
end;

function f_Cos(fAngle: single): single;
var fASqr, fResult: single;
begin
 fASqr := fAngle * fAngle;
 fResult := 3.705e-02;
 fResult := fResult * fASqr;
 fResult := fResult - 4.967e-01;
 fResult := fResult * fASqr;
 fResult := fResult + 1;
 fResult := fResult * fAngle;
 result := fResult;
end;

function f_arctan(fValue: single): single;
var fVSqr, fResult: single;
begin
 fVSqr := fValue * fValue;
 fResult := 0.0208351;
 fResult := fResult * fVSqr;
 fResult := fResult - 0.085133;
 fResult := fResult * fVSqr;
 fResult := fResult + 0.180141;
 fResult := fResult * fVSqr;
 fResult := fResult - 0.3302995;
 fResult := fResult * fVSqr;
 fResult := fResult + 0.999866;
 fResult := fResult * fValue;
 result := fResult;
end;

function f_ln2(f: single): single;
begin
 result := (((longint((@f)^) and $7f800000) shr 23)-$7f)+(longint((@f)^) and
  $007fffff)/$800000;
end;

function f_floorln2(f: single): longint;
begin
 result := (((longint((@f)^) and $7f800000) shr 23)-$7f);
end;

function f_abs(f: single): single;
var i: longint;
begin
 i := longint((@f)^) and $7FFFFFFF;
 result := single((@i)^);
end;

function f_neg(f: single):single;
var i, j: longint;
begin
 j := $80000000;
 i := longint((@f)^) xor j;
 result := single((@i)^);
end;

function f_sgn(f: single): longint;
begin
 result := 1 - ((longint((@f)^) shr 31) shl 1);
end;

function f_log2(val: single): single;
var log2, x: longint;
begin
 x := longint((@val)^);
 log2 := ((x shr 23) and 255) - 128;
 x := x and (not(255 shl 23));
 x := x + 127 shl 23;
 result := single((@x)^) + log2;
end;

function f_power(i: single; n: integer): single;
var l: longint;
begin
 l := longint((@i)^);
 l := l - $3F800000;
 l := l shl (n - 1);
 l := l + $3F800000;
 result := single((@l)^);
end;

function f_root(i: single; n: integer): single;
var l: longint;
begin
 l := longint((@i)^);
 l := l - $3F800000;
 l := l shr (n - 1);
 l := l + $3F800000;
 result := single((@l)^);
end;

function f_cliplo(x, l: single): single;
begin
 x := x - l;
 x := x + f_abs(x);
 x := x * 0.5;
 x := x + l;
 result := x;
end;

function f_cliphi(x, h: single): single;
begin
 x := h - x;
 x := x + f_abs(x);
 x := x * 0.5;
 x := h - x;
 result := x;
end;

function f_clip(x, l, h: single): single;
var x1, x2: single;
begin
 x1 := f_abs(x - l);
 x2 := f_abs(x - h);
 x := x1 + l + h;
 x := x - x2;
 x := x * 0.5;
 result := x;
end;

// scale logarithmicly from 20 Hz to 20 kHz
function FreqLinearToLog(value: single): single;
begin
 result := (20 * power(2, value * 9.965784284662088765571752446703612804412841796875));
end;

function FreqLogToLinear(value: single): single;
begin
 result := (ln(value/20)/ln(2))/9.965784284662088765571752446703612804412841796875;
end;

function OnOff(fvalue: single): boolean;
begin
 result := fvalue > 0.5;
end;

function undenormalize(fvalue: single): single;
begin
 if (f_abs(fvalue) < 1.0e-15) then fvalue := 0.0;
 result := fvalue;
end;

procedure Msg(b: boolean);
begin if b then Msg('TRUE') else Msg('FALSE');end;
procedure Msg(m: String; m2: String = '');
begin MessageBox(0,PChar(m),PChar(m2),mb_ok); end;
procedure Msg(i: integer);
begin MessageBox(0,PChar(inttostr(i)),'',mb_ok); end;
procedure Msg(s: single);
begin MessageBox(0,PChar(floattostrf(s,fffixed,3,3)),'',mb_ok); end;
procedure Msg(m: String; i: integer);
begin MessageBox(0,PChar(m+' '+inttostr(i)),'',mb_ok); end;

const fGrdDiv = 0.5;
function Saturate(input, fMax: single): single;
var x1, x2: single;
begin
 x1 := f_abs(input + fMax);
 x2 := f_abs(input - fMax);
 result := fGrdDiv * (x1 - x2);
end;

function SplitString(S: AnsiString; Delimiter: AnsiChar): TStrArray;
var C: Integer;
begin
 repeat
  SetLength(Result, Length(Result)+ 1);
  C := Pos(Delimiter, S);
  if C = 0 then C := Length(S) + 1;
  Result[Length(Result)- 1] := Copy(S, 1, C- 1);
  Delete(S, 1, C);
 until length(S)= 0;
end;

function MakeGoodFileName(s: AnsiString; replchar: AnsiChar = '_'): AnsiString;
var i: integer;
begin
 result := '';
 for i := 1 to length(s) do
  if not (s[i] in ['*', '\', '/', '[', ']',
   '"', '|', '<', '>', '?', ':']) then
   result := result + s[i]
  else
   result := result + replchar;
end;

function FillStr(s: AnsiString; len: integer; FillChar: AnsiChar): AnsiString;
begin
 if len < 0 then
  while length(s) < -len do s := FillChar + s
 else
  while length(s) < len do s := s + FillChar;
 Result := s; 
end;

function sync2smp(syncfactor, bpm, srate: single): longint;
begin
 result := f_round(syncfactor * srate * 60 / bpm);
end;

function ms2smp(ms, srate: single): single;
begin
 result := ms * srate / 1000;
end;

function smp2ms(smp, srate: single): single;
begin
 result := smp * 1000 / srate;
end;

function getSyncFactor(base_factor: single;
 dotted, triads: boolean): single;
begin
 result := base_factor;
 if dotted then result := result * 1.5;
 if triads then result := result / 3;
end;

end.