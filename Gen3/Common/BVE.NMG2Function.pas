unit BVE.NMG2Function;

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
  FMX.Graphics,
  BVE.NMG2Types,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2Object;

type
  TG2Function = class(TG2ObservedObject, IG2Function, IG2Observer)
  private
    [weak] FWModule: IG2Module;
    FParamList: TList<pointer>;

    FFunctionID: integer;
    FMasterParamIndex: integer;
    FDependencies: string;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object);
    procedure AddParam(aParam: IG2Param);
    procedure ParseDependencies;
    function GetParam(const aIndex: integer): IG2Param;
    function GetParamCount: integer;
  protected
    function InfoFunction: string;
    function DelayDispValue(aType: integer; aRange: integer; aValue: integer): string;
  public
    constructor Create(const aFunctionID: integer; aModule: IG2Module; const aMasterParamIndex: integer; const aDependencies: string); overload;
    constructor Create(const aFunctionID: integer; aModule: IG2Module; const aParamIndex: integer); overload;
    destructor Destroy; override;

    function GetAsText: string;
    property Params[const aIndex: integer]: IG2Param read GetParam;
    property ParamCount: integer read GetParamCount;
  end;

implementation
uses
  System.Math;

{ TG2Function }

constructor TG2Function.Create(const aFunctionID: integer; aModule: IG2Module;
  const aMasterParamIndex: integer; const aDependencies : string);
begin
  inherited Create;

  SetWeak(@FWModule, aModule);
  FParamList := TList<pointer>.Create;

  FFunctionID := aFunctionID;
  FDependencies := aDependencies;
  FMasterParamIndex := aMasterParamIndex;

  ParseDependencies;
end;

constructor TG2Function.Create(const aFunctionID : integer; aModule : IG2Module;
  const aParamIndex : integer);
begin
  inherited Create;

  SetWeak(@FWModule, aModule);
  FParamList := TList<pointer>.Create;

  FFunctionID := aFunctionID;
  FDependencies := '';

  AddParam(aModule.Param[aParamIndex]);
end;

destructor TG2Function.Destroy;
begin
  FParamList.Free;
  SetWeak(@FWModule, nil);

  inherited;
end;

procedure TG2Function.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  NotifyObservers(aG2Event, aG2Object);
end;

procedure TG2Function.AddParam(aParam: IG2Param);
begin
  FParamList.Add(pointer(aParam));
  aParam.RegisterObserver(self);
end;

procedure TG2Function.ParseDependencies;
var
  sl: TStringList;
  i, Value, c: integer;
  MasterParam: IG2Param;
begin
  sl := TStringList.Create;
  try
    sl.DelimitedText := FDependencies;

    // Find ref to master parameter
    if FMasterParamIndex >= 0 then
    begin
      i := 0;
      while (i<sl.Count)
      and (sl[i]<>IntToStr(FMasterParamIndex))
      and (sl[i]<>'s'+IntToStr(FMasterParamIndex)) do
        inc(i);

      if (i<sl.Count) then
      begin
        // Assign master parameter first
        if (Lowercase(sl[ i][1]) = 's') then
        begin
          // One of the static params
          MasterParam := FWModule.Mode[ FMasterParamIndex];
        end else begin
          MasterParam := FWModule.Param[ FMasterParamIndex];
        end;
      end else begin
        // No dependencies in the list, only assign the master ref param
        MasterParam := FWModule.Param[ FMasterParamIndex];
        AddParam(MasterParam);
      end;
    end;

    // If a master ref was assigned, assign the rest
    if assigned(MasterParam) then
    begin
      for i := 0 to sl.Count - 1 do
      begin
        if (sl[i].Length > 0) and (Lowercase(sl[i].Chars[0]) = 's') then
        begin
          val(sl[i].Substring(1, sl[i].Length - 1), Value, c);
          if c = 0 then
          begin
            AddParam(FWModule.Mode[ value]);
          end;
        end else begin
          val(sl[i], Value, c);
          if c = 0 then
          begin
            AddParam(FWModule.Param[ value]);
          end;
        end;
      end;
    end;

  finally
    sl.Free;
  end;
end;

function TG2Function.InfoFunction: string;
var t : single;
    FreqCourse, FreqFine, FreqMode, LevelShiftValue : Byte;
    Exponent, Freq, Fact, Temp : single;
    Value0, Value1, Value2 : integer;
    ID, TempValue : integer;
    Param : IG2Param;
begin
  if assigned(FWModule) and (ParamCount > 0) then
    Value0 := Params[0].Value
  else
    exit;

  if FFunctionID = 0 then
    ID := Params[0].InfoFunctionIndex
  else
    ID := FFunctionID;


  case ID of
  0  : begin
         if Value0 = 127 then
           Result := '100.0'
         else
           Result := FloatToStr( Round(1000 * Value0 / 128) / 10);
       end;
  2  : begin // Seq Length, Dly Clk
         Result := IntToStr(Value0 + 1);
       end;
  3  : begin // KB
         case Value0 of
         0 : Result := 'Off';
         1 : Result := 'On';
         end;
       end;
  4  : begin // Mode
         case Value0 of
         0 : Result := 'Poly';
         1 : Result := 'Mono';
         end;
       end;
  5  : begin // Mixer Inv
         case Value0 of
         0 : Result := 'Normal';
         1 : Result := 'Inverted';
         end;
       end;
  7  : begin // On/Off
         case Value0 of
         0 : Result := 'Muted';
         1 : Result := 'Active';
         end;
       end;
  8  : begin // Dly,Flt On/Off
         case Value0 of
         0 : Result := 'Bypass';
         1 : Result := 'Active';
         end;
       end;
  9 : begin // Seq, cycles
         case Value0 of
         0 : Result := '1-Cycle';
         1 : Result := 'Loop';
         end;
      end;
  11 : begin // Note quant, Notes
         TempValue := Value0;
         if TempValue = 0 then
           Result := 'Off'
         else
           Result := IntToStr(TempValue);
       end;
  12 : begin // Mystery modules PolarFade/PolarPan...
         Result := IntToStr(Value0);
       end;
  13 : begin // Note detect
         Result := GetKeyName(Value0);
       end;
  137, 16 : begin // Env Sustain
         if ParamCount > 1 then
           LevelShiftValue := Params[1].Value
         else
           LevelShiftValue := 0;

         case LevelShiftValue of
         0 : begin // Pos
               Result := Format('%.1g', [1.0 * round(Value0/2)])
             end;
         1 : begin // PosInv
               Result := Format('%.1g', [1.0 * round(Value0/2)])
             end;
         2 : begin // Neg
               Result := Format('%.1g', [1.0 * round(Value0/2)])
             end;
         3 : begin // NegInv
               Result := Format('%.1g', [1.0 * round(Value0/2)])
             end;
         4 : begin // Bip
               Result := Format('%.1g', [1.0 * round(Value0) - 64])
             end;
         5 : begin // BipInv
               Result := Format('%.1g', [1.0 * round(Value0) - 64])
             end;
         end;
       end;
  96, 17 : begin // RndPattern PatternA / FltPhase FB / NoteZone SendTrans
         case Value0 of
         127 : Result := IntToStr(64);
         else
           Result := IntToStr(Value0 - 64);
         end;
       end;
  18 : begin // Seq, Pol
         case Value0 of
         0 : Result := 'Bipol';
         1 : Result := 'Unipol';
         end;
       end;
  19 : begin // MixStereo Pan
         TempValue := Value0 - 64;
         if TempValue = 0 then
           Result := '0'
         else
           if TempValue < 0 then
             Result := IntToStr(TempValue)
           else
             Result := '+' + IntToStr(TempValue);
       end;
  21 : begin //DrumSynth NoiseFltFreq
         Freq := 440 * power(2, (Value0 - 65) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  22 : begin // Drumsynth, Masterfreq
         Result := G2FloatToStr(20.02 * power(2, Value0 / 24), 4) + 'Hz';
       end;
  23 : begin // Drumsynth, SlaveRatio
         TempValue := Value0;
         case TempValue of
         0  : Result := '1:1';
         48 : Result := '2:1';
         96 : Result := '4:1';
           else begin
             Exponent := (Value0 / 48);
             Fact := power(2, Exponent);
             Result :=  'x' + G2FloatToStrFixed( Fact, 4)
           end;
         end;
       end;
  26 : begin // Osc KB
         case Value0 of
         0 : Result := 'KBT Off';
         1 : Result := 'KBT On';
         end;
       end;
  28 : begin // Env Attack, Decay, Release
         t := ENV_TIMES[Value0];
         if ENV_TIMES[Value0] < 1.0 then
           Result := Format('%.3g', [t*1000]) + 'm'
         else
           Result := Format('%.3g', [t]) + 's';
       end;
  30 : begin // FreqShift Range
         case Value0 of
         0 : Result := 'Sub';
         1 : Result := 'Lo';
         2 : Result := 'Hi';
         end;
       end;
  35 : begin // Note Quant, Range
         TempValue := Value0;
         if TempValue = 0 then
           Result := '0'
         else
           if TempValue = 127 then
             Result := '+-64'
           else
             Result := '+-' + Format('%.1f', [TempValue / 2]);
       end;
  36 : begin // EqPeak Gain
         case Value0 of
         127 : Result := G2FloatToStrFixed( 64/3.55555, 4) + 'dB';
         else
           Result := G2FloatToStrFixed( (Value0 - 64)/3.55555, 4) + 'dB';
         end;
       end;
  38 : begin // EqPeak Freq
         Freq := 20 * power(2, (Value0) / 13.169);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  39 : begin // FltPhase Freq
         //TextFunction;
         Freq := 100 * power(2, (Value0) / 17.34515804);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  43 : begin // Env, Seq Gate/Trigger
         case Value0 of
         0 : Result := 'Trig';
         1 : Result := 'Gate';
         end;
       end;
  110, 45 : begin // ClkGen Tempo
         if assigned(FWModule) and (ParamCount = 3) then
         begin
           Value1 := Params[1].Value;
           Value2 := Params[2].Value;
           if Value1 = 0 then
             Result := '--'
           else
             if Value2 = 1 then
               Result := 'MASTER'
             else
               Result := G2BPM( Value0) + ' BPM';
         end;
       end;
  46 : begin // LevelShift
         case Value0 of
         0 : Result := 'Pos';
         1 : Result := 'PosInv';
         2 : Result := 'Neg';
         3 : Result := 'NegInv';
         4 : Result := 'Bip';
         5 : Result := 'BipInv';
         end;
       end;
  47 : begin // General On/Off
         case Value0 of
         0 : Result := 'Off';
         1 : Result := 'On';
         end;
       end;
  49 : begin // Env L1/L2
         case Value0 of
         0 : Result := 'L1';
         1 : Result := 'L2';
         end;
       end;
  50 : begin // Env L1/L2/L3/Trig
         case Value0 of
         0 : Result := 'L1';
         1 : Result := 'L2';
         2 : Result := 'L3';
         3 : Result := 'Trg';
         end;
       end;
  52 : begin // Fade2-1 Mix
         TempValue := Value0 - 64;
         if TempValue = 0 then
           Result := 'Mute'
         else
           if TempValue = -64 then
             Result := 'I1:127'
           else
             if TempValue < 0 then
               Result := 'I1:' + IntToStr(-TempValue*2)
             else
               if TempValue = 63 then
                 Result := 'I2:127'
               else
                 Result := 'I2:' + IntToStr(TempValue*2);
       end;
  53 : begin // Fade1-2 Mix
         TempValue := Value0 - 64;
         if TempValue = 0 then
           Result := 'Mute'
         else
           if TempValue = -64 then
             Result := 'O1:127'
           else
             if TempValue < 0 then
               Result := 'O1:' + IntToStr(-TempValue*2)
             else
               if TempValue = 63 then
                 Result := 'O2:127'
               else
                 Result := 'O2:' + IntToStr(TempValue*2);
       end;
  55 : begin // LevMod ModType
         TempValue := Value0;
         case TempValue of
         0 : Result := 'None';
         64 : Result := 'Am';
         127 : Result := 'Rm';
         else
           Result := IntToStr(TempValue);
         end;
       end;
  57 : begin // Mix8-1A Pad
         case Value0 of
         0 : Result := '0dB';
         1 : Result := '-6dB';
         2 : Result := '-12dB';
         end;
       end;
  58 : begin // Osc Waveform
         case Value0 of
         0 : Result := 'Sine';
         1 : Result := 'Tri';
         2 : Result := 'Saw';
         3 : Result := 'Sqr50';
         4 : Result := 'Sqr25';
         5 : Result := 'Sqr10';
         end;
       end;
  59 : begin // Osc Fine
         TempValue := (Value0-64)*100 div 128;
         if TempValue < 0 then
           Result := IntToStr(TempValue)
         else
           Result := '+' + IntToStr(TempValue);
       end;
  60, 61 : begin // Osc freq
         if assigned(FWModule) and (ParamCount = 3) then
         begin
           FreqCourse := Value0;
           FreqFine := Params[1].Value;
           FreqMode := Params[2].Value;
           Result := FreqDispValue( FreqMode, FreqCourse, FreqFine);
         end;
       end;
  63 : begin // Osc Freq mode
          case Value0 of
          0 : Result := 'Semi';
          1 : Result := 'Freq';
          2 : Result := 'Factor';
          3 : Result := 'Partial';
          end;
       end;
  64 : begin // Sw2-1 Sel
          Result := IntToStr( Value0 + 1);
       end;
  68 : begin // PartQuant, range
          TempValue := Value0;
          if TempValue > 64 then
            Result := '+-' + IntToStr(Trunc(TempValue / 2)) + '*'
          else
            Result := '+-' + IntToStr(Trunc(TempValue / 2));
       end;
  69 : begin // NoteScaler, range
          TempValue := Value0;
          if TempValue = 127 then
            Result := '+-64.0'
          else
            Result := '+-' + Format('%.1f',[TempValue / 2]);
            if (TempValue mod 24 = 0) then
              Result := Result + '-Oct'
            else
              if ((TempValue mod 24) mod 20 = 0) then
                Result := Result + '-7th'
              else
                if ((TempValue mod 24) mod 14 = 0) then
                  Result := Result + '-5th';
       end;
  70 : begin // Level scaler, Gain
         TempValue := Value0;
         if TempValue < 64  then
           Result := G2FloatToStrFixed( -8.0 + 16.0 * TempValue / 127, 3) + 'dB'
         else
           if TempValue > 64 then
             Result := G2FloatToStrFixed( -8.0 + 16.0 * TempValue / 127, 3) + 'dB'
           else
             Result := '0.0dB';

       end;
  71 : begin // Flt KBT
          case Value0 of
          0 : Result := 'KBT Off';
          1 : Result := 'KBT 25%';
          2 : Result := 'KBT 50%';
          3 : Result := 'KBT75%';
          4 : Result := 'KBT 100%';
          end;
       end;
  72 : begin // Flt GC
          case Value0 of
          0 : Result := 'GC Off';
          1 : Result := 'GC On';
          end;
       end;
  73 : begin // Flt Slope
          case Value0 of
          0 : Result := '6dB';
          1 : Result := '12dB';
          end;
       end;
  74 : begin // FltNord dB/Oct
          case Value0 of
          0 : Result := '12dB';
          1 : Result := '24dB';
          end;
       end;
  75 : begin // FltNord FilterType
          case Value0 of
          0 : Result := 'LP';
          1 : Result := 'BP';
          2 : Result := 'HP';
          3 : Result := 'BR';
          end;
       end;
  76 : begin // EqPeak BandWidth
           Result := G2FloatToStrFixed( (128 - Value0) / 64, 4) + 'Oct'
       end;
  78 : begin // FltVoice Vowel
          case Value0 of
          0 : Result := 'A';
          1 : Result := 'E';
          2 : Result := 'I';
          3 : Result := 'O';
          4 : Result := 'U';
          5 : Result := 'Y';
          6 : Result := 'AA';
          7 : Result := 'AE';
          8 : Result := 'OE';
          end;
       end;
  79 : begin // Vocoder BandSel
         TempValue := Value0;
         case TempValue of
         0 : Result := 'Off';
         else
           Result := IntToStr(TempValue);
         end;
       end;
  80 : begin // Vocoder Emphasis
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
       end;
  81 : begin // Vocoder Monitor
          case Value0 of
          0 : Result := 'Active';
          1 : Result := 'Monitor';
          end;
       end;
  82 : begin // Clip, shape
          case Value0 of
          0 : Result := 'Asym';
          1 : Result := 'Sym';
          end;
       end;
  83 : begin // Rect Mode
          case Value0 of
          0 : Result := 'HalfPos';
          1 : Result := 'HalfNeg';
          2 : Result := 'FullPos';
          3 : Result := 'FullNeg';
          end;
       end;
  84 : begin // ShpStatic Mode
          case Value0 of
          0 : Result := 'Inv x3';
          1 : Result := 'Inv x2';
          2 : Result := 'x2';
          3 : Result := 'x3';
          end;
       end;
  88 : begin // Digitizer Rate
         Freq := 440 * power(2, (Value0 - 45) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
       end;
  90 : begin // Reverb RoomType
          case Value0 of
          0 : Result := 'Small';
          1 : Result := 'Medium';
          2 : Result := 'Large';
          3 : Result := 'Hall';
          end;
       end;
  93 : begin // FltClassic dB/Oct
          case Value0 of
          0 : Result := '12dB';
          1 : Result := '18dB';
          2 : Result := '24dB';
          end;
       end;
  99 : begin // Sw1-4 Sel CHECK
          Result := IntToStr( Params[0].Value * 4);
        end;
  100 : begin // SwOnOffM On, SwOnOffT On, Sw2-1M Sel, Sw2-1 Sel, Sw1-2M Sel, Sw1-2 Sel CHECK
          Result := IntToStr( Params[0].Value * 4);
        end;
  101 : begin // Sw4-1 Sel, Sw8-1 Sel, Sw1-8 Sel CHECK
          Result := IntToStr( Params[0].Value * 4);
        end;
  102 : begin // MixFader Lev1
         if Value0 = 127 then
           Result := '100.0'
         else
           Result := FloatToStr( Round(1000 * Value0 / 128) / 10);
        end;
  103  : begin // LFO Freq
           if assigned(FWModule) and (ParamCount = 2) then begin
             Value1 := Params[1].Value;
             case Value1 of
             0 : Result := G2FloatToStr( 699 / (Value0+1), 4) + 's';
             1 : if Value0 < 32 then
                   Result := G2FloatToStr(1/(0.0159 * power(2, Value0 / 12)), 4) + 's'
                 else
                   Result := G2FloatToStr(0.0159 * power(2, Value0 / 12), 4) + 'Hz';
             2 : Result := G2FloatToStr(0.2555 * power(2, Value0 / 12), 4) + 'Hz';
             3 : Result := G2BPM( Value0);
             4 : begin; // Clock
                   case Value0 of
                   0..3 : Result := '64/1';
                   4..7 : Result := '48/1';
                   8..11 : Result := '32/1';
                   12..15 : Result := '24/1';
                   16..19 : Result := '16/1';
                   20..23 : Result := '12/1';
                   24..27 : Result := '8/1';
                   28..31 : Result := '6/1';
                   32..35 : Result := '4/1';
                   36..39 : Result := '3/1';
                   40..43 : Result := '2/1';
                   44..47 : Result := '1/1D';
                   48..51 : Result := '1/1';
                   52..55 : Result := '1/2D';
                   56..59 : Result := '1/1T';
                   60..63 : Result := '1/2';
                   64..67 : Result := '1/4D';
                   68..71 : Result := '1/2T';
                   72..75 : Result := '1/4';
                   76..79 : Result := '1/8D';
                   80..83 : Result := '1/4T';
                   84..87 : Result := '1/8';
                   88..91 : Result := '1/16D';
                   92..95 : Result := '1/8T';
                   96..99 : Result := '1/16';
                   100..103 : Result := '1/32D';
                   104..107 : Result := '1/16T';
                   108..111 : Result := '1/32';
                   112..115 : Result := '1/64D';
                   116..119 : Result := '1/32T';
                   120..123 : Result := '1/64';
                   124..127 : Result := '1/64T';
                   end;
                 end;
             end;
           end;
         end;
  107 : begin // Reverb time
          if assigned(FWModule) and (ParamCount = 2) then
          begin
            Temp := 0;
            Value1 := Params[1].Value;
            case Value1 of
            0 : Temp := 3/127 * Value0;
            1 : Temp := 6/127 * Value0;
            2 : Temp := 9/127 * Value0;
            3 : Temp := 12/127 * Value0;
            end;
            if Temp < 1 then
              Result := G2FloatToStrFixed( Temp* 1000, 4) + 'ms'
            else
              Result := G2FloatToStrFixed( Temp, 5) + 's';
          end;
        end;
  104 : begin // LFO Range
          case Value0 of
          0 : Result := 'Sub';
          1 : Result := 'Lo';
          2 : Result := 'Hi';
          3 : Result := 'BPM';
          4 : Result := 'Clk';
          end;
        end;
  105 : begin // LfoA KBT
          case Value0 of
          0 : Result := 'KBT off';
          1 : Result := 'KBT 25%';
          2 : Result := 'KBT 50%';
          3 : Result := 'KBT 75%';
          4 : Result := 'KBT 100';
          end;
        end;
  106 : begin // LfoA Wave
          case Value0 of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          4 : Result := 'RndStep';
          5 : Result := 'Rnd';
          end;
        end;
  108 : begin // Midi Channel Send
           TempValue := Value0;
           case TempValue of
           0..15 : Result := IntToStr(TempValue + 1);
           16 : Result := 'This';
           17 : Result := 'Slot A';
           18 : Result := 'Slot B';
           19 : Result := 'Slot C';
           20 : Result := 'Slot D';
           end;
        end;
  109 : begin // CtrlRcv Ch, NoteRcv Ch, NoteZone RcvCh
           TempValue := Value0;
           case TempValue of
           0..15 : Result := IntToStr(TempValue + 1);
           16 : Result := 'This';
           end;
        end;
  114 : begin // ValSw2-1 Val
          TempValue := Value0;
          if TempValue = 63 then
            Result := '64'
          else
            Result := IntToStr( Value0);
        end;
  122 : begin // logic Pulse/Delay Range
         if assigned(FWModule) and (ParamCount = 2) then begin
           t := PULSE_DELAY_RANGE[ Value0];
           Value1 := Params[1].Value;
           case Value1 of
           0 : begin // Sub
                 t := t/100;
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           1 : begin // Lo
                 t := t/10;
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           2 : begin // Hi
                 if t < 1000 then
                   Result := G2FloatToStrFixed( t, 4) + 'm'
                 else
                   Result := G2FloatToStrFixed( t / 1000, 4) + 's';
               end;
           end;
         end;
       end;
  123 : begin // Flt Freq
          FreqCourse := Value0;
          Exponent := (FreqCourse - 60) / 12;
          Freq := 440.0 * power(2, Exponent);
          if Freq >= 1000 then
            //Result := Format('%.4g', [Freq / 1000]) + 'kHz'
            Result := G2FloatToStrFixed( Freq / 1000, 4) + 'kHz'
          else
            //Result := Format('%.4g', [Freq]) + 'Hz';
            Result := G2FloatToStrFixed( Freq, 5) + 'Hz'
        end;
  124 : begin // Flt Resonance
          t := FILTER_RESONANCE[ Value0];
          Result := G2FloatToStrFixed( t, 4);
        end;
  125 : begin // Osc FM Mod
          case Value0 of
          0 : Result := 'FM Lin';
          1 : Result := 'FM Trk';
          end;
        end;
  27, 126 : begin // Osc Shape
          Result := IntToStr(trunc(50 + 50.0 * Value0 / 128)) + '%';
        end;
  127 : begin // LevConv InputType
          case Value0 of
          0 : Result := 'Bipol';
          1 : Result := 'Pos';
          2 : Result := 'Neg';
          end;
        end;
  128 : begin // LfoShpA Shape
          Result := IntToStr(trunc(1 + 97.0 * Value0 / 127)) + '%';
        end;
  129 : begin // EnvFollow Attack
          t := ENV_FOLLOW_ATTACK[ Value0];
          if t = 0 then
            Result := 'Fast'
          else
            if t < 1000 then
              Result := G2FloatToStrFixed( t, 4) + 'm'
            else
              Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  130 : begin //EnvFollow release
          t := ENV_FOLLOW_RELEASE[ Value0];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  132 : begin // Key Quant, Range
          TempValue := Value0;
          if TempValue = 127 then
            Result := '+-64.0'
          else
            Result := '+-' + Format('%.1f', [TempValue / 2]);
        end;
  133  : begin // SeqVal
           if assigned(FWModule) and (ParamCount = 2) then begin
             Value1 := Params[1].Value;
             case Value1 of
             0 : begin // Bipol
                   Result := IntToStr(Value0 - 64);
                 end;
             1 : begin // Unipol
                   Result := IntToStr(Value0);
                 end;
             end;
           end;
         end;
  134 : begin // SeqCtrl, XFade
          case Value0 of
          0 : Result := '0%';
          1 : Result := '25%';
          2 : Result := '50%';
          3 : Result := '100%';
          end;
        end;
  136 : begin // Env Shape
          case Value0 of
          0 : Result := 'LogExp';
          1 : Result := 'LinExp';
          2 : Result := 'ExpExp';
          3 : Result := 'LinLin';
          end;
        end;
  138 : begin // Env NR Button
          case Value0 of
          0 : Result := 'Normal';
          1 : Result := 'Reset';
          end;
        end;
  139 : begin // Env Decy/Release
          case Value0 of
          0 : Result := 'AD';
          1 : Result := 'AR';
          end;
        end;
  140 : begin // Dly time
          if assigned(FWModule) and (ParamCount = 3) then
          begin
            Value1 := Params[1].Value;
            Value2 := Params[2].Value;
            if Value1 = 0 then
              Result := DelayDispValue(0, Value2, Value0)
            else
              Result := DelayDispValue(4, Value2, Value0);
          end;
        end;
  141 : begin // Dly time
          if assigned(FWModule) and (ParamCount = 2) then
          begin
            Value1 := Params[1].Value;
            Result := DelayDispValue(0, Value1, Value0);
          end;
        end;
  142 : begin // Freq Shift
          if assigned(FWModule) and (ParamCount = 2) then
          begin
            Value1 := Params[1].Value;
            case Value1 of
            0 : begin // Sub
                  t := FREQ_SHIFT_SUB[ Value0];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            1 : begin // Lo
                  t := FREQ_SHIFT_LO[ Value0];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            2 : begin // High
                  t := FREQ_SHIFT_HI[ Value0];
                  Result := G2FloatToStr( t, 5) + 'Hz';
                end;
            end;
          end;
        end;
  143 : begin // Dly time
          if assigned(FWModule) and (ParamCount = 3) then
          begin
            Value1 := Params[1].Value;
            Value2 := Params[2].Value;
            if Value1 = 0 then
              Result := DelayDispValue(2, Value2, Value0)
            else
              Result := DelayDispValue(4, Value2, Value0);
          end;
        end;
  144 : begin // Dly Time/Clk
          case Value0 of
          0 : Result := 'Time';
          1 : Result := 'ClkSync';
          end;
        end;
  145 : begin // Dly8 time
          if assigned(FWModule) and (ParamCount = 2) then
          begin
            Value1 := Params[1].Value;
            Result := DelayDispValue(1, Value1, Value0);
          end;
        end;
  146 : begin // Dly time
          if assigned(FWModule) and (ParamCount = 3) then
          begin
            Value1 := Params[1].Value;
            Value2 := Params[2].Value;
            if Value1 = 0 then
              Result := DelayDispValue(3, Value2, Value0)
            else
              Result := DelayDispValue(4, Value2, Value0);
          end;
        end;
  147 : begin // LevAmp Gain TODO
          Result := IntToStr( Value0);
        end;
  148 : begin // LevAmp Type
          case Value0 of
          0 : Result := 'Lin';
          1 : Result := 'dB';
          end;
        end;
  149 : begin // FX In, pad
          case Value0 of
          0 : Result := '+6dB';
          1 : Result := '0dB';
          2 : Result := '-6dB';
          3 : Result := '-12dB';
          end;
        end;
  150 : begin // 2 in source
          case Value0 of
          0 : Result := 'In 1/2';
          1 : Result := 'In 3/4';
          2 : Result := 'Bus 1/2';
          3 : Result := 'Bus 3/4';
          end;
        end;
  152 : begin // FX In, source
          case Value0 of
          0 : Result := 'Fx 1/2';
          1 : Result := 'Fx 3/4';
          end;
        end;
  155 : begin // OscA Waveform
          case Value0 of
          0 : Result := 'Sine1';
          1 : Result := 'Sine2';
          2 : Result := 'Sine3';
          3 : Result := 'Sine4';
          4 : Result := 'TriSaw';
          5 : Result := 'Pulse';
          end;
       end;
  156 : begin // OscB Waveform
          case Value0 of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          4 : Result := 'DualSaw';
          end;
       end;
  157 : begin // Glide, shape
          case Value0 of
          0 : Result := 'Log';
          1 : Result := 'Lin';
          end;
        end;
  159 : begin // NoiseGate Attack
          t := NOISE_GATE_ATTACK[ Value0];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  160 : begin // NoiseGate Release
          t := NOISE_GATE_RELEASE[ Value0];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 4) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  163 : begin // LFO, phase
          Result := IntToStr(round(Value0 / 128 * 360));
        end;
  164 : begin // LFO, wave
          case Value0 of
          0 : Result := 'Sine';
          1 : Result := 'Tri';
          2 : Result := 'Saw';
          3 : Result := 'Sqr';
          end;
        end;
  165 : begin // LfoShpA Wave
          case Value0 of
          0 : Result := 'Sine';
          1 : Result := 'CosBell';
          2 : Result := 'TriBell';
          3 : Result := 'Saw2Tri';
          4 : Result := 'Sqr2Tri';
          5 : Result := 'Sqr';
          end;
        end;
  166 : begin // ClkGen BeatSync
           case Value0 of
           0 : Result := '1';
           1 : Result := '2';
           2 : Result := '4';
           3 : Result := '8';
           4 : Result := '16';
           5 : Result := '32';
           end;
        end;
  167 : begin // Overdrive Type
          case Value0 of
          0 : Result := 'Soft';
          1 : Result := 'Hard';
          2 : Result := 'Fat';
          3 : Result := 'Heavy';
          end;
        end;
  168 : begin // Overdrive Shape
          case Value0 of
          0 : Result := 'Asym';
          1 : Result := 'Sym';
          end;
        end;
  169 : begin // ModAmt ExpLin
          case Value0 of
          0 : Result := 'Exp';
          1 : Result := 'Lin';
          end;
        end;
  170 : begin // Phaser Type
          case Value0 of
          0 : Result := 'Type I';
          1 : Result := 'Type II';
          end;
        end;
  172 : begin // FltPhase Type
          case Value0 of
          0 : Result := '1';
          1 : Result := '2';
          2 : Result := '3';
          3 : Result := '4';
          4 : Result := '5';
          5 : Result := '6';
          end;
        end;
  173 : begin // FltComb Freq
         Freq := 440 * power(2, (Value0 - 69) / 12);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
        end;
  174 : begin // Compressor Attack
          Result := COMPR_ATTACK_TIMES[ Value0];
        end;
  175 : begin // Compressor Release
          Result := COMPR_RELEASE_TIMES[ Value0];
        end;
  176 : begin // Compressor Treshold
          case Value0 of
          42 : Result := 'Off';
          else
            Result := IntToStr(Value0 - 30) + 'dB';
          end;
        end;
  177 : begin // Compressor Ratio
          TempValue := Value0;
          if (TempValue >= 0) and (TempValue < 10) then
            Result := G2FloatToStrFixed( 1 + TempValue/10, 3) + ':1'
          else
            if TempValue < 25  then
              Result := G2FloatToStrFixed( TempValue/5, 3) + ':1'
            else
              if TempValue < 35  then
                Result := G2FloatToStrFixed( 5 + (TempValue-25)/2, 3) + ':1'
              else
                if TempValue < 45  then
                  Result := G2FloatToStrFixed( 10 + (TempValue-35), 3) + ':1'
                else
                  if TempValue < 60  then
                    Result := G2FloatToStrFixed( 20 + (TempValue-45)*2, 3) + ':1'
                  else
                    if TempValue <= 66  then
                      Result := G2FloatToStrFixed( 50 + (TempValue-60)*5, 3) + ':1'
        end;
  178 : begin // Compressor RefLevel
          Result := IntToStr( Value0 - 30) + 'dB';
        end;
  179 : begin // Midi values & operator
          Result := IntToStr( Value0);
        end;
  180 : begin // Out Pad
          case Value0 of
          0 : Result := '0dB';
          1 : Result := '+6dB';
          2 : Result := '+12dB';
          3 : Result := '+18dB';
          end;
        end;
  181 : begin // MonoKey, priority
          case Value0 of
          0 : Result := 'Last';
          1 : Result := 'Low';
          2 : Result := 'High';
          end;
        end;
  182 : begin // Out dest
          case Value0 of
          0 : Result := 'Out 1/2';
          1 : Result := 'Out 3/4';
          2 : Result := 'Fx 1/2';
          3 : Result := 'Fx 3/4';
          4 : Result := 'Bus 1/2';
          5 : Result := 'Bus 3/4';
          end;
        end;
  183 : begin // 4 Out dest
          case Value0 of
          0 : Result := 'Out';
          1 : Result := 'Fx';
          2 : Result := 'Bus';
          end;
        end;
  184 : begin // Eq3Band MidFreq
         Freq := 100 * power(2, Value0 / 20.089);
         if Freq >= 1000 then
           Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
         else
           Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
        end;
  185 : begin // Osc dual Saw phase (0..357)
          Result := IntToStr(round(Value0 / 128 * 360));
        end;
  187 : begin // ClkGen Source
          case Value0 of
          0 : Result := 'Intern';
          1 : Result := 'Master';
          end;
        end;
  188 : begin // Mix4-1C Pad
          case Value0 of
          0 : Result := '0dB';
          1 : Result := '-6dB';
          end;
        end;
  189 : begin // ModAmt InvertMode
          case Value0 of
          0 : Result := 'm';
          1 : Result := '1-m';
          end;
        end;
  190 : begin // ClkGen Swing
          Result := Format('%.1f',[50 + 25 * Value0 / 127]) + '%';
        end;
  191 : begin // Flt Freq Mod
          Result := Format('%.1f',[200.0 * Value0 / 127]) + '%';
        end;
  192 : begin // ShpExp Curve
          case Value0 of
          0 : Result := 'x2';
          1 : Result := 'x3';
          2 : Result := 'x4';
          3 : Result := 'x5';
          end;
        end;
  194 : begin // Key Quant, capture
          case Value0 of
          0 : Result := 'Closest';
          1 : Result := 'Evenly';
          end;
        end;
  195 : begin // Digitizer Bits
          case Value0 of
          12 : Result := 'Off'
          else
            Result := IntToStr(Value0 + 1)
          end;
        end;
  196 : begin // Operator FreqDetune
          Result := IntToStr(Value0 - 7);
        end;
  197 : begin // Operator BrPoint
          Result := GetKeyName(Value0 + 9);
        end;
  198 : begin // Operator freq
          if assigned(FWModule) and (ParamCount = 3) then
          begin
            Value1 := Params[1].Value;
            Value2 := Params[2].Value;
            case Value2 of
            0 : begin // Ratio  x0.50...x16.00..x31.00
                  if Value0 = 0 then
                  begin
                    Fact := 0.5;
                  end else begin
                    Fact := Value0;
                  end;
                 Fact := Fact + Fact * Value1 / 100;
                 Result :=  'x' + G2FloatToStrFixed( Fact, 4)
                end;
            1 : begin // Fixed 1Hz,10Hz,100Hz,1000Hz etc
                 Freq := trunc(power(10, (Value0 mod 4)));
                  if Freq >= 1000 then
                    Result :=  G2FloatToStrFixed( Freq / 1000, 5) + 'kHz'
                  else
                    Result := G2FloatToStrFixed( Freq, 5) + 'Hz';
                end;
            end;
          end;
        end;
  199 : begin // Operator RDepthMode
          Result := IntToStr(Value0);
        end;
  200 : begin // Operator freq mode
          case Value0 of
          0 : Result := 'Ratio';
          1 : Result := 'Fixed';
          end;
        end;
  201 : begin // PShift ShiftSemi
          if assigned(FWModule) and (ParamCount = 2) then
          begin
            FreqCourse := Value0;
            FreqFine := Params[1].Value;
            Result := FreqDispValue( 4, FreqCourse, FreqFine);
          end;
        end;
  202 : begin // PShift Delay
          case Value0 of
          0 : Result := '12.5m';
          1 : Result := '25m';
          2 : Result := '50m';
          3 : Result := '100m';
          end;
        end;
  203 : begin // Random Step
          case Value0 of
          0 : Result := '0%';
          1 : Result := '25%';
          2 : Result := '50%';
          3 : Result := '75%';
          4 : Result := '100%';
          end;
        end;
  204 : begin // RandomA OutType
          case Value0 of
          0 : Result := 'Bip';
          1 : Result := 'Pos';
          2 : Result := 'Neg';
          end;
        end;
  205 : begin // Random Clk A, Step
          Result := IntToStr(100 * Value0 div 127) + '%';
        end;
  207 : begin // RandomA Step
          case Value0 of
          0 : Result := '25%';
          1 : Result := '50%';
          2 : Result := '75%';
          3 : Result := '100%';
          end;
        end;
  209 : begin // Scratch Ratio
          case Value0 of
          64  : Result := 'x0';
          127 : Result := 'x' + G2FloatToStrFixed(4, 4);
          else
            Result := 'x' + G2FloatToStrFixed((Value0 - 64)/16, 4);
          end;
        end;
  210 : begin // NoteZone ThruMode
          case Value0 of
          0 : Result := 'Notes';
          1 : Result := 'Note+CC';
          end;
        end;
  211 : begin // Automate Echo
          case Value0 of
          0 : Result := 'EchoOff';
          1 : Result := 'EchoOn';
          end;
        end;
  212 : begin // RndTrig Step
          Result := IntToStr(100 * Value0 div 127) + '%';
        end;
  213 : begin // Eq2Band LoFreq
          case Value0 of
          0 : Result := '80 Hz';
          1 : Result := '110 Hz';
          2 : Result := '160 Hz';
          end;
        end;
  214 : begin // Eq2Band HiFreq
          case Value0 of
          0 : Result := '6 kHz';
          1 : Result := '8 kHz';
          2 : Result := '12 kHz';
          end;
        end;
  215 : begin // Flanger Rate
          //Result := G2FloatToStr(440 * power(2, ((298.210634 - Value0) / -32.64072819)), 4) + 'Hz';
          t := FLANGER_RATE[ Value0];
          Result := G2FloatToStrFixed(t, 4) + 'Hz';
        end;
  216 : begin // Phaser Freq
          //Result := G2FloatToStr(440 * power(2, ((298.210634 - Value0) / -32.64072819)), 4) + 'Hz';
          t := PHASER_FREQ[ Value0];
          Result := G2FloatToStrFixed(t, 4) + 'Hz';
        end;
  217  : begin // Glide time
           t := GLIDE_TIME[ Value0];
           if t < 1000 then
             Result := G2FloatToStrFixed(t, 4) + 'm'
           else
             Result := G2FloatToStrFixed(t/1000, 4) + 's';
         end;
  218 : begin // DrumSynth NoiseFltMode
          case Value0 of
          0 : Result := 'LP';
          1 : Result := 'BP';
          2 : Result := 'HP';
          end;
        end;
  220 : begin // Pitch track and Noise gate threshhold
          t := NOISEGATE_PITCHTRACK_THRESHHOLD[ Value0];
          if Value0 = 0 then
            Result := 'Inf.'
          else
            Result := G2FloatToStr(t, 4) + 'dB';
        end;
  500 : begin // SeqCtrl (Not found in original moduledef)
          if assigned(FWModule) then
          begin
            TempValue := Value0;
            Param := FWModule.Param[ 33];
            if assigned(Param) then
            begin
              // Polarity
              case Param.GetValue of
              0 : Result := IntToStr( TempValue - 64);
              1 : if TempValue = 127 then
                    Result := '64.0'
                  else
                    Result := FloatToStr( Round(TempValue / 2 * 10)/10);
              end;
            end;
          end;
        end;
  501 : begin // Master clock
          Result := IntToStr( Value0);
        end;
  502 : begin // Master clock run
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  503 : begin // Voices
          case Value0 of
          0 : Result := 'Legato';
          1 : Result := 'Mono';
          else
            Result := IntToStr(Value0-1);
          end;
        end;
  504 : begin // voice mode
          case Value0 of
          0 : Result := 'Poly';
          1 : Result := 'Mono';
          2 : Result := 'Mono';
          end;
        end;
  505 : begin // Arp speed
          case Value0 of
          0 : Result := '1/8';
          1 : Result := '1/8T';
          2 : Result := '1/16';
          3 : Result := '1/16T';
          end;
        end;
  506 : begin // Arp on/off
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  507 : begin // Arp dir
          case Value0 of
          0 : Result := 'Up';
          1 : Result := 'Down';
          2 : Result := 'Up/Down';
          3 : Result := 'Rnd';
          end;
        end;
  508 : begin // Arp Octaves
          case Value0 of
          0 : Result := '1 Oct';
          1 : Result := '2 Oct';
          2 : Result := '3 Oct';
          3 : Result := '4 Oct';
          end;
        end;
  509 : begin // Vibrato Depth
         if Value0 = 127 then
           Result := '100 cnt'
         else
           Result := IntToStr(Round(Value0 / 128)) + ' cnt';
        end;
  510 : begin // Vibrato Mod Srce
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'AfTouch';
          2 : Result := 'Wheel';
          end;
        end;
  511 : begin // Glide Speed
          t := PATCH_SETTINGS_GLIDE[ Value0];
          if t < 1000 then
            Result := G2FloatToStrFixed( t, 3) + 'm'
          else
            Result := G2FloatToStrFixed( t/1000, 4) + 's';
        end;
  512 : begin // Glide Type
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'Normal';
          2 : Result := 'Auto';
          end;
        end;
  513 : begin // Bend range
          Result := IntToStr(Value0 + 1) + ' semi';
        end;
  514 : begin // Bend On/Off
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  515 : begin // Volume
          t := PATCH_SETTINGS_VOLUME[ Value0];
          Result := G2FloatToStr(t, 3) + ' dB';
        end;
  516 : begin // Mute On/Off
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  517 : begin // Sustain
          case Value0 of
          0 : Result := 'Off';
          1 : Result := 'On';
          end;
        end;
  518 : begin // Octave Shift
          case Value0 of
          0 : Result := '-2';
          1 : Result := '-1';
          2 : Result := '0';
          3 : Result := '1';
          4 : Result := '2';
          end;
        end;
  519 : begin // Morph 1
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Wheel';
          end;
        end;
  520 : begin // Morph 2
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Vel';
          end;
        end;
  521 : begin // Morph 3
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Keyb';
          end;
        end;
  522 : begin // Morph 4
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Aft.Tch';
          end;
        end;
  523 : begin // Morph 5
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Sust.Pd';
          end;
        end;
  524 : begin // Morph 6
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'Ctrl.Pd';
          end;
        end;
  525 : begin // Morph 7
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'P.Stick';
          end;
        end;
  526 : begin // Morph 8
          case Value0 of
          0 : Result := 'Knob';
          1 : Result := 'G.Wh2';
          end;
        end;
  else
    //Result := IntToStr(Value0);
    Result := '??? ' + IntToStr(FFunctionID) + ' ???';
  end;
end;

// TODO Parameter unit conversion:
// ===============================
// Phaser Freq 216 ??? 216 ???
// Flanger Rate 215 ??? 215 ???
// EnvFollow Attack 129 ??? 129 ???
// EnvFollow Release 130 ??? 130 ???
// NoiseGate Attack 159 ??? 159 ???
// NoiseGate Release 160 ??? 160 ???
// Pulse Range 30 ??? 30 ???
// Delay Range 30 ??? 30 ???
// Patch Volume
// Patch Glide

// Pitch track
// Filter Resonance

function TG2Function.DelayDispValue( aType : integer; aRange : integer; aValue : integer): string;
var DlyRange, DlyMin, DlyMax : single;
begin
  case aType of
  0 : begin // ranges 5m,25m,100m,500m,1sm,2s,2.7s
         case aRange of
         0 : begin
               DlyMin := 0.05;
               DlyMax := 5.3;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         1 : begin
               DlyMin := 0.21;
               DlyMax := 25.1;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         2 : begin
               DlyMin := 0.8;
               DlyMax := 100;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         3 : begin
               DlyMin := 3.95;
               DlyMax := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         4 : begin
               DlyMin := 7.89;
               DlyMax := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,000s'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         5 : begin
               DlyMin := 15.8;
               DlyMax := 2000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 64 then
                   Result := G2FloatToStr((DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126)/1000, 5) + 's'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 4) + 'm';
             end;
         6 : begin
               DlyMin := 21.3;
               DlyMax := 2700;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 48 then
                   Result := G2FloatToStr((DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126)/1000, 5) + 's'
                 else
                   Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * (aValue - 1) /126, 5) + 'm';
             end;
         end;
      end;
  1 : begin // ranges 5m,25m,100m,500m,1sm,2s,2.7s for DelayEight
         DlyMin := 0;
         DlyMax := 0;
         case aRange of
         0 : begin
               DlyMin := 0;
               DlyMax := 0.66;
             end;
         1 : begin
               DlyMin := 0;
               DlyMax := 3.14;
             end;
         2 : begin
               DlyMin := 0;
               DlyMax := 12.6;
             end;
         3 : begin
               DlyMin := 0;
               DlyMax := 62.5;
             end;
         4 : begin
               DlyMin := 0;
               DlyMax := 125;
             end;
         5 : begin
               DlyMin := 0;
               DlyMax := 250;
             end;
         6 : begin
               DlyMin := 0;
               DlyMax := 338;
             end;
         end;
         Result := G2FloatToStr(DlyMin + (DlyMax - DlyMin) * aValue /127, 4) + 'm';
      end;
  2 : begin // ranges 500m,1s,2s,2.7s
         case aRange of
         0 : begin
               DlyRange := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         1 : begin
               DlyRange := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,00s'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         2 : begin
               DlyRange := 2000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 64 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         3 : begin
               DlyRange := 2700;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 48 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         end;
      end;
  3 : begin // ranges 500m,1s,1.351s
         case aRange of
         0 : begin
               DlyRange := 500;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         1 : begin
               DlyRange := 1000;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue = 127 then
                   Result := '1,00s'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         2 : begin
               DlyRange := 1351;
               if aValue = 0 then
                 Result := '0,01m'
               else
                 if aValue >= 95 then
                   Result := G2FloatToStr(DlyRange * aValue /127000,5) + 's'
                 else
                   Result := G2FloatToStr(DlyRange * aValue /127,4) + 'm';
             end;
         end;
      end;
  4 : begin // Clock synced
        case aValue of
            0..3 : Result := '1/64T';
            4..7 : Result := '1/64';
           8..11 : Result := '1/32T';
          12..15 : Result := '1/64D';
          16..19 : Result := '1/32';
          20..23 : Result := '1/16T';
          24..27 : Result := '1/32D';
          28..31 : Result := '1/16';
          32..35 : Result := '1/16';
          36..39 : Result := '1/8T';
          40..43 : Result := '1/8T';
          44..47 : Result := '1/16D';
          48..51 : Result := '1/16D';
          52..55 : Result := '1/8';
          56..59 : Result := '1/8';
          60..63 : Result := '1/4T';
          64..67 : Result := '1/4T';
          68..71 : Result := '1/8D';
          72..75 : Result := '1/8D';
          76..79 : Result := '1/4';
          80..83 : Result := '1/4';
          84..87 : Result := '1/2T';
          88..91 : Result := '1/2T';
          92..95 : Result := '1/4D';
          96..99 : Result := '1/4D';
        100..103 : Result := '1/2';
        104..107 : Result := '1/2';
        108..111 : Result := '1/1T';
        112..115 : Result := '1/2D';
        116..119 : Result := '1/1';
        120..123 : Result := '1/1D';
        124..127 : Result := '2/1';
        end;
      end;
  end;
end;

function TG2Function.GetAsText: string;
begin
  Result := InfoFunction;
end;

function TG2Function.GetParam(const aIndex: integer): IG2Param;
begin
  Result := IG2Param(FParamList[aIndex]);
end;

function TG2Function.GetParamCount: integer;
begin
  Result := FParamList.Count;
end;

end.
