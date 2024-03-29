unit BVE.NMG2Data;

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

{$I CompilerSettings.Inc}

interface

uses
  System.SysUtils,
  BVE.NMG2Types;

type
  TParameterRangeType = (prtActiveMonitor, prtAdAr, prtBipolar_127,
    prtBipPosNeg, prtBipUni, prtClassicSlope, prtClipShape, prtClkDivMode,
    prtClkGenBeatSync, prtClkGenSwing, prtCombType, prtCompressorAttack,
    prtCompressorRatio, prtCompressorRefLevel, prtCompressorRelease,
    prtDelayRange_1, prtDelayRange_2, prtDelayRange_3, prtDelayTime_1,
    prtDelayTime_2, prtDelayTime_3, prtDigitizerBits, prtDigitizerRate,
    prtDrumSynthFreq, prtDrumSynthNoiseFlt, prtDrumSynthRatio, prtDst_1,
    prtDst_2, prtDxAlgorithm, prtDxFeedback, prtEnvFollowAttack,
    prtEnvFollowRelease, prtEnvLevel, prtEnvNR, prtEnvShape_3, prtEnvTime,
    prtEqdB, prtEqHiFreq, prtEqLoFreq, prtEqMidFreq, prtEqPeakBandwidth,
    prtExpLin_1, prtExpLin_2, prtFade12Mix, prtFade21Mix, prtFlangerRate,
    prtFlipFlopMode, prtFltFreq, prtFltPhaseNotchCount, prtFltPhaseType,
    prtFltSlope_1, prtFltSlope_2, prtFmLinTrk, prtFreq_1, prtFreq_2, prtFreq_3,
    prtFreqCoarse, prtFreqFine, prtFreqMode_2, prtFreqMode_3, prtFreqShiftFreq,
    prtFreqShiftRange, prtGateMode, prtGcOffOn, prtGlideTime, prtHpLpSlopeMode,
    prtInternalMaster, prtKBT_4, prtKeyQuantCapture, prtLevAmpGain,
    prtLevBipUni, prtLevel_100, prtLevel_200, prtLevModAmRm, prtLevScaledB,
    prtLfoA_WaveForm, prtLfoB_WaveForm, prtLfoRange_3, prtLfoRange_4,
    prtLfoRate_3, prtLfoRate_4, prtLfoShpA__WaveForm, prtLfoShpAPW,
    prtLfoWaveForm_1, prtLinDB, prtLogicDelayMode, prtLogicRange, prtLogicTime,
    prtLogLin, prtLoopOnce, prtLpBpHp, prtLpBpHpBr, prtMidiCh_16, prtMidiCh_17,
    prtMidiCh_20, prtMidiData, prtMixInvert, prtMixLevel, prtModAmtInvert,
    prtMonoKeyMode, prtNoiseColor, prtNoiseGateAttack, prtNoiseGateRelease,
    prtNoteQuantNotes, prtNoteRange, prtNoteZoneThru, prtOffOn, prtOpAmod,
    prtOpBrPpoint, prtOpDepth, prtOpDepthMode, prtOpFreqCoarse, prtOpFreqDetune,
    prtOpFreqFine, prtOpLevel, prtOpRateScale, prtOpTime, prtOpVel,
    prtOscA_WaveForm, prtOscBWaveForm, prtOscShpA_WaveForm, prtOscWaveForm_1,
    prtOscWaveForm_2, prtOscWaveForm_3, prtOutTypeLfo, prtOverdriveType,
    prtPad_1, prtPad_2, prtPad_3, prtPad_4, prtPartialRange, prtPhase,
    prtPhaserFreq, prtPhaserType, prtPolyMono, prtPosNegInv, prtPosNegInvBip,
    prtPosNegInvBipInv, prtPShiftCoarse, prtPShiftFine, prtPulseMode, prtPW,
    prtRandomAStepProb, prtRange_128, prtRange_64, prtRangeBip_128, prtRateBpm,
    prtRatioFixed, prtRectMode, prtRes_1, prtReverbTime, prtRnd_1, prtRndEdge,
    prtRndStepPulse, prtRoomType, prtSaturateCurve, prtScratchDelay,
    prtScratchRatio, prtSeqCtrlXFade, prtSeqLen, prtShpExpCurve,
    prtShpStaticMode, prtSource_1, prtSource_2, prtSource_3, prtSustainMode_1,
    prtSustainMode_2, prtSw_1_in, prtsw_2_in, prtsw_3_in, prtTimeClk,
    prtTreshold_127, prtTreshold_42, prtTrigGate, prtValSwVal, prtVocoderBand,
    prtVowel, prtSeqOffOn, prtreson_alg, prtDriver_range, prtPulsOsc_wave,
    prtShelveEq_type, prtVolLevel, prtVolMute, prtGlideType, prtGlideSpeed,
    prtBendOnOff, prtBendRange, prtVibrMod, prtVibrDepth, prtVibrRate,
    prtArpOnOff, prtArpSpeed, prtArpDirection, prtArpOctaves, prtOctaveShift,
    prtSustainPedal, prtMorphValue, prtMorphOnOff, prtMasterClk,
    prtMasterClkRun, prtVoices, prtVoiceMode);

  TG2ModuleDef = record
    ModuleID: Integer;
    ModuleName: string;
    FileName: string;
    Tooltip: string;
    Height: Integer;
    XPos: Integer;
    YPos: Integer;
    Version: string;
    IsLed: Integer;
    Uprate: Integer;
    Page: TModulePage;
    PageIndex: Integer;
  end;

  TG2ParamDef = record
    ParamID: Integer;
    ParamType: Integer;
    RangeType: TParameterRangeType;
    LowValue: Integer;
    HighValue: Integer;
    DefaultValue: Integer;
    Definitions: string;
    Comments: string;
    slButtonText: string;
  end;

  TG2ModuleParamDef = record
    ModuleID: Integer;
    ParamIndex: Integer;
    ParamID: Integer;
    ParamName: string;
    DefaultValue: Integer;
    DefaultKnob: Integer;
    ButtonParam: Integer;
    slParamLabel: string;
  end;

  TG2ConnectorDef = record
    ModuleID: Integer;
    ConnectorIndex: Integer;
    ConnectorName: string;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    ConnectorType: TConnectorType;
    BandWidth: TBandwidthType;
  end;

  TG2LineDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    ZPos: Integer;
    Length: Integer;
    Orientation: TOrientationType;
    LineWidth: TLineWidthType;
  end;

  TG2BitmapDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    ZPos: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TG2SymbolDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    ZPos: Integer;
    SymbolType: TSymbolType;
    Width: Integer;
    Height: Integer;
  end;

  TG2TextDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    ZPos: Integer;
    FontSize: Integer;
    slText: string;
  end;

  TG2LedDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    LedType: TLedType;
    GroupID: Integer;
  end;

  TG2MiniVUDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    Orientation: TOrientationType;
    GroupID: Integer;
  end;

  TG2TextFieldDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    Width: Integer;
    MasterRef: Integer;
    TextFunc: Integer;
    slDependencies: string;
  end;

  TG2TextEditDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    slText: string;
    TextEditType: TButtonTextType;
    Width: Integer;
  end;

  TG2GraphDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    Width: Integer;
    Height: Integer;
    GraphFunc: Integer;
    slDependencies: string;
  end;

  TG2KnobDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    KnobType: TKnobType;
  end;

  TG2ButtonTextDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    slText: string;
    ButtonType: TButtonTextType;
    ButtonStyle: TButtonStyleType;
    Width: Integer;
    slImageID: string;
    ImageWidth: Integer;
  end;

  TG2ButtonFlatDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    slText: string;
    ButtonStyle: TButtonStyleType;
    Width: Integer;
    slImageID: string;
    ImageCount: Integer;
    ImageWidth: Integer;
  end;

  TG2ButtonIncDecDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    ButtonType: TButtonIncDecType;
  end;

  TG2LevelShiftDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    slImageID: string;
  end;

  TG2ButtonRadioDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    Orientation: TOrientationType;
    ButtonCount: Integer;
    ButtonWidth: Integer;
    slText: string;
    ButtonStyle: TButtonStyleType;
    slImageID: string;
    ImageWidth: Integer;
  end;

  TG2ButtonRadioEditDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    ButtonColumns: Integer;
    ButtonRows: Integer;
    slText: string;
  end;

  TG2PartSelectorDef = record
    ModuleID: Integer;
    ID: Integer;
    XPos: Integer;
    YPos: Integer;
    CodeRef: Integer;
    InfoFunc: Integer;
    Width: Integer;
    Height: Integer;
    slText: string;
    slImageID: string;
    ImageCount: Integer;
    ImageWidth: Integer;
  end;

const
  ParamDefs: array [0 .. 209] of TG2ParamDef = ((ParamID: 0; ParamType: 2;
    RangeType: prtActiveMonitor; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Mon;'), (ParamID: 1;
    ParamType: 2; RangeType: prtAdAr; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: '';
    Comments: 'Selects between Decay and Release modes';
    slButtonText: 'Dcy;Rel;'), (ParamID: 2; ParamType: 2;
    RangeType: prtBipolar_127; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: 'not necesarraly a linear control';
    slButtonText: ''), (ParamID: 3; ParamType: 2; RangeType: prtBipPosNeg;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'BiPol;Pos;Neg;'), (ParamID: 4; ParamType: 2;
    RangeType: prtBipUni; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: 'Determines display for [LevBipUni]';
    slButtonText: 'BiP;Uni;'), (ParamID: 5; ParamType: 2;
    RangeType: prtClassicSlope; LowValue: 0; HighValue: 2; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '12;18;24;'), (ParamID: 6;
    ParamType: 2; RangeType: prtClipShape; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: 'Asym;Sym;'),
    (ParamID: 7; ParamType: 1; RangeType: prtClkDivMode; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: '';
    Comments: 'Gated mode follows input clock''s positive pulse width';
    slButtonText: ''), (ParamID: 8; ParamType: 2; RangeType: prtClkGenBeatSync;
    LowValue: 0; HighValue: 5; DefaultValue: 2; Definitions: ''; Comments: '';
    slButtonText: '1;2;4;8;16;32;'), (ParamID: 9; ParamType: 2;
    RangeType: prtClkGenSwing; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 10;
    ParamType: 2; RangeType: prtCombType; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Notch;Peak;Deep;'), (ParamID: 11; ParamType: 2;
    RangeType: prtCompressorAttack; LowValue: 0; HighValue: 127;
    DefaultValue: 1; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 12; ParamType: 2; RangeType: prtCompressorRatio; LowValue: 0;
    HighValue: 66; DefaultValue: 20; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 13; ParamType: 2;
    RangeType: prtCompressorRefLevel; LowValue: 0; HighValue: 42;
    DefaultValue: 30; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 14; ParamType: 2; RangeType: prtCompressorRelease; LowValue: 0;
    HighValue: 127; DefaultValue: 20; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 15; ParamType: 1; RangeType: prtDelayRange_1;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: '';
    Comments: 'Determines [DelayTime_1]'; slButtonText: ''), (ParamID: 16;
    ParamType: 1; RangeType: prtDelayRange_2; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: '';
    Comments: 'Possibly determines [DelayTime_1], [DelayTime_2] and [DelayTime_3]';
    slButtonText: ''), (ParamID: 17; ParamType: 1; RangeType: prtDelayRange_3;
    LowValue: 0; HighValue: 6; DefaultValue: 0; Definitions: '';
    Comments: 'Determines [DelayTime_3]'; slButtonText: ''), (ParamID: 18;
    ParamType: 2; RangeType: prtDelayTime_1; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: '';
    Comments: 'Time( 500 ms, 1.0 s, 1.35 s), Clk. Determined by [DelayRange_1] and by [TimeClk] (if present)';
    slButtonText: ''), (ParamID: 19; ParamType: 2; RangeType: prtDelayTime_2;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Time( 500 ms, 1.0 s, 2.0 s, 2.7 s), Clk. Determined by [DelayRange_2] and by [TimeClk] (if present)';
    slButtonText: ''), (ParamID: 20; ParamType: 2; RangeType: prtDelayTime_3;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Time( 5ms, 25ms, 100ms, 500ms, 1.0s, 2.0s, 2.7s), Clk. Determined by [DelayRange_3] and [TimeClk] (if present)';
    slButtonText: ''), (ParamID: 21; ParamType: 2; RangeType: prtDigitizerBits;
    LowValue: 0; HighValue: 12; DefaultValue: 11; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 22; ParamType: 2; RangeType: prtDigitizerRate;
    LowValue: 0; HighValue: 127; DefaultValue: 64; Definitions: '';
    Comments: ''; slButtonText: ''), (ParamID: 23; ParamType: 2;
    RangeType: prtDrumSynthFreq; LowValue: 0; HighValue: 127; DefaultValue: 42;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 24;
    ParamType: 2; RangeType: prtDrumSynthNoiseFlt; LowValue: 0; HighValue: 127;
    DefaultValue: 57; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 25; ParamType: 2; RangeType: prtDrumSynthRatio; LowValue: 0;
    HighValue: 127; DefaultValue: 15; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 26; ParamType: 2; RangeType: prtDst_1;
    LowValue: 0; HighValue: 5; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '1/2;3/4;1/2;3/4;1/2;3/4;'), (ParamID: 27; ParamType: 2;
    RangeType: prtDst_2; LowValue: 0; HighValue: 2; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Out;Fx;Bus;'), (ParamID: 28;
    ParamType: 2; RangeType: prtDxAlgorithm; LowValue: 0; HighValue: 31;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 29; ParamType: 2; RangeType: prtDxFeedback; LowValue: 0;
    HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 30; ParamType: 2;
    RangeType: prtEnvFollowAttack; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 31;
    ParamType: 2; RangeType: prtEnvFollowRelease; LowValue: 0; HighValue: 127;
    DefaultValue: 20; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 32; ParamType: 2; RangeType: prtEnvLevel; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 33; ParamType: 2; RangeType: prtEnvNR;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 34; ParamType: 2; RangeType: prtEnvShape_3;
    LowValue: 0; HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 35; ParamType: 2; RangeType: prtEnvTime;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 36; ParamType: 2; RangeType: prtEqdB;
    LowValue: 0; HighValue: 127; DefaultValue: 64; Definitions: '';
    Comments: ''; slButtonText: ''), (ParamID: 37; ParamType: 2;
    RangeType: prtEqHiFreq; LowValue: 0; HighValue: 2; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '6k;8k;12k;'), (ParamID: 38;
    ParamType: 2; RangeType: prtEqLoFreq; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '80;110;160;'), (ParamID: 39; ParamType: 2;
    RangeType: prtEqMidFreq; LowValue: 0; HighValue: 127; DefaultValue: 93;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 40;
    ParamType: 2; RangeType: prtEqPeakBandwidth; LowValue: 0; HighValue: 127;
    DefaultValue: 64; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 41; ParamType: 2; RangeType: prtExpLin_1; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Exp;Lin;'), (ParamID: 42; ParamType: 2;
    RangeType: prtExpLin_2; LowValue: 0; HighValue: 2; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Exp;Lin;dB;'), (ParamID: 43;
    ParamType: 2; RangeType: prtFade12Mix; LowValue: 0; HighValue: 127;
    DefaultValue: 64; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 44; ParamType: 2; RangeType: prtFade21Mix; LowValue: 0;
    HighValue: 127; DefaultValue: 64; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 45; ParamType: 2; RangeType: prtFlangerRate;
    LowValue: 0; HighValue: 127; DefaultValue: 64; Definitions: '';
    Comments: ''; slButtonText: ''), (ParamID: 46; ParamType: 1;
    RangeType: prtFlipFlopMode; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 47;
    ParamType: 2; RangeType: prtFltFreq; LowValue: 0; HighValue: 127;
    DefaultValue: 75; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 48; ParamType: 2; RangeType: prtFltPhaseNotchCount; LowValue: 0;
    HighValue: 5; DefaultValue: 2; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 49; ParamType: 2; RangeType: prtFltPhaseType;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Notch;Peak;Deep;'), (ParamID: 50; ParamType: 2;
    RangeType: prtFltSlope_1; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '6;12;'), (ParamID: 51;
    ParamType: 2; RangeType: prtFltSlope_2; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: '12;24;'),
    (ParamID: 52; ParamType: 2; RangeType: prtFmLinTrk; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'FM Lin;FM Trk;'), (ParamID: 53; ParamType: 2;
    RangeType: prtFreq_1; LowValue: 0; HighValue: 127; DefaultValue: 64;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 54;
    ParamType: 2; RangeType: prtFreq_2; LowValue: 0; HighValue: 127;
    DefaultValue: 64; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 55; ParamType: 2; RangeType: prtFreq_3; LowValue: 0;
    HighValue: 127; DefaultValue: 60; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 56; ParamType: 2; RangeType: prtFreqCoarse;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Modes : Note, Semi, Freq, Fac and Part'; slButtonText: ''),
    (ParamID: 57; ParamType: 2; RangeType: prtFreqFine; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: ''; Comments: 'In cents';
    slButtonText: ''), (ParamID: 58; ParamType: 2; RangeType: prtFreqMode_2;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Semi;Freq;Fac;'), (ParamID: 59; ParamType: 2;
    RangeType: prtFreqMode_3; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Semi;Freq;Fac;Part;'),
    (ParamID: 60; ParamType: 2; RangeType: prtFreqShiftFreq; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Sub, Lo, Hi. Determined by [FreqShiftRange]'; slButtonText: ''),
    (ParamID: 61; ParamType: 2; RangeType: prtFreqShiftRange; LowValue: 0;
    HighValue: 2; DefaultValue: 0; Definitions: '';
    Comments: 'Determines [FreqShiftFreq] range'; slButtonText: 'Sub;Lo;Hi;'),
    (ParamID: 62; ParamType: 1; RangeType: prtGateMode; LowValue: 0;
    HighValue: 5; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 63; ParamType: 2; RangeType: prtGcOffOn;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'GC;GC;'), (ParamID: 64; ParamType: 2;
    RangeType: prtGlideTime; LowValue: 0; HighValue: 127; DefaultValue: 64;
    Definitions: '';
    Comments: 'Log, Lin. Determined by the Shape parameter ([LogLin])';
    slButtonText: ''), (ParamID: 65; ParamType: 1; RangeType: prtHpLpSlopeMode;
    LowValue: 0; HighValue: 5; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 66; ParamType: 2; RangeType: prtInternalMaster;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Internal;Master;'), (ParamID: 67; ParamType: 2;
    RangeType: prtKBT_4; LowValue: 0; HighValue: 4; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Off;25%;50%;75%;100;'),
    (ParamID: 68; ParamType: 2; RangeType: prtKeyQuantCapture; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Closest;Evenly;'), (ParamID: 69; ParamType: 2;
    RangeType: prtLevAmpGain; LowValue: 0; HighValue: 127; DefaultValue: 64;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 70;
    ParamType: 2; RangeType: prtLevBipUni; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: '';
    Comments: 'Bip, Uni. Determined by [BipUni] setting'; slButtonText: ''),
    (ParamID: 71; ParamType: 2; RangeType: prtLevel_100; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Can be used to denote a (not necessarily linear) 0 .. 100 range or a percentage';
    slButtonText: ''), (ParamID: 72; ParamType: 2; RangeType: prtLevel_200;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Percentage'; slButtonText: ''), (ParamID: 73; ParamType: 2;
    RangeType: prtLevModAmRm; LowValue: 0; HighValue: 127; DefaultValue: 64;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 74;
    ParamType: 2; RangeType: prtLevScaledB; LowValue: 0; HighValue: 127;
    DefaultValue: 64; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 75; ParamType: 2; RangeType: prtLfoA_WaveForm; LowValue: 0;
    HighValue: 5; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 76; ParamType: 2; RangeType: prtLfoB_WaveForm;
    LowValue: 0; HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '1;2;3;4;5;'), (ParamID: 77; ParamType: 2;
    RangeType: prtLfoRange_3; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: 'Determines [LfoRate_3]';
    slButtonText: 'Sub;Lo;Hi;BPM;Clk;'), (ParamID: 78; ParamType: 2;
    RangeType: prtLfoRange_4; LowValue: 0; HighValue: 4; DefaultValue: 0;
    Definitions: ''; Comments: 'Determines [LfoRate_4]';
    slButtonText: 'Sub;Lo;Hi;BPM;Clk;'), (ParamID: 79; ParamType: 2;
    RangeType: prtLfoRate_3; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: '';
    Comments: 'Sub, Lo, Hi, BPM (BPM is the same as for RateBpm). Determined by [LfoRange_3]';
    slButtonText: ''), (ParamID: 80; ParamType: 2; RangeType: prtLfoRate_4;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Sub, Lo, Hi, BPM, Clock (BPM is the same as for RateBpm). Determined by [LfoRange_4]';
    slButtonText: ''), (ParamID: 81; ParamType: 2;
    RangeType: prtLfoShpA__WaveForm; LowValue: 0; HighValue: 5; DefaultValue: 0;
    Definitions: ''; Comments: '';
    slButtonText: 'Sine;CosBell;TriBell;Saw2Tri;Sqr2Tri;Sqr;'), (ParamID: 82;
    ParamType: 2; RangeType: prtLfoShpAPW; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 83; ParamType: 1; RangeType: prtLfoWaveForm_1; LowValue: 0;
    HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 84; ParamType: 2; RangeType: prtLinDB;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Lin;dB;'), (ParamID: 85; ParamType: 1;
    RangeType: prtLogicDelayMode; LowValue: 0; HighValue: 2; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 86;
    ParamType: 2; RangeType: prtLogicRange; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: 'Determines [LogicTime]';
    slButtonText: 'Sub;Lo;Hi;'), (ParamID: 87; ParamType: 2;
    RangeType: prtLogicTime; LowValue: 0; HighValue: 127; DefaultValue: 1;
    Definitions: ''; Comments: 'Sub, Lo, Hi. Determined by [LogicRange]';
    slButtonText: ''), (ParamID: 88; ParamType: 2; RangeType: prtLogLin;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Log;Lin;'), (ParamID: 89; ParamType: 2;
    RangeType: prtLoopOnce; LowValue: 0; HighValue: 1; DefaultValue: 1;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 90;
    ParamType: 2; RangeType: prtLpBpHp; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: 'LP;BP;HP;'),
    (ParamID: 91; ParamType: 2; RangeType: prtLpBpHpBr; LowValue: 0;
    HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'LP;BP;HP;BR;'), (ParamID: 92; ParamType: 2;
    RangeType: prtMidiCh_16; LowValue: 0; HighValue: 16; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 93;
    ParamType: 2; RangeType: prtMidiCh_17; LowValue: 0; HighValue: 17;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 94; ParamType: 2; RangeType: prtMidiCh_20; LowValue: 0;
    HighValue: 20; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 95; ParamType: 2; RangeType: prtMidiData;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'One to one mapping with MIDI data values'; slButtonText: ''),
    (ParamID: 96; ParamType: 2; RangeType: prtMixInvert; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Inv;'), (ParamID: 97; ParamType: 2; RangeType: prtMixLevel;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Lin, Exp, dB'; slButtonText: ''), (ParamID: 98; ParamType: 2;
    RangeType: prtModAmtInvert; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'm;1-m;'), (ParamID: 99;
    ParamType: 2; RangeType: prtMonoKeyMode; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Last;Lo;Hi;'), (ParamID: 100; ParamType: 2;
    RangeType: prtNoiseColor; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 101;
    ParamType: 2; RangeType: prtNoiseGateAttack; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 102; ParamType: 2; RangeType: prtNoiseGateRelease; LowValue: 0;
    HighValue: 127; DefaultValue: 64; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 103; ParamType: 2;
    RangeType: prtNoteQuantNotes; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 104;
    ParamType: 2; RangeType: prtNoteRange; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 105; ParamType: 2; RangeType: prtNoteZoneThru; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Notes Only;Note+Ctrls;'), (ParamID: 106; ParamType: 2;
    RangeType: prtOffOn; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Off;On;'), (ParamID: 107;
    ParamType: 2; RangeType: prtOpAmod; LowValue: 0; HighValue: 7;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 108; ParamType: 2; RangeType: prtOpBrPpoint; LowValue: 0;
    HighValue: 99; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 109; ParamType: 2; RangeType: prtOpDepth;
    LowValue: 0; HighValue: 99; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 110; ParamType: 2; RangeType: prtOpDepthMode;
    LowValue: 0; HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '-Lin;-Exp;+Exp;+Lin;'), (ParamID: 111; ParamType: 2;
    RangeType: prtOpFreqCoarse; LowValue: 0; HighValue: 31; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 112;
    ParamType: 2; RangeType: prtOpFreqDetune; LowValue: 0; HighValue: 14;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 113; ParamType: 2; RangeType: prtOpFreqFine; LowValue: 0;
    HighValue: 99; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 114; ParamType: 2; RangeType: prtOpLevel;
    LowValue: 0; HighValue: 99; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 115; ParamType: 2; RangeType: prtOpRateScale;
    LowValue: 0; HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 116; ParamType: 2; RangeType: prtOpTime;
    LowValue: 0; HighValue: 99; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 117; ParamType: 2; RangeType: prtOpVel;
    LowValue: 0; HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 118; ParamType: 2; RangeType: prtOscA_WaveForm;
    LowValue: 0; HighValue: 5; DefaultValue: 2; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 119; ParamType: 2; RangeType: prtOscBWaveForm;
    LowValue: 0; HighValue: 4; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 120; ParamType: 2;
    RangeType: prtOscShpA_WaveForm; LowValue: 0; HighValue: 5; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 121;
    ParamType: 1; RangeType: prtOscWaveForm_1; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 122; ParamType: 1; RangeType: prtOscWaveForm_2; LowValue: 0;
    HighValue: 5; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 123; ParamType: 1; RangeType: prtOscWaveForm_3;
    LowValue: 0; HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 124; ParamType: 2; RangeType: prtOutTypeLfo;
    LowValue: 0; HighValue: 5; DefaultValue: 4; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 125; ParamType: 2; RangeType: prtOverdriveType;
    LowValue: 0; HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Soft;Hard;Fat;Heavy;'), (ParamID: 126; ParamType: 2;
    RangeType: prtPad_1; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '0dB;+6dB;+12dB;+18dB;'),
    (ParamID: 127; ParamType: 2; RangeType: prtPad_2; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: '0dB;-6dB;'),
    (ParamID: 128; ParamType: 2; RangeType: prtPad_3; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '0dB;-6dB;-12dB;'), (ParamID: 129; ParamType: 2;
    RangeType: prtPad_4; LowValue: 0; HighValue: 3; DefaultValue: 1;
    Definitions: ''; Comments: ''; slButtonText: '+6dB;0dB;-6dB;-12dB;'),
    (ParamID: 130; ParamType: 2; RangeType: prtPartialRange; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: '* clipped at {+-}32 and the lowest bit is not effective';
    slButtonText: ''), (ParamID: 131; ParamType: 2; RangeType: prtPhase;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'In degrees, 360 degrees in a full circle'; slButtonText: ''),
    (ParamID: 132; ParamType: 2; RangeType: prtPhaserFreq; LowValue: 0;
    HighValue: 127; DefaultValue: 64; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 133; ParamType: 2; RangeType: prtPhaserType;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Type I;Type II;'), (ParamID: 134; ParamType: 2;
    RangeType: prtPolyMono; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Poly;Mono;'), (ParamID: 135;
    ParamType: 2; RangeType: prtPosNegInv; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 136; ParamType: 2; RangeType: prtPosNegInvBip; LowValue: 0;
    HighValue: 4; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 137; ParamType: 2;
    RangeType: prtPosNegInvBipInv; LowValue: 0; HighValue: 5; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 138;
    ParamType: 2; RangeType: prtPShiftCoarse; LowValue: 0; HighValue: 127;
    DefaultValue: 64; Definitions: ''; Comments: 'Semitones'; slButtonText: ''),
    (ParamID: 139; ParamType: 2; RangeType: prtPShiftFine; LowValue: 0;
    HighValue: 127; DefaultValue: 64; Definitions: ''; Comments: 'Cents ?';
    slButtonText: ''), (ParamID: 140; ParamType: 1; RangeType: prtPulseMode;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 141; ParamType: 2; RangeType: prtPW;
    LowValue: 0; HighValue: 127; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 142; ParamType: 2;
    RangeType: prtRandomAStepProb; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '25%;50%;75%;100%;'),
    (ParamID: 143; ParamType: 2; RangeType: prtRange_128; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Mapping of origin zero to origin one'; slButtonText: ''),
    (ParamID: 144; ParamType: 2; RangeType: prtRange_64; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 145; ParamType: 2; RangeType: prtRangeBip_128;
    LowValue: 0; HighValue: 127; DefaultValue: 64; Definitions: '';
    Comments: ''; slButtonText: ''), (ParamID: 146; ParamType: 2;
    RangeType: prtRateBpm; LowValue: 0; HighValue: 127; DefaultValue: 64;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 147;
    ParamType: 2; RangeType: prtRatioFixed; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Ratio;Fixed;'), (ParamID: 148; ParamType: 2;
    RangeType: prtRectMode; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 149;
    ParamType: 2; RangeType: prtRes_1; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 150; ParamType: 2; RangeType: prtReverbTime; LowValue: 0;
    HighValue: 127; DefaultValue: 0; Definitions: '';
    Comments: 'Small, Medium, Large, Hall. Determined by [RoomType]';
    slButtonText: ''), (ParamID: 151; ParamType: 1; RangeType: prtRnd_1;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 152; ParamType: 2; RangeType: prtRndEdge;
    LowValue: 0; HighValue: 4; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '0%;25%;50%;75%;100%;'), (ParamID: 153; ParamType: 1;
    RangeType: prtRndStepPulse; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 154;
    ParamType: 1; RangeType: prtRoomType; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: '';
    Comments: 'Determines ranges for [ReverbTime]'; slButtonText: ''),
    (ParamID: 155; ParamType: 2; RangeType: prtSaturateCurve; LowValue: 0;
    HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '1;2;3;4;'), (ParamID: 156; ParamType: 2;
    RangeType: prtScratchDelay; LowValue: 0; HighValue: 3; DefaultValue: 2;
    Definitions: ''; Comments: 'mili seconds';
    slButtonText: '12.5 ms;25 ms;50 ms;100 ms;'), (ParamID: 157; ParamType: 2;
    RangeType: prtScratchRatio; LowValue: 0; HighValue: 127; DefaultValue: 80;
    Definitions: ''; Comments: 'negative speeds mean backwards playing';
    slButtonText: ''), (ParamID: 158; ParamType: 2; RangeType: prtSeqCtrlXFade;
    LowValue: 0; HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Off;25%;50%;100%;'), (ParamID: 159; ParamType: 2;
    RangeType: prtSeqLen; LowValue: 0; HighValue: 15; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 160;
    ParamType: 2; RangeType: prtShpExpCurve; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'x2;x3;x4;x5;'), (ParamID: 161; ParamType: 2;
    RangeType: prtShpStaticMode; LowValue: 0; HighValue: 3; DefaultValue: 1;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 162;
    ParamType: 2; RangeType: prtSource_1; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: '1/2;3/4;'),
    (ParamID: 163; ParamType: 2; RangeType: prtSource_2; LowValue: 0;
    HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '1/2;3/4;1/2;3/4;'), (ParamID: 164; ParamType: 2;
    RangeType: prtSource_3; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'In;Bus;'), (ParamID: 165;
    ParamType: 2; RangeType: prtSustainMode_1; LowValue: 0; HighValue: 1;
    DefaultValue: 1; Definitions: ''; Comments: ''; slButtonText: 'L1;L2;'),
    (ParamID: 166; ParamType: 2; RangeType: prtSustainMode_2; LowValue: 0;
    HighValue: 3; DefaultValue: 2; Definitions: ''; Comments: '';
    slButtonText: 'L1;L2;L3;Trg;'), (ParamID: 167; ParamType: 2;
    RangeType: prtSw_1_in; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '1;2'), (ParamID: 168;
    ParamType: 2; RangeType: prtsw_2_in; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: '1;2;3;4'),
    (ParamID: 169; ParamType: 2; RangeType: prtsw_3_in; LowValue: 0;
    HighValue: 7; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: '1;2;3;4;5;6;7;8'), (ParamID: 170; ParamType: 2;
    RangeType: prtTimeClk; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Time;Clk;'), (ParamID: 171;
    ParamType: 2; RangeType: prtTreshold_127; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 172; ParamType: 2; RangeType: prtTreshold_42; LowValue: 0;
    HighValue: 42; DefaultValue: 18; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 173; ParamType: 2; RangeType: prtTrigGate;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'T;G'), (ParamID: 174; ParamType: 2; RangeType: prtValSwVal;
    LowValue: 0; HighValue: 63; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 175; ParamType: 2; RangeType: prtVocoderBand;
    LowValue: 0; HighValue: 16; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 176; ParamType: 2; RangeType: prtVowel;
    LowValue: 0; HighValue: 8; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 177; ParamType: 2; RangeType: prtSeqOffOn;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ';'), (ParamID: 178; ParamType: 2; RangeType: prtreson_alg;
    LowValue: 0; HighValue: 4; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'String1;String2;Tube1;Tube2;Tube3'), (ParamID: 179;
    ParamType: 1; RangeType: prtDriver_range; LowValue: 0; HighValue: 3;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 180; ParamType: 1; RangeType: prtPulsOsc_wave; LowValue: 0;
    HighValue: 6; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 181; ParamType: 2; RangeType: prtShelveEq_type;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Hi;Lo'), (ParamID: 182; ParamType: 2; RangeType: prtVolLevel;
    LowValue: 0; HighValue: 127; DefaultValue: 100; Definitions: '';
    Comments: ''; slButtonText: ''), (ParamID: 183; ParamType: 2;
    RangeType: prtVolMute; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Off;On'), (ParamID: 184;
    ParamType: 2; RangeType: prtGlideType; LowValue: 0; HighValue: 2;
    DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Auto;Normal;Off'), (ParamID: 185; ParamType: 2;
    RangeType: prtGlideSpeed; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 186;
    ParamType: 2; RangeType: prtBendOnOff; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: 'Off;On'),
    (ParamID: 187; ParamType: 2; RangeType: prtBendRange; LowValue: 0;
    HighValue: 23; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 188; ParamType: 2; RangeType: prtVibrMod;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Wheel;AfTouch;Off'), (ParamID: 189; ParamType: 2;
    RangeType: prtVibrDepth; LowValue: 0; HighValue: 127; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: ''), (ParamID: 190;
    ParamType: 2; RangeType: prtVibrRate; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 191; ParamType: 2; RangeType: prtArpOnOff; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Off;On'), (ParamID: 192; ParamType: 2;
    RangeType: prtArpSpeed; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '1/8;1/8T;1/16;1/16T'),
    (ParamID: 193; ParamType: 2; RangeType: prtArpDirection; LowValue: 0;
    HighValue: 3; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Up;Dn;UpDn;Rnd'), (ParamID: 194; ParamType: 2;
    RangeType: prtArpOctaves; LowValue: 0; HighValue: 3; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: '1;2;3;4'), (ParamID: 195;
    ParamType: 2; RangeType: prtOctaveShift; LowValue: 0; HighValue: 4;
    DefaultValue: 2; Definitions: ''; Comments: '';
    slButtonText: '-2;-1;0;1;2'), (ParamID: 196; ParamType: 2;
    RangeType: prtSustainPedal; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Off;On'), (ParamID: 197;
    ParamType: 2; RangeType: prtMorphValue; LowValue: 0; HighValue: 127;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: ''),
    (ParamID: 198; ParamType: 2; RangeType: prtMorphOnOff; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Knob;Wheel'), (ParamID: 199; ParamType: 2;
    RangeType: prtMorphOnOff; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Knob;Vel'), (ParamID: 200;
    ParamType: 2; RangeType: prtMorphOnOff; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: 'Knob;Keyb'),
    (ParamID: 201; ParamType: 2; RangeType: prtMorphOnOff; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Knob;Aft.Tch'), (ParamID: 202; ParamType: 2;
    RangeType: prtMorphOnOff; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Knob;Sust.Pd;G Wh 1'),
    (ParamID: 203; ParamType: 2; RangeType: prtMorphOnOff; LowValue: 0;
    HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Knob;Ctrl.Pd'), (ParamID: 204; ParamType: 2;
    RangeType: prtMorphOnOff; LowValue: 0; HighValue: 1; DefaultValue: 0;
    Definitions: ''; Comments: ''; slButtonText: 'Knob;P.Stick'), (ParamID: 205;
    ParamType: 2; RangeType: prtMorphOnOff; LowValue: 0; HighValue: 1;
    DefaultValue: 0; Definitions: ''; Comments: ''; slButtonText: 'Knob;G.Wh2'),
    (ParamID: 206; ParamType: 2; RangeType: prtMasterClk; LowValue: 30;
    HighValue: 240; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 207; ParamType: 2; RangeType: prtMasterClkRun;
    LowValue: 0; HighValue: 1; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Off;On'), (ParamID: 208; ParamType: 2; RangeType: prtVoices;
    LowValue: 0; HighValue: 31; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: ''), (ParamID: 209; ParamType: 2; RangeType: prtVoiceMode;
    LowValue: 0; HighValue: 2; DefaultValue: 0; Definitions: ''; Comments: '';
    slButtonText: 'Poly;Mono;Legato'));

  ModuleDefs: array [0 .. 169] of TG2ModuleDef = ((ModuleID: 1;
    ModuleName: 'Keyboard'; FileName: 'IOKeyboard'; Tooltip: 'Keyboard';
    Height: 2; XPos: 24; YPos: 12; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpInOut; PageIndex: 5), (ModuleID: 3; ModuleName: '4-Out';
    FileName: 'Out4'; Tooltip: '4 Outputs'; Height: 2; XPos: 0; YPos: 0;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpInOut; PageIndex: 1),
    (ModuleID: 4; ModuleName: '2-Out'; FileName: 'Out2'; Tooltip: '2 Outputs';
    Height: 2; XPos: 0; YPos: 0; Version: '210'; IsLed: 1; Uprate: 0;
    Page: mpInOut; PageIndex: 0), (ModuleID: 5; ModuleName: 'Invert';
    FileName: 'LogicInvert'; Tooltip: 'Logic Inverter'; Height: 2; XPos: 0;
    YPos: 0; Version: '208'; IsLed: 0; Uprate: 0; Page: mpLogic; PageIndex: 1),
    (ModuleID: 7; ModuleName: 'OscB'; FileName: 'OscB'; Tooltip: 'Osc B';
    Height: 5; XPos: 0; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpOsc; PageIndex: 1), (ModuleID: 8; ModuleName: 'OscShpB';
    FileName: 'OscC'; Tooltip: 'Osc Shape B'; Height: 4; XPos: 38; YPos: 43;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 6),
    (ModuleID: 9; ModuleName: 'OscC'; FileName: 'OscC'; Tooltip: 'Osc C';
    Height: 3; XPos: 27; YPos: 39; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpOsc; PageIndex: 2), (ModuleID: 12; ModuleName: 'Reverb';
    FileName: 'FXReverb'; Tooltip: 'Reverb'; Height: 3; XPos: 0; YPos: 0;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 7),
    (ModuleID: 13; ModuleName: 'OscString'; FileName: 'PMString';
    Tooltip: 'Osc String'; Height: 3; XPos: 39; YPos: 13; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 13), (ModuleID: 15;
    ModuleName: 'Sw8-1'; FileName: 'Sw8-1Switch'; Tooltip: 'Switch 8-1';
    Height: 4; XPos: 0; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpSwitch; PageIndex: 5), (ModuleID: 17; ModuleName: 'ValSw1-2';
    FileName: 'Sw1-2Mux'; Tooltip: 'Value Switch 1-2'; Height: 2; XPos: 9;
    YPos: 10; Version: '206'; IsLed: 1; Uprate: 0; Page: mpSwitch;
    PageIndex: 11), (ModuleID: 18; ModuleName: 'X-Fade'; FileName: 'MxrX-Fade';
    Tooltip: 'Cross Fader'; Height: 2; XPos: 23; YPos: 16; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 13), (ModuleID: 19;
    ModuleName: 'Mix4-1B'; FileName: 'MxrMixer4-1A'; Tooltip: 'Mixer 4-1 B';
    Height: 2; XPos: 83; YPos: 74; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpMixer; PageIndex: 5), (ModuleID: 20; ModuleName: 'EnvADSR';
    FileName: 'EnvADSR'; Tooltip: 'Envelope ADSR'; Height: 4; XPos: 67;
    YPos: 50; Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 0),
    (ModuleID: 21; ModuleName: 'Mux1-8'; FileName: 'Sw1-8Mux';
    Tooltip: 'Multiplexer 1-8'; Height: 2; XPos: 23; YPos: 5; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 14), (ModuleID: 22;
    ModuleName: 'PartQuant'; FileName: 'NotePPartialGen';
    Tooltip: 'Partial Quantizer'; Height: 2; XPos: 20; YPos: 74; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpNote; PageIndex: 2), (ModuleID: 23;
    ModuleName: 'ModADSR'; FileName: 'EnvMOD';
    Tooltip: 'Envelope Modulation ADSR'; Height: 5; XPos: 58; YPos: 28;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 8),
    (ModuleID: 24; ModuleName: 'LfoC'; FileName: 'LfoF'; Tooltip: 'LFO C';
    Height: 2; XPos: 1; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpLFO; PageIndex: 2), (ModuleID: 25; ModuleName: 'LfoShpA';
    FileName: 'LfoC'; Tooltip: 'LFO Shape A'; Height: 5; XPos: 33; YPos: 9;
    Version: '231'; IsLed: 0; Uprate: 0; Page: mpLFO; PageIndex: 3),
    (ModuleID: 26; ModuleName: 'LfoA'; FileName: 'LfoA'; Tooltip: 'LFO A';
    Height: 3; XPos: 1; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpLFO; PageIndex: 0), (ModuleID: 27; ModuleName: 'OscMaster';
    FileName: 'OscMaster'; Tooltip: 'Osc Master'; Height: 3; XPos: 61; YPos: 3;
    Version: '234'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 16),
    (ModuleID: 28; ModuleName: 'Saturate'; FileName: 'ShaperSaturate';
    Tooltip: 'Saturate'; Height: 2; XPos: 0; YPos: 0; Version: '234'; IsLed: 0;
    Uprate: 0; Page: mpShaper; PageIndex: 2), (ModuleID: 29;
    ModuleName: 'MetNoise'; FileName: 'OscMetalNoise';
    Tooltip: 'Metallic noise oscillator'; Height: 2; XPos: 75; YPos: 28;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 10),
    (ModuleID: 30; ModuleName: 'Device'; FileName: 'IODevice';
    Tooltip: 'Device'; Height: 3; XPos: 52; YPos: 6; Version: '230'; IsLed: 0;
    Uprate: 0; Page: mpInOut; PageIndex: 7), (ModuleID: 31; ModuleName: 'Noise';
    FileName: 'OscNoise'; Tooltip: 'Noise'; Height: 2; XPos: 6; YPos: 7;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 9),
    (ModuleID: 32; ModuleName: 'Eq2Band'; FileName: 'fltEqLoHi';
    Tooltip: 'Eq 2 Band'; Height: 3; XPos: 0; YPos: 0; Version: '208'; IsLed: 0;
    Uprate: 0; Page: mpFilter; PageIndex: 12), (ModuleID: 33;
    ModuleName: 'Eq3band'; FileName: 'FltEqLoMidHi'; Tooltip: 'Eq 3 Band';
    Height: 4; XPos: 0; YPos: 0; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpFilter; PageIndex: 13), (ModuleID: 34; ModuleName: 'ShpExp';
    FileName: 'ShaperShapeB'; Tooltip: 'Shape Exp'; Height: 2; XPos: 0; YPos: 0;
    Version: '234'; IsLed: 0; Uprate: 0; Page: mpShaper; PageIndex: 3),
    (ModuleID: 35; ModuleName: 'Driver'; FileName: 'PMDriver';
    Tooltip: 'Driver'; Height: 3; XPos: 0; YPos: 0; Version: '234'; IsLed: 0;
    Uprate: 0; Page: mpOsc; PageIndex: 18), (ModuleID: 36;
    ModuleName: 'SwOnOffM'; FileName: 'SwOnOffMoment';
    Tooltip: 'Switch On/Off Momentary'; Height: 2; XPos: 25; YPos: 12;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 0),
    (ModuleID: 38; ModuleName: 'Pulse'; FileName: 'LogicPulse';
    Tooltip: 'Pulse'; Height: 2; XPos: 0; YPos: 0; Version: '208'; IsLed: 1;
    Uprate: 0; Page: mpLogic; PageIndex: 4), (ModuleID: 40;
    ModuleName: 'Mix8-1B'; FileName: 'MxrMixer8-1B'; Tooltip: 'Mixer 8-1 B';
    Height: 4; XPos: 46; YPos: 23; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpMixer; PageIndex: 9), (ModuleID: 41; ModuleName: 'EnvH';
    FileName: 'EnvH'; Tooltip: 'Envelope Hold'; Height: 2; XPos: 37; YPos: 45;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 1),
    (ModuleID: 42; ModuleName: 'Delay'; FileName: 'LogicDelay';
    Tooltip: 'Logic Delay'; Height: 2; XPos: 24; YPos: 26; Version: '208';
    IsLed: 1; Uprate: 0; Page: mpLogic; PageIndex: 5), (ModuleID: 43;
    ModuleName: 'Constant'; FileName: 'LevCOffsetLev';
    Tooltip: 'Constant Value'; Height: 2; XPos: 17; YPos: 11; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpLevel; PageIndex: 0), (ModuleID: 44;
    ModuleName: 'LevMult'; FileName: 'LevCGainCont';
    Tooltip: 'Level Multiplier'; Height: 2; XPos: 15; YPos: 19; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpLevel; PageIndex: 6), (ModuleID: 45;
    ModuleName: 'FltVoice'; FileName: 'FilterVocal'; Tooltip: 'Filter Voice';
    Height: 4; XPos: 1; YPos: 0; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpFilter; PageIndex: 9), (ModuleID: 46; ModuleName: 'EnvAHD';
    FileName: 'EnvAHD'; Tooltip: 'Envelope AHD'; Height: 4; XPos: 17; YPos: 5;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 4),
    (ModuleID: 47; ModuleName: 'Pan'; FileName: 'MxrPan'; Tooltip: 'Pan';
    Height: 2; XPos: 13; YPos: 9; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpMixer; PageIndex: 12), (ModuleID: 48; ModuleName: 'MixStereo';
    FileName: 'StMixer'; Tooltip: 'Mixer Stereo'; Height: 5; XPos: 181;
    YPos: 84; Version: '202'; IsLed: 1; Uprate: 0; Page: mpMixer;
    PageIndex: 11), (ModuleID: 49; ModuleName: 'FltMulti';
    FileName: 'FilterMulti'; Tooltip: 'Filter Multi-mode'; Height: 4; XPos: 10;
    YPos: 7; Version: '208'; IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 4),
    (ModuleID: 50; ModuleName: 'ConstSwT'; FileName: 'LevCOffsetSw';
    Tooltip: 'Constant Switch Toggling'; Height: 2; XPos: 9; YPos: 18;
    Version: '206'; IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 2),
    (ModuleID: 51; ModuleName: 'FltNord'; FileName: 'Filter1224';
    Tooltip: 'Filter Nord'; Height: 5; XPos: 0; YPos: 0; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 2), (ModuleID: 52;
    ModuleName: 'EnvMulti'; FileName: 'EnvMulti'; Tooltip: 'Envelope Multi';
    Height: 6; XPos: 52; YPos: 16; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpEnv; PageIndex: 6), (ModuleID: 53; ModuleName: 'SandH';
    FileName: 'SwSandH'; Tooltip: 'Sample & Hold'; Height: 2; XPos: 7; YPos: 5;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 16),
    (ModuleID: 54; ModuleName: 'FltStatic'; FileName: 'FilterStatic';
    Tooltip: 'Filter Static'; Height: 3; XPos: 0; YPos: 0; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 5), (ModuleID: 55;
    ModuleName: 'EnvD'; FileName: 'EnvDcy'; Tooltip: 'Envelope Decay';
    Height: 2; XPos: 56; YPos: 10; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpEnv; PageIndex: 2), (ModuleID: 56; ModuleName: 'Resonator';
    FileName: 'PMResonator'; Tooltip: 'Resonator'; Height: 5; XPos: 0; YPos: 0;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 19),
    (ModuleID: 57; ModuleName: 'Automate'; FileName: 'MIDIAutomate';
    Tooltip: 'MIDI Control Automate'; Height: 2; XPos: 31; YPos: 5;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 6),
    (ModuleID: 58; ModuleName: 'DrumSynth'; FileName: 'OscDrum';
    Tooltip: 'Drum Synthesizer'; Height: 8; XPos: 31; YPos: 9; Version: '238';
    IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 12), (ModuleID: 59;
    ModuleName: 'CompLev'; FileName: 'LogicCompareLev';
    Tooltip: 'Compare to Level'; Height: 2; XPos: 6; YPos: 8; Version: '208';
    IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 10), (ModuleID: 60;
    ModuleName: 'Mux8-1X'; FileName: 'Sw8-1XMux';
    Tooltip: 'Multiplexer 8-1 with variable X-Fade'; Height: 3; XPos: 0;
    YPos: 0; Version: '206'; IsLed: 1; Uprate: 0; Page: mpSwitch;
    PageIndex: 15), (ModuleID: 61; ModuleName: 'Clip'; FileName: 'ShaperClip';
    Tooltip: 'Clip'; Height: 2; XPos: 0; YPos: 0; Version: '208'; IsLed: 0;
    Uprate: 0; Page: mpShaper; PageIndex: 0), (ModuleID: 62;
    ModuleName: 'Overdrive'; FileName: 'ShaperOverdrive'; Tooltip: 'Overdrive';
    Height: 2; XPos: 0; YPos: 0; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpShaper; PageIndex: 1), (ModuleID: 63; ModuleName: 'Scratch';
    FileName: 'PitchShuttle'; Tooltip: 'Scratch'; Height: 3; XPos: 104;
    YPos: 13; Version: '238'; IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 6),
    (ModuleID: 64; ModuleName: 'Gate'; FileName: 'LogicGate'; Tooltip: 'Gate';
    Height: 2; XPos: 23; YPos: 23; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpLogic; PageIndex: 0), (ModuleID: 66; ModuleName: 'Mix2-1B';
    FileName: 'MxrMixer2-1'; Tooltip: 'Mixer 2-1 B'; Height: 2; XPos: 36;
    YPos: 26; Version: '206'; IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 3),
    (ModuleID: 68; ModuleName: 'ClkGen'; FileName: 'ClkGen';
    Tooltip: 'Clock Generator'; Height: 4; XPos: 65; YPos: 13; Version: '203';
    IsLed: 1; Uprate: 0; Page: mpLFO; PageIndex: 4), (ModuleID: 69;
    ModuleName: 'ClkDiv'; FileName: 'LogicClkDiv'; Tooltip: 'Clock Divider';
    Height: 2; XPos: 1; YPos: 0; Version: '208'; IsLed: 1; Uprate: 0;
    Page: mpLogic; PageIndex: 3), (ModuleID: 71; ModuleName: 'EnvFollow';
    FileName: 'EnvFollow'; Tooltip: 'Envelope Follower'; Height: 2; XPos: 21;
    YPos: 27; Version: '206'; IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 8),
    (ModuleID: 72; ModuleName: 'NoteScaler'; FileName: 'NotePNoteScaler';
    Tooltip: 'Note Scaler'; Height: 2; XPos: 20; YPos: 122; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpNote; PageIndex: 3), (ModuleID: 74;
    ModuleName: 'WaveWrap'; FileName: 'ShaperWaveWrap'; Tooltip: 'Wave Wrapper';
    Height: 2; XPos: 0; YPos: 0; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpShaper; PageIndex: 4), (ModuleID: 75; ModuleName: 'NoteQuant';
    FileName: 'NotePNoteQuant'; Tooltip: 'Note Quantizer'; Height: 2; XPos: 37;
    YPos: 38; Version: '208'; IsLed: 1; Uprate: 0; Page: mpNote; PageIndex: 0),
    (ModuleID: 76; ModuleName: 'SwOnOffT'; FileName: 'SwOnOffToggle';
    Tooltip: 'Switch On/Off Toggling'; Height: 2; XPos: 4; YPos: 10;
    Version: '206'; IsLed: 1; Uprate: 0; Page: mpSwitch; PageIndex: 1),
    (ModuleID: 78; ModuleName: 'Sw1-8'; FileName: 'Sw1-8Switch';
    Tooltip: 'Switch 1-8'; Height: 4; XPos: 0; YPos: 0; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 9), (ModuleID: 79;
    ModuleName: 'Sw4-1'; FileName: 'Sw4-1Switch'; Tooltip: 'Switch 4-1';
    Height: 3; XPos: 0; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpSwitch; PageIndex: 4), (ModuleID: 81; ModuleName: 'LevAmp';
    FileName: 'LevCAmplifier'; Tooltip: 'Level Amplifier'; Height: 2; XPos: 19;
    YPos: 15; Version: '206'; IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 5),
    (ModuleID: 82; ModuleName: 'Rect'; FileName: 'ShaperDiode';
    Tooltip: 'Rectifier'; Height: 2; XPos: 9; YPos: 4; Version: '208'; IsLed: 1;
    Uprate: 0; Page: mpShaper; PageIndex: 6), (ModuleID: 83;
    ModuleName: 'ShpStatic'; FileName: 'ShaperShaper'; Tooltip: 'Shape Static';
    Height: 2; XPos: 0; YPos: 1; Version: '208'; IsLed: 1; Uprate: 0;
    Page: mpShaper; PageIndex: 5), (ModuleID: 84; ModuleName: 'EnvADR';
    FileName: 'EnvAD'; Tooltip: 'Envelope AD/R'; Height: 3; XPos: 35; YPos: 33;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 3),
    (ModuleID: 85; ModuleName: 'WindSw'; FileName: 'SwWindSw';
    Tooltip: 'Window Switch'; Height: 2; XPos: 25; YPos: 9; Version: '206';
    IsLed: 1; Uprate: 0; Page: mpSwitch; PageIndex: 12), (ModuleID: 86;
    ModuleName: '8Counter'; FileName: 'Logic8-Counter'; Tooltip: '8 Counter';
    Height: 2; XPos: 46; YPos: 47; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpLogic; PageIndex: 6), (ModuleID: 87; ModuleName: 'FltLP';
    FileName: 'FilterLP'; Tooltip: 'Filter Lowpass'; Height: 2; XPos: 0;
    YPos: 0; Version: '208'; IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 0),
    (ModuleID: 88; ModuleName: 'Sw1-4'; FileName: 'Sw1-4Switch';
    Tooltip: 'Switch 1-4'; Height: 3; XPos: 0; YPos: 0; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 8), (ModuleID: 89;
    ModuleName: 'Flanger'; FileName: 'FXFlanger'; Tooltip: 'Flanger'; Height: 3;
    XPos: 47; YPos: 13; Version: '208'; IsLed: 0; Uprate: 0; Page: mpFX;
    PageIndex: 2), (ModuleID: 90; ModuleName: 'Sw1-2'; FileName: 'Sw1-2Switch';
    Tooltip: 'Switch 1-2'; Height: 2; XPos: 15; YPos: 14; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 7), (ModuleID: 91;
    ModuleName: 'FlipFlop'; FileName: 'LogicFlipFlop'; Tooltip: 'Flip Flop';
    Height: 2; XPos: 25; YPos: 21; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpLogic; PageIndex: 2), (ModuleID: 92; ModuleName: 'FltClassic';
    FileName: 'FilterClassic'; Tooltip: 'Filter Classic'; Height: 4; XPos: 0;
    YPos: 0; Version: '210'; IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 3),
    (ModuleID: 94; ModuleName: 'StChorus'; FileName: 'FXChorus';
    Tooltip: 'Stereo Chorus'; Height: 3; XPos: 0; YPos: 0; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 0), (ModuleID: 96;
    ModuleName: 'OscD'; FileName: 'OscD'; Tooltip: 'Osc D'; Height: 2; XPos: 47;
    YPos: 25; Version: '206'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 3),
    (ModuleID: 97; ModuleName: 'OscA'; FileName: 'OscA'; Tooltip: 'Osc A';
    Height: 3; XPos: 31; YPos: 35; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpOsc; PageIndex: 0), (ModuleID: 98; ModuleName: 'FreqShift';
    FileName: 'FXBodeShift'; Tooltip: 'Frequency Shifter'; Height: 3; XPos: 70;
    YPos: 20; Version: '208'; IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 4),
    (ModuleID: 100; ModuleName: 'Sw2-1'; FileName: 'Sw2-1Switch';
    Tooltip: 'Switch 2-1'; Height: 2; XPos: 6; YPos: 14; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 3), (ModuleID: 102;
    ModuleName: 'FltPhase'; FileName: 'FXPhaser'; Tooltip: 'Filter Phase';
    Height: 5; XPos: 0; YPos: 0; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpFilter; PageIndex: 6), (ModuleID: 103; ModuleName: 'EqPeak';
    FileName: 'FilterParaEQ'; Tooltip: 'Eq Peak'; Height: 4; XPos: 0; YPos: 0;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 11),
    (ModuleID: 105; ModuleName: 'ValSw2-1'; FileName: 'Sw2-1Mux';
    Tooltip: 'Value Switch 2-1'; Height: 2; XPos: 9; YPos: 9; Version: '206';
    IsLed: 1; Uprate: 0; Page: mpSwitch; PageIndex: 10), (ModuleID: 106;
    ModuleName: 'OscNoise'; FileName: 'TunedNoise'; Tooltip: 'Noise oscillator';
    Height: 3; XPos: 59; YPos: 7; Version: '238'; IsLed: 0; Uprate: 0;
    Page: mpOsc; PageIndex: 8), (ModuleID: 108; ModuleName: 'Vocoder';
    FileName: 'FilterVocoder'; Tooltip: 'Vocoder'; Height: 8; XPos: 88;
    YPos: 37; Version: '208'; IsLed: 1; Uprate: 0; Page: mpFilter;
    PageIndex: 10), (ModuleID: 112; ModuleName: 'LevAdd';
    FileName: 'LevCLevAdd'; Tooltip: 'Level Add'; Height: 2; XPos: 14; YPos: 16;
    Version: '206'; IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 3),
    (ModuleID: 113; ModuleName: 'Fade1-2'; FileName: 'MxrFade1-2';
    Tooltip: 'Fader 1-2'; Height: 2; XPos: 21; YPos: 24; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 14), (ModuleID: 114;
    ModuleName: 'Fade2-1'; FileName: 'MxrFade2-1'; Tooltip: 'Fader 2-1';
    Height: 2; XPos: 21; YPos: 24; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpMixer; PageIndex: 15), (ModuleID: 115; ModuleName: 'LevScaler';
    FileName: 'NotePNoteVelScal'; Tooltip: 'Level Scaler'; Height: 3; XPos: 0;
    YPos: 0; Version: '208'; IsLed: 1; Uprate: 0; Page: mpNote; PageIndex: 7),
    (ModuleID: 116; ModuleName: 'Mix8-1A'; FileName: 'MxrMixer8-1A';
    Tooltip: 'Mixer 8-1 A'; Height: 2; XPos: 105; YPos: 98; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 8), (ModuleID: 117;
    ModuleName: 'LevMod'; FileName: 'LevCRingMod'; Tooltip: 'Level Modulator';
    Height: 3; XPos: 34; YPos: 22; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpLevel; PageIndex: 7), (ModuleID: 118; ModuleName: 'Digitizer';
    FileName: 'FXDigitizer'; Tooltip: 'Digitizer'; Height: 3; XPos: 57; YPos: 7;
    Version: '208'; IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 3),
    (ModuleID: 119; ModuleName: 'EnvADDSR'; FileName: 'EnvADBDSR';
    Tooltip: 'Envelope ADBDSR'; Height: 5; XPos: 69; YPos: 45; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 5), (ModuleID: 121;
    ModuleName: 'SeqNote'; FileName: 'SeqNote'; Tooltip: 'Sequencer Note';
    Height: 9; XPos: 45; YPos: 15; Version: '210'; IsLed: 0; Uprate: 0;
    Page: mpSeq; PageIndex: 3), (ModuleID: 123; ModuleName: 'Mix4-1C';
    FileName: 'MxrMixer4-1B'; Tooltip: 'Mixer 4-1 C'; Height: 4; XPos: 69;
    YPos: 46; Version: '206'; IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 6),
    (ModuleID: 124; ModuleName: 'Mux8-1'; FileName: 'Sw8-1Mux';
    Tooltip: 'Multiplexer 8-1'; Height: 2; XPos: 46; YPos: 28; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 13), (ModuleID: 125;
    ModuleName: 'WahWah'; FileName: 'Wah'; Tooltip: 'Wah-Wah'; Height: 2;
    XPos: 10; YPos: 19; Version: '202'; IsLed: 0; Uprate: 0; Page: mpFilter;
    PageIndex: 8), (ModuleID: 126; ModuleName: 'Name'; FileName: 'Name';
    Tooltip: 'Name'; Height: 1; XPos: 0; YPos: 0; Version: '210'; IsLed: 0;
    Uprate: 0; Page: mpInOut; PageIndex: 10), (ModuleID: 127;
    ModuleName: 'Fx-In'; FileName: 'CVAIn'; Tooltip: 'Fx Input'; Height: 2;
    XPos: 0; YPos: 0; Version: '210'; IsLed: 1; Uprate: 0; Page: mpInOut;
    PageIndex: 4), (ModuleID: 128; ModuleName: 'MinMax'; FileName: 'MinMax';
    Tooltip: 'Min/Max Compare'; Height: 2; XPos: 26; YPos: 10; Version: '203';
    IsLed: 0; Uprate: 0; Page: mpLevel; PageIndex: 12), (ModuleID: 130;
    ModuleName: 'BinCounter'; FileName: 'LogicBinCounter';
    Tooltip: 'Binary Counter'; Height: 2; XPos: 0; YPos: 1; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpLogic; PageIndex: 7), (ModuleID: 131;
    ModuleName: 'ADConv'; FileName: 'LogicADConv'; Tooltip: 'A/D Converter';
    Height: 2; XPos: 47; YPos: 31; Version: '208'; IsLed: 0; Uprate: 0;
    Page: mpLogic; PageIndex: 8), (ModuleID: 132; ModuleName: 'DAConv';
    FileName: 'LogicDAConv'; Tooltip: 'D/A Converter'; Height: 2; XPos: 11;
    YPos: 6; Version: '206'; IsLed: 0; Uprate: 0; Page: mpLogic; PageIndex: 9),
    (ModuleID: 134; ModuleName: 'FltHP'; FileName: 'FilterHP';
    Tooltip: 'Filter Highpass'; Height: 2; XPos: 20; YPos: 17; Version: '208';
    IsLed: 0; Uprate: 0; Page: mpFilter; PageIndex: 1), (ModuleID: 139;
    ModuleName: 'T&H'; FileName: 'SwT&H'; Tooltip: 'Track & Hold'; Height: 2;
    XPos: 8; YPos: 7; Version: '208'; IsLed: 0; Uprate: 0; Page: mpSwitch;
    PageIndex: 17), (ModuleID: 140; ModuleName: 'Mix4-1S';
    FileName: 'MxrMixer4-2'; Tooltip: 'Mixer 4-1 Stereo'; Height: 4; XPos: 23;
    YPos: 0; Version: '206'; IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 7),
    (ModuleID: 141; ModuleName: 'CtrlSend'; FileName: 'MIDICtrlSend';
    Tooltip: 'MIDI Control Send'; Height: 2; XPos: 145; YPos: 120;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 0),
    (ModuleID: 142; ModuleName: 'PCSend'; FileName: 'MIDIPCSend';
    Tooltip: 'MIDI Program Change Send'; Height: 2; XPos: 124; YPos: 98;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 1),
    (ModuleID: 143; ModuleName: 'NoteSend'; FileName: 'MIDINoteSend';
    Tooltip: 'MIDI Note Send'; Height: 2; XPos: 99; YPos: 76; Version: '210';
    IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 2), (ModuleID: 144;
    ModuleName: 'SeqEvent'; FileName: 'SeqEvent'; Tooltip: 'Sequencer Event';
    Height: 5; XPos: 39; YPos: 14; Version: '210'; IsLed: 0; Uprate: 0;
    Page: mpSeq; PageIndex: 0), (ModuleID: 145; ModuleName: 'SeqVal';
    FileName: 'SeqValue'; Tooltip: 'Sequencer Values'; Height: 8; XPos: 0;
    YPos: 0; Version: '210'; IsLed: 0; Uprate: 0; Page: mpSeq; PageIndex: 1),
    (ModuleID: 146; ModuleName: 'SeqLev'; FileName: 'SeqCtrl';
    Tooltip: 'Sequencer Level'; Height: 8; XPos: 49; YPos: 7; Version: '210';
    IsLed: 0; Uprate: 0; Page: mpSeq; PageIndex: 2), (ModuleID: 147;
    ModuleName: 'CtrlRcv'; FileName: 'MIDICtrlRcv';
    Tooltip: 'MIDI Control Receive'; Height: 2; XPos: 65; YPos: 32;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 3),
    (ModuleID: 148; ModuleName: 'NoteRcv'; FileName: 'MIDINoteRcv';
    Tooltip: 'MIDI Note Receive'; Height: 2; XPos: 42; YPos: 9; Version: '210';
    IsLed: 1; Uprate: 0; Page: mpMidi; PageIndex: 4), (ModuleID: 149;
    ModuleName: 'NoteZone'; FileName: 'MIDINoteZone'; Tooltip: 'MIDI Note Zone';
    Height: 3; XPos: 63; YPos: 9; Version: '210'; IsLed: 1; Uprate: 0;
    Page: mpMidi; PageIndex: 5), (ModuleID: 150; ModuleName: 'Compress';
    FileName: 'FXComp'; Tooltip: 'Compressor'; Height: 5; XPos: 197; YPos: 27;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpFX; PageIndex: 8),
    (ModuleID: 152; ModuleName: 'KeyQuant'; FileName: 'KeyQuantizer';
    Tooltip: 'Key Quantizer'; Height: 2; XPos: 42; YPos: 30; Version: '210';
    IsLed: 0; Uprate: 0; Page: mpNote; PageIndex: 1), (ModuleID: 154;
    ModuleName: 'SeqCtr'; FileName: 'SeqVolt'; Tooltip: 'Sequencer Controlled';
    Height: 8; XPos: 28; YPos: 9; Version: '210'; IsLed: 0; Uprate: 0;
    Page: mpSeq; PageIndex: 4), (ModuleID: 156; ModuleName: 'NoteDet';
    FileName: 'NoteDetector'; Tooltip: 'Note Detector'; Height: 2; XPos: 16;
    YPos: 15; Version: '210'; IsLed: 1; Uprate: 0; Page: mpInOut; PageIndex: 9),
    (ModuleID: 157; ModuleName: 'LevConv'; FileName: 'LevConv';
    Tooltip: 'Level Converter'; Height: 2; XPos: 18; YPos: 21; Version: '210';
    IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 4), (ModuleID: 158;
    ModuleName: 'Glide'; FileName: 'Portamento'; Tooltip: 'Glide'; Height: 2;
    XPos: 1; YPos: 1; Version: '210'; IsLed: 0; Uprate: 0; Page: mpNote;
    PageIndex: 4), (ModuleID: 159; ModuleName: 'CompSig'; FileName: 'CompareB';
    Tooltip: 'Compare to Signal'; Height: 2; XPos: 34; YPos: 28; Version: '210';
    IsLed: 0; Uprate: 0; Page: mpLevel; PageIndex: 11), (ModuleID: 160;
    ModuleName: 'ZeroCnt'; FileName: 'PitchTrack';
    Tooltip: 'Zero Crossing Counter'; Height: 2; XPos: 80; YPos: 57;
    Version: '210'; IsLed: 0; Uprate: 0; Page: mpNote; PageIndex: 6),
    (ModuleID: 161; ModuleName: 'MixFader'; FileName: 'MixFader8';
    Tooltip: 'Mixer 8-1 Fader'; Height: 9; XPos: 140; YPos: 47; Version: '210';
    IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 10), (ModuleID: 162;
    ModuleName: 'FltComb'; FileName: 'DelayLine'; Tooltip: 'Filter Comb';
    Height: 4; XPos: 0; YPos: 0; Version: '232'; IsLed: 0; Uprate: 0;
    Page: mpFilter; PageIndex: 7), (ModuleID: 163; ModuleName: 'OscShpA';
    FileName: 'OscSinShp'; Tooltip: 'Osc Shape A'; Height: 5; XPos: 17;
    YPos: 10; Version: '206'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 5),
    (ModuleID: 164; ModuleName: 'OscDual'; FileName: 'OscJun';
    Tooltip: 'Osc Dual'; Height: 5; XPos: 55; YPos: 38; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 7), (ModuleID: 165;
    ModuleName: 'DXRouter'; FileName: 'DXRouter'; Tooltip: 'DX style router';
    Height: 6; XPos: 128; YPos: 36; Version: '220'; IsLed: 0; Uprate: 0;
    Page: mpOsc; PageIndex: 15), (ModuleID: 167; ModuleName: 'PShift';
    FileName: 'PitchShifter'; Tooltip: 'Pitch Shifter'; Height: 3; XPos: 183;
    YPos: 95; Version: '238'; IsLed: 0; Uprate: 0; Page: mpFX; PageIndex: 5),
    (ModuleID: 169; ModuleName: 'ModAHD'; FileName: 'EnvAHDMod';
    Tooltip: 'Envelope Modulation AHD'; Height: 5; XPos: 61; YPos: 8;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpEnv; PageIndex: 7),
    (ModuleID: 170; ModuleName: '2-In'; FileName: 'In2'; Tooltip: '2 Inputs';
    Height: 2; XPos: 0; YPos: 0; Version: '210'; IsLed: 1; Uprate: 0;
    Page: mpInOut; PageIndex: 2), (ModuleID: 171; ModuleName: '4-In';
    FileName: 'In4'; Tooltip: '4 Inputs'; Height: 2; XPos: 0; YPos: 0;
    Version: '210'; IsLed: 1; Uprate: 0; Page: mpInOut; PageIndex: 3),
    (ModuleID: 172; ModuleName: 'DlySingleA'; FileName: 'Dly1A';
    Tooltip: 'Delay Static'; Height: 2; XPos: 0; YPos: 0; Version: '228';
    IsLed: 0; Uprate: 0; Page: mpDelay; PageIndex: 0), (ModuleID: 173;
    ModuleName: 'DlySingleB'; FileName: 'Dly1B'; Tooltip: 'Delay Single';
    Height: 2; XPos: 0; YPos: 0; Version: '228'; IsLed: 0; Uprate: 0;
    Page: mpDelay; PageIndex: 1), (ModuleID: 174; ModuleName: 'DelayDual';
    FileName: 'Dly2'; Tooltip: 'Delay Dual'; Height: 3; XPos: 10; YPos: 7;
    Version: '228'; IsLed: 0; Uprate: 0; Page: mpDelay; PageIndex: 2),
    (ModuleID: 175; ModuleName: 'DelayQuad'; FileName: 'Dly4';
    Tooltip: 'Delay Quad'; Height: 5; XPos: 11; YPos: 2; Version: '228';
    IsLed: 0; Uprate: 0; Page: mpDelay; PageIndex: 3), (ModuleID: 176;
    ModuleName: 'DelayA'; FileName: 'DlyA'; Tooltip: 'Delay A'; Height: 3;
    XPos: 0; YPos: 0; Version: '228'; IsLed: 0; Uprate: 0; Page: mpDelay;
    PageIndex: 7), (ModuleID: 177; ModuleName: 'DelayB'; FileName: 'DlyB';
    Tooltip: 'Delay B'; Height: 4; XPos: 7; YPos: 3; Version: '228'; IsLed: 0;
    Uprate: 0; Page: mpDelay; PageIndex: 8), (ModuleID: 178;
    ModuleName: 'DlyClock'; FileName: 'DlyClock'; Tooltip: 'Delay Clocked';
    Height: 2; XPos: 0; YPos: 0; Version: '228'; IsLed: 1; Uprate: 0;
    Page: mpDelay; PageIndex: 6), (ModuleID: 179; ModuleName: 'DlyShiftReg';
    FileName: 'DlyShift'; Tooltip: 'Shift Register'; Height: 2; XPos: 44;
    YPos: 28; Version: '228'; IsLed: 0; Uprate: 0; Page: mpDelay; PageIndex: 5),
    (ModuleID: 180; ModuleName: 'Operator'; FileName: 'Operator';
    Tooltip: 'FM Operator'; Height: 12; XPos: 103; YPos: 18; Version: '226';
    IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 14), (ModuleID: 181;
    ModuleName: 'DlyEight'; FileName: 'Dly8'; Tooltip: 'Delay 8 Tap'; Height: 3;
    XPos: 2; YPos: 4; Version: '228'; IsLed: 0; Uprate: 0; Page: mpDelay;
    PageIndex: 4), (ModuleID: 182; ModuleName: 'DlyStereo';
    FileName: 'DlyStereo'; Tooltip: 'Delay Stereo'; Height: 5; XPos: 0; YPos: 0;
    Version: '228'; IsLed: 0; Uprate: 0; Page: mpDelay; PageIndex: 9),
    (ModuleID: 183; ModuleName: 'OscPM'; FileName: 'OscPH';
    Tooltip: 'Osc Phase Mod'; Height: 3; XPos: 13; YPos: 38; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 4), (ModuleID: 184;
    ModuleName: 'Mix1-1A'; FileName: 'MxrMixer1_1'; Tooltip: 'Mixer 1-1 A';
    Height: 2; XPos: 23; YPos: 23; Version: '206'; IsLed: 0; Uprate: 0;
    Page: mpMixer; PageIndex: 0), (ModuleID: 185; ModuleName: 'Mix1-1S';
    FileName: 'MxrMixer2_2'; Tooltip: 'Mixer 1-1 Stereo'; Height: 2; XPos: 14;
    YPos: 5; Version: '206'; IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 1),
    (ModuleID: 186; ModuleName: 'Sw1-2M'; FileName: 'Sw1-2Mom';
    Tooltip: 'Switch 1-2 Momentary'; Height: 2; XPos: 10; YPos: 12;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 6),
    (ModuleID: 187; ModuleName: 'Sw2-1M'; FileName: 'Sw2-1Mom';
    Tooltip: 'Switch 2-1 Momentary'; Height: 2; XPos: 10; YPos: 8;
    Version: '206'; IsLed: 0; Uprate: 0; Page: mpSwitch; PageIndex: 2),
    (ModuleID: 188; ModuleName: 'ConstSwM'; FileName: 'LevCOffsetSwMom';
    Tooltip: 'Constant Switch Momentary'; Height: 2; XPos: 15; YPos: 22;
    Version: '206'; IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 1),
    (ModuleID: 189; ModuleName: 'NoiseGate'; FileName: 'NoiseGate';
    Tooltip: 'Noise Gate'; Height: 3; XPos: 9; YPos: 5; Version: '206';
    IsLed: 1; Uprate: 0; Page: mpLevel; PageIndex: 9), (ModuleID: 190;
    ModuleName: 'LfoB'; FileName: 'lfob'; Tooltip: 'LFO B'; Height: 4; XPos: 0;
    YPos: 0; Version: '231'; IsLed: 0; Uprate: 0; Page: mpLFO; PageIndex: 1),
    (ModuleID: 192; ModuleName: 'Phaser'; FileName: 'Phaser'; Tooltip: 'Phaser';
    Height: 2; XPos: 10; YPos: 19; Version: '230'; IsLed: 0; Uprate: 0;
    Page: mpFX; PageIndex: 1), (ModuleID: 193; ModuleName: 'Mix4-1A';
    FileName: 'MxrMixer4-1C'; Tooltip: 'Mixer 4-1 A'; Height: 2; XPos: 55;
    YPos: 52; Version: '206'; IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 4),
    (ModuleID: 194; ModuleName: 'Mix2-1A'; FileName: 'MxrMixer2-1A';
    Tooltip: 'Mixer 2-1 A'; Height: 2; XPos: 0; YPos: 0; Version: '206';
    IsLed: 0; Uprate: 0; Page: mpMixer; PageIndex: 2), (ModuleID: 195;
    ModuleName: 'ModAmt'; FileName: 'ModAmount'; Tooltip: 'Modulation Amount';
    Height: 2; XPos: 20; YPos: 19; Version: '230'; IsLed: 0; Uprate: 0;
    Page: mpLevel; PageIndex: 13), (ModuleID: 196; ModuleName: 'OscPerc';
    FileName: 'OscPerc'; Tooltip: 'Osc Percussion'; Height: 3; XPos: 62;
    YPos: 4; Version: '232'; IsLed: 0; Uprate: 0; Page: mpOsc; PageIndex: 11),
    (ModuleID: 197; ModuleName: 'Status'; FileName: 'Status'; Tooltip: 'Status';
    Height: 2; XPos: 3; YPos: 19; Version: '230'; IsLed: 0; Uprate: 0;
    Page: mpInOut; PageIndex: 8), (ModuleID: 198; ModuleName: 'PitchTrack';
    FileName: 'PitchDetect'; Tooltip: 'Pitch tracker'; Height: 2; XPos: 55;
    YPos: 11; Version: '208'; IsLed: 1; Uprate: 0; Page: mpNote; PageIndex: 5),
    (ModuleID: 199; ModuleName: 'MonoKey'; FileName: 'IOMonoKeyboard';
    Tooltip: 'Monophonic Keyboard'; Height: 2; XPos: 0; YPos: 0; Version: '230';
    IsLed: 1; Uprate: 0; Page: mpInOut; PageIndex: 6), (ModuleID: 200;
    ModuleName: 'RandomA'; FileName: 'RndLfoA'; Tooltip: 'Random A'; Height: 2;
    XPos: 0; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0; Page: mpRnd;
    PageIndex: 0), (ModuleID: 201; ModuleName: 'Red2Blue'; FileName: 'Red2Blue';
    Tooltip: 'Red 2 Blue'; Height: 2; XPos: 0; YPos: 0; Version: '238';
    IsLed: 0; Uprate: 0; Page: mpTest; PageIndex: 2), (ModuleID: 202;
    ModuleName: 'RandomB'; FileName: 'RndLfoB'; Tooltip: 'Random B'; Height: 3;
    XPos: 0; YPos: 0; Version: '206'; IsLed: 0; Uprate: 0; Page: mpRnd;
    PageIndex: 1), (ModuleID: 203; ModuleName: 'Blue2Red'; FileName: 'Blue2Red';
    Tooltip: 'Blue 2 Red'; Height: 2; XPos: 0; YPos: 0; Version: '238';
    IsLed: 0; Uprate: 0; Page: mpTest; PageIndex: 1), (ModuleID: 204;
    ModuleName: 'RndClkA'; FileName: 'RndClkA'; Tooltip: 'Random Clock A';
    Height: 2; XPos: 61; YPos: 31; Version: '238'; IsLed: 0; Uprate: 0;
    Page: mpRnd; PageIndex: 2), (ModuleID: 205; ModuleName: 'RndTrig';
    FileName: 'RndClkB'; Tooltip: 'Random Trig'; Height: 2; XPos: 38; YPos: 8;
    Version: '238'; IsLed: 0; Uprate: 0; Page: mpRnd; PageIndex: 4),
    (ModuleID: 206; ModuleName: 'RndClkB'; FileName: 'RndClkC';
    Tooltip: 'Random Clock B'; Height: 3; XPos: 0; YPos: 0; Version: '238';
    IsLed: 0; Uprate: 0; Page: mpRnd; PageIndex: 3), (ModuleID: 208;
    ModuleName: 'RndPattern'; FileName: 'RndClkPattern';
    Tooltip: 'Random Pattern'; Height: 3; XPos: 0; YPos: 0; Version: '238';
    IsLed: 1; Uprate: 0; Page: mpRnd; PageIndex: 5));

  ModuleParams: array [0 .. 914] of TG2ModuleParamDef = ((ModuleID: 3;
    ParamIndex: 0; ParamID: 27; ParamName: 'Dest'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 3;
    ParamIndex: 1; ParamID: 106; ParamName: 'On/Off'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 3;
    ParamIndex: 2; ParamID: 126; ParamName: 'Pad'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 4;
    ParamIndex: 0; ParamID: 26; ParamName: 'Dest'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 0; slParamLabel: ''), (ModuleID: 4;
    ParamIndex: 1; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 2; ButtonParam: 1; slParamLabel: ''), (ModuleID: 4;
    ParamIndex: 2; ParamID: 126; ParamName: 'Pad'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 0; ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 4; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 3; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 4; ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 4; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 5; ParamID: 71; ParamName: 'Fm M'; DefaultValue: 0;
    DefaultKnob: 6; ButtonParam: 10; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 6; ParamID: 141; ParamName: 'Shape'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 7; ParamID: 71; ParamName: 'Shape M'; DefaultValue: 0;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 8; ParamID: 119; ParamName: 'Wave'; DefaultValue: 2;
    DefaultKnob: 3; ButtonParam: 8; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 9; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 7; ButtonParam: 9; slParamLabel: ''), (ModuleID: 7;
    ParamIndex: 10; ParamID: 52; ParamName: 'Fm PTrk'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 10; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 0; ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 4; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 3; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 4; ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 4; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 5; ParamID: 71; ParamName: 'Fm M'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 9; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 6; ParamID: 141; ParamName: 'Shape'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 7; ParamID: 71; ParamName: 'Shape M'; DefaultValue: 0;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 8; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 6; ButtonParam: 8; slParamLabel: ''), (ModuleID: 8;
    ParamIndex: 9; ParamID: 52; ParamName: 'Fm PTrk'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 9; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 0; ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 3; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 3; ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 4; ParamID: 71; ParamName: 'Fm M'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 6; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 5; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 4; ButtonParam: 5; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 6; ParamID: 52; ParamName: 'Fm PTrk'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 6; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 7; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 12;
    ParamIndex: 0; ParamID: 150; ParamName: 'Time'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 12;
    ParamIndex: 1; ParamID: 71; ParamName: 'Brightness'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 12;
    ParamIndex: 2; ParamID: 71; ParamName: 'DryWet'; DefaultValue: 64;
    DefaultKnob: 2; ButtonParam: 3; slParamLabel: ''), (ModuleID: 12;
    ParamIndex: 3; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 0; ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 4; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 3; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 4; ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 4; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 5; ParamID: 71; ParamName: 'Decay'; DefaultValue: 80;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 6; ParamID: 71; ParamName: 'Moisture'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 13;
    ParamIndex: 7; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 5; ButtonParam: 7; slParamLabel: ''), (ModuleID: 15;
    ParamIndex: 0; ParamID: 169; ParamName: 'Sel'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 0;
    slParamLabel: 'In 1;In 2;In 3;In 4;In 5;In 6;In 7;In 8'), (ModuleID: 17;
    ParamIndex: 0; ParamID: 174; ParamName: 'Val'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 18;
    ParamIndex: 0; ParamID: 71; ParamName: 'MixMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 18;
    ParamIndex: 1; ParamID: 2; ParamName: 'Mix'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 18;
    ParamIndex: 2; ParamID: 88; ParamName: 'LogLin'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 19;
    ParamIndex: 0; ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 19;
    ParamIndex: 1; ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 19;
    ParamIndex: 2; ParamID: 97; ParamName: 'Lev3'; DefaultValue: 100;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 19;
    ParamIndex: 3; ParamID: 97; ParamName: 'Lev4'; DefaultValue: 100;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 19;
    ParamIndex: 4; ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 4; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 0; ParamID: 34; ParamName: 'Shape'; DefaultValue: 0;
    DefaultKnob: 5; ButtonParam: 5; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 1; ParamID: 35; ParamName: 'Attack'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 7; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 2; ParamID: 35; ParamName: 'Decay'; DefaultValue: 54;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 3; ParamID: 32; ParamName: 'Sustain'; DefaultValue: 100;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 4; ParamID: 35; ParamName: 'Release'; DefaultValue: 14;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 5; ParamID: 137; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 5; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 6; ParamID: 106; ParamName: 'KB'; DefaultValue: 1;
    DefaultKnob: 4; ButtonParam: 6; slParamLabel: ''), (ModuleID: 20;
    ParamIndex: 7; ParamID: 33; ParamName: 'Reset'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 7; slParamLabel: ''), (ModuleID: 22;
    ParamIndex: 0; ParamID: 130; ParamName: 'Range'; DefaultValue: 127;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 0; ParamID: 35; ParamName: 'Attack'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 1; ParamID: 35; ParamName: 'Decay'; DefaultValue: 54;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 2; ParamID: 32; ParamName: 'Sustain'; DefaultValue: 100;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 3; ParamID: 35; ParamName: 'Release'; DefaultValue: 14;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 4; ParamID: 71; ParamName: 'Atk M'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 5; ParamID: 71; ParamName: 'Dcy M'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 6; ParamID: 71; ParamName: 'Sust M'; DefaultValue: 0;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 7; ParamID: 71; ParamName: 'Rel M'; DefaultValue: 0;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 8; ParamID: 137; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: 8; ButtonParam: 8; slParamLabel: ''), (ModuleID: 23;
    ParamIndex: 9; ParamID: 106; ParamName: 'KB'; DefaultValue: 1;
    DefaultKnob: 9; ButtonParam: 9; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 0; ParamID: 79; ParamName: 'Rate'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 3; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 1; ParamID: 134; ParamName: 'Mode'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 2; ParamID: 124; ParamName: 'OutType'; DefaultValue: 4;
    DefaultKnob: 2; ButtonParam: 2; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 3; ParamID: 77; ParamName: 'Range'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 4; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 4; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 0; ParamID: 80; ParamName: 'Rate'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 1; ParamID: 78; ParamName: 'Range'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 2; ParamID: 67; ParamName: 'KBT'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 2; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 3; ParamID: 71; ParamName: 'Rate M'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 4; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 10; ButtonParam: 4; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 5; ParamID: 82; ParamName: 'Shape'; DefaultValue: 64;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 6; ParamID: 71; ParamName: 'Phase M'; DefaultValue: 0;
    DefaultKnob: 8; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 7; ParamID: 131; ParamName: 'Phase'; DefaultValue: 0;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 8; ParamID: 71; ParamName: 'Shape M'; DefaultValue: 0;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 9; ParamID: 134; ParamName: 'Mode'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 9; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 10; ParamID: 124; ParamName: 'OutType'; DefaultValue: 4;
    DefaultKnob: 9; ButtonParam: 10; slParamLabel: ''), (ModuleID: 25;
    ParamIndex: 11; ParamID: 81; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 11; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 0; ParamID: 79; ParamName: 'Rate'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 7; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 1; ParamID: 134; ParamName: 'Mode'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 1; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 2; ParamID: 67; ParamName: 'KBT'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 2; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 3; ParamID: 71; ParamName: 'Rate M'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 4; ParamID: 75; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 4; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 5; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 6; ButtonParam: 5; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 6; ParamID: 124; ParamName: 'OutType'; DefaultValue: 4;
    DefaultKnob: 5; ButtonParam: 6; slParamLabel: ''), (ModuleID: 26;
    ParamIndex: 7; ParamID: 77; ParamName: 'Range'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 7; slParamLabel: ''), (ModuleID: 27;
    ParamIndex: 0; ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 3; slParamLabel: ''), (ModuleID: 27;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 27;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 27;
    ParamIndex: 3; ParamID: 58; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 27;
    ParamIndex: 4; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 28;
    ParamIndex: 0; ParamID: 71; ParamName: 'Amount'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 28;
    ParamIndex: 1; ParamID: 71; ParamName: 'AmountMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 28;
    ParamIndex: 2; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 2; slParamLabel: ''), (ModuleID: 28;
    ParamIndex: 3; ParamID: 155; ParamName: 'Curve'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 3; slParamLabel: ''), (ModuleID: 29;
    ParamIndex: 0; ParamID: 71; ParamName: 'Color'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 29;
    ParamIndex: 1; ParamID: 71; ParamName: 'Freq'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 29;
    ParamIndex: 2; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 2; ButtonParam: 2; slParamLabel: ''), (ModuleID: 29;
    ParamIndex: 3; ParamID: 71; ParamName: 'FreqMod'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 29;
    ParamIndex: 4; ParamID: 71; ParamName: 'ColorMod'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 31;
    ParamIndex: 0; ParamID: 100; ParamName: 'Color'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 31;
    ParamIndex: 1; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 0; ParamID: 36; ParamName: 'LoSlope'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 4; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 1; ParamID: 36; ParamName: 'HiSlope'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: 5; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 2; ParamID: 71; ParamName: 'Level'; DefaultValue: 127;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 3; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 3; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 4; ParamID: 38; ParamName: 'LoFreq'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 4; slParamLabel: ''), (ModuleID: 32;
    ParamIndex: 5; ParamID: 37; ParamName: 'HiFreq'; DefaultValue: 2;
    DefaultKnob: - 1; ButtonParam: 5; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 0; ParamID: 36; ParamName: 'LoSlope'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 6; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 1; ParamID: 36; ParamName: 'MidGain'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 2; ParamID: 39; ParamName: 'MidFreq'; DefaultValue: 93;
    DefaultKnob: 2; ButtonParam: 2; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 3; ParamID: 36; ParamName: 'HiSlope'; DefaultValue: 64;
    DefaultKnob: 3; ButtonParam: 7; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 4; ParamID: 71; ParamName: 'Level'; DefaultValue: 127;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 5; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 5; ButtonParam: 5; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 6; ParamID: 38; ParamName: 'LoFreq'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 6; slParamLabel: ''), (ModuleID: 33;
    ParamIndex: 7; ParamID: 37; ParamName: 'HiFreq'; DefaultValue: 2;
    DefaultKnob: - 1; ButtonParam: 7; slParamLabel: ''), (ModuleID: 34;
    ParamIndex: 0; ParamID: 71; ParamName: 'Amount'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 34;
    ParamIndex: 1; ParamID: 71; ParamName: 'AmountMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 34;
    ParamIndex: 2; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 2; slParamLabel: ''), (ModuleID: 34;
    ParamIndex: 3; ParamID: 160; ParamName: 'Curve'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 3; slParamLabel: ''), (ModuleID: 35;
    ParamIndex: 0; ParamID: 32; ParamName: 'Embouch'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 35;
    ParamIndex: 1; ParamID: 32; ParamName: 'Stiffn'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 36;
    ParamIndex: 0; ParamID: 106; ParamName: 'On'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 0; slParamLabel: 'On'), (ModuleID: 38;
    ParamIndex: 0; ParamID: 87; ParamName: 'Time'; DefaultValue: 1;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 38;
    ParamIndex: 1; ParamID: 71; ParamName: 'TimeMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 38;
    ParamIndex: 2; ParamID: 86; ParamName: 'Range'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 0; ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 1; ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 2; ParamID: 97; ParamName: 'Lev3'; DefaultValue: 100;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 3; ParamID: 97; ParamName: 'Lev4'; DefaultValue: 100;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 4; ParamID: 97; ParamName: 'Lev5'; DefaultValue: 100;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 5; ParamID: 97; ParamName: 'Lev6'; DefaultValue: 100;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 6; ParamID: 97; ParamName: 'Lev7'; DefaultValue: 100;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 7; ParamID: 97; ParamName: 'Lev8'; DefaultValue: 100;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 8; ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0;
    DefaultKnob: 8; ButtonParam: 8; slParamLabel: ''), (ModuleID: 40;
    ParamIndex: 9; ParamID: 128; ParamName: 'Pad'; DefaultValue: 0;
    DefaultKnob: 9; ButtonParam: 9; slParamLabel: ''), (ModuleID: 41;
    ParamIndex: 0; ParamID: 35; ParamName: 'Hold'; DefaultValue: 32;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 41;
    ParamIndex: 1; ParamID: 135; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 42;
    ParamIndex: 0; ParamID: 87; ParamName: 'Time'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 42;
    ParamIndex: 1; ParamID: 71; ParamName: 'TimeMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 42;
    ParamIndex: 2; ParamID: 86; ParamName: 'Range'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 43;
    ParamIndex: 0; ParamID: 70; ParamName: 'Level'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 1; slParamLabel: ''), (ModuleID: 43;
    ParamIndex: 1; ParamID: 4; ParamName: 'BipUni'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 0; ParamID: 176; ParamName: 'Vowel1'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 0; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 1; ParamID: 176; ParamName: 'Vowel2'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 2; ParamID: 176; ParamName: 'Vowel3'; DefaultValue: 2;
    DefaultKnob: 4; ButtonParam: 2; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 3; ParamID: 71; ParamName: 'Level'; DefaultValue: 100;
    DefaultKnob: 8; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 4; ParamID: 71; ParamName: 'Vowel'; DefaultValue: 64;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 5; ParamID: 2; ParamName: 'VowelMod'; DefaultValue: 0;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 6; ParamID: 71; ParamName: 'Freq'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 7; ParamID: 2; ParamName: 'FreqMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 8; ParamID: 2; ParamName: 'Res'; DefaultValue: 64;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 45;
    ParamIndex: 9; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 9; ButtonParam: 9; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 0; ParamID: 34; ParamName: 'Shape'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 5; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 1; ParamID: 35; ParamName: 'Attack'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 3; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 2; ParamID: 35; ParamName: 'Hold'; DefaultValue: 32;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 3; ParamID: 33; ParamName: 'Reset'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 4; ParamID: 35; ParamName: 'Decay'; DefaultValue: 14;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 5; ParamID: 135; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 5; slParamLabel: ''), (ModuleID: 46;
    ParamIndex: 6; ParamID: 106; ParamName: 'KB'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 6; slParamLabel: ''), (ModuleID: 47;
    ParamIndex: 0; ParamID: 71; ParamName: 'PanMod'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 47;
    ParamIndex: 1; ParamID: 2; ParamName: 'Pan'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 47;
    ParamIndex: 2; ParamID: 88; ParamName: 'LogLin'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 0; ParamID: 71; ParamName: 'Lev1'; DefaultValue: 100;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 1; ParamID: 71; ParamName: 'Lev2'; DefaultValue: 100;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 2; ParamID: 71; ParamName: 'Lev3'; DefaultValue: 100;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 3; ParamID: 71; ParamName: 'Lev4'; DefaultValue: 100;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 4; ParamID: 71; ParamName: 'Lev5'; DefaultValue: 100;
    DefaultKnob: 8; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 5; ParamID: 71; ParamName: 'Lev6'; DefaultValue: 100;
    DefaultKnob: 10; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 6; ParamID: 2; ParamName: 'Pan1'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 7; ParamID: 2; ParamName: 'Pan2'; DefaultValue: 64;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 8; ParamID: 2; ParamName: 'Pan3'; DefaultValue: 64;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 9; ParamID: 2; ParamName: 'Pan4'; DefaultValue: 64;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 10; ParamID: 2; ParamName: 'Pan5'; DefaultValue: 64;
    DefaultKnob: 9; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 11; ParamID: 2; ParamName: 'Pan6'; DefaultValue: 64;
    DefaultKnob: 11; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 48;
    ParamIndex: 12; ParamID: 71; ParamName: 'LevMaster'; DefaultValue: 100;
    DefaultKnob: 12; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 0; ParamID: 47; ParamName: 'Freq'; DefaultValue: 75;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 1; ParamID: 72; ParamName: 'Freq M'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 2; ParamID: 67; ParamName: 'KBT'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 3; ParamID: 63; ParamName: 'GComp'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 4; ParamID: 149; ParamName: 'Res'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 3; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 5; ParamID: 50; ParamName: 'dB/Oct'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 5; slParamLabel: ''), (ModuleID: 49;
    ParamIndex: 6; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 4; ButtonParam: 6; slParamLabel: ''), (ModuleID: 50;
    ParamIndex: 0; ParamID: 70; ParamName: 'Lev'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: 1; slParamLabel: ''), (ModuleID: 50;
    ParamIndex: 1; ParamID: 106; ParamName: 'On'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 1; slParamLabel: 'Switch'), (ModuleID: 50;
    ParamIndex: 2; ParamID: 4; ParamName: 'BipUni'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 0; ParamID: 47; ParamName: 'Freq'; DefaultValue: 75;
    DefaultKnob: 0; ButtonParam: 2; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 1; ParamID: 72; ParamName: 'Freq M'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 2; ParamID: 67; ParamName: 'KBT'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 2; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 3; ParamID: 63; ParamName: 'GComp'; DefaultValue: 1;
    DefaultKnob: - 1; ButtonParam: 3; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 4; ParamID: 149; ParamName: 'Res'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 3; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 5; ParamID: 51; ParamName: 'dB/Oct'; DefaultValue: 1;
    DefaultKnob: 6; ButtonParam: 5; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 6; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 7; ButtonParam: 6; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 7; ParamID: 71; ParamName: 'FM_Lin'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 8; ParamID: 91; ParamName: 'FilterType'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 8; slParamLabel: ''), (ModuleID: 51;
    ParamIndex: 9; ParamID: 71; ParamName: 'ResMod'; DefaultValue: 0;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 0; ParamID: 32; ParamName: 'Level1'; DefaultValue: 127;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 1; ParamID: 32; ParamName: 'Level2'; DefaultValue: 45;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 2; ParamID: 32; ParamName: 'Level3'; DefaultValue: 64;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 3; ParamID: 32; ParamName: 'Level4'; DefaultValue: 0;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 4; ParamID: 35; ParamName: 'Time1'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: 8; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 5; ParamID: 35; ParamName: 'Time2'; DefaultValue: 30;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 6; ParamID: 35; ParamName: 'Time3'; DefaultValue: 30;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 7; ParamID: 35; ParamName: 'Time4'; DefaultValue: 14;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 8; ParamID: 33; ParamName: 'Reset'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 8; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 9; ParamID: 166; ParamName: 'SusPlac'; DefaultValue: 2;
    DefaultKnob: 8; ButtonParam: 9; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 10; ParamID: 136; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: 10; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 11; ParamID: 106; ParamName: 'KBG'; DefaultValue: 1;
    DefaultKnob: 9; ButtonParam: 11; slParamLabel: ''), (ModuleID: 52;
    ParamIndex: 12; ParamID: 34; ParamName: 'Shape'; DefaultValue: 0;
    DefaultKnob: 10; ButtonParam: 10; slParamLabel: ''), (ModuleID: 54;
    ParamIndex: 0; ParamID: 47; ParamName: 'Freq'; DefaultValue: 75;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 54;
    ParamIndex: 1; ParamID: 149; ParamName: 'Res'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 54;
    ParamIndex: 2; ParamID: 90; ParamName: 'FilterType'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: 2; slParamLabel: ''), (ModuleID: 54;
    ParamIndex: 3; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: 3; slParamLabel: ''), (ModuleID: 54;
    ParamIndex: 4; ParamID: 63; ParamName: 'GComp'; DefaultValue: 0;
    DefaultKnob: 4; ButtonParam: 4; slParamLabel: ''), (ModuleID: 55;
    ParamIndex: 0; ParamID: 35; ParamName: 'Decay'; DefaultValue: 54;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 55;
    ParamIndex: 1; ParamID: 135; ParamName: 'OutType'; DefaultValue: 0;
    DefaultKnob: 1; ButtonParam: 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 0; ParamID: 56; ParamName: 'Course'; DefaultValue: 64;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 1; ParamID: 57; ParamName: 'Fine'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 2; ParamID: 106; ParamName: 'KBT'; DefaultValue: 1;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 3; ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 4; ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 5; ParamID: 32; ParamName: 'Decay'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 6; ParamID: 32; ParamName: 'Damp'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 7; ParamID: 106; ParamName: 'On/Off'; DefaultValue: 1;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 8; ParamID: 32; ParamName: 'Pos'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 56;
    ParamIndex: 9; ParamID: 178; ParamName: 'Algorithm'; DefaultValue: 0;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 57;
    ParamIndex: 0; ParamID: 95; ParamName: 'Ctrl'; DefaultValue: 7;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 57;
    ParamIndex: 1; ParamID: 95; ParamName: 'Val'; DefaultValue: 64;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 57;
    ParamIndex: 2; ParamID: 94; ParamName: 'Ch'; DefaultValue: 0;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 57;
    ParamIndex: 3; ParamID: 106; ParamName: 'Echo'; DefaultValue: 0;
    DefaultKnob: 3; ButtonParam: 3; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 0; ParamID: 23; ParamName: 'Masterfreq'; DefaultValue: 42;
    DefaultKnob: 0; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 1; ParamID: 25; ParamName: 'SlaveRatio'; DefaultValue: 15;
    DefaultKnob: 3; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 2; ParamID: 35; ParamName: 'MasterDecay'; DefaultValue: 46;
    DefaultKnob: 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 3; ParamID: 35; ParamName: 'SlaveDecay'; DefaultValue: 50;
    DefaultKnob: 4; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 4; ParamID: 71; ParamName: 'MasterLevel'; DefaultValue: 120;
    DefaultKnob: 2; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 5; ParamID: 71; ParamName: 'SlaveLevel'; DefaultValue: 102;
    DefaultKnob: 5; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 6; ParamID: 24; ParamName: 'NoiseFltFreq'; DefaultValue: 57;
    DefaultKnob: 6; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 7; ParamID: 71; ParamName: 'NoiseFltRes'; DefaultValue: 32;
    DefaultKnob: 7; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 8; ParamID: 71; ParamName: 'NoiseFltSweep'; DefaultValue: 39;
    DefaultKnob: 8; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 9; ParamID: 35; ParamName: 'NoiseFltDecay'; DefaultValue: 49;
    DefaultKnob: 9; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 10; ParamID: 90; ParamName: 'NoiseFltMode'; DefaultValue: 1;
    DefaultKnob: 10; ButtonParam: 10; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 11; ParamID: 71; ParamName: 'BendAmount'; DefaultValue: 68;
    DefaultKnob: 11; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 12; ParamID: 35; ParamName: 'BendDecay'; DefaultValue: 61;
    DefaultKnob: 12; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 13; ParamID: 71; ParamName: 'Click'; DefaultValue: 79;
    DefaultKnob: 13; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 14; ParamID: 71; ParamName: 'Noise'; DefaultValue: 115;
    DefaultKnob: 14; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 58;
    ParamIndex: 15; ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1;
    DefaultKnob: 15; ButtonParam: 15; slParamLabel: ''), (ModuleID: 59;
    ParamIndex: 0; ParamID: 2; ParamName: 'C'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 60; ParamIndex: 0;
    ParamID: 71; ParamName: 'XFade'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 61; ParamIndex: 0;
    ParamID: 71; ParamName: 'ClipLevMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 61; ParamIndex: 1;
    ParamID: 71; ParamName: 'ClipLev'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 61; ParamIndex: 2;
    ParamID: 6; ParamName: 'Shape'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 61; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 62; ParamIndex: 0;
    ParamID: 71; ParamName: 'AmountMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 62; ParamIndex: 1;
    ParamID: 71; ParamName: 'Amount'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 62; ParamIndex: 2;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 62; ParamIndex: 3;
    ParamID: 125; ParamName: 'Type'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 62; ParamIndex: 4; ParamID: 6;
    ParamName: 'Shape'; DefaultValue: 1; DefaultKnob: 3; ButtonParam: 4;
    slParamLabel: ''), (ModuleID: 63; ParamIndex: 0; ParamID: 157;
    ParamName: 'Ratio'; DefaultValue: 80; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 63; ParamIndex: 1; ParamID: 71;
    ParamName: 'RatioMod'; DefaultValue: 0; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 63; ParamIndex: 2; ParamID: 156;
    ParamName: 'Delay'; DefaultValue: 2; DefaultKnob: 2; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 63; ParamIndex: 3; ParamID: 106;
    ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3; ButtonParam: 3;
    slParamLabel: ''), (ModuleID: 66; ParamIndex: 0; ParamID: 96;
    ParamName: 'Inv1'; DefaultValue: 0; DefaultKnob: 1; ButtonParam: 0;
    slParamLabel: ''), (ModuleID: 66; ParamIndex: 1; ParamID: 97;
    ParamName: 'Lev1'; DefaultValue: 100; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 66; ParamIndex: 2; ParamID: 96;
    ParamName: 'Inv2'; DefaultValue: 0; DefaultKnob: 3; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 66; ParamIndex: 3; ParamID: 97;
    ParamName: 'Lev2'; DefaultValue: 100; DefaultKnob: 2; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 66; ParamIndex: 4; ParamID: 42;
    ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 4; ButtonParam: 4;
    slParamLabel: ''), (ModuleID: 68; ParamIndex: 0; ParamID: 146;
    ParamName: 'Tempo'; DefaultValue: 64; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 68; ParamIndex: 1; ParamID: 106;
    ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2; ButtonParam: 1;
    slParamLabel: ''), (ModuleID: 68; ParamIndex: 2; ParamID: 66;
    ParamName: 'Source'; DefaultValue: 0; DefaultKnob: 3; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 68; ParamIndex: 3; ParamID: 8;
    ParamName: 'BeatSync'; DefaultValue: 2; DefaultKnob: 1; ButtonParam: 3;
    slParamLabel: ''), (ModuleID: 68; ParamIndex: 4; ParamID: 9;
    ParamName: 'Swing'; DefaultValue: 0; DefaultKnob: 4; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 69; ParamIndex: 0; ParamID: 143;
    ParamName: 'Divider'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 71; ParamIndex: 0; ParamID: 30;
    ParamName: 'Attack'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 71; ParamIndex: 1; ParamID: 31;
    ParamName: 'Release'; DefaultValue: 20; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 72; ParamIndex: 0; ParamID: 104;
    ParamName: 'Range'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 74; ParamIndex: 0; ParamID: 71;
    ParamName: 'AmountMod'; DefaultValue: 0; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 74; ParamIndex: 1; ParamID: 71;
    ParamName: 'Amount'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 74; ParamIndex: 2; ParamID: 106;
    ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 75; ParamIndex: 0; ParamID: 104;
    ParamName: 'Range'; DefaultValue: 127; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 75; ParamIndex: 1; ParamID: 103;
    ParamName: 'Notes'; DefaultValue: 0; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 76; ParamIndex: 0; ParamID: 106;
    ParamName: 'On'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: 0;
    slParamLabel: 'On'), (ModuleID: 78; ParamIndex: 0; ParamID: 169;
    ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: 0;
    slParamLabel: 'Out 1;Out 2;Out 3;Out 4;Out 5;Out 6;Out 7;Out 8'),
    (ModuleID: 79; ParamIndex: 0; ParamID: 168; ParamName: 'Sel';
    DefaultValue: 0; DefaultKnob: 0; ButtonParam: 0;
    slParamLabel: 'In 1;In 2;In 3;In 4'), (ModuleID: 81; ParamIndex: 0;
    ParamID: 69; ParamName: 'Gain'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 81; ParamIndex: 1;
    ParamID: 84; ParamName: 'Type'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 82; ParamIndex: 0;
    ParamID: 148; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 82; ParamIndex: 1;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 83; ParamIndex: 0;
    ParamID: 161; ParamName: 'Mode'; DefaultValue: 1; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 83; ParamIndex: 1;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 84; ParamIndex: 0;
    ParamID: 34; ParamName: 'Shape'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 84; ParamIndex: 1;
    ParamID: 35; ParamName: 'Attack'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 84; ParamIndex: 2;
    ParamID: 33; ParamName: 'Reset'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 84; ParamIndex: 3;
    ParamID: 35; ParamName: 'Release'; DefaultValue: 54; DefaultKnob: 1;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 84; ParamIndex: 4;
    ParamID: 173; ParamName: 'TG'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 84; ParamIndex: 5;
    ParamID: 135; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 84; ParamIndex: 6;
    ParamID: 106; ParamName: 'KB'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 84; ParamIndex: 7; ParamID: 1;
    ParamName: 'DcyRel'; DefaultValue: 0; DefaultKnob: - 1; ButtonParam: 7;
    slParamLabel: ''), (ModuleID: 85; ParamIndex: 0; ParamID: 144;
    ParamName: 'ValFrom'; DefaultValue: 40; DefaultKnob: 0; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 85; ParamIndex: 1; ParamID: 144;
    ParamName: 'ValTo'; DefaultValue: 80; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 87; ParamIndex: 0; ParamID: 47;
    ParamName: 'Freq'; DefaultValue: 75; DefaultKnob: 0; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 87; ParamIndex: 1; ParamID: 71;
    ParamName: 'FreqMod'; DefaultValue: 0; DefaultKnob: 1; ButtonParam: - 1;
    slParamLabel: ''), (ModuleID: 87; ParamIndex: 2; ParamID: 67;
    ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1; ButtonParam: 2;
    slParamLabel: ''), (ModuleID: 87; ParamIndex: 3; ParamID: 106;
    ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2; ButtonParam: 3;
    slParamLabel: ''), (ModuleID: 88; ParamIndex: 0; ParamID: 168;
    ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0; ButtonParam: 0;
    slParamLabel: 'Out 1;Out 2;Out 3;Out 4'), (ModuleID: 89; ParamIndex: 0;
    ParamID: 45; ParamName: 'Rate'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 89; ParamIndex: 1;
    ParamID: 71; ParamName: 'Range'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 89; ParamIndex: 2;
    ParamID: 71; ParamName: 'FeedBack'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 89; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 90; ParamIndex: 0;
    ParamID: 167; ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: 'Out 1;Out 2'), (ModuleID: 92; ParamIndex: 0;
    ParamID: 47; ParamName: 'Freq'; DefaultValue: 75; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 92; ParamIndex: 1;
    ParamID: 72; ParamName: 'Freq M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 92; ParamIndex: 2;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 92; ParamIndex: 3;
    ParamID: 71; ParamName: 'Res'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 92; ParamIndex: 4;
    ParamID: 5; ParamName: 'dB/Oct'; DefaultValue: 2; DefaultKnob: 3;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 92; ParamIndex: 5;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 94; ParamIndex: 0;
    ParamID: 71; ParamName: 'Detune'; DefaultValue: 20; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 94; ParamIndex: 1;
    ParamID: 71; ParamName: 'Amount'; DefaultValue: 127; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 94; ParamIndex: 2;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 96; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 96; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 96; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 96; ParamIndex: 3;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 96; ParamIndex: 4;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 97; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 97; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 97; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 97; ParamIndex: 3;
    ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 97; ParamIndex: 4;
    ParamID: 118; ParamName: 'Wave'; DefaultValue: 2; DefaultKnob: 3;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 97; ParamIndex: 5;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 97; ParamIndex: 6;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 98; ParamIndex: 0;
    ParamID: 60; ParamName: 'FreqShift'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 98; ParamIndex: 1;
    ParamID: 71; ParamName: 'ShiftMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 98; ParamIndex: 2;
    ParamID: 61; ParamName: 'Range'; DefaultValue: 2; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 98; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 100; ParamIndex: 0;
    ParamID: 167; ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: 'In 1;In 2'), (ModuleID: 102; ParamIndex: 0;
    ParamID: 71; ParamName: 'Freq M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 102; ParamIndex: 1;
    ParamID: 54; ParamName: 'Freq'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 2;
    ParamID: 71; ParamName: 'SpreadMod'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 3;
    ParamID: 2; ParamName: 'FB'; DefaultValue: 64; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 4;
    ParamID: 48; ParamName: 'NotchCount'; DefaultValue: 2; DefaultKnob: 6;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 102; ParamIndex: 5;
    ParamID: 71; ParamName: 'Spread'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 6;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 7;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 102; ParamIndex: 7;
    ParamID: 71; ParamName: 'Level'; DefaultValue: 127; DefaultKnob: 8;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 8;
    ParamID: 71; ParamName: 'FBMod'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 102; ParamIndex: 9;
    ParamID: 49; ParamName: 'Type'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 102; ParamIndex: 10;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 103; ParamIndex: 0;
    ParamID: 55; ParamName: 'Freq'; DefaultValue: 60; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 103; ParamIndex: 1;
    ParamID: 36; ParamName: 'Gain'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 103; ParamIndex: 2;
    ParamID: 40; ParamName: 'BandWidth'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 103; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 103; ParamIndex: 4;
    ParamID: 71; ParamName: 'Level'; DefaultValue: 127; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 105; ParamIndex: 0;
    ParamID: 174; ParamName: 'Val'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 106; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 106; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 106; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 106; ParamIndex: 3;
    ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 106; ParamIndex: 4;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 106; ParamIndex: 5;
    ParamID: 71; ParamName: 'Width'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 106; ParamIndex: 6;
    ParamID: 71; ParamName: 'WidthMod'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 106; ParamIndex: 7;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 5;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 108; ParamIndex: 0;
    ParamID: 175; ParamName: 'BandSel_01'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 1;
    ParamID: 175; ParamName: 'BandSel_02'; DefaultValue: 2; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 2;
    ParamID: 175; ParamName: 'BandSel_03'; DefaultValue: 3; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 3;
    ParamID: 175; ParamName: 'BandSel_04'; DefaultValue: 4; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 4;
    ParamID: 175; ParamName: 'BandSel_05'; DefaultValue: 5; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 5;
    ParamID: 175; ParamName: 'BandSel_06'; DefaultValue: 6; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 6;
    ParamID: 175; ParamName: 'BandSel_07'; DefaultValue: 7; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 7;
    ParamID: 175; ParamName: 'BandSel_08'; DefaultValue: 8; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 8;
    ParamID: 175; ParamName: 'BandSel_09'; DefaultValue: 9; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 9;
    ParamID: 175; ParamName: 'BandSel_10'; DefaultValue: 10; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 10;
    ParamID: 175; ParamName: 'BandSel_11'; DefaultValue: 11; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 11;
    ParamID: 175; ParamName: 'BandSel_12'; DefaultValue: 12; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 12;
    ParamID: 175; ParamName: 'BandSel_13'; DefaultValue: 13; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 13;
    ParamID: 175; ParamName: 'BandSel_14'; DefaultValue: 14; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 14;
    ParamID: 175; ParamName: 'BandSel_15'; DefaultValue: 15; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 15;
    ParamID: 175; ParamName: 'BandSel_16'; DefaultValue: 16; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 16;
    ParamID: 106; ParamName: 'Emphasis'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 108; ParamIndex: 17;
    ParamID: 0; ParamName: 'Monitor'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 112; ParamIndex: 0;
    ParamID: 70; ParamName: 'Level'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 112; ParamIndex: 1;
    ParamID: 4; ParamName: 'BipUni'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 113; ParamIndex: 0;
    ParamID: 43; ParamName: 'Mix'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 113; ParamIndex: 1;
    ParamID: 71; ParamName: 'MixMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 114; ParamIndex: 0;
    ParamID: 44; ParamName: 'Mix'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 114; ParamIndex: 1;
    ParamID: 71; ParamName: 'MixMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 115; ParamIndex: 0;
    ParamID: 74; ParamName: 'L'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 115; ParamIndex: 1;
    ParamID: 56; ParamName: 'BP'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 115; ParamIndex: 2;
    ParamID: 74; ParamName: 'R'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 115; ParamIndex: 3;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 116; ParamIndex: 0;
    ParamID: 128; ParamName: 'Pad'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 117; ParamIndex: 0;
    ParamID: 71; ParamName: 'ModDepth'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 117; ParamIndex: 1;
    ParamID: 73; ParamName: 'ModType'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 118; ParamIndex: 0;
    ParamID: 21; ParamName: 'Bits'; DefaultValue: 11; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 118; ParamIndex: 1;
    ParamID: 22; ParamName: 'Rate'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 118; ParamIndex: 2;
    ParamID: 71; ParamName: 'Rate M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 118; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 119; ParamIndex: 0;
    ParamID: 106; ParamName: 'KB'; DefaultValue: 1; DefaultKnob: 7;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 119; ParamIndex: 1;
    ParamID: 34; ParamName: 'Shape'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 119; ParamIndex: 2;
    ParamID: 35; ParamName: 'Attack'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 119; ParamIndex: 3;
    ParamID: 35; ParamName: 'Decay1'; DefaultValue: 54; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 119; ParamIndex: 4;
    ParamID: 32; ParamName: 'Level1'; DefaultValue: 100; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 119; ParamIndex: 5;
    ParamID: 35; ParamName: 'Decay2'; DefaultValue: 54; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 119; ParamIndex: 6;
    ParamID: 32; ParamName: 'Level2'; DefaultValue: 70; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 119; ParamIndex: 7;
    ParamID: 35; ParamName: 'Release'; DefaultValue: 14; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 119; ParamIndex: 8;
    ParamID: 165; ParamName: 'SustainMode'; DefaultValue: 1; DefaultKnob: 6;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 119; ParamIndex: 9;
    ParamID: 137; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 119; ParamIndex: 10;
    ParamID: 33; ParamName: 'Reset'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 121; ParamIndex: 0;
    ParamID: 56; ParamName: 'Step 1'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 121; ParamIndex: 1;
    ParamID: 56; ParamName: 'Step 2'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 121; ParamIndex: 2;
    ParamID: 56; ParamName: 'Step 3'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 121; ParamIndex: 3;
    ParamID: 56; ParamName: 'Step 4'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 121; ParamIndex: 4;
    ParamID: 56; ParamName: 'Step 5'; DefaultValue: 64; DefaultKnob: 4;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 121; ParamIndex: 5;
    ParamID: 56; ParamName: 'Step 6'; DefaultValue: 64; DefaultKnob: 5;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 121; ParamIndex: 6;
    ParamID: 56; ParamName: 'Step 7'; DefaultValue: 64; DefaultKnob: 6;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 121; ParamIndex: 7;
    ParamID: 56; ParamName: 'Step 8'; DefaultValue: 64; DefaultKnob: 7;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 121; ParamIndex: 8;
    ParamID: 56; ParamName: 'Step 9'; DefaultValue: 64; DefaultKnob: 8;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 121; ParamIndex: 9;
    ParamID: 56; ParamName: 'Step 10'; DefaultValue: 64; DefaultKnob: 9;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 121; ParamIndex: 10;
    ParamID: 56; ParamName: 'Step 11'; DefaultValue: 64; DefaultKnob: 10;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 121; ParamIndex: 11;
    ParamID: 56; ParamName: 'Step 12'; DefaultValue: 64; DefaultKnob: 11;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 121; ParamIndex: 12;
    ParamID: 56; ParamName: 'Step 13'; DefaultValue: 64; DefaultKnob: 12;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 121; ParamIndex: 13;
    ParamID: 56; ParamName: 'Step 14'; DefaultValue: 64; DefaultKnob: 13;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 121; ParamIndex: 14;
    ParamID: 56; ParamName: 'Step 15'; DefaultValue: 64; DefaultKnob: 14;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 121; ParamIndex: 15;
    ParamID: 56; ParamName: 'Step 16'; DefaultValue: 64; DefaultKnob: 15;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 121; ParamIndex: 16;
    ParamID: 177; ParamName: 'Evnt 1'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 121; ParamIndex: 17;
    ParamID: 177; ParamName: 'Evnt 2'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 121; ParamIndex: 18;
    ParamID: 177; ParamName: 'Evnt 3'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 121; ParamIndex: 19;
    ParamID: 177; ParamName: 'Evnt 4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 121; ParamIndex: 20;
    ParamID: 177; ParamName: 'Evnt 5'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 121; ParamIndex: 21;
    ParamID: 177; ParamName: 'Evnt 6'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 121; ParamIndex: 22;
    ParamID: 177; ParamName: 'Evnt 7'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 121; ParamIndex: 23;
    ParamID: 177; ParamName: 'Evnt 8'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 121; ParamIndex: 24;
    ParamID: 177; ParamName: 'Evnt 9'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 121; ParamIndex: 25;
    ParamID: 177; ParamName: 'Evnt 10'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 121; ParamIndex: 26;
    ParamID: 177; ParamName: 'Evnt 11'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 121; ParamIndex: 27;
    ParamID: 177; ParamName: 'Evnt 12'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 121; ParamIndex: 28;
    ParamID: 177; ParamName: 'Evnt 13'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 121; ParamIndex: 29;
    ParamID: 177; ParamName: 'Evnt 14'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 121; ParamIndex: 30;
    ParamID: 177; ParamName: 'Evnt 15'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 121; ParamIndex: 31;
    ParamID: 177; ParamName: 'Evnt 16'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 121; ParamIndex: 32;
    ParamID: 89; ParamName: 'Cycles'; DefaultValue: 1; DefaultKnob: 17;
    ButtonParam: 32; slParamLabel: ''), (ModuleID: 121; ParamIndex: 33;
    ParamID: 159; ParamName: 'Length'; DefaultValue: 15; DefaultKnob: 18;
    ButtonParam: 33; slParamLabel: ''), (ModuleID: 121; ParamIndex: 34;
    ParamID: 173; ParamName: 'Pulse'; DefaultValue: 0; DefaultKnob: 16;
    ButtonParam: 34; slParamLabel: ''), (ModuleID: 121; ParamIndex: 35;
    ParamID: 106; ParamName: 'Clear'; DefaultValue: 0; DefaultKnob: 19;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 121; ParamIndex: 36;
    ParamID: 106; ParamName: 'Random'; DefaultValue: 0; DefaultKnob: 20;
    ButtonParam: 36; slParamLabel: ''), (ModuleID: 123; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 123; ParamIndex: 1;
    ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100; DefaultKnob: 1;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 123; ParamIndex: 2;
    ParamID: 97; ParamName: 'Lev3'; DefaultValue: 100; DefaultKnob: 2;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 123; ParamIndex: 3;
    ParamID: 97; ParamName: 'Lev4'; DefaultValue: 100; DefaultKnob: 3;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 123; ParamIndex: 4;
    ParamID: 106; ParamName: 'On1'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: 'Ch 1'), (ModuleID: 123; ParamIndex: 5;
    ParamID: 106; ParamName: 'On2'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 5; slParamLabel: 'Ch 2'), (ModuleID: 123; ParamIndex: 6;
    ParamID: 106; ParamName: 'On3'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 6; slParamLabel: 'Ch 3'), (ModuleID: 123; ParamIndex: 7;
    ParamID: 106; ParamName: 'On4'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 7; slParamLabel: 'Ch 4'), (ModuleID: 123; ParamIndex: 8;
    ParamID: 127; ParamName: 'Pad'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 123; ParamIndex: 9;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 125; ParamIndex: 0;
    ParamID: 71; ParamName: 'SweepMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 125; ParamIndex: 1;
    ParamID: 71; ParamName: 'Sweep'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 125; ParamIndex: 2;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 127; ParamIndex: 0;
    ParamID: 162; ParamName: 'Source'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 127; ParamIndex: 1;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 127; ParamIndex: 2;
    ParamID: 129; ParamName: 'Pad'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 134; ParamIndex: 0;
    ParamID: 47; ParamName: 'Freq'; DefaultValue: 60; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 134; ParamIndex: 1;
    ParamID: 71; ParamName: 'FreqMod'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 134; ParamIndex: 2;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 134; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 140; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 140; ParamIndex: 1;
    ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100; DefaultKnob: 1;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 140; ParamIndex: 2;
    ParamID: 97; ParamName: 'Lev3'; DefaultValue: 100; DefaultKnob: 2;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 140; ParamIndex: 3;
    ParamID: 97; ParamName: 'Lev4'; DefaultValue: 100; DefaultKnob: 3;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 140; ParamIndex: 4;
    ParamID: 106; ParamName: 'On1'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: 'Ch 1'), (ModuleID: 140; ParamIndex: 5;
    ParamID: 106; ParamName: 'On2'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 5; slParamLabel: 'Ch 2'), (ModuleID: 140; ParamIndex: 6;
    ParamID: 106; ParamName: 'On3'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 6; slParamLabel: 'Ch 3'), (ModuleID: 140; ParamIndex: 7;
    ParamID: 106; ParamName: 'On4'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 7; slParamLabel: 'Ch 4'), (ModuleID: 140; ParamIndex: 8;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 141; ParamIndex: 0;
    ParamID: 95; ParamName: 'Ctrl'; DefaultValue: 1; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 141; ParamIndex: 1;
    ParamID: 95; ParamName: 'Val'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 141; ParamIndex: 2;
    ParamID: 94; ParamName: 'Ch'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 142; ParamIndex: 0;
    ParamID: 95; ParamName: 'Program'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 142; ParamIndex: 1;
    ParamID: 92; ParamName: 'Ch'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 143; ParamIndex: 0;
    ParamID: 95; ParamName: 'Vel'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 143; ParamIndex: 1;
    ParamID: 95; ParamName: 'Note'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 143; ParamIndex: 2;
    ParamID: 94; ParamName: 'Ch'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 144; ParamIndex: 0;
    ParamID: 177; ParamName: 'Step 1'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 144; ParamIndex: 1;
    ParamID: 177; ParamName: 'Step 2'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 144; ParamIndex: 2;
    ParamID: 177; ParamName: 'Step 3'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 144; ParamIndex: 3;
    ParamID: 177; ParamName: 'Step 4'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 144; ParamIndex: 4;
    ParamID: 177; ParamName: 'Step 5'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 144; ParamIndex: 5;
    ParamID: 177; ParamName: 'Step 6'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 144; ParamIndex: 6;
    ParamID: 177; ParamName: 'Step 7'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 144; ParamIndex: 7;
    ParamID: 177; ParamName: 'Step 8'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 144; ParamIndex: 8;
    ParamID: 177; ParamName: 'Step 9'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 144; ParamIndex: 9;
    ParamID: 177; ParamName: 'Step 10'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 144; ParamIndex: 10;
    ParamID: 177; ParamName: 'Step 11'; DefaultValue: 0; DefaultKnob: 10;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 144; ParamIndex: 11;
    ParamID: 177; ParamName: 'Step 12'; DefaultValue: 0; DefaultKnob: 11;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 144; ParamIndex: 12;
    ParamID: 177; ParamName: 'Step 13'; DefaultValue: 0; DefaultKnob: 12;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 144; ParamIndex: 13;
    ParamID: 177; ParamName: 'Step 14'; DefaultValue: 0; DefaultKnob: 13;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 144; ParamIndex: 14;
    ParamID: 177; ParamName: 'Step 15'; DefaultValue: 0; DefaultKnob: 14;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 144; ParamIndex: 15;
    ParamID: 177; ParamName: 'Step 16'; DefaultValue: 0; DefaultKnob: 15;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 144; ParamIndex: 16;
    ParamID: 177; ParamName: 'Evnt 1'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 144; ParamIndex: 17;
    ParamID: 177; ParamName: 'Evnt 2'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 144; ParamIndex: 18;
    ParamID: 177; ParamName: 'Evnt 3'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 144; ParamIndex: 19;
    ParamID: 177; ParamName: 'Evnt 4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 144; ParamIndex: 20;
    ParamID: 177; ParamName: 'Evnt 5'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 144; ParamIndex: 21;
    ParamID: 177; ParamName: 'Evnt 6'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 144; ParamIndex: 22;
    ParamID: 177; ParamName: 'Evnt 7'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 144; ParamIndex: 23;
    ParamID: 177; ParamName: 'Evnt 8'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 144; ParamIndex: 24;
    ParamID: 177; ParamName: 'Evnt 9'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 144; ParamIndex: 25;
    ParamID: 177; ParamName: 'Evnt 10'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 144; ParamIndex: 26;
    ParamID: 177; ParamName: 'Evnt 11'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 144; ParamIndex: 27;
    ParamID: 177; ParamName: 'Evnt 12'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 144; ParamIndex: 28;
    ParamID: 177; ParamName: 'Evnt 13'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 144; ParamIndex: 29;
    ParamID: 177; ParamName: 'Evnt 14'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 144; ParamIndex: 30;
    ParamID: 177; ParamName: 'Evnt 15'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 144; ParamIndex: 31;
    ParamID: 177; ParamName: 'Evnt 16'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 144; ParamIndex: 32;
    ParamID: 89; ParamName: 'Cycles'; DefaultValue: 1; DefaultKnob: 17;
    ButtonParam: 32; slParamLabel: ''), (ModuleID: 144; ParamIndex: 33;
    ParamID: 159; ParamName: 'Length'; DefaultValue: 15; DefaultKnob: 18;
    ButtonParam: 33; slParamLabel: ''), (ModuleID: 144; ParamIndex: 34;
    ParamID: 173; ParamName: 'PulseUp'; DefaultValue: 0; DefaultKnob: 16;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 144; ParamIndex: 35;
    ParamID: 173; ParamName: 'PulseLo'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 145; ParamIndex: 0;
    ParamID: 70; ParamName: 'Step 1'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 145; ParamIndex: 1;
    ParamID: 70; ParamName: 'Step 2'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 145; ParamIndex: 2;
    ParamID: 70; ParamName: 'Step 3'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 145; ParamIndex: 3;
    ParamID: 70; ParamName: 'Step 4'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 145; ParamIndex: 4;
    ParamID: 70; ParamName: 'Step 5'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 145; ParamIndex: 5;
    ParamID: 70; ParamName: 'Step 6'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 145; ParamIndex: 6;
    ParamID: 70; ParamName: 'Step 7'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 145; ParamIndex: 7;
    ParamID: 70; ParamName: 'Step 8'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 145; ParamIndex: 8;
    ParamID: 70; ParamName: 'Step 9'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 145; ParamIndex: 9;
    ParamID: 70; ParamName: 'Step 10'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 145; ParamIndex: 10;
    ParamID: 70; ParamName: 'Step 11'; DefaultValue: 0; DefaultKnob: 10;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 145; ParamIndex: 11;
    ParamID: 70; ParamName: 'Step 12'; DefaultValue: 0; DefaultKnob: 11;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 145; ParamIndex: 12;
    ParamID: 70; ParamName: 'Step 13'; DefaultValue: 0; DefaultKnob: 12;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 145; ParamIndex: 13;
    ParamID: 70; ParamName: 'Step 14'; DefaultValue: 0; DefaultKnob: 13;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 145; ParamIndex: 14;
    ParamID: 70; ParamName: 'Step 15'; DefaultValue: 0; DefaultKnob: 14;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 145; ParamIndex: 15;
    ParamID: 70; ParamName: 'Step 16'; DefaultValue: 0; DefaultKnob: 15;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 145; ParamIndex: 16;
    ParamID: 177; ParamName: 'Evnt 1'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 145; ParamIndex: 17;
    ParamID: 177; ParamName: 'Evnt 2'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 145; ParamIndex: 18;
    ParamID: 177; ParamName: 'Evnt 3'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 145; ParamIndex: 19;
    ParamID: 177; ParamName: 'Evnt 4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 145; ParamIndex: 20;
    ParamID: 177; ParamName: 'Evnt 5'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 145; ParamIndex: 21;
    ParamID: 177; ParamName: 'Evnt 6'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 145; ParamIndex: 22;
    ParamID: 177; ParamName: 'Evnt 7'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 145; ParamIndex: 23;
    ParamID: 177; ParamName: 'Evnt 8'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 145; ParamIndex: 24;
    ParamID: 177; ParamName: 'Evnt 9'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 145; ParamIndex: 25;
    ParamID: 177; ParamName: 'Evnt 10'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 145; ParamIndex: 26;
    ParamID: 177; ParamName: 'Evnt 11'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 145; ParamIndex: 27;
    ParamID: 177; ParamName: 'Evnt 12'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 145; ParamIndex: 28;
    ParamID: 177; ParamName: 'Evnt 13'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 145; ParamIndex: 29;
    ParamID: 177; ParamName: 'Evnt 14'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 145; ParamIndex: 30;
    ParamID: 177; ParamName: 'Evnt 15'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 145; ParamIndex: 31;
    ParamID: 177; ParamName: 'Evnt 16'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 145; ParamIndex: 32;
    ParamID: 89; ParamName: 'Cycles'; DefaultValue: 1; DefaultKnob: 17;
    ButtonParam: 32; slParamLabel: ''), (ModuleID: 145; ParamIndex: 33;
    ParamID: 159; ParamName: 'Length'; DefaultValue: 15; DefaultKnob: 18;
    ButtonParam: 33; slParamLabel: ''), (ModuleID: 145; ParamIndex: 34;
    ParamID: 4; ParamName: 'Pol'; DefaultValue: 0; DefaultKnob: 21;
    ButtonParam: 34; slParamLabel: ''), (ModuleID: 145; ParamIndex: 35;
    ParamID: 173; ParamName: 'Pulse'; DefaultValue: 0; DefaultKnob: 16;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 145; ParamIndex: 36;
    ParamID: 106; ParamName: 'Clear'; DefaultValue: 0; DefaultKnob: 19;
    ButtonParam: 36; slParamLabel: ''), (ModuleID: 145; ParamIndex: 37;
    ParamID: 106; ParamName: 'Random'; DefaultValue: 0; DefaultKnob: 20;
    ButtonParam: 37; slParamLabel: ''), (ModuleID: 146; ParamIndex: 0;
    ParamID: 70; ParamName: 'Step 1'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 146; ParamIndex: 1;
    ParamID: 70; ParamName: 'Step 2'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 146; ParamIndex: 2;
    ParamID: 70; ParamName: 'Step 3'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 146; ParamIndex: 3;
    ParamID: 70; ParamName: 'Step 4'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 146; ParamIndex: 4;
    ParamID: 70; ParamName: 'Step 5'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 146; ParamIndex: 5;
    ParamID: 70; ParamName: 'Step 6'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 146; ParamIndex: 6;
    ParamID: 70; ParamName: 'Step 7'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 146; ParamIndex: 7;
    ParamID: 70; ParamName: 'Step 8'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 146; ParamIndex: 8;
    ParamID: 70; ParamName: 'Step 9'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 146; ParamIndex: 9;
    ParamID: 70; ParamName: 'Step 10'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 146; ParamIndex: 10;
    ParamID: 70; ParamName: 'Step 11'; DefaultValue: 0; DefaultKnob: 10;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 146; ParamIndex: 11;
    ParamID: 70; ParamName: 'Step 12'; DefaultValue: 0; DefaultKnob: 11;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 146; ParamIndex: 12;
    ParamID: 70; ParamName: 'Step 13'; DefaultValue: 0; DefaultKnob: 12;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 146; ParamIndex: 13;
    ParamID: 70; ParamName: 'Step 14'; DefaultValue: 0; DefaultKnob: 13;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 146; ParamIndex: 14;
    ParamID: 70; ParamName: 'Step 15'; DefaultValue: 0; DefaultKnob: 14;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 146; ParamIndex: 15;
    ParamID: 70; ParamName: 'Step 16'; DefaultValue: 0; DefaultKnob: 15;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 146; ParamIndex: 16;
    ParamID: 177; ParamName: 'Evnt 1'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 146; ParamIndex: 17;
    ParamID: 177; ParamName: 'Evnt 2'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 146; ParamIndex: 18;
    ParamID: 177; ParamName: 'Evnt 3'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 146; ParamIndex: 19;
    ParamID: 177; ParamName: 'Evnt 4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 146; ParamIndex: 20;
    ParamID: 177; ParamName: 'Evnt 5'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 146; ParamIndex: 21;
    ParamID: 177; ParamName: 'Evnt 6'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 146; ParamIndex: 22;
    ParamID: 177; ParamName: 'Evnt 7'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 146; ParamIndex: 23;
    ParamID: 177; ParamName: 'Evnt 8'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 146; ParamIndex: 24;
    ParamID: 177; ParamName: 'Evnt 9'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 146; ParamIndex: 25;
    ParamID: 177; ParamName: 'Evnt 10'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 146; ParamIndex: 26;
    ParamID: 177; ParamName: 'Evnt 11'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 146; ParamIndex: 27;
    ParamID: 177; ParamName: 'Evnt 12'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 146; ParamIndex: 28;
    ParamID: 177; ParamName: 'Evnt 13'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 146; ParamIndex: 29;
    ParamID: 177; ParamName: 'Evnt 14'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 146; ParamIndex: 30;
    ParamID: 177; ParamName: 'Evnt 15'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 146; ParamIndex: 31;
    ParamID: 177; ParamName: 'Evnt 16'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 146; ParamIndex: 32;
    ParamID: 89; ParamName: 'Cycles'; DefaultValue: 1; DefaultKnob: 17;
    ButtonParam: 32; slParamLabel: ''), (ModuleID: 146; ParamIndex: 33;
    ParamID: 159; ParamName: 'Length'; DefaultValue: 15; DefaultKnob: 18;
    ButtonParam: 33; slParamLabel: ''), (ModuleID: 146; ParamIndex: 34;
    ParamID: 4; ParamName: 'Pol'; DefaultValue: 1; DefaultKnob: 21;
    ButtonParam: 34; slParamLabel: ''), (ModuleID: 146; ParamIndex: 35;
    ParamID: 173; ParamName: 'Pulse'; DefaultValue: 0; DefaultKnob: 16;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 146; ParamIndex: 36;
    ParamID: 106; ParamName: 'Clear'; DefaultValue: 0; DefaultKnob: 19;
    ButtonParam: 36; slParamLabel: ''), (ModuleID: 146; ParamIndex: 37;
    ParamID: 106; ParamName: 'Random'; DefaultValue: 0; DefaultKnob: 20;
    ButtonParam: 37; slParamLabel: ''), (ModuleID: 147; ParamIndex: 0;
    ParamID: 95; ParamName: 'Ctrl'; DefaultValue: 7; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 147; ParamIndex: 1;
    ParamID: 92; ParamName: 'Ch'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 148; ParamIndex: 0;
    ParamID: 95; ParamName: 'Note'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 148; ParamIndex: 1;
    ParamID: 93; ParamName: 'Ch'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 0;
    ParamID: 93; ParamName: 'RcvCh'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 1;
    ParamID: 95; ParamName: 'RcvMin'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 2;
    ParamID: 95; ParamName: 'RcvMax'; DefaultValue: 127; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 3;
    ParamID: 2; ParamName: 'SendTrans'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 4;
    ParamID: 94; ParamName: 'SendCh'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 149; ParamIndex: 5;
    ParamID: 105; ParamName: 'ThruMode'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 150; ParamIndex: 0;
    ParamID: 172; ParamName: 'Treshold'; DefaultValue: 18; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 150; ParamIndex: 1;
    ParamID: 12; ParamName: 'Ratio'; DefaultValue: 20; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 150; ParamIndex: 2;
    ParamID: 11; ParamName: 'Attack'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 150; ParamIndex: 3;
    ParamID: 14; ParamName: 'Release'; DefaultValue: 20; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 150; ParamIndex: 4;
    ParamID: 13; ParamName: 'RefLevel'; DefaultValue: 30; DefaultKnob: 4;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 150; ParamIndex: 5;
    ParamID: 106; ParamName: 'SideChain'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 150; ParamIndex: 6;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 152; ParamIndex: 0;
    ParamID: 104; ParamName: 'Range'; DefaultValue: 127; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 152; ParamIndex: 1;
    ParamID: 68; ParamName: 'Capture'; DefaultValue: 0; DefaultKnob: 13;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 152; ParamIndex: 2;
    ParamID: 106; ParamName: 'Note E'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 152; ParamIndex: 3;
    ParamID: 106; ParamName: 'Note F'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 152; ParamIndex: 4;
    ParamID: 106; ParamName: 'Note F#'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 152; ParamIndex: 5;
    ParamID: 106; ParamName: 'Note G'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 152; ParamIndex: 6;
    ParamID: 106; ParamName: 'Note G#'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 152; ParamIndex: 7;
    ParamID: 106; ParamName: 'Note A'; DefaultValue: 0; DefaultKnob: 10;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 152; ParamIndex: 8;
    ParamID: 106; ParamName: 'Note A#'; DefaultValue: 0; DefaultKnob: 11;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 152; ParamIndex: 9;
    ParamID: 106; ParamName: 'Note B'; DefaultValue: 0; DefaultKnob: 12;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 152; ParamIndex: 10;
    ParamID: 106; ParamName: 'Note C'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 152; ParamIndex: 11;
    ParamID: 106; ParamName: 'Note C#'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 11; slParamLabel: ''), (ModuleID: 152; ParamIndex: 12;
    ParamID: 106; ParamName: 'Note D'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 12; slParamLabel: ''), (ModuleID: 152; ParamIndex: 13;
    ParamID: 106; ParamName: 'Note D#'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 13; slParamLabel: ''), (ModuleID: 154; ParamIndex: 0;
    ParamID: 70; ParamName: 'Step 1'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 154; ParamIndex: 1;
    ParamID: 70; ParamName: 'Step 2'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 154; ParamIndex: 2;
    ParamID: 70; ParamName: 'Step 3'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 154; ParamIndex: 3;
    ParamID: 70; ParamName: 'Step 4'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 154; ParamIndex: 4;
    ParamID: 70; ParamName: 'Step 5'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 154; ParamIndex: 5;
    ParamID: 70; ParamName: 'Step 6'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 154; ParamIndex: 6;
    ParamID: 70; ParamName: 'Step 7'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 154; ParamIndex: 7;
    ParamID: 70; ParamName: 'Step 8'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 154; ParamIndex: 8;
    ParamID: 70; ParamName: 'Step 9'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 154; ParamIndex: 9;
    ParamID: 70; ParamName: 'Step 10'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 154; ParamIndex: 10;
    ParamID: 70; ParamName: 'Step 11'; DefaultValue: 0; DefaultKnob: 10;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 154; ParamIndex: 11;
    ParamID: 70; ParamName: 'Step 12'; DefaultValue: 0; DefaultKnob: 11;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 154; ParamIndex: 12;
    ParamID: 70; ParamName: 'Step 13'; DefaultValue: 0; DefaultKnob: 12;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 154; ParamIndex: 13;
    ParamID: 70; ParamName: 'Step 14'; DefaultValue: 0; DefaultKnob: 13;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 154; ParamIndex: 14;
    ParamID: 70; ParamName: 'Step 15'; DefaultValue: 0; DefaultKnob: 14;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 154; ParamIndex: 15;
    ParamID: 70; ParamName: 'Step 16'; DefaultValue: 0; DefaultKnob: 15;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 154; ParamIndex: 16;
    ParamID: 177; ParamName: 'Evnt 1'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 154; ParamIndex: 17;
    ParamID: 177; ParamName: 'Evnt 2'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 154; ParamIndex: 18;
    ParamID: 177; ParamName: 'Evnt 3'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 18; slParamLabel: ''), (ModuleID: 154; ParamIndex: 19;
    ParamID: 177; ParamName: 'Evnt 4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 19; slParamLabel: ''), (ModuleID: 154; ParamIndex: 20;
    ParamID: 177; ParamName: 'Evnt 5'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 20; slParamLabel: ''), (ModuleID: 154; ParamIndex: 21;
    ParamID: 177; ParamName: 'Evnt 6'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 21; slParamLabel: ''), (ModuleID: 154; ParamIndex: 22;
    ParamID: 177; ParamName: 'Evnt 7'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 22; slParamLabel: ''), (ModuleID: 154; ParamIndex: 23;
    ParamID: 177; ParamName: 'Evnt 8'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 23; slParamLabel: ''), (ModuleID: 154; ParamIndex: 24;
    ParamID: 177; ParamName: 'Evnt 9'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 24; slParamLabel: ''), (ModuleID: 154; ParamIndex: 25;
    ParamID: 177; ParamName: 'Evnt 10'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 25; slParamLabel: ''), (ModuleID: 154; ParamIndex: 26;
    ParamID: 177; ParamName: 'Evnt 11'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 26; slParamLabel: ''), (ModuleID: 154; ParamIndex: 27;
    ParamID: 177; ParamName: 'Evnt 12'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 27; slParamLabel: ''), (ModuleID: 154; ParamIndex: 28;
    ParamID: 177; ParamName: 'Evnt 13'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 28; slParamLabel: ''), (ModuleID: 154; ParamIndex: 29;
    ParamID: 177; ParamName: 'Evnt 14'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 29; slParamLabel: ''), (ModuleID: 154; ParamIndex: 30;
    ParamID: 177; ParamName: 'Evnt 15'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 30; slParamLabel: ''), (ModuleID: 154; ParamIndex: 31;
    ParamID: 177; ParamName: 'Evnt 16'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 31; slParamLabel: ''), (ModuleID: 154; ParamIndex: 32;
    ParamID: 173; ParamName: 'Pulse'; DefaultValue: 0; DefaultKnob: 16;
    ButtonParam: 32; slParamLabel: ''), (ModuleID: 154; ParamIndex: 33;
    ParamID: 4; ParamName: 'Pol'; DefaultValue: 0; DefaultKnob: 20;
    ButtonParam: 33; slParamLabel: ''), (ModuleID: 154; ParamIndex: 34;
    ParamID: 158; ParamName: 'XFade'; DefaultValue: 0; DefaultKnob: 17;
    ButtonParam: 34; slParamLabel: ''), (ModuleID: 154; ParamIndex: 35;
    ParamID: 106; ParamName: 'Random'; DefaultValue: 0; DefaultKnob: 19;
    ButtonParam: 35; slParamLabel: ''), (ModuleID: 154; ParamIndex: 36;
    ParamID: 106; ParamName: 'Clear'; DefaultValue: 0; DefaultKnob: 18;
    ButtonParam: 36; slParamLabel: ''), (ModuleID: 156; ParamIndex: 0;
    ParamID: 56; ParamName: 'Note'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 157; ParamIndex: 0;
    ParamID: 137; ParamName: 'OutType'; DefaultValue: 4; DefaultKnob: 1;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 157; ParamIndex: 1;
    ParamID: 3; ParamName: 'InputType'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 158; ParamIndex: 0;
    ParamID: 64; ParamName: 'Time'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 158; ParamIndex: 1;
    ParamID: 106; ParamName: 'Glide'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 158; ParamIndex: 2;
    ParamID: 88; ParamName: 'Shape'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 161; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 161; ParamIndex: 1;
    ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100; DefaultKnob: 1;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 161; ParamIndex: 2;
    ParamID: 97; ParamName: 'Lev3'; DefaultValue: 100; DefaultKnob: 2;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 161; ParamIndex: 3;
    ParamID: 97; ParamName: 'Lev4'; DefaultValue: 100; DefaultKnob: 3;
    ButtonParam: 11; slParamLabel: ''), (ModuleID: 161; ParamIndex: 4;
    ParamID: 97; ParamName: 'Lev5'; DefaultValue: 100; DefaultKnob: 4;
    ButtonParam: 12; slParamLabel: ''), (ModuleID: 161; ParamIndex: 5;
    ParamID: 97; ParamName: 'Lev6'; DefaultValue: 100; DefaultKnob: 5;
    ButtonParam: 13; slParamLabel: ''), (ModuleID: 161; ParamIndex: 6;
    ParamID: 97; ParamName: 'Lev7'; DefaultValue: 100; DefaultKnob: 6;
    ButtonParam: 14; slParamLabel: ''), (ModuleID: 161; ParamIndex: 7;
    ParamID: 97; ParamName: 'Lev8'; DefaultValue: 100; DefaultKnob: 7;
    ButtonParam: 15; slParamLabel: ''), (ModuleID: 161; ParamIndex: 8;
    ParamID: 106; ParamName: 'On1'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 8; slParamLabel: 'Ch 1'), (ModuleID: 161; ParamIndex: 9;
    ParamID: 106; ParamName: 'On2'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 9; slParamLabel: 'Ch 2'), (ModuleID: 161; ParamIndex: 10;
    ParamID: 106; ParamName: 'On3'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 10; slParamLabel: 'Ch 3'), (ModuleID: 161; ParamIndex: 11;
    ParamID: 106; ParamName: 'On4'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 11; slParamLabel: 'Ch 4'), (ModuleID: 161; ParamIndex: 12;
    ParamID: 106; ParamName: 'On5'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 12; slParamLabel: 'Ch 5'), (ModuleID: 161; ParamIndex: 13;
    ParamID: 106; ParamName: 'On6'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 13; slParamLabel: 'Ch 6'), (ModuleID: 161; ParamIndex: 14;
    ParamID: 106; ParamName: 'On7'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 14; slParamLabel: 'Ch 7'), (ModuleID: 161; ParamIndex: 15;
    ParamID: 106; ParamName: 'On8'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 15; slParamLabel: 'Ch 8'), (ModuleID: 161; ParamIndex: 16;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 16; slParamLabel: ''), (ModuleID: 161; ParamIndex: 17;
    ParamID: 128; ParamName: 'Pad'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 17; slParamLabel: ''), (ModuleID: 162; ParamIndex: 0;
    ParamID: 53; ParamName: 'Freq'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 162; ParamIndex: 1;
    ParamID: 71; ParamName: 'Freq M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 162; ParamIndex: 2;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 162; ParamIndex: 3;
    ParamID: 2; ParamName: 'FB'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 162; ParamIndex: 4;
    ParamID: 71; ParamName: 'FBMod'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 162; ParamIndex: 5;
    ParamID: 10; ParamName: 'Type'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 162; ParamIndex: 6;
    ParamID: 71; ParamName: 'Lev'; DefaultValue: 127; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 162; ParamIndex: 7;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 6;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 163; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 163; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 163; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 163; ParamIndex: 3;
    ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 163; ParamIndex: 4;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 163; ParamIndex: 5;
    ParamID: 71; ParamName: 'Fm M'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 163; ParamIndex: 6;
    ParamID: 52; ParamName: 'Fm PTrk'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 163; ParamIndex: 7;
    ParamID: 141; ParamName: 'Shape'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 163; ParamIndex: 8;
    ParamID: 71; ParamName: 'Shape M'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 163; ParamIndex: 9;
    ParamID: 120; ParamName: 'Wave'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 163; ParamIndex: 10;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 7;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 164; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 164; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 164; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 164; ParamIndex: 3;
    ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 4;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 164; ParamIndex: 5;
    ParamID: 71; ParamName: 'SqrLevel'; DefaultValue: 127; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 6;
    ParamID: 71; ParamName: 'PWMod'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 7;
    ParamID: 71; ParamName: 'SawLevel'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 8;
    ParamID: 131; ParamName: 'SawPhase'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 9;
    ParamID: 71; ParamName: 'SubOctLevel'; DefaultValue: 0; DefaultKnob: 9;
    ButtonParam: 13; slParamLabel: ''), (ModuleID: 164; ParamIndex: 10;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 10;
    ButtonParam: 10; slParamLabel: ''), (ModuleID: 164; ParamIndex: 11;
    ParamID: 141; ParamName: 'SqrPW'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 12;
    ParamID: 71; ParamName: 'Phase M'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 164; ParamIndex: 13;
    ParamID: 106; ParamName: 'Soft'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 13; slParamLabel: ''), (ModuleID: 165; ParamIndex: 0;
    ParamID: 28; ParamName: 'Algorithm'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 165; ParamIndex: 1;
    ParamID: 29; ParamName: 'Feedback'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 167; ParamIndex: 0;
    ParamID: 138; ParamName: 'ShiftSemi'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 167; ParamIndex: 1;
    ParamID: 139; ParamName: 'ShiftFine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 167; ParamIndex: 2;
    ParamID: 71; ParamName: 'ShiftMod'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 167; ParamIndex: 3;
    ParamID: 156; ParamName: 'Delay'; DefaultValue: 2; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 167; ParamIndex: 4;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 169; ParamIndex: 0;
    ParamID: 35; ParamName: 'Attack'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 1;
    ParamID: 35; ParamName: 'Hold'; DefaultValue: 32; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 2;
    ParamID: 35; ParamName: 'Decay'; DefaultValue: 14; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 3;
    ParamID: 71; ParamName: 'Atk M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 4;
    ParamID: 71; ParamName: 'Hold M'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 5;
    ParamID: 71; ParamName: 'Dcy M'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 169; ParamIndex: 6;
    ParamID: 135; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 169; ParamIndex: 7;
    ParamID: 106; ParamName: 'KB'; DefaultValue: 1; DefaultKnob: 7;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 170; ParamIndex: 0;
    ParamID: 163; ParamName: 'Source'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 170; ParamIndex: 1;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 170; ParamIndex: 2;
    ParamID: 129; ParamName: 'Pad'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 171; ParamIndex: 0;
    ParamID: 164; ParamName: 'Source'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 171; ParamIndex: 1;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 171; ParamIndex: 2;
    ParamID: 129; ParamName: 'Pad'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 172; ParamIndex: 0;
    ParamID: 20; ParamName: 'Time'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 173; ParamIndex: 0;
    ParamID: 20; ParamName: 'Time'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 173; ParamIndex: 1;
    ParamID: 71; ParamName: 'TimeMod'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 174; ParamIndex: 0;
    ParamID: 20; ParamName: 'Time1'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 174; ParamIndex: 1;
    ParamID: 71; ParamName: 'Time1Mod'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 174; ParamIndex: 2;
    ParamID: 20; ParamName: 'Time2'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 174; ParamIndex: 3;
    ParamID: 71; ParamName: 'Time2Mod'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 0;
    ParamID: 20; ParamName: 'Time1'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 1;
    ParamID: 71; ParamName: 'Time1Mod'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 2;
    ParamID: 20; ParamName: 'Time2'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 3;
    ParamID: 71; ParamName: 'Time2Mod'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 4;
    ParamID: 20; ParamName: 'Time3'; DefaultValue: 64; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 5;
    ParamID: 71; ParamName: 'Time3Mod'; DefaultValue: 64; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 6;
    ParamID: 20; ParamName: 'Time4'; DefaultValue: 64; DefaultKnob: 6;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 7;
    ParamID: 71; ParamName: 'Time4Mod'; DefaultValue: 64; DefaultKnob: 7;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175; ParamIndex: 8;
    ParamID: 170; ParamName: 'TimeClk'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 176; ParamIndex: 0;
    ParamID: 19; ParamName: 'Time'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 176; ParamIndex: 1;
    ParamID: 71; ParamName: 'FB'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 176; ParamIndex: 2;
    ParamID: 71; ParamName: 'Filter'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 176; ParamIndex: 3;
    ParamID: 71; ParamName: 'DryWet'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 176; ParamIndex: 4;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 176; ParamIndex: 5;
    ParamID: 170; ParamName: 'TimeClk'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 177; ParamIndex: 0;
    ParamID: 19; ParamName: 'Time'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177; ParamIndex: 1;
    ParamID: 71; ParamName: 'FB'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177; ParamIndex: 2;
    ParamID: 71; ParamName: 'LP'; DefaultValue: 127; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177; ParamIndex: 3;
    ParamID: 71; ParamName: 'DryWet'; DefaultValue: 64; DefaultKnob: 4;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 177; ParamIndex: 4;
    ParamID: 170; ParamName: 'TimeClk'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 177; ParamIndex: 5;
    ParamID: 71; ParamName: 'FBMod'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177; ParamIndex: 6;
    ParamID: 71; ParamName: 'DryWetMod'; DefaultValue: 0; DefaultKnob: 7;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177; ParamIndex: 7;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 177; ParamIndex: 8;
    ParamID: 71; ParamName: 'HP'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 178; ParamIndex: 0;
    ParamID: 143; ParamName: 'Delay'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 0;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 1;
    ParamID: 106; ParamName: 'Sync'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 2;
    ParamID: 147; ParamName: 'RatioFixed'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 3;
    ParamID: 111; ParamName: 'Coarse'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 4;
    ParamID: 113; ParamName: 'Fine'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 5;
    ParamID: 112; ParamName: 'FreqDetune'; DefaultValue: 7; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 6;
    ParamID: 117; ParamName: 'Vel'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 7;
    ParamID: 115; ParamName: 'RateScale'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 8;
    ParamID: 116; ParamName: 'R1'; DefaultValue: 90; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 9;
    ParamID: 114; ParamName: 'L1'; DefaultValue: 99; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 10;
    ParamID: 116; ParamName: 'R2'; DefaultValue: 80; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 11;
    ParamID: 114; ParamName: 'L2'; DefaultValue: 99; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 12;
    ParamID: 116; ParamName: 'R3'; DefaultValue: 70; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 13;
    ParamID: 114; ParamName: 'L3'; DefaultValue: 99; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 14;
    ParamID: 116; ParamName: 'R4'; DefaultValue: 70; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 15;
    ParamID: 114; ParamName: 'L4'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 16;
    ParamID: 107; ParamName: 'AMod'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 17;
    ParamID: 108; ParamName: 'BrPoint'; DefaultValue: 50; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 18;
    ParamID: 110; ParamName: 'LDepthMode'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 19;
    ParamID: 109; ParamName: 'LDepth'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 20;
    ParamID: 110; ParamName: 'RDepthMode'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 21;
    ParamID: 109; ParamName: 'RDepth'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 22;
    ParamID: 114; ParamName: 'OutLevel'; DefaultValue: 99; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 23;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 180; ParamIndex: 24;
    ParamID: 106; ParamName: 'EnvKB'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 181; ParamIndex: 0;
    ParamID: 20; ParamName: 'Time'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 0;
    ParamID: 18; ParamName: 'TimeLeft'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 1;
    ParamID: 18; ParamName: 'TimeRight'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 2;
    ParamID: 71; ParamName: 'FBLeft'; DefaultValue: 64; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 3;
    ParamID: 71; ParamName: 'FBRight'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 4;
    ParamID: 71; ParamName: 'XFBLeft'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 5;
    ParamID: 71; ParamName: 'XFBRight'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 6;
    ParamID: 170; ParamName: 'TimeClk'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 182; ParamIndex: 7;
    ParamID: 71; ParamName: 'LP'; DefaultValue: 127; DefaultKnob: 7;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182; ParamIndex: 8;
    ParamID: 71; ParamName: 'DryWet'; DefaultValue: 64; DefaultKnob: 9;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 182; ParamIndex: 9;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 9; slParamLabel: ''), (ModuleID: 182; ParamIndex: 10;
    ParamID: 71; ParamName: 'HP'; DefaultValue: 0; DefaultKnob: 8;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 183; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 183; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 183; ParamIndex: 2;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 183; ParamIndex: 3;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 183; ParamIndex: 4;
    ParamID: 71; ParamName: 'Phase M'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 183; ParamIndex: 5;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 183; ParamIndex: 6;
    ParamID: 71; ParamName: 'FreqMod'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 184; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 184; ParamIndex: 1;
    ParamID: 106; ParamName: 'On'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: 'Ch 1'), (ModuleID: 184; ParamIndex: 2;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 185; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 185; ParamIndex: 1;
    ParamID: 106; ParamName: 'On'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: 'Ch 1'), (ModuleID: 185; ParamIndex: 2;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 186; ParamIndex: 0;
    ParamID: 106; ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: 'Switch'), (ModuleID: 187; ParamIndex: 0;
    ParamID: 106; ParamName: 'Sel'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 188; ParamIndex: 0;
    ParamID: 70; ParamName: 'Lev'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 188; ParamIndex: 1;
    ParamID: 106; ParamName: 'On'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: 'Switch'), (ModuleID: 188; ParamIndex: 2;
    ParamID: 4; ParamName: 'BipUni'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 189; ParamIndex: 0;
    ParamID: 171; ParamName: 'Treshold'; DefaultValue: 20; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 189; ParamIndex: 1;
    ParamID: 101; ParamName: 'Attack'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 189; ParamIndex: 2;
    ParamID: 102; ParamName: 'Release'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 189; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 190; ParamIndex: 0;
    ParamID: 80; ParamName: 'Rate'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 190; ParamIndex: 1;
    ParamID: 71; ParamName: 'Rate M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 190; ParamIndex: 2;
    ParamID: 78; ParamName: 'Range'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 190; ParamIndex: 3;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 190; ParamIndex: 4;
    ParamID: 76; ParamName: 'Wave'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 190; ParamIndex: 5;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 190; ParamIndex: 6;
    ParamID: 131; ParamName: 'Phase'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 190; ParamIndex: 7;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 8;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 190; ParamIndex: 8;
    ParamID: 124; ParamName: 'OutType'; DefaultValue: 4; DefaultKnob: 7;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 190; ParamIndex: 9;
    ParamID: 71; ParamName: 'Phase M'; DefaultValue: 0; DefaultKnob: 6;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 192; ParamIndex: 0;
    ParamID: 133; ParamName: 'Type'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 192; ParamIndex: 1;
    ParamID: 132; ParamName: 'Freq'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 192; ParamIndex: 2;
    ParamID: 71; ParamName: 'FeedBack'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 192; ParamIndex: 3;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 194; ParamIndex: 0;
    ParamID: 97; ParamName: 'Lev1'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 194; ParamIndex: 1;
    ParamID: 106; ParamName: 'On1'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: 'Ch 1'), (ModuleID: 194; ParamIndex: 2;
    ParamID: 97; ParamName: 'Lev2'; DefaultValue: 100; DefaultKnob: 1;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 194; ParamIndex: 3;
    ParamID: 106; ParamName: 'On2'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 3; slParamLabel: 'Ch 2'), (ModuleID: 194; ParamIndex: 4;
    ParamID: 42; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 195; ParamIndex: 0;
    ParamID: 71; ParamName: 'ModDepth'; DefaultValue: 100; DefaultKnob: 0;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 195; ParamIndex: 1;
    ParamID: 106; ParamName: 'On'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: 1; slParamLabel: 'On'), (ModuleID: 195; ParamIndex: 2;
    ParamID: 41; ParamName: 'ExpLin'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 195; ParamIndex: 3;
    ParamID: 98; ParamName: 'InvertMode'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 196; ParamIndex: 0;
    ParamID: 56; ParamName: 'Coarse'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 196; ParamIndex: 1;
    ParamID: 57; ParamName: 'Fine'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 196; ParamIndex: 2;
    ParamID: 59; ParamName: 'Tune Md'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 196; ParamIndex: 3;
    ParamID: 106; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: - 1;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 196; ParamIndex: 4;
    ParamID: 71; ParamName: 'Pitch M'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 196; ParamIndex: 5;
    ParamID: 71; ParamName: 'Decay'; DefaultValue: 64; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 196; ParamIndex: 6;
    ParamID: 71; ParamName: 'Click'; DefaultValue: 64; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 196; ParamIndex: 7;
    ParamID: 106; ParamName: 'Punch'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 196; ParamIndex: 8;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 6;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 198; ParamIndex: 0;
    ParamID: 171; ParamName: 'Treshold'; DefaultValue: 20; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 199; ParamIndex: 0;
    ParamID: 99; ParamName: 'Priorty'; DefaultValue: 0; DefaultKnob: 0;
    ButtonParam: 0; slParamLabel: ''), (ModuleID: 200; ParamIndex: 0;
    ParamID: 79; ParamName: 'Rate'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 200; ParamIndex: 1;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 200; ParamIndex: 2;
    ParamID: 3; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 200; ParamIndex: 3;
    ParamID: 77; ParamName: 'Range'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 200; ParamIndex: 4;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 200; ParamIndex: 5;
    ParamID: 152; ParamName: 'Edge'; DefaultValue: 4; DefaultKnob: 2;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 200; ParamIndex: 6;
    ParamID: 142; ParamName: 'Step'; DefaultValue: 3; DefaultKnob: 5;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 202; ParamIndex: 0;
    ParamID: 79; ParamName: 'Rate'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: 7; slParamLabel: ''), (ModuleID: 202; ParamIndex: 1;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 202; ParamIndex: 2;
    ParamID: 67; ParamName: 'KBT'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 202; ParamIndex: 3;
    ParamID: 71; ParamName: 'Rate M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 202; ParamIndex: 4;
    ParamID: 71; ParamName: 'Step'; DefaultValue: 127; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 202; ParamIndex: 5;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 6;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 202; ParamIndex: 6;
    ParamID: 3; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 6; slParamLabel: ''), (ModuleID: 202; ParamIndex: 7;
    ParamID: 77; ParamName: 'Range'; DefaultValue: 1; DefaultKnob: - 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 202; ParamIndex: 8;
    ParamID: 152; ParamName: 'Edge'; DefaultValue: 4; DefaultKnob: 7;
    ButtonParam: 8; slParamLabel: ''), (ModuleID: 204; ParamIndex: 0;
    ParamID: 71; ParamName: 'Step'; DefaultValue: 127; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 204; ParamIndex: 1;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 204; ParamIndex: 2;
    ParamID: 106; ParamName: 'Dice'; DefaultValue: 0; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: 'Dice'), (ModuleID: 204; ParamIndex: 3;
    ParamID: 3; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 204; ParamIndex: 4;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 4;
    ButtonParam: 4; slParamLabel: ''), (ModuleID: 205; ParamIndex: 0;
    ParamID: 71; ParamName: 'Step'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 205; ParamIndex: 1;
    ParamID: 71; ParamName: 'Step M'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 205; ParamIndex: 2;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 205; ParamIndex: 3;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 206; ParamIndex: 0;
    ParamID: 71; ParamName: 'Step'; DefaultValue: 127; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 206; ParamIndex: 1;
    ParamID: 3; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 1;
    ButtonParam: 1; slParamLabel: ''), (ModuleID: 206; ParamIndex: 2;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 2;
    ButtonParam: 2; slParamLabel: ''), (ModuleID: 206; ParamIndex: 3;
    ParamID: 134; ParamName: 'Mode'; DefaultValue: 0; DefaultKnob: 3;
    ButtonParam: 3; slParamLabel: ''), (ModuleID: 206; ParamIndex: 4;
    ParamID: 71; ParamName: 'Step M'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 0;
    ParamID: 145; ParamName: 'PatternA'; DefaultValue: 64; DefaultKnob: 0;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 1;
    ParamID: 71; ParamName: 'PatternB'; DefaultValue: 64; DefaultKnob: 1;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 2;
    ParamID: 145; ParamName: 'Step'; DefaultValue: 127; DefaultKnob: 3;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 3;
    ParamID: 143; ParamName: 'LoopCount'; DefaultValue: 15; DefaultKnob: 2;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 4;
    ParamID: 71; ParamName: 'Step M'; DefaultValue: 0; DefaultKnob: 4;
    ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208; ParamIndex: 5;
    ParamID: 3; ParamName: 'OutType'; DefaultValue: 0; DefaultKnob: 5;
    ButtonParam: 5; slParamLabel: ''), (ModuleID: 208; ParamIndex: 6;
    ParamID: 106; ParamName: 'On/OFF'; DefaultValue: 1; DefaultKnob: 6;
    ButtonParam: 6; slParamLabel: ''));

  ModuleModes: array [0 .. 24] of TG2ModuleParamDef = ((ModuleID: 8;
    ParamIndex: 0; ParamID: 123; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 9;
    ParamIndex: 0; ParamID: 122; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 12;
    ParamIndex: 0; ParamID: 154; ParamName: 'RoomType'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 24;
    ParamIndex: 0; ParamID: 83; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 35;
    ParamIndex: 0; ParamID: 179; ParamName: 'Driver T'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 38;
    ParamIndex: 0; ParamID: 140; ParamName: 'PulseMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 42;
    ParamIndex: 0; ParamID: 85; ParamName: 'DelayMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 64;
    ParamIndex: 0; ParamID: 62; ParamName: 'GateMode1'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 64;
    ParamIndex: 1; ParamID: 62; ParamName: 'GateMode2'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 69;
    ParamIndex: 0; ParamID: 7; ParamName: 'DivMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 87;
    ParamIndex: 0; ParamID: 65; ParamName: 'SlopeMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 91;
    ParamIndex: 0; ParamID: 46; ParamName: 'OperationMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 96;
    ParamIndex: 0; ParamID: 122; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 134;
    ParamIndex: 0; ParamID: 65; ParamName: 'SlopeMode'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 172;
    ParamIndex: 0; ParamID: 17; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 173;
    ParamIndex: 0; ParamID: 17; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 174;
    ParamIndex: 0; ParamID: 17; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 175;
    ParamIndex: 0; ParamID: 17; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 176;
    ParamIndex: 0; ParamID: 16; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 177;
    ParamIndex: 0; ParamID: 16; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 181;
    ParamIndex: 0; ParamID: 17; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 182;
    ParamIndex: 0; ParamID: 15; ParamName: 'DelayRange'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 183;
    ParamIndex: 0; ParamID: 121; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 206;
    ParamIndex: 0; ParamID: 151; ParamName: 'Character'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''), (ModuleID: 208;
    ParamIndex: 0; ParamID: 153; ParamName: 'Wave'; DefaultValue: 0;
    DefaultKnob: - 1; ButtonParam: - 1; slParamLabel: ''));

  ModuleInputs: array [0 .. 446] of TG2ConnectorDef = ((ModuleID: 3;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 1; XPos: 162; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 3; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 0; XPos: 188;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 3; ConnectorIndex: 2; ConnectorName: 'In3';
    ID: 3; XPos: 214; YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 3; ConnectorIndex: 3; ConnectorName: 'In4';
    ID: 2; XPos: 240; YPos: 13; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 4; ConnectorIndex: 0; ConnectorName: 'InL';
    ID: 2; XPos: 214; YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 4; ConnectorIndex: 1; ConnectorName: 'InR';
    ID: 1; XPos: 240; YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 5; ConnectorIndex: 0; ConnectorName: 'In1';
    ID: 6; XPos: 100; YPos: 9; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 5; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 9; XPos: 193; YPos: 9; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 7;
    ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 1; XPos: 4; YPos: 43;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 7; ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 2; XPos: 4;
    YPos: 59; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 7; ConnectorIndex: 2;
    ConnectorName: 'Sync'; ID: 0; XPos: 4; YPos: 17; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 7;
    ConnectorIndex: 3; ConnectorName: 'FmMod'; ID: 7; XPos: 115; YPos: 59;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 7; ConnectorIndex: 4; ConnectorName: 'Shape M'; ID: 13;
    XPos: 180; YPos: 59; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 8; ConnectorIndex: 0;
    ConnectorName: 'Pitch'; ID: 0; XPos: 4; YPos: 28; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 8;
    ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 3; XPos: 4; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 8; ConnectorIndex: 2; ConnectorName: 'Sync'; ID: 1; XPos: 4;
    YPos: 14; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 8; ConnectorIndex: 3;
    ConnectorName: 'FmMod'; ID: 12; XPos: 137; YPos: 44; CodeRef: 3;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 8;
    ConnectorIndex: 4; ConnectorName: 'Shape M'; ID: 15; XPos: 187; YPos: 44;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 9; ConnectorIndex: 0; ConnectorName: 'PitchVar'; ID: 18; XPos: 4;
    YPos: 29; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 9; ConnectorIndex: 1;
    ConnectorName: 'Sync'; ID: 4; XPos: 158; YPos: 29; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 9;
    ConnectorIndex: 2; ConnectorName: 'FmMod'; ID: 6; XPos: 182; YPos: 29;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 9; ConnectorIndex: 3; ConnectorName: 'Pitch'; ID: 0; XPos: 4;
    YPos: 13; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 12; ConnectorIndex: 0;
    ConnectorName: 'InL'; ID: 16; XPos: 220; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 12;
    ConnectorIndex: 1; ConnectorName: 'InR'; ID: 3; XPos: 240; YPos: 4;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 13; ConnectorIndex: 0; ConnectorName: 'In'; ID: 24; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 13; ConnectorIndex: 1;
    ConnectorName: 'Pitch'; ID: 0; XPos: 4; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 13;
    ConnectorIndex: 2; ConnectorName: 'PitchVar'; ID: 1; XPos: 4; YPos: 29;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 15; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 12; XPos: 82;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 15; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 17; XPos: 124; YPos: 4; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 15;
    ConnectorIndex: 2; ConnectorName: 'In3'; ID: 18; XPos: 166; YPos: 4;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 15; ConnectorIndex: 3; ConnectorName: 'In4'; ID: 19; XPos: 208;
    YPos: 4; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 15; ConnectorIndex: 4;
    ConnectorName: 'In5'; ID: 22; XPos: 99; YPos: 15; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 15;
    ConnectorIndex: 5; ConnectorName: 'In6'; ID: 23; XPos: 141; YPos: 15;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 15; ConnectorIndex: 6; ConnectorName: 'In7'; ID: 24; XPos: 183;
    YPos: 15; CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 15; ConnectorIndex: 7;
    ConnectorName: 'In8'; ID: 26; XPos: 225; YPos: 15; CodeRef: 7; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 17;
    ConnectorIndex: 0; ConnectorName: 'Input'; ID: 1; XPos: 180; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 17; ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 9; XPos: 4;
    YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 18; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 1; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 18;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 2; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 18; ConnectorIndex: 2; ConnectorName: 'Mod'; ID: 3; XPos: 87;
    YPos: 14; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 19; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 1; XPos: 76; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 19;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 4; XPos: 114; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 19; ConnectorIndex: 2; ConnectorName: 'In3'; ID: 7; XPos: 152;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 19; ConnectorIndex: 3;
    ConnectorName: 'In4'; ID: 10; XPos: 190; YPos: 13; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 19;
    ConnectorIndex: 4; ConnectorName: 'Chain'; ID: 15; XPos: 4; YPos: 14;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 20; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 20; ConnectorIndex: 1;
    ConnectorName: 'Gate'; ID: 10; XPos: 4; YPos: 30; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 20;
    ConnectorIndex: 2; ConnectorName: 'AM'; ID: 18; XPos: 4; YPos: 45;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 21; ConnectorIndex: 0; ConnectorName: 'In'; ID: 33; XPos: 80;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 21; ConnectorIndex: 1;
    ConnectorName: 'Ctrl'; ID: 44; XPos: 4; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 22;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 2; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 23; ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 18; XPos: 4;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 23; ConnectorIndex: 1;
    ConnectorName: 'AMod'; ID: 26; XPos: 43; YPos: 59; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 23;
    ConnectorIndex: 2; ConnectorName: 'DMod'; ID: 29; XPos: 79; YPos: 59;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 23; ConnectorIndex: 3; ConnectorName: 'SMod'; ID: 32; XPos: 115;
    YPos: 59; CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 23; ConnectorIndex: 4;
    ConnectorName: 'RMod'; ID: 35; XPos: 151; YPos: 59; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 23;
    ConnectorIndex: 5; ConnectorName: 'In'; ID: 13; XPos: 240; YPos: 4;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 23; ConnectorIndex: 6; ConnectorName: 'AM'; ID: 21; XPos: 4;
    YPos: 60; CodeRef: 6; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 24; ConnectorIndex: 0;
    ConnectorName: 'Rate'; ID: 9; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 25;
    ConnectorIndex: 0; ConnectorName: 'Rate'; ID: 4; XPos: 23; YPos: 59;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 25; ConnectorIndex: 1; ConnectorName: 'RateVar'; ID: 5; XPos: 40;
    YPos: 59; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 25; ConnectorIndex: 2;
    ConnectorName: 'Rst'; ID: 3; XPos: 4; YPos: 26; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 25;
    ConnectorIndex: 3; ConnectorName: 'Shape M'; ID: 28; XPos: 125; YPos: 59;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 25; ConnectorIndex: 4; ConnectorName: 'Phase M'; ID: 16;
    XPos: 163; YPos: 59; CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 25; ConnectorIndex: 5;
    ConnectorName: 'Dir'; ID: 13; XPos: 25; YPos: 26; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 26;
    ConnectorIndex: 0; ConnectorName: 'Rate'; ID: 3; XPos: 4; YPos: 14;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 26; ConnectorIndex: 1; ConnectorName: 'RateVar'; ID: 0; XPos: 4;
    YPos: 29; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 27; ConnectorIndex: 0;
    ConnectorName: 'Pitch'; ID: 2; XPos: 4; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 27;
    ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 1; XPos: 4; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 28; ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 28; ConnectorIndex: 1;
    ConnectorName: 'Mod'; ID: 1; XPos: 108; YPos: 14; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 29;
    ConnectorIndex: 0; ConnectorName: 'FreqMod'; ID: 2; XPos: 66; YPos: 14;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 29; ConnectorIndex: 1; ConnectorName: 'ColorMod'; ID: 9;
    XPos: 150; YPos: 14; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 32; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 11; XPos: 240; YPos: 3; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 33;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 11; XPos: 240; YPos: 4;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 34; ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 34; ConnectorIndex: 1;
    ConnectorName: 'ModIn'; ID: 1; XPos: 108; YPos: 14; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 35;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 4; XPos: 215; YPos: 28;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 35; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 8; XPos: 8;
    YPos: 28; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 36; ConnectorIndex: 0; ConnectorName: 'In';
    ID: 1; XPos: 210; YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 38; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 7; XPos: 192; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 38;
    ConnectorIndex: 1; ConnectorName: 'Time'; ID: 1; XPos: 64; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 40; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 5; XPos: 45;
    YPos: 20; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 40; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 6; XPos: 70; YPos: 20; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 40;
    ConnectorIndex: 2; ConnectorName: 'In3'; ID: 11; XPos: 95; YPos: 20;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 40; ConnectorIndex: 3; ConnectorName: 'In4'; ID: 12; XPos: 120;
    YPos: 20; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 40; ConnectorIndex: 4;
    ConnectorName: 'In5'; ID: 17; XPos: 145; YPos: 20; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 40;
    ConnectorIndex: 5; ConnectorName: 'In6'; ID: 18; XPos: 170; YPos: 20;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 40; ConnectorIndex: 6; ConnectorName: 'In7'; ID: 24; XPos: 195;
    YPos: 20; CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 40; ConnectorIndex: 7;
    ConnectorName: 'In8'; ID: 27; XPos: 220; YPos: 20; CodeRef: 7; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 40;
    ConnectorIndex: 8; ConnectorName: 'Chain'; ID: 0; XPos: 240; YPos: 4;
    CodeRef: 8; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 41; ConnectorIndex: 0; ConnectorName: 'Trig'; ID: 1; XPos: 4;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 41; ConnectorIndex: 1; ConnectorName: 'AM';
    ID: 4; XPos: 49; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 41;
    ConnectorIndex: 2; ConnectorName: 'In'; ID: 9; XPos: 210; YPos: 11;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 42; ConnectorIndex: 0; ConnectorName: 'In'; ID: 6; XPos: 192;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 42; ConnectorIndex: 1;
    ConnectorName: 'TimeMod'; ID: 0; XPos: 64; YPos: 14; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 44;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 210; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 44; ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 6; XPos: 180;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 45; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 1; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 45;
    ConnectorIndex: 1; ConnectorName: 'Vowel'; ID: 19; XPos: 124; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 45; ConnectorIndex: 2; ConnectorName: 'FreqMod'; ID: 12; XPos: 4;
    YPos: 44; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 46; ConnectorIndex: 0;
    ConnectorName: 'Trig'; ID: 2; XPos: 4; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 46;
    ConnectorIndex: 1; ConnectorName: 'AM'; ID: 7; XPos: 4; YPos: 45;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 46; ConnectorIndex: 2; ConnectorName: 'In'; ID: 9; XPos: 240;
    YPos: 4; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 47; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 4; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 47;
    ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 0; XPos: 87; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 48; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 3; XPos: 9;
    YPos: 22; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 48; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 8; XPos: 41; YPos: 22; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 48;
    ConnectorIndex: 2; ConnectorName: 'In3'; ID: 13; XPos: 73; YPos: 22;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 48; ConnectorIndex: 3; ConnectorName: 'In4'; ID: 18; XPos: 105;
    YPos: 22; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 48; ConnectorIndex: 4;
    ConnectorName: 'In5'; ID: 23; XPos: 137; YPos: 22; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 48;
    ConnectorIndex: 5; ConnectorName: 'In6'; ID: 28; XPos: 169; YPos: 22;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 49; ConnectorIndex: 0; ConnectorName: 'In'; ID: 15; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 49; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 1; XPos: 4; YPos: 44; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 49;
    ConnectorIndex: 2; ConnectorName: 'Pitch'; ID: 2; XPos: 4; YPos: 28;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 51; ConnectorIndex: 0; ConnectorName: 'In'; ID: 23; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 51; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 0; XPos: 4; YPos: 59; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 51;
    ConnectorIndex: 2; ConnectorName: 'Pitch'; ID: 3; XPos: 4; YPos: 43;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 51; ConnectorIndex: 3; ConnectorName: 'FMLin'; ID: 4; XPos: 45;
    YPos: 59; CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 51; ConnectorIndex: 4;
    ConnectorName: 'Res'; ID: 26; XPos: 124; YPos: 59; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 52;
    ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 6; XPos: 4; YPos: 28;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 52; ConnectorIndex: 1; ConnectorName: 'In'; ID: 1; XPos: 210;
    YPos: 4; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 52; ConnectorIndex: 2;
    ConnectorName: 'AM'; ID: 10; XPos: 42; YPos: 28; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 53;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 192; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 53; ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 5; XPos: 162;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 54; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 23; XPos: 240; YPos: 3; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 55;
    ConnectorIndex: 0; ConnectorName: 'Trig'; ID: 0; XPos: 4; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 55; ConnectorIndex: 1; ConnectorName: 'AM'; ID: 5; XPos: 49;
    YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 55; ConnectorIndex: 2; ConnectorName: 'In';
    ID: 8; XPos: 210; YPos: 11; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 56; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 24; XPos: 239; YPos: 5; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 56;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 0; XPos: 5; YPos: 54;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 56; ConnectorIndex: 2; ConnectorName: 'In3'; ID: 1; XPos: 21;
    YPos: 54; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 57; ConnectorIndex: 0; ConnectorName: 'In';
    ID: 1; XPos: 10; YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 58; ConnectorIndex: 0;
    ConnectorName: 'Trig'; ID: 18; XPos: 4; YPos: 26; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 58;
    ConnectorIndex: 1; ConnectorName: 'Vel'; ID: 19; XPos: 4; YPos: 105;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 58; ConnectorIndex: 2; ConnectorName: 'Pitch'; ID: 20; XPos: 4;
    YPos: 67; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 59; ConnectorIndex: 0; ConnectorName: 'In';
    ID: 2; XPos: 120; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 60;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 32; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 60; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 1; XPos: 52;
    YPos: 30; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 60; ConnectorIndex: 2;
    ConnectorName: 'In3'; ID: 4; XPos: 72; YPos: 30; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 60;
    ConnectorIndex: 3; ConnectorName: 'In4'; ID: 6; XPos: 92; YPos: 30;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 60; ConnectorIndex: 4; ConnectorName: 'In5'; ID: 8; XPos: 112;
    YPos: 30; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 60; ConnectorIndex: 5;
    ConnectorName: 'In6'; ID: 10; XPos: 132; YPos: 30; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 60;
    ConnectorIndex: 6; ConnectorName: 'In7'; ID: 12; XPos: 152; YPos: 30;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 60; ConnectorIndex: 7; ConnectorName: 'In8'; ID: 14; XPos: 172;
    YPos: 30; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 60; ConnectorIndex: 8;
    ConnectorName: 'Ctrl'; ID: 24; XPos: 4; YPos: 30; CodeRef: 8; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 61;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 61; ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 0; XPos: 78;
    YPos: 14; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 62; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 5; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 62;
    ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 0; XPos: 108; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 63; ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 63; ConnectorIndex: 1;
    ConnectorName: 'Mod'; ID: 6; XPos: 11; YPos: 29; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 64;
    ConnectorIndex: 0; ConnectorName: 'In1_1'; ID: 13; XPos: 80; YPos: 4;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 64; ConnectorIndex: 1; ConnectorName: 'In1_2'; ID: 12; XPos: 70;
    YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 64; ConnectorIndex: 2;
    ConnectorName: 'In2_1'; ID: 6; XPos: 173; YPos: 4; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 64;
    ConnectorIndex: 3; ConnectorName: 'In2_2'; ID: 5; XPos: 163; YPos: 15;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 66; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 4; XPos: 70;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 66; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 8; XPos: 155; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 66;
    ConnectorIndex: 2; ConnectorName: 'Chain'; ID: 0; XPos: 4; YPos: 15;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 68; ConnectorIndex: 0; ConnectorName: 'Reset'; ID: 14; XPos: 4;
    YPos: 16; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 69; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 0; XPos: 78; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 69;
    ConnectorIndex: 1; ConnectorName: 'Reset'; ID: 3; XPos: 116; YPos: 11;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 71; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 72; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 3; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 74;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 74; ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 0; XPos: 108;
    YPos: 14; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 75; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 6; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 76;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 210; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 78; ConnectorIndex: 0; ConnectorName: 'In'; ID: 13; XPos: 50;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 79; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 12; XPos: 82; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 79;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 17; XPos: 126; YPos: 11;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 79; ConnectorIndex: 2; ConnectorName: 'In3'; ID: 18; XPos: 169;
    YPos: 11; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 79; ConnectorIndex: 3;
    ConnectorName: 'In4'; ID: 19; XPos: 212; YPos: 11; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 81;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 210; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 82; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 83; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 1; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 84;
    ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 6; XPos: 4; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 84; ConnectorIndex: 1; ConnectorName: 'In'; ID: 0; XPos: 240;
    YPos: 4; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 84; ConnectorIndex: 2;
    ConnectorName: 'AM'; ID: 10; XPos: 52; YPos: 30; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 85;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 8; XPos: 210; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 85; ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 0; XPos: 4;
    YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 86; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 40; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 86;
    ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 44; XPos: 44; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 87; ConnectorIndex: 0; ConnectorName: 'In'; ID: 5; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 87; ConnectorIndex: 1;
    ConnectorName: 'Pitch'; ID: 1; XPos: 63; YPos: 14; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 88;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 6; XPos: 50; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 89; ConnectorIndex: 0; ConnectorName: 'In'; ID: 6; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 90; ConnectorIndex: 0; ConnectorName: 'In';
    ID: 4; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 91; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 3; XPos: 116; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 91;
    ConnectorIndex: 1; ConnectorName: 'Res'; ID: 4; XPos: 143; YPos: 5;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 91; ConnectorIndex: 2; ConnectorName: 'In'; ID: 5; XPos: 80;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 92; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 16; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 92;
    ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 1; XPos: 4; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 92; ConnectorIndex: 2; ConnectorName: 'Pitch'; ID: 2; XPos: 4;
    YPos: 28; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 94; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 3; XPos: 228; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 96;
    ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 9; XPos: 4; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 97; ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 3; XPos: 4;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 97; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 0; XPos: 4; YPos: 29; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 98;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 21; XPos: 228; YPos: 4;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 98; ConnectorIndex: 1; ConnectorName: 'Shift'; ID: 3; XPos: 11;
    YPos: 29; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 100; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 3; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 100;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 1; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 102; ConnectorIndex: 0; ConnectorName: 'In'; ID: 2; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 102; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 13; XPos: 4; YPos: 59; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 102; ConnectorIndex: 2; ConnectorName: 'Spr'; ID: 6; XPos: 81;
    YPos: 59; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 102; ConnectorIndex: 3;
    ConnectorName: 'FB'; ID: 10; XPos: 122; YPos: 59; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 102;
    ConnectorIndex: 4; ConnectorName: 'Pitch'; ID: 22; XPos: 4; YPos: 43;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 103; ConnectorIndex: 0; ConnectorName: 'In'; ID: 11; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 105; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 2; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 105;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 0; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 105; ConnectorIndex: 2; ConnectorName: 'Ctrl'; ID: 8; XPos: 4;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 106; ConnectorIndex: 0;
    ConnectorName: 'Pitch'; ID: 7; XPos: 4; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 106;
    ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 4; XPos: 4; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 106; ConnectorIndex: 2; ConnectorName: 'Width'; ID: 13;
    XPos: 159; YPos: 29; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 108; ConnectorIndex: 0;
    ConnectorName: 'Ctrl'; ID: 1; XPos: 6; YPos: 25; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 108;
    ConnectorIndex: 1; ConnectorName: 'In'; ID: 24; XPos: 210; YPos: 102;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 112; ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 210;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 113; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 1; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 113;
    ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 8; XPos: 87; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 114; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 180;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 114; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 3; XPos: 210; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 114;
    ConnectorIndex: 2; ConnectorName: 'Mod'; ID: 9; XPos: 87; YPos: 14;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 115; ConnectorIndex: 0; ConnectorName: 'Note'; ID: 12; XPos: 4;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 115; ConnectorIndex: 1;
    ConnectorName: 'In'; ID: 8; XPos: 240; YPos: 4; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 116;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 76; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 116; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 1; XPos: 95;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 116; ConnectorIndex: 2;
    ConnectorName: 'In3'; ID: 4; XPos: 114; YPos: 13; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 116;
    ConnectorIndex: 3; ConnectorName: 'In4'; ID: 6; XPos: 133; YPos: 13;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 116; ConnectorIndex: 4; ConnectorName: 'In5'; ID: 8; XPos: 152;
    YPos: 13; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 116; ConnectorIndex: 5;
    ConnectorName: 'In6'; ID: 10; XPos: 171; YPos: 13; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 116;
    ConnectorIndex: 6; ConnectorName: 'In7'; ID: 12; XPos: 190; YPos: 13;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 116; ConnectorIndex: 7; ConnectorName: 'In8'; ID: 14; XPos: 209;
    YPos: 13; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 117; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 0; XPos: 210; YPos: 28; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 117;
    ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 2; XPos: 210; YPos: 7;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 117; ConnectorIndex: 2; ConnectorName: 'ModDepth'; ID: 6;
    XPos: 91; YPos: 29; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 118; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 2; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 118;
    ConnectorIndex: 1; ConnectorName: 'Rate'; ID: 11; XPos: 11; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 119; ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 1; XPos: 4;
    YPos: 36; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 119; ConnectorIndex: 1;
    ConnectorName: 'AM'; ID: 6; XPos: 4; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 119;
    ConnectorIndex: 2; ConnectorName: 'In'; ID: 5; XPos: 210; YPos: 4;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 121; ConnectorIndex: 0; ConnectorName: 'Clk'; ID: 1; XPos: 4;
    YPos: 43; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 121; ConnectorIndex: 1;
    ConnectorName: 'Rst'; ID: 0; XPos: 4; YPos: 66; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 121;
    ConnectorIndex: 2; ConnectorName: 'Loop'; ID: 3; XPos: 4; YPos: 88;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 121; ConnectorIndex: 3; ConnectorName: 'Park'; ID: 5; XPos: 204;
    YPos: 4; CodeRef: 3; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 121; ConnectorIndex: 4;
    ConnectorName: 'Note'; ID: 115; XPos: 4; YPos: 104; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 121;
    ConnectorIndex: 5; ConnectorName: 'Trig'; ID: 82; XPos: 4; YPos: 120;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 121; ConnectorIndex: 6; ConnectorName: 'RecVal'; ID: 22;
    XPos: 98; YPos: 4; CodeRef: 6; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 121; ConnectorIndex: 7;
    ConnectorName: 'RecEnable'; ID: 23; XPos: 111; YPos: 4; CodeRef: 7;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 123;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 48; YPos: 23;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 123; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 4; XPos: 96;
    YPos: 23; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 123; ConnectorIndex: 2;
    ConnectorName: 'In3'; ID: 7; XPos: 144; YPos: 23; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 123;
    ConnectorIndex: 3; ConnectorName: 'In4'; ID: 10; XPos: 192; YPos: 23;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 123; ConnectorIndex: 4; ConnectorName: 'Chain'; ID: 14;
    XPos: 240; YPos: 4; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 124; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 1; XPos: 80; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 124;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 0; XPos: 100; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 124; ConnectorIndex: 2; ConnectorName: 'In3'; ID: 5; XPos: 120;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 124; ConnectorIndex: 3;
    ConnectorName: 'In4'; ID: 7; XPos: 140; YPos: 13; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 124;
    ConnectorIndex: 4; ConnectorName: 'In5'; ID: 9; XPos: 160; YPos: 13;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 124; ConnectorIndex: 5; ConnectorName: 'In6'; ID: 11; XPos: 180;
    YPos: 13; CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 124; ConnectorIndex: 6;
    ConnectorName: 'In7'; ID: 13; XPos: 200; YPos: 13; CodeRef: 6; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 124;
    ConnectorIndex: 7; ConnectorName: 'In8'; ID: 15; XPos: 220; YPos: 13;
    CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 124; ConnectorIndex: 8; ConnectorName: 'Ctrl'; ID: 25; XPos: 4;
    YPos: 15; CodeRef: 8; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 125; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 2; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 125;
    ConnectorIndex: 1; ConnectorName: 'Sweep'; ID: 1; XPos: 97; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 128; ConnectorIndex: 0; ConnectorName: 'A'; ID: 0; XPos: 120;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 128; ConnectorIndex: 1;
    ConnectorName: 'B'; ID: 1; XPos: 150; YPos: 11; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 130;
    ConnectorIndex: 0; ConnectorName: 'Clk'; ID: 41; XPos: 4; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 130; ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 42; XPos: 44;
    YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 131; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 40; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 132;
    ConnectorIndex: 0; ConnectorName: 'D0'; ID: 1; XPos: 80; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 132; ConnectorIndex: 1; ConnectorName: 'D1'; ID: 0; XPos: 100;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 132; ConnectorIndex: 2;
    ConnectorName: 'D2'; ID: 5; XPos: 120; YPos: 13; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 132;
    ConnectorIndex: 3; ConnectorName: 'D3'; ID: 7; XPos: 140; YPos: 13;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 132; ConnectorIndex: 4; ConnectorName: 'D4'; ID: 9; XPos: 160;
    YPos: 13; CodeRef: 4; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 132; ConnectorIndex: 5;
    ConnectorName: 'D5'; ID: 11; XPos: 180; YPos: 13; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 132;
    ConnectorIndex: 6; ConnectorName: 'D6'; ID: 13; XPos: 200; YPos: 13;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 132; ConnectorIndex: 7; ConnectorName: 'D7'; ID: 15; XPos: 220;
    YPos: 13; CodeRef: 7; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 134; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 4; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 134;
    ConnectorIndex: 1; ConnectorName: 'Pitch'; ID: 0; XPos: 63; YPos: 14;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 139; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 192;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 139; ConnectorIndex: 1;
    ConnectorName: 'Ctrl'; ID: 4; XPos: 162; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 140;
    ConnectorIndex: 0; ConnectorName: 'In1L'; ID: 2; XPos: 4; YPos: 24;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 140; ConnectorIndex: 1; ConnectorName: 'In1R'; ID: 1; XPos: 19;
    YPos: 24; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 140; ConnectorIndex: 2;
    ConnectorName: 'In2L'; ID: 8; XPos: 58; YPos: 24; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 140;
    ConnectorIndex: 3; ConnectorName: 'In2R'; ID: 6; XPos: 73; YPos: 24;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 140; ConnectorIndex: 4; ConnectorName: 'In3L'; ID: 19; XPos: 112;
    YPos: 24; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 140; ConnectorIndex: 5;
    ConnectorName: 'In3R'; ID: 16; XPos: 127; YPos: 24; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 140;
    ConnectorIndex: 6; ConnectorName: 'In4L'; ID: 25; XPos: 166; YPos: 24;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 140; ConnectorIndex: 7; ConnectorName: 'In4R'; ID: 23; XPos: 181;
    YPos: 24; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 140; ConnectorIndex: 8;
    ConnectorName: 'ChainL'; ID: 30; XPos: 223; YPos: 4; CodeRef: 8;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 140;
    ConnectorIndex: 9; ConnectorName: 'ChainR'; ID: 15; XPos: 240; YPos: 4;
    CodeRef: 9; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 141; ConnectorIndex: 0; ConnectorName: 'Send'; ID: 1; XPos: 10;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 141; ConnectorIndex: 1;
    ConnectorName: 'Value'; ID: 13; XPos: 140; YPos: 15; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 142;
    ConnectorIndex: 0; ConnectorName: 'Send'; ID: 0; XPos: 10; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 142; ConnectorIndex: 1; ConnectorName: 'Program'; ID: 11;
    XPos: 140; YPos: 15; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 143; ConnectorIndex: 0;
    ConnectorName: 'Gate'; ID: 13; XPos: 10; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 143;
    ConnectorIndex: 1; ConnectorName: 'Vel'; ID: 12; XPos: 65; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 143; ConnectorIndex: 2; ConnectorName: 'Note'; ID: 9; XPos: 137;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 144; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 0; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 144;
    ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 1; XPos: 4; YPos: 37;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 144; ConnectorIndex: 2; ConnectorName: 'Loop'; ID: 2; XPos: 4;
    YPos: 60; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 144; ConnectorIndex: 3;
    ConnectorName: 'Park'; ID: 6; XPos: 204; YPos: 4; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 144;
    ConnectorIndex: 4; ConnectorName: 'Trig1'; ID: 83; XPos: 21; YPos: 44;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 144; ConnectorIndex: 5; ConnectorName: 'Trig2'; ID: 84; XPos: 21;
    YPos: 60; CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 145; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 1; XPos: 4; YPos: 25; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 145;
    ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 0; XPos: 4; YPos: 47;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 145; ConnectorIndex: 2; ConnectorName: 'Loop'; ID: 3; XPos: 4;
    YPos: 69; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 145; ConnectorIndex: 3;
    ConnectorName: 'Park'; ID: 5; XPos: 204; YPos: 4; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 145;
    ConnectorIndex: 4; ConnectorName: 'Val'; ID: 115; XPos: 4; YPos: 84;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 145; ConnectorIndex: 5; ConnectorName: 'Trig'; ID: 82; XPos: 4;
    YPos: 105; CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 146; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 1; XPos: 4; YPos: 24; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 146;
    ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 0; XPos: 4; YPos: 47;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 146; ConnectorIndex: 2; ConnectorName: 'Loop'; ID: 3; XPos: 4;
    YPos: 69; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 146; ConnectorIndex: 3;
    ConnectorName: 'Park'; ID: 5; XPos: 204; YPos: 4; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 146;
    ConnectorIndex: 4; ConnectorName: 'Val'; ID: 115; XPos: 4; YPos: 88;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 146; ConnectorIndex: 5; ConnectorName: 'Trig'; ID: 82; XPos: 4;
    YPos: 105; CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 150; ConnectorIndex: 0;
    ConnectorName: 'InL'; ID: 1; XPos: 225; YPos: 26; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 150;
    ConnectorIndex: 1; ConnectorName: 'InR'; ID: 0; XPos: 240; YPos: 26;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 150; ConnectorIndex: 2; ConnectorName: 'SideChain'; ID: 15;
    XPos: 79; YPos: 4; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 152; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 12; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 154;
    ConnectorIndex: 0; ConnectorName: 'Ctrl'; ID: 1; XPos: 4; YPos: 24;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 154; ConnectorIndex: 1; ConnectorName: 'Val'; ID: 115; XPos: 4;
    YPos: 87; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 154; ConnectorIndex: 2;
    ConnectorName: 'Trig'; ID: 82; XPos: 4; YPos: 105; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 157;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 158; ConnectorIndex: 0; ConnectorName: 'In'; ID: 7; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 158; ConnectorIndex: 1;
    ConnectorName: 'On'; ID: 3; XPos: 4; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 159;
    ConnectorIndex: 0; ConnectorName: 'A'; ID: 1; XPos: 120; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 159; ConnectorIndex: 1; ConnectorName: 'B'; ID: 0; XPos: 150;
    YPos: 11; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 160; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 0; XPos: 210; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 161;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 2; XPos: 12; YPos: 22;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 161; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 4; XPos: 41;
    YPos: 22; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 161; ConnectorIndex: 2;
    ConnectorName: 'In3'; ID: 6; XPos: 70; YPos: 22; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 161;
    ConnectorIndex: 3; ConnectorName: 'In4'; ID: 8; XPos: 99; YPos: 22;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 161; ConnectorIndex: 4; ConnectorName: 'In5'; ID: 10; XPos: 128;
    YPos: 22; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 161; ConnectorIndex: 5;
    ConnectorName: 'In6'; ID: 12; XPos: 157; YPos: 22; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 161;
    ConnectorIndex: 6; ConnectorName: 'In7'; ID: 14; XPos: 186; YPos: 22;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 161; ConnectorIndex: 7; ConnectorName: 'In8'; ID: 16; XPos: 215;
    YPos: 22; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 161; ConnectorIndex: 8;
    ConnectorName: 'Chain'; ID: 47; XPos: 240; YPos: 4; CodeRef: 8; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 162;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 240; YPos: 4;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 162; ConnectorIndex: 1; ConnectorName: 'Pitch'; ID: 15; XPos: 4;
    YPos: 28; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 162; ConnectorIndex: 2;
    ConnectorName: 'PitchVar'; ID: 7; XPos: 4; YPos: 44; CodeRef: 2;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 162; ConnectorIndex: 3; ConnectorName: 'FB'; ID: 20; XPos: 122;
    YPos: 44; CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 163; ConnectorIndex: 0;
    ConnectorName: 'Pitch'; ID: 1; XPos: 4; YPos: 43; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 163;
    ConnectorIndex: 1; ConnectorName: 'PitchVar'; ID: 2; XPos: 4; YPos: 59;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 163; ConnectorIndex: 2; ConnectorName: 'Sync'; ID: 0; XPos: 4;
    YPos: 17; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 163; ConnectorIndex: 3;
    ConnectorName: 'FM'; ID: 7; XPos: 115; YPos: 59; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 163;
    ConnectorIndex: 4; ConnectorName: 'Shape'; ID: 13; XPos: 180; YPos: 59;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 164; ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 1; XPos: 4;
    YPos: 43; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 164; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 2; XPos: 4; YPos: 59; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 164;
    ConnectorIndex: 2; ConnectorName: 'Sync'; ID: 0; XPos: 4; YPos: 17;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 164; ConnectorIndex: 3; ConnectorName: 'PW'; ID: 7; XPos: 124;
    YPos: 22; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 164; ConnectorIndex: 4;
    ConnectorName: 'Phase'; ID: 15; XPos: 124; YPos: 59; CodeRef: 4;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 165;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 7; YPos: 75;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 165; ConnectorIndex: 1; ConnectorName: 'In2'; ID: 1; XPos: 44;
    YPos: 75; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 165; ConnectorIndex: 2;
    ConnectorName: 'In3'; ID: 2; XPos: 81; YPos: 75; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 165;
    ConnectorIndex: 3; ConnectorName: 'In4'; ID: 3; XPos: 118; YPos: 75;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 165; ConnectorIndex: 4; ConnectorName: 'In5'; ID: 4; XPos: 155;
    YPos: 75; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 165; ConnectorIndex: 5;
    ConnectorName: 'In6'; ID: 10; XPos: 192; YPos: 75; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 167;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 240; YPos: 4;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 167; ConnectorIndex: 1; ConnectorName: 'Pitch'; ID: 6; XPos: 11;
    YPos: 29; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 169; ConnectorIndex: 0;
    ConnectorName: 'Trig'; ID: 2; XPos: 4; YPos: 31; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 169;
    ConnectorIndex: 1; ConnectorName: 'A'; ID: 4; XPos: 66; YPos: 59;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 169; ConnectorIndex: 2; ConnectorName: 'H'; ID: 19; XPos: 110;
    YPos: 59; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 169; ConnectorIndex: 3; ConnectorName: 'D';
    ID: 27; XPos: 153; YPos: 59; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 169;
    ConnectorIndex: 4; ConnectorName: 'In'; ID: 9; XPos: 240; YPos: 4;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 169; ConnectorIndex: 5; ConnectorName: 'AM'; ID: 7; XPos: 4;
    YPos: 60; CodeRef: 5; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 172; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 5; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 173;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 8; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 173; ConnectorIndex: 1; ConnectorName: 'Time'; ID: 4; XPos: 101;
    YPos: 14; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 174; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 9; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 174;
    ConnectorIndex: 1; ConnectorName: 'Time1'; ID: 5; XPos: 93; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 174; ConnectorIndex: 2; ConnectorName: 'Time2'; ID: 18;
    XPos: 168; YPos: 29; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 175; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 3; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 175;
    ConnectorIndex: 1; ConnectorName: 'Time1'; ID: 0; XPos: 55; YPos: 59;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 175; ConnectorIndex: 2; ConnectorName: 'Time2'; ID: 11;
    XPos: 101; YPos: 59; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 175; ConnectorIndex: 3;
    ConnectorName: 'Time3'; ID: 17; XPos: 149; YPos: 59; CodeRef: 3;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 175;
    ConnectorIndex: 4; ConnectorName: 'Time4'; ID: 23; XPos: 196; YPos: 59;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 176; ConnectorIndex: 0; ConnectorName: 'In'; ID: 6; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 177; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 10; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 177;
    ConnectorIndex: 1; ConnectorName: 'FBMod'; ID: 15; XPos: 120; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 177; ConnectorIndex: 2; ConnectorName: 'DryWetMod'; ID: 20;
    XPos: 197; YPos: 44; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 178; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 6; XPos: 210; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 178;
    ConnectorIndex: 1; ConnectorName: 'Clk'; ID: 0; XPos: 4; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 179; ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 80;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 179; ConnectorIndex: 1;
    ConnectorName: 'Clk'; ID: 0; XPos: 4; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 180;
    ConnectorIndex: 0; ConnectorName: 'Freq'; ID: 27; XPos: 4; YPos: 49;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 180; ConnectorIndex: 1; ConnectorName: 'FM'; ID: 40; XPos: 240;
    YPos: 19; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 180; ConnectorIndex: 2;
    ConnectorName: 'Gate'; ID: 51; XPos: 4; YPos: 75; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 180;
    ConnectorIndex: 3; ConnectorName: 'Note'; ID: 77; XPos: 4; YPos: 100;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 180; ConnectorIndex: 4; ConnectorName: 'AMod'; ID: 74; XPos: 4;
    YPos: 142; CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 180; ConnectorIndex: 5;
    ConnectorName: 'Vel'; ID: 75; XPos: 4; YPos: 165; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 180;
    ConnectorIndex: 6; ConnectorName: 'Pitch'; ID: 26; XPos: 4; YPos: 23;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 181; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 182; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 20; XPos: 240; YPos: 4; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 183;
    ConnectorIndex: 0; ConnectorName: 'PitchVar'; ID: 18; XPos: 4; YPos: 29;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 183; ConnectorIndex: 1; ConnectorName: 'Sync'; ID: 4; XPos: 158;
    YPos: 29; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 183; ConnectorIndex: 2;
    ConnectorName: 'Phase M'; ID: 6; XPos: 182; YPos: 29; CodeRef: 2;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 183;
    ConnectorIndex: 3; ConnectorName: 'Pitch'; ID: 0; XPos: 4; YPos: 13;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 184; ConnectorIndex: 0; ConnectorName: 'In'; ID: 1; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 184; ConnectorIndex: 1;
    ConnectorName: 'Chain'; ID: 30; XPos: 4; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 185;
    ConnectorIndex: 0; ConnectorName: 'InL'; ID: 2; XPos: 182; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 185; ConnectorIndex: 1; ConnectorName: 'InR'; ID: 1; XPos: 202;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 185; ConnectorIndex: 2;
    ConnectorName: 'LChain'; ID: 30; XPos: 4; YPos: 15; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 185;
    ConnectorIndex: 3; ConnectorName: 'RChain'; ID: 15; XPos: 24; YPos: 15;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 186; ConnectorIndex: 0; ConnectorName: 'In'; ID: 4; XPos: 180;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 187; ConnectorIndex: 0;
    ConnectorName: 'InOff'; ID: 3; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 187;
    ConnectorIndex: 1; ConnectorName: 'InOn'; ID: 1; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 189; ConnectorIndex: 0; ConnectorName: 'In'; ID: 0; XPos: 240;
    YPos: 4; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 190; ConnectorIndex: 0;
    ConnectorName: 'Rate'; ID: 2; XPos: 28; YPos: 44; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 190;
    ConnectorIndex: 1; ConnectorName: 'RateVar'; ID: 1; XPos: 45; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 190; ConnectorIndex: 2; ConnectorName: 'Rst'; ID: 17; XPos: 4;
    YPos: 16; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 190; ConnectorIndex: 3;
    ConnectorName: 'Phase'; ID: 21; XPos: 140; YPos: 44; CodeRef: 3;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 192;
    ConnectorIndex: 0; ConnectorName: 'In'; ID: 5; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 193; ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 76;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 193; ConnectorIndex: 1;
    ConnectorName: 'In2'; ID: 1; XPos: 114; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 193;
    ConnectorIndex: 2; ConnectorName: 'In3'; ID: 4; XPos: 152; YPos: 13;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 193; ConnectorIndex: 3; ConnectorName: 'In4'; ID: 6; XPos: 190;
    YPos: 13; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 194; ConnectorIndex: 0;
    ConnectorName: 'In1'; ID: 4; XPos: 120; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 194;
    ConnectorIndex: 1; ConnectorName: 'In2'; ID: 8; XPos: 202; YPos: 11;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 194; ConnectorIndex: 2; ConnectorName: 'InChain'; ID: 0; XPos: 4;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 195; ConnectorIndex: 0;
    ConnectorName: 'In'; ID: 2; XPos: 210; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 195;
    ConnectorIndex: 1; ConnectorName: 'Mod'; ID: 5; XPos: 190; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 196; ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 0; XPos: 4;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 196; ConnectorIndex: 1;
    ConnectorName: 'PitchVar'; ID: 1; XPos: 4; YPos: 29; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 196;
    ConnectorIndex: 2; ConnectorName: 'Trig'; ID: 5; XPos: 44; YPos: 29;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 198; ConnectorIndex: 0; ConnectorName: 'In'; ID: 2; XPos: 4;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 200; ConnectorIndex: 0;
    ConnectorName: 'Rate'; ID: 9; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 201;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 209; YPos: 9;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 202; ConnectorIndex: 0; ConnectorName: 'Rate'; ID: 3; XPos: 4;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 202; ConnectorIndex: 1;
    ConnectorName: 'RateVar'; ID: 0; XPos: 4; YPos: 29; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 203;
    ConnectorIndex: 0; ConnectorName: 'In1'; ID: 0; XPos: 210; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 204; ConnectorIndex: 0; ConnectorName: 'Clk'; ID: 1; XPos: 4;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 204; ConnectorIndex: 1;
    ConnectorName: 'Rst'; ID: 10; XPos: 68; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 204;
    ConnectorIndex: 2; ConnectorName: 'Seed'; ID: 14; XPos: 103; YPos: 14;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 205; ConnectorIndex: 0; ConnectorName: 'Clk'; ID: 0; XPos: 4;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 205; ConnectorIndex: 1;
    ConnectorName: 'Rst'; ID: 6; XPos: 68; YPos: 15; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 205;
    ConnectorIndex: 2; ConnectorName: 'Seed'; ID: 4; XPos: 103; YPos: 14;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 205; ConnectorIndex: 3; ConnectorName: 'Prob'; ID: 13; XPos: 118;
    YPos: 14; CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 206; ConnectorIndex: 0;
    ConnectorName: 'Clk'; ID: 0; XPos: 4; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 206;
    ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 6; XPos: 4; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 206; ConnectorIndex: 2; ConnectorName: 'Seed'; ID: 13; XPos: 52;
    YPos: 29; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 206; ConnectorIndex: 3;
    ConnectorName: 'Step'; ID: 23; XPos: 150; YPos: 29; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 208;
    ConnectorIndex: 0; ConnectorName: 'Clk'; ID: 1; XPos: 4; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 208; ConnectorIndex: 1; ConnectorName: 'Rst'; ID: 0; XPos: 4;
    YPos: 30; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 208; ConnectorIndex: 2;
    ConnectorName: 'A'; ID: 16; XPos: 51; YPos: 15; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 208;
    ConnectorIndex: 3; ConnectorName: 'B'; ID: 9; XPos: 51; YPos: 30;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 208; ConnectorIndex: 4; ConnectorName: 'Step'; ID: 27; XPos: 117;
    YPos: 29; CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic));

  ModuleOutputs: array [0 .. 311] of TG2ConnectorDef = ((ModuleID: 1;
    ConnectorIndex: 0; ConnectorName: 'Pitch'; ID: 0; XPos: 28; YPos: 14;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 1; ConnectorIndex: 1; ConnectorName: 'Gate'; ID: 1; XPos: 118;
    YPos: 14; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 1; ConnectorIndex: 2; ConnectorName: 'Lin';
    ID: 2; XPos: 156; YPos: 14; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 1;
    ConnectorIndex: 3; ConnectorName: 'Release'; ID: 3; XPos: 240; YPos: 14;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 1; ConnectorIndex: 4; ConnectorName: 'Note'; ID: 7; XPos: 74;
    YPos: 14; CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 1; ConnectorIndex: 5; ConnectorName: 'Exp';
    ID: 10; XPos: 191; YPos: 14; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 5;
    ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 1; XPos: 147; YPos: 9;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 5; ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 4; XPos: 240;
    YPos: 9; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 7; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 17; XPos: 240; YPos: 60; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 8;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 18; XPos: 240; YPos: 45;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 9; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 12; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 12; ConnectorIndex: 0;
    ConnectorName: 'OutL'; ID: 15; XPos: 220; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 12;
    ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 19; XPos: 240; YPos: 30;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 13; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 23; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 15; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 20; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 15;
    ConnectorIndex: 1; ConnectorName: 'Control'; ID: 11; XPos: 4; YPos: 45;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 17; ConnectorIndex: 0; ConnectorName: 'OutOn'; ID: 0; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 17; ConnectorIndex: 1;
    ConnectorName: 'OutOff'; ID: 5; XPos: 210; YPos: 13; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 18;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 19; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 13; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 20; ConnectorIndex: 0;
    ConnectorName: 'Env'; ID: 4; XPos: 223; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 20;
    ConnectorIndex: 1; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 45;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 21; ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 0; XPos: 100;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 21; ConnectorIndex: 1;
    ConnectorName: 'Out2'; ID: 1; XPos: 120; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 21;
    ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 4; XPos: 140; YPos: 13;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 21; ConnectorIndex: 3; ConnectorName: 'Out4'; ID: 10; XPos: 160;
    YPos: 13; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 21; ConnectorIndex: 4;
    ConnectorName: 'Out5'; ID: 14; XPos: 180; YPos: 13; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 21;
    ConnectorIndex: 5; ConnectorName: 'Out6'; ID: 26; XPos: 200; YPos: 13;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 21; ConnectorIndex: 6; ConnectorName: 'Out7'; ID: 29; XPos: 220;
    YPos: 13; CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 21; ConnectorIndex: 7;
    ConnectorName: 'Out8'; ID: 31; XPos: 240; YPos: 13; CodeRef: 7; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 22;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 23; ConnectorIndex: 0; ConnectorName: 'Env'; ID: 17; XPos: 223;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 23; ConnectorIndex: 1;
    ConnectorName: 'Out'; ID: 14; XPos: 240; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 24;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 7; XPos: 239; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 25; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 25; ConnectorIndex: 1;
    ConnectorName: 'Snc'; ID: 32; XPos: 4; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 26;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 8; XPos: 240; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 27; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 28; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 5; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 29;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 4; XPos: 240; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 30; ConnectorIndex: 0; ConnectorName: 'Wheel'; ID: 6; XPos: 12;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 30; ConnectorIndex: 1;
    ConnectorName: 'AftTouch'; ID: 9; XPos: 52; YPos: 30; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 30;
    ConnectorIndex: 2; ConnectorName: 'ControlPedal'; ID: 12; XPos: 92;
    YPos: 30; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 30; ConnectorIndex: 3;
    ConnectorName: 'SustainPedal'; ID: 3; XPos: 132; YPos: 30; CodeRef: 3;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 30;
    ConnectorIndex: 4; ConnectorName: 'PitchStick'; ID: 0; XPos: 172; YPos: 30;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 30; ConnectorIndex: 5; ConnectorName: 'GlobalWheel1'; ID: 11;
    XPos: 210; YPos: 30; CodeRef: 5; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 30; ConnectorIndex: 6;
    ConnectorName: 'GlobalWheel2'; ID: 14; XPos: 240; YPos: 30; CodeRef: 6;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 31;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 32; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 10; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 33; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 10; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 34;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 5; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 35; ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 5; XPos: 239;
    YPos: 28; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 36; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 36;
    ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 2; XPos: 4; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 38; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 8; XPos: 240;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 40; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 23; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 41;
    ConnectorIndex: 0; ConnectorName: 'Env'; ID: 13; XPos: 194; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 41; ConnectorIndex: 1; ConnectorName: 'Out'; ID: 10; XPos: 240;
    YPos: 11; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 42; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 9; XPos: 240; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 43;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 10;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 44; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 45; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 46;
    ConnectorIndex: 0; ConnectorName: 'Env'; ID: 13; XPos: 223; YPos: 45;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 46; ConnectorIndex: 1; ConnectorName: 'Out'; ID: 10; XPos: 240;
    YPos: 45; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 47; ConnectorIndex: 0;
    ConnectorName: 'OutL'; ID: 5; XPos: 210; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 47;
    ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 7; XPos: 240; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 48; ConnectorIndex: 0; ConnectorName: 'OutL'; ID: 36; XPos: 216;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 48; ConnectorIndex: 1;
    ConnectorName: 'OutR'; ID: 35; XPos: 240; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 49;
    ConnectorIndex: 0; ConnectorName: 'LP'; ID: 16; XPos: 240; YPos: 45;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 49; ConnectorIndex: 1; ConnectorName: 'BP'; ID: 17; XPos: 240;
    YPos: 31; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 49; ConnectorIndex: 2; ConnectorName: 'HP';
    ID: 18; XPos: 240; YPos: 17; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 50;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 10;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 51; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 22; XPos: 240;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 52; ConnectorIndex: 0;
    ConnectorName: 'Env'; ID: 5; XPos: 240; YPos: 21; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 52;
    ConnectorIndex: 1; ConnectorName: 'Out'; ID: 2; XPos: 240; YPos: 4;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 53; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 54; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 22; XPos: 240; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 55;
    ConnectorIndex: 0; ConnectorName: 'Env'; ID: 12; XPos: 194; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 55; ConnectorIndex: 1; ConnectorName: 'Out'; ID: 11; XPos: 240;
    YPos: 11; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 56; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 23; XPos: 215; YPos: 5; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 56;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 22; XPos: 239; YPos: 57;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 57; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 11; XPos: 65;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 58; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 23; XPos: 240; YPos: 105; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 59;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 60; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 17; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 61; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 9; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 62;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 63; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 2; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 64; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 8; XPos: 147; YPos: 9; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 64;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 1; XPos: 240; YPos: 9;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 66; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 11; XPos: 240;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 68; ConnectorIndex: 0;
    ConnectorName: '1/96'; ID: 6; XPos: 240; YPos: 17; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 68;
    ConnectorIndex: 1; ConnectorName: '1/16'; ID: 7; XPos: 240; YPos: 31;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 68; ConnectorIndex: 2; ConnectorName: 'ClkActive'; ID: 8;
    XPos: 240; YPos: 3; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 68; ConnectorIndex: 3;
    ConnectorName: 'Sync'; ID: 16; XPos: 240; YPos: 45; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 69;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 9; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 71; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 72; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 2; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 74;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 7; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 75; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 7; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 76; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 76;
    ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 3; XPos: 4; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 78; ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 16; XPos: 88;
    YPos: 33; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 78; ConnectorIndex: 1;
    ConnectorName: 'Out2'; ID: 23; XPos: 131; YPos: 33; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 78;
    ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 24; XPos: 174; YPos: 33;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 78; ConnectorIndex: 3; ConnectorName: 'Out4'; ID: 25; XPos: 217;
    YPos: 33; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 78; ConnectorIndex: 4;
    ConnectorName: 'Out5'; ID: 18; XPos: 111; YPos: 45; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 78;
    ConnectorIndex: 5; ConnectorName: 'Out6'; ID: 19; XPos: 154; YPos: 45;
    CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 78; ConnectorIndex: 6; ConnectorName: 'Out7'; ID: 20; XPos: 197;
    YPos: 45; CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 78; ConnectorIndex: 7;
    ConnectorName: 'Out8'; ID: 21; XPos: 240; YPos: 45; CodeRef: 7; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 78;
    ConnectorIndex: 8; ConnectorName: 'Ctrl'; ID: 10; XPos: 4; YPos: 45;
    CodeRef: 8; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 79; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 20; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 79; ConnectorIndex: 1;
    ConnectorName: 'Ctrl'; ID: 11; XPos: 4; YPos: 30; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 81;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 82; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 83; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 84;
    ConnectorIndex: 0; ConnectorName: 'Env'; ID: 4; XPos: 223; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 84; ConnectorIndex: 1; ConnectorName: 'Out'; ID: 1; XPos: 240;
    YPos: 30; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 84; ConnectorIndex: 2;
    ConnectorName: 'End'; ID: 13; XPos: 205; YPos: 30; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 85;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 9; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 85; ConnectorIndex: 1; ConnectorName: 'Gate'; ID: 12; XPos: 181;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 86; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 16; XPos: 79; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 86;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 33; XPos: 102; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 86; ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 34; XPos: 125;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 86; ConnectorIndex: 3;
    ConnectorName: 'Out4'; ID: 35; XPos: 148; YPos: 13; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 86;
    ConnectorIndex: 4; ConnectorName: 'Out5'; ID: 36; XPos: 171; YPos: 13;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 86; ConnectorIndex: 5; ConnectorName: 'Out6'; ID: 37; XPos: 194;
    YPos: 13; CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 86; ConnectorIndex: 6;
    ConnectorName: 'Out7'; ID: 38; XPos: 217; YPos: 13; CodeRef: 6; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 86;
    ConnectorIndex: 7; ConnectorName: 'Out8'; ID: 39; XPos: 240; YPos: 13;
    CodeRef: 7; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 87; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 10; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 88; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 8; XPos: 88; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 88;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 11; XPos: 131; YPos: 30;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 88; ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 12; XPos: 174;
    YPos: 30; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 88; ConnectorIndex: 3;
    ConnectorName: 'Out4'; ID: 1; XPos: 217; YPos: 30; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 88;
    ConnectorIndex: 4; ConnectorName: 'Ctrl'; ID: 0; XPos: 4; YPos: 30;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 89; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 5; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 90; ConnectorIndex: 0;
    ConnectorName: 'Out2'; ID: 1; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 90;
    ConnectorIndex: 1; ConnectorName: 'Out1'; ID: 0; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 90; ConnectorIndex: 2; ConnectorName: 'Ctrl'; ID: 3; XPos: 4;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 91; ConnectorIndex: 0;
    ConnectorName: 'NotQ'; ID: 7; XPos: 214; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 91;
    ConnectorIndex: 1; ConnectorName: 'Q'; ID: 10; XPos: 240; YPos: 5;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 92; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 15; XPos: 240;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 94; ConnectorIndex: 0;
    ConnectorName: 'OutL'; ID: 2; XPos: 216; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 94;
    ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 8; XPos: 240; YPos: 30;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 96; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 7; XPos: 240;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 97; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 8; XPos: 240; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 98;
    ConnectorIndex: 0; ConnectorName: 'Dn'; ID: 1; XPos: 216; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 98; ConnectorIndex: 1; ConnectorName: 'Up'; ID: 0; XPos: 240;
    YPos: 30; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 100; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 100;
    ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 2; XPos: 4; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 102; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 103; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 10; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 105;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 106; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 2; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 108; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 25; XPos: 240; YPos: 102; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 112;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 113; ConnectorIndex: 0; ConnectorName: 'OutL'; ID: 6; XPos: 210;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 113; ConnectorIndex: 1;
    ConnectorName: 'OutR'; ID: 2; XPos: 240; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 114;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 2; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 115; ConnectorIndex: 0; ConnectorName: 'Level'; ID: 7; XPos: 210;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 115; ConnectorIndex: 1;
    ConnectorName: 'Out'; ID: 9; XPos: 240; YPos: 30; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 116;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 16; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 117; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240;
    YPos: 28; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 118; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 119;
    ConnectorIndex: 0; ConnectorName: 'Env'; ID: 10; XPos: 240; YPos: 21;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 119; ConnectorIndex: 1; ConnectorName: 'Out'; ID: 8; XPos: 240;
    YPos: 4; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 121; ConnectorIndex: 0;
    ConnectorName: 'Link'; ID: 8; XPos: 240; YPos: 26; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 121;
    ConnectorIndex: 1; ConnectorName: 'Note'; ID: 114; XPos: 240; YPos: 104;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 121; ConnectorIndex: 2; ConnectorName: 'Trig'; ID: 58; XPos: 240;
    YPos: 120; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 123; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 12; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 124;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 16; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 125; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 127; ConnectorIndex: 0;
    ConnectorName: 'OutL'; ID: 3; XPos: 214; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 127;
    ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 0; XPos: 240; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 128; ConnectorIndex: 0; ConnectorName: 'Min'; ID: 2; XPos: 210;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 128; ConnectorIndex: 1;
    ConnectorName: 'Max'; ID: 3; XPos: 240; YPos: 11; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 130;
    ConnectorIndex: 0; ConnectorName: 'Out001'; ID: 17; XPos: 79; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 130; ConnectorIndex: 1; ConnectorName: 'Out002'; ID: 32;
    XPos: 102; YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 130; ConnectorIndex: 2;
    ConnectorName: 'Out004'; ID: 35; XPos: 125; YPos: 13; CodeRef: 2;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 130;
    ConnectorIndex: 3; ConnectorName: 'Out008'; ID: 34; XPos: 148; YPos: 13;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 130; ConnectorIndex: 4; ConnectorName: 'Out016'; ID: 37;
    XPos: 171; YPos: 13; CodeRef: 4; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 130; ConnectorIndex: 5;
    ConnectorName: 'Out032'; ID: 36; XPos: 194; YPos: 13; CodeRef: 5;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 130;
    ConnectorIndex: 6; ConnectorName: 'Out064'; ID: 39; XPos: 217; YPos: 13;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 130; ConnectorIndex: 7; ConnectorName: 'Out128'; ID: 38;
    XPos: 240; YPos: 13; CodeRef: 7; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 131; ConnectorIndex: 0;
    ConnectorName: 'D0'; ID: 17; XPos: 79; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 131;
    ConnectorIndex: 1; ConnectorName: 'D1'; ID: 32; XPos: 102; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 131; ConnectorIndex: 2; ConnectorName: 'D2'; ID: 35; XPos: 125;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 131; ConnectorIndex: 3;
    ConnectorName: 'D3'; ID: 34; XPos: 148; YPos: 13; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 131;
    ConnectorIndex: 4; ConnectorName: 'D4'; ID: 37; XPos: 171; YPos: 13;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 131; ConnectorIndex: 5; ConnectorName: 'D5'; ID: 36; XPos: 194;
    YPos: 13; CodeRef: 5; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 131; ConnectorIndex: 6;
    ConnectorName: 'D6'; ID: 39; XPos: 217; YPos: 13; CodeRef: 6; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 131;
    ConnectorIndex: 7; ConnectorName: 'D7'; ID: 38; XPos: 240; YPos: 13;
    CodeRef: 7; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 132; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 16; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 134; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 11; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 139;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 140; ConnectorIndex: 0; ConnectorName: 'OutL'; ID: 28; XPos: 223;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 140; ConnectorIndex: 1;
    ConnectorName: 'OutR'; ID: 13; XPos: 240; YPos: 45; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 141;
    ConnectorIndex: 0; ConnectorName: 'Send'; ID: 11; XPos: 65; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 142; ConnectorIndex: 0; ConnectorName: 'Send'; ID: 10; XPos: 65;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 144; ConnectorIndex: 0;
    ConnectorName: 'Link'; ID: 4; XPos: 240; YPos: 27; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 144;
    ConnectorIndex: 1; ConnectorName: 'Trig1'; ID: 59; XPos: 240; YPos: 44;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic),
    (ModuleID: 144; ConnectorIndex: 2; ConnectorName: 'Trig2'; ID: 60;
    XPos: 240; YPos: 60; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 145; ConnectorIndex: 0;
    ConnectorName: 'Link'; ID: 8; XPos: 240; YPos: 26; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 145;
    ConnectorIndex: 1; ConnectorName: 'Val'; ID: 114; XPos: 240; YPos: 84;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 145; ConnectorIndex: 2; ConnectorName: 'Trig'; ID: 58; XPos: 240;
    YPos: 105; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 146; ConnectorIndex: 0;
    ConnectorName: 'Link'; ID: 8; XPos: 240; YPos: 26; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 146;
    ConnectorIndex: 1; ConnectorName: 'Val'; ID: 114; XPos: 240; YPos: 87;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 146; ConnectorIndex: 2; ConnectorName: 'Trig'; ID: 58; XPos: 240;
    YPos: 105; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 147; ConnectorIndex: 0;
    ConnectorName: 'Rcv'; ID: 8; XPos: 210; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 147;
    ConnectorIndex: 1; ConnectorName: 'Val'; ID: 2; XPos: 240; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 148; ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 5; XPos: 188;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btStatic), (ModuleID: 148; ConnectorIndex: 1;
    ConnectorName: 'Vel'; ID: 3; XPos: 210; YPos: 13; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 148;
    ConnectorIndex: 2; ConnectorName: 'RelVel'; ID: 11; XPos: 232; YPos: 13;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 150; ConnectorIndex: 0; ConnectorName: 'OutR'; ID: 3; XPos: 240;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 150; ConnectorIndex: 1;
    ConnectorName: 'OutL'; ID: 2; XPos: 225; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 152;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 13; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 154; ConnectorIndex: 0; ConnectorName: 'Val'; ID: 114; XPos: 240;
    YPos: 86; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 154; ConnectorIndex: 1;
    ConnectorName: 'Trig'; ID: 58; XPos: 240; YPos: 105; CodeRef: 1;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 156;
    ConnectorIndex: 0; ConnectorName: 'Gate'; ID: 6; XPos: 172; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 156; ConnectorIndex: 1; ConnectorName: 'Vel'; ID: 4; XPos: 202;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 156; ConnectorIndex: 2;
    ConnectorName: 'RelVel'; ID: 8; XPos: 232; YPos: 13; CodeRef: 2;
    InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 157;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 158; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 8; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btDynamic), (ModuleID: 159; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 2; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 160;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 161; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 25; XPos: 240;
    YPos: 120; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 162; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 45; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 163;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 17; XPos: 240; YPos: 60;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 164; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 17; XPos: 240;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 165; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 5; XPos: 21; YPos: 75; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 165;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 6; XPos: 58; YPos: 75;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 165; ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 7; XPos: 95;
    YPos: 75; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 165; ConnectorIndex: 3;
    ConnectorName: 'Out4'; ID: 8; XPos: 132; YPos: 75; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 165;
    ConnectorIndex: 4; ConnectorName: 'Out5'; ID: 9; XPos: 169; YPos: 75;
    CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 165; ConnectorIndex: 5; ConnectorName: 'Out6'; ID: 11; XPos: 206;
    YPos: 75; CodeRef: 5; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 165; ConnectorIndex: 6;
    ConnectorName: 'Main'; ID: 12; XPos: 240; YPos: 75; CodeRef: 6; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 167;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 2; XPos: 240; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 169; ConnectorIndex: 0; ConnectorName: 'Env'; ID: 13; XPos: 223;
    YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 169; ConnectorIndex: 1;
    ConnectorName: 'Out'; ID: 10; XPos: 240; YPos: 60; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 170;
    ConnectorIndex: 0; ConnectorName: 'OutL'; ID: 1; XPos: 214; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 170; ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 0; XPos: 240;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 171; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 6; XPos: 162; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 171;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 4; XPos: 188; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 171; ConnectorIndex: 2; ConnectorName: 'Out3'; ID: 1; XPos: 214;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 171; ConnectorIndex: 3;
    ConnectorName: 'Out4'; ID: 0; XPos: 240; YPos: 13; CodeRef: 3; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 172;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 173; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 11; XPos: 240;
    YPos: 11; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 174; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 11; XPos: 138; YPos: 29; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 174;
    ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 10; XPos: 213; YPos: 29;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 175; ConnectorIndex: 0; ConnectorName: 'OutMain'; ID: 26;
    XPos: 240; YPos: 60; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 175; ConnectorIndex: 1;
    ConnectorName: 'Out1'; ID: 6; XPos: 74; YPos: 36; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 175;
    ConnectorIndex: 2; ConnectorName: 'Out2'; ID: 10; XPos: 122; YPos: 36;
    CodeRef: 2; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 175; ConnectorIndex: 3; ConnectorName: 'Out3'; ID: 16; XPos: 169;
    YPos: 36; CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 175; ConnectorIndex: 4;
    ConnectorName: 'Out4'; ID: 22; XPos: 216; YPos: 35; CodeRef: 4; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 176;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 7; XPos: 240; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 177; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 11; XPos: 240;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 178; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 7; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 179;
    ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 3; XPos: 100; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 179; ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 4; XPos: 120;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 179; ConnectorIndex: 2;
    ConnectorName: 'Out3'; ID: 5; XPos: 140; YPos: 13; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 179;
    ConnectorIndex: 3; ConnectorName: 'Out4'; ID: 6; XPos: 160; YPos: 13;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 179; ConnectorIndex: 4; ConnectorName: 'Out5'; ID: 7; XPos: 180;
    YPos: 13; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 179; ConnectorIndex: 5;
    ConnectorName: 'Out6'; ID: 8; XPos: 200; YPos: 13; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 179;
    ConnectorIndex: 6; ConnectorName: 'Out7'; ID: 9; XPos: 220; YPos: 13;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 179; ConnectorIndex: 7; ConnectorName: 'Out8'; ID: 10; XPos: 240;
    YPos: 13; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 180; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 61; XPos: 240; YPos: 165; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 181;
    ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 1; XPos: 100; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 181; ConnectorIndex: 1; ConnectorName: 'Out2'; ID: 2; XPos: 120;
    YPos: 30; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 181; ConnectorIndex: 2;
    ConnectorName: 'Out3'; ID: 3; XPos: 140; YPos: 30; CodeRef: 2; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 181;
    ConnectorIndex: 3; ConnectorName: 'Out4'; ID: 4; XPos: 160; YPos: 30;
    CodeRef: 3; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 181; ConnectorIndex: 4; ConnectorName: 'Out5'; ID: 5; XPos: 180;
    YPos: 30; CodeRef: 4; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 181; ConnectorIndex: 5;
    ConnectorName: 'Out6'; ID: 6; XPos: 200; YPos: 30; CodeRef: 5; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 181;
    ConnectorIndex: 6; ConnectorName: 'Out7'; ID: 7; XPos: 220; YPos: 30;
    CodeRef: 6; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 181; ConnectorIndex: 7; ConnectorName: 'Out8'; ID: 8; XPos: 240;
    YPos: 30; CodeRef: 7; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 182; ConnectorIndex: 0;
    ConnectorName: 'OutL'; ID: 12; XPos: 220; YPos: 60; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 182;
    ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 13; XPos: 240; YPos: 60;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 183; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 12; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 184; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 13; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 185;
    ConnectorIndex: 0; ConnectorName: 'OutL'; ID: 28; XPos: 220; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 185; ConnectorIndex: 1; ConnectorName: 'OutR'; ID: 13; XPos: 240;
    YPos: 13; CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 186; ConnectorIndex: 0;
    ConnectorName: 'OutOn'; ID: 1; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 186;
    ConnectorIndex: 1; ConnectorName: 'OutOff'; ID: 0; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 186; ConnectorIndex: 2; ConnectorName: 'Ctrl'; ID: 3; XPos: 4;
    YPos: 15; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 187; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 0; XPos: 240; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 187;
    ConnectorIndex: 1; ConnectorName: 'Ctrl'; ID: 2; XPos: 4; YPos: 15;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 188; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 0; XPos: 240;
    YPos: 10; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 189; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 189;
    ConnectorIndex: 1; ConnectorName: 'Env'; ID: 8; XPos: 223; YPos: 30;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 190; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240;
    YPos: 45; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 190; ConnectorIndex: 1;
    ConnectorName: 'Sync'; ID: 18; XPos: 4; YPos: 45; CodeRef: 1; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 192;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 6; XPos: 240; YPos: 11;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btStatic),
    (ModuleID: 193; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 16; XPos: 240;
    YPos: 13; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btDynamic), (ModuleID: 194; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 11; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btDynamic), (ModuleID: 195;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 13;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio; BandWidth: btDynamic),
    (ModuleID: 196; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 16; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctAudio;
    BandWidth: btStatic), (ModuleID: 197; ConnectorIndex: 0;
    ConnectorName: 'PatchActive'; ID: 3; XPos: 107; YPos: 13; CodeRef: 0;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic), (ModuleID: 197;
    ConnectorIndex: 1; ConnectorName: 'VarActive'; ID: 0; XPos: 166; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 197; ConnectorIndex: 2; ConnectorName: 'VoiceNo'; ID: 5;
    XPos: 225; YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 198; ConnectorIndex: 0;
    ConnectorName: 'Period'; ID: 6; XPos: 173; YPos: 13; CodeRef: 0;
    InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btDynamic), (ModuleID: 198;
    ConnectorIndex: 1; ConnectorName: 'Gate'; ID: 3; XPos: 240; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 198; ConnectorIndex: 2; ConnectorName: 'Pitch'; ID: 8; XPos: 199;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 199; ConnectorIndex: 0;
    ConnectorName: 'Pitch'; ID: 2; XPos: 180; YPos: 13; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 199;
    ConnectorIndex: 1; ConnectorName: 'Gate'; ID: 1; XPos: 210; YPos: 13;
    CodeRef: 1; InfoFunc: 0; ConnectorType: ctLogic; BandWidth: btStatic),
    (ModuleID: 199; ConnectorIndex: 2; ConnectorName: 'Vel'; ID: 3; XPos: 240;
    YPos: 13; CodeRef: 2; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 200; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 7; XPos: 239; YPos: 15; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btStatic), (ModuleID: 201;
    ConnectorIndex: 0; ConnectorName: 'Out1'; ID: 1; XPos: 239; YPos: 9;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btStatic),
    (ModuleID: 202; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 8; XPos: 240;
    YPos: 30; CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl;
    BandWidth: btStatic), (ModuleID: 203; ConnectorIndex: 0;
    ConnectorName: 'Out1'; ID: 1; XPos: 240; YPos: 11; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctAudio; BandWidth: btStatic), (ModuleID: 204;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 15;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic),
    (ModuleID: 205; ConnectorIndex: 0; ConnectorName: 'Out'; ID: 1; XPos: 239;
    YPos: 15; CodeRef: 0; InfoFunc: 0; ConnectorType: ctLogic;
    BandWidth: btDynamic), (ModuleID: 206; ConnectorIndex: 0;
    ConnectorName: 'Out'; ID: 1; XPos: 240; YPos: 30; CodeRef: 0; InfoFunc: 0;
    ConnectorType: ctControl; BandWidth: btDynamic), (ModuleID: 208;
    ConnectorIndex: 0; ConnectorName: 'Out'; ID: 3; XPos: 240; YPos: 30;
    CodeRef: 0; InfoFunc: 0; ConnectorType: ctControl; BandWidth: btDynamic));

  TextDefs: array [0 .. 908] of TG2TextDef = ((ModuleID: 1; ID: 4; XPos: 4;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 1; ID: 5;
    XPos: 96; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 1;
    ID: 6; XPos: 141; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Lin'),
    (ModuleID: 1; ID: 8; XPos: 206; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'Release'), (ModuleID: 1; ID: 9; XPos: 52; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Note'), (ModuleID: 1; ID: 11; XPos: 173; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'Exp'), (ModuleID: 1; ID: 12; XPos: 152;
    YPos: 4; ZPos: 0; FontSize: 9; slText: 'Velocity'), (ModuleID: 3; ID: 4;
    XPos: 166; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 3; ID: 5;
    XPos: 192; YPos: 3; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 3; ID: 6;
    XPos: 218; YPos: 3; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 3; ID: 7;
    XPos: 243; YPos: 3; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 3;
    ID: 11; XPos: 5; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Pad'),
    (ModuleID: 4; ID: 3; XPos: 217; YPos: 3; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 4; ID: 4; XPos: 243; YPos: 3; ZPos: 0; FontSize: 9; slText: 'R'),
    (ModuleID: 4; ID: 5; XPos: 124; YPos: 2; ZPos: 0; FontSize: 9;
    slText: 'Fx'), (ModuleID: 4; ID: 6; XPos: 155; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Bus'), (ModuleID: 4; ID: 7; XPos: 88; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Out'), (ModuleID: 4; ID: 10; XPos: 5;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'Pad'), (ModuleID: 7; ID: 22;
    XPos: 17; YPos: 43; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 7;
    ID: 24; XPos: 22; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Sync'),
    (ModuleID: 7; ID: 25; XPos: 160; YPos: 23; ZPos: 0; FontSize: 9;
    slText: 'Shape'), (ModuleID: 7; ID: 28; XPos: 79; YPos: 36; ZPos: 0;
    FontSize: 9; slText: 'Cent'), (ModuleID: 7; ID: 11; XPos: 107; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 8; ID: 22; XPos: 131;
    YPos: 9; ZPos: 0; FontSize: 9; slText: 'Shape'), (ModuleID: 8; ID: 24;
    XPos: 17; YPos: 28; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 8;
    ID: 25; XPos: 22; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Sync'),
    (ModuleID: 8; ID: 27; XPos: 109; YPos: 21; ZPos: 0; FontSize: 9;
    slText: 'Cent'), (ModuleID: 8; ID: 5; XPos: 50; YPos: 23; ZPos: 0;
    FontSize: 9; slText: 'KBT'), (ModuleID: 8; ID: 29; XPos: 174; YPos: 34;
    ZPos: 0; FontSize: 9; slText: 'Shape'), (ModuleID: 9; ID: 15; XPos: 17;
    YPos: 13; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 9; ID: 16;
    XPos: 130; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Cent'), (ModuleID: 9;
    ID: 17; XPos: 155; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Sync'),
    (ModuleID: 9; ID: 13; XPos: 55; YPos: 31; ZPos: 0; FontSize: 9;
    slText: 'KBT'), (ModuleID: 12; ID: 1; XPos: 80; YPos: 10; ZPos: 0;
    FontSize: 9; slText: 'Time'), (ModuleID: 12; ID: 10; XPos: 120; YPos: 10;
    ZPos: 0; FontSize: 9; slText: 'Brightness'), (ModuleID: 12; ID: 14; XPos: 4;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Type'), (ModuleID: 12; ID: 20;
    XPos: 234; YPos: 6; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 12;
    ID: 21; XPos: 214; YPos: 6; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 12; ID: 23; XPos: 171; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Dry/Wet'), (ModuleID: 13; ID: 5; XPos: 17; YPos: 13; ZPos: 0;
    FontSize: 9; slText: 'Pitch'), (ModuleID: 13; ID: 8; XPos: 131; YPos: 6;
    ZPos: 0; FontSize: 9; slText: 'Cent'), (ModuleID: 13; ID: 15; XPos: 161;
    YPos: 9; ZPos: 0; FontSize: 9; slText: 'Decay'), (ModuleID: 13; ID: 20;
    XPos: 195; YPos: 9; ZPos: 0; FontSize: 9; slText: 'Damp'), (ModuleID: 13;
    ID: 11; XPos: 55; YPos: 31; ZPos: 0; FontSize: 9; slText: 'KBT'),
    (ModuleID: 15; ID: 5; XPos: 62; YPos: 33; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 15; ID: 8; XPos: 61; YPos: 46; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 15; ID: 13; XPos: 4; YPos: 34;
    ZPos: 0; FontSize: 9; slText: 'Control'), (ModuleID: 15; ID: 15; XPos: 78;
    YPos: 6; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 15; ID: 14;
    XPos: 118; YPos: 6; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 15;
    ID: 16; XPos: 201; YPos: 6; ZPos: 0; FontSize: 9; slText: '4'),
    (ModuleID: 15; ID: 21; XPos: 160; YPos: 6; ZPos: 0; FontSize: 9;
    slText: '3'), (ModuleID: 15; ID: 25; XPos: 94; YPos: 18; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 15; ID: 28; XPos: 136; YPos: 18;
    ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 15; ID: 27; XPos: 219;
    YPos: 18; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 15; ID: 30;
    XPos: 178; YPos: 18; ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 17;
    ID: 6; XPos: 241; YPos: 3; ZPos: 0; FontSize: 9; slText: 'On'),
    (ModuleID: 17; ID: 8; XPos: 18; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Ctrl'), (ModuleID: 17; ID: 10; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Ctrl'), (ModuleID: 17; ID: 13; XPos: 84; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Ctrl Value'), (ModuleID: 17; ID: 16;
    XPos: 145; YPos: 7; ZPos: 0; FontSize: 9; slText: 'On'), (ModuleID: 18;
    ID: 7; XPos: 134; YPos: 17; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 18; ID: 8; XPos: 162; YPos: 17; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 18; ID: 9; XPos: 176; YPos: 16; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 18; ID: 10; XPos: 204; YPos: 16;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 19; ID: 3; XPos: 85; YPos: 4;
    ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 19; ID: 6; XPos: 123;
    YPos: 4; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 19; ID: 9;
    XPos: 161; YPos: 4; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 19;
    ID: 12; XPos: 198; YPos: 4; ZPos: 0; FontSize: 9; slText: '4'),
    (ModuleID: 19; ID: 16; XPos: 18; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Chain'), (ModuleID: 20; ID: 12; XPos: 43; YPos: 33; ZPos: 0;
    FontSize: 9; slText: 'A'), (ModuleID: 20; ID: 13; XPos: 143; YPos: 33;
    ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 20; ID: 16; XPos: 80;
    YPos: 33; ZPos: 0; FontSize: 9; slText: 'D'), (ModuleID: 20; ID: 17;
    XPos: 17; YPos: 32; ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 20;
    ID: 19; XPos: 17; YPos: 47; ZPos: 0; FontSize: 9; slText: 'AM'),
    (ModuleID: 20; ID: 22; XPos: 111; YPos: 33; ZPos: 0; FontSize: 9;
    slText: 'S'), (ModuleID: 20; ID: 25; XPos: 223; YPos: 35; ZPos: 0;
    FontSize: 9; slText: 'Env'), (ModuleID: 20; ID: 27; XPos: 176; YPos: 35;
    ZPos: 0; FontSize: 9; slText: 'Shape'), (ModuleID: 21; ID: 2; XPos: 104;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 21; ID: 3;
    XPos: 117; YPos: 3; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 21;
    ID: 5; XPos: 137; YPos: 3; ZPos: 0; FontSize: 9; slText: '3'),
    (ModuleID: 21; ID: 7; XPos: 157; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '4'), (ModuleID: 21; ID: 9; XPos: 177; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 21; ID: 11; XPos: 197; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 21; ID: 13; XPos: 217;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 21; ID: 15;
    XPos: 237; YPos: 3; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 21;
    ID: 27; XPos: 18; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Ctrl'),
    (ModuleID: 21; ID: 6; XPos: 98; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 21; ID: 8; XPos: 117; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 21; ID: 12; XPos: 137; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '3'), (ModuleID: 21; ID: 17; XPos: 156; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 21; ID: 28; XPos: 177;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 21; ID: 30;
    XPos: 197; YPos: 3; ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 21;
    ID: 32; XPos: 217; YPos: 3; ZPos: 0; FontSize: 9; slText: '7'),
    (ModuleID: 21; ID: 34; XPos: 237; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '8'), (ModuleID: 21; ID: 45; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Ctrl'), (ModuleID: 22; ID: 4; XPos: 112; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 23; ID: 4; XPos: 30;
    YPos: 30; ZPos: 0; FontSize: 9; slText: 'A'), (ModuleID: 23; ID: 5;
    XPos: 140; YPos: 30; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 23;
    ID: 8; XPos: 68; YPos: 30; ZPos: 0; FontSize: 9; slText: 'D'),
    (ModuleID: 23; ID: 11; XPos: 104; YPos: 30; ZPos: 0; FontSize: 9;
    slText: 'S'), (ModuleID: 23; ID: 20; XPos: 17; YPos: 47; ZPos: 0;
    FontSize: 9; slText: 'Gate'), (ModuleID: 23; ID: 22; XPos: 17; YPos: 62;
    ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 23; ID: 39; XPos: 220;
    YPos: 50; ZPos: 0; FontSize: 9; slText: 'Env'), (ModuleID: 24; ID: 0;
    XPos: 18; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rate'), (ModuleID: 25;
    ID: 2; XPos: 119; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Shape'),
    (ModuleID: 25; ID: 9; XPos: 28; YPos: 50; ZPos: 0; FontSize: 9;
    slText: 'Rate'), (ModuleID: 25; ID: 20; XPos: 26; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Dir'), (ModuleID: 25; ID: 25; XPos: 156; YPos: 19;
    ZPos: 0; FontSize: 9; slText: 'Phase'), (ModuleID: 25; ID: 27; XPos: 48;
    YPos: 14; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 25; ID: 33;
    XPos: 4; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 25;
    ID: 36; XPos: 4; YPos: 51; ZPos: 0; FontSize: 9; slText: 'Snc'),
    (ModuleID: 26; ID: 11; XPos: 18; YPos: 14; ZPos: 0; FontSize: 9;
    slText: 'Rate'), (ModuleID: 26; ID: 12; XPos: 147; YPos: 31; ZPos: 0;
    FontSize: 9; slText: 'KBT'), (ModuleID: 27; ID: 9; XPos: 17; YPos: 13;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 27; ID: 10; XPos: 131;
    YPos: 6; ZPos: 0; FontSize: 9; slText: 'Cent'), (ModuleID: 27; ID: 7;
    XPos: 55; YPos: 31; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 27;
    ID: 13; XPos: 230; YPos: 20; ZPos: 0; FontSize: 9; slText: 'Pitch'),
    (ModuleID: 28; ID: 10; XPos: 15; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'Curve'), (ModuleID: 29; ID: 3; XPos: 209; YPos: 4; ZPos: 0;
    FontSize: 9; slText: 'Color'), (ModuleID: 29; ID: 6; XPos: 125; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 30; ID: 1; XPos: 167;
    YPos: 19; ZPos: 0; FontSize: 9; slText: 'Stick'), (ModuleID: 30; ID: 4;
    XPos: 126; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Pedal'), (ModuleID: 30;
    ID: 7; XPos: 4; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Wheel'),
    (ModuleID: 30; ID: 10; XPos: 37; YPos: 19; ZPos: 0; FontSize: 9;
    slText: 'AftTouch'), (ModuleID: 30; ID: 13; XPos: 87; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'Pedal'), (ModuleID: 30; ID: 2; XPos: 83; YPos: 9;
    ZPos: 0; FontSize: 9; slText: 'Control'), (ModuleID: 30; ID: 5; XPos: 122;
    YPos: 9; ZPos: 0; FontSize: 9; slText: 'Sustain'), (ModuleID: 30; ID: 8;
    XPos: 167; YPos: 9; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 30;
    ID: 15; XPos: 218; YPos: 9; ZPos: 0; FontSize: 9; slText: 'Global'),
    (ModuleID: 30; ID: 16; XPos: 205; YPos: 32; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 30; ID: 17; XPos: 234; YPos: 32; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 30; ID: 18; XPos: 215; YPos: 19;
    ZPos: 0; FontSize: 9; slText: 'Wheels'), (ModuleID: 31; ID: 2; XPos: 101;
    YPos: 18; ZPos: 0; FontSize: 9; slText: 'White'), (ModuleID: 31; ID: 3;
    XPos: 153; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Colored'),
    (ModuleID: 32; ID: 7; XPos: 82; YPos: 20; ZPos: 0; FontSize: 9;
    slText: 'Lo'), (ModuleID: 32; ID: 9; XPos: 131; YPos: 20; ZPos: 0;
    FontSize: 9; slText: 'Hi'), (ModuleID: 32; ID: 15; XPos: 6; YPos: 26;
    ZPos: 0; FontSize: 9; slText: 'Lev'), (ModuleID: 33; ID: 7; XPos: 8;
    YPos: 34; ZPos: 0; FontSize: 9; slText: 'Lo'), (ModuleID: 33; ID: 8;
    XPos: 50; YPos: 34; ZPos: 0; FontSize: 9; slText: 'Gain'), (ModuleID: 33;
    ID: 9; XPos: 142; YPos: 34; ZPos: 0; FontSize: 9; slText: 'Hi'),
    (ModuleID: 33; ID: 15; XPos: 184; YPos: 41; ZPos: 0; FontSize: 9;
    slText: 'Lev'), (ModuleID: 33; ID: 20; XPos: 92; YPos: 34; ZPos: 0;
    FontSize: 9; slText: 'Freq'), (ModuleID: 33; ID: 22; XPos: 83; YPos: 5;
    ZPos: 0; FontSize: 9; slText: 'Mid'), (ModuleID: 34; ID: 10; XPos: 15;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'Curve'), (ModuleID: 35; ID: 10;
    XPos: 117; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Embouchure'),
    (ModuleID: 35; ID: 11; XPos: 178; YPos: 5; ZPos: 0; FontSize: 9;
    slText: 'Stiffness'), (ModuleID: 35; ID: 12; XPos: 4; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Excitation'), (ModuleID: 35; ID: 13; XPos: 81;
    YPos: 5; ZPos: 0; FontSize: 9; slText: 'Type'), (ModuleID: 35; ID: 6;
    XPos: 225; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Drv'), (ModuleID: 36;
    ID: 4; XPos: 38; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Ctrl'),
    (ModuleID: 40; ID: 7; XPos: 40; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 40; ID: 8; XPos: 64; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 40; ID: 13; XPos: 89; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 40; ID: 14; XPos: 114;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 40; ID: 19;
    XPos: 139; YPos: 22; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 40;
    ID: 20; XPos: 164; YPos: 22; ZPos: 0; FontSize: 9; slText: '6'),
    (ModuleID: 40; ID: 26; XPos: 189; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '7'), (ModuleID: 40; ID: 29; XPos: 214; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '8'), (ModuleID: 40; ID: 1; XPos: 216; YPos: 5;
    ZPos: 0; FontSize: 9; slText: 'Chain'), (ModuleID: 40; ID: 3; XPos: 14;
    YPos: 34; ZPos: 0; FontSize: 9; slText: 'Pad'), (ModuleID: 41; ID: 2;
    XPos: 22; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Trig'), (ModuleID: 41;
    ID: 5; XPos: 62; YPos: 17; ZPos: 0; FontSize: 9; slText: 'AM'),
    (ModuleID: 41; ID: 14; XPos: 191; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'Env'), (ModuleID: 45; ID: 15; XPos: 42; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'Freq'), (ModuleID: 45; ID: 18; XPos: 73; YPos: 19;
    ZPos: 0; FontSize: 9; slText: 'Res'), (ModuleID: 45; ID: 23; XPos: 213;
    YPos: 4; ZPos: 0; FontSize: 9; slText: 'Level'), (ModuleID: 45; ID: 24;
    XPos: 122; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Vowel navigator'),
    (ModuleID: 46; ID: 0; XPos: 23; YPos: 32; ZPos: 0; FontSize: 9;
    slText: 'Trig'), (ModuleID: 46; ID: 8; XPos: 18; YPos: 47; ZPos: 0;
    FontSize: 9; slText: 'AM'), (ModuleID: 46; ID: 21; XPos: 52; YPos: 34;
    ZPos: 0; FontSize: 9; slText: 'A'), (ModuleID: 46; ID: 22; XPos: 138;
    YPos: 34; ZPos: 0; FontSize: 9; slText: 'D'), (ModuleID: 46; ID: 25;
    XPos: 96; YPos: 35; ZPos: 0; FontSize: 9; slText: 'H'), (ModuleID: 46;
    ID: 30; XPos: 220; YPos: 35; ZPos: 0; FontSize: 9; slText: 'Env'),
    (ModuleID: 46; ID: 6; XPos: 177; YPos: 34; ZPos: 0; FontSize: 9;
    slText: 'Shape'), (ModuleID: 47; ID: 6; XPos: 133; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'L'), (ModuleID: 47; ID: 9; XPos: 162; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 47; ID: 8; XPos: 203;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 47; ID: 11;
    XPos: 232; YPos: 16; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 48;
    ID: 4; XPos: 13; YPos: 14; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 48; ID: 9; XPos: 44; YPos: 14; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 48; ID: 14; XPos: 76; YPos: 14; ZPos: 0;
    FontSize: 9; slText: '3'), (ModuleID: 48; ID: 19; XPos: 108; YPos: 14;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 48; ID: 24; XPos: 140;
    YPos: 14; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 48; ID: 29;
    XPos: 172; YPos: 14; ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 48;
    ID: 31; XPos: 4; YPos: 37; ZPos: 0; FontSize: 9; slText: 'Pan'),
    (ModuleID: 48; ID: 32; XPos: 4; YPos: 62; ZPos: 0; FontSize: 9;
    slText: 'Lvl'), (ModuleID: 48; ID: 37; XPos: 214; YPos: 40; ZPos: 0;
    FontSize: 9; slText: 'Master'), (ModuleID: 48; ID: 40; XPos: 209; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Main Out'), (ModuleID: 48; ID: 49; XPos: 219;
    YPos: 51; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 48; ID: 50;
    XPos: 243; YPos: 51; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 49;
    ID: 4; XPos: 17; YPos: 23; ZPos: 0; FontSize: 9; slText: 'Pitch'),
    (ModuleID: 49; ID: 6; XPos: 54; YPos: 32; ZPos: 0; FontSize: 9;
    slText: 'KBT'), (ModuleID: 49; ID: 9; XPos: 98; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Freq'), (ModuleID: 49; ID: 14; XPos: 188; YPos: 6;
    ZPos: 0; FontSize: 9; slText: 'dB/Oct'), (ModuleID: 49; ID: 19; XPos: 227;
    YPos: 19; ZPos: 0; FontSize: 9; slText: 'HP'), (ModuleID: 49; ID: 20;
    XPos: 227; YPos: 33; ZPos: 0; FontSize: 9; slText: 'BP'), (ModuleID: 49;
    ID: 21; XPos: 227; YPos: 47; ZPos: 0; FontSize: 9; slText: 'LP'),
    (ModuleID: 49; ID: 28; XPos: 153; YPos: 6; ZPos: 0; FontSize: 9;
    slText: 'Res'), (ModuleID: 51; ID: 5; XPos: 17; YPos: 38; ZPos: 0;
    FontSize: 9; slText: 'Pitch'), (ModuleID: 51; ID: 8; XPos: 46; YPos: 42;
    ZPos: 0; FontSize: 9; slText: 'FM Lin'), (ModuleID: 51; ID: 12; XPos: 87;
    YPos: 47; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 51; ID: 13;
    XPos: 81; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 51;
    ID: 18; XPos: 139; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Res'),
    (ModuleID: 51; ID: 21; XPos: 194; YPos: 40; ZPos: 0; FontSize: 9;
    slText: 'dB/Oct'), (ModuleID: 51; ID: 29; XPos: 120; YPos: 49; ZPos: 0;
    FontSize: 9; slText: 'Res'), (ModuleID: 52; ID: 8; XPos: 17; YPos: 30;
    ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 52; ID: 9; XPos: 40;
    YPos: 19; ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 52; ID: 13;
    XPos: 55; YPos: 40; ZPos: 0; FontSize: 9; slText: 'L1'), (ModuleID: 52;
    ID: 16; XPos: 115; YPos: 40; ZPos: 0; FontSize: 9; slText: 'L2'),
    (ModuleID: 52; ID: 19; XPos: 173; YPos: 40; ZPos: 0; FontSize: 9;
    slText: 'L3'), (ModuleID: 52; ID: 22; XPos: 232; YPos: 40; ZPos: 0;
    FontSize: 9; slText: 'L4'), (ModuleID: 52; ID: 25; XPos: 27; YPos: 40;
    ZPos: 0; FontSize: 9; slText: 'T1'), (ModuleID: 52; ID: 28; XPos: 85;
    YPos: 40; ZPos: 0; FontSize: 9; slText: 'T2'), (ModuleID: 52; ID: 31;
    XPos: 145; YPos: 40; ZPos: 0; FontSize: 9; slText: 'T3'), (ModuleID: 52;
    ID: 34; XPos: 204; YPos: 40; ZPos: 0; FontSize: 9; slText: 'T4'),
    (ModuleID: 52; ID: 41; XPos: 59; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'Sustain'), (ModuleID: 52; ID: 43; XPos: 178; YPos: 10; ZPos: 0;
    FontSize: 9; slText: 'Shape'), (ModuleID: 52; ID: 35; XPos: 223; YPos: 22;
    ZPos: 0; FontSize: 9; slText: 'Env'), (ModuleID: 53; ID: 6; XPos: 78;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Sample and Hold'), (ModuleID: 53;
    ID: 9; XPos: 144; YPos: 15; ZPos: 0; FontSize: 9; slText: 'Ctrl'),
    (ModuleID: 54; ID: 13; XPos: 19; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Freq'), (ModuleID: 54; ID: 18; XPos: 86; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Res'), (ModuleID: 55; ID: 3; XPos: 22; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Trig'), (ModuleID: 55; ID: 4; XPos: 62;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 55; ID: 14;
    XPos: 191; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Env'), (ModuleID: 56;
    ID: 5; XPos: 5; YPos: 41; ZPos: 0; FontSize: 9; slText: 'Pitch'),
    (ModuleID: 56; ID: 8; XPos: 113; YPos: 6; ZPos: 0; FontSize: 9;
    slText: 'Cent'), (ModuleID: 56; ID: 15; XPos: 182; YPos: 39; ZPos: 0;
    FontSize: 9; slText: 'Decay'), (ModuleID: 56; ID: 20; XPos: 184; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Damp'), (ModuleID: 56; ID: 11; XPos: 37;
    YPos: 32; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 56; ID: 13;
    XPos: 226; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Drv'), (ModuleID: 56;
    ID: 17; XPos: 149; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Pos'),
    (ModuleID: 56; ID: 19; XPos: 74; YPos: 56; ZPos: 0; FontSize: 9;
    slText: 'Algorithm'), (ModuleID: 56; ID: 21; XPos: 235; YPos: 46; ZPos: 0;
    FontSize: 9; slText: 'Out'), (ModuleID: 57; ID: 5; XPos: 212; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 57; ID: 7; XPos: 111;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 57; ID: 10;
    XPos: 154; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Value'), (ModuleID: 57;
    ID: 12; XPos: 24; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Send'),
    (ModuleID: 57; ID: 17; XPos: 81; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'Echo'), (ModuleID: 58; ID: 26; XPos: 66; YPos: 33; ZPos: 0;
    FontSize: 9; slText: 'Dcy'), (ModuleID: 58; ID: 27; XPos: 94; YPos: 33;
    ZPos: 0; FontSize: 9; slText: 'Lev'), (ModuleID: 58; ID: 29; XPos: 127;
    YPos: 38; ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 58; ID: 30;
    XPos: 155; YPos: 38; ZPos: 0; FontSize: 9; slText: 'Res'), (ModuleID: 58;
    ID: 31; XPos: 182; YPos: 38; ZPos: 0; FontSize: 9; slText: 'Swp'),
    (ModuleID: 58; ID: 32; XPos: 208; YPos: 38; ZPos: 0; FontSize: 9;
    slText: 'Dcy'), (ModuleID: 58; ID: 34; XPos: 126; YPos: 10; ZPos: 0;
    FontSize: 9; slText: 'Preset'), (ModuleID: 58; ID: 36; XPos: 127; YPos: 85;
    ZPos: 0; FontSize: 9; slText: 'Amt'), (ModuleID: 58; ID: 37; XPos: 155;
    YPos: 85; ZPos: 0; FontSize: 9; slText: 'Dcy'), (ModuleID: 58; ID: 38;
    XPos: 184; YPos: 85; ZPos: 0; FontSize: 9; slText: 'Click'), (ModuleID: 58;
    ID: 39; XPos: 210; YPos: 85; ZPos: 0; FontSize: 9; slText: 'Noise'),
    (ModuleID: 58; ID: 40; XPos: 4; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Trig'), (ModuleID: 58; ID: 41; XPos: 4; YPos: 96; ZPos: 0;
    FontSize: 9; slText: 'Vel'), (ModuleID: 58; ID: 42; XPos: 4; YPos: 58;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 58; ID: 44; XPos: 94;
    YPos: 85; ZPos: 0; FontSize: 9; slText: 'Lev'), (ModuleID: 58; ID: 45;
    XPos: 66; YPos: 85; ZPos: 0; FontSize: 9; slText: 'Dcy'), (ModuleID: 59;
    ID: 4; XPos: 110; YPos: 13; ZPos: 0; FontSize: 9; slText: 'A'),
    (ModuleID: 59; ID: 5; XPos: 214; YPos: 13; ZPos: 0; FontSize: 9;
    slText: 'A>=C'), (ModuleID: 59; ID: 6; XPos: 141; YPos: 13; ZPos: 0;
    FontSize: 9; slText: 'C'), (ModuleID: 60; ID: 2; XPos: 30; YPos: 21;
    ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 60; ID: 3; XPos: 49;
    YPos: 21; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 60; ID: 5;
    XPos: 69; YPos: 21; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 60;
    ID: 7; XPos: 88; YPos: 21; ZPos: 0; FontSize: 9; slText: '4'),
    (ModuleID: 60; ID: 9; XPos: 109; YPos: 21; ZPos: 0; FontSize: 9;
    slText: '5'), (ModuleID: 60; ID: 11; XPos: 129; YPos: 21; ZPos: 0;
    FontSize: 9; slText: '6'), (ModuleID: 60; ID: 13; XPos: 149; YPos: 21;
    ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 60; ID: 15; XPos: 169;
    YPos: 21; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 60; ID: 26;
    XPos: 4; YPos: 21; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 60;
    ID: 29; XPos: 183; YPos: 10; ZPos: 0; FontSize: 9; slText: 'X-Fade'),
    (ModuleID: 61; ID: 7; XPos: 113; YPos: 2; ZPos: 0; FontSize: 9;
    slText: 'Clip Lev'), (ModuleID: 61; ID: 6; XPos: 4; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Shape'), (ModuleID: 62; ID: 13; XPos: 77; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Type'), (ModuleID: 62; ID: 10; XPos: 4;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'Shape'), (ModuleID: 63; ID: 3;
    XPos: 88; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Pitch Ratio'),
    (ModuleID: 63; ID: 4; XPos: 201; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Delay'), (ModuleID: 63; ID: 11; XPos: 4; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'Ratio'), (ModuleID: 66; ID: 1; XPos: 17; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Chain'), (ModuleID: 68; ID: 9; XPos: 220;
    YPos: 19; ZPos: 0; FontSize: 9; slText: '1/96'), (ModuleID: 68; ID: 10;
    XPos: 222; YPos: 33; ZPos: 0; FontSize: 9; slText: '1/16'), (ModuleID: 68;
    ID: 11; XPos: 192; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Clk Active'),
    (ModuleID: 68; ID: 12; XPos: 100; YPos: 5; ZPos: 0; FontSize: 9;
    slText: 'Tempo'), (ModuleID: 68; ID: 17; XPos: 216; YPos: 47; ZPos: 0;
    FontSize: 9; slText: 'Sync'), (ModuleID: 68; ID: 1; XPos: 88; YPos: 41;
    ZPos: 0; FontSize: 9; slText: 'Sync every'), (ModuleID: 68; ID: 19;
    XPos: 179; YPos: 42; ZPos: 0; FontSize: 9; slText: 'beats'), (ModuleID: 68;
    ID: 0; XPos: 11; YPos: 35; ZPos: 0; FontSize: 9; slText: 'Source'),
    (ModuleID: 68; ID: 15; XPos: 22; YPos: 19; ZPos: 0; FontSize: 9;
    slText: 'Reset'), (ModuleID: 68; ID: 22; XPos: 55; YPos: 25; ZPos: 0;
    FontSize: 9; slText: 'Swing'), (ModuleID: 69; ID: 2; XPos: 97; YPos: 13;
    ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 69; ID: 4; XPos: 135;
    YPos: 13; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 69; ID: 8;
    XPos: 155; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Divider'), (ModuleID: 71;
    ID: 7; XPos: 81; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Attack'),
    (ModuleID: 71; ID: 8; XPos: 148; YPos: 2; ZPos: 0; FontSize: 9;
    slText: 'Release'), (ModuleID: 72; ID: 5; XPos: 104; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Range'), (ModuleID: 75; ID: 2; XPos: 82; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 75; ID: 5; XPos: 144;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Notes'), (ModuleID: 76; ID: 5;
    XPos: 38; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 78;
    ID: 4; XPos: 78; YPos: 7; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 78;
    ID: 9; XPos: 77; YPos: 20; ZPos: 0; FontSize: 9; slText: '5'),
    (ModuleID: 78; ID: 12; XPos: 4; YPos: 34; ZPos: 0; FontSize: 9;
    slText: 'Control'), (ModuleID: 78; ID: 14; XPos: 83; YPos: 35; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 78; ID: 15; XPos: 125; YPos: 35;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 78; ID: 17; XPos: 210;
    YPos: 35; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 78; ID: 22;
    XPos: 168; YPos: 35; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 78;
    ID: 26; XPos: 105; YPos: 48; ZPos: 0; FontSize: 9; slText: '5'),
    (ModuleID: 78; ID: 27; XPos: 148; YPos: 48; ZPos: 0; FontSize: 9;
    slText: '6'), (ModuleID: 78; ID: 28; XPos: 234; YPos: 48; ZPos: 0;
    FontSize: 9; slText: '8'), (ModuleID: 78; ID: 29; XPos: 191; YPos: 48;
    ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 78; ID: 1; XPos: 53;
    YPos: 36; ZPos: 0; FontSize: 9; slText: 'In'), (ModuleID: 79; ID: 13;
    XPos: 4; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Control'), (ModuleID: 79;
    ID: 15; XPos: 78; YPos: 13; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 79; ID: 14; XPos: 120; YPos: 13; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 79; ID: 16; XPos: 205; YPos: 13; ZPos: 0;
    FontSize: 9; slText: '4'), (ModuleID: 79; ID: 21; XPos: 163; YPos: 13;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 84; ID: 11; XPos: 50;
    YPos: 20; ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 84; ID: 14;
    XPos: 185; YPos: 32; ZPos: 0; FontSize: 9; slText: 'End'), (ModuleID: 84;
    ID: 21; XPos: 81; YPos: 20; ZPos: 0; FontSize: 9; slText: 'A'),
    (ModuleID: 84; ID: 24; XPos: 162; YPos: 4; ZPos: 0; FontSize: 9;
    slText: 'Shape'), (ModuleID: 85; ID: 1; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Ctrl'), (ModuleID: 85; ID: 4; XPos: 79; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'From'), (ModuleID: 85; ID: 7; XPos: 136;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'To'), (ModuleID: 86; ID: 0;
    XPos: 77; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 86; ID: 1;
    XPos: 99; YPos: 3; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 86; ID: 2;
    XPos: 122; YPos: 3; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 86;
    ID: 3; XPos: 145; YPos: 3; ZPos: 0; FontSize: 9; slText: '4'),
    (ModuleID: 86; ID: 4; XPos: 168; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '5'), (ModuleID: 86; ID: 5; XPos: 191; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '6'), (ModuleID: 86; ID: 6; XPos: 214; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 86; ID: 7; XPos: 237;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 86; ID: 17;
    XPos: 77; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 86;
    ID: 18; XPos: 99; YPos: 3; ZPos: 0; FontSize: 9; slText: '2'),
    (ModuleID: 86; ID: 19; XPos: 122; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '3'), (ModuleID: 86; ID: 20; XPos: 143; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '4'), (ModuleID: 86; ID: 21; XPos: 168; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 86; ID: 22; XPos: 191;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 86; ID: 23;
    XPos: 214; YPos: 3; ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 86;
    ID: 24; XPos: 237; YPos: 3; ZPos: 0; FontSize: 9; slText: '8'),
    (ModuleID: 86; ID: 42; XPos: 22; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Clk'), (ModuleID: 86; ID: 45; XPos: 58; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Rst'), (ModuleID: 87; ID: 7; XPos: 4; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 88; ID: 3; XPos: 4;
    YPos: 19; ZPos: 0; FontSize: 9; slText: 'Control'), (ModuleID: 88; ID: 9;
    XPos: 84; YPos: 32; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 88;
    ID: 10; XPos: 125; YPos: 32; ZPos: 0; FontSize: 9; slText: '2'),
    (ModuleID: 88; ID: 16; XPos: 210; YPos: 32; ZPos: 0; FontSize: 9;
    slText: '4'), (ModuleID: 88; ID: 13; XPos: 168; YPos: 32; ZPos: 0;
    FontSize: 9; slText: '3'), (ModuleID: 88; ID: 5; XPos: 53; YPos: 21;
    ZPos: 0; FontSize: 9; slText: 'In'), (ModuleID: 89; ID: 2; XPos: 106;
    YPos: 10; ZPos: 0; FontSize: 9; slText: 'Rate'), (ModuleID: 89; ID: 3;
    XPos: 150; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 89;
    ID: 11; XPos: 189; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Feedback'),
    (ModuleID: 90; ID: 5; XPos: 38; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'Ctrl'), (ModuleID: 90; ID: 10; XPos: 206; YPos: 15; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 90; ID: 11; XPos: 243; YPos: 5;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 91; ID: 12; XPos: 63;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 91; ID: 13;
    XPos: 97; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 92;
    ID: 4; XPos: 17; YPos: 23; ZPos: 0; FontSize: 9; slText: 'Pitch'),
    (ModuleID: 92; ID: 8; XPos: 54; YPos: 32; ZPos: 0; FontSize: 9;
    slText: 'KBT'), (ModuleID: 92; ID: 9; XPos: 98; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Freq'), (ModuleID: 92; ID: 11; XPos: 153; YPos: 6;
    ZPos: 0; FontSize: 9; slText: 'Res'), (ModuleID: 92; ID: 14; XPos: 194;
    YPos: 33; ZPos: 0; FontSize: 9; slText: 'dB/Oct'), (ModuleID: 94; ID: 5;
    XPos: 233; YPos: 32; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 94;
    ID: 9; XPos: 210; YPos: 32; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 94; ID: 10; XPos: 86; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Detune'), (ModuleID: 94; ID: 11; XPos: 131; YPos: 10; ZPos: 0;
    FontSize: 9; slText: 'Amount'), (ModuleID: 96; ID: 0; XPos: 17; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 96; ID: 10; XPos: 182;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'F'), (ModuleID: 96; ID: 1; XPos: 43;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 97; ID: 11;
    XPos: 17; YPos: 13; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 97;
    ID: 13; XPos: 127; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Cent'),
    (ModuleID: 97; ID: 6; XPos: 51; YPos: 31; ZPos: 0; FontSize: 9;
    slText: 'KBT'), (ModuleID: 98; ID: 6; XPos: 4; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'Shift'), (ModuleID: 98; ID: 18; XPos: 229; YPos: 32;
    ZPos: 0; FontSize: 9; slText: 'Up'), (ModuleID: 98; ID: 19; XPos: 205;
    YPos: 32; ZPos: 0; FontSize: 9; slText: 'Dn'), (ModuleID: 98; ID: 7;
    XPos: 84; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 98;
    ID: 10; XPos: 118; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Freq Shift'),
    (ModuleID: 100; ID: 4; XPos: 38; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'Ctrl'), (ModuleID: 100; ID: 6; XPos: 176; YPos: 15; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 100; ID: 9; XPos: 223; YPos: 15;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 102; ID: 9; XPos: 87;
    YPos: 11; ZPos: 0; FontSize: 9; slText: 'Spread'), (ModuleID: 102; ID: 16;
    XPos: 50; YPos: 14; ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 102;
    ID: 27; XPos: 157; YPos: 47; ZPos: 0; FontSize: 9; slText: 'Notch'),
    (ModuleID: 102; ID: 29; XPos: 130; YPos: 6; ZPos: 0; FontSize: 9;
    slText: 'FB'), (ModuleID: 102; ID: 23; XPos: 153; YPos: 10; ZPos: 0;
    FontSize: 9; slText: 'Type'), (ModuleID: 102; ID: 17; XPos: 17; YPos: 43;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 102; ID: 24; XPos: 9;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 102; ID: 32;
    XPos: 200; YPos: 42; ZPos: 0; FontSize: 9; slText: 'Level'), (ModuleID: 102;
    ID: 33; XPos: 80; YPos: 49; ZPos: 0; FontSize: 9; slText: 'Spr'),
    (ModuleID: 102; ID: 34; XPos: 121; YPos: 49; ZPos: 0; FontSize: 9;
    slText: 'FB'), (ModuleID: 103; ID: 7; XPos: 28; YPos: 35; ZPos: 0;
    FontSize: 9; slText: 'Freq'), (ModuleID: 103; ID: 8; XPos: 77; YPos: 35;
    ZPos: 0; FontSize: 9; slText: 'Gain'), (ModuleID: 103; ID: 9; XPos: 126;
    YPos: 35; ZPos: 0; FontSize: 9; slText: 'BW'), (ModuleID: 103; ID: 15;
    XPos: 184; YPos: 41; ZPos: 0; FontSize: 9; slText: 'Lev'), (ModuleID: 105;
    ID: 9; XPos: 18; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Ctrl'),
    (ModuleID: 105; ID: 11; XPos: 18; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Ctrl'), (ModuleID: 105; ID: 12; XPos: 84; YPos: 3; ZPos: 0;
    FontSize: 9; slText: 'Ctrl Value'), (ModuleID: 105; ID: 17; XPos: 145;
    YPos: 7; ZPos: 0; FontSize: 9; slText: 'On'), (ModuleID: 105; ID: 7;
    XPos: 223; YPos: 15; ZPos: 0; FontSize: 9; slText: 'On'), (ModuleID: 106;
    ID: 3; XPos: 180; YPos: 8; ZPos: 0; FontSize: 9; slText: 'Width'),
    (ModuleID: 106; ID: 10; XPos: 17; YPos: 13; ZPos: 0; FontSize: 9;
    slText: 'Pitch'), (ModuleID: 106; ID: 12; XPos: 131; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Cent'), (ModuleID: 106; ID: 18; XPos: 55; YPos: 31;
    ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 108; ID: 0; XPos: 41;
    YPos: 15; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 108; ID: 2;
    XPos: 52; YPos: 15; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 108;
    ID: 3; XPos: 64; YPos: 15; ZPos: 0; FontSize: 9; slText: '3'),
    (ModuleID: 108; ID: 4; XPos: 76; YPos: 15; ZPos: 0; FontSize: 9;
    slText: '4'), (ModuleID: 108; ID: 5; XPos: 88; YPos: 15; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 108; ID: 6; XPos: 100; YPos: 15;
    ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 108; ID: 7; XPos: 112;
    YPos: 15; ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 108; ID: 8;
    XPos: 124; YPos: 15; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 108;
    ID: 9; XPos: 136; YPos: 15; ZPos: 0; FontSize: 9; slText: '9'),
    (ModuleID: 108; ID: 10; XPos: 147; YPos: 15; ZPos: 0; FontSize: 9;
    slText: '10'), (ModuleID: 108; ID: 11; XPos: 160; YPos: 15; ZPos: 0;
    FontSize: 9; slText: '11'), (ModuleID: 108; ID: 12; XPos: 171; YPos: 15;
    ZPos: 0; FontSize: 9; slText: '12'), (ModuleID: 108; ID: 13; XPos: 183;
    YPos: 15; ZPos: 0; FontSize: 9; slText: '13'), (ModuleID: 108; ID: 14;
    XPos: 194; YPos: 15; ZPos: 0; FontSize: 9; slText: '14'), (ModuleID: 108;
    ID: 15; XPos: 206; YPos: 15; ZPos: 0; FontSize: 9; slText: '15'),
    (ModuleID: 108; ID: 16; XPos: 218; YPos: 15; ZPos: 0; FontSize: 9;
    slText: '16'), (ModuleID: 108; ID: 17; XPos: 6; YPos: 15; ZPos: 0;
    FontSize: 9; slText: 'Ctrl'), (ModuleID: 108; ID: 71; XPos: 102; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Analysis'), (ModuleID: 108; ID: 64; XPos: 8;
    YPos: 93; ZPos: 0; FontSize: 9; slText: 'Emp'), (ModuleID: 113; ID: 5;
    XPos: 203; YPos: 16; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 113;
    ID: 7; XPos: 233; YPos: 16; ZPos: 0; FontSize: 9; slText: 'R'),
    (ModuleID: 113; ID: 3; XPos: 133; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'L'), (ModuleID: 113; ID: 4; XPos: 163; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'R'), (ModuleID: 114; ID: 6; XPos: 176; YPos: 16;
    ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 114; ID: 7; XPos: 204;
    YPos: 16; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 114; ID: 4;
    XPos: 135; YPos: 16; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 114;
    ID: 5; XPos: 163; YPos: 16; ZPos: 0; FontSize: 9; slText: '2'),
    (ModuleID: 115; ID: 14; XPos: 4; YPos: 20; ZPos: 0; FontSize: 9;
    slText: 'Note'), (ModuleID: 115; ID: 10; XPos: 117; YPos: 20; ZPos: 0;
    FontSize: 9; slText: 'BP'), (ModuleID: 115; ID: 11; XPos: 83; YPos: 20;
    ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 115; ID: 13; XPos: 147;
    YPos: 20; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 115; ID: 16;
    XPos: 33; YPos: 20; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 115;
    ID: 18; XPos: 185; YPos: 32; ZPos: 0; FontSize: 9; slText: 'Level'),
    (ModuleID: 116; ID: 2; XPos: 80; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 116; ID: 3; XPos: 99; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 116; ID: 5; XPos: 118; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 116; ID: 7; XPos: 136;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 116; ID: 9;
    XPos: 156; YPos: 3; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 116;
    ID: 11; XPos: 175; YPos: 3; ZPos: 0; FontSize: 9; slText: '6'),
    (ModuleID: 116; ID: 13; XPos: 194; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '7'), (ModuleID: 116; ID: 15; XPos: 213; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '8'), (ModuleID: 116; ID: 17; XPos: 4; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'Pad'), (ModuleID: 117; ID: 10; XPos: 136;
    YPos: 33; ZPos: 0; FontSize: 9; slText: '0'), (ModuleID: 117; ID: 11;
    XPos: 166; YPos: 33; ZPos: 0; FontSize: 9; slText: 'Bal.'), (ModuleID: 117;
    ID: 12; XPos: 146; YPos: 7; ZPos: 0; FontSize: 9; slText: 'AM'),
    (ModuleID: 117; ID: 13; XPos: 191; YPos: 9; ZPos: 0; FontSize: 9;
    slText: 'Mod'), (ModuleID: 117; ID: 14; XPos: 87; YPos: 11; ZPos: 0;
    FontSize: 9; slText: 'Mod depth'), (ModuleID: 118; ID: 8; XPos: 162;
    YPos: 29; ZPos: 0; FontSize: 9; slText: 'Bits'), (ModuleID: 118; ID: 10;
    XPos: 161; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Quantization'),
    (ModuleID: 118; ID: 15; XPos: 85; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Sample Rate'), (ModuleID: 118; ID: 9; XPos: 4; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'Rate'), (ModuleID: 119; ID: 7; XPos: 17; YPos: 62;
    ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 119; ID: 3; XPos: 17;
    YPos: 38; ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 119; ID: 18;
    XPos: 45; YPos: 49; ZPos: 0; FontSize: 9; slText: 'A'), (ModuleID: 119;
    ID: 19; XPos: 148; YPos: 49; ZPos: 0; FontSize: 9; slText: 'D2'),
    (ModuleID: 119; ID: 22; XPos: 81; YPos: 49; ZPos: 0; FontSize: 9;
    slText: 'D1'), (ModuleID: 119; ID: 25; XPos: 116; YPos: 49; ZPos: 0;
    FontSize: 9; slText: 'L1'), (ModuleID: 119; ID: 28; XPos: 182; YPos: 49;
    ZPos: 0; FontSize: 9; slText: 'L2'), (ModuleID: 119; ID: 31; XPos: 219;
    YPos: 49; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 119; ID: 33;
    XPos: 37; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Sustain'),
    (ModuleID: 119; ID: 23; XPos: 223; YPos: 22; ZPos: 0; FontSize: 9;
    slText: 'Env'), (ModuleID: 119; ID: 34; XPos: 178; YPos: 9; ZPos: 0;
    FontSize: 9; slText: 'Shape'), (ModuleID: 121; ID: 2; XPos: 4; YPos: 34;
    ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 121; ID: 4; XPos: 4;
    YPos: 57; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 121; ID: 7;
    XPos: 218; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Park'), (ModuleID: 121;
    ID: 9; XPos: 234; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Link'),
    (ModuleID: 121; ID: 65; XPos: 29; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 121; ID: 64; XPos: 40; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 121; ID: 67; XPos: 52; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 121; ID: 66; XPos: 64;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 121; ID: 69;
    XPos: 76; YPos: 22; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 121;
    ID: 68; XPos: 88; YPos: 22; ZPos: 0; FontSize: 9; slText: '6'),
    (ModuleID: 121; ID: 71; XPos: 100; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '7'), (ModuleID: 121; ID: 70; XPos: 112; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '8'), (ModuleID: 121; ID: 73; XPos: 124; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '9'), (ModuleID: 121; ID: 72; XPos: 135;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '10'), (ModuleID: 121; ID: 75;
    XPos: 148; YPos: 22; ZPos: 0; FontSize: 9; slText: '11'), (ModuleID: 121;
    ID: 74; XPos: 159; YPos: 22; ZPos: 0; FontSize: 9; slText: '12'),
    (ModuleID: 121; ID: 77; XPos: 171; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '13'), (ModuleID: 121; ID: 76; XPos: 183; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '14'), (ModuleID: 121; ID: 79; XPos: 195; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '15'), (ModuleID: 121; ID: 78; XPos: 207;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '16'), (ModuleID: 121; ID: 83;
    XPos: 129; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Length'), (ModuleID: 121;
    ID: 11; XPos: 4; YPos: 79; ZPos: 0; FontSize: 9; slText: 'Loop'),
    (ModuleID: 121; ID: 21; XPos: 82; YPos: 5; ZPos: 0; FontSize: 9;
    slText: 'Rec'), (ModuleID: 123; ID: 2; XPos: 43; YPos: 25; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 123; ID: 5; XPos: 90; YPos: 25;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 123; ID: 8; XPos: 138;
    YPos: 25; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 123; ID: 11;
    XPos: 186; YPos: 25; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 123;
    ID: 15; XPos: 216; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Chain'),
    (ModuleID: 123; ID: 20; XPos: 14; YPos: 34; ZPos: 0; FontSize: 9;
    slText: 'Pad'), (ModuleID: 124; ID: 3; XPos: 78; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 124; ID: 2; XPos: 97; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 124; ID: 4; XPos: 117;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 124; ID: 6;
    XPos: 136; YPos: 3; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 124;
    ID: 8; XPos: 157; YPos: 3; ZPos: 0; FontSize: 9; slText: '5'),
    (ModuleID: 124; ID: 10; XPos: 177; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '6'), (ModuleID: 124; ID: 12; XPos: 197; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '7'), (ModuleID: 124; ID: 14; XPos: 217; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 124; ID: 26; XPos: 18;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 125; ID: 5;
    XPos: 163; YPos: 5; ZPos: 0; FontSize: 9; slText: 'Sweep'), (ModuleID: 127;
    ID: 8; XPos: 217; YPos: 3; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 127; ID: 2; XPos: 243; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'R'), (ModuleID: 127; ID: 6; XPos: 91; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Fx'), (ModuleID: 127; ID: 9; XPos: 5; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'Pad'), (ModuleID: 128; ID: 4; XPos: 110;
    YPos: 13; ZPos: 0; FontSize: 9; slText: 'A'), (ModuleID: 128; ID: 5;
    XPos: 142; YPos: 13; ZPos: 0; FontSize: 9; slText: 'B'), (ModuleID: 128;
    ID: 6; XPos: 222; YPos: 13; ZPos: 0; FontSize: 9; slText: 'Max'),
    (ModuleID: 128; ID: 7; XPos: 193; YPos: 13; ZPos: 0; FontSize: 9;
    slText: 'Min'), (ModuleID: 130; ID: 16; XPos: 77; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 130; ID: 19; XPos: 98; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 130; ID: 18; XPos: 122;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 130; ID: 21;
    XPos: 145; YPos: 3; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 130;
    ID: 20; XPos: 165; YPos: 3; ZPos: 0; FontSize: 9; slText: '16'),
    (ModuleID: 130; ID: 23; XPos: 186; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '32'), (ModuleID: 130; ID: 22; XPos: 208; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '64'), (ModuleID: 130; ID: 25; XPos: 229; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '128'), (ModuleID: 130; ID: 43; XPos: 22;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 130; ID: 44;
    XPos: 58; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 131;
    ID: 16; XPos: 68; YPos: 15; ZPos: 0; FontSize: 9; slText: 'D0'),
    (ModuleID: 131; ID: 19; XPos: 93; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'D1'), (ModuleID: 131; ID: 18; XPos: 114; YPos: 15; ZPos: 0;
    FontSize: 9; slText: 'D2'), (ModuleID: 131; ID: 21; XPos: 137; YPos: 15;
    ZPos: 0; FontSize: 9; slText: 'D3'), (ModuleID: 131; ID: 20; XPos: 160;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'D4'), (ModuleID: 131; ID: 23;
    XPos: 183; YPos: 15; ZPos: 0; FontSize: 9; slText: 'D5'), (ModuleID: 131;
    ID: 22; XPos: 206; YPos: 15; ZPos: 0; FontSize: 9; slText: 'D6'),
    (ModuleID: 131; ID: 25; XPos: 229; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'D7'), (ModuleID: 131; ID: 41; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Input'), (ModuleID: 132; ID: 3; XPos: 80; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'D0'), (ModuleID: 132; ID: 2; XPos: 100;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'D1'), (ModuleID: 132; ID: 4;
    XPos: 120; YPos: 3; ZPos: 0; FontSize: 9; slText: 'D2'), (ModuleID: 132;
    ID: 6; XPos: 140; YPos: 3; ZPos: 0; FontSize: 9; slText: 'D3'),
    (ModuleID: 132; ID: 8; XPos: 160; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'D4'), (ModuleID: 132; ID: 10; XPos: 180; YPos: 3; ZPos: 0;
    FontSize: 9; slText: 'D5'), (ModuleID: 132; ID: 12; XPos: 200; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'D6'), (ModuleID: 132; ID: 14; XPos: 220;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'D7'), (ModuleID: 134; ID: 7;
    XPos: 4; YPos: 17; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 139;
    ID: 7; XPos: 84; YPos: 7; ZPos: 0; FontSize: 9; slText: 'High = Track'),
    (ModuleID: 139; ID: 9; XPos: 144; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'Ctrl'), (ModuleID: 139; ID: 10; XPos: 85; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Low = Hold'), (ModuleID: 140; ID: 14; XPos: 199;
    YPos: 5; ZPos: 0; FontSize: 9; slText: 'Chain'), (ModuleID: 140; ID: 3;
    XPos: 7; YPos: 16; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 140;
    ID: 4; XPos: 22; YPos: 16; ZPos: 0; FontSize: 9; slText: 'R'),
    (ModuleID: 140; ID: 9; XPos: 61; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'L'), (ModuleID: 140; ID: 10; XPos: 76; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'R'), (ModuleID: 140; ID: 20; XPos: 115; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 140; ID: 21; XPos: 130;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 140; ID: 26;
    XPos: 169; YPos: 16; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 140;
    ID: 27; XPos: 184; YPos: 16; ZPos: 0; FontSize: 9; slText: 'R'),
    (ModuleID: 140; ID: 31; XPos: 226; YPos: 19; ZPos: 0; FontSize: 9;
    slText: 'L'), (ModuleID: 140; ID: 32; XPos: 243; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'R'), (ModuleID: 141; ID: 5; XPos: 212; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 141; ID: 7; XPos: 97;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 141; ID: 10;
    XPos: 154; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Value'), (ModuleID: 141;
    ID: 12; XPos: 24; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Send'),
    (ModuleID: 142; ID: 4; XPos: 212; YPos: 2; ZPos: 0; FontSize: 9;
    slText: 'Chan'), (ModuleID: 142; ID: 8; XPos: 139; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Program'), (ModuleID: 142; ID: 14; XPos: 23; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Send'), (ModuleID: 143; ID: 4; XPos: 212;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 143; ID: 8;
    XPos: 154; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Note'), (ModuleID: 143;
    ID: 7; XPos: 84; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Vel'),
    (ModuleID: 143; ID: 15; XPos: 24; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Gate'), (ModuleID: 144; ID: 3; XPos: 21; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Clk'), (ModuleID: 144; ID: 5; XPos: 4; YPos: 28;
    ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 144; ID: 9; XPos: 218;
    YPos: 6; ZPos: 0; FontSize: 9; slText: 'Park'), (ModuleID: 144; ID: 7;
    XPos: 235; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Link'), (ModuleID: 144;
    ID: 64; XPos: 38; YPos: 26; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 144; ID: 65; XPos: 49; YPos: 26; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 144; ID: 66; XPos: 61; YPos: 26; ZPos: 0;
    FontSize: 9; slText: '3'), (ModuleID: 144; ID: 67; XPos: 73; YPos: 26;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 144; ID: 68; XPos: 85;
    YPos: 26; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 144; ID: 69;
    XPos: 97; YPos: 26; ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 144;
    ID: 70; XPos: 109; YPos: 26; ZPos: 0; FontSize: 9; slText: '7'),
    (ModuleID: 144; ID: 71; XPos: 121; YPos: 26; ZPos: 0; FontSize: 9;
    slText: '8'), (ModuleID: 144; ID: 72; XPos: 133; YPos: 26; ZPos: 0;
    FontSize: 9; slText: '9'), (ModuleID: 144; ID: 73; XPos: 144; YPos: 26;
    ZPos: 0; FontSize: 9; slText: '10'), (ModuleID: 144; ID: 74; XPos: 157;
    YPos: 26; ZPos: 0; FontSize: 9; slText: '11'), (ModuleID: 144; ID: 75;
    XPos: 168; YPos: 26; ZPos: 0; FontSize: 9; slText: '12'), (ModuleID: 144;
    ID: 76; XPos: 180; YPos: 26; ZPos: 0; FontSize: 9; slText: '13'),
    (ModuleID: 144; ID: 77; XPos: 192; YPos: 26; ZPos: 0; FontSize: 9;
    slText: '14'), (ModuleID: 144; ID: 78; XPos: 204; YPos: 26; ZPos: 0;
    FontSize: 9; slText: '15'), (ModuleID: 144; ID: 79; XPos: 216; YPos: 26;
    ZPos: 0; FontSize: 9; slText: '16'), (ModuleID: 144; ID: 82; XPos: 115;
    YPos: 6; ZPos: 0; FontSize: 9; slText: 'Length'), (ModuleID: 144; ID: 87;
    XPos: 5; YPos: 50; ZPos: 0; FontSize: 9; slText: 'Lp'), (ModuleID: 145;
    ID: 2; XPos: 4; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Clk'),
    (ModuleID: 145; ID: 4; XPos: 4; YPos: 38; ZPos: 0; FontSize: 9;
    slText: 'Rst'), (ModuleID: 145; ID: 7; XPos: 218; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Park'), (ModuleID: 145; ID: 9; XPos: 234; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Link'), (ModuleID: 145; ID: 65; XPos: 33;
    YPos: 25; ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 145; ID: 64;
    XPos: 44; YPos: 25; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 145;
    ID: 67; XPos: 56; YPos: 25; ZPos: 0; FontSize: 9; slText: '3'),
    (ModuleID: 145; ID: 66; XPos: 68; YPos: 25; ZPos: 0; FontSize: 9;
    slText: '4'), (ModuleID: 145; ID: 69; XPos: 80; YPos: 25; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 145; ID: 68; XPos: 92; YPos: 25;
    ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 145; ID: 71; XPos: 104;
    YPos: 25; ZPos: 0; FontSize: 9; slText: '7'), (ModuleID: 145; ID: 70;
    XPos: 116; YPos: 25; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 145;
    ID: 73; XPos: 128; YPos: 25; ZPos: 0; FontSize: 9; slText: '9'),
    (ModuleID: 145; ID: 72; XPos: 139; YPos: 25; ZPos: 0; FontSize: 9;
    slText: '10'), (ModuleID: 145; ID: 75; XPos: 152; YPos: 25; ZPos: 0;
    FontSize: 9; slText: '11'), (ModuleID: 145; ID: 74; XPos: 163; YPos: 25;
    ZPos: 0; FontSize: 9; slText: '12'), (ModuleID: 145; ID: 77; XPos: 175;
    YPos: 25; ZPos: 0; FontSize: 9; slText: '13'), (ModuleID: 145; ID: 76;
    XPos: 187; YPos: 25; ZPos: 0; FontSize: 9; slText: '14'), (ModuleID: 145;
    ID: 79; XPos: 199; YPos: 25; ZPos: 0; FontSize: 9; slText: '15'),
    (ModuleID: 145; ID: 78; XPos: 211; YPos: 25; ZPos: 0; FontSize: 9;
    slText: '16'), (ModuleID: 145; ID: 83; XPos: 114; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Length'), (ModuleID: 145; ID: 119; XPos: 4; YPos: 60;
    ZPos: 0; FontSize: 9; slText: 'Loop'), (ModuleID: 146; ID: 2; XPos: 4;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 146; ID: 4;
    XPos: 4; YPos: 38; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 146;
    ID: 7; XPos: 218; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Park'),
    (ModuleID: 146; ID: 9; XPos: 234; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Link'), (ModuleID: 146; ID: 65; XPos: 32; YPos: 23; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 146; ID: 64; XPos: 43; YPos: 23;
    ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 146; ID: 67; XPos: 55;
    YPos: 23; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 146; ID: 66;
    XPos: 67; YPos: 23; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 146;
    ID: 69; XPos: 79; YPos: 23; ZPos: 0; FontSize: 9; slText: '5'),
    (ModuleID: 146; ID: 68; XPos: 91; YPos: 23; ZPos: 0; FontSize: 9;
    slText: '6'), (ModuleID: 146; ID: 71; XPos: 103; YPos: 23; ZPos: 0;
    FontSize: 9; slText: '7'), (ModuleID: 146; ID: 70; XPos: 115; YPos: 23;
    ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 146; ID: 73; XPos: 127;
    YPos: 23; ZPos: 0; FontSize: 9; slText: '9'), (ModuleID: 146; ID: 72;
    XPos: 138; YPos: 23; ZPos: 0; FontSize: 9; slText: '10'), (ModuleID: 146;
    ID: 75; XPos: 151; YPos: 23; ZPos: 0; FontSize: 9; slText: '11'),
    (ModuleID: 146; ID: 74; XPos: 162; YPos: 23; ZPos: 0; FontSize: 9;
    slText: '12'), (ModuleID: 146; ID: 77; XPos: 174; YPos: 23; ZPos: 0;
    FontSize: 9; slText: '13'), (ModuleID: 146; ID: 76; XPos: 186; YPos: 23;
    ZPos: 0; FontSize: 9; slText: '14'), (ModuleID: 146; ID: 79; XPos: 198;
    YPos: 23; ZPos: 0; FontSize: 9; slText: '15'), (ModuleID: 146; ID: 78;
    XPos: 210; YPos: 23; ZPos: 0; FontSize: 9; slText: '16'), (ModuleID: 146;
    ID: 83; XPos: 115; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Length'),
    (ModuleID: 146; ID: 11; XPos: 4; YPos: 60; ZPos: 0; FontSize: 9;
    slText: 'Loop'), (ModuleID: 147; ID: 4; XPos: 143; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Chan'), (ModuleID: 147; ID: 6; XPos: 97; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 147; ID: 3; XPos: 239;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Val'), (ModuleID: 147; ID: 9;
    XPos: 207; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Rcv'), (ModuleID: 148;
    ID: 2; XPos: 82; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Note'),
    (ModuleID: 148; ID: 4; XPos: 209; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'Vel'), (ModuleID: 148; ID: 6; XPos: 185; YPos: 3; ZPos: 0;
    FontSize: 9; slText: 'Gate'), (ModuleID: 148; ID: 8; XPos: 143; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 148; ID: 12; XPos: 225;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'RelVel'), (ModuleID: 149; ID: 1;
    XPos: 27; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 149;
    ID: 4; XPos: 72; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Min'),
    (ModuleID: 149; ID: 7; XPos: 110; YPos: 18; ZPos: 0; FontSize: 9;
    slText: 'Max'), (ModuleID: 149; ID: 10; XPos: 4; YPos: 23; ZPos: 0;
    FontSize: 9; slText: 'Rcv'), (ModuleID: 149; ID: 12; XPos: 214; YPos: 18;
    ZPos: 0; FontSize: 9; slText: 'Chan'), (ModuleID: 149; ID: 14; XPos: 147;
    YPos: 23; ZPos: 0; FontSize: 9; slText: 'Send'), (ModuleID: 149; ID: 16;
    XPos: 169; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Trans'), (ModuleID: 149;
    ID: 9; XPos: 133; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Let thru:'),
    (ModuleID: 150; ID: 9; XPos: 5; YPos: 23; ZPos: 0; FontSize: 9;
    slText: 'Thresh'), (ModuleID: 150; ID: 10; XPos: 43; YPos: 23; ZPos: 0;
    FontSize: 9; slText: 'Ratio'), (ModuleID: 150; ID: 11; XPos: 75; YPos: 23;
    ZPos: 0; FontSize: 9; slText: 'Attack'), (ModuleID: 150; ID: 12; XPos: 109;
    YPos: 23; ZPos: 0; FontSize: 9; slText: 'Release'), (ModuleID: 150; ID: 13;
    XPos: 146; YPos: 23; ZPos: 0; FontSize: 9; slText: 'Ref Lvl'),
    (ModuleID: 150; ID: 18; XPos: 227; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'L'), (ModuleID: 150; ID: 19; XPos: 243; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'R'), (ModuleID: 150; ID: 37; XPos: 182; YPos: 62;
    ZPos: 0; FontSize: 9; slText: '30'), (ModuleID: 150; ID: 36; XPos: 182;
    YPos: 52; ZPos: 0; FontSize: 9; slText: '24'), (ModuleID: 150; ID: 39;
    XPos: 184; YPos: 42; ZPos: 0; FontSize: 9; slText: '15'), (ModuleID: 150;
    ID: 41; XPos: 187; YPos: 31; ZPos: 0; FontSize: 9; slText: '9'),
    (ModuleID: 150; ID: 43; XPos: 187; YPos: 21; ZPos: 0; FontSize: 9;
    slText: '4'), (ModuleID: 150; ID: 45; XPos: 189; YPos: 11; ZPos: 0;
    FontSize: 9; slText: '1'), (ModuleID: 150; ID: 22; XPos: 167; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Gain Reduction'), (ModuleID: 152; ID: 18;
    XPos: 82; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 152;
    ID: 15; XPos: 4; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Capture'),
    (ModuleID: 154; ID: 65; XPos: 32; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 154; ID: 64; XPos: 43; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 154; ID: 67; XPos: 55; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 154; ID: 66; XPos: 67;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 154; ID: 69;
    XPos: 79; YPos: 22; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 154;
    ID: 68; XPos: 91; YPos: 22; ZPos: 0; FontSize: 9; slText: '6'),
    (ModuleID: 154; ID: 71; XPos: 103; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '7'), (ModuleID: 154; ID: 70; XPos: 115; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '8'), (ModuleID: 154; ID: 73; XPos: 127; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '9'), (ModuleID: 154; ID: 72; XPos: 138;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '10'), (ModuleID: 154; ID: 75;
    XPos: 151; YPos: 22; ZPos: 0; FontSize: 9; slText: '11'), (ModuleID: 154;
    ID: 74; XPos: 162; YPos: 22; ZPos: 0; FontSize: 9; slText: '12'),
    (ModuleID: 154; ID: 77; XPos: 174; YPos: 22; ZPos: 0; FontSize: 9;
    slText: '13'), (ModuleID: 154; ID: 76; XPos: 186; YPos: 22; ZPos: 0;
    FontSize: 9; slText: '14'), (ModuleID: 154; ID: 79; XPos: 198; YPos: 22;
    ZPos: 0; FontSize: 9; slText: '15'), (ModuleID: 154; ID: 78; XPos: 210;
    YPos: 22; ZPos: 0; FontSize: 9; slText: '16'), (ModuleID: 154; ID: 0;
    XPos: 4; YPos: 15; ZPos: 0; FontSize: 9; slText: 'Ctr'), (ModuleID: 154;
    ID: 3; XPos: 219; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Park'),
    (ModuleID: 154; ID: 5; XPos: 112; YPos: 7; ZPos: 0; FontSize: 9;
    slText: 'XFade'), (ModuleID: 156; ID: 7; XPos: 90; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Note'), (ModuleID: 156; ID: 3; XPos: 201; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Vel'), (ModuleID: 156; ID: 5; XPos: 167;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 156; ID: 9;
    XPos: 225; YPos: 3; ZPos: 0; FontSize: 9; slText: 'RelVel'), (ModuleID: 157;
    ID: 3; XPos: 105; YPos: 2; ZPos: 0; FontSize: 9; slText: 'In signal type'),
    (ModuleID: 158; ID: 9; XPos: 116; YPos: 2; ZPos: 0; FontSize: 9;
    slText: 'Time'), (ModuleID: 158; ID: 12; XPos: 181; YPos: 4; ZPos: 0;
    FontSize: 9; slText: 'Shape'), (ModuleID: 158; ID: 0; XPos: 80; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Glide'), (ModuleID: 158; ID: 2; XPos: 17;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Glide on'), (ModuleID: 159; ID: 3;
    XPos: 110; YPos: 13; ZPos: 0; FontSize: 9; slText: 'A'), (ModuleID: 159;
    ID: 4; XPos: 142; YPos: 13; ZPos: 0; FontSize: 9; slText: 'B'),
    (ModuleID: 159; ID: 5; XPos: 214; YPos: 13; ZPos: 0; FontSize: 9;
    slText: 'A>=B'), (ModuleID: 160; ID: 2; XPos: 212; YPos: 3; ZPos: 0;
    FontSize: 9; slText: 'In'), (ModuleID: 160; ID: 3; XPos: 230; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 161; ID: 33; XPos: 237;
    YPos: 92; ZPos: 0; FontSize: 9; slText: 'Out'), (ModuleID: 161; ID: 37;
    XPos: 209; YPos: 24; ZPos: 0; FontSize: 9; slText: '8'), (ModuleID: 161;
    ID: 38; XPos: 180; YPos: 24; ZPos: 0; FontSize: 9; slText: '7'),
    (ModuleID: 161; ID: 39; XPos: 151; YPos: 24; ZPos: 0; FontSize: 9;
    slText: '6'), (ModuleID: 161; ID: 40; XPos: 122; YPos: 24; ZPos: 0;
    FontSize: 9; slText: '5'), (ModuleID: 161; ID: 41; XPos: 93; YPos: 24;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 161; ID: 42; XPos: 64;
    YPos: 24; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 161; ID: 43;
    XPos: 35; YPos: 24; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 161;
    ID: 44; XPos: 7; YPos: 24; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 161; ID: 46; XPos: 216; YPos: 5; ZPos: 0; FontSize: 9;
    slText: 'Chain'), (ModuleID: 161; ID: 34; XPos: 159; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Pad'), (ModuleID: 162; ID: 8; XPos: 82; YPos: 8;
    ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 162; ID: 10; XPos: 114;
    YPos: 5; ZPos: 0; FontSize: 9; slText: 'FB'), (ModuleID: 162; ID: 16;
    XPos: 17; YPos: 28; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 162;
    ID: 18; XPos: 47; YPos: 34; ZPos: 0; FontSize: 9; slText: 'KBT'),
    (ModuleID: 162; ID: 19; XPos: 153; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Type'), (ModuleID: 162; ID: 24; XPos: 185; YPos: 42; ZPos: 0;
    FontSize: 9; slText: 'Lev'), (ModuleID: 162; ID: 25; XPos: 122; YPos: 35;
    ZPos: 0; FontSize: 9; slText: 'FB'), (ModuleID: 163; ID: 22; XPos: 17;
    YPos: 43; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 163; ID: 24;
    XPos: 22; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Sync'), (ModuleID: 163;
    ID: 25; XPos: 159; YPos: 21; ZPos: 0; FontSize: 9; slText: 'Shape'),
    (ModuleID: 163; ID: 23; XPos: 79; YPos: 36; ZPos: 0; FontSize: 9;
    slText: 'Cent'), (ModuleID: 163; ID: 11; XPos: 104; YPos: 19; ZPos: 0;
    FontSize: 9; slText: 'KBT'), (ModuleID: 164; ID: 23; XPos: 79; YPos: 36;
    ZPos: 0; FontSize: 9; slText: 'Cent'), (ModuleID: 164; ID: 22; XPos: 17;
    YPos: 43; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 164; ID: 24;
    XPos: 22; YPos: 19; ZPos: 0; FontSize: 9; slText: 'Sync'), (ModuleID: 164;
    ID: 25; XPos: 163; YPos: 4; ZPos: 0; FontSize: 9; slText: 'PW'),
    (ModuleID: 164; ID: 27; XPos: 222; YPos: 4; ZPos: 0; FontSize: 9;
    slText: 'Sub'), (ModuleID: 164; ID: 13; XPos: 158; YPos: 41; ZPos: 0;
    FontSize: 9; slText: 'Phase'), (ModuleID: 164; ID: 36; XPos: 187; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Lvl'), (ModuleID: 164; ID: 37; XPos: 187;
    YPos: 41; ZPos: 0; FontSize: 9; slText: 'Lvl'), (ModuleID: 164; ID: 11;
    XPos: 103; YPos: 33; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 164;
    ID: 31; XPos: 227; YPos: 36; ZPos: 0; FontSize: 9; slText: 'Soft'),
    (ModuleID: 165; ID: 23; XPos: 57; YPos: 28; ZPos: 0; FontSize: 9;
    slText: 'Algorithm'), (ModuleID: 165; ID: 24; XPos: 6; YPos: 28; ZPos: 0;
    FontSize: 9; slText: 'Feedback'), (ModuleID: 165; ID: 50; XPos: 4; YPos: 77;
    ZPos: 0; FontSize: 9; slText: '1'), (ModuleID: 165; ID: 51; XPos: 39;
    YPos: 77; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 165; ID: 52;
    XPos: 76; YPos: 77; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 165;
    ID: 53; XPos: 112; YPos: 77; ZPos: 0; FontSize: 9; slText: '4'),
    (ModuleID: 165; ID: 54; XPos: 150; YPos: 77; ZPos: 0; FontSize: 9;
    slText: '5'), (ModuleID: 165; ID: 55; XPos: 187; YPos: 77; ZPos: 0;
    FontSize: 9; slText: '6'), (ModuleID: 165; ID: 15; XPos: 220; YPos: 77;
    ZPos: 0; FontSize: 9; slText: 'Main'), (ModuleID: 167; ID: 3; XPos: 138;
    YPos: 4; ZPos: 0; FontSize: 9; slText: 'Semi'), (ModuleID: 167; ID: 4;
    XPos: 168; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Fine'), (ModuleID: 167;
    ID: 10; XPos: 202; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Delay'),
    (ModuleID: 167; ID: 12; XPos: 4; YPos: 19; ZPos: 0; FontSize: 9;
    slText: 'Pitch'), (ModuleID: 167; ID: 9; XPos: 83; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Pitch Shift'), (ModuleID: 169; ID: 0; XPos: 23;
    YPos: 34; ZPos: 0; FontSize: 9; slText: 'Trig'), (ModuleID: 169; ID: 8;
    XPos: 18; YPos: 62; ZPos: 0; FontSize: 9; slText: 'AM'), (ModuleID: 169;
    ID: 21; XPos: 51; YPos: 32; ZPos: 0; FontSize: 9; slText: 'A'),
    (ModuleID: 169; ID: 22; XPos: 138; YPos: 32; ZPos: 0; FontSize: 9;
    slText: 'D'), (ModuleID: 169; ID: 25; XPos: 94; YPos: 32; ZPos: 0;
    FontSize: 9; slText: 'H'), (ModuleID: 169; ID: 30; XPos: 220; YPos: 50;
    ZPos: 0; FontSize: 9; slText: 'Env'), (ModuleID: 170; ID: 5; XPos: 92;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'In'), (ModuleID: 170; ID: 7;
    XPos: 121; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Bus'), (ModuleID: 170;
    ID: 8; XPos: 217; YPos: 3; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 170; ID: 9; XPos: 243; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'R'), (ModuleID: 170; ID: 6; XPos: 5; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Pad'), (ModuleID: 171; ID: 9; XPos: 243; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 171; ID: 10; XPos: 217;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 171; ID: 11;
    XPos: 192; YPos: 3; ZPos: 0; FontSize: 9; slText: '2'), (ModuleID: 171;
    ID: 12; XPos: 166; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 171; ID: 14; XPos: 5; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'Pad'), (ModuleID: 172; ID: 1; XPos: 12; YPos: 15; ZPos: 0;
    FontSize: 9; slText: 'Range'), (ModuleID: 172; ID: 2; XPos: 148; YPos: 2;
    ZPos: 0; FontSize: 9; slText: 'Time'), (ModuleID: 173; ID: 0; XPos: 12;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 173; ID: 3;
    XPos: 148; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Time'), (ModuleID: 174;
    ID: 1; XPos: 5; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Range'),
    (ModuleID: 174; ID: 2; XPos: 232; YPos: 4; ZPos: 0; FontSize: 9;
    slText: 'In'), (ModuleID: 175; ID: 29; XPos: 5; YPos: 47; ZPos: 0;
    FontSize: 9; slText: 'Range'), (ModuleID: 175; ID: 31; XPos: 231; YPos: 5;
    ZPos: 0; FontSize: 9; slText: 'In'), (ModuleID: 176; ID: 1; XPos: 5;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Range'), (ModuleID: 176; ID: 10;
    XPos: 131; YPos: 10; ZPos: 0; FontSize: 9; slText: 'FB'), (ModuleID: 176;
    ID: 11; XPos: 163; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Filter'),
    (ModuleID: 176; ID: 12; XPos: 194; YPos: 10; ZPos: 0; FontSize: 9;
    slText: 'Dry/Wet'), (ModuleID: 177; ID: 0; XPos: 5; YPos: 18; ZPos: 0;
    FontSize: 9; slText: 'Range'), (ModuleID: 177; ID: 12; XPos: 194; YPos: 4;
    ZPos: 0; FontSize: 9; slText: 'Dry/Wet'), (ModuleID: 177; ID: 13; XPos: 131;
    YPos: 4; ZPos: 0; FontSize: 9; slText: 'FB'), (ModuleID: 177; ID: 18;
    XPos: 163; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Filter'), (ModuleID: 177;
    ID: 27; XPos: 158; YPos: 14; ZPos: 0; FontSize: 9; slText: 'HP'),
    (ModuleID: 177; ID: 28; XPos: 158; YPos: 39; ZPos: 0; FontSize: 9;
    slText: 'LP'), (ModuleID: 178; ID: 1; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Clk'), (ModuleID: 178; ID: 2; XPos: 106; YPos: 6;
    ZPos: 0; FontSize: 9; slText: 'Sample'), (ModuleID: 178; ID: 3; XPos: 109;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'Delay'), (ModuleID: 179; ID: 2;
    XPos: 18; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 179;
    ID: 12; XPos: 103; YPos: 3; ZPos: 0; FontSize: 9; slText: '1'),
    (ModuleID: 179; ID: 13; XPos: 123; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '2'), (ModuleID: 179; ID: 14; XPos: 143; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '3'), (ModuleID: 179; ID: 15; XPos: 162; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 179; ID: 16; XPos: 183;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '5'), (ModuleID: 179; ID: 17;
    XPos: 203; YPos: 3; ZPos: 0; FontSize: 9; slText: '6'), (ModuleID: 179;
    ID: 18; XPos: 223; YPos: 3; ZPos: 0; FontSize: 9; slText: '7'),
    (ModuleID: 179; ID: 19; XPos: 243; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '8'), (ModuleID: 180; ID: 16; XPos: 69; YPos: 74; ZPos: 0;
    FontSize: 9; slText: 'L1'), (ModuleID: 180; ID: 17; XPos: 44; YPos: 74;
    ZPos: 0; FontSize: 9; slText: 'R1'), (ModuleID: 180; ID: 18; XPos: 98;
    YPos: 74; ZPos: 0; FontSize: 9; slText: 'R2'), (ModuleID: 180; ID: 20;
    XPos: 124; YPos: 74; ZPos: 0; FontSize: 9; slText: 'L2'), (ModuleID: 180;
    ID: 19; XPos: 179; YPos: 74; ZPos: 0; FontSize: 9; slText: 'L3'),
    (ModuleID: 180; ID: 22; XPos: 233; YPos: 74; ZPos: 0; FontSize: 9;
    slText: 'L4'), (ModuleID: 180; ID: 21; XPos: 153; YPos: 74; ZPos: 0;
    FontSize: 9; slText: 'R3'), (ModuleID: 180; ID: 24; XPos: 207; YPos: 74;
    ZPos: 0; FontSize: 9; slText: 'R4'), (ModuleID: 180; ID: 29; XPos: 4;
    YPos: 15; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 180; ID: 36;
    XPos: 142; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Coarse'), (ModuleID: 180;
    ID: 37; XPos: 178; YPos: 6; ZPos: 0; FontSize: 9; slText: 'Fine'),
    (ModuleID: 180; ID: 41; XPos: 237; YPos: 11; ZPos: 0; FontSize: 9;
    slText: 'FM'), (ModuleID: 180; ID: 42; XPos: 202; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Detune'), (ModuleID: 180; ID: 47; XPos: 182; YPos: 43;
    ZPos: 0; FontSize: 9; slText: 'Vel'), (ModuleID: 180; ID: 48; XPos: 208;
    YPos: 43; ZPos: 0; FontSize: 9; slText: 'RateScale'), (ModuleID: 180;
    ID: 52; XPos: 4; YPos: 66; ZPos: 0; FontSize: 9; slText: 'Gate'),
    (ModuleID: 180; ID: 57; XPos: 4; YPos: 115; ZPos: 0; FontSize: 9;
    slText: 'AMod'), (ModuleID: 180; ID: 60; XPos: 205; YPos: 129; ZPos: 0;
    FontSize: 9; slText: 'OutLev'), (ModuleID: 180; ID: 65; XPos: 151;
    YPos: 152; ZPos: 0; FontSize: 9; slText: 'R-Depth'), (ModuleID: 180; ID: 69;
    XPos: 83; YPos: 152; ZPos: 0; FontSize: 9; slText: 'L-Depth'),
    (ModuleID: 180; ID: 73; XPos: 35; YPos: 144; ZPos: 0; FontSize: 9;
    slText: 'BrPoint'), (ModuleID: 180; ID: 76; XPos: 4; YPos: 156; ZPos: 0;
    FontSize: 9; slText: 'Vel'), (ModuleID: 180; ID: 78; XPos: 4; YPos: 91;
    ZPos: 0; FontSize: 9; slText: 'Note'), (ModuleID: 180; ID: 28; XPos: 4;
    YPos: 41; ZPos: 0; FontSize: 9; slText: 'Freq'), (ModuleID: 180; ID: 49;
    XPos: 34; YPos: 15; ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 181;
    ID: 12; XPos: 12; YPos: 22; ZPos: 0; FontSize: 9; slText: 'Range'),
    (ModuleID: 181; ID: 9; XPos: 233; YPos: 5; ZPos: 0; FontSize: 9;
    slText: 'In'), (ModuleID: 181; ID: 14; XPos: 118; YPos: 2; ZPos: 0;
    FontSize: 9; slText: 'Time'), (ModuleID: 182; ID: 0; XPos: 102; YPos: 13;
    ZPos: 0; FontSize: 9; slText: 'FB'), (ModuleID: 182; ID: 4; XPos: 7;
    YPos: 31; ZPos: 0; FontSize: 9; slText: 'Left'), (ModuleID: 182; ID: 6;
    XPos: 125; YPos: 13; ZPos: 0; FontSize: 9; slText: 'X-FB'), (ModuleID: 182;
    ID: 10; XPos: 4; YPos: 56; ZPos: 0; FontSize: 9; slText: 'Right'),
    (ModuleID: 182; ID: 14; XPos: 201; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'Dry/Wet'), (ModuleID: 182; ID: 22; XPos: 165; YPos: 6; ZPos: 0;
    FontSize: 9; slText: 'Filter'), (ModuleID: 182; ID: 23; XPos: 234; YPos: 62;
    ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 182; ID: 24; XPos: 214;
    YPos: 62; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 182; ID: 17;
    XPos: 159; YPos: 15; ZPos: 0; FontSize: 9; slText: 'LP'), (ModuleID: 182;
    ID: 26; XPos: 184; YPos: 15; ZPos: 0; FontSize: 9; slText: 'HP'),
    (ModuleID: 182; ID: 30; XPos: 159; YPos: 47; ZPos: 0; FontSize: 9;
    slText: 'Range'), (ModuleID: 183; ID: 15; XPos: 186; YPos: 4; ZPos: 0;
    FontSize: 9; slText: 'Phase'), (ModuleID: 183; ID: 16; XPos: 131; YPos: 6;
    ZPos: 0; FontSize: 9; slText: 'Cent'), (ModuleID: 183; ID: 17; XPos: 155;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Sync'), (ModuleID: 183; ID: 21;
    XPos: 190; YPos: 13; ZPos: 0; FontSize: 9; slText: 'Mod'), (ModuleID: 183;
    ID: 9; XPos: 17; YPos: 13; ZPos: 0; FontSize: 9; slText: 'Pitch'),
    (ModuleID: 183; ID: 13; XPos: 55; YPos: 31; ZPos: 0; FontSize: 9;
    slText: 'KBT'), (ModuleID: 184; ID: 14; XPos: 18; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Chain'), (ModuleID: 185; ID: 14; XPos: 45; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Chain'), (ModuleID: 185; ID: 3; XPos: 185;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'L'), (ModuleID: 185; ID: 4;
    XPos: 205; YPos: 3; ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 185;
    ID: 31; XPos: 223; YPos: 3; ZPos: 0; FontSize: 9; slText: 'L'),
    (ModuleID: 185; ID: 32; XPos: 243; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'R'), (ModuleID: 185; ID: 5; XPos: 17; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'L'), (ModuleID: 185; ID: 6; XPos: 37; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'R'), (ModuleID: 186; ID: 5; XPos: 38;
    YPos: 16; ZPos: 0; FontSize: 9; slText: 'Ctrl'), (ModuleID: 186; ID: 10;
    XPos: 241; YPos: 3; ZPos: 0; FontSize: 9; slText: 'On'), (ModuleID: 187;
    ID: 4; XPos: 38; YPos: 16; ZPos: 0; FontSize: 9; slText: 'Ctrl'),
    (ModuleID: 187; ID: 9; XPos: 223; YPos: 15; ZPos: 0; FontSize: 9;
    slText: 'On'), (ModuleID: 189; ID: 21; XPos: 12; YPos: 17; ZPos: 0;
    FontSize: 9; slText: 'Attack'), (ModuleID: 189; ID: 4; XPos: 76; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Release'), (ModuleID: 189; ID: 5; XPos: 142;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Threshold'), (ModuleID: 190; ID: 9;
    XPos: 143; YPos: 23; ZPos: 0; FontSize: 9; slText: 'Phase'), (ModuleID: 190;
    ID: 14; XPos: 116; YPos: 35; ZPos: 0; FontSize: 9; slText: 'KBT'),
    (ModuleID: 190; ID: 20; XPos: 22; YPos: 18; ZPos: 0; FontSize: 9;
    slText: 'Rst'), (ModuleID: 190; ID: 24; XPos: 33; YPos: 35; ZPos: 0;
    FontSize: 9; slText: 'Rate'), (ModuleID: 190; ID: 26; XPos: 4; YPos: 36;
    ZPos: 0; FontSize: 9; slText: 'Snc'), (ModuleID: 192; ID: 2; XPos: 93;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Rate'), (ModuleID: 192; ID: 4;
    XPos: 159; YPos: 7; ZPos: 0; FontSize: 9; slText: 'Feed'), (ModuleID: 192;
    ID: 10; XPos: 159; YPos: 15; ZPos: 0; FontSize: 9; slText: 'back'),
    (ModuleID: 193; ID: 2; XPos: 80; YPos: 3; ZPos: 0; FontSize: 9;
    slText: '1'), (ModuleID: 193; ID: 3; XPos: 118; YPos: 3; ZPos: 0;
    FontSize: 9; slText: '2'), (ModuleID: 193; ID: 5; XPos: 155; YPos: 3;
    ZPos: 0; FontSize: 9; slText: '3'), (ModuleID: 193; ID: 7; XPos: 193;
    YPos: 3; ZPos: 0; FontSize: 9; slText: '4'), (ModuleID: 194; ID: 1;
    XPos: 18; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Chain'), (ModuleID: 195;
    ID: 6; XPos: 79; YPos: 7; ZPos: 0; FontSize: 9; slText: 'Mod'),
    (ModuleID: 195; ID: 10; XPos: 74; YPos: 16; ZPos: 0; FontSize: 9;
    slText: 'depth'), (ModuleID: 195; ID: 11; XPos: 170; YPos: 14; ZPos: 0;
    FontSize: 9; slText: 'Mod'), (ModuleID: 196; ID: 4; XPos: 175; YPos: 10;
    ZPos: 0; FontSize: 9; slText: 'Dcy'), (ModuleID: 196; ID: 7; XPos: 45;
    YPos: 20; ZPos: 0; FontSize: 9; slText: 'Trig'), (ModuleID: 196; ID: 18;
    XPos: 17; YPos: 13; ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 196;
    ID: 19; XPos: 197; YPos: 10; ZPos: 0; FontSize: 9; slText: 'Click'),
    (ModuleID: 196; ID: 20; XPos: 149; YPos: 6; ZPos: 0; FontSize: 9;
    slText: 'Cent'), (ModuleID: 196; ID: 21; XPos: 225; YPos: 4; ZPos: 0;
    FontSize: 9; slText: 'Punch'), (ModuleID: 196; ID: 9; XPos: 75; YPos: 31;
    ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 197; ID: 1; XPos: 148;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Var. Active'), (ModuleID: 197;
    ID: 2; XPos: 83; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Patch Active'),
    (ModuleID: 197; ID: 4; XPos: 212; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'Voice No.'), (ModuleID: 198; ID: 4; XPos: 17; YPos: 16; ZPos: 0;
    FontSize: 9; slText: 'Input'), (ModuleID: 198; ID: 5; XPos: 230; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Pitch'), (ModuleID: 198; ID: 7; XPos: 165;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Period'), (ModuleID: 198; ID: 11;
    XPos: 200; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Gate'), (ModuleID: 198;
    ID: 10; XPos: 88; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Threshold'),
    (ModuleID: 199; ID: 0; XPos: 206; YPos: 3; ZPos: 0; FontSize: 9;
    slText: 'Gate'), (ModuleID: 199; ID: 4; XPos: 175; YPos: 3; ZPos: 0;
    FontSize: 9; slText: 'Pitch'), (ModuleID: 199; ID: 5; XPos: 239; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'Vel'), (ModuleID: 200; ID: 0; XPos: 18;
    YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rate'), (ModuleID: 200; ID: 6;
    XPos: 197; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Edge'), (ModuleID: 200;
    ID: 1; XPos: 77; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Rate'),
    (ModuleID: 200; ID: 13; XPos: 170; YPos: 4; ZPos: 0; FontSize: 9;
    slText: 'Step'), (ModuleID: 202; ID: 11; XPos: 17; YPos: 13; ZPos: 0;
    FontSize: 9; slText: 'Rate'), (ModuleID: 202; ID: 12; XPos: 154; YPos: 10;
    ZPos: 0; FontSize: 9; slText: 'KBT'), (ModuleID: 202; ID: 7; XPos: 181;
    YPos: 10; ZPos: 0; FontSize: 9; slText: 'Step'), (ModuleID: 202; ID: 18;
    XPos: 213; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Edge'), (ModuleID: 203;
    ID: 3; XPos: 5; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'For Clavia indoor debug purposes only'), (ModuleID: 204; ID: 0;
    XPos: 22; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 204;
    ID: 4; XPos: 170; YPos: 2; ZPos: 0; FontSize: 9; slText: 'Step'),
    (ModuleID: 204; ID: 13; XPos: 86; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Rst'), (ModuleID: 204; ID: 15; XPos: 98; YPos: 4; ZPos: 0;
    FontSize: 9; slText: 'Seed'), (ModuleID: 205; ID: 2; XPos: 22; YPos: 17;
    ZPos: 0; FontSize: 9; slText: 'Clk'), (ModuleID: 205; ID: 7; XPos: 161;
    YPos: 2; ZPos: 0; FontSize: 9; slText: 'Prob'), (ModuleID: 205; ID: 12;
    XPos: 86; YPos: 17; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 205;
    ID: 14; XPos: 98; YPos: 4; ZPos: 0; FontSize: 9; slText: 'Seed'),
    (ModuleID: 206; ID: 2; XPos: 23; YPos: 17; ZPos: 0; FontSize: 9;
    slText: 'Clk'), (ModuleID: 206; ID: 7; XPos: 165; YPos: 7; ZPos: 0;
    FontSize: 9; slText: 'Step'), (ModuleID: 206; ID: 11; XPos: 77; YPos: 16;
    ZPos: 0; FontSize: 9; slText: 'Character'), (ModuleID: 206; ID: 12;
    XPos: 23; YPos: 31; ZPos: 0; FontSize: 9; slText: 'Rst'), (ModuleID: 206;
    ID: 14; XPos: 48; YPos: 18; ZPos: 0; FontSize: 9; slText: 'Seed'),
    (ModuleID: 208; ID: 2; XPos: 22; YPos: 18; ZPos: 0; FontSize: 9;
    slText: 'Clk'), (ModuleID: 208; ID: 5; XPos: 22; YPos: 32; ZPos: 0;
    FontSize: 9; slText: 'Rst'), (ModuleID: 208; ID: 4; XPos: 80; YPos: 3;
    ZPos: 0; FontSize: 9; slText: 'pattern'), (ModuleID: 208; ID: 6; XPos: 121;
    YPos: 3; ZPos: 0; FontSize: 9; slText: 'Step/'), (ModuleID: 208; ID: 13;
    XPos: 183; YPos: 3; ZPos: 0; FontSize: 9; slText: 'Loop'), (ModuleID: 208;
    ID: 18; XPos: 43; YPos: 17; ZPos: 0; FontSize: 9; slText: 'A'),
    (ModuleID: 208; ID: 21; XPos: 44; YPos: 32; ZPos: 0; FontSize: 9;
    slText: 'B'), (ModuleID: 208; ID: 22; XPos: 121; YPos: 12; ZPos: 0;
    FontSize: 9; slText: 'Prob'));

  LedDefs: array [0 .. 173] of TG2LedDef = ((ModuleID: 5; ID: 0; XPos: 137;
    YPos: 11; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 5; ID: 3; XPos: 230; YPos: 11; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 1), (ModuleID: 17; ID: 14; XPos: 146; YPos: 17;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 20;
    ID: 11; XPos: 6; YPos: 17; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 21; ID: 36; XPos: 102; YPos: 3; CodeRef: 0;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 21; ID: 37;
    XPos: 122; YPos: 3; CodeRef: 1; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 21; ID: 38; XPos: 142; YPos: 3; CodeRef: 2; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 21; ID: 39; XPos: 162; YPos: 3;
    CodeRef: 3; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 21;
    ID: 40; XPos: 182; YPos: 3; CodeRef: 4; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 21; ID: 41; XPos: 202; YPos: 3; CodeRef: 5;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 21; ID: 42;
    XPos: 222; YPos: 3; CodeRef: 6; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 21; ID: 43; XPos: 242; YPos: 3; CodeRef: 7; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 23; ID: 19; XPos: 6; YPos: 33;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 24;
    ID: 5; XPos: 226; YPos: 4; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 25; ID: 23; XPos: 242; YPos: 51; CodeRef: 0;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 26; ID: 15;
    XPos: 242; YPos: 21; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 38; ID: 9; XPos: 242; YPos: 4; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 41; ID: 3; XPos: 40; YPos: 18;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 42;
    ID: 8; XPos: 242; YPos: 4; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 46; ID: 1; XPos: 6; YPos: 18; CodeRef: 0;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 52; ID: 7; XPos: 6;
    YPos: 18; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 55; ID: 2; XPos: 40; YPos: 18; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 58; ID: 21; XPos: 6; YPos: 39;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 60;
    ID: 16; XPos: 34; YPos: 21; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 60; ID: 19; XPos: 54; YPos: 21; CodeRef: 1;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 60; ID: 18; XPos: 74;
    YPos: 21; CodeRef: 2; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 60; ID: 21; XPos: 94; YPos: 21; CodeRef: 3; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 60; ID: 20; XPos: 114; YPos: 21;
    CodeRef: 4; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 60;
    ID: 23; XPos: 134; YPos: 21; CodeRef: 5; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 60; ID: 22; XPos: 154; YPos: 21; CodeRef: 6;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 60; ID: 25;
    XPos: 174; YPos: 21; CodeRef: 7; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 64; ID: 0; XPos: 230; YPos: 11; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 1), (ModuleID: 64; ID: 7; XPos: 137; YPos: 11;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 84;
    ID: 9; XPos: 6; YPos: 18; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 85; ID: 14; XPos: 198; YPos: 15; CodeRef: 0;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 86; ID: 25; XPos: 81;
    YPos: 3; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 86; ID: 26; XPos: 104; YPos: 3; CodeRef: 1; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 86; ID: 27; XPos: 127; YPos: 3;
    CodeRef: 2; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 86;
    ID: 28; XPos: 150; YPos: 3; CodeRef: 3; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 86; ID: 29; XPos: 173; YPos: 3; CodeRef: 4;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 86; ID: 30;
    XPos: 196; YPos: 3; CodeRef: 5; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 86; ID: 31; XPos: 219; YPos: 3; CodeRef: 6; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 86; ID: 32; XPos: 242; YPos: 3;
    CodeRef: 7; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 91;
    ID: 6; XPos: 204; YPos: 17; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 91; ID: 9; XPos: 230; YPos: 7; CodeRef: 1;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 105; ID: 15;
    XPos: 146; YPos: 17; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 119; ID: 2; XPos: 6; YPos: 19; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 121; ID: 6; XPos: 242; YPos: 7;
    CodeRef: 16; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 121;
    ID: 42; XPos: 25; YPos: 31; CodeRef: 0; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 45; XPos: 37; YPos: 31; CodeRef: 1;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 44;
    XPos: 49; YPos: 31; CodeRef: 2; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 47; XPos: 61; YPos: 31; CodeRef: 3;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 46;
    XPos: 169; YPos: 31; CodeRef: 12; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 49; XPos: 181; YPos: 31; CodeRef: 13;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 48;
    XPos: 193; YPos: 31; CodeRef: 14; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 51; XPos: 205; YPos: 31; CodeRef: 15;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 50;
    XPos: 121; YPos: 31; CodeRef: 8; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 53; XPos: 133; YPos: 31; CodeRef: 9;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 52;
    XPos: 145; YPos: 31; CodeRef: 10; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 55; XPos: 157; YPos: 31; CodeRef: 11;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 54;
    XPos: 73; YPos: 31; CodeRef: 4; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 57; XPos: 85; YPos: 31; CodeRef: 5;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 121; ID: 56;
    XPos: 97; YPos: 31; CodeRef: 6; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 121; ID: 59; XPos: 109; YPos: 31; CodeRef: 7;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 124; ID: 17;
    XPos: 82; YPos: 3; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 124; ID: 18; XPos: 102; YPos: 3; CodeRef: 1; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 124; ID: 19; XPos: 122; YPos: 3;
    CodeRef: 2; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 124;
    ID: 20; XPos: 142; YPos: 3; CodeRef: 3; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 124; ID: 21; XPos: 162; YPos: 3; CodeRef: 4;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 124; ID: 22;
    XPos: 182; YPos: 3; CodeRef: 5; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 124; ID: 23; XPos: 202; YPos: 3; CodeRef: 6; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 124; ID: 24; XPos: 222; YPos: 3;
    CodeRef: 7; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 130;
    ID: 24; XPos: 81; YPos: 3; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 130; ID: 27; XPos: 104; YPos: 3; CodeRef: 1;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 130; ID: 26;
    XPos: 127; YPos: 3; CodeRef: 2; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 130; ID: 29; XPos: 150; YPos: 3; CodeRef: 3; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 130; ID: 28; XPos: 173; YPos: 3;
    CodeRef: 4; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 130;
    ID: 31; XPos: 196; YPos: 3; CodeRef: 5; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 130; ID: 30; XPos: 219; YPos: 3; CodeRef: 6;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 130; ID: 33;
    XPos: 242; YPos: 3; CodeRef: 7; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 131; ID: 24; XPos: 81; YPos: 3; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 131; ID: 27; XPos: 105; YPos: 3;
    CodeRef: 1; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 131;
    ID: 26; XPos: 127; YPos: 3; CodeRef: 2; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 131; ID: 29; XPos: 150; YPos: 3; CodeRef: 3;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 131; ID: 28;
    XPos: 173; YPos: 3; CodeRef: 4; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 131; ID: 31; XPos: 196; YPos: 3; CodeRef: 5; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 131; ID: 30; XPos: 219; YPos: 3;
    CodeRef: 6; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 131;
    ID: 33; XPos: 242; YPos: 3; CodeRef: 7; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 144; ID: 8; XPos: 242; YPos: 7; CodeRef: 16;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 144; ID: 43;
    XPos: 34; YPos: 36; CodeRef: 0; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 44; XPos: 46; YPos: 36; CodeRef: 1;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 45;
    XPos: 58; YPos: 36; CodeRef: 2; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 46; XPos: 70; YPos: 36; CodeRef: 3;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 47;
    XPos: 178; YPos: 36; CodeRef: 12; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 48; XPos: 190; YPos: 36; CodeRef: 13;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 49;
    XPos: 202; YPos: 36; CodeRef: 14; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 50; XPos: 214; YPos: 36; CodeRef: 15;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 51;
    XPos: 130; YPos: 36; CodeRef: 8; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 52; XPos: 142; YPos: 36; CodeRef: 9;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 53;
    XPos: 154; YPos: 36; CodeRef: 10; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 54; XPos: 166; YPos: 36; CodeRef: 11;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 55;
    XPos: 82; YPos: 36; CodeRef: 4; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 56; XPos: 94; YPos: 36; CodeRef: 5;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 144; ID: 57;
    XPos: 106; YPos: 36; CodeRef: 6; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 144; ID: 58; XPos: 118; YPos: 36; CodeRef: 7;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 6;
    XPos: 242; YPos: 7; CodeRef: 16; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 145; ID: 42; XPos: 29; YPos: 35; CodeRef: 0; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 45; XPos: 41;
    YPos: 35; CodeRef: 1; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 44; XPos: 53; YPos: 35; CodeRef: 2; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 47; XPos: 65;
    YPos: 35; CodeRef: 3; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 46; XPos: 173; YPos: 35; CodeRef: 12; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 49; XPos: 185;
    YPos: 35; CodeRef: 13; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 48; XPos: 197; YPos: 35; CodeRef: 14; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 51; XPos: 209;
    YPos: 35; CodeRef: 15; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 50; XPos: 125; YPos: 35; CodeRef: 8; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 53; XPos: 137;
    YPos: 35; CodeRef: 9; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 52; XPos: 149; YPos: 35; CodeRef: 10; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 55; XPos: 161;
    YPos: 35; CodeRef: 11; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 54; XPos: 77; YPos: 35; CodeRef: 4; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 57; XPos: 89;
    YPos: 35; CodeRef: 5; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 145; ID: 56; XPos: 101; YPos: 35; CodeRef: 6; InfoFunc: 0;
    LedType: ltSequencer; GroupID: 0), (ModuleID: 145; ID: 59; XPos: 113;
    YPos: 35; CodeRef: 7; InfoFunc: 0; LedType: ltSequencer; GroupID: 0),
    (ModuleID: 146; ID: 6; XPos: 242; YPos: 7; CodeRef: 16; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 146; ID: 42; XPos: 28; YPos: 32;
    CodeRef: 0; InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146;
    ID: 45; XPos: 40; YPos: 32; CodeRef: 1; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 44; XPos: 52; YPos: 32; CodeRef: 2;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 47;
    XPos: 64; YPos: 32; CodeRef: 3; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 46; XPos: 172; YPos: 32; CodeRef: 12;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 49;
    XPos: 184; YPos: 32; CodeRef: 13; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 48; XPos: 196; YPos: 32; CodeRef: 14;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 51;
    XPos: 208; YPos: 32; CodeRef: 15; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 50; XPos: 124; YPos: 32; CodeRef: 8;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 53;
    XPos: 136; YPos: 32; CodeRef: 9; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 52; XPos: 148; YPos: 32; CodeRef: 10;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 55;
    XPos: 160; YPos: 32; CodeRef: 11; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 54; XPos: 76; YPos: 32; CodeRef: 4;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 57;
    XPos: 88; YPos: 32; CodeRef: 5; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 146; ID: 56; XPos: 100; YPos: 32; CodeRef: 6;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 146; ID: 59;
    XPos: 112; YPos: 32; CodeRef: 7; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 150; ID: 21; XPos: 195; YPos: 65; CodeRef: 9;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 150; ID: 27;
    XPos: 195; YPos: 59; CodeRef: 8; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 150; ID: 28; XPos: 195; YPos: 29; CodeRef: 3;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 150; ID: 29;
    XPos: 195; YPos: 23; CodeRef: 2; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 150; ID: 30; XPos: 195; YPos: 41; CodeRef: 5;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 150; ID: 31;
    XPos: 195; YPos: 35; CodeRef: 4; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 150; ID: 32; XPos: 195; YPos: 47; CodeRef: 6;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 150; ID: 33;
    XPos: 195; YPos: 53; CodeRef: 7; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 150; ID: 34; XPos: 195; YPos: 17; CodeRef: 1;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 150; ID: 35;
    XPos: 195; YPos: 11; CodeRef: 0; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 42; XPos: 28; YPos: 31; CodeRef: 0;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 45;
    XPos: 40; YPos: 31; CodeRef: 1; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 44; XPos: 52; YPos: 31; CodeRef: 2;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 47;
    XPos: 64; YPos: 31; CodeRef: 3; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 46; XPos: 172; YPos: 31; CodeRef: 12;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 49;
    XPos: 184; YPos: 31; CodeRef: 13; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 48; XPos: 196; YPos: 31; CodeRef: 14;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 51;
    XPos: 208; YPos: 31; CodeRef: 15; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 50; XPos: 124; YPos: 31; CodeRef: 8;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 53;
    XPos: 136; YPos: 31; CodeRef: 9; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 52; XPos: 148; YPos: 31; CodeRef: 10;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 55;
    XPos: 160; YPos: 31; CodeRef: 11; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 54; XPos: 76; YPos: 31; CodeRef: 4;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 57;
    XPos: 88; YPos: 31; CodeRef: 5; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 56; XPos: 100; YPos: 31; CodeRef: 6;
    InfoFunc: 0; LedType: ltSequencer; GroupID: 0), (ModuleID: 154; ID: 59;
    XPos: 112; YPos: 31; CodeRef: 7; InfoFunc: 0; LedType: ltSequencer;
    GroupID: 0), (ModuleID: 154; ID: 2; XPos: 242; YPos: 7; CodeRef: 16;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 156; ID: 2;
    XPos: 186; YPos: 15; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 169; ID: 1; XPos: 6; YPos: 19; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 180; ID: 53; XPos: 17; YPos: 77;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 189;
    ID: 9; XPos: 213; YPos: 32; CodeRef: 0; InfoFunc: 0; LedType: ltGreen;
    GroupID: 0), (ModuleID: 190; ID: 11; XPos: 242; YPos: 36; CodeRef: 0;
    InfoFunc: 0; LedType: ltGreen; GroupID: 0), (ModuleID: 198; ID: 9;
    XPos: 212; YPos: 15; CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0),
    (ModuleID: 200; ID: 5; XPos: 226; YPos: 4; CodeRef: 0; InfoFunc: 0;
    LedType: ltGreen; GroupID: 0), (ModuleID: 202; ID: 14; XPos: 242; YPos: 21;
    CodeRef: 0; InfoFunc: 0; LedType: ltGreen; GroupID: 0));

  MiniVUDefs: array [0 .. 28] of TG2MiniVUDef = ((ModuleID: 3; ID: 12;
    XPos: 153; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 3; ID: 13; XPos: 179; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 1), (ModuleID: 3; ID: 14;
    XPos: 205; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 2), (ModuleID: 3; ID: 15; XPos: 231; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 3), (ModuleID: 4; ID: 9;
    XPos: 205; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 4; ID: 11; XPos: 231; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 1), (ModuleID: 19; ID: 14;
    XPos: 231; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 32; ID: 16; XPos: 227; YPos: 25; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 33; ID: 16;
    XPos: 227; YPos: 40; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 40; ID: 22; XPos: 242; YPos: 27; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 45; ID: 26;
    XPos: 230; YPos: 40; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 48; ID: 2; XPos: 207; YPos: 55; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 48; ID: 7;
    XPos: 231; YPos: 55; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 1), (ModuleID: 102; ID: 31; XPos: 227; YPos: 55; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 103; ID: 16;
    XPos: 227; YPos: 40; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 116; ID: 19; XPos: 231; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 123; ID: 13;
    XPos: 242; YPos: 27; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 127; ID: 4; XPos: 205; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 127; ID: 1;
    XPos: 231; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 1), (ModuleID: 140; ID: 12; XPos: 242; YPos: 27; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 1), (ModuleID: 140; ID: 29;
    XPos: 225; YPos: 27; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 161; ID: 45; XPos: 242; YPos: 102; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 0), (ModuleID: 162; ID: 12;
    XPos: 227; YPos: 40; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 170; ID: 2; XPos: 231; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 1), (ModuleID: 170; ID: 3;
    XPos: 205; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0), (ModuleID: 171; ID: 2; XPos: 231; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 3), (ModuleID: 171; ID: 3;
    XPos: 205; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 2), (ModuleID: 171; ID: 5; XPos: 179; YPos: 10; CodeRef: 0;
    InfoFunc: 0; Orientation: otVertical; GroupID: 1), (ModuleID: 171; ID: 7;
    XPos: 153; YPos: 10; CodeRef: 0; InfoFunc: 0; Orientation: otVertical;
    GroupID: 0));

  GraphDefs: array [0 .. 39] of TG2GraphDef = ((ModuleID: 8; ID: 26; XPos: 208;
    YPos: 4; Width: 35; Height: 22; GraphFunc: 18; slDependencies: '6,S0'),
    (ModuleID: 20; ID: 5; XPos: 173; YPos: 4; Width: 61; Height: 28;
    GraphFunc: 3; slDependencies: '1,2,3,4,0,5'), (ModuleID: 23; ID: 12;
    XPos: 173; YPos: 4; Width: 61; Height: 28; GraphFunc: 4;
    slDependencies: '0,1,2,3,8'), (ModuleID: 25; ID: 19; XPos: 199; YPos: 4;
    Width: 52; Height: 28; GraphFunc: 34; slDependencies: '11,5,7,10'),
    (ModuleID: 28; ID: 7; XPos: 173; YPos: 4; Width: 34; Height: 22;
    GraphFunc: 39; slDependencies: '0,3'), (ModuleID: 32; ID: 0; XPos: 173;
    YPos: 4; Width: 52; Height: 28; GraphFunc: 36; slDependencies: '0,1'),
    (ModuleID: 33; ID: 0; XPos: 182; YPos: 4; Width: 52; Height: 28;
    GraphFunc: 37; slDependencies: '0,1,2,3'), (ModuleID: 34; ID: 7; XPos: 173;
    YPos: 4; Width: 34; Height: 22; GraphFunc: 38; slDependencies: '0,3'),
    (ModuleID: 41; ID: 8; XPos: 142; YPos: 4; Width: 31; Height: 22;
    GraphFunc: 7; slDependencies: '0,1'), (ModuleID: 46; ID: 14; XPos: 176;
    YPos: 4; Width: 58; Height: 28; GraphFunc: 28; slDependencies: '1,2,4,0,5'),
    (ModuleID: 51; ID: 19; XPos: 182; YPos: 4; Width: 52; Height: 28;
    GraphFunc: 21; slDependencies: '0,4,8,3,5,6'), (ModuleID: 52; ID: 0;
    XPos: 92; YPos: 4; Width: 84; Height: 28; GraphFunc: 17;
    slDependencies: '0,1,2,3,4,5,6,7,9,10,12'), (ModuleID: 54; ID: 19;
    XPos: 182; YPos: 4; Width: 52; Height: 28; GraphFunc: 40;
    slDependencies: '0,1,2,4,3'), (ModuleID: 55; ID: 9; XPos: 142; YPos: 4;
    Width: 31; Height: 22; GraphFunc: 6; slDependencies: '0,1'), (ModuleID: 58;
    ID: 17; XPos: 157; YPos: 4; Width: 64; Height: 21; GraphFunc: 46;
    slDependencies: '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14'), (ModuleID: 60;
    ID: 28; XPos: 217; YPos: 4; Width: 34; Height: 22; GraphFunc: 12;
    slDependencies: '0'), (ModuleID: 61; ID: 8; XPos: 174; YPos: 4; Width: 34;
    Height: 22; GraphFunc: 14; slDependencies: '2,1'), (ModuleID: 62; ID: 4;
    XPos: 173; YPos: 4; Width: 34; Height: 22; GraphFunc: 15;
    slDependencies: '4,1'), (ModuleID: 74; ID: 5; XPos: 173; YPos: 4; Width: 34;
    Height: 22; GraphFunc: 16; slDependencies: '1'), (ModuleID: 84; ID: 5;
    XPos: 189; YPos: 3; Width: 45; Height: 24; GraphFunc: 1;
    slDependencies: '1,3,0,7,5'), (ModuleID: 87; ID: 9; XPos: 168; YPos: 4;
    Width: 32; Height: 22; GraphFunc: 30; slDependencies: '0,3,S0'),
    (ModuleID: 92; ID: 12; XPos: 182; YPos: 4; Width: 52; Height: 28;
    GraphFunc: 20; slDependencies: '0,3,4,5'), (ModuleID: 102; ID: 1; XPos: 182;
    YPos: 4; Width: 52; Height: 28; GraphFunc: 13; slDependencies: '5,3,4,9'),
    (ModuleID: 103; ID: 0; XPos: 182; YPos: 4; Width: 52; Height: 28;
    GraphFunc: 10; slDependencies: '0,1,2'), (ModuleID: 106; ID: 19; XPos: 220;
    YPos: 4; Width: 31; Height: 22; GraphFunc: 43; slDependencies: '6'),
    (ModuleID: 108; ID: 18; XPos: 35; YPos: 28; Width: 194; Height: 47;
    GraphFunc: 8; slDependencies: '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15'),
    (ModuleID: 108; ID: 72; XPos: 31; YPos: 103; Width: 154; Height: 12;
    GraphFunc: 22; slDependencies: '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15'),
    (ModuleID: 115; ID: 6; XPos: 180; YPos: 4; Width: 56; Height: 24;
    GraphFunc: 11; slDependencies: '0,1,2'), (ModuleID: 119; ID: 0; XPos: 92;
    YPos: 4; Width: 84; Height: 28; GraphFunc: 23;
    slDependencies: '2,3,4,5,6,7,1,8,9'), (ModuleID: 121; ID: 101; XPos: 217;
    YPos: 24; Width: 11; Height: 91; GraphFunc: 25;
    slDependencies: '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15'), (ModuleID: 121;
    ID: 13; XPos: 231; YPos: 40; Width: 21; Height: 25; GraphFunc: 26;
    slDependencies: '0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15'), (ModuleID: 134;
    ID: 8; XPos: 168; YPos: 4; Width: 32; Height: 22; GraphFunc: 31;
    slDependencies: '0,3,S0'), (ModuleID: 162; ID: 14; XPos: 182; YPos: 4;
    Width: 52; Height: 28; GraphFunc: 35; slDependencies: '3,5'),
    (ModuleID: 163; ID: 10; XPos: 213; YPos: 24; Width: 38; Height: 22;
    GraphFunc: 32; slDependencies: '7,9'), (ModuleID: 165; ID: 13; XPos: 112;
    YPos: 4; Width: 139; Height: 67; GraphFunc: 42; slDependencies: '0'),
    (ModuleID: 169; ID: 14; XPos: 176; YPos: 4; Width: 58; Height: 28;
    GraphFunc: 28; slDependencies: '0,1,2,6'), (ModuleID: 180; ID: 23; XPos: 87;
    YPos: 43; Width: 85; Height: 28; GraphFunc: 29;
    slDependencies: '8,9,10,11,12,13,14,15'), (ModuleID: 180; ID: 62; XPos: 103;
    YPos: 127; Width: 63; Height: 24; GraphFunc: 41;
    slDependencies: '17,18,19,20,21'), (ModuleID: 190; ID: 16; XPos: 199;
    YPos: 4; Width: 52; Height: 28; GraphFunc: 33; slDependencies: '4,6,8'),
    (ModuleID: 205; ID: 8; XPos: 214; YPos: 4; Width: 21; Height: 22;
    GraphFunc: 45; slDependencies: '0'));

  ButtonTextDefs: array [0 .. 196] of TG2ButtonTextDef = ((ModuleID: 3; ID: 9;
    XPos: 138; YPos: 14; CodeRef: 1; InfoFunc: 7; slText: 'M';
    ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13; slImageID: '4;4';
    ImageWidth: 9), (ModuleID: 4; ID: 8; XPos: 190; YPos: 14; CodeRef: 1;
    InfoFunc: 7; slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim;
    Width: 13; slImageID: '4;4'; ImageWidth: 9), (ModuleID: 7; ID: 18;
    XPos: 224; YPos: 59; CodeRef: 9; InfoFunc: 7; slText: 'M';
    ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13; slImageID: '4;4';
    ImageWidth: 9), (ModuleID: 8; ID: 19; XPos: 224; YPos: 44; CodeRef: 8;
    InfoFunc: 7; slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim;
    Width: 13; slImageID: '4;4'; ImageWidth: 9), (ModuleID: 9; ID: 11;
    XPos: 224; YPos: 29; CodeRef: 5; InfoFunc: 7; slText: 'M';
    ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13; slImageID: '4;4';
    ImageWidth: 9), (ModuleID: 12; ID: 17; XPos: 204; YPos: 29; CodeRef: 3;
    InfoFunc: 8; slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim;
    Width: 13; slImageID: '4;4'; ImageWidth: 9), (ModuleID: 13; ID: 27;
    XPos: 224; YPos: 29; CodeRef: 7; InfoFunc: 7; slText: 'M';
    ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13; slImageID: '4;4';
    ImageWidth: 9), (ModuleID: 20; ID: 23; XPos: 16; YPos: 16; CodeRef: 6;
    InfoFunc: 47; slText: 'KB'; ButtonType: bttCheck; ButtonStyle: btsSlim;
    Width: 18; slImageID: ''; ImageWidth: 0), (ModuleID: 23; ID: 38; XPos: 4;
    YPos: 17; CodeRef: 9; InfoFunc: 3; slText: 'KB'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 24; ID: 8; XPos: 238; YPos: 3; CodeRef: 4; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 25; ID: 1; XPos: 224; YPos: 59;
    CodeRef: 4; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 26; ID: 9; XPos: 224; YPos: 29; CodeRef: 5; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 28; ID: 9; XPos: 224; YPos: 10;
    CodeRef: 2; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 29; ID: 1; XPos: 239; YPos: 2; CodeRef: 2; InfoFunc: 7;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 31; ID: 5; XPos: 239; YPos: 2;
    CodeRef: 1; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 32; ID: 12; XPos: 239; YPos: 16; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 33; ID: 12; XPos: 239;
    YPos: 24; CodeRef: 5; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 34; ID: 9; XPos: 224; YPos: 10; CodeRef: 2; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 45; ID: 2; XPos: 239; YPos: 24;
    CodeRef: 9; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 46; ID: 4; XPos: 16; YPos: 16; CodeRef: 6; InfoFunc: 3;
    slText: 'KB'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 18;
    slImageID: ''; ImageWidth: 0), (ModuleID: 49; ID: 10; XPos: 128; YPos: 40;
    CodeRef: 3; InfoFunc: 72; slText: 'GC'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 49; ID: 22; XPos: 208; YPos: 44; CodeRef: 6; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 51; ID: 17; XPos: 112;
    YPos: 34; CodeRef: 3; InfoFunc: 72; slText: 'GC'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 51; ID: 24; XPos: 239; YPos: 31; CodeRef: 6; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 52; ID: 4; XPos: 16; YPos: 16;
    CodeRef: 11; InfoFunc: 3; slText: 'KB'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 54; ID: 17; XPos: 136; YPos: 29; CodeRef: 4; InfoFunc: 72;
    slText: 'GC'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 18;
    slImageID: ''; ImageWidth: 0), (ModuleID: 54; ID: 24; XPos: 239; YPos: 16;
    CodeRef: 3; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 56; ID: 27; XPos: 223; YPos: 57; CodeRef: 7; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 58; ID: 24; XPos: 239;
    YPos: 90; CodeRef: 15; InfoFunc: 3; slText: 'On'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 61; ID: 10; XPos: 224; YPos: 10; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 62; ID: 7; XPos: 224; YPos: 10;
    CodeRef: 2; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 63; ID: 9; XPos: 239; YPos: 17; CodeRef: 3; InfoFunc: 8;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 66; ID: 3; XPos: 82; YPos: 14;
    CodeRef: 0; InfoFunc: 5; slText: 'Inv'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 66; ID: 7; XPos: 167; YPos: 14; CodeRef: 2; InfoFunc: 5;
    slText: 'Inv'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 18;
    slImageID: ''; ImageWidth: 0), (ModuleID: 68; ID: 5; XPos: 176; YPos: 17;
    CodeRef: 1; InfoFunc: 3; slText: 'On'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 74; ID: 6; XPos: 224; YPos: 10; CodeRef: 2; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 82; ID: 2; XPos: 224; YPos: 10;
    CodeRef: 1; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 83; ID: 3; XPos: 224; YPos: 11; CodeRef: 1; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 84; ID: 8; XPos: 18; YPos: 16;
    CodeRef: 6; InfoFunc: 3; slText: 'KB'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 87; ID: 11; XPos: 224; YPos: 10; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 89; ID: 4; XPos: 239; YPos: 17;
    CodeRef: 3; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 92; ID: 17; XPos: 239; YPos: 24; CodeRef: 5; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 94; ID: 4; XPos: 227; YPos: 17;
    CodeRef: 2; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 96; ID: 8; XPos: 239; YPos: 2; CodeRef: 4; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 97; ID: 9; XPos: 224; YPos: 29;
    CodeRef: 5; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 98; ID: 2; XPos: 227; YPos: 17; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 102; ID: 3; XPos: 239;
    YPos: 31; CodeRef: 6; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 103; ID: 12; XPos: 239; YPos: 24; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 106; ID: 1; XPos: 224;
    YPos: 29; CodeRef: 7; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 108; ID: 19; XPos: 6; YPos: 40; CodeRef: 17; InfoFunc: 81;
    slText: 'Mon'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageWidth: 0), (ModuleID: 118; ID: 0; XPos: 239; YPos: 17;
    CodeRef: 3; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 119; ID: 4; XPos: 16; YPos: 17; CodeRef: 0; InfoFunc: 47;
    slText: 'KB'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 18;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 26; XPos: 25; YPos: 119;
    CodeRef: 16; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 29; XPos: 37; YPos: 119; CodeRef: 17; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 28; XPos: 61; YPos: 119;
    CodeRef: 19; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 31; XPos: 49; YPos: 119; CodeRef: 18; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 30; XPos: 73; YPos: 119;
    CodeRef: 20; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 33; XPos: 85; YPos: 119; CodeRef: 21; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 32; XPos: 109; YPos: 119;
    CodeRef: 23; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 35; XPos: 97; YPos: 119; CodeRef: 22; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 34; XPos: 121; YPos: 119;
    CodeRef: 24; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 37; XPos: 133; YPos: 119; CodeRef: 25; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 36; XPos: 157; YPos: 119;
    CodeRef: 27; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 39; XPos: 145; YPos: 119; CodeRef: 26; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 38; XPos: 169; YPos: 119;
    CodeRef: 28; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 41; XPos: 181; YPos: 119; CodeRef: 29; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 40; XPos: 205; YPos: 119;
    CodeRef: 31; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 43; XPos: 193; YPos: 119; CodeRef: 30; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 121; ID: 200; XPos: 231; YPos: 86;
    CodeRef: 35; InfoFunc: 47; slText: 'Rnd;Rnd'; ButtonType: bttPush;
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageWidth: 0),
    (ModuleID: 121; ID: 201; XPos: 231; YPos: 71; CodeRef: 36; InfoFunc: 47;
    slText: 'Clr;Clr'; ButtonType: bttPush; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageWidth: 0), (ModuleID: 125; ID: 8; XPos: 224; YPos: 10;
    CodeRef: 2; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 127; ID: 10; XPos: 190; YPos: 14; CodeRef: 1; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 134; ID: 10; XPos: 224;
    YPos: 10; CodeRef: 3; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 144; ID: 10; XPos: 34; YPos: 44; CodeRef: 0; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 12; XPos: 46; YPos: 44;
    CodeRef: 1; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 13; XPos: 70; YPos: 44; CodeRef: 3; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 15; XPos: 58; YPos: 44;
    CodeRef: 2; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 16; XPos: 82; YPos: 44; CodeRef: 4; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 17; XPos: 94; YPos: 44;
    CodeRef: 5; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 18; XPos: 118; YPos: 44; CodeRef: 7; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 19; XPos: 106; YPos: 44;
    CodeRef: 6; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 11; XPos: 130; YPos: 44; CodeRef: 8; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 20; XPos: 142; YPos: 44;
    CodeRef: 9; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 21; XPos: 166; YPos: 44; CodeRef: 11; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 22; XPos: 154; YPos: 44;
    CodeRef: 10; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 23; XPos: 178; YPos: 44; CodeRef: 12; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 24; XPos: 190; YPos: 44;
    CodeRef: 13; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 25; XPos: 214; YPos: 44; CodeRef: 15; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 26; XPos: 202; YPos: 44;
    CodeRef: 14; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 27; XPos: 34; YPos: 59; CodeRef: 16; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 28; XPos: 46; YPos: 59;
    CodeRef: 17; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 29; XPos: 70; YPos: 59; CodeRef: 19; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 30; XPos: 58; YPos: 59;
    CodeRef: 18; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 31; XPos: 82; YPos: 59; CodeRef: 20; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 32; XPos: 94; YPos: 59;
    CodeRef: 21; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 33; XPos: 118; YPos: 59; CodeRef: 23; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 34; XPos: 106; YPos: 59;
    CodeRef: 22; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 35; XPos: 130; YPos: 59; CodeRef: 24; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 36; XPos: 142; YPos: 59;
    CodeRef: 25; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 37; XPos: 166; YPos: 59; CodeRef: 27; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 38; XPos: 154; YPos: 59;
    CodeRef: 26; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 39; XPos: 178; YPos: 59; CodeRef: 28; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 40; XPos: 190; YPos: 59;
    CodeRef: 29; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 144; ID: 41; XPos: 214; YPos: 59; CodeRef: 31; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 144; ID: 42; XPos: 202; YPos: 59;
    CodeRef: 30; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 26; XPos: 29; YPos: 104; CodeRef: 16; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 29; XPos: 41; YPos: 104;
    CodeRef: 17; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 28; XPos: 65; YPos: 104; CodeRef: 19; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 31; XPos: 53; YPos: 104;
    CodeRef: 18; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 30; XPos: 77; YPos: 104; CodeRef: 20; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 33; XPos: 89; YPos: 104;
    CodeRef: 21; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 32; XPos: 113; YPos: 104; CodeRef: 23; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 35; XPos: 101; YPos: 104;
    CodeRef: 22; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 34; XPos: 125; YPos: 104; CodeRef: 24; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 37; XPos: 137; YPos: 104;
    CodeRef: 25; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 36; XPos: 161; YPos: 104; CodeRef: 27; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 39; XPos: 149; YPos: 104;
    CodeRef: 26; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 38; XPos: 173; YPos: 104; CodeRef: 28; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 41; XPos: 185; YPos: 104;
    CodeRef: 29; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 40; XPos: 209; YPos: 104; CodeRef: 31; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 43; XPos: 197; YPos: 104;
    CodeRef: 30; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 145; ID: 122; XPos: 231; YPos: 55; CodeRef: 36; InfoFunc: 47;
    slText: 'Rnd'; ButtonType: bttPush; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageWidth: 0), (ModuleID: 145; ID: 123; XPos: 231; YPos: 41;
    CodeRef: 37; InfoFunc: 47; slText: 'Clr'; ButtonType: bttPush;
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 26; XPos: 28; YPos: 104; CodeRef: 16; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 29; XPos: 40; YPos: 104;
    CodeRef: 17; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 28; XPos: 64; YPos: 104; CodeRef: 19; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 31; XPos: 52; YPos: 104;
    CodeRef: 18; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 30; XPos: 76; YPos: 104; CodeRef: 20; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 33; XPos: 88; YPos: 104;
    CodeRef: 21; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 32; XPos: 112; YPos: 104; CodeRef: 23; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 35; XPos: 100; YPos: 104;
    CodeRef: 22; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 34; XPos: 124; YPos: 104; CodeRef: 24; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 37; XPos: 136; YPos: 104;
    CodeRef: 25; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 36; XPos: 160; YPos: 104; CodeRef: 27; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 39; XPos: 148; YPos: 104;
    CodeRef: 26; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 38; XPos: 172; YPos: 104; CodeRef: 28; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 41; XPos: 184; YPos: 104;
    CodeRef: 29; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 40; XPos: 208; YPos: 104; CodeRef: 31; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 43; XPos: 196; YPos: 104;
    CodeRef: 30; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 146; ID: 200; XPos: 231; YPos: 56; CodeRef: 36; InfoFunc: 47;
    slText: 'Rnd'; ButtonType: bttPush; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageWidth: 0), (ModuleID: 146; ID: 12; XPos: 231; YPos: 41;
    CodeRef: 37; InfoFunc: 47; slText: 'Clr'; ButtonType: bttPush;
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageWidth: 0),
    (ModuleID: 150; ID: 14; XPos: 93; YPos: 4; CodeRef: 5; InfoFunc: 3;
    slText: 'Side Chain'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 49;
    slImageID: ''; ImageWidth: 0), (ModuleID: 150; ID: 16; XPos: 209; YPos: 59;
    CodeRef: 6; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 152; ID: 0; XPos: 137; YPos: 14; CodeRef: 10; InfoFunc: 3;
    slText: 'C'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 1; XPos: 147; YPos: 14;
    CodeRef: 12; InfoFunc: 3; slText: 'D'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 152; ID: 2; XPos: 157; YPos: 14; CodeRef: 2; InfoFunc: 3;
    slText: 'E'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 3; XPos: 167; YPos: 14;
    CodeRef: 3; InfoFunc: 3; slText: 'F'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 152; ID: 4; XPos: 177; YPos: 14; CodeRef: 5; InfoFunc: 3;
    slText: 'G'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 5; XPos: 187; YPos: 14;
    CodeRef: 7; InfoFunc: 3; slText: 'A'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 152; ID: 6; XPos: 197; YPos: 14; CodeRef: 9; InfoFunc: 3;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 7; XPos: 142; YPos: 2;
    CodeRef: 11; InfoFunc: 3; slText: 'C#'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 152; ID: 8; XPos: 152; YPos: 2; CodeRef: 13; InfoFunc: 3;
    slText: 'D#'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 9; XPos: 172; YPos: 2;
    CodeRef: 4; InfoFunc: 3; slText: 'F#'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 152; ID: 10; XPos: 182; YPos: 2; CodeRef: 6; InfoFunc: 3;
    slText: 'G#'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 10;
    slImageID: ''; ImageWidth: 8), (ModuleID: 152; ID: 11; XPos: 192; YPos: 2;
    CodeRef: 8; InfoFunc: 3; slText: 'Bb'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 10; slImageID: ''; ImageWidth: 8),
    (ModuleID: 154; ID: 26; XPos: 28; YPos: 104; CodeRef: 16; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 29; XPos: 40; YPos: 104;
    CodeRef: 17; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 28; XPos: 64; YPos: 104; CodeRef: 19; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 31; XPos: 52; YPos: 104;
    CodeRef: 18; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 30; XPos: 76; YPos: 104; CodeRef: 20; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 33; XPos: 88; YPos: 104;
    CodeRef: 21; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 32; XPos: 112; YPos: 104; CodeRef: 23; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 35; XPos: 100; YPos: 104;
    CodeRef: 22; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 34; XPos: 124; YPos: 104; CodeRef: 24; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 37; XPos: 136; YPos: 104;
    CodeRef: 25; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 36; XPos: 160; YPos: 104; CodeRef: 27; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 39; XPos: 148; YPos: 104;
    CodeRef: 26; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 38; XPos: 172; YPos: 104; CodeRef: 28; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 41; XPos: 184; YPos: 104;
    CodeRef: 29; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 40; XPos: 208; YPos: 104; CodeRef: 31; InfoFunc: 47;
    slText: ''; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 12;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 43; XPos: 196; YPos: 104;
    CodeRef: 30; InfoFunc: 47; slText: ''; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 12; slImageID: ''; ImageWidth: 0),
    (ModuleID: 154; ID: 6; XPos: 231; YPos: 51; CodeRef: 35; InfoFunc: 47;
    slText: 'Rnd'; ButtonType: bttPush; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageWidth: 0), (ModuleID: 154; ID: 7; XPos: 231; YPos: 36;
    CodeRef: 36; InfoFunc: 47; slText: 'Clr'; ButtonType: bttPush;
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageWidth: 0),
    (ModuleID: 162; ID: 3; XPos: 239; YPos: 24; CodeRef: 7; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 163; ID: 18; XPos: 224;
    YPos: 59; CodeRef: 10; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 164; ID: 18; XPos: 224; YPos: 59; CodeRef: 10; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 167; ID: 14; XPos: 239;
    YPos: 17; CodeRef: 4; InfoFunc: 8; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 169; ID: 31; XPos: 16; YPos: 17; CodeRef: 7; InfoFunc: 3;
    slText: 'KB'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 18;
    slImageID: ''; ImageWidth: 0), (ModuleID: 170; ID: 10; XPos: 190; YPos: 14;
    CodeRef: 1; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 171; ID: 13; XPos: 138; YPos: 14; CodeRef: 1; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 176; ID: 8; XPos: 239;
    YPos: 17; CodeRef: 4; InfoFunc: 8; slText: 'B'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 177; ID: 9; XPos: 239; YPos: 24; CodeRef: 7; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 180; ID: 31; XPos: 60;
    YPos: 20; CodeRef: 1; InfoFunc: 3; slText: 'Sync'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageWidth: 0),
    (ModuleID: 180; ID: 79; XPos: 239; YPos: 150; CodeRef: 23; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 180; ID: 54; XPos: 36;
    YPos: 55; CodeRef: 24; InfoFunc: 3; slText: 'KB'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 18; slImageID: ''; ImageWidth: 0),
    (ModuleID: 182; ID: 19; XPos: 239; YPos: 31; CodeRef: 9; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 183; ID: 11; XPos: 224;
    YPos: 29; CodeRef: 5; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 189; ID: 7; XPos: 239; YPos: 17; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 190; ID: 7; XPos: 224;
    YPos: 44; CodeRef: 7; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 192; ID: 8; XPos: 224; YPos: 10; CodeRef: 3; InfoFunc: 8;
    slText: 'B'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 196; ID: 17; XPos: 224;
    YPos: 29; CodeRef: 8; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 200; ID: 8; XPos: 238; YPos: 2; CodeRef: 4; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 202; ID: 9; XPos: 224;
    YPos: 29; CodeRef: 5; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 204; ID: 6; XPos: 239; YPos: 2; CodeRef: 4; InfoFunc: 7;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 205; ID: 5; XPos: 238; YPos: 2;
    CodeRef: 2; InfoFunc: 0; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9),
    (ModuleID: 206; ID: 5; XPos: 239; YPos: 15; CodeRef: 2; InfoFunc: 0;
    slText: 'M'; ButtonType: bttCheck; ButtonStyle: btsSlim; Width: 13;
    slImageID: '4;4'; ImageWidth: 9), (ModuleID: 208; ID: 20; XPos: 224;
    YPos: 29; CodeRef: 6; InfoFunc: 7; slText: 'M'; ButtonType: bttCheck;
    ButtonStyle: btsSlim; Width: 13; slImageID: '4;4'; ImageWidth: 9));

  ButtonFlatDefs: array [0 .. 152] of TG2ButtonFlatDef = ((ModuleID: 3; ID: 10;
    XPos: 23; YPos: 14; CodeRef: 2; InfoFunc: 180;
    slText: '0dB;+6dB;+12dB;+18dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 4; ID: 12;
    XPos: 23; YPos: 14; CodeRef: 2; InfoFunc: 180;
    slText: '0dB;+6dB;+12dB;+18dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 7; ID: 21;
    XPos: 47; YPos: 34; CodeRef: 4; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 7; ID: 27; XPos: 114; YPos: 38; CodeRef: 10;
    InfoFunc: 125; slText: 'FM Lin;FM Trk'; ButtonStyle: btsSlim; Width: 36;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 7; ID: 29;
    XPos: 126; YPos: 15; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 8; ID: 8; XPos: 77; YPos: 19; CodeRef: 4;
    InfoFunc: 63; slText: 'Semi;Freq;Fac;Part'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 8; ID: 11;
    XPos: 136; YPos: 23; CodeRef: 9; InfoFunc: 125; slText: 'FM Lin;FM Trk';
    ButtonStyle: btsSlim; Width: 36; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 8; ID: 28; XPos: 48; YPos: 32; CodeRef: 2;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 9; ID: 14;
    XPos: 98; YPos: 4; CodeRef: 3; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 9; ID: 9; XPos: 181; YPos: 5; CodeRef: 6;
    InfoFunc: 125; slText: 'FM Lin;FM Trk'; ButtonStyle: btsSlim; Width: 36;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 9; ID: 21;
    XPos: 74; YPos: 29; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 13; ID: 7; XPos: 98; YPos: 4; CodeRef: 4;
    InfoFunc: 63; slText: 'Semi;Freq;Fac;Part'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 13; ID: 12;
    XPos: 74; YPos: 29; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 18; ID: 13; XPos: 42; YPos: 14; CodeRef: 2;
    InfoFunc: 157; slText: 'Log;Lin'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 19; ID: 0;
    XPos: 42; YPos: 14; CodeRef: 4; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 20; ID: 24; XPos: 178; YPos: 44; CodeRef: 0;
    InfoFunc: 136; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '33;34;35;36'; ImageCount: 4; ImageWidth: 18), (ModuleID: 20;
    ID: 28; XPos: 35; YPos: 44; CodeRef: 7; InfoFunc: 138; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '37;38'; ImageCount: 2;
    ImageWidth: 10), (ModuleID: 24; ID: 4; XPos: 77; YPos: 9; CodeRef: 3;
    InfoFunc: 104; slText: 'Rate Sub;Rate Lo;Rate Hi;BPM;Clk';
    ButtonStyle: btsSlim; Width: 42; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 24; ID: 10; XPos: 42; YPos: 14; CodeRef: 1;
    InfoFunc: 4; slText: 'Poly;Mono'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 25; ID: 11;
    XPos: 74; YPos: 19; CodeRef: 1; InfoFunc: 104;
    slText: 'Rate Sub;Rate Lo;Rate Hi;BPM;Clk'; ButtonStyle: btsSlim; Width: 42;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 25; ID: 26;
    XPos: 44; YPos: 22; CodeRef: 2; InfoFunc: 105;
    slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 25; ID: 14;
    XPos: 41; YPos: 36; CodeRef: 9; InfoFunc: 4; slText: 'Poly;Mono';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 26; ID: 6; XPos: 166; YPos: 29; CodeRef: 2;
    InfoFunc: 105; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 26;
    ID: 16; XPos: 77; YPos: 9; CodeRef: 7; InfoFunc: 104;
    slText: 'Rate Sub;Rate Lo;Rate Hi;BPM;Clk'; ButtonStyle: btsSlim; Width: 42;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 26; ID: 13;
    XPos: 43; YPos: 24; CodeRef: 1; InfoFunc: 4; slText: 'Poly;Mono';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 27; ID: 11; XPos: 98; YPos: 4; CodeRef: 3;
    InfoFunc: 63; slText: 'Semi;Freq;Fac'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 27; ID: 12;
    XPos: 74; YPos: 29; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 32; ID: 3; XPos: 77; YPos: 29; CodeRef: 4;
    InfoFunc: 213; slText: '80;110;160'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 32; ID: 4;
    XPos: 126; YPos: 29; CodeRef: 5; InfoFunc: 214; slText: '6k;8k;12k';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 33; ID: 24; XPos: 137; YPos: 44; CodeRef: 7;
    InfoFunc: 214; slText: '6k;8k;12k'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 33; ID: 25;
    XPos: 4; YPos: 44; CodeRef: 6; InfoFunc: 213; slText: '80;110;160';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 38; ID: 2; XPos: 101; YPos: 9; CodeRef: 2;
    InfoFunc: 30; slText: 'Sub;Lo;Hi'; ButtonStyle: btsSlim; Width: 23;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 40; ID: 2; XPos: 6;
    YPos: 19; CodeRef: 8; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 40; ID: 30; XPos: 6; YPos: 43; CodeRef: 9;
    InfoFunc: 188; slText: ' 0dB;-6dB;-12dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 42; ID: 3;
    XPos: 101; YPos: 9; CodeRef: 2; InfoFunc: 30; slText: 'Sub;Lo;Hi';
    ButtonStyle: btsSlim; Width: 23; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 43; ID: 5; XPos: 54; YPos: 14; CodeRef: 1;
    InfoFunc: 18; slText: 'BiP;Uni'; ButtonStyle: btsSlim; Width: 23;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 46; ID: 5;
    XPos: 178; YPos: 44; CodeRef: 0; InfoFunc: 136; slText: '';
    ButtonStyle: btsSlim; Width: 20; slImageID: '33;34;35;36'; ImageCount: 4;
    ImageWidth: 18), (ModuleID: 46; ID: 20; XPos: 46; YPos: 44; CodeRef: 3;
    InfoFunc: 138; slText: ''; ButtonStyle: btsSlim; Width: 15;
    slImageID: '37;38'; ImageCount: 2; ImageWidth: 10), (ModuleID: 47; ID: 13;
    XPos: 42; YPos: 14; CodeRef: 2; InfoFunc: 157; slText: 'Log;Lin';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 49; ID: 5; XPos: 49; YPos: 42; CodeRef: 2;
    InfoFunc: 71; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 50;
    ID: 3; XPos: 54; YPos: 14; CodeRef: 2; InfoFunc: 18; slText: 'BiP;Uni';
    ButtonStyle: btsSlim; Width: 23; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 51; ID: 11; XPos: 83; YPos: 57; CodeRef: 2;
    InfoFunc: 71; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 52;
    ID: 40; XPos: 179; YPos: 20; CodeRef: 12; InfoFunc: 136; slText: '';
    ButtonStyle: btsSlim; Width: 20; slImageID: '33;34;35;36'; ImageCount: 4;
    ImageWidth: 18), (ModuleID: 52; ID: 38; XPos: 68; YPos: 24; CodeRef: 9;
    InfoFunc: 50; slText: 'L1;L2;L3;Trg'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 52; ID: 45;
    XPos: 4; YPos: 74; CodeRef: 8; InfoFunc: 138; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '37;38'; ImageCount: 2;
    ImageWidth: 10), (ModuleID: 56; ID: 7; XPos: 81; YPos: 4; CodeRef: 4;
    InfoFunc: 63; slText: 'Semi;Freq;Fac;Part'; ButtonStyle: btsSlim; Width: 28;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 56; ID: 12;
    XPos: 56; YPos: 30; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 22; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 56; ID: 18; XPos: 119; YPos: 53; CodeRef: 9;
    InfoFunc: 0; slText: 'String1;String2;Tube1;Tube2;Tube3';
    ButtonStyle: btsNormal; Width: 45; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 57; ID: 13; XPos: 81; YPos: 14; CodeRef: 3;
    InfoFunc: 211; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 61; ID: 12;
    XPos: 32; YPos: 14; CodeRef: 2; InfoFunc: 82; slText: 'Asym;Sym';
    ButtonStyle: btsSlim; Width: 32; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 62; ID: 11; XPos: 68; YPos: 14; CodeRef: 3;
    InfoFunc: 167; slText: 'Soft;Hard;Fat;Heavy'; ButtonStyle: btsSlim;
    Width: 36; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 62;
    ID: 9; XPos: 32; YPos: 14; CodeRef: 4; InfoFunc: 168; slText: 'Asym;Sym';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 63; ID: 5; XPos: 195; YPos: 20; CodeRef: 2;
    InfoFunc: 202; slText: '12.5m;25m;50m;100m'; ButtonStyle: btsSlim;
    Width: 30; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 66;
    ID: 2; XPos: 41; YPos: 14; CodeRef: 4; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 68; ID: 4; XPos: 4; YPos: 44; CodeRef: 2;
    InfoFunc: 187; slText: 'Internal;Master'; ButtonStyle: btsSlim; Width: 42;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 81; ID: 7;
    XPos: 54; YPos: 14; CodeRef: 1; InfoFunc: 148; slText: 'Lin;dB';
    ButtonStyle: btsSlim; Width: 23; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 84; ID: 7; XPos: 18; YPos: 29; CodeRef: 4;
    InfoFunc: 43; slText: 'Trig;Gate'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 84; ID: 23;
    XPos: 165; YPos: 14; CodeRef: 0; InfoFunc: 136; slText: '';
    ButtonStyle: btsSlim; Width: 20; slImageID: '33;34;35;36'; ImageCount: 4;
    ImageWidth: 18), (ModuleID: 84; ID: 20; XPos: 119; YPos: 29; CodeRef: 7;
    InfoFunc: 139; slText: 'Dcy;Rel'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 84; ID: 22;
    XPos: 76; YPos: 29; CodeRef: 2; InfoFunc: 138; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: ''; ImageCount: 2;
    ImageWidth: 10), (ModuleID: 87; ID: 6; XPos: 24; YPos: 14; CodeRef: 2;
    InfoFunc: 71; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 26; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 92;
    ID: 7; XPos: 49; YPos: 42; CodeRef: 2; InfoFunc: 71;
    slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 96; ID: 4;
    XPos: 134; YPos: 9; CodeRef: 3; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 96; ID: 11; XPos: 61; YPos: 14; CodeRef: 2;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 97; ID: 14;
    XPos: 94; YPos: 4; CodeRef: 6; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 97; ID: 12; XPos: 70; YPos: 29; CodeRef: 2;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 98; ID: 9;
    XPos: 86; YPos: 28; CodeRef: 2; InfoFunc: 30; slText: 'Sub;Lo;Hi';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 102; ID: 21; XPos: 149; YPos: 20; CodeRef: 9;
    InfoFunc: 172; slText: 'Notch;Peak;Deep'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 102; ID: 18;
    XPos: 4; YPos: 25; CodeRef: 10; InfoFunc: 71;
    slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 106; ID: 16;
    XPos: 98; YPos: 4; CodeRef: 4; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 106; ID: 17; XPos: 74; YPos: 29; CodeRef: 2;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 108; ID: 65;
    XPos: 7; YPos: 103; CodeRef: 16; InfoFunc: 80; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 112; ID: 9; XPos: 54; YPos: 14; CodeRef: 1;
    InfoFunc: 18; slText: 'BiP;Uni'; ButtonStyle: btsSlim; Width: 23;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 115; ID: 17;
    XPos: 30; YPos: 29; CodeRef: 3; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 116; ID: 18; XPos: 22; YPos: 14; CodeRef: 0;
    InfoFunc: 57; slText: ' 0dB;-6dB;-12dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 119; ID: 32;
    XPos: 70; YPos: 17; CodeRef: 8; InfoFunc: 49; slText: 'L1;L2';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 119; ID: 16; XPos: 179; YPos: 20; CodeRef: 1;
    InfoFunc: 136; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '33;34;35;36'; ImageCount: 4; ImageWidth: 18), (ModuleID: 119;
    ID: 36; XPos: 37; YPos: 59; CodeRef: 10; InfoFunc: 138; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '37;38'; ImageCount: 2;
    ImageWidth: 10), (ModuleID: 121; ID: 12; XPos: 4; YPos: 18; CodeRef: 32;
    InfoFunc: 9; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '67;68'; ImageCount: 2; ImageWidth: 15), (ModuleID: 121; ID: 10;
    XPos: 221; YPos: 119; CodeRef: 34; InfoFunc: 43; slText: 'T;G';
    ButtonStyle: btsSlim; Width: 13; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 123; ID: 21; XPos: 6; YPos: 19; CodeRef: 9;
    InfoFunc: 169; slText: 'Exp;Lin;dB'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 123; ID: 22;
    XPos: 6; YPos: 43; CodeRef: 8; InfoFunc: 188; slText: ' 0dB;-6dB';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 127; ID: 7; XPos: 23; YPos: 14; CodeRef: 2;
    InfoFunc: 149; slText: '+6dB;0dB;-6dB;-12dB'; ButtonStyle: btsSlim;
    Width: 30; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 134;
    ID: 6; XPos: 24; YPos: 14; CodeRef: 2; InfoFunc: 71;
    slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 26;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 140; ID: 33;
    XPos: 85; YPos: 4; CodeRef: 8; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 144; ID: 90; XPos: 87; YPos: 4; CodeRef: 32;
    InfoFunc: 9; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '67;68'; ImageCount: 2; ImageWidth: 15), (ModuleID: 144; ID: 14;
    XPos: 227; YPos: 44; CodeRef: 34; InfoFunc: 43; slText: 'T;G';
    ButtonStyle: btsSlim; Width: 11; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 144; ID: 88; XPos: 227; YPos: 59; CodeRef: 35;
    InfoFunc: 43; slText: 'T;G'; ButtonStyle: btsSlim; Width: 11; slImageID: '';
    ImageCount: 0; ImageWidth: 0), (ModuleID: 145; ID: 124; XPos: 87; YPos: 4;
    CodeRef: 32; InfoFunc: 9; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '67;68'; ImageCount: 2; ImageWidth: 15), (ModuleID: 145; ID: 10;
    XPos: 223; YPos: 104; CodeRef: 35; InfoFunc: 43; slText: 'T;G';
    ButtonStyle: btsSlim; Width: 13; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 145; ID: 120; XPos: 231; YPos: 69; CodeRef: 34;
    InfoFunc: 18; slText: 'BiP;Uni'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 146; ID: 19;
    XPos: 231; YPos: 71; CodeRef: 34; InfoFunc: 18; slText: 'BiP;Uni';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 146; ID: 13; XPos: 222; YPos: 104; CodeRef: 35;
    InfoFunc: 43; slText: 'T;G'; ButtonStyle: btsSlim; Width: 13; slImageID: '';
    ImageCount: 0; ImageWidth: 0), (ModuleID: 146; ID: 18; XPos: 87; YPos: 4;
    CodeRef: 32; InfoFunc: 9; slText: ''; ButtonStyle: btsSlim; Width: 20;
    slImageID: '67;68'; ImageCount: 2; ImageWidth: 15), (ModuleID: 149; ID: 19;
    XPos: 170; YPos: 4; CodeRef: 5; InfoFunc: 210;
    slText: 'Notes Only;Note+Ctrls'; ButtonStyle: btsSlim; Width: 60;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 152; ID: 19;
    XPos: 40; YPos: 14; CodeRef: 1; InfoFunc: 194; slText: 'Closest;Evenly';
    ButtonStyle: btsSlim; Width: 36; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 154; ID: 4; XPos: 141; YPos: 4; CodeRef: 34;
    InfoFunc: 134; slText: 'Off;25%;50%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 154; ID: 8;
    XPos: 231; YPos: 66; CodeRef: 33; InfoFunc: 18; slText: 'BiP;Uni';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 154; ID: 9; XPos: 222; YPos: 104; CodeRef: 32;
    InfoFunc: 43; slText: 'T;G'; ButtonStyle: btsSlim; Width: 13; slImageID: '';
    ImageCount: 0; ImageWidth: 0), (ModuleID: 158; ID: 11; XPos: 181; YPos: 14;
    CodeRef: 2; InfoFunc: 157; slText: 'Log;Lin'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 158;
    ID: 1; XPos: 80; YPos: 14; CodeRef: 1; InfoFunc: 3; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 161; ID: 35; XPos: 90; YPos: 4; CodeRef: 16;
    InfoFunc: 169; slText: 'Exp;Lin;dB'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 161; ID: 48;
    XPos: 177; YPos: 4; CodeRef: 17; InfoFunc: 188; slText: ' 0dB;-6dB;-12dB';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 162; ID: 17; XPos: 42; YPos: 44; CodeRef: 2;
    InfoFunc: 71; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 162;
    ID: 23; XPos: 149; YPos: 20; CodeRef: 5; InfoFunc: 172;
    slText: 'Notch;Peak;Deep'; ButtonStyle: btsSlim; Width: 30; slImageID: '';
    ImageCount: 0; ImageWidth: 0), (ModuleID: 163; ID: 21; XPos: 47; YPos: 34;
    CodeRef: 4; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 163; ID: 27; XPos: 114; YPos: 38; CodeRef: 6;
    InfoFunc: 125; slText: 'FM Lin;FM Trk'; ButtonStyle: btsSlim; Width: 36;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 163; ID: 28;
    XPos: 123; YPos: 17; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 164; ID: 21; XPos: 48; YPos: 34; CodeRef: 4;
    InfoFunc: 63; slText: 'Semi;Freq;Fac;Part'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 164; ID: 38;
    XPos: 101; YPos: 42; CodeRef: 2; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 164; ID: 39; XPos: 226; YPos: 45; CodeRef: 13;
    InfoFunc: 3; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 167; ID: 13;
    XPos: 195; YPos: 20; CodeRef: 3; InfoFunc: 202;
    slText: '12.5 ms;25 ms;50 ms;100 ms'; ButtonStyle: btsSlim; Width: 36;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 170; ID: 11;
    XPos: 23; YPos: 14; CodeRef: 2; InfoFunc: 149;
    slText: '+6dB;0dB;-6dB;-12dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 171; ID: 15;
    XPos: 23; YPos: 14; CodeRef: 2; InfoFunc: 149;
    slText: '+6dB;0dB;-6dB;-12dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 175; ID: 7;
    XPos: 4; YPos: 17; CodeRef: 8; InfoFunc: 144; slText: 'Time;Clk';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 176; ID: 15; XPos: 58; YPos: 13; CodeRef: 5;
    InfoFunc: 144; slText: 'Time;Clk'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 177; ID: 29;
    XPos: 58; YPos: 14; CodeRef: 4; InfoFunc: 144; slText: 'Time;Clk';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 180; ID: 33; XPos: 97; YPos: 4; CodeRef: 2;
    InfoFunc: 200; slText: 'Ratio;Fixed'; ButtonStyle: btsSlim; Width: 36;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 180; ID: 66;
    XPos: 142; YPos: 164; CodeRef: 20; InfoFunc: 199;
    slText: '-Lin;-Exp;+Exp;+Lin'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 180; ID: 70;
    XPos: 74; YPos: 164; CodeRef: 18; InfoFunc: 199;
    slText: '-Lin;-Exp;+Exp;+Lin'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 180; ID: 50;
    XPos: 32; YPos: 24; CodeRef: 0; InfoFunc: 26; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 182; ID: 18; XPos: 32; YPos: 15; CodeRef: 6;
    InfoFunc: 144; slText: 'Time;Clk'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 183; ID: 14;
    XPos: 98; YPos: 4; CodeRef: 3; InfoFunc: 63; slText: 'Semi;Freq;Fac;Part';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 183; ID: 22; XPos: 74; YPos: 29; CodeRef: 2;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 184; ID: 2;
    XPos: 43; YPos: 14; CodeRef: 2; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 185; ID: 7; XPos: 76; YPos: 14; CodeRef: 2;
    InfoFunc: 169; slText: 'Exp;Lin;dB'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 188; ID: 3;
    XPos: 54; YPos: 14; CodeRef: 2; InfoFunc: 18; slText: 'BiP;Uni';
    ButtonStyle: btsSlim; Width: 23; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 190; ID: 13; XPos: 112; YPos: 44; CodeRef: 3;
    InfoFunc: 105; slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 190;
    ID: 15; XPos: 77; YPos: 4; CodeRef: 2; InfoFunc: 104;
    slText: 'Rate Sub;Rate Lo;Rate Hi;BPM;Clk'; ButtonStyle: btsSlim; Width: 42;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 190; ID: 27;
    XPos: 43; YPos: 19; CodeRef: 5; InfoFunc: 4; slText: 'Poly;Mono';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 192; ID: 0; XPos: 9; YPos: 14; CodeRef: 0;
    InfoFunc: 170; slText: 'Type I;Type II'; ButtonStyle: btsSlim; Width: 40;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 194; ID: 2;
    XPos: 43; YPos: 14; CodeRef: 4; InfoFunc: 169; slText: 'Exp;Lin;dB';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 195; ID: 13; XPos: 4; YPos: 14; CodeRef: 2;
    InfoFunc: 169; slText: 'Exp;Lin'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 195; ID: 7;
    XPos: 38; YPos: 14; CodeRef: 3; InfoFunc: 189; slText: 'm;1-m';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 196; ID: 10; XPos: 117; YPos: 4; CodeRef: 2;
    InfoFunc: 0; slText: 'Semi;Freq;Fac;Part'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 196; ID: 22;
    XPos: 228; YPos: 13; CodeRef: 7; InfoFunc: 3; slText: 'Off;On';
    ButtonStyle: btsSlim; Width: 20; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 196; ID: 15; XPos: 94; YPos: 29; CodeRef: 3;
    InfoFunc: 26; slText: 'Off;On'; ButtonStyle: btsSlim; Width: 20;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 200; ID: 4;
    XPos: 74; YPos: 14; CodeRef: 3; InfoFunc: 104; slText: 'Sub;Lo;Hi;BPM;Clk';
    ButtonStyle: btsSlim; Width: 25; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 200; ID: 10; XPos: 42; YPos: 14; CodeRef: 1;
    InfoFunc: 4; slText: 'Poly;Mono'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 200; ID: 11;
    XPos: 194; YPos: 14; CodeRef: 5; InfoFunc: 203;
    slText: '0%;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 200; ID: 12;
    XPos: 222; YPos: 14; CodeRef: 2; InfoFunc: 204; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '25;27;29'; ImageCount: 3;
    ImageWidth: 13), (ModuleID: 200; ID: 14; XPos: 167; YPos: 14; CodeRef: 6;
    InfoFunc: 207; slText: '25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 202; ID: 6;
    XPos: 151; YPos: 20; CodeRef: 2; InfoFunc: 105;
    slText: 'Off;25%;50%;75%;100%'; ButtonStyle: btsSlim; Width: 25;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 202; ID: 16;
    XPos: 81; YPos: 8; CodeRef: 7; InfoFunc: 104;
    slText: 'Rate Sub;Rate Lo;Rate Hi;BPM;Clk'; ButtonStyle: btsSlim; Width: 42;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 202; ID: 13;
    XPos: 46; YPos: 21; CodeRef: 1; InfoFunc: 4; slText: 'Poly;Mono';
    ButtonStyle: btsSlim; Width: 30; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 202; ID: 15; XPos: 211; YPos: 14; CodeRef: 8;
    InfoFunc: 203; slText: '0%;25%;50%;75%;100%'; ButtonStyle: btsSlim;
    Width: 25; slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 202;
    ID: 21; XPos: 206; YPos: 29; CodeRef: 6; InfoFunc: 204; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '25;27;29'; ImageCount: 3;
    ImageWidth: 13), (ModuleID: 204; ID: 5; XPos: 36; YPos: 14; CodeRef: 1;
    InfoFunc: 4; slText: 'Poly;Mono'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 204; ID: 7;
    XPos: 222; YPos: 14; CodeRef: 3; InfoFunc: 204; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '25;27;29'; ImageCount: 3;
    ImageWidth: 13), (ModuleID: 205; ID: 19; XPos: 36; YPos: 14; CodeRef: 3;
    InfoFunc: 4; slText: 'Poly;Mono'; ButtonStyle: btsSlim; Width: 30;
    slImageID: ''; ImageCount: 0; ImageWidth: 0), (ModuleID: 206; ID: 19;
    XPos: 125; YPos: 8; CodeRef: 3; InfoFunc: 4; slText: 'Poly;Mono';
    ButtonStyle: btsSlim; Width: 28; slImageID: ''; ImageCount: 0;
    ImageWidth: 0), (ModuleID: 206; ID: 26; XPos: 222; YPos: 29; CodeRef: 1;
    InfoFunc: 204; slText: ''; ButtonStyle: btsSlim; Width: 15;
    slImageID: '25;27;29'; ImageCount: 3; ImageWidth: 13), (ModuleID: 208;
    ID: 19; XPos: 206; YPos: 29; CodeRef: 5; InfoFunc: 204; slText: '';
    ButtonStyle: btsSlim; Width: 15; slImageID: '25;27;29'; ImageCount: 3;
    ImageWidth: 13));

  ButtonRadioDefs: array [0 .. 22] of TG2ButtonRadioDef = ((ModuleID: 3; ID: 8;
    XPos: 78; YPos: 8; CodeRef: 0; InfoFunc: 183; Orientation: otHorizontal;
    ButtonCount: 3; ButtonWidth: 20; slText: 'Out;Fx;Bus';
    ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0), (ModuleID: 4; ID: 0;
    XPos: 78; YPos: 12; CodeRef: 0; InfoFunc: 182; Orientation: otHorizontal;
    ButtonCount: 6; ButtonWidth: 18; slText: '1/2;3/4;1/2;3/4;1/2;3/4';
    ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0), (ModuleID: 7; ID: 16;
    XPos: 165; YPos: 4; CodeRef: 8; InfoFunc: 156; Orientation: otHorizontal;
    ButtonCount: 5; ButtonWidth: 18; slText: ''; ButtonStyle: btsNormal;
    slImageID: '5;6;7;8;11'; ImageWidth: 15), (ModuleID: 25; ID: 31; XPos: 92;
    YPos: 4; CodeRef: 11; InfoFunc: 165; Orientation: otHorizontal;
    ButtonCount: 6; ButtonWidth: 18; slText: '1;2;3;4;5';
    ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 15), (ModuleID: 26;
    ID: 7; XPos: 148; YPos: 4; CodeRef: 4; InfoFunc: 106;
    Orientation: otHorizontal; ButtonCount: 6; ButtonWidth: 18; slText: '';
    ButtonStyle: btsNormal; slImageID: '5;6;7;8;21;22'; ImageWidth: 13),
    (ModuleID: 28; ID: 6; XPos: 43; YPos: 13; CodeRef: 3; InfoFunc: 2;
    Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 13;
    slText: '1;2;3;4'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 34; ID: 6; XPos: 43; YPos: 13; CodeRef: 3; InfoFunc: 192;
    Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 16;
    slText: 'x2;x3;x4;x5'; ButtonStyle: btsNormal; slImageID: '';
    ImageWidth: 0), (ModuleID: 49; ID: 13; XPos: 186; YPos: 16; CodeRef: 5;
    InfoFunc: 73; Orientation: otHorizontal; ButtonCount: 2; ButtonWidth: 18;
    slText: '6;12'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 51; ID: 14; XPos: 161; YPos: 4; CodeRef: 8; InfoFunc: 75;
    Orientation: otVertical; ButtonCount: 4; ButtonWidth: 18;
    slText: 'BR;HP;BP;LP'; ButtonStyle: btsNormal; slImageID: '';
    ImageWidth: 0), (ModuleID: 51; ID: 20; XPos: 192; YPos: 50; CodeRef: 5;
    InfoFunc: 74; Orientation: otHorizontal; ButtonCount: 2; ButtonWidth: 18;
    slText: '12;24'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 54; ID: 14; XPos: 127; YPos: 4; CodeRef: 2; InfoFunc: 75;
    Orientation: otHorizontal; ButtonCount: 3; ButtonWidth: 18;
    slText: 'LP;BP;HP'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 58; ID: 10; XPos: 231; YPos: 30; CodeRef: 10; InfoFunc: 218;
    Orientation: otVertical; ButtonCount: 3; ButtonWidth: 18;
    slText: 'HP;BP;LP'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 82; ID: 4; XPos: 96; YPos: 8; CodeRef: 0; InfoFunc: 83;
    Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 22; slText: '';
    ButtonStyle: btsNormal; slImageID: '47;48;49;50'; ImageWidth: 13),
    (ModuleID: 83; ID: 5; XPos: 96; YPos: 8; CodeRef: 0; InfoFunc: 84;
    Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 22; slText: '';
    ButtonStyle: btsNormal; slImageID: '51;52;53;54'; ImageWidth: 19),
    (ModuleID: 92; ID: 13; XPos: 182; YPos: 42; CodeRef: 4; InfoFunc: 93;
    Orientation: otHorizontal; ButtonCount: 3; ButtonWidth: 18;
    slText: '12;18;24'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 97; ID: 7; XPos: 148; YPos: 4; CodeRef: 4; InfoFunc: 58;
    Orientation: otHorizontal; ButtonCount: 6; ButtonWidth: 18; slText: '';
    ButtonStyle: btsNormal; slImageID: '5;6;7;8;9;10'; ImageWidth: 13),
    (ModuleID: 127; ID: 5; XPos: 78; YPos: 12; CodeRef: 0; InfoFunc: 152;
    Orientation: otHorizontal; ButtonCount: 2; ButtonWidth: 18;
    slText: '1/2;3/4'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 157; ID: 0; XPos: 96; YPos: 12; CodeRef: 1; InfoFunc: 127;
    Orientation: otHorizontal; ButtonCount: 3; ButtonWidth: 25;
    slText: 'BiPol;Pos;Neg'; ButtonStyle: btsNormal; slImageID: '';
    ImageWidth: 0), (ModuleID: 163; ID: 16; XPos: 148; YPos: 4; CodeRef: 9;
    InfoFunc: 155; Orientation: otHorizontal; ButtonCount: 6; ButtonWidth: 18;
    slText: ''; ButtonStyle: btsNormal; slImageID: '18;17;16;15;14;12';
    ImageWidth: 15), (ModuleID: 170; ID: 4; XPos: 78; YPos: 12; CodeRef: 0;
    InfoFunc: 150; Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 18;
    slText: '1/2;3/4;1/2;3/4'; ButtonStyle: btsNormal; slImageID: '';
    ImageWidth: 0), (ModuleID: 171; ID: 8; XPos: 78; YPos: 8; CodeRef: 0;
    InfoFunc: 151; Orientation: otHorizontal; ButtonCount: 2; ButtonWidth: 20;
    slText: 'In;Bus'; ButtonStyle: btsNormal; slImageID: ''; ImageWidth: 0),
    (ModuleID: 190; ID: 4; XPos: 126; YPos: 4; CodeRef: 4; InfoFunc: 164;
    Orientation: otHorizontal; ButtonCount: 4; ButtonWidth: 18;
    slText: '1;2;3;4;5'; ButtonStyle: btsNormal; slImageID: '5;6;7;8';
    ImageWidth: 15), (ModuleID: 199; ID: 6; XPos: 88; YPos: 8; CodeRef: 0;
    InfoFunc: 181; Orientation: otHorizontal; ButtonCount: 3; ButtonWidth: 25;
    slText: 'Last;Lo;Hi'; ButtonStyle: btsNormal; slImageID: '';
    ImageWidth: 0));

  KnobDefs: array [0 .. 420] of TG2KnobDef = ((ModuleID: 7; ID: 3; XPos: 18;
    YPos: 52; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 7; ID: 12;
    XPos: 188; YPos: 26; CodeRef: 6; InfoFunc: 126; KnobType: ktMedium),
    (ModuleID: 7; ID: 5; XPos: 50; YPos: 48; CodeRef: 0; InfoFunc: 61;
    KnobType: ktBig), (ModuleID: 7; ID: 6; XPos: 78; YPos: 44; CodeRef: 1;
    InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 7; ID: 8; XPos: 129;
    YPos: 52; CodeRef: 5; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 7; ID: 14;
    XPos: 194; YPos: 52; CodeRef: 7; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 8; ID: 2; XPos: 18; YPos: 37; CodeRef: 3; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 8; ID: 6; XPos: 185; YPos: 4; CodeRef: 6;
    InfoFunc: 126; KnobType: ktMedium), (ModuleID: 8; ID: 9; XPos: 80; YPos: 33;
    CodeRef: 0; InfoFunc: 61; KnobType: ktBig), (ModuleID: 8; ID: 10; XPos: 108;
    YPos: 29; CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 8;
    ID: 13; XPos: 151; YPos: 37; CodeRef: 5; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 8; ID: 16; XPos: 201; YPos: 37; CodeRef: 7; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 9; ID: 2; XPos: 102; YPos: 18; CodeRef: 0;
    InfoFunc: 61; KnobType: ktBig), (ModuleID: 9; ID: 3; XPos: 129; YPos: 14;
    CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 9; ID: 7;
    XPos: 196; YPos: 22; CodeRef: 4; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 9; ID: 19; XPos: 18; YPos: 22; CodeRef: 7; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 12; ID: 0; XPos: 94; YPos: 20; CodeRef: 0;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 12; ID: 9; XPos: 149; YPos: 20;
    CodeRef: 1; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 12; ID: 12;
    XPos: 180; YPos: 20; CodeRef: 2; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 13; ID: 2; XPos: 18; YPos: 22; CodeRef: 3; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 13; ID: 6; XPos: 102; YPos: 18; CodeRef: 0;
    InfoFunc: 61; KnobType: ktBig), (ModuleID: 13; ID: 9; XPos: 130; YPos: 14;
    CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 13; ID: 10;
    XPos: 164; YPos: 20; CodeRef: 5; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 13; ID: 16; XPos: 195; YPos: 20; CodeRef: 6; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 18; ID: 4; XPos: 101; YPos: 7; CodeRef: 0;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 18; ID: 0; XPos: 140; YPos: 3;
    CodeRef: 1; InfoFunc: 17; KnobType: ktReset), (ModuleID: 19; ID: 2;
    XPos: 90; YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 19; ID: 5; XPos: 128; YPos: 5; CodeRef: 1; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 19; ID: 8; XPos: 166; YPos: 5; CodeRef: 2;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 19; ID: 11; XPos: 204; YPos: 5;
    CodeRef: 3; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 20; ID: 8;
    XPos: 53; YPos: 35; CodeRef: 1; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 20; ID: 9; XPos: 150; YPos: 35; CodeRef: 4; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 20; ID: 15; XPos: 87; YPos: 35; CodeRef: 2;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 20; ID: 21; XPos: 117;
    YPos: 35; CodeRef: 3; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 22;
    ID: 0; XPos: 148; YPos: 5; CodeRef: 0; InfoFunc: 68; KnobType: ktMedium),
    (ModuleID: 23; ID: 2; XPos: 38; YPos: 31; CodeRef: 0; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 23; ID: 3; XPos: 146; YPos: 31; CodeRef: 3;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 23; ID: 7; XPos: 74; YPos: 31;
    CodeRef: 1; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 23; ID: 10;
    XPos: 110; YPos: 31; CodeRef: 2; InfoFunc: 16; KnobType: ktMedium),
    (ModuleID: 23; ID: 27; XPos: 57; YPos: 52; CodeRef: 4; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 23; ID: 30; XPos: 93; YPos: 52; CodeRef: 5;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 23; ID: 33; XPos: 129; YPos: 52;
    CodeRef: 6; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 23; ID: 36;
    XPos: 165; YPos: 52; CodeRef: 7; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 24; ID: 3; XPos: 165; YPos: 5; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 25; ID: 6; XPos: 54; YPos: 52; CodeRef: 3;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 25; ID: 10; XPos: 121; YPos: 28;
    CodeRef: 5; InfoFunc: 128; KnobType: ktMedium), (ModuleID: 25; ID: 12;
    XPos: 84; YPos: 50; CodeRef: 0; InfoFunc: 61; KnobType: ktMedium),
    (ModuleID: 25; ID: 17; XPos: 177; YPos: 52; CodeRef: 6; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 25; ID: 24; XPos: 176; YPos: 28; CodeRef: 7;
    InfoFunc: 163; KnobType: ktMedium), (ModuleID: 25; ID: 29; XPos: 139;
    YPos: 52; CodeRef: 8; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 26; ID: 1;
    XPos: 18; YPos: 22; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 26; ID: 4; XPos: 121; YPos: 20; CodeRef: 0; InfoFunc: 61;
    KnobType: ktMedium), (ModuleID: 27; ID: 0; XPos: 18; YPos: 22; CodeRef: 4;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 27; ID: 5; XPos: 102; YPos: 18;
    CodeRef: 0; InfoFunc: 61; KnobType: ktBig), (ModuleID: 27; ID: 4; XPos: 130;
    YPos: 14; CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 28;
    ID: 0; XPos: 149; YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 28; ID: 2; XPos: 122; YPos: 7; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 29; ID: 0; XPos: 187; YPos: 5; CodeRef: 0;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 29; ID: 5; XPos: 103; YPos: 5;
    CodeRef: 1; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 29; ID: 7;
    XPos: 80; YPos: 7; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 29; ID: 10; XPos: 164; YPos: 7; CodeRef: 4; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 31; ID: 0; XPos: 129; YPos: 5; CodeRef: 0;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 32; ID: 1; XPos: 148; YPos: 20;
    CodeRef: 1; InfoFunc: 36; KnobType: ktMedium), (ModuleID: 32; ID: 2;
    XPos: 99; YPos: 20; CodeRef: 0; InfoFunc: 36; KnobType: ktMedium),
    (ModuleID: 32; ID: 14; XPos: 24; YPos: 20; CodeRef: 2; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 33; ID: 1; XPos: 158; YPos: 35; CodeRef: 3;
    InfoFunc: 36; KnobType: ktMedium), (ModuleID: 33; ID: 2; XPos: 25; YPos: 35;
    CodeRef: 0; InfoFunc: 36; KnobType: ktMedium), (ModuleID: 33; ID: 3;
    XPos: 68; YPos: 35; CodeRef: 1; InfoFunc: 36; KnobType: ktMedium),
    (ModuleID: 33; ID: 14; XPos: 202; YPos: 35; CodeRef: 4; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 33; ID: 19; XPos: 113; YPos: 35; CodeRef: 2;
    InfoFunc: 184; KnobType: ktMedium), (ModuleID: 34; ID: 0; XPos: 149;
    YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 34; ID: 2;
    XPos: 122; YPos: 7; CodeRef: 1; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 35; ID: 0; XPos: 188; YPos: 18; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 35; ID: 2; XPos: 134; YPos: 18; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 38; ID: 0; XPos: 78; YPos: 7;
    CodeRef: 1; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 38; ID: 4;
    XPos: 167; YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 40; ID: 4; XPos: 39; YPos: 35; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 40; ID: 9; XPos: 64; YPos: 35; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 40; ID: 10; XPos: 89; YPos: 35;
    CodeRef: 2; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 40; ID: 15;
    XPos: 114; YPos: 35; CodeRef: 3; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 40; ID: 16; XPos: 139; YPos: 35; CodeRef: 4; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 40; ID: 21; XPos: 164; YPos: 35; CodeRef: 5;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 40; ID: 25; XPos: 189;
    YPos: 35; CodeRef: 6; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 40;
    ID: 28; XPos: 214; YPos: 35; CodeRef: 7; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 41; ID: 7; XPos: 119; YPos: 5; CodeRef: 0; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 42; ID: 1; XPos: 78; YPos: 7; CodeRef: 1;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 42; ID: 5; XPos: 167; YPos: 5;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 43; ID: 2;
    XPos: 163; YPos: 5; CodeRef: 0; InfoFunc: 16; KnobType: ktMedium),
    (ModuleID: 45; ID: 10; XPos: 164; YPos: 33; CodeRef: 4; InfoFunc: 0;
    KnobType: ktBig), (ModuleID: 45; ID: 11; XPos: 42; YPos: 29; CodeRef: 6;
    InfoFunc: 17; KnobType: ktResetMedium), (ModuleID: 45; ID: 13; XPos: 18;
    YPos: 37; CodeRef: 7; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 45;
    ID: 17; XPos: 70; YPos: 29; CodeRef: 8; InfoFunc: 17;
    KnobType: ktResetMedium), (ModuleID: 45; ID: 20; XPos: 138; YPos: 37;
    CodeRef: 5; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 45; ID: 22;
    XPos: 215; YPos: 14; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 46; ID: 17; XPos: 64; YPos: 35; CodeRef: 1; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 46; ID: 18; XPos: 150; YPos: 35; CodeRef: 4;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 46; ID: 24; XPos: 107;
    YPos: 35; CodeRef: 2; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 47;
    ID: 1; XPos: 101; YPos: 7; CodeRef: 0; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 47; ID: 3; XPos: 140; YPos: 3; CodeRef: 1; InfoFunc: 17;
    KnobType: ktReset), (ModuleID: 48; ID: 0; XPos: 21; YPos: 22; CodeRef: 6;
    InfoFunc: 19; KnobType: ktReset), (ModuleID: 48; ID: 1; XPos: 20; YPos: 50;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 48; ID: 5;
    XPos: 53; YPos: 22; CodeRef: 7; InfoFunc: 19; KnobType: ktReset),
    (ModuleID: 48; ID: 6; XPos: 52; YPos: 50; CodeRef: 1; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 48; ID: 10; XPos: 85; YPos: 22; CodeRef: 8;
    InfoFunc: 19; KnobType: ktReset), (ModuleID: 48; ID: 11; XPos: 84; YPos: 50;
    CodeRef: 2; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 48; ID: 15;
    XPos: 117; YPos: 22; CodeRef: 9; InfoFunc: 19; KnobType: ktReset),
    (ModuleID: 48; ID: 16; XPos: 116; YPos: 50; CodeRef: 3; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 48; ID: 20; XPos: 149; YPos: 22;
    CodeRef: 10; InfoFunc: 19; KnobType: ktReset), (ModuleID: 48; ID: 21;
    XPos: 148; YPos: 50; CodeRef: 4; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 48; ID: 25; XPos: 181; YPos: 22; CodeRef: 11; InfoFunc: 19;
    KnobType: ktReset), (ModuleID: 48; ID: 26; XPos: 180; YPos: 50; CodeRef: 5;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 48; ID: 34; XPos: 218;
    YPos: 14; CodeRef: 12; InfoFunc: 0; KnobType: ktBig), (ModuleID: 49; ID: 0;
    XPos: 18; YPos: 32; CodeRef: 1; InfoFunc: 191; KnobType: ktReset),
    (ModuleID: 49; ID: 8; XPos: 97; YPos: 33; CodeRef: 0; InfoFunc: 123;
    KnobType: ktBig), (ModuleID: 49; ID: 11; XPos: 150; YPos: 35; CodeRef: 4;
    InfoFunc: 124; KnobType: ktMedium), (ModuleID: 50; ID: 5; XPos: 163;
    YPos: 5; CodeRef: 0; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 51;
    ID: 1; XPos: 18; YPos: 47; CodeRef: 1; InfoFunc: 191; KnobType: ktReset),
    (ModuleID: 51; ID: 6; XPos: 59; YPos: 52; CodeRef: 7; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 51; ID: 10; XPos: 79; YPos: 14; CodeRef: 0;
    InfoFunc: 123; KnobType: ktBig), (ModuleID: 51; ID: 16; XPos: 137; YPos: 15;
    CodeRef: 4; InfoFunc: 124; KnobType: ktMedium), (ModuleID: 51; ID: 27;
    XPos: 138; YPos: 52; CodeRef: 9; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 52; ID: 12; XPos: 49; YPos: 65; CodeRef: 0; InfoFunc: 16;
    KnobType: ktMedium), (ModuleID: 52; ID: 15; XPos: 109; YPos: 65; CodeRef: 1;
    InfoFunc: 16; KnobType: ktMedium), (ModuleID: 52; ID: 18; XPos: 169;
    YPos: 65; CodeRef: 2; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 52;
    ID: 21; XPos: 229; YPos: 65; CodeRef: 3; InfoFunc: 16; KnobType: ktMedium),
    (ModuleID: 52; ID: 24; XPos: 22; YPos: 65; CodeRef: 4; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 52; ID: 27; XPos: 80; YPos: 65; CodeRef: 5;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 52; ID: 30; XPos: 140;
    YPos: 65; CodeRef: 6; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 52;
    ID: 33; XPos: 200; YPos: 65; CodeRef: 7; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 54; ID: 10; XPos: 53; YPos: 18; CodeRef: 0; InfoFunc: 123;
    KnobType: ktBig), (ModuleID: 54; ID: 16; XPos: 110; YPos: 20; CodeRef: 1;
    InfoFunc: 124; KnobType: ktMedium), (ModuleID: 55; ID: 6; XPos: 119;
    YPos: 5; CodeRef: 0; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 56;
    ID: 2; XPos: 38; YPos: 49; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 56; ID: 6; XPos: 84; YPos: 18; CodeRef: 0; InfoFunc: 61;
    KnobType: ktBig), (ModuleID: 56; ID: 9; XPos: 112; YPos: 14; CodeRef: 1;
    InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 56; ID: 10; XPos: 185;
    YPos: 49; CodeRef: 5; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 56;
    ID: 16; XPos: 185; YPos: 15; CodeRef: 6; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 56; ID: 14; XPos: 147; YPos: 20; CodeRef: 8; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 57; ID: 4; XPos: 180; YPos: 5; CodeRef: 1;
    InfoFunc: 179; KnobType: ktMedium), (ModuleID: 58; ID: 0; XPos: 34;
    YPos: 43; CodeRef: 0; InfoFunc: 22; KnobType: ktMedium), (ModuleID: 58;
    ID: 1; XPos: 34; YPos: 95; CodeRef: 1; InfoFunc: 23; KnobType: ktMedium),
    (ModuleID: 58; ID: 2; XPos: 63; YPos: 43; CodeRef: 2; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 58; ID: 3; XPos: 63; YPos: 95; CodeRef: 3;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 58; ID: 4; XPos: 92; YPos: 43;
    CodeRef: 4; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 58; ID: 5;
    XPos: 92; YPos: 95; CodeRef: 5; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 58; ID: 6; XPos: 126; YPos: 48; CodeRef: 6; InfoFunc: 21;
    KnobType: ktMedium), (ModuleID: 58; ID: 7; XPos: 153; YPos: 48; CodeRef: 7;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 58; ID: 8; XPos: 180; YPos: 48;
    CodeRef: 8; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 58; ID: 9;
    XPos: 207; YPos: 48; CodeRef: 9; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 58; ID: 11; XPos: 126; YPos: 95; CodeRef: 11; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 58; ID: 12; XPos: 153; YPos: 95;
    CodeRef: 12; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 58; ID: 13;
    XPos: 184; YPos: 95; CodeRef: 13; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 58; ID: 14; XPos: 211; YPos: 95; CodeRef: 14; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 59; ID: 1; XPos: 173; YPos: 5; CodeRef: 0;
    InfoFunc: 17; KnobType: ktMedium), (ModuleID: 60; ID: 27; XPos: 190;
    YPos: 20; CodeRef: 0; InfoFunc: 56; KnobType: ktMedium), (ModuleID: 61;
    ID: 1; XPos: 92; YPos: 7; CodeRef: 0; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 61; ID: 5; XPos: 150; YPos: 5; CodeRef: 1; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 62; ID: 2; XPos: 149; YPos: 5; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 62; ID: 1; XPos: 122; YPos: 7;
    CodeRef: 0; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 63; ID: 0;
    XPos: 139; YPos: 14; CodeRef: 0; InfoFunc: 209; KnobType: ktResetMedium),
    (ModuleID: 63; ID: 7; XPos: 25; YPos: 22; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 66; ID: 5; XPos: 131; YPos: 5; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 66; ID: 9; XPos: 216; YPos: 5;
    CodeRef: 3; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 68; ID: 3;
    XPos: 150; YPos: 6; CodeRef: 0; InfoFunc: 45; KnobType: ktBig),
    (ModuleID: 68; ID: 21; XPos: 57; YPos: 35; CodeRef: 4; InfoFunc: 190;
    KnobType: ktMedium), (ModuleID: 71; ID: 4; XPos: 115; YPos: 5; CodeRef: 0;
    InfoFunc: 129; KnobType: ktMedium), (ModuleID: 71; ID: 6; XPos: 183;
    YPos: 5; CodeRef: 1; InfoFunc: 130; KnobType: ktMedium), (ModuleID: 72;
    ID: 1; XPos: 148; YPos: 5; CodeRef: 0; InfoFunc: 69; KnobType: ktMedium),
    (ModuleID: 74; ID: 1; XPos: 122; YPos: 7; CodeRef: 0; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 74; ID: 3; XPos: 149; YPos: 5; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 75; ID: 0; XPos: 114; YPos: 5;
    CodeRef: 0; InfoFunc: 35; KnobType: ktMedium), (ModuleID: 81; ID: 0;
    XPos: 140; YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 84; ID: 17; XPos: 93; YPos: 20; CodeRef: 1; InfoFunc: 28;
    KnobType: ktMedium), (ModuleID: 84; ID: 18; XPos: 142; YPos: 20; CodeRef: 3;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 85; ID: 2; XPos: 105; YPos: 5;
    CodeRef: 0; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 85; ID: 5;
    XPos: 157; YPos: 5; CodeRef: 1; InfoFunc: 16; KnobType: ktMedium),
    (ModuleID: 87; ID: 0; XPos: 77; YPos: 7; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 87; ID: 4; XPos: 145; YPos: 5; CodeRef: 0;
    InfoFunc: 123; KnobType: ktMedium), (ModuleID: 89; ID: 1; XPos: 106;
    YPos: 20; CodeRef: 0; InfoFunc: 215; KnobType: ktMedium), (ModuleID: 89;
    ID: 0; XPos: 152; YPos: 20; CodeRef: 1; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 89; ID: 10; XPos: 199; YPos: 20; CodeRef: 2; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 92; ID: 0; XPos: 18; YPos: 32; CodeRef: 1;
    InfoFunc: 191; KnobType: ktReset), (ModuleID: 92; ID: 6; XPos: 97; YPos: 33;
    CodeRef: 0; InfoFunc: 123; KnobType: ktBig), (ModuleID: 92; ID: 10;
    XPos: 150; YPos: 35; CodeRef: 3; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 94; ID: 0; XPos: 90; YPos: 20; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 94; ID: 1; XPos: 138; YPos: 20; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 96; ID: 3; XPos: 161; YPos: 5;
    CodeRef: 0; InfoFunc: 61; KnobType: ktMedium), (ModuleID: 96; ID: 5;
    XPos: 185; YPos: 3; CodeRef: 1; InfoFunc: 59; KnobType: ktReset),
    (ModuleID: 97; ID: 1; XPos: 18; YPos: 22; CodeRef: 3; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 97; ID: 4; XPos: 98; YPos: 18; CodeRef: 0;
    InfoFunc: 61; KnobType: ktBig), (ModuleID: 97; ID: 5; XPos: 126; YPos: 14;
    CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 98; ID: 4;
    XPos: 25; YPos: 22; CodeRef: 1; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 98; ID: 8; XPos: 166; YPos: 20; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 102; ID: 5; XPos: 91; YPos: 21; CodeRef: 5;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 102; ID: 7; XPos: 95; YPos: 52;
    CodeRef: 2; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 102; ID: 12;
    XPos: 48; YPos: 41; CodeRef: 1; InfoFunc: 39; KnobType: ktBig),
    (ModuleID: 102; ID: 14; XPos: 18; YPos: 52; CodeRef: 0; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 102; ID: 28; XPos: 125; YPos: 15; CodeRef: 3;
    InfoFunc: 17; KnobType: ktResetMedium), (ModuleID: 102; ID: 30; XPos: 203;
    YPos: 52; CodeRef: 7; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 102;
    ID: 19; XPos: 136; YPos: 52; CodeRef: 8; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 103; ID: 1; XPos: 143; YPos: 35; CodeRef: 2; InfoFunc: 76;
    KnobType: ktMedium), (ModuleID: 103; ID: 2; XPos: 49; YPos: 35; CodeRef: 0;
    InfoFunc: 38; KnobType: ktMedium), (ModuleID: 103; ID: 3; XPos: 96;
    YPos: 35; CodeRef: 1; InfoFunc: 36; KnobType: ktMedium), (ModuleID: 103;
    ID: 14; XPos: 203; YPos: 35; CodeRef: 4; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 106; ID: 0; XPos: 195; YPos: 18; CodeRef: 6; InfoFunc: 0;
    KnobType: ktBig), (ModuleID: 106; ID: 14; XPos: 173; YPos: 22; CodeRef: 5;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 106; ID: 5; XPos: 18; YPos: 22;
    CodeRef: 3; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 106; ID: 8;
    XPos: 102; YPos: 18; CodeRef: 0; InfoFunc: 61; KnobType: ktBig),
    (ModuleID: 106; ID: 9; XPos: 130; YPos: 14; CodeRef: 1; InfoFunc: 59;
    KnobType: ktResetMedium), (ModuleID: 112; ID: 0; XPos: 142; YPos: 5;
    CodeRef: 0; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 113; ID: 0;
    XPos: 140; YPos: 5; CodeRef: 0; InfoFunc: 53; KnobType: ktMedium),
    (ModuleID: 113; ID: 9; XPos: 101; YPos: 7; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 114; ID: 1; XPos: 140; YPos: 5; CodeRef: 0;
    InfoFunc: 52; KnobType: ktMedium), (ModuleID: 114; ID: 8; XPos: 101;
    YPos: 7; CodeRef: 1; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 115; ID: 2;
    XPos: 154; YPos: 20; CodeRef: 2; InfoFunc: 70; KnobType: ktMedium),
    (ModuleID: 115; ID: 3; XPos: 90; YPos: 20; CodeRef: 0; InfoFunc: 70;
    KnobType: ktMedium), (ModuleID: 117; ID: 7; XPos: 105; YPos: 22; CodeRef: 0;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 117; ID: 9; XPos: 144; YPos: 17;
    CodeRef: 1; InfoFunc: 55; KnobType: ktReset), (ModuleID: 118; ID: 12;
    XPos: 25; YPos: 22; CodeRef: 2; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 118; ID: 14; XPos: 131; YPos: 20; CodeRef: 1; InfoFunc: 88;
    KnobType: ktMedium), (ModuleID: 119; ID: 14; XPos: 55; YPos: 50; CodeRef: 2;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 119; ID: 15; XPos: 159;
    YPos: 50; CodeRef: 5; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 119;
    ID: 21; XPos: 91; YPos: 50; CodeRef: 3; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 119; ID: 24; XPos: 126; YPos: 50; CodeRef: 4; InfoFunc: 16;
    KnobType: ktMedium), (ModuleID: 119; ID: 27; XPos: 193; YPos: 50;
    CodeRef: 6; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 119; ID: 30;
    XPos: 227; YPos: 50; CodeRef: 7; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 121; ID: 17; XPos: 25; YPos: 40; CodeRef: 0; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 121; ID: 27; XPos: 37; YPos: 40;
    CodeRef: 1; InfoFunc: 13; KnobType: ktSeqSlider), (ModuleID: 121; ID: 60;
    XPos: 49; YPos: 40; CodeRef: 2; InfoFunc: 13; KnobType: ktSeqSlider),
    (ModuleID: 121; ID: 61; XPos: 61; YPos: 40; CodeRef: 3; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 121; ID: 84; XPos: 73; YPos: 40;
    CodeRef: 4; InfoFunc: 13; KnobType: ktSeqSlider), (ModuleID: 121; ID: 85;
    XPos: 85; YPos: 40; CodeRef: 5; InfoFunc: 13; KnobType: ktSeqSlider),
    (ModuleID: 121; ID: 86; XPos: 97; YPos: 40; CodeRef: 6; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 121; ID: 88; XPos: 109; YPos: 40;
    CodeRef: 7; InfoFunc: 13; KnobType: ktSeqSlider), (ModuleID: 121; ID: 89;
    XPos: 121; YPos: 40; CodeRef: 8; InfoFunc: 13; KnobType: ktSeqSlider),
    (ModuleID: 121; ID: 90; XPos: 133; YPos: 40; CodeRef: 9; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 121; ID: 91; XPos: 145; YPos: 40;
    CodeRef: 10; InfoFunc: 13; KnobType: ktSeqSlider), (ModuleID: 121; ID: 92;
    XPos: 157; YPos: 40; CodeRef: 11; InfoFunc: 13; KnobType: ktSeqSlider),
    (ModuleID: 121; ID: 93; XPos: 169; YPos: 40; CodeRef: 12; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 121; ID: 94; XPos: 181; YPos: 40;
    CodeRef: 13; InfoFunc: 13; KnobType: ktSeqSlider), (ModuleID: 121; ID: 95;
    XPos: 193; YPos: 40; CodeRef: 14; InfoFunc: 13; KnobType: ktSeqSlider),
    (ModuleID: 121; ID: 96; XPos: 205; YPos: 40; CodeRef: 15; InfoFunc: 13;
    KnobType: ktSeqSlider), (ModuleID: 123; ID: 1; XPos: 62; YPos: 16;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 123; ID: 3;
    XPos: 110; YPos: 16; CodeRef: 1; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 123; ID: 6; XPos: 158; YPos: 16; CodeRef: 2; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 123; ID: 9; XPos: 206; YPos: 16; CodeRef: 3;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 125; ID: 0; XPos: 139; YPos: 5;
    CodeRef: 1; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 125; ID: 4;
    XPos: 111; YPos: 7; CodeRef: 0; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 134; ID: 1; XPos: 77; YPos: 7; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 134; ID: 5; XPos: 145; YPos: 5; CodeRef: 0;
    InfoFunc: 123; KnobType: ktMedium), (ModuleID: 140; ID: 0; XPos: 33;
    YPos: 17; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 140;
    ID: 5; XPos: 87; YPos: 17; CodeRef: 1; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 140; ID: 11; XPos: 141; YPos: 17; CodeRef: 2; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 140; ID: 22; XPos: 195; YPos: 17;
    CodeRef: 3; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 141; ID: 4;
    XPos: 180; YPos: 5; CodeRef: 1; InfoFunc: 179; KnobType: ktMedium),
    (ModuleID: 142; ID: 5; XPos: 180; YPos: 5; CodeRef: 0; InfoFunc: 179;
    KnobType: ktMedium), (ModuleID: 143; ID: 3; XPos: 180; YPos: 5; CodeRef: 1;
    InfoFunc: 13; KnobType: ktMedium), (ModuleID: 143; ID: 6; XPos: 105;
    YPos: 5; CodeRef: 0; InfoFunc: 179; KnobType: ktMedium), (ModuleID: 146;
    ID: 26; XPos: 28; YPos: 40; CodeRef: 0; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 146; ID: 28; XPos: 40; YPos: 40; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 146; ID: 29; XPos: 52; YPos: 40; CodeRef: 2;
    InfoFunc: 0; KnobType: ktSlider), (ModuleID: 146; ID: 30; XPos: 64;
    YPos: 40; CodeRef: 3; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 146;
    ID: 31; XPos: 76; YPos: 40; CodeRef: 4; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 146; ID: 32; XPos: 88; YPos: 40; CodeRef: 5; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 146; ID: 33; XPos: 100; YPos: 40;
    CodeRef: 6; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 146; ID: 34;
    XPos: 112; YPos: 40; CodeRef: 7; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 146; ID: 35; XPos: 124; YPos: 40; CodeRef: 8; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 146; ID: 36; XPos: 136; YPos: 40;
    CodeRef: 9; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 146; ID: 37;
    XPos: 148; YPos: 40; CodeRef: 10; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 146; ID: 38; XPos: 160; YPos: 40; CodeRef: 11; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 146; ID: 39; XPos: 172; YPos: 40;
    CodeRef: 12; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 146; ID: 40;
    XPos: 184; YPos: 40; CodeRef: 13; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 146; ID: 41; XPos: 196; YPos: 40; CodeRef: 14; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 146; ID: 43; XPos: 208; YPos: 40;
    CodeRef: 15; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 148; ID: 1;
    XPos: 108; YPos: 5; CodeRef: 0; InfoFunc: 13; KnobType: ktMedium),
    (ModuleID: 150; ID: 4; XPos: 10; YPos: 50; CodeRef: 0; InfoFunc: 176;
    KnobType: ktMedium), (ModuleID: 150; ID: 5; XPos: 43; YPos: 50; CodeRef: 1;
    InfoFunc: 177; KnobType: ktMedium), (ModuleID: 150; ID: 6; XPos: 79;
    YPos: 50; CodeRef: 2; InfoFunc: 174; KnobType: ktMedium), (ModuleID: 150;
    ID: 7; XPos: 115; YPos: 50; CodeRef: 3; InfoFunc: 175; KnobType: ktMedium),
    (ModuleID: 150; ID: 8; XPos: 152; YPos: 50; CodeRef: 4; InfoFunc: 178;
    KnobType: ktMedium), (ModuleID: 152; ID: 16; XPos: 114; YPos: 5; CodeRef: 0;
    InfoFunc: 132; KnobType: ktMedium), (ModuleID: 154; ID: 26; XPos: 28;
    YPos: 39; CodeRef: 0; InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154;
    ID: 28; XPos: 40; YPos: 39; CodeRef: 1; InfoFunc: 500; KnobType: ktSlider),
    (ModuleID: 154; ID: 29; XPos: 52; YPos: 39; CodeRef: 2; InfoFunc: 500;
    KnobType: ktSlider), (ModuleID: 154; ID: 30; XPos: 64; YPos: 39; CodeRef: 3;
    InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154; ID: 31; XPos: 76;
    YPos: 39; CodeRef: 4; InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154;
    ID: 32; XPos: 88; YPos: 39; CodeRef: 5; InfoFunc: 500; KnobType: ktSlider),
    (ModuleID: 154; ID: 33; XPos: 100; YPos: 39; CodeRef: 6; InfoFunc: 500;
    KnobType: ktSlider), (ModuleID: 154; ID: 34; XPos: 112; YPos: 39;
    CodeRef: 7; InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154; ID: 35;
    XPos: 124; YPos: 39; CodeRef: 8; InfoFunc: 500; KnobType: ktSlider),
    (ModuleID: 154; ID: 36; XPos: 136; YPos: 39; CodeRef: 9; InfoFunc: 500;
    KnobType: ktSlider), (ModuleID: 154; ID: 37; XPos: 148; YPos: 39;
    CodeRef: 10; InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154; ID: 38;
    XPos: 160; YPos: 39; CodeRef: 11; InfoFunc: 500; KnobType: ktSlider),
    (ModuleID: 154; ID: 39; XPos: 172; YPos: 39; CodeRef: 12; InfoFunc: 500;
    KnobType: ktSlider), (ModuleID: 154; ID: 40; XPos: 184; YPos: 39;
    CodeRef: 13; InfoFunc: 500; KnobType: ktSlider), (ModuleID: 154; ID: 41;
    XPos: 196; YPos: 39; CodeRef: 14; InfoFunc: 500; KnobType: ktSlider),
    (ModuleID: 154; ID: 43; XPos: 208; YPos: 39; CodeRef: 15; InfoFunc: 500;
    KnobType: ktSlider), (ModuleID: 156; ID: 0; XPos: 120; YPos: 5; CodeRef: 0;
    InfoFunc: 13; KnobType: ktMedium), (ModuleID: 158; ID: 5; XPos: 155;
    YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 161;
    ID: 0; XPos: 12; YPos: 53; CodeRef: 0; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 161; ID: 18; XPos: 41; YPos: 53; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 161; ID: 19; XPos: 70; YPos: 53; CodeRef: 2;
    InfoFunc: 0; KnobType: ktSlider), (ModuleID: 161; ID: 20; XPos: 99;
    YPos: 53; CodeRef: 3; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 161;
    ID: 21; XPos: 128; YPos: 53; CodeRef: 4; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 161; ID: 22; XPos: 157; YPos: 53; CodeRef: 5; InfoFunc: 0;
    KnobType: ktSlider), (ModuleID: 161; ID: 23; XPos: 186; YPos: 53;
    CodeRef: 6; InfoFunc: 0; KnobType: ktSlider), (ModuleID: 161; ID: 24;
    XPos: 215; YPos: 53; CodeRef: 7; InfoFunc: 0; KnobType: ktSlider),
    (ModuleID: 162; ID: 4; XPos: 81; YPos: 35; CodeRef: 0; InfoFunc: 173;
    KnobType: ktMedium), (ModuleID: 162; ID: 6; XPos: 18; YPos: 37; CodeRef: 1;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 162; ID: 11; XPos: 204;
    YPos: 37; CodeRef: 6; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 162;
    ID: 13; XPos: 125; YPos: 5; CodeRef: 3; InfoFunc: 17;
    KnobType: ktResetMedium), (ModuleID: 162; ID: 21; XPos: 136; YPos: 37;
    CodeRef: 4; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 163; ID: 3;
    XPos: 18; YPos: 52; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 163; ID: 12; XPos: 188; YPos: 25; CodeRef: 7; InfoFunc: 126;
    KnobType: ktMedium), (ModuleID: 163; ID: 5; XPos: 50; YPos: 48; CodeRef: 0;
    InfoFunc: 61; KnobType: ktBig), (ModuleID: 163; ID: 6; XPos: 78; YPos: 44;
    CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium), (ModuleID: 163; ID: 8;
    XPos: 129; YPos: 52; CodeRef: 5; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 163; ID: 14; XPos: 194; YPos: 52; CodeRef: 8; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 164; ID: 3; XPos: 18; YPos: 52; CodeRef: 3;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 164; ID: 12; XPos: 161;
    YPos: 50; CodeRef: 8; InfoFunc: 185; KnobType: ktMedium), (ModuleID: 164;
    ID: 5; XPos: 50; YPos: 48; CodeRef: 0; InfoFunc: 61; KnobType: ktBig),
    (ModuleID: 164; ID: 6; XPos: 78; YPos: 44; CodeRef: 1; InfoFunc: 59;
    KnobType: ktResetMedium), (ModuleID: 164; ID: 8; XPos: 138; YPos: 15;
    CodeRef: 6; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 164; ID: 10;
    XPos: 189; YPos: 13; CodeRef: 5; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 164; ID: 16; XPos: 189; YPos: 50; CodeRef: 7; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 164; ID: 26; XPos: 225; YPos: 13;
    CodeRef: 9; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 164; ID: 29;
    XPos: 138; YPos: 52; CodeRef: 12; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 164; ID: 32; XPos: 161; YPos: 13; CodeRef: 11; InfoFunc: 126;
    KnobType: ktMedium), (ModuleID: 167; ID: 0; XPos: 139; YPos: 14; CodeRef: 0;
    InfoFunc: 61; KnobType: ktResetMedium), (ModuleID: 167; ID: 5; XPos: 166;
    YPos: 14; CodeRef: 1; InfoFunc: 59; KnobType: ktResetMedium),
    (ModuleID: 167; ID: 7; XPos: 25; YPos: 22; CodeRef: 2; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 169; ID: 17; XPos: 60; YPos: 31; CodeRef: 0;
    InfoFunc: 28; KnobType: ktMedium), (ModuleID: 169; ID: 18; XPos: 146;
    YPos: 31; CodeRef: 2; InfoFunc: 28; KnobType: ktMedium), (ModuleID: 169;
    ID: 24; XPos: 103; YPos: 31; CodeRef: 1; InfoFunc: 28; KnobType: ktMedium),
    (ModuleID: 169; ID: 5; XPos: 80; YPos: 52; CodeRef: 3; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 169; ID: 20; XPos: 124; YPos: 52; CodeRef: 4;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 169; ID: 28; XPos: 167;
    YPos: 52; CodeRef: 5; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 172;
    ID: 3; XPos: 182; YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 173; ID: 2; XPos: 182; YPos: 5; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 173; ID: 5; XPos: 115; YPos: 7; CodeRef: 1;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 174; ID: 3; XPos: 133; YPos: 4;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 174; ID: 4;
    XPos: 107; YPos: 22; CodeRef: 1; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 174; ID: 16; XPos: 208; YPos: 4; CodeRef: 2; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 174; ID: 19; XPos: 182; YPos: 22;
    CodeRef: 3; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 175; ID: 4;
    XPos: 48; YPos: 31; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 175; ID: 1; XPos: 69; YPos: 52; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 175; ID: 8; XPos: 95; YPos: 31; CodeRef: 2;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 175; ID: 12; XPos: 115;
    YPos: 52; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 175;
    ID: 14; XPos: 142; YPos: 31; CodeRef: 4; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 175; ID: 18; XPos: 163; YPos: 52; CodeRef: 5; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 175; ID: 20; XPos: 189; YPos: 31; CodeRef: 6;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 175; ID: 24; XPos: 210;
    YPos: 52; CodeRef: 7; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 176;
    ID: 2; XPos: 95; YPos: 20; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 176; ID: 3; XPos: 126; YPos: 20; CodeRef: 1; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 176; ID: 4; XPos: 164; YPos: 20; CodeRef: 2;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 176; ID: 5; XPos: 203;
    YPos: 20; CodeRef: 3; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 177;
    ID: 6; XPos: 95; YPos: 21; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 177; ID: 7; XPos: 126; YPos: 12; CodeRef: 1; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 177; ID: 8; XPos: 203; YPos: 12; CodeRef: 3;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 177; ID: 16; XPos: 134;
    YPos: 37; CodeRef: 5; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 177;
    ID: 19; XPos: 170; YPos: 37; CodeRef: 2; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 177; ID: 21; XPos: 211; YPos: 37; CodeRef: 6; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 177; ID: 5; XPos: 170; YPos: 14; CodeRef: 8;
    InfoFunc: 0; KnobType: ktSmall), (ModuleID: 178; ID: 5; XPos: 182; YPos: 5;
    CodeRef: 0; InfoFunc: 2; KnobType: ktMedium), (ModuleID: 180; ID: 0;
    XPos: 38; YPos: 99; CodeRef: 8; InfoFunc: 179; KnobType: ktMedium),
    (ModuleID: 180; ID: 1; XPos: 64; YPos: 99; CodeRef: 9; InfoFunc: 179;
    KnobType: ktMedium), (ModuleID: 180; ID: 2; XPos: 93; YPos: 99; CodeRef: 10;
    InfoFunc: 179; KnobType: ktMedium), (ModuleID: 180; ID: 3; XPos: 119;
    YPos: 99; CodeRef: 11; InfoFunc: 179; KnobType: ktMedium), (ModuleID: 180;
    ID: 4; XPos: 148; YPos: 99; CodeRef: 12; InfoFunc: 179; KnobType: ktMedium),
    (ModuleID: 180; ID: 5; XPos: 174; YPos: 99; CodeRef: 13; InfoFunc: 179;
    KnobType: ktMedium), (ModuleID: 180; ID: 6; XPos: 203; YPos: 99;
    CodeRef: 14; InfoFunc: 179; KnobType: ktMedium), (ModuleID: 180; ID: 7;
    XPos: 229; YPos: 99; CodeRef: 15; InfoFunc: 179; KnobType: ktMedium),
    (ModuleID: 180; ID: 34; XPos: 146; YPos: 15; CodeRef: 3; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 180; ID: 35; XPos: 176; YPos: 15;
    CodeRef: 4; InfoFunc: 179; KnobType: ktMedium), (ModuleID: 180; ID: 58;
    XPos: 211; YPos: 155; CodeRef: 22; InfoFunc: 179; KnobType: ktMedium),
    (ModuleID: 181; ID: 10; XPos: 152; YPos: 4; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 182; ID: 1; XPos: 97; YPos: 23; CodeRef: 2;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 182; ID: 3; XPos: 69; YPos: 23;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 182; ID: 5;
    XPos: 125; YPos: 23; CodeRef: 4; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 182; ID: 7; XPos: 97; YPos: 50; CodeRef: 3; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 182; ID: 9; XPos: 69; YPos: 50; CodeRef: 1;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 182; ID: 11; XPos: 125;
    YPos: 50; CodeRef: 5; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 182;
    ID: 15; XPos: 210; YPos: 25; CodeRef: 8; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 182; ID: 21; XPos: 153; YPos: 25; CodeRef: 7; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 182; ID: 16; XPos: 179; YPos: 25;
    CodeRef: 10; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 183; ID: 2;
    XPos: 102; YPos: 18; CodeRef: 0; InfoFunc: 61; KnobType: ktBig),
    (ModuleID: 183; ID: 3; XPos: 130; YPos: 14; CodeRef: 1; InfoFunc: 59;
    KnobType: ktResetMedium), (ModuleID: 183; ID: 7; XPos: 196; YPos: 22;
    CodeRef: 4; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 183; ID: 19;
    XPos: 18; YPos: 22; CodeRef: 6; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 184; ID: 0; XPos: 158; YPos: 5; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 185; ID: 0; XPos: 156; YPos: 5; CodeRef: 0;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 188; ID: 5; XPos: 163; YPos: 5;
    CodeRef: 0; InfoFunc: 16; KnobType: ktMedium), (ModuleID: 189; ID: 17;
    XPos: 187; YPos: 20; CodeRef: 0; InfoFunc: 220; KnobType: ktMedium),
    (ModuleID: 189; ID: 18; XPos: 48; YPos: 20; CodeRef: 1; InfoFunc: 159;
    KnobType: ktMedium), (ModuleID: 189; ID: 3; XPos: 113; YPos: 20; CodeRef: 2;
    InfoFunc: 160; KnobType: ktMedium), (ModuleID: 190; ID: 0; XPos: 59;
    YPos: 37; CodeRef: 1; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 190;
    ID: 5; XPos: 87; YPos: 35; CodeRef: 0; InfoFunc: 61; KnobType: ktMedium),
    (ModuleID: 190; ID: 23; XPos: 154; YPos: 37; CodeRef: 9; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 190; ID: 25; XPos: 176; YPos: 35; CodeRef: 6;
    InfoFunc: 163; KnobType: ktMedium), (ModuleID: 192; ID: 1; XPos: 124;
    YPos: 5; CodeRef: 1; InfoFunc: 216; KnobType: ktMedium), (ModuleID: 192;
    ID: 3; XPos: 182; YPos: 5; CodeRef: 2; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 194; ID: 5; XPos: 134; YPos: 5; CodeRef: 0; InfoFunc: 0;
    KnobType: ktMedium), (ModuleID: 194; ID: 9; XPos: 216; YPos: 5; CodeRef: 2;
    InfoFunc: 0; KnobType: ktMedium), (ModuleID: 195; ID: 4; XPos: 99; YPos: 5;
    CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 196; ID: 11;
    XPos: 121; YPos: 18; CodeRef: 0; InfoFunc: 61; KnobType: ktBig),
    (ModuleID: 196; ID: 12; XPos: 149; YPos: 14; CodeRef: 1; InfoFunc: 59;
    KnobType: ktResetMedium), (ModuleID: 196; ID: 13; XPos: 173; YPos: 20;
    CodeRef: 5; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 196; ID: 14;
    XPos: 197; YPos: 20; CodeRef: 6; InfoFunc: 0; KnobType: ktMedium),
    (ModuleID: 196; ID: 2; XPos: 18; YPos: 22; CodeRef: 4; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 198; ID: 12; XPos: 133; YPos: 5; CodeRef: 0;
    InfoFunc: 220; KnobType: ktMedium), (ModuleID: 200; ID: 3; XPos: 143;
    YPos: 5; CodeRef: 0; InfoFunc: 0; KnobType: ktMedium), (ModuleID: 202;
    ID: 1; XPos: 18; YPos: 22; CodeRef: 3; InfoFunc: 0; KnobType: ktSmall),
    (ModuleID: 202; ID: 4; XPos: 125; YPos: 20; CodeRef: 0; InfoFunc: 61;
    KnobType: ktMedium), (ModuleID: 202; ID: 5; XPos: 181; YPos: 20; CodeRef: 4;
    InfoFunc: 205; KnobType: ktMedium), (ModuleID: 204; ID: 2; XPos: 198;
    YPos: 5; CodeRef: 0; InfoFunc: 205; KnobType: ktMedium), (ModuleID: 205;
    ID: 3; XPos: 190; YPos: 5; CodeRef: 0; InfoFunc: 212; KnobType: ktMedium),
    (ModuleID: 205; ID: 16; XPos: 132; YPos: 7; CodeRef: 1; InfoFunc: 0;
    KnobType: ktSmall), (ModuleID: 206; ID: 3; XPos: 192; YPos: 20; CodeRef: 0;
    InfoFunc: 205; KnobType: ktMedium), (ModuleID: 206; ID: 24; XPos: 165;
    YPos: 22; CodeRef: 4; InfoFunc: 0; KnobType: ktSmall), (ModuleID: 208;
    ID: 7; XPos: 155; YPos: 20; CodeRef: 2; InfoFunc: 205; KnobType: ktMedium),
    (ModuleID: 208; ID: 28; XPos: 131; YPos: 22; CodeRef: 4; InfoFunc: 0;
    KnobType: ktSmall));

  LineDefs: array [0 .. 305] of TG2LineDef = ((ModuleID: 1; ID: 13; XPos: 188;
    YPos: 7; ZPos: 0; Length: 62; Orientation: otHorizontal; LineWidth: lwThin),
    (ModuleID: 1; ID: 14; XPos: 140; YPos: 7; ZPos: 0; Length: 10;
    Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 5; ID: 2;
    XPos: 133; YPos: 14; ZPos: 0; Length: 18; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 5; ID: 8; XPos: 105; YPos: 14; ZPos: 0;
    Length: 14; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 5;
    ID: 5; XPos: 226; YPos: 14; ZPos: 0; Length: 18; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 5; ID: 11; XPos: 198; YPos: 14; ZPos: 0;
    Length: 14; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 7;
    ID: 4; XPos: 8; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 7; ID: 9; XPos: 118; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 7;
    ID: 15; XPos: 183; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 7; ID: 10; XPos: 221; YPos: 21; ZPos: 0;
    Length: 26; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 7;
    ID: 23; XPos: 210; YPos: 36; ZPos: 0; Length: 14; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 7; ID: 30; XPos: 221; YPos: 22; ZPos: 0;
    Length: 15; Orientation: otVertical; LineWidth: lwThick), (ModuleID: 8;
    ID: 23; XPos: 8; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 8; ID: 14; XPos: 140; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 8;
    ID: 21; XPos: 190; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 9; ID: 8; XPos: 184; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 9;
    ID: 20; XPos: 8; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 12; ID: 8; XPos: 225; YPos: 11; ZPos: 0;
    Length: 20; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 12;
    ID: 24; XPos: 245; YPos: 12; ZPos: 0; Length: 20; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 12; ID: 4; XPos: 196; YPos: 34; ZPos: 0;
    Length: 16; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 13;
    ID: 3; XPos: 8; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 13; ID: 26; XPos: 245; YPos: 27; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 13;
    ID: 13; XPos: 240; YPos: 17; ZPos: 0; Length: 11; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 13; ID: 14; XPos: 240; YPos: 27; ZPos: 0;
    Length: 11; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 13;
    ID: 17; XPos: 245; YPos: 8; ZPos: 0; Length: 10; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 13; ID: 18; XPos: 250; YPos: 17; ZPos: 0;
    Length: 11; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 13;
    ID: 19; XPos: 240; YPos: 17; ZPos: 0; Length: 11; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 17; ID: 3; XPos: 185; YPos: 6; ZPos: 0;
    Length: 46; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 17;
    ID: 4; XPos: 185; YPos: 6; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 17; ID: 15; XPos: 121; YPos: 20; ZPos: 0;
    Length: 29; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 18;
    ID: 5; XPos: 91; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 20; ID: 26; XPos: 245; YPos: 10; ZPos: 0;
    Length: 15; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 20;
    ID: 29; XPos: 245; YPos: 37; ZPos: 0; Length: 15; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 23; ID: 28; XPos: 47; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 23;
    ID: 31; XPos: 83; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 23; ID: 34; XPos: 119; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 23;
    ID: 37; XPos: 155; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 23; ID: 23; XPos: 245; YPos: 9; ZPos: 0;
    Length: 22; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 23;
    ID: 24; XPos: 245; YPos: 43; ZPos: 0; Length: 22; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 25; ID: 7; XPos: 43; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 25;
    ID: 18; XPos: 167; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 25; ID: 30; XPos: 129; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 26;
    ID: 2; XPos: 8; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 27; ID: 3; XPos: 7; YPos: 33; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 28;
    ID: 3; XPos: 110; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 28; ID: 8; XPos: 214; YPos: 16; ZPos: 0;
    Length: 34; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 29;
    ID: 8; XPos: 72; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 29; ID: 11; XPos: 155; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 32;
    ID: 13; XPos: 244; YPos: 8; ZPos: 0; Length: 31; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 33; ID: 13; XPos: 245; YPos: 10; ZPos: 0;
    Length: 42; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 33;
    ID: 23; XPos: 50; YPos: 14; ZPos: 0; Length: 82; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 34; ID: 3; XPos: 110; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 34;
    ID: 8; XPos: 214; YPos: 16; ZPos: 0; Length: 34; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 35; ID: 3; XPos: 224; YPos: 34; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 36;
    ID: 8; XPos: 173; YPos: 5; ZPos: 0; Length: 56; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 36; ID: 7; XPos: 173; YPos: 5; ZPos: 0;
    Length: 5; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 38;
    ID: 3; XPos: 68; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 38; ID: 10; XPos: 198; YPos: 9; ZPos: 0;
    Length: 20; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 38;
    ID: 11; XPos: 226; YPos: 20; ZPos: 0; Length: 20; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 42; ID: 2; XPos: 68; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 42;
    ID: 10; XPos: 196; YPos: 9; ZPos: 0; Length: 20; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 42; ID: 11; XPos: 225; YPos: 20; ZPos: 0;
    Length: 20; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 43;
    ID: 0; XPos: 206; YPos: 15; ZPos: 0; Length: 40; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 44; ID: 3; XPos: 185; YPos: 6; ZPos: 0;
    Length: 47; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 44;
    ID: 4; XPos: 185; YPos: 6; ZPos: 0; Length: 9; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 45; ID: 3; XPos: 245; YPos: 11; ZPos: 0;
    Length: 44; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 45;
    ID: 14; XPos: 8; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 45; ID: 21; XPos: 128; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 45;
    ID: 16; XPos: 97; YPos: 5; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 45; ID: 25; XPos: 209; YPos: 5; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 46;
    ID: 19; XPos: 245; YPos: 9; ZPos: 0; Length: 16; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 46; ID: 26; XPos: 245; YPos: 37; ZPos: 0;
    Length: 16; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 47;
    ID: 2; XPos: 91; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 49; ID: 3; XPos: 8; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 49;
    ID: 26; XPos: 245; YPos: 13; ZPos: 0; Length: 34; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 50; ID: 7; XPos: 199; YPos: 15; ZPos: 0;
    Length: 21; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 51;
    ID: 2; XPos: 8; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 51; ID: 7; XPos: 50; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 51;
    ID: 25; XPos: 245; YPos: 9; ZPos: 0; Length: 56; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 51; ID: 28; XPos: 128; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 52;
    ID: 36; XPos: 39; YPos: 42; ZPos: 0; Length: 12; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 52; ID: 37; XPos: 99; YPos: 42; ZPos: 0;
    Length: 12; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 52;
    ID: 39; XPos: 158; YPos: 42; ZPos: 0; Length: 12; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 52; ID: 44; XPos: 217; YPos: 42; ZPos: 0;
    Length: 12; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 53;
    ID: 3; XPos: 167; YPos: 6; ZPos: 0; Length: 47; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 53; ID: 4; XPos: 167; YPos: 6; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 53;
    ID: 10; XPos: 235; YPos: 18; ZPos: 0; Length: 15; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 54; ID: 25; XPos: 245; YPos: 11; ZPos: 0;
    Length: 25; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 56;
    ID: 3; XPos: 25; YPos: 59; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 56; ID: 26; XPos: 224; YPos: 10; ZPos: 0;
    Length: 17; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 58;
    ID: 22; XPos: 209; YPos: 26; ZPos: 0; Length: 40; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 58; ID: 28; XPos: 209; YPos: 27; ZPos: 0;
    Length: 40; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 61;
    ID: 2; XPos: 82; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 61; ID: 11; XPos: 214; YPos: 16; ZPos: 0;
    Length: 34; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 62;
    ID: 3; XPos: 110; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 62; ID: 8; XPos: 214; YPos: 16; ZPos: 0;
    Length: 32; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 63;
    ID: 8; XPos: 16; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 63; ID: 12; XPos: 245; YPos: 10; ZPos: 0;
    Length: 25; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 64;
    ID: 3; XPos: 164; YPos: 20; ZPos: 0; Length: 30; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 64; ID: 4; XPos: 182; YPos: 9; ZPos: 0;
    Length: 11; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 64;
    ID: 10; XPos: 70; YPos: 20; ZPos: 0; Length: 30; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 64; ID: 11; XPos: 91; YPos: 9; ZPos: 0;
    Length: 11; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 64;
    ID: 14; XPos: 132; YPos: 14; ZPos: 0; Length: 18; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 64; ID: 15; XPos: 225; YPos: 14; ZPos: 0;
    Length: 18; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 74;
    ID: 2; XPos: 110; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 74; ID: 8; XPos: 214; YPos: 16; ZPos: 0;
    Length: 32; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 76;
    ID: 7; XPos: 173; YPos: 5; ZPos: 0; Length: 56; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 76; ID: 9; XPos: 173; YPos: 5; ZPos: 0;
    Length: 5; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 81;
    ID: 4; XPos: 180; YPos: 5; ZPos: 0; Length: 52; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 81; ID: 6; XPos: 180; YPos: 5; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 82;
    ID: 3; XPos: 212; YPos: 16; ZPos: 0; Length: 36; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 83; ID: 2; XPos: 210; YPos: 16; ZPos: 0;
    Length: 40; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 85;
    ID: 11; XPos: 186; YPos: 6; ZPos: 0; Length: 43; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 85; ID: 13; XPos: 186; YPos: 6; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 87;
    ID: 3; XPos: 67; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 87; ID: 12; XPos: 215; YPos: 16; ZPos: 0;
    Length: 32; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 89;
    ID: 9; XPos: 245; YPos: 11; ZPos: 0; Length: 21; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 90; ID: 9; XPos: 185; YPos: 6; ZPos: 0;
    Length: 46; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 90;
    ID: 8; XPos: 185; YPos: 6; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 91; ID: 1; XPos: 122; YPos: 20; ZPos: 0;
    Length: 42; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 91;
    ID: 2; XPos: 152; YPos: 10; ZPos: 0; Length: 11; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 91; ID: 8; XPos: 199; YPos: 20; ZPos: 0;
    Length: 18; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 91;
    ID: 11; XPos: 188; YPos: 10; ZPos: 0; Length: 58; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 92; ID: 3; XPos: 8; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 92;
    ID: 21; XPos: 245; YPos: 12; ZPos: 0; Length: 37; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 94; ID: 6; XPos: 221; YPos: 23; ZPos: 0;
    Length: 13; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 94;
    ID: 7; XPos: 245; YPos: 23; ZPos: 0; Length: 13; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 94; ID: 12; XPos: 221; YPos: 23; ZPos: 0;
    Length: 25; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 94;
    ID: 13; XPos: 233; YPos: 9; ZPos: 0; Length: 15; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 97; ID: 2; XPos: 7; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 98;
    ID: 5; XPos: 15; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 98; ID: 25; XPos: 221; YPos: 23; ZPos: 0;
    Length: 25; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 98;
    ID: 26; XPos: 245; YPos: 24; ZPos: 0; Length: 15; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 98; ID: 29; XPos: 221; YPos: 24; ZPos: 0;
    Length: 9; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 98;
    ID: 11; XPos: 233; YPos: 9; ZPos: 0; Length: 9; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 100; ID: 8; XPos: 199; YPos: 6; ZPos: 0;
    Length: 48; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 100;
    ID: 10; XPos: 246; YPos: 6; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 102; ID: 4; XPos: 245; YPos: 12; ZPos: 0;
    Length: 54; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 102;
    ID: 8; XPos: 86; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 102; ID: 15; XPos: 9; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 102;
    ID: 20; XPos: 124; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 103; ID: 13; XPos: 245; YPos: 10; ZPos: 0;
    Length: 40; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 105;
    ID: 14; XPos: 121; YPos: 20; ZPos: 0; Length: 29; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 105; ID: 4; XPos: 200; YPos: 6; ZPos: 0;
    Length: 47; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 105;
    ID: 5; XPos: 246; YPos: 6; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 106; ID: 15; XPos: 162; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 106;
    ID: 6; XPos: 7; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 108; ID: 35; XPos: 42; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 44; XPos: 54; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 45; XPos: 66; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 46; XPos: 78; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 47; XPos: 90; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 48; XPos: 102; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 49; XPos: 114; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 50; XPos: 126; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 51; XPos: 138; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 52; XPos: 150; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 53; XPos: 162; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 54; XPos: 174; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 55; XPos: 186; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 56; XPos: 198; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 57; XPos: 210; YPos: 24; ZPos: 0;
    Length: 76; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 58; XPos: 222; YPos: 24; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 108; ID: 59; XPos: 26; YPos: 24; ZPos: 0;
    Length: 8; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 108;
    ID: 60; XPos: 15; YPos: 31; ZPos: 0; Length: 11; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 108; ID: 61; XPos: 26; YPos: 24; ZPos: 0;
    Length: 197; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 108;
    ID: 62; XPos: 42; YPos: 99; ZPos: 0; Length: 188; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 108; ID: 63; XPos: 229; YPos: 99; ZPos: 0;
    Length: 3; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 112;
    ID: 7; XPos: 181; YPos: 5; ZPos: 0; Length: 50; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 112; ID: 6; XPos: 180; YPos: 5; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 112;
    ID: 8; XPos: 230; YPos: 6; ZPos: 0; Length: 4; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 112; ID: 4; XPos: 235; YPos: 18; ZPos: 0;
    Length: 10; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 113;
    ID: 11; XPos: 91; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 114; ID: 12; XPos: 91; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 117;
    ID: 5; XPos: 237; YPos: 13; ZPos: 0; Length: 9; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 117; ID: 8; XPos: 95; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 117;
    ID: 15; XPos: 231; YPos: 21; ZPos: 0; Length: 7; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 118; ID: 5; XPos: 245; YPos: 11; ZPos: 0;
    Length: 21; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 118;
    ID: 13; XPos: 15; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 121; ID: 63; XPos: 213; YPos: 125; ZPos: 0;
    Length: 33; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 121;
    ID: 87; XPos: 11; YPos: 125; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 121; ID: 14; XPos: 9; YPos: 109; ZPos: 0;
    Length: 16; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 121;
    ID: 16; XPos: 225; YPos: 109; ZPos: 0; Length: 15;
    Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 121; ID: 20;
    XPos: 197; YPos: 42; ZPos: 0; Length: 40; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 121; ID: 24; XPos: 126; YPos: 3; ZPos: 0;
    Length: 13; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 121;
    ID: 19; XPos: 24; YPos: 39; ZPos: 0; Length: 193; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 121; ID: 98; XPos: 24; YPos: 115; ZPos: 0;
    Length: 205; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 121;
    ID: 99; XPos: 24; YPos: 39; ZPos: 0; Length: 76; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 121; ID: 100; XPos: 228; YPos: 23; ZPos: 0;
    Length: 92; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 121;
    ID: 97; XPos: 216; YPos: 23; ZPos: 0; Length: 13; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 121; ID: 102; XPos: 216; YPos: 23; ZPos: 0;
    Length: 40; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 125;
    ID: 6; XPos: 218; YPos: 16; ZPos: 0; Length: 25; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 125; ID: 7; XPos: 99; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 134;
    ID: 2; XPos: 67; YPos: 17; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 134; ID: 12; XPos: 215; YPos: 16; ZPos: 0;
    Length: 32; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 139;
    ID: 2; XPos: 167; YPos: 6; ZPos: 0; Length: 47; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 139; ID: 5; XPos: 167; YPos: 6; ZPos: 0;
    Length: 10; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 139;
    ID: 8; XPos: 235; YPos: 18; ZPos: 0; Length: 15; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 144; ID: 61; XPos: 217; YPos: 49; ZPos: 0;
    Length: 29; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 144;
    ID: 62; XPos: 218; YPos: 65; ZPos: 0; Length: 29; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 144; ID: 85; XPos: 25; YPos: 49; ZPos: 0;
    Length: 21; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 144;
    ID: 86; XPos: 24; YPos: 65; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 145; ID: 60; XPos: 218; YPos: 110; ZPos: 0;
    Length: 26; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 145;
    ID: 84; XPos: 9; YPos: 110; ZPos: 0; Length: 26; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 145; ID: 90; XPos: 58; YPos: 38; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 11; XPos: 46; YPos: 38; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 12; XPos: 34; YPos: 38; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 91; XPos: 82; YPos: 38; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 92; XPos: 70; YPos: 38; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 95; XPos: 106; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 96; XPos: 94; YPos: 39; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 99; XPos: 130; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 100; XPos: 118; YPos: 39; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 103; XPos: 154; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 104; XPos: 142; YPos: 39; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 107; XPos: 178; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 108; XPos: 166; YPos: 39; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 111; XPos: 202; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 112; XPos: 190; YPos: 39; ZPos: 0;
    Length: 50; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 145;
    ID: 116; XPos: 214; YPos: 39; ZPos: 0; Length: 50; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 145; ID: 117; XPos: 8; YPos: 89; ZPos: 0;
    Length: 28; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 145;
    ID: 118; XPos: 220; YPos: 89; ZPos: 0; Length: 21;
    Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 146; ID: 63;
    XPos: 216; YPos: 110; ZPos: 0; Length: 28; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 146; ID: 87; XPos: 9; YPos: 110; ZPos: 0;
    Length: 25; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 146;
    ID: 82; XPos: 23; YPos: 86; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 146; ID: 84; XPos: 204; YPos: 86; ZPos: 0;
    Length: 21; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 146;
    ID: 14; XPos: 11; YPos: 93; ZPos: 0; Length: 13; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 146; ID: 15; XPos: 23; YPos: 86; ZPos: 0;
    Length: 7; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 146;
    ID: 16; XPos: 225; YPos: 92; ZPos: 0; Length: 19; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 146; ID: 17; XPos: 225; YPos: 86; ZPos: 0;
    Length: 7; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 150;
    ID: 17; XPos: 245; YPos: 30; ZPos: 0; Length: 13; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 150; ID: 40; XPos: 245; YPos: 54; ZPos: 0;
    Length: 13; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 150;
    ID: 42; XPos: 240; YPos: 43; ZPos: 0; Length: 11; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 150; ID: 48; XPos: 250; YPos: 43; ZPos: 0;
    Length: 11; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 150;
    ID: 50; XPos: 240; YPos: 43; ZPos: 0; Length: 11; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 150; ID: 51; XPos: 240; YPos: 53; ZPos: 0;
    Length: 11; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 150;
    ID: 53; XPos: 230; YPos: 30; ZPos: 0; Length: 13; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 150; ID: 54; XPos: 230; YPos: 54; ZPos: 0;
    Length: 13; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 150;
    ID: 55; XPos: 225; YPos: 43; ZPos: 0; Length: 11; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 150; ID: 56; XPos: 235; YPos: 43; ZPos: 0;
    Length: 11; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 150;
    ID: 57; XPos: 225; YPos: 43; ZPos: 0; Length: 11; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 150; ID: 58; XPos: 225; YPos: 53; ZPos: 0;
    Length: 11; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 154;
    ID: 63; XPos: 216; YPos: 110; ZPos: 0; Length: 31;
    Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 154; ID: 87;
    XPos: 10; YPos: 110; ZPos: 0; Length: 25; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 154; ID: 82; XPos: 23; YPos: 85; ZPos: 0;
    Length: 21; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 154;
    ID: 84; XPos: 204; YPos: 85; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 154; ID: 14; XPos: 12; YPos: 92; ZPos: 0;
    Length: 12; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 154;
    ID: 15; XPos: 23; YPos: 85; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 154; ID: 16; XPos: 225; YPos: 91; ZPos: 0;
    Length: 15; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 154;
    ID: 17; XPos: 225; YPos: 85; ZPos: 0; Length: 7; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 157; ID: 2; XPos: 215; YPos: 16; ZPos: 0;
    Length: 32; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 162;
    ID: 2; XPos: 245; YPos: 7; ZPos: 0; Length: 44; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 162; ID: 9; XPos: 9; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 162;
    ID: 22; XPos: 124; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 163; ID: 4; XPos: 8; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 163;
    ID: 9; XPos: 121; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 163; ID: 15; XPos: 183; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 164;
    ID: 4; XPos: 8; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 164; ID: 9; XPos: 127; YPos: 25; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 164;
    ID: 28; XPos: 123; YPos: 38; ZPos: 0; Length: 93; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 164; ID: 14; XPos: 216; YPos: 18; ZPos: 0;
    Length: 40; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 164;
    ID: 30; XPos: 126; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 167; ID: 8; XPos: 15; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 167;
    ID: 15; XPos: 245; YPos: 11; ZPos: 0; Length: 25; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 169; ID: 6; XPos: 70; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 169;
    ID: 26; XPos: 112; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 169; ID: 29; XPos: 155; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 169;
    ID: 32; XPos: 245; YPos: 8; ZPos: 0; Length: 23; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 169; ID: 33; XPos: 245; YPos: 44; ZPos: 0;
    Length: 23; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 173;
    ID: 6; XPos: 103; YPos: 18; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 174; ID: 7; XPos: 95; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 174;
    ID: 20; XPos: 170; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 175; ID: 2; XPos: 57; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 175;
    ID: 13; XPos: 103; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 175; ID: 19; XPos: 151; YPos: 62; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 175;
    ID: 25; XPos: 198; YPos: 62; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 176; ID: 13; XPos: 245; YPos: 9; ZPos: 0;
    Length: 27; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 177;
    ID: 17; XPos: 124; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 177; ID: 22; XPos: 200; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 177;
    ID: 26; XPos: 245; YPos: 11; ZPos: 0; Length: 40; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 179; ID: 11; XPos: 83; YPos: 18; ZPos: 0;
    Length: 164; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 181;
    ID: 15; XPos: 102; YPos: 35; ZPos: 0; Length: 145;
    Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 182; ID: 27;
    XPos: 245; YPos: 9; ZPos: 0; Length: 58; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 182; ID: 28; XPos: 225; YPos: 51; ZPos: 0;
    Length: 15; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 182;
    ID: 29; XPos: 225; YPos: 51; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 183; ID: 8; XPos: 186; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 183;
    ID: 20; XPos: 8; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 184; ID: 3; XPos: 216; YPos: 16; ZPos: 0;
    Length: 30; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 186;
    ID: 9; XPos: 185; YPos: 6; ZPos: 0; Length: 46; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 186; ID: 8; XPos: 185; YPos: 6; ZPos: 0;
    Length: 7; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 187;
    ID: 8; XPos: 200; YPos: 6; ZPos: 0; Length: 47; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 187; ID: 10; XPos: 246; YPos: 6; ZPos: 0;
    Length: 7; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 188;
    ID: 7; XPos: 200; YPos: 15; ZPos: 0; Length: 21; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 189; ID: 2; XPos: 245; YPos: 8; ZPos: 0;
    Length: 30; Orientation: otVertical; LineWidth: lwThin), (ModuleID: 190;
    ID: 3; XPos: 48; YPos: 47; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 190; ID: 22; XPos: 144; YPos: 47; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 192;
    ID: 7; XPos: 212; YPos: 16; ZPos: 0; Length: 36; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 195; ID: 8; XPos: 195; YPos: 7; ZPos: 0;
    Length: 36; Orientation: otHorizontal; LineWidth: lwThin), (ModuleID: 195;
    ID: 9; XPos: 195; YPos: 7; ZPos: 0; Length: 13; Orientation: otVertical;
    LineWidth: lwThin), (ModuleID: 196; ID: 3; XPos: 8; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 201;
    ID: 2; XPos: 214; YPos: 15; ZPos: 0; Length: 28; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 202; ID: 2; XPos: 8; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 203;
    ID: 2; XPos: 215; YPos: 16; ZPos: 0; Length: 28; Orientation: otHorizontal;
    LineWidth: lwThin), (ModuleID: 205; ID: 17; XPos: 120; YPos: 17; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick), (ModuleID: 206;
    ID: 25; XPos: 153; YPos: 32; ZPos: 0; Length: 22; Orientation: otHorizontal;
    LineWidth: lwThick), (ModuleID: 208; ID: 29; XPos: 120; YPos: 32; ZPos: 0;
    Length: 22; Orientation: otHorizontal; LineWidth: lwThick));

  BitmapDefs: array [0 .. 49] of TG2BitmapDef = ((ModuleID: 5; ID: 7; XPos: 117;
    YPos: 7; ZPos: 0; Width: 18; Height: 15), (ModuleID: 5; ID: 10; XPos: 210;
    YPos: 7; ZPos: 0; Width: 18; Height: 15), (ModuleID: 17; ID: 2; XPos: 218;
    YPos: 6; ZPos: 0; Width: 24; Height: 16), (ModuleID: 18; ID: 12; XPos: 184;
    YPos: 4; ZPos: 0; Width: 62; Height: 9), (ModuleID: 20; ID: 2; XPos: 240;
    YPos: 20; ZPos: 0; Width: 11; Height: 21), (ModuleID: 23; ID: 15; XPos: 240;
    YPos: 27; ZPos: 0; Width: 11; Height: 21), (ModuleID: 36; ID: 6; XPos: 218;
    YPos: 6; ZPos: 0; Width: 23; Height: 16), (ModuleID: 36; ID: 9; XPos: 143;
    YPos: 11; ZPos: 0; Width: 7; Height: 12), (ModuleID: 41; ID: 11; XPos: 221;
    YPos: 11; ZPos: 0; Width: 21; Height: 11), (ModuleID: 46; ID: 11; XPos: 240;
    YPos: 20; ZPos: 0; Width: 11; Height: 21), (ModuleID: 47; ID: 12; XPos: 184;
    YPos: 4; ZPos: 0; Width: 62; Height: 9), (ModuleID: 50; ID: 4; XPos: 217;
    YPos: 9; ZPos: 0; Width: 23; Height: 10), (ModuleID: 52; ID: 3; XPos: 219;
    YPos: 4; ZPos: 0; Width: 24; Height: 11), (ModuleID: 53; ID: 2; XPos: 203;
    YPos: 6; ZPos: 0; Width: 23; Height: 16), (ModuleID: 53; ID: 7; XPos: 225;
    YPos: 13; ZPos: 0; Width: 11; Height: 12), (ModuleID: 55; ID: 10; XPos: 221;
    YPos: 11; ZPos: 0; Width: 21; Height: 11), (ModuleID: 57; ID: 16; XPos: 46;
    YPos: 14; ZPos: 0; Width: 20; Height: 12), (ModuleID: 58; ID: 35; XPos: 24;
    YPos: 14; ZPos: 0; Width: 91; Height: 11), (ModuleID: 58; ID: 43; XPos: 24;
    YPos: 67; ZPos: 0; Width: 91; Height: 11), (ModuleID: 58; ID: 33; XPos: 123;
    YPos: 26; ZPos: 0; Width: 91; Height: 11), (ModuleID: 58; ID: 46; XPos: 125;
    YPos: 72; ZPos: 0; Width: 49; Height: 11), (ModuleID: 76; ID: 8; XPos: 218;
    YPos: 6; ZPos: 0; Width: 23; Height: 16), (ModuleID: 84; ID: 2; XPos: 240;
    YPos: 12; ZPos: 0; Width: 11; Height: 21), (ModuleID: 85; ID: 10; XPos: 218;
    YPos: 6; ZPos: 0; Width: 23; Height: 16), (ModuleID: 90; ID: 6; XPos: 218;
    YPos: 6; ZPos: 0; Width: 24; Height: 16), (ModuleID: 100; ID: 7; XPos: 187;
    YPos: 6; ZPos: 0; Width: 24; Height: 16), (ModuleID: 105; ID: 3; XPos: 188;
    YPos: 6; ZPos: 0; Width: 24; Height: 16), (ModuleID: 112; ID: 2; XPos: 219;
    YPos: 8; ZPos: 0; Width: 19; Height: 17), (ModuleID: 113; ID: 12; XPos: 184;
    YPos: 4; ZPos: 0; Width: 62; Height: 9), (ModuleID: 114; ID: 11; XPos: 184;
    YPos: 4; ZPos: 0; Width: 62; Height: 9), (ModuleID: 115; ID: 15; XPos: 240;
    YPos: 10; ZPos: 0; Width: 11; Height: 24), (ModuleID: 117; ID: 4; XPos: 220;
    YPos: 9; ZPos: 0; Width: 18; Height: 7), (ModuleID: 119; ID: 9; XPos: 221;
    YPos: 4; ZPos: 0; Width: 21; Height: 11), (ModuleID: 139; ID: 3; XPos: 203;
    YPos: 6; ZPos: 0; Width: 23; Height: 16), (ModuleID: 139; ID: 6; XPos: 225;
    YPos: 13; ZPos: 0; Width: 11; Height: 12), (ModuleID: 141; ID: 16; XPos: 46;
    YPos: 14; ZPos: 0; Width: 20; Height: 12), (ModuleID: 142; ID: 15; XPos: 45;
    YPos: 14; ZPos: 0; Width: 20; Height: 12), (ModuleID: 143; ID: 14; XPos: 4;
    YPos: 14; ZPos: 0; Width: 5; Height: 13), (ModuleID: 164; ID: 33; XPos: 201;
    YPos: 39; ZPos: 0; Width: 14; Height: 11), (ModuleID: 164; ID: 34;
    XPos: 201; YPos: 2; ZPos: 0; Width: 14; Height: 11), (ModuleID: 164; ID: 35;
    XPos: 237; YPos: 2; ZPos: 0; Width: 14; Height: 11), (ModuleID: 169; ID: 11;
    XPos: 240; YPos: 27; ZPos: 0; Width: 11; Height: 21), (ModuleID: 180;
    ID: 82; XPos: 34; YPos: 39; ZPos: 0; Width: 215; Height: 12),
    (ModuleID: 180; ID: 25; XPos: 35; YPos: 123; ZPos: 0; Width: 215;
    Height: 12), (ModuleID: 186; ID: 6; XPos: 218; YPos: 6; ZPos: 0; Width: 24;
    Height: 16), (ModuleID: 186; ID: 11; XPos: 102; YPos: 9; ZPos: 0; Width: 7;
    Height: 12), (ModuleID: 187; ID: 7; XPos: 188; YPos: 6; ZPos: 0; Width: 24;
    Height: 16), (ModuleID: 187; ID: 11; XPos: 102; YPos: 9; ZPos: 0; Width: 7;
    Height: 12), (ModuleID: 188; ID: 4; XPos: 218; YPos: 9; ZPos: 0; Width: 23;
    Height: 10), (ModuleID: 188; ID: 6; XPos: 102; YPos: 9; ZPos: 0; Width: 7;
    Height: 12));

  SymbolDefs: array [0 .. 64] of TG2SymbolDef = ((ModuleID: 7; ID: 19; XPos: 17;
    YPos: 18; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 8;
    ID: 4; XPos: 17; YPos: 15; ZPos: 0; SymbolType: stTrig1; Width: 3;
    Height: 10), (ModuleID: 9; ID: 5; XPos: 171; YPos: 30; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 22; ID: 5; XPos: 214;
    YPos: 11; ZPos: 0; SymbolType: stBox; Width: 34; Height: 11), (ModuleID: 25;
    ID: 8; XPos: 17; YPos: 27; ZPos: 0; SymbolType: stTrig1; Width: 3;
    Height: 10), (ModuleID: 41; ID: 0; XPos: 17; YPos: 16; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 44; ID: 2; XPos: 215;
    YPos: 7; ZPos: 0; SymbolType: stAmplifier; Width: 34; Height: 18),
    (ModuleID: 46; ID: 3; XPos: 18; YPos: 30; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 53; ID: 8; XPos: 175; YPos: 13; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 55; ID: 1; XPos: 17;
    YPos: 16; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 57; ID: 15; XPos: 4; YPos: 15; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 68; ID: 20; XPos: 17; YPos: 17; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 69; ID: 1; XPos: 129;
    YPos: 11; ZPos: 0; SymbolType: stTrig2; Width: 4; Height: 10),
    (ModuleID: 69; ID: 5; XPos: 91; YPos: 12; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 71; ID: 2; XPos: 214; YPos: 11; ZPos: 0;
    SymbolType: stBox; Width: 34; Height: 11), (ModuleID: 72; ID: 4; XPos: 214;
    YPos: 11; ZPos: 0; SymbolType: stBox; Width: 34; Height: 11), (ModuleID: 75;
    ID: 8; XPos: 214; YPos: 11; ZPos: 0; SymbolType: stBox; Width: 34;
    Height: 11), (ModuleID: 81; ID: 2; XPos: 215; YPos: 6; ZPos: 0;
    SymbolType: stAmplifier; Width: 34; Height: 19), (ModuleID: 84; ID: 12;
    XPos: 201; YPos: 31; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 86; ID: 41; XPos: 17; YPos: 16; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 91; ID: 14; XPos: 111; YPos: 16; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 108; ID: 26;
    XPos: 214; YPos: 102; ZPos: 0; SymbolType: stBox; Width: 34; Height: 11),
    (ModuleID: 117; ID: 3; XPos: 215; YPos: 22; ZPos: 0;
    SymbolType: stAmplifier; Width: 34; Height: 18), (ModuleID: 121; ID: 62;
    XPos: 17; YPos: 67; ZPos: 0; SymbolType: stTrig2; Width: 4; Height: 10),
    (ModuleID: 121; ID: 15; XPos: 17; YPos: 89; ZPos: 0; SymbolType: stTrig2;
    Width: 4; Height: 10), (ModuleID: 121; ID: 18; XPos: 200; YPos: 5; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 121; ID: 103;
    XPos: 17; YPos: 44; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 130; ID: 40; XPos: 17; YPos: 16; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 141; ID: 15; XPos: 4; YPos: 15; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 142; ID: 16; XPos: 4;
    YPos: 15; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 143; ID: 16; XPos: 60; YPos: 16; ZPos: 0; SymbolType: stTrig2;
    Width: 4; Height: 10), (ModuleID: 143; ID: 17; XPos: 133; YPos: 16; ZPos: 0;
    SymbolType: stTrig2; Width: 4; Height: 10), (ModuleID: 144; ID: 63;
    XPos: 16; YPos: 38; ZPos: 0; SymbolType: stTrig2; Width: 4; Height: 10),
    (ModuleID: 144; ID: 91; XPos: 200; YPos: 5; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 144; ID: 89; XPos: 16; YPos: 60; ZPos: 0;
    SymbolType: stTrig2; Width: 4; Height: 10), (ModuleID: 144; ID: 92;
    XPos: 16; YPos: 16; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 145; ID: 62; XPos: 17; YPos: 48; ZPos: 0; SymbolType: stTrig2;
    Width: 4; Height: 10), (ModuleID: 145; ID: 121; XPos: 17; YPos: 70; ZPos: 0;
    SymbolType: stTrig2; Width: 4; Height: 10), (ModuleID: 145; ID: 125;
    XPos: 199; YPos: 5; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 145; ID: 126; XPos: 17; YPos: 26; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 146; ID: 10; XPos: 16; YPos: 25; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 146; ID: 20;
    XPos: 16; YPos: 48; ZPos: 0; SymbolType: stTrig2; Width: 4; Height: 10),
    (ModuleID: 146; ID: 21; XPos: 16; YPos: 70; ZPos: 0; SymbolType: stTrig2;
    Width: 4; Height: 10), (ModuleID: 146; ID: 22; XPos: 200; YPos: 5; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 152; ID: 14;
    XPos: 216; YPos: 11; ZPos: 0; SymbolType: stBox; Width: 30; Height: 11),
    (ModuleID: 158; ID: 10; XPos: 214; YPos: 11; ZPos: 0; SymbolType: stBox;
    Width: 34; Height: 11), (ModuleID: 160; ID: 4; XPos: 214; YPos: 13; ZPos: 0;
    SymbolType: stBox; Width: 34; Height: 11), (ModuleID: 163; ID: 19; XPos: 17;
    YPos: 18; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 164; ID: 19; XPos: 17; YPos: 18; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 169; ID: 3; XPos: 18; YPos: 32; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 172; ID: 8;
    XPos: 214; YPos: 11; ZPos: 0; SymbolType: stBox; Width: 34; Height: 11),
    (ModuleID: 173; ID: 9; XPos: 214; YPos: 11; ZPos: 0; SymbolType: stBox;
    Width: 34; Height: 11), (ModuleID: 178; ID: 8; XPos: 214; YPos: 11; ZPos: 0;
    SymbolType: stBox; Width: 34; Height: 11), (ModuleID: 183; ID: 5; XPos: 171;
    YPos: 30; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 190; ID: 19; XPos: 17; YPos: 17; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 195; ID: 0; XPos: 214; YPos: 7; ZPos: 0;
    SymbolType: stAmplifier; Width: 34; Height: 18), (ModuleID: 196; ID: 6;
    XPos: 58; YPos: 30; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 204; ID: 8; XPos: 17; YPos: 16; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 204; ID: 12; XPos: 81; YPos: 16; ZPos: 0;
    SymbolType: stTrig2; Width: 4; Height: 10), (ModuleID: 205; ID: 15;
    XPos: 17; YPos: 16; ZPos: 0; SymbolType: stTrig1; Width: 3; Height: 10),
    (ModuleID: 205; ID: 9; XPos: 81; YPos: 16; ZPos: 0; SymbolType: stTrig2;
    Width: 4; Height: 10), (ModuleID: 206; ID: 15; XPos: 17; YPos: 16; ZPos: 0;
    SymbolType: stTrig1; Width: 3; Height: 10), (ModuleID: 206; ID: 9; XPos: 17;
    YPos: 30; ZPos: 0; SymbolType: stTrig2; Width: 4; Height: 10),
    (ModuleID: 208; ID: 15; XPos: 17; YPos: 16; ZPos: 0; SymbolType: stTrig1;
    Width: 3; Height: 10), (ModuleID: 208; ID: 14; XPos: 17; YPos: 30; ZPos: 0;
    SymbolType: stTrig2; Width: 4; Height: 10));

  TextFieldDefs: array [0 .. 228] of TG2TextFieldDef = ((ModuleID: 7; ID: 20;
    XPos: 48; YPos: 15; Width: 50; MasterRef: 0; TextFunc: 60;
    slDependencies: '0,1,4'), (ModuleID: 7; ID: 26; XPos: 159; YPos: 33;
    Width: 26; MasterRef: 6; TextFunc: 0; slDependencies: ''), (ModuleID: 8;
    ID: 17; XPos: 157; YPos: 7; Width: 26; MasterRef: 6; TextFunc: 27;
    slDependencies: ''), (ModuleID: 8; ID: 7; XPos: 79; YPos: 4; Width: 50;
    MasterRef: 0; TextFunc: 60; slDependencies: '0,1,4'), (ModuleID: 9; ID: 1;
    XPos: 46; YPos: 14; Width: 50; MasterRef: 0; TextFunc: 60;
    slDependencies: '0,1,3'), (ModuleID: 12; ID: 2; XPos: 56; YPos: 27;
    Width: 35; MasterRef: 0; TextFunc: 107; slDependencies: '0,S0'),
    (ModuleID: 12; ID: 11; XPos: 120; YPos: 27; Width: 26; MasterRef: 1;
    TextFunc: 0; slDependencies: ''), (ModuleID: 13; ID: 4; XPos: 46; YPos: 14;
    Width: 50; MasterRef: 0; TextFunc: 60; slDependencies: '0,1,4'),
    (ModuleID: 15; ID: 10; XPos: 19; YPos: 42; Width: 17; MasterRef: 0;
    TextFunc: 101; slDependencies: '0'), (ModuleID: 17; ID: 11; XPos: 106;
    YPos: 13; Width: 17; MasterRef: 0; TextFunc: 0; slDependencies: ''),
    (ModuleID: 20; ID: 6; XPos: 42; YPos: 17; Width: 32; MasterRef: 1;
    TextFunc: 0; slDependencies: ''), (ModuleID: 20; ID: 7; XPos: 139; YPos: 17;
    Width: 32; MasterRef: 4; TextFunc: 0; slDependencies: ''), (ModuleID: 20;
    ID: 14; XPos: 76; YPos: 17; Width: 32; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 20; ID: 20; XPos: 110; YPos: 17; Width: 27;
    MasterRef: 3; TextFunc: 0; slDependencies: ''), (ModuleID: 22; ID: 1;
    XPos: 105; YPos: 12; Width: 40; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 23; ID: 0; XPos: 30; YPos: 14; Width: 35;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 23; ID: 1;
    XPos: 136; YPos: 14; Width: 35; MasterRef: 3; TextFunc: 0;
    slDependencies: ''), (ModuleID: 23; ID: 6; XPos: 68; YPos: 14; Width: 35;
    MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 23; ID: 9;
    XPos: 106; YPos: 14; Width: 27; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 24; ID: 2; XPos: 122; YPos: 8; Width: 40;
    MasterRef: 0; TextFunc: 103; slDependencies: '0,3'), (ModuleID: 25; ID: 34;
    XPos: 75; YPos: 33; Width: 40; MasterRef: 0; TextFunc: 103;
    slDependencies: '0,1'), (ModuleID: 25; ID: 35; XPos: 146; YPos: 32;
    Width: 27; MasterRef: 7; TextFunc: 0; slDependencies: ''), (ModuleID: 26;
    ID: 10; XPos: 78; YPos: 23; Width: 40; MasterRef: 0; TextFunc: 103;
    slDependencies: '0,7'), (ModuleID: 27; ID: 8; XPos: 46; YPos: 14; Width: 50;
    MasterRef: 0; TextFunc: 60; slDependencies: '0,1,3'), (ModuleID: 32; ID: 5;
    XPos: 80; YPos: 4; Width: 40; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 32; ID: 6; XPos: 129; YPos: 4; Width: 40;
    MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 33; ID: 5;
    XPos: 4; YPos: 17; Width: 40; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 33; ID: 4; XPos: 50; YPos: 17; Width: 40;
    MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 33; ID: 6;
    XPos: 139; YPos: 17; Width: 40; MasterRef: 3; TextFunc: 0;
    slDependencies: ''), (ModuleID: 33; ID: 18; XPos: 92; YPos: 17; Width: 40;
    MasterRef: 2; TextFunc: 0; slDependencies: ''), (ModuleID: 36; ID: 5;
    XPos: 18; YPos: 13; Width: 17; MasterRef: 0; TextFunc: 100;
    slDependencies: '0'), (ModuleID: 38; ID: 5; XPos: 126; YPos: 8; Width: 38;
    MasterRef: 0; TextFunc: 122; slDependencies: '0,2'), (ModuleID: 41; ID: 6;
    XPos: 81; YPos: 12; Width: 35; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 42; ID: 4; XPos: 126; YPos: 8; Width: 38;
    MasterRef: 0; TextFunc: 122; slDependencies: '0,2'), (ModuleID: 43; ID: 3;
    XPos: 187; YPos: 8; Width: 28; MasterRef: 0; TextFunc: 96;
    slDependencies: '0,1'), (ModuleID: 45; ID: 4; XPos: 139; YPos: 15;
    Width: 17; MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 45;
    ID: 6; XPos: 176; YPos: 15; Width: 17; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 45; ID: 8; XPos: 102; YPos: 15; Width: 17;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 46; ID: 15;
    XPos: 51; YPos: 18; Width: 35; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 46; ID: 16; XPos: 137; YPos: 18; Width: 35;
    MasterRef: 4; TextFunc: 0; slDependencies: ''), (ModuleID: 46; ID: 23;
    XPos: 94; YPos: 18; Width: 35; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 49; ID: 7; XPos: 86; YPos: 16; Width: 44;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 49; ID: 12;
    XPos: 148; YPos: 16; Width: 26; MasterRef: 4; TextFunc: 0;
    slDependencies: ''), (ModuleID: 50; ID: 1; XPos: 187; YPos: 8; Width: 28;
    MasterRef: 0; TextFunc: 96; slDependencies: '0,2'), (ModuleID: 51; ID: 9;
    XPos: 32; YPos: 18; Width: 44; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 51; ID: 15; XPos: 108; YPos: 18; Width: 26;
    MasterRef: 4; TextFunc: 0; slDependencies: ''), (ModuleID: 52; ID: 11;
    XPos: 47; YPos: 49; Width: 24; MasterRef: 0; TextFunc: 137;
    slDependencies: '0,10'), (ModuleID: 52; ID: 14; XPos: 107; YPos: 49;
    Width: 24; MasterRef: 1; TextFunc: 137; slDependencies: '1,10'),
    (ModuleID: 52; ID: 17; XPos: 167; YPos: 49; Width: 24; MasterRef: 2;
    TextFunc: 137; slDependencies: '2,10'), (ModuleID: 52; ID: 20; XPos: 227;
    YPos: 49; Width: 24; MasterRef: 3; TextFunc: 137; slDependencies: '3,10'),
    (ModuleID: 52; ID: 23; XPos: 13; YPos: 49; Width: 32; MasterRef: 4;
    TextFunc: 0; slDependencies: ''), (ModuleID: 52; ID: 26; XPos: 73; YPos: 49;
    Width: 32; MasterRef: 5; TextFunc: 0; slDependencies: ''), (ModuleID: 52;
    ID: 29; XPos: 133; YPos: 49; Width: 32; MasterRef: 6; TextFunc: 0;
    slDependencies: ''), (ModuleID: 52; ID: 32; XPos: 193; YPos: 49; Width: 32;
    MasterRef: 7; TextFunc: 0; slDependencies: ''), (ModuleID: 54; ID: 9;
    XPos: 6; YPos: 27; Width: 44; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 54; ID: 15; XPos: 81; YPos: 27; Width: 26;
    MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 55; ID: 7;
    XPos: 81; YPos: 12; Width: 35; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 56; ID: 4; XPos: 28; YPos: 15; Width: 50;
    MasterRef: 0; TextFunc: 60; slDependencies: '0,1,4'), (ModuleID: 57; ID: 0;
    XPos: 208; YPos: 12; Width: 29; MasterRef: 2; TextFunc: 108;
    slDependencies: '2'), (ModuleID: 57; ID: 2; XPos: 108; YPos: 12; Width: 23;
    MasterRef: 0; TextFunc: 179; slDependencies: ''), (ModuleID: 57; ID: 3;
    XPos: 154; YPos: 12; Width: 23; MasterRef: 1; TextFunc: 179;
    slDependencies: ''), (ModuleID: 58; ID: 15; XPos: 25; YPos: 27; Width: 40;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 58; ID: 16;
    XPos: 25; YPos: 80; Width: 40; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 59; ID: 0; XPos: 149; YPos: 8; Width: 21;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 61; ID: 3;
    XPos: 121; YPos: 12; Width: 26; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 63; ID: 10; XPos: 87; YPos: 27; Width: 49;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 66; ID: 6;
    XPos: 101; YPos: 12; Width: 28; MasterRef: 1; TextFunc: 102;
    slDependencies: '1,4'), (ModuleID: 66; ID: 10; XPos: 186; YPos: 12;
    Width: 28; MasterRef: 3; TextFunc: 102; slDependencies: '3,4'),
    (ModuleID: 68; ID: 2; XPos: 86; YPos: 15; Width: 58; MasterRef: 0;
    TextFunc: 110; slDependencies: '0,1,2'), (ModuleID: 68; ID: 13; XPos: 140;
    YPos: 39; Width: 22; MasterRef: 3; TextFunc: 166; slDependencies: '3'),
    (ModuleID: 69; ID: 6; XPos: 162; YPos: 12; Width: 21; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 71; ID: 3; XPos: 80; YPos: 12;
    Width: 32; MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 71;
    ID: 5; XPos: 148; YPos: 12; Width: 32; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 72; ID: 0; XPos: 90; YPos: 12; Width: 55;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 75; ID: 1;
    XPos: 79; YPos: 12; Width: 32; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 75; ID: 3; XPos: 145; YPos: 12; Width: 23;
    MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 76; ID: 4;
    XPos: 18; YPos: 13; Width: 17; MasterRef: 0; TextFunc: 100;
    slDependencies: '0'), (ModuleID: 78; ID: 11; XPos: 19; YPos: 42; Width: 17;
    MasterRef: 0; TextFunc: 101; slDependencies: '0'), (ModuleID: 79; ID: 10;
    XPos: 19; YPos: 27; Width: 17; MasterRef: 0; TextFunc: 101;
    slDependencies: '0'), (ModuleID: 81; ID: 5; XPos: 164; YPos: 10; Width: 32;
    MasterRef: 0; TextFunc: 147; slDependencies: '0,1'), (ModuleID: 84; ID: 15;
    XPos: 79; YPos: 4; Width: 35; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 84; ID: 16; XPos: 123; YPos: 4; Width: 35;
    MasterRef: 3; TextFunc: 0; slDependencies: ''), (ModuleID: 85; ID: 3;
    XPos: 77; YPos: 12; Width: 25; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 85; ID: 6; XPos: 129; YPos: 12; Width: 25;
    MasterRef: 1; TextFunc: 12; slDependencies: ''), (ModuleID: 87; ID: 2;
    XPos: 99; YPos: 8; Width: 44; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 88; ID: 2; XPos: 19; YPos: 27; Width: 17;
    MasterRef: 0; TextFunc: 99; slDependencies: '0'), (ModuleID: 89; ID: 12;
    XPos: 62; YPos: 23; Width: 40; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 90; ID: 2; XPos: 18; YPos: 13; Width: 17;
    MasterRef: 0; TextFunc: 100; slDependencies: '0'), (ModuleID: 92; ID: 5;
    XPos: 86; YPos: 16; Width: 44; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 92; ID: 22; XPos: 148; YPos: 16; Width: 26;
    MasterRef: 3; TextFunc: 0; slDependencies: ''), (ModuleID: 96; ID: 2;
    XPos: 83; YPos: 8; Width: 50; MasterRef: 0; TextFunc: 60;
    slDependencies: '0,1,3'), (ModuleID: 97; ID: 10; XPos: 42; YPos: 14;
    Width: 50; MasterRef: 0; TextFunc: 60; slDependencies: '0,1,6'),
    (ModuleID: 98; ID: 27; XPos: 117; YPos: 27; Width: 46; MasterRef: 0;
    TextFunc: 142; slDependencies: '0,2'), (ModuleID: 100; ID: 5; XPos: 18;
    YPos: 13; Width: 17; MasterRef: 0; TextFunc: 100; slDependencies: '0'),
    (ModuleID: 102; ID: 11; XPos: 37; YPos: 24; Width: 44; MasterRef: 1;
    TextFunc: 39; slDependencies: ''), (ModuleID: 102; ID: 25; XPos: 164;
    YPos: 57; Width: 17; MasterRef: 4; TextFunc: 0; slDependencies: ''),
    (ModuleID: 103; ID: 5; XPos: 27; YPos: 18; Width: 44; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 103; ID: 4; XPos: 76; YPos: 18;
    Width: 44; MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 103;
    ID: 6; XPos: 125; YPos: 18; Width: 44; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 105; ID: 10; XPos: 106; YPos: 13; Width: 17;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 106; ID: 11;
    XPos: 46; YPos: 14; Width: 50; MasterRef: 0; TextFunc: 60;
    slDependencies: '0,1,4'), (ModuleID: 112; ID: 5; XPos: 167; YPos: 10;
    Width: 28; MasterRef: 0; TextFunc: 96; slDependencies: '0,1'),
    (ModuleID: 115; ID: 1; XPos: 116; YPos: 4; Width: 25; MasterRef: 1;
    TextFunc: 0; slDependencies: ''), (ModuleID: 115; ID: 0; XPos: 79; YPos: 4;
    Width: 35; MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 115;
    ID: 5; XPos: 143; YPos: 4; Width: 35; MasterRef: 2; TextFunc: 0;
    slDependencies: ''), (ModuleID: 118; ID: 6; XPos: 180; YPos: 27; Width: 23;
    MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 118; ID: 16;
    XPos: 84; YPos: 27; Width: 44; MasterRef: 1; TextFunc: 0;
    slDependencies: ''), (ModuleID: 119; ID: 12; XPos: 42; YPos: 34; Width: 35;
    MasterRef: 2; TextFunc: 0; slDependencies: ''), (ModuleID: 119; ID: 13;
    XPos: 148; YPos: 34; Width: 35; MasterRef: 5; TextFunc: 0;
    slDependencies: ''), (ModuleID: 119; ID: 20; XPos: 80; YPos: 34; Width: 35;
    MasterRef: 3; TextFunc: 0; slDependencies: ''), (ModuleID: 119; ID: 17;
    XPos: 118; YPos: 34; Width: 27; MasterRef: 4; TextFunc: 0;
    slDependencies: ''), (ModuleID: 119; ID: 26; XPos: 186; YPos: 34; Width: 27;
    MasterRef: 6; TextFunc: 0; slDependencies: ''), (ModuleID: 119; ID: 29;
    XPos: 216; YPos: 34; Width: 35; MasterRef: 7; TextFunc: 0;
    slDependencies: ''), (ModuleID: 121; ID: 80; XPos: 179; YPos: 4; Width: 16;
    MasterRef: 33; TextFunc: 2; slDependencies: '33'), (ModuleID: 134; ID: 3;
    XPos: 99; YPos: 8; Width: 44; MasterRef: 0; TextFunc: 0;
    slDependencies: ''), (ModuleID: 141; ID: 0; XPos: 208; YPos: 12; Width: 29;
    MasterRef: 2; TextFunc: 108; slDependencies: '2'), (ModuleID: 141; ID: 2;
    XPos: 95; YPos: 12; Width: 23; MasterRef: 0; TextFunc: 179;
    slDependencies: ''), (ModuleID: 141; ID: 3; XPos: 154; YPos: 12; Width: 23;
    MasterRef: 1; TextFunc: 179; slDependencies: ''), (ModuleID: 142; ID: 1;
    XPos: 208; YPos: 12; Width: 29; MasterRef: 1; TextFunc: 108;
    slDependencies: '1'), (ModuleID: 142; ID: 2; XPos: 154; YPos: 12; Width: 23;
    MasterRef: 0; TextFunc: 179; slDependencies: ''), (ModuleID: 143; ID: 1;
    XPos: 208; YPos: 12; Width: 29; MasterRef: 2; TextFunc: 108;
    slDependencies: '2'), (ModuleID: 143; ID: 2; XPos: 151; YPos: 12; Width: 26;
    MasterRef: 1; TextFunc: 13; slDependencies: '1'), (ModuleID: 143; ID: 0;
    XPos: 79; YPos: 12; Width: 23; MasterRef: 0; TextFunc: 179;
    slDependencies: ''), (ModuleID: 144; ID: 81; XPos: 167; YPos: 4; Width: 16;
    MasterRef: 33; TextFunc: 2; slDependencies: '33'), (ModuleID: 145; ID: 80;
    XPos: 167; YPos: 4; Width: 16; MasterRef: 33; TextFunc: 2;
    slDependencies: '33'), (ModuleID: 145; ID: 86; XPos: 24; YPos: 45;
    Width: 21; MasterRef: 0; TextFunc: 133; slDependencies: '0,34'),
    (ModuleID: 145; ID: 88; XPos: 36; YPos: 62; Width: 21; MasterRef: 1;
    TextFunc: 133; slDependencies: '1,34'), (ModuleID: 145; ID: 89; XPos: 48;
    YPos: 45; Width: 21; MasterRef: 2; TextFunc: 133; slDependencies: '2,34'),
    (ModuleID: 145; ID: 85; XPos: 60; YPos: 62; Width: 21; MasterRef: 3;
    TextFunc: 133; slDependencies: '3,34'), (ModuleID: 145; ID: 87; XPos: 72;
    YPos: 45; Width: 21; MasterRef: 4; TextFunc: 133; slDependencies: '4,34'),
    (ModuleID: 145; ID: 93; XPos: 84; YPos: 62; Width: 21; MasterRef: 5;
    TextFunc: 133; slDependencies: '5,34'), (ModuleID: 145; ID: 94; XPos: 96;
    YPos: 45; Width: 21; MasterRef: 6; TextFunc: 133; slDependencies: '6,34'),
    (ModuleID: 145; ID: 97; XPos: 108; YPos: 62; Width: 21; MasterRef: 7;
    TextFunc: 133; slDependencies: '7,34'), (ModuleID: 145; ID: 98; XPos: 120;
    YPos: 45; Width: 21; MasterRef: 8; TextFunc: 133; slDependencies: '8,34'),
    (ModuleID: 145; ID: 101; XPos: 132; YPos: 62; Width: 21; MasterRef: 9;
    TextFunc: 133; slDependencies: '9,34'), (ModuleID: 145; ID: 102; XPos: 144;
    YPos: 45; Width: 21; MasterRef: 10; TextFunc: 133; slDependencies: '10,34'),
    (ModuleID: 145; ID: 105; XPos: 156; YPos: 62; Width: 21; MasterRef: 11;
    TextFunc: 133; slDependencies: '11,34'), (ModuleID: 145; ID: 106; XPos: 168;
    YPos: 45; Width: 21; MasterRef: 12; TextFunc: 133; slDependencies: '12,34'),
    (ModuleID: 145; ID: 109; XPos: 180; YPos: 62; Width: 21; MasterRef: 13;
    TextFunc: 133; slDependencies: '13,34'), (ModuleID: 145; ID: 110; XPos: 192;
    YPos: 45; Width: 21; MasterRef: 14; TextFunc: 133; slDependencies: '14,34'),
    (ModuleID: 145; ID: 113; XPos: 204; YPos: 62; Width: 21; MasterRef: 15;
    TextFunc: 133; slDependencies: '15,34'), (ModuleID: 146; ID: 80; XPos: 167;
    YPos: 4; Width: 16; MasterRef: 33; TextFunc: 2; slDependencies: '33'),
    (ModuleID: 147; ID: 0; XPos: 139; YPos: 12; Width: 29; MasterRef: 1;
    TextFunc: 109; slDependencies: '1'), (ModuleID: 147; ID: 1; XPos: 95;
    YPos: 12; Width: 20; MasterRef: 0; TextFunc: 179; slDependencies: ''),
    (ModuleID: 148; ID: 0; XPos: 79; YPos: 12; Width: 26; MasterRef: 0;
    TextFunc: 13; slDependencies: '0'), (ModuleID: 148; ID: 7; XPos: 139;
    YPos: 12; Width: 29; MasterRef: 1; TextFunc: 109; slDependencies: '1'),
    (ModuleID: 149; ID: 0; XPos: 23; YPos: 27; Width: 29; MasterRef: 0;
    TextFunc: 109; slDependencies: '0'), (ModuleID: 149; ID: 2; XPos: 67;
    YPos: 27; Width: 25; MasterRef: 1; TextFunc: 13; slDependencies: '1'),
    (ModuleID: 149; ID: 6; XPos: 107; YPos: 27; Width: 25; MasterRef: 2;
    TextFunc: 13; slDependencies: '2'), (ModuleID: 149; ID: 11; XPos: 170;
    YPos: 27; Width: 25; MasterRef: 3; TextFunc: 17; slDependencies: '3'),
    (ModuleID: 149; ID: 15; XPos: 210; YPos: 27; Width: 29; MasterRef: 4;
    TextFunc: 108; slDependencies: '4'), (ModuleID: 150; ID: 23; XPos: 4;
    YPos: 33; Width: 32; MasterRef: 0; TextFunc: 0; slDependencies: ''),
    (ModuleID: 150; ID: 24; XPos: 39; YPos: 33; Width: 30; MasterRef: 1;
    TextFunc: 0; slDependencies: ''), (ModuleID: 150; ID: 25; XPos: 74;
    YPos: 33; Width: 32; MasterRef: 2; TextFunc: 0; slDependencies: ''),
    (ModuleID: 150; ID: 26; XPos: 109; YPos: 33; Width: 32; MasterRef: 3;
    TextFunc: 0; slDependencies: ''), (ModuleID: 150; ID: 38; XPos: 146;
    YPos: 33; Width: 32; MasterRef: 4; TextFunc: 0; slDependencies: ''),
    (ModuleID: 152; ID: 17; XPos: 79; YPos: 12; Width: 32; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 156; ID: 1; XPos: 85; YPos: 12;
    Width: 32; MasterRef: 0; TextFunc: 13; slDependencies: '0'), (ModuleID: 158;
    ID: 6; XPos: 104; YPos: 12; Width: 48; MasterRef: 0; TextFunc: 217;
    slDependencies: '0,2'), (ModuleID: 161; ID: 3; XPos: 4; YPos: 37; Width: 28;
    MasterRef: 0; TextFunc: 102; slDependencies: '0,16'), (ModuleID: 161; ID: 5;
    XPos: 33; YPos: 37; Width: 28; MasterRef: 1; TextFunc: 102;
    slDependencies: '1,16'), (ModuleID: 161; ID: 7; XPos: 62; YPos: 37;
    Width: 28; MasterRef: 2; TextFunc: 102; slDependencies: '2,16'),
    (ModuleID: 161; ID: 9; XPos: 91; YPos: 37; Width: 28; MasterRef: 3;
    TextFunc: 102; slDependencies: '3,16'), (ModuleID: 161; ID: 11; XPos: 120;
    YPos: 37; Width: 28; MasterRef: 4; TextFunc: 102; slDependencies: '4,16'),
    (ModuleID: 161; ID: 13; XPos: 149; YPos: 37; Width: 28; MasterRef: 5;
    TextFunc: 102; slDependencies: '5,16'), (ModuleID: 161; ID: 15; XPos: 178;
    YPos: 37; Width: 28; MasterRef: 6; TextFunc: 102; slDependencies: '6,16'),
    (ModuleID: 161; ID: 17; XPos: 207; YPos: 37; Width: 28; MasterRef: 7;
    TextFunc: 102; slDependencies: '7,16'), (ModuleID: 162; ID: 5; XPos: 66;
    YPos: 18; Width: 50; MasterRef: 0; TextFunc: 173; slDependencies: '0'),
    (ModuleID: 163; ID: 20; XPos: 48; YPos: 17; Width: 50; MasterRef: 0;
    TextFunc: 60; slDependencies: '0,1,4'), (ModuleID: 163; ID: 26; XPos: 159;
    YPos: 31; Width: 26; MasterRef: 7; TextFunc: 0; slDependencies: ''),
    (ModuleID: 164; ID: 20; XPos: 48; YPos: 17; Width: 50; MasterRef: 0;
    TextFunc: 60; slDependencies: '0,1,4'), (ModuleID: 165; ID: 19; XPos: 60;
    YPos: 40; Width: 20; MasterRef: 0; TextFunc: 2; slDependencies: ''),
    (ModuleID: 165; ID: 20; XPos: 10; YPos: 40; Width: 20; MasterRef: 1;
    TextFunc: 179; slDependencies: '1'), (ModuleID: 167; ID: 11; XPos: 76;
    YPos: 27; Width: 60; MasterRef: 0; TextFunc: 201; slDependencies: '0,1'),
    (ModuleID: 169; ID: 15; XPos: 51; YPos: 14; Width: 35; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 169; ID: 16; XPos: 137;
    YPos: 14; Width: 35; MasterRef: 2; TextFunc: 0; slDependencies: ''),
    (ModuleID: 169; ID: 23; XPos: 94; YPos: 14; Width: 35; MasterRef: 1;
    TextFunc: 0; slDependencies: ''), (ModuleID: 172; ID: 4; XPos: 139;
    YPos: 12; Width: 40; MasterRef: 0; TextFunc: 141; slDependencies: '0,S0'),
    (ModuleID: 173; ID: 7; XPos: 139; YPos: 12; Width: 40; MasterRef: 0;
    TextFunc: 141; slDependencies: '0,S0'), (ModuleID: 174; ID: 6; XPos: 90;
    YPos: 6; Width: 40; MasterRef: 0; TextFunc: 141; slDependencies: '0,S0'),
    (ModuleID: 174; ID: 21; XPos: 165; YPos: 6; Width: 40; MasterRef: 2;
    TextFunc: 141; slDependencies: '2,S0'), (ModuleID: 175; ID: 5; XPos: 48;
    YPos: 15; Width: 40; MasterRef: 0; TextFunc: 140; slDependencies: '0,8,S0'),
    (ModuleID: 175; ID: 9; XPos: 95; YPos: 15; Width: 40; MasterRef: 2;
    TextFunc: 140; slDependencies: '2,8,S0'), (ModuleID: 175; ID: 15; XPos: 142;
    YPos: 15; Width: 40; MasterRef: 4; TextFunc: 140; slDependencies: '4,8,S0'),
    (ModuleID: 175; ID: 21; XPos: 189; YPos: 15; Width: 40; MasterRef: 6;
    TextFunc: 140; slDependencies: '6,8,S0'), (ModuleID: 176; ID: 14; XPos: 52;
    YPos: 27; Width: 40; MasterRef: 0; TextFunc: 143; slDependencies: '0,5,S0'),
    (ModuleID: 177; ID: 14; XPos: 52; YPos: 28; Width: 40; MasterRef: 0;
    TextFunc: 143; slDependencies: '0,4,S0'), (ModuleID: 178; ID: 4; XPos: 139;
    YPos: 8; Width: 40; MasterRef: 0; TextFunc: 2; slDependencies: ''),
    (ModuleID: 180; ID: 8; XPos: 37; YPos: 83; Width: 22; MasterRef: 8;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 9; XPos: 63;
    YPos: 83; Width: 22; MasterRef: 9; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 10; XPos: 92; YPos: 83; Width: 22; MasterRef: 10;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 11; XPos: 118;
    YPos: 83; Width: 22; MasterRef: 11; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 12; XPos: 147; YPos: 83; Width: 22; MasterRef: 12;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 13; XPos: 173;
    YPos: 83; Width: 22; MasterRef: 13; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 14; XPos: 202; YPos: 83; Width: 22; MasterRef: 14;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 15; XPos: 228;
    YPos: 83; Width: 22; MasterRef: 15; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 32; XPos: 89; YPos: 20; Width: 50; MasterRef: 4;
    TextFunc: 198; slDependencies: '3,4,2'), (ModuleID: 180; ID: 38; XPos: 204;
    YPos: 19; Width: 15; MasterRef: 5; TextFunc: 0; slDependencies: ''),
    (ModuleID: 180; ID: 43; XPos: 181; YPos: 54; Width: 15; MasterRef: 6;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 45; XPos: 217;
    YPos: 54; Width: 15; MasterRef: 7; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 55; XPos: 5; YPos: 126; Width: 11; MasterRef: 16;
    TextFunc: 179; slDependencies: ''), (ModuleID: 180; ID: 59; XPos: 210;
    YPos: 139; Width: 22; MasterRef: 22; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 63; XPos: 170; YPos: 162; Width: 20; MasterRef: 21;
    TextFunc: 179; slDependencies: '21'), (ModuleID: 180; ID: 67; XPos: 102;
    YPos: 162; Width: 20; MasterRef: 19; TextFunc: 179; slDependencies: ''),
    (ModuleID: 180; ID: 71; XPos: 30; YPos: 158; Width: 24; MasterRef: 17;
    TextFunc: 197; slDependencies: ''), (ModuleID: 181; ID: 11; XPos: 109;
    YPos: 11; Width: 40; MasterRef: 0; TextFunc: 145; slDependencies: '0,s0'),
    (ModuleID: 182; ID: 2; XPos: 27; YPos: 29; Width: 40; MasterRef: 0;
    TextFunc: 146; slDependencies: '0,6,s0'), (ModuleID: 182; ID: 8; XPos: 27;
    YPos: 53; Width: 40; MasterRef: 0; TextFunc: 146; slDependencies: '1,6,s0'),
    (ModuleID: 183; ID: 1; XPos: 46; YPos: 14; Width: 50; MasterRef: 0;
    TextFunc: 60; slDependencies: '0,1,3'), (ModuleID: 186; ID: 2; XPos: 18;
    YPos: 13; Width: 17; MasterRef: 0; TextFunc: 100; slDependencies: '0'),
    (ModuleID: 187; ID: 5; XPos: 18; YPos: 13; Width: 17; MasterRef: 0;
    TextFunc: 100; slDependencies: '0'), (ModuleID: 188; ID: 1; XPos: 187;
    YPos: 8; Width: 28; MasterRef: 0; TextFunc: 96; slDependencies: '0,2'),
    (ModuleID: 189; ID: 6; XPos: 142; YPos: 27; Width: 42; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 189; ID: 10; XPos: 10;
    YPos: 27; Width: 35; MasterRef: 1; TextFunc: 0; slDependencies: ''),
    (ModuleID: 189; ID: 11; XPos: 75; YPos: 27; Width: 35; MasterRef: 2;
    TextFunc: 0; slDependencies: ''), (ModuleID: 190; ID: 8; XPos: 78; YPos: 18;
    Width: 40; MasterRef: 0; TextFunc: 103; slDependencies: '0,2'),
    (ModuleID: 190; ID: 28; XPos: 170; YPos: 20; Width: 25; MasterRef: 6;
    TextFunc: 0; slDependencies: ''), (ModuleID: 192; ID: 9; XPos: 81; YPos: 12;
    Width: 40; MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 196;
    ID: 8; XPos: 66; YPos: 14; Width: 50; MasterRef: 0; TextFunc: 60;
    slDependencies: '0,1,2'), (ModuleID: 198; ID: 0; XPos: 88; YPos: 12;
    Width: 42; MasterRef: 0; TextFunc: 0; slDependencies: ''), (ModuleID: 200;
    ID: 2; XPos: 101; YPos: 8; Width: 40; MasterRef: 0; TextFunc: 103;
    slDependencies: '0,3'), (ModuleID: 202; ID: 10; XPos: 82; YPos: 23;
    Width: 40; MasterRef: 0; TextFunc: 103; slDependencies: '0,7'),
    (ModuleID: 204; ID: 11; XPos: 163; YPos: 12; Width: 32; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 205; ID: 18; XPos: 155;
    YPos: 12; Width: 32; MasterRef: 0; TextFunc: 0; slDependencies: ''),
    (ModuleID: 206; ID: 18; XPos: 186; YPos: 4; Width: 32; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 208; ID: 10; XPos: 183;
    YPos: 13; Width: 21; MasterRef: 3; TextFunc: 2; slDependencies: ''),
    (ModuleID: 208; ID: 12; XPos: 65; YPos: 13; Width: 22; MasterRef: 0;
    TextFunc: 0; slDependencies: ''), (ModuleID: 208; ID: 8; XPos: 65; YPos: 28;
    Width: 22; MasterRef: 1; TextFunc: 0; slDependencies: ''), (ModuleID: 208;
    ID: 25; XPos: 147; YPos: 4; Width: 32; MasterRef: 2; TextFunc: 0;
    slDependencies: ''));

  TextEditDefs: array [0 .. 27] of TG2TextEditDef = ((ModuleID: 36; ID: 3;
    XPos: 152; YPos: 10; CodeRef: 0; InfoFunc: 3; slText: 'On';
    TextEditType: bttPush; Width: 43), (ModuleID: 50; ID: 2; XPos: 111; YPos: 8;
    CodeRef: 1; InfoFunc: 3; slText: 'Switch'; TextEditType: bttCheck;
    Width: 43), (ModuleID: 76; ID: 2; XPos: 152; YPos: 10; CodeRef: 0;
    InfoFunc: 3; slText: 'On'; TextEditType: bttCheck; Width: 43),
    (ModuleID: 123; ID: 16; XPos: 41; YPos: 42; CodeRef: 4; InfoFunc: 3;
    slText: 'Ch 1'; TextEditType: bttCheck; Width: 43), (ModuleID: 123; ID: 17;
    XPos: 89; YPos: 42; CodeRef: 5; InfoFunc: 3; slText: 'Ch 2';
    TextEditType: bttCheck; Width: 43), (ModuleID: 123; ID: 18; XPos: 137;
    YPos: 42; CodeRef: 6; InfoFunc: 3; slText: 'Ch 3'; TextEditType: bttCheck;
    Width: 43), (ModuleID: 123; ID: 19; XPos: 185; YPos: 42; CodeRef: 7;
    InfoFunc: 3; slText: 'Ch 4'; TextEditType: bttCheck; Width: 43),
    (ModuleID: 140; ID: 17; XPos: 7; YPos: 42; CodeRef: 4; InfoFunc: 3;
    slText: 'Ch 1'; TextEditType: bttCheck; Width: 43), (ModuleID: 140; ID: 7;
    XPos: 61; YPos: 42; CodeRef: 5; InfoFunc: 3; slText: 'Ch 2';
    TextEditType: bttCheck; Width: 43), (ModuleID: 140; ID: 18; XPos: 115;
    YPos: 42; CodeRef: 6; InfoFunc: 3; slText: 'Ch 3'; TextEditType: bttCheck;
    Width: 43), (ModuleID: 140; ID: 24; XPos: 169; YPos: 42; CodeRef: 7;
    InfoFunc: 3; slText: 'Ch 4'; TextEditType: bttCheck; Width: 43),
    (ModuleID: 161; ID: 1; XPos: 4; YPos: 117; CodeRef: 8; InfoFunc: 3;
    slText: 'Ch1'; TextEditType: bttCheck; Width: 27), (ModuleID: 161; ID: 26;
    XPos: 33; YPos: 117; CodeRef: 9; InfoFunc: 3; slText: 'Ch2';
    TextEditType: bttCheck; Width: 27), (ModuleID: 161; ID: 27; XPos: 62;
    YPos: 117; CodeRef: 10; InfoFunc: 3; slText: 'Ch3'; TextEditType: bttCheck;
    Width: 27), (ModuleID: 161; ID: 28; XPos: 91; YPos: 117; CodeRef: 11;
    InfoFunc: 3; slText: 'Ch4'; TextEditType: bttCheck; Width: 27),
    (ModuleID: 161; ID: 29; XPos: 120; YPos: 117; CodeRef: 12; InfoFunc: 3;
    slText: 'Ch5'; TextEditType: bttCheck; Width: 27), (ModuleID: 161; ID: 30;
    XPos: 149; YPos: 117; CodeRef: 13; InfoFunc: 3; slText: 'Ch6';
    TextEditType: bttCheck; Width: 27), (ModuleID: 161; ID: 31; XPos: 178;
    YPos: 117; CodeRef: 14; InfoFunc: 3; slText: 'Ch7'; TextEditType: bttCheck;
    Width: 27), (ModuleID: 161; ID: 32; XPos: 207; YPos: 117; CodeRef: 15;
    InfoFunc: 3; slText: 'Ch8'; TextEditType: bttCheck; Width: 27),
    (ModuleID: 184; ID: 17; XPos: 112; YPos: 8; CodeRef: 1; InfoFunc: 3;
    slText: 'Ch 1'; TextEditType: bttCheck; Width: 43), (ModuleID: 185; ID: 17;
    XPos: 110; YPos: 8; CodeRef: 1; InfoFunc: 3; slText: 'Ch 1';
    TextEditType: bttCheck; Width: 43), (ModuleID: 186; ID: 7; XPos: 111;
    YPos: 8; CodeRef: 0; InfoFunc: 3; slText: 'Switch'; TextEditType: bttPush;
    Width: 43), (ModuleID: 187; ID: 6; XPos: 111; YPos: 8; CodeRef: 0;
    InfoFunc: 3; slText: 'Switch'; TextEditType: bttPush; Width: 43),
    (ModuleID: 188; ID: 2; XPos: 111; YPos: 8; CodeRef: 1; InfoFunc: 3;
    slText: 'Switch'; TextEditType: bttPush; Width: 43), (ModuleID: 194; ID: 3;
    XPos: 77; YPos: 8; CodeRef: 1; InfoFunc: 3; slText: 'Ch 1';
    TextEditType: bttCheck; Width: 40), (ModuleID: 194; ID: 6; XPos: 159;
    YPos: 8; CodeRef: 3; InfoFunc: 3; slText: 'Ch 2'; TextEditType: bttCheck;
    Width: 40), (ModuleID: 195; ID: 3; XPos: 123; YPos: 8; CodeRef: 1;
    InfoFunc: 3; slText: 'On'; TextEditType: bttCheck; Width: 43),
    (ModuleID: 204; ID: 9; XPos: 122; YPos: 8; CodeRef: 2; InfoFunc: 3;
    slText: 'Dice'; TextEditType: bttCheck; Width: 38));

  ButtonIncDecDefs: array [0 .. 72] of TG2ButtonIncDecDef = ((ModuleID: 17;
    ID: 12; XPos: 82; YPos: 15; CodeRef: 0; InfoFunc: 114;
    ButtonType: bidLeftRight), (ModuleID: 45; ID: 5; XPos: 157; YPos: 12;
    CodeRef: 1; InfoFunc: 78; ButtonType: bidUpDown), (ModuleID: 45; ID: 7;
    XPos: 194; YPos: 12; CodeRef: 2; InfoFunc: 78; ButtonType: bidUpDown),
    (ModuleID: 45; ID: 9; XPos: 120; YPos: 12; CodeRef: 0; InfoFunc: 78;
    ButtonType: bidUpDown), (ModuleID: 57; ID: 6; XPos: 240; YPos: 5;
    CodeRef: 2; InfoFunc: 108; ButtonType: bidUpDown), (ModuleID: 57; ID: 8;
    XPos: 134; YPos: 5; CodeRef: 0; InfoFunc: 179; ButtonType: bidUpDown),
    (ModuleID: 68; ID: 18; XPos: 165; YPos: 35; CodeRef: 3; InfoFunc: 166;
    ButtonType: bidUpDown), (ModuleID: 69; ID: 7; XPos: 186; YPos: 5;
    CodeRef: 0; InfoFunc: 2; ButtonType: bidUpDown), (ModuleID: 75; ID: 4;
    XPos: 171; YPos: 5; CodeRef: 1; InfoFunc: 11; ButtonType: bidUpDown),
    (ModuleID: 102; ID: 26; XPos: 184; YPos: 50; CodeRef: 4; InfoFunc: 2;
    ButtonType: bidUpDown), (ModuleID: 105; ID: 13; XPos: 82; YPos: 15;
    CodeRef: 0; InfoFunc: 114; ButtonType: bidLeftRight), (ModuleID: 108;
    ID: 27; XPos: 37; YPos: 75; CodeRef: 0; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 108; ID: 28; XPos: 49; YPos: 75;
    CodeRef: 1; InfoFunc: 79; ButtonType: bidUpDown), (ModuleID: 108; ID: 29;
    XPos: 61; YPos: 75; CodeRef: 2; InfoFunc: 79; ButtonType: bidUpDown),
    (ModuleID: 108; ID: 30; XPos: 73; YPos: 75; CodeRef: 3; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 108; ID: 31; XPos: 85; YPos: 75;
    CodeRef: 4; InfoFunc: 79; ButtonType: bidUpDown), (ModuleID: 108; ID: 32;
    XPos: 97; YPos: 75; CodeRef: 5; InfoFunc: 79; ButtonType: bidUpDown),
    (ModuleID: 108; ID: 33; XPos: 109; YPos: 75; CodeRef: 6; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 108; ID: 34; XPos: 121; YPos: 75;
    CodeRef: 7; InfoFunc: 79; ButtonType: bidUpDown), (ModuleID: 108; ID: 36;
    XPos: 133; YPos: 75; CodeRef: 8; InfoFunc: 79; ButtonType: bidUpDown),
    (ModuleID: 108; ID: 37; XPos: 145; YPos: 75; CodeRef: 9; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 108; ID: 38; XPos: 157; YPos: 75;
    CodeRef: 10; InfoFunc: 79; ButtonType: bidUpDown), (ModuleID: 108; ID: 39;
    XPos: 169; YPos: 75; CodeRef: 11; InfoFunc: 79; ButtonType: bidUpDown),
    (ModuleID: 108; ID: 40; XPos: 181; YPos: 75; CodeRef: 12; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 108; ID: 41; XPos: 193; YPos: 75;
    CodeRef: 13; InfoFunc: 79; ButtonType: bidUpDown), (ModuleID: 108; ID: 42;
    XPos: 205; YPos: 75; CodeRef: 14; InfoFunc: 79; ButtonType: bidUpDown),
    (ModuleID: 108; ID: 43; XPos: 217; YPos: 75; CodeRef: 15; InfoFunc: 79;
    ButtonType: bidUpDown), (ModuleID: 115; ID: 4; XPos: 130; YPos: 20;
    CodeRef: 1; InfoFunc: 13; ButtonType: bidUpDown), (ModuleID: 118; ID: 7;
    XPos: 206; YPos: 20; CodeRef: 0; InfoFunc: 195; ButtonType: bidUpDown),
    (ModuleID: 121; ID: 81; XPos: 158; YPos: 4; CodeRef: 33; InfoFunc: 2;
    ButtonType: bidLeftRight), (ModuleID: 141; ID: 6; XPos: 240; YPos: 5;
    CodeRef: 2; InfoFunc: 108; ButtonType: bidUpDown), (ModuleID: 141; ID: 8;
    XPos: 121; YPos: 5; CodeRef: 0; InfoFunc: 179; ButtonType: bidUpDown),
    (ModuleID: 142; ID: 7; XPos: 240; YPos: 5; CodeRef: 1; InfoFunc: 108;
    ButtonType: bidUpDown), (ModuleID: 143; ID: 5; XPos: 240; YPos: 5;
    CodeRef: 2; InfoFunc: 108; ButtonType: bidUpDown), (ModuleID: 144; ID: 80;
    XPos: 145; YPos: 4; CodeRef: 33; InfoFunc: 2; ButtonType: bidLeftRight),
    (ModuleID: 145; ID: 81; XPos: 145; YPos: 4; CodeRef: 33; InfoFunc: 2;
    ButtonType: bidLeftRight), (ModuleID: 145; ID: 13; XPos: 29; YPos: 79;
    CodeRef: 0; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 145; ID: 14;
    XPos: 41; YPos: 79; CodeRef: 1; InfoFunc: 0; ButtonType: bidUpDown),
    (ModuleID: 145; ID: 15; XPos: 53; YPos: 79; CodeRef: 2; InfoFunc: 0;
    ButtonType: bidUpDown), (ModuleID: 145; ID: 16; XPos: 65; YPos: 79;
    CodeRef: 3; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 145; ID: 17;
    XPos: 77; YPos: 79; CodeRef: 4; InfoFunc: 0; ButtonType: bidUpDown),
    (ModuleID: 145; ID: 18; XPos: 89; YPos: 79; CodeRef: 5; InfoFunc: 0;
    ButtonType: bidUpDown), (ModuleID: 145; ID: 19; XPos: 101; YPos: 79;
    CodeRef: 6; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 145; ID: 20;
    XPos: 113; YPos: 79; CodeRef: 7; InfoFunc: 0; ButtonType: bidUpDown),
    (ModuleID: 145; ID: 21; XPos: 125; YPos: 79; CodeRef: 8; InfoFunc: 0;
    ButtonType: bidUpDown), (ModuleID: 145; ID: 22; XPos: 137; YPos: 79;
    CodeRef: 9; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 145; ID: 23;
    XPos: 149; YPos: 79; CodeRef: 10; InfoFunc: 0; ButtonType: bidUpDown),
    (ModuleID: 145; ID: 24; XPos: 161; YPos: 79; CodeRef: 11; InfoFunc: 0;
    ButtonType: bidUpDown), (ModuleID: 145; ID: 25; XPos: 173; YPos: 79;
    CodeRef: 12; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 145; ID: 27;
    XPos: 185; YPos: 79; CodeRef: 13; InfoFunc: 0; ButtonType: bidUpDown),
    (ModuleID: 145; ID: 61; XPos: 197; YPos: 79; CodeRef: 14; InfoFunc: 0;
    ButtonType: bidUpDown), (ModuleID: 145; ID: 63; XPos: 209; YPos: 79;
    CodeRef: 15; InfoFunc: 0; ButtonType: bidUpDown), (ModuleID: 146; ID: 81;
    XPos: 145; YPos: 4; CodeRef: 33; InfoFunc: 2; ButtonType: bidLeftRight),
    (ModuleID: 147; ID: 5; XPos: 171; YPos: 5; CodeRef: 1; InfoFunc: 109;
    ButtonType: bidUpDown), (ModuleID: 147; ID: 7; XPos: 118; YPos: 5;
    CodeRef: 0; InfoFunc: 179; ButtonType: bidUpDown), (ModuleID: 148; ID: 9;
    XPos: 171; YPos: 5; CodeRef: 1; InfoFunc: 109; ButtonType: bidUpDown),
    (ModuleID: 149; ID: 3; XPos: 53; YPos: 20; CodeRef: 0; InfoFunc: 109;
    ButtonType: bidUpDown), (ModuleID: 149; ID: 5; XPos: 93; YPos: 20;
    CodeRef: 1; InfoFunc: 13; ButtonType: bidUpDown), (ModuleID: 149; ID: 8;
    XPos: 133; YPos: 20; CodeRef: 2; InfoFunc: 13; ButtonType: bidUpDown),
    (ModuleID: 149; ID: 13; XPos: 196; YPos: 20; CodeRef: 3; InfoFunc: 17;
    ButtonType: bidUpDown), (ModuleID: 149; ID: 17; XPos: 240; YPos: 20;
    CodeRef: 4; InfoFunc: 108; ButtonType: bidUpDown), (ModuleID: 165; ID: 21;
    XPos: 83; YPos: 37; CodeRef: 0; InfoFunc: 2; ButtonType: bidUpDown),
    (ModuleID: 165; ID: 14; XPos: 33; YPos: 37; CodeRef: 1; InfoFunc: 179;
    ButtonType: bidUpDown), (ModuleID: 180; ID: 39; XPos: 221; YPos: 16;
    CodeRef: 5; InfoFunc: 196; ButtonType: bidUpDown), (ModuleID: 180; ID: 44;
    XPos: 198; YPos: 51; CodeRef: 6; InfoFunc: 179; ButtonType: bidUpDown),
    (ModuleID: 180; ID: 46; XPos: 234; YPos: 51; CodeRef: 7; InfoFunc: 179;
    ButtonType: bidUpDown), (ModuleID: 180; ID: 56; XPos: 18; YPos: 123;
    CodeRef: 16; InfoFunc: 179; ButtonType: bidUpDown), (ModuleID: 180; ID: 64;
    XPos: 193; YPos: 155; CodeRef: 21; InfoFunc: 179; ButtonType: bidUpDown),
    (ModuleID: 180; ID: 68; XPos: 125; YPos: 155; CodeRef: 19; InfoFunc: 179;
    ButtonType: bidUpDown), (ModuleID: 180; ID: 72; XPos: 57; YPos: 155;
    CodeRef: 17; InfoFunc: 197; ButtonType: bidUpDown), (ModuleID: 208; ID: 11;
    XPos: 183; YPos: 30; CodeRef: 3; InfoFunc: 2; ButtonType: bidLeftRight),
    (ModuleID: 208; ID: 23; XPos: 89; YPos: 15; CodeRef: 0; InfoFunc: 17;
    ButtonType: bidLeftRight), (ModuleID: 208; ID: 24; XPos: 89; YPos: 30;
    CodeRef: 1; InfoFunc: 17; ButtonType: bidLeftRight));

  LevelShiftDefs: array [0 .. 13] of TG2LevelShiftDef = ((ModuleID: 20; ID: 3;
    XPos: 204; YPos: 44; CodeRef: 5; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 23; ID: 16; XPos: 200;
    YPos: 59; CodeRef: 8; InfoFunc: 46; slImageID: '27;28;29;30;25;26';
    ), (ModuleID: 24; ID: 1; XPos: 222; YPos: 14; CodeRef: 2; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 25; ID: 22; XPos: 206;
    YPos: 59; CodeRef: 10; InfoFunc: 46; slImageID: '27;28;29;30;25;26';
    ), (ModuleID: 26; ID: 14; XPos: 206; YPos: 29; CodeRef: 6; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 41; ID: 12; XPos: 176;
    YPos: 14; CodeRef: 1; InfoFunc: 46; slImageID: '27;28;29;30;25;26';
    ), (ModuleID: 46; ID: 12; XPos: 203; YPos: 44; CodeRef: 5; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 52; ID: 42; XPos: 205;
    YPos: 20; CodeRef: 10; InfoFunc: 46; slImageID: '27;28;29;30;25;26';
    ), (ModuleID: 55; ID: 13; XPos: 176; YPos: 14; CodeRef: 1; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 84; ID: 3; XPos: 167; YPos: 29;
    CodeRef: 5; InfoFunc: 46; slImageID: '27;28;29;30;25;26';
    ), (ModuleID: 119; ID: 11; XPos: 205; YPos: 20; CodeRef: 9; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 157; ID: 1; XPos: 223;
    YPos: 10; CodeRef: 0; InfoFunc: 46; slImageID: '25;26;27;28;29;30';
    ), (ModuleID: 169; ID: 12; XPos: 200; YPos: 59; CodeRef: 6; InfoFunc: 46;
    slImageID: '27;28;29;30;25;26';), (ModuleID: 190; ID: 10; XPos: 206;
    YPos: 44; CodeRef: 8; InfoFunc: 46; slImageID: '27;28;29;30;25;26';));

  ButtonRadioEditDefs: array [0 .. 5] of TG2ButtonRadioEditDef = ((ModuleID: 15;
    ID: 0; XPos: 67; YPos: 29; CodeRef: 0; InfoFunc: 64; ButtonColumns: 4;
    ButtonRows: 2; slText: 'In 1;In 2;In 3;In 4;In 5;In 6;In 7;In 8'),
    (ModuleID: 78; ID: 0; XPos: 82; YPos: 4; CodeRef: 0; InfoFunc: 64;
    ButtonColumns: 4; ButtonRows: 2;
    slText: 'Out 1;Out 2;Out 3;Out 4;Out 5;Out 6;Out 7;Out 8'), (ModuleID: 79;
    ID: 0; XPos: 67; YPos: 27; CodeRef: 0; InfoFunc: 64; ButtonColumns: 4;
    ButtonRows: 1; slText: 'In 1;In 2;In 3;In 4'), (ModuleID: 88; ID: 4;
    XPos: 82; YPos: 10; CodeRef: 0; InfoFunc: 64; ButtonColumns: 4;
    ButtonRows: 1; slText: 'Out 1;Out 2;Out 3;Out 4'), (ModuleID: 90; ID: 7;
    XPos: 83; YPos: 8; CodeRef: 0; InfoFunc: 64; ButtonColumns: 2;
    ButtonRows: 1; slText: 'Out 1;Out 2'), (ModuleID: 100; ID: 11; XPos: 83;
    YPos: 8; CodeRef: 0; InfoFunc: 64; ButtonColumns: 2; ButtonRows: 1;
    slText: 'In 1;In 2'));

  PartSelectorDefs: array [0 .. 24] of TG2PartSelectorDef = ((ModuleID: 8;
    ID: 20; XPos: 210; YPos: 4; CodeRef: 0; InfoFunc: 0; Width: 41; Height: 22;
    slText: ''; slImageID: '18;17;16;15;14;11;12'; ImageCount: 8;
    ImageWidth: 33), (ModuleID: 9; ID: 10; XPos: 220; YPos: 4; CodeRef: 0;
    InfoFunc: 0; Width: 31; Height: 22; slText: ''; slImageID: '5;6;7;8;9;10';
    ImageCount: 6; ImageWidth: 22), (ModuleID: 12; ID: 13; XPos: 4; YPos: 27;
    CodeRef: 0; InfoFunc: 90; Width: 50; Height: 14;
    slText: 'Small;Medium;Large;Hall'; slImageID: ''; ImageCount: 4;
    ImageWidth: 48), (ModuleID: 24; ID: 6; XPos: 189; YPos: 4; CodeRef: 0;
    InfoFunc: 0; Width: 31; Height: 22; slText: '';
    slImageID: '5;6;7;8;21;22;23;24'; ImageCount: 8; ImageWidth: 22),
    (ModuleID: 35; ID: 7; XPos: 71; YPos: 22; CodeRef: 0; InfoFunc: 0;
    Width: 47; Height: 17; slText: 'Reed;Bow;Lip;Malet'; slImageID: '';
    ImageCount: 4; ImageWidth: 33), (ModuleID: 38; ID: 6; XPos: 207; YPos: 4;
    CodeRef: 0; InfoFunc: 0; Width: 29; Height: 22; slText: '';
    slImageID: '40;41'; ImageCount: 2; ImageWidth: 25), (ModuleID: 42; ID: 7;
    XPos: 207; YPos: 4; CodeRef: 0; InfoFunc: 0; Width: 29; Height: 22;
    slText: ''; slImageID: '42;43;44'; ImageCount: 3; ImageWidth: 27),
    (ModuleID: 64; ID: 2; XPos: 188; YPos: 4; CodeRef: 1; InfoFunc: 0;
    Width: 39; Height: 22; slText: 'AND;NAND;OR;NOR;XOR;XNOR'; slImageID: '';
    ImageCount: 6; ImageWidth: 90), (ModuleID: 64; ID: 9; XPos: 95; YPos: 4;
    CodeRef: 0; InfoFunc: 0; Width: 39; Height: 22;
    slText: 'AND;NAND;OR;NOR;XOR;XNOR'; slImageID: ''; ImageCount: 6;
    ImageWidth: 90), (ModuleID: 69; ID: 10; XPos: 208; YPos: 4; CodeRef: 0;
    InfoFunc: 0; Width: 29; Height: 22; slText: ''; slImageID: '45;46';
    ImageCount: 2; ImageWidth: 25), (ModuleID: 87; ID: 8; XPos: 168; YPos: 4;
    CodeRef: 0; InfoFunc: 0; Width: 40; Height: 22; slText: '';
    slImageID: '55;56;57;58;59;60'; ImageCount: 6; ImageWidth: 30),
    (ModuleID: 91; ID: 0; XPos: 159; YPos: 3; CodeRef: 0; InfoFunc: 0;
    Width: 42; Height: 24; slText: ''; slImageID: '19;20'; ImageCount: 2;
    ImageWidth: 42), (ModuleID: 96; ID: 6; XPos: 205; YPos: 4; CodeRef: 0;
    InfoFunc: 0; Width: 31; Height: 22; slText: ''; slImageID: '5;6;7;8;9;10';
    ImageCount: 6; ImageWidth: 22), (ModuleID: 134; ID: 9; XPos: 168; YPos: 4;
    CodeRef: 0; InfoFunc: 0; Width: 40; Height: 22; slText: '';
    slImageID: '61;62;63;64;65;66'; ImageCount: 6; ImageWidth: 30),
    (ModuleID: 172; ID: 0; XPos: 40; YPos: 13; CodeRef: 0; InfoFunc: 0;
    Width: 40; Height: 13; slText: '5m;25m;100m;500m;1.0s;2.0s;2.7s';
    slImageID: ''; ImageCount: 7; ImageWidth: 30), (ModuleID: 173; ID: 1;
    XPos: 40; YPos: 13; CodeRef: 0; InfoFunc: 0; Width: 40; Height: 13;
    slText: '5m;25m;100m;500m;1.0s;2.0s;2.7s'; slImageID: ''; ImageCount: 7;
    ImageWidth: 30), (ModuleID: 174; ID: 0; XPos: 4; YPos: 27; CodeRef: 0;
    InfoFunc: 0; Width: 40; Height: 14;
    slText: '5m;25m;100m;500m;1.0s;2.0s;2.7s'; slImageID: ''; ImageCount: 7;
    ImageWidth: 30), (ModuleID: 175; ID: 30; XPos: 4; YPos: 57; CodeRef: 0;
    InfoFunc: 0; Width: 40; Height: 14;
    slText: '5m;25m;100m;500m;1.0s;2.0s;2.7s'; slImageID: ''; ImageCount: 7;
    ImageWidth: 30), (ModuleID: 176; ID: 0; XPos: 4; YPos: 27; CodeRef: 0;
    InfoFunc: 0; Width: 40; Height: 14; slText: '500m;1.0s;2.0s;2.7s';
    slImageID: ''; ImageCount: 4; ImageWidth: 30), (ModuleID: 177; ID: 1;
    XPos: 4; YPos: 28; CodeRef: 0; InfoFunc: 0; Width: 40; Height: 14;
    slText: '500m;1.0s;2.0s;2.7s'; slImageID: ''; ImageCount: 4;
    ImageWidth: 30), (ModuleID: 181; ID: 13; XPos: 40; YPos: 20; CodeRef: 0;
    InfoFunc: 0; Width: 40; Height: 14;
    slText: '5m;25m;100m;500m;1.0s;2.0s;2.7s'; slImageID: ''; ImageCount: 7;
    ImageWidth: 30), (ModuleID: 182; ID: 25; XPos: 158; YPos: 57; CodeRef: 0;
    InfoFunc: 0; Width: 40; Height: 14; slText: '500m;1.0s;1.35s';
    slImageID: ''; ImageCount: 3; ImageWidth: 30), (ModuleID: 183; ID: 10;
    XPos: 220; YPos: 4; CodeRef: 0; InfoFunc: 0; Width: 31; Height: 22;
    slText: ''; slImageID: '5;6'; ImageCount: 2; ImageWidth: 22),
    (ModuleID: 206; ID: 10; XPos: 79; YPos: 27; CodeRef: 0; InfoFunc: 0;
    Width: 41; Height: 14; slText: 'Rnd 1;Rnd 2'; slImageID: ''; ImageCount: 2;
    ImageWidth: 30), (ModuleID: 208; ID: 26; XPos: 208; YPos: 4; CodeRef: 0;
    InfoFunc: 0; Width: 43; Height: 20; slText: ''; slImageID: '31;32';
    ImageCount: 2; ImageWidth: 33));

function GetDataModuleDef(aTypeID: byte): TG2ModuleDef;
function GetDataParamDefIndex(aParamID: Integer): Integer;
function GetDataModuleIndex(aModuleType: Integer): Integer;
function GetDataModuleInputIndex(aModuleType, aConnectorIndex: Integer): Integer;
function GetDataModuleOutputIndex(aModuleType, aConnectorIndex: Integer): Integer;
function GetDataModuleParamIndex(aModuleType, aParamIndex: Integer): Integer;
function GetDataModuleModeIndex(aModuleType, aModeIndex: Integer): Integer;
function GetDataInputCount(aModuleType: Integer): Integer;
function GetDataOutputCount(aModuleType: Integer): Integer;
function GetDataParamCount(aModuleType: Integer): Integer;
function GetDataModeCount(aModuleType: Integer): Integer;
function GetDataLedCount(aModuleType: Integer): Integer;
function GetDataMiniVUCount(aModuleType: Integer): Integer;
function GetDataLedsInGroup(aModuleType, aGroupID: Integer): Integer;

implementation

function GetDataModuleDef(aTypeID: byte): TG2ModuleDef;
begin
  Result := ModuleDefs[GetDataModuleIndex(aTypeID)];
end;

function GetDataLedsInGroup(aModuleType, aGroupID: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(LedDefs) do
    if (LedDefs[i].ModuleID = aModuleType) and (LedDefs[i].GroupID = aGroupID)
    then
      inc(Result);
end;

function GetDataLedCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(LedDefs) do
    if LedDefs[i].ModuleID = aModuleType then
      inc(Result);
end;

function GetDataMiniVUCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(MiniVUDefs) do
    if MiniVUDefs[i].ModuleID = aModuleType then
      inc(Result);
end;

function GetDataParamDefIndex(aParamID: Integer): Integer;
begin
  Result := 0;
  while (Result <= High(ParamDefs)) and
    not(ParamDefs[Result].ParamID = aParamID) do
    inc(Result);
  if Result > High(ParamDefs) then
    raise Exception.Create('ParamID ' + IntToStr(aParamID) + ' not found.');
end;

function GetDataModuleIndex(aModuleType: Integer): Integer;
begin
  Result := 0;
  while (Result <= High(ModuleDefs)) and
    not(ModuleDefs[Result].ModuleID = aModuleType) do
    inc(Result);
  if Result > High(ModuleDefs) then
    raise Exception.Create('ModuleID ' + IntToStr(aModuleType) + ' not found.');
end;

function GetDataModuleInputIndex(aModuleType, aConnectorIndex: Integer)
  : Integer;
begin
  Result := 0;
  while (Result <= High(ModuleInputs)) and
    not((ModuleInputs[Result].ModuleID = aModuleType) and
    (ModuleInputs[Result].ConnectorIndex = aConnectorIndex)) do
    inc(Result);
  if Result > High(ModuleInputs) then
    raise Exception.Create('ModuleID ' + IntToStr(aModuleType) + ', Input ' +
      IntToStr(aConnectorIndex) + ' not found.');
end;

function GetDataModuleOutputIndex(aModuleType, aConnectorIndex: Integer): Integer;
begin
  Result := 0;
  while (Result <= High(ModuleOutputs)) and
    not((ModuleOutputs[Result].ModuleID = aModuleType) and
    (ModuleOutputs[Result].ConnectorIndex = aConnectorIndex)) do
    inc(Result);
  if Result > High(ModuleOutputs) then
    raise Exception.Create('ModuleID ' + IntToStr(aModuleType) + ', Output ' +
      IntToStr(aConnectorIndex) + ' not found.');
end;

function GetDataModuleParamIndex(aModuleType, aParamIndex: Integer): Integer;
begin
  Result := 0;
  while (Result <= High(ModuleParams)) and
    not((ModuleParams[Result].ModuleID = aModuleType) and
    (ModuleParams[Result].ParamIndex = aParamIndex)) do
    inc(Result);
  if Result > High(ModuleParams) then
    raise Exception.Create('ModuleID ' + IntToStr(aModuleType) + ', Param ' +
      IntToStr(aParamIndex) + ' not found.');
end;

function GetDataModuleModeIndex(aModuleType, aModeIndex: Integer): Integer;
begin
  Result := 0;
  while (Result <= High(ModuleModes)) and
    not((ModuleModes[Result].ModuleID = aModuleType) and
    (ModuleModes[Result].ParamIndex = aModeIndex)) do
    inc(Result);
  if Result > High(ModuleModes) then
    raise Exception.Create('ModuleID ' + IntToStr(aModuleType) + ', Mode ' +
      IntToStr(aModeIndex) + ' not found.');
end;

function GetDataInputCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(ModuleInputs) do
    if ModuleInputs[i].ModuleID = aModuleType then
      inc(Result);
end;

function GetDataOutputCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(ModuleOutputs) do
    if ModuleOutputs[i].ModuleID = aModuleType then
      inc(Result);
end;

function GetDataParamCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(ModuleParams) do
    if ModuleParams[i].ModuleID = aModuleType then
      inc(Result);
end;

function GetDataModeCount(aModuleType: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to High(ModuleModes) do
    if ModuleModes[i].ModuleID = aModuleType then
      inc(Result);
end;

end.
