unit BVE.NMG2Types;

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
// ////////////////////////////////////////////////////////////////////////////
//
// This unit contains the G2 constants and types and some utility functions.
//
// ////////////////////////////////////////////////////////////////////////////

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

{$I CompilerSettings.Inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.math,
  System.Character;

const
  FILE_PATCH_EXT = '.pch2';
  FILE_PERF_EXT = '.prf2';

  NMG2_VERSION = '0.26';
  PATCH_VERSION = 23;
  NMORPHS = 8;
  N_VARIATIONS = 9;
  N_USB_VARIATIONS = 10;
  NSLOTS = 4;
  VENDOR_ID = $FFC; // clavia, g2
  PRODUCT_ID = 2;
  PATCH_DATA = $00;
  PERF_DATA = $01;
  LOCATION_FX = 0;
  LOCATION_VA = 1;
  LOCATION_PATCH_SETTINGS = 2;
  XCL_CONTROL_HIGHLIGHT = $00FFFF00;
  XCL_LED = $0000FF00;
  CL_SELECTED_PATCH = $00B6B6B6;
  CL_SELECTED_PARAM = $00FFFFFF;
  CL_KNOB_MORPH = $00F4948C;
  CL_KNOB_MORPH_SELECTED = $008582F4;
  CL_DISPLAY_BACKGRND = $00555555;
  CL_BTN_FACE = $00D0D0D0;
  CL_BTN_BORDER = $00404040;
  XCL_CLAVIA_RED = $00423FBE; // $001620D1;//$006550DC;//$005A5692;
  XCL_CLAVIA_BLUE = $00531F00;
  CL_MIDI_AWARE_BOX = $00BBBBFF;
  // Module dimensions
  UNITS_COL = 255;
  UNITS_ROW = 15;
  UNIT_MARGIN = 2;
  COLOR_RED = 0;
  COLOR_BLUE = 1;
  COLOR_YELLOW = 2;
  COLOR_ORANGE = 3;
  COLOR_GREEN = 4;
  COLOR_PURPLE = 5;
  COLOR_WHITE = 6;
  // Cable simulation parameters
  CABLE_CONTROL_MARGIN = 6;
  NCABLE_ELEMENTS = 16;
  CABLE_THICKNESS = 2;
  TENSION = 10;
  DAMP = 0.8;
  GRAVITY = 0.04;
  MAX_BULK_DATA_SIZE = 4096;
  MAX_INTERRUPT_DATA_SIZE = 4096;
  LOGCMD_NUL = $00;
  LOGCMD_HDR = $01;
  LOGCMD_ERR = $02;
  LOGCMD_TAB = $03;
  LOGCMD_SAV = $04;
  CMD_SLOT = $08;
  CMD_SYS = $0C;
  CMD_INIT = $80;
  CMD_CUST = $0B; // Some custom message i use between server and client
  CMD_REQ = $20;
  CMD_RESP = $00;
  CMD_NO_RESP = $30;

const
  Q_SYNTH_SETTINGS = $02;

  S_SYNTH_SETTINGS = $03;
  Q_ASSIGNED_VOICES = $04;
  R_ASSIGNED_VOICES = $05;
  // $06
  // $07
  // $08
  S_SEL_SLOT = $09;
  S_RETREIVE = $0A;
  S_STORE = $0B;
  S_CLEAR = $0C;
  R_STORE = $0D; // ?
  S_CLEAR_BANK = $0E;
  // $0f
  Q_PERF_SETTINGS = $10;
  C_PERF_SETTINGS = $11;
  R_CLEAR_BANK = $12;
  R_LIST_NAMES = $13;
  Q_LIST_NAMES = $14;
  R_CLEAR = $15;
  R_ADD_NAMES = $16;
  S_PATCH_BANK_UPLOAD = $17;
  R_PATCH_BANK_UPDLOAD = $18;
  S_PATCH_BANK_DATA = $19;
  // $1a
  // $1b
  S_ASS_GLOBAL_KNOB = $1C;
  S_DEASS_GLOB_KNOB = $1D;
  S_SEL_GLOBAL_PAGE = $1E;
  // $1f
  // $20
  C_PATCH_DESCR = $21;
  S_ASSIGN_MIDICC = $22;
  S_DEASSIGN_MIDICC = $23;
  // $24
  S_ASSIGN_KNOB = $25;
  S_DEASSIGN_KNOB = $26;
  S_PATCH_NAME = $27;
  Q_PATCH_NAME = $28;
  C_PERF_NAME = $29;
  S_SET_UPRATE = $2A;
  S_SET_MODE = $2B;
  // $2c
  S_SEL_PARAM_PAGE = $2D;
  Q_SELECTED_PARAM = $2E;
  S_SEL_PARAM = $2F;
  S_ADD_MODULE = $30;
  S_SET_MODULE_COLOR = $31;
  S_DEL_MODULE = $32;
  S_SET_MODULE_LABEL = $33;
  S_MOV_MODULE = $34;
  Q_VERSION_CNT = $35;
  // $36
  S_SET_PATCH = $37;
  R_PATCH_VERSION_CHANGE = $38; // After upload patch through midi
  R_LED_DATA = $39;
  R_VOLUME_DATA = $3A;
  Q_MASTER_CLOCK = $3B;
  Q_PATCH = $3C;
  S_MIDI_DUMP = $3D;
  S_SET_PARAM_MODE = $3E;
  S_SET_MASTER_CLOCK = $3F;
  S_SET_PARAM = $40;
  // $41
  S_SET_PARAM_LABEL = $42;
  S_SET_MORPH_RANGE = $43;
  S_COPY_VARIATION = $44;
  // $45
  // $46
  // $47
  // $48
  // $49
  C_MODULE_LIST = $4A;
  // $4b
  Q_PARAMS = $4C;
  C_PARAM_LIST = $4D;
  // $4e
  Q_PARAM_NAMES = $4F;
  S_ADD_CABLE = $50;
  S_DEL_CABLE = $51;
  C_CABLE_LIST = $52;
  // $53
  S_CABLE_COLOR = $54;
  S_CTRL_SNAPSHOT = $55;
  S_PLAY_NOTE = $56;
  // $57
  // $58
  M_UNKNOWN_2 = $59;
  C_MODULE_NAMES = $5A;
  C_PARAM_NAMES = $5B;
  // $5c
  R_EXT_MASTER_CLOCK = $5D;
  Q_GLOBAL_KNOBS = $5E;
  C_KNOBS_GLOBAL = $5F;
  C_CONTROLLERS = $60;
  // $61
  C_KNOBS = $62;
  // $63
  // $64
  C_MORPH_PARAM = $65;
  // $66
  // $67
  Q_CURRENT_NOTE = $68;
  C_CURRENT_NOTE_2 = $69;
  S_SEL_VARIATION = $6A;
  // $6b
  // $6c
  // $6d
  Q_PATCH_TEXT = $6E;
  C_PATCH_NOTES = $6F;
  M_UNKNOWN_6 = $70;
  Q_RESOURCES_USED = $71;
  R_RESOURCES_USED = $72;
  // 73
  // 74
  // 75
  // 76
  // 77
  // 78
  // 79
  // 7a
  // 7b
  // 7c
  S_START_STOP_COM = $7D;
  R_ERROR = $7E;
  R_OK = $7F;
  R_MIDI_CC = $80;
  M_UNKNOWN_1 = $81;

  START_COMM = $00;
  STOP_COMM = $01;
  PATCH_ENV = $04;
  ENV_ATTACK = $01;
  ENV_DECAY = $02;
  ENV_SUSTAIN = $03;
  ENV_RELEASE = $04;
  PATCH_MORPH = $01;
  PATCH_VOLUME = $02;
  VOLUME_LEVEL = $00;
  VOLUME_MUTE = $01;
  PATCH_GLIDE = $03;
  GLIDE_TYPE = $00;
  GLIDE_SPEED = $01;
  PATCH_BEND = $04;
  BEND_ON_OFF = $00;
  BEND_RANGE = $01;
  PATCH_VIBRATO = $05;
  VIBRATO_MOD = $00;
  VIBRATO_DEPTH = $01;
  VIBRATO_RATE = $02;
  PATCH_ARPEGGIATOR = $06;
  ARP_ON_OFF = $00;
  ARP_SPEED = $01;
  ARP_DIRECTION = $02;
  ARP_OCTAVES = $03;
  PATCH_SUSTAIN = $07;
  SUSTAIN_PEDAL = $01;
  OCTAVE_SHIFT = $00;
  PATCH_MASTERCLOCK = $08;
  PATCH_VOICES = $09;
  STD_MORPH_NAMES: array [0 .. 7] of string = ('Wheel', 'Vel', 'Keyb',
    'Aft.Tch', 'Sust.Pd', 'Ctrl.Pd', 'P.Stick', 'G.Wh 2');
  PARAM_PAGE_NAMES: array [0 .. 4] of string = ('Osc', 'LFO', 'Env', 'Filter',
    'Effect');
  CATEGORIES: array [0 .. 15] of string = ('None', 'Acoustic', 'Sequencer',
    'Bass', 'Classic', 'Drum', 'Fantasy', 'FX', 'Lead', 'Organ', 'Pad', 'Piano',
    'Synth', 'Audio in', 'User1', 'User2');
  MODULECATEGORIES: array [0 .. 16] of string = ('In/Out', 'Note', 'Osc', 'LFO',
    'Rnd', 'Env', 'Filter', 'FX', 'Delay', 'Shaper', 'Level', 'Mixer', 'Switch',
    'Logic', 'Seq', 'MIDI', 'Test');
  ENV_TIMES: array [0 .. 127] of Single = (0.0005, 0.0006, 0.0007, 0.0009,
    0.0011, 0.0013, 0.0015, 0.0018, 0.0021, 0.0025, 0.0030, 0.0035, 0.0040,
    0.0047, 0.0055, 0.0063, 0.0073, 0.0084, 0.0097, 0.0111, 0.0127, 0.0145,
    0.0165, 0.0187, 0.0212, 0.0240, 0.0271, 0.0306, 0.0344, 0.0387, 0.0434,
    0.0486, 0.0543, 0.0606, 0.0676, 0.0752, 0.0836, 0.0928, 0.1030, 0.1140,
    0.1260, 0.1390, 0.1530, 0.1690, 0.1860, 0.2040, 0.2240, 0.2460, 0.2690,
    0.2950, 0.3220, 0.3520, 0.3840, 0.4190, 0.4560, 0.4960, 0.5400, 0.5860,
    0.6360, 0.6900, 0.7480, 0.8100, 0.8760, 0.9470, 1.0200, 1.1000, 1.1900,
    1.2800, 1.3800, 1.4900, 1.6000, 1.7200, 1.8500, 1.9900, 2.1300, 2.2800,
    2.4600, 2.6200, 2.8100, 3.0000, 3.2100, 3.4300, 3.6600, 3.9100, 4.1700,
    4.4500, 4.7400, 5.0500, 5.3700, 5.7200, 6.0800, 6.4700, 6.8700, 7.3000,
    7.7500, 8.2200, 8.7200, 9.2500, 9.8000, 10.400, 11.000, 11.600, 12.300,
    13.000, 13.800, 14.600, 15.400, 16.200, 17.100, 18.100, 19.100, 20.100,
    21.200, 22.400, 23.500, 24.800, 26.100, 27.500, 28.900, 30.400, 32.000,
    33.600, 35.300, 37.100, 38.900, 40.900, 42.900, 45.000);
  UNKNOWN_TIMES: array [0 .. 127] of string = ('0.2m', '0.3m', '0.4m', '0.5m',
    '0.6m', '0.8m', '0.9m', '1.0m', '1.2m', '1.4m', '1.6m', '1.7m', '2.0m',
    '2.2m', '2.4m', '2.6m', '2.9m', '3.1m', '3.4m', '3.7m', '4.0m', '4.3m',
    '4.6m', '4.9m', '5.3m', '5.6m', '6.0m', '6.3m', '6.7m', '7.1m', '7.5m',
    '7.9m', '8.4m', '8.8m', '9.3m', '9.7m', '10.2m', '10.7m', '11.2m', '11.7m',
    '12.2m', '12.7m', '13.3m', '13.8m', '14.4m', '14.9m', '15.5m', '16.1m',
    '16.7m', '17.4m', '18.0m', '18.6m', '19.3m', '19.9m', '20.6m', '21.3m',
    '22.0m', '22.7m', '23.4m', '24.1m', '24.9m', '25.6m', '26.4m', '27.2m',
    '28.0m', '28.8m', '29.6m', '30.4m', '31.2m', '32.1m', '32.9m', '33.8m',
    '34.6m', '35.5m', '36.4m', '37.3m', '38.3m', '39.2m', '40.1m', '41.1m',
    '42.0m', '43.0m', '44.0m', '45.0m', '46.0m', '47.0m', '48.1m', '49.1m',
    '50.2m', '51.2m', '52.3m', '53.4m', '54.5m', '55.6m', '56.7m', '57.9m',
    '59.0m', '60.2m', '61.3m', '62.5m', '63.7m', '64.9m', '66.1m', '67.3m',
    '68.6m', '69.8m', '71.1m', '72.3m', '73.6m', '74.9m', '76.2m', '77.5m',
    '78.8m', '80.2m', '81.5m', '82.9m', '84.2m', '85.6m', '87.0m', '88.4m',
    '89.8m', '91.2m', '92.7m', '94.1m', '95.6m', '97.0m', '98.5m', '100m');
  COMPR_ATTACK_TIMES: array [0 .. 127] of string = ('Fast', '0.53m', '0.56m',
    '0.59m', '0.63m', '0.67m', '0.71m', '0.75m', '0.79m', '0.84m', '0.89m',
    '0.94m', '1.00m', '1.06m', '1.12m', '1.19m', '1.26m', '1.33m', '1.41m',
    '1.50m', '1.59m', '1.68m', '1.78m', '1.89m', '2.00m', '2.12m', '2.24m',
    '2.38m', '2.52m', '2.67m', '2.83m', '3.00m', '3.17m', '3.36m', '3.56m',
    '3.78m', '4.00m', '4.24m', '4.49m', '4.76m', '5.04m', '5.34m', '5.66m',
    '5.99m', '6.35m', '6.73m', '7.13m', '7.55m', '8.00m', '8.48m', '8.98m',
    '9.51m', '10.1m', '10.7m', '11.3m', '12.0m', '12.7m', '13.5m', '14.3m',
    '15.1m', '16.0m', '17.0m', '18.0m', '19.0m', '20.2m', '21.4m', '22.6m',
    '24.0m', '25.4m', '26.9m', '28.5m', '30.2m', '32.0m', '33.9m', '35.9m',
    '38.1m', '40.3m', '42.7m', '45.3m', '47.9m', '50.8m', '53.8m', '57.0m',
    '60.4m', '64.0m', '67.8m', '71.8m', '76.1m', '80.6m', '85.4m', '90.5m',
    '95.9m', ' 102m', ' 108m', ' 114m', ' 121m', ' 128m', ' 136m', ' 144m',
    ' 152m', ' 161m', ' 171m', ' 181m', ' 192m', ' 203m', ' 215m', ' 228m',
    ' 242m', ' 256m', ' 271m', ' 287m', ' 304m', ' 323m', ' 342m', ' 362m',
    ' 384m', ' 406m', ' 431m', ' 456m', ' 483m', ' 512m', ' 542m', ' 575m',
    ' 609m', ' 645m', ' 683m', ' 724m', ' 767m');
  COMPR_RELEASE_TIMES: array [0 .. 127] of string = (' 125m', ' 129m', ' 134m',
    ' 139m', ' 144m', ' 149m', ' 154m', ' 159m', ' 165m', ' 171m', ' 177m',
    ' 183m', ' 189m', ' 196m', ' 203m', ' 210m', ' 218m', ' 225m', ' 233m',
    ' 241m', ' 250m', ' 259m', ' 268m', ' 277m', ' 287m', ' 297m', ' 308m',
    ' 319m', ' 330m', ' 342m', ' 354m', ' 366m', ' 379m', ' 392m', ' 406m',
    ' 420m', ' 435m', ' 451m', ' 467m', ' 483m', ' 500m', ' 518m', ' 536m',
    ' 555m', ' 574m', ' 595m', ' 616m', ' 637m', ' 660m', ' 683m', ' 707m',
    ' 732m', ' 758m', ' 785m', ' 812m', ' 841m', ' 871m', ' 901m', ' 933m',
    ' 966m', '1.00s', '1.04s', '1.07s', '1.11s', '1.15s', '1.19s', '1.23s',
    '1.27s', '1.32s', '1.37s', '1.41s', '1.46s', '1.52s', '1.57s', '1.62s',
    '1.68s', '1.74s', '1.80s', '1.87s', '1.93s', '2.00s', '2.07s', '2.14s',
    '2.22s', '2.30s', '2.38s', '2.46s', '2.55s', '2.64s', '2.73s', '2.83s',
    '2.93s', '3.03s', '3.14s', '3.25s', '3.36s', '3.48s', '3.61s', '3.73s',
    '3.86s', '4.00s', '4.14s', '4.29s', '4.44s', '4.59s', '4.76s', '4.92s',
    '5.10s', '5.28s', '5.46s', '5.66s', '5.86s', '6.06s', '6.28s', '6.50s',
    '6.73s', '6.96s', '7.21s', '7.46s', '7.73s', '8.00s', '8.28s', '8.57s',
    '8.88s', '9.19s', '9.51s', '9.85s', '10.2s');
  // Unknown: '12.0s','12.3s','12.7s','13.1s','13.7s','14.9s','16.4s','18.3s',
  // '19.3s','20.5s','21.5s','22.5s','23.5s'
  // Made lookup tables for the following parameters, because I couldn't figure out
  // the formula's
  FILTER_RESONANCE: array [0 .. 127] of Single = (0.50, 0.51, 0.51, 0.52, 0.53,
    0.54, 0.55, 0.55, 0.56, 0.57, 0.58, 0.59, 0.60, 0.61, 0.62, 0.63, 0.64,
    0.64, 0.66, 0.67, 0.68, 0.69, 0.70, 0.71, 0.73, 0.74, 0.75, 0.76, 0.78,
    0.79, 0.81, 0.82, 0.84, 0.84, 0.87, 0.88, 0.90, 0.92, 0.94, 0.95, 0.97,
    0.99, 1.01, 1.03, 1.06, 1.08, 1.10, 1.12, 1.15, 1.17, 1.20, 1.23, 1.25,
    1.28, 1.31, 1.34, 1.37, 1.41, 1.44, 1.48, 1.51, 1.55, 1.59, 1.63, 1.67,
    1.72, 1.76, 1.81, 1.86, 1.91, 1.97, 2.03, 2.08, 2.15, 2.21, 2.28, 2.35,
    2.42, 2.50, 2.58, 2.67, 2.76, 2.85, 2.95, 3.05, 3.16, 3.28, 3.40, 3.53,
    3.67, 3.81, 3.96, 4.13, 4.30, 4.49, 4.68, 4.89, 5.12, 5.36, 5.61, 5.89,
    6.19, 6.51, 6.85, 7.23, 7.64, 8.08, 8.56, 9.08, 9.66, 10, 11, 12, 13, 14,
    15, 16, 17, 19, 20, 22, 25, 27, 30, 34, 38, 44, 50);
  PHASER_FREQ: array [0 .. 127] of Single = (0.05, 0.05, 0.05, 0.05, 0.06, 0.06,
    0.07, 0.08, 0.09, 0.10, 0.12, 0.13, 0.15, 0.17, 0.19, 0.21, 0.23, 0.25,
    0.28, 0.30, 0.33, 0.36, 0.39, 0.42, 0.46, 0.49, 0.53, 0.57, 0.61, 0.65,
    0.69, 0.73, 0.78, 0.82, 0.87, 0.92, 0.97, 1.02, 1.08, 1.13, 1.19, 1.25,
    1.31, 1.37, 1.43, 1.49, 1.56, 1.63, 1.69, 1.76, 1.83, 1.91, 1.98, 2.05,
    2.13, 2.21, 2.29, 2.37, 2.45, 2.53, 2.62, 2.71, 2.80, 2.88, 2.98, 3.07,
    3.16, 3.26, 3.35, 3.45, 3.55, 3.65, 3.75, 3.86, 3.96, 4.07, 4.18, 4.29,
    4.40, 4.51, 4.62, 4.74, 4.86, 4.97, 5.09, 5.21, 5.34, 5.46, 5.58, 5.71,
    5.84, 5.97, 6.10, 6.23, 6.37, 6.50, 6.64, 6.77, 6.92, 7.06, 7.20, 7.34,
    7.49, 7.63, 7.78, 7.93, 8.08, 8.23, 8.39, 8.54, 8.70, 8.86, 9.02, 9.18,
    9.34, 9.50, 9.67, 9.84, 10.0, 10.2, 10.3, 10.5, 10.7, 10.9, 11.0, 11.2,
    11.4, 11.6);
  FLANGER_RATE: array [0 .. 127] of Single = (0.01, 0.02, 0.05, 0.07, 0.09,
    0.11, 0.14, 0.16, 0.18, 0.21, 0.23, 0.25, 0.27, 0.30, 0.32, 0.34, 0.37,
    0.39, 0.41, 0.43, 0.46, 0.48, 0.50, 0.53, 0.55, 0.57, 0.60, 0.62, 0.64,
    0.66, 0.69, 0.71, 0.73, 0.76, 0.78, 0.80, 0.82, 0.85, 0.87, 0.89, 0.92,
    0.94, 0.96, 0.98, 1.01, 1.03, 1.05, 1.08, 1.10, 1.12, 1.14, 1.17, 1.19,
    1.21, 1.24, 1.26, 1.28, 1.30, 1.33, 1.35, 1.37, 1.40, 1.42, 1.44, 1.46,
    1.49, 1.51, 1.53, 1.56, 1.58, 1.60, 1.63, 1.65, 1.67, 1.69, 1.72, 1.74,
    1.76, 1.79, 1.81, 1.83, 1.85, 1.88, 1.90, 1.92, 1.95, 1.97, 1.99, 2.01,
    2.04, 2.06, 2.08, 2.11, 2.13, 2.15, 2.17, 2.20, 2.22, 2.24, 2.27, 2.29,
    2.31, 2.33, 2.36, 2.38, 2.40, 2.43, 2.45, 2.47, 2.49, 2.52, 2.54, 2.56,
    2.59, 2.61, 2.63, 2.66, 2.68, 2.70, 2.72, 2.75, 2.77, 2.79, 2.82, 2.84,
    2.86, 2.88, 2.91);
  FREQ_SHIFT_SUB: array [0 .. 127] of Single = (0.000, 0.000, 0.000, 0.000,
    0.000, 0.001, 0.001, 0.001, 0.002, 0.003, 0.004, 0.006, 0.007, 0.009, 0.012,
    0.014, 0.018, 0.021, 0.025, 0.029, 0.034, 0.040, 0.046, 0.052, 0.059, 0.067,
    0.075, 0.084, 0.094, 0.10, 0.12, 0.13, 0.14, 0.15, 0.17, 0.18, 0.20, 0.22,
    0.24, 0.25, 0.27, 0.30, 0.32, 0.34, 0.37, 0.39, 0.42, 0.45, 0.47, 0.50,
    0.54, 0.57, 0.60, 0.64, 0.67, 0.71, 0.75, 0.79, 0.84, 0.88, 0.93, 0.97,
    1.02, 1.07, 1.12, 1.18, 1.23, 1.29, 1.35, 1.41, 1.47, 1.53, 1.60, 1.67,
    1.74, 1.81, 1.88, 1.96, 2.03, 2.11, 2.19, 2.28, 2.36, 2.45, 2.54, 2.63,
    2.73, 2.82, 2.92, 3.02, 3.12, 3.23, 3.34, 3.45, 3.56, 3.67, 3.79, 3.91,
    4.03, 4.16, 4.29, 4.42, 4.55, 4.68, 4.82, 4.96, 5.11, 5.25, 5.40, 5.55,
    5.71, 5.86, 6.02, 6.18, 6.35, 6.52, 6.69, 6.87, 7.04, 7.22, 7.41, 7.59,
    7.78, 7.98, 8.17, 8.37, 8.57, 8.78);
  FREQ_SHIFT_LO: array [0 .. 127] of Single = (0.000, 0.000, 0.000, 0.001,
    0.003, 0.006, 0.010, 0.016, 0.024, 0.035, 0.048, 0.063, 0.082, 0.10, 0.13,
    0.16, 0.20, 0.23, 0.28, 0.33, 0.38, 0.44, 0.51, 0.58, 0.66, 0.74, 0.84,
    0.94, 1.05, 1.16, 1.29, 1.42, 1.56, 1.71, 1.87, 2.04, 2.22, 2.41, 2.61,
    2.83, 3.05, 3.28, 3.53, 3.79, 4.06, 4.34, 4.64, 4.95, 5.27, 5.61, 5.96,
    6.32, 6.70, 7.09, 7.50, 7.93, 8.37, 8.82, 9.30, 9.79, 10.3, 10.8, 11.4,
    11.9, 12.5, 13.1, 13.7, 14.3, 15.0, 15.7, 16.3, 17.1, 17.8, 18.5, 19.3,
    20.1, 20.9, 21.8, 22.6, 23.5, 24.4, 25.3, 26.3, 27.2, 28.2, 29.3, 30.3,
    31.4, 32.5, 33.6, 34.7, 35.9, 37.1, 38.3, 39.6, 40.9, 42.2, 43.5, 44.8,
    46.2, 47.6, 49.1, 50.6, 52.1, 53.6, 55.2, 56.7, 58.4, 60.0, 61.7, 63.4,
    65.2, 66.9, 68.8, 70.6, 72.5, 74.4, 76.3, 78.3, 80.3, 82.3, 84.4, 86.5,
    88.7, 90.8, 93.1, 95.3, 97.6);
  FREQ_SHIFT_HI: array [0 .. 127] of Single = (0.000, 0.001, 0.006, 0.021,
    0.049, 0.096, 0.17, 0.26, 0.39, 0.56, 0.77, 1.02, 1.32, 1.68, 2.10, 2.58,
    3.14, 3.76, 4.46, 5.25, 6.12, 7.09, 8.15, 9.31, 10.6, 12.0, 13.5, 15.1,
    16.8, 18.7, 20.7, 22.8, 25.1, 27.5, 30.1, 32.8, 35.7, 38.8, 42.0, 45.4,
    49.0, 52.8, 56.7, 60.9, 65.2, 69.8, 74.5, 79.5, 84.7, 90.1, 95.7, 102, 108,
    114, 121, 127, 134, 142, 149, 157, 165, 174, 182, 191, 201, 210, 220, 230,
    241, 251, 263, 274, 286, 298, 310, 323, 336, 349, 363, 377, 392, 407, 422,
    438, 454, 470, 487, 504, 522, 540, 558, 577, 596, 616, 636, 656, 677, 699,
    720, 743, 765, 789, 812, 836, 861, 886, 912, 938, 964, 991, 1019, 1047,
    1075, 1105, 1134, 1164, 1195, 1226, 1258, 1290, 1323, 1356, 1390, 1424,
    1459, 1495, 1531, 1568);

  PULSE_DELAY_RANGE: array [0 .. 127] of Single = (10.4, 11.1, 11.9, 12.8, 13.7,
    14.7, 15.8, 16.9, 18.1, 19.4, 20.8, 22.3, 23.9, 25.6, 27.5, 29.4, 31.6,
    33.8, 36.3, 38.9, 41.7, 44.8, 48.0, 51.5, 55.2, 59.3, 63.6, 68.2, 73.2,
    78.5, 84.2, 90.4, 97.0, 104, 112, 120, 129, 138, 148, 159, 171, 183, 197,
    211, 227, 244, 262, 281, 302, 324, 348, 374, 402, 432, 464, 498, 535, 575,
    618, 664, 713, 767, 824, 886, 952, 1030, 1100, 1190, 1280, 1370, 1470, 1580,
    1700, 1830, 1970, 2120, 2280, 2450, 2630, 2830, 3050, 3280, 3530, 3790,
    4080, 4390, 4720, 5080, 5470, 5890, 6340, 6820, 7340, 7900, 8500, 9160,
    9860, 10700, 11500, 12400, 13300, 14300, 15400, 16600, 17900, 19300, 20700,
    22300, 24100, 25900, 27900, 30100, 32400, 34900, 37600, 40600, 43700, 47100,
    50800, 54800, 59000, 63600, 68600, 74000, 79800, 86000, 92800, 100000);
  ENV_FOLLOW_ATTACK: array [0 .. 127] of Single = (0.00, 0.53, 0.56, 0.60, 0.64,
    0.67, 0.72, 0.76, 0.81, 0.86, 0.91, 0.97, 1.03, 1.09, 1.16, 1.23, 1.30,
    1.38, 1.47, 1.56, 1.66, 1.76, 1.87, 1.98, 2.10, 2.23, 2.37, 2.52, 2.67,
    2.84, 3.01, 3.20, 3.39, 3.60, 3.83, 4.06, 4.31, 4.58, 4.86, 5.16, 5.48,
    5.82, 6.18, 6.56, 6.96, 7.39, 7.85, 8.33, 8.84, 9.39, 9.97, 10.6, 11.2,
    11.9, 12.7, 13.4, 14.3, 15.2, 16.1, 17.1, 18.1, 19.3, 20.4, 21.7, 23.0,
    24.5, 26.0, 27.6, 29.3, 31.1, 33.0, 35.0, 37.2, 39.5, 41.9, 44.5, 47.3,
    50.2, 53.3, 56.5, 60.0, 63.7, 67.7, 71.8, 76.3, 81.0, 86.0, 91.3, 96.9, 103,
    109, 116, 123, 131, 139, 147, 156, 166, 176, 187, 199, 211, 224, 238, 253,
    268, 285, 302, 321, 341, 362, 384, 408, 433, 459, 488, 518, 550, 584, 620,
    659, 699, 742, 787, 837, 888, 942, 1000);
  ENV_FOLLOW_RELEASE: array [0 .. 127] of Single = (10.0, 10.5, 10.9, 11.4,
    12.0, 12.5, 13.1, 13.7, 14.3, 15.0, 15.7, 16.4, 17.1, 17.9, 18.8, 19.6,
    20.5, 21.5, 22.4, 23.5, 24.6, 25.7, 26.9, 28.1, 29.4, 30.7, 32.1, 33.6,
    35.2, 36.8, 38.5, 40.2, 42.1, 44.0, 46.0, 48.2, 50.4, 52.7, 55.1, 57.6,
    60.3, 63.1, 66.0, 69.0, 72.2, 75.5, 78.9, 82.6, 86.3, 90.3, 94.5, 98.8, 103,
    108, 113, 118, 124, 129, 135, 142, 148, 155, 162, 169, 177, 185, 194, 203,
    212, 222, 232, 243, 254, 265, 278, 291, 304, 318, 332, 347, 363, 380, 398,
    416, 435, 455, 476, 498, 521, 545, 570, 596, 624, 652, 682, 713, 747, 780,
    816, 854, 894, 934, 977, 1020, 1070, 1120, 1170, 1220, 1280, 1340, 1400,
    1460, 1530, 1600, 1680, 1760, 1840, 1920, 2010, 2100, 2200, 2300, 2410,
    2520, 2630, 2760, 2870, 3000);
  NOISE_GATE_ATTACK: array [0 .. 127] of Single = (0.2, 0.3, 0.4, 0.5, 0.6, 0.8,
    0.9, 1.0, 1.2, 1.4, 1.6, 1.7, 2.0, 2.2, 2.4, 2.6, 2.9, 3.1, 3.4, 3.7, 4.0,
    4.3, 4.6, 4.9, 5.3, 5.6, 6.0, 6.3, 6.7, 7.1, 7.5, 7.9, 8.4, 8.8, 9.3, 9.7,
    10.2, 10.7, 11.2, 11.7, 12.2, 12.7, 13.3, 13.8, 14.4, 14.9, 15.5, 16.1,
    16.7, 17.4, 18.0, 18.6, 19.3, 19.9, 20.6, 21.3, 22.0, 22.7, 23.4, 24.1,
    24.9, 25.6, 26.4, 27.2, 28.0, 28.8, 29.6, 30.4, 31.2, 32.1, 32.9, 33.8,
    34.6, 35.5, 36.4, 37.3, 38.3, 39.2, 40.1, 41.1, 42.0, 43.0, 44.0, 45.0,
    46.0, 47.0, 48.1, 49.1, 50.2, 51.2, 52.3, 53.4, 54.5, 55.6, 56.7, 57.9,
    59.0, 60.2, 61.3, 62.5, 63.7, 64.9, 66.1, 67.3, 68.6, 69.8, 71.1, 72.3,
    73.6, 74.9, 76.2, 77.5, 78.8, 80.2, 81.5, 82.9, 84.2, 85.6, 87.0, 88.4,
    89.8, 91.2, 92.7, 94.1, 95.6, 97.0, 98.5, 100);
  NOISE_GATE_RELEASE: array [0 .. 127] of Single = (0.5, 0.59, 0.67, 0.76, 0.87,
    0.98, 1.11, 1.25, 1.40, 1.57, 1.75, 1.95, 2.17, 2.41, 2.66, 2.94, 3.25,
    3.57, 3.92, 4.30, 4.71, 5.15, 5.62, 6.12, 6.66, 7.24, 7.85, 8.51, 9.21,
    9.96, 10.7, 11.6, 12.5, 13.4, 14.4, 15.5, 16.6, 17.8, 19.1, 20.4, 21.8,
    23.3, 24.9, 26.5, 28.2, 30.0, 32.0, 34.0, 36.1, 38.3, 40.6, 43.0, 45.5,
    48.2, 51.0, 53.9, 56.9, 60.1, 63.4, 66.8, 70.4, 74.2, 78.1, 82.2, 86.4,
    90.9, 95.5, 100, 105, 110, 116, 121, 127, 133, 139, 146, 153, 160, 167, 174,
    182, 190, 198, 207, 216, 225, 234, 244, 254, 265, 275, 286, 298, 310, 322,
    335, 348, 361, 375, 389, 404, 419, 434, 450, 467, 484, 501, 519, 537, 556,
    578, 596, 616, 638, 659, 682, 705, 728, 752, 777, 802, 828, 855, 883, 911,
    940, 970, 1000);
  NOISEGATE_PITCHTRACK_THRESHHOLD: array [0 .. 127] of Single = (-100.0, -42.1,
    -36.1, -32.5, -30.0, -28.1, -26.5, -25.2, -24.0, -23.0, -22.1, -21.2, -20.5,
    -19.8, -19.2, -18.6, -18.0, -17.5, -17.0, -16.5, -16.1, -15.6, -15.2, -14.8,
    -14.5, -14.1, -13.8, -13.4, -13.1, -12.8, -12.5, -12.2, -12.0, -11.7, -11.4,
    -11.2, -11.0, -10.7, -10.5, -10.3, -10.0, -9.8, -9.6, -9.4, -9.2, -9.0,
    -8.8, -8.6, -8.5, -8.3, -8.1, -7.9, -7.8, -7.6, -7.4, -7.3, -7.1, -7.0,
    -6.8, -6.7, -6.5, -6.4, -6.2, -6.1, -6.0, -5.8, -5.7, -5.6, -5.4, -5.3,
    -5.2, -5.1, -4.9, -4.8, -4.7, -4.6, -4.5, -4.3, -4.2, -4.1, -4.0, -3.9,
    -3.8, -3.7, -3.6, -3.5, -3.4, -3.3, -3.2, -3.1, -3.0, -2.9, -2.8, -2.7,
    -2.6, -2.5, -2.4, -2.3, -2.3, -2.2, -2.1, -2.0, -1.9, -1.8, -1.7, -1.7,
    -1.6, -1.5, -1.4, -1.3, -1.2, -1.2, -1.1, -1.0, -0.9, -0.9, -0.8, -0.7,
    -0.6, -0.6, -0.5, -0.4, -0.3, -0.3, -0.2, -0.1, -0.1, -0.0);
  PATCH_SETTINGS_VOLUME: array [0 .. 127] of Single = (-78, -77, -76, -74, -73,
    -72, -71, -70, -68, -67, -66, -65, -64, -63, -62, -61, -60, -59, -58, -57,
    -56, -55, -54, -53, -52, -51, -50, -49, -48, -48, -47, -46, -45, -44, -43,
    -43, -42, -41, -40, -39, -39, -38, -37, -36, -36, -35, -34, -34, -33, -32,
    -32, -31, -30, -30, -29, -28, -28, -27, -27, -26, -26, -25, -24, -24, -23,
    -23, -22, -22, -21, -21, -20, -20, -19, -19, -18, -18, -17, -17, -16, -16,
    -15, -15, -15, -14, -14, -13, -13, -12, -12, -12, -11, -11, -11, -10, -9.8,
    -9.4, -9.1, -8.7, -8.4, -8.0, -7.7, -7.4, -7.0, -6.7, -6.4, -6.1, -5.7,
    -5.4, -5.1, -4.8, -4.5, -4.2, -3.9, -3.6, -3.4, -3.1, -2.8, -2.5, -2.3,
    -2.0, -1.7, -1.5, -1.2, -1.0, -0.7, -0.5, -0.2, -0.0);
  GLIDE_TIME: array [0 .. 127] of Single = (0.2, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,
    0.9, 1.0, 1.2, 1.4, 1.7, 2.0, 2.3, 2.7, 3.1, 3.6, 4.2, 4.8, 5.5, 6.3, 7.2,
    8.2, 9.3, 11, 12, 14, 15, 17, 19, 22, 24, 27, 30, 34, 38, 42, 46, 51, 57,
    63, 69, 77, 84, 93, 102, 112, 123, 135, 147, 161, 178, 192, 209, 228, 248,
    270, 293, 318, 345, 374, 405, 438, 473, 511, 552, 595, 642, 691, 744, 800,
    860, 924, 992, 1100, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 2000,
    2100, 2200, 2400, 2500, 2700, 2900, 3000, 3200, 3400, 3600, 3900, 4100,
    4400, 4600, 4900, 5200, 5500, 5800, 6100, 6500, 6900, 7300, 7700, 8000,
    8600, 9000, 9500, 10100, 10600, 11100, 11700, 12400, 13000, 13700, 14400,
    15200, 15800, 16800, 17500, 18300, 19200, 20200, 21200, 22400);
  PATCH_SETTINGS_GLIDE: array [0 .. 127] of Single = (19, 20, 21, 23, 24, 26,
    27, 29, 31, 33, 35, 37, 40, 42, 45, 48, 51, 55, 58, 62, 66, 71, 75, 80, 86,
    91, 97, 104, 110, 118, 125, 133, 142, 151, 160, 170, 181, 192, 203, 215,
    227, 240, 253, 267, 281, 296, 311, 326, 342, 358, 374, 390, 406, 423, 440,
    456, 473, 490, 506, 523, 540, 556, 573, 589, 605, 621, 637, 653, 669, 685,
    701, 717, 733, 749, 765, 781, 798, 815, 832, 849, 867, 885, 903, 922, 942,
    962, 983, 1000, 1030, 1050, 1070, 1100, 1130, 1150, 1180, 1210, 1240, 1280,
    1310, 1350, 1390, 1430, 1470, 1520, 1570, 1620, 1680, 1740, 1800, 1870,
    1950, 2030, 2120, 2220, 2330, 2450, 2580, 2720, 2890, 3070, 3280, 3520,
    3800, 4120, 4510, 4980, 5550, 6270);

  EXTENDED_MODULE_IDS = [2, 6, 10, 11, 14, 35, 39, 56, 65, 70, 77, 80, 95, 99,
    107, 110, 111, 122, 133, 135, 136, 137, 138, 151, 153, 155, 168, 191, 201,
    203, 207, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219];
  KeyNames: array [0 .. 11] of string = ('C', 'C#', 'D', 'D#', 'E', 'F', 'F#',
    'G', 'G#', 'A', 'A#', 'B');
  MaxPixelCount = 32768;

type
  TPatchFileType = (pftPatch, pftPerf, pftEnd);
  TMessageDataType = (mdtResponseMessage = 0, mdtSendMessage = 1);
  TParamType = (ptParam, ptMode, ptMasterClock, ptVoiceMode);
  TKnobType = (ktBig, ktMedium, ktSmall, ktExtraSmall, ktReset, ktResetMedium,
    ktSlider, ktSeqSlider, ktNone);
  TButtonTextType = (bttNormal, bttPush, bttCheck, bttCheckBox);
  TButtonStyleType = (btsSlim, btsNormal);
  TOrientationType = (otHorizontal, otVertical);
  TButtonIncDecType = (bidLeftRight, bidUpDown);
  TIconType = (itNone, itUp, itDown, itLeft, itRight, itCheck, itSine, itSaw,
    itPulse, itTri, itPulse25, itPulse10);
  TBandwidthType = (btStatic, btDynamic);
  TConnectorType = (ctAudio, ctLogic, ctControl);
  TConnectorKind = (ckInput, ckOutput);
  TLedType = (ltSequencer, ltGreen, ltMiniVU);
  TLocationType = (ltFX = 0, ltVA = 1, ltPatch = 2);
  TClientType = (ctEditor, ctVST);
  TMidiDeviceAssignmentType = (mdatNone, mdatCtrl, mdatSysEx);
  TModulePage = (mpInOut, mpNote, mpOsc, mpLFO, mpRnd, mpEnv, mpFilter, mpFX,
    mpDelay, mpShaper, mpLevel, mpMixer, mpSwitch, mpLogic, mpSeq,
    mpMidi, mpTest);
  TLineWidthType = (lwThick, lwThin);
  TSymbolType = (stTrig1, stTrig2, stBox, stAmplifier);
  TG2ParamSource = (g2psParam, g2psGlobal, g2psMorph, g2psPatch);
  TG2ControlType = (g2ctKnob, g2ctButton);
  TByteBuffer = packed array of Byte;
  TStaticByteBuffer = packed array [0 .. MAX_BULK_DATA_SIZE - 1] of Byte;
  PStaticByteBuffer = ^TStaticByteBuffer;
  tG2Event = (EvtUSBActiveChange, EvtUSBError, EvtBeforeSendMessage,
    EvtReceiveResponseMessage, EvtNextInitStep, EvtAfterG2Init,
    EvtAfterPerfInit, EvtAfterSlotInit, EvtPerfsSettingsUpdate, EvtPerfUpdate,
    EvtSynthSettingsUpdate, EvtBeforePatchUpdate, EvtPatchUpdate,
    EvtVariationChange, EvtCopyVariation, EvtMidiClockReceive,
    EvtClockRunChange, EvtClockBPMChange, EvtMidiCCRecieve,
    EvtAfterGetAssignedVoices, EvtPatchLoadChange, EvtSelectSlot,
    EvtSelectLocation, EvtSelectModule, EvtSelectParam, EvtLabelValueChange,
    EvtMorphChange, EvtDeleteModule, EvtAfterRetreivePatch, EvtAfterBankList,
    EvtAfterStore, EvtAfterClear, EvtAfterClearBank, EvtAfterBankDownload,
    EvtDeassignKnob, EvtAssignKnob, EvtDeassignGlobalKnob, EvtAssignGlobalKnob,
    EvtValueChange, EvtStateChange, EvtInvalidate);
  IG2Subject = interface;

  IG2Observer = interface
    ['{44FB1F31-448E-43FB-9BF9-5E9938B0EDBB}']
    procedure Update(aG2Event: tG2Event);
    procedure RemoveReference(aData: IG2Subject);
  end;

  IG2Subject = interface
    ['{4ACA1A45-7A5D-4AA7-A1F6-75F51E8F5F3A}']
    procedure RegisterObserver(aObserver: IG2Observer);
    procedure RemoveObserver(aObserver: IG2Observer);
    procedure NotifyObservers(aG2Event: tG2Event);
  end;

  // IG2ParamObserver = interface;
  IG2DataLed = interface(IG2Subject)
    ['{3CE71DAA-F061-40A4-80BF-348A5AEDC6E6}']
    procedure SetValue(const aValue: Byte);
    function GetValue: Byte;
    function GetLocation: TLocationType;
    function GetGroupID: Byte;
    function GetModuleIndex: Byte;
    function GetLedType: TLedType;
    function GetGroupCount: Integer;
    property Location: TLocationType read GetLocation;
    property ModuleIndex: Byte read GetModuleIndex;
    property GroupID: Byte read GetGroupID;
    property LedType: TLedType read GetLedType;
    property GroupCount: Integer read GetGroupCount;
  end;

  IG2DataParam = interface(IG2Subject)
    ['{4C28F845-210F-4CDD-82EC-8F1CCFEE9F9D}']
    procedure SetValue(const aValue: Byte);
    function GetValue: Byte;
    function GetHighValue: Byte;
    function GetLowValue: Byte;
    function GetValueText(aIndex: Integer): string;
    function TextFunction: string;
    function HasMorph: boolean;
    procedure SetSelectedMorphValue(const aValue: Byte);
    function GetSelectedMorphValue: Byte;
    procedure SetSelected(const aValue: boolean);
    function GetSelected: boolean;
  end;

  IG2LedObserver = interface(IG2Observer)
    ['{B044DDF3-B423-4517-B010-062B7F62280C}']
    function GetCodeRef: Integer;
    procedure SetValue(const aValue: Integer);
    procedure Redraw;
  end;

  IG2ParamObserver = interface(IG2Observer)
    ['{FA9A0B2E-960F-44DC-B47D-32876D9A50E9}']
    procedure SetValue(const aValue: Integer);
    procedure SetButtonText(const aValue: TStrings);
    procedure SetMorphValue(const aValue: Integer);
    procedure SetHasMorph(const aValue: boolean);
    procedure SetSelected(const aValue: boolean);
    function GetValueText(aIndex: Integer): string;
    procedure SetValueText(aIndex: Integer; const aValue: string);
    function GetCodeRef: Integer;
    procedure Redraw;
    // procedure ClearDataDependency( aData : IG2DataParam);
    property ValueText[aIndex: Integer]: string read GetValueText
      write SetValueText;
    property CodeRef: Integer read GetCodeRef;
  end;

  IG2MultiParamObserver = interface(IG2ParamObserver)
    ['{A6F880E1-8C16-49A2-9ED6-3A7296678B02}']
    procedure AddDataDependency(aData: IG2DataParam;
      aDeassignEvent: TNotifyEvent);
    procedure ClearDataDependencies;
  end;

  // IControlLinkList = interface
  // ['{7B61BD4F-FC96-4BC7-9BFE-9F10A99FA45F}']
  // procedure AssignControl( aControl : IG2Control);
  // procedure DeassignControl( aControl : IG2Control);
  // procedure OnDeassignControl( Sender : TObject);
  // function GetList : IList<IG2Control>;
  // end;
  TBits16 = Word;
  TBits14 = Word;
  TBits12 = Word;
  TBits10 = Word;
  TBits8 = Byte;
  TBits7 = Byte;
  TBits6 = Byte;
  TBits5 = Byte;
  TBits4 = Byte;
  TBits3 = Byte;
  TBits2 = Byte;
  TBits1 = Byte;

  // TEndToken = array of Char;
  TBitReader = class
    FReadBitPointer: Integer;
    FBitBuffer: Byte;
    constructor Create;
    function ReadBits(aStream: TStream; NoOffBits: Byte): Cardinal;
  end;

  TBitWriter = class
    FWriteBitPointer: Integer;
    FBitBuffer: Byte;
    constructor Create;
    procedure WriteBits(aStream: TStream; Value: Cardinal; NoOffBits: Byte);
    function GetWriteBufferBitsLeft: Integer;
  end;

  TPatchChunk = class
    FStream: TStream;
    FId: Byte;
    FSize: Word;
    FLogLines: TStrings;
    FBitReader: TBitReader;
    FBitWriter: TBitWriter;
    FWriteBuffer: TMemoryStream;
    FReadBuffer: TMemoryStream;
    FWriteCrc: Word;
    FReadCrc: Word;
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function PeekID: Integer;
    procedure ReadChunk;
    procedure ReadBuffer(aSize: Integer);
    function ReadName: string;
    function ReadBits(NoOffBits: Byte): Cardinal;
    function GetReadBufferBitsLeft: Integer;
    procedure WriteChunk(aId: Byte);
    procedure WriteBits(Value: Cardinal; NoOffBits: Byte);
    procedure WriteName(Value: string);
    procedure WriteCrc(aStream: TStream);
    procedure Flush;
    procedure DumpChunkData(aStream: TStream);
  end;

  TModuleDefStream = class;

  TModuleDefList = class(TStringList)
  private
    function GetModulePanelDef(const aName: string): TModuleDefStream;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadModulePanelDefs(const aFilename: string);
    property ModulePanelDef[const aName: string]: TModuleDefStream
      read GetModulePanelDef;
  end;

  TModuleDefStream = class(TMemoryStream)
  public
    constructor Create; overload;
    constructor Create(Filename: string); overload;
    destructor Destroy; override;
    function GetNextString(var source: string;
      EndTokens: array of Char): string;
    function GetNextByte(var source: string; EndTokens: array of Char): Byte;
    function GetNextInteger(var source: string;
      EndTokens: array of Char): Integer;
    procedure ReadSpaces;
    function ReadUntil(EndTokens: array of Char): string;
    function ReadConst(Value: string): boolean;
    function ReadOptions(sl: TStrings;
      ListTokens, EndTokens: array of Char): Integer;
    function PeekValue(Name: string;
      ValueEndTokens, EndTokens: array of Char): string;
    function UnQuote(Value: string): string;
  end;

function CrcClavia(Seed: Integer; aVal: Integer): Word;
function G2BPM(aValue: Integer): string;
function BoolToByte(Value: boolean): Byte;
function BoolToInt(Value: boolean): Integer;
function BoolToStr(Value: boolean): string;
function HexToByte(c: Char): Byte;
function IntToByte(i: Integer): Byte;
function ByteToInt(b: Byte): Integer;
function max(v1, v2: Integer): Integer;
function min(v1, v2: Integer): Integer;
function CompletePath(path: string): string;
function ConvertFwSlahsToBwSlash(Filename: string): string;
function ConvertBwSlahsToFwSlash(Filename: string): string;
function ConvertToObjectName(aValue: string): string;
function GetKeyName(aKeyNumber: Integer): string;
function G2FloatToStr(aValue: Single; aLen: Integer): string;
function G2FloatToStrFixed(aValue: Single; aLen: Integer): string;
function PatchNameFromFileName(aFilename: string): string;
{$IFNDEF MSWINDOWS}
function GetTickCount: Integer;
{$ENDIF}
procedure StringToByteBuffer(const aValue: string; aByteBuffer: TByteBuffer);
function ByteBufferToString(const aByteBuffer: TByteBuffer): string;
procedure StringToByteArray(const aValue: string; aByteArray: array of Byte;
  const aLength: Integer);
function ByteArrayToString(const aByteArray: array of Byte;
  const aLength: Integer): string;
function GetModulePageNo(aPageName: string): Integer;
function FreqDispValue(aMode: Integer; aFreqCourse: Integer;
  aFreqFine: Integer): string;
function IntToSignedByte(aValue: Integer): Byte;
function SignedByteToInt(aValue: Byte): Integer;
function CheckCCAllowed(aMidiCC: Integer): boolean;
{$IFNDEF G2_VST}
function InRange(Value: string; min, max: Integer): Integer;
{$ENDIF}

var
  ModuleColors: array [0 .. 24] of Integer = (
    $00C0C0C0,
    $00BABACC, // 1
    $00BACCBA, // 2
    $00CCBAB0, // 3
    $00AACBD0, // 4
    $00D4A074, // 5
    $007A77E5, // 6 R
    $00BDC17B, // 7
    $0080B982, // 8
    $0048D1E7, // 9
    $0062D193, // 10
    $007DC7DE, // 11
    $00C29A8F, // 12
    $00817DBA, // 13
    $008D8DCA, // 14
    $00A5D1DE, // 15
    $009CCF94, // 16
    $00C7D669, // 17
    $00C8D2A0, // 18
    $00D2D2BE, // 19
    $00C08C80, // 20
    $00C773D6, // 21
    $00BE82BE, // 22
    $00D2A0CD, // 23
    $00D2BED2
  ); // 24

  ModuleColorOrder: array [0 .. 24] of Integer = (
    0,
    6,
    13,
    14,
    1,
    9,
    11,
    15,
    4,
    10,
    8,
    16,
    2,
    17,
    7,
    18,
    19,
    5,
    20,
    12,
    3,
    21,
    22,
    23,
    24
  );

  CableColors: array [0 .. 6] of Integer = (
    $005A5AFF,
    $00FF6464,
    $0050E6E6,
    $0050C0FF,
    $0050D250,
    $00E600C8,
    $00FFFFFF
  );

  ModulePageColors: array [0 .. 15, 0 .. 1] of Integer =
    (($00646464, $00FFFFFF), ($00FD028C, $00FFFFFF), ($000000CA, $00FFFFFF),
    ($00CA0000, $00FFFFFF), ($00E69600, $00000000), ($00BBBB00, $00000000),
    ($00848200, $00000000), ($006064EA, $00000000), ($00FF5983, $00000000),
    ($007371CE, $00000000), ($0000A040, $00000000), ($004080FF, $00000000),
    ($00FFD966, $00000000), ($0001EAF5, $00000000), ($0041CFFE, $00000000),
    ($000080C0, $00000000));

  G_HighlightColor: Integer;
  G_LedColor: Integer;
  G_SlotStripColor: Integer;
  G_SlotStripInverseColor: Integer;
  G_SlotStripDisabledColor: Integer;
  G_CableThickness: Integer;

implementation

function CheckCCAllowed(aMidiCC: Integer): boolean;
begin
  if (aMidiCC > 0) and (aMidiCC <= 119) then
  begin
    Result := not(aMidiCC in [1, 2, 7, 11, 17, 18, 32, 64, 70, 96, 97])
  end
  else
    Result := False;
end;

function GetModulePageNo(aPageName: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while (i < 16) and (Uppercase(aPageName) <> Uppercase(MODULECATEGORIES[i])) do
    inc(i);
  if i < 16 then
    Result := i;
end;

procedure StringToByteBuffer(const aValue: string; aByteBuffer: TByteBuffer);
var
  i: Integer;
begin
  SetLength(aByteBuffer, aValue.Length);
  for i := 0 to aValue.Length - 1 do
    aByteBuffer[i] := Byte(aValue.Chars[i]);
end;

function ByteBufferToString(const aByteBuffer: TByteBuffer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(aByteBuffer) - 1 do
    Result := Result + Char(aByteBuffer[i]);
end;

procedure StringToByteArray(const aValue: string; aByteArray: array of Byte;
  const aLength: Integer);
var
  i: Integer;
begin
  for i := 0 to aLength - 1 do
    aByteArray[i] := Byte(aValue.Chars[i]);
end;

function ByteArrayToString(const aByteArray: array of Byte;
  const aLength: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aLength - 1 do
    Result := Result + Char(aByteArray[i]);
end;

function GetTickCount: Integer;
begin
  Result := trunc(24 * 60 * 60 * 1000 * frac(GetTime));
end;

function PatchNameFromFileName(aFilename: string): string;
var
  i: Integer;
begin
  Result := '';
  aFilename := ExtractFilename(aFilename);
  i := 0;
  while (i < aFilename.Length) and (i < 16) and (aFilename.Chars[i] <> '.') do
  begin
    Result := Result + aFilename.Chars[i];
    inc(i);
  end;
end;

function G2BPM(aValue: Integer): string;
begin
  if aValue <= 32 then
    Result := IntToStr(24 + 2 * aValue)
  else if aValue <= 96 then
    Result := IntToStr(88 + aValue - 32)
  else
    Result := IntToStr(152 + (aValue - 96) * 2);
end;

function CompletePath(path: string): string;
begin
  if path.Length > 0 then
  begin
    if path.Chars[path.Length - 1] = '\' then
      Result := path
    else
      Result := path + '\';
  end
  else
    Result := '';
end;

function G2FloatToStr(aValue: Single; aLen: Integer): string;
var
  intpart: Single;
  fl, t, p: Integer;
begin
  intpart := abs(aValue);
  fl := 1; // calc numbers before decimal point
  while intpart > 10 do
  begin
    inc(fl);
    intpart := intpart / 10;
  end;
  fl := aLen - (fl + 1);
  if fl > 0 then
  begin
    p := 1; // calc number of visible decimals
    while fl > 0 do
    begin
      p := p * 10;
      dec(fl);
    end;
    t := round(aValue * p);
    Result := FloatToStr(t / p);
  end
  else
    Result := IntToStr(round(aValue));
end;

function G2FloatToStrFixed(aValue: Single; aLen: Integer): string;
var
  temp: Single;
  fl, t, p: Integer;
begin
  temp := abs(aValue);
  fl := 1; // calc numbers before decimal point
  while temp >= 10 do
  begin
    inc(fl);
    temp := temp / 10;
  end;
  fl := aLen - (fl + 1);
  if fl > 0 then
  begin
    p := 1; // calc number of visible decimals
    while fl > 0 do
    begin
      p := p * 10;
      dec(fl);
    end;
    t := round(aValue * p);
    if p > 1 then
    begin
      Result := FloatToStr(t / p);
      if frac(t / p) = 0 then
        Result := Result + '.';
      if t < 0 then
        inc(aLen); // - sign
      while Length(Result) < aLen do
        Result := Result + '0'
    end
    else
      Result := FloatToStr(t / p);
  end
  else
    Result := IntToStr(round(aValue));
end;

function GetKeyName(aKeyNumber: Integer): string;
var
  Key, Octave: Integer;
begin
  Key := aKeyNumber mod 12;
  Octave := aKeyNumber div 12 - 1;
  Result := KeyNames[Key] + IntToStr(Octave);
end;

function ConvertToObjectName(aValue: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to aValue.Length - 1 do
  begin
    if aValue.Chars[i].IsInArray([' ', '/', '#']) then
      Result := Result + '_'
    else
      Result := Result + aValue.Chars[i];
  end;
end;

function ConvertFwSlahsToBwSlash(Filename: string): string;
var
  i: Integer;
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    for i := 0 to Filename.Length - 1 do
      if Filename.Chars[i] = '/' then
        sb.Append('\')
      else
        sb.Append(Filename.Chars[i]);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function ConvertBwSlahsToFwSlash(Filename: string): string;
var
  i: Integer;
  sb: TStringBuilder;
begin
  Result := '';
  sb := TStringBuilder.Create;
  try
    for i := 0 to Filename.Length - 1 do
      if Filename.Chars[i] = '\' then
        sb.Append('/')
      else
        sb.Append(Filename.Chars[i]);
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function CrcClavia(Seed: Integer; aVal: Integer): Word;
var
  i: Integer;
  aCrc: Integer;
  k: Integer;
begin
  k := (((Seed shr 8) xor aVal) and 255) shl 8;
  aCrc := 0;
  for i := 1 to 8 do
  begin
    if (aCrc xor k) and $8000 <> 0 then
      aCrc := (aCrc shl 1) xor $1021
    else
      aCrc := aCrc shl 1;
    k := k shl 1;
  end;
  Result := ((Seed shl 8) xor aCrc) and $FFFF;
end;

function HexToByte(c: Char): Byte;
begin
  if c >= 'a' then
    Result := 10 + ord(c) - ord('a')
  else
    Result := ord(c) - ord('0');
end;

function BoolToByte(Value: boolean): Byte;
begin
  If Value then
    Result := 1
  else
    Result := 0;
end;

function BoolToInt(Value: boolean): Integer;
begin
  if Value then
    Result := 1
  else
    Result := 0;
end;

function BoolToStr(Value: boolean): string;
begin
  if Value then
    Result := 'True'
  else
    Result := 'False';
end;

function IntToByte(i: Integer): Byte;
begin
  if i < 0 then
    Result := 256 + i
  else
    Result := i;
end;

function ByteToInt(b: Byte): Integer;
begin
  if b > 127 then
    Result := b - 256
  else
    Result := b;
end;

function InRange(Value: string; min, max: Integer): Integer;
var
  Code: Integer;
begin
  val(Value, Result, Code);
  if Code <> 0 then
    raise Exception.Create('Number required.');
  if (Result < min) or (Result > max) then
    raise Exception.Create('Value out of range (' + IntToStr(min) + '..' +
      IntToStr(max) + ').');
end;

function max(v1, v2: Integer): Integer;
begin
  if v1 > v2 then
    Result := v1
  else
    Result := v2;
end;

function min(v1, v2: Integer): Integer;
begin
  if v1 < v2 then
    Result := v1
  else
    Result := v2;
end;

function FreqDispValue(aMode: Integer; aFreqCourse: Integer;
  aFreqFine: Integer): string;
var
  iValue1, iValue2: Integer;
  Exponent, Freq, Fact: Single;
begin
  case aMode of
    0:
      begin // Semi
        iValue1 := aFreqCourse - 64;
        Result := '';
        if iValue1 < 0 then
          Result := Result + IntToStr(iValue1)
        else
          Result := Result + '+' + IntToStr(iValue1);
        Result := Result + '  ';
        iValue2 := (aFreqFine - 64) * 100 div 128;
        if iValue2 < 0 then
          Result := Result + IntToStr(iValue2)
        else
          Result := Result + '+' + IntToStr(iValue2);
      end;
    1:
      begin // Freq
        // http://www.phy.mtu.edu/~suits/NoteFreqCalcs.html
        Exponent := ((aFreqCourse - 69) + (aFreqFine - 64) / 128) / 12;
        Freq := 440.0 * power(2, Exponent);
        if Freq >= 1000 then
          Result := G2FloatToStrFixed(Freq / 1000, 5) + 'kHz'
        else
          Result := G2FloatToStrFixed(Freq, 5) + 'Hz';
      end;
    2:
      begin // Fac
        Exponent := ((aFreqCourse - 64) + (aFreqFine - 64) / 128) / 12;
        Fact := power(2, Exponent);
        Result := 'x' + G2FloatToStrFixed(Fact, 6)
      end;
    3:
      begin // Part
        if (aFreqCourse = 0) then
        begin
          Result := G2FloatToStrFixed(0, 5) + 'Hz';
        end
        else if aFreqCourse <= 32 then
        begin
          Exponent := -(((32 - aFreqCourse) * 4) + 77 - (aFreqFine - 64) /
            128) / 12;
          Freq := 440.0 * power(2, Exponent);
          Result := G2FloatToStrFixed(Freq, 5) + 'Hz';
        end
        else
        begin
          if (aFreqCourse > 32) and (aFreqCourse <= 64) then
          begin
            iValue1 := 64 - aFreqCourse + 1;
            Result := '1:' + IntToStr(iValue1);
          end
          else
          begin
            iValue1 := aFreqCourse - 64 + 1;
            Result := IntToStr(iValue1) + ':1';
          end;
          Result := Result + '  ';
          iValue2 := (aFreqFine - 64) * 100 div 128;
          if iValue2 < 0 then
            Result := Result + IntToStr(iValue2)
          else
            Result := Result + '+' + IntToStr(iValue2);
        end;
      end;
    4:
      begin // Semi PShift
        iValue1 := aFreqCourse - 64;
        Result := '';
        if iValue1 < 0 then
          Result := Result + G2FloatToStrFixed(iValue1 / 4, 4)
        else
          Result := Result + '+' + G2FloatToStrFixed(iValue1 / 4, 4);
        Result := Result + '  ';
        iValue2 := (aFreqFine - 64) * 100 div 128;
        if iValue2 < 0 then
          Result := Result + IntToStr(iValue2)
        else
          Result := Result + '+' + IntToStr(iValue2);
      end;
  end;
end;

function IntToSignedByte(aValue: Integer): Byte;
begin
  if aValue < 0 then
  begin
    if aValue >= -128 then
      Result := 256 + aValue
    else
      Result := 128;
  end
  else
  begin
    if aValue <= 127 then
      Result := aValue
    else
      Result := 127;
  end;
end;

function SignedByteToInt(aValue: Byte): Integer;
begin
  if aValue > 127 then
    Result := aValue - 256
  else
    Result := aValue;
end;

// ==== TModuleDefList =========================================================

constructor TModuleDefList.Create;
begin
  inherited Create;
end;

destructor TModuleDefList.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Objects[i].Free;
  inherited;
end;

procedure TModuleDefList.LoadModulePanelDefs(const aFilename: string);
var
  sl: TStringList;
  i, m, c: Integer;
  b: Byte;
  MemStream: TModuleDefStream;
  module_name, line: string;
begin
  sl := TStringList.Create;
  try
    m := 0;
    sl.LoadFromFile(aFilename);
    i := 0;
    while i < sl.Count do
    begin
      if sl[i].Contains('<#File:') then
      begin
        c := 7;
        module_name := '';
        while sl[i].Chars[c] <> '#' do
        begin
          module_name := module_name + sl[i].Chars[c];
          inc(c);
        end;
        m := Add(module_name);
        MemStream := TModuleDefStream.Create;
        Objects[m] := MemStream;
        inc(i);
        while (i < sl.Count) and (not sl[i].Contains('<#File:')) do
        begin
          line := sl[i];
          for c := 0 to line.Length - 1 do
          begin
            b := Byte(line.Chars[c]);
            MemStream.Write(b, 1);
          end;
          b := 13;
          MemStream.Write(b, 1);
          inc(i);
        end;
      end
      else
        raise Exception.Create('"<#File:" expected.');
    end;
  finally
    sl.Free;
  end;
end;

function TModuleDefList.GetModulePanelDef(const aName: string)
  : TModuleDefStream;
var
  i: Integer;
begin
  i := IndexOf(aName);
  if i >= 0 then
    Result := Objects[i] as TModuleDefStream
  else
    Result := nil;
end;

// ==== TModuleDefStream =======================================================

constructor TModuleDefStream.Create(Filename: string);
begin
  inherited Create;
  LoadFromFile(Filename);
end;

constructor TModuleDefStream.Create;
begin
  inherited Create;
end;

destructor TModuleDefStream.Destroy;
begin
  inherited;
end;

function TModuleDefStream.GetNextString(var source: string;
  EndTokens: array of Char): string;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  while (i < source.Length) and not(source.Chars[i].IsInArray(EndTokens)) do
    inc(i);
  Result := source.Substring(0, i);
  source := source.Substring(i);
end;

function TModuleDefStream.GetNextByte(var source: string;
  EndTokens: array of Char): Byte;
var
  Value: string;
begin
  Value := GetNextString(source, EndTokens);
  Result := StrToInt(Value);
end;

function TModuleDefStream.GetNextInteger(var source: string;
  EndTokens: array of Char): Integer;
var
  Value: string;
begin
  Value := GetNextString(source, EndTokens);
  Result := StrToInt(Value);
end;

procedure TModuleDefStream.ReadSpaces;
var
  b: Byte;
begin
  while (Position < Size) and (Read(b, 1) = 1) and
    (Char(b).IsInArray([' ', #9, #10, #13])) do
  begin
  end;
  if not(Char(b).IsInArray([' ', #9, #10, #13])) then
    Position := Position - 1;
end;

function TModuleDefStream.ReadUntil(EndTokens: array of Char): string;
var
  b: Byte;
begin
  Result := '';
  while (Position < Size) and (Read(b, 1) = 1) and
    not(Char(b).IsInArray(EndTokens)) do
  begin
    Result := Result + Char(b);
  end;
end;

function TModuleDefStream.ReadConst(Value: string): boolean;
var
  b: Byte;
  i: Integer;
begin
  Result := False;
  i := 1;
  while (Position < Size) and (i <= Length(Value)) and (Read(b, 1) = 1) and
    (Value[i] = Char(b)) do
  begin
    inc(i);
  end;
  Result := True;
end;

function TModuleDefStream.ReadOptions(sl: TStrings;
  ListTokens, EndTokens: array of Char): Integer;
var
  b: Byte;
  s: string;
begin
  while (Position < Size) and (Read(b, 1) = 1) and
    not(Char(b).IsInArray(EndTokens)) do
  begin
    if (Char(b).IsInArray(ListTokens)) then
    begin
      sl.Add(s);
      s := '';
    end
    else
      s := s + Char(b);
  end;
  if s <> '' then
    sl.Add(s);
  Result := sl.Count;
end;

function TModuleDefStream.PeekValue(Name: string;
  ValueEndTokens, EndTokens: array of Char): string;
var
  oldpos, start: Integer;
  c: Char;
  found: boolean;
begin
  Result := '';
  oldpos := Position;
  try
    found := False;
    start := Position;
    Read(c, 1);
    while (Position < Size) and not(found) and not(c.IsInArray(EndTokens)) do
    begin
      if c <> Name.Chars[Position - 1 - start] then
      begin
        start := Position;
      end
      else if Position - start = Length(Name) then
        found := True;
      if not(found) then
        Read(c, 1);
    end;
    if found then
    begin
      Read(c, 1);
      while (Position < Size) and not(c.IsInArray(ValueEndTokens)) do
      begin
        Result := Result + c;
        Read(c, 1);
      end;
    end;
  finally
    Position := oldpos;
  end;
end;

function TModuleDefStream.UnQuote(Value: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Value.Length - 1 do
    if Value.Chars[i] <> '"' then
      Result := Result + Value.Chars[i];
end;

// ==== TBitReader =============================================================

constructor TBitReader.Create;
begin
  FReadBitPointer := 8; // Forces a new read of a new Byte in the BitBuffer
  FBitBuffer := 0;
end;

function TBitReader.ReadBits(aStream: TStream; NoOffBits: Byte): Cardinal;
var
  Mask: Byte;
  BitBufferSize, BitsLeft: Integer;
begin
  // Clavia uses a kind of compression with the patch data
  BitBufferSize := SizeOf(FBitBuffer) * 8;
  BitsLeft := BitBufferSize - FReadBitPointer; // Bits not read in BitBuffer;
  Mask := $FF shr FReadBitPointer; // Readpointer : msb = 0, lsb = 7 !
  if (NoOffBits - BitsLeft) <= 0 then
  begin // The bits left in the buffer can be used
    Result := (FBitBuffer and Mask) // Clear the bits that are not needed
      shr (BitsLeft - NoOffBits);
    // Move the remaining bits to the right position
    FReadBitPointer := FReadBitPointer + NoOffBits; // New readpointer position
  end
  else
  begin // Read new data
    NoOffBits := NoOffBits - BitsLeft;
    Result := (FBitBuffer and Mask)
    // Clear the bits that are not needed from the old buffer
      shl NoOffBits; // Move the remaining bits to the right position
    while (NoOffBits > 0) do
    begin
      // Read a new Byte in the bitbuffer
      if aStream.Read(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
        raise Exception.Create('Read error.');
      if NoOffBits < BitBufferSize then
      begin
        FReadBitPointer := NoOffBits;
        Result := Result + FBitBuffer shr (BitBufferSize - FReadBitPointer);
        // Add the bits from the new bitbuffer}
        NoOffBits := 0;
      end
      else
      begin
        FReadBitPointer := 8;
        NoOffBits := NoOffBits - BitBufferSize;
        Result := Result + FBitBuffer shl (NoOffBits);
        // Add the bits from the new bitbuffer}
      end;
    end;
  end;
end;

// ==== TBitWriter =============================================================

constructor TBitWriter.Create;
begin
  FWriteBitPointer := 0;
  FBitBuffer := 0;
end;

procedure TBitWriter.WriteBits(aStream: TStream; Value: Cardinal;
  NoOffBits: Byte);
var
  Mask: Cardinal;
  BitBufferSize, BitsLeft: Integer;
begin
  BitBufferSize := SizeOf(FBitBuffer) * 8;
  BitsLeft := BitBufferSize - FWriteBitPointer;
  while NoOffBits > BitsLeft do
  begin
    FBitBuffer := FBitBuffer + Value shr (NoOffBits - BitsLeft);
    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');
    Mask := $FFFFFFFF shr (32 - (NoOffBits - BitsLeft));
    Value := Value and Mask;
    FBitBuffer := 0;
    FWriteBitPointer := 0;
    NoOffBits := NoOffBits - BitsLeft;
    BitsLeft := BitBufferSize - FWriteBitPointer;
  end;
  if NoOffBits = BitsLeft then
  begin
    FBitBuffer := FBitBuffer + Value;
    if aStream.Write(FBitBuffer, SizeOf(FBitBuffer)) <> SizeOf(FBitBuffer) then
      raise Exception.Create('Write error.');
    FBitBuffer := 0;
    FWriteBitPointer := 0;
  end
  else
  begin
    FBitBuffer := FBitBuffer + Value shl (BitsLeft - NoOffBits);
    FWriteBitPointer := FWriteBitPointer + NoOffBits;
  end;
end;

function TBitWriter.GetWriteBufferBitsLeft: Integer;
begin
  Result := SizeOf(FBitBuffer) * 8 - FWriteBitPointer;
end;

// ==== TPatchChunk ============================================================

constructor TPatchChunk.Create(aStream: TStream);
begin
  FStream := aStream;
  // aStream.Position := 0;
  FBitReader := TBitReader.Create;
  FBitWriter := TBitWriter.Create;
  FWriteBuffer := TMemoryStream.Create;
  FReadBuffer := TMemoryStream.Create;
  { if aStream.Size > 0 then begin
    ReadBuffer(aStream.Size);
    end; }
  FWriteCrc := 0;
  FReadCrc := 0;
end;

destructor TPatchChunk.Destroy;
begin
  FReadBuffer.Free;
  FWriteBuffer.Free;
  FBitWriter.Free;
  FBitReader.Free;
  inherited;
end;

procedure TPatchChunk.ReadBuffer(aSize: Integer);
var
  i: Integer;
  Crc: Word;
begin
  FReadBuffer.Size := aSize;
  if FStream.Read(FReadBuffer.Memory^, aSize) <> aSize then
    raise Exception.Create('Read error');
  for i := 0 to aSize - 1 do
    FReadCrc := CrcClavia(FReadCrc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);
  if assigned(FLogLines) then
  begin
    // Calculate another CRC for the buffer, to be able to compare chunkdata in logfile
    Crc := 0;
    for i := 0 to aSize - 1 do
      Crc := CrcClavia(Crc, PStaticByteBuffer(FReadBuffer.Memory)^[i]);
    FLogLines.Add('Caluclated Crc : ' + IntToHex(Crc, 4));
    FLogLines.Add('');
  end;
  FReadBuffer.Position := 0;
  FBitReader.FReadBitPointer := 8;
  // Forces a new read of a new Byte in the BitBuffer
end;

function TPatchChunk.PeekID: Integer;
var
  aId: Byte;
begin
  Result := -1;
  if FStream.Read(aId, SizeOf(aId)) = SizeOf(aId) then
  begin
    Result := aId;
    FStream.Position := FStream.Position - 1;
  end;
end;

procedure TPatchChunk.ReadChunk;
var
  bm, bl: Byte;
begin
  if FStream.Read(FId, SizeOf(FId)) <> SizeOf(FId) then
    raise Exception.Create('Error reading chunk.');
  if FStream.Read(bm, SizeOf(bm)) <> SizeOf(bm) then
    raise Exception.Create('Error reading chunk.');
  if FStream.Read(bl, SizeOf(bl)) <> SizeOf(bl) then
    raise Exception.Create('Error reading chunk.');
  FSize := bm * 256 + bl;
  if assigned(FLogLines) then
  begin
    FLogLines.Add('Chunk id: ' + IntToHex(FId, 2) + ', size: ' +
      IntToStr(FSize));
    DumpChunkData(FStream);
  end;
  // Calc crc
  FReadCrc := CrcClavia(FReadCrc, FId);
  FReadCrc := CrcClavia(FReadCrc, bm);
  FReadCrc := CrcClavia(FReadCrc, bl);
  ReadBuffer(FSize);
end;

function TPatchChunk.ReadBits(NoOffBits: Byte): Cardinal;
begin
  Result := FBitReader.ReadBits(FReadBuffer, NoOffBits);
end;

procedure TPatchChunk.WriteBits(Value: Cardinal; NoOffBits: Byte);
begin
  FBitWriter.WriteBits(FWriteBuffer, Value, NoOffBits);
end;

procedure TPatchChunk.WriteChunk(aId: Byte);
var
  BufSize: Word;
  bm, bl: Byte;
  i: Integer;
begin
  // Write the remaining Byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');

  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;
  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;
  bm := Hi(BufSize);
  bl := Lo(BufSize);
  // Calc crc
  FWriteCrc := CrcClavia(FWriteCrc, aId);
  FWriteCrc := CrcClavia(FWriteCrc, bm);
  FWriteCrc := CrcClavia(FWriteCrc, bl);
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia(FWriteCrc,
      PStaticByteBuffer(FWriteBuffer.Memory)^[i]);
  if (FStream.Write(aId, 1) <> 1) then
    raise Exception.Create('Write error');
  if (FStream.Write(bm, 1) <> 1) then
    raise Exception.Create('Write error');
  if (FStream.Write(bl, 1) <> 1) then
    raise Exception.Create('Write error');
  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');
  FWriteBuffer.Clear;
end;

procedure TPatchChunk.Flush;
var
  BufSize: Word;
  i: Integer;
begin
  // Write the remaining Byte if any
  if FBitWriter.FWriteBitPointer > 0 then
    if FWriteBuffer.Write(FBitWriter.FBitBuffer, 1) <> 1 then
      raise Exception.Create('Write error.');
  FBitWriter.FWriteBitPointer := 0;
  FBitWriter.FBitBuffer := 0;
  FWriteBuffer.Position := 0;
  BufSize := FWriteBuffer.Size;
  // Calc crc
  for i := 0 to BufSize - 1 do
    FWriteCrc := CrcClavia(FWriteCrc,
      PStaticByteBuffer(FWriteBuffer.Memory)^[i]);
  if (FStream.Write(FWriteBuffer.Memory^, BufSize) <> BufSize) then
    raise Exception.Create('Write error');
  FWriteBuffer.Clear;
end;

function TPatchChunk.GetReadBufferBitsLeft: Integer;
begin
  Result := SizeOf(FBitReader.FBitBuffer) * 8 - FBitReader.FReadBitPointer +
    (FReadBuffer.Size - FReadBuffer.Position) * 8;
end;

procedure TPatchChunk.WriteCrc(aStream: TStream);
var
  bm, bl: Byte;
begin
  bm := (FWriteCrc shr 8) and $FF;
  aStream.Write(bm, SizeOf(bm));
  bl := FWriteCrc and $FF;
  aStream.Write(bl, SizeOf(bl));
end;

function TPatchChunk.ReadName: string;
var
  b: Byte;
begin
  Result := '';
  b := ReadBits(8);
  while (b <> 0) do
  begin
    Result := Result + Char(b);
    if Length(Result) = 16 then
      break;
    b := ReadBits(8);
  end;
end;

procedure TPatchChunk.WriteName(Value: string);
var
  i: Integer;
  b: Byte;
begin
  i := 0;
  while (i < Value.Length) and (i < 16) do
  begin
    b := Byte(Value.Chars[i]);
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
    inc(i);
  end;
  if (i < 16) then
  begin
    b := 0;
    if FWriteBuffer.Write(b, 1) <> 1 then
      raise Exception.Create('Write error.');
  end;
end;

procedure TPatchChunk.DumpChunkData(aStream: TStream);
var
  p, i, c, Position: Integer;
  char_line, line: string;
  b: Byte;
begin
  Position := aStream.Position;
  try
    c := 0;
    i := 0;
    p := 0;
    line := '';
    char_line := '';;
    while (i < FSize) do
    begin
      if c < 16 then
      begin
        aStream.Read(b, 1);
        line := line + IntToHex(b, 2) + ' ';
        if b >= 32 then
          char_line := char_line + chr(b)
        else
          char_line := char_line + '.';
        inc(c);
        inc(i);
      end
      else
      begin
        FLogLines.Add(IntToHex(p, 6) + ' - ' + line + ' ' + char_line);
        p := i;
        c := 0;
        line := '';
        char_line := '';
      end;
    end;
    if c <> 0 then
      FLogLines.Add(IntToHex(p, 6) + ' - ' + line + stringofchar(' ',
        16 * 3 - Length(line) + 1) + char_line);
  finally
    aStream.Position := Position;
  end;
end;

initialization

G_HighlightColor := XCL_CONTROL_HIGHLIGHT;
G_SlotStripColor := XCL_CLAVIA_RED;
G_SlotStripInverseColor := XCL_CLAVIA_BLUE;
G_SlotStripDisabledColor := CL_BTN_FACE;
G_CableThickness := 2;

end.
