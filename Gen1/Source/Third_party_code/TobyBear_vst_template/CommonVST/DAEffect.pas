//******************************************************************************
//
//  DAEffect.pas
//  21 May 2003
//
//  Part of the VST 2.3 SDK for Delphi
//  by Frederic Vanmol
//     http://www.axiworld.be
//     frederic@axiworld.be
//
//------------------------------------------------------------------------------
//
// VST Plug-Ins SDK
// Version 2.3 extension
// © 2003, Steinberg Media Technologies, All Rights Reserved
//
//******************************************************************************
unit
    DAEffect;

interface

uses
    Windows;

//------------------------------------------------------------------------------
// misc def's
//------------------------------------------------------------------------------
type
    PPSingle = ^PSingle;

const
     kEffectMagic : AnsiString = 'VstP';

type
    PAEffect = ^AEffect;

    TAudioMasterCallbackFunc = function(effect: PAEffect; opcode, index,
		                        value: longint; ptr: pointer;
                                        opt: Single): longint; cdecl;
    TDispatcherFunc = function(effect: PAEffect; opcode, index, value: longint;
                               ptr: pointer; opt: Single): longint; cdecl;
    TProcessProc = procedure(effect: PAEffect; inputs, outputs: PPSingle;
                             sampleframes: longint); cdecl;
    TSetParameterProc = procedure(effect: PAEffect; index: longint;
                                  parameter: Single); cdecl;
    TGetParameterFunc = function(effect: PAEffect; index: longint): Single; cdecl;
    TProcessReplacingProc = procedure(effect: PAEffect; inputs, outputs: PPSingle;
                                      sampleframes: longint); cdecl;

    // prototype for plug-in main
    TMainProc = function(audioMaster: TAudioMasterCallbackFunc): PAEffect; cdecl;


//---------------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------------
    AEffect = record
      magic: longint;			// must be kEffectMagic ('VstP')
      dispatcher: TDispatcherFunc;
      process: TProcessProc;
      setParameter: TSetParameterProc;
      getParameter: TGetParameterFunc;
      numPrograms: longint;
      numParams: longint;	// all programs are assumed to have numParams parameters
      numInputs: longint;	//
      numOutputs: longint;	//
      flags: longint;		// see constants
      reservedForHost: pointer;	// reserved for Host, must be 0 (Dont use it)
      resvd2: longint;		// reserved for Host, must be 0 (Dont use it)
      initialDelay: longint;	// for algorithms which need input in the first place
      realQualities: longint;	// number of realtime qualities (0: realtime)
      offQualities: longint;	// number of offline qualities (0: realtime only)
      ioRatio: longint;		// input samplerate to output samplerate ratio, not used yet
      vObject: pointer;		// for class access (see AudioEffect.hpp), MUST be 0 else!
      user: pointer;		// user access
      uniqueID: longint;	// pls choose 4 character as unique as possible.
				// this is used to identify an effect for save+load
      version: longint;		// (example 1100 for version 1.1.0.0)
      processReplacing: TProcessReplacingProc;

      future: array[0..59] of AnsiChar;	// pls zero
    end;


//---------------------------------------------------------------------------------------------
// flags bits
//---------------------------------------------------------------------------------------------

const
     effFlagsHasEditor     = 1;	  // if set, is expected to react to editor messages
     effFlagsHasClip	   = 2;	  // return > 1. in getVu() if clipped
     effFlagsHasVu	   = 4;	  // return vu value in getVu(); > 1. means clipped
     effFlagsCanMono	   = 8;	  // if numInputs == 2, makes sense to be used for mono in
     effFlagsCanReplacing  = 16;  // supports in place output (processReplacing() exsists)
     effFlagsProgramChunks = 32;  // program data are handled in formatless chunks

//---------------------------------------------------------------------------------------------
// dispatcher opCodes
//---------------------------------------------------------------------------------------------

const
     effOpen            = 0; 	// initialise
     effClose           = 1; 	// exit, release all memory and other resources!

     effSetProgram	= 2;	// program no in <value>
     effGetProgram      = 3; 	// return current program no.
     effSetProgramName  = 4;	// user changed program name (max 24 char + 0) to as passed in string
     effGetProgramName  = 5;	// stuff program name (max 24 char + 0) into string

     effGetParamLabel   = 6;	// stuff parameter <index> label (max 8 char + 0) into string
			     	// (examples: sec, dB, type)
     effGetParamDisplay = 7;	// stuff parameter <index> textual representation into string
			     	// (examples: 0.5, -3, PLATE)
     effGetParamName    = 8;	// stuff parameter <index> label (max 8 char + 0) into string
			     	// (examples: Time, Gain, RoomType)
     effGetVu           = 9; 	// called if (flags & (effFlagsHasClip | effFlagsHasVu))

     // system

     effSetSampleRate   = 10;	// in opt (float value in Hz; for example 44100.0Hz)
     effSetBlockSize    = 11;	// in value (this is the maximun size of an audio block,
                                // pls check sampleframes in process call)
     effMainsChanged    = 12;	// the user has switched the 'power on' button to
			     	// value (0 off, else on). This only switches audio
			     	// processing; you should flush delay buffers etc.
     // editor

     effEditGetRect     = 13;   // stuff rect (top, left, bottom, right) into ptr
     effEditOpen        = 14; 	// system dependant Window pointer in ptr
     effEditClose       = 15; 	// no arguments
     effEditDraw        = 16; 	// draw method, ptr points to rect  (MAC only)
     effEditMouse       = 17; 	// index: x, value: y (MAC only)
     effEditKey         = 18; 	// system keycode in value
     effEditIdle        = 19; 	// no arguments. Be gentle!
     effEditTop         = 20; 	// window has topped, no arguments
     effEditSleep       = 21; 	// window goes to background

     // new

     effIdentify        = 22; 	// returns 'NvEf'
     effGetChunk        = 23; 	// host requests pointer to chunk into (void**)ptr, byteSize returned
     effSetChunk        = 24; 	// plug-in receives saved chunk, byteSize passed

     effNumOpcodes      = 25;


//---------------------------------------------------------------------------------------------
// audioMaster opCodes
//---------------------------------------------------------------------------------------------

const
     audioMasterAutomate     = 0;  // index, value, returns 0
     audioMasterVersion      = 1;  // vst version, currently 2 (0 for older)
     audioMasterCurrentId    = 2;  // returns the unique id of a plug that's currently
                                   // loading
     audioMasterIdle 	     = 3;  // call application idle routine (this will
                                   // call effEditIdle for all open editors too)
     audioMasterPinConnected = 4;  // inquire if an input or output is beeing connected;
                                   // index enumerates input or output counting from zero,
                                   // value is 0 for input and != 0 otherwise. note: the
                                   // return value is 0 for <true> such that older versions
                                   // will always return true.


implementation

end.
