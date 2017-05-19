//******************************************************************************
//
//  DAEffectX.pas
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
    DAEffectX;

interface

uses
    DAEffect;
//-------------------------------------------------------------------------------------------------------
// VstEvent
//-------------------------------------------------------------------------------------------------------

type
    PVstEvent = ^VstEvent;
    VstEvent = packed record    // a generic timestamped event
      vType       : longint;    // see enum below
      byteSize    : longint;    // of this event, excl. type and byteSize
      deltaFrames : longint;	// sample frames related to the current block start sample position
      flags       : longint;    // generic flags, none defined yet (0)
      data        : array[0..15] of byte;  // size may vary but is usually 16
    end;

const
     // VstEvent types
     kVstMidiType      = 1;  // midi event, can be cast as VstMidiEvent (see below)
     kVstAudioType     = 2;  // audio
     kVstVideoType     = 3;  // video
     kVstParameterType = 4;  // parameter
     kVstTriggerType   = 5;  // trigger
     kVstSysExType     = 6;  // midi system exclusive
     // ...etc


type

    PVstEvents = ^VstEvents;
    VstEvents = packed record    // a block of events for the current audio block
      numEvents : longint;
      reserved  : longint;                   // zero
      events    : array [0..2047] of PVstEvent;  // variable
    end;

    // defined events
    PVstMidiEvent = ^VstMidiEvent;
    VstMidiEvent = packed record // to be casted from a VstEvent
      vType           : longint;    // kVstMidiType
      byteSize        : longint;    // 24
      deltaFrames     : longint;    // sample frames related to the current block start sample position
      flags           : longint;    // none defined yet
      noteLength      : longint;    // (in sample frames) of entire note, if available, else 0
      noteOffset      : longint;    // offset into note from note start if available, else 0
      midiData        : array[0..3] of byte;  // 1 thru 3 midi bytes; midiData[3] is reserved (zero)
      detune          : byte;		      // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
      noteOffVelocity : byte;
      reserved1       : byte;		      // zero
      reserved2       : byte;      	      // zero
    end;


//-------------------------------------------------------------------------------------------------------
// VstTimeInfo
//-------------------------------------------------------------------------------------------------------

// VstTimeInfo as requested via audioMasterGetTime (getTimeInfo())
// refers to the current time slice. note the new slice is
// already started when processEvents() is called

type
    PVstTimeInfo = ^VstTimeInfo;
    VstTimeInfo = packed record
      samplePos: Double;         // current location
      sampleRate: Double;
      nanoSeconds: Double;       // system time
      ppqPos: Double;            // 1 ppq
      tempo: Double;             // in bpm
      barStartPos: Double;       // last bar start, in 1 ppq
      cycleStartPos: Double;     // 1 ppq
      cycleEndPos: Double;       // 1 ppq
      timeSigNumerator: longint; // time signature
      timeSigDenominator: longint;
      smpteOffset: longint;
      smpteFrameRate: longint;   // 0:24, 1:25, 2:29.97, 3:30, 4:29.97 df, 5:30 df
      samplesToNextClock: longint; // midi clock resolution (24 ppq), can be negative
      flags: longint;              // see below
    end;


const
     kVstTransportChanged     = 1;          // Indicates that Playing, Cycle or Recording has changed
     kVstTransportPlaying     = 1 shl 1;    // 1 << 1
     kVstTransportCycleActive = 1 shl 2;    // 1 << 2
     kVstTransportRecording   = 1 shl 3;    // 1 << 3

     kVstAutomationWriting    = 1 shl 6;    // 1 << 6
     kVstAutomationReading    = 1 shl 7;    // 1 << 7


     // flags which indicate which of the fields in this VstTimeInfo
     //  are valid; samplePos and sampleRate are always valid
     kVstNanosValid           = 1 shl 8;    // 1 << 8
     kVstPpqPosValid          = 1 shl 9;    // 1 << 9
     kVstTempoValid           = 1 shl 10;   // 1 << 10
     kVstBarsValid            = 1 shl 11;   // 1 << 11
     kVstCyclePosValid        = 1 shl 12;   // 1 << 12  // start and end
     kVstTimeSigValid         = 1 shl 13;   // 1 << 13
     kVstSmpteValid           = 1 shl 14;   // 1 << 14
     kVstClockValid           = 1 shl 15;   // 1 << 15

//-------------------------------------------------------------------------------------------------------
// Variable IO for Offline Processing
//-------------------------------------------------------------------------------------------------------

type
    PVstVariableIo = ^VstVariableIo;
    VstVariableIo = packed record
      inputs: PPSingle;
      outputs: PPSingle;
      numSamplesInput: longint;
      numSamplesOutput: longint;
      numSamplesInputProcessed: ^longint;
      numSamplesOutputProcessed: ^longint;
    end;


//---------------------------------------------------------------------------------------------
// new audioMaster opCodes
//---------------------------------------------------------------------------------------------

const
     //---from here VST 2.0 extension opcodes------------------------------------------------------
     // VstEvents + VstTimeInfo
     audioMasterWantMidi = audioMasterPinConnected + 2;       // <value> is a filter which is currently ignored
     audioMasterGetTime = audioMasterPinConnected + 3;        // returns const VstTimeInfo* (or 0 if not supported)
							      // <value> should contain a mask indicating which fields are required
							      // (see valid masks above), as some items may require extensive
							      // conversions
     audioMasterProcessEvents = audioMasterPinConnected + 4;  // VstEvents* in <ptr>
     audioMasterSetTime = audioMasterPinConnected + 5;	      // VstTimenfo* in <ptr>, filter in <value>, not supported
     audioMasterTempoAt = audioMasterPinConnected + 6;	      // returns tempo (in bpm * 10000) at sample frame location passed in <value>

     // parameters
     audioMasterGetNumAutomatableParameters = audioMasterPinConnected + 7;
     audioMasterGetParameterQuantization = audioMasterPinConnected + 8;	// returns the integer value for +1.0 representation,
									// or 1 if full single float precision is maintained
									// in automation. parameter index in <value> (-1: all, any)
     // connections, configuration
     audioMasterIOChanged = audioMasterPinConnected + 9;		// numInputs and/or numOutputs has changed
     audioMasterNeedIdle = audioMasterPinConnected + 10;		// plug needs idle calls (outside its editor window)
     audioMasterSizeWindow = audioMasterPinConnected + 11;		// index: width, value: height
     audioMasterGetSampleRate = audioMasterPinConnected + 12;
     audioMasterGetBlockSize = audioMasterPinConnected + 13;
     audioMasterGetInputLatency = audioMasterPinConnected + 14;
     audioMasterGetOutputLatency = audioMasterPinConnected + 15;
     audioMasterGetPreviousPlug = audioMasterPinConnected + 16;		// input pin in <value> (-1: first to come), returns cEffect*
     audioMasterGetNextPlug = audioMasterPinConnected + 17;		// output pin in <value> (-1: first to come), returns cEffect*

     // realtime info
     audioMasterWillReplaceOrAccumulate = audioMasterPinConnected + 18;	// returns: 0: not supported, 1: replace, 2: accumulate
     audioMasterGetCurrentProcessLevel = audioMasterPinConnected + 19;	// returns: 0: not supported,
									         // 1: currently in user thread (gui)
									         // 2: currently in audio thread (where process is called)
									         // 3: currently in 'sequencer' thread (midi, timer etc)
									         // 4: currently offline processing and thus in user thread
									         // other: not defined, but probably pre-empting user thread.
     audioMasterGetAutomationState = audioMasterPinConnected + 20;	         // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write

     // offline
     audioMasterOfflineStart = audioMasterPinConnected + 21;
     audioMasterOfflineRead = audioMasterPinConnected + 22;		 // ptr points to offline structure, see below. return 0: error, 1 ok
     audioMasterOfflineWrite = audioMasterPinConnected + 23;		 // same as read
     audioMasterOfflineGetCurrentPass = audioMasterPinConnected + 24;
     audioMasterOfflineGetCurrentMetaPass = audioMasterPinConnected + 25;

     // other
     audioMasterSetOutputSampleRate = audioMasterPinConnected + 26;	// for variable i/o, sample rate in <opt>
     audioMasterGetSpeakerArrangement = audioMasterPinConnected + 27;	// result in ret
     audioMasterGetOutputSpeakerArrangement = audioMasterGetSpeakerArrangement;
     audioMasterGetVendorString = audioMasterPinConnected + 28;	     	// fills <ptr> with a string identifying the vendor (max 64 char)
     audioMasterGetProductString = audioMasterPinConnected + 29;	// fills <ptr> with a string with product name (max 64 char)
     audioMasterGetVendorVersion = audioMasterPinConnected + 30;	// returns vendor-specific version
     audioMasterVendorSpecific = audioMasterPinConnected + 31;		// no definition, vendor specific handling
     audioMasterSetIcon = audioMasterPinConnected + 32;			// void* in <ptr>, format not defined yet
     audioMasterCanDo = audioMasterPinConnected + 33;			// string in ptr, see below
     audioMasterGetLanguage = audioMasterPinConnected + 34;		// see enum
     audioMasterOpenWindow = audioMasterPinConnected + 35;		// returns platform specific ptr
     audioMasterCloseWindow = audioMasterPinConnected + 36;		// close window, platform specific handle in <ptr>
     audioMasterGetDirectory = audioMasterPinConnected + 37;		// get plug directory, FSSpec on MAC, else char*
     audioMasterUpdateDisplay = audioMasterPinConnected + 38;		// something has changed, update 'multi-fx' display

     //---from here VST 2.1 extension opcodes------------------------------------------------------
     audioMasterVST21 = audioMasterPinConnected+39;
     audioMasterBeginEdit        = audioMasterVST21;                    // begin of automation session (when mouse down), parameter index in <index>
     audioMasterEndEdit          = audioMasterVST21+1;                  // end of automation session (when mouse up),     parameter index in <index>
     audioMasterOpenFileSelector = audioMasterVST21+2;	                // open a fileselector window with VstFileSelect* in <ptr>

     //---from here VST 2.2 extension opcodes------------------------------------------------------
     audioMasterVST22 = audioMasterVST21+3;
     audioMasterCloseFileSelector = audioMasterVST22;                   // close a fileselector operation with VstFileSelect* in <ptr>: Must be always called after an open !
     audioMasterEditFile          = audioMasterVST22+1;                 // open an editor for audio (defined by XML text in ptr)
     audioMasterGetChunkFile      = audioMasterVST22+2;                 // get the native path of currently loading bank or project
									// (called from writeChunk) void* in <ptr> (char[2048], or sizeof(FSSpec))

     //---from here VST 2.3 extension opcodes------------------------------------------------------
     audioMasterVST23 = audioMasterVST22+3;
     audioMasterGetInputSpeakerArrangement = audioMasterVST23;          // result a VstSpeakerArrangement in ret



//-------------------------------------------------
// Language
//-------------------------------------------------

type
    VstHostLanguage = longint;

const
     kVstLangEnglish  = 1;
     kVstLangGerman   = 2;
     kVstLangFrench   = 3;
     kVstLangItalian  = 4;
     kVstLangSpanish  = 5;
     kVstLangJapanese = 6;



//------------------------------------------------------------------------------
// dispatcher opCodes
//------------------------------------------------------------------------------

const
     //---from here VST 2.0 extension opcodes---------------------------------------------------------
     // VstEvents
     effProcessEvents = effSetChunk + 1;      // VstEvents* in <ptr>

     // parameters and programs
     effCanBeAutomated = effSetChunk + 2;     // parameter index in <index>
     effString2Parameter = effSetChunk + 3;   // parameter index in <index>, string in <ptr>
     effGetNumProgramCategories = effSetChunk + 4;   // no arguments. this is for dividing programs into groups (like GM)
     effGetProgramNameIndexed = effSetChunk + 5;     // get program name of category <value>, program <index> into <ptr>.
                                                     // category (that is, <value>) may be -1, in which case program indices
                                                     // are enumerated linearily (as usual); otherwise, each category starts
                                                     // over with index 0.
     effCopyProgram = effSetChunk + 6;	      // copy current program to destination <index>
					      // note: implies setParameter
     // connections, configuration
     effConnectInput = effSetChunk + 7;	      // input at <index> has been (dis-)connected;
					      // <value> == 0: disconnected, else connected
     effConnectOutput = effSetChunk + 8;      // same as input
     effGetInputProperties = effSetChunk + 9; // <index>, VstPinProperties* in ptr, return != 0 => true
     effGetOutputProperties = effSetChunk + 10;	   // dto
     effGetPlugCategory = effSetChunk + 11;   // no parameter, return value is category

     // realtime
     effGetCurrentPosition = effSetChunk + 12;	   // for external dsp, see flag bits below
     effGetDestinationBuffer = effSetChunk + 13;   // for external dsp, see flag bits below. returns float*

     // offline
     effOfflineNotify = effSetChunk + 14;     // ptr = VstAudioFile array, value = count, index = start flag
     effOfflinePrepare = effSetChunk + 15;    // ptr = VstOfflineTask array, value = count
     effOfflineRun = effSetChunk + 16;	      // dto

     // other
     effProcessVarIo = effSetChunk + 17;      // VstVariableIo* in <ptr>
     effSetSpeakerArrangement = effSetChunk + 18;   // VstSpeakerArrangement* pluginInput in <value>
						    // VstSpeakerArrangement* pluginOutput in <ptr>
     effSetBlockSizeAndSampleRate = effSetChunk + 19;  // block size in <value>, sampleRate in <opt>
     effSetBypass = effSetChunk + 20;	      // onOff in <value> (0 = off)
     effGetEffectName = effSetChunk + 21;     // char* name (max 32 bytes) in <ptr>
     effGetErrorText = effSetChunk + 22;      // char* text (max 256 bytes) in <ptr>
     effGetVendorString = effSetChunk + 23;   // fills <ptr> with a string identifying the vendor (max 64 char)
     effGetProductString = effSetChunk + 24;  // fills <ptr> with a string with product name (max 64 char)
     effGetVendorVersion = effSetChunk + 25;  // returns vendor-specific version
     effVendorSpecific = effSetChunk + 26;    // no definition, vendor specific handling
     effCanDo = effSetChunk + 27;	      // <ptr>
     effGetTailSize = effSetChunk + 28;	      // returns tail size; 0 is default (return 1 for 'no tail')
     effIdle = effSetChunk + 29;	      // idle call in response to audioMasterneedIdle. must
					      // return 1 to keep idle calls beeing issued

     // gui
     effGetIcon = effSetChunk + 30;	      // void* in <ptr>, not yet defined
     effSetViewPosition = effSetChunk + 31;   // set view position (in window) to x <index> y <value>

     // and...
     effGetParameterProperties = effSetChunk + 32;   // of param <index>, VstParameterProperties* in <ptr>
     effKeysRequired = effSetChunk + 33;	     // returns 0: needs keys (default for 1.0 plugs), 1: don't need
     effGetVstVersion = effSetChunk + 34;	     // returns 2; older versions return 0

     effNumV2Opcodes = effSetChunk + 35;
     // note that effNumOpcodes doesn't apply anymore


     //---from here VST 2.1 extension opcodes---------------------------------------------------------
     effEditKeyDown = effNumV2Opcodes;       // character in <index>, virtual in <value>, modifiers in <opt>,
	                                        // return -1 if not used, return 1 if used
     effEditKeyUp = effEditKeyDown+1;        // character in <index>, virtual in <value>, modifiers in <opt>
	                                        // return -1 if not used, return 1 if used
     effSetEditKnobMode = effEditKeyDown+2;  // mode in <value>: 0: circular, 1:circular relativ, 2:linear

     // midi plugins channeldependent programs
     effGetMidiProgramName = effEditKeyDown+3;   // passed <ptr> points to MidiProgramName struct.
						      // struct will be filled with information for 'thisProgramIndex'.
						      // returns number of used programIndexes.
						      // if 0 is returned, no MidiProgramNames supported.

     effGetCurrentMidiProgram = effEditKeyDown+4;  // returns the programIndex of the current program.
						      // passed <ptr> points to MidiProgramName struct.
						      // struct will be filled with information for the current program.

     effGetMidiProgramCategory = effEditKeyDown+5; // passed <ptr> points to MidiProgramCategory struct.
						      // struct will be filled with information for 'thisCategoryIndex'.
						      // returns number of used categoryIndexes.
						      // if 0 is returned, no MidiProgramCategories supported.

     effHasMidiProgramsChanged = effEditKeyDown+6; // returns 1 if the MidiProgramNames or MidiKeyNames
						      // had changed on this channel, 0 otherwise. <ptr> ignored.

     effGetMidiKeyName = effEditKeyDown+7;         // passed <ptr> points to MidiKeyName struct.
						      // struct will be filled with information for 'thisProgramIndex' and
						      // 'thisKeyNumber'. If keyName is "" the standard name of the key
						      // will be displayed. If 0 is returned, no MidiKeyNames are
						      // defined for 'thisProgramIndex'.

     effBeginSetProgram = effEditKeyDown+8;        // called before a new program is loaded
     effEndSetProgram = effEditKeyDown+9;          // called when the program is loaded

     effNumV2_1Opcodes = effEditKeyDown+10;

     //---from here VST 2.3 extension opcodes---------------------------------------------------------
     effGetSpeakerArrangement = effNumV2_1Opcodes;         // VstSpeakerArrangement** pluginInput in <value>
                                                           // VstSpeakerArrangement** pluginOutput in <ptr>
     effShellGetNextPlugin = effGetSpeakerArrangement+1;   // This opcode is only called, if plugin is of type kPlugCategShell.
                                                               // returns the next plugin's uniqueID.
                                                               // <ptr> points to a char buffer of size 64, which is to be filled
                                                               // with the name of the plugin including the terminating zero.
     effStartProcess = effGetSpeakerArrangement+2;         // Called before the start of process call
     effStopProcess = effGetSpeakerArrangement+3;          // Called after the stop of process call
     effSetTotalSampleToProcess = effGetSpeakerArrangement+4;  // Called in offline (non RealTime) Process before process is called, indicates how many sample will be processed
     effSetPanLaw = effGetSpeakerArrangement+5;            // PanLaw : Type (Linear, Equal Power,.. see enum PanLaw Type) in <value>,
                                                           // Gain in <opt>: for Linear : [1.0 => 0dB PanLaw], [~0.58 => -4.5dB], [0.5 => -6.02dB]
     effBeginLoadBank = effGetSpeakerArrangement+6;        // Called before a Bank is loaded, <ptr> points to VstPatchChunkInfo structure
                                                               // return -1 if the Bank can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
     effBeginLoadProgram = effGetSpeakerArrangement+7;     // Called before a Program is loaded, <ptr> points to VstPatchChunkInfo structure
                                                               // return -1 if the Program can not be loaded, return 1 if it can be loaded else 0 (for compatibility)
     effNumV2_3Opcodes = effGetSpeakerArrangement+8;



//-------------------------------------------------
// Parameter Properties
//-------------------------------------------------

type
    PVstParameterProperties = ^VstParameterProperties;
    VstParameterProperties = packed record
      stepFloat        : single;
      smallStepFloat   : single;
      largeStepFloat   : single;
      vLabel           : array[0..63] of AnsiChar;
      flags            : longint;    // see below
      minInteger       : longint;
      maxInteger       : longint;
      stepInteger      : longint;
      largeStepInteger : longint;
      shortLabel       : array[0..7] of AnsiChar;   // recommended: 6 + delimiter

      // the following are for remote controller display purposes.
      // note that the kVstParameterSupportsDisplayIndex flag must be set.
      // host can scan all parameters, and find out in what order
      // to display them:

      displayIndex     : smallint;     // for remote controllers, the index where this parameter
				       // should be displayed (starting with 0)

      // host can also possibly display the parameter group (category), such as
      // ---------------------------
      // Osc 1
      // Wave  Detune  Octave  Mod
      // ---------------------------
      // if the plug supports it (flag kVstParameterSupportsDisplayCategory)
      category         : smallint;     // 0: no category, else group index + 1
      numParametersInCategory : smallint;
      reserved         : smallint;
      categoryLabel    : array[0..23] of AnsiChar;    // for instance, "Osc 1"

      future           : array[0..15] of AnsiChar;
    end;

//---Parameter Properties Flags--------------------
const
     kVstParameterIsSwitch                =  1;   // 1 << 0
     kVstParameterUsesIntegerMinMax       =  2;   // 1 << 1
     kVstParameterUsesFloatStep           =  4;   // 1 << 2
     kVstParameterUsesIntStep             =  8;   // 1 << 3
     kVstParameterSupportsDisplayIndex 	  = 16;   // 1 << 4
     kVstParameterSupportsDisplayCategory = 32;   // 1 << 5
     kVstParameterCanRamp		  = 64;   // 1 << 6


//-------------------------------------------------
// Pin Properties
//-------------------------------------------------

type
    PVstPinProperties = ^VstPinProperties;
    VstPinProperties = packed record
      vLabel          : array[0..63] of AnsiChar;
      flags           : longint;              // see pin properties flags
      arrangementType : longint;
      shortLabel      : array[0..7] of AnsiChar;  // recommended: 6 + delimiter
      future          : array[0..47] of byte;
    end;

//---Pin Properties Flags--------------------------
const
     kVstPinIsActive   = 1; // 1 << 0
     kVstPinIsStereo   = 2; // 1 << 1
     kVstPinUseSpeaker = 4; // 1 << 2


//-------------------------------------------------
// Plugin Category
//-------------------------------------------------

type
    VstPlugCategory = longint;

const
     kPlugCategUnknown        = 0;
     kPlugCategEffect         = 1;
     kPlugCategSynth          = 2;
     kPlugCategAnalysis       = 3;
     kPlugCategMastering      = 4;
     kPlugCategSpacializer    = 5;     // 'panners'
     kPlugCategRoomFx         = 6;     // delays and reverbs
     kPlugSurroundFx          = 7;     // dedicated surround processor
     kPlugCategRestoration    = 8;
     kPlugCategOfflineProcess = 9;
     kPlugCategShell          = 10;    // plugin which is only a container of plugins.
     kPlugCategGenerator      = 11;



//-------------------------------------------------
// Midi Plugins Channel Dependent Programs
//-------------------------------------------------

type
    PMidiProgramName = ^MidiProgramName;
    MidiProgramName = packed record
      thisProgramIndex      : longint;  // >= 0. fill struct for this program index.
      name                  : array[0..63] of AnsiChar;
      midiProgram           : shortint;	// -1:off, 0-127
      midiBankMsb           : shortint;	// -1:off, 0-127
      midiBankLsb           : shortint;	// -1:off, 0-127
      reserved              : byte;     // zero
      parentCategoryIndex   : longint;  // -1:no parent category
      flags                 : longint;  // omni etc, see below
    end;

//---MidiProgramName Flags-------------------------
const
     kMidiIsOmni = 1;     // default is multi. for omni mode, channel 0
		          // is used for inquiries and program changes

//---MidiProgramName-------------------------------
type
    PMidiProgramCategory = ^MidiProgramCategory;
    MidiProgramCategory = packed record
      thisCategoryIndex   : longint;      // >= 0. fill struct for this category index.
      name                : array[0..63] of AnsiChar;
      parentCategoryIndex : longint;      // -1:no parent category
      flags               : longint;      // reserved, none defined yet, zero.
    end;

//---MidiKeyName-----------------------------------
type    
    PMidiKeyName = ^MidiKeyName;
    MidiKeyName = packed record
      thisProgramIndex : longint;    // >= 0. fill struct for this program index.
      thisKeyNumber    : longint;    // 0 - 127. fill struct for this key number.
      keyName          : array[0..63] of AnsiChar;
      reserved         : longint;    // zero
      flags            : longint;    // reserved, none defined yet, zero.
    end;

//-------------------------------------------------
// Flags Bits
//-------------------------------------------------
const
     effFlagsIsSynth       = 256;  // 1 << 8    // host may assign mixer channels for its outputs
     effFlagsNoSoundInStop = 512;  // 1 << 9    // does not produce sound when input is all silence
     effFlagsExtIsAsync    = 1024; // 1 << 10   // for external dsp; plug returns immedeately from process()
                                                // host polls plug position (current block) via effGetCurrentPosition
     effFlagsExtHasBuffer  = 2048; // 1 << 11   // external dsp, may have their own output buffe (32 bit float)
                                                // host then requests this via effGetDestinationBuffer

//---------------------------------------------------------------------------------------------
// surround setup
//---------------------------------------------------------------------------------------------

//---Speaker Properties----------------------------
type
    PVstSpeakerProperties = ^VstSpeakerProperties;
    VstSpeakerProperties = record
                                        // units:      range:            except:
      azimuth   : single;               // rad         -PI...PI		10.f for LFE channel
      elevation : single;               // rad         -PI/2...PI/2	10.f for LFE channel
      radius    : single;               // meter                          0.f for LFE channel
      reserved  : single;               // 0.
      name      : array[0..63] of AnsiChar; // for new setups, new names should be given (L/R/C... won't do)
      vType     : longint;              // speaker type
      future    : array[0..27] of byte;
    end;

// note: the origin for azimuth is right (as by math conventions dealing with radians);
// the elevation origin is also right, visualizing a rotation of a circle across the
// -pi/pi axis of the horizontal circle. thus, an elevation of -pi/2 corresponds
// to bottom, and a speaker standing on the left, and 'beaming' upwards would have
// an azimuth of -pi, and an elevation of pi/2.
// for user interface representation, grads are more likely to be used, and the
// origins will obviously 'shift' accordingly.

//---Speaker Arrangement---------------------------
type
    PVstSpeakerArrangement = ^VstSpeakerArrangement;
    VstSpeakerArrangement = record
      vType       : longint;                              // (was float lfeGain) LFE channel gain is adjusted [dB] higher than other channels)
      numChannels : longint;                              // number of channels in this speaker arrangement
      speakers    : array[0..7] of VstSpeakerProperties;  // variable
    end;


//---Speaker Types---------------------------------
type
    VstSpeakerType = longint;

const
     kSpeakerUndefined = $7fffffff;       // Undefined
     kSpeakerM         = 0;               // Mono (M)
     kSpeakerL         = 1;               // Left (L)
     kSpeakerR         = 2;               // Right (R)
     kSpeakerC         = 3;               // Center (C)
     kSpeakerLfe       = 4;               // Subbass (Lfe)
     kSpeakerLs        = 5;               // Left Surround (Ls)
     kSpeakerRs        = 6;               // Right Surround (Rs)
     kSpeakerLc        = 7;               // Left of Center (Lc)
     kSpeakerRc        = 8;               // Right of Center (Rc)
     kSpeakerS         = 9;               // Surround (S)
     kSpeakerCs        = kSpeakerS;       // Center of Surround (Cs) = Surround (S)
     kSpeakerSl        = 10;              // Side Left (Sl)
     kSpeakerSr        = 11;              // Side Right (Sr)
     kSpeakerTm        = 12;              // Top Middle (Tm)
     kSpeakerTfl       = 13;              // Top Front Left (Tfl)
     kSpeakerTfc       = 14;              // Top Front Center (Tfc)
     kSpeakerTfr       = 15;              // Top Front Right (Tfr)
     kSpeakerTrl       = 16;              // Top Rear Left (Trl)
     kSpeakerTrc       = 17;              // Top Rear Center (Trc)
     kSpeakerTrr       = 18;              // Top Rear Right (Trr)
     kSpeakerLfe2      = 19;              // Subbass 2 (Lfe2)


// user-defined speaker types (to be extended in the negative range)
// (will be handled as their corresponding speaker types with abs values:
// e.g abs(kSpeakerU1) == kSpeakerL, abs(kSpeakerU2) == kSpeakerR)
const
     kSpeakerU32 = -32;
     kSpeakerU31 = -31;
     kSpeakerU30 = -30;
     kSpeakerU29 = -29;
     kSpeakerU28 = -28;
     kSpeakerU27 = -27;
     kSpeakerU26 = -26;
     kSpeakerU25 = -25;
     kSpeakerU24 = -24;
     kSpeakerU23 = -23;
     kSpeakerU22 = -22;
     kSpeakerU21 = -21;
     kSpeakerU20 = -20;			// == kSpeakerLfe2
     kSpeakerU19 = -19;			// == kSpeakerTrr
     kSpeakerU18 = -18;			// == kSpeakerTrc
     kSpeakerU17 = -17;			// == kSpeakerTrl
     kSpeakerU16 = -16;			// == kSpeakerTfr
     kSpeakerU15 = -15;			// == kSpeakerTfc
     kSpeakerU14 = -14;			// == kSpeakerTfl
     kSpeakerU13 = -13;			// == kSpeakerTm
     kSpeakerU12 = -12;			// == kSpeakerSr
     kSpeakerU11 = -11;			// == kSpeakerSl
     kSpeakerU10 = -10;			// == kSpeakerCs
     kSpeakerU9	 = -9;			// == kSpeakerS
     kSpeakerU8	 = -8;			// == kSpeakerRc
     kSpeakerU7	 = -7;			// == kSpeakerLc
     kSpeakerU6	 = -6;			// == kSpeakerRs
     kSpeakerU5	 = -5;			// == kSpeakerLs
     kSpeakerU4	 = -4;			// == kSpeakerLfe
     kSpeakerU3	 = -3;			// == kSpeakerC
     kSpeakerU2	 = -2;			// == kSpeakerR
     kSpeakerU1	 = -1;			// == kSpeakerL


//---Speaker Arrangement Types---------------------
type
    VstSpeakerArrangementType = longint;

const
     kSpeakerArrUserDefined     = -2;
     kSpeakerArrEmpty           = -1;

     kSpeakerArrMono            =  0;   // M

     kSpeakerArrStereo          = 1;	// L R
     kSpeakerArrStereoSurround  = 2;	// Ls Rs
     kSpeakerArrStereoCenter	= 3;    // Lc Rc
     kSpeakerArrStereoSide      = 4;    // Sl Sr
     kSpeakerArrStereoCLfe      = 5;    // C Lfe

     kSpeakerArr30Cine          = 6;    // L R C
     kSpeakerArr30Music         = 7;    // L R S
     kSpeakerArr31Cine          = 8;    // L R C Lfe
     kSpeakerArr31Music         = 9;    // L R Lfe S

     kSpeakerArr40Cine          = 10;   // L R C   S (LCRS)
     kSpeakerArr40Music         = 11;   // L R Ls  Rs (Quadro)
     kSpeakerArr41Cine          = 12;   // L R C   Lfe S (LCRS+Lfe)
     kSpeakerArr41Music         = 13;   // L R Lfe Ls Rs (Quadro+Lfe)

     kSpeakerArr50              = 14;   // L R C Ls  Rs
     kSpeakerArr51              = 15;   // L R C Lfe Ls Rs

     kSpeakerArr60Cine          = 16;   // L R C   Ls  Rs Cs
     kSpeakerArr60Music         = 17;   // L R Ls  Rs  Sl Sr
     kSpeakerArr61Cine          = 18;   // L R C   Lfe Ls Rs Cs
     kSpeakerArr61Music         = 19;   // L R Lfe Ls  Rs Sl Sr

     kSpeakerArr70Cine          = 20;   // L R C Ls  Rs Lc Rc
     kSpeakerArr70Music         = 21;   // L R C Ls  Rs Sl Sr
     kSpeakerArr71Cine          = 22;   // L R C Lfe Ls Rs Lc Rc
     kSpeakerArr71Music         = 23;   // L R C Lfe Ls Rs Sl Sr

     kSpeakerArr80Cine          = 24;   // L R C Ls  Rs Lc Rc Cs
     kSpeakerArr80Music         = 25;   // L R C Ls  Rs Cs Sl Sr
     kSpeakerArr81Cine          = 26;   // L R C Lfe Ls Rs Lc Rc Cs
     kSpeakerArr81Music         = 27;   // L R C Lfe Ls Rs Cs Sl Sr

     kSpeakerArr102             = 28;   // L R C Lfe Ls Rs Tfl Tfc Tfr Trl Trr Lfe2

     kNumSpeakerArr             = 29;




//-------------------------------------------------
// Offline Processing
//-------------------------------------------------

type
    PVstOfflineTask = ^VstOfflineTask;
    VstOfflineTask = packed record
      processName: array[0..95] of AnsiChar;  // set by plug

      // audio access
      readPosition: Double;               // set by plug/host
      writePosition: Double;              // set by plug/host
      readCount: longint;                 // set by plug/host
      writeCount: longint;                // set by plug
      sizeInputBuffer: longint;           // set by host
      sizeOutputBuffer: longint;          // set by host
      inputBuffer: pointer;               // set by host
      outputBuffer: pointer;              // set by host
      positionToProcessFrom: Double;      // set by host
      numFramesToProcess: Double;         // set by host
      maxFramesToWrite: Double;           // set by plug

      // other data access
      extraBuffer: pointer;               // set by plug
      value: longint;                     // set by host or plug
      index: longint;                     // set by host or plug

      // file attributes
      numFramesInSourceFile: Double;      // set by host
      sourceSampleRate: Double;           // set by host or plug
      destinationSampleRate: Double;      // set by host or plug
      numSourceChannels: longint;         // set by host or plug
      numDestinationChannels: longint;    // set by host or plug
      sourceFormat: longint;              // set by host
      destinationFormat: longint;         // set by plug
      outputText: array[0..511] of AnsiChar;  // set by plug or host

      // progress notification
      progress: Double;                   // set by plug
      progressMode: longint;              // reserved for future
      progressText: array[0..99] of AnsiChar; // set by plug

      flags: longint;         // set by host and plug; see VstOfflineTaskFlags
      returnValue: longint;   // reserved for future
      hostOwned: pointer;     // set by host
      plugOwned: pointer;     // set by plug

      future: array[0..1023] of byte;
    end;

//---VstOfflineTask Flags--------------------------
type
    VstOfflineTaskFlags = longint;

const
     // set by host
     kVstOfflineUnvalidParameter = 1;     // 1 << 0
     kVstOfflineNewFile          = 2;     // 1 << 1

     // set by plug
     kVstOfflinePlugError        = 1024;  // 1 << 10
     kVstOfflineInterleavedAudio = 2048;  // 1 << 11
     kVstOfflineTempOutputFile   = 4096;  // 1 << 12
     kVstOfflineFloatOutputFile  = 8192;  // 1 << 13
     kVstOfflineRandomWrite      = 16384; // 1 << 14
     kVstOfflineStretch          = 32768; // 1 << 15
     kVstOfflineNoThread         = 65536; // 1 << 16



//---Option passed to offlineRead/offlineWrite-----
type
    VstOfflineOption = longint;

const
     kVstOfflineAudio      = 0;  // reading/writing audio samples
     kVstOfflinePeaks	   = 1;  // reading graphic representation
     kVstOfflineParameter  = 2;  // reading/writing parameters
     kVstOfflineMarker     = 3;  // reading/writing marker
     kVstOfflineCursor     = 4;  // reading/moving edit cursor
     kVstOfflineSelection  = 5;  // reading/changing selection
     kVstOfflineQueryFiles = 6;  // to request the host to call asynchronously offlineNotify

//---Structure passed to offlineNotify and offlineStart
type
    PVstAudioFile = ^VstAudioFile;
    VstAudioFile = packed record
      flags: longint;		  	// see enum VstAudioFileFlags
      hostOwned: pointer;	  	// any data private to host
      plugOwned: pointer;	  	// any data private to plugin
      name: array[0..99] of AnsiChar; 	// file title
      uniqueId: longint;	  	// uniquely identify a file during a session
      sampleRate: Double;	  	// file sample rate
      numChannels: longint; 	        // number of channels (1 for mono, 2 for stereo...)
      numFrames: Double;	  	// number of frames in the audio file
      format: longint;		  	// reserved for future
      editCursorPosition: Double;	// -1 if no such cursor
      selectionStart: Double;		// frame index of first selected frame, or -1
      selectionSize: Double;		// number of frames in selection, or 0
      selectedChannelsMask: longint;	// 1 bit per channel
      numMarkers: longint;   		// number of markers in the file
      timeRulerUnit: longint;		// see doc for possible values
      timeRulerOffset: Double;	        // offset in time ruler (positive or negative)
      tempo: Double;			// as bpm
      timeSigNumerator: longint;	// time signature numerator
      timeSigDenominator: longint;	// time signature denominator
      ticksPerBlackNote: longint;	// resolution
      smpteFrameRate: longint;		// smpte rate (set as in VstTimeInfo)

      future: array[0..63] of byte;
    end;


//---VstAudioFile Flags----------------------------
type
    VstAudioFileFlags = longint;

const
     // set by host (in call offlineNotify)
     kVstOfflineReadOnly	    = 1;      // 1 << 0
     kVstOfflineNoRateConversion    = 2;      // 1 << 1
     kVstOfflineNoChannelChange	    = 4;      // 1 << 2

     // Set by plug (in function offlineStart)
     kVstOfflineCanProcessSelection = 1024;   // 1 << 10
     kVstOfflineNoCrossfade	    = 2048;   // 1 << 11
     kVstOfflineWantRead	    = 4096;   // 1 << 12
     kVstOfflineWantWrite	    = 8192;   // 1 << 13
     kVstOfflineWantWriteMarker	    = 16384;  // 1 << 14
     kVstOfflineWantMoveCursor	    = 32768;  // 1 << 15
     kVstOfflineWantSelect	    = 65536;  // 1 << 16

//---VstAudioFileMarker----------------------------
type
    PVstAudioFileMarker = ^VstAudioFileMarker;
    VstAudioFileMarker = packed record
      position: Double;
      name: array[0..31] of AnsiChar;
      vType: longint;
      id: longint;
      reserved: longint;
    end;

//---------------------------------------------------------------------------------------------
// others
//---------------------------------------------------------------------------------------------

//---Structure used for openWindow and closeWindow
type
    PVstWindow = ^VstWindow;
    VstWindow = packed record
      title: array[0..127] of AnsiChar;    // title
      xPos: smallint;                  // position and size
      yPos: smallint;
      width: smallint;
      height: smallint;
      style: longint;                  // 0: with title, 1: without title

      parent: pointer;                 // parent of this window
      userHandle: pointer;             // reserved
      winHandle: pointer;              // reserved

      future: array[0..103] of byte;
    end;


//---Structure and enum used for keyUp/keyDown-----
type
    PVstKeyCode = ^VstKeyCode;
    VstKeyCode = packed record
      character : longint;
      virt      : byte;   // see enum VstVirtualKey
      modifier  : byte;   // see enum VstModifierKey
    end;

//---Used by member virt of VstKeyCode-------------
type
    VstVirtualKey = longint;

const
     VKEY_BACK         = 1;
     VKEY_TAB          = 2;
     VKEY_CLEAR        = 3;
     VKEY_RETURN       = 4;
     VKEY_PAUSE        = 5;
     VKEY_ESCAPE       = 6;
     VKEY_SPACE        = 7;
     VKEY_NEXT         = 8;
     VKEY_END          = 9;
     VKEY_HOME         = 10;

     VKEY_LEFT         = 11;
     VKEY_UP           = 12;
     VKEY_RIGHT        = 13;
     VKEY_DOWN         = 14;
     VKEY_PAGEUP       = 15;
     VKEY_PAGEDOWN     = 16;

     VKEY_SELECT       = 17;
     VKEY_PRINT        = 18;
     VKEY_ENTER        = 19;
     VKEY_SNAPSHOT     = 20;
     VKEY_INSERT       = 21;
     VKEY_DELETE       = 22;
     VKEY_HELP         = 23;
     VKEY_NUMPAD0      = 24;
     VKEY_NUMPAD1      = 25;
     VKEY_NUMPAD2      = 26;
     VKEY_NUMPAD3      = 27;
     VKEY_NUMPAD4      = 28;
     VKEY_NUMPAD5      = 29;
     VKEY_NUMPAD6      = 30;
     VKEY_NUMPAD7      = 31;
     VKEY_NUMPAD8      = 32;
     VKEY_NUMPAD9      = 33;
     VKEY_MULTIPLY     = 34;
     VKEY_ADD          = 35;
     VKEY_SEPARATOR    = 36;
     VKEY_SUBTRACT     = 37;
     VKEY_DECIMAL      = 38;
     VKEY_DIVIDE       = 39;
     VKEY_F1           = 40;
     VKEY_F2           = 41;
     VKEY_F3           = 42;
     VKEY_F4           = 43;
     VKEY_F5           = 44;
     VKEY_F6           = 45;
     VKEY_F7           = 46;
     VKEY_F8           = 47;
     VKEY_F9           = 48;
     VKEY_F10          = 49;
     VKEY_F11          = 50;
     VKEY_F12          = 51;
     VKEY_NUMLOCK      = 52;
     VKEY_SCROLL       = 53;

     VKEY_SHIFT        = 54;
     VKEY_CONTROL      = 55;
     VKEY_ALT          = 56;

     VKEY_EQUALS       = 57;


//---Used by member modifier of VstKeyCode---------
type
    VstModifierKey = longint;

const
     MODIFIER_SHIFT     = 1 shl 0; // Shift
     MODIFIER_ALTERNATE = 1 shl 1; // Alt
     MODIFIER_COMMAND   = 1 shl 2; // Control on Mac
     MODIFIER_CONTROL   = 1 shl 3; // Ctrl on PC, Apple on Mac



//---Used by audioMasterOpenFileSelector-----------
type
    PVstFileType = ^VstFileType;
    VstFileType = packed record
      name      : array[0..127] of AnsiChar;
      macType   : array[0..7] of AnsiChar;
      dosType   : array[0..7] of AnsiChar;
      unixType  : array[0..7] of AnsiChar;
      mimeType1 : array[0..127] of AnsiChar;
      mimeType2 : array[0..127] of AnsiChar;
    end;

    PVstFileSelect = ^VstFileSelect;
    VstFileSelect = packed record
      command              : longint;         // see enum kVstFileLoad....
      vType                : longint;         // see enum kVstFileType...

      macCreator           : longint;         // optional: 0 = no creator

      nbFileTypes          : longint;         // nb of fileTypes to used
      fileTypes            : PVstFileType;    // list of fileTypes

      title                : array[0..1023] of AnsiChar;  // text display in the file selector's title

      initialPath          : pAnsiChar;   // initial path

      returnPath           : pAnsiChar;   // use with kVstFileLoad and kVstDirectorySelect
				          // if null is passed, the host will allocated memory
					  // the plugin should then called closeOpenFileSelector for freeing memory
      sizeReturnPath       : longint;

      returnMultiplePaths  : ^pAnsichar;  // use with kVstMultipleFilesLoad
					  // the host allocates this array. The plugin should then called closeOpenFileSelector for freeing memory
      nbReturnPath         : longint; // number of selected paths

      reserved             : longint; // reserved for host application
      future               : array[0..115] of byte;   // future use
    end;

const
     kVstFileLoad          = 0;
     kVstFileSave          = 1;
     kVstMultipleFilesLoad = 2;
     kVstDirectorySelect   = 3;

     kVstFileType = 0;


//---Structure used for effBeginLoadBank/effBeginLoadProgram--
type
    PVstPatchChunkInfo = ^VstPatchChunkInfo;
    VstPatchChunkInfo = packed record
      version        : longint;               // Format Version (should be 1)
      pluginUniqueID : longint;               // UniqueID of the plugin
      pluginVersion  : longint;               // Plugin Version
      numElements    : longint;	              // Number of Programs (Bank) or Parameters (Program)
      future         : array[0..47] of AnsiChar;
    end;


//---PanLaw Type-----------------------------------
type
    VstPanLawType = longint;

const
     kLinearPanLaw     = 0;   // L = pan * M; R = (1 - pan) * M;
     kEqualPowerPanLaw = 1;   // L = pow (pan, 0.5) * M; R = pow ((1 - pan), 0.5) * M;


implementation

end.
