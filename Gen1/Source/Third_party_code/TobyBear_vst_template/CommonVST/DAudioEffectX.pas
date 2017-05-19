//******************************************************************************
//
//  DAudioEffectX.pas
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
    DAudioEffectX;

interface

uses
    Windows, DAEffect, DAEffectX, DAudioEffect;

{$DEFINE VST_2_1_EXTENSIONS} // version 2.1 extensions for Midi Program Names
{$DEFINE VST_2_2_EXTENSIONS} // version 2.2 extensions
{$DEFINE VST_2_3_EXTENSIONS} // version 2.3 extensions

//----------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------
// AudioEffectX extends AudioEffect with the new features. so you should derive
// your plug from AudioEffectX
//----------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------

type
    AudioEffectX = class(AudioEffect)
    public
      constructor Create(audioMaster: TAudioMasterCallbackFunc; numPrograms,
                         numParams: longint);
      destructor Destroy; override;

      function dispatcher(opcode, index, value: longint; ptr: pointer;
                          opt: Single): longint; override;

      // 'Plug -> Host' are methods which go from Plugin to Host, and are usually not overridden
      // 'Host -> Plug' are methods which you may override to implement the according functionality (to Host)


      //------------------------------------------------------------------------
      // events + time
      //------------------------------------------------------------------------

      // Plug -> Host
      procedure wantEvents(filter: longint);  // filter is currently ignored, midi channel data only (default)
//      virtual void wantEvents (long filter = 1);     // default is 1 for this!
      function getTimeInfo(filter: longint): PVstTimeInfo; virtual;  // returns const VstTimeInfo* (or 0 if not supported)
                                                                     // filter should contain a mask indicating which fields are requested
                                                                     // (see valid masks in aeffectx.h), as some items may require extensive
                                                                     // conversions
      procedure setTimeInfo(filter: longint; ti: PVstTimeInfo); virtual;
      function tempoAt(pos: longint): longint; virtual; // returns tempo (in bpm * 10000) at sample frame location <pos>
      function sendVstEventsToHost(events: PVstEvents): boolean;  // true: success

      // Host -> Plug
      function processEvents(events: PVstEvents): longint; virtual;  // wants no more...else return 1!
                       // VstEvents and VstMidiEvents are declared in aeffectx.h

      //------------------------------------------------------------------------
      // parameters and programs
      //------------------------------------------------------------------------

      // Plug -> Host
      function getNumAutomatableParameters: longint; virtual;
      function getParameterQuantization: longint; virtual; // returns the integer value for +1.0 representation,
                                                           // or 1 if full single float precision is maintained
                                                           // in automation. parameter index in <value> (-1: all, any)
      // Host -> Plug
      function canParameterBeAutomated(index: longint): boolean; virtual;
      function string2parameter(index: longint; text: pAnsiChar): boolean; virtual; // note: implies setParameter. text==0 is to be
      function getChannelParameter(channel, index: longint): Single; virtual;
      function getNumCategories: longint; virtual;
      function getProgramNameIndexed(category, index: longint; text: pAnsichar): boolean; virtual;
      function copyProgram(destination: longint): boolean; virtual; // expected to check the capability (returns true).

      //------------------------------------------------------------------------
      // connections, configuration
      //------------------------------------------------------------------------

      // Plug -> Host
      function ioChanged: boolean; virtual;   // tell host numInputs and/or numOutputs and/or numParameters has changed
      function needIdle: boolean; virtual;    // plug needs idle calls (outside its editor window)
      function sizeWindow(width, height: longint): boolean; virtual;
      function updateSampleRate: Double; virtual;  // gets and returns sample rate from host (may issue setSampleRate() )
      function updateBlockSize: longint; virtual;  // same for block size
      function getInputLatency: longint; virtual;
      function getOutputLatency: longint; virtual;
      function getPreviousPlug(input: longint): PAEffect; virtual;  // input can be -1 in which case the first found is returned
      function getNextPlug(output: longint): PAEffect; virtual;     // output can be -1 in which case the first found is returned

      // Host -> Plug
      procedure inputConnected(index: longint; state: boolean); virtual;  // input at <index> has been (dis-)connected,
      procedure outputConnected(index: longint; state: boolean); virtual;  // same as input; state == true: connected
      function getInputProperties(index: longint; properties: PVstPinProperties): boolean; virtual;
      function getOutputProperties(index: longint; properties: PVstPinProperties): boolean; virtual;
      function getPlugCategory: VstPlugCategory; virtual;

      //------------------------------------------------------------------------
      // realtime
      //------------------------------------------------------------------------

      // Plug -> Host
      function willProcessReplacing: longint; virtual; // returns 0: not implemented, 1: replacing, 2: accumulating
      function getCurrentProcessLevel: longint; virtual;  // returns: 0: not supported,
                                                                   // 1: currently in user thread (gui)
                                                                   // 2: currently in audio thread or irq (where process is called)
                                                                   // 3: currently in 'sequencer' thread or irq (midi, timer etc)
                                                                   // 4: currently offline processing and thus in user thread
                                                                   // other: not defined, but probably pre-empting user thread.
      function getAutomationState: longint; virtual;  // returns 0: not supported, 1: off, 2:read, 3:write, 4:read/write
      procedure wantAsyncOperation(state: boolean); virtual;  // notify host that we want to operate asynchronously.
                                                              // process() will return immedeately; host will poll getCurrentPosition
                                                              // to see if data are available in time.
      procedure hasExternalBuffer(state: boolean); virtual;   // external dsp, may have their own output buffe (32 bit float)
                                                              // host then requests this via effGetDestinationBuffer

      // Host -> Plug
      function reportCurrentPosition: longint; virtual;  // for external dsp, see wantAsyncOperation ()
      function reportDestinationBuffer: PSingle; virtual;  // for external dsp (dma option)

      //------------------------------------------------------------------------
      // offline
      //------------------------------------------------------------------------

      // Plug -> Host
      function offlineRead(offline: PVstOfflineTask; option: VstOfflineOption; readSource: boolean): boolean; virtual;
      function offlineWrite(offline: PVstOfflineTask; option: VstOfflineOption): boolean; virtual;
      function offlineStart(ptr: PVstAudioFile; numAudioFiles: longint; numNewAudioFiles: longint): boolean; virtual;
      function offlineGetCurrentPass: longint; virtual;
      function offlineGetCurrentMetaPass: longint; virtual;

      // Host -> Plug
      function offlineNotify(ptr: PVstAudioFile; numAudioFiles: longint; start: boolean): boolean; virtual;
      function offlinePrepare(offline: PVstOfflineTask; count: longint): boolean; virtual;
      function offlineRun(offline: PVstOfflineTask; count: longint): boolean; virtual;

      function offlineGetNumPasses: longint; virtual;
      function offlineGetNumMetaPasses: longint; virtual;

      //------------------------------------------------------------------------
      // other
      //------------------------------------------------------------------------

      // Plug -> Host
      procedure setOutputSampleRate(samplerate: Single); virtual;
      function getInputSpeakerArrangement: PVstSpeakerArrangement; virtual;
      function getOutputSpeakerArrangement: PVstSpeakerArrangement; virtual;
      function getHostVendorString(text: pAnsiChar): boolean; virtual;  // fills <text> with a string identifying the vendor (max 64 char)
      function getHostProductString(text: pAnsiChar): boolean; virtual; // fills <text> with a string with product name (max 64 char)
      function getHostVendorVersion: longint; virtual;  // returns vendor-specific version
      function hostVendorSpecific(lArg1, lArg2: longint; ptrArg: pointer;
                                  floatArg: Single): longint; virtual;  // no definition
      function canHostDo(text: pAnsiChar): longint; virtual;  // see 'hostCanDos' in audioeffectx.cpp
                                                          // returns 0: don't know (default), 1: yes, -1: no
      procedure isSynth(state: boolean); virtual;   // will call wantEvents if true
      procedure noTail(state: boolean); virtual;    // true: tells host we produce no output when silence comes in
                                                    // enables host to omit process() when no data are present
                                                    // on any one input.
      function getHostLanguage: longint; virtual;   // returns VstHostLanguage
      function openWindow(aWindow: PVstWindow): pointer; virtual;  // create new window
      function closeWindow(aWindow: PVstWindow): boolean; virtual; // close a newly created window
      function getDirectory: pointer; virtual;  // get the plug's directory, FSSpec on mac, else char*
      function updateDisplay: boolean; virtual; // something has changed, update 'multi-fx' display
														// returns true if supported

      // Host -> Plug
      function processVariableIo(varIo: PVstVariableIo): boolean; virtual;
      function setSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): boolean; virtual;
      function getSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): boolean; virtual;
      procedure setBlockSizeAndSampleRate(aBlockSize: longint; aSampleRate: Single); virtual;
      function setBypass(onOff: boolean): boolean; virtual; // for 'soft-bypass; process() still called
      function getEffectName(name: pAnsiChar): boolean; virtual;  // name max 32 char
      function getErrorText(text: pAnsiChar): boolean; virtual;  // max 256 char
      function getVendorString(text: pAnsiChar): boolean; virtual;  // fill text with a string identifying the vendor (max 64 char)
      function getProductString(text: pAnsiChar): boolean; virtual; // fills text with a string with product name (max 64 char)
      function getVendorVersion: longint; virtual;
      function vendorSpecific(lArg1, lArg2: longint; ptrArg: pointer;
                              floatArg: Single): longint; virtual; // return vendor-specific version
                                                                   // no definition, vendor specific handling
      function canDo(text: pAnsiChar): longint; virtual; // see 'plugCanDos' in audioeffectx.cpp. return values:
                                                          // 0: don't know (default), 1: yes, -1: no
      function getIcon: pointer; virtual;  // not yet defined
      function setViewPosition(x, y: longint): boolean; virtual;
      function getTailSize: longint; virtual;
      function fxIdle: longint; virtual;
      function getParameterProperties(index: longint; p: PVstParameterProperties): boolean; virtual;
      function keysRequired: boolean; virtual;  // version 1 plugs will return true

      function getVstVersion: longint; virtual;


      //----------------------------------------------------------------------------------------------------------------------------
      // midi program names, are always defined per channel, valid channels are 0 - 15
      //----------------------------------------------------------------------------------------------------------------------------

      {$IFDEF VST_2_1_EXTENSIONS}
      // Host -> Plug
      function getMidiProgramName(channel: longint; midiProgramName: PMidiProgramName): longint; virtual; {return 0;}
                                                               // Struct will be filled with information for 'thisProgramIndex'.
							       // Returns number of used programIndexes.
							       // If 0 is returned, no MidiProgramNames supported.
      function getCurrentMidiProgram(channel: longint; currentProgram: PMidiProgramName): longint; virtual; {return -1;}
							       // returns the programIndex of the current program. -1 means not supported.
							       // Struct will be filled with information for the current program.
      function getMidiProgramCategory(channel: longint; category: PMidiProgramCategory): longint; virtual; {return 0;}
							       // Struct will be filled with information for 'thisCategoryIndex'.
							       // Returns number of used categoryIndexes.
							       // If 0 is returned, no MidiProgramCategories supported/ used.
      function hasMidiProgramsChanged(channel: longint): boolean; virtual; {return false;}
							       // returns true if the MidiProgramNames, MidiKeyNames or
							       // MidiControllerNames had changed on this channel.
      function getMidiKeyName(channel: longint; keyName: PMidiKeyName): boolean; virtual; {return false;}
							       // Struct will be filled with information for 'thisProgramIndex' and 'thisKeyNumber'
							       // if keyName is "" the standard name of the key will be displayed.
							       // If false is returned, no MidiKeyNames defined for 'thisProgramIndex'.

      function beginSetProgram: boolean; virtual; { return false; } // called before a program is loaded
      function endSetProgram: boolean; virtual; { return false; }   // called after...

      // Plug -> Host
      function beginEdit(index: longint): boolean; virtual;  // to be called before a setParameterAutomated with mouse move (one per Mouse Down)
      function endEdit(index: longint): boolean; virtual;    // to be called after a setParameterAutomated (on Mouse Up)

      function openFileSelector(ptr: PVstFileSelect): boolean; virtual;
      {$ENDIF}    //VST_2_1_EXTENSIONS

      {$IFDEF VST_2_2_EXTENSIONS}
      function closeFileSelector(ptr: PVstFileSelect): boolean;
      function getChunkFile(nativePath: pointer): boolean;
      {$ENDIF}  //VST_2_2_EXTENSIONS

      {$IFDEF VST_2_3_EXTENSIONS}
      // Host -> Plug
      function setTotalSampleToProcess(value: longint): longint; virtual;   // Called in offline (non RealTime) Process before process is called, indicates how many sample will be processed
      function getNextShellPlugin(name: pAnsiChar): longint; virtual;           // This opcode is only called, if Plugin is of type kPlugCategShell.
                                                                            // should return the next plugin's uniqueID.
                                                                            // name points to a char buffer of size 64, which is to be filled
                                                                            // with the name of the plugin including the terminating zero.
      function startProcess: longint; virtual;                              // Called one time before the start of process call
      function stopProcess: longint; virtual;                               // Called after the stop of process call
      function setPanLaw(vType: longint; val: single): boolean; virtual;    // Set the Panning Law used by the Host
      function beginLoadBank(ptr: PVstPatchChunkInfo): longint; virtual;    // Called before a Bank is loaded.
                                                                            // returns -1 if the Bank cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)
      function beginLoadProgram(ptr: PVstPatchChunkInfo): longint; virtual; // Called before a Program is loaded. (called before beginSetProgram)
                                                                            // returns -1 if the Program cannot be loaded, returns 1 if it can be loaded else 0 (for compatibility)

      // Tools
      function allocateArrangement(var arrangement: PVstSpeakerArrangement; nbChannels: longint): boolean; virtual;   // Allocate memory for a VstSpeakerArrangement containing the given number of channels
      function deallocateArrangement(var arrangement: PVstSpeakerArrangement): boolean; virtual;                      // Delete/free memory for a speaker arrangement
      function copySpeaker(copyTo, copyFrom: PVstSpeakerProperties): boolean; virtual;    // Feed the "to" speaker properties with the same values than "from"'s ones.
                                                                                          // It is assumed here that "to" exists yet, ie this function won't
                                                                                          // allocate memory for the speaker (this will prevent from having
                                                                                          // a difference between an Arrangement's number of channels and
                                                                                          // its actual speakers...)
      function matchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): boolean; virtual;    // "to" is deleted, then created and initialized with the same values as "from" ones ("from" must exist).
      {$ENDIF} // VST_2_3_EXTENSIONS
    end;


implementation

uses
    SysUtils;

//------------------------------------------------------------------------------
//
//  'canDo' strings. note other 'canDos' can be evaluated by calling the
//  according function, for instance if getSampleRate returns 0, you
//  will certainly want to assume that this selector is not supported.
//
//------------------------------------------------------------------------------

const
     NumHostCanDos = 14 {$IFDEF VST_2_2_EXTENSIONS}+ 2{$ENDIF} {$IFDEF VST_2_3_EXTENSIONS}+ 2{$ENDIF};
     NumPlugCanDos = 23 {$IFDEF VST_2_1_EXTENSIONS}+ 2{$ENDIF} {$IFDEF VST_2_3_EXTENSIONS}+ 1{$ENDIF};

type
    THostCanDosArray = array[0..NumHostCanDos-1] of pAnsiChar;
    TPlugCanDosArray = array[0..NumPlugCanDos-1] of pAnsiChar;

const
     hostCanDos: THostCanDosArray = (
        'sendVstEvents',
        'sendVstMidiEvent',
        'sendVstTimeInfo',
        'receiveVstEvents',
        'receiveVstMidiEvent',
        'receiveVstTimeInfo',

        'reportConnectionChanges',
        'acceptIOChanges',
        'sizeWindow',

        'asyncProcessing',
        'offline',
        'supplyIdle',
        'supportShell',     // 'shell' handling via uniqueID as suggested by Waves
	'openFileSelector'

        {$IFDEF VST_2_2_EXTENSIONS}
	,
        'editFile',
	'closeFileSelector'
        {$ENDIF}       // VST_2_2_EXTENSIONS

        {$IFDEF VST_2_3_EXTENSIONS}
        ,
        'shellCategory',
	'startStopProcess'
        {$ENDIF} // VST_2_3_EXTENSIONS
     );

const
     plugCanDos: TPlugCanDosArray = (
        'sendVstEvents',
        'sendVstMidiEvent',
        'sendVstTimeInfo',
        'receiveVstEvents',
        'receiveVstMidiEvent',
        'receiveVstTimeInfo',
        'offline',
        'plugAsChannelInsert',
        'plugAsSend',
        'mixDryWet',
        'noRealTime',
        'multipass',
        'metapass',
        '1in1out',
        '1in2out',
        '2in1out',
        '2in2out',
        '2in4out',
        '4in2out',
        '4in4out',
        '4in8out',      // 4:2 matrix to surround bus
        '8in4out',      // surround bus to 4:2 matrix
        '8in8out'

        {$IFDEF VST_2_1_EXTENSIONS}
	,
	'midiProgramNames',
	'conformsToWindowRules'	         // mac: doesn't mess with grafport. general: may want
					      // to call sizeWindow (). if you want to use sizeWindow (),
                                              // you must return true (1) in canDo ("conformsToWindowRules")
        {$ENDIF} //VST_2_1_EXTENSIONS

        {$IFDEF VST_2_3_EXTENSIONS}
	,
	'bypass'
        {$ENDIF} // VST_2_3_EXTENSIONS

     );

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// AudioEffectX extends AudioEffect with the new features. so you should derive
// your plug from AudioEffectX
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// VstEvents + VstTimeInfo
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
constructor AudioEffectX.Create(audioMaster: TAudioMasterCallbackFunc; numPrograms, numParams: longint);
begin
  inherited Create(audioMaster, numPrograms, numParams);
end;

//------------------------------------------------------------------------------
destructor AudioEffectX.Destroy;
begin
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function AudioEffectX.dispatcher(opcode, index, value: longint; ptr: pointer; opt: Single): longint;
var
   v       : longint;
   keyCode : VstKeyCode;
begin
  v := 0;

  case opcode of
    // VstEvents
    effProcessEvents:  v := processEvents(ptr);

    // parameters and programs
    effCanBeAutomated:  if canParameterBeAutomated(index) then  v := 1
                        else v := 0;
    effString2Parameter:  if string2parameter(index, ptr) then  v := 1
                          else v := 0;
    effGetNumProgramCategories:  v := getNumCategories;
    effGetProgramNameIndexed:  if getProgramNameIndexed(value, index, ptr) then
                                  v := 1
                               else  v := 0;
    effCopyProgram:  if copyProgram(index) then  v := 1 else  v := 0;

    // connections, configuration
    effConnectInput:  begin
        inputConnected(index, (value <> 0));
        v := 1;
      end;
    effConnectOutput:  begin
        outputConnected (index, (value <> 0));
        v := 1;
      end;
    effGetInputProperties:  if getInputProperties(index, ptr) then  v := 1
                            else  v := 0;
    effGetOutputProperties:  if getOutputProperties(index, ptr) then  v := 1
                             else  v := 0;
    effGetPlugCategory:  v := getPlugCategory;

    // realtime
    effGetCurrentPosition:  v := reportCurrentPosition;
    effGetDestinationBuffer:  v := longint(reportDestinationBuffer);

    // offline
    effOfflineNotify:  if offlineNotify(ptr, value, (index <> 0)) then  v := 1
                       else  v := 0;
    effOfflinePrepare:  if offlinePrepare(ptr, value) then  v := 1 else  v := 0;
    effOfflineRun:  if offlineRun(ptr, value) then  v := 1 else  v := 0;

    // other
    effSetSpeakerArrangement:  if setSpeakerArrangement(pointer(value), ptr) then
                                 v := 1
                               else  v := 0;
    effProcessVarIo:  if processVariableIo(ptr) then  v := 1 else  v := 0;
    effSetBlockSizeAndSampleRate:  begin
        setBlockSizeAndSampleRate(value, opt);
        v := 1;
      end;
    effSetBypass:  if setBypass(value <> 0) then  v := 1 else  v := 0;
    effGetEffectName:  if getEffectName(ptr) then  v := 1 else  v := 0;
    effGetErrorText:  if getErrorText(ptr) then  v := 1 else  v := 0;
    effGetVendorString:  if getVendorString(ptr) then  v := 1 else  v := 0;
    effGetProductString:  if getProductString(ptr) then  v := 1 else  v := 0;
    effGetVendorVersion:  v := getVendorVersion;
    effVendorSpecific:  v := vendorSpecific(index, value, ptr, opt);
    effCanDo:  v := canDo(ptr);
    effGetIcon:  v := longint(getIcon);
    effSetViewPosition:  if setViewPosition(index, value) then  v := 1
                         else  v := 0;
    effGetTailSize:  v := getTailSize;
    effIdle:  v := fxIdle;
    effGetParameterProperties:  if getParameterProperties(index, ptr) then
                                  v := 1
                                else  v := 0;
    effKeysRequired:  if keysRequired then  v := 0 else  v := 1; // reversed to keep v1 compatibility
    effGetVstVersion:  v := getVstVersion;

    {$IFDEF VST_2_1_EXTENSIONS}
    effEditKeyDown:
      if Assigned(editor) then
      begin
        keyCode.character := index;
        keyCode.virt := value;
        keyCode.modifier := Round(opt);
	v := editor.onKeyDown(keyCode);
      end;

    effEditKeyUp:
      if Assigned(editor) then
      begin
        keyCode.character := index;
        keyCode.virt := value;
        keyCode.modifier := Round(opt);
        v := editor.onKeyUp(keyCode);
      end;

    effSetEditKnobMode:  if Assigned(editor) then
                           v := editor.setKnobMode(value);

    effGetMidiProgramName:  v := getMidiProgramName(index, PMidiProgramName(ptr));
    effGetCurrentMidiProgram:  v := getCurrentMidiProgram(index, PMidiProgramName(ptr));
    effGetMidiProgramCategory:  v := getMidiProgramCategory(index, PMidiProgramCategory(ptr));
    effHasMidiProgramsChanged:  if hasMidiProgramsChanged(index) then
                                  v := 1
                                else
                                  v := 0;
    effGetMidiKeyName:  if getMidiKeyName(index, PMidiKeyName(ptr)) then
                          v := 1
                        else
                          v := 0;
    effBeginSetProgram:  if beginSetProgram then
                           v := 1
                         else
                           v := 0;
    effEndSetProgram:  if endSetProgram then
                         v := 1
                       else
                         v := 0;
    {$ENDIF} //VST_2_1_EXTENSIONS

    {$IFDEF VST_2_3_EXTENSIONS}
    effGetSpeakerArrangement:  if getSpeakerArrangement(PVstSpeakerArrangement(value), PVstSpeakerArrangement(ptr)) then
                                 v := 1
                               else
                                 v := 0;
    effSetTotalSampleToProcess:  v := setTotalSampleToProcess(value);
    effShellGetNextPlugin:  v := getNextShellPlugin (pAnsiChar(ptr));
    effStartProcess:  v := startProcess;
    effStopProcess:  v := stopProcess ();
    effSetPanLaw:  if setPanLaw(value, opt) then
                     v := 1
                   else
                     v := 0;
    effBeginLoadBank:  v := beginLoadBank(PVstPatchChunkInfo(ptr));
    effBeginLoadProgram:  v := beginLoadProgram(PVstPatchChunkInfo(ptr));
    {$ENDIF} // VST_2_3_EXTENSIONS

    // version 1.0 or unknown
    else
      v := inherited dispatcher(opcode, index, value, ptr, opt);
  end;

  Result := v;
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.wantEvents(filter: longint);
begin
  if Assigned(audioMaster) then
    audioMaster(Effect, audioMasterWantMidi, 0, filter, nil, 0);
end;

//------------------------------------------------------------------------------
function AudioEffectX.getTimeInfo(filter: longint): PVstTimeInfo;
begin
  if Assigned(audioMaster) then
    Result := PVstTimeInfo(audioMaster (Effect, audioMasterGetTime, 0, filter, nil, 0))
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.setTimeInfo(filter: longint; ti: PVstTimeInfo);
begin
  if Assigned(audioMaster) then
    audioMaster(Effect, audioMasterSetTime, 0, filter, ti, 0);
end;

//------------------------------------------------------------------------------
function AudioEffectX.tempoAt(pos: longint): longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster (Effect, audioMasterTempoAt, 0, pos, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.sendVstEventsToHost(events: PVstEvents): boolean;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterProcessEvents, 0, 0, events, 0) = 1
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
// parameters
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function AudioEffectX.getNumAutomatableParameters: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetNumAutomatableParameters, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getParameterQuantization: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetParameterQuantization, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// configuration
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function AudioEffectX.ioChanged: boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterIOChanged, 0, 0, nil, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.needIdle: boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterNeedIdle, 0, 0, nil, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.sizeWindow(width, height: longint): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterSizeWindow, width, height, nil, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.updateSampleRate: Double;
var
   res: longint;
begin
  if Assigned(audioMaster) then
  begin
    res := audioMaster(Effect, audioMasterGetSampleRate, 0, 0, nil, 0);
    if (res > 0) then
      sampleRate := res;
  end;

  Result := sampleRate;
end;

//------------------------------------------------------------------------------
function AudioEffectX.updateBlockSize: longint;
var
   res: longint;
begin
  if Assigned(audioMaster) then
  begin
    res := audioMaster(Effect, audioMasterGetBlockSize, 0, 0, nil, 0);
    if (res > 0) then
      blockSize := res;
  end;

  Result := blockSize;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getInputLatency: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetInputLatency, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getOutputLatency: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetOutputLatency, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getPreviousPlug(input: longint): PAEffect;
begin
  if Assigned(audioMaster) then
    Result := PAEffect(audioMaster (Effect, audioMasterGetPreviousPlug, 0, 0, nil, 0))
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getNextPlug(output: longint): PAEffect;
begin
  if Assigned(audioMaster) then
    Result := PAEffect(audioMaster(Effect, audioMasterGetNextPlug, 0, 0, nil, 0))
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
// configuration
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function AudioEffectX.willProcessReplacing: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterWillReplaceOrAccumulate, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getCurrentProcessLevel: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetCurrentProcessLevel, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getAutomationState: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetAutomationState, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.wantAsyncOperation(state: boolean);
begin
  if state then
    Effect^.flags  := Effect^.flags or effFlagsExtIsAsync
  else
    Effect^.flags := Effect^.flags and not effFlagsExtIsAsync;
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.hasExternalBuffer(state: boolean);
begin
  if state then
    Effect^.flags := Effect^.flags or effFlagsExtHasBuffer
  else
    Effect^.flags := Effect^.flags and not effFlagsExtHasBuffer;
end;

//------------------------------------------------------------------------------
// offline
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
function AudioEffectX.offlineRead(offline: PVstOfflineTask; option: VstOfflineOption;
                                  readSource: boolean): boolean;
var
   ireadsource: integer;
begin
  if readSource then  ireadsource := 1 else  ireadsource := 0;

  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterOfflineRead, ireadsource, option, offline, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.offlineWrite(offline: PVstOfflineTask; option: VstOfflineOption): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterOfflineWrite, 0, option, offline, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.offlineStart(ptr: PVstAudioFile; numAudioFiles: longint;
                      numNewAudioFiles: longint): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterOfflineStart, numNewAudioFiles,
                           numAudioFiles, ptr, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.offlineGetCurrentPass: longint;
begin
  if Assigned(audioMaster) then
  begin
    if audioMaster(Effect, audioMasterOfflineGetCurrentPass, 0, 0, nil, 0) <> 0 then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.offlineGetCurrentMetaPass: longint;
begin
  if Assigned(audioMaster) then
  begin
    if (audioMaster(Effect, audioMasterOfflineGetCurrentMetaPass, 0, 0, nil, 0) <> 0) then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
// other
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
procedure AudioEffectX.setOutputSampleRate(sampleRate: Single);
begin
  if Assigned(audioMaster) then
    audioMaster(Effect, audioMasterSetOutputSampleRate, 0, 0, nil, sampleRate);
end;

//------------------------------------------------------------------------------
function AudioEffectX.getSpeakerArrangement(var pluginInput, pluginOutput: PVstSpeakerArrangement): boolean;
begin
  pluginInput := nil;
  pluginOutput := nil;
  Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getHostVendorString(text: pAnsiChar): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterGetVendorString, 0, 0, text, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getHostProductString(text: pAnsichar): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterGetProductString, 0, 0, text, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getHostVendorVersion: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetVendorVersion, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.hostVendorSpecific(lArg1, lArg2: longint; ptrArg: pointer;
                                         floatArg: Single): longint;
begin
  Result := 0;
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterVendorSpecific, lArg1, lArg2,
                          ptrArg, floatArg);
end;

//------------------------------------------------------------------------------
function AudioEffectX.canHostDo(text: pAnsiChar): longint;
begin
  Result := 0;
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterCanDo, 0, 0, text, 0);
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.isSynth(state: boolean);
begin
  if state then
    Effect^.flags := Effect^.flags or effFlagsIsSynth
  else
    Effect^.flags := Effect^.flags and not effFlagsIsSynth;
end;

//------------------------------------------------------------------------------
procedure AudioEffectX.noTail(state: boolean);
begin
  if state then
    Effect^.flags := Effect^.flags or effFlagsNoSoundInStop
  else
    Effect^.flags := Effect^.flags and not effFlagsNoSoundInStop;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getHostLanguage: longint;
begin
  if Assigned(audioMaster) then
    Result := audioMaster(Effect, audioMasterGetLanguage, 0, 0, nil, 0)
  else
    Result := 0;
end;

//------------------------------------------------------------------------------
function AudioEffectX.openWindow(aWindow: PVstWindow): pointer;
begin
  if Assigned(audioMaster) then
    Result := pointer(audioMaster(Effect, audioMasterOpenWindow, 0, 0, aWindow, 0))
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
function AudioEffectX.closeWindow(aWindow: PVstWindow): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterCloseWindow, 0, 0, aWindow, 0) <> 0)
  else
    Result := FALSE;
end;

//------------------------------------------------------------------------------
function AudioEffectX.getDirectory: pointer;
begin
  if Assigned(audioMaster) then
    Result := pointer(audioMaster(Effect, audioMasterGetDirectory, 0, 0, nil, 0))
  else
    Result := nil;
end;

//------------------------------------------------------------------------------
function AudioEffectX.updateDisplay: boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterUpdateDisplay, 0, 0, nil, 0) <> 0)
  else
    Result := FALSE;
end;

function AudioEffectX.processEvents(events: PVstEvents): longint;
begin
  Result := 0;
end;

function AudioEffectX.canParameterBeAutomated(index: longint): boolean;
begin
  Result := TRUE;
end;

function AudioEffectX.string2parameter(index: longint; text: pAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getChannelParameter(channel, index: longint): Single;
begin
  Result := 0;
end;

function AudioEffectX.getNumCategories: longint;
begin
  Result := 1;
end;

function AudioEffectX.getProgramNameIndexed(category, index: longint; text: pAnsichar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.copyProgram(destination: longint): boolean;
begin
  Result := FALSE;
end;

procedure AudioEffectX.inputConnected(index: longint; state: boolean);
begin
  {}
end;

procedure AudioEffectX.outputConnected(index: longint; state: boolean);
begin
  {}
end;

function AudioEffectX.getInputProperties(index: longint; properties: PVstPinProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getOutputProperties(index: longint; properties: PVstPinProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getPlugCategory: VstPlugcategory;
begin
  if (Effect^.flags and effFlagsIsSynth = effFlagsIsSynth) then
    Result := kPlugCategSynth
  else
    Result := kPlugCategUnknown;
end;

function AudioEffectX.reportCurrentPosition: longint;
begin
  Result := 0;
end;

function AudioEffectX.reportDestinationBuffer: PSingle;
begin
  Result := nil;
end;

function AudioEffectX.offlineNotify(ptr: PVstAudioFile; numAudioFiles: longint; start: boolean): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.offlinePrepare(offline: PVstOfflineTask; count: longint): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.offlineRun(offline: PVstOfflineTask; count: longint): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.offlineGetNumPasses: longint;
begin
  Result := 0;
end;

function AudioEffectX.offlineGetNumMetaPasses: longint;
begin
  Result := 0;
end;

function AudioEffectX.processVariableIo(varIo: PVstVariableIo): boolean;
begin
    Result := FALSE;
end;

function AudioEffectX.setSpeakerArrangement(pluginInput, pluginOutput: PVstSpeakerArrangement): boolean;
begin
  Result := FALSE;
end;

procedure AudioEffectX.setBlockSizeAndSampleRate(aBlockSize: longint; aSampleRate: Single);
begin
  fBlockSize := aBlockSize;
  fSampleRate := aSampleRate;
end;

function AudioEffectX.setBypass(onOff: boolean): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getEffectName(name: pAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getErrorText(text: pAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getVendorString(text: pAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getProductString(text: pAnsiChar): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getVendorVersion: longint;
begin
  Result := 0;
end;

function AudioEffectX.vendorSpecific(lArg1, lArg2: longint; ptrArg: pointer;
 floatArg: Single): longint;
begin
  Result := 0;
end;

function AudioEffectX.canDo(text: pAnsiChar): longint;
begin
  Result := 0;
end;

function AudioEffectX.getIcon: pointer;
begin
  Result := nil;
end;

function AudioEffectX.setViewPosition(x, y: longint): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getTailSize: longint;
begin
  Result := 0;
end;

function AudioEffectX.fxIdle: longint;
begin
  Result := 0;
end;

function AudioEffectX.getParameterProperties(index: longint; p: PVstParameterProperties): boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.keysRequired: boolean;
begin
  Result := FALSE;
end;

function AudioEffectX.getVstVersion: longint;
begin
  Result := 2;
  {$IFDEF VST_2_1_EXTENSIONS}
  Result := 2100;
  {$ENDIF}
  {$IFDEF VST_2_2_EXTENSIONS}
  Result := 2200;
  {$ENDIF}
  {$IFDEF VST_2_3_EXTENSIONS}
  Result := 2300;
  {$ENDIF}
end;

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.getMidiProgramName(channel: longint; midiProgramName: PMidiProgramName): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.getCurrentMidiProgram(channel: longint; currentProgram: PMidiProgramName): longint;
begin
  Result := -1;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.getMidiProgramCategory(channel: longint; category: PMidiProgramCategory): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.hasMidiProgramsChanged(channel: longint): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.getMidiKeyName(channel: longint; keyName: PMidiKeyName): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.beginSetProgram: boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.endSetProgram: boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.beginEdit(index: longint): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterBeginEdit, index, 0, nil, 0) <> 0)
  else
    Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.endEdit(index: longint): boolean;
begin
  if Assigned(audioMaster) then
    Result := (audioMaster(Effect, audioMasterEndEdit, index, 0, nil, 0) <> 0)
  else
    Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AudioEffectX.openFileSelector(ptr: PVstFileSelect): boolean;
begin
  if Assigned(audioMaster) and (ptr <> nil) then
    Result := (audioMaster(Effect, audioMasterOpenFileSelector, 0, 0, ptr, 0) <> 0)
  else
    Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_2_EXTENSIONS}
function AudioEffectX.closeFileSelector(ptr: PVstFileSelect): boolean;
begin
  if Assigned(audioMaster) and (ptr <> nil) then
    Result := (audioMaster(Effect, audioMasterCloseFileSelector, 0, 0, ptr, 0) <> 0)
  else
    Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_2_EXTENSIONS}
function AudioEffectX.getChunkFile(nativePath: pointer): boolean;
begin
  if Assigned(audioMaster) and (nativePath <> nil) then
    Result := (audioMaster(Effect, audioMasterGetChunkFile, 0, 0, nativePath, 0) <> 0)
  else
    Result := FALSE;
end;
{$ENDIF}

function AudioEffectX.getInputSpeakerArrangement: PVstSpeakerArrangement;
begin
  if Assigned(audioMaster) then
    Result := PVstSpeakerArrangement(audioMaster(Effect, audioMasterGetInputSpeakerArrangement, 0, 0, nil, 0))
  else
    Result := nil;
end;

function AudioEffectX.getOutputSpeakerArrangement: PVstSpeakerArrangement;
begin
  if Assigned(audioMaster) then
    Result := PVstSpeakerArrangement(audioMaster(Effect, audioMasterGetOutputSpeakerArrangement, 0, 0, nil, 0))
  else
    Result := nil;
end;

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.setTotalSampleToProcess(value: longint): longint;
begin
  Result := value;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.getNextShellPlugin(name: pAnsiChar): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.startProcess: longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.stopProcess: longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.setPanLaw(vType: longint; val: single): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.beginLoadBank(ptr: PVstPatchChunkInfo): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.beginLoadProgram(ptr: PVstPatchChunkInfo): longint;
begin
  Result := 0;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.allocateArrangement(var arrangement: PVstSpeakerArrangement; nbChannels: longint): boolean;
var
   size : longint;
begin
  if Assigned(arrangement) then
  begin
    FreeMem(arrangement);
    arrangement := nil;
  end;

  size := SizeOf(longint) + SizeOf(longint) + (nbChannels) * SizeOf(VstSpeakerProperties);
  GetMem(arrangement, size);
  if not Assigned(arrangement) then
  begin
    Result := FALSE;
    Exit;
  end;

  FillChar(arrangement^, size, 0);
  arrangement^.numChannels := nbChannels;
  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.deallocateArrangement(var arrangement: PVstSpeakerArrangement): boolean;
begin
  if Assigned(arrangement) then
  begin
    FreeMem(arrangement);
    arrangement := nil;
  end;
  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.copySpeaker(copyTo, copyFrom: PVstSpeakerProperties): boolean;
begin
  // We assume here that "to" exists yet, ie this function won't
  // allocate memory for the speaker (this will prevent from having
  // a difference between an Arrangement's number of channels and
  // its actual speakers...)
  if (copyFrom = nil) or (copyTo = nil) then
  begin
    Result := FALSE;
    Exit;
  end;

  StrCopy(copyTo^.name, copyFrom^.name);
  copyTo^.vType := copyFrom^.vType;
  copyTo^.azimuth := copyFrom^.azimuth;
  copyTo^.elevation := copyFrom^.elevation;
  copyTo^.radius := copyFrom^.radius;
  copyTo^.reserved := copyFrom^.reserved;
  CopyMemory(@(copyTo.future), @(copyFrom.future), 28);

  Result := TRUE;
end;
{$ENDIF}

{$IFDEF VST_2_3_EXTENSIONS}
function AudioEffectX.matchArrangement(var matchTo: PVstSpeakerArrangement; matchFrom: PVstSpeakerArrangement): boolean;
var
   i: integer;
begin
  if matchFrom = nil then
  begin
    Result := FALSE;
    Exit;
  end;

  if not deallocateArrangement(matchTo) or not allocateArrangement(matchTo, matchFrom^.numChannels) then
  begin
    Result := FALSE;
    Exit;
  end;

  matchTo^.vType := matchFrom^.vType;
  for i := 0 to matchTo^.numChannels-1 do
  begin
    if not copySpeaker(@(matchTo^.speakers[i]), @(matchFrom^.speakers[i])) then
    begin
      Result := FALSE;
      Exit;
    end;
  end;

  Result := FALSE;
end;
{$ENDIF}

end.
