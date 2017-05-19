//******************************************************************************
//
//  DAudioEffect.pas
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
//------------------------------------------------------------------------------
//
//  Classes : AudioEffect
//            AEffEditor
//
//  Functions : dispatchEffectClass
//              getParameterClass
//              setParameterClass
//              processClass
//              processClassReplacing
//
//******************************************************************************
unit
    DAudioEffect;

interface

// comment out these defines if you want to make a VST 1.0 plugin

{$DEFINE VST_2_1_EXTENSIONS} // version 2.1 extensions for Midi Program Names
{$DEFINE VST_2_2_EXTENSIONS} // version 2.2 extensions


uses
    DAEffect, DVSTUtils
    {$IFDEF VST_2_1_EXTENSIONS}, DAEffectX{$ENDIF};


type
    AEffEditor = class;

    AudioEffect = class
    private
      procedure SetEditor(newvalue: AEffEditor);
    protected
      // members
      fEffect     : AEffect;
      fSampleRate : Single;
      fEditor     : AEffEditor;
      numPrograms : longint;
      numParams   : longint;
      fBlockSize  : longint;
      procedure setSampleRate(newvalue: Single); virtual;
      procedure setBlockSize(newvalue: longint); virtual;
      function GetEffect: PAEffect;
    public
      curProgram  : longint;
      audioMaster : TAudioMasterCallbackFunc;

      constructor Create(audioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: longint);
      destructor Destroy; override;

      procedure setParameter(index: longint; value: Single); virtual;
      function getParameter(index: longint): Single; virtual;
      procedure setParameterAutomated(index: longint; value: Single); virtual;

      // called from audio master
      procedure Process(inputs, outputs: PPSingle; sampleFrames: longint); virtual; abstract;
      procedure ProcessReplacing(inputs, outputs: PPSingle; sampleFrames: longint); virtual;
      function Dispatcher(opcode, index, value: longint; ptr: pointer; opt: Single): longint; virtual;
      procedure Open; virtual;
      procedure Close; virtual;
      function getProgram: longint; virtual;
      procedure SetProgram(aProgram: longint); virtual;    // don't forget to set curProgram
      procedure setProgramName(name: pAnsichar); virtual;      // all following refer to curProgram
      procedure getProgramName(name: pAnsichar); virtual;
      procedure getParameterLabel(index: longint; text: pAnsichar); virtual;
      procedure getParameterDisplay(index: longint; text: pAnsichar); virtual;
      procedure getParameterName(index: longint; text: pAnsichar); virtual;
      function getVU: Single; virtual;
      function getChunk(var data: pointer; isPreset: Boolean): longint; virtual;   // returns byteSize
      function setChunk(data: pointer; byteSize: longint; isPreset: Boolean): longint; virtual;
      procedure Suspend; virtual;
      procedure Resume; virtual;

      // setup
      procedure setUniqueID(ID: longint); virtual;  // must call this!
      procedure setNumInputs(inputs: longint); virtual;
      procedure setNumOutputs(outputs: longint); virtual;
      procedure hasVU(state: boolean); virtual;
      procedure hasClip(state: boolean); virtual;
      procedure canMono(state: boolean); virtual;
      procedure canProcessReplacing(state: boolean); virtual;
      procedure programsAreChunks(state: boolean); virtual;
      procedure setRealtimeQualities(qualities: longint); virtual;
      procedure setOfflineQualities(qualities: longint); virtual;
      procedure setInitialDelay(delay: longint); virtual;

      // host communication
      function getMasterVersion: longint; virtual;
      function getCurrentUniqueID: longint; virtual;
      procedure masterIdle; virtual;
      function isInputConnected(input: longint): boolean; virtual;
      function isOutputConnected(output: longint): boolean; virtual;

      // properties
      property Effect: PAEffect read GetEffect;
      property Editor: AEffEditor read fEditor write SetEditor;
      property SampleRate: Single read fSampleRate write setSampleRate;
      property BlockSize: longint read fBlockSize write setBlockSize;
    end;

    AEffEditor = class
    protected
      fEffect: AudioEffect;
      systemWindow: pointer;
      updateFlag: longint;
    public
      constructor Create(aEffect: AudioEffect); virtual;
      function getRect(var rect: PERect): longint; virtual;
      function open(ptr: pointer): longint; virtual;
      procedure close; virtual;
      procedure idle; virtual;
      procedure update; virtual;
      procedure postUpdate; virtual;

      {$IFDEF VST_2_1_EXTENSIONS}
      function onKeyDown(var keyCode: VstKeyCode): longint; virtual;
      function onKeyUp(var keyCode: VstKeyCode): longint; virtual;
      function setKnobMode(val: integer): longint; virtual;
      function onWheel(distance: single): boolean; virtual; 
      {$ENDIF}

      property Effect: AudioEffect read fEffect;
    end;





// Needs to be defined by the audio effect and is
// called to create the audio effect object instance.
// function createEffectInstance(audioMaster: TAudioMasterCallbackFunc): AudioEffect;

function dispatchEffectClass(effect: PAEffect; opcode, index, value: longint;
                             ptr: pointer; opt: Single): longint; cdecl;
function getParameterClass(effect: PAEffect; index: longint): Single; cdecl;
procedure setParameterClass(effect: PAEffect; index: longint; value: Single); cdecl;
procedure processClass(effect: PAEffect; inputs, outputs: PPSingle;
                       sampleframes: longint); cdecl;
procedure processClassReplacing(effect: PAEffect; inputs, outputs: PPSingle;
                                sampleframes: longint); cdecl;






implementation

//------------------------------------------------------------------------------
//  AudioEffect
//------------------------------------------------------------------------------

constructor AudioEffect.Create(audioMaster: TAudioMasterCallbackFunc;
                               aNumPrograms, aNumParams: longint);
begin
  inherited Create;

  Self.audioMaster := audioMaster;
  fEditor := nil;
  Self.numPrograms := aNumPrograms;
  Self.numParams := aNumParams;
  Self.curProgram := 0;

  fEffect.magic := FourCharToLong(kEffectMagic[1], kEffectMagic[2], kEffectMagic[3], kEffectMagic[4]);
  fEffect.dispatcher := @dispatchEffectClass;
  fEffect.process := @processClass;
  fEffect.setParameter := @setParameterClass;
  fEffect.getParameter := @getParameterClass;
  fEffect.numPrograms := aNumPrograms;
  fEffect.numParams := aNumParams;
  fEffect.numInputs := 1;
  fEffect.numOutputs := 2;
  fEffect.flags := 0;
  fEffect.reservedForHost := nil;
  fEffect.resvd2 := 0;
  fEffect.initialDelay := 0;
  fEffect.realQualities := 0;
  fEffect.offQualities := 0;
  fEffect.ioRatio := 1;
  fEffect.vObject := Self;
  fEffect.user := nil;
  fEffect.uniqueID := FourCharToLong('N', 'o', 'E', 'f');  // you must set this!
  fEffect.version := 1;
  fEffect.processReplacing := processClassReplacing;

  fSampleRate := 44100;
  fBlockSize := 1024;
end;

destructor AudioEffect.Destroy;
begin
  if Assigned(Editor) then
    Editor.Free;

  inherited Destroy;
end;

function AudioEffect.dispatcher(opcode, index, value: longint; ptr: pointer;
                                opt: Single): longint;
var
   v  : longint;
   pe : PERect;
begin
  v := 0;

  case opcode of
    effOpen            :  Open;
    effClose           :  Close;
    effSetProgram      :  if (value < numPrograms) then  setProgram(value);
    effGetProgram      :  v := getProgram;
    effSetProgramName  :  setProgramName(ptr);
    effGetProgramName  :  getProgramName(ptr);
    effGetParamLabel   :  getParameterLabel(index, ptr);
    effGetParamDisplay :  getParameterDisplay(index, ptr);
    effGetParamName    :  getParameterName(index, ptr);

    effSetSampleRate   :  setSampleRate(opt);
    effSetBlockSize    :  setBlockSize(value);
    effMainsChanged    :  if (value = 0) then  Suspend else  Resume;
    effGetVu           :  v := round(getVu * 32767);

    // editor
    effEditGetRect     :  if Assigned(Editor) then
                          begin
                            pe := PPERect(ptr)^;
                            v := Editor.getRect(pe);
                            PPERect(ptr)^ := pe;
                          end;
    effEditOpen        :  if Assigned(editor) then
                             v := Editor.Open(ptr);
    effEditClose       :  if Assigned(editor) then  Editor.Close;
    effEditIdle        :  if Assigned(editor) then  Editor.Idle;

    // new
    effIdentify        :  v := FourChartoLong('N', 'v', 'E', 'f');
    effGetChunk        :  v := getChunk(pointer(ptr^), (index <> 0));
    effSetChunk        :  v := setChunk(ptr, value, (index <> 0));
  end;

  Result := v;
end;

procedure AudioEffect.setParameter(index: longint; value: Single);
begin
  {}
end;

procedure AudioEffect.setParameterAutomated(index: longint; value: Single);
begin
  setParameter(index, value);
  if Assigned(audioMaster) then
    audioMaster(Effect, audioMasterAutomate, index, 0, nil, value);  // value is in opt
end;


function AudioEffect.getParameter(index: longint): Single;
begin
  Result := 0;
end;

procedure AudioEffect.setEditor(newvalue: AEffEditor);
begin
  fEditor := newvalue;
  if Assigned(editor) then
    Effect^.flags := Effect^.flags or effFlagsHasEditor
  else
    Effect^.flags := Effect^.flags and not effFlagsHasEditor;
end;

procedure AudioEffect.ProcessReplacing(inputs, outputs: PPSingle; sampleFrames: longint);
begin
  {}
end;

procedure AudioEffect.Open;
begin
  {}
end;

procedure AudioEffect.Close;
begin
  {}
end;
               
function AudioEffect.getProgram: longint;
begin
  Result := curProgram;
end;

procedure AudioEffect.setProgram(aProgram: longint);
begin
  curProgram := aProgram;
end;

procedure AudioEffect.setProgramName(name: pAnsichar);
begin
  {}
end;

procedure AudioEffect.getProgramName(name: pAnsichar);
begin
  {}
end;

procedure AudioEffect.getParameterLabel(index: longint; text: pAnsichar);
begin
  {}
end;

procedure AudioEffect.getParameterDisplay(index: longint; text: pAnsichar);
begin
  {}
end;

procedure AudioEffect.getParameterName(index: longint; text: pAnsichar);
begin
  {}
end;

function AudioEffect.getVU: Single;
begin
  Result := 0;
end;

function AudioEffect.getChunk(var data: pointer; isPreset: Boolean): longint;
begin
  Result := 0;
end;

function AudioEffect.setChunk(data: pointer; byteSize: longint; isPreset: Boolean): longint;
begin
  Result := 0;
end;

procedure AudioEffect.setSampleRate(newvalue: Single);
begin
  fSampleRate := newvalue;
end;

procedure AudioEffect.setBlockSize(newvalue: longint);
begin
  fBlockSize := newvalue;
end;

procedure AudioEffect.Suspend;
begin
  {}
end;

procedure AudioEffect.Resume;
begin
  {}
end;

procedure AudioEffect.setUniqueID(ID: longint);
begin
  Effect^.uniqueID := ID;
end;

procedure AudioEffect.setNumInputs(inputs: longint);
begin
  Effect^.NumInputs := inputs;
end;

procedure AudioEffect.setNumOutputs(outputs: longint);
begin
  Effect^.NumOutputs := outputs;
end;

function AudioEffect.getMasterVersion: longint;
var
   version: longint;
begin
  version := 1;
  if Assigned(audioMaster) then
  begin
    version := audioMaster(Effect, audioMasterVersion, 0, 0, nil, 0);
    if (version = 0) then   // old
      version := 1;
  end;

    Result := version;
end;

function AudioEffect.getCurrentUniqueId: longint;
var
   id: longint;
begin
  id := 0;
  if Assigned(audioMaster) then
    id := audioMaster(Effect, audioMasterCurrentId, 0, 0, nil, 0);
  Result := id;
end;

//-----------------------------------------------------------------------------
procedure AudioEffect.masterIdle;
begin
  if Assigned(audioMaster) then
    audioMaster(Effect, audioMasterIdle, 0, 0, nil, 0);
end;

//-----------------------------------------------------------------------------
function AudioEffect.isInputConnected(input: longint): boolean;
var
   ret: longint;
begin
  ret := 0;
  if Assigned(audioMaster) then
    ret := audioMaster(Effect, audioMasterPinConnected, input, 0, nil, 0);

  Result := (ret = 0);  // return value is 0 for true
end;

//-----------------------------------------------------------------------------
function AudioEffect.isOutputConnected(output: longint): boolean;
var
   ret: longint;
begin
  ret := 0;
  if Assigned(audioMaster) then
    ret := audioMaster(Effect, audioMasterPinConnected, output, 1, nil, 0);

  Result := (ret = 0);    // return value is 0 for true
end;

//-----------------------------------------------------------------------------
// flags
procedure AudioEffect.hasVu(state: boolean);
begin
 if state then
  Effect^.flags := Effect^.flags or effFlagsHasVu
 else
  Effect^.flags := Effect^.flags and not effFlagsHasVu;
end;

procedure AudioEffect.hasClip(state: boolean);
begin
 if state then
  Effect^.flags := Effect^.flags or effFlagsHasClip
 else
  Effect^.flags := Effect^.flags and not effFlagsHasClip;
end;

procedure AudioEffect.canMono(state: boolean);
begin
 if state then
  Effect^.flags := Effect^.flags or effFlagsCanMono
 else
  Effect^.flags := Effect^.flags and not effFlagsCanMono;
end;

procedure AudioEffect.canProcessReplacing(state: boolean);
begin
 if state then
  Effect^.flags := Effect^.flags or effFlagsCanReplacing
 else
  Effect^.flags := Effect^.flags and not effFlagsCanReplacing;
end;

procedure AudioEffect.programsAreChunks(state: boolean);
begin
 if state then
  Effect^.flags := Effect^.flags or effFlagsProgramChunks
 else
  Effect^.flags := Effect^.flags and not effFlagsProgramChunks;
end;

procedure AudioEffect.setRealtimeQualities(qualities: longint);
begin
 Effect^.realQualities := qualities;
end;

procedure AudioEffect.setOfflineQualities(qualities: longint);
begin
 Effect^.offQualities := qualities;
end;

procedure AudioEffect.setInitialDelay(delay: longint);
begin
 Effect^.initialDelay := delay;
end;

function AudioEffect.GetEffect: PAEffect;
begin
 Result := @fEffect;
end;

//------------------------------------------------------------------------------
//  AEffEditor
//------------------------------------------------------------------------------

constructor AEffEditor.Create(aEffect: AudioEffect);
begin
  inherited Create;
  fEffect := aEffect;
  updateFlag := 0;
end;

function AEffEditor.getRect(var rect: PERect): longint;
begin
  rect := nil;
  Result := 0;
end;

function AEffEditor.open(ptr: pointer): longint;
begin
  systemWindow := ptr;
  Result := 0;
end;

procedure AEffEditor.close;
begin

end;

procedure AEffEditor.idle;
begin
  if updateFlag <> 0 then
  begin
    updateFlag := 0;
    update;
  end;
end;

procedure AEffEditor.update;
begin
  {}
end;

procedure AEffEditor.postUpdate;
begin
  updateFlag := 1;
end;






//------------------------------------------------------------------------------
// Functions
//------------------------------------------------------------------------------

function dispatchEffectClass(effect: PAEffect; opcode, index, value: longint;
                             ptr: pointer; opt: Single): longint;
var
   ae: AudioEffect;
begin
  ae := AudioEffect(effect^.vObject);

  if (opcode = effClose) then
  begin
    ae.dispatcher(opcode, index, value, ptr, opt);
    ae.Free;
    Result := 1;
  end
  else
    Result := ae.dispatcher(opcode, index, value, ptr, opt);
end;

//-----------------------------------------------------------------------------
function getParameterClass(effect: PAEffect; index: longint): Single;
var
   ae: AudioEffect;
begin
  ae := AudioEffect(effect^.vObject);
  Result := ae.getParameter(index);
end;

//-----------------------------------------------------------------------------
procedure setParameterClass(effect: PAEffect; index: longint; value: Single);
var
   ae: AudioEffect;
begin
  ae := AudioEffect(effect^.vObject);
  ae.setParameter(index, value);
end;

//-----------------------------------------------------------------------------
procedure processClass(effect: PAEffect; inputs, outputs: PPSingle;
                       sampleframes: longint);
var
   ae: AudioEffect;
begin
  ae := AudioEffect(effect^.vObject);
  ae.process(inputs, outputs, sampleFrames);
end;

//-----------------------------------------------------------------------------
procedure processClassReplacing(effect: PAEffect; inputs, outputs: PPSingle;
                                sampleframes: longint);
var
   ae: AudioEffect;
begin
  ae := AudioEffect(effect^.vObject);
  ae.processReplacing(inputs, outputs, sampleFrames);
end;

//-----------------------------------------------------------------------------


{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.onKeyDown(var keyCode: VstKeyCode): longint;
begin
  Result := -1;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.onKeyUp(var keyCode: VstKeyCode): longint;
begin
  Result := -1;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.onWheel(distance: single): boolean;
begin
  Result := FALSE;
end;
{$ENDIF}

{$IFDEF VST_2_1_EXTENSIONS}
function AEffEditor.setKnobMode(val: integer): longint;
begin
  Result := 0;
end;
{$ENDIF}

initialization
  Set8087CW(Default8087CW or $3F);
end.
