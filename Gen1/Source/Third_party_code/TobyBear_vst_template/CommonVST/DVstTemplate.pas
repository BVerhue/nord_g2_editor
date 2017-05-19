unit DVstTemplate;
{$DEFINE FixedBuffer}
// For the G2 VST No audio processing is used. And when FIXEDBUFFER = False
// samplebuffers are constantly newly allocated which fragments memory!
{remove the line above to let the host define the buffer size}



interface
uses Windows, DVstUtils, DAEffect, DAEffectX, DDspUtils,
     DAudioEffect, DAudioEffectX, Forms, Messages, Classes;
              
const WM_EDITOROPEN = WM_USER + 961;
      maxMidiEvents = 2048;

type                       
 TEditorForm = class of TForm;

 TParameterChangeEvent = procedure(Index: Integer) of object;

 TVstCanDo = (sendVstEvents, sendVstMidiEvent, sendVstTimeInfo,
  receiveVstEvents, receiveVstMidiEvent, receiveVstTimeInfo,
  offline, plugAsChannelInsert, plugAsSend, mixDryWet, noRealTime,
  multipass, metapass, _1in1out, _1in2out, _2in1out, _2in2out,
  _2in4out, _4in2out, _4in4out, _4in8out, _8in4out, _8in8out,
  midiProgramNames, conformsToWindowRules, bypass);
 TVstCanDos = set of TVstCanDo;

 TVstPluginCategory = (cgUnknown, cgEffect, cgSynth, cgAnalysis,
  cgMastering, cgSpacializer, cgRoomFx, cgSurroundFx, cgRestoration,
  cgOfflineProcess, cgShell, cgGenerator);

 TVstPluginProperty = (prIsSynth, prHasVu, prHasClip,
  prCanMono, prCanReplacing, prProgramsAreChunks,
  prNoSoundinStop, prIsAsync, prExtHasBuffer);
 TVstPluginProperties = set of TVstPluginProperty;

 TCurveType = (ctLinear, ctLogarithmic, ctExponential);

 TVstParameterProperty = class
 private
  i1, i2: single;
  function Smooth(i: single): single;
 public
  min, max: single;
  curve: TCurveType;
  curveFactor: single;
  name: array[0..30] of AnsiChar;
  units: array[0..8] of AnsiChar;
  smoothingFactor: single;
 end;

 TVstProgram = class
  fPar: array of single;
  name: array[0..50] of AnsiChar;
 end;

 TVstTemplateEditor = class;

 TVstTemplate = class(AudioEffectX)
 private
  InsTmp, OutsTmp: TArrayOfSingleArray;
  fOnEditorIdle: TNotifyEvent;
  fOnParameterChangeEvent: TParameterChangeEvent;
  MidiEvent: VstEvents;
  numMidiEvents: integer;
  fTempo: single;
  fNumPrograms: integer;
  fNumParams: integer;
  fCanDos: TVstCanDos;
  fNumInputs: integer;
  fNumOutputs: integer;
  fID: AnsiString;
  fVendorVersion: integer;
  fEffectName: AnsiString;
  fVendorName: AnsiString;
  fProductName: AnsiString;
  fPlugCategory: TVstPluginCategory;
  fInitialDelay: integer;
  fTailSize: integer;
  fPluginProperties: TVstPluginProperties;
  fEditorNeedsUpdate: boolean;
  fBufferSize: integer;
  function GetCurProgram: integer;
  function GetPluginEditor: TVstTemplateEditor;
  procedure SetfNumInputs(const Value: integer);
  procedure SetfNumOutputs(const Value: integer);
  procedure SetfInitialDelay(const Value: integer);
  procedure SetBufferSize(const Value: integer);
 public
  sampleCounter: integer;
  ParameterProperties: array of TVstParameterProperty;
  Programs: array of TVstProgram;
  function getParameterEx(index: integer): single;
  procedure setParameterEx(index: integer; const Value: single); virtual;
  property ParameterEx[index: integer]: single read getParameterEx write setParameterEx;
  procedure MIDI_Out(b1, b2, b3, b4: byte; offset: integer = 0);
  procedure MIDI_CC(ch, num, val: integer; offset: integer = 0);
  procedure MIDI_ChannelAftertouch(ch, val: integer; offset: integer = 0);
  procedure MIDI_NoteOff(ch, note, val: integer; offset: integer = 0);
  procedure MIDI_NoteOn(ch, note, val: integer; offset: integer = 0);
  procedure MIDI_PitchBend(ch, val: integer; offset: integer = 0);
  procedure MIDI_PitchBend2(ch, x1, x2: integer; offset: integer = 0);
  procedure MIDI_PolyAftertouch(ch, note, val: integer; offset: integer = 0);
  procedure MIDI_ProgramChange(ch, val: integer; offset: integer = 0);
  constructor Create(audioMaster: TAudioMasterCallbackFunc; numPrograms: integer; numParams: integer; EditorForm: TEditorForm);
  destructor Destroy; override;
  procedure InitializeParameters; virtual;
  procedure process(inputs, outputs: PPSingle; sampleframes: Longint); override;
  procedure processReplacing(inputs, outputs: PPSingle; sampleframes: Longint); override;
  procedure processAudio(const inputs, outputs: TArrayOfSingleArray; sampleframes: Longint); virtual;
  procedure processMIDI(ev: VstMidiEvent); virtual;
  procedure setProgram(aProgram: Longint); override;
  function canDo(text: PAnsiChar): longint; override;
  procedure setProgramName(name: PAnsiChar); override;
  procedure getProgramName(name: PAnsiChar); override;
  function processEvents(ev: PVstEvents): longint; override;
  procedure Resume; override;
  function getVendorVersion: longint; override;
  function getProductString(text: PAnsiChar): boolean; override;
  function getEffectName(name: PAnsiChar): boolean; override;
  function getVendorString(text: PAnsiChar): boolean; override;
  procedure getParameterLabel(index: longint; text: PAnsiChar); override;
  procedure getParameterName(index: longint; text: PAnsiChar); override;
  procedure getParameterDisplay(index: longint; text: PAnsiChar); override;
  function getProgramNameIndexed(category, index: longint; text: PAnsiChar): boolean; override;
  procedure setParameter(index: Longint; value: Single); override;
  function getParameter(index: Longint): Single; override;
  procedure Suspend; override;
  function getInputProperties(index: longint; properties: PVstPinProperties): boolean; override;
  function getOutputProperties(index: longint; properties: PVstPinProperties): boolean; override;
  property PluginEditor: TVstTemplateEditor read GetPluginEditor;
 published
  property BufferSize: integer read fBuffersize write SetBufferSize;
  property OnEditorIdle: TNotifyEvent read fOnEditorIdle write fOnEditorIdle;
  property editorNeedsUpdate: boolean read fEditorNeedsUpdate write fEditorNeedsUpdate;
  property canDos: TVstCanDos read fCanDos write fCanDos;
  property numInputs: integer read fNumInputs write SetfNumInputs;
  property numOutputs: integer read fNumOutputs write SetfNumOutputs;
  property UniqueID: AnsiString read fID write fID;
  property EffectName: AnsiString read fEffectName write fEffectName;
  property ProductName: AnsiString read fProductName write fProductName;
  property VendorName: AnsiString read fVendorName write fVendorName;
  property VendorVersion: integer read fVendorVersion write fVendorVersion;
  property PlugCategory: TVstPluginCategory read fPlugCategory write fPlugCategory;
  property Properties: TVstPluginProperties read fPluginProperties write fPluginProperties;
  property initialDelay: integer read fInitialDelay write SetfInitialDelay;
  property tailSize: integer read fTailSize write fTailSize;
  property CurrentProgram: integer read GetCurProgram write SetProgram;
  property Tempo: single read fTempo;
  property OnParameterChange: TParameterChangeEvent read fOnParameterChangeEvent write fOnParameterChangeEvent;
 end;

 //this is the editor class
 TVstTemplateEditor = class(AEffEditor)
 private
  r: ERect;
  useCount: Longint;
  EdForm: TEditorForm;
  systemWindow: HWnd;
 public
  Editor: TForm;
  constructor Create(effect: AudioEffect; EditorForm: TEditorForm); reintroduce;
  destructor Destroy; override;
  function getRect(var rect: PERect): longint; override;
  function Open(ptr: Pointer): Longint; override;
  procedure Close; override;
  procedure Idle; override;
  procedure Update; override;
 end;

implementation
uses SysUtils;

function TVstTemplate.canDo(text: PAnsiChar): longint;
begin
 Result := -1;
 if StrComp(text, 'receiveVstEvents') = 0 then
  Result := integer(receiveVstEvents in canDos) else
 if StrComp(text, 'receiveVstMidiEvent') = 0 then
  Result := integer(receiveVstMidiEvent in canDos) else
 if StrComp(text, 'receiveVstTimeInfo') = 0 then
  Result := integer(receiveVstTimeInfo in canDos) else
 if StrComp(text, 'sendVstEvents') = 0 then
  Result := integer(sendVstEvents in canDos) else
 if StrComp(text, 'sendVstMidiEvent') = 0 then
  Result := integer(sendVstMidiEvent in canDos) else
 if StrComp(text, 'sendVstTimeInfo') = 0 then
  Result := integer(sendVstTimeInfo in canDos) else
 if StrComp(text, 'offline') = 0 then
  Result := integer(offline in canDos) else
 if StrComp(text, 'plugAsChannelInsert') = 0 then
  Result := integer(plugAsChannelInsert in canDos) else
 if StrComp(text, 'plugAsSend') = 0 then
  Result := integer(plugAsSend in canDos) else
 if StrComp(text, 'mixDryWet') = 0 then
  Result := integer(mixDryWet in canDos) else
 if StrComp(text, 'noRealTime') = 0 then
  Result := integer(noRealTime in canDos) else
 if StrComp(text, 'multipass') = 0 then
  Result := integer(multipass in canDos) else
 if StrComp(text, 'metapass') = 0 then
  Result := integer(metapass in canDos) else
 if StrComp(text, '1in1out') = 0 then
  Result := integer(_1in1out in canDos) else
 if StrComp(text, '1in2out') = 0 then
  Result := integer(_1in2out in canDos) else
 if StrComp(text, '2in1out') = 0 then
  Result := integer(_2in1out in canDos) else
 if StrComp(text, '2in2out') = 0 then
  Result := integer(_2in2out in canDos) else
 if StrComp(text, '2in4out') = 0 then
  Result := integer(_2in4out in canDos) else
 if StrComp(text, '4in2out') = 0 then
  Result := integer(_4in2out in canDos) else
 if StrComp(text, '4in4out') = 0 then
  Result := integer(_4in4out in canDos) else
 if StrComp(text, '4in8out') = 0 then
  Result := integer(_4in8out in canDos) else
 if StrComp(text, '8in4out') = 0 then
  Result := integer(_8in4out in canDos) else
 if StrComp(text, '8in8out') = 0 then
  Result := integer(_8in8out in canDos) else
 if StrComp(text, 'midiProgramNames') = 0 then
  Result := integer(midiProgramNames in canDos) else
 if StrComp(text, 'conformsToWindowRules') = 0 then
  Result := integer(conformsToWindowRules in canDos) else
 if StrComp(text, 'bypass') = 0 then
  Result := integer(bypass in canDos);
end;

constructor TVstTemplate.Create(
 audioMaster: TAudioMasterCallbackFunc;
 numPrograms: integer;
 numParams: integer;
 EditorForm: TEditorForm);
var i: integer;
    s: AnsiString;
begin
 numMidiEvents := 0;
 fNumPrograms := numPrograms;
 fNumParams := numParams;
 if fBufferSize = 0 then BufferSize := 2048;
 inherited Create(audioMaster, fNumPrograms, fNumParams);
 // create the programs
 SetLength(Programs, fNumPrograms);
 for i := fNumPrograms - 1 downto 0 do
 begin
  Programs[i] := TVstProgram.Create;
  StrCopy(Programs[i].Name, 'Init'); // set program name
  SetLength(Programs[i].fPar, fNumParams);
  FillChar(Programs[i].fPar[0], sizeof(single), 0);
 end;
 SetLength(ParameterProperties, fNumParams);
 for i := 0 to fNumParams - 1 do
 begin
  ParameterProperties[i] := TVstParameterProperty.Create;
  ParameterProperties[i].min := 0;
  ParameterProperties[i].i1 := 0;
  ParameterProperties[i].i2 := 0;
  ParameterProperties[i].max := 1;
  ParameterProperties[i].curve := ctLinear;
  ParameterProperties[i].curveFactor := 1;
  ParameterProperties[i].smoothingFactor := 1;
  StrCopy(ParameterProperties[i].name, PAnsiChar('Parameter ' + inttostr(i)));
 end;

 MidiEvent.numEvents := 0;
 for i := 0 to maxMidiEvents - 1 do
 begin
  GetMem(MidiEvent.events[i], sizeof(VstMidiEvent));
  FillChar(MidiEvent.events[i]^, sizeof(VstMidiEvent), 0);
  PVstMidiEvent(MidiEvent.events[i])^.vType := 1;
  PVstMidiEvent(MidiEvent.events[i])^.byteSize := 24;
 end;

 // create the editor window
 if EditorForm <> nil then
  Editor := TVstTemplateEditor.Create(Self, EditorForm);

 Randomize;
 Suspend;

 s := fID;
 while length(s) < 4 do s := s + ' ';
 setUniqueID(FourCharToLong(s[1], s[2], s[3], s[4]));

 hasVu(prHasVu in fPluginProperties);
 isSynth(prIsSynth in fPluginProperties);
 hasClip(prHasClip in fPluginProperties);
 canMono(prCanMono in fPluginProperties);
 canProcessReplacing(prCanReplacing in fPluginProperties);
 programsAreChunks(prProgramsAreChunks in fPluginProperties);
 hasExternalBuffer(prExtHasBuffer in fPluginProperties);
 wantAsyncOperation(prIsAsync in fPluginProperties);
 noTail(prNoSoundinStop in fPluginProperties);
 setNumInputs(fNumInputs);
 setNumOutputs(fNumOutputs);
 SetInitialDelay(fInitialDelay);
 setProgram(0);
 fEditorNeedsUpdate := true;
 InitializeParameters;
end;

destructor TVstTemplate.Destroy;
var i: integer;
begin
 for i := 0 to maxMidiEvents - 1 do FreeMem(MidiEvent.events[i]);
 for i := 0 to fNumPrograms - 1 do SetLength(Programs[i].fPar, 0);
 for i := 0 to fNumPrograms - 1 do Programs[i].Free;
 for i := 0 to fNumParams - 1 do ParameterProperties[i].Free;
 SetLength(Programs, 0);
 SetLength(ParameterProperties, 0);
 inherited Destroy;
end;

procedure TVstTemplate.setProgram(aProgram: Longint);
var i: integer;
begin
 if (aProgram < 0) or (aProgram > fNumPrograms - 1) then exit;
 curProgram := aProgram;
 for i := 0 to fNumParams - 1 do
  SetParameter(i, programs[curProgram].fpar[i]);
 fEditorNeedsUpdate := true;
end;

procedure TVstTemplate.setProgramName(name:PAnsiChar);
begin
 StrCopy(programs[curProgram].name, name);
 fEditorNeedsUpdate := true;
end;

procedure TVstTemplate.getProgramName(name: PAnsiChar);
begin
 StrCopy(name, programs[curProgram].name)
end;

procedure TVstTemplate.Suspend;
begin
end;

procedure TVstTemplate.setParameter(index: Longint; value: Single);
begin
 if (value > 1) then value := 1 else
 if (value < 0) then value := 0;
 if (index >= fNumParams) or (index < 0) then exit;
 programs[curProgram].fPar[index] := value;
 fEditorNeedsUpdate := true;
 if Assigned(OnParameterChange) then OnParameterChange(Index);
end;

function TVstTemplate.getParameter(index: Longint): Single;
begin
 if (index >= fNumParams) or (index < 0) then result := 0
  else result := programs[curProgram].fPar[index];
end;

procedure TVstTemplate.process(inputs, outputs: PPSingle;
 sampleFrames: Longint);
var Ins: TArrayOfSingleArray absolute inputs;
    Outs: TArrayOfSingleArray absolute outputs;
    i, j: Integer;
begin
 fTempo := TempoAt(0) / 10000; // get current bpm tempo from host
 {$IFDEF FixedBuffer}
 for j := 0 to sampleFrames - 1 do
 begin
  for i := 0 to fNumInputs - 1 do
   InsTmp[i][sampleCounter] := Ins[i][j];
  for i := 0 to fNumOutputs - 1 do
   Outs[i][j] := Outs[i][j] + OutsTmp[i][sampleCounter];
  inc(sampleCounter);
  if sampleCounter >= fBufferSize then
  begin
   sampleCounter := 0;
   processAudio(InsTmp, OutsTmp, fBufferSize);
  end;
 end;
 {$ELSE}
 // BVE: This fragments memory! can lead no OutOfMemory errors!
 SetLength(OutsTmp, fNumOutputs, sampleFrames);
 SetLength(InsTmp, fNumInputs, sampleFrames);
 for j := 0 to fNumOutputs - 1 do
  FillChar(OutsTmp[j][0], sampleFrames, 0);
 for j := 0 to fNumInputs - 1 do
  Move(Ins[j][0], InsTmp[j][0], sampleFrames * Sizeof(Single));
 processAudio(InsTmp, OutsTmp, sampleFrames);
 for j := 0 to fNumOutputs - 1 do
  for i := 0 to sampleFrames - 1 do
   Outs[j, i] := Outs[j, i] + OutsTmp[j, i];
 {$ENDIF}
 if numMidiEvents > 0 then
 begin
  MidiEvent.numEvents := numMidiEvents;
  sendVstEventsToHost(@MidiEvent);
  numMidiEvents := 0;
 end;
end;

procedure TVstTemplate.processReplacing(inputs,
 outputs: PPSingle; sampleFrames: Longint);
var Ins: TArrayOfSingleArray absolute inputs;
    Outs: TArrayOfSingleArray absolute outputs;
    i, j: Integer;
begin
 fTempo := TempoAt(0) / 10000; // get current bpm tempo from host
 {$IFDEF FixedBuffer}
 for j := 0 to sampleFrames - 1 do
 begin
  for i := 0 to fNumInputs - 1 do
   InsTmp[i][sampleCounter] := Ins[i][j];
  for i := 0 to fNumOutputs - 1 do
   Outs[i][j] := OutsTmp[i][sampleCounter];
  inc(sampleCounter);
  if sampleCounter >= fBufferSize then
  begin
   sampleCounter := 0;
   processAudio(InsTmp, OutsTmp, fBufferSize);
  end;
 end;
 {$ELSE}
 // BVE: This fragments memory! can lead no OutOfMemory errors!
 SetLength(InsTmp, fNumInputs, sampleFrames);
 SetLength(OutsTmp, fNumOutputs, sampleFrames);
 for j := 0 to fNumOutputs - 1 do
  FillChar(OutsTmp[j][0], sampleFrames, 0);
 for j := 0 to fNumInputs - 1 do
  Move(Ins[j][0], InsTmp[j][0], sampleFrames * Sizeof(Single));
 processAudio(InsTmp, OutsTmp, sampleFrames);
 for j := 0 to fNumOutputs - 1 do
  Move(OutsTmp[j][0], Outs[j][0], sampleFrames * Sizeof(Single));
 {$ENDIF}
 if numMidiEvents > 0 then
 begin
  MidiEvent.numEvents := numMidiEvents;
  sendVstEventsToHost(@MidiEvent);
  numMidiEvents := 0;
 end;
end;

function TVstTemplate.getInputProperties(index: longint;
 properties: PVstPinProperties): boolean;
begin
 Result := false;
 if (index < fNumInputs) then
 begin
  // set name of input channel:
  StrPCopy(properties^.vLabel, 'Input #' + inttostr(index + 1));
  // activate input channel:
  properties^.flags := kVstPinIsActive;
  // make channel stereo bound (2-channel signal):
  properties^.flags := properties^.flags or kVstPinIsStereo;
  Result := true;
 end;
end;

function TVstTemplate.getOutputProperties(index: longint;
 properties: PVstPinProperties): boolean;
begin
 Result := false;
 if (index < fNumOutputs) then
 begin
  // set name of output channel:
  StrPCopy(properties^.vLabel, 'Output #' + inttostr(index + 1));
  // activate output channel:
  properties^.flags := kVstPinIsActive;
  // make channel stereo bound (2-channel signal):
  properties^.flags := properties^.flags or kVstPinIsStereo;
  Result := true;
 end;
end;

function TVstTemplate.getProgramNameIndexed(category,
 index: longint; text: PAnsiChar): boolean;
begin
 Result := false;
 if (index < fNumPrograms) then
 begin
  StrCopy(text, programs[index].name);
  Result := true;
 end;
end;

procedure TVstTemplate.getParameterName(index: longint;
 text: PAnsiChar);
begin
 if (index >= fNumParams) then
  StrPCopy(text, 'undefined')
 else
  StrCopy(text, ParameterProperties[index].name);
end;

procedure TVstTemplate.getParameterDisplay(index: longint; text: PAnsiChar);
begin
 if (index >= fNumParams) then
  StrPCopy(text, 'undefined')
 else
  float2Ansistring(getParameterEx(index), text);
end;

procedure TVstTemplate.getParameterLabel(index: longint; text: PAnsiChar);
begin
 if (index >= fNumParams) then
  StrPCopy(text, 'undefined')
 else
  StrCopy(text, ParameterProperties[index].units);
end;

function TVstTemplate.getEffectName(name: PAnsiChar): boolean;
begin
 StrPCopy(name, fEffectName);
 Result := true;
end;

function TVstTemplate.getVendorString(text: PAnsiChar): boolean;
begin
 StrPCopy(text, fVendorName);
 Result := true;
end;

function TVstTemplate.getProductString(text: PAnsiChar): boolean;
begin
 StrPCopy(text, fEffectName);
 Result := true;
end;

function TVstTemplate.getVendorVersion: longint;
begin
 Result := fVendorVersion;
end;

function TVstTemplate.processEvents(ev: PVstEvents): longint;
var i: integer;
begin
 for i := 0 to ev^.numEvents - 1 do
  if (ev^.events[i].vtype = kVstMidiType) then
   processMidi(PVstMidiEvent(ev^.events[i])^);
 Result := 1;
end;

procedure TVstTemplate.Resume;
begin
 // important for all plugins that want to receive MIDI!
 wantEvents(1);
end;

function TVstTemplate.GetPluginEditor: TVstTemplateEditor;
begin
 Result := Editor as TVstTemplateEditor;
end;

procedure TVstTemplate.MIDI_Out(b1, b2, b3, b4: byte;
 offset: integer);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := b1;
  midiData[1] := b2;
  midiData[2] := b3;
  midiData[3] := b4;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_CC(ch, num, val: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $B0 + ch;
  midiData[1] := num;
  midiData[2] := val;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_ChannelAftertouch(ch, val: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $D0 + ch;
  midiData[1] := val;
  midiData[2] := 0;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_NoteOff(ch, note, val: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $80 + ch;
  midiData[1] := note;
  midiData[2] := val;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_NoteOn(ch, note, val: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $90 + ch;
  midiData[1] := note;
  midiData[2] := val;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_PitchBend(ch, val: integer;
 offset: integer = 0);
var a, b: integer;
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  a := (val div 128) + 64;
  b := (val div 128);
  b := val - b * 128;
  midiData[0] := $E0 + ch;
  midiData[1] := b;
  midiData[2] := a;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_PitchBend2(ch, x1, x2: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $E0 + ch;
  midiData[1] := x1;
  midiData[2] := x2;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_PolyAftertouch(ch, note,
 val: integer; offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $A0 + ch;
  midiData[1] := note;
  midiData[2] := val;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.MIDI_ProgramChange(ch, val: integer;
 offset: integer = 0);
begin
 with PVstMidiEvent(MidiEvent.events[numMidiEvents])^ do
 begin
  midiData[0] := $D0 + ch;
  midiData[1] := val;
  midiData[2] := 0;
  deltaframes := offset;
  if numMidiEvents < maxMidiEvents - 1 then inc(numMidiEvents);
 end;
end;

procedure TVstTemplate.processMIDI(ev: VstMidiEvent);
begin
end;

function TVstTemplate.GetCurProgram: integer;
begin
 result := CurProgram;
end;

procedure TVstTemplate.processAudio(const inputs,
 outputs: TArrayOfSingleArray; sampleframes: Integer);
begin
end;

// TVstTemplateEditor

constructor TVstTemplateEditor.Create(effect: AudioEffect;
 EditorForm: TEditorForm);
begin
 inherited Create(effect);
 EdForm := EditorForm;
 useCount := 0;
end;

destructor TVstTemplateEditor.Destroy;
begin
 if assigned(Editor) then
 begin
  Editor.Free;
  Editor := nil;
  systemWindow := 0;
 end;
 inherited Destroy;
end;

function TVstTemplateEditor.getRect(var rect: PERect): longint;
begin
 r.top := 0;
 r.left := 0;
 r.bottom := 602;
 r.right := 271;
 if assigned(Editor) then
 begin
  r.bottom := Editor.ClientHeight;
  r.right := Editor.ClientWidth;
 end;
 rect := @r;
 Result := 1;
end;

function TVstTemplateEditor.Open(ptr: Pointer): Longint;
begin
 systemWindow := HWnd(ptr);
 Inc(useCount);
 if (useCount = 1) or (not assigned(Editor)) then
 begin
  Editor := EdForm.CreateParented(systemWindow);
  Editor.SetBounds(0, 0, Editor.Width, Editor.Height);
  Editor.Visible := True;
  PostMessage(Editor.Handle, WM_EDITOROPEN, integer(fEffect), 0);
 end;
 Result := 1;
end;

procedure TVstTemplateEditor.Close;
begin
 dec(useCount);
 if useCount <= 0 then
 begin
  useCount := 0;
  Editor.Free;
  Editor := nil;
  systemWindow := 0;
 end;
end;

procedure TVstTemplateEditor.Idle;
begin
 if not Assigned(Editor) then exit;
 if Assigned(Effect) then
  if Assigned(TVstTemplate(Effect).OnEditorIdle) then
   TVstTemplate(Effect).OnEditorIdle(self.Editor);
end;

procedure TVstTemplateEditor.Update;
begin
 if not Assigned(Editor) then exit;
end;

procedure TVstTemplate.InitializeParameters;
begin
 //
end;

function TVstTemplate.getParameterEx(index: integer): single;
var s: single;
begin
 s := getParameter(index);
 case ParameterProperties[index].curve of
 ctLogarithmic: s := exp(s * ln(ParameterProperties[index].curveFactor + 1)) - 1;
 ctExponential: s := ln(ParameterProperties[index].curveFactor * s + 1) / ln(ParameterProperties[index].curveFactor + 1);
 else
 end;
 Result :=
  ParameterProperties[index].Smooth(s * (ParameterProperties[index].max - ParameterProperties[index].min)
   + ParameterProperties[index].min);
end;

procedure TVstTemplate.setParameterEx(index: integer;
 const Value: single);
var s: single;
begin
 s := (value - ParameterProperties[index].min) / (ParameterProperties[index].max - ParameterProperties[index].min);
 case ParameterProperties[index].curve of
 ctLogarithmic: s := ln(ParameterProperties[index].curveFactor * s + 1) / ln(ParameterProperties[index].curveFactor + 1);
 ctExponential: s := exp(s * ln(ParameterProperties[index].curveFactor + 1)) - 1;
 else
 end;
 setParameterAutomated(index, s);
 if Assigned(OnParameterChange) then OnParameterChange(Index);
end;

procedure TVstTemplate.setfNumInputs(const Value: integer);
begin
 fNumInputs := Value;
 SetNumInputs(Value);
 ioChanged;
end;

procedure TVstTemplate.setfNumOutputs(const Value: integer);
begin
 fNumOutputs := Value;
 SetNumOutputs(fNumOutputs);
 ioChanged;
end;

procedure TVstTemplate.SetfInitialDelay(const Value: integer);
begin
 fInitialDelay := Value;
 SetInitialDelay(fInitialDelay);
 ioChanged;
end;

procedure TVstTemplate.SetBufferSize(const Value: integer);
begin
 fBufferSize := Value;
 SetLength(InsTmp, fNumInputs, fBufferSize);
 SetLength(OutsTmp, fNumOutputs, fBufferSize);
end;

{ TVstParameterProperty }

function TVstParameterProperty.Smooth(i: single): single;
begin
 i1 := i1 + SmoothingFactor * (i - i1);
 i2 := i2 + SmoothingFactor * (i1 - i2);
 Result := i2;
end;

end.



