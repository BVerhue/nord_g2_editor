program G2_editor;

uses
  Forms,
  g2_database in '..\..\Source\Common\g2_database.pas',
  g2_types in '..\..\Source\Common\g2_types.pas',
  g2_file in '..\..\Source\Common\g2_file.pas',
  g2_mess in '..\..\Source\Common\g2_mess.pas',
  g2_usb in '..\..\Source\Common\g2_usb.pas',
  g2_midi in '..\..\Source\Common\g2_midi.pas',
  g2_graph in '..\..\Source\Common\g2_graph.pas',
  g2_classes in '..\..\Source\Common\g2_classes.pas',
  OSCUtils in '..\..\Source\Common\OSCUtils.pas',
  UnitG2Editor in '..\..\Source\Windows\UnitG2Editor.pas' {frmG2Main},
  UnitPatchSettings in '..\..\Source\Windows\UnitPatchSettings.pas' {frmPatchSettings},
  UnitLog in '..\..\Source\Windows\UnitLog.pas' {frmLog},
  UnitParameterPages in '..\..\Source\Windows\UnitParameterPages.pas' {frmParameterPages},
  UnitSeqGrid in '..\..\Source\Windows\UnitSeqGrid.pas' {frmSeqGrid},
  UnitSynthSettings in '..\..\Source\Windows\UnitSynthSettings.pas' {frmSynthSettings},
  UnitPerfSettings in '..\..\Source\Windows\UnitPerfSettings.pas' {frmPerfSettings},
  UnitEditLabel in '..\..\Source\Windows\UnitEditLabel.pas' {frmEditLabel},
  UnitSettings in '..\..\Source\Windows\UnitSettings.pas' {frmSettings},
  UnitEditorTools in '..\..\Source\Windows\UnitEditorTools.pas' {frmEditorTools},
  UnitPatchBrowser in '..\..\Source\Windows\UnitPatchBrowser.pas' {frmPatchBrowser},
  UnitModuleDef in '..\..\Source\Windows\UnitModuleDef.pas' {frmModuleDef},
  UnitPatchNotes in '..\..\Source\Windows\UnitPatchNotes.pas' {frmPatchNotes},
  UnitMidiMapping in '..\..\Source\Windows\UnitMidiMapping.pas' {frmMidiMapping},
  UnitPatchManager in '..\..\Source\Windows\UnitPatchManager.pas' {frmPatchManager},
  UnitPatchBrowserFilterModules in '..\..\Source\Windows\UnitPatchBrowserFilterModules.pas' {frmPatchBrowserModuleFilter},
  dom in '..\..\Source\Third_party_code\FPC_XML\Source\dom.pas',
  uriparser in '..\..\Source\Third_party_code\FPC_XML\Source\uriparser.pas',
  xmlread in '..\..\Source\Third_party_code\FPC_XML\Source\xmlread.pas',
  xmlutils in '..\..\Source\Third_party_code\FPC_XML\Source\xmlutils.pas',
  xmlwrite in '..\..\Source\Third_party_code\FPC_XML\Source\xmlwrite.pas',
  LibUSBWinDyn in '..\..\Source\Common\LibUSBWinDyn.pas',
  MidiIn in '..\..\Source\Third_party_code\MIDIIO\Source\MidiIn.pas',
  MidiOut in '..\..\Source\Third_party_code\MIDIIO\Source\MidiOut.pas',
  MidiType in '..\..\Source\Third_party_code\MIDIIO\Source\MidiType.pas',
  CircBuf in '..\..\Source\Third_party_code\MIDIIO\Source\CircBuf.pas',
  MidiDefs in '..\..\Source\Third_party_code\MIDIIO\Source\MidiDefs.pas',
  MidiCallback in '..\..\Source\Third_party_code\MIDIIO\Source\MidiCallback.pas',
  MidiCons in '..\..\Source\Third_party_code\MIDIIO\Source\MidiCons.pas',
  MidiKeyPatchArray in '..\..\Source\Third_party_code\MIDIIO\Source\MidiKeyPatchArray.pas',
  graph_util_vcl in '..\..\Source\Common\graph_util_vcl.pas',
  fastbitmap in '..\..\Source\Common\fastbitmap.pas',
  JawsCtrls in '..\..\Source\Common\JawsCtrls.pas',
  MusicalKeyboard in '..\..\Source\Common\MusicalKeyboard.pas',
  UnitPatchBuffer in '..\..\Source\Windows\UnitPatchBuffer.pas' {frmPatchBuffer},
  Sidepanel in '..\..\Source\Common\Sidepanel.pas';

{$R *.res}

begin
  Application.Initialize;
{$IFDEF FPC}
{$ELSE}
  Application.MainFormOnTaskbar := True;
{$ENDIF}
  Application.CreateForm(TfrmG2Main, frmG2Main);
  Application.CreateForm(TfrmPatchSettings, frmPatchSettings);
  Application.CreateForm(TfrmLog, frmLog);
  Application.CreateForm(TfrmParameterPages, frmParameterPages);
  Application.CreateForm(TfrmSeqGrid, frmSeqGrid);
  Application.CreateForm(TfrmSynthSettings, frmSynthSettings);
  Application.CreateForm(TfrmPerfSettings, frmPerfSettings);
  Application.CreateForm(TfrmEditLabel, frmEditLabel);
  Application.CreateForm(TfrmEditorTools, frmEditorTools);
  Application.CreateForm(TfrmPatchBrowserModuleFilter, frmPatchBrowserModuleFilter);
  Application.CreateForm(TfrmPatchBrowser, frmPatchBrowser);
  Application.CreateForm(TfrmPatchManager, frmPatchManager);
  Application.CreateForm(TfrmPatchNotes, frmPatchNotes);
  Application.CreateForm(TfrmMidiMapping, frmMidiMapping);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmModuleDef, frmModuleDef);
  Application.CreateForm(TfrmPatchBuffer, frmPatchBuffer);
  frmG2Main.StartupTimer.Enabled := True;
  Application.Run;
end.
