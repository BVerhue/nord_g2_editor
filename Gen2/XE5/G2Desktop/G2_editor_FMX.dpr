program G2_editor_FMX;

{$Include Capabilities.inc}

uses
  System.SysUtils,
  FMX.Forms,
  {$IFDEF MSWINDOWS}
  FMX.Canvas.GDIP,
  {$ENDIF }
  FMX.Types,
  FMX.Dialogs,
  UnitG2EditorFMX_FM in 'UnitG2EditorFMX_FM.pas' {frmEditorMain},
  BVE.NMG2Types in '..\..\Common\BVE.NMG2Types.pas',
  BVE.NMG2File in '..\..\Common\BVE.NMG2File.pas',
  BVE.NMG2Mess in '..\..\Common\BVE.NMG2Mess.pas',
  BVE.NMG2USB in '..\..\Common\BVE.NMG2USB.pas',
  {$IF Defined(TCPIPNETWORK)}
  BVE.NMG2TCPIP in '..\..\Common\BVE.NMG2TCPIP.pas',
  {$ENDIF }
  BVE.NMG2GraphFMX in '..\..\Common\BVE.NMG2GraphFMX.pas',
  BVE.NMG2ControlsFMX in '..\..\Common\BVE.NMG2ControlsFMX.pas',
  BVE.NMG2Pathdata in '..\..\Common\BVE.NMG2Pathdata.pas',
  BVE.NMG2AppSettings in '..\..\Common\BVE.NMG2AppSettings.pas',
  BVE.NMG2SymbolDef in '..\..\Common\BVE.NMG2SymbolDef.pas',
  BVE.NMG2Data in '..\..\Common\BVE.NMG2Data.pas',
  UnitUtils in '..\Frames\UnitUtils.pas',
  UnitParam in '..\Frames\UnitParam.pas' {frameParam: TFrame},
  UnitModule in '..\Frames\UnitModule.pas' {frameModule: TFrame},
  UnitKnobs in '..\Frames\UnitKnobs.pas' {frameKnobs: TFrame},
  UnitSynthSettings in '..\Frames\UnitSynthSettings.pas' {frameSynthSettings: TFrame},
  UnitAddModule in '..\Frames\UnitAddModule.pas' {FrameAddModule: TFrame},
  UnitBanks in '..\Frames\UnitBanks.pas' {frameBanks: TFrame},
  UnitMessageDlg in '..\Frames\UnitMessageDlg.pas' {frameMessageDlg: TFrame},
  UnitPianoRoll in '..\Frames\UnitPianoRoll.pas' {framePianoRoll: TFrame},
  UnitAppSynthSettings in '..\Frames\UnitAppSynthSettings.pas' {frameAppSynthSettings: TFrame},
  UnitAddCable in '..\Frames\UnitAddCable.pas' {frameAddCable: TFrame},
  UnitLog in '..\Frames\UnitLog.pas' {frameLog: TFrame},
  UnitPatch in '..\Frames\UnitPatch.pas' {framePatch: TFrame},
  BVE.NMG2ColorScheme in '..\..\Common\BVE.NMG2ColorScheme.pas',
  UnitSlot in '..\Frames\UnitSlot.pas' {frameSlot: TFrame},
  UnitEditorCentral2 in '..\Frames\UnitEditorCentral2.pas' {frameEditorCentralStrip: TFrame},
  UnitPatchSettings in '..\Frames\UnitPatchSettings.pas' {framePatchSettings: TFrame},
  UnitAppSettingsForm in '..\Forms\UnitAppSettingsForm.pas' {frmAppSettings},
  UnitBanksForm in '..\Forms\UnitBanksForm.pas' {frmBanks},
  UnitKeyboardForm in '..\Forms\UnitKeyboardForm.pas' {frmKeyboard},
  UnitKnobsForm in '..\Forms\UnitKnobsForm.pas' {frmKnobs},
  UnitPatchSettingsForm in '..\Forms\UnitPatchSettingsForm.pas' {frmPatchSettings},
  UnitSynthSettingsForm in '..\Forms\UnitSynthSettingsForm.pas' {frmSynthSettings},
  UnitAppSettings in '..\Frames\UnitAppSettings.pas' {frameAppSettings: TFrame},
  UnitSlotStrip in '..\Frames\UnitSlotStrip.pas' {frameSlotStrip: TFrame};

{$R *.res}

var
  path : string;

begin
{$IFDEF MSWINDOWS}
  // Use this on XP
  //GlobalUseDirect2D := False;
  GlobalUseDirect2D := true;
{$ELSE}
  //GlobalUseDirect2D := true;
{$ENDIF}

  GlobalUseHWEffects := false;
  {GlobalUseHWEffects := true;
  GlobalUseDirect2D := False;
  GlobalUseDirect2DSoftware := False;
  GlobalDisableFocusEffect := True;}

  //GlobalUseGPUCanvas := True;

  //path := ExtractFilePath(GetModuleName(0));
  //PathDataList.LoadFromFile(path + 'symbols.svg');

  Application.Initialize;
  Application.CreateForm(TfrmEditorMain, frmEditorMain);
  Application.Run;
end.
