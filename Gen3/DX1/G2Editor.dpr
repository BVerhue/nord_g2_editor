program G2Editor;

uses
  System.StartUpCopy,
  FMX.Forms,
  BVE.NMG2ControlsGL in '..\Common\BVE.NMG2ControlsGL.pas',
  BVE.NMG2TexturesGL in '..\Common\BVE.NMG2TexturesGL.pas',
  BVE.NMG2CablePhysics in '..\Common\BVE.NMG2CablePhysics.pas',
  BVE.NMG2PatchSettings in '..\Common\BVE.NMG2PatchSettings.pas',
  BVE.NMG2Classes in '..\Common\BVE.NMG2Classes.pas',
  BVE.NMG2Types in '..\Common\BVE.NMG2Types.pas',
  BVE.NMG2USB in '..\Common\BVE.NMG2USB.pas',
  {$IFDEF Android}
  Androidapi.JNI.USB in '..\Common\Androidapi.JNI.USB.pas',
  {$ENDIF }
  {$IFDEF MSWINDOWS}
  LibUSBWinDyn in '..\Common\LibUSBWinDyn.pas',
  BVE.NMG2USBWin in '..\Common\BVE.NMG2USBWin.pas',
  {$ENDIF }
  BVE.NMG2Cable in '..\Common\BVE.NMG2Cable.pas',
  BVE.NMG2Connector in '..\Common\BVE.NMG2Connector.pas',
  BVE.NMG2Data in '..\Common\BVE.NMG2Data.pas',
  BVE.NMG2Knob in '..\Common\BVE.NMG2Knob.pas',
  BVE.NMG2Controller in '..\Common\BVE.NMG2Controller.pas',
  BVE.NMG2Stream in '..\Common\BVE.NMG2Stream.pas',
  BVE.NMG2Synth in '..\Common\BVE.NMG2Synth.pas',
  BVE.NMG2Com in '..\Common\BVE.NMG2Com.pas',
  BVE.NMG2ComUSB in '..\Common\BVE.NMG2ComUSB.pas',
  BVE.NMG2ComManager in '..\Common\BVE.NMG2ComManager.pas',
  BVE.NMG2Led in '..\Common\BVE.NMG2Led.pas',
  BVE.NMG2Param in '..\Common\BVE.NMG2Param.pas',
  BVE.NMG2FileIntf in '..\Common\BVE.NMG2FileIntf.pas',
  BVE.NMG2Slot in '..\Common\BVE.NMG2Slot.pas',
  BVE.NMG2Function in '..\Common\BVE.NMG2Function.pas',
  BVE.NMG2Patch in '..\Common\BVE.NMG2Patch.pas',
  BVE.NMG2PatchPart in '..\Common\BVE.NMG2PatchPart.pas',
  BVE.NMG2Module in '..\Common\BVE.NMG2Module.pas',
  BVE.NMG2Object in '..\Common\BVE.NMG2Object.pas',
  BVE.NMG2Perf in '..\Common\BVE.NMG2Perf.pas',
  BVE.NMG2AppSettings in '..\Common\BVE.NMG2AppSettings.pas',
  BVE.NMG2Controls in '..\Common\BVE.NMG2Controls.pas',
  BVE.NMG2Pathdata in '..\Common\BVE.NMG2Pathdata.pas',
  UnitPatch in '..\UI\UnitPatch.pas',
  UnitBanks in '..\UI\UnitBanks.pas',
  UnitFormPerf in 'Forms\UnitFormPerf.pas' {Form1},
  UnitSelectModule in '..\UI\UnitSelectModule.pas',
  UnitFormSlot in 'Forms\UnitFormSlot.pas' {frmSlot},
  BVE.NMG2ControlsG2 in '..\Common\BVE.NMG2ControlsG2.pas',
  BVE.NMG2GraphTypes in '..\Common\BVE.NMG2GraphTypes.pas',
  UnitPerfSlot in '..\UI\UnitPerfSlot.pas',
  UnitKeyboard in '..\UI\UnitKeyboard.pas',
  UnitFormBanks in 'Forms\UnitFormBanks.pas' {frmBanks},
  UnitFormAddModule in 'Forms\UnitFormAddModule.pas' {frmAddModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
