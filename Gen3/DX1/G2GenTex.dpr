program G2GenTex;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFormGenTex in 'Forms\UnitFormGenTex.pas' {Form1},
  BVE.NMG2ControlsG2 in '..\Common\BVE.NMG2ControlsG2.pas',
  BVE.NMG2Classes in '..\Common\BVE.NMG2Classes.pas',
  BVE.NMG2Controls in '..\Common\BVE.NMG2Controls.pas',
  BVE.NMG2ControlsGL in '..\Common\BVE.NMG2ControlsGL.pas',
  BVE.NMG2FileIntf in '..\Common\BVE.NMG2FileIntf.pas',
  BVE.NMG2GraphTypes in '..\Common\BVE.NMG2GraphTypes.pas',
  BVE.NMG2TexturesGL in '..\Common\BVE.NMG2TexturesGL.pas',
  BVE.NMG2Types in '..\Common\BVE.NMG2Types.pas',
  BVE.NMG2Pathdata in '..\Common\BVE.NMG2Pathdata.pas',
  BVE.NMG2CablePhysics in '..\Common\BVE.NMG2CablePhysics.pas',
  BVE.NMG2Cable in '..\Common\BVE.NMG2Cable.pas',
  BVE.NMG2Data in '..\Common\BVE.NMG2Data.pas',
  BVE.NMG2Module in '..\Common\BVE.NMG2Module.pas',
  BVE.NMG2Param in '..\Common\BVE.NMG2Param.pas',
  BVE.NMG2Object in '..\Common\BVE.NMG2Object.pas',
  BVE.NMG2Connector in '..\Common\BVE.NMG2Connector.pas',
  BVE.NMG2Function in '..\Common\BVE.NMG2Function.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
