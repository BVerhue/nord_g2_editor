unit UnitLog;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, BVE.NMG2ControlsFMX, BVE.NMG2File;

type
  TframeLog = class(TFrame)
    Panel1: TPanel;
    mLog: TMemo;
    G2BtnText1: TG2BtnText;
    G2BtnText2: TG2BtnText;
    procedure G2BtnText1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure G2BtnText2ChangeValue(Sender: TObject; const aValue: Integer);
  private
    FSynth : TG2File;
  public
    property Synth : TG2File read FSynth write FSynth;
  end;

implementation

{$R *.fmx}

procedure TframeLog.G2BtnText1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then begin
    FSynth.AssignLog( mLog.Lines);
  end;

end;

procedure TframeLog.G2BtnText2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    FSynth.ClearLog;

end;

end.
