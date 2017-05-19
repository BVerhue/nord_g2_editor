unit UnitKeyboardForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BVE.NMG2ControlsFMX;

type
  TfrmKeyboard = class(TForm)
    G2Keyboard: TG2Keyboard;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKeyboard: TfrmKeyboard;

implementation

{$R *.fmx}

end.
