unit UnitKnobsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UnitKnobs;

type
  TfrmKnobs = class(TForm)
    frameKnobs: TframeKnobs;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKnobs: TfrmKnobs;

implementation

{$R *.fmx}

end.
