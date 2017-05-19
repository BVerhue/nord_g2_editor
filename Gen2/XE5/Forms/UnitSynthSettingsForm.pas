unit UnitSynthSettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UnitSynthSettings;

type
  TfrmSynthSettings = class(TForm)
    frameSynthSettings: TframeSynthSettings;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSynthSettings: TfrmSynthSettings;

implementation

{$R *.fmx}

end.
