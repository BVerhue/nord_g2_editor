unit UnitPatchSettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UnitPatchSettings;

type
  TfrmPatchSettings = class(TForm)
    framePatchSettings: TframePatchSettings;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPatchSettings: TfrmPatchSettings;

implementation

{$R *.fmx}

end.
