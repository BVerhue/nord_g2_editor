unit UnitBanksForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UnitBanks;

type
  TfrmBanks = class(TForm)
    frameBanks: TframeBanks;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBanks: TfrmBanks;

implementation

{$R *.fmx}

end.
