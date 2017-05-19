unit UnitAppSettingsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  UnitAppSettings;

type
  TfrmAppSettings = class(TForm)
    frameAppSettings: TframeAppSettings;
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAppSettings: TfrmAppSettings;

implementation
uses
  UnitUtils;

{$R *.fmx}

procedure TfrmAppSettings.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
var path, filename : string;
begin
  case Key of
  vkF1 :
    begin
      path := ExtractFilePath(GetModuleName(0));
      filename := 'QuickReference.txt';

      TUtils.Open(path+filename);
    end;
  end;
end;

end.
