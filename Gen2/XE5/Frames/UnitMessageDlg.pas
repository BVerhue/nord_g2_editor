unit UnitMessageDlg;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, FMX.Layouts;

type
  TframeMessageDlg = class(TFrame)
    lbMessage: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Rectangle1: TRectangle;
    Layout1: TLayout;
  private
    function GetLabelMessage: string;
    procedure SetLabelMessage(const Value: string);
  public
    property LabelMessage : string read GetLabelMessage write SetLabelMessage;
  end;

implementation

{$R *.fmx}

{ TframeMessageDlg }

function TframeMessageDlg.GetLabelMessage: string;
begin
  Result := lbMessage.Text;
end;

procedure TframeMessageDlg.SetLabelMessage(const Value: string);
begin
  lbMessage.Text := Value;
  lbMessage.Repaint;
end;

end.
