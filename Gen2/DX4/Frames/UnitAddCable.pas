unit UnitAddCable;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  BVE.NMG2ControlsFMX, FMX.Objects, BVE.NMG2GraphFMX, BVE.NMG2File;

type
  TframeAddCable = class(TFrame)
    rCableDialog: TRectangle;
    btCancel: TG2BtnText;
    rbDeleteCable: TG2BtnRadio;
    G2Label1: TG2Label;
    procedure btCancelChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbDeleteCableButtonClick(Sender: TObject; const aIndex: Integer);
  private
    [Weak] FConnector : TG2GraphConnectorFMX;

    FOnClose : TNotifyEvent;
    procedure SetConnector(const Value: TG2GraphConnectorFMX);
  public
    procedure UpdateControls;

    property Connector : TG2GraphConnectorFMX read FConnector write SetConnector;
    property OnClose : TNotifyEvent read FOnClose write FOnCLose;
  end;

implementation

{$R *.fmx}

procedure TframeAddCable.btCancelChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 1 then begin
    if assigned(FOnClose) then
      FOnClose(self)
    else
      Visible := False;
  end;
end;

procedure TframeAddCable.rbDeleteCableButtonClick(Sender: TObject;
  const aIndex: Integer);
var Patch : TG2GraphPatchFMX;
begin
  if assigned(FConnector) then begin
    Patch := FConnector.Module.PatchPart.Patch as TG2GraphPatchFMX;
    Patch.MessDeleteConnection( FConnector.Location, FConnector.Cables[(Sender as TG2BtnRadio).Value]);
  end;

  if assigned(FOnClose) then
    FOnClose(self)
  else
    Visible := False;
end;

procedure TframeAddCable.SetConnector(const Value: TG2GraphConnectorFMX);
begin
  FConnector := Value;
  UpdateControls;
end;

procedure TframeAddCable.UpdateControls;
var i : integer;
    Module : TG2FileModule;
    MaxWidth : single;
begin
  if assigned(FConnector) then begin
    if FConnector.CableCount > 0 then begin
      MaxWidth := Width - rbDeleteCable.Position.X - 75;

      rbDeleteCable.ButtonText.Clear;
      rbDeleteCable.ButtonCount := FConnector.CableCount;
      rbDeleteCable.Value := -1;
      for i := 0 to FConnector.CableCount - 1 do begin
        if FConnector.Cables[i].FromConnector = FConnector then
          Module := FConnector.Cables[i].ToConnector.Module
        else
          Module := FConnector.Module;;

        rbDeleteCable.ButtonText.Add(Module.ModuleName);
      end;

      if FConnector.CableCount * 65 > MaxWidth then
        rbDeleteCable.UnscaledWidth := MaxWidth
      else
        rbDeleteCable.UnscaledWidth := FConnector.CableCount * 65;

      rbDeleteCable.Visible := True;
    end else
      rbDeleteCable.Visible := False;
  end else begin
    rbDeleteCable.Visible := False;
  end;
end;

end.
