unit UnitPerfSettings;

//  ////////////////////////////////////////////////////////////////////////////
//
//  Copyright (C) 2011 Bruno Verhue
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
//  ////////////////////////////////////////////////////////////////////////////

{$I ..\..\Source\Common\Includes\delphi_version.inc}

interface

uses
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Classes, System.Variants,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Controls, VCL.Graphics, VCL.Forms,
  VCL.Dialogs,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
{$ENDIF}
  g2_types, g2_classes, JawsCtrls;

type
  TfrmPerfSettings = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    rbStop: TRadioButton;
    rbRun: TRadioButton;
    cbEnableA: TCheckBox;
    cbEnableB: TCheckBox;
    cbEnableC: TCheckBox;
    cbEnableD: TCheckBox;
    cbKeyboardA: TCheckBox;
    cbKeyboardB: TCheckBox;
    cbKeyboardC: TCheckBox;
    cbKeyboardD: TCheckBox;
    cbHoldA: TCheckBox;
    cbHoldB: TCheckBox;
    cbHoldC: TCheckBox;
    cbHoldD: TCheckBox;
    cbKeyboardRange: TCheckBox;
    StaticText1: TStaticText;
    ePerfName: DEdit;
    udRate: DUpDown;
    eRate: DEdit;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    eLowerA: DEdit;
    eLowerB: DEdit;
    eLowerC: DEdit;
    eLowerD: DEdit;
    eUpperA: DEdit;
    eUpperB: DEdit;
    eUpperC: DEdit;
    eUpperD: DEdit;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    StaticText9: TStaticText;
    StaticText10: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure ePerfNameExit(Sender: TObject);
    procedure PerfChange(Sender: TObject);
    procedure udRateClick(Sender: TObject; Button: TUDBtnType);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure udRateChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: SmallInt; Direction: TUpDownDirection);
  private
    { Private declarations }
    FDisableControls : boolean;
  public
    { Public declarations }
    procedure updateDialog;
  end;

var
  frmPerfSettings: TfrmPerfSettings;

implementation

uses UnitG2Editor;

{$R *.dfm}

{ TfrmPerfSettings }

procedure TfrmPerfSettings.ePerfNameExit(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  G2.Performance.PerformanceName := AnsiString(ePerfName.Text);
  G2.Performance.SendSetPerfNameMessage( G2.Performance.PerformanceName);
end;

procedure TfrmPerfSettings.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmPerfSettings.FormShow(Sender: TObject);
begin
  updateDialog;
end;

procedure TfrmPerfSettings.PerfChange(Sender: TObject);
var G2 : TG2;
begin
  if FDisableControls then
    exit;

  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  G2.Performance.MasterClock := StrToInt(eRate.Text);

  if rbRun.Checked then
    G2.Performance.MasterClockRun := 1
  else
    G2.Performance.MasterClockRun := 0;

  G2.Performance.KeyboardRangeEnabled := BoolToByte(cbKeyBoardRange.Checked);

  G2.Performance.SlotA.Enabled := BoolToByte(cbEnableA.Checked);
  G2.Performance.SlotA.Hold := BoolToByte(cbHoldA.Checked);
  G2.Performance.SlotA.Keyboard := BoolToByte(cbKeyboardA.Checked);
  G2.Performance.SlotA.Upper := StrToInt(eUpperA.Text);
  G2.Performance.SlotA.Lower := StrToInt(eLowerA.Text);

  G2.Performance.SlotB.Enabled := BoolToByte(cbEnableB.Checked);
  G2.Performance.SlotB.Hold := BoolToByte(cbHoldB.Checked);
  G2.Performance.SlotB.Keyboard := BoolToByte(cbKeyboardB.Checked);
  G2.Performance.SlotB.Upper := StrToInt(eUpperB.Text);
  G2.Performance.SlotB.Lower := StrToInt(eLowerB.Text);

  G2.Performance.SlotC.Enabled := BoolToByte(cbEnableC.Checked);
  G2.Performance.SlotC.Hold := BoolToByte(cbHoldC.Checked);
  G2.Performance.SlotC.Keyboard := BoolToByte(cbKeyboardC.Checked);
  G2.Performance.SlotC.Upper := StrToInt(eUpperC.Text);
  G2.Performance.SlotC.Lower := StrToInt(eLowerC.Text);

  G2.Performance.SlotD.Enabled := BoolToByte(cbEnableD.Checked);
  G2.Performance.SlotD.Hold := BoolToByte(cbHoldD.Checked);
  G2.Performance.SlotD.Keyboard := BoolToByte(cbKeyboardD.Checked);
  G2.Performance.SlotD.Upper := StrToInt(eUpperD.Text);
  G2.Performance.SlotD.Lower := StrToInt(eLowerD.Text);

  G2.Performance.SendSetPerfSettingsMessage;
end;

procedure TfrmPerfSettings.udRateChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: SmallInt; Direction: TUpDownDirection);
begin
  if FDisableControls or (NewValue < 30) or (NewValue > 240) then begin
    AllowChange := False;
    exit;
  end;

  AllowChange := True;
  eRate.Text := IntToStr(NewValue);
  PerfChange(self);
end;

procedure TfrmPerfSettings.udRateClick(Sender: TObject; Button: TUDBtnType);
begin
//  eRate.Text := IntToStr(udRate.Position);
//  PerfChange(self);
end;

procedure TfrmPerfSettings.updateDialog;
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  FDisableControls := True;
  try
    ePerfName.Text := string(G2.Performance.PerformanceName);

    eRate.Text := IntToStr(G2.Performance.MasterClock);
    udRate.Position := G2.Performance.MasterClock;
    if G2.Performance.MasterClockRun = 1 then
      rbRun.Checked := True
    else
      rbStop.Checked := True;
    cbKeyboardRange.Checked := G2.Performance.KeyboardRangeEnabled = 1;

    cbEnableA.Checked   := G2.Performance.Slot[0].Enabled = 1;
    cbHoldA.Checked     := G2.Performance.Slot[0].Hold = 1;
    cbKeyboardA.Checked := G2.Performance.Slot[0].Keyboard = 1;
    eLowerA.Text        := IntToStr(G2.Performance.Slot[0].Lower);
    eUpperA.Text        := IntToStr(G2.Performance.Slot[0].Upper);
    eLowerA.Enabled     := cbKeyboardRange.Checked;
    eUpperA.Enabled     := cbKeyboardRange.Checked;

    cbEnableB.Checked   := G2.Performance.Slot[1].Enabled = 1;
    cbHoldB.Checked     := G2.Performance.Slot[1].Hold = 1;
    cbKeyboardB.Checked := G2.Performance.Slot[1].Keyboard = 1;
    eLowerB.Text        := IntToStr(G2.Performance.Slot[1].Lower);
    eUpperB.Text        := IntToStr(G2.Performance.Slot[1].Upper);
    eLowerB.Enabled     := cbKeyboardRange.Checked;
    eUpperB.Enabled     := cbKeyboardRange.Checked;

    cbEnableC.Checked   := G2.Performance.Slot[2].Enabled = 1;
    cbHoldC.Checked     := G2.Performance.Slot[2].Hold = 1;
    cbKeyboardC.Checked := G2.Performance.Slot[2].Keyboard = 1;
    eLowerC.Text        := IntToStr(G2.Performance.Slot[2].Lower);
    eUpperC.Text        := IntToStr(G2.Performance.Slot[2].Upper);
    eLowerC.Enabled     := cbKeyboardRange.Checked;
    eUpperC.Enabled     := cbKeyboardRange.Checked;

    cbEnableD.Checked   := G2.Performance.Slot[3].Enabled = 1;
    cbHoldD.Checked     := G2.Performance.Slot[3].Hold = 1;
    cbKeyboardD.Checked := G2.Performance.Slot[3].Keyboard = 1;
    eLowerD.Text        := IntToStr(G2.Performance.Slot[3].Lower);
    eUpperD.Text        := IntToStr(G2.Performance.Slot[3].Upper);
    eLowerD.Enabled     := cbKeyboardRange.Checked;
    eUpperD.Enabled     := cbKeyboardRange.Checked;

  finally
    FDisableControls := False;
  end;
end;

end.
