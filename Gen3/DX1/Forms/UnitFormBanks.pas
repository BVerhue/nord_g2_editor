unit UnitFormBanks;

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

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2FileIntf,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2,
  BVE.NMG2TexturesGL,
  UnitBanks;

type
  TPanelBanks = class(TG2PanelGL)
  private
    FlbBanks: TLabelGL;
    FBtnRetrieve: TBtnTextGL;
    FBtnStore: TBtnTextGL;
    FBtnClear: TBtnTextGL;
    FlbFile: TLabelGL;
    FBtnLoad: TBtnTextGL;
    FBtnSave: TBtnTextGL;
    FOnRestore: TNotifyEvent;
    procedure BtnRetrieveClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BtnStoreClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BtnClearClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BtnLoadClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BtnSaveClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
    procedure UpdateControls;

    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
  end;

  TfrmBanks = class(TG2FormGL)
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBanksCtrl: TBanksGL;
    FPanelBanks: TPanelBanks;
    procedure Restore(Sender: TObject);
  protected
    procedure SetConnectionManager(const Value: IG2ConnectionManager); override;
    procedure SetConnectionIndex(const Value: integer); override;
  public
    procedure LoadFileStream(aFilename: string);
  end;

var
  frmBanks: TfrmBanks;

implementation

{$R *.fmx}

{ TPanelBanks }

procedure TPanelBanks.BtnClearClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
end;

procedure TPanelBanks.BtnLoadClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
{$IFDEF ANDROID}
var
  Path: string;
{$ENDIF}
begin
{$IFDEF ANDROID}
  Path := System.IOUtils.TPath.GetDocumentsPath + '/';
  frmBanks.LoadFileStream(Path + 'hi_hat_machine.pch2');
{$ELSE}
  if frmBanks.OpenDialog1.Execute then
  begin
    frmBanks.LoadFileStream(frmBanks.OpenDialog1.FileName);
  end;
{$ENDIF}
end;

procedure TPanelBanks.BtnRetrieveClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
  if assigned(FOnRestore) then
    FOnRestore(Self);
end;

procedure TPanelBanks.BtnSaveClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin

end;

procedure TPanelBanks.BtnStoreClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin

end;

constructor TPanelBanks.Create(AOwner: TComponent);
begin
  inherited;

  FLbBanks := TLabelGL.Create(Self);
  FLbBanks.Parent := Self;
  FLbBanks.Font.Size := 6 * SCALE_X;
  FLbBanks.Font.Style := [TFontStyle.fsBold];
  FLbBanks.Caption := 'Banks';

  FBtnRetrieve := TBtnTextGL.Create(Self);
  FBtnRetrieve.Parent := Self;
  FBtnRetrieve.BtnWidth := 32;
  FBtnRetrieve.BtnHeight := 12;
  FBtnRetrieve.BtnType := bttPush;
  FBtnRetrieve.Texts := 'Retrieve;Retrieve';
  FBtnRetrieve.OnClk := BtnRetrieveClk;

  FBtnStore := TBtnTextGL.Create(Self);
  FBtnStore.Parent := Self;
  FBtnStore.BtnWidth := 32;
  FBtnStore.BtnHeight := 12;
  FBtnStore.BtnType := bttPush;
  FBtnStore.Texts := 'Store;Store';
  FBtnStore.OnClk := BtnStoreClk;

  FBtnClear := TBtnTextGL.Create(Self);
  FBtnClear.Parent := Self;
  FBtnClear.BtnWidth := 32;
  FBtnClear.BtnHeight := 12;
  FBtnClear.BtnType := bttPush;
  FBtnClear.Texts := 'Clear;Clear';
  FBtnClear.OnClk := BtnClearClk;

  FLbFile := TLabelGL.Create(Self);
  FLbFile.Parent := Self;
  FLbFile.Font.Size := 6 * SCALE_X;
  FLbFile.Font.Style := [TFontStyle.fsBold];
  FLbFile.Caption := 'File';

  FBtnLoad := TBtnTextGL.Create(Self);
  FBtnLoad.Parent := Self;
  FBtnLoad.BtnWidth := 32;
  FBtnLoad.BtnHeight := 12;
  FBtnLoad.BtnType := bttPush;
  FBtnLoad.Texts := 'Load;Load';
  FBtnLoad.OnClk := BtnLoadClk;

  FBtnSave := TBtnTextGL.Create(Self);
  FBtnSave.Parent := Self;
  FBtnSave.BtnWidth := 32;
  FBtnSave.BtnHeight := 12;
  FBtnSave.BtnType := bttPush;
  FBtnSave.Texts := 'Save;Save';
  FBtnSave.OnClk := BtnSaveClk;
end;

destructor TPanelBanks.Destroy;
begin
  inherited;
end;

procedure TPanelBanks.Resize;
begin
  inherited;
  LayoutControls;
  FullRepaint;
end;

procedure TPanelBanks.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  inherited;
end;

procedure TPanelBanks.UpdateControls;
begin
//
end;

procedure TfrmBanks.FormCreate(Sender: TObject);
begin
  FPanelBanks := TPanelBanks.Create(self);
  FPanelBanks.Parent := Self;
  FPanelBanks.Height := 57;
  FPanelBanks.Align := TAlignLayout.MostTop;
  FPanelBanks.OnRestore := Restore;

  FBanksCtrl := TBanksGL.Create(self);
  FBanksCtrl.Parent := Self;
  FBanksCtrl.Align := TAlignLayout.Client;
  FBanksCtrl.OnRestore := Restore;
end;

procedure TfrmBanks.FormShow(Sender: TObject);
begin
  FPanelBanks.LayoutControls;
  FBanksCtrl.LayoutControls;
end;

procedure TfrmBanks.LoadFileStream(aFilename: string);
begin
  ConMan.LoadFromFile(aFilename);
  Invalidate;
end;

procedure TfrmBanks.Restore(Sender: TObject);
var
  SlotIndex: integer;
begin
  if assigned(Connection) then
  begin
    if FBanksCtrl.BankFileType = 1 then
      SlotIndex := 4
    else
      SlotIndex := Connection.SelectedSlotIndex;

    Connection.SynthRetreivePatch(
      SlotIndex,
      FBanksCtrl.Bank,
      FBanksCtrl.BankSlot);
    Hide;
  end;
end;

procedure TfrmBanks.SetConnectionIndex(const Value: integer);
begin
  inherited;
  FBanksCtrl.ConnectionIndex := Value;
  FPanelBanks.ConnectionIndex := Value;
end;

procedure TfrmBanks.SetConnectionManager(const Value: IG2ConnectionManager);
begin
  inherited;
  FBanksCtrl.ConMan := Value;
  FPanelBanks.ConMan := Value;
end;

end.
