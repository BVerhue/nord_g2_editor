unit UnitBanks;

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

// Banks control

interface
uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Viewport3D,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Objects,
  FMX.MaterialSources,
  BVE.NMG2Types,
  BVE.NMG2FileIntf,
  BVE.NMG2TexturesGL,
  BVE.NMG2ControlsGL,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2;

type
  TBanksGL = class(TG2PanelGL)
  private
    FBtnFileType: TBtnRadioEditGL;
    FBtnBank: TBtnRadioEditGL;
    FBtnBankSlot: TBtnRadioEditGL;

    FOnRestore: TNotifyEvent;

    procedure FileTypeClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BankSlotClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
    procedure BankClk(Sender: TObject; Shift: TShiftState; const aValue: integer);

    function GetBank: integer;
    function GetBankSlot: integer;
    function GetBankFileType: integer;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
    procedure UpdateControls;
    procedure LayoutControls; override;

    property BankFileType: integer read GetBankFileType;
    property Bank: integer read GetBank;
    property BankSlot: integer read GetBankSlot;

    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
  end;

implementation
uses
  BVE.NMG2GraphTypes;

{ TBanksGL }

procedure TBanksGL.BankClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
  UpdateControls;
end;

procedure TBanksGL.BankSlotClk(Sender: TObject; Shift: TShiftState; const aValue: integer);
begin
  if ssDouble in Shift then
  begin
    if assigned(FOnRestore) then
      FOnRestore(Self);
  end;
end;

constructor TBanksGL.Create(AOwner: TComponent);
begin
  inherited;

  FBtnFileType := TBtnRadioEditGL.Create(self);
  FBtnFileType.Parent := Self;
  FBtnFileType.TextureID := GetBtnTexID(36, 12);
  FBtnFileType.TextureList := TextureList;
  FBtnFileType.Padding := 1.5;
  FBtnFileType.ColCount := 1;
  FBtnFileType.RowCount := 2;
  FBtnFileType.OnClk := FileTypeClk;

  FBtnBank := TBtnRadioEditGL.Create(self);
  FBtnBank.Parent := Self;
  FBtnBank.TextureID := GetBtnTexID(18, 12);
  FBtnBank.TextureList := TextureList;
  FBtnBank.Padding := 1.5;
  FBtnBank.ColCount := 16;
  FBtnBank.RowCount := 2;
  FBtnBank.Texts := '1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20;21;22;23;24;25;26;27;28;29;30;31;32';
  FBtnBank.OnClk := BankClk;

  FBtnBankSlot := TBtnRadioEditGL.Create(self);
  FBtnBankSlot.Parent := Self;
  FBtnBankSlot.TextureID := GetBtnTexID(40, 12);
  FBtnBankSlot.TextureList := TextureList;
  FBtnBankSlot.ColCount := 8;
  FBtnBankSlot.RowCount := 16;
  FBtnBankSlot.Padding := 1.5;
  FBtnBankSlot.OnClk := BankSlotClk;
end;

destructor TBanksGL.Destroy;
begin
  inherited;
end;

procedure TBanksGL.FileTypeClk(Sender: TObject; Shift: TShiftState;
  const aValue: integer);
begin
  UpdateControls;
end;

function TBanksGL.GetBank: integer;
begin
  Result := FBtnBank.Value;
end;

function TBanksGL.GetBankSlot: integer;
begin
  Result := FBtnBankSlot.Value;
end;

function TBanksGL.GetBankFileType: integer;
begin
  Result := FBtnFileType.Value;
end;

procedure TBanksGL.LayoutControls;
var
  Margin: single;
  dx: single;
begin
  Margin := 10;

  dx := (Width - (FBtnFileType.Width + Margin + FBtnBank.Width)) / 2;

  FBtnFileType.Position := Point3D(dx, Margin, 0);

  FBtnBank.Position := Point3D(dx + FBtnFileType.Width + Margin, Margin, 0);

  FBtnBankSlot.Position := Point3D( 0, FBtnBank.Height + Margin + Margin, 0);
  FBtnBankSlot.BtnWidth := (Width / 8 - FBtnBankSlot.Padding) / SCALE_X;
  FullRepaint;
end;

procedure TBanksGL.Resize;
begin
  inherited;
  LayoutControls;
end;

procedure TBanksGL.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtAfterG2Init:
      begin
        UpdateControls;
      end;
  end;
end;

procedure TBanksGL.UpdateControls;
var
  sl: TStringList;
  i, BankIndex: integer;
  Synth: IG2Synth;
  PatchType: TPatchFileType;
  BankItem: IG2BankItem;
begin
  if (not assigned(ConMan)) or (not assigned(ConMan.SelectedConnection)) then
    exit;

  Synth := ConMan.SelectedConnection.Synth;

  if not assigned(Synth) then
    exit;

  BankIndex := FBtnBank.Value;
  case FBtnFileType.Value of
    0: PatchType := pftPatch;
    1: PatchType := pftPerf;
    else
      PatchType := pftPatch;
  end;

  sl := TStringList.Create;
  try
    sl.Delimiter := ';';

    for i := 0 to 127 do
    begin
      sl.Add('..Empty..');
    end;

    for i := 0 to Synth.BankItemCount - 1 do begin
      BankItem := Synth.BankItems[i];
      if (BankItem.Bank = BankIndex) and (BankItem.PatchFileType = PatchType) then begin
        sl[BankItem.Patch] := IntToStr(BankItem.Patch) + ' ' + BankItem.PatchName;
      end;
    end;

    FBtnBankSlot.Texts := sl.DelimitedText;

  finally
    sl.Free;
  end;
end;

end.
