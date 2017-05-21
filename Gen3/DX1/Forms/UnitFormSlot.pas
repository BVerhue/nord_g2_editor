unit UnitFormSlot;

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
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.Edit,
  FMX.Dialogs,
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2Controls,
  BVE.NMG2ControlsGL,
  BVE.NMG2ControlsG2,
  BVE.NMG2TexturesGL,
  UnitPatch,
  UnitSelectModule;

type
  TPanelSlot = class(TG2SlotPanelGL)
  private
    FLbSlot: TTexControlGL;
    FTePatch: TTextEditGL;
    FBtnVariation: TBtnRadioGL;
    FKnobVolume: TG2KnobGL;
    FBtnMute: TG2BtnTextGL;
    FBtnAdd: TBtnTextGL;
    FLbVoices: TTextFieldGL;
    FBtnVoices: TBtnIncDecGL;
    FBtnPart: TBtnRadioGL;
  private
    procedure VariationClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PatchClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure AddClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure VoicesClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PartClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);

    procedure ConnectParams;

    procedure UpdateVoices;
  protected
    procedure SetSlotIndex(const Value: integer); override;

    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
    procedure UpdateControls;
  end;

  TPanelModule = class(TG2SlotPanelGL)
  private
    FBtnCopy: TG2BtnTextGL;
    FBtnPaste: TG2BtnTextGL;
    FBtnDelete: TG2BtnTextGL;
    FBtnCancel: TG2BtnTextGL;
  private
    procedure CopyClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure PasteClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure DeleteClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
    procedure CancelClk(Sender: TObject; Shift: TShiftState; const aBtnIndex: integer);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
  end;

  TPanelPatchSettings = class(TG2SlotPanelGL)
  private
    FBtnArpEnable: TBtnTextGL;
    FBtnArpRate: TBtnRadioGL;
    FBtnArpDirection: TBtnRadioGL;
    FBtnArpRange: TBtnRadioGL;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
  end;

  TfrmSlot = class(TG2SlotFormGL)
    mbDrawCable: TPanel;
    bDcDelete: TButton;
    bDcCancel: TButton;
    mbModule: TPanel;
    mModuleDelete: TButton;
    mbCancel: TButton;
    bModuleCopy: TButton;
    bModulePaste: TButton;
    procedure FormCreate(Sender: TObject);
    procedure bDcCancelClick(Sender: TObject);
    procedure bDcDeleteClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FPanelSlot: TPanelSlot;
    FPanelModule: TPanelModule;
    FPatchSettings: TPanelPatchSettings;
    FPatchCtrl: TPatchGL;

    procedure EnableMenu(aMenu: TObject);
    procedure EnableCtrl(aCtrl: TObject);

    procedure OperationModeChange;

    procedure Update(aG2Event: TG2Event; const aG2Object: IG2Object); override;
  public
    property PatchCtrl: TPatchGL read FPatchCtrl;
  end;

implementation
uses
  BVE.NMG2GraphTypes,
  UnitFormAddModule,
  UnitFormBanks;

{$R *.fmx}

// -----------------------------------------------------------------------------
//
//                                TfrmSlot
//
// -----------------------------------------------------------------------------

procedure TfrmSlot.bDcCancelClick(Sender: TObject);
begin
  FPatchCtrl.OperationMode := omNormal;
end;

procedure TfrmSlot.bDcDeleteClick(Sender: TObject);
var
  CableList: TList<IG2Cable>;
  FromConnector, ToConnector: IG2Connector;
  Cable: IG2Cable;
begin
  if assigned(FPatchCtrl.FromConnector) then
  begin
    CableList := FPatchCtrl.FromConnector.Connector.CreateCableList;
    try
      for Cable in CableList do
      begin
        FromConnector := Cable.ConnFrom;
        ToConnector := Cable.ConnTo;

        Connection.PatchCableDelete(
          FromConnector.SlotIndex,
          FromConnector.LocationIndex,
          FromConnector.ModuleIndex,
          FromConnector.ConnectorIndex,
          FromConnector.ConnectorKind,
          ToConnector.ModuleIndex,
          ToConnector.ConnectorIndex,
          ToConnector.ConnectorKind);
      end;
    finally
      CableList.Free;
    end;
    FPatchCtrl.OperationMode := omNormal;
  end;
end;

procedure TfrmSlot.EnableCtrl(aCtrl: TObject);
begin
end;

procedure TfrmSlot.EnableMenu(aMenu: TObject);
begin
  FPanelSlot.Visible := (FPanelSlot = aMenu);
  mbDrawCable.Visible := (mbDrawCable = aMenu);
  FPanelModule.Visible := (mbModule = aMenu);
end;

procedure TfrmSlot.FormActivate(Sender: TObject);
begin
  Connection.PerfSelectSlot(FPatchCtrl.SlotIndex);
end;

procedure TfrmSlot.FormCreate(Sender: TObject);
begin
  FPanelSlot := TPanelSlot.Create(Self);
  FPanelSlot.Parent := Self;
  FPanelSlot.Align := TAlignLayout.MostTop;

  FPanelModule := TPanelModule.Create(Self);
  FPanelModule.Parent := Self;
  FPanelModule.Visible := False;
  FPanelModule.Align := TAlignLayout.MostTop;

  FPatchSettings := TPanelPatchSettings.Create(Self);
  FPatchSettings.Parent := Self;
  FPatchSettings.Visible := False;
  FPatchSettings.Align := TAlignLayout.Client;

  FPatchCtrl := TPatchGL.Create(self);
  FPatchCtrl.Parent := Self;
  FPatchCtrl.Align := TAlignLayout.Client;

  FPanelSlot.Visible := True;
  mbDrawCable.Visible := False;
  mbModule.Visible := False;
end;

procedure TfrmSlot.FormShow(Sender: TObject);
begin
  FPanelSlot.LayoutControls;
end;

procedure TfrmSlot.OperationModeChange;
begin
  if not assigned(Slot) then
    exit;

  case Slot.OperationMode of
    omAddModule:
      begin
        frmAddModule.Show;
      end;
    omDrawCable:
      begin
        EnableMenu(mbDrawCable);
        EnableCtrl(nil);
      end;
    omModule:
      begin
        EnableMenu(mbModule);
        EnableCtrl(nil);
      end;
    {omLog:
      begin
        EnableMenu(mbSlot);
        EnableCtrl(Memo1);
      end;}
    else
    begin
      EnableMenu(FPanelSlot);
      EnableCtrl(nil);
    end;
  end;
end;

procedure TfrmSlot.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    evtOperationModeChange:
      OperationModeChange;
    evtSelectLocation:
      begin
        if Slot.Patch.SelectedLocation = 2 then
        begin
          FPatchCtrl.Visible := False;
          FPatchSettings.Visible := True;
        end else begin
          FPatchSettings.Visible := False;
          FPatchCtrl.Visible := True;
        end;
      end;
  end;
end;

// -----------------------------------------------------------------------------
//
//                               TPanelSlot
//
// -----------------------------------------------------------------------------

procedure TPanelSlot.AddClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
    Slot.OperationMode := omAddModule;
end;

procedure TPanelSlot.ConnectParams;
begin
  if assigned(Slot) and assigned(Slot.Patch) then
  begin
    FKnobVolume.Param := Slot.Patch.Modules[LOCATION_PATCH, PATCH_VOLUME].Param[VOLUME_LEVEL];
    FBtnMute.Param := Slot.Patch.Modules[LOCATION_PATCH, PATCH_VOLUME].Param[VOLUME_MUTE];
  end else begin
    FKnobVolume.Param := nil;
    FBtnMute.Param := nil;
  end;
end;

constructor TPanelSlot.Create(AOwner: TComponent);
begin
  inherited;

  ShowScrollBars := False;

  Height := 57;

  FLbSlot := TTexControlGL.Create(Self);
  FLbSlot.Parent := Self;
  FLbSlot.TextureID := GetLabelTexID(12, 'A');
  FLbSlot.TextureList := TextureList;

  FTePatch := TTextEditGL.Create(Self);
  FTePatch.Parent := Self;
  FTePatch.TextureID := GetBtnTexID(43, 12);
  FTePatch.TextureList := TextureList;
  FTePatch.BtnType := TButtonTextType.bttPush;
  FTePatch.Caption := '';
  FTePatch.OnClk := PatchClk;

  FBtnAdd := TBtnTextGL.Create(Self);
  FBtnAdd.Parent := Self;
  FBtnAdd.TextureID := GetBtnTextTexID(24, 12, '', 'Add;Add');
  FBtnAdd.TextureList := TextureList;
  FBtnAdd.BtnType := TButtonTextType.bttPush;
  FBtnAdd.OnClk := AddClk;

  FBtnVariation := TBtnRadioGL.Create(Self);
  FBtnVariation.Parent := Self;
  FBtnVariation.TextureID := GetBtnRadioTexID(10, 12, '', '1;2;3;4;5;6;7;8');
  FBtnVariation.TextureList := TextureList;
  FBtnVariation.TexCols := 8;
  FBtnVariation.TexRows := 2;
  FBtnVariation.ColCount := 8;
  FBtnVariation.RowCount := 1;
  FBtnVariation.OnClk := VariationClk;

  FKnobVolume := TG2KnobGL.Create(Self);
  FKnobVolume.Parent := Self;
  FKnobVolume.TextureID := GetKnobTexID(ktHSlider);
  FKnobVolume.TextureList := TextureList;
  FKnobVolume.KnobType := ktHSlider;
  FKnobVolume.MinValue := 0;
  FKnobVolume.MaxValue := 127;

  FBtnMute := TG2BtnTextGL.Create(Self);
  FBtnMute.Parent := Self;
  FBtnMute.TextureID := GetBtnTextTexID(24, 12, '', 'Mute;On');
  FBtnMute.TextureList := TextureList;
  FBtnMute.BtnType := TButtonTextType.bttCheck;

  FLbVoices := TTextFieldGL.Create(Self);
  FLbVoices.Parent := Self;
  FLbVoices.TextureList := TextureList;
  FLbVoices.FieldWidth := 35;
  FLbVoices.FieldHeight := 12;
  FLbVoices.Caption := '      ';

  FBtnVoices := TBtnIncDecGL.Create(Self);
  FBtnVoices.Parent := Self;
  FBtnVoices.ColCount := 2;
  FBtnVoices.RowCount := 1;
  FBtnVoices.TextureID := GetBtnIncDecTexID(13, 12, True);
  FBtnVoices.TextureList := TextureList;
  FBtnVoices.OnClk := VoicesClk;

  FBtnPart := TBtnRadioGL.Create(Self);
  FBtnPart.Parent := Self;
  FBtnPart.TextureID := GetBtnRadioTexID(16, 12, '', 'FX;VA;Pch');
  FBtnPart.TextureList := TextureList;
  FBtnPart.TexCols := 3;
  FBtnPart.TexRows := 2;
  FBtnPart.ColCount := 3;
  FBtnPart.RowCount := 1;
  FBtnPart.OnClk := PartClk;
end;

destructor TPanelSlot.Destroy;
begin
  inherited;
end;

procedure TPanelSlot.PartClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.SelectedLocationIndex := aBtnIndex;
end;

procedure TPanelSlot.PatchClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
    Slot.OperationMode := omBanks;
end;

procedure TPanelSlot.Resize;
begin
  inherited;
  LayoutControls;
  FullRepaint
end;

procedure TPanelSlot.SetSlotIndex(const Value: integer);
begin
  inherited;
  UpdateControls;
end;

procedure TPanelSlot.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  case aG2Event of
    EvtUSBActiveChange:
      ;
    EvtUSBError:
      ;
    EvtBeforeSendMessage:
      ;
    EvtProcessResponseMsg:
      ;
    EvtNextInitStep:
      ;
    EvtAfterG2Init:
      begin
        UpdateControls;
      end;
    EvtAfterPerfInit:
      ;
    EvtAfterSlotInit:
      ;
    EvtPerfsSettingsUpdate:
      ;
    EvtPerfUpdate:
      ;
    EvtSynthSettingsUpdate:
      ;
    EvtBeforePatchUpdate:
      begin
      end;
    EvtPatchUpdate:
      begin
        UpdateControls;
      end;
    EvtVariationChange:
      begin
        if assigned(Slot) and assigned(Slot.Patch) then
          FBtnVariation.Value := Slot.Patch.Settings.ActiveVariation;
      end;
    EvtAfterGetAssignedVoices:
      begin
        FBtnVoices.MaxValue := 32;
        FBtnVoices.MinValue := 0;
        FBtnVoices.Value := Slot.Patch.Settings.VoiceCount + 2 - Slot.Patch.Settings.VoiceMode - 1;

        UpdateVoices;
      end;
  end;
end;

procedure TPanelSlot.UpdateControls;
begin
  case SlotIndex of
  0: FLbSlot.TextureID := GetLabelTexID(12, 'A');
  1: FLbSlot.TextureID := GetLabelTexID(12, 'B');
  2: FLbSlot.TextureID := GetLabelTexID(12, 'C');
  3: FLbSlot.TextureID := GetLabelTexID(12, 'D');
  end;
  ConnectParams;

  if assigned(Slot) then
  begin
    if assigned(Slot.Patch) then
    begin
      FBtnVariation.Value := Slot.Patch.Settings.ActiveVariation;
      FTePatch.Caption := Slot.Patch.PatchName;
      FBtnPart.Value := Connection.SelectedLocationIndex;
    end;

    UpdateVoices;
  end;

  FullRepaint;
end;

procedure TPanelSlot.UpdateVoices;
begin
  if FBtnVoices.Value = 0 then
    FLbVoices.Caption := 'Legato'
  else
    if FBtnVoices.Value = 1 then
      FLbVoices.Caption := 'Mono'
    else
      FLbVoices.Caption := IntToStr(Slot.AssignedVoices) + '(' + IntToStr(FBtnVoices.Value) + ')';
end;

procedure TPanelSlot.VariationClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.SlotSelectVariation(SlotIndex, aBtnIndex);
end;

procedure TPanelSlot.VoicesClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
var
  PatchSettings: IG2PatchSettings;
begin
  PatchSettings := Slot.Patch.Settings;

  case FBtnVoices.Value of
  0 : begin // Legato
        PatchSettings.VoiceCount := 1;
        PatchSettings.VoiceMode := 2;
      end;
  1 : begin // Mono
        PatchSettings.VoiceCount := 1;
        PatchSettings.VoiceMode := 1;
      end;
  2..32 :
      begin
        PatchSettings.VoiceCount := FBtnVoices.Value - 1;
        PatchSettings.VoiceMode := 0;
      end;
  end;

  UpdateVoices;

  Connection.PatchSettings(Slot.Patch, PatchSettings);
end;

// -----------------------------------------------------------------------------
//
//                              TPanelModule
//
// -----------------------------------------------------------------------------

constructor TPanelModule.Create(AOwner: TComponent);
begin
  inherited;

  ShowScrollBars := False;

  Height := 57;

  FBtnCopy := TG2BtnTextGL.Create(Self);
  FBtnCopy.Parent := Self;
  FBtnCopy.TextureID := GetBtnTextTexID(24, 12, '', 'Copy;Copy');
  FBtnCopy.TextureList := TextureList;
  FBtnCopy.BtnType := TButtonTextType.bttPush;
  FBtnCopy.OnClk := CopyClk;

  FBtnPaste := TG2BtnTextGL.Create(Self);
  FBtnPaste.Parent := Self;
  FBtnPaste.TextureID := GetBtnTextTexID(24, 12, '', 'Paste;Paste');
  FBtnPaste.TextureList := TextureList;
  FBtnPaste.BtnType := TButtonTextType.bttPush;
  FBtnPaste.OnClk := PasteClk;

  FBtnDelete := TG2BtnTextGL.Create(Self);
  FBtnDelete.Parent := Self;
  FBtnDelete.TextureID := GetBtnTextTexID(24, 12, '', 'Delete;Delete');
  FBtnDelete.TextureList := TextureList;
  FBtnDelete.BtnType := TButtonTextType.bttPush;
  FBtnDelete.OnClk := DeleteClk;

  FBtnCancel := TG2BtnTextGL.Create(Self);
  FBtnCancel.Parent := Self;
  FBtnCancel.TextureID := GetBtnTextTexID(24, 12, '', 'Cancel;Cancel');
  FBtnCancel.TextureList := TextureList;
  FBtnCancel.BtnType := TButtonTextType.bttPush;
  FBtnCancel.OnClk := CancelClk;
end;

procedure TPanelModule.CancelClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) then
    Slot.OperationMode := omNormal;
end;

procedure TPanelModule.CopyClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.Copy;
end;

procedure TPanelModule.DeleteClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  if assigned(Slot) and assigned(Slot.Patch) then
    Connection.PatchModulesSelectedDelete(
      Slot.Patch.PatchPart[Slot.Patch.SelectedLocation]);
end;

destructor TPanelModule.Destroy;
begin
  inherited;
end;

procedure TPanelModule.PasteClk(Sender: TObject; Shift: TShiftState;
  const aBtnIndex: integer);
begin
  Connection.Paste;
end;

procedure TPanelModule.Resize;
begin
  inherited;
  LayoutControls;
  FullRepaint
end;

procedure TPanelModule.Update(aG2Event: TG2Event; const aG2Object: IG2Object);
begin
  inherited;
end;

// -----------------------------------------------------------------------------
//
//                           TPanelPatchSettings
//
// -----------------------------------------------------------------------------

constructor TPanelPatchSettings.Create(AOwner: TComponent);
begin
  inherited;

  FBtnArpEnable := TG2BtnTextGL.Create(Self);
  FBtnArpEnable.Parent := Self;
  FBtnArpEnable.TextureID := GetBtnTextTexID(24, 12, '', 'Arp off;Arp on');
  FBtnArpEnable.TextureList := TextureList;
  FBtnArpEnable.BtnType := TButtonTextType.bttCheck;

  FBtnArpRate := TBtnRadioGL.Create(Self);
  FBtnArpRate.Parent := Self;
  FBtnArpRate.TextureID := GetBtnRadioTexID(24, 12, '', '1/8;1/8T;1/16;1/16T');
  FBtnArpRate.TextureList := TextureList;
  FBtnArpRate.TexCols := 4;
  FBtnArpRate.TexRows := 2;
  FBtnArpRate.ColCount := 4;
  FBtnArpRate.RowCount := 1;

  FBtnArpDirection := TBtnRadioGL.Create(Self);
  FBtnArpDirection.Parent := Self;
  FBtnArpDirection.TextureID := GetBtnRadioTexID(24, 12, '', 'Up;Down;Up/Dn;Rnd');
  FBtnArpDirection.TextureList := TextureList;
  FBtnArpDirection.TexCols := 4;
  FBtnArpDirection.TexRows := 2;
  FBtnArpDirection.ColCount := 4;
  FBtnArpDirection.RowCount := 1;

  FBtnArpRange := TBtnRadioGL.Create(Self);
  FBtnArpRange.Parent := Self;
  FBtnArpRange.TextureID := GetBtnRadioTexID(24, 12, '', '4 oct;3 oct; 2 oct; 1 oct');
  FBtnArpRange.TextureList := TextureList;
  FBtnArpRange.TexCols := 4;
  FBtnArpRange.TexRows := 2;
  FBtnArpRange.ColCount := 4;
  FBtnArpRange.RowCount := 1;
end;

destructor TPanelPatchSettings.Destroy;
begin
  inherited;
end;

procedure TPanelPatchSettings.Resize;
begin
  inherited;
end;

procedure TPanelPatchSettings.Update(aG2Event: TG2Event;
  const aG2Object: IG2Object);
begin
  inherited;
end;

end.
