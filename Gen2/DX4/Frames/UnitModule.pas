unit UnitModule;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

{$I ..\Common\CompilerSettings.Inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Rtti,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.TabControl,
  FMX.Edit,
  FMX.Layouts,
  FMX.Controls.Presentation,
  BVE.NMG2Types,
  BVE.NMG2USB,
  BVE.NMG2GraphFMX,
  BVE.NMG2ControlsFMX,
  UnitSlot;

type
  TframeModule = class(TFrame, IG2Observer)
    Layout1: TLayout;
    eModuleName: TEdit;
    btModuleCopy: TG2BtnText;
    btModuleDelete: TG2BtnText;
    btModuleCut: TG2BtnText;
    btModulePaste: TG2BtnText;
    btSelectMulti: TG2BtnText;
    TabControl1: TTabControl;
    TabItem2: TTabItem;
    btUndo: TG2BtnText;
    TabItem1: TTabItem;
    eModuleLabel: TEdit;
    TabItem3: TTabItem;
    rbModuleColor: TG2BtnRadioEdit;
    btModuleColorDefault: TG2BtnText;
    G2Label1: TG2Label;
    G2Label2: TG2Label;
    Panel1: TPanel;
    G2Label11: TG2Label;
    rbPage: TG2BtnRadio;
    ArrayLayout1: TArrayLayout;
    btPageIndex1: TG2BtnText;
    btPageIndex2: TG2BtnText;
    btPageIndex3: TG2BtnText;
    Panel2: TPanel;
    G2Label3: TG2Label;
    rbGlobalPage: TG2BtnRadio;
    ArrayLayout2: TArrayLayout;
    btGlobalPageIndex1: TG2BtnText;
    btGlobalPageIndex2: TG2BtnText;
    btGlobalPageIndex3: TG2BtnText;
    btPianoRoll: TG2BtnText;
    btPasteParams: TG2BtnText;
    btSelectAllVA: TG2BtnText;
    btSelectAllFX: TG2BtnText;
    procedure btModuleDeleteChangeValue(Sender: TObject; const aValue: Integer);
    procedure btModuleCutChangeValue(Sender: TObject; const aValue: Integer);
    procedure btModulePasteChangeValue(Sender: TObject; const aValue: Integer);
    procedure btModuleCopyChangeValue(Sender: TObject; const aValue: Integer);
    procedure btUndoChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbModuleColorPaintElement(Sender: TObject;
      const aElementType: TControlElementType; aElementIndex: Integer;
      aStyleSet: TG2StyleSet);
    procedure eModuleLabelExit(Sender: TObject);
    procedure rbPageChangeValue(Sender: TObject; const aValue: Integer);
    procedure btPageIndex1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btPageIndex2ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btPageIndex3ChangeValue(Sender: TObject; const aValue: Integer);
    procedure btGlobalPageIndex1ChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btGlobalPageIndex2ChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btGlobalPageIndex3ChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure rbModuleColorChangeValue(Sender: TObject; const aValue: Integer);
    procedure btModuleColorDefaultChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btPianoRollChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSelectMultiChangeValue(Sender: TObject; const aValue: Integer);
    procedure btPasteParamsChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSelectAllVAChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSelectAllFXChangeValue(Sender: TObject; const aValue: Integer);
  private
    [Weak] FframeSlot: TframeSlot;
    FCopyPatch: TG2GraphPatchPartFMX;
    FCopyVariation: Integer;
    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference(aData: IG2Subject);
    procedure SetCopyPatch(const Value: TG2GraphPatchPartFMX);
    procedure SetframeSlot(const Value: TframeSlot);
  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateControls;
    procedure ModuleAssignKnobs(aPageIndex: Integer);
    procedure ModuleAssignGlobalKnobs(aPageIndex: Integer);
    procedure Copy;
    procedure Cut;
    procedure Paste;
    procedure PasteParams;
    procedure Delete;
    procedure Undo;
    procedure SetStateStyles(aStateStyleList: TG2StateStyleList);
    property frameSlot: TframeSlot read FframeSlot write SetframeSlot;
    property CopyPatch: TG2GraphPatchPartFMX read FCopyPatch;
  end;

implementation

{$R *.fmx}

{ TFrameModule }
constructor TframeModule.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  FCopyPatch := nil;
  rbPage.Value := 0;
  btPageIndex1.Value := 0;
  btPageIndex2.Value := 0;
  btPageIndex3.Value := 0;
  rbGlobalPage.Value := 0;
  btGlobalPageIndex1.Value := 0;
  btGlobalPageIndex2.Value := 0;
  btGlobalPageIndex3.Value := 0;
end;

procedure TframeModule.Copy;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.PatchPart) then
  begin
    if assigned(FCopyPatch) then
      FCopyPatch.DisposeOF;

    FCopyPatch := TG2GraphPatchPartFMX.CopyModules(FframeSlot.Patch,
      FframeSlot.PatchPart, FframeSlot.PatchPart.SelectedModuleList);
    FCopyPatch.Location := FframeSlot.SelectedLocation;
    // FCopyPatch.ModulePanelDefs := FFrameSlot.Patch.ModulePanelDefs;
    FCopyVariation := FframeSlot.Patch.ActiveVariation;
    UpdateControls;
  end;
end;

procedure TframeModule.Cut;
begin
  Copy;
  Delete;
end;

procedure TframeModule.Paste;
var
  Rect: TRect;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.PatchPart) then
  begin
    if assigned(FCopyPatch) then
    begin
      Rect := FCopyPatch.ModuleList.UnitsRect;
      FframeSlot.ActivateSelection(
        FCopyPatch,
        smCopy,
        ((Rect.Left + (Rect.Right - Rect.Left) / 2)
          * (UNITS_COL + UNIT_MARGIN * 2)),
        ((Rect.Top + (Rect.Bottom - Rect.Top) / 2)
          * (UNITS_ROW + UNIT_MARGIN * 2)));
    end;
  end;
end;

procedure TframeModule.PasteParams;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.Patch) then
  begin
    if assigned(FCopyPatch) and FframeSlot.Patch.CheckParametersFit(FCopyPatch,
      FframeSlot.Patch.SelectedLocation) then
    begin
      FframeSlot.Patch.MessCopyParameters(FCopyPatch,
        FframeSlot.Patch.SelectedLocation, FCopyVariation,
        FframeSlot.Patch.ActiveVariation);
    end;
  end;
end;

procedure TframeModule.Delete;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.PatchPart) then
    if FframeSlot.PatchPart.SelectedModuleList.Count > 0 then
      FframeSlot.PatchPart.Patch.MessDeleteModules(FframeSlot.SelectedLocation);
end;

procedure TframeModule.Undo;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.PatchPart) then
  begin
    (FframeSlot.Patch as TG2USBPatch).SendUndoMessage;
    UpdateControls;
  end;
end;

procedure TframeModule.btGlobalPageIndex1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  ModuleAssignGlobalKnobs(rbGlobalPage.Value * 3 + 0);
end;

procedure TframeModule.btGlobalPageIndex2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  ModuleAssignGlobalKnobs(rbGlobalPage.Value * 3 + 1);
end;

procedure TframeModule.btGlobalPageIndex3ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  ModuleAssignGlobalKnobs(rbGlobalPage.Value * 3 + 2);
end;

procedure TframeModule.btModuleCopyChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Copy;
end;

procedure TframeModule.btModuleCutChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Cut;
end;

procedure TframeModule.btModuleDeleteChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Delete;
end;

procedure TframeModule.btModulePasteChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Paste;
end;

procedure TframeModule.btPageIndex1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    ModuleAssignKnobs(rbPage.Value * 3 + 0);
end;

procedure TframeModule.btPageIndex2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    ModuleAssignKnobs(rbPage.Value * 3 + 1);
end;

procedure TframeModule.btPageIndex3ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    ModuleAssignKnobs(rbPage.Value * 3 + 2);
end;

procedure TframeModule.btPasteParamsChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if not assigned(FframeSlot.Patch) then
    Exit;
  if aValue = 0 then
    PasteParams;
end;

procedure TframeModule.btPianoRollChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if not assigned(FframeSlot.Patch) then
    Exit;
  if aValue = 0 then
    FframeSlot.ActivatePianoroll;
end;

procedure TframeModule.btSelectAllFXChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if not assigned(FframeSlot.Patch) then
    Exit;
  if aValue = 0 then
    FframeSlot.Patch.PatchPart[ord(ltFX)].SelectAll;
end;

procedure TframeModule.btSelectAllVAChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if not assigned(FframeSlot.Patch) then
    Exit;
  if not assigned(FframeSlot.Patch) then
    Exit;
  if aValue = 0 then
    FframeSlot.Patch.PatchPart[ord(ltVA)].SelectAll;
end;

procedure TframeModule.btSelectMultiChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if not assigned(FframeSlot.Patch) then
    Exit;
  FframeSlot.Patch.SelectMultiple := aValue = 1;
end;

procedure TframeModule.btUndoChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    Undo;
end;

procedure TframeModule.eModuleLabelExit(Sender: TObject);
var
  Module: TG2GraphModuleFMX;
begin
  if not assigned(FframeSlot.PatchPart) then
    Exit; // Location is ltPatch

  // Set KillFocusOnReturn
  if FframeSlot.PatchPart.SelectedModuleList.Count = 1 then
  begin
    Module := FframeSlot.PatchPart.SelectedModuleList[0] as TG2GraphModuleFMX;
    FframeSlot.PatchPart.Patch.MessSetModuleLabel(Module.Location,
      Module.ModuleIndex, eModuleLabel.Text);
  end;
end;

procedure TframeModule.btModuleColorDefaultChangeValue(Sender: TObject;
  const aValue: Integer);
var
  i: Integer;
  Module: TG2GraphModuleFMX;
begin
  if not assigned(FframeSlot.PatchPart) then
    Exit; // Location is ltPatch

  if aValue = 0 then
    for i := 0 to FframeSlot.PatchPart.SelectedModuleList.Count - 1 do
    begin
      Module := FframeSlot.PatchPart.SelectedModuleList[i] as TG2GraphModuleFMX;
      FframeSlot.PatchPart.Patch.MessSetModuleColor(
        FframeSlot.PatchPart.Location, Module.ModuleIndex, 0);
    end;
end;

procedure TframeModule.rbModuleColorChangeValue(Sender: TObject;
  const aValue: Integer);
var
  i: Integer;
  Module: TG2GraphModuleFMX;
begin
  if not assigned(FframeSlot.PatchPart) then
    Exit; // Location is ltPatch

  for i := 0 to FframeSlot.PatchPart.SelectedModuleList.Count - 1 do
  begin
    Module := FframeSlot.PatchPart.SelectedModuleList[i] as TG2GraphModuleFMX;
    FframeSlot.PatchPart.Patch.MessSetModuleColor(FframeSlot.PatchPart.Location,
      Module.ModuleIndex, ModuleColorOrder[aValue + 1]);
  end;
end;

procedure TframeModule.rbModuleColorPaintElement(Sender: TObject;
  const aElementType: TControlElementType; aElementIndex: Integer;
  aStyleSet: TG2StyleSet);
begin
  aStyleSet.Fill.Color := ConvertToAlpha(
    ModuleColors[ModuleColorOrder[aElementIndex + 1]]);
end;

procedure TframeModule.ModuleAssignGlobalKnobs(aPageIndex: Integer);
var
  Module: TG2GraphModuleFMX;
begin
  if not assigned(FframeSlot.PatchPart) then
    Exit; // Location is ltPatch

  if FframeSlot.PatchPart.SelectedModuleList.Count = 1 then
  begin
    Module := FframeSlot.PatchPart.SelectedModuleList[0] as TG2GraphModuleFMX;
    if Module.AssignableKnobCount > 0 then
      (FframeSlot.PatchPart.Patch as TG2GraphPatchFMX).MessModuleAssignGlobalKnobs(
        Module, aPageIndex);
  end;
end;

procedure TframeModule.ModuleAssignKnobs(aPageIndex: Integer);
var
  Module: TG2GraphModuleFMX;
begin
  if not assigned(FframeSlot.PatchPart) then
    Exit; // Location is ltPatch
  if FframeSlot.PatchPart.SelectedModuleList.Count = 1 then
  begin
    Module := FframeSlot.PatchPart.SelectedModuleList[0] as TG2GraphModuleFMX;
    if Module.AssignableKnobCount > 0 then
      (FframeSlot.PatchPart.Patch as TG2GraphPatchFMX).MessModuleAssignKnobs(
        Module, aPageIndex);
  end;
end;

procedure TframeModule.rbPageChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  //
end;

procedure TframeModule.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeModule.SetCopyPatch(const Value: TG2GraphPatchPartFMX);
begin
  if FCopyPatch <> Value then
  begin
    FCopyPatch := Value;
    UpdateControls;
  end;
end;

procedure TframeModule.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframeModule.SetframeSlot(const Value: TframeSlot);
begin
  if FframeSlot <> Value then
  begin
    if assigned(FframeSlot) and assigned(FframeSlot.Patch) then
      FframeSlot.Patch.RemoveObserver(self);

    FframeSlot := Value;

    if assigned(FframeSlot) and assigned(FframeSlot.Patch) then
      FframeSlot.Patch.RegisterObserver(self);

    UpdateControls;
  end;
end;

procedure TframeModule.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange:
      ;
    EvtUSBError:
      ;
    EvtBeforeSendMessage:
      ;
    EvtReceiveResponseMessage:
      ;
    EvtNextInitStep:
      ;
    EvtAfterG2Init:
      ;
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
      ;
    EvtPatchUpdate:
      ;
    EvtVariationChange:
      ;
    EvtCopyVariation:
      ;
    EvtMidiClockReceive:
      ;
    EvtClockRunChange:
      ;
    EvtClockBPMChange:
      ;
    EvtMidiCCRecieve:
      ;
    EvtAfterGetAssignedVoices:
      ;
    EvtPatchLoadChange:
      ;
    EvtSelectSlot:
      ;
    EvtSelectLocation:
      begin
        UpdateControls;
      end;
    EvtSelectModule:
      begin
        UpdateControls;
      end;
    EvtSelectParam:
      ;
    EvtLabelValueChange:
      begin
        UpdateControls;
      end;
    EvtMorphChange:
      ;
    EvtDeleteModule:
      begin
        UpdateControls;
      end;
    EvtAfterRetreivePatch:
      ;
    EvtAfterBankList:
      ;
    EvtAfterStore:
      ;
    EvtAfterClear:
      ;
    EvtAfterClearBank:
      ;
    EvtAfterBankDownload:
      ;
    EvtDeassignKnob:
      ;
    EvtAssignKnob:
      ;
    EvtDeassignGlobalKnob:
      ;
    EvtAssignGlobalKnob:
      ;
  end;
end;

procedure TframeModule.UpdateControls;
begin
  if assigned(FframeSlot) and assigned(FframeSlot.PatchPart) then
  begin
    if (FframeSlot.PatchPart.SelectedModuleList.Count > 0) then
    begin
      if FframeSlot.PatchPart.SelectedModuleList.Count = 1 then
      begin
        eModuleName.Text := FframeSlot.PatchPart.SelectedModuleList[0].ModuleName;
        eModuleLabel.Enabled := True;
        eModuleLabel.Text := FframeSlot.PatchPart.GetModuleLabel(
          FframeSlot.PatchPart.SelectedModuleList[0].ModuleIndex);;
        btPageIndex1.State := csDefault;
        btPageIndex2.State := csDefault;
        btPageIndex3.State := csDefault;
        btGlobalPageIndex1.State := csDefault;
        btGlobalPageIndex2.State := csDefault;
        btGlobalPageIndex3.State := csDefault;
        if FframeSlot.PatchPart.SelectedModuleList[0].TypeID = 121 then
          btPianoRoll.State := csDefault
        else
          btPianoRoll.State := csDisabled;
      end
      else
      begin
        eModuleName.Text := 'Multiple (' +
          IntToStr(FframeSlot.PatchPart.SelectedModuleList.Count) + ')';
        eModuleLabel.Text := '';
        eModuleLabel.Enabled := False;
        btPageIndex1.State := csDisabled;
        btPageIndex2.State := csDisabled;
        btPageIndex3.State := csDisabled;
        btGlobalPageIndex1.State := csDisabled;
        btGlobalPageIndex2.State := csDisabled;
        btGlobalPageIndex3.State := csDisabled;
        btPianoRoll.State := csDisabled;
      end;
      btModuleCut.State := csDefault;
      btModuleCopy.State := csDefault;
      btModuleDelete.State := csDefault;
    end
    else
    begin
      eModuleName.Text := 'None';
      btModuleCut.State := csDisabled;
      btModuleCopy.State := csDisabled;
      btModuleDelete.State := csDisabled;
      btPianoRoll.State := csDisabled;
      btPageIndex1.State := csDisabled;
      btPageIndex2.State := csDisabled;
      btPageIndex3.State := csDisabled;
      btGlobalPageIndex1.State := csDisabled;
      btGlobalPageIndex2.State := csDisabled;
      btGlobalPageIndex3.State := csDisabled;
    end;

    btSelectMulti.State := csDefault;
    if FframeSlot.Patch.SelectMultiple then
      btSelectMulti.Value := 1
    else
      btSelectMulti.Value := 0;

    if assigned(FCopyPatch) and (FCopyPatch.ModuleList.Count > 0) then
    begin
      btModulePaste.State := csDefault;
      if FframeSlot.Patch.CheckParametersFit(FCopyPatch,
        FframeSlot.PatchPart.Location) then
        btPasteParams.State := csDefault
      else
        btPasteParams.State := csDisabled;
    end
    else
    begin
      btModulePaste.State := csDisabled;
      btPasteParams.State := csDisabled;
    end;
  end
  else
  begin
    btModuleCut.State := csDisabled;
    btModuleCopy.State := csDisabled;
    btModulePaste.State := csDisabled;
    btModuleDelete.State := csDisabled;
    btPianoRoll.State := csDisabled;
    btPasteParams.State := csDisabled;
    btPageIndex1.State := csDisabled;
    btPageIndex2.State := csDisabled;
    btPageIndex3.State := csDisabled;
    btGlobalPageIndex1.State := csDisabled;
    btGlobalPageIndex2.State := csDisabled;
    btGlobalPageIndex3.State := csDisabled;
    btSelectMulti.State := csDisabled;
  end;
end;

end.
