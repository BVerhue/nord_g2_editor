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

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects,
{$IFDEF VER260}
  FMX.Graphics,
{$ENDIF}
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2ControlsFMX, BVE.NMG2GraphFMX,
  FMX.Layouts, FMX.ListBox, FMX.TreeView, UnitAppSettings;

type
  TframeBanks = class(TFrame, IG2Observer)
    Rectangle1: TRectangle;
    rbeBank: TG2BtnRadioEdit;
    rbeSlot: TG2BtnRadioEdit;
    Rectangle2: TRectangle;
    btRetreive: TG2BtnText;
    btStoreCurrentPatch: TG2BtnText;
    Layout1: TLayout;
    btClear: TG2BtnText;
    rbPatchType: TG2BtnRadio;
    Layout3: TLayout;
    btLoadBank: TG2BtnText;
    btSaveBank: TG2BtnText;
    procedure rbeSlotPaintElement(Sender: TObject;
      const aElementType: TControlElementType; aElementIndex: Integer;
      aStyleSet: TG2StyleSet);
    procedure btLoadPerfMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btRetreiveChangeValue(Sender: TObject; const aValue: Integer);
    procedure btStoreCurrentPatchChangeValue(Sender: TObject;
      const aValue: Integer);
    procedure btClearChangeValue(Sender: TObject; const aValue: Integer);
    procedure btLoadBankChangeValue(Sender: TObject; const aValue: Integer);
    procedure btSaveBankChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbPatchTypeChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbeBankChangeValue(Sender: TObject; const aValue: Integer);
    procedure rbeSlotDblClick(Sender: TObject);
  private
    [Weak] FSynth : TG2GraphFMX;
    [Weak] FFrameAppSettings : TframeAppSettings;

    FPatchType : TPatchFileType;

    procedure Update(aG2Event: TG2Event);
    procedure RemoveReference( aData : IG2Subject);

    procedure SetSynth(const Value: TG2GraphFMX);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure Retrieve(aSlotIndex, aBankIndex, aPatchIndex: integer);
    procedure Store( aSlotIndex, aBankIndex, aPatchIndex : integer);
    procedure SetPatchCategorie(aPatchIndex, aCategoryIndex: integer);
    procedure ClearBank( aPatchFileType : TPatchFileType; aBankIndex: integer);
    procedure ClearLocation( aPatchFileType : TPatchFileType; aBankIndex, aPatchIndex : integer);
    procedure LoadBank(const aFilename : string);
    procedure SaveBank(const aFilename : string);

    procedure UpdateBanks;
    procedure UpdateControls;

    procedure SetStateStyles( aStateStyleList : TG2StateStyleList);

    property frameAppSettings : TframeAppSettings read FframeAppSettings write FframeAppSettings;
    property Synth : TG2GraphFMX read FSynth write SetSynth;
  end;

implementation

{$R *.fmx}

function ConvertToAlpha( aColor : integer): TAlphaColor;
begin
  Result := $ff000000
          + (Cardinal(aColor) and $000000ff) shl 16
          + (Cardinal(aColor) and $0000ff00)
          + (Cardinal(aColor) and $00ff0000) shr 16;
end;

//==============================================================================
//
//                            TframeBanks
//
//==============================================================================

constructor TframeBanks.Create(AOwner: TComponent);
begin
  inherited;

  rbPatchType.Value := 0;
end;

destructor TframeBanks.Destroy;
begin
  inherited;
end;

//==============================================================================
//
//                             Functions
//
//==============================================================================

procedure TframeBanks.RemoveReference(aData: IG2Subject);
begin
  //
end;

procedure TframeBanks.Retrieve( aSlotIndex, aBankIndex,
  aPatchIndex: integer);
begin
  if assigned(Synth) then begin
    case FPatchType of
      pftPatch :
        begin
          Synth.SendRetrieveMessage( aSlotIndex, aBankIndex, aPatchIndex)
        end;
      pftPerf :
        begin
          Synth.SendRetrieveMessage( 4, aBankIndex, aPatchIndex)
        end;
    end;
  end;
end;

procedure TframeBanks.SetPatchCategorie(aPatchIndex,
  aCategoryIndex: integer);
var FPatchDescription : TPatchDescription;
begin
  if assigned(Synth) then begin
    FPatchDescription := Synth.GetSlot(aPatchIndex).Patch.PatchDescription;
    FPatchDescription.Categorie := aCategoryIndex;
    (Synth.GetSlot( aPatchIndex).Patch as TG2GraphPatchFMX).MessSetPatchDescription( FPatchDescription);
  end;
end;

procedure TframeBanks.Store( aSlotIndex, aBankIndex, aPatchIndex : integer);
begin
  if assigned(Synth) then begin
    case FPatchType of
      pftPatch :
        begin
          Synth.SendStoreMessage( aSlotIndex, aBankIndex, aPatchIndex)
        end;
      pftPerf :
        begin
           Synth.SendStoreMessage( 4, aBankIndex, aPatchIndex)
        end;
    end;
  end;
end;

procedure TframeBanks.ClearBank( aPatchFileType : TPatchFileType; aBankIndex: integer);
var FromLocation, ToLocation : byte;
begin
  if assigned(Synth) then begin
    if Synth.BankList.FindFirstLast( aPatchFileType, aBankIndex, FromLocation, ToLocation) then begin
      if MessageDlg('All patches in bank ' + IntToStr(aBankIndex + 1) + ' wil be deleted, sure?', TMsgDlgType.mtWarning,  mbOKCancel, 0) = mrOk then
        Synth.SendClearBankMessage( aPatchFileType, aBankIndex, FromLocation, ToLocation);
    end else
      raise Exception.Create('Bank is already empty.');
  end;
end;

procedure TframeBanks.ClearLocation( aPatchFileType : TPatchFileType; aBankIndex, aPatchIndex : integer);
begin
  if assigned(Synth) then begin
    Synth.SendClearMessage( aPatchFileType, aBankIndex, aPatchIndex)
  end;
end;

procedure TframeBanks.LoadBank(const aFilename : string);
var PatchFileType, Bank, Location : byte;
    PatchName : string;
begin
  if assigned(Synth) then begin
    if FPatchType = pftPatch then begin
      Synth.BankDumpFolder := ExtractFilePath(aFileName);
      Synth.BankDumpFileName := aFileName;
      Synth.BankDumpList.LoadFromFile( aFileName);

      Synth.BankDumpDestBank := rbeBank.Value;

      // Read bank dump list
      Synth.BankDumpListIndex := 0;
      if Synth.BankDumpList[Synth.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump' then
        raise Exception.Create('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

      if MessageDlg('Patch bank ' + IntToStr(Synth.BankDumpDestBank + 1)
                  + ' will be overwritten with patches from patch list ' + ExtractFileName(aFileName)
                  + ', sure?', TMsgDlgType.mtConfirmation, mbOKCancel, 0) = mrOk then
        Synth.NextPatchBankDownloadMessage( self);
    end else begin
      Synth.BankDumpFolder := ExtractFilePath(aFileName);
      Synth.BankDumpFileName := aFileName;
      Synth.BankDumpList.LoadFromFile( aFileName);

      Synth.BankDumpDestBank := rbeBank.Value;

      // Read bank dump list
      Synth.BankDumpListIndex := 0;
      if Synth.BankDumpList[Synth.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump' then
        raise Exception.Create('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

      if MessageDlg('Perf bank ' + IntToStr(Synth.BankDumpDestBank + 1)
                  + ' will be overwritten with performances from patch list ' + ExtractFileName(aFileName)
                  + ', sure?', TMsgDlgType.mtConfirmation, mbOKCancel, 0) = mrOk then
        Synth.NextPerfBankDownloadMessage( self);
    end;
  end;
end;

procedure TframeBanks.SaveBank(const aFilename : string);
begin
  if assigned(Synth) then begin
    Synth.BankDumpList.Clear;
    Synth.BankDumpList.Add('Version=Nord Modular G2 Bank Dump');
    case FPatchType of
      pftPatch :
        begin
          Synth.BankDumpFileName := 'PatchBank' + IntToStr(rbeBank.Value + 1) + '.pchList';
        end;
      pftPerf :
        begin
          Synth.BankDumpFileName := 'PerfBank' + IntToStr(rbeBank.Value + 1) + '.pchList';
        end;
      else
        raise Exception.Create('Unknown patch file type.');
    end;

    Synth.BankDumpFolder := ExtractFilePath( aFileName);
    Synth.SendUploadBankMessage( FPatchType, rbeBank.Value, 0);
  end;
end;


//==============================================================================
//
//                                    GUI
//
//==============================================================================

procedure TframeBanks.btClearChangeValue(Sender: TObject;
  const aValue: Integer);
var BankItem : TBankItem;
begin
  if aValue = 0 then begin
    BankItem := rbeSlot.ButtonText.Objects[ rbeSlot.Value] as TBankItem;
    if not assigned(BankItem) then
      exit;

    if MessageDlg('Clear slot?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
      exit;

    ClearLocation( FPatchType, BankItem.Bank, BankItem.Patch);
  end;
end;

procedure TframeBanks.btLoadBankChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  //
end;

procedure TframeBanks.btLoadPerfMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var filename : string;
begin
end;

procedure TframeBanks.btRetreiveChangeValue(Sender: TObject;
  const aValue: Integer);
var BankItem : TBankItem;
begin
  if aValue = 0 then begin
    BankItem := rbeSlot.ButtonText.Objects[ rbeSlot.Value] as TBankItem;
    if not assigned(BankItem) then
      exit;
    Retrieve( Synth.Performance.SelectedSlot, BankItem.Bank, BankItem.Patch);
  end;
end;

procedure TframeBanks.btSaveBankChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  //
end;

procedure TframeBanks.btStoreCurrentPatchChangeValue(Sender: TObject;
  const aValue: Integer);
var BankItem : TBankItem;
begin
  if aValue = 0 then begin
    BankItem := rbeSlot.ButtonText.Objects[ rbeSlot.Value] as TBankItem;
    if assigned(BankItem) then
      if MessageDlg('Replace patch?', TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrNo then
        exit;
    Store( Synth.Performance.SelectedSlot, rbeBank.Value, rbeSlot.Value);
  end;
end;

procedure TframeBanks.rbeBankChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateBanks;
end;

procedure TframeBanks.rbeSlotDblClick(Sender: TObject);
var BankItem : TBankItem;
begin
  if rbeSlot.Value >=0  then begin
    BankItem := rbeSlot.ButtonText.Objects[ rbeSlot.Value] as TBankItem;
    if not assigned(BankItem) then
      exit;
    Retrieve( Synth.Performance.SelectedSlot, BankItem.Bank, BankItem.Patch);
  end;
end;

procedure TframeBanks.rbeSlotPaintElement(Sender: TObject;
  const aElementType: TControlElementType; aElementIndex: Integer;
  aStyleSet: TG2StyleSet);
var BankItem : TBankItem;
begin
  if (aElementIndex <> rbeSlot.Value) and (aElementIndex < rbeSlot.ButtonText.Count) then begin
    BankItem := rbeSlot.ButtonText.Objects[aElementIndex] as TBankItem;
    if not assigned(BankItem) then
      exit;

    case aElementType of
      ceBackGround: aStyleSet.Fill.Color := ConvertToAlpha(ModuleColors[ BankItem.Category]);
    end;
  end;
end;

procedure TframeBanks.rbPatchTypeChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  UpdateBanks;
end;

procedure TframeBanks.SetStateStyles(aStateStyleList: TG2StateStyleList);
begin
  ComponentSetStateStylesRecursive(self, aStateStyleList);
end;

procedure TframeBanks.SetSynth(const Value: TG2GraphFMX);
begin
  if FSynth <> Value then begin
    if assigned(FSynth) then
      FSynth.RemoveObserver(self);

    FSynth := Value;

    if assigned(FSynth) then
      FSynth.RegisterObserver(self);

    UpdateControls;
  end;
end;

procedure TframeBanks.Update(aG2Event: TG2Event);
begin
  case aG2Event of
    EvtUSBActiveChange: ;
    EvtUSBError: ;
    EvtBeforeSendMessage: ;
    EvtReceiveResponseMessage: ;
    EvtNextInitStep: ;
    EvtAfterG2Init: ;
    EvtAfterPerfInit: ;
    EvtAfterSlotInit: ;
    EvtPerfsSettingsUpdate: ;
    EvtPerfUpdate: ;
    EvtSynthSettingsUpdate: ;
    EvtBeforePatchUpdate: ;
    EvtPatchUpdate: ;
    EvtVariationChange: ;
    EvtCopyVariation: ;
    EvtMidiClockReceive: ;
    EvtClockRunChange: ;
    EvtClockBPMChange: ;
    EvtMidiCCRecieve: ;
    EvtAfterGetAssignedVoices: ;
    EvtPatchLoadChange: ;
    EvtSelectSlot: ;
    EvtSelectLocation: ;
    EvtSelectModule: ;
    EvtSelectParam: ;
    EvtLabelValueChange: ;
    EvtMorphChange: ;
    EvtDeleteModule: ;
    EvtAfterRetreivePatch: ;
    EvtAfterBankList:
      begin
        UpdateControls;
      end;
    EvtAfterStore:
      begin
        UpdateControls;
      end;
    EvtAfterClear:
      begin
        UpdateControls;
      end;
    EvtAfterClearBank:
      begin
        UpdateControls;
      end;
    EvtAfterBankDownload:
      begin
        UpdateControls;
      end;
    EvtDeassignKnob: ;
    EvtAssignKnob: ;
    EvtDeassignGlobalKnob: ;
    EvtAssignGlobalKnob: ;
  end;
end;

procedure TframeBanks.UpdateBanks;
var BankIndex, SlotIndex, i : integer;
    BankItem : TBankItem;
begin
  if not assigned(Synth) then
    exit;

  BankIndex := rbeBank.Value;
  case rbPatchType.Value of
  0 : FPatchType := pftPatch;
  1 : FPatchType := pftPerf;
  end;

  rbeBank.ButtonText.Clear;
  for i := 0 to 31 do begin
    if (FPatchType = pftPerf) then begin
      if (i<8) then
        rbeBank.ButtonText.Add(IntToStr(i+1))
    end else
      rbeBank.ButtonText.Add(IntToStr(i+1));
  end;
  rbeBank.Redraw;

  rbeSlot.ButtonText.Clear;
  rbeSlot.ButtonRows := 16;
  rbeSlot.ButtonColumns := 8;
  // Add Empty
  for i := 0  to 127 do begin
    rbeSlot.ButtonText.Add( IntToStr(i) + ' ' + '..Empty..');
  end;

  for i := 0 to Synth.BankList.Count - 1 do begin
    BankItem := Synth.BankList[i];
    if (BankItem.Bank = BankIndex) and (BankItem.PatchFileType = FPatchType) then begin
      rbeSlot.ButtonText[ BankItem.Patch] := IntToStr(BankItem.Patch) + ' ' + BankItem.PatchName;
      rbeSlot.ButtonText.Objects[ BankItem.Patch] := BankItem;
    end;
  end;

  {i := 0;
  BankItem := Synth.BankList.FindNext( FPatchType, BankIndex, 0);
  if BankItem <> nil then begin
    while BankItem <> nil do begin
      rbeSlot.ButtonText[i] := IntToStr(BankItem.Patch) + ' ' + BankItem.PatchName;
      rbeSlot.ButtonText.Objects[i] := BankItem;
      i := BankItem.Patch;
      BankItem := Synth.BankList.FindNext( FPatchType, BankIndex, BankItem.Patch);
    end;
  end;}

  rbeSlot.Redraw;
end;

procedure TframeBanks.UpdateControls;
begin
  UpdateBanks;
end;

end.
