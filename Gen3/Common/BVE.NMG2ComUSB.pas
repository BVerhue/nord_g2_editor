unit BVE.NMG2ComUSB;

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
  System.Classes,
  System.Generics.Collections,
{$IF Defined(MSWINDOWS)}
  BVE.NMG2USBWin,
{$ENDIF}
{$IF Defined(MACOS)}
  BVE.NMG2USBunix,
{$ENDIF}
{$IF Defined(Android)}
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.USB,
  BVE.NMG2USBAndroid,
{$ENDIF}
  BVE.NMG2Types,
  BVE.NMG2Classes,
  BVE.NMG2FileIntf,
  BVE.NMG2Com;

type
  TG2USBConnection = class(TG2Connection)
  public
{$IF Defined(MSWINDOWS)}
    constructor Create(aConMan: IG2ConnectionManager);
{$ENDIF}
{$IF Defined(MACOS)}
    constructor Create(aConMan: IG2ConnectionManager; aUSBDevice: Plibusb_device);
{$ENDIF}
{$IF Defined(Android)}
    constructor Create(aConMan: IG2ConnectionManager);
{$ENDIF}
    destructor Destroy; override;

    procedure SendMsg(aMsg: TG2SendMessage); override;

    procedure ParamControllerAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMidiCC: byte); override;
    procedure ParamKnobAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte); override;
    procedure ParamGlobalKnobAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte); override;
    procedure ParamGlobalKnobDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte); override;
    procedure ParamKnobDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte); override;
    procedure ParamControllerDeassign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte); override;
    procedure ParamLabel(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aLabelIndex: byte; aLabel: string); override;
    procedure ParamSetValue(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aVariation, aValue: byte); override;
    procedure ParamSelect(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte); override;
    procedure ParamSetMorph(const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMorph, aValue, aNegative, aVariation: byte); override;

    procedure ModuleLabel(const aSlotIndex, aLocationIndex, aModuleIndex: byte;
      const aLabel: string); override;
    procedure ModuleColor(const aSlotIndex, aLocationIndex, aModuleIndex,
      aModuleColor: byte); override;
    procedure ModuleGlobalKnobsAssign(const aSlotIndex, aLocationIndex,
      aModuleIndex, aPageIndex: byte); override;
    procedure ModuleKnobsAssign(const aSlotIndex, aLocationIndex, aModuleIndex,
      aPageIndex: byte); override;

    procedure PatchSettings(aPatch: IG2Patch;
      aPatchSettings: IG2PatchSettings); override;
    procedure PatchModuleAdd(aPatchPart: IG2PatchPart;
      aModuleTypeID, aCol, aRow: byte); override;
    procedure PatchCableAdd(const aSlotIndex, aLocationIndex, aFromModuleIndex,
      aFromConnectorIndex: byte; const aFromConnectorKind: TConnectorKind;
      const aFromConnectorColor: TCableColor;
      const aToModuleIndex, aToConnectorIndex: byte;
      const aToConnectorKind: TConnectorKind;
      const aToConnectorColor: TCableColor); override;
    procedure PatchCableDelete(const aSlotIndex, aLocationIndex,
      aFromModuleIndex, aFromConnectorIndex: byte;
      const aFromConnectorKind: TConnectorKind;
      const aToModuleIndex, aToConnectorIndex: byte;
      const aToConnectorKind: TConnectorKind); override;
    procedure PatchModulesSelectedMove(aPatchPart: IG2PatchPart;
      aModuleChangeList: TModuleChangeList); override;
    procedure PatchModulesSelectedCopy(aDestPatchPart, aSrcePatchPart
      : IG2PatchPart); override;
    procedure PatchModulesSelectedDelete(aPatchPart: IG2PatchPart); override;
    procedure PatchParamsCopy(aDestPatchPart, aSrcePatchPart: IG2PatchPart;
      aFromVariation, aToVariation: byte); override;

    procedure SlotGetPatchVersion(const aSlotIndex: byte); override;
    procedure SlotGetPatch(const aSlotIndex: byte); override;
    procedure SlotGetPatchName(const aSlotIndex: byte); override;
    procedure SlotGetCurrentNote(const aSlotIndex: byte); override;
    procedure SlotGetPatchNotes(const aSlotIndex: byte); override;
    procedure SlotGetResourceTable(const aSlotIndex,
      aLocationIndex: byte); override;
    procedure SlotUnknown6(const aSlotIndex: byte); override;
    procedure SlotGetSelectedParam(const aSlotIndex: byte); override;
    procedure SlotSetPatchName(const aSlotIndex: byte;
      aPatchName: string); override;
    procedure SlotSetPatchCategory(const aSlotIndex, aCategory: byte); override;
    procedure SlotSetVoices(const aSlotIndex, aVoiceCount,
      aVoiceMode: byte); override;
    procedure SlotSelectVariation(const aSlotIndex, aVariation: byte); override;
    procedure SlotCopyVariation(const aSlotIndex, aFromVariation,
      aToVariation: byte); override;
    procedure SlotPatchLoad(const aSlotIndex: byte; const aPatchName: string;
      aPatch: IG2Patch); override;
    procedure SlotControllerSnapshot(const aSlotIndex: byte); override;
    procedure SlotVariationSelect(const aSlotIndex, aVariation: byte); override;

    procedure PerfGetSettings; override;
    procedure PerfSetSettings; override;
    procedure PerfSelectSlot(const aSlotIndex: byte); override;
    procedure PerfName(const aPerfName: string); override;
    procedure PerfUnknown2Message; override;
    procedure PerfGetGlobalKnobs; override;

    procedure SynthInit; override;
    procedure SynthStartStopCommunication(const aStop: byte); override;
    procedure SynthGetPatchVersion; override;
    procedure SynthGetMasterClock; override;
    procedure SynthUnknown1Message; override;
    procedure SynthGetAssignedVoices; override;
    procedure SynthGetList(const aPatchFileType: TPatchFileType;
      const aBank, aPatch: byte); override;
    procedure SynthRetreivePatch(const aSlotIndex, aBankIndex,
      aPatchIndex: byte); override;
    procedure SynthStorePatch(const aSlotIndex, aBankIndex,
      aPatchIndex: byte); override;
    procedure SynthBankClear(const PatchFileType: TPatchFileType;
      const aBank, aFromLocation, aToLocation: byte); override;
    procedure SynthPatchClear(const aPatchFileType: TPatchFileType;
      const aBank, aPatch: byte); override;
    procedure SynthBankUpload(const aPatchFileType: TPatchFileType;
      const aBank, aLocation: byte); override;
    procedure SynthBankPatchDownload(const aBank, aLocation: byte;
      const aPatchName: string; aPatch: IG2Patch); override;
    procedure SynthPerfLoad(const aPerfName: string; aPerf: IG2Perf); override;
    procedure SynthPerfMode(const aPerfMode: byte); override;
    procedure SynthNoteOnOff(const aNote, aOnOff: byte); override;
    procedure SynthMidiDump; override;
    procedure SynthSettings; override;
  end;

  TG2USBCommand = class(TG2Command)
  private
    FConnection: TG2USBConnection;
    FMsg: TG2SendMessage;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection); overload;
    constructor Create(aConnection: TG2USBConnection;
      aMsg: TG2SendMessage); overload;
    destructor Destroy; override;
  end;

  TG2USBUndoableCommand = class(TG2UndoableCommand)
  private
    FConnection: TG2USBConnection;
    FMsg, FUndoMsg: TG2SendMessage;
  protected
    procedure DoExecute; override;
    procedure DoRollback; override;
  public
    constructor Create(aConnection: TG2USBConnection);
    destructor Destroy; override;
  end;

  {TG2USBCmdSelectSlot = class(TG2USBCommand)
  private
    FSlotIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; aPerf: IG2Perf;
      const aSlotIndex: byte);
  end;

  TG2USBCmdPerfName = class(TG2USBCommand)
  private
    FPerfName: string;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aPerfName: string);
  end;

  TG2USBCmdPerfSettings = class(TG2USBCommand)
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection);
  end;}

  TG2USBPatchUndoableCommand = class(TG2USBUndoableCommand)
  private
    FSlotIndex: byte;
    FLocationIndex: byte;
    function GetPatch: IG2Patch;
    function GetPatchPart: IG2PatchPart;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex,
      aLocationIndex: byte);
    procedure Init;

    property Patch: IG2Patch read GetPatch;
    property PatchPart: IG2PatchPart read GetPatchPart;
  end;

  TG2USBCmdModuleAdd = class(TG2USBPatchUndoableCommand)
  private
    FModuleTypeID, FCol, FRow: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; aPatchPart: IG2PatchPart;
      const aModuleTypeID, aCol, aRow: byte);
  end;

  TG2USBCmdCableAdd = class(TG2USBPatchUndoableCommand)
  private
    FFromModuleIndex, FFromConnectorIndex: byte;
    FFromConnectorKind: TConnectorKind;
    FFromConnectorColor: TCableColor;
    FToModuleIndex, FToConnectorIndex: byte;
    FToConnectorKind: TConnectorKind;
    FToConnectorColor: TCableColor;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aFromModuleIndex, aFromConnectorIndex
      : byte; const aFromConnectorKind: TConnectorKind;
      const aFromConnectorColor: TCableColor;
      const aToModuleIndex, aToConnectorIndex: byte;
      const aToConnectorKind: TConnectorKind;
      const aToConnectorColor: TCableColor);
  end;

  TG2USBCmdCableDelete = class(TG2USBPatchUndoableCommand)
  private
    FFromModuleIndex, FFromConnectorIndex: byte;
    FFromConnectorKind: TConnectorKind;
    FToModuleIndex, FToConnectorIndex: byte;
    FToConnectorKind: TConnectorKind;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aFromModuleIndex, aFromConnectorIndex
      : byte; const aFromConnectorKind: TConnectorKind;
      const aToModuleIndex, aToConnectorIndex: byte;
      const aToConnectorKind: TConnectorKind);
  end;

  TG2USBCmdModuleColor = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FModuleColor: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aModuleIndex, aModuleColor: byte);
  end;

  TG2USBCmdModuleLabel = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FModuleLabel: string;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aModuleIndex: byte;
      const aLabel: string);
  end;

  TG2USBCmdModuleKnobAssign = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FPageIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aModuleIndex, aPageIndex: byte);
  end;

  TG2USBCmdModuleGlobalKnobAssign = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FPageIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      const aSlotIndex, aLocationIndex, aModuleIndex, aPageIndex: byte);
  end;

  TG2USBCmdModulesMove = class(TG2USBPatchUndoableCommand)
  private
    FModuleChangeList: TModuleChangeList;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; aPatchPart: IG2PatchPart;
      aModuleChangeList: TModuleChangeList);
    destructor Destroy; override;
  end;

  TG2USBCmdPatchPartAdd = class(TG2USBPatchUndoableCommand)
  private
    FSrcePatchPart: IG2PatchPart;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      aDestPatchPart, aSrcePatchPart: IG2PatchPart);
    destructor Destroy; override;
  end;

  TG2USBCmdPatchPartDelete = class(TG2USBPatchUndoableCommand)
  private
    FModuleChangeList: TModuleChangeList;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; aPatchPart: IG2PatchPart);
    destructor Destroy; override;
  end;

  TG2USBCmdPatchParamsCopy = class(TG2USBPatchUndoableCommand)
  private
    FSrcePatchPart: IG2PatchPart;
    FFromVariation, FToVariation: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection;
      aDestPatchPart, aSrcePatchPart: IG2PatchPart;
      aFromVariation, aToVariation: byte);
    destructor Destroy; override;
  end;

  TG2USBCmdPatchSettings = class(TG2USBPatchUndoableCommand)
  private
    FPatchSettings: IG2PatchSettings;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; aPatch: IG2Patch;
      aPatchSettings: IG2PatchSettings);
    destructor Destroy; override;
  end;

  TG2USBCmdParamKnobAssign = class(TG2USBPatchUndoableCommand)
  private
    FKnobIndex: byte;
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte);
  end;

  TG2USBCmdParamKnobDeassign = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
  end;

  TG2USBCmdParamGlobalKnobAssign = class(TG2USBPatchUndoableCommand)
  private
    FKnobIndex: byte;
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aKnobIndex: byte);
  end;

  TG2USBCmdParamGlobalKnobDeassign = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
  end;

  TG2USBCmdParamControllerAssign = class(TG2USBPatchUndoableCommand)
  private
    FMidiCC: byte;
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aMidiCC: byte);
  end;

  TG2USBCmdParamControllerDeassign = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FParamIndex: byte;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex: byte);
  end;

  TG2USBCmdParamLabel = class(TG2USBPatchUndoableCommand)
  private
    FModuleIndex: byte;
    FParamIndex: byte;
    FLabelIndex: byte;
    FLabel: string;
  protected
    procedure DoExecute; override;
  public
    constructor Create(aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex, aModuleIndex,
      aParamIndex, aLabelIndex: byte; aLabel: string);
  end;

implementation
uses
  BVE.NMG2PatchPart;

// ------------------------------------------------------------------------------
//
// TG2USBConnection
//
// ------------------------------------------------------------------------------
{$IF Defined(MSWINDOWS)}
constructor TG2USBConnection.Create(aConMan: IG2ConnectionManager);
begin
  inherited Create(aConMan);
end;
{$ENDIF}
{$IF Defined(MACOS)}
constructor TG2USBConnection.Create(aConMan: IG2ConnectionManager;
  aUSBDevice: Plibusb_device);
begin
  inherited Create(aConMan);
end;
{$ENDIF}
{$IF Defined(Android)}
constructor TG2USBConnection.Create(aConMan: IG2ConnectionManager);
begin
  inherited Create(aConMan);
end;
{$ENDIF}

destructor TG2USBConnection.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
//
//                                 Commands
//
// ------------------------------------------------------------------------------

procedure TG2USBConnection.ModuleColor(const aSlotIndex, aLocationIndex,
  aModuleIndex, aModuleColor: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModuleColor.Create(self, aSlotIndex, aLocationIndex,
    aModuleIndex, aModuleColor));
end;

procedure TG2USBConnection.ModuleGlobalKnobsAssign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aPageIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModuleGlobalKnobAssign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aPageIndex));
end;

procedure TG2USBConnection.ModuleKnobsAssign(const aSlotIndex, aLocationIndex,
  aModuleIndex, aPageIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModuleKnobAssign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aPageIndex));
end;

procedure TG2USBConnection.ModuleLabel(const aSlotIndex, aLocationIndex,
  aModuleIndex: byte; const aLabel: string);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModuleLabel.Create(self, aSlotIndex, aLocationIndex,
    aModuleIndex, aLabel));
end;

procedure TG2USBConnection.ParamControllerAssign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex, aMidiCC: byte);
begin
  inherited;

  ExecuteCommand( TG2USBCmdParamControllerAssign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aParamIndex, aMidiCC));
end;

procedure TG2USBConnection.ParamControllerDeassign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamControllerDeassign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aParamIndex));
end;

procedure TG2USBConnection.ParamGlobalKnobAssign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex, aKnobIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamGlobalKnobAssign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aParamIndex, aKnobIndex));
end;

procedure TG2USBConnection.ParamGlobalKnobDeassign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamGlobalKnobDeassign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aParamIndex));
end;

procedure TG2USBConnection.ParamKnobAssign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex, aKnobIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamKnobAssign.Create(self, aSlotIndex, aLocationIndex,
    aModuleIndex, aParamIndex, aKnobIndex));
end;

procedure TG2USBConnection.ParamKnobDeassign(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamKnobDeassign.Create(self, aSlotIndex,
    aLocationIndex, aModuleIndex, aParamIndex));
end;

procedure TG2USBConnection.ParamLabel(const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex, aLabelIndex: byte; aLabel: string);
begin
  inherited;

  ExecuteCommand(TG2USBCmdParamLabel.Create(self, aSlotIndex, aLocationIndex,
    aModuleIndex, aParamIndex, aLabelIndex, aLabel));
end;

procedure TG2USBConnection.ParamSetValue(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex, aVariation, aValue: byte);
begin
  if assigned(Client) then
    Client.AddParamUpdRec(aSlotIndex, Slot[aSlotIndex].PatchVersion,
      S_SET_PARAM, aLocationIndex, aModuleIndex, aParamIndex, 0, aValue, 0,
      aVariation);
end;

procedure TG2USBConnection.ParamSelect(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex: byte);
begin
  if assigned(Client) then
    Client.AddParamUpdRec(aSlotIndex, Slot[aSlotIndex].PatchVersion,
      S_SEL_PARAM, aLocationIndex, aModuleIndex, aParamIndex, 0, 0, 0, 0);
end;

procedure TG2USBConnection.ParamSetMorph(const aSlotIndex, aLocationIndex,
  aModuleIndex, aParamIndex, aMorph, aValue, aNegative, aVariation: byte);
begin
  if assigned(Client) then
    Client.AddParamUpdRec(aSlotIndex, Slot[aSlotIndex].PatchVersion,
      S_SET_MORPH_RANGE, aLocationIndex, aModuleIndex, aParamIndex, aMorph,
      aValue, aNegative, aVariation);
end;

procedure TG2USBConnection.PatchCableAdd(const aSlotIndex, aLocationIndex,
  aFromModuleIndex, aFromConnectorIndex: byte;
  const aFromConnectorKind: TConnectorKind;
  const aFromConnectorColor: TCableColor;
  const aToModuleIndex, aToConnectorIndex: byte;
  const aToConnectorKind: TConnectorKind; const aToConnectorColor: TCableColor);
begin
  inherited;

  ExecuteCommand(TG2USBCmdCableAdd.Create(self, aSlotIndex, aLocationIndex,
    aFromModuleIndex, aFromConnectorIndex, aFromConnectorKind,
    aFromConnectorColor, aToModuleIndex, aToConnectorIndex, aToConnectorKind,
    aToConnectorColor));
end;

procedure TG2USBConnection.PatchCableDelete(const aSlotIndex, aLocationIndex,
  aFromModuleIndex, aFromConnectorIndex: byte;
  const aFromConnectorKind: TConnectorKind;
  const aToModuleIndex, aToConnectorIndex: byte;
  const aToConnectorKind: TConnectorKind);
begin
  inherited;

  ExecuteCommand(TG2USBCmdCableDelete.Create(self, aSlotIndex, aLocationIndex,
    aFromModuleIndex, aFromConnectorIndex, aFromConnectorKind, aToModuleIndex,
    aToConnectorIndex, aToConnectorKind));
end;

procedure TG2USBConnection.PatchSettings(aPatch: IG2Patch;
  aPatchSettings: IG2PatchSettings);
begin
  inherited;

  ExecuteCommand(TG2USBCmdPatchSettings.Create(self, aPatch, aPatchSettings));
end;

procedure TG2USBConnection.PatchModuleAdd(aPatchPart: IG2PatchPart;
  aModuleTypeID, aCol, aRow: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModuleAdd.Create(self, aPatchPart, aModuleTypeID,
    aCol, aRow));
end;

procedure TG2USBConnection.PatchModulesSelectedCopy(aDestPatchPart,
  aSrcePatchPart: IG2PatchPart);
begin
  inherited;

  ExecuteCommand(TG2USBCmdPatchPartAdd.Create(self, aDestPatchPart,
    aSrcePatchPart));
end;

procedure TG2USBConnection.PatchModulesSelectedDelete(aPatchPart: IG2PatchPart);
begin
  inherited;

  ExecuteCommand(TG2USBCmdPatchPartDelete.Create(self, aPatchPart));
end;

procedure TG2USBConnection.PatchModulesSelectedMove(aPatchPart: IG2PatchPart;
  aModuleChangeList: TModuleChangeList);
begin
  inherited;

  ExecuteCommand(TG2USBCmdModulesMove.Create(self, aPatchPart,
    aModuleChangeList));
end;

procedure TG2USBConnection.PatchParamsCopy(aDestPatchPart, aSrcePatchPart
  : IG2PatchPart; aFromVariation, aToVariation: byte);
begin
  inherited;

  ExecuteCommand(TG2USBCmdPatchParamsCopy.Create(self, aDestPatchPart,
    aSrcePatchPart, aFromVariation, aToVariation));
end;

procedure TG2USBConnection.SlotGetPatchVersion(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].AddMsgGetPatchVersion(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetPatch(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetPatch(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetPatchName(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetPatchName(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetCurrentNote(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetCurrentNote(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetPatchNotes(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetPatchNotes(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetResourceTable(const aSlotIndex,
  aLocationIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetResourceTable(Cmd.FMsg, aLocationIndex);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotUnknown6(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgUnknown6(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotGetSelectedParam(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgGetSelectedParam(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotSetPatchCategory(const aSlotIndex,
  aCategory: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgControllerSnapshot(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotSetVoices(const aSlotIndex, aVoiceCount,
  aVoiceMode: byte);
begin

end;

procedure TG2USBConnection.SlotSelectVariation(const aSlotIndex,
  aVariation: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgSelectVariation(aVariation, Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotSetPatchName(const aSlotIndex: byte;
  aPatchName: string);
begin
end;

procedure TG2USBConnection.SlotCopyVariation(const aSlotIndex, aFromVariation,
  aToVariation: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgCopyVariation(aFromVariation, aToVariation,
    Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotPatchLoad(const aSlotIndex: byte;
  const aPatchName: string; aPatch: IG2Patch);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].AddMsgPatchLoad(Cmd.FMsg, aPatchName, aPatch);
  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SendMsg(aMsg: TG2SendMessage);
begin
  if assigned(Client) then
    Client.SendMsg(aMsg)
  else
    if (aMsg.HasResponse) then begin
      aMsg.Position := 0;
      DoProcessSendMsg(aMsg);
    end;
end;

procedure TG2USBConnection.SlotControllerSnapshot(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgControllerSnapshot(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SlotVariationSelect(const aSlotIndex,
  aVariation: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.Slot[aSlotIndex].InitMsg(Cmd.FMsg);
  Perf.Slot[aSlotIndex].AddMsgSelectVariation(aVariation, Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfName(const aPerfName: string);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgSetPerfName(Cmd.FMsg, aPerfName);
  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfSelectSlot(const aSlotIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgSelectSlot(Cmd.FMsg, aSlotIndex);
  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfGetSettings;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgGetPerfSettings(Cmd.FMsg);
  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfSetSettings;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgSetPerfSettings(Cmd.FMsg);
  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfUnknown2Message;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgUnknown2(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.PerfGetGlobalKnobs;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Perf.InitMsg(Cmd.FMsg);
  Perf.AddMsgGetGlobalKnobs(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthBankClear(const PatchFileType: TPatchFileType;
  const aBank, aFromLocation, aToLocation: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);
  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgClearBankMessage(Cmd.FMsg, PatchFileType, aBank, aFromLocation,
    aToLocation);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthBankPatchDownload(const aBank, aLocation: byte;
  const aPatchName: string; aPatch: IG2Patch);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgDownloadPatchBankMessage(Cmd.FMsg, aBank, aLocation,
    aPatchName, aPatch);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthBankUpload(const aPatchFileType: TPatchFileType;
  const aBank, aLocation: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgUploadBankMessage(Cmd.FMsg, aPatchFileType, aBank, aLocation);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthInit;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgInit(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthStartStopCommunication(const aStop: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgStartStopCommunication(Cmd.FMsg, aStop);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthGetPatchVersion;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgGetPatchVersion(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthUnknown1Message;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgUnknown1Message(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthGetAssignedVoices;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgGetAssignedVoices(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthGetMasterClock;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgGetMasterClockMessage(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthGetList(const aPatchFileType: TPatchFileType;
  const aBank, aPatch: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgListMessage(Cmd.FMsg, aPatchFileType, aBank, aPatch);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthMidiDump;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgMidiDumpMessage(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthPatchClear(const aPatchFileType: TPatchFileType;
  const aBank, aPatch: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgClearMessage(Cmd.FMsg, aPatchFileType, aBank, aPatch);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthPerfLoad(const aPerfName: string;
  aPerf: IG2Perf);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgSetPerformance(Cmd.FMsg, aPerfName, aPerf);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthPerfMode(const aPerfMode: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgSetModeMessage(Cmd.FMsg, aPerfMode);

  ExecuteCommand(Cmd);

  Perf.StartInit(Self, True);
end;

procedure TG2USBConnection.SynthNoteOnOff(const aNote, aOnOff: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgNoteMessage(Cmd.FMsg, aNote, aOnOff);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthRetreivePatch(const aSlotIndex, aBankIndex,
  aPatchIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgRetrieveMessage(Cmd.FMsg, aSlotIndex, aBankIndex, aPatchIndex);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthSettings;
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgGetSynthSettings(Cmd.FMsg);

  ExecuteCommand(Cmd);
end;

procedure TG2USBConnection.SynthStorePatch(const aSlotIndex, aBankIndex,
  aPatchIndex: byte);
var
  Cmd: TG2USBCommand;
begin
  Cmd := TG2USBCommand.Create(self);

  Synth.InitMsg(Cmd.FMsg);
  Synth.AddMsgStoreMessage(Cmd.FMsg, aSlotIndex, aBankIndex, aPatchIndex);

  ExecuteCommand(Cmd);
end;

{ Tprocedure TG2USBConnection.SynthUnknown1Message;
  begin
  inherited;

  end;

  procedure TG2USBConnection.USBDone;
  begin

  end;

  procedure TG2USBConnection.USBInit;
  begin

  end;

  procedure TG2USBConnection.USBDone;
  begin

  end;

  procedure TG2USBConnection.USBInit;
  begin

  end;

  procedure TG2USBConnection.USBDone;
  begin

  end;

  procedure TG2USBConnection.USBInit;
  begin

  end;

  G2USBCommand }

constructor TG2USBCommand.Create(aConnection: TG2USBConnection);
begin
  inherited Create;

  FConnection := aConnection;
  FMsg := TG2SendMessage.Create;
end;

constructor TG2USBCommand.Create(aConnection: TG2USBConnection;
  aMsg: TG2SendMessage);
begin
  inherited Create;

  FConnection := aConnection;
  FMsg := aMsg;
end;

destructor TG2USBCommand.Destroy;
begin
  FMsg.Free;

  inherited;
end;

procedure TG2USBCommand.DoExecute;
begin
  inherited;

  FConnection.SendMsg(FMsg);
end;

{ TG2USBUndoableCommand }

constructor TG2USBUndoableCommand.Create(aConnection: TG2USBConnection);
begin
  inherited Create;

  FConnection := aConnection;

  FMsg := TG2SendMessage.Create;
  FUndoMsg := TG2SendMessage.Create;
end;

destructor TG2USBUndoableCommand.Destroy;
begin
  FUndoMsg.Free;
  FMsg.Free;

  inherited;
end;

procedure TG2USBUndoableCommand.DoExecute;
begin
  inherited;
  FConnection.SendMsg(FMsg);
end;

procedure TG2USBUndoableCommand.DoRollback;
begin
  inherited;
  FConnection.SendMsg(FUndoMsg);
end;

{ TG2USBPatchUndoableCommand }

constructor TG2USBPatchUndoableCommand.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex: byte);
begin
  inherited Create(aConnection);

  FSlotIndex := aSlotIndex;
  FLocationIndex := aLocationIndex;
end;

function TG2USBPatchUndoableCommand.GetPatch: IG2Patch;
begin
  Result := FConnection.Patch[FSlotIndex];
end;

function TG2USBPatchUndoableCommand.GetPatchPart: IG2PatchPart;
begin
  Result := FConnection.PatchPart[FSlotIndex, FLocationIndex]
end;

procedure TG2USBPatchUndoableCommand.Init;
begin
  FConnection.Slot[FSlotIndex].InitMsg(FMsg);
  FConnection.Slot[FSlotIndex].InitUndoMsg(FUndoMsg);
end;

{ TG2USBCmdModuleAdd }

constructor TG2USBCmdModuleAdd.Create(aConnection: TG2USBConnection;
  aPatchPart: IG2PatchPart; const aModuleTypeID, aCol, aRow: byte);
begin
  inherited Create(aConnection, aPatchPart.SlotIndex, aPatchPart.LocationIndex);

  FModuleTypeID := aModuleTypeID;
  FCol := aCol;
  FRow := aRow;
end;

procedure TG2USBCmdModuleAdd.DoExecute;
begin
  Init;
  PatchPart.AddMessAddModule(FModuleTypeID, FCol, FRow, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdModuleColor }

constructor TG2USBCmdModuleColor.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aModuleColor: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FModuleColor := aModuleColor;
end;

procedure TG2USBCmdModuleColor.DoExecute;
var
  Module: IG2Module;
begin
  Init;
  Module := PatchPart.FindModule(FModuleIndex);
  Module.AddMessModuleColor(FModuleColor, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdModuleLabel }

constructor TG2USBCmdModuleLabel.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex: byte; const aLabel: string);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FModuleLabel := aLabel;
end;

procedure TG2USBCmdModuleLabel.DoExecute;
var
  Module: IG2Module;
begin
  Init;
  Module := PatchPart.FindModule(FModuleIndex);
  Module.AddMessModuleLabel(FModuleLabel, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdCableAdd }

constructor TG2USBCmdCableAdd.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aFromModuleIndex, aFromConnectorIndex: byte;
  const aFromConnectorKind: TConnectorKind;
  const aFromConnectorColor: TCableColor;
  const aToModuleIndex, aToConnectorIndex: byte;
  const aToConnectorKind: TConnectorKind; const aToConnectorColor: TCableColor);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FFromModuleIndex := aFromModuleIndex;
  FFromConnectorIndex := aFromConnectorIndex;
  FFromConnectorKind := aFromConnectorKind;
  FFromConnectorColor := aFromConnectorColor;
  FToModuleIndex := aToModuleIndex;
  FToConnectorIndex := aToConnectorIndex;
  FToConnectorKind := aToConnectorKind;
  FToConnectorColor := aToConnectorColor;
end;

procedure TG2USBCmdCableAdd.DoExecute;
begin
  Init;
  PatchPart.AddMessAddCable(FFromModuleIndex, FFromConnectorKind,
    FFromConnectorIndex, ord(FFromConnectorColor), FToModuleIndex,
    FToConnectorKind, FToConnectorIndex, ord(FToConnectorColor), FMsg,
    FUndoMsg);

  inherited;
end;

{ TG2USBCmdCableDelete }

constructor TG2USBCmdCableDelete.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aFromModuleIndex, aFromConnectorIndex: byte;
  const aFromConnectorKind: TConnectorKind;
  const aToModuleIndex, aToConnectorIndex: byte;
  const aToConnectorKind: TConnectorKind);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FFromModuleIndex := aFromModuleIndex;
  FFromConnectorIndex := aFromConnectorIndex;
  FToModuleIndex := aToModuleIndex;
  FToConnectorIndex := aToConnectorIndex;
  FFromConnectorKind := aFromConnectorKind;
  FToConnectorKind := aToConnectorKind;
end;

procedure TG2USBCmdCableDelete.DoExecute;
var
  Cable: IG2Cable;
begin
  inherited;

  Init;

  Cable := PatchPart.FindCable(FFromModuleIndex, FFromConnectorIndex,
    FToModuleIndex, FToConnectorIndex);

  if assigned(Cable) then
    PatchPart.AddMessDeleteCable(Cable, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdModuleKnobAssign }

constructor TG2USBCmdModuleKnobAssign.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aPageIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FPageIndex := aPageIndex;

end;

procedure TG2USBCmdModuleKnobAssign.DoExecute;
var
  Module: IG2Module;
begin
  Init;
  Module := PatchPart.FindModule(FModuleIndex);
  PatchPart.Patch.AddMessAssignKnobs(Module, FPageIndex, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdModuleGlobalKnobAssign }

constructor TG2USBCmdModuleGlobalKnobAssign.Create
  (aConnection: TG2USBConnection; const aSlotIndex, aLocationIndex,
  aModuleIndex, aPageIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FPageIndex := aPageIndex;
end;

procedure TG2USBCmdModuleGlobalKnobAssign.DoExecute;
var
  Module: IG2Module;
begin
  Init;
  Module := PatchPart.FindModule(FModuleIndex);
  PatchPart.Patch.Perf.AddMessAssignGlobalKnobs(Module, FPageIndex, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdModulesMove }

constructor TG2USBCmdModulesMove.Create(aConnection: TG2USBConnection;
  aPatchPart: IG2PatchPart; aModuleChangeList: TModuleChangeList);
begin
  inherited Create(aConnection, aPatchPart.SlotIndex, aPatchPart.LocationIndex);

  FModuleChangeList := TModuleChangeList.CopyCreate(aModuleChangeList);
end;

destructor TG2USBCmdModulesMove.Destroy;
begin
  FModuleChangeList.Free;
  inherited;
end;

procedure TG2USBCmdModulesMove.DoExecute;
var
  ExistingModuleChangeList: TModuleChangeList;
  Module: IG2Module;
begin
  ExistingModuleChangeList := TModuleChangeList.Create;
  try
    Init;
    for Module in PatchPart.ModuleList do
    begin
      if FModuleChangeList.FindModuleChange(Module.ModuleIndex) = nil then
      begin
        ExistingModuleChangeList.Add(TModuleChange.Create(
          Module.ModuleIndex,
          Module.ModuleName,
          Module.ModuleFileName,
          Module.Row,
          Module.Col,
          Module.HeightUnits));
      end;
    end;

    ExistingModuleChangeList.MergeList(FModuleChangeList);

    ExistingModuleChangeList.AddMessages(FLocationIndex, FMsg, FUndoMsg);

    inherited;
  finally
    ExistingModuleChangeList.Free;
  end;
end;

{ TG2USBCmdPatchPartAdd }

constructor TG2USBCmdPatchPartAdd.Create(aConnection: TG2USBConnection;
  aDestPatchPart, aSrcePatchPart: IG2PatchPart);
begin
  inherited Create(aConnection, aDestPatchPart.SlotIndex,
    aDestPatchPart.LocationIndex);

  FSrcePatchPart := aSrcePatchPart;
end;

destructor TG2USBCmdPatchPartAdd.Destroy;
begin
  FSrcePatchPart := nil;
  inherited;
end;

procedure TG2USBCmdPatchPartAdd.DoExecute;
begin
  Init;

  // Select the modules in the srce patchpart
  // FSrcePatchPart.SelectAll;
  PatchPart.AddMessAddPatchPart(FSrcePatchPart, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdPatchPartDelete }

constructor TG2USBCmdPatchPartDelete.Create(aConnection: TG2USBConnection;
  aPatchPart: IG2PatchPart);
var
  Module: IG2Module;
  SelectedModuleList: TList<IG2Module>;
begin
  inherited Create(aConnection, aPatchPart.SlotIndex, aPatchPart.LocationIndex);

  FModuleChangeList := TModuleChangeList.Create;

  SelectedModuleList := aPatchPart.CreateSelectedModuleList;
  try
    for Module in SelectedModuleList do
      FModuleChangeList.Add(TModuleChange.Create(
        Module.ModuleIndex,
        Module.ModuleName,
        Module.ModuleFileName,
        Module.Row,
        Module.Col,
        Module.HeightUnits));
  finally
    SelectedModuleList.Free;
  end;
end;

destructor TG2USBCmdPatchPartDelete.Destroy;
begin
  FModuleChangeList.Free;
  inherited;
end;

procedure TG2USBCmdPatchPartDelete.DoExecute;
var
  ModuleChange: TModuleChange;
begin
  Init;

  PatchPart.UnselectModules;
  for ModuleChange in FModuleChangeList do
  begin
    PatchPart.SelectModule(ModuleChange.OldModuleIndex, True, False);
  end;

  PatchPart.AddMessDeleteModules(FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdPatchParamsCopy }

constructor TG2USBCmdPatchParamsCopy.Create(aConnection: TG2USBConnection;
  aDestPatchPart, aSrcePatchPart: IG2PatchPart;
  aFromVariation, aToVariation: byte);
begin
  inherited Create(aConnection, aDestPatchPart.SlotIndex,
    aDestPatchPart.LocationIndex);

  FSrcePatchPart := aSrcePatchPart;
  FFromVariation := aFromVariation;
  FToVariation := aToVariation;
end;

destructor TG2USBCmdPatchParamsCopy.Destroy;
begin
  FSrcePatchPart := nil;

  inherited;
end;

procedure TG2USBCmdPatchParamsCopy.DoExecute;
begin
  Init;

  FSrcePatchPart.SelectAll;

  // Create the message and the undo message for adding the parameter values
  PatchPart.AddMessPasteParams(FSrcePatchPart, FFromVariation, FToVariation,
    FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamKnobAssign }

constructor TG2USBCmdParamKnobAssign.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aKnobIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
  FKnobIndex := aKnobIndex;
end;

procedure TG2USBCmdParamKnobAssign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
  Knob: IG2Knob;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Knob := Patch.KnobList[FKnobIndex];

  Param.AddMessAssignKnob(Knob, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamKnobDeassign }

constructor TG2USBCmdParamKnobDeassign.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
end;

procedure TG2USBCmdParamKnobDeassign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Param.AddMessDeassignKnob(FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamGlobalKnobAssign }

constructor TG2USBCmdParamGlobalKnobAssign.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aKnobIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
  FKnobIndex := aKnobIndex;
end;

procedure TG2USBCmdParamGlobalKnobAssign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
  Knob: IG2Knob;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Knob := FConnection.Perf.GlobalKnobList[FKnobIndex];

  Param.AddMessAssignGlobalKnob(Knob, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamGlobalKnobDeassign }

constructor TG2USBCmdParamGlobalKnobDeassign.Create
  (aConnection: TG2USBConnection; const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
end;

procedure TG2USBCmdParamGlobalKnobDeassign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Param.AddMessDeassignGlobalKnob(FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamControllerAssign }

constructor TG2USBCmdParamControllerAssign.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aMidiCC: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
  FMidiCC := aMidiCC;
end;

procedure TG2USBCmdParamControllerAssign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Param.AddMessAssignController(FMidiCC, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamControllerDeassign }

constructor TG2USBCmdParamControllerDeassign.Create
  (aConnection: TG2USBConnection; const aSlotIndex,
  aLocationIndex, aModuleIndex, aParamIndex: byte);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
end;

procedure TG2USBCmdParamControllerDeassign.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Param.AddMessDeassignController(FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdParamLabel }

constructor TG2USBCmdParamLabel.Create(aConnection: TG2USBConnection;
  const aSlotIndex, aLocationIndex, aModuleIndex, aParamIndex, aLabelIndex: byte; aLabel: string);
begin
  inherited Create(aConnection, aSlotIndex, aLocationIndex);

  FModuleIndex := aModuleIndex;
  FParamIndex := aParamIndex;
  FLabelIndex := aLabelIndex;
  FLabel := aLabel;
end;

procedure TG2USBCmdParamLabel.DoExecute;
var
  Module: IG2Module;
  Param: IG2Param;
begin
  Init;

  Module := PatchPart.FindModule(FModuleIndex);
  if not assigned(Module) then
    exit;

  Param := Module.Param[FParamIndex];

  Module.AddMessParamLabel(Param, FLabelIndex, FLabel, FMsg, FUndoMsg);

  inherited;
end;

{ TG2USBCmdPatchSettings }

constructor TG2USBCmdPatchSettings.Create(aConnection: TG2USBConnection;
  aPatch: IG2Patch; aPatchSettings: IG2PatchSettings);
begin
  inherited Create(aConnection, aPatch.SlotIndex, LOCATION_PATCH);

  FPatchSettings := aPatchSettings;
end;

destructor TG2USBCmdPatchSettings.Destroy;
begin
  FPatchSettings := nil;

  inherited;
end;

procedure TG2USBCmdPatchSettings.DoExecute;
begin
  Init;

  Patch.AddMessPatchSettings(FPatchSettings, FMsg, FUndoMsg);

  inherited;
end;

end.
