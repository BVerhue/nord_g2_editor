unit UnitG2EditorFMX;

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

{$INCLUDE Capabilities.inc}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, System.Contnrs, System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.Memo, FMX.Ani, FMX.Edit, FMX.Effects, FMX.Menus, FMX.StdCtrls,
  FMX.ActnList, FMX.TreeView, FMX.TabControl, System.Generics.Collections,
{$IFDEF MSWINDOWS}
  LibUSBWinDyn,
{$ENDIF}
{$IFDEF MACOS}
  libusb_dyn,
{$ENDIF}
  XMLDoc, XMLIntf, BVE.NMG2ParamDef, BVE.NMG2ModuleDef,
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2USB, BVE.NMG2GraphFMX,
  UnitSlot, UnitSlotStrip, BVE.NMG2ControlsFMX, FMX.Gestures,
  FMX.ListBox, UnitSynthStrip, UnitBanks, UnitAppSettings,
  UnitPatchSettings, UnitMessageDlg,
  UnitParam, UnitModule, UnitAddModule, UnitSynth, UnitEditorCentral,
  BVE.NMG2PathData;

type
  TfrmEditorMain = class(TForm)
    OpenDialog1: TOpenDialog;
    TimerStartup: TTimer;
    SaveDialog1: TSaveDialog;
    ControlStyles: TG2StateStyleList;
    DisplayBigStyles: TG2StateStyleList;
    frameEditorCentral: TframeEditorCentral;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerStartupTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FG2List : TObjectList<TG2GraphFMX>;
    FXMLModuleDefs : TXMLDocument;
    FXMLParamDefs  : TXMLDocument;
    FModuleDefList : IXMLModuleDefListType;
    FParamDefList  : IXMLParamDefListType;
    FModulePanelDefs : TModuleDefList;
    FframeMessageDlg : TframeMessageDlg;
  public
{$IFDEF MSWINDOWS}
    procedure AddSynth( aIndex : integer; G2USBDevice : pusb_device);
{$ELSE}
    procedure AddSynth( aIndex : integer; G2USBDevice : Plibusb_device);
{$ENDIF}
    procedure ShowMessageFrame( Sender : TObject; aMessage : string);
    procedure CloseMessageFrame( Sender : TObject);

    procedure GlobalExceptionHandler(Sender: TObject; E : Exception);
  end;

var
  frmEditorMain: TfrmEditorMain;

implementation

{$R *.fmx}

function MemoryUsed: cardinal;
{$IFDEF MSWINDOWS}
var st: TMemoryManagerState;
    sb: TSmallBlockTypeState;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  GetMemoryManagerState(st);
  result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do begin
    result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
  //frmEditorMain.eMemory.Text := IntToStr(result);
{$ENDIF}
end;

//==============================================================================
//
//                            TfrmEditorMain
//
//==============================================================================

procedure TfrmEditorMain.FormCreate(Sender: TObject);
var i : integer;
    G2DeviceList : TList;
    FModuleDefsFileName,
    FParamDefsFileName,
    path : string;
begin
  FormatSettings.DecimalSeparator := '.';

  Application.OnException := GlobalExceptionHandler;

  FG2List := TObjectList<TG2GraphFMX>.Create(True);

{$IFDEF MACOS}
  // Path to Executable
  path := ExtractFilePath(GetModuleName(0));
{$ELSE}
  path := '';
{$ENDIF}

  FXMLModuleDefs := TXMLDocument.Create(self);
  FXMLParamDefs := TXMLDocument.Create(self);

  FModuleDefsFileName := Path + 'ModuleDef_v3.xml';
  FParamDefsFileName := Path + 'ParamDef_v3.xml';

  if not(FileExists( FModuleDefsFileName)) then
    raise Exception.Create('Module definitions xml file not found : ' + FModuleDefsFileName);

  if not(FileExists( FParamDefsFileName)) then
    raise Exception.Create('Parameter definitions xml file not found : ' + FParamDefsFileName);

  //ShowMessage('Loading ' + FModuleDefsFileName);

  FXMLModuleDefs.FileName := FModuleDefsFileName;
  FXMLModuleDefs.Active := True;
  FModuleDefList := GetModuleDefList(FXMLModuleDefs);

  //ShowMessage('Loading ' + FParamDefsFileName);

  FXMLParamDefs.FileName := FParamDefsFileName;
  FXMLParamDefs.Active := True;
  FParamDefList := GEtParamDefList(FXMLParamDefs);

  //ShowMessage('Loading ' + Path + 'ModulePanelDefs.txt');

  FModulePanelDefs := TModuleDefList.Create;
  FModulePanelDefs.LoadModulePanelDefs(Path + 'ModulePanelDefs.txt');

  frameEditorCentral.SetStateStyles( ControlStyles);

  frameEditorCentral.sbSynths.BringToFront;

  frameEditorCentral.OnShowAppMessage := ShowMessageFrame;
  frameEditorCentral.OnCloseAppMessage := CloseMessageFrame;
  frameEditorCentral.ModulePanelDefs := FModulePanelDefs;
  frameEditorCentral.ModuleDefList := FModuleDefList;
  frameEditorCentral.ParamDefList := FParamDefList;

  //ShowMessage('Initializing libusb');

  G2DeviceList := TList.Create;
  try
    GetUSBDeviceList( G2DeviceList);

    //ShowMessage(IntToStr(G2DeviceList.Count) + ' devices found');

    if G2DeviceList.Count > 0 then begin
      for i := 0 to min(G2DeviceList.Count,3) - 1 do begin
{$IFDEF MSWINDOWS}
        AddSynth( i, pusb_device(G2DeviceList[i]));
{$ELSE}
        AddSynth( i, Plibusb_device(G2DeviceList[i]));
{$ENDIF}
      end;
    end else begin
      AddSynth(0, nil);
      ShowMessage('No g2 device found')
    end;
  finally
    G2DeviceList.Free;
  end;

  TimerStartup.Enabled := True;
  ShowMessageFrame(self, 'Starting application...');
end;

procedure TfrmEditorMain.FormDestroy(Sender: TObject);
var i : integer;
begin
  for i := 0 to FG2List.Count - 1 do begin
    FG2List[i].USBActive := False;
  end;

  FModulePanelDefs.Free;
  CloseMessageFrame(self);

  if assigned(FframeMessageDlg) then
    FframeMessageDlg.Free;
  FG2List.Free;
end;

procedure TfrmEditorMain.TimerStartupTimer(Sender: TObject);
var i : integer;
    pd : TG2PathDataList;
begin
  TimerStartup.Enabled := False;

  frameEditorCentral.Init;

  for i := 0 to FG2List.Count - 1 do begin
    ShowMessageFrame(self, 'Connecting to G2 nr. ' + IntToStr(i) + '...');
    FG2List[i].USBActive := True;
  end;

  CloseMessageFrame(self);
  MemoryUsed;
end;

procedure TfrmEditorMain.ShowMessageFrame(Sender: TObject; aMessage: string);
begin
  if not assigned(FframeMessageDlg) then begin
    FframeMessageDlg := TframeMessageDlg.Create(nil);
    FframeMessageDlg.Parent := Self;
    FframeMessageDlg.Align := TAlignLayout.alCenter;
  end;
  FframeMessageDlg.BringToFront;
  FframeMessageDlg.LabelMessage := aMessage;
  FframeMessageDlg.Repaint;
  Application.ProcessMessages;
end;

procedure TfrmEditorMain.CloseMessageFrame( Sender : TObject);
begin
  FreeAndNil(FframeMessageDlg);
end;

procedure TfrmEditorMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//{$IFDEF MACOS}
//  UnloadLIBUSB;
//{$ENDIF}
end;

procedure TfrmEditorMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  ShowMessageFrame(self, 'Waiting for message stream to stop...');
  CanClose := True;
end;

{$IFDEF MSWINDOWS}
procedure TfrmEditorMain.AddSynth( aIndex : integer; G2USBDevice: pusb_device);
{$ELSE}
procedure TfrmEditorMain.AddSynth( aIndex : integer; G2USBDevice: Plibusb_device);
{$ENDIF}
var path : string;
    G2 : TG2GraphFMX;
    i : integer;
begin
  G2 := TG2GraphFMX.Create(self);
  G2.ClientType := ctEditor;
  G2.Host := '127.0.0.1';
  G2.Port := 2501 + FG2List.Count;
  G2.IsServer := True;
  FG2List.Add( G2);

  //G2.ModulePanelDefs := FModulePanelDefs;

  //G2.ModuleDefList := FModuleDefList;
  //G2.ParamDefList := FParamDefList;
  G2.G2USBDevice := G2USBDevice;

  frameEditorCentral.AddSynth(aIndex, G2);
end;

procedure TfrmEditorMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  frameEditorCentral.ProcessKeyDown(Key, KeyChar, Shift);
end;

procedure TfrmEditorMain.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  frameEditorCentral.ProcessKeyUp(Key, KeyChar, Shift);
end;

procedure TfrmEditorMain.GlobalExceptionHandler(Sender: TObject; E: Exception);
begin
  if assigned(FframeMessageDlg) then
    FreeAndNil(FframeMessageDlg);

  MessageDlg('An error occured : ' + E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
end;

end.
