unit UnitG2EditorFMX_FM;

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
  FMX.ActnList, FMX.TreeView, FMX.TabControl, FMX.Graphics,
  System.Generics.Collections,
  BVE.NMG2Types, BVE.NMG2File, BVE.NMG2USB, BVE.NMG2GraphFMX,
  UnitSlot, UnitSlotStrip, BVE.NMG2ControlsFMX, FMX.Gestures,
  FMX.ListBox, UnitBanks, UnitAppSettings,
  UnitPatchSettings, UnitMessageDlg,
  UnitParam, UnitModule, UnitAddModule,
  BVE.NMG2ColorScheme, xmldom, XMLDoc, XMLIntf,
  BVE.NMG2PathData, UnitEditorCentral2;

type
  TfrmEditorMain = class(TForm)
    OpenDialog1: TOpenDialog;
    TimerStartup: TTimer;
    SaveDialog1: TSaveDialog;
    ControlStyles: TG2StateStyleList;
    frameEditorCentralStrip: TframeEditorCentralStrip;
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
    FframeMessageDlg : TframeMessageDlg;
  public
    procedure ShowMessageFrame( Sender : TObject; aMessage : string);
    procedure CloseMessageFrame( Sender : TObject);

    procedure LoadColorScheme;
    procedure SaveColorScheme;

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

  LoadColorScheme;
  //SaveColorScheme;

  frameEditorCentralStrip.SetStateStyles( ControlStyles);

  frameEditorCentralStrip.OnShowAppMessage := ShowMessageFrame;
  frameEditorCentralStrip.OnCloseAppMessage := CloseMessageFrame;

  frameEditorCentralStrip.DeviceDiscovery;

  TimerStartup.Enabled := True;
  ShowMessageFrame(self, 'Starting application...');
end;

procedure TfrmEditorMain.FormDestroy(Sender: TObject);
begin
  CloseMessageFrame(self);

  if assigned(FframeMessageDlg) then
    FframeMessageDlg.Free;
end;

procedure TfrmEditorMain.TimerStartupTimer(Sender: TObject);
var i : integer;
    pd : TG2PathDataList;
begin
  TimerStartup.Enabled := False;

  frameEditorCentralStrip.Init;

  for i := 0 to frameEditorCentralStrip.G2List.Count - 1 do begin
    ShowMessageFrame(self, 'Connecting to G2 nr. ' + IntToStr(i) + '...');
    frameEditorCentralStrip.G2List[i].USBActive := True;
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

procedure TfrmEditorMain.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  frameEditorCentralStrip.ProcessKeyDown(Key, KeyChar, Shift);
end;

procedure TfrmEditorMain.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  frameEditorCentralStrip.ProcessKeyUp(Key, KeyChar, Shift);
end;

procedure TfrmEditorMain.GlobalExceptionHandler(Sender: TObject; E: Exception);
begin
  if assigned(FframeMessageDlg) then
    FreeAndNil(FframeMessageDlg);

  MessageDlg('An error occured : ' + E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
end;

procedure TfrmEditorMain.LoadColorScheme;
var XMLColorScheme : TXMLDocument;
    XMLColorSchemeType : IXMLColorSchemeType;
    path, filename : string;

  function GetColor(const aValue : string): TAlphaColor;
  var HexValue : string;
  begin
    Assert((Length(aValue)) = 9);
    Assert(aValue.Chars[0] = '#');
    HexValue := '$' + aValue.Chars[1] + aValue.Chars[2]
              + aValue.Chars[3] + aValue.Chars[4]  // Red
              + aValue.Chars[5] + aValue.Chars[6]  // Green
              + aValue.Chars[7] + aValue.Chars[8]; // Blue
    Result := strToInt64( HexValue)
  end;

  function GetFillKind(const aValue : string): TBrushKind;
  begin
    Result := TBrushKind.bkSolid;
    if trim(Lowercase(aValue)) = 'solid' then
      Result := TBrushKind.bkSolid
    else
      if trim(Lowercase(aValue)) = 'none' then
        Result := TBrushKind.bkNone;
  end;

  function GetFontStyle(const aValue : string): TFontStyles;
  begin
    Result := [];
    if trim(Lowercase(aValue)) = 'bold' then
      Result := [TFontStyle.fsBold]
    else
      if trim(Lowercase(aValue)) = 'italic' then
        Result := [TFontStyle.fsItalic]
      else
        if trim(Lowercase(aValue)) = 'underline' then
          Result := [TFontStyle.fsUnderline];
  end;

begin
  path := ExtractFilePath(GetModuleName(0));
  filename := 'g2EditorColorScheme.xml';

  if not FileExists(path + filename) then
    exit;


  XMLColorScheme := TXMLDocument.Create(self);
  try
    XMLColorScheme.FileName := path + filename;

    XMLColorScheme.Active := True;
    try
      XMLColorSchemeType := GetColorScheme(XMLColorScheme);

      frameEditorCentralStrip.frameSlotStrip1.BackColor := GetColor(XMLColorSchemeType.SlotStrip.BackColor);
      frameEditorCentralStrip.frameSlotStrip2.BackColor := GetColor(XMLColorSchemeType.SlotStrip.BackColor);
      frameEditorCentralStrip.frameSlotStrip3.BackColor := GetColor(XMLColorSchemeType.SlotStrip.BackColor);
      frameEditorCentralStrip.frameSlotStrip4.BackColor := GetColor(XMLColorSchemeType.SlotStrip.BackColor);
      frameEditorCentralStrip.frameSlotStrip1.SelectedColor := GetColor(XMLColorSchemeType.SlotStrip.SelectedColor);
      frameEditorCentralStrip.frameSlotStrip2.SelectedColor := GetColor(XMLColorSchemeType.SlotStrip.SelectedColor);
      frameEditorCentralStrip.frameSlotStrip3.SelectedColor := GetColor(XMLColorSchemeType.SlotStrip.SelectedColor);
      frameEditorCentralStrip.frameSlotStrip4.SelectedColor := GetColor(XMLColorSchemeType.SlotStrip.SelectedColor);

      ControlStyles.DefaultFill.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].DefaultFill.Color);
      ControlStyles.DefaultFill.Kind := GetFillKind(XMLColorSchemeType.ControlStateStyles[0].DefaultFill.Kind);

      ControlStyles.DefaultFont.Family := XMLColorSchemeType.ControlStateStyles[0].DefaultFont.Family;
      ControlStyles.DefaultFont.Size := XMLColorSchemeType.ControlStateStyles[0].DefaultFont.Size;
      ControlStyles.DefaultFontColor := GetColor(XMLColorSchemeType.ControlStateStyles[0].DefaultFont.Color);
      ControlStyles.DefaultFont.Style := GetFontStyle(XMLColorSchemeType.ControlStateStyles[0].DefaultFont.Style);

      ControlStyles.DefaultStroke.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].DefaultStroke.Color);
      ControlStyles.DefaultStroke.Thickness := XMLColorSchemeType.ControlStateStyles[0].DefaultStroke.Thickness;

      ControlStyles.DisabledFill.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].DisabledFill.Color);
      ControlStyles.DisabledFill.Kind := GetFillKind(XMLColorSchemeType.ControlStateStyles[0].DisabledFill.Kind);

      ControlStyles.DisabledFont.Family := XMLColorSchemeType.ControlStateStyles[0].DisabledFont.Family;
      ControlStyles.DisabledFont.Size := XMLColorSchemeType.ControlStateStyles[0].DisabledFont.Size;
      ControlStyles.DisabledFontColor := GetColor(XMLColorSchemeType.ControlStateStyles[0].DisabledFont.Color);
      ControlStyles.DisabledFont.Style := GetFontStyle(XMLColorSchemeType.ControlStateStyles[0].DisabledFont.Style);

      ControlStyles.DisabledStroke.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].DisabledStroke.Color);
      ControlStyles.DisabledStroke.Thickness := XMLColorSchemeType.ControlStateStyles[0].DisabledStroke.Thickness;

      ControlStyles.FocusedFill.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedFill.Color);
      ControlStyles.FocusedFill.Kind := GetFillKind(XMLColorSchemeType.ControlStateStyles[0].FocusedFill.Kind);

      ControlStyles.FocusedFont.Family := XMLColorSchemeType.ControlStateStyles[0].FocusedFont.Family;
      ControlStyles.FocusedFont.Size := XMLColorSchemeType.ControlStateStyles[0].FocusedFont.Size;
      ControlStyles.FocusedFontColor := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedFont.Color);
      ControlStyles.FocusedFont.Style := GetFontStyle(XMLColorSchemeType.ControlStateStyles[0].FocusedFont.Style);

      ControlStyles.FocusedStroke.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedStroke.Color);
      ControlStyles.FocusedStroke.Thickness := XMLColorSchemeType.ControlStateStyles[0].FocusedStroke.Thickness;

      ControlStyles.SelectedFill.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].SelectedFill.Color);
      ControlStyles.SelectedFill.Kind := GetFillKind(XMLColorSchemeType.ControlStateStyles[0].SelectedFill.Kind);

      ControlStyles.SelectedFont.Family := XMLColorSchemeType.ControlStateStyles[0].SelectedFont.Family;
      ControlStyles.SelectedFont.Size := XMLColorSchemeType.ControlStateStyles[0].SelectedFont.Size;
      ControlStyles.SelectedFontColor := GetColor(XMLColorSchemeType.ControlStateStyles[0].SelectedFont.Color);
      ControlStyles.SelectedFont.Style := GetFontStyle(XMLColorSchemeType.ControlStateStyles[0].SelectedFont.Style);

      ControlStyles.FocusedStroke.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedStroke.Color);
      ControlStyles.FocusedStroke.Thickness := XMLColorSchemeType.ControlStateStyles[0].FocusedStroke.Thickness;

      ControlStyles.FocusedSelectedFill.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFill.Color);
      ControlStyles.FocusedSelectedFill.Kind := GetFillKind(XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFill.Kind);

      ControlStyles.FocusedSelectedFont.Family := XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFont.Family;
      ControlStyles.FocusedSelectedFont.Size := XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFont.Size;
      ControlStyles.FocusedSelectedFontColor := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFont.Color);
      ControlStyles.FocusedSelectedFont.Style := GetFontStyle(XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedFont.Style);

      ControlStyles.FocusedSelectedStroke.Color := GetColor(XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedStroke.Color);
      ControlStyles.FocusedSelectedStroke.Thickness := XMLColorSchemeType.ControlStateStyles[0].FocusedSelectedStroke.Thickness;
    finally
      XMLColorScheme.Active := False;
    end;
  finally
    XMLColorScheme.Free;
  end;
end;

procedure TfrmEditorMain.SaveColorScheme;
var sl : TStringList;
    path, filename : string;
begin
  sl := TStringList.Create;
  try
    path := ExtractFilePath(GetModuleName(0));
    filename := 'g2EditorColorScheme.xml';

    sl.Add('<?xml version="1.0"?>');
    sl.Add('<ColorScheme xmlns="http://www.yourtargetnamespace.com">');
    sl.Add(IndentSpaces(2) + '<SlotStrip');
    sl.Add(IndentSpaces(4) + 'backColor="' + CssColorName(TAlphaColorRec.Silver) + '"');
    sl.Add(IndentSpaces(4) + 'selectedColor="' + CssColorName(TAlphaColorRec.Seagreen) + '">');
    sl.Add(IndentSpaces(2) + '</SlotStrip>');
    sl.Add(IndentSpaces(2) + '<ControlStateStyles>');
    ControlStyles.WriteToStrings(4, sl);
    sl.Add(IndentSpaces(2) + '</ControlStateStyles>');
    sl.Add('</ColorScheme>');

    sl.SaveToFile(path + filename);

  finally
    sl.Free;
  end;
end;


end.
