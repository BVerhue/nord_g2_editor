unit UnitPatchManager;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ActnList,  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Tabs, Vcl.ActnMan,
  Vcl.Controls, Vcl.Menus, Vcl.Grids, Vcl.PlatformDefaultStyleActnCtrls,
{$ELSE}
  XPStyleActnCtrls,
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Forms, Dialogs, StdCtrls, ActnList,  ExtCtrls, ComCtrls, Tabs,
  ActnMan, Controls, Menus, Grids,
{$ENDIF}
  g2_types, g2_database, g2_file, graph_util_vcl, MusicalKeyboard, g2_midi,
  g2_graph, g2_classes, DOM, XMLRead, XMLWrite, JawsCtrls;

const
  MAXBUFFER = 4096;

  COL_NAME = 0;
  COL_CATEGORY = 1;
  COL_SLOT = 2;

  COL_FILE = 0;
  COL_DATE = 1;
  COL_PATH = 2;

type
  TfrmPatchManager = class(TForm)
    ActionManager1: TActionManager;
    aLoadPatch: TAction;
    tcPerfPatch: TTabControl;
    aLoadPerf: TAction;
    puBank: TPopupMenu;
    dgBank: TDrawGrid;
    Panel1: TPanel;
    tcBank: TTabControl;
    pPerf: TPanel;
    Label5: TLabel;
    pSlot: TPanel;
    Keyboard: TKeyboard;
    aRefresh: TAction;
    miClear: TMenuItem;
    miUploadToSlotA: TMenuItem;
    miUploadToSlotB: TMenuItem;
    miUploadToSlotC: TMenuItem;
    miUploadToSlotD: TMenuItem;
    N1: TMenuItem;
    dgCategories: TDrawGrid;
    btEnable: TG2GraphButtonText;
    Label6: TLabel;
    btKeyboard: TG2GraphButtonText;
    btHold: TG2GraphButtonText;
    Label7: TLabel;
    Label8: TLabel;
    bidLowKey: TG2GraphButtonIncDec;
    bidHighKey: TG2GraphButtonIncDec;
    gdLowKey: TG2GraphDisplay;
    gdHighKey: TG2GraphDisplay;
    rbVariation: TG2GraphButtonRadio;
    N2: TMenuItem;
    aClearBank: TAction;
    aClearBankLocation: TAction;
    aUploadBankToDisk: TAction;
    aDownloadBankToG2: TAction;
    Action1: TAction;
    miClearBank: TMenuItem;
    N3: TMenuItem;
    Uploadbanktodisk1: TMenuItem;
    DownloadbanktoG21: TMenuItem;
    Label9: TLabel;
    btKeyboardRange: TG2GraphButtonText;
    Label10: TLabel;
    puPerf: TPopupMenu;
    puPatch: TPopupMenu;
    Initperformance1: TMenuItem;
    Loadperformance1: TMenuItem;
    Saveperformance1: TMenuItem;
    Initpatch1: TMenuItem;
    Loadpatch1: TMenuItem;
    Savepatch1: TMenuItem;
    pSelSlotA: TPanel;
    pSelSlotB: TPanel;
    pSelSlotC: TPanel;
    pSelSlotD: TPanel;
    Label1: TLabel;
    pSlotA: TPanel;
    pSlotB: TPanel;
    Label2: TLabel;
    pSlotC: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    pSlotD: TPanel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    N4: TMenuItem;
    Renameperformance1: TMenuItem;
    N5: TMenuItem;
    Renamepatch1: TMenuItem;
    btPerfMode: TG2GraphButtonText;
    Label11: TLabel;
    procedure aLoadPatchExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aLoadPerfExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure dgBankDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure tcBankChange(Sender: TObject);
    procedure pSlotDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure pSlotDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure dgBankMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pSlotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgBankDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure dgBankDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure aRefreshExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure dgCategoriesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure btEnableClick(Sender: TObject);
    procedure btKeyboardClick(Sender: TObject);
    procedure btHoldClick(Sender: TObject);
    procedure bidHighKeyClick(Sender: TObject);
    procedure bidLowKeyClick(Sender: TObject);
    procedure KeyboardSetRange(Sender: TObject; aLowKey, aHighKey: Integer);
    procedure KeyboardNoteOff(Sender: TObject; aOcatve, aKey: Integer);
    procedure KeyboardNoteOn(Sender: TObject; aOcatve, aKey: Integer);
    procedure tcPerfPatchChange(Sender: TObject);
    procedure aClearBankLocationExecute(Sender: TObject);
    procedure aClearBankExecute(Sender: TObject);
    procedure rbVariationClick(Sender: TObject);
    procedure btKeyboardRangeClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btHoldMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btKeyboardMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btEnableMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pPerfMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgCategoriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aUploadBankToDiskExecute(Sender: TObject);
    procedure aDownloadBankToG2Execute(Sender: TObject);
    procedure btPerfModeClick(Sender: TObject);
  private
    { Private declarations }
    FSource : integer;
    FBankIndex,
    FPatchIndex : integer;
    FCatIndex : integer;
    FSelectedSlot : integer;
    FSelectedGridCoord : TGridCoord;
  public
    { Public declarations }
    procedure Restore( aSlotIndex, aBankIndex, aPatchIndex : integer);
    procedure Store( aSlotIndex, aBankIndex, aPatchIndex : integer);
    procedure ClearLocation( aPatchFileType : TPatchFileType; aBankIndex, aPatchIndex : integer);
    procedure ClearBank( aPatchFileType : TPatchFileType; aBankIndex : integer);
    procedure SwitchPatchSlot( aSlot1, aSlot2 : integer);
    procedure SetPatchCategorie( aPatchIndex, aCategoryIndex : integer);
    function GetSlotPanel( aIndex : integer): TPanel;
    function GetSelectedLocationName: string;
    procedure LoadIniXML;
    procedure Update;
    procedure UpdateSlot;
  end;

var
  frmPatchManager: TfrmPatchManager;

implementation

{$R *.dfm}

uses UnitG2Editor, UnitMidiMapping, UnitEditLabel;

{ TSearchThread }


procedure TfrmPatchManager.aClearBankExecute(Sender: TObject);
begin
  ClearBank( TPatchFileType(1 - tcPerfPatch.TabIndex), tcBank.TabIndex);
end;

procedure TfrmPatchManager.aClearBankLocationExecute(Sender: TObject);
begin
  ClearLocation( TPatchFileType(1 - tcPerfPatch.TabIndex), tcBank.TabIndex, (FSelectedGridCoord.Y - 1) * 8 + (FSelectedGridCoord.X - 1));
end;

procedure TfrmPatchManager.aDownloadBankToG2Execute(Sender: TObject);
var G2 : TG2;
    PatchFileType, Bank, Location : byte;
    PatchName : string;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    if TPatchFileType(1 - tcPerfPatch.TabIndex) = pftPatch then begin
      OpenDialog1.Filter := 'patch list files (*.pchlist)|*.pchlist';
      if OpenDialog1.Execute then begin

        G2.BankDumpFolder := ExtractFilePath(OpenDialog1.FileName);
        G2.BankDumpFileName := OpenDialog1.FileName;
        G2.BankDumpList.LoadFromFile( OpenDialog1.FileName);

        G2.BankDumpDestBank := tcBank.TabIndex;

        // Read bank dump list
        G2.BankDumpListIndex := 0;
        if G2.BankDumpList[G2.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump' then
          raise Exception.Create('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

        if MessageDlg('Patch bank ' + IntToStr(G2.BankDumpDestBank + 1)
                    + ' will be overwritten with patches from patch list ' + ExtractFileName(OpenDialog1.FileName)
                    + ', sure?', mtConfirmation, mbOKCancel, 0) = mrOk then
          G2.NextPatchBankDownloadMessage( self);
      end;
    end else begin
      OpenDialog1.Filter := 'patch list files (*.pchlist)|*.pchlist';
      if OpenDialog1.Execute then begin

        G2.BankDumpFolder := ExtractFilePath(OpenDialog1.FileName);
        G2.BankDumpFileName := OpenDialog1.FileName;
        G2.BankDumpList.LoadFromFile( OpenDialog1.FileName);

        G2.BankDumpDestBank := tcBank.TabIndex;

        // Read bank dump list
        G2.BankDumpListIndex := 0;
        if G2.BankDumpList[G2.BankDumpListIndex] <> 'Version=Nord Modular G2 Bank Dump' then
          raise Exception.Create('Corrupt bank dump index file (has to start with: Version=Nord Modular G2 Bank Dump).');

        if MessageDlg('Perf bank ' + IntToStr(G2.BankDumpDestBank + 1)
                    + ' will be overwritten with performances from patch list ' + ExtractFileName(OpenDialog1.FileName)
                    + ', sure?', mtConfirmation, mbOKCancel, 0) = mrOk then
          G2.NextPerfBankDownloadMessage( self);
      end;
    end;
  end;
end;

procedure TfrmPatchManager.aUploadBankToDiskExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin

    G2.BankDumpList.Clear;
    G2.BankDumpList.Add('Version=Nord Modular G2 Bank Dump');
    case TPatchFileType(1-tcPerfPatch.TabIndex) of
      pftPatch :
        begin
          G2.BankDumpFileName := 'PatchBank' + IntToStr(tcBank.TabIndex + 1) + '.pchList';
        end;
      pftPerf :
        begin
          G2.BankDumpFileName := 'PerfBank' + IntToStr(tcBank.TabIndex + 1) + '.pchList';
        end;
      else
        raise Exception.Create('Unknown patch file type.');
    end;

    SaveDialog1.Filter := 'patch list files (*.pchlist)|*.pchlist';
    SaveDialog1.FileName := G2.BankDumpFileName;
    if SaveDialog1.Execute then begin
      G2.BankDumpFolder := ExtractFilePath( SaveDialog1.FileName);
      G2.SendUploadBankMessage( TPatchFileType(1 - tcPerfPatch.TabIndex), tcBank.TabIndex, 0);
    end;
  end;
end;

procedure TfrmPatchManager.aLoadPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then
    G2.LoadFileStream( '');
  frmG2Main.SetFocus;
end;

procedure TfrmPatchManager.aLoadPerfExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then
    G2.LoadFileStream( '');
  frmG2Main.SetFocus;
end;

procedure TfrmPatchManager.aRefreshExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.RefreshBanks;
    Update;
  end;
end;

procedure TfrmPatchManager.bidHighKeyClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Upper := bidHighKey.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.bidLowKeyClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Lower := bidLowKey.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.btEnableClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Enabled := btEnable.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.btEnableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := btEnable.ClientToScreen( Point(0, btEnable.Height));
    frmMidiMapping.PopupMenu( btEnable, P.X, P.Y);
    btEnable.Invalidate;
  end;
end;

procedure TfrmPatchManager.btHoldClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Hold := btHold.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.btHoldMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := btHold.ClientToScreen( Point(0, btHold.Height));
    frmMidiMapping.PopupMenu( btHold, P.X, P.Y);
    btHold.Invalidate;
  end;
end;

procedure TfrmPatchManager.btKeyboardClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Keyboard := btKeyboard.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.btKeyboardMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := btKeyboard.ClientToScreen( Point(0, btKeyboard.Height));
    frmMidiMapping.PopupMenu( btKeyboard, P.X, P.Y);
    btKeyboard.Invalidate;
  end;
end;

procedure TfrmPatchManager.btKeyboardRangeClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.KeyboardRangeEnabled := btKeyboardRange.Value;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

function Pad( s : AnsiString; l : integer): AnsiString;
begin
  Result := s;
  while Length(Result) < l do
    Result := ' ' + Result;
end;

procedure TfrmPatchManager.dgBankDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var pt: TGridcoord;
    Panel : TPanel;
begin
  if Source is TPanel then begin
    Panel := Source as TPanel;
    pt := dgBank.MouseCoord( x, y );
    if (pt.x > 0) and (pt.Y > 0) then
      Store( (Source as TPanel).Tag, tcBank.TabIndex, (pt.Y - 1) * 8 + (pt.X - 1));
  end;
end;

procedure TfrmPatchManager.dgBankDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var Panel : TPanel;
begin
  if Source is TPanel then begin
    Panel := Source as TPanel;
    if (Panel.Tag = 4) and (tcPerfPatch.TabIndex = 0) then
      Accept := True
    else
      if (Panel.Tag in [0,1,2,3]) and (tcPerfPatch.TabIndex = 1) then
        Accept := True
      else
        Accept := False;
  end;
end;

procedure TfrmPatchManager.dgBankDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var i : integer;
    G2 : TG2;
begin
  with Sender as TDrawGrid do begin
    if ARow = 0 then begin
      if ACol > 0 then begin
        TextCenter( canvas, Rect, IntToStr(ACol));
      end;
    end else
      if ACol = 0 then begin
        TextCenter( canvas, Rect, IntToStr((ARow-1) * 8));
      end else begin
        G2 := frmG2Main.SelectedCtrlG2;
        if assigned(G2) then begin
          if G2.USBActive then begin

            i := 0;
            while (i<G2.BankList.Count) and not( (G2.BankList[i].Bank = tcBank.TabIndex)
                                             and (G2.BankList[i].Patch = ((ARow -1) * 8 + (ACol-1)))
                                             and (ord(G2.BankList[i].PatchFileType) = 1 - tcPerfPatch.TabIndex)) do
              inc(i);

            if (i<G2.BankList.Count) then begin
              canvas.Brush.Color := ModuleColors[ G2.BankList[i].Category];
              TextCenter( canvas, Rect, G2.BankList[i].PatchName);
            end else begin
              canvas.Brush.Color := clBtnFace;
              TextCenter( canvas, Rect, 'Empty');
            end;
          end else begin
            canvas.Brush.Color := clBtnFace;
            canvas.FillRect(Rect);
          end;
        end;
      end;
    end;
end;

procedure TfrmPatchManager.dgBankMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssLeft in Shift then begin
    if (dgBank.Row > 0) and (dgBank.Col > 0) then begin
      FBankIndex := tcBank.TabIndex;
      FPatchIndex := (dgBank.Row - 1) * 8 + (dgBank.Col - 1);
      dgBank.BeginDrag( False);
    end;
  end else
    if ssRight in Shift then begin
      FSelectedGridCoord := dgBank.MouseCoord( x, y );

      case tcPerfPatch.TabIndex of
      0 : begin
            aClearBank.Caption := 'Clear perf bank ' + IntToStr(tcBank.TabIndex + 1);
            aClearBankLocation.Caption := 'Clear location ' + GetSelectedLocationName;
          end;
      1 : begin
            aClearBank.Caption := 'Clear patch bank ' + IntToStr(tcBank.TabIndex + 1);
            aClearBankLocation.Caption := 'Clear location ' + GetSelectedLocationName;
          end;
      end;

      GetCursorPos(P);
      puBank.Popup( P.X, P.Y);
    end;
end;

procedure TfrmPatchManager.dgCategoriesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  with Sender as TDrawGrid do begin
    canvas.Brush.Color := ModuleColors[ ACol];
    TextCenter( canvas, Rect, CATEGORIES[ACol]);
  end;
end;

procedure TfrmPatchManager.dgCategoriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    FCatIndex := dgCategories.Col;
    dgCategories.BeginDrag( False);
  end;
end;

procedure TfrmPatchManager.FormCreate(Sender: TObject);
begin
  LoadIniXML;
end;

procedure TfrmPatchManager.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  frmG2Main.HandleMainKeyDown( Key, Shift);
end;

procedure TfrmPatchManager.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE  then
    Close;
end;

procedure TfrmPatchManager.FormResize(Sender: TObject);
begin
  Update;
end;

procedure TfrmPatchManager.FormShow(Sender: TObject);
begin
  Update;
end;

procedure TfrmPatchManager.btPerfModeClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  G2.PerfMode := btPerfMode.Value;
end;

function TfrmPatchManager.GetSelectedLocationName: string;
var i : integer;
    G2 : TG2;
    BankItem : TBankItem;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    i := (FSelectedGridCoord.Y - 1) * 8 + (FSelectedGridCoord.X - 1);
    BankItem := G2.BankList.Find( TPatchFileType(1 - tcPerfPatch.TabIndex), tcBank.TabIndex, i);
    if assigned(BankItem) then
      Result := IntToStr(tcBank.TabIndex + 1) + '-' + IntToStr(i + 1) + ' ' + BankItem.PatchName
    else
      Result := IntToStr(tcBank.TabIndex + 1) + '-' + IntToStr(i + 1);
  end;
end;

function TfrmPatchManager.GetSlotPanel(aIndex: integer): TPanel;
begin
  case aIndex of
  0 : Result := pSlotA;
  1 : Result := pSlotB;
  2 : Result := pSlotC;
  3 : Result := pSlotD;
  4 : Result := pPerf;
  end;
end;

procedure TfrmPatchManager.KeyboardNoteOff(Sender: TObject; aOcatve,
  aKey: Integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendNoteMessage( aOcatve * 12 + aKey, 1);
  end;
end;

procedure TfrmPatchManager.KeyboardNoteOn(Sender: TObject; aOcatve,
  aKey: Integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendNoteMessage( aOcatve * 12 + aKey, 0);
  end;
end;

procedure TfrmPatchManager.KeyboardSetRange(Sender: TObject; aLowKey,
  aHighKey: Integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.Performance.Slot[ FSelectedSlot].Lower := aLowKey;
    G2.Performance.Slot[ FSelectedSlot].Upper := aHighKey;
    G2.Performance.SendSetPerfSettingsMessage;
  end;
end;

procedure TfrmPatchManager.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    FormSettingsNode : TXMLFormSettingsType;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchManagerForm'));
      if assigned(FormSettingsNode) then begin
        SetFormPosition( self,
                         FormSettingsNode.PosX,
                         FormSettingsNode.PosY,
                         FormSettingsNode.SizeX,
                         FormSettingsNode.SizeY);
        Visible := FormSettingsNode.Visible;
      end;
    end;
  finally
    Doc.Free;
  end;
end;

procedure TfrmPatchManager.pSlotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Panel : TPanel;
    P : TPoint;
   G2 : TG2;
begin
  if Sender is TPanel then begin
    FSelectedSlot := (Sender as TPanel).Tag;
    G2 := frmG2Main.SelectedCtrlG2;
    if assigned(G2) then begin
      G2.SelectedSlotIndex := FSelectedSlot;
    end;
    Update;
  end;

  if ssLeft in Shift then begin
     if Sender is TPanel then
       (Sender as TPanel).BeginDrag( False);
  end else
    if ssRight in Shift then begin
      if Sender is TPanel then begin
        frmEditLabel.Left := (Sender as TPanel).ClientToScreen(Point(0, 0)).X;
        frmEditLabel.Top := (Sender as TPanel).ClientToScreen(Point(0, 0)).Y;
      end;
      GetCursorPos(P);
      puPatch.Popup( P.X, P.Y);
    end;
end;

procedure TfrmPatchManager.rbVariationClick(Sender: TObject);
begin
  frmG2Main.SelectVariation( FSelectedSlot, rbVariation.Value);
end;

procedure TfrmPatchManager.pPerfMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssLeft in Shift then begin
    pPerf.BeginDrag( False);
  end else
    if ssRight in Shift then begin
      if Sender is TPanel then begin
        frmEditLabel.Left := (Sender as TPanel).ClientToScreen(Point(0, 0)).X;
        frmEditLabel.Top := (Sender as TPanel).ClientToScreen(Point(0, 0)).Y;
      end;
      GetCursorPos(P);
      puPerf.Popup( P.X, P.Y);
    end;
end;

procedure TfrmPatchManager.pSlotDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var Panel, SrcePanel : TPanel;
    DrawGrid : TDrawGrid;
begin
  if Sender is TPanel then begin
    Panel := Sender as TPanel;
    if (Source is TDrawGrid) then begin
      DrawGrid := (Source as TDrawGrid);
      if DrawGrid.Name = 'dgBank' then
        Restore( Panel.Tag, FBankIndex, FPatchIndex);
      if DrawGrid.Name = 'dgCategories' then
        SetPatchCategorie( Panel.Tag, FCatIndex);
    end else
      if (Source is TPanel) then begin
        SrcePanel := Source as TPanel;
        if (SrcePanel.Tag < 4) and (Panel.Tag < 4) then
          SwitchPatchSlot( Panel.Tag, SrcePanel.Tag);
      end;
  end;
end;

procedure TfrmPatchManager.pSlotDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var PanelSource, PanelSender : TPanel;
begin
  Accept := False;
  if Sender is TPanel then begin
    PanelSender := Sender as TPanel;
    if (Source is TPanel) then begin
      PanelSource := Source as TPanel;
      if PanelSender.Tag = 4 then
        Accept := False
      else
        if PanelSource.Tag <> 4 then
          Accept := PanelSource.Tag <> PanelSender.Tag
        else
          Accept := False;
    end else
      if (Source is TDrawGrid) then begin
        case TPatchFileType(1 - tcPerfPatch.TabIndex) of
          pftPerf : Accept := PanelSender.Tag = 4;
          pftPatch : Accept := PanelSender.Tag <> 4;
          else
            Accept := False;
        end;
      end;
  end;
end;

procedure TfrmPatchManager.Restore(aSlotIndex, aBankIndex,
  aPatchIndex: integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendRetrieveMessage( aSlotIndex, aBankIndex, aPatchIndex)
  end;
end;

procedure TfrmPatchManager.SetPatchCategorie(aPatchIndex,
  aCategoryIndex: integer);
var G2 : TG2;
    FPatchDescription : TPatchDescription;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    FPatchDescription := G2.Patch[ aPatchIndex].PatchDescription;
    FPatchDescription.Categorie := aCategoryIndex;
    (G2.Patch[ aPatchIndex] as TG2Patch).MessSetPatchDescription( FPatchDescription);
  end;
end;

procedure TfrmPatchManager.Store( aSlotIndex, aBankIndex, aPatchIndex : integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendStoreMessage( aSlotIndex, aBankIndex, aPatchIndex)
  end;
end;

procedure TfrmPatchManager.ClearBank( aPatchFileType : TPatchFileType; aBankIndex: integer);
var G2 : TG2;
    FromLocation, ToLocation : byte;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    if G2.BankList.FindFirstLast( aPatchFileType, aBankIndex, FromLocation, ToLocation) then begin
      if MessageDlg('All patches in bank ' + IntToStr(aBankIndex + 1) + ' wil be deleted, sure?', mtWarning,  mbOKCancel, 0) = mrOk then
        G2.SendClearBankMessage( aPatchFileType, aBankIndex, FromLocation, ToLocation);
    end else
      raise Exception.Create('Bank is already empty.');
  end;
end;

procedure TfrmPatchManager.ClearLocation( aPatchFileType : TPatchFileType; aBankIndex, aPatchIndex : integer);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendClearMessage( aPatchFileType, aBankIndex, aPatchIndex)
  end;
end;

procedure TfrmPatchManager.SwitchPatchSlot(aSlot1, aSlot2: integer);
var G2 : TG2;
    PatchData1, PatchData2 : TMemoryStream;
    G2FileDataStream : TG2FileDataStream;
    PatchName1, PatchName2 : AnsiString;
    Patch : TG2FilePatch;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    PatchData1 := TMemoryStream.Create;
    PatchData2 := TmemoryStream.Create;
    Patch := TG2FilePatch.Create(nil);
    try
      G2.Patch[ aSlot1].SaveToFile( PatchData1);
      PatchName1 := G2.Patch[ aSlot1].PatchName;
      G2.Patch[ aSlot2].SaveToFile( PatchData2);
      PatchName2 := G2.Patch[ aSlot2].PatchName;

      Patch.LoadFromFile( PatchData1, nil);
      G2.Slot[ aSlot2].SendSetPatchMessage( PatchName1, Patch);

      Patch.LoadFromFile( PatchData2, nil);
      G2.Slot[ aSlot1].SendSetPatchMessage( PatchName2, Patch);
    finally
      Patch.Free;
      PatchData2.Free;
      PatchData1.Free;
    end;
  end;
end;

procedure TfrmPatchManager.tcBankChange(Sender: TObject);
begin
  Update;
end;

procedure TfrmPatchManager.tcPerfPatchChange(Sender: TObject);
begin
  Update;
end;

procedure TfrmPatchManager.UpdateSlot;
var i : integer;
    G2 : TG2;
    Perf : TG2FilePerformance;
    Slot : TG2FileSlot;
    Patch : TG2FilePatch;
    Panel : TPanel;
    BankItem : TBankItem;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin

    FSelectedSlot := G2.SelectedSlot.SlotIndex;

    pSlot.Color := G_SlotStripColor;
    pSlot.Font.Size := 8;
    pSlot.Font.Style := [fsbold];
    pSlot.Font.Color := G_SlotStripInverseColor;

    rbVariation.HightlightColor := G_HighlightColor;

    Keyboard.Left := 0;
    Keyboard.Width := pSlot.ClientWidth;

    btPerfMode.HightlightColor := G_HighlightColor;
    btPerfMode.InitValue(G2.PerfMode);

    Perf := G2.Performance;
    if assigned(Perf) then begin
      btKeyboardRange.HightlightColor := G_HighlightColor;
      btKeyboardRange.Value := Perf.KeyboardRangeEnabled;
    end;

    Slot := G2.Slot[ FSelectedSlot];
    if assigned(Slot) then begin
      rbVariation.InitValue( Slot.Patch.ActiveVariation);

      btEnable.HightlightColor := G_HighlightColor;
      btKeyboard.HightlightColor := G_HighlightColor;
      btHold.HightlightColor := G_HighlightColor;

      btEnable.Value := Slot.Enabled;
      btKeyboard.Value := Slot.Keyboard;
      btHold.Value := Slot.Hold;

      gdLowKey.Line[0] := GetKeyName(Slot.Lower);
      gdHighKey.Line[0] := GetKeyName(Slot.Upper);

      bidLowKey.Value := Slot.Lower;
      bidHighKey.Value := Slot.Upper;

      Keyboard.HighLightColorBlackKeys := Darker(G_HighlightColor, 200);
      Keyboard.HighLightColorWhiteKeys := G_HighlightColor;
      Keyboard.LowKey := Slot.Lower;
      Keyboard.HighKey := Slot.Upper;
    end;

    pPerf.Color := ModuleColors[ 0];
    pPerf.Caption := G2.Performance.PerformanceName;

    for i := 0 to 3 do begin
      Slot := G2.Performance.Slot[i];
      Patch := G2.Patch[i];
      Panel := GetSlotPanel(i);
      if (not assigned(Patch)) or ((Patch.ModuleList[ ord(ltVA)].Count = 0) and (Patch.ModuleList[ ord(ltFX)].Count = 0)) then begin
        Panel.Caption := 'Empty';
        Panel.Color := clBtnFace;
      end else begin
        Panel.Caption := Patch.PatchName;
        Panel.Color := ModuleColors[ Patch.PatchDescription.Categorie]
      end;
    end;

    pSelSlotA.Color := G_HighlightColor;
    pSelSlotB.Color := G_HighlightColor;
    pSelSlotC.Color := G_HighlightColor;
    pSelSlotD.Color := G_HighlightColor;

    pSelSlotA.ParentColor := FSelectedSlot <> 0;
    pSelSlotB.ParentColor := FSelectedSlot <> 1;
    pSelSlotC.ParentColor := FSelectedSlot <> 2;
    pSelSlotD.ParentColor := FSelectedSlot <> 3;
  end;
end;

procedure TfrmPatchManager.Update;
begin
  dgBank.RowCount := 17;
  dgBank.ColCount := 9;
  dgBank.DefaultRowHeight := (dgBank.ClientHeight - 16 - (dgBank.RowCount * 1)) div (dgBank.RowCount - 1);
  dgBank.DefaultColWidth := (dgBank.ClientWidth - 16 - (dgBank.ColCount * 1)) div (dgBank.ColCount - 1);
  dgBank.ColWidths[0] := 16;
  dgBank.RowHeights[0] := 16;
  dgBank.Invalidate;

  dgCategories.RowCount := 1;
  dgCategories.ColCount := 16;
  dgCategories.DefaultRowHeight := dgCategories.ClientHeight;
  dgCategories.DefaultColWidth := (dgCategories.ClientWidth - (dgCategories.ColCount * 1)) div (dgCategories.ColCount);
  dgCategories.Invalidate;

  case tcPerfPatch.TabIndex of
  0 : begin
        while tcBank.Tabs.Count > 8 do
          tcBank.Tabs.Delete( tcBank.Tabs.Count - 1);
      end;
  1 : begin
        while tcBank.Tabs.Count < 32 do
          tcBank.Tabs.Add( IntToStr(tcBank.Tabs.Count + 1));
      end;
  end;

  UpdateSlot;
end;

end.
