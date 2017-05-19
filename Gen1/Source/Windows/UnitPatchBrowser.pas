unit UnitPatchBrowser;

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
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, VCL.Forms, VCL.Dialogs, VCL.StdCtrls,
  VCL.ActnList,  VCL.ExtCtrls, VCL.ComCtrls, VCL.Tabs, VCL.ActnMan,
  VCL.Controls, Vcl.Menus, VCL.PlatformDefaultStyleActnCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Forms, Dialogs, StdCtrls, ActnList,  ExtCtrls, ComCtrls, Tabs,
  Menus, XPStyleActnCtrls, ActnMan, Controls,
{$ENDIF}
  g2_types, g2_database, g2_file, g2_classes,
  DOM, XMLRead, XMLWrite, JawsCtrls;

const
  MAXBUFFER = 4096;

  COL_NAME = 0;
  COL_CATEGORY = 1;
  COL_SLOT = 2;

  COL_FILE = 0;
  COL_DATE = 1;
  COL_PATH = 2;

type
  TSearchThread = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
    procedure AddFile;
    //procedure ShowFile;
  public
    FCurrSr : TSearchRec;
    FCurrPath, FPath, FMask, FSearch : string;
    FPos : integer;
  end;

  TfrmPatchBrowser = class(TForm)
    ActionManager1: TActionManager;
    aReadDir: TAction;
    aSearch: TAction;
    aLoadPatch: TAction;
    tcSource: TTabControl;
    aShowPerfs: TAction;
    aShowPatches: TAction;
    aRestore: TAction;
    aReadDirPerf: TAction;
    aLoadPerf: TAction;
    lvInternal: DListView;
    lvExternalPatch: DListView;
    lvExternalPerf: DListView;
    puBank: TPopupMenu;
    Panel1: TPanel;
    cbParsePatches: TCheckBox;
    cbFilterModules: TCheckBox;
    bSelectModules: TButton;
    StaticText1: TStaticText;
    stPatchesFound: TStaticText;
    procedure aReadDirExecute(Sender: TObject);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvExternalPatchColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvExternalCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure aShowPerfsExecute(Sender: TObject);
    procedure aShowPatchesExecute(Sender: TObject);
    procedure tcSourceChange(Sender: TObject);
    procedure lvInternalColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvInternalCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure aRestoreExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvExternalPerfColumnClick(Sender: TObject; Column: TListColumn);
    procedure aLoadPerfExecute(Sender: TObject);
    procedure cbParsePatchesClick(Sender: TObject);
    procedure bSelectModulesClick(Sender: TObject);
    procedure lvExternalPerfKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvExternalPatchKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvInternalKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvExternalPatchMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FParsePatches : boolean;
    procedure SetParsePatches( aValue : boolean);
  public
    { Public declarations }
    FSearchThread : TSearchThread;
    FExternalSortCol : integer;
    FExternalSortDir : integer;
    FInternalSortCol : integer;
    FInternalSortDir : integer;
    function  GetRelativePath( aPath : string): string;
    procedure AddFile( Path, Filename: string; Datum : TDateTime);
    procedure AddSlot( BankItem : TBankItem);
    procedure OnSearchThreadTerminate(Sender: TObject);
    procedure LoadIniXML;

    property ParsePatches : boolean read FParsePatches write SetParsePatches;
  end;

var
  frmPatchBrowser: TfrmPatchBrowser;

implementation

{$R *.dfm}

uses UnitG2Editor, UnitSettings, UnitPatchBrowserFilterModules;

{ TSearchThread }

procedure TSearchThread.Execute;

  procedure SearchDir(path : string);
  var sr : TSearchRec;
  begin
    if FindFirst(path + '*.*', faAnyFile, sr) = 0 then begin
      repeat
        FCurrPath := path;
        FCurrSr := sr;
        if (sr.Attr and faDirectory) = 0 then begin
          // Geen subdirectory
          Synchronize(AddFile);
        end else begin
          // subdirectory
          if (sr.Name <> '.') and (sr.Name <> '..') then
            SearchDir(path + sr.Name + '\');
        end;

      until (FindNext(sr) <> 0) or Terminated;
      FindClose(sr);
    end;
  end;

begin
  { Place thread code here }
  SearchDir(FPath);
end;

procedure TfrmPatchBrowser.OnSearchThreadTerminate(Sender: TObject);
begin
  cbParsePatches.Enabled := True;
  cbFilterModules.Enabled := True;
  bSelectModules.Enabled := True;
  FSearchThread := nil;
end;

procedure TSearchThread.AddFile;
var sr : TSearchRec;
begin
  {$IFDEF G2_VER200_up}
    // Don't know exactly in what version this was changed
    frmPatchBrowser.AddFile(FCurrPath, FCurrSr.Name, FCurrSr.TimeStamp);
  {$ELSE}
    frmPatchBrowser.AddFile(FCurrPath, FCurrSr.Name, FileDateToDateTime(FCurrSr.Time));
  {$ENDIF}
end;


{ frmPatchBrowser }

procedure TfrmPatchBrowser.FormCreate(Sender: TObject);
begin
  LoadIniXML;
  FParsePatches := False;
  FExternalSortDir := 0;
  FInternalSortDir := 0;
end;

procedure TfrmPatchBrowser.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE  then
    Close;
end;

procedure TfrmPatchBrowser.FormShow(Sender: TObject);
begin
  tcSourceChange(Self);
end;

procedure TfrmPatchBrowser.LoadIniXML;
var Doc : TXMLDocument;
    RootNode : TDOMNode;
    PatchBrowserSettingsNode : TXMLPatchBrowserSettingsType;
    FormSettingsNode : TXMLFormSettingsType;
    G2 : TG2;
begin
  if not FileExists('G2_editor_ini.xml') then
    exit;

  Doc := TXMLDocument.Create;
  try
    ReadXMLFile( Doc, 'G2_editor_ini.xml');

    RootNode := Doc.FindNode('G2_Editor_settings');
    if assigned(RootNode) then begin

      PatchBrowserSettingsNode := TXMLPatchBrowserSettingsType(RootNode.FindNode('PatchBrowserSettings'));
      if assigned(PatchBrowserSettingsNode) then begin
        FExternalSortCol := PatchBrowserSettingsNode.ExternalSortCol;
        FInternalSortCol := PatchBrowserSettingsNode.InternalSortCol;
        if PatchBrowserSettingsNode.SelectedTab < tcSource.Tabs.Count then
          tcSource.TabIndex := PatchBrowserSettingsNode.SelectedTab;
      end;

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatcBrowserForm'));
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

procedure TfrmPatchBrowser.bSelectModulesClick(Sender: TObject);
begin
  if frmPatchBrowserModuleFilter.ShowModal = mrOk then begin
    frmPatchBrowserModuleFilter.UpdateSelectedModules;
    cbFilterModules.Checked := True;
    if not cbParsePatches.Checked then
      cbParsePatches.Checked := True
    else
      tcSourceChange( self);
  end else
    cbFilterModules.Checked := False;
end;

procedure TfrmPatchBrowser.cbParsePatchesClick(Sender: TObject);
begin
  ParsePatches := cbParsePatches.Checked;
  tcSourceChange( self);
end;

procedure TfrmPatchBrowser.lvExternalPatchColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FExternalSortCol = Column.Index then begin
    FExternalSortDir := 1 - FExternalSortDir;
    lvExternalPatch.AlphaSort;
  end else begin
    FExternalSortDir := 0;
    FExternalSortCol := Column.Index;
    lvExternalPatch.AlphaSort;
  end;
end;

procedure TfrmPatchBrowser.lvExternalPerfColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FExternalSortCol = Column.Index then begin
    FExternalSortDir := 1 - FExternalSortDir;
    lvExternalPerf.AlphaSort;
  end else begin
    FExternalSortDir := 0;
    FExternalSortCol := Column.Index;
    lvExternalPerf.AlphaSort;
  end;
end;

procedure TfrmPatchBrowser.lvInternalColumnClick(Sender: TObject; Column: TListColumn);
begin
  if FInternalSortCol = Column.Index then begin
    FInternalSortDir := 1 - FInternalSortDir;
    lvInternal.AlphaSort;
  end else begin
    FInternalSortDir := 0;
    FInternalSortCol := Column.Index;
    lvInternal.AlphaSort;
  end;
end;

procedure TfrmPatchBrowser.lvExternalPatchKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aLoadPatchExecute(self);
end;

procedure TfrmPatchBrowser.lvExternalPatchMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    lvExternalPatch.BeginDrag( False);
  end;
end;

procedure TfrmPatchBrowser.lvExternalPerfKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aLoadPerfExecute(self);
end;

procedure TfrmPatchBrowser.lvInternalKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    aRestoreExecute(self);
end;

procedure TfrmPatchBrowser.tcSourceChange(Sender: TObject);
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  case tcSource.TabIndex of
  0 : begin
        lvInternal.Visible := False;
        lvExternalPatch.Visible := False;
        lvExternalPerf.Visible := True;
        lvExternalPerf.Align := alClient;

        cbParsePatches.Checked := False;
        aReadDirExecute(self);
      end;
  1 : begin
        lvInternal.Visible := False;
        lvExternalPerf.Visible := False;
        lvExternalPatch.Visible := True;
        lvExternalPatch.Align := alClient;

        aReadDirExecute(self);
      end;
  2 : begin
        cbParsePatches.Checked := False;
        aShowPerfsExecute(self);
      end;
  3 : begin
        cbParsePatches.Checked := False;
        aShowPatchesExecute(self);
      end;
  end;
end;

procedure TfrmPatchBrowser.aLoadPatchExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then
    G2.LoadFileStream( frmSettings.ePatchRootFolder.Text + lvExternalPatch.Selected.SubItems[1] + lvExternalPatch.Selected.Caption);
  frmG2Main.SetFocus;
end;

procedure TfrmPatchBrowser.aLoadPerfExecute(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then
    G2.LoadFileStream( frmSettings.ePatchRootFolder.Text + lvExternalPerf.Selected.SubItems[1] + lvExternalPerf.Selected.Caption);
  frmG2Main.SetFocus;
end;

procedure TfrmPatchBrowser.aRestoreExecute(Sender: TObject);
var BankItem : TBankItem;
    G2 : TG2;
begin
 BankItem := TBankItem(lvInternal.Selected.Data);
  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then begin
    if BankItem.PatchFileType = pftPerf then
      G2.SendRetrieveMessage( 4, BankItem.Bank, BankItem.Patch)
    else
      G2.SendRetrieveMessage( G2.SelectedSlotIndex, BankItem.Bank, BankItem.Patch);
  end;
  frmG2Main.SetFocus;
end;

procedure TfrmPatchBrowser.aReadDirExecute(Sender: TObject);
begin
  lvExternalPatch.Items.Clear;
  lvExternalPerf.Items.Clear;
  stPatchesFound.Caption := IntToStr(0);

  if Length(frmSettings.ePatchRootFolder.Text)> 2 then begin
    if frmSettings.ePatchRootFolder.Text[Length(frmSettings.ePatchRootFolder.Text)] <> '\'  then
      frmSettings.ePatchRootFolder.Text := frmSettings.ePatchRootFolder.Text + '\';

    FSearchThread := TSearchThread.Create(True);
    FSearchThread.FPath := frmSettings.ePatchRootFolder.Text;
    FSearchThread.OnTerminate := OnSearchThreadTerminate;
    FSearchThread.FreeOnTerminate := True;

    cbParsePatches.Enabled := False;
    cbFilterModules.Enabled := False;
    bSelectModules.Enabled := False;

{$IFDEF G2_VER200_up}
    FSearchThread.Start;
{$ELSE}
    FSearchThread.Resume;
{$ENDIF}
  end;
  Invalidate;
end;

function Pad( s : AnsiString; l : integer): AnsiString;
begin
  Result := s;
  while Length(Result) < l do
    Result := ' ' + Result;
end;

procedure TfrmPatchBrowser.aShowPatchesExecute(Sender: TObject);
var i : integer;
    G2 : TG2;
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  lvInternal.Clear;
  lvExternalPerf.Visible := False;
  lvExternalPatch.Visible := False;
  lvInternal.Visible := True;
  lvInternal.Align := alClient;

  lvInternal.Clear;

  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then begin
    if G2.USBActive then begin
      for i := 0 to G2.BankList.Count - 1 do begin
        if G2.BankList[i].PatchFileType = pftPatch then
          AddSlot( G2.BankList[i]);
      end;
    end else begin
      // No usb, just fill the list with bank and slot no's
    end;
  end;
  lvInternal.AlphaSort;
end;

procedure TfrmPatchBrowser.aShowPerfsExecute(Sender: TObject);
var i : integer;
    G2 : TG2;
begin
  if assigned(FSearchThread) then
    FSearchThread.Terminate;

  lvInternal.Clear;
  lvExternalPerf.Visible := False;
  lvExternalPatch.Visible := False;
  lvInternal.Visible := True;
  lvInternal.Align := alClient;

  lvInternal.Clear;

  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then begin
    for i := 0 to G2.BankList.Count - 1 do begin
      if G2.BankList[i].PatchFileType = pftPerf then
        AddSlot( G2.BankList[i]);
    end;
  end;
  lvInternal.AlphaSort;
end;

procedure TfrmPatchBrowser.lvExternalCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var intItem1,
    intItem2: string;
begin
   case FExternalSortCol of
   0 : begin
         intItem1 := Uppercase(Item1.Caption);
         intItem2 := Uppercase(Item2.Caption);
       end;
     else begin
       if FExternalSortCol - 1 < Item1.SubItems.Count then
         intItem1 := Uppercase(Item1.SubItems[FExternalSortCol - 1])
       else
         intItem1 := '';

       if FExternalSortCol - 1 < Item2.SubItems.Count then
         intItem2 := Uppercase(Item2.SubItems[FExternalSortCol - 1])
       else
         intItem2 := '';
     end;
   end;

   if FExternalSortDir = 0 then begin

     if intItem1 < intItem2 then
       Compare := -1
     else
     if intItem1 > intItem2 then
       Compare := 1
     else // intItem1 = intItem2
       Compare := 0;

   end else begin

     if intItem1 < intItem2 then
       Compare := 1
     else
     if intItem1 > intItem2 then
       Compare := -1
     else // intItem1 = intItem2
       Compare := 0;

   end;
end;

procedure TfrmPatchBrowser.lvInternalCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
   intItem1,
   intItem2: string;
begin
   case FInternalSortCol of
   1 : begin
         intItem1 := Uppercase(Item1.SubItems[0]);
         intItem2 := Uppercase(Item2.SubItems[0]);
       end;
   2 : begin
         intItem1 := Uppercase(Item1.SubItems[1]);
         intItem2 := Uppercase(Item2.SubItems[1]);
       end;
   else begin
       intItem1 := Uppercase(Item1.Caption);
       intItem2 := Uppercase(Item2.Caption);
     end;
   end;

   if FInternalSortDir = 0 then begin

     if intItem1 < intItem2 then
       Compare := -1
     else
     if intItem1 > intItem2 then
       Compare := 1
     else // intItem1 = intItem2
       Compare := 0;

   end else begin

     if intItem1 < intItem2 then
       Compare := 1
     else
     if intItem1 > intItem2 then
       Compare := -1
     else // intItem1 = intItem2
       Compare := 0;

   end;
end;

procedure TfrmPatchBrowser.SetParsePatches( aValue : boolean);
var ListColumn : TListColumn;
    G2 : TG2;
begin
  if aValue <> FParsePatches then begin
    FParsePatches := aValue;

    lvExternalPatch.Columns.Clear;
    ListColumn := lvExternalPatch.Columns.Add;
    ListColumn.Caption := 'Patch file';
    ListColumn.Width := 150;
    ListColumn := lvExternalPatch.Columns.Add;
    ListColumn.Caption := 'Date';
    ListColumn.Width := 80;
    ListColumn := lvExternalPatch.Columns.Add;
    ListColumn.Caption := 'Path';
    ListColumn.Width := 80;

    if FParsePatches then begin
      ListColumn := lvExternalPatch.Columns.Add;
      ListColumn.Caption := 'ParseMsg';
      ListColumn.Width := 50;

      ListColumn := lvExternalPatch.Columns.Add;
      ListColumn.Caption := 'Version';
      ListColumn.Width := 30;

      ListColumn := lvExternalPatch.Columns.Add;
      ListColumn.Caption := 'Category';
      ListColumn.Width := 50;

      ListColumn := lvExternalPatch.Columns.Add;
      ListColumn.Caption := 'Patch notes';
      ListColumn.Width := 100;
    end;
  end;
end;

function TfrmPatchBrowser.GetRelativePath( aPath : string): string;
var rpl : integer;
begin
  // Relative to root path
  rpl := Length(frmSettings.ePatchRootFolder.Text);
  Result := copy( aPath, rpl + 1, Length( aPath) - (rpl));
end;

procedure TfrmPatchBrowser.AddFile(path, filename: string; datum : TDateTime);
var ListItem : TListItem;
    ListColumn : TListColumn;
    ext : string;
    Patch : TG2FilePatch;
    FileStream : TFileStream;
    l, m, i, Count : integer;
    AddPatch : boolean;
    Module : TG2FileModule;
begin
  ListItem := nil;
  ext := ExtractFileExt(filename);
  if lowercase(ext) = '.pch2' then begin

    if FParsePatches then begin
      FileStream := TFileStream.Create( path + filename, fmOpenRead);
      Patch := TG2FilePatch.Create(nil);
      try
        try
          Patch.LoadFromFile( FileStream, nil);

          if cbFilterModules.Checked then begin

            AddPatch := True;

            for i := 0 to frmPatchBrowserModuleFilter.FSelectedModules.Count - 1 do begin

              Count := Patch.PatchPart[0].GetNoOffModuleType(integer(frmPatchBrowserModuleFilter.FSelectedModules.Objects[i]))
                     + Patch.PatchPart[1].GetNoOffModuleType(integer(frmPatchBrowserModuleFilter.FSelectedModules.Objects[i]));

              if Count = 0 then
                AddPatch := False;
            end;
          end else
            AddPatch := True;

          if AddPatch then begin
            ListItem := lvExternalPatch.Items.Add;
            ListItem.Caption := filename;
            ListItem.SubItems.Add( FormatDateTime('yyyy/mm/dd hh:nn:ss', datum));
            ListItem.SubItems.Add( GetRelativePath(path));
            ListItem.SubItems.Add( ''); // ParseMsg
            ListItem.SubItems.Add( IntToStr( Patch.PatchVersion));
            ListItem.SubItems.Add( CATEGORIES[ Patch.PatchDescription.Categorie]);
            ListItem.SubItems.Add( Patch.PatchNotes.Text );
          end;

          except on E:Exception do begin
            ListItem := lvExternalPatch.Items.Add;
            ListItem.Caption := filename;
            ListItem.SubItems.Add( FormatDateTime('yyyy/mm/dd hh:nn:ss', datum));
            ListItem.SubItems.Add( GetRelativePath(path));
            ListItem.SubItems.Add( E.Message); // ParseMsg
            ListItem.SubItems.Add( IntToStr( Patch.PatchVersion));
          end;
        end;

      finally
        Patch.Free;
        FileStream.Free;
      end;
    end else begin
      ListItem := lvExternalPatch.Items.Add;
      if assigned(ListItem) then begin
        ListItem.Caption := filename;
        ListItem.SubItems.Add( FormatDateTime('yyyy/mm/dd hh:nn:ss', datum));
        ListItem.SubItems.Add( GetRelativePath(path));
      end;
    end;
    stPatchesFound.Caption := IntToStr(lvExternalPatch.Items.Count);
  end else begin
    if lowercase(ext) = '.prf2' then begin
      ListItem := lvExternalPerf.Items.Add;
      if assigned(ListItem) then begin
        ListItem.Caption := filename;
        ListItem.SubItems.Add( FormatDateTime('yyyy/mm/dd hh:nn:ss', datum));
        ListItem.SubItems.Add( GetRelativePath( path));
      end;
      stPatchesFound.Caption := IntToStr(lvExternalPerf.Items.Count);
    end;
  end;
end;

procedure TfrmPatchBrowser.AddSlot( BankItem : TBankItem);
var ListItem : TListItem;
begin
  ListItem := lvInternal.Items.Add;
  ListItem.Caption := string(BankItem.PatchName);
  ListItem.SubItems.Add( CATEGORIES[ BankItem.Category]);
  ListItem.SubItems.Add( string(Pad(AnsiString(IntToStr( BankItem.Bank)), 2) + ':'
                              + Pad(AnsiString(IntToStr( BankItem.Patch)), 3)));
  ListItem.Data := BankItem;
end;

end.
