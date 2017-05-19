unit UnitPatchBuffer;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan, Vcl.StdCtrls, Vcl.ComCtrls,
  DOM, XMLRead, XMLWrite,
  g2_database, g2_types, g2_graph, g2_file, g2_mess, g2_usb, g2_midi, g2_classes;

type
  TfrmPatchBuffer = class(TForm)
    lvPatchList: TListView;
    Panel1: TPanel;
    Button1: TButton;
    ActionManager1: TActionManager;
    aFormClose: TAction;
    Splitter1: TSplitter;
    Panel2: TPanel;
    G2Buffer: TG2;
    sbVaBuffer: TG2GraphScrollBox;
    sbFXBuffer: TG2GraphScrollBox;
    Splitter2: TSplitter;
    OpenDialog1: TOpenDialog;
    aPatchAdd: TAction;
    Button2: TButton;
    aSelectVA: TAction;
    aSelectFX: TAction;
    aLoadPatch: TAction;
    aCopySelectionToClipboard: TAction;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    aCopyPatchSelection: TAction;
    procedure aFormCloseExecute(Sender: TObject);
    procedure aPatchAddExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure aSelectVAExecute(Sender: TObject);
    procedure aSelectFXExecute(Sender: TObject);
    procedure aLoadPatchExecute(Sender: TObject);
    procedure lvPatchListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure sbVaBufferMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbFXBufferMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aCopyPatchSelectionExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure SearchDir( aPath : string);
    procedure AddPatch( aFilename : string);
  end;

var
  frmPatchBuffer: TfrmPatchBuffer;

implementation

{$R *.dfm}

uses UnitG2Editor;

procedure TfrmPatchBuffer.FormCreate(Sender: TObject);
begin
  G2Buffer.LoadModuleDefs('');
  LoadIniXML;
end;

procedure TfrmPatchBuffer.FormDestroy(Sender: TObject);
begin
  if assigned(frmG2Main) and assigned(frmG2Main.FCopyPatch) then begin
    frmG2Main.FCopyPatch.Free;
    frmG2Main.FCopyPatch := nil;
  end;
end;

procedure TfrmPatchBuffer.LoadIniXML;
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

      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('PatchBufferForm'));
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

procedure TfrmPatchBuffer.aCopyPatchSelectionExecute(Sender: TObject);
begin
  if assigned( frmG2Main.FCopyPatch) then
    frmG2Main.FCopyPatch.Free;

  frmG2Main.FCopyPatch := TG2FilePatchPart.CopyModules( G2Buffer.SelectedPatch, G2Buffer.SelectedPatchPart, G2Buffer.SelectedPatchPart.SelectedModuleList);
  frmG2Main.SetFocus;
  frmG2Main.PastePatchSelection;
end;

procedure TfrmPatchBuffer.aFormCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmPatchBuffer.AddPatch( aFilename : string);
var Patch : TG2GraphPatch;
    FileStream : TFileStream;
begin
  Patch := TG2GraphPatch.Create(self);
  try
    Filestream := TFileStream.Create( aFileName, fmOpenRead);
    try
      Patch.LoadFromFile( FileStream, nil);
      lvPatchList.AddItem( ExtractFileName( aFileName), Patch);
    finally
      FileStream.Free;
    end;
  except on E:Exception do begin
      Patch.Free;
    end;
  end;
end;

procedure TfrmPatchBuffer.SearchDir( aPath : string);
var sr : TSearchRec;
begin
  if FindFirst( aPath + '*.*', faAnyFile, sr) = 0 then begin
    repeat
      if (sr.Attr and faDirectory) = 0 then begin
        AddPatch( aPath + '/' + sr.Name);
      end else begin
        if (sr.Name = '.') and (sr.Name <> '..') then begin
        end;
      end;
    until (FindNext(sr) <> 0);
    FindClose(sr);
  end;
end;

procedure TfrmPatchBuffer.aLoadPatchExecute(Sender: TObject);
var G2 : TG2;
    ListItem : TListItem;
begin
  if lvPatchList.ItemIndex <> -1 then begin
    ListItem := lvPatchList.Items[ lvPatchList.ItemIndex];
    G2 := frmG2Main.SelectedEditG2;
    if assigned(G2) then begin
      G2.SelectedSlot.SendSetPatchMessage( ListItem.Caption, TG2GraphPatch(ListItem.Data));
      frmG2Main.SetFocus;
    end;
  end;
end;

procedure TfrmPatchBuffer.aPatchAddExecute(Sender: TObject);
var i : integer;
begin
  OpenDialog1.Options := [ofAllowMultiSelect, ofFileMustExist];
  OpenDialog1.Filter := 'patch files (*.pch2)|*.pch2|extended patch files (*.pch2x)|*.pch2x|sysex files (*.syx)|*.syx|bin files (*.bin)|*.bin';
  if OpenDialog1.Execute then begin
    for i := 0 to OpenDialog1.Files.Count - 1 do begin
      AddPatch(OpenDialog1.Files[i]);
    end;
  end;
end;

procedure TfrmPatchBuffer.aSelectFXExecute(Sender: TObject);
var i : integer;
begin
  G2Buffer.SelectedPatch.SelectedLocation := ltFX;
  for i := 0 to G2Buffer.SelectedPatchPart.ModuleList.Count - 1 do
    G2Buffer.SelectedPatchPart.ModuleList[i].Selected := True;

  sbVaBuffer.Invalidate;
  sbFXBuffer.Invalidate;
end;

procedure TfrmPatchBuffer.aSelectVAExecute(Sender: TObject);
var i : integer;
begin
  G2Buffer.SelectedPatch.SelectedLocation := ltVA;
  for i := 0 to G2Buffer.SelectedPatchPart.ModuleList.Count - 1 do
    G2Buffer.SelectedPatchPart.ModuleList[i].Selected := True;

  sbVaBuffer.Invalidate;
  sbFXBuffer.Invalidate;
end;

procedure TfrmPatchBuffer.lvPatchListSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var ListItem : TListItem;
begin
  if lvPatchList.ItemIndex <> -1 then begin
    ListItem := lvPatchList.Items[ lvPatchList.ItemIndex];
    G2Buffer.SelectedSlot.SendSetPatchMessage( ListItem.Caption, TG2GraphPatch(ListItem.Data));
  end;
end;

procedure TfrmPatchBuffer.sbFXBufferMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  G2Buffer.SelectedPatch.SelectedLocation := ltFX;
end;

procedure TfrmPatchBuffer.sbVaBufferMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  G2Buffer.SelectedPatch.SelectedLocation := ltVA;
end;

end.
