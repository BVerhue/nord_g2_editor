unit UnitLog;

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
{$IFDEF G2_VER220_up}
  WinAPI.Windows, WinAPI.Messages, System.SysUtils, System.Variants,
  System.Classes, VCL.Graphics, VCL.Controls, VCL.Forms, VCL.Dialogs,
  VCL.StdCtrls, VCL.ExtCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
{$ENDIF}
  G2_Types, g2_mess, G2_USB, g2_classes;

type
  TfrmLog = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    eCommand: TEdit;
    bSendMsg: TButton;
    Panel2: TPanel;
    bRefresh: TButton;
    bClear: TButton;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    procedure bSendMsgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bRefreshClick(Sender: TObject);
    procedure bClearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLog: TfrmLog;

implementation

uses UnitG2Editor;

{$R *.dfm}

{$if CompilerVersion <= 18.5}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ifend}

procedure TfrmLog.bClearClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.ClearLog;
    G2.AssignLog( Memo1.Lines);
  end;
end;


procedure TfrmLog.bRefreshClick(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.AssignLog( Memo1.Lines);
  end;
end;

procedure TfrmLog.bSendMsgClick(Sender: TObject);
var MemStream : TG2SendMessage;
    G2 : TG2;

    i, j : integer;
    b : byte;

    function GetHexValue(h : char): integer;
    begin
      if CharInSet(h, ['0'..'9']) then
        Result := ord(h) - 48
      else
        Result := ord(h) - 55;
    end;

begin
  // Send a custom message to the G2

  MemStream := TG2SendMessage.Create;

  b := 0;
  i := 1;
  j := 0;
  while (i<= Length(eCommand.Text)) do begin

    if CharInSet(eCommand.Text[i], ['0'..'9']) or CharInSet(eCommand.Text[i], ['a'..'f'])
      or CharInSet(eCommand.Text[i], ['A'..'F']) then begin

      b := b * 16 + GetHexValue(uppercase(eCommand.Text)[i]);
      inc(j);

      if j = 2 then begin
        MemStream.Write(b, 1);
        j := 0;
      end;
    end;

    inc(i);
  end;

  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.SendCmdMessage( MemStream);
  end;
end;

procedure TfrmLog.Button2Click(Sender: TObject);
var sr : TSearchRec;
    i, p : integer;
    G2 : TG2;
    ModuleFileName : string;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if not assigned(G2) then
    exit;

  if FindFirst('C:\Users\Bruno\Delphi\nmg2editor\Build\Modules\' + '*.*', faAnyFile, sr) = 0 then begin
    repeat
      if (sr.Attr and faDirectory) = 0 then begin

        ModuleFileName := ExtractFileName(sr.Name);
        p := pos('.', ModuleFileName);
        if p > 0 then
          ModuleFileName := copy(ModuleFileName, 1, p-1);

        // Search module in ModuleDef
        i := 0;
        while (i < G2.FModuleDefList.Count) and ( G2.FModuleDefList.ModuleDef[i].ShortName <> ModuleFileName) do
          inc(i);

        if (i < G2.FModuleDefList.Count) then
          Memo1.Lines.Add( ModuleFileName)
        else
          Memo1.Lines.Add( ModuleFileName + ' not found in ModuleDef');
      end;
    until (FindNext(sr) <> 0);
    FindClose(sr);
  end;
end;

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  //frmG2Main.G2.LogLines := Memo1.Lines;
end;

procedure TfrmLog.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmLog.FormShow(Sender: TObject);
var G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin
    G2.AssignLog( Memo1.Lines);
  end;
end;

end.
