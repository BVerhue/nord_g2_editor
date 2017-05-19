unit UnitPatchBrowserFilterModules;

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.CheckLst,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, CheckLst,
{$ENDIF}
  g2_database;

type
  TfrmPatchBrowserModuleFilter = class(TForm)
    cblModules: TCheckListBox;
    Panel1: TPanel;
    bOk: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FSelectedModules : TStringList;
    procedure UpdateModules( aModuleDefList : TXMLModuleDefListType);
    function ModuleChecked( aModuleID : integer): boolean;
    procedure UpdateSelectedModules;
  end;

var
  frmPatchBrowserModuleFilter: TfrmPatchBrowserModuleFilter;

implementation

{$R *.dfm}

procedure TfrmPatchBrowserModuleFilter.UpdateModules( aModuleDefList : TXMLModuleDefListType);
var m : integer;
    sl : TStringList;
    i : integer;
begin
  sl := TStringList.Create;
  sl.Sorted := True;
  for m := 0 to aModuleDefList.Count - 1 do begin
    i := sl.Add( aModuleDefList.ModuleDef[m].ShortName);
    sl.Objects[i] := pointer(aModuleDefList.ModuleDef[m].ModuleType);
  end;

  for m := 0 to sl.Count - 1 do begin
    i := cblModules.Items.Add(sl[m]);
    cblModules.Items.Objects[i] := sl.Objects[m];
  end;
end;

procedure TfrmPatchBrowserModuleFilter.FormCreate(Sender: TObject);
begin
  FSelectedModules := TStringList.Create;
end;

procedure TfrmPatchBrowserModuleFilter.FormDestroy(Sender: TObject);
begin
  FSelectedModules.Free;
end;

function TfrmPatchBrowserModuleFilter.ModuleChecked( aModuleID : integer): boolean;
var i : integer;
begin
  i := 0;
  while (i<cblModules.Items.Count) and (integer(cblModules.Items.Objects[i]) <> aModuleID) do
    inc(i);

  if (i<cblModules.Items.Count) then begin
    Result := cblModules.Checked[i]
  end else
    Result := False;
end;

procedure TfrmPatchBrowserModuleFilter.UpdateSelectedModules;
var i, m : integer;
begin
  FSelectedModules.Clear;
  for i := 0 to cblModules.Count - 1 do begin
    if cblModules.Checked[i] then begin
      m := FSelectedModules.Add(cblModules.Items[i]);
      FSelectedModules.Objects[m] := cblModules.Items.Objects[i];
    end;
  end;


end;

end.
