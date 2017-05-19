unit UnitSeqGrid;

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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinAPI.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}
{$IFDEF G2_VER220_up}
  WinAPI.Messages, System.SysUtils, System.Variants, System.Classes, VCL.Graphics,
  VCL.Controls, VCL.Forms, VCL.Dialogs, VCL.Grids,
{$ELSE}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids,
{$ENDIF}
  g2_types, g2_file, g2_graph, g2_classes, g2_midi;

type
  TfrmSeqGrid = class(TForm)
    DrawGrid1: TDrawGrid;
    b1: TG2GraphButtonText;
    b2: TG2GraphButtonText;
    b3: TG2GraphButtonText;
    b4: TG2GraphButtonText;
    b5: TG2GraphButtonText;
    b6: TG2GraphButtonText;
    b7: TG2GraphButtonText;
    b8: TG2GraphButtonText;
    b9: TG2GraphButtonText;
    b10: TG2GraphButtonText;
    b11: TG2GraphButtonText;
    b12: TG2GraphButtonText;
    b13: TG2GraphButtonText;
    b14: TG2GraphButtonText;
    b15: TG2GraphButtonText;
    b16: TG2GraphButtonText;
    bLeft: TG2GraphButtonText;
    bRight: TG2GraphButtonText;
    bRnd: TG2GraphButtonText;
    bClr: TG2GraphButtonText;
    bUp: TG2GraphButtonText;
    bDown: TG2GraphButtonText;
    bnC: TG2GraphButtonText;
    bnCsharp: TG2GraphButtonText;
    bnD: TG2GraphButtonText;
    bnDsharp: TG2GraphButtonText;
    bnE: TG2GraphButtonText;
    bnF: TG2GraphButtonText;
    bnFsharp: TG2GraphButtonText;
    bnG: TG2GraphButtonText;
    bnGsharp: TG2GraphButtonText;
    bnA: TG2GraphButtonText;
    bnAsharp: TG2GraphButtonText;
    bnB: TG2GraphButtonText;
    G2GraphDisplay1: TG2GraphDisplay;
    procedure FormCreate(Sender: TObject);
    procedure DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure DrawGrid1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bLeftMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bRightMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bUpMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bDownMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bRndMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bClrMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bNoteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    FModuleIndex :  integer;
    FLocation : TLocationType;
    FNoteQuant : array[0..11] of byte;
    FNoteQuantCount : integer;
    function  GetOctaaf( aValue : byte): byte;
    function  GetNoot( aValue : byte): byte;
    function  GetValue( aOctaaf, aNoot : byte): byte;
    function  GetModule : TG2FileModule;
    procedure SetModule( aModule: TG2FileModule);
    procedure InitBtn( aBtn, aValue : byte);
    procedure UpdateControls;
  end;

var
  frmSeqGrid: TfrmSeqGrid;

implementation

uses UnitG2Editor;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TfrmSeqGrid }

function TfrmSeqGrid.GetModule: TG2FileModule;
var Module : TG2FileModule;
    G2 : TG2;
begin
  Result := nil;
  G2 := frmG2Main.SelectedEditG2;
  if assigned(G2) then begin
    Module := G2.SelectedPatch.GetModule( ord(FLocation), FModuleIndex);
    if not( assigned(Module) and (Module.TypeID = 121)) then begin
      Result := nil;
      Close;
    end else
      Result := Module;
  end;
end;

function TfrmSeqGrid.GetNoot(aValue: byte): byte;
begin
  Result := aValue - (12 * GetOctaaf(aValue) + 28);
end;

function TfrmSeqGrid.GetOctaaf(aValue: byte): byte;
begin
  Result := (aValue - 28) div 12;
end;

function TfrmSeqGrid.GetValue(aOctaaf, aNoot: byte): byte;
begin
  Result := aOctaaf * 12 + aNoot + 28;
end;

procedure TfrmSeqGrid.bClrMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : integer;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    for i := 0 to 15 do
      Module.Parameter[ 0 + i].SetParameterValue( 64);
  end;
  Update;
end;

procedure TfrmSeqGrid.bRndMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, j, k, d, nd : integer;
    Module : TG2FileModule;
    Value, Oct, Note : byte;
begin
  Module := GetModule;
  if assigned( Module) then begin
    for i := 0 to 15 do begin
      Value := Module.Parameter[ 0 + i].GetParameterValue + (trunc(Random * 36) - 18);
      if FNoteQuantCount > 0 then begin
        Oct := GetOctaaf( Value);
        Note := GetNoot( Value);
        d := 12;
        k := 0;
        for j := 0 to FNoteQuantCount - 1 do begin
          nd := abs(FNoteQuant[j] - Note);
          if nd<d then begin
            d := nd;
            k := j;
          end
        end;
        Value := GetValue(Oct, FNoteQuant[k]);
      end;
      Module.Parameter[ 0 + i].SetParameterValue( Value);
    end;
  end;
  Update;
end;

procedure TfrmSeqGrid.bDownMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : integer;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    for i := 0 to 15 do
      Module.Parameter[ 0 + i].SetParameterValue( Module.Parameter[ 0 + i].GetParameterValue - 1);
  end;
  Update;
end;

procedure TfrmSeqGrid.bUpMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : integer;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    for i := 0 to 15 do
      Module.Parameter[ 0 + i].SetParameterValue( Module.Parameter[ 0 + i].GetParameterValue + 1);
  end;
  Update;
end;

procedure TfrmSeqGrid.bLeftMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var NoteValue, BtnValue : byte;
    i : integer;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    BtnValue  := Module.Parameter[16].GetParameterValue;
    NoteValue := Module.Parameter[0].GetParameterValue;
    for i := 1 to 15 do begin
      Module.Parameter[16 + i - 1].SetParameterValue( Module.Parameter[16 + i].GetParameterValue);
      Module.Parameter[ 0 + i - 1].SetParameterValue( Module.Parameter[ 0 + i].GetParameterValue);
    end;
    Module.Parameter[31].SetParameterValue( BtnValue);
    Module.Parameter[15].SetParameterValue( NoteValue);
  end;
  Update;
end;

procedure TfrmSeqGrid.bNoteMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i : integer;

  procedure SetNoteQuant( aValue : byte);
  begin
    FNoteQuant[ FNoteQuantCount] := aValue;
    inc(FNoteQuantCount);
  end;

begin
  FNoteQuantCount := 0;
  for i := 0 to 11 do begin
    case i of
    0  : if bnC.Value = 1      then SetNoteQuant(i);
    1  : if bnCsharp.Value = 1 then SetNoteQuant(i);
    2  : if bnD.Value = 1      then SetNoteQuant(i);
    3  : if bnDsharp.Value = 1 then SetNoteQuant(i);
    4  : if bnE.Value = 1      then SetNoteQuant(i);
    5  : if bnF.Value = 1      then SetNoteQuant(i);
    6  : if bnFsharp.Value = 1 then SetNoteQuant(i);
    7  : if bnG.Value = 1      then SetNoteQuant(i);
    8  : if bnGsharp.Value = 1 then SetNoteQuant(i);
    9  : if bnA.Value = 1      then SetNoteQuant(i);
    10 : if bnAsharp.Value = 1 then SetNoteQuant(i);
    11 : if bnB.Value = 1      then SetNoteQuant(i);
    end;
  end;
end;

procedure TfrmSeqGrid.bRightMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var NoteValue, BtnValue : byte;
    i : integer;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    BtnValue  := Module.Parameter[31].GetParameterValue;
    NoteValue := Module.Parameter[15].GetParameterValue;
    for i := 14 downto 0 do begin
      Module.Parameter[16 + i + 1].SetParameterValue( Module.Parameter[16 + i].GetParameterValue);
      Module.Parameter[ 0 + i + 1].SetParameterValue( Module.Parameter[ 0 + i].GetParameterValue);
    end;
    Module.Parameter[16].SetParameterValue( BtnValue);
    Module.Parameter[ 0].SetParameterValue( NoteValue);
  end;
  Update;
end;

procedure TfrmSeqGrid.BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    Param := Module.Parameter[16 + (Sender as TG2GraphButtonText).Tag];
    if (assigned(Param)) then begin
      Param.SetParameterValue((Sender as TG2GraphButtonText).Value);
    end;
  end;
end;

procedure TfrmSeqGrid.DrawGrid1Click(Sender: TObject);
var Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    Param := Module.Parameter[DrawGrid1.Col];
    if (assigned(Param)) then begin
      Param.SetParameterValue(128 - DrawGrid1.Row);
      DrawGrid1.Invalidate;
    end;
  end;
end;

procedure TfrmSeqGrid.DrawGrid1DrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    Param := Module.Parameter[aCol];
    if (assigned(Param)) and (Param.GetParameterValue = 128 - ARow) then
      (Sender as TDrawGrid).Canvas.Brush.Color := clAqua
    else
      (Sender as TDrawGrid).Canvas.Brush.Color := clBlue;
    (Sender as TDrawGrid).Canvas.FillRect( Rect);
  end;
end;

procedure TfrmSeqGrid.FormActivate(Sender: TObject);
begin
  Update;
end;

procedure TfrmSeqGrid.FormCreate(Sender: TObject);
begin
  FModuleIndex := 0;
  FLocation := ltVA;
end;

procedure TfrmSeqGrid.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmSeqGrid.InitBtn(aBtn, aValue: byte);
begin
  case aBtn of
   0 : b1.Value := aValue;
   1 : b2.Value := aValue;
   2 : b3.Value := aValue;
   3 : b4.Value := aValue;
   4 : b5.Value := aValue;
   5 : b6.Value := aValue;
   6 : b7.Value := aValue;
   7 : b8.Value := aValue;
   8 : b9.Value := aValue;
   9 : b10.Value := aValue;
  10 : b11.Value := aValue;
  11 : b12.Value := aValue;
  12 : b13.Value := aValue;
  13 : b14.Value := aValue;
  14 : b15.Value := aValue;
  15 : b16.Value := aValue;
  end;
end;

procedure TfrmSeqGrid.SetModule(aModule: TG2FileModule);
begin
  if aModule.TypeID <> 121 then
    raise Exception.Create('Module is not a note sequencer.');

  FModuleIndex := aModule.ModuleIndex;
  FLocation := aModule.Location;
end;

procedure TfrmSeqGrid.UpdateControls;
var i : integer;
    Param : TG2FileParameter;
    Module : TG2FileModule;
begin
  Module := GetModule;
  if assigned( Module) then begin
    DrawGrid1.Invalidate;
    for i := 0 to 15 do begin
      Param := Module.Parameter[16 + i];
      if assigned(Param) then
        InitBtn( i, Param.GetParameterValue);
    end;
  end;
end;

end.
