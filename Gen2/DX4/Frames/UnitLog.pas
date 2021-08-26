unit UnitLog;

// ////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011 Bruno Verhue
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// ////////////////////////////////////////////////////////////////////////////

{$I ..\Common\CompilerSettings.Inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Memo,
  BVE.NMG2ControlsFMX,
  BVE.NMG2File;

type
  TframeLog = class(TFrame)
    Panel1: TPanel;
    mLog: TMemo;
    G2BtnText1: TG2BtnText;
    G2BtnText2: TG2BtnText;
    procedure G2BtnText1ChangeValue(Sender: TObject; const aValue: Integer);
    procedure G2BtnText2ChangeValue(Sender: TObject; const aValue: Integer);
  private
    FSynth: TG2File;
  public
    property Synth: TG2File read FSynth write FSynth;
  end;

implementation

{$R *.fmx}

procedure TframeLog.G2BtnText1ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
  begin
    FSynth.AssignLog(mLog.Lines);
  end;
end;

procedure TframeLog.G2BtnText2ChangeValue(Sender: TObject;
  const aValue: Integer);
begin
  if aValue = 0 then
    FSynth.ClearLog;
end;

end.
