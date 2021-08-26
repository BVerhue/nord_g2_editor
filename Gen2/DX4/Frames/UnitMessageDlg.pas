unit UnitMessageDlg;

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
  System.Rtti,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Controls.Presentation;

type
  TframeMessageDlg = class(TFrame)
    lbMessage: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Rectangle1: TRectangle;
    Layout1: TLayout;
  private
    function GetLabelMessage: string;
    procedure SetLabelMessage(const Value: string);
  public
    property LabelMessage : string read GetLabelMessage write SetLabelMessage;
  end;

implementation

{$R *.fmx}

{ TframeMessageDlg }

function TframeMessageDlg.GetLabelMessage: string;
begin
  Result := lbMessage.Text;
end;

procedure TframeMessageDlg.SetLabelMessage(const Value: string);
begin
  lbMessage.Text := Value;
  lbMessage.Repaint;
end;

end.
