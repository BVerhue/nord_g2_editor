unit UnitFormAddModule;

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
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2FileIntf,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2,
  BVE.NMG2TexturesGL,
  UnitSelectModule;

type
  TfrmAddModule = class(TG2FormGL)
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FSelectModuleCtrl: TSelectModuleGL;
    procedure OnAddModule(Sender: TObject);
  end;

var
  frmAddModule: TfrmAddModule;

implementation

{$R *.fmx}

procedure TfrmAddModule.FormCreate(Sender: TObject);
begin
  FSelectModuleCtrl := TSelectModuleGL.Create(Self);
  FSelectModuleCtrl.Parent := Self;
  FSelectModuleCtrl.Align := TAlignLayout.Client;
  FSelectModuleCtrl.OnAddModule := OnAddModule;
end;

procedure TfrmAddModule.FormResize(Sender: TObject);
begin
  //
end;

procedure TfrmAddModule.FormShow(Sender: TObject);
begin
  FSelectModuleCtrl.CalcLayout;
end;

procedure TfrmAddModule.OnAddModule(Sender: TObject);
begin
{$IFDEF ANDROID}
  Close;
{$ENDIF}
end;

end.
