unit UnitKeyboard;

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
  System.Math.Vectors,
  System.Generics.Collections,
  FMX.Forms,
  FMX.Types,
  FMX.Controls,
  FMX.Graphics,
  FMX.Types3D,
  FMX.Viewport3D,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Objects,
  FMX.MaterialSources,
  BVE.NMG2Types,
  BVE.NMG2GraphTypes,
  BVE.NMG2Data,
  BVE.NMG2FileIntf,
  BVE.NMG2TexturesGL,
  BVE.NMG2Controls,
  BVE.NMG2ControlsG2,
  UnitFormSlot;

type
  TKeyboardPanelGL = class(TG2PanelGL)
  private
    FOctaves: array[0..8] of TKeyboardGL;
    FKeyValue: integer;

    procedure KeyChange(Sender: TObject);
    procedure SetKeyValue(const Value: integer);
  protected
    procedure Resize; override;
    procedure SetTextureList(const Value: TTextureListGL); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LayoutControls; override;

    procedure UpdateControls;

    property KeyValue: integer read FKeyValue write SetKeyValue;
  end;


implementation

{ TKeyboardPanelGL }

constructor TKeyboardPanelGL.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited;

  FKeyValue := -1;

  for i := 0 to 7 do
  begin
    FOctaves[i] := TKeyboardGL.Create(Self);
    FOctaves[i].Parent := Self;
    FOctaves[i].ID := i;
    FOctaves[i].OnChange := KeyChange;
  end;
end;

destructor TKeyboardPanelGL.Destroy;
begin
  inherited;
end;

procedure TKeyboardPanelGL.KeyChange(Sender: TObject);
var
  Keyboard: TKeyboardGL;
begin
  if Sender is TKeyboardGL then
  begin
    Keyboard := (Sender as TKeyboardGL);
    if Keyboard.Value = -1 then
      KeyValue := -1
    else
      KeyValue := Keyboard.Value + 12 * Keyboard.ID;
  end;
end;

procedure TKeyboardPanelGL.LayoutControls;
var
  FMXObject: TFMXObject;
  Octave: TKeyboardGL;
  DX, Margin: single;
begin
  Margin := 0 * SCALE_X;
  if assigned(Content) and assigned(Content.Children) then
  begin
    DX := Margin;
    for FMXObject in Content.Children do
      if (FMXObject is TKeyboardGL) then
      begin
        Octave := FMXObject as TKeyboardGL;
        Octave.Position := Point3D(DX, 0, 0);
        DX := DX + Octave.Width + Margin;
      end;
    LayoutRect := CalcLayoutRect;
    Invalidate;
  end;
end;

procedure TKeyboardPanelGL.Resize;
begin
  inherited;
  LayoutControls;
end;

procedure TKeyboardPanelGL.SetKeyValue(const Value: integer);
begin
  if FKeyValue <> Value then
  begin
    if FKeyValue <> -1 then
      ConMan.SelectedConnection.SynthNoteOnOff(FKeyValue, $01);

    FKeyValue := Value;

    if FKeyValue <> -1 then
      ConMan.SelectedConnection.SynthNoteOnOff(FKeyValue, $00);
  end;
end;

procedure TKeyboardPanelGL.SetTextureList(const Value: TTextureListGL);
begin
  inherited;
  LayoutControls;
end;

procedure TKeyboardPanelGL.UpdateControls;
begin
//
end;

end.
