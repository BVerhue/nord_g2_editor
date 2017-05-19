unit UnitParameterPages;

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

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
{$IFDEF FPC}
{$ELSE}
  {$IFDEF G2_VER220_up}
    WinApi.Windows,
  {$ELSE}
    Windows,
  {$ENDIF}
{$ENDIF}
{$IFDEF G2_VER220_up}
  WinApi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
{$ELSE}
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
{$ENDIF}
  g2_graph, g2_midi, g2_types, G2_File, G2_classes,
  graph_util_vcl, g2_database, DOM, XMLRead, XMLWrite;

type
  TfrmParameterPages = class(TForm)
    skP1: TG2GraphKnob;
    skP2: TG2GraphKnob;
    skP3: TG2GraphKnob;
    skP4: TG2GraphKnob;
    skP5: TG2GraphKnob;
    skP6: TG2GraphKnob;
    skP7: TG2GraphKnob;
    skP8: TG2GraphKnob;
    Disp1A: TG2GraphDisplay;
    Disp2A: TG2GraphDisplay;
    Disp3A: TG2GraphDisplay;
    Disp4A: TG2GraphDisplay;
    Disp5A: TG2GraphDisplay;
    Disp6A: TG2GraphDisplay;
    Disp7A: TG2GraphDisplay;
    Disp8A: TG2GraphDisplay;
    Disp1B: TG2GraphDisplay;
    disp2B: TG2GraphDisplay;
    Disp3B: TG2GraphDisplay;
    Disp4B: TG2GraphDisplay;
    Disp5B: TG2GraphDisplay;
    Disp6B: TG2GraphDisplay;
    Disp7B: TG2GraphDisplay;
    Disp8B: TG2GraphDisplay;
    bfP1: TG2GraphButtonFlat;
    bfP2: TG2GraphButtonFlat;
    bfP3: TG2GraphButtonFlat;
    bfP4: TG2GraphButtonFlat;
    bfP5: TG2GraphButtonFlat;
    bfP6: TG2GraphButtonFlat;
    bfP7: TG2GraphButtonFlat;
    bfP8: TG2GraphButtonFlat;
    Disp1C: TG2GraphDisplay;
    Disp2C: TG2GraphDisplay;
    Disp3C: TG2GraphDisplay;
    Disp4C: TG2GraphDisplay;
    Disp5C: TG2GraphDisplay;
    Disp6C: TG2GraphDisplay;
    Disp7C: TG2GraphDisplay;
    Disp8C: TG2GraphDisplay;
    pRight: TG2GraphPanel;
    rbParamPage: TG2GraphButtonRadio;
    pBottom: TG2GraphPanel;
    rbSlot: TG2GraphButtonRadio;
    rbVariation: TG2GraphButtonRadio;
    rbMode: TG2GraphButtonRadio;
    rbParamColumn: TG2GraphButtonRadio;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btGlobalPagesChange(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure skPMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bfPMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbParamPageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbParamPageChange(Sender: TObject);
    procedure rbParamColumnChange(Sender: TObject);
    procedure rbParamColumnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbVariationClick(Sender: TObject);
    procedure rbSlotClick(Sender: TObject);
    procedure rbSlotChange(Sender: TObject);
    procedure rbModeClick(Sender: TObject);
    procedure rbModeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbVariationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbSlotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rbModeChange(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FExtBitmap       : TBitmap;
    FKnobArray       : array[0..7] of TG2GraphKnob;
    FDispKnobArray   : array[0..7] of TG2GraphDisplay;
    FDispModuleArray : array[0..7] of TG2GraphDisplay;
    FButtonArray     : array[0..7] of TG2GraphButtonFlat;
    FDispButtonArray : array[0..7] of TG2GraphDisplay;
{$IFDEF FPC}
{$ELSE}
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message WM_ERASEBKGND;
{$ENDIF}
  public
    { Public declarations }
    procedure LoadIniXML;
    procedure UpdateControls;
    procedure UpdateColorScema;
    function GetKnobIndexOffset : integer;
  end;

var
  frmParameterPages: TfrmParameterPages;

implementation
uses
  UnitG2Editor, UnitMidiMapping;

{$IFNDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

{ TfrmParameterPages }

procedure TfrmParameterPages.FormCreate(Sender: TObject);
begin
  FExtBitmap := TBitmap.Create;
  FExtBitMap.PixelFormat := pf24Bit;

  FDispModuleArray[0] := Disp1B;
  FDispModuleArray[1] := Disp2B;
  FDispModuleArray[2] := Disp3B;
  FDispModuleArray[3] := Disp4B;
  FDispModuleArray[4] := Disp5B;
  FDispModuleArray[5] := Disp6B;
  FDispModuleArray[6] := Disp7B;
  FDispModuleArray[7] := Disp8B;

  FDispKnobArray[0] := Disp1A;
  FDispKnobArray[1] := Disp2A;
  FDispKnobArray[2] := Disp3A;
  FDispKnobArray[3] := Disp4A;
  FDispKnobArray[4] := Disp5A;
  FDispKnobArray[5] := Disp6A;
  FDispKnobArray[6] := Disp7A;
  FDispKnobArray[7] := Disp8A;

  FKnobArray[0] := skP1;
  FKnobArray[1] := skP2;
  FKnobArray[2] := skP3;
  FKnobArray[3] := skP4;
  FKnobArray[4] := skP5;
  FKnobArray[5] := skP6;
  FKnobArray[6] := skP7;
  FKnobArray[7] := skP8;

  FButtonArray[0] := bfP1;
  FButtonArray[1] := bfP2;
  FButtonArray[2] := bfP3;
  FButtonArray[3] := bfP4;
  FButtonArray[4] := bfP5;
  FButtonArray[5] := bfP6;
  FButtonArray[6] := bfP7;
  FButtonArray[7] := bfP8;

  FDispButtonArray[0] := Disp1C;
  FDispButtonArray[1] := Disp2C;
  FDispButtonArray[2] := Disp3C;
  FDispButtonArray[3] := Disp4C;
  FDispButtonArray[4] := Disp5C;
  FDispButtonArray[5] := Disp6C;
  FDispButtonArray[6] := Disp7C;
  FDispButtonArray[7] := Disp8C;

  LoadIniXML;
end;

procedure TfrmParameterPages.FormDestroy(Sender: TObject);
begin
  FExtBitmap.Free;
end;

procedure TfrmParameterPages.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  frmG2Main.HandleMainKeyDown( Key, Shift);
end;

procedure TfrmParameterPages.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmParameterPages.LoadIniXML;
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
      FormSettingsNode := TXMLFormSettingsType(RootNode.FindNode('ParameterPagesForm'));
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

{$IFDEF FPC}
{$ELSE}
procedure TfrmParameterPages.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1;
end;

procedure TfrmParameterPages.WMPaint(var Msg: TWMPaint);
var PS        : TPaintStruct;
    i         : integer;
    Rect      : TRect;
    Control   : TG2GraphChildControl;
begin
  BeginPaint(Handle, PS);

  if PS.rcPaint.Right - PS.rcPaint.Left > FExtBitmap.Width then
    FExtBitmap.Width := PS.rcPaint.Right - PS.rcPaint.Left;
  if PS.rcPaint.Bottom - PS.rcPaint.Top > FExtBitmap.Height then
    FExtBitmap.Height := PS.rcPaint.Bottom - PS.rcPaint.Top;

  //PaintBackGround( FExtBitmap.Canvas, PS.rcPaint);
  Rect.Left := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Top := 0;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  FExtBitmap.Canvas.Brush.Color := Color;
  FExtBitmap.Canvas.FillRect( Rect);

  for i := 0 to ControlCount - 1 do begin

    if Controls[i] is TG2GraphChildControl then begin
      Control := Controls[i] as TG2GraphChildControl;
      if RectOverlap( Control.BoundsRect, PS.rcPaint) then
        Control.PaintOn( FExtBitmap.Canvas, PS.rcPaint);
      Msg.Result := 0;
    end;
  end;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Right := PS.rcPaint.Right - PS.rcPaint.Left;
  Rect.Bottom := PS.rcPaint.Bottom - PS.rcPaint.Top;

  Canvas.CopyRect( PS.rcPaint, FExtBitmap.Canvas, Rect);

  EndPaint(Handle, PS);
end;
{$ENDIF}

procedure TfrmParameterPages.FormShow(Sender: TObject);
begin
  UpdateControls;
end;

function TfrmParameterPages.GetKnobIndexOffset: integer;
begin
  Result := rbParamColumn.Value * 8 + rbParamPage.Value * 8 * 3;
end;

procedure TfrmParameterPages.rbModeChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.rbModeClick(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.rbModeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := rbMode.ClientToScreen( Point(0, rbMode.Height));
    frmMidiMapping.PopupMenu( rbMode, P.X, P.Y);
    rbMode.Invalidate;
  end;
end;

procedure TfrmParameterPages.rbParamColumnChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.rbParamColumnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := rbParamColumn.ClientToScreen( Point(0, rbParamColumn.Height));
    frmMidiMapping.PopupMenu( rbParamColumn, P.X, P.Y);
    rbParamColumn.Invalidate;
  end;
end;

procedure TfrmParameterPages.rbParamPageChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.rbParamPageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := rbParamPage.ClientToScreen( Point(0, rbParamPage.Height));
    frmMidiMapping.PopupMenu( rbParamPage, P.X, P.Y);
    rbParamPage.Invalidate;
  end;
end;

procedure TfrmParameterPages.rbSlotChange(Sender: TObject);
begin
  //UpdateControls;
end;

procedure TfrmParameterPages.rbSlotClick(Sender: TObject);
begin
  frmG2Main.SelectSlot( rbSlot.Value);
end;

procedure TfrmParameterPages.rbSlotMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := rbSlot.ClientToScreen( Point(0, rbSlot.Height));
    frmMidiMapping.PopupMenu( rbSlot, P.X, P.Y);
    rbSlot.Invalidate;
  end;
end;

procedure TfrmParameterPages.rbVariationClick(Sender: TObject);
begin
  frmG2Main.SelectVariation( rbSlot.Value, rbVariation.Value);
end;

procedure TfrmParameterPages.rbVariationMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var P : TPoint;
begin
  if ssRight in Shift then begin
    P := rbVariation.ClientToScreen( Point(0, rbVariation.Height));
    frmMidiMapping.PopupMenu( rbVariation, P.X, P.Y);
    rbVariation.Invalidate;
  end;
end;

procedure TfrmParameterPages.skPMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Knob : TG2GraphKnob;
    P : TPoint;
begin
  if Sender is TG2GraphKnob then begin
    Knob := Sender as TG2GraphKnob;
    if assigned(Knob.Parameter) then
      Knob.Parameter.Selected := True;

    if ssRight in Shift then begin
      P := Knob.ClientToScreen( Point(0, 0));
      frmMidiMapping.PopupMenu( Knob, P.X, P.Y);
      Knob.Invalidate;
    end;
  end;
end;

procedure TfrmParameterPages.bfPMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Btn : TG2GraphButtonFlat;
    P : TPoint;
begin
  if Sender is TG2GraphButtonFlat then begin
    Btn :=  Sender as TG2GraphButtonFlat;
    if assigned(Btn.Parameter) then
      Btn.Parameter.Selected := True;

    if ssRight in Shift then begin
      P := Btn.ClientToScreen( Point(0, 0));
      frmMidiMapping.PopupMenu( Btn, P.X, P.Y);
      Btn.Invalidate;
    end;
  end;
end;

procedure TfrmParameterPages.btGlobalPagesChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TfrmParameterPages.UpdateControls;
var i : integer;
    Perf : TG2Performance;
    Patch : TG2Patch;
    Module : TG2FileModule;
    Knob : TKnob;
    G2 : TG2;
begin
  G2 := frmG2Main.SelectedCtrlG2;
  if assigned(G2) then begin

    rbSlot.Value := G2.SelectedSlotIndex;
    rbVariation.Value := G2.Patch[ rbSlot.Value].ActiveVariation;

    UpdateColorScema;

    case rbMode.Value of
    0 : begin // Params
          Patch := G2.SelectedPatch as TG2Patch;
          for i := 0 to 7 do begin

            Knob := Patch.GetKnob( GetKnobIndexOffset + i);
            FDispKnobArray[i].DisplayType := 1;
            FDispModuleArray[i].DisplayType := 5;
            FDispButtonArray[i].DisplayType := 3;

            if assigned(Knob) and (Knob.IsAssigned = 1) then begin

              FKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
              FDispKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);

              if (i>0) and assigned(FDispModuleArray[i-1].Parameter)
                and assigned( Knob.Parameter)
                and (Knob.Parameter.Module = FDispModuleArray[i-1].Parameter.Module) then
                FDispModuleArray[i].Visible := false
              else
                FDispModuleArray[i].Visible := True;

              FDispModuleArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);

              if assigned(Knob.Parameter.ButtonParam) then begin
                FButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
                FDispButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
              end else begin
                FButtonArray[i].SetParameter( nil);
                FDispButtonArray[i].SetParameter( nil);
              end;

            end else begin

              FKnobArray[i].SetParameter( nil);
              FDispKnobArray[i].SetParameter( nil);
              FDispModuleArray[i].SetParameter( nil);
              FButtonArray[i].SetParameter( nil);
              FDispButtonArray[i].SetParameter(nil);

            end;
          end;
        end;
    1 : begin // Global params
          Perf := G2.Performance;
          for i := 0 to 7 do begin

            Knob := Perf.GetGlobalKnob( GetKnobIndexOffset + i);
            FDispKnobArray[i].DisplayType := 1;
            FDispModuleArray[i].DisplayType := 4;
            FDispButtonArray[i].DisplayType := 3;

            if assigned(Knob) and (Knob.IsAssigned = 1) then begin

              FKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);
              FDispKnobArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);

              if (i>0) and assigned(FDispModuleArray[i-1].Parameter)
                and assigned( Knob.Parameter)
                and (Knob.Parameter.Module = FDispModuleArray[i-1].Parameter.Module) then
                FDispModuleArray[i].Visible := false
              else
                FDispModuleArray[i].Visible := True;

              FDispModuleArray[i].SetParameter( Knob.Parameter as TG2GraphParameter);

              if assigned(Knob.Parameter.ButtonParam) then begin
                FButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
                FDispButtonArray[i].SetParameter( Knob.Parameter.ButtonParam);
              end else begin
                FButtonArray[i].SetParameter( nil);
                FDispButtonArray[i].SetParameter( nil);
              end;

            end else begin
              FKnobArray[i].SetParameter( nil);
              FDispKnobArray[i].SetParameter( nil);
              FDispModuleArray[i].SetParameter( nil);
              FButtonArray[i].SetParameter( nil);
              FDispButtonArray[i].SetParameter(nil);
            end;
          end;
        end;
    2 : begin // Morphs
          Patch := G2.SelectedPatch as TG2Patch;
          for i := 0 to 7 do begin
            FDispKnobArray[i].DisplayType := 1;
            FDispModuleArray[i].DisplayType := 5;
            FDispButtonArray[i].DisplayType := 3;

            FKnobArray[i].SetParameter( Patch.Parameter[ PATCH_MORPH, i] as TG2GraphParameter);
            FDispKnobArray[i].SetParameter( Patch.Parameter[ PATCH_MORPH, i] as TG2GraphParameter);
            FDispModuleArray[i].SetParameter( Patch.Parameter[ PATCH_MORPH, i] as TG2GraphParameter);
            FButtonArray[i].SetParameter( Patch.Parameter[ PATCH_MORPH, i + 8] as TG2GraphParameter);
            FDispButtonArray[i].SetParameter( Patch.Parameter[ PATCH_MORPH, i + 8] as TG2GraphParameter);
          end;
        end;
    3 : begin // Patch
          Patch := G2.SelectedPatch as TG2Patch;
          for i := 0 to 7 do begin
            FDispKnobArray[i].DisplayType := 1;
            FDispModuleArray[i].DisplayType := 5;
            FDispButtonArray[i].DisplayType := 3;
          end;

          // Master clock
          FKnobArray[0].SetParameter( Patch.Parameter[ PATCH_MASTERCLOCK, 0] as TG2GraphParameter);
          FDispKnobArray[0].SetParameter( Patch.Parameter[ PATCH_MASTERCLOCK, 0] as TG2GraphParameter);
          FDispModuleArray[0].SetParameter( Patch.Parameter[ PATCH_MASTERCLOCK, 0] as TG2GraphParameter);
          // Master clock stop/run
          FButtonArray[0].SetParameter( Patch.Parameter[ PATCH_MASTERCLOCK, 1] as TG2GraphParameter);
          FDispButtonArray[0].SetParameter( Patch.Parameter[ PATCH_MASTERCLOCK, 1] as TG2GraphParameter);

          // Voice mode
          FKnobArray[1].SetParameter( Patch.Parameter[ PATCH_VOICES, 0] as TG2GraphParameter);
          FDispKnobArray[1].SetParameter( Patch.Parameter[ PATCH_VOICES, 0] as TG2GraphParameter);
          FDispModuleArray[1].SetParameter( Patch.Parameter[ PATCH_VOICES, 0] as TG2GraphParameter);
          // Voice mode (Poly, Mono, Legato)
          FButtonArray[1].SetParameter( Patch.Parameter[ PATCH_VOICES, 1] as TG2GraphParameter);
          FDispButtonArray[1].SetParameter( Patch.Parameter[ PATCH_VOICES, 1] as TG2GraphParameter);

          // Arpeggiator speed
          FKnobArray[2].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED] as TG2GraphParameter);
          FDispKnobArray[2].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED] as TG2GraphParameter);
          FDispModuleArray[2].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_SPEED] as TG2GraphParameter);
          // Arpeggiator on/off
          FButtonArray[2].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_ON_OFF] as TG2GraphParameter);
          FDispButtonArray[2].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_ON_OFF] as TG2GraphParameter);

          // Arpeggiator direction
          FKnobArray[3].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_DIRECTION] as TG2GraphParameter);
          FDispKnobArray[3].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_DIRECTION] as TG2GraphParameter);
          FDispModuleArray[3].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_DIRECTION] as TG2GraphParameter);
          // Arpeggiator octaves
          FButtonArray[3].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_OCTAVES] as TG2GraphParameter);
          FDispButtonArray[3].SetParameter( Patch.Parameter[ PATCH_ARPEGGIATOR, ARP_OCTAVES] as TG2GraphParameter);

          // Vibrato
          FKnobArray[4].SetParameter( Patch.Parameter[ PATCH_VIBRATO, VIBRATO_DEPTH] as TG2GraphParameter);
          FDispKnobArray[4].SetParameter( Patch.Parameter[ PATCH_VIBRATO, VIBRATO_DEPTH] as TG2GraphParameter);
          FDispModuleArray[4].SetParameter( Patch.Parameter[ PATCH_VIBRATO, VIBRATO_DEPTH] as TG2GraphParameter);
          // Vibrato mod source
          FButtonArray[4].SetParameter( Patch.Parameter[ PATCH_VIBRATO, VIBRATO_MOD] as TG2GraphParameter);
          FDispButtonArray[4].SetParameter( Patch.Parameter[ PATCH_VIBRATO, VIBRATO_MOD] as TG2GraphParameter);

          // Glide speed
          FKnobArray[5].SetParameter( Patch.Parameter[ PATCH_GLIDE, GLIDE_SPEED] as TG2GraphParameter);
          FDispKnobArray[5].SetParameter( Patch.Parameter[ PATCH_GLIDE, GLIDE_SPEED] as TG2GraphParameter);
          FDispModuleArray[5].SetParameter( Patch.Parameter[ PATCH_GLIDE, GLIDE_SPEED] as TG2GraphParameter);
          // Glide type
          FButtonArray[5].SetParameter( Patch.Parameter[ PATCH_GLIDE, GLIDE_TYPE] as TG2GraphParameter);
          FDispButtonArray[5].SetParameter( Patch.Parameter[ PATCH_GLIDE, GLIDE_TYPE] as TG2GraphParameter);

          // Bend range
          FKnobArray[6].SetParameter( Patch.Parameter[ PATCH_BEND, BEND_RANGE] as TG2GraphParameter);
          FDispKnobArray[6].SetParameter( Patch.Parameter[ PATCH_BEND, BEND_RANGE] as TG2GraphParameter);
          FDispModuleArray[6].SetParameter( Patch.Parameter[ PATCH_BEND, BEND_RANGE] as TG2GraphParameter);
          // Bend on/off
          FButtonArray[6].SetParameter( Patch.Parameter[ PATCH_BEND, BEND_ON_OFF] as TG2GraphParameter);
          FDispButtonArray[6].SetParameter( Patch.Parameter[ PATCH_BEND, BEND_ON_OFF] as TG2GraphParameter);

          // Volume
          FKnobArray[7].SetParameter( Patch.Parameter[ PATCH_VOLUME, VOLUME_LEVEL] as TG2GraphParameter);
          FDispKnobArray[7].SetParameter( Patch.Parameter[ PATCH_VOLUME, VOLUME_LEVEL] as TG2GraphParameter);
          FDispModuleArray[7].SetParameter( Patch.Parameter[ PATCH_VOLUME, VOLUME_LEVEL] as TG2GraphParameter);
          // Mute
          FButtonArray[7].SetParameter( Patch.Parameter[ PATCH_VOLUME, VOLUME_MUTE] as TG2GraphParameter);
          FDispButtonArray[7].SetParameter( Patch.Parameter[ PATCH_VOLUME, VOLUME_MUTE] as TG2GraphParameter);
        end;
    end;
  end;
  Invalidate;
end;

procedure TfrmParameterPages.UpdateColorScema;
begin
  rbParamPage.HightlightColor := G_HighLightColor;
  rbParamColumn.HightlightColor := G_HighlightColor;
  rbVariation.HightlightColor := G_HighlightColor;
  rbSlot.HightlightColor := G_HighlightColor;
  rbMode.HightlightColor := G_HighlightColor;
end;



end.
