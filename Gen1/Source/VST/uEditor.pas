unit uEditor;

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
//  This vst was based on Tobybear's template : http://www.tobybear.de/
//
//  ////////////////////////////////////////////////////////////////////////////

interface
uses Windows, Forms, DAudioEffectX, Messages, ExtCtrls,
     Classes, Graphics, StdCtrls, Controls, DVstUtils,
     DVstTemplate, DAEffectX,
     g2_types, g2_graph, g2_file, g2_usb, g2_classes, g2_midi;

type
  TPluginEditorWindow = class(TForm)
    Updater: TTimer;
    G2GraphPanel1: TG2GraphPanel;
    skP1: TG2GraphKnob;
    skP2: TG2GraphKnob;
    skP3: TG2GraphKnob;
    skP4: TG2GraphKnob;
    skP5: TG2GraphKnob;
    skP6: TG2GraphKnob;
    skP7: TG2GraphKnob;
    skP8: TG2GraphKnob;
    G2GraphPanel2: TG2GraphPanel;
    rbPage: TG2GraphButtonRadio;
    rbPageColumn: TG2GraphButtonRadio;
    G2GraphLabel1: TG2GraphLabel;
    Panel1: TPanel;
    lbStatus: TLabel;
    bfP1: TG2GraphButtonFlat;
    bfP2: TG2GraphButtonFlat;
    bfP3: TG2GraphButtonFlat;
    bfP4: TG2GraphButtonFlat;
    bfP5: TG2GraphButtonFlat;
    bfP6: TG2GraphButtonFlat;
    bfP7: TG2GraphButtonFlat;
    bfP8: TG2GraphButtonFlat;
    Disp1A: TG2GraphDisplay;
    Disp2A: TG2GraphDisplay;
    Disp3A: TG2GraphDisplay;
    Disp4A: TG2GraphDisplay;
    Disp5A: TG2GraphDisplay;
    Disp6A: TG2GraphDisplay;
    Disp7A: TG2GraphDisplay;
    Disp8A: TG2GraphDisplay;
    Disp1B: TG2GraphDisplay;
    Disp2B: TG2GraphDisplay;
    Disp3B: TG2GraphDisplay;
    Disp4B: TG2GraphDisplay;
    Disp5B: TG2GraphDisplay;
    Disp6B: TG2GraphDisplay;
    Disp7B: TG2GraphDisplay;
    Disp8B: TG2GraphDisplay;
    Disp1C: TG2GraphDisplay;
    Disp2C: TG2GraphDisplay;
    Disp3C: TG2GraphDisplay;
    Disp4C: TG2GraphDisplay;
    Disp5C: TG2GraphDisplay;
    Disp6C: TG2GraphDisplay;
    Disp7C: TG2GraphDisplay;
    Disp8C: TG2GraphDisplay;
    G2GraphPanel3: TG2GraphPanel;
    rbVariationA: TG2GraphButtonRadio;
    eNameA: TEdit;
    G2GraphLabel2: TG2GraphLabel;
    G2GraphLabel3: TG2GraphLabel;
    G2GraphLabel4: TG2GraphLabel;
    G2GraphPanel4: TG2GraphPanel;
    rbVariationB: TG2GraphButtonRadio;
    eNameB: TEdit;
    G2GraphLabel5: TG2GraphLabel;
    G2GraphLabel6: TG2GraphLabel;
    G2GraphLabel7: TG2GraphLabel;
    G2GraphPanel5: TG2GraphPanel;
    rbVariationC: TG2GraphButtonRadio;
    eNameC: TEdit;
    G2GraphLabel8: TG2GraphLabel;
    G2GraphLabel9: TG2GraphLabel;
    G2GraphLabel10: TG2GraphLabel;
    G2GraphPanel6: TG2GraphPanel;
    rbVariationD: TG2GraphButtonRadio;
    eNameD: TEdit;
    G2GraphLabel11: TG2GraphLabel;
    G2GraphLabel12: TG2GraphLabel;
    G2GraphLabel13: TG2GraphLabel;
    procedure UpdaterTimer(Sender: TObject);
    procedure rbPageColumnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rbPageClick(Sender: TObject);
    procedure rbVariationAClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure skPChange(Sender: TObject);
    procedure bfPChange(Sender: TObject);
  private
    FDisableControls : boolean;
    FEffect    : TVstTemplate;
    FKnobArray       : array[0..7] of TG2GraphKnob;
    FDispKnobArray   : array[0..7] of TG2GraphDisplay;
    FDispModuleArray : array[0..7] of TG2GraphDisplay;
    FButtonArray     : array[0..7] of TG2GraphButtonFlat;
    FDispButtonArray : array[0..7] of TG2GraphDisplay;
    procedure OnEditorOpen(var Msg: TMessage); message WM_EDITOROPEN;
  public
    procedure UpdateControls;
    function  GetKnobIndexOffset : integer;

    property Effect: TVstTemplate read FEffect write FEffect;
  end;

implementation
uses SysUtils, uPlugin;
{$R *.DFM}

procedure TPluginEditorWindow.Button1Click(Sender: TObject);
begin
  if (not assigned(Effect)) or (not assigned((Effect as APlugin).FG2)) then
    exit;

  (Effect as APlugin).FG2.save_log;
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
  DoubleBuffered := False;

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
end;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
begin
  if (not assigned(Effect)) or not(Effect.editorNeedsUpdate) or (not assigned((Effect as APlugin).FG2)) then
    exit;

 Updater.Enabled := False;
 try
    Effect.editorNeedsUpdate := false;
    UpdateControls;
  finally
    Updater.Enabled := True;
  end;
end;

procedure TPluginEditorWindow.UpdateControls;
var i, j : integer;
    Perf : TG2USBPerformance;
    Knob : TGlobalKnob;
    Module : TG2FileModule;
    Param, ButtonParam : TG2FileParameter;
    Status : String;
begin
  if (not assigned(Effect)) or (not assigned((Effect as APlugin).FG2)) then
    exit;

  FDisableControls := true;
  try
    try
      lbStatus.Caption := (Effect as APlugin).GetStatusText;

      eNameA.Text        := (Effect as APlugin).FG2.GetSlot(0).GetPatch.PatchName;
      rbVariationA.Value := (Effect as APlugin).FG2.GetSlot(0).GetPatch.ActiveVariation;
      eNameB.Text        := (Effect as APlugin).FG2.GetSlot(1).GetPatch.PatchName;
      rbVariationB.Value := (Effect as APlugin).FG2.GetSlot(1).GetPatch.ActiveVariation;
      eNameC.Text        := (Effect as APlugin).FG2.GetSlot(2).GetPatch.PatchName;
      rbVariationC.Value := (Effect as APlugin).FG2.GetSlot(2).GetPatch.ActiveVariation;
      eNameD.Text        := (Effect as APlugin).FG2.GetSlot(3).GetPatch.PatchName;
      rbVariationD.Value := (Effect as APlugin).FG2.GetSlot(3).GetPatch.ActiveVariation;

      Perf := (Effect as APlugin).FG2.GetPerformance;
      for i := 0 to 7 do begin
        Knob := Perf.GetGlobalKnob( GetKnobIndexOffset + i);
        if assigned(Knob) and (Knob.IsAssigned = 1) and assigned(Knob.Parameter) then begin
          Param := Knob.Parameter;
          FKnobArray[i].Value := Knob.KnobValue;
          FKnobArray[i].LowValue := Param.LowValue;
          FKnobArray[i].HighValue := Param.HighValue;
          //FDispKnobArray[i].Line[0] := Param.TextFunction(1003, 0, 2);
          //FDispKnobArray[i].Line[1] := Param.TextFunction(1003, 1, 2);
          //FDispModuleArray[i].Line[0] := Param.TextFunction(1002, 0, 1);
          FDispKnobArray[i].Line[0] := Param.ParamName;
          FDispKnobArray[i].Line[1] := IntToStr(Param.GetParameterValue);
          FDispModuleArray[i].Line[0] := Param.ModuleName;
          FDispButtonArray[i].Line[0] := '';
          FButtonArray[i].ButtonText.Clear;
          if assigned(Param.ButtonParam) then begin
            ButtonParam := Param.ButtonParam;
            for j := 0 to ButtonParam.ButtonTextCount - 1 do
              FButtonArray[i].ButtonText.Add( ButtonParam.ButtonText[j]);
            FButtonArray[i].Value := ButtonParam.GetParameterValue;
            FButtonArray[i].LowValue := ButtonParam.LowValue;
            FButtonArray[i].HighValue := ButtonParam.HighValue;
            //FDispButtonArray[i].Line[0] := ButtonParam.TextFunction(1003, 0, 1);
            FDispButtonArray[i].Line[0] := ButtonParam.ParamName;
          end else begin
            FButtonArray[i].ButtonText.Clear;
            FButtonArray[i].Value := 0;
            FButtonArray[i].LowValue := 0;
            FButtonArray[i].HighValue := 0;
            FDispButtonArray[i].Line[0] := '';
            FButtonArray[i].Invalidate;
          end;
        end else begin
          FKnobArray[i].Value := 0;
          FKnobArray[i].LowValue := 0;
          FKnobArray[i].HighValue := 127;
          FDispKnobArray[i].Line[0] := '';
          FDispKnobArray[i].Line[1] := '';
          FDispModuleArray[i].Line[0] := '';
          FDispButtonArray[i].Line[0] := '';
          FButtonArray[i].ButtonText.Clear;
          FButtonArray[i].Value := 0;
          FButtonArray[i].LowValue := 0;
          FButtonArray[i].HighValue := 0;
        end;
      end;
    except on E:Exception do begin
       (Effect as APlugin).FG2.add_log_line('UpdateControls : ' + E.Message, LOGCMD_NUL);
       (Effect as APlugin).FG2.save_log;
      end;
    end;
  finally
    FDisableControls := false;
  end;
end;

function TPluginEditorWindow.GetKnobIndexOffset: integer;
begin
  Result := rbPageColumn.Value * 8 + rbPage.Value * 8 * 3;
end;

procedure TPluginEditorWindow.rbPageClick(Sender: TObject);
begin
  Effect.editorNeedsUpdate := True;
end;

procedure TPluginEditorWindow.rbPageColumnClick(Sender: TObject);
begin
  Effect.editorNeedsUpdate := True;
end;

procedure TPluginEditorWindow.rbVariationAClick(Sender: TObject);
begin
  if FDisableControls then
    exit;

  if Sender is TG2GraphButtonRadio then begin
    with Sender as TG2GraphButtonRadio do
      case Tag of
      0 : begin
           (Effect as APlugin).setParameterAutomated( 240, rbVariationA.Value / 7);
          end;
      1 : begin
           (Effect as APlugin).setParameterAutomated( 240 + 1, rbVariationB.Value / 7);
          end;
      2 : begin
           (Effect as APlugin).setParameterAutomated( 240 + 2, rbVariationC.Value / 7);
          end;
      3 : begin
           (Effect as APlugin).setParameterAutomated( 240 + 3, rbVariationD.Value / 7);
          end;
      end;
  end;
end;

procedure TPluginEditorWindow.bfPChange(Sender: TObject);
var KnobIndex : integer;
    Knob : TGlobalKnob;
    G2 : TG2USB;
    ButtonParam : TG2FileParameter;
    ButtonFloatValue : single;
begin
  if FDisableControls then
    exit;

  G2 := (Effect as APlugin).FG2;

  if Sender is TG2GraphButtonFlat then
    with (Sender) as TG2GraphButtonFlat do begin

      KnobIndex := GetKnobIndexOffset + tag;
      Knob := G2.GetPerformance.GetGlobalKnob( KnobIndex);
      if (not assigned(Knob)) or (Knob.IsAssigned = 0) or (not assigned(Knob.Parameter)) then
        exit;

      ButtonParam := Knob.Parameter.ButtonParam;
      if not assigned(ButtonParam) then
        exit;

      if ButtonParam.HighValue - ButtonParam.LowValue <> 0 then
        ButtonFloatValue := Value / (ButtonParam.HighValue - ButtonParam.LowValue)
      else
        ButtonFloatValue := 0;

      (Effect as APlugin).setParameterAutomated( KnobIndex + 120, ButtonFloatValue);
    end;
end;

procedure TPluginEditorWindow.skPChange(Sender: TObject);
var KnobIndex : integer;
    Knob : TGlobalKnob;
    KnobFloatValue : single;
    G2 : TG2USB;
begin
  if FDisableControls then
    exit;

  G2 := (Effect as APlugin).FG2;

  if Sender is TG2GraphKnob then
    with (Sender) as TG2GraphKnob do begin

      KnobIndex := GetKnobIndexOffset + tag;
      Knob := G2.GetPerformance.GetGlobalKnob( KnobIndex);
      if (not assigned(Knob)) or (Knob.IsAssigned = 0) or (not assigned(Knob.Parameter)) then
        exit;

      if Knob.KnobHighValue - Knob.KnobLowValue <> 0 then
        KnobFloatValue := Value / (Knob.KnobHighValue - Knob.KnobLowValue)
      else
        KnobFloatValue := 0;

      (Effect as APlugin).setParameterAutomated( KnobIndex, KnobFloatValue);

    end;
end;

procedure TPluginEditorWindow.OnEditorOpen(var Msg: TMessage);
begin
  Effect := TVstTemplate(Msg.WParam);
  Effect.editorNeedsUpdate := True;
  Updater.Enabled := True;
end;

end.

