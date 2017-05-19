object frmPatchSettings: TfrmPatchSettings
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Patch settings'
  ClientHeight = 161
  ClientWidth = 602
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object G2GraphPanel1: TG2GraphPanel
    Left = 0
    Top = 0
    Width = 602
    Height = 161
    Align = alClient
    BevelOuter = bvNone
    Caption = 'G2GraphPanel1'
    TabOrder = 0
    object G2GraphLabel1: TG2GraphLabel
      Left = 14
      Top = 17
      Width = 58
      Height = 17
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Sustain Pedal'
    end
    object G2GraphLabel2: TG2GraphLabel
      Left = 14
      Top = 65
      Width = 55
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Octave Shift'
    end
    object G2GraphLabel3: TG2GraphLabel
      Left = 152
      Top = 17
      Width = 48
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Arpeggiator'
    end
    object G2GraphLabel4: TG2GraphLabel
      Left = 401
      Top = 17
      Width = 22
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Glide'
    end
    object G2GraphLabel5: TG2GraphLabel
      Left = 294
      Top = 17
      Width = 58
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Vibrato Depth'
    end
    object G2GraphLabel6: TG2GraphLabel
      Left = 502
      Top = 16
      Width = 22
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Bend'
    end
    object kBendRange: TG2GraphKnob
      Left = 502
      Top = 56
      Width = 92
      Height = 15
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clWhite
      Value = 0
      LowValue = 0
      HighValue = 127
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      KnobType = ktSlider
      Orientation = otHorizontal
      HightlightColor = clAqua
    end
    object kGlideSpeed: TG2GraphKnob
      Left = 401
      Top = 33
      Width = 91
      Height = 15
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clWhite
      Value = 0
      LowValue = 0
      HighValue = 127
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      KnobType = ktSlider
      Orientation = otHorizontal
      HightlightColor = clAqua
    end
    object kVibratoDepth: TG2GraphKnob
      Left = 294
      Top = 33
      Width = 91
      Height = 15
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clWhite
      Value = 0
      LowValue = 0
      HighValue = 127
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      KnobType = ktSlider
      Orientation = otHorizontal
      HightlightColor = clAqua
    end
    object kVibratoRate: TG2GraphKnob
      Left = 294
      Top = 133
      Width = 91
      Height = 15
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clWhite
      Value = 0
      LowValue = 0
      HighValue = 127
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      KnobType = ktSlider
      Orientation = otHorizontal
      HightlightColor = clAqua
    end
    object obArpeggiatorOnOff: TG2GraphButtonRadio
      Left = 152
      Top = 33
      Width = 91
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 1
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'off'
        'on')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 2
    end
    object obArpOctaves: TG2GraphButtonRadio
      Left = 152
      Top = 99
      Width = 121
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 3
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        '1'
        '2'
        '3'
        '4')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 4
    end
    object obArpSpeed: TG2GraphButtonRadio
      Left = 152
      Top = 55
      Width = 121
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 3
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        '1/8'
        '1/8T'
        '1/16'
        '1/16T')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 4
    end
    object obBendOnOff: TG2GraphButtonRadio
      Left = 502
      Top = 33
      Width = 91
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 1
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'off'
        'on')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 2
    end
    object obGlideType: TG2GraphButtonRadio
      Left = 401
      Top = 56
      Width = 91
      Height = 53
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 2
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'Auto'
        'Normal'
        'Off')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otVertical
      ButtonCount = 3
    end
    object obOctaveShift: TG2GraphButtonRadio
      Left = 14
      Top = 81
      Width = 121
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 4
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        '-2'
        '-1'
        '0'
        '1'
        '2')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 5
    end
    object obSustainPedalOnOff: TG2GraphButtonRadio
      Left = 14
      Top = 34
      Width = 91
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 1
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'off'
        'on')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 2
    end
    object obVibratoMod: TG2GraphButtonRadio
      Left = 294
      Top = 56
      Width = 91
      Height = 53
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 2
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'Wheel'
        'AfTouch'
        'Off')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otVertical
      ButtonCount = 3
    end
    object obArpDir: TG2GraphButtonRadio
      Left = 152
      Top = 77
      Width = 121
      Height = 16
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = clActiveBorder
      Value = 0
      LowValue = 0
      HighValue = 3
      OnMouseUp = PatchCtrlMouseUp
      ParentColor = False
      ParentFont = False
      ButtonText.Strings = (
        'Up'
        'Dn'
        'UpDn'
        'Rnd')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 4
    end
    object G2GraphLabel7: TG2GraphLabel
      Left = 294
      Top = 117
      Width = 49
      Height = 10
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Value = 0
      LowValue = 0
      HighValue = 0
      ParentFont = False
      Caption = 'Vibrato rate'
    end
  end
end
