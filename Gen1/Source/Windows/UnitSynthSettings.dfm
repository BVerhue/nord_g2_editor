object frmSynthSettings: TfrmSynthSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Synth Settings'
  ClientHeight = 271
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 27
    Height = 13
    Caption = 'Name'
  end
  object lblControlPedalGain: TLabel
    Left = 379
    Top = 170
    Width = 122
    Height = 13
    Caption = 'Control pedal gain: x1.00'
  end
  object eSynthName: TEdit
    Left = 61
    Top = 13
    Width = 121
    Height = 21
    TabOrder = 0
    OnExit = SynthChange
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 40
    Width = 166
    Height = 153
    Caption = 'MIDI Channel'
    TabOrder = 1
    object Label2: TLabel
      Left = 10
      Top = 32
      Width = 28
      Height = 13
      Caption = 'Slot A'
    end
    object Label3: TLabel
      Left = 10
      Top = 58
      Width = 27
      Height = 13
      Caption = 'Slot B'
    end
    object Label4: TLabel
      Left = 10
      Top = 86
      Width = 28
      Height = 13
      Caption = 'Slot C'
    end
    object Label5: TLabel
      Left = 10
      Top = 114
      Width = 28
      Height = 13
      Caption = 'Slot D'
    end
    object udMidiChannelA: TUpDown
      Left = 87
      Top = 26
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 0
      OnChangingEx = udMidiChannelAChangingEx
    end
    object eMidiChannelA: TEdit
      Left = 53
      Top = 29
      Width = 33
      Height = 21
      TabOrder = 1
      OnExit = SynthChange
    end
    object cbMidiActiveA: TCheckBox
      Left = 111
      Top = 31
      Width = 53
      Height = 17
      Caption = 'Active'
      TabOrder = 2
      OnClick = cbMidiActiveAClick
    end
    object cbMidiActiveB: TCheckBox
      Left = 111
      Top = 57
      Width = 51
      Height = 17
      Caption = 'Active'
      TabOrder = 3
      OnClick = cbMidiActiveBClick
    end
    object cbMidiActiveC: TCheckBox
      Left = 111
      Top = 85
      Width = 51
      Height = 17
      Caption = 'Active'
      TabOrder = 4
      OnClick = cbMidiActiveCClick
    end
    object cbMidiActiveD: TCheckBox
      Left = 111
      Top = 113
      Width = 50
      Height = 17
      Caption = 'Active'
      TabOrder = 5
      OnClick = cbMidiActiveDClick
    end
    object eMidiChannelB: TEdit
      Left = 53
      Top = 55
      Width = 33
      Height = 21
      TabOrder = 6
      OnExit = SynthChange
    end
    object eMidiChannelC: TEdit
      Left = 53
      Top = 83
      Width = 33
      Height = 21
      TabOrder = 7
      OnExit = SynthChange
    end
    object eMidiChannelD: TEdit
      Left = 53
      Top = 111
      Width = 33
      Height = 21
      TabOrder = 8
      OnExit = SynthChange
    end
    object udMidiChannelB: TUpDown
      Left = 87
      Top = 54
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 9
      OnChangingEx = udMidiChannelBChangingEx
    end
    object udMidiChannelC: TUpDown
      Left = 87
      Top = 82
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 10
      OnChangingEx = udMidiChannelCChangingEx
    end
    object udMidiChannelD: TUpDown
      Left = 87
      Top = 111
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 11
      OnChangingEx = udMidiChannelDChangingEx
    end
  end
  object GroupBox2: TGroupBox
    Left = 16
    Top = 199
    Width = 112
    Height = 66
    Caption = 'File'
    TabOrder = 2
    object lbFileFreeMem: TLabel
      Left = 10
      Top = 16
      Width = 86
      Height = 13
      Caption = 'Free mem : 100%'
    end
    object cbMemoryProtect: TCheckBox
      Left = 10
      Top = 35
      Width = 97
      Height = 17
      Caption = 'Memory Protect'
      TabOrder = 0
      OnClick = SynthChange
    end
  end
  object GroupBox4: TGroupBox
    Left = 188
    Top = 8
    Width = 185
    Height = 185
    Caption = 'MIDI Setings'
    TabOrder = 3
    object Label6: TLabel
      Left = 16
      Top = 24
      Width = 77
      Height = 13
      Caption = 'Gloabel Channel'
    end
    object Label7: TLabel
      Left = 16
      Top = 52
      Width = 43
      Height = 13
      Caption = 'Sysex ID'
    end
    object Label8: TLabel
      Left = 16
      Top = 118
      Width = 52
      Height = 13
      Caption = 'Controllers'
    end
    object Label9: TLabel
      Left = 99
      Top = 118
      Width = 80
      Height = 13
      Caption = 'Program Change'
    end
    object eGlobalChannel: TEdit
      Left = 99
      Top = 21
      Width = 33
      Height = 21
      TabOrder = 0
      OnExit = SynthChange
    end
    object eSysexID: TEdit
      Left = 99
      Top = 49
      Width = 33
      Height = 21
      TabOrder = 1
      OnExit = SynthChange
    end
    object udGlobalChannel: TUpDown
      Left = 134
      Top = 19
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 2
      OnChangingEx = udGlobalChannelChangingEx
    end
    object udSysexID: TUpDown
      Left = 134
      Top = 47
      Width = 17
      Height = 25
      Max = 16
      TabOrder = 3
      OnChangingEx = udSysexIDChangingEx
    end
    object cbSendClock: TCheckBox
      Left = 16
      Top = 73
      Width = 97
      Height = 17
      Caption = 'Send Clock'
      TabOrder = 4
      OnClick = SynthChange
    end
    object cbIgnoreExternalClock: TCheckBox
      Left = 16
      Top = 95
      Width = 129
      Height = 17
      Caption = 'Ignore External Clock'
      TabOrder = 5
      OnClick = SynthChange
    end
    object cbControllersReceive: TCheckBox
      Left = 16
      Top = 135
      Width = 66
      Height = 17
      Caption = 'Receive'
      TabOrder = 6
      OnClick = SynthChange
    end
    object cbControllersSend: TCheckBox
      Left = 16
      Top = 155
      Width = 66
      Height = 17
      Caption = 'Send'
      TabOrder = 7
      OnClick = SynthChange
    end
    object cbProgramChangeSend: TCheckBox
      Left = 99
      Top = 155
      Width = 57
      Height = 17
      Caption = 'Send'
      TabOrder = 8
      OnClick = SynthChange
    end
    object cbProgramChangeReceive: TCheckBox
      Left = 99
      Top = 135
      Width = 65
      Height = 17
      Caption = 'Receive'
      TabOrder = 9
      OnClick = SynthChange
    end
  end
  object GroupBox5: TGroupBox
    Left = 379
    Top = 8
    Width = 150
    Height = 152
    Caption = 'Tune'
    TabOrder = 4
    object lbMasterTune: TLabel
      Left = 16
      Top = 24
      Width = 115
      Height = 13
      Caption = 'Master Tune   440.00Hz'
    end
    object Label10: TLabel
      Left = 26
      Top = 50
      Width = 22
      Height = 13
      Caption = 'Semi'
    end
    object Label11: TLabel
      Left = 26
      Top = 80
      Width = 23
      Height = 13
      Caption = 'Cent'
    end
    object eTuneSemi: TEdit
      Left = 64
      Top = 47
      Width = 49
      Height = 21
      TabOrder = 0
      OnExit = SynthChange
    end
    object eTuneCent: TEdit
      Left = 64
      Top = 77
      Width = 49
      Height = 21
      TabOrder = 1
      OnExit = SynthChange
    end
    object udTuneSemi: TUpDown
      Left = 116
      Top = 44
      Width = 17
      Height = 25
      Min = -6
      Max = 6
      TabOrder = 2
      OnChangingEx = udTuneSemiChangingEx
    end
    object udTuneCent: TUpDown
      Left = 116
      Top = 75
      Width = 17
      Height = 25
      Min = -100
      TabOrder = 3
      OnChangingEx = udTuneCentChangingEx
    end
    object cbGlobaleOctaveShift: TCheckBox
      Left = 16
      Top = 106
      Width = 131
      Height = 17
      Caption = 'Global Octave Shift'
      TabOrder = 4
      OnClick = SynthChange
    end
    object rbOctShift1: TRadioButton
      Left = 16
      Top = 128
      Width = 17
      Height = 17
      TabOrder = 5
      OnClick = SynthChange
    end
    object rbOctShift2: TRadioButton
      Left = 34
      Top = 128
      Width = 17
      Height = 17
      TabOrder = 6
      OnClick = SynthChange
    end
    object rbOctShift3: TRadioButton
      Left = 52
      Top = 128
      Width = 17
      Height = 17
      TabOrder = 7
      OnClick = SynthChange
    end
    object rbOctShift4: TRadioButton
      Left = 69
      Top = 128
      Width = 17
      Height = 17
      TabOrder = 8
      OnClick = SynthChange
    end
    object rbOctShift5: TRadioButton
      Left = 86
      Top = 128
      Width = 17
      Height = 17
      TabOrder = 9
      OnClick = SynthChange
    end
  end
  object rgPedalPolarity: TRadioGroup
    Left = 134
    Top = 199
    Width = 107
    Height = 66
    Caption = 'Pedal polarity'
    Items.Strings = (
      'Open'
      'Closed')
    TabOrder = 5
    OnClick = SynthChange
  end
  object udControlPedalGain: TUpDown
    Left = 515
    Top = 164
    Width = 17
    Height = 25
    Max = 32
    TabOrder = 6
    OnChangingEx = udControlPedalGainChangingEx
  end
  object cbLocalOn: TCheckBox
    Left = 379
    Top = 191
    Width = 97
    Height = 17
    Caption = 'Local On'
    TabOrder = 7
    OnClick = SynthChange
  end
end
