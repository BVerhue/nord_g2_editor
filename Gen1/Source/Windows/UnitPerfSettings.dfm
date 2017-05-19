object frmPerfSettings: TfrmPerfSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Performance Settings'
  ClientHeight = 218
  ClientWidth = 660
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
  object GroupBox1: TGroupBox
    Left = 176
    Top = 8
    Width = 257
    Height = 49
    Caption = 'Master Clock'
    TabOrder = 2
    TabStop = True
    object rbStop: TRadioButton
      Left = 144
      Top = 18
      Width = 41
      Height = 17
      Caption = 'Stop'
      TabOrder = 3
      TabStop = True
      OnClick = PerfChange
    end
    object rbRun: TRadioButton
      Left = 200
      Top = 18
      Width = 49
      Height = 17
      Caption = 'Run'
      TabOrder = 4
      TabStop = True
      OnClick = PerfChange
    end
    object udRate: DUpDown
      Left = 112
      Top = 14
      Width = 17
      Height = 25
      Min = 30
      Max = 240
      Position = 30
      TabOrder = 2
      TabStop = True
      OnChangingEx = udRateChangingEx
      OnClick = udRateClick
    end
    object eRate: DEdit
      Left = 67
      Top = 16
      Width = 42
      Height = 21
      TabOrder = 1
      OnExit = PerfChange
    end
    object StaticText2: TStaticText
      Left = 8
      Top = 20
      Width = 58
      Height = 17
      Caption = 'Rate (BPM)'
      TabOrder = 0
      TabStop = True
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 56
    Width = 644
    Height = 149
    Caption = 'Slots'
    TabOrder = 3
    TabStop = True
    object cbEnableA: TCheckBox
      Left = 16
      Top = 39
      Width = 82
      Height = 17
      Caption = 'Enable slot A'
      TabOrder = 0
      OnClick = PerfChange
    end
    object cbEnableB: TCheckBox
      Left = 16
      Top = 66
      Width = 82
      Height = 17
      Caption = 'Enable slot B'
      TabOrder = 1
      OnClick = PerfChange
    end
    object cbEnableC: TCheckBox
      Left = 16
      Top = 93
      Width = 82
      Height = 17
      Caption = 'Enable slot C'
      TabOrder = 2
      OnClick = PerfChange
    end
    object cbEnableD: TCheckBox
      Left = 16
      Top = 120
      Width = 82
      Height = 17
      Caption = 'Enable slot D'
      TabOrder = 3
      OnClick = PerfChange
    end
    object cbKeyboardA: TCheckBox
      Left = 120
      Top = 39
      Width = 105
      Height = 17
      Caption = 'Keyboard slot A'
      TabOrder = 4
      OnClick = PerfChange
    end
    object cbKeyboardB: TCheckBox
      Left = 120
      Top = 66
      Width = 105
      Height = 17
      Caption = 'Keyboard slot B'
      TabOrder = 5
      OnClick = PerfChange
    end
    object cbKeyboardC: TCheckBox
      Left = 120
      Top = 93
      Width = 105
      Height = 17
      Caption = 'Keyboard slot C'
      TabOrder = 6
      OnClick = PerfChange
    end
    object cbKeyboardD: TCheckBox
      Left = 120
      Top = 120
      Width = 105
      Height = 17
      Caption = 'Keyboard slot D'
      TabOrder = 7
      OnClick = PerfChange
    end
    object cbHoldA: TCheckBox
      Left = 244
      Top = 39
      Width = 70
      Height = 17
      Caption = 'Hold slot A'
      TabOrder = 8
      OnClick = PerfChange
    end
    object cbHoldB: TCheckBox
      Left = 244
      Top = 66
      Width = 70
      Height = 17
      Caption = 'Hold slot B'
      TabOrder = 9
      OnClick = PerfChange
    end
    object cbHoldC: TCheckBox
      Left = 244
      Top = 93
      Width = 70
      Height = 17
      Caption = 'Hold slot C'
      TabOrder = 10
      OnClick = PerfChange
    end
    object cbHoldD: TCheckBox
      Left = 244
      Top = 120
      Width = 70
      Height = 17
      Caption = 'Hold slot D'
      TabOrder = 11
      OnClick = PerfChange
    end
    object cbKeyboardRange: TCheckBox
      Left = 338
      Top = 14
      Width = 97
      Height = 17
      Caption = 'Keyboard range'
      TabOrder = 12
      OnClick = PerfChange
    end
    object StaticText3: TStaticText
      Left = 338
      Top = 41
      Width = 83
      Height = 17
      Caption = 'Lower key slot A'
      TabOrder = 13
      TabStop = True
    end
    object StaticText4: TStaticText
      Left = 489
      Top = 41
      Width = 83
      Height = 17
      Caption = 'Upper key slot A'
      TabOrder = 15
      TabStop = True
    end
    object eLowerA: DEdit
      Left = 427
      Top = 37
      Width = 38
      Height = 21
      TabOrder = 14
      Text = 'eLowerA'
      OnExit = PerfChange
    end
    object eLowerB: DEdit
      Left = 427
      Top = 64
      Width = 38
      Height = 21
      TabOrder = 18
      Text = 'eLowerB'
      OnExit = PerfChange
    end
    object eLowerC: DEdit
      Left = 427
      Top = 91
      Width = 38
      Height = 21
      TabOrder = 22
      Text = 'eLowerC'
      OnExit = PerfChange
    end
    object eLowerD: DEdit
      Left = 427
      Top = 118
      Width = 38
      Height = 21
      TabOrder = 26
      Text = 'eLowerD'
      OnExit = PerfChange
    end
    object eUpperA: DEdit
      Left = 578
      Top = 37
      Width = 38
      Height = 21
      TabOrder = 16
      Text = 'eUpperA'
      OnExit = PerfChange
    end
    object eUpperB: DEdit
      Left = 578
      Top = 64
      Width = 38
      Height = 21
      TabOrder = 20
      Text = 'eUpperB'
      OnExit = PerfChange
    end
    object eUpperC: DEdit
      Left = 578
      Top = 91
      Width = 38
      Height = 21
      TabOrder = 24
      Text = 'eUpperC'
      OnExit = PerfChange
    end
    object eUpperD: DEdit
      Left = 578
      Top = 118
      Width = 38
      Height = 21
      TabOrder = 28
      Text = 'eUpperD'
      OnExit = PerfChange
    end
    object StaticText5: TStaticText
      Left = 338
      Top = 68
      Width = 82
      Height = 17
      Caption = 'Lower key slot B'
      TabOrder = 17
      TabStop = True
    end
    object StaticText6: TStaticText
      Left = 338
      Top = 95
      Width = 83
      Height = 17
      Caption = 'Lower key slot C'
      TabOrder = 21
      TabStop = True
    end
    object StaticText7: TStaticText
      Left = 338
      Top = 122
      Width = 83
      Height = 17
      Caption = 'Lower key slot D'
      TabOrder = 25
      TabStop = True
    end
    object StaticText8: TStaticText
      Left = 489
      Top = 68
      Width = 82
      Height = 17
      Caption = 'Upper key slot B'
      TabOrder = 19
      TabStop = True
    end
    object StaticText9: TStaticText
      Left = 489
      Top = 95
      Width = 83
      Height = 17
      Caption = 'Upper key slot C'
      TabOrder = 23
      TabStop = True
    end
    object StaticText10: TStaticText
      Left = 488
      Top = 122
      Width = 83
      Height = 17
      Caption = 'Upper key slot D'
      TabOrder = 27
      TabStop = True
    end
  end
  object StaticText1: TStaticText
    Left = 16
    Top = 8
    Width = 31
    Height = 17
    Caption = 'Name'
    TabOrder = 0
    TabStop = True
  end
  object ePerfName: DEdit
    Left = 16
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
end
