object frmG2Server: TfrmG2Server
  Left = 0
  Top = 0
  Caption = 'G2 Server'
  ClientHeight = 410
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 309
    Width = 457
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitLeft = 13
    ExplicitTop = 260
  end
  object Splitter2: TSplitter
    Left = 0
    Top = 173
    Width = 457
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 137
    ExplicitWidth = 43
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 457
    Height = 33
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 347
      Top = 10
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 230
      Top = 10
      Width = 112
      Height = 13
      Caption = 'Send message queue : '
    end
    object cbUSBActive: TCheckBox
      Left = 13
      Top = 8
      Width = 97
      Height = 17
      Caption = 'USB Active'
      TabOrder = 0
    end
    object cbLog: TCheckBox
      Left = 104
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Log messages'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 369
    Width = 457
    Height = 41
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      457
      41)
    object Button1: TButton
      Left = 362
      Top = 6
      Width = 75
      Height = 25
      Action = aQuit
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 33
    Width = 457
    Height = 140
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo1')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object ListBox1: TListBox
    Left = 0
    Top = 312
    Width = 457
    Height = 57
    Align = alBottom
    ItemHeight = 13
    TabOrder = 3
  end
  object Memo2: TMemo
    Left = 0
    Top = 176
    Width = 457
    Height = 133
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Memo2')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object G2: TG2
    ClientType = ctEditor
    LogLevel = 0
    IsServer = True
    Port = 2501
    Host = '127.0.0.1'
    USBActive = False
    ProcessLedData = True
    OnUSBActiveChange = G2USBActiveChange
    OnDeleteClient = G2DeleteClient
    SelectedSlotIndex = 0
    Left = 24
    Top = 88
  end
  object ActionManager1: TActionManager
    Left = 56
    Top = 88
    StyleName = 'XP Style'
    object aQuit: TAction
      Caption = 'Quit'
      OnExecute = aQuitExecute
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 88
    Top = 88
  end
  object Timer2: TTimer
    Interval = 250
    Left = 120
    Top = 88
  end
end
