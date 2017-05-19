object frmModuleDef: TfrmModuleDef
  Left = 0
  Top = 0
  Caption = 'Module definition'
  ClientHeight = 348
  ClientWidth = 661
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
    Left = 137
    Top = 0
    Height = 348
    ExplicitLeft = 616
    ExplicitTop = 200
    ExplicitHeight = 100
  end
  object lbModules: TListBox
    Left = 0
    Top = 0
    Width = 137
    Height = 348
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
    OnClick = lbModulesClick
  end
  object Panel3: TPanel
    Left = 140
    Top = 0
    Width = 521
    Height = 348
    Align = alClient
    Caption = 'Panel3'
    TabOrder = 1
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 519
      Height = 41
      Align = alTop
      TabOrder = 0
      object Label1: TLabel
        Left = 23
        Top = 14
        Width = 34
        Height = 13
        Caption = 'Module'
      end
      object eModuleName: TEdit
        Left = 77
        Top = 11
        Width = 156
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
      end
    end
    object sgParams: TStringGrid
      Left = 1
      Top = 42
      Width = 519
      Height = 272
      Align = alClient
      DefaultRowHeight = 16
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
      TabOrder = 1
    end
    object Panel2: TPanel
      Left = 1
      Top = 314
      Width = 519
      Height = 33
      Align = alBottom
      TabOrder = 2
      DesignSize = (
        519
        33)
      object Button1: TButton
        Left = 428
        Top = 4
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Update'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        Left = 272
        Top = 4
        Width = 129
        Height = 25
        Action = aExtractModuleInfo
        TabOrder = 1
      end
    end
  end
  object Button3: TButton
    Left = 302
    Top = 318
    Width = 88
    Height = 25
    Caption = 'Create SVG'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 164
    Top = 318
    Width = 111
    Height = 25
    Caption = 'Create modules file'
    TabOrder = 3
    OnClick = Button4Click
  end
  object ActionManager1: TActionManager
    Left = 448
    Top = 16
    StyleName = 'XP Style'
    object aExtractModuleInfo: TAction
      Caption = 'Extract module info'
      OnExecute = aExtractModuleInfoExecute
    end
  end
  object G2_module_def: TG2
    ClientType = ctEditor
    LogLevel = 0
    ErrorMessage = False
    ErrorMessageNo = 0
    IsServer = False
    Port = 2501
    Host = '127.0.0.1'
    USBActive = False
    ProcessLedData = False
    TimerBroadcastLedMessages = 0
    SelectedSlotIndex = 0
    Left = 40
    Top = 128
  end
end
