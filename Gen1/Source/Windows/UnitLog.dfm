object frmLog: TfrmLog
  Left = 0
  Top = 0
  Caption = 'Log'
  ClientHeight = 502
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 35
    Width = 722
    Height = 407
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      ''
      'G2 usb demo'
      ''
      
        'This software is intended for educational/research purposes, use' +
        ' at your own risk!'
      ''
      'This demo uses the open source libusb-win32 usb driver.'
      ''
      'Download libusb-win32 snapshot from'
      'http://sourceforge.net/projects/libusb-win32/files/'
      'Make a system restore point'
      'Install as a filter driver on the existing clavia usb driver'
      
        'Do NOT install as device driver (because it then permanently rep' +
        'laces the clavia driver!)'
      ''
      
        'After you installed the filter driver, test if the clavia softwa' +
        're still works. If not, do a system restore.'
      ''
      
        'The demo can not run alongside the clavia software, before you r' +
        'un the one you have to stop the other.')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 442
    Width = 722
    Height = 60
    Align = alBottom
    TabOrder = 1
    DesignSize = (
      722
      60)
    object eCommand: TEdit
      Left = 16
      Top = 15
      Width = 587
      Height = 21
      Hint = 'Here you can type a message (hex seperated with space)'
      Anchors = [akLeft, akTop, akRight]
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object bSendMsg: TButton
      Left = 620
      Top = 13
      Width = 75
      Height = 25
      Hint = 
        'With this you can send a typed messages to the G2 (hex seperated' +
        ' with a space)'
      Anchors = [akTop, akRight]
      Caption = 'Send Msg'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = bSendMsgClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 722
    Height = 35
    Align = alTop
    TabOrder = 2
    object bRefresh: TButton
      Left = 95
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = bRefreshClick
    end
    object bClear: TButton
      Left = 176
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Clear'
      TabOrder = 1
      OnClick = bClearClick
    end
    object Button1: TButton
      Left = 14
      Top = 4
      Width = 75
      Height = 25
      Action = frmG2Main.aSaveLogFile
      TabOrder = 2
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 48
  end
end
