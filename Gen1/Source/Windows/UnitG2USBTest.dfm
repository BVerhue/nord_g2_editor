object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'G2  USB Connection Test'
  ClientHeight = 408
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 381
    Height = 367
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Test USB Connection:'
      ''
      '- Close G2 editor(s)'
      '- Switch off G2'
      '- Switch on G2'
      '- Wait 5 secs.'
      '- Press button "Connect G2"'
      ''
      ''
      ''
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 381
    Height = 41
    Align = alTop
    TabOrder = 1
    object Button1: TButton
      Left = 16
      Top = 10
      Width = 89
      Height = 25
      Caption = 'Connect G2'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 128
      Top = 10
      Width = 89
      Height = 25
      Caption = 'Save log'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 232
    Top = 16
  end
end
