object frmTestModule: TfrmTestModule
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Test module'
  ClientHeight = 439
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 353
    Height = 73
    Caption = 
      'This module  is not published in the original Clavia software, p' +
      'robably because it never left the experimental stage. The module' +
      ' definition was however found and you can try out here if the mo' +
      'dule still exists in your current G2 firmware.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 95
    Width = 353
    Height = 32
    Caption = 
      'To try it out, you have to guess the Module type ID, enter it in' +
      ' the editbox and press Ok.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 144
    Width = 353
    Height = 49
    Caption = 
      'WARNING: entering the wrong number will probably result in a cor' +
      'rupted patch an you may have to reboot your G2, so use at your o' +
      'wn risk.'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 16
    Top = 264
    Width = 353
    Height = 57
    Caption = 
      'For those who want to experiment further: the editor uses the mo' +
      'dule definition in the file ModuleDef.xml to construct the modul' +
      'e insert message, you can edit the module definition manually in' +
      ' the xml file if you think it might be wrong. But remember to ma' +
      'ke a backup of the xml file.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
    WordWrap = True
  end
  object Label5: TLabel
    Left = 40
    Top = 222
    Width = 186
    Height = 16
    Caption = 'Enter a module type ID (1..219):'
  end
  object Label6: TLabel
    Left = 16
    Top = 327
    Width = 340
    Height = 39
    Caption = 
      'Some free ID'#39's : 2, (6), 10, 11, 14, 39, 65, 70, 77, 80, (95), 9' +
      '9, 107, 110, 111, 122, 133, (135), (136), (137), (138), 151, (15' +
      '3), 155, 168, 191, 207, 209, 210, 211..219'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsItalic]
    ParentFont = False
    WordWrap = True
  end
  object eModuleTypeID: TEdit
    Left = 240
    Top = 219
    Width = 81
    Height = 24
    TabOrder = 0
  end
  object bOk: TButton
    Left = 294
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 193
    Top = 400
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
