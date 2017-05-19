object frmPatchBrowserModuleFilter: TfrmPatchBrowserModuleFilter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Select modules'
  ClientHeight = 392
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object cblModules: TCheckListBox
    Left = 0
    Top = 0
    Width = 236
    Height = 351
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 351
    Width = 236
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object bOk: TButton
      Left = 152
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
    end
    object bCancel: TButton
      Left = 64
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
