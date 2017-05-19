object frmPatchBrowser: TfrmPatchBrowser
  Left = 0
  Top = 0
  Caption = 'Patch browser'
  ClientHeight = 400
  ClientWidth = 530
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
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tcSource: TTabControl
    Left = 0
    Top = 0
    Width = 530
    Height = 400
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Perf. (disk)'
      'Patch (disk)'
      'Perf.'
      'Patch')
    TabIndex = 0
    OnChange = tcSourceChange
    object lvInternal: DListView
      Left = 16
      Top = 296
      Width = 495
      Height = 89
      Columns = <
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Category'
          Width = 120
        end
        item
          Caption = 'Slot'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = lvInternalColumnClick
      OnCompare = lvInternalCompare
      OnDblClick = aRestoreExecute
      OnKeyDown = lvInternalKeyDown
    end
    object lvExternalPatch: DListView
      Left = 16
      Top = 184
      Width = 495
      Height = 89
      Columns = <
        item
          Caption = 'Patch file'
          Width = 150
        end
        item
          Caption = 'Date'
          Width = 80
        end
        item
          Caption = 'Path'
          Width = 250
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 2
      ViewStyle = vsReport
      OnColumnClick = lvExternalPatchColumnClick
      OnCompare = lvExternalCompare
      OnDblClick = aLoadPatchExecute
      OnKeyDown = lvExternalPatchKeyDown
      OnMouseDown = lvExternalPatchMouseDown
    end
    object lvExternalPerf: DListView
      Left = 16
      Top = 80
      Width = 495
      Height = 89
      Columns = <
        item
          Caption = 'Perf file'
          Width = 150
        end
        item
          Caption = 'Date'
          Width = 80
        end
        item
          Caption = 'Path'
          Width = 250
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 3
      ViewStyle = vsReport
      OnColumnClick = lvExternalPerfColumnClick
      OnCompare = lvExternalCompare
      OnDblClick = aLoadPerfExecute
      OnKeyDown = lvExternalPerfKeyDown
    end
    object Panel1: TPanel
      Left = 4
      Top = 24
      Width = 522
      Height = 41
      Align = alTop
      TabOrder = 0
      object cbParsePatches: TCheckBox
        Left = 143
        Top = 12
        Width = 89
        Height = 17
        Caption = 'Parse patches'
        TabOrder = 2
        OnClick = cbParsePatchesClick
      end
      object cbFilterModules: TCheckBox
        Left = 255
        Top = 12
        Width = 87
        Height = 17
        Caption = 'Filter modules'
        TabOrder = 3
      end
      object bSelectModules: TButton
        Left = 347
        Top = 8
        Width = 89
        Height = 25
        Caption = 'Select modules'
        TabOrder = 4
        OnClick = bSelectModulesClick
      end
      object StaticText1: TStaticText
        Left = 15
        Top = 14
        Width = 83
        Height = 17
        Caption = 'Patches found : '
        TabOrder = 0
      end
      object stPatchesFound: TStaticText
        Left = 97
        Top = 14
        Width = 10
        Height = 17
        Caption = '0'
        TabOrder = 1
      end
    end
  end
  object ActionManager1: TActionManager
    Left = 184
    Top = 184
    StyleName = 'XP Style'
    object aReadDir: TAction
      Caption = 'Read dir'
      OnExecute = aReadDirExecute
    end
    object aSearch: TAction
      Caption = 'Search'
    end
    object aLoadPatch: TAction
      Caption = 'Load patch'
      OnExecute = aLoadPatchExecute
    end
    object aShowPerfs: TAction
      Caption = 'Perf.'
      OnExecute = aShowPerfsExecute
    end
    object aShowPatches: TAction
      Caption = 'Patch'
      OnExecute = aShowPatchesExecute
    end
    object aRestore: TAction
      Caption = 'Restore'
      OnExecute = aRestoreExecute
    end
    object aReadDirPerf: TAction
      Caption = 'Read dir perf'
    end
    object aLoadPerf: TAction
      Caption = 'Load perf'
      OnExecute = aLoadPerfExecute
    end
  end
  object puBank: TPopupMenu
    Left = 272
    Top = 184
  end
end
