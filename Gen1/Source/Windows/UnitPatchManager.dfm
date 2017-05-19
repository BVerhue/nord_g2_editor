object frmPatchManager: TfrmPatchManager
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Patch manager'
  ClientHeight = 521
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object tcPerfPatch: TTabControl
    Left = 0
    Top = 196
    Width = 560
    Height = 325
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Performances'
      'Patches')
    TabIndex = 0
    OnChange = tcPerfPatchChange
    ExplicitWidth = 565
    object tcBank: TTabControl
      Left = 4
      Top = 25
      Width = 552
      Height = 296
      Align = alClient
      TabOrder = 0
      Tabs.Strings = (
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
      TabIndex = 0
      OnChange = tcBankChange
      ExplicitWidth = 557
      object dgBank: TDrawGrid
        Left = 4
        Top = 25
        Width = 544
        Height = 267
        Align = alClient
        BorderStyle = bsNone
        ColCount = 4
        DefaultColWidth = 72
        DefaultRowHeight = 40
        RowCount = 4
        TabOrder = 0
        OnDragDrop = dgBankDragDrop
        OnDragOver = dgBankDragOver
        OnDrawCell = dgBankDrawCell
        OnMouseDown = dgBankMouseDown
        ExplicitWidth = 549
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 560
    Height = 196
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 565
    object Label5: TLabel
      Left = 18
      Top = 8
      Width = 72
      Height = 14
      Caption = 'Performance'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btKeyboardRange: TG2GraphButtonText
      Left = 190
      Top = 87
      Width = 30
      Height = 21
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = 13684944
      Value = 0
      LowValue = 0
      HighValue = 1
      ParentColor = False
      ParentFont = False
      Caption = 'KeyboardRange'
      OnClick = btKeyboardRangeClick
      ButtonText.Strings = (
        'Off'
        'On')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 1
      ButtonTextType = bttCheck
    end
    object Label10: TLabel
      Left = 119
      Top = 90
      Width = 65
      Height = 14
      Caption = 'Keyb. range'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object btPerfMode: TG2GraphButtonText
      Left = 74
      Top = 87
      Width = 30
      Height = 21
      MidiAware = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Color = 13684944
      Value = 1
      LowValue = 0
      HighValue = 1
      ParentColor = False
      ParentFont = False
      OnClick = btPerfModeClick
      ButtonText.Strings = (
        'Off'
        'On')
      HightlightColor = clAqua
      BorderColor = clBlack
      Bevel = False
      Orientation = otHorizontal
      ButtonCount = 1
      ButtonTextType = bttCheck
    end
    object Label11: TLabel
      Left = 9
      Top = 90
      Width = 58
      Height = 14
      Caption = 'Perf mode'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object pPerf: TPanel
      Tag = 4
      Left = 5
      Top = 28
      Width = 96
      Height = 47
      BevelOuter = bvLowered
      Caption = 'Empty'
      ParentBackground = False
      TabOrder = 0
      OnDragDrop = pSlotDragDrop
      OnDragOver = pSlotDragOver
      OnMouseDown = pPerfMouseDown
    end
    object pSlot: TPanel
      Left = 0
      Top = 134
      Width = 560
      Height = 62
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
      ExplicitWidth = 565
      object Keyboard: TKeyboard
        Left = 0
        Top = 2
        Width = 561
        Height = 26
        Orientation = kbHorizontal
        Octaves = 10
        BaseOctave = 0
        LowKey = 0
        HighKey = 0
        HighLightColorBlackKeys = clTeal
        HighLightColorWhiteKeys = clAqua
        OnNoteOn = KeyboardNoteOn
        OnNoteOff = KeyboardNoteOff
        OnSetRange = KeyboardSetRange
      end
      object btEnable: TG2GraphButtonText
        Left = 51
        Top = 34
        Width = 30
        Height = 21
        MidiAware = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 1
        OnMouseDown = btEnableMouseDown
        ParentColor = False
        ParentFont = False
        Caption = 'Enable'
        OnClick = btEnableClick
        ButtonText.Strings = (
          'Off'
          'On')
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = False
        Orientation = otHorizontal
        ButtonCount = 1
        ButtonTextType = bttCheck
      end
      object Label6: TLabel
        Left = 10
        Top = 38
        Width = 36
        Height = 14
        Caption = 'Enable'
      end
      object btKeyboard: TG2GraphButtonText
        Left = 121
        Top = 34
        Width = 30
        Height = 21
        MidiAware = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 1
        OnMouseDown = btKeyboardMouseDown
        ParentColor = False
        ParentFont = False
        Caption = 'Keyboard'
        OnClick = btKeyboardClick
        ButtonText.Strings = (
          'Off'
          'On')
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = False
        Orientation = otHorizontal
        ButtonCount = 1
        ButtonTextType = bttCheck
      end
      object btHold: TG2GraphButtonText
        Left = 187
        Top = 34
        Width = 30
        Height = 21
        MidiAware = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 1
        OnMouseDown = btHoldMouseDown
        ParentColor = False
        ParentFont = False
        Caption = 'Hold'
        OnClick = btHoldClick
        ButtonText.Strings = (
          'Off'
          'On')
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = False
        Orientation = otHorizontal
        ButtonCount = 1
        ButtonTextType = bttCheck
      end
      object Label7: TLabel
        Left = 87
        Top = 38
        Width = 30
        Height = 14
        Caption = 'Keyb.'
      end
      object Label8: TLabel
        Left = 158
        Top = 38
        Width = 24
        Height = 14
        Caption = 'Hold'
      end
      object bidLowKey: TG2GraphButtonIncDec
        Left = 257
        Top = 34
        Width = 26
        Height = 21
        MidiAware = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 127
        ParentColor = False
        ParentFont = False
        OnClick = bidLowKeyClick
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = True
        Orientation = otHorizontal
        ButtonCount = 2
      end
      object bidHighKey: TG2GraphButtonIncDec
        Left = 331
        Top = 34
        Width = 26
        Height = 21
        MidiAware = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = []
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 127
        ParentColor = False
        ParentFont = False
        OnClick = bidHighKeyClick
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = True
        Orientation = otHorizontal
        ButtonCount = 2
      end
      object gdLowKey: TG2GraphDisplay
        Left = 226
        Top = 34
        Width = 30
        Height = 21
        MidiAware = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Color = 5592405
        Value = 0
        LowValue = 0
        HighValue = 0
        ParentColor = False
        ParentFont = False
        LineCount = 1
        TextFunction = 0
        DisplayType = 0
      end
      object gdHighKey: TG2GraphDisplay
        Left = 298
        Top = 34
        Width = 32
        Height = 21
        MidiAware = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Color = 5592405
        Value = 0
        LowValue = 0
        HighValue = 0
        ParentColor = False
        ParentFont = False
        LineCount = 1
        TextFunction = 0
        DisplayType = 0
      end
      object rbVariation: TG2GraphButtonRadio
        Left = 389
        Top = 34
        Width = 169
        Height = 21
        MidiAware = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -9
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        Color = 13684944
        Value = 0
        LowValue = 0
        HighValue = 7
        ParentColor = False
        ParentFont = False
        OnClick = rbVariationClick
        ButtonText.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8')
        HightlightColor = clAqua
        BorderColor = clBlack
        Bevel = False
        Orientation = otHorizontal
        ButtonCount = 8
      end
      object Label9: TLabel
        Left = 365
        Top = 38
        Width = 18
        Height = 14
        Caption = 'Var'
      end
    end
    object dgCategories: TDrawGrid
      Left = 0
      Top = 114
      Width = 560
      Height = 20
      Align = alBottom
      BevelEdges = []
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      ColCount = 16
      DefaultColWidth = 32
      DefaultRowHeight = 16
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      TabOrder = 2
      OnDrawCell = dgCategoriesDrawCell
      OnMouseDown = dgCategoriesMouseDown
      ExplicitWidth = 565
    end
    object pSelSlotA: TPanel
      Left = 110
      Top = 5
      Width = 110
      Height = 76
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 3
      object Label1: TLabel
        Left = 37
        Top = 3
        Width = 32
        Height = 14
        Caption = 'Slot A'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pSlotA: TPanel
        Left = 8
        Top = 23
        Width = 96
        Height = 47
        BevelOuter = bvLowered
        Caption = 'Empty'
        ParentBackground = False
        TabOrder = 0
        OnDragDrop = pSlotDragDrop
        OnDragOver = pSlotDragOver
        OnMouseDown = pSlotMouseDown
      end
    end
    object pSelSlotB: TPanel
      Left = 223
      Top = 5
      Width = 110
      Height = 76
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 4
      object Label2: TLabel
        Left = 38
        Top = 3
        Width = 31
        Height = 14
        Caption = 'Slot B'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pSlotB: TPanel
        Tag = 1
        Left = 8
        Top = 23
        Width = 96
        Height = 47
        BevelOuter = bvLowered
        Caption = 'Empty'
        ParentBackground = False
        TabOrder = 0
        OnDragDrop = pSlotDragDrop
        OnDragOver = pSlotDragOver
        OnMouseDown = pSlotMouseDown
      end
    end
    object pSelSlotC: TPanel
      Left = 336
      Top = 5
      Width = 110
      Height = 76
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 5
      object Label3: TLabel
        Left = 39
        Top = 3
        Width = 32
        Height = 14
        Caption = 'Slot C'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pSlotC: TPanel
        Tag = 2
        Left = 8
        Top = 23
        Width = 96
        Height = 47
        BevelOuter = bvLowered
        Caption = 'Empty'
        ParentBackground = False
        TabOrder = 0
        OnDragDrop = pSlotDragDrop
        OnDragOver = pSlotDragOver
        OnMouseDown = pSlotMouseDown
      end
    end
    object pSelSlotD: TPanel
      Left = 449
      Top = 5
      Width = 110
      Height = 76
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 6
      object Label4: TLabel
        Left = 37
        Top = 3
        Width = 31
        Height = 14
        Caption = 'Slot D'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object pSlotD: TPanel
        Tag = 3
        Left = 7
        Top = 23
        Width = 96
        Height = 47
        BevelOuter = bvLowered
        Caption = 'Empty'
        ParentBackground = False
        TabOrder = 0
        OnDragDrop = pSlotDragDrop
        OnDragOver = pSlotDragOver
        OnMouseDown = pSlotMouseDown
      end
    end
  end
  object ActionManager1: TActionManager
    Left = 176
    Top = 304
    StyleName = 'XP Style'
    object aLoadPatch: TAction
      Caption = 'Load patch'
      OnExecute = aLoadPatchExecute
    end
    object aLoadPerf: TAction
      Caption = 'Load perf'
      OnExecute = aLoadPerfExecute
    end
    object aRefresh: TAction
      Caption = 'Refresh'
      OnExecute = aRefreshExecute
    end
    object aClearBank: TAction
      Caption = 'Clear bank'
      OnExecute = aClearBankExecute
    end
    object aClearBankLocation: TAction
      Caption = 'Clear bank location'
      OnExecute = aClearBankLocationExecute
    end
    object aUploadBankToDisk: TAction
      Caption = 'Upload bank to disk'
      OnExecute = aUploadBankToDiskExecute
    end
    object aDownloadBankToG2: TAction
      Caption = 'Download bank to G2'
      OnExecute = aDownloadBankToG2Execute
    end
    object Action1: TAction
      Caption = 'Action1'
    end
  end
  object puBank: TPopupMenu
    Left = 256
    Top = 304
    object miUploadToSlotA: TMenuItem
      Caption = 'Upload to slot A'
    end
    object miUploadToSlotB: TMenuItem
      Caption = 'Upload to slot B'
    end
    object miUploadToSlotC: TMenuItem
      Caption = 'Upload to slot C'
    end
    object miUploadToSlotD: TMenuItem
      Caption = 'Upload to slot D'
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Uploadbanktodisk1: TMenuItem
      Action = aUploadBankToDisk
    end
    object DownloadbanktoG21: TMenuItem
      Action = aDownloadBankToG2
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miClear: TMenuItem
      Action = aClearBankLocation
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miClearBank: TMenuItem
      Action = aClearBank
    end
  end
  object puPerf: TPopupMenu
    Left = 320
    Top = 304
    object Initperformance1: TMenuItem
      Action = frmG2Main.aInitPerf
    end
    object Loadperformance1: TMenuItem
      Action = frmG2Main.aLoadPerformance
    end
    object Saveperformance1: TMenuItem
      Action = frmG2Main.aSavePerformance
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object Renameperformance1: TMenuItem
      Action = frmG2Main.aPerfRename
    end
  end
  object puPatch: TPopupMenu
    Left = 376
    Top = 304
    object Initpatch1: TMenuItem
      Action = frmG2Main.aInitPatch
    end
    object Loadpatch1: TMenuItem
      Action = frmG2Main.aLoadPatch
    end
    object Savepatch1: TMenuItem
      Action = frmG2Main.aSavePatch
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Renamepatch1: TMenuItem
      Action = frmG2Main.aPatchRename
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 432
    Top = 304
  end
  object SaveDialog1: TSaveDialog
    Left = 496
    Top = 304
  end
end
