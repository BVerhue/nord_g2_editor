object frmSettings: TfrmSettings
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Application settings'
  ClientHeight = 438
  ClientWidth = 522
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 522
    Height = 375
    ActivePage = TabSheet3
    Align = alClient
    TabOrder = 0
    object TabSheet3: TTabSheet
      Caption = 'General'
      ImageIndex = 2
      object GroupBox1: TGroupBox
        Left = 12
        Top = 24
        Width = 485
        Height = 153
        Caption = 'Folders'
        TabOrder = 0
        object bSelectPtachRootFolder: TButton
          Left = 423
          Top = 23
          Width = 35
          Height = 25
          Caption = '...'
          TabOrder = 2
          OnClick = bSelectPtachRootFolderClick
        end
        object StaticText12: TStaticText
          Left = 11
          Top = 30
          Width = 85
          Height = 17
          Caption = 'Patch root folder'
          FocusControl = ePatchRootFolder
          TabOrder = 0
          TabStop = True
        end
        object ePatchRootFolder: DEdit
          Left = 112
          Top = 26
          Width = 301
          Height = 21
          TabOrder = 1
          OnEnter = ePatchRootFolderEnter
        end
        object StaticText16: TStaticText
          Left = 12
          Top = 60
          Width = 78
          Height = 17
          Caption = 'Module help file'
          TabOrder = 3
        end
        object StaticText17: TStaticText
          Left = 12
          Top = 89
          Width = 66
          Height = 17
          Caption = 'g2ools folder'
          TabOrder = 6
        end
        object eModuleHelpFile: TEdit
          Left = 112
          Top = 56
          Width = 301
          Height = 21
          TabOrder = 4
          OnEnter = eModuleHelpFileEnter
        end
        object eG2oolsFolder: TEdit
          Left = 112
          Top = 85
          Width = 301
          Height = 21
          TabOrder = 7
          OnEnter = eG2oolsFolderEnter
        end
        object bSelectModuleHelpFile: TButton
          Left = 423
          Top = 54
          Width = 35
          Height = 25
          Caption = '...'
          TabOrder = 5
          OnClick = bSelectModuleHelpFileClick
        end
        object bSelectG2oolsFolder: TButton
          Left = 423
          Top = 83
          Width = 35
          Height = 25
          Caption = '...'
          TabOrder = 8
          OnClick = bSelectG2oolsFolderClick
        end
        object ePatchBufferFolder: TEdit
          Left = 112
          Top = 114
          Width = 301
          Height = 21
          TabOrder = 10
          OnEnter = ePatchBufferFolderEnter
        end
        object bSelectPatchBufferFolder: TButton
          Left = 423
          Top = 114
          Width = 35
          Height = 25
          Caption = '...'
          TabOrder = 11
          OnClick = bSelectPatchBufferFolderClick
        end
        object StaticText19: TStaticText
          Left = 11
          Top = 118
          Width = 95
          Height = 17
          Caption = 'Patch buffer folder'
          TabOrder = 9
        end
      end
      object GroupBox2: TGroupBox
        Left = 12
        Top = 191
        Width = 485
        Height = 82
        Caption = 'USB Message log'
        TabOrder = 1
        object cbLogEnabled: TCheckBox
          Left = 12
          Top = 32
          Width = 97
          Height = 17
          Caption = 'Log enabled'
          TabOrder = 0
          OnClick = cbLogEnabledClick
          OnEnter = cbLogEnabledEnter
        end
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Ctrl Midi'
      ImageIndex = 5
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StaticText6: TStaticText
        Left = 17
        Top = 21
        Width = 53
        Height = 17
        Caption = 'Ctrl midi in'
        TabOrder = 0
        TabStop = True
      end
      object bMidiMapping: TButton
        Left = 17
        Top = 290
        Width = 75
        Height = 25
        Caption = 'Mapping'
        TabOrder = 2
        OnClick = bMidiMappingClick
      end
      object StaticText18: TStaticText
        Left = 17
        Top = 171
        Width = 61
        Height = 17
        Caption = 'Ctrl midi out'
        TabOrder = 1
      end
      object clbCtrlMidiInDevices: TCheckListBox
        Left = 104
        Top = 21
        Width = 369
        Height = 144
        OnClickCheck = clbCtrlMidiInDevicesClickCheck
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        OnEnter = clbCtrlMidiInDevicesEnter
      end
      object clbCtrlMidiOutDevices: TCheckListBox
        Left = 104
        Top = 171
        Width = 369
        Height = 144
        OnClickCheck = clbCtrlMidiOutDevicesClickCheck
        ItemHeight = 13
        Sorted = True
        TabOrder = 4
        OnEnter = clbCtrlMidiOutDevicesEnter
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'TCP-IP'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cbIsServer: TCheckBox
        Left = 198
        Top = 62
        Width = 91
        Height = 17
        Caption = 'Is server'
        TabOrder = 0
        OnClick = cbIsServerClick
        OnEnter = cbIsServerEnter
      end
      object StaticText1: TStaticText
        Left = 32
        Top = 97
        Width = 24
        Height = 17
        Caption = 'Port'
        FocusControl = ePort
        TabOrder = 1
        TabStop = True
      end
      object StaticText2: TStaticText
        Left = 32
        Top = 132
        Width = 26
        Height = 17
        Caption = 'Host'
        FocusControl = eHost
        TabOrder = 3
        TabStop = True
      end
      object StaticText3: TStaticText
        Left = 32
        Top = 227
        Width = 137
        Height = 17
        Caption = 'Broadcast leds interval (ms)'
        FocusControl = eTimerBroadcastLedMessages
        TabOrder = 6
        TabStop = True
      end
      object ePort: DEdit
        Left = 198
        Top = 93
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '2501'
        OnEnter = ePortEnter
      end
      object eHost: DEdit
        Left = 198
        Top = 127
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '127.0.0.1'
        OnEnter = eHostEnter
      end
      object eTimerBroadcastLedMessages: DEdit
        Left = 198
        Top = 223
        Width = 121
        Height = 21
        TabOrder = 7
        Text = '500'
        OnEnter = eTimerBroadcastLedMessagesEnter
      end
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 514
        Height = 25
        Align = alTop
        Caption = 'Panel3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 8
      end
      object bCreateG2VSTIni: TButton
        Left = 198
        Top = 162
        Width = 121
        Height = 25
        Caption = 'Create G2 VST Ini file'
        TabOrder = 5
        OnClick = bCreateG2VSTIniClick
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Midi sysex'
      ImageIndex = 3
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object StaticText5: TStaticText
        Left = 19
        Top = 189
        Width = 73
        Height = 17
        Caption = 'Sysex midi out'
        TabOrder = 1
        TabStop = True
      end
      object StaticText4: TStaticText
        Left = 19
        Top = 39
        Width = 65
        Height = 17
        Caption = 'Sysex midi in'
        TabOrder = 0
        TabStop = True
      end
      object clbSysexMidiInDevices: TCheckListBox
        Left = 104
        Top = 39
        Width = 345
        Height = 144
        OnClickCheck = clbSysexMidiInDevicesClickCheck
        ItemHeight = 13
        Sorted = True
        TabOrder = 2
        OnEnter = clbSysexMidiInDevicesEnter
      end
      object clbSysExMidiOutDevices: TCheckListBox
        Left = 104
        Top = 189
        Width = 345
        Height = 144
        OnClickCheck = clbSysExMidiOutDevicesClickCheck
        ItemHeight = 13
        Sorted = True
        TabOrder = 3
        OnEnter = clbSysExMidiOutDevicesEnter
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 514
        Height = 25
        Align = alTop
        Caption = 'Panel3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Editor'
      ImageIndex = 4
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object cbSlotStripColor: TColorBox
        Left = 184
        Top = 88
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 3
        OnChange = cbSlotStripColorChange
        OnEnter = cbSlotStripColorEnter
      end
      object cbSlotStripInverseColor: TColorBox
        Left = 184
        Top = 116
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 5
        OnChange = cbSlotStripInverseColorChange
        OnEnter = cbSlotStripInverseColorEnter
      end
      object cbSlotStripDisabledColor: TColorBox
        Left = 184
        Top = 144
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 7
        OnChange = cbSlotStripDisabledColorChange
        OnEnter = cbSlotStripDisabledColorEnter
      end
      object cbHighLightColor: TColorBox
        Left = 184
        Top = 172
        Width = 145
        Height = 22
        Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbCustomColors]
        TabOrder = 9
        OnChange = cbHighLightColorChange
        OnEnter = cbHighLightColorEnter
      end
      object cbOnlyTextMenus: TCheckBox
        Left = 184
        Top = 230
        Width = 97
        Height = 17
        Caption = 'Only text menus'
        TabOrder = 12
        OnClick = cbOnlyTextMenusClick
        OnEnter = cbOnlyTextMenusEnter
      end
      object StaticText7: TStaticText
        Left = 48
        Top = 62
        Width = 78
        Height = 17
        Caption = 'Cable thickness'
        FocusControl = eCableThickness
        TabOrder = 0
        TabStop = True
      end
      object StaticText8: TStaticText
        Left = 48
        Top = 93
        Width = 72
        Height = 17
        Caption = 'Slot strip color'
        FocusControl = cbSlotStripColor
        TabOrder = 2
        TabStop = True
      end
      object StaticText9: TStaticText
        Left = 48
        Top = 121
        Width = 110
        Height = 17
        Caption = 'Slot strip inverse color'
        FocusControl = cbSlotStripInverseColor
        TabOrder = 4
        TabStop = True
      end
      object StaticText10: TStaticText
        Left = 48
        Top = 149
        Width = 114
        Height = 17
        Caption = 'Slot strip disabled color'
        FocusControl = cbSlotStripDisabledColor
        TabOrder = 6
        TabStop = True
      end
      object StaticText11: TStaticText
        Left = 48
        Top = 177
        Width = 71
        Height = 17
        Caption = 'Highlight color'
        FocusControl = cbHighLightColor
        TabOrder = 8
        TabStop = True
      end
      object eCableThickness: DEdit
        Left = 184
        Top = 58
        Width = 33
        Height = 21
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        TabOrder = 1
        Text = '2'
        OnEnter = eCableThicknessEnter
      end
      object StaticText15: TStaticText
        Left = 48
        Top = 205
        Width = 47
        Height = 17
        Caption = 'Led color'
        TabOrder = 10
        TabStop = True
      end
      object cbLedColor: TColorBox
        Left = 184
        Top = 200
        Width = 145
        Height = 22
        TabOrder = 11
        OnChange = cbLedColorChange
        OnEnter = cbLedColorEnter
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 514
        Height = 25
        Align = alTop
        Caption = 'Panel3'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 13
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'OSC'
      ImageIndex = 1
      TabVisible = False
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 514
        Height = 59
        Align = alTop
        TabOrder = 0
        object Button2: TButton
          Left = 334
          Top = 11
          Width = 75
          Height = 25
          Caption = 'Activate'
          TabOrder = 4
          OnClick = Button2Click
        end
        object StaticText13: TStaticText
          Left = 19
          Top = 17
          Width = 63
          Height = 17
          Caption = 'OSC Host IP'
          FocusControl = eOSCServerIP
          TabOrder = 0
          TabStop = True
        end
        object StaticText14: TStaticText
          Left = 191
          Top = 17
          Width = 73
          Height = 17
          Caption = 'OSC Host Port'
          FocusControl = eOSCHostPort
          TabOrder = 2
          TabStop = True
        end
        object eOSCServerIP: DEdit
          Left = 90
          Top = 13
          Width = 73
          Height = 21
          TabOrder = 1
          Text = '127.0.0.1'
        end
        object eOSCHostPort: DEdit
          Left = 273
          Top = 13
          Width = 41
          Height = 21
          TabOrder = 3
          Text = '5678'
        end
      end
      object Memo1: TMemo
        Left = 0
        Top = 59
        Width = 514
        Height = 288
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
        TabOrder = 1
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 375
    Width = 522
    Height = 63
    Align = alBottom
    TabOrder = 1
    object lExplenation: TStaticText
      Left = 1
      Top = 1
      Width = 520
      Height = 61
      Align = alClient
      Alignment = taCenter
      Caption = 'Application settings'
      TabOrder = 0
    end
  end
  object IdUDPServer1: TIdUDPServer
    OnStatus = IdUDPServer1Status
    Bindings = <>
    DefaultPort = 5678
    Left = 392
  end
  object OpenDialog1: TOpenDialog
    Left = 336
  end
end
