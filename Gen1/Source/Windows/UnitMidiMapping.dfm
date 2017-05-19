object frmMidiMapping: TfrmMidiMapping
  Left = 0
  Top = 0
  Caption = 'Editor UI midi mapping'
  ClientHeight = 290
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lvEditorMidiAssignments: DListView
    Left = 0
    Top = 0
    Width = 554
    Height = 290
    Align = alClient
    Columns = <
      item
        Caption = 'ControlPath'
        Width = 200
      end
      item
        Caption = 'Index'
        Width = 70
      end
      item
        Caption = 'Channel'
      end
      item
        Caption = 'Note'
      end
      item
        Caption = 'CC'
      end
      item
        Caption = 'MinValue'
      end
      item
        Caption = 'MaxValue'
      end>
    TabOrder = 0
    ViewStyle = vsReport
    OnClick = lvEditorMidiAssignmentsClick
  end
  object puCtrlAssignMidi: TPopupMenu
    Left = 208
    Top = 32
  end
end
