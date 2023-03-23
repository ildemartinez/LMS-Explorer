object LMSCategoryForm: TLMSCategoryForm
  Left = 0
  Top = 0
  Caption = 'LMSCategoryForm'
  ClientHeight = 299
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ActionToolBar1: TActionToolBar
    Left = 0
    Top = 0
    Width = 852
    Height = 23
    ActionManager = ActionManager1
    Caption = 'ActionToolBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Spacing = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 23
    Width = 852
    Height = 276
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    object TabControl1: TTabControl
      Left = 1
      Top = 1
      Width = 850
      Height = 274
      Align = alClient
      TabOrder = 0
      Tabs.Strings = (
        'Courses and users')
      TabIndex = 0
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action1
            Caption = '&Open courses in Moodle'
          end
          item
            Action = Action2
            Caption = 'O&pen course users'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 456
    Top = 96
    StyleName = 'Platform Default'
    object Action1: TAction
      Caption = 'Open courses in Moodle'
      OnExecute = Action1Execute
      OnUpdate = Action1Update
    end
    object Action2: TAction
      AutoCheck = True
      Caption = 'Open course users'
      OnExecute = Action2Execute
      OnUpdate = Action2Update
    end
  end
end
