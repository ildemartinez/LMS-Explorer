object LMSCourseForm: TLMSCourseForm
  Left = 0
  Top = 0
  Caption = 'LMSCourseForm'
  ClientHeight = 534
  ClientWidth = 876
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
  object TabControl1: TTabControl
    Left = 0
    Top = 25
    Width = 876
    Height = 509
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Users and Groups')
    TabIndex = 0
    ExplicitTop = 176
    ExplicitHeight = 358
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 876
    Height = 25
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
    ExplicitLeft = 544
    ExplicitTop = 64
    ExplicitWidth = 150
    ExplicitHeight = 29
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action1
            Caption = '&Locate in LMS'
          end
          item
            Caption = '-'
          end
          item
            Action = Action5
            Caption = 'E&dit course'
          end
          item
            Caption = '-'
          end
          item
            Action = Action2
            Caption = 'L&ocate course users'
          end
          item
            Caption = '-'
          end
          item
            Action = Action3
            Caption = '&User Profile'
          end
          item
            Caption = '-'
          end
          item
            Action = Action4
            Caption = '&Edit user profile'
          end>
        ActionBar = ActionMainMenuBar1
      end
      item
      end>
    Left = 432
    Top = 88
    StyleName = 'Platform Default'
    object Action1: TAction
      Caption = 'Locate in LMS'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = 'Locate course users'
      OnExecute = Action2Execute
    end
    object Action3: TAction
      Caption = 'User Profile'
      OnExecute = Action3Execute
      OnUpdate = Action3Update
    end
    object Action4: TAction
      Caption = 'Edit user profile'
      OnExecute = Action4Execute
      OnUpdate = Action4Update
    end
    object Action5: TAction
      Caption = 'Edit course'
      OnExecute = Action5Execute
    end
  end
end
