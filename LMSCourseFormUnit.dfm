object LMSCourseForm: TLMSCourseForm
  Left = 55
  Top = 309
  Caption = 'LMSCourseForm'
  ClientHeight = 488
  ClientWidth = 765
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 765
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager1
    Caption = 'ActionMainMenuBar1'
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
    ExplicitWidth = 876
  end
  object Panel2: TPanel
    Left = 0
    Top = 25
    Width = 765
    Height = 463
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    ExplicitLeft = 687
    ExplicitTop = 280
    ExplicitWidth = 185
    ExplicitHeight = 41
    object TabControl1: TTabControl
      Left = 1
      Top = 1
      Width = 763
      Height = 359
      Align = alClient
      TabOrder = 0
      Tabs.Strings = (
        'Users and Groups')
      TabIndex = 0
      ExplicitTop = 6
      object Panel1: TPanel
        Left = 4
        Top = 24
        Width = 755
        Height = 25
        Align = alTop
        TabOrder = 0
        ExplicitWidth = 673
        object Edit1: TEdit
          Left = 1
          Top = 1
          Width = 313
          Height = 23
          Align = alLeft
          TabOrder = 0
          OnChange = Edit1Change
          ExplicitHeight = 21
        end
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 360
      Width = 763
      Height = 102
      Align = alBottom
      TabOrder = 1
      object Memo1: TMemo
        Left = 1
        Top = 1
        Width = 185
        Height = 100
        Align = alLeft
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = 6
      end
    end
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
    Left = 464
    Top = 152
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
