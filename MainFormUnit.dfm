object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LMS Explorer'
  ClientHeight = 542
  ClientWidth = 1002
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar1: TStatusBar
    Left = 0
    Top = 523
    Width = 1002
    Height = 19
    Panels = <>
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 1002
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
  end
  object Memo1: TMemo
    Left = 0
    Top = 434
    Width = 1002
    Height = 89
    Align = alBottom
    TabOrder = 2
  end
  object Panel1: TPanel
    Left = 0
    Top = 25
    Width = 185
    Height = 409
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 3
    ExplicitLeft = 56
    ExplicitTop = 160
    ExplicitHeight = 41
    object Edit1: TEdit
      Left = 1
      Top = 1
      Width = 183
      Height = 21
      Align = alTop
      TabOrder = 0
      OnChange = Edit1Change
      ExplicitLeft = 32
      ExplicitTop = 96
      ExplicitWidth = 121
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = Action1
                Caption = '&About'
              end>
            Caption = '&Help'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 344
    Top = 72
    StyleName = 'Platform Default'
    object Action1: TAction
      Category = 'Help'
      Caption = 'About'
    end
  end
end
