object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'LMS Explorer'
  ClientHeight = 685
  ClientWidth = 1354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 281
    Top = 25
    Width = 5
    Height = 530
  end
  object ActionMainMenuBar1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 1354
    Height = 25
    UseSystemFont = False
    ActionManager = MainActionManager
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
    Top = 555
    Width = 1354
    Height = 130
    Align = alBottom
    TabOrder = 1
    Visible = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 25
    Width = 281
    Height = 530
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 2
    object ActionMainMenuBar2: TActionMainMenuBar
      Left = 1
      Top = 22
      Width = 279
      Height = 25
      UseSystemFont = False
      ActionManager = CoursesActionManager
      Caption = 'ActionMainMenuBar2'
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
    object edFilter: TEdit
      Left = 1
      Top = 1
      Width = 279
      Height = 21
      Align = alTop
      TabOrder = 1
      TextHint = 'Filter courses'
      OnChange = edFilterChange
    end
  end
  object MainActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = actExit
                Caption = '&Exit'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = WindowCascade1
                ImageIndex = 17
              end
              item
                Action = WindowMinimizeAll1
              end
              item
                Action = Action2
                Caption = 'C&lose All'
              end>
            Caption = 'W&indow'
          end
          item
            Caption = '-'
          end
          item
            Items = <
              item
                Action = actAbout
                Caption = '&About'
              end>
            Caption = '&Help'
          end>
        ActionBar = ActionMainMenuBar1
      end>
    Left = 544
    Top = 224
    StyleName = 'Platform Default'
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About'
      OnExecute = actAboutExecute
    end
    object WindowCascade1: TWindowCascade
      Category = 'Window'
      Caption = '&Cascade'
      Enabled = False
      Hint = 'Cascade'
      ImageIndex = 17
    end
    object WindowMinimizeAll1: TWindowMinimizeAll
      Category = 'Window'
      Caption = '&Minimize All'
      Enabled = False
      Hint = 'Minimize All'
    end
    object Action2: TAction
      Category = 'Window'
      Caption = 'Close All'
      Hint = 'Close all windows'
      OnExecute = Action2Execute
      OnUpdate = Action2Update
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
  end
  object CoursesActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action3
            Caption = '&Clear filter'
          end
          item
            Caption = '-'
          end>
        ActionBar = ActionMainMenuBar2
      end>
    Left = 440
    Top = 176
    StyleName = 'Platform Default'
    object Action3: TAction
      Caption = 'Clear filter'
      OnExecute = Action3Execute
      OnUpdate = Action3Update
    end
  end
  object TrayIcon1: TTrayIcon
    BalloonTitle = 'Notice'
    BalloonTimeout = 5000
    BalloonFlags = bfInfo
    OnDblClick = TrayIcon1DblClick
    Left = 472
    Top = 72
  end
  object ApplicationEvents1: TApplicationEvents
    OnMinimize = ApplicationEvents1Minimize
    Left = 592
    Top = 72
  end
end
