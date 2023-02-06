object LMSForm: TLMSForm
  Left = 0
  Top = 0
  Caption = 'LMSForm'
  ClientHeight = 336
  ClientWidth = 582
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 582
    Height = 336
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 965
    ExplicitHeight = 585
    object TabSheet1: TTabSheet
      Caption = 'Users'
      object Panel1: TPanel
        Left = 0
        Top = 23
        Width = 574
        Height = 25
        Align = alTop
        TabOrder = 0
        ExplicitTop = 0
        ExplicitWidth = 957
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
      object ActionToolBar1: TActionToolBar
        Left = 0
        Top = 0
        Width = 574
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
    end
    object TabSheet2: TTabSheet
      AlignWithMargins = True
      Caption = 'Configuration'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 568
        Height = 65
        Align = alTop
        Caption = 'Service configuration '
        TabOrder = 0
        ExplicitWidth = 951
        object Button1: TButton
          Left = 19
          Top = 24
          Width = 214
          Height = 25
          Caption = 'Got to LMS external service configuration'
          TabOrder = 0
          OnClick = Button1Click
        end
      end
    end
  end
  object ActionManager1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Action = Action1
            Caption = '&Admin users'
          end
          item
            Caption = '-'
          end
          item
            Action = Action2
            Caption = '&Create User'
          end
          item
            Action = Action3
            Caption = '&Upload users'
          end>
      end
      item
        Items = <
          item
            Action = Action2
            Caption = '&Create User'
          end
          item
            Action = Action1
            Caption = '&Admin users'
          end
          item
            Action = Action3
            Caption = '&Upload users'
          end>
        ActionBar = ActionToolBar1
      end>
    Left = 428
    Top = 152
    StyleName = 'Platform Default'
    object Action1: TAction
      Caption = 'Admin users'
      OnExecute = Action1Execute
    end
    object Action2: TAction
      Caption = 'Create User'
      OnExecute = Action2Execute
    end
    object Action3: TAction
      Caption = 'Upload users'
      OnExecute = Action3Execute
    end
  end
end
