object LMSForm: TLMSForm
  Left = 0
  Top = 0
  Caption = 'LMSForm'
  ClientHeight = 585
  ClientWidth = 965
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
    Width = 965
    Height = 585
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Users'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 957
        Height = 25
        Align = alTop
        TabOrder = 0
        ExplicitTop = 8
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
    object TabSheet2: TTabSheet
      AlignWithMargins = True
      Caption = 'Configuration'
      ImageIndex = 1
      object GroupBox1: TGroupBox
        Left = 0
        Top = 0
        Width = 951
        Height = 65
        Align = alTop
        Caption = 'Service configuration '
        TabOrder = 0
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
end
