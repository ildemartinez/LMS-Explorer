object LMSForm: TLMSForm
  Left = 0
  Top = 0
  Caption = 'LMSForm'
  ClientHeight = 543
  ClientWidth = 803
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
    Top = 0
    Width = 803
    Height = 543
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'Users')
    TabIndex = 0
    object Panel1: TPanel
      Left = 4
      Top = 24
      Width = 795
      Height = 25
      Align = alTop
      TabOrder = 0
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
end
