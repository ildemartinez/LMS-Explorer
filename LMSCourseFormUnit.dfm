object LMSCourseForm: TLMSCourseForm
  Left = 0
  Top = 0
  Caption = 'LMSCourseForm'
  ClientHeight = 764
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
  object Label1: TLabel
    Left = 72
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Label1'
    OnClick = LinkLabel1Click
  end
  object TabControl1: TTabControl
    Left = 56
    Top = 104
    Width = 665
    Height = 353
    TabOrder = 0
    Tabs.Strings = (
      'Participantes')
    TabIndex = 0
    object Memo1: TMemo
      Left = 88
      Top = 80
      Width = 321
      Height = 161
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object Button1: TButton
    Left = 160
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 1
    OnClick = Button1Click
  end
end
