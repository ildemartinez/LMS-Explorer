object LMSUserPasswordForm: TLMSUserPasswordForm
  Left = 0
  Top = 0
  Caption = 'Connect to LMS'
  ClientHeight = 127
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  DesignSize = (
    332
    127)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 27
    Width = 48
    Height = 13
    Caption = 'Username'
  end
  object Label2: TLabel
    Left = 8
    Top = 54
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Button1: TButton
    Left = 246
    Top = 94
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Ok'
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 384
    ExplicitTop = 169
  end
  object Button2: TButton
    Left = 165
    Top = 94
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 303
    ExplicitTop = 169
  end
  object EdUserName: TEdit
    Left = 88
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'EdUserName'
  end
  object EdPassword: TEdit
    Left = 88
    Top = 51
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'EdUserName'
  end
end
