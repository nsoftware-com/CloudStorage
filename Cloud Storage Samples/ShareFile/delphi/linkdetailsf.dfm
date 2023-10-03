object FormLinkDetails: TFormLinkDetails
  Left = 0
  Top = 0
  Caption = 'Link Details'
  ClientHeight = 115
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 415
    Height = 39
    Caption = 
      'The link below has been created to share the selected item(s) or' +
      ' request items to be uploaded in the current directory. Send thi' +
      's link to other parties to allow access to the item(s) or folder' +
      '.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 53
    Width = 22
    Height = 13
    Caption = 'Link:'
  end
  object editLink: TEdit
    Left = 36
    Top = 50
    Width = 392
    Height = 21
    ReadOnly = True
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 353
    Top = 77
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = btnOKClick
  end
end
