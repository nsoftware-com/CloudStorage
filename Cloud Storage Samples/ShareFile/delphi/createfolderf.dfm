object FormCreateFolder: TFormCreateFolder
  Left = 0
  Top = 0
  Caption = 'Create Folder'
  ClientHeight = 117
  ClientWidth = 337
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
    Width = 291
    Height = 26
    Caption = 
      'Specify the name of the new folder that will be placed in the cu' +
      'rrent folder.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Name: '
  end
  object btnOk: TButton
    Left = 8
    Top = 88
    Width = 57
    Height = 25
    Caption = 'Ok'
    TabOrder = 0
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 272
    Top = 88
    Width = 57
    Height = 25
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object editFolderName: TEdit
    Left = 48
    Top = 61
    Width = 281
    Height = 21
    TabOrder = 2
  end
end
