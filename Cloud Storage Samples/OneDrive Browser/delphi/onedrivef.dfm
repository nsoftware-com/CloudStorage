object FormOnedrive: TFormOnedrive
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'OneDrive Demo'
  ClientHeight = 550
  ClientWidth = 630
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  PixelsPerInch = 96
  DesignSize = (
    630
    550)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 598
    Height = 26
    Caption = 
      'This demo shows how to List, Upload, Download, and Delete docume' +
      'nts from OneDrive. To begin click Authorize to allow the applica' +
      'tion to access your account. The Authorization String is created' +
      ' during this process.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 204
    Width = 614
    Height = 338
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Documents'
    TabOrder = 2
    DesignSize = (
      614
      338)
    object lvwDocuments: TListView
      Left = 8
      Top = 24
      Width = 598
      Height = 273
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Title'
          Width = 200
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'Last Modified'
          Width = 125
        end
        item
          Caption = 'Id'
          Width = 150
        end>
      HideSelection = False
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object btnListDocuments: TButton
      Left = 8
      Top = 303
      Width = 97
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'List Documents'
      TabOrder = 1
      OnClick = btnListDocumentsClick
    end
    object btnUpload: TButton
      Left = 111
      Top = 303
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Upload'
      Enabled = False
      TabOrder = 2
      OnClick = btnUploadClick
    end
    object btnDownload: TButton
      Left = 192
      Top = 303
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Download'
      Enabled = False
      TabOrder = 3
      OnClick = btnDownloadClick
    end
    object btnDelete: TButton
      Left = 273
      Top = 303
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Delete'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteClick
    end
  end
  object GroupBox4: TGroupBox
    Left = 9
    Top = 47
    Width = 614
    Height = 82
    Caption = 'OAuth Authorization'
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Client Id:'
    end
    object Label3: TLabel
      Left = 16
      Top = 51
      Width = 65
      Height = 13
      Caption = 'Client Secret:'
    end
    object txtClientId: TEdit
      Left = 121
      Top = 24
      Width = 344
      Height = 21
      TabOrder = 0
      Text = 'cd3ac0b7-c936-4b69-a958-ba45a4fb7963'
    end
    object txtClientSecret: TEdit
      Left = 121
      Top = 51
      Width = 344
      Height = 21
      TabOrder = 1
      Text = 'h1I8Q~3BTa.iBJremF5SjLAWwPSc7w164pFjkabZ'
    end
    object btnAuthorize: TButton
      Left = 488
      Top = 24
      Width = 105
      Height = 25
      Caption = '&Authorize'
      TabOrder = 2
      OnClick = btnAuthorizeClick
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 141
    Width = 614
    Height = 57
    Caption = 'OAuth Authorization String'
    TabOrder = 1
    object Label4: TLabel
      Left = 16
      Top = 24
      Width = 99
      Height = 13
      Caption = 'Authorization String:'
    end
    object txtAuthorization: TEdit
      Left = 121
      Top = 24
      Width = 344
      Height = 21
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 488
    Top = 24
  end
  object SaveDialog1: TSaveDialog
    Left = 440
    Top = 24
  end
  object csOneDrive1: TcsOneDrive
    SSLCertStore = 'MY'
    Left = 520
    Top = 157
  end
end


