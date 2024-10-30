object FormBox: TFormBox
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Box Demo'
  ClientHeight = 593
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
    593)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 614
    Height = 52
    Caption = 
      'This demo shows how to List, Upload, Download, and Delete docume' +
      'nts from Box. To begin, you will first need to setup an app in t' +
      'he Box developer console to obtain your App Key and App Secret. ' +
      'You will also need to setup a Redirect URI to http://localhost:P' +
      'ORT, where PORT is the port number configured below. Then click ' +
      'Authorize to allow the application to access your account; the A' +
      'uthorization String is created during this process.'
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
    Top = 247
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
          Width = 250
        end
        item
          Caption = 'Type'
          Width = 125
        end
        item
          Caption = 'Id'
          Width = 225
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
    Left = 8
    Top = 68
    Width = 614
    Height = 110
    Caption = 'OAuth Authorization'
    TabOrder = 0
    object Label2: TLabel
      Left = 16
      Top = 24
      Width = 44
      Height = 13
      Caption = 'App Key:'
    end
    object Label3: TLabel
      Left = 16
      Top = 51
      Width = 57
      Height = 13
      Caption = 'App Secret:'
    end
    object Label5: TLabel
      Left = 16
      Top = 78
      Width = 88
      Height = 13
      Caption = 'Redirect URI Port:'
    end
    object txtClientId: TEdit
      Left = 121
      Top = 24
      Width = 344
      Height = 21
      TabOrder = 0
    end
    object txtClientSecret: TEdit
      Left = 121
      Top = 51
      Width = 344
      Height = 21
      TabOrder = 1
    end
    object btnAuthorize: TButton
      Left = 488
      Top = 24
      Width = 105
      Height = 25
      Caption = '&Authorize'
      TabOrder = 3
      OnClick = btnAuthorizeClick
    end
    object txtPort: TEdit
      Left = 121
      Top = 78
      Width = 80
      Height = 21
      TabOrder = 2
      Text = '7777'
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 184
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
    Top = 144
  end
  object SaveDialog1: TSaveDialog
    Left = 440
    Top = 144
  end
  object csBox1: TcsBox
    SSLCertStore = 'MY'
    Left = 512
    Top = 208
  end
end


