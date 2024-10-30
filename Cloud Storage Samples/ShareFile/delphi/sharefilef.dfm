object FormSharefile: TFormSharefile
  Left = 0
  Top = 0
  Caption = 'ShareFile'
  ClientHeight = 638
  ClientWidth = 596
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
    Width = 573
    Height = 26
    Caption = 
      'This demo shows how to list, upload, download, delete, and share' +
      ' documents from ShareFile.  To begin, click Authorize to allow t' +
      'he application to access your account.'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 8
    Top = 263
    Width = 203
    Height = 13
    Caption = 'Double click a folder to change directories.'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 581
    Height = 65
    Caption = 'ShareFile'
    TabOrder = 0
    object Label2: TLabel
      Left = 17
      Top = 25
      Width = 98
      Height = 13
      Caption = 'Account Subdomain:'
    end
    object txtAccountSubdomain: TEdit
      Left = 121
      Top = 25
      Width = 352
      Height = 21
      TabOrder = 0
      Text = 'nsoftware'
    end
  end
  object GroupBox2: TGroupBox
    Left = 9
    Top = 111
    Width = 580
    Height = 146
    Caption = 'OAuth Authorization'
    TabOrder = 1
    object Label3: TLabel
      Left = 19
      Top = 36
      Width = 44
      Height = 13
      Caption = 'Client Id:'
    end
    object Label4: TLabel
      Left = 19
      Top = 63
      Width = 65
      Height = 13
      Caption = 'Client Secret:'
    end
    object Label5: TLabel
      Left = 19
      Top = 90
      Width = 99
      Height = 13
      Caption = 'Authorization String:'
    end
    object txtClientId: TEdit
      Left = 121
      Top = 33
      Width = 352
      Height = 21
      TabOrder = 0
      Text = 'Hb7kR4d6mBvQxnOe9Eg6V8CMIVjQ67vD'
    end
    object txtClientSecret: TEdit
      Left = 121
      Top = 60
      Width = 352
      Height = 21
      TabOrder = 1
      Text = 'jHAZH6f1Ac5O9krW9az3fLYFEZW4KoF1sr1hqDoVgrvv1zwc'
    end
    object txtAuthorization: TEdit
      Left = 121
      Top = 87
      Width = 352
      Height = 21
      TabOrder = 2
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 288
    Width = 580
    Height = 330
    Caption = 'Documents'
    TabOrder = 2
    DesignSize = (
      580
      330)
    object lstDocuments: TListView
      Left = 3
      Top = 24
      Width = 574
      Height = 267
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
          Caption = 'Creation Date'
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
      OnDblClick = lstDocumentsDblClick
    end
    object btnUpload: TButton
      Left = 90
      Top = 297
      Width = 71
      Height = 25
      Caption = '&Upload'
      Enabled = False
      TabOrder = 1
      OnClick = btnUploadClick
    end
    object btnDownload: TButton
      Left = 167
      Top = 297
      Width = 74
      Height = 25
      Caption = '&Download'
      Enabled = False
      TabOrder = 2
      OnClick = btnDownloadClick
    end
    object btnDelete: TButton
      Left = 247
      Top = 297
      Width = 74
      Height = 25
      Caption = 'D&elete'
      Enabled = False
      TabOrder = 3
      OnClick = btnDeleteClick
    end
    object btnRequest: TButton
      Left = 407
      Top = 297
      Width = 74
      Height = 25
      Caption = '&Request Link'
      Enabled = False
      TabOrder = 4
      OnClick = btnRequestClick
    end
    object btnCreate: TButton
      Left = 327
      Top = 297
      Width = 74
      Height = 25
      Caption = '&Create Folder'
      Enabled = False
      TabOrder = 5
      OnClick = btnCreateClick
    end
    object btnShare: TButton
      Left = 487
      Top = 297
      Width = 74
      Height = 25
      Caption = '&Share Link'
      Enabled = False
      TabOrder = 6
      OnClick = btnShareClick
    end
  end
  object btnAuthorize: TButton
    Left = 496
    Top = 142
    Width = 75
    Height = 25
    Caption = '&Authorize'
    TabOrder = 3
    OnClick = btnAuthorizeClick
  end
  object btnList: TButton
    Left = 8
    Top = 585
    Width = 84
    Height = 25
    Caption = '&List Documents'
    Enabled = False
    TabOrder = 4
    OnClick = btnListClick
  end
  object SaveDialog1: TSaveDialog
    Left = 448
    Top = 224
  end
  object OpenDialog1: TOpenDialog
    Left = 384
    Top = 224
  end
  object csShareFile1: TcsShareFile
    SSLCertStore = 'MY'
    Left = 504
    Top = 176
  end
  object csOAuth1: TcsOAuth
    SSLCertStore = 'MY'
    WebServerSSLCertStore = 'MY'
    Left = 528
    Top = 232
  end
end


