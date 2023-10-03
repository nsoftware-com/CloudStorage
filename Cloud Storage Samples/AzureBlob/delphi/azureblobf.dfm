object FormAzureblob: TFormAzureblob
  Left = 171
  Top = 103
  Caption = 'Azure Blob Demo'
  ClientHeight = 600
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    496
    600)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 477
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This demo shows how to use the Cloud Storage to interact with Az' +
      'ure Blob Storage Service.  It is assumed that you'#39've already sig' +
      'ned up for the service (at http://www.microsoft.com/windowsazure' +
      '/) and have an Account and Access Key which are required to use ' +
      'the service.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clHighlight
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 49
    Width = 482
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Blob Storage Service Authentication'
    TabOrder = 0
    DesignSize = (
      482
      97)
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 43
      Height = 13
      Caption = 'Account:'
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 59
      Height = 13
      Caption = 'Access Key:'
    end
    object tAccount: TEdit
      Left = 73
      Top = 24
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object tAccessKey: TEdit
      Left = 73
      Top = 56
      Width = 337
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object bGo: TButton
      Left = 416
      Top = 51
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Go'
      TabOrder = 2
      OnClick = bGoClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 152
    Width = 482
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Blob Containers'
    TabOrder = 1
    DesignSize = (
      482
      209)
    object lvwContainers: TListView
      Left = 8
      Top = 16
      Width = 402
      Height = 185
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 185
        end
        item
          Caption = 'Last Modified'
          Width = 200
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvwContainersSelectItem
    end
    object bNewContainer: TButton
      Left = 417
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&New'
      TabOrder = 1
      OnClick = bNewContainerClick
    end
    object bDeleteContainer: TButton
      Left = 417
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 2
      OnClick = bDeleteContainerClick
    end
  end
  object gbBlobs: TGroupBox
    Left = 8
    Top = 368
    Width = 482
    Height = 224
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Blobs'
    TabOrder = 2
    DesignSize = (
      482
      224)
    object lvwBlobs: TListView
      Left = 8
      Top = 16
      Width = 402
      Height = 200
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 185
        end
        item
          Caption = 'Last Modified'
          Width = 100
        end
        item
          Caption = 'Size'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object bNewBlob: TButton
      Left = 417
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = bNewBlobClick
    end
    object bDeleteBlob: TButton
      Left = 417
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 2
      OnClick = bDeleteBlobClick
    end
    object bGetBlob: TButton
      Left = 417
      Top = 80
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Get File'
      TabOrder = 3
      OnClick = bGetBlobClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 344
    Top = 65528
  end
  object OpenDialog1: TOpenDialog
    Left = 400
  end
  object csAzureBlob1: TcsAzureBlob
    SSLCertStore = 'MY'
    OnBlobList = csAzureblob1BlobList
    OnContainerList = csAzureblob1ContainerList
    Left = 440
    Top = 272
  end
end


