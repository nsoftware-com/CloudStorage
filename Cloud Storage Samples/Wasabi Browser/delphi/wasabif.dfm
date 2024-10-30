object FormWasabi: TFormWasabi
  Left = 171
  Top = 103
  Caption = 'Wasabi Demo'
  ClientHeight = 589
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    479
    589)
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 421
    Height = 39
    Anchors = [akLeft, akTop, akRight]
    Caption = 
      'This demo shows how to use the Cloud Storage to interact with Wa' +
      'sabi, an alternative to Amazon S3. It is assumed that you'#39've alr' +
      'eady signed up for the service (at http://www.wasabi.com) and ob' +
      'tained your Access Key and Secret Key.'
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
    Top = 48
    Width = 473
    Height = 97
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Wasabi Authentication'
    TabOrder = 0
    DesignSize = (
      473
      97)
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 98
      Height = 13
      Caption = 'Wasabi Access Key:'
    end
    object Label3: TLabel
      Left = 8
      Top = 56
      Width = 94
      Height = 13
      Caption = 'Wasabi Secret Key:'
    end
    object tAccessKey: TEdit
      Left = 104
      Top = 24
      Width = 297
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object tSecretKey: TEdit
      Left = 104
      Top = 56
      Width = 297
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
    end
    object bGo: TButton
      Left = 408
      Top = 40
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
    Width = 473
    Height = 209
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Wasabi Buckets'
    TabOrder = 1
    DesignSize = (
      473
      209)
    object lvwBuckets: TListView
      Left = 8
      Top = 16
      Width = 393
      Height = 185
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Name'
          Width = 185
        end
        item
          Caption = 'Creation Date'
          Width = 200
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lvwBucketsSelectItem
    end
    object bNewBucket: TButton
      Left = 408
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&New'
      TabOrder = 1
      OnClick = bNewBucketClick
    end
    object bDelete: TButton
      Left = 408
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Delete'
      TabOrder = 2
      OnClick = bDeleteClick
    end
  end
  object gbObjects: TGroupBox
    Left = 8
    Top = 368
    Width = 473
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Objects'
    TabOrder = 2
    DesignSize = (
      473
      225)
    object lvwObjects: TListView
      Left = 8
      Top = 16
      Width = 393
      Height = 201
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
        end
        item
          Caption = 'Owner'
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object bNewObject: TButton
      Left = 408
      Top = 16
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Add'
      TabOrder = 1
      OnClick = bNewObjectClick
    end
    object bDeleteObject: TButton
      Left = 408
      Top = 48
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      TabOrder = 2
      OnClick = bDeleteObjectClick
    end
    object bGetFile: TButton
      Left = 408
      Top = 80
      Width = 57
      Height = 25
      Anchors = [akTop, akRight]
      Caption = '&Get File'
      TabOrder = 3
      OnClick = bGetFileClick
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 424
    Top = 112
  end
  object OpenDialog1: TOpenDialog
    Left = 424
    Top = 112
  end
  object csWasabi1: TcsWasabi
    Region = 'us-east-1'
    SSLCertStore = 'MY'
    OnBucketList = csWasabi1BucketList
    OnObjectList = csWasabi1ObjectList
    Left = 432
    Top = 256
  end
end


