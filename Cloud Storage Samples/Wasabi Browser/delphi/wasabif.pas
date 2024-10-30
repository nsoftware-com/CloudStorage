(*
 * Cloud Storage 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of Cloud Storage in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/cloudstorage
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)
unit wasabif;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, cscore, cstypes, cswasabi;

type
  TFormWasabi = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    tAccessKey: TEdit;
    Label3: TLabel;
    tSecretKey: TEdit;
    bGo: TButton;
    GroupBox2: TGroupBox;
    lvwBuckets: TListView;
    bNewBucket: TButton;
    bDelete: TButton;
    gbObjects: TGroupBox;
    lvwObjects: TListView;
    bNewObject: TButton;
    bDeleteObject: TButton;
    bGetFile: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    csWasabi1: TcsWasabi;
    procedure bGoClick(Sender: TObject);
    procedure bNewBucketClick(Sender: TObject);
    procedure bDeleteClick(Sender: TObject);
    procedure bDeleteObjectClick(Sender: TObject);
    procedure bGetFileClick(Sender: TObject);
    procedure bNewObjectClick(Sender: TObject);
    procedure lvwBucketsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure csWasabi1BucketList(Sender: TObject; const BucketName,
      CreationDate, OwnerId, OwnerName: String);
    procedure csWasabi1ObjectList(Sender: TObject; const ObjectName,
      LastModified: string; Size: Int64; const ETag, OwnerId, OwnerName,
      UploadId, VersionId: string; LatestVersion, Deleted: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormWasabi: TFormWasabi;

implementation

{$R *.dfm}

procedure TFormWasabi.bGoClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    csWasabi1.AccessKey := tAccessKey.Text;
    csWasabi1.SecretKey := tSecretKey.Text;

    lvwBuckets.Items.Clear();
    csWasabi1.ListBuckets();
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormWasabi.bNewBucketClick(Sender: TObject);
var newBucketName:string;
begin
  if InputQuery('Enter Bucket Name', 'Bucket Name?',
                newBucketName) then
  begin
    try
      Screen.Cursor := crHourGlass;
      csWasabi1.Bucket := newBucketName;
      csWasabi1.CreateBucket();

      lvwBuckets.Items.Clear();
      csWasabi1.ListBuckets();

      gbObjects.Caption := 'Objects in bucket ' + csWasabi1.Bucket;
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWasabi.bDeleteClick(Sender: TObject);
begin
  if lvwBuckets.SelCount = 0 then
  begin
    ShowMessage('Select a bucket first!');
    exit;
  end;

  if MessageDlg('Delete bucket ' + csWasabi1.Bucket + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      csWasabi1.DeleteBucket();
      lvwBuckets.Items.Clear();
      csWasabi1.ListBuckets();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWasabi.bDeleteObjectClick(Sender: TObject);
begin
  if lvwObjects.SelCount = 0 then
  begin
    ShowMessage('Select an object first!');
    exit;
  end;

  if MessageDlg('Delete object ' + lvwObjects.Selected.Caption + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      csWasabi1.DeleteObject(lvwObjects.Selected.Caption);
      lvwObjects.Items.Clear();
      csWasabi1.ListObjects();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWasabi.bGetFileClick(Sender: TObject);
begin
  if SaveDialog1.Execute = true then
  begin
    try
      Screen.Cursor := crHourGlass;
      csWasabi1.LocalFile := SaveDialog1.FileName;
      csWasabi1.GetObject(lvwObjects.Selected.Caption);
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWasabi.bNewObjectClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Screen.Cursor := crHourGlass;
      csWasabi1.LocalFile := OpenDialog1.FileName;
      csWasabi1.CreateObject(ExtractFileName(OpenDialog1.FileName));

      lvwObjects.Items.Clear();
      csWasabi1.ListObjects();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormWasabi.lvwBucketsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lvwBuckets.SelCount = 0 then exit;

  try
    Screen.Cursor := crHourGlass;
    csWasabi1.Bucket := lvwBuckets.Selected.Caption;

    lvwObjects.Items.Clear();
    csWasabi1.ListObjects();
    gbObjects.Caption := 'Objects in bucket ' + lvwBuckets.Selected.Caption;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormWasabi.csWasabi1BucketList(Sender: TObject; const BucketName,
  CreationDate, OwnerId, OwnerName: String);
begin
  lvwBuckets.Items.Add();
  lvwBuckets.Items.Item[lvwBuckets.Items.Count - 1].Caption := BucketName;
  lvwBuckets.Items.Item[lvwBuckets.Items.Count - 1].SubItems.Add(CreationDate);
end;

procedure TFormWasabi.csWasabi1ObjectList(Sender: TObject; const ObjectName,
  LastModified: string; Size: Int64; const ETag, OwnerId, OwnerName, UploadId,
  VersionId: string; LatestVersion, Deleted: Boolean);
begin
  lvwObjects.Items.Add();
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].Caption := ObjectName;
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(LastModified);
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(IntToStr(Size));
  lvwObjects.Items.Item[lvwObjects.Items.Count - 1].SubItems.Add(OwnerName);
end;

end.

