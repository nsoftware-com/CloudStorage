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
unit azureblobf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, cscore, cstypes, csazureblob;

type
  TFormAzureblob = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    tAccount: TEdit;
    Label3: TLabel;
    tAccessKey: TEdit;
    bGo: TButton;
    GroupBox2: TGroupBox;
    lvwContainers: TListView;
    bNewContainer: TButton;
    bDeleteContainer: TButton;
    gbBlobs: TGroupBox;
    lvwBlobs: TListView;
    bNewBlob: TButton;
    bDeleteBlob: TButton;
    bGetBlob: TButton;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    csAzureBlob1: TcsAzureBlob;
    procedure bGoClick(Sender: TObject);
    procedure bNewContainerClick(Sender: TObject);
    procedure bDeleteContainerClick(Sender: TObject);
    procedure bDeleteBlobClick(Sender: TObject);
    procedure bGetBlobClick(Sender: TObject);
    procedure bNewBlobClick(Sender: TObject);
    procedure lvwContainersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure csAzureblob1ContainerList(Sender: TObject; const Name, ModifiedTime,
      ETag: String; IsLeased: Boolean; LeaseState: Integer);
    procedure csAzureblob1BlobList(Sender: TObject; const Name, Container: String;
      BlobType: Integer; const Snapshot: String; ContentLength: Int64; const ContentType,
      CreatedTime, ModifiedTime, ETag: String; SoftDeleted, IsLeased: Boolean; LeaseState: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAzureblob: TFormAzureblob;

implementation

{$R *.dfm}

procedure TFormAzureblob.bGoClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;
    csAzureblob1.Account := tAccount.Text;
    csAzureblob1.AccessKey := tAccessKey.Text;

    lvwContainers.Items.Clear();
    csAzureblob1.ListContainers;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormAzureblob.bNewContainerClick(Sender: TObject);
var newContainerName:string;
begin
  if InputQuery('Enter Container Name', 'Container Name?',
                newContainerName) then
  begin
    try
      Screen.Cursor := crHourGlass;
      csAzureblob1.Container := newContainerName;
      csAzureblob1.CreateContainer();
      lvwContainers.Items.Clear();
      csAzureblob1.ListContainers();

      gbBlobs.Caption := 'Blobs in container ' + csAzureblob1.Container;
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAzureblob.bDeleteContainerClick(Sender: TObject);
begin
  if lvwContainers.SelCount = 0 then
  begin
    ShowMessage('Select a bucket first!');
    exit;
  end;

  if MessageDlg('Delete container ' + csAzureblob1.Container + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      csAzureblob1.DeleteContainer();
      lvwContainers.Items.Clear();
      csAzureblob1.ListContainers();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAzureblob.bDeleteBlobClick(Sender: TObject);
begin
  if lvwBlobs.SelCount = 0 then
  begin
    ShowMessage('Select a blob first!');
    exit;
  end;

  if MessageDlg('Delete blob ' + lvwBlobs.Selected.Caption + '?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    try
      Screen.Cursor := crHourGlass;
      csAzureblob1.DeleteBlob(lvwBlobs.Selected.Caption, 2);
      lvwBlobs.Items.Clear();
      csAzureblob1.ListBlobs();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAzureblob.bGetBlobClick(Sender: TObject);
begin
  if SaveDialog1.Execute = true then
  begin
    try
      Screen.Cursor := crHourGlass;
      csAzureblob1.LocalFile := SaveDialog1.FileName;
      csAzureblob1.GetBlob(lvwBlobs.Selected.Caption);
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAzureblob.bNewBlobClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      Screen.Cursor := crHourGlass;
      csAzureblob1.LocalFile := OpenDialog1.FileName;
      csAzureblob1.CreateBlob(ExtractFileName(OpenDialog1.FileName), 0, -1);

      lvwBlobs.Items.Clear();
      csAzureblob1.ListBlobs();
    except on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
    end;
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormAzureblob.lvwContainersSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lvwContainers.SelCount = 0 then exit;

  try
    Screen.Cursor := crHourGlass;
    csAzureblob1.Container := lvwContainers.Selected.Caption;

    lvwBlobs.Items.Clear();
    csAzureblob1.ListBlobs();
    gbBlobs.Caption := 'Blobs in container ' + lvwContainers.Selected.Caption;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormAzureblob.csAzureblob1BlobList(Sender: TObject; const Name, Container: String;
  BlobType: Integer; const Snapshot: String; ContentLength: Int64; const ContentType, CreatedTime,
  ModifiedTime, ETag: String; SoftDeleted, IsLeased: Boolean; LeaseState: Integer);
begin
  lvwBlobs.Items.Add();
  lvwBlobs.Items.Item[lvwBlobs.Items.Count - 1].Caption := Name;
  lvwBlobs.Items.Item[lvwBlobs.Items.Count - 1].SubItems.Add(ModifiedTime);
  lvwBlobs.Items.Item[lvwBlobs.Items.Count - 1].SubItems.Add(IntToStr(ContentLength));
end;

procedure TFormAzureblob.csAzureblob1ContainerList(Sender: TObject; const Name,
  ModifiedTime, ETag: String; IsLeased: Boolean; LeaseState: Integer);
begin
  lvwContainers.Items.Add();
  lvwContainers.Items.Item[lvwContainers.Items.Count - 1].Caption := Name;
  lvwContainers.Items.Item[lvwContainers.Items.Count - 1].SubItems.Add(ModifiedTime);
end;

end.

