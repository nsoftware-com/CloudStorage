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
unit googledrivef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, csoauth, cscore, cstypes, csgoogledrive;

type
  TFormGoogledrive = class(TForm)
    GroupBox2: TGroupBox;
    lvwDocuments: TListView;
    btnListDocuments: TButton;
    btnUpload: TButton;
    btnDownload: TButton;
    btnDelete: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    txtClientId: TEdit;
    txtClientSecret: TEdit;
    btnAuthorize: TButton;
    GroupBox5: TGroupBox;
    Label4: TLabel;
    txtAuthorization: TEdit;
    csGoogleDrive1: TcsGoogleDrive;
    procedure btnListDocumentsClick(Sender: TObject);
    procedure RefreshList();
    procedure btnUploadClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnAuthorizeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGoogledrive: TFormGoogledrive;

implementation

{$R *.dfm}

procedure TFormGoogledrive.btnAuthorizeClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    txtAuthorization.Text := '';

    csGoogleDrive1.OAuth.ClientId := txtClientId.Text;
    csGoogleDrive1.OAuth.ClientSecret := txtClientSecret.Text;
    csGoogleDrive1.OAuth.ServerAuthURL := 'https://accounts.google.com/o/oauth2/auth';
    csGoogleDrive1.OAuth.ServerTokenURL := 'https://accounts.google.com/o/oauth2/token';
    csGoogleDrive1.OAuth.AuthorizationScope := 'https://www.googleapis.com/auth/drive';
    csGoogleDrive1.Authorize();
    txtAuthorization.Text := csGoogleDrive1.OAuth.AccessToken;

  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormGoogledrive.btnDeleteClick(Sender: TObject);
begin
  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    csGoogleDrive1.DeleteResource(lvwDocuments.Selected.SubItems[2]);

    RefreshList();

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormGoogledrive.btnDownloadClick(Sender: TObject);
begin

  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    SaveDialog1.FileName := lvwDocuments.Selected.Caption;
    if SaveDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csGoogleDrive1.LocalFile := SaveDialog1.FileName;
      csGoogleDrive1.DownloadFile(lvwDocuments.Selected.SubItems[2],'');
    end;

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormGoogledrive.btnListDocumentsClick(Sender: TObject);
begin
  try
    Screen.Cursor := crHourGlass;

    RefreshList();

    btnDownload.Enabled := True;
    btnUpload.Enabled := True;
    btnDelete.Enabled := True;

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormGoogledrive.btnUploadClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csGoogleDrive1.LocalFile := OpenDialog1.FileName;
      csGoogleDrive1.UploadFile(ExtractFileName(OpenDialog1.FileName), '');
      RefreshList();
    end;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormGoogledrive.RefreshList();
var
i : Integer;
begin

  lvwDocuments.Items.Clear();

  csGoogleDrive1.ListResources();

  for i := 0 to csGoogleDrive1.ResourceCount - 1 do begin

    lvwDocuments.Items.Add();
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].Caption := csGoogleDrive1.ResourceName[i];
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csGoogleDrive1.ResourceOwner[i]);
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csGoogleDrive1.ResourceModifiedTime[i]);
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csGoogleDrive1.ResourceId[i]);
  end;
end;

end.

