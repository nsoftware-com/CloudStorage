(*
 * Cloud Storage 2022 Delphi Edition - Sample Project
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
unit onedrivef;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, csoauth, cscore, cstypes, csonedrive;

type
  TFormOnedrive = class(TForm)
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
    csOneDrive1: TcsOneDrive;
    procedure btnListDocumentsClick(Sender: TObject);
    procedure RefreshList();
    function ResolveResourceType(t: TcsonedriveResourceTypes): String;
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
  FormOnedrive: TFormOnedrive;

implementation

{$R *.dfm}

procedure TFormOnedrive.btnAuthorizeClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    txtAuthorization.Text := '';

    csOneDrive1.OAuth.ClientId := txtClientId.Text;
    csOneDrive1.OAuth.ClientSecret := txtClientSecret.Text;
    csOneDrive1.OAuth.ServerAuthURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/authorize';
    csOneDrive1.OAuth.ServerTokenURL := 'https://login.microsoftonline.com/common/oauth2/v2.0/token';
    csOneDrive1.Config('OAuthWebServerPort=7777'); // http://localhost:7777 is a registered redirect_url for the app
    csOneDrive1.OAuth.AuthorizationScope := 'offline_access files.readwrite files.readwrite.all';
    csOneDrive1.Authorize();
    txtAuthorization.Text := csOneDrive1.OAuth.AccessToken;

  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormOnedrive.btnDeleteClick(Sender: TObject);
begin
  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    csOneDrive1.RemoteId:=lvwDocuments.Selected.SubItems[2];
    csOneDrive1.DeleteResource();
    csOneDrive1.RemoteId:='';

    RefreshList();

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormOnedrive.btnDownloadClick(Sender: TObject);
begin

  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    SaveDialog1.FileName := lvwDocuments.Selected.Caption;
    if SaveDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csOneDrive1.LocalFile := SaveDialog1.FileName;
      csOneDrive1.RemoteId:=lvwDocuments.Selected.SubItems[2];
      csOneDrive1.DownloadFile();
    end;

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormOnedrive.btnListDocumentsClick(Sender: TObject);
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

procedure TFormOnedrive.btnUploadClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csOneDrive1.LocalFile := OpenDialog1.FileName;
      csOneDrive1.UploadFile(ExtractFileName(OpenDialog1.FileName));
      RefreshList();
    end;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormOnedrive.RefreshList();
var
i : Integer;
begin

  lvwDocuments.Items.Clear();

  csOneDrive1.ListResources();

  for i := 0 to csOneDrive1.ResourceCount - 1 do begin

    lvwDocuments.Items.Add();
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].Caption := csOneDrive1.ResourceName[i];
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(ResolveResourceType(csOneDrive1.ResourceType[i]));
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csOneDrive1.ResourceModifiedTime[i]);
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csOneDrive1.ResourceId[i]);
  end;
end;

function TFormOnedrive.ResolveResourceType(t: TcsonedriveResourceTypes): String;
begin
  case t of
    odrtFile: Result := 'File';
    odrtFolder: Result := 'Folder';
  else
    Result := 'Unknown';
  end;
end;

end.


