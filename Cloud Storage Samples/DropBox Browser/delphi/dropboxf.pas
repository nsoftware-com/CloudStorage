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
unit dropboxf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, csoauth, cscore, cstypes, csdropbox;

type
  TFormDropbox = class(TForm)
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
    Label5: TLabel;
    txtPort: TEdit;
    csDropbox1: TcsDropbox;
    procedure btnListDocumentsClick(Sender: TObject);
    procedure RefreshList();
    function ResolveResourceType(t: TcsdropboxResourceTypes): String;
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
  FormDropbox: TFormDropbox;

implementation

{$R *.dfm}

procedure TFormDropbox.btnAuthorizeClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    txtAuthorization.Text := '';

    csDropbox1.OAuth.ClientId := txtClientId.Text;
    csDropbox1.OAuth.ClientSecret := txtClientSecret.Text;
    csDropbox1.OAuth.ServerAuthURL := 'https://www.dropbox.com/oauth2/authorize';
    csDropbox1.OAuth.ServerTokenURL := 'https://api.dropboxapi.com/oauth2/token';
    csDropbox1.Config('OAuthWebServerPort=' + txtPort.Text);
    csDropbox1.Authorize();
    txtAuthorization.Text := csDropbox1.OAuth.AccessToken;

  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormDropbox.btnDeleteClick(Sender: TObject);
begin
  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;

    csDropBox1.DeleteResource(lvwDocuments.Selected.SubItems[3]);

    RefreshList();

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormDropbox.btnDownloadClick(Sender: TObject);
begin

  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    SaveDialog1.FileName := lvwDocuments.Selected.Caption;
    if SaveDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csDropBox1.LocalFile := SaveDialog1.FileName;
      csDropBox1.DownloadFile(lvwDocuments.Selected.SubItems[3]);
    end;

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormDropbox.btnListDocumentsClick(Sender: TObject);
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

procedure TFormDropbox.btnUploadClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csDropBox1.LocalFile := OpenDialog1.FileName;
      csDropBox1.UploadFile(ExtractFileName(OpenDialog1.FileName));
      RefreshList();
    end;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormDropbox.RefreshList();
var
i : Integer;
begin

  lvwDocuments.Items.Clear();

  csDropBox1.ListResources('');

  for i := 0 to csDropBox1.ResourceCount - 1 do begin

    lvwDocuments.Items.Add();
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].Caption := csDropBox1.ResourceName[i];
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(ResolveResourceType(csDropBox1.ResourceType[i]));
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csDropBox1.ResourceModifiedTime[i]);
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csDropBox1.ResourceId[i]);
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csDropBox1.ResourcePath[i]);
  end;
end;

function TFormDropbox.ResolveResourceType(t: TcsdropboxResourceTypes): String;
begin
  case t of
    drtFile: Result := 'File';
    drtFolder: Result := 'Folder';
  else
    Result := 'Unknown';
  end;
end;

end.

