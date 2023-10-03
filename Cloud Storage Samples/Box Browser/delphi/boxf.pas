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
unit boxf;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, csoauth, cscore, cstypes, csbox;

type
  TFormBox = class(TForm)
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
    csBox1: TcsBox;
    procedure btnListDocumentsClick(Sender: TObject);
    procedure RefreshList();
    function ResolveResourceType(t: TcsboxResourceTypes): String;
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
  FormBox: TFormBox;

implementation

{$R *.dfm}

procedure TFormBox.btnAuthorizeClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    txtAuthorization.Text := '';

    csBox1.OAuth.ClientId := txtClientId.Text;
    csBox1.OAuth.ClientSecret := txtClientSecret.Text;
    csBox1.OAuth.ServerAuthURL := 'https://app.box.com/api/oauth2/authorize';
    csBox1.OAuth.ServerTokenURL := 'https://api.box.com/oauth2/token';
    csBox1.Config('OAuthWebServerPort=' + txtPort.Text);
    csBox1.Authorize();
    txtAuthorization.Text := csBox1.OAuth.AccessToken;

  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormBox.btnDeleteClick(Sender: TObject);
begin
  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    csBox1.DeleteResource(lvwDocuments.Selected.SubItems[1]);

    RefreshList();

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormBox.btnDownloadClick(Sender: TObject);
begin

  if not(Assigned(lvwDocuments.Selected)) then begin
    exit;
  end;

  try
    SaveDialog1.FileName := lvwDocuments.Selected.Caption;
    if SaveDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csBox1.LocalFile := SaveDialog1.FileName;
      csBox1.DownloadFile(lvwDocuments.Selected.SubItems[1]);
    end;

  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormBox.btnListDocumentsClick(Sender: TObject);
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

procedure TFormBox.btnUploadClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute = true then begin
      Screen.Cursor := crHourGlass;
      csBox1.LocalFile := OpenDialog1.FileName;
      csBox1.UploadFile(ExtractFileName(OpenDialog1.FileName), '');
      RefreshList();
    end;
  except on ex: ECloudStorage do
    ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormBox.RefreshList();
var
i : Integer;
begin

  lvwDocuments.Items.Clear();

  csBox1.ListResources('');

  for i := 0 to csBox1.ResourceCount - 1 do begin
    lvwDocuments.Items.Add();
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].Caption := csBox1.ResourceName[i];
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(ResolveResourceType(csBox1.ResourceType[i]));
    lvwDocuments.Items.Item[lvwDocuments.Items.Count - 1].SubItems.Add(csBox1.ResourceId[i]);
  end;
end;

function TFormBox.ResolveResourceType(t: TcsboxResourceTypes): String;
begin
  case t of
    brtFile: Result := 'File';
    brtFolder: Result := 'Folder';
  else
    Result := 'Unknown';
  end;
end;

end.

