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
unit sharefilef;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  createfolderf, linkdetailsf, csoauth, cscore, cstypes, cssharefile;

type
  TFormSharefile = class(TForm)
    Label1: TLabel;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    txtAccountSubdomain: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    txtClientId: TEdit;
    Label3: TLabel;
    txtClientSecret: TEdit;
    Label4: TLabel;
    txtAuthorization: TEdit;
    Label5: TLabel;
    btnAuthorize: TButton;
    Label6: TLabel;
    lstDocuments: TListView;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    btnList: TButton;
    btnUpload: TButton;
    btnDownload: TButton;
    btnDelete: TButton;
    btnRequest: TButton;
    btnCreate: TButton;
    btnShare: TButton;
    csShareFile1: TcsShareFile;
    csOAuth1: TcsOAuth;
    procedure btnAuthorizeClick(Sender: TObject);
    procedure btnListClick(Sender: TObject);
    function ResolveItemType(t: TcssharefileItemTypes): String;
    function Refreshlist(FolderId: String): Integer;
    function AddToList(Name, ItemType, Date, Id: String): Integer;
    procedure btnUploadClick(Sender: TObject);
    procedure btnDownloadClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure lstDocumentsDblClick(Sender: TObject);
    procedure btnRequestClick(Sender: TObject);
    procedure btnShareClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSharefile: TFormSharefile;
  CurrentFolderId: String;

implementation

{$R *.dfm}

procedure TFormSharefile.btnAuthorizeClick(Sender: TObject);
begin
  try
    csShareFile1.Config('LogLevel=3');
    txtAuthorization.Text := '';
    { NOTE: ShareFile requires a registered redirect URI which is https://
      *       and explicitly disallows localhost. A valid public domain with SSL enabled must
      *       be specific when creating your ClientId and ClientSecret values
      *
      *       Visit https://api.sharefile.com/apikeys to create your credentials
      *
      * IMPORTANT:
      *      Because of the ShareFile restrictions detailed above this demo
      *      make use of an existing public URL (oauth.nsoftware.com)
      *      This should NOT be used in production. It is only used here to
      *      facilitate running of this demo. In a real application you should
      *      register your own Redirect URI.
      *
      *      For any questions please contact support@nsoftware.com
    }
    csOAuth1.ClientProfile := TcsTOauthClientProfiles.ocpApplication;;
    csOAuth1.ClientId := txtClientId.Text;
    csOAuth1.ClientSecret := txtClientSecret.Text;
    csOAuth1.ServerAuthURL := 'https://secure.sharefile.com/oauth/authorize';
    csOAuth1.ServerTokenURL := 'https://' + txtAccountSubdomain.Text +
      '.sharefile.com/oauth/token';

    csOAuth1.WebServerPort := 7878;
    // Specify a particular port, it will be used in the redirect.
    csOAuth1.StartWebServer();
    // In most cases the webserver is automatically started when GetAuthorization is called, in this case due to the use of oauth.nsoftware.com it is started manually.
    csOAuth1.ReturnURL := 'https://oauth.nsoftware.com/oauthdemo';
    // This URL should NOT be used in a production application. It is only for testing purposes.
    csOAuth1.AddParam('state', 'http://localhost:' +
      IntToStr(csOAuth1.WebServerPort));
    // The oauth.nsoftware.com server uses this value to redirect to the local server embedded within the OAuth component.

    txtAuthorization.Text := csOAuth1.GetAuthorization();
    // Console.WriteLine(oauth1.RefreshToken); //RefreshToken may be used to automatically obtain a new token without user interaction.
    btnListClick(Sender);

  Except
    on E: Exception do
      ShowMessage(E.ClassName + ' error raised, with message: ' + E.Message);

  end;
end;

procedure TFormSharefile.btnCreateClick(Sender: TObject);
var
  res: TModalResult;
begin
  res := FormCreateFolder.ShowModal();
  if res = mrOk then
  begin
    csShareFile1.CreateFolder(FormCreateFolder.editFolderName.Text,
      CurrentFolderId);
    Refreshlist(CurrentFolderId);
  end;

end;

procedure TFormSharefile.btnDeleteClick(Sender: TObject);
begin
  if not(Assigned(lstDocuments.Selected)) then
  begin
    exit;
  end;

  try
    Screen.Cursor := crHourGlass;

    csShareFile1.DeleteItem(lstDocuments.Selected.SubItems[2]);

    Refreshlist(CurrentFolderId);

  except
    on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormSharefile.btnDownloadClick(Sender: TObject);
begin
  if not(Assigned(lstDocuments.Selected)) then
  begin
    exit;
  end;

  try
    SaveDialog1.FileName := lstDocuments.Selected.Caption;
    if SaveDialog1.Execute = true then
    begin
      Screen.Cursor := crHourGlass;
      csShareFile1.LocalFile := SaveDialog1.FileName;
      csShareFile1.DownloadFile(lstDocuments.Selected.SubItems[2]);
    end;

  except
    on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormSharefile.btnListClick(Sender: TObject);
begin
  csShareFile1.AccountSubdomain := txtAccountSubdomain.Text;
  csShareFile1.Authorization := txtAuthorization.Text;
  Refreshlist('home');

  btnList.Enabled := true;
  btnUpload.Enabled := true;
  btnDownload.Enabled := true;
  btnDelete.Enabled := true;
  btnCreate.Enabled := true;
  btnShare.Enabled := true;
  btnRequest.Enabled := true;
end;

procedure TFormSharefile.btnRequestClick(Sender: TObject);
begin
  FormLinkDetails.editLink.Text := csShareFile1.CreateRequestLink(CurrentFolderId);
  FormLinkDetails.ShowModal();
end;

procedure TFormSharefile.btnShareClick(Sender: TObject);
begin
  if not(Assigned(lstDocuments.Selected)) then
  begin
    exit;
  end;
  FormLinkDetails.editLink.Text := csShareFile1.CreateLink(lstDocuments.Selected.SubItems[2]);
  FormLinkDetails.ShowModal();
end;

procedure TFormSharefile.btnUploadClick(Sender: TObject);
begin
  try
    if OpenDialog1.Execute = true then
    begin
      Screen.Cursor := crHourGlass;
      csShareFile1.LocalFile := OpenDialog1.FileName;
      csShareFile1.UploadFile(ExtractFileName(OpenDialog1.FileName),
        CurrentFolderId);
      Refreshlist(CurrentFolderId);
    end;
  except
    on ex: ECloudStorage do
      ShowMessage('Exception: ' + ex.Message);
  end;
  Screen.Cursor := crDefault;
end;

procedure TFormSharefile.lstDocumentsDblClick(Sender: TObject);
begin
  if not(Assigned(lstDocuments.Selected)) then
  begin
    exit;
  end;
  if (not(lstDocuments.Selected.SubItems[2] = '')) and
    (lstDocuments.Selected.SubItems[0] = 'Folder') then
  begin
    Refreshlist(lstDocuments.Selected.SubItems[2]);
  end;
end;

Function TFormSharefile.Refreshlist(FolderId: string): Integer;
var
  ParentId: String;
  i: Integer;
  currentItem: TcsShareFileItem;
begin
  Screen.Cursor := crHourGlass;
  Try
    lstDocuments.Clear();

    csShareFile1.GetItemInfo(FolderId);
    ParentId := csShareFile1.Items[0].ParentId;
    CurrentFolderId := csShareFile1.Items[0].Id;
    AddToList('..', 'Folder', '', ParentId);

    csShareFile1.ListItems(FolderId);
    for i := 0 to csShareFile1.ItemCount - 1 do
    begin
      currentItem := csShareFile1.Items[i];
      AddToList(currentItem.Name, ResolveItemType(currentItem.ItemType),
        currentItem.CreationDate, currentItem.Id);
    end;

  Except
    on E: ECloudStorage do
      ShowMessage(E.ClassName + ' error raised, with message: ' + E.Message);
  End;

  Result := 0;
  Screen.Cursor := crDefault;
end;

Function TFormSharefile.AddToList(Name, ItemType, Date, Id: String): Integer;
begin
  lstDocuments.Items.Add();
  lstDocuments.Items.Item[lstDocuments.Items.Count - 1].Caption := Name;
  lstDocuments.Items.Item[lstDocuments.Items.Count - 1].SubItems.Add(ItemType);
  lstDocuments.Items.Item[lstDocuments.Items.Count - 1].SubItems.Add(Date);
  lstDocuments.Items.Item[lstDocuments.Items.Count - 1].SubItems.Add(Id);
end;

Function TFormSharefile.ResolveItemType(t: TcssharefileItemTypes): String;
begin
  case t of
    sfitFile:
      Result := 'File';
    sfitFolder:
      Result := 'Folder';
    sfitLink:
      Result := 'Link';
    sfitNote:
      Result := 'Note';
  else
    Result := 'Unknown';
  end;
end;

end.

