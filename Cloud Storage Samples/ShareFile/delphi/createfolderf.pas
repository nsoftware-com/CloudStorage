unit createfolderf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormCreateFolder = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    editFolderName: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCreateFolder: TFormCreateFolder;

implementation

{$R *.dfm}

procedure TFormCreateFolder.btnCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormCreateFolder.btnOkClick(Sender: TObject);
begin
    if Length(editFolderName.Text) > 0 then
    begin
      ModalResult := mrOk;
    end
    else
    begin
      ModalResult :=  mrCancel;
    end;
end;

end.
