unit linkdetailsf;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormLinkDetails = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    editLink: TEdit;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLinkDetails: TFormLinkDetails;

implementation

{$R *.dfm}

procedure TFormLinkDetails.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


end.
