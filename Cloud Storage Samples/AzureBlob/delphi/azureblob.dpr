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

program azureblob;

uses
  Forms,
  azureblobf in 'azureblobf.pas' {FormAzureblob};

begin
  Application.Initialize;

  Application.CreateForm(TFormAzureblob, FormAzureblob);
  Application.Run;
end.


         
