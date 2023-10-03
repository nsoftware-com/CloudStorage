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

program sharefile;

uses
  Forms,
  createfolderf in 'createfolderf.pas'   {FormCreatefolderf},
  linkdetailsf in 'linkdetailsf.pas'   {FormLinkdetailsf},
  sharefilef in 'sharefilef.pas' {FormSharefile};

begin
  Application.Initialize;

  Application.CreateForm(TFormSharefile, FormSharefile);
  Application.CreateForm(TFormCreatefolder, FormCreatefolder);

  Application.CreateForm(TFormLinkdetails, FormLinkdetails);

  Application.Run;
end.


         
