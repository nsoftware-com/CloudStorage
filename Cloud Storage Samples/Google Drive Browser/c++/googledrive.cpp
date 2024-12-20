/*
 * Cloud Storage 2024 C++ Edition - Sample Project
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
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "../../include/cloudstorage.h"
#define LINE_LEN 100

class MyGoogleDrive : public GoogleDrive
{
public:
	
	virtual int FireSSLServerAuthentication(GoogleDriveSSLServerAuthenticationEventParams *e)
	{
		e->Accept = true;
		return 0;
	}
};

MyGoogleDrive googledrive1;

void printOptions()
{
	printf( "Type ? to select this menu or a number 1-5 to perform one of the following actions:\n"
	        "1.  List Documents\n"
	        "2.  Download Document\n"
	        "3.  Upload Document\n"
	        "4.  Delete Document\n"
	        "5.  Quit\n");
}

void displayDocuments(){

	int ret_code = googledrive1.ListResources();
	if(ret_code)
		goto error;

	printf("%-4.1s%-42.40s%-22.20s%-32.30s\n\n","#","Name","Author","Modified Data");

	for(int i=0;i<googledrive1.GetResourceCount();i++)
	{
		printf("%-4.1d%-42.40s%-22.20s%-32.30s\n",i,googledrive1.GetResourceName(i),googledrive1.GetResourceOwner(i),googledrive1.GetResourceModifiedTime(i));
	}
	return;

error:
	printf("\nError: %d", ret_code);
	if (googledrive1.GetLastError())
	{
		printf( " \"%s\"\n", googledrive1.GetLastError() );
	}
	printf("Press any key to continue");
	fflush(stdin);
	getchar();
	exit(ret_code);
}

int main(int argc, char **argv)
{
  char buffer[LINE_LEN+1];
  int ret_code = 0;

  printf("***************************************************************\n");
  printf("* This demo shows how to use the GoogleDrive component to     *\n");
  printf("* list, download, upload, and delete documents.               *\n");
  printf("***************************************************************\n\n");

  printf("Client ID: ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer)-1] = '\0';
  googledrive1.SetOAuthClientId(buffer);
  
  printf("Client Secret: ");
  fgets(buffer, LINE_LEN, stdin);
  buffer[strlen(buffer)-1] = '\0';
  googledrive1.SetOAuthClientSecret(buffer);


  googledrive1.SetOAuthServerTokenURL("https://accounts.google.com/o/oauth2/token");
  googledrive1.SetOAuthServerAuthURL("https://accounts.google.com/o/oauth2/auth");
  googledrive1.SetOAuthAuthorizationScope("https://www.googleapis.com/auth/drive");

  printf("\nPerforming OAuth authentication ...\n");

  googledrive1.Authorize();

  ret_code = googledrive1.GetLastErrorCode();

  if(ret_code)
    goto error;

	printf("\n");
	printOptions();

	while (1)
	{
		printf( "\n> " );
		scanf("%80s",buffer);

		if ( ! strcmp(buffer, "?") )
		{
			printOptions();
		}

		else if ( ! strcmp(buffer, "1") ) //List documents
		{
			displayDocuments();
		}

		else if ( ! strcmp(buffer, "2") ) //Download document
		{
			printf("\nDocument Number: ");
			scanf("%80s",buffer);
			int docNum = atoi(buffer);

			printf("Download Directory: ");
			scanf("%80s",buffer);
			strcat(buffer,"\\");
			strcat(buffer,googledrive1.GetResourceName(docNum));
			googledrive1.SetLocalFile(buffer);

			ret_code = googledrive1.DownloadFile(googledrive1.GetResourceId(docNum), ""); //Use the default file format
			if(ret_code)
				goto error;

			printf("\n Download Successful\n");
		}

		else if ( ! strcmp(buffer, "3") ) //Upload document
		{
			printf("\nLocal File: ");
			scanf("%80s",buffer);
			googledrive1.SetLocalFile(buffer);
			
			printf("Name of New File on Server: ");
			scanf("%80s",buffer);
      googledrive1.UploadFile(buffer, "");
			ret_code = googledrive1.GetLastErrorCode();

			if(ret_code)
				goto error;

			displayDocuments();
		}

		else if ( ! strcmp(buffer, "4") ) //Delete Document
		{
			printf("\nDocument Number: ");
			scanf("%80s",buffer);
			char *fileToDelete = googledrive1.GetResourceId(atoi(buffer));

			ret_code = googledrive1.DeleteResource(fileToDelete);

			if(ret_code)
				goto error;

			displayDocuments();
		}

		else if ( ! strcmp(buffer, "5") )
		{
			exit(0);
		}

		else if ( ! strcmp(buffer, "") )
		{
			// Do nothing
		}
		else
		{
			printf( "Please select a number from [1-5].\n" );
		} // end of command checking
	}  // end of main while loop
	return ret_code;

error:
	printf("\nError: %d", ret_code);
	if (googledrive1.GetLastErrorCode())
	{
		printf( " \"%s\"\n", googledrive1.GetLastError() );
	}
	printf("Press any key to continue");
	fflush(stdin);
	getchar();
	return ret_code;

}

