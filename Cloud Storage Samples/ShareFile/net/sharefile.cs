/*
 * Cloud Storage 2024 .NET Edition - Sample Project
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
 * 
 */

﻿using System;
using System.IO;
using nsoftware.CloudStorage;

class sharefileDemo
{
  private static ShareFile sharefile = new nsoftware.CloudStorage.ShareFile();
  private static OAuth oauth = new nsoftware.CloudStorage.OAuth();
  private static string currentFolderId = "";
  private static string subdomain = "";

  static void Main(string[] args)
  {
    /* NOTE: ShareFile requires a registered redirect URI which is https://
     *       and explicitly disallows localhost. A valid public domain with SSL enabled must
     *       be specified when creating your ClientId and ClientSecret values.
     *       
     *       Visit https://api.sharefile.com/apikeys to create your credentials.
     *       
     * IMPORTANT: 
     *      Because of the ShareFile restrictions detailed above, this demo
     *      makes use of an existing public URL (oauth.nsoftware.com).
     *      This should NOT be used in production. It is only used here to
     *      facilitate running of this demo. In a real application, you should
     *      register your own Redirect URI.
     *      
     *      For any questions, please contact support@nsoftware.com.
     */

    try
    {
      // Prompt for authentication information.
      Console.Write("Enter your account subdomain: ");
      subdomain = Console.ReadLine();
      oauth.ClientProfile = OAuthClientProfiles.ocpApplication;
      Console.Write("Enter your OAuth client ID: "); // Can use Hb7kR4d6mBvQxnOe9Eg6V8CMIVjQ67vD for testing purposes.
      oauth.ClientId = Console.ReadLine();
      Console.Write("Enter your OAuth client secret: "); // Can use jHAZH6f1Ac5O9krW9az3fLYFEZW4KoF1sr1hqDoVgrvv1zwc for testing purposes.
      oauth.ClientSecret = Console.ReadLine();

      oauth.ServerAuthURL = "https://secure.sharefile.com/oauth/authorize";
      oauth.ServerTokenURL = "https://" + subdomain + ".sharefile.com/oauth/token";

      oauth.WebServerPort = 7878; // Specify a particular port, as it will be used in the redirect.
      oauth.StartWebServer(); // In most cases, the webserver is automatically started when GetAuthorization is called. In this case, due to the use of oauth.nsoftware.com, it is started manually.
      oauth.ReturnURL = "https://oauth.nsoftware.com/oauthdemo"; // This URL should NOT be used in a production application. It is only for testing purposes.
      oauth.AddParam("state", "http://localhost:" + oauth.WebServerPort); // The oauth.nsoftware.com server uses this value to redirect to the local server embedded within the OAuth class.

      // oauth.RefreshToken = ""; // Set to value from previous runs to expedite the login process.
      sharefile.Authorization = oauth.GetAuthorization();
      // Console.WriteLine(oauth.RefreshToken); // RefreshToken may be used to automatically obtain a new token without user interaction.

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("sharefile> ");
      string command;
      string[] arguments;

      Console.WriteLine("Home Directory Listings: ");
      ListItems("home");

      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  ls                           list items in the current folder");
          Console.WriteLine("  cd <resource number>         change to the folder with the specified number from the list");
          Console.WriteLine("  get <resource number>        download the item with the specified number from the list");
          Console.WriteLine("  put <file>                   upload a new file");
          Console.WriteLine("  del <resource number>        delete the item with the specified number from the list");
          Console.WriteLine("  mkdir <name>                 make a new folder");
          Console.WriteLine("  share <resource number>      share the item with the specified number from the list");
          Console.WriteLine("  request                      create a new request link");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0] == "ls")
        {
          ListItems(currentFolderId);
        }
        else if (arguments[0] == "cd")
        {
          if (arguments.Length > 1)
          {
            int folderSelection = int.Parse(arguments[1]);
            if (folderSelection >= 0)
            {
              ListItems(sharefile.Items[folderSelection].Id);
            }
            else
            {
              sharefile.GetItemInfo(currentFolderId);
              ListItems(sharefile.Items[0].ParentId);
            }
          }
        }
        else if (arguments[0] == "get")
        {
          if (arguments.Length > 1)
          {
            ShareFileItem downloadSelection = sharefile.Items[int.Parse(arguments[1])];
            sharefile.LocalFile = downloadSelection.Name;
            sharefile.DownloadFile(downloadSelection.Id);

            Console.WriteLine("Download complete.");
          }
        }
        else if (arguments[0] == "put")
        {
          if (arguments.Length > 1)
          {
            sharefile.LocalFile = arguments[1];
            FileInfo file = new FileInfo(sharefile.LocalFile);
            sharefile.UploadFile(file.Name, currentFolderId);

            Console.WriteLine("Upload complete.");
          }
        }
        else if (arguments[0] == "del")
        {
          if (arguments.Length > 1)
          {
            ShareFileItem deleteSelection = sharefile.Items[int.Parse(arguments[1])];
            sharefile.DeleteItem(deleteSelection.Id);

            Console.WriteLine("Deletion complete.");
          }
        }
        else if (arguments[0] == "mkdir")
        {
          if (arguments.Length > 1)
          {
            sharefile.CreateFolder(arguments[1], currentFolderId);
          }
        }
        else if (arguments[0] == "share")
        {
          if (arguments.Length > 1)
          {
            ShareFileItem shareSelection = sharefile.Items[int.Parse(arguments[1])];
            Console.WriteLine("Share Link: " + sharefile.CreateLink(shareSelection.Id));
          }
        }
        else if (arguments[0] == "request")
        {
          sharefile.CreateRequestLink(currentFolderId);
        }
        else if (arguments[0] == "quit")
        {
          break;
        }
        else if (arguments[0] == "")
        {
          // Do nothing.
        }
        else
        {
          Console.WriteLine("Invalid command.");
        } // End of command checking.

        Console.Write("sharefile> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void ListItems(string folderId)
  {
    try
    {
      sharefile.AccountSubdomain = subdomain;
      sharefile.GetItemInfo(folderId);
      currentFolderId = sharefile.Items[0].Id;

      Console.WriteLine();
      Console.WriteLine("{0,-3}{1,-40}{2,-20}{3,-30}", "#", "Title", "Type", "Creation Date");
      Console.WriteLine("--------------------------------------------------------------------------------");

      sharefile.ListItems(currentFolderId);
      Console.WriteLine("{0,-3}{1,-20}{2,-30}{3,-40}", "-1", "..", "Folder", "");

      for (int i = 0; i < sharefile.Items.Count; i++)
      {
        ShareFileItem currentItem = sharefile.Items[i];
        Console.WriteLine("{0,-3}{1,-20}{2,-30}{3,-40}", i, sharefile.Items[i].Name, ResolveResourceType(sharefile.Items[i].Type), sharefile.Items[i].CreationDate);
      }
      Console.WriteLine();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static string ResolveResourceType(ShareFileItemTypes type)
  {
    switch (type)
    {
      case ShareFileItemTypes.sfitFile:
        return "File";
      case ShareFileItemTypes.sfitFolder:
        return "Folder";
      default:
        return "Unknown";
    }
  }
}




class ConsoleDemo
{
  /// <summary>
  /// Takes a list of switch arguments or name-value arguments and turns it into a dictionary.
  /// </summary>
  public static System.Collections.Generic.Dictionary<string, string> ParseArgs(string[] args)
  {
    System.Collections.Generic.Dictionary<string, string> dict = new System.Collections.Generic.Dictionary<string, string>();

    for (int i = 0; i < args.Length; i++)
    {
      // Add a key to the dictionary for each argument.
      if (args[i].StartsWith("/"))
      {
        // If the next argument does NOT start with a "/", then it is a value.
        if (i + 1 < args.Length && !args[i + 1].StartsWith("/"))
        {
          // Save the value and skip the next entry in the list of arguments.
          dict.Add(args[i].ToLower().TrimStart('/'), args[i + 1]);
          i++;
        }
        else
        {
          // If the next argument starts with a "/", then we assume the current one is a switch.
          dict.Add(args[i].ToLower().TrimStart('/'), "");
        }
      }
      else
      {
        // If the argument does not start with a "/", store the argument based on the index.
        dict.Add(i.ToString(), args[i].ToLower());
      }
    }
    return dict;
  }
  /// <summary>
  /// Asks for user input interactively and returns the string response.
  /// </summary>
  public static string Prompt(string prompt, string defaultVal)
  {
    Console.Write(prompt + (defaultVal.Length > 0 ? " [" + defaultVal + "]": "") + ": ");
    string val = Console.ReadLine();
    if (val.Length == 0) val = defaultVal;
    return val;
  }
}