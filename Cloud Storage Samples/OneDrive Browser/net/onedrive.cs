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

class onedriveDemo
{
  private static OneDrive onedrive = new nsoftware.CloudStorage.OneDrive();

  static void Main(string[] args)
  {
    try
    {
      onedrive.OnSSLServerAuthentication += onedrive_OnSSLServerAuthentication;

      // Prompt for authentication information.
      Console.Write("Enter your OAuth client ID: ");
      onedrive.OAuth.ClientId = Console.ReadLine();
      Console.Write("Enter your OAuth client secret: ");
      onedrive.OAuth.ClientSecret = Console.ReadLine();

      onedrive.OAuth.ServerAuthURL = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize";
      onedrive.OAuth.ServerTokenURL = "https://login.microsoftonline.com/common/oauth2/v2.0/token";
      onedrive.OAuth.AuthorizationScope = "offline_access files.readwrite files.readwrite.all";

      Console.Write("Enter a redirect URI port: ");
      onedrive.Config("OAuthWebServerPort=" + Console.ReadLine());
      onedrive.Config("OAuthBrowserResponseTimeout=60");
      onedrive.Authorize();

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("onedrive> ");
      string command;
      string[] arguments;

      while (true)
      {
        command = Console.ReadLine();
        arguments = command.Split();

        if (arguments[0] == "?" || arguments[0] == "help")
        {
          Console.WriteLine("Commands: ");
          Console.WriteLine("  ?                            display the list of valid commands");
          Console.WriteLine("  help                         display the list of valid commands");
          Console.WriteLine("  ls                           list resources in the root folder");
          Console.WriteLine("  get <resource number>        download the file resource with the specified number from the list");
          Console.WriteLine("  put <file>                   upload a new file resource");
          Console.WriteLine("  del <resource number>        delete the resource with the specified number from the list");
          Console.WriteLine("  quit                         exit the application");
        }
        else if (arguments[0] == "ls")
        {
          ListDocuments();
        }
        else if (arguments[0] == "get")
        {
          if (arguments.Length > 1)
          {
            OneDriveResource resource = onedrive.Resources[int.Parse(arguments[1])];
            onedrive.LocalFile = resource.Name;
            onedrive.RemoteId = resource.Id;
            onedrive.DownloadFile(); // Use the default file format.

            Console.WriteLine("Download complete.");
          }
        }
        else if (arguments[0] == "put")
        {
          if (arguments.Length > 1)
          {
            onedrive.LocalFile = arguments[1];
            onedrive.RemoteId = "";
            FileInfo file = new FileInfo(onedrive.LocalFile);
            onedrive.UploadFile(file.Name);

            Console.WriteLine("Upload complete.");
          }
        }
        else if (arguments[0] == "del")
        {
          if (arguments.Length > 1)
          {
            OneDriveResource resource = onedrive.Resources[int.Parse(arguments[1])];
            onedrive.RemoteId = resource.Id;
            onedrive.DeleteResource();

            Console.WriteLine("Deletion complete.");
          }
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

        Console.Write("onedrive> ");
      }
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static void ListDocuments()
  {
    try
    {
      onedrive.RemoteId = ""; // Clear the documents collection.
      onedrive.RemotePath = "";
      onedrive.ListResources();

      Console.WriteLine();
      Console.WriteLine("{0,-3}{1,-40}{2,-20}{3,-30}", "#", "Title", "Type", "Last Modified");
      Console.WriteLine("--------------------------------------------------------------------------------");

      for (int i = 0; i < onedrive.Resources.Count; i++)
      {
        Console.WriteLine("{0,-3}{1,-40}{2,-20}{3,-30}", i, onedrive.Resources[i].Name, ResolveResourceType(onedrive.Resources[i].Type), onedrive.Resources[i].ModifiedTime);
      }
      Console.WriteLine();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static string ResolveResourceType(OneDriveResourceTypes type)
  {
    switch (type)
    {
      case OneDriveResourceTypes.odrtFile:
        return "File";
      case OneDriveResourceTypes.odrtFolder:
        return "Folder";
      default:
        return "Unknown";
    }
  }

  private static void onedrive_OnSSLServerAuthentication(object sender, OneDriveSSLServerAuthenticationEventArgs e)
  {
    if (e.Accept) return;
    Console.Write("Server provided the following certificate:\nIssuer: " + e.CertIssuer + "\nSubject: " + e.CertSubject + "\n");
    Console.Write("The following problems have been determined for this certificate: " + e.Status + "\n");
    Console.Write("Would you like to continue anyways? [y/n] ");
    if (Console.Read() == 'y') e.Accept = true;
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