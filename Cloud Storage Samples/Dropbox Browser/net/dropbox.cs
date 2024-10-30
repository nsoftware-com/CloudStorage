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

class dropboxDemo
{
  private static Dropbox dropbox = new nsoftware.CloudStorage.Dropbox();

  static void Main(string[] args)
  {
    try
    {
      dropbox.OnSSLServerAuthentication += dropbox_OnSSLServerAuthentication;

      // Prompt for authentication information.
      Console.Write("Enter your OAuth client ID: ");
      dropbox.OAuth.ClientId = Console.ReadLine();
      Console.Write("Enter your OAuth client secret: ");
      dropbox.OAuth.ClientSecret = Console.ReadLine();

      dropbox.OAuth.ServerAuthURL = "https://www.dropbox.com/oauth2/authorize";
      dropbox.OAuth.ServerTokenURL = "https://api.dropboxapi.com/oauth2/token";

      Console.Write("Enter a redirect URI port: ");
      dropbox.Config("OAuthWebServerPort=" + Console.ReadLine());
      dropbox.Config("OAuthBrowserResponseTimeout=60");
      dropbox.Authorize();

      // Process user commands.
      Console.WriteLine("Type \"?\" or \"help\" for a list of commands.");
      Console.Write("dropbox> ");
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
            DropboxResource resource = dropbox.Resources[int.Parse(arguments[1])];
            dropbox.LocalFile = resource.Name;
            dropbox.DownloadFile(resource.Id); // Use the default file format.

            Console.WriteLine("Download complete.");
          }
        }
        else if (arguments[0] == "put")
        {
          if (arguments.Length > 1)
          {
            dropbox.LocalFile = arguments[1];
            FileInfo file = new FileInfo(dropbox.LocalFile);
            dropbox.UploadFile(file.Name);

            Console.WriteLine("Upload complete.");
          }
        }
        else if (arguments[0] == "del")
        {
          if (arguments.Length > 1)
          {
            DropboxResource resource = dropbox.Resources[int.Parse(arguments[1])];
            dropbox.DeleteResource(resource.Id);

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

        Console.Write("dropbox> ");
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
      dropbox.ListResources("");

      Console.WriteLine();
      Console.WriteLine("{0,-3}{1,-40}{2,-20}{3,-30}", "#", "Title", "Type", "Last Modified");
      Console.WriteLine("--------------------------------------------------------------------------------");

      for (int i = 0; i < dropbox.Resources.Count; i++)
      {
        Console.WriteLine("{0,-3}{1,-40}{2,-20}{3,-30}", i, dropbox.Resources[i].Name, ResolveResourceType(dropbox.Resources[i].Type), dropbox.Resources[i].ModifiedTime);
      }
      Console.WriteLine();
    }
    catch (Exception ex)
    {
      Console.WriteLine(ex.Message);
    }
  }

  private static string ResolveResourceType(DropboxResourceTypes type)
  {
    switch (type)
    {
      case DropboxResourceTypes.drtFile:
        return "File";
      case DropboxResourceTypes.drtFolder:
        return "Folder";
      default:
        return "Unknown";
    }
  }

  private static void dropbox_OnSSLServerAuthentication(object sender, DropboxSSLServerAuthenticationEventArgs e)
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