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

﻿using nsoftware.CloudStorage;
using System;

public class AzureBlobDemo
{
  // Declare private static fields for AzureBlob blob and currentBucket string.
  private static AzureBlob azureblob;
  private static string currentBucket;

  // Define Main function as an asynchronous task.
  static void Main(string[] args)
  {
    try
    {
      //Prompt the user to enter Azure Blob account and access key.
      Console.Write("Azure Account: ");
      var account = Console.ReadLine();

      Console.Write("Access Key: ");
      var accessKey = Console.ReadLine();

      // Initialize the AzureBlob object and set its account and access key.
      azureblob = new AzureBlob();
      azureblob.Account = account;
      azureblob.AccessKey = accessKey;


      // Print container list title and then list all the containers.
      Console.WriteLine("\nContainer List:\n");
      azureblob.ListContainers();
      foreach (var container in azureblob.Containers)
      {
        Console.WriteLine(container.Name);
      }

      // Print the menu options to the console.
      PrintMenu();

      // Loop until user enters "q" for quit.
      string buffer = "";
      while (!buffer.Equals("q", StringComparison.InvariantCultureIgnoreCase))
      {
        Console.Write("Enter command: ");
        buffer = Console.ReadLine();

        // Switch statement to handle different commands entered by the user.
        switch (buffer.ToLower().Trim())
        {
          case "cd":
            // Change the current container.
            Console.Write("Which container?: ");
            azureblob.Container = Console.ReadLine();
            Console.WriteLine($"Current container is now {azureblob.Container}.");
            break;

          case "ls":
            // List all blobs in the current container.
            try
            {
              azureblob.ListBlobs();
            }
            catch (CloudStorageException ex)
            {
              Console.WriteLine($"{ex.Message}");
              break;
            }
            foreach (var entry in azureblob.Blobs)
            {
              Console.WriteLine(entry.Name);
            }
            break;

          case "get":
            // Get an blob(file) from the current container.
            Console.Write("Which file: ");
            var blob = Console.ReadLine();
            azureblob.GetBlob(blob);
            Console.WriteLine("The content of the file:\n=========================\n" + azureblob.BlobData + "\n=========================\n");
            break;

          case "put":
            // Upload a local file to the current container as a new blob.
            Console.Write("Name of new Blob: ");
            var blobName = Console.ReadLine();
            Console.Write("Local file to upload: ");
            azureblob.LocalFile = Console.ReadLine();
            azureblob.CreateBlob(blobName, 0, -1);
            Console.WriteLine("Blob created.");
            break;

          case "q":
            // Exit the program.
            Environment.Exit(0);
            break;

          case "?":
            // Print the menu again.
            PrintMenu();
            break;

          default:
            Console.WriteLine("Invalid command!");
            break;
        }
      }
    }
    // Handle exceptions by printing the exception message to the console.
    catch (Exception e)
    {
      Console.WriteLine(e.Message);
    }
  }

  // Method to print the menu options to the console.
  private static void PrintMenu()
  {
    Console.WriteLine("\n?\t-\tHelp\n" +
        "cd\t-\tChange to Container\n" +
        "ls\t-\tList Blobs\n" +
        "get\t-\tView Blob Data\n" +
        "put\t-\tCreate Blob using local file\n" +
        "q\t-\tQuit\n");
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