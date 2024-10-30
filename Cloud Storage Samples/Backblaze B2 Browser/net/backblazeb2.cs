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
using System.Linq;
public class BackblazeB2Demo
{
  // Declare private static fields for BackblazeB2 object and currentBucket string.
  private static BackblazeB2 backblazeb2;

  // Define Main function as an asynchronous task.
  static void Main(string[] args)
  {
    try
    {
      //Prompt the user to enter application key ID and the corresponding application key.
      Console.Write("Backblaze B2 Application Key Id: ");
      var keyID = Console.ReadLine();

      Console.Write("Backblaze B2 Application Key: ");
      var key = Console.ReadLine();

      // Initialize the BackblazeB2 object and set application key and its ID .
      backblazeb2 = new BackblazeB2();
      backblazeb2.ApplicationKeyId = keyID;
      backblazeb2.ApplicationKey = key;


      // Print bucket list title and then list all the buckets.
      Console.WriteLine("\nBucket List:\n");
      backblazeb2.ListBuckets();
      foreach (var bucket in backblazeb2.Buckets)
      {
        Console.WriteLine($"Name: {bucket.Name}\t ID: {bucket.Id}");
      }

      // Print the menu options to the console.
      PrintMenu();

      // Loop until user enters "q" for quit.
      string buffer = "";
      string bucketID = "";
      while (!buffer.Equals("q", StringComparison.InvariantCultureIgnoreCase))
      {
        Console.Write("Enter command: ");
        buffer = Console.ReadLine();

        // Switch statement to handle different commands entered by the user.
        switch (buffer.ToLower().Trim())
        {
          case "cd":
            // Change the current bucket.
            Console.Write("Which bucket?: ");
            bucketID = Console.ReadLine();
            Console.WriteLine($"Current bucket ID is now {bucketID}.");
            break;

          case "ls":
            // List all files in the current bucket.
            try
            {
              backblazeb2.ListFiles(bucketID);
            }
            catch (CloudStorageException ex)
            {
              Console.WriteLine($"{ex.Message}");
              break;
            }
            foreach (var entry in backblazeb2.Files)
            {
              Console.WriteLine(entry.Name);
            }
            break;

          case "get":
            // Get a file from the current bucket.
            Console.Write("Which file: ");
            var fileName = Console.ReadLine();
            backblazeb2.GetBucketInfo(bucketID);
            var bucketInfo = backblazeb2.Buckets.FirstOrDefault(x => x.Id == bucketID);
            backblazeb2.DownloadFile(bucketInfo.Name, fileName);
            Console.WriteLine("The content of the file:\n=========================\n" + backblazeb2.FileData + "\n=========================\n");
            break;

          case "put":
            // Upload a local file to the current bucket as a new file.
            Console.Write("Name of new object: ");
            var newObject = Console.ReadLine();
            Console.Write("Local file to upload: ");
            backblazeb2.LocalFile = Console.ReadLine();
            backblazeb2.UploadFile(bucketID, newObject);
            Console.WriteLine("Object created.");
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
        "cd\t-\tChange to bucket\n" +
        "ls\t-\tList Objects\n" +
        "get\t-\tGet remoteFile\n" +
        "put\t-\tPut localFile\n" +
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