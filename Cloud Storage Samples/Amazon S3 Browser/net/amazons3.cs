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

public class AmazonS3Demo
{
  // Declare private static fields for AmazonS3 object and currentBucket string.
  private static AmazonS3 amazons3;
  private static string currentBucket;

  // Define Main function as an asynchronous task.
  static void Main(string[] args)
  {
    try
    {
      //Prompt the user to enter AWS access key and secret key.
      Console.Write("AWS Access Key: ");
      var accessKey = Console.ReadLine();

      Console.Write("AWS Secret Key: ");
      var secretKey = Console.ReadLine();

      // Initialize the AmazonS3 object and set its access and secret keys.
      amazons3 = new AmazonS3();
      amazons3.AccessKey = accessKey;
      amazons3.SecretKey = secretKey;


      // Print bucket list title and then list all the buckets.
      Console.WriteLine("\nBucket List:\n");
      amazons3.ListBuckets();
      foreach (var bucket in amazons3.Buckets)
      {
        Console.WriteLine(bucket.Name);
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
            // Change the current bucket.
            Console.Write("Which bucket?: ");
            amazons3.Bucket = Console.ReadLine();
            Console.WriteLine($"Current bucket is now {amazons3.Bucket}.");
            break;

          case "ls":
            // List all objects in the current bucket.
            try
            {
              amazons3.ListObjects();
            }
            catch (CloudStorageException ex)
            {
              Console.WriteLine($"{ex.Message}");
              break;
            }
            foreach (var entry in amazons3.Objects)
            {
              Console.WriteLine(entry.Name);
            }
            break;

          case "get":
            // Get an object (file) from the current bucket.
            Console.Write("Which file: ");
            var key = Console.ReadLine();
            amazons3.GetObject(key);
            Console.WriteLine("The content of the file:\n=========================\n" + amazons3.ObjectData + "\n=========================\n");
            break;

          case "put":
            // Upload a local file to the current bucket as a new object.
            Console.Write("Name of new object: ");
            var newObject = Console.ReadLine();
            Console.Write("Local file to upload: ");
            amazons3.LocalFile = Console.ReadLine();
            amazons3.CreateObject(newObject);
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