/*
 * Cloud Storage 2024 Java Edition - Sample Project
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

import java.io.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import cloudstorage.*;

public class backblazeb2 extends ConsoleDemo {
  public backblazeb2() {
    BackblazeB2 backblazeb21 = new BackblazeB2();
    String bucketName = null, bucketId = null;
    String buffer = "";
    try {
      backblazeb21.addBackblazeB2EventListener(new B2Events());
      System.out.print("Backblaze B2 Application Key Id: ");
      backblazeb21.setApplicationKeyId(input());
      System.out.print("Backblaze B2 Application Key: ");
      backblazeb21.setApplicationKey(input());
      System.out.println("\nBucket List:\n");
      backblazeb21.listBuckets();
      printMenu();
      while (!buffer.equals("q")) {
        System.out.print("Enter command: ");
        buffer = input();
        if (buffer.toLowerCase().equals("cd")) {
          System.out.print("Enter Bucket Id: ");
          bucketId = input();
          backblazeb21.getBucketInfo(bucketId);
          bucketName = backblazeb21.getBuckets().get(0).getName();
        } else if (buffer.toLowerCase().equals("lb")) {
          backblazeb21.listBuckets();
        } else if (buffer.toLowerCase().equals("lf")) {
          if (bucketId == null) System.out.println("Must select a bucket first!");
          else backblazeb21.listFiles(bucketId);
        } else if (buffer.toLowerCase().equals("get")) {
          if (bucketId == null) System.out.println("Must select a bucket first!");
          else {
            backblazeb21.setLocalFile("");
            System.out.print("Remote file: ");
            backblazeb21.downloadFile(bucketName, input());
            System.out.println("The content of the file:\n" + new String(backblazeb21.getFileData()));
          }
        } else if (buffer.toLowerCase().equals("put")) {
          if (bucketId == null) System.out.println("Must select a bucket first!");
          else {
            String name;
            System.out.print("Name of new file:");
            name = input();
            System.out.print("Local file to upload:");
            backblazeb21.setLocalFile(input());
            backblazeb21.uploadFile(bucketId, name);
            System.out.println("File created.");
          }
        } else if (buffer.toLowerCase().equals("q")) {
          System.exit(0);
        } else if (buffer.toLowerCase().equals("?")) {
          printMenu();
        }
      }
    } catch (CloudStorageException e) {
      System.out.println(e.getMessage());
      System.exit(e.getCode());
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
  }

  private void printMenu() {
    System.out.println("\n?\t-\tHelp\n" +
      "cd\t-\tChange to bucket\n" +
      "lb\t-\tList Buckets\n" +
      "lf\t-\tList Files\n" +
      "get\t-\tGet remoteFile\n" +
      "put\t-\tPut localFile\n" +
      "q\t-\tQuit\n");
  }

  public static void main(String[] args) {
    try {
      new backblazeb2();
    } catch (Exception ex) {
      System.out.println("Exception: " + ex.getMessage());
    }
  }
}

class B2Events extends DefaultBackblazeB2EventListener {
  public void bucketList(BackblazeB2BucketListEvent arg) {
    System.out.println(arg.name + " (" + arg.id + ")");
  }

  public void fileList(BackblazeB2FileListEvent arg) {
    System.out.println(arg.name);
  }

  public void SSLServerAuthentication(BackblazeB2SSLServerAuthenticationEvent arg) {
    arg.accept = true;
  }
}
class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof CloudStorageException) {
      System.out.print(" (" + ((CloudStorageException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



