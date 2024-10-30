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
import cloudstorage.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class azureblob {

	public azureblob() {

		AzureBlob azureblob1 = new AzureBlob();
		String buffer = "";
		try {
			azureblob1.addAzureBlobEventListener(new AzureBlobEvents());
			System.out.print("Azure Account: ");
			azureblob1.setAccount(input());
			System.out.print("Access Key: ");
			azureblob1.setAccessKey(input());
			System.out.println("\nContainer List:\n");
			azureblob1.listContainers();
			printMenu();
			while (!buffer.equals("q")) {
				System.out.print("Enter command: ");
				buffer = input();
				if (buffer.toLowerCase().startsWith("cd")) {
					System.out.print("Container name: ");
					buffer = input();
					azureblob1.setContainer(buffer);
				} else if (buffer.toLowerCase().startsWith("ls")) {
					azureblob1.listBlobs();
				} else if (buffer.toLowerCase().startsWith("get")) {
					azureblob1.setLocalFile("");
					System.out.print("Blob name: ");
					azureblob1.getBlob(input());
					System.out.println("Contents of the file:\n"
							+ new String(azureblob1.getBlobData()));
				} else if (buffer.toLowerCase().startsWith("put")) {
					String rem_object;
					System.out.print("Name of new blob: ");
					rem_object = input();
					System.out.print("Local file to upload: ");
					azureblob1.setLocalFile(input());
					azureblob1.createBlob(rem_object, 0, -1);
					System.out.println("Blob created.");
				} else if (buffer.toLowerCase().equals("q")) {
					System.exit(0);
				} else if (buffer.toLowerCase().equals("?")) {
					printMenu();
				}
			}
		} catch (CloudStorageException e) {
			System.out.println(e.getMessage());
			System.exit(e.getCode());
			return;
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}

	}

	private String input() throws IOException {
		BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));
		return bf.readLine();
	}

	private void printMenu() {
		System.out.println("\n?\t-\tHelp\n" + "cd\t-\tChange to container\n"
				+ "ls\t-\tList Blobs\n" + "get\t-\tGet remoteFile\n"
				+ "put\t-\tPut localFile\n" + "q\t-\tQuit\n");
	}

	public static void main(String[] args) {
		try {
			new azureblob();
		} catch (Exception ex) {
			System.out.println("Exception: " + ex.getMessage());
		}
	}

}

class AzureBlobEvents extends DefaultAzureBlobEventListener {
	public void blobList(AzureBlobBlobListEvent arg) {
		System.out.println(arg.name);
	}
	public void containerList(AzureBlobContainerListEvent arg) {
		System.out.println(arg.name);
	}
	public void SSLServerAuthentication(AzureBlobSSLServerAuthenticationEvent arg) {
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



