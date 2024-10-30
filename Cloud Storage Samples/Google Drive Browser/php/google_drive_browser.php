<?php
/*
 * Cloud Storage 2024 PHP Edition - Sample Project
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
require_once('../include/cloudstorage_googledrive.php');
require_once('../include/cloudstorage_oauth.php');
require_once('../include/cloudstorage_const.php');
?>
<?php
  $googledrive1 = new CloudStorage_GoogleDrive();
?>
<form method=POST>
<center>
<table width="90%">

 <tr><td>OAuth Client Id:        <td><input type=text name=clientId size=40>
 <tr><td>OAuth Client Secret:    <td><input type=password name=clientSecret size=40>

 <tr><td><td><input type=submit value="List Documents">

</table>
</center>
</form>

<?php

// Setup the ReturnURL
if ($_SERVER['HTTPS'] == 'ON') {
  $pageUrl = 'https://';
} else {
  $pageUrl = 'http://';
}
$pageUrl .= $_SERVER['HTTP_HOST'];
$pageUrl .= $_SERVER['PHP_SELF'];

try{
	if ($_SERVER['REQUEST_METHOD'] == "POST") {

		if (array_key_exists("clientId", $_POST))
  	  {
	  	// Add these values to cookies
	  	setcookie('clientId', $_POST['clientId'], time() + 900);  
	  	setcookie('clientSecret', $_POST['clientSecret'], time() + 900);
	  
	  	$googledrive1->setOAuthClientProfile(1);
		$googledrive1->doConfig("OAuthUsePKCE = false");
	  	$googledrive1->setOAuthClientId($_POST['clientId']);
	  	$googledrive1->setOAuthClientSecret($_POST['clientSecret']);
	  	$googledrive1->setOAuthServerAuthURL('https://accounts.google.com/o/oauth2/auth');
	  	$googledrive1->setOAuthServerTokenURL('https://accounts.google.com/o/oauth2/token');
	  	$googledrive1->setOAuthAuthorizationScope('https://www.googleapis.com/auth/drive');
	  	$googledrive1->setOAuthReturnURL($pageUrl);
	  	$url = $googledrive1->getOAuthWebAuthURL();
	  	header('Location: '.$url);
	  	exit();
			} 
	}
	else
	{
		// Redirected from authorization page. Get authorization string and retrieve user information. 
		if (array_key_exists('code', $_GET))
		{
	  	$googledrive1->setOAuthClientProfile(1);
		$googledrive1->doConfig("OAuthUsePKCE = false");
	  	$googledrive1->setOAuthClientId($_COOKIE['clientId']);
	  	$googledrive1->setOAuthClientSecret($_COOKIE['clientSecret']);
	  	$googledrive1->setOAuthServerAuthURL('https://accounts.google.com/o/oauth2/auth');
	  	$googledrive1->setOAuthServerTokenURL('https://accounts.google.com/o/oauth2/token');
	  	$googledrive1->setOAuthAuthorizationScope('https://www.googleapis.com/auth/drive');
	    $googledrive1->setOAuthAuthorizationCode($_GET['code']);
	  	$googledrive1->setOAuthReturnURL($pageUrl);
	  	$googledrive1->doAuthorize();
	  	$googledrive1->doListResources();
?>

<center>
<table width="90%">
  <tr>
    <th>File Name</th>
    <th>Author</th>
    <th>Modification Time</th>
  </tr>

<?php
  		for($i= 0; $i < $googledrive1->getResourceCount(); $i++)
  		{
  			$googledrive1->setResourceId($i, $i);
?>
  <tr>
    <td nowrap><?php echo htmlspecialchars($googledrive1->getResourceName($i)); ?></td>
  	<td nowrap><?php echo htmlspecialchars($googledrive1->getResourceOwner($i)); ?></td>
    <td nowrap><?php echo htmlspecialchars($googledrive1->getResourceModifiedTime($i)); ?></td>
  </tr>

<?php
  		}
  	}  
  }
?>

</table>
</center>

<?php
}
catch (Exception $e) {
	echo 'Error: ',  $e->getMessage(), "\n";
} 

?>

