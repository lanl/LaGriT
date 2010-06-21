<?php

// Written by Brian Lain <blain@lanl.gov>
//            EES Comptuer Support Team
// 2/11/2004

 
// Address to send mail to.
$email_address = "lagrit-reg@lanl.gov";
// $email_address = "blain@lanl.gov";
$email_subject = "LaGriT Registration";
$email_from = "lagrit-reg@lanl.gov";
$headers = "From: LaGriT_Registration<$email_from>\n";
$headers .= "Reply-To: LaGriT_Registration<$email_from>\n";

// is_blank() function def.
function is_blank($var)
{
	return trim($var) == '';
}

// Output red text if condition is true.
function red_if($text,$condition)
{
	if($condition)
		return "<font color=\"red\">$text</font>";
	else
		return $text;
}

function Add2DB($email_text)
{
    $db = mysql_connect("localhost","web","web@lanl");
    $email_text = mysql_real_escape_string($email_text,$db);
    $SQL = "INSERT into email_log (email_text) VALUES ('$email_text')";      
    mysql_select_db("lagrit", $db);
    mysql_query($SQL,$db); 
}

?>


<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2 Final//EN">
<html>
<head>

<title>LaGriT Registration Form</title>

<hr>
<h1><center>LaGriT Registration Form</center></h1>
<hr>
</head>
<body BGCOLOR="#FFFFFF">


<?php

if($_POST &&
   !is_blank(trim($_POST["First_Name"])) && 
   !is_blank(trim($_POST["Last_Name"])) &&
   !is_blank(trim($_POST["email_address"]))  &&
   !is_blank($_POST["Organization"]) &&
   !is_blank($_POST["Organization_Address"]) &&
   !is_blank($_POST["Organization_Type"]) &&
   !is_blank($_POST["Operating_System"]) &&
   !is_blank($_POST["Description"]) &&
   !is_blank($_POST["Phone"]))
{

	$date = date("F j, Y, g:i a");

	$email_body = "Date: $date \n".
		      "Name: ".$_POST['First_Name']." ".$_POST['Last_Name'] ."\n".
		      "Email Address: ". $_POST['email_address'] ."\n".
		      "Organization: ". $_POST['Organization'] ."\n".
		      "Organization Address: ". str_replace("\n",", ",str_replace("\r","",$_POST['Organization_Address'])) ."\n".
		      "Phone: ". $_POST['Phone'] ."\n".
		      "Fax: ". $_POST['Fax'] ."\n".
		      "Organization Type: ".  $_POST['Organization_Type'] ."\n".
		      "Operating System: ".  $_POST['Operating_System'] ."\n".
		      "OS Detail: ".  $_POST['OS_detail'] ."\n".
		      "Description: ". $_POST['Description'] ."\n";

	print nl2br("<center><h2><font color=\"red\">Your request has been received.<br /><br />Thank You</font></h2></center>");
	Add2DB(strip_tags("TO: $email_address\nSUBJECT: $email_subject\nEXTRA HEADERS: $headers \n\n$email_body"));
        mail($email_address, $email_subject, $email_body, $headers); 

}
else
{
?>

<p>

<?php if($_POST) print "<center><h2><font color=\"red\">A required field is missing.<br>Please fill in all required fields!</font></h2></center>"; ?>

<form action="<?php echo $_SERVER['PHP_SELF']; ?>" method="POST">

<b>*-Required Field</b><p>

<table>
<tr>
	<td align=right><b>Date: </b></td>
	<td><?php print date("F j, Y, g:i a"); ?></td>
</tr>
<tr>

	<td align=right><b>
	<?php print red_if("*First Name:",$_POST && is_blank($_POST['First_Name']));  ?>
	</b></td>
	<td><input type=text size=30 name="First_Name" value="<?php print $_POST['First_Name']; ?>"></td>
</tr>
<tr>
	<td align=right><b>
	<?php print red_if("*Last Name:",$_POST && is_blank($_POST['Last_Name'])); ?>
	</b></td>
	<td><input type=text size=30 name="Last_Name" value="<?php print $_POST['Last_Name']; ?>"></td>
</tr>

<tr>
	<td align=right><b>
	<?php print red_if("*Email Address:",$_POST && is_blank($_POST['email_address'])); ?>
	</b></td>
	<td><input type=text size=30 name="email_address" value="<?php print $_POST['email_address']; ?>">(moniker@server)</td>
</tr>
<tr>
	<td align=right><b>
	<?php print red_if("*Organization:",$_POST && is_blank($_POST['Organization'])); ?>
	</b></td>
	<td><input type=text size=30 name="Organization" value="<?php print $_POST['Organization']; ?>"></td>
</tr>
<tr>
	<td valign=top><b>
	<?php print red_if("*Organization Address:",$_POST && is_blank($_POST['Organization_Address'])); ?>
	</b></td>
	<td><textarea name="Organization_Address" rows=4 cols=40 wrap=soft><?php print $_POST['Organization_Address']; ?></textarea></td>
</tr>
<tr>
	<td align=right><b>
	<?php print red_if("*Phone:",$_POST && is_blank($_POST['Phone'])); ?>
	</b></td>
	<td><input type=text size=30 name="Phone" value="<?php print $_POST['Phone']; ?>"></td>
</tr>
<tr>
	<td align=right><b>Fax:</b></td>
	<td><input type=text size=30 name="Fax" value="<?php print $_POST['Fax']; ?>"></td>
</tr>
</table>
<table>
<tr>
	<td><b>
	<?php print red_if("*Organization Type:",$_POST && is_blank($_POST['Organization_Type'])); ?>
	</b></td>
	<td>
	    <input type="radio" name="Organization_Type" value="University" <?php if($_POST && $_POST['Organization_Type'] == "University") print "checked"; ?>>University
	    <input type="radio" name="Organization_Type" value="US Govt." <?php if($_POST && $_POST['Organization_Type'] == "US Govt.") print "checked"; ?>>US Govt.
	    <input type="radio" name="Organization_Type" value="Commercial" <?php if($_POST && $_POST['Organization_Type'] == "Commercial") print "checked"; ?>>Commercial
	    <input type="radio" name="Organization_Type" value="Other" <?php if($_POST && $_POST['Organization_Type'] == "Other") print "checked";?> >Other
	</td>
</tr>
</table>
<table>
<tr>
        <td><b>
        <?php print red_if("*Operating System:",$_POST && is_blank($_POST['Operating_System'])); ?>
        </b></td>
        <td>
            <input type="radio" name="Operating_System" value="Linux" <?php if($_POST && $_POST['Operating_System'] == "Linux") print "checked"; ?>>Linux
            <input type="radio" name="Operating_System" value="SunOS/Solaris" <?php if($_POST && $_POST['Operating_System'] == "SunOS/Solaris") print "checked"; ?>>Sun OS/Solaris
            <input type="radio" name="Operating_System" value="MacOSX Darwin" <?php if($_POST && $_POST['Operating_System'] == "MacOSX Darwin") print "checked"; ?>>MacOSX Darwin
            <input type="radio" name="Operating_System" value="Other" <?php if($_POST && $_POST['Operating_System'] == "Other") print "checked";?> >Other (describe below)
        </td>

<tr>
	<td align=right><b>
	<td><input type=text size=80 name="OS_detail" value="<?php print $_POST['OS_detail']; ?>"></td>

</tr>
<tr>
	<td align=right valign=top><b>
	<?php print red_if("*Breifly describe<br>how you will be<br>using the code:",$_POST && is_blank($_POST['Description'])); ?>
	</b></td>
	<td><textarea name="Description" rows=4 cols=40 wrap=soft><?php print $_POST['Description']; ?></textarea></td>
</tr>
</table>

<p>
	<center><input type="Submit" name="submit" value="Register for LaGriT">
	<input type="Reset" value="Clear this form"></center>
</form>
</table>



<?php

}

?>


<hr WIDTH=60% Align=CENTER>
<p align=center>
<a HREF="http://www.ees.lanl.gov/"><b>EES Division</b></a> |
<a href="http://www.lanl.gov/">LANL</a> |
<a href="http://www.doe.gov/">DOE</a><br>
<a href="http://www.lanl.gov/Internal/phonebook.html">Phone Book</a> |
<a href="http://www.lanl.gov/cgi-bin/aliweb/aliwebsimple-lanl.pl">Search</a> |

<a href="http://www.lanl.gov/Internal/help.html">Help</a>
</strong>
</p>

<p align=center> <font SIZE="-1">
<strong>
L O S &#160; A L A M O S &#160; N A T I O N A L &#160; L A B O R A T O R Y
</strong>
<br>

<em><type size -1>Operated by the University of California for the US Department of 
Energy</em>
</p>

<address>
<p align=center>

<a HREF="mailto:www@vega.lanl.gov">webmaster</a> - 
<a href="http://www.lanl.gov/Misc/copyright.html">Copyright &#169; UC 2004</a> - 
<a href="http://www.lanl.gov/Misc/disclaimer.html"> Disclaimer</a> -


5 February 2004


</font>
</p>
</address>

</body>
</html>


</html>
