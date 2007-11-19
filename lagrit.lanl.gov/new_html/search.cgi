#!/usr/bin/perl
##############################################################################
# Simple Search                 Version 1.0                                  #
# Copyright 1996 Matt Wright    mattw@worldwidemart.com                      #
# Created 12/16/95              Last Modified 12/16/95                       #
# Scripts Archive at:           http://www.worldwidemart.com/scripts/        #
##############################################################################
# COPYRIGHT NOTICE                                                           #
# Copyright 1996 Matthew M. Wright  All Rights Reserved.                     #
#                                                                            #
# Simple Search may be used and modified free of charge by anyone so long as #
# this copyright notice and the comments above remain intact.  By using this #
# code you agree to indemnify Matthew M. Wright from any liability that      #
# might arise from it's use.                                                 #
#                                                                            #
# Selling the code for this program without prior written consent is         #
# expressly forbidden.  In other words, please ask first before you try and  #
# make money off of my program.                                              #
#                                                                            #
# Obtain permission before redistributing this software over the Internet or #
# in any other medium.  In all cases copyright and header must remain intact.#
##############################################################################
# Define Variables							     #
system("pwd > dirget");
open(in,"dirget");
while(<in>) {
if ($_ ne "/t1www/lagrit") {
$basedir = '/t1www/lagrit/new_html';
$baseurl = 'http://t01www.lanl.gov/lagrit/new_html/';}
else {
$basedir = '/home/webpages/t1/lagrit/new_html';
$baseurl = 'http://lagrit.lanl.gov/new_html/';}
}
@files = ('*.html*');
$title = "LaGrit Manual";
 
 
 
# Done									     #
##############################################################################
 
# Parse Form Search Information
&parse_form;
 
# Get Files To Search Through
&get_files;
 
# Search the files
&search;
 
# Print Results of Search
&return_html;
 
 
sub parse_form {
 
   # Get the input
   read(STDIN, $buffer, $ENV{'CONTENT_LENGTH'});
 
   # Split the name-value pairs
   @pairs = split(/&/, $buffer);
 
   foreach $pair (@pairs) {
      ($name, $value) = split(/=/, $pair);
 
      $value =~ tr/+/ /;
      $value =~ s/%([a-fA-F0-9][a-fA-F0-9])/pack("C", hex($1))/eg;
 
      $FORM{$name} = $value;
   }
}
 
sub get_files {
 
   chdir($basedir);
   foreach $file (@files) {
      $ls = `ls $file`;
      @ls = split(/\s+/,$ls);
      foreach $temp_file (@ls) {
         if (-d $file) {
            $filename = "$file$temp_file";
            if (-T $filename) {
               push(@FILES,$filename);
            }
         }
         elsif (-T $temp_file) {
            push(@FILES,$temp_file);
         }
      }
   }
}
 
sub search {
 
   @terms = split(/\s+/, $FORM{'terms'});
 
   foreach $FILE (@FILES) {
 
      open(FILE,"$FILE");
      @LINES = <FILE>;
      close(FILE);
 
      $string = join(' ',@LINES);
      $string =~ s/\n//g;
      if ($FORM{'boolean'} eq 'AND') {
         foreach $term (@terms) {
            if ($FORM{'case'} eq 'Insensitive') {
               if (!($string =~ /$term/i)) {
                  $include{$FILE} = 'no';
  		  last;
               }
               else {
                  $include{$FILE} = 'yes';
               }
            }
            elsif ($FORM{'case'} eq 'Sensitive') {
               if (!($string =~ /$term/)) {
                  $include{$FILE} = 'no';
                  last;
               }
               else {
                  $include{$FILE} = 'yes';
               }
            }
         }
      }
      elsif ($FORM{'boolean'} eq 'OR') {
         foreach $term (@terms) {
            if ($FORM{'case'} eq 'Insensitive') {
               if ($string =~ /$term/i) {
                  $include{$FILE} = 'yes';
                  last;
               }
               else {
                  $include{$FILE} = 'no';
               }
            }
            elsif ($FORM{'case'} eq 'Sensitive') {
               if ($string =~ /$term/) {
		  $include{$FILE} = 'yes';
                  last;
               }
               else {
                  $include{$FILE} = 'no';
               }
            }
         }
      }
         $titles{$FILE} = "$FILE";
   }
}
 
sub return_html {
   print "Content-type: text/html\n\n";
   print "<html>\n <head>\n  <title>Results of Search</title>\n </head>\n";
   print "<body>\n <center>\n  <h1>Results of Search in $title</h1>\n </center>\n";
   print "Below are the results of your Search in no particular order:<p><hr size=7 width=75%><p>\n";
   print "<ul>\n";
   foreach $key (keys %include) {
      if ($include{$key} eq 'yes') {
         print "<li><a href=\"$baseurl$key\">$titles{$key}</a>\n";
      }
   }
   print "</ul>\n";
   print "<hr size=7 width=75%>\n";
   print "Search Information:<p>\n";
   print "<ul>\n";
   print "<li><b>Terms:</b> ";
   $i = 0;
   foreach $term (@terms) {
      print "$term";
      $i++;
      if (!($i == @terms)) {
         print ", ";
      }
   }
   print "\n";
   print "<li><b>Boolean Used:</b> $FORM{'boolean'}\n";
   print "<li><b>Case $FORM{'case'}</b>\n";
   print "</ul><br><hr size=7 width=75%><P>\n";
   print "</ul>\n";
   print "<hr size=7 width=75%>\n";
   print "</body>\n</html>\n";
}
 
