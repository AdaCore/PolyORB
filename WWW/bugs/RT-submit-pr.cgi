#!/usr/bin/perl -w
####################################################################
#
# Filename: OncallForm.cgi
#
# Author: Kenneth L. Hamer
#
# Purpose: A web form for entering oncall reports into Request Tracker
#
# Use: Like any other CGI
#
# Related files and directories: Creates files of the form OncallForm#
#   where # is the process number the CGI us running as.  It should delete
#   them when done.
#
####################################################################
#
# CVS revision information:
#
# $Name:  $
#
# $Id: //depot/adabroker/main/WWW/bugs/RT-submit-pr.cgi#4 $
#
####################################################################
 
use IO::File;
use Time::localtime;
use CGI::Pretty qw/:standard/; 
use strict;

##
#  This is where group info is set
##
 
$main::rt = '/usr/local/rt/bin/rt';
$main::versions = [ '1.0pre3', '1.0pre2', '1.0pre1', 'other (please specify)' ];
$main::start_prio = 70;
$main::end_prio = 70;

## $main::groups = ['CFS','CSS','DOS','DSM','PCS'];
## %main::queues = ('CFS'=>'DSS-Oncall',
##                  'CSS'=>'general',
##                  'DOS'=>'DOS-Oncall',
##                  'DSM'=>'general',
##                  'PCS'=>'DSS-Oncall');

# Begin processing
 
$main::oh_cgi = new CGI;
$main::oh_cgi->use_named_parameters(1);
 
if ( ! $main::oh_cgi->param() ) {
  &show_form();
}
else {
  &process_form();
}

########################################################################
#
# Subroutine: show_form
#
# Purpose: Display the form
#
# Use: &show_form();
#
# Inputs: Some cookies
#
# Outputs: Sets a bunch of CGI parameters
#
########################################################################
 
sub show_form {
 
  my($s_form,$s_header,$s_footer);
  my($s_group,$s_userid,$s_time,$s_date,$s_inoffice);
  my($s_down,$s_up,$s_resolved,$s_ppn,$s_scn,$s_contact,$s_rtreporter);
  my($s_summary,$s_version,$s_description,$s_resolution,$s_environment);
  my($s_usr,$s_grp);
  my($oh_cgi) = $main::oh_cgi;
 
  my ($bgcolor, $color, $errcolor);
  $bgcolor = '#ffffff';
  $errcolor = 'mistyrose';

  # Write the header
  $s_header = $oh_cgi->header();
  $s_header .= $oh_cgi->start_html(-title=>'AdaBroker problem report',
                      -author=>'quinot@adabroker.eu.org',
                      -BGCOLOR=>$bgcolor,
                      -text=>'#000033',
                      -link=>'#2244cc',
                      -vlink=>'#112288',
                     );
  $s_header .= img { -src=>'/adabroker.jpg',
		-alt=>'AdaBroker',
		-align=>'left',
		-hspace=>'5',
		-vspace=>'5'
	       };
  $s_header .= $oh_cgi->h1 ( {-align=>'right'},
	       'Submit a problem report'
	     );

  $s_header .= br { -clear=>'all' };

  $s_header .= $oh_cgi->hr;

  $s_header .= '<P>Thank you for taking the time to let us know about a
     problem with AdaBroker.  Please fill out the form as completely as
     possible.';

  $s_header .= $oh_cgi->start_multipart_form();
 
##   # The group selection dialog 
##   if ( defined $oh_cgi->cookie({name=>'group'}) ) {
##     # We found the group in a cookie
##     $s_grp = $oh_cgi->cookie({name=>'group'});
##   }
##   else {
##     $s_grp = $main::groups[0];
##   } 
##   $s_group = "Group: ";
##   $s_group .= $oh_cgi->popup_menu({name=>'group',
##                                    values=>$main::groups,
##                                    default=>$s_grp});
##  
##   # The userid field 
##   if ( defined $oh_cgi->user_name() ) {
##     # Then we can pick up the user from the server authentication
##     $s_usr = $oh_cgi->user_name();
##   }
##   elsif ( defined $oh_cgi->cookie({name=>'userid'}) ) {
##     # The we found the userid in a cookie
##     $s_usr = $oh_cgi->cookie({name=>'userid'});
##   }
##   else {
##     $s_usr = "";
##   }
##   $s_userid = "User ID: ";
##   $s_userid .= $oh_cgi->textfield({name=>'userid',
##                                        size=>'4',
##                                        maxlength=>'4',
##                                        default=>$s_usr});
##   # The time field
##   $s_time = "Time: ";
##   $s_time .= $oh_cgi->textfield({name=>'timehour',
##                                  size=>'2',
##                                  maxlength=>'2',
##                                  default=>sprintf("%02d",localtime->hour())});
##  
##   $s_time .= ":";
##   $s_time .= $oh_cgi->textfield({name=>'timemin',
##                                  size=>'2',
##                                  maxlength=>'2',
##                                  default=>sprintf("%02d",localtime->min())});
##   $s_time .= " ";
##   $s_time .= $oh_cgi->popup_menu({name=>'timeampm',
##                                         values=>['24h','AM','PM']});
##  
##   # The date field
##   $s_date = "Date: ";
##   $s_date .= $oh_cgi->textfield({name=>'month',
##                                  size=>'2',
##                                  maxlength=>'2',
##                                  default=>sprintf("%02d",localtime->mon()+1)});
##   $s_date .= "/";
##   $s_date .= $oh_cgi->textfield({name=>'day',
##                                  size=>'2',
##                                  maxlength=>'2',
##                                  default=>sprintf("%02d",localtime->mday())});
##   $s_date .= "/";
##   $s_date .= $oh_cgi->textfield({name=>'year',
##                                  size=>'4',
##                                  maxlength=>'4',
##                                  default=>sprintf("%04d",localtime->year()+1900)});
##  
##   if ( localtime->hour() >= 7 && localtime->hour() <= 17 ) {
##     # We're probably in the office
##     $s_inoffice = $oh_cgi->checkbox({name=>'inoffice',
##                                      value=>'yes',
##                                      checked=>'checked',
##                                      label=>'In Office'});
##   }
##   else {
##     # We're probably out of the office
##     $s_inoffice = $oh_cgi->checkbox({name=>'inoffice',
##                                      value=>'yes',
##                                      label=>'In Office'});
##   }
##  
##   # The time down field
##   $s_down = "Time Down: ";
##   $s_down .= $oh_cgi->textfield({name=>'downhour',
##                                        size=>'2',
##                                        maxlength=>'2'});
##   $s_down .= ":";
##   $s_down .= $oh_cgi->textfield({name=>'downmin',
##                                        size=>'2',
##                                        maxlength=>'2'});
##   $s_down .= " ";
##   $s_down .= $oh_cgi->popup_menu({name=>'downampm',
##                                         values=>['24h','AM','PM']});
##  
##   # The time up field
##   $s_up = "Time Up: ";
##   $s_up .= $oh_cgi->textfield({name=>'uphour',
##                                size=>'2',
##                                maxlength=>'2'});
##   $s_up .= ":";
##   $s_up .= $oh_cgi->textfield({name=>'upmin',
##                                size=>'2',
##                                maxlength=>'2'});
##   $s_up .= " ";
##   $s_up .= $oh_cgi->popup_menu({name=>'upampm',
##                                 values=>['24h','AM','PM']});
##  
##   # The resolved checkbox
##   $s_resolved = $oh_cgi->checkbox({name=>'resolved',
##                                    value=>'yes',
##                                    checked=>'checked',
##                                    label=>'Resolved'});
##  
##   # The PPN, SCN, contacts and systems fields
##   $s_ppn = "PPN(s): ";
##   $s_ppn .= $oh_cgi->textfield({name=>'ppn'});
##  
##   $s_scn = "SCN(s): ";
##   $s_scn .= $oh_cgi->textfield({name=>'scn'});
##  
##   $s_contact = "Contact(s): ";
##   $s_contact .= $oh_cgi->textfield({name=>'contact'});
##  
##   $s_system = "System(s): ";
##   $s_system .= $oh_cgi->textfield({name=>'system'});
 
  # Requester's identity.
  $s_rtreporter = "<B>Your e-mail address:</B><BR>";
  $s_rtreporter .= $oh_cgi->textfield({name=>'reporter',
                                    size=>'72',
                                    maxlength=>'72'});
  $s_rtreporter .= "<P><B>Warning!</B> It is critical that you provide\n"
     . "us with a correct address, since this is the only way for us to\n"
     . "keep you informed of the problem status.\n";

  # The summary, description, resolution, and comments boxes.
  $s_summary = "<B>One-line summary:</B><BR>";
  $s_summary .= $oh_cgi->textfield({name=>'summary',
                                    size=>'72',
                                    maxlength=>'72'});
  
   $s_version = "<B>Version:</B><BR>";
   $s_version .= $oh_cgi->popup_menu({name=>'version',
				      values=>$main::versions,
				      default=>$main::versions[0]});
  
  $s_description = "<B>Full description:</B><BR>";
  $s_description .= $oh_cgi->textarea({name=>'description',
                                       rows=>'10',
                                       columns=>'72',
                                       wrap=>'hard'});
 
  $s_resolution = "<B>Fix or work-around, if known:</B><BR>";
  $s_resolution .= $oh_cgi->textarea({name=>'resolution',
                                      rows=>'5',
                                      columns=>'72',
                                      wrap=>'hard'});
 
  $s_environment = "<B>Environment</B>:<BR>";
  $s_environment .= $oh_cgi->textarea({name=>'environment',
                                      rows=>'5',
                                      columns=>'72',
                                      wrap=>'hard'});
 
  $s_footer .= $oh_cgi->submit({value=>'Submit Report'});
  $s_footer .= $oh_cgi->reset({value=>'Reset Form'});
  $s_footer .= $oh_cgi->endform();
  $s_footer .= $oh_cgi->end_html();
 
  # Now let's wrap it all up in once nice bundle
  $s_form = $s_header;
##   $s_form .= $oh_cgi->table({width=>'100%'},
##     $oh_cgi->Tr([
##       $oh_cgi->td([$s_time,$s_date,$s_inoffice]),
##       $oh_cgi->td([$s_group,$s_userid,''])
##     ])
##   );
##   $s_form .= $oh_cgi->hr();
##   $s_form .= $oh_cgi->table({width=>'100%'},
##     $oh_cgi->Tr([
##       $oh_cgi->td([$s_down,$s_up,$s_resolved]),
##       $oh_cgi->td([$s_ppn,$s_scn,'']),
##       $oh_cgi->td([$s_contact,$s_system,''])
##     ])
##   );
  $s_form .= $oh_cgi->hr();
  $s_form .= $oh_cgi->p($s_rtreporter);
  $s_form .= $oh_cgi->p($s_summary);
  $s_form .= $oh_cgi->p($s_version);
  $s_form .= $oh_cgi->p($s_description);
  $s_form .= $oh_cgi->p($s_environment);
  $s_form .= $oh_cgi->p($s_resolution);
  $s_form .= $oh_cgi->hr();
  $s_form .= $s_footer;
 
  # Spit it out
  STDOUT->print($s_form);
}
 
########################################################################
#
# Subroutine: process_form
#
# Purpose: Process the submitted form data.
#
# Use: &process_form();
#
# Inputs: A bunch of CGI parameters
#
# Outputs: Creates a request in Request Tracker, sets some cookies.
#
########################################################################

sub process_form {
 
  my($s_form,$s_reqnum,$s_status,$s_file,$s_time,$s_date);
  my($s_useridc,$s_groupc);
  my($s_rtqueue,$s_rtarea,$s_rtgiveto,$s_rtsubject,$s_rtbody, $s_rtreporter);
  my(@a_temp);
  my($oh_cgi) = $main::oh_cgi;
 
  # Create cookies to preserve userid and group information across
  # sessions, as a convenience.
  if ( defined $oh_cgi->param('userid') ) {
    $s_useridc = $oh_cgi->cookie({name=>'userid',
                                  value=>($oh_cgi->param('userid')),
                                  expires=>'+7d'});
  }
  if ( defined $oh_cgi->param('group') ) {
    $s_groupc = $oh_cgi->cookie({name=>'group',
                                 value=>($oh_cgi->param('group')),
                                 expires=>'+7d'});
  }
 
  # Put together information to be entered into Request Tracker
 
  #$s_rtqueue = $main::queues{$oh_cgi->param('group')};
  $s_rtqueue = 'submitted';
  $s_rtarea = '';
  #$s_rtgiveto = $oh_cgi->param('userid'); # May barf if user does not exist?
  $s_rtgiveto = '';
                                          # Nope, rt doesn't care.
  $s_rtreporter = $oh_cgi->param('reporter');
  $s_rtsubject = $oh_cgi->param('summary');

##   $a_temp[0] = $oh_cgi->param('timehour');
##   $a_temp[1] = $oh_cgi->param('timemin');
##   if ( $oh_cgi->param('timeampm') eq 'PM' ) {
##     # Convert to 24h time
##     $a_temp[0] += 12;
##   }
##   $s_time = sprintf("%02d:%02d",$a_temp[0],$a_temp[1]);
##   $s_date= sprintf("%02d/%02d/%04d",
##                     $oh_cgi->param('month'),
##                     $oh_cgi->param('day'),
##                     $oh_cgi->param('year'));
##   $s_rtbody .= "Time: $s_time\n";
##   $s_rtbody .= "Date: $s_date\n";
##   if ( defined $oh_cgi->param('inoffice') 
##        && $oh_cgi->param('inoffice') eq 'yes' ) {
##     $a_temp[0] = 'YES';
##   }
##   else {
##     $a_temp[0] = 'NO';
##   }
##   $s_rtbody .= "In Office: $a_temp[0]\n";
##   if ( $oh_cgi->param('downhour') ne '' && $oh_cgi->param('downmin') ne '' ) { 
##     $a_temp[0] = $oh_cgi->param('downhour');
##     $a_temp[1] = $oh_cgi->param('downmin');
##     if ( $oh_cgi->param('downampm') eq 'PM' ) {
##       # Convert to 24h time
##       $a_temp[0] += 12;
##     }
##     $s_rtbody .= sprintf("Time Down: %02d:%02d\n",$a_temp[0],$a_temp[1]);
##   }
##   else {
##     $s_rtbody .= "Time Down: N/A\n";
##   }
##   if ( $oh_cgi->param('uphour') ne '' && $oh_cgi->param('upmin') ne '' ) {
##     $a_temp[0] = $oh_cgi->param('uphour');
##     $a_temp[1] = $oh_cgi->param('upmin');
##     if ( $oh_cgi->param('upampm') eq 'PM' ) {
##       # Convert to 24h time
##       $a_temp[0] += 12;
##     }
##     $s_rtbody .= sprintf("Time Up: %02d:%02d\n",$a_temp[0],$a_temp[1]);
##   }
##   else {
##     $s_rtbody .= "Time Up: N/A\n";
##   } 
##   $s_rtbody .= sprintf("PPN(s): %s\n", $oh_cgi->param('ppn'));
##   $s_rtbody .= sprintf("SCN(s): %s\n", $oh_cgi->param('scn'));
##   $s_rtbody .= sprintf("Contact(s): %s\n", $oh_cgi->param('contact'));
##   $s_rtbody .= sprintf("System(s): %s\n", $oh_cgi->param('system'));
##   $s_rtbody .= "Summary:\n";
##   $s_rtbody .= $oh_cgi->param('summary');
  $s_rtbody .= "Version:\n";
  $s_rtbody .= $oh_cgi->param('version');
  $s_rtbody .= "Description:\n";
  $s_rtbody .= $oh_cgi->param('description');
  $s_rtbody .= "\n\nEnvironment:\n";
  $s_rtbody .= $oh_cgi->param('environment');
  $s_rtbody .= "\n\nResolution:\n";
  $s_rtbody .= $oh_cgi->param('resolution');
  ## $s_rtbody .= "\n\nComments:\n";
  ## $s_rtbody .= $oh_cgi->param('comments');
  $s_rtbody .= "\n";

  $s_status = '';
 
  # Enter the data into request tracker
  $s_file = new IO::File("> /tmp/OncallForm$$");
  if ( ! defined $s_file ) {
    $s_status = "Error: Unable to create temporary file /tmp/OncallForm$$ <BR>";
  }
  else {
    $s_file->print("$s_rtqueue\n");
    $s_file->print("$s_rtarea\n");
    $s_file->print("$s_rtgiveto\n");
    $s_file->print("$s_rtreporter\n");
    if ( $s_rtsubject ne '' ) {
      $s_file->print("$s_rtsubject\n");
    }
    else {
      $s_file->print("$s_rtgiveto $s_time $s_date\n");
    }
    $s_file->print("$main::start_prio\n");
    $s_file->print("$main::end_prio\n");
    $s_file->print("\n"); # Due date (MM/DD/YY)
    $s_file->print("$s_rtbody\n.\n"); 
  }
  $s_file->close();
 
  $s_file = new IO::File("$main::rt -create < /tmp/OncallForm$$ |");
  if ( ! defined $s_file ) {
    $s_status = "Error: Unable to execute $main::rt -create < /tmp/OncallForm$$ <BR>"; 
  }
  else {
    while(<$s_file>) {
      if ( /^Request #(\d+) created/ ) {
        # A request was created
        $s_reqnum = $1;
      }
    }
    if ( ! defined $s_reqnum ) {
      $s_status .= "Error: Did not receive an item number.  Item may not have been created.<BR>\n";
    }
  }
  $s_file->close();
  if ( $s_status eq '' ) { 
    $s_status = "Your report has been entered into queue $s_rtqueue as item $s_reqnum.<BR>\n";
  }
  unlink("/tmp/OncallForm$$"); 
 
  # Resolve the request if appropriate
  if ( defined $oh_cgi->param('resolved') 
       && defined $s_reqnum 
       && $oh_cgi->param('resolved') eq 'yes' ) {
    $s_file = new IO::File("$main::rt -resolve $s_reqnum |");
    if ( ! defined $s_file ) {
      $s_status .= "Error: Unable to execute $main::rt -resolve $s_reqnum.<BR>\n";
    }
    else {
      while(<$s_file>) {
        if ( /^Request #$s_reqnum has been resolved./ ) {
          $s_status .= "Item $s_reqnum has been resolved.<BR>\n";
        }
      }
      $s_file->close();
    } 
  }
 
  # Put together response page, and send it out
  $s_form = $oh_cgi->header({cookie=>[$s_useridc,$s_groupc]});
  $s_form .= $oh_cgi->start_html({title=>'AdaBroker problem report submitted'});
  $s_form .= $s_status;
  $s_form .= $oh_cgi->hr();
  $s_form .= "<PRE>\n";
  $s_form .= $s_rtbody;
  $s_form .= "</PRE>\n";
  $s_form .= $oh_cgi->hr();
  $s_form .= sprintf("<A HREF=\"%s\">Go Back</A>",
                     $oh_cgi->url({relative=>'1'}));
  $s_form .= $oh_cgi->end_html();
 
  STDOUT->print($s_form);
}
 
 
