#!/usr/local/bin/perl

#  $Id$

use CGI qw( :standard );
use CGI qw( shortcuts );
use CGI::Carp qw(fatalsToBrowser);

$NAME = 'adabroker-wwwgnats';
$VERSION = 'v1.0';
$error_mail = 'gnats-admin';

$query_pr = '/usr/bin/query-pr --restricted -d adabroker';
$file_pr  = '/usr/bin/file-pr -d adabroker';

################################################################
#
#  This script can be called in 3 main ways:
#
#    1) with no params (from the web)
#        a) run make to create new form if needed
#        b) cat the form to the browser
#
#    2) with parameters (from the web)
#        a) get possible lists from query-pr
#        b) check for errors
#        c) if errors, print error message & form
#           else, pront finished form & submit pr
#
#   NOT IMPLEMENTED - form is built with each script invocation
#    3) with parameter mode=makeform (from UNIX command line)
#        a) get possible lists from query-pr
#        b) create a blank form, print to STDOUT
#
################################################################


################################################################
#
#  Main Loop
#
#  branch to function based on path in calling URL
#
################################################################

my $q = new CGI;
  
if ($q->param) {       # script re-entry
    submit_pr ($q);
} else {               # run for the first time
    $q->param('mode', 'make_form');
    submit_pr ($q);
}

exit 0;

################################################################
#
# Define a hash of lists.
# These lists are the possible values for fields in the form.
#
# If values are passed from a form for processing, this list is used
#    to verify that the passed value is valid.
#
# These lists are also used to create scrolling lists on the form
# 
#  $possible{'class'     }[ lists of all class values ]
#  $possible{'categories'}[ lists of all category values ]
#  $possible{'submitter'}[ lists of all submitters values ]
#
################################################################

sub get_possible_values {
    my $q = shift @_;
    $rh_possible = shift @_;

    # Hard coded values
    #
    $$rh_possible{'confidential'} = [ 'yes', 'no' ];
    $$rh_possible{'severity'}     = [ 'non-critical', 'serious', 'critical' ];
    $$rh_possible{'priority'}     = [ 'low', 'medium', 'high' ];

    # Obtained dynamic values from query-pr
    #
    @categories  = map { (split ':', $_)[0] } `$query_pr --list-categories`;
    @classes     = map { (split ':', $_)[0] } `$query_pr --list-classes`;
    @submitters  = map { (split ':', $_)[0] } `$query_pr --list-submitters`;

    #
    # assign filtered values to "possible" hash
    #
    if ($q->server_name() eq 'www-in.juniper.net') {
	$category_filter              = '(pending)';
	$class_filter                 = '(mistaken|duplicate|unreproducible)';
    } else {
	$category_filter              = '((hw|esw|rom)-.*|gnats|pending)';
	$class_filter                 = '(mistaken|duplicate|unreproducible)';
    }

    @f_categories                 = grep !/^$category_filter$/, @categories;
    $$rh_possible{'category'}     = \@f_categories;

    @f_classes                    = grep !/^$class_filter$/, @classes;
    $$rh_possible{'class'}        = \@f_classes;

    push @submitters, 'unknown';
    $$rh_possible{'submitter'}    = \@submitters;

}

################################################################
#
# Define the default values for some of the fields
#
################################################################

sub get_default_values {
    my $q = shift @_;
    my $rh_default = shift @_;

    $$rh_default{'confidential'}  = 'no';
    $$rh_default{'class'}         = 'sw-bug';
    $$rh_default{'submitter'}  = 'net';

}

################################################################
#
# submit_pr actions
#
# 1) get hash of possible values using gnats query-pr command
# 2) if passed values, check values
#     A) if error, define error message
# 3) print form
#
################################################################
#  Variables
#                        (cli NOT IMPLEMENTED
#  $if     'cli'         script called from cli (else assume http server)
#                        
#  $mode   'make_form'   used to make the form only
#          'error'       error detected in processing form
#          'ok'          all checks passed, pr will be e-mailed
#         

sub submit_pr {
    my $q = shift @_;

    my $if = ($q->param('if')) ? $q->param('if') : 'http';
    my $mode = ($q->param('mode')) ? $q->param('mode') : 'ok';
    
    # 1) 
    #
    get_possible_values ($q, \%possible);
    get_default_values ($q, \%default);

    my @fields = ('email',
		  'originator',
		  'organization',
		  'synopsis',
		  'submitter',
		  'confidential',
		  'category',
		  'severity',
		  'priority',
		  'class',
		  'release',
		  );

    my @wide_fields = ('environment',
		       'description',
		       'howtorepeat',
		       'fix'
		       );

    my %field_label;
    foreach (@fields, @wide_fields) {
        $field_label{$_} = ucfirst $_;
    }

    $field_label{'email'}         = 'Your Electronic Mail Address';
    $field_label{'originator'}    = 'Your name';
    $field_label{'organization'}  = 'Your Organization or Company';
    $field_label{'submitter'}     = 'Submitter ID';
    $field_label{'synopsis'}      = 'One line summary of the problem';
    $field_label{'release'}       = 'Which software release are you using';
    $field_label{'environment'}   = 'Environment (output of "show version" on the problem machine)';
    $field_label{'description'}   = 'Full Description';
    $field_label{'howtorepeat'}   = 'How to repeat the problem';
    $field_label{'fix'}           = 'Fix/workaround to the problem (if known)';

    # 2) check form values or assign defaults
    #
    my ($error, $message, %error_message);

    if ($mode eq 'ok') {                 # A form has been submitted, check it

        #  Read in values from the form into 'Q' name space
        #
        $q->import_names('Q');           

	# Data checking
        #  1) Check that email has some characters
        #
	($temp = $Q::email) =~ s/\s//g;
        if ($temp eq '') {
	    $error = 'email_req';
	    $mode = 'error';
	}

        #  2) Check that synopsys has some characters
        #
	unless ($error) {
	    ($temp = $Q::synopsis) =~ s/\s//g;
	    if ($temp eq '') {
		$error = 'synopsis_req';
		$mode = 'error';
	    }
	}

        #  3) Check that each parameter is in the list of possible values
        #
	${Q::submitter} =~ s/\s*//g;           # remove spaces
	${Q::submitter} = lc ${Q::submitter}; # normalized (lower case) submitter IS

	my ($pat, $temp);
	unless ($error) {
            foreach $list ('submitter',
                           'confidential',
                           'category',
                           'severity',
                           'priority',
                           'class',
		          ) {

		$pat = ${'Q::' . $list};

	        if ($pat =~ /^\s*$/) {
		    $error = $list . '_unselected';
		    $mode = 'error';
		    last;
		}

	        unless (grep { m/^$pat$/ } @{$possible{$list}}) {
		    $error = $list;
		    $mode = 'error';
		    last;
		}
	    }
	}


        #  4) If the bug is 'sw-bug', release field must contain something
        #
	unless ($error) {
	    if ($Q::class eq 'sw-bug') {

		($temp = $Q::release) =~ s/\s//g;
		if ($temp eq '') {
		    $error = 'release_req';
		    $mode = 'error';
		}
            }
        }

        #  5) If the bug is 'sw-bug', howtorepeat field must contain something
        #
	unless ($error) {
	    if ($Q::class eq 'sw-bug') {

		($temp = $Q::howtorepeat) =~ s/\s//g;
		if ($temp eq '') {
		    $error = 'howtorepeat_req';
		    $mode = 'error';
		}
	    }
	}

        #
        #   End of Form data checking
        #

        #  remove <pre> and </pre> tags for wide_fields

        foreach (@wide_fields) {
	    ${'Q::' . $_} =~ s/^<pre>//;
	    ${'Q::' . $_} =~ s/<\/pre>$//;
	}


    } else {                             # Set defaults for a new form
	foreach (keys %default) {
	    ${'Q::' . $_} = $default{$_};
	}
    }

    # 3) if form data okay, email the pr

    my ($resp, $pr);

    if ($mode eq 'ok') {

        push @header, "To: bugs\n";
        push @header, "Subject: $Q::synopsis\n";
        push @header, "From: $Q::email\n";
        push @header, "Reply-To: $Q::email\n";
        push @header, "Bcc: $error_mail\n";
        push @header, "X-send-pr: $NAME $VERSION\n";

        my @records = ('submitter', 
	   	       'originator',
		       'organization',
		       'confidential',		      
		       'synopsis',
		       'severity',
		       'priority',
		       'category',
		       'class',
		       'release',
		       'environment',
		       'description',
		       'howtorepeat',
		       'fix'
		      );

	foreach (@records) {
	    $record_key{$_} = ucfirst $_;
	}
	$record_key{'submitter'} = 'Submitter-Id';
	$record_key{'howtorepeat'} = 'How-To-Repeat';

	my @body;
	foreach (@records) {
	    push @body, ">$record_key{$_}:\t\t${'Q::' .$_}\n";
	}

        my $tmpfile = "/tmp/file-pr.log.$$";

        open MAIL, "|$file_pr --debug 1>&2 2>$tmpfile";
 	print MAIL @header;
        print MAIL "\n\n";
	print MAIL @body;
        close MAIL;

        open FILE_PR_LOG, "$tmpfile";

	$submit = 'failed';
        while (chomp($line1 = <FILE_PR_LOG>)) {
	    if ($line1 =~ /^file-pr: responsible person is:\s*(\S+)\s*$/) {
		$resp = $1;
		undef $submit;
		last;
	    }
	}

        chomp($line2 = <FILE_PR_LOG>);
	if ($line2 =~ /^file-pr: PR written out:\s*(\S+)\s*$/) {
            my @path_rev = reverse(split '/', (split ':', $line2)[2]);
            $pr = "$path_rev[1]/$path_rev[0]";
        } else {
	    $submit = 'failed';
        }

	if ($submit eq 'failed') {
 	    @file_pr_error = <FILE_PR_LOG>;
	    $file_pr_error = join "<br>", $line1, $line2, @file_pr_error;

            @header[0] =~ s/bugs/gnats-admin/;
            @header[1] =~ s/(Subject: )/$1(ERROR) submit-pr.cgi failed: /;

            open MAIL, "| /usr/sbin/sendmail -oi -t";
 	    print MAIL @header;
            print MAIL "\n\n";
	    print MAIL @body;
	    print MAIL "\n\n*** Error from file-pr ***\n";
	    print MAIL join "\n", $line1, $line2, @file_pr_error;
            close MAIL;

	} else {
#  	    `rm $tmpfile`;
        }
        close FILE_PR_LOG;
    }

    # Define the message
    unless ($mode eq 'make_form') {
        if ($mode eq 'ok') {
	    if ($submit eq 'failed') {
		$message = "ERROR: An error was detected during the PR submission process.<br>";
		$message .= "ERROR: Your PR has NOT been submitted.<br><br>";
		$message .= "$file_pr_error";
	    } else {
		$message = "Your problem report (PR) has been submitted as: $pr<br>";
		$message .= "The responsible person for this PR is: $resp<br>";
		$message .= "An e-mail acknowledgement will be sent to: $Q::email";
	    }
        } else {
	    # Non-standard error messages
	    $error_message{'email_req'} = 'Electronic Mail Address is a required field.';
	    $error_message{'synopsis_req'} = 'Problem Synopsis is a required field.';
	    $error_message{'release_req'} = "For $Q::class problems, you must enter the software release data.";
            $error_message{'howtorepeat_req'} = "For $Q::class problems, you must enter a description of how to repeat the problem.";
            $error_message{'submitter'} = q/Invalid name for Submitter ID<br>use "unknown" if you don't know your Submitter ID./;

	    # Standard error messages
            my $err;
            ($err = $error) =~ s/_.*//g;
	    $message = ($error =~ /_unselected/) ? "You must select a value for $field_label{$err}"
                                             : "Invalid value for $field_label{$err}";
	    $message = (exists $error_message{$error}) ? $error_message{$error} : $message;
	    $message = 'Error: ' . $message;
        }
    }
                      
    # 4) print out the HTML form
    #
    unless ($if eq 'cli') {
	print $q->header;
    }

    #  generate the HTML form
    #
    my ($bgcolor, $color, $errcolor);
    $bgcolor = '#ffffff';
    $errcolor = 'mistyrose';

    print $q->start_html(-title=>'Submit a problem report',
                         -author=>'quinot@adabroker.eu.org',
                         -BGCOLOR=>$bgcolor,
                         -text=>'#000033',
                         -link=>'#2244cc',
                         -vlink=>'#112288',
                        );

    print img { -src=>'/adabroker.jpg',
		-alt=>'AdaBroker',
		-align=>'left',
		-hspace=>'5',
		-vspace=>'5'
	       };


    print h1 ( {-align=>'right'},
	       'Submit a problem report'
	     );

    print br { -clear=>'all' };

    print hr;

    print 'Thank you for taking the time to let us know about a problem with
           AdaBroker.  Please fill out the form as completely as possible.
           Make sure you fill in the "Environment" field as requested with the
           output from the machine on which problem occurred.';

    if ($message) {    
	$color = ($error) ? $errcolor : $bgcolor;
	print $q->h1($message);
	print $q->hr;
        $error =~ s/_.*//g;
    }


    # print Form
    #
    print $q->start_form(-method=>'POST',
			 -action=>'submit-pr.cgi'
			 );

    my %elem_type = ('email'        => 'textfield 40',
		    'originator'    => 'textfield 40',
		    'organization'  => 'textfield 40',
		    'submitter'     => 'textfield 40',
                    'synopsis'      => 'textfield 72',
                    'confidential'  => 'scrollist 2',
                    'category'      => 'scrollist 3',
                    'severity'      => 'scrollist 3',
                    'priority'      => 'scrollist 3',
                    'class'         => 'scrollist 3',
                    'release'       => 'textfield 40',
                    'environment'   => 'textarea 6 72',
                    'description'   => 'textarea 6 72',
                    'howtorepeat'   => 'textarea 6 72',
                    'fix'           => 'textarea 6 72',
		   );

    print "<table>\n";

    my @type;
    foreach (@fields) {

	$color = ($error eq $_) ? $errcolor : $bgcolor;
	print "<tr bgcolor=$color><td align=right>";
	print b ("$field_label{$_}");
	print "</td><td>";
	if ($mode eq 'ok') {
	    if (${'Q::'.$_}) {
		print "${'Q::'.$_}";
	    } else {
		print '&nbsp';
	    }
        } else {
            @type = split " ", $elem_type{$_};
            if ($type[0] eq 'textfield') {
                print $q->textfield(-name=>$_,
	    		            -default=>${'Q::'.$_},
			            -size=>$type[1],
			            -maxlength=>$type[1]
			           );
            }
            if ($type[0] eq 'scrollist') {
                print $q->scrolling_list(-name=>$_,
			                 -values=>$possible{$_},
	    		                 -default=>${'Q::'.$_},
			                 -size=>$type[1],
			                );
            }
            if ($type[0] eq 'textarea') {
#	    		            -default=>"<pre>${'Q::'.$_}</pre>",
                print $q->textarea(-name=>$_,
	    		            -default=>"${'Q::'.$_}",
			            -rows=>$type[1],
			            -columns=>$type[2],
	                            -wrap=>'physical'
			           );
            }
        }
	print "</td></tr>\n";
    }

    print "</table><table>";
    foreach (@wide_fields) {

	$color = ($error eq $_) ? $errcolor : $bgcolor;
	print "<tr><td colspan=2>";
	print b ("$field_label{$_}");
	print "</td></tr>";
	print "<tr bgcolor=$color><td colspan=2>";
	if ($mode eq 'ok') {
	    if (${'Q::'.$_}) {
		print "${'Q::'.$_}";
	    } else {
		print '&nbsp';
	    }
        } else {
            @type = split " ", $elem_type{$_};
            if ($type[0] eq 'textfield') {
                print $q->textfield(-name=>$_,
	    		            -default=>${'Q::'.$_},
			            -size=>$type[1],
			            -maxlength=>$type[1]
			           );
            }
            if ($type[0] eq 'scrollist') {
                print $q->scrolling_list(-name=>$_,
			                 -values=>$possible{$_},
	    		                 -default=>${'Q::'.$_},
			                 -size=>$type[1],
			                );
            }
            if ($type[0] eq 'textarea') {
#	    		            -default=>"<pre>${'Q::'.$_}</pre>",
                print $q->textarea(-name=>$_,
	    		            -default=>${'Q::'.$_},
			            -rows=>$type[1],
			            -columns=>$type[2],
	                            -wrap=>'physical'
			           );
            }
        }
	print "</td></tr>\n";
    }

    print "</table>";

    unless ($mode eq 'ok') {
	print $q->submit(-name=>'Submit');
	print $q->reset;
    }

    print address ('AdaBroker / ',
	           $q->a({href=>'mailto:quinot@adabroker.eu.org'},
                        'quinot@adabroker.eu.org'
                        ),
                  );

    print $q->end_form();
    print $q->end_html();

}
