#!/usr/local/bin/perl
# $Id$
#
# GNATS problem tracking report generator
#
# This script can be used to generate reports from the GNATS database.
# The same tool is used to generate both HTML and ascii reports.
#
# Copyright (c) 1994-1998, FreeBSD Inc.
# All rights reserved.
# Copyright (c) 1997-1999, Juniper Networks Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#  notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#
# $Id$
#
# -----------------------------------------------------------------------
# TODO:
#	Add back support for sorting on dates once we confirm new gnats
#	    has been installed at FreeBSD Inc. (arrival/modified).
#	Move the HTML formatting crap out to a separate library ala
#	    the old cgi-style.pl so it can be shared with query-pr.cgi.
# -----------------------------------------------------------------------
#
# $preformat = 1;

use Getopt::Long;
use CGI;

$html_mode     = 1 if $ENV{'DOCUMENT_ROOT'};

$ENV{'PATH'}   = '/bin:/usr/bin:/usr/sbin:/sbin:/usr/local/bin';
$query_pr      = '/usr/bin/query-pr -d adabroker';


#
# Customization variables to move things back and forth between
# Juniper Networks and FreeBSD, Inc.
#
$project = "AdaBroker";	# FreeBSD

if ($project eq "Juniper") {
    $mail_prefix	= "bug-";
    $mail_unass		= "bugs";
    $default_submitter	= "juniper";
} else {
    $mail_prefix	= "ab-";
    $mail_unass		= "ab-bugs";
    $default_sumbitter	= "net";
}


#Usage: query-pr [-FGhiPRqVx] [-C confidential] [-c category] [-d directory]
#       [-e severity] [-m mtext] [-O originator] [-o outfile] [-p priority]
#       [-L class] [-r responsible] [-S submitter] [-s state] [-t text]
#       [-b date] [-a date] [-B date] [-M date] [-z date] [-Z date]
#       [-y synopsis] [-A release] [--full] [--help] [--print-path] [--version]
#       [--summary] [--sql] [--skip-closed] [--category=category]
#       [--confidential=yes|no] [--directory=directory] [--output=outfile]
#       [--originator=name] [--priority=level] [--class=class]
#       [--responsible=person] [--release=release] [--restricted]
#       [--quarter=quarter] [--keywords=regexp]
#       [--required-before=date] [--required-after=date]
#       [--arrived-before=date] [--arrived-after=date]
#       [--modified-before=date] [--modified-after=date]
#       [--closed-before=date] [--closed-after=date]
#       [--severity=severity] [--state=state] [--submitter=submitter]
#       [--list-categories] [--list-classes] [--list-responsible]
#       [--list-states] [--list-submitters] [--list-config]
#       [--synopsis=synopsis] [--text=text] [--multitext=mtext] [PR] [PR]...

@input_fields = ( "confifdential", "originator", "priority", "class",
		  "category", "responsible", "release", "quarter", "keywords",
		  "required-before", "required-after",
		  "arrived-before", "arrived-after",
		  "modified-before", "modified-after",
		  "closed-before", "closed-after",
		  "severity", "state", "submitter",
		  "synopsis", "text", "multitext",
		  # now our special fields ...
		  "closed", "public" );

#
#----------------------------------------------------------------------------
# HTML management
#----------------------------------------------------------------------------
#

#
# Convert a line from ascii to HTML-ascii (handle things that are likely
# to be displayed incorrectly.
#
sub html_fixline {
    my $line = shift;

    $line =~ s/&/&amp;/g;
    $line =~ s/</&lt;/g;
    $line =~ s/>/&gt;/g;

    return $line;
}

#
# Print an initial HTML header
#	Place whatever stylistic data you want right here.
#
#	XXX Project specific data found here (Juniper vs. FreeBSD)
# 
sub html_header {
    my $title = shift;

    print $query->header;
    print $query->start_html('title'   => $title,
			     'author'  => 'quinot@adabroker.eu.org',
			     'bgcolor' => '#ffffff');
    print <<EOM;
<IMG SRC="/adabroker.jpg" ALT="AdaBroker" ALIGN=LEFT>
<H1 ALIGN=RIGHT>$title</H1>
<BR CLEAR=ALL>
<HR NOSHADE>
EOM
}

#
# Print project specific HTML footer information here
#
#	XXX Project specific data found here (Juniper vs. FreeBSD)
#
sub html_footer {
    print <<EOM;
</BODY>
EOM
}

#
# These self references are attempts to only change a single variable at a time.
# If someone does a multiple-variable query they will probably do weird things.
#
sub html_refs {
    my $me_ref1 = $self_ref . '?';
    $me_ref1 .= "sort=$input{sort}" if $input{sort};
    $me_ref1 .= '&' if ($me_ref1 !~/\?$/);

    my $me_ref2 = $self_ref . '?';
    foreach (@input_fields) {
	if ($input{$_}) {
	    $me_ref2 .= '&' if ($me_ref2 !~/\?$/);
	    $me_ref2 .= $_ . '=' . $input{$_};
	}
    }

    my $me_ref3 = $me_ref2;
    $me_ref3 =~ s/\?closed=on&/?/g;
    $me_ref3 =~ s/\?closed=on//g;
    $me_ref3 =~ s/&closed=on//g;

    print <<EOM;
<P>
You may view summaries by
	<A HREF="${me_ref1}severity=summary">Severity</A>,
	<A HREF="${me_ref1}state=summary">State</A>,
	<A HREF="${me_ref1}category=summary">Category</A>,
	<A HREF="${me_ref1}priority=summary">Priority</A>,
	<A HREF="${me_ref1}class=summary">Class</A>, or
	<A HREF="${me_ref1}responsible=summary">Responsible Party</A>.
<BR>
You may also sort by 
	<A HREF="${me_ref2}&sort=category">Category</A>,
	<A HREF="${me_ref2}&sort=priority">Priority</A>,
	<A HREF="${me_ref2}&sort=severity">Severity</A>, or
	<A HREF="${me_ref2}&sort=responsible">Responsible Party</A>.<BR>
Or <A HREF="$self_ref?query">formulate a specific query</A>.
EOM

    if ($input{closed} eq "on") {
	print <<EOM 
<A HREF="${me_ref3}">Exclude closed reports.</A><BR>
EOM
    } else {
	print <<EOM
<A HREF="${me_ref3}&closed=on">Include closed reports.</A><BR>
EOM
    }

#
# The following are Juniper additions, some of our standard weekly
# reports for problem tracking meetings.
#
    if ($project eq "AdaBroker") {
	$base   = "$self_ref?priority=summary&sort=severity";
	$b_new  = $base . "&state=active&arrived-after=1%20week%20ago";
	$b_sw   = $base . "&state=active&class=sw-bug";
	$b_chg  = $base . "&state=active&class=change-request";

	print <<EOM;
Weekly reports:
	<A HREF="$b_new">new this week</A>,
	<A HREF="$b_sw">software</A>,
	<A HREF="$b_chg">change-requests</A>.
<P>
EOM
	}
}

#
# Severities are hardcoded in gnats (ick)
#
&create_field(\@f_severities, "?error?",	"?",	0, "");
&create_field(\@f_severities, "critical",	"c",	1, "");
&create_field(\@f_severities, "serious",	"s",	2, "");
&create_field(\@f_severities, "non-critical",	"n",	3, "");

#
# As are priorities
#
&create_field(\@f_priorities, "?error?",	"?",	0, "");
&create_field(\@f_priorities, "high",		"h",	1, "");
&create_field(\@f_priorities, "medium",		"m",	2, "");
&create_field(\@f_priorities, "low",		"l",	3, "");

#
# Use the same mechanism for handling our own web script's sorting hacks.
# This is overkill, I'm just being lazy.
#
&create_field(\@f_sort,	"category",		"",	1, "");
&create_field(\@f_sort,	"priority",		"",	2, "");
&create_field(\@f_sort,	"responsible",		"",	3, "");
&create_field(\@f_sort,	"severity",		"",	4, "");
&create_field(\@f_sort,	"last-modified",	"",	5, "");

#
# Get all dynamic information from GNATS
#
&get_states;
&get_classes;
&get_categories;
&get_submitters;
&get_responsibles;

if ($html_mode) {
	CGI::ReadParse(*input);
	$query = $input{CGI};

	$query->use_named_parameters(1);

	$self_ref = $query->script_name;
	($query_pr_ref = $self_ref) =~ s/-summary//;

	if ($query->query_string eq 'keywords=query') {
	    &html_header("Query $project problem reports");
	    &query_form;
	    &html_footer;
	    exit(0);
	}

	&html_header("$project problem reports");
	&html_refs;

} else {

	%optctl = (
		"closed"		=> \$input{closed},
		"quiet"			=> \$input{quiet},
		"public"		=> \$input{public},
		"wide"			=> \$wide,
		"releases"		=> \$releases,

		"sort"			=> \$input{sort},
		"category"		=> \$input{category},
		"class"			=> \$input{class},
		"priority"		=> \$input{priority},
		"submitter"		=> \$input{'submitter'},
		"responsible"		=> \$input{responsible},
		"severity"		=> \$input{severity},
		"state"			=> \$input{state},
		"release"		=> \$input{release},
		"arrived-before"	=> \$input{'arrived-before'},
		"arrived-after"		=> \$input{'arrived-after'},
		"closed-before"		=> \$input{'closed-before'},
		"closed-after"		=> \$input{'closed-after'},
		"modified-before"	=> \$input{'modified-before'},
		"modified-after"	=> \$input{'modified-after'},
		"synopsis"		=> \$input{'synopsis'},
	);

	GetOptions(\%optctl,
		   "closed", "quiet", "public", "wide", "releases",
		   "sort=s", "class=s", "category=s", "priority=s",
		   "submitter=s",
		   "responsible=s", "severity=s", "state=s", "release=s",
		   "arrived-before=s", "arrived-after=s",
		   "closed-before=s", "closed-after=s",
		   "modified-before=s", "modified-after=s",
		   "synopsis=s") ||
	    die "usage:\n" .
"--closed		Include closed reports\n",
"--quiet			Skip headers\n",
"--public			Omit confidential reports\n",
"--wide			Unlimited line length\n",
"--releases		Print release information\n\n",
"--arrived-after=<date>	PR arrived after <date>\n",
"--arrived-before=<date>	PR arrived before <date>\n",
"--closed-after=<date>	PR closed after <date>\n",
"--closed-before=<date>	PR closed before <date>\n",
"--modified-after=<date>	PR modified after <date>\n",
"--modified-before=<date>	PR modified before <date>\n",
"--category=<>		summary:<category>\n",
"--class=<>		summary:",
	join(':', &field_names(@f_classes)), "\n",
"--priority=<>		summary:",
	join(':', &field_names(@f_priorities)), "\n",
"--release=<string>	Release field contains <string>\n",
"--responsible=<>	summary:<individual>\n",
"--severity=<>		summary:",
	join(':', &field_names(@f_severities)), "\n",
"--sort=<>		",
	join(':', &field_names(@f_sort)), "\n",
"--submitter=<>		",
	join(':', &field_names(@f_submitters)), "\n",
"--state=<>		summary:active:",
	join(':', &field_names(@f_states)), "\n";
}

#------------------------------------------------------------------------

# Build up the query request
$query_args  = '';
$query_args .= '--skip-closed ' unless $input{closed};
#### ACTUALLY DO NOT EVER OUTPUT CONFIDENTIAL BUGS TO WWW:
$query_args .= '--restricted '  if     $input{public} || $html_mode;

foreach (@input_fields) {
    next if ($_ eq "closed" || $_ eq "public");

    if ($input{$_} && $input{$_} ne "summary") {
	if ($html_mode) {
	    $query_args .= " --${_}=\'" . join("|", $query->param($_)) . "\'";
	} else {
	    $query_args .= " --${_}=\'" . $input{$_} . "\'";
	}
    }
}

# Handle active pseudo-state (can't just change $input{state} because
# $input is now a tied variable (sigh).. kludge it).
$query_args =~ s/--state='active'/--state='$active_states'/;

&query_gnats($query_args);

#
# Sort reports as necessary
#
if ($input{sort} eq 'category') {
	@prs = sort { $a->{category} eq $b->{category} ?
			$a->{number} <=> $b->{number}  :
			$a->{category} cmp $b->{category}
		    } @prs;

} elsif ($input{sort} eq 'responsible') {
	@prs = sort { $a->{responsible} eq $b->{responsible} ?
			$a->{number} <=> $b->{number}  :
			$a->{responsible} cmp $b->{responsible}
		    } @prs;

} elsif ($input{sort} eq 'last-modified') {
	@prs = sort { $a->{'last-modified'} eq $b->{'last-modified'} ?
			$a->{number} <=> $b->{number}  :
			$a->{'last-modified'} cmp $b->{'last-modified'}
		    } @prs;

} elsif ($input{sort} eq 'priority') {
	@prs = sort { ($a->{priority})->{sql} eq ($b->{priority})->{sql} ?
			$a->{number} <=> $b->{number}  :
			($a->{priority})->{sql} cmp ($b->{priority})->{sql}
		    } @prs;

} elsif ($input{sort} eq 'severity') {
	@prs = sort { ($a->{severity})->{sql} eq ($b->{severity})->{sql} ?
			$a->{number} <=> $b->{number}  :
			($a->{severity})->{sql} cmp ($b->{severity})->{sql}
		    } @prs;

} else {
	$input{sort} = 'none';
}

#
# Generate the appropriate report(s)
#
if ($#prs < $[) {
	print "No matches to your query\n";

} elsif ($input{'responsible'} eq 'summary') {
	&norm_summary('responsible', @f_responsibles);

	&gnats_summary("Unassigned problems", '$entry->{responsible} eq ""');
		  
} elsif ($input{'state'} eq 'summary') {
	&enum_summary('state', @f_states);

} elsif ($input{'category'} eq 'summary') {
	&norm_summary('category', @f_categories);

} elsif ($input{'priority'} eq 'summary') {
	&enum_summary('priority', @f_priorities);

} elsif ($input{'severity'} eq '' || $input{'severity'} eq 'summary') {
	&enum_summary('severity', @f_severities);

} else {
	&gnats_summary("Requested problem reports", 1);
}

&html_footer if $html_mode;
exit(0);

#------------------------------------------------------------------------

sub norm_query {
    my $category = shift;
    my $field	 = shift;

    &gnats_summary("$field $category problems",
	    '$entry->{\'' . $category . '\'} eq "' .  $field . '"');
}

sub norm_summary {
    my $category = shift;
    my %field_type;

    foreach $field_type (@_) {
	&norm_query($category, $field_type->{name});
    }
}

sub enum_query {
    my $category = shift;
    my $field    = shift;

    &gnats_summary("$field $category problems",
	    '($entry->{\'' . $category . '\'})->{\'name\'} eq "' .
	    $field . '"');
}

sub enum_summary {
    my $category = shift;
    my %field_type;

    foreach $field_type (@_) {
	&enum_query($category, $field_type->{name});
    }
}

sub get_categories {
    open(Q, "$query_pr --list-categories |") ||
	die "Cannot get categories\n";

    my $sql = 0;
    &create_field(\@f_categories, "?error?", "?", $sql++, "");

    while(<Q>) {
	chomp;
	my ($cat, $desc, $responsible, $notify) = split(/:/);
	&create_field(\@f_categories, $cat, $cat, $sql++, $desc);
    }
    close(Q);
}

#
# Get states from gnats
#
# Add a pseudo-state for reports called 'active' which means anything
# not suspended or closed.
#
sub get_states {
    open(Q, "$query_pr --list-states |") || die "Cannot get states\n";

    my $sql = 0;
    my @active;
    &create_field(\@f_states, "?error?", "?", $sql++, "");

    while(<Q>) {
	chomp;
	my ($state, $desc) = split(/:/);
	&create_field(\@f_states, $state, substr($state, 0, 1), $sql++, $desc);

	push(@active, $state) unless ($state eq "suspended" ||
				      $state eq "closed");
    }
    close (Q);

    $active_states = join('|', @active);
}

#
# Get classes from gnats
#
sub get_classes {
    open(Q, "$query_pr --list-classes |") || die "Cannot get classes\n";

    my $sql = 0;
    &create_field(\@f_classes, "?error?", "?", $sql++, "");

    while(<Q>) {
	chomp;
	my ($class, $desc) = split(/:/);
	my ($short) = substr($class, 0, 1);

	#
	# Support conflicts with sw-bug
	# Duplicate conflicts with doc-bug
	#
	$short = "S" if $class eq "support";
	$short = "D" if $class eq "duplicate";

	&create_field(\@f_classes, $class, $short, $sql++, $desc);
    }
    close (Q);
}

#
# Get responsible parties from gnats
#
sub get_responsibles {
    open(Q, "$query_pr --list-responsible |") ||
	die "Cannot get resposible parties\n";

    my $sql = 0;
    &create_field(\@f_responsibles, "?error?", "?", $sql++, "");

    while(<Q>) {
	chomp;
	my ($resp, $desc) = split(/:/);
	&create_field(\@f_responsibles, $resp, $desc, $sql++, $desc);
    }
    close (Q);
}

#
# Get submitter-id's from gnats
#
sub get_submitters {
    open(Q, "$query_pr --list-submitters |") ||
	die "Cannot get submitter-id's\n";

    my $sql = 0;
    &create_field(\@f_submitters, "?error?", "?", $sql++);

    while(<Q>) {
	chomp;
	my ($id, $desc, $other) = split(/:/);
	&create_field(\@f_submitters, $id, $desc, $sql++);
    }
    close (Q);
}

#
# Load up the PR array with all problems matching a given query
#
sub query_gnats {
    my $report = shift;

    #print "Report: $report\n";

    open(Q, "$query_pr --sql $report|") || die "Cannot query the reports.\n";

    while(<Q>) {
	chomp;

	my ($s_number, $s_category, $s_synopsis, $s_confidential,
	    $s_severity, $s_priority, $s_responsible, $s_state,
	    $s_class, $s_submitter, $s_arrival_date, $s_originator,
	    $s_release, $s_last_modified, $s_closed_date, $s_quarter,
	    $s_keywords, $s_anythingelse) = split(/\s*\|/);

	my %rec = (
		'submitter'	=> $s_submitter,
		'number'	=> $s_number,
		'category'	=> $s_category,
		'synopsis'	=> $s_synopsis,
		'confidential'	=> $s_confidential,
		'severity'	=> &field_from_sql(\@f_severities, $s_severity),
		'priority'	=> &field_from_sql(\@f_priorities, $s_priority),
		'responsible'	=> $s_responsible,
		'state'		=> &field_from_sql(\@f_states, $s_state),
		'class'		=> &field_from_sql(\@f_classes, $s_class),
		'submitter'	=> $s_submitter,
		'arrival'	=> $s_arrival_date,
		'originator'	=> $s_originator,
		'release'	=> $s_release,
		'last-modified'	=> $s_last_modified,
		'closed-date'	=> $s_closed_date,
		'quarter'	=> $s_quarter,
		'keywords'	=> $s_keywords,
	);

	push(@prs, \%rec);
    }
    close(Q);
}

#
# Clean up release field
#
#	At Juniper, for software defects, the relevant releases are
#	listed as "JUNOS-x.x", we cut this down for display purposes
#	to just have the relevant version numbers displayed.
#
sub clean_release {
    my $release = shift;

    if ($project eq "Juniper") {
	$release =~ s/junos-(\d+\.\d+)\S*/\1/gi;
	$release =~ s/,/ /g;
	$release =~ s/\s+/ /g;
    }

    return $release;
}

#
# Clean up the responsible field
#
# 	We have mailing lists for responsible parties, if the bug is
#	assigned to a list, we consider it pending individual
#	assignment and just clear the field for display.
#	
sub clean_responsible {
    my $responsible = shift;

    $responsible  =~ s/@.*//;
    $responsible  =~ tr/A-Z/a-z/;
    $responsible  =  "" if ($responsible =~ /^$mail_unass/);
    $responsible  =  "" if ($responsible =~ /^$mail_prefix/);
    $responsible  =~ s/^$mail_prefix//;

    # cut it down to a username's length to keep it narrow
    $responsible  = substr($responsible, 0, 8);

    return $responsible;
}

#
# Clean up the submitter field
#
# 	We use the submitter field for each special submitter,
#	and have an default submitter field.  If a report was
#	submitted with this default submitter ID, display the
#	last name of the originator instead.
#
sub clean_submitter {
    my $submitter  = shift;
    my $originator = shift;

    if ($submitter eq $default_submitter) {
	$originator =~ s/,.*//;
	$originator =~ s/.*\s+(\S+)/\1/;
	$originator =~ s/A-Z/a-z/;
	$submitter = substr(${originator}, 0, 9);
    } else {
	$submitter = "<strong>${submitter}</strong>"
		if ($html_mode && !$preformat);
    }

    return $submitter;
}

#
# Shorten the date entries down to something readable/manageable.
# We're deliberately causing a Y2K issue here by shortening the
# century down to 2 bytes to leave more room for the synopsis.
# Will we never learn?
#
sub clean_date {
    my $date = shift;

    $date =~ s/^\d\d//;		# chop off the century
    $date =~ s/\s+.*//;		# chop off the time

    return $date;
}

#
# Generate a summary of all problem reports matching the current query.
#
sub gnats_summary {
    my $header		= ucfirst(shift);
    my $report		= shift;
    my $counter		= 0;

    foreach $entry (@prs) {
	next if (($report ne '') && (eval($report) == 0));

	if ($counter++ == 0) {
	    if ($html_mode) {
		if ($preformat) {
		    print <<EOM;
<H3>${header}</H3>
<PRE>
VPSC Release(s)   Arrived  Submitter Tracker            Respons. Description
<HR>
EOM
		} else {
		    print <<EOM;
<HR>
<TABLE>
<CAPTION><STRONG><FONT SIZE="+1">$header</FONT><STRONG></CAPTION>
<TR ALIGN=LEFT>
<TH>VPSC</TH>
<TH NOWRAP>Release(s)</TH>
<TH>Arrived</TH>
<TH>Submitter</TH>
<TH>Tracker</TH>
<TH>Respons.</TH>
<TH>Description</TH>
EOM
	      }
	    } else {
		print "${header}:\n\n" .
		      "VPSC Arrived  ID                 Respons. Description\n".
		      "---- -------- ------------------ -------- -----------" .
		      "--------------------------\n";
	    }
	}

	my $synopsis    = $entry->{synopsis};
	my $tracker     = $entry->{category} . '/' . $entry->{number};
	my $title       = $tracker;
	my $severity    = ($entry->{severity})->{short};
	my $priority    = ($entry->{priority})->{short};
	my $state       = ($entry->{state})->{short};
	my $class       = ($entry->{class})->{short};
	my $responsible = &clean_responsible($entry->{responsible});

	$tracker = substr($tracker, length($tracker) - 18, 18)
		if length($tracker) > 18 && (!$html_mode || $preformat);

	if ($html_mode) {
	    $synopsis = &html_fixline($synopsis);
	    $title    = "<a href=\"${query_pr_ref}?pr=" . $entry->{number} .
			"\">${tracker}</a>";
	}

	if ($html_mode) {
	    if ($preformat) {
		printf "%s%s%s%s %-12.12s %s %-9.9s %s %-8.8s %s\n",
		       $severity, $priority, $state, $class,
		       &clean_release($entry->{release}),
		       &clean_date($entry->{arrival}),
		       &clean_submitter($entry->{submitter},
					$entry->{originator}),
		       $title . ' ' x (18 - length($tracker)),
		       $responsible,
		       $synopsis;
	    } else {
		printf "<TR VALIGN=TOP>" .
		       "<TD NOWRAP><PRE>%s%s%s%s</PRE></TD>" .
		       "<TD>%s</TD>" .
		       "<TD NOWRAP>%s</TD>" .
		       "<TD NOWRAP>%s</TD>" .
		       "<TD NOWRAP>%s</TD>" .
		       "<TD NOWRAP>%s</TD>" .
		       "<TD>%s</TD>\n" ,
		       $severity, $priority, $state, $class,
		       &clean_release($entry->{release}),
		       &clean_date($entry->{arrival}),
		       &clean_submitter($entry->{submitter},
					$entry->{originator}),
		       $title,
		       $responsible,
		       $synopsis;
	    }
	} else {
	    #
	    # We don't have as much flexibility with the ascii format,
	    # so ditch the release and submitter fields.
	    #
	    printf "%s%s%s%s %s %s %-8.8s %s\n",
		    $severity, $priority, $state, $class,
		    &clean_date($entry->{arrival}),
		    $title . ' ' x (18 - length($tracker)),
		    $responsible,
		    ($wide ? $synopsis : substr($synopsis, 0, 37));
	}
    }

    if ($counter) {
	if ($html_mode) {
	    if ($preformat) {
		print "</PRE><P>\n";
	    } else {
		print "</TABLE><P>\n";
	    }
	}
	print "\n${counter} problems total.\n\n";
    }
    return $counter;
}

# ---------------------------------------------------------------------------
# Enumerated field management routines
#
# These are a bunch of "complex as all hell" routines for building
# some perl5 hashes.  They should be cleaned up.

sub create_field {
    local *table = shift;
    my $name  = shift;
    my $short = shift;
    my $sql   = shift;
    my $desc  = shift;

    my %rec   = ( name => $name, sql => $sql, short => $short, desc => $desc);
    push(@table, \%rec);
};

sub field_from_sql {
    local *table  = shift;
    my $sqlval = shift;

    my $entry;
    foreach $entry (@table) {
	return $entry if $entry->{sql} eq $sqlval;
    }
    return undef;
}

sub field_from_name {
    local *table  = shift;
    my $name = shift;

    my $entry;
    foreach $entry (@table) {
	return $entry if $entry->{name} eq $name;
    }
    return undef;
}

sub field_names {
    my $field_type;
    my @result;

    foreach $field_type (@_) {
	$_ = $field_type->{name};
	next if $_ eq '?error?';
	push(@result, $_);
    }
    return @result;
}

sub field_list {
    my $prefix  = shift;
    my $display = shift;
    my $name    = shift;
    my @values  = &field_names(@_);

    print $prefix;
    print "<TD><B>$display</B>:</TD><TD>";
    print $query->scrolling_list(-name   => $name,
				 -values => [ @values ],
				 -size   => 5,
				 -multiple => 'true');
    print "</TD>";
}

sub field_boxes {
    my $prefix  = shift;
    my $display = shift;
    my $name    = shift;
    my @values  = &field_names(@_);

    print $prefix;
    print "<TD><B>$display</B>:</TD><TD COLSPAN=4>";
    print $query->checkbox_group(-name   => $name,
				 -values => [ @values ]);
    print "</TD>";
}

sub field_radio {
    my $prefix  = shift;
    my $display = shift;
    my $name    = shift;
    my @values  = &field_names(@_);

    print $prefix;
    print "<TD><B>$display</B>:</TD><TD COLSPAN=4>";
    print $query->radio_group(-name   => $name,
			      -values => [ @values ]);
    print "</TD>";
}

sub field_text {
    my $prefix  = shift;
    my $display = shift;
    my $name    = shift;

    print $prefix;
    print "<TD><B>$display</B>:</TD><TD>";
    print $query->textfield(-name => $name);
    print "</TD>";
}


# ---------------------------------------------------------------------------
# Display the "query" form for customer problem report queries.
#

sub query_form {
print qq`

Please select the items you wish to search for.  Multiple items are AND'ed
together.
<P>

`;

print $query->startform(-method => 'get'), "<TABLE>\n";

&field_boxes("<TR>",	"States",	"state",	@f_states);
&field_boxes("<TR>",	"Severity",	"severity",	@f_severities);
&field_boxes("<TR>",	"Priority",	"priority",	@f_priorities);
&field_radio("<TR>",	"Sort by",	"sort",		@f_sort);

&field_list("<TR>",	"Responsible",	"responsible",	@f_responsibles);
&field_list("",		"Submitter",	"submitter",	@f_submitters);
&field_list("<TR>",	"Category",	"category",	@f_categories);
&field_list("",		"Classes",	"class",	@f_classes);

&field_text("<TR>",	"Arrived Before",		"arrived-before");
&field_text("",		"Arrived After",		"arrived-after");
&field_text("<TR>",	"Closed Before",		"closed-before");
&field_text("",		"Closed After",			"closed-after");
&field_text("<TR>",	"Modified Before",		"modified-before");
&field_text("",		"Modified After",		"modified-after");
&field_text("<TR>",	"Release",			"release");
&field_text("",		"Originator",			"originator");
&field_text("<TR>",	"Target",			"quarter");
&field_text("",		"Text in multi-line fields",	"multitext");

print qq`

<TR>
<TD COLSPAN=2><B>Include closed reports</B>:
<INPUT TYPE=CHECKBOX NAME="closed"></TD>

<TD COLSPAN=2><B>Exclude confidential reports</B>:
<INPUT TYPE=CHECKBOX NAME="public"></TD>

</TABLE><P>`,

$query->submit(-value => " Query PR's "),
$query->reset,
$query->endform();

}
