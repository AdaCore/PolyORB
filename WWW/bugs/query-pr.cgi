#!/usr/local/bin/perl
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
use CGI;
$ENV{'PATH'} = "/bin:/usr/bin:/usr/sbin:/sbin:/usr/local/bin";

$project = "AdaBroker";		# FreeBSD

if ($project eq "Juniper") {
    $default_domain	= '@juniper.net';
    $submit		= 'bugs\@juniper.net';
    $query_pr		= 'nquery-pr';
    $cvsweb		= 'http://www-in.juniper.net/cgi-bin/cvsweb.cgi/';
} elsif ($project eq "AdaBroker") {
    $default_domain	= '@adabroker.eu.org';
    $author             = 'quinot@adabroker.eu.org';
    $submit		= 'bugs@adabroker.eu.org';
    $query_pr		= 'query-pr';
    $query_pr_args      = '-d adabroker';
    $restrict		= '--restricted';
    # $cvsweb		= 'http://adabroker.eu.org/';
} else {
    die ("Unknown project!");
}

sub html_header {
    my $title = shift;

    print $query->header;
    print $query->start_html('title'  => $title,
			     'author' => $author,
			     'bgcolor'=> '#ffffff');

    print <<EOM;
<img src="/adabroker.jpg"
     alt="AdaBroker" align=left>
<h1  align=right>$title</h1>
<br clear=all>
<hr noshade>
EOM
}

sub html_footer {
}

CGI::ReadParse(*input);

$input{pr} = $ENV{'QUERY_STRING'} unless $input{pr};
$query     = $input{CGI};
$query->use_named_parameters(1);
($scriptname = $ENV{'SCRIPT_NAME'}) =~ s|^/?|/|;
$scriptname =~ s|/$||;
($summary = $scriptname) =~ s/query-pr/query-pr-summary/;

unless ($input{pr}) {
    &html_header("PR Query Interface");

    print "<p>Please enter the PR number you wish to query:</p>\n";

    print "<FORM METHOD=GET ACTION=\"$scriptname\">\n";
    print "<INPUT TYPE=TEXT NAME=pr></FORM>\n";
    print "<p>See also the <A HREF=\"$summary\">PR summary</A></p>\n";
    &html_footer;
    exit 0;
}

# be tolerant to <category>/<PR id> queries
$input{pr} =~ s%^[a-z]+/([0-9]+)$%$1%; 

if ($input{pr} < 1 || $input{pr} > 99999) {
    &html_header("$project Problem Report");

    print "<p>Invalid problem report number: $input{pr}</p>\n";

    &html_footer;
    exit 0;
}

unless (open(Q, "$query_pr $query_pr_args $restrict -F $input{pr} 2>&1 |")) {
    &html_header("$project Problem Report");
    print "<p>Unable to open PR database.</p>\n";
    &html_footer;
    die "Unable to query PR's";
}

$multiline = 0;
$from = "";
$replyto = "";

$state = "header";
while(<Q>) {
    chomp;

    $html_fixup = 1;

    #
    # If we have an error return from query-pr, print it and abort
    #
    if (/^${query_pr}: /) {
	&html_header("$project Problem Report");
	print "<PRE>$_\n";
	print <Q>;
	print "</PRE>\n";
	&html_footer;
	exit 0;
    }

    #
    # Process useful crap out of the e-mail header on the front
    #
    if ($state eq "header") {
	if (/^From:\s*(.*)$/i) {
	    $from = $1;
	    $from =~ s/.*<(.*)>.*/$1/;
	    $from =~ s/\s*\(.*\)\s*//;
	}
	if (/^Reply-to:\s*(.*)$/i) {
	    $replyto = $1;
	    $replyto =~ s/.*<(.*)>.*/$1/;
	    $replyto =~ s/\s*\(.*\)\s*//;
	}

	# End of e-mail header
	if (/^$/) {
	    $from = $replyto if ($replyto);
	    $email = $from;
	    if ($default_domain) {
		$email .= $default_domain unless ($email =~ /@/);
	    }
	    $state = "title";
	}
    }

    #
    # Process the one line fields
    #
    if ($state eq "title") {
	if (/^>Number:/) {
	    $number   = &getline($_);
	} elsif (/^>Category:/) {
	    $category = &getline($_);
	} elsif (/^>Synopsis:/) {
	    $synopsis = &getline($_);
	    $synopsis =~ s/[\t]+/ /g;
	    $syn = &fixline($synopsis);

	    &html_header("$category/$number");
            print "<h1>$syn</h1>\n<table>\n";
	    $state = "table";
	}
    }

    if ($state eq "table") {
	#
	# Special case for responsible, fake up the line and add the
	# e-mail info we sucked in from the header
	#
	if (/^>Responsible:/) {
	    $_ = &getline($_);
	    s/\(.*\)//;			# remove personal name
	    s/\s+//g;
	    $_ = $_ . $default_domain if !/@/;
	    $_ = '>Responsible:<a href="mailto:' . $_ . '">' . $_ . '</a>';
	    $html_fixup = 0;
	}

	#
	# Ditto with originator line
	#
	if (/^>Originator:/ && $from) {
	    $_ .= " <a href=\"mailto:$email\">" . &fixline($from) . '</a>';
	    $html_fixup = 0;
	}

	#
	# Organization is the first of the multi-line entry areas,
	# we're done with the table.
	#
	if (/^>Organization:/) {
	    print "</table>\n<dl>\n";
	    $state = "multiline";
	} else {
	    if (!/^>(\S+):\s*(.*)\s*/) {
		print "<tr><th>ERROR</th><td>" . &fixline($_) . "</td>\n";
	    } else {
		$line = $html_fixup ? &fixline($2) : $2;
		print "<tr><th align=left valign=top nowrap>$1</th><td>$line</td>\n";
	    }
	}
    }

    if ($state eq "multiline") {
	if (/^>(\S+):\s*(.*)/) {
	    print $trailer . "\n" unless ($blank);
	    $trailer = "<dt><strong>$1</strong><dd>\n";
	    if ($html_fixup) {
		$trailer .= &fixline($2);
	    } else {
		$trailer .= $2;
	    }
	    $blank = !($2);
	    $multiline = 0;
	} else {
	    unless ($multiline) {
		next if /^\s*$/;
		print $trailer . "\n<pre>\n";
	    }
	    $multiline = 1;
	    $blank = 0;
	    print $html_fixup ? &fixline($_) : $_ , "\n";
	    $trailer = "</pre>";
	}
    }
}
close(Q);

print "$trailer\n" unless ($blank);
print "</dl>";

$syn   =~ s/[\?&%"]/"%" . sprintf("%02X", unpack(C, $&))/eg;
$email =~ s/[\?&%]/"%" . sprintf("%02X", unpack(C, $&))/eg;

print "<A HREF=\"mailto:${submit},${email}?subject=Re: ${category}/${number}: ${syn}\">Submit Followup</A>\n";

&html_footer;
exit 0;

sub getline
{
    local($_) = @_;
    ($tag,$remainder) = split(/[ \t]+/, $_, 2);
    return $remainder;
}

sub cvsweb {
    local($file) = shift;
    $file =~ s/[,.;]$//;
    return $cvsweb . $file;
}
    
sub srcref {
    local($_) = shift;

    local($rev) = '(rev\.?|revision):?\s+[0-9]\.[0-9.]+(\s+of)?';
    local($src) = '((src|www|doc|ports)/[^\s]+)';

    if (m%$rev\s*$src%oi || m%$src\s*$ref%) {
	s#$src#sprintf("<a href=%c%s%c>%s</a>", 34, &cvsweb($1), 34, $1)#ge;
    }

    return $_;
}

sub fixline {
    local($line) = shift;
    
    $line =~ s/&/&amp;/g;
    $line =~ s/</&lt;/g;
    $line =~ s/>/&gt;/g;
    $line =~ s%((http|ftp)://[^\s"\)\>,;]+)%<A HREF="$1">$1</A>%gi;
    $line =~ s%(\WPR[:s# \t]+)([a-z0-9-]+\/)?([0-9]+)%$1<A HREF="$scriptname?pr=$3">$2$3</A>%ig; 

    if ($cvsweb) {
	return &srcref($line);
    } else {
	return $line;
    }
}
