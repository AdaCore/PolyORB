#! /usr/bin/env perl
#
# $Id$
#
# This package adjusts correctly any file given on the command
# line. If a line looks like: -- (blanks) $, Revision: NNN, $ (blanks) --
# then it will be centered again with the same number of column than
# the first line containing only dashes.
#

$temp = "/tmp/norm$$";

$| = 1;

foreach $i (@ARGV) {
  print "Checking $i...";
  $fl = 0;
  $modified = 0;
  open (file, $i);
  open (temp, ">$temp");
  while (<file>) {
    if (/^(-+)$/ && !$fl) {
      $fl = length($1);
      print " $fl columns...";
    }
    $save = $_;
    if (/^--\s+(\$Revi(d?)sion: \S+ \$)\s+--$/ &&
	(length($_) != ($fl + 1)) &&
	!$modified) {
      if (!$fl) {
	print " 78 columns (default)...";
	$fl = 78;
      }
      $rev = $1;
      $free = ($fl - 4) - length($rev);
      $left = $free / 2;
      $str = "--" . (" " x $left) . $rev . (" " x ($free-$left)) . "--\n";
      print temp $str;
      if (!($str eq $save)) {
	$modified++;
      }
    } else {
      print temp;
    }
  }
  if ($modified) {
    print " updated\n";
    unlink ($i);
    rename ($temp, $i);
  } else {
    print " unmodified\n";
    unlink ($temp);
  }
}
