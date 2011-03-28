#! /usr/local/bin/perl -pi.bak
#
# $Id$
#
# This programs removes all the ending -- on files given on the command
# line. A typical use with zsh is:
#
#   Utils/nokeywords.pl **/*.ad[bs]
#
# Original files are renamed with .bak extensions.
#

s/^(--\s\s+\$Rev[i]sion)(: [\d\.]+\s)?\$.*/\1\$/g

