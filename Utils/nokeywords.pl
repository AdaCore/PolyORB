#! /usr/local/bin/perl -pi.bak
#
# $Id$
#
# This programs removes all the RCS keywords on files given on the command
# line. A typical use with zsh is:
#
#   Utils/nokeywords.pl **/*.ad[bs]
#
# Original files are renamed with .bak extensions.
#

s/(\$)Rev[i]sion.*(\$)/\1Revision\2/g
