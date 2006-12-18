#! /bin/ksh

# run_cpp
# $Id$

# This optional wrapper can be used when no C++ preprocessor is available
# to fall back on a standard UNIX C preprocessor producing output on
# stdout only.

while [ $# -gt 0 ]; do
  case "$1" in
    -o)
      shift; redir="> \"$1\""; shift ;;
    -I)
      shift; args="$args \"-I$1\""; shift ;;
    *)
      args="$args \"$1\""; shift ;;
  esac
done
eval "/lib/cpp $args $redir"
