#! /usr/bin/env python

import getopt
import os
import shutil
import string
import sys


def usage():
    print ("Usage: install.py [-m MODE] [-R] SRCFILE... DSTDIR")
    sys.exit(2)


def ensure_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)

def _rstrip(line):
    return line.strip()

def main():
    dir = False
    mode = 0o444
    strip = False
    verbose = False
    preserve_relative_path = False

    try:
        opts, args = getopt.getopt(sys.argv[1:], "dm:svR")
    except getopt.GetoptError as err:
        # print help information and exit:
        print (str(err))  # will print something like "option -a not recognized"
        usage()

    for o, a in opts:
        if o == "-d":
            dir = True
        elif o == "-m":
            mode = int(a, 8)
        elif o == "-s":
            strip = True
        elif o == "-v":
            verbose = True
        elif o == "-R":
            preserve_relative_path = True
        else:
            assert False, "unhandled option"

    if len(args) == 0:
        usage()

    if not dir:
        dst = args.pop()

    for file in args:
        if file == '-':
            args.extend(map(_rstrip, sys.stdin.readlines()))

        elif dir:
            ensure_dir(file)

        else:
            if os.path.isdir(dst):
                if preserve_relative_path:
                    dstfile = file
                else:
                    dstfile = os.path.basename(file)
                dstfile = os.path.join(dst, dstfile)
                if preserve_relative_path:
                    ensure_dir(os.path.dirname(dstfile))
            else:
                dstfile = dst

            if verbose:
                print ("%s -> %s" % (file, dstfile))

            if os.path.exists(dstfile):
                # Before doing the unlink, ensure that we have write access to
                # the file (required on Windows).
                os.chmod(dstfile, 0o600)
                os.unlink(dstfile)
            shutil.copy2(file, dstfile)
            os.chmod(dstfile, mode)
            if strip:
                os.spawn(os.P_WAIT, "strip", dstfile)

if __name__ == "__main__":
    main()
