#! /usr/bin/env python

import getopt, os, shutil, string, sys

def usage():
    print "Usage: install.py [-m MODE] SRCFILE... DSTDIR"
    sys.exit(2)

def main():
    dir = False
    mode = 0444
    strip = False
    verbose = False

    try:
        opts, args = getopt.getopt(sys.argv[1:], "dm:sv")
    except getopt.GetoptError, err:
        # print help information and exit:
        print str(err) # will print something like "option -a not recognized"
        usage()

    for o, a in opts:
        if o == "-d":
            dir = True
        elif o == "-m":
            mode = string.atoi(a, 8)
        elif o == "-s":
            strip = True
        elif o == "-v":
            verbose = True
        else:
            assert False, "unhandled option"

    if len(args) == 0:
        usage()

    if not dir:
        dst = args.pop()

    for file in args:
        if file == '-':
            args.extend(map(string.rstrip, sys.stdin.readlines()))

        elif dir:
            if not os.path.isdir(file):
                os.makedirs(file)

        else:
            if os.path.isdir(dst):
                dstfile = os.path.join(dst,os.path.basename(file))
            else:
                dstfile = dst

            if verbose:
                print "%s -> %s" % (file, dstfile)

            if os.path.exists(dstfile):
                # Before doing the unlink, ensure that we have write access to
                # the file (required on Windows).
                os.chmod(dstfile, 0600)
                os.unlink(dstfile)
            shutil.copy2(file, dstfile)
            os.chmod(dstfile, mode)
            if strip:
                os.spawn(os.P_WAIT, "strip", dstfile)

if __name__ == "__main__":
    main()
