#! /usr/bin/env python
#

import string, sys, re, os

def read_MANIFEST ():
  MANIFEST = []
  for l in open ("MANIFEST", "r").readlines ():
    m = re.match ("^src/(.*\.ad[sb])$", l)
    if m:
      MANIFEST.append (m.group (1))
  return MANIFEST

def read_Makefile ():
  Makefile = []
  st = 0
  for l in open ("src/Makefile.am", "r").readlines ():
    if re.match ("^ADA_SPECS_WITH_BODY =", l):
      st = 2
      continue
    if re.match ("^ADA_SPECS =", l):
      st = 1
      continue
    if re.match ("^$", l):
      st = 0
      continue
    if st > 0:
      m = re.match ("^\s*(\S*\.ad)[sb]", l)
      if m:
        Makefile.append (m.group (1) + 's')
        if st > 1:
          Makefile.append (m.group (1) + 'b')
          
  return Makefile
  
def read_files ():
  files = []
  for l in os.popen ("cd src && ls *.ad[sb]").readlines ():
    files.append (string.strip (l))
  return files

def compare_lists (l1, l2):
  ll1 = eval (l1)[:]
  ll2 = eval (l2)[:]

  not_in_ll1 = []
  for f in ll2:
    try:
      ll1.remove (f)
    except:
      not_in_ll1.append (f)
        
  if len (ll1) > 0:
    print ""
    print ("These are in " + l1 + " but not in " + l2 + ":")
    print ("  " + string.join (ll1, "\n  "))
    
  if len (not_in_ll1) > 0:
    print ""
    print ("These are in " + l2 + " but not in " + l1 + ":")
    print ("  " + string.join (not_in_ll1, "\n  "))
    

MANIFEST = read_MANIFEST ()
Makefile = read_Makefile ()
files = read_files ()

compare_lists ("files", "MANIFEST")
compare_lists ("files", "Makefile")
