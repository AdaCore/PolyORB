#! /usr/bin/env python
#

import string, sys, re, os, glob, stat

# All dirs: check MANIFEST vs. files

def get_subdirs (dir):
  res = [dir]
  vars = {}
  for l in open (dir + "/Makefile.am", "r").readlines ():
    m = re.match ("^([A-Za-z_]*)\s*=\s*(.*)$", l)
    
    if m:
      if len (m.group (2)) > 0:
        vars[m.group (1)] = m.group (2)

      if m.group (1) != 'corba_dir' \
             and m.group (1) != 'dsa_dir' \
             and m.group (1) != 'giop_dir' \
             and m.group (1) != 'moma_dir' \
             and m.group (1) != 'soap_dir' \
             and m.group (1) != 'srp_dir' \
             and m.group (1) != 'SUBDIRS':
        continue

      dirs = map (lambda s, d=dir: d + "/" + s,
        string.split (m.group (2), ' '))
  
      for d in dirs:
        if re.match (dir + "/\.?$", d):
          continue

        if re.match (dir + "/\@", d):
          continue

        d = re.sub("\\$\\(([^)]*)\\)", lambda mm, v=vars: v[mm.group (1)], d)

        sub = get_subdirs (d)
        for dd in sub:
          if not (dd in res):
            res = res + [dd]
            
  return res

def read_MANIFEST (dir):
  MANIFEST = []
  for l in open ("MANIFEST", "r").readlines ():
    l = l[:-1]
    if re.match (dir + "/[^/]*$", l):
      MANIFEST.append (l)
      if l[-3:] == ".in":
        MANIFEST.append (l[:-3])
      
  return MANIFEST

def read_files (dir):
  l = []
  for f in glob.glob (dir + "/*"):
    if not re.search ("(\.(lo|o|ali|la)|~)$", f):
      mode = os.stat(f)[stat.ST_MODE]
      if stat.S_ISREG(mode):
        l.append(f)
  return l

# Additional checks for src/:
#  allsrc

alis_seen = []

def read_one_ali (alidir, file):
  
  try:
    f = open (alidir + "/" + file, "r")
  except:
    return []
  
  units = []
  for l in f.readlines ():
    if l[0] == 'U':
      units.append ("src/" + re.match ("^U\s*\S*\s*(\S*\.ad[bs])", l).group (1))
    elif l[0] == 'W':
      m = re.match ("^W\s\S*\s*\S*\s*(\S*\.ali)", l)
      if m:
        ali = m.group (1)
        try:
          i = alis_seen.index (ali)
        except:
          alis_seen.append (ali)
          units = units + read_one_ali (alidir, ali)
  return units

def read_allsrc (alidir):
  return read_one_ali (alidir, "allsrc.ali")

def compare_lists (l1, l2, reverse):
  ll1 = eval (l1)[:]
  ll2 = eval (l2)[:]

  not_in_ll1 = []
  for f in ll2:
    try:
      ll1.remove (f)
    except:
      not_in_ll1.append (f)
        
  if len (ll1) > 0:
    print ("These are in " + l1 + " but not in " + l2 + ":")
    print ("  " + string.join (ll1, "\n  "))
    print ""
    
  if reverse and len (not_in_ll1) > 0:
    print ("These are in " + l2 + " but not in " + l1 + ":")
    print ("  " + string.join (not_in_ll1, "\n  "))
    print ""
    
if len (sys.argv) > 1:
  allsrc = read_allsrc (sys.argv[1])
  compare_lists ("files", "allsrc", 0)

subdirs = get_subdirs ("src")
for d in subdirs:
  print "Checking " + d + "/...\n"
  
  MANIFEST = read_MANIFEST (d)
  files = read_files (d)

  compare_lists ("files", "MANIFEST", 1)

subdirs = get_subdirs ("examples") + get_subdirs ("cos") + get_subdirs ("idls")

for d in subdirs:
  print "Checking " + d + "/...\n"
  
  MANIFEST = read_MANIFEST (d)
  files = read_files (d)

  compare_lists ("files", "MANIFEST", 1)
