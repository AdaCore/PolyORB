#! /usr/bin/python

from sys import *
from re import match

state = 0
acc = {}
ins = open ("idl_fe-tree.ads", "r")
outs = open ("idl_fe-tree-accessors.ads", "w")
outb = open ("idl_fe-tree-accessors.adb", "w")

outs.write ("""with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;

package Idl_Fe.Tree.Accessors is

   Kind_Error : exception;

""")
outb.write ("package body Idl_Fe.Tree.Accessors is\n")

while 1:
  line = ins.readline ()
  if line == "":
    break
  if match ("[ 	]*--", line):
    continue

  line = line[:-1]

  m = match (".*type N_(.*) is new .*N_.* with record", line)
  if m:
    state = 1
    node = m.group(1)
    continue

  if not state:
    continue

  if match (".*end record", line):
    state = 0
    continue

  m = match ("[^a-zA-Z]*([a-zA-Z_]*) : ([a-zA-Z_.]*)", line)
  if m:
    if not acc.has_key (m.group(1)):
      acc[m.group(1)] = {}
    if not acc[m.group(1)].has_key (m.group(2)):
      acc[m.group(1)][m.group(2)] = ()
    acc[m.group(1)][m.group(2)] = (node,) + acc[m.group(1)][m.group(2)];

for (func, th) in acc.items():
  for (type, nodelist) in th.items():
    outs.writelines (["   function ", func , " (Node : N_Root_Acc) return ", type, ";\n"])
    outb.writelines (["   function ", func , " (Node : N_Root_Acc) return ", type, " is\n"])
    outb.write      ("   begin\n")
    outb.write      ("      case Get_Kind (Node.all) is\n")

    for nodekind in nodelist:
      outb.writelines (["         when K_", nodekind, " =>\n"])
      outb.writelines (["            return N_", nodekind, "_Acc (Node).", func, ";\n"])

    outb.writelines (["""         when others =>
            raise Kind_Error;
      end case;
   end """, func, ";\n\n"])

outs.write ("end Idl_Fe.Tree.Accessors;\n")
outb.write ("end Idl_Fe.Tree.Accessors;\n")
ins.close()
outs.close()
outb.close()







