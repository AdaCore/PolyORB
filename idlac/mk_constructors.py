#! /usr/bin/python

from sys import *
from re import match

state = 0
acc = {}
ins = open ("idl_fe-tree.ads", "r")
outs = open ("idl_fe-tree-constructors.ads", "w")
outb = open ("idl_fe-tree-constructors.adb", "w")

outs.write ("""with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;

package Idl_Fe.Tree.Constructors is

""")

outb.write ("package body Idl_Fe.Tree.Constructors is\n")

while 1:
  line = ins.readline ()
  if line == "":
    break
  if match ("[ 	]*--", line):
    continue

  line = line[:-1]

  m = match (".*type N_(.*) is new .*N_.* with record", line)
  if m:
    name = m.group(1)
    outs.writelines (["   function new", name , " (Node : N_Root_Acc) return N_", name, "_Acc;\n"])
    outb.writelines (["   function new", name , " (Node : N_Root_Acc) return N_", name, "_Acc is\n"])
    outb.write      ( "     Result : N_", name, "_Acc := new N_", name, ";\n") 
    outb.write      ( "   begin\n")
    outb.write      ( "     Set_Old (Result, Node);\n")
    outb.write      ( "   end new", name, ";\n\n")
    continue

outs.write ("end Idl_Fe.Tree.Constructors;\n")
outb.write ("end Idl_Fe.Tree.Constructors;\n")
ins.close()
outs.close()
outb.close()
