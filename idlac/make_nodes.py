#! /usr/bin/env python
#

import string, sys

class Node:

    nodes = {}

    def __init__ (self, name, parent = None):
        self.name = name
        if parent: self.parent = parent
        elif name == "Root": self.parent = None
        else: self.parent = "Root"
        self.fields = []
        Node.nodes [name] = self

    def add_field (self, name, type):
        self.fields.append ((name, type))

def get_words (l):
    return string.split (l [2:-1])

def get_fields (n):
    if n.parent: return get_fields (Node.nodes [n.parent]) + n.fields
    else: return n.fields

def append (kind, name, type):
    if fields.has_key (name):
        if typess [name] != type:
            sys.stderr.write ("Conflicting types for field %s\n" % name)
            sys.exit (1)
        fields [name].append (kind)
    else:
        fields [name] = [kind]
        typess [name] = type

fields = {}
typess = {}

current = None
started = 0

input = open ("nodes.txt", "r").readlines()
for i in input:
    words = get_words (i)
    if not words: continue
    if not started:
        if words == ['START']:
            started = 1
            continue
        continue
    if words == ['END']: break
    if len (words) == 3 and words [1] == ':':
        current.add_field (words [0], words [2])
    elif len (words) > 0:
        if len (words) == 3 and words [1] == '<--':
            current = Node (words [0], words [2])
        else:
            current = Node (words [0])
    else:
        sys.stderr.write ("Unknown line: %s\n" % i)
        sys.exit (1)

nodes = Node.nodes.keys()
nodes.sort ()

types = []
spec = []
body = []

for n in nodes:
    if n == "Root": continue
    i = Node.nodes [n]
    types.append ("K_%s" % n)
    spec.append ("   --")
    spec.append ("   --  %s" % n)
    spec.append ("   --")
    for (name, type) in get_fields (i):
        append (n, name, type)
        spec.append ("   --     %-25s : %s" % (name, type))
    spec.append ("   --")
    spec.append ("")
    spec.append ("   function Make_%s return Node_Id;" % n)
    spec.append ("")
    body.append ("   function Make_%s return Node_Id is" % n)
    body.append ("      Node  : constant Node_Access := new Node_Type;")
    body.append ("      Index : constant Node_Id     := Allocate;")
    body.append ("   begin")
    body.append ("      Node.Kind := K_%s;" % n)
    body.append ("      Table (Index) := Node;")
    body.append ("      return Index;")
    body.append ("   end Make_%s;" % n)
    body.append ("")

f = fields.keys ()
f.sort ()

for name in f:
    type = typess [name]
    kinds = fields [name]
    spec.append ("   function %s (N : Node_Id) return %s;" % (name, type))
    spec.append ("   procedure Set_%s (N : Node_Id; V : %s);" % (name, type))
    spec.append ("")
    body.append ("   function %s (N : Node_Id) return %s is" % (name, type))
    body.append ("      Node : constant Node_Access := Retrieve_Node (N);")
    body.append ("   begin")
    for k in range (len (kinds)):
        if k == 0:
            if len (kinds) == 1:
                assrt = ["      pragma Assert (Node.Kind = %s);" % kinds [k]]
            else:
                assrt = ["      pragma Assert (Node.Kind = %s" %
                         kinds [k]]
        else:
            if k == len (kinds) - 1: expr = ");"
            else: expr = ""
            assrt.append ("             or else Node.Kind = %s%s" %
                          (kinds [k], expr))
    body = body + assrt
    body.append ("      return Node.%s;" % name)
    body.append ("   end %s;" % name)
    body.append ("")
    body.append ("   procedure Set_%s (N : Node_Id; V : %s)" % (name, type))
    body.append ("   is")
    body.append ("      Node : constant Node_Access := Retrieve_Node (N);")
    body.append ("   begin")
    body = body + assrt
    body.append ("      Node.%s := V;" % name)
    body.append ("   end Set_%s;" % name)
    body.append ("")

print """with GNAT.Table;
with Nodes;      use Nodes;
with Types;      use Types;

package Nodes_Access is

   type Node_Kind is"""
for n in range (len (types)):
    if n == 0: s = '('
    else: s = ' '
    if n == len (types) - 1: e = ');'
    else: e = ','
    print "      %s%s%s" % (s, types [n], e)
print
for i in spec:
    print i
print ""
print "private"
print
print "   type Node_Type is limited record"
for i in f:
    if typess [i] == "Node_Id": init = ":= No_Node"
    else: init = ""
    print "      %-25s : %-25s%s;" % (i, typess [i], init)
print "   end record;"
print """
   type Node_Access is access Node_Type;

   package Nodes_Table is
      new GNAT.Table (Table_Component_Type => Node_Access,
                      Table_Index_Type     => Node_Id,
                      Table_Low_Bound      => 1,
                      Table_Initial        => 1024,
                      Table_Increment      => 100);

end Nodes_Access;"""

print "package body Nodes_Access is"
print
print "   use Nodes_Table;"
print
for i in body:
    print i
print "end Nodes_Access;"
