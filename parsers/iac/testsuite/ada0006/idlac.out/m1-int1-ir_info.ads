-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with CORBA.Repository_Root.IRObject;

package m1.int1.IR_Info is

   procedure Register_IR_Info;

   function IR_attr1
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_bool1
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_int1
     return CORBA.Repository_Root.IRObject.Ref;

end m1.int1.IR_Info;
