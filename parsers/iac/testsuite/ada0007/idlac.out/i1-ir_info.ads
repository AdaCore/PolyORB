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

package i1.IR_Info is

   procedure Register_IR_Info;

   function IR_New_Float
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_Tab_Float
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_val1
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_val2
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_tab_val
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_tab
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_i1
     return CORBA.Repository_Root.IRObject.Ref;

end i1.IR_Info;
