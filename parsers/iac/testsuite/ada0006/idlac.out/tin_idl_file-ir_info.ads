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

package tin_IDL_File.IR_Info is

   procedure Register_IR_Info;

   function IR_New_Float
     return CORBA.Repository_Root.IRObject.Ref;

   function IR_New_Boolean
     return CORBA.Repository_Root.IRObject.Ref;

end tin_IDL_File.IR_Info;
