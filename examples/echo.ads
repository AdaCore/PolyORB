----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA;
with CORBA.Object;

package Echo is

   type Ref is new CORBA.Object.Ref with null record;

   function echoString
     (Self : in Ref;
      Mesg : in CORBA.String)
     return CORBA.String;

   Repository_Id : constant Standard.String
     := "IDL:Echo:1.0";

end Echo;
