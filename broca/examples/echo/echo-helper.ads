----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with CORBA.Object;

package Echo.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Echo.Ref;
   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Echo.Ref;

end Echo.Helper;
