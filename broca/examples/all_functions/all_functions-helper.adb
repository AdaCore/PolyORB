----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------

with Broca.Exceptions;
with Broca.Refs;

package body all_functions.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return all_functions.Ref
   is
      Result : all_functions.Ref;
   begin
      Set (Result,
           CORBA.Object.Get (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return all_functions.Ref
   is
      Result : all_functions.Ref;
   begin
      Result := Unchecked_To_Ref (The_Ref);
      if Is_A (Result, Repository_Id_Ü) then
         return Result;
      else
         Broca.Exceptions.Raise_Bad_Param;
      end if;
   end To_Ref;

end all_functions.Helper;
