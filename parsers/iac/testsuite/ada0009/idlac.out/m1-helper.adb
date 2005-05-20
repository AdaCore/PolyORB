-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with CORBA;

package body m1.Helper is

   function From_Any (Item : in CORBA.Any)
      return m1.b1 is
      Result : constant CORBA.Boolean := CORBA.From_Any (Item);
   begin
      return m1.b1 (Result);
   end From_Any;

   function To_Any
     (Item : in m1.b1)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Boolean (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_b1);
      return Result;
   end To_Any;
   
   procedure Deferred_Initialization is
   begin
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("b1");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:m1/b1:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_b1, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_b1, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_b1, CORBA.To_Any (CORBA.TC_Boolean));
      end;
   
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"m1.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end m1.Helper;
