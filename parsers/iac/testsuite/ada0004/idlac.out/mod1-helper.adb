-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
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

package body mod1.Helper is

   function From_Any (Item : in CORBA.Any)
      return mod1.bool is
      Result : constant CORBA.Boolean := CORBA.From_Any (Item);
   begin
      return mod1.bool (Result);
   end From_Any;

   function To_Any
     (Item : in mod1.bool)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Boolean (Item));
   begin
      CORBA.Set_Type (Result, TC_bool);
      return Result;
   end To_Any;
   procedure Deferred_Initialization is
   begin
      null;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("bool");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:mod1/bool:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_bool, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_bool, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_bool, CORBA.To_Any (CORBA.TC_Boolean));
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
         (Name      => +"mod1.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end mod1.Helper;
