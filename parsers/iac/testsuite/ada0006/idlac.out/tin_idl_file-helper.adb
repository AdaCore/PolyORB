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

package body tin_IDL_File.Helper is

   function From_Any (Item : in CORBA.Any)
      return tin_IDL_File.New_Float is
      Result : constant CORBA.Float := CORBA.From_Any (Item);
   begin
      return tin_IDL_File.New_Float (Result);
   end From_Any;

   function To_Any
     (Item : in tin_IDL_File.New_Float)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Float (Item));
   begin
      CORBA.Set_Type (Result, TC_New_Float);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return tin_IDL_File.New_Boolean is
      Result : constant CORBA.Boolean := CORBA.From_Any (Item);
   begin
      return tin_IDL_File.New_Boolean (Result);
   end From_Any;

   function To_Any
     (Item : in tin_IDL_File.New_Boolean)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Boolean (Item));
   begin
      CORBA.Set_Type (Result, TC_New_Boolean);
      return Result;
   end To_Any;
   procedure Deferred_Initialization is
   begin
      null;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("New_Float");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:New_Float:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (CORBA.TC_Float));
      end;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("New_Boolean");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:New_Boolean:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Boolean, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Boolean, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Boolean, CORBA.To_Any (CORBA.TC_Boolean));
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
         (Name      => +"tin_IDL_File.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end tin_IDL_File.Helper;
