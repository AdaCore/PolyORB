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
with CORBA.Object.Helper;
with CORBA.Object;

package body i1.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return i1.Ref
   is
      Result : i1.Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return i1.Ref
   is
      use CORBA;
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id) then
         return Unchecked_To_Ref (The_Ref);
      end if;
      CORBA.Raise_Bad_Param (Default_Sys_Member);
   end To_Ref;

   function From_Any (Item : in CORBA.Any)
      return i1.Ref is
   begin
      return To_Ref (CORBA.Object.Helper.From_Any (Item));
   end From_Any;

   function To_Any
     (Item : in i1.Ref)
     return CORBA.Any is
      A : CORBA.Any := CORBA.Object.Helper.To_Any
        (CORBA.Object.Ref (Item));
   begin
      CORBA.Internals.Set_Type (A, TC_i1);
      return A;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.New_Float is
      Result : constant CORBA.Float := CORBA.From_Any (Item);
   begin
      return i1.New_Float (Result);
   end From_Any;

   function To_Any
     (Item : in i1.New_Float)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Float (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_New_Float);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.Tab_Float is
      Result : i1.Tab_Float;
      use type CORBA.Unsigned_Long;
      J_Ü : CORBA.Unsigned_Long := 0;
   begin
      for J_Ü0 in 0 .. 10 - 1 loop
         Result (J_Ü0) := CORBA.From_Any
            (CORBA.Internals.Get_Aggregate_Element
             (Item, CORBA.TC_Float, J_Ü));
         J_Ü := J_Ü + 1;
      end loop;
      return Result;
   end From_Any;

   function To_Any
     (Item : in i1.Tab_Float)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.Internals.Get_Empty_Any_Aggregate
        (TC_Tab_Float);
   begin
      for J_Ü0 in 0 .. 10 - 1 loop
         CORBA.Internals.Add_Aggregate_Element (Result,
                                      CORBA.To_Any (Item (J_Ü0)));
      end loop;
      return Result;
   end To_Any;
   
   procedure Deferred_Initialization is
   begin
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("i1");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:i1:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_i1, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_i1, CORBA.To_Any (Id));
      end;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("New_Float");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:i1/New_Float:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (CORBA.TC_Float));
      end;
   
      declare
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_Tab_Float, CORBA.To_Any (CORBA.Unsigned_Long (10)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Tab_Float, CORBA.To_Any (CORBA.TC_Float));
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
         (Name      => +"i1.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end i1.Helper;
