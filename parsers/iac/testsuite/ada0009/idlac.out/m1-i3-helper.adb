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
with m1.i1.Helper;
with CORBA;
with CORBA.Object.Helper;
with CORBA.Object;

package body m1.i3.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return m1.i3.Ref
   is
      Result : m1.i3.Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return m1.i3.Ref
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
      return m1.i3.Ref is
   begin
      return To_Ref (CORBA.Object.Helper.From_Any (Item));
   end From_Any;

   function To_Any
     (Item : in m1.i3.Ref)
     return CORBA.Any is
      A : CORBA.Any := CORBA.Object.Helper.To_Any
        (CORBA.Object.Ref (Item));
   begin
      CORBA.Internals.Set_Type (A, TC_i3);
      return A;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return m1.i3.new_float is
      Result : constant m1.i1.t1 := m1.i1.Helper.From_Any (Item);
   begin
      return m1.i3.new_float (Result);
   end From_Any;

   function To_Any
     (Item : in m1.i3.new_float)
     return CORBA.Any is
      Result : CORBA.Any := m1.i1.Helper.To_Any (m1.i1.t1 (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_new_float);
      return Result;
   end To_Any;
   
   procedure Deferred_Initialization is
   begin
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("i3");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:m1/i3:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_i3, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_i3, CORBA.To_Any (Id));
      end;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("new_float");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:m1/i3/new_float:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_float, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_float, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_float, CORBA.To_Any (m1.i1.Helper.TC_t1));
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
         (Name      => +"m1.i3.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
                  & "m1.i1.Helper"
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end m1.i3.Helper;
