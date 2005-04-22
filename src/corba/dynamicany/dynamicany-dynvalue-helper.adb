
with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body DynamicAny.DynValue.Helper is

   procedure Deferred_Initialization;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization is
   begin
      declare
         Name : constant CORBA.String := CORBA.To_CORBA_String ("DynValue");
         Id   : constant CORBA.String
           := CORBA.To_CORBA_String ("IDL:omg.org/DynamicAny/DynValue:1.0");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynValue, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_DynValue, CORBA.To_Any (Id));
      end;
   end Deferred_Initialization;

   ------------------
   -- To_Local_Ref --
   ------------------

   function To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynValue.Local_Ref
   is
   begin
      if CORBA.Object.Is_Nil (The_Ref)
        or else CORBA.Object.Is_A (The_Ref, Repository_Id)
      then
         return Unchecked_To_Local_Ref (The_Ref);
      end if;

      CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
   end To_Local_Ref;

   ----------------------------
   -- Unchecked_To_Local_Ref --
   ----------------------------

   function Unchecked_To_Local_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
      return DynamicAny.DynValue.Local_Ref
   is
      Result : DynamicAny.DynValue.Local_Ref;

   begin
      Set (Result, CORBA.Object.Object_Of (The_Ref));

      return Result;
   end Unchecked_To_Local_Ref;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"DynamicAny.DynValue.Helper",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end DynamicAny.DynValue.Helper;