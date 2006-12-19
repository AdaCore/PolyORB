with CORBA.Object;
with CORBA;
with CORBA.Object.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Harness.Helper is

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id)
      then
         return Unchecked_To_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Ref;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref
   is
      Result : Harness.Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function From_Any
     (Item : in CORBA.Any)
     return Harness.Ref
   is
   begin
      return To_Ref
        (CORBA.Object.Helper.From_Any
           (Item));
   end From_Any;

   function To_Any
     (Item : in Harness.Ref)
     return CORBA.Any
   is
      A : CORBA.Any :=
        CORBA.Object.Helper.To_Any
           (CORBA.Object.Ref
              (Item));
   begin
      CORBA.Set_Type
        (A,
         TC_Harness);
      return A;
   end To_Any;

   procedure Deferred_Initialization

   is
   begin
      declare
         Name_ü : CORBA.String :=
           CORBA.To_CORBA_String
              ("Harness");
         Id_ü : CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:Harness:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Harness,
            CORBA.To_Any
              (Name_ü));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Harness,
            CORBA.To_Any
              (Id_ü));
      end;
   end Deferred_Initialization;

begin
   PolyORB.Initialization.Register_Module
     (PolyORB.Initialization.Module_Info'
        (Name => PolyORB.Utils.Strings."+"
           ("Harness.Helper"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end Harness.Helper;
