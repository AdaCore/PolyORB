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
with CORBA.Object.Helper;
with CORBA.Object;

package body mod1.Int1.Helper is

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return mod1.Int1.Ref
   is
      Result : mod1.Int1.Ref;
   begin
      Set (Result,
           CORBA.Object.Object_Of (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return mod1.Int1.Ref
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
      return mod1.Int1.Ref is
   begin
      return To_Ref (CORBA.Object.Helper.From_Any (Item));
   end From_Any;

   function To_Any
     (Item : in mod1.Int1.Ref)
     return CORBA.Any is
      A : CORBA.Any := CORBA.Object.Helper.To_Any
        (CORBA.Object.Ref (Item));
   begin
      CORBA.Set_Type (A, TC_Int1);
      return A;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return mod1.Int1.New_Float is
      Result : constant CORBA.Float := CORBA.From_Any (Item);
   begin
      return mod1.Int1.New_Float (Result);
   end From_Any;

   function To_Any
     (Item : in mod1.Int1.New_Float)
     return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Float (Item));
   begin
      CORBA.Set_Type (Result, TC_New_Float);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return mod1.Int1.Color is
      Index : CORBA.Any :=
         CORBA.Get_Aggregate_Element (Item,
                                      CORBA.TC_Unsigned_Long,
                                      CORBA.Unsigned_Long (0));
      Position : constant CORBA.Unsigned_Long := CORBA.From_Any (Index);
   begin
      return Color'Val (Position);
   end From_Any;

   function To_Any
     (Item : in mod1.Int1.Color)
     return CORBA.Any is
      Result : CORBA.Any :=
         CORBA.Get_Empty_Any_Aggregate (TC_Color);
   begin
      CORBA.Add_Aggregate_Element
         (Result,
          CORBA.To_Any (CORBA.Unsigned_Long (Color'Pos (Item))));
      return Result;
   end To_Any;
   procedure Deferred_Initialization is
   begin
      null;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("Int1");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:mod1/Int1:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_Int1, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Int1, CORBA.To_Any (Id));
      end;
   
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("New_Float");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:mod1/Int1/New_Float:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_New_Float, CORBA.To_Any (CORBA.TC_Float));
      end;
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("Color");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:mod1/Int1/Color:1.0");
         Red_Name : CORBA.String := CORBA.To_CORBA_String ("Red");
         Blue_Name : CORBA.String := CORBA.To_CORBA_String ("Blue");
         Green_Name : CORBA.String := CORBA.To_CORBA_String ("Green");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_Color, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Color, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Color, CORBA.To_Any (Red_Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Color, CORBA.To_Any (Blue_Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_Color, CORBA.To_Any (Green_Name));
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
         (Name      => +"mod1.Int1.Helper",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end mod1.Int1.Helper;
