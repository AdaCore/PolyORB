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
      return i1.new_string
   is
      Result : constant CORBA.String := CORBA.From_Any (Item);
   begin
      return i1.new_string (Result);
   end From_Any;

   function To_Any
     (Item : in i1.new_string)
     return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.String (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_new_string);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.New_Float
   is
      Result : constant CORBA.Float := CORBA.From_Any (Item);
   begin
      return i1.New_Float (Result);
   end From_Any;

   function To_Any
     (Item : in i1.New_Float)
     return CORBA.Any
   is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Float (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_New_Float);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.n2
   is
      Result : constant i1.new_string := i1.Helper.From_Any (Item);
   begin
      return i1.n2 (Result);
   end From_Any;

   function To_Any
     (Item : in i1.n2)
     return CORBA.Any
   is
      Result : CORBA.Any := i1.Helper.To_Any (i1.new_string (Item));
   begin
      CORBA.Internals.Set_Type (Result, TC_n2);
      return Result;
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.tab
   is
      Result : i1.tab;
      Aux    : array (Natural range 0 .. 1) of CORBA.Any;

   begin
      Aux (0) := Item;

      for J_Ü0 in 0 .. 4 - 1 loop
         Aux (1) :=
           CORBA.Internals.Get_Aggregate_Element
           (Aux (0),
            TC_tab_TC_Dimension_1,
            CORBA.Unsigned_Long (J_Ü0));

         for J_Ü1 in 0 .. 2 - 1 loop
            Result (J_Ü0, J_Ü1) :=
              i1.Helper.From_Any
              (CORBA.Internals.Get_Aggregate_Element
               (Aux (1),
                i1.Helper.TC_new_string,
                CORBA.Unsigned_Long (J_Ü1)));
         end loop;
      end loop;

      return Result;
   end From_Any;

   function To_Any
     (Item : in i1.tab)
     return CORBA.Any
   is
      Result : array (Natural range 0 .. 1) of CORBA.Any;

   begin
      Result (0) :=
        CORBA.Internals.Get_Empty_Any_Aggregate
        (TC_tab);

      for J_Ü0 in 0 .. 4 - 1 loop
         Result (1) :=
           CORBA.Internals.Get_Empty_Any_Aggregate
           (TC_tab_TC_Dimension_1);

         for J_Ü1 in 0 .. 2 - 1 loop
            CORBA.Internals.Add_Aggregate_Element
              (Result (1),
               i1.Helper.To_Any (Item (J_Ü0, J_Ü1)));
         end loop;

         CORBA.Internals.Add_Aggregate_Element (Result (0), Result (1));
      end loop;

      return Result (0);
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.f33
   is
      Result : i1.f33;
      Aux    : array (Natural range 0 .. 0) of CORBA.Any;

   begin
      Aux (0) := Item;

      for J_Ü0 in 0 .. 120 - 1 loop
         Result (J_Ü0) :=
           CORBA.From_Any
           (CORBA.Internals.Get_Aggregate_Element
            (Aux (0),
             CORBA.TC_Float,
             CORBA.Unsigned_Long (J_Ü0)));
      end loop;

      return Result;
   end From_Any;

   function To_Any
     (Item : in i1.f33)
     return CORBA.Any
   is
      Result : array (Natural range 0 .. 0) of CORBA.Any;

   begin
      Result (0) :=
        CORBA.Internals.Get_Empty_Any_Aggregate
        (TC_f33);

      for J_Ü0 in 0 .. 120 - 1 loop
         CORBA.Internals.Add_Aggregate_Element
           (Result (0),
            CORBA.To_Any (Item (J_Ü0)));
      end loop;

      return Result (0);
   end To_Any;

   function From_Any (Item : in CORBA.Any)
      return i1.f45
   is
      Result : i1.f45;
      Aux    : array (Natural range 0 .. 2) of CORBA.Any;

   begin
      Aux (0) := Item;

      for J_Ü0 in 0 .. 4 - 1 loop
         Aux (1) :=
           CORBA.Internals.Get_Aggregate_Element
           (Aux (0),
            TC_f45_TC_Dimension_1,
            CORBA.Unsigned_Long (J_Ü0));

         for J_Ü1 in 0 .. 5 - 1 loop
            Aux (2) :=
              CORBA.Internals.Get_Aggregate_Element
              (Aux (1),
               TC_f45_TC_Dimension_2,
               CORBA.Unsigned_Long (J_Ü1));

            for J_Ü2 in 0 .. 6 - 1 loop
               Result (J_Ü0, J_Ü1, J_Ü2) :=
                 CORBA.From_Any
                 (CORBA.Internals.Get_Aggregate_Element
                  (Aux (2),
                   CORBA.TC_Float,
                   CORBA.Unsigned_Long (J_Ü2)));
            end loop;
         end loop;
      end loop;

      return Result;
   end From_Any;

   function To_Any
     (Item : in i1.f45)
     return CORBA.Any
   is
      Result : array (Natural range 0 .. 2) of CORBA.Any;

   begin
      Result (0) :=
        CORBA.Internals.Get_Empty_Any_Aggregate
        (TC_f45);

      for J_Ü0 in 0 .. 4 - 1 loop
         Result (1) :=
           CORBA.Internals.Get_Empty_Any_Aggregate
           (TC_f45_TC_Dimension_1);

         for J_Ü1 in 0 .. 5 - 1 loop
            Result (2) :=
              CORBA.Internals.Get_Empty_Any_Aggregate
              (TC_f45_TC_Dimension_2);

            for J_Ü2 in 0 .. 6 - 1 loop
               CORBA.Internals.Add_Aggregate_Element
                 (Result (2),
                  CORBA.To_Any (Item (J_Ü0, J_Ü1, J_Ü2)));
            end loop;

            CORBA.Internals.Add_Aggregate_Element (Result (1), Result (2));
         end loop;

         CORBA.Internals.Add_Aggregate_Element (Result (0), Result (1));
      end loop;

      return Result (0);
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
         Name : CORBA.String := CORBA.To_CORBA_String ("new_string");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:i1/new_string:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_string, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_string, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_new_string, CORBA.To_Any (CORBA.TC_String));
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
         Name : CORBA.String := CORBA.To_CORBA_String ("n2");
         Id : CORBA.String := CORBA.To_CORBA_String ("IDL:i1/n2:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_n2, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_n2, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter (TC_n2, CORBA.To_Any (i1.Helper.TC_new_string));
      end;
   
      declare
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_tab_TC_Dimension_1, CORBA.To_Any (CORBA.Unsigned_Long (2)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_tab_TC_Dimension_1, CORBA.To_Any (i1.Helper.TC_new_string));
         CORBA.TypeCode.Internals.Add_Parameter (TC_tab, CORBA.To_Any (CORBA.Unsigned_Long (4)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_tab, CORBA.To_Any (TC_tab_TC_Dimension_1));
      end;
   
      declare
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_f33, CORBA.To_Any (CORBA.Unsigned_Long (120)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f33, CORBA.To_Any (CORBA.TC_Float));
      end;
   
      declare
      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45_TC_Dimension_2, CORBA.To_Any (CORBA.Unsigned_Long (6)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45_TC_Dimension_2, CORBA.To_Any (CORBA.TC_Float));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45_TC_Dimension_1, CORBA.To_Any (CORBA.Unsigned_Long (5)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45_TC_Dimension_1, CORBA.To_Any (TC_f45_TC_Dimension_2));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45, CORBA.To_Any (CORBA.Unsigned_Long (4)));
         CORBA.TypeCode.Internals.Add_Parameter (TC_f45, CORBA.To_Any (TC_f45_TC_Dimension_1));
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
