with GNAT.OS_Lib; use GNAT.OS_Lib;

with Charset; use Charset;
with Namet;   use Namet;
with Utils;   use Utils;

with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.Runtime is

   RUD : array (RU_Id) of Node_Id;
   RED : array (RE_Id) of Node_Id;
   --  Arrays of run-time entity and unit designators

   type Casing_Rule is record
      Size : Natural;
      From : String_Access;
      Into : String_Access;
   end record;

   Rules : array (1 .. 64) of Casing_Rule;
   Rules_Last : Natural := 0;

   procedure Apply_Casing_Rules (S : in out String);
   procedure Register_Casing_Rule (S : String);
   procedure Declare_Subunit (N : Node_Id);

   ------------------------
   -- Apply_Casing_Rules --
   ------------------------

   procedure Apply_Casing_Rules (S : in out String) is
      New_Word : Boolean := True;
      Length   : Natural := S'Length;

   begin
      To_Lower (S);
      for I in S'Range loop
         if New_Word then
            New_Word := False;
            for J in 1 .. Rules_Last loop
               if Rules (J).Size <= Length
                 and then S (I .. I + Rules (J).Size - 1) = Rules (J).From.all
               then
                  S (I .. I + Rules (J).Size - 1) := Rules (J).Into.all;
               end if;
            end loop;
         end if;
         if S (I) = '_' then
            New_Word := True;
         end if;
         Length := Length - 1;
      end loop;
      Capitalize (S);
   end Apply_Casing_Rules;

   ---------------------
   -- Declare_Subunit --
   ---------------------

   procedure Declare_Subunit (N : Node_Id) is
      S : Node_Id;

   begin
      pragma Assert (Kind (N) = K_Designator);
      S := Corresponding_Node (Defining_Identifier (N));
      pragma Assert (Kind (S) = K_Package_Specification);
      Set_Is_Subunit_Package (S, True);
   end Declare_Subunit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Position   : Integer;
      Name       : Name_Id;
      Identifier : Node_Id;
      Length     : Natural;
      Pkg_Spec   : Node_Id;

   begin
      Register_Casing_Rule ("AbstractBase");
      Register_Casing_Rule ("ARG_INOUT");
      Register_Casing_Rule ("ARG_IN");
      Register_Casing_Rule ("ARG_OUT");
      Register_Casing_Rule ("CORBA");
      Register_Casing_Rule ("NamedValue");
      Register_Casing_Rule ("NVList");
      Register_Casing_Rule ("ORB");
      Register_Casing_Rule ("PolyORB");
      Register_Casing_Rule ("PortableServer");
      Register_Casing_Rule ("ServerRequest");
      Register_Casing_Rule ("TC_");
      Register_Casing_Rule ("TypeCode");
      Register_Casing_Rule ("ExceptionList");

      for U in RU_Id'Succ (RU_Id'First) .. RU_Id'Last loop
         Set_Str_To_Name_Buffer (RU_Id'Image (U));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));

         RUD (U) := New_Node (K_Designator);

         Position := 0;
         Length   := 0;
         Name     := Name_Find;
         Length   := Name_Len;
         Set_Name_Table_Info (Name, RU_Id'Pos (U));

         while Name_Len > 0 loop
            if Name_Buffer (Name_Len) = '_' then
               Name_Len := Name_Len - 1;
               Position := Integer (Get_Name_Table_Info (Name_Find));
               exit when Position > 0;

            else
               Name_Len := Name_Len - 1;
            end if;
         end loop;

         --  When there is a parent, remove parent unit name from
         --  unit name to get real identifier.

         if Position > 0 then
            Set_Str_To_Name_Buffer
              (Name_Buffer (Name_Len + 2 .. Length));
            Name := Name_Find;
            Set_Parent_Unit_Name (RUD (U), RUD (RU_Id'Val (Position)));
         end if;

         Get_Name_String (Name);
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));
         Identifier := Make_Defining_Identifier (Name_Find);
         Set_Defining_Identifier (RUD (U), Identifier);
         Pkg_Spec := New_Node (K_Package_Specification);
         Set_Is_Runtime_Package (Pkg_Spec, True);
         Set_Corresponding_Node (Identifier, Pkg_Spec);

         if Position > 0 then
            Set_Parent_Unit_Name
              (Identifier,
               Defining_Identifier (Parent_Unit_Name (RUD (U))));
         end if;

         Set_Name_Table_Info
           (To_Spec_Name (Fully_Qualified_Name (RUD (U))), Int (RUD (U)));
      end loop;

      Declare_Subunit (RUD (RU_CORBA_Internals));
      Declare_Subunit (RUD (RU_CORBA_TypeCode));
      Declare_Subunit (RUD (RU_CORBA_TypeCode_Internals));
      Declare_Subunit (RUD (RU_PolyORB_Any_TypeCode));
      Declare_Subunit (RUD (RU_PolyORB_Any_TypeCode_Internals));
      Declare_Subunit (RUD (RU_CORBA_Object_Internals));
      Declare_Subunit (RUD (RU_PortableServer_Internals));
      Declare_Subunit (RUD (RU_CORBA_ExceptionList_Internals));

      --  Package Standard is not a subunit but it has to be handled
      --  in a specific way as well as subunit.

      Declare_Subunit (RUD (RU_Standard));

      for E in RE_Id loop
         Set_Str_To_Name_Buffer (RE_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Apply_Casing_Rules (Name_Buffer (1 .. Name_Len));

         while Name_Buffer (Name_Len) in '0' .. '9'
           or else Name_Buffer (Name_Len) = '_'
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if E = RE_Add then
            Set_Str_To_Name_Buffer (Quoted ("+"));
         end if;

         Name := Name_Find;

         RED (E) := New_Node (K_Designator);
         Set_Defining_Identifier
           (RED (E), Make_Defining_Identifier (Name));
         Set_Parent_Unit_Name (RED (E), RUD (RE_Unit_Table (E)));
      end loop;
   end Initialize;

   --------
   -- RE --
   --------

   function RE (Id : RE_Id) return Node_Id is
   begin
      return Copy_Designator (RED (Id));
   end RE;

   --------------------------
   -- Register_Casing_Rule --
   --------------------------

   procedure Register_Casing_Rule (S : String) is
   begin
      Rules_Last := Rules_Last + 1;
      Rules (Rules_Last).Size := S'Length;
      Rules (Rules_Last).Into := new String'(S);
      Rules (Rules_Last).From := new String'(S);
      To_Lower (Rules (Rules_Last).From.all);
   end Register_Casing_Rule;

   --------
   -- RU --
   --------

   function RU (Id : RU_Id) return Node_Id is
   begin
      return Copy_Designator (RUD (Id));
   end RU;

end Backend.BE_Ada.Runtime;