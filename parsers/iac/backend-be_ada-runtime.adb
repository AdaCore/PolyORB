with Namet;   use Namet;
with Utils;   use Utils;

with Backend.BE_Ada.Nodes;  use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils; use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.Runtime is

   RUD : array (RU_Id) of Node_Id;
   RED : array (RE_Id) of Node_Id;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Position   : Integer;
      Identifier : Name_Id;
      Length     : Natural;
      procedure Special_Name (U : RU_Id; Str : String);
      procedure Special_Name (E : RE_Id; Str : String);

      ------------------
      -- Special_Name --
      ------------------

      procedure Special_Name (U : RU_Id; Str : String) is
      begin
         Set_Str_To_Name_Buffer (Str);
         Set_Name
           (Defining_Identifier (RUD (U)), Name_Find);
      end Special_Name;

      ------------------
      -- Special_Name --
      ------------------

      procedure Special_Name (E : RE_Id; Str : String) is
      begin
         Set_Str_To_Name_Buffer (Str);
         Set_Name
           (Defining_Identifier (RED (E)), Name_Find);
      end Special_Name;

   begin
      for U in RU_Id loop
         Set_Str_To_Name_Buffer (RU_Id'Image (U));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         if Name_Buffer (1 .. Name_Len) = "Null" then
            RUD (U) := No_Node;
         else
            Position   := 0;
            Length     := 0;
            Identifier := Name_Find;
            Length     := Name_Len;
            Set_Name_Table_Info (Identifier, RU_Id'Pos (U));

            while Name_Len > 0 loop
               if Name_Buffer (Name_Len) = '_' then
                  Name_Len := Name_Len - 1;
                  Position := Integer (Get_Name_Table_Info (Name_Find));
                  exit when Position > 0;
               else
                  Name_Len := Name_Len - 1;
               end if;
            end loop;

            RUD (U) := New_Node (K_Designator);
            if Position > 0 then
               Set_Str_To_Name_Buffer
                 (Name_Buffer (Name_Len + 2 .. Length));
               Identifier := Name_Find;
               Set_Parent_Unit_Name (RUD (U), RUD (RU_Id'Val (Position)));
            end if;
            Set_Defining_Identifier
              (RUD (U), Make_Defining_Identifier (Identifier));
         end if;
      end loop;

      Special_Name (RU_CORBA, "CORBA");
      Special_Name (RU_PolyORB, "PolyORB");
      Special_Name (RU_PolyORB_Any_NVList, "NVList");
      Special_Name (RU_CORBA_AbstractBase, "AbstractBase");
      Special_Name (RU_PolyORB_Any_TypeCode, "TypeCode");
      Special_Name (RU_CORBA_TypeCode, "TypeCode");

      for E in RE_Id loop
         Set_Str_To_Name_Buffer (RE_Id'Image (E));
         Set_Str_To_Name_Buffer (Name_Buffer (4 .. Name_Len));
         Capitalize (Name_Buffer (1 .. Name_Len));
         while Name_Buffer (Name_Len) in '0' .. '9'
           or else Name_Buffer (Name_Len) = '_'
         loop
            Name_Len := Name_Len - 1;
         end loop;
         Identifier := Name_Find;
         RED (E) := New_Node (K_Designator);
         Set_Defining_Identifier
           (RED (E), Make_Defining_Identifier (Identifier));
         Set_Parent_Unit_Name (RED (E), RUD (RE_Unit_Table (E)));
      end loop;
      Special_Name (RE_To_CORBA_String, "To_CORBA_String");
      Special_Name (RE_To_PolyORB_String, "To_PolyORB_String");
      Special_Name (RE_NamedValue, "NamedValue");
      Special_Name (RE_TC_Object, "TC_Object");
      Special_Name (RE_TC_Alias, "TC_Alias");
      Special_Name (RE_TC_Enum, "TC_Enum");
      Special_Name (RE_TC_Struct, "TC_Struct");
      Special_Name (RE_To_CORBA_Object, "To_CORBA_Object");
   end Initialize;

   --------
   -- RE --
   --------

   function RE (Id : RE_Id) return Node_Id is
   begin
      return Copy_Designator (RED (Id));
   end RE;

   --------
   -- RU --
   --------

   function RU (Id : RU_Id) return Node_Id is
   begin
      return Copy_Designator (RUD (Id));
   end RU;
end Backend.BE_Ada.Runtime;
