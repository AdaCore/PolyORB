------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     B A C K E N D . B E _ T Y P E S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Output;            use Output;
with Errors;            use Errors;
with Namet;             use Namet;
with Locations;         use Locations;
with Scopes;            use Scopes;

with Frontend.Nutils;   use Frontend.Nutils;
with Frontend.Nodes;    use Frontend.Nodes;

package body Backend.BE_Types is

   --  Local variables declarations

   --  Declare a string to represent
   --  all the types of an idl file.
   Idl_Void               : constant String := "VOID";
   Idl_Short              : constant String := "SHORT";
   Idl_Long               : constant String := "LONG";
   Idl_Longlong           : constant String := "LONGLONG";
   Idl_Ushort             : constant String := "USHORT";
   Idl_Ulong              : constant String := "ULONG";
   Idl_Ulonglong          : constant String := "ULONGLONG";
   Idl_LongDouble         : constant String := "LONGDOUBLE";
   Idl_Float              : constant String := "FLOAT";
   Idl_Double             : constant String := "DOUBLE";
   Idl_Boolean            : constant String := "BOOLEAN";
   Idl_Char               : constant String := "CHAR";
   Idl_WChar              : constant String := "WCHAR";
   Idl_String             : constant String := "STRING";
   Idl_WString            : constant String := "WSTRING";
   Idl_Octet              : constant String := "OCTET";
   Idl_Any                : constant String := "ANY";
   Idl_Object             : constant String := "OBJECT";
   Idl_Alias              : constant String := "ALIAS";
   Idl_Struct             : constant String := "STRUCT";
   Idl_Union              : constant String := "UNION";
   Idl_Enum               : constant String := "ENUM";
   Idl_Sequence           : constant String := "SEQUENCE";
   Idl_Array              : constant String := "ARRAY_T";
   Idl_Except             : constant String := "EXCEPT";
   Idl_Fixed              : constant String := "FIXED";
   Idl_Value              : constant String := "VALUE";
   Idl_Valuebox           : constant String := "VALUEBOX";
   Idl_Native             : constant String := "NATIVE";
   Idl_Typecode           : constant String := "TYPECODE";

   --  Not yet implemented
   --  idl_Principal          : constant String := "PRINCIPAL";
   --  Idl_Typecode           : constant String := "TYPECODE";
   --  Idl_Abstract_Interface : constant String := "ABS_INTERFACE";
   --  Idl_Local_Interface    : constant String := "LOC_INTERFACE";
   --  Idl_Component          : constant String := "COMPONENT";
   --  Idl_Home               : constant String := "HOME";
   --  Idl_Event              : constant String := "EVENT";

   --  Local operations declarations

   procedure Generate (E : Node_Id; L : in out List_Id);
   procedure Generate_Base_Type (E : Node_Id; L : List_Id);
   procedure Generate_Exception_Declaration (E : Node_Id; L : in out List_Id);
   procedure Generate_Interface_Declaration (E : Node_Id; L : in out List_Id);
   procedure Generate_Module (E : Node_Id; L : in out List_Id);
   procedure Generate_Operation_Declaration (E : Node_Id; L : in out List_Id);
   procedure Generate_Structure_Type (E : Node_Id; L : in out List_Id);
   procedure Generate_Switch_Alternative (E : Node_Id; L : in out List_Id);
   procedure Generate_Type_Declaration (E : Node_Id; L : in out List_Id);
   procedure Generate_Union_Type (E : Node_Id; L : in out List_Id);
   procedure Generate_Value_Declaration (E : Node_Id; L : in out List_Id);

   procedure Insert (S : String; L : List_Id);
   --  Append the Node_Kind to the list only if the Node_Kind
   --  is not present yet.
   --  ??? bogus comment, there is no Node_Kind anywhere in this declaration

   procedure Print_List
      (L : List_Id; Output : File_Descriptor := Standout);
   --  Print the list on a file descriptor. By default that is
   --  the standard output.

   procedure Insert_Required_Types (L : List_Id);
   --  Insert the types always required by PolyORB for a CORBA application.

   ------------
   -- Insert --
   ------------

   procedure Insert (S : String; L : List_Id) is
      Node : Node_Id;
      N    : Name_Id;
   begin
      Set_Str_To_Name_Buffer (S);
      N := Name_Find;
      if Get_Name_Table_Info (N) = 0 then
         Set_Name_Table_Info (N, 1);
         Node := New_Node (K_Identifier, No_Location);
         Set_Name (Node, N);
         Append_To (L, Node);
      end if;
   end Insert;

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List
      (L : List_Id; Output : File_Descriptor := Standout)
   is
      Node : Node_Id := First_Entity (L);
   begin
      Set_Output (Output);
      while Present (Node) loop
         Get_Name_String (Name (Node));
         if Output /= Standout then
            Write_Line (Name_Buffer (1 .. Name_Len) & " := True");
         else
            Write_Line (Name_Buffer (1 .. Name_Len));
         end if;
         Node := Next_Entity (Node);
      end loop;
      Set_Standard_Output;
   end Print_List;

   ---------------------------
   -- Insert_Required_Types --
   ---------------------------

   procedure Insert_Required_Types (L : List_Id) is
   begin

      --  The string type and thus unsigned long are always used
      --  but they can not appear in the idl file. So we add them into
      --  the list. Note that if the type string is present in the list,
      --  this operation has no effect on the type list.

      Insert (Idl_String, L);
      Insert (Idl_Ulong, L);

      --  The any type is always needed when building TypeCode variables.
      Insert (Idl_Any, L);

   end Insert_Required_Types;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
      List_Of_Types : List_Id := New_List (No_Location);
      Descriptor    : File_Descriptor;
      Output_File   : constant String
         := Get_Name_String (IDL_Spec_Name) & ".typ";
   begin
      Generate (E, List_Of_Types);

      --  Insert the types always required
      Insert_Required_Types (List_Of_Types);

      if Print_Types then
         Print_List (List_Of_Types);
      end if;

      --  Open the temporary file
      Descriptor := Create_New_File (Output_File, Text);

      --  Check the file descriptor
      if Descriptor = Invalid_FD then
         DE ("fail to open the file called " & Output_File);
         raise Fatal_Error;
      end if;

      --  Write into a temporary file the symbols corresponding
      --  to the types present in the idl file.
      --  This file will be passed to the gnat preprocessor
      --  to eliminate the useless code.
      Print_List (List_Of_Types, Descriptor);

      Close (Descriptor);
   end Generate;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id; L : in out List_Id) is
   begin
      case Kind (E) is
         when K_Attribute_Declaration =>
            Generate (Type_Spec (E), L);

         when K_Complex_Declarator =>
            Insert (Idl_Array, L);
            Insert (Idl_Ulong, L);

         when K_Constant_Declaration =>
            Generate (Type_Spec (E), L);

         when K_Element =>
            Generate (Type_Spec (E), L);

         when K_Enumeration_Type =>
            Insert (Idl_Enum, L);
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);

         when K_Exception_Declaration =>
            Generate_Exception_Declaration (E, L);

         when K_Fixed_Point_Type =>
            Insert (Idl_Fixed, L);
            Insert (Idl_Ushort, L);
            Insert (Idl_Short, L);

         when K_Interface_Declaration =>
            Generate_Interface_Declaration (E, L);

         when K_Member | K_State_Member =>
            Generate (Type_Spec (E), L);

         when K_Module =>
            Generate_Module (E, L);

         when K_Operation_Declaration
           | K_Initializer_Declaration =>
            Generate_Operation_Declaration (E, L);

         when K_Native_Type =>
            Insert (Idl_Native, L);
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);

         when K_Parameter_Declaration =>
            Generate (Type_Spec (E), L);

         when K_Simple_Declarator =>
            Insert (Idl_Alias, L);
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);

         when K_Sequence_Type =>
            Insert (Idl_Sequence, L);
            Insert (Idl_Ulong, L);
            Generate (Type_Spec (E), L);

         when K_Specification =>
            Generate_Module (E, L);

         when K_String_Type =>
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);

         when K_Wide_String_Type =>
            Insert (Idl_WString, L);
            Insert (Idl_Ulong, L);

         when K_Structure_Type =>
            Generate_Structure_Type (E, L);

         when K_Switch_Alternative =>
            Generate_Switch_Alternative (E, L);

         when K_Type_Declaration =>
            Generate_Type_Declaration (E, L);

         when K_Union_Type =>
            Generate_Union_Type (E, L);

         when K_Value_Declaration
           | K_Abstract_Value_Declaration =>
            Generate_Value_Declaration (E, L);

         when K_Value_Box_Declaration =>
            Insert (Idl_Valuebox, L);
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);

         when K_Float .. K_Value_Base =>
            Generate_Base_Type (E, L);

         when others =>
            Dummy (E);
      end case;
   end Generate;

   ------------------------
   -- Generate_Base_Type --
   ------------------------

   procedure Generate_Base_Type (E : Node_Id; L : List_Id) is
   begin
      case Kind (E) is
         when K_Float =>
            Insert (Idl_Float, L);
         when K_Double =>
            Insert (Idl_Double, L);
         when K_Long_Double =>
            Insert (Idl_LongDouble, L);
         when K_Short =>
            Insert (Idl_Short, L);
         when K_Long =>
            Insert (Idl_Long, L);
         when K_Long_Long =>
            Insert (Idl_Longlong, L);
         when K_Unsigned_Short =>
            Insert (Idl_Ushort, L);
         when K_Unsigned_Long =>
            Insert (Idl_Ulong, L);
         when K_Unsigned_Long_Long =>
            Insert (Idl_Ulonglong, L);
         when K_Char =>
            Insert (Idl_Char, L);
         when K_Wide_Char =>
            Insert (Idl_WChar, L);
         when K_String =>
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);
         when K_Wide_String =>
            Insert (Idl_WString, L);
            Insert (Idl_Ulong, L);
         when K_Boolean =>
            Insert (Idl_Boolean, L);
         when K_Octet =>
            Insert (Idl_Octet, L);
         when K_Object =>
            Insert (Idl_Object, L);
            Insert (Idl_String, L);
            Insert (Idl_Ulong, L);
            Insert (Idl_Typecode, L);
         when K_Any =>
            Insert (Idl_Any, L);
         when K_Void =>
            Insert (Idl_Void, L);
         when others =>
            raise Program_Error;
      end case;
   end Generate_Base_Type;

   ------------------------------------
   -- Generate_Exception_Declaration --
   ------------------------------------

   procedure Generate_Exception_Declaration
      (E : Node_Id; L : in out List_Id)
   is
      C : Node_Id;
      LL : List_Id;
   begin
      Insert (Idl_Except, L);
      Insert (Idl_String, L);
      Insert (Idl_Ulong, L);
      LL := Members (E);
      C := First_Entity (LL);
      while Present (C) loop
         Generate (C, L);
         C := Next_Entity (C);
      end loop;
   end Generate_Exception_Declaration;

   ------------------------------------
   -- Generate_Interface_Declaration --
   ------------------------------------

   procedure Generate_Interface_Declaration
      (E : Node_Id; L : in out List_Id)
   is
      F : Node_Id := No_Node;
      B : List_Id;
   begin
      B := Interface_Body (E);
      if not Is_Empty (B) then
         F := First_Entity (B);
         while Present (F) loop
            Generate (F, L);
            F := Next_Entity (F);
         end loop;
      end if;
   end Generate_Interface_Declaration;

   ---------------------
   -- Generate_Module --
   ---------------------

   procedure Generate_Module (E : Node_Id; L : in out List_Id) is
      C : Node_Id;
      LL : List_Id;
   begin
      LL := Definitions (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         while Present (C) loop
            Generate (C, L);
            C := Next_Entity (C);
         end loop;
      end if;
   end Generate_Module;

   ------------------------------------
   -- Generate_Operation_Declaration --
   ------------------------------------

   procedure Generate_Operation_Declaration
      (E : Node_Id; L : in out List_Id)
   is
      C : Node_Id;
      LL : List_Id;
   begin
      if Kind (E) /= K_Initializer_Declaration then
         Generate (Type_Spec (E), L);
      end if;

      LL := Parameters (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         loop
            Generate (C, L);
            C := Next_Entity (C);
            exit when No (C);
         end loop;
      end if;
   end Generate_Operation_Declaration;

   -----------------------------
   -- Generate_Structure_Type --
   -----------------------------

   procedure Generate_Structure_Type (E : Node_Id; L : in out List_Id) is
      LL : List_Id;
      C : Node_Id;

   begin
      Insert (Idl_Struct, L);
      Insert (Idl_String, L);
      Insert (Idl_Ulong, L);
      LL := Members (E);
      if not Is_Empty (LL) then
         C := First_Entity (LL);
         while Present (C) loop
            Generate (C, L);
            C := Next_Entity (C);
         end loop;
      end if;
   end Generate_Structure_Type;

   ---------------------------------
   -- Generate_Switch_Alternative --
   ---------------------------------

   procedure Generate_Switch_Alternative (E : Node_Id; L : in out List_Id) is
      LL : Node_Id := First_Entity (Labels (E));

   begin
      while Present (LL) loop
         Generate (LL, L);
         LL := Next_Entity (LL);
      end loop;
      Generate (Element (E), L);
   end Generate_Switch_Alternative;

   -------------------------------
   -- Generate_Type_Declaration --
   -------------------------------

   procedure Generate_Type_Declaration (E : Node_Id; L : in out List_Id) is
      D : Node_Id := First_Entity (Declarators (E));

   begin
      Generate (Type_Spec (E), L);
      loop
         Generate (D, L);
         D := Next_Entity (D);
         exit when No (D);
      end loop;
   end Generate_Type_Declaration;

   -------------------------
   -- Generate_Union_Type --
   -------------------------

   procedure Generate_Union_Type (E : Node_Id; L : in out List_Id) is
      N : Node_Id := First_Entity (Switch_Type_Body (E));

   begin
      Insert (Idl_Union, L);
      Insert (Idl_Long, L);
      Insert (Idl_String, L);
      Insert (Idl_Ulong, L);
      Generate (Switch_Type_Spec (E), L);
      while Present (N) loop
         Generate (N, L);
         N := Next_Entity (N);
      end loop;
   end Generate_Union_Type;

   --------------------------------
   -- Generate_Value_Declaration --
   --------------------------------

   procedure Generate_Value_Declaration (E : Node_Id; L : in out List_Id) is
      N : Node_Id;
      LL : List_Id;

   begin
      Insert (Idl_Value, L);
      Insert (Idl_String, L);
      Insert (Idl_Ulong, L);
      Insert (Idl_Short, L);
      LL := Value_Body (E);
      if not Is_Empty (LL) then
         N := First_Entity (LL);
         while Present (N) loop
            Generate (N, L);
            N := Next_Entity (N);
         end loop;
      end if;
   end Generate_Value_Declaration;

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Str (Hdr);
      Write_Str ("-p       Print the list generate");
      Write_Eol;
   end Usage;

end Backend.BE_Types;
