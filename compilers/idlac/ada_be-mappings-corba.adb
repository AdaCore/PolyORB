------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . M A P P I N G S . C O R B A                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The CORBA personality IDL mapping.

with Idlac_Errors;                  use Idlac_Errors;

with Idl_Fe.Tree;                   use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic;         use Idl_Fe.Tree.Synthetic;
with Ada_Be.Identifiers;            use Ada_Be.Identifiers;
with Ada_Be.Mappings.CORBA.ALM_1_2; use Ada_Be.Mappings.CORBA.ALM_1_2;

package body Ada_Be.Mappings.CORBA is

   use Idl_Fe.Types;

   Skel_Suffix   : constant String := ".Skel";
   Helper_Suffix : constant String := ".Helper";

   --------------------------
   -- Ada_Helper_Unit_Name --
   --------------------------

   function Ada_Helper_Unit_Name
     (Mapping : access CORBA_Mapping_Type;
      Node    : Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when K_Module | K_Interface | K_ValueType | K_Ben_Idl_File =>
            if Is_Well_Known_Node (Node) then
               return Fetch_Helper_Unit_Name (Node);
            else
               return Client_Stubs_Unit_Name (Mapping, Node) & Helper_Suffix;
            end if;

         when K_Forward_Interface | K_Forward_ValueType =>
            return Ada_Helper_Unit_Name (Mapping, Forward (Node));

         when
           K_Sequence_Instance |
           K_String_Instance   |
           K_Declarator        |
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Exception         |
           K_Boxed_ValueType   =>
            null;

         when K_Scoped_Name =>
            return Ada_Helper_Unit_Name (Mapping, Value (Node));

         when K_Short           |
           K_Long               |
           K_Long_Long          |
           K_Unsigned_Short     |
           K_Unsigned_Long      |
           K_Unsigned_Long_Long |
           K_Char               |
           K_Wide_Char          |
           K_Boolean            |
           K_Float              |
           K_Double             |
           K_Long_Double        |
           K_String             |
           K_Wide_String        |
           K_Octet              |
           K_Any                |
           K_Void               =>
            return "CORBA";

         when K_Object =>
            return "CORBA.Object.Helper";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("No helpers for " & Node_Kind'Image (NK) & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;
      end case;
      return Client_Stubs_Unit_Name (Mapping, Parent_Scope (Node))
               & Helper_Suffix;
   end Ada_Helper_Unit_Name;

   ----------------------------
   -- Ada_Type_Defining_Name --
   ----------------------------

   function Ada_Type_Defining_Name
     (Mapping : access CORBA_Mapping_Type;
      Node    : Node_Id)
      return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Interface         |
           K_Forward_Interface =>
            return Calling_Stubs_Type (Mapping, Node);

         when
           K_ValueType         |
           K_Forward_ValueType =>

            if Abst (Node) then
               return "Abstract_Value_Ref";
            else
               return "Value_Ref";
            end if;

         when others =>
            --  Improper use: node N is not an
            --  Interface or ValueType.

            Error
              ("Improper call of Ada_Type_Defining_Name with a "
               & Node_Kind'Image (NK), Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Ada_Type_Defining_Name;

   -----------------------
   -- Library_Unit_Name --
   -----------------------

   overriding function Library_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      if Is_Well_Known_Node (Node) then
         return Fetch_Unit_Name (Node);
      end if;

      case NK is
         when
           K_Interface    |
           K_Module       |
           K_ValueType    |
           K_Ben_Idl_File =>
            return Ada_Full_Name (Node);

         when
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Declarator        |
           K_Forward_Interface |
           K_Forward_ValueType |
           K_Boxed_ValueType   |
           K_Exception         |
           K_Sequence_Instance |
           K_String_Instance   =>
            return Library_Unit_Name (Self, Parent_Scope (Node));

         when K_Scoped_Name =>
            return Library_Unit_Name (Self, Value (Node));

         when
           K_Short              |
           K_Long               |
           K_Long_Long          |
           K_Unsigned_Short     |
           K_Unsigned_Long      |
           K_Unsigned_Long_Long |
           K_Char               |
           K_Wide_Char          |
           K_Boolean            |
           K_Float              |
           K_Double             |
           K_Long_Double        |
           K_String             |
           K_Wide_String        |
           K_Octet              |
           K_Any                =>
            return "CORBA";

         when K_Object =>
            return "CORBA.Object";

         when others =>
            Error
              ("A " & Node_Kind'Image (NK)
               & " is not a mapped entity.",
               Fatal, Get_Location (Node));
      end case;
      return "";
   end Library_Unit_Name;

   overriding function Client_Stubs_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Library_Unit_Name (Self, Node);
   end Client_Stubs_Unit_Name;

   overriding function Server_Skel_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Client_Stubs_Unit_Name (Self, Node) & Skel_Suffix;
   end Server_Skel_Unit_Name;

   overriding function Self_For_Operation
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self, Node);
      pragma Warnings (On);
   begin
      return "Self";
      --  In CORBA stubs, the target objet is always passed
      --  as a formal parameter named Self.
   end Self_For_Operation;

   --------------------------------
   -- Code_Generation_Suppressed --
   --------------------------------

   function Code_Generation_Suppressed
     (Mapping : access CORBA_Mapping_Type;
      Node    : Node_Id)
      return Boolean
   is
      pragma Unreferenced (Mapping);

      function Has_Period_Delimited_Prefix
        (Name   : String;
         Prefix : String) return Boolean;
      --  Return True iff Name has Prefix, matching only complete
      --  period-separated elements.

      ---------------------------------
      -- Has_Period_Delimited_Prefix --
      ---------------------------------

      function Has_Period_Delimited_Prefix
        (Name   : String;
         Prefix : String) return Boolean
      is
         Length : constant Natural := Prefix'Length;
      begin
         return Name'Length >= Length
           and then Name (Name'First .. Name'First + Length - 1) = Prefix
           and then (Name'Length = Length
                     or else Name (Name'First + Length) = '.');
      end Has_Period_Delimited_Prefix;

   begin
      pragma Assert (Kind (Node) = K_Ben_Idl_File
        or else Kind (Node) = K_Module
        or else Kind (Node) = K_Interface);

      declare
         Name : constant String := Ada_Full_Name (Node);

      begin
         --  By default all CORBA modules are predefined, except
         --  for CORBA.Repository_Root.

         return Has_Period_Delimited_Prefix (Name, "CORBA")
           and then not Has_Period_Delimited_Prefix (Name,
                          "CORBA.Repository_Root");
      end;
   end Code_Generation_Suppressed;

   -------------------
   -- Map_Type_Name --
   -------------------

   overriding procedure Map_Type_Name
     (Self : access CORBA_Mapping_Type;
      Node : Node_Id;
      Unit : out ASU.Unbounded_String;
      Typ  : out ASU.Unbounded_String)
   is
      use Ada.Strings.Unbounded;
      NK : constant Node_Kind := Kind (Node);
   begin
      Unit := +Library_Unit_Name (Self, Node);

      case NK is
         when
           K_Interface         |
           K_ValueType         =>
            Typ := Unit & "." & Ada_Type_Defining_Name (Self, Node);
         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            Typ := Unit & "." & Ada_Name (Node)
                     & "." & Ada_Type_Defining_Name (Self, Node);

         when K_Sequence_Instance =>
            Typ := +(Ada_Full_Name (Node) & ".Sequence");

         when K_String_Instance =>
            if Is_Wide (Node) then
               Typ := +(Ada_Full_Name (Node) & ".Bounded_Wide_String");
            else
               Typ := +(Ada_Full_Name (Node) & ".Bounded_String");
            end if;

         when
           K_Enum            |
           K_Union           |
           K_Struct          |
           K_Exception       |
           K_Boxed_ValueType |
           K_Declarator      =>
            Typ := +Ada_Full_Name (Node);

         when K_Scoped_Name =>
            Map_Type_Name (Self, Value (Node), Unit, Typ);

         when K_Short =>
            Typ := +"CORBA.Short";

         when K_Long =>
            Typ := +"CORBA.Long";

         when K_Long_Long =>
            Typ := +"CORBA.Long_Long";

         when K_Unsigned_Short =>
            Typ := +"CORBA.Unsigned_Short";

         when K_Unsigned_Long =>
            Typ := +"CORBA.Unsigned_Long";

         when K_Unsigned_Long_Long =>
            Typ := +"CORBA.Unsigned_Long_Long";

         when K_Char =>
            Typ := +"CORBA.Char";

         when K_Wide_Char =>
            Typ := +"CORBA.Wchar";

         when K_Boolean =>
            Typ := +"CORBA.Boolean";

         when K_Float =>
            Typ := +"CORBA.Float";

         when K_Double =>
            Typ := +"CORBA.Double";

         when K_Long_Double =>
            Typ := +"CORBA.Long_Double";

         when K_String =>
            Typ := +"CORBA.String";

         when K_Wide_String =>
            Typ := +"CORBA.Wide_String";

         when K_Octet =>
            Typ := +"CORBA.Octet";

         when K_Object =>
            Typ := +"CORBA.Object.Ref";

         when K_Any =>
            Typ := +"CORBA.Any";

         when others =>
            --  Improper use: node N is not mapped to an Ada type

            Error
              ("This Ada_Type_Name : A " & Node_Kind'Image (NK)
               & " does not denote a type.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy

            raise Program_Error;

      end case;
   end Map_Type_Name;

   overriding function Calling_Stubs_Type
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id) return String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      if Abst (Node) then
         return "Abstract_Ref";

      elsif Kind (Node) = K_Interface and then Local (Node) then
         return "Local_Ref";

      elsif Is_Well_Known_Node (Node) then
         return Fetch_Calling_Stubs_Type_Name (Node);

      else
         return "Ref";
      end if;
   end Calling_Stubs_Type;

   overriding function Generate_Scope_In_Child_Package
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return Boolean
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      pragma Assert (Is_Gen_Scope (Node));

      --  For CORBA, all Gen_Scopes are generated in child packages

      return True;
   end Generate_Scope_In_Child_Package;

end Ada_Be.Mappings.CORBA;
