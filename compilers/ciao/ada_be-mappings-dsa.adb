------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                  A D A _ B E . M A P P I N G S . D S A                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2002 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  The DSA personality IDL mapping.

--  $Id$

with Ada.Characters.Handling; --  use Ada.Characters.Handling;

with Asis.Compilation_Units; use Asis.Compilation_Units;
with Asis.Elements; use Asis.Elements;
with Asis.Declarations;

with Errors; use Errors;

with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Idl2Ada; use Ada_Be.Idl2Ada;

with CIAO.ASIS_Queries;
with CIAO.Translator.State; use CIAO.Translator.State;

package body Ada_Be.Mappings.DSA is

   use Idl_Fe.Types;

   function Library_Unit_Name
     (Self : access DSA_Mapping_Type;
      Node : Node_Id)
     return String
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when
           K_Interface    |
           K_Module       |
           K_ValueType    |
           K_Ben_Idl_File =>
            declare
               E : constant Asis.Element := Get_Origin (Node);
            begin
               if not Is_Nil (E) then
                  return Ada.Characters.Handling.To_String
                    (Unit_Full_Name
                     (Enclosing_Compilation_Unit (E)));
               else
                  return "Standard";
               end if;
            end;

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
           K_Wide_String        =>
            return "Standard";

         when K_Octet =>
            return "Ada.Streams";

         when K_Any =>
            return "PolyORB.Any";

         when K_Object =>
            return "DSA.Object";

         when others =>
            Error
              ("A " & NK'Img
               & " is not a mapped entity.",
               Fatal, Get_Location (Node));
      end case;
      return "";
   end Library_Unit_Name;

   function Client_Stubs_Unit_Name
     (Self : access DSA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Library_Unit_Name (Self, Node) & "_Stubs";
   end Client_Stubs_Unit_Name;

   -------------------
   -- Map_Type_Name --
   -------------------

   procedure Map_Type_Name
     (Self : access DSA_Mapping_Type;
      Node : Node_Id;
      Unit : out ASU.Unbounded_String;
      Typ  : out ASU.Unbounded_String)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      Unit := +Library_Unit_Name (Self, Node);

      case NK is
         when
           K_Interface         |
           K_Forward_Interface |
           K_ValueType         |
           K_Forward_ValueType =>
            Typ := +(Library_Unit_Name (Self, Node)
                     & "." & Ada_Type_Defining_Name (Node));

         when K_Sequence_Instance =>
            Typ := +(Ada_Full_Name (Node) & ".Sequence");

         when K_String_Instance =>
            Typ := +(Ada_Full_Name (Node) & ".Bounded_String");

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
            Typ := +"Integer";
            --  XXX What if a subtype thereof was meant?

         when K_Long =>
            Typ := +"Integer";
            --  XXX ditto

         when K_Long_Long =>
            Typ := +"Integer";
            --  XXX ditto

         when K_Unsigned_Short =>
            Typ := +"Natural";
            --  XXX ditto

         when K_Unsigned_Long =>
            Typ := +"Natural";
            --  XXX ditto

         when K_Unsigned_Long_Long =>
            Typ := +"Natural";
            --  XXX ditto

         when K_Char =>
            Typ := +"Character";

         when K_Wide_Char =>
            Typ := +"Wide_Character";

         when K_Boolean =>
            Typ := +"Boolean";

         when K_Float =>
            Typ := +"Float";

         when K_Double =>
            Typ := +"Float";

         when K_Long_Double =>
            Typ := +"Float";

         when K_String =>
            Typ := +"String";

         when K_Wide_String =>
            Typ := +"Wide_String";

         when K_Octet =>
            Typ := +"Ada.Streams.Stream_Element";

         when K_Object =>
            Typ := +"DSA.Object.Ref";

         when K_Any =>
            Typ := +"PolyORB.Any.Any";

         when others =>
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("This Ada_Type_Name : A " & NK'Img
               & " does not denote a type.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Map_Type_Name;

   ------------------------
   -- Self_For_Operation --
   ------------------------

   function Self_For_Operation
     (Self : access DSA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String
   is
      use CIAO.ASIS_Queries;

      E : constant Asis.Element := Get_Origin (Node);
      PS_Node : constant Node_Id := Parent_Scope (Node);
   begin
      if Unit_Category (Enclosing_Compilation_Unit (E))
         = Remote_Call_Interface
      then
         --  This is an RPC operation from an RCI package:
         --  the Self object is determined internally by
         --  the calling stubs.
         return Client_Stubs_Unit_Name (Self, PS_Node)
           & ".Get_Target_Ref";
      else
         declare
            Controlling_Formals :
              constant Asis.Parameter_Specification_List
              := Controlling_Formal_Parameters (E);
            CFN : constant Asis.Defining_Name_List
              := Asis.Declarations.Names
              (Controlling_Formals (Controlling_Formals'First));
         begin
            return Ada.Characters.Handling.To_String
              (Asis.Declarations.Defining_Name_Image
               (CFN (CFN'First)));
         end;
      end if;
   end Self_For_Operation;

   ---------------------------
   -- Server_Skel_Unit_Name --
   ---------------------------

   function Server_Skel_Unit_Name
     (Self : access DSA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Ada_Name (Node) & "_RPC_Receiver";
   end Server_Skel_Unit_Name;

   -------------------------------------
   -- Generate_Scope_In_Child_Package --
   -------------------------------------

   function Generate_Scope_In_Child_Package
     (Self : access DSA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return Boolean is
   begin
      pragma Assert (Is_Gen_Scope (Node));
      if Kind (Node) = K_Module then
         declare
            E : constant Asis.Element := Get_Origin (Node);
         begin
            return Is_Equal
              (E, Unit_Declaration (Enclosing_Compilation_Unit (E)));
            --  True only if E is a library unit declaration.
         end;
      end if;
      return False;
   end Generate_Scope_In_Child_Package;

   ------------------------
   -- Calling_Stubs_Type --
   ------------------------

   function Calling_Stubs_Type
     (Self : access DSA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String
   is
   begin
      return Name (Node) & "_Calling_Stubs";
   end Calling_Stubs_Type;

end Ada_Be.Mappings.DSA;
