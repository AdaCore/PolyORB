------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                A D A _ B E . M A P P I N G S . C O R B A                 --
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

--  The CORBA personality IDL mapping.

--  $Id$

with Errors; use Errors;

with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;
with Ada_Be.Identifiers; use Ada_Be.Identifiers;
with Ada_Be.Idl2Ada; use Ada_Be.Idl2Ada;

package body Ada_Be.Mappings.CORBA is

   use Idl_Fe.Types;

   Skel_Suffix : constant String := ".Skel";

   function Library_Unit_Name
     (Self : access CORBA_Mapping_Type;
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

   function Client_Stubs_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Library_Unit_Name (Self, Node);
   end Client_Stubs_Unit_Name;

   function Server_Skel_Unit_Name
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return Client_Stubs_Unit_Name (Self, Node) & Skel_Suffix;
   end Server_Skel_Unit_Name;

   function Self_For_Operation
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      return "Self";
      --  In CORBA stubs, the target objet is always passed
      --  as a formal parameter named Self.
   end Self_For_Operation;

   procedure Map_Type_Name
     (Self : access CORBA_Mapping_Type;
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
           K_ValueType         =>
            Typ := +(Library_Unit_Name (Self, Node)
                     & "." & Ada_Type_Defining_Name (Node));
         when
           K_Forward_Interface |
           K_Forward_ValueType =>
            Typ := +(Library_Unit_Name (Self, Node)
                     & "." & Ada_Name (Node)
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
            Typ := +"CORBA.Wide_Char";

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
            --  Improper use: node N is not
            --  mapped to an Ada type.

            Error
              ("This Ada_Type_Name : A " & Node_Kind'Image (NK)
               & " does not denote a type.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy.
            raise Program_Error;

      end case;
   end Map_Type_Name;

   function Calling_Stubs_Type
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return String is
   begin
      if Abst (Node) then
         return "Abstract_Ref";
      else
         return "Ref";
      end if;
   end Calling_Stubs_Type;

   function Generate_Scope_In_Child_Package
     (Self : access CORBA_Mapping_Type;
      Node : Idl_Fe.Types.Node_Id)
     return Boolean is
   begin
      pragma Assert (Is_Gen_Scope (Node));
      return True;
      --  For CORBA, all Gen_Scopes are generated in
      --  separate child packages.
   end Generate_Scope_In_Child_Package;

end Ada_Be.Mappings.CORBA;
