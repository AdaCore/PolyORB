------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . H E L P E R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Idlac_Flags;           use Idlac_Flags;
with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Utils;                 use Utils;

package body Ada_Be.Idl2Ada.Helper is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada.helper");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   ----------------------------------------
   -- Specialised generation subprograms --
   ----------------------------------------

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the From_Any operation of a type

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id);
   --  Generate the profile for the To_Any operation of a type

   procedure Gen_Raise_From_Any_Profile
     (CU   : in out Compilation_Unit;
      Node : in     Node_Id);
   --  Generate the Raise_<exception>_From_Any procedure for an
   --  exception. The name of the procedure is
   --  Raise_From_Any_Name (Node).

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an interface declaration

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an interface declaration

   procedure Gen_ValueType_Spec
     (CU : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate the spec of the helper package for a valuetype declaration

   procedure Gen_ValueType_Body
     (CU : in out Compilation_Unit;
      Node : in Node_Id);
   --  Generate the body of the helper package for a valuetype declaration

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an enum declaration

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an enum declaration

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a struct or an
   --  exception declaration

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a struct or an
   --  exception declaration

   procedure Gen_String_Instance_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a string instance

   procedure Gen_String_Instance_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a string instance

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an union declaration

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an union declaration

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for an array declaration

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for an array declaration

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a sequence declaration

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a sequence declaration

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the spec of the helper package for a fixed type declaration

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id);
   --  Generate the body of the helper package for a fixed type declaration

   procedure Gen_Array_TC
     (CU                : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node         : in     Node_Id);
   --  generate lines to fill in an array typecode
   --  only used in the type_declarator part of gen_node_body

   function Raise_From_Any_Name (Node : in Node_Id) return String;
   --  Return the name of a procedure that raises that exception
   --  from an occurrence stored in an Any.

   function Type_Modifier (Node : in Node_Id) return String;
   --  Return the type modifier associed with the ValueType Node

   function Visibility (Node : in Node_Id) return String;
   --  Return the visibility of a state member

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   -------------------
   -- Gen_Node_Spec --
   -------------------

   procedure Gen_Node_Spec
     (CU        : in out Compilation_Unit;
      Node      :        Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            Gen_Interface_Spec (CU, Node);

         when K_Enum =>
            Gen_Enum_Spec (CU, Node);

         when K_Type_Declarator =>
--             if Is_Interface_Type (T_Type (Node)) then
--                null;
--             elsif
            if Kind (T_Type (Node)) = K_Fixed then
               Gen_Fixed_Spec (CU, Node);
            else
               declare
                  It   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Spec (CU, Decl_Node);
                  end loop;
               end;
            end if;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Struct_Exception_Spec (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Spec (CU, Node);

         when K_Union =>
            Gen_Union_Spec (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Spec (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Spec (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Spec (CU, Node);
            Gen_Raise_From_Any_Profile (CU, Node);
            PL (CU, ";");
            PL (CU, "pragma No_Return ("
                & Raise_From_Any_Name (Node) & ");");

         when others =>
            null;

      end case;
   end Gen_Node_Spec;

   -------------------
   -- Gen_Node_Body --
   -------------------

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Interface =>
            Gen_Interface_Body (CU, Node);

         when K_Enum =>
            Gen_Enum_Body (CU, Node);

         when K_Type_Declarator =>
--            if Is_Interface_Type (T_Type (Node)) then
--               null;
            if Kind (T_Type (Node)) = K_Fixed then
               Gen_Fixed_Body (CU, Node);
            else
               declare
                  It   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It, Declarators (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Body (CU, Decl_Node);
                  end loop;
               end;
            end if;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Struct_Exception_Body (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Body (CU, Node);

         when K_Union =>
            Gen_Union_Body (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Body (CU, Node);

         when K_ValueType =>
            Gen_ValueType_Body (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Body (CU, Node);
            Gen_Raise_From_Any_Profile (CU, Node);
            PL (CU, "");
            PL (CU, "is");
            II (CU);
            PL (CU, "Members : constant "
                & Ada_Name (Members_Type (Node)));
            PL (CU, "  := From_Any (Item);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "PolyORB.Exceptions");
            PL (CU, "PolyORB.Exceptions.User_Raise_Exception");
            PL (CU, "  (" & Ada_Name (Node) & "'Identity,");
            II (CU);
            PL (CU, "Members);");
            DI (CU);
            DI (CU);
            PL (CU, "end " & Raise_From_Any_Name (Node) & ";");

            Divert (CU, Deferred_Initialization);
            --  This has to be done in deferred initialization,
            --  after the TypeCode has been constructed.
            PL (CU, "PolyORB.Exceptions.Register_Exception");
            PL (CU, "  (" & Ada_TC_Name (Node) & ",");
            II (CU);
            PL (CU, Raise_From_Any_Name (Node) & "'Access);");
            DI (CU);
            Divert (CU, Initialization_Dependencies);
            PL (CU, "& ""exceptions""");
            Divert (CU, Visible_Declarations);
         when others =>
            null;

      end case;
   end Gen_Node_Body;

   --------------------------
   -- Gen_From_Any_Profile --
   --------------------------

   procedure Gen_From_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in     Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "function From_Any (Item : in CORBA.Any)");
      II (CU);
      Put (CU, "return "
           & Ada_Type_Name (Type_Node));
      DI (CU);
   end Gen_From_Any_Profile;

   ------------------------
   -- Gen_To_Any_Profile --
   ------------------------

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "function To_Any");
      PL (CU, "  (Item : in "
          & Ada_Type_Name (Type_Node)
          & ")");
      Put (CU, "  return CORBA.Any");
   end Gen_To_Any_Profile;

   --------------------------------
   -- Gen_Raise_From_Any_Profile --
   --------------------------------

   procedure Gen_Raise_From_Any_Profile
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      Add_With (CU, "CORBA");
      PL (CU, "procedure " & Raise_From_Any_Name (Node));
      Put (CU, "  (Item : in CORBA.Any)");
   end Gen_Raise_From_Any_Profile;

   ------------------------
   -- Gen_Interface_Spec --
   ------------------------

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      --  Unchecked_To_<reference>
      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
      end;

      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := PolyORB.Any.TypeCode.TC_Object;");

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Interface_Spec;

   --------------------------------
   -- Gen_Forward_Interface_Spec --
   --------------------------------

   procedure Gen_Forward_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      --  Unchecked_To_<reference>
      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name & ";");
      end;

      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := PolyORB.Any.TypeCode.TC_Object;");

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU,  Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Forward_Interface_Spec;

   ------------------------
   -- Gen_ValueType_Spec --
   ------------------------

   procedure Gen_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : in Node_Id)
   is
      Type_Name : constant String
        := Ada_Type_Defining_Name (Node);

      Type_Full_Name : constant String
        := Ada_Type_Name (Node);
      V_Impl_Name : constant String
        := Ada_Name (Node) & ".Value_Impl.Object'Class";
   begin
      pragma Assert (Kind (Node) = K_ValueType);
      Add_With (CU, "CORBA.Value");
      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : in CORBA.Value.Base'Class)");
      PL (CU, "  return " & Type_Full_Name & ";");

      --  generate code for supported interfaces
      --  generate this portion of code if there is a non abstract
      --  supported interface.
      if Supports_Non_Abstract_Interface (Node) then
         Add_With (CU, Ada_Full_Name (Node) & ".Value_Impl");
         NL (CU);
         PL (CU, "type Servant");
         II (CU);
         PL (CU,
             "(Value : access "
             & V_Impl_Name
             & ")");
         Add_With (CU, "PortableServer");
         PL (CU, "is new PortableServer.Servant_Base with null record;");
         DI (CU);
         PL (CU,
             "type Servant_Ref is access all Servant'Class;");
         NL (CU);
         PL (CU, "function To_Servant");
         PL (CU, "  (Self : access "
             & V_Impl_Name
             & ")");
         PL (CU, "  return Servant_Ref;");
      end if;

      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := PolyORB.Any.TypeCode.TC_Value;");

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         Add_With (CU, "PolyORB.CORBA_P.Value.Helper");
         NL (CU);
         PL (CU, "use PolyORB.CORBA_P.Value.Helper;");
         NL (CU);
         PL (CU, "--  Prototypes for internal conversion procedures");
         PL (CU, "procedure From_Any");
         II (CU);
         PL (CU, "(Item              : in     CORBA.Any;");
         PL (CU, " Result_Ref        : in out " & Type_Full_Name & ";");
         PL (CU, " Unmarshalled_List : in out AnyRef_Seq.Sequence);");
         DI (CU);
         NL (CU);
         PL (CU, "procedure To_Any");
         II (CU);
         PL (CU, "(Item            : in     " & Type_Full_Name & ";");
         PL (CU, " Result          : in out CORBA.Any;");
         PL (CU, " Marshalled_List : in out RefAny_Seq.Sequence);");
         DI (CU);
         NL (CU);

         Add_Elaborate_Body (CU);
      end if;

   end Gen_ValueType_Spec;

   ------------------------
   -- Gen_ValueType_Body --
   ------------------------

   procedure Gen_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : in Node_Id) is

      Type_Name : constant String
        := Ada_Type_Defining_Name (Node);

      Type_Full_Name : constant String
        := Ada_Type_Name (Node);

      V_Impl_Name : constant String
        := Ada_Name (Node) & ".Value_Impl.Object'Class";
   begin
      pragma Assert (Kind (Node) = K_ValueType);

      Add_With (CU, "CORBA.Value");
      Add_With (CU, "PolyORB.Log");
      Add_With (CU, "Ada.Strings.Unbounded");

      PL (CU, "use PolyORB.Log;");
      PL (CU, "use PolyORB.Any;");
      PL (CU, "use PolyORB.CORBA_P.Value.Helper;");
      PL (CU, "use CORBA.Value;");

      NL (CU);
      PL (CU, "--  Logging for this package.");
      PL (CU, "package L is new PolyORB.Log.Facility_Log (""" & Name (CU)
          & """);");
      PL (CU, "procedure O (Message : in Standard.String; Level :" &
          " Log_Level := Debug)");
      PL (CU, "  renames L.Output;");
      NL (CU);

      PL (CU, "--  Pointer type for Value_Refs.");
      PL (CU, "type Value_Ptr is access Value_Ref;");
      NL (CU);

      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : in CORBA.Value.Base'Class)");
      PL (CU, "  return " & Type_Full_Name & " is");
      II (CU);
      PL (CU, "Result : " & Type_Full_Name & ";");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "if CORBA.Value.Is_Nil (The_Ref)");
      PL (CU, "  or else CORBA.Value.Is_A (The_Ref, "
          & Repository_Id_Name (Node) & ") then");
      II (CU);
      PL (CU, "Set (Result, CORBA.Value.Object_Of (The_Ref));");
      PL (CU, "return Result;");
      DI (CU);
      PL (CU, "else");
      II (CU);
      PL (CU, "CORBA.Raise_Bad_Param (Default_Sys_Member);");
      DI (CU);
      PL (CU, "end if;");
      DI (CU);
      PL (CU, "end To_" & Type_Name & ";");

      --  generate code for supported interfaces
      --  generate this portion of code if there is a non abstract
      --  supported interface.
      if Supports_Non_Abstract_Interface (Node) then
         NL (CU);
         PL (CU, "function To_Servant");
         PL (CU, "  (Self : access "
             & V_Impl_Name
             & ")");
         PL (CU, "  return Servant_Ref is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return new Servant (Self);");
         DI (CU);
         PL (CU, "end To_Servant;");
      end if;

      if Generate_Dyn then

         NL (CU);
         PL (CU, "--  Wrappers for the recursive procedures.");
         Add_With (CU, "PolyORB.CORBA_P.Value.Helper");
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Result_Ref : " & Type_Full_Name & ";");
         PL (CU, "New_Sequence : AnyRef_Seq.Sequence := " &
             "AnyRef_Seq.Null_Sequence;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "From_Any (Item, Result_Ref, New_Sequence);");
         PL (CU, "return Result_Ref;");
         DI (CU);
         PL (CU, "end From_Any;");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Result_Any : CORBA.Any;");
         PL (CU, "New_Sequence : RefAny_Seq.Sequence :=" &
             " RefAny_Seq.Null_Sequence;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "To_Any (Item, Result_Any, New_Sequence);");
         PL (CU, "return Result_Any;");
         DI (CU);
         PL (CU, "end To_Any;");
         NL (CU);

         Add_With (CU, Ada_Full_Name (Node) & ".Value_Impl");
         Add_With (CU, "CORBA.Impl");

         PL (CU, "--  Actual From_Any conversion procedure.");
         PL (CU, "procedure From_Any");
         PL (CU, "   (Item              : in     CORBA.Any;");
         PL (CU, "    Result_Ref        : in out " & Type_Full_Name & ";");
         PL (CU, "    Unmarshalled_List : in out AnyRef_Seq.Sequence)");
         PL (CU, "is");
         II (CU);
         PL (CU, "--  Get the ID, and then check the association list.");
         PL (CU, "ID_Tag : CORBA.Any := CORBA.Get_Aggregate_Element");
         PL (CU, "   (Item, CORBA.TC_String, CORBA.Unsigned_Long (0));");
         PL (CU, "Temp_String : CORBA.String := CORBA.From_Any (ID_Tag);");
         PL (CU, "My_ID : Any_ID;");
         PL (CU, "Index : Natural;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "My_ID := Ada.Strings.Unbounded.To_String");
         PL (CU, "   (Ada.Strings.Unbounded.Unbounded_String (Temp_String));");
         PL (CU, "pragma Debug (O (""From_Any: "" & My_ID));");
         NL (CU);
         PL (CU, "Index := PolyORB.CORBA_P.Value.Helper.Find_Ref.Index");
         PL (CU, "   (Unmarshalled_List, My_ID);");
         NL (CU);
         PL (CU, "if Index = 0 then");
         II (CU);
         PL (CU, "declare");
         II (CU);
         PL (CU, "List_Item : AnyRef_Element;");
         PL (CU, "Result : " & Ada_Full_Name (Node)
             & ".Value_Impl.Object_Ptr :=");
         PL (CU, "   new " & Ada_Full_Name (Node) & ".Value_Impl.Object;");
         PL (CU, "Temp_Any : CORBA.Any;");
         PL (CU, "Temp_Ref : Value_Ptr :=");
         PL (CU, "   new " & Type_Full_Name & ";");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  Save the Any <-> Ref association.");
         PL (CU, "List_Item.Ref := Ref_Ptr (Temp_Ref);");
         PL (CU, "List_Item.Any := My_ID;");
         PL (CU, "AnyRef_Seq.Append (Unmarshalled_List, List_Item);");
         NL (CU);

         --  Type dependent section.
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
            Position : Natural := 1;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               if Is_State_Member (Member_Node) then
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, State_Declarators (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);

                        PL (CU, "--  Common code.");
                        PL (CU, "Temp_Any := CORBA.Get_Aggregate_Element");
                        PL (CU, "   (Item, "
                            & Ada_Full_TC_Name (State_Type (Member_Node))
                            & ", CORBA.Unsigned_Long ("
                            & Natural'Image (Position) & "));");
                        PL (CU, "pragma Debug (O (""member"
                            & Natural'Image (Position)
                            & " = "" & CORBA.Image (Temp_Any)));");
                        declare
                           Type_Node : constant Node_Id :=
                              State_Type (Member_Node);
                        begin
                           Add_With
                              (CU, Ada_Helper_Name (Type_Node));

                           if (Kind (Type_Node) = K_Scoped_Name) and then
                             ((Kind (Value (Type_Node)) =
                                  K_ValueType or
                              (Kind (Value (Type_Node)) =
                                  K_Forward_ValueType)))
                           then
                              PL (CU, "--  ValueType specific.");
                              PL (CU, "declare");
                              PL (CU, "   New_Ref : " &
                                  Ada_Type_Name (State_Type (Member_Node)) &
                                  ";");
                              PL (CU, "begin");
                              II (CU);
                              PL (CU,
                                  Ada_Helper_Name (State_Type (Member_Node))
                                  & ".From_Any (Temp_Any, New_Ref,"
                                  & " Unmarshalled_List);");
                              PL (CU, "Result." & Ada_Name (Decl_Node)
                                  & " := New_Ref;");
                              DI (CU);
                              PL (CU, "end;");
                           else
                              PL (CU, "--  Regular member.");
                              PL (CU, "Result." & Ada_Name (Decl_Node)
                                  & " := "
                                  & Ada_Helper_Name (State_Type (Member_Node))
                                  & ".From_Any (Temp_Any);");
                           end if;
                        end;
                        Position := Position + 1;
                        NL (CU);
                     end loop;
                  end;
               end if;
            end loop;
         end;
         PL (CU, "--  Return a pointer on the newly created object.");
         PL (CU, "Set (Result_Ref, CORBA.Impl.Object_Ptr (Result));");
         DI (CU);
         PL (CU, "end;");
         DI (CU);
         PL (CU, "else");
         II (CU);
         PL (CU, "declare");
         II (CU);
         PL (CU, "List_Item : AnyRef_Element :=");
         PL (CU, "   AnyRef_Seq.Element_Of (Unmarshalled_List, Index);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "pragma Debug (O (""pointer to "" & My_ID));");
         PL (CU, "Set (Result_Ref, " &
             "CORBA.AbstractBase.Entity_Of (List_Item.Ref.all));");
         DI (CU);
         PL (CU, "end;");
         DI (CU);
         PL (CU, "end if;");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         PL (CU, "--  Actual To_Any conversion procedure.");
         PL (CU, "procedure To_Any");
         II (CU);
         PL (CU, "(Item            : in     " & Type_Full_Name & ";");
         PL (CU, " Result          : in out CORBA.Any;");
         PL (CU, " Marshalled_List : in out RefAny_Seq.Sequence)");
         DI (CU);
         PL (CU, " is");
         II (CU);
         PL (CU, "My_ID : constant Any_ID := Get_ID (Result);");
         PL (CU, "Index : constant Natural :=");
         PL (CU, "   Find_Any.Index (Marshalled_List," &
             " CORBA.AbstractBase.Ref (Item));");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "if Index = 0 then");
         II (CU);
         PL (CU, "declare");
         II (CU);
         PL (CU, "Temp_Result : PolyORB.Any.Any;");
         PL (CU, "Object_U : " & Ada_Full_Name (Node)
             & ".Value_Impl.Object_Ptr;");
         PL (CU, "List_Item : RefAny_Element;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "Temp_Result := CORBA.Get_Empty_Any_Aggregate (" &
             Ada_TC_Name (Node) & ");");
         PL (CU, "Object_U := " & Ada_Full_Name (Node)
             & ".Value_Impl.Object_Ptr");
         PL (CU, "   (Object_Of (Item));");
         NL (CU);

         PL (CU, "--  We save the association Item <-> Temp_Result.");
         PL (CU, "List_Item.Ref := CORBA.AbstractBase.Ref (Item);");
         PL (CU, "List_Item.Any := My_ID;");
         PL (CU, "RefAny_Seq.Append (Marshalled_List, List_Item);");
         NL (CU);

         PL (CU, "--  Put the ID first into the aggregate.");
         PL (CU, "CORBA.Add_Aggregate_Element");
         PL (CU, "   (Temp_Result, CORBA.To_Any");
         PL (CU, "       (CORBA.To_CORBA_String (My_ID)));");
         PL (CU, "pragma Debug (O (""To_Any: ID="" & My_ID));");
         NL (CU);

         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               if Is_State_Member (Member_Node) then
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, State_Declarators (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        declare
                           Type_Node : constant Node_Id :=
                              State_Type (Member_Node);
                        begin
                           Add_With
                              (CU, Ada_Helper_Name (Type_Node));

                           if (Kind (Type_Node) = K_Scoped_Name) and then
                             ((Kind (Value (Type_Node)) =
                                  K_ValueType or
                              (Kind (Value (Type_Node)) =
                                  K_Forward_ValueType)))
                           then
                              PL (CU, "--  ValueType member.");
                              PL (CU, "declare");
                              PL (CU, "   Temp_Any : CORBA.Any;");
                              PL (CU, "begin");
                              II (CU);
                              PL (CU, "Temp_Any := Get_Empty_Any_Aggregate"
                                  & " ("
                                  & Ada_Full_TC_Name (State_Type (Member_Node))
                                  & ");");
                              PL (CU,
                                  Ada_Helper_Name (State_Type (Member_Node)) &
                                  ".To_Any (Object_U." & Ada_Name (Decl_Node)
                                  & ", Temp_Any, Marshalled_List);");
                              PL (CU, "pragma Debug (O (""To_Any: member=""" &
                                  " & CORBA.Image (Temp_Any)));");
                              PL (CU,
                                  "CORBA.Add_Aggregate_Element (Temp_Result,");
                              PL (CU,
                                  "                             Temp_Any);");
                              DI (CU);
                              PL (CU, "end;");

                           else
                              PL (CU, "--  Regular member.");
                              PL (CU, "CORBA.Add_Aggregate_Element");
                              PL (CU, "  (Temp_Result, "
                                  & Ada_Helper_Name (State_Type (Member_Node))
                                  & ".To_Any (Object_U."
                                  & Ada_Name (Decl_Node)
                                  & "));");
                              PL (CU,
                                  " pragma Debug (O (""To_Any: member1=""" &
                                  " & CORBA.Image (CORBA.To_Any (Object_U." &
                                  Ada_Name (Decl_Node) & "))));");
                           end if;
                        end;
                     end loop;
                  end;
               end if;
            end loop;
         end;
         PL (CU, "Result := Temp_Result;");
         DI (CU);
         PL (CU, "end;");
         DI (CU);
         PL (CU, "else");
         II (CU);
         PL (CU, "declare");
         PL (CU, "   List_Item : RefAny_Element :=");
         PL (CU, "      RefAny_Seq.Element_Of (Marshalled_List, Index);");
         PL (CU, "   Result_ID : Any_ID := List_Item.Any;");
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         PL (CU, "  (Result, CORBA.To_Any");
         PL (CU, "     (CORBA.To_CORBA_String (Result_ID)));");
         PL (CU, "pragma Debug (O (""To_Any: pointer="" & Result_ID));");
         DI (CU);
         PL (CU, "end;");
         DI (CU);
         PL (CU, "end if;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String :=");
         PL (CU, "   CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String :=");
         PL (CU, "   CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");

         --  Declare the names and types of the members of the value

         declare
            It   : Node_Iterator;
            State_Member_Node_Id : Node_Id;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, State_Member_Node_Id);
               if Is_State_Member (State_Member_Node_Id) then
                  declare
                     It2 : Node_Iterator;
                     Content_Node_Id : Node_Id;
                  begin
                     Init (It2, State_Declarators (State_Member_Node_Id));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Content_Node_Id);
                        PL (CU, "Name_"
                            & Ada_Name (Content_Node_Id)
                            & " : CORBA.String := CORBA.To_CORBA_String ("""
                            & Ada_Name (Content_Node_Id)
                            & """);");
                     end loop;
                  end;
               end if;
            end loop;
         end;

         DI (CU);
         PL (CU, "begin");
         II (CU);

         --  Put the name and repository Id for the value
         PL (CU, "CORBA.TypeCode.Add_Parameter");
         PL (CU, "  (" & Ada_TC_Name (Node) & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter");
         PL (CU, "  (" & Ada_TC_Name (Node) & ", CORBA.To_Any (Id));");

         --  Add the type modifier tag
         PL (CU, "CORBA.TypeCode.Add_Parameter");
         PL (CU, "  (" & Ada_TC_Name (Node)
             & ", CORBA.To_Any (CORBA.Short ("
             & Type_Modifier (Node) & ")));");

         --  Add the concrete base type
         --  XXX For the moment, a null TC is passed
         PL (CU, "CORBA.TypeCode.Add_Parameter");
         PL (CU, "  (" & Ada_TC_Name (Node)
             & ", CORBA.To_Any (CORBA.TC_Null));");

         --  Add the visibility, type and name of the different
         --  members of the valuetype
         declare
            It   : Node_Iterator;
            State_Member_Node_Id : Node_Id;
         begin
            Init (It, Contents (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, State_Member_Node_Id);
               if Is_State_Member (State_Member_Node_Id) then
                  declare
                     It2 : Node_Iterator;
                     Content_Node_Id : Node_Id;
                  begin
                     Init (It2, State_Declarators (State_Member_Node_Id));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Content_Node_Id);
                        PL (CU, "CORBA.TypeCode.Add_Parameter");
                        II (CU);
                        PL (CU, "(" & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ( CORBA.Short ("
                            & Visibility (State_Member_Node_Id)
                            & ")));");
                        DI (CU);
                        PL (CU, "CORBA.TypeCode.Add_Parameter");
                        II (CU);
                        PL (CU, "(" & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ("
                            & Ada_Full_TC_Name
                               (State_Type (State_Member_Node_Id))
                            & "));");
                        DI (CU);
                        PL (CU, "CORBA.TypeCode.Add_Parameter");
                        II (CU);
                        PL (CU, "(" & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ("
                            & "Name_"
                            & Ada_Name (Content_Node_Id)
                            & "));");
                        DI (CU);
                     end loop;
                  end;
               end if;
            end loop;
         end;

         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);

      end if;

   end Gen_ValueType_Body;

   ------------------------
   -- Gen_Interface_Body --
   ------------------------

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Type_Defining_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         NL (CU);
         PL (CU, "function Unchecked_To_" & Type_Defining_Name);
         Add_With (CU, "CORBA.Object");
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "Result : " & Type_Name & ";");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "Set (Result,");
         PL (CU,
             "     CORBA.Object.Object_Of (The_Ref));");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end Unchecked_To_" & Type_Defining_Name & ";");

         --  To_<reference>

         --    The standard mandates type checking during narrowing
         --    (4.6.2 Narrowing Object References).
         --    Doing the check properly implies either
         --       1. querying the interface repository
         --          (not implemented yet);
         --    or 2. calling Is_A (Repository_Id) on an
         --          object reference whose type maps the actual
         --          (i. e. most derived) interface of The_Ref.
         --          (which is impossible if that type is not
         --          known on the partition where To_Ref is called);
         --    or 3. a remote invocation of an Is_A method of
         --          the object.
         --
         --    The most general and correct solution to this
         --    problem is 3. When a remote call is not desired,
         --    the user should use Unchecked_To_Ref, whose purpose
         --    is precisely that.
         --
         --    This solution is implemented as a dispatching call
         --    to Is_A on the source object reference. The remote
         --    Is_A operation will be invoked if necessary.

         NL (CU);
         PL (CU, "function To_" & Type_Defining_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "use CORBA;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "if CORBA.Object.Is_Nil (The_Ref)");
         PL (CU, "  or else CORBA.Object.Is_A (The_Ref, "
             & Repository_Id_Name (Node) & ") then");
         II (CU);
         PL (CU, "return Unchecked_To_"
             & Type_Defining_Name
             & " (The_Ref);");
         DI (CU);
         PL (CU, "end if;");

         PL (CU, "CORBA.Raise_Bad_Param (Default_Sys_Member);");
         DI (CU);
         PL (CU, "end To_" & Type_Defining_Name & ";");
      end;

      if Generate_Dyn then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Node)
             & " (CORBA.Object.Helper."
             & "From_Any (Item));");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "A : CORBA.Any := CORBA.Object.Helper.To_Any");
         PL (CU, "  (CORBA.Object.Ref (Item));");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Set_Type (A, " & Ada_TC_Name (Node) & ");");
         PL (CU, "return A;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Interface_Body;

   --------------------------------
   -- Gen_Forward_Interface_Body --
   --------------------------------

   procedure Gen_Forward_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         Add_With (CU, "CORBA.Object");
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "Result : " & Type_Name & ";");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, Ada_Name (Node) & ".Set (Result,");
         PL (CU,
             "     CORBA.Object.Object_Of (The_Ref));");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end Unchecked_To_" & Short_Type_Name & ";");

         --  To_<reference>
         --  see the corresponding comment in gen_interface_body
         --  if you want more information.

         NL (CU);
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : in CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
         II (CU);
         PL (CU, "use CORBA;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "if CORBA.Object.Is_Nil (The_Ref)");
         PL (CU, "  or else CORBA.Object.Is_A (The_Ref, """
             & Idl_Repository_Id (Forward (Node)) & """) then");
         II (CU);
         PL (CU, "return Unchecked_To_"
             & Short_Type_Name
             & " (The_Ref);");
         DI (CU);
         PL (CU, "end if;");

         PL (CU, "CORBA.Raise_Bad_Param (Default_Sys_Member);");
         DI (CU);
         PL (CU, "end To_" & Short_Type_Name & ";");
      end;

      if Generate_Dyn then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Node)
             & " (CORBA.Object.Helper."
             & "From_Any (Item));");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return CORBA.Object.Helper.To_Any "
             & "(CORBA.Object.Ref (Item));");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Forward (Node))
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Forward_Interface_Body;

   -------------------
   -- Gen_Enum_Spec --
   -------------------

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_Enum;");
         DI (CU);

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Enum_Spec;

   -------------------
   -- Gen_Enum_body --
   -------------------

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      if Generate_Dyn then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Index : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element (Item,");
         PL (CU, "                             "
             & "CORBA.TC_Unsigned_Long,");
         PL (CU, "                             "
             & "CORBA.Unsigned_Long (0));");
         DI (CU);
         PL (CU, "Position : constant CORBA.Unsigned_Long "
             & ":= CORBA.From_Any (Index);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "return "
             & Ada_Name (Node)
             & "'Val (Position);");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         II (CU);
         PL (CU, "(Result,");
         PL (CU, " CORBA.To_Any (CORBA.Unsigned_Long ("
             & Ada_Name (Node)
             & "'Pos (Item))));");
         DI (CU);
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         declare
            It   : Node_Iterator;
            E_Node : Node_Id;
         begin
            Init (It, Enumerators (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, E_Node);
               PL (CU, Ada_Name (E_Node)
                   & "_Name : CORBA.String := CORBA.To_CORBA_String ("""
                   & Ada_Name (E_Node)
                   & """);");
            end loop;
         end;

         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         declare
            It   : Node_Iterator;
            E_Node : Node_Id;
         begin
            Init (It, Enumerators (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, E_Node);
               PL (CU, "CORBA.TypeCode.Add_Parameter ("
                   & Ada_TC_Name (Node)
                   & ", CORBA.To_Any ("
                   & Ada_Name (E_Node)
                   & "_Name));");
            end loop;
         end;
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Enum_Body;

   -------------------------------
   -- Gen_Struct_Exception_Spec --
   -------------------------------

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
      Struct_Node : Node_Id;
   begin
      if Generate_Dyn then
         --  Typecode generation
         Add_With (CU, "CORBA");

         NL (CU);
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         if Kind (Node) = K_Struct then
            PL (CU, "CORBA.TypeCode.TC_Struct;");
         else
            PL (CU, "CORBA.TypeCode.TC_Except;");
         end if;
         DI (CU);

         if Kind (Node) = K_Struct then
            Struct_Node := Node;
         else
            Struct_Node := Members_Type (Node);
         end if;

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Struct_Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Struct_Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Struct_Exception_Spec;

   -------------------------------
   -- Gen_Struct_Exception_Body --
   -------------------------------

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Struct_Node : Node_Id;
      Is_Empty : Boolean;
   begin
      if Generate_Dyn then
         if Kind (Node) = K_Struct then
            Struct_Node := Node;
         else
            Struct_Node := Members_Type (Node);
         end if;

         Is_Empty := Length (Members (Node)) = 0;

         --  From_Any
         Add_With (CU, "CORBA", Use_It => True);
         NL (CU);
         Gen_From_Any_Profile (CU, Struct_Node);
         PL (CU, " is");
         II (CU);
         if not Is_Empty then
            PL (CU, "Index : CORBA.Any;");
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Struct_Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU, "Result_"
                            & Ada_Name (Decl_Node)
                            & " : "
                            & Ada_Type_Name (M_Type (Member_Node))
                            & ";");
                     end loop;
                  end;
               end loop;
            end;
         else
            PL (CU, "Result : "
                & Ada_Name (Struct_Node)
                & ";");
         end if;
         if Is_Empty then
            PL (CU, "pragma Warnings (Off);");
            PL (CU, "pragma Unreferenced (Item);");
            PL (CU, "pragma Warnings (On);");
         end if;
         DI (CU);
         PL (CU, "begin");
         II (CU);
         if Is_Empty then
            PL (CU, "return Result;");
         else
            declare
               It   : Node_Iterator;
               Member_Node : Node_Id;
               I : Integer := 0;
            begin
               Init (It, Members (Struct_Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        PL (CU,
                            "Index := CORBA.Get_Aggrega"
                            & "te_Element (Item,");
                        Add_With (CU, Ada_Helper_Name
                                  (M_Type (Member_Node)));
                        PL (CU,
                            "                                      "
                            & Ada_Full_TC_Name (M_Type (Member_Node))
                            & ",");
                        PL (CU,
                            "                                      "
                            & "CORBA.Unsigned_Long ("
                            & Integer'Image (I)
                            &"));");
                        Add_With (CU, Ada_Helper_Name
                                  (M_Type (Member_Node)));
                        PL (CU, "Result_"
                            & Ada_Name (Decl_Node)
                            & " := "
                            & Ada_Helper_Name (M_Type (Member_Node))
                            & ".From_Any (Index);");
                        I := I + 1;
                     end loop;
                  end;
               end loop;
            end;
            PL (CU, "return");
            II (CU);
            declare
               First_Member : Boolean := True;
               Begin_Of_Line : String (1 .. 1) := "(";
               End_Of_Line : String (1 .. 2) := ", ";
               It   : Node_Iterator;
               Member_Node : Node_Id;
            begin
               Init (It, Members (Struct_Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);
                  declare
                     It2   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It2, Decl (Member_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);
                        if Is_End (It) and Is_End (It2) then
                           End_Of_Line := ");";
                        end if;
                        PL (CU, Begin_Of_Line
                            & Ada_Name (Decl_Node)
                            & " => Result_"
                            & Ada_Name (Decl_Node)
                            & End_Of_Line);
                        if First_Member then
                           First_Member := False;
                           Begin_Of_Line := " ";
                        end if;
                     end loop;
                  end;
               end loop;
            end;
            DI (CU);
         end if;
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Struct_Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         if Is_Empty then
            PL (CU, "pragma Warnings (Off);");
            PL (CU, "pragma Unreferenced (Item);");
            PL (CU, "pragma Warnings (On);");
         end if;
         DI (CU);
         PL (CU, "begin");
         II (CU);
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     PL (CU, "CORBA.Add_Aggregate_Element");
                     II (CU);
                     Add_With (CU, Ada_Helper_Name (M_Type (Member_Node)));
                     PL (CU, "(Result, "
                         & Ada_Helper_Name (M_Type (Member_Node))
                         & ".To_Any (Item."
                         & Ada_Name (Decl_Node)
                         & "));");
                     DI (CU);
                  end loop;
               end;
            end loop;
         end;
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     PL (CU, "Arg_Name_"
                         & Ada_Name (Decl_Node)
                         & " : CORBA.String := CORBA.To_CORBA_String ("""
                         & Ada_Name (Decl_Node)
                         & """);");
                  end loop;
               end;
            end loop;
         end;
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         declare
            It   : Node_Iterator;
            Member_Node : Node_Id;
         begin
            Init (It, Members (Struct_Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Member_Node);
               declare
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, Decl (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     Add_With (CU, Ada_Helper_Name (M_Type (Member_Node)));
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Full_TC_Name (M_Type (Member_Node))
                         & "));");
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any (Arg_Name_"
                         & Ada_Name (Decl_Node)
                         & "));");
                  end loop;
               end;
            end loop;
         end;
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Struct_Exception_Body;

   ------------------------------
   -- Gen_String_Instance_Spec --
   ------------------------------

   procedure Gen_String_Instance_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  Typecode generation
         Add_With (CU, "CORBA");

         NL (CU);
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_String;");
         DI (CU);

         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_String_Instance_Spec;

   ------------------------------
   -- Gen_String_Instance_Body --
   ------------------------------

   procedure Gen_String_Instance_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  From_Any
         Add_With (CU, "CORBA");
         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Result : CORBA.String := CORBA.From_Any (Item);");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  This is bad code. To be improved when "
             & "CORBA.Bounded_String will exist");
         PL (CU, "return "
             & Ada_Full_Name (Node)
             & ".To_Bounded_String (Result);");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         Add_With (CU, "CORBA");
         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "--  This is bad code. To be improved when "
             & "CORBA.Bounded_String will exist");
         PL (CU, "return CORBA.To_Any ("
             & Ada_Full_Name (Node)
             & ".To_String (Item));");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in the typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any ("
             & Utils.Img (Expr_Value (Bound (Node)))
             & "));");
         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_String_Instance_Body;

   --------------------
   -- Gen_Union_Spec --
   --------------------

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin

      if Generate_Dyn then
         --  TypeCode generation

         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object :=");
         II (CU);
         PL (CU, "CORBA.TypeCode.TC_Union;");
         DI (CU);

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>

         Add_Elaborate_Body (CU);
      end if;
   end Gen_Union_Spec;

   --------------------
   -- Gen_Union_Body --
   --------------------

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         Add_With (CU, "CORBA", Use_It => True);
         Add_With (CU, Ada_Helper_Name (Switch_Type (Node)));

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         PL (CU, "Label_Any : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Aggregate_Element (Item,");
         PL (CU, "                             "
             & Ada_Full_TC_Name (Switch_Type (Node)) & ",");
         PL (CU, "                             "
             & "CORBA.Unsigned_Long (0));");
         DI (CU);
         PL (CU, "Label : constant "
             & Ada_Type_Name (Switch_Type (Node))
             & " := "
             & Ada_Helper_Name (Switch_Type (Node))
             & ".From_Any (Label_Any);");
         PL (CU, "Result : "
             & Ada_Type_Name (Node)
             & " (Label);");
         PL (CU, "Index : CORBA.Any;");
         PL (CU, "I : Natural := 1;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "case Label is");
         II (CU);
         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
            Has_Default : Boolean := False;
         begin
            Init (It, Cases (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);
               declare
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;
               begin
                  if Default_Index (Node) = I then
                     Has_Default := True;
                     Put (CU, "when others");
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        if First_Label then
                           Put (CU, "when ");
                           First_Label := False;
                        else
                           Put (CU, " | ");
                        end if;
                        Gen_Constant_Value (CU, Label_Node);
                     end loop;
                  end if;
                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "Index := CORBA.Get_Aggregate_Element");
                  II (CU);
                  PL (CU, "(Item,");

                  Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));

                  PL (CU, " "
                      & Ada_Full_TC_Name (Case_Type (Case_Node))
                      & ",");
                  PL (CU, " CORBA.Unsigned_Long (I));");
                  I := I + 1;
                  DI (CU);
                  PL (CU, "I := I + 1;");
                  PL (CU, "Result."
                      & Ada_Name (Case_Decl (Case_Node))
                      & " := "
                      & Ada_Helper_Name (Case_Type (Case_Node))
                      & ".From_Any (Index);");
                  DI (CU);
               end;
            end loop;
            if not Has_Default then
               Gen_When_Others_Clause (CU);
            end if;
         end;

         DI (CU);
         PL (CU, "end case;");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Add_Aggregate_Element");
         II (CU);
         Add_With (CU, Ada_Helper_Name (Switch_Type (Node)));
         PL (CU, "(Result, "
             & Ada_Helper_Name (Switch_Type (Node))
             & ".To_Any (Item.Switch));");
         DI (CU);
         PL (CU, "case Item.Switch is");
         II (CU);

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
            Has_Default : Boolean := False;
         begin
            Init (It, Cases (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;
               begin
                  if Default_Index (Node) = I then
                     Put (CU, "when others");
                     Has_Default := True;
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        if First_Label then
                           Put (CU, "when ");
                           First_Label := False;
                        else
                           Put (CU, " | ");
                        end if;
                        Gen_Constant_Value (CU, Label_Node);
                     end loop;
                  end if;
                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "CORBA.Add_Aggregate_Element");
                  II (CU);
                  Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                  PL (CU, "(Result, "
                      & Ada_Helper_Name (Case_Type (Case_Node))
                      & ".To_Any (Item."
                      & Ada_Name (Case_Decl (Case_Node))
                      & "));");
                  I := I + 1;
                  DI (CU);
                  DI (CU);
               end;
            end loop;
            if not Has_Default then
               Gen_When_Others_Clause (CU);
            end if;
         end;

         DI (CU);
         PL (CU, "end case;");
         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");

         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");
         PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
          & """);");

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               PL (CU, "Arg_Name_"
                   & Ada_Name (Case_Decl (Case_Node))
                   & " : CORBA.String := CORBA.To_CORBA_String ("""
                   & Ada_Name (Case_Decl (Case_Node))
                   & """);");
            end loop;
         end;

         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Name));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (Id));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any ("
             & Ada_Full_TC_Name (Switch_Type (Node))
             & "));");
         PL (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Node)
             & ", CORBA.To_Any (CORBA.Long'("
             & Long_Integer_Img (Default_Index (Node))
          & ")));");

         declare
            It   : Node_Iterator;
            Case_Node : Node_Id;
            I : Long_Integer := 0;
         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  It2   : Node_Iterator;
                  Label_Node : Node_Id;
               begin
                  if Default_Index (Node) = I then
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Helper_Name (Switch_Type (Node))
                         & ".To_Any ("
                         & Ada_Type_Name (Switch_Type (Node))
                         & "'First)));");

                     Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Full_TC_Name (Case_Type (Case_Node))
                         & "));");
                     PL (CU, "CORBA.TypeCode.Add_Parameter ("
                         & Ada_TC_Name (Node)
                         & ", CORBA.To_Any (Arg_Name_"
                         & Ada_Name (Case_Decl (Case_Node))
                      & "));");
                  else
                     Init (It2, Labels (Case_Node));
                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Label_Node);
                        Put (CU, "CORBA.TypeCode.Add_Parameter ("
                             & Ada_TC_Name (Node)
                             & ", CORBA.To_Any ("
                             & Ada_Helper_Name (Switch_Type (Node))
                             & ".To_Any ("
                             & Ada_Type_Name (Switch_Type (Node))
                             & "'(");
                        Gen_Constant_Value (CU, Label_Node);
                        PL (CU, "))));");

                        Add_With (CU, Ada_Helper_Name (Case_Type (Case_Node)));
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any ("
                            & Ada_Full_TC_Name (Case_Type (Case_Node))
                            & "));");
                        PL (CU, "CORBA.TypeCode.Add_Parameter ("
                            & Ada_TC_Name (Node)
                            & ", CORBA.To_Any (Arg_Name_"
                            & Ada_Name (Case_Decl (Case_Node))
                            & "));");
                     end loop;
                  end if;
                  I := I + 1;
               end;
            end loop;
         end;

         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Union_Body;

   ------------------------------
   -- Gen_Type_Declarator_Spec --
   ------------------------------

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Is_Array : constant Boolean
        := Length (Array_Bounds (Node)) > 0;
   begin
      if Generate_Dyn then
         --  TypeCode

         NL (CU);
         Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

         Put (CU, Ada_TC_Name (Node)
              & " : CORBA.TypeCode.Object := CORBA.TypeCode.");
         if Is_Array then
            PL (CU, "TC_Array;");
         else
            PL (CU, "TC_Alias;");
         end if;

         if Is_Array then
            Add_Elaborate_Body (CU);
         end if;

         if not Is_Interface_Type (Node) then
            --  From_Any

            NL (CU);
            Gen_From_Any_Profile (CU, Node);
            PL (CU, ";");

            --  To_Any

            NL (CU);
            Gen_To_Any_Profile (CU, Node);
            PL (CU, ";");
         end if;
      end if;
   end Gen_Type_Declarator_Spec;

   ------------------------------
   -- Gen_Type_Declarator_Body --
   ------------------------------

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Is_Array : constant Boolean
        := Length (Array_Bounds (Node)) > 0;
      Type_Node : constant Node_Id := T_Type (Parent (Node));
      Helper_Name : constant String := Ada_Helper_Name (Type_Node);
   begin
      if Generate_Dyn then
         --  Fill in typecode TC_<name of the type>

         Divert (CU, Deferred_Initialization);
         NL (CU);
         PL (CU, "declare");
         II (CU);
         Add_With (CU, "CORBA");

         if Is_Array then
            for I in 1 .. Length (Array_Bounds (Node)) - 1 loop
               PL (CU, "TC_"
                   & Img (I)
                   & " : CORBA.TypeCode.Object := "
                   & "CORBA.TypeCode.TC_Array;");
            end loop;
         else
            PL (CU, "Name : CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Node)
                & """);");
            PL (CU, "Id : CORBA.String := CORBA.To_CORBA_String ("""
                & Idl_Repository_Id (Node)
                & """);");
         end if;

         DI (CU);
         PL (CU, "begin");
         II (CU);

         if Is_Array then
            Gen_Array_TC (CU, Type_Node, Node);
         else
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Name));");
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any (Id));");
            Add_With (CU, Ada_Helper_Name (Type_Node));
            PL (CU, "CORBA.TypeCode.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any ("
                & Ada_Full_TC_Name (Type_Node)
                & "));");
         end if;

         DI (CU);
         PL (CU, "end;");
         Divert (CU, Visible_Declarations);

         if Is_Interface_Type (Type_Node) then
            return;
         end if;

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);

         Add_With (CU, Ada_Helper_Name (Type_Node));

         if Helper_Name /= "CORBA"
           and then Helper_Name /= "CORBA.Object.Helper"
           and then Helper_Name /= Name (CU)
         then
            Divert (CU, Initialization_Dependencies);
            PL (CU, "& """ & Ada_Helper_Name (Type_Node) & """");
            Divert (CU, Visible_Declarations);
         end if;

         if Is_Array then
            PL (CU, "Result : "
                & Ada_Type_Name (Node)
                & ";");
            DI (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It : Node_Iterator;
               Bound_Node : Node_Id;
               Number : Integer := 0;
            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  Put (CU, "for I"
                       & Img (Number)
                       & " in 0 .. ");
                  Gen_Node_Stubs_Spec (CU, Bound_Node);
                  PL (CU, " - 1 loop");
                  Number := Number + 1;
                  II (CU);
               end loop;

               Put (CU, "Result ");
               for I in 0 .. Number - 1 loop
                  if I = 0 then
                     Put (CU, "(");
                  else
                     Put (CU, ", ");
                  end if;
                  Put (CU, "I" & Img (I));
                  if I = Number - 1 then
                     Put (CU, ")");
                  end if;
               end loop;

               PL (CU, " := "
                   & Ada_Helper_Name (Type_Node)
                   & ".From_Any");
               II (CU);
               Add_With (CU, "CORBA");
               PL (CU, "(CORBA.Get_Aggregate_Element (Item,");
               Add_With (CU, Ada_Helper_Name (Type_Node));
               PL (CU, "                              "
                   & Ada_Full_TC_Name (Type_Node)
                   & ",");
               Put (CU, "                             "
                    & " CORBA.Unsigned_Long (");

               declare
                  Index : Natural := 0;
                  First_Bound : Boolean := True;
               begin
                  Init (Bounds_It, Array_Bounds (Node));

                  while not Is_End (Bounds_It) loop
                     Get_Next_Node (Bounds_It, Bound_Node);

                     if First_Bound then
                        First_Bound := False;
                     else
                        Put (CU, " + ");
                     end if;
                     Put (CU, "I" & Img (Index));
                     for J in Index + 1 .. Number - 1 loop
                        Put (CU, " * ");
                        Gen_Node_Stubs_Spec (CU, Bound_Node);
                     end loop;
                     Index := Index + 1;
                  end loop;
               end;

               PL (CU, ")));");
               DI (CU);
               for I in 1 .. Number loop
                  DI (CU);
                  PL (CU, "end loop;");
               end loop;
            end;
            PL (CU, "return Result;");

         else
            PL (CU, "Result : constant "
                & Ada_Type_Name (Type_Node)
                & " := "
                & Ada_Helper_Name (Type_Node)
                & ".From_Any (Item);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return "
                & Ada_Type_Name (Node)
                & " (Result);");

         end if;
         DI (CU);
         PL (CU, "end From_Any;");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, " is");
         II (CU);

         if Is_Array then

            Add_With (CU, "CORBA");
            PL (CU, "Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate");
            PL (CU, "  (" & Ada_TC_Name (Node) & ");");
            DI (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It : Node_Iterator;
               Bound_Node : Node_Id;
               Number : Natural := 0;
            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  Put (CU, "for I"
                       & Img (Number)
                       & " in 0 .. ");
                  Gen_Node_Stubs_Spec (CU, Bound_Node);
                  PL (CU, " - 1 loop");
                  Number := Number + 1;
                  II (CU);
               end loop;

               PL (CU, "CORBA.Add_Aggregate_Element (Result,");
               Add_With (CU, Ada_Helper_Name (Type_Node));
               Put (CU, "                             "
                    & Ada_Helper_Name (Type_Node)
                    & ".To_Any (Item (I0");
               for I in 1 .. Number - 1 loop
                  Put (CU, ", I" & Img (I));
               end loop;
               PL (CU, ")));");
               for I in 1 .. Number loop
                  DI (CU);
                  PL (CU, "end loop;");
               end loop;

            end;
            PL (CU, "return Result;");

         else
            Add_With (CU, Ada_Helper_Name (Type_Node));
            Add_With (CU, "CORBA");
            PL (CU, "Result : CORBA.Any := "
                & Ada_Helper_Name (Type_Node)
                & ".To_Any ("
                & Ada_Type_Name (Type_Node)
                & " (Item));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.Set_Type (Result, "
                & Ada_TC_Name (Node)
                & ");");
            PL (CU, "return Result;");
         end if;

         DI (CU);
         PL (CU, "end To_Any;");

      end if;
   end Gen_Type_Declarator_Body;

   -----------------------
   -- Gen_Sequence_Spec --
   -----------------------

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id) is
   begin
      if Generate_Dyn then
         --  TypeCode

         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Node)
             & " : CORBA.TypeCode.Object");
         PL (CU, "  := CORBA.TypeCode.TC_Sequence;");

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Fill in typecode TC_<name of the type>

         Add_Elaborate_Body (CU);
      end if;
   end Gen_Sequence_Spec;

   -----------------------
   -- Gen_Sequence_Body --
   -----------------------

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Seq_Helper_Name : constant String
        := Ada_Name (Node) & "_Helper";
   begin
      if not Generate_Dyn then
         return;
      end if;

      if Bound (Sequence (Node)) = No_Node then
         Add_With (CU, "PolyORB.Sequences.Unbounded.Helper",
                   Elab_Control => Elaborate_All);
         --  WAG:3.15
      else
         Add_With (CU, "PolyORB.Sequences.Bounded.Helper",
                   Elab_Control => Elaborate_All);
         --  WAG:3.15
      end if;

      Add_With (CU, Ada_Helper_Name (Sequence_Type (Sequence (Node))));

      NL (CU);
      PL (CU, "package " & Seq_Helper_Name
          & " is new " & Ada_Name (Node) & ".Helper");
      Put (CU, "  (");
      II (CU);

      PL (CU, "Element_To_Any =>" & ASCII.LF
          & "  " & Ada_Helper_Name
          (Sequence_Type (Sequence (Node))) & ".To_Any,");
      PL (CU, "Element_From_Any =>" & ASCII.LF
          & "  " & Ada_Helper_Name
          (Sequence_Type (Sequence (Node))) & ".From_Any);");

      DI (CU);

      NL (CU);
      Gen_From_Any_Profile (CU, Node);
      NL (CU);
      PL (CU, " renames " & Seq_Helper_Name & ".From_Any;");
      Gen_To_Any_Profile (CU, Node);
      NL (CU);
      PL (CU, " renames " & Seq_Helper_Name & ".To_Any;");

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, Seq_Helper_Name & ".Initialize ("
          & Ada_Full_TC_Name
          (Sequence_Type (Sequence (Node))) & ");");
      PL (CU, Ada_TC_Name (Node) & " := "
          & Seq_Helper_Name & ".Sequence_TC;");
      Divert (CU, Visible_Declarations);
   end Gen_Sequence_Body;

   --------------------
   -- Gen_Fixed_Spec --
   --------------------

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Decl_Node : constant Node_Id := Head (Declarators (Node));
   begin
      if Generate_Dyn then
         --  TypeCode
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, Ada_TC_Name (Decl_Node)
             & " : CORBA.TypeCode.Object :="
             & " CORBA.TypeCode.TC_Fixed;");
         --  From_Any
         NL (CU);
         Gen_From_Any_Profile (CU, Decl_Node);
         PL (CU, ";");
         --  To_Any
         NL (CU);
         Gen_To_Any_Profile (CU, Decl_Node);
         PL (CU, ";");
         --  Fill in typecode TC_<name of the type>
         Add_Elaborate_Body (CU);
      end if;
   end Gen_Fixed_Spec;

   --------------------
   -- Gen_Fixed_Body --
   --------------------

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Node      : in     Node_Id)
   is
      Decl_Node : constant Node_Id := Head (Declarators (Node));
      Type_Name : constant String := Ada_Name (Decl_Node);
   begin
      if Generate_Dyn then
         NL (CU);
         Add_With (CU, "CORBA");
         PL (CU, "package CDR_"
             & Type_Name & " is");
         Add_With
           (CU, "CORBA.Fixed_Point",
            Elab_Control => Elaborate_All);
         PL (CU, "  new CORBA.Fixed_Point ("
             & Ada_Full_Name (Decl_Node) & ");");
         --  From_Any
         Gen_From_Any_Profile
           (CU, Decl_Node);
         PL (CU, " renames CDR_" & Type_Name
             & ".From_Any;");
         --  To_Any
         Gen_To_Any_Profile
           (CU, Decl_Node);
         PL (CU, " renames CDR_" & Type_Name
             & ".To_Any;");
         --  Fill in typecode TC_<name of the type>
         Divert (CU, Deferred_Initialization);
         NL (CU);
         Put (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Decl_Node)
             & ", CORBA.To_Any (CORBA.Unsigned_Short (");
         Gen_Constant_Value (CU, Digits_Nb (T_Type (Node)));
         PL (CU, ")));");
         Put (CU, "CORBA.TypeCode.Add_Parameter ("
             & Ada_TC_Name (Decl_Node)
             & ", CORBA.To_Any (CORBA.Short (");
         Gen_Constant_Value (CU, Scale (T_Type (Node)));
         PL (CU, ")));");
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Fixed_Body;

   ------------------
   -- Gen_Array_TC --
   ------------------

   procedure Gen_Array_TC
     (CU        : in out Compilation_Unit;
      Element_Type_Node : in     Node_Id;
      Decl_Node : in     Node_Id)
   is

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Element_Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id);

      procedure Rec_Gen_Array_TC
        (CU             : in out Compilation_Unit;
         It             : in out Node_Iterator;
         First_Bound    : in     Boolean;
         Index          : in     Integer;
         Element_Type_Node      : in     Node_Id;
         Decl_Node      : in     Node_Id) is
         Bound_Node : Node_Id;
         Last_Bound : Boolean := False;
      begin
         Get_Next_Node (It, Bound_Node);
         if not Is_End (It) then
            Rec_Gen_Array_TC
              (CU, It, False, Index + 1,
               Element_Type_Node, Decl_Node);
         else
            Last_Bound := True;
         end if;
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         Put (CU, ", CORBA.To_Any (CORBA.Unsigned_Long (");
         Gen_Node_Stubs_Spec (CU, Bound_Node);
         PL (CU, ")));");
         Put (CU, "CORBA.TypeCode.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, "TC_" & Img (Index));
         end if;
         if Last_Bound then
            Put (CU, ", "
                 & "CORBA.To_Any ("
                 & Ada_Full_TC_Name (Element_Type_Node));
         else
            Put (CU, ", To_Any (TC_"
                 & Img (Index + 1));
         end if;
         PL (CU, "));");
      end Rec_Gen_Array_TC;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Add_With (CU, "CORBA");
      Rec_Gen_Array_TC
        (CU, Bounds_It, True, 0, Element_Type_Node, Decl_Node);
   end Gen_Array_TC;

   function Raise_From_Any_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_Exception);
      return "Raise_" & Ada_Name (Node) & "_From_Any";
   end Raise_From_Any_Name;

   function Type_Modifier (Node : in Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_ValueType);

      if Boolean'Pos (Abst (Node))
        + Boolean'Pos (Custom (Node))
        + Boolean'Pos (Truncatable (Node)) > 1
      then
         --  A Value Type cannot be at the same time
         --  abstract, custom or trucatable
         raise Program_Error;
      end if;

      if Abst (Node) then
         return "CORBA.VTM_ABSTRACT";
      end if;

      if Custom (Node) then
         return "CORBA.VTM_CUSTOM";
      end if;

      if Truncatable (Node) then
         return "CORBA.VTM_TRUNCATABLE";
      end if;

      return "CORBA.VTM_NONE";

   end Type_Modifier;

   function Visibility (Node : in Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_State_Member);
      if Is_Public (Node) then
         return "CORBA.PUBLIC_MEMBER";
      else
         return "CORBA.PRIVATE_MEMBER";
      end if;
   end Visibility;
end Ada_Be.Idl2Ada.Helper;
