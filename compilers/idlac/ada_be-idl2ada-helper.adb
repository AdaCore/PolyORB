------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . H E L P E R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2011, Free Software Foundation, Inc.          --
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

with Ada.Strings.Unbounded;

with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Mappings.CORBA; use Ada_Be.Mappings.CORBA;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;
with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

with Idlac_Errors; use Idlac_Errors;
with Platform;     use Platform;
with String_Sets;  use String_Sets;
with Idlac_Utils;  use Idlac_Utils;

package body Ada_Be.Idl2Ada.Helper is

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada.helper");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   ----------------------
   -- Utility routines --
   ----------------------

   procedure Gen_From_Any_Profile
     (CU             : in out Compilation_Unit;
      Type_Node      : Node_Id;
      From_Container : Boolean);
   --  Generate the profile for the From_Any operation of a type.
   --  If From_Container is true, formal parameter is an Any_Container'Class,
   --  else it is an Any.

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : Node_Id);
   --  Generate the profile for the To_Any operation of a type

   procedure Gen_Raise_From_Any_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the Raise_<exception>_From_Any procedure for an
   --  exception. The name of the procedure is
   --  Raise_From_Any_Name (Node).

   procedure Gen_Raise_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the Raise_<exception> procedure for an exception.

   --------------------------------
   -- Aggregate content wrappers --
   --------------------------------

   procedure Gen_Aggregate_Content_Wrapper_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  For an aggregate type, generate an Aggregate_Content derived type with
   --  the declaration of the appropriate primitive operations.

   procedure Gen_Aggregate_Content_Wrapper_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the bodies of the Aggregate_Content primitive operations for
   --  Node.

   procedure Gen_Get_Aggregate_Element_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Get_Aggregate_Element primitive operation

   procedure Gen_Set_Aggregate_Element_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Set_Aggregate_Element primitive operation

   procedure Gen_Get_Aggregate_Count_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Get_Aggregate_Count primitive operation

   procedure Gen_Set_Aggregate_Count_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Set_Aggregate_Count primitive operation

   procedure Gen_Clone_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Clone primitive operation

   procedure Gen_Finalize_Value_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Finalize_Value primitive operation

   procedure Gen_Wrap_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the profile of the Wrap function

   -----------------------------------------------------------
   -- Specialised generation subprograms for each node kind --
   -----------------------------------------------------------

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for an interface declaration

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for an interface declaration

   procedure Gen_ValueType_Spec
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the spec of the helper package for a valuetype declaration

   procedure Gen_ValueType_Body
     (CU : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the body of the helper package for a valuetype declaration

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for an enum declaration

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for an enum declaration

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for a struct or an
   --  exception declaration

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for a struct or an
   --  exception declaration

   procedure Gen_String_Instance_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for a string instance

   procedure Gen_String_Instance_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for a string instance

   procedure Gen_Union_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for an union declaration

   procedure Gen_Union_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for an union declaration

   procedure Gen_Type_Declarator_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for an array declaration

   procedure Gen_Type_Declarator_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for an array declaration

   procedure Gen_Sequence_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the spec of the helper package for a sequence declaration

   procedure Gen_Sequence_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id);
   --  Generate the body of the helper package for a sequence declaration

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Decl_Node : Node_Id);
   --  Generate the spec of the helper package for a fixed type declaration

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Decl_Node : Node_Id);
   --  Generate the body of the helper package for a fixed type declaration

   procedure Gen_Array_TC
     (CU                : in out Compilation_Unit;
      Element_Type_Node : Node_Id;
      Decl_Node         : Node_Id);
   --  generate lines to fill in an array typecode
   --  only used in the type_declarator part of gen_node_body

   procedure Gen_Boxed_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the spec of the helper package for a valuebox declaration

   procedure Gen_Boxed_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate the body of the helper package for a valuebox declaration

   function Raise_From_Any_Name (Node : Node_Id) return String;
   --  Return the name of a procedure that raises that exception
   --  from an occurrence stored in an Any.

   function Raise_Name (Node : Node_Id) return String;
   --  Return the name of a procedure that raises that exception
   --  with specified members values.

   function Type_Modifier (Node : Node_Id) return String;
   --  Return the type modifier associed with the ValueType Node

   function Visibility (Node : Node_Id) return String;
   --  Return the visibility of a state member

   function Loop_Parameter (Dim : Natural) return String;
   --  Return a unique name for the Dim'th loop parameter
   --  for iteration over an array.

   ----------------------------------------------
   -- End of internal subprograms declarations --
   ----------------------------------------------

   Helper_Deps : String_Sets.Set;
   --  Cache of already recorded helper dependencies

   ---------------------------
   -- Add_Helper_Dependency --
   ---------------------------

   procedure Add_Helper_Dependency
     (CU          : in out Compilation_Unit;
      Helper_Name :        String)
   is
      Previous_Diversion : constant Diversion := Current_Diversion (CU);
      Dep_Key : constant String := Name (CU) & '/' & Helper_Name;
   begin
      if Contains (Helper_Deps, Dep_Key) then
         --  This dependency is already set
         return;
      end if;
      Insert (Helper_Deps, Dep_Key);

      Add_With (CU, Helper_Name);
      Divert (CU, Initialization_Dependencies);
      if Helper_Name = "CORBA.Object.Helper" then
         PL (CU, "& ""corba.object""");

      elsif Helper_Name = "CORBA.Helper" then
         PL (CU, "& ""corba.helper""");

      elsif Helper_Name /= "CORBA"
        and then Helper_Name /= Name (CU)
      then
         PL (CU, "& """ & Helper_Name & """");
      end if;
      Divert (CU, Previous_Diversion);
   end Add_Helper_Dependency;

   ----------------------------------------
   -- Gen_Aggregate_Content_Wrapper_Spec --
   ----------------------------------------

   procedure Gen_Aggregate_Content_Wrapper_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      Gen_Wrap_Profile (CU, Node);
      PL (CU, ";");
   end Gen_Aggregate_Content_Wrapper_Spec;

   ----------------------------------------
   -- Gen_Aggregate_Content_Wrapper_Body --
   ----------------------------------------

   procedure Gen_Aggregate_Content_Wrapper_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Members_Count : Integer;
      --  Members count for this aggregate kind if known at compile time;
      --  -1 if dynamic.

      Dim : Integer;
      --  Dimensionality, for the array case

      NK  : constant Node_Kind := Kind (Node);

      Index : Natural;
      It : Node_Iterator;
      M_Node : Node_Id;

   begin

      --  Content_<type> and declarations for its overriding subprograms

      Add_With (CU, "PolyORB.Types");
      NL (CU);

      if NK = K_Declarator then
         Dim := Length (Array_Bounds (Node));
         if Dim > 1 then
            PL (CU, "type " & T_Indices & Ada_Name (Node) & " is array (1 .."
                & Integer'Image (Dim - 1) & ") of Integer;");
         end if;
      end if;

      PL (CU, "type " & T_Ptr & Ada_Name (Node) &
          " is access all " & Ada_Type_Name (Node) & ";");
      PL (CU, "type " & T_Content & Ada_Name (Node) & " is");
      PL (CU, "  new PolyORB.Any.Aggregate_Content with");
      PL (CU, "record");
      II (CU);
      PL (CU, "V : " & T_Ptr & Ada_Name (Node) & ";");

      case NK is
         when K_Declarator =>
            if Dim > 1 then
               PL (CU, "Dimen   : Positive;");
               PL (CU, "Indices : " & T_Indices & Ada_Name (Node) & ";");
            end if;

         when K_Enum =>
            PL (CU, "Repr_Cache : aliased PolyORB.Types.Unsigned_Long;");

         when K_Union =>
            PL (CU, "Switch_Cache : aliased "
                & Ada_Type_Name (Switch_Type (Node)) & ";");

         when others =>
            null;
      end case;

      DI (CU);
      PL (CU, "end record;");
      NL (CU);

      Gen_Get_Aggregate_Element_Profile (CU, Node);
      PL (CU, ";");

      case NK is
         when K_Enum | K_Union =>
            Gen_Set_Aggregate_Element_Profile (CU, Node);
            PL (CU, ";");

         when others =>
            null;

      end case;

      --  XXX Ada 2005: These should be declared 'overriding'

      Gen_Get_Aggregate_Count_Profile (CU, Node);
      PL (CU, ";");

      Gen_Set_Aggregate_Count_Profile (CU, Node);
      PL (CU, ";");

      Gen_Clone_Profile (CU, Node);
      PL (CU, ";");

      Gen_Finalize_Value_Profile (CU, Node);
      PL (CU, ";");

      --  Pre-compute members count, if appropriate

      case NK is
         when K_Enum =>
            Members_Count := 1;

         when K_Union =>
            Members_Count := 2;

         when K_Struct =>
            Members_Count := -1;
            --  Will be updated below while generating Get_Aggregate_Element.
            --  We can't just use Length (Members (Node)) because some members
            --  may have several declarators.

         when K_Declarator =>
            Members_Count := -1;

         when others =>
            Error
              ("No Member_Count for " & NK'Img & " nodes.",
               Fatal, Get_Location (Node));

            --  Keep the compiler happy

            raise Program_Error;
      end case;

      --  Array lengths list

      if NK = K_Declarator then
         declare
            Bound_Node : Node_Id;
            Bounds_It  : Node_Iterator;
            Index      : Positive;
         begin
            NL (CU);
            Put (CU, T_Lengths & Ada_Name (Node) & " : constant array (1 .."
                & Dim'Img & ") of PolyORB.Types.Unsigned_Long := (");

            Index := 1;
            Init (Bounds_It, Array_Bounds (Node));
            while not Is_End (Bounds_It) loop
               Get_Next_Node (Bounds_It, Bound_Node);

               if Index > 1 then
                  Put (CU, ", ");
               end if;

               Put (CU, Img (Index) & " => ");
               Gen_Constant_Value (CU, Expr => Bound_Node, Typ => No_Node);

               Index := Index + 1;
            end loop;
            PL (CU, ");");
         end;
      end if;

      --  Get_Aggregate_Element

      NL (CU);
      Gen_Get_Aggregate_Element_Profile (CU, Node);
      NL (CU);
      PL (CU, "is");
      II (CU);
      PL (CU, "use type PolyORB.Types.Unsigned_Long;");
      PL (CU, "use type PolyORB.Any.Mechanism;");

      --  ACC.V might be uninitialized and have an invalid representation
      --  (case of Get_Aggregate_Element being called from within an
      --  unmarshall routine), in which case we know that we will
      --  overwrite it without using the invalid value; we must
      --  disable validity checks here so that we do not fail a runtime
      --  check on the bogus value.

      PL (CU, "pragma Suppress (" & Validity_Check_Name & ");");

      --  The TypeCode formal is of no use here (we always statically know
      --  the type of each aggregate element).

      PL (CU, "pragma Unreferenced (TC);");

      if NK = K_Enum then

         --  An enum always has exactly one element, so we can ignore the
         --  provided index.

         PL (CU, "pragma Unreferenced (Index);");
      end if;

      DI (CU);
      PL (CU, "begin");
      II (CU);

      case NK is
         when K_Enum =>
            PL (CU, "ACC.Repr_Cache := "
                & Ada_Type_Name (Node) & "'Pos (ACC.V.all);");
            PL (CU, "Mech.all := PolyORB.Any.By_Value;");
            PL (CU, "return PolyORB.Any.Wrap "
                  & "(ACC.Repr_Cache'Unrestricted_Access);");

         when K_Struct =>

            PL (CU, "Mech.all := PolyORB.Any.By_Reference;");
            PL (CU, "case Index is");
            II (CU);
            Index := 0;
            Init (It, Members (Node));
            while not Is_End (It) loop
               Get_Next_Node (It, M_Node);
               declare
                  M_Typ  : constant Node_Id := M_Type (M_Node);
                  It2    : Node_Iterator;
                  M_Decl : Node_Id;
               begin
                  Init (It2, Decl (M_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, M_Decl);
                     PL (CU, "when" & Index'Img & " =>");
                     II (CU);
                     Put (CU, "return ");
                     Gen_Wrap_Call
                       (CU, M_Typ, "ACC.V." & Ada_Name (M_Decl));
                     PL (CU, ";");
                     DI (CU);
                     Index := Index + 1;
                  end loop;
               end;
            end loop;
            PL (CU, "when others =>");
            II (CU);
            PL (CU, "raise Constraint_Error;");
            DI (CU);
            DI (CU);
            PL (CU, "end case;");

            Members_Count := Index;

         when K_Declarator =>
            PL (CU, "Mech.all := PolyORB.Any.By_Reference;");

            if Dim > 1 then
               PL (CU, "if ACC.Dimen < " & Img (Dim) & " then");
               II (CU);
               PL (CU, "declare");
               II (CU);
               PL (CU, "R_ACC : " & T_Content & Ada_Name (Node)
                   & " := ACC.all;");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "R_ACC.Indices (R_ACC.Dimen) := Integer (Index);");
               PL (CU, "R_ACC.Dimen := R_ACC.Dimen + 1;");
               PL (CU, "return R_ACC;");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
               PL (CU, "else");
               II (CU);
            end if;

            declare
               use Ada.Strings.Unbounded;
               Elt_Reference : Unbounded_String;
            begin
               Put (CU, "return ");

               Elt_Reference := To_Unbounded_String ("ACC.V (");
               for J in 1 .. Dim - 1 loop
                  Append (Elt_Reference, "ACC.Indices (" & Img (J) & "), ");
               end loop;
               Append (Elt_Reference, "Integer (Index))");
               Gen_Wrap_Call (CU,
                 T_Type (Parent (Node)), To_String (Elt_Reference));
               PL (CU, ";");
            end;

            if Dim > 1 then
               DI (CU);
               PL (CU, "end if;");
            end if;

         when K_Union =>

            --  Discriminant case

            PL (CU, "if Index = 0 then");
            II (CU);

            --  Discriminant must be managed by value, because changing the
            --  discriminant value requires a complete record aggregate
            --  assignment. We provide a distinct component as we do not want
            --  the current discriminant to be altered in place.

            PL (CU, "Mech.all := PolyORB.Any.By_Value;");
            PL (CU, "ACC.Switch_Cache := ACC.V.Switch;");
            Put (CU, "return ");
            Gen_Wrap_Call (CU, Switch_Type (Node), "ACC.Switch_Cache");
            PL (CU, ";");
            DI (CU);
            PL (CU, "else");
            II (CU);

            --  Union member case

            PL (CU, "pragma Assert (Index = 1);");
            PL (CU, "Mech.all := PolyORB.Any.By_Reference;");
            PL (CU, "case ACC.V.Switch is");

            --  XXX This block is duplicated from Gen_Union_Body and should
            --  be factored.

            declare
               It          : Node_Iterator;
               Case_Node   : Node_Id;
               J           : Long_Integer := 0;
               Has_Default : Boolean := False;
            begin
               Init (It, Cases (Node));
               while not Is_End (It) loop
                  Get_Next_Node (It, Case_Node);

                  II (CU);
                  declare
                     It2         : Node_Iterator;
                     Label_Node  : Node_Id;
                     First_Label : Boolean := True;
                  begin
                     if Default_Index (Node) = J then
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
                           Gen_Constant_Value (CU,
                             Expr => Label_Node, Typ => Switch_Type (Node));
                        end loop;
                     end if;
                     PL (CU, " =>");
                     II (CU);
                     Put (CU, "return ");
                     Gen_Wrap_Call (CU, Case_Type (Case_Node), "ACC.V."
                       & Ada_Name (Case_Decl (Case_Node)));
                     PL (CU, ";");
                     J := J + 1;
                     DI (CU);
                     DI (CU);
                  end;
               end loop;

               if not Has_Default then
                  Gen_When_Others_Clause (CU);
               end if;
            end;

            PL (CU, "end case;");
            DI (CU);
            PL (CU, "end if;");

         when others =>
            null;

      end case;
      DI (CU);
      PL (CU, "end Get_Aggregate_Element;");

      --  Set_Aggregate_Element

      if NK = K_Enum or else NK = K_Union then
         NL (CU);
         Gen_Set_Aggregate_Element_Profile (CU, Node);
         NL (CU);
         PL (CU, "is");
         II (CU);
         PL (CU, "pragma Unreferenced (TC);");
         PL (CU, "use type PolyORB.Types.Unsigned_Long;");
         PL (CU, "pragma Assert (Index = 0);");

         if NK = K_Enum then
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "ACC.V.all := " & Ada_Type_Name (Node) & "'Val ("
                & "PolyORB.Types.Unsigned_Long'"
                & "(PolyORB.Any.From_Any (From_C)));");

         else
            declare
               ST_Node : constant Node_Id := Switch_Type (Node);
               S_Helper_Name : constant String := Helper_Unit (ST_Node);
            begin
               PL (CU, "New_Switch : constant " & Ada_Type_Name (ST_Node)
                   & " := " & S_Helper_Name & ".From_Any (From_C);");
            end;
            PL (CU, "New_Union : "
                & Ada_Type_Name (Node) & " (Switch => New_Switch);");
            PL (CU, "pragma Warnings (Off, New_Union);");
            PL (CU, "--  Use default initialization");
            NL (CU);
            PL (CU, "pragma Suppress (Discriminant_Check);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "ACC.V.all := New_Union;");
         end if;

         DI (CU);
         PL (CU, "end Set_Aggregate_Element;");
      end if;

      --  Get_Aggregate_Count

      NL (CU);
      Gen_Get_Aggregate_Count_Profile (CU, Node);
      NL (CU);
      PL (CU, "is");
      II (CU);

      if Members_Count > 0 or else (NK = K_Declarator and then Dim = 1) then
         PL (CU, "pragma Unreferenced (ACC);");
      end if;

      DI (CU);
      PL (CU, "begin");
      II (CU);

      if Members_Count >= 0 then
         PL (CU, "return" & Members_Count'Img & ";");

      elsif NK = K_Declarator then
         Put (CU, "return " & T_Lengths & Ada_Name (Node) & " (");
         if Dim > 1 then
            Put (CU, "ACC.Dimen");
         else
            Put (CU, "1");
         end if;
         PL (CU, ");");

      else
         pragma Assert (Kind (Node) = K_Sequence);
         PL (CU, "return PolyORB.Types.Unsigned_Long");
         PL (CU, "  (" & Ada_Name (Node) & ".Length (ACC.V.all));");
      end if;
      DI (CU);
      PL (CU, "end Get_Aggregate_Count;");

      --  Set_Aggregate_Count

      NL (CU);
      Gen_Set_Aggregate_Count_Profile (CU, Node);
      NL (CU);
      PL (CU, "is");
      II (CU);
      PL (CU, "use type PolyORB.Types.Unsigned_Long;");
      if Members_Count > 0 or else (NK = K_Declarator and then Dim = 1) then
         PL (CU, "pragma Unreferenced (ACC);");
      end if;
      DI (CU);
      PL (CU, "begin");
      II (CU);

      if Members_Count >= 0 then
         PL (CU, "if Count /=" & Members_Count'Img & " then");
         II (CU);
         PL (CU, "raise Program_Error;");
         DI (CU);
         PL (CU, "end if;");

      else
         pragma Assert (NK = K_Declarator);

         Put (CU, "if Count /= "
              & T_Lengths & Ada_Name (Node) & " (");
         if Dim > 1 then
            Put (CU, "ACC.Dimen");
         else
            Put (CU, "1");
         end if;
         PL (CU, ") then");
         II (CU);
         PL (CU, "raise Program_Error;");
         DI (CU);
         PL (CU, "end if;");
      end if;

      DI (CU);
      PL (CU, "end Set_Aggregate_Count;");

      --  Clone

      NL (CU);
      Gen_Clone_Profile (CU, Node);
      NL (CU);
      PL (CU, "is");
      II (CU);
      PL (CU, "use type PolyORB.Any.Content_Ptr;");
      PL (CU, "Target : PolyORB.Any.Content_Ptr;");
      if NK = K_Union then
         PL (CU, "pragma Suppress (Discriminant_Check);");
      end if;
      DI (CU);

      declare
         Target_Obj : constant String :=
                        T_Content & Ada_Name (Node) & " (Target.all)";

         procedure Assign_Component (Comp_Name : String);
         --  Generate assignment of named component of ACC to same component
         --  of Target.

         procedure Assign_Component (Comp_Name : String) is
         begin
            PL (CU, Target_Obj & "." & Comp_Name
                & ":= ACC." & Comp_Name & ";");
         end Assign_Component;

      begin
         PL (CU, "begin");
         II (CU);
         PL (CU, "if Into /= null then");
         II (CU);
         PL (CU, "if Into.all not in "
             & T_Content & Ada_Name (Node) & " then");
         II (CU);
         PL (CU, "return null;");
         DI (CU);
         PL (CU, "end if;");
         PL (CU, "Target := Into;");
         PL (CU, Target_Obj & ".V.all := ACC.V.all;");
         DI (CU);
         PL (CU, "else");
         II (CU);
         PL (CU, "Target := new " & T_Content & Ada_Name (Node) & ";");

         --  Ensure the allocation here is at least as large as the actual
         --  value (can't use the default discriminant, as the allocator
         --  allocates constrained object, which might be too small).

         PL (CU, T_Content & Ada_Name (Node) & " (Target.all).V := "
             & "new " & Ada_Type_Name (Node) & "'(ACC.V.all);");
         DI (CU);
         PL (CU, "end if;");

         case NK is
            when K_Declarator =>
               if Dim > 1 then
                  Assign_Component ("Dimen");
                  Assign_Component ("Indices");
               end if;

            when K_Enum =>
               Assign_Component ("Repr_Cache");

            when K_Union =>
               Assign_Component ("Switch_Cache");

            when others =>
               null;

         end case;
      end;
      PL (CU, "return Target;");
      DI (CU);
      PL (CU, "end Clone;");

      --  Finalize_Value

      NL (CU);
      Gen_Finalize_Value_Profile (CU, Node);
      NL (CU);
      PL (CU, "is");
      II (CU);
      Add_With (CU, "Ada.Unchecked_Deallocation");
      PL (CU, "procedure Free is new Ada.Unchecked_Deallocation");
      PL (CU, "  ("
           & Ada_Type_Name (Node) & ", " & T_Ptr & Ada_Name (Node) & ");");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "Free (ACC.V);");
      DI (CU);
      PL (CU, "end Finalize_Value;");

      --  Wrap

      NL (CU);
      Gen_Wrap_Profile (CU, Node);
      PL (CU, " is");
      PL (CU, "begin");
      II (CU);
      Put (CU, "return " & T_Content & Ada_Name (Node)
          & "'(PolyORB.Any.Aggregate_Content with V => "
           & T_Ptr & Ada_Name (Node) & " (X)");

      case NK is
         when K_Declarator =>
            if Dim > 1 then
               PL  (CU, ",");
               PL  (CU, "  Dimen => 1,");
               Put (CU, "  Indices => (others => 0)");
            end if;

         when K_Enum =>
            PL (CU, ",");
            Put (CU, "  Repr_Cache => 0");

         when K_Union =>
            PL (CU, ",");
            Put (CU, "  Switch_Cache => X.Switch");

         when others =>
            null;
      end case;

      PL (CU, ");");
      DI (CU);
      PL (CU, "end Wrap;");

   end Gen_Aggregate_Content_Wrapper_Body;

   ---------------------------------------
   -- Gen_Get_Aggregate_Element_Profile --
   ---------------------------------------

   procedure Gen_Get_Aggregate_Element_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      PL (CU, "function Get_Aggregate_Element");
      PL (CU, "  (ACC   : not null access "
              & T_Content & Ada_Name (Node) & ";");
      PL (CU, "   TC    : PolyORB.Any.TypeCode.Object_Ptr;");
      PL (CU, "   Index : PolyORB.Types.Unsigned_Long;");
      Put (CU, "   Mech  : not null access PolyORB.Any.Mechanism)"
           & " return PolyORB.Any.Content'Class");
   end Gen_Get_Aggregate_Element_Profile;

   -------------------------------------
   -- Gen_Get_Aggregate_Count_Profile --
   -------------------------------------

   procedure Gen_Get_Aggregate_Count_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      Add_With (CU, "PolyORB.Types");
      PL (CU, "function Get_Aggregate_Count");
      Put (CU, "  (ACC : " & T_Content & Ada_Name (Node)
          & ") return PolyORB.Types.Unsigned_Long");
   end Gen_Get_Aggregate_Count_Profile;

   -------------------------------------
   -- Gen_Set_Aggregate_Count_Profile --
   -------------------------------------

   procedure Gen_Set_Aggregate_Count_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      PL (CU, "procedure Set_Aggregate_Count");
      PL (CU, "  (ACC : in out " & T_Content & Ada_Name (Node) & ";");
      Put (CU, "   Count : PolyORB.Types.Unsigned_Long)");
   end Gen_Set_Aggregate_Count_Profile;

   ---------------------------------------
   -- Gen_Set_Aggregate_Element_Profile --
   ---------------------------------------

   procedure Gen_Set_Aggregate_Element_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      PL (CU, "procedure Set_Aggregate_Element");
      PL (CU, "  (ACC    : in out " & T_Content & Ada_Name (Node) & ";");
      PL (CU, "   TC     : PolyORB.Any.TypeCode.Object_Ptr;");
      PL (CU, "   Index  : PolyORB.Types.Unsigned_Long;");
      Put (CU, "   From_C : in out PolyORB.Any.Any_Container'Class)");
   end Gen_Set_Aggregate_Element_Profile;

   -----------------------
   -- Gen_Clone_Profile --
   -----------------------

   procedure Gen_Clone_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      PL (CU, "function Clone");
      PL (CU, "  (ACC  : " & T_Content & Ada_Name (Node) & ";");
      Put (CU, "   Into : PolyORB.Any.Content_Ptr := null) "
           & "return PolyORB.Any.Content_Ptr");
   end Gen_Clone_Profile;

   --------------------------------
   -- Gen_Finalize_Value_Profile --
   --------------------------------

   procedure Gen_Finalize_Value_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      PL (CU, "procedure Finalize_Value");
      Put (CU, "  (ACC : in out " & T_Content & Ada_Name (Node) & ")");
   end Gen_Finalize_Value_Profile;

   -------------------
   -- Gen_Node_Spec --
   -------------------

   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      case Kind (Node) is

         when K_Interface =>
            Gen_Interface_Spec (CU, Node);

         when K_Enum =>
            Gen_Aggregate_Content_Wrapper_Spec (CU, Node);
            Gen_Enum_Spec (CU, Node);

         when K_Type_Declarator =>
            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Declarators (Node));
               if Kind (T_Type (Node)) = K_Fixed then
                  Get_Next_Node (It, Decl_Node);
                  pragma Assert (Is_End (It));
                  Gen_Fixed_Spec (CU, Decl_Node);

               else
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Spec (CU, Decl_Node);
                  end loop;
               end if;
            end;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Aggregate_Content_Wrapper_Spec (CU, Node);
               Gen_Struct_Exception_Spec (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Spec (CU, Node);

         when K_Union =>
            Gen_Aggregate_Content_Wrapper_Spec (CU, Node);
            Gen_Union_Spec (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Spec (CU, Node);

         when K_ValueType =>
            Gen_Aggregate_Content_Wrapper_Spec (CU, Node);
            Gen_ValueType_Spec (CU, Node);

         when K_Boxed_ValueType =>
            Gen_Boxed_ValueType_Spec (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Spec (CU, Node);
            Gen_Raise_Profile (CU, Node);
            PL (CU, ";");
            PL (CU, "pragma No_Return ("
                & Raise_Name (Node) & ");");

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
            Gen_Aggregate_Content_Wrapper_Body (CU, Node);
            Gen_Enum_Body (CU, Node);

         when K_Type_Declarator =>
            declare
               It   : Node_Iterator;
               Decl_Node : Node_Id;
            begin
               Init (It, Declarators (Node));
               if Kind (T_Type (Node)) = K_Fixed then
                  Get_Next_Node (It, Decl_Node);
                  pragma Assert (Is_End (It));
                  Gen_Fixed_Body (CU, Decl_Node);

               else
                  while not Is_End (It) loop
                     Get_Next_Node (It, Decl_Node);
                     Gen_Type_Declarator_Body (CU, Decl_Node);
                  end loop;
               end if;
            end;

         when K_Struct =>
            if not Is_Exception_Members (Node) then
               Gen_Aggregate_Content_Wrapper_Body (CU, Node);
               Gen_Struct_Exception_Body (CU, Node);
            end if;

         when K_String_Instance =>
            Gen_String_Instance_Body (CU, Node);

         when K_Union =>
            Gen_Aggregate_Content_Wrapper_Body (CU, Node);
            Gen_Union_Body (CU, Node);

         when K_Sequence_Instance =>
            Gen_Sequence_Body (CU, Node);

         when K_ValueType =>
            Gen_Aggregate_Content_Wrapper_Body (CU, Node);
            Gen_ValueType_Body (CU, Node);

         when K_Boxed_ValueType =>
            Gen_Boxed_ValueType_Body (CU, Node);

         when K_Exception =>
            Gen_Struct_Exception_Body (CU, Node);

            if not Has_Local_Component (Node) then
               Gen_Raise_From_Any_Profile (CU, Node);
               PL (CU, ";");
               PL (CU, "pragma No_Return ("
                   & Raise_From_Any_Name (Node) & ");");

               Gen_Raise_From_Any_Profile (CU, Node);
               PL (CU, "");
               PL (CU, "is");
               II (CU);
               PL (CU, "Members : constant "
                   & Ada_Name (Members_Type (Node))
                   & " := From_Any (CORBA.Any (Item));");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               Add_With (CU, "PolyORB.Exceptions");
               PL (CU, "PolyORB.Exceptions.User_Raise_Exception");
               PL (CU, "  (" & Ada_Name (Node) & "'Identity,");
               II (CU);
               PL (CU, "Members,");
               PL (CU, "Message);");
               DI (CU);
               DI (CU);
               PL (CU, "end " & Raise_From_Any_Name (Node) & ";");

               --  Register raiser

               --  This has to be done in deferred initialization, after the
               --  TypeCode has been constructed.

               Divert (CU, Deferred_Initialization);

               PL (CU, "PolyORB.Exceptions.Register_Exception");
               PL (CU, "  (CORBA.TypeCode.Internals.To_PolyORB_Object ("
                       & Ada_TC_Name (Node) & "),");
               II (CU);
               PL (CU, Raise_From_Any_Name (Node) & "'Access);");
               DI (CU);
               Divert (CU, Initialization_Dependencies);
               PL (CU, "& ""exceptions""");
               Divert (CU, Visible_Declarations);
            end if;

            Gen_Raise_Profile (CU, Node);
            PL (CU, "");
            PL (CU, "is");
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "PolyORB.Exceptions");
            PL (CU, "PolyORB.Exceptions.User_Raise_Exception");
            PL (CU, "  (" & Ada_Name (Node) & "'Identity,");
            II (CU);
            PL (CU, "Members);");
            DI (CU);
            DI (CU);
            PL (CU, "end " & Raise_Name (Node) & ";");

         when others =>
            null;
      end case;
   end Gen_Node_Body;

   --------------------------
   -- Gen_From_Any_Profile --
   --------------------------

   procedure Gen_From_Any_Profile
     (CU             : in out Compilation_Unit;
      Type_Node      : Node_Id;
      From_Container : Boolean)
   is
   begin
      if From_Container then
         Add_With (CU, "PolyORB.Any");
         Put (CU, "function From_Any (C : PolyORB.Any.Any_Container'Class) "
              & "return " & Ada_Type_Name (Type_Node));
      else
         Put (CU, "function From_Any (Item : CORBA.Any) "
              & "return " & Ada_Type_Name (Type_Node));
      end if;
   end Gen_From_Any_Profile;

   ------------------------
   -- Gen_To_Any_Profile --
   ------------------------

   procedure Gen_To_Any_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : Node_Id)
   is
   begin
      PL (CU, "function To_Any");
      Put (CU, "  (Item : " & Ada_Type_Name (Type_Node)
          & ") return CORBA.Any");
   end Gen_To_Any_Profile;

   --------------------------------
   -- Gen_Raise_From_Any_Profile --
   --------------------------------

   procedure Gen_Raise_From_Any_Profile
     (CU : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      Add_With (CU, "PolyORB.Any");
      Add_With (CU, "PolyORB.Std");
      PL  (CU, "");
      PL  (CU, "procedure " & Raise_From_Any_Name (Node));
      PL  (CU, "  (Item    : PolyORB.Any.Any;");
      Put (CU, "   Message : PolyORB.Std.String)");
   end Gen_Raise_From_Any_Profile;

   -----------------------
   -- Gen_Raise_Profile --
   -----------------------

   procedure Gen_Raise_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      PL (CU, "");
      PL (CU, "procedure " & Raise_Name (Node));
      Put (CU, "  (Members : " & Ada_Name (Members_Type (Node)) & ")");
   end Gen_Raise_Profile;

   ------------------------
   -- Gen_Interface_Spec --
   ------------------------

   procedure Gen_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
   begin
      --  Unchecked_To_<reference>

      declare
         Short_Type_Name : constant String :=
                             Ada_Type_Defining_Name (Mapping, Node);
         Type_Name : constant String := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)"
               & " return " & Type_Name & ";");
         NL (CU);
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)"
               & " return " & Type_Name & ";");
      end;

      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      if not Local (Node) then

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

      end if;
   end Gen_Interface_Spec;

   --------------------------------
   -- Gen_Forward_Interface_Spec --
   --------------------------------

   procedure Gen_Forward_Interface_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Short_Type_Name : constant String :=
                             Ada_Type_Defining_Name (Mapping, Node);
         Type_Name : constant String := Ada_Type_Name (Node);
      begin
         Add_With (CU, "CORBA.Object");
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)"
               & " return " & Type_Name & ";");
         NL (CU);
         PL (CU, "function To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)"
               & " return " & Type_Name & ";");
      end;

      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      if not Local (Node) then

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU,  Node);
         PL (CU, ";");

      end if;
   end Gen_Forward_Interface_Spec;

   ------------------------
   -- Gen_ValueType_Spec --
   ------------------------

   procedure Gen_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Type_Name : constant String := Ada_Type_Defining_Name (Mapping, Node);

      Type_Full_Name : constant String
        := Ada_Type_Name (Node);
      V_Impl_Name : constant String
        := Ada_Name (Node) & ".Value_Impl.Object'Class";
   begin
      pragma Assert (Kind (Node) = K_ValueType);
      Add_With (CU, "CORBA.Value");
      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : CORBA.Value.Base'Class)");
      PL (CU, "  return " & Type_Full_Name & ";");

      --  Generate code for supported non-abstract interfaces, if any

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

      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA");
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => False);
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
      PL (CU, "(Item              : CORBA.Any;");
      PL (CU, " Result_Ref        : in out " & Type_Full_Name & ";");
      PL (CU, " Unmarshalled_List : in out AnyRef_Seq.Sequence);");
      DI (CU);
      NL (CU);
      PL (CU, "procedure To_Any");
      II (CU);
      PL (CU, "(Item            : " & Type_Full_Name & ";");
      PL (CU, " Result          : in out CORBA.Any;");
      PL (CU, " Marshalled_List : in out RefAny_Seq.Sequence);");
      DI (CU);
      NL (CU);
   end Gen_ValueType_Spec;

   ------------------------
   -- Gen_ValueType_Body --
   ------------------------

   procedure Gen_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is

      Type_Name : constant String
        := Ada_Type_Defining_Name (Mapping, Node);

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
      PL (CU, "use PolyORB.Std;");
      PL (CU, "use PolyORB.CORBA_P.Value.Helper;");
      PL (CU, "use CORBA.Value;");

      NL (CU);
      PL (CU, "--  Logging for this package.");
      PL (CU, "package L is new PolyORB.Log.Facility_Log (""" & Name (CU)
          & """);");
      PL (CU, "procedure O (Message : PolyORB.Std.String; Level :" &
          " Log_Level := Debug)");
      PL (CU, "  renames L.Output;");
      NL (CU);

      PL (CU, "--  Pointer type for Value_Refs.");
      PL (CU, "type Value_Ptr is access Value_Ref;");
      NL (CU);

      NL (CU);
      PL (CU, "function To_" & Type_Name);
      PL (CU, "  (The_Ref : CORBA.Value.Base'Class)");
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
      PL (CU, "CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);");
      DI (CU);
      PL (CU, "end if;");
      DI (CU);
      PL (CU, "end To_" & Type_Name & ";");

      --  Generate code for supported non-abstract interfaces, if any

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

      NL (CU);
      PL (CU, "--  Wrappers for the recursive procedures");
      NL (CU);
      Add_With (CU, "PolyORB.CORBA_P.Value.Helper");
      Gen_From_Any_Profile (CU, Node, From_Container => False);
      PL (CU, " is");
      II (CU);
      PL (CU, "Result_Ref : " & Type_Full_Name & ";");
      PL (CU, "New_Sequence : AnyRef_Seq.Sequence := "
          & "AnyRef_Seq.Null_Sequence;");
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
      PL (CU, "New_Sequence : RefAny_Seq.Sequence :="
          & " RefAny_Seq.Null_Sequence;");
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
      PL (CU, "   (Item              : CORBA.Any;");
      PL (CU, "    Result_Ref        : in out " & Type_Full_Name & ";");
      PL (CU, "    Unmarshalled_List : in out AnyRef_Seq.Sequence)");
      PL (CU, "is");
      II (CU);
      PL (CU, "--  Get the ID, and then check the association list.");
      PL (CU, "ID_Tag : constant CORBA.Any :=");
      PL (CU, "           CORBA.Internals.Get_Aggregate_Element");
      PL (CU, "             (Item, CORBA.TC_String, "
                            & "CORBA.Unsigned_Long (0));");
      PL (CU, "Temp_String : constant CORBA.String :=");
      PL (CU, "                CORBA.From_Any (ID_Tag);");
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

      --  Type dependent section

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
                  Type_Node : constant Node_Id := State_Type (Member_Node);
                  Helper_Name : constant String := Helper_Unit (Type_Node);
                  TCU_Name    : constant String := TC_Unit (Type_Node);
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, State_Declarators (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);

                     PL (CU, "--  Common code");
                     PL (CU,
                         "Temp_Any := CORBA.Internals.Get_Aggregate_Element");
                     Add_Helper_Dependency (CU, TCU_Name);
                     Add_With (CU, Helper_Name);
                     PL (CU, "   (Item, "
                         & Ada_Full_TC_Name (Type_Node)
                         & ", CORBA.Unsigned_Long ("
                         & Natural'Image (Position) & "));");
                     PL (CU, "pragma Debug (O (""member"
                         & Natural'Image (Position)
                         & " = "" & CORBA.Image (Temp_Any)));");
                     declare
                        Decl_Name : constant String := Ada_Name (Decl_Node);
                     begin
                        if (Kind (Type_Node) = K_Scoped_Name)
                          and then ((Kind (Value (Type_Node)) = K_ValueType
                                      or else
                                    (Kind (Value (Type_Node)) =
                                             K_Forward_ValueType)))
                        then
                           PL (CU, "--  ValueType specific");
                           NL (CU);
                           PL (CU, "declare");
                           PL (CU, "   New_Ref : "
                               & Ada_Type_Name (State_Type (Member_Node))
                               & ";");
                           PL (CU, "begin");
                           II (CU);
                           PL (CU,
                               Helper_Name
                               & ".From_Any (Temp_Any, New_Ref,"
                               & " Unmarshalled_List);");
                           PL (CU, "Result." & Decl_Name & " := New_Ref;");
                           DI (CU);
                           PL (CU, "end;");
                        else
                           PL (CU, "--  Regular member.");
                           PL (CU, "Result." & Decl_Name
                               & " := "
                               & Helper_Name
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
      PL (CU, "   AnyRef_Seq.Get_Element (Unmarshalled_List, Index);");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "pragma Debug (O (""pointer to "" & My_ID));");
      PL (CU, "Set (Result_Ref, "
          & "CORBA.AbstractBase.Entity_Of (List_Item.Ref.all));");
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
      PL (CU, "(Item            : " & Type_Full_Name & ";");
      PL (CU, " Result          : in out CORBA.Any;");
      PL (CU, " Marshalled_List : in out RefAny_Seq.Sequence)");
      DI (CU);
      PL (CU, " is");
      II (CU);
      PL (CU, "My_ID : constant Any_ID := Get_ID (Result);");
      PL (CU, "Index : constant Natural :=");
      PL (CU, "   Find_Any.Index (Marshalled_List,"
          & " CORBA.AbstractBase.Ref (Item));");
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
      PL (CU, "Temp_Result := CORBA.Internals.Get_Empty_Any_Aggregate ("
          & Ada_TC_Name (Node) & ");");
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
      PL (CU, "CORBA.Internals.Add_Aggregate_Element");
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
                  Type_Node : constant Node_Id := State_Type (Member_Node);
                  Helper_Name : constant String := Helper_Unit (Type_Node);
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;
               begin
                  Init (It2, State_Declarators (Member_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     declare
                        Decl_Name : constant String := Ada_Name (Decl_Node);
                     begin
                        if (Kind (Type_Node) = K_Scoped_Name) and then
                          ((Kind (Value (Type_Node)) =
                               K_ValueType or else
                           (Kind (Value (Type_Node)) =
                               K_Forward_ValueType)))
                        then
                           PL (CU, "--  ValueType member.");
                           PL (CU, "declare");
                           PL (CU, "   Temp_Any : CORBA.Any;");
                           PL (CU, "begin");
                           II (CU);
                           PL (CU, "Temp_Any := "
                               & "CORBA.Internals.Get_Empty_Any_Aggregate"
                               & " ("
                               & Ada_Full_TC_Name (State_Type (Member_Node))
                               & ");");
                           PL (CU,
                               Helper_Name
                               & ".To_Any (Object_U." & Decl_Name
                               & ", Temp_Any, Marshalled_List);");
                           PL (CU, "pragma Debug (O (""To_Any: member="""
                               & " & CORBA.Image (Temp_Any)));");
                           PL (CU,
                               "CORBA.Internals.Add_Aggregate_Element "
                               & "(Temp_Result,");
                           PL (CU,
                               "                             Temp_Any);");
                           DI (CU);
                           PL (CU, "end;");

                        else
                           PL (CU, "--  Regular member.");
                           PL (CU, "CORBA.Internals.Add_Aggregate_Element");
                           PL (CU, "  (Temp_Result, "
                               & Helper_Name
                               & ".To_Any (Object_U."
                               & Decl_Name
                               & "));");
                           PL (CU,
                               " pragma Debug (O (""To_Any: member1="""
                               & " & CORBA.Image (CORBA.To_Any (Object_U."
                               & Decl_Name & "))));");
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
      PL (CU, "CORBA.Internals.Add_Aggregate_Element");
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
      PL (CU, "Name : constant CORBA.String :=");
      PL (CU, "   CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id   : constant CORBA.String :=");
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
                         & " : constant CORBA.String := "
                         & "CORBA.To_CORBA_String ("""
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

      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Value);");

      --  Put the name and repository Id for the value

      PL (CU, "CORBA.Internals.Add_Parameter");
      PL (CU, "  (" & Ada_TC_Name (Node) & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter");
      PL (CU, "  (" & Ada_TC_Name (Node) & ", CORBA.To_Any (Id));");

      --  Add the type modifier tag

      PL (CU, "CORBA.Internals.Add_Parameter");
      PL (CU, "  (" & Ada_TC_Name (Node)
          & ", CORBA.To_Any (CORBA.Short ("
          & Type_Modifier (Node) & ")));");

      --  Add the concrete base type
      --  XXX For the moment, a null TC is passed
      PL (CU, "CORBA.Internals.Add_Parameter");
      PL (CU, "  (" & Ada_TC_Name (Node)
          & ", CORBA.To_Any (CORBA.TC_Null));");

      --  Add visibility, type and name for each member

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
                     PL (CU, "CORBA.Internals.Add_Parameter");
                     II (CU);
                     PL (CU, "(" & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ( CORBA.Short ("
                         & Visibility (State_Member_Node_Id)
                         & ")));");
                     DI (CU);
                     PL (CU, "CORBA.Internals.Add_Parameter");
                     II (CU);
                     PL (CU, "(" & Ada_TC_Name (Node)
                         & ", CORBA.To_Any ("
                         & Ada_Full_TC_Name
                            (State_Type (State_Member_Node_Id))
                         & "));");
                     DI (CU);
                     PL (CU, "CORBA.Internals.Add_Parameter");
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

      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_ValueType_Body;

   ------------------------
   -- Gen_Interface_Body --
   ------------------------

   procedure Gen_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Type_Defining_Name : constant String
           := Ada_Type_Defining_Name (Mapping, Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         NL (CU);
         PL (CU, "function Unchecked_To_" & Type_Defining_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)");
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
         --
         --    Doing the check properly implies either
         --       1. querying the interface repository (not implemented yet);
         --    or 2. calling Is_A (Repository_Id) on an object reference whose
         --          type maps the actual (i. e. most derived) interface of
         --          The_Ref (which is impossible if that type is not
         --          known on the partition where To_Ref is called);
         --    or 3. a remote invocation of an Is_A method of the designated
         --          object.
         --
         --    The most general and correct solution to this problem is 3. When
         --    a remote call is not desired, the user should use
         --    Unchecked_To_Ref, whose purpose is precisely that.
         --
         --    This solution is implemented as a dispatching call to Is_A on
         --    the source object reference. The remote Is_A operation will be
         --    invoked if necessary.

         NL (CU);
         PL (CU, "function To_" & Type_Defining_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
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

         PL (CU, "CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);");
         DI (CU);
         PL (CU, "end To_" & Type_Defining_Name & ";");
      end;

      if not Local (Node) then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Mapping, Node)
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
         PL (CU, "CORBA.Internals.Set_Type (A, " & Ada_TC_Name (Node) & ");");
         PL (CU, "return A;");
         DI (CU);
         PL (CU, "end To_Any;");

      end if;

      --  Fill in the typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id   : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
          & """);");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Object);");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Id));");
      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_Interface_Body;

   --------------------------------
   -- Gen_Forward_Interface_Body --
   --------------------------------

   procedure Gen_Forward_Interface_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
   begin

      --  Unchecked_To_<reference>

      declare
         Short_Type_Name : constant String
           := Ada_Type_Defining_Name (Mapping, Node);
         Type_Name : constant String
           := Ada_Type_Name (Node);
      begin
         NL (CU);
         PL (CU, "function Unchecked_To_" & Short_Type_Name);
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)");
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
         PL (CU, "  (The_Ref : CORBA.Object.Ref'Class)");
         PL (CU, "  return " & Type_Name);
         PL (CU, "is");
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

         PL (CU, "CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);");
         DI (CU);
         PL (CU, "end To_" & Short_Type_Name & ";");
      end;

      if not Local (Node) then
         --  From_Any

         Add_With (CU, "CORBA.Object.Helper");
         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, " is");
         PL (CU, "begin");
         II (CU);
         PL (CU, "return To_"
             & Ada_Type_Defining_Name (Mapping, Node)
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
      end if;

      --  Fill in the typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Ada_Name (Forward (Node))
          & """);");
      PL (CU, "Id : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
          & """);");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Object);");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Id));");
      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_Forward_Interface_Body;

   -------------------
   -- Gen_Enum_Spec --
   -------------------

   procedure Gen_Enum_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
   begin
      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA");
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => True);
      PL (CU, ";");

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => False);
      PL (CU, ";");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Node);
      PL (CU, ";");
   end Gen_Enum_Spec;

   -------------------
   -- Gen_Enum_Body --
   -------------------

   procedure Gen_Enum_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id)
   is
   begin
      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => True);
      PL (CU, " is");
      PL (CU, "begin");
      II (CU);
      PL (CU, "return " & Ada_Full_Name (Node)
          & "'Val (PolyORB.Types.Unsigned_Long'("
          & "PolyORB.Any.Get_Aggregate_Element (C, 0)));");
      DI (CU);
      PL (CU, "end From_Any;");

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => False);
      PL (CU, " is");
      PL (CU, "begin");
      II (CU);
      PL (CU, "return From_Any (CORBA.Get_Container (Item).all);");
      DI (CU);
      PL (CU, "end From_Any;");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Node);
      PL (CU, " is");
      II (CU);
      PL (CU, "Result : CORBA.Any :=");
      II (CU);
      PL (CU, "CORBA.Internals.Get_Empty_Any_Aggregate ("
          & Ada_TC_Name (Node) & ");");
      DI (CU);
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "CORBA.Internals.Add_Aggregate_Element");
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
      PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
          & """);");
      declare
         It     : Node_Iterator;
         E_Node : Node_Id;
      begin
         Init (It, Enumerators (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, E_Node);
            PL (CU, Ada_Name (E_Node)
                & "_Name : constant CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (E_Node)
                & """);");
         end loop;
      end;

      DI (CU);
      PL (CU, "begin");
      II (CU);
      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Enum);");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Id));");

      declare
         It     : Node_Iterator;
         E_Node : Node_Id;
      begin
         Init (It, Enumerators (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, E_Node);
            PL (CU, "CORBA.Internals.Add_Parameter ("
                & Ada_TC_Name (Node)
                & ", CORBA.To_Any ("
                & Ada_Name (E_Node)
                & "_Name));");
         end loop;
      end;
      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_Enum_Body;

   -------------------------------
   -- Gen_Struct_Exception_Spec --
   -------------------------------

   procedure Gen_Struct_Exception_Spec
     (CU        : in out Compilation_Unit;
      Node      : Node_Id) is
      Struct_Node : Node_Id;
   begin
      --  Typecode generation

      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

      NL (CU);
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      if Kind (Node) = K_Struct then
         Struct_Node := Node;
      else
         Struct_Node := Members_Type (Node);
      end if;

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Struct_Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Struct_Node);
         PL (CU, ";");
      end if;
   end Gen_Struct_Exception_Spec;

   -------------------------------
   -- Gen_Struct_Exception_Body --
   -------------------------------

   procedure Gen_Struct_Exception_Body
     (CU        : in out Compilation_Unit;
      Node      : Node_Id)
   is
      Struct_Node : Node_Id;
      Is_Empty    : Boolean;
   begin
      if Kind (Node) = K_Struct then
         Struct_Node := Node;
      else
         Struct_Node := Members_Type (Node);
      end if;

      Is_Empty := Length (Members (Node)) = 0;

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Struct_Node, From_Container => False);
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
               It          : Node_Iterator;
               Member_Node : Node_Id;
               J           : Integer := 0;

            begin
               Init (It, Members (Struct_Node));

               while not Is_End (It) loop
                  Get_Next_Node (It, Member_Node);

                  declare
                     It2         : Node_Iterator;
                     Decl_Node   : Node_Id;
                     Type_Node   : constant Node_Id := M_Type (Member_Node);
                     Helper_Name : constant String  := Helper_Unit (Type_Node);
                     TCU_Name    : constant String  := TC_Unit (Type_Node);

                  begin
                     Add_Helper_Dependency (CU, TCU_Name);
                     Add_With (CU, Helper_Name);

                     Init (It2, Decl (Member_Node));

                     while not Is_End (It2) loop
                        Get_Next_Node (It2, Decl_Node);

                        PL (CU, "Index := "
                            & "CORBA.Internals.Get_Aggregate_Element (Item,");
                        Add_With (CU,
                                  Ada_Helper_Unit_Name
                                  (Mapping, M_Type (Member_Node)));

                        PL (CU,
                            "                                      "
                            & Ada_Full_TC_Name (Type_Node)
                            & ",");
                        PL (CU,
                            "                                      "
                            & "CORBA.Unsigned_Long ("
                            & Integer'Image (J)
                            &"));");
                        Add_With (CU, Helper_Name);
                        PL (CU, "Result_"
                            & Ada_Name (Decl_Node)
                            & " := "
                            & Helper_Name
                            & ".From_Any (Index);");
                        J := J + 1;
                     end loop;
                  end;
               end loop;
            end;

            PL (CU, "return");
            II (CU);

            declare
               First_Member  : Boolean := True;
               Begin_Of_Line : String (1 .. 1) := "(";
               End_Of_Line   : String (1 .. 2) := ", ";
               It            : Node_Iterator;
               Member_Node   : Node_Id;

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

                        if Is_End (It) and then Is_End (It2) then
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
         Put (CU, "Result : ");
         if Is_Empty then
            Put (CU, "constant ");
         end if;

         PL (CU, "CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Internals.Get_Empty_Any_Aggregate ("
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
                  Type_Node : constant Node_Id := M_Type (Member_Node);
                  Helper_Name : constant String := Helper_Unit (Type_Node);
                  It2   : Node_Iterator;
                  Decl_Node : Node_Id;

               begin
                  Init (It2, Decl (Member_Node));

                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Decl_Node);
                     PL (CU, "CORBA.Internals.Add_Aggregate_Element");
                     II (CU);
                     PL (CU, "(Result, " & Helper_Name & ".To_Any (Item."
                         & Ada_Name (Decl_Node) & "));");
                     DI (CU);
                  end loop;
               end;
            end loop;
         end;

         PL (CU, "return Result;");
         DI (CU);
         PL (CU, "end To_Any;");
      end if;

      --  Fill in typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
          & """);");

      declare
         It          : Node_Iterator;
         Member_Node : Node_Id;

      begin
         Init (It, Members (Struct_Node));

         while not Is_End (It) loop
            Get_Next_Node (It, Member_Node);

            declare
               It2       : Node_Iterator;
               Decl_Node : Node_Id;

            begin
               Init (It2, Decl (Member_Node));

               while not Is_End (It2) loop
                  Get_Next_Node (It2, Decl_Node);
                  PL (CU, "Arg_Name_"
                      & Ada_Name (Decl_Node)
                      & " : constant CORBA.String := CORBA.To_CORBA_String ("""
                      & Ada_Name (Decl_Node)
                      & """);");
               end loop;
            end;
         end loop;
      end;

      DI (CU);
      PL (CU, "begin");
      II (CU);

      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      Put (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
             & " (PolyORB.Any.TypeCode.");
      if Kind (Node) = K_Struct then
         Put (CU, "TC_Struct");
      else
         Put (CU, "TC_Except");
      end if;
      PL (CU, ");");

      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Node)
          & ", CORBA.To_Any (Id));");

      declare
         It          : Node_Iterator;
         Member_Node : Node_Id;

      begin
         Init (It, Members (Struct_Node));

         while not Is_End (It) loop
            Get_Next_Node (It, Member_Node);

            Add_With
              (CU, Ada_Helper_Unit_Name (Mapping, M_Type (Member_Node)));

            declare
               It2       : Node_Iterator;
               Decl_Node : Node_Id;

            begin
               Init (It2, Decl (Member_Node));

               while not Is_End (It2) loop
                  Get_Next_Node (It2, Decl_Node);
                  PL (CU, "CORBA.Internals.Add_Parameter ("
                      & Ada_TC_Name (Node)
                      & ", CORBA.To_Any ("
                      & Ada_Full_TC_Name (M_Type (Member_Node))
                      & "));");
                  PL (CU, "CORBA.Internals.Add_Parameter ("
                      & Ada_TC_Name (Node)
                      & ", CORBA.To_Any (Arg_Name_"
                      & Ada_Name (Decl_Node)
                      & "));");
               end loop;
            end;
         end loop;
      end;
      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_Struct_Exception_Body;

   ------------------------------
   -- Gen_String_Instance_Spec --
   ------------------------------

   procedure Gen_String_Instance_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      --  Typecode generation

      Add_With (CU, "CORBA");

      NL (CU);
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => False);
      PL (CU, ";");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Node);
      PL (CU, ";");

      --  Wrap

      NL (CU);
      Gen_Wrap_Profile (CU, Node);
      PL (CU, ";");

   end Gen_String_Instance_Spec;

   ------------------------------
   -- Gen_String_Instance_Body --
   ------------------------------

   procedure Gen_String_Instance_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Node, From_Container => False);
      NL (CU);
      PL (CU, "  renames " & Ada_Name (Node) & ".From_Any;");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Node);
      NL (CU);
      PL (CU, "  renames " & Ada_Name (Node) & ".To_Any;");

      --  Wrap

      NL (CU);
      Gen_Wrap_Profile (CU, Node);
      NL (CU);
      PL (CU, "  renames " & Ada_Name (Node) & ".Wrap;");

      --  Fill in the typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "begin");
      II (CU);
      Put (CU, Ada_TC_Name (Node) & " := ");

      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

      if Is_Wide (Node) then
         PL (CU, "CORBA.TypeCode.Internals.Build_Wstring_TC ("
             & Img (Expr_Value (Bound (Node))) & ");");
      else
         PL (CU, "CORBA.TypeCode.Internals.Build_String_TC ("
             & Img (Expr_Value (Bound (Node))) & ");");
      end if;

      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_String_Instance_Body;

   --------------------
   -- Gen_Union_Spec --
   --------------------

   procedure Gen_Union_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      --  TypeCode generation

      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

      NL (CU);
      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");
      end if;
   end Gen_Union_Spec;

   --------------------
   -- Gen_Union_Body --
   --------------------

   procedure Gen_Union_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      ST_Node : constant Node_Id := Switch_Type (Node);
      Switch_Helper_Name : constant String := Helper_Unit (ST_Node);
      Switch_TCU_Name    : constant String := TC_Unit (ST_Node);

   begin
      Add_Helper_Dependency (CU, Switch_TCU_Name);
      Add_Helper_Dependency (CU, Switch_Helper_Name);

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, " is");
         II (CU);
         PL (CU, "Label_Any : constant CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Internals.Get_Aggregate_Element (Item,");
         PL (CU, "                             "
             & Ada_Full_TC_Name (ST_Node) & ",");
         PL (CU, "                             CORBA.Unsigned_Long (0));");
         DI (CU);
         PL (CU, "Label : constant "
             & Ada_Type_Name (ST_Node)
             & " := "
             & Switch_Helper_Name
             & ".From_Any (Label_Any);");
         PL (CU, "Result : "
             & Ada_Type_Name (Node)
             & " (Label);");
         PL (CU, "Index : CORBA.Any;");
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "case Label is");
         II (CU);

         declare
            It          : Node_Iterator;
            Case_Node   : Node_Id;
            J           : Long_Integer := 0;
            Has_Default : Boolean := False;

         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  CT_Node : constant Node_Id := Case_Type (Case_Node);
                  Helper_Name : constant String := Helper_Unit (CT_Node);
                  TCU_Name    : constant String := TC_Unit (CT_Node);
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;

               begin
                  Add_Helper_Dependency (CU, TCU_Name);
                  Add_With (CU, Helper_Name);

                  if Default_Index (Node) = J then
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

                        Gen_Constant_Value
                          (CU,
                           Expr => Label_Node, Typ => ST_Node);
                     end loop;
                  end if;

                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "Index := CORBA.Internals.Get_Aggregate_Element");
                  II (CU);
                  PL (CU, "(Item,");

                  PL (CU, " " & Ada_Full_TC_Name (CT_Node) & ",");
                  PL (CU, " CORBA.Unsigned_Long (1));");
                  J := J + 1;
                  DI (CU);
                  PL (CU, "Result."
                      & Ada_Name (Case_Decl (Case_Node))
                      & " := "
                      & Helper_Name
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
         PL (CU, "Result : CORBA.Any :=");
         II (CU);
         PL (CU, "CORBA.Internals.Get_Empty_Any_Aggregate ("
             & Ada_TC_Name (Node)
             & ");");
         DI (CU);
         DI (CU);
         PL (CU, "begin");
         II (CU);
         PL (CU, "CORBA.Internals.Add_Aggregate_Element");
         II (CU);
         PL (CU, "(Result, " & Switch_Helper_Name & ".To_Any (Item.Switch));");
         DI (CU);
         PL (CU, "case Item.Switch is");
         II (CU);

         declare
            It          : Node_Iterator;
            Case_Node   : Node_Id;
            J           : Long_Integer := 0;
            Has_Default : Boolean := False;

         begin
            Init (It, Cases (Node));

            while not Is_End (It) loop
               Get_Next_Node (It, Case_Node);

               declare
                  CT_Node     : constant Node_Id := Case_Type (Case_Node);
                  Helper_Name : constant String  := Helper_Unit (CT_Node);
                  It2         : Node_Iterator;
                  Label_Node  : Node_Id;
                  First_Label : Boolean := True;

               begin
                  if Default_Index (Node) = J then
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

                        Gen_Constant_Value
                          (CU,
                           Expr => Label_Node, Typ => ST_Node);
                     end loop;
                  end if;

                  PL (CU, " =>");
                  II (CU);
                  PL (CU, "CORBA.Internals.Add_Aggregate_Element");
                  II (CU);
                  PL (CU, "(Result, "
                      & Helper_Name
                      & ".To_Any (Item."
                      & Ada_Name (Case_Decl (Case_Node))
                      & "));");
                  J := J + 1;
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
      end if;

      --  Fill in typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id : constant CORBA.String := CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
       & """);");

      declare
         It        : Node_Iterator;
         Case_Node : Node_Id;

      begin
         Init (It, Cases (Node));

         while not Is_End (It) loop
            Get_Next_Node (It, Case_Node);

            PL (CU, "Arg_Name_"
                & Ada_Name (Case_Decl (Case_Node))
                & " : constant CORBA.String := CORBA.To_CORBA_String ("""
                & Ada_Name (Case_Decl (Case_Node))
                & """);");
         end loop;
      end;

      DI (CU);
      PL (CU, "begin");
      II (CU);
      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Union);");
      PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
            & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
            & "   CORBA.To_Any (Name));");
      PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
            & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
            & "   CORBA.To_Any (Id));");
      PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
            & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
            & "   CORBA.To_Any (" & Ada_Full_TC_Name (ST_Node) & "));");

      Put (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
             & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
             & "   CORBA.To_Any (");
      declare
         Default  : Long_Integer := Default_Index (Node);
         Negative : constant Boolean := Default < 0;
      begin
         if Negative then
            --  CORBA."-" (unary) may not be directly visible
            Put (CU, "CORBA.""-"" (");
            Default := -Default;
         end if;

         Put (CU, "CORBA.Long'(" & Long_Integer_Img (Default) & ")");

         if Negative then
            Put (CU, ")");
         end if;
      end;

      PL (CU, "));");

      declare
         It        : Node_Iterator;
         Case_Node : Node_Id;
         I         : Long_Integer := 0;

      begin
         Init (It, Cases (Node));

         while not Is_End (It) loop
            Get_Next_Node (It, Case_Node);

            declare
               It2        : Node_Iterator;
               Label_Node : Node_Id;
            begin
               if Default_Index (Node) = I then
                  PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
                        & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                        & "   " & Switch_Helper_Name
                        & ".To_Any (" & Ada_Type_Name (ST_Node)
                        & "'First));");

                  Add_Helper_Dependency
                    (CU,
                     Ada_Helper_Unit_Name (Mapping, Case_Type (Case_Node)));

                  PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
                        & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                        & "  CORBA.To_Any ("
                        & Ada_Full_TC_Name (Case_Type (Case_Node))
                        & "));");

                  PL (CU, "CORBA.Internals.Add_Parameter" & ASCII.LF
                        & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                        & "   CORBA.To_Any (Arg_Name_"
                        & Ada_Name (Case_Decl (Case_Node))
                        & "));");

               else
                  Init (It2, Labels (Case_Node));
                  while not Is_End (It2) loop
                     Get_Next_Node (It2, Label_Node);
                     Put (CU, "CORBA.Internals.Add_Parameter"
                            & ASCII.LF
                            & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                            & "   " & Switch_Helper_Name
                            & ".To_Any (" & Ada_Type_Name (ST_Node) & "'(");
                     Gen_Constant_Value (CU,
                       Expr => Label_Node, Typ => ST_Node);
                     PL (CU, ")));");

                     Add_Helper_Dependency
                       (CU,
                        Ada_Helper_Unit_Name (Mapping, Case_Type (Case_Node)));

                     PL (CU, "CORBA.Internals.Add_Parameter"
                           & ASCII.LF
                           & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                           & "   CORBA.To_Any ("
                           & Ada_Full_TC_Name (Case_Type (Case_Node))
                           & "));");
                     PL (CU, "CORBA.Internals.Add_Parameter"
                           & ASCII.LF
                           & "  (" & Ada_TC_Name (Node) & "," & ASCII.LF
                           & "   CORBA.To_Any (Arg_Name_"
                           & Ada_Name (Case_Decl (Case_Node))
                           & "));");
                  end loop;
               end if;

               I := I + 1;
            end;
         end loop;
      end;

      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);
   end Gen_Union_Body;

   ------------------------------
   -- Gen_Type_Declarator_Spec --
   ------------------------------

   procedure Gen_Type_Declarator_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Is_Array : constant Boolean := Length (Array_Bounds (Node)) > 0;

   begin
      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);

      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");
      if Is_Array then
         for J in 1 .. Length (Array_Bounds (Node)) - 1 loop
            PL (CU, Ada_TC_Name (Node) & "_TC_Dimension_"
                & Img (J)
                & " : CORBA.TypeCode.Object;");
         end loop;
         Gen_Aggregate_Content_Wrapper_Spec (CU, Node);
      end if;

      if not Is_Interface_Type (Node)
        and then not Has_Local_Component (Node)
      then

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  Generate From_Any operating on PolyORB.Any.Any_Container'Class
         --  for elementary scalar types and enum types, as these can be used
         --  as switch type for unions.

         if not Is_Array then
            case Kind (Root_Type (Node)) is
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
                 K_Octet              |
                 K_Enum               =>
                  NL (CU);
                  Gen_From_Any_Profile (CU, Node, From_Container => True);
                  PL (CU, ";");

               when others =>
                  null;
            end case;
         end if;

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

      end if;
   end Gen_Type_Declarator_Spec;

   ------------------------------
   -- Gen_Type_Declarator_Body --
   ------------------------------

   procedure Gen_Type_Declarator_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Is_Array    : constant Boolean := Length (Array_Bounds (Node)) > 0;
      Type_Node   : constant Node_Id := T_Type (Parent (Node));
      Helper_Name : constant String := Helper_Unit (Type_Node);
      TCU_Name    : constant String := TC_Unit (Type_Node);
   begin
      --  Fill in typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);

      if not Is_Array then
         PL (CU, "Name : constant CORBA.String := CORBA.To_CORBA_String ("""
             & Ada_Name (Node)
             & """);");
         PL (CU, "Id : constant CORBA.String := CORBA.To_CORBA_String ("""
             & Idl_Repository_Id (Node)
             & """);");
      end if;

      DI (CU);
      PL (CU, "begin");
      II (CU);

      Put (CU, Ada_TC_Name (Node) & " :=");

      if not Is_Array then
         Add_Helper_Dependency (CU, TCU_Name);
         PL (CU, " CORBA.TypeCode.Internals.Build_Alias_TC");
         PL (CU, "  (Name => Name, Id => Id, Parent => "
               & Ada_Full_TC_Name (Type_Node) & ");");

      else
         Add_With (CU, "PolyORB.Any");
         NL (CU);
         Put (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object (");
         PL (CU, "PolyORB.Any.TypeCode.TC_Array);");

         for J in 1 .. Length (Array_Bounds (Node)) - 1 loop
            PL (CU, Ada_TC_Name (Node) & "_TC_Dimension_"
                & Img (J)
                & " := "
                & "CORBA.TypeCode.Internals.To_CORBA_Object"
                & " (PolyORB.Any.TypeCode.TC_Array);");
         end loop;
         Gen_Array_TC (CU, Type_Node, Node);
      end if;

      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      DI (CU);
      PL (CU, "end;");
      Divert (CU, Visible_Declarations);

      if Is_Interface_Type (Type_Node)
        and then not Is_Array
      then
         return;
      end if;

      if not Has_Local_Component (Node) then
         if Is_Array then
            Gen_Aggregate_Content_Wrapper_Body (CU, Node);
         end if;

         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         NL (CU);
         PL (CU, "is");
         II (CU);

         Add_Helper_Dependency (CU, Helper_Name);

         if Is_Array then
            PL (CU, "Result : " & Ada_Type_Name (Node) & ";");
            PL (CU, "Aux    : array (Natural range 0 .. "
                & Img (Length (Array_Bounds (Node)) - 1) & ") of CORBA.Any;");

            DI (CU);
            NL (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It  : Node_Iterator;
               Bound_Node : Node_Id;
               Dim        : Natural := 0;

            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  if Dim = 0 then
                     PL (CU, "Aux (0) := Item;");

                  else
                     PL (CU, "Aux (" & Img (Dim) & ") :=");
                     PL (CU, "  CORBA.Internals.Get_Aggregate_Element");
                     PL (CU, "  (Aux (" & Img (Dim - 1) & "),");
                     PL (CU, "   " & Ada_TC_Name (Node) & "_TC_Dimension_"
                         & Img (Dim) & ",");
                     PL (CU, "   CORBA.Unsigned_Long ("
                         & Loop_Parameter (Dim - 1) & "));");
                  end if;

                  NL (CU);
                  Put (CU, "for " & Loop_Parameter (Dim) & " in 0 .. ");
                  Gen_Constant_Value (CU, Expr => Bound_Node, Typ => No_Node);
                  PL (CU, " - 1 loop");
                  II (CU);

                  Dim := Dim + 1;
               end loop;

               Put (CU, "Result ");

               for J in 0 .. Dim - 1 loop
                  if J = 0 then
                     Put (CU, "(");
                  else
                     Put (CU, ", ");
                  end if;
                  Put (CU, Loop_Parameter (J));
                  if J = Dim - 1 then
                     Put (CU, ")");
                  end if;
               end loop;

               PL (CU, " :=");
               PL (CU, "  " & Helper_Name & ".From_Any");
               PL (CU, "  (CORBA.Internals.Get_Aggregate_Element");
               PL (CU, "   (Aux (" & Img (Dim - 1) & "),");
               PL (CU, "    " & Ada_Full_TC_Name (Type_Node) & ",");
               PL (CU, "    CORBA.Unsigned_Long ("
                   & Loop_Parameter (Dim - 1) & ")));");

               for J in 1 .. Dim loop
                  DI (CU);
                  PL (CU, "end loop;");
               end loop;
            end;

            NL (CU);
            PL (CU, "return Result;");

         else
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return " & Ada_Type_Name (Node)
                & " (" & Ada_Type_Name (Type_Node)
                & "'(" & Helper_Name & ".From_Any (Item)));");

         end if;

         DI (CU);
         PL (CU, "end From_Any;");

         --  See Gen_Type_Declarator_Spec for details about the second
         --  version of From_Any.

         if not Is_Array then
            case Kind (Root_Type (Node)) is
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
                 K_Octet              |
                 K_Enum               =>
                  NL (CU);
                  Gen_From_Any_Profile (CU, Node, From_Container => True);
                  PL (CU, " is");
                  PL (CU, "begin");
                  II (CU);
                  PL (CU, "return " & Ada_Type_Name (Node)
                      & " (" & Ada_Type_Name (Type_Node)
                      & "'(" & Helper_Name & ".From_Any (C)));");
                  DI (CU);
                  PL (CU, "end From_Any;");

               when others =>
                  null;
            end case;
         end if;

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         NL (CU);
         PL (CU, "is");
         II (CU);

         if Is_Array then
            PL (CU,
                "Result : array (Natural range 0 .. "
                & Img (Length (Array_Bounds (Node)) - 1)
                & ") of CORBA.Any;");
            DI (CU);
            NL (CU);
            PL (CU, "begin");
            II (CU);

            declare
               Bounds_It  : Node_Iterator;
               Bound_Node : Node_Id;
               Dim        : Natural := 0;

            begin
               Init (Bounds_It, Array_Bounds (Node));

               while not Is_End (Bounds_It) loop
                  Get_Next_Node (Bounds_It, Bound_Node);

                  PL (CU, "Result (" & Img (Dim) & ") :=");
                  PL (CU, "  CORBA.Internals.Get_Empty_Any_Aggregate");

                  if Dim = 0 then
                     PL (CU, "  (" & Ada_TC_Name (Node) & ");");

                  else
                     PL (CU, "  (" & Ada_TC_Name (Node) & "_TC_Dimension_"
                         & Img (Dim) & ");");
                  end if;

                  NL (CU);
                  Put (CU, "for " & Loop_Parameter (Dim) & " in 0 .. ");
                  Gen_Constant_Value (CU, Expr => Bound_Node, Typ => No_Node);
                  PL (CU, " - 1 loop");
                  II (CU);

                  Dim := Dim + 1;
               end loop;

               PL (CU, "CORBA.Internals.Add_Aggregate_Element");
               PL (CU, "  (Result (" & Img (Dim - 1) & "),");
               II (CU);
               Put (CU, Helper_Name & ".To_Any (Item (" & Loop_Parameter (0));
               for J in 1 .. Dim - 1 loop
                  Put (CU, ", " & Loop_Parameter (J));
               end loop;
               PL (CU, ")));");
               DI (CU);

               for J in reverse 1 .. Dim loop
                  if J /= Dim then
                     PL (CU, "CORBA.Internals.Add_Aggregate_Element (Result ("
                         & Img (J - 1) & "), Result ("
                         & Img (J) & "));");
                  end if;

                  DI (CU);
                  PL (CU, "end loop;");
                  NL (CU);
               end loop;
            end;

            PL (CU, "return Result (0);");

         else
            PL (CU, "Result : CORBA.Any := "
                & Helper_Name & ".To_Any (" & Ada_Type_Name (Type_Node)
                & " (Item));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "CORBA.Internals.Set_Type (Result, "
                & Ada_TC_Name (Node) & ");");
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
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA");

      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object;");

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Wrap

         NL (CU);
         Gen_Wrap_Profile (CU, Node);
         PL (CU, ";");
      end if;
   end Gen_Sequence_Spec;

   -----------------------
   -- Gen_Sequence_Body --
   -----------------------

   procedure Gen_Sequence_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Seq_Helper_Name : constant String  := Ada_Name (Node) & "_Helper";
      Seq_TC_Name     : constant String  := Ada_TC_Name (Node);

      Elt_Type        : constant Node_Id := Sequence_Type (Sequence (Node));
      Elt_Helper_Name : constant String  := Helper_Unit (Elt_Type);
      Elt_TCU_Name    : constant String  := TC_Unit (Elt_Type);
      Elt_TC_Name     : constant String  := Ada_Full_TC_Name (Elt_Type);
      Elt_Wrap_Name   : constant String  := Ada_Name (Node) & "_Element_Wrap";

      B_Node  : constant Node_Id := Bound (Sequence (Node));
      B_Value : Idl_Integer      := 0;

   begin
      if not Has_Local_Component (Node) then
         if B_Node = No_Node then
            Add_With (CU,
              "PolyORB.Sequences.Unbounded.CORBA_Helper",
              Elab_Control => Elaborate_All);
         else
            Add_With (CU,
              "PolyORB.Sequences.Bounded.CORBA_Helper",
              Elab_Control => Elaborate_All);
            B_Value := Integer_Value (B_Node);
         end if;

         Add_Helper_Dependency (CU, Elt_Helper_Name);
         --  For element To_Any/From_Any

         --  Generate Element_Wrap

         NL (CU);
         PL (CU, "function " & Elt_Wrap_Name & " (X : access "
             & Ada_Type_Name (Elt_Type)
             & ") return PolyORB.Any.Content'Class is");
         PL (CU, "begin");
         II (CU);
         Put (CU, "return ");
         Gen_Wrap_Call (CU, Elt_Type, "X.all");
         PL (CU, ";");
         DI (CU);
         PL (CU, "end " & Elt_Wrap_Name & ";");

         --  Instantiate generic sequence helper

         NL (CU);
         PL (CU, "package " & Seq_Helper_Name
             & " is new " & Ada_Name (Node) & ".CORBA_Helper");
         Put (CU, "  (");
         II (CU);
         PL (CU, "Element_To_Any   => " & Elt_Helper_Name & ".To_Any,");
         PL (CU, "Element_From_Any => " & Elt_Helper_Name & ".From_Any,");
         PL (CU, "Element_Wrap     => " & Elt_Wrap_Name & ");");
         DI (CU);

         --  Generate renamings-as-body from instance

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         NL (CU);
         PL (CU, "  renames " & Seq_Helper_Name & ".From_Any;");

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         NL (CU);
         PL (CU, "  renames " & Seq_Helper_Name & ".To_Any;");

         NL (CU);
         Gen_Wrap_Profile (CU, Node);
         NL (CU);
         PL (CU, "  renames " & Seq_Helper_Name & ".Wrap;");
      end if;

      Divert (CU, Deferred_Initialization);

      Add_Helper_Dependency (CU, Elt_TCU_Name);
      --  For element TypeCode

      NL (CU);
      PL (CU, Ada_TC_Name (Node) & " := ");
      PL (CU, "  CORBA.TypeCode.Internals.Build_Sequence_TC");
      PL (CU, "    (" & Elt_TC_Name & ", " & Img (B_Value) & ");");

      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Node) & ");");

      if not Has_Local_Component (Node) then
         Put (CU, Seq_Helper_Name & ".Initialize" & ASCII.LF & "  (");
         II (CU);
         PL (CU, "Element_TC  => " & Elt_TC_Name & ",");
         PL (CU, "Sequence_TC => " & Seq_TC_Name & ");");
         DI (CU);
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Sequence_Body;

   --------------------
   -- Gen_Fixed_Spec --
   --------------------

   procedure Gen_Fixed_Spec
     (CU        : in out Compilation_Unit;
      Decl_Node : Node_Id)
   is
   begin
      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      PL (CU, Ada_TC_Name (Decl_Node) & " : CORBA.TypeCode.Object;");

      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Decl_Node, From_Container => False);
      PL (CU, ";");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Decl_Node);
      PL (CU, ";");

      --  Wrap

      NL (CU);
      Gen_Wrap_Profile (CU, Decl_Node);
      PL (CU, ";");

   end Gen_Fixed_Spec;

   --------------------
   -- Gen_Fixed_Body --
   --------------------

   procedure Gen_Fixed_Body
     (CU        : in out Compilation_Unit;
      Decl_Node : Node_Id)
   is
      Fixed_Node : constant Node_Id := T_Type (Parent (Decl_Node));
      Type_Name  : constant String  := Ada_Name (Decl_Node);
      Helpers_Inst_Name : constant String := T_Helpers & Type_Name;
   begin
      NL (CU);
      PL (CU, "package " & Helpers_Inst_Name & " is");
      Add_With (CU, "CORBA.Fixed_Point", Elab_Control => Elaborate_All);
      PL (CU, "  new CORBA.Fixed_Point (" & Ada_Full_Name (Decl_Node) & ");");

      --  From_Any

      NL (CU);
      Gen_From_Any_Profile (CU, Decl_Node, From_Container => False);
      NL (CU);
      PL (CU, "  renames " & Helpers_Inst_Name & ".From_Any;");

      --  To_Any

      NL (CU);
      Gen_To_Any_Profile (CU, Decl_Node);
      NL (CU);
      PL (CU, "  renames " & Helpers_Inst_Name & ".To_Any;");

      --  Wrap

      NL (CU);
      Gen_Wrap_Profile (CU, Decl_Node);
      NL (CU);
      PL (CU, "  renames " & Helpers_Inst_Name & ".Wrap;");

      --  Fill in typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      Add_With (CU, "PolyORB.Any");
      PL (CU, Ada_TC_Name (Decl_Node) & " :=");
      PL (CU, "  CORBA.TypeCode.Internals.To_CORBA_Object"
            & " (PolyORB.Any.TypeCode.TC_Fixed);");
      Put (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Decl_Node)
          & ", CORBA.To_Any (CORBA.Unsigned_Short (");
      Gen_Constant_Value (CU,
        Expr => Digits_Nb (Fixed_Node), Typ => No_Node);
      PL (CU, ")));");
      Put (CU, "CORBA.Internals.Add_Parameter ("
          & Ada_TC_Name (Decl_Node)
          & ", CORBA.To_Any (CORBA.Short (");
      Gen_Constant_Value (CU, Expr => Scale (Fixed_Node), Typ => No_Node);
      PL (CU, ")));");
      PL (CU, "CORBA.TypeCode.Internals.Disable_Reference_Counting ("
              & Ada_TC_Name (Decl_Node) & ");");
      Divert (CU, Visible_Declarations);
   end Gen_Fixed_Body;

   ------------------------------
   -- Gen_Boxed_ValueType_Spec --
   ------------------------------

   procedure Gen_Boxed_ValueType_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      --  TypeCode

      NL (CU);
      Add_With (CU, "CORBA");
      Add_With (CU, "PolyORB.Any");

      PL (CU, Ada_TC_Name (Node) & " : CORBA.TypeCode.Object");
      PL (CU, "  := CORBA.TypeCode.Internals.To_CORBA_Object "
          & "(PolyORB.Any.TypeCode.TC_ValueBox);");

      if not Has_Local_Component (Node) then
         --  From_Any

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         PL (CU, ";");

         --  To_Any

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         PL (CU, ";");

         --  Wrap

         NL (CU);
         Gen_Wrap_Profile (CU, Node);
         PL (CU, ";");
      end if;
   end Gen_Boxed_ValueType_Spec;

   ------------------------------
   -- Gen_Boxed_ValueType_Body --
   ------------------------------

   procedure Gen_Boxed_ValueType_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      Box_Helper_Name : constant String  := Ada_Name (Node) & "_Helper";
      Box_TC_Name     : constant String  := Ada_TC_Name (Node);

      Elt_Type        : constant Node_Id := Boxed_Type (Node);
      Elt_Helper_Name : constant String  := Helper_Unit (Elt_Type);
      Elt_TCU_Name    : constant String  := TC_Unit (Elt_Type);
      Elt_TC_Name     : constant String  := Ada_Full_TC_Name (Elt_Type);
      Elt_Wrap_Name   : constant String  := Ada_Name (Node) & "_Element_Wrap";

   begin
      if not Has_Local_Component (Node) then
         Add_With (CU,
           "CORBA.Value.Box.Helper",
           Elab_Control => Elaborate_All);

         Add_Helper_Dependency (CU, Elt_Helper_Name);
         --  For element To_Any/From_Any

         Add_Helper_Dependency (CU, Elt_TCU_Name);
         --  For element TypeCode

         --  Generate Element_Wrap

         NL (CU);
         PL (CU, "function " & Elt_Wrap_Name & " (X : access "
             & Ada_Type_Name (Elt_Type)
             & ") return PolyORB.Any.Content'Class is");
         PL (CU, "begin");
         II (CU);
         Put (CU, "return ");
         Gen_Wrap_Call (CU, Elt_Type, "X.all");
         PL (CU, ";");
         DI (CU);
         PL (CU, "end " & Elt_Wrap_Name & ";");

         --  Instantiate generic valuebox helper

         NL (CU);
         PL (CU, "package " & Box_Helper_Name
             & " is new " & Ada_Name (Node) & "_Value_Box.Helper");
         Put (CU, "  (");
         II (CU);
         PL (CU, "Element_To_Any   => " & Elt_Helper_Name & ".To_Any,");
         PL (CU, "Element_From_Any => " & Elt_Helper_Name & ".From_Any,");
         PL (CU, "Element_Wrap     => " & Elt_Wrap_Name & ");");
         DI (CU);

         --  Generate renamings-as-body from instance

         NL (CU);
         Gen_From_Any_Profile (CU, Node, From_Container => False);
         NL (CU);
         PL (CU, "  renames " & Box_Helper_Name & ".From_Any;");

         NL (CU);
         Gen_To_Any_Profile (CU, Node);
         NL (CU);
         PL (CU, "  renames " & Box_Helper_Name & ".To_Any;");

         NL (CU);
         Gen_Wrap_Profile (CU, Node);
         NL (CU);
         PL (CU, "  renames " & Box_Helper_Name & ".Wrap;");
      end if;

      --  Fill in the typecode TC_<name of the type>

      Divert (CU, Deferred_Initialization);
      NL (CU);
      PL (CU, "declare");
      II (CU);
      PL (CU, "Name : CORBA.String :=");
      PL (CU, "   CORBA.To_CORBA_String ("""
          & Ada_Name (Node)
          & """);");
      PL (CU, "Id : CORBA.String :=");
      PL (CU, "   CORBA.To_CORBA_String ("""
          & Idl_Repository_Id (Node)
          & """);");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      --  Put the name and repository Id for the value

      PL (CU, "CORBA.TypeCode.Internals.Add_Parameter");
      PL (CU, "  (" & Box_TC_Name & ", CORBA.To_Any (Name));");
      PL (CU, "CORBA.TypeCode.Internals.Add_Parameter");
      PL (CU, "  (" & Box_TC_Name & ", CORBA.To_Any (Id));");
      PL (CU, "CORBA.TypeCode.Internals.Add_Parameter");
      PL (CU, "  (" & Box_TC_Name & ", CORBA.To_Any (" & Elt_TC_Name & "));");

      DI (CU);
      PL (CU, "end;");

      if not Has_Local_Component (Node) then
         NL (CU);
         Put (CU, Box_Helper_Name & ".Initialize" & ASCII.LF & "  (");
         II (CU);
         PL (CU, "Element_TC => " & Elt_TC_Name & ",");
         PL (CU, "Box_Ref_TC => " & Box_TC_Name & ");");
         DI (CU);
         Divert (CU, Visible_Declarations);
      end if;
   end Gen_Boxed_ValueType_Body;

   ------------------
   -- Gen_Array_TC --
   ------------------

   procedure Gen_Array_TC
     (CU                : in out Compilation_Unit;
      Element_Type_Node :        Node_Id;
      Decl_Node         :        Node_Id)
   is
      procedure Rec_Gen_Array_TC
        (CU                : in out Compilation_Unit;
         It                : in out Node_Iterator;
         First_Bound       :        Boolean;
         Index             :        Integer;
         Element_Type_Node :        Node_Id;
         Decl_Node         :        Node_Id);
      --  Recursively generate the typecode for the component subtype of an
      --  array, then generate the typecode for the array itself. This is node
      --  by advancing the bounds iterator one step, to unwind one dimension,
      --  until no bounds remain, at which point we reference the typecode for
      --  the ultimate element type.

      ----------------------
      -- Rec_Gen_Array_TC --
      ----------------------

      procedure Rec_Gen_Array_TC
        (CU                : in out Compilation_Unit;
         It                : in out Node_Iterator;
         First_Bound       :        Boolean;
         Index             :        Integer;
         Element_Type_Node :        Node_Id;
         Decl_Node         :        Node_Id)
      is
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
         Put (CU, "CORBA.Internals.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, Ada_TC_Name (Decl_Node) & "_TC_Dimension_" & Img (Index));
         end if;
         Put (CU, ", CORBA.To_Any (CORBA.Unsigned_Long (");
         Gen_Constant_Value (CU, Expr => Bound_Node, Typ => No_Node);
         PL (CU, ")));");
         Put (CU, "CORBA.Internals.Add_Parameter (");
         if First_Bound then
            Put (CU, Ada_TC_Name (Decl_Node));
         else
            Put (CU, Ada_TC_Name (Decl_Node) & "_TC_Dimension_" & Img (Index));
         end if;
         if Last_Bound then
            Put (CU, ", "
                 & "CORBA.To_Any ("
                 & Ada_Full_TC_Name (Element_Type_Node));
         else
            Put (CU, ", CORBA.To_Any ("
                 & Ada_TC_Name (Decl_Node) & "_TC_Dimension_"
                 & Img (Index + 1));
         end if;
         PL (CU, "));");
      end Rec_Gen_Array_TC;

      Bounds_It : Node_Iterator;
   begin
      Init (Bounds_It, Array_Bounds (Decl_Node));
      Rec_Gen_Array_TC
        (CU, Bounds_It, True, 0, Element_Type_Node, Decl_Node);
   end Gen_Array_TC;

   -------------------
   -- Gen_Wrap_Call --
   -------------------

   procedure Gen_Wrap_Call
     (CU   : in out Compilation_Unit;
      Typ  : Node_Id;
      Expr : String)
   is
      Root_Typ : Node_Id := Root_Type (Typ);
   begin
      if Is_Interface_Type (Root_Typ)
        and then Ada_Type_Name (Root_Typ) /= "CORBA.TypeCode.Object"
      then
         Root_Typ := Idl_Fe.Tree.Make_Object (Loc (Root_Typ));
      end if;

      declare
         Helper_Name : constant String := Helper_Unit (Root_Typ);
         Convert     : constant Boolean :=
                         (Ada_Type_Name (Root_Typ) /= Ada_Type_Name (Typ));
      begin
         Add_With (CU, Helper_Name);

         --  Perform view conversion to root type, then take
         --  'Unrestricted_Access.

         Put (CU, Helper_Name & ".Wrap ("
           & Conditional_Call
               (Func      => Ada_Type_Name (Root_Typ),
                Only_When => Convert,
                Expr      => Expr)
              & "'Unrestricted_Access)");
      end;
   end Gen_Wrap_Call;

   ----------------------
   -- Gen_Wrap_Profile --
   ----------------------

   procedure Gen_Wrap_Profile
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      Add_With (CU, "PolyORB.Any");
      Put (CU, "function Wrap (X : access "
           & Ada_Type_Name (Node) & ") return PolyORB.Any.Content'Class");
   end Gen_Wrap_Profile;

   --------------------
   -- Loop_Parameter --
   --------------------

   function Loop_Parameter (Dim : Natural) return String is
   begin
      return T_J & Img (Dim);
   end Loop_Parameter;

   -------------------------
   -- Raise_From_Any_Name --
   -------------------------

   function Raise_From_Any_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_Exception);
      return "Raise_" & Ada_Name (Node) & "_From_Any";
   end Raise_From_Any_Name;

   ----------------
   -- Raise_Name --
   ----------------

   function Raise_Name (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_Exception);
      return "Raise_" & Ada_Name (Node);
   end Raise_Name;

   -------------------
   -- Type_Modifier --
   -------------------

   function Type_Modifier (Node : Node_Id) return String is
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

   ----------------
   -- Visibility --
   ----------------

   function Visibility (Node : Node_Id) return String is
   begin
      pragma Assert (Kind (Node) = K_State_Member);
      if Is_Public (Node) then
         return "CORBA.PUBLIC_MEMBER";
      else
         return "CORBA.PRIVATE_MEMBER";
      end if;
   end Visibility;

end Ada_Be.Idl2Ada.Helper;
