------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                A D A _ B E . I D L 2 A D A . S T R E A M                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;

with Utils;                 use Utils;

with Ada_Be.Idl2Ada.Value_Impl;

with Ada_Be.Debug;

package body Ada_Be.Idl2Ada.Stream is

   use Ada_Be.Source_Streams;

   Flag : constant Natural := Ada_Be.Debug.Is_Active ("ada_be.idl2ada.stream");
   procedure O is new Ada_Be.Debug.Output (Flag);

   procedure Gen_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id);
   --  Generate the profile for the Marshall procedure
   --  of a type.
   --  FIXME: This is marshall-by-value.
   --         Marshall-by-reference should be produced
   --         as well. For details on marshalling by
   --         value vs. marshalling by reference,
   --         see the spec of Broca.Buffers.

   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id);
   --  Generate the profile for the Unmarshall function
   --  of a type.
   --  FIXME: This is unmarshall-by-value (see above).

   procedure Gen_Array_Iterator
     (CU               : in out Compilation_Unit;
      Array_Name       : String;
      Array_Dimensions : Natural;
      Stmt_Template    : String);
   --  Generate "for" loops that iterate over Array_Name,
   --  an array with Array_Dimensions dimensions, performing
   --  statement Stmt_Template on each array cell. The first
   --  occurence of the '%' character in Stmt_Template is
   --  replaced by the proper indices, with parentheses.


   --------------------------------------
   --  End of subprogram declarations  --
   --------------------------------------

   ----------------------------
   --  Gen_Marshall_Profile  --
   ----------------------------
   procedure Gen_Marshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "procedure Marshall");
      PL (CU, "  (Buffer : access Buffer_Type;");
      Put (CU, "   Val    : in "
           & Ada_Type_Name (Type_Node) & ")");
   end Gen_Marshall_Profile;

   ------------------------------
   --  Gen_Unmarshall_Profile  --
   ------------------------------
   procedure Gen_Unmarshall_Profile
     (CU        : in out Compilation_Unit;
      Type_Node : in Node_Id) is
   begin
      Add_With (CU, "Broca.Buffers", Use_It => True);
      PL (CU, "function Unmarshall");
      PL (CU, "  (Buffer : access Buffer_Type)");
      Put (CU, "  return "
           & Ada_Type_Name (Type_Node));
   end Gen_Unmarshall_Profile;

   --------------------------
   --  Gen_Array_Iterator  --
   --------------------------
   procedure Gen_Array_Iterator
     (CU               : in out Compilation_Unit;
      Array_Name       : String;
      Array_Dimensions : Natural;
      Stmt_Template    : String)
   is
      Indices_Pos : Natural := Stmt_Template'Last + 1;
      Prefix_End, Suffix_Start : Natural;

      function Identifier (Dimension : Positive) return String;
      --  Return an identifier that depends on the dimension and on the
      --  total dimension. If there is only one dimension, "I" will be
      --  used, "I1", "I2", ..., "In" otherwise.

      ----------------
      -- Identifier --
      ----------------

      function Identifier (Dimension : Positive) return String is
      begin
         pragma Assert (Dimension <= Array_Dimensions);
         if Array_Dimensions = 1 then
            return "I";
         else
            return "I" & Img (Dimension);
         end if;
      end Identifier;

   begin
      for I in Stmt_Template'Range loop
         if Stmt_Template (I) = '%' then
            Indices_Pos := I;
            exit;
         end if;
      end loop;

      pragma Assert (Indices_Pos in Stmt_Template'Range);

      Prefix_End := Indices_Pos - 1;
      while Prefix_End >= Stmt_Template'First and then
        Stmt_Template (Prefix_End) = ' ' loop
         Prefix_End := Prefix_End - 1;
      end loop;

      Suffix_Start := Indices_Pos + 1;

      declare
         Stmt_Prefix : constant String
           := Stmt_Template
           (Stmt_Template'First .. Prefix_End);
         Stmt_Suffix : constant String
           := Stmt_Template
           (Suffix_Start .. Stmt_Template'Last);
      begin
         for Dimen in 1 .. Array_Dimensions loop
            PL
              (CU, "for " & Identifier (Dimen)
               & " in " & Array_Name & "'Range ("
               & Img (Dimen) & ") loop");
            II (CU);
         end loop;

         PL (CU, Stmt_Prefix);
         Put (CU, "  (");
         for Dimen in 1 .. Array_Dimensions loop
            if Dimen /= 1 then
               Put (CU, ", ");
            end if;
            Put (CU, Identifier (Dimen));
         end loop;
         PL (CU, ")" & Stmt_Suffix);
         for Dimen in 1 .. Array_Dimensions loop
            DI (CU);
            PL (CU, "end loop;");
         end loop;
      end;
   end Gen_Array_Iterator;

   -------------------
   -- Gen_Node_Spec --
   -------------------

   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         when K_Exception =>
            null;

         when K_ValueType =>
            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, ";");

         when
           K_Forward_Interface |
           K_Interface =>
            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, ";");

         when
           K_Enum              |
           K_Union             |
           K_Struct            |
           K_Sequence_Instance |
           K_String_Instance   =>
            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, ";");
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, ";");

         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
            begin
               if not Is_Interface then

                  declare
                     It   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, Decl_Node);

                        NL (CU);
                        Gen_Marshall_Profile
                          (CU, Decl_Node);
                        PL (CU, ";");
                        Gen_Unmarshall_Profile
                          (CU, Decl_Node);
                        PL (CU, ";");
                     end loop;
                  end;
               end if;
            end;

         when others =>
            null;
      end case;

   end Gen_Node_Spec;

   -------------------
   -- Gen_Node_Body --
   -------------------

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
      NK : constant Node_Kind
        := Kind (Node);
   begin
      case NK is

         when K_Exception =>
            null;

         when K_ValueType =>

            if not Abst (Node) then
               NL (CU);
               Add_With (CU, "Broca.CDR", Use_It => True);
               Add_With (CU, "CORBA.Impl");
               Add_With (CU, "Broca.Buffers", Use_It => True);
               PL (CU, "procedure Marshall");
               PL (CU, "  (Buffer : access Buffer_Type;");
               PL (CU, "   Val    : in CORBA.Impl.Object_Ptr;");
               Add_With (CU, "Broca.Value.Stream");
               PL (CU, "   Already_Marshalled : in out "
                   & "Broca.Value.Stream.ISeq.Sequence;");
               PL (CU, "   Formal : in CORBA.RepositoryId;");
               PL (CU, "   Only_Members : in Boolean;");
               PL (CU, "   Nesting_Depth : in CORBA.Long) is");
               II (CU);
               Add_With (CU,
                         Ada_Full_Name (Node)
                         & Ada_Be.Idl2Ada.Value_Impl.Suffix);
               PL (CU,
                   "Obj : "
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object_Ptr");
               PL (CU,
                   "   := "
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object_Ptr (Val);");
               PL (CU, "With_Chunking : Boolean;");
               PL (CU, "use CORBA;");
               DI (CU);
               PL (CU, "begin");
               II (CU);

               --  FIXME chunking should be used to when the buffer
               --  is full or the valuetype contains custom marshalled code
               PL (CU, "With_Chunking := not Only_Members;");

               --  FIXME : we suppose there is no chunking and
               --  no type information (no truncation therefore)
               PL (CU, "Marshall (Buffer, "
                   & "Broca.Value.Stream.Default_Value_Tag);");

               --  marshall all the state members
               declare
                  It : Node_Iterator;
                  Current : Node_Id;
               begin
                  Init (It, Contents (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Current);
                     if Kind (Current) = K_State_Member then
                        pragma Debug (O ("Kind(State_Type (Current)) = "
                                         & Node_Kind'Image
                                         (Kind (State_Type (Current)))));
                        if Kind (State_Type (Current)) = K_Scoped_Name
                        and then Kind (Value (State_Type (Current)))
                          = K_ValueType then
                           --  marshall a valuetype state member
                           PL (CU, "Broca.Value.Stream.Marshall");
                           PL (CU, "  (Buffer,");
                           PL (CU, "   Obj."
                               & Ada_Name (Head (State_Declarators (Current)))
                               & ",");
                           PL (CU, "   CORBA.To_CORBA_String");
                           PL (CU, "     ("
                               & Parent_Scope_Name
                               (Head (State_Declarators (Current)))
                               & "." & T_Repository_Id & "),");
                           PL (CU, "   Already_Marshalled,");
                           PL (CU, "   Nesting_Depth + 1);");

                        else
                           --  unmarshall a *non-valuetype* state member
                           Add_With_Stream (CU, State_Type (Current));
                           PL (CU,
                               "Marshall (Buffer, Obj."
                               & Ada_Name (Head (State_Declarators (Current)))
                               & ");");
                           --  State Members are expanded so that
                           --  their State_Declarators list contains
                           --  a single node
                        end if;
                     end if;
                  end loop;
               end;
               DI (CU);
               PL (CU, "end Marshall;");
            end if;

            --  Unmarshall
            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            Add_With (CU, "CORBA.AbstractBase");
            PL (CU, "New_Ref : " & Ada_Type_Name (Node) & ";");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "Broca.Value.Stream");
            PL (CU, "Broca.Value.Stream.Unmarshall");
            PL (CU, "  (Buffer,");
            PL (CU, "   CORBA.To_CORBA_String");
            PL (CU, "  ("
                & Ada_Full_Name (Node)
                & "." & T_Repository_Id
                & "),");
            PL (CU, "New_Ref);");
            PL (CU, "return New_Ref;");
            DI (CU);
            PL (CU, "end Unmarshall;");

            --  Unmarshall_Fields
            if not Abst (Node) then
               NL (CU);
               Add_With (CU, "Broca.Buffers", Use_It => True);
               PL (CU, "procedure Unmarshall_Fields");
               PL (CU, "  (Buffer : access Buffer_Type;");
               PL (CU, "   Val    : in out CORBA.Value.Base'Class;");
               PL (CU, "   Already_Unmarshalled : in out "
                   & "Broca.Value.Stream.ISeq.Sequence;");
               PL (CU, "   With_Chunking : in Boolean;");
               PL (CU, "   Nesting_Depth : in CORBA.Long;");
               PL (CU, "   Closing_Tag_Read : out CORBA.Long) is");
               II (CU);
               PL (CU,
                   "Obj : "
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object_Ptr := null;");
               PL (CU, "use CORBA;");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               Add_With (CU, "CORBA.Value");
               PL (CU, "if CORBA.Value.Is_Nil (Val) then");
               II (CU);
               PL (CU, "Obj := new "
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object;");
               PL (CU, "CORBA.Value.Set (Val, CORBA.Impl.Object_Ptr (Obj));");
               DI (CU);
               PL (CU, "else");
               II (CU);
               PL (CU, "Obj := "
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object_Ptr");
               PL (CU, "  (CORBA.Value.Object_Of (Val));");
               PL (CU, "raise Program_Error;");
               PL (CU, "--  code should never come here for the moment");
               --  since truncatoin not supported

               DI (CU);
               PL (CU, "end if;");

               --  suppose there is no chunking

               --  unmarshall all the state members
               declare
                  It : Node_Iterator;
                  Current : Node_Id;
               begin
                  Init (It, Contents (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Current);
                     if Kind (Current) = K_State_Member then
                        pragma Debug (O ("Kind(State_Type (Current)) = "
                                         & Node_Kind'Image
                                         (Kind (State_Type (Current)))));
                        if Kind (State_Type (Current)) = K_Scoped_Name
                          and then Kind (Value (State_Type (Current)))
                          = K_ValueType then
                           --  unmarshall a valuetype state member
                           PL (CU, "Broca.Value.Stream.Unmarshall");
                           PL (CU, "  (Buffer,");
                           PL (CU,
                               "   CORBA.To_CORBA_String");
                           PL (CU, "     ("
                               & Parent_Scope_Name
                               (Head (State_Declarators (Current)))
                               & "."
                               & T_Repository_Id
                               & "),");
                           PL (CU,
                               "   Obj."
                               & Ada_Name (Head (State_Declarators (Current)))
                               & ",");
                           PL (CU, "Already_Unmarshalled,");
                           PL (CU, "With_Chunking,");
                           PL (CU, "Nesting_Depth + 1,");
                           PL (CU, "Closing_Tag_Read);");

                        else
                           --  unmarshall a standard state member
                           Add_With_Stream (CU, State_Type (Current));
                           PL (CU,
                               "Obj."
                               & Ada_Name (Head (State_Declarators (Current)))
                               & ":= Unmarshall (Buffer);");
                           --  State Members are expanded so that
                           --  their State_Declarators list contains
                           --  a single node
                        end if;
                     end if;
                  end loop;
               end;
               DI (CU);
               PL (CU, "end Unmarshall_Fields;");
            end if;

            --  register operations
            if not Abst (Node) then
               Divert (CU, Elaboration);
               NL (CU);
               PL (CU, "Broca.Value.Stream.Marshall_Store.Register_Operation");
               PL (CU, "  ("
                   & Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Impl.Suffix
                   & ".Object'Tag,");
               PL (CU, "   Marshall'Access);");
               NL (CU);
               PL (CU, "Broca.Value.Stream.Unmarshall_Fields_Store"
                   & ".Register_Operation");
               PL (CU, "  ("
                   & Ada_Full_Name (Node)
                   & "." & T_Repository_Id
                   & ",");
               PL (CU, "Unmarshall_Fields'Access);");
               Divert (CU, Visible_Declarations);
            end if;

         when K_Struct =>

            declare
               S_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  if Is_End (It) then
                     PL (CU, "--  Empty structure.");
                     PL (CU, "null;");
                  end if;
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);

                     Add_With_Stream (CU, M_Type (Member_Node));

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Get_Next_Node (DIt, Decl_Node);

                           PL (CU, "Marshall (Buffer, Val."
                                     & Ada_Name (Decl_Node)
                                     & ");");
                        end loop;
                     end;
                  end loop;
               end;

               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, Node);
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, T_Returns & " : " & S_Name & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Member_Node : Node_Id;
               begin
                  Init (It, Members (Node));
                  if Is_End (It) then
                     PL (CU, "--  Empty structure.");
                  end if;
                  while not Is_End (It) loop
                     Get_Next_Node (It, Member_Node);

                     declare
                        DIt   : Node_Iterator;
                        Decl_Node : Node_Id;
                     begin
                        Init (DIt, Decl (Member_Node));
                        while not Is_End (DIt) loop
                           Get_Next_Node (DIt, Decl_Node);

                           PL (CU, T_Returns & "."
                                     & Ada_Name (Decl_Node)
                                     & " := Unmarshall (Buffer);");
                        end loop;
                     end;

                  end loop;
               end;
               PL (CU, "return " & T_Returns & ";");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Union =>

            declare
               U_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               NL (CU);
               Gen_Marshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);

               Add_With_Stream (CU, Switch_Type (Node));
               PL (CU, "Marshall (Buffer, Val.Switch);");
               PL (CU, "case Val.Switch is");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Case_Node : Node_Id;
                  Has_Default : Boolean := False;
               begin
                  Init (It, Cases (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Case_Node);

                     NL (CU);
                     Gen_When_Clause (CU, Case_Node, Has_Default);
                     II (CU);
                     Add_With_Stream (CU, Case_Type (Case_Node));
                     PL (CU, "Marshall (Buffer, Val."
                               & Ada_Name
                               (Case_Decl (Case_Node))
                               & ");");
                     DI (CU);
                  end loop;

                  if not Has_Default then
                     Gen_When_Others_Clause (CU);
                  end if;
               end;

               DI (CU);
               PL (CU, "end case;");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, Node);
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, "Switch : "
                         & Ada_Type_Name (Switch_Type (Node))
                         & ";");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "Switch := Unmarshall (Buffer);");
               NL (CU);
               PL (CU, "declare");
               II (CU);
               PL (CU, T_Returns & " : " & U_Name & " (Switch);");
               DI (CU);
               PL (CU, "begin");
               II (CU);
               PL (CU, "case Switch is");
               II (CU);

               declare
                  It   : Node_Iterator;
                  Case_Node : Node_Id;
                  Has_Default : Boolean := False;
               begin
                  Init (It, Cases (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, Case_Node);

                     NL (CU);
                     Gen_When_Clause (CU, Case_Node, Has_Default);
                     II (CU);
                     PL (CU, T_Returns & "."
                         & Ada_Name (Case_Decl (Case_Node))
                         & " := Unmarshall (Buffer);");
                     DI (CU);

                  end loop;
                  if not Has_Default then
                     Gen_When_Others_Clause (CU);
                  end if;
               end;

               DI (CU);
               PL (CU, "end case;");
               NL (CU);
               PL (CU, "return " & T_Returns & ";");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Enum =>

            declare
               E_Name : constant String
                 := Ada_Type_Name (Node);
            begin
               Add_With (CU, "CORBA");
               Add_With (CU, "Broca.CDR", Use_It => True);

               NL (CU);
               Gen_Marshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "if Val'Valid then");
               II (CU);
               PL (CU, "Marshall");
               PL (CU, "  (Buffer,");
               PL (CU, "   CORBA.Unsigned_Long ("
                   & E_Name & "'Pos (Val)));");
               DI (CU);
               PL (CU, "else");
               II (CU);
               PL (CU, "Marshall");
               PL (CU, "  (Buffer,");
               PL (CU, "   CORBA.Unsigned_Long'(0));");
               DI (CU);
               PL (CU, "end if;");
               DI (CU);
               PL (CU, "end Marshall;");

               NL (CU);
               Gen_Unmarshall_Profile (CU, Node);
               PL (CU, " is");
               PL (CU, "begin");
               II (CU);
               PL (CU, "return " & E_Name
                         &"'Val");
               PL
                 (CU,
                  "  (CORBA.Unsigned_Long'(Unmarshall (Buffer)));");
               DI (CU);
               PL (CU, "end Unmarshall;");
            end;

         when K_Type_Declarator =>

            declare
               Is_Interface : constant Boolean
                 := Is_Interface_Type (T_Type (Node));
               Is_Fixed : constant Boolean
                 := Kind (T_Type (Node)) = K_Fixed;
            begin
               if Is_Interface then
                  null;
               elsif Is_Fixed then
                  declare
                     Decl_Node : constant Node_Id
                       := Head (Declarators (Node));
                     Type_Name : constant String
                       := Ada_Name (Decl_Node);
                  begin
                     NL (CU);
                     PL (CU, "package CDR_"
                         & Type_Name & " is");
                     PL (CU, "  new Broca.CDR.Fixed_Point ("
                         & Type_Name & ");");

                     Gen_Marshall_Profile
                       (CU, Decl_Node);
                     PL (CU, " renames CDR_" & Type_Name
                         & ".Marshall;");
                     Gen_Unmarshall_Profile
                       (CU, Decl_Node);
                     PL (CU, " renames CDR_" & Type_Name
                         & ".Unmarshall;");
                  end;
               else
                  declare
                     Base_Type_Name : constant String
                       := Ada_Type_Name (T_Type (Node));

                     It   : Node_Iterator;
                     Decl_Node : Node_Id;
                  begin
                     Add_With_Stream (CU, T_Type (Node));
                     Init (It, Declarators (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, Decl_Node);

                        declare
                           Type_Name : constant String
                             := Ada_Type_Name (Decl_Node);
                           Array_Dimensions : constant Natural
                             := Length (Array_Bounds (Decl_Node));
                        begin
                           NL (CU);
                           Gen_Marshall_Profile
                             (CU, Decl_Node);
                           PL (CU, " is");
                           PL (CU, "begin");
                           II (CU);
                           if Array_Dimensions /= 0 then
                              Gen_Array_Iterator
                                (CU, "Val", Array_Dimensions,
                                 "Marshall (Buffer, Val %);");
                           else
                              PL (CU, "Marshall");
                              PL (CU, "  (Buffer,");
                              PL (CU, "   "
                                  & Base_Type_Name
                                  & " (Val));");
                           end if;
                           DI (CU);
                           PL (CU, "end Marshall;");

                           NL (CU);
                           Gen_Unmarshall_Profile
                             (CU, Decl_Node);
                           if Array_Dimensions /= 0 then
                              NL (CU);
                              PL (CU, "is");
                              II (CU);
                              PL (CU, T_Returns & " : " & Type_Name & ";");
                              DI (CU);
                              PL (CU, "begin");
                              II (CU);

                              Gen_Array_Iterator
                                (CU, T_Returns, Array_Dimensions,
                                 T_Returns & " % := Unmarshall (Buffer);");

                              PL (CU, "return " & T_Returns & ";");
                           else
                              PL (CU, " is");
                              PL (CU, "begin");
                              II (CU);
                              PL (CU, "return " & Type_Name);
                              PL (CU, "  (" & Base_Type_Name & "'");
                              PL (CU, "   (Unmarshall (Buffer)));");
                           end if;

                           DI (CU);
                           PL (CU, "end Unmarshall;");
                        end;
                     end loop;
                  end;
               end if;
            end;

         when
           K_Interface         |
           K_Forward_Interface =>
            Add_With (CU, "Broca.CDR");
--             if NK = K_Forward_Interface then
--                NL (CU);
--                Gen_Marshall_Profile (CU, Node);
--                if NK = K_Forward_Interface then
--                   NL (CU);
--                   PL (CU, "is");
--                   II (CU);
--                   PL (CU, "use " & Ada_Name (Node) & ";");
--                   DI (CU);
--                else
--                   PL (CU, " is");
--                end if;
--                PL (CU, "begin");
--                II (CU);
--                PL (CU, "Broca.CDR.Marshall (Buffer, Val);");
--                DI (CU);
--                PL (CU, "end Marshall;");
--             end if;

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            if NK = K_Forward_Interface then
               NL (CU);
               PL (CU, "is");
               II (CU);
               PL (CU, "use " & Ada_Name (Node) & ";");
               DI (CU);
            else
               PL (CU, " is");
            end if;
            II (CU);
            PL (CU, "New_Ref : " & Ada_Type_Name (Node) & ";");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "Broca.CDR.Unmarshall (Buffer, New_Ref);");
            PL (CU, "return New_Ref;");
            DI (CU);
            PL (CU, "end Unmarshall;");

         when K_Sequence_Instance =>

            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Elements : constant Element_Array");
            PL (CU, "  := To_Element_Array (Val);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "Marshall");
            PL (CU, "  (Buffer, CORBA.Unsigned_Long (Elements'Length));");

            Add_With_Stream (CU, Sequence_Type (Sequence (Node)));
            PL (CU, "for I in Elements'Range loop");
            II (CU);
            PL (CU, "Marshall (Buffer, Elements (I));");
            DI (CU);
            PL (CU, "end loop;");
            DI (CU);
            PL (CU, "end Marshall;");

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            NL (CU);
            PL (CU, "is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Length : constant CORBA.Unsigned_Long");
            PL (CU, "  := Unmarshall (Buffer);");
            PL (CU, "Elements : Element_Array (1 .. Integer (Length));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "for I in Elements'Range loop");
            II (CU);
            PL (CU, "Elements (I) := Unmarshall (Buffer);");
            DI (CU);
            PL (CU, "end loop;");
            PL (CU, "return To_Sequence (Elements);");
            DI (CU);
            PL (CU, "end Unmarshall;");

         when K_String_Instance =>

            NL (CU);
            Gen_Marshall_Profile (CU, Node);
            PL (CU, " is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            if Is_Wide (Node) then
               PL (CU, "Data : constant CORBA.Wide_String");
            else
               PL (CU, "Data : constant CORBA.String");
            end if;
            PL (CU, "  := To_String (Val);");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            Add_With (CU, "Broca.CDR");
            PL (CU, "Marshall");
            PL (CU, "  (Buffer, Data);");
            DI (CU);
            PL (CU, "end Marshall;");

            NL (CU);
            Gen_Unmarshall_Profile (CU, Node);
            NL (CU);
            PL (CU, "is");
            II (CU);
            PL (CU, "use " & Ada_Name (Node) & ";");
            NL (CU);
            PL (CU, "Data : constant Bounded_String");
            PL (CU, "  := To_Bounded_String (Unmarshall (Buffer));");
            DI (CU);
            PL (CU, "begin");
            II (CU);
            PL (CU, "return Data;");
            DI (CU);
            PL (CU, "end Unmarshall;");

         when others =>
            null;
      end case;

   end Gen_Node_Body;

end Ada_Be.Idl2Ada.Stream;
