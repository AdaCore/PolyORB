-----------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--            A D A _ B E . I D L 2 A D A . S K E L                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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
with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;

with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

package body Ada_Be.Idl2Ada.Skel is

   Flag : constant Natural := Ada_Be.Debug.Is_Active
     ("ada_be.idl2ada.skel");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   Want_Interface_Repository : constant Boolean := False;

   procedure Gen_Is_A
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id);
   --  Generate server-side support for the Is_A
   --  operation.

   procedure Gen_Body_Common_Start
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id);
   --  generates code for skel_body that is common
   --  for interfaces and valuetypes supporting interfaces
   --  at the beginning of the package.

   procedure Gen_Invoke
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate a static dispatcher fragment for operation Node.

   -------------------
   -- Gen_Node_Spec --
   -------------------

   procedure Gen_Node_Spec
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is
         when K_ValueType =>
            --  ValueTypes cannot have delegates
            pragma Assert (SK = Skeleton);

            if Supports (Node) /= Nil_List then
               Add_Elaborate_Body (CU);
            end if;

         when K_Interface =>
            if not Abst (Node) then
               --  No skel or impl packages are generated for
               --  abstract interfaces.
               if SK = Skeleton then
                  Add_Elaborate_Body (CU);
               end if;
            end if;

         when others =>
            null;
      end case;
   end Gen_Node_Spec;

   -------------------
   -- Gen_Node_Body --
   -------------------

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is

         when K_ValueType =>
            pragma Assert (SK = Skeleton);
            if Supports (Node) /= Nil_List then
               Gen_Body_Common_Start (CU, SK, Node);
               Gen_Is_A (CU, SK, Node);
               Gen_Invoke (CU, Node);

               declare
                  It     : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Supports (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     Add_With (CU,
                               Ada_Full_Name (P_Node) & ".Skel",
                               Elab_Control => Elaborate);
                  end loop;
               end;
            end if;

         when K_Interface =>
            if not Abst (Node) then
               --  No skel or impl packages are generated for
               --  abstract interfaces.
               Add_With (CU, "PolyORB.CORBA_P.Exceptions");
               Add_With (CU, "PortableServer",
                         Use_It => False,
                         Elab_Control => Elaborate_All);

--                if SK = Skeleton then
--                   Add_With (CU, Ada_Full_Name (Node) & Impl.Suffix);
--                   NL (CU);
--                   PL (CU,
--                       "type Object_Ptr is access all "
--                       & Ada_Full_Name (Node)
--                       & Impl.Suffix
--                       & ".Object'Class;");
--                end if;

               Gen_Body_Common_Start (CU, SK, Node);
               Gen_Is_A (CU, SK, Node);
               Gen_Invoke (CU, Node);
            end if;

         when K_Operation =>

            Gen_Invoke (CU, Node);

         when others =>
            null;
      end case;
   end Gen_Node_Body;

   --------------
   -- Gen_Is_A --
   --------------

   procedure Gen_Is_A
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface)
                     or else (NK = K_ValueType));
      --  FIXME: Hard-coded string constant.

      PL (CU, "if Operation = ""_is_a"" then");
      II (CU);

      PL (CU, "declare");
      II (CU);

      PL (CU, "Type_Id            : CORBA.String;");
      PL (CU, "Arg_Name_Ü_Type_Id : constant CORBA.Identifier");
      PL (CU, ":= CORBA.To_CORBA_String (""Type_Id"");");
      PL (CU, "Argument_Ü_Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);");
      PL (CU, "");
      PL (CU, "Result_Ü           : CORBA.Boolean;");
      PL (CU, "Argument_Ü_Result_Ü : CORBA.Any;");
      PL (CU, "Ctx_Ü              : CORBA.Context.Ref");
      PL (CU, "  := CORBA.Context.Nil_Ref;");
      PL (CU, "Arg_List_Ü         : CORBA.NVList.Ref;");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "--  Create argument list");
      NL (CU);
      PL (CU, "CORBA.ORB.Create_List (0, Arg_List_Ü);");
      PL (CU, "CORBA.NVList.Add_Item");
      PL (CU, "(Arg_List_Ü,");
      PL (CU, "Arg_Name_Ü_Type_Id,");
      PL (CU, "Argument_Ü_Type_Id,");
      PL (CU, "CORBA.ARG_IN);");
      NL (CU);

      PL (CU, "CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);");
      NL (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "--  Convert arguments from their Any");
      NL (CU);
      PL (CU, "Type_Id :=");
      PL (CU, "  CORBA.From_Any (Argument_Ü_Type_Id);");
      PL (CU, "--  Call implementation");
      Put (CU, "Result_Ü := ");

      if NK = K_Interface then
         Put (CU, Ada_Full_Name (Node));
      else
         pragma Assert (SK = Skeleton);
         Add_With (CU,
                   Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Value_Skel.Suffix);
         Put (CU,
              Ada_Full_Name (Node)
              & Ada_Be.Idl2Ada.Value_Skel.Suffix);
      end if;
      PL (CU, ".Is_A");
      PL (CU, "  (CORBA.To_Standard_String (Type_Id));");
      DI (CU);

      PL (CU, "end;");
      NL (CU);
      PL (CU, "-- Set Result");
      NL (CU);
      PL (CU, "CORBA.ServerRequest.Set_Result");
      PL (CU, "(Request,");
      PL (CU, "CORBA.To_Any (");
      PL (CU, "Result_Ü));");
      PL (CU, "return;");
      DI (CU);
      PL (CU, "end;");
      DI (CU);
      PL (CU, "end if;");
   end Gen_Is_A;

   ----------------------------
   --  Gen_Body_Common_Start --
   ----------------------------

   procedure Gen_Body_Common_Start
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface)
                     or else (NK = K_ValueType));
      Add_With (CU, "PortableServer");
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      --  CORBA.To_CORBA_String is used in skel elab.

      PL (CU, "pragma Warnings (Off);");
      PL (CU, "--  Some variables may be unused, and some style");
      PL (CU, "--  checks may not pass.");
      NL (CU);
      PL (CU, "--  Skeleton subprograms");
      NL (CU);
      PL (CU, "function Servant_Is_A");
      PL (CU, "  (Obj : PortableServer.Servant)");
      PL (CU, "  return Boolean;");
      NL (CU);
      PL (CU, "function Servant_Is_A");
      PL (CU, "  (Obj : PortableServer.Servant)");
      PL (CU, "  return Boolean is");
      PL (CU, "begin");
      II (CU);
      Put (CU, "return Obj.all in ");
      if SK = Delegate then
         Put (CU, "Object'Class");
      elsif NK = K_Interface then
         Add_With (CU,
                   Ada_Full_Name (Node)
                   & Impl.Suffix);
         Put (CU,
              Ada_Full_Name (Node)
              & Impl.Suffix
              & ".Object'Class");
      else
         Add_With (CU,
                   Ada_Full_Name (Node)
                   & Ada_Be.Idl2Ada.Helper.Suffix);
         Put (CU,
              Ada_Full_Name (Node)
              & Ada_Be.Idl2Ada.Helper.Suffix
              & ".Servant'Class");
      end if;
      PL (CU, ";");
      DI (CU);
      PL (CU, "end Servant_Is_A;");
      Add_With (CU, "CORBA.ServerRequest");
      NL (CU);
      PL (CU, "procedure Invoke");
      PL (CU, "  (Self : PortableServer.Servant;");
      II (CU);
      PL (CU, "Request : in CORBA.ServerRequest.Object_ptr)");
      DI (CU);
      PL (CU, "is");
      II (CU);
      PL (CU, "Operation : constant Standard.String");
      PL (CU, "   := CORBA.To_Standard_String");
      PL (CU, "        (CORBA.ServerRequest.Operation");
      PL (CU, "         (Request.all));");
      DI (CU);
      PL (CU, "begin");
      II (CU);
   end Gen_Body_Common_Start;

   -------------------------
   -- Gen_Body_Common_End --
   -------------------------

   procedure Gen_Body_Common_End
     (CU   : in out Compilation_Unit;
      SK   : in Skel_Kind;
      Node : in Node_Id)
   is
      NK     : constant Node_Kind := Kind (Node);
      It     : Node_Iterator;
      P_Node : Node_Id;
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));
      NL (CU);
      PL (CU, "PolyORB.CORBA_P.Exceptions.Raise_Bad_Operation;");
      DI (CU);
      PL (CU, "end Invoke;");

      Divert (CU, Elaboration);

      if SK = Skeleton then
         Init (It, Parents (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, P_Node);
            Add_With (CU,
                      Ada_Full_Name (P_Node) & ".Skel",
                      Elab_Control => Elaborate);
         end loop;
      end if;

      PL (CU, "PortableServer.Register_Skeleton");
      Put (CU, "  (CORBA.To_CORBA_String (");
      Put (CU, Ada_Full_Name (Node));
      PL (CU, "." & Repository_Id_Name (Node) &"),");
      if SK = Skeleton then
         PL (CU, "   Servant_Is_A'Access,");
         PL (CU, "   Invoke'Access);");
      else
         PL (CU, "   Servant_Is_A'Unrestricted_Access,");
         PL (CU, "   Invoke'Unrestricted_Access);");
      end if;
   end Gen_Body_Common_End;

   ------------
   -- Suffix --
   ------------

   function Suffix (SK : Skel_Kind) return String is
   begin
      case SK is
         when Skeleton => return ".Skel";
         when Delegate => return ".Delegate";
      end case;
   end Suffix;

   ----------------
   -- Gen_Invoke --
   ----------------

   procedure Gen_Invoke
     (CU   : in out Compilation_Unit;
      Node : Node_Id) is
   begin
      case Kind (Node) is

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>


            declare
               --  O_Name : constant String
               --    := Ada_Operation_Name (Node);
               O_Type : constant Node_Id
                 := Operation_Type (Node);
               Response_Expected : constant Boolean
                 := not Is_Oneway (Node);
               Is_Function : constant Boolean
                 := Kind (O_Type) /= K_Void;
               Raise_Something : constant Boolean
                 := not (Raises (Node) = Nil_List);

               Max_Len : Integer := T_Result_Name'Length;

            begin
               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     if not Is_Returns (P_Node) then
                        declare
                           L : constant Integer
                             := Ada_Name (Declarator (P_Node))'Length;
                        begin
                           if Max_Len < (L + T_Argument'Length) then
                              Max_Len := L + T_Argument'Length;
                           end if;
                        end;
                     end if;
                  end loop;
               end;

               NL (CU);
               PL (CU,
                   "if Operation = """
                   & Idl_Operation_Id (Node)
                   & """ then");
               II (CU);

               NL (CU);
               PL (CU, "declare");
               II (CU);

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     declare
                        Arg_Name : constant String
                          := Ada_Name (Declarator (P_Node));
                        Helper_Name : constant String
                          := Helper_Unit (Param_Type (P_Node));
                     begin

                        PL (CU, Justify (Arg_Name, Max_Len) & " : "
                            & Ada_Type_Name (Param_Type (P_Node))  & ";");

                        if not Is_Returns (P_Node) then
                           PL (CU, Justify (T_Arg_Name & Arg_Name, Max_Len)
                               & " : constant CORBA.Identifier");
                           PL (CU, "  := CORBA.To_CORBA_String ("""
                               & Arg_Name & """);");
                        end if;

                        Add_With (CU, Helper_Name);


                        Put (CU, Justify (T_Argument & Arg_Name, Max_Len)
                            & " : CORBA.Any := " & Helper_Name & ".To_Any"
                            & " (");
                        Gen_Forward_Conversion
                          (CU, Param_Type (P_Node), "From_Forward",  Arg_Name);
                        PL (CU, ");");
                        NL (CU);
                     end;
                  end loop;
               end;

               if Is_Function then
                  PL (CU, Justify (T_Result, Max_Len)
                      & " : " & Ada_Type_Name (O_Type) & ";");
                  PL (CU, Justify (T_Argument & T_Result, Max_Len)
                      & " : CORBA.Any;");
               end if;

               Add_With (CU, "CORBA.Context");
               Add_With (CU, "CORBA.NVList");
               PL (CU, Justify (T_Ctx, Max_Len)
                   & " : CORBA.Context.Ref := CORBA.Context.Nil_Ref;");
               PL (CU, Justify (T_Arg_List, Max_Len) & " : CORBA.NVList.Ref;");
               if Raise_Something then
                  Add_With (CU, "CORBA.ExceptionList");
                  PL (CU, Justify (T_Excp_List, Max_Len)
                    & " : CORBA.ExceptionList.Ref;");
               end if;

               DI (CU);
               PL (CU, "begin");
               II (CU);


               PL (CU, "--  Create argument list");
               NL (CU);
               Add_With (CU, "CORBA.ORB");
               PL (CU, "CORBA.ORB.Create_List (0, " & T_Arg_List & ");");

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop

                     Get_Next_Node (It, P_Node);

                     if not Is_Returns (P_Node) then
                        declare
                           Arg_Name : constant String
                             := Ada_Name (Declarator (P_Node));
                        begin
                           PL (CU, "CORBA.NVList.Add_Item");
                           PL (CU, "  (" & T_Arg_List & ",");
                           II (CU);
                           PL (CU, T_Arg_Name & Arg_Name & ",");
                           PL (CU, T_Argument & Arg_Name & ",");

                           case Mode (P_Node) is
                              when Mode_In =>
                                 PL (CU, "CORBA.ARG_IN);");
                              when Mode_Inout =>
                                 PL (CU, "CORBA.ARG_INOUT);");
                              when Mode_Out =>
                                 PL (CU, "CORBA.ARG_OUT);");
                              when others =>
                                 null;
                           end case;
                           DI (CU);
                        end;
                     end if;
                  end loop;
               end;

               NL (CU);
               PL (CU, "CORBA.ServerRequest.Arguments (Request, "
                   & T_Arg_List & ");");
               NL (CU);


               PL (CU, "begin");
               II (CU);
               PL (CU, "--  Convert arguments from their Any");
               NL (CU);

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop

                     Get_Next_Node (It, P_Node);

                     declare
                        Arg_Name : constant String
                          := Ada_Name (Declarator (P_Node));
                        PT_Node : constant Node_Id
                          := Param_Type (P_Node);
                        Helper_Name : constant String
                          := Helper_Unit (PT_Node);
                     begin
                        if not Is_Returns (P_Node) then
                           if Mode (P_Node) = Mode_In
                             or else Mode (P_Node) = Mode_Inout
                           then
                              PL (CU, Arg_Name & " :=");
                              Put (CU, "  ");
                              Gen_Forward_Conversion
                                (CU, PT_Node, "To_Forward",
                                 Helper_Name & ".From_Any ("
                                 & T_Argument & Arg_Name & ")");
                              PL (CU, ";");
                           end if;
                        end if;
                     end;

                  end loop;
               end;

               PL (CU, "--  Call implementation");

               if Is_Function then
                  Put (CU, T_Result & " := ");
               end if;

               declare
                  Impl_Name : constant String
                    := Ada_Full_Name (Parent_Scope (Node))
                    & ".Impl";
               begin
                  PL (CU, Impl_Name & "."
                      & Ada_Operation_Name (Node));
                  Put (CU, "  (" & Impl_Name
                       & ".Object'Class (Self.all)'Access");
               end;

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     Put (CU, ", " & Ada_Name (Declarator (P_Node)));
                  end loop;
               end;
               PL (CU, ");");

               if Raise_Something then
                  DI (CU);
                  PL (CU, "exception");
                  II (CU);

                  declare
                     It : Node_Iterator;
                     R_Node : Node_Id;
                     E_Node : Node_Id;
                  begin
                     Init (It, Raises (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, R_Node);
                        E_Node := Value (R_Node);
                        Add_With_Entity (CU, E_Node);

                        declare
                           Prefix : constant String
                             := Helper_Unit (E_Node);
                        begin
                           Add_With (CU, Prefix);

                           PL (CU, "when E : " & Ada_Name (E_Node) & " =>");
                           II (CU);
                           PL (CU, "declare");
                           II (CU);
                           PL (CU, "Members : " & Ada_Name (E_Node)
                               & "_Members;");
                           DI (CU);
                           PL (CU, "begin");
                           II (CU);
                           PL (CU, "Get_Members (E, Members);");
                           PL (CU, "CORBA.ServerRequest.Set_Exception");
                           PL (CU, "  (Request,");
                           II (CU);
                           PL (CU, Prefix & ".To_Any (Members));");
                           DI (CU);
                           PL (CU, "return;");
                           DI (CU);
                           PL (CU, "end;");
                           DI (CU);
                        end;
                     end loop;
                  end;
               end if;

--                PL (CU, "when others =>");
--                II (CU);
--                PL (CU, "declare");
--                II (CU);
--                PL (CU, "use CORBA;");
--                PL (CU, "Members : constant Unknown_Members");
--                PL (CU, "  := (Minor => 1, Completed => Completed_Maybe);");
--                DI (CU);
--                PL (CU, "begin");
--                II (CU);
--                PL (CU, "CORBA.ServerRequest.Set_Exception");
--                PL (CU, "  (Request,");
--                II (CU);
--                PL (CU, "CORBA.To_Any (Members));");
--                DI (CU);
--                PL (CU, "return;");
--                DI (CU);
--                PL (CU, "end;");
--                DI (CU);

               DI (CU);
               PL (CU, "end;");

               if Response_Expected then

                  declare
                     It   : Node_Iterator;
                     P_Node : Node_Id;
                     First : Boolean := True;
                  begin
                     Init (It, Parameters (Node));
                     while not Is_End (It) loop
                        Get_Next_Node (It, P_Node);

                        if not Is_Returns (P_Node) then


                           if Mode (P_Node) =  Mode_Inout
                             or else Mode (P_Node) = Mode_Out
                           then
                              declare
                                 Arg_Name : constant String
                                   := Ada_Name (Declarator (P_Node));
                                 Helper_Name : constant String
                                   := Helper_Unit (Param_Type (P_Node));
                              begin

                                 if First then
                                    NL (CU);
                                    PL (CU, "--  Set out arguments.");
                                    NL (CU);
                                    First := False;
                                 end if;

                                 Add_With (CU, Helper_Name);
                                 Add_With (CU, "PolyORB.Any");
                                 PL (CU, "PolyORB.Any.Copy_Any_Value");
                                 PL (CU, "  (" & T_Argument & Arg_Name & ",");
                                 II (CU);
                                 PL (CU, Helper_Name & ".To_Any");
                                 Put (CU, "  (");
                                 II (CU);
                                 Gen_Forward_Conversion
                                   (CU, Param_Type (P_Node),
                                    "From_Forward", Arg_Name);
                                 DI (CU);
                                 PL (CU, "));");
                                 DI (CU);
                                 NL (CU);
                              end;
                           end if;

                        end if;

                     end loop;
                  end;

                  if Kind (Original_Operation_Type (Node)) /= K_Void then
                     NL (CU);
                     PL (CU, "-- Set Result");
                     NL (CU);

                     declare
                        OT_Node : constant Node_Id
                          := Original_Operation_Type (Node);
                        Prefix : constant String
                          := Helper_Unit (OT_Node);
                     begin
                        Add_With (CU, Prefix);

                        if Is_Function then
                           PL (CU, "CORBA.ServerRequest.Set_Result");
                           PL (CU, "  (Request, ");
                           II (CU);
                           PL (CU, Prefix & ".To_Any (");
                           Gen_Forward_Conversion
                             (CU, OT_Node,
                              "From_Forward", T_Result);
                           PL (CU, "));");
                           DI (CU);
                        else
                           PL (CU, "CORBA.ServerRequest.Set_Result");
                           PL (CU, "  (Request, ");
                           II (CU);
                           PL (CU, Prefix & ".To_Any (");
                           Gen_Forward_Conversion
                             (CU, OT_Node,
                              "From_Forward", "Returns");
                           PL (CU, "));");
                           DI (CU);
                        end if;
                     end;
                  end if;
               end if;

               PL (CU, "return;");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
               PL (CU, "end if;");
            end;

         when others =>
            null;

      end case;

   end Gen_Invoke;

end Ada_Be.Idl2Ada.Skel;
