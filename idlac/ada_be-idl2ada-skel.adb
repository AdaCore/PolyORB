-----------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--            A D A _ B E . I D L 2 A D A . S K E L                         --
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
with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;

with Ada_Be.Debug;

package body Ada_Be.Idl2Ada.Skel is

   Flag : constant Natural := Ada_Be.Debug.Is_Active
     ("ada_be.idl2ada.skel");
   procedure O is new Ada_Be.Debug.Output (Flag);

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
               Add_With (CU, "Broca.Buffers");
               Add_With (CU, "Broca.Exceptions");
               Add_With (CU, "PortableServer",
                         Use_It => False,
                         Elab_Control => Elaborate_All);

               if SK = Skeleton then
                  Add_With (CU, Ada_Full_Name (Node) & Impl.Suffix);
                  NL (CU);
                  PL (CU,
                      "type Object_Ptr is access all "
                      & Ada_Full_Name (Node)
                      & Impl.Suffix
                      & ".Object'Class;");
               end if;

               Gen_Body_Common_Start (CU, SK, Node);
               Gen_Is_A (CU, SK, Node);
            end if;

         when K_Operation =>

            Add_With (CU, "Broca.CDR", Use_It => True);
            Add_With (CU, "Broca.GIOP");

            declare
               I_Node : constant Node_Id := Parent_Scope (Node);
               --  The Interface Or valuetype node that contains
               --  this operation.

               Operation_Type_Node : constant Node_Id :=
                 Operation_Type (Node);
               Is_Function : constant Boolean :=
                 Kind (Operation_Type_Node) /= K_Void;
               Is_Supported : constant Boolean :=
                 Kind (I_Node) = K_ValueType;
            begin
               pragma Debug (O ("Node is a " & Kind (Node)'Img));
               pragma Debug (O ("Its parent scope is a " & Kind (I_Node)'Img));
               pragma Assert (Kind (I_Node) = K_Interface
                              or else Kind (I_Node) = K_ValueType);

               NL (CU);
               PL (CU,
                   "if Operation = """
                   & Idl_Operation_Id (Node)
                   & """ then");
               II (CU);
               NL (CU);
               PL (CU, "--  Sanity check");
               if Is_Oneway (Node) then
                  PL (CU, "if Response_Expected then");
               else
                  PL (CU, "if not Response_Expected then");
               end if;
               II (CU);
               Add_With (CU,
                         "Broca.Exceptions");
               PL (CU, "Broca.Exceptions.Raise_Bad_Param;");
               DI (CU);
               PL (CU, "end if;");
               NL (CU);

               PL (CU, "declare");
               II (CU);

               --  Declare local args
               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin

                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     PL (CU, "IDL_"
                         & Ada_Name (Declarator (P_Node))
                         & " : "
                         & Ada_Type_Name (Param_Type (P_Node)) & ";");
                  end loop;
               end;

               if Is_Function then
                  PL (CU, T_Returns & " : "
                      & Ada_Type_Name (Operation_Type_Node)
                      & ";");
               end if;

               if Is_Supported then
                  Add_With (CU,
                            Ada_Full_Name (I_Node)
                            & Ada_Be.Idl2Ada.Value_Skel.Suffix);
                  PL (CU,
                      Ada_Be.Temporaries.T_Value_Operation
                      & " : "
                      & Ada_Full_Name (I_Node)
                      & Ada_Be.Idl2Ada.Value_Skel.Suffix
                      & "."
                      & Ada_Operation_Name (Node)
                      & "_Type;");

               end if;

               DI (CU);
               PL (CU, "begin");
               II (CU);

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     if First then
                        NL (CU);
                        PL (CU, "--  Unmarshall in and inout arguments");
                        First := False;
                     end if;

                     case Mode (P_Node) is
                        when
                          Mode_In    |
                          Mode_Inout =>
                           Add_With_Stream (CU, Param_Type (P_Node));

                           PL (CU, "IDL_"
                               & Ada_Name (Declarator (P_Node))
                               & " := Unmarshall (Request_Buffer);");
                        when others =>
                           null;
                     end case;
                  end loop;
               end;

               NL (CU);
               PL (CU, "begin");
               --  This begin is for the "exception" statement below
               --  to catch exceptions that can be raised during invocation
               --  of the servant's method.
               II (CU);

               if Is_Supported then
                  PL (CU, "--  Get the operation");
                  PL (CU,
                      Ada_Be.Temporaries.T_Value_Operation
                      & " := "
                      & Ada_Full_Name (I_Node)
                      & Ada_Be.Idl2Ada.Value_Skel.Suffix
                      & "."
                      & Ada_Operation_Name (Node)
                      & "_Store.Get_Operation");
                  Add_With (CU,
                            Ada_Full_Name (I_Node)
                            & Ada_Be.Idl2Ada.Helper.Suffix);
                  PL (CU, "  ("
                      & Ada_Full_Name (I_Node)
                      & Ada_Be.Idl2Ada.Helper.Suffix
                      & ".Servant_Ref (Obj).Value.all'Tag);");
                  NL (CU);
               end if;

               PL (CU, "--  Call implementation");

               if Is_Function then
                  Put (CU, T_Returns & " := ");
               end if;

               if Is_Supported then
                  PL (CU, Ada_Be.Temporaries.T_Value_Operation);
                  Add_With (CU, "CORBA.Impl");
                  PL (CU, "  (CORBA.Impl.Object_Ptr");
                  Put (CU, "   ("
                       & Ada_Full_Name (I_Node)
                       & Ada_Be.Idl2Ada.Helper.Suffix
                       & ".Servant_Ref (Obj).Value)");
               else
                  if SK = Skeleton then
                     PL (CU, Ada_Full_Name (I_Node) & Impl.Suffix
                         & "." & Ada_Name (Node));
                  else
                     PL (CU, Ada_Name (Node));
                  end if;
                  Put (CU, "  (Object_Ptr (Obj)");
                  if SK = Delegate then
                     Put (CU, " .Real");
                  end if;
               end if;
               II (CU);

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin

                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     PL (CU, ",");
                     Put (CU, "IDL_"
                          & Ada_Name (Declarator (P_Node)));
                  end loop;
                  PL (CU, ");");
               end;
               DI (CU);

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
                     --  Each R_Node is a scoped_name
                     --  that denotes an exception.

                     Add_With_Entity (CU, E_Node);
                     NL (CU);
                     PL (CU, "when E : " & Ada_Full_Name (E_Node)
                         & " =>");
                     II (CU);

                     PL (CU, "declare");
                     II (CU);
                     PL (CU, T_Members & " : "
                         & Ada_Type_Name (Members_Type (E_Node))
                         & ";");
                     DI (CU);
                     PL (CU, "begin");
                     II (CU);
                     PL (CU, Parent_Scope_Name (E_Node)
                         & ".Get_Members (E, " & T_Members & ");");
                     NL (CU);
                     PL (CU, "--  Marshall service context");
                     PL (CU, "Marshall");
                     PL (CU, "  (Reply_Buffer,");
                     PL (CU,
                         "   CORBA.Unsigned_Long "
                         & "(Broca.GIOP.No_Context));");

                     NL (CU);
                     PL (CU, "--  Marshall request ID");
                     PL (CU, "Marshall (Reply_Buffer, Request_Id);");

                     NL (CU);
                     PL (CU, "--  Marshall reply status");
                     PL (CU, "Broca.GIOP.Marshall");
                     PL (CU, "  (Reply_Buffer,");
                     PL (CU, "   Broca.GIOP.User_Exception);");

                     NL (CU);
                     PL (CU, "--  Marshall exception");
                     PL (CU, "Marshall");
                     PL (CU, "  (Reply_Buffer, CORBA.String (");
                     PL (CU, "   " & Ada_Full_Name (Parent_Scope (E_Node))
                         & "." & Repository_Id_Name (E_Node) & "));");
                     Add_With_Stream (CU, Members_Type (E_Node));
                     PL (CU, "Marshall (Reply_Buffer, " & T_Members & ");");
                     PL (CU, "return;");
                     DI (CU);
                     PL (CU, "end;");

                     DI (CU);
                  end loop;

                  PL (CU, "when E : others =>");
                  II (CU);
                  NL (CU);
                  PL (CU, "--  An exception was raised which is not listed");
                  PL (CU, "--  in this operation's ""raises"" clause.");
                  NL (CU);
                  PL (CU, "Broca.Exceptions.User_Purge_Members (E);");
                  PL (CU, "Broca.Exceptions.Raise_Unknown");
                  PL (CU, "  (Status => CORBA.Completed_Maybe);");
                  DI (CU);
                  DI (CU);
                  PL (CU, "end;");
               end;


               --  FIXME: This code is duplicated (above for each
               --    exception that can be raised by this operation,
               --    and also in Gen_Is_A_Skel).

               NL (CU);
               PL (CU, "--  Marshall service context");
               PL (CU, "Marshall");
               PL (CU, "  (Reply_Buffer,");
               PL (CU, "   CORBA.Unsigned_Long (Broca.GIOP.No_Context));");

               NL (CU);
               PL (CU, "--  Marshall request ID");
               PL (CU, "Marshall (Reply_Buffer, Request_Id);");

               NL (CU);
               PL (CU, "--  Marshall reply status");
               PL (CU, "Broca.GIOP.Marshall");
               PL (CU, "  (Reply_Buffer,");
               PL (CU, "   Broca.GIOP.No_Exception);");

               if Kind (Original_Operation_Type (Node)) /= K_Void then
                  Add_With_Stream (CU, Original_Operation_Type (Node));
                  NL (CU);
                  PL (CU, "--  Marshall return value");
                  if Is_Function then
                     PL (CU, "Marshall (Reply_Buffer, " & T_Returns & ");");
                  else
                     PL (CU, "Marshall (Reply_Buffer, IDL_Returns);");
                  end if;
               end if;

               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
                  First : Boolean := True;
               begin
                  if First then
                     NL (CU);
                     PL (CU, "--  Marshall inout and out arguments");
                     First := False;
                  end if;

                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);

                     if not Is_Returns (P_Node) then
                        case Mode (P_Node) is
                           when
                             Mode_Inout |
                             Mode_Out   =>
                              Add_With_Stream (CU, Param_Type (P_Node));

                              PL (CU, "Marshall (Reply_Buffer, IDL_"
                                  & Ada_Name (Declarator (P_Node)) & ");");
                           when others =>
                              null;
                        end case;
                     end if;
                  end loop;
               end;

               PL (CU, "return;");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
               PL (CU, "end if;");
            end;

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
      PL (CU, "IDL_Logical_Type_Id : constant Standard.String");
      PL (CU, "  := Unmarshall (Request_Buffer);");
      PL (CU, T_Returns & " : constant CORBA.Boolean");
      Put (CU, "  := ");
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
      PL (CU, ".Is_A (IDL_Logical_Type_Id);");
      DI (CU);
      PL (CU, "begin");
      II (CU);

      --  FIXME: The following code fragment is duplicated
      --    (from Skel.Gen_Node_Body).
      PL (CU, "--  Marshall service context");
      PL (CU, "Marshall");
      PL (CU, "  (Reply_Buffer,");
      PL (CU, "   CORBA.Unsigned_Long (Broca.GIOP.No_Context));");
      NL (CU);
      PL (CU, "--  Marshall request ID");
      PL (CU, "Marshall (Reply_Buffer, Request_Id);");
      NL (CU);
      PL (CU, "--  Marshall reply status");
      PL (CU, "Broca.GIOP.Marshall");
      PL (CU, "  (Reply_Buffer,");
      PL (CU, "   Broca.GIOP.No_Exception);");
      NL (CU);
      PL (CU, "--  Marshall return value");
      Add_With (CU, "Broca.CDR");
      PL (CU, "Marshall (Reply_Buffer, " & T_Returns & ");");
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
      Add_With (CU, "CORBA");
      Add_With (CU, "Broca.Buffers");
      NL (CU);
      PL (CU, "--  Skeleton subprograms");
      NL (CU);
      PL (CU, "function Servant_Is_A");
      PL (CU, "  (Obj : PortableServer.Servant)");
      PL (CU, "  return Boolean;");
      PL (CU, "procedure GIOP_Dispatch");
      PL (CU, "  (Obj : PortableServer.Servant;");
      II (CU);
      PL (CU, "Operation : Standard.String;");
      PL (CU, "Request_Id : CORBA.Unsigned_Long;");
      PL (CU, "Response_Expected : CORBA.Boolean;");
      PL (CU,
          "Request_Buffer : access Broca.Buffers.Buffer_Type;");
      PL (CU,
          "Reply_Buffer   : access Broca.Buffers.Buffer_Type);");
      DI (CU);
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
      NL (CU);
      PL (CU, "procedure GIOP_Dispatch");
      PL (CU, "  (Obj : PortableServer.Servant;");
      II (CU);
      PL (CU, "Operation : Standard.String;");
      PL (CU, "Request_Id : CORBA.Unsigned_Long;");
      PL (CU, "Response_Expected : CORBA.Boolean;");
      PL (CU,
          "Request_Buffer : access Broca.Buffers.Buffer_Type;");
      PL (CU,
          "Reply_Buffer   : access Broca.Buffers.Buffer_Type) is");
      DI (CU);
      PL (CU, "begin");
      II (CU);
   end Gen_Body_Common_Start;

   ---------------------------
   --  Gen_Body_Common_End  --
   ---------------------------
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
      PL (CU, "Broca.Exceptions.Raise_Bad_Operation;");
      DI (CU);
      PL (CU, "end GIOP_Dispatch;");
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
         PL (CU, "   GIOP_Dispatch'Access);");
      else
         PL (CU, "   Servant_Is_A'Unrestricted_Access,");
         PL (CU, "   GIOP_Dispatch'Unrestricted_Access);");
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

end Ada_Be.Idl2Ada.Skel;
