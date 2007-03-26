------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A D A _ B E . I D L 2 A D A . S K E L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
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

with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;
with Ada_Be.Temporaries;    use Ada_Be.Temporaries;

with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

package body Ada_Be.Idl2Ada.Skel is

   Flag : constant Natural := Ada_Be.Debug.Is_Active
     ("ada_be.idl2ada.skel");
   procedure O is new Ada_Be.Debug.Output (Flag);
   pragma Warnings (Off);
   pragma Unreferenced (O);
   pragma Warnings (On);

   procedure Gen_Is_A
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean);
   --  Generate server-side support for the Is_A operation

   procedure Gen_Non_Existent
     (CU          : in out Compilation_Unit;
      Node        : Node_Id);
   --  Generate server-side support for the Non_Existent operation

   procedure Gen_Get_Interface
     (CU          : in out Compilation_Unit;
      Node        : Node_Id);
   --  Generate server-side support for the Get_Interface operation

   procedure Gen_Get_Domain_Managers
     (CU          : in out Compilation_Unit;
      Node        : Node_Id);
   --  Generate server-side support for the Get_Domain_Managers operation

   procedure Gen_Body_Common_Start
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean);
   --  generates code for skel_body that is common
   --  for interfaces and valuetypes supporting interfaces
   --  at the beginning of the package.

   procedure Gen_Invoke
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate a static dispatcher fragment for operation Node.

   Decls_Div : Diversion;

   -------------------
   -- Gen_Node_Body --
   -------------------

   procedure Gen_Node_Body
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is

         when K_ValueType =>
            pragma Assert (not Is_Delegate);
            if Supports (Node) /= Nil_List then
               Gen_Body_Common_Start (CU, Node, Is_Delegate);

               --  Predefined operations

               Gen_Is_A         (CU, Node, Is_Delegate);
               Gen_Non_Existent (CU, Node);

               declare
                  It     : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Supports (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     Add_With (CU,
                               Ada_Full_Name (P_Node)
                               & Skel.Suffix (Is_Delegate),
                               No_Warnings  => True);
                  end loop;
               end;
            end if;

         when K_Interface =>

            --  No skel or impl packages are generated for abstract interfaces

            if not Abst (Node) then
               Add_With (CU, "PolyORB.CORBA_P.Exceptions");
               Add_With (CU, "PortableServer",
                         Use_It       => False,
                         Elab_Control => Elaborate_All);

               Gen_Body_Common_Start (CU, Node, Is_Delegate);

               --  Predefined operations

               Gen_Is_A                (CU, Node, Is_Delegate);
               Gen_Non_Existent        (CU, Node);
               Gen_Get_Interface       (CU, Node);
               Gen_Get_Domain_Managers (CU, Node);

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
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));

      PL (CU, "if Operation = ""_is_a"" then");
      II (CU);

      PL (CU, "declare");
      II (CU);

      PL (CU, "Type_Id : CORBA.String;");
      Divert (CU, Decls_Div);
      NL (CU);
      PL (CU, "Is_A" & T_Arg_Name & "Type_Id :"
            & " constant CORBA.Identifier");
      PL (CU, ":= CORBA.To_CORBA_String (""Type_Id"");");
      Divert (CU, Operation_Body);
      PL (CU, T_Arg_Any & "Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);");
      PL (CU, "");
      PL (CU, T_Result & " : CORBA.Boolean;");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "CORBA.NVList.Add_Item");
      PL (CU, "(" & T_Arg_List & ",");
      PL (CU, "Is_A" & T_Arg_Name & "Type_Id,");
      PL (CU, T_Arg_Any & "Type_Id,");
      PL (CU, "CORBA.ARG_IN);");
      NL (CU);

      PL (CU, "CORBA.ServerRequest.Arguments (Request, " & T_Arg_List & ");");
      NL (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "--  Convert arguments from their Any");
      NL (CU);
      PL (CU, "Type_Id :=");
      PL (CU, "  CORBA.From_Any (" & T_Arg_Any & "Type_Id);");
      NL (CU);
      PL (CU, "--  Call implementation");
      NL (CU);
      Put (CU, T_Result & " := ");

      if NK = K_Interface then
         Put (CU, Ada_Full_Name (Node));
      else
         pragma Assert (not Is_Delegate);
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
      PL (CU, "CORBA.To_Any (" & T_Result & "));");
      DI (CU);
      PL (CU, "end;");
      DI (CU);
   end Gen_Is_A;

   ----------------------
   -- Gen_Non_Existent --
   ----------------------

   procedure Gen_Non_Existent
     (CU          : in out Compilation_Unit;
      Node        : Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin

      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));

      Add_With (CU, "CORBA.Object.Helper");
      Add_With (CU, "PolyORB.CORBA_P.IR_Hooks");

      --  The correct operation name is _non_existent; however, for
      --  compatibility with legacy implementations of the GIOP 1.0 and 1.2
      --  standards, the alternative name _not_existent is also supported.

      NL (CU);
      PL (CU, "elsif Operation = ""_non_existent""");
      PL (CU, "  or else Operation = ""_not_existent""");
      PL (CU, "then");
      II (CU);

      NL (CU);
      PL (CU, "CORBA.ServerRequest.Arguments (Request, " & T_Arg_List & ");");
      NL (CU);
      PL (CU, "CORBA.ServerRequest.Set_Result");
      PL (CU, "  (Request,");
      PL (CU, "   CORBA.To_Any (CORBA.Boolean'(False)));");
      DI (CU);
   end Gen_Non_Existent;

   -----------------------
   -- Gen_Get_Interface --
   -----------------------

   procedure Gen_Get_Interface
     (CU          : in out Compilation_Unit;
      Node        : Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));

      Add_With (CU, "CORBA.Object.Helper");
      Add_With (CU, "PolyORB.CORBA_P.IR_Hooks");

      NL (CU);
      PL (CU, "elsif Operation = ""_interface"" then");
      II (CU);

      NL (CU);
      PL (CU, "CORBA.ServerRequest.Arguments (Request, " & T_Arg_List & ");");
      NL (CU);
      PL (CU, "CORBA.ServerRequest.Set_Result");
      PL (CU, "  (Request,");
      PL (CU, "   CORBA.Object.Helper.To_Any");
      PL (CU, "   (CORBA.Object.Ref");
      PL (CU, "    (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition");
      PL (CU, "     (CORBA.To_CORBA_String (Repository_Id)))));");
      DI (CU);
   end Gen_Get_Interface;

   -----------------------------
   -- Gen_Get_Domain_Managers --
   -----------------------------

   procedure Gen_Get_Domain_Managers
     (CU          : in out Compilation_Unit;
      Node        : Node_Id)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));

      Add_With (CU, "PolyORB.CORBA_P.Domain_Management");

      NL (CU);
      PL (CU, "elsif Operation = ""_domain_managers"" then");
      II (CU);

      NL (CU);
      PL (CU, "CORBA.ServerRequest.Arguments (Request, " & T_Arg_List & ");");
      NL (CU);
      PL (CU, "CORBA.ServerRequest.Set_Result");
      PL (CU, "  (Request,");
      PL (CU, "   PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers");
      PL (CU, "   (Self));");
      DI (CU);
   end Gen_Get_Domain_Managers;

   ----------------------------
   --  Gen_Body_Common_Start --
   ----------------------------

   procedure Gen_Body_Common_Start
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));
      Add_With (CU, "PortableServer");
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      --  CORBA.To_CORBA_String is used in skel elab

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
      if Is_Delegate then
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

      Decls_Div := Current_Diversion (CU);
      Divert (CU, Operation_Body);
      NL (CU);
      Add_With (CU, "CORBA.ServerRequest");
      Add_With (CU, "PolyORB.Std");
      PL (CU, "procedure Invoke");
      PL (CU, "  (Self : PortableServer.Servant;");
      II (CU);
      PL (CU, "Request : CORBA.ServerRequest.Object_Ptr)");
      DI (CU);
      PL (CU, "is");
      II (CU);
      PL (CU, "Operation : constant PolyORB.Std.String");
      PL (CU, "   := CORBA.To_Standard_String");
      PL (CU, "        (CORBA.ServerRequest.Operation");
      PL (CU, "         (Request.all));");

      Add_With (CU, "CORBA.NVList");
      PL (CU, T_Arg_List & " : CORBA.NVList.Ref;");

      DI (CU);
      PL (CU, "begin");
      II (CU);

      Add_With (CU, "CORBA.ORB");
      PL (CU, "CORBA.ORB.Create_List (0, " & T_Arg_List & ");");

   end Gen_Body_Common_Start;

   -------------------------
   -- Gen_Body_Common_End --
   -------------------------

   procedure Gen_Body_Common_End
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean)
   is
      NK     : constant Node_Kind := Kind (Node);
      It     : Node_Iterator;
      P_Node : Node_Id;
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));

      NL (CU);
      PL (CU, "else");
      II (CU);
      PL (CU, "CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);");
      DI (CU);
      PL (CU, "end if;");
      DI (CU);
      PL (CU, "exception");
      II (CU);
      PL (CU, "when E : others =>");
      II (CU);
      PL (CU, "CORBA.ServerRequest.Set_Exception");
      PL (CU, "  (Request,");
      II (CU);
      PL (CU, "CORBA.Internals.To_CORBA_Any");
      PL (CU, "(PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E)));");
      DI (CU);
      Add_With (CU, "PolyORB.QoS.Exception_Informations");
      PL (CU, "PolyORB.QoS.Exception_Informations.Set_Exception_Information");
      PL (CU, "  (Request, E);");
      DI (CU);
      DI (CU);
      PL (CU, "end Invoke;");
      Divert (CU, Decls_Div);
      Undivert (CU, Operation_Body);

      if not Is_Delegate then
         Init (It, Parents (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, P_Node);
            Add_With (CU,
                      Ada_Full_Name (P_Node)
                      & Skel.Suffix (Is_Delegate => False),
                      No_Warnings => True);
         end loop;
      end if;

      Divert (CU, Deferred_Initialization);

      PL (CU, "PortableServer.Internals.Register_Skeleton");
      Put (CU, "  (CORBA.To_CORBA_String (");
      Put (CU, Ada_Full_Name (Node));
      PL (CU, "." & Repository_Id_Name (Node) &"),");
      if not Is_Delegate then
         PL (CU, "   Servant_Is_A'Access,");
         PL (CU, "   Is_A'Access,");
         PL (CU, "   Invoke'Access);");
      else
         PL (CU, "   Servant_Is_A'Unrestricted_Access,");
         PL (CU, "   Is_A'Access,");
         PL (CU, "   Invoke'Unrestricted_Access);");
      end if;

   end Gen_Body_Common_End;

   ------------
   -- Suffix --
   ------------

   function Suffix (Is_Delegate : Boolean) return String is
   begin
      if Is_Delegate then
         return ".Delegate";
      else
         return ".Skel";
      end if;
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
               O_Type     : constant Node_Id := Operation_Type (Node);
               Org_O_Type : constant Node_Id := Original_Operation_Type (Node);

               O_Name     : constant String := Ada_Operation_Name (Node);

               Response_Expected : constant Boolean := not Is_Oneway (Node);
               Is_Function : constant Boolean := Kind (O_Type) /= K_Void;

               Is_Class_Wide : constant Boolean :=
                                 Is_Function
                                   and then Kind (O_Type) = K_Scoped_Name
                                   and then S_Type (O_Type)
                                     = Original_Parent_Scope (Node);
               --  For an operation that returns a reference to its own
               --  interface type, the return type is classwide, so we need to
               --  convert it to the corresponding root type in the assignment
               --  to Result.

               Raise_Something : constant Boolean :=
                                   not (Raises (Node) = Nil_List);

               Has_Out_Args : Boolean;
               --  True when there are arguments of mode out or in out

               Max_Len : Integer := T_Result_Name'Length;
               Arg_Seen : Boolean;

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
                           if Max_Len < (L + T_Arg_Any'Length) then
                              Max_Len := L + T_Arg_Any'Length;
                           end if;
                        end;
                     end if;
                  end loop;
               end;

               NL (CU);
               PL (CU,
                   "elsif Operation = """
                   & Idl_Operation_Id (Node)
                   & """ then");
               II (CU);

               Arg_Seen := False;
               declare
                  It       : Node_Iterator;
                  P_Node   : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     if not Arg_Seen then
                        Add_With (CU, "PolyORB.Any");

                        NL (CU);
                        PL (CU, "declare");
                        II (CU);
                     end if;

                     Get_Next_Node (It, P_Node);

                     if not Is_Returns (P_Node) then
                        declare
                           Arg_Name : constant String :=
                                        Ada_Name (Declarator (P_Node));
                           P_Typ : constant Node_Id := Param_Type (P_Node);

                        begin
                           PL (CU, Justify (T_Argument & Arg_Name, Max_Len)
                             & " : " & Ada_Type_Name (Param_Type (P_Node))
                               & ";");
                           --  Kill warning about uninitialized variable (it is
                           --  accessed only through the wrapper below).

                           PL (CU, "pragma Warnings (Off, "
                               & T_Argument & Arg_Name & ");");

                           PL (CU, Justify (T_Arg_CC & Arg_Name, Max_Len)
                               & " : aliased PolyORB.Any.Content'Class"
                               & " :=");
                           II (CU);
                           Helper.Gen_Wrap_Call (CU, P_Typ,
                                                 T_Argument & Arg_Name);
                           DI (CU);
                           PL (CU, ";");

                           Divert (CU, Decls_Div);
                           if not Arg_Seen then
                              NL (CU);
                              Arg_Seen := True;
                           end if;

                           PL (CU,
                             Justify (O_Name & T_Arg_Name & Arg_Name,
                               Max_Len)
                             & " : constant CORBA.Identifier :=");
                           PL (CU, "  CORBA.To_CORBA_String ("""
                               & Arg_Name & """);");
                           Divert (CU, Operation_Body);

                           Add_With (CU, TC_Unit (P_Typ));

                           PL (CU, Justify (T_Arg_Any & Arg_Name, Max_Len)
                               & " : CORBA.Any := "
                               & "CORBA.Internals.Get_Wrapper_Any ("
                               & Ada_Full_TC_Name (P_Typ) & ", "
                               & T_Arg_CC & Arg_Name & "'Unchecked_Access);");

                           NL (CU);
                        end;
                     end if;
                  end loop;
               end;

               if Kind (Org_O_Type) /= K_Void then
                  if not Arg_Seen then
                     Add_With (CU, "PolyORB.Any");

                     NL (CU);
                     PL (CU, "declare");
                     II (CU);
                     Arg_Seen := True;
                  end if;

                  Add_With_Entity (CU, Org_O_Type);
                  PL (CU, Justify (T_Result, Max_Len)
                      & " : " & Ada_Type_Name (Org_O_Type) & ";");

                  --  Kill warning about unreferenced variable (it is accessed
                  --  only through the wrapper below).

                  PL (CU, "pragma Warnings (Off, " & T_Result & ");");

                  PL (CU, Justify (T_Arg_CC & T_Result, Max_Len)
                      & " : aliased PolyORB.Any.Content'Class"
                      & " :=");
                  II (CU);
                  Helper.Gen_Wrap_Call (CU, Org_O_Type, T_Result);
                  DI (CU);
                  PL (CU, ";");

                  Add_With (CU, TC_Unit (Org_O_Type));
                  PL (CU, Justify (T_Arg_Any & T_Result, Max_Len)
                      & " : CORBA.Any := CORBA.Internals.Get_Wrapper_Any ("
                      & Ada_Full_TC_Name (Org_O_Type)
                      & ", " & T_Arg_CC & T_Result & "'Unchecked_Access);");
               end if;

               if Arg_Seen then
                  DI (CU);
               end if;

               PL (CU, "begin");
               II (CU);

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
                           PL (CU, O_Name & T_Arg_Name & Arg_Name & ",");
                           PL (CU, T_Arg_Any & Arg_Name & ",");

                           case Mode (P_Node) is
                              when Mode_In =>
                                 PL (CU, "CORBA.ARG_IN);");

                              when Mode_Inout =>
                                 PL (CU, "CORBA.ARG_INOUT);");
                                 Has_Out_Args := True;

                              when Mode_Out =>
                                 PL (CU, "CORBA.ARG_OUT);");
                                 Has_Out_Args := True;

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

               if Is_Function then
                  Put (CU, T_Result & " := ");
                  if Is_Class_Wide then
                     Put (CU, Ada_Type_Name (O_Type) & " (");
                  end if;
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

               II (CU);
               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     PL (CU, ",");
                     if Is_Returns (P_Node) then
                        Put (CU, T_Result);
                     else
                        Put (CU, T_Argument & Ada_Name (Declarator (P_Node)));
                     end if;
                  end loop;
               end;
               Put (CU, ")");
               if Is_Class_Wide then
                  Put (CU, ")");
               end if;
               PL (CU, ";");
               DI (CU);

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

                           PL (CU, "when E : "
                               & Ada_Full_Name (E_Node) & " =>");
                           II (CU);
                           PL (CU, "declare");
                           II (CU);
                           PL (CU, "Members : " & Ada_Full_Name (E_Node)
                               & "_Members;");
                           DI (CU);
                           PL (CU, "begin");
                           II (CU);
                           PL (CU, Parent_Scope_Name (E_Node)
                               & ".Get_Members (E, Members);");
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

               DI (CU);
               PL (CU, "end;");

               if Response_Expected then
                  if Kind (Original_Operation_Type (Node)) /= K_Void then
                     PL (CU, "CORBA.ServerRequest.Set_Result");
                     PL (CU, "  (Request, " & T_Arg_Any & T_Result & ");");
                  end if;

                  --  The Any's for out or inout arguments must remain valid
                  --  after the skel is exited, for the marshalling of the
                  --  answer: copy them now.

                  if Has_Out_Args then
                     PL (CU, "CORBA.NVList.Internals.Clone_Out_Args ("
                         & T_Arg_List & ");");
                  end if;
               end if;

               PL (CU, "return;");
               DI (CU);
               PL (CU, "end;");
               DI (CU);
            end;

         when others =>
            null;

      end case;

   end Gen_Invoke;

   -------------------
   -- Gen_Node_Spec --
   -------------------

   procedure Gen_Node_Spec
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean)
   is
      pragma Unreferenced (CU, Node, Is_Delegate);
   begin
      null;
   end Gen_Node_Spec;

end Ada_Be.Idl2Ada.Skel;
