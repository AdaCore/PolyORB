------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A D A _ B E . I D L 2 A D A . S K E L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Idl_Fe.Types;          use Idl_Fe.Types;
with Idl_Fe.Tree;           use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;
with Ada_Be.Idl2Ada.Impl;
with Ada_Be.Idl2Ada.Helper;
with Ada_Be.Idl2Ada.Value_Skel;
with Ada_Be.Mappings.CORBA; use Ada_Be.Mappings.CORBA;
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
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean);
   --  Generate server-side support for the Is_A
   --  operation.

   procedure Gen_Get_Interface
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean);
   --  Generate server-side support for the Get_Interface operation

   procedure Gen_Get_Domain_Managers
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean);
   --  Generate server-side support for the Get_Domain_Managers operation

   procedure Gen_Body_Common_Start
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean);
   --  generates code for skel_body that is common
   --  for interfaces and valuetypes supporting interfaces
   --  at the beginning of the package.

   procedure Gen_Invoke
     (CU   : in out Compilation_Unit;
      Node : Node_Id);
   --  Generate a static dispatcher fragment for operation Node.

   -------------------
   -- Gen_Node_Body --
   -------------------

   procedure Gen_Node_Body
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      case NK is

         when K_ValueType =>
            pragma Assert (not Is_Delegate);
            if Supports (Node) /= Nil_List then
               Gen_Body_Common_Start (CU, Node, Is_Delegate);
               Gen_Is_A (CU, Node, Is_Delegate);
               Gen_Invoke (CU, Node);

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
                               Elab_Control => Elaborate,
                               No_Warnings  => True);
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

--                if not Is_Delegate then
--                   Add_With (CU, Ada_Full_Name (Node) & Impl.Suffix);
--                   NL (CU);
--                   PL (CU,
--                       "type Object_Ptr is access all "
--                       & Ada_Full_Name (Node)
--                       & Impl.Suffix
--                       & ".Object'Class;");
--                end if;

               Gen_Body_Common_Start (CU, Node, Is_Delegate);
               Gen_Is_A (CU, Node, Is_Delegate);
               Gen_Get_Interface (CU, Node, Is_Delegate);
               Gen_Get_Domain_Managers (CU, Node, Is_Delegate);
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
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface)
                     or else (NK = K_ValueType));

      PL (CU, "if Operation = ""_is_a"" then");
      II (CU);

      PL (CU, "declare");
      II (CU);

      PL (CU, "Type_Id : CORBA.String;");
      PL (CU, T_Arg_Name & "Type_Id : constant CORBA.Identifier");
      PL (CU, ":= CORBA.To_CORBA_String (""Type_Id"");");
      PL (CU, T_Argument & "Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);");
      PL (CU, "");
      PL (CU, T_Result & " : CORBA.Boolean;");
      DI (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "CORBA.NVList.Add_Item");
      PL (CU, "(" & T_Arg_List & ",");
      PL (CU, T_Arg_Name & "Type_Id,");
      PL (CU, T_Argument & "Type_Id,");
      PL (CU, "CORBA.ARG_IN);");
      NL (CU);

      PL (CU, "CORBA.ServerRequest.Arguments (Request, " & T_Arg_List & ");");
      NL (CU);
      PL (CU, "begin");
      II (CU);
      PL (CU, "--  Convert arguments from their Any");
      NL (CU);
      PL (CU, "Type_Id :=");
      PL (CU, "  CORBA.From_Any (" & T_Argument & "Type_Id);");
      NL (CU);
      PL (CU, "--  Call implementation");
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
      PL (CU, "return;");
      DI (CU);
      PL (CU, "end;");
      DI (CU);
   end Gen_Is_A;

   -----------------------
   -- Gen_Get_Interface --
   -----------------------

   procedure Gen_Get_Interface
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
   is
      pragma Unreferenced (Is_Delegate);

      NK : constant Node_Kind := Kind (Node);

   begin
      pragma Assert ((NK = K_Interface)
                     or else (NK = K_ValueType));

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
      NL (CU);
      PL (CU, "return;");
      DI (CU);
   end Gen_Get_Interface;

   -----------------------------
   -- Gen_Get_Domain_Managers --
   -----------------------------

   procedure Gen_Get_Domain_Managers
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
   is
      pragma Unreferenced (Is_Delegate);

      NK : constant Node_Kind := Kind (Node);

   begin
      pragma Assert ((NK = K_Interface)
                     or else (NK = K_ValueType));

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
      NL (CU);
      PL (CU, "return;");
      DI (CU);
   end Gen_Get_Domain_Managers;

   ----------------------------
   --  Gen_Body_Common_Start --
   ----------------------------

   procedure Gen_Body_Common_Start
     (CU          : in out Compilation_Unit;
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
   is
      NK : constant Node_Kind := Kind (Node);
   begin
      pragma Assert ((NK = K_Interface) or else (NK = K_ValueType));
      Add_With (CU, "PortableServer");
      Add_With (CU, "CORBA", Elab_Control => Elaborate_All);
      --  CORBA.To_CORBA_String is used in skel elab.

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
      Node        : in     Node_Id;
      Is_Delegate : in     Boolean)
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
      PL (CU, "begin");
      II (CU);
      PL (CU, "CORBA.ServerRequest.Set_Exception");
      PL (CU, "  (Request,");
      II (CU);
      PL (CU, "CORBA.Internals.To_CORBA_Any "
          & "(PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E)));");
      DI (CU);
      PL (CU, "return;");
      DI (CU);
      PL (CU, "end;");
      DI (CU);
      DI (CU);
      PL (CU, "end Invoke;");

      if not Is_Delegate then
         Init (It, Parents (Node));
         while not Is_End (It) loop
            Get_Next_Node (It, P_Node);
            Add_With (CU,
                      Ada_Full_Name (P_Node)
                      & Skel.Suffix (Is_Delegate => False),
                      Elab_Control => Elaborate,
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

   function Suffix (Is_Delegate : in Boolean) return String is
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
                   "elsif Operation = """
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

                        P_Typ : constant Node_Id := Param_Type (P_Node);
                        Helper_Name : constant String
                          := Ada_Helper_Unit_Name (Mapping, P_Typ);

                     begin

                        PL (CU, Justify (Arg_Name, Max_Len) & " : "
                            & Ada_Type_Name (Param_Type (P_Node))  & ";");

                        if not Is_Returns (P_Node) then
                           PL (CU, Justify (T_Arg_Name & Arg_Name, Max_Len)
                               & " : constant CORBA.Identifier :=");
                           PL (CU, "  CORBA.To_CORBA_String ("""
                               & Arg_Name & """);");

                           Add_With (CU, Helper_Name);

                           PL (CU, Justify (T_Argument & Arg_Name, Max_Len)
                               & " : CORBA.Any := CORBA.Get_Empty_Any");
                           PL (CU, "  (" & Ada_Full_TC_Name (P_Typ) & ");");

                           NL (CU);
                        end if;
                     end;
                  end loop;
               end;

               if Is_Function then
                  PL (CU, Justify (T_Result, Max_Len)
                      & " : " & Ada_Type_Name (O_Type) & ";");
               end if;

               DI (CU);
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
                           PL (CU, T_Arg_Name & Arg_Name & ",");
                           PL (CU, T_Argument & Arg_Name & ",");

                           case Mode (P_Node) is
                              when Mode_In =>
                                 PL (CU, "CORBA.ARG_IN);");
                              when Mode_Inout =>
                                 PL (CU, "CORBA.ARG_INOUT);");
                              when Mode_Out =>
                                 PL (CU, "CORBA.ARG_OUT);");
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
                              Add_With (CU, Helper_Name);
                              PL (CU, Arg_Name & " :=");
                              Put (CU, "  " & Helper_Name & ".From_Any ("
                                & T_Argument & Arg_Name & ")");
                              PL (CU, ";");
                           end if;
                        end if;
                     end;

                  end loop;
               end;

               NL (CU);
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

               II (CU);
               declare
                  It   : Node_Iterator;
                  P_Node : Node_Id;
               begin
                  Init (It, Parameters (Node));
                  while not Is_End (It) loop
                     Get_Next_Node (It, P_Node);
                     Put (CU, "," & ASCII.LF & Ada_Name (Declarator (P_Node)));
                  end loop;
               end;
               PL (CU, ");");
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
                                 Add_With (CU, "CORBA");
                                 PL (CU, "CORBA.Internals.Move_Any_Value");
                                 PL (CU, "  (" & T_Argument & Arg_Name & ",");
                                 II (CU);
                                 PL (CU, Helper_Name & ".To_Any");
                                 Put (CU, "  (" & Arg_Name & "));");
                                 DI (CU);
                                 NL (CU);
                              end;
                           end if;

                        end if;

                     end loop;
                  end;

                  if Kind (Original_Operation_Type (Node)) /= K_Void then
                     NL (CU);
                     PL (CU, "-- Set result");
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
                           PL (CU, Prefix & ".To_Any (" & T_Result & "));");
                           DI (CU);
                        else
                           PL (CU, "CORBA.ServerRequest.Set_Result");
                           PL (CU, "  (Request, ");
                           II (CU);
                           PL (CU, Prefix & ".To_Any (Returns));");
                           DI (CU);
                        end if;
                     end;
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
