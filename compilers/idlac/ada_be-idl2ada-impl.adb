------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A D A _ B E . I D L 2 A D A . I M P L                   --
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

with Idl_Fe.Tree;           use Idl_Fe.Tree;

with Ada_Be.Identifiers;    use Ada_Be.Identifiers;

package body Ada_Be.Idl2Ada.Impl is

   procedure Gen_Node_Spec
     (CU          : in out Compilation_Unit;
      Node        : Node_Id;
      Is_Delegate : Boolean := False)
   is
   begin

      case Kind (Node) is

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>

            if Is_Delegate then
               Gen_Operation_Profile
                 (CU, Node, "access Wrapped", Is_Delegate => True);
               PL (CU, ";");

            elsif not Is_Implicit_Inherited (Node) then
               Gen_Operation_Profile (CU, Node, "not null access Object");
               PL (CU, ";");
            end if;

         when others =>
            null;

      end case;

   end Gen_Node_Spec;

   procedure Gen_Node_Body
     (CU   : in out Compilation_Unit;
      Node : Node_Id)
   is
   begin
      case Kind (Node) is

         ----------------
         -- Operations --
         ----------------

         when K_Operation =>

            --  Implicitly inherited operation are not overridden by default

            if Is_Implicit_Inherited (Node) then
               return;
            end if;

            declare
               Is_Function : constant Boolean :=
                               Kind (Operation_Type (Node)) /= K_Void;
            begin
               NL (CU);
               Gen_Operation_Profile (CU, Node, "not null access Object");
               if Is_Function then
                  NL (CU);
                  PL (CU, "is");
                  II (CU);
                  PL (CU, "Result : "
                      & Ada_Type_Name (Operation_Type (Node)) & ";");
                  DI (CU);
               else
                  PL (CU, " is");
               end if;

               PL (CU, "begin");
               II (CU);
               NL (CU);
               PL (CU, "--  Insert implementation of " & Ada_Name (Node));
               NL (CU);
               if Is_Function then
                  PL (CU, "return Result;");
               else
                  PL (CU, "null;");
               end if;
               DI (CU);
               PL (CU, "end " & Ada_Operation_Name (Node) & ";");
            end;

         when others =>
            null;

      end case;

   end Gen_Node_Body;

end Ada_Be.Idl2Ada.Impl;
