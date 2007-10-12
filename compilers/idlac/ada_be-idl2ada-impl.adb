------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  A D A _ B E . I D L 2 A D A . I M P L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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
               Gen_Operation_Profile (CU, Node, "access Object");
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
               Gen_Operation_Profile (CU, Node, "access Object");
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
