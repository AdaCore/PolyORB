------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A D A _ B E . I D E N T I F I E R S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

with Idlac_Utils; use Idlac_Utils;

with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Debug;
pragma Elaborate_All (Ada_Be.Debug);

package body Ada_Be.Identifiers is

   Flag : constant Natural
     := Ada_Be.Debug.Is_Active ("ada_be.identifiers");
   procedure O is new Ada_Be.Debug.Output (Flag);

   -------------------
   -- Ada_Full_Name --
   -------------------

   function Ada_Full_Name (Node : Node_Id) return String is
   begin
      pragma Debug
        (O ("Ada_Full_Name: enter (Node = " & Img (Node)
            & ", Kind = " & Img (Kind (Node)) & ")"));

      case Kind (Node) is
         when K_Scoped_Name =>
            return Ada_Full_Name (Value (Node));

         when K_Ben_Idl_File =>
            return Ada_Name (Node);

         when K_Repository =>
            raise Program_Error;

         when others =>
            declare
               P_Node    : constant Node_Id := Parent_Scope (Node);
               Node_Name : constant String  := Ada_Name (Node);
            begin
               pragma Assert (Kind (P_Node) /= K_Repository);

               if Kind (P_Node) = K_Ben_Idl_File
                 and then Is_Gen_Scope (Node)
               then
                  return Node_Name;
               else
                  return Ada_Full_Name (P_Node) & "." & Node_Name;
               end if;
            end;
      end case;
   end Ada_Full_Name;

   --------------
   -- Ada_Name --
   --------------

   function Ada_Name (Node : Node_Id) return String is
      NK : constant Node_Kind := Kind (Node);

      Result : String := Name (Node) & "U";
      --  Reserve an additional character for the case of a terminal underscore

      First : Integer := Result'First;
      Last  : Integer := Result'Last - 1;
   begin
      while First <= Last and then Result (First) = '_' loop
         First := First + 1;
      end loop;

      if NK = K_Operation
        and then Original_Node (Node) /= No_Node
        and then Kind (Original_Node (Node)) = K_Attribute
      then
         if Result (First) = 'g' then
            Result (First) := 'G';
         elsif Result (First) = 's' then
            Result (First) := 'S';
         else
            raise Program_Error;
         end if;
      end if;

      for J in First .. Last loop
         if Result (J) = '_'
           and then J < Last
           and then Result (J + 1) = '_'
         then
            Result (J + 1) := 'U';
         end if;
      end loop;

      if Result (Last) = '_' then
         Last := Last + 1;
      end if;

      if False
        or else NK = K_Forward_Interface
        or else NK = K_Forward_ValueType
      then
         return Result (First .. Last) & "_Forward";
      else
         return Result (First .. Last);
      end if;
   end Ada_Name;

   function Parent_Scope_Name
     (Node : Node_Id)
     return String is
   begin
      pragma Debug (O ("Parent_Scope_Name : enter & end"));
      pragma Debug (O ("Parent_Scope_Name : node kind is "
                       & Node_Kind'Image (Kind (Node))));
      return Ada_Full_Name (Parent_Scope (Node));
   end Parent_Scope_Name;

end Ada_Be.Identifiers;
