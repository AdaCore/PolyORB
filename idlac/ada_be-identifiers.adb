------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   A D A _ B E . I D E N T I F I E R S                    --
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

with Utils; use Utils;

with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

with Ada_Be.Debug;

package body Ada_Be.Identifiers is

   --------------
   --   Debug  --
   --------------

   Flag : constant Natural
     := Ada_Be.Debug.Is_Active ("ada_be.identifiers");
   procedure O is new Ada_Be.Debug.Output (Flag);

   function Ada_Full_Name
     (Node : Node_Id)
     return String is
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
               P_Node : constant Node_Id
                 := Parent_Scope (Node);
            begin
               pragma Assert (Kind (P_Node) /= K_Repository);

               if Kind (P_Node) = K_Ben_Idl_File
                 and then Is_Gen_Scope (Node) then
                  return Ada_Name (Node);
               else
                  return Ada_Full_Name (P_Node)
                    & "." & Ada_Name (Node);
               end if;
            end;
      end case;
   end Ada_Full_Name;

   ----------------
   --  Ada_Name  --
   ----------------
   function Ada_Name
     (Node : Node_Id)
     return String
   is
      Result : String
        := Name (Node);
      First : Integer := Result'First;

   begin
      while First <= Result'Last
        and then Result (First) = '_' loop
         First := First + 1;
      end loop;

      for I in First .. Result'Last loop
         if Result (I) = '_'
           and then I < Result'Last
           and then Result (I + 1) = '_' then
            Result (I + 1) := 'U';
         end if;
      end loop;

      if False
        or else Kind (Node) = K_Forward_Interface
        or else Kind (Node) = K_Forward_ValueType
      then
         return Result (First .. Result'Last) & "_Forward";
      else
         return Result (First .. Result'Last);
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
