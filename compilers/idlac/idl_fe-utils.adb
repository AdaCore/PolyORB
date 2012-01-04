------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . U T I L S                          --
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

with Idlac_Utils; use Idlac_Utils;

package body Idl_Fe.Utils is

   procedure Add_Identifier_With_Renaming
     (Node       : Node_Id;
      Identifier : String;
      Scope      : Node_Id := No_Node;
      Is_Inheritable : Boolean := True)
   is
      Suffix : Integer := 1;
   begin
      if not Add_Identifier (Node, Identifier, Scope, Is_Inheritable) then
         while not Add_Identifier
           (Node, Identifier & "_" & Img (Suffix), Scope, Is_Inheritable)
         loop
            Suffix := Suffix + 1;
         end loop;
      end if;
   end Add_Identifier_With_Renaming;

end Idl_Fe.Utils;
