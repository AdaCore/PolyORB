------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         I D L _ F E . U T I L S                          --
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

--  $Id$

with Utils; use Utils;

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
