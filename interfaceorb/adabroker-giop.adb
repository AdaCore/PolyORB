------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       A D A B R O K E R . G I O P                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

package body AdaBroker.GIOP is

   --------------------------------
   -- Reply_Status_Type_To_C_Int --
   --------------------------------

   function Reply_Status_Type_To_C_Int
     (Status : in Reply_Status_Type)
      return Interfaces.C.int
   is
   begin
      return Interfaces.C.int (Reply_Status_Type'Pos (Status));
   end Reply_Status_Type_To_C_Int;

   --------------------------------
   -- C_Int_To_Reply_Status_Type --
   --------------------------------

   function C_Int_To_Reply_Status_Type
     (N : in Interfaces.C.int)
      return Reply_Status_Type is
   begin
      return Reply_Status_Type'Val (Integer (N));
   end C_Int_To_Reply_Status_Type;

end AdaBroker.GIOP;
