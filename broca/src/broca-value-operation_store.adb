------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--          B R O C A . V A L U E . O P E R A T I O N _ S T O R E           --
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

with Broca.Exceptions;
with Broca.Soft_Links;

package body Broca.Value.Operation_Store is

   type Operation_Store;
   type Operation_Store_Ptr is access all Operation_Store'Class;

   type Operation_Store is tagged record
      Tag : Ada.Tags.Tag;
      Operation : Operation_Type;
      Next : Operation_Store_Ptr;
   end record;

   The_Store : Operation_Store_Ptr := null;

   ------------------------
   -- Register_Operation --
   ------------------------

   procedure Register_Operation
     (T : in Ada.Tags.Tag;
      Op : in Operation_Type) is
   begin
      Broca.Soft_Links.Enter_Critical_Section;
      The_Store := new Operation_Store'
        (Tag => T,
         Operation => Op,
         Next => The_Store);
      Broca.Soft_Links.Leave_Critical_Section;
   end Register_Operation;

   -------------------
   -- Get_Operation --
   -------------------

   function Get_Operation
     (T : in Ada.Tags.Tag)
     return Operation_Type
   is
      Temp : Operation_Store_Ptr;
   begin
      Temp := The_Store;
      Broca.Soft_Links.Enter_Critical_Section;
      while Temp /= null loop
         exit when Ada.Tags."=" (Temp.Tag, T);
         Temp := Temp.Next;
      end loop;
      if Temp = null then
         Broca.Exceptions.Raise_Internal;
      end if;
      Broca.Soft_Links.Leave_Critical_Section;
      return Temp.Operation;
   end Get_Operation;

end Broca.Value.Operation_Store;
