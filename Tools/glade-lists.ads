------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                          G L A D E . L I S T S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------
generic
   type Item is private;
package GLADE.Lists is

   type List is private;

   function Create return List;

   procedure Append (L : List; I : Item);
   --  Append item I to the end of list L

   procedure Modify (L : List; I : Item);
   --  Update current item of list L with a new value I

   procedure Remove (L : List; I : Item);
   --  Find and remove item I from list L

   procedure Head (L : List);
   --  Set current item of list L to the head of the list

   function  Next (L : List) return Boolean;
   --  Go to next item of the list and return true if successful

   procedure Read (L : List; I : out Item);
   --  Read value of current item of the list

private

   type Item_Node;
   type Item_List is access Item_Node;
   type Item_Node is record
      Data : Item;
      Next : Item_List;
   end record;

   type List_Type is record
      Head  : Item_List;
      Tail  : Item_List;
      Index : Item_List;
   end record;

   type List is access List_Type;

end GLADE.Lists;
