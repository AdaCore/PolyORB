------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with System.Storage_Elements;

package System.Secondary_Stack is

   package SSE renames System.Storage_Elements;

   procedure SS_Init (Stk : out System.Address; Size : Natural);
   --  Initialize the secondary stack with a main stack of the given Size.
   --  All further allocations which do not overflow the main stack will not
   --  generate dynamic (de)allocation calls. If the main Stack overflows
   --  a new chuck of at least the same size will be allocated and linked
   --  to the previous chunk.
   --
   --  Note: the reason that Stk is passed is that SS_Init is called before
   --  the proper interface is established to obtain the address of the
   --  stack using System.Task_Specific_Data.Get_Sec_Stack_Addr.

   procedure SS_Allocate
     (Address      :    out System.Address;
      Storage_Size : in     SSE.Storage_Count);
   --  Allocate enough space for a 'Storage_Size' bytes object with Maximum
   --  alignment. The address of the allocated space is returned in 'Address'

   procedure SS_Free (Stk : System.Address);
   --  Release the memory allocated for the Secondary Stack. That is to say,
   --  all the allocated chuncks.

   type Mark_Id is private;
   --  Type used to mark the stack.

   function SS_Mark return Mark_Id;
   --  Return the Mark corresponding to the current state of the stack

   procedure SS_Release (M : Mark_Id);
   --  Restore the state of the stack corresponding to the mark M. If an
   --  additional chunk have been allocated, it will never be freed during a

   generic
      with procedure Put_Line (S : String);
   procedure SS_Info;
   --  Debugging procedure used to print out secondary Stack allocation
   --  information. This procedure is generic in order to avoid a direct
   --  dependance on a particular IO package.

private

   SS_Pool : Integer;
   --  Unused entity that is just present to ease the sharing of the pool
   --  mechanism for specific allocation/deallocation in the compiler

   type Mark_Id is new SSE.Integer_Address;

end System.Secondary_Stack;
