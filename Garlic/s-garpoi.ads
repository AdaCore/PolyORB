------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--              S Y S T E M . G A R L I C . P O I N T E R S                 --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;

generic
   type Index is (<>);
   type Element is private;
   type Element_Array is array (Index range <>) of aliased Element;
   Default_Terminator : Element;

package System.Garlic.Pointers is

   subtype ptrdiff_t is Interfaces.C.ptrdiff_t;

   type Pointer is access all Element;

   function Value
     (Ref        : in Pointer;
      Terminator : in Element := Default_Terminator)
      return       Element_Array;

   function Value
     (Ref    : in Pointer;
      Length : in ptrdiff_t)
      return   Element_Array;

   Pointer_Error : exception;

   --------------------------------
   -- C-style Pointer Arithmetic --
   --------------------------------

   function "+" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer;
   function "+" (Left : in ptrdiff_t; Right : in Pointer)   return Pointer;
   function "-" (Left : in Pointer;   Right : in ptrdiff_t) return Pointer;
   function "-" (Left : in Pointer;   Right : in Pointer)   return ptrdiff_t;

   procedure Increment (Ref : in out Pointer);
   procedure Decrement (Ref : in out Pointer);

   pragma Convention (Intrinsic, "+");
   pragma Convention (Intrinsic, "-");
   pragma Convention (Intrinsic, Increment);
   pragma Convention (Intrinsic, Decrement);

   function Virtual_Length
     (Ref        : in Pointer;
      Terminator : in Element := Default_Terminator)
      return       ptrdiff_t;

   procedure Copy_Terminated_Array
     (Source     : in Pointer;
      Target     : in Pointer;
      Limit      : in ptrdiff_t := ptrdiff_t'Last;
      Terminator : in Element := Default_Terminator);

   procedure Copy_Array
     (Source  : in Pointer;
      Target  : in Pointer;
      Length  : in ptrdiff_t);

private
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline (Decrement);
   pragma Inline (Increment);

end System.Garlic.Pointers;
