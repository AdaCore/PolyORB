------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--               S Y S T E M . G A R L I C . P O I N T E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
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
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with System.Garlic.Addresses;

generic
   type Designated (<>) is private;
   type Pointer is access Designated;

package System.Garlic.Pointers is

   --  This package defines two functions used to create a handle
   --  corresponding to a local pointer type and to retrieve the
   --  pointer value from the handle.

   --  Note that if the target machine has a 32 bits address space, then
   --  no table will be used at all as a simple conversion is enough.

   function Create_Handle (P : Pointer) return Addresses.Handle;
   --  Create a handle from a pointer. If the pointer has already been
   --  given to this function, the same result will be returned.

   function Retrieve_Pointer (H : Addresses.Handle) return Pointer;
   --  Return the pointer associated to the handle. The handle must have
   --  been created by a call to Create_Handle in the same instance of this
   --  generic package.

end System.Garlic.Pointers;
