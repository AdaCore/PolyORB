------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--       S Y S T E M . G A R L I C . S T O R A G E _ H A N D L I N G        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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

with System.Garlic.Utils;
with System.Storage_Elements;
with System.Storage_Pools;

generic
   Max_Objects        : Positive;
   Static_Object_Size : System.Storage_Elements.Storage_Count;
package System.Garlic.Storage_Handling is

   --  This package needs comments ???

   type Garlic_Storage_Pool is
     new System.Storage_Pools.Root_Storage_Pool with private;

   procedure Initialize (Pool : in out Garlic_Storage_Pool);
   procedure Finalize   (Pool : in out Garlic_Storage_Pool);

   procedure Allocate
     (Pool                     : in out Garlic_Storage_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count);

   procedure Deallocate
     (Pool                     : in out Garlic_Storage_Pool;
      Storage_Address          : in Address;
      Size_In_Storage_Elements : in System.Storage_Elements.Storage_Count;
      Alignment                : in System.Storage_Elements.Storage_Count);

   function Storage_Size (Pool : Garlic_Storage_Pool)
      return System.Storage_Elements.Storage_Count;

private

   type Array_Of_Addresses is array (1 .. Max_Objects) of Address;
   type Array_Of_Boolean   is array (1 .. Max_Objects) of Boolean;

   type Garlic_Storage_Pool is
     new System.Storage_Pools.Root_Storage_Pool with record
        N_Objects : Natural;
        pragma Atomic (N_Objects);
        Addresses : Array_Of_Addresses;
        Used      : Array_Of_Boolean;
        Mutex     : Utils.Mutex_Type;
     end record;

end System.Garlic.Storage_Handling;
