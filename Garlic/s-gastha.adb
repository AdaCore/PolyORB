------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--       S Y S T E M . G A R L I C . S T O R A G E _ H A N D L I N G        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with Interfaces.C;

package body System.Garlic.Storage_Handling is

   package IC  renames Interfaces.C;
   package SSE renames System.Storage_Elements;
   package SSP renames System.Storage_Pools;

   use type SSE.Storage_Count;
   use type IC.int;

   function malloc (Size : IC.int) return Address;
   pragma Import (C, malloc, "__gnat_malloc");

   procedure free (P : in Address);
   pragma Import (C, free, "__gnat_free");

   Initialized : Boolean := False;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in out Garlic_Storage_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : in SSE.Storage_Count;
      Alignment                : in SSE.Storage_Count) is
   begin
      pragma Assert (Initialized);

      --  Garlic_Storage_Pool is a controlled type. It is initialized
      --  before invoking the Initialize procedure of this
      --  unit. Soft_Links may not be registered when the storage pool
      --  is initialized. We have to initialize the mutexes of the
      --  storage pool outside from its initialization subprogram.

      if not Pool.Ready then
         Soft_Links.Create (Pool.Mutex);
         Pool.Ready := True;
      end if;

      if Size_In_Storage_Elements > Static_Object_Size
        or else Pool.N_Objects = Max_Objects
      then
         Storage_Address := malloc (IC.int (Size_In_Storage_Elements));
         if Storage_Address = Null_Address then
            raise Storage_Error;
         end if;
         return;
      end if;

      Soft_Links.Enter (Pool.Mutex);
      for I in 1 .. Max_Objects loop
         if not Pool.Used (I) then
            Pool.Used (I)   := True;
            Storage_Address := Pool.Addresses (I);
            Pool.N_Objects := Pool.N_Objects + 1;
            Soft_Links.Leave (Pool.Mutex);
            return;
         end if;
      end loop;
      Soft_Links.Leave (Pool.Mutex);
      Storage_Address := malloc (IC.int (Size_In_Storage_Elements));
      if Storage_Address = Null_Address then
         raise Storage_Error;
      end if;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in out Garlic_Storage_Pool;
      Storage_Address          : in Address;
      Size_In_Storage_Elements : in SSE.Storage_Count;
      Alignment                : in SSE.Storage_Count)
   is
   begin

      --  Garlic_Storage_Pool is a controlled type. It is initialized
      --  before invoking the Initialize procedure of this
      --  unit. Soft_Links may not be registered when the storage pool
      --  is initialized. We have to initialize the mutexes of the
      --  storage pool outside from its initialization subprogram.

      if not Pool.Ready then
         Soft_Links.Create (Pool.Mutex);
         Pool.Ready := True;
      end if;

      --  Shortcut: if the object is too big to have been allocated from
      --  the pool, that means that it has been allocated on the heap.

      if Size_In_Storage_Elements > Static_Object_Size
        or else Pool.N_Objects = 0
      then
         free (Storage_Address);
         return;
      end if;

      for I in 1 .. Max_Objects loop
         if Pool.Used (I) and then Pool.Addresses (I) = Storage_Address then
            Pool.Used (I) := False;
            Pool.N_Objects := Pool.N_Objects - 1;
            return;
         end if;
      end loop;
      free (Storage_Address);
   end Deallocate;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Pool : in out Garlic_Storage_Pool) is
   begin
      for I in 1 .. Max_Objects loop
         free (Pool.Addresses (I));
      end loop;
      if Pool.Ready then
         Soft_Links.Destroy (Pool.Mutex);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      --  Garlic_Storage_Pool is a controlled type. It is initialized
      --  before invoking the Initialize procedure of this
      --  unit. Soft_Links may not be registered when the storage pool
      --  is initialized. We have to initialize the mutexes of the
      --  storage pool outside from its initialization subprogram.

      --  When this boolean is set to true, we indicate that the soft
      --  links are available. Therefore, the storage pool can safely
      --  create the mutexes needed.

      Initialized := True;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Pool : in out Garlic_Storage_Pool) is
   begin
      Pool.N_Objects := 0;
      Pool.Used      := (others => False);
      for I in 1 .. Max_Objects loop
         Pool.Addresses (I) := malloc (IC.int (Static_Object_Size));
      end loop;
      if Initialized then
         Soft_Links.Create (Pool.Mutex);
         Pool.Ready := True;
      else
         Pool.Ready := False;
      end if;
   end Initialize;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Pool : Garlic_Storage_Pool)
     return SSE.Storage_Count is
   begin
      return SSE.Storage_Count'Last;
   end Storage_Size;

end System.Garlic.Storage_Handling;
