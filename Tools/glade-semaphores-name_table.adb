------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--          G L A D E . S E M A P H O R E S . N A M E _ T A B L E           --
--                                                                          --
--                                 B o d y                                  --
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

with GLADE.Name_Table;         use GLADE.Name_Table;
with GLADE.Protected_Objects;  use GLADE.Protected_Objects;
with GNAT.Table;

package body GLADE.Semaphores.Name_Table is

   type Owner_Type is record
      Owner : Semaphore_Access;
      Index : Positive;
   end record;

   package Owner_Table is new GNAT.Table
     (Table_Component_Type => Owner_Type,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100);

   Partition_Mutex : Mutex_Type;

   ------------------------
   -- Get_Probable_Owner --
   ------------------------

   procedure Get_Probable_Owner (Semaphore          : in Semaphore_Access;
                                 Name               : in String;
                                 Probable_Owner     : out Semaphore_Access;
                                 Index              : out Positive)
   is
      Current_Index  : Name_Id;
      Current_Info   : Integer;
      New_Slot       : Boolean;
   begin
      --  Do not do concurrent accesses to the tables

      Partition_Mutex.Enter;

      --  Look for the index and choose a new slot if the name has not been
      --  seen yet

      Current_Index := Get (Name & "'semaphore");
      Current_Info  := Get_Info (Current_Index);
      if Current_Info = 0 then
         Current_Info := Owner_Table.Allocate;
         Set_Info (Current_Index, Current_Info);
         New_Slot := True;
      else
         New_Slot := False;
      end if;

      --  If the slot has just been created, then the probable owner is
      --  the sender itself.

      if New_Slot then
         Owner_Table.Table (Current_Info) .Owner := Semaphore;
         Owner_Table.Table (Current_Info) .Index := 1;
      else
         Owner_Table.Table (Current_Info) .Index :=
           Owner_Table.Table (Current_Info) .Index + 1;
      end if;

      --  Unlock the tables then return
      Partition_Mutex.Leave;
      Probable_Owner := Owner_Table.Table (Current_Info) .Owner;
      Index          := Owner_Table.Table (Current_Info) .Index;

   end Get_Probable_Owner;

end GLADE.Semaphores.Name_Table;
