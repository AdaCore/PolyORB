------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . L O C K I N G                 --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Task_Identification;  use Ada.Task_Identification;
with System.Garlic.Soft_Links;

package body System.Garlic.Locking is

   procedure Enter_Critical_Section;
   procedure Leave_Critical_Section;
   --  Procedures that will be registered through the soft-links mechanism

   protected Lock is
      entry Enter (Id : in Task_Id);
      --  Enter the critical section

      procedure Leave (Id : in Task_Id);
      --  Leave the critical section

   private
      entry Enter_Wait (Id : in Task_Id);
      --  Enter the critical section whenever it is not locked at all

      Lock_Level : Natural := 0;
      --  Number of levels of locks, 0 means unlocked

      Lock_Owner : Task_Id;
      --  Task locking the critical section
   end Lock;

   ----------------------------
   -- Enter_Critical_Section --
   ----------------------------

   procedure Enter_Critical_Section is
   begin
      Lock.Enter (Current_Task);
   end Enter_Critical_Section;

   ----------------------------
   -- Leave_Critical_Section --
   ----------------------------

   procedure Leave_Critical_Section is
   begin
      Lock.Leave (Current_Task);
   end Leave_Critical_Section;

   ----------
   -- Lock --
   ----------

   protected body Lock is

      -----------
      -- Enter --
      -----------

      entry Enter (Id : in Task_Id) when True is
      begin
         if Lock_Level > 0 and then Id = Lock_Owner then

            --  The task already has one or more locks on this object. In this
            --  case, we just need to increase the lock level.

            Lock_Level := Lock_Level + 1;
         else
            requeue Enter_Wait with abort;
         end if;
      end Enter;

      ----------------
      -- Enter_Wait --
      ----------------

      entry Enter_Wait (Id : in Task_Id) when Lock_Level = 0 is
      begin
         Lock_Level := 1;
         Lock_Owner := Id;
      end Enter_Wait;

      -----------
      -- Leave --
      -----------

      procedure Leave (Id : in Task_Id) is
      begin
         pragma Assert (Id = Lock_Owner);
         if Lock_Level > 0 then
            Lock_Level := Lock_Level - 1;
         else
            raise Program_Error;
         end if;
      end Leave;

   end Lock;

begin
   Soft_Links.Register_Enter_Critical_Section (Enter_Critical_Section'Access);
   Soft_Links.Register_Leave_Critical_Section (Leave_Critical_Section'Access);
end System.Garlic.Locking;
