------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . E V E N T S                          --
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

package body Broca.Events is

   procedure Process (E : in out Null_Event) is
   begin
      null;
   end;
   
   procedure Lock (M : access Event_Manager);
   --  Get exclusive access to M's internal structure.
   
   procedure Unlock (M : access Event_Manager);
   --  Release exclusive access to M's internal structure.

   procedure Lock (M : access Event_Manager) is
   begin
      --  XXX Not implemented yet.
      null;
   end Lock;
   
   procedure Unlock (M : access Event_Manager) is
   begin
      --  XXX Not implemented yet.
      null;
   end Unlock;

   procedure Link_Source
     (M  : access Event_Manager;
      AS : Asynchronous_Event_Source_Ptr) is
   begin
      Lock (M);
      Unlink_Source (M);
      M.Asynch_Source := AS;
      Unlock (M);
   end Link_Source;
   
   procedure Unlink_Source
     (M : access Event_Manager) is
   begin
      Lock (M);
      Wakeup (M.Asynch_Source.all);
      M.Asynch_Source := null;
      Unlock (M);
   end Unlink_Source;
   
end Broca.Events;
