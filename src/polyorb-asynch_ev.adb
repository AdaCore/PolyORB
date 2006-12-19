------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . A S Y N C H _ E V                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Abstract data type for an asynchrous event source.

with Ada.Unchecked_Deallocation;

package body PolyORB.Asynch_Ev is

   ------------
   -- AEM_Of --
   ------------

   function AEM_Of (AES : Asynch_Ev_Source) return Asynch_Ev_Monitor_Access is
   begin
      return AES.Monitor;
   end AEM_Of;

   ----------------
   -- Notepad_Of --
   ----------------

   function Notepad_Of
     (AES : Asynch_Ev_Source_Access)
     return Annotations.Notepad_Access is
   begin
      return AES.Notes'Access;
   end Notepad_Of;

   -----------------------
   -- Unregister_Source --
   -----------------------

   function Unregister_Source (AES : Asynch_Ev_Source_Access) return Boolean is
      Success : Boolean;
   begin
      pragma Assert (AES /= null and then AES.Monitor /= null);
      Unregister_Source (AES.Monitor.all, AES, Success);
      return Success;
   end Unregister_Source;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (AES : in out Asynch_Ev_Source_Access)
   is
      procedure Free is
         new Ada.Unchecked_Deallocation
        (Asynch_Ev_Source'Class, Asynch_Ev_Source_Access);
   begin
      Annotations.Destroy (AES.Notes);
      Free (AES);
   end Destroy;

   ---------
   -- Run --
   ---------

   procedure Run
     (AEH : access AES_Event_Handler)
   is
      use PolyORB.Jobs;
   begin

      --  Redispatch on Handle_Event operation.
      --  Note: this may destroy AEH.

      Handle_Event
        (AES_Event_Handler'Class (AEH.all)'Access);

   end Run;

end PolyORB.Asynch_Ev;
