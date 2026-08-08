------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . A S Y N C H _ E V                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2014, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Abstract data type for an asynchrous event source.

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

package body PolyORB.Asynch_Ev is

   ------------
   -- AEM_Of --
   ------------

   function AEM_Of (AES : Asynch_Ev_Source) return Asynch_Ev_Monitor_Access is
   begin
      return AES.Monitor;
   end AEM_Of;

   -------------
   -- Handler --
   -------------

   function Handler
     (AES : Asynch_Ev_Source'Class) return access AES_Event_Handler'Class is
   begin
      return AES.Handler;
   end Handler;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler
     (AES : in out Asynch_Ev_Source'Class;
      H   : access AES_Event_Handler'Class)
   is
   begin
      AES.Handler := H;
   end Set_Handler;

   ---------------
   -- Stabilize --
   ---------------

   function Stabilize (H : access AES_Event_Handler) return Boolean is
      pragma Unreferenced (H);
   begin
      return True;
   end Stabilize;

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
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Asynch_Ev_Source'Class,
         Name => Asynch_Ev_Source_Access);
   begin
      Free (AES);
   end Destroy;

   ---------
   -- Run --
   ---------

   overriding procedure Run (AEH : not null access AES_Event_Handler) is
   begin
      --  Redispatch on Handle_Event operation.
      --  Note: this may destroy AEH.

      Handle_Event (AES_Event_Handler'Class (AEH.all)'Access);
   end Run;

end PolyORB.Asynch_Ev;
