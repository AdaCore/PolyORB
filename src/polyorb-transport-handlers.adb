------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . H A N D L E R S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Event handlers associated with all transport access points and endpoints

with PolyORB.Asynch_Ev;
with PolyORB.Components;
with PolyORB.Filters.Iface;

package body PolyORB.Transport.Handlers is

   ------------------
   -- Handle_Event --
   ------------------

   overriding procedure Handle_Event (H : access TE_AES_Event_Handler) is
      use PolyORB.Components;
      use PolyORB.ORB;
      use PolyORB.Smart_Pointers;

      Dependent_Binding_Object : constant Smart_Pointers.Ref :=
                                   H.Dependent_Binding_Object;
      pragma Unreferenced (Dependent_Binding_Object);
      --  Reference the dependent binding object to ensure it does not
      --  disappear under us while we are processing the event (present for
      --  reference counting purposes only, not otherwise referenced).

   begin
      H.Dependent_Binding_Object.Release;
      --  From this point on, H may handle another event

      declare
         Reply : Message'Class :=
           Emit
             (Component_Access (H.TE),
              Filters.Iface.Data_Indication'(Data_Amount => 0));
         --  The size of the data received is not known yet

      begin
         if Reply in Filters.Iface.Filter_Error then

            --  Notify the tasking policy that an endpoint is being destroyed

            Handle_Close_Connection (H.ORB.Tasking_Policy, H.TE);

            --  Close the endpoint. Note: for the case of a client side
            --  endpoint, this may clear the last reference to the BO, except
            --  for the above Dependent_Binding_Object).

            Emit_No_Reply
              (Component_Access (H.TE),
               Filters.Iface.Disconnect_Indication'(
                 Error => Filters.Iface.Filter_Error (Reply).Error));

            --  For the case of a server-side transport endpoint, the binding
            --  object is still be referenced by the TE for keep-alive purposes
            --  so we need to detach it now.

            if not Is_Nil (H.TE.Dependent_Binding_Object) then
               pragma Assert (Entity_Of (H.TE.Dependent_Binding_Object)
                                = H.TE.Binding_Object);
               Smart_Pointers.Set (H.TE.Dependent_Binding_Object, null);
            end if;

            --  Clear error once it has been propagated through the stack

            Errors.Catch (Filters.Iface.Filter_Error (Reply).Error);

            --  The complete binding object will be finalized after we exit
            --  this subprogram, if it is not referenced anywhere else anymore.
         end if;
      end;
   end Handle_Event;

   ---------------
   -- Stabilize --
   ---------------

   overriding function Stabilize
     (H : access TE_AES_Event_Handler) return Boolean
   is
   begin
      pragma Assert (H.Dependent_Binding_Object.Is_Nil);
      Smart_Pointers.Reuse_Entity
        (H.Dependent_Binding_Object, H.TE.Binding_Object);

      return not H.Dependent_Binding_Object.Is_Nil;
   end Stabilize;

end PolyORB.Transport.Handlers;
