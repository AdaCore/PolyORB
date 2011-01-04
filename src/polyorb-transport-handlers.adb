------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . H A N D L E R S            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Event handlers associated with all transport access points and endpoints

with PolyORB.Asynch_Ev;
with PolyORB.Components;
with PolyORB.Filters.Iface;

package body PolyORB.Transport.Handlers is

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event (H : access TE_AES_Event_Handler) is
      use PolyORB.Components;
      use PolyORB.ORB;

      Reply : constant Message'Class :=
                Emit
                  (Component_Access (H.TE),
                   Filters.Iface.Data_Indication'(Data_Amount => 0));
      --  The size of the data received is not known yet

   begin
      if Reply in Filters.Iface.Filter_Error then

         --  Notify the tasking policy that an endpoint is being destroyed.

         Handle_Close_Connection (H.ORB.Tasking_Policy, H.TE);

         declare
            use PolyORB.Smart_Pointers;

            Dependent_Binding_Object : Ref;
         begin
            --  Ensure that the binding object remains referenced while we
            --  are dismantling it.

            Reuse_Entity (Dependent_Binding_Object, H.TE.Binding_Object);
            pragma Assert (not Is_Nil (Dependent_Binding_Object));

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

            --  The complete binding object will be finalised when this block
            --  is exited, provided it is not referenced anymore.
         end;

      else
         null;
      end if;

   end Handle_Event;

end PolyORB.Transport.Handlers;
