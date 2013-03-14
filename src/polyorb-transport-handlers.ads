------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . H A N D L E R S            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

--  Event handlers associated with all transport access points and
--  transport endpoints.

with PolyORB.Binding_Data;
with PolyORB.Filters;
with PolyORB.ORB;

package PolyORB.Transport.Handlers is

   type Transport_Event_Handler is
     abstract new PolyORB.Asynch_Ev.AES_Event_Handler with
      record
         ORB : PolyORB.ORB.ORB_Access;
      end record;

   --------------------------------
   -- Access point event handler --
   --------------------------------

   type TAP_AES_Event_Handler is
     abstract new Transport_Event_Handler with
      record
         TAP : PolyORB.Transport.Transport_Access_Point_Access;
         --  Factory of Transport_Endpoint components

         Filter_Factory_Chain : Filters.Factories_Access;
         --  Factory of Filter (protocol stack) components

         Profile_Factory : Binding_Data.Profile_Factory_Access;
         --  Factory of profiles capable of associating the address of TAP and
         --  the specification of the protocol implemented by
         --  Filter_Factory_Chain with an object id.
      end record;

   ----------------------------
   -- Endpoint event handler --
   ----------------------------

   type TE_AES_Event_Handler is
     new Transport_Event_Handler with
      record
         TE : PolyORB.Transport.Transport_Endpoint_Access;
         --  Back pointer to the corresponding endpoint

         Dependent_Binding_Object : Smart_Pointers.Ref;
         --  Binding object associated with TE
         --  Set only while processing an event, to ensure that the BO does
         --  not disappear prematurely.
      end record;

   overriding function Stabilize
     (H : access TE_AES_Event_Handler) return Boolean;

   overriding procedure Handle_Event (H : access TE_AES_Event_Handler);

end PolyORB.Transport.Handlers;
