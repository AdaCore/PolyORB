------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              D H B . W O R K E R _ F A C T O R Y . I M P L               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with CORBA.Impl;

with DHB.Worker.Helper;
with DHB.Worker.Impl;

with DHB.Worker_Factory.Skel;
pragma Warnings (Off);
--  Compiler wants Elaborate_All, but that causes cycles
pragma Elaborate (DHB.Worker_Factory.Skel);
pragma Warnings (On);
pragma Warnings (Off, DHB.Worker_Factory.Skel);

with PortableServer.POA;

package body DHB.Worker_Factory.Impl is

   ------------
   -- Create --
   ------------

   function Create (Self : access Object) return DHB.Worker.Ref is
      Object : constant CORBA.Impl.Object_Ptr
        := new DHB.Worker.Impl.Object;

   begin
      return DHB.Worker.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (PortableServer.POA.Local_Ref (Self.RT_POA),
          PortableServer.Servant (Object)));
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self       : access Object;
      The_Worker : DHB.Worker.Ref)
   is
      pragma Unreferenced (Self, The_Worker);

   begin
      raise Program_Error;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : access Object;
      RT_POA : RTPortableServer.POA.Local_Ref)
   is
   begin
      Self.RT_POA := RT_POA;
   end Initialize;

end DHB.Worker_Factory.Impl;
