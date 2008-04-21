------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   D H B . B A C K G R O U N D _ W O R K E R _ F A C T O R Y . I M P L    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 2006-2008, Free Software Foundation, Inc.           --
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

with CORBA.Impl;

with DHB.Background_Worker.Helper;
with DHB.Background_Worker.Impl;

with DHB.Background_Worker_Factory.Skel;
pragma Warnings (Off);
--  Compiler wants Elaborate_All, but that causes cycles
pragma Elaborate (DHB.Background_Worker_Factory.Skel);
pragma Warnings (On);
pragma Warnings (Off, DHB.Background_Worker_Factory.Skel);

with PortableServer.POA;

package body DHB.Background_Worker_Factory.Impl is

   ------------
   -- Create --
   ------------

   function Create (Self : access Object) return DHB.Background_Worker.Ref is
      Object : constant CORBA.Impl.Object_Ptr
        := new DHB.Background_Worker.Impl.Object;

   begin
      return DHB.Background_Worker.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (PortableServer.POA.Local_Ref (Self.RT_POA),
          PortableServer.Servant (Object)));
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self       : access Object;
      The_Worker : DHB.Background_Worker.Ref)
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

end DHB.Background_Worker_Factory.Impl;
