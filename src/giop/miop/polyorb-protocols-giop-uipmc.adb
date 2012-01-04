------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . P R O T O C O L S . G I O P . U I P M C          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.UIPMC is

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access UIPMC_Protocol;
      Session :    out Filter_Access) is
   begin
      PolyORB.Protocols.GIOP.Create
        (GIOP_Protocol (Proto.all)'Access, Session);
      GIOP_Session (Session.all).Conf := UIPMC_Conf'Access;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
      F : constant Flags := Sync_None or Sync_With_Transport;
   begin
      PolyORB.Protocols.GIOP.Initialize
        (UIPMC_Conf'Access,
         GIOP_Default_Version,
         F,
         False,
         "miop",
         "polyorb.protocols.miop.giop");
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"protocols.giop.uipmc",
       Conflicts => Empty,
       Depends   => +"setup.uipmc",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Protocols.GIOP.UIPMC;
