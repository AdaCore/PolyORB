------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . P R O T O C O L S . G I O P . U I P M C         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.UIPMC is

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access UIPMC_Protocol;
      Session :    out Filter_Access) is
   begin
      PolyORB.Protocols.GIOP.Create (GIOP_Protocol (Proto.all)'Access,
                                     Session);
      GIOP_Session (Session.all).Conf := UIPMC_Conf'Access;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize
   is
      use PolyORB.Requests;

      F : constant Flags :=
        Sync_None or
        Sync_With_Transport;
   begin
      PolyORB.Protocols.GIOP.Initialize
        (UIPMC_Conf'Access,
         GIOP_Version'(Major => 1, Minor => 2),
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
       Init      => Initialize'Access));

end PolyORB.Protocols.GIOP.UIPMC;
