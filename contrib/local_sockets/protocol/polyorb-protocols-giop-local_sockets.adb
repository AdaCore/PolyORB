------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.PROTOCOLS.GIOP.LOCAL_SOCKETS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Protocols.GIOP.Local_Sockets is

   ------------
   -- Create --
   ------------

   procedure Create
     (Proto   : access Local_Sockets_Protocol;
      Session : out Filter_Access)
   is
   begin
      PolyORB.Protocols.GIOP.Create
        (GIOP_Protocol (Proto.all)'Access,
         Session);
      GIOP_Session (Session.all).Conf := Local_Sockets_Conf'Access;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Protocols.GIOP.Initialize
        (Local_Sockets_Conf'Access,
         GIOP_Version'(Major => 1, Minor => 2),
         PolyORB.Requests.Sync_With_Transport,
         False,
         "lsiop",
         "polyorb.protocols.giop.lsiop");
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
     (Name      => +"protocols.giop.lsiop",
      Conflicts => Empty,
      Depends   => +"protocols.giop.giop_1_2",
      Provides  => Empty,
      Implicit  => False,
      Init      => Initialize'Access));
end PolyORB.Protocols.GIOP.Local_Sockets;
