------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . A S Y N C H _ E V . S O C K E T S . S S L         --
--                                                                          --
--                                 S p e c                                  --
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

--  An asynchrous event source that is a set of SSL sockets.

with PolyORB.SSL;

package PolyORB.Asynch_Ev.Sockets.SSL is

   pragma Elaborate_Body;

   type SSL_Event_Monitor is new Socket_Event_Monitor with private;

   type SSL_Event_Source is new Socket_Event_Source with private;

   procedure Register_Source
     (AEM     : access SSL_Event_Monitor;
      AES     :        Asynch_Ev_Source_Access;
      Success :    out Boolean);

   function Check_Sources
     (AEM     : access SSL_Event_Monitor;
      Timeout :        Duration)
     return AES_Array;

   function Create_Event_Source
     (Socket : PolyORB.SSL.SSL_Socket_Type)
     return Asynch_Ev_Source_Access;

   function Create_Event_Source
     (Socket : PolyORB.Sockets.Socket_Type)
     return Asynch_Ev_Source_Access;
   --  XXX This subprogram can be removed once multiple event source
   --  monitors are implemented in ORB Controllers

   function AEM_Factory_Of (AES : SSL_Event_Source) return AEM_Factory;

private

   type SSL_Event_Source is new Socket_Event_Source with record
      SSL_Socket : PolyORB.SSL.SSL_Socket_Type;
   end record;

   type SSL_Event_Monitor is new Socket_Event_Monitor with null record;

end PolyORB.Asynch_Ev.Sockets.SSL;
