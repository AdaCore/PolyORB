------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . A S Y N C H _ E V . L O C A L _ S O C K E T S       --
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

with PolyORB.Local_Sockets;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Asynch_Ev.Local_Sockets is

   pragma Elaborate_Body;

   type Local_Event_Monitor is new Asynch_Ev_Monitor with private;

   procedure Create (AEM : out Local_Event_Monitor);

   procedure Destroy (AEM : in out Local_Event_Monitor);

   type Local_Event_Source is new Asynch_Ev_Source with private;

   procedure Register_Source
     (AEM     : access Local_Event_Monitor;
      AES     : Asynch_Ev_Source_Access;
      Success : out Boolean);

   procedure Unregister_Source
     (AEM : in out Local_Event_Monitor;
      AES : Asynch_Ev_Source_Access);

   function Check_Sources
     (AEM     : access Local_Event_Monitor;
      Timeout : Duration)
      return    AES_Array;

   procedure Abort_Check_Sources (AEM : Local_Event_Monitor);

   function Create_Event_Source
     (Socket : PolyORB.Local_Sockets.Local_Socket_Access)
      return   Asynch_Ev_Source_Access;

   function AEM_Factory_Of (AES : Local_Event_Source) return AEM_Factory;

private
   type Local_Event_Source is new Asynch_Ev_Source with record
      Socket : PolyORB.Local_Sockets.Local_Socket_Access;
   end record;

   package Source_Lists is new PolyORB.Utils.Chained_Lists (
      Asynch_Ev_Source_Access,
      Doubly_Chained => True);

   type Local_Event_Monitor is new Asynch_Ev_Monitor with record
      Selector      : PolyORB.Local_Sockets.Local_Selector_Type;
      Monitored_Set : PolyORB.Local_Sockets.Local_Socket_Set_Type;
      Sources       : Source_Lists.List;
   end record;

end PolyORB.Asynch_Ev.Local_Sockets;
