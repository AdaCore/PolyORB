------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A S Y N C H _ E V . S O C K E T S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2011, Free Software Foundation, Inc.          --
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

--  An asynchrous event source that is a set of socket descriptors.

with PolyORB.Sockets;
with PolyORB.Utils.Ilists;

package PolyORB.Asynch_Ev.Sockets is

   pragma Elaborate_Body;

   type Socket_Event_Monitor is new Asynch_Ev_Monitor with private;

   procedure Create (AEM : out Socket_Event_Monitor);

   procedure Destroy (AEM : in out Socket_Event_Monitor);

   function Has_Sources (AEM : Socket_Event_Monitor) return Boolean;

   type Socket_Event_Source is new Asynch_Ev_Source with private;

   function Register_Source
     (AEM     : access Socket_Event_Monitor;
      AES     : Asynch_Ev_Source_Access) return Register_Source_Result;

   procedure Unregister_Source
     (AEM     : in out Socket_Event_Monitor;
      AES     : Asynch_Ev_Source_Access;
      Success : out Boolean);

   function Check_Sources
     (AEM     : access Socket_Event_Monitor;
      Timeout : Duration) return AES_Array;

   procedure Abort_Check_Sources (AEM : Socket_Event_Monitor);

   function Create_Event_Source
     (Socket : PolyORB.Sockets.Socket_Type) return Asynch_Ev_Source_Access;

   function AEM_Factory_Of (AES : Socket_Event_Source) return AEM_Factory;

private

   package Ilists renames Utils.Ilists;

   type SES_Access is access all Socket_Event_Source'Class;
   type Links_Type is array (Ilists.Link_Type) of aliased SES_Access;

   type Socket_Event_Source is new Asynch_Ev_Source with record
      Links  : Links_Type;
      Socket : PolyORB.Sockets.Socket_Type;
   end record;

   function Link
     (S     : access Socket_Event_Source'Class;
      Which : Ilists.Link_Type) return access SES_Access;
   pragma Inline (Link);
   --  Accessor for Links

   package Source_Lists is
     new Ilists.Lists
       (T             => Socket_Event_Source'Class,
        T_Acc         => SES_Access,
        Doubly_Linked => True);

   type Socket_Event_Monitor is new Asynch_Ev_Monitor with record
      Selector      : PolyORB.Sockets.Selector_Type;
      Monitored_Set : PolyORB.Sockets.Socket_Set_Type;
      Sources       : Source_Lists.List;
   end record;

end PolyORB.Asynch_Ev.Sockets;
