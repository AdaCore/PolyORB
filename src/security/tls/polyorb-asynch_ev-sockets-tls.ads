------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        P O L Y O R B . A S Y N C H _ E V . S O C K E T S . T L S         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  An asynchrous event source that is a set of SSL sockets.

with PolyORB.TLS;

package PolyORB.Asynch_Ev.Sockets.TLS is

   pragma Elaborate_Body;

   type TLS_Event_Monitor is new Socket_Event_Monitor with private;

   type TLS_Event_Source is new Socket_Event_Source with private;

   overriding function Register_Source
     (AEM     : access TLS_Event_Monitor;
      AES     : Asynch_Ev_Source_Access) return Register_Source_Result;

   overriding function Check_Sources
     (AEM     : access TLS_Event_Monitor;
      Timeout : Duration) return AES_Array;

   function Create_Event_Source
     (Socket : PolyORB.TLS.TLS_Socket_Type)
     return Asynch_Ev_Source_Access;

   function Create_Event_Source
     (Socket : PolyORB.Sockets.Socket_Type)
     return Asynch_Ev_Source_Access;

   overriding function AEM_Factory_Of
     (AES : TLS_Event_Source)
     return AEM_Factory;

private

   type TLS_Event_Source is new Socket_Event_Source with record
      TLS_Socket : PolyORB.TLS.TLS_Socket_Type;
   end record;

   type TLS_Event_Monitor is new Socket_Event_Monitor with null record;

end PolyORB.Asynch_Ev.Sockets.TLS;
