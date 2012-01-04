------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P E R I O D I C _ C L I E N T S                      --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Real_Time;

with CORBA;
with RTCORBA;

with DHB;

package Periodic_Clients is

   type Periodic_Task_Information is record
     Id                        : Natural;
     Worker_String_Ref         : CORBA.String;
     Client_Priority           : RTCORBA.Priority;
     Client_Workload           : Positive;
     Initial_Server_Workload   : DHB.KWIPS;
     Server_Workload_Increment : DHB.KWIPS;
     Period                    : Ada.Real_Time.Time_Span;
   end record;

   type Periodic_Task_Array is array (Positive range <>)
     of Periodic_Task_Information;

   procedure Run_Test_1 (PTA : Periodic_Task_Array);

   procedure Run_Test_2 (Worker_String_Ref : CORBA.String);

end Periodic_Clients;
