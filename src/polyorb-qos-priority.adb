------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . Q O S . P R I O R I T Y                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Buffers;
with PolyORB.Initialization;
with PolyORB.References;
with PolyORB.Representations.CDR.Common;
with PolyORB.Request_QoS;
with PolyORB.QoS.Service_Contexts;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Types;

package body PolyORB.QoS.Priority is

   use PolyORB.Buffers;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.QoS.Service_Contexts;

   function To_RTCorbaPriority_Service_Context
     (QoS : QoS_Parameter_Access)
      return Service_Context;

   function To_QoS_Static_Priority_Parameter
     (SC : Service_Context)
      return QoS_Parameter_Access;

   --------------------
   -- Fetch_Priority --
   --------------------

   function Fetch_Priority
     (Ref : PolyORB.References.Ref)
     return QoS_Parameter_Access;

   function Fetch_Priority
     (Ref : PolyORB.References.Ref)
     return QoS_Parameter_Access
   is
      use PolyORB.Tasking.Threads;
      use PolyORB.Tasking.Threads.Annotations;

      pragma Unreferenced (Ref);

      Note : Thread_Priority_Note;

   begin
      Get_Note (Get_Current_Thread_Notepad.all, Note, Default_Note);

      if Note /= Default_Note then
         return new QoS_Static_Priority'(Kind => Static_Priority,
                                         EP => Note.Priority);
      else
         return null;
      end if;
   end Fetch_Priority;

   --------------------------------------
   -- To_QoS_Static_Priority_Parameter --
   --------------------------------------

   function To_QoS_Static_Priority_Parameter
     (SC : Service_Context)
      return QoS_Parameter_Access
   is
      Buffer : aliased Buffer_Type;
      EP     : PolyORB.Types.Short;
   begin
      Decapsulate (SC.Context_Data, Buffer'Access);

      EP := Unmarshall (Buffer'Access);
      return
        new QoS_Static_Priority'
        (Kind => Static_Priority,
         EP   => PolyORB.Tasking.Priorities.External_Priority (EP));
   end To_QoS_Static_Priority_Parameter;

   ----------------------------------------
   -- To_RTCorbaPriority_Service_Context --
   ----------------------------------------

   function To_RTCorbaPriority_Service_Context
     (QoS : QoS_Parameter_Access)
      return Service_Context
   is
      Buffer : Buffer_Access;
      Result : Service_Context := (RTCorbaPriority, null);
   begin
      if QoS = null then
         return Result;
      end if;

      Buffer := new Buffer_Type;

      Start_Encapsulation (Buffer);

      Marshall
        (Buffer, PolyORB.Types.Short (QoS_Static_Priority (QoS.all).EP));

      Result.Context_Data := new Encapsulation'(Encapsulate (Buffer));

      Release (Buffer);

      return Result;
   end To_RTCorbaPriority_Service_Context;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Request_QoS.Register (Static_Priority, Fetch_Priority'Access);

      Register (Static_Priority, To_RTCorbaPriority_Service_Context'Access);
      Register (RTCorbaPriority, To_QoS_Static_Priority_Parameter'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"request_qos.priority",
       Conflicts => Empty,
       Depends   => +"tasking.annotations",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.QoS.Priority;
