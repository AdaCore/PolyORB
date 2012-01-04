------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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

with Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Initialization;
with PolyORB.ORB.Iface;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Types;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is

   use Ada.Text_IO;
   use PolyORB.Utils.Report;

   -------------------
   -- Issue_Request --
   -------------------

   procedure Issue_Request
     (Obj_Ref   : PolyORB.References.Ref;
      Req_Flags : PolyORB.Requests.Flags);

   procedure Issue_Request
     (Obj_Ref   : PolyORB.References.Ref;
      Req_Flags : PolyORB.Requests.Flags)
   is
      use PolyORB.Any;
      use PolyORB.Any.NVList;
      use PolyORB.Components;
      use PolyORB.ORB.Iface;
      use PolyORB.Requests;
      use PolyORB.Types;

      Req : Request_Access;
      Args : PolyORB.Any.NVList.Ref;
      Result : PolyORB.Any.NamedValue;

   begin
      Create (Args);
      Add_Item
        (Args,
         To_PolyORB_String ("ping"),
         To_Any (To_PolyORB_String ("ping !")),
         ARG_IN);

      Create_Request
        (Obj_Ref,
         "ping",
         Args,
         Result,
         PolyORB.Any.ExceptionList.Nil_Ref,
         Req,
         Req_Flags);

      Output ("Created servant request with flag"
              & PolyORB.Requests.Flags'Image (Req_Flags),
              True);

      Emit_No_Reply
        (Component_Access (PolyORB.Setup.The_ORB),
         Queue_Request'(Request   => Req,
                        Requestor => null));

      PolyORB.ORB.Run
        (PolyORB.Setup.The_ORB,
         Request  => Req,
         May_Exit => True);
   end Issue_Request;

begin
   New_Test ("Request synchronization policies");

   PolyORB.Initialization.Initialize_World;

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   declare
      Obj_Ref : PolyORB.References.Ref;

   begin
      PolyORB.References.String_To_Object
        (Ada.Command_Line.Argument (1), Obj_Ref);

      Issue_Request (Obj_Ref, PolyORB.Requests.Sync_None);
      Issue_Request (Obj_Ref, PolyORB.Requests.Sync_With_Transport);
      Issue_Request (Obj_Ref, PolyORB.Requests.Sync_With_Server);
      Issue_Request (Obj_Ref, PolyORB.Requests.Sync_With_Target);
   end;

   End_Report;
end Client;
