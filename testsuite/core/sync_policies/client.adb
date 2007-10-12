------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2005 Free Software Foundation, Inc.          --
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
         (Condition =>
            Req.Completed'Access,
          Task_Info => Req.Requesting_Task'Access),
         May_Poll => True);
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
