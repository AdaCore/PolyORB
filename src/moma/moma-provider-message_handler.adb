------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . P R O V I D E R . M E S S A G E _ H A N D L E R         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Message_Handler servant.

--  $Id$

with MOMA.Message_Consumers.Queues;
with MOMA.Messages;
with MOMA.Destinations;
with MOMA.Types;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Types;
with PolyORB.Requests;

package body MOMA.Provider.Message_Handler is

   use MOMA.Destinations;
   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Types;
   use PolyORB.Requests;

   package L is
     new PolyORB.Log.Facility_Log ("moma.provider.message_handler");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Object_Acc;
                         Message_Queue : Queue_Acc;
                         Self_Ref : PolyORB.References.Ref;
                         New_Notifier_Procedure : in Notifier := null;
                         New_Handler_Procedure : in Handler := null)
   is
   begin
      Self.Message_Queue := Message_Queue;
      Self.Self_Ref := Self_Ref;
      if New_Handler_Procedure /= null then
         Set_Handler (Self, New_Handler_Procedure);
      else
         Set_Notifier (Self, New_Notifier_Procedure);
      end if;
   end Initialize;

   ------------
   -- Invoke --
   ------------

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access)
   is
      Args        : PolyORB.Any.NVList.Ref;
      Operation   : constant String := To_Standard_String (Req.all.Operation);
   begin
      pragma Debug (O ("The message handler is executing the request:"
                    & PolyORB.Requests.Image (Req.all)));

      PolyORB.Any.NVList.Create (Args);

      Args := Get_Parameter_Profile (Operation);
      PolyORB.Requests.Arguments (Req, Args);

      if Req.all.Operation = To_PolyORB_String ("Notify") then
         begin
            Notify (Self);
         end;
      elsif Operation = "Handle" then
         declare
            use PolyORB.Any.NVList.Internals;
            Args_Sequence  : constant NV_Sequence_Access := List_Of (Args);
            Message        : PolyORB.Any.Any :=
              NV_Sequence.Element_Of (Args_Sequence.all, 1).Argument;
         begin
            Handle (Self, Message);
         end;
      end if;
   end Invoke;

   ---------------------------
   -- Get_Parameter_Profile --
   ---------------------------

   function Get_Parameter_Profile
     (Method : String)
     return PolyORB.Any.NVList.Ref
   is
      Result : PolyORB.Any.NVList.Ref;
   begin
      PolyORB.Any.NVList.Create (Result);
      pragma Debug (O ("Parameter profile for " & Method & " requested."));
      if Method = "Notify" then
         null;
      elsif Method = "Handle" then
         PolyORB.Any.NVList.Add_Item
            (Result,
             (Name      => To_PolyORB_String ("Message"),
              Argument  => PolyORB.Any.Get_Empty_Any
                              (MOMA.Messages.TC_MOMA_Message),
              Arg_Modes => PolyORB.Any.ARG_IN));
      else
         raise Program_Error;
      end if;
      return Result;
   end Get_Parameter_Profile;

   ------------------------
   -- Get_Result_Profile --
   ------------------------

   function Get_Result_Profile
     (Method : String)
     return PolyORB.Any.Any
   is
      use PolyORB.Any;

   begin
      pragma Debug (O ("Result profile for " & Method & " requested."));
      if Method = "Handle" or Method = "Notify" then
         return Get_Empty_Any (TypeCode.TC_Void);
      else
         raise Program_Error;
      end if;
   end Get_Result_Profile;

   -------------
   -- If_Desc --
   -------------

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description is
   begin
      return
        (PP_Desc => Get_Parameter_Profile'Access,
         RP_Desc => Get_Result_Profile'Access);
   end If_Desc;

   ------------
   -- Handle --
   ------------

   procedure Handle (Self    : access Object;
                     Message : PolyORB.Any.Any)
   is
      Rcvd_Message : constant MOMA.Messages.Message'Class :=
         MOMA.Messages.From_Any (Message);
   begin
      if Self.Handler_Procedure /= null then
         Self.Handler_Procedure.all (Self.Message_Queue.all, Rcvd_Message);
      end if;
      Self.Notifier_Procedure := null;
   end Handle;

   ------------
   -- Notify --
   ------------

   procedure Notify (Self : access Object)
   is
   begin
      if Self.Notifier_Procedure /= null then
         Self.Notifier_Procedure.all (Self.Message_Queue.all);
      end if;
      Self.Handler_Procedure := null;
   end Notify;

   -----------------------
   -- Register_To_Queue --
   -----------------------

   procedure Register_To_Queue (Self : access Object)
   is
      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
      Queue_Ref   : PolyORB.References.Ref;
      Self_Dest   : constant MOMA.Destinations.Destination :=
         MOMA.Destinations.Create (To_PolyORB_String (""), Self.Self_Ref);
   begin
      pragma Debug (O ("Registering Message_Handler with " &
         Call_Back_Behavior'Image (Self.Behavior) & " behavior"));
      if Self.Message_Queue /= null then
         Queue_Ref := MOMA.Message_Consumers.Get_Ref (
            MOMA.Message_Consumers.Message_Consumer (Self.Message_Queue.all));
         PolyORB.Any.NVList.Create (Arg_List);
         PolyORB.Any.NVList.Add_Item (Arg_List,
                                      To_PolyORB_String ("Message_Handler"),
                                      To_Any (Self_Dest),
                                      PolyORB.Any.ARG_IN);
         PolyORB.Any.NVList.Add_Item (Arg_List,
                                      To_PolyORB_String ("Behavior"),
                                      To_Any (To_PolyORB_String (
                                         Call_Back_Behavior'Image (
                                            Self.Behavior))),
                                      PolyORB.Any.ARG_IN);
         Result := (Name      => To_PolyORB_String ("Result"),
                    Argument  => PolyORB.Any.Get_Empty_Any
                                    (TypeCode.TC_Any),
                    Arg_Modes => 0);
         PolyORB.Requests.Create_Request
           (Target    => Queue_Ref,
            Operation => "Register_Handler",
            Arg_List  => Arg_List,
            Result    => Result,
            Req       => Request);
         PolyORB.Requests.Invoke (Request);
         PolyORB.Requests.Destroy_Request (Request);
      end if;
   end Register_To_Queue;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler (Self : access Object;
                          New_Handler_Procedure : in Handler) is
      Previous_Behavior :
         constant MOMA.Types.Call_Back_Behavior := Self.Behavior;
   begin
      Self.Notifier_Procedure := null;
      Self.Handler_Procedure := New_Handler_Procedure;
      if New_Handler_Procedure /= null then
         Self.Behavior := Handle;
      else
         Self.Behavior := None;
      end if;
      if Self.Behavior /= Previous_Behavior then
         Register_To_Queue (Self);
      end if;
   end Set_Handler;

   ------------------
   -- Set_Notifier --
   ------------------

   procedure Set_Notifier (Self : access Object;
                           New_Notifier_Procedure : in Notifier) is
      Previous_Behavior :
         constant MOMA.Types.Call_Back_Behavior := Self.Behavior;
   begin
      Self.Handler_Procedure := null;
      Self.Notifier_Procedure := New_Notifier_Procedure;
      if New_Notifier_Procedure /= null then
         Self.Behavior := Notify;
      else
         Self.Behavior := None;
      end if;
      if Self.Behavior /= Previous_Behavior then
         Register_To_Queue (Self);
      end if;
   end Set_Notifier;

   ---------------
   -- Set_Queue --
   ---------------

   procedure Set_Queue
     (Self : access Object;
      New_Queue : Queue_Acc) is
   begin
      Self.Message_Queue := New_Queue;
      Register_To_Queue (Self);
   end Set_Queue;

   ----------------------
   -- Template_Handler --
   ----------------------

   procedure Template_Handler (
      Message_Queue : MOMA.Message_Consumers.Queues.Queue;
      Message : MOMA.Messages.Message'Class)
   is
   begin
      pragma Debug (O ("Message_Handler is handling message"));
      null;
   end Template_Handler;

   -----------------------
   -- Template_Notifier --
   -----------------------

   procedure Template_Notifier (
      Message_Queue : MOMA.Message_Consumers.Queues.Queue)
   is
   begin
      pragma Debug (O ("Message_Handler is being notified of a message"));
      null;
   end Template_Notifier;

end MOMA.Provider.Message_Handler;
