------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--        M O M A . M E S S A G E _ C O N S U M E R S . Q U E U E S         --
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

--  $Id: //droopi/main/src/moma/moma-message_consumers-queues.adb#3

with PolyORB.Any;
with PolyORB.Any.NVList;
--  with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Types;

with MOMA.Messages;
with MOMA.Provider.Message_Handler;

package body MOMA.Message_Consumers.Queues is

   use MOMA.Messages;

   use PolyORB.Any;
   --  use PolyORB.Log;
   use PolyORB.Types;

   --  package L is
   --    new PolyORB.Log.Facility_Log ("moma.message_consumers.queues");
   --  procedure O (Message : in Standard.String; Level : Log_Level := Debug)
   --    renames L.Output;

   ------------------------
   -- Get_Queue Function --
   ------------------------

   function Get_Queue (Self : Queue) return MOMA.Destinations.Queue is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      return Get_Queue (Self);
      pragma Warnings (On);
   end Get_Queue;

   -------------
   -- Receive --
   -------------

   function Receive (Self : Queue)
                     return MOMA.Messages.Message'Class
   is
      Argument_Mesg : PolyORB.Any.Any := PolyORB.Any.To_Any
        (To_PolyORB_String (""));
      --  XXX Temporary hack, should pass message filter ... or not ?

      Request        : PolyORB.Requests.Request_Access;
      Arg_List       : PolyORB.Any.NVList.Ref;
      Result         : PolyORB.Any.NamedValue;
      Result_Name    : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item (Arg_List,
                                   To_PolyORB_String ("Message"),
                                   Argument_Mesg,
                                   PolyORB.Any.ARG_IN);

      Result := (Name      => PolyORB.Types.Identifier (Result_Name),
                 Argument  => PolyORB.Any.Get_Empty_Any (TC_MOMA_Message),
                 Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => Get_Ref (Self),
         Operation => "Get",
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);

      PolyORB.Requests.Destroy_Request (Request);

      return MOMA.Messages.From_Any (Result.Argument);
   end Receive;

   function Receive (Timeout : Time) return MOMA.Messages.Message is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Receive (Timeout);
      pragma Warnings (On);
   end Receive;

   ---------------------
   -- Receive_No_Wait --
   ---------------------

   function Receive_No_Wait return MOMA.Messages.Message is
   begin
      raise PolyORB.Not_Implemented;
      pragma Warnings (Off);
      return Receive_No_Wait;
      pragma Warnings (On);
   end Receive_No_Wait;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler (Self : in out Queue;
                          New_Handler_Procedure : in Handler) is
   begin
      Self.Handler_Procedure := New_Handler_Procedure;
      if not Self.Has_Initialized_Message_Handler then
         Initialize_Message_Handler (Self);
      end if;
   end Set_Handler;

   ------------------
   -- Set_Notifier --
   ------------------

   procedure Set_Notifier (Self : in out Queue;
                           New_Notifier_Procedure : in Notifier) is
   begin
      Self.Notifier_Procedure := New_Notifier_Procedure;
      if not Self.Has_Initialized_Message_Handler then
         Initialize_Message_Handler (Self);
      end if;
   end Set_Notifier;

   ------------
   -- Handle --
   ------------

   procedure Handle (Self : Queue;
                     Message : MOMA.Messages.Message'Class) is
   begin
      if Self.Handler_Procedure /= null then
         Self.Handler_Procedure.all (Self, Message);
      end if;
   end Handle;

   ------------
   -- Notify --
   ------------

   procedure Notify (Self : Queue) is
   begin
      if Self.Notifier_Procedure /= null then
         Self.Notifier_Procedure.all (Self);
      end if;
   end Notify;

   --------------------------------
   -- Initialize_Message_Handler --
   --------------------------------

   procedure Initialize_Message_Handler (Self : in out Queue) is
      Message_Handler_Ref : PolyORB.References.Ref;
   begin
      Message_Handler_Ref :=
         MOMA.Provider.Message_Handler.Initialize (Self);
      Self.Has_Initialized_Message_Handler := True;
      raise PolyORB.Not_Implemented;
      --  XXX Then send the reference encapsulated in a request
      --  to the actual Message Consumer servant.

   end Initialize_Message_Handler;

end MOMA.Message_Consumers.Queues;



