------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                M O M A . M E S S A G E _ H A N D L E R S                 --
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

--  $Id: //droopi/main/src/moma/moma-message_handlers.adb

with MOMA.Messages;
with MOMA.Destinations;
with MOMA.Provider.Message_Handler;
with MOMA.Types;

with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Log;
with PolyORB.Minimal_Servant.Tools;
with PolyORB.Types;
with PolyORB.Requests;

package body MOMA.Message_Handlers is

   use MOMA.Messages;
   use MOMA.Message_Consumers;
   use MOMA.Destinations;
   use MOMA.Types;

   use PolyORB.Any;
   use PolyORB.Any.NVList;
   use PolyORB.Log;
   use PolyORB.Minimal_Servant.Tools;
   use PolyORB.Types;
   use PolyORB.Requests;

   package L is
     new PolyORB.Log.Facility_Log ("moma.message_handlers");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------
   -- Create_Handler --
   ---------------------

   function Create_Handler
     (Session             : MOMA.Sessions.Session;
      Message_Cons        : MOMA.Message_Consumers.Message_Consumer_Acc;
      Notifier_Procedure  : Notifier := null;
      Handler_Procedure   : Handler := null;
      Behavior            : MOMA.Types.Call_Back_Behavior := None)
      return MOMA.Message_Handlers.Message_Handler_Acc
   is
      Self    : MOMA.Message_Handlers.Message_Handler_Acc :=
         new MOMA.Message_Handlers.Message_Handler;
      Servant : constant MOMA.Provider.Message_Handler.Object_Acc :=
         new MOMA.Provider.Message_Handler.Object;
      Servant_Ref : PolyORB.References.Ref;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (Session);
      pragma Warnings (On);
      --  XXX Session is to be used to 'place' the receiver
      --  using session position in the POA.
      Initiate_Servant (Servant,
                        MOMA.Provider.Message_Handler.If_Desc,
                        MOMA.Types.MOMA_Type_Id,
                        Servant_Ref);
      Self.Message_Cons := Message_Cons;
      Self.Servant_Ref := Servant_Ref;
      Self.Handler_Procedure := Handler_Procedure;
      Self.Notifier_Procedure := Notifier_Procedure;
      Self.Behavior := Behavior;
      MOMA.Provider.Message_Handler.Initialize (Servant, Self);
      if Behavior /= None then
         Register_To_Servant (Self);
      end if;
      return Self;
   end Create_Handler;

   -----------------
   -- Get_Handler --
   -----------------

   function  Get_Handler (Self : access Message_Handler)
      return Handler
   is
   begin
      return Self.Handler_Procedure;
   end Get_Handler;

   ------------------
   -- Get_Notifier --
   ------------------

   function Get_Notifier (Self : access Message_Handler)
      return Notifier
   is
   begin
      return Self.Notifier_Procedure;
   end Get_Notifier;

   -------------------------
   -- Register_To_Servant --
   -------------------------

   procedure Register_To_Servant (Self : access Message_Handler)
   is
      Request     : PolyORB.Requests.Request_Access;
      Arg_List    : PolyORB.Any.NVList.Ref;
      Result      : PolyORB.Any.NamedValue;
      Servant_Ref : PolyORB.References.Ref;
      Self_Dest   : constant MOMA.Destinations.Destination :=
         MOMA.Destinations.Create_Destination
            (To_PolyORB_String (""), Self.Servant_Ref);
   begin
      pragma Debug (O ("Registering Message_Handler with " &
         Call_Back_Behavior'Image (Self.Behavior) & " behavior"));
      if Self.Message_Cons /= null then
         Servant_Ref := MOMA.Message_Consumers.Get_Ref (Self.Message_Cons.all);
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
           (Target    => Servant_Ref,
            Operation => "Register_Handler",
            Arg_List  => Arg_List,
            Result    => Result,
            Req       => Request);
         PolyORB.Requests.Invoke (Request);
         pragma Debug (O ("Register_Handler request complete"));
         PolyORB.Requests.Destroy_Request (Request);
         pragma Debug (O ("Register_Handler request destroyed"));
      end if;
   end Register_To_Servant;

   ------------------
   -- Set_Behavior --
   ------------------

   procedure Set_Behavior (
      Self           : access Message_Handler;
      New_Behavior   : in MOMA.Types.Call_Back_Behavior)
   is
      Previous_Behavior :
         constant MOMA.Types.Call_Back_Behavior := Self.Behavior;
   begin
      Self.Behavior := New_Behavior;
      if (New_Behavior /= Previous_Behavior) then
         Register_To_Servant (Self);
      end if;
   end Set_Behavior;

   -----------------
   -- Set_Handler --
   -----------------

   procedure Set_Handler (
      Self                    : access Message_Handler;
      New_Handler_Procedure   : in Handler;
      Handle_Behavior         : Boolean := False)
   is
   begin
      Self.Handler_Procedure := New_Handler_Procedure;
      if Handle_Behavior then
         Set_Behavior (Self, Handle);
      end if;
   end Set_Handler;

   ------------------
   -- Set_Notifier --
   ------------------

   procedure Set_Notifier (
      Self                    : access Message_Handler;
      New_Notifier_Procedure  : in Notifier;
      Notify_Behavior         : Boolean := False)
   is
   begin
      Self.Notifier_Procedure := New_Notifier_Procedure;
      if Notify_Behavior then
         Set_Behavior (Self, Notify);
      end if;
   end Set_Notifier;

   ----------------------
   -- Template_Handler --
   ----------------------

   procedure Template_Handler (
      Self     : access Message_Handler;
      Message  : MOMA.Messages.Message'Class)
   is
      Id : constant String := MOMA.Types.To_Standard_String (
         MOMA.Messages.Get_Message_Id (Message));
   begin
      pragma Debug (O ("Message_Handler is handling message"));
      if Id = "Stop handling messages" then
         Set_Behavior (Self, None);
      end if;
   end Template_Handler;

   -----------------------
   -- Template_Notifier --
   -----------------------

   procedure Template_Notifier (
      Self : access Message_Handler)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);
   begin
      pragma Debug (O ("Message_Handler is being notified of a message"));
      null;
   end Template_Notifier;

end MOMA.Message_Handlers;

