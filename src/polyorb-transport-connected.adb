------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T R A N S P O R T . C O N N E C T E D           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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

--  Abstract connected transport service access points and transport endpoints.

with Ada.Exceptions;

with PolyORB.Log;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.ORB.Interface;

package body PolyORB.Transport.Connected is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.transport.connected");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event
     (H : access Connected_TAP_AES_Event_Handler)
   is
      use PolyORB.Components;
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Filters;

      New_TE     : Transport_Endpoint_Access;
      New_Filter : Filter_Access;
   begin
      pragma Debug (O ("Handle_Event: Connected TAP AES"));

      Accept_Connection
        (Connected_Transport_Access_Point'Class (H.TAP.all), New_TE);
      --  Create transport endpoint.

      New_Filter := Create_Filter_Chain
        (H.Filter_Factory_Chain);
      --  Create filter chain for end point

      pragma Debug (O ("Inserting new source: Endpoint"));
      Register_Endpoint (ORB_Access (H.ORB),
                         New_TE,
                         New_Filter, Server);
      --  Register end poitn to ORB

      Emit_No_Reply
        (H.ORB,
         Monitor_Access_Point'(TAP => H.TAP));
      --  Continue monitoring the TAP's AES.
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event
     (H : access Connected_TE_AES_Event_Handler)
   is
      use PolyORB.Components;
      use PolyORB.ORB;
   begin
      Emit_No_Reply
        (Component_Access (H.TE),
         Filters.Interface.Data_Indication'
         (Data_Amount => 0));
      --  The size of the data received is not known yet.

   exception
      when Connection_Closed =>
         O ("Connection closed.");

         --  Close has been called on the transport endpoint.
         --  Both the Endpoint and the associated AES must
         --  now be destroyed.
         Handle_Close_Server_Connection
           (ORB_Access (H.ORB).Tasking_Policy, H.TE);

         Destroy (H.TE);
         --  Destroy the transport endpoint and the associated
         --  protocol stack.

         Destroy (H.AES);
         --  No need to Unregister_Source, because the AES
         --  is already unregistered while an event is being
         --  processed.

      when E : others =>
         O ("Got exception while sending Data_Indication:", Error);
         O (Ada.Exceptions.Exception_Information (E), Error);
         Close (H.TE.all);

         Destroy (H.TE);
         Destroy (H.AES);
   end Handle_Event;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (TE  : access Connected_Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Components;
      use PolyORB.Filters;
      use PolyORB.Filters.Interface;
      use PolyORB.Buffers;

      Nothing : Components.Null_Message;
   begin
      if Msg in Connect_Indication then
         return Emit (TE.Upper, Msg);

      elsif Msg in Data_Expected then
         declare
            DE : Data_Expected renames Data_Expected (Msg);
         begin
            pragma Assert (DE.In_Buf /= null);
            TE.In_Buf := DE.In_Buf;
            TE.Max    := DE.Max;
         end;

         return Emit
           (TE.Server, ORB.Interface.Monitor_Endpoint'
              (TE => Transport_Endpoint_Access (TE)));

      elsif Msg in Data_Indication then
         pragma Debug (O ("Data received"));

         if TE.In_Buf = null then
            O ("Unexpected data (no buffer)");

            Close (Transport_Endpoint'Class (TE.all));
            raise Connection_Closed;
            --  Notify the ORB that the socket was disconnected.
         end if;

         declare
            use type Ada.Streams.Stream_Element_Count;
            Size : Ada.Streams.Stream_Element_Count := TE.Max;
         begin
            Read (Transport_Endpoint'Class (TE.all), TE.In_Buf, Size);

            if Size = 0 then
               O ("Connection closed.");

               Close (Transport_Endpoint'Class (TE.all));
               raise Connection_Closed;
               --  Notify the ORB that the socket was disconnected.
               --  The sender of the Data_Indication message is
               --  reponsible for handling this exception and closing
               --  the transport endpoint, if necessary.
            end if;

            return Emit (TE.Upper, Data_Indication'(Data_Amount => Size));
            --  Note: this component guarantees that the upper layers will
            --  only receive Data_Indications with a non-zero Data_Amount.

         end;

      elsif Msg in Data_Out then
         Write (Transport_Endpoint'Class (TE.all), Data_Out (Msg).Out_Buf);

      elsif Msg in Set_Server then
         TE.Server := Set_Server (Msg).Server;
         return Emit (TE.Upper, Msg);

      elsif Msg in Connect_Confirmation then
         return Emit (TE.Upper, Msg);

      elsif Msg in Disconnect_Request then
         Close (Transport_Endpoint'Class (TE.all));
         return Emit
           (TE.Server, ORB.Interface.Unregister_Endpoint'
            (TE => Transport_Endpoint_Access (TE)));

      else
         --  Must not happen.
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

end PolyORB.Transport.Connected;
