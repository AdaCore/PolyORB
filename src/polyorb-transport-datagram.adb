------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . T R A N S P O R T . D A T A G R A M            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

with Ada.Exceptions;

with PolyORB.Log;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.ORB.Interface;
with PolyORB.Transport.Handlers;

package body PolyORB.Transport.Datagram is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.transport.datagram");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (TAP : access Datagram_Transport_Access_Point)
     return Datagram_Transport_Endpoint_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TAP);
      pragma Warnings (On);
   begin
      pragma Debug (O ("Return null endpoint"));
      return null;
   end Create_Endpoint;

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event
     (H : access Datagram_TAP_AES_Event_Handler)
   is
      use PolyORB.Components;
      use PolyORB.ORB;
      use PolyORB.ORB.Interface;
      use PolyORB.Filters;

      --  Create associated Endpoint
      New_TE : constant Transport_Endpoint_Access
        := Transport_Endpoint_Access
        (Create_Endpoint (Datagram_Transport_Access_Point_Access (H.TAP)));
      New_Filter : Filter_Access;
   begin
      if New_TE /= null then
         pragma Debug (O ("Create and register Endpoint"));
         New_Filter := Create_Filter_Chain
           (H.Filter_Factory_Chain);
         --  Create filter chain

         Register_Endpoint (ORB_Access (H.ORB),
                            New_TE,
                            New_Filter,
                            Server);
         --  Monitor the endpoint
      end if;
   end Handle_Event;

   ------------------
   -- Handle_Event --
   ------------------

   procedure Handle_Event
     (H : access Datagram_TE_AES_Event_Handler)
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
     (TE  : access Datagram_Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Buffers;
      use PolyORB.Components;
      use PolyORB.Exceptions;
      use PolyORB.Filters;
      use PolyORB.Filters.Interface;

      Nothing : Components.Null_Message;
   begin
      if Msg in Connect_Indication then
         return Emit (TE.Upper, Msg);

      elsif Msg in Data_Expected'Class then
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

         declare
            use type Ada.Streams.Stream_Element_Count;
            Size : Ada.Streams.Stream_Element_Count := TE.Max;
            Error : Exceptions.Error_Container;
         begin
            if TE.In_Buf = null then
               O ("Unexpected data (no buffer)");

               Close (Transport_Endpoint'Class (TE.all));

               --  XXX raise Connection_Closed;
               Throw (Error, Comm_Failure_E,
                      System_Exception_Members'
                      (Minor => 0, Completed => Completed_Maybe));
               --  Notify the ORB that the socket is closed.
            else
               Read
                 (Transport_Endpoint'Class (TE.all), TE.In_Buf, Size, Error);
            end if;

            if not Is_Error (Error) then
               pragma Assert (Size > 0);
               return Emit (TE.Upper, Data_Indication'(Data_Amount => Size));
               --  Note: this component guarantees that the upper layers will
               --  only receive Data_Indications with a non-zero Data_Amount.
            else
               return Emit (TE.Upper, Filter_Error'(Error => Error));
            end if;
         end;

      elsif Msg in Data_Out then
         declare
            Error : Exceptions.Error_Container;
         begin
            Write
              (Transport_Endpoint'Class (TE.all),
               Data_Out (Msg).Out_Buf, Error);
            if Exceptions.Is_Error (Error) then
               return Filter_Error'(Error => Error);
            end if;
         end;

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

end PolyORB.Transport.Datagram;
