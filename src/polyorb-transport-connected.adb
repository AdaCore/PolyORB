------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . T R A N S P O R T . C O N N E C T E D           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  Abstract connected transport service access points and transport
--  endpoints.

--  $Id$

with PolyORB.Binding_Objects;
with PolyORB.Components;
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

      New_TE : Transport_Endpoint_Access;
   begin
      pragma Debug (O ("Handle_Event: Connected TAP AES"));

      Accept_Connection
        (Connected_Transport_Access_Point'Class (H.TAP.all), New_TE);
      --  Create transport endpoint.

      Binding_Objects.Setup_Binding_Object
        (The_ORB => H.ORB,
         TE      => New_TE,
         FFC     => H.Filter_Factory_Chain.all,
         Role    => ORB.Server,
         BO_Ref  => New_TE.Dependent_Binding_Object);
      --  Setup binding object.

      Emit_No_Reply
        (Component_Access (H.ORB),
         Monitor_Access_Point'(TAP => H.TAP));
      --  Continue monitoring the TAP's AES.
   end Handle_Event;

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (TE  : access Connected_Transport_Endpoint;
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

      elsif Msg in Data_Expected then
         declare
            DE : Data_Expected renames Data_Expected (Msg);
         begin
            pragma Assert (DE.In_Buf /= null);
            TE.In_Buf := DE.In_Buf;
            TE.Max    := DE.Max;
         end;

         if Is_Data_Available (Connected_Transport_Endpoint'Class (TE.all),
                               Natural (TE.Max))
         then
            pragma Debug (O ("TE has " & TE.Max'Img
                             & " bytes waiting, will read data"));

            return Handle_Message
              (TE, Data_Indication'(Data_Amount => TE.Max));
         else
            pragma Debug (O ("No enough data on TE, ORB will monitor TE"));

            return Emit
              (TE.Server, ORB.Interface.Monitor_Endpoint'
               (TE => Transport_Endpoint_Access (TE)));
         end if;


      elsif Msg in Data_Indication then
         pragma Debug (O ("Data received"));

         declare
            use type Ada.Streams.Stream_Element_Count;
            Size : Ada.Streams.Stream_Element_Count := TE.Max;

            Error : Error_Container;
         begin

            if TE.In_Buf = null then
               O ("Unexpected data (no buffer)");

               Throw (Error, Comm_Failure_E,
                      System_Exception_Members'
                      (Minor => 0, Completed => Completed_Maybe));
               --  Notify the ORB that the socket was disconnected.

            else
               Read
                 (Transport_Endpoint'Class (TE.all), TE.In_Buf, Size, Error);
            end if;

            if not Is_Error (Error) and then Size /= 0 then
               return Emit (TE.Upper, Data_Indication'
                            (Data_Amount => Size));
            else
               return Filter_Error'(Error => Error);
            end if;
         end;

      elsif Msg in Data_Out then
         declare
            Error : Error_Container;
         begin
            Write (Transport_Endpoint'Class (TE.all),
                   Data_Out (Msg).Out_Buf, Error);

            if Is_Error (Error) then
               return Filter_Error'(Error => Error);
            end if;
         end;

      elsif Msg in Set_Server then
         TE.Server := Set_Server (Msg).Server;
         return Emit (TE.Upper, Msg);

      elsif Msg in Connect_Confirmation
        or else Msg in Disconnect_Indication
      then
         return Emit (TE.Upper, Msg);

      elsif Msg in Disconnect_Request then
         Close (Transport_Endpoint'Class (TE.all)'Access);
      else
         --  Must not happen.
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

end PolyORB.Transport.Connected;
