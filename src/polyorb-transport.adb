------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . T R A N S P O R T                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Transport is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.transport");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Notepad_Of (TAP : Transport_Access_Point_Access)
     return Annotations.Notepad_Access is
   begin
      return TAP.Notepad'Access;
   end Notepad_Of;

   function Handle_Message
     (TAP : access Transport_Access_Point;
      Msg : Components.Message'Class)
     return Components.Message'Class is
   begin
      raise Unhandled_Message;
      --  Small is beautiful.

      return Handle_Message (TAP, Msg);
      --  Keep the compiler happy.
   end Handle_Message;

   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper : Components.Component_Access) is
   begin
      Components.Connect (TE.Upper, Upper);
   end Connect_Upper;

   procedure Destroy (TE : in out Transport_Endpoint_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Transport_Endpoint'Class, Transport_Endpoint_Access);
   begin
      Destroy (TE.Upper);
      Free (TE);
   end Destroy;

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class
   is
      use PolyORB.Buffers;

      Nothing : Components.Null_Message;
   begin
      if Msg in Connect_Indication then
         return Emit (TE.Upper, Msg);
      elsif Msg in Data_Expected then
         declare
            DE : Data_Expected renames Data_Expected (Msg);
         begin
            TE.In_Buf := DE.In_Buf;
            TE.Max    := DE.Max;
         end;

      elsif Msg in Data_Indication then
         pragma Debug (O ("Data received"));

         if TE.In_Buf = null then
            O ("Unexpected data!");

            raise Connection_Closed;
            --  Notify the ORB that the socket was disconnected.
            --  XXX what to do? who closes what?
         end if;

         declare
            Size : Stream_Element_Count := TE.Max;
         begin
            Read (Transport_Endpoint'Class (TE.all), TE.In_Buf, Size);

            if Size = 0 then
               O ("Connection closed.");

               raise Connection_Closed;
               --  Notify the ORB that the socket was disconnected.
               --  XXX what to do? who closes what?
            end if;

            return Emit (TE.Upper, Msg);
         end;
      elsif Msg in Data_Out then
         Write (Transport_Endpoint'Class (TE.all), Data_Out (Msg).Out_Buf);
      elsif Msg in Connect_Confirmation then
         return Emit (TE.Upper, Msg);
      else
         --  Must not happen.
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

end PolyORB.Transport;
