--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Droopi.Filters.Interface;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.Transport is

   use Droopi.Filters.Interface;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.transport");
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

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg : Components.Message'Class)
     return Components.Message'Class
   is
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

end Droopi.Transport;
