--  Abstract transport service access points and
--  communication endpoints.

--  $Id$

with Droopi.Filters;
with Droopi.Log;

package body Droopi.Transport is
   
   use Droopi.Filters.Data_Units;
   use Droopi.Log;
   
   package L is new Droopi.Log.Facility_Log ("droopi.transport");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   procedure Connect_Upper
     (TE    : access Transport_Endpoint;
      Upper : Components.Component_Access) is
   begin
      Components.Connect (TE.Upper, Upper);
   end Connect_Upper;

   function Handle_Message
     (TE  : access Transport_Endpoint;
      Msg : Components.Message'Class)
     return Boolean is
   begin
      if Msg in Connect_Indication then
         Emit (TE.Upper, Msg);
	 
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

            Emit (TE.Upper, Msg);
         end;
      elsif Msg in Data_Out then
         Write (Transport_Endpoint'Class (TE.all), Data_Out (Msg).Out_Buf);
      else
         --  Must not happen.
         pragma Assert (False);
         null;
      end if;
      return True;
   end Handle_Message;

end Droopi.Transport;
