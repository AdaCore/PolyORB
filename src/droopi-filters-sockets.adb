--  A filter that wraps a Socket.

--  $Id$

with Droopi.Log;
with Droopi.Sockets;

package body Droopi.Filters.Sockets is

   use Droopi.Log;
   use Droopi.Sockets;

   package L is new Droopi.Log.Facility_Log ("droopi.filters.sockets");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   procedure Create (Sock : in out Active_Socket) is
   begin
      pragma Assert (Sock.Channel = null);
      --  Must not create a filter for a socket that already has one.

      Sock.Channel := new Socket_Filter;
      Socket_Filter (Sock.Channel.all).Sock := Sock;
   end Create;

   function Handle_Message
     (SF : access Socket_Filter;
      S  : Components.Message'Class)
     return Boolean
   is
      use Filters.Data_Units;
   begin
      pragma Assert (SF.Upper /= null);

      if S in Connect_Indication then
         Handle_Message (SF.Upper, S);
      elsif S in Data_Expected then
         declare
            DE : Data_Expected renames Data_Expected (S);
         begin
            SF.In_Buf := DE.In_Buf;
            SF.Max := DE.Max;
         end;
      elsif S in Data_Indication then
         pragma Debug (O ("Data received on socket "
                          & Image (SF.Sock.Socket)));

         if SF.In_Buf = null then
            O ("Unexpected data received on fd" & Image (SF.Sock.Socket));

            raise Connection_Closed;
            --  Notify the ORB that the socket was disconnected.
         end if;

         declare
            Data_Received : Stream_Element_Count;
         begin
            Droopi.Buffers.Receive_Buffer
              (SF.In_Buf, SF.Sock.Socket, SF.Max, Data_Received);

            if Data_Received = 0 then
               O ("Connection closed on fd " & Image (SF.Sock.Socket));

               raise Connection_Closed;
               --  Notify the ORB that the socket was disconnected.
            end if;

            Handle_Message (SF.Upper, S);
         end;
      elsif S in Data_Out then
         Droopi.Buffers.Send_Buffer (Data_Out (S).Out_Buf, SF.Sock.Socket);
      else
         --  Must not happen.
         pragma Assert (False);
         null;
      end if;
      return True;
   end Handle_Message;

end Droopi.Filters.Sockets;
