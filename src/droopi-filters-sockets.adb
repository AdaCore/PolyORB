--  A filter that wraps a Socket.

--  $Id$

with Droopi.Log; use Droopi.Log;
with Droopi.Sockets;

package body Droopi.Filters.Sockets is

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

   procedure Handle_Data_Unit
     (SF : access Socket_Filter;
      S  :  Data_Unit) is
   begin
      pragma Assert (SF.Upper /= null);

      case S.Kind is
         when Connect_Indication =>
            Handle_Data_Unit (SF.Upper, S);

         when Data_Expected =>
            SF.In_Buf := S.In_Buf;
            SF.Max := S.Max;

         when Data_Indication =>
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

               Handle_Data_Unit (SF.Upper, S);
            end;

         when Data_Out =>
            Droopi.Buffers.Send_Buffer (S.Out_Buf, SF.Sock.Socket);

         when others =>
            --  Must not happen.
            pragma Assert (False);
            null;

      end case;
   end Handle_Data_Unit;

end Droopi.Filters.Sockets;
