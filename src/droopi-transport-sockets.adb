--  Socket implementation of transport service access points
--  and communication endpoints.

--  $Id$

with Droopi.Asynch_Ev.Sockets;
with Droopi.Log;

package body Droopi.Transport.Sockets is

   use Droopi.Asynch_Ev.Sockets;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.transport.sockets");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Create_Transport_Access_Point
     (Socket : Socket_Type)
     return Transport_Access_Point_Access
   is
      Result : constant Transport_Access_Point_Access
        := new Socket_Access_Point;
   begin
      Socket_Access_Point (Result.all).Socket := Socket;
      return Result;
   end Create_Transport_Access_Point;

   function Create_Event_Source
     (TAP : Socket_Access_Point)
     return Asynch_Ev_Source_Access is
   begin
      return Create_Event_Source (TAP.Socket);
   end Create_Event_Source;

   procedure Accept_Connection
     (TAP : Socket_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_TE : constant Transport_Endpoint_Access
        := new Socket_Endpoint;
   begin
      Accept_Socket
        (Server  => TAP.Socket,
         Socket  => Socket_Endpoint (New_TE.all).Socket,
         Address => Socket_Endpoint (New_TE.all).Addr);
      TE := New_TE;
   end Accept_Connection;

   function Create_Event_Source
     (TE : Socket_Endpoint)
     return Asynch_Ev_Source_Access is
   begin
      return Create_Event_Source (TE.Socket);
   end Create_Event_Source;

   procedure Read
     (TE     : Socket_Endpoint;
      Buffer : Buffer_Access;
      Size   : in out Stream_Element_Count)
   is
      Data_Received : Stream_Element_Count;
   begin
      Droopi.Buffers.Receive_Buffer
        (Buffer, TE.Socket, Size, Data_Received);

      if Data_Received = 0 then
         O ("Connection closed on fd " & Image (TE.Socket));

         raise Connection_Closed;
      end if;
      pragma Assert (Data_Received <= Size);
      Size := Data_Received;
   end Read;

   procedure Write
     (TE     : Socket_Endpoint;
      Buffer : Buffer_Access)
   is
   begin
      Droopi.Buffers.Send_Buffer (Buffer, TE.Socket);
   end Write;

   procedure Close (TE : Socket_Endpoint) is
   begin
      Close_Socket (TE.Socket);
   end Close;

end Droopi.Transport.Sockets;
