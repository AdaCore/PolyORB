------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TRANSPORT.DATAGRAM.SOCKETS_IN                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Datagram Socket Access Point and End Point to recieve data from network

with System.Storage_Elements;

with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Log;

package body PolyORB.Transport.Datagram.Sockets_In is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.datagram.sockets_in");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------
   -- Init_Socket_In --
   --------------------

   procedure Init_Socket_In
     (SAP         : in out Socket_In_Access_Point;
      Socket      : in     Socket_Type;
      Address     : in out Sock_Addr_Type;
      Update_Addr :        Boolean := True)
   is
   begin
      Bind_Socket (Socket, Address);

      SAP.Socket := Socket;

      if Update_Addr then
         SAP.Addr := Get_Socket_Name (Socket);
         if SAP.Addr.Addr = Any_Inet_Addr then
            SAP.Addr.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
         end if;
         Address := SAP.Addr;
      else
         SAP.Addr := Address;
      end if;
   end Init_Socket_In;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP : access Socket_In_Access_Point)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TAP.Socket);

   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            TAP.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (SAP : Socket_In_Access_Point) return Sock_Addr_Type is
   begin
      return SAP.Addr;
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE   : in out Socket_In_Endpoint;
      S    :        Socket_Type;
      Addr :        Sock_Addr_Type) is
   begin
      TE.Addr := Addr;
      TE.Socket := S;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE : access Socket_In_Endpoint)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TE.Socket);

   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            TE.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   ----------
   -- Read --
   ----------

   procedure Read
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  :    out Errors.Error_Container)
   is
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Data_Received : Stream_Element_Count;
      Request : Request_Type (N_Bytes_To_Read);

      procedure Receive_Socket (V : access Iovec);
      --  Lowlevel socket receive

      procedure Receive_Socket (V : access Iovec) is
         Count : Ada.Streams.Stream_Element_Count;
         Vecs  : Vector_Type (1 .. 1);
         pragma Import (Ada, Vecs);
         for Vecs'Address use V.all'Address;
      begin
         PolyORB.Sockets.Receive_Vector (TE.Socket, Vecs, Count);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Receive_Socket;

      procedure Receive_Buffer is new PolyORB.Buffers.Receive_Buffer
        (Receive_Socket);

   begin
      --  Must read all data in one call from datagram socket.
      --  Amount read is often greater than asked amount.

      Control_Socket (TE.Socket, Request);
      Size := Stream_Element_Offset (Request.Size);
      pragma Debug (O ("To read :" & Size'Img));
      begin
         Receive_Buffer (Buffer, Size, Data_Received);
      exception
         when PolyORB.Sockets.Socket_Error =>
            Throw (Error, Comm_Failure_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw (Error, Unknown_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));
      end;

      pragma Assert (Data_Received /= 0);
      pragma Debug (O (Data_Received'Img & " byte(s) received"));
      pragma Assert (Data_Received <= Size);

      Size := Data_Received;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
   is
   begin
      raise Program_Error;
      --  Should never happen
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Socket_In_Endpoint) is
   begin
      pragma Debug (O ("Closing UDP socket"));
      if TE.Closed then
         return;
      end if;

      PolyORB.Transport.Datagram.Close
        (Datagram_Transport_Endpoint (TE.all)'Access);
      TE.Socket := No_Socket;
   end Close;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (TAP : access Socket_In_Access_Point)
     return Datagram_Transport_Endpoint_Access
   is
      TE : constant Datagram_Transport_Endpoint_Access
        := new Socket_In_Endpoint;

   begin
      pragma Debug (O ("Create Endpoint for UDP socket"));

      Socket_In_Endpoint (TE.all).Addr := TAP.Addr;
      Socket_In_Endpoint (TE.all).Socket := TAP.Socket;
      return TE;
   end Create_Endpoint;

end PolyORB.Transport.Datagram.Sockets_In;
