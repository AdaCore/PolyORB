------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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

--  Socket implementation of transport service access points
--  and communication endpoints.

--  $Id$

with Ada.Exceptions;

with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Log;

package body PolyORB.Transport.Connected.Sockets is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.sockets");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (TAP :     Socket_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket : Socket_Type;
      New_Address : Sock_Addr_Type;
   begin
      TE := new Socket_Endpoint;
      Accept_Socket
        (Server  => TAP.Socket,
         Socket  => New_Socket,
         Address => New_Address);
      Create (Socket_Endpoint (TE.all), New_Socket);
   end Accept_Connection;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (SAP : Socket_Access_Point)
     return Sock_Addr_Type is
   begin
      return SAP.Addr;
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (SAP     : in out Socket_Access_Point;
      Socket  :        Socket_Type;
      Address : in out Sock_Addr_Type) is
   begin
      Bind_Socket (Socket, Address);
      Listen_Socket (Socket);

      SAP.Socket := Socket;

      if Address.Addr = Any_Inet_Addr then

         --  Address is unspecified, choose one IP for the SAP looking
         --  up hostname.

         SAP.Addr.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
         Address := SAP.Addr;

      else

         --  Use specified address IP for SAP

         SAP.Addr := Address;
      end if;

      if SAP.Addr.Port = Any_Port then
         SAP.Addr.Port := Get_Socket_Name (Socket).Port;
      end if;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP : access Socket_Access_Point)
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

   ------------
   -- Create --
   ------------

   procedure Create
     (TE : in out Socket_Endpoint;
      S  :        Socket_Type) is
   begin
      TE.Socket := S;
      Create (TE.Mutex);
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE : access Socket_Endpoint)
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

   -----------------------
   -- Is_Data_Available --
   -----------------------

   function Is_Data_Available
     (TE : Socket_Endpoint;
      N  : Natural)
     return Boolean
   is
      Request : Request_Type (N_Bytes_To_Read);

   begin
      Control_Socket (TE.Socket, Request);

      pragma Debug (O ("Found" & Request.Size'Img & " bytes waiting"));

      return Request.Size >= N;
   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  :    out Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;

      Data_Received : Stream_Element_Count;
   begin
      begin
         PolyORB.Buffers.Receive_Buffer
           (Buffer, TE.Socket, Size, Data_Received);
      exception
         when PolyORB.Sockets.Socket_Error =>
            Throw
              (Error, Comm_Failure_E,
               System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
            return;

         when others =>
            Throw
              (Error, Unknown_E,
               System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
            return;
      end;

      pragma Assert (Data_Received <= Size);
      Size := Data_Received;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;
   begin
      pragma Debug (O ("Write: enter"));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      pragma Debug (O ("TE mutex acquired"));

      begin
         PolyORB.Buffers.Send_Buffer (Buffer, TE.Socket);
      exception
         when PolyORB.Sockets.Socket_Error =>
            Throw
              (Error, Comm_Failure_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw
              (Error, Unknown_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
      end;
      Leave (TE.Mutex);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Socket_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;

      Enter (TE.Mutex);
      begin
         if TE.Socket /= No_Socket then
            pragma Debug (O ("Closing socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            Close_Socket (TE.Socket);
            TE.Socket := No_Socket;
         end if;
         Leave (TE.Mutex);
         PolyORB.Transport.Connected.Close
           (Connected_Transport_Endpoint (TE.all)'Access);
      exception
         when E : others =>
            pragma Debug (O ("Close (Socket_Endpoint): got "
                             & Ada.Exceptions.Exception_Information (E)));
            null;
      end;
   end Close;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (TE : in out Socket_Endpoint) is
   begin
      Destroy (TE.Mutex);
      Connected.Destroy (Connected_Transport_Endpoint (TE));
   end Destroy;

end PolyORB.Transport.Connected.Sockets;
