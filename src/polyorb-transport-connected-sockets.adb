------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
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

--  Socket implementation of transport service access points
--  and communication endpoints.

--  $Id$

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
      SAP.Addr   := Get_Socket_Name (Socket);
      if SAP.Addr.Addr = Any_Inet_Addr then
         SAP.Addr.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
      end if;
      Address := SAP.Addr;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP : Socket_Access_Point)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TAP.Socket);
   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            new Connected_TAP_AES_Event_Handler));
      return Ev_Src;
   end Create_Event_Source;

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (TAP :     Socket_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_TE : constant Transport_Endpoint_Access
        := new Socket_Endpoint;
      --  XXX dynamic allocation
   begin
      Accept_Socket
        (Server  => TAP.Socket,
         Socket  => Socket_Endpoint (New_TE.all).Socket,
         Address => Socket_Endpoint (New_TE.all).Addr);
      Create (Socket_Endpoint (New_TE.all).Mutex);
      TE := New_TE;
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
     (TE : Socket_Endpoint)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TE.Socket);
   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            new Connected_TE_AES_Event_Handler));
      return Ev_Src;
   end Create_Event_Source;

   -----------
   -- Read --
   -----------

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count)
   is
      Data_Received : Stream_Element_Count;
   begin
      PolyORB.Buffers.Receive_Buffer
        (Buffer, TE.Socket, Size, Data_Received);

      if Data_Received = 0 then
         O ("Connection closed on fd " & Image (TE.Socket));
         Close (TE);

         raise Connection_Closed;
      end if;
      pragma Assert (Data_Received <= Size);
      Size := Data_Received;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access) is
   begin
      pragma Debug (O ("Write: enter"));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      pragma Debug (O ("TE mutex acquired"));

      PolyORB.Buffers.Send_Buffer (Buffer, TE.Socket);
      Leave (TE.Mutex);
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : in out Socket_Endpoint) is
   begin
      Enter (TE.Mutex);
      --  Ensure that no-one is holding this mutex, since
      --  we are about to destroy it.
      if TE.Socket /= No_Socket then
         Close_Socket (TE.Socket);
      end if;
      TE.Socket := No_Socket;
      Destroy (TE.Mutex);
   end Close;

end PolyORB.Transport.Connected.Sockets;
