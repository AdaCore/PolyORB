------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.DATAGRAM.SOCKETS_OUT                   --
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

--  Datagram Socket End Point to send data to network

--  $Id$

with PolyORB.Log;

package body PolyORB.Transport.Datagram.Sockets_Out is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.datagram.sockets_out");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE : Socket_Out_Endpoint)
     return Asynch_Ev_Source_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (TE);
      pragma Warnings (On);
   begin
      pragma Debug (O ("Return null event source for datagram out end point"));
      return null;
   end Create_Event_Source;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE   : in out Socket_Out_Endpoint;
      S    :        Socket_Type;
      Addr :        Sock_Addr_Type)
   is
   begin
      TE.Socket := S;
      TE.Addr := Addr;
      Create (TE.Mutex);
   end Create;

   -----------
   -- Read --
   -----------

   procedure Read
     (TE     : in out Socket_Out_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  :    out Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (TE, Buffer, Size);
      pragma Warnings (On);
   begin
      raise End_Point_Write_Only;
      --  Never happens.
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
   (TE     : in out Socket_Out_Endpoint;
    Buffer :        Buffers.Buffer_Access;
    Error  :    out Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;
   begin
      pragma Debug (O ("Write: enter"));
      pragma Debug (O ("Send to : " & Image (TE.Addr)));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      begin
         PolyORB.Buffers.Send_Buffer (Buffer, TE.Socket, TE.Addr);
      exception
         when PolyORB.Sockets.Socket_Error =>
            Throw (Error, Comm_Failure_E, System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw (Error, Unknown_E, System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));
      end;
      Leave (TE.Mutex);
      pragma Debug (O ("Write: leave"));
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : in out Socket_Out_Endpoint) is
   begin
      pragma Debug (O ("Closing UDP socket"));
      TE.Socket := No_Socket;
      Destroy (TE.Mutex);
   end Close;

end PolyORB.Transport.Datagram.Sockets_Out;
