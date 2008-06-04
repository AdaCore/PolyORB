------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.DATAGRAM.SOCKETS_OUT                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

--  Datagram Socket End Point to send data to network

with Ada.Exceptions;

with PolyORB.Log;

package body PolyORB.Transport.Datagram.Sockets_Out is

   use Ada.Streams;

   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.datagram.sockets_out");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE : access Socket_Out_Endpoint)
     return Asynch_Ev_Source_Access
   is
      pragma Unreferenced (TE);

   begin
      return null;
      --  No event source for datagram out endpoint
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
      Error  :    out Errors.Error_Container)
   is
      pragma Unreferenced (TE, Buffer, Size);

   begin
      raise Program_Error;
      --  Should never happen
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
   (TE     : in out Socket_Out_Endpoint;
    Buffer :        Buffers.Buffer_Access;
    Error  :    out Errors.Error_Container)
   is
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Data : constant Stream_Element_Array :=
        To_Stream_Element_Array (Buffer);
      --  ??? Possible stack overflow if Buffer contains too much data

      Last : Stream_Element_Offset;

   begin
      pragma Debug (O ("Write: enter"));
      pragma Debug (O ("Send to : " & Image (TE.Addr)));

      begin
         PolyORB.Sockets.Send_Socket (TE.Socket, Data, Last, TE.Addr);
      exception
         when E : Sockets.Socket_Error =>
            O ("send failed: " & Ada.Exceptions.Exception_Information (E),
               Notice);
            Throw (Error, Comm_Failure_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw (Error, Unknown_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));
      end;
      pragma Debug (O ("Write: leave"));
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Socket_Out_Endpoint) is
   begin
      pragma Debug (O ("Closing UDP socket"));
      if TE.Closed then
         return;
      end if;

      Enter (TE.Mutex);
      TE.Socket := No_Socket;
      Leave (TE.Mutex);
      PolyORB.Transport.Datagram.Close
        (Datagram_Transport_Endpoint (TE.all)'Access);
   end Close;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (TE : in out Socket_Out_Endpoint) is
   begin
      Destroy (TE.Mutex);
      Datagram.Destroy (Datagram_Transport_Endpoint (TE));
   end Destroy;

end PolyORB.Transport.Datagram.Sockets_Out;
