------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.TRANSPORT.CONNECTED.LOCAL_SOCKETS                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Asynch_Ev.Local_Sockets;
with PolyORB.Log;
with PolyORB.Local_Sockets;
package body PolyORB.Transport.Connected.Local_Sockets is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Local_Sockets;
   use PolyORB.Log;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Local_Sockets;
   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.ls");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (TAP : Local_Socket_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket  : Local_Socket_Type;
      New_Address : Local_Socket_Addr;
   begin
      pragma Debug (O ("Accept_Connection: enter"));
      TE := new Local_Socket_Endpoint;

      Accept_Socket
        (Server  => TAP.Socket.all,
         Socket  => New_Socket,
         Address => New_Address);

      Create (Local_Socket_Endpoint (TE.all), New_Socket);

      --  Local_Socket_Endpoint(TE.all).Socket := TAP.Socket;

      --  Create (Local_Socket_Endpoint (TE.all),
      --  Local_Socket_Endpoint(TE.all).Socket.all );

      --  Local_Socket_Endpoint(TE.all).Socket.addr := Address_Of (TAP.Socket);
      pragma Debug (O ("Accept_Connection: leave"));
   end Accept_Connection;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (SAP  : Local_Socket_Access_Point)
      return Local_Socket_Addr
   is
   begin
      return Address_Of (SAP.Socket.all);
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (SAP     : in out Local_Socket_Access_Point;
      Socket  : Local_Socket_Type;
      Address : in out Local_Socket_Addr)
   is
   begin
      pragma Debug (O ("Create :enter"));
      Listen_Socket (Socket);
      SAP.Socket     := new Local_Socket_Type;
      SAP.Socket.all := Socket;
      Address        := Address_Of (SAP.Socket.all);

      --  Listen_Socket (Socket);
      --  SAP.Socket.all := Copy (Socket);
      --  Address := Address_Of (SAP.Socket);

      pragma Debug
        (O ("Create: SAP.Socket.Port" & Address_Of (SAP.Socket.all).
            LPort'Img));
      pragma Debug (O ("Create :leave"));

   end Create;

   -----------
   -- Image --
   -----------

   function Image (TAP : Local_Socket_Access_Point) return String is
   begin
      return "TAP : Addr = " &
             TAP.Addr.LPort'Img &
             "Socket" &
             PolyORB.Local_Sockets.Address_Of (TAP.Socket.all).LPort'Img;
   end Image;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP  : access Local_Socket_Access_Point)
      return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access :=
         Create_Event_Source (TAP.Socket);
   begin
      Set_Note
        (Notepad_Of (Ev_Src).all,
         AES_Note'(Annotations.Note with Handler => TAP.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE : in out Local_Socket_Endpoint;
      S  : Local_Socket_Type)
   is
   begin
      TE.Socket := new Local_Socket_Type'(S);
      Create (TE.Mutex);
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE   : access Local_Socket_Endpoint)
      return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access :=
         Create_Event_Source (TE.Socket);
   begin
      Set_Note
        (Notepad_Of (Ev_Src).all,
         AES_Note'(Annotations.Note with Handler => TE.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   function Is_Data_Available
     (TE   : Local_Socket_Endpoint;
      N    : Natural)
      return Boolean
   is
   begin
      pragma Debug (O ("Is data available : enter "));
      return Is_Data_Available (TE.Socket, N);

   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   procedure Read
     (TE     : in out Local_Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  : out Errors.Error_Container)
   is
      use PolyORB.Errors;

   begin
      --  Must read all data in one call

      pragma Debug (O ("Wanted:" & Size'Img));

      PolyORB.Local_Sockets.Read (TE.Socket, Buffer, Size);
      pragma Debug (O ("Got:" & PolyORB.Buffers.Length (Buffer)'Img));

   exception
      when others =>
         Throw
           (Error,
            Unknown_E,
            System_Exception_Members'
           (Minor     => 0,
            Completed => Completed_Maybe));
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (TE     : in out Local_Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container)
   is
      use PolyORB.Errors;
   begin

      PolyORB.Local_Sockets.Write (TE.Socket, Buffer);
      pragma Debug (O ("Written:" & PolyORB.Buffers.Length (Buffer)'Img));

      --   pragma Assert (Size = PolyORB.Buffers.Length (Buffer));

   exception
      when others =>
         Throw
           (Error,
            Unknown_E,
            System_Exception_Members'
           (Minor     => 0,
            Completed => Completed_Maybe));

   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Local_Socket_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;
      TE.Closed := True;
      Close (TE.Socket);
      TE.Socket := null;
   end Close;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (TE : in out Local_Socket_Endpoint) is
   begin
      Connected.Destroy (Connected_Transport_Endpoint (TE));
   end Destroy;

end PolyORB.Transport.Connected.Local_Sockets;
