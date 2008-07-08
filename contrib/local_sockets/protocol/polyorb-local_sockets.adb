------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . L O C A L _ S O C K E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Streams;
with PolyORB.Initialization;
with PolyORB.Buffers;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Opaque;
with PolyORB.Log;

package body PolyORB.Local_Sockets is
   use PolyORB.Log;
   use PolyORB.Buffers;
   use PolyORB.Opaque;

   package L is new PolyORB.Log.Facility_Log ("polyorb.local_sockets");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   Initialized : Boolean := False;

   Sockets_Array : array (1 .. Channel_Number) of Local_Socket_Access;

   -------------------------
   -- Create_Local_Socket --
   -------------------------

   function Create_Local_Socket return Local_Socket_Access is
      Socket_Ptr : constant Local_Socket_Access := new Local_Socket_Type;

   begin
      pragma Debug (O ("Create local socket : enter"));
      if not Initialized then
         Initialize;
      end if;

      --  Find a free channel

      for J in Sockets_Array'Range loop
         if not Sockets_Array (J).all.Used then
            Sockets_Array (J).Used := True;
            Socket_Ptr.Addr.LPort  := Port (J);

            return Socket_Ptr;
         end if;
      end loop;

      raise Program_Error;

      pragma Debug (O ("Create local socket : leave"));
      return null;
   end Create_Local_Socket;

   ----------
   -- Copy --
   ----------

   function Copy (S : Local_Socket_Type) return Local_Socket_Type is
      Result : Local_Socket_Type;
   begin
      Channel (Result) := Channel (S);
      Result.Addr      := S.Addr;
      Result.Connecting_Port := S.Connecting_Port;
      Result.Selector  := S.Selector;
      return Result;
   end Copy;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket (Addr : Local_Socket_Addr) return Local_Socket_Type is
   begin
      return Sockets_Array (Integer (Addr.LPort)).all;
   end Get_Socket;

   ----------
   -- Read --
   ----------

   procedure Read
     (Socket : Local_Socket_Access;
      Buffer : PolyORB.Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count)
   is
      use Ada.Streams;
      I : Integer :=
         Integer (Sockets_Array (Integer (Socket.Addr.LPort)).Connecting_Port);
   begin
      pragma Assert (Initialized);

      if Sockets_Array (I).Connecting_Only then
         pragma Debug
           (O ("The socket" & I'Img & " is dedicated to accept connections"));
         I := Integer (Sockets_Array (I).Connecting_Port);
         pragma Assert (I /= 0);
      end if;

      pragma Debug (O ("Read on the socket number :" & I'Img));
      PTM.Enter (Sockets_Array (I).My_Mutex);

         PTCV.Wait
           (Sockets_Array (I).Not_Empty_Condition,
            Sockets_Array (I).My_Mutex);

      declare
         Data        : Opaque_Pointer;
         Buffer_Size : constant Stream_Element_Offset := Length (Buffer);
      begin
         Rewind (Sockets_Array (I).Shared);
         Set_Initial_Position (Sockets_Array (I).Shared, 0);

         Extract_Data
           (Sockets_Array (I).Shared,
            Data,
            Size + Length (Buffer),
            True);

         Release_Contents (Buffer.all);
         Initialize_Buffer
           (Buffer,
            Buffer_Size + Size,
            Data,
            Little_Endian,
            0);
         Set_CDR_Position (Buffer, Buffer_Size);

         if Length (Sockets_Array (I).Shared) = Length (Buffer) then
            pragma Debug (O ("no more data"));
            PTCV.Signal (Sockets_Array (I).Not_Full_Condition);
            Sockets_Array (I).Empty_Buffer := True;
            Sockets_Array (I).Full_Buffer  := False;
            Release_Contents (Sockets_Array (I).Shared.all);
            Sockets_Array (I).Shared := null;
         end if;
         if Sockets_Array (I).Selector /= null then
            PTM.Enter (Sockets_Array (I).Selector.Mutex);
            PTCV.Signal (Sockets_Array (I).Selector.CV);
            PTM.Leave (Sockets_Array (I).Selector.Mutex);
         end if;
      end;

      PTM.Leave (Sockets_Array (I).My_Mutex);
   end Read;

   -------------------
   -- Create_Socket --
   -------------------

   procedure Create_Socket (Socket : in out Local_Socket_Type) is
      S_Access : constant  Local_Socket_Access := Create_Local_Socket;
   begin
      Socket := S_Access.all;
   end Create_Socket;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Server  : Local_Socket_Type;
      Socket  : out Local_Socket_Type;
      Address : out Local_Socket_Addr)
   is
      Index : constant Integer := Integer (Server.Addr.LPort);

   begin
      PTM.Enter (Sockets_Array (Index).My_Mutex);
      if Sockets_Array (Index).Connecting_Port = 0 then

         Create_Socket (Socket);
         Sockets_Array (Index).Connecting_Port := Socket.Addr.LPort;
      end if;

      Socket.Connecting_Port := Sockets_Array (Index).Connecting_Port;

      Socket.Addr.LPort := Server.Addr.LPort;
      Socket.Selector := Sockets_Array (Index).Selector;
      Address := Address_Of (Socket);
      PTCV.Signal (Sockets_Array (Index).Is_Connected);
      PTM.Leave (Sockets_Array (Index).My_Mutex);
      if Sockets_Array (Index).Selector /= null then
         PTM.Enter (Sockets_Array (Index).Selector.Mutex);
         PTCV.Signal (Sockets_Array (Index).Selector.CV);
         PTM.Leave (Sockets_Array (Index).Selector.Mutex);
      end if;
   end Accept_Socket;

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket
     (Socket : Local_Socket_Type;
      Server : in out Local_Socket_Addr)
   is
      Index : constant Integer := Integer (Server.LPort);

   begin
      pragma Assert (Index /= 0);

      --  XXX should ensure that the server is already set up !

      PTM.Enter (Sockets_Array (Index).My_Mutex);

      Sockets_Array (Integer (Socket.Addr.LPort)).Connecting_Port :=
        Sockets_Array (Index).Connecting_Port;
      PTCV.Signal (Sockets_Array (Index).Is_Connected);

      pragma Debug
        (O ("THE SOCKET IS CONNECTED NOW" &
             Sockets_Array (Index).Connecting_Port'Img));

      pragma Assert (Sockets_Array (Index).Connecting_Port /= 0);

      Sockets_Array (Integer (Server.LPort)).Selector  :=
        Sockets_Array (Index).Selector;

      if Sockets_Array (Index).Selector /= null then
         PTM.Enter (Sockets_Array (Index).Selector.Mutex);
         PTCV.Signal (Sockets_Array (Index).Selector.CV);
         PTM.Leave (Sockets_Array (Index).Selector.Mutex);
      end if;
   end Connect_Socket;

   -------------------
   -- Listen_Socket --
   -------------------

   procedure Listen_Socket (Socket : Local_Socket_Type) is
      Index : constant Integer := Integer (Socket.Addr.LPort);
   begin
      Sockets_Array (Index).Connecting_Only := True;
   end Listen_Socket;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   function Is_Data_Available
     (S    : Local_Socket_Access;
      N    : Natural)
      return Boolean
   is
   begin
      if S /= null and then
        Sockets_Array (Integer (S.all.Connecting_Port)).Shared /= null then
         return Integer
           (Length
            (Sockets_Array (Integer (S.all.Connecting_Port)).Shared)) >= N;
      end if;

      return False;
   end Is_Data_Available;

   -----------
   -- Write --
   -----------

   procedure Write
     (Socket : in Local_Socket_Access;
      Buffer : in PolyORB.Buffers.Buffer_Access)
   is
      I       : Integer := Integer (Socket.Addr.LPort);
   begin
      if not Initialized then
         Initialize;
      end if;
      I := Integer (Sockets_Array (I).Connecting_Port);

      PTM.Enter (Sockets_Array (I).My_Mutex);

      while Sockets_Array (I).Full_Buffer loop  --  no place in the buffer
         PTCV.Wait
           (Sockets_Array (I).Not_Full_Condition,
            Sockets_Array (I).My_Mutex);
      end loop;

      --  Put the data

      Sockets_Array (I).Shared := Copy (Buffer);

      --  The buffer is no longer full

      Sockets_Array (I).Empty_Buffer := False;
      Sockets_Array (I).Full_Buffer  := True;
      PTCV.Signal (Sockets_Array (I).Not_Empty_Condition);
      PTM.Leave (Sockets_Array (I).My_Mutex);
      pragma Debug (O ("written On : " & I'Img));

      if Sockets_Array (I).Selector /= null then
         pragma Debug (O ("Signal an event to the selctor"));
         PTM.Enter (Sockets_Array (I).Selector.Mutex);
         PTCV.Signal (Sockets_Array (I).Selector.CV);
         PTM.Leave (Sockets_Array (I).Selector.Mutex);
      else
         pragma Debug (O ("The socket" & I'Img & " is not controlled!!!"));
         null;
      end if;
   end Write;

   ------------
   --  Close --
   ------------

   procedure Close (Socket : in out Local_Socket_Access) is
   begin
      PTM.Enter (Sockets_Array (Integer (Socket.Addr.LPort)).My_Mutex);
      Socket.Used     := False;
      Socket.Selector := null;
      Release_Contents (Channel (Socket.all).Shared.all);
      PTM.Leave (Sockets_Array (Integer (Socket.Addr.LPort)).My_Mutex);
   end Close;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (S : Local_Socket_Type) return Local_Socket_Addr is
   begin
      return S.Addr;
   end Address_Of;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (S    : in out Local_Socket_Type;
      Addr : Local_Socket_Addr)
   is
   begin
      S.Addr := Addr;
   end Set_Address;

   -----------
   -- Image --
   -----------

   function Image (S : Local_Socket_Type) return String is
   begin
      return ("SOCKET, Port = " &
              Integer (S.Addr.LPort)'Img &
              "Connecting_port = " &
              Integer (S.Connecting_Port)'Img);
   end Image;

   -----------------
   -- GlobalImage --
   -----------------

   procedure GlobalImage is
   begin
      for I in Sockets_Array'Range loop
         pragma Debug (O (Image (Sockets_Array (I).all)));
         null;
      end loop;
   end GlobalImage;

   -----------
   -- Empty --
   -----------

   procedure Empty (Item : in out Local_Socket_Set_Type) is
   begin
      Deallocate (Item);
   end Empty;

   ---------
   -- Set --
   ---------

   procedure Set
     (Item   : in out Local_Socket_Set_Type;
      Socket : in Local_Socket_Access)
   is
   begin
      pragma Debug (O ("Set : Enter"));
      if Socket = null then
         raise Program_Error;
      end if;
      Append (Item, Socket);
      pragma Debug (O ("Set : Leave"));
   end Set;

   ------------
   -- Is_Set --
   ------------

   function Is_Set
     (Item   : Local_Socket_Set_Type;
      Socket : Local_Socket_Access)
      return   Boolean
   is
      use Local_Socket_Lists;
      It : Iterator := First (Item);
   begin
      while not Last (It) loop
         if Value (It).all.all = Socket.all then
            return True;
         end if;
         Next (It);
      end loop;

      return False;
   end Is_Set;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Item   : in out Local_Socket_Set_Type;
      Socket : in Local_Socket_Access)
   is
   begin
      Remove (Item, Socket);
   end Clear;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : Local_Socket_Set_Type;
      Target : in out Local_Socket_Set_Type)
   is
   begin
      Target := Duplicate (Source);
   end Copy;

   ---------------------
   -- Create_Selector --
   ---------------------

   procedure Create_Selector (Selector : out Local_Selector_Type) is
      Result : Local_Selector_Type;
   begin
      PTM.Create (Result.Mutex);
      PTCV.Create (Result.CV);
      Selector := Result;
      pragma Debug (O ("Selector created"));
   end Create_Selector;

   --------------------
   -- Close_Selector --
   --------------------

   procedure Close_Selector (Selector : in out Local_Selector_Type) is
   begin
      Empty (Selector.Set);
      PTM.Destroy (Selector.Mutex);
      PTCV.Destroy (Selector.CV);
   end Close_Selector;

   --------------------
   -- Check_Selector --
   --------------------

   procedure Check_Selector
     (Selector     : in out Local_Selector_Type;
      R_Socket_Set : in out Local_Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : in Standard.Duration := Forever)
   is
      pragma Unreferenced (Timeout);
      use Local_Socket_Lists;
      use type Ada.Streams.Stream_Element_Offset;
      --  XXX need some optimizations

      It : Local_Socket_Lists.Iterator := First (R_Socket_Set);

   begin
      pragma Debug (O ("Check_Selector : Enter"));
      pragma Debug
        (O
            ("going to listen on " &
             Length (R_Socket_Set)'Img &
             " socket(s)"));
      It := First (R_Socket_Set);
      while not Local_Socket_Lists.Last (It) loop

         Local_Socket_Lists.Next (It);
      end loop;

      PTM.Enter (Selector.Mutex);
      It := First (R_Socket_Set);
      while not Local_Socket_Lists.Last (It) loop
         if Value (It).all = null then
            raise Program_Error;
         end if;
         if Value (It).all.Shared /= null
           and then Length (Value (It).all.Shared) > 0
         then

            Status := Completed;
            PTM.Leave (Selector.Mutex);
            return;
         end if;
         Local_Socket_Lists.Next (It);
      end loop;

      It := First (R_Socket_Set);

      while not Local_Socket_Lists.Last (It) loop

         Sockets_Array (Integer (Address_Of (Value (It).all.all).LPort)).
           Selector
            := Selector'Unrestricted_Access;
         Local_Socket_Lists.Next (It);

      end loop;

      PTCV.Wait (Selector.CV, Selector.Mutex);
      pragma Debug (O ("Selector : An Event is Detected"));

      It := First (R_Socket_Set);
      while not Local_Socket_Lists.Last (It) loop
         Value (It).all.Selector := null;
         Local_Socket_Lists.Next (It);
      end loop;

      PTM.Leave (Selector.Mutex);
      pragma Debug (O ("Check_Selector : Leave"));
   end Check_Selector;

   --------------------
   -- Abort_Selector --
   --------------------

   procedure Abort_Selector (Selector : in Local_Selector_Type) is
   begin
      pragma Debug (O ("about to abort Selector "));
      PTM.Enter (Selector.Mutex);
      PTCV.Signal (Selector.CV);
      PTM.Leave (Selector.Mutex);
   end Abort_Selector;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         for I in  Sockets_Array'Range loop
            Sockets_Array (I)            := new Local_Socket_Type;
            Sockets_Array (I).Addr.LPort := Port (I);
            PTM.Create (Sockets_Array (I).My_Mutex);
            PTCV.Create (Sockets_Array (I).Not_Full_Condition);
            PTCV.Create (Sockets_Array (I).Not_Empty_Condition);
            PTCV.Create (Sockets_Array (I).Is_Connected);
         end loop;

      end if;
      Initialized := True;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
     (Name      => +"local_sockets",
      Conflicts => Empty,
      Depends   => +"tasking.mutexes" &
                   "tasking.threads" &
                   "tasking.condition_variables",
      Provides  => Empty,
      Implicit  => False,
      Init      => Initialize'Access,
      Shutdown  => null));
end PolyORB.Local_Sockets;
