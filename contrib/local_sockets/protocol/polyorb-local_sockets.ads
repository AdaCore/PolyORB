------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . L O C A L _ S O C K E T S                 --
--                                                                          --
--                                 S p e c                                  --
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

with Ada.Streams;
with PolyORB.Buffers;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Utils.Chained_Lists;

package PolyORB.Local_Sockets is

   --  This package contains some functionalities similar to those of
   --  GNAT.Sockets. The communications are simulated by some
   --  Ravenscar compilant synchronization primitives offered by PolyORB.

   Channel_Number : constant Natural := 10;

   procedure Initialize;

   package PTCV renames PolyORB.Tasking.Condition_Variables;
   package PTM renames PolyORB.Tasking.Mutexes;

   type Port is new Integer range 0 .. Channel_Number;

   type Local_Socket_Addr is record
      LPort : Port;
   end record;

   -----------------------
   -- Local_Socket_Type --
   -----------------------

   type Local_Socket_Type is private;
   type Local_Socket_Access is access Local_Socket_Type;

   function Create_Local_Socket return Local_Socket_Access;

   function Copy (S : Local_Socket_Type) return Local_Socket_Type;

   function Get_Socket (Addr : Local_Socket_Addr) return Local_Socket_Type;

   procedure Read
     (Socket : Local_Socket_Access;
      Buffer : PolyORB.Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count);

   procedure Create_Socket (Socket : in out Local_Socket_Type);

   procedure Accept_Socket
     (Server  : Local_Socket_Type;
      Socket  : out Local_Socket_Type;
      Address : out Local_Socket_Addr);
   --  Extract the first connection request on the queue of pending
   --  connections, creates a new connected socket with mostly the
   --  same properties as Server, and allocates a new socket. The
   --  returned Address is filled in with the address of the
   --  connection.

   procedure Connect_Socket
     (Socket : Local_Socket_Type;
      Server : in out Local_Socket_Addr);
   --  Make a connection to another socket which has the address of
   --  Server.

   procedure Listen_Socket (Socket : Local_Socket_Type);
   --  To accept connections, a socket is first created with
   --  Create_Socket,

   function Is_Data_Available
     (S    : Local_Socket_Access;
      N    : Natural)
      return Boolean;

   procedure Write
     (Socket : in Local_Socket_Access;
      Buffer : in PolyORB.Buffers.Buffer_Access);

   procedure Close (Socket : in out Local_Socket_Access);

   function Address_Of (S : Local_Socket_Type) return Local_Socket_Addr;

   pragma Inline (Address_Of);

   procedure Set_Address
     (S    : in out Local_Socket_Type;
      Addr : Local_Socket_Addr);
   pragma Inline (Set_Address);

   function Image (S : Local_Socket_Type) return String;
   procedure GlobalImage;

   ---------------------------
   -- Local_Socket_Set_Type --
   ---------------------------

   type Local_Socket_Set_Type is limited private;

   procedure Empty (Item : in out Local_Socket_Set_Type);
   --  Remove all Sockets from Item and deallocate internal data

   procedure Set
     (Item   : in out Local_Socket_Set_Type;
      Socket : in Local_Socket_Access);
   --  Insert Socket into Item

   function Is_Set
     (Item   : Local_Socket_Set_Type;
      Socket : Local_Socket_Access)
      return   Boolean;
   --  Return True iff Socket is present in Item

   procedure Clear
     (Item   : in out Local_Socket_Set_Type;
      Socket : in Local_Socket_Access);
   --  Remove Socket from Item

   procedure Copy
     (Source : Local_Socket_Set_Type;
      Target : in out Local_Socket_Set_Type);

   -------------------------
   -- Local_Selector_Type --
   -------------------------

   type Local_Selector_Type is limited private;

   procedure Create_Selector (Selector : out Local_Selector_Type);

   procedure Close_Selector (Selector : in out Local_Selector_Type);
   --  Close Selector and all internal descriptors associated

   type Selector_Status is (Completed, Expired, Aborted);

   Forever : constant := Duration (Integer'Last) * 1.0;

   --  This procedure provides the same functionnalty than the
   --  GNAT.Sockets one : it blocks until an event happens in a set of
   --  channels.

   procedure Check_Selector
     (Selector     : in out Local_Selector_Type;
      R_Socket_Set : in out Local_Socket_Set_Type;
      Status       : out Selector_Status;
      Timeout      : in Standard.Duration := Forever);

   procedure Abort_Selector (Selector : in Local_Selector_Type);
   --  Send an abort signal to the selector.

private
   type Channel is tagged record
      Empty_Buffer                            : Boolean := True;
      Full_Buffer                             : Boolean := False;
      IO_V                                    : aliased PolyORB.Buffers.Iovec;
      My_Mutex                                : PTM.Mutex_Access;
      Not_Full_Condition, Not_Empty_Condition : PTCV.Condition_Access;
      Shared                                  : PolyORB.Buffers.Buffer_Access;
      Connecting_Only                         : Boolean := False;

   end record;

   type Local_Selector_Type_Access is access Local_Selector_Type;

   type Local_Socket_Type is new Channel with record
      Used            : Boolean := False;
      Addr            : Local_Socket_Addr;
      Connected       : Boolean := False;
      Is_Connected    : PTCV.Condition_Access;
      Connecting_Port : Port    := Port (0);
      Selector        : Local_Selector_Type_Access;
   end record;

   package Local_Socket_Lists is new PolyORB.Utils.Chained_Lists (
      Local_Socket_Access);

   type Local_Socket_Set_Type is new Local_Socket_Lists.List;

   type Local_Selector_Type is record
      Set   : Local_Socket_Set_Type;
      Mutex : PTM.Mutex_Access;
      CV    : PTCV.Condition_Access;
   end record;

end PolyORB.Local_Sockets;
