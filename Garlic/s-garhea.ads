------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . H E A R T                    --
--                                                                          --
--                                S p e c                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Streams;
with System.Garlic.Physical_Location;
with System.RPC;

package System.Garlic.Heart is

   Null_Partition_ID : constant System.RPC.Partition_ID;
   --  Means "no Partition_ID known at this time".

   --  Boot server coordinates.

   function Get_Boot_Server return String;
   --  This function returns the coordinates of the boot server.

   function Get_Boot_Server return System.RPC.Partition_ID;
   --  Return the partition of the boot server.

   procedure Is_Boot_Partition (Yes : in Boolean);
   --  Called when we are on the boot partition.

   function Is_Boot_Partition return Boolean;
   --  This function return True when the local partition is the boot
   --  partition.

   procedure Set_Boot_Location
     (Location : in System.Garlic.Physical_Location.Location);
   --  Set boot server coordinates.

   --  Garlic settings.

   type Reconnection_Type is (Immediately,
                              When_Needed);
   --  Immediately reconnects as soon as a connection is broken (default).
   --  When_Needed waits for this connection to be necessary.

   type Shutdown_Type is (Shutdown_On_Any_Partition_Error,
                          Shutdown_On_Boot_Partition_Error,
                          Never_Shutdown_On_Partition_Error);
   --  Three ways of terminating Garlic.

   procedure Set_Policy
     (Reconnection : Reconnection_Type := Immediately;
      Shutdown     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error);
   --  Sets Garlic policy about Shutdowns and reconnections.

   --  Elaboration control.

   procedure Elaboration_Is_Terminated;
   --  This procedure must be called as the first instruction of the
   --  main procedure.

   procedure Wait_Until_Elaboration_Is_Terminated;
   --  This procedure blocks until elaboration is terminated

   procedure Soft_Shutdown;
   --  Shutdown everything after signaling to all known reachable
   --  partitions to shutdown also.

   protected Shutdown_Keeper is
      entry Wait;
      procedure Signal;
   private
      In_Progress : Boolean := False;
   end Shutdown_Keeper;
   --  This protected object may be safely "waited" in asynchronous
   --  transfer of control blocks. It will be unblocked whenever
   --  a shutdown has been decided. It prevents requests from being
   --  blocked upon program logical termination.

   --  Execution control.

   type String_Ptr is access String;

   protected Fatal_Error is
      entry Occurred
        (What    : out Ada.Exceptions.Exception_Id;
         Message : out String_Ptr);
      procedure Signal
        (What    : in Ada.Exceptions.Exception_Id;
         Message : in String := "");
   private
      Exc : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
      Msg : String_Ptr := null;
   end Fatal_Error;
   --  This protected object is a keeper to cancel the main procedure if
   --  needed.

   --  Local partition.

   procedure Set_My_Location
     (Location : in System.Garlic.Physical_Location.Location);
   --  Set my coordinates.

   function Get_My_Location
     return System.Garlic.Physical_Location.Location;
   --  Get my coordinates.

   function Get_My_Partition_ID return System.RPC.Partition_ID;
   --  Return the Partition_ID of the running partition. If the
   --  Partition_ID isn't known yet the function will block until
   --  the server gives it to us.

   function Get_My_Partition_ID_Immediately
     return System.RPC.Partition_ID;
   pragma Inline (Get_My_Partition_ID_Immediately);
   --  Return the Partition_ID if it's known otherwise return
   --  Null_Partition_ID.

   procedure Set_My_Partition_ID (Partition : System.RPC.Partition_ID);
   --  Set my partition ID.

   --  Remote partition.

   procedure Add_New_Partition_ID (Partition : in System.RPC.Partition_ID);
   --  Declare that a Partition is to be used. This means that if needed
   --  we will connect to it.

   procedure Remote_Partition_Error
     (Partition : in System.RPC.Partition_ID);
   --  Signal that a partition is dead.

   type RPC_Error_Notifier_Type is
      access procedure (Partition : in System.RPC.Partition_ID);

   procedure Register_Partition_Error_Notification
     (Callback : in RPC_Error_Notifier_Type);
   --  Signal that Communication_Error on pending RPCs

   function Get_Partition_Location (Partition : System.RPC.Partition_ID)
     return System.Garlic.Physical_Location.Location;
   --  Return a partition's location.

   type Opcode is (Invalid_Operation,
                   No_Operation,
                   Set_Location,
                   Query_Location,
                   Query_Location_Answer,
                   Shutdown,
                   Remote_Call,
                   Shutdown_Synchronization,
                   Name_Service);
   subtype Internal_Opcode is Opcode
     range No_Operation .. Shutdown;
   subtype Public_Opcode is Opcode
     range Remote_Call .. Name_Service;
   --  Type of the current operation. Note that Invalid_Operation is here
   --  to catch the trivial case where zeros are sent instead of a real
   --  request.

   type Public_Receiver is
      access procedure (Partition : in System.RPC.Partition_ID;
                        Operation : in Public_Opcode;
                        Params    : access System.RPC.Params_Stream_Type);
   --  A procedure which will get the requests.

   procedure Send
     (Partition : in System.RPC.Partition_ID;
      Operation : in Opcode;
      Params    : access System.RPC.Params_Stream_Type);
   --  Send something to a remote partition.

   procedure Receive
     (Operation : in Opcode;
      Receiver  : in Public_Receiver);
   --  Receive something from a remote partition given the Opcode. The
   --  receiver will have to read the Params_Stream_Type before returning.

   procedure Has_Arrived
     (Partition : in System.RPC.Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array);
   --  Called by a protocol to signal that something has arrived.

   --  "Name service" functions.

   function Allocate_Partition_ID return System.RPC.Partition_ID;
   --  Allocate a new partition ID.

   procedure Register
     (Partition : in System.RPC.Partition_ID;
      Location  : in System.Garlic.Physical_Location.Location);
   --  Register a partition whose location has been queried or
   --  received spontaneously.

   function Latest_Allocated_Partition_ID return System.RPC.Partition_ID;
   --  This function is used by the Termination mechanism which needs
   --  to address all the partitions.

private

   Null_Partition_ID : constant System.RPC.Partition_ID :=
     System.RPC.Partition_ID'First;

end System.Garlic.Heart;
