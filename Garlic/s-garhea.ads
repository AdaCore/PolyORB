------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . H E A R T                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Streams;
with System.Garlic.Name_Table;
with System.Garlic.Physical_Location;
with System.Garlic.Streams;
with System.Garlic.Types;

--  These ones should not be needed, but the binder needs them to get a
--  correct dependencies order ???

pragma Warnings (Off);
with System.Tasking.Initialization;
pragma Elaborate_All (System.Tasking.Initialization);
with System.Tasking.Protected_Objects;
pragma Elaborate_All (System.Tasking.Protected_Objects);
pragma Warnings (On);

package System.Garlic.Heart is

   My_Partition_Name : Name_Table.Name_Id;

   Null_Partition_ID : constant Types.Partition_ID;
   --  Means "no Partition_ID known at this time"

   type Partition_Data is record
      Location : Physical_Location.Location_Type;
      Name     : Name_Table.Name_Id;
      Known    : Boolean;
      Queried  : Boolean;
   end record;
   --  Location holds the location, Name the name of the partition, Known
   --  the fact that we already have information on this partition, and
   --  Queried the fact that the caller has to obtain the information using
   --  another way.

   -----------------
   -- Boot server --
   -----------------

   function Get_Boot_Server return String;
   --  This function returns the coordinates of the boot server

   function Get_Boot_Server return Types.Partition_ID;
   --  Return the partition of the boot server

   procedure Initialize;
   --  Initialize the package

   procedure Set_Is_Boot_Partition (Yes : in Boolean);
   --  Called when we are on the boot partition

   function Is_Boot_Partition return Boolean;
   --  This function return True when the local partition is the boot
   --  partition.

   procedure Set_Boot_Location
     (Location : in System.Garlic.Physical_Location.Location_Type);
   --  Set boot server coordinates

   --------------
   -- Settings --
   --------------

   type Reconnection_Type is (Immediately,
                              When_Needed);
   --  Immediately reconnects as soon as a connection is broken (default).
   --  When_Needed waits for this connection to be necessary.

   type Shutdown_Type is (Shutdown_On_Any_Partition_Error,
                          Shutdown_On_Boot_Partition_Error,
                          Never_Shutdown_On_Partition_Error);
   --  Three ways of terminating Garlic

   type Termination_Type is (Unknown_Termination,
                             Local_Termination,
                             Global_Termination,
                             Deferred_Termination);
   --  Three ways of terminating a partition. Should be synchronized with
   --  the type above ???

   type Execution_Mode_Type is (Trace_Mode,
                                Replay_Mode,
                                Normal_Mode);

   procedure Set_Policy
     (Reconnection : Reconnection_Type := Immediately;
      Shutdown     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error);
   --  Sets Garlic policy about Shutdowns and reconnections

   -------------------------
   -- Elaboration control --
   -------------------------

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
      function Is_In_Progress return Boolean;
   private
      In_Progress : Boolean := False;
   end Shutdown_Keeper;
   --  This protected object may be safely "waited" in asynchronous
   --  transfer of control blocks. It will be unblocked whenever
   --  a shutdown has been decided. It prevents requests from being
   --  blocked upon program logical termination.

   -----------------------
   -- Execution control --
   -----------------------

   protected Fatal_Error is
      entry Occurred
        (What    : out Ada.Exceptions.Exception_Id;
         Message : out Types.String_Access);
      procedure Signal
        (What    : in Ada.Exceptions.Exception_Id;
         Message : in String := "");
   private
      Exc : Ada.Exceptions.Exception_Id := Ada.Exceptions.Null_Id;
      Msg : Types.String_Access := null;
   end Fatal_Error;
   --  This protected object is a keeper to cancel the main procedure if
   --  needed.

   ---------------------
   -- Local partition --
   ---------------------

   procedure Set_My_Location
     (Location : in System.Garlic.Physical_Location.Location_Type);
   --  Set my coordinates

   function Get_My_Location
     return System.Garlic.Physical_Location.Location_Type;
   --  Get my coordinates

   function Get_My_Partition_ID return Types.Partition_ID;
   --  Return the Partition_ID of the running partition. If the
   --  Partition_ID isn't known yet the function will block until
   --  the server gives it to us.

   function Get_My_Partition_ID_Immediately
     return Types.Partition_ID;
   pragma Inline (Get_My_Partition_ID_Immediately);
   --  Return the Partition_ID if it's known otherwise return
   --  Null_Partition_ID.

   procedure Set_My_Partition_ID (Partition : Types.Partition_ID);
   --  Set my partition ID

   ----------------------
   -- Remote partition --
   ----------------------

   procedure Add_New_Partition_ID (Partition : in Types.Partition_ID);
   --  Declare that a Partition is to be used. This means that if needed
   --  we will connect to it.

   procedure Remote_Partition_Error
     (Partition : in Types.Partition_ID);
   --  Signal that a partition is dead

   type RPC_Error_Notifier_Type is
      access procedure (Partition : in Types.Partition_ID);

   procedure Register_Partition_Error_Notification
     (Callback : in RPC_Error_Notifier_Type);
   --  Register a procedure that will be called whenever a communication
   --  error occurs during a remote call.

   function Get_Partition_Data (Partition : Types.Partition_ID)
     return Partition_Data;
   --  Return a partition's location

   type Opcode is (Invalid_Operation,        -- First Internal Opcode
                   No_Operation,
                   Set_Location,
                   Query_Location,
                   Query_Location_Answer,
                   Shutdown,                 -- Last Internal Opcode
                   Remote_Call,              -- First Public Opcode
                   Shutdown_Synchronization,
                   Name_Service,
                   User_Message,
                   Filtering);               -- Last Public Opcode
   subtype Internal_Opcode is Opcode
     range No_Operation .. Shutdown;
   subtype Public_Opcode is Opcode
     range Remote_Call .. Filtering;
   --  Type of the current operation. Note that Invalid_Operation is here
   --  to catch the trivial case where zeros are sent instead of a real
   --  request. These types *must* be updated as soon as Opcode is updated

   type Public_Receiver is
      access procedure (Partition : in Types.Partition_ID;
                        Operation : in Public_Opcode;
                        Params    : access Streams.Params_Stream_Type);
   --  A procedure which will get the requests

   procedure Send
     (Partition : in Types.Partition_ID;
      Operation : in Opcode;
      Params    : access Streams.Params_Stream_Type);
   --  Send something to a remote partition

   procedure Receive
     (Operation : in Opcode;
      Receiver  : in Public_Receiver);
   --  Receive something from a remote partition given the Opcode. The
   --  receiver will have to read the Params_Stream_Type before returning.

   procedure Has_Arrived
     (Partition : in Types.Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array);
   --  Called by a protocol to signal that something has arrived

   ----------------
   -- PID server --
   ----------------

   function Allocate_Partition_ID return Types.Partition_ID;
   --  Allocate a new partition ID

   function Latest_Allocated_Partition_ID return Types.Partition_ID;
   --  This function is used by the Termination mechanism which needs
   --  to address all the partitions.

   procedure Register_RPC_Shutdown (S : System.Garlic.Types.Shutdown_Access);
   --  Register a shutdown procedure which applies to system.RPC hierarchy.
   --  This allows us not to with System.RPC which would produce
   --  circular dependency.

private

   Null_Partition_ID : constant Types.Partition_ID := Types.Partition_ID'First;

end System.Garlic.Heart;
