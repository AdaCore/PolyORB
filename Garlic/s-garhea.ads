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
with System.Garlic.Utils;

--  These ones should not be needed, but the binder needs them to get a
--  correct dependencies order ???

with System.Tasking.Initialization;
pragma Elaborate_All (System.Tasking.Initialization);
pragma Warnings (Off, System.Tasking.Initialization);
with System.Tasking.Protected_Objects;
pragma Elaborate_All (System.Tasking.Protected_Objects);
pragma Warnings (Off, System.Tasking.Protected_Objects);

package System.Garlic.Heart is

   -----------------
   -- Boot server --
   -----------------

   procedure Initialize;
   --  Initialize the package

   procedure Set_Is_Boot_Partition (Yes : in Boolean);
   --  Called when we are on the boot partition

   function Is_Boot_Partition return Boolean;
   pragma Inline (Is_Boot_Partition);
   --  This function return True when the local partition is the boot
   --  partition.

   procedure Set_Boot_Location
     (Location : in System.Garlic.Physical_Location.Location_Type);
   --  Set boot server coordinates

   --------------
   -- Settings --
   --------------

   procedure Set_Policy
     (Shutdown     : Types.Shutdown_Type     :=
        Types.Shutdown_On_Boot_Partition_Error);
   --  Sets Garlic policy about Shutdowns and reconnections

   -------------------------
   -- Elaboration control --
   -------------------------

   procedure Complete_Elaboration;
   --  This procedure must be called as the first instruction of the
   --  main procedure.

   procedure Wait_Until_Elaboration_Is_Terminated;
   --  This procedure blocks until elaboration is terminated

   procedure Soft_Shutdown;
   --  Shutdown everything after signaling to all known reachable
   --  partitions to shutdown also.

   function Is_Shutdown_In_Progress return Boolean;
   pragma Inline (Is_Shutdown_In_Progress);
   --  This function will return True when shutdown is in progress

   function Blocking_Partition (Partition : Types.Partition_ID) return Boolean;
   --  Return True if a partition has a local termination but is still
   --  alive. This means that the whole distributed program cannot terminate
   --  because a client is still working.

   function Reconnection_Policy (Partition : Types.Partition_ID)
     return Types.Reconnection_Type;
   --  Return policy to use when reconnecting to Partition

   function Termination_Policy (Partition : Types.Partition_ID)
     return Types.Termination_Type;
   --  Return the termination policy of a remote partition

   function Name (Partition : Types.Partition_ID)
     return Name_Table.Name_Id;
   function Name (Partition : Types.Partition_ID)
     return String;
   --  Return the name of a partition in its coded or plaintext form

   function Location (Partition : Types.Partition_ID)
     return Physical_Location.Location_Type;
   --  Return the location of a partition

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

   function Can_Have_A_Light_Runtime return Boolean;
   --  Return True if this partition is suitable for having a light runtime.
   --  The result is built from several checks concerning the termination
   --  policy and some other static parameters.

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

   type Opcode is (Invalid_Operation,        -- First Internal Opcode
                   No_Operation,
                   Set_Public_Data,
                   Query_Public_Data,
                   Query_Public_Data_Answer,
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
   --  request. These types *must* be updated as soon as Opcode is updated.
   --  The definition of a public opcode is an opcode that will be used by
   --  an external module, while an internal opcode will be handled by this
   --  package.

   type Public_Receiver is
      access procedure (Partition : in Types.Partition_ID;
                        Operation : in Public_Opcode;
                        Params    : access Streams.Params_Stream_Type);
   --  A procedure which will get the requests

   procedure Send
     (Partition : in Types.Partition_ID;
      Operation : in Opcode;
      Params    : access Streams.Params_Stream_Type);
   --  Send something to a remote partition after calling the appropriate
   --  filter.

   procedure Receive
     (Operation : in Opcode;
      Receiver  : in Public_Receiver);
   --  Receive something from a remote partition given the Opcode. The
   --  receiver will have to read the Params_Stream_Type before returning.

   procedure Has_Arrived
     (Partition     : in Types.Partition_ID;
      Filtered_Data : access Ada.Streams.Stream_Element_Array;
      Offset        : in Ada.Streams.Stream_Element_Count := 0);
   --  Called by a protocol to signal that something has arrived. Data has
   --  not been unfiltered yet. Offset represents the number of bytes that
   --  should not been considered.

end System.Garlic.Heart;
