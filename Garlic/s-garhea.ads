------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . H E A R T                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
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

with Ada.Streams;
with System.Garlic.Name_Table;
with System.Garlic.Physical_Location;
with System.Garlic.Streams;
with System.Garlic.Types;

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

   procedure Set_Boot_Location
     (Location : in System.Garlic.Physical_Location.Location_Type);
   --  Set boot server coordinates

   --------------
   -- Settings --
   --------------

   procedure Next_Partition
     (Partition : in out Types.Partition_ID;
      Increment : in Boolean := True;
      Allocated : in Boolean := True);
   --  Find next partition id after Partition which is Allocated (or
   --  un-allocated when Allocated is False). If Partition is
   --  Null_Partition, start from the first partition id. If there
   --  is no candidate, keep Partition unmodified. If Increment is false,
   --  then scan table by decrementing.

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
   pragma Inline (Location);
   --  Return the location of a partition

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

   procedure Wait_For_Elaboration_Completion;
   --  This procedure blocks until elaboration is terminated

   procedure Soft_Shutdown;
   --  Shutdown everything after signaling to all known reachable
   --  partitions to shutdown also.

   Shutdown_In_Progress : Boolean := False;
   pragma Atomic (Shutdown_In_Progress);

   ---------------------
   -- Local partition --
   ---------------------

   function Get_My_Partition_ID return Types.Partition_ID;
   --  Return the Partition_ID of the running partition. If the
   --  Partition_ID isn't known yet the function will block until
   --  the server gives it to us.

   procedure Set_My_Partition_ID (Partition : Types.Partition_ID);
   --  Set my partition ID

   function Can_Have_A_Light_Runtime return Boolean;
   --  Return True if this partition is suitable for having a light runtime.
   --  The result is built from several checks concerning the termination
   --  policy and some other static parameters.

   ----------------------
   -- Remote partition --
   ----------------------

   procedure Remote_Partition_Error
     (Partition : in Types.Partition_ID);
   --  Signal that a partition is dead

   type RPC_Error_Notifier_Type is
      access procedure (Partition : in Types.Partition_ID);

   procedure Register_Partition_Error_Notification
     (Callback : in RPC_Error_Notifier_Type);
   --  Register a procedure that will be called whenever a communication
   --  error occurs during a remote call.

   type Any_Opcode is
      (Invalid_Operation,
       No_Operation,             -- First Internal Opcode
       Partition_Operation,
       Shutdown_Operation,       -- Last Internal Opcode
       Remote_Call,              -- First Public Opcode
       User_Message,
       Group_Service,
       Shutdown_Service,
       Unit_Name_Service,
       Filtering_Service);       -- Last Public Opcode
   subtype Internal_Opcode is Any_Opcode
     range No_Operation .. Shutdown_Operation;
   subtype External_Opcode is Any_Opcode
     range Remote_Call .. Filtering_Service;
   --  Type of the current operation. Note that Invalid_Operation is here
   --  to catch the trivial case where zeros are sent instead of a real
   --  request. These types *must* be updated as soon as Opcode is updated.
   --  The definition of a public opcode is an opcode that will be used by
   --  an external module, while an internal opcode will be handled by this
   --  package.

   type Request_Handler is
      access procedure
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type);
   --  A procedure which will get the requests

   procedure Analyze_Stream
     (Partition  : out Types.Partition_ID;
      Opcode     : out Any_Opcode;
      Unfiltered : out Streams.Stream_Element_Access;
      Filtered   : in  Streams.Stream_Element_Access;
      Offset     : in Ada.Streams.Stream_Element_Count := 0);
   --  Called by a protocol to signal that something has arrived. Data has
   --  not been unfiltered yet. Offset represents the number of bytes that
   --  should not been considered.

   procedure Handle_Any_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in Any_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type);

   procedure Process_Stream
     (Partition  : in Types.Partition_ID;
      Opcode     : in Any_Opcode;
      Unfiltered : in Streams.Stream_Element_Access);

   procedure Register_Handler
     (Opcode  : in Any_Opcode;
      Handler : in Request_Handler);
   --  Receive something from a remote partition given the Opcode. The
   --  receiver will have to read the Params_Stream_Type before returning.

   procedure Send
     (Partition : in Types.Partition_ID;
      Opcode    : in Any_Opcode;
      Params    : access Streams.Params_Stream_Type);
   --  Send something to a remote partition after calling the appropriate
   --  filter.

end System.Garlic.Heart;
