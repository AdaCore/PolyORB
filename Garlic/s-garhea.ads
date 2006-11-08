------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . H E A R T                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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
with System.Garlic.Exceptions;
with System.Garlic.Protocols;
with System.Garlic.Streams;
with System.Garlic.Types;

package System.Garlic.Heart is

   -----------------
   -- Boot server --
   -----------------

   procedure Initialize;
   --  Initialize package

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

   procedure Wait_For_Elaboration_Completion;
   --  This procedure blocks until elaboration is terminated

   function Shutdown_Activated return Boolean;
   pragma Inline (Shutdown_Activated);

   procedure Activate_Shutdown;
   --  Shutdown everything after signaling to all known reachable
   --  partitions to shutdown also.

   ---------------------
   -- Local partition --
   ---------------------

   procedure Get_My_Partition_ID
     (PID   :    out Types.Partition_ID;
      Error : in out Exceptions.Error_Type);
   --  Return the Partition_ID of the current partition. If the
   --  Partition_ID isn't known yet the function will block until
   --  the server gives it to us.

   procedure Set_My_Partition_ID (Error : in out Exceptions.Error_Type);
   --  Used when the current partition id has been computed

   procedure Wait_For_My_Partition_ID;
   --  Wait for the partition id to be computed

   ----------------------
   -- Remote partition --
   ----------------------

   procedure Notify_Partition_Error
     (Partition : Types.Partition_ID);
   --  Signal that a partition is dead. This is ignored when shutdown is in
   --  progress. First we invalidate this partition to Garlic.Partitions.
   --  Depending on the shutdown policy, Soft_Shutdown is
   --  called. Otherwise, RPC shutdown is called to notify pending RPC
   --  callers of a communication error.

   type RPC_Error_Notifier_Type is
      access procedure (Partition : Types.Partition_ID);

   procedure Register_RPC_Error_Notifier
     (Callback : RPC_Error_Notifier_Type);
   --  Register a procedure that will be called whenever a communication
   --  error occurs during a remote subprogram call.

   type Any_Opcode is
      (Invalid_Operation,
       No_Operation,             --  First Internal Opcode
       Partition_Operation,
       Shutdown_Operation,       --  Last Internal Opcode
       Remote_Call,              --  First Public Opcode
       User_Message,
       Group_Service,
       Shutdown_Service,
       Unit_Name_Service,
       DSM_Service,              --  Distributed Shared Memory Service
       Filtering_Service);       --  Last Public Opcode
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
     (Partition : Types.Partition_ID;
      Opcode    : External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Exceptions.Error_Type);
   --  A procedure which will get the requests. Except for group requests,
   --  a request is stored in Query and if needed, the request answer is
   --  stored in Reply. For group request, see Garlic.Group special
   --  handling.

   procedure Analyze_Stream
     (Partition  : in out Types.Partition_ID;
      Protocol   : Protocols.Protocol_Access;
      Opcode     : out Any_Opcode;
      Unfiltered : out Streams.Stream_Element_Access;
      Filtered   : Streams.Stream_Element_Access;
      Offset     : Ada.Streams.Stream_Element_Offset;
      Error      : in out Exceptions.Error_Type);
   --  Called by a protocol to signal that something has
   --  arrived. Filtered has not been unfiltered yet. Offset
   --  represents the number of bytes that should not been
   --  considered. Extract the beginning of the stream: PID, Opcode
   --  and Unfiltered. This way, the protocol task knows the partition
   --  it has in charge. The protocol used to communicate with this
   --  partition is passed on the first connection with this
   --  partition. To indicate a first connection, the caller passes a
   --  Partition set to Null_PID. This partition may have no self
   --  location. By preserving the protocol, we will be able to
   --  contact the partition back.

   procedure Handle_Any_Request
     (Partition : Types.Partition_ID;
      Opcode    : Any_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Exceptions.Error_Type);
   --  A procedure which will get the requests. Except for group requests,
   --  a request is stored in Query and if needed, the request answer is
   --  stored in Reply. For group request, see Garlic.Group special
   --  handling.

   procedure Process_Stream
     (Partition  : Types.Partition_ID;
      Opcode     : Any_Opcode;
      Unfiltered : Streams.Stream_Element_Access;
      Error      : in out Exceptions.Error_Type);
   --  The stream has been already analyzed. PID and Opcode are known. The
   --  stream is also unfiltered. Execute the request handler corresponding
   --  to Opcode.

   procedure Register_Handler
     (Opcode  : Any_Opcode;
      Handler : Request_Handler);
   --  Receive something from a remote partition given the Opcode. The
   --  receiver will have to read the Params_Stream_Type before returning.

   procedure Send
     (Partition : Types.Partition_ID;
      Opcode    : Any_Opcode;
      Params    : access Streams.Params_Stream_Type;
      Error     : in out Exceptions.Error_Type);
   --  Send something to a remote partition after calling the appropriate
   --  filter.

   procedure Send_Boot_Server
     (Opcode : Any_Opcode;
      Params : access Streams.Params_Stream_Type;
      Error  : out Exceptions.Error_Type);
   --  Send something to boot server. When this partition is no longer
   --  available, then try to find a boot mirror. When no boot mirror is
   --  available, signal an error.

end System.Garlic.Heart;
