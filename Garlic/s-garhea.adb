------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                 S Y S T E M . G A R L I C . H E A R T                    --
--                                                                          --
--                                B o d y                                   --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Protocols;
with System.Garlic.Termination;
with System.Garlic.Utils;

package body System.Garlic.Heart is

   --  The protocol used is:
   --
   --   - <QUERY_LOCATION> <PARTITION_ID>
   --   - <QUERY_LOCATION_ANSWER> <PARTITION_ID> <LOCATION>
   --   - <SET_LOCATION> <LOCATION>
   --   - <SHUTDOWN>

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("HEART", "(s-garhea): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use System.RPC;
   --  Needed to make Partition_ID and Params_Stream_Type visible.

   use type Ada.Exceptions.Exception_Id;
   --  Needed to have /=.

   use System.Garlic.Utils;
   --  Needed to have To_Stream_Element_Array.

   --  Constants and subtypes of System.RPC.Partition_ID.

   subtype Valid_Partition_ID is Partition_ID
     range Null_Partition_ID + 1 .. Partition_ID'Last;
   Server_Partition_ID : constant Valid_Partition_ID
     := Valid_Partition_ID'First;

   Reconnection_Policy : Reconnection_Type := Immediately;
   Shutdown_Policy     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error;
   --  These parameters control how Garlic will act in face of errors.
   --  They don't need extra protection because they should not be modified
   --  by more than one task (in fact, they should not be modified after
   --  the elaboration is terminated).

   Elaboration_Barrier : Utils.Barrier;
   --  This barrier will be no longer blocking when the elaboration is
   --  terminated.

   function Get_Protocol
     (Partition : Partition_ID)
      return Protocols.Protocol_Access;
   pragma Inline (Get_Protocol);
   --  Return (fast) the protocol of a partition using a cache
   --  whenever possible.

   protected type Local_Partition_ID_Type is
      entry Get (Partition : out Partition_ID);
      procedure Set (Partition : in Partition_ID);
      function Get_Immediately return Partition_ID;
   private
      In_Progress     : Boolean := False;
   end Local_Partition_ID_Type;
   --  Local partition ID.

   type Local_Partition_ID_Access is access Local_Partition_ID_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Local_Partition_ID_Type,
                                      Local_Partition_ID_Access);

   Local_Partition_ID : Local_Partition_ID_Access :=
     new Local_Partition_ID_Type;

   Local_Partition : Partition_ID := Null_Partition_ID;
   --  Fast version for direct access.

   Is_Boot : Boolean;
   --  Set to True if we are on the boot partition.

   type Partition_Data is record
      Location : Physical_Location.Location;
      Known    : Boolean := False;
      Queried  : Boolean := False;
   end record;
   --  Location holds the location, Known the fact that we already have
   --  information on this partition, and Queried the fact that the caller
   --  has to obtain the information using another way.

   type Partition_Data_Array is array (Valid_Partition_ID) of Partition_Data;

   protected type Partition_Map_Type is
      procedure Set_Data
        (Partition : in Partition_ID;
         Data      : in Partition_Data);
      function Get_Data (Partition : Partition_ID) return Partition_Data;
      entry Wait_For_Data (Partition_ID) (Data : out Partition_Data);
   private
      Map : Partition_Data_Array;
   end Partition_Map_Type;
   --  Data available for a partition. When the data is not available for
   --  a given partition, Wait_For_Data will block, unless the caller *has*
   --  to ask for information about the given partition (in this case,
   --  the Queried field is set to True). If we are on the server, Queried
   --  is never set to True since we wait for the partition itself to
   --  register (this should have occurred already in a normal utilization
   --  since there is no way for a partition to ask for another one if
   --  the name server has not mapped the name of the wanted package on the
   --  partition number).

   Partition_Map_Cache : Partition_Data_Array;
   --  This acts as a Cache for Partition_Map, this means that if Known
   --  is True for a given partition, there is no need to use the overhead
   --  of the protected type to query a partition location.

   Protocols_Cache : array (Partition_ID) of Protocols.Protocol_Access;
   --  Copy of the protocol type of the Partition_Map_Cache.

   type Partition_Map_Access is access Partition_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_Map_Type,
                                      Partition_Map_Access);

   Partition_Map : Partition_Map_Access :=
     new Partition_Map_Type;

   My_Location : Physical_Location.Location;
   --  My own location.

   type Allocated_Map is array (Partition_ID range <>) of Boolean;
   --  Type of allocated partitions.

   protected type Partition_ID_Allocation_Type is
      procedure Allocate (Partition : out Partition_ID);
      procedure Free (Partition : in Partition_ID);
      function Latest return Partition_ID;
   private
      Latest_Partition : Valid_Partition_ID :=
        Server_Partition_ID;
      Allocated        : Allocated_Map (Valid_Partition_ID) :=
        (Server_Partition_ID => True,
         others              => False);
   end Partition_ID_Allocation_Type;

   type Partition_ID_Allocation_Access is
      access Partition_ID_Allocation_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Partition_ID_Allocation_Type,
                                      Partition_ID_Allocation_Access);

   Partition_ID_Allocation : Partition_ID_Allocation_Access :=
     new Partition_ID_Allocation_Type;

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Operation : in Internal_Opcode;
      Params    : access Params_Stream_Type);
   --  Internal operation.

   procedure Handle_Public
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type);
   --  Public operation.

   type Receiver_Array is array (Public_Opcode) of Public_Receiver;

   protected type Receiver_Map_Type is
      procedure Set (Operation : in Opcode; Receiver : in Public_Receiver);
      entry Get (Opcode) (Receiver : out Public_Receiver);
   private
      Receiver_Data : Receiver_Array;
   end Receiver_Map_Type;
   --  A list of receivers.

   type Receiver_Map_Access is access Receiver_Map_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Receiver_Map_Type,
                                      Receiver_Map_Access);

   Receiver_Map : Receiver_Map_Access := new Receiver_Map_Type;

   procedure Shutdown;
   --  Generates a local shutdown.

   --------------------------
   -- Add_New_Partition_ID --
   --------------------------

   procedure Add_New_Partition_ID (Partition : in Partition_ID) is
      Empty : aliased Params_Stream_Type (0);
   begin
      if Reconnection_Policy = Immediately then

         --  Send a NOP to establish the connection.

         Send (Partition, No_Operation, Empty'Access);
      end if;
   end Add_New_Partition_ID;

   ---------------------------
   -- Allocate_Partition_ID --
   ---------------------------

   function Allocate_Partition_ID return Partition_ID is
      Partition : Partition_ID;
   begin
      Partition_ID_Allocation.Allocate (Partition);
      pragma Debug (D (D_Server, "Allocating partition" & Partition'Img));
      return Partition;
   end Allocate_Partition_ID;

   -------------------------------
   -- Elaboration_Is_Terminated --
   -------------------------------

   procedure Elaboration_Is_Terminated is
   begin
      pragma Debug
        (D (D_Elaborate, "Signaling that elaboration is terminated"));
      Elaboration_Barrier.Signal_All (Permanent => True);
   end Elaboration_Is_Terminated;

   -----------------
   -- Fatal_Error --
   -----------------

   protected body Fatal_Error is

      --------------
      -- Occurred --
      --------------

      entry Occurred
        (What    : out Ada.Exceptions.Exception_Id;
         Message : out String_Ptr)
      when Exc /= Ada.Exceptions.Null_Id is
      begin
         What    := Exc;
         Message := new String'(Msg.all);
      end Occurred;

      ------------
      -- Signal --
      ------------

      procedure Signal
        (What    : in Ada.Exceptions.Exception_Id;
         Message : in String := "")
      is
         procedure Free is new Ada.Unchecked_Deallocation (String, String_Ptr);
      begin
         Free (Msg);
         Exc := What;
         Msg := new String'(Message);
      end Signal;

   end Fatal_Error;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return Partition_ID is
   begin
      return Server_Partition_ID;
   end Get_Boot_Server;

   ---------------------
   -- Get_Boot_Server --
   ---------------------

   function Get_Boot_Server return String is
      Data : Partition_Data;
   begin
      Partition_Map.Wait_For_Data (Server_Partition_ID) (Data);
      return Physical_Location.To_String
        (Data.Location);
   end Get_Boot_Server;

   ---------------------
   -- Get_My_Location --
   ---------------------

   function Get_My_Location return Physical_Location.Location is
   begin
      return My_Location;
   end Get_My_Location;

   -------------------------
   -- Get_My_Partition_ID --
   -------------------------

   function Get_My_Partition_ID return Partition_ID is
      Partition : Partition_ID;
   begin
      Local_Partition_ID.Get (Partition);
      if Partition = Null_Partition_ID then
         declare
            use Physical_Location;
            Params : aliased Params_Stream_Type (0);
         begin

            --  We will send a Set_Location to the server. This will cause
            --  a dialog to be established and a new Partition_ID to be
            --  allocated, and our location will be registered into
            --  the server's base.

            Location'Write (Params'Access,
                            My_Location);
            Send (Server_Partition_ID, Set_Location, Params'Access);
            Local_Partition_ID.Get (Partition);
         end;
      end if;
      return Partition;
   end Get_My_Partition_ID;

   -------------------------------------
   -- Get_My_Partition_ID_Immediately --
   -------------------------------------

   function Get_My_Partition_ID_Immediately return Partition_ID is
   begin
      if Local_Partition = Null_Partition_ID then
         return Local_Partition_ID.Get_Immediately;
      else
         return Local_Partition;
      end if;
   end Get_My_Partition_ID_Immediately;

   ----------------------------
   -- Get_Partition_Location --
   ----------------------------

   function Get_Partition_Location (Partition : Partition_ID)
     return Physical_Location.Location is
      Data : Partition_Data;
   begin
      pragma Debug
        (D (D_Table,
            "Looking in my tables for location of partition" & Partition'Img));

      --  If the partition location is in the cache, then get it from
      --  there instead of using the protected type.

      if Partition_Map_Cache (Partition) .Known then
         return Partition_Map_Cache (Partition) .Location;
      end if;

      Partition_Map.Wait_For_Data (Partition) (Data);
      if Data.Queried then

         --  We have to query the server for the location.

         declare
            Params : aliased Params_Stream_Type (0);
         begin
            pragma Debug
              (D (D_Garlic,
                  "Asking for location of partition" & Partition'Img));
            Partition_ID'Write (Params'Access, Partition);
            Send (Server_Partition_ID, Query_Location, Params'Access);
         end;

         Partition_Map.Wait_For_Data (Partition) (Data);

         pragma Debug
           (D (D_Table,
               "Can now serve location of partition" & Partition'Img));
      end if;
      return Data.Location;
   end Get_Partition_Location;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol (Partition : Partition_ID)
     return Protocols.Protocol_Access
   is
   begin
      if not Partition_Map_Cache (Partition) .Known then
         declare
            Dummy : constant Physical_Location.Location :=
              Get_Partition_Location (Partition);
         begin
            null;
         end;
      end if;
      return Protocols_Cache (Partition);
   end Get_Protocol;

   ---------------------
   -- Handle_Internal --
   ---------------------

   procedure Handle_Internal
     (Partition : in Partition_ID;
      Operation : in Internal_Opcode;
      Params    : access Params_Stream_Type)
   is
   begin

      Termination.Activity_Detected;

      case Operation is

         when No_Operation => null;

         when Set_Location =>
            declare
               Loc  : Physical_Location.Location;
               Data : Partition_Data;
            begin
               pragma Debug
                 (D (D_Server,
                     "I received location of partition" & Partition'Img));
               Physical_Location.Location'Read (Params, Loc);
               pragma Debug
                 (D (D_Server,
                     "Partition" & Partition'Img &
                     " is at " & Physical_Location.To_String (Loc)));
               Data := (Location => Loc,
                        Known    => True,
                        Queried  => False);
               Partition_Map.Set_Data (Partition, Data);
            end;

         when Query_Location =>
            declare
               Asked : Partition_ID;
               Loc   : Physical_Location.Location;
               Ans   : aliased Params_Stream_Type (0);
            begin
               Partition_ID'Read (Params, Asked);
               if not Asked'Valid then
                  pragma Debug
                    (D (D_Debug, "Received invalid partition ID"));
                  raise Constraint_Error;
               end if;
               pragma Debug
                 (D (D_Server,
                     "Partition" & Partition'Img &
                     " asked me for location of partition" & Asked'Img));
               Loc := Get_Partition_Location (Asked);
               pragma Debug
                 (D (D_Server,
                     "Giving location of partition" & Asked'Img &
                     " to partition" & Partition'Img));
               Partition_ID'Write (Ans'Access, Asked);
               Physical_Location.Location'Write (Ans'Access, Loc);
               Send (Partition, Query_Location_Answer, Ans'Access);
            end;

         when Query_Location_Answer =>
            declare
               Asked : Partition_ID;
               Loc   : Physical_Location.Location;
               Data  : Partition_Data;
            begin
               Partition_ID'Read (Params, Asked);
               if not Asked'Valid then
                  pragma Debug (D (D_Debug, "Received invalid partition ID"));
                  raise Constraint_Error;
               end if;
               pragma Debug
                 (D (D_Garlic,
                     "I received the answer for location of partition" &
                     Asked'Img));
               Physical_Location.Location'Read (Params, Loc);
               Data := (Location => Loc,
                        Known    => True,
                        Queried  => False);
               Partition_Map.Set_Data (Asked, Data);
            end;

         when Shutdown =>
            pragma Debug
              (D (D_Garlic,
                  "I received a shutdown request from partition" &
                  Partition'Img));
            Heart.Shutdown;

      end case;

      exception
         when others =>
            pragma Debug (D (D_Garlic, "Handle internal: fatal error"));
            raise Communication_Error;
   end Handle_Internal;

   -------------------
   -- Handle_Public --
   -------------------

   procedure Handle_Public
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Params_Stream_Type)
   is
      Receiver : Public_Receiver;
   begin
      if Operation /= Shutdown_Synchronization then
         Termination.Activity_Detected;
      end if;
      Receiver_Map.Get (Operation) (Receiver);
      Receiver (Partition, Operation, Params);
   end Handle_Public;

   -----------------
   -- Has_Arrived --
   -----------------

   procedure Has_Arrived
     (Partition : in Partition_ID;
      Data      : in Ada.Streams.Stream_Element_Array)
   is
      Params    : aliased Params_Stream_Type (Data'Length);
      Operation : Opcode;
   begin
      To_Params_Stream_Type (Data, Params'Access);
      Opcode'Read (Params'Access, Operation);
      if not Operation'Valid then
         pragma Debug
           (D (D_Debug, "Received unknown opcode"));
         raise Constraint_Error;
      end if;
      pragma Debug
        (D (D_Debug,
            "Received request with opcode " & Operation'Img));
      if Operation in Internal_Opcode then
         Handle_Internal (Partition, Operation, Params'Access);
      elsif Operation in Public_Opcode then
         Handle_Public (Partition, Operation, Params'Access);
      else
         pragma Debug (D (D_Debug, "Aborting due to invalid opcode"));
         raise Constraint_Error;
      end if;
   end Has_Arrived;

   -----------------------
   -- Is_Boot_Partition --
   -----------------------

   procedure Is_Boot_Partition (Yes : in Boolean) is
   begin
      Is_Boot := Yes;
      if Is_Boot then
         Local_Partition_ID.Set (Server_Partition_ID);
      end if;
   end Is_Boot_Partition;

   -----------------------
   -- Is_Boot_Partition --
   -----------------------

   function Is_Boot_Partition return Boolean is
   begin
      return Is_Boot;
   end Is_Boot_Partition;

   -----------------------------------
   -- Latest_Allocated_Partition_ID --
   -----------------------------------

   function Latest_Allocated_Partition_ID return Partition_ID is
   begin
      return Partition_ID_Allocation.Latest;
   end Latest_Allocated_Partition_ID;

   -----------------------------
   -- Local_Partition_ID_Type --
   -----------------------------

   protected body Local_Partition_ID_Type is

      ---------
      -- Get --
      ---------

      entry Get (Partition : out Partition_ID)
      when Local_Partition /= Null_Partition_ID or not In_Progress is
      begin
         if Partition = Null_Partition_ID then
            In_Progress := True;
         end if;
         Partition := Local_Partition;
      end Get;

      ---------------------
      -- Get_Immediately --
      ---------------------

      function Get_Immediately return Partition_ID is
      begin
         return Local_Partition;
      end Get_Immediately;

      ---------
      -- Set --
      ---------

      procedure Set (Partition : in Partition_ID) is
      begin
         In_Progress := False;
         Local_Partition := Partition;
      end Set;

   end Local_Partition_ID_Type;

   ----------------------------------
   -- Partition_ID_Allocation_Type --
   ----------------------------------

   protected body Partition_ID_Allocation_Type is

      --------------
      -- Allocate --
      --------------

      procedure Allocate (Partition : out Partition_ID) is
      begin
         for I in Allocated'Range loop
            if not Allocated (I) then
               Partition := I;
               Allocated (I) := True;
               if I > Latest_Partition then
                  Latest_Partition := I;
               end if;
               return;
            end if;
         end loop;
         raise Constraint_Error;
      end Allocate;

      ----------
      -- Free --
      ----------

      procedure Free (Partition : in Partition_ID) is
      begin
         Allocated (Partition) := False;
         if Partition = Latest_Partition then
            for I in reverse Allocated'First .. Partition - 1 loop
               if Allocated (I) then
                  Latest_Partition := I;
                  return;
               end if;
            end loop;

            --  We should not be here, since the server partition id
            --  has to stay allocated.

            pragma Assert (False);

         end if;
      end Free;

      ------------
      -- Latest --
      ------------

      function Latest return Partition_ID is
      begin
         return Latest_Partition;
      end Latest;

   end Partition_ID_Allocation_Type;

   ------------------------
   -- Partition_Map_Type --
   ------------------------

   protected body Partition_Map_Type is

      --------------
      -- Get_Data --
      --------------

      function Get_Data (Partition : Partition_ID) return Partition_Data is
      begin
         return Map (Partition);
      end Get_Data;

      --------------
      -- Set_Data --
      --------------

      procedure Set_Data
        (Partition : in Partition_ID;
         Data      : in Partition_Data) is
      begin
         Map (Partition) := Data;
         Partition_Map_Cache (Partition) := Data;
         Protocols_Cache (Partition) :=
           Physical_Location.Get_Protocol (Data.Location);
      end Set_Data;

      -------------------
      -- Wait_For_Data --
      -------------------

      entry Wait_For_Data (for Partition in Partition_ID)
        (Data : out Partition_Data)
      when Map (Partition).Known or else not Map (Partition).Queried is
      begin
         if Map (Partition).Known then
            Data := Map (Partition);
         else
            Map (Partition).Queried := True;
            Data := Map (Partition);
         end if;
      end Wait_For_Data;

   end Partition_Map_Type;

   -------------
   -- Receive --
   -------------

   procedure Receive (Operation : in Opcode; Receiver : in Public_Receiver) is
   begin
      pragma Debug
        (D (D_Garlic,
            "Receiver for operation " & Operation'Img & " is now registered"));
      Receiver_Map.Set (Operation, Receiver);
   end Receive;

   ------------------
   -- Receiver_Map --
   ------------------

   protected body Receiver_Map_Type is

      ---------
      -- Get --
      ---------

      entry Get (for Operation in Opcode) (Receiver : out Public_Receiver)
      when Receiver_Data (Operation) /= null is
      begin
         Receiver := Receiver_Data (Operation);
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Operation : in Opcode; Receiver : in Public_Receiver) is
      begin
         Receiver_Data (Operation) := Receiver;
      end Set;

   end Receiver_Map_Type;

   --------------
   -- Register --
   --------------

   procedure Register
     (Partition : in Partition_ID;
      Location  : in Physical_Location.Location)
   is
      Data : constant Partition_Data := (Location => Location,
                                         Known    => True,
                                         Queried  => False);
   begin
      Partition_Map.Set_Data (Partition, Data);
   end Register;

   ----------------------------
   -- Remote_Partition_Error --
   ----------------------------

   procedure Remote_Partition_Error (Partition : in Partition_ID) is
   begin
      pragma Debug
        (D (D_Communication,
            "It seems that partition" & Partition'Img & " is dead"));
      if Is_Boot_Partition and then
        Shutdown_Policy = Shutdown_On_Any_Partition_Error then
         pragma Debug
            (D (D_Communication, "Due to the policy, I will shutdown"));
         Soft_Shutdown;
      end if;
      if Partition = Server_Partition_ID and then
        Shutdown_Policy = Shutdown_On_Boot_Partition_Error then
         pragma Debug
           (D (D_Communication, "I cannot live without a boot partition"));
         Soft_Shutdown;
      end if;
   end Remote_Partition_Error;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Operation : in Opcode;
      Params    : access System.RPC.Params_Stream_Type)
   is
      Protocol  : constant Protocols.Protocol_Access :=
        Get_Protocol (Partition);
      Op_Params : aliased Params_Stream_Type (0);
      use type Ada.Streams.Stream_Element_Array;
   begin
      --  if Partition = Get_My_Partition_ID_Immediately then
      --     pragma Debug (D (D_Garlic, "Cannot send to myself, huh ?"));
      --     raise Communication_Error;
      --  end if;
      Opcode'Write (Op_Params'Access, Operation);
      declare
         use type Ada.Streams.Stream_Element_Offset;
         Header : constant Ada.Streams.Stream_Element_Array :=
           To_Stream_Element_Array (Op_Params'Access);
         Packet : aliased Ada.Streams.Stream_Element_Array :=
           To_Stream_Element_Array (Params,
                                    Protocols.Unused_Space + Header'Length);
      begin
         Packet (Packet'First + Protocols.Unused_Space ..
                 Packet'First + Protocols.Unused_Space + Header'Length - 1) :=
           Header;
         Protocols.Send (Protocol, Partition, Packet'Access);
      end;
   end Send;

   -----------------------
   -- Set_Boot_Location --
   -----------------------

   procedure Set_Boot_Location (Location : in Physical_Location.Location)
   is
      Data : constant Partition_Data := (Location => Location,
                                         Known    => True,
                                         Queried  => False);
   begin
      Partition_Map.Set_Data (Server_Partition_ID, Data);
   end Set_Boot_Location;

   ---------------------
   -- Set_My_Location --
   ---------------------

   procedure Set_My_Location (Location : in Physical_Location.Location)
   is
   begin
      My_Location := Location;
   end Set_My_Location;

   -------------------------
   -- Set_My_Partition_ID --
   -------------------------

   procedure Set_My_Partition_ID (Partition : in Partition_ID) is
   begin
      Local_Partition_ID.Set (Partition);
   end Set_My_Partition_ID;

   ----------------
   -- Set_Policy --
   ----------------

   procedure Set_Policy
     (Reconnection : Reconnection_Type := Immediately;
      Shutdown     : Shutdown_Type     := Shutdown_On_Boot_Partition_Error)
   is
   begin
      Reconnection_Policy := Reconnection;
      Shutdown_Policy     := Shutdown;
   end Set_Policy;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Termination.Shutdown;
      Physical_Location.Shutdown;
      Shutdown_Keeper.Signal;
      Free (Local_Partition_ID);
      Free (Partition_Map);
      Free (Partition_ID_Allocation);
      Free (Receiver_Map);
   end Shutdown;

   ---------------------
   -- Shutdown_Keeper --
   ---------------------

   protected body Shutdown_Keeper is

      ------------
      -- Signal --
      ------------

      procedure Signal is
      begin
         In_Progress := True;
      end Signal;

      ----------
      -- Wait --
      ----------

      entry Wait when In_Progress is
      begin
         null;
      end Wait;

   end Shutdown_Keeper;

   -------------------
   -- Soft_Shutdown --
   -------------------

   procedure Soft_Shutdown is
   begin
      if Is_Boot_Partition then
         for Partition in
           Server_Partition_ID + 1 .. Partition_ID_Allocation.Latest loop
            declare
               Empty : aliased Params_Stream_Type (0);
            begin
               Send (Partition, Shutdown, Empty'Access);
            exception
               when Communication_Error => null;
            end;
         end loop;
      end if;
      Heart.Shutdown;
   end Soft_Shutdown;

   ------------------------------------------
   -- Wait_Until_Elaboration_Is_Terminated --
   ------------------------------------------

   procedure Wait_Until_Elaboration_Is_Terminated is
   begin
      Elaboration_Barrier.Wait;
   end Wait_Until_Elaboration_Is_Terminated;

end System.Garlic.Heart;
