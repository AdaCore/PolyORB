------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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
with Interfaces;                use Interfaces;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;       use System.Garlic.Debug;
with System.Garlic.Exceptions;  use System.Garlic.Exceptions;
with System.Garlic.Group;       use System.Garlic.Group;
with System.Garlic.Heart;       use System.Garlic.Heart;
with System.Garlic.Options;     use System.Garlic.Options;
with System.Garlic.Partitions;  use System.Garlic.Partitions;
with System.Garlic.Soft_Links;
with System.Garlic.Streams;     use System.Garlic.Streams;
with System.Garlic.Table;
with System.Garlic.Types;       use System.Garlic.Types;
with System.Garlic.Utils;       use System.Garlic.Utils;

package body System.Garlic.Units is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARUNI", "(s-garuni): ");

   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   type Request_Id is new Natural;
   Null_Request_Id  : constant Request_Id := 0;
   First_Request_Id : constant Request_Id := 3_000_000;
   Request_Id_Increment : constant := 10;

   subtype Request_List is Request_Id;
   Null_Request_List : constant Request_List := Null_Request_Id;

   type Request_Kind is (Copy_Units_Table,
                         Define_New_Units,
                         Invalidate_Units,
                         Pull_Units_Table,
                         Push_Units_Table);

   type Unit_Status is (Queried, Undefined, Declared, Defined, Invalid);
   --  The order is very important. At the beginning, a unit is
   --  UNDEFINED on the caller side. It will send a request to get
   --  info on this request and the new status will be QUERIED (just
   --  to avoid multiple requests). On the receiver side, it is
   --  DECLARED. The receiver registers the unit to the boot
   --  server. The boot server will try to get a first agreement from
   --  other boot mirrors. If the unit info has not been modified
   --  after a first pass, then the unit becomes DEFINED and this new
   --  info is sent once again to other boot mirrors. When the unit
   --  becomes DEFINED, the boot mirrors are allowed to answer to
   --  pending request from other partitions. When a partition dies,
   --  all its units are INVALID. If the reconnection mode of the
   --  partition is Block_Until_Restart, then the unit status is set
   --  to UNDEFINED. Otherwise, it is set to INVALID. If the
   --  reconnection mode is Fail_Until_Restart, then the status can
   --  get back to DEFINED.

   type Unit_Info is
      record
         Next_Unit : Types.Unit_Id;
         Partition : Types.Partition_ID;
         Receiver  : Interfaces.Unsigned_64;
         Version   : Types.Version_Type;
         Status    : Unit_Status;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Info :=
     (Next_Unit => Types.Null_Unit_Id,
      Partition => Types.Null_PID,
      Receiver  => 0,
      Version   => Types.Null_Version,
      Status    => Undefined,
      Requests  => Null_Request_List);

   --  Next_Unit   : units on the same partition are linked together
   --  Partition   : unit partition id
   --  Receiver    : unit rpc receiver
   --  Version     : unit version id
   --  Status      : unit info status
   --  Pending     : true when requests are pending
   --  Requests    : pending requests - list of partition ids

   type Request_Type (Kind : Request_Kind := Pull_Units_Table) is
      record
         case Kind is
            when Copy_Units_Table |
                 Define_New_Units |
                 Pull_Units_Table |
                 Push_Units_Table =>
               null;

            when Invalidate_Units =>
               Partition : Types.Partition_ID;

         end case;
      end record;

   package Units is new System.Garlic.Table.Complex
     (Index_Type     => Types.Unit_Id,
      Null_Index     => Types.Null_Unit_Id,
      First_Index    => Types.First_Unit_Id,
      Initial_Size   => Types.Unit_Id_Increment,
      Increment_Size => Types.Unit_Id_Increment,
      Component_Type => Unit_Info,
      Null_Component => Null_Unit);

   type Request_Info is record
      PID  : Partition_ID;
      Next : Request_Id;
   end record;
   Null_Request_Info : constant Request_Info := (Null_PID, Null_Request_Id);

   package Requests is new System.Garlic.Table.Complex
       (Request_Id,
        Null_Request_Id,
        First_Request_Id,
        Request_Id_Increment,
        Request_Id_Increment,
        Request_Info,
        Null_Request_Info);

   procedure Extract
     (List : in out Request_List;
      PID  : out Partition_ID);

   procedure Insert
     (List : in out Request_List;
      PID  : in Partition_ID);

   procedure Answer_Pending_Requests (List : in Request_List);
   --  A boot mirror can receive a request on a unit for which it has
   --  no info on it yet. So, we keep track of this request in order
   --  to answer it later on. When info becomes available, answer to
   --  pending requests.

   procedure Dump_Unit_Table;

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info);

   function Dump_Request_List
     (List : Request_List;
      Root : Boolean := True) return String;

   procedure Get_Unit_Info
     (Unit  : in Unit_Id;
      Info  : out Unit_Info;
      Error : in out Error_Type);
   --  Return unit info on these unit. If status is unknown, then ask
   --  a boot mirror for a copy of unit info.

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Global message receiver. This Handle_Request is quite complex
   --  because it interacts rather strongly with the group
   --  communication package.

   procedure Invalidate_Unit_List
     (Partition : in Partition_ID);
   --  Modify status of units configured on Partition. This final status is
   --  INVALID when Partition reconnection mode is Reject_On_Restart or
   --  Fail_Until_Restart, UNDEFINED when reconnection is
   --  Block_Until_Restart.

   procedure Store_New_Unit
     (Unit      : in Unit_Id;
      Partition : in Partition_ID;
      Receiver  : in Unsigned_64;
      Version   : in Version_Type;
      Status    : in Unit_Status;
      Pending   : in out Request_List);
   --  Fill new unit slot and link unit into the partition unit list.
   --  Return a pending requests list. Basically, this procedure
   --  merges the new info with the old info already present. For
   --  instance, an update from INVALID to DEFINED will be ignored
   --  because the sender does not know that a particular partition
   --  has been invalidated.

   function Get_First_Remote_Unit
     (PID  : Partition_ID)
     return Unit_Id;
   --  Return the first unit configured on this partition.

   procedure Set_First_Remote_Unit
     (PID  : in Partition_ID;
      Unit : in Unit_Id);
   --  Assign Unit as the first remote unit of this partition.

   procedure Read_Units
     (Stream : access Params_Stream_Type;
      List   : out Request_List;
      Error  : in out Error_Type);

   procedure Write_Units
     (Stream : access Params_Stream_Type);
   --  Marshal and unmarshal the units table. We marshal this table
   --  per partition seq1 = (True, Partition_ID, seq2). Then, we
   --  marshal each unit of this partition. seq2 = (True, Name,
   --  Receiver, Partition, Status). This ends up with False to
   --  indicate the end of one of the list.

   type Elab_Unit_Node;
   type Elab_Unit_List is access Elab_Unit_Node;
   type Elab_Unit_Node is
      record
         PID      : Partition_ID;
         Name     : String_Access;
         Version  : Version_Type;
         Receiver : Interfaces.Unsigned_64;
         Previous : Elab_Unit_List;
         Next     : Elab_Unit_List;
      end record;

   Elab_Units : Elab_Unit_List;
   --  List of elaborated units to register to the boot server.

   procedure Free is
     new Ada.Unchecked_Deallocation (Elab_Unit_Node, Elab_Unit_List);

   Define_Units : constant Request_Type := (Kind => Define_New_Units);
   Copy_Units   : constant Request_Type := (Kind => Copy_Units_Table);
   Push_Units   : constant Request_Type := (Kind => Push_Units_Table);
   --  Shortcuts.

   -----------------------------
   -- Answer_Pending_Requests --
   -----------------------------

   procedure Answer_Pending_Requests (List : in Request_List) is
      Root : Request_Id := List;
      PID  : Partition_ID;
   begin
      while Root /= Null_Request_List loop
         Extract (Root, PID);

         if Self_PID /= PID then
            --  Send the whole unit table even if the request was on a
            --  specific unit.

            declare
               Reply : aliased Params_Stream_Type (0);
               Error : Error_Type;
            begin
               pragma Debug (D ("Handling pending request for partition " &
                                Partition_ID'Image (PID)));
               Request_Type'Output (Reply'Access, Push_Units);
               Write_Units         (Reply'Access);
               Send (PID, Unit_Name_Service, Reply'Access, Error);
               Deallocate (Reply);
               Catch (Error);
               pragma Debug (D ("Request for partition" &
                                Partition_ID'Image (PID) & " handled"));
            end;
         end if;
      end loop;
   exception
      when Error : others =>
         pragma Debug (D ("Exception raised in Answer_Pending_Requests: " &
                          Ada.Exceptions.Exception_Information (Error)));
         raise;
   end Answer_Pending_Requests;

   -----------------------
   -- Dump_Request_List --
   -----------------------

   function Dump_Request_List
     (List : Request_List;
      Root : Boolean := True) return String
   is
      Info : Request_Info;
   begin
      if List /= Null_Request_List then
         Info := Requests.Get_Component (List);
         return Info.PID'Img & Dump_Request_List (Info.Next, False);
      end if;
      if True then
         return " no partition";
      else
         return "";
      end if;
   end Dump_Request_List;

   --------------------
   -- Dump_Unit_Info --
   --------------------

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info)
   is
   begin
      D ("* Unit " & Units.Get_Name (Unit) & " -" & Unit'Img);
      D ("   Partition    "  & Info.Partition'Img);
      D ("   Receiver     "  & Info.Receiver'Img);
      if Info.Version /= Null_Version then
         D ("   Version       " & String (Info.Version));
      else
         D ("   Version       <no version>");
      end if;
      if Info.Requests /= Null_Request_List then
         D ("   Requests     " & Dump_Request_List (Info.Requests));
      end if;
      D ("   Status        " & Info.Status'Img);
      D ("   Next Unit    " & Info.Next_Unit'Img);
   end Dump_Unit_Info;

   ---------------------
   -- Dump_Unit_Table --
   ---------------------

   procedure Dump_Unit_Table
   is
      Unit  : Unit_Id;
      Info  : Unit_Info;
      PIDs  : Partition_List := Known_Partitions;
   begin
      D ("Unit Info Table");
      D ("---------------");
      for I in PIDs'Range loop
         Unit := Get_First_Remote_Unit (PIDs (I));
         if Unit /= Null_Unit_Id then
            D ("** Partition" & PIDs (I)'Img);
         end if;
         while Unit /= Null_Unit_Id loop
            Info := Units.Get_Component (Unit);
            Dump_Unit_Info (Unit, Info);
            Unit := Info.Next_Unit;
         end loop;
      end loop;
      D ("** Not Yet Configured Units");
      for U in First_Unit_Id .. Units.Last loop
         Info := Units.Get_Component (U);
         if Info.Requests /= Null_Request_List then
            Dump_Unit_Info (U, Info);
         end if;
      end loop;
   end Dump_Unit_Table;

   -------------
   -- Extract --
   -------------

   procedure Extract
     (List : in out Request_List;
      PID  : out Partition_ID)
   is
      Info : Request_Info;
   begin
      Info := Requests.Get_Component (List);
      Requests.Set_Component (List, Null_Request_Info);
      PID  := Info.PID;
      List := Info.Next;
   end Extract;

   ---------------------------
   -- Get_First_Remote_Unit --
   ---------------------------

   function Get_First_Remote_Unit
     (PID : Partition_ID)
     return Unit_Id
   is
      Root : Unit_Id;
   begin
      Root := Units.Get_Index ("root " & PID'Img);
      return Units.Get_Component (Root).Next_Unit;
   end Get_First_Remote_Unit;

   -------------------
   -- Get_Partition --
   -------------------

   procedure Get_Partition
     (Unit      : in Types.Unit_Id;
      Partition : out Types.Partition_ID;
      Error     : in out Error_Type)
   is
      Info : Unit_Info;
   begin
      Get_Unit_Info (Unit, Info, Error);
      if Info.Status = Invalid then
         Throw (Error, "Partition" & Info.Partition'Img & " is unreachable");
      end if;
      Partition := Info.Partition;
   end Get_Partition;

   ------------------
   -- Get_Receiver --
   ------------------

   procedure Get_Receiver
     (Unit     : in Types.Unit_Id;
      Receiver : out Interfaces.Unsigned_64;
      Error    : in out Error_Type)
   is
      Info : Unit_Info;
   begin
      Get_Unit_Info (Unit, Info, Error);
      if Info.Status = Invalid then
         Throw (Error, "Partition" & Info.Partition'Img & " is unreachable");
      end if;
      Receiver := Info.Receiver;
   end Get_Receiver;

   -----------------
   -- Get_Unit_Id --
   -----------------

   function Get_Unit_Id (Name : String) return Types.Unit_Id is
   begin
      return Units.Get_Index (Name);
   end Get_Unit_Id;

   -----------------
   -- Get_Version --
   -----------------

   procedure Get_Version
     (Unit    : in Types.Unit_Id;
      Version : out Types.Version_Type;
      Error   : in out Error_Type)
   is
      Info : Unit_Info;
   begin
      Get_Unit_Info (Unit, Info, Error);
      Version := Info.Version;
   end Get_Version;

   -------------------
   -- Get_Unit_Info --
   -------------------

   procedure Get_Unit_Info
     (Unit  : in Unit_Id;
      Info  : out Unit_Info;
      Error : in out Error_Type)
   is
      Version : Version_Id;
      Current : Unit_Info;
   begin
      pragma Assert (Unit /= Null_Unit_Id);

      if Elab_Units /= null then

         --  If the partition is not yet fully elaborated, then look
         --  for unit in the local unit list. Otherwise, we will have
         --  to wait for info from other partitions.

         Soft_Links.Enter_Critical_Section;
         declare
            Name : String := Units.Get_Name (Unit);
            List : Elab_Unit_List := Elab_Units;
         begin
            while List /= null loop
               if List.Name.all = Name then
                  Info.Next_Unit := Null_Unit_Id;
                  Info.Partition := Self_PID;
                  Info.Receiver  := List.Receiver;
                  Info.Version   := List.Version;
                  Info.Status    := Declared;
                  Info.Requests  := Null_Request_List;
                  Soft_Links.Leave_Critical_Section;
                  return;
               end if;
               List := List.Next;
            end loop;
         end;
         Soft_Links.Leave_Critical_Section;
      end if;

      loop
         Current := Units.Get_Component (Unit);

         exit when Current.Status in Defined .. Invalid;

         pragma Debug
           (D ("Looking for information on unit "&  Units.Get_Name (Unit)));

         Units.Enter;
         Current := Units.Get_Component (Unit);

         --  In the meantime, unit can be different. Check again.

         if not Is_Boot_Server
           and then Current.Status in Undefined .. Declared
         then
            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Request_Type'Output (Query'Access, (Kind => Pull_Units_Table));
               String'Output (Query'Access, Units.Get_Name (Unit));
               Send_Boot_Server (Unit_Name_Service, Query'Access, Error);
               if Found (Error) then
                  Units.Leave;
                  return;
               end if;
            end;
            Current.Status := Queried;
            Units.Set_Component (Unit, Current);
         end if;

         Units.Leave (Version);
         exit when Current.Status in Defined .. Invalid;

         Units.Differ (Version);
      end loop;

      Info := Current;
   end Get_Unit_Info;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type)
   is
      To_All  : aliased Params_Stream_Type (0);
      Pending : Request_List := Null_Request_List;
      Request : Request_Type;
      Unit    : Unit_Id;
      Info    : Unit_Info;
   begin
      Request := Request_Type'Input (Query);

      pragma Debug
        (D ("Receive from partition" & Partition'Img &
            " request " & Request.Kind'Img));

      Units.Enter;

      case Request.Kind is
         when Pull_Units_Table =>

            --  A Pull_Units_Table request is always on a given unit.  The
            --  sender does not want to poll in order to get info on this
            --  unit. So, if info is not available, keep track of the
            --  request.

            Unit := Units.Get_Index (String'Input (Query));
            Info := Units.Get_Component (Unit);

            case Info.Status is
               when Defined | Invalid =>
                  pragma Debug (D (Units.Get_Name (Unit) & " is known"));

                  --  Reply with the whole unit list.

                  Request_Type'Output (Reply, Push_Units);
                  Write_Units (Reply);

               when Declared | Undefined =>
                  pragma Debug
                    (D ("Queuing request from" & Partition'Img &
                        " on " & Units.Get_Name (Unit)));

                  --  Insert this request in order to answer it when
                  --  info is available.

                  Insert (Info.Requests, Partition);
                  Units.Set_Component (Unit, Info);

               when Queried =>
                  pragma Assert (False);
                  null;

            end case;

         when Copy_Units_Table =>

            --  Merge local unit list with the list we received.

            Read_Units (Query, Pending, Error);
            if Found (Error) then
               Units.Leave;
               return;
            end if;

            declare
               List  : Request_List := Pending;
               Info  : Request_Info;
               Again : Boolean := False;
            begin

               --  Having a pending request for the current partition means
               --  that it has to send a second copy of the unit info table.
               --  Some units were only DECLARED but are now DEFINED.

               while List /= Null_Request_List loop
                  Info := Requests.Get_Component (List);
                  if Info.PID = Self_PID then
                     Again := True;
                     exit;
                  end if;
                  List := Info.Next;
               end loop;
               Answer_Pending_Requests (Pending);

               --  This is a group request. If it has started on this
               --  partition, then do not follow up except when a
               --  second pass is explicitly required. The second pass
               --  will be handled at the group communication level.

               if Partition /= Self_PID then
                  Request_Type'Output (Query, Copy_Units);
                  Write_Units (Query);

               elsif Again then
                  Request_Type'Output (Reply, Copy_Units);
                  Write_Units (Reply);
               end if;
            end;

         when Define_New_Units |
           Push_Units_Table =>

            --  Merge the local unit list with the list we received.

            Read_Units (Query, Pending, Error);
            if Found (Error) then
               Units.Leave;
               return;
            end if;

            --  If there are new units and if this partition is a boot
            --  mirror, then broadcast the new unit table to the other
            --  boot mirrors. If so, we will answer to pending
            --  requests when the current partition receives its copy
            --  back.

            if Request.Kind = Define_New_Units
              and then Is_Boot_Mirror
            then
               if N_Boot_Mirrors > 1 then
                  Request_Type'Output (To_All'Access, Copy_Units);
                  Write_Units (To_All'Access);

               else
                  Answer_Pending_Requests (Pending);
               end if;
            end if;

         when Invalidate_Units =>

            --  This does not work like partitions ???

            Invalidate_Unit_List (Request.Partition);
            if Is_Boot_Server
              and then N_Boot_Mirrors > 1
            then
               Request_Type'Output (To_All'Access, Copy_Units);
               Write_Units (To_All'Access);
            end if;

      end case;

      pragma Debug (Dump_Unit_Table);

      Units.Leave;

      --  Send the new table to boot mirrors group.

      if not Empty (To_All'Access) then
         Broadcast (Unit_Name_Service, To_All'Access);
      end if;

   exception when others =>
      Throw (Error, "Data error in Units.Handle_Request");
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Units.Initialize;
      Requests.Initialize;
      Register_Handler (Unit_Name_Service, Handle_Request'Access);
   end Initialize;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List : in out Request_List;
      PID  : in Partition_ID)
   is
      Info    : Request_Info;
      Request : Request_Id;
      Last    : Request_Id;
   begin
      --  If this request has already been inserted then ignored.

      Request := List;
      while Request /= Null_Request_Id loop
         Info := Requests.Get_Component (Request);
         if Info.PID = PID then
            return;
         end if;
         Request := Info.Next;
      end loop;

      Last    := Requests.Last + 1;
      Request := First_Request_Id;
      while Request <= Last loop
         Info := Requests.Get_Component (Request);
         exit when Info = Null_Request_Info;
         Request := Request + 1;
      end loop;
      Requests.Set_Component (Request, (PID, List));

      List := Request;
   end Insert;

   --------------------------------
   -- Invalidate_Partition_Units --
   --------------------------------

   procedure Invalidate_Partition_Units
     (Partition : in Partition_ID)
   is
      Query  : aliased Params_Stream_Type (0);
      To_All : aliased Params_Stream_Type (0);
      Error  : Error_Type;
   begin
      if Shutdown_Activated then
         return;
      end if;

      Units.Enter;
      Invalidate_Unit_List (Partition);

      if Is_Boot_Mirror then
         if N_Boot_Mirrors > 1 then
            Request_Type'Output (To_All'Access, Copy_Units);
            Write_Units (To_All'Access);
         end if;

      else
         Request_Type'Output (Query'Access, (Invalidate_Units, Partition));
      end if;

      Units.Leave;

      if not Empty (To_All'Access) then
         Broadcast (Unit_Name_Service, To_All'Access);

      elsif not Empty (Query'Access) then
         Send_Boot_Server (Unit_Name_Service, Query'Access, Error);
         Catch (Error);
      end if;
   end Invalidate_Partition_Units;

   --------------------------
   -- Invalidate_Unit_List --
   --------------------------

   procedure Invalidate_Unit_List
     (Partition : in Partition_ID)
   is
      Status       : Unit_Status;
      Unit         : Unit_Id;
      Info         : Unit_Info;
      Reconnection : Reconnection_Type;
      Error        : Error_Type;
   begin
      if Shutdown_Activated then
         return;
      end if;

      --  If we have problems to get the reconnection policy, then we
      --  cannot reach a boot server and the shutdown will be decided later
      --  on. Ignore this invalidation anyway.

      Get_Reconnection_Policy (Partition, Reconnection, Error);
      if Found (Error) then
         Catch (Error);
         return;
      end if;

      --  If reconnection is blocked until restart, then we are in the
      --  situation similar to the one we have during elaboration. So,
      --  status should be Undefined.

      if Reconnection = Block_Until_Restart then
         Status := Undefined;
      else
         Status := Invalid;
      end if;

      Unit := Get_First_Remote_Unit (Partition);
      while Unit /= Null_Unit_Id loop
         Info := Units.Get_Component (Unit);
         Info.Status := Status;

         pragma Debug
           (D ("RCI unit " & Units.Get_Name (Unit) &
               " status is now " & Status'Img));

         Units.Set_Component (Unit, Info);
         Unit := Info.Next_Unit;
      end loop;
   end Invalidate_Unit_List;

   ----------------
   -- Read_Units --
   ----------------

   procedure Read_Units
     (Stream : access Params_Stream_Type;
      List   : out Request_List;
      Error  : in out Error_Type)
   is
      Unit      : Unit_Id;
      Partition : Partition_ID;
      Receiver  : Unsigned_64;
      Version   : Version_Type;
      Status    : Unit_Status;

   begin
      List := Null_Request_List;

      while Boolean'Input (Stream) loop
         Partition_ID'Read (Stream, Partition);
         pragma Debug
           (D ("Read units mapped on partition" & Partition'Img));

         --  Set_First_Remote_Unit (Partition, Null_Unit_Id);
         while Boolean'Input (Stream) loop
            Unit := Units.Get_Index (String'Input (Stream));
            pragma Debug
              (D ("Read unit info about " & Units.Get_Name (Unit)));

            Unsigned_64'Read  (Stream, Receiver);
            Version_Type'Read (Stream, Version);
            Unit_Status'Read  (Stream, Status);
            Store_New_Unit (Unit, Partition, Receiver, Version, Status, List);
         end loop;
      end loop;

      pragma Debug (D ("Requests from" & Dump_Request_List (List) &
                       " can be answered"));
      pragma Debug (Dump_Unit_Table);

   exception when others =>
      Throw (Error, "Data error in Units.Read_Units");
   end Read_Units;

   -------------------
   -- Register_Unit --
   -------------------

   procedure Register_Unit
     (Partition : in Partition_ID;
      Name      : in String;
      Receiver  : in Interfaces.Unsigned_64;
      Version   : in Types.Version_Type)
   is
      Node : Elab_Unit_List := new Elab_Unit_Node;

   begin
      --  We need this critical section because a package can be
      --  already elaborated and one of its tasks can ask for the
      --  partition id of a local rci unit.

      Soft_Links.Enter_Critical_Section;
      Node.all :=
        (PID      => Partition,
         Name     => new String'(Name),
         Version  => Version,
         Receiver => Receiver,
         Previous => null,
         Next     => Elab_Units);
      if Elab_Units /= null then
         Elab_Units.Previous := Node;
      end if;
      Elab_Units := Node;
      Soft_Links.Leave_Critical_Section;
   end Register_Unit;

   -----------------------------------
   -- Register_Units_On_Boot_Server --
   -----------------------------------

   procedure Register_Units_On_Boot_Server
     (Partition : in Partition_ID;
      Error     : in out Error_Type)
   is
      List  : Elab_Unit_List;
      Node  : Elab_Unit_List;
      Next  : Elab_Unit_List;
      Unit  : Unit_Id;
      Info  : Unit_Info;
      Query : aliased Params_Stream_Type (0);

   begin
      --  Elab_Units include the units of the current partition and
      --  the passive partitions. Extract from this list the units
      --  concerning this partition.

      Soft_Links.Enter_Critical_Section;
      Next := Elab_Units;
      while Next /= null loop
         Node := Next;
         Next := Node.Next;

         if Node.PID = Partition then

            --  This is a special case to remove the first element of
            --  the double linked list.

            if Node.Previous = null then
               Elab_Units := Elab_Units.Next;
               if Elab_Units /= null then
                  Elab_Units.Previous := null;
               end if;

            else
               Node.Previous.Next := Node.Next;
               if Node.Next /= null then
                  Node.Next.Previous := Node.Previous;
               end if;
            end if;

            --  Add this unit to the list of units registered to this
            --  partition.

            Node.Next     := List;
            Node.Previous := null;
            List          := Node;
         end if;
      end loop;
      Soft_Links.Leave_Critical_Section;

      if List /= null then
         Request_Type'Output (Query'Access, Define_Units);
         Boolean'Write       (Query'Access, True);
         Partition_ID'Write  (Query'Access, Partition);
      end if;

      Node := List;
      while Node /= null loop

         pragma Debug (D ("Register unit " & Node.Name.all));

         Boolean'Write      (Query'Access, True);
         String'Output      (Query'Access, Node.Name.all);
         Unsigned_64'Write  (Query'Access, Node.Receiver);
         Version_Type'Write (Query'Access, Node.Version);
         Unit_Status'Write  (Query'Access, Declared);

         Node := Node.Next;
      end loop;

      --  Ask for unit info back.

      if not Empty (Query'Access) then
         Boolean'Write (Query'Access, False);
         Boolean'Write (Query'Access, False);
         Send_Boot_Server (Unit_Name_Service, Query'Access, Error);
         if Found (Error) then
            return;
         end if;
      end if;

      --  Info should be the same. Otherwise, there is a conflict and
      --  these units are duplicated. The partition has to shutdown.

      while List /= null loop
         Unit := Units.Get_Index (List.Name.all);
         Get_Unit_Info (Unit, Info, Error);
         if Found (Error) then
            return;
         end if;

         if Info.Status /= Defined
           or else Info.Partition /= Partition
         then
            pragma Debug (D ("info.status    " & Info.Status'Img));
            pragma Debug (D ("info.partition"  & Info.Partition'Img));
            pragma Debug (D ("partition     "  & Partition'Img));

            pragma Debug (D ("unit " & List.Name.all &
                             " is already declared"));
            Activate_Shutdown;
            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity,
               "unit " & List.Name.all & " is already declared");
         end if;

         Node := List;
         List := List.Next;

         Destroy (Node.Name);
         Free (Node);
      end loop;
   end Register_Units_On_Boot_Server;

   ---------------------------
   -- Set_First_Remote_Unit --
   ---------------------------

   procedure Set_First_Remote_Unit
     (PID  : in Partition_ID;
      Unit : in Unit_Id)
   is
      Root : Unit_Id;
      Info : Unit_Info;
   begin
      Root := Units.Get_Index ("root " & PID'Img);
      Info := Units.Get_Component (Root);
      Info.Next_Unit := Unit;
      Units.Set_Component (Root, Info);
   end Set_First_Remote_Unit;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      --  Resume tasks waiting for an update of units info table.

      Units.Update;
   end Shutdown;

   --------------------
   -- Store_New_Unit --
   --------------------

   procedure Store_New_Unit
     (Unit      : in Unit_Id;
      Partition : in Partition_ID;
      Receiver  : in Unsigned_64;
      Version   : in Version_Type;
      Status    : in Unit_Status;
      Pending   : in out Request_List)
   is
      Current_Info      : Unit_Info := Units.Get_Component (Unit);
      Current_Status    : Unit_Status renames Current_Info.Status;
      Current_Partition : Partition_ID renames Current_Info.Partition;

   begin

      --  If a request is supposed to change the status from INVALID
      --  to DEFINED, then this request should be ignored. The sender
      --  is not yet aware that the partition has been invalidated
      --  with all its units.

      if Current_Status = Invalid and then Status = Defined then
         pragma Debug (D ("Ignoring late request to define unit " &
                          Units.Get_Name (Unit)));
         return;
      end if;

      --  If the status is DECLARED when it is already set to DEFINED on
      --  this partition, then a partition tries to register twice an unit.

      if Current_Status = Defined and then Status = Declared then
         pragma Debug (D ("Ignoring multiple registration of unit " &
                          Units.Get_Name (Unit)));
         return;
      end if;

      if Current_Status = Invalid and then Status = Declared then

         --  Check reconnection policy and exit when the unit must be
         --  configured on a partition with Reject_On_Restart policy.

         pragma Debug (D ("Checking renewed unit " & Units.Get_Name (Unit)));

         declare
            Reconnection : Reconnection_Type;
            Error        : Error_Type;
         begin
            Get_Reconnection_Policy
              (Current_Info.Partition, Reconnection, Error);
            if Found (Error) then
               Catch (Error);
               return;
            end if;
            if Reconnection = Reject_On_Restart then
               pragma Debug (D ("Rejecting unit " & Units.Get_Name (Unit) &
                                "on restart"));
               return;
            end if;
         end;
      end if;

      if Current_Partition /= Null_PID
        and then Current_Partition /= Partition
        and then (Current_Status = Undefined
                  or else Current_Status = Invalid
                  or else Current_Status = Queried)
      then
         --  A unit may have an UNDEFINED status once its partition has
         --  been invalidated. This comes from a partition reconnection
         --  mode set to Block_Until_Restart. But Status is supposed to
         --  evolve as follow: UNDEFINED -> QUERIED -> DEFINED ->
         --  INVALID. With the reconnection mode above, the status evolves
         --  from Defined to Undefined. To set its status back to DEFINED,
         --  the partition id has to be different. Note that we have to
         --  remove the current unit from the previous partition units
         --  list.

         declare
            Previous_Unit : Unit_Id;
            Previous_Info : Unit_Info;

         begin
            Previous_Unit := Get_First_Remote_Unit (Current_Partition);
            if Previous_Unit = Unit then
               Previous_Info := Units.Get_Component (Previous_Unit);
               Set_First_Remote_Unit
                 (Current_Partition, Previous_Info.Next_Unit);

            else
               loop
                  Previous_Info := Units.Get_Component (Previous_Unit);
                  exit when Previous_Info.Next_Unit = Unit;
                  Previous_Unit := Previous_Info.Next_Unit;
                  pragma Assert (Previous_Unit /= Null_Unit_Id);
               end loop;
               Previous_Info.Next_Unit := Current_Info.Next_Unit;
               Units.Set_Component (Previous_Unit, Previous_Info);
            end if;

            pragma Debug (D ("Dequeuing unit " & Units.Get_Name (Unit) &
                             " from partition" &
                             Partition_ID'Image (Current_Partition)));
         end;
      end if;


      --  When Current_Info.Status and Status are both set to
      --  DECLARED, we have to resolve a conflict.  We discard the
      --  unit declared by the partition of greater partition id. If
      --  the unit is declared by a partition whose boot partition is
      --  the current partition, then the token has performed a full
      --  pass and the unit status is no longer DECLARED but
      --  DEFINED. That means it has been accepted by all the boot
      --  mirrors. As the status of this unit has been modified, we
      --  need to send a new copy of the table to the other boot
      --  mirrors.  For this purpose, Pending (Self_PID) is set to
      --  true. That means the current partition has the answer to its
      --  pending request that was whether or not the unit has been
      --  successfully defined.

      if Current_Status = Declared
        and then Status = Declared
      then
         if Current_Partition < Partition then
            pragma Debug (D ("Ignoring late conflict on unit " &
                             Units.Get_Name (Unit)));
            return;

         else
            declare
               Boot  : Partition_ID;
               Error : Error_Type;
            begin
               Get_Boot_Partition (Partition, Boot, Error);
               pragma Debug (D ("boot_partition (" &
                                Partition'Img &
                                ") =" & Boot'Img));
               if not Found (Error)
                 and then Boot = Self_PID
               then
                  pragma Debug (D ("Defining unit " & Units.Get_Name (Unit)));
                  Current_Status := Defined;
                  Insert (Pending, Self_PID);

               else
                  pragma Debug (D ("Declaring unit " & Units.Get_Name (Unit)));
                  Current_Status := Declared;
               end if;
               Catch (Error);
            end;
         end if;

      elsif Status = Declared
        and then Options.Is_Boot_Server
        and then N_Boot_Mirrors = 1
      then

         --  If the case below, we do not need a group consensus. Just
         --  set the status to Defined. There is no possible conflict.

         pragma Debug (D ("Defining (forced) unit " & Units.Get_Name (Unit)));
         Current_Status := Defined;

      else
         Current_Status := Status;
      end if;

      --  Add this unit in the partition unit list

      if Current_Status in Declared .. Invalid
        and then Current_Partition /= Partition
      then
         pragma Debug
           (D ("Add unit " & Units.Get_Name (Unit) &
               " to partition" & Partition'Img));

         Current_Info.Next_Unit := Get_First_Remote_Unit (Partition);
         Set_First_Remote_Unit (Partition, Unit);
      end if;

      Current_Info.Receiver  := Receiver;
      Current_Info.Version   := Version;
      Current_Info.Partition := Partition;

      --  Add pending requests to Pending and mark them as handled

      if Current_Status in Defined .. Invalid then
         if Current_Info.Requests /= Null_Request_List then
            pragma Debug (D ("Dequeuing pending requests for unit " &
                             Units.Get_Name (Unit)));
            declare
               PID : Partition_ID;
            begin
               while Current_Info.Requests /= Null_Request_List loop
                  Extract (Current_Info.Requests, PID);
                  Insert  (Pending, PID);
               end loop;
            end;
         end if;
      end if;

      Units.Set_Component (Unit, Current_Info);
   end Store_New_Unit;

   -----------------
   -- Write_Units --
   -----------------

   procedure Write_Units
     (Stream : access Params_Stream_Type)
   is
      PIDs : Partition_List := Known_Partitions;
      Unit : Unit_Id;
      Info : Unit_Info;
   begin
      for I in PIDs'Range loop
         Unit := Get_First_Remote_Unit (PIDs (I));
         if Unit /= Null_Unit_Id then
            Boolean'Write      (Stream, True);
            Partition_ID'Write (Stream, PIDs (I));
            while Unit /= Null_Unit_Id loop
               Info := Units.Get_Component (Unit);
               if Info.Status /= Queried then
                  Boolean'Write      (Stream, True);
                  String'Output      (Stream, Units.Get_Name (Unit));
                  Unsigned_64'Write  (Stream, Info.Receiver);
                  Version_Type'Write (Stream, Info.Version);
                  Unit_Status'Write  (Stream, Info.Status);
               end if;
               Unit := Info.Next_Unit;
            end loop;
            Boolean'Write      (Stream, False);
         end if;
      end loop;
      Boolean'Write      (Stream, False);
   end Write_Units;

end System.Garlic.Units;
