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
with Interfaces;                use Interfaces;
with Unchecked_Deallocation;
with System.Garlic.Debug;       use System.Garlic.Debug;
with System.Garlic.Group;       use System.Garlic.Group;
with System.Garlic.Heart;       use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Options;     use System.Garlic.Options;
with System.Garlic.Partitions;  use System.Garlic.Partitions;
with System.Garlic.Streams;     use System.Garlic.Streams;
with System.Garlic.Types;       use System.Garlic.Types;
with System.Garlic.Utils;       use System.Garlic.Utils;

package body System.Garlic.Units is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GARUNI", "(s-garuni): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   procedure Answer_Pending_Requests
     (List : Request_List);
   --  A boot server or mirror can receive a request on an unit for which
   --  it has no info on it yet. So, we keep track of this request in order
   --  to answer it later on. When info becomes available, answer to
   --  pending requests.

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info);

   procedure Dump_Unit_Table;

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type;
      Error     : in out Error_Type);
   --  Global message receiver. This Handle_Request is quite complex
   --  because it interacts rather strongly with the group communication
   --  package. For normal requests (other than Copy_Unit_Table), Query and
   --  Reply have the usual meaning. For Copy_Unit_Table, Query can be
   --  updated because a followup is intented. When Query is empty, there
   --  is another case of followup with Reply. This is a request for a new
   --  communication broadcast. There is no other way to do this because
   --  Handle_Request is invoked asynchronously and we need to use the
   --  current task to start the next group request.

   procedure Invalidate_Unit_List
     (Partition : in Partition_ID);
   --  Modify status of units configured on Partition. This final status is
   --  Invalid when Partition reconnection mode is Rejected_On_Restart or
   --  Failed_Until_Restart, Undefined when reconnection is
   --  Blocked_Until_Restart.

   procedure Store_New_Unit
     (Unit      : in Unit_Id;
      Partition : in Partition_ID;
      Receiver  : in Unsigned_64;
      Version   : in String_Access;
      Status    : in Unit_Status;
      Pending   : in out Request_List);
   --  Fill new unit slot and link unit into the partition unit list.
   --  Return a pending requests list. Basically, this procedure *merges*
   --  the new info with the old info already present. For instance, an
   --  update from Invalidated to Defined will be ignored because the
   --  sender does know that a particular partition has been invalidated.

   procedure Read_Units
     (Stream : access Params_Stream_Type;
      List   : out Request_List);
   procedure Write_Units
     (Stream : access Params_Stream_Type);
   --  Marshal and unmarshal the units table. We marshal this table
   --  par partition seq1 = (True, Partition_ID, seq2). Then, we
   --  marshal each unit of this partition. seq2 = (True, Name, Receiver,
   --  Partition, Status).


   --  Units on a same partition are chained together. This is the root of
   --  the list.

   Units_Per_Partition : array (Valid_Partition_ID) of Unit_Id;


   --  This is a list of receiver units to register when
   --  Establish_RPC_Receiver is called.

   type Receiver_Node;
   type Receiver_List is access Receiver_Node;
   type Receiver_Node is
      record
         Name     : String_Access;
         Version  : String_Access;
         Receiver : Interfaces.Unsigned_64;
         Next     : Receiver_List;
      end record;
   Receivers : Receiver_List;

   procedure Free is new Unchecked_Deallocation (Receiver_Node, Receiver_List);


   --  Basic requests

   Define_Units : constant Request_Type := (Kind => Define_New_Units);
   Copy_Units   : constant Request_Type := (Kind => Copy_Units_Table);
   Push_Units   : constant Request_Type := (Kind => Push_Units_Table);


   -----------------------------
   -- Answer_Pending_Requests --
   -----------------------------

   procedure Answer_Pending_Requests
     (List : Request_List)
   is
      Reply : aliased Params_Stream_Type (0);
      Error : Error_Type := No_Error;
   begin
      for PID in List'Range loop
         if List (PID) then

            --  Send the whole table even if the request was on a specific
            --  unit.

            Request_Type'Output (Reply'Access, Push_Units);
            Write_Units         (Reply'Access);
            Send (PID, Unit_Name_Service, Reply'Access, Error);
            Catch (Error);
         end if;
      end loop;
   end Answer_Pending_Requests;

   --------------------
   -- Dump_Unit_Info --
   --------------------

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info) is
   begin
      D (D_Dump, "* Unit " & Units.Get_Name (Unit));
      D (D_Dump, "   Partition    "  & Info.Partition'Img);
      D (D_Dump, "   Receiver     "  & Info.Receiver'Img);
      if Info.Version /= null then
         D (D_Dump, "   Version       " & Info.Version.all);
      else
         D (D_Dump, "   Version       <no version>");
      end if;
      D (D_Dump, "   Status        " & Info.Status'Img);
      D (D_Dump, "   Next Unit    " & Info.Next_Unit'Img);
   end Dump_Unit_Info;

   ---------------------
   -- Dump_Unit_Table --
   ---------------------

   procedure Dump_Unit_Table
   is
      Unit : Unit_Id;
      Info : Unit_Info;
   begin
      D (D_Dump, "Unit Info Table");
      D (D_Dump, "---------------");
      for P in Units_Per_Partition'Range loop
         Unit := Units_Per_Partition (P);
         if Unit /= Null_Unit_Id then
            D (D_Dump, "** Partition" & P'Img);
         end if;
         while Unit /= Null_Unit_Id loop
            Info := Units.Get_Component (Unit);
            Dump_Unit_Info (Unit, Info);
            Unit := Info.Next_Unit;
         end loop;
      end loop;
   end Dump_Unit_Table;

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
      loop
         Current := Units.Get_Component (Unit);

         exit when Current.Status in Defined .. Invalid;

         pragma Debug
           (D (D_Debug,
               "Looking for information on unit "&  Units.Get_Name (Unit)));
         Dump_Unit_Info (Unit, Current);

         Units.Enter;
         Current := Units.Get_Component (Unit);

         --  In the meantime, unit status can be different. Check again.

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
      To_All  : Boolean;
      Token   : aliased Params_Stream_Type (0);
      Pending : Request_List := Null_List;
      Request : Request_Type;
      Unit    : Unit_Id;
      Info    : Unit_Info;
   begin
      Request := Request_Type'Input (Query);

      pragma Debug
        (D (D_Warning,
            "Receive from partition" & Partition'Img &
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
                  pragma Debug
                    (D (D_Debug, Units.Get_Name (Unit) & " is known"));

                  Request_Type'Output (Reply, Push_Units);
                  Write_Units (Reply);

               when Declared | Undefined =>
                  pragma Debug
                    (D (D_Debug,
                        "Queuing request from" & Partition'Img &
                        " on " & Units.Get_Name (Unit)));

                  --  Queue this request in order to answer it when info is
                  --  available.

                  Info.Pending := True;
                  Info.Requests (Partition) := True;
                  Units.Set_Component (Unit, Info);

               when Queried =>
                  pragma Assert (False);
                  null;

            end case;

            --  No group communication

            To_All := False;

         when Copy_Units_Table =>

            Read_Units (Query, Pending);

            declare
               Second_Pass : Boolean;
            begin

               --  Having a pending request for the current partition means
               --  that it has to send a second copy of the unit info table.
               --  Some units were only Declared but are now Defined.

               Second_Pass := Pending (Self_PID);
               Pending (Self_PID) := False;
               Answer_Pending_Requests (Pending);

               --  This is a group request. If it has started on this
               --  partition, then do not follow up except when a second
               --  pass is explicitly required. The second pass will handle
               --  at the group communication level.

               if Partition /= Self_PID then
                  Request_Type'Output (Query, Copy_Units);
                  Write_Units (Query);

               elsif Second_Pass then
                  Request_Type'Output (Reply, Copy_Units);
                  Write_Units (Reply);
               end if;
            end;

            --  No group communication

            To_All := False;

         when Define_New_Units |
           Push_Units_Table =>

            Read_Units (Query, Pending);
            To_All := Is_Boot_Server
              and then (Request.Kind = Define_New_Units);

         when Invalidate_Units =>

            Invalidate_Unit_List (Request.Partition);
            To_All := Is_Boot_Server;

      end case;

      --  For some units table modifications, we send the new table to boot
      --  mirrors group. We marshal the table while we still have a lock on
      --  it.

      if To_All then
         Request_Type'Output (Token'Access, Copy_Units);
         Write_Units (Token'Access);
      end if;

      Dump_Unit_Table;

      Units.Leave;

      --  Send the new table to boot mirrors group.

      if not Empty (Token'Access) then
         Broadcast (Unit_Name_Service, Token'Access, Error);
      end if;
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Handler (Unit_Name_Service, Handle_Request'Access);
   end Initialize;

   --------------------------------
   -- Invalidate_Partition_Units --
   --------------------------------

   procedure Invalidate_Partition_Units
     (Partition : in Partition_ID)
   is
      Query : aliased Params_Stream_Type (0);
      Error : Error_Type := No_Error;
   begin
      if Shutdown_In_Progress then
         return;
      end if;

      Units.Enter;
      Invalidate_Unit_List (Partition);
      Units.Leave;

      if Is_Boot_Mirror then
         Request_Type'Output (Query'Access, Copy_Units);
         Write_Units (Query'Access);
         Broadcast (Unit_Name_Service, Query'Access, Error);

      else
         Request_Type'Output (Query'Access, (Invalidate_Units, Partition));
         Send_Boot_Server (Unit_Name_Service, Query'Access, Error);
      end if;

      Catch (Error);
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
      Error        : Error_Type := No_Error;
   begin
      if Shutdown_In_Progress then
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

      if Reconnection = Blocked_Until_Restart then
         Status := Undefined;
      else
         Status := Invalid;
      end if;

      Unit := Units_Per_Partition (Partition);
      while Unit /= Null_Unit_Id loop
         Info := Units.Get_Component (Unit);
         Info.Status := Status;

         pragma Debug
           (D (D_Debug,
               "RCI unit " & Units.Get_Name (Unit) &
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
      List   : out Request_List)
   is
      Unit      : Unit_Id;
      Partition : Partition_ID;
      Receiver  : Unsigned_64;
      Version   : String_Access;
      Status    : Unit_Status;
   begin
      while Boolean'Input (Stream) loop
         Partition_ID'Read (Stream, Partition);
         pragma Debug
           (D (D_Debug, "Read units mapped on partition" & Partition'Img));

         while Boolean'Input (Stream) loop
            Unit := Units.Get_Index (String'Input (Stream));
            pragma Debug
              (D (D_Debug, "Read unit info about " & Units.Get_Name (Unit)));

            Unsigned_64'Read (Stream, Receiver);
            Version := new String'(String'Input (Stream));
            Unit_Status'Read (Stream, Status);
            Store_New_Unit (Unit, Partition, Receiver, Version, Status, List);
         end loop;
      end loop;
   end Read_Units;

   -------------------
   -- Register_Unit --
   -------------------

   procedure Register_Unit
     (Name     : in String;
      Receiver : in Interfaces.Unsigned_64;
      Version  : in Utils.String_Access)
   is
      Node : Receiver_List := new Receiver_Node;
   begin
      Node.Name     := new String'(Name);
      Node.Version  := Version;
      Node.Receiver := Receiver;
      Node.Next     := Receivers;
      Receivers     := Node;
   end Register_Unit;

   -----------------------------------
   -- Register_Units_On_Boot_Server --
   -----------------------------------

   procedure Register_Units_On_Boot_Server
     (Error : in out Error_Type)
   is
      Node  : Receiver_List := Receivers;
      Unit  : Unit_Id;
      Info  : Unit_Info;
      Query : aliased Params_Stream_Type (0);
   begin
      if Node /= null then
         Request_Type'Output (Query'Access, Define_Units);
         Boolean'Write       (Query'Access, True);
         Partition_ID'Write  (Query'Access, Self_PID);
      end if;

      while Node /= null loop
         pragma Debug (D (D_Debug, "Register unit " & Node.Name.all));

         Boolean'Write     (Query'Access, True);
         String'Output     (Query'Access, Node.Name.all);
         Unsigned_64'Write (Query'Access, Node.Receiver);
         String'Output     (Query'Access, Node.Version.all);
         Unit_Status'Write (Query'Access, Declared);

         Node  := Node.Next;
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

      while Receivers /= null loop
         Unit := Units.Get_Index (Receivers.Name.all);
         Get_Unit_Info (Unit, Info, Error);
         if Found (Error) then
            return;
         end if;

         if Info.Status /= Defined
           or else Info.Partition /= Self_PID
         then
            Soft_Shutdown;
            Ada.Exceptions.Raise_Exception
              (Program_Error'Identity,
               "RCI unit " & Receivers.Name.all & " is already declared");
         end if;

         Node      := Receivers;
         Receivers := Receivers.Next;

         Free (Node.Name);
         Free (Node.Version);
         Free (Node);
      end loop;
   end Register_Units_On_Boot_Server;

   --------------------
   -- Store_New_Unit --
   --------------------

   procedure Store_New_Unit
     (Unit      : in Unit_Id;
      Partition : in Partition_ID;
      Receiver  : in Unsigned_64;
      Version   : in String_Access;
      Status    : in Unit_Status;
      Pending   : in out Request_List)
   is
      Previous_Unit : Unit_Id;
      Previous_Info : Unit_Info;
      New_Unit_Info : Unit_Info;
   begin
      New_Unit_Info := Units.Get_Component (Unit);

      --  If a request is supposed to switch the status from Invalid to
      --  Defined, then this request should be ignored. The sender doesn't
      --  know yet that the partition has been invalidated with all its units.

      if Status = Defined
        and then New_Unit_Info.Status = Invalid
      then
         return;
      end if;

      --  If the status is Declared when it is already set to Defined on
      --  this partition, then a partition tries to register twice an unit.

      if Status = Declared
        and then New_Unit_Info.Status = Defined
      then
         return;
      end if;

      --  If the unit is already registered as a unit of a given partition,
      --  then find the unit previous to the current unit in the partition
      --  unit list. If this unit is the first one in the list, then
      --  the previous unit is the unit itself.

      if New_Unit_Info.Partition = Null_PID then
         Previous_Unit := Null_Unit_Id;
         Previous_Info := Null_Unit;

      else
         Previous_Unit := Units_Per_Partition (New_Unit_Info.Partition);
         if Previous_Unit /= Unit then
            while Previous_Unit /= Null_Unit_Id loop
               Previous_Info := Units.Get_Component (Previous_Unit);
               exit when Previous_Info.Next_Unit = Unit;
               Previous_Unit := Previous_Info.Next_Unit;
            end loop;
         end if;

         --  A unit may have an Undefined status once its partition has
         --  been invalidated. This comes from a partition reconnection
         --  mode set to Blocked_Until_Restart. But, Status is supposed to
         --  evolve as follow: Undefined -> Queried -> Defined ->
         --  Invalid. With the reconnection mode above, the status evolves
         --  from Defined to Undefined. To set its status back to Defined,
         --  the partition id has to be different. Note that we have to
         --  remove the current unit from the previous partition units
         --  list. Therefore, Previous_Unit is set back to Null_Unit_Id.

         if New_Unit_Info.Status = Undefined
           and then New_Unit_Info.Partition /= Partition
         then
            if Previous_Unit = Unit then
               Units_Per_Partition (New_Unit_Info.Partition)
                 := New_Unit_Info.Next_Unit;
            else
               Previous_Info.Next_Unit := New_Unit_Info.Next_Unit;
               Units.Set_Component (Previous_Unit, Previous_Info);
            end if;
            Previous_Unit := Null_Unit_Id;
         end if;
      end if;

      --  When Status and New_Unit_Info.Status are both set to Declared, we
      --  have to resolve a conflict.  We discard the unit declared by the
      --  partition of greater partition id. If the unit is declared by a
      --  partition whose boot partition is the current partition, then the
      --  token has performed a full pass and the unit status is no longer
      --  Declared but Defined. That means it has been accepted by all the
      --  boot mirrors. As the status of this unit has been modified, we
      --  need to send a new copy of the table to the other boot mirrors.
      --  For this purpose, Pending (Self_PID) is set to true. That means
      --  the current partition has the answer to its pending request that
      --  was whether or not the unit has been successfully defined.

      if Status = Declared
        and then New_Unit_Info.Status = Declared
      then
         if New_Unit_Info.Partition < Partition then
            return;
         elsif Boot_Partition (Partition) = Self_PID then
            New_Unit_Info.Status := Defined;
            Pending (Self_PID) := True;
         else
            New_Unit_Info.Status := Declared;
         end if;
      else
         New_Unit_Info.Status := Status;
      end if;

      New_Unit_Info.Receiver  := Receiver;
      New_Unit_Info.Version   := Version;
      New_Unit_Info.Partition := Partition;

      --  Add this unit in the partition unit list.

      if New_Unit_Info.Status in Declared .. Invalid then
         if Previous_Unit = Null_Unit_Id then
            pragma Debug
              (D (D_Debug,
                  "Add new unit " & Units.Get_Name (Unit) &
                  " to partition" & Partition'Img));

            New_Unit_Info.Next_Unit := Units_Per_Partition (Partition);
            Units_Per_Partition (Partition) := Unit;
         end if;
      end if;

      --  Dequeue pending requests

      if New_Unit_Info.Status in Defined .. Invalid then
         if New_Unit_Info.Pending then
            for PID in New_Unit_Info.Requests'Range loop
               if New_Unit_Info.Requests (PID) then
                  Pending (PID) := True;
                  New_Unit_Info.Requests (PID) := False;
               end if;
            end loop;
            New_Unit_Info.Pending := False;
         end if;
      end if;

      Units.Set_Component (Unit, New_Unit_Info);
   end Store_New_Unit;

   -----------------
   -- Write_Units --
   -----------------

   procedure Write_Units
     (Stream : access Params_Stream_Type)
   is
      Unit : Unit_Id;
      Info : Unit_Info;
   begin
      for P in Units_Per_Partition'Range loop
         Unit := Units_Per_Partition (P);
         if Unit /= Null_Unit_Id then
            Boolean'Write      (Stream, True);
            Partition_ID'Write (Stream, P);
            while Unit /= Null_Unit_Id loop
               Info := Units.Get_Component (Unit);
               if Info.Status /= Queried then
                  Boolean'Write     (Stream, True);
                  String'Output     (Stream, Units.Get_Name (Unit));
                  Unsigned_64'Write (Stream, Info.Receiver);
                  String'Output     (Stream, Info.Version.all);
                  Unit_Status'Write (Stream, Info.Status);
               end if;
               Unit := Info.Next_Unit;
            end loop;
            Boolean'Write      (Stream, False);
         end if;
      end loop;
      Boolean'Write      (Stream, False);
   end Write_Units;

end System.Garlic.Units;
