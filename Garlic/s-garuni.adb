------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                  S Y S T E M . G A R L I C . U N I T S                   --
--                                                                          --
--                                 B o d y                                  --
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

with System.Garlic.Debug;       use System.Garlic.Debug;
with System.Garlic.Heart;       use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Options; use System.Garlic.Options;
with System.Garlic.Startup;
pragma Warnings (Off, System.Garlic.Startup);
with System.Garlic.Soft_Links;  use System.Garlic.Soft_Links;
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

   use System.Garlic.Units.X;

   Local_Partition  : constant Partition_ID := Get_My_Partition_ID;
   Get_Unit_Request : constant Request_Type :=
     (Get_Unit, Local_Partition, 0, null, null);

   procedure Add_To_Partition_RCI_List
     (N : in Unit_Id;
      U : in out Unit_Type;
      P : in Partition_ID);
   --  Link all the RCI units of a partition together. There is a
   --  single-linked list which begins with a fake unit of name
   --  RCI_List_Name (Partition). But don't duplicate it in the list
   --  if it has already been declared.

   procedure Add_To_Partition_RCI_List
     (N : in Unit_Id;
      U : in out Unit_Type;
      P : in Partition_ID) is
      RCI_List_Id   : Unit_Id;
      RCI_List_Unit : Unit_Type;

   begin
      if U.Partition = P then
         return;
      end if;

      pragma Debug
        (D (D_Debug,
            "Add " & Get_Name (N) & " in RCI list of partition" & P'Img));

      RCI_List_Id   := Partition_RCI_List (P);
      RCI_List_Unit := Get_Component (RCI_List_Id);

      U.Next_Unit := RCI_List_Unit.Next_Unit;
      RCI_List_Unit.Next_Unit := N;
      Set_Component (RCI_List_Id, RCI_List_Unit);
   end Add_To_Partition_RCI_List;

   ------------------
   -- Get_RCI_Data --
   ------------------

   procedure Get_RCI_Data
     (Cache     : in Cache_Type;
      Receiver  : out Interfaces.Unsigned_64;
      Partition : out Types.Partition_ID;
      Status    : out Unit_Status)
   is
   begin
      Enter_Critical_Section;
      Status    := Cache.RCI_Unit_Status;
      Receiver  := Cache.Package_Receiver;
      Partition := Cache.Active_Partition;
      Leave_Critical_Section;
   end Get_RCI_Data;

   ------------
   -- Update --
   ------------

   procedure Update
     (Cache     : out Cache_Type;
      Partition : in  Types.Partition_ID;
      Status    : in  Unit_Status) is
   begin
      Cache.RCI_Unit_Status := Status;
   end Update;

   -------------
   -- Process --
   -------------

   procedure Process
     (N       : in Unit_Id;
      Request : in Request_Type;
      Unit    : in out Unit_Type;
      Status  : out Status_Type)
   is
      Reconnection : Reconnection_Type;

   begin
      case Request.Command is
         when Get_Unit =>

            --  If Request.Cache is different from null then the request
            --  comes from an RCI_Info package. This cache reference
            --  has to be saved for later use.

            if Request.Cache /= null then
               Unit.Cache := Request.Cache;
            end if;

            case Unit.Status is
               when Known =>
                  pragma Debug (D (D_Debug, Get_Name (N) & " is known"));

                  --  If the request is remote, then send a set request
                  --  to the remote partition. Note that the cache reference
                  --  is useless.

                  if Request.Partition /= Local_Partition then
                     declare
                        R : Request_Type
                          := (Set_Unit,
                              Unit.Partition,
                              Unit.Receiver,
                              Unit.Version,
                              null);
                     begin
                        Send (Request.Partition, R, N);
                     end;
                  end if;

                  --  Unit kept unmodified

                  Status := Unmodified;

               when Unknown | Queried =>
                  pragma Debug (D (D_Debug, Get_Name (N) & " is unknown"));

                  if not Boot_Partition then

                     --  Ask the boot server for this info

                     if Unit.Status = Unknown then
                        Send (Boot_PID, Request, N);
                     end if;
                     Unit.Status := Queried;

                     --  Unit modified and request unsatisfied

                     Status := Postponed;

                  elsif Request.Partition /= Boot_PID then
                     pragma Debug
                       (D (D_Debug,
                           "Queuing request from" & Request.Partition'Img &
                           " on " & Get_Name (N)));

                     --  Queue this request in order to answer it when info
                     --  is available. Note that this is a remote request
                     --  which should not be postponed.

                     Unit.Pending := True;
                     Unit.Requests (Request.Partition) := True;

                     --  Unit modified but request satisfied (pending)

                     Status := Modified;

                  else
                     --  Request unsatisfied

                     Status := Postponed;
                  end if;

               when Invalid =>
                  pragma Debug (D (D_Debug, Get_Name (N) & " is invalid"));

                  --  If the request is remote, then send a set request
                  --  to the remote partition. Note that the cache reference
                  --  is useless.

                  if Request.Partition /= Local_Partition then
                     declare
                        R : Request_Type
                          := (Set_Unit,
                              Null_PID,
                              Unit.Receiver,
                              Unit.Version,
                              null);
                     begin
                        --  Send invalid values especially Partition_ID
                        --  to show that the registration has failed.

                        Send (Request.Partition, R, N);
                     end;
                  end if;

                  --  Unit kept unmodified

                  Status := Unmodified;

            end case;

         when Set_Unit =>

            case Unit.Status is
               when Unknown | Invalid =>

                  --  If the unit was already registered on a dead partition
                  --  and if this partition should be rejected on restart
                  --  then keep name server as is.

                  if Unit.Status = Invalid
                    and then
                    Boot_Partition
                    and then
                    Reconnection_Policy (Unit.Partition) = Rejected_On_Restart
                  then
                     Status := Unmodified;
                     return;
                  end if;

                  if Boot_Partition then

                     --  Link this unit to the list of units configured
                     --  on this partition.

                     Add_To_Partition_RCI_List (N, Unit, Request.Partition);

                     --  All set requests are handled on boot partition

                     Unit.Status    := Known;
                     Unit.Receiver  := Request.Receiver;
                     Unit.Version   := Request.Version;
                     Unit.Partition := Request.Partition;

                     --  Answer pending requests

                     if Unit.Pending then
                        pragma Debug
                          (D (D_Debug,
                              "Answer pending requests on " &
                              Get_Name (N)));

                        --  Dequeue pending requests

                        for P in Unit.Requests'Range loop
                           if Unit.Requests (P) then
                              Send (P, Request, N);
                              Unit.Requests (P) := False;
                           end if;
                        end loop;
                        Unit.Pending := False;
                     end if;

                     if Unit.Cache /= null then
                        Set_RCI_Data
                          (Unit.Cache.all,
                           Unit.Receiver,
                           Unit.Partition);
                     end if;

                     --  Unit modified

                     Status := Modified;

                  else
                     pragma Debug
                       (D (D_Debug, Get_Name (N) &
                           " registered on boot server"));

                     --  Forward set request to boot partition

                     Send (Boot_PID, Request, N);

                     --  Unit kept unmodified

                     Status := Unmodified;

                  end if;

               when Queried =>

                  --  Unit modified

                  Status := Modified;

                  --  If Request.Partition is null, then it means
                  --  this partition has been invalidated and it is
                  --  still invalid.

                  if Request.Partition = Null_PID then
                     Unit.Status := Known;
                     return;
                  end if;

                  Add_To_Partition_RCI_List (N, Unit, Request.Partition);

                  --  Store info. Note this info can be obtained after
                  --  a "set" request. In this case, we obtain the
                  --  first partition which has declared this unit.
                  --  Register_Receiving_Stub will raise an exception
                  --  if the partition on which is declared this unit is
                  --  not the current partition. This is possible when
                  --  this unit is declared twice.

                  Unit.Status    := Known;
                  Unit.Receiver  := Request.Receiver;
                  Unit.Version   := Request.Version;
                  Unit.Partition := Request.Partition;

                  if Unit.Cache /= null then
                     Set_RCI_Data
                       (Unit.Cache.all,
                        Unit.Receiver,
                        Unit.Partition);
                  end if;

               when Known =>

                  Status := Unmodified;

            end case;

         when Invalidate =>

            Status       := Modified;
            Reconnection := Reconnection_Policy (Request.Partition);

            --  Send the same request to boot_server

            if not Boot_Partition then
               Send (Boot_PID, Request, N);
            end if;

            --  Invalidate all RCI units of a given partition

            declare
               U : Unit_Type;
               I : Unit_Id;

            begin
               I := Unit.Next_Unit;
               while I /= Null_Unit_Id loop
                  U := Get_Component (I);

                  if Reconnection = Blocked_Until_Restart then
                     U.Status    := Unknown;
                  else
                     U.Status      := Invalid;
                  end if;

                  --  Update cache status
                  if U.Cache /= null then
                     Update (U.Cache.all, U.Partition, U.Status);
                  end if;

                  pragma Debug
                    (D (D_Debug,
                        "RCI unit " & Get_Name (I) &
                        " status is now " & U.Status'Img));

                  Set_Component (I, U);
                  I := U.Next_Unit;
               end loop;
            end;

      end case;
   end Process;

   ------------------------
   -- Partition_RCI_List --
   ------------------------

   function Partition_RCI_List
     (Partition : in Types.Partition_ID)
      return Unit_Id is
      Name : constant String := "partition " & Partition'Img;
   begin
      return Get_Index (Name);
   end Partition_RCI_List;

   ----------
   -- Send --
   ----------

   procedure Send
     (Partition : in Partition_ID;
      Request   : in Request_Type;
      Unit      : in Unit_Id)
   is
      Params : aliased Params_Stream_Type (0);
   begin
      Request_Id'Write (Params'Access, Request.Command);
      case Request.Command is
         when Set_Unit =>
            pragma Debug
              (D (D_RNS,
                  "Send " & Request.Command'Img &
                  " on "  & Get_Name (Unit) &
                  " to "  & Partition'Img));

            String'Output (Params'Access, Get_Name (Unit));
            Partition_ID'Write (Params'Access, Request.Partition);
            Interfaces.Unsigned_64'Write (Params'Access, Request.Receiver);
            String'Output (Params'Access, Request.Version.all);

         when Get_Unit =>
            pragma Debug
              (D (D_RNS,
                  "Send " & Request.Command'Img &
                  " on "  & Get_Name (Unit) &
                  " to "  & Partition'Img));

            String'Output (Params'Access, Get_Name (Unit));

         when Invalidate =>
            pragma Debug
              (D (D_RNS,
                  "Send " & Request.Command'Img &
                  " on "  & Request.Partition'Img &
                  " to "  & Partition'Img));

            Partition_ID'Write (Params'Access, Request.Partition);
      end case;

      Send (Partition, Unit_Name_Service, Params'Access);
   end Send;

   ------------------
   -- Set_RCI_Data --
   ------------------

   procedure Set_RCI_Data
     (Cache     : out Cache_Type;
      Receiver  : in Interfaces.Unsigned_64;
      Partition : in Types.Partition_ID)
   is
   begin
      Enter_Critical_Section;
      Cache.RCI_Unit_Status  := Known;
      Cache.Package_Receiver := Receiver;
      Cache.Active_Partition := Partition;
      Leave_Critical_Section;
   end Set_RCI_Data;

end System.Garlic.Units;
