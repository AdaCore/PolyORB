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
with System.Garlic.Options;     use System.Garlic.Options;
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

   procedure Add_To_Partition_RCI_List
     (Unit : in Unit_Id;
      Info : in out Unit_Info);
   --  Link all the RCI units of a partition together. There is a
   --  single-linked list which begins with a fake unit of name
   --  RCI_List_Name (Unit.Partition). But don't duplicate it in the list
   --  if it has already been declared.

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info);

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type);
   --  Global message receiver

   procedure Invalidate_Info
     (Partition : in Partition_ID);

   function Partition_RCI_List
     (Partition : in Types.Partition_ID)
      return Unit_Id;
   --  Build an unique unit name from a partition id. This name
   --  corresponds to a fake unit. This unit is the root of a rci unit
   --  list. Each unit of this list is configured on this partition.

   -------------------------------
   -- Add_To_Partition_RCI_List --
   -------------------------------

   procedure Add_To_Partition_RCI_List
     (Unit : in Unit_Id;
      Info : in out Unit_Info)
   is
      RCI_List_Id   : Unit_Id;
      RCI_List_Unit : Unit_Info;
   begin
      RCI_List_Id   := Partition_RCI_List (Info.Partition);
      RCI_List_Unit := Units.Get_Component (RCI_List_Id);

      declare
         U : Unit_Info;
         I : Unit_Id   := RCI_List_Unit.Next_Unit;
      begin
         while I /= Null_Unit_Id loop
            if I = Unit then
               return;
            end if;
            U := Units.Get_Component (I);
            I := U.Next_Unit;
         end loop;
      end;

      pragma Debug
        (D (D_Debug,
            "Add " & Units.Get_Name (Unit) &
            " to partition" & Info.Partition'Img &
            " RCI units"));

      Info.Next_Unit := RCI_List_Unit.Next_Unit;
      RCI_List_Unit.Next_Unit := Unit;
      Units.Set_Component (RCI_List_Id, RCI_List_Unit);
   end Add_To_Partition_RCI_List;

   --------------------
   -- Dump_Unit_Info --
   --------------------

   procedure Dump_Unit_Info
     (Unit : in Unit_Id;
      Info : in Unit_Info) is
   begin
      D (D_Dump, "Information on unit " & Units.Get_Name (Unit));
      D (D_Dump, "Partition    "  & Info.Partition'Img);
      D (D_Dump, "Receiver     "  & Info.Receiver'Img);
      if Info.Version /= null then
         D (D_Dump, "Version       " & Info.Version.all);
      else
         D (D_Dump, "Version       <no version>");
      end if;
      D (D_Dump, "Receiver      " & Info.Status'Img);
   end Dump_Unit_Info;

   -------------------
   -- Get_Unit_Info --
   -------------------

   function Get_Unit_Info
     (Unit : Unit_Id)
      return Unit_Info
   is
      Version : Version_Id;
      Info    : Unit_Info;
   begin
      loop
         Info := Units.Get_Component (Unit);

         exit when Info.Status = Known;

         pragma Debug
           (D (D_Debug,
               "Looking for information on unit "&  Units.Get_Name (Unit)));

         Units.Enter;
         Info := Units.Get_Component (Unit);

         if not Boot_Partition
           and then Info.Status = Unknown
         then
            declare
               Query : aliased Params_Stream_Type (0);
            begin
               Request_Type'Output (Query'Access, (Kind => Get_Unit_Info));
               String'Output (Query'Access, Units.Get_Name (Unit));
               Send (Boot_PID, Unit_Name_Service, Query'Access);
            end;
            Info.Status := Queried;
            Units.Set_Component (Unit, Info);
         end if;

         Units.Leave (Version);
         exit when Info.Status = Known;
         Units.Differ (Version);
      end loop;

      return Info;
   end Get_Unit_Info;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Params_Stream_Type;
      Reply     : access Params_Stream_Type)
   is
      Request : Request_Type;
      Unit    : Unit_Id;
      Info    : Unit_Info;
   begin
      Request := Request_Type'Input (Query);
      if Request.Kind /= Invalidate_Info then
         Unit := Units.Get_Index (String'Input (Query));
      end if;

      if Request.Kind /= Invalidate_Info then
         pragma Debug
           (D (D_Debug, "Receive request " & Request.Kind'Img &
               " on unit " & Units.Get_Name (Unit) &
               " from partition" & Partition'Img));
         null;
      else
         pragma Debug
           (D (D_Debug, "Receive request " & Request.Kind'Img &
               " on partition" & Request.Wrong_PID'Img &
               " from partition" & Partition'Img));
         null;
      end if;

      Units.Enter;
      Info := Units.Get_Component (Unit);

      case Request.Kind is
         when Get_Unit_Info =>

            case Info.Status is
               when Known =>
                  pragma Debug
                    (D (D_Debug, Units.Get_Name (Unit) & " is known"));

                  Request_Type'Output
                    (Reply,
                     (Set_Unit_Info,
                      Info.Partition,
                      Info.Receiver,
                      Info.Version));
                  String'Output
                    (Reply,
                     Units.Get_Name (Unit));

               when Unknown =>
                  pragma Debug
                    (D (D_Debug,
                        "Queuing request from" & Partition'Img &
                        " on " & Units.Get_Name (Unit)));

                  --  Queue this request in order to answer it when info is
                  --  available. Note that this is a remote request which
                  --  should not be postponed.

                  Info.Pending := True;
                  Info.Requests (Partition) := True;
                  Units.Set_Component (Unit, Info);

               when Invalid =>
                  pragma Debug
                    (D (D_Debug, Units.Get_Name (Unit) & " is invalid"));

                  Request_Type'Output
                    (Reply,
                     (Set_Unit_Info,
                      Null_PID,
                      Info.Receiver,
                      Info.Version));
                  String'Output
                    (Reply,
                     Units.Get_Name (Unit));

               when Queried =>
                  pragma Assert (False);
                  null;

            end case;

         when Set_Unit_Info =>

            if Request.Partition /= Null_PID then
               if Info.Status /= Known then
                  --  Link this unit to the list of units configured on
                  --  this partition.

                  Info.Status    := Known;
                  Info.Receiver  := Request.Receiver;
                  Info.Version   := Request.Version;
                  Info.Partition := Request.Partition;

                  Add_To_Partition_RCI_List (Unit, Info);
               end if;

            else
               Info.Status    := Invalid;
            end if;

            --  Dequeue pending requests

            if Info.Pending then
               for PID in Info.Requests'Range loop
                  if Info.Requests (PID) then
                     declare
                        Reply : aliased Params_Stream_Type (0);
                     begin
                        Request_Type'Output
                          (Reply'Access,
                           (Set_Unit_Info,
                            Info.Partition,
                            Info.Receiver,
                            Info.Version));
                        String'Output
                          (Reply'Access,
                           Units.Get_Name (Unit));

                        pragma Debug
                          (D (D_Debug,
                              "Reply to partition" & PID'Img &
                              " pending request on unit " &
                              Units.Get_Name (Unit)));

                        Send (PID, Unit_Name_Service, Reply'Access);
                     end;
                     Info.Requests (PID) := False;
                  end if;
               end loop;
               Info.Pending := False;
            end if;

            Dump_Unit_Info (Unit, Info);
            Units.Set_Component (Unit, Info);

         when Invalidate_Info =>
            Invalidate_Info (Request.Wrong_PID);

      end case;

      Units.Leave;
   end Handle_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Handler (Unit_Name_Service, Handle_Request'Access);
   end Initialize;

   ---------------------
   -- Invalidate_Info --
   ---------------------

   procedure Invalidate_Info
     (Partition : in Partition_ID)
   is
      Status : Unit_Status;
      Unit   : Unit_Id;
      Info   : Unit_Info;
   begin
      if Shutdown_In_Progress then
         return;
      end if;

      if Reconnection_Policy (Partition) = Blocked_Until_Restart then
         Status := Unknown;
      else
         Status := Invalid;
      end if;

      Unit := Partition_RCI_List (Partition);
      Unit := Units.Get_Component (Unit).Next_Unit;

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
   end Invalidate_Info;

   --------------------------
   -- Invalidate_Partition --
   --------------------------

   procedure Invalidate_Partition
     (Partition : in Partition_ID)
   is
      Query : aliased Params_Stream_Type (0);
   begin
      Units.Enter;
      Invalidate_Info (Partition);
      Units.Leave;

      if not Boot_Partition then
         Request_Type'Output
           (Query'Access,
            (Invalidate_Info,
             Partition));
         Send (Boot_PID, Unit_Name_Service, Query'Access);
      end if;
   end Invalidate_Partition;

   ------------------------
   -- Partition_RCI_List --
   ------------------------

   function Partition_RCI_List
     (Partition : in Types.Partition_ID)
      return Unit_Id
   is
      Name : constant String := "partition " & Partition'Img;
   begin
      return Units.Get_Index (Name);
   end Partition_RCI_List;

   -------------------
   -- Set_Unit_Info --
   -------------------

   procedure Set_Unit_Info
     (Unit     : in Unit_Id;
      Receiver : in Interfaces.Unsigned_64;
      Version  : in Utils.String_Access)
   is
      Query : aliased Params_Stream_Type (0);
   begin
      Request_Type'Output
        (Query'Access,
         (Set_Unit_Info,
          Self_PID,
          Receiver,
          Version));
      String'Output
        (Query'Access,
         Units.Get_Name (Unit));
      Send (Boot_PID, Unit_Name_Service, Query'Access);
   end Set_Unit_Info;

end System.Garlic.Units;
