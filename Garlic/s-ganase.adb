------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--            S Y S T E M . G A R L I C . N A M E _ S E R V E R             --
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

with System.Garlic.Debug;   use System.Garlic.Debug;
with System.Garlic.Heart;   use System.Garlic.Heart;
with System.Garlic.Streams; use System.Garlic.Streams;

package body System.Garlic.Name_Server is

   use System.Garlic.Name_Table;
   use System.Garlic.Physical_Location, System.Garlic.Types;
   use System.Garlic.Protocols;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_GANASE", "(s-ganase): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Protocols_Cache : array (System.Garlic.Types.Partition_ID) of
     System.Garlic.Protocols.Protocol_Access;
   --  Copy of the protocol type of the Partition_Map_Cache

   Partition_Map_Cache : Partition_Data_Array;
   --  This acts as a Cache for Partition_Map, this means that if Known
   --  is True for a given partition, there is no need to use the overhead
   --  of the protected type to query a partition location.

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
      Partition_Map.Wait_For_Data (Server_Partition_ID, Data);
      return To_String (Data.Public.Location);
   end Get_Boot_Server;

   ------------------------
   -- Get_Partition_Data --
   ------------------------

   function Get_Partition_Data (Partition : Partition_ID)
     return Partition_Data is
      Data : Partition_Data;

   begin
      pragma Debug
        (D (D_Table,
            "Looking locally for information on partition" & Partition'Img));

      --  If the partition location is in the cache, then get it from
      --  there instead of using the protected type.

      if Partition_Map_Cache (Partition) .Known then
         return Partition_Map_Cache (Partition);
      end if;

      Partition_Map.Wait_For_Data (Partition, Data);
      if Data.Queried then

         --  We have to query the server for the public data of the partition

         declare
            Params : aliased Params_Stream_Type (0);
         begin
            pragma Debug
              (D (D_Garlic,
                  "Asking for information on partition" & Partition'Img));
            Partition_ID'Write (Params'Access, Partition);
            Send (Server_Partition_ID, Query_Public_Data, Params'Access);
         end;

         Partition_Map.Wait_For_Data (Partition, Data);

         pragma Debug
           (D (D_Table,
               "Caching information on partition" & Partition'Img));
      end if;

      return Data;
   end Get_Partition_Data;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Partition : Partition_ID)
      return Protocols.Protocol_Access is
   begin
      --  If the partition is the boot server, then the protocol is
      --  already known even when Partition_Data is only partially
      --  initialized.

      if Partition /= Server_Partition_ID
        and then not Partition_Map_Cache (Partition) .Known then
         declare
            pragma Warnings (Off);
            Dummy : constant Partition_Data := Get_Partition_Data (Partition);
            pragma Warnings (On);
         begin
            null;
         end;
      end if;

      return Protocols_Cache (Partition);
   end Get_Protocol;

   ---------------------
   -- Get_Public_Data --
   ---------------------

   function Get_Public_Data (Partition : Partition_ID) return Public_Data is
   begin
      return Get_Partition_Data (Partition) .Public;
   end Get_Public_Data;

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
           Physical_Location.Get_Protocol (Data.Public.Location);
         if Queue'Count > 0 then
            New_Data := True;
         end if;
      end Set_Data;

      -------------------
      -- Wait_For_Data --
      -------------------

      entry Wait_For_Data
         (Partition : in Partition_ID;
          Data      : out Partition_Data)
         when not New_Data is
      begin
         if Map (Partition).Known then
            Data := Map (Partition);
         elsif Map (Partition).Queried then
            requeue Queue with abort;
         else
            Map (Partition).Queried := True;
            Data := Map (Partition);
         end if;
      end Wait_For_Data;

      --  Local entries implementing wait queues below.

      entry Queue
         (Partition : in Partition_ID;
          Data      : out Partition_Data)
         when New_Data is
      begin
         if Queue'Count = 0 then
            New_Data := False;
         end if;
         requeue Wait_For_Data with abort;
      end Queue;

   end Partition_Map_Type;

end System.Garlic.Name_Server;
