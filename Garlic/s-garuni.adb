with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Heart; use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
with System.Garlic.Termination;
pragma Elaborate_All (System.Garlic.Termination);
with System.Garlic.Utils; use System.Garlic.Utils;
pragma Elaborate_All (System.Garlic.Utils);
with System.RPC; use System.RPC;
pragma Elaborate_All (System.RPC);

with GNAT.IO; use GNAT.IO;

package body System.Garlic.Units is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("UNITS", "(s-garuni): ");

   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   ----------------
   -- Cache_Type --
   ----------------

   protected body Cache_Type is

      ------------------
      -- Get_RCI_Data --
      ------------------

      procedure Get_RCI_Data
        (Receiver  : out RPC_Receiver;
         Partition : out Partition_ID;
         Done      : out Boolean) is
      begin
         if not Cache_Consistent then
            Done      := False;
            Receiver  := null;
            Partition := Partition_ID'First;
         else
            Done      := True;
            Receiver  := Package_Receiver;
            Partition := Active_Partition;
         end if;
      end Get_RCI_Data;

      ------------------
      -- Set_RCI_Data --
      ------------------

      procedure Set_RCI_Data
        (Receiver  : in RPC_Receiver;
         Partition : in Partition_ID) is
      begin
         Cache_Consistent := True;
         Package_Receiver := Receiver;
         Active_Partition := Partition;
      end Set_RCI_Data;

   end Cache_Type;

end System.Garlic.Units;
