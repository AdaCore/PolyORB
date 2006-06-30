with System.Partition_Interface;
with System.RPC;

package System.DSA_Services is
   pragma Elaborate_Body;

   function Get_Active_Partition_ID
     (Name : Partition_Interface.Unit_Name) return RPC.Partition_ID
     renames System.Partition_Interface.Get_Active_Partition_ID;
   --  Get the Partition_ID of the partition where remote call interface
   --  resides.

end System.DSA_Services;
