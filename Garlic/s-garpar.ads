with System.Garlic.Protocols;
with System.Garlic.Table;
with System.Garlic.Types;
with System.Garlic.Utils;
with System.Garlic.Physical_Location;

private package System.Garlic.Partitions is

   type Partition_Info is record
      Allocated    : Boolean;
      Location     : Physical_Location.Location_Type;
      Protocol     : Protocols.Protocol_Access;
      Logical_Name : Utils.String_Access;
      Termination  : Types.Termination_Type;
      Reconnection : Types.Reconnection_Type;
      Status       : Types.Status_Type;
   end record;
   --    Allocated    : true when this slot is not empty
   --    Location     : partition physical location
   --    Protocol     : cache for location protocol
   --    Logical_Name : name of the partition (may be duplicated)
   --    Termination  : termination policy to adopt for this partition
   --    Reconnection : reconnection policy to adopt for this partition
   --    Status       : partition info status

   Null_Partition : constant Partition_Info :=
     (Allocated    => False,
      Location     => Physical_Location.Null_Location,
      Protocol     => null,
      Logical_Name => null,
      Termination  => Types.Global_Termination,
      Reconnection => Types.Rejected_On_Restart,
      Status       => Types.None);

   type Request_Kind is (Get_Partition_Info, Set_Partition_Info);

   type Request_Type (Kind : Request_Kind := Get_Partition_Info) is
      record
         case Kind is
            when Get_Partition_Info =>
               Reply_To_PID : Types.Partition_ID;
            when Set_Partition_Info =>
               Logical_Name : Utils.String_Access;
               Location     : Physical_Location.Location_Type;
               Termination  : Types.Termination_Type;
               Reconnection : Types.Reconnection_Type;
         end case;
      end record;

   package Partitions is new System.Garlic.Table.Complex
     (Index_Type     => Types.Partition_ID,
      Null_Index     => Types.Null_PID,
      First_Index    => Types.Boot_PID,
      Initial_Size   => Natural (Types.Last_PID),
      Increment_Size => 0,
      Component_Type => Partition_Info,
      Null_Component => Null_Partition);

end System.Garlic.Partitions;
