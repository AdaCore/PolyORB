with System.Garlic.Name_Table;
with System.Garlic.Table;
with System.Garlic.Heart;
with System.Garlic.Utils;
with System.RPC;

package System.Garlic.Units is

   type Unit_Id is new Natural;
   Null_Unit_Id : constant Unit_Id := 0;

   type Request_List is array (RPC.Partition_ID) of Boolean;
   type Request_Id is (Get_Unit, Set_Unit);

   protected type Cache_Type is

      procedure Get_RCI_Data
        (Receiver  : out RPC.RPC_Receiver;
         Partition : out RPC.Partition_ID;
         Done      : out Boolean);

      procedure Set_RCI_Data
        (Receiver  : in RPC.RPC_Receiver;
         Partition : in RPC.Partition_ID);

   private

      Cache_Consistent : Boolean := False;
      Active_Partition : RPC.Partition_ID;
      Package_Receiver : RPC.RPC_Receiver;

   end Cache_Type;

   type Cache_Access is access Cache_Type;

   type Unit_Status is (Unknown, Queried, Known);

   type Unit_Type is
      record
         Partition : RPC.Partition_ID;
         Receiver  : RPC.RPC_Receiver;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
         Status    : Unit_Status;
         Pending   : Boolean;
         Requests  : Request_List;
      end record;

   Null_Unit : constant Unit_Type
     := (Partition => System.Garlic.Heart.Null_Partition_ID,
         Receiver  => null,
         Version   => null,
         Cache     => null,
         Status    => Unknown,
         Pending   => False,
         Requests  => (others => False));

   type Request_Type is
      record
         Command   : Request_Id;
         Partition : RPC.Partition_ID;
         Receiver  : RPC.RPC_Receiver;
         Version   : Utils.String_Access;
         Cache     : Cache_Access;
      end record;

   Null_Request : constant Request_Type
     := (Command   => Get_Unit,
         Partition => System.Garlic.Heart.Null_Partition_ID,
         Receiver  => null,
         Version   => null,
         Cache     => null);

   package Units is new System.Garlic.Table.Concurrent
     (Index_Type     => Unit_Id,
      Initial_Size   => 20,
      Increment_Size => 20,
      Component_Type => Unit_Type,
      Null_Component => Null_Unit,
      Parameter_Type => Request_Type);

end System.Garlic.Units;
