with Polyorb.Types; use Polyorb.Types;
with Ada.Unchecked_Deallocation;

package Statistics is

   Nbr_Ietr : Polyorb.Types.Unsigned_Long;

   --  Initialize the statistics module
   procedure Initialise_Stats
     (Table_Size : Natural);

   --  Insert a new operation id and a duration
   procedure Insert_Duration (Op_Id : Natural; Delta1 : Duration);

   --  Print the table on the standard output
   procedure Print_Table;

   --  Finalization of the statistics module
   procedure Finalize;

private

   Tab_Size : Natural;

   type Op_Avg_Duration_Type is record
      Times       : Natural  := 0; --  number of times the operation is called
      Op_Duration : Duration := 0.0;
   end record;

   type Ops_Array is array (Natural range <>) of Op_Avg_Duration_Type;
   type Ops_Array_Ptr is access Ops_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Ops_Array, Ops_Array_Ptr);

   Ops_Table : Ops_Array_Ptr;

end Statistics;
