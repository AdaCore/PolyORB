with Ada.Text_IO;

with Polyorb.Types; use Polyorb.Types;

package body Statistics is

   Actual_Nbr_Iter : Polyorb.Types.Unsigned_Long;
   ----------------------
   -- Initialise_Stats --
   ----------------------

   procedure Initialise_Stats
     (Table_Size : Natural)
   is
      pragma Assert (Table_Size > 0);
   begin
      Tab_Size := Table_Size;
      Ops_Table := new Ops_Array (0 .. Tab_Size - 1);
      Actual_Nbr_Iter := 0;
   end Initialise_Stats;

   ---------------------
   -- Insert_Duration --
   ---------------------

   procedure Insert_Duration (Op_Id : Natural; Delta1 : Duration) is
      pragma Assert (Op_Id < Tab_Size);
      Op_Avg_Duration : Op_Avg_Duration_Type := Ops_Table (Op_Id);
   begin
      Actual_Nbr_Iter := Actual_Nbr_Iter + 1;
      --  If the operation is already called, the new duration is the average
      --  of all the durations
      Op_Avg_Duration.Op_Duration :=
        (Op_Avg_Duration.Op_Duration * Op_Avg_Duration.Times + Delta1) /
        (Op_Avg_Duration.Times + 1);
      Op_Avg_Duration.Times := Op_Avg_Duration.Times + 1;
      Ops_Table (Op_Id) := Op_Avg_Duration;
      if Actual_Nbr_Iter = Nbr_Ietr then
         Print_Table;
         Finalize;
         Initialise_Stats (Tab_Size);
      end if;
   end Insert_Duration;

   -----------------
   -- Print_Table --
   -----------------

   procedure Print_Table is
      Op_Avg_Duration : Op_Avg_Duration_Type;
   begin
      for I in Ops_Table'Range loop
         Op_Avg_Duration := Ops_Table (I);
         Ada.Text_IO.Put_Line
           (Natural'Image (I) &
            "          "      &
            Duration'Image (Op_Avg_Duration.Op_Duration));
      end loop;
   end Print_Table;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Free (Ops_Table);
      Ops_Table := null;
   end Finalize;

end Statistics;
