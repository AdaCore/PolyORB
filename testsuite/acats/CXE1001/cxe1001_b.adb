
------------------------------------------------------------------------

with Report;
with System;
with CXE1001_P;      -- a procedure who's ID is to be checked
with CXE1001_Q;      -- a procedure who's ID is to be checked

procedure CXE1001_B is

   type Hold_Partition_ID is range System.Min_Int..System.Max_Int;
   P_ID : Hold_Partition_ID; 
   Q_ID : Hold_Partition_ID; 

begin

   Report.Test ("CXE1001_B", "Check Partition IDs. " &
                                 "-- This is the SECOND PARTITION");

   CXE1001_P;
   CXE1001_Q;
   P_ID := CXE1001_P'Partition_ID;
   Q_ID := CXE1001_Q'Partition_ID;
   if P_ID /= Q_ID then 
      Report.Failed ("Partition IDs of the procedures in this " &
                     "partition are not the same");
   end if;
   Report.Special_Action ("Partition ID of SECOND Partition is: " &
                           Hold_Partition_ID'image(P_ID) & 
                           ".  Check that this is different from that " &
                           "of the FIRST partition");

   Report.Result;

end CXE1001_B;
