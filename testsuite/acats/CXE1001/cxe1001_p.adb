pragma Style_Checks (Off);
------------------------------------------------------------------------

-- The following are the two library level declarations who's Partition_ID
-- attributes are to be checked

with Report; 
--
procedure CXE1001_P is
   -- Note: This module is not declared-pure
begin 
   Report.Comment ("Executing Procedure CXE1001_P");
end CXE1001_P;
