
-----------------------------------------------------------------------------

package CXE2001_Part_B is
  pragma Remote_Call_Interface;
  
  procedure Test_Finished;
  procedure Set_Shared_Data (Value : Integer);
  procedure Increment_Counter;
end CXE2001_Part_B;
