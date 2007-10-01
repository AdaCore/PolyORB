
-----------------------------------------------------------------------------

with CXE4005_Common;  use CXE4005_Common;
package CXE4005_Part_A1 is
  pragma Remote_Call_Interface;
 
  type RACWT is access all CXE4005_Common.Root_Tagged_Type'Class;

  -- provide remote access values to other partitions
  function Get_RACWT (Which_Type : Type_Selection) 
       return CXE4005_Part_A1.RACWT;

  -- for checking E.4(18);6.0.
  procedure Takes_Class_Wide (X : CXE4005_Common.Open_Tagged_Type'Class);
  function Return_Open_Tagged_Type_Class
    return CXE4005_Common.Open_Tagged_Type'Class;

  -- coordination of test termination across partitions
  procedure Can_Quit;
  procedure Quit;

end CXE4005_Part_A1;
