
-----------------------------------------------------------------------------

with CXE4005_Common;  use CXE4005_Common;
with CXE4005_Part_A1;
package CXE4005_Part_B is
  pragma Remote_Call_Interface;

  -- provide remote access values to other partitions
  function Get_RACWT (Which_Type : Type_Selection)
      return CXE4005_Part_A1.RACWT;
end CXE4005_Part_B;
