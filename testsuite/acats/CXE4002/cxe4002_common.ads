pragma Style_Checks (Off);

package CXE4002_Common is
  pragma Pure;

  -- types for parameters passed between partitions

  type Little_Number is range 0..7;

  type Integer_Vector is array (2..11) of Integer;

  subtype Description is String (1..10);
  type Record_Data is 
    record
      Part_No : Integer;
      Cost    : Float;
      Name    : Description;
    end record;
end CXE4002_Common;
