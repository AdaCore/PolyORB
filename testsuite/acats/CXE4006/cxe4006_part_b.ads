
-----------------------------------------------------------------------------

with CXE4006_Common;  use CXE4006_Common;
with CXE4006_Part_A1;
package CXE4006_Part_B is
  pragma Remote_Call_Interface;

  -- tagged types that can be passed between partitions
  type B_Tagged_Type is new Root_Tagged_Type
       with null record;

  procedure Single_Controlling_Operand (
      RTT         : in out B_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);


  procedure Wrapped_Around (
      X           : in out Root_Tagged_Type'Class;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);
end CXE4006_Part_B;
