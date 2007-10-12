
-----------------------------------------------------------------------------

with CXE4006_Common;  use CXE4006_Common;
package CXE4006_Part_A1 is
  pragma Remote_Call_Interface;

  -- coordination of test termination across partitions
  procedure Can_Quit;
  procedure Quit;

  -- tagged types that can be passed between partitions
  type A1_Tagged_Type_1 is new Root_Tagged_Type with
    record
      A1_1_Component : Character := ' ';
    end record;

  procedure Single_Controlling_Operand (
      RTT         : in out A1_Tagged_Type_1;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

  type A1_Tagged_Type_2 is new A1_Tagged_Type_1 with
    record
       A1_2_Component : Float;
    end record;

  procedure Single_Controlling_Operand (
      RTT         : in out A1_Tagged_Type_2;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

  ----------

  procedure Make_Dispatching_Call_With (
      X           : in out Root_Tagged_Type'Class;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);
end CXE4006_Part_A1;
