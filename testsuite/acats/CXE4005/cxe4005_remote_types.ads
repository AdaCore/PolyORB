
-----------------------------------------------------------------------------

with CXE4005_Common; use CXE4005_Common;
package CXE4005_Remote_Types is
  pragma Remote_Types;

  type RT_Tagged_Type is new Root_Tagged_Type with null record;

  procedure Single_Controlling_Operand (
      RTT         : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer);
  procedure Dual_Controlling_Operands (
      RTT1        : access RT_Tagged_Type;
      RTT2        : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer);
end CXE4005_Remote_Types;
