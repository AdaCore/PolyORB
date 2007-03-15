
package body CXE4005_Remote_Types is
  --
  -- The serial number for all objects of RT_Tagged_Type will contain
  -- a 6 as the least significant digit.  Make sure the correct object
  -- is passed to these routines.
  --

  procedure Single_Controlling_Operand (
      RTT         : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer) is
  begin
    Obj_SN := Serial_Number(RTT);
    if Serial_Number(RTT) mod 10 /= 6 then
      raise Wrong_Object;
    end if;
  end Single_Controlling_Operand;

  procedure Dual_Controlling_Operands (
      RTT1        : access RT_Tagged_Type;
      RTT2        : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer) is
  begin
    Obj_SN1 := Serial_Number(RTT1);
    Obj_SN2 := Serial_Number(RTT2);
    if Serial_Number(RTT1) mod 10 /= 6 or 
       Serial_Number(RTT2) mod 10 /= 6 then
      raise Wrong_Object;
    end if;
  end Dual_Controlling_Operands;

end CXE4005_Remote_Types;
