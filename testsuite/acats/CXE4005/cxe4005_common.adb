
---
-- This package is pure so it cannot depend upon Report
---
package body CXE4005_Common is
  Op_Is_Zero : exception;   

  -- All objects that do not have an overriding definition of
  -- Single_Controlling_Operand and Dual_Controlling_Operands
  -- have a serial number with the least significant digit in
  -- the range from 1 to 5.
  -- If a wrong object is passed to these
  -- routines then the exception Wrong_Object is raised.

  procedure Single_Controlling_Operand (
      RTT         : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer) is
  begin
    Obj_SN := Serial_Number(RTT);
    if RTT.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
  end Single_Controlling_Operand;

  procedure Dual_Controlling_Operands (
      RTT1        : access Root_Tagged_Type;
      RTT2        : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer) is
  begin
    Obj_SN1 := RTT1.Serial_Number;
    Obj_SN2 := RTT2.Serial_Number;

    if RTT1.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
    if RTT2.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
  end Dual_Controlling_Operands;

  procedure Set_Serial_Number (
      RTT         : access Root_Tagged_Type;
      Sn          : in     Integer) is
  begin
    RTT.Serial_Number := Sn;
  end Set_Serial_Number;

  function Serial_Number (RTT : access Root_Tagged_Type) return Integer is
  begin
    return RTT.Serial_Number;
  end Serial_Number;

  procedure Open_Op (OTT : Open_Tagged_Type) is
  begin
    if OTT.Field = 0 then
      raise Op_Is_Zero;
    end if;
  end Open_Op;

end CXE4005_Common;
