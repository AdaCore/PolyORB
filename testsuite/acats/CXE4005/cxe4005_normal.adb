

with Report;
package body CXE4005_Normal is
  procedure Single_Controlling_Operand (
      RTT         : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer) is
  begin
    Report.Failed ("Call made where type is declared in a normal " &
                   "package.  Test number " &
                   Integer'Image (Test_Number));
    Obj_SN := Serial_Number(RTT);
  end Single_Controlling_Operand;

  procedure Dual_Controlling_Operands (
      RTT1        : access Cant_Use_In_Remote_Call;
      RTT2        : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer)is
  begin
    Report.Failed ("Call made where type is declared in a normal " &
                   "package.  Test number " &
                   Integer'Image (Test_Number));
    Obj_SN1 := Serial_Number(RTT1);
    Obj_SN2 := Serial_Number(RTT2);
  end Dual_Controlling_Operands;
end CXE4005_Normal;
