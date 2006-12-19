
-----------------------------------------------------------------------------

with CXE4005_Common;
package CXE4005_Normal is
  type Cant_Use_In_Remote_Call is new CXE4005_Common.Root_Tagged_Type
       with null record;

  procedure Single_Controlling_Operand (
      RTT         : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer);
  procedure Dual_Controlling_Operands (
      RTT1        : access Cant_Use_In_Remote_Call;
      RTT2        : access Cant_Use_In_Remote_Call;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer);

  type Open_But_Not_For_Export is new CXE4005_Common.Open_Tagged_Type
      with null record;
end CXE4005_Normal;
