

package body CXE4006_Normal is
  procedure Single_Controlling_Operand (
      RTT         : in out Normal_Spec_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
  begin
    Callee := Normal_Spec;
  end Single_Controlling_Operand;

end CXE4006_Normal;
