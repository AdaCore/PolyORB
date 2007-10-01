
-----------------------------------------------------------------------------

with CXE4006_Common;  use CXE4006_Common;
package CXE4006_Normal is
  type Normal_Spec_Tagged_Type is new CXE4006_Common.Root_Tagged_Type
       with null record;

  procedure Single_Controlling_Operand (
      RTT         : in out Normal_Spec_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);
end CXE4006_Normal;
