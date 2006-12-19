
---
-- This package is pure so it cannot depend upon Report
---
package body CXE4006_Common is

  procedure Single_Controlling_Operand (
      RTT         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
    Expected : Integer;
  begin
    case Test_Number is
      when 1001 => Expected := 100;
      when 2001 => Expected := 200;
      when others => raise Failed_Check;
    end case;

    if RTT.Common_Record_Field /= Expected then
      raise Failed_Check;
    end if;

    RTT.Common_Record_Field := Expected + 5;
    Callee := Common_Spec;
  end Single_Controlling_Operand;

end CXE4006_Common;
