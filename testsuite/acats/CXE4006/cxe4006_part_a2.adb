
-----------------------------------------------------------------------------

with Report;
with CXE4006_Part_B;
with CXE4006_Normal;
package body CXE4006_Part_A2 is

  procedure Single_Controlling_Operand (
      RTT         : in out A2_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
    Expected : Integer := 0;
    Expected_String : String_20;
  begin
    case Test_Number is
      when 1004   => Expected := 130;
                     Expected_String := "12345678901234567890";
      when 2004   => Expected := 230;
                     Expected_String := "24680135790987654321";
      when others => Report.Failed ("CXE4006_Part_A2 bad test number" &
                                    Integer'Image (Test_Number));
    end case;

    if RTT.Common_Record_Field /= Expected then
      Report.Failed ("CXE4006_Part_A2 expected" &
                     Integer'Image (Expected) &
                     " but received" &
                     Integer'Image (RTT.Common_Record_Field) &
                     " in test" &
                     Integer'Image (Test_Number));
    end if;
    if RTT.A2_Component /= Expected_String then
      Report.Failed ("CXE4006_Part_A2 expected '" &
                     Expected_String &
                     "' but received '" &
                     RTT.A2_Component &
                     "' in test" &
                     Integer'Image (Test_Number));
    end if;

    RTT.Common_Record_Field := Expected + 8;
    Callee := Part_A2_Spec;
  end Single_Controlling_Operand;


  -- pass thru procedure
  procedure Call_B (
      X           : in out Root_Tagged_Type'Class;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
  begin
    CXE4006_Part_B.Wrapped_Around (X, Test_Number, Callee);
  end Call_B;

end CXE4006_Part_A2;
