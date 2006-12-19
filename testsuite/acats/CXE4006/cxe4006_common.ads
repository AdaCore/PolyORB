
package CXE4006_Common is
  pragma Pure;

  -- controls progress output from the tests.  The value of this
  -- flag does not affect whether or not the test passes.
  Verbose : constant Boolean := False;

  -- exception to signify that the test number or object
  -- was not a one of the expected values
  Failed_Check : exception;

  -- instances of types derived from Root_Tagged_Type.
  -- Used to identify the routine that received the dispatching call.

  type Type_Decl_Location is (
        Common_Spec,
        Part_A1_1_Spec,
        Part_A1_2_Spec,
        Part_A2_Spec,
        Part_B_Spec,
        Part_B_Body,
        Normal_Spec);

  -- root tagged type for remote access to class wide type test
  type Root_Tagged_Type is tagged
    record
      Common_Record_Field : Integer := 1234;
    end record;

  procedure Single_Controlling_Operand (
      RTT         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

end CXE4006_Common;
