
package CXE4005_Common is
  pragma Pure;

  -- controls progress output from the tests.  
  Verbose : constant Boolean := False;

  -- exception to signify that the serial number of an object
  -- was not a one of the expected values for that type
  Wrong_Object : exception;

  -- identification of where a type is declared and where
  -- an access type was evaluated that refers to an object
  -- of that type.
  type Type_Selection is (Common_Spec,        --  xx1
                          RT_Spec,            --  xx6
                          B_Body,             --  xx7
                          Normal_Spec);       --  xx8
  type Access_Evaluation is (A1,              --  1xx
                             A2,              --  2xx
                             B);              --  3xx

  -- root tagged type for remote access to class wide type test
  type Root_Tagged_Type is tagged limited private;

  procedure Single_Controlling_Operand (
      RTT         : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer);
  procedure Dual_Controlling_Operands (
      RTT1        : access Root_Tagged_Type;
      RTT2        : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer);

  procedure Set_Serial_Number (
      RTT         : access Root_Tagged_Type;
      Sn          : in     Integer);

  function Serial_Number (RTT : access Root_Tagged_Type) return Integer;

  type Open_Tagged_Type is tagged 
    record
       Field : Integer;
    end record;
  procedure Open_Op (OTT : Open_Tagged_Type);

private
  type Root_Tagged_Type is tagged limited
    record
      Serial_Number        : Integer := 123;
    end record;
end CXE4005_Common;
