pragma Style_Checks (Off);
-----------------------------------------------------------------------------

package CXE4002_Part_A2 is
  -- This package supports the remote access tests
  pragma Remote_Call_Interface;

  procedure Call_With_2 (T : Integer);
  procedure Call_With_3 (T : Integer);

  procedure Mixed_1 (X : in Integer;  Y : out Integer; Z : in out Integer);
  procedure Mixed_2 (X : in Integer;  Y : out Integer; Z : in out Integer);

  type Remote_Proc is access procedure (X : Integer);
  type Remote_Proc_Mixed is access 
        procedure (A : in Integer;  B : out Integer;  C : in out Integer);

end CXE4002_Part_A2;
