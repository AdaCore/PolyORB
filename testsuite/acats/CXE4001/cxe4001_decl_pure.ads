pragma Style_Checks (Off);

package CXE4001_Decl_Pure is
  pragma Pure;
  Visible_User_Defined_Exception : exception;

  -- The setting of the following flag affects how much output is 
  -- produced when the test runs.  This output can be used to help
  -- debug problems with the test.
  Verbose : constant Boolean := False;
end CXE4001_Decl_Pure;
