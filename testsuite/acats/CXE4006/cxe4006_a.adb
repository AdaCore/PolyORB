
-----------------------------------------------------------------------------

with CXE4006_Common;
with CXE4006_Part_A1;
with CXE4006_Part_A2;
with Report;
procedure CXE4006_A is
begin
  -- this partition is a server that deals with calls
  -- from CXE4006_B.
  Report.Test ("CXE4006_A", "Remote dispatching calls (server)");
  CXE4006_Part_A1.Can_Quit; -- OK to quit now.

  -- Report.Result is called in the body of CXE4006_Part_A1.
end CXE4006_A;
