with GNAT.Perfect_Hash.Generators;
use GNAT.Perfect_Hash.Generators;
procedure Harness_Gen_Table is
begin
   Initialize (1223);
   --  Insert ("_Is_A");
   Insert ("echoULong1");
   Insert ("echoULong2");
   Insert ("echoULong3");
   Insert ("echoULong4");
   Insert ("echoULong5");
   Insert ("echoULong6");
   Insert ("echoULong7");
   Insert ("echoULong8");
   Insert ("echoULong9");
   Insert ("echoULong10");
   Insert ("echoULong11");
   Insert ("echoULong12");
   Insert ("echoULong13");
   Insert ("echoULong14");
   Insert ("echoULong15");
   Insert ("echoULong16");
   Insert ("echoULong17");
   Insert ("echoULong18");
   Insert ("echoULong19");
   Insert ("echoULong20");
   Compute;
   Produce ("harness_operation_hash");
   Finalize;

end Harness_Gen_Table;
