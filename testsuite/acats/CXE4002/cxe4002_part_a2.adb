pragma Style_Checks (Off);
-----------------------------------------------------------------------------

with Report;
package body CXE4002_Part_A2 is

  procedure Call_With_2 (T : Integer) is
  begin
    if T /= 2 then
      Report.Failed ("expected 2 but received" & Integer'Image (T));      
    end if;
  end;

  procedure Call_With_3 (T : Integer) is
  begin
    if T /= 3 then
      Report.Failed ("expected 3 but received" & Integer'Image (T));      
    end if;
  end;

  procedure Mixed_1 (X : in Integer;  Y : out Integer; Z : in out Integer) is
  begin
    if X /= 20    or
       Z /= 30    then
      Report.Failed ("Mixed_1 IN parameters are not the expected value");
    end if;
    Y := 25;
    Z := 35;
  end Mixed_1;

  procedure Mixed_2 (X : in Integer;  Y : out Integer; Z : in out Integer) is
  begin
    if X /= 200    or
       Z /= 300    then
      Report.Failed ("Mixed_2 IN parameters are not the expected value");
    end if;
    Y := 250;
    Z := 350;
  end Mixed_2;

end CXE4002_Part_A2;
