pragma Style_Checks (Off);
-----------------------------------------------------------------------------

package body CXE2001_Shared is
  protected body Shared_Counter is
    procedure Increment is
    begin
      Count := Count + 1;
    end Increment;

    function Value return Integer is
    begin
      return Count;
    end Value;
  end Shared_Counter;
end CXE2001_Shared;
