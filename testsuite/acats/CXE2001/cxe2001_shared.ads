pragma Style_Checks (Off);
-----------------------------------------------------------------------------

package CXE2001_Shared is
  pragma Shared_Passive;

  Shared_Data : Integer := 35;

  protected Shared_Counter is
    procedure Increment;
    function Value return Integer;
  private
    Count : Integer := 0;
  end Shared_Counter;
end CXE2001_Shared;
