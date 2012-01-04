------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 2 _ P A R T _ A 2                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2012, Free Software Foundation, Inc.             --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

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
