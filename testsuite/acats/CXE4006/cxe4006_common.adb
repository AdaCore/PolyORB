------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 6 _ C O M M O N                        --
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

---
-- This package is pure so it cannot depend upon Report
---
package body CXE4006_Common is

  procedure Single_Controlling_Operand (
      RTT         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
    Expected : Integer;
  begin
    case Test_Number is
      when 1001 => Expected := 100;
      when 2001 => Expected := 200;
      when others => raise Failed_Check;
    end case;

    if RTT.Common_Record_Field /= Expected then
      raise Failed_Check;
    end if;

    RTT.Common_Record_Field := Expected + 5;
    Callee := Common_Spec;
  end Single_Controlling_Operand;

end CXE4006_Common;
