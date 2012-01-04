------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 C X E 4 0 0 5 _ R E M O T E _ T Y P E S                  --
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

package body CXE4005_Remote_Types is
  --
  -- The serial number for all objects of RT_Tagged_Type will contain
  -- a 6 as the least significant digit.  Make sure the correct object
  -- is passed to these routines.
  --

  procedure Single_Controlling_Operand (
      RTT         : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer) is
  begin
    Obj_SN := Serial_Number(RTT);
    if Serial_Number(RTT) mod 10 /= 6 then
      raise Wrong_Object;
    end if;
  end Single_Controlling_Operand;

  procedure Dual_Controlling_Operands (
      RTT1        : access RT_Tagged_Type;
      RTT2        : access RT_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer) is
  begin
    Obj_SN1 := Serial_Number(RTT1);
    Obj_SN2 := Serial_Number(RTT2);
    if Serial_Number(RTT1) mod 10 /= 6 or 
       Serial_Number(RTT2) mod 10 /= 6 then
      raise Wrong_Object;
    end if;
  end Dual_Controlling_Operands;

end CXE4005_Remote_Types;
