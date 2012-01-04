------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 5 _ C O M M O N                        --
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
package body CXE4005_Common is
  Op_Is_Zero : exception;   

  -- All objects that do not have an overriding definition of
  -- Single_Controlling_Operand and Dual_Controlling_Operands
  -- have a serial number with the least significant digit in
  -- the range from 1 to 5.
  -- If a wrong object is passed to these
  -- routines then the exception Wrong_Object is raised.

  procedure Single_Controlling_Operand (
      RTT         : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer) is
  begin
    Obj_SN := Serial_Number(RTT);
    if RTT.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
  end Single_Controlling_Operand;

  procedure Dual_Controlling_Operands (
      RTT1        : access Root_Tagged_Type;
      RTT2        : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer) is
  begin
    Obj_SN1 := RTT1.Serial_Number;
    Obj_SN2 := RTT2.Serial_Number;

    if RTT1.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
    if RTT2.Serial_Number mod 10 not in 1..5 then
      raise Wrong_Object;
    end if;
  end Dual_Controlling_Operands;

  procedure Set_Serial_Number (
      RTT         : access Root_Tagged_Type;
      Sn          : in     Integer) is
  begin
    RTT.Serial_Number := Sn;
  end Set_Serial_Number;

  function Serial_Number (RTT : access Root_Tagged_Type) return Integer is
  begin
    return RTT.Serial_Number;
  end Serial_Number;

  procedure Open_Op (OTT : Open_Tagged_Type) is
  begin
    if OTT.Field = 0 then
      raise Op_Is_Zero;
    end if;
  end Open_Op;

end CXE4005_Common;
