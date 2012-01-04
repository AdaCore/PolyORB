------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 6 _ P A R T _ B                        --
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

-----------------------------------------------------------------------------

with CXE4006_Part_A1;
with CXE4006_Part_A2;
with CXE4006_Normal;
with Report;
with Ada.Tags;   use type Ada.Tags.Tag;
package body CXE4006_Part_B is

  type B_Body_Tagged_Type is new CXE4006_Common.Root_Tagged_Type
       with null record;

  Root_Obj   : CXE4006_Common.Root_Tagged_Type;
  A1_1_Obj   : CXE4006_Part_A1.A1_Tagged_Type_1;
  A1_2_Obj   : CXE4006_Part_A1.A1_Tagged_Type_2;
  A2_Obj     : CXE4006_Part_A2.A2_Tagged_Type;
  B_Obj      : CXE4006_Part_B.B_Tagged_Type;
  B_Body_Obj : CXE4006_Part_B.B_Body_Tagged_Type;
  Normal_Obj : CXE4006_Normal.Normal_Spec_Tagged_Type;

  procedure Single_Controlling_Operand (
      RTT         : in out B_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
    Expected : Integer := 0;
  begin
    case Test_Number is
      when 1005   => Expected := 140;
      when 2005   => Expected := 240;
      when others => Report.Failed ("CXE4006_Part_A2 bad test number" &
                                    Integer'Image (Test_Number));
    end case;

    if RTT.Common_Record_Field /= Expected then
      Report.Failed ("CXE4006_Part_B expected" &
                     Integer'Image (Expected) &
                     " but received" &
                     Integer'Image (RTT.Common_Record_Field) &
                     " in test" &
                     Integer'Image (Test_Number));
    end if;

    RTT.Common_Record_Field := Expected + 9;
    Callee := Part_B_Spec;

  end Single_Controlling_Operand;

  -- this procedure will pass all the parameters along to
  -- partition A CXE4006_Part_A1.Make_Dispatching_Call_With.
  -- Prior to making the call, the tag of X is checked to make
  -- sure it is correct.
  procedure Wrapped_Around (
      X           : in out Root_Tagged_Type'Class;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location) is
    Good_Tag : Boolean := False;
  begin
    if Verbose then
      Report.Comment ("wrap around test number" &
                      Integer'Image (Test_Number));
    end if;

    case Test_Number is
      when 2001 => Good_Tag := X'Tag = CXE4006_Common.Root_Tagged_Type'Tag;
      when 2002 => Good_Tag := X'Tag = CXE4006_Part_A1.A1_Tagged_Type_1'Tag;
      when 2003 => Good_Tag := X'Tag = CXE4006_Part_A1.A1_Tagged_Type_2'Tag;
      when 2004 => Good_Tag := X'Tag = CXE4006_Part_A2.A2_Tagged_Type'Tag;
      when 2005 => Good_Tag := X'Tag = CXE4006_Part_B.B_Tagged_Type'Tag;
      when 3001 =>
         Report.Failed ("test 3001 call should not have been made");
         return;  -- just to avoid extra error messages
      when others =>
         Report.Failed ("bad test number in wrap around" &
                        Integer'Image (Test_Number));
    end case;

    if not Good_Tag then
      Report.Failed ("unexpected tag value in wrap around test" &
                     Integer'Image (Test_Number));
    end if;

    CXE4006_Part_A1.Make_Dispatching_Call_With (X, Test_Number, Callee);
  end Wrapped_Around;

end CXE4006_Part_B;
