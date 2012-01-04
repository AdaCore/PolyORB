------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 6 _ C O M M O N                        --
--                                                                          --
--                                 S p e c                                  --
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

package CXE4006_Common is
  pragma Pure;

  -- controls progress output from the tests.  The value of this
  -- flag does not affect whether or not the test passes.
  Verbose : constant Boolean := False;

  -- exception to signify that the test number or object
  -- was not a one of the expected values
  Failed_Check : exception;

  -- instances of types derived from Root_Tagged_Type.
  -- Used to identify the routine that received the dispatching call.

  type Type_Decl_Location is (
        Common_Spec,
        Part_A1_1_Spec,
        Part_A1_2_Spec,
        Part_A2_Spec,
        Part_B_Spec,
        Part_B_Body,
        Normal_Spec);

  -- root tagged type for remote access to class wide type test
  type Root_Tagged_Type is tagged
    record
      Common_Record_Field : Integer := 1234;
    end record;

  procedure Single_Controlling_Operand (
      RTT         : in out Root_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

end CXE4006_Common;
