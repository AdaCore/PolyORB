------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 5 _ C O M M O N                        --
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

package CXE4005_Common is
  pragma Pure;

  -- controls progress output from the tests.  
  Verbose : constant Boolean := False;

  -- exception to signify that the serial number of an object
  -- was not a one of the expected values for that type
  Wrong_Object : exception;

  -- identification of where a type is declared and where
  -- an access type was evaluated that refers to an object
  -- of that type.
  type Type_Selection is (Common_Spec,        --  xx1
                          RT_Spec,            --  xx6
                          B_Body,             --  xx7
                          Normal_Spec);       --  xx8
  type Access_Evaluation is (A1,              --  1xx
                             A2,              --  2xx
                             B);              --  3xx

  -- root tagged type for remote access to class wide type test
  type Root_Tagged_Type is tagged limited private;

  procedure Single_Controlling_Operand (
      RTT         : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN      : out    Integer);
  procedure Dual_Controlling_Operands (
      RTT1        : access Root_Tagged_Type;
      RTT2        : access Root_Tagged_Type;
      Test_Number : in     Integer;
      Obj_SN1     : out    Integer;
      Obj_SN2     : out    Integer);

  procedure Set_Serial_Number (
      RTT         : access Root_Tagged_Type;
      Sn          : in     Integer);

  function Serial_Number (RTT : access Root_Tagged_Type) return Integer;

  type Open_Tagged_Type is tagged 
    record
       Field : Integer;
    end record;
  procedure Open_Op (OTT : Open_Tagged_Type);

private
  type Root_Tagged_Type is tagged limited
    record
      Serial_Number        : Integer := 123;
    end record;
end CXE4005_Common;
