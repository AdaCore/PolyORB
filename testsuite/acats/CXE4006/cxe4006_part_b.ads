------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C X E 4 0 0 6 _ P A R T _ B                        --
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

-----------------------------------------------------------------------------

with CXE4006_Common;  use CXE4006_Common;
with CXE4006_Part_A1;
package CXE4006_Part_B is
  pragma Remote_Call_Interface;

  -- tagged types that can be passed between partitions
  type B_Tagged_Type is new Root_Tagged_Type
       with null record;

  procedure Single_Controlling_Operand (
      RTT         : in out B_Tagged_Type;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);

  procedure Wrapped_Around (
      X           : in out Root_Tagged_Type'Class;
      Test_Number : in     Integer;
      Callee      :    out Type_Decl_Location);
end CXE4006_Part_B;
