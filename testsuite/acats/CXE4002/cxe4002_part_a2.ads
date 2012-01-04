------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      C X E 4 0 0 2 _ P A R T _ A 2                       --
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

pragma Style_Checks (Off);
-----------------------------------------------------------------------------

package CXE4002_Part_A2 is
  -- This package supports the remote access tests
  pragma Remote_Call_Interface;

  procedure Call_With_2 (T : Integer);
  procedure Call_With_3 (T : Integer);

  procedure Mixed_1 (X : in Integer;  Y : out Integer; Z : in out Integer);
  procedure Mixed_2 (X : in Integer;  Y : out Integer; Z : in out Integer);

  type Remote_Proc is access procedure (X : Integer);
  type Remote_Proc_Mixed is access 
        procedure (A : in Integer;  B : out Integer;  C : in out Integer);

end CXE4002_Part_A2;
