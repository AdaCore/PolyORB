------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             O U T F I L E S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

--  Management of output files

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Types;       use Types;

package Outfiles is

   function Set_Output (File_Name : Name_Id) return File_Descriptor;
   --  Adjust the output depending on the command line options (output
   --  to files or to stdout), and return a file descriptor for subsequent
   --  call to Release_Output.

   procedure Release_Output (Fd : File_Descriptor);
   --  Releases the output by closing the opened file descriptor, if required

end Outfiles;
