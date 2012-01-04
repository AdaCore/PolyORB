------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                F L A G S                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Types; use Types;

package Flags is

   Main_Source      : Types.Name_Id := Types.No_Name;
   --  IDL source name

   Use_Stdout       : Boolean       := False;
   --  True when we want to generate sources in the standard output

   Print_Full_Tree  : Boolean       := False;
   --  Output tree

   Preprocess_Only  : Boolean       := False;
   --  True when we only preprocess the IDL source file and output it

   No_Preprocess    : Boolean       := False;
   --  No preprocessing pass, assume input file has already been preprocessed

   Compile_Only     : Boolean       := False;
   --  True when we only compile the IDL source file and exit

   Output_Directory : String_Ptr    := null;
   --  The output directory

   Quiet            : Boolean       := False;
   --  Quiet mode: no output unless warnings or errors need to be displayed

end Flags;
