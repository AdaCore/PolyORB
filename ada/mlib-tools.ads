------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                           M L I B . T O O L S                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--              Copyright (C) 1999, Ada Core Technologies, Inc.             --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides an easy way of calling various tools such as gcc,
--  gnatbind, ar, etc...

package MLib.Tools is

   procedure Delete_File (Filename : in String);
   --  delete the file filename.

   procedure Gcc (Output_File : in String;
                  Objects     : in Argument_List;
                  Options     : in Argument_List;
                  Base_File   : in String := "";
                  Build_Lib   : in Boolean := False);
   --  run gcc binary.

   procedure Gnatbind (Alis : in Argument_List);
   --  run gnatbind binary on a given set of ali files to generate the
   --  adainit/adafinal routines

   procedure Ar (Output_File : in String;
                 Objects     : in Argument_List);
   --  run ar to move all the binaries inside the archive.
   --  it run the command : ar m output_file objects

   procedure Locate;
   --  look for the tools in the path and record the full path for each one
   --  in a variable.

end MLib.Tools;
