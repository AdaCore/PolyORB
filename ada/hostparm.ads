------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             H O S T P A R M                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$                              --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  This package defines some system dependent parameters for GNAT. These
--  are parameters that are relevant to the host machine on which the
--  compiler is running, and thus this package is part of the compiler.

package Hostparm is
pragma Pure (Hostparm);

   OpenVMS : constant Boolean := False;
   --  Set true for compilers for OpenVMS systems. This is really a target
   --  flag, which should be sorted out some time??? For now we have no cross
   --  compilers with OpenVMS as the target so there is no confusion.

   Long_Integer_Size : constant := 0;
   --  This is the length of the long integer type in bits. A value of
   --  zero means that the length is to be taken from the C type long.
   --  Otherwise a non-zero length overrides the C length. This is used
   --  for the Alpha VMS port, where Long_Integer is 64 bits, even though
   --  the C long type remains at 32 bits. This is really a target flag,
   --  which should be sorted out some time??? For now we have no cross
   --  compilers with OpenVMS as the target, so there is no confusion

   Normalized_CWD : constant String := "./";
   --  Normalized string to access current directory

   Max_Line_Length : constant := 255;
   --  Maximum source line length. This can be set to any value up to
   --  2**15 - 1, a limit imposed by the assumption that column numbers
   --  can be stored in 16 bits (see Types.Column_Number). A value of
   --  200 is the minimum value required (RM 2.2(15)), but we use 255
   --  for most GNAT targets since this is DEC Ada compatible.

   Max_Name_Length : constant := 1024;
   --  Maximum length of unit name (including all dots, and " (spec)") and
   --  of file names in the library, must be at least Max_Line_Length, but
   --  can be larger.

   Max_Instantiations : constant := 4000;
   --  Maximum number of instantiations permitted (to stop runaway cases
   --  of nested instantiations). These situations probably only occur in
   --  specially concocted test cases.

   Tag_Errors : constant Boolean := False;
   --  If set to true, then brief form error messages will be prefaced by
   --  the string "error:"

   Exclude_Missing_Objects : constant Boolean := True;
   --  If set to true, gnatbind will exclude from consideration all
   --  non-existent .o files.

end Hostparm;
