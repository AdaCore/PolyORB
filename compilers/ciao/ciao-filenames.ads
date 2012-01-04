------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       C I A O . F I L E N A M E S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

--  Mapping of file names
with Asis;

package CIAO.Filenames is

   --  The name of the IDL file that contains the mapping of the
   --  given Ada file.
   function IDL_File_Name (Ada_File_Name : String)
     return String;

   --  The name of the source file that contains the declaration
   --  or body of the library unit whose full name is given.
   type Unit_Part is (Unit_Declaration, Unit_Body);

   function Ada_File_Name (Full_Name : Asis.Program_Text;
                           Part      : Unit_Part := Unit_Declaration)
     return String;

end CIAO.Filenames;
