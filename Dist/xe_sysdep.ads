------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides interfaces to operating systems calls dealing with
--  files. The package body is selected at configuration time.

package XE_Sysdep is

   procedure Copy_File (From_File, To_File : String);
   --  This function copy the file From_File to To_File and preserve the
   --  time stamps and all other file attributes associated to From_File.
   --  Raises XE_Utils.Fatal_Error if it fails.

   procedure Force_Remove (File : String);
   --  Remove file even if it is read-only. After this call File must have
   --  been removed from the file system.
   --  Raises XE_Utils.Fatal_Error if it fails.

   procedure Set_Executable_Attribute (File : String);
   --  Set Executable attribute on File. After this operation it must be
   --  possible to execute File from a shell.
   --  Raises XE_Utils.Fatal_Error if it fails.

end XE_Sysdep;
