------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                            X E _ S Y S D E P                             --
--                                                                          --
--                                 B o d y                                  --
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

--  This is the standard version of this package. It will works fine on any
--  UNIX like operating system.
--
--  The requirements are :
--
--  . chmod tool must be found and support "u+x" (add execute attribute for
--    the user) to a file.

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with XE_Utils;     use XE_Utils;

package body XE_Sysdep is

   Chmod : String_Access;

   ------------------
   -- Force_Remove --
   ------------------

   procedure Force_Remove (File : String) is
      C_File  : constant String := File & ASCII.NUL;
      Success : Boolean;
   begin
      Delete_File (C_File'Address, Success);
   end Force_Remove;

   ------------------------------
   -- Set_Executable_Attribute --
   ------------------------------

   procedure Set_Executable_Attribute (File : String) is
   begin
      if Chmod = null then
         --  looks for chmod in the PATH
         Chmod := Locate_Exec_On_Path ("chmod");

         if Chmod = null then
            Message ("chmod is not in your path");
            raise Fatal_Error;
         end if;
      end if;

      Execute
        (Chmod,
         (1 => new String'("u+x"),
          2 => new String'(File)));
   end Set_Executable_Attribute;

end XE_Sysdep;
