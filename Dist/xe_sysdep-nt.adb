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
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

--  this is the NT version of this package.

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with XE_Utils;     use XE_Utils;

package body XE_Sysdep is

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (From_File, To_File : String) is

      function System (C : String) return Integer;
      pragma Import (C, System);

      Command : constant String :=
        "copy " & From_File & ' ' & To_File & " > nul" & ASCII.NUL;

   begin
      if System (Command) /= 0 then
         Message ("cannot copy file " & From_File & " to " & To_File);
         raise Fatal_Error;
      end if;
   end Copy_File;

   ------------------
   -- Force_Remove --
   ------------------

   --  A file with read-only attribute can't be removed by calling unlink like
   --  under UNIX. Therefore it is needed to set the file write attribute
   --  before removing it.

   procedure Force_Remove (File : String) is

      C_File  : constant String := File & ASCII.NUL;
      Success : Boolean;

      Std_Read_Write : constant := 16#8180#;
      --  regular file with read/write attribute

      function Chmod (File : String; Mode : Integer) return Integer;
      pragma Import (C, Chmod);

   begin
      if Chmod (C_File, Std_Read_Write) /= 0 then
         null;
      end if;

      Delete_File (C_File'Address, Success);
   end Force_Remove;

   ------------------------------
   -- Set_Executable_Attribute --
   ------------------------------

   --  there is nothing to be done under NT. An executable is a file with
   --  ".exe" suffix under NT. There is no executable file attribute under NT.

   procedure Set_Executable_Attribute (File : String) is
   begin
      null;
   end Set_Executable_Attribute;

end XE_Sysdep;
