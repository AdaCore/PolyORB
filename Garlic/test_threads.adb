------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                         T E S T _ T H R E A D S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-1999 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

--  This program tries to determine if a given platform supports
--  thread-blocking IOs.
--
--  The command line to use is: gnatmake -a test_threads
--

with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System.Garlic.Thin; use System.Garlic.Thin;

procedure Test_Threads is

   FD    : aliased Two_Int;
   Dummy : int;

   task Writer;
   task body Writer is
   begin
      delay 5.0;
      Dummy := C_Write (FD (1), New_String ("abcde"), 1);
   end Writer;

   Buffer : constant chars_ptr := New_String ("abcde");

begin
   Dummy := C_Pipe (FD'Access);
   Put_Line ("If the following test hangs more than 20 seconds, then kill");
   Put_Line ("it and assume the answer is 'This platform does *NOT* support");
   Put_Line ("thread-blocking IOs'");
   Dummy := C_Read (FD (0), Buffer, 1);
   Put_Line ("This platform *DOES* probably support thread-blocking IOs");
end Test_Threads;
