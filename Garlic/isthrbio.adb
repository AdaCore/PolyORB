------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             I S T H R B I O                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with Interfaces.C;       use Interfaces.C;
with System.Garlic.Thin; use System.Garlic.Thin;

with System.Garlic.TCP_Platform_Specific;
pragma Warnings (Off, System.Garlic.TCP_Platform_Specific);

--  Is Thread Blocking IO

procedure IsThrBIO is

   Process_Blocking_IO : Boolean;

   task A_Task is
      entry Start;
      entry Stop;
   end A_Task;

   Result  : int;
   Input   : constant Fd_Set_Access  := new Fd_Set;
   Timeout : constant Timeval_Access := new Timeval'(1, 0);

   ------------
   -- A_Task --
   ------------

   task body A_Task is
   begin
      accept Start;
      delay 0.1;
      select
         delay 0.1;
         Process_Blocking_IO := False;
      or
         accept Stop;
         Process_Blocking_IO := True;
      end select;
      select
         accept Stop;
      or
         terminate;
      end select;
   end A_Task;

begin
   Clear (Input.all);
   A_Task.Start;
   Result := C_Select (1, Input, null, null, Timeout);
   A_Task.Stop;
   if Process_Blocking_IO then
      OS_Exit (0);
   else
      OS_Exit (1);
   end if;
end IsThrBIO;
