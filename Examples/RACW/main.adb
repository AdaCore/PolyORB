------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                                M A I N                                   --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$                              --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
--                                                                          --
-- GLADE  is  free software;  you  can redistribute  it  and/or  modify  it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version.  GLADE  is  distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General  Public  License  distributed with GLADE;  see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                GLADE is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Common; use Common;
with Controller; use Controller;
procedure Main is
   task type Anonymous_Task is
      pragma Storage_Size (150000);
      entry Start (D : Integer);
   end Anonymous_Task;
   task body Anonymous_Task is
      W : Worker_Access;
      J : Job;
   begin
      accept Start (D : Integer) do
	 J.Job_Duration := D;
      end Start;
      W := Get_Worker;
      Do_Job (W, J);
      Register (W);
   exception when others =>
      Put_Line ("Anonymous is dead");
   end Anonymous_Task;
   Table : array (1 .. 15) of Anonymous_Task;
begin
   for I in Table'Range loop
      Table (I).Start (I);
   end loop;
end Main;
