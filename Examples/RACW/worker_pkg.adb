------------------------------------------------------------------------------
--                                                                          --
--                            GLADE EXAMPLES                                --
--                                                                          --
--                          W O R K E R _ P K G                             --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                           $Revision$
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

with Common;
with Controller;
package body Worker_Pkg is
   procedure Do_Job (W : access Real_Worker; J : Job) is
      D : Integer := J.Job_Duration;
      S : Integer := 0;
   begin
      while D > 0 loop
         delay 0.3;
         D := D - W.Speed;
         S := S + 1;
      end loop;
      Controller.Done ("Job (" & Integer'Image (S) &
                       " timeslots) at speed" & Integer'Image (W.Speed));
   end Do_Job;
   Local : aliased Real_Worker;
begin
   Controller.Get_Integer ("Speed : ", Local.Speed);
   Controller.Register (Local'Access);
end Worker_Pkg;
