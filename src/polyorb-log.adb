------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Characters.Handling;
with Ada.Text_IO; use Ada.Text_IO;

with PolyORB.Configuration;

package body PolyORB.Log is

   -------------------
   -- Get_Log_Level --
   -------------------

   Log_Section : constant String := "log";

   Default_Log_Level : constant Log_Level := Notice;

   function Get_Log_Level (Facility : in String) return Log_Level
   is
      Log_Level_Name : constant String
        := Ada.Characters.Handling.To_Upper
        (PolyORB.Configuration.Get_Conf
         (Section => Log_Section,
          Key     => Facility,
          Default => Log_Level'Image (Default_Log_Level)));
   begin
      for Level in Log_Level'Range loop
         if Log_Level_Name = Log_Level'Image (Level) then
            return Level;
         end if;
      end loop;
      return Default_Log_Level;
   end Get_Log_Level;

   -------------------------------
   -- Generic body Facility_Log --
   -------------------------------

   package body Facility_Log is

      Initialized : Boolean := False;
      Facility_Level : Log_Level := Info;

      procedure Output
        (Message : in String;
         Level   : Log_Level := Debug)
      is
      begin
         if not Initialized then
            Facility_Level := Get_Log_Level (Facility);
            Initialized := True;
         end if;

         if Level >= Facility_Level then
            Put_Line (Facility & ": " & Message);
         end if;
      end Output;
   end Facility_Log;

end PolyORB.Log;
