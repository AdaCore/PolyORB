------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

pragma Warnings (Off);
with System.IO; use System.IO;
--  Package System.IO is GNAT-specific; for other compilers use
--  Ada.Text_IO instead.
pragma Warnings (On);

--  with PolyORB.Configuration;

package body PolyORB.Log is

   -------------------
   -- Get_Log_Level --
   -------------------

   function Get_Log_Level (Facility : in String) return Log_Level;
   --  Returns the user-requested log level for facility Flag.

   function Get_Log_Level (Facility : in String) return Log_Level
   is
   begin
      if Get_Conf_Hook /= null then
         return Log_Level'Value
           (Get_Conf_Hook
              (Section => Log_Section,
               Key     => Facility,
               Default => Log_Level'Image (Default_Log_Level)));
      else
         return Default_Log_Level;
      end if;
   end Get_Log_Level;

   -------------------------------
   -- Generic body Facility_Log --
   -------------------------------

   package body Facility_Log is

      Initialized    : Boolean   := False;
      Facility_Level : Log_Level := Info;
      Counter        : Natural   := 0;

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

      procedure Increment is
         Old_Counter : constant Natural := Counter;
      begin
         Counter := Counter + 1;
         Output ("Counter "
                 & Integer'Image (Old_Counter)
                 & " -> "
                 & Integer'Image (Counter));
      end Increment;

      procedure Decrement is
         Old_Counter : constant Natural := Counter;
      begin
         Counter := Counter - 1;

         if Counter < 0 then
            raise Program_Error;
         end if;

         Output ("Counter "
                 & Integer'Image (Old_Counter)
                 & " -> "
                 & Integer'Image (Counter));
      end Decrement;
   end Facility_Log;

end PolyORB.Log;
