------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . L O G                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System;

package body PolyORB.Log is

   procedure Output
     (Initialized    : in out Boolean;
      Facility_Level : in out Log_Level;
      Facility       : access String;
      Message        : access String;
      Level          : Log_Level := Debug);
   --  Common code shared by all instances of Facility_Log:
   --  * if Initialized is not True, look up log level for Facility,
   --    cache it in Facility_Level, and set Initialized to True.
   --  * then in all cases, output Message if Level >= Facility_Level.

   -------------------
   -- Get_Log_Level --
   -------------------

   function Get_Log_Level (Facility : in String) return Log_Level;
   --  Returns the user-requested log level for facility Flag.

   function Get_Log_Level (Facility : in String)
                          return Log_Level is
   begin
      if Get_Conf_Hook /= null then
         return Log_Level'Value
           (Get_Conf_Hook
              (Section => Log_Section,
               Key     => Facility,
               Default => Log_Level'Image (Default_Log_Level)));
      else
         return Unknown;
      end if;
   end Get_Log_Level;

   -------------------------------
   -- Generic body Facility_Log --
   -------------------------------

   package body Facility_Log is

      Initialized    : Boolean   := False;
      Facility_Level : Log_Level := Info;

      ------------
      -- Output --
      ------------

      procedure Output
        (Message : String;
         Level   : Log_Level := Debug) is
      begin
         Log.Output (Initialized, Facility_Level, Facility'Unrestricted_Access,
           Message'Unrestricted_Access, Level);
      end Output;

   end Facility_Log;

   --------------------------------
   -- Package body for Internals --
   --------------------------------

   package body Internals is

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String)
      is
         SS : aliased String := S & ASCII.LF;

         procedure C_Write
           (Fd  : Interfaces.C.int;
            P   : System.Address;
            Len : Interfaces.C.int);
         pragma Import (C, C_Write, "write");
      begin
         C_Write (2, SS (SS'First)'Address, SS'Length);
         --  2 is standard error.

      end Put_Line;

   end Internals;

   ------------
   -- Output --
   ------------

   procedure Output
     (Initialized    : in out Boolean;
      Facility_Level : in out Log_Level;
      Facility       : access String;
      Message        : access String;
      Level          : Log_Level := Debug) is
   begin
      if not Initialized then
         Facility_Level := Get_Log_Level (Facility.all);
         if Facility_Level /= Unknown then
            Initialized := True;
         end if;
      end if;

      if Level >= Facility_Level then
         Internals.Put_Line (Facility.all & ": " & Message.all);
      end if;
   end Output;

end PolyORB.Log;
