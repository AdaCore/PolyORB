------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

with PolyORB.Utils.Report;

with PolyORB.Utils.RT_Calendar;
pragma Elaborate_All (PolyORB.Utils.RT_Calendar);
pragma Warnings (Off, PolyORB.Utils.RT_Calendar);
--  Instead of with'ing the tasking policy we directly with' the
--  calendar implementation, as full-tasking and ravenscar profiles
--  rely on rt_calendar: this is just to simplify the testing
--  procedure

with PolyORB.Calendar;
with PolyORB.Initialization;

procedure Test001 is
   use PolyORB.Utils.Report;
   use PolyORB.Calendar;

begin
   PolyORB.Initialization.Initialize_World;

   declare
      Clock1 : Time_Type'Class := Clock;
      Clock2 : Time_Type_Access;
      Duree  : Duration;

      Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration;
   begin

      New_Test ("Tasking policies");

      Clock2 := Create;
      Clock2.all := Clock;

      Output ("Comparison between two clocks", Clock1 <= Clock2.all);

      Duree := Clock2.all - Clock1;
      Clock1 := Clock2.all + 2 * Duree;
      Output ("Additions and substractions", Clock1 > Clock2.all);

      Split (Clock1,
             Year => Year,
             Month => Month,
             Day => Day,
             Seconds => Seconds);
      Clock2.all := Time_Of (Year => Year,
                             Month => Month,
                             Day => Day,
                             Seconds => Seconds);
      Output ("Time splitting/construction", Clock1 = Clock2.all);

      End_Report;
   end;
end Test001;
