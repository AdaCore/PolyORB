------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             POLYORB.TASKING.PROFILES.NO_TASKING.CALENDAR                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2003 Free Software Foundation, Inc.              --
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

with Ada.Unchecked_Deallocation;
with Ada.Calendar;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Utils.Strings;

package body PolyORB.Tasking.Profiles.No_Tasking.Calendar is

   procedure Free is new Ada.Unchecked_Deallocation
     (No_Tasking_Time_Type, No_Tasking_Time_Type_Access);

   ------------
   -- Create --
   ------------

   function Create
     (CF : access No_Tasking_Clock_Factory)
     return Time_Type_Access
   is
      pragma Warnings (Off);
      pragma Unreferenced (CF);
      pragma Warnings (On);

   begin
      return new No_Tasking_Time_Type;
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (CF    : access No_Tasking_Clock_Factory;
      Clock : in out Time_Type_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (CF);
      pragma Warnings (On);

   begin
      Free (No_Tasking_Time_Type_Access (Clock));
   end Destroy;

   -----------
   -- Split --
   -----------

   procedure Split
     (Date    :     No_Tasking_Time_Type;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
   begin
      Ada.Calendar.Split (Date.Time, Year, Month, Day, Seconds);
   end Split;

   -------------
   -- Time_Of --
   -------------

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
     return    No_Tasking_Time_Type
   is
      Result : No_Tasking_Time_Type;

   begin
      Result.Time := Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
      return Result;
   end Time_Of;

   ---------
   -- Day --
   ---------

   function Day (Date : No_Tasking_Time_Type) return Day_Number is
   begin
      return Ada.Calendar.Day (Date.Time);
   end Day;

   -----------
   -- Month --
   -----------

   function Month (Date : No_Tasking_Time_Type) return Month_Number is
   begin
      return Ada.Calendar.Month (Date.Time);
   end Month;

   -------------
   -- Seconds --
   -------------

   function Seconds (Date : No_Tasking_Time_Type) return Day_Duration is
   begin
      return Ada.Calendar.Seconds (Date.Time);
   end Seconds;

   ------------
   --  Year  --
   ------------

   function Year    (Date : No_Tasking_Time_Type) return Year_Number is
   begin
      return Ada.Calendar.Year (Date.Time);
   end Year;

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : No_Tasking_Time_Type;
      Right : Duration)
     return No_Tasking_Time_Type
   is
      use Ada.Calendar;

   begin
      return No_Tasking_Time_Type'(Time => Left.Time + Right);
   end "+";

   function "+"
     (Left  : Duration;
      Right : No_Tasking_Time_Type)
     return No_Tasking_Time_Type
   is
      use Ada.Calendar;

   begin
      return No_Tasking_Time_Type'(Time => Left + Right.Time);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (Left  : No_Tasking_Time_Type;
      Right : Duration)
     return No_Tasking_Time_Type
   is
      use Ada.Calendar;

   begin
      return No_Tasking_Time_Type'(Time => Left.Time - Right);
   end "-";

   function "-"
     (Left  : No_Tasking_Time_Type;
      Right : No_Tasking_Time_Type)
     return Duration
   is
      use Ada.Calendar;

   begin
      return Left.Time - Right.Time;
   end "-";

   ---------
   -- "<" --
   ---------

   function "<"  (Left, Right : No_Tasking_Time_Type) return Boolean is
      use Ada.Calendar;

   begin
      return Left.Time < Right.Time;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<=" (Left, Right : No_Tasking_Time_Type) return Boolean is
      use Ada.Calendar;

   begin
      return Left.Time <= Right.Time;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">"  (Left, Right : No_Tasking_Time_Type) return Boolean is
      use Ada.Calendar;

   begin
      return Left.Time > Right.Time;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : No_Tasking_Time_Type) return Boolean is
      use Ada.Calendar;

   begin
      return Left.Time >= Right.Time;
   end ">=";

   -----------
   -- Clock --
   -----------

   function Clock
     (CF : access No_Tasking_Clock_Factory)
     return Time_Type'Class
   is
      pragma Warnings (Off);
      pragma Unreferenced (CF);
      pragma Warnings (On);

   begin
      return No_Tasking_Time_Type'(Time => Ada.Calendar.Clock);
   end Clock;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.Calendar.Register_Clock_Factory
        (The_No_Tasking_Clock_Factory'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"tasking.profiles.no_tasking.calendar",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"calendar",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Tasking.Profiles.No_Tasking.Calendar;
