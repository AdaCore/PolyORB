------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.TASKING.PROFILES.RAVENSCAR.CALENDAR                 --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Calendar; use PolyORB.Calendar;
with Ada.Real_Time;

package PolyORB.Tasking.Profiles.Ravenscar.Calendar is

   procedure Initialize;
   --  Initializes this package

   type Ravenscar_Time_Type is new Time_Type with private;
   type Ravenscar_Time_Type_Access is access all Ravenscar_Time_Type;

   type Ravenscar_Clock_Factory is new Clock_Factory_Type with null record;

   function Create (CF : access Ravenscar_Clock_Factory)
                   return Time_Type_Access;

   function Clock (CF : access Ravenscar_Clock_Factory)
                     return Time_Type'Class;

   procedure Destroy (CF : access Ravenscar_Clock_Factory;
                      Clock : in out Time_Type_Access);

   procedure Split
     (Date    : Ravenscar_Time_Type;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);

   function Year    (Date : Ravenscar_Time_Type) return Year_Number;
   function Month   (Date : Ravenscar_Time_Type) return Month_Number;
   function Day     (Date : Ravenscar_Time_Type) return Day_Number;
   function Seconds (Date : Ravenscar_Time_Type) return Day_Duration;

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
      return    Ravenscar_Time_Type;

   function "+" (Left : Ravenscar_Time_Type; Right : Duration)
                return Ravenscar_Time_Type;
   function "+" (Left : Duration; Right : Ravenscar_Time_Type)
                return Ravenscar_Time_Type;
   function "-" (Left : Ravenscar_Time_Type; Right : Duration)
                return Ravenscar_Time_Type;
   function "-" (Left : Ravenscar_Time_Type; Right : Ravenscar_Time_Type)
                return Duration;

   function "<"  (Left, Right : Ravenscar_Time_Type) return Boolean;
   function "<=" (Left, Right : Ravenscar_Time_Type) return Boolean;
   function ">"  (Left, Right : Ravenscar_Time_Type) return Boolean;
   function ">=" (Left, Right : Ravenscar_Time_Type) return Boolean;

   type Ravenscar_Clock_Factory_Access is access all Ravenscar_Clock_Factory;

   The_Ravenscar_Clock_Factory : aliased Ravenscar_Clock_Factory;

private

   type Ravenscar_Time_Type is new Time_Type with record
      Time : Ada.Real_Time.Time;
   end record;

end PolyORB.Tasking.Profiles.Ravenscar.Calendar;
