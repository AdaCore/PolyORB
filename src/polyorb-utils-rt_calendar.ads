------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . U T I L S . R T _ C A L E N D A R             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

--  This package is to be used through PolyORB.Calendar.
--  WARNING: This package relies on Ada.Real_Time, which will load the
--  ada tasking libraries

with PolyORB.Calendar; use PolyORB.Calendar;
with Ada.Real_Time;

package PolyORB.Utils.RT_Calendar is

   type RT_Time_Type is new Time_Type with private;
   type RT_Time_Type_Access is access all RT_Time_Type;

   procedure Split
     (Date    :     RT_Time_Type;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration);

   function Year    (Date : RT_Time_Type) return Year_Number;
   function Month   (Date : RT_Time_Type) return Month_Number;
   function Day     (Date : RT_Time_Type) return Day_Number;
   function Seconds (Date : RT_Time_Type) return Day_Duration;

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
     return RT_Time_Type;

   function "+" (Left : RT_Time_Type; Right : Duration) return RT_Time_Type;
   function "+" (Left : Duration; Right : RT_Time_Type) return RT_Time_Type;

   function "-" (Left : RT_Time_Type; Right : Duration) return RT_Time_Type;
   function "-" (Left : RT_Time_Type; Right : RT_Time_Type) return Duration;

   function "<"  (Left, Right : RT_Time_Type) return Boolean;
   function "<=" (Left, Right : RT_Time_Type) return Boolean;
   function ">"  (Left, Right : RT_Time_Type) return Boolean;
   function ">=" (Left, Right : RT_Time_Type) return Boolean;

   type RT_Clock_Factory is new Clock_Factory_Type with null record;
   type RT_Clock_Factory_Access is access all RT_Clock_Factory;

   function Create (CF : access RT_Clock_Factory) return Time_Type_Access;

   function Clock (CF : access RT_Clock_Factory) return Time_Type'Class;

   procedure Destroy
     (CF    : access RT_Clock_Factory;
      Clock : in out Time_Type_Access);

private

   type RT_Time_Type is new Time_Type with record
      Time : Ada.Real_Time.Time;
   end record;

end PolyORB.Utils.RT_Calendar;
