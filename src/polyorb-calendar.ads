------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . C A L E N D A R                      --
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

--  PolYORB.Calendar provides a uniform API to manipulate time, close
--  to Ada.Calendar. It is compatible with the No Tasking, Full
--  Tasking and Ravenscar profiles.

--  $Id$

package PolyORB.Calendar is

   pragma Preelaborate;

   type Time_Type is abstract tagged null record;
   type Time_Type_Access is access all Time_Type'Class;

   subtype Year_Number  is Integer range 1901 .. 2099;
   subtype Month_Number is Integer range 1 .. 12;
   subtype Day_Number   is Integer range 1 .. 31;
   subtype Day_Duration is Duration range 0.0 .. 86_400.0;

   function Create return Time_Type_Access;

   procedure Destroy (Clock : in out Time_Type_Access);

   function Clock return Time_Type'Class;

   procedure Split
     (Date    :     Time_Type;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
      is abstract;

   function Time_Of
     (Year    : Year_Number;
      Month   : Month_Number;
      Day     : Day_Number;
      Seconds : Day_Duration := 0.0)
     return Time_Type
      is abstract;

   function Year    (Date : Time_Type) return Year_Number is abstract;
   function Month   (Date : Time_Type) return Month_Number is abstract;
   function Day     (Date : Time_Type) return Day_Number is abstract;
   function Seconds (Date : Time_Type) return Day_Duration is abstract;

   function "+"
     (Left  : Time_Type;
      Right : Duration)
     return Time_Type
      is abstract;

   function "+"
     (Left  : Duration;
      Right : Time_Type)
     return Time_Type
      is abstract;

   function "-"
     (Left  : Time_Type;
      Right : Duration)
     return Time_Type
      is abstract;

   function "-"
     (Left  : Time_Type;
      Right : Time_Type)
     return Duration
      is abstract;

   function "<"  (Left, Right : Time_Type) return Boolean is abstract;
   function "<=" (Left, Right : Time_Type) return Boolean is abstract;
   function ">"  (Left, Right : Time_Type) return Boolean is abstract;
   function ">=" (Left, Right : Time_Type) return Boolean is abstract;

   Time_Error : exception;

   -------------------
   -- Clock_Factory --
   -------------------

   --  The following functions are not meant to be called by the
   --  applications.

   type Clock_Factory_Type is abstract tagged null record;
   type Clock_Factory_Access is access all Clock_Factory_Type'Class;

   function Clock
     (CF : access Clock_Factory_Type)
     return Time_Type'Class
     is abstract;

   function Create
     (CF : access Clock_Factory_Type)
     return Time_Type_Access
      is abstract;

   procedure Destroy
     (CF    : access Clock_Factory_Type;
      Clock : in out Time_Type_Access)
      is abstract;

   procedure Register_Clock_Factory (CF : Clock_Factory_Access);

end PolyORB.Calendar;
