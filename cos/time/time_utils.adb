------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                           T I M E _ U T I L S                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;
with System;

package body Time_Utils is

   use Timebase;

   Time_Offset : constant TimeT := 141_427 * 86_400 * 10_000_000;
   --  Time offset between 15 october 1582 and 1 january 1970.

   ---------
   -- "+" --
   ---------

   function "+" (A : TimeBase.TimeT; B : TimeBase.InaccuracyT)
     return TimeBase.TimeT is
   begin
      return A + TimeBase.TimeT (B);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : TimeBase.TimeT; B : TimeBase.InaccuracyT)
     return TimeBase.TimeT is
   begin
      return A - TimeBase.TimeT (B);
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (A : TimeBase.TimeT; B : TimeBase.TdfT)
     return TimeBase.TimeT is
   begin
      return A + TimeBase.TimeT (B) * 600_000_000;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (A : TimeBase.TimeT; B : TimeBase.TdfT)
     return TimeBase.TimeT is
   begin
      return A + (-B);
   end "-";

   -------------
   -- Compare --
   -------------

   function Compare (A : TimeBase.TimeT; B : TimeBase.TimeT)
     return CosTime.TimeComparison
   is
      use CosTime;
   begin
      if A < B then
         return TCLessThan;
      elsif A = B then
         return TCEqualTo;
      else
         return TCGreaterThan;
      end if;
   end Compare;

   ------------------
   -- Current_Time --
   ------------------

   function Current_Time return TimeBase.TimeT is
      function Unix_Time (TLOC : System.Address := System.Null_Address)
                         return Interfaces.C.long;
      pragma Import (C, Unix_Time, "time");
   begin
      return TimeBase.TimeT (Unix_Time) * 10_000_000 + Time_Offset;
   end Current_Time;

   function Current_Inaccuracy return TimeBase.InaccuracyT is
   begin
      --  Time on Unix has 1 second of precision
      return 10_000_000;
   end Current_Inaccuracy;

   -----------------
   -- Current_Tdf --
   -----------------

   function Current_Tdf return TimeBase.TdfT is
   begin
      --  Return UTC
      return 0;
   end Current_Tdf;

end Time_Utils;
