------------------------------------------------------------------------------
--                                                                          --
--                           ADABROKER SERVICES                             --
--                                                                          --
--                     C O S T I M E . T I O . I M P L                      --
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

with Broca.Server_Tools;
with CORBA;
with CosTime.TIO.Helper;
with CosTime.TIO.Skel;
with CosTime.UTO.Helper;
with CosTime.UTO.Impl;
with PortableServer;
with Time_Utils;          use Time_Utils;

package body CosTime.TIO.Impl is

   use TimeBase;

   type TIO_Ptr is access Object;
   type UTO_Ptr is access UTO.Impl.Object;

   procedure Do_Overlap
     (A_Interval : in IntervalT;
      B_Interval : in IntervalT;
      Returns    : out OverlapType;
      Overlaps   : out IntervalT);

   ----------------
   -- Do_Overlap --
   ----------------

   procedure Do_Overlap
     (A_Interval : in IntervalT;
      B_Interval : in IntervalT;
      Returns    : out OverlapType;
      Overlaps   : out IntervalT)
   is
   begin
      if A_Interval.Upper_Bound < B_Interval.Lower_Bound
        or else A_Interval.Lower_Bound > B_Interval.Upper_Bound
      then
         Returns := OTNoOverlap;
         Overlaps.Lower_Bound :=
           TimeT'Min (A_Interval.Upper_Bound, B_Interval.Upper_Bound);
         Overlaps.Upper_Bound :=
           TimeT'Max (A_Interval.Lower_Bound, B_Interval.Lower_Bound);
      elsif A_Interval.Lower_Bound <= B_Interval.Lower_Bound
        and then A_Interval.Upper_Bound >= B_Interval.Upper_Bound
      then
         Returns := OTContainer;
         Overlaps := B_Interval;
      elsif A_Interval.Lower_Bound >= B_Interval.Lower_Bound
        and then A_Interval.Upper_Bound <= B_Interval.Upper_Bound
      then
         Returns := OTContained;
         Overlaps := A_Interval;
      else
         Returns := OTOverlap;
         Overlaps.Lower_Bound :=
           TimeT'Max (A_Interval.Lower_Bound, B_Interval.Lower_Bound);
         Overlaps.Upper_Bound :=
           TimeT'Min (A_Interval.Upper_Bound, B_Interval.Upper_Bound);
      end if;
   end Do_Overlap;

   -----------------------
   -- get_time_interval --
   -----------------------

   function get_time_interval
     (Self : access Object)
     return IntervalT
   is
   begin
      return Self.Interval;
   end get_time_interval;

   --------------
   -- overlaps --
   --------------

   procedure overlaps
     (Self : access Object;
      interval : in CosTime.TIO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out OverlapType)
   is
      A_Interval : IntervalT renames Self.Interval;
      B_Interval : constant IntervalT := get_time_interval (interval);
      Result     : constant TIO_Ptr := new Object;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => get_time_interval (interval),
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      Broca.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), overlap);
   end overlaps;

   -----------
   -- spans --
   -----------

   procedure spans
     (Self : access Object;
      time : in CosTime.UTO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out OverlapType)
   is
      Tim        : constant TimeT       := UTO.get_time (time);
      Ina        : constant InaccuracyT := UTO.get_inaccuracy (time);
      B_Interval : constant IntervalT   := (Lower_Bound => Tim - Ina,
                                            Upper_Bound => Tim + Ina);
      Result     : constant TIO_Ptr := new Object;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => B_Interval,
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      Broca.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), overlap);
   end spans;

   ----------
   -- time --
   ----------

   function time
     (Self : access Object)
     return UTO.Ref
   is
      Result : constant UTO_Ptr := new UTO.Impl.Object;
      R      : UTO.Ref;
   begin
      Result.Time :=
        (Self.Interval.Upper_Bound - Self.Interval.Lower_Bound) / 2;
      Result.Inaccuracy := InaccuracyT
        (Self.Interval.Upper_Bound - Self.Interval.Lower_Bound);
      Broca.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end time;

end CosTime.TIO.Impl;
