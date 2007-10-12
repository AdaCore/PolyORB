------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     C O S T I M E . T I O . I M P L                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.CORBA_P.Server_Tools;

with Time_Utils;

with CosTime.UTO.Impl;

with CosTime.TIO.Skel;
pragma Warnings (Off, CosTime.TIO.Skel);

package body CosTime.TIO.Impl is

   use TimeBase;
   use Time_Utils;

   type TIO_Ptr is access Object;
   type UTO_Ptr is access UTO.Impl.Object;

   procedure Do_Overlap
     (A_Interval : IntervalT;
      B_Interval : IntervalT;
      Returns    : out OverlapType;
      Overlaps   : out IntervalT);

   ----------------
   -- Do_Overlap --
   ----------------

   procedure Do_Overlap
     (A_Interval : IntervalT;
      B_Interval : IntervalT;
      Returns    : out OverlapType;
      Overlaps   : out IntervalT)
   is
   begin
      if A_Interval.upper_bound < B_Interval.lower_bound
        or else A_Interval.lower_bound > B_Interval.upper_bound
      then
         Returns := OTNoOverlap;
         Overlaps.lower_bound :=
           TimeT'Min (A_Interval.upper_bound, B_Interval.upper_bound);
         Overlaps.upper_bound :=
           TimeT'Max (A_Interval.lower_bound, B_Interval.lower_bound);
      elsif A_Interval.lower_bound <= B_Interval.lower_bound
        and then A_Interval.upper_bound >= B_Interval.upper_bound
      then
         Returns := OTContainer;
         Overlaps := B_Interval;
      elsif A_Interval.lower_bound >= B_Interval.lower_bound
        and then A_Interval.upper_bound <= B_Interval.upper_bound
      then
         Returns := OTContained;
         Overlaps := A_Interval;
      else
         Returns := OTOverlap;
         Overlaps.lower_bound :=
           TimeT'Max (A_Interval.lower_bound, B_Interval.lower_bound);
         Overlaps.upper_bound :=
           TimeT'Min (A_Interval.upper_bound, B_Interval.upper_bound);
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
      interval : CosTime.TIO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out OverlapType)
   is
      pragma Warnings (Off);
      A_Interval : IntervalT renames Self.Interval;
      B_Interval : constant IntervalT := Get_time_interval (interval);
      --  XXX is it necessary ?
      pragma Warnings (On);
      Result     : constant TIO_Ptr := new Object;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => Get_time_interval (interval),
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), overlap);
   end overlaps;

   -----------
   -- spans --
   -----------

   procedure spans
     (Self : access Object;
      time : CosTime.UTO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out OverlapType)
   is
      Tim        : constant TimeT       := UTO.Get_time (time);
      Ina        : constant InaccuracyT := UTO.Get_inaccuracy (time);
      B_Interval : constant IntervalT   := (lower_bound => Tim - Ina,
                                            upper_bound => Tim + Ina);
      Result     : constant TIO_Ptr := new Object;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => B_Interval,
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
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
        (Self.Interval.upper_bound - Self.Interval.lower_bound) / 2;
      Result.Inaccuracy := InaccuracyT
        (Self.Interval.upper_bound - Self.Interval.lower_bound);
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return R;
   end time;

end CosTime.TIO.Impl;
