----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with CosTime.TIO.Skel;
with CosTime.TIO.Helper;
with CosTime.UTO.Impl;
with CosTime.UTO.Helper;
with PortableServer;
with Broca.Basic_Startup;
with Time_Utils; use Time_Utils;

package body CosTime.TIO.Impl is

   use TimeBase;

   type TIO_Ptr is access Object;
   type UTO_Ptr is access UTO.Impl.Object;

   procedure Do_Overlap
     (A_Interval : in IntervalT;
      B_Interval : in IntervalT;
      Returns    : out OverlapType;
      Overlaps   : out IntervalT);

   function get_time_interval
     (Self : access Object)
     return IntervalT
   is
   begin
      return Self.Interval;
   end get_time_interval;

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
      R          : CORBA.Object.Ref;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => B_Interval,
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      overlap := Helper.To_Ref (R);
   end spans;

   procedure overlaps
     (Self : access Object;
      interval : in CosTime.TIO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out OverlapType)
   is
      A_Interval : IntervalT renames Self.Interval;
      B_Interval : constant IntervalT := get_time_interval (interval);
      Result     : constant TIO_Ptr := new Object;
      R          : CORBA.Object.Ref;
   begin
      Do_Overlap (A_Interval => Self.Interval,
                  B_Interval => get_time_interval (interval),
                  Overlaps   => Result.Interval,
                  Returns    => Returns);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      overlap := Helper.To_Ref (R);
   end overlaps;

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

   function time
     (Self : access Object)
     return UTO.Ref
   is
      Result : constant UTO_Ptr := new UTO.Impl.Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Time :=
        (Self.Interval.Upper_Bound - Self.Interval.Lower_Bound) / 2;
      Result.Inaccuracy := InaccuracyT
        (Self.Interval.Upper_Bound - Self.Interval.Lower_Bound);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return UTO.Helper.To_Ref (R);
   end time;

end CosTime.TIO.Impl;
