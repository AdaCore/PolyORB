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

package body CosTime.TIO.Impl is

   use TimeBase;

   type TIO_Ptr is access Object;
   type UTO_Ptr is access UTO.Impl.Object;

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
      Returns : out CORBA.Boolean) is
   begin
      --  Does nothing XXXXX FIXME
      null;
   end spans;

   procedure overlaps
     (Self : access Object;
      interval : in CosTime.TIO.Ref;
      overlap : out CosTime.TIO.Ref;
      Returns : out CORBA.Boolean)
   is
      A_Interval : IntervalT renames Self.Interval;
      B_Interval : constant IntervalT := get_time_interval (interval);
      Result     : constant TIO_Ptr := new TIO.Impl.Object;
      R          : CORBA.Object.Ref;
   begin
      if A_Interval.Upper_Bound < B_Interval.Lower_Bound then
         Returns := False;
         Result.Interval.Lower_Bound := A_Interval.Upper_Bound;
         Result.Interval.Upper_Bound := B_Interval.Lower_Bound;
      elsif B_Interval.Upper_Bound < A_Interval.Lower_Bound then
         Returns := False;
         Result.Interval.Lower_Bound := B_Interval.Upper_Bound;
         Result.Interval.Upper_Bound := A_Interval.Lower_Bound;
      else
         Returns := True;
         Result.Interval.Lower_Bound :=
           TimeT'Max (A_Interval.Lower_Bound, B_Interval.Lower_Bound);
         Result.Interval.Upper_Bound :=
           TimeT'Min (A_Interval.Upper_Bound, B_Interval.Upper_Bound);
      end if;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      overlap := Helper.To_Ref (R);
   end overlaps;

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
