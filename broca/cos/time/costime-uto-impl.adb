----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with TimeBase; use TimeBase;
with CosTime.UTO.Skel;
with CosTime.UTO.Helper;
with CosTime.TIO.Impl;
with CosTime.TIO.Helper;
with Interfaces.C;
with System;
with Time_Utils; use Time_Utils;
with Broca.Basic_Startup;
with PortableServer;

package body CosTime.UTO.Impl is

   type UTO_Ptr is access Object;
   type TIO_Ptr is access CosTime.TIO.Impl.Object;

   function get_time
     (Self : access Object)
     return TimeT
   is
   begin
      return Self.Time;
   end get_time;

   function get_inaccuracy
     (Self : access Object)
     return InaccuracyT
   is
   begin
      return Self.Inaccuracy;
   end get_inaccuracy;

   function get_tdf
     (Self : access Object)
     return TdfT
   is
   begin
      return Self.Tdf;
   end get_tdf;

   function get_utc_time
     (Self : access Object)
     return UtcT
   is
   begin
      return (time    => Self.Time,
              inacclo => CORBA.Unsigned_Long (Self.Inaccuracy rem (2 ** 32)),
              inacchi => CORBA.Unsigned_Short (Self.Inaccuracy / (2 ** 32)),
              tdf     => Self.Tdf);
   end get_utc_time;

   function absolute_time
     (Self : access Object)
     return Ref
   is
      Result : constant UTO_Ptr := new Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Time := Self.Time + Current_Time;
      Result.Inaccuracy := Self.Inaccuracy + Current_Inaccuracy;
      Result.Tdf := Self.Tdf + Current_Tdf;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return Helper.To_Ref (R);
   end absolute_time;

   function compare_time
     (Self : access Object;
      comparison_type : in ComparisonType;
      uto : in Ref)
     return TimeComparison
   is
      Other_Time : constant TimeT := get_time (uto);
      Other_Tdf  : constant TdfT  := get_tdf (uto);
   begin
      if Comparison_Type = MidC then
         return Compare (Self.Time + Self.Tdf, Other_Time + Other_Tdf);
      else
         declare
            Other_Inaccuracy : constant InaccuracyT := get_inaccuracy (uto);
            Comp_Low         : constant TimeComparison :=
              Compare (Self.Time - Self.Inaccuracy + Self.Tdf,
                       Other_Time - Other_Inaccuracy + Other_Tdf);
            Comp_High        : constant TimeComparison :=
              Compare (Self.Time + Self.Inaccuracy + Self.Tdf,
                       Other_Time + Other_Inaccuracy + Other_Tdf);
         begin
            if Comp_Low = Comp_High then
               return Comp_Low;
            else
               return TCIndeterminate;
            end if;
         end;
      end if;
   end compare_time;

   function time_to_interval
     (Self : access Object;
      uto : in Ref)
     return TIO_Forward.Ref
   is
      Other_Time : constant TimeT   := get_time (uto);
      Result     : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R          : CORBA.Object.Ref;
   begin
      Result.Interval.Lower_Bound := TimeT'Min (Self.Time, Other_Time);
      Result.Interval.Upper_Bound := TimeT'Max (Self.Time, Other_Time);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return CosTime.TIO.Convert_Forward.To_Forward
        (CosTime.TIO.Helper.To_Ref (R));
   end time_to_interval;

   function interval
     (Self : access Object)
     return TIO_Forward.Ref
   is
      Result : constant TIO_Ptr := new CosTime.TIO.Impl.Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Interval.Lower_Bound := Self.Time - Self.Tdf;
      Result.Interval.Upper_Bound := Self.Time + Self.Tdf;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return CosTime.TIO.Convert_Forward.To_Forward
        (CosTime.TIO.Helper.To_Ref (R));
   end interval;

end CosTime.UTO.Impl;
