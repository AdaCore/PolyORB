----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosTime.TIO.Impl;
with TimeBase; use Timebase;
with Costime.TIO.Helper;
with CosTime.UTO.Impl;
with Costime.UTO.Helper;
with CosTime.TimeService.Skel;
with Time_Utils; use Time_Utils;
with Broca.Basic_Startup;
with PortableServer;

package body CosTime.TimeService.Impl is

   type UTO_Ptr is access UTO.Impl.Object;
   type TIO_Ptr is access TIO.Impl.Object;

   function universal_time
     (Self : access Object)
     return CosTime.UTO.Ref
   is
   begin
      return new_universal_time
        (Self       => Self,
         time       => Current_Time,
         inaccuracy => Current_Inaccuracy,
         tdf        => Current_Tdf);
   end universal_time;


   function secure_universal_time
     (Self : access Object)
     return CosTime.UTO.Ref
   is
   begin
      raise TimeUnavailable;
      return universal_time (Self);
   end secure_universal_time;


   function new_universal_time
     (Self : access Object;
      time : in TimeBase.TimeT;
      inaccuracy : in TimeBase.InaccuracyT;
      tdf : in TimeBase.TdfT)
     return CosTime.UTO.Ref
   is
      Result : constant UTO_Ptr := new UTO.Impl.Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Time := time;
      Result.Inaccuracy := inaccuracy;
      Result.Tdf := tdf;
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return UTO.Helper.To_Ref (R);
   end new_universal_time;


   function uto_from_utc
     (Self : access Object;
      utc : in TimeBase.UtcT)
     return CosTime.UTO.Ref
   is
      use CORBA;
   begin
      return new_universal_time
        (Self       => Self,
         time       => utc.time,
         inaccuracy => InaccuracyT (utc.inacchi * 2 ** 32) +
                       InaccuracyT (utc.inacclo),
         tdf        => utc.tdf);
   end uto_from_utc;


   function new_interval
     (Self : access Object;
      lower : in TimeBase.TimeT;
      upper : in TimeBase.TimeT)
     return CosTime.TIO.Ref
   is
      Result : constant TIO_Ptr := new TIO.Impl.Object;
      R      : CORBA.Object.Ref;
   begin
      Result.Interval := (lower_bound => lower, upper_bound => upper);
      Broca.Basic_Startup.Initiate_Servant
        (PortableServer.Servant (Result), R);
      return TIO.Helper.To_Ref (R);
   end new_interval;

end CosTime.TimeService.Impl;
