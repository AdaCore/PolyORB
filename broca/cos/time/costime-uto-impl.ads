----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with Ada.Calendar;
with TimeBase;
with PortableServer;

package CosTime.UTO.Impl is

   type Object is new PortableServer.Servant_Base with record
      Time       : TimeBase.TimeT := 0;
      Inaccuracy : TimeBase.InaccuracyT := 0;
      Tdf        : Timebase.TdfT := 0;
   end record;

   function get_time
     (Self : access Object)
     return TimeBase.TimeT;

   function get_inaccuracy
     (Self : access Object)
     return TimeBase.InaccuracyT;

   function get_tdf
     (Self : access Object)
     return TimeBase.TdfT;

   function get_utc_time
     (Self : access Object)
     return TimeBase.UtcT;

   function absolute_time
     (Self : access Object)
     return Ref;

   function compare_time
     (Self : access Object;
      comparison_type : in ComparisonType;
      uto : in Ref)
     return TimeComparison;

   function time_to_interval
     (Self : access Object;
      uto : in Ref)
     return TIO_Forward.Ref;

   function interval
     (Self : access Object)
     return TIO_Forward.Ref;

end CosTime.UTO.Impl;
