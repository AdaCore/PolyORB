----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with CosTime.UTO;
with TimeBase;
with PortableServer;

package CosTime.TIO.Impl is

   type Object is new PortableServer.Servant_Base with record
      Interval : Timebase.IntervalT;
   end record;

   function get_time_interval
     (Self : access Object)
     return TimeBase.IntervalT;

   procedure spans
     (Self : access Object;
      time : in UTO.Ref;
      overlap : out Ref;
      Returns : out OverlapType);

   procedure overlaps
     (Self : access Object;
      interval : in Ref;
      overlap : out Ref;
      Returns : out OverlapType);

   function time
     (Self : access Object)
     return UTO.Ref;

end CosTime.TIO.Impl;
