----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CosTime.TIO;
with TimeBase;
with CosTime.UTO;
with PortableServer;

package CosTime.TimeService.Impl is

   type Object is
     new PortableServer.Servant_Base with null record;

   function universal_time
     (Self : access Object)
     return CosTime.UTO.Ref;

   function secure_universal_time
     (Self : access Object)
     return CosTime.UTO.Ref;

   function new_universal_time
     (Self : access Object;
      time : in TimeBase.TimeT;
      inaccuracy : in TimeBase.InaccuracyT;
      tdf : in TimeBase.TdfT)
     return CosTime.UTO.Ref;

   function uto_from_utc
     (Self : access Object;
      utc : in TimeBase.UtcT)
     return CosTime.UTO.Ref;

   function new_interval
     (Self : access Object;
      lower : in TimeBase.TimeT;
      upper : in TimeBase.TimeT)
     return CosTime.TIO.Ref;

end CosTime.TimeService.Impl;
