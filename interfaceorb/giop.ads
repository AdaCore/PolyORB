-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package giop                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Giop is

   type ReplyStatusType is (NO_EXCEPTION, USER_EXCEPTION,
                            SYSTEM_EXCEPTION, LOCATION_FORWARD);
   --  corresponds to enum ReplyStatusType { NO_EXCEPTION, USER_EXCEPTION,
   --                                      SYSTEM_EXCEPTION, LOCATION_FORWARD }
   -- in GIOP.h L 81

private

end Giop ;

