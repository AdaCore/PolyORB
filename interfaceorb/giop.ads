-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class GIOP         ----
----   declared in GIOP.h.                                         ----
----     It provides some Ada equivalents of C++ types.            ----
----                                                               ----
----                  package giop                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Giop is

   type Reply_Status_Type is (NO_EXCEPTION, USER_EXCEPTION,
                              SYSTEM_EXCEPTION, LOCATION_FORWARD);
   --  corresponds to enum ReplyStatusType { NO_EXCEPTION, USER_EXCEPTION,
   --                                      SYSTEM_EXCEPTION, LOCATION_FORWARD }
   -- in GIOP.h L 81

private

end Giop ;

