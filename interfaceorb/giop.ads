-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class GIOP         ----
----   declared in GIOP.h.                                         ----
----     It provides some Ada equivalents of C++ types and the     ----
----   corresponding translation fonctions.                        ----
----                                                               ----
----                  package giop                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/28/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.C ;


package Giop is

   type Reply_Status_Type is (NO_EXCEPTION, USER_EXCEPTION,
                              SYSTEM_EXCEPTION, LOCATION_FORWARD);
   --  corresponds to enum ReplyStatusType { NO_EXCEPTION, USER_EXCEPTION,
   --                                      SYSTEM_EXCEPTION, LOCATION_FORWARD }
   -- in GIOP.h L 81

   function Reply_Status_Type_To_C_Int (Status : in Reply_Status_Type)
                                        return Interfaces.C.Int ;
   -- transforms the Ada type Reply_Status_Type into a C int
   -- in order to make it compatible with the C definition
   -- of ReplyStatusType where each value is actually an int value

   function C_Int_To_Reply_Status_Type (N : in Interfaces.C.Int)
                                        return Reply_Status_Type ;
   -- transforms a C int into an Ada value of type Reply_Status_Type
   -- according to the C way of defining enumerations.
   -- It means 0 -> NO_EXCEPTION, 1 -> USER_EXCEPTION ...
   -- An Ada Exception C_Out_Of_Range is raised if the C value is incorrect.


private

end Giop ;



