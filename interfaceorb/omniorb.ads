-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniORB                              ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package Omniorb is


   type ObjectKey is new ??? ;
   -- corresponds to omniORB::objectKey

   fatalException : exception;
   type fatalException_Members is new System_Exception_Members with
     record
        Pd_File : CORBA.String;
        Pd_Line CROBA.Int;
        Pd_Errmsg CORBA.String;
     end record;
   -- corresponds to class fatalException
   -- in omniORB.h L 470

private


end Omniorb ;
