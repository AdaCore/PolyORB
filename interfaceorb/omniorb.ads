-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniORB                              ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with System ;
with Corba ;

package Omniorb is


   type ObjectKey is private ;
   -- corresponds to omniORB::objectKey
   -- wrapped ??
   -- functions rewritten ??

   fatalException : exception ;

   type fatalException_Members is new Corba.System_Exception_Members with
     record
        Pd_File : Corba.String;
        Pd_Line : Corba.Long;
        Pd_Errmsg : Corba.String;
     end record;
   -- corresponds to class fatalException
   -- in omniORB.h L 470

private

   type ObjectKey is new System.Address ;

end Omniorb ;
