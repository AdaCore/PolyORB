-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package boa                                  ----
----                                                               ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba.Object ;

package Boa is

   type Object is limited private ;

   procedure Obj_Is_Ready(Self : in out Object ;
                          Obj : in Corba.Object.Ref'Class) ;
   -- wrapper around CORBA::BOA::obj_is_ready(Object_ptr,
   --                                         ImplementationDef_ptr p=0)
   -- in CORBA.h L2004


   function GetBoa(Obj: in Corba.Object.Ref'Class) return Object ;
   -- returns the Ada Boa.Object

   procedure Dispose(Self: in out Object ;
                     Obj : in Corba.Object.Ref'Class) ;
   -- wrapper around CORBA::BOA::dispose
   -- in CORBA.h L 1964

private

   type Object is limited null record ;
   -- record containing a pointer to the omniORB BOA


end Boa ;






