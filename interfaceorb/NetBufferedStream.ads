-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package NetBufferedStream                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package NetBufferedStream is

   type Object is limited private ;

   function IsReusingExistingConnection (Self : in Object) return CORBA::Boolean;
   -- wrapper around     _CORBA_Boolean isReUsingExistingConnection() const;
   -- de la classe Sync
   -- in rope.h L 395


private



end NetBufferedStream ;

