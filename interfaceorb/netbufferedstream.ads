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

   type Object is new BufferedStream.Object with null record;

   function IsReusingExistingConnection (Self : in Object) return CORBA::Boolean;
   -- wrapper around     _CORBA_Boolean isReUsingExistingConnection() const;
   -- de la classe Sync
   -- in rope.h L 395

   function Align_And_Put_Bytes (...) return ...;
   -- wrapper around inline void* align_and_put_bytes(omni::alignment_t align,
   --                                      size_t nbytes,
   --                                      _CORBA_Boolean startMTU=0,
   --                                      _CORBA_Boolean at_most_once=0)
   -- in bufferedStream.h L 406

private



end NetBufferedStream ;

