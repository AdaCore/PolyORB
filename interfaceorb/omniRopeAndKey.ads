-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniRopeAndKey                       ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package OmniRopeAndKey is

   type Object is limited private ;

   procedure Init (Self : in out Object,
                     ...
                     );
   -- wrapper around inline omniRopeAndKey(Rope *r,
   --                              _CORBA_Octet *k, _CORBA_ULong ksize)
   -- in omniInternal.h L 234

   function Key (Self : in Object) return CORBA::Octet;
   -- wrapper around inline _CORBA_Octet* key()
   -- in omniInternal.h L 250

private



end OmniRopeAndKey ;
