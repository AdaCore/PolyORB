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

with Corba, Rope ;

package OmniRopeAndKey is

   type Object is limited private ;

   procedure Init (Self : in out Object ;
                     R : in Rope.Object ;
                     K : in CORBA.Octet ;
                     Ksize : in CORBA.Unsigned_Long);
   -- wrapper around inline omniRopeAndKey(Rope *r,
   --                              _CORBA_Octet *k, _CORBA_ULong ksize)
   -- in omniInternal.h L 234

   function Key (Self : in Object) return CORBA.Octet;
   -- wrapper around inline _CORBA_Octet* key()
   -- in omniInternal.h L 250

   function Rope (Self : in Object) return Rope.Object;
   -- wrapper around   inline Rope* rope() const { return pd_r; }
   -- in omniInternal.h L 248

   function Key_Size (Self : in Object) return CORBA.Unsigned_Long ;
   -- wrapper around inline _CORBA_ULong  keysize() const { return pd_keysize; }
   -- in omniInternal.h L 259

private

   type Object is null record ;

end OmniRopeAndKey ;
