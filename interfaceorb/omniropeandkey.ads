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
with Interfaces.CPP ;

package OmniRopeAndKey is

--   type Object is tagged private ;

   type Object is tagged record
      Pd_R : Rope.Object_Access ;
      Pd_KeySize : Corba.Unsigned_Long ;
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record;

   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;

   procedure Init (This : in out Object ;
                     R : in Rope.Object ;
                     K : in CORBA.Octet ;
                     Ksize : in CORBA.Unsigned_Long);
   -- wrapper around inline omniRopeAndKey(Rope *r,
   --                              _CORBA_Octet *k, _CORBA_ULong ksize)
   -- in omniInternal.h L 234

   function Get_Key (This : in Object) return CORBA.Octet;
   -- wrapper around inline _CORBA_Octet* key()
   -- in omniInternal.h L 250

   function Get_Rope (This : in Object) return Rope.Object;
   -- wrapper around   inline Rope* rope() const { return pd_r; }
   -- in omniInternal.h L 248

   function Key_Size (This : in Object) return CORBA.Unsigned_Long ;
   -- wrapper around inline _CORBA_ULong  keysize() const { return pd_keysize; }
   -- in omniInternal.h L 259


private


end OmniRopeAndKey ;
