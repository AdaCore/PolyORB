-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is the equivalent in Ada of the C            ----
----   package Ada_omniRopeAndKey.                                 ----
----     It provides the same functions as this package plus       ----
----   the Ada version of each one if the arguments types are      ----
----   different.                                                  ----
----     It includes a Init function since a Ada class has no      ----
----   constructor.                                                ----
----                                                               ----
----                                                               ----
----                  package body omniRopeAndKey                  ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/17/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Corba, Rope ;
with Interfaces.CPP ;

package OmniRopeAndKey is

   type Object is tagged record
      Pd_R : Rope.Object_Ptr;
      Pd_KeySize : Corba.Unsigned_Long ;
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record;

   type Object_Ptr is access all Object ;

   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"");

   procedure Init (This : in out Object'Class ;
                   R : in Rope.Object ;
                   K : in CORBA.Octet ;
                   Ksize : in CORBA.Unsigned_Long);
   -- wrapper around inline omniRopeAndKey(Rope *r,
   --                              _CORBA_Octet *k, _CORBA_ULong ksize)
   -- in omniInternal.h L 234

   function Get_Key (This : in Object'Class) return CORBA.Octet;
   -- wrapper around inline _CORBA_Octet* key()
   -- in omniInternal.h L 250

--   function Get_Rope (This : in Object'Class) return Rope.Object;
   -- corresponds to inline Rope* rope() const { return pd_r; }
   -- in omniInternal.h L 248

   function Key_Size (This : in Object'Class) return CORBA.Unsigned_Long ;
   -- corresponds to inline _CORBA_ULong  keysize() const { return pd_keysize; }
   -- in omniInternal.h L 259

end OmniRopeAndKey ;






