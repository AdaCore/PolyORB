-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C++ class              ----
----   Ada_OmniRopeAndKey declared in Ada_OmniRopeAndKey.          ----
----     It provides the same functions as this package plus       ----
----   the Ada version of thouse where arguments types are         ----
----   to be change.                                               ----
----     It includes a Init function since a Ada class has no      ----
----   constructor.                                                ----
----                                                               ----
----                                                               ----
----                  package omniRopeAndKey                       ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/18/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


with Interfaces.C ;
with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with Rope ;
with Corba ;

package OmniRopeAndKey is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record;

   pragma CPP_Class (Object) ;
   pragma CPP_Vtable (Object,Table,1) ;
   -- This object is wrapped around Ada_OmniRopeAndKey
   -- (see Ada_OmniRopeAndKey.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object


   procedure C_Init (Self : in out Object'Class ;
                     R : System.Address ;
                     K : System.Address ;
                     Ksize : Interfaces.C.Unsigned_Long) ;
   pragma Import (C,C_Init,"Init__18Ada_OmniRopeAndKeyP4RopePUcUl") ;
   -- wrapper around  Ada_OmniRopeAndKey function Init
   -- (see Ada_OmniRopeAndKey.hh)

   procedure Init (Self : in out Object'Class ;
                   R : in Rope.Object ;
                   K : in CORBA.Octet ;
                   Ksize : in CORBA.Unsigned_Long);
   -- Ada equivalent of C function C_Init


   procedure C_Init2 (Self : in out Object'Class) ;
   pragma Import (C,C_Init2,"Init__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function Init
   -- (see Ada_OmniRopeAndKey.hh)

   procedure Init (Self : in out Object'Class) ;
   -- Ada equivalent of C function C_Init2
   -- needed for avoiding a name conflict


   function C_Get_Rope (Self : in Object'Class) return System.Address ;
   pragma Import (C,C_Get_Rope,"rope__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function rope
   -- (see Ada_OmniRopeAndKey.hh)

   function Get_Rope (Self : in Object'Class) return Rope.Object;
   -- Ada equivalent of C function C_Get_Rope


   function C_Get_Key (Self : in Object'Class) return System.Address;
   pragma Import (C,C_Get_Key,"key__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function key
   -- (see Ada_OmniRopeAndKey.hh)

   function Get_Key (Self : in Object'Class) return CORBA.Octet;
   -- Ada equivalent of C function C_Get_Key


   function C_Key_Size (Self : in Object'Class) return Interfaces.C.Unsigned_Long ;
   pragma Import (C,C_Key_Size,"keysize__18Ada_OmniRopeAndKey") ;
   -- wrapper around  Ada_OmniRopeAndKey function keysize
   -- (see Ada_OmniRopeAndKey.hh)

   function Key_Size (Self : in Object'Class) return CORBA.Unsigned_Long ;
   -- Ada equivalent of C function C_Key_Size


   function Equals(Self : in Object'Class ;
                   Other : in Object'Class) return Boolean ;
   pragma Import(C, Equals, "???") ;
   -- redefinition of the operator to compare the C++ objects
   -- see Ada_OmniRopeAndKey.hh

   function "="(Self : in Object'Class ;
                Other : in Object'Class) return Boolean
   renames Equals ;

private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__14omniRopeAndKey");
   -- wrapped around the C constructor of Rope

end OmniRopeAndKey ;






