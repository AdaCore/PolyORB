-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around a C++ class whose name     ----
----   is BOA declared in file CORBA.h.                            ----
----     It provides two types of methods : the C functions        ----
----   of the BOA class and their equivalent in                    ----
----   Ada. (the first ones have a C_ prefix.)                     ----
----                                                               ----
----                                                               ----
----                  package boa                                  ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/24/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.CPP ;
with System ;
with Corba.Object ;
with Omniobject ;

package Corba.Boa is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around BOA (see CORBA.h)

   type Object_Ptr is access Object ;
   -- just to give a name to pointers on Object

   procedure Object_Is_Ready(Self: in Object'Class ;
                             Obj: in Omniobject.Implemented_Object'Class ) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an implemented object has been created
   -- to register it into the ORB
   -- (as a local object )
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT

   procedure Object_Is_Ready(Self: in Object'Class ;
                             Obj: in Corba.Object.Ref'Class ) ;
   -- calls the C++ function omni::objectIsReady
   -- has to be done when an implemented object has been created
   -- to register it into the ORB
   -- (as a proxy object )
   -- BEWARE : MUST BE CALLED ONLY ONCE FOR EACH OBJECT


   --function Get_Boa (Self: in Object'Class)
   --                  return Object'Class ;
   -- Ada equivalent to C function C_Get_Boa


   --procedure Dispose(Self: in Object'Class ;
   --                  Obj : in Corba.Object.Ref'Class) ;
   -- Ada equivalent of C procedure C_Dispose


private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__Q25CORBA3BOA");
   -- wrapped around the C constructor of BOA

end Corba.Boa ;

