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

   type Object_Access is access Object ;
   -- just to give a name to pointers on Object


   procedure C_Obj_Is_Ready (Self: in Object'Class ;
                             object: in System.Address ;
                             p: in System.Address) ;
   pragma Import (C,C_Obj_Is_Ready,"toto") ;
   -- wrapper around BOA function obj_is_ready
   -- (see CORBA.h)

   procedure Obj_Is_Ready(Self: in Object'Class ;
                          object: in Corba.Object.Ref'Class ;
                          p: in Omniobject.Implemented_Object) ;
   -- Ada equivalent of C procedure C_Obj_Is_Ready


   function C_Get_Boa (Self: in Object'Class)
                      return System.Address ;
   pragma Import (C,C_Get_Boa,"toto") ;
   -- wrapper around BOA function getBOA
   -- (see CORBA.h)

   function Get_Boa (Self: in Object'Class)
                     return Object'Class ;
   -- Ada equivalent to C function C_Get_Boa


   procedure C_Dispose (Self : in Object'Class ;
                        object : in System.Address) ;
   pragma Import (C,C_Dispose,"toto") ;
   -- wrapper around BOA function dispose
   -- (see CORBA.h)

   procedure Dispose(Self: in Object'Class ;
                     Obj : in Corba.Object.Ref'Class) ;
   -- Ada equivalent of C procedure C_Dispose


private

   function Init return Object'Class;
   pragma CPP_Constructor (Init);
   pragma Import (CPP,Init,"__Q25CORBA3BOA");
   -- wrapped around the C constructor of BOA

end Corba.Boa ;

