-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C class rope           ----
----   declared in rope.h.                                         ----
----     It provides an empty rope type with a single function :   ----
----   Null_Rope which return a null rope.                         ----
----                                                               ----
----                                                               ----
----                  package rope                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/18/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.CPP ;

package Rope is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around Rope (see Rope.h)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object

private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__8Ada_Rope");
   -- wrapped around the C constructor of Rope

   Null_Rope : Object ;

end Rope ;
