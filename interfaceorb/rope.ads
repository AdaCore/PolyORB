-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                                                               ----
----                                                               ----
----                                                               ----
----                                                               ----
----                                                               ----
----                  package rope                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
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
   -- This object is wrapped around Ada_OmniObject (see Ada_OmniObject.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object

--   function Null_Rope return Object ;

private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"");

   --   Null_Rope_Internal : Object ;

end Rope ;
