-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                                                               ----
----     This package is wrapped around the C class                ----
----   omniObjectManager declared in omniInternal.h.               ----
----     It provides the sames functions as the C class (ie        ----
----   one function : NilObjectManager).                           ----
----                                                               ----
----                                                               ----
----                  package omniObjectManager                    ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/18/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Interfaces.CPP ;
with System ;

package OmniObjectManager is

   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around OmniObjectManager (see omniInternal.h)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object

   function C_Nil_Object_Manager return System.Address ;
   pragma Import (C,C_Nil_Object_Manager,"nilObjectManager__10omniObject") ;
   -- wrapper around static omniObjectManager* nilObjectManager();
   -- (see omniInternal.h L 514)

   function Nil_Object_Manager return Object ;
   pragma CPP_Virtual(Nil_Object_Manager) ;
   -- Ada equivalent to C function C_Nil_Object_Manager

private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__17omniObjectManager");
   -- wrapped around the C constructor of OmniObjectManager

end OmniObjectManager ;

