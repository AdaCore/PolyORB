----------------------------------------------------------------------------
----                                                                    ----
----                  AdaBroker                                         ----
----                                                                    ----
----      This class is wrapped around the C class proxyObjectFactory   ----
----   (see proxyFactory.h).                                            ----
----      It provides the same functions as the C class and it is the   ----
----   root of all proxyObjectFactories.                                ----
----      The aim of such objects is to create new instance of proxy    ----
----   objects.                                                         ----
----                                                                    ----
----                                                                    ----
----                package proxyobjectfactory                          ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

with Interfaces.CPP ;
with Interfaces.C ;
with System ;
--with Corba, Corba.Object ;
--with Iop ;
--with Rope ;

package Proxyobjectfactory is

   type Object is abstract tagged record  ;
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;

   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object is wrapped around proxyObjectFactory
   -- (see proxyFactory.hh)

   type Object_Ptr is access all Object ;
   -- just to give a name to pointers on Object


   function C_New_Proxy_Object (Self : in Object'Class ;
                                R : in System.Address ;
                                Key : in Interfaces.C.Unsigned_Long ;
                                Profiles : in System.Address ;
                                Release : in Sysdep.C_Booelan)
                                return
   pragma Import (C,C_New_Proxy_Object,"") ;
   -- wrapper around
   -- (see omniInternal.h L 514)


   function New_Proxy_Object(Self : in Object ;
                             R : in Rope.Object ;
                             Key : in Corba.Octet ;
                             Profiles : in Iop.Tagged_Profile_List ;
                             Release : in Corba.Boolean)
                             return Corba.Object.Ref ;
   -- called by the C++ code
   -- The Name in C++ is : newProxyObject
   -- !!!! appelle moi pour le corba.object.Ref !!!!!

   function Is_A(Base_RepoID : in String) return Corba.Boolean  ;
   -- called by the ORB, exactly like Dispatch
   -- the name of the C++ function is is_a

   function Repository_Id return Corba.String is abstract ;
   -- called by the ORB, exactly like Dispatch
   -- The Name of The C++ function is irRepoId

   function Nil return Corba.Object.Ref'Class  ;



private

   function Constructor return Object'Class;
   pragma CPP_Constructor (Constructor);
   pragma Import (CPP,Constructor,"__18proxyObjectFactory");
   -- wrapped around the C constructor of Rope

end Proxyobjectfactory ;


