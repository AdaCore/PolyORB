----------------------------------------------------------------------------
----                                                                    ----
----                package proxyobjectfactory                          ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------

-- This class is the Ada equivalent of omniORB's proxyObjectFactory
-- it is the root of all proxyObjectFactories.
-- The aim of such objects is to create new instance
-- of proxy objects




with Corba, Corba.Object ;
with Iop ;
with Rope ;

package Proxyobjectfactory is

   type Object is abstract tagged private ;

   procedure Init(Self : in Object) ;
   --its only gaol is to call the C++ constructor

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

   type Object is abstract tagged null record ;

end Proxyobjectfactory ;
