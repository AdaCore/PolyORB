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

   function Is_A(Base_RepoID : in String) return Corba.Boolean  ;

   function Nil return Corba.Object.Ref'Class  ;

private

   type Object is abstract tagged null record ;

end Proxyobjectfactory ;
