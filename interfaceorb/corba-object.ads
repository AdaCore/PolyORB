-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Object                         ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------


package Corba.Object is

   type Ref is tagged private;

   --I void release();
   procedure Release (Self : in out Ref'class);
   -- Not implemented in omniORB
   -- neither here

   --I Object duplicate();
   -- use assignment


   --------------------------------------------------
   ---        omniORB specific                    ---
   --------------------------------------------------

   procedure PR_Setobj(  ) ;
   -- wrapper around void CORBA::Object::PR_setobj(omniObject *obj)
   -- in corbaObject.cc L121

   function PR_Getobj(  ) return ;
   -- wrapper around omniObject* CORBA::Object::PR_getobj()
   -- in corbaObject.cc L128



private



end Corba.Object ;
