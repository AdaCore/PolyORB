-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Boa                            ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Corba.Boa is


   --------------------------------------------------
   ---        not specified in CORBA2.2          ----
   --------------------------------------------------

   procedure Init(Orb_Name : in String) ;
   -- wrapper around BOA_ptr CORBA::ORB::BOA_init(int& argc, char** argv,
   --                               const char* boa_identifier);
   -- in CORBA.h L 2092


   procedure Object_Is_Ready(The_Object : Corba.Object'Class) ;
   -- wrapper around void
   -- CORBA::
   --BOA::obj_is_ready(Object_ptr op, ImplementationDef_ptr ip /* ignored */)
   --{
   --  omniObject *obj = op->PR_getobj();
   --  omni::objectIsReady(obj);
   --  return;
   --} corbaBOA.cc L355
   -- which is useless
   -- so wrapper around void
   -- omni::objectIsReady(omniObject* obj)
   -- in objectRef.CC L 230
   --
   -- this function must also create the underlying C++ object
   -- since it has not been done yet

   procedure Impl_Is_Ready(The_Object : Corba.Object'Class) ;
   -- wrapper around impl_is_ready() ;


  private



end Corba.Boa ;
