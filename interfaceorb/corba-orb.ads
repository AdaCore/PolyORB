-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package CORBA.Orb                            ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba.Object ;
with Corba.Boa ;
with Corba.Implementation_Defined ;

package Corba.Orb is


   --------------------------------------------------
   ---          specification CORBA 2.0          ----
   --------------------------------------------------


   -- string object_to_string (in Object obj);
   -- client-side
   function Object_To_String (Obj : CORBA.Object.Ref'class)
                              return CORBA.String;


   -- Object string_to_object (in string str);
   -- client-side
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class);

   -- string object_to_string (in Object obj);
   -- server-side
   function Object_To_String (Obj : in CORBA.Implementation_Defined.Object'class)
                              return CORBA.String;


   -- Object string_to_object (in string str);
   -- server-side
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Implementation_Defined.Object'class);



   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   procedure Init(Orb_Name : in Standard.String) ;
   -- wrapper around CORBA::ORB_init(int& argc, char** argv,
   --                               const char* orb_identifier);
   -- in CORBA.h L 2246


private



end Corba.Orb ;
