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

package Corba.Orb is


   --------------------------------------------------
   ---          specification CORBA 2.0          ----
   --------------------------------------------------


   -- string object_to_string (in Object obj);
   -- client-side
   function Object_To_String (Obj : in CORBA.Object.Ref'class)
                              return CORBA.String;


   -- Object string_to_object (in string str);
   -- client-side
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class);

   -- string object_to_string (in Object obj);
   -- server-side
   function Object_To_String (Obj : in CORBA.Object.Object'class)
                              return CORBA.String;



   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   function ORB_Init(Orb_Name : in Standard.String) return Object'Class ;
   -- initializes and returns the ORB with parameters of the command line


   function BOA_Init(Self : in Object'Class ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object'Class ;
   -- initializes and returns the BOA

private



end Corba.Orb ;
