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

with Interfaces.CPP ;
with Corba.Object ;
with Corba.Boa ;
with Omniobject ;

package Corba.Orb is


   type Object is tagged record
      Table : Interfaces.CPP.Vtable_Ptr ;
   end record ;
   pragma CPP_Class (Object);
   pragma CPP_Vtable (Object,Table,1);
   -- This object corresponds to CORBA::ORB

   type Object_Ptr is access all Object'Class ;


   --------------------------------------------------
   ---          specification CORBA 2.0          ----
   --------------------------------------------------


   -- string object_to_string (in Object obj);
   -- client-side
   function Object_To_String (Obj : in CORBA.Object.Ref'class)
                              return CORBA.String
     renames Corba.Object.Object_To_String ;


   -- Object string_to_object (in string str);
   -- client-side
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class)
     renames Corba.Object.String_To_Object ;

   -- string object_to_string (in Object obj);
   -- server-side
   function Object_To_String (Obj : in Omniobject.Implemented_Object'class)
                              return CORBA.String
     renames Omniobject.Object_To_String ;



   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   function ORB_Init(Orb_Name : in Standard.String) return Object_Ptr ;
   -- initializes and returns the ORB with parameters of the command line


   function BOA_Init(Self : in Object'Class ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object_Ptr ;
   -- initializes and returns the BOA

private

   function Constructor return Object'Class;
   pragma Import (CPP,Constructor,"__Q25CORBA3ORB");
   pragma CPP_Constructor (Constructor);
   -- wrapped around the C constructor of CORBA::ORB


end Corba.Orb ;








