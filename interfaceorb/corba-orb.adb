-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body CORBA.Orb                       ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    :                                                   ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

-- A Faire :

-- ecrire ici dexu fonctions C_Object_To_String et C_string_to_object
-- ce sont les fonctions du C++ que l'on importe

-- implementer les 3 fonctions declarees ici en calquant le code
-- d'omniORB, corbaOrb.cc L348

-- il faudra peut-etre avoir un object Nil_object dans omniobject
-- (je n'en sais rien)

with Omniobject ;

package body Corba.Orb is


   --------------------------------------------------
   ---          specification CORBA 2.0          ----
   --------------------------------------------------

   -- Object_To_String
   -------------------
   function Object_To_String (Obj : in CORBA.Object.Ref'class)
                              return CORBA.String is
   begin
   end ;


   -- String_To_Object
   -------------------
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) is
   begin

   end ;


   -- Object_To_String
   -------------------
   function Object_To_String (Obj : in CORBA.Object.Object'class)
                              return CORBA.String is
   begin

   end ;



   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   procedure Init(Orb_Name : in Standard.String) ;
   -- wrapper around CORBA::ORB_init(int& argc, char** argv,
   --                               const char* orb_identifier);
   -- in CORBA.h L 2246


private



end Corba.Orb ;
