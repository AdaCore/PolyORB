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

with Corba.Command_Line ;
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
      return To_Corba_String("") ;
   end ;


   -- String_To_Object
   -------------------
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) is
   begin
      null ;
   end ;


   -- Object_To_String
   -------------------
   function Object_To_String (Obj : in CORBA.Object.Object'class)
                              return CORBA.String is
   begin
      return To_Corba_String("") ;
   end ;



   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   function C_ORB_Init( Argc : in Interfaces.C.Int ;
                        Argv : in System.Address ;
                        Orbname : in Interfaces.C.Chars_Ptr)
                   return Object'Class ;
   pragma Import(C, C_ORB_Init, "ORB_init__5CORBARiPPcPCc") ;
   -- wrapper around CORBA::ORB_init(int& argc, char** argv,
   --                               const char* orb_identifier);
   -- in corbaOrb.cc L 170


   -- ORB_Init
   -----------
   function ORB_Init(Orb_Name : in Standard.String) return Object is
      C_Orb_Name : Interfaces.C.Chars_Ptr ;
   begin

      C_Orb_Name := Interfaces.C.Strings.New_String(Orb_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      return C_Orb_Init(Corba.Command_Line.Argc,
                        Corba.Command_Line.Argv,
                        C_Orb_Name) ;
   end ;


   -- C_BOA_Init
   -------------
   function C_BOA_Init(Self : in Object'Class ;
                       Argc : in Interfaces.C.Int ;
                       Argv : in System.Address ;
                       Boaname : in Interfaces.C.Chars_Ptr)
                   return Corba.Boa.Object'Class ;
   pragma Import(C,C_BOA_Init,"") ;
   -- corresponds to
   -- CORBA::BOA_ptr
   -- CORBA::
   -- ORB::BOA_init(int &argc, char **argv, const char *boa_identifier)
   -- corbaBoa.cc L 180

   -- BOA_Init
   -----------
   function BOA_Init(Self : in Object'Class ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object'Class is
       C_Boa_Name : Interfaces.C.Chars_Ptr ;
   begin

      C_Boa_Name := Interfaces.C.Strings.New_String(Boa_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      return C_Boa_Init(Self,
                        Corba.Command_Line.Argc,
                        Corba.Command_Line.Argv,
                        C_Orb_Name) ;
   end ;




end Corba.Orb ;
