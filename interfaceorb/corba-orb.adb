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

with System ;
with System.Address_To_Access_Conversions ;
with Ada.Exceptions ;
with Ada.Unchecked_Conversion ;
with Interfaces.C.Strings ;
with Corba.Command_Line ;
with Omniobject ;
use type Omniobject.Object_Ptr ;

with Corba.Object ; use Corba.Object ;
with Corba.Dynamic_Type ;

package body Corba.Orb is


   --------------------------------------------------
   ---         specification CORBA 2.0           ----
   --------------------------------------------------

   -- String_To_Object
   -------------------
   procedure String_to_Object (From : in CORBA.String;
                               To : out CORBA.Object.Ref'class) is
      RepoId : Corba.String ;
      Tmp_Omniobj : Omniobject.Object_Ptr ;
      Tmp_Dyn_Type : Corba.Object.Constant_Ref_Ptr ;
   begin
      -- Get the omniobject
      Tmp_Omniobj := Omniobject.String_To_Object(From) ;

      -- if the result is correct
      if not (Tmp_Omniobj = null) then

         -- check if the omniobject we got can be put into
         -- To (type implied the repoId)
         RepoId := Omniobject.Get_Repository_Id(Tmp_Omniobj.all) ;

         if Is_A(To, RepoId) then
            Tmp_Dyn_Type :=
              Corba.Dynamic_Type.Get_Dynamic_Type_From_Repository_Id(From) ;
            Set_Fields(To,Tmp_Omniobj, Tmp_Dyn_Type) ;
            return ;
         end if ;
      end if ;

      -- otherwise, the operation is illegal return Nil_Ref
      -- in the right class
      Set_Fields(To, null, null) ;

   end ;




   --------------------------------------------------
   ---             omniORB2 specific             ----
   --------------------------------------------------

   function C_ORB_Init( Argc : in Interfaces.C.Int ;
                        Argv : in System.Address ;
                        Orbname : in Interfaces.C.Strings.Chars_Ptr)
                       return System.Address ;
   pragma Import(CPP, C_ORB_Init, "ORB_init__5CORBARiPPcPCc") ;
   -- wrapper around CORBA::ORB_init(int& argc, char** argv,
   --                               const char* orb_identifier);
   -- in corbaOrb.cc L 170


   -- ORB_Init
   -----------
   function ORB_Init(Orb_Name : in Standard.String) return Object_Ptr is
      C_Orb_Name : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      package A2a is new System.Address_To_Access_Conversions(Object) ;
      function Conv is new Ada.Unchecked_Conversion(A2a.Object_Pointer, Object_Ptr);
   begin
      C_Orb_Name := Interfaces.C.Strings.New_String(Orb_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      C_Result :=  C_Orb_Init(Corba.Command_Line.Argc,
                              Corba.Command_Line.Argv,
                              C_Orb_Name) ;
      return Conv(A2a.To_Pointer(C_Result)) ;
   end ;


   -- C_BOA_Init
   -------------
   function C_BOA_Init(Self : in Object'Class ;
                       Argc : in Interfaces.C.Int ;
                       Argv : in System.Address ;
                       Boaname : in Interfaces.C.Strings.Chars_Ptr)
                   return System.Address ;
   pragma Import(CPP,C_BOA_Init,"BOA_init__Q25CORBA3ORBRiPPcPCc") ;
   -- corresponds to
   -- CORBA::BOA_ptr
   -- CORBA::
   -- ORB::BOA_init(int &argc, char **argv, const char *boa_identifier)
   -- corbaBoa.cc L 180

   -- BOA_Init
   -----------
   function BOA_Init(Self : in Object_Ptr ;
                     Boa_Name : in Standard.String)
                     return Corba.Boa.Object_Ptr is
      C_Boa_Name : Interfaces.C.Strings.Chars_Ptr ;
      C_Result : System.Address ;
      package A2a is new System.Address_To_Access_Conversions(Corba.Boa.Object) ;
      function Conv is new Ada.Unchecked_Conversion(A2a.Object_Pointer,
                                                    Corba.Boa.Object_Ptr);
   begin

      C_Boa_Name := Interfaces.C.Strings.New_String(Boa_Name) ;
      -- Never deallocated, but it may be used by the ORB
      -- and this function is called only once

      C_Result :=  C_Boa_Init(Self.all,
                        Corba.Command_Line.Argc,
                        Corba.Command_Line.Argv,
                        C_Boa_Name) ;
      return Conv(A2a.To_Pointer(C_Result)) ;
   end ;




end Corba.Orb ;
