-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body CORBA                           ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/10/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Corba.Exceptions ;

package body Corba is


   -----------------------------------------------------------
   ----           Exceptions in spec                       ---
   -----------------------------------------------------------

   -- GetMembers
   -------------
   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To : out Ex_Body) is
   begin
      Corba.Exceptions.Get_Members (From,To) ;
   end ;

   -- Raise_Corba_Exception
   ------------------------
   procedure Raise_Corba_Exception(Excp : in Ada.Exceptions.Exception_Id ;
                                   Excp_Memb: in Idl_Exception_Members_Ptr) is
   begin
      Corba.Exceptions.Raise_Corba_Exception(Excp, Excp_Memb) ;
   end ;


   -----------------------------------------------------------
   ----        not in spec, AdaBroker specific             ---
   -----------------------------------------------------------

   -- C_Raise_Ada_Exception
   ------------------------
   procedure C_Raise_Ada_Exception (Msg : in Interfaces.C.Strings.Chars_Ptr) is
      Ada_Msg : Standard.String := Interfaces.C.Strings.Value (Msg) ;
   begin
      -- argument already tranformed in a Ada type ...
      -- ... so calls the Ada procedure
      Raise_Ada_Exception (Ada_Msg) ;
   end ;


   -- Raise_Ada_Exception
   ----------------------
   procedure Raise_Ada_Exception (Msg : in Standard.String) is
   begin
      Ada.Exceptions.Raise_Exception (Corba.No_Initialisation_Error'Identity,Msg) ;
   end ;


   -- To_Corba_String
   ------------------
    function To_Corba_String(S: in Standard.String) return Corba.String is

    begin
       return Corba.String(Ada.Strings.Unbounded.To_Unbounded_String(S)) ;
    end ;


    -- To_Standard_String
    ---------------------
    function To_Standard_String(S: in Corba.String) return Standard.String is
    begin
       return Ada.Strings.Unbounded.To_String(Ada.Strings.Unbounded.Unbounded_String(S)) ;
    end;

    -- Length
    ---------
    function Length(Str : in Corba.String) return Corba.Unsigned_Long is
    begin
       return Corba.Unsigned_Long(Ada.Strings.Unbounded.
                                  Length(Ada.Strings.Unbounded.
                                         Unbounded_String(Str))) ;
    end ;



   -----------------------------------------------------------
   ----           not in spec  omniORB2 specific           ---
   -----------------------------------------------------------


    function Omni_CallTransientExceptionHandler return CORBA.Boolean is
    begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Omni_CallTransientExceptionHandler") ;
      return False ;
    end ;


    function Omni_CallCommFailureExceptionHandler return CORBA.Boolean is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Omni_CallCommFailureExceptionHandler") ;
       return False ;
    end ;


    function Omni_CallSystemExceptionHandler return CORBA.Boolean is
    begin
       Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                      "Omni_CallSystemExceptionHandler") ;
       return False ;
    end ;






end Corba ;






