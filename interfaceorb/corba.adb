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



package body Corba is



   -----------------------------------------------------------
   ----           Exceptions in spec                       ---
   -----------------------------------------------------------

   -- GetMembers
   -------------
   procedure Get_Members (From : in Ada.Exceptions.Exception_Occurrence;
                          To : out System_Exception_Members) is
   begin
      raise Not_Implemented_Yet ;
   end ;


    -----------------------------------------------------------
    ----        not in spec, AdaBroker specific             ---
    -----------------------------------------------------------

   -- To_Corba_String
   ------------------
    function To_Corba_String(S: in Standard.String) return Corba.String is
    begin
      raise Not_Implemented_Yet ;
    end ;


    -- To_Standard_String
    ---------------------
    function To_Standard_String(S: in Corba.String) return Standard.String is
    begin
      raise Not_Implemented_Yet ;
    end;


   -----------------------------------------------------------
   ----           not in spec  omniORB2 specific           ---
   -----------------------------------------------------------


    function Omni_CallTransientExceptionHandler return CORBA.Boolean is
    begin
      raise Not_Implemented_Yet ;
    end ;


    function Omni_CallCommFailureExceptionHandler return CORBA.Boolean is
    begin
       raise Not_Implemented_Yet ;
    end ;


    function Omni_CallSystemExceptionHandler return CORBA.Boolean is
    begin
       raise Not_Implemented_Yet ;
    end ;






end Corba ;
