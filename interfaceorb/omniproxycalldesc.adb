-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omniProxyCallDesc                    ----
----                                                               ----
----      This is a root class. For each subprogram of an IDL      ----
----    interface, a descendant of this class has to be provided.  ----
----    It contains al the information to make the remote call :   ----
----    arguments, results, exceptions, and how to send them on/   ----
----    reveive them from a giop.                                  ----
----                                                               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;

package body omniProxyCallDesc is


   -- Init
   -------
   procedure Init (Self : in out Object ;
                   Has_Exceptions : Corba.Boolean := False ) is
   begin
      Self.Pd_Has_User_Exception := Has_Exceptions ;
   end ;


   -- User_Exception
   -----------------
   procedure User_Exception (Self : in Object ;
                             Giop_Client : in Giop_C.Object ;
                             RepoId : in CORBA.String) is
   begin
      if Self.Pd_Has_User_Exception then
         Ada.Exceptions.Raise_Exception(Corba.Adabroker_Fatal_Error'Identity,
                                        "Omniproxycalldesc.User_Exception"
                                        & Corba.CRLF
                                        & "Should not be called for a calldesc that can throw exceptions"
                                        & "This procedure should have been overloaded") ;
      else
         -- nothing to be done since we do not throw any exception
         null ;
      end if ;
   end ;


   -- Has_User_Exceptions
   ----------------------
   function Has_User_Exceptions (Self : in Object)
                                 return CORBA.Boolean is
   begin
      return Self.Pd_Has_User_Exception ;
   end ;


end omniproxyCallDesc ;

