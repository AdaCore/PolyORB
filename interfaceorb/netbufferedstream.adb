-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body NetBufferedStream               ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/22/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;

package body NetBufferedStream is

   function Is_Reusing_Existing_Connection (Self : in Object)
                                            return CORBA.Boolean is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "NetBufferedStream.Is_Reusing_Existing_Connection") ;
      return False ;
   end ;


end NetBufferedStream ;





