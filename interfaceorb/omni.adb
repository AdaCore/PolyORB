-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package body omni                            ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;

Package body Omni is


   -- Align_To
   -----------
   function Align_To(P: in Corba.Unsigned_Long ;
                     Align : in Alignment_T)
                     return Corba.Unsigned_Long is
   begin
      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Not_Implemented_Yet'Identity,
                                     "Omni.Align_To") ;
      return Corba.Unsigned_Long(0) ;
   end ;


end Omni ;
