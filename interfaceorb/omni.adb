-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----     This package is wrapped around the C class omni_C2Ada     ----
----   declared in omni_C2Ada.hh;                                  ----
----     It provides the 2 functions of omni_C2Ada and their       ----
----   equivalent in ADA.                                          ----
----                                                               ----
----                  package omni                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------

with Ada.Exceptions ;
with System.Address_To_Access_Conversions ;

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
