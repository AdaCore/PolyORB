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
with Corba ;
use type Corba.Unsigned_Long ;

Package body Omni is


   -- Align_To
   -----------
   function Align_To(Size : in Corba.Unsigned_Long ;
                     Align : in Alignment_T)
                     return Corba.Unsigned_Long is
      Temp : Corba.Unsigned_Long ;
   begin
      Temp := Size mod Corba.Unsigned_Long (Align) ;
      return Size + Corba.Unsigned_Long (Align) - Size ;
   end ;


end Omni ;
