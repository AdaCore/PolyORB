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

with Interfaces.CPP ;
with Interfaces.C.Strings ;
with System ;
with OmniObject ;
with Corba ;

package Omni is


   type Alignment_T is new Integer;
   ALIGN_1 : constant Alignment_T := 1;
   ALIGN_2 : constant Alignment_T := 2;
   ALIGN_4 : constant Alignment_T := 4;
   ALIGN_8 : constant Alignment_T := 8;
   -- corresponds to enum alignment_t { ALIGN_1 = 1, ALIGN_2 = 2, ALIGN_4 = 4, ALIGN_8 = 8 };
   -- In Internal.h L 162


   function Align_To(P: in Corba.Unsigned_Long ;
                     Align : in Alignment_T)
                     return Corba.Unsigned_Long ;
   -- wrapper around   static inline ptr_arith_t align_to(
   --                     ptr_arith_t p,
   --                     alignment_t align)
   -- In Internal.h L 166

--private



end Omni ;
