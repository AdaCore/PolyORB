-----------------------------------------------------------------------
----                                                               ----
----                  AdaBroker                                    ----
----                                                               ----
----                  package omni                                 ----
----                                                               ----
----   authors : Sebastien Ponce, Fabien Azavant                   ----
----   date    : 02/08/99                                          ----
----                                                               ----
----                                                               ----
-----------------------------------------------------------------------



package Omni is


   type Alignment_T is new Integer;
   ALIGN_1 : constant Alignment_T := 1;
   ALIGN_2 : constant Alignment_T := 2;
   ALIGN_4 : constant Alignment_T := 4;
   ALIGN_8 : constant Alignment_T := 8;
   -- corresponds to enum alignment_t { ALIGN_1 = 1, ALIGN_2 = 2, ALIGN_4 = 4, ALIGN_8 = 8 };
   -- In Internal.h L 162


   -- function Align_To()  ;
   -- wrapper around   static inline ptr_arith_t align_to(
   --                     ptr_arith_t p,
   --                     alignment_t align)
   -- In Internal.h L 166

private



end Omni ;
