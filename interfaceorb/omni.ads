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

   -----------------------------------------------------------
   -- The functions object_to_string and string_to_object here
   -- are never used.
   -- I am leaving them in case I'm wrong !
   ----------------------------------------------------------

   function C_Object_To_String (Obj : in System.Address)
                                return Interfaces.C.Strings.Chars_Ptr;
   pragma Import (C,C_Object_To_String,"objectToString__10omni_C2AdaP14Ada_OmniObject") ;
   -- wrapper around omni_C2Ada function objectToString
   -- (see omni_C2Ada.hh)

   function Object_To_String (Obj : in OmniObject.Object)
                              return String ;
   -- Ada equivalent of C function C_Init


   function C_String_To_Object (str : in Interfaces.C.Strings.Chars_Ptr)
                                return System.Address ;
   pragma Import (C,C_String_To_Object,"stringToObject__10omni_C2AdaPCc") ;
   -- wrapper around omni_C2Ada function stringToObject
   -- (see omni_C2Ada.hh)

   function String_To_Object (Str : in String)
                              return OmniObject.Object ;
    -- Ada equivalent of C function C_Init


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
