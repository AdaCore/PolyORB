
--  This package is wrapped around the C class omni_C2Ada declared in
--  omni_C2Ada.hh; It provides the 2 functions of omni_C2Ada and their
--  equivalent in ADA.

with CORBA;

package AdaBroker.Omni is

   type Alignment_T is new Integer;
   ALIGN_1 : constant Alignment_T := 1;
   ALIGN_2 : constant Alignment_T := 2;
   ALIGN_4 : constant Alignment_T := 4;
   ALIGN_8 : constant Alignment_T := 8;
   --  Several kind of alignements for a list of Bytes

   function Align_To
     (Size  : in CORBA.Unsigned_Long;
      Align : in Alignment_T)
      return CORBA.Unsigned_Long;
   --  This function increases size in order to align it with the
   --  alignement given in parameter.

end AdaBroker.Omni;
