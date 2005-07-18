with Types; use Types;

package Backend.BE_Ada is

   procedure Generate (E : Node_Id);
   procedure Configure;
   procedure Usage    (Indent : Natural);

   Print_Ada_Tree       : Boolean := False;
   Print_IDL_Tree       : Boolean := False;
   Impl_Packages_Gen    : Boolean := False;
   Disable_Pkg_Impl_Gen : Boolean := False;
   Disable_Pkg_Spec_Gen : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   Output_Tree_Warnings : Boolean := False;

   --  The flags below are related to the code optimization performed by Iac :

   --  Skeleton optimization using minimale perfect hash functions instead of
   --  the big "if .. elsif .. elsif ..."

   Use_Minimal_Hash_Function : Boolean := False;
   Optimize_CPU              : Boolean := False;
   Optimize_Memory           : Boolean := False;

end Backend.BE_Ada;
