with Types; use Types;

package Backend.BE_Ada is

   procedure Generate (E : Node_Id);
   procedure Configure;
   procedure Usage    (Indent : Natural);

   Print_Ada_Tree       : Boolean := False;
   Print_IDL_Tree       : Boolean := False;
   Impl_Packages_Gen    : Boolean := True;  -- temporary enabled
   Disable_Pkg_Impl_Gen : Boolean := False;
   Disable_Pkg_Spec_Gen : Boolean := False;
   Output_Unit_Withing  : Boolean := False;
   Output_Tree_Warnings : Boolean := False;

end Backend.BE_Ada;
