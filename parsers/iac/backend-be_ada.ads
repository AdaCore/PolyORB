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
   Customer_K_To_V                    : Float   := 0.0;

   --  In some particular cases, some parts of the IDL tree must not be
   --  generated. The entities below achieve this goal
   type Package_Type is
     (PK_Stub_Spec,
      PK_Stub_Body,
      PK_Helper_Spec,
      PK_Helper_Body,
      PK_Skel_Spec,
      PK_Skel_Body,
      PK_Impl_Spec,
      PK_Impl_Body);

   function Map_Particular_CORBA_Parts
     (E  : Node_Id;
      PK : Package_Type)
     return Boolean;

end Backend.BE_Ada;
