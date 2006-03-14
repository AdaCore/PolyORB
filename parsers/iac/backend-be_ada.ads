------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                       B A C K E N D . B E _ A D A                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

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
   Generate_Imported    : Boolean := False;

   --  The flags below are related to the code optimization performed by Iac :

   --  Skeleton optimization using minimale perfect hash functions instead of
   --  the big "if .. elsif .. elsif ..."

   Use_Minimal_Hash_Function : Boolean := False;
   Optimize_CPU              : Boolean := False;
   Optimize_Memory           : Boolean := False;

   --  The flag below is related to the request handling method (SSI or DII)
   --  By default, the DII is Used

   Use_SII : Boolean := False;

   --  In some particular cases, some parts of the IDL tree must not be
   --  generated. The entities below achieve this goal

   type Package_Type is
     (PK_CDR_Spec,
      PK_CDR_Body,
      PK_Helper_Spec,
      PK_Helper_Body,
      PK_Impl_Spec,
      PK_Impl_Body,
      PK_Skel_Spec,
      PK_Skel_Body,
      PK_Stub_Spec,
      PK_Stub_Body);

   function Map_Particular_CORBA_Parts
     (E  : Node_Id;
      PK : Package_Type)
     return Boolean;

end Backend.BE_Ada;
