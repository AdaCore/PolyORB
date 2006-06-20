------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                 B A C K E N D . B E _ C O R B A _ A D A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
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

package Backend.BE_CORBA_Ada is

   procedure Generate (E : Node_Id);
   procedure Usage    (Indent : Natural);

   -----------------------
   -- General use flags --
   -----------------------

   Impl_Packages_Gen       : Boolean := False;
   --  True when we generate implementation templates

   Disable_Pkg_Body_Gen    : Boolean := False;
   Disable_Pkg_Spec_Gen    : Boolean := False;
   --  We can generate only spec or only bodies

   Generate_Imported       : Boolean := False;
   --  Generate code for the imported IDL units

   Disable_Client_Code_Gen : Boolean := False;
   --  Control the client side code generation

   Disable_Server_Code_Gen : Boolean := False;
   --  Control the server side code generation

   ---------------------
   -- Debugging flags --
   ---------------------

   Print_Ada_Tree       : Boolean := False;
   --  Controls the dumping of the Ada tree

   Output_Unit_Withing  : Boolean := False;
   --  Outputs the Withed units

   Output_Tree_Warnings : Boolean := False;
   --  Outputs the Warning encoutred while building the Ada tree

   -----------------------------
   -- Code optimization flags --
   -----------------------------

   --  Skeleton optimization using minimale perfect hash functions
   --  instead of the big "if .. elsif .. elsif ..."

   Use_Minimal_Hash_Function : Boolean := False;
   Optimize_CPU              : Boolean := False;
   Optimize_Memory           : Boolean := False;

   --  The flag below is related to the request handling method (SSI
   --  or DII) By default, the it's the Used

   Use_SII : Boolean := False;

   --  Marshaller optimization using a one time allocation by calculating
   --  the message body size of a GIOP request (used with SII handling)

   Use_Optimized_Buffers_Allocation : Boolean := False;

   --------------------
   -- Temporary Flag --
   --------------------

   Generate_Helpers_Initializers : Boolean := False;
   --  FIXME : This flag controls the generation of the Init
   --  subpackage of a Helper package. It should be removed after the
   --  achievment of the Helpers Initializer new iplementation

   --  In some particular cases, some parts of the IDL tree must not
   --  be generated. The entities below achieve this goal

   type Package_Type is
     (PK_CDR_Spec,
      PK_CDR_Body,
      PK_Buffers_Spec,
      PK_Buffers_Body,
      PK_Helper_Spec,
      PK_Helper_Body,
      PK_Impl_Spec,
      PK_Impl_Body,
      PK_Init_Spec,
      PK_Init_Body,
      PK_Skel_Spec,
      PK_Skel_Body,
      PK_Stub_Spec,
      PK_Stub_Body);

   function Map_Particular_CORBA_Parts
     (E  : Node_Id;
      PK : Package_Type)
     return Boolean;

end Backend.BE_CORBA_Ada;
