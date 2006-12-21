------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 B A C K E N D . B E _ C O R B A _ A D A                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  This is the package responsible of generating the CORBA Ada tree
--  from the IDL tree according to the CORBA Ada mapping
--  specifications.

package Backend.BE_CORBA_Ada is

   procedure Generate (E : Node_Id);
   --  Creates the Ada tree, then depending on the user chosen options
   --  generate the Ada code, dumps the tree...

   --  The procedure Generate uses Visitor Functions. Visitor_XXX
   --  stands for visit IDL node XXX. The returned value of this
   --  function is either a Node_Id or a List_Id, it's related with
   --  the context of each IDL structure in the IDL tree.

   --  The source code generation is realized by calling the
   --  Backend.BE_CORBA_Ada.Generator.Generate (N); (N : the root
   --  IDL_Unit node as defined in
   --  Backend.BE_CORBA_Ada.IDL_To_Ada). This procedure uses
   --  Generate_XXX (stands for Generate the corresponding XXX node
   --  source).

   procedure Usage (Indent : Natural);

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
   --  Outputs the Warning encountered while building the Ada tree

   -----------------------------
   -- Code optimization flags --
   -----------------------------

   --  Skeleton optimization using minimal perfect hash functions
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

   --  Marshalling optimization using the representation clause of the
   --  Ada language to make the padding between parameters (used with
   --  SII handling)

   Use_Compiler_Alignment : Boolean := False;

   --  In some particular cases, some parts of the IDL tree must not
   --  be generated. The entities below achieve this goal

   type Package_Type is
     (PK_CDR_Spec,
      PK_CDR_Body,
      PK_Buffers_Spec,
      PK_Buffers_Body,
      PK_Aligned_Spec,
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
   --  The mapping for some predefined CORBA IDL entities (the CORBA
   --  module) is slightly different from the mapping of other
   --  ``normal'' IDL entities. This function maps these entities and
   --  return True if the passed `E' parameter is a Particular CORBA
   --  entity.

end Backend.BE_CORBA_Ada;
