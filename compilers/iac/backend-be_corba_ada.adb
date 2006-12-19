------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 B A C K E N D . B E _ C O R B A _ A D A                  --
--                                                                          --
--                                 B o d y                                  --
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

with Output;    use Output;
with Values;    use Values;

with Frontend.Nodes;            use Frontend.Nodes;

with Backend.BE_CORBA_Ada.Debug;      use Backend.BE_CORBA_Ada.Debug;
with Backend.BE_CORBA_Ada.Expand;
with Backend.BE_CORBA_Ada.IDL_To_Ada; use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Generator;  use Backend.BE_CORBA_Ada.Generator;
with Backend.BE_CORBA_Ada.Nutils;     use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Runtime;    use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Nodes;

with Backend.BE_CORBA_Ada.Helpers;
with Backend.BE_CORBA_Ada.Helpers_Internals;
with Backend.BE_CORBA_Ada.Impls;
with Backend.BE_CORBA_Ada.Stubs;
with Backend.BE_CORBA_Ada.Skels;
with Backend.BE_CORBA_Ada.CDRs;
with Backend.BE_CORBA_Ada.Buffers;
with Backend.BE_CORBA_Ada.Aligned;

package body Backend.BE_CORBA_Ada is

   package BEN renames Backend.BE_CORBA_Ada.Nodes;
   package FEN renames Frontend.Nodes;

   procedure Initialize;

   procedure Visit (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
   begin
      Initialize;
      Visit_Specification (E);

      if Print_Ada_Tree then
         W_Node_Id (BEN.Stub_Node (BE_Node (Identifier (E))));
      else
         Generator.Generate (BEN.Stub_Node (BE_Node (Identifier (E))));
      end if;
   end Generate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Runtime.Initialize;
      Set_Space_Increment (3);
      Int0_Val := New_Integer_Value (0, 1, 10);
      Int1_Val := New_Integer_Value (1, 1, 10);
      Nutils.Initialize;
   end Initialize;

   -----------
   -- Usage --
   -----------

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Line
        (Hdr & "-i       Generate implementation packages");
      Write_Line
        (Hdr & "-c       Generate code for client side only");
      Write_Line
        (Hdr & "-s       Generate code for server side only");
      Write_Line
        (Hdr & "-hc      Using perfect minimal hash tables in skeletons");
      Write_Line
        (Hdr & "         and minimize CPU time");
      Write_Line
        (Hdr & "-hm      Using perfect minimal hash tables in skeletons");
      Write_Line
        (Hdr & "         and minimize memory space");
      Write_Line
        (Hdr & "-rs      Use the SII/SSI to handle requests");
      Write_Line
        (Hdr & "-ro      Use the SII/SSI and optimize buffer allocation");
      Write_Line
        (Hdr & "-ra      Use the SII/SSI and optimize parameter marshalling");
      Write_Line
        (Hdr & "-rd      Use the DII/DSI to handle requests");
      Write_Line
        (Hdr & "-da      Dump the Ada tree");
      Write_Line
        (Hdr & "-db      Generate only the package bodies");
      Write_Line
        (Hdr & "-ds      Generate only the package specs");
      Write_Line
        (Hdr & "-dw      Output the withed entities");
      Write_Line
        (Hdr & "-dt      Output tree warnings");
      Write_Line
        (Hdr & "-di      Generate code for imported entities");
   end Usage;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      --  Generate packages specifications

      --  NB : Even if the user did not request the generation of
      --  implementation templates, the Ada trees relative to the
      --  specs of these Units have to be created because they are
      --  used by the skeleton sub-tree. However the code spec is
      --  generated if and only if the user requested it (see the
      --  Map_IDL_Unit in the Backen.BE_Ada.IDL_To_Ada package for
      --  more details).

      --  Created independently from the command line options

      Stubs.Package_Spec.Visit (E);
      Helpers.Package_Spec.Visit (E);
      Helpers_Internals.Package_Spec.Visit (E);
      Impls.Package_Spec.Visit (E);

      if not Disable_Server_Code_Gen then
         Skels.Package_Spec.Visit (E);
      end if;

      if Use_SII then
         CDRs.Package_Spec.Visit (E);
      end if;

      if Use_Optimized_Buffers_Allocation then
         Buffers.Package_Spec.Visit (E);
      end if;

      if Use_Compiler_Alignment then
         Aligned.Package_Spec.Visit (E);
      end if;

      --  Generate packages bodies

      if not Disable_Client_Code_Gen then
         Stubs.Package_Body.Visit (E);
      end if;

      --  The order is important here because the dependencies of the
      --  Helper package are computed while building the Initialize
      --  routines

      Helpers_Internals.Package_Body.Visit (E);
      Helpers.Package_Body.Visit (E);

      if not Disable_Server_Code_Gen then
         Skels.Package_Body.Visit (E);
      end if;

      if Impl_Packages_Gen then
         Impls.Package_Body.Visit (E);
      end if;

      if Use_SII then
         CDRs.Package_Body.Visit (E);
      end if;

      if Use_Optimized_Buffers_Allocation then
         Buffers.Package_Body.Visit (E);
      end if;
   end Visit;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      N          : Node_Id;
   begin
      Backend.BE_CORBA_Ada.Expand.Expand (E);
      N := Map_IDL_Unit (E);
      Push_Entity (N);
      Visit (E);
      Pop_Entity;
   end Visit_Specification;

   -----------------------------
   -- Get_Not_Suppressed_Part --
   -----------------------------

   function Map_Particular_CORBA_Parts
     (E  : Node_Id;
      PK : Package_Type)
     return Boolean
   is

      procedure Dispatched_Visit (Entity : Node_Id);
      --  This procedure calls the right Visit procedure depending on
      --  the PK parameter. This call doesn't occur only if a code
      --  generation must be done for Entity

      ----------------------
      -- Dispatched_Visit --
      ----------------------

      procedure Dispatched_Visit (Entity : Node_Id) is
         E_Name : Name_Id;
      begin
         if FEN.Kind (Entity) = K_Module then
            E_Name := FEN.IDL_Name (Identifier (Entity));
         else
            return;
         end if;

         if E_Name = Nutils.Repository_Root_Name then

            --  Uncomment the instruction below if you want to
            --  generate code for the CORBA::IDL_Sequences module:

            --  or else E_Name = Nutils.IDL_Sequences_name

            case PK is
               when PK_CDR_Spec =>
                  CDRs.Package_Spec.Visit (Entity);
               when PK_CDR_Body =>
                  CDRs.Package_Body.Visit (Entity);
               when PK_Buffers_Spec =>
                  Buffers.Package_Spec.Visit (Entity);
               when PK_Buffers_Body =>
                  Buffers.Package_Body.Visit (Entity);
               when PK_Aligned_Spec =>
                  Aligned.Package_Spec.Visit (Entity);
               when PK_Stub_Spec   =>
                  Stubs.Package_Spec.Visit (Entity);
               when PK_Stub_Body   =>
                  Stubs.Package_Body.Visit (Entity);
               when PK_Helper_Spec =>
                  Helpers.Package_Spec.Visit (Entity);
               when PK_Helper_Body =>
                  Helpers.Package_Body.Visit (Entity);
               when PK_Init_Spec =>
                  Helpers_Internals.Package_Spec.Visit (Entity);
               when PK_Init_Body =>
                  Helpers_Internals.Package_Body.Visit (Entity);
               when PK_Skel_Spec   =>
                  Skels.Package_Spec.Visit (Entity);
               when PK_Skel_Body   =>
                  Skels.Package_Body.Visit (Entity);
               when PK_Impl_Spec   =>
                  Impls.Package_Spec.Visit (Entity);
               when PK_Impl_Body   =>
                  Impls.Package_Body.Visit (Entity);
            end case;
         end if;
      end Dispatched_Visit;

      Result     : Boolean := False;
      Definition : Node_Id;
   begin
      if FEN.Kind (E) = K_Module then
         if FEN.IDL_Name (Identifier (E)) = Nutils.CORBA_Name then
            Definition := First_Entity (Definitions (E));

            while Present (Definition) loop
               Dispatched_Visit (Definition);
               Definition := Next_Entity (Definition);
            end loop;

            Result := True;
         end if;
      end if;
      return Result;
   end Map_Particular_CORBA_Parts;

end Backend.BE_CORBA_Ada;
