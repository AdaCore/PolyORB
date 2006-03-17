------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                       B A C K E N D . B E _ A D A                        --
--                                                                          --
--                                 B o d y                                  --
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

with Output;    use Output;
with Values;    use Values;

with Frontend.Nodes;            use Frontend.Nodes;

with Backend.BE_Ada.Debug;      use Backend.BE_Ada.Debug;
with Backend.BE_Ada.Expand;
with Backend.BE_Ada.IDL_To_Ada; use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Generator;  use Backend.BE_Ada.Generator;
with Backend.BE_Ada.Nutils;     use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;    use Backend.BE_Ada.Runtime;
with Backend.BE_Ada.Nodes;

with Backend.BE_Ada.Helpers;
with Backend.BE_Ada.Impls;
with Backend.BE_Ada.Stubs;
with Backend.BE_Ada.Skels;
with Backend.BE_Ada.CDRs;

package body Backend.BE_Ada is

   package BEN renames Backend.BE_Ada.Nodes;
   package FEN renames Frontend.Nodes;

   procedure Initialize;

   procedure Visit (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
      Print_Tree : Boolean := False;

   begin
      Initialize;
      Visit_Specification (E);

      if Print_Ada_Tree then
         W_Node_Id (BEN.Stub_Node (BE_Node (Identifier (E))));
         Print_Tree := True;
      end if;

      if not Print_Tree then
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
      Write_Str (Hdr);
      Write_Str ("-i       Generate implementation packages");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-c       Generate code for client side only");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-s       Generate code for server side only");
      Write_Eol;

      Write_Str (Hdr);
      Write_Str ("-hc      Using perfect minimal hash tables in skeletons");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("         and minimize CPU time");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-hm      Using perfect minimal hash tables in skeletons");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("         and minimize memory space");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-rs      Use the SII to handle requests");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-rd      Use the DII to handle requests");
      Write_Eol;

      Write_Str (Hdr);
      Write_Str ("-da      Dump the Ada tree");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-db      Generate only the package bodies");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-ds      Generate only the package specs");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-dw      Output the withed entities");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-dt      Output tree warnings");
      Write_Eol;
      Write_Str (Hdr);
      Write_Str ("-di      Generate code for imported entities");
      Write_Eol;
   end Usage;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      --  Generate packages specifications

      --  NB : Even if the user did not request the generation of
      --  implementation templates or the Client side code, the Ada
      --  trees relative to the specs of these Units has to be created
      --  because it's used by the skeleton subtree. However the code
      --  spec is generated if and only if the user requested it (see
      --  the Map_IDL_Unit in the Backen.BE_Ada.IDL_ToAda package for
      --  more details).

      --  Created independently from the command line options

      Stubs.Package_Spec.Visit (E);
      Helpers.Package_Spec.Visit (E);
      Impls.Package_Spec.Visit (E);

      if not Disable_Server_Code_Gen then
         Skels.Package_Spec.Visit (E);
      end if;

      if Use_SII then
         CDRs.Package_Spec.Visit (E);
      end if;

      --  Generate packages bodies

      if not Disable_Client_Code_Gen then
         Stubs.Package_Body.Visit (E);
      end if;

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
   end Visit;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      N          : Node_Id;
   begin
      Backend.BE_Ada.Expand.Expand (E);
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
      --  This procedure calls the rignt Visit procedure depending on the
      --  PK parameter. This call doesn't occur only if a code generation
      --  must be done for Entity

      procedure Dispatched_Visit (Entity : Node_Id);

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

            --  Uncomment the instruction below if you want to generate code
            --  for the CORBA::IDL_Sequences module
            --  or else E_Name = Nutils.IDL_Sequences_name

            case PK is
               when PK_CDR_Spec =>
                  CDRs.Package_Spec.Visit (Entity);
               when PK_CDR_Body =>
                  CDRs.Package_Body.Visit (Entity);
               when PK_Stub_Spec   =>
                  Stubs.Package_Spec.Visit (Entity);
               when PK_Stub_Body   =>
                  Stubs.Package_Body.Visit (Entity);
               when PK_Helper_Spec =>
                  Helpers.Package_Spec.Visit (Entity);
               when PK_Helper_Body =>
                  Helpers.Package_Body.Visit (Entity);
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

end Backend.BE_Ada;
