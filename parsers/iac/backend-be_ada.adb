with GNAT.Command_Line; use GNAT.Command_Line;

with Output;    use Output;
with Types;     use Types;
with Values;    use Values;

with Frontend.Nodes;            use Frontend.Nodes;
with Frontend.Debug;

with Backend.BE_Ada.Debug;      use Backend.BE_Ada.Debug;
with Backend.BE_Ada.IDL_To_Ada; use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Generator;  use Backend.BE_Ada.Generator;
with Backend.BE_Ada.Nutils;     use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.Runtime;    use Backend.BE_Ada.Runtime;
with Backend.BE_Ada.Nodes;

with Backend.BE_Ada.Helpers;
with Backend.BE_Ada.Impls;
with Backend.BE_Ada.Stubs;
with Backend.BE_Ada.Skels;


package body Backend.BE_Ada is

   Print_Ada_Tree : Boolean := False;
   Print_IDL_Tree : Boolean := False;
   Build_Impls    : constant Boolean := True;

   package BEN renames Backend.BE_Ada.Nodes;

   procedure Initialize;

   procedure Visit (E : Node_Id);
   procedure Visit_Specification (E : Node_Id);

   ---------------
   -- Configure --
   ---------------

   procedure Configure is
   begin
      loop
         case Getopt ("t i l s b :") is
            when ASCII.NUL =>
               exit;

            when 'b' =>
               Generate_Bodies := True;
               Generate_Specs := False;

            when 's' =>
               Generate_Specs := True;
               Generate_Bodies := False;

            when 'i' =>
               Print_IDL_Tree := True;

            when 'l' =>
               Var_Name_Len := Natural'Value (Parameter);

            when 't' =>
               Print_Ada_Tree := True;

            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Configure;

   --------------
   -- Generate --
   --------------

   procedure Generate (E : Node_Id) is
   begin
      Initialize;
      Visit_Specification (E);

      if Print_IDL_Tree then
         Frontend.Debug.W_Node_Id (E);
      end if;

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
      Nutils.Initialize;
   end Initialize;

   -----------
   -- Usage --
   -----------

   procedure Usage (Indent : Natural) is
      Hdr : constant String (1 .. Indent - 1) := (others => ' ');
   begin
      Write_Str (Hdr);
      Write_Str ("-i       Generate Implementation files");
      Write_Str ("-t       Dump Ada tree");
      Write_Eol;
   end Usage;

   -----------
   -- Visit --
   -----------

   procedure Visit (E : Node_Id) is
   begin
      Stubs.Package_Spec.Visit (E);
      Helpers.Package_Spec.Visit (E);
      Skels.Package_Spec.Visit (E);
      Stubs.Package_Body.Visit (E);
      Helpers.Package_Body.Visit (E);

      if Build_Impls then
         Impls.Package_Spec.Visit (E);
         Impls.Package_Body.Visit (E);
      end if;
   end Visit;

   -------------------------
   -- Visit_Specification --
   -------------------------

   procedure Visit_Specification (E : Node_Id) is
      Definition : Node_Id;
   begin
      Push_Entity (Map_IDL_Unit (E));
      Definition := First_Entity (Definitions (E));
      while Present (Definition) loop
         Visit (Definition);
         Definition := Next_Entity (Definition);
      end loop;
      Pop_Entity;
   end Visit_Specification;

end Backend.BE_Ada;
