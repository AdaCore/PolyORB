with Namet;  use Namet;
with Types;  use Types;

with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;

package body Backend.BE_Ada.CDRs is

   package FEN renames Frontend.Nodes;
   --  package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_Ada.Nodes;
   --  package BEU renames Backend.BE_Ada.Nutils;

   package body Package_Spec is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;

         end case;
      end Visit;

      ---------------------------------
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
      begin
         D := First_Entity (Declarators (E));
         while Present (D) loop
            Set_Str_To_Name_Buffer
              ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Visible_Part (Current_Package));
            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No CDR package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_CDR_Spec;

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_CDR_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Str_To_Name_Buffer
           ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Visible_Part (Current_Package));
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));
         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;
         Pop_Entity;
      end Visit_Specification;
   end Package_Spec;

   package body Package_Body is

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Attribute_Declaration =>
               Visit_Attribute_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;

         end case;
      end Visit;

      ---------------------------------
      -- Visit_Attribute_Declaration --
      ---------------------------------

      procedure Visit_Attribute_Declaration (E : Node_Id) is
         N : Node_Id;
         D : Node_Id;
      begin
         D := First_Entity (Declarators (E));
         while Present (D) loop
            Set_Str_To_Name_Buffer
              ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Statements (Current_Package));
            D := Next_Entity (D);
         end loop;
      end Visit_Attribute_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  No CDR package is generated for a local interface

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_CDR_Body;

         N := First_Entity (Interface_Body (E));
         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_CDR_Body) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));
            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;
            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         Set_Str_To_Name_Buffer
           ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Statements (Current_Package));
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));
         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;
         Pop_Entity;
      end Visit_Specification;
   end Package_Body;

end Backend.BE_Ada.CDRs;
