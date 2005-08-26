with Namet;  use Namet;
with Types;  use Types;

with Frontend.Nodes;  use Frontend.Nodes;

with Backend.BE_Ada.Nodes;       use Backend.BE_Ada.Nodes;
with Backend.BE_Ada.Nutils;      use Backend.BE_Ada.Nutils;
with Backend.BE_Ada.IDL_To_Ada;  use Backend.BE_Ada.IDL_To_Ada;
with Backend.BE_Ada.Runtime;     use Backend.BE_Ada.Runtime;

package body Backend.BE_Ada.CDRs is

   package FEN renames Frontend.Nodes;
   --  package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_Ada.Nodes;
   package BEU renames Backend.BE_Ada.Nutils;

   package body Package_Spec is

      function Args_Type_Record (N : Node_Id) return Node_Id;
      function From_CDR_Spec (N : Node_Id) return Node_Id;
      function To_CDR_Spec (N : Node_Id) return Node_Id;
      function Update_Request_Spec (N : Node_Id) return Node_Id;

      procedure Visit_Attribute_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Specification (E : Node_Id);

      ----------------------
      -- Args_Type_Record --
      ----------------------

      function Args_Type_Record (N : Node_Id) return Node_Id is
         pragma Assert (BEN.Kind (N) = K_Subprogram_Specification);
         P          : constant List_Id := Parameter_Profile (N);
         T          : constant Node_Id := Return_Type (N);
         Components : List_Id;
         Component  : Node_Id;
         Parameter  : Node_Id;
         Args_Type  : Node_Id := No_Node;
      begin
         Components := New_List (K_Component_List);

         --  For each parameter in the subprogram profile, a member with the
         --  same name and the same type is generated in the record

         if not BEU.Is_Empty (P) then

            --  Skip the first parameter corresponding to 'Self'

            Parameter := Next_Node (First_Node (P));
            while Present (Parameter) loop
               Component := Make_Component_Declaration
                 (Defining_Identifier => Defining_Identifier (Parameter),
                  Subtype_Indication  => Parameter_Type (Parameter));
               Append_Node_To_List (Component, Components);
               Parameter := Next_Node (Parameter);
            end loop;
         end if;

         --  If the subprogram is a function, we add an additional member
         --  corresponding to the result of the function.

         if Present (T) then
            Component := Make_Component_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Returns)),
               Subtype_Indication  => T);
            Append_Node_To_List (Component, Components);
         end if;

         --  If the subprogram is a procedure that takes no parameter, not type
         --  is declared

         if not BEU.Is_Empty (Components) then
            Args_Type := Make_Full_Type_Declaration
              (Defining_Identifier => Map_Args_Type_Identifier
               (Defining_Identifier (N)),
               Type_Definition     => Make_Record_Type_Definition
               (Make_Record_Definition
                (Components)));
            Set_Correct_Parent_Unit_Name
              (Defining_Identifier (Args_Type),
               Defining_Identifier (CDR_Package (Current_Entity)));
         end if;

         return Args_Type;
      end Args_Type_Record;

      -------------------
      -- From_CDR_Spec --
      -------------------

      function From_CDR_Spec (N : Node_Id) return Node_Id is
         pragma Assert (BEN.Kind (N) = K_Subprogram_Specification);
         Profile   : List_Id;
         Parameter : Node_Id;
         Spec      : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);

         --  Role parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Role)),
            Subtype_Mark        => RE (RE_Entity_Role),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Buffer parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Buffer)),
            Subtype_Mark        => RE (RE_Buffer_Access),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Representation parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Representation)),
            Subtype_Mark        => Make_Attribute_Designator
            (RE (RE_CDR_Representation), A_Class),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  First_Arg_Alignment parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Error parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Error)),
            Subtype_Mark        => RE (RE_Error_Container),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (Parameter, Profile);

         --  Subprogram Specification

         Spec := Make_Subprogram_Specification
           (Map_From_CDR_Identifier (Defining_Identifier (N)),
            Profile,
            No_Node);
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (Spec),
            Defining_Identifier (CDR_Package (Current_Entity)));

         return Spec;
      end From_CDR_Spec;

      -----------------
      -- To_CDR_Spec --
      -----------------

      function To_CDR_Spec (N : Node_Id) return Node_Id is
         pragma Assert (BEN.Kind (N) = K_Subprogram_Specification);
         Profile   : List_Id;
         Parameter : Node_Id;
         Spec      : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);

         --  Role parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Role)),
            Subtype_Mark        => RE (RE_Entity_Role),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Buffer parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Buffer)),
            Subtype_Mark        => RE (RE_Buffer_Access),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Representation parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Representation)),
            Subtype_Mark        => Make_Attribute_Designator
            (RE (RE_CDR_Representation), A_Class),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  First_Arg_Alignment parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_First_Arg_Alignment)),
            Subtype_Mark        => RE (RE_Alignment_Type),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Error parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Error)),
            Subtype_Mark        => RE (RE_Error_Container),
            Parameter_Mode      => Mode_Inout);
         Append_Node_To_List (Parameter, Profile);

         --  Subprogram Specification

         Spec := Make_Subprogram_Specification
           (Map_To_CDR_Identifier (Defining_Identifier (N)),
            Profile,
            No_Node);
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (Spec),
            Defining_Identifier (CDR_Package (Current_Entity)));

         return Spec;
      end To_CDR_Spec;

      -------------------------
      -- Update_Request_Spec --
      -------------------------

      function Update_Request_Spec (N : Node_Id) return Node_Id is
         pragma Assert (BEN.Kind (N) = K_Subprogram_Specification);
         Profile   : List_Id;
         Parameter : Node_Id;
         Spec      : Node_Id;
      begin
         Profile  := New_List (K_Parameter_Profile);

         --  Request parameter

         Parameter := Make_Parameter_Specification
           (Defining_Identifier => Make_Defining_Identifier
            (PN (P_Request)),
            Subtype_Mark        => RE (RE_Entity_Role),
            Parameter_Mode      => Mode_In);
         Append_Node_To_List (Parameter, Profile);

         --  Subprogram Specification

         Spec := Make_Subprogram_Specification
           (Map_Update_Request_Identifier (Defining_Identifier (N)),
            Profile,
            No_Node);
         Set_Correct_Parent_Unit_Name
           (Defining_Identifier (Spec),
            Defining_Identifier (CDR_Package (Current_Entity)));

         return Spec;
      end Update_Request_Spec;

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
         N    : Node_Id;
         D    : Node_Id;
         Spec : Node_Id;
      begin
         Set_CDR_Spec;

         D := First_Entity (Declarators (E));
         while Present (D) loop
            --  Explaining comment

            Set_Str_To_Name_Buffer
              ("Attribute : ");
            Get_Name_String_And_Append (IDL_Name (Identifier (D)));
            N := Make_Ada_Comment (Name_Find);
            Append_Node_To_List (N, Visible_Part (Current_Package));

            --  The generation process is the same for the GETTER and the
            --  SETTER so we don't duplicate the code

            Spec := (Stub_Node
                     (BE_Node
                      (Identifier (D)))); --  GETTER
            for Index in 1 .. 2 loop --  GETTER, then, SETTER

               --  Generating the 'Operation_Name'_Args_Type declaration

               N := Args_Type_Record (Spec);
               if Present (N) then
                  Append_Node_To_List (N, Visible_Part (Current_Package));
                  Bind_FE_To_Type_Def (Identifier (D), N);

                  --  Declaring the global varable from which the marshalling
                  --  is done and into which the unmarshalled parameter are
                  --  stored

                  N := Make_Object_Declaration
                    (Defining_Identifier => Map_Args_Identifier
                     (Defining_Identifier (Spec)),
                     Object_Definition   => Copy_Node
                     (Defining_Identifier (N)));
                  Set_Correct_Parent_Unit_Name
                    (Defining_Identifier (N),
                     Defining_Identifier (CDR_Package (Current_Entity)));
                  Append_Node_To_List (N, Visible_Part (Current_Package));
                  Bind_FE_To_Args_Record (Identifier (D), N);
               end if;

               --  Generating the 'Operation_Name'_From_CDR spec

               N := From_CDR_Spec (Spec);
               Append_Node_To_List (N, Visible_Part (Current_Package));
               Bind_FE_To_From_CDR (Identifier (D), N);

               --  Generating the 'Operation_Name'_To_CDR spec

               N := To_CDR_Spec (Spec);
               Append_Node_To_List (N, Visible_Part (Current_Package));
               Bind_FE_To_To_CDR (Identifier (D), N);

               --  Generating the 'Operation_Name'_Update_Request spec

               N := Update_Request_Spec (Spec);
               Append_Node_To_List (N, Visible_Part (Current_Package));
               Bind_FE_To_Update_Request (Identifier (D), N);

               if not Is_Readonly (E) then
                  Spec := Next_Node (Spec); --  SETTER
               else
                  exit;
               end if;
            end loop;
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
         N    : Node_Id;
         Spec : Node_Id;
      begin
         Set_CDR_Spec;

         --  Explaining comment

         Set_Str_To_Name_Buffer
           ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_Node_To_List (N, Visible_Part (Current_Package));

         --  Getting the subprogram spec

         Spec := Stub_Node (BE_Node (Identifier (E)));

         --  Generating the 'Operation_Name'_Args_Type declaration

         N := Args_Type_Record (Spec);
         if Present (N) then
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_Type_Def (Identifier (E), N);

            --  Declaring the global varable from which the marshalling is
            --  done and into which the unmarshalled parameter are stored

            N := Make_Object_Declaration
              (Defining_Identifier => Map_Args_Identifier
               (Defining_Identifier (Spec)),
               Object_Definition   => Copy_Node
               (Defining_Identifier (N)));
            Set_Correct_Parent_Unit_Name
              (Defining_Identifier (N),
               Defining_Identifier (CDR_Package (Current_Entity)));
            Append_Node_To_List (N, Visible_Part (Current_Package));
            Bind_FE_To_Args_Record (Identifier (E), N);
         end if;

         --  Generating the 'Operation_Name'_From_CDR spec

         N := From_CDR_Spec (Spec);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_From_CDR (Identifier (E), N);

         --  Generating the 'Operation_Name'_To_CDR spec

         N := To_CDR_Spec (Spec);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_To_CDR (Identifier (E), N);

         --  Generating the 'Operation_Name'_Update_Request spec

         N := Update_Request_Spec (Spec);
         Append_Node_To_List (N, Visible_Part (Current_Package));
         Bind_FE_To_Update_Request (Identifier (E), N);

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
