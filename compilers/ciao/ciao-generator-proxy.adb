----------------------------------------
--                                    --
--       ----  ---     --  ----       --
--       -      -     - -  -  -       --
--       -      -    ----  -  -       --
--       ----  ---  -   -  ----       --
--                                    --
----------------------------------------
--  CORBA                             --
--  Interface for                     --
--  Ada'95 distributed systems annex  --
--  Objects                           --
----------------------------------------
--  Copyright (c) 1999                --
--  École nationale supérieure des    --
--  télécommunications                --
----------------------------------------

--  The proxy package generator.
--  Produces a CORBA servant implementation from an
--  annotated IDL tree obtained as output of the translator.
--  $Id: //depot/ciao/main/ciao-generator-proxy.adb#22 $

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Wide_Text_Io;           use Ada.Wide_Text_Io;

with Asis;                       use Asis;
with Asis.Compilation_Units;     use Asis.Compilation_Units;
with Asis.Declarations;          use Asis.Declarations;
with Asis.Definitions;           use Asis.Definitions;
with Asis.Elements;              use Asis.Elements;

with CIAO.Asis_Queries;          use CIAO.Asis_Queries;
with CIAO.IDL_Tree;              use CIAO.IDL_Tree;
with CIAO.IDL_Syntax;            use CIAO.IDL_Syntax;
with CIAO.Namet;                 use CIAO.Namet;
with CIAO.Nlists;                use CIAO.Nlists;
with CIAO.Options;               use CIAO.Options;
with CIAO.Translator.Maps;       use CIAO.Translator.Maps;
with CIAO.Translator.State;      use CIAO.Translator.State;
with CIAO.Types;                 use CIAO.Types;
with CIAO.Ada_Source_Streams;    use CIAO.Ada_Source_Streams;

package body CIAO.Generator.Proxy is

   use ORB_Deps;

   ---------------------------------
   -- A stack of IDL module names --
   ---------------------------------

   type Stack_Node;
   type Stack is access all Stack_Node;

   type Stack_Node is record
      Name : Wide_String_Ptr;
      Next : Stack;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (Stack_Node, Stack);

   Module_Names : Stack := null;

   procedure Push (Name : Wide_String) is
   begin
      Module_Names := new Stack_Node'(Name => new Wide_String'(Name),
                                      Next => Module_Names);
   end Push;

   procedure Pop is
      Old_Top : Stack := Module_Names;
   begin
      Module_Names := Old_Top.Next;
      Free (Old_Top.Name);
      Free (Old_Top);
   end;

   function Depth return Natural is
      Count : Natural := 0;
      Module : Stack := Module_Names;
   begin
      while Module /= null loop
         Count := Count + 1;
         Module := Module.Next;
      end loop;

      return Count;
   end Depth;

   function Map_Identifier
     (Id : Wide_String)
     return Wide_String
   is
      Ret : Wide_String := Id;
      First : Integer := Id'First;
      Prev_Underscore : Boolean := False;
   begin
      while First < Ret'Last
        and then Ret (First) = '_' loop
         First := First + 1;
      end loop;

      for I in First .. Ret'Last loop
         if Ret (I) = '_' then
            if Prev_Underscore then
               Ret (I) := 'U';
            end if;
         end if;
         Prev_Underscore := (Ret (I) = '_');
      end loop;

      return Ret (First .. Ret'Last);
   end Map_Identifier;

   function Module_Name
     (Module : Stack)
     return Wide_String is
   begin
      if Module = null then
         return "EMPTY_STACK";
      end if;
      if Module.Next /= null then
         return Module_Name (Module.Next)
           & "."
           & Map_Identifier (Module.Name.all);
      else
         return Map_Identifier (Module.Name.all);
      end if;
   end Module_Name;

   function Current_Ada_Name return Wide_String is
   begin
      return Module_Name (Module_Names);
   end Current_Ada_Name;

   -------------------------------
   -- The source code generator --
   -------------------------------

   --  Wrapper routines for new Source_Streams package

   type Library_Unit is record
      Name : Wide_String_Ptr;
      Unit_Spec : CIAO.Ada_Source_Streams.Compilation_Unit;
      Unit_Body : CIAO.Ada_Source_Streams.Compilation_Unit;
   end record;

   procedure Append
     (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit;
      Line : Wide_String);
   --  Append Line to Unit.

   function New_Package
     (Name : Wide_String)
     return Library_Unit;
   --  Create a Library_Unit.

   procedure Add_Wide_With
     (Unit      : in out CIAO.Ada_Source_Streams.Compilation_Unit;
      Dep       : Wide_String;
      Use_It    : Boolean := False;
      Elab_Control : Elab_Control_Pragma := None);
   --  Add Dep to the semantic dependecies of Unit,
   --  if it is not already present. If Use_It is true,
   --  a "use" clause will be added for that unit.
   --  Additionnally, an elaboration control pragma may
   --  be inserted according to Elab_Control.

   procedure Generate
     (LU : Library_Unit);
   --  Generate the spec and body for LU.

   procedure Append
     (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit;
      Line : Wide_String) is
   begin
      CIAO.Ada_Source_Streams.Put_Line (Unit, To_String (Line));
   end Append;

   function New_Package
     (Name : Wide_String)
     return Library_Unit is
   begin
      return Library_Unit'
        (Name => new Wide_String'(Name),
         Unit_Spec => New_Package (To_String (Name), Unit_Spec),
         Unit_Body => New_Package (To_String (Name), Unit_Body));
   end New_Package;

   procedure Add_Wide_With
     (Unit      : in out CIAO.Ada_Source_Streams.Compilation_Unit;
      Dep       : Wide_String;
      Use_It    : Boolean := False;
      Elab_Control : Elab_Control_Pragma := None) is
   begin
      Add_With (Unit, To_String (Dep), Use_It, Elab_Control);
   end Add_Wide_With;

   --  Short-cuts for code generation subprograms
   procedure PL
     (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit;
      Line : Wide_String) renames Append;

   procedure NL (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit)
     renames New_Line;
   procedure II (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit)
     renames Inc_Indent;
   procedure DI (Unit : in out CIAO.Ada_Source_Streams.Compilation_Unit)
     renames Dec_Indent;

   procedure Generate
     (LU : Library_Unit) is
   begin
      Generate (LU.Unit_Spec);
      Generate (LU.Unit_Body);
   end Generate;

   procedure Generate
     (Tree : in Node_Id) is

      -------------------------------------------------------
      -- Global variables of Generate                      --
      -- These variables are used to pass around the       --
      -- reprensetations produced by recursive invocations --
      -- of Generate_Node.                                 --
      -------------------------------------------------------

      RACWs_Package   : Library_Unit;
      --  Package where RACWs for distributed objects of the current modules
      --  are declared.

      Convert_Package : Library_Unit;
      --  Package where conversion functions for the user-defined types
      --  of the current module are declared.

      Impl_Package    : Library_Unit;
      --  The Impl package for the current interface.

      DSA_Package     : Wide_String_Ptr;
      --  The DSA module within which the distributed
      --  object corresponding to the current interface
      --  notionally resides.

      ----------------------
      -- Utility routines --
      ----------------------

      procedure With_Type
        (N : Node_Id;
         L : in out CIAO.Ada_Source_Streams.Compilation_Unit);
      --  A dependency is added to L on the package where
      --  the type denoted by N is defined.

      procedure With_Convert_For_Type
        (N : Node_Id;
         L : in out CIAO.Ada_Source_Streams.Compilation_Unit);
      --  A dependency is added to L on the Convert package
      --  for the module that defines the type denoted by node N
      --  (an <op_type_spec>, <param_type_spec> or <simple_type_spec>).

      procedure Generate_Node
        (N : Node_Id;
         Buffer : in out Unbounded_Wide_String);
      --  Do the generation for node N into Buffer (if this node
      --  produces intermediate text), or into the current packages.

      -----------------------------------------------
      -- Some node kinds require heavy processing; --
      -- we handle these in separated subprograms. --
      -----------------------------------------------

      --  <type_dcl>

      procedure Produce_Type_Converters
        (IDL_Declaration : Node_Id;
         Ada_Declaration : Asis.Declaration);
      --  Produce the To_Ada and To_CORBA functions
      --  for the type declared by Ada_Declaration,
      --  which is translated to <type_dcl> IDL_Declaration.

      --  <interface_dcl>

      procedure Produce_Interface
        (IDL_Declaration : Node_Id);
      --  Produce the complete translation of an <interface_dcl>.

      ---------------------------------
      -- Bodies of local subprograms --
      ---------------------------------

      procedure With_Type
        (N : Node_Id;
         L : in out CIAO.Ada_Source_Streams.Compilation_Unit)
      is
         Type_Name_Buffer : Unbounded_Wide_String;
      begin
         pragma Assert (False
           or else Node_Kind (N) = N_Op_Type_Spec
           or else Node_Kind (N) = N_Param_Type_Spec
           or else Node_Kind (N) = N_Simple_Type_Spec);

         Generate_Node (N, Type_Name_Buffer);
         declare
            Type_Name : constant Wide_String
              := To_Wide_String (Type_Name_Buffer);
            Dot : Integer := Type_Name'Last;
         begin
            while Dot >= Type_Name'First loop
               exit when Type_Name (Dot) = '.';
               Dot := Dot - 1;
            end loop;

            if Dot > Type_Name'First then
               if Type_Name (Type_Name'First .. Dot - 1) = "CORBA" then
                  --  A base type.

                  Add_Wide_With (L, "CORBA", Elab_Control => Elaborate);
               else
                  --  A scoped name that denotes a user-defined
                  --  type.

                  Add_Wide_With (L, Type_Name (Type_Name'First .. Dot - 1));
               end if;
            end if;
         end;
      end With_Type;

      procedure With_Convert_For_Type
        (N : Node_Id;
         L : in out CIAO.Ada_Source_Streams.Compilation_Unit)
      is
         Type_Name_Buffer : Unbounded_Wide_String;
      begin
         pragma Assert (False
           or else Node_Kind (N) = N_Op_Type_Spec
           or else Node_Kind (N) = N_Param_Type_Spec
           or else Node_Kind (N) = N_Simple_Type_Spec);

         Generate_Node (N, Type_Name_Buffer);
         declare
            --  XXX
            --  This is wrong -- Map_Identifier works on a
            --  single name component, and won't suppress leading
            --  underscores at start of components of a qualified
            --  name. But for now, it will make do.
            Type_Name : constant Wide_String
              := Map_Identifier (To_Wide_String (Type_Name_Buffer));
            Dot : Integer := Type_Name'Last;
         begin
            while Dot >= Type_Name'First loop
               exit when Type_Name (Dot) = '.';
               Dot := Dot - 1;
            end loop;

            if Dot > Type_Name'First then
               if Type_Name (Type_Name'First .. Dot - 1) = "CORBA" then
                  --  A base type.

                  Add_Wide_With (L, "CIAO_Runtime.Convert", Use_It => True);
               else
                  --  A scoped name that denotes a user-defined
                  --  type.

                  Add_Wide_With (L, Type_Name (Type_Name'First .. Dot - 1)
                            & ".Convert", Use_It => True);
               end if;
            end if;
         end;
      end With_Convert_For_Type;

      procedure Produce_Type_Converters
        (IDL_Declaration : Node_Id;
         Ada_Declaration : Asis.Declaration) is
         Ada_Type_Name : constant Wide_String
           := Ada_Full_Name (Ada_Declaration);
         --  The type as declared in the DSA package.

         CORBA_Type_Name : constant Wide_String
           := "DSA_" & Ada_Type_Name;
         --  The type as mapped by the IDL to Ada translator.

         procedure Produce_Specification
           (Conversion_Ada_Type_Name : Wide_String) is
         begin
            NL (Convert_Package.Unit_Spec);
            Append (Convert_Package.Unit_Spec,
                    "function To_CORBA (Ada_Val : " & Conversion_Ada_Type_Name
                      & ") return " & CORBA_Type_Name & ";");
            Append (Convert_Package.Unit_Spec,
                    "function To_Ada (CORBA_Val : " & CORBA_Type_Name &
                       ") return " & Conversion_Ada_Type_Name & ";");
         end Produce_Specification;

         procedure Produce_Body_Opaque
           (Conversion_Ada_Type_Name : Wide_String)
         is
            --  The translation of this declaration is a <type_dcl>
            --  within the current module, which defines a sequence<octet>
            --  type. According to the CORBA specification, this sequence
            --  type is mapped to an implementation-dependant instanciation
            --  of CORBA.Sequences within the package that maps the
            --  current module.

            Sequences_Instance : constant Wide_String
              := Current_Ada_Name & "." &
              Sequences_Package
                (Template_Type_Spec
                 (Specific_Type_Spec
                  (Type_Spec
                   (Type_Declarator
                    (CIAO.Translator.State.Get_Translation
                     (Ada_Declaration))))));

            To_Sequence : constant Wide_String
              := Sequences_Instance & ".To_Sequence";
            To_Element_Array : constant Wide_String
              := Sequences_Instance & ".To_Element_Array";
         begin

            --------------
            -- To_CORBA --
            --------------

            Add_Wide_With (Convert_Package.Unit_Body, "CIAO_Runtime.Encap_Streams",
                      Use_It => True);
            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_CORBA (Ada_Val : " & Conversion_Ada_Type_Name &
                      ") return " & CORBA_Type_Name & " is");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "St : aliased Stream;");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, Conversion_Ada_Type_Name &
                    "'Output (St'Access, Ada_Val);");
            Append (Convert_Package.Unit_Body, "return " & CORBA_Type_Name);
            Append (Convert_Package.Unit_Body, "  (" & To_Sequence);
            Append (Convert_Package.Unit_Body, "   (" & Sequences_Instance &
                    ".Element_Array (Get_Seq (St))));");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_CORBA;");

            ------------
            -- To_Ada --
            ------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_Ada (CORBA_Val : " & CORBA_Type_Name & ") return "
                    & Conversion_Ada_Type_Name & " is");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "St : aliased Stream;");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "Set_Seq (St, Octet_Array");
            Append (Convert_Package.Unit_Body, "  (" & To_Element_Array);
            Append (Convert_Package.Unit_Body, "  ("
                    & Sequences_Instance & ".Sequence (CORBA_Val))));");
            Append (Convert_Package.Unit_Body, "return " & Conversion_Ada_Type_Name &
                    "'Input (St'Access);");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_Ada;");
         end Produce_Body_Opaque;

         procedure Produce_Body_Nickname
           (Conversion_Ada_Type_Name : Wide_String;
            Ada_Parent_Type_Name     : Wide_String;
            CORBA_Parent_Type_Name   : Wide_String;
            Unchecked_Conversion     : Boolean := False) is
         begin

            --------------
            -- To_CORBA --
            --------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_CORBA (Ada_Val : "
                      & Conversion_Ada_Type_Name
                    & ") return " & CORBA_Type_Name);
            Append (Convert_Package.Unit_Body, "is");
            if Unchecked_Conversion then
               Add_Wide_With (Convert_Package.Unit_Body, "Ada.Unchecked_Conversion");

               II (Convert_Package.Unit_Body);
               Append
                 (Convert_Package.Unit_Body,
                  "function Unchecked_Conversion is new Ada.Unchecked_Conversion");
               Append
                 (Convert_Package.Unit_Body,
                  "  ("
                  & Conversion_Ada_Type_Name & ", "
                  & Ada_Parent_Type_Name & ");");
               DI (Convert_Package.Unit_Body);
            end if;
            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "return " & CORBA_Type_Name);
            Append (Convert_Package.Unit_Body, "  ("
                    & CORBA_Parent_Type_Name & "'");
            if Unchecked_Conversion then
               Append
                 (Convert_Package.Unit_Body,
                  "   (To_CORBA (Unchecked_Conversion (Ada_Val))));");
            else
               Append
                 (Convert_Package.Unit_Body,
                  "   (To_CORBA ("
                  & Ada_Parent_Type_Name &
                  " (Ada_Val))));");
            end if;
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_CORBA;");

            ------------
            -- To_Ada --
            ------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_Ada (CORBA_Val : " & CORBA_Type_Name
                    & ") return " & Conversion_Ada_Type_Name & " is");

            if Unchecked_Conversion then
               Add_Wide_With (Convert_Package.Unit_Body, "Ada.Unchecked_Conversion");

               II (Convert_Package.Unit_Body);
               Append
                 (Convert_Package.Unit_Body,
                  "function Unchecked_Conversion is new Ada.Unchecked_Conversion");
               Append
                 (Convert_Package.Unit_Body,
                  "  ("
                  & Ada_Parent_Type_Name & ", "
                  & Conversion_Ada_Type_Name & ");");
               DI (Convert_Package.Unit_Body);
            end if;


            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            if Unchecked_Conversion then
               Append
                 (Convert_Package.Unit_Body,
                  "return Unchecked_Conversion");
            else
               Append
                 (Convert_Package.Unit_Body,
                  "return " & Conversion_Ada_Type_Name);
            end if;
            Append (Convert_Package.Unit_Body, "  ("
                    & Ada_Parent_Type_Name & "'");
            Append (Convert_Package.Unit_Body, "   (To_Ada ("
                    & CORBA_Parent_Type_Name &
                    " (CORBA_Val))));");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_Ada;");
         end Produce_Body_Nickname;

         procedure Produce_Body_Enum
           (Conversion_Ada_Type_Name : Wide_String)
         is
            Literals : constant Asis.Declaration_List
              := Enumeration_Literal_Declarations
              (Type_Declaration_View (Ada_Declaration));
         begin

            --------------
            -- To_CORBA --
            --------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_CORBA (Ada_Val : " & Conversion_Ada_Type_Name &
                      ") return " & CORBA_Type_Name & " is");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "Map : constant array (" & Conversion_Ada_Type_Name
                    & ") of " & CORBA_Type_Name);
            Append (Convert_Package.Unit_Body, "  := (");
            for I in Literals'Range loop
               declare
                  Buffer : Unbounded_Wide_String;
                  Defining_Literal : constant Asis.Defining_Name
                    := Declaration_Name (Literals (I));
                  Literal_Image : constant Asis.Program_Text
                    := Isolated_Element_Image (Defining_Literal);
               begin
                  Buffer := To_Unbounded_Wide_String (Literal_Image & " => ");
                  case Defining_Name_Kind (Defining_Literal) is
                     when A_Defining_Enumeration_Literal =>
                        Append (Buffer, Literal_Image);
                     when A_Defining_Character_Literal =>
                        Append (Buffer, Character_Literal_Identifier (Literal_Image));
                     when others =>
                        -- XXX ERROR should not happen.
                        pragma Assert (False);
                        null;

                  end case;

                  if I /= Literals'Last then
                     Append (Buffer, ",");
                  end if;
                  Append (Convert_Package.Unit_Body, "    " & To_Wide_String (Buffer));
               end;
            end loop;
            Append (Convert_Package.Unit_Body, "  );");
            DI (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "return Map (Ada_Val);");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_CORBA;");

            ------------
            -- To_Ada --
            ------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_Ada (CORBA_Val : " & CORBA_Type_Name & ") return "
                    & Conversion_Ada_Type_Name & " is");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "Map : constant array (" & CORBA_Type_Name
                    & ") of " & Conversion_Ada_Type_Name);
            Append (Convert_Package.Unit_Body, "  := (");

            for I in Literals'Range loop
               declare
                  Buffer : Unbounded_Wide_String;
                  Defining_Literal : constant Asis.Defining_Name
                    := Declaration_Name (Literals (I));
                  Literal_Image : constant Asis.Program_Text
                    := Isolated_Element_Image (Defining_Literal);
               begin
                  case Defining_Name_Kind (Defining_Literal) is
                     when A_Defining_Enumeration_Literal =>
                        Buffer := To_Unbounded_Wide_String (Literal_Image);
                     when A_Defining_Character_Literal =>
                        Buffer := To_Unbounded_Wide_String (Character_Literal_Identifier (Literal_Image));
                     when others =>
                        raise Program_Error;
                        -- XXX ERROR should not happen.
                  end case;
                  Append (Buffer, " => " & Literal_Image);

                  if I /= Literals'Last then
                     Append (Buffer, ",");
                  end if;
                  Append (Convert_Package.Unit_Body, "    " & To_Wide_String (Buffer));
               end;
            end loop;
            Append (Convert_Package.Unit_Body, "  );");
            DI (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "return Map (CORBA_Val);");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_Ada;");
         end Produce_Body_Enum;

         procedure Produce_Body_Array (Conversion_Ada_Type_Name : Wide_String) is
            Array_Type_Definition : constant Asis.Definition
              := Type_Declaration_View (Ada_Declaration);
            Dimensions : Natural;
            Index_Subtype_Name : Unbounded_Wide_String;

            Sequences_Instance : constant Wide_String
              := Current_Ada_Name & "." &
              Sequences_Package
              (Template_Type_Spec
               (Specific_Type_Spec
                (Type_Spec
                 (Last
                  (Members
                   (Structure
                    (Specific_Type_Spec
                     (Type_Spec
                      (Type_Declarator
                       (CIAO.Translator.State.Get_Translation
                        (Ada_Declaration)))))))))));

            --  The translation of this declaration is a <type_dcl> within
            --  the current module, which defines a struct type with two
            --  members: a low bound, or array of low bounds, and a sequence
            --  of elements.

            --  According to the CORBA specification, this sequence mapped
            --  to an implementation-dependant instanciation of CORBA.Sequences
            --  within the package that maps the current module.

            --  These nested calls are obscene.

            To_Sequence : constant Wide_String
              := Sequences_Instance & ".To_Sequence";
            To_Element_Array : constant Wide_String
              := Sequences_Instance & ".To_Element_Array";
         begin

            if Type_Kind (Array_Type_Definition)
              = An_Unconstrained_Array_Definition then
               declare
                  Indices : constant Asis.Expression_List
                    := Index_Subtype_Definitions (Array_Type_Definition);
               begin
                  Index_Subtype_Name := To_Unbounded_Wide_String
                    (Ada_Full_Name
                     (Corresponding_Entity_Name_Declaration
                      (Indices (Indices'First))));
                  Dimensions := Indices'Length;
               end;
            else
               declare
                  Indices : constant Asis.Expression_List
                    := Discrete_Subtype_Definitions (Array_Type_Definition);
               begin
                  Index_Subtype_Name := To_Unbounded_Wide_String
                    (Discrete_Subtype_Name (Indices (Indices'First)));
                  Dimensions := Indices'Length;
               end;
            end if;

            pragma Assert (Dimensions = 1);
            -- XXX FOR NOW due to a bug in the Sun IDL front-end
            -- XXX (no support for nested sequences),
            -- XXX multi-dimensional arrays are *not* supported.
            -- XXX The hereunder code must be rewritten if/when
            -- XXX multidimensional arrays are supported.

            --------------
            -- To_CORBA --
            --------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_CORBA (Ada_Val : " & Conversion_Ada_Type_Name &
                      ") return " & CORBA_Type_Name & " is");
            II (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "CORBA_Elements : "
                    & Sequences_Instance & ".Element_Array (0 .. Ada_Val'Length - 1);");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "begin");

            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "for I in Ada_Val'Range loop");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "CORBA_Elements ("
                    & To_Wide_String (Index_Subtype_Name) & "'Pos (I) - "
                    & To_Wide_String (Index_Subtype_Name)
                    & "'Pos (Ada_Val'First)) :=");
            Append (Convert_PAckage.Unit_Body, "  To_CORBA (Ada_Val (I));");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end loop;");
            NL (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "return " & CORBA_Type_Name
                    & "'(Low_Bound => " & To_Wide_String (Index_Subtype_Name) & "'Pos (Ada_Val'First),");
            Append (Convert_Package.Unit_Body, "  Array_Values => "
                    & Sequences_Instance & ".To_Sequence (CORBA_Elements));");
            DI (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "end To_CORBA;");

            ------------
            -- To_Ada --
            ------------

            NL (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body,
                    "function To_Ada (CORBA_Val : " & CORBA_Type_Name & ") return "
                    & Conversion_Ada_Type_Name & " is");
            II (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "CORBA_Elements : constant "
                    & Sequences_Instance & ".Element_Array :=");
            Append (Convert_Package.Unit_Body, "  " & Sequences_Instance
                    & ".To_Element_Array (CORBA_Val.Array_Values);");
            if Type_Kind (Array_Type_Definition)
              = A_Constrained_Array_Definition then
               Append (Convert_Package.Unit_Body, "Ada_Elements : "
                       & Conversion_Ada_Type_Name & ";");
            else
               Append (Convert_Package.Unit_Body, "Ada_Elements : "
                       & Conversion_Ada_Type_Name & "(");
               Append (Convert_Package.Unit_Body, "  "
                       & To_Wide_String (Index_Subtype_Name) & "'Val ("
                       & "CORBA_Val.Low_Bound + "
                       & Sequences_Instance
                       & ".Length (CORBA_Val.Array_Values)));");
            end if;
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "begin");
            II (Convert_Package.Unit_Body);

            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "for I in CORBA_Elements'Range loop");
            II (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "Ada_Elements ("
                    & To_Wide_String (Index_Subtype_Name) & "'Val ("
                    & To_Wide_String (Index_Subtype_Name) & "'Pos (Ada_Elements'First) + I)) :=");
            Append (Convert_PAckage.Unit_Body, "  To_Ada (CORBA_Elements (I));");
            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end loop;");
            NL (Convert_Package.Unit_Body);

            Append (Convert_Package.Unit_Body, "return Ada_Elements;");

            DI (Convert_Package.Unit_Body);
            Append (Convert_Package.Unit_Body, "end To_Ada;");
         end Produce_Body_Array;

      begin   --  Produce_Type_Converters
         case Declaration_Kind (Ada_Declaration) is
            when
              An_Ordinary_Type_Declaration |
              A_Subtype_Declaration        =>
               declare
                  Ada_Definition : constant Asis.Definition
                    := Type_Declaration_View (Ada_Declaration);
                  DK : constant Asis.Definition_Kinds
                    := Definition_Kind (Ada_Definition);
               begin
                  if DK = A_Subtype_Indication
                  then
                     --  A subtype declaration

                     declare
                        CORBA_Ancestor_Type : constant Node_Id
                          := Type_Spec (Type_Declarator (Get_Translation (Ada_Declaration)));
                        CORBA_Ancestor_Type_Name : Unbounded_Wide_String;
                     begin
                        Generate_Node (CORBA_Ancestor_Type, CORBA_Ancestor_Type_Name);
                        Produce_Specification (Ada_Type_Name);
                        Produce_Body_Nickname (Ada_Type_Name,
                                               Ada_Full_Name
                                               (Corresponding_Entity_Name_Declaration
                                                (Asis.Definitions.Subtype_Mark (Ada_Definition))),
                                               To_Wide_String (CORBA_Ancestor_Type_Name));
                     end;
                  else
                     pragma Assert (DK = A_Type_Definition);

                     case Type_Kind (Ada_Definition) is
                        when A_Derived_Type_Definition =>
                           --  A derivation of a non-tagged type.

                           declare
                              CORBA_Ancestor_Type : constant Node_Id
                                := Type_Spec (Type_Declarator (Get_Translation (Ada_Declaration)));
                              CORBA_Ancestor_Type_Name : Unbounded_Wide_String;
                           begin
                              Generate_Node (CORBA_Ancestor_Type, CORBA_Ancestor_Type_Name);
                              Produce_Specification (Ada_Type_Name);
                              Produce_Body_Nickname (Ada_Type_Name,
                                                     Ada_Full_Name
                                                     (Corresponding_Entity_Name_Declaration
                                                      (Asis.Definitions.Subtype_Mark
                                                       (Parent_Subtype_Indication (Ada_Definition)))),
                                                     To_Wide_String (CORBA_Ancestor_Type_Name));
                           end;

                        when An_Enumeration_Type_Definition =>
                           Produce_Specification (Ada_Type_Name);
                           Produce_Body_Enum (Ada_Type_Name);

                        when
                          A_Signed_Integer_Type_Definition   |
                          A_Modular_Type_Definition          |
                          A_Floating_Point_Definition        |
                          An_Ordinary_Fixed_Point_Definition |
                          A_Decimal_Fixed_Point_Definition   =>
                           --  A new numeric type.

                           -- XXX TODO
                           Put_Line ("XXX numeric conversion not implemented.");

                        when
                          An_Unconstrained_Array_Definition |
                          A_Constrained_Array_Definition    =>
                           Produce_Specification (Ada_Type_Name);
                           Produce_Body_Array (Ada_Type_Name);

                        when A_Record_Type_Definition =>
                           -- XXX TODO
                           Put_Line ("XXX record conversion not implemented.");

                        when A_Tagged_Record_Type_Definition =>
                           Produce_Specification (Ada_Type_Name & "'Class");
                           Produce_Body_Opaque (Ada_Type_Name & "'Class");

                        when A_Derived_Record_Extension_Definition =>
                           -- XXX See below (private extension definition).
                           null;

                        when An_Access_Type_Definition =>
                           --  A RACW.
                           declare
                              DSA_Object_Subtype : constant Asis.Expression
                                := Asis.Definitions.Subtype_Mark
                                (Asis.Definitions.Access_To_Object_Definition
                                 (Ada_Definition));
                              DSA_Object_Name : constant Wide_String
                                := Ada_Full_Name
                                (Corresponding_Entity_Name_Declaration (DSA_Object_Subtype));
                           begin
                              Add_Wide_With
                                (Convert_Package.Unit_Spec,
                                 Unit_Full_Name (Enclosing_Compilation_Unit (Ada_Definition)));
                              --  The Convert package spec needs the declaration of the
                              --  newly-defined RACW type.

                              Add_Wide_With (Convert_Package.Unit_Body,
                                          "RACW_DSA_" & Unit_Full_Name
                                          (Enclosing_Compilation_Unit
                                           (Corresponding_Entity_Name_Definition
                                            (DSA_Object_Subtype))));
                              --  The Convert package body needs the declaration of the
                              --  parent Ada type, which is the canonical CIAO RACW
                              --  for the distributed object.

                              --  The declaration for the CORBA parent type
                              --  is already visible, because the parent unit of
                              --  the convert package contains the declaration of
                              --  the new CORBA type as a subtype of the parent CORBA
                              --  type (the forward object reference for the interface).

                              --  XXX Thomas 2000-04-07 Thomas test...
                              With_Convert_For_Type (Specific_Type_Spec
                                                     (Type_Spec
                                                      (Type_Declarator
                                                       (IDL_Declaration))),
                                                     Convert_Package.Unit_Body);

                              --  The Convert package body requires the declaration
                              --  of the conversion subprograms for the parent types.

                              Produce_Specification (Ada_Type_Name);
                              Produce_Body_Nickname (Ada_Type_Name,
                                                     "RACW_DSA_" & DSA_Object_Name,
                                                     "DSA_" & DSA_Object_Name &
                                                     ".Ref",
                                                     Unchecked_Conversion => True);
                           end;
                        when others =>
                           --  Cannot happen.
                           raise Program_Error;

                     end case;

                  end if;
               end;

            when
              A_Task_Type_Declaration      |
              A_Protected_Type_Declaration |
              A_Private_Type_Declaration   =>
               --  A non-(limited tagged) private type,
               --  or a task or protected type
               --  (translated as an encapsulation).

               if Declaration_Kind (Ada_Declaration) = A_Private_Type_Declaration
                 and then Is_Tagged_Type (Ada_Declaration) then
                  Produce_Specification (Ada_Type_Name & "'Class");
                  Produce_Body_Opaque (Ada_Type_Name & "'Class");
               else
                  Produce_Specification (Ada_Type_Name);
                  Produce_Body_Opaque (Ada_Type_Name);
               end if;

            when A_Private_Extension_Declaration =>
               --  A derivation of a non-limited tagged
               --  type. No conversion function is generated
               --  because those of the classwide root
               --  type are used.
               --  (See also above: A_Record_Extension_Definition.)

               --  XXX visibility?? Translator should set a semantic flag
               --  in the IDL tree on this <type_dcl> node to state that
               --  With_Convert_For_Type should with the convert package
               --  for the /ancestor/ type (of which we have a scoped_name).
               --  => TODO in CIAO.Translator and in
               --  With_Convert_For_Type !! XXX
               null;

            when others =>
               --  Cannot happen.
               raise Program_Error;
         end case;
      end Produce_Type_Converters;

      procedure Produce_Interface
        (IDL_Declaration : Node_Id)
      is
         Dummy : Unbounded_Wide_String;

         Distributed_Object_Declaration : constant Asis.Element
           := Origin (IDL_Declaration);
         Export : Node_Id := First (Interface_Body (IDL_Declaration));
         Interface : Node_Id := First (Interfaces (IDL_Declaration));

         Current_Name : constant Wide_String := Current_Ada_Name;
      begin
         Free (DSA_Package);
         DSA_Package := new Wide_String'
           (Current_Name (Current_Name'First + 4 .. Current_Name'Last));
         --  Strip "DSA_" prefix.

         if not Is_Nil (Distributed_Object_Declaration) then

            --  This is a potentially distributed object type
            --  declared in a Pure or Remote_Types package.

            declare
               Object_Names : constant Asis.Defining_Name_List
                 := Names (Distributed_Object_Declaration);
               Object_Name : constant Asis.Program_Text
                 := Isolated_Element_Image (Object_Names (Object_Names'First));

               Ref_Package : constant Wide_String
                 := Current_Ada_Name;
               --  DSA_<moduleName>.<interfaceName>

               Helper_Package : constant Wide_String
                 := Current_Ada_Name & ".Helper";
               --  DSA_<moduleName>.<interfaceName>.Helper

               RACW_Type : constant Wide_String
                 := "RACW_" & Ref_Package;

               procedure Produce_Reference_Converters
                 (Forward : Boolean)
               is
                  To_Ref : constant Wide_String
                    := Ref_Package & ".Helper.To_Ref";
                  Real_Ref_Type : constant Wide_String
                    := Ref_Package & ".Ref";
                  Forward_Ref_Type : constant Wide_String
                    := Ref_Package & "_Forward.Ref";
                  Ref_Type : Wide_String_Ptr;
               begin
                  if Forward then
                     Ref_Type := new Wide_String'(Forward_Ref_Type);
                  else
                     Ref_Type := new Wide_String'(Real_Ref_Type);
                  end if;

                  --  Specification

                  NL (Convert_Package.Unit_Spec);
                  if Forward then
                     --  With the library package that contains the
                     --  forward reference declaration.

                     --  This is dependant on the CORBA implementation.
                     --  The following is correct for Broca, where the
                     --  instance of CORBA.Forward is a subpackage of
                     --  the package that maps the enclosing scope.
                     --  (In CIAO, the enclosing scope is always
                     --  a <module> or <interface>.)

                     declare
                        Dot : Integer := Ref_Package'Last;
                     begin
                        while (Ref_Package (Dot) /= '.') loop
                           Dot := Dot - 1;
                           pragma Assert (Dot > Ref_Package'First);
                        end loop;

                        Add_Wide_With
                          (Convert_Package.Unit_Spec,
                           Ref_Package
                           (Ref_Package'First .. Dot - 1));
                     end;
                  else
                     Add_Wide_With (Convert_Package.Unit_Spec, Ref_Package);
                  end if;

                  Append (Convert_Package.Unit_Spec,
                          "function To_CORBA (RACW : " & RACW_Type
                          & ") return " & Ref_Type.all & ";");
                  Append (Convert_Package.Unit_Spec,
                          "function To_Ada (Ref : " & Ref_Type.all
                          & ") return " & RACW_Type & ";");

                  --  Body
                  Add_With
                    (Convert_Package.Unit_Body, To_String (Helper_Package));
                  NL (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body,
                          "function To_CORBA (RACW : " & RACW_Type
                          & ") return " & Ref_Type.all & " is");
                  II (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "use CORBA;");
                  Append (Convert_Package.Unit_Body, "use " &
                          ObjectId_Sequences_Package & ";");
                  NL (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "St : aliased Stream;");
                  DI (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "begin");
                  II (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, RACW_Type
                          & "'Output (St'Access, RACW);");
                  if Forward then
                     Append (Convert_Package.Unit_Body,
                             "return " & Ref_Package &
                             ".Convert_Forward.To_Forward");
                     Append (Convert_Package.Unit_Body,
                             "  (" & To_Ref);
                     Append (Convert_Package.Unit_Body,
                             "   (Create_Reference_With_Id (" & Ref_Package
                             & ".Impl.POA,");
                     Append (Convert_Package.Unit_Body,
                             "    PortableServer.ObjectId (To_Sequence "
                             & "(Element_Array (Get_Seq (St)))),");
                     Append (Convert_Package.Unit_Body,
                             "      CORBA.To_CORBA_String (" & Ref_Package
                             & ".Repository_Id))));");
                  else
                     Append (Convert_Package.Unit_Body,
                             "return " & To_Ref);
                     Append (Convert_Package.Unit_Body,
                             "   (Create_Reference_With_Id (" & Ref_Package
                             & ".Impl.POA,");
                     Append (Convert_Package.Unit_Body,
                             "    PortableServer.ObjectId (To_Sequence "
                             & "(Element_Array (Get_Seq (St)))),");
                     Append (Convert_Package.Unit_Body,
                             "      CORBA.To_CORBA_String (" & Ref_Package
                             & ".Repository_Id)));");
                  end if;

                  DI (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "end To_CORBA;");

                  NL (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body,
                          "function To_Ada (Ref : " & Ref_Type.all
                          & ") return " & RACW_Type & " is");
                  II (Convert_Package.Unit_Body);

                  Append (Convert_Package.Unit_Body,
                          "Object_Id : constant PortableServer.ObjectId :=");
                  if Forward then
                     Append (Convert_Package.Unit_Body,
                             "  PortableServer.POA.Reference_To_Id ("
                             & Ref_Package & ".Impl.POA, " & Ref_Package &
                             ".Convert_Forward.From_Forward (Ref));");
                  else
                     Append (Convert_Package.Unit_Body,
                             "  PortableServer.POA.Reference_To_Id ("
                             & Ref_Package & ".Impl.POA, Ref);");
                  end if;
                  DI (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "begin");
                  II (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body,
                          "return ObjectId_To_RACW (Object_Id);");
                  DI (Convert_Package.Unit_Body);
                  Append (Convert_Package.Unit_Body, "end To_Ada;");
               end Produce_Reference_Converters;

            begin

               --  Declare a RACW for this interface, for our internal use

               Append (RACWs_Package.Unit_Spec, "type " & Object_Name
                       & " is access all "
                       & Ref_Package (Ref_Package'First + 4
                                      .. Ref_Package'Last) & "'Class;");
               --  Strip DSA_ prefix.

               NL (RACWs_Package.Unit_Spec);

               --  Declare conversion functions between RACW and CORBA
               --  reference.

               Add_Wide_With (Convert_Package.Unit_Spec, RACWs_Package.Name.all);

               Add_Wide_With
                 (Convert_Package.Unit_Spec,
                  "PortableServer",
                  Elab_Control => Elaborate);
               --  For PortableServer.ObjectId.

               Add_Wide_With
                 (Convert_Package.Unit_Body, "PortableServer.POA",
                  Use_It => True,
                  Elab_Control => Elaborate);
               --  For Create_Reference_With_Id.

               Add_Wide_With (Convert_Package.Unit_Body, Ref_Package & ".Impl");
               Add_Wide_With
                 (Convert_Package.Unit_Body,
                  "CIAO_Runtime.Encap_Streams",
                  Use_It => True);
               Add_Wide_With (Convert_Package.Unit_Body, ObjectId_Sequences_Dependency);

               -------------------------------------
               -- The ObjectId to RACW converter. --
               -------------------------------------

               NL (Convert_Package.Unit_Spec);
               Append (Convert_Package.Unit_Spec,
                       "function ObjectId_To_RACW (Object_Id : PortableServer.ObjectId)");
               Append (Convert_Package.Unit_Spec,
                       "  return " & RACW_Type & ";");

               NL (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body,
                       "function ObjectId_To_RACW (Object_Id : PortableServer.ObjectId)");
               Append (Convert_Package.Unit_Body,
                       "  return " & RACW_Type & " is");
               II (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body, "use " & ObjectId_Sequences_Package & ";");
               NL (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body, "St : aliased Stream;");
               DI (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body, "begin");
               II (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body, "Set_Seq (St, Octet_Array");
               Append (Convert_Package.Unit_Body, "  (To_Element_Array ("
                       & ObjectId_Sequences_Package
                       & ".Sequence (Object_Id))));");
               Append (Convert_Package.Unit_Body, "return " &
                       RACW_Type & "'Input (St'Access);");
               DI (Convert_Package.Unit_Body);
               Append (Convert_Package.Unit_Body, "end ObjectId_To_RACW;");

               ----------------------------------------
               -- The reference <-> RACW converters. --
               ----------------------------------------

               Produce_Reference_Converters (Forward => False);
               Produce_Reference_Converters (Forward => True);
            end;
         end if;

         --  Generate implementation of the interface

         declare
            Ada_Name : constant Wide_String
              := Current_Ada_Name;
            Inh_Spec : constant List_Id
              := Inheritance_Spec
              (Interface_Header (IDL_Declaration));

            ImS : CIAO.Ada_Source_Streams.Compilation_Unit
              renames Impl_Package.Unit_Spec;
            ImB : CIAO.Ada_Source_Streams.Compilation_Unit
              renames Impl_Package.Unit_Body;

         begin
            Impl_Package := New_Package (Ada_Name & ".Impl");

            Add_Wide_With (ImS, "PortableServer.POA", Elab_Control => Elaborate);
            Add_Wide_With
              (ImB, Ada_Name & ".Skel",
               Use_It => False,
               Elab_Control => Elaborate);
            Add_Wide_With (ImB, "CIAO_Runtime.Convert", Use_It => True);

            if not Is_Remote_Subprograms (IDL_Declaration) then
               Add_Wide_With (ImB, Convert_Package.Name.all, Use_It => True);
               --  We are producing the interface that represents a distributed
               --  object: the corresponding conversion subprograms reside
               --  in the current conversion package.
            end if;

            if Is_Empty_List (Inh_Spec) then
               --  This interface has no ancestors

               Add_Wide_With (ImS, "PortableServer");
               PL (ImS, "type Object is new PortableServer.Servant_Base with null record;");
            else
               declare
                  Ancestor : constant Node_Id
                    := First (Inh_Spec);
                  Ancestor_Name : Unbounded_Wide_String;
               begin
                  Generate_Node (Ancestor, Ancestor_Name);
                  Add_Wide_With (ImS,
                            To_Wide_String (Ancestor_Name) & ".Impl");
                  PL (ImS, "type Object is new "
                          & To_Wide_String (Ancestor_Name)
                          & ".Impl.Object" & " with null record;");
               end;
            end if;
            NL (ImS);
            PL (ImS, "type Object_Ptr is access all Object;");

            while Present (Export) loop
               Generate_Node (Export, Dummy);
               Export := Next (Export);
            end loop;

            while Present (Interface) loop
               Generate_Node (Interface, Dummy);
               Export := Next (Interface);
            end loop;

            Add_Wide_With (ImB, "PortableServer");
            Add_Wide_With (ImS, "PortableServer.POA");
            Add_Wide_With (ImS, "PortableServer.POAManager");

            NL (ImB);
            PL (ImB, "My_Server : constant Object_Ptr := new Object;");

            if not Is_Nil (Distributed_Object_Declaration) then
               NL (ImS);
               PL (ImS, "POA : PortableServer.POA.Ref;");
               --  That POA is used by the Convert package for this interface,
               --  so it must be exposed by the Impl.

               Add_Wide_With (ImB, "CORBA");
               Add_Wide_With (ImB, "PortableServer.POAManager");
               Add_Wide_With (ImB, "Ada.Text_IO", Use_It => True);
               --  For display of the IOR (To_Standard_String, Put_Line).

               NL (ImB);
               PL (ImB, "procedure Initialize_Proxy (Root_POA : PortableServer.POA.Ref)");
               PL (ImB, "is");
               II (ImB);
               PL (ImB, "use PortableServer;");
               NL (ImB);
               PL (ImB, "Nil_POAManager : PortableServer.POAManager.Ref;");
               NL (ImB);
               DI (ImB);
               PL (ImB, "begin");
               II (ImB);
               NL (ImB);
               PL (ImB, "--  Create the POA");
               NL (ImB);
               PL (ImB, "POA := PortableServer.POA.Ref (PortableServer.POA.Create_POA");
               PL (ImB, "  (Root_POA,");
               II (ImB);
               PL (ImB, "CORBA.To_CORBA_String (""" & Ada_Name & "_Proxy""),");
               PL (ImB, "A_POAManager => Nil_POAManager,");
               PL (ImB, "Tp => ORB_CTRL_MODEL,");
               PL (ImB, "Lp => TRANSIENT,");
               PL (ImB, "Up => MULTIPLE_ID,");
               PL (ImB, "Ip => USER_ID,");
               PL (ImB, "Ap => NO_IMPLICIT_ACTIVATION,");
               PL (ImB, "Sp => NON_RETAIN,");
               PL (ImB, "Rp => USE_DEFAULT_SERVANT));");
               DI (ImB);
               NL (ImB);
               PL (ImB, "--  Set the default servant");
               NL (ImB);
               PL (ImB, "PortableServer.POA.Set_Servant (POA, PortableServer.Servant (My_Server));");
               NL (ImB);
               PL (ImB, "--  Activate the POAManager");
               NL (ImB);
               PL (ImB, "PortableServer.POAManager.Activate");
               PL (ImB, "  (PortableServer.POA.Get_The_POAManager (POA));");
               DI (ImB);
               PL (ImB, "end Initialize_Proxy;");
               NL (ImB);

            else
               Add_Wide_With (ImB, "CORBA");
               Add_Wide_With (ImB, "Ada.Text_IO", Use_It => True);
               --  For display of the IOR (To_Standard_String, Put_Line).
               Add_Wide_With (ImB, "Broca.Naming_Tools");
               --  To register the RCI reference with the CORBA Naming Service.

               NL (ImS);
               PL (ImS, "procedure Initialize_Proxy (Root_POA : PortableServer.POA.Ref);");
               NL (ImS);
               NL (ImB);
               PL (ImB, "procedure Initialize_Proxy (Root_POA : PortableServer.POA.Ref) is");
               II (ImB);
               PL (ImB, "My_Server_Oid : PortableServer.ObjectId;");
               PL (ImB, "My_Server_Ref : CORBA.Object.Ref;");
               PL (ImB, "IOR    : CORBA.String;");
               DI (ImB);
               PL (ImB, "begin");
               II (ImB);
               PL (ImB, "My_Server_Oid := PortableServer.POA.Activate_Object");
               PL (ImB, "  (Root_Poa, PortableServer.Servant (My_Server));");
               PL (ImB, "My_Server_Ref := PortableServer.POA.Servant_To_Reference");
               PL (ImB, "  (Root_Poa, PortableServer.Servant (My_Server));");
               NL (ImB);
               PL (ImB, "Broca.Naming_Tools.Register (""CIAO.subcontext/" & Ada_Name
                   & ".Remote Call Interface"", My_Server_Ref, Rebind => True);");
               --  XXX hard-coded constants: CIAO.subcontext, Remote Call Interface
               PL (ImB, "IOR := CORBA.Object.Object_To_String (My_Server_Ref);");
               PL (ImB, "Put_Line (""'"" & CORBA.To_Standard_String (IOR) & ""'"");");
               DI (ImB);
               PL (ImB, "end Initialize_Proxy;");

            end if;

            Divert (ImB, Elaboration);

            Add_Wide_With (ImB, "CORBA.ORB", Elab_Control => Elaborate);
            PL (ImB, "Initialize_Proxy");
            PL (ImB, "  (Root_POA => PortableServer.POA.To_Ref");
            PL (ImB, "   (CORBA.ORB.Resolve_Initial_References");
            PL (ImB, "    (CORBA.ORB.To_CORBA_String (""RootPOA""))));");

            Generate (Impl_Package);
         end;
      end Produce_Interface;

      procedure Generate_Node
        (N : Node_Id;
         Buffer : in out Unbounded_Wide_String) is
      begin
         case Node_Kind (N) is

            -----------------------------------------------------------
            -- Nodes that generate an Ada representation into Buffer --
            -----------------------------------------------------------

            --  Scoped names

            when N_Scoped_Name =>
               if Present (Prefix (N)) then
                  if Node_Kind (Prefix (N)) = N_Scoped_Name then
                     Generate_Node (Prefix (N), Buffer);
                     Append (Buffer, ".");
                  end if;
               end if;

               declare
                  Name_Element : Wide_String
                    := Get_Name (N);
                  Prev_Underscore : Boolean := False;
               begin
                  -- XXX This code should be factored with that of Current_Ada_Name.
                  for I in Name_Element'Range loop
                     if Prev_Underscore and then Name_Element (I) = '_' then
                        Name_Element (I) := 'U';
                        Prev_Underscore := False;
                     end if;
                     Prev_Underscore := (Name_Element (I) = '_');
                  end loop;
                  Append (Buffer, Name_Element);
               end;

            --  Keywords

            when
              N_Keyword_Default |
              N_Keyword_Void    =>
               Append (Buffer, Nkind'Wide_Image (Node_Kind (N)));

            when N_Keyword_In    =>
               Append (Buffer, "in");
            when N_Keyword_Out   =>
               Append (Buffer, "out");
            when N_Keyword_Inout =>
               Append (Buffer, "in out");

            --  Base types

            when N_Base_Type_Char =>
               Append (Buffer, "CORBA.Char");

            when N_Base_Type_Boolean =>
               Append (Buffer, "CORBA.Boolean");

            when N_Base_Type_Long =>
               Append (Buffer, "CORBA.Long");

            when N_Base_Type_Double =>
               Append (Buffer, "CORBA.Double");

            when N_Base_Type_Unsigned_Long =>
               Append (Buffer, "CORBA.Unsigned_Long");

            when N_Base_Type_Long_Long =>
               Append (Buffer, "CORBA.Long_Long");

            when N_Base_Type_Long_Double =>
               Append (Buffer, "CORBA.Long_Double");

            when N_Base_Type_Unsigned_Long_Long =>
               Append (Buffer, "CORBA.Unsigned_Long_Long");

            when N_Base_Type_String =>
               Append (Buffer, "CORBA.String");

            when N_Base_Type_Octet =>
               Append (Buffer, "CORBA.Octet");

            ---------------------
            -- Other terminals --
            ---------------------

            -- Actually generated in N_Scoped_Name.
            -- when N_Absolute =>
            --    -- XXX Put (File, "::");

            when N_Preprocessor_Include =>
               -- XXX NOT USED!
               if Unit_Used (N) then
                  -- XXX Put (File, "#include """);
                  -- XXX Put_Name (N);
                  -- XXX | Put_Line (File, """");
                  null;
               end if;

            -------------------------------------------------------------
            -- Nodes that produce code into the various proxy packages --
            -------------------------------------------------------------

            when N_Specification =>
               declare
                  Def : Node_Id := First (CIAO.IDL_Syntax.Definitions (N));
                  Int : Node_Id := First (Interfaces (N));
               begin
                  while Present (Def) loop
                     Generate_Node (Def, Buffer);
                     Def := Next (Def);
                  end loop;

                  while Present (Int) loop
                     Generate_Node (Int, Buffer);
                     Int := Next (Int);
                  end loop;
               end;

            when N_Module =>
               declare
                  Def       : Node_Id := First (CIAO.IDL_Syntax.Definitions (N));
                  Interface : Node_Id := First (Interfaces (N));
                  My_Name   : constant Wide_String := Get_Name (N);
               begin
                  Push (My_Name);

                  if Depth = 1 then
                     --  This is a "root" <module>, i. e. one which is declared
                     --  directly within a <specification>, and which embodies
                     --  the translation of a library unit.

                     --  The RACWs_Package contains the declaration of one canonical
                     --  RACW for each interface declared in the module that corresponds
                     --  to an actual DSA distributed object type.
                     RACWs_Package := New_Package ("RACW_" & Current_Ada_Name);
                     Add_Wide_With (RACWs_Package.Unit_Spec,
                                    My_Name (My_Name'First + 4 .. My_Name'Last));
                     --  Strip DSA_ prefix.

                     --  The RACWs package depends on the declaration of the
                     --  actual DSA objet types.
                     NL (RACWs_Package.Unit_Spec);
                     PL (RACWs_Package.Unit_Spec, "pragma Remote_Types;");
                     Set_Empty (RACWs_Package.Unit_Spec);

                     --  The Convert_Package contains converter subprograms for the various
                     --  types declared in the module.
                     Convert_Package := New_Package (Current_Ada_Name & ".Convert");
                  else
                     NL (RACWs_Package.Unit_Spec);
                     Append (RACWs_Package.Unit_Spec, "package " & My_Name & " is");
                     II (RACWs_Package.Unit_Spec);
                  end if;

                  while Present (Def) loop
                     Generate_Node (Def, Buffer);
                     Def := Next (Def);
                  end loop;

                  while Present (Interface) loop
                     Generate_Node (Interface, Buffer);
                     Interface := Next (Interface);
                  end loop;

                  if Depth = 1 then
                     NL (RACWs_Package.Unit_Spec);
                     NL (RACWs_Package.Unit_Body);
                     Generate (RACWs_Package);
                     NL (Convert_Package.Unit_Spec);
                     NL (Convert_Package.Unit_Body);
                     Generate (Convert_Package);
                  else
                     DI (RACWs_Package.Unit_Spec);
                     Append (RACWs_Package.Unit_Spec, "end " & My_Name & ";");
                  end if;
                  Pop;
              end;

            when N_Interface =>
               Generate_Node (Specific_Interface (N), Buffer);

            when N_Interface_Dcl =>
               declare
                  My_Name   : constant Wide_String := Get_Name (Interface_Header (N));
                  Initial_Depth : Integer;
               begin
                  Push (My_Name);

                  Initial_Depth := Depth;
                  if Is_Remote_Subprograms (N) then
                     if Depth = 1 then
                        --  This is a "root" <interface>, i. e. one which is declared
                        --  directly within a <specification>, and which embodies
                        --  the translation of a (RCI) library unit.

                        --  The RACWs_Package contains the declaration of one canonical
                        --  RACW for each interface declared in the module that corresponds
                        --  to an actual DSA distributed object type.
                        RACWs_Package := New_Package ("RACW_" & Current_Ada_Name);
                        Add_Wide_With (RACWs_Package.Unit_Spec,
                                       My_Name (My_Name'First + 4 .. My_Name'Last));
                        --  Strip DSA_ prefix.

                        --  The RACWs package depends on the declaration of the
                        --  actual DSA objet types.
                        Append (RACWs_Package.Unit_Spec, "pragma Remote_Types;");
                        Set_Empty (RACWs_Package.Unit_Spec);

                        --  The Convert_Package contains converter subprograms for the various
                        --  types declared in the module.
                        Convert_Package := New_Package (Current_Ada_Name & ".Convert");
                     else
                        NL (RACWs_Package.Unit_Spec);
                        Append (RACWs_Package.Unit_Spec, "package " & My_Name & " is");
                        II (RACWs_Package.Unit_Spec);
                     end if;
                  end if;

                  Produce_Interface (N);

                  if Is_Remote_Subprograms (N) then
                     pragma Assert (Depth = Initial_Depth);
                     if Initial_Depth = 1 then
                        NL (RACWs_Package.Unit_Spec);
                        NL (RACWs_Package.Unit_Body);
                        Generate (RACWs_Package);
                        NL (Convert_Package.Unit_Spec);
                        NL (Convert_Package.Unit_Body);
                        Generate (Convert_Package);
                     else
                        DI (RACWs_Package.Unit_Spec);
                        Append (RACWs_Package.Unit_Spec, "end " & My_Name & ";");
                        NL (RACWs_Package.Unit_Spec);
                     end if;
                  end if;
               end;

               Pop;

            when N_Forward_Dcl =>
               --  Nothing to do.
               null;

            when N_Interface_Header =>
               null;

            when N_Type_Dcl =>
               Produce_Type_Converters (N, Origin (N));
               --  Generate conversion functions for the
               --  newly defined type.

            when
              N_Type_Declarator |
              N_Member          =>
               -- XXX Do nothing for now
               raise Program_Error;

            when N_Declarator =>
               -- Generate_Node (Specific_Declarator (N), Buffer);
               -- XXX Do nothing for now
               null;

            when
              N_Simple_Declarator |
              N_Enumerator        =>
               -- XXX Put_Name (N);
               null;
            when N_Simple_Type_Spec  =>
               -- XXX Check that!
               -- XXX Untyped traversal! Node1 is either <scoped_name> or <template_type_spec>.
               Generate_Node (Node1 (N), Buffer);

            when N_Type_Spec =>
               Generate_Node (Specific_Type_Spec (N), Buffer);

            when N_Constr_Type_Spec =>
               Generate_Node (Structure (N), Buffer);

            when N_Struct_Type =>
               declare
                  Member : Node_Id := First (Members (N));
               begin
                  -- XXX TODO
                  pragma Assert (False);

                  while Present (Member) loop
                     Generate_Node (Member, Buffer);
                     Member := Next (Member);
                  end loop;
                  Indent_Level := Indent_Level - 1;
               end;

            when N_Union_Type =>
               -- XXX TODO
               pragma Assert (False);
               null;

            when N_Case_Element =>
               -- XXX TODO
               raise Program_Error;

            when N_Element_Spec =>
               -- XXX TODO
               raise Program_Error;

            when N_Enum_Type =>
               declare
                  Enumerator : Node_Id := First (Enumerators (N));
               begin
                  -- XXX TODO
                  pragma Assert (False);

                  -- XXX Put_Name (N);
                  -- XXX | Put_Line (File, " {");
                  Indent_Level := Indent_Level + 1;
                  while Present (Enumerator) loop
                     -- XXX | Put_Indent;
                     Generate_Node (Enumerator, Buffer);
                     Enumerator := Next (Enumerator);
                     if Present (Enumerator) then
                        -- XXX Put (File, ",");
                        null;
                     end if;
                     -- XXX | NL (File);
                  end loop;
                  Indent_Level := Indent_Level - 1;
                  -- XXX | Put_Indent;
                  -- XXX Put (File, "}");
               end;

            when N_Sequence_Type =>
               -- XXX Put (File, "sequence<");
               Generate_Node (Specific_Type_Spec (N), Buffer);
               -- XXX Put (File, ">");

            when N_Array_Declarator =>
               declare
                  Array_Size : Node_Id := First (Fixed_Array_Sizes (N));
               begin
                  -- XXX Put_Name (N);

                  while Present (Array_Size) loop
                     -- XXX Put (File, "[");
                     -- XXX Put (File, Unbiased_Uint'Image (Size_Value (Array_Size)));
                     -- XXX Put (File, "]");
                     Array_Size := Next (Array_Size);
                  end loop;
               end;

            when N_Op_Dcl =>
               declare
                  Ada_Declaration : constant Asis.Declaration := Origin (N);

                  type Parameter_Profile_Ptr is access Asis.Parameter_Specification_List;
                  procedure Free is new Ada.Unchecked_Deallocation
                    (Asis.Parameter_Specification_List,
                     Parameter_Profile_Ptr);

                  Profile_Buffer : Unbounded_Wide_String;

                  Parameter_Profile : Parameter_Profile_Ptr
                    := null;

                  Result_Profile : Asis.Element := Nil_Element;
                  Param_Dcl : Node_Id := First (Param_Dcls (N));
                  Op_Name : constant Wide_String
                    := Get_Name (N);
                  Is_Function : constant Boolean
                    := Node_Kind (Operation_Value_Type (Op_Type_Spec (N))) /= N_Keyword_Void;

               begin
                  --  <op_attribute> NOT IMPLEMENTED.
                  if Is_Function then
                     Profile_Buffer := To_Unbounded_Wide_String ("function ");
                  else
                     Profile_Buffer := To_Unbounded_Wide_String ("procedure ");
                  end if;
                  Append (Profile_Buffer, Op_Name & " (Self : access Object");

                  while Present (Param_Dcl) loop
                     Generate_Node (Param_Dcl, Profile_Buffer);
                     With_Type
                       (Param_Type_Spec (Param_Dcl),
                        Impl_Package.Unit_Spec);
                     With_Convert_For_Type
                       (Param_Type_Spec (Param_Dcl),
                        Impl_Package.Unit_Body);
                     Param_Dcl := Next (Param_Dcl);
                  end loop;
                  Append (Profile_Buffer, ")");
                  if Is_Function then
                     Append (Profile_Buffer, " return ");
                     Generate_Node (Op_Type_Spec (N), Profile_Buffer);
                     With_Convert_For_Type (Op_Type_Spec (N), Impl_Package.Unit_Body);
                  end if;

                  NL (Impl_Package.Unit_Spec);
                  PL (Impl_Package.Unit_Spec, To_Wide_String (Profile_Buffer) & ";");

                  NL (Impl_Package.Unit_Body);
                  PL (Impl_Package.Unit_Body, To_Wide_String (Profile_Buffer) & " is");

                  PL (Impl_Package.Unit_Body, "begin");
                  II (Impl_Package.Unit_Body);

                  PL (Impl_Package.Unit_Body, "declare");
                  II (Impl_Package.Unit_Body);

                  case Declaration_Kind (Ada_Declaration) is
                     when A_Procedure_Declaration =>
                        Add_Wide_With (Impl_Package.Unit_Body,
                                  Unit_Full_Name (Enclosing_Compilation_Unit (Ada_Declaration)));
                        Parameter_Profile := new Asis.Parameter_Specification_List'
                          (Asis.Declarations.Parameter_Profile (Ada_Declaration));

                     when A_Function_Declaration =>
                        Add_Wide_With (Impl_Package.Unit_Body,
                                  Unit_Full_Name (Enclosing_Compilation_Unit (Ada_Declaration)));
                        Parameter_Profile := new Asis.Parameter_Specification_List'
                          (Asis.Declarations.Parameter_Profile (Ada_Declaration));
                        Result_Profile := Asis.Declarations.Result_Profile (Ada_Declaration);

                     when An_Ordinary_Type_Declaration =>
                        --  A remote access to subprogram declaration.
                        case Access_Type_Kind (Type_Declaration_View (Ada_Declaration)) is
                           when
                             An_Access_To_Procedure           |
                             An_Access_To_Protected_Procedure =>
                              Parameter_Profile := new Parameter_Specification_List'
                                (Access_To_Subprogram_Parameter_Profile
                                 (Type_Declaration_View (Ada_Declaration)));
                           when
                             An_Access_To_Function           |
                             An_Access_To_Protected_Function =>
                              Parameter_Profile := new Parameter_Specification_List'
                                (Access_To_Subprogram_Parameter_Profile
                                 (Type_Declaration_View (Ada_Declaration)));
                              Result_Profile := Access_To_Function_Result_Profile
                                (Type_Declaration_View (Ada_Declaration));
                           when others =>
                              -- XXX Error cannot happen
                              raise Program_Error;
                        end case;

                     when
                       A_Constant_Declaration          |
                       A_Deferred_Constant_Declaration |
                       An_Integer_Number_Declaration   |
                       A_Real_Number_Declaration       =>
                        --  A constant declaration.
                        Result_Profile := Asis.Definitions.Subtype_Mark
                          (Object_Declaration_View (Ada_Declaration));
                        -- XXX A constant of a record or array type ???
                     when others =>
                        -- XXX Error cannot happen
                        raise Program_Error;
                  end case;

                  if Parameter_Profile /= null then
                     declare
                        P : Parameter_Specification_List
                          renames Parameter_Profile.all;
                        Implicit_Self_Parameter : Asis.Element
                          := Nil_Element;
                        Subprogram_Name : constant Asis.Defining_Name_List
                          := Names (Ada_Declaration);

                        First_Parameter : Boolean := True;
                     begin
                        if Declaration_Kind (Ada_Declaration) in
                          A_Procedure_Declaration .. A_Function_Declaration
                          and then not (Is_Remote_Subprograms (Parent (N))) then
                           --  This interface corresponds to a real
                           --  distributed object type: every operation
                           --  has an implicit Self parameter.
                           declare
                              Controlling_Formals : constant Asis.Parameter_Specification_List
                                := Controlling_Formal_Parameters (Ada_Declaration);
                           begin
                              Implicit_Self_Parameter
                                := Controlling_Formals (Controlling_Formals'First);
                           end;
                        end if;

                        --  The invocation syntagm for the subprogram is constructed
                        --  in Buffer.
                        Buffer := To_Unbounded_Wide_String
                          (Ada_Full_Name (Ada_Declaration));

                        for I in P'Range loop
                           declare
                              N : constant Asis.Defining_Name_List
                                := Names (P (I));
                              Parameter_Name : constant Asis.Program_Text
                                := Isolated_Element_Image (N (N'First));
                           begin
                              if First_Parameter then
                                 First_Parameter := False;
                                 Append (Buffer, " (");
                              else
                                 Append (Buffer, ", ");
                              end if;

                              --  Append (Buffer, Parameter_Name & " => Ada_" & Parameter_Name);
                              --  Named parameter associations cause
                              --  problems with DSA in GNAT 3.12p and
                              --  earlier (GNAT bug 7112-004).
                              Append (Buffer, "Ada_" & Parameter_Name);
                              if Is_Identical (Implicit_Self_Parameter, P (I)) then

                                 --  The actual for the implicit Self parameter is
                                 --  of a derived type of its first subtype.

                                 Add_Wide_With (Impl_Package.Unit_Body,
                                           "RACW_DSA_" &
                                           Unit_Full_Name (Enclosing_Compilation_Unit
                                                           (Corresponding_Entity_Name_Definition
                                                            (Declaration_Subtype_Mark (P (I))))));

                                 if Trait_Kind (P (I)) /= An_Access_Definition_Trait
                                 then
                                    --  This parameter is not an access parameter.

                                    Append (Buffer, ".all");
                                 end if;

                                 PL (Impl_Package.Unit_Body,
                                         "Self_ObjectId : constant PortableServer.ObjectId");
                                 PL (Impl_Package.Unit_Body,
                                         "  := PortableServer.POA.Servant_To_Id (POA, PortableServer.Servant (Self));");

                                 PL (Impl_Package.Unit_Body,
                                     "Ada_" & Parameter_Name & " : constant RACW_DSA_" &
                                     Ada_Full_Name
                                     (Corresponding_Entity_Name_Declaration
                                      (Declaration_Subtype_Mark (P (I)))) &
                                     " := ObjectId_To_RACW (Self_ObjectId);");
                              elsif Is_Controlling_Formal (P (I)) then
                                 --  ...the same goes for other (explicit) controlling
                                 --  formal parameters.
                                 Add_Wide_With (Impl_Package.Unit_Body,
                                           "RACW_DSA_" &
                                           Unit_Full_Name (Enclosing_Compilation_Unit
                                                           (Corresponding_Entity_Name_Definition
                                                            (Declaration_Subtype_Mark (P (I))))));

                                 Append (Impl_Package.Unit_Body,
                                         "Ada_" & Parameter_Name & " : constant RACW_" &
                                         Ada_Full_Name
                                         (Corresponding_Entity_Name_Declaration
                                          (Declaration_Subtype_Mark (P (I)))) &
                                         " := To_Ada (" & Parameter_Name & ");");
                              else
                                 declare
                                    Ada_Type_Name : constant Asis.Program_Text
                                      := Ada_Full_Name
                                      (Corresponding_Entity_Name_Declaration
                                       (Declaration_Subtype_Mark (P (I))));
                                 begin
                                    Append (Impl_Package.Unit_Body,
                                            "Ada_" & Parameter_Name & " : constant " &
                                            Ada_Type_Name & " := " &
                                            Ada_Type_Name & " (To_Ada (" & Parameter_Name & "));");
                                 end;
                              end if;
                           end;
                        end loop;

                        if not First_Parameter then
                           Append (Buffer, ")");
                        end if;
                        Append (Buffer, ";");
                     end;
                  end if;

                  DI (Impl_Package.Unit_Body);
                  Append (Impl_Package.Unit_Body, "begin");
                  II (Impl_Package.Unit_Body);

                  if Is_Function then
                     PL (Impl_Package.Unit_Body, "declare");
                     II (Impl_Package.Unit_Body);
                     declare
                        Invocation : constant Unbounded_Wide_String := Buffer;
                     begin
                        PL (Impl_Package.Unit_Body, "Result : constant " &
                                Ada_Full_Name (Corresponding_Entity_Name_Declaration (Result_Profile)));
                        PL (Impl_Package.Unit_Body, "  := " & To_Wide_String (Invocation));
                     end;
                     DI (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "begin");
                     II (Impl_Package.Unit_Body);

                     PL (Impl_Package.Unit_Body, "declare");
                     II (Impl_Package.Unit_Body);
                     Buffer := To_Unbounded_Wide_String ("CORBA_Result : constant ");
                     Generate_Node (Op_Type_Spec (N), Buffer);
                     Append (Buffer, " := To_CORBA (Result);");
                     PL (Impl_Package.Unit_Body, To_Wide_String (Buffer));
                     DI (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "begin");
                     II (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "return CORBA_Result;");
                     DI (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "end;");

                     DI (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "exception");
                     II (Impl_Package.Unit_Body);
                     -- XXX TODO exceptions for conversion of result
                     PL (Impl_Package.Unit_Body, "when others => raise;");
                     DI (Impl_Package.Unit_Body);
                     PL (Impl_Package.Unit_Body, "end;");
                  else
                     -- TODO invoke procedure
                     PL (Impl_Package.Unit_Body, To_Wide_String (Buffer));
                  end if;

                  DI (Impl_Package.Unit_Body);
                  PL (Impl_Package.Unit_Body, "exception");
                  II (Impl_Package.Unit_Body);
                  -- XXX TODO exceptions for execution of the request
                  PL (Impl_Package.Unit_Body, "when others => raise;");
                  DI (Impl_Package.Unit_Body);
                  PL (Impl_Package.Unit_Body, "end;");

                  DI (Impl_Package.Unit_Body);
                  PL (Impl_Package.Unit_Body, "exception");
                  II (Impl_Package.Unit_Body);
                  -- XXX TODO exceptions for conversion of parameters
                  PL (Impl_Package.Unit_Body, "when others => raise;");
                  DI (Impl_Package.Unit_Body);
                  PL (Impl_Package.Unit_Body, "end " & Op_Name & ";");

                  -- Generate_Node (Raises_Expr (N));
                  --  <context_expr> NOT IMPLEMENTED.
               end;

            when N_Op_Type_Spec =>
               Generate_Node (Operation_Value_Type (N), Buffer);

            when N_Param_Type_Spec =>
               -- XXX ugly abstraction violation! Untyped traversal.
               Generate_Node (Node1 (N), Buffer);

            when N_Param_Dcl =>
               declare
                  Param_Name : constant Wide_String
                    := Get_Name (N);
               begin
                  Append (Buffer, "; " & Param_Name & " : ");
                  Generate_Node (Parameter_Attribute (N), Buffer);
                  Append (Buffer, " ");
                  Generate_Node (Param_Type_Spec (N), Buffer);
               end;

            when N_Param_Attribute =>
               -- XXX ugly abstraction violation! Untyped traversal.
               Generate_Node (Node1 (N), Buffer);

            when N_Raises_Expr =>
               -- XXX TODO
               raise Program_Error;

            when others =>
               --  Impossible, should not happen!
               --  (N_Empty, N_Error, N_Unused_At_Start).
               --   /XXXXXX            raise Program_Error;
               pragma Assert (False);
               null;

         end case;
      end Generate_Node;

      Dummy : Unbounded_Wide_String;

   begin
      Generate_Node (Tree, Dummy);
   end Generate;

end CIAO.Generator.Proxy;
