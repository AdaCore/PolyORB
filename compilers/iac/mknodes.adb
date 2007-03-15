------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              M K N O D E S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;  use Ada.Command_Line;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Table;
with GNAT.OS_Lib;       use GNAT.OS_Lib;
with GNAT.Directory_Operations;

with Errors;    use Errors;
with Flags;     use Flags;
with Lexer;     use Lexer;
with Locations; use Locations;
with Namet;     use Namet;
with Output;    use Output;
with Outfiles;  use Outfiles;
with Types;     use Types;

procedure Mknodes is

   Debug     : Boolean := False;
   Optimized : Boolean := False;

   function GNS (N : Name_Id) return String renames Get_Name_String;

   Module_Name : Name_Id;

   type Color_Type is new Int;
   No_Color : constant Color_Type := 0;

   type Declaration_Type is new Byte;
   Present : constant Declaration_Type := 0;
   Missing : constant Declaration_Type := 1;

   type Color_Array is array (Color_Type range <>) of Boolean;

   package Nodes is

      type Node_Kind is
        (K_Boolean,
         K_Octet,
         K_Long,
         K_Interface_Declaration,
         K_Typedef,
         K_Attribute,
         K_None);

      function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id;

      function Kind (N : Node_Id) return Node_Kind;
      function Loc (N : Node_Id) return Location;

      function Identifier (N : Node_Id) return Name_Id;
      procedure Set_Identifier (N : Node_Id; V : Name_Id);

      function Type_Spec (N : Node_Id) return Node_Id;
      procedure Set_Type_Spec (N : Node_Id; V : Node_Id);

      function Scope_Entity (N : Node_Id) return Node_Id;
      procedure Set_Scope_Entity (N : Node_Id; V : Node_Id);

      function First_Entity (N : Node_Id) return Node_Id;
      procedure Set_First_Entity (N : Node_Id; V : Node_Id);

      function Last_Entity (N : Node_Id) return Node_Id;
      procedure Set_Last_Entity (N : Node_Id; V : Node_Id);

      function Next_Entity (N : Node_Id) return Node_Id;
      procedure Set_Next_Entity (N : Node_Id; V : Node_Id);

   private

      type Node_Type is record
         Kind         : Node_Kind;
         Loc          : Location;
         Identifier   : Name_Id;
         Type_Spec    : Node_Id;
         Scope_Entity : Node_Id;
         First_Entity : Node_Id;
         Last_Entity  : Node_Id;
         Next_Entity  : Node_Id;
      end record;

      Default_Node : constant Node_Type :=
        (K_None,
         No_Location,
         No_Name,
         No_Node,
         No_Node,
         No_Node,
         No_Node,
         No_Node);

      package Entries is
         new GNAT.Table (Node_Type, Node_Id, No_Node + 1, 100, 10);

   end Nodes;

   use Nodes;

   Type_Prefix : constant String := "T ";
   Attr_Prefix : constant String := "A ";

   type Node_Array is array (Natural range <>) of Node_Id;

   Base_Types  : array (K_Boolean .. K_Long) of Node_Id;
   --  Nodes corresponding for the Pseudo-IDL base types

   First_Attribute : Node_Id := No_Node;
   Last_Attribute  : Node_Id := No_Node;
   N_Attributes    : Natural := 0;

   First_Interface : Node_Id := No_Node;
   Last_Interface  : Node_Id := No_Node;
   N_Interfaces    : Natural := 0;

   procedure Add_Attribute_To_Interface
     (Attribute : Node_Id;
      Iface     : Node_Id);
   --  Add attribute into interface using First_Entity, Last_Entity of
   --  Interfaces and Next_Entity of Attributes.

   function Is_Attribute_In_Interface
     (Attribute : Node_Id;
      Iface     : Node_Id) return Boolean;
   --  Look for attribute through a depth exploration of the
   --  inheritance spec of interface.

   function Inheritance_Tree (I : Node_Id) return Node_Array;
   --  Return the inheritance tree. The oldest ancestors are
   --  first. The interface itself is last.

   function Has_Attribute (I : Node_Id) return Boolean;
   --  Return True when interface I has at least on attribute

   function Base_Kind (T : Node_Id) return Node_Kind;
   --  As interfaces are represented as Node_Id and as Node_Id is
   --  represented as a long integer, the base type of an interface is
   --  K_Long. For defined types, return the kind of T base type.

   function Color (N : Node_Id) return Color_Type;
   procedure Set_Color (N : Node_Id; V : Color_Type);
   --  To allocate a slot for an attribute in a base type array, we
   --  use classical coloration algorithm. The base types are also
   --  colored to store the greatest color used for them.

   function Declaration (N : Node_Id) return Declaration_Type;
   procedure Set_Declaration (N : Node_Id; V : Declaration_Type);
   --  To avoid multiple declarations in spec and body.

   procedure Assign_Color_To_Attribute (Attribute : Node_Id);
   --  Compute adjacent attributes that is attributes included in the
   --  same interfaces than Attribute. Then find a color not already
   --  assigned to these adjacent attributes.

   procedure Declare_Attribute (A : Node_Id);
   --  Declare an attribute A

   procedure Declare_Type (N : Node_Id);
   --  Declare a type N

   procedure Mark_As_Incompatible (A : Node_Id; B : Node_Id);
   --  Mark the attributes A and B as incomatible. Incompatible
   --  atribues are attributes belonging to a same
   --  interface. Therefore, they cannot have the same color. The
   --  order of passed parameters is with no importance for this
   --  procedure.

   function Are_Incompatible (A : Node_Id; B : Node_Id) return Boolean;
   --  Return True if the attribute A and B have been marked as
   --  incompatible. The order of passed parameters is with no
   --  importance for this function.

   function Resolve_Type  (N : Name_Id) return Node_Id;
   --  Return a type of name N

   function P_Attribute return Node_Id;
   function P_Definition return Node_Id;
   function P_Interface return Node_Id;
   function P_Typedef return Node_Id;
   --  Parsing routines

   procedure W_Pragma_Assert  (Attribute : Node_Id);
   procedure W_Attribute_Body (A : String; N : String; T : String);
   procedure W_Attribute_Spec (A : String; N : String; T : String);
   procedure W_Attribute_Body (A : Node_Id);
   procedure W_Attribute_Spec (A : Node_Id);
   procedure W_Indentation    (N : Natural := 1);
   procedure W_Comment_Message;
   procedure W_Package_Body;
   procedure W_Package_Spec;
   procedure W_Subprogram_Call
     (I   : Natural;
      F   : String;
      PN1 : String := No_Str;
      PN2 : String := No_Str;
      PN3 : String := No_Str;
      PN4 : String := No_Str;
      PN5 : String := No_Str);
   procedure W_Subprogram_Signature
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str);
   procedure W_Subprogram_Declaration
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str);
   procedure W_Subprogram_Definition
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str);
   procedure W_Subprogram_Definition_End
     (I   : Natural;
      F   : String);
   procedure W_Table_Access   (N : Character; A : String);
   procedure W_Type_Attribute (K : Node_Kind);
   procedure W_Type_Attribute (A : String; T : String);
   procedure W_With           (P : String);
   --  Output routines (operating on current output stream)

   subtype String_3 is String (1 .. 3);
   function Output_File_Name
      (Source_File_Name : Name_Id; Extension : String_3) return Name_Id;
   --  Return Source_File_Name with trailing "idl" extension replaced with
   --  the indicated new extension.

   function Quote (S : String) return String;
   function Set   (S : String) return String;
   function W     (S : String) return String;
   --  Formatting routines

   package body Nodes is

      use Entries;

      ------------------
      -- First_Entity --
      ------------------

      function First_Entity (N : Node_Id) return Node_Id is
      begin
         return Table (N).First_Entity;
      end First_Entity;

      ----------------
      -- Identifier --
      ----------------

      function Identifier (N : Node_Id) return Name_Id is
      begin
         return Table (N).Identifier;
      end Identifier;

      ----------
      -- Kind --
      ----------

      function Kind (N : Node_Id) return Node_Kind is
      begin
         return Table (N).Kind;
      end Kind;

      -----------------
      -- Last_Entity --
      -----------------

      function Last_Entity (N : Node_Id) return Node_Id is
      begin
         return Table (N).Last_Entity;
      end Last_Entity;

      ---------
      -- Loc --
      ---------

      function Loc (N : Node_Id) return Location is
      begin
         return Table (N).Loc;
      end Loc;

      --------------
      -- New_Node --
      --------------

      function New_Node (Kind : Node_Kind; Loc : Location) return Node_Id is
         Node : Node_Id;
      begin
         Increment_Last;
         Node := Entries.Last;
         Table (Node) := Default_Node;
         Table (Node).Kind := Kind;
         Table (Node).Loc  := Loc;

         return Node;
      end New_Node;

      -----------------
      -- Next_Entity --
      -----------------

      function Next_Entity (N : Node_Id) return Node_Id is
      begin
         return Table (N).Next_Entity;
      end Next_Entity;

      ------------------
      -- Scope_Entity --
      ------------------

      function Scope_Entity (N : Node_Id) return Node_Id is
      begin
         return Table (N).Scope_Entity;
      end Scope_Entity;

      ----------------------
      -- Set_First_Entity --
      ----------------------

      procedure Set_First_Entity (N : Node_Id; V : Node_Id) is
      begin
         Table (N).First_Entity := V;
      end Set_First_Entity;

      --------------------
      -- Set_Identifier --
      --------------------

      procedure Set_Identifier (N : Node_Id; V : Name_Id) is
      begin
         Table (N).Identifier := V;
      end Set_Identifier;

      ---------------------
      -- Set_Last_Entity --
      ---------------------

      procedure Set_Last_Entity (N : Node_Id; V : Node_Id) is
      begin
         Table (N).Last_Entity := V;
      end Set_Last_Entity;

      ---------------------
      -- Set_Next_Entity --
      ---------------------

      procedure Set_Next_Entity (N : Node_Id; V : Node_Id) is
      begin
         Table (N).Next_Entity := V;
      end Set_Next_Entity;

      ----------------------
      -- Set_Scope_Entity --
      ----------------------

      procedure Set_Scope_Entity (N : Node_Id; V : Node_Id) is
      begin
         Table (N).Scope_Entity := V;
      end Set_Scope_Entity;

      -------------------
      -- Set_Type_Spec --
      -------------------

      procedure Set_Type_Spec (N : Node_Id; V : Node_Id) is
      begin
         Table (N).Type_Spec := V;
      end Set_Type_Spec;

      ---------------
      -- Type_Spec --
      ---------------

      function Type_Spec (N : Node_Id) return Node_Id is
      begin
         return Table (N).Type_Spec;
      end Type_Spec;

   end Nodes;

   --------------------------------
   -- Add_Attribute_To_Interface --
   --------------------------------

   procedure Add_Attribute_To_Interface
     (Attribute : Node_Id;
      Iface     : Node_Id) is
   begin
      --  Attribute nodes are contiguous. There is no need to chain them

      if First_Entity (Iface) = No_Node then
         Set_First_Entity (Iface, Attribute);
      end if;
      Set_Last_Entity (Iface, Attribute);
   end Add_Attribute_To_Interface;

   ----------------------
   -- Are_Incompatible --
   ----------------------

   function Are_Incompatible (A : Node_Id; B : Node_Id) return Boolean is
      Name_A : constant Name_Id := Identifier (A);
      Name_B : constant Name_Id := Identifier (B);
   begin
      Set_Str_To_Name_Buffer (Attr_Prefix);
      Get_Name_String_And_Append (Name_A);
      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (Name_B);

      if Get_Name_Table_Byte (Name_Find) = 1 then
         return True;
      end if;

      Set_Str_To_Name_Buffer (Attr_Prefix);
      Get_Name_String_And_Append (Name_B);
      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (Name_A);

      if Get_Name_Table_Byte (Name_Find) = 1 then
         return True;
      end if;

      return False;
   end Are_Incompatible;

   -------------------------------
   -- Assign_Color_To_Attribute --
   -------------------------------

   procedure Assign_Color_To_Attribute (Attribute : Node_Id) is
      Kind : constant Node_Kind := Base_Kind (Type_Spec (Attribute));
      Used : Color_Array (No_Color .. Color_Type (N_Attributes));
      Attr : Node_Id;
      Name : constant Name_Id := Identifier (Attribute);
      Intf : Node_Id;

   begin
      if Debug then
         Write_Str  ("--  Assign color to ");
         Write_Name (Identifier (Attribute));
         Write_Eol;
      end if;

      Used := (others => False);

      Attr := First_Attribute;

      while Attr /= No_Node loop
         if Identifier (Attr) = Name then
            Intf := Scope_Entity (Attr);

            if Debug then
               Write_Str ("--   Found attribute in ");
               Write_Name (Identifier (Intf));
               Write_Eol;
            end if;

            while Intf /= No_Node loop
               --  Mark adjacent attributes as incompatible. Attribute
               --  A2 is adjacent to attribute A1 when A2 and A1
               --  belong to a common interface. To do this we must:

               --  1 - Traverse the list of the parent interfaces and
               --  get all the attributes of these parents.

               --  2 - Traverse the list of all the child interfaces
               --  and get all the attributes of these children.
               --  However this kind of traversal is very complex to
               --  perform because the child interfaces do not form a
               --  list but a tree. We use the following workaround
               --  that has the same effect: each time we find a
               --  couple of incompatible attributes, we mark this
               --  couple.

               if Has_Attribute (Intf) then
                  for Neighbor in First_Entity (Intf) .. Last_Entity (Intf)
                  loop
                     --  Mark the two attributes as incompatible

                     Mark_As_Incompatible (Attribute, Neighbor);
                  end loop;
               end if;

               Intf := Type_Spec (Intf);
            end loop;
         end if;

         Attr := Next_Entity (Attr);
      end loop;

      --  Second pass to complete the work. We search all the
      --  attributes that are incompatible with `Attribute' and we set
      --  their color as used. Note that the number of such attributes
      --  is *greater than* or eaqual the number of attributes marked
      --  in the previous phase. It could be greater beacause
      --  attributes belonging to child interfaces could be handled
      --  before `Attribute'.

      Attr := First_Attribute;

      while Attr /= No_Node loop
         if Are_Incompatible (Attribute, Attr) then
            if Debug then
               Write_Str ("--     Conflict with ");
               Write_Name (Identifier (Attr));
               Write_Str (" from ");
               Write_Name (Identifier (Scope_Entity (Attr)));
               Write_Str (" ");
               Write_Int (Int (Color (Attr)));
               Write_Eol;
            end if;

            Used (Color (Attr)) := True;
         end if;

         Attr := Next_Entity (Attr);
      end loop;

      --  Find a color not used by adjacent attributes

      for C in No_Color + 1 .. Used'Last loop
         if not Used (C) then
            Set_Color (Attribute, C);

            if Color (Base_Types (Kind)) < C then
               Set_Color (Base_Types (Kind), C);
            end if;

            if Debug then
               Write_Str ("--   Decide to assign ");
               Write_Int (Int (C));
               Write_Str (" to ");
               Write_Name (Identifier (Attribute));
               Write_Eol;
            end if;

            exit;
         end if;
      end loop;
   end Assign_Color_To_Attribute;

   ---------------
   -- Base_Kind --
   ---------------

   function Base_Kind (T : Node_Id) return Node_Kind is
   begin
      case Kind (T) is
         when K_Interface_Declaration =>
            return K_Long;
         when K_Boolean .. K_Long =>
            return Kind (T);
         when K_Typedef =>
            return Base_Kind (Type_Spec (T));
         when others =>
            raise Program_Error;
      end case;
   end Base_Kind;

   -----------
   -- Color --
   -----------

   function Color (N : Node_Id) return Color_Type is
   begin
      return Color_Type (Get_Name_Table_Info (Identifier (N)));
   end Color;

   -----------------
   -- Declaration --
   -----------------

   function Declaration (N : Node_Id) return Declaration_Type is
   begin
      return Declaration_Type (Get_Name_Table_Byte (Identifier (N)));
   end Declaration;

   -----------------------
   -- Declare_Attribute --
   -----------------------

   procedure Declare_Attribute (A : Node_Id) is
      Type_Name : constant Name_Id := Identifier (Type_Spec (A));
      Attr_Name : constant Name_Id := Identifier (A);
      Attribute : Node_Id;
   begin
      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         if Identifier (Attribute) = Attr_Name then
            if Identifier (Type_Spec (Attribute)) /= Type_Name then
               Error_Loc (1) := Loc (A);
               DE ("attribute type inconsistency");
               return;
            end if;
         end if;

         Attribute := Next_Entity (Attribute);
      end loop;

      if First_Attribute = No_Node then
         First_Attribute := A;
      end if;

      if Last_Attribute /= No_Node then
         Set_Next_Entity (Last_Attribute, A);
      end if;

      Last_Attribute := A;
      N_Attributes := N_Attributes + 1;
   end Declare_Attribute;

   ------------------
   -- Declare_Type --
   ------------------

   procedure Declare_Type (N : Node_Id) is
   begin
      Set_Str_To_Name_Buffer (Type_Prefix);
      case Kind (N) is
         when K_Boolean =>
            Add_Str_To_Name_Buffer (Image (T_Boolean));
         when K_Octet =>
            Add_Str_To_Name_Buffer (Image (T_Octet));
         when K_Long =>
            Add_Str_To_Name_Buffer (Image (T_Long));
         when others =>
            Get_Name_String_And_Append (Identifier (N));
      end case;

      Set_Name_Table_Info (Name_Find, Int (N));

      case Kind (N) is
         when K_Boolean =>
            Set_Str_To_Name_Buffer ("Boolean");
            Set_Identifier (N, Name_Find);
         when K_Octet =>
            Set_Str_To_Name_Buffer ("Byte");
            Set_Identifier (N, Name_Find);
         when K_Long =>
            Set_Str_To_Name_Buffer ("Int");
            Set_Identifier (N, Name_Find);
         when others =>
            null;
      end case;
   end Declare_Type;

   -------------------
   -- Has_Attribute --
   -------------------

   function Has_Attribute (I : Node_Id) return Boolean is
   begin
      return First_Entity (I) /= No_Node;
   end Has_Attribute;

   ----------------------
   -- Inheritance_Tree --
   ----------------------

   function Inheritance_Tree (I : Node_Id) return Node_Array is
      Depth  : Natural := 0;
      Parent : Node_Id;
   begin
      pragma Assert (Kind (I) = K_Interface_Declaration);

      Parent := I;

      while Parent /= No_Node loop
         Depth := Depth + 1;
         Parent := Type_Spec (Parent);
      end loop;

      declare
         Tree : Node_Array (1 .. Depth);
      begin
         Parent := I;

         for D in reverse Tree'Range loop
            Tree (D) := Parent;
            Parent := Type_Spec (Parent);
         end loop;

         return Tree;
      end;
   end Inheritance_Tree;

   -------------------------------
   -- Is_Attribute_In_Interface --
   -------------------------------

   function Is_Attribute_In_Interface
     (Attribute : Node_Id;
      Iface     : Node_Id)
     return Boolean
   is
      N : constant Name_Id := Identifier (Attribute);
      I : Node_Id := Iface;
   begin
      while I /= No_Node loop
         if Has_Attribute (I) then
            for A in First_Entity (I) .. Last_Entity (I) loop
               if Identifier (A) = N then
                  return True;
               end if;
            end loop;
         end if;
         I := Type_Spec (I);
      end loop;

      return False;
   end Is_Attribute_In_Interface;

   --------------------------
   -- Mark_As_Incompatible --
   --------------------------

   procedure Mark_As_Incompatible (A : Node_Id; B : Node_Id) is
      Name_A : constant Name_Id := Identifier (A);
      Name_B : constant Name_Id := Identifier (B);
   begin
      Set_Str_To_Name_Buffer (Attr_Prefix);
      Get_Name_String_And_Append (Name_A);
      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (Name_B);
      Set_Name_Table_Byte (Name_Find, 1);

      Set_Str_To_Name_Buffer (Attr_Prefix);
      Get_Name_String_And_Append (Name_B);
      Add_Char_To_Name_Buffer (' ');
      Get_Name_String_And_Append (Name_A);
      Set_Name_Table_Byte (Name_Find, 1);
   end Mark_As_Incompatible;

   ----------------------
   -- Output_File_Name --
   ----------------------

   function Output_File_Name
      (Source_File_Name : Name_Id; Extension : String_3) return Name_Id is
   begin
      Get_Name_String (Source_File_Name);
      declare
         Base_Name : constant String :=
                       GNAT.Directory_Operations.Base_Name
                         (Path   => Name_Buffer (1 .. Name_Len),
                          Suffix => "idl");
      begin
         Name_Len := Base_Name'Length + 3;
         Name_Buffer (1 .. Base_Name'Length)            := Base_Name;
         Name_Buffer (Base_Name'Length + 1 .. Name_Len) := Extension;
      end;
      return Name_Find;
   end Output_File_Name;

   -----------------
   -- P_Attribute --
   -----------------

   function P_Attribute return Node_Id is
      Attribute : Node_Id;
      Type_Spec : Node_Id;
   begin
      --  Parse type specifier

      Scan_Token;
      Type_Spec := Resolve_Type (Token_Name);

      if Type_Spec = No_Node then
         Error_Loc (1) := Token_Location;
         DE ("unknown type");
         return No_Node;
      end if;

      Attribute := New_Node (K_Attribute, Token_Location);
      Set_Type_Spec (Attribute, Type_Spec);

      --  Parse identifier

      Scan_Token (T_Identifier);

      if Token = T_Error then
         return No_Node;
      end if;

      Set_Identifier (Attribute, Token_Name);

      Declare_Attribute (Attribute);

      Scan_Token (T_Semi_Colon);

      if Token = T_Error then
         return No_Node;
      end if;

      return Attribute;
   end P_Attribute;

   ------------------
   -- P_Definition --
   ------------------

   function P_Definition return Node_Id is
      Definition : Node_Id := No_Node;
      State      : Location;
   begin
      Save_Lexer (State);
      Scan_Token ((T_Typedef, T_Interface));
      case Token is
         when T_Typedef =>
            Restore_Lexer (State);
            Definition := P_Typedef;

         when T_Interface =>
            Restore_Lexer (State);
            Definition := P_Interface;

         when others =>
            null;
      end case;

      if Definition /= No_Node then
         Save_Lexer (State);
         Scan_Token (T_Semi_Colon);

         if Token /= T_Semi_Colon then
            Definition := No_Node;
         end if;
      end if;

      if Definition = No_Node then
         Restore_Lexer (State);
         Skip_Declaration (T_Semi_Colon);
      end if;

      return Definition;
   end P_Definition;

   -----------------
   -- P_Interface --
   -----------------

   function P_Interface return Node_Id is
      Iface     : Node_Id;
      Attribute : Node_Id;
      Type_Spec : Node_Id;
   begin
      Scan_Token; --  past "interface"
      Iface := New_Node (K_Interface_Declaration, Token_Location);

      --  Parse identifier

      Scan_Token (T_Identifier);

      if Token = T_Error then
         return No_Node;
      end if;

      Set_Identifier (Iface, Token_Name);

      if Resolve_Type (Identifier (Iface)) /= No_Node then
         Error_Loc (1) := Token_Location;
         DE ("interface already defined");
         return No_Node;
      end if;

      if First_Interface = No_Node then
         First_Interface := Iface;
      end if;

      if Last_Interface /= No_Node then
         Set_Next_Entity (Last_Interface, Iface);
      end if;

      Last_Interface := Iface;
      N_Interfaces := N_Interfaces + 1;

      Scan_Token ((T_Left_Brace, T_Colon));

      if Token = T_Error then
         return No_Node;
      end if;

      if Token = T_Colon then
         --  Parse interface inheritance spec

         Scan_Token (T_Identifier);

         if Token = T_Error then
            return No_Node;
         end if;

         Type_Spec := Resolve_Type (Token_Name);

         if Type_Spec = No_Node
           or else Kind (Type_Spec) /= K_Interface_Declaration
         then
            Error_Loc (1) := Token_Location;
            DE ("unknown interface");
            return No_Node;
         end if;

         Set_Type_Spec (Iface, Type_Spec);

         Scan_Token (T_Left_Brace);

         if Token = T_Error then
            return No_Node;
         end if;
      end if;

      Declare_Type (Iface);

      loop
         case Next_Token is
            when T_Identifier
              | T_Boolean
              | T_Octet
              | T_Long =>
               Attribute := P_Attribute;

               if Is_Attribute_In_Interface (Attribute, Iface) then
                  Error_Loc (1) := Loc (Attribute);
                  DE ("attribute already defined");
                  return No_Node;
               end if;

               Set_Scope_Entity (Attribute, Iface);
               Add_Attribute_To_Interface (Attribute, Iface);

            when T_Right_Brace =>
               Scan_Token;
               exit;

            when others =>
               return No_Node;
         end case;
      end loop;

      return Iface;
   end P_Interface;

   ---------------
   -- P_Typedef --
   ---------------

   function P_Typedef return Node_Id is
      Type_Spec : Node_Id;
      Type_Decl : Node_Id;

   begin
      Scan_Token; --  past "typedef"
      Type_Decl := New_Node (K_Typedef, Token_Location);

      --  Parse type spec

      Scan_Token ((T_Identifier, T_Boolean, T_Octet, T_Long));

      if Token = T_Error then
         return No_Node;
      end if;

      Type_Spec := Resolve_Type (Token_Name);

      if Type_Spec = No_Node then
         Error_Loc (1) := Token_Location;
         DE ("unknown type");
         return No_Node;
      end if;

      Set_Type_Spec (Type_Decl, Type_Spec);

      --  Parse identifier

      Scan_Token (T_Identifier);

      if Token = T_Error then
         return No_Node;
      end if;

      Set_Identifier (Type_Decl, Token_Name);

      if Resolve_Type (Identifier (Type_Decl)) /= No_Node then
         Error_Loc (1) := Token_Location;
         DE ("type already defined");
         return No_Node;
      end if;

      Declare_Type (Type_Decl);

      return Type_Decl;
   end P_Typedef;

   -----------
   -- Quote --
   -----------

   function Quote (S : String) return String is
   begin
      return """" & S & """";
   end Quote;

   ------------------
   -- Resolve_Type --
   ------------------

   function Resolve_Type (N : Name_Id) return Node_Id is
      Result : Node_Id;
   begin
      Set_Str_To_Name_Buffer (Type_Prefix);
      Get_Name_String_And_Append (N);

      Result := Node_Id (Get_Name_Table_Info (Name_Find));

      if Result = No_Node or else Kind (Result) = K_Attribute then
         return No_Node;
      end if;

      return Result;
   end Resolve_Type;

   ---------
   -- Set --
   ---------

   function Set (S : String) return String is
   begin
      return "Set_" & S;
   end Set;

   ---------------
   -- Set_Color --
   ---------------

   procedure Set_Color (N : Node_Id; V : Color_Type) is
   begin
      Set_Name_Table_Info (Identifier (N), Int (V));
   end Set_Color;

   ---------------------
   -- Set_Declaration --
   ---------------------

   procedure Set_Declaration (N : Node_Id; V : Declaration_Type) is
   begin
      Set_Name_Table_Byte (Identifier (N), Byte (V));
   end Set_Declaration;

   -------
   -- W --
   -------

   function W (S : String) return String is
   begin
      return "W_" & S;
   end W;

   ----------------------
   -- W_Attribute_Body --
   ----------------------

   procedure W_Attribute_Body (A : Node_Id) is
      K  : Node_Kind;
      NS : Node_Id;
   begin
      NS := Scope_Entity (A);
      while Type_Spec (NS) /= No_Node loop
         NS := Type_Spec (NS);
      end loop;
      K := Base_Kind (Type_Spec (A));
      W_Subprogram_Definition
        (1, GNS (Identifier (A)),
         'N', GNS (Identifier (NS)),
         ' ', GNS (Identifier (Type_Spec (A))));
      W_Pragma_Assert (A);
      W_Indentation (2);
      Write_Str  ("return ");
      Write_Name (Identifier (Type_Spec (A)));
      Write_Str  (" (");
      Set_Char_To_Name_Buffer (Node_Kind'Image (K) (3));
      Add_Str_To_Name_Buffer  (" (");
      Add_Nat_To_Name_Buffer  (Int (Color (A)));
      Add_Char_To_Name_Buffer (')');
      W_Table_Access ('N', GNS (Name_Find));
      Write_Line (");");
      W_Subprogram_Definition_End (1, GNS (Identifier (A)));
      Write_Eol;

      W_Subprogram_Definition
        (1, Set (GNS (Identifier (A))),
         'N', GNS (Identifier (NS)),
         'V', GNS (Identifier (Type_Spec (A))));
      W_Pragma_Assert (A);
      W_Indentation (2);
      Set_Char_To_Name_Buffer (Node_Kind'Image (K)(3));
      Add_Str_To_Name_Buffer  (" (");
      Add_Nat_To_Name_Buffer  (Int (Color (A)));
      Add_Char_To_Name_Buffer (')');
      W_Table_Access ('N', GNS (Name_Find));
      Write_Str  (" := ");
      Write_Name (Identifier (Base_Types (K)));
      Write_Line (" (V);");
      W_Subprogram_Definition_End (1, Set (GNS (Identifier (A))));
      Write_Eol;
   end W_Attribute_Body;

   ----------------------
   -- W_Attribute_Body --
   ----------------------

   procedure W_Attribute_Body (A : String; N : String; T : String) is
   begin
      W_Subprogram_Definition (1, A, 'N', N, ' ', T);
      W_Indentation (2);
      Write_Str  ("return ");
      W_Table_Access ('N', A);
      Write_Line (";");
      W_Subprogram_Definition_End (1, A);
      Write_Eol;

      W_Subprogram_Definition (1, Set (A), 'N', N, 'V', T);
      W_Indentation (2);
      W_Table_Access ('N', A);
      Write_Line (" := V;");
      W_Subprogram_Definition_End (1, Set (A));
      Write_Eol;
   end W_Attribute_Body;

   ----------------------
   -- W_Attribute_Spec --
   ----------------------

   procedure W_Attribute_Spec (A : Node_Id) is
      NS   : Node_Id;
   begin
      NS := Scope_Entity (A);

      while Type_Spec (NS) /= No_Node loop
         NS := Type_Spec (NS);
      end loop;

      W_Attribute_Spec
        (GNS (Identifier (A)),
         GNS (Identifier (NS)),
         GNS (Identifier (Type_Spec (A))));
   end W_Attribute_Spec;

   ----------------------
   -- W_Attribute_Spec --
   ----------------------

   procedure W_Attribute_Spec (A : String; N : String; T : String) is
   begin
      W_Subprogram_Declaration (1, A, 'N', N, ' ', T);
      W_Subprogram_Declaration (1, Set (A), 'N', N, 'V', T);
   end W_Attribute_Spec;

   -----------------------
   -- W_Comment_Message --
   -----------------------

   procedure W_Comment_Message is
   begin
      Write_Line ("--  This file has been generated automatically"
                 & " by `mknodes'. Do not");
      Write_Line ("--  hand modify this file since your changes"
                 & " will be overridden.");
      Write_Eol;
   end W_Comment_Message;

   -------------------
   -- W_Indentation --
   -------------------

   procedure W_Indentation (N : Natural := 1) is
   begin
      for I in 1 .. N loop
         Write_Str ("   ");
      end loop;
   end W_Indentation;

   --------------------
   -- W_Package_Body --
   --------------------

   procedure W_Package_Body is
      Attribute : Node_Id;
      Iface     : Node_Id;
      Base_Type : Node_Id;
   begin
      W_Comment_Message;

      Get_Name_String (Module_Name);
      Name_Buffer (Name_Len - 4 .. Name_Len) := "Debug";
      W_With (Name_Buffer (1 .. Name_Len));
      Write_Eol;
      Write_Str ("package body ");
      Write_Name (Module_Name);
      Write_Line (" is");
      Write_Eol;
      W_Indentation;
      Write_Line ("pragma Warnings (Off);");
      W_Indentation;
      Write_Line ("use Entries;");
      Write_Eol;

      W_Attribute_Body ("Kind", "Node_Id", "Node_Kind");
      W_Attribute_Body ("Loc", "Node_Id", "Location");

      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         Set_Declaration (Attribute, Missing);
         Attribute := Next_Entity (Attribute);
      end loop;

      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         if Declaration (Attribute) = Missing then
            W_Attribute_Body (Attribute);
            Set_Declaration (Attribute, Present);
         end if;

         Attribute := Next_Entity (Attribute);
      end loop;

      W_Subprogram_Definition
        (1, W ("Node"), 'N', "Node_Id");
      W_Indentation (2);

      if Optimized then
         Write_Line ("null;");

      else
         Write_Line ("case Kind (N) is");
         Iface := First_Interface;

         while Iface /= No_Node loop
            if Type_Spec (Iface) /= No_Node then
               W_Indentation (3);
               Write_Str  ("when K_");
               Write_Name (Identifier (Iface));
               Write_Line (" =>");
               Base_Type := Iface;

               while Type_Spec (Base_Type) /= No_Node loop
                  Base_Type := Type_Spec (Base_Type);
               end loop;

               W_Subprogram_Call
                 (4, W (GNS (Identifier (Iface))),
                  GNS (Identifier (Base_Type)) & " (N)");
            end if;

            Iface := Next_Entity (Iface);
         end loop;

         W_Indentation (3);
         Write_Line ("when others =>");
         W_Indentation (4);
         Write_Line ("null;");
         W_Indentation (2);
         Write_Line ("end case;");
      end if;

      W_Subprogram_Definition_End (1, W ("Node"));
      Write_Eol;

      if not Optimized then
         Iface := First_Interface;

         while Iface /= No_Node loop
            if Type_Spec (Iface) /= No_Node then
               Base_Type := Iface;

               while Type_Spec (Base_Type) /= No_Node loop
                  Base_Type := Type_Spec (Base_Type);
               end loop;

               W_Subprogram_Definition
                 (1, W (GNS (Identifier (Iface))),
                  'N', GNS (Identifier (Base_Type)));

               W_Subprogram_Call
                 (2, W ("Node_Header"), "Node_Id (N)");

               Attribute := First_Attribute;

               while Attribute /= No_Node loop
                  Set_Declaration (Attribute, Missing);
                  Attribute := Next_Entity (Attribute);
               end loop;

               Attribute := First_Attribute;

               while Attribute /= No_Node loop
                  if Declaration (Attribute) = Missing
                    and then Is_Attribute_In_Interface (Attribute, Iface)
                  then
                     if Kind (Type_Spec (Attribute)) =
                       K_Interface_Declaration
                     then
                        W_Subprogram_Call
                          (2, W ("Node_Attribute"),
                           Quote (GNS (Identifier (Attribute))),
                           Quote (GNS (Identifier (Type_Spec (Attribute)))),
                           "Image (" & GNS (Identifier (Attribute)) & " (N))",
                           "Int (" & GNS (Identifier (Attribute)) & " (N))");
                     else
                        W_Subprogram_Call
                          (2, W ("Node_Attribute"),
                           Quote (GNS (Identifier (Attribute))),
                           Quote (GNS (Identifier (Type_Spec (Attribute)))),
                           "Image (" & GNS (Identifier (Attribute)) & " (N))");
                     end if;

                     Set_Declaration (Attribute, Present);
                  end if;

                  Attribute := Next_Entity (Attribute);
               end loop;

               W_Subprogram_Definition_End
                 (1, W (GNS (Identifier (Iface))));
               Write_Eol;
            end if;

            Iface := Next_Entity (Iface);
         end loop;
      end if;

      Write_Str  ("end ");
      Write_Name (Module_Name);
      Write_Line (";");
   end W_Package_Body;

   --------------------
   -- W_Package_Spec --
   --------------------

   procedure W_Package_Spec is
      Iface     : Node_Id;
      Attribute : Node_Id;
   begin
      W_Comment_Message;

      Write_Line ("with GNAT.Table;");

      --  The packages Locations and Types may have been included by a
      --  parent package of the generated package (or may not). We
      --  disable a warning generated when enabling the GNAT style
      --  checks.

      Write_Line ("pragma Warnings (Off);");
      Write_Line ("with Locations; use Locations;");
      Write_Line ("with Types;     use Types;");
      Write_Line ("pragma Warnings (On);");
      Write_Eol;
      Write_Str  ("package ");
      Write_Name (Module_Name);
      Write_Line (" is");
      Write_Eol;

      --  Describe Node_Kind type (all interfaces)

      W_Indentation;
      Write_Line  ("type Node_Kind is");
      W_Indentation;
      Write_Str ("  (");
      Iface := First_Interface;

      while Iface /= No_Node loop
         Write_Str  ("K_");
         Write_Name (Identifier (Iface));

         if Iface = Last_Interface then
            Write_Line (");");
         else
            Write_Line (",");
            W_Indentation (2);
         end if;

         Iface := Next_Entity (Iface);
      end loop;

      Write_Eol;

      --  Describe interface attributes

      Iface := First_Interface;

      while Iface /= No_Node loop
         --  Output a description of interface

         W_Indentation;
         Write_Line ("--");
         W_Indentation;
         Write_Str  ("--  ");
         Write_Name (Identifier (Iface));
         Write_Eol;
         W_Indentation;
         Write_Line ("--");

         declare
            Tree : constant Node_Array := Inheritance_Tree (Iface);
         begin
            for I in Tree'Range loop
               if Has_Attribute (Tree (I)) then
                  for A in First_Entity (Tree (I)) .. Last_Entity (Tree (I))
                  loop
                     W_Indentation;
                     Write_Str  ("--    ");
                     Get_Name_String (Identifier (A));
                     Name_Buffer (Name_Len + 1 .. 25) := (others => ' ');
                     Write_Str   (Name_Buffer (1 .. 25));
                     Write_Str   (": ");
                     Write_Name (Identifier (Type_Spec (A)));
                     Write_Eol;
                  end loop;
               end if;
            end loop;

            W_Indentation;
            Write_Line ("--");
            Write_Eol;

            --  Output signature of interface output when this is not
            --  a basic interface.

            if not Optimized then
               if Tree'Length > 1 then
                  W_Subprogram_Declaration
                    (1, W (GNS (Identifier (Iface))),
                     'N', GNS (Identifier (Tree (Tree'First))));
                  Write_Eol;
               end if;
            end if;
         end;
         Iface := Next_Entity (Iface);
      end loop;

      --  Describe attribute accessors

      W_Attribute_Spec ("Kind", "Node_Id", "Node_Kind");
      Write_Eol;
      W_Attribute_Spec ("Loc", "Node_Id", "Location");
      Write_Eol;

      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         Set_Declaration (Attribute, Missing);
         Attribute := Next_Entity (Attribute);
      end loop;

      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         if Declaration (Attribute) = Missing then
            W_Attribute_Spec (Attribute);
            Write_Eol;
            Set_Declaration (Attribute, Present);
         end if;

         Attribute := Next_Entity (Attribute);
      end loop;

      W_Subprogram_Declaration
        (1, W ("Node"), 'N', "Node_Id");
      Write_Eol;

      --  Describe slot table types

      for K in K_Boolean .. K_Long loop
         W_Indentation (1);
         Write_Str  ("type ");
         Write_Name (Identifier (Base_Types (K)));
         Write_Str  ("_Array is array (1 .. ");
         Write_Int  (Int (Color (Base_Types (K))));
         Write_Str  (") of ");
         Write_Name (Identifier (Base_Types (K)));
         Write_Line (";");
      end loop;

      Write_Eol;

      --  Describe Node_Entry type and its attributes

      W_Indentation;
      Write_Line  ("type Node_Entry is record");
      W_Type_Attribute ("Kind", "Node_Kind");

      for K in K_Boolean .. K_Long loop
         if Color (Base_Types (K)) > 0 then
            W_Type_Attribute (K);
         end if;
      end loop;

      W_Type_Attribute ("Loc", "Location");
      W_Indentation;
      Write_Line  ("end record;");
      Write_Eol;

      --  Provide a default node

      W_Indentation;
      Write_Line  ("Default_Node : constant Node_Entry :=");
      W_Indentation;
      Write_Line  ("  (Node_Kind'First,");

      for K in K_Boolean .. K_Long loop
         if Color (Base_Types (K)) > 0 then
            W_Indentation (2);
            Write_Str  ("(others => ");

            if K = K_Boolean then
               Write_Str ("False");
            else
               Write_Int (0);
            end if;

            Write_Line ("),");
         end if;
      end loop;

      W_Indentation (2);
      Write_Line  ("No_Location);");
      Write_Eol;

      --  Provide node table

      W_Indentation;
      Write_Line ("package Entries is new GNAT.Table");
      W_Indentation;
      Write_Line ("  (Node_Entry, Node_Id, No_Node + 1, 1000, 100);");
      Write_Eol;
      Write_Str  ("end ");
      Write_Name (Module_Name);
      Write_Line (";");
   end W_Package_Spec;

   ---------------------
   -- W_Pragma_Assert --
   ---------------------

   procedure W_Pragma_Assert (Attribute : Node_Id) is
      Iface : Node_Id;
   begin
      W_Indentation (2);
      Write_Str     ("pragma Assert (False");
      Iface := First_Interface;

      while Iface /= No_Node loop
         if Is_Attribute_In_Interface (Attribute, Iface) then
            Write_Eol;
            W_Indentation (2);
            Write_Str  ("  or else ");
            W_Table_Access ('N', "Kind");
            Write_Str  (" = K_");
            Write_Name (Identifier (Iface));
         end if;

         Iface := Next_Entity (Iface);
      end loop;

      Write_Line (");");
      Write_Eol;
   end W_Pragma_Assert;

   -----------------------
   -- W_Subprogram_Call --
   -----------------------

   procedure W_Subprogram_Call
     (I   : Natural;
      F   : String;
      PN1 : String := No_Str;
      PN2 : String := No_Str;
      PN3 : String := No_Str;
      PN4 : String := No_Str;
      PN5 : String := No_Str) is
   begin
      W_Indentation (I);
      Write_Line (F);

      if PN1 /= No_Str then
         W_Indentation (I);
         Write_Str ("  (");
         Write_Str (PN1);
      end if;

      if PN2 /= No_Str then
         Write_Line (",");
         W_Indentation (I + 1);
         Write_Str (PN2);
      end if;

      if PN3 /= No_Str then
         Write_Line (",");
         W_Indentation (I + 1);
         Write_Str (PN3);
      end if;

      if PN4 /= No_Str then
         Write_Line (",");
         W_Indentation (I + 1);
         Write_Str (PN4);
      end if;

      if PN5 /= No_Str then
         Write_Line (",");
         W_Indentation (I + 1);
         Write_Str (PN5);
      end if;

      Write_Line (");");
   end W_Subprogram_Call;

   ------------------------------
   -- W_Subprogram_Declaration --
   ------------------------------

   procedure W_Subprogram_Declaration
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str) is
   begin
      W_Subprogram_Signature (I, F, PN1, PT1, PN2, PT2);
      Write_Line (";");
   end W_Subprogram_Declaration;

   -----------------------------
   -- W_Subprogram_Definition --
   -----------------------------

   procedure W_Subprogram_Definition
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str) is
   begin
      W_Subprogram_Signature (I, F, PN1, PT1, PN2, PT2);
      Write_Line (" is");
      W_Indentation (I);
      Write_Line ("begin");
   end W_Subprogram_Definition;

   ---------------------------------
   -- W_Subprogram_Definition_End --
   ---------------------------------

   procedure W_Subprogram_Definition_End
     (I   : Natural;
      F   : String) is
   begin
      W_Indentation (I);
      Write_Line ("end " & F & ";");
   end W_Subprogram_Definition_End;

   ----------------------------
   -- W_Subprogram_Signature --
   ----------------------------

   procedure W_Subprogram_Signature
     (I   : Natural;
      F   : String;
      PN1 : Character;
      PT1 : String;
      PN2 : Character := ' ';
      PT2 : String := No_Str) is
   begin
      W_Indentation (I);

      if PN2 = ' ' and then PT2 /= No_Str then
         Write_Str  ("function");
      else
         Write_Str  ("procedure");
      end if;

      Write_Str  (" ");
      Write_Str  (F);
      Write_Str  (" (");
      Write_Char (PN1);
      Write_Str  (" : ");
      Write_Str  (PT1);

      if PT2 = No_Str then
         Write_Char (')');
         return;
      end if;

      if PN2 = ' ' then
         Write_Str  (") return ");
         Write_Str  (PT2);
      else
         Write_Str  ("; ");
         Write_Char (PN2);
         Write_Str  (" : ");
         Write_Str  (PT2);
         Write_Char (')');
      end if;
   end W_Subprogram_Signature;

   --------------------
   -- W_Table_Access --
   --------------------

   procedure W_Table_Access (N : Character; A : String) is
   begin
      Write_Str  ("Table (Node_Id (");
      Write_Char (N);
      Write_Str  (")).");
      Write_Str  (A);
   end W_Table_Access;

   ----------------------
   -- W_Type_Attribute --
   ----------------------

   procedure W_Type_Attribute (A : String; T : String) is
   begin
      W_Indentation (2);
      Write_Str  (A);
      Write_Str  (" : ");
      Write_Str  (T);
      Write_Line (";");
   end W_Type_Attribute;

   ----------------------
   -- W_Type_Attribute --
   ----------------------

   procedure W_Type_Attribute (K : Node_Kind) is
   begin
      W_Indentation (2);
      Set_Str_To_Name_Buffer (Node_Kind'Image (K));
      Write_Char (Name_Buffer (3));
      Write_Str  (" : ");
      Write_Name (Identifier (Base_Types (K)));
      Write_Line ("_Array;");
   end W_Type_Attribute;

   ------------
   -- W_With --
   ------------

   procedure W_With (P : String) is
   begin
      Write_Line ("with " & P & "; use " & P & ";");
   end W_With;

   Source_File_Name  : Name_Id;
   Source_File       : File_Descriptor;
   Attribute         : Node_Id;
   Definition        : Node_Id;
   pragma Unreferenced (Definition); --  Because never read

begin

   --  Initialization step

   Namet.Initialize;

   loop
      case Getopt ("d O p") is
         when 'd' =>
            Debug := True;

         when 'O' =>
            Optimized := True;

         when 'p' =>
            Print_On_Stdout := True;

         when ASCII.NUL =>
            exit;

         when others =>
            raise Program_Error;
      end case;
   end loop;

   Set_Str_To_Name_Buffer (Get_Argument);
   if Name_Len = 0 then
      DE ("no file name");
      return;
   end if;

   Source_File_Name := Name_Find;

   if not Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
      Add_Str_To_Name_Buffer (".idl");

      if Is_Regular_File (Name_Buffer (1 .. Name_Len)) then
         Source_File_Name := Name_Find;
      else
         Error_Name (1) := Source_File_Name;
         DE ("% not found");
         return;
      end if;
   end if;

   Get_Name_String (Source_File_Name);

   if Name_Len < 4
        or else Name_Buffer (Name_Len - 3 .. Name_Len) /= ".idl"
   then
      DE ("source file name must end with "".idl""");
      return;
   end if;

   Name_Buffer (Name_Len + 1) := ASCII.NUL;

   Source_File := Open_Read (Name_Buffer'Address, Binary);

   --  Lexer step
   Lexer.Process (Source_File, Source_File_Name);

   for T in Base_Types'Range loop
      Base_Types (T) := New_Node (T, Token_Location);
      Declare_Type (Base_Types (T));
   end loop;

   Scan_Token (T_Module);

   if Token = T_Error then
      return;
   end if;

   Scan_Token (T_Identifier);

   if Token = T_Error then
      return;
   end if;

   Module_Name := Token_Name;

   while Next_Token = T_Colon_Colon loop
      Scan_Token;
      Scan_Token (T_Identifier);
      Get_Name_String (Module_Name);
      Add_Char_To_Name_Buffer ('.');
      Get_Name_String_And_Append (Token_Name);
      Module_Name := Name_Find;
   end loop;

   Scan_Token (T_Left_Brace);

   if Token = T_Error then
      return;
   end if;

   loop
      case Next_Token is
         when T_Right_Brace =>
            Scan_Token;
            exit;
         when T_EOF =>
            exit;
         when others =>
            Definition := P_Definition;
      end case;
   end loop;

   Scan_Token (T_Semi_Colon);

   if N_Errors > 0 then
      Error_Int (1) := N_Errors;
      Error_Int (2) := N_Warnings;

      if N_Warnings > 0 then
         DE ("$ error(s) and $ warning(s)");
      else
         DE ("$ error(s)");
      end if;

      Set_Exit_Status (Failure);
      return;

   elsif N_Warnings > 0 then
      Error_Int (1) := N_Warnings;
      DE ("$ warning(s)");
   end if;

   Attribute := First_Attribute;

   while Attribute /= No_Node loop
      Set_Color (Attribute, No_Color);
      Attribute := Next_Entity (Attribute);
   end loop;

   for K in K_Boolean .. K_Long loop
      Set_Color (Base_Types (K), No_Color);
      Attribute := First_Attribute;

      while Attribute /= No_Node loop
         if Base_Kind (Type_Spec (Attribute)) = K
           and then Color (Attribute) = No_Color
         then
            Assign_Color_To_Attribute (Attribute);
         end if;

         Attribute := Next_Entity (Attribute);
      end loop;
   end loop;

   declare
      Fd : File_Descriptor;
   begin
      Fd := Set_Output (Output_File_Name (Source_File_Name, "ads"));
      W_Package_Spec;
      Release_Output (Fd);

      Fd := Set_Output (Output_File_Name (Source_File_Name, "adb"));
      W_Package_Body;
      Release_Output (Fd);
   end;

end Mknodes;
