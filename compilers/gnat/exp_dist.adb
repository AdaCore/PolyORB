------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P_ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $LastChangedRevision$
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

with Atree;       use Atree;
with Einfo;       use Einfo;
with Elists;      use Elists;
with Exp_Hlpr;    use Exp_Hlpr;
with Exp_Strm;    use Exp_Strm;
with Exp_Tss;     use Exp_Tss;
with Exp_Util;    use Exp_Util;
with GNAT.HTable; use GNAT.HTable;
with Sinput;       use Sinput;
with Lib;         use Lib;
with Namet;       use Namet;
with Nlists;      use Nlists;
with Nmake;       use Nmake;
with Opt;         use Opt;
with Rtsfind;     use Rtsfind;
with Sem;         use Sem;
with Sem_Ch3;     use Sem_Ch3;
with Sem_Ch8;     use Sem_Ch8;
with Sem_Dist;    use Sem_Dist;
with Sem_Util;    use Sem_Util;
with Sinfo;       use Sinfo;
with Snames;      use Snames;
with Stand;       use Stand;
with Stringt;     use Stringt;
with Tbuild;      use Tbuild;
with Uintp;       use Uintp;
with Uname;       use Uname;

package body Exp_Dist is

   --  The following model has been used to implement distributed objects:
   --  given a designated type D and a RACW type R, then a record of the
   --  form:
   --    type Stub is tagged record
   --       [...declaration similar to s-parint.ads RACW_Stub_Type...]
   --    end record;
   --  is built. This type has two properties:
   --
   --    1) Since it has the same structure than RACW_Stub_Type, it can be
   --       converted to and from this type to make it suitable for
   --       System.Partition_Interface.Get_Unique_Remote_Pointer in order
   --       to avoid memory leaks when the same remote object arrive on the
   --       same partition by following different pathes
   --
   --    2) It also has the same dispatching table as the designated type D,
   --       and thus can be used as an object designated by a value of type
   --       R on any partition other than the one on which the object has
   --       been created, since only dispatching calls will be performed and
   --       the fields themselves will not be used. We call Derive_Subprograms
   --       to fake half a derivation to ensure that the subprograms do have
   --       the same dispatching table.

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Build_General_Calling_Stubs
     (Decls                     : in List_Id;
      Statements                : in List_Id;
      Target_Object             : in Node_Id;
      Subprogram_Id             : in Node_Id;
      Asynchronous              : in Node_Id := Empty;
      Is_Known_Asynchronous     : in Boolean := False;
      Is_Known_Non_Asynchronous : in Boolean := False;
      Is_Function               : in Boolean;
      Spec                      : in Node_Id;
      Stub_Type                 : in Entity_Id := Empty;
      RACW_Type                 : in Entity_Id := Empty;
      Nod                       : in Node_Id);
   --  Build calling stubs for general purpose. The parameters are:
   --    Decls             : a place to put declarations
   --    Statements        : a place to put statements
   --    Target_Object     : a node containing the target object
   --    Subprogram_Id     : a node containing the subprogram ID
   --    Asynchronous      : True if an APC must be made instead of an RPC.
   --                        The value needs not be supplied if one of the
   --                        Is_Known_... is True.
   --    Is_Known_Async... : True if we know that this is asynchronous
   --    Is_Known_Non_A... : True if we know that this is not asynchronous
   --    Spec              : a node with a Parameter_Specifications and
   --                        a Subtype_Mark if applicable
   --    Object_Type       : in case of a RACW, parameters of type access to
   --                        Object_Type will be marshalled using the
   --                        address of this object (the addr field) rather
   --                        than using the 'Write on the object itself
   --    Nod               : used to provide sloc for generated code

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Locator                  : Entity_Id := Empty;
      New_Name                 : Name_Id   := No_Name)
      return                     Node_Id;
   --  Build the calling stub for a given subprogram with the subprogram ID
   --  being Subp_Id. If Stub_Type is given, then the "addr" field of
   --  parameters of this type will be marshalled instead of the object
   --  itself. It will then be converted into Stub_Type before performing
   --  the real call. If Dynamically_Asynchronous is True, then it will be
   --  computed at run time whether the call is asynchronous or not.
   --  Otherwise, the value of the formal Asynchronous will be used.
   --  If Locator is not Empty, it will be used instead of RCI_Cache. If
   --  New_Name is given, then it will be used instead of the original name.

   function Build_Subprogram_Receiving_Stubs
     (Vis_Decl                 : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Parent_Primitive         : Entity_Id := Empty)
      return                     Node_Id;
   --  Build the receiving stub for a given subprogram. The subprogram
   --  declaration is also built by this procedure, and the value returned
   --  is a N_Subprogram_Body. If a parameter of type access to Stub_Type is
   --  found in the specification, then its address is read from the stream
   --  instead of the object itself and converted into an access to
   --  class-wide type before doing the real call using any of the RACW type
   --  pointing on the designated type.

   function Build_RPC_Receiver_Specification
     (RPC_Receiver      : Entity_Id;
      Request_Parameter : Entity_Id)
      return Node_Id;
   --  Make a subprogram specification for an RPC receiver,
   --  with the given defining unit name and formal parameters.

   procedure Build_RPC_Receiver_Body
     (RPC_Receiver :     Entity_Id;
      Request      : out Entity_Id;
      Subp_Id      : out Entity_Id;
      Stmts        : out List_Id;
      Decl         : out Node_Id);
   --  Make a subprogram body for an RPC receiver, with the given
   --  defining unit name. On return:
   --    - Subp_Id is the Standard.String variable that contains
   --      the identifier of the desired subprogram,
   --    - Stmts is the place where the request dispatching
   --      statements can occur,
   --    - Decl is the subprogram body declaration.

   function Build_Ordered_Parameters_List (Spec : Node_Id) return List_Id;
   --  Return an ordered parameter list: unconstrained parameters are put
   --  at the beginning of the list and constrained ones are put after. If
   --  there are no parameters, an empty list is returned.

   procedure Add_Calling_Stubs_To_Declarations
     (Pkg_Spec : in Node_Id;
      Decls    : in List_Id);
   --  Add calling stubs to the declarative part

   procedure Add_Receiving_Stubs_To_Declarations
     (Pkg_Spec : in Node_Id;
      Decls    : in List_Id);
   --  Add receiving stubs to the declarative part

   procedure Add_RAS_Dereference_TSS (N : in Node_Id);
   --  Add a subprogram body for RAS dereference

   procedure Add_RAS_Access_TSS (N : in Node_Id);
   --  Add a subprogram body for RAS Access attribute

   function Could_Be_Asynchronous (Spec : Node_Id) return Boolean;
   --  Return True if nothing prevents the program whose specification is
   --  given to be asynchronous (i.e. no out parameter).

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id;
   function Get_String_Id (Val : String) return String_Id;
   --  Ugly functions used to retrieve a package name. Inherited from the
   --  old exp_dist.adb and not rewritten yet ???

   function Pack_Entity_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Entity_Id;
      Etyp   : Entity_Id := Empty)
      return   Node_Id;
   --  Pack Object (of type Etyp) into Stream. If Etyp is not given,
   --  then Etype (Object) will be used if present. If the type is
   --  constrained, then 'Write will be used to output the object,
   --  If the type is unconstrained, 'Output will be used.

   function Pack_Node_Into_Stream
     (Loc    : Source_Ptr;
      Stream : Entity_Id;
      Object : Node_Id;
      Etyp   : Entity_Id)
      return   Node_Id;
   pragma Unreferenced (Pack_Node_Into_Stream);
   --  Similar to above, with an arbitrary node instead of an entity

   function Pack_Node_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Node_Id;
      Etyp   : Entity_Id)
      return   Node_Id;
   --  Similar to above, with Stream instead of Stream'Access

   function Scope_Of_Spec (Spec : Node_Id) return Entity_Id;
   --  Return the scope represented by a given spec

   function Need_Extra_Constrained (Parameter : Node_Id) return Boolean;
   --  Return True if the current parameter needs an extra formal to reflect
   --  its constrained status.

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id; Stub_Type : Entity_Id)
      return Boolean;
   --  Return True if the current parameter is a controlling formal argument
   --  of type Stub_Type or access to Stub_Type.

   procedure Set_Renaming_TSS
     (Typ     : Entity_Id;
      Nam     : Entity_Id;
      TSS_Nam : Name_Id);
   --  Create a renaming declaration of subprogram Nam,
   --  and register it as a TSS for Typ with name TSS_Nam.

   procedure Declare_Create_NVList
     (Loc    : Source_Ptr;
      NVList : Entity_Id;
      Decls  : List_Id;
      Stmts  : List_Id);
   --  Append the declaration of NVList to Decls, and its
   --  initialization to Stmts.

   function Parameter_Passing_Mode
     (Loc         : Source_Ptr;
      Parameter   : Entity_Id;
      Constrained : Boolean)
      return Node_Id;
   --  Return an expression that denotes the parameter passing
   --  mode to be used for Parameter in distribution stubs,
   --  where Constrained is Parameter's constrained status.

   function Add_Parameter_To_NVList
     (Loc         : Source_Ptr;
      NVList      : Entity_Id;
      Parameter   : Entity_Id;
      Constrained : Boolean;
      RACW_Ctrl   : Boolean := False;
      Any         : Entity_Id)
      return Node_Id;
   --  Return a call to Add_Item to add the Any corresponding
   --  to the designated formal Parameter (with the indicated
   --  Constrained status) to NVList. RACW_Ctrl must be set to
   --  True for controlling formals of distributed object primitive
   --  operations.

   type Stub_Structure is record
      Stub_Type            : Entity_Id;
      Stub_Type_Access     : Entity_Id;
      Object_RPC_Receiver  : Entity_Id;
      RACW_Type            : Entity_Id;
   end record;
   --  This structure is necessary because of the two phases analysis of
   --  a RACW declaration occurring in the same Remote_Types package as the
   --  designated type. RACW_Type is any of the RACW types pointing on this
   --  designated type, it is used here to save an anonymous type creation
   --  for each primitive operation.

   Empty_Stub_Structure : constant Stub_Structure :=
     (Empty, Empty, Empty, Empty);

   type Hash_Index is range 0 .. 46;
   function Hash (F : Entity_Id) return Hash_Index;
   function Hash (F : Name_Id) return Hash_Index;

   package Stubs_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Stub_Structure,
                         No_Element => Empty_Stub_Structure,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RACW designated type and its stub type

   package Asynchronous_Flags_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Node_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RACW type and the node holding the value True if
   --  the RACW is asynchronous and False otherwise.

   package RCI_Locator_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RCI package on which All_Calls_Remote applies and
   --  the generic instantiation of RCI_Locator for this package.

   package RCI_Calling_Stubs_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Entity_Id,
                         No_Element => Empty,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a RCI subprogram and the corresponding calling stubs

   package Subprogram_Identifier_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => String_Id,
                         No_Element => No_String,
                         Key        => Entity_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a remote subprogram and the corresponding
   --  subprogram identifier.

   package Overload_Counter_Table is
      new Simple_HTable (Header_Num => Hash_Index,
                         Element    => Int,
                         No_Element => 0,
                         Key        => Name_Id,
                         Hash       => Hash,
                         Equal      => "=");
   --  Mapping between a subprogram name and an integer that
   --  counts the number of defining subprogram names with that
   --  Name_Id encountered so far in a given context (an interface).

   procedure Add_Stub_Type
     (Designated_Type     : in Entity_Id;
      RACW_Type           : in Entity_Id;
      Decls               : in List_Id;
      Stub_Type           : out Entity_Id;
      Stub_Type_Access    : out Entity_Id;
      Object_RPC_Receiver : out Entity_Id;
      Existing            : out Boolean);
   --  Add the declaration of the stub type, the access to stub type and the
   --  object RPC receiver at the end of Decls. If these already exist,
   --  then nothing is added in the tree but the right values are returned
   --  anyhow and Existing is set to True.

   procedure Add_RACW_Read_Attribute
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Declarations        : in List_Id);
   --  Add Read attribute in Decls for the RACW type. The Read attribute
   --  is added right after the RACW_Type declaration while the body is
   --  inserted after Declarations.

   procedure Add_RACW_Write_Attribute
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Object_RPC_Receiver : in Entity_Id;
      Declarations        : in List_Id);
   --  Same thing for the Write attribute

   procedure Add_RACW_Read_Write_Attributes
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Object_RPC_Receiver : in Entity_Id;
      Declarations        : in List_Id);
   --  Add Read and Write attributes declarations and bodies for a given
   --  RACW type. The declarations are added just after the declaration
   --  of the RACW type itself, while the bodies are inserted at the end
   --  of Decls.

   procedure Add_RACW_From_Any
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Declarations        : in List_Id);
   --  Add the From_Any TSS for this RACW type.

   procedure Add_RACW_To_Any
     (Designated_Type  : in Entity_Id;
      RACW_Type        : in Entity_Id;
      Stub_Type        : in Entity_Id;
      Stub_Type_Access : in Entity_Id;
      Declarations     : in List_Id);
   --  Add the To_Any TSS for this RACW type.

   procedure Add_RACW_TypeCode
     (Designated_Type  : in Entity_Id;
      RACW_Type        : in Entity_Id;
      Declarations     : in List_Id);
   --  Add the TypeCode TSS for this RACW type.

   procedure Add_RAS_From_Any
     (RAS_Type     : in Entity_Id;
      Declarations : in List_Id);
   --  Add the From_Any TSS for this RAS type.

   procedure Add_RAS_To_Any
     (RAS_Type        : in Entity_Id;
      Declarations    : in List_Id);
   --  Add the To_Any TSS for this RAS type.

   procedure Add_RAS_TypeCode
     (RAS_Type        : in Entity_Id;
      Declarations    : in List_Id);
   --  Add the TypeCode TSS for this RAS type.

   procedure Assign_Subprogram_Identifier
     (Def :     Entity_Id;
      Id  : out String_Id);
   --  Determine the distribution subprogram identifier to
   --  be used for remote subprogram Def, return it in Id and
   --  store it in a hash table for later retrieval by
   --  Get_Subprogram_Identifier.

   procedure Reserve_NamingContext_Methods;
   --  Mark the method names for interface NamingContext as
   --  already used in the overload table, so no clashes
   --  occur with user code (RCIs implement the NamingContext
   --  interface to allow their methods to be accessed as
   --  objects (RAS)).

   function RCI_Package_Locator
     (Loc          : Source_Ptr;
      Package_Spec : Node_Id)
      return         Node_Id;
   --  Instantiate the generic package RCI_Locator in order to locate
   --  the RCI package whose spec is given as argument.

   function Make_Tag_Check (Loc : Source_Ptr; N : Node_Id) return Node_Id;
   --  Surround a node N by a tag check, as in:
   --      begin
   --         <N>;
   --      exception
   --         when E : Ada.Tags.Tag_Error =>
   --           Raise_Exception (Program_Error'Identity,
   --                            Exception_Message (E));
   --      end;

   function Input_With_Tag_Check
     (Loc      : Source_Ptr;
      Var_Type : Entity_Id;
      Stream   : Entity_Id)
      return Node_Id;
   pragma Unreferenced (Input_With_Tag_Check);
   --  Return a function with the following form:
   --    function R return Var_Type is
   --    begin
   --       return Var_Type'Input (S);
   --    exception
   --       when E : Ada.Tags.Tag_Error =>
   --           Raise_Exception (Program_Error'Identity,
   --                            Exception_Message (E));
   --    end R;

   ----------------------------------
   -- Assign_Subprogram_Identifier --
   ----------------------------------

   procedure Assign_Subprogram_Identifier
     (Def :     Entity_Id;
      Id  : out String_Id)
   is
      Sdef : Source_Ptr := Sloc (Def);
      Tdef : Source_Buffer_Ptr;
      N    : constant Name_Id := Chars (Def);

      Overload_Order : constant Int :=
        Overload_Counter_Table.Get (N) + 1;
   begin
      Overload_Counter_Table.Set (N, Overload_Order);

      Get_Name_String (N);

      if False and then not Is_Operator_Symbol_Name (N) then

         --  XXX THIS IS BORKEN because it gets called
         --  with a def that does not come_from_source.

         --  Not a defining_operator_name

         Tdef := Source_Text (Get_Source_File_Index (Sdef));
         for J in 1 .. Name_Len loop
            Name_Buffer (J) := Tdef (Sdef + Source_Ptr (J) - 1);
         end loop;

         if Name_Buffer (1) = 'O' then
            --  Do not risk a clash with a defining operator name.
            --  XXX this is an approximation, actually this change
            --  should be made /only/ if there is an actual clash.
            Name_Buffer (1) := 'o';
         end if;

      end if;

      --  Homonym handling: as in Exp_Dbug, but much simpler,
      --  because the only entities for which we have to generate
      --  names here need only to be disambiguated within their
      --  own scope.

      if Overload_Order > 1 then
         Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "__";
         Name_Len := Name_Len + 2;
         Add_Nat_To_Name_Buffer (Overload_Order);
      end if;

      Id := String_From_Name_Buffer;
      Subprogram_Identifier_Table.Set (Def, Id);
   end Assign_Subprogram_Identifier;

   ------------------------------------
   -- Local variables and structures --
   ------------------------------------

   RCI_Cache : Node_Id;

--    Output_From_Constrained : constant array (Boolean) of Name_Id :=
--      (False => Name_Output,
--       True  => Name_Write);
   --  The attribute to choose depending on the fact that the parameter
   --  is constrained or not. There is no such thing as Input_From_Constrained
   --  since this require separate mechanisms ('Input is a function while
   --  'Read is a procedure).

   ---------------------------------------
   -- Add_Calling_Stubs_To_Declarations --
   ---------------------------------------

   procedure Add_Calling_Stubs_To_Declarations
     (Pkg_Spec : in Node_Id;
      Decls    : in List_Id)
   is
      Current_Declaration       : Node_Id;

      Loc                       : constant Source_Ptr := Sloc (Pkg_Spec);

      RCI_Instantiation         : Node_Id;

      Subp_Stubs                : Node_Id;
      Subp_Str                  : String_Id;
   begin
      --  The first thing added is an instantiation of the generic package
      --  System.PolyORB_Interface.RCI_Locator with the name of the (current)
      --  remote package. This will act as an interface with the name server
      --  to determine the Partition_ID and the RPC_Receiver for the
      --  receiver of this package.

      RCI_Instantiation := RCI_Package_Locator (Loc, Pkg_Spec);
      RCI_Cache         := Defining_Unit_Name (RCI_Instantiation);

      Append_To (Decls, RCI_Instantiation);
      Analyze (RCI_Instantiation);

      --  For each subprogram declaration visible in the spec, we do
      --  build a body. We also increment a counter to assign a different
      --  Subprogram_Id to each subprograms. The receiving stubs processing
      --  do use the same mechanism and will thus assign the same Id and
      --  do the correct dispatching.

      Overload_Counter_Table.Reset;
      Reserve_NamingContext_Methods;

      Current_Declaration := First (Visible_Declarations (Pkg_Spec));

      while Current_Declaration /= Empty loop

         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            Assign_Subprogram_Identifier (
              Defining_Unit_Name (Specification (
                Current_Declaration)),
              Subp_Str);

            Subp_Stubs :=
              Build_Subprogram_Calling_Stubs (
                Vis_Decl     => Current_Declaration,
                Subp_Id      => Make_String_Literal (Loc, Subp_Str),
                Asynchronous =>
                  Nkind (Specification (Current_Declaration)) =
                    N_Procedure_Specification
                  and then
                    Is_Asynchronous (Defining_Unit_Name (Specification
                      (Current_Declaration))));

            Append_To (Decls, Subp_Stubs);
            Analyze (Subp_Stubs);
         end if;

         Next (Current_Declaration);
      end loop;

   end Add_Calling_Stubs_To_Declarations;

   -----------------------------
   -- Add_Parameter_To_NVList --
   -----------------------------

   function Add_Parameter_To_NVList
     (Loc         : Source_Ptr;
      NVList      : Entity_Id;
      Parameter   : Entity_Id;
      Constrained : Boolean;
      RACW_Ctrl   : Boolean := False;
      Any         : Entity_Id)
      return Node_Id
   is
      Parameter_Name_String : String_Id;
      Parameter_Mode : Node_Id;
   begin
      if Nkind (Parameter) = N_Defining_Identifier then
         Get_Name_String (Chars (Parameter));
      else
         Get_Name_String (Chars (Defining_Identifier
                                  (Parameter)));
      end if;
      Parameter_Name_String := String_From_Name_Buffer;

      if RACW_Ctrl then
         Parameter_Mode := New_Occurrence_Of
           (RTE (RE_Mode_In), Loc);
      else
         Parameter_Mode := Parameter_Passing_Mode (Loc,
           Parameter, Constrained);
      end if;

      return
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of
              (RTE (RE_NVList_Add_Item), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (NVList, Loc),
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of
                  (RTE (RE_To_PolyORB_String), Loc),
              Parameter_Associations => New_List (
                Make_String_Literal (Loc,
                  Strval => Parameter_Name_String))),
            New_Occurrence_Of (Any, Loc),
            Parameter_Mode));
   end Add_Parameter_To_NVList;

   -----------------------
   -- Add_RACW_Features --
   -----------------------

   procedure Add_RACW_Features (RACW_Type : in Entity_Id)
   is
      Desig : constant Entity_Id :=
                Etype (Designated_Type (RACW_Type));
      Decls : List_Id :=
                List_Containing (Declaration_Node (RACW_Type));

      Same_Scope : constant Boolean :=
                     Scope (Desig) = Scope (RACW_Type);

      Stub_Type           : Entity_Id;
      Stub_Type_Access    : Entity_Id;
      Object_RPC_Receiver : Entity_Id;
      Existing            : Boolean;

   begin
      if not Expander_Active then
         return;
      end if;

      if Same_Scope then

         --  We are declaring a RACW in the same package than its designated
         --  type, so the list to use for late declarations must be the
         --  private part of the package. We do know that this private part
         --  exists since the designated type has to be a private one.

         Decls := Private_Declarations
           (Package_Specification_Of_Scope (Current_Scope));

      elsif Nkind (Parent (Decls)) = N_Package_Specification
        and then Present (Private_Declarations (Parent (Decls)))
      then
         Decls := Private_Declarations (Parent (Decls));
      end if;

      --  If we were unable to find the declarations, that means that the
      --  completion of the type was missing. We can safely return and let
      --  the error be caught by the semantic analysis.

      if No (Decls) then
         return;
      end if;

      Add_Stub_Type
        (Designated_Type     => Desig,
         RACW_Type           => RACW_Type,
         Decls               => Decls,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         Existing            => Existing);

      Add_RACW_Read_Write_Attributes
        (RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         Declarations        => Decls);

      Add_RACW_From_Any
        (RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Declarations        => Decls);

      Add_RACW_To_Any
        (Designated_Type     => Desig,
         RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Declarations        => Decls);

      Add_RACW_TypeCode
        (Designated_Type     => Desig,
         RACW_Type           => RACW_Type,
         Declarations        => Decls);

      if not Same_Scope and then not Existing then

         --  The RACW has been declared in another scope than the designated
         --  type and has not been handled by another RACW in the same
         --  package as the first one, so add primitive for the stub type
         --  here.

         Add_RACW_Primitive_Declarations_And_Bodies
           (Designated_Type  => Desig,
            Insertion_Node   =>
              Declaration_Node (Object_RPC_Receiver),
            Decls            => Decls);

      else
         Add_Access_Type_To_Process (E => Desig, A => RACW_Type);
      end if;
   end Add_RACW_Features;

   -----------------------
   -- Add_RACW_From_Any --
   -----------------------

   procedure Add_RACW_From_Any
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Declarations        : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);
      Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);

      Fnam : constant Entity_Id
        := Make_Defining_Identifier (Loc, New_Internal_Name ('F'));

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      Decls             : List_Id;
      Statements        : List_Id;
      Stub_Statements   : List_Id;
      Local_Statements  : List_Id;
      Remote_Statements : List_Id;
      --  Various parts of the subprogram

      Any_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_A);

      Reference         : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('R'));

      Is_Local          : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('L'));

      Addr              : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('A'));

      Stubbed_Result    : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('S'));

      Asynchronous_Node : constant Node_Id :=
        Asynchronous_Flags_Table.Get (RACW_Type);
      --  The asynchronous flag object declared in Add_RACW_Read_Attribute.
   begin

      --  Object declarations

      Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Reference,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Object_Ref), Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of (RTE (RE_FA_ObjRef), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Any_Parameter, Loc)))),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Stubbed_Result,
          Object_Definition   =>
            New_Occurrence_Of (Stub_Type_Access, Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Is_Local,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Boolean, Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Addr,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Address), Loc)));

      --  If the ref Is_Nil, return a null pointer.

      Statements := New_List (
        Make_Implicit_If_Statement (RACW_Type,
          Condition =>
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of (RTE (RE_Is_Nil), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Reference, Loc))),
          Then_Statements => New_List (
            Make_Return_Statement (Loc,
              Expression =>
                Make_Null (Loc)))));

      Append_To (Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Reference, Loc),
            New_Occurrence_Of (Is_Local, Loc),
            New_Occurrence_Of (Addr, Loc))));

      --  If the object is located on another partition, then a stub
      --  object will be created with all the information needed to
      --  rebuild the real object at the other end. This stanza
      --  is always used in the case of RAS types, for which a
      --  stub is required even for local subprograms.

      Stub_Statements := New_List (

        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Stubbed_Result, Loc),
          Expression =>
            Make_Allocator (Loc,
              New_Occurrence_Of (Stub_Type, Loc))),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Target)),
          Expression =>
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of (RTE (RE_Entity_Of), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Reference, Loc)))),

        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Inc_Usage), Loc),
          Parameter_Associations => New_List (
            Make_Selected_Component (Loc,
              Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
              Selector_Name => Make_Identifier (Loc, Name_Target)))),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Asynchronous)),
          Expression =>
            New_Occurrence_Of (
              Defining_Identifier (Asynchronous_Node), Loc)),

        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Get_Unique_Remote_Pointer), Loc),
          Parameter_Associations => New_List (
            Unchecked_Convert_To (RTE (RE_RACW_Stub_Type_Access),
              New_Occurrence_Of (Stubbed_Result, Loc)))));
      --  XXX Houston we have a problem. Here, the Asynchronous flag
      --  in the stub type is set if, and only if, the RACW type has
      --  a pragma Asynchronous. This is incorrect for RACWs that
      --  implement RAS types, because in that case the /designated
      --  subprogram/ (not the type) might be asynchronous, and
      --  that causes the stub to need to be asynchronous too.
      --  The solution is to transport a RAS as a struct containing
      --  a Ref and an asynchronous flag, and to properly alter
      --  the Asynchronous component in the stub type in the RAS's
      --  _From_Any TSS.

      --  Distinguish between the local and remote cases, and execute the
      --  appropriate piece of code.

      if Is_RAS then
         Append_List_To (Statements, Stub_Statements);
         --  Always instanciate a stub.

         Local_Statements := New_List (
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix =>
                   New_Occurrence_Of (Stubbed_Result, Loc),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Addr)),
             Expression =>
               New_Occurrence_Of (Addr, Loc)));

         Remote_Statements := New_List (Make_Null_Statement (Loc));

      else
         Local_Statements := New_List (
           Make_Return_Statement (Loc,
             Expression =>
               Unchecked_Convert_To (RACW_Type,
                 New_Occurrence_Of (Addr, Loc))));

         Remote_Statements := Stub_Statements;
      end if;

      Append_To (Statements,
        Make_Implicit_If_Statement (RACW_Type,
          Condition =>
            New_Occurrence_Of (Is_Local, Loc),
          Then_Statements => Local_Statements,
          Else_Statements => Remote_Statements));

      Append_To (Statements,
        Make_Return_Statement (Loc,
          Expression => Unchecked_Convert_To (RACW_Type,
            New_Occurrence_Of (Stubbed_Result, Loc))));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Any_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (RTE (RE_Any), Loc))),
          Subtype_Mark => New_Occurrence_Of (RACW_Type, Loc));

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RACW_Parameter must
      --  refer to the entity in the declaration spec, not those
      --  of the body spec.

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Statements));

      Insert_After (Declaration_Node (RACW_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RACW_Type, Fnam, Name_uFrom_Any);
   end Add_RACW_From_Any;

   ------------------------------------------------
   -- Add_RACW_Primitive_Declarations_And_Bodies --
   ------------------------------------------------

   procedure Add_RACW_Primitive_Declarations_And_Bodies
     (Designated_Type : in Entity_Id;
      Insertion_Node  : in Node_Id;
      Decls           : in List_Id)
   is
      --  Set sloc of generated declaration to be that of the
      --  insertion node, so the declarations are recognized as
      --  belonging to the current package.

      Loc : constant Source_Ptr := Sloc (Insertion_Node);

      Stub_Elements : constant Stub_Structure :=
        Stubs_Table.Get (Designated_Type);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);
      Is_RAS : constant Boolean :=
        not Comes_From_Source (Stub_Elements.RACW_Type);

      Current_Insertion_Node : Node_Id := Insertion_Node;

      RPC_Receiver : constant Entity_Id
        := Make_Defining_Identifier (Loc, New_Internal_Name ('H'));

      RPC_Receiver_Statements        : List_Id;
      RPC_Receiver_Case_Alternatives : constant List_Id := New_List;
      RPC_Receiver_Request           : Entity_Id;
      RPC_Receiver_Subp_Id           : Entity_Id;

      Subp_Str : String_Id;

      Current_Primitive_Elmt   : Elmt_Id;
      Current_Primitive        : Entity_Id;
      Current_Primitive_Body   : Node_Id;
      Current_Primitive_Spec   : Node_Id;
      Current_Primitive_Decl   : Node_Id;
      Current_Primitive_Number : Int := 0;

      Current_Primitive_Alias : Node_Id;

      Current_Receiver      : Entity_Id;
      Current_Receiver_Body : Node_Id;

      RPC_Receiver_Decl : Node_Id;

      Possibly_Asynchronous : Boolean;

   begin
      if not Expander_Active then
         return;
      end if;

      --  Build callers, receivers for every primitive operations and a RPC
      --  receiver for this type.

      if not Is_RAS then
         Build_RPC_Receiver_Body (
           RPC_Receiver => RPC_Receiver,
           Request      => RPC_Receiver_Request,
           Subp_Id      => RPC_Receiver_Subp_Id,
           Stmts        => RPC_Receiver_Statements,
           Decl         => RPC_Receiver_Decl);
      end if;

      if Present (Primitive_Operations (Designated_Type)) then

         Overload_Counter_Table.Reset;

         Current_Primitive_Elmt :=
           First_Elmt (Primitive_Operations (Designated_Type));

         while Current_Primitive_Elmt /= No_Elmt loop

            Current_Primitive := Node (Current_Primitive_Elmt);

            --  Copy the primitive of all the parents, except predefined
            --  ones that are not remotely dispatching.

            if Chars (Current_Primitive) /= Name_uSize
              and then Chars (Current_Primitive) /= Name_uAlignment
              and then Chars (Current_Primitive) /= Name_uDeep_Finalize
            then
               --  The first thing to do is build an up-to-date copy of
               --  the spec with all the formals referencing Designated_Type
               --  transformed into formals referencing Stub_Type. Since this
               --  primitive may have been inherited, go back the alias chain
               --  until the real primitive has been found.

               Current_Primitive_Alias := Current_Primitive;
               while Present (Alias (Current_Primitive_Alias)) loop
                  pragma Assert
                    (Current_Primitive_Alias
                      /= Alias (Current_Primitive_Alias));
                  Current_Primitive_Alias := Alias (Current_Primitive_Alias);
               end loop;

               Current_Primitive_Spec :=
                 Copy_Specification (Loc,
                   Spec        => Parent (Current_Primitive_Alias),
                   Object_Type => Designated_Type,
                   Stub_Type   => Stub_Elements.Stub_Type);

               Current_Primitive_Decl :=
                 Make_Subprogram_Declaration (Loc,
                   Specification => Current_Primitive_Spec);

               Insert_After (Current_Insertion_Node, Current_Primitive_Decl);
               Analyze (Current_Primitive_Decl);
               Current_Insertion_Node := Current_Primitive_Decl;

               Possibly_Asynchronous :=
                 Nkind (Current_Primitive_Spec) = N_Procedure_Specification
                 and then Could_Be_Asynchronous (Current_Primitive_Spec);

               Assign_Subprogram_Identifier (
                 Defining_Unit_Name (Current_Primitive_Spec),
                 Subp_Str);

               Current_Primitive_Body :=
                 Build_Subprogram_Calling_Stubs
                   (Vis_Decl                 => Current_Primitive_Decl,
                    Subp_Id                  =>
                      Make_String_Literal (Loc, Subp_Str),
                    Asynchronous             => Possibly_Asynchronous,
                    Dynamically_Asynchronous => Possibly_Asynchronous,
                    Stub_Type                => Stub_Elements.Stub_Type,
                    RACW_Type                => Stub_Elements.RACW_Type);
               Append_To (Decls, Current_Primitive_Body);

               --  Analyzing the body here would cause the Stub type to be
               --  frozen, thus preventing subsequent primitive declarations.
               --  For this reason, it will be analyzed later in the
               --  regular flow.

               --  Build the receiver stubs (only for RACWs that correspond
               --  to real objects, not for RAS).

               if not Is_RAS then
                  Current_Receiver_Body :=
                    Build_Subprogram_Receiving_Stubs
                      (Vis_Decl                 => Current_Primitive_Decl,
                       Asynchronous             => Possibly_Asynchronous,
                       Dynamically_Asynchronous => Possibly_Asynchronous,
                       Stub_Type                => Stub_Elements.Stub_Type,
                       RACW_Type                => Stub_Elements.RACW_Type,
                       Parent_Primitive         => Current_Primitive);

                  Append_To (Decls, Current_Receiver_Body);

                  --  Add a case alternative to the receiver

--                 Append_To (RPC_Receiver_Case_Alternatives,
--                   Make_Case_Statement_Alternative (Loc,
--                     Discrete_Choices => New_List (
--                       Make_Integer_Literal (Loc, Current_Primitive_Number)),

--                     Statements       => New_List (
--                       Make_Procedure_Call_Statement (Loc,
--                         Name                   =>
--                           New_Occurrence_Of (Current_Receiver, Loc),
--                         Parameter_Associations => New_List (
--                           New_Occurrence_Of (
--                             RPC_Receiver_Request, Loc))))));

                  Current_Receiver :=
                     Defining_Unit_Name (
                       Specification (Current_Receiver_Body));
                  Append_To (RPC_Receiver_Case_Alternatives,
                    Make_Implicit_If_Statement (Designated_Type,
                      Condition =>
                        Make_Function_Call (Loc,
                          Name =>
                            New_Occurrence_Of (
                              RTE (RE_Caseless_String_Eq), Loc),
                          Parameter_Associations => New_List (
                            New_Occurrence_Of (RPC_Receiver_Subp_Id, Loc),
                            Make_String_Literal (Loc, Subp_Str))),
                      Then_Statements => New_List (
                        Make_Procedure_Call_Statement (Loc,
                          Name                   =>
                            New_Occurrence_Of (Current_Receiver, Loc),
                          Parameter_Associations => New_List (
                            New_Occurrence_Of (
                              RPC_Receiver_Request, Loc))))));
               end if;
               --  Increment the index of current primitive

               Current_Primitive_Number := Current_Primitive_Number + 1;
            end if;

            Next_Elmt (Current_Primitive_Elmt);
         end loop;
      end if;

      --  Build the case statement and the heart of the subprogram

--        Append_To (RPC_Receiver_Case_Alternatives,
--          Make_Case_Statement_Alternative (Loc,
--            Discrete_Choices => New_List (Make_Others_Choice (Loc)),
--            Statements       => New_List (Make_Null_Statement (Loc))));

--        Append_To (RPC_Receiver_Statements,
--          Make_Case_Statement (Loc,
--            Expression   =>
--              New_Occurrence_Of (RPC_Receiver_OLD_Subp_Id, Loc),
--            Alternatives => RPC_Receiver_Case_Alternatives));

      if not Is_RAS then
         Append_List_To (RPC_Receiver_Statements,
           RPC_Receiver_Case_Alternatives);

         Append_To (Decls, RPC_Receiver_Decl);
--        Append_To (Decls,
--          Make_Assignment_Statement (Loc,
--            Name =>
--              Make_Selected_Component (Loc,
--                Prefix =>
--                  New_Occurrence_Of (
--                    Stub_Elements.Object_RPC_Receiver, Loc),
--                Selector_Name =>
--                  Make_Identifier (Loc, Name_Handler)),
--            Expression =>
--              Make_Attribute_Reference (Loc,
--                Prefix =>
--                  New_Occurrence_Of (
--                    Defining_Unit_Name (
--                      Specification (RPC_Receiver_Decl)), Loc),
--                Attribute_Name =>
--                  Name_Access)));

--        Append_Freeze_Action (Designated_Type,
         Append_To (Decls,
           Make_Procedure_Call_Statement (Loc,
              Name =>
                New_Occurrence_Of (
                  RTE (RE_Register_Obj_Receiving_Stub), Loc),

              Parameter_Associations => New_List (
                  --  Name
                Make_String_Literal (Loc,
                  Full_Qualified_Name (Designated_Type)),

                  --  Handler
                Make_Attribute_Reference (Loc,
                  Prefix =>
                    New_Occurrence_Of (
                      Defining_Unit_Name (
                        Specification (RPC_Receiver_Decl)), Loc),
                  Attribute_Name =>
                    Name_Access),

                  --  Receiver
                Make_Attribute_Reference (Loc,
                  Prefix =>
                   New_Occurrence_Of (
                     Stub_Elements.Object_RPC_Receiver, Loc),
                  Attribute_Name =>
                    Name_Access))));

         --  Do not analyze RPC receiver at this stage since it will otherwise
         --  reference subprograms that have not been analyzed yet. It will
         --  be analyzed in the regular flow.
      end if;

   end Add_RACW_Primitive_Declarations_And_Bodies;

   -----------------------------
   -- Add_RACW_Read_Attribute --
   -----------------------------

   procedure Add_RACW_Read_Attribute
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Declarations        : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Proc_Decl : Node_Id;
      Attr_Decl : Node_Id;

      Body_Node : Node_Id;

      Decls             : List_Id;
      Statements        : List_Id;
      Local_Statements  : List_Id;
      Remote_Statements : List_Id;
      --  Various parts of the procedure

      Procedure_Name    : constant Name_Id   :=
                            New_Internal_Name ('R');
      Source_Partition  : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('P'));
      Source_Receiver   : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('S'));
      Source_Address    : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('P'));
      Stubbed_Result    : constant Entity_Id  :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('S'));
      Asynchronous_Flag : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('S'));
      Asynchronous_Node : Node_Id;

      --  Functions to create occurrences of the formal
      --  parameter names.

      function Stream_Parameter return Node_Id;
      function Result return Node_Id;

      function Stream_Parameter return Node_Id is
      begin
         return Make_Identifier (Loc, Name_S);
      end Stream_Parameter;

      function Result return Node_Id is
      begin
         return Make_Identifier (Loc, Name_V);
      end Result;

   begin
      --  Declare the asynchronous flag. This flag will be changed to True
      --  whenever it is known that the RACW type is asynchronous. Also, the
      --  node gets stored since it may be rewritten when we process the
      --  asynchronous pragma.

      Asynchronous_Node := Make_Object_Declaration (Loc,
          Defining_Identifier => Asynchronous_Flag,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc),
          Expression          => New_Occurrence_Of (Standard_False, Loc));
      Append_To (Declarations, Asynchronous_Node);
      Set_Ekind (Asynchronous_Flag, E_Variable);
      Set_Etype (Asynchronous_Flag, Standard_Boolean);
      Asynchronous_Flags_Table.Set (RACW_Type, Asynchronous_Node);

      --  Object declarations

      Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Source_Partition,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Partition_ID), Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Source_Receiver,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Source_Address,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Stubbed_Result,
          Object_Definition   =>
            New_Occurrence_Of (Stub_Type_Access, Loc)));

      --  Read the source Partition_ID and RPC_Receiver from incoming stream

      Statements := New_List (
        Make_Attribute_Reference (Loc,
          Prefix         =>
            New_Occurrence_Of (RTE (RE_Partition_ID), Loc),
          Attribute_Name => Name_Read,
          Expressions    => New_List (
            Stream_Parameter,
            New_Occurrence_Of (Source_Partition, Loc))),

        Make_Attribute_Reference (Loc,
          Prefix         =>
            New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
          Attribute_Name =>
            Name_Read,
          Expressions    => New_List (
            Stream_Parameter,
            New_Occurrence_Of (Source_Receiver, Loc))),

        Make_Attribute_Reference (Loc,
          Prefix         =>
            New_Occurrence_Of (RTE (RE_Unsigned_64), Loc),
          Attribute_Name =>
            Name_Read,
          Expressions    => New_List (
            Stream_Parameter,
            New_Occurrence_Of (Source_Address, Loc))));

      --  If the Address is Null_Address, then return a null object

      Append_To (Statements,
        Make_Implicit_If_Statement (RACW_Type,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  => New_Occurrence_Of (Source_Address, Loc),
              Right_Opnd => Make_Integer_Literal (Loc, Uint_0)),
          Then_Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => Result,
              Expression => Make_Null (Loc)),
            Make_Return_Statement (Loc))));

      --  If the RACW denotes an object created on the current partition, then
      --  Local_Statements will be executed. The real object will be used.

      Local_Statements := New_List (
        Make_Assignment_Statement (Loc,
          Name       => Result,
          Expression =>
            Unchecked_Convert_To (RACW_Type,
              OK_Convert_To (RTE (RE_Address),
                New_Occurrence_Of (Source_Address, Loc)))));

      --  If the object is located on another partition, then a stub object
      --  will be created with all the information needed to rebuild the
      --  real object at the other end.

      Remote_Statements := New_List (

        Make_Assignment_Statement (Loc,
          Name       => New_Occurrence_Of (Stubbed_Result, Loc),
          Expression =>
            Make_Allocator (Loc,
              New_Occurrence_Of (Stub_Type, Loc))),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Origin)),
          Expression =>
            New_Occurrence_Of (Source_Partition, Loc)),

        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Receiver)),
          Expression =>
            New_Occurrence_Of (Source_Receiver, Loc)));

--          Make_Assignment_Statement (Loc,
--            Name       => Make_Selected_Component (Loc,
--              Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
--              Selector_Name => Make_Identifier (Loc, Name_Addr)),
--            Expression =>
--              New_Occurrence_Of (Source_Address, Loc)));
--  XXX

      Append_To (Remote_Statements,
        Make_Assignment_Statement (Loc,
          Name       => Make_Selected_Component (Loc,
            Prefix        => New_Occurrence_Of (Stubbed_Result, Loc),
            Selector_Name => Make_Identifier (Loc, Name_Asynchronous)),
          Expression =>
            New_Occurrence_Of (Asynchronous_Flag, Loc)));

      Append_To (Remote_Statements,
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Get_Unique_Remote_Pointer), Loc),
          Parameter_Associations => New_List (
            Unchecked_Convert_To (RTE (RE_RACW_Stub_Type_Access),
              New_Occurrence_Of (Stubbed_Result, Loc)))));

      Append_To (Remote_Statements,
        Make_Assignment_Statement (Loc,
          Name       => Result,
          Expression => Unchecked_Convert_To (RACW_Type,
            New_Occurrence_Of (Stubbed_Result, Loc))));

      --  Distinguish between the local and remote cases, and execute the
      --  appropriate piece of code.

      Append_To (Statements,
        Make_Implicit_If_Statement (RACW_Type,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  =>
                Make_Function_Call (Loc,
                  Name =>
                    New_Occurrence_Of (RTE (RE_Get_Local_Partition_Id), Loc)),
              Right_Opnd => New_Occurrence_Of (Source_Partition, Loc)),
          Then_Statements => Local_Statements,
          Else_Statements => Remote_Statements));

      Build_Stream_Procedure
        (Loc, RACW_Type, Body_Node,
         Make_Defining_Identifier (Loc, Procedure_Name),
         Statements, Outp => True);
      Set_Declarations (Body_Node, Decls);

      Proc_Decl := Make_Subprogram_Declaration (Loc,
        Copy_Specification (Loc, Specification (Body_Node)));

      Attr_Decl :=
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Occurrence_Of (RACW_Type, Loc),
          Chars      => Name_Read,
          Expression =>
            New_Occurrence_Of (
              Defining_Unit_Name (Specification (Proc_Decl)), Loc));

      Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
      Insert_After (Proc_Decl, Attr_Decl);
      Append_To (Declarations, Body_Node);
   end Add_RACW_Read_Attribute;

   ------------------------------------
   -- Add_RACW_Read_Write_Attributes --
   ------------------------------------

   procedure Add_RACW_Read_Write_Attributes
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Object_RPC_Receiver : in Entity_Id;
      Declarations        : in List_Id)
   is
   begin
      Add_RACW_Write_Attribute
        (RACW_Type           => RACW_Type,
         Stub_Type           => Stub_Type,
         Stub_Type_Access    => Stub_Type_Access,
         Object_RPC_Receiver => Object_RPC_Receiver,
         Declarations        => Declarations);

      Add_RACW_Read_Attribute
        (RACW_Type        => RACW_Type,
         Stub_Type        => Stub_Type,
         Stub_Type_Access => Stub_Type_Access,
         Declarations     => Declarations);
   end Add_RACW_Read_Write_Attributes;

   ---------------------
   -- Add_RACW_To_Any --
   ---------------------

   procedure Add_RACW_To_Any
     (Designated_Type  : in Entity_Id;
      RACW_Type        : in Entity_Id;
      Stub_Type        : in Entity_Id;
      Stub_Type_Access : in Entity_Id;
      Declarations     : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);
      Is_RAS : constant Boolean := not Comes_From_Source (RACW_Type);

      Fnam : Entity_Id;

      Stub_Elements : constant Stub_Structure :=
        Stubs_Table.Get (Designated_Type);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      Decls             : List_Id;
      Statements        : List_Id;
      Null_Statements   : List_Id;
      Local_Statements  : List_Id;
      Remote_Statements : List_Id;
      If_Node           : Node_Id;
      --  Various parts of the subprogram

      RACW_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_R);

      Reference         : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('R'));
      Any               : constant Entity_Id :=
                            Make_Defining_Identifier
                              (Loc, New_Internal_Name ('A'));
   begin

      --  Object declarations

      Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Reference,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Object_Ref), Loc)),
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Any,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Any), Loc)));

      --  We first convert the RACW into an object reference,
      --  then we call To_Any on that reference and fix the
      --  Any's TypeCode.

      --  If the object is located on another partition, use the
      --  reference from the stub type.

      Remote_Statements := New_List (
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Set_Ref), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Reference, Loc),
            Make_Selected_Component (Loc,
              Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                New_Occurrence_Of (RACW_Parameter, Loc)),
              Selector_Name =>
                Make_Identifier (Loc, Name_Target)))));

      --  If the object is null, nothing to do (Reference is already
      --  a Nil ref.)

      Null_Statements := New_List (Make_Null_Statement (Loc));

      --  Distinguish between the null, local and remote cases,
      --  and execute the appropriate piece of code.

      If_Node :=
        Make_Implicit_If_Statement (RACW_Type,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  => New_Occurrence_Of (RACW_Parameter, Loc),
              Right_Opnd => Make_Null (Loc)),
          Then_Statements => Null_Statements,
          Else_Statements => Remote_Statements);

      if not Is_RAS then

         --  If this is a real RACW, handle the case where it
         --  designates a local object.

         Local_Statements := New_List (
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Get_Reference), Loc),
             Parameter_Associations => New_List (
               Unchecked_Convert_To (
                 RTE (RE_Address),
                 New_Occurrence_Of (RACW_Parameter, Loc)),
               Make_String_Literal (Loc,
                 Full_Qualified_Name (Designated_Type)),
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (Stub_Elements.Object_RPC_Receiver, Loc),
                 Attribute_Name =>
                   Name_Access),
               New_Occurrence_Of (Reference, Loc))));

         Set_Elsif_Parts (If_Node, New_List (
            Make_Elsif_Part (Loc,
              Condition       =>
                Make_Op_Ne (Loc,
                  Left_Opnd  =>
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (RACW_Parameter, Loc),
                      Attribute_Name => Name_Tag),
                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Stub_Type, Loc),
                      Attribute_Name => Name_Tag)),
              Then_Statements => Local_Statements)));
      end if;

      Statements := New_List (
        If_Node,
        Make_Assignment_Statement (Loc,
          Name =>
            New_Occurrence_Of (Any, Loc),
          Expression =>
            Make_Function_Call (Loc,
              Name => New_Occurrence_Of (RTE (RE_TA_ObjRef), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Reference, Loc)))),
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Set_TC), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Any, Loc),
            Make_Selected_Component (Loc,
              Prefix =>
                New_Occurrence_Of (
                  Stub_Elements.Object_RPC_Receiver, Loc),
              Selector_Name =>
                Make_Identifier (Loc, Name_Obj_TypeCode)))),
        Make_Return_Statement (Loc,
          Expression =>
            New_Occurrence_Of (Any, Loc)));

      Fnam := Make_Defining_Identifier (
        Loc, New_Internal_Name ('T'));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                RACW_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (RACW_Type, Loc))),
          Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RACW_Parameter must
      --  refer to the entity in the declaration spec, not in
      --  the body spec.

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Statements));

      Insert_After (Declaration_Node (RACW_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RACW_Type, Fnam, Name_uTo_Any);
   end Add_RACW_To_Any;

   -----------------------
   -- Add_RACW_TypeCode --
   -----------------------

   procedure Add_RACW_TypeCode
     (Designated_Type  : in Entity_Id;
      RACW_Type        : in Entity_Id;
      Declarations     : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Fnam : Entity_Id;

      Stub_Elements : constant Stub_Structure :=
        Stubs_Table.Get (Designated_Type);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      RACW_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_R);

   begin

      Fnam := Make_Defining_Identifier (
        Loc, New_Internal_Name ('T'));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                RACW_Parameter,
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  New_Occurrence_Of (RACW_Type, Loc)))),
          Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));
      --  Dummy 'access RACW' argument, just over overload.

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RACW_Parameter must
      --  refer to the entity in the declaration spec, not those
      --  of the body spec.

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => Empty_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Selected_Component (Loc,
                      Prefix =>
                        New_Occurrence_Of (
                          Stub_Elements.Object_RPC_Receiver, Loc),
                      Selector_Name =>
                        Make_Identifier (Loc, Name_Obj_TypeCode))))));

      Insert_After (Declaration_Node (RACW_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RACW_Type, Fnam, Name_uTypeCode);
   end Add_RACW_TypeCode;

   ------------------------------
   -- Add_RACW_Write_Attribute --
   ------------------------------

   procedure Add_RACW_Write_Attribute
     (RACW_Type           : in Entity_Id;
      Stub_Type           : in Entity_Id;
      Stub_Type_Access    : in Entity_Id;
      Object_RPC_Receiver : in Entity_Id;
      Declarations        : in List_Id)
   is
      Loc  : constant Source_Ptr := Sloc (RACW_Type);

      Body_Node      : Node_Id;

      Proc_Decl      : Node_Id;
      Attr_Decl      : Node_Id;

      Statements        : List_Id;
      Local_Statements  : List_Id;
      Remote_Statements : List_Id;
      Null_Statements   : List_Id;

      Procedure_Name    : constant Name_Id := New_Internal_Name ('R');

      --  Functions to create occurrences of the formal
      --  parameter names.

      function Stream_Parameter return Node_Id;
      function Object return Node_Id;

      function Stream_Parameter return Node_Id is
      begin
         return Make_Identifier (Loc, Name_S);
      end Stream_Parameter;

      function Object return Node_Id is
      begin
         return Make_Identifier (Loc, Name_V);
      end Object;

   begin
      --  Build the code fragment corresponding to the marshalling of a
      --  local object.

      Local_Statements := New_List (

        Pack_Entity_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => RTE (RE_Get_Local_Partition_Id)),

        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => OK_Convert_To (RTE (RE_Unsigned_64),
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Object_RPC_Receiver, Loc),
              Attribute_Name => Name_Address)),
          Etyp   => RTE (RE_Unsigned_64)),

        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => OK_Convert_To (RTE (RE_Unsigned_64),
            Make_Attribute_Reference (Loc,
              Prefix         =>
                Make_Explicit_Dereference (Loc,
                  Prefix => Object),
              Attribute_Name => Name_Address)),
          Etyp   => RTE (RE_Unsigned_64)));

      --  Build the code fragment corresponding to the marshalling of
      --  a remote object.

      Remote_Statements := New_List (

        Pack_Node_Into_Stream_Access (Loc,
         Stream => Stream_Parameter,
         Object =>
            Make_Selected_Component (Loc,
              Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                Object),
              Selector_Name =>
                Make_Identifier (Loc, Name_Origin)),
         Etyp   => RTE (RE_Partition_ID)),

        Pack_Node_Into_Stream_Access (Loc,
         Stream => Stream_Parameter,
         Object =>
            Make_Selected_Component (Loc,
              Prefix        => Unchecked_Convert_To (Stub_Type_Access,
                Object),
              Selector_Name =>
                Make_Identifier (Loc, Name_Receiver)),
         Etyp   => RTE (RE_Unsigned_64)));

--          Pack_Node_Into_Stream_Access (Loc,
--           Stream => Stream_Parameter,
--           Object =>
--              Make_Selected_Component (Loc,
--                Prefix        => Unchecked_Convert_To (Stub_Type_Access,
--                  Object),
--                Selector_Name =>
--                  Make_Identifier (Loc, Name_Addr)),
--           Etyp   => RTE (RE_Unsigned_64)));

      --  Build the code fragment corresponding to the marshalling of a null
      --  object.

      Null_Statements := New_List (

        Pack_Entity_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => RTE (RE_Get_Local_Partition_Id)),

        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => OK_Convert_To (RTE (RE_Unsigned_64),
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Object_RPC_Receiver, Loc),
              Attribute_Name => Name_Address)),
          Etyp   => RTE (RE_Unsigned_64)),

        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream_Parameter,
          Object => Make_Integer_Literal (Loc, Uint_0),
          Etyp   => RTE (RE_Unsigned_64)));

      Statements := New_List (
        Make_Implicit_If_Statement (RACW_Type,
          Condition       =>
            Make_Op_Eq (Loc,
              Left_Opnd  => Object,
              Right_Opnd => Make_Null (Loc)),
          Then_Statements => Null_Statements,
          Elsif_Parts     => New_List (
            Make_Elsif_Part (Loc,
              Condition       =>
                Make_Op_Eq (Loc,
                  Left_Opnd  =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => Object,
                      Attribute_Name => Name_Tag),
                  Right_Opnd =>
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Stub_Type, Loc),
                      Attribute_Name => Name_Tag)),
              Then_Statements => Remote_Statements)),
          Else_Statements => Local_Statements));

      Build_Stream_Procedure
        (Loc, RACW_Type, Body_Node,
         Make_Defining_Identifier (Loc, Procedure_Name),
         Statements, Outp => False);

      Proc_Decl := Make_Subprogram_Declaration (Loc,
        Copy_Specification (Loc, Specification (Body_Node)));

      Attr_Decl :=
        Make_Attribute_Definition_Clause (Loc,
          Name       => New_Occurrence_Of (RACW_Type, Loc),
          Chars      => Name_Write,
          Expression =>
            New_Occurrence_Of (
              Defining_Unit_Name (Specification (Proc_Decl)), Loc));

      Insert_After (Declaration_Node (RACW_Type), Proc_Decl);
      Insert_After (Proc_Decl, Attr_Decl);
      Append_To (Declarations, Body_Node);
   end Add_RACW_Write_Attribute;

   ------------------------
   -- Add_RAS_Access_TSS --
   ------------------------

   procedure Add_RAS_Access_TSS (N : in Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Ras_Type  : constant Entity_Id := Defining_Identifier (N);
      Fat_Type  : constant Entity_Id := Equivalent_Type (Ras_Type);
      RACW_Type : constant Entity_Id :=
        Underlying_RACW_Type (Ras_Type);

      Desig     : constant Entity_Id :=
        Etype (Designated_Type (RACW_Type));

      Stub_Elements : constant Stub_Structure :=
        Stubs_Table.Get (Desig);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      Stub_Ptr : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

      --  Ras_Type is the access to subprogram type while Fat_Type points to
      --  the record type corresponding to a remote access to subprogram type.

      Proc_Decls        : List_Id;
      Proc_Statements   : constant List_Id := New_List;

      Proc_Spec : Node_Id;
      Proc_Body : Node_Id;

      Proc : Node_Id;

      Package_Name : Node_Id;
      Subp_Id      : Node_Id;
      Return_Value : Node_Id;

      Asynch_P     : Entity_Id;
      --  Is the procedure to which the 'Access applies asynchronous?
      Asynch_T     : Node_Id;
      --  Is the RAS type asynchronous?

      All_Calls_Remote : Entity_Id;
      --  True if an All_Calls_Remote pragma applies to the RCI unit
      --  that contains the procedure.

      Subp_Ref : Entity_Id;

      Is_Local : Entity_Id;
      --  For the call to Get_Local_Address

      procedure Set_Field (Field_Name : in Name_Id; Value : in Node_Id);
      --  Set a field name for the return value

      procedure Set_Field (Field_Name : in Name_Id; Value : in Node_Id)
      is
      begin
         Append_To (Proc_Statements,
           Make_Assignment_Statement (Loc,
             Name       =>
               Make_Selected_Component (Loc,
                 Prefix        => New_Occurrence_Of (Stub_Ptr, Loc),
                 Selector_Name => Make_Identifier (Loc, Field_Name)),
             Expression => Value));
      end Set_Field;

   --  Start of processing for Add_RAS_Access_Attribute

   begin
      Is_Local := Make_Defining_Identifier (Loc, New_Internal_Name ('L'));
      Package_Name := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
      Subp_Id := Make_Defining_Identifier (Loc, New_Internal_Name ('N'));
      Asynch_P := Make_Defining_Identifier (Loc, New_Internal_Name ('B'));
      Return_Value := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
      All_Calls_Remote :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Subp_Ref := Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

      --  Create the object which will be returned of type Fat_Type

      Proc_Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Stub_Ptr,
          Constant_Present    => True,
          Object_Definition   =>
            New_Occurrence_Of (Stub_Elements.Stub_Type_Access, Loc),
          Expression          =>
            Make_Allocator (Loc,
              New_Occurrence_Of (
                Stub_Elements.Stub_Type, Loc))),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Return_Value,
          Object_Definition   =>
            New_Occurrence_Of (Fat_Type, Loc),
          Expression =>
            Make_Aggregate (Loc,
              Component_Associations => New_List (
                Make_Component_Association (Loc,
                  Choices =>
                    New_List (Make_Identifier (Loc, Name_Ras)),
                  Expression =>
                    Unchecked_Convert_To (RACW_Type,
                       New_Occurrence_Of (
                         Stub_Ptr, Loc)))))),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Subp_Ref,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Object_Ref), Loc)),

        Make_Object_Declaration (Loc,
          Defining_Identifier => Is_Local,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Boolean, Loc)));

      --  Initialize the fields of the record type with the appropriate data

      Append_To (Proc_Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Get_RAS_Ref), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Package_Name, Loc),
            New_Occurrence_Of (Subp_Id, Loc),
            New_Occurrence_Of (Subp_Ref, Loc))));

      Set_Field (Name_Target,
        Make_Function_Call (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Entity_Of), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Subp_Ref, Loc))));

      Append_To (Proc_Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Inc_Usage), Loc),
          Parameter_Associations => New_List (
            Make_Selected_Component (Loc,
              Prefix        => New_Occurrence_Of (Stub_Ptr, Loc),
              Selector_Name => Make_Identifier (Loc, Name_Target)))));

      Append_To (Proc_Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Subp_Ref, Loc),
            New_Occurrence_Of (Is_Local, Loc),
            Make_Selected_Component (Loc,
              Prefix        => New_Occurrence_Of (Stub_Ptr, Loc),
              Selector_Name => Make_Identifier (Loc, Name_Addr)))));
      --  At this point Addr is Null_Address if the reference is remote,
      --  and the local address of the real subprogram otherwise.

      Append_To (Proc_Statements,
        Make_Implicit_If_Statement (N,
          Condition =>
            New_Occurrence_Of (All_Calls_Remote, Loc),
          Then_Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       =>
                Make_Selected_Component (Loc,
                  Prefix        => New_Occurrence_Of (Stub_Ptr, Loc),
                  Selector_Name => Make_Identifier (Loc, Name_Addr)),
              Expression => New_Occurrence_Of (RTE (RE_Null_Address), Loc)))));
      --  If this is an All_Calls_Remote local subprogram, then do not
      --  store the local subprogram address here, to prevent it from
      --  being called directly when the RAS is dereferenced.

      if Is_Asynchronous (Ras_Type) then
         Asynch_T := New_Occurrence_Of (Standard_True, Loc);
      else
         Asynch_T := New_Occurrence_Of (Standard_False, Loc);
      end if;

      Set_Field (Name_Asynchronous,
        Make_Or_Else (Loc,
          New_Occurrence_Of (Asynch_P, Loc),
          Asynch_T));
      --  E.4.1(9) A remote call is asynchronous if it is a call to
      --  a procedure, or a call through a value of an access-to-procedure
      --  type, to which a pragma Asynchronous applies.
      --  Parameter Asynch_P is true when the procedure is asynchronous;
      --  Expression Asynch_T is true when the type is asynchronous.

      Append_To (Proc_Statements,
        Make_Return_Statement (Loc,
          Expression =>
            New_Occurrence_Of (Return_Value, Loc)));

      Proc := Make_Defining_Identifier (Loc, Name_uRAS_Access);

      Proc_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name       => Proc,
          Parameter_Specifications => New_List (

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Package_Name,
              Parameter_Type      =>
                New_Occurrence_Of (Standard_String, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Subp_Id,
              Parameter_Type      =>
                New_Occurrence_Of (Standard_String, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => Asynch_P,
              Parameter_Type      =>
                New_Occurrence_Of (Standard_Boolean, Loc)),

            Make_Parameter_Specification (Loc,
              Defining_Identifier => All_Calls_Remote,
              Parameter_Type      =>
                New_Occurrence_Of (Standard_Boolean, Loc))),

         Subtype_Mark =>
           New_Occurrence_Of (Fat_Type, Loc));

      --  Set the kind and return type of the function to prevent ambiguities
      --  between Ras_Type and Fat_Type in subsequent analysis.

      Set_Ekind (Proc, E_Function);
      Set_Etype (Proc, New_Occurrence_Of (Fat_Type, Loc));

      Proc_Body :=
        Make_Subprogram_Body (Loc,
          Specification              => Proc_Spec,
          Declarations               => Proc_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Proc_Statements));

      Set_TSS (Fat_Type, Proc);

   end Add_RAS_Access_TSS;

   -----------------------------
   -- Add_RAS_Dereference_TSS --
   -----------------------------

   procedure Add_RAS_Dereference_TSS (N : in Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Type_Def : constant Node_Id   := Type_Definition (N);

      RAS_Type : constant Entity_Id := Defining_Identifier (N);
      Fat_Type : constant Entity_Id := Equivalent_Type (RAS_Type);

      RACW_Type : constant Entity_Id := Underlying_RACW_Type (RAS_Type);
      Desig : constant Entity_Id :=
        Etype (Designated_Type (RACW_Type));
      Stub_Elements : constant Stub_Structure := Stubs_Table.Get (Desig);
      pragma Assert (Stub_Elements /= Empty_Stub_Structure);

      Proc_Decls      : constant List_Id := New_List;
      Proc_Statements : constant List_Id := New_List;

      Direct_Statements : constant List_Id := New_List;

      Inner_Decls      : constant List_Id := New_List;
      Remote_Statements : constant List_Id := New_List;
      RACW_Primitive_Name : Node_Id;

      Proc : Node_Id;

      Proc_Spec : Node_Id;
      Proc_Body : Node_Id;

      Param_Specs : constant List_Id := New_List;
      Param_Assoc_Direct : constant List_Id := New_List;
      Param_Assoc_Remote : constant List_Id := New_List;

      RAS_Parameter : Node_Id;

      RACW_Parameter   : Entity_Id;
      Converted_Ras    : Node_Id;

      Is_Function : constant Boolean :=
        Nkind (Type_Def) = N_Access_Function_Definition;

      Spec : constant Node_Id := Type_Def;

      Current_Parameter : Node_Id;

   begin
      --  The way to do it is test if the Ras field is non-null and then if
      --  the Origin field is equal to the current partition ID (which is in
      --  fact Current_Package'Partition_ID). If this is the case, then it
      --  is safe to dereference the Ras field directly rather than
      --  performing a remote call.

      RAS_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      RACW_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      Append_To (Proc_Decls,
        Make_Object_Renaming_Declaration (Loc,
          Defining_Identifier => RACW_Parameter,
          Subtype_Mark        =>
            New_Occurrence_Of (RACW_Type, Loc),
          Name                =>
            Make_Selected_Component (Loc,
              Prefix        =>
                New_Occurrence_Of (RAS_Parameter, Loc),
              Selector_Name =>
                Make_Identifier (Loc, Name_Ras))));

      if Present (Parameter_Specifications (Type_Def)) then
         Current_Parameter := First (Parameter_Specifications (Type_Def));

         while Current_Parameter /= Empty loop
            Append_To (Param_Specs,
              Make_Parameter_Specification (Loc,
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    Chars => Chars (Defining_Identifier (Current_Parameter))),
                In_Present        => In_Present (Current_Parameter),
                Out_Present       => Out_Present (Current_Parameter),
                Parameter_Type    =>
                  New_Occurrence_Of
                    (Etype (Parameter_Type (Current_Parameter)), Loc),
                Expression        =>
                  New_Copy_Tree (Expression (Current_Parameter))));

            Append_To (Param_Assoc_Direct,
              Make_Identifier (Loc,
                Chars => Chars (Defining_Identifier (Current_Parameter))));
            Append_To (Param_Assoc_Remote,
              Make_Identifier (Loc,
                Chars => Chars (Defining_Identifier (Current_Parameter))));

            Next (Current_Parameter);
         end loop;
      end if;

      --  In the case of a call to a local subprogram, for
      --  a library unit to which no pragma All_Calls_Remote
      --  applies: call the real subprogram directly.

      Converted_Ras :=
        Unchecked_Convert_To (RAS_Type,
          Make_Selected_Component (Loc,
            Prefix        =>
              Unchecked_Convert_To (Stub_Elements.Stub_Type_Access,
                New_Occurrence_Of (RACW_Parameter, Loc)),
            Selector_Name => Make_Identifier (Loc, Name_Addr)));

      if Is_Function then
         Append_To (Direct_Statements,
           Make_Return_Statement (Loc,
             Expression =>
               Make_Function_Call (Loc,
                 Name                   =>
                   Make_Explicit_Dereference (Loc,
                     Prefix => Converted_Ras),
                 Parameter_Associations => Param_Assoc_Direct)));

      else
         Append_To (Direct_Statements,
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               Make_Explicit_Dereference (Loc,
                 Prefix => Converted_Ras),
             Parameter_Associations => Param_Assoc_Direct));
      end if;

      --  In the remote case, perform the call through the
      --  underlying RACW.

      Prepend_To (Param_Specs,
        Make_Parameter_Specification (Loc,
          Defining_Identifier => RAS_Parameter,
          In_Present          => True,
          Parameter_Type      =>
            New_Occurrence_Of (Fat_Type, Loc)));

      RACW_Primitive_Name :=
        Make_Selected_Component (Loc,
          Prefix =>
            New_Occurrence_Of (Scope (RACW_Type), Loc),
          Selector_Name =>
            Make_Identifier (Loc, Name_Call));

      if Is_Function then
         Append_To (Remote_Statements,
           Make_Return_Statement (Loc,
             Expression =>
               Make_Function_Call (Loc,
                 Name                   => RACW_Primitive_Name,
                 Parameter_Associations => Param_Assoc_Remote)));

      else
         Append_To (Remote_Statements,
           Make_Procedure_Call_Statement (Loc,
             Name                   => RACW_Primitive_Name,
             Parameter_Associations => Param_Assoc_Remote));
      end if;

      --  Build the complete subprogram.

      Proc := Make_Defining_Identifier (Loc, Name_uRAS_Dereference);

      if Is_Function then
         Proc_Spec :=
           Make_Function_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => Param_Specs,
             Subtype_Mark             =>
               New_Occurrence_Of (
                 Entity (Subtype_Mark (Spec)), Loc));

         Set_Ekind (Proc, E_Function);
         Set_Etype (Proc,
           New_Occurrence_Of (Entity (Subtype_Mark (Spec)), Loc));

      else
         Proc_Spec :=
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       => Proc,
             Parameter_Specifications => Param_Specs);

         Set_Ekind (Proc, E_Procedure);
         Set_Etype (Proc, Standard_Void_Type);
      end if;

      Prepend_To (Param_Assoc_Remote,
        Make_Selected_Component (Loc,
          Prefix =>
            New_Occurrence_Of (RAS_Parameter, Loc),
          Selector_Name =>
            Make_Identifier (Loc, Name_Ras)));

      Append_To (Proc_Statements,
        Make_Implicit_If_Statement (N,
          Condition =>
            Make_Op_Ne (Loc,
              Left_Opnd  =>
                Make_Selected_Component (Loc,
                  Prefix        =>
                    Unchecked_Convert_To (Stub_Elements.Stub_Type_Access,
                      New_Occurrence_Of (RACW_Parameter, Loc)),
                  Selector_Name => Make_Identifier (Loc, Name_Addr)),
              Right_Opnd =>
                New_Occurrence_Of (RTE (RE_Null_Address), Loc)),
          Then_Statements =>
            Direct_Statements,

          Else_Statements => New_List (
            Make_Block_Statement (Loc,
              Declarations               => Inner_Decls,
              Handled_Statement_Sequence =>
                Make_Handled_Sequence_Of_Statements (Loc,
                  Statements => Remote_Statements)))));

      Proc_Body :=
        Make_Subprogram_Body (Loc,
          Specification              => Proc_Spec,
          Declarations               => Proc_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Proc_Statements));

      Set_TSS (Fat_Type, Proc);
   end Add_RAS_Dereference_TSS;

   procedure Add_RAS_From_Any
     (RAS_Type     : in Entity_Id;
      Declarations : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RAS_Type);

      Fnam : constant Entity_Id
        := Make_Defining_Identifier (Loc, New_Internal_Name ('F'));

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      Statements : List_Id;

      Any_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_A);

   begin

      Statements := New_List (
        Make_Return_Statement (Loc,
          Expression =>
            Make_Aggregate (Loc,
              Component_Associations => New_List (
                Make_Component_Association (Loc,
                  Choices => New_List (
                    Make_Identifier (Loc, Name_Ras)),
                  Expression =>
                    Build_From_Any_Call (
                      Underlying_RACW_Type (RAS_Type),
                      New_Occurrence_Of (Any_Parameter, Loc),
                      No_List))))));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                Any_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (RTE (RE_Any), Loc))),
          Subtype_Mark => New_Occurrence_Of (RAS_Type, Loc));

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RACW_Parameter must
      --  refer to the entity in the declaration spec, not those
      --  of the body spec.

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => No_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Statements));

      Insert_After (Declaration_Node (RAS_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RAS_Type, Fnam, Name_uFrom_Any);
   end Add_RAS_From_Any;

   procedure Add_RAS_To_Any
     (RAS_Type        : in Entity_Id;
      Declarations    : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RAS_Type);

      Fnam : Entity_Id;

      Decls : List_Id;
      Statements : List_Id;

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      Any : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

      RAS_Parameter : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

      RACW_Parameter : constant Node_Id :=
        Make_Selected_Component (Loc,
          Prefix =>
            New_Occurrence_Of (RAS_Parameter, Loc),
          Selector_Name =>
            Make_Identifier (Loc, Name_Ras));

   begin

      --  Object declarations

      Set_Etype (RACW_Parameter, Underlying_RACW_Type (RAS_Type));
      Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Any,
          Object_Definition =>
            New_Occurrence_Of (RTE (RE_Any), Loc),
          Expression =>
            Build_To_Any_Call (RACW_Parameter, No_List)));

      Statements := New_List (
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Set_TC), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Any, Loc),
            Build_TypeCode_Call (Loc, RAS_Type, Decls))),
        Make_Return_Statement (Loc,
          Expression =>
            New_Occurrence_Of (Any, Loc)));

      Fnam := Make_Defining_Identifier (
        Loc, New_Internal_Name ('T'));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                RAS_Parameter,
              Parameter_Type =>
                New_Occurrence_Of (RAS_Type, Loc))),
          Subtype_Mark => New_Occurrence_Of (RTE (RE_Any), Loc));

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RAS_Parameter must
      --  refer to the entity in the declaration spec, not in
      --  the body spec.

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Statements));

      Insert_After (Declaration_Node (RAS_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RAS_Type, Fnam, Name_uTo_Any);
   end Add_RAS_To_Any;

   ----------------------
   -- Add_RAS_TypeCode --
   ----------------------

   procedure Add_RAS_TypeCode
     (RAS_Type        : in Entity_Id;
      Declarations    : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (RAS_Type);

      Fnam : Entity_Id;

      Func_Spec : Node_Id;
      Func_Decl : Node_Id;
      Func_Body : Node_Id;

      Decls : constant List_Id := New_List;
      Name_String : String_Id;
      Repo_Id_String : String_Id;

      RAS_Parameter : constant Entity_Id
        := Make_Defining_Identifier (Loc, Name_R);

   begin

      Fnam := Make_Defining_Identifier (
        Loc, New_Internal_Name ('T'));

      Func_Spec :=
        Make_Function_Specification (Loc,
          Defining_Unit_Name =>
            Fnam,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier =>
                RAS_Parameter,
              Parameter_Type =>
                Make_Access_Definition (Loc,
                  New_Occurrence_Of (RAS_Type, Loc)))),
          Subtype_Mark => New_Occurrence_Of (RTE (RE_TypeCode), Loc));
      --  Dummy 'access RAS' argument, just over overload.

      Func_Decl := Make_Subprogram_Declaration (Loc, Func_Spec);
      --  NOTE: The usage occurrences of RAS_Parameter must
      --  refer to the entity in the declaration spec, not those
      --  of the body spec.

      --  XXX the following is heavily borrowed from Exp_Hlpr
      --  and should be factored out.

      Get_Name_String (Chars
        (Defining_Identifier (Declaration_Node (RAS_Type))));
      Name_String := String_From_Name_Buffer;
      Repo_Id_String := Name_String;
      --  XXX should compute a proper repository id!

      Func_Body :=
        Make_Subprogram_Body (Loc,
          Specification              =>
            Copy_Specification (Loc, Func_Spec),
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Return_Statement (Loc,
                  Expression =>
                    Make_Function_Call (Loc,
                      Name =>
                        New_Occurrence_Of (RTE (RE_TC_Build), Loc),
                      Parameter_Associations => New_List (
                        New_Occurrence_Of (RTE (RE_TC_Alias), Loc),
                        Make_Aggregate (Loc,
                          Expressions =>
                            New_List (
                              Make_Function_Call (Loc,
                                Name =>
                                  New_Occurrence_Of (RTE (RE_TA_String), Loc),
                                Parameter_Associations => New_List (
                                  Make_String_Literal (Loc, Name_String))),
                              Make_Function_Call (Loc,
                                Name =>
                                  New_Occurrence_Of (RTE (RE_TA_String), Loc),
                                Parameter_Associations => New_List (
                                  Make_String_Literal (Loc, Repo_Id_String))),
                              Make_Function_Call (Loc,
                                Name =>
                                  New_Occurrence_Of (RTE (RE_TA_TC), Loc),
                                Parameter_Associations => New_List (
                                  Build_TypeCode_Call (Loc,
                                    Underlying_RACW_Type (RAS_Type),
                                    Decls)))))))))));

      Insert_After (Declaration_Node (RAS_Type), Func_Decl);
      Append_To (Declarations, Func_Body);

      Set_Renaming_TSS (RAS_Type, Fnam, Name_uTypeCode);
   end Add_RAS_TypeCode;

   -----------------------
   -- Add_RAST_Features --
   -----------------------

   procedure Add_RAST_Features (Vis_Decl : Node_Id) is
      RAS_Type : constant Entity_Id :=
        Equivalent_Type (Defining_Identifier (Vis_Decl));

      Spec : constant Node_Id :=
        Specification (Unit (Enclosing_Lib_Unit_Node (Vis_Decl)));
      Decls : List_Id := Private_Declarations (Spec);

   begin
      pragma Assert (No (TSS (RAS_Type, Name_uRAS_Access)));

      Add_RAS_Dereference_TSS (Vis_Decl);
      Add_RAS_Access_TSS (Vis_Decl);

      if No (Decls) then
         Decls := Visible_Declarations (Spec);
      end if;

      Add_RAS_From_Any (RAS_Type, Decls);
      Add_RAS_TypeCode (RAS_Type, Decls);
      Add_RAS_To_Any (RAS_Type, Decls);
      --  To_Any uses TypeCode, and therefore needs to
      --  be generated last.
   end Add_RAST_Features;

   -----------------------------------------
   -- Add_Receiving_Stubs_To_Declarations --
   -----------------------------------------

   procedure Add_Receiving_Stubs_To_Declarations
     (Pkg_Spec : in Node_Id;
      Decls    : in List_Id)
   is
      Loc : constant Source_Ptr := Sloc (Pkg_Spec);

      Pkg_RPC_Receiver             : Node_Id;
      Pkg_RPC_Receiver_Object      : Node_Id;

      Pkg_RPC_Receiver_Body        : Node_Id;
      Pkg_RPC_Receiver_Decls       : List_Id;
      Pkg_RPC_Receiver_Stmts       : List_Id;
      Pkg_RPC_Receiver_Cases       : constant List_Id := New_List;
      --  A Pkg_RPC_Receiver is built to decode the request

      Request                     : Node_Id;
      --  Request object received from neutral layer

      Subp_Id                     : Node_Id;
      --  Subprogram identifier as received from the neutral
      --  distribution core.

      Is_Local : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('L'));
      Local_Address : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      --  Address of a local subprogram designated by a
      --  reference corresponding to a RAS.

      Subp_Index : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('I'));
      --  Internal index as determined by matching either the
      --  method name from the request structure, or the local
      --  subprogram address (in case of a RAS).

      Dispatch_On_Address : constant List_Id := New_List;
      Dispatch_On_Name    : constant List_Id := New_List;

      Current_Declaration       : Node_Id;
      Current_Stubs             : Node_Id;
      Current_Subprogram_Number : Int := 0;

      Subp_Info_Array : constant Entity_Id :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('I'));

      Subp_Info_List : constant List_Id := New_List;

      Dummy_Register_Name : Name_Id;
      Dummy_Register_Spec : Node_Id;
      Dummy_Register_Decl : Node_Id;
      Dummy_Register_Body : Node_Id;

      All_Calls_Remote_E : Entity_Id;

   begin
      --  Building receiving stubs consist in several operations:

      --    - a package RPC receiver must be built. This subprogram
      --      will get a Subprogram_Id from the incoming stream
      --      and will dispatch the call to the right subprogram

      --    - a receiving stub for any subprogram visible in the package
      --      spec. This stub will read all the parameters from the stream,
      --      and put the result as well as the exception occurrence in the
      --      output stream

      --    - a dummy package with an empty spec and a body made of an
      --      elaboration part, whose job is to register the receiving
      --      part of this RCI package on the name server. This is done
      --      by calling System.Partition_Interface.Register_Receiving_Stub

      Pkg_RPC_Receiver :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('H'));

      --  The RPC receiver subprogram

      Build_RPC_Receiver_Body (
        RPC_Receiver => Pkg_RPC_Receiver,
        Request      => Request,
        Subp_Id      => Subp_Id,
        Stmts        => Pkg_RPC_Receiver_Stmts,
        Decl         => Pkg_RPC_Receiver_Body);
      Pkg_RPC_Receiver_Decls := Declarations (Pkg_RPC_Receiver_Body);

      --  Extract local address information from the target reference:
      --  if non-null, that means that this is a reference that denotes
      --  one particular operation, and hence that the operation name
      --  must not be taken into account for dispatching.

      Append_To (Pkg_RPC_Receiver_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Is_Local,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Boolean, Loc)));
      Append_To (Pkg_RPC_Receiver_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Local_Address,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Address), Loc)));
      Append_To (Pkg_RPC_Receiver_Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Subp_Index,
          Object_Definition   =>
            New_Occurrence_Of (Standard_Integer, Loc)));

      Append_To (Pkg_RPC_Receiver_Stmts,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Get_Local_Address), Loc),
          Parameter_Associations => New_List (
            Make_Selected_Component (Loc,
              Prefix =>
                New_Occurrence_Of (Request, Loc),
              Selector_Name =>
                Make_Identifier (Loc, Name_Target)),
            New_Occurrence_Of (Is_Local, Loc),
            New_Occurrence_Of (Local_Address, Loc))));

      --  Determine whether the reference that was used to make
      --  the call was the base RCI reference (in which case
      --  Local_Address is 0, and the method identifier from the
      --  request must be used to determine which subprogram is
      --  called) or a reference identifying one particular subprogram
      --  (in which case Local_Address is the address of that
      --  subprogram, and the method name from the request is
      --  ignored).
      --  In each case, cascaded elsifs are used to determine the
      --  proper subprogram index. Using hash tables might be
      --  more efficient.

      Append_To (Pkg_RPC_Receiver_Stmts,
        Make_Implicit_If_Statement (Pkg_Spec,
          Condition =>
            Make_Op_Ne (Loc,
              Left_Opnd  => New_Occurrence_Of (Local_Address, Loc),
              Right_Opnd => New_Occurrence_Of (RTE (RE_Null_Address), Loc)),
          Then_Statements => New_List (
            Make_Implicit_If_Statement (Pkg_Spec,
              Condition =>
                New_Occurrence_Of (Standard_False, Loc),
              Then_Statements => New_List (
                Make_Null_Statement (Loc)),
              Elsif_Parts =>
                Dispatch_On_Address)),
          Else_Statements => New_List (
            Make_Implicit_If_Statement (Pkg_Spec,
              Condition =>
                New_Occurrence_Of (Standard_False, Loc),
              Then_Statements => New_List (
                Make_Null_Statement (Loc)),
              Elsif_Parts =>
                Dispatch_On_Name))));

      --  For each subprogram, the receiving stub will be built and a
      --  case statement will be made on the Subprogram_Id to dispatch
      --  to the right subprogram.

      Overload_Counter_Table.Reset;
      Reserve_NamingContext_Methods;

      Current_Declaration := First (Visible_Declarations (Pkg_Spec));

      while Current_Declaration /= Empty loop

         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            declare
               Loc : constant Source_Ptr :=
                 Sloc (Current_Declaration);
               --  While specifically processing Current_Declaration,
               --  use its Sloc as the location of all generated
               --  nodes.

               Subp_Def       : constant Entity_Id
                 := Defining_Unit_Name (
                      Specification (Current_Declaration));

               Subp_Dist_Name : constant Entity_Id
                 := Make_Defining_Identifier (Loc,
                      New_External_Name (
                        Related_Id   => Chars (Subp_Def),
                        Suffix       => 'D',
                        Suffix_Index => -1));

               Subp_Val       : String_Id;
               Case_Stmts     : List_Id;

            begin
               Current_Stubs :=
                 Build_Subprogram_Receiving_Stubs
                   (Vis_Decl     => Current_Declaration,
                    Asynchronous =>
                      Nkind (Specification (Current_Declaration)) =
                          N_Procedure_Specification
                        and then Is_Asynchronous
                          (Defining_Unit_Name (Specification
                             (Current_Declaration))));

               Append_To (Decls, Current_Stubs);
               Analyze (Current_Stubs);

               --  Actuals :=
               --    New_List (New_Occurrence_Of (Stream_Parameter, Loc));

               if Nkind (Specification (Current_Declaration))
                   = N_Function_Specification
                 or else
                   not Is_Asynchronous (
                     Defining_Entity (Specification (Current_Declaration)))
               then
                  --  An asynchronous procedure does not want an output
                  --  parameter, since no result and no exception will
                  --  ever be returned.

--                 Append_To (Actuals,
--                   New_Occurrence_Of (Result_Parameter, Loc));
                  null;
               end if;


               --  Add subprogram to subprograms table for this receiver

               Assign_Subprogram_Identifier (Subp_Def, Subp_Val);

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Subp_Dist_Name,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (
                     Standard_String, Loc),
                   Expression          =>
                     Make_String_Literal (Loc, Subp_Val)));
               Analyze (Last (Decls));

               Append_To (Subp_Info_List,
                 Make_Component_Association (Loc,
                   Choices => New_List (
                     Make_Integer_Literal (Loc,
                       Current_Subprogram_Number)),
                   Expression =>
                     Make_Aggregate (Loc,
                       Expressions => New_List (
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (
                               Subp_Dist_Name, Loc),
                           Attribute_Name => Name_Address),
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (
                               Subp_Dist_Name, Loc),
                           Attribute_Name => Name_Length),
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Subp_Def, Loc),
                           Attribute_Name => Name_Address)))));

               Case_Stmts := New_List (
                 Make_Procedure_Call_Statement (Loc,
                    Name                   =>
                      New_Occurrence_Of (
                        Defining_Entity (Current_Stubs), Loc),
                    Parameter_Associations =>
                      New_List (New_Occurrence_Of (Request, Loc))));

               if Nkind (Specification (Current_Declaration))
                   = N_Function_Specification
                 or else
                   not Is_Asynchronous (
                     Defining_Entity (Specification (Current_Declaration)))
               then
                  Append_To (Case_Stmts, Make_Return_Statement (Loc));
               end if;

               Append_To (Dispatch_On_Name,
                 Make_Elsif_Part (Loc,
                   Condition =>
                     Make_Function_Call (Loc,
                       Name =>
                         New_Occurrence_Of (RTE (RE_Caseless_String_Eq), Loc),
                       Parameter_Associations => New_List (
                         New_Occurrence_Of (Subp_Id, Loc),
                         New_Occurrence_Of (Subp_Dist_Name, Loc))),
                   Then_Statements => New_List (
                     Make_Assignment_Statement (Loc,
                       New_Occurrence_Of (Subp_Index, Loc),
                       Make_Integer_Literal (Loc,
                          Current_Subprogram_Number)))));
               Append_To (Dispatch_On_Address,
                 Make_Elsif_Part (Loc,
                   Condition =>
                     Make_Op_Eq (Loc,
                       Left_Opnd  =>
                         New_Occurrence_Of (Local_Address, Loc),
                       Right_Opnd =>
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (Subp_Def, Loc),
                           Attribute_Name => Name_Address)),
                   Then_Statements => New_List (
                     Make_Assignment_Statement (Loc,
                       New_Occurrence_Of (Subp_Index, Loc),
                       Make_Integer_Literal (Loc,
                          Current_Subprogram_Number)))));
--             Append_To (Pkg_RPC_Receiver_Cases,
--                   Make_Implicit_If_Statement (Pkg_Spec,
--                     Condition =>
--                       Make_Function_Call (Loc,
--                         Name =>
--                 New_Occurrence_Of (RTE (RE_Caseless_String_Eq), Loc),
--                         Parameter_Associations => New_List (
--                           New_Occurrence_Of (Subp_Id, Loc),
--                           New_Occurrence_Of (Subp_Dist_Name, Loc))),
--                     Then_Statements =>
--                       Case_Stmts));
               Append_To (Pkg_RPC_Receiver_Cases,
                 Make_Case_Statement_Alternative (Loc,
                   Discrete_Choices =>
                     New_List (
                       Make_Integer_Literal (Loc,
                          Current_Subprogram_Number)),
                   Statements       =>
                     Case_Stmts));
            end;

            Current_Subprogram_Number
              := Current_Subprogram_Number + 1;

         end if;

         Next (Current_Declaration);
      end loop;

      --  If we receive an invalid Subprogram_Id, it is best to do nothing
      --  rather than raising an exception since we do not want someone
      --  to crash a remote partition by sending invalid subprogram ids.
      --  This is consistent with the other parts of the case statement
      --  since even in presence of incorrect parameters in the stream,
      --  every exception will be caught and (if the subprogram is not an
      --  APC) put into the result stream and sent away.

      Append_To (Pkg_RPC_Receiver_Cases,
        Make_Case_Statement_Alternative (Loc,
          Discrete_Choices =>
            New_List (Make_Others_Choice (Loc)),
          Statements       =>
            New_List (Make_Null_Statement (Loc))));

      Append_To (Pkg_RPC_Receiver_Stmts,
        Make_Case_Statement (Loc,
          Expression   =>
            New_Occurrence_Of (Subp_Index, Loc),
          Alternatives => Pkg_RPC_Receiver_Cases));
--        Append_List_To (Pkg_RPC_Receiver_Stmts,
--          Pkg_RPC_Receiver_Cases);

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Subp_Info_Array,
          Constant_Present    => True,
          Aliased_Present     => True,
          Object_Definition   =>
--              Make_Unconstrained_Array_Definition (Loc,
--                Subtype_Marks => New_List (
--                  New_Occurrence_Of (Standard_Integer, Loc)),
--                Subtype_Indication =>
--                  New_Occurrence_Of (RTE (RE_RCI_Subp_Info), Loc)),
            New_Occurrence_Of (RTE (RE_RCI_Subp_Info_Array), Loc),
          Expression          =>
            Make_Aggregate (Loc,
              Component_Associations => Subp_Info_List)));
      Analyze (Last (Decls));

      Append_To (Decls, Pkg_RPC_Receiver_Body);
      Analyze (Last (Decls));

      Pkg_RPC_Receiver_Object :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, New_Internal_Name ('R')),
          Aliased_Present     => True,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Servant), Loc));
      Append_To (Decls, Pkg_RPC_Receiver_Object);
      Analyze (Last (Decls));

      --  Construction of the dummy package used to initialize the
      --  Handler field of the Pkg_Receiver_Object, and then register
      --  the package receiving stubs on the nameserver.

      Dummy_Register_Name := New_Internal_Name ('P');

      Dummy_Register_Spec :=
        Make_Package_Specification (Loc,
          Defining_Unit_Name   =>
            Make_Defining_Identifier (Loc, Dummy_Register_Name),
          Visible_Declarations => No_List,
          End_Label => Empty);

      Dummy_Register_Decl :=
        Make_Package_Declaration (Loc,
          Specification => Dummy_Register_Spec);

      Append_To (Decls, Dummy_Register_Decl);
      Analyze (Dummy_Register_Decl);

      if Has_All_Calls_Remote (Defining_Unit_Name (Pkg_Spec)) then
         All_Calls_Remote_E := Standard_True;
      else
         All_Calls_Remote_E := Standard_False;
      end if;

      Dummy_Register_Body :=
        Make_Package_Body (Loc,
          Defining_Unit_Name         =>
            Make_Defining_Identifier (Loc, Dummy_Register_Name),
          Declarations               => No_List,

          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (
                Make_Procedure_Call_Statement (Loc,
                  Name                   =>
                    New_Occurrence_Of
                       (RTE (RE_Register_Pkg_Receiving_Stub), Loc),
                  Parameter_Associations => New_List (

                     --  Name
                    Make_String_Literal (Loc,
                      Strval => Get_Pkg_Name_String_Id (Pkg_Spec)),

                     --  Version
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of
                           (Defining_Entity (Pkg_Spec), Loc),
                      Attribute_Name =>
                        Name_Version),

                     --  Handler
                    Make_Attribute_Reference (Loc,
                      Prefix          =>
                        New_Occurrence_Of (Pkg_RPC_Receiver, Loc),
                      Attribute_Name  => Name_Access),

                     --  Receiver
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (
                          Defining_Identifier (
                            Pkg_RPC_Receiver_Object), Loc),
                      Attribute_Name =>
                        Name_Access),

                     --  Subp_Info
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (Subp_Info_Array, Loc),
                      Attribute_Name =>
                        Name_Access),

                     --  Is_All_Calls_Remote
                    New_Occurrence_Of (All_Calls_Remote_E, Loc))))));

      Append_To (Decls, Dummy_Register_Body);
      Analyze (Dummy_Register_Body);
   end Add_Receiving_Stubs_To_Declarations;

   -------------------
   -- Add_Stub_Type --
   -------------------

   procedure Add_Stub_Type
     (Designated_Type     : in Entity_Id;
      RACW_Type           : in Entity_Id;
      Decls               : in List_Id;
      Stub_Type           : out Entity_Id;
      Stub_Type_Access    : out Entity_Id;
      Object_RPC_Receiver : out Entity_Id;
      Existing            : out Boolean)
   is
      Loc : constant Source_Ptr := Sloc (RACW_Type);

      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Designated_Type);

      Stub_Type_Declaration           : Node_Id;
      Stub_Type_Access_Declaration    : Node_Id;
      Object_RPC_Receiver_Declaration : Node_Id;

   begin
      if Stub_Elements /= Empty_Stub_Structure then
         Stub_Type           := Stub_Elements.Stub_Type;
         Stub_Type_Access    := Stub_Elements.Stub_Type_Access;
         Object_RPC_Receiver := Stub_Elements.Object_RPC_Receiver;
         Existing            := True;
         return;
      end if;

      Existing             := False;
      Stub_Type            :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Stub_Type_Access     :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      Object_RPC_Receiver  :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

      Stubs_Table.Set (Designated_Type,
        (Stub_Type            => Stub_Type,
         Stub_Type_Access     => Stub_Type_Access,
         Object_RPC_Receiver  => Object_RPC_Receiver,
         RACW_Type            => RACW_Type));

      --  The stub type definition below must match exactly the one in
      --  s-parint.ads, since unchecked conversions will be used in
      --  s-parint.adb to modify pointers passed to Get_Unique_Remote_Pointer.

      Stub_Type_Declaration :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Stub_Type,
          Type_Definition     =>
            Make_Record_Definition (Loc,
              Tagged_Present  => True,
              Limited_Present => True,
              Component_List  =>
                Make_Component_List (Loc,
                  Component_Items => New_List (

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc, Name_Origin),
                      Subtype_Indication  =>
                        New_Occurrence_Of (RTE (RE_Partition_ID), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc, Name_Receiver),
                      Subtype_Indication  =>
                        New_Occurrence_Of (RTE (RE_Unsigned_64), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc, Name_Target),
                      Subtype_Indication  =>
                        New_Occurrence_Of (RTE (RE_Entity_Ptr), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc, Name_Addr),
                      Subtype_Indication  =>
                        New_Occurrence_Of (RTE (RE_Address), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc, Name_Asynchronous),
                      Subtype_Indication  =>
                        New_Reference_To (Standard_Boolean, Loc))))));

      Append_To (Decls, Stub_Type_Declaration);
      Analyze (Stub_Type_Declaration);

      --  This is in no way a type derivation, but we fake it to make
      --  sure that the dispatching table gets built with the corresponding
      --  primitive operations at the right place.

      Derive_Subprograms (Parent_Type  => Designated_Type,
                          Derived_Type => Stub_Type);

      Stub_Type_Access_Declaration :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Stub_Type_Access,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              Subtype_Indication => New_Occurrence_Of (Stub_Type, Loc)));

      Append_To (Decls, Stub_Type_Access_Declaration);
      Analyze (Stub_Type_Access_Declaration);

      Object_RPC_Receiver_Declaration :=
        Make_Object_Declaration (Loc,
          Defining_Identifier =>
            Object_RPC_Receiver,
          Aliased_Present     => True,
          Object_Definition   =>
            New_Occurrence_Of (RTE (RE_Servant), Loc));
      --  During the 2nd phase of analysis, the RPC receiver
      --  subprogram is constructed, and must be assigned into
      --  the Handler component of this object.

--        Make_Procedure_Specification (Loc,
--              Defining_Unit_Name       => Object_RPC_Receiver,
--              Parameter_Specifications => New_List (
--                Make_Parameter_Specification (Loc,
--                  Defining_Identifier => RPC_Receiver_Stream,
--                  Parameter_Type      =>
--                    Make_Access_Definition (Loc,
--                      Subtype_Mark =>
--                 New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc))),

--                Make_Parameter_Specification (Loc,
--                  Defining_Identifier => RPC_Receiver_Result,
--                  Parameter_Type      =>
--                    Make_Access_Definition (Loc,
--                      Subtype_Mark =>
--                        New_Occurrence_Of
--                          (RTE (RE_Params_Stream_Type), Loc))))));

      Append_To (Decls, Object_RPC_Receiver_Declaration);
   end Add_Stub_Type;

   ---------------------------------
   -- Build_General_Calling_Stubs --
   ---------------------------------

   procedure Build_General_Calling_Stubs
     (Decls                     : List_Id;
      Statements                : List_Id;
      Target_Object             : Node_Id;
      Subprogram_Id             : Node_Id;
      Asynchronous              : Node_Id   := Empty;
      Is_Known_Asynchronous     : Boolean   := False;
      Is_Known_Non_Asynchronous : Boolean   := False;
      Is_Function               : Boolean;
      Spec                      : Node_Id;
      Stub_Type                 : Entity_Id := Empty;
      RACW_Type                 : Entity_Id := Empty;
      Nod                       : Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nod);

      Arguments : Node_Id;
      --  Name of the named values list used to transmit parameters
      --  to the remote package

      Request : Node_Id;
      --  The request object constructed by these stubs.

      Result : Node_Id;
      --  Name of the result named value (in non-APC cases) which get the
      --  result of the remote subprogram.

      Result_TC : Node_Id;
      --  Typecode expression for the result of the request (void
      --  typecode for procedures).

      Exception_Return : Node_Id;
      --  Name of the parameter which will hold the exception sent by the
      --  remote subprogram.

      Current_Parameter : Node_Id;
      --  Current parameter being handled

      Ordered_Parameters_List : constant List_Id :=
                                  Build_Ordered_Parameters_List (Spec);

      Asynchronous_Statements     : List_Id := No_List;
      Non_Asynchronous_Statements : List_Id := No_List;
      --  Statements specifics to the Asynchronous/Non-Asynchronous cases.

      Extra_Formal_Statements : constant List_Id := New_List;
      --  List of statements for extra formal parameters. It will appear after
      --  the regular statements for writing out parameters.

      After_Statements : constant List_Id := New_List;
      --  Statements to be executed after call returns (to assign
      --  in out or out parameter values).

      Etyp : Entity_Id;
      --  The type of the formal parameter being processed.

      Is_Controlling_Formal         : Boolean;
      Is_First_Controlling_Formal   : Boolean;
      First_Controlling_Formal_Seen : Boolean := False;
      --  Controlling formal parameters of distributed object
      --  primitives require special handling, and the first
      --  such parameter needs even more.

   begin
      --  The general form of a calling stub for a given subprogram is:

      --    procedure X (...) is
      --      P : constant Partition_ID := RCI_Cache.Get_Active_Partition_ID;
      --      Stream, Result : aliased System.RPC.Params_Stream_Type (0);
      --    begin
      --       Put_Package_RPC_Receiver_In_Stream; (the package RPC receiver
      --                  comes from RCI_Cache.Get_RCI_Package_Receiver)
      --       Put_Subprogram_Id_In_Stream;
      --       Put_Parameters_In_Stream;
      --       Do_RPC (Stream, Result);
      --       Read_Exception_Occurrence_From_Result; Raise_It;
      --       Read_Out_Parameters_And_Function_Return_From_Stream;
      --    end X;

      --  There are some variations: Do_APC is called for an asynchronous
      --  procedure and the part after the call is completely ommitted
      --  as well as the declaration of Result. For a function call,
      --  'Input is always used to read the result even if it is constrained.

      Request :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => Request,
          Aliased_Present     => False,
          Object_Definition   =>
              New_Occurrence_Of (RTE (RE_Request_Access), Loc)));

      if not Is_Known_Asynchronous then
--           Result_Parameter :=
--             Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

--           Append_To (Decls,
--             Make_Object_Declaration (Loc,
--               Defining_Identifier => Result_Parameter,
--               Aliased_Present     => True,
--               Object_Definition   =>
--                 Make_Subtype_Indication (Loc,
--                   Subtype_Mark =>
--                     New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc),
--                   Constraint   =>
--                     Make_Index_Or_Discriminant_Constraint (Loc,
--                       Constraints =>
--                         New_List (Make_Integer_Literal (Loc, 0))))));
--  XXX unnecessary with PolyORB (result NV is modified in place.)

         Result :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         if Is_Function then
            Result_TC := Build_TypeCode_Call (Loc,
              Etype (Subtype_Mark (Spec)), Decls);
         else
            Result_TC := New_Occurrence_Of (RTE (RE_TC_Void), Loc);
         end if;

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Result,
             Aliased_Present     => False,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_NamedValue), Loc),
             Expression =>
               Make_Aggregate (Loc,
                 Component_Associations => New_List (
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Name)),
                     Expression =>
                       New_Occurrence_Of (RTE (RE_Result_Name), Loc)),
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Argument)),
                     Expression =>
                       Make_Function_Call (Loc,
                         Name =>
                           New_Occurrence_Of (RTE (RE_Get_Empty_Any), Loc),
                         Parameter_Associations => New_List (
                           Result_TC))),
                   Make_Component_Association (Loc,
                     Choices => New_List (
                       Make_Identifier (Loc, Name_Arg_Modes)),
                     Expression =>
                       Make_Integer_Literal (Loc, 0))))));

         Exception_Return :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Exception_Return,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc)));

      else
         Result := Empty;
         Exception_Return := Empty;
      end if;

      --  Initialize and fill in arguments list

      Arguments :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Declare_Create_NVList (Loc, Arguments, Decls, Statements);

      Current_Parameter := First (Ordered_Parameters_List);

      while Current_Parameter /= Empty loop

         if Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type) then
            Is_Controlling_Formal := True;
            Is_First_Controlling_Formal :=
              not First_Controlling_Formal_Seen;
            First_Controlling_Formal_Seen := True;
         else
            Is_Controlling_Formal := False;
            Is_First_Controlling_Formal := False;
         end if;

         if Is_Controlling_Formal then

            --  In the case of a controlling formal argument, we send
            --  its reference.

            Etyp := RACW_Type;

         else
            Etyp := Etype (Parameter_Type (Current_Parameter));
         end if;

         --  The first controlling formal parameter is treated
         --  specially: it is used to set the target object of
         --  the call.

         if not Is_First_Controlling_Formal then

            declare
               Constrained : constant Boolean :=
                               Is_Constrained (Etyp)
                                 or else Is_Elementary_Type (Etyp);

               Any : constant Entity_Id :=
                                 Make_Defining_Identifier (Loc,
                                   New_Internal_Name ('A'));

               Actual_Parameter : constant Node_Id :=
                                    New_Occurrence_Of (
                                      Defining_Identifier (
                                        Current_Parameter), Loc);

               Expr : Node_Id;

            begin
               Etyp := Etype (Parameter_Type (Current_Parameter));
               Set_Etype (Actual_Parameter, Etyp);

--                if In_Present (Current_Parameter)
--                  or else not Out_Present (Current_Parameter)
--                  or else not Constrained
--                then
--                   Append_To (Statements,
--                     Make_Attribute_Reference (Loc,
--                       Prefix         =>
--                         New_Occurrence_Of (Etyp, Loc),
--                       Attribute_Name => Output_From_Constrained
--                             (Constrained),
--                       Expressions    => New_List (
--                         Make_Attribute_Reference (Loc,
--                           Prefix         =>
--                             New_Occurrence_Of (Stream_Parameter, Loc),
--                           Attribute_Name => Name_Access),
--                         New_Occurrence_Of (
--                           Defining_Identifier (Current_Parameter), Loc))));
--                end if;

               if In_Present (Current_Parameter)
                 or else not Out_Present (Current_Parameter)
                 or else not Constrained
                 or else Is_Controlling_Formal
               then
                  --  The parameter has an input value, is constrained
                  --  at runtime by an input value, or is a controlling
                  --  formal parameter (always passed as a reference)
                  --  other than the first one.

                  Expr := Build_To_Any_Call (Actual_Parameter, Decls);
               else
                  Expr := Make_Function_Call (Loc,
                    Name =>
                      New_Occurrence_Of (RTE (RE_Get_Empty_Any), Loc),
                    Parameter_Associations => New_List (
                      Build_TypeCode_Call (Loc, Etyp, Decls)));
               end if;

               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier =>
                     Any,
                   Aliased_Present     => False,
                   Object_Definition   =>
                     New_Occurrence_Of (RTE (RE_Any), Loc),
                   Expression          =>
                     Expr));

               Append_To (Statements,
                 Add_Parameter_To_NVList (Loc,
                   Parameter   => Current_Parameter,
                   NVList      => Arguments,
                   Constrained => Constrained,
                   Any         => Any));

               if Out_Present (Current_Parameter)
                 and then not Is_Controlling_Formal
               then
                  Append_To (After_Statements,
--                      Make_Attribute_Reference (Loc,
--                        Prefix         =>
--                          New_Occurrence_Of (
--                            Etype (Parameter_Type (Current_Parameter)), Loc),

--                        Attribute_Name => Name_Read,

--                        Expressions    => New_List (
--                          Make_Attribute_Reference (Loc,
--                            Prefix         =>
--                              New_Occurrence_Of (Result_Parameter, Loc),
--                            Attribute_Name =>
--                              Name_Access),
--                          New_Occurrence_Of (
--                            Defining_Identifier (Current_Parameter), Loc)))
                    Make_Assignment_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (
                          Defining_Identifier (Current_Parameter), Loc),
                        Expression =>
                          Build_From_Any_Call (
                            Etype (Parameter_Type (Current_Parameter)),
                            Make_Selected_Component (Loc,
                              Prefix =>
                                New_Occurrence_Of (Result, Loc),
                              Selector_Name =>
                                Make_Identifier (Loc, Name_Argument)),
                            Decls)));

               end if;
            end;
         end if;

         --  If the current parameter has a dynamic constrained status,
         --  then this status is transmitted as well.
         --  This should be done for accessibility as well ???

         if Nkind (Parameter_Type (Current_Parameter)) /= N_Access_Definition
           and then Need_Extra_Constrained (Current_Parameter)
         then
            --  In this block, we do not use the extra formal that has been
            --  created because it does not exist at the time of expansion
            --  when building calling stubs for remote access to subprogram
            --  types. We create an extra variable of this type and push it
            --  in the stream after the regular parameters.

            declare
               Extra_Any_Parameter : constant Entity_Id :=
                                   Make_Defining_Identifier
                                     (Loc, New_Internal_Name ('P'));

            begin
--                Append_To (Decls,
--                  Make_Object_Declaration (Loc,
--                    Defining_Identifier => Extra_Parameter,
--                    Constant_Present    => True,
--                    Object_Definition   =>
--                      New_Occurrence_Of (Standard_Boolean, Loc),
--                    Expression          =>
--                      Make_Attribute_Reference (Loc,
--                        Prefix         =>
--                          New_Occurrence_Of (
--                            Defining_Identifier (Current_Parameter), Loc),
--                        Attribute_Name => Name_Constrained)));


               Append_To (Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier =>
                     Extra_Any_Parameter,
                   Aliased_Present     => False,
                   Object_Definition   =>
                     New_Occurrence_Of (RTE (RE_Any), Loc),
                   Expression          =>
                     Build_To_Any_Call (
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (
                             Defining_Identifier (Current_Parameter), Loc),
                         Attribute_Name => Name_Constrained),
                       Decls)));
               Append_To (Extra_Formal_Statements,
                 Add_Parameter_To_NVList (Loc,
                   Parameter   => Extra_Any_Parameter,
                   NVList      => Arguments,
                   Constrained => True,
                   Any         => Extra_Any_Parameter));
            end;
         end if;

         Next (Current_Parameter);
      end loop;

      --  Append the formal statements list to the statements

      Append_List_To (Statements, Extra_Formal_Statements);

      Append_To (Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Request_Create), Loc),
          Parameter_Associations => New_List (
            Target_Object,
            Subprogram_Id,
            New_Occurrence_Of (Arguments, Loc),
            New_Occurrence_Of (Result, Loc),
            New_Occurrence_Of (RTE (RE_Nil_Exc_List), Loc))));

      Append_To (Parameter_Associations (Last (Statements)),
            New_Occurrence_Of (Request, Loc));

      if not Is_Known_Non_Asynchronous then

--           --  Build the call to System.RPC.Do_APC

--           Asynchronous_Statements := New_List (
--             Make_Procedure_Call_Statement (Loc,
--               Name                   =>
--                 New_Occurrence_Of (RTE (RE_Do_Apc), Loc),
--               Parameter_Associations => New_List (
--                 New_Occurrence_Of (Target_Partition, Loc),
--                 Make_Attribute_Reference (Loc,
--                   Prefix         =>
--                     New_Occurrence_Of (Stream_Parameter, Loc),
--                   Attribute_Name =>
--                     Name_Access))));
         Asynchronous_Statements := New_List (
           Make_Null_Statement (Loc));
         --  XXX Asynch calls not supported yet.
      else
         Asynchronous_Statements := No_List;
      end if;

      if not Is_Known_Asynchronous then

         Non_Asynchronous_Statements := New_List (
            Make_Procedure_Call_Statement (Loc,
              Name                   =>
                New_Occurrence_Of (RTE (RE_Request_Invoke), Loc),
              Parameter_Associations => New_List (
                New_Occurrence_Of (Request, Loc))));

--          --  Read the exception occurrence from the result stream and
--          --  reraise it. It does no harm if this is a Null_Occurrence since
--          --  this does nothing.

--          Append_To (Non_Asynchronous_Statements,
--            Make_Attribute_Reference (Loc,
--              Prefix         =>
--                New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),

--              Attribute_Name =>
--                Name_Read,

--              Expressions    => New_List (
--                Make_Attribute_Reference (Loc,
--                  Prefix         =>
--                    New_Occurrence_Of (Result_Parameter, Loc),
--                  Attribute_Name =>
--                    Name_Access),
--                New_Occurrence_Of (Exception_Return, Loc))));

--          Append_To (Non_Asynchronous_Statements,
--            Make_Procedure_Call_Statement (Loc,
--              Name                   =>
--                New_Occurrence_Of (RTE (RE_Reraise_Occurrence), Loc),
--              Parameter_Associations => New_List (
--                New_Occurrence_Of (Exception_Return, Loc))));

         if Is_Function then

            --  If this is a function call, then read the value and return
            --  it. The return value is written/read using 'Output/'Input.

            Append_To (Non_Asynchronous_Statements,
              Make_Tag_Check (Loc,
                Make_Return_Statement (Loc,
               --  Expression =>
               --   Make_Attribute_Reference (Loc,
               --     Prefix         =>
               --       New_Occurrence_Of (
               --         Etype (Subtype_Mark (Spec)), Loc),

               --     Attribute_Name => Name_Input,

               --     Expressions    => New_List (
               --       Make_Attribute_Reference (Loc,
               --         Prefix         =>
               --           New_Occurrence_Of (Result_Parameter, Loc),
               --         Attribute_Name => Name_Access)))
                    Build_From_Any_Call (
                      Etype (Subtype_Mark (Spec)),
                      Make_Selected_Component (Loc,
                        Prefix =>
                          New_Occurrence_Of (Result, Loc),
                        Selector_Name =>
                          Make_Identifier (Loc, Name_Argument)),
                      Decls))));
         end if;
      end if;

      Append_List_To (Non_Asynchronous_Statements,
        After_Statements);

      if Is_Known_Asynchronous then
         Append_List_To (Statements, Asynchronous_Statements);

      elsif Is_Known_Non_Asynchronous then
         Append_List_To (Statements, Non_Asynchronous_Statements);

      else
         pragma Assert (Asynchronous /= Empty);
--           Prepend_To (Asynchronous_Statements,
--             Make_Attribute_Reference (Loc,
--               Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
--               Attribute_Name => Name_Write,
--               Expressions    => New_List (
--                 Make_Attribute_Reference (Loc,
--               Prefix         => New_Occurrence_Of (Stream_Parameter, Loc),
--               Attribute_Name => Name_Access),
--                 New_Occurrence_Of (Standard_True, Loc))));
--           Prepend_To (Non_Asynchronous_Statements,
--             Make_Attribute_Reference (Loc,
--               Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
--               Attribute_Name => Name_Write,
--               Expressions    => New_List (
--                 Make_Attribute_Reference (Loc,
--               Prefix         => New_Occurrence_Of (Stream_Parameter, Loc),
--               Attribute_Name => Name_Access),
--                 New_Occurrence_Of (Standard_False, Loc))));
         Append_To (Statements,
           Make_Implicit_If_Statement (Nod,
             Condition       => Asynchronous,
             Then_Statements => Asynchronous_Statements,
             Else_Statements => Non_Asynchronous_Statements));
      end if;
   end Build_General_Calling_Stubs;

   -----------------------------------
   -- Build_Ordered_Parameters_List --
   -----------------------------------

   function Build_Ordered_Parameters_List (Spec : Node_Id) return List_Id is
      Constrained_List   : List_Id;
      Unconstrained_List : List_Id;
      Current_Parameter  : Node_Id;

   begin
      if not Present (Parameter_Specifications (Spec)) then
         return New_List;
      end if;

      Constrained_List   := New_List;
      Unconstrained_List := New_List;

      --  Loop through the parameters and add them to the right list

      Current_Parameter := First (Parameter_Specifications (Spec));
      while Current_Parameter /= Empty loop

         if Nkind (Parameter_Type (Current_Parameter)) = N_Access_Definition
             or else
           Is_Constrained (Etype (Parameter_Type (Current_Parameter)))
             or else
           Is_Elementary_Type (Etype (Parameter_Type (Current_Parameter)))
         then
            Append_To (Constrained_List, New_Copy (Current_Parameter));
         else
            Append_To (Unconstrained_List, New_Copy (Current_Parameter));
         end if;

         Next (Current_Parameter);
      end loop;

      --  Unconstrained parameters are returned first

      Append_List_To (Unconstrained_List, Constrained_List);

      return Unconstrained_List;

   end Build_Ordered_Parameters_List;

   ----------------------------------
   -- Build_Passive_Partition_Stub --
   ----------------------------------

   procedure Build_Passive_Partition_Stub (U : Node_Id) is
      Pkg_Spec : Node_Id;
      L        : List_Id;
      Reg      : Node_Id;
      Loc      : constant Source_Ptr := Sloc (U);
      Dist_OK  : Entity_Id;

   begin
      --  Verify that the implementation supports distribution, by accessing
      --  a type defined in the proper version of system.rpc

      Dist_OK := RTE (RE_Params_Stream_Type);

      --  Use body if present, spec otherwise

      if Nkind (U) = N_Package_Declaration then
         Pkg_Spec := Specification (U);
         L := Visible_Declarations (Pkg_Spec);
      else
         Pkg_Spec := Parent (Corresponding_Spec (U));
         L := Declarations (U);
      end if;

      Reg :=
        Make_Procedure_Call_Statement (Loc,
          Name                   =>
            New_Occurrence_Of (RTE (RE_Register_Passive_Package), Loc),
          Parameter_Associations => New_List (
            Make_String_Literal (Loc, Get_Pkg_Name_String_Id (Pkg_Spec)),
            Make_Attribute_Reference (Loc,
              Prefix         =>
                New_Occurrence_Of (Defining_Entity (Pkg_Spec), Loc),
              Attribute_Name =>
                Name_Version)));
      Append_To (L, Reg);
      Analyze (Reg);
   end Build_Passive_Partition_Stub;

   -----------------------------
   -- Build_RPC_Receiver_Body --
   -----------------------------

   procedure Build_RPC_Receiver_Body
     (RPC_Receiver :     Entity_Id;
      Request      : out Entity_Id;
      Subp_Id      : out Entity_Id;
      Stmts        : out List_Id;
      Decl         : out Node_Id)
   is
      Loc : constant Source_Ptr := Sloc (RPC_Receiver);

      Pkg_RPC_Receiver_Spec : Node_Id;
      Pkg_RPC_Receiver_Decls       : List_Id;
   begin
      Request := Make_Defining_Identifier (Loc, Name_R);

      Pkg_RPC_Receiver_Spec :=
        Build_RPC_Receiver_Specification (
          RPC_Receiver      => RPC_Receiver,
          Request_Parameter => Request);

      Subp_Id := Make_Defining_Identifier (Loc, Name_P);

      Pkg_RPC_Receiver_Decls := New_List (
        Make_Object_Declaration (Loc,
          Defining_Identifier => Subp_Id,
          Constant_Present    => True,
          Object_Definition   =>
            New_Occurrence_Of (Standard_String, Loc),
          Expression          =>
            Make_Function_Call (Loc,
              Name =>
                New_Occurrence_Of (RTE (RE_To_Standard_String), Loc),
              Parameter_Associations => New_List (
                Make_Selected_Component (Loc,
                  Prefix =>
                    New_Occurrence_Of (Request, Loc),
                  Selector_Name =>
                    Make_Identifier (Loc, Name_Operation))))));

      Stmts := New_List;

      Decl :=
        Make_Subprogram_Body (Loc,
          Specification              => Pkg_RPC_Receiver_Spec,
          Declarations               => Pkg_RPC_Receiver_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Stmts));

   end Build_RPC_Receiver_Body;

   --------------------------------------
   -- Build_RPC_Receiver_Specification --
   --------------------------------------

   function Build_RPC_Receiver_Specification
     (RPC_Receiver      : Entity_Id;
      Request_Parameter : Entity_Id)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (RPC_Receiver);

   begin
      return
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       => RPC_Receiver,
          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Request_Parameter,
              Parameter_Type      =>
                New_Occurrence_Of (
                  RTE (RE_Request_Access), Loc))));

--          Make_Parameter_Specification (Loc,
--                Defining_Identifier => Stream_Parameter,
--                Parameter_Type      =>
--                  Make_Access_Definition (Loc,
--                    Subtype_Mark =>
--                      New_Occurrence_Of (RTE (RE_Params_Stream_Type), Loc))),

--              Make_Parameter_Specification (Loc,
--                Defining_Identifier => Result_Parameter,
--                Parameter_Type      =>
--                  Make_Access_Definition (Loc,
--                    Subtype_Mark =>
--                      New_Occurrence_Of
--                        (RTE (RE_Params_Stream_Type), Loc)))));
   end Build_RPC_Receiver_Specification;

   ------------------------------------
   -- Build_Subprogram_Calling_Stubs --
   ------------------------------------

   function Build_Subprogram_Calling_Stubs
     (Vis_Decl                 : Node_Id;
      Subp_Id                  : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Locator                  : Entity_Id := Empty;
      New_Name                 : Name_Id   := No_Name)
      return                     Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Vis_Decl);

      Target_Reference : constant Entity_Id :=
        Make_Defining_Identifier (Loc,
          New_Internal_Name ('T'));
      Target_Object : Node_Id;
      --  Reference to the target object.

      Decls      : constant List_Id := New_List;
      Statements : constant List_Id := New_List;

      Subp_Spec : Node_Id;
      --  The specification of the body

      Controlling_Parameter : Entity_Id := Empty;
      --  RPC_Receiver          : Node_Id;

      Asynchronous_Expr : Node_Id := Empty;

      RCI_Locator : Entity_Id;

      Spec_To_Use : Node_Id;

      procedure Insert_Partition_Check (Parameter : in Node_Id);
      --  Check that the parameter has been elaborated on the same partition
      --  than the controlling parameter (E.4(19)).

      ----------------------------
      -- Insert_Partition_Check --
      ----------------------------

      procedure Insert_Partition_Check (Parameter : in Node_Id) is
         Parameter_Entity  : constant Entity_Id :=
                               Defining_Identifier (Parameter);
         Designated_Object : Node_Id;
         Condition         : Node_Id;

      begin
         --  The expression that will be built is of the form:
         --    if not (Parameter in Stub_Type and then
         --            Parameter.Origin = Controlling.Origin)
         --    then
         --      raise Constraint_Error;
         --    end if;
         --
         --  Condition contains the reversed condition. Also, Parameter is
         --  dereferenced if it is an access type. We do not check that
         --  Parameter is in Stub_Type since such a check has been inserted
         --  at the point of call already (a tag check since we have multiple
         --  controlling operands).

         if Nkind (Parameter_Type (Parameter)) = N_Access_Definition then
            Designated_Object :=
              Make_Explicit_Dereference (Loc,
                Prefix => New_Occurrence_Of (Parameter_Entity, Loc));
         else
            Designated_Object := New_Occurrence_Of (Parameter_Entity, Loc);
         end if;

         Condition :=
           Make_Op_Eq (Loc,
             Left_Opnd  =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Parameter_Entity, Loc),
               Selector_Name =>
                 Make_Identifier (Loc, Name_Origin)),

             Right_Opnd =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Controlling_Parameter, Loc),
               Selector_Name =>
                 Make_Identifier (Loc, Name_Origin)));

         Append_To (Decls,
           Make_Raise_Constraint_Error (Loc,
             Condition       =>
               Make_Op_Not (Loc, Right_Opnd => Condition),
             Reason => CE_Partition_Check_Failed));
      end Insert_Partition_Check;

   --  Start of processing for Build_Subprogram_Calling_Stubs

   begin
--        Target_Partition :=
--          Make_Defining_Identifier (Loc, New_Internal_Name ('P'));

      Subp_Spec := Copy_Specification (Loc,
        Spec     => Specification (Vis_Decl),
        New_Name => New_Name);

      if Locator = Empty then
         RCI_Locator := RCI_Cache;
         Spec_To_Use := Specification (Vis_Decl);
      else
         RCI_Locator := Locator;
         Spec_To_Use := Subp_Spec;
      end if;

      --  Find a controlling argument if we have a stub type. Also check
      --  if this subprogram can be made asynchronous.

      if Stub_Type /= Empty
         and then Present (Parameter_Specifications (Spec_To_Use))
      then
         declare
            Current_Parameter : Node_Id :=
                                  First (Parameter_Specifications
                                           (Spec_To_Use));
         begin
            while Current_Parameter /= Empty loop

               if
                 Is_RACW_Controlling_Formal (Current_Parameter, Stub_Type)
               then
                  if Controlling_Parameter = Empty then
                     Controlling_Parameter :=
                       Defining_Identifier (Current_Parameter);
                  else
                     Insert_Partition_Check (Current_Parameter);
                  end if;
               end if;

               Next (Current_Parameter);
            end loop;
         end;
      end if;

      if Stub_Type /= Empty then
         pragma Assert (Controlling_Parameter /= Empty);

--           Append_To (Decls,
--             Make_Object_Declaration (Loc,
--               Defining_Identifier => Target_Partition,
--               Constant_Present    => True,
--               Object_Definition   =>
--                 New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

--               Expression          =>
--                 Make_Selected_Component (Loc,
--                   Prefix        =>
--                     New_Occurrence_Of (Controlling_Parameter, Loc),
--                   Selector_Name =>
--                     Make_Identifier (Loc, Name_Origin))));
--           RPC_Receiver :=
--             Make_Selected_Component (Loc,
--               Prefix        =>
--                 New_Occurrence_Of (Controlling_Parameter, Loc),
--               Selector_Name =>
--                 Make_Identifier (Loc, Name_Receiver));
         Append_To (Decls,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Target_Reference,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Object_Ref), Loc)));

         Append_To (Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Set_Ref), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Target_Reference, Loc),
               Make_Selected_Component (Loc,
                 Prefix        =>
                   New_Occurrence_Of (Controlling_Parameter, Loc),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Target)))));
         --  Controlling_Parameter has the same components
         --  as System.PolyORB_Interfance.RACW_Stub_Type.

         Target_Object := New_Occurrence_Of (Target_Reference, Loc);

      else
--           Append_To (Decls,
--             Make_Object_Declaration (Loc,
--               Defining_Identifier => Target_Partition,
--               Constant_Present    => True,
--               Object_Definition   =>
--                 New_Occurrence_Of (RTE (RE_Partition_ID), Loc),

--               Expression          =>
--                 Make_Function_Call (Loc,
--                   Name => Make_Selected_Component (Loc,
--                     Prefix        =>
--                   Make_Identifier (Loc, Chars (RCI_Locator)),
--                     Selector_Name =>
--                   Make_Identifier (Loc, Name_Get_Active_Partition_ID)))));

--           RPC_Receiver :=
--             Make_Selected_Component (Loc,
--               Prefix        =>
--                 Make_Identifier (Loc, Chars (RCI_Locator)),
--               Selector_Name =>
--                 Make_Identifier (Loc, Name_Get_RCI_Package_Receiver));

         Target_Object :=
           Make_Selected_Component (Loc,
             Prefix        =>
               Make_Identifier (Loc, Chars (RCI_Locator)),
             Selector_Name =>
               Make_Identifier (Loc, Name_Get_RCI_Package_Ref));
      end if;

      if Dynamically_Asynchronous then
         Asynchronous_Expr :=
           Make_Selected_Component (Loc,
             Prefix        =>
               New_Occurrence_Of (Controlling_Parameter, Loc),
             Selector_Name =>
               Make_Identifier (Loc, Name_Asynchronous));
      end if;

      Build_General_Calling_Stubs
        (Decls                 => Decls,
         Statements            => Statements,
         Target_Object         => Target_Object,
         Subprogram_Id         => Subp_Id,
         Asynchronous          => Asynchronous_Expr,
         Is_Known_Asynchronous => Asynchronous
                                    and then not Dynamically_Asynchronous,
         Is_Known_Non_Asynchronous
                               => not Asynchronous
                                    and then not Dynamically_Asynchronous,
         Is_Function           => Nkind (Spec_To_Use) =
                                    N_Function_Specification,
         Spec                  => Spec_To_Use,
         Stub_Type             => Stub_Type,
         RACW_Type             => RACW_Type,
         Nod                   => Vis_Decl);

      RCI_Calling_Stubs_Table.Set
        (Defining_Unit_Name (Specification (Vis_Decl)),
         Defining_Unit_Name (Spec_To_Use));

      return
        Make_Subprogram_Body (Loc,
          Specification              => Subp_Spec,
          Declarations               => Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, Statements));
   end Build_Subprogram_Calling_Stubs;

   --------------------------------------
   -- Build_Subprogram_Receiving_Stubs --
   --------------------------------------

   function Build_Subprogram_Receiving_Stubs
     (Vis_Decl                 : Node_Id;
      Asynchronous             : Boolean;
      Dynamically_Asynchronous : Boolean   := False;
      Stub_Type                : Entity_Id := Empty;
      RACW_Type                : Entity_Id := Empty;
      Parent_Primitive         : Entity_Id := Empty)
      return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Vis_Decl);

      Request_Parameter : Node_Id;
      --  See explanations of those in Build_Subprogram_Calling_Stubs

      Outer_Decls : constant List_Id := New_List;
      --  At the outermost level, an NVList and Any's are
      --  declared for all parameters. The Dynamic_Async
      --  flag also needs to be declared there to be visible
      --  from the exception handling code.

      Outer_Statements : constant List_Id := New_List;
      --  Statements that occur priori to the declaration
      --  of the actual parameter variables.

      Decls : constant List_Id := New_List;
      --  All the parameters will get declared before calling the real
      --  subprograms. Also the out parameters will be declared.
      --  At this level, parameters may be unconstrained.

      Statements : constant List_Id := New_List;

      Extra_Formal_Statements : constant List_Id := New_List;
      --  Statements concerning extra formal parameters

      After_Statements : constant List_Id := New_List;
      --  Statements to be executed after the subprogram call

      Inner_Decls : List_Id := No_List;
      --  In case of a function, the inner declarations are needed since
      --  the result may be unconstrained.

      Excep_Handler : Node_Id;
      Excep_Choice  : Entity_Id;
      Excep_Code    : List_Id;

      Parameter_List : constant List_Id := New_List;
      --  List of parameters to be passed to the subprogram.

      First_Controlling_Formal_Seen : Boolean := False;

      Current_Parameter : Node_Id;

      Ordered_Parameters_List : constant List_Id :=
                                  Build_Ordered_Parameters_List
                                    (Specification (Vis_Decl));

      Arguments : Node_Id;
      --  Name of the named values list used to retrieve parameters

      Subp_Spec : Node_Id;
      --  Subprogram specification

      Called_Subprogram : Node_Id;
      --  The subprogram to call

      --  Null_Raise_Statement : Node_Id;

      Dynamic_Async : Entity_Id;

   begin
      if RACW_Type /= Empty then
         Called_Subprogram :=
           New_Occurrence_Of (Parent_Primitive, Loc);
      else
         Called_Subprogram :=
           New_Occurrence_Of (
             Defining_Unit_Name (Specification (Vis_Decl)), Loc);
      end if;

      Request_Parameter :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

      if Dynamically_Asynchronous then
         Dynamic_Async :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('S'));
      else
         Dynamic_Async := Empty;
      end if;

      Arguments :=
        Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
      Declare_Create_NVList (Loc, Arguments, Outer_Decls, Outer_Statements);

--        if not Asynchronous or else Dynamically_Asynchronous then
--           Result_Parameter :=
--             Make_Defining_Identifier (Loc, New_Internal_Name ('S'));

--       --  The first statement after the subprogram call is a statement to
--       --  writes a Null_Occurrence into the result stream.

--           Null_Raise_Statement :=
--             Make_Attribute_Reference (Loc,
--               Prefix         =>
--                 New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
--               Attribute_Name => Name_Write,
--               Expressions    => New_List (
--                 New_Occurrence_Of (Result_Parameter, Loc),
--                 New_Occurrence_Of (RTE (RE_Null_Occurrence), Loc)));

--           if Dynamically_Asynchronous then
--              Null_Raise_Statement :=
--                Make_Implicit_If_Statement (Vis_Decl,
--                  Condition       =>
--                  Make_Op_Not (Loc, New_Occurrence_Of (Dynamic_Async, Loc)),
--                  Then_Statements => New_List (Null_Raise_Statement));
--           end if;

--           Append_To (After_Statements, Null_Raise_Statement);

--        else
--           Result_Parameter := Empty;
--        end if;

      --  XXX REMOVE (useless for PolyORB since by default we have
      --  an empty any in the exception information.)

      --  Loop through every parameter and get its value from the stream. If
      --  the parameter is unconstrained, then the parameter is read using
      --  'Input at the point of declaration.

      Current_Parameter := First (Ordered_Parameters_List);

      while Current_Parameter /= Empty loop

         declare
            Etyp        : Entity_Id;
            Constrained : Boolean;
            Any         : Entity_Id;
            Object      : Entity_Id;
            Expr        : Node_Id := Empty;

            Is_Controlling_Formal : constant Boolean
              := Is_RACW_Controlling_Formal
                   (Current_Parameter, Stub_Type);

            Is_First_Controlling_Formal : Boolean := False;
         begin
            if Is_Controlling_Formal then

               --  Controlling formals in distributed object primitive
               --  operations are handled specially:
               --    - the first controlling formal is used as the
               --      target of the call;
               --    - the remaining controlling formals are transmitted
               --      as RACWs.

               Etyp := RACW_Type;
               Is_First_Controlling_Formal :=
                 not First_Controlling_Formal_Seen;
               First_Controlling_Formal_Seen := True;
            else
               Etyp := Etype (Parameter_Type (Current_Parameter));
            end if;

            Constrained :=
              Is_Constrained (Etyp)
              or else Is_Elementary_Type (Etyp);

            if not Is_First_Controlling_Formal then
               Any := Make_Defining_Identifier (Loc, New_Internal_Name ('A'));
               Append_To (Outer_Decls,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier =>
                     Any,
                   Object_Definition   =>
                     New_Occurrence_Of (RTE (RE_Any), Loc),
                   Expression =>
                     Make_Function_Call (Loc,
                       Name =>
                         New_Occurrence_Of (RTE (RE_Get_Empty_Any), Loc),
                       Parameter_Associations => New_List (
                         Build_TypeCode_Call (Loc, Etyp, Outer_Decls)))));

               Append_To (Outer_Statements,
                 Add_Parameter_To_NVList (Loc,
                   Parameter   => Current_Parameter,
                   NVList      => Arguments,
                   Constrained => Constrained,
                   Any         => Any));
            end if;

            Object := Make_Defining_Identifier (Loc, New_Internal_Name ('P'));
            Set_Ekind (Object, E_Variable);

            if Is_First_Controlling_Formal then
               declare
                  Addr : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      New_Internal_Name ('A'));
                  Is_Local : constant Entity_Id :=
                    Make_Defining_Identifier (Loc,
                      New_Internal_Name ('L'));
               begin

                  --  Special case: obtain the first controlling
                  --  formal from the target of the remote call,
                  --  instead of the argument list.

                  Append_To (Outer_Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Addr,
                      Object_Definition =>
                        New_Occurrence_Of (RTE (RE_Address), Loc)));
                  Append_To (Outer_Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Is_Local,
                      Object_Definition =>
                        New_Occurrence_Of (Standard_Boolean, Loc)));
                  Append_To (Outer_Statements,
                    Make_Procedure_Call_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (
                          RTE (RE_Get_Local_Address), Loc),
                      Parameter_Associations => New_List (
                        Make_Selected_Component (Loc,
                          Prefix =>
                            New_Occurrence_Of (
                              Request_Parameter, Loc),
                          Selector_Name =>
                            Make_Identifier (Loc, Name_Target)),
                        New_Occurrence_Of (Is_Local, Loc),
                        New_Occurrence_Of (Addr, Loc))));

                  Expr := Unchecked_Convert_To (RACW_Type,
                    New_Occurrence_Of (Addr, Loc));
               end;

            elsif In_Present (Current_Parameter)
               or else not Out_Present (Current_Parameter)
               or else not Constrained
            then
               --  If an input parameter is contrained, then its reading is
               --  deferred until the beginning of the subprogram body. If
               --  it is unconstrained, then an expression is built for
               --  the object declaration and the variable is set using
               --  'Input instead of 'Read.

               Expr := Build_From_Any_Call (
                         Etyp, New_Occurrence_Of (Any, Loc), Decls);

               if Constrained then
--                    Append_To (Statements,
--                      Make_Attribute_Reference (Loc,
--                        Prefix         => New_Occurrence_Of (Etyp, Loc),
--                        Attribute_Name => Name_Read,
--                        Expressions    => New_List (
--                          New_Occurrence_Of (Stream_Parameter, Loc),
--                          New_Occurrence_Of (Object, Loc))));
                  Append_To (Statements,
                    Make_Assignment_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (Object, Loc),
                      Expression =>
                         Expr));
                  Expr := Empty;
               else
--                    Expr := Input_With_Tag_Check (Loc,
--                      Var_Type => Etyp,
--                      Stream   => Stream_Parameter);
--                    Append_To (Decls, Expr);
--                    Expr := Make_Function_Call (Loc,
--                      New_Occurrence_Of (Defining_Unit_Name
--                        (Specification (Expr)), Loc));
                  null;
                  --  Expr will be used to initialize (and constrain)
                  --  the parameter when it is declared.
               end if;

            end if;

            --  If we do not have to output the current parameter, then
            --  it can well be flagged as constant. This may allow further
            --  optimizations done by the back end.

            Append_To (Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Object,
                Constant_Present    =>
                  not Constrained and then not Out_Present (Current_Parameter),
                Object_Definition   =>
                  New_Occurrence_Of (Etyp, Loc),
                Expression          => Expr));
            Set_Etype (Object, Etyp);

            --  An out parameter may be written back using a 'Write
            --  attribute instead of a 'Output because it has been
            --  constrained by the parameter given to the caller. Note that
            --  out controlling arguments in the case of a RACW are not put
            --  back in the stream because the pointer on them has not
            --  changed.

            if Out_Present (Current_Parameter)
              and then not Is_Controlling_Formal then
--                 Append_To (After_Statements,
--                   Make_Attribute_Reference (Loc,
--                     Prefix         => New_Occurrence_Of (Etyp, Loc),
--                     Attribute_Name => Name_Write,
--                     Expressions    => New_List (
--                         New_Occurrence_Of (Result_Parameter, Loc),
--                         New_Occurrence_Of (Object, Loc))));

               Append_To (After_Statements,
                 Make_Procedure_Call_Statement (Loc,
                   Name =>
                     New_Occurrence_Of (RTE (RE_Copy_Any_Value), Loc),
                   Parameter_Associations => New_List (
                     New_Occurrence_Of (Any, Loc),
                     Build_To_Any_Call (
                       New_Occurrence_Of (Object, Loc),
                       Decls))));
            end if;

            --  For RACW controlling formals, the Etyp of Object
            --  is always an RACW, even if the parameter is not
            --  of an anonymous access type. In such case, we
            --  need to dereference it at call time.

            if Is_Controlling_Formal then

               if Nkind (Parameter_Type (Current_Parameter)) /=
                 N_Access_Definition
               then
                  Append_To (Parameter_List,
                    Make_Parameter_Association (Loc,
                      Selector_Name             =>
                        New_Occurrence_Of (
                          Defining_Identifier (Current_Parameter), Loc),
                      Explicit_Actual_Parameter =>
                        Make_Explicit_Dereference (Loc,
                          Unchecked_Convert_To (RACW_Type,
                            OK_Convert_To (RTE (RE_Address),
                              New_Occurrence_Of (Object, Loc))))));
               else
                  Append_To (Parameter_List,
                    Make_Parameter_Association (Loc,
                      Selector_Name             =>
                        New_Occurrence_Of (
                          Defining_Identifier (Current_Parameter), Loc),
                      Explicit_Actual_Parameter =>
                        Unchecked_Convert_To (RACW_Type,
                          OK_Convert_To (RTE (RE_Address),
                            New_Occurrence_Of (Object, Loc)))));
               end if;
            else
               Append_To (Parameter_List,
                 Make_Parameter_Association (Loc,
                   Selector_Name             =>
                     New_Occurrence_Of (
                       Defining_Identifier (Current_Parameter), Loc),
                   Explicit_Actual_Parameter =>
                     New_Occurrence_Of (Object, Loc)));
            end if;

            --  If the current parameter needs an extra formal, then read it
            --  from the stream and set the corresponding semantic field in
            --  the variable. If the kind of the parameter identifier is
            --  E_Void, then this is a compiler generated parameter that
            --  doesn't need an extra constrained status.

            --  The case of Extra_Accessibility should also be handled ???

            if Nkind (Parameter_Type (Current_Parameter)) /=
                                                      N_Access_Definition
              and then
                Ekind (Defining_Identifier (Current_Parameter)) /= E_Void
              and then
                Present (Extra_Constrained
                  (Defining_Identifier (Current_Parameter)))
            then
               declare
                  Extra_Parameter : constant Entity_Id :=
                                      Extra_Constrained
                                        (Defining_Identifier
                                          (Current_Parameter));
                  Extra_Any : constant Entity_Id :=
                    Make_Defining_Identifier
                      (Loc, New_Internal_Name ('A'));
                  Formal_Entity : constant Entity_Id :=
                                    Make_Defining_Identifier
                                        (Loc, Chars (Extra_Parameter));

                  Formal_Type : constant Entity_Id :=
                                  Etype (Extra_Parameter);
               begin
                  Append_To (Outer_Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier =>
                        Extra_Any,
                      Object_Definition   =>
                        New_Occurrence_Of (RTE (RE_Any), Loc)));

                  Append_To (Outer_Statements,
                    Add_Parameter_To_NVList (Loc,
                      Parameter   => Extra_Parameter,
                      NVList      => Arguments,
                      Constrained => True,
                      Any         => Extra_Any));

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Formal_Entity,
                      Object_Definition   =>
                        New_Occurrence_Of (Formal_Type, Loc)));

--                    Append_To (Extra_Formal_Statements,
--                      Make_Attribute_Reference (Loc,
--                   Prefix         => New_Occurrence_Of (Formal_Type, Loc),
--                        Attribute_Name => Name_Read,
--                        Expressions    => New_List (
--                          New_Occurrence_Of (Stream_Parameter, Loc),
--                          New_Occurrence_Of (Formal_Entity, Loc))));
                  Append_To (Extra_Formal_Statements,
                    Make_Assignment_Statement (Loc,
                      Name =>
                        New_Occurrence_Of (Extra_Parameter, Loc),
                      Expression =>
                        Build_From_Any_Call (
                          Etype (Extra_Parameter),
                          New_Occurrence_Of (Extra_Any, Loc),
                    Decls)));
                  Set_Extra_Constrained (Object, Formal_Entity);

               end;
            end if;
         end;

         Next (Current_Parameter);
      end loop;

      Append_To (Outer_Statements,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_Request_Arguments), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (Request_Parameter, Loc),
            New_Occurrence_Of (Arguments, Loc))));

      Append_List_To (Statements, Extra_Formal_Statements);

      if Nkind (Specification (Vis_Decl)) = N_Function_Specification then

         --  The remote subprogram is a function. We build an inner block to
         --  be able to hold a potentially unconstrained result in a variable.

         declare
            Etyp   : constant Entity_Id :=
                       Etype (Subtype_Mark (Specification (Vis_Decl)));
            Result : constant Node_Id   :=
                       Make_Defining_Identifier (Loc, New_Internal_Name ('R'));

         begin
            Inner_Decls := New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Result,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Etyp, Loc),
                Expression          =>
                  Make_Function_Call (Loc,
                    Name                   => Called_Subprogram,
                    Parameter_Associations => Parameter_List)));
--              Append_To (After_Statements,
--                Make_Attribute_Reference (Loc,
--                  Prefix         => New_Occurrence_Of (Etyp, Loc),
--                  Attribute_Name => Name_Output,
--                  Expressions    => New_List (
--                    New_Occurrence_Of (Result_Parameter, Loc),
--                    New_Occurrence_Of (Result, Loc))));
            Set_Etype (Result, Etyp);
            Append_To (After_Statements,
              Make_Procedure_Call_Statement (Loc,
                Name =>
                  New_Occurrence_Of (RTE (RE_Set_Result), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (Request_Parameter, Loc),
                  Build_To_Any_Call (
                    New_Occurrence_Of (Result, Loc),
                    Decls))));
            --  A DSA function does not have out or inout arguments.

         end;

         Append_To (Statements,
           Make_Block_Statement (Loc,
             Declarations               => Inner_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => After_Statements)));

      else
         --  The remote subprogram is a procedure. We do not need any inner
         --  block in this case.

         if Dynamically_Asynchronous then
            Append_To (Outer_Decls,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Dynamic_Async,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Boolean, Loc)));

--              Append_To (Statements,
--                Make_Attribute_Reference (Loc,
--              Prefix         => New_Occurrence_Of (Standard_Boolean, Loc),
--                  Attribute_Name => Name_Read,
--                  Expressions    => New_List (
--                    New_Occurrence_Of (Stream_Parameter, Loc),
--                    New_Occurrence_Of (Dynamic_Async, Loc))));
--  XXX TBD asynchronous/assign Dynamic_Async flag.
         end if;

         Append_To (After_Statements,
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Occurrence_Of (RTE (RE_Request_Set_Out), Loc),
             Parameter_Associations => New_List (
               New_Occurrence_Of (Request_Parameter, Loc))));

         Append_To (Statements,
           Make_Procedure_Call_Statement (Loc,
             Name                   => Called_Subprogram,
             Parameter_Associations => Parameter_List));

         Append_List_To (Statements, After_Statements);

      end if;

      Subp_Spec :=
        Make_Procedure_Specification (Loc,
          Defining_Unit_Name       =>
            Make_Defining_Identifier (Loc, New_Internal_Name ('F')),

          Parameter_Specifications => New_List (
            Make_Parameter_Specification (Loc,
              Defining_Identifier => Request_Parameter,
              Parameter_Type      =>
                New_Occurrence_Of (RTE (RE_Request_Access), Loc))));

      if Asynchronous and then not Dynamically_Asynchronous then

         --  An asynchronous procedure wants an exception handler
         --  with an others clause that does nothing.

         Excep_Handler :=
           Make_Exception_Handler (Loc,
             Exception_Choices =>
               New_List (Make_Others_Choice (Loc)),
             Statements        => New_List (
               Make_Null_Statement (Loc)));

      else

         --  In the other cases, if an exception is raised, then the
         --  exception occurrence is copied into the output stream and
         --  no other output parameter is written.

         Excep_Choice :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

--           Excep_Code := New_List (
--             Make_Attribute_Reference (Loc,
--               Prefix         =>
--                 New_Occurrence_Of (RTE (RE_Exception_Occurrence), Loc),
--               Attribute_Name => Name_Write,
--               Expressions    => New_List (
--                 New_Occurrence_Of (Result_Parameter, Loc),
--                 New_Occurrence_Of (Excep_Choice, Loc))));

         Excep_Code := New_List (Make_Null_Statement (Loc));
         --  XXX TBD exceptions!

         if Dynamically_Asynchronous then
            Excep_Code := New_List (
              Make_Implicit_If_Statement (Vis_Decl,
                Condition       => Make_Op_Not (Loc,
                  New_Occurrence_Of (Dynamic_Async, Loc)),
                Then_Statements => Excep_Code));
         end if;
         Excep_Handler :=
           Make_Exception_Handler (Loc,
             Choice_Parameter   => Excep_Choice,
             Exception_Choices  => New_List (Make_Others_Choice (Loc)),
             Statements         => Excep_Code);

      end if;

      Append_To (Outer_Statements,
        Make_Block_Statement (Loc,
          Declarations =>
            Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => Statements)));

      return
        Make_Subprogram_Body (Loc,
          Specification              => Subp_Spec,
          Declarations               => Outer_Decls,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements         => Outer_Statements,
              Exception_Handlers => New_List (Excep_Handler)));

   end Build_Subprogram_Receiving_Stubs;

   ------------------------
   -- Copy_Specification --
   ------------------------

   function Copy_Specification
     (Loc         : Source_Ptr;
      Spec        : Node_Id;
      Object_Type : Entity_Id := Empty;
      Stub_Type   : Entity_Id := Empty;
      New_Name    : Name_Id   := No_Name)
      return        Node_Id
   is
      Parameters : List_Id := No_List;

      Current_Parameter : Node_Id;
      Current_Type      : Node_Id;
      Current_Etype     : Entity_Id;

      Name_For_New_Spec : Name_Id := No_Name;

      New_Identifier : Entity_Id;

   begin
      if New_Name = No_Name
        and then Nkind (Spec) in N_Subprogram_Specification
      then
         Name_For_New_Spec := Chars (Defining_Unit_Name (Spec));
      else
         Name_For_New_Spec := New_Name;
      end if;
      pragma Assert (Name_For_New_Spec /= No_Name);

      if Present (Parameter_Specifications (Spec)) then

         Parameters        := New_List;
         Current_Parameter := First (Parameter_Specifications (Spec));

         while Current_Parameter /= Empty loop

            Current_Type := Parameter_Type (Current_Parameter);

            if Nkind (Current_Type) = N_Access_Definition then
               Current_Etype := Entity (Subtype_Mark (Current_Type));

               if Object_Type = Empty then
                  Current_Type :=
                    Make_Access_Definition (Loc,
                      Subtype_Mark =>
                        New_Occurrence_Of (Current_Etype, Loc));
               else
                  pragma Assert
                    (Root_Type (Current_Etype) = Root_Type (Object_Type));
                  Current_Type :=
                    Make_Access_Definition (Loc,
                      Subtype_Mark => New_Occurrence_Of (Stub_Type, Loc));
               end if;

            else
               Current_Etype := Entity (Current_Type);

               if Object_Type /= Empty
                 and then Current_Etype = Object_Type
               then
                  Current_Type := New_Occurrence_Of (Stub_Type, Loc);
               else
                  Current_Type := New_Occurrence_Of (Current_Etype, Loc);
               end if;
            end if;

            New_Identifier := Make_Defining_Identifier (Loc,
              Chars (Defining_Identifier (Current_Parameter)));

            Append_To (Parameters,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => New_Identifier,
                Parameter_Type      => Current_Type,
                In_Present          => In_Present (Current_Parameter),
                Out_Present         => Out_Present (Current_Parameter),
                Expression          =>
                  New_Copy_Tree (Expression (Current_Parameter))));

            Next (Current_Parameter);
         end loop;
      end if;

      if Nkind (Spec) = N_Function_Specification
        or else Nkind (Spec) = N_Access_Function_Definition
      then
         return
           Make_Function_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc,
                 Chars => Name_For_New_Spec),
             Parameter_Specifications => Parameters,
             Subtype_Mark             =>
               New_Occurrence_Of (Entity (Subtype_Mark (Spec)), Loc));

      else
         return
           Make_Procedure_Specification (Loc,
             Defining_Unit_Name       =>
               Make_Defining_Identifier (Loc,
                 Chars => Name_For_New_Spec),
             Parameter_Specifications => Parameters);
      end if;

   end Copy_Specification;

--     function Copy_Specification
--       (Loc         : Source_Ptr;
--        Spec        : Node_Id;
--        Object_Type : Entity_Id := Empty;
--        Stub_Type   : Entity_Id := Empty;
--        New_Name    : Name_Id   := No_Name)
--        return        Node_Id
--     is
--        Map : constant Elist_Id := New_Elmt_List;
--     begin
--        if Object_Type /= Empty then
--           pragma Assert (Stub_Type /= Empty);
--           Append_Elmt (Node => Object_Type, To => Map);
--           Append_Elmt (Node => Stub_Type,   To => Map);
--        end if;
--
--        if New_Name /= No_Name then
--           Append_Elmt (Node => Defining_Unit_Name (Spec), To => Map);
--           Append_Elmt (
--             Node => Make_Defining_Identifier (Loc, New_Name),
--             To   => Map);
--        end if;
--
--        return Copy_Tree_Redefining_Entities (Spec, Map, Loc);
--     end Copy_Specification;

   ---------------------------
   -- Could_Be_Asynchronous --
   ---------------------------

   function Could_Be_Asynchronous (Spec : Node_Id) return Boolean is
      Current_Parameter : Node_Id;

   begin
      if Present (Parameter_Specifications (Spec)) then
         Current_Parameter := First (Parameter_Specifications (Spec));
         while Current_Parameter /= Empty loop
            if Out_Present (Current_Parameter) then
               return False;
            end if;

            Next (Current_Parameter);
         end loop;
      end if;

      return True;
   end Could_Be_Asynchronous;

   ---------------------------
   -- Declare_Create_NVList --
   ---------------------------

   procedure Declare_Create_NVList
     (Loc    : Source_Ptr;
      NVList : Entity_Id;
      Decls  : List_Id;
      Stmts  : List_Id)
   is
   begin
      Append_To (Decls,
        Make_Object_Declaration (Loc,
          Defining_Identifier => NVList,
          Aliased_Present     => False,
          Object_Definition   =>
              New_Occurrence_Of (RTE (RE_NVList_Ref), Loc)));

      Append_To (Stmts,
        Make_Procedure_Call_Statement (Loc,
          Name =>
            New_Occurrence_Of (RTE (RE_NVList_Create), Loc),
          Parameter_Associations => New_List (
            New_Occurrence_Of (NVList, Loc))));
   end Declare_Create_NVList;

   ---------------------------------------------
   -- Expand_All_Calls_Remote_Subprogram_Call --
   ---------------------------------------------

   procedure Expand_All_Calls_Remote_Subprogram_Call (N : in Node_Id) is
      Called_Subprogram : constant Entity_Id  := Entity (Name (N));
      RCI_Package       : constant Entity_Id  := Scope (Called_Subprogram);
      Loc               : constant Source_Ptr := Sloc (N);
      RCI_Locator       : Node_Id;
      RCI_Cache         : Entity_Id;
      Calling_Stubs     : Node_Id;
      E_Calling_Stubs   : Entity_Id;
      Subp_Id           : Node_Id;

   begin
      E_Calling_Stubs := RCI_Calling_Stubs_Table.Get (Called_Subprogram);

      if E_Calling_Stubs = Empty then
         RCI_Cache := RCI_Locator_Table.Get (RCI_Package);

         if RCI_Cache = Empty then
            RCI_Locator :=
              RCI_Package_Locator
                (Loc, Specification (Unit_Declaration_Node (RCI_Package)));
            Prepend_To (Current_Sem_Unit_Declarations, RCI_Locator);

            --  The RCI_Locator package is inserted at the top level in the
            --  current unit, and must appear in the proper scope, so that it
            --  is not prematurely removed by the GCC back-end.

            declare
               Scop : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);

            begin
               if Ekind (Scop) = E_Package_Body then
                  New_Scope (Spec_Entity (Scop));

               elsif Ekind (Scop) = E_Subprogram_Body then
                  New_Scope
                     (Corresponding_Spec (Unit_Declaration_Node (Scop)));

               else
                  New_Scope (Scop);
               end if;

               Analyze (RCI_Locator);
               Pop_Scope;
            end;

            RCI_Cache   := Defining_Unit_Name (RCI_Locator);

         else
            RCI_Locator := Parent (RCI_Cache);
         end if;

         Subp_Id := Make_String_Literal (Loc,
           Get_Subprogram_Identifier (Called_Subprogram));

         Calling_Stubs := Build_Subprogram_Calling_Stubs
           (Vis_Decl               => Parent (Parent (Called_Subprogram)),
            Subp_Id                => Subp_Id,
            Asynchronous           => Nkind (N) = N_Procedure_Call_Statement
                                        and then
                                      Is_Asynchronous (Called_Subprogram),
            Locator                => RCI_Cache,
            New_Name               => New_Internal_Name ('S'));
         Insert_After (RCI_Locator, Calling_Stubs);
         Analyze (Calling_Stubs);
         E_Calling_Stubs := Defining_Unit_Name (Specification (Calling_Stubs));
      end if;

      Rewrite (Name (N), New_Occurrence_Of (E_Calling_Stubs, Loc));
   end Expand_All_Calls_Remote_Subprogram_Call;

   ---------------------------------
   -- Expand_Calling_Stubs_Bodies --
   ---------------------------------

   procedure Expand_Calling_Stubs_Bodies (Unit_Node : in Node_Id) is
      Spec  : constant Node_Id := Specification (Unit_Node);
      Decls : constant List_Id := Visible_Declarations (Spec);

   begin
      New_Scope (Scope_Of_Spec (Spec));
      Add_Calling_Stubs_To_Declarations (Specification (Unit_Node),
                                         Decls);
      Pop_Scope;
   end Expand_Calling_Stubs_Bodies;

   -----------------------------------
   -- Expand_Receiving_Stubs_Bodies --
   -----------------------------------

   procedure Expand_Receiving_Stubs_Bodies (Unit_Node : in Node_Id) is
      Spec  : Node_Id;
      Decls : List_Id;
      Temp  : List_Id;

   begin
      if Nkind (Unit_Node) = N_Package_Declaration then
         Spec  := Specification (Unit_Node);
         Decls := Visible_Declarations (Spec);
         New_Scope (Scope_Of_Spec (Spec));
         Add_Receiving_Stubs_To_Declarations (Spec, Decls);

      else
         Spec  :=
           Package_Specification_Of_Scope (Corresponding_Spec (Unit_Node));
         Decls := Declarations (Unit_Node);
         New_Scope (Scope_Of_Spec (Unit_Node));
         Temp := New_List;
         Add_Receiving_Stubs_To_Declarations (Spec, Temp);
         Insert_List_Before (First (Decls), Temp);
      end if;

      Pop_Scope;
   end Expand_Receiving_Stubs_Bodies;

   ----------------------------
   -- Get_Pkg_Name_string_Id --
   ----------------------------

   function Get_Pkg_Name_String_Id (Decl_Node : Node_Id) return String_Id is
      Unit_Name_Id : constant Unit_Name_Type := Get_Unit_Name (Decl_Node);

   begin
      Get_Unit_Name_String (Unit_Name_Id);

      --  Remove seven last character (" (spec)" or " (body)").

      Name_Len := Name_Len - 7;
      pragma Assert (Name_Buffer (Name_Len + 1) = ' ');

      return Get_String_Id (Name_Buffer (1 .. Name_Len));
   end Get_Pkg_Name_String_Id;

   -------------------
   -- Get_String_Id --
   -------------------

   function Get_String_Id (Val : String) return String_Id is
   begin
      Start_String;
      Store_String_Chars (Val);
      return End_String;
   end Get_String_Id;

   -------------------------------
   -- Get_Subprogram_Identifier --
   -------------------------------

   function Get_Subprogram_Identifier
     (Def : Entity_Id)
      return String_Id
   is
      Result : String_Id :=
        Subprogram_Identifier_Table.Get (Def);

      Current_Declaration : Node_Id;
      Current_Subp        : Entity_Id;
      Current_Subp_Str    : String_Id;
   begin
      if Result = No_String then
         --  We are looking up this subprogram's identifier
         --  outside of the context of generating calling
         --  or receiving stubs; hence, we are processing
         --  a 'Access attribute_reference for an RCI subprogram,
         --  for the purpose of obtaining a RAS value.

         pragma Assert
           (Is_Remote_Call_Interface (Scope (Def))
              and then
              (Nkind (Parent (Def)) = N_Procedure_Specification
                 or else
               Nkind (Parent (Def)) = N_Function_Specification));

         Current_Declaration :=
           First (Visible_Declarations
             (Package_Specification_Of_Scope (Scope (Def))));

         while Current_Declaration /= Empty loop
            if Nkind (Current_Declaration) = N_Subprogram_Declaration
              and then Comes_From_Source (Current_Declaration)
            then
               Current_Subp := Defining_Unit_Name (Specification (
                 Current_Declaration));
               Assign_Subprogram_Identifier (
                 Current_Subp, Current_Subp_Str);
               if Current_Subp = Def then
                  Result := Current_Subp_Str;
               end if;
            end if;
            Next (Current_Declaration);
         end loop;
      end if;
      pragma Assert (Result /= No_String);
      return Result;
   end Get_Subprogram_Identifier;

   ----------
   -- Hash --
   ----------

   function Hash (F : Entity_Id) return Hash_Index is
   begin
      return Hash_Index (Natural (F) mod Positive (Hash_Index'Last + 1));
   end Hash;

   function Hash (F : Name_Id) return Hash_Index is
   begin
      return Hash_Index (Natural (F) mod Positive (Hash_Index'Last + 1));
   end Hash;

   --------------------------
   -- Input_With_Tag_Check --
   --------------------------

   function Input_With_Tag_Check
     (Loc      : Source_Ptr;
      Var_Type : Entity_Id;
      Stream   : Entity_Id)
      return     Node_Id
   is
   begin
      return
        Make_Subprogram_Body (Loc,
          Specification              => Make_Function_Specification (Loc,
            Defining_Unit_Name =>
              Make_Defining_Identifier (Loc, New_Internal_Name ('S')),
            Subtype_Mark       => New_Occurrence_Of (Var_Type, Loc)),
          Declarations               => No_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc, New_List (
              Make_Tag_Check (Loc,
                Make_Return_Statement (Loc,
                  Make_Attribute_Reference (Loc,
                    Prefix         => New_Occurrence_Of (Var_Type, Loc),
                    Attribute_Name => Name_Input,
                    Expressions    =>
                      New_List (New_Occurrence_Of (Stream, Loc))))))));
   end Input_With_Tag_Check;

   --------------------------------
   -- Is_RACW_Controlling_Formal --
   --------------------------------

   function Is_RACW_Controlling_Formal
     (Parameter : Node_Id;
      Stub_Type : Entity_Id)
      return      Boolean
   is
      Typ : Entity_Id;

   begin
      --  If the kind of the parameter is E_Void, then it is not a
      --  controlling formal (this can happen in the context of RAS).

      if Ekind (Defining_Identifier (Parameter)) = E_Void then
         return False;
      end if;

      --  If the parameter is not a controlling formal, then it cannot
      --  be possibly a RACW_Controlling_Formal.

      if not Is_Controlling_Formal (Defining_Identifier (Parameter)) then
         return False;
      end if;

      Typ := Parameter_Type (Parameter);
      return (Nkind (Typ) = N_Access_Definition
               and then Etype (Subtype_Mark (Typ)) = Stub_Type)
        or else Etype (Typ) = Stub_Type;
   end Is_RACW_Controlling_Formal;

   --------------------
   -- Make_Tag_Check --
   --------------------

   function Make_Tag_Check (Loc : Source_Ptr; N : Node_Id) return Node_Id is
      Occ : constant Entity_Id :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('E'));

   begin
      return Make_Block_Statement (Loc,
        Handled_Statement_Sequence =>
          Make_Handled_Sequence_Of_Statements (Loc,
            Statements         => New_List (N),

            Exception_Handlers => New_List (
              Make_Exception_Handler (Loc,
                Choice_Parameter => Occ,

                Exception_Choices =>
                  New_List (New_Occurrence_Of (RTE (RE_Tag_Error), Loc)),

                Statements =>
                  New_List (Make_Procedure_Call_Statement (Loc,
                    New_Occurrence_Of
                      (RTE (RE_Raise_Program_Error_Unknown_Tag), Loc),
                    New_List (New_Occurrence_Of (Occ, Loc))))))));
   end Make_Tag_Check;

   ----------------------------
   -- Need_Extra_Constrained --
   ----------------------------

   function Need_Extra_Constrained (Parameter : Node_Id) return Boolean is
      Etyp : constant Entity_Id := Etype (Parameter_Type (Parameter));

   begin
      return Out_Present (Parameter)
        and then Has_Discriminants (Etyp)
        and then not Is_Constrained (Etyp)
        and then not Is_Indefinite_Subtype (Etyp);
   end Need_Extra_Constrained;

   ------------------------------------
   -- Pack_Entity_Into_Stream_Access --
   ------------------------------------

   function Pack_Entity_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Entity_Id;
      Etyp   : Entity_Id := Empty)
      return   Node_Id
   is
      Typ : Entity_Id;

   begin
      if Etyp /= Empty then
         Typ := Etyp;
      else
         Typ := Etype (Object);
      end if;

      return
        Pack_Node_Into_Stream_Access (Loc,
          Stream => Stream,
          Object => New_Occurrence_Of (Object, Loc),
          Etyp   => Typ);
   end Pack_Entity_Into_Stream_Access;

   ---------------------------
   -- Pack_Node_Into_Stream --
   ---------------------------

   function Pack_Node_Into_Stream
     (Loc    : Source_Ptr;
      Stream : Entity_Id;
      Object : Node_Id;
      Etyp   : Entity_Id)
      return   Node_Id
   is
      Write_Attribute : Name_Id := Name_Write;

   begin
      if not Is_Constrained (Etyp) then
         Write_Attribute := Name_Output;
      end if;

      return
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Etyp, Loc),
          Attribute_Name => Write_Attribute,
          Expressions    => New_List (
            Make_Attribute_Reference (Loc,
              Prefix         => New_Occurrence_Of (Stream, Loc),
              Attribute_Name => Name_Access),
            Object));
   end Pack_Node_Into_Stream;

   ----------------------------------
   -- Pack_Node_Into_Stream_Access --
   ----------------------------------

   function Pack_Node_Into_Stream_Access
     (Loc    : Source_Ptr;
      Stream : Node_Id;
      Object : Node_Id;
      Etyp   : Entity_Id)
      return   Node_Id
   is
      Write_Attribute : Name_Id := Name_Write;

   begin
      if not Is_Constrained (Etyp) then
         Write_Attribute := Name_Output;
      end if;

      return
        Make_Attribute_Reference (Loc,
          Prefix         => New_Occurrence_Of (Etyp, Loc),
          Attribute_Name => Write_Attribute,
          Expressions    => New_List (
            Stream,
            Object));
   end Pack_Node_Into_Stream_Access;

   ----------------------------
   -- Parameter_Passing_Mode --
   ----------------------------

   function Parameter_Passing_Mode
     (Loc         : Source_Ptr;
      Parameter   : Entity_Id;
      Constrained : Boolean)
      return Node_Id
   is
      Lib_RE : RE_Id;
   begin
      if Out_Present (Parameter) then
         if In_Present (Parameter)
           or else not Constrained
         then
            --  Unconstrained formals must be translated
            --  to 'in' or 'inout', not 'out', because
            --  they need to be constrained by the actual.

            Lib_RE := RE_Mode_Inout;
         else
            Lib_RE := RE_Mode_Out;
         end if;
      else
         Lib_RE := RE_Mode_In;
      end if;
      return New_Occurrence_Of (RTE (Lib_RE), Loc);
   end Parameter_Passing_Mode;

   -------------------------------
   -- RACW_Type_Is_Asynchronous --
   -------------------------------

   procedure RACW_Type_Is_Asynchronous (RACW_Type : in Entity_Id) is
      N : constant Node_Id := Asynchronous_Flags_Table.Get (RACW_Type);
      pragma Assert (N /= Empty);

   begin
      Replace (Expression (N), New_Occurrence_Of (Standard_True, Sloc (N)));
   end RACW_Type_Is_Asynchronous;

   -------------------------
   -- RCI_Package_Locator --
   -------------------------

   function RCI_Package_Locator
     (Loc          : Source_Ptr;
      Package_Spec : Node_Id)
      return         Node_Id
   is
      Inst : constant Node_Id :=
               Make_Package_Instantiation (Loc,
                 Defining_Unit_Name   =>
                   Make_Defining_Identifier (Loc, New_Internal_Name ('R')),
                 Name                 =>
                   New_Occurrence_Of (RTE (RE_RCI_Locator), Loc),
                 Generic_Associations => New_List (
                   Make_Generic_Association (Loc,
                     Selector_Name                     =>
                       Make_Identifier (Loc, Name_RCI_Name),
                     Explicit_Generic_Actual_Parameter =>
                       Make_String_Literal (Loc,
                         Strval => Get_Pkg_Name_String_Id (Package_Spec)))));

   begin
      RCI_Locator_Table.Set (Defining_Unit_Name (Package_Spec),
        Defining_Unit_Name (Inst));
      return Inst;
   end RCI_Package_Locator;

   -----------------------------------------------
   -- Remote_Types_Tagged_Full_View_Encountered --
   -----------------------------------------------

   procedure Remote_Types_Tagged_Full_View_Encountered
     (Full_View : in Entity_Id)
   is
      Stub_Elements : constant Stub_Structure :=
                        Stubs_Table.Get (Full_View);

   begin
      if Stub_Elements /= Empty_Stub_Structure then
         Add_RACW_Primitive_Declarations_And_Bodies
           (Full_View,
            Declaration_Node (Stub_Elements.Object_RPC_Receiver),
            List_Containing (Declaration_Node (Full_View)));
      end if;
   end Remote_Types_Tagged_Full_View_Encountered;

   procedure Reserve_NamingContext_Methods is
      Str_Resolve : constant String := "resolve";
   begin
      Name_Buffer (1 .. Str_Resolve'Length) := Str_Resolve;
      Name_Len := Str_Resolve'Length;
      Overload_Counter_Table.Set (Name_Find, 1);
   end Reserve_NamingContext_Methods;

   -------------------
   -- Scope_Of_Spec --
   -------------------

   function Scope_Of_Spec (Spec : Node_Id) return Entity_Id is
      Unit_Name : Node_Id := Defining_Unit_Name (Spec);

   begin
      while Nkind (Unit_Name) /= N_Defining_Identifier loop
         Unit_Name := Defining_Identifier (Unit_Name);
      end loop;

      return Unit_Name;
   end Scope_Of_Spec;

   procedure Set_Renaming_TSS
     (Typ     : Entity_Id;
      Nam     : Entity_Id;
      TSS_Nam : Name_Id)
   is
      Loc : constant Source_Ptr := Sloc (Nam);
      Spec : constant Node_Id := Parent (Nam);

      TSS_Node : constant Node_Id
        := Make_Subprogram_Renaming_Declaration (Loc,
          Specification =>
            Copy_Specification (Loc,
               Spec     => Spec,
               New_Name => TSS_Nam),
          Name => New_Occurrence_Of (Nam, Loc));
      Snam : constant Entity_Id
        := Defining_Unit_Name (Specification (TSS_Node));
   begin
      if Nkind (Spec) = N_Function_Specification then
         Set_Ekind (Snam, E_Function);
         Set_Etype (Snam, Entity (Subtype_Mark (Spec)));
      else
         Set_Ekind (Snam, E_Procedure);
         Set_Etype (Snam, Standard_Void_Type);
      end if;
      Set_TSS (Typ, Snam);
   end Set_Renaming_TSS;

   function Underlying_RACW_Type
     (RAS_Typ : Entity_Id)
      return Entity_Id
   is
      Record_Type : Entity_Id;
   begin
      if Ekind (RAS_Typ) = E_Record_Type then
         Record_Type := RAS_Typ;
      else
         pragma Assert (Present (Equivalent_Type (RAS_Typ)));
         Record_Type := Equivalent_Type (RAS_Typ);
      end if;

      return
        Etype (Subtype_Indication (
          First (Component_Items (Component_List (
            Type_Definition (Declaration_Node (Record_Type)))))));
   end Underlying_RACW_Type;

end Exp_Dist;
