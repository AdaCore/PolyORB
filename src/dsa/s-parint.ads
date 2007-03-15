------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

--  This is the version of System.Partition_Interface for PolyORB.
--  It shares part of its spec with the GLADE version and the GNAT RTL version.

pragma Warnings (Off);
with PolyORB.Partition_Elaboration;
pragma Elaborate_All (PolyORB.Partition_Elaboration);
pragma Warnings (On);

with Ada.Exceptions;
with Ada.Streams;

with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.Opaque;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;
with PolyORB.Initialization;

with System.RPC;
with System.Unsigned_Types;

package System.Partition_Interface is

   pragma Elaborate_Body;

   type DSA_Implementation_Name is (No_DSA, GARLIC_DSA, PolyORB_DSA);
   DSA_Implementation : constant DSA_Implementation_Name := PolyORB_DSA;

   type Subprogram_Id is new Natural;
   --  This type is used exclusively by stubs

   subtype Unit_Name is String;
   --  Name of Ada units

   subtype Object_Ref is PolyORB.References.Ref;

   subtype Entity_Ptr is PolyORB.Smart_Pointers.Entity_Ptr;

   procedure Raise_Program_Error_Unknown_Tag
     (E : Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Raise_Program_Error_Unknown_Tag);
   --  Raise Program_Error with the same message as E one

   function Get_Active_Partition_ID (Name : Unit_Name) return RPC.Partition_ID;
   --  Get the Partition_ID of the partition where unit Name resides

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  Return the Partition_ID of the current partition

   procedure Set_Local_Partition_ID (PID : RPC.Partition_ID);
   --  Set the Partition_ID of the current partition

   ---------------------------------------
   -- Remote access-to-subprogram types --
   ---------------------------------------

   type RAS_Proxy_Type is tagged limited record
      All_Calls_Remote : Boolean;
      Target           : Entity_Ptr;
      Subp_Id          : Subprogram_Id := 0;
      --  This component is unused for PolyORB (it is used only for the GARLIC
      --  implementation of RAS.)
   end record;

   type RAS_Proxy_Type_Access is access RAS_Proxy_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating
   --  Exp_Dist.Build_Remote_Supbrogram_Proxy_Type.

   procedure Get_RAS_Info
     (Pkg_Name        :     String;
      Subprogram_Name :     String;
      Subp_Ref        : out Object_Ref);
   --  Return the RAS object reference associated with the named subprogram.

   -----------------------------
   --  Remote Call Interfaces --
   -----------------------------

   --  Receiving stubs contain a table of all subprograms exported by the unit.

   type RCI_Subp_Info is record
      Name        : System.Address;
      Name_Length : Integer;
      --  Subprogram distribution identifier

      Addr        : System.Address;
      --  Local address of the proxy object
   end record;

   type RCI_Subp_Info_Array is array (Integer range <>)
     of RCI_Subp_Info;

   subtype Request_Access is PolyORB.Requests.Request_Access;

   generic
      RCI_Name : String;
      Version  : String;
   package RCI_Locator is
      function Get_RCI_Package_Ref
        return Object_Ref;
   end RCI_Locator;
   --  Calling stubs need a cache of the object reference associated with each
   --  RCI unit.

   procedure Check
     (Name    : Unit_Name;
      Version : String;
      RCI     : Boolean := True);
   --  Use by the main subprogram to check that a remote receiver unit has has
   --  the same version than the caller's one.

   --------------------------
   -- RPC receiver objects --
   --------------------------

   --  One RPC receiver is created for each supported interface, i.e. one for
   --  each RCI library unit, and one for each type that is the designated type
   --  of one or more RACW type.

   function Caseless_String_Eq (S1, S2 : String) return Boolean;
   --  Case-less equality of S1 and S2

   type Request_Handler_Access is access
     procedure (R : Request_Access);

   type Private_Info is abstract tagged null record;
   type Private_Info_Access is access all Private_Info'Class;

   type Servant is new PolyORB.Servants.Servant with record
      Handler        : Request_Handler_Access;
      --  The dispatching routine.

      Object_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
      --  Null for RCI servants (the root POA will be used in this case)

      Obj_TypeCode   : PolyORB.Any.TypeCode.Object;
      --  The TypeCode to be used for references to objects of this type

      Impl_Info      : Private_Info_Access;
   end record;
   type Servant_Access is access all Servant'Class;

   type Receiving_Stub_Kind is (Obj_Stub, Pkg_Stub);

   procedure Register_Obj_Receiving_Stub
     (Name          : String;
      Handler       : Request_Handler_Access;
      Receiver      : Servant_Access);
   --  Register Receiver as the RPC servant for distributed objects of type
   --  Name, at elaboration time.

   procedure Register_Pkg_Receiving_Stub
     (Name                : String;
      Version             : String;
      Handler             : Request_Handler_Access;
      Receiver            : Servant_Access;
      Subp_Info           : System.Address;
      Subp_Info_Len       : Integer;
      Is_All_Calls_Remote : Boolean);
   --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.
   --  Subp_Info is the address of an array of a statically subtype
   --  of RCI_Subp_Info_Array with a range of 0 .. Subp_Info_Len - 1.

   procedure Register_Unit_On_Name_Server (Name : String;
                                           Kind : String;
                                           Obj  : PolyORB.References.Ref);
   --  Register one receiving stub on the name server

   function Retrieve_Receiving_Stub (Name : String;
                                     Kind : Receiving_Stub_Kind)
     return Servant_Access;
   --  Return the servant for distributed objects with given Name and Kind, or
   --  null if non-existant.

   ---------------------------------
   -- Remote Access to Class Wide --
   ---------------------------------

   type RACW_Stub_Type is tagged limited record
      Target       : Entity_Ptr;
      --  Target cannot be a References.Ref (a controlled type) because that
      --  would pollute RACW_Stub_Type's dispatch table (which must be exactly
      --  identical to that of the designated tagged type).  Target must be a
      --  pointer to References.Reference_Info.

      Asynchronous : Boolean;
   end record;

   type RACW_Stub_Type_Access is access all RACW_Stub_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating exp_dist.adb

   function Same_Partition
     (Left  : access RACW_Stub_Type;
      Right : access RACW_Stub_Type) return Boolean;
   --  Determine whether Left and Right correspond to objects instantiated
   --  on the same partition, for enforcement of E.4(19).

   --------------------------------------------
   -- Support for RACWs as object references --
   --------------------------------------------

   procedure Inc_Usage (E : PolyORB.Smart_Pointers.Entity_Ptr)
     renames PolyORB.Smart_Pointers.Inc_Usage;
   --  In stubs for remote objects, the object reference information is stored
   --  as a naked Entity_Ptr. We therefore need to account for this reference
   --  by hand.

   procedure Set_Ref
     (The_Ref    : in out PolyORB.References.Ref;
      The_Entity :        PolyORB.Smart_Pointers.Entity_Ptr)
     renames PolyORB.References.Set;
   function Make_Ref (The_Entity : PolyORB.Smart_Pointers.Entity_Ptr)
     return PolyORB.References.Ref;
   function Entity_Of
     (R : PolyORB.References.Ref)
      return PolyORB.Smart_Pointers.Entity_Ptr
     renames PolyORB.References.Entity_Of;
   --  Conversion from Entity_Ptr to Ref and reverse

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access);
   --  Get a unique pointer on a remote object. On entry, Handler is expected
   --  to be a pointer to a local variable of any stub type compatible with
   --  RACW_Stub_Type; on exit, it is a pointer to a variable allocated on the
   --  heap (either a newly allocated instance, or a previous existing instance
   --  for the same remote object). Note that newly-allocated stubs are always
   --  of type RACW_Stub_Type, so a tag fixup is required afterwards.

   function To_PolyORB_String (S : String) return PolyORB.Types.Identifier
     renames PolyORB.Types.To_PolyORB_String;

   function Is_Nil (R : PolyORB.References.Ref) return Boolean
     renames PolyORB.References.Is_Nil;

   procedure Get_Local_Address
     (Ref      : PolyORB.References.Ref;
      Is_Local : out Boolean;
      Addr     : out System.Address);
   --  If Ref denotes a local object, Is_Local is set to True, and Addr is set
   --  to the object's actual address, else Is_Local is set to False and Addr
   --  is set to Null_Address.

   procedure Get_Reference
     (Addr     :        System.Address;
      Typ      :        String;
      Receiver : access Servant;
      Ref      :    out PolyORB.References.Ref);
   --  Create a reference that can be used to desginate the object whose
   --  address is Addr, whose type is the designated type of a RACW type
   --  associated with Servant.

   ------------------------------
   -- Any and associated types --
   ------------------------------

   subtype Identifier is PolyORB.Types.Identifier;
   Result_Name : constant Identifier
     := PolyORB.Types.To_PolyORB_String ("Result");

   subtype Any is PolyORB.Any.Any;
   Mode_In    : PolyORB.Any.Flags renames PolyORB.Any.ARG_IN;
   Mode_Out   : PolyORB.Any.Flags renames PolyORB.Any.ARG_OUT;
   Mode_Inout : PolyORB.Any.Flags renames PolyORB.Any.ARG_INOUT;
   subtype NamedValue is PolyORB.Any.NamedValue;
   subtype TypeCode is PolyORB.Any.TypeCode.Object;
   procedure Set_TC
     (A : in out Any;
      T : PolyORB.Any.TypeCode.Object)
      renames PolyORB.Any.Set_Type;

   function Get_TC (A : Any) return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.Get_Unwound_Type;

   function Create_Any
     (Tc : PolyORB.Any.TypeCode.Object)
      return Any
     renames PolyORB.Any.Get_Empty_Any_Aggregate;

   function Content_Type
     (Self : PolyORB.Any.TypeCode.Object)
      return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.Content_Type;

   function Any_Member_Type
     (A     : Any;
      Index : System.Unsigned_Types.Long_Unsigned)
      return PolyORB.Any.TypeCode.Object;
   --  Return type of the Index'th component in Tk_Struct or Tk_Union Any A

   subtype NVList_Ref is PolyORB.Any.NVList.Ref;
   procedure NVList_Create (NVList : out PolyORB.Any.NVList.Ref)
     renames PolyORB.Any.NVList.Create;
   procedure NVList_Add_Item
     (Self       :    PolyORB.Any.NVList.Ref;
      Item_Name  : PolyORB.Types.Identifier;
      Item       : Any;
      Item_Flags : PolyORB.Any.Flags)
     renames PolyORB.Any.NVList.Add_Item;

   -----------------------------------------------
   -- Elementary From_Any and To_Any operations --
   -----------------------------------------------

   subtype Unsigned is System.Unsigned_Types.Unsigned;
   subtype Long_Unsigned is
     System.Unsigned_Types.Long_Unsigned;
   subtype Long_Long_Unsigned is
     System.Unsigned_Types.Long_Long_Unsigned;
   subtype Short_Unsigned is
     System.Unsigned_Types.Short_Unsigned;
   subtype Short_Short_Unsigned is
     System.Unsigned_Types.Short_Short_Unsigned;

--       function FA_AD (Item : Any) return X;
--       function FA_AS (Item : Any) return X;

   function FA_B (Item : Any) return Boolean;
   function FA_C (Item : Any) return Character;
   function FA_F (Item : Any) return Float;
   function FA_I (Item : Any) return Integer;
   function FA_U (Item : Any) return Unsigned;

   function FA_LF (Item : Any) return Long_Float;
   function FA_LI (Item : Any) return Long_Integer;
   function FA_LU (Item : Any) return Long_Unsigned;

   function FA_LLF (Item : Any) return Long_Long_Float;
   function FA_LLI (Item : Any) return Long_Long_Integer;
   function FA_LLU (Item : Any) return Long_Long_Unsigned;

   function FA_SF (Item : Any) return Short_Float;
   function FA_SI (Item : Any) return Short_Integer;
   function FA_SU (Item : Any) return Short_Unsigned;

   function FA_SSI (Item : Any) return Short_Short_Integer;
   function FA_SSU (Item : Any) return Short_Short_Unsigned;
   function FA_WC (Item : Any) return Wide_Character;

   function FA_String (Item : Any) return String;

   function FA_ObjRef
     (Item : Any)
      return PolyORB.References.Ref
     renames PolyORB.Any.ObjRef.From_Any;

--     function TA_AD (X) return Any;
--     function TA_AS (X) return Any;

   function TA_A (A : Any) return Any
     renames PolyORB.Any.To_Any;

   function TA_B (Item : Boolean) return Any;
   function TA_C (Item : Character) return Any;
   function TA_F (Item : Float) return Any;
   function TA_I (Item : Integer) return Any;
   function TA_U (Item : Unsigned) return Any;
   function TA_LF (Item : Long_Float) return Any;
   function TA_LI (Item : Long_Integer) return Any;
   function TA_LU (Item : Long_Unsigned) return Any;
   function TA_LLF (Item : Long_Long_Float) return Any;
   function TA_LLI (Item : Long_Long_Integer) return Any;
   function TA_LLU (Item : Long_Long_Unsigned) return Any;
   function TA_SF (Item : Short_Float) return Any;
   function TA_SI (Item : Short_Integer) return Any;
   function TA_SU (Item : Short_Unsigned) return Any;
   function TA_SSI (Item : Short_Short_Integer) return Any;
   function TA_SSU (Item : Short_Short_Unsigned) return Any;
   function TA_WC (Item : Wide_Character) return Any;

   function TA_String (S : String) return Any;

   function TA_ObjRef (R : PolyORB.References.Ref)
     return Any
     renames PolyORB.Any.ObjRef.To_Any;

   function TA_TC (TC : PolyORB.Any.TypeCode.Object) return Any
     renames PolyORB.Any.To_Any;

   --       function TC_AD return PolyORB.Any.TypeCode.Object
   --       renames PolyORB.Any.TC_X;
   --       function TC_AS return PolyORB.Any.TypeCode.Object
   --       renames PolyORB.Any.TC_X;

   --  The typecodes below define the mapping of Ada elementary
   --  types onto PolyORB types.

   function TC_B return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Boolean;
   function TC_C return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Char;
   function TC_F return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Float;

   --  Warning! Ada numeric types have platform dependant sizes, PolyORB types
   --  are fixed size: this mapping may need to be changed for other platforms
   --  (or the biggest PolyORB type for each Ada type should be selected, if
   --  cross-platform interoperability is desired.

   function TC_Any return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Any;
   function TC_I return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Long;
   function TC_LF return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Double;
   function TC_LI return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Long;
   function TC_LLF return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Long_Double;
   function TC_LLI return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Long_Long;
   function TC_LLU return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Unsigned_Long_Long;
   function TC_LU return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Unsigned_Long;
   function TC_SF return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Float;

   function TC_SI return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Short;
   function TC_SSI return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Short;
   function TC_SSU return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Octet;
   function TC_SU return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Unsigned_Short;
   function TC_U return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Unsigned_Long;
   function TC_WC return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TC_Wchar;

   function TC_String return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_String;
   function TC_Void return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Void;
   function TC_Opaque return PolyORB.Any.TypeCode.Object;

   function TC_Alias return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Alias;
   --  Empty Tk_Alias typecode
   function TC_Array return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Array;
   --  Empty Tk_Array typecode
   function TC_Sequence return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Sequence;
   --  Empty Tk_Sequence typecode
   function TC_Struct return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Struct;
   --  Empty Tk_Struct typecode
   function TC_Object return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Object;
   --  Empty Tk_ObjRef typecode
   function TC_Union return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Union;
   --  Empty Tk_Union typecode

   subtype Any_Array is PolyORB.Any.TypeCode.Any_Array;

   function TC_Build
     (Base       : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.Build_Complex_TC;

   procedure Move_Any_Value (Dest, Src : Any)
     renames PolyORB.Any.Move_Any_Value;

   function Any_Aggregate_Build
     (TypeCode : PolyORB.Any.TypeCode.Object;
      Contents : Any_Array)
      return Any;

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : Any)
     renames PolyORB.Any.Add_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any;
      Tc    : PolyORB.Any.TypeCode.Object;
      Index : System.Unsigned_Types.Long_Unsigned)
      return Any;

   function Get_Any_Type (A : Any) return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.Get_Type;

   function Get_Nested_Sequence_Length
     (Value : Any;
      Depth : Positive)
     return Unsigned;
   --  Return the length of the sequence at nesting level Depth within Value,
   --  a Tk_Struct any representing an unconstrained array.

   function Extract_Union_Value (U : Any) return Any;
   --  Given an Any of a union type, return an Any for the value of the union

   -----------------------------------------------------------------------
   -- Support for opaque data transfer using stream-oriented attributes --
   -----------------------------------------------------------------------

   type Buffer_Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   --  A stream based on a PolyORB buffer

   procedure Read
     (Stream : in out Buffer_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Write
     (Stream : in out Buffer_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Any_To_BS (Item : Any; Stream : out Buffer_Stream_Type);
   procedure BS_To_Any (Stream : Buffer_Stream_Type; Item : out Any);

   procedure Allocate_Buffer (Stream : in out Buffer_Stream_Type);
   procedure Release_Buffer (Stream : in out Buffer_Stream_Type);

   --------------
   -- Requests --
   --------------

   Nil_Exc_List : PolyORB.Any.ExceptionList.Ref
      renames PolyORB.Any.ExceptionList.Nil_Ref;

   procedure Request_Create
     (Target    :        PolyORB.References.Ref;
      Operation :        String;
      Arg_List  :        PolyORB.Any.NVList.Ref;
      Result    : in out PolyORB.Any.NamedValue;
      Exc_List  :        PolyORB.Any.ExceptionList.Ref
        := PolyORB.Any.ExceptionList.Nil_Ref;
      Req       :    out PolyORB.Requests.Request_Access;
      Req_Flags :        PolyORB.Requests.Flags;
      Deferred_Arguments_Session :
        PolyORB.Components.Component_Access := null;
      Identification : PolyORB.Requests.Arguments_Identification
        := PolyORB.Requests.Ident_By_Position;
      Dependent_Binding_Object : PolyORB.Smart_Pointers.Entity_Ptr
        := null
     ) renames PolyORB.Requests.Create_Request;

   procedure Request_Invoke
     (R            : PolyORB.Requests.Request_Access;
      Invoke_Flags : PolyORB.Requests.Flags          := 0);

   procedure Request_Arguments
     (R     :        PolyORB.Requests.Request_Access;
      Args  : in out PolyORB.Any.NVList.Ref);

   procedure Request_Set_Out
     (R     : PolyORB.Requests.Request_Access);

   procedure Set_Result
     (Self : PolyORB.Requests.Request_Access;
      Val  : Any)
     renames PolyORB.Requests.Set_Result;

   Asynchronous_P_To_Sync_Scope : constant array (Boolean)
     of PolyORB.Requests.Flags
     := (False => PolyORB.Requests.Sync_With_Target,
         True  => PolyORB.Requests.Sync_With_Transport);
   --  Request_Flags to use for a request according to whether or not the call
   --  is asynchronous.

   procedure Request_Raise_Occurrence (R : in out Request_Access);
   --  If R terminated with an exception, raise that exception. In that case,
   --  the request is destroyed before raising the exception, and this
   --  subprogram does not return. If no exception occurred, do nothing.

   procedure Register_Termination_Manager
     (Ref      : PolyORB.References.Ref;
      Oid      : PolyORB.Objects.Object_Id_Access;
      Address  : System.Address;
      Shutdown : PolyORB.Initialization.Finalizer);
   --  Register the termination manager of the local partition

private

   pragma Inline
     (FA_B, FA_C, FA_F, FA_I, FA_U, FA_LF, FA_LI, FA_LU,
      FA_LLF, FA_LLI, FA_LLU, FA_SF, FA_SI, FA_SU,
      FA_SSI, FA_SSU, FA_WC, FA_String,

      TA_B, TA_C, TA_F, TA_I, TA_U, TA_LF, TA_LI, TA_LU,
      TA_LLF, TA_LLI, TA_LLU, TA_SF, TA_SI, TA_SU,
      TA_SSI, TA_SSU, TA_WC, TA_String);

   pragma Inline (Caseless_String_Eq, Get_Aggregate_Element);

   function Execute_Servant
     (Self : not null access Servant;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;
   pragma Inline (Execute_Servant);

   --  During elaboration, each RCI package and each distributed object type
   --  registers a Receiving_Stub entry.

   type Receiving_Stub is new Private_Info with record
      Kind                : Receiving_Stub_Kind;
      --  Indicates whetger this info is relative to RACW type or a RCI

      Name                : PolyORB.Utils.Strings.String_Ptr;
      --  Fully qualified name of the RACW or RCI

      Version             : PolyORB.Utils.Strings.String_Ptr;
      --  For RCIs only: library unit version

      Receiver            : Servant_Access;
      --  The RPC receiver (servant) object

      Is_All_Calls_Remote : Boolean;
      --  For RCIs only: true iff a pragma All_Calls_Remote applies to unit

      Subp_Info           : System.Address;
      Subp_Info_Len       : Integer;
      --  For RCIs only: mapping of RCI subprogram names to addresses.
      --  For the definition of these values, cf. the specification of
      --  Register_Pkg_Receiving_Stubs.

   end record;

   package Receiving_Stub_Lists is new PolyORB.Utils.Chained_Lists
     (Receiving_Stub);

   All_Receiving_Stubs : Receiving_Stub_Lists.List;

   type Buffer_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Buf : PolyORB.Buffers.Buffer_Access;
      Arr : PolyORB.Opaque.Zone_Access;
   end record;

   procedure Finalize (X : in out Buffer_Stream_Type);

end System.Partition_Interface;
