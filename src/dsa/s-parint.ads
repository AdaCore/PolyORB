------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           S Y S T E M . P A R T I T I O N _ I N T E R F A C E            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

--  This is the version of System.Partition_Interface for PolyORB.
--  It shares part of its spec with the GLADE version and the GNAT RTL version.

pragma Warnings (Off);
with PolyORB.Partition_Elaboration;
pragma Elaborate_All (PolyORB.Partition_Elaboration);
pragma Warnings (On);

with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Tags;

with Interfaces;

with System.DSA_Types;
with System.RPC;

with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Buffers;
with PolyORB.Components;
with PolyORB.Initialization;
with PolyORB.Objects;
with PolyORB.Obj_Adapters;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Strings;

package System.Partition_Interface is

   pragma Elaborate_Body;

   type DSA_Implementation_Name is (No_DSA, GARLIC_DSA, PolyORB_DSA);
   DSA_Implementation : constant DSA_Implementation_Name := PolyORB_DSA;
   --  Identification of this DSA implementation variant

   PCS_Version : constant := 5;
   --  Version of the PCS API (for Exp_Dist consistency check).
   --  This version number is matched against corresponding element of
   --  Exp_Dist.PCS_Version_Number to ensure that the versions of Exp_Dist and
   --  the PCS are consistent.

   package DSAT renames System.DSA_Types;

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

   function Get_Local_Partition_Name return String;
   --  Return the name of the current partition

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  Return the Partition_ID of the current partition

   procedure Set_Local_Partition_ID (PID : RPC.Partition_ID);
   --  Set the Partition_ID of the current partition

   function Local_PID_Allocated return Boolean;
   pragma Inline (Local_PID_Allocated);
   --  True once the local partition ID is known

   ---------------------------------
   -- Interoperable numeric types --
   ---------------------------------

   subtype I8  is Interfaces.Integer_8;
   subtype I16 is Interfaces.Integer_16;
   subtype I32 is Interfaces.Integer_32;
   subtype I64 is Interfaces.Integer_64;

   subtype U8  is Interfaces.Unsigned_8;
   subtype U16 is Interfaces.Unsigned_16;
   subtype U32 is Interfaces.Unsigned_32;
   subtype U64 is Interfaces.Unsigned_64;

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
   --  Exp_Dist.Build_Remote_Subprogram_Proxy_Type.

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

   type RCI_Subp_Info_Array is array (Integer range <>) of RCI_Subp_Info;

   subtype Request is PolyORB.Requests.Request;
   subtype Request_Access is PolyORB.Requests.Request_Access;

   generic
      RCI_Name : String;
      Version  : String;
      pragma Unreferenced (Version);
      --  Not used anymore, kept to avoid compiler API change

   package RCI_Locator is
      function Get_RCI_Package_Ref return Object_Ref;
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

   type Request_Handler_Access is access procedure (R : Request_Access);

   type Private_Info is private;

   type Servant is new PolyORB.Servants.Servant with record
      Handler        : Request_Handler_Access;
      --  The dispatching routine.

      Object_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
      --  Null for RCI servants (the root POA will be used in this case)

      Obj_TypeCode   : PolyORB.Any.TypeCode.Local_Ref;
      --  The TypeCode to be used for references to objects of this type

      Impl_Info      : aliased Private_Info;
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
   --  Subp_Info is the address of an array of a statically constrained subtype
   --  of RCI_Subp_Info_Array with a range of 0 .. Subp_Info_Len - 1.

   function Find_Receiving_Stub
     (Name : String; Kind : Receiving_Stub_Kind) return Servant_Access;
   --  Return the servant for distributed objects with given Name and Kind, or
   --  null if non-existant.

   procedure Activate_RPC_Receivers;
   --  Start processing incoming remote calls

   ---------------------------------
   -- Remote Access to Class Wide --
   ---------------------------------

   type RACW_Stub_Type is tagged limited record
      Target       : Entity_Ptr;
      --  Target cannot be a References.Ref (a controlled type) because that
      --  would pollute RACW_Stub_Type's dispatch table (which must be exactly
      --  identical to that of the designated tagged type). Target must be a
      --  pointer to References.Reference_Info.

      Asynchronous : Boolean;
   end record;

   type RACW_Stub_Type_Access is access all RACW_Stub_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating Exp_Dist
   --  accordingly.

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
   function Make_Ref
     (The_Entity : PolyORB.Smart_Pointers.Entity_Ptr)
      return PolyORB.References.Ref;
   function Entity_Of
     (R : PolyORB.References.Ref) return PolyORB.Smart_Pointers.Entity_Ptr
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

   function Get_Reference
     (RACW      : System.Address;
      Type_Name : String;
      Stub_Tag  : Ada.Tags.Tag;
      Is_RAS    : Boolean;
      Receiver  : access Servant) return PolyORB.References.Ref;
   --  Create a reference from an RACW value with designated type Type_Name.
   --  Stub_Tag is the tag of the associated stub type. Is_RAS is True if the
   --  RACW is implementing a remote access-to-subprogram type.
   --  Receiver is the associated servant.

   procedure Build_Local_Reference
     (Addr     : System.Address;
      Typ      : String;
      Receiver : access Servant;
      Ref      : out PolyORB.References.Ref);
   --  Create a reference that can be used to designate the local object whose
   --  address is Addr, whose type is the designated type of a RACW type
   --  associated with Servant.

   function Get_RACW
     (Ref          : PolyORB.References.Ref;
      Stub_Tag     : Ada.Tags.Tag;
      Is_RAS       : Boolean;
      Asynchronous : Boolean) return System.Address;
   --  From an object reference, create a remote access-to-classwide value
   --  designating the same object. Is_RAS indicates whether the RACW is
   --  implementing a remote access-to-subprogram type.
   --  The returned address is either the address of a local object, or the
   --  address of a stub object having the given tag if the designated object
   --  is remote. For a nil ref, a null address is returned. If All_Calls_
   --  Remote is True, the address of a stub object is returned even if the
   --  reference is local.

   ------------------------------
   -- Any and associated types --
   ------------------------------

   package PATC renames PolyORB.Any.TypeCode;

   subtype Identifier is PolyORB.Types.Identifier;
   Result_Name : constant Identifier :=
                   PolyORB.Types.To_PolyORB_String ("Result");

   subtype Any is PolyORB.Any.Any;
   Mode_In    : PolyORB.Any.Flags renames PolyORB.Any.ARG_IN;
   Mode_Out   : PolyORB.Any.Flags renames PolyORB.Any.ARG_OUT;
   Mode_Inout : PolyORB.Any.Flags renames PolyORB.Any.ARG_INOUT;
   subtype NamedValue is PolyORB.Any.NamedValue;
   subtype TypeCode is PolyORB.Any.TypeCode.Local_Ref;
   procedure Set_TC (A : in out Any; T : PATC.Local_Ref)
     renames PolyORB.Any.Set_Type;

   function Get_TC (A : Any) return PATC.Local_Ref;

   function Create_Any (TC : PATC.Local_Ref) return Any;
   --  Same as PolyORB.Any.Get_Empty_Any_Aggregate, except for the case where
   --  TC is a sequence<octet> typecode, in which case an Any using the
   --  specific shadow content type for such sequences is returned instead of
   --  a Default_Aggregate_Content (Any_To_BS relies on this property).

   function Content_Type
     (Self : PATC.Local_Ref) return PATC.Local_Ref
     renames PATC.Content_Type;

   function Any_Member_Type
     (A     : Any;
      Index : U32) return PATC.Local_Ref;
   --  Return type of the Index'th component in Tk_Struct or Tk_Union Any A

   subtype NVList_Ref is PolyORB.Any.NVList.Ref;
   procedure NVList_Create (NVList : out PolyORB.Any.NVList.Ref)
     renames PolyORB.Any.NVList.Create;
   procedure NVList_Add_Item
     (Self       : PolyORB.Any.NVList.Ref;
      Item_Name  : PolyORB.Types.Identifier;
      Item       : Any;
      Item_Flags : PolyORB.Any.Flags)
     renames PolyORB.Any.NVList.Add_Item;

   -----------------------------------------------
   -- Elementary From_Any and To_Any operations --
   -----------------------------------------------

   function FA_A (Item : Any) return DSAT.Any_Container_Ptr;
   function FA_B (Item : Any) return Boolean;
   function FA_C (Item : Any) return Character;
   function FA_F (Item : Any) return Float;

   function FA_I8  (Item : Any) return I8;
   function FA_I16 (Item : Any) return I16;
   function FA_I32 (Item : Any) return I32;
   function FA_I64 (Item : Any) return I64;

   function FA_U8  (Item : Any) return U8;
   function FA_U16 (Item : Any) return U16;
   function FA_U32 (Item : Any) return U32;
   function FA_U64 (Item : Any) return U64;

   function FA_SF  (Item : Any) return Short_Float;
   function FA_LF  (Item : Any) return Long_Float;
   function FA_LLF (Item : Any) return Long_Long_Float;

   function FA_WC (Item : Any) return Wide_Character;

   function FA_String
     (Item : Any) return Ada.Strings.Unbounded.Unbounded_String;
   function FA_ObjRef (Item : Any) return PolyORB.References.Ref
     renames PolyORB.Any.ObjRef.From_Any;

   function TA_A (Item : DSAT.Any_Container_Ptr) return Any;
   function TA_B (Item : Boolean) return Any;
   function TA_C (Item : Character) return Any;
   function TA_F (Item : Float) return Any;

   function TA_I8  (Item : I8)  return Any;
   function TA_I16 (Item : I16) return Any;
   function TA_I32 (Item : I32) return Any;
   function TA_I64 (Item : I64) return Any;

   function TA_U8  (Item : U8)  return Any;
   function TA_U16 (Item : U16) return Any;
   function TA_U32 (Item : U32) return Any;
   function TA_U64 (Item : U64) return Any;

   function TA_SF (Item : Short_Float) return Any;
   function TA_LF (Item : Long_Float) return Any;
   function TA_LLF (Item : Long_Long_Float) return Any;

   function TA_WC (Item : Wide_Character) return Any;
   function TA_String (S : Ada.Strings.Unbounded.Unbounded_String) return Any;
   function TA_ObjRef (R : PolyORB.References.Ref) return Any
     renames PolyORB.Any.ObjRef.To_Any;

   function TA_Std_String (S : String) return Any;
   function TA_TC (TC : PATC.Local_Ref) return Any
     renames PolyORB.Any.To_Any;

   --  The typecodes below define the mapping of Ada elementary types to
   --  PolyORB types.

   function TC_A return PATC.Local_Ref
     renames PolyORB.Any.TC_Any;
   function TC_B return PATC.Local_Ref
     renames PolyORB.Any.TC_Boolean;
   function TC_C return PATC.Local_Ref
     renames PolyORB.Any.TC_Char;
   function TC_F return PATC.Local_Ref
     renames PolyORB.Any.TC_Float;

   --  Note: no signed 8 bit integer type in the PolyORB data model

   function TC_I8 return PATC.Local_Ref
     renames PolyORB.Any.TC_Short;
   function TC_I16 return PATC.Local_Ref
     renames PolyORB.Any.TC_Short;
   function TC_I32 return PATC.Local_Ref
     renames PolyORB.Any.TC_Long;
   function TC_I64 return PATC.Local_Ref
     renames PolyORB.Any.TC_Long_Long;

   function TC_U8 return PATC.Local_Ref
     renames PolyORB.Any.TC_Octet;
   function TC_U16 return PATC.Local_Ref
     renames PolyORB.Any.TC_Unsigned_Short;
   function TC_U32 return PATC.Local_Ref
     renames PolyORB.Any.TC_Unsigned_Long;
   function TC_U64 return PATC.Local_Ref
     renames PolyORB.Any.TC_Unsigned_Long_Long;

   function TC_SF return PATC.Local_Ref
     renames PolyORB.Any.TC_Float;
   function TC_LF return PATC.Local_Ref
     renames PolyORB.Any.TC_Double;
   function TC_LLF return PATC.Local_Ref
     renames PolyORB.Any.TC_Long_Double;

   function TC_WC return PATC.Local_Ref
     renames PolyORB.Any.TC_Wchar;

   function TC_String return PATC.Local_Ref
     renames PATC.TC_String;
   function TC_Void return PATC.Local_Ref
     renames PATC.TC_Void;
   function TC_Opaque return PATC.Local_Ref;

   function TC_Alias return PATC.Local_Ref
     renames PATC.TC_Alias;
   --  Empty Tk_Alias typecode
   function TC_Array return PATC.Local_Ref
     renames PATC.TC_Array;
   --  Empty Tk_Array typecode
   function TC_Sequence return PATC.Local_Ref
     renames PATC.TC_Sequence;
   --  Empty Tk_Sequence typecode
   function TC_Struct return PATC.Local_Ref
     renames PATC.TC_Struct;
   --  Empty Tk_Struct typecode
   function TC_Object return PATC.Local_Ref
     renames PATC.TC_Object;
   --  Empty Tk_ObjRef typecode
   function TC_Union return PATC.Local_Ref
     renames PATC.TC_Union;
   --  Empty Tk_Union typecode

   subtype Any_Array is PATC.Any_Array;

   function TC_Build
     (Base       : PATC.Local_Ref;
      Parameters : Any_Array) return PATC.Local_Ref;

   procedure Move_Any_Value (Dest, Src : Any)
     renames PolyORB.Any.Move_Any_Value;

   function Any_Aggregate_Build
     (TypeCode : PATC.Local_Ref;
      Contents : Any_Array) return Any;

   procedure Add_Aggregate_Element
     (Value   : in out Any;
      Element : Any)
     renames PolyORB.Any.Add_Aggregate_Element;

   function Get_Aggregate_Element
     (Value : Any;
      TC    : PATC.Local_Ref;
      Index : U32) return Any;

   function Get_Any_Type (A : Any) return PATC.Local_Ref
     renames PolyORB.Any.Get_Type;

   function Get_Nested_Sequence_Length
     (Value : Any;
      Depth : Positive) return U32;
   --  Return the length of the sequence at nesting level Depth within Value,
   --  a Tk_Struct any representing an unconstrained array.

   function Extract_Union_Value (U : Any) return Any;
   --  Given an Any of a union type, return an Any for the value of the union

   -----------------------------------------------------------------------
   -- Support for opaque data transfer using stream-oriented attributes --
   -----------------------------------------------------------------------

   type Buffer_Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   --  A stream based on a PolyORB buffer

   overriding procedure Read
     (Stream : in out Buffer_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Buffer_Stream_Type;
      Item   : Ada.Streams.Stream_Element_Array);

   procedure Any_To_BS (Item : Any; Stream : out Buffer_Stream_Type);
   procedure BS_To_Any (Stream : Buffer_Stream_Type; Item : out Any);
   --  Conversion between an Any for an opaque sequence of octets and an Ada
   --  Stream based on a PolyORB buffer. For Any_To_BS, the lifetime of the
   --  Stream object shall not exceed that of Item.

   procedure Release_Buffer (Stream : in out Buffer_Stream_Type);
   --  Return storage allocated for Stream, needs to be called after any data
   --  has been written to the buffer.

   --------------
   -- Requests --
   --------------

   Nil_Exc_List : PolyORB.Any.ExceptionList.Ref
      renames PolyORB.Any.ExceptionList.Nil_Ref;

   procedure Request_Setup
     (Req       : out PolyORB.Requests.Request;
      Target    : PolyORB.References.Ref;
      Operation : String;
      Arg_List  : PolyORB.Any.NVList.Ref;
      Result    : in out PolyORB.Any.NamedValue;
      Exc_List  : PolyORB.Any.ExceptionList.Ref :=
                    PolyORB.Any.ExceptionList.Nil_Ref;
      Req_Flags :   PolyORB.Requests.Flags;
      Deferred_Arguments_Session : PolyORB.Components.Component_Access := null;
      Identification :   PolyORB.Requests.Arguments_Identification :=
                           PolyORB.Requests.Ident_By_Position;
      Dependent_Binding_Object : PolyORB.Smart_Pointers.Entity_Ptr := null
     ) renames PolyORB.Requests.Setup_Request;

   procedure Request_Invoke
     (R            : access PolyORB.Requests.Request;
      Invoke_Flags : PolyORB.Requests.Flags := 0);

   procedure Request_Arguments
     (R     : PolyORB.Requests.Request_Access;
      Args  : in out PolyORB.Any.NVList.Ref);

   procedure Request_Set_Out (R : PolyORB.Requests.Request_Access);

   procedure Set_Result
     (Self : PolyORB.Requests.Request_Access;
      Val  : Any)
     renames PolyORB.Requests.Set_Result;

   Asynchronous_P_To_Sync_Scope : constant array (Boolean)
     of PolyORB.Requests.Flags :=
       (False => PolyORB.Requests.Sync_With_Target,
        True  => PolyORB.Requests.Sync_With_Transport);

   --  Request_Flags to use for a request according to whether or not the call
   --  is asynchronous.

   procedure Request_Raise_Occurrence (R : Request);
   --  If R terminated with an exception, raise that exception
   --  If no exception occurred, do nothing.

   procedure Register_Termination_Manager
     (Ref      : PolyORB.References.Ref;
      Oid      : PolyORB.Objects.Object_Id_Access;
      Address  : System.Address;
      Shutdown : PolyORB.Initialization.Finalizer);
   --  Register the termination manager of the local partition

   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "");
   --  This procedure is unused for PolyORB (it is used only for the GARLIC
   --  implementation of Shared Passive.)

   procedure Register_RACW_In_Name_Server
     (Addr     : System.Address;
      Type_Tag : Ada.Tags.Tag;
      Name     : String;
      Kind     : String);
   --  Register a RACW in name server. Type_Tag is the name of pointed type.

   procedure Retrieve_RACW_From_Name_Server
     (Name     : String;
      Kind     : String;
      Stub_Tag : Ada.Tags.Tag;
      Addr     : out System.Address);
   --  Retreive a RACW from name server.

private

   pragma Inline
     (FA_B, FA_C, FA_F,
      FA_I8, FA_I16, FA_I32, FA_I64,
      FA_U8, FA_U16, FA_U32, FA_U64,
      FA_SF, FA_LF, FA_LLF,
      FA_WC, FA_String,

      TA_B, TA_C, TA_F,
      TA_I8, TA_I16, TA_I32, TA_I64,
      TA_U8, TA_U16, TA_U32, TA_U64,
      TA_SF, TA_LF, TA_LLF,
      TA_WC, TA_String);

   pragma Inline (Caseless_String_Eq, Get_Aggregate_Element);

   overriding function Execute_Servant
     (Self : not null access Servant;
      Req  : PolyORB.Requests.Request_Access) return Boolean;

   type Buffer_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Buf : aliased PolyORB.Buffers.Buffer_Type;
   end record;

   type Private_Info_Access is access all Private_Info;

   type Private_Info is record
      Next                : aliased Private_Info_Access;
      --  For chaining on All_Receiving_Stubs list

      Kind                : Receiving_Stub_Kind;
      --  Indicates whether this info is relative to RACW type or a RCI

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

end System.Partition_Interface;
