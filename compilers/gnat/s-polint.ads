with System.RPC;
with Interfaces;

with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Components;
with PolyORB.Obj_Adapters;
with PolyORB.Objects;
with PolyORB.POA_Config;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package System.PolyORB_Interface is

   pragma Elaborate_Body;

   subtype Object_Ref is PolyORB.References.Ref;

   ---------------------
   -- RCI information --
   ---------------------

   --  Calling stubs need a cache of the object reference
   --  associated with each RCI unit.

   generic
      RCI_Name : String;
   package RCI_Locator is
      function Get_RCI_Package_Ref
        return Object_Ref;
   end RCI_Locator;

   --  Receiving stubs contain a table of all subprograms
   --  exported by the unit.

   type RCI_Subp_Info is record
      Name : System.Address;
      --  Subprogram distribution identifier

      Addr : System.Address;
      --  Local address of the actual subprogram
   end record;

   ---------------------------------------
   -- Remote access-to-subprogram types --
   ---------------------------------------

   procedure Get_RAS_Ref
     (Pkg_Name        :     String;
      Subprogram_Name :     String;
      Subp_Ref        : out Object_Ref);
   --  Return the RAS object reference associated with the
   --  named subprogram.

   --------------------------
   -- RPC receiver objects --
   --------------------------

   --  One RPC receiver is created for each supported interface,
   --  i.e. one for each RCI library unit, and one for each
   --  type that is the designated type of one or more RACW type.

   function Caseless_String_Eq (S1, S2 : String) return Boolean;
   --  Case-less equality of S1 and S2.

   subtype Request_Access is PolyORB.Requests.Request_Access;

   type Request_Handler_Access is access
     procedure (R : Request_Access);

   type Servant is new PolyORB.Objects.Servant with record
      Handler        : Request_Handler_Access;
      --  The dispatching routine.

      Object_Adapter : PolyORB.Obj_Adapters.Obj_Adapter_Access;
      --  Null for RCI servants (the root POA will be used in
      --  this case.)

      Obj_TypeCode   : PolyORB.Any.TypeCode.Object;
      --  The TypeCode to be used for references to objects
      --  of this type.
   end record;
   type Servant_Access is access all Servant'Class;

   procedure Register_Obj_Receiving_Stub
     (Name          : in String;
      Handler       : in Request_Handler_Access;
      Receiver      : in Servant_Access);
   --  Register Receiver as the RPC servant for distributed objects
   --  of type Name, at elaboration time.

   procedure Register_Pkg_Receiving_Stub
     (Name                : String;
      Version             : String;
      Handler             : Request_Handler_Access;
      Receiver            : Servant_Access;
      Is_All_Calls_Remote : Boolean);
      --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.

   --------------------------------------------
   -- Support for RACWs as object references --
   --------------------------------------------

   subtype Entity_Ptr is PolyORB.Smart_Pointers.Entity_Ptr;

   procedure Inc_Usage (E : PolyORB.Smart_Pointers.Entity_Ptr)
     renames PolyORB.Smart_Pointers.Inc_Usage;
   --  In stubs for remote objects, the object reference
   --  information is stored as a naked Entity_Ptr. We therefore
   --  need to account for this reference by hand.

   procedure Set_Ref
     (The_Ref    : in out PolyORB.References.Ref;
      The_Entity :        PolyORB.Smart_Pointers.Entity_Ptr)
     renames PolyORB.References.Set;
   function Entity_Of
     (R : PolyORB.References.Ref)
      return PolyORB.Smart_Pointers.Entity_Ptr
     renames PolyORB.References.Entity_Of;
   --  Conversion from Entity_Ptr to Ref and reverse

   type RACW_Stub_Type is tagged limited record
      Origin       : System.RPC.Partition_ID;
      Receiver     : Interfaces.Unsigned_64;
      --  XXX the 2 fields above are placeholders and must not
      --  be used (they are kept here only while Exp_Dist is
      --  not completely updated for PolyORB).

      Target       : Entity_Ptr;
      --  Target cannot be a References.Ref (a controlled type)
      --  because that would pollute RACW_Stub_Type's dispatch
      --  table (which must be exactly identical to that of
      --  the designated tagged type).
      --  Target must be a pointer to References.Reference_Info.

      Addr         : System.Address := System.Null_Address;
      --  If this stub is for a remote access-to-subprogram type
      --  that designates a local subprogram, then this field
      --  is set to that subprogram's address, else it is
      --  Null_Address.

      Asynchronous : Boolean;
   end record;
   type RACW_Stub_Type_Access is access RACW_Stub_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating
   --  exp_dist.adb.

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access);
   --  Get a unique pointer on a remote object

   function To_PolyORB_String (S : String)
     return PolyORB.Types.Identifier
     renames PolyORB.Types.To_PolyORB_String;
   function To_Standard_String (S : PolyORB.Types.Identifier)
     return String
     renames PolyORB.Types.To_Standard_String;

   function Is_Nil (R : PolyORB.References.Ref) return Boolean
     renames PolyORB.References.Is_Nil;

   RACW_POA_Config : PolyORB.POA_Config.Configuration_Access;

   procedure Get_Local_Address
     (Ref      : PolyORB.References.Ref;
      Is_Local : out Boolean;
      Addr     : out System.Address);
   --  If Ref denotes a local object, Is_Local is set to True,
   --  and Addr is set to the object's actual address, else
   --  Is_Local is set to False and Addr is set to Null_Address.

   procedure Get_Reference
     (Addr     :        System.Address;
      Typ      :        String;
      Receiver : access Servant;
      Ref      :    out PolyORB.References.Ref);
   --  Create a reference that can be used to desginate the
   --  object whose address is Addr, whose type is the designated
   --  type of a RACW type associated with Servant.

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

   function Get_Empty_Any
     (Tc : PolyORB.Any.TypeCode.Object)
      return Any
     renames PolyORB.Any.Get_Empty_Any;

   subtype NVList_Ref is PolyORB.Any.NVList.Ref;
   procedure NVList_Create (NVList : out PolyORB.Any.NVList.Ref)
     renames PolyORB.Any.NVList.Create;
   procedure NVList_Add_Item
     (Self       :    PolyORB.Any.NVList.Ref;
      Item_Name  : in PolyORB.Types.Identifier;
      Item       : in Any;
      Item_Flags : in PolyORB.Any.Flags)
     renames PolyORB.Any.NVList.Add_Item;


   --  Elementary From_Any and To_Any operations

--       function FA_AD (Item : Any) return X;
--       function FA_AS (Item : Any) return X;
   function FA_B (Item : Any) return Boolean;
   function FA_C (Item : Any) return Character;
   function FA_F (Item : Any) return Float;
   function FA_I (Item : Any) return Integer;
   function FA_LF (Item : Any) return Long_Float;
   function FA_LI (Item : Any) return Long_Integer;
   function FA_LLF (Item : Any) return Long_Long_Float;
   function FA_LLI (Item : Any) return Long_Long_Integer;
--       function FA_LLU (Item : Any) return X;
--       function FA_LU (Item : Any) return X;
   function FA_SF (Item : Any) return Short_Float;
   function FA_SI (Item : Any) return Short_Integer;
   function FA_SSI (Item : Any) return Short_Short_Integer;
--       function FA_SSU (Item : Any) return X;
--       function FA_SU (Item : Any) return X;
--       function FA_U (Item : Any) return X;
   function FA_WC (Item : Any) return Wide_Character;

   function FA_String (Item : Any) return String;

   function FA_ObjRef
     (Item : Any)
      return PolyORB.References.Ref
     renames PolyORB.Any.ObjRef.From_Any;

--     function TA_AD (X) return Any;
--     function TA_AS (X) return Any;
   function TA_B (Item : Boolean) return Any;
   function TA_C (Item : Character) return Any;
   function TA_F (Item : Float) return Any;
   function TA_I (Item : Integer) return Any;
   function TA_LF (Item : Long_Float) return Any;
   function TA_LI (Item : Long_Integer) return Any;
   function TA_LLF (Item : Long_Long_Float) return Any;
   function TA_LLI (Item : Long_Long_Integer) return Any;
--     function TA_LLU (X) return Any;
--     function TA_LU (X) return Any;
   function TA_SF (Item : Short_Float) return Any;
   function TA_SI (Item : Short_Integer) return Any;
   function TA_SSI (Item : Short_Short_Integer) return Any;
--     function TA_SSU (X) return Any;
--     function TA_SU (X) return Any;
--     function TA_U (X) return Any;
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

   --  Warning! Ada numeric types have platform dependant sizes,
   --  PolyORB types are fixed size: this mapping may need to
   --  be changed for other platforms (or the biggest PolyORB
   --  type for each Ada type should be selected, if cross-platform
   --  interoperability is desired.

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

   function TC_Alias return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Alias;
   --  Empty Tk_Alias typecode.

   type Any_Array is array (Natural range <>) of Any;

   function TC_Build
     (Base       : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object;

   procedure Copy_Any_Value (Dest, Src : Any)
     renames PolyORB.Any.Copy_Any_Value;

   function Any_Aggregate_Build
     (TypeCode : PolyORB.Any.TypeCode.Object;
      Contents : Any_Array)
      return Any;

   --------------
   -- Requests --
   --------------

   Nil_Exc_List : PolyORB.Any.ExceptionList.Ref
      renames PolyORB.Any.ExceptionList.Nil_Ref;

   procedure Request_Create
     (Target    : in     PolyORB.References.Ref;
      Operation : in     PolyORB.Requests.Operation_Id;
      Arg_List  : in     PolyORB.Any.NVList.Ref;
      Result    : in out PolyORB.Any.NamedValue;
      Exc_List  : in     PolyORB.Any.ExceptionList.Ref
        := PolyORB.Any.ExceptionList.Nil_Ref;
      Req       :    out PolyORB.Requests.Request_Access;
      Req_Flags : in     PolyORB.Requests.Flags := 0;
      Deferred_Arguments_Session :
        in PolyORB.Components.Component_Access := null
     ) renames PolyORB.Requests.Create_Request;

   procedure Request_Invoke
     (R            : PolyORB.Requests.Request_Access;
      Invoke_Flags : PolyORB.Requests.Flags          := 0)
     renames PolyORB.Requests.Invoke;

   procedure Request_Arguments
     (R    :        PolyORB.Requests.Request_Access;
      Args : in out PolyORB.Any.NVList.Ref)
     renames PolyORB.Requests.Arguments;

   procedure Request_Set_Out
     (R : PolyORB.Requests.Request_Access)
     renames PolyORB.Requests.Set_Out_Args;

   procedure Set_Result
     (Self : PolyORB.Requests.Request_Access;
      Val  : Any)
     renames PolyORB.Requests.Set_Result;

private

   pragma Inline
     (FA_B, FA_C, FA_F, FA_I, FA_LF, FA_LI, FA_LLF, FA_LLI,
      FA_SF, FA_SI, FA_SSI, FA_WC, FA_String,

      TA_B, TA_C, TA_F, TA_I, TA_LF, TA_LI, TA_LLF, TA_LLI,
      TA_SF, TA_SI, TA_SSI, TA_WC, TA_String);

   pragma Inline (Caseless_String_Eq);

   function Handle_Message
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class;
   pragma Inline (Handle_Message);

end System.PolyORB_Interface;
