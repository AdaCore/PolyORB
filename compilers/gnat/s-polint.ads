with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.Objects;
with PolyORB.Objects.Interface;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Types;

package System.PolyORB_Interface is

   pragma Elaborate_Body;

   function To_PolyORB_String (S : String)
     return PolyORB.Types.Identifier
     renames PolyORB.Types.To_PolyORB_String;
   function To_Standard_String (S : PolyORB.Types.Identifier)
     return String
     renames PolyORB.Types.To_Standard_String;

   subtype Any is PolyORB.Any.Any;
   Mode_In    : PolyORB.Any.Flags renames PolyORB.Any.ARG_IN;
   Mode_Out   : PolyORB.Any.Flags renames PolyORB.Any.ARG_OUT;
   Mode_Inout : PolyORB.Any.Flags renames PolyORB.Any.ARG_INOUT;
   subtype NamedValue is PolyORB.Any.NamedValue;
   subtype TypeCode is PolyORB.Any.TypeCode.Object;
   procedure Set_TC
     (A : in out PolyORB.Any.Any;
      T : PolyORB.Any.TypeCode.Object)
      renames PolyORB.Any.Set_Type;

   subtype Object_Ref is PolyORB.References.Ref;

   function Get_Empty_Any
     (Tc : PolyORB.Any.TypeCode.Object)
      return PolyORB.Any.Any
     renames PolyORB.Any.Get_Empty_Any;

   subtype NVList_Ref is PolyORB.Any.NVList.Ref;
   procedure NVList_Create (NVList : out PolyORB.Any.NVList.Ref)
     renames PolyORB.Any.NVList.Create;
   procedure NVList_Add_Item
     (Self       :    PolyORB.Any.NVList.Ref;
      Item_Name  : in PolyORB.Types.Identifier;
      Item       : in PolyORB.Any.Any;
      Item_Flags : in PolyORB.Any.Flags)
     renames PolyORB.Any.NVList.Add_Item;

   subtype Request_Access is PolyORB.Requests.Request_Access;
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
      Req_Flags : in     PolyORB.Any.Flags := 0;
      Deferred_Arguments_Session :
        in PolyORB.Components.Component_Access := null
     ) renames PolyORB.Requests.Create_Request;

   procedure Request_Invoke
     (R            : PolyORB.Requests.Request_Access;
      Invoke_Flags : PolyORB.Any.Flags               := 0)
     renames PolyORB.Requests.Invoke;

   procedure Request_Arguments
     (R    :        PolyORB.Requests.Request_Access;
      Args : in out PolyORB.Any.NVList.Ref)
     renames PolyORB.Requests.Arguments;

   procedure Request_Set_Out
     (R : PolyORB.Requests.Request_Access)
     renames PolyORB.Requests.Set_Out_Args;

--       function FA_AD (Item : PolyORB.Any.Any) return X;
--       function FA_AS (Item : PolyORB.Any.Any) return X;
   function FA_B (Item : PolyORB.Any.Any) return Boolean;
   function FA_C (Item : PolyORB.Any.Any) return Character;
   function FA_F (Item : PolyORB.Any.Any) return Float;
   function FA_I (Item : PolyORB.Any.Any) return Integer;
   function FA_LF (Item : PolyORB.Any.Any) return Long_Float;
   function FA_LI (Item : PolyORB.Any.Any) return Long_Integer;
   function FA_LLF (Item : PolyORB.Any.Any) return Long_Long_Float;
   function FA_LLI (Item : PolyORB.Any.Any) return Long_Long_Integer;
--       function FA_LLU (Item : PolyORB.Any.Any) return X;
--       function FA_LU (Item : PolyORB.Any.Any) return X;
   function FA_SF (Item : PolyORB.Any.Any) return Short_Float;
   function FA_SI (Item : PolyORB.Any.Any) return Short_Integer;
   function FA_SSI (Item : PolyORB.Any.Any) return Short_Short_Integer;
--       function FA_SSU (Item : PolyORB.Any.Any) return X;
--       function FA_SU (Item : PolyORB.Any.Any) return X;
--       function FA_U (Item : PolyORB.Any.Any) return X;
   function FA_WC (Item : PolyORB.Any.Any) return Wide_Character;

   function FA_String (Item : PolyORB.Any.Any) return String;

--     function TA_AD (X) return PolyORB.Any.Any;
--     function TA_AS (X) return PolyORB.Any.Any;
   function TA_B (Item : Boolean) return PolyORB.Any.Any;
   function TA_C (Item : Character) return PolyORB.Any.Any;
   function TA_F (Item : Float) return PolyORB.Any.Any;
   function TA_I (Item : Integer) return PolyORB.Any.Any;
   function TA_LF (Item : Long_Float) return PolyORB.Any.Any;
   function TA_LI (Item : Long_Integer) return PolyORB.Any.Any;
   function TA_LLF (Item : Long_Long_Float) return PolyORB.Any.Any;
   function TA_LLI (Item : Long_Long_Integer) return PolyORB.Any.Any;
--     function TA_LLU (X) return PolyORB.Any.Any;
--     function TA_LU (X) return PolyORB.Any.Any;
   function TA_SF (Item : Short_Float) return PolyORB.Any.Any;
   function TA_SI (Item : Short_Integer) return PolyORB.Any.Any;
   function TA_SSI (Item : Short_Short_Integer) return PolyORB.Any.Any;
--     function TA_SSU (X) return PolyORB.Any.Any;
--     function TA_SU (X) return PolyORB.Any.Any;
--     function TA_U (X) return PolyORB.Any.Any;
   function TA_WC (Item : Wide_Character) return PolyORB.Any.Any;

   function TA_String (S : String) return PolyORB.Any.Any;
   function TA_TC (TC : PolyORB.Any.TypeCode.Object) return PolyORB.Any.Any
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

   function TC_Alias return PolyORB.Any.TypeCode.Object
     renames PolyORB.Any.TypeCode.TC_Alias;
   --  Empty Tk_Alias typecode.

   type Any_Array is array (Natural range <>) of PolyORB.Any.Any;

   function TC_Build
     (Base       : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object;

   procedure Copy_Any_Value (Dest, Src : PolyORB.Any.Any)
     renames PolyORB.Any.Copy_Any_Value;

   procedure Set_Result
     (Self : PolyORB.Requests.Request_Access;
      Val  : PolyORB.Any.Any)
     renames PolyORB.Requests.Set_Result;

   --  RPC receiver objets are specialized PolyORB components

   type Message_Handler_Access is access
     function (M : PolyORB.Components.Message'Class)
               return PolyORB.Components.Message'Class;

   type Servant is new PolyORB.Objects.Servant with record
      Handler : Message_Handler_Access;
   end record;
   subtype Servant_Access is PolyORB.Objects.Servant_Access;

   function Handle_Message
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class;

   procedure Register_Receiving_Stub
     (Name     : in String;
      Receiver : in Servant_Access;
      Version  : in String := "");
   --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.

   subtype Message is PolyORB.Components.Message;
   subtype Null_Message is PolyORB.Components.Null_Message;
   subtype Execute_Request is
     PolyORB.Objects.Interface.Execute_Request;
   subtype Executed_Request is
     PolyORB.Objects.Interface.Executed_Request;

   function Caseless_String_Eq (S1, S2 : String) return Boolean;
   --  Case-less equality of S1 and S2.

   generic
      Name : String;
   package RCI_Info is
      function Get_RCI_Package_Ref
        return PolyORB.References.Ref;
   end RCI_Info;

private

   pragma Inline
     (FA_B, FA_C, FA_F, FA_I, FA_LF, FA_LI, FA_LLF, FA_LLI,
      FA_SF, FA_SI, FA_SSI, FA_WC, FA_String,

      TA_B, TA_C, TA_F, TA_I, TA_LF, TA_LI, TA_LLF, TA_LLI,
      TA_SF, TA_SI, TA_SSI, TA_WC, TA_String);

   pragma Inline (Caseless_String_Eq);

end System.PolyORB_Interface;
