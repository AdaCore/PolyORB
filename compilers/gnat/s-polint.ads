with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Types;

package System.PolyORB_Interface is

   pragma Elaborate_Body;

   function To_PolyORB_String (S : Standard.String)
     return PolyORB.Types.Identifier
     renames PolyORB.Types.To_PolyORB_String;

   subtype Any is PolyORB.Any.Any;
   Mode_In    : PolyORB.Any.Flags renames PolyORB.Any.ARG_IN;
   Mode_Out   : PolyORB.Any.Flags renames PolyORB.Any.ARG_OUT;
   Mode_Inout : PolyORB.Any.Flags renames PolyORB.Any.ARG_INOUT;
   subtype NamedValue is PolyORB.Any.NamedValue;
   subtype TypeCode is PolyORB.Any.TypeCode.Object;

   subtype Object_Ref is PolyORB.References.Ref;

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
      Deferred_Arguments_Session : in PolyORB.Components.Component_Access := null
     ) renames PolyORB.Requests.Create_Request;

--       function FA_AD (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_AS (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_B (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_C (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_F (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_I (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LF (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LI (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LLF (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LLI (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LLU (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_LU (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_SF (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_SI (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_SSI (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_SSU (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_SU (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_U (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;
--       function FA_WC (Item : PolyORB.Any.Any) return X renames PolyORB.Any.From_Any;

--       function TA_AD (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_AS (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_B (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_C (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_F (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_I (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LF (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LI (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LLF (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LLI (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LLU (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_LU (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_SF (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_SI (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_SSI (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_SSU (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_SU (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_U (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
--       function TA_WC (X) return PolyORB.Any.Any renames PolyORB.Any.To_Any;
   function TA_String (S : String) return PolyORB.Any.Any;
   function TA_TC (TC : PolyORB.Any.TypeCode.Object) return PolyORB.Any.Any
     renames PolyORB.Any.To_Any;
--       function TC_AD return PolyORB.Any.TypeCode.Object renames PolyORB.Any.TC_X;
--       function TC_AS return PolyORB.Any.TypeCode.Object renames PolyORB.Any.TC_X;

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

   type Any_Array is array (Natural range <>) of PolyORB.Any.Any;

   function TC_Build
     (Base : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object;

end System.PolyORB_Interface;
