with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Types;

package System.PolyORB_Interface is

   subtype NamedValue is PolyORB.Any.NamedValue;
   subtype Object_Ref is PolyORB.References.Ref;
   subtype NVList_Ref is PolyORB.Any.NVList.Ref;
   procedure NVList_Create (NVList : out PolyORB.Any.NVList.Ref)
     renames PolyORB.Any.NVList.Create;
   function To_PolyORB_String (S : String) return PolyORB.Types.String
     renames PolyORB.Types.To_PolyORB_String;
   subtype Operation_Id is PolyORB.Requests.Operation_Id;
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


end System.PolyORB_Interface;
