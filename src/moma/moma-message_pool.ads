with PolyORB.Minimal_Servant;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Obj_Adapters.Simple;

package MOMA.Message_Pool is

   type Object is new PolyORB.Minimal_Servant.Servant with record
      Msg : PolyORB.Types.String;
   end record;

   type Object_Acc is access Object;

   procedure Invoke
     (Self : access Object;
      Req  : in     PolyORB.Requests.Request_Access);

   function If_Desc
     return PolyORB.Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);

end MOMA.Message_Pool;
