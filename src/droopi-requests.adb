--  The Request object.

--  $Id$

with Droopi.Log;

package body Droopi.Requests is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.requests");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     CORBA.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out CORBA.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access
      --  Req_Flags : in     Flags
     )
   is
      Res : constant Request_Access := new Request;
   begin
      Res.Target := Target;
      Res.Operation := new String'(Operation);
      Res.Args := Arg_List;
      Res.Result := Result;
      Req := Res;
   end Create_Request;

   procedure Free is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   procedure Destroy_Request
     (Req : in out Request_Access) is
   begin
      Free (Req.Operation);
      Free (Req);
   end Destroy_Request;

   procedure Execute_Request
     (Req : in out Request)
   is
      The_Result : constant String_Ptr := new String'
        ("Your request " & Image (Req)
         & " was executed."
         & ASCII.CR & ASCII.LF);
   begin
      pragma Debug (O ("Execute: enter"));
      pragma Debug
        (O ("Result is « "
            & The_Result (The_Result'First .. The_Result'Last - 2)
            & " »."));
      --  XXX TODO
      --  Request.Res := The_Result;
      null;
   end Execute_Request;

   function Image (Req : Request) return String is
   begin
      return "Request: " & Req.Operation.all
        & " on object " & References.Image (Req.Target)
        & " with arguments " & CORBA.NVList.Image (Req.Args);
   end Image;

end Droopi.Requests;
