--  The Request object.

--  $Id$

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.Requests is

   use Droopi.Log;
   use Droopi.Types;

   package L is new Droopi.Log.Facility_Log ("droopi.requests");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     Any.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     Any.NVList.Ref;
      Result    : in out Any.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access
      --  Req_Flags : in     Flags
     )
   is
      Res : constant Request_Access := new Request;
   begin
      Res.Target    := Target;
      Res.Operation := To_Droopi_String (Operation);
      Res.Args      := Arg_List;
      Res.Result    := Result;

      Req := Res;
   end Create_Request;

--    procedure Destroy_Request
--      (Req : in out Request_Access) is
--    begin
--       Free (Req);
--    end Destroy_Request;

   function Image (Req : Request) return String
   is
      S1 : constant String
        := "Operation: " & To_Standard_String (Req.Operation)
        & " on object " & References.Image (Req.Target);
   begin
      declare
         S2 : constant String := Any.NVList.Image (Req.Args);
      begin
         return S1 & " with arguments " & S2;
      end;
   exception
      when others =>
         --  Could not render arguments.
         return S1 & " with non-representable arguments";
   end Image;

end Droopi.Requests;
