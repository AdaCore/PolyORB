--  The Request object.

--  $Id$

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);
with Droopi.ORB;
with Droopi.ORB.Interface;
with Droopi.Protocols.Interface;
with Droopi.Setup;

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
      Result_Any : Droopi.Any.Any := Any.Get_By_Ref (Result.Argument);
   begin
      Res.Target    := Target;
      Res.Operation := To_Droopi_String (Operation);
      Res.Args      := Arg_List;
      Res.Result    :=
        (Name      => Result.Name,
         Argument  => Result_Any,
         Arg_Modes => Result.Arg_Modes);

      Req := Res;
   end Create_Request;

   procedure Invoke (Self : Request_Access)
   is
      use Droopi.ORB;
      use Droopi.ORB.Interface;
      use Droopi.Setup;

   begin
      Droopi.ORB.Queue_Request_To_Handler
        (The_ORB.Tasking_Policy, The_ORB,
         Queue_Request'
         (Request   => Self,
          Requestor => null));
      --  XXX Only synchronous requests are supported!

      --  Execute the ORB until the request is completed.
      ORB.Run
        (The_ORB, Exit_Condition_T'
         (Condition => Self.Completed'Access,
          Task_Info => Self.Requesting_Task'Access),
         May_Poll => True);

   end Invoke;

   procedure Arguments
     (Self : Request_Access;
      Args : in out Any.NVList.Ref)
   is
      use Any.NVList;
      use Components;

   begin
      if Is_Nil (Self.Args) then
         pragma Assert (Self.Deferred_Arguments_Session /= null);
         Self.Args := Args;
         Components.Emit_No_Reply
           (Self.Deferred_Arguments_Session,
            Protocols.Interface.Unmarshall_Arguments'(Args => Args));
         Self.Deferred_Arguments_Session := null;
      else
         pragma Assert (Self.Deferred_Arguments_Session = null);
         Args := Self.Args;
      end if;
   end Arguments;

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
