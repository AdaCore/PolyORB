--  The Request object.

--  $Id$

with Droopi.Log;

package body Droopi.Requests is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.requests");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create_Request
     (Req       : out Request_Access;
      Target    : Object;
      Operation : String;
      Args      : CORBA.NVList.Ref)
   is
      Result : constant Request_Access := new Request;
   begin
      Result.Target := new String'(Target.all);
      Result.Operation := new String'(Operation);
      Result.Args := Args;
      Req := Result;
   end Create_Request;

   procedure Free is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   procedure Destroy_Request
     (Req : in out Request_Access) is
   begin
      Free (Req.Target);
      Free (Req.Operation);
      Free (Req);
   end Destroy_Request;

   procedure Execute (Req : Request; Res : out Result)
   is
      The_Result : constant Result := new String'
        ("Your request " & Req.Operation.all
         & " was executed on object " & Req.Target.all
         --  & " with arguments " & Req.Args.all
         & "." & ASCII.CR & ASCII.LF);
   begin
      pragma Debug (O ("Execute: enter"));
      pragma Debug
        (O ("Result is « "
            & The_Result (The_Result'First .. The_Result'Last - 2)
            & " »."));
      Res := The_Result;
   end Execute;

   procedure Destroy (Res : in out Result) is
   begin
      Free (Res);
   end Destroy;

end Droopi.Requests;
