--  Support for object method invocation protocols.

--  $Id$

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects.Interface;
with PolyORB.Protocols.Interface;

package body PolyORB.Protocols is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Objects.Interface;
   use PolyORB.ORB.Interface;
   use PolyORB.Protocols.Interface;

   package L is new PolyORB.Log.Facility_Log ("polyorb.protocols");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Free is new Ada.Unchecked_Deallocation
     (Session'Class, Session_Access);

   procedure Destroy_Session (S : in out Session_Access) is
   begin
      Free (S);
   end Destroy_Session;

   procedure Handle_Unmarshall_Arguments
     (S    : access Session;
      Args : in out Any.NVList.Ref) is
   begin
      raise Program_Error;
      --  By default: no support for deferred arguments
      --  unmarshalling.
   end Handle_Unmarshall_Arguments;

   function Handle_Message
     (Sess : access Session;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      Nothing : Components.Null_Message;
   begin
      pragma Debug
        (O ("Handling message of type "
            & Ada.Tags.External_Tag (S'Tag)));
      if S in Connect_Indication then
         Handle_Connect_Indication (Session_Access (Sess));
      elsif S in Connect_Confirmation then
         Handle_Connect_Confirmation (Session_Access (Sess));
      elsif S in Disconnect_Indication then
         Handle_Disconnect (Session_Access (Sess));
      elsif S in Data_Indication then
         Handle_Data_Indication (Session_Access (Sess));
      elsif S in Unmarshall_Arguments then
         declare
            Args : PolyORB.Any.NVList.Ref
              := Unmarshall_Arguments (S).Args;
         begin
            Handle_Unmarshall_Arguments
              (Session_Access (Sess), Args);
            return Unmarshalled_Arguments'(Args => Args);
         end;
      elsif S in Set_Server then
         Sess.Server := Set_Server (S).Server;
      elsif S in Execute_Request then
         Invoke_Request
           (Session_Access (Sess),
            Execute_Request (S).Req);
      elsif S in Executed_Request then
         declare
            Var_Req : Request_Access
              := Executed_Request (S).Req;
         begin
            Send_Reply
              (Session_Access (Sess),
               Executed_Request (S).Req);
            pragma Debug (O ("Destroying request..."));
            Destroy_Request (Var_Req);
            pragma Debug (O ("... done."));
         end;
      elsif S in Queue_Request then
         --  XXX
         --  This is very wrong:
         --    * a session should not ever receive Queue_Request
         --      (this is a message from the ORB interface!)
         --    * Put_Line must NEVER EVER be used at all.
         --      debugging messages MUST use the PolyORB.Log mechanism.
         --
         --  Therefore disabling all the branch.
         --  Thomas 20010823
         raise Program_Error;
--          Ada.Text_IO.Put_Line ("message is queue request");
--          Sess.Pending_Request := Queue_Request (S);
--          PolyORB.Soft_Links.Update (Sess.Request_Watcher);
      else
         raise Components.Unhandled_Message;
      end if;
      return Nothing;
   end Handle_Message;

   procedure Expect_Data
     (S      : access Session;
      In_Buf : Buffers.Buffer_Access;
      Max    : Ada.Streams.Stream_Element_Count) is
   begin
      Emit_No_Reply
        (Port   => Lower (S),
         Msg    => Data_Expected'
           (In_Buf => In_Buf, Max => Max));
   end Expect_Data;

   -------------------------
   -- Get_Request_Watcher --
   -------------------------

   function Get_Request_Watcher
     (S : in Session_Access)
     return PolyORB.Soft_Links.Watcher_Access
   is
   begin
      return S.Request_Watcher;
   end Get_Request_Watcher;

   -------------------------
   -- Set_Request_Watcher --
   -------------------------

   procedure Set_Request_Watcher
     (S : in Session_Access;
      W : PolyORB.Soft_Links.Watcher_Access)
   is
   begin
      S.Request_Watcher := W;
   end Set_Request_Watcher;

   -------------------------
   -- Get_Pending_Request --
   -------------------------

   function Get_Pending_Request
     (S : in Session_Access)
     return ORB.Interface.Queue_Request
   is
   begin
      return S.Pending_Request;
   end Get_Pending_Request;

end PolyORB.Protocols;
