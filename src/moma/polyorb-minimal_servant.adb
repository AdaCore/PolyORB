with Ada.Tags;

with PolyORB.Log;
with PolyORB.Requests;
with PolyORB.Objects.Interface;

package body PolyORB.Minimal_Servant is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.minimal_servant");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------------
   -- Handle_Message --
   --------------------

   function Execute_Servant
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
      use PolyORB.Objects.Interface;

   begin
      pragma Debug (O ("Handling message of type "
                & Ada.Tags.External_Tag (Msg'Tag)));

      if Msg in Execute_Request then
         declare
            use PolyORB.Requests;

            R : constant Request_Access := Execute_Request (Msg).Req;
         begin
            Invoke (Servant'Class (Self.all)'Access, R);

            Set_Out_Args (R);

            return Executed_Request'(Req => R);
         end;
      else
         raise PolyORB.Components.Unhandled_Message;
      end if;
   end Execute_Servant;

   function Execute_Servant
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class is
   begin
      return Execute_Servant (Self.As_Servant, Msg);
   end Execute_Servant;

   ------------------------
   -- To_PolyORB_Servant --
   ------------------------

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access is
   begin
      return S.Neutral_View'Access;
   end To_PolyORB_Servant;

   ----------
   -- '=' --
   ----------

   function "=" (X, Y : Implementation) return Boolean
   is
   begin
      raise Program_Error;
      return False;
   end "=";

end PolyORB.Minimal_Servant;
