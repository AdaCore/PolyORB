with PolyORB.Components;
with PolyORB.Servants;
with PolyORB.Smart_Pointers;
with PolyORB.Requests;

package PolyORB.Minimal_Servant is

   pragma Elaborate_Body;

   type Servant is abstract new PolyORB.Smart_Pointers.Entity
     with private;

   type Servant_Acc is access Servant;

   function Handle_Message
     (Self : access Servant;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class;

   function To_PolyORB_Servant (S : access Servant)
     return PolyORB.Servants.Servant_Access;

   procedure Invoke
     (Self    : access Servant;
      Request : in     PolyORB.Requests.Request_Access)
      is abstract;

private

   type Implementation (As_Servant : access Servant'Class)
   is new PolyORB.Servants.Servant with null record;
   --  The MOMA personality is based on the Portable Object Adapter.

   function "=" (X, Y : Implementation) return Boolean;
   --  XXX Why does the compiler require the presence of this operator?
   --  As a descendant of Component, Implementation is a limited type!

   function Handle_Message
     (Self : access Implementation;
      Msg  : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

   type Servant is abstract new PolyORB.Smart_Pointers.Entity with
   record
      Neutral_View : aliased Implementation (Servant'Access);
      --  The PolyORB (personality-neutral) view of this servant.
   end record;

end PolyORB.Minimal_Servant;
