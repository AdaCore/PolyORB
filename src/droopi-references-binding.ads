--  Object references (binding operation).

--  $Id$

with Droopi.Objects;
with Droopi.ORB;

package Droopi.References.Binding is

   pragma Elaborate_Body;

   function Bind
     (R         : Ref;
      Local_ORB : ORB.ORB_Access)
     return Objects.Servant_Access;
   --  Bind R to a servant, and return that servant.
   --  Local_ORB is the local middleware. It is used to determine
   --  whether reference profiles are local. Its object adapter
   --  is queried to resolve local object ids into servants.

   Invalid_Reference : exception;
   --  Raised when an attempt is made to bind a reference
   --  that is null or has no supported profile.

end Droopi.References.Binding;
