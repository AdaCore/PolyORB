--  Object references (binding operation).

--  $Id$

with PolyORB.Components;
with PolyORB.ORB;

package PolyORB.References.Binding is

   pragma Elaborate_Body;

   function Bind
     (R         : Ref;
      Local_ORB : ORB.ORB_Access)
     return Components.Component_Access;
   --  Bind R to a servant, and return that servant (or a surrogate
   --  thereof).
   --  Local_ORB is the local middleware. It is used to determine
   --  whether reference profiles are local. Its object adapter
   --  is queried to resolve local object ids into servants.
   --  When a remote reference is to be bound, Local_ORB is in
   --  charge of all the transport and communication aspects
   --  of the binding operation. It must then return a remote
   --  surrogate of the object designated by R.

   Invalid_Reference : exception;
   --  Raised when an attempt is made to bind a reference
   --  that is null or has no supported profile.

end PolyORB.References.Binding;
