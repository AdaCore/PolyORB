--  The Request object.

--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Annotations;
with Droopi.Any;
with Droopi.Any.NVList;
with Droopi.References;
with Droopi.Storage_Pools;
with Droopi.Task_Info;
with Droopi.Types;

package Droopi.Requests is

   -------------
   -- Request --
   -------------

   subtype Operation_Id is String;
   --  XXX or Types.Identifier??

   type Request is limited record
      --  Ctx        : CORBA.Context.Ref;
      Target    : References.Ref;
      Operation : Types.Identifier;
      Args      : Any.NVList.Ref;
      Result    : Any.NamedValue;
      --  Exc_List   : CORBA.ExceptionList.Ref;
      --  Ctxt_List  : CORBA.ContextList.Ref;
      --  Req_Flags  : CORBA.Flags;

      Completed : aliased Boolean := False;
      Requesting_Task : aliased Task_Info.Task_Info_Access;

      Notepad : Annotations.Notepad;
      --  Request objects are manipulated by both the
      --  Application layer (which creates them on the client
      --  side and handles their execution on the server side)
      --  and the Protocol layer (which sends them out on
      --  the client side and receives them on the server side).
      --
      --  If only one layer had a need to associate specific
      --  information with a Request object, one could imagine
      --  that this layer would make a type extension of Request
      --  to store such information.
      --
      --  In our case, /both/ layers may need to attach
      --  layer-specific information to the same request object.
      --  Ada type derivation can therefore not be used (this
      --  would require actual Request objects to derive from
      --  both the application-layer derivation and the
      --  protocol-layer derivation). For this reason, annotations
      --  are used instead to allow each layer to independently
      --  store its specific add-on information in a Request.

   end record;

   type Request_Access is access all Request;
   for Request_Access'Storage_Pool
     use Droopi.Storage_Pools.Debug_Pool;

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     CORBA.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     Any.NVList.Ref;
      Result    : in out Any.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access
      --  Req_Flags : in     Flags
     );

   procedure Invoke (Self : Request_Access);
   --  Run Self.

   procedure Destroy_Request is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   function Image (Req : Request) return String;
   --  For debugging purposes.

end Droopi.Requests;
