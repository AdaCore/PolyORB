--  The CORBA Dynamic Invocation Interface.

--  $Id$

with CORBA.AbstractBase;
with CORBA.Context;
with CORBA.ContextList;
with CORBA.ExceptionList;
with CORBA.NVList;

with Droopi.Requests;

package CORBA.Request is

   --  pragma Elaborate_Body;

   type Object is limited private;

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags);

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0);

   procedure Delete (Self : in out Object);

   --  XXX incomplete!

   --  The following is specific to DROOPI.

   function To_Droopi_Request
     (Request : Object)
     return Droopi.Requests.Request_Access;

   function To_CORBA_Request
     (Request : Droopi.Requests.Request_Access)
     return Object;

private

   type Object is record
      The_Request : Droopi.Requests.Request_Access;
   end record;
   --  XXX Would it not be simpler to declare
   --  type Object is new Droopi.Requests.Request_Access; ?
   --  (as is presently done in CORBA.ServerRequest!)

   pragma Inline (To_Droopi_Request);
   pragma Inline (To_CORBA_Request);

end CORBA.Request;

