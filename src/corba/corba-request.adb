--  $Id$

with CORBA.Context;
with CORBA.NVList;

with Droopi.Requests;

package body CORBA.Request is

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out Object;
      Req_Flags : in     Flags) is
   begin
      Droopi.Requests.Create_Request
        (Target    => Self,
         Operation => Operation,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Object.The_Request);
      --  XXX Some arguments are not taken into account!
   end Create_Request;

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
   begin
      raise Droopi.Not_Implemented;
   end Create_Request;

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0) is
   begin
      raise Droopi.Not_Implemented;
   end Invoke;

   procedure Delete (Self : in out Object) is
   begin
      Droopi.Requests.Destroy_Request (Self.The_Request);
   end Delete;

   function Get_Droopi_Request
     (Request : Object)
     return Droopi.Requests.Request_Access is
   begin
      return Request.The_Request;
   end Get_Droopi_Request;

end CORBA.Request;

