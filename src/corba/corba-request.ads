--  $Id$

with CORBA.Context;
with CORBA.NVList;
with CORBA.Object;

with Droopi.Requests;

package CORBA.Request is

   type Object is limited private;

   function Get_Droopi_Request
     (Request : Object)
     return Droopi.Requests.Request;

   procedure Create_Request
     (Self      : in     CORBA.Object.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : access NamedValue;
      Request   :    out Object;
      Req_Flags : in     Flags);

private

   type Object is record
      The_Request : Droopi.Requests.Request;
   end record;

end CORBA.Request;

