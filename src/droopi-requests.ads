--  The Request object.

--  $Id$

with Ada.Unchecked_Deallocation;
with CORBA;
with CORBA.NVList;

with Droopi.References;

package Droopi.Requests is

   pragma Elaborate_Body;

   -------------
   -- Request --
   -------------

   type String_Ptr is access String;
   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ptr);
   --  XXX DUMMY

   subtype Operation_Id is String;
   --  XXX DUMMY VERSION of Operation_Id

   type Request is limited record
      --  Ctx        : CORBA.Context.Ref;
      Target    : References.Ref;
      Operation : String_Ptr;
      Args      : CORBA.NVList.Ref;
      Result    : CORBA.NamedValue;
      --  Exc_List   : CORBA.ExceptionList.Ref;
      --  Ctxt_List  : CORBA.ContextList.Ref;
      --  Req_Flags  : CORBA.Flags;
      Completed : aliased Boolean := False;
   end record;

   type Request_Access is access all Request;

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     CORBA.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out CORBA.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access
      --  Req_Flags : in     Flags
     );

   procedure Destroy_Request
     (Req : in out Request_Access);

   function Image (Req : Request) return String;
   --  For debugging purposes.

end Droopi.Requests;
