--  The Request object.

--  $Id$

with Ada.Unchecked_Deallocation;
with CORBA.NVList;

package Droopi.Requests is

   pragma Elaborate_Body;

   -------------
   -- Request --
   -------------

   --  A DUMMY ONE, to be replaced by something like
   --  CORBA.Request.

   type Request is limited private;
   type Request_Access is access all Request;

   type String_Ptr is access String;
   subtype Object is String_Ptr;
   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Ptr);

   procedure Create_Request
     (Req       : out Request_Access;
      Target    : Object;
      Operation : String;
      Args      : CORBA.NVList.Ref);

   procedure Destroy_Request
     (Req : in out Request_Access);

   ------------
   -- Result --
   ------------

   --  Juste a placeholder, likewise.

   subtype Result is String_Ptr;

   procedure Execute (Req : Request; Res : out Result);
   procedure Destroy (Res : in out Result);

private

   type Request is tagged limited record
      Target    : Object;
      Operation : String_Ptr;
      Args      : CORBA.NVList.Ref;
   end record;

end Droopi.Requests;
