--  Mapping for the standard ServerRequest interface

--  $Id$

with PolyORB.Requests;

with CORBA.NVList;

package CORBA.ServerRequest is

   pragma Elaborate_Body;


--     interface ServerRequest { // PIDL
--         readonly attribute  Identifier operation;
--         void                arguments    (inout NVList nv);
--         Context             ctx();
--         void                set_result   (in any val);
--         void                set_exception(in any val);
--     };

   subtype Object is PolyORB.Requests.Request;
   subtype Object_Ptr is PolyORB.Requests.Request_Access;

   function Operation (O : Object) return Identifier;
   procedure Arguments (O : access Object; NV : in out NVList.Ref);
   --  function Ctx return Context;
   procedure Set_Result (O : access Object; Val : Any);
   procedure Set_Exception (O : Object; Val : Any);

   --------------------------------------
   -- The following is PolyORB-specific --
   --------------------------------------

--    function To_PolyORB_Request
--      (O : access Object)
--      return PolyORB.Requests.Request_Access;

--    function To_CORBA_ServerRequest
--      (R : PolyORB.Requests.Request_Access)
--      return Object_Ptr;

--  private

--    type Object is new PolyORB.Requests.Request with null record;

--    pragma Inline (To_PolyORB_Request);
--    pragma Inline (To_CORBA_ServerRequest);

end CORBA.ServerRequest;
