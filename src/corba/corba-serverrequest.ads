--  Mapping for the standard ServerRequest interface

--  $Id$

with Droopi.Requests;

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

   type Object is private;

   function Operation (O : Object) return Identifier;
   procedure Arguments (O : Object; NV : in out NVList.Ref);
   --  function Ctx return Context;
   procedure Set_Result (O : Object; Val : Any);
   procedure Set_Exception (O : Object; Val : Any);

private

   type Object is new Droopi.Requests.Request_Access;

end CORBA.ServerRequest;
