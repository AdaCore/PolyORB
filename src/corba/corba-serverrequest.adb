--  Mapping for the standard ServerRequest interface

--  $Id$

with PolyORB;
with PolyORB.Any.NVList;
with PolyORB.Types;

package body CORBA.ServerRequest is

   function Operation (O : Object) return Identifier is
   begin
      return O.Operation;
   end Operation;

   procedure Arguments (O : access Object; NV : in out NVList.Ref) is
      PolyORB_Args : PolyORB.Any.NVList.Ref
        := CORBA.NVList.To_PolyORB_Ref (NV);
   begin
      PolyORB.Requests.Arguments
        (PolyORB.Requests.Request_Access (O), PolyORB_Args);
      NV := CORBA.NVList.To_CORBA_Ref (PolyORB_Args);
   end Arguments;

   procedure Set_Result (O : access Object; Val : Any) is
   begin
      O.Result :=
        (Name      => PolyORB.Types.To_PolyORB_String ("result"),
         Argument  => Val,
         Arg_Modes => ARG_OUT);
   end Set_Result;

   procedure Set_Exception (O : Object; Val : Any) is
   begin
      --  O.Exception_Info := Val;
      raise PolyORB.Not_Implemented;
   end Set_Exception;

--    function To_PolyORB_Request
--      (O : Object)
--      return PolyORB.Requests.Request_Access is
--    begin
--       return PolyORB.Requests.Request_Access (O);
--    end To_PolyORB_Request;

--    function To_CORBA_ServerRequest
--      (R : PolyORB.Requests.Request_Access)
--      return Object is
--    begin
--       return Object (R);
--    end To_CORBA_ServerRequest;

end CORBA.ServerRequest;
