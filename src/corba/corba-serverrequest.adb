--  Mapping for the standard ServerRequest interface

--  $Id$

with Droopi;
with Droopi.Types;

package body CORBA.ServerRequest is

   function Operation (O : Object) return Identifier is
   begin
      return O.Operation;
   end Operation;

   procedure Arguments (O : Object; NV : in out NVList.Ref) is
   begin
      NV := CORBA.NVList.To_CORBA_Ref (O.Args);
   end Arguments;

   procedure Set_Result (O : Object; Val : Any) is
   begin
      O.Result :=
        (Name      => Droopi.Types.To_Droopi_String ("result"),
         Argument  => Val,
         Arg_Modes => ARG_OUT);
   end Set_Result;

   procedure Set_Exception (O : Object; Val : Any) is
   begin
      --  O.Exception_Info := Val;
      raise Droopi.Not_Implemented;
   end Set_Exception;

   function To_Droopi_Request
     (O : Object)
     return Droopi.Requests.Request_Access is
   begin
      return Droopi.Requests.Request_Access (O);
   end To_Droopi_Request;

   function To_CORBA_ServerRequest
     (R : Droopi.Requests.Request_Access)
     return Object is
   begin
      return Object (R);
   end To_CORBA_ServerRequest;

end CORBA.ServerRequest;
