------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  The Request object.

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Annotations;
with PolyORB.Any;
with PolyORB.Any.NVList;
with PolyORB.Components;
with PolyORB.References;
with PolyORB.Storage_Pools;
with PolyORB.Task_Info;
with PolyORB.Types;

package PolyORB.Requests is

   -------------
   -- Request --
   -------------

   subtype Operation_Id is String;
   --  XXX or Types.Identifier??

   ------------------------------------------
   -- Synchronisation of request execution --
   ------------------------------------------

   type Synchronisation_Scope is
     (None,
      With_Transport,
      With_Server,
      With_Target);
   --  A 'synchronistaion scope' value is associated with
   --  each request object.

   --  When a request is not synchronised, the middleware returns
   --  to the caller before passing the request to the transport
   --  layer. The middleware MUST guarantee that the call is
   --  non-blocking.

   --  When a request is synchronised With_Transport, the middleware
   --  must not return to the caller before the corresponding
   --  message has been accepted by the transport layer.

   --  When a request is synchronised With_Server, the middleware
   --  does not return before receiving a confirmation that the
   --  request message has been received by the server middleware.

   --  When a request is synchronised With_Target, the middlware
   --  does not return to the caller before receinving a confirmation
   --  that the request has been executed by the target object.

   type Request is limited record
      --  Ctx        : CORBA.Context.Ref;
      Target    : References.Ref;
      Operation : Types.Identifier;

      Args      : Any.NVList.Ref;
      --  The arguments to the request, for transmission
      --  from caller to callee.

      --  On the server side, the Protocol layer MAY set this
      --  component directly OR set the following component
      --  instead. The Application layer MUST NOT access this
      --  component directly, and MUST use operation Arguments
      --  below to retrieve the arguments from an invocation and
      --  set up the structure for returned (out-mode) arguments.

      Out_Args : Any.NVList.Ref;
      --  Same as Args but for transmission from callee to caller.

      Deferred_Arguments_Session : Components.Component_Access;
      --  If Args have not been unmarshalled at the time the
      --  request is created, then this component must be
      --  set to the Session on which the arguments are
      --  waiting to be unmarshalled.

      --  When creating a Request object with deferred arguments,
      --  it is the Protocol layer's responsibility to ensure that
      --  consistent information is presented when
      --  Unmarshall_Arguments is called.

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
     use PolyORB.Storage_Pools.Debug_Pool;

   procedure Create_Request
     (Target    : in     References.Ref;
      --  May or may not be local!
      --  Ctx       : in     CORBA.Context.Ref;
      Operation : in     Operation_Id;
      Arg_List  : in     Any.NVList.Ref;
      Result    : in out Any.NamedValue;
      --  Exc_List  : in     ExceptionList.Ref;
      --  Ctxt_List : in     ContextList.Ref;
      Req       :    out Request_Access;
      --  Req_Flags : in     Flags;
      Deferred_Arguments_Session : in Components.Component_Access := null
     );

   procedure Invoke (Self : Request_Access);
   --  Run Self.

   procedure Arguments
     (Self : Request_Access;
      Args : in out Any.NVList.Ref);
   --  Retrieve the invocation's arguments into Args.
   --  Call back the protocol layer to do the unmarshalling,
   --  if necessary. Should be called exactly once from within
   --  a servant's Invoke primitive. Args MUST be a correctly
   --  typed NVList for the signature of the method being invoked.

   procedure Destroy_Request is new Ada.Unchecked_Deallocation
     (Request, Request_Access);

   function Image (Req : Request) return String;
   --  For debugging purposes.

   procedure Pump_Up_Arguments
     (Dst_Args        : in out Any.NVList.Ref;
      Src_Args        :        Any.NVList.Ref;
      Direction       :        Any.Flags;
      Ignore_Src_Mode :        Boolean        := True);
   --  True arguments of direction Direction (or INOUT) from received
   --  protocol arguments list P_Args (either from a request, on server
   --  side, or for a reply, on client side) into A_Args.

end PolyORB.Requests;
