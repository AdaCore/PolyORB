------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     P O L Y O R B . R E Q U E S T S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  The Request object

pragma Ada_2005;

with Ada.Finalization;

with PolyORB.Annotations;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.References;
with PolyORB.Smart_Pointers;
with PolyORB.Task_Info;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Types;
with PolyORB.Utils.Simple_Flags;
with PolyORB.Utils.Strings;

package PolyORB.Requests is

   use PolyORB.Errors;

   type Arguments_Identification is array (1 .. 2) of Boolean;
   pragma Pack (Arguments_Identification);

   Ident_By_Position : constant Arguments_Identification;
   Ident_By_Name     : constant Arguments_Identification;
   Ident_Unspecified : constant Arguments_Identification;
   Ident_Both        : constant Arguments_Identification;
   --  These constants are used to indicate how the arguments are
   --  handled by the personalities (both client and
   --  server). Ident_Unspecified is not supposed to be used directly
   --  by a personality, as it means that we do not know how the
   --  arguments are identified. It is used by the internal mechanisms.

   type Flags is new Types.Unsigned_Long;
   package Unsigned_Long_Flags is new PolyORB.Utils.Simple_Flags (Flags);

   ------------------------------------------
   -- Synchronisation of request execution --
   ------------------------------------------

   Sync_None           : constant Flags;
   Sync_With_Transport : constant Flags;
   Sync_With_Server    : constant Flags;
   Sync_With_Target    : constant Flags;
   Sync_Call_Back      : constant Flags;
   --  Flags to be used for member Req_Flags of request.

   --  When a request is not synchronised, the middleware returns to the caller
   --  before passing the request to the transport layer. The middleware MUST
   --  guarantee that the call is non-blocking.

   --  When a request is synchronised With_Transport, the middleware must not
   --  return to the caller before the corresponding message has been accepted
   --  by the transport layer.

   --  When a request is synchronised With_Server, the middleware does not
   --  return before receiving a confirmation that the request message has been
   --  received by the server middleware.

   --  When a request is synchronised With_Target, the middlware does not
   --  return to the caller before receinving a confirmation that the request
   --  has been executed by the target object.

   --  Document Sync_Call_Back???

   -------------
   -- Request --
   -------------

   Default_Flags : constant Flags;
   --  Default flag for member Req_Flags of request.

   type Request is new Ada.Finalization.Limited_Controlled with record
      Target    : References.Ref;
      --  A ref designating the target object

      Operation : PolyORB.Utils.Strings.String_Ptr;
      --  The name of the method to be invoked

      Args_Ident : Arguments_Identification := Ident_By_Position;
      --  To optimize the handling of Args, we can provide a hint about the
      --  structure of the NV_List.

      Args      : Any.NVList.Ref;
      --  The arguments to the request, for transmission from caller to callee.

      --  On the server side, the Protocol layer MAY set this component
      --  directly OR set the following component instead. The Application
      --  layer MUST NOT access this component directly, and MUST use operation
      --  Arguments below to retrieve the arguments from an invocation and set
      --  up the structure for returned (out-mode) arguments.

      Out_Args : Any.NVList.Ref;
      --  Same as Args but for transmission from callee to caller

      Deferred_Arguments_Session : Components.Component_Access;
      --  If Args have not been unmarshalled at the time the request is
      --  created, then this component must be set to the Session on which the
      --  arguments are waiting to be unmarshalled.

      Arguments_Called  : Boolean := False;
      Set_Result_Called : Boolean := False;
      --  Flags to guard against double invocation of Arguments and Set_Result
      --  on the same request.

      --  When creating a Request object with deferred arguments, it is the
      --  Protocol layer's responsibility to ensure that consistent information
      --  is presented when Unmarshall_Arguments is called.

      Result    : Any.NamedValue;
      --  The result returned by the object after execution of this request

      Exc_List   : PolyORB.Any.ExceptionList.Ref;
      --  The list of user exceptions potentially raised by this request
      --  XXX This is client-side info. How do we construct it on a proxy
      --      object?

      Exception_Info : aliased Any.Any;
      --  If non-empty, information relative to an exception raised during
      --  execution of this request.

      Req_Flags : Flags;
      --  Additional flags

      Aborted   : Boolean := False;
      --  Set True if the request is aborted

      Completed : aliased Boolean := False;
      --  Indicate whether the request is completed or not.
      --  Note: request execution state when completing the request depends on
      --  synchronisation flags used.

      Requesting_Task : aliased PolyORB.Task_Info.Task_Info_Access;
      --  Task requesting request completion. This task will be executing the
      --  ORB main loop until the request is completed.
      --  Note: Requesting_Task is set up when entering ORB main loop,
      --  see PolyORB.ORB.Run for more details.

      Requesting_Component : Components.Component_Access;
      --  Component requesting request execution. The response, if any, will be
      --  redirected to this component.

      Surrogate : Components.Component_Access;
      --  Component handling request execution

      Profile : Binding_Data.Profile_Access;
      --  Profile of target ref selected when binding to Surrogate

      Upcall_Abortable : access Tasking.Threads.Abortable'Class;
      Upcall_Abortable_Mutex : Tasking.Mutexes.Mutex_Access;
      --  While the request is being served by an upcall to an application
      --  servant, this handle is set to designate the corresponding abortable
      --  object, to allow the execution to be aborted. Abortion should occur
      --  under Upcall_Abortable_Mutex protection to ensure that the abortable
      --  object is not prematurely destroyed.

      Dependent_Binding_Object : Smart_Pointers.Ref;
      --  A reference to the binding object from which a server-side request
      --  was created. Used to prevent said BO from being destroyed will the
      --  request is still being processed by the application layer.

      --  XXX study feasibility & cost of merging Dependent_Binding_Object with
      --  Requestor? Maybe by making all components Non_Controlled_Entities?

      Notepad : Annotations.Notepad;
      --  Request objects are manipulated by both the Application layer (which
      --  creates them on the client side and handles their execution on the
      --  server side) and the Protocol layer (which sends them out on the
      --  client side and receives them on the server side).
      --
      --  If only one layer had a need to associate specific information with a
      --  Request object, one could imagine that this layer would make a type
      --  extension of Request to store such information.
      --
      --  In our case, /both/ layers may need to attach layer-specific
      --  information to the same request object. Ada type derivation can
      --  therefore not be used (this would require actual Request objects to
      --  derive from both the application-layer derivation and the
      --  protocol-layer derivation). For this reason, annotations are used
      --  instead to allow each layer to independently store its specific
      --  add-on information in a Request.
   end record;

   procedure Initialize (Req : in out Request);
   procedure Finalize (Req : in out Request);

   type Request_Access is access all Request;

   procedure Create_Request
     (Target                     : References.Ref;
      Operation                  : String;
      Arg_List                   : Any.NVList.Ref;
      Result                     : in out Any.NamedValue;
      Exc_List                   : Any.ExceptionList.Ref :=
                                     Any.ExceptionList.Nil_Ref;
      Req                        : out Request_Access;
      Req_Flags                  : Flags := Default_Flags;
      Deferred_Arguments_Session : Components.Component_Access := null;
      Identification             : Arguments_Identification :=
                                     Ident_By_Position;
      Dependent_Binding_Object   : Smart_Pointers.Entity_Ptr := null);

   procedure Setup_Request
     (Req                        : out Request;
      Target                     : References.Ref;
      Operation                  : String;
      Arg_List                   : Any.NVList.Ref;
      Result                     : in out Any.NamedValue;
      Exc_List                   : Any.ExceptionList.Ref :=
                                     Any.ExceptionList.Nil_Ref;
      Req_Flags                  : Flags := Default_Flags;
      Deferred_Arguments_Session : Components.Component_Access := null;
      Identification             : Arguments_Identification :=
                                     Ident_By_Position;
      Dependent_Binding_Object   : Smart_Pointers.Entity_Ptr := null);

   procedure Invoke (Self : access Request; Invoke_Flags : Flags := 0);
   --  Run Self.
   --  XXX Invoke_Flags is currently set to 0, and not used. It is kept
   --  for future use.

   procedure Arguments
     (Self           :        Request_Access;
      Args           : in out Any.NVList.Ref;
      Error          : in out Error_Container;
      Identification :        Arguments_Identification := Ident_By_Position;
      Can_Extend     :        Boolean := False);
      --  Retrieve the invocation's arguments into Args. Call back the protocol
      --  layer to do the unmarshalling, if necessary. Should be called exactly
      --  once from within a servant's Invoke primitive. Args MUST be a
      --  correctly typed NVList for the signature of the method being invoked.
      --  If Can_Extend is set to True and Self contains extra arguments that
      --  are not required by Args, they are appended. Identification is used
      --  to specify the capailities of the server personality.

   procedure Set_Result (Self : Request_Access; Val : Any.Any);
   --  Set the value of Self's result to Val. Assert no error has been thrown

   procedure Set_Result
     (Self  :        Request_Access;
      Val   :        Any.Any;
      Error : in out Error_Container);
   --  Set the value of Self's result to Val

   procedure Set_Exception (Self : in out Request; Error : Error_Container);
   pragma Inline (Set_Exception);

   procedure Set_Out_Args
     (Self           :        Request_Access;
      Error          : in out Error_Container;
      Identification : Arguments_Identification := Ident_By_Position);
   --  Copy back the values of out and inout arguments from Out_Args to Args.
   --  Identification is used to specify the capabilities of the server
   --  personality.

   procedure Destroy_Request (Req : in out Request_Access);

   function Image (Req : Request) return String;
   --  For debugging purposes

   procedure Reset_Request (Request : in out PolyORB.Requests.Request);
   --  Set request to a state where it can be re-issued: exception and
   --  arguments status are reset.

private

   Sync_None           : constant Flags := 1;
   Sync_With_Transport : constant Flags := 2;
   Sync_With_Server    : constant Flags := 4;
   Sync_With_Target    : constant Flags := 8;
   Sync_Call_Back      : constant Flags := 16;
   Default_Flags       : constant Flags := Sync_With_Target;

   Ident_By_Position : constant Arguments_Identification := (True,  False);
   Ident_By_Name     : constant Arguments_Identification := (False, True);
   Ident_Unspecified : constant Arguments_Identification := (False, False);
   Ident_Both        : constant Arguments_Identification := (True,  True);

end PolyORB.Requests;
