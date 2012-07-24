------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;

with PolyORB.Annotations;
with PolyORB.Any;
with PolyORB.Binding_Data;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Interceptors_Slots;
with PolyORB.Errors.Helper;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.POA;
with PolyORB.QoS.Addressing_Modes;
with PolyORB.QoS.Service_Contexts;
with PolyORB.References;
with PolyORB.Requests;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers.Controlled_Entities;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads.Annotations;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings;

with PortableServer;

with PortableInterceptor.ClientRequestInfo;
with PortableInterceptor.ClientRequestInfo.Impl;
with PortableInterceptor.IORInfo.Impl;
with PortableInterceptor.IORInterceptor_3_0.Helper;
with PortableInterceptor.ORBInitInfo.Impl;
with PortableInterceptor.ServerRequestInfo;
with PortableInterceptor.ServerRequestInfo.Impl;

package body PolyORB.CORBA_P.Interceptors is

   use PolyORB.Annotations;
   use PolyORB.CORBA_P.Interceptors_Slots;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Requests.Unsigned_Long_Flags;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads.Annotations;
   use type PolyORB.Any.TypeCode.Local_Ref;

   package PSPCE renames PolyORB.Smart_Pointers.Controlled_Entities;

   --  Client Interceptors

   function "="
     (Left, Right : PortableInterceptor.ClientRequestInterceptor.Local_Ref)
      return Boolean;

   package ClientRequestInterceptor_Lists is
      new PolyORB.Utils.Chained_Lists
           (PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   All_Client_Interceptors : ClientRequestInterceptor_Lists.List;

   procedure Client_Invoke
     (Request : access PolyORB.Requests.Request;
      Flags   : PolyORB.Requests.Flags);

   function Create_Client_Request_Info
     (Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long;
      Point      : Client_Interception_Point;
      Target     : CORBA.Object.Ref)
      return PortableInterceptor.ClientRequestInfo.Local_Ref;

   generic
      with procedure Operation
        (Self : PortableInterceptor.ClientRequestInterceptor.Local_Ref;
         Info : PortableInterceptor.ClientRequestInfo.Local_Ref);
   procedure Call_Client_Request_Interceptor_Operation
     (Self     : PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Info     : PortableInterceptor.ClientRequestInfo.Local_Ref;
      Forward  : Boolean;
      Excp_Inf : in out PolyORB.Any.Any);

   --  Server interceptors

   type Server_Interceptor_Note is new PolyORB.Annotations.Note with record
      Servant             : PortableServer.Servant;
      Profile             : PolyORB.Binding_Data.Profile_Access;
      Request_Id          : CORBA.Unsigned_Long;
      Last_Interceptor    : Natural;
      Exception_Info      : PolyORB.Any.Any;
      Intermediate_Called : Boolean;
   end record;

   function "="
     (Left, Right : PortableInterceptor.ServerRequestInterceptor.Local_Ref)
      return Boolean;

   package ServerRequestInterceptor_Lists is
      new PolyORB.Utils.Chained_Lists
           (PortableInterceptor.ServerRequestInterceptor.Local_Ref);

   All_Server_Interceptors : ServerRequestInterceptor_Lists.List;

   procedure Server_Invoke
     (Servant : access PSPCE.Entity'Class;
      Request : access PolyORB.Requests.Request;
      Profile : PolyORB.Binding_Data.Profile_Access);

   procedure Server_Intermediate
     (Request        : access PolyORB.Requests.Request;
      From_Arguments : Boolean);

   function Create_Server_Request_Info
     (Servant      : PortableServer.Servant;
      Request      : PolyORB.Requests.Request_Access;
      Request_Id   : CORBA.Unsigned_Long;
      Profile      : PolyORB.Binding_Data.Profile_Access;
      Point        : Server_Interception_Point;
      Args_Present : Boolean)
      return PortableInterceptor.ServerRequestInfo.Local_Ref;

   generic
      with procedure Operation
        (Self : PortableInterceptor.ServerRequestInterceptor.Local_Ref;
         Info : PortableInterceptor.ServerRequestInfo.Local_Ref);
   procedure Call_Server_Request_Interceptor_Operation
     (Self     : PortableInterceptor.ServerRequestInterceptor.Local_Ref;
      Info     : PortableInterceptor.ServerRequestInfo.Local_Ref;
      Forward  : Boolean;
      Excp_Inf : in out PolyORB.Any.Any);

   --  IOR interceptors

   function "="
     (Left, Right : PortableInterceptor.IORInterceptor.Local_Ref)
      return Boolean;

   package IORInterceptor_Lists is
      new PolyORB.Utils.Chained_Lists
           (PortableInterceptor.IORInterceptor.Local_Ref);

   All_IOR_Interceptors : IORInterceptor_Lists.List;

   procedure POA_Create
     (POA   : PolyORB.POA.Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container);

   --  ORB_Initializers

   function "=" (Left, Right : PortableInterceptor.ORBInitializer.Local_Ref)
     return Boolean;

   package Initializer_Ref_Lists is
     new PolyORB.Utils.Chained_Lists
          (PortableInterceptor.ORBInitializer.Local_Ref);

   All_Initializer_Refs : Initializer_Ref_Lists.List;

   --  Internal subprograms

   function To_PolyORB_ForwardRequest_Members_Any
     (Members : PortableInterceptor.ForwardRequest_Members)
      return PolyORB.Any.Any;
   pragma Inline (To_PolyORB_ForwardRequest_Members_Any);
   --  Converting PortableInterceptor::ForwardRequest_Members into
   --  PolyORB internal representation.

   --  Request_Id generation support

   Request_Id_Counter : CORBA.Unsigned_Long := 0;
   Request_Id_Mutex   : Mutex_Access        := null;

   function Allocate_Request_Id return CORBA.Unsigned_Long;
   --  Allocate unused Request_Id

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : PortableInterceptor.ClientRequestInterceptor.Local_Ref)
      return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : PortableInterceptor.IORInterceptor.Local_Ref)
     return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : PortableInterceptor.ORBInitializer.Local_Ref)
      return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : PortableInterceptor.ServerRequestInterceptor.Local_Ref)
      return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ------------------------------------
   -- Add_Client_Request_Interceptor --
   ------------------------------------

   procedure Add_Client_Request_Interceptor
     (Interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref)
   is
   begin
      ClientRequestInterceptor_Lists.Append
        (All_Client_Interceptors, Interceptor);
   end Add_Client_Request_Interceptor;

   -------------------------
   -- Add_IOR_Interceptor --
   -------------------------

   procedure Add_IOR_Interceptor
     (Interceptor : PortableInterceptor.IORInterceptor.Local_Ref)
   is
   begin
      IORInterceptor_Lists.Append (All_IOR_Interceptors, Interceptor);
   end Add_IOR_Interceptor;

   ------------------------------------
   -- Add_Server_Request_Interceptor --
   ------------------------------------

   procedure Add_Server_Request_Interceptor
     (Interceptor : PortableInterceptor.ServerRequestInterceptor.Local_Ref)
   is
   begin
      ServerRequestInterceptor_Lists.Append
        (All_Server_Interceptors, Interceptor);
   end Add_Server_Request_Interceptor;

   -------------------------
   -- Allocate_Request_Id --
   -------------------------

   function Allocate_Request_Id return CORBA.Unsigned_Long is
      use type CORBA.Unsigned_Long;

      Result : CORBA.Unsigned_Long;
   begin
      Enter (Request_Id_Mutex);

      Result             := Request_Id_Counter;
      Request_Id_Counter := Request_Id_Counter + 1;

      Leave (Request_Id_Mutex);

      return Result;
   end Allocate_Request_Id;

   -----------------------------------------------
   -- Call_Client_Request_Interceptor_Operation --
   -----------------------------------------------

   procedure Call_Client_Request_Interceptor_Operation
     (Self     : PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Info     : PortableInterceptor.ClientRequestInfo.Local_Ref;
      Forward  : Boolean;
      Excp_Inf : in out PolyORB.Any.Any)
   is
   begin
      Operation (Self, Info);

   exception
      when E : CORBA.Unknown |
               CORBA.Bad_Param |
               CORBA.No_Memory |
               CORBA.Imp_Limit |
               CORBA.Comm_Failure |
               CORBA.Inv_Objref |
               CORBA.No_Permission |
               CORBA.Internal |
               CORBA.Marshal |
               CORBA.Initialize |
               CORBA.No_Implement |
               CORBA.Bad_TypeCode |
               CORBA.Bad_Operation |
               CORBA.No_Resources |
               CORBA.No_Response |
               CORBA.Persist_Store |
               CORBA.Bad_Inv_Order |
               CORBA.Transient |
               CORBA.Free_Mem |
               CORBA.Inv_Ident |
               CORBA.Inv_Flag |
               CORBA.Intf_Repos |
               CORBA.Bad_Context |
               CORBA.Obj_Adapter |
               CORBA.Data_Conversion |
               CORBA.Object_Not_Exist |
               CORBA.Transaction_Required |
               CORBA.Transaction_Rolledback |
               CORBA.Invalid_Transaction |
               CORBA.Inv_Policy |
               CORBA.Codeset_Incompatible |
               CORBA.Rebind |
               CORBA.Timeout |
               CORBA.Transaction_Unavailable |
               CORBA.Transaction_Mode |
               CORBA.Bad_Qos =>

         Excp_Inf := PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E);

      when E : PortableInterceptor.ForwardRequest =>

         --  If forwarding at this interception point is allowed then
         --  convert PortableInterceptor::ForwardRequest to
         --  PolyORB::ForwardRequest.

         if Forward then
            declare
               Members : PortableInterceptor.ForwardRequest_Members;

            begin
               PolyORB.Exceptions.User_Get_Members (E, Members);

               Excp_Inf := To_PolyORB_ForwardRequest_Members_Any (Members);
            end;

         else
            raise;
         end if;
   end Call_Client_Request_Interceptor_Operation;

   ---------------------------
   -- Call_ORB_Initializers --
   ---------------------------

   procedure Call_ORB_Initializers is
      use Initializer_Ref_Lists;

      Info_Ptr : constant PortableInterceptor.ORBInitInfo.Impl.Object_Ptr
        := new PortableInterceptor.ORBInitInfo.Impl.Object;
      Info_Ref : PortableInterceptor.ORBInitInfo.Local_Ref;
   begin
      PortableInterceptor.ORBInitInfo.Impl.Init (Info_Ptr);

      PortableInterceptor.ORBInitInfo.Set
        (Info_Ref, PolyORB.Smart_Pointers.Entity_Ptr (Info_Ptr));

      declare
         Iter : Iterator := First (All_Initializer_Refs);
      begin
         while not Last (Iter) loop
            PortableInterceptor.ORBInitializer.Pre_Init
              (Value (Iter).all, Info_Ref);
            Next (Iter);
         end loop;
      end;

      declare
         Iter : Iterator := First (All_Initializer_Refs);
      begin
         while not Last (Iter) loop
            PortableInterceptor.ORBInitializer.Post_Init
              (Value (Iter).all, Info_Ref);
            Next (Iter);
         end loop;
      end;

      --  Mark in ORBInitInfo the fact of initialization complete. This is
      --  required for raise exceptions on all ORBInitInfo operations if some
      --  of Interceptors cache ORBInitInfo reference.

      PortableInterceptor.ORBInitInfo.Impl.Post_Init_Done (Info_Ptr);
   end Call_ORB_Initializers;

   -----------------------------------------------
   -- Call_Server_Request_Interceptor_Operation --
   -----------------------------------------------

   procedure Call_Server_Request_Interceptor_Operation
     (Self     : PortableInterceptor.ServerRequestInterceptor.Local_Ref;
      Info     : PortableInterceptor.ServerRequestInfo.Local_Ref;
      Forward  : Boolean;
      Excp_Inf : in out PolyORB.Any.Any)
   is
   begin
      Operation (Self, Info);

   exception
      when E : CORBA.Unknown |
               CORBA.Bad_Param |
               CORBA.No_Memory |
               CORBA.Imp_Limit |
               CORBA.Comm_Failure |
               CORBA.Inv_Objref |
               CORBA.No_Permission |
               CORBA.Internal |
               CORBA.Marshal |
               CORBA.Initialize |
               CORBA.No_Implement |
               CORBA.Bad_TypeCode |
               CORBA.Bad_Operation |
               CORBA.No_Resources |
               CORBA.No_Response |
               CORBA.Persist_Store |
               CORBA.Bad_Inv_Order |
               CORBA.Transient |
               CORBA.Free_Mem |
               CORBA.Inv_Ident |
               CORBA.Inv_Flag |
               CORBA.Intf_Repos |
               CORBA.Bad_Context |
               CORBA.Obj_Adapter |
               CORBA.Data_Conversion |
               CORBA.Object_Not_Exist |
               CORBA.Transaction_Required |
               CORBA.Transaction_Rolledback |
               CORBA.Invalid_Transaction |
               CORBA.Inv_Policy |
               CORBA.Codeset_Incompatible |
               CORBA.Rebind |
               CORBA.Timeout |
               CORBA.Transaction_Unavailable |
               CORBA.Transaction_Mode |
               CORBA.Bad_Qos =>

         Excp_Inf := PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E);

      when E : PortableInterceptor.ForwardRequest =>

         --  If forwarding at this interception point is allowed then
         --  convert PortableInterceptor::ForwardRequest to
         --  PolyORB::ForwardRequest.

         if Forward then
            declare
               Members : PortableInterceptor.ForwardRequest_Members;

            begin
               PolyORB.Exceptions.User_Get_Members (E, Members);

               Excp_Inf := To_PolyORB_ForwardRequest_Members_Any (Members);
            end;

         else
            raise;
         end if;
   end Call_Server_Request_Interceptor_Operation;

   -------------------
   -- Client_Invoke --
   -------------------

   procedure Client_Invoke
     (Request : access PolyORB.Requests.Request;
      Flags   : PolyORB.Requests.Flags)
   is
      use ClientRequestInterceptor_Lists;
      use type PolyORB.Any.TypeCode.Object;
      use type PolyORB.Requests.Request_Access;

      procedure Call_Send_Request is
         new Call_Client_Request_Interceptor_Operation
              (PortableInterceptor.ClientRequestInterceptor.Send_Request);

      procedure Call_Receive_Reply is
         new Call_Client_Request_Interceptor_Operation
             (PortableInterceptor.ClientRequestInterceptor.Receive_Reply);

      procedure Call_Receive_Exception is
         new Call_Client_Request_Interceptor_Operation
              (PortableInterceptor.ClientRequestInterceptor.Receive_Exception);

      procedure Call_Receive_Other is
         new Call_Client_Request_Interceptor_Operation
              (PortableInterceptor.ClientRequestInterceptor.Receive_Other);

      Req_Id  : constant CORBA.Unsigned_Long := Allocate_Request_Id;

      Target  : constant CORBA.Object.Ref :=
        CORBA.Object.Internals.To_CORBA_Ref (Request.Target);
      TSC     : Slots_Note;
      Index   : Natural;

   begin
      --  Getting thread scope slots information (allocating thread scope
      --  slots if it is not allocated), and make "logical copy" and place it
      --  in the request.

      Get_Note (Get_Current_Thread_Notepad.all, TSC, Invalid_Slots_Note);

      if not Is_Allocated (TSC) then
         Allocate_Slots (TSC);
      end if;

      loop
         Set_Note (Request.Notepad, TSC);

         Rebuild_Request_Service_Contexts (Request.all);

         Index := Length (All_Client_Interceptors);

         --  Call Send_Request on all interceptors.

         for J in 0 .. Index - 1 loop
            Call_Send_Request
              (Element (All_Client_Interceptors, J).all,
               Create_Client_Request_Info
                 (Request.all'Unchecked_Access, Req_Id, Send_Request, Target),
               True,
               Request.Exception_Info);

            --  If got system or ForwardRequest exception then avoid call
            --  Send_Request on other Interceptors.

            if not PolyORB.Any.Is_Empty (Request.Exception_Info) then
               Index := J;
               exit;
            end if;
         end loop;

         Rebuild_Request_QoS_Parameters (Request.all);

         --  Avoid operation invocation if interceptor raise system
         --  exception.

         if Index = Length (All_Client_Interceptors) then
            PolyORB.Requests.Invoke (Request, Flags);

            --  Restore request scope slots, because it may be changed
            --  during invocation.

            Set_Note (Request.Notepad, TSC);
         end if;

         Rebuild_Request_Service_Contexts (Request.all);
         Rebuild_Reply_Service_Contexts (Request.all);

         for J in reverse 0 .. Index - 1 loop
            if not PolyORB.Any.Is_Empty (Request.Exception_Info) then
               if PolyORB.Any.Get_Type (Request.Exception_Info)
                    = PolyORB.Errors.Helper.TC_ForwardRequest
                 or else PolyORB.Any.Get_Type (Request.Exception_Info)
                    = PolyORB.Errors.Helper.TC_NeedsAddressingMode
               then
                  Call_Receive_Other
                    (Element (All_Client_Interceptors, J).all,
                     Create_Client_Request_Info
                       (Request.all'Unchecked_Access,
                        Req_Id,
                        Receive_Other,
                        Target),
                     True,
                     Request.Exception_Info);

               else
                  Call_Receive_Exception
                    (Element (All_Client_Interceptors, J).all,
                     Create_Client_Request_Info
                       (Request.all'Unchecked_Access,
                        Req_Id,
                        Receive_Exception,
                        Target),
                     True,
                     Request.Exception_Info);
               end if;

            else
               if Is_Set (Requests.Sync_With_Server, Request.Req_Flags)
                 or else Is_Set (Requests.Sync_With_Target, Request.Req_Flags)
               then
                  --  A reply is expected

                  Call_Receive_Reply
                    (Element (All_Client_Interceptors, J).all,
                     Create_Client_Request_Info
                       (Request.all'Unchecked_Access,
                        Req_Id,
                        Receive_Reply,
                        Target),
                     False,
                     Request.Exception_Info);
               else
                  Call_Receive_Other
                    (Element (All_Client_Interceptors, J).all,
                     Create_Client_Request_Info
                       (Request.all'Unchecked_Access,
                        Req_Id,
                        Receive_Other,
                        Target),
                     True,
                     Request.Exception_Info);
               end if;
            end if;
         end loop;

         exit when PolyORB.Any.Is_Empty (Request.Exception_Info)
           or else (PolyORB.Any.Get_Type (Request.Exception_Info)
                      /= PolyORB.Errors.Helper.TC_ForwardRequest
             and then PolyORB.Any.Get_Type (Request.Exception_Info)
                      /= PolyORB.Errors.Helper.TC_NeedsAddressingMode);

         --  XXX Reinvocation is possible iff request sync_scope is
         --  Sync_With_Server or Sync_With_Target. May be we add
         --  pragma Assert here?

         if PolyORB.Any.Get_Type (Request.Exception_Info)
              = PolyORB.Errors.Helper.TC_ForwardRequest
         then
            --  Reinvocation. Extract object reference from ForwardRequest
            --  exception and reinitialize request.

            declare
               Members : constant PolyORB.Errors.ForwardRequest_Members
                 := PolyORB.Errors.Helper.From_Any (Request.Exception_Info);
               Ref     : PolyORB.References.Ref;

            begin
               PolyORB.References.Set
                 (Ref,
                  Smart_Pointers.Entity_Of (Members.Forward_Reference));

               PolyORB.Requests.Reset_Request (Request.all);
            end;

         else
            --  Reinvocation. Set requested GIOP addressing mode and
            --  reinitialize request.

            declare
               use PolyORB.QoS;
               use PolyORB.QoS.Addressing_Modes;
               use PolyORB.Request_QoS;

               Members : constant PolyORB.Errors.NeedsAddressingMode_Members
                 := PolyORB.Errors.Helper.From_Any (Request.Exception_Info);

            begin
               PolyORB.Requests.Reset_Request (Request.all);

               Add_Request_QoS
                 (Request.all,
                  GIOP_Addressing_Mode,
                  new QoS_GIOP_Addressing_Mode_Parameter'
                  (Kind => GIOP_Addressing_Mode,
                   Mode => Members.Mode));
            end;
         end if;
      end loop;

      --  Restoring thread scope slots.

      Set_Note (Get_Current_Thread_Notepad.all, TSC);
   end Client_Invoke;

   --------------------------------
   -- Create_Client_Request_Info --
   --------------------------------

   function Create_Client_Request_Info
     (Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long;
      Point      : Client_Interception_Point;
      Target     : CORBA.Object.Ref)
      return PortableInterceptor.ClientRequestInfo.Local_Ref
   is
      Info_Ptr : constant PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
         := new PortableInterceptor.ClientRequestInfo.Impl.Object;
      Info_Ref : PortableInterceptor.ClientRequestInfo.Local_Ref;

   begin
      PortableInterceptor.ClientRequestInfo.Impl.Init
       (Info_Ptr, Point, Request, Request_Id, Target);

      PortableInterceptor.ClientRequestInfo.Set
        (Info_Ref, PolyORB.Smart_Pointers.Entity_Ptr (Info_Ptr));

      return Info_Ref;
   end Create_Client_Request_Info;

   --------------------------------
   -- Create_Server_Request_Info --
   --------------------------------

   function Create_Server_Request_Info
     (Servant      : PortableServer.Servant;
      Request      : PolyORB.Requests.Request_Access;
      Request_Id   : CORBA.Unsigned_Long;
      Profile      : PolyORB.Binding_Data.Profile_Access;
      Point        : Server_Interception_Point;
      Args_Present : Boolean)
      return PortableInterceptor.ServerRequestInfo.Local_Ref
   is
      Info_Ptr : constant PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
         := new PortableInterceptor.ServerRequestInfo.Impl.Object;
      Info_Ref : PortableInterceptor.ServerRequestInfo.Local_Ref;
   begin
      PortableInterceptor.ServerRequestInfo.Impl.Init
       (Info_Ptr, Point, Servant, Request, Request_Id, Profile, Args_Present);

      PortableInterceptor.ServerRequestInfo.Set
        (Info_Ref, PolyORB.Smart_Pointers.Entity_Ptr (Info_Ptr));

      return Info_Ref;
   end Create_Server_Request_Info;

   ------------------------------------------
   -- Is_Client_Request_Interceptor_Exists --
   ------------------------------------------

   function Is_Client_Request_Interceptor_Exists
     (Name : String)
      return Boolean
   is
      Iter : ClientRequestInterceptor_Lists.Iterator
         := ClientRequestInterceptor_Lists.First (All_Client_Interceptors);
   begin
      if Name = "" then
         return False;
      end if;

      while not ClientRequestInterceptor_Lists.Last (Iter) loop
         if CORBA.To_Standard_String
              (PortableInterceptor.ClientRequestInterceptor.Get_Name
                (ClientRequestInterceptor_Lists.Value (Iter).all))
             = Name
         then
            return True;
         end if;

         ClientRequestInterceptor_Lists.Next (Iter);
      end loop;

      return False;
   end Is_Client_Request_Interceptor_Exists;

   -------------------------------
   -- Is_IOR_Interceptor_Exists --
   -------------------------------

   function Is_IOR_Interceptor_Exists
     (Name : String)
      return Boolean
   is
      Iter : IORInterceptor_Lists.Iterator
         := IORInterceptor_Lists.First (All_IOR_Interceptors);
   begin
      if Name = "" then
         return False;
      end if;

      while not IORInterceptor_Lists.Last (Iter) loop
         if CORBA.To_Standard_String
              (PortableInterceptor.IORInterceptor.Get_Name
                (IORInterceptor_Lists.Value (Iter).all))
             = Name
         then
            return True;
         end if;

         IORInterceptor_Lists.Next (Iter);
      end loop;

      return False;
   end Is_IOR_Interceptor_Exists;

   ------------------------------------------
   -- Is_Server_Request_Interceptor_Exists --
   ------------------------------------------

   function Is_Server_Request_Interceptor_Exists
     (Name : String)
      return Boolean
   is
      Iter : ServerRequestInterceptor_Lists.Iterator
         := ServerRequestInterceptor_Lists.First (All_Server_Interceptors);
   begin
      if Name = "" then
         return False;
      end if;

      while not ServerRequestInterceptor_Lists.Last (Iter) loop
         if CORBA.To_Standard_String
              (PortableInterceptor.ServerRequestInterceptor.Get_Name
                (ServerRequestInterceptor_Lists.Value (Iter).all))
             = Name
         then
            return True;
         end if;

         ServerRequestInterceptor_Lists.Next (Iter);
      end loop;

      return False;
   end Is_Server_Request_Interceptor_Exists;

   ----------------
   -- POA_Create --
   ----------------

   procedure POA_Create
     (POA   : PolyORB.POA.Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      Iter     : IORInterceptor_Lists.Iterator;
      Info     : PortableInterceptor.IORInfo.Local_Ref;
      Info_Obj : constant PortableInterceptor.IORInfo.Impl.Object_Ptr
        := new PortableInterceptor.IORInfo.Impl.Object;

   begin
      --  Creating and initializing IOR Info object.

      PortableInterceptor.IORInfo.Impl.Init (Info_Obj, POA);
      PortableInterceptor.IORInfo.Set
        (Info, PolyORB.Smart_Pointers.Entity_Ptr (Info_Obj));

      --  Call Establish_Components on all registered IOR interceptors.

      Iter := IORInterceptor_Lists.First (All_IOR_Interceptors);
      while not IORInterceptor_Lists.Last (Iter) loop
         begin
            PortableInterceptor.IORInterceptor.Establish_Components
              (IORInterceptor_Lists.Value (Iter).all,
               Info);
         exception
            when others =>
               null;
         end;
         IORInterceptor_Lists.Next (Iter);
      end loop;

      --  Call Components_Established on all registered IOR interceptors
      --  with version 3.0.

      Iter := IORInterceptor_Lists.First (All_IOR_Interceptors);
      while not IORInterceptor_Lists.Last (Iter) loop
         if PortableInterceptor.IORInterceptor.Is_A
             (IORInterceptor_Lists.Value (Iter).all,
              PortableInterceptor.IORInterceptor_3_0.Repository_Id)
         then
            begin
               PortableInterceptor.IORInterceptor_3_0.Components_Established
                 (PortableInterceptor.IORInterceptor_3_0.Helper.To_Local_Ref
                   (IORInterceptor_Lists.Value (Iter).all),
                  Info);
            exception
               when others =>
                  PolyORB.Errors.Throw
                   (Error,
                    PolyORB.Errors.Obj_Adapter_E,
                    PolyORB.Errors.System_Exception_Members'
                     (Minor     => 6,
                      Completed => PolyORB.Errors.Completed_No));
                  return;
            end;
         end if;
         IORInterceptor_Lists.Next (Iter);
      end loop;
   end POA_Create;

   ------------------------------
   -- Register_ORB_Initializer --
   ------------------------------

   procedure Register_ORB_Initializer
     (Init : PortableInterceptor.ORBInitializer.Local_Ref)
   is
      use Initializer_Ref_Lists;
   begin
      Append (All_Initializer_Refs, Init);
   end Register_ORB_Initializer;

   -------------------------
   -- Server_Intermediate --
   -------------------------

   procedure Server_Intermediate
     (Request        : access PolyORB.Requests.Request;
      From_Arguments : Boolean)
   is
      use ServerRequestInterceptor_Lists;

      procedure Call_Receive_Request is
         new Call_Server_Request_Interceptor_Operation
              (PortableInterceptor.ServerRequestInterceptor.Receive_Request);

      Note             : Server_Interceptor_Note;
      Break_Invocation : Boolean := False;

      It : Iterator := First (All_Server_Interceptors);

   begin
      PolyORB.Annotations.Get_Note (Request.Notepad, Note);

      if not Note.Intermediate_Called then
         Note.Intermediate_Called := True;

         while not Last (It) loop
            Call_Receive_Request
              (Value (It).all,
               Create_Server_Request_Info
                 (Note.Servant,
                  Request.all'Unchecked_Access,
                  Note.Request_Id,
                  Note.Profile,
                  Receive_Request,
                  From_Arguments),
               True,
               Note.Exception_Info);

            if not PolyORB.Any.Is_Empty (Note.Exception_Info) then
               --  Exception information can't be saved in Request,
               --  because skeleton replace it to CORBA.UNKNOWN
               --  exception.

               Break_Invocation := True;
               exit;
            end if;
            Next (It);
         end loop;
      end if;

      Rebuild_Reply_QoS_Parameters (Request.all);

      PolyORB.Annotations.Set_Note (Request.Notepad, Note);

      if Break_Invocation then
         --  XXX Is this valid for PolyORB::ForwardRequest?

         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Note.Exception_Info);
      end if;
   end Server_Intermediate;

   -------------------
   -- Server_Invoke --
   -------------------

   procedure Server_Invoke
     (Servant : access PSPCE.Entity'Class;
      Request : access PolyORB.Requests.Request;
      Profile : PolyORB.Binding_Data.Profile_Access)
   is
      use ServerRequestInterceptor_Lists;
      use type PolyORB.Any.TypeCode.Object;

      package PISRI renames PortableInterceptor.ServerRequestInterceptor;

      procedure Call_Receive_Request_Service_Contexts is
         new Call_Server_Request_Interceptor_Operation
              (PISRI.Receive_Request_Service_Contexts);

      procedure Call_Send_Reply is
         new Call_Server_Request_Interceptor_Operation (PISRI.Send_Reply);

      procedure Call_Send_Exception is
         new Call_Server_Request_Interceptor_Operation (PISRI.Send_Exception);

      procedure Call_Send_Other is
         new Call_Server_Request_Interceptor_Operation (PISRI.Send_Other);

      RSC             : Slots_Note;
      Empty_Any       : PolyORB.Any.Any;
      Skip_Invocation : Boolean := False;
      Note            : Server_Interceptor_Note
        := (PolyORB.Annotations.Note with
              Servant             => PortableServer.Servant (Servant),
              Profile             => Profile,
              Request_Id          => Allocate_Request_Id,
              Last_Interceptor    => Length (All_Server_Interceptors),
              Exception_Info      => Empty_Any,
              Intermediate_Called => False);
   begin
      --  Allocating thread request scope slots. Storing it in the request.

      Allocate_Slots (RSC);
      Set_Note (Request.Notepad, RSC);

      Rebuild_Request_Service_Contexts (Request.all);

      for J in 0 .. Note.Last_Interceptor - 1 loop
         Call_Receive_Request_Service_Contexts
           (Element (All_Server_Interceptors, J).all,
            Create_Server_Request_Info
              (null,
               Request.all'Unchecked_Access,
               Note.Request_Id,
               Profile,
               Receive_Request_Service_Contexts,
               False),
            True,
            Request.Exception_Info);

         --  If got system or ForwardRequest exception then avoid call
         --  Receive_Request_Service_Contexts on other Interceptors.

         if not PolyORB.Any.Is_Empty (Request.Exception_Info) then
            Note.Last_Interceptor  := J;
            Skip_Invocation        := True;
            exit;
         end if;
      end loop;

      Rebuild_Reply_QoS_Parameters (Request.all);

      --  Copy ing request scope slots to thread scope slots

      Get_Note (Request.Notepad, RSC);
      Set_Note (Get_Current_Thread_Notepad.all, RSC);

      --  Saving in request information for calling intermediate
      --  interception point.

      Set_Note (Request.Notepad, Note);

      if not Skip_Invocation then
         PortableServer.Invoke
           (PortableServer.DynamicImplementation'Class (Servant.all)'Access,
            Request.all'Unchecked_Access);
         --  Redispatch
      end if;

      Get_Note (Request.Notepad, Note);

      if not PolyORB.Any.Is_Empty (Note.Exception_Info) then
         --  If a system exception or ForwardRequest exception will be
         --  raised in Receive_Request interception point then replace
         --  Request exception information, because it may be replaced
         --  in skeleton.

         Request.Exception_Info := Note.Exception_Info;
      end if;

      --  Retrieve thread scope slots and copy it back to request
      --  scope slots.

      Get_Note (Get_Current_Thread_Notepad.all, RSC);
      Set_Note (Request.Notepad, RSC);

      for J in reverse 0 .. Note.Last_Interceptor - 1 loop
         if not PolyORB.Any.Is_Empty (Request.Exception_Info) then
            if PolyORB.Any.Get_Type (Request.Exception_Info)
                 = PolyORB.Errors.Helper.TC_ForwardRequest
              or else PolyORB.Any.Get_Type (Request.Exception_Info)
                 = PolyORB.Errors.Helper.TC_NeedsAddressingMode
            then
               Call_Send_Other
                 (Element (All_Server_Interceptors, J).all,
                  Create_Server_Request_Info
                    (null,
                     Request.all'Unchecked_Access,
                     Note.Request_Id,
                     Profile,
                     Send_Other,
                     True),
                  True,
                  Request.Exception_Info);

            else
               Call_Send_Exception
                 (Element (All_Server_Interceptors, J).all,
                  Create_Server_Request_Info
                    (null,
                     Request.all'Unchecked_Access,
                     Note.Request_Id,
                     Profile,
                     Send_Exception,
                     True),
                  True,
                  Request.Exception_Info);
            end if;

         else
            if Is_Set (Requests.Sync_With_Server, Request.Req_Flags)
              or else Is_Set (Requests.Sync_With_Target, Request.Req_Flags)
            then
               --  A reply is expected

               Call_Send_Reply
                 (Element (All_Server_Interceptors, J).all,
                  Create_Server_Request_Info
                    (null,
                     Request.all'Unchecked_Access,
                     Note.Request_Id,
                     Profile,
                     Send_Reply,
                     True),
                  False,
                  Request.Exception_Info);
            else
               Call_Send_Other
                 (Element (All_Server_Interceptors, J).all,
                  Create_Server_Request_Info
                    (null,
                     Request.all'Unchecked_Access,
                     Note.Request_Id,
                     Profile,
                     Send_Other,
                     True),
                  True,
                  Request.Exception_Info);
            end if;
         end if;
      end loop;

      Rebuild_Reply_QoS_Parameters (Request.all);
   end Server_Invoke;

   -------------------------------------------
   -- To_PolyORB_ForwardRequest_Members_Any --
   -------------------------------------------

   function To_PolyORB_ForwardRequest_Members_Any
     (Members : PortableInterceptor.ForwardRequest_Members)
      return PolyORB.Any.Any
   is
   begin
      return
        PolyORB.Errors.Helper.To_Any
        (PolyORB.Errors.ForwardRequest_Members'
         (Forward_Reference =>
            PolyORB.Smart_Pointers.Ref
          (CORBA.Object.Internals.To_PolyORB_Ref (Members.Forward))));
   end To_PolyORB_ForwardRequest_Members_Any;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke := Client_Invoke'Access;
      PolyORB.CORBA_P.Interceptors_Hooks.Server_Invoke := Server_Invoke'Access;
      PolyORB.CORBA_P.Interceptors_Hooks.Server_Intermediate :=
        Server_Intermediate'Access;
      PolyORB.CORBA_P.Interceptors_Hooks.POA_Create := POA_Create'Access;

      Create
        (Request_Id_Mutex, "polyorb.corba_p.interceptors.request_id_mutex");
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.corba_p.interceptors",
       Conflicts => Empty,
       Depends   => +"corba.request"
       & "portableserver",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.CORBA_P.Interceptors;
