------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         P O L Y O R B . C O R B A _ P . I N T E R C E P T O R S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.Object;
with PolyORB.Any;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.Requests;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Chained_Lists;

with PortableInterceptor.ClientRequestInfo;
with PortableInterceptor.ClientRequestInfo.Impl;
with PortableInterceptor.ClientRequestInterceptor;
with PortableInterceptor.Helper;
with PortableInterceptor.ORBInitInfo.Impl;

package body PolyORB.CORBA_P.Interceptors is

   --  Client Interceptors

   function "="
     (Left, Right : in PortableInterceptor.ClientRequestInterceptor.Local_Ref)
      return Boolean;

   package ClientRequestInterceptor_Lists is
      new PolyORB.Utils.Chained_Lists
           (PortableInterceptor.ClientRequestInterceptor.Local_Ref);

   All_Client_Interceptors : ClientRequestInterceptor_Lists.List;

   procedure Client_Invoke
     (Self  : in PolyORB.Requests.Request_Access;
      Flags : in PolyORB.Requests.Flags);

   procedure Client_Invoke
     (Self   : in PolyORB.Requests.Request_Access;
      Flags  : in PolyORB.Requests.Flags;
      Target : in CORBA.Object.Ref);

   function Create_Client_Request_Info
     (Request : in PolyORB.Requests.Request_Access;
      Point   : in Client_Interception_Point;
      Target  : in CORBA.Object.Ref)
      return PortableInterceptor.ClientRequestInfo.Local_Ref;

   generic
      with procedure Operation
        (Self : in PortableInterceptor.ClientRequestInterceptor.Local_Ref;
         Info : in PortableInterceptor.ClientRequestInfo.Local_Ref);
   procedure Call_Client_Request_Interceptor_Operation
     (Self     : in     PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Info     : in     PortableInterceptor.ClientRequestInfo.Local_Ref;
      Excp_Inf :    out PolyORB.Any.Any;
      Forward  :    out Boolean);

   --  ORB_Initializers

   function "=" (Left, Right : in PortableInterceptor.ORBInitializer.Local_Ref)
     return Boolean;

   package Initializer_Ref_Lists is
     new PolyORB.Utils.Chained_Lists
          (PortableInterceptor.ORBInitializer.Local_Ref);

   All_Initializer_Refs : Initializer_Ref_Lists.List;

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : in PortableInterceptor.ClientRequestInterceptor.Local_Ref)
      return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ---------
   -- "=" --
   ---------

   function "="
     (Left, Right : in PortableInterceptor.ORBInitializer.Local_Ref)
     return Boolean
   is
   begin
      return CORBA.Object.Is_Equivalent (CORBA.Object.Ref (Left), Right);
   end "=";

   ------------------------------------
   -- Add_Client_Request_Interceptor --
   ------------------------------------

   procedure Add_Client_Request_Interceptor
     (Interceptor : in PortableInterceptor.ClientRequestInterceptor.Local_Ref)
   is
   begin
      ClientRequestInterceptor_Lists.Append
        (All_Client_Interceptors, Interceptor);
   end Add_Client_Request_Interceptor;

   -----------------------------------------------
   -- Call_Client_Request_Interceptor_Operation --
   -----------------------------------------------

   procedure Call_Client_Request_Interceptor_Operation
     (Self     : in     PortableInterceptor.ClientRequestInterceptor.Local_Ref;
      Info     : in     PortableInterceptor.ClientRequestInfo.Local_Ref;
      Excp_Inf :    out PolyORB.Any.Any;
      Forward  :    out Boolean)
   is
      Empty_Any : PolyORB.Any.Any;
   begin
      Operation (Self, Info);
      Forward := False;
      Excp_Inf := Empty_Any;

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
         Forward  := False;

      when E : PortableInterceptor.ForwardRequest =>
         Excp_Inf := PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E);
         Forward  := True;
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

   -------------------
   -- Client_Invoke --
   -------------------

   procedure Client_Invoke
     (Self  : in PolyORB.Requests.Request_Access;
      Flags : in PolyORB.Requests.Flags)
   is
      Target : CORBA.Object.Ref;
   begin
      CORBA.Object.Convert_To_CORBA_Ref (Self.Target, Target);
      Client_Invoke (Self, Flags, Target);
   end Client_Invoke;

   -------------------
   -- Client_Invoke --
   -------------------

   procedure Client_Invoke
     (Self   : in PolyORB.Requests.Request_Access;
      Flags  : in PolyORB.Requests.Flags;
      Target : in CORBA.Object.Ref)
   is
      use ClientRequestInterceptor_Lists;

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

      Index          : Natural := Length (All_Client_Interceptors);
      Exception_Info : PolyORB.Any.Any;
      Forward        : Boolean := False;

   begin
      --  First, call Send_Request on all interceptors.

      for J in 0 .. Index - 1 loop
         Call_Send_Request
           (Element (All_Client_Interceptors, J).all,
            Create_Client_Request_Info (Self, Send_Request, Target),
            Exception_Info,
            Forward);

         --  If got system or ForwardRequest exception then avoid call
         --  Send_Request on other Interceptors.

         if not PolyORB.Any.Is_Empty (Exception_Info) then
            Self.Exception_Info := Exception_Info;
            Index := J;

            exit;
         end if;
      end loop;

      --  Avoid operation invocation if interceptor raise system exception.

      if Index = Length (All_Client_Interceptors) then
         PolyORB.Requests.Invoke (Self, Flags);
      end if;

      for J in reverse 0 .. Index - 1 loop
         if not PolyORB.Any.Is_Empty (Self.Exception_Info) then
            if Forward then
               Call_Receive_Other
                 (Element (All_Client_Interceptors, J).all,
                  Create_Client_Request_Info (Self, Receive_Other, Target),
                  Exception_Info,
                  Forward);

               if not PolyORB.Any.Is_Empty (Exception_Info) then
                  Self.Exception_Info := Exception_Info;
               end if;
            else
               Call_Receive_Exception
                 (Element (All_Client_Interceptors, J).all,
                  Create_Client_Request_Info (Self, Receive_Exception, Target),
                  Exception_Info,
                  Forward);

               if not PolyORB.Any.Is_Empty (Exception_Info) then
                  Self.Exception_Info := Exception_Info;
               end if;
            end if;
         else

            Call_Receive_Reply
              (Element (All_Client_Interceptors, J).all,
               Create_Client_Request_Info (Self, Receive_Reply, Target),
               Exception_Info,
               Forward);

            --  ForwardRequest from Receive_Reply handled as ordinary
            --  exception.

            if Forward then
               PortableInterceptor.Helper.Raise_ForwardRequest_From_Any
                 (Exception_Info);
            elsif not PolyORB.Any.Is_Empty (Exception_Info) then
               Self.Exception_Info := Exception_Info;
            end if;
         end if;
      end loop;

      if Forward then
         --  Reinvocation. Extract object reference from ForwardRequest
         --  exception, change request target, empty Exception_Info and
         --  reinvoke request.

         declare
            Members   : constant PortableInterceptor.ForwardRequest_Members
              := PortableInterceptor.Helper.From_Any
              (CORBA.Internals.To_CORBA_Any (Self.Exception_Info));
            Empty_Any : PolyORB.Any.Any;
         begin
            Self.Target := CORBA.Object.To_PolyORB_Ref (Members.Forward);
            Self.Exception_Info := Empty_Any;
            Client_Invoke (Self, Flags, Target);
         end;
      end if;
   end Client_Invoke;

   --------------------------------
   -- Create_Client_Request_Info --
   --------------------------------

   function Create_Client_Request_Info
     (Request : in PolyORB.Requests.Request_Access;
      Point   : in Client_Interception_Point;
      Target  : in CORBA.Object.Ref)
      return PortableInterceptor.ClientRequestInfo.Local_Ref
   is
      Info_Ptr : constant PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
         := new PortableInterceptor.ClientRequestInfo.Impl.Object;
      Info_Ref : PortableInterceptor.ClientRequestInfo.Local_Ref;

   begin
      PortableInterceptor.ClientRequestInfo.Impl.Init
       (Info_Ptr, Point, Request, Target);

      PortableInterceptor.ClientRequestInfo.Set
        (Info_Ref, PolyORB.Smart_Pointers.Entity_Ptr (Info_Ptr));

      return Info_Ref;
   end Create_Client_Request_Info;

   ------------------------------------------
   -- Is_Client_Request_Interceptor_Exists --
   ------------------------------------------

   function Is_Client_Request_Interceptor_Exists
     (Name : in String)
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

   ------------------------------
   -- Register_ORB_Initializer --
   ------------------------------

   procedure Register_ORB_Initializer
     (Init : in PortableInterceptor.ORBInitializer.Local_Ref)
   is
      use Initializer_Ref_Lists;
   begin
      Append (All_Initializer_Refs, Init);
   end Register_ORB_Initializer;

begin
   PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke := Client_Invoke'Access;
end PolyORB.CORBA_P.Interceptors;
