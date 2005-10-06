------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               PORTABLEINTERCEPTOR.CLIENTREQUESTINFO.IMPL                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

with PolyORB.Any;
with PolyORB.Binding_Data;
with PolyORB.Buffers;
with PolyORB.CORBA_P.Codec_Utils;
with PolyORB.CORBA_P.Interceptors;
with PolyORB.QoS.Service_Contexts;
with PolyORB.References.Binding;
with PolyORB.References.IOR;
with PolyORB.Representations.CDR.Common;
with PolyORB.Request_QoS;
with PolyORB.Types;

package body PortableInterceptor.ClientRequestInfo.Impl is

   use PolyORB.CORBA_P.Codec_Utils;
   use PolyORB.CORBA_P.Interceptors;
   use PolyORB.QoS;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Request_QoS;

   ---------------------------------
   -- Add_Request_Service_Context --
   ---------------------------------

   procedure Add_Request_Service_Context
     (Self            : access Object;
      Service_Context : in     IOP.ServiceContext;
      Replace         : in     CORBA.Boolean)
   is
      use Service_Context_Lists;
      use type Service_Id;

      procedure Free is
        new Ada.Unchecked_Deallocation (Encapsulation, Encapsulation_Access);

      SCP  : QoS_GIOP_Service_Contexts_Parameter_Access;
      Iter : Iterator;
   begin
      if Self.Point /= Send_Request then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      SCP :=
        QoS_GIOP_Service_Contexts_Parameter_Access
         (Extract_Request_Parameter (GIOP_Service_Contexts, Self.Request));

      if SCP = null then
         SCP := new QoS_GIOP_Service_Contexts_Parameter;
         Add_Request_QoS
           (Self.Request,
            GIOP_Service_Contexts,
            QoS_Parameter_Access (SCP));
      end if;

      Iter := First (SCP.Service_Contexts);
      while not Last (Iter) loop
         if Value (Iter).Context_Id
              = Service_Id (Service_Context.Context_Id)
         then
            if not Replace then
               CORBA.Raise_Bad_Inv_Order
                (CORBA.Bad_Inv_Order_Members'
                 (Minor     => 15,
                  Completed => CORBA.Completed_No));
            end if;

            Free (Value (Iter).Context_Data);
            Value (Iter).Context_Data :=
              new Encapsulation'
              (To_Encapsulation
               (CORBA.IDL_Sequences.IDL_SEQUENCE_Octet.Sequence
                (Service_Context.Context_Data)));

            return;
         end if;
         Next (Iter);
      end loop;

      Append
        (SCP.Service_Contexts,
         (Service_Id (Service_Context.Context_Id),
          new Encapsulation'
          (To_Encapsulation
           (CORBA.IDL_Sequences.IDL_SEQUENCE_Octet.Sequence
            (Service_Context.Context_Data)))));
   end Add_Request_Service_Context;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Self : access Object)
      return Dynamic.ParameterList
   is
   begin
      if Self.Point /= Send_Request and Self.Point /= Receive_Reply then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Arguments
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Arguments;

   ------------------
   -- Get_Contexts --
   ------------------

   function Get_Contexts
     (Self : access Object)
      return Dynamic.ContextList
   is
   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Contexts
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Contexts;

   -----------------------------
   -- Get_Effective_Component --
   -----------------------------

   function Get_Effective_Component
     (Self : access Object;
      Id   : in     IOP.ComponentId)
      return IOP.TaggedComponent
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Id);

      Result : IOP.TaggedComponent;
      pragma Warnings (Off, Result);

   begin
      raise Program_Error;
      return Result;
   end Get_Effective_Component;

   ---------------------------
   -- Get_Effective_Profile --
   ---------------------------

   function Get_Effective_Profile
     (Self : access Object)
      return IOP.TaggedProfile
   is
      Profile : PolyORB.Binding_Data.Profile_Access;
      Result  : IOP.TaggedProfile;

   begin
      Profile :=
        PolyORB.References.Binding.Get_Preferred_Profile
        (Self.Request.Target, True);

      declare
         Buffer  : PolyORB.Buffers.Buffer_Access
           := new PolyORB.Buffers.Buffer_Type;
         Success : Boolean;

      begin
         --  Marshall profile with IOR rules

         PolyORB.References.IOR.Marshall_Profile (Buffer, Profile, Success);

         if not Success then
            raise Program_Error;
         end if;

         PolyORB.Buffers.Show (Buffer);
         PolyORB.Buffers.Rewind (Buffer);

         --  Unmarshall profile tag and profile data

         Result.Tag :=
           IOP.ProfileId
           (PolyORB.Types.Unsigned_Long'
            (PolyORB.Representations.CDR.Common.Unmarshall (Buffer)));

         Result.Profile_Data :=
           CORBA.IDL_Sequences.OctetSeq
           (PolyORB.CORBA_P.Codec_Utils.To_Sequence
            (PolyORB.Representations.CDR.Common.Unmarshall (Buffer)));

         PolyORB.Buffers.Release (Buffer);
      end;

      return Result;
   end Get_Effective_Profile;

   --------------------------
   -- Get_Effective_Target --
   --------------------------

   function Get_Effective_Target
     (Self : access Object)
      return CORBA.Object.Ref
   is
      Result : CORBA.Object.Ref;

   begin
      CORBA.Object.Internals.Convert_To_CORBA_Ref
        (Self.Request.Target, Result);

      return Result;
   end Get_Effective_Target;

   --------------------
   -- Get_Exceptions --
   --------------------

   function Get_Exceptions
     (Self : access Object)
      return Dynamic.ExceptionList
   is
   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Exceptions
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Exceptions;

   ---------------------------
   -- Get_Forward_Reference --
   ---------------------------

   function Get_Forward_Reference
     (Self : access Object)
      return CORBA.Object.Ref
   is
   begin
      if Self.Point /= Receive_Other then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Forward_Reference
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Forward_Reference;

   ---------------------------
   -- Get_Operation_Context --
   ---------------------------

   function Get_Operation_Context
     (Self : access Object)
      return Dynamic.RequestContext
   is
   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Operation_Context
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Operation_Context;

   ----------------------------
   -- Get_Received_Exception --
   ----------------------------

   function Get_Received_Exception
     (Self : access Object)
      return CORBA.Any
   is
   begin
      if Self.Point /= Receive_Exception then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return CORBA.Internals.To_CORBA_Any (Self.Request.Exception_Info);
   end Get_Received_Exception;

   -------------------------------
   -- Get_Received_Exception_Id --
   -------------------------------

   function Get_Received_Exception_Id
     (Self : access Object)
      return CORBA.RepositoryId
   is
   begin
      if Self.Point /= Receive_Exception then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        CORBA.RepositoryId
         (PolyORB.Any.TypeCode.Id
           (PolyORB.Any.Get_Type (Self.Request.Exception_Info)));
   end Get_Received_Exception_Id;

   -------------------------------
   -- Get_Reply_Service_Context --
   -------------------------------

   function Get_Reply_Service_Context
     (Self : access Object;
      Id   : in     IOP.ServiceId)
      return IOP.ServiceContext
   is
   begin
      if Self.Point = Send_Request
        or else Self.Point = Send_Poll
      then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return RequestInfo.Impl.Get_Reply_Service_Context
              (RequestInfo.Impl.Object (Self.all)'Access, Id);
   end Get_Reply_Service_Context;

   ----------------------
   -- Get_Reply_Status --
   ----------------------

   function Get_Reply_Status
     (Self : access Object)
      return ReplyStatus
   is
   begin
      if Self.Point = Send_Request
        or else Self.Point = Send_Poll
      then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Reply_Status
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Reply_Status;

   ------------------------
   -- Get_Request_Policy --
   ------------------------

   function Get_Request_Policy
     (Self     : access Object;
      IDL_Type : in     CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      pragma Unreferenced (IDL_Type);
      Result : CORBA.Policy.Ref;

   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise Program_Error;
      return Result;
   end Get_Request_Policy;

   ---------------------------------
   -- Get_Request_Service_Context --
   ---------------------------------

   function Get_Request_Service_Context
     (Self : access Object;
      Id   : in     IOP.ServiceId)
      return IOP.ServiceContext
   is
   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return RequestInfo.Impl.Get_Request_Service_Context
              (RequestInfo.Impl.Object (Self.all)'Access, Id);
   end Get_Request_Service_Context;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result
     (Self : access Object)
      return CORBA.Any
   is
   begin
      if Self.Point /= Receive_Reply then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return RequestInfo.Impl.Get_Result
              (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Result;

   --------------------
   -- Get_Sync_Scope --
   --------------------

   function Get_Sync_Scope
     (Self : access Object)
      return Messaging.SyncScope
   is
   begin
      if Self.Point = Send_Poll then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return RequestInfo.Impl.Get_Sync_Scope
              (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Sync_Scope;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
     (Self : access Object)
      return CORBA.Object.Ref
   is
   begin
      return Self.Target;
   end Get_Target;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self       : access Object;
      Point      : in     Client_Interception_Point;
      Request    : in     PolyORB.Requests.Request_Access;
      Request_Id : in     CORBA.Unsigned_Long;
      Target     : in     CORBA.Object.Ref)
   is
   begin
      RequestInfo.Impl.Init
        (RequestInfo.Impl.Object_Ptr (Self), Request, Request_Id);
      Self.Point   := Point;
      Self.Request := Request;
      Self.Target  := Target;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     String)
      return Boolean
   is
      pragma Unreferenced (Self);

   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.ClientRequestInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           PortableInterceptor.RequestInfo.Repository_Id);
   end Is_A;

--   function Get_Effective_Components
--     (Self : access Object;
--      Id   : in     IOP.ComponentId)
--      return IOP.TaggedComponentSeq;

end PortableInterceptor.ClientRequestInfo.Impl;
