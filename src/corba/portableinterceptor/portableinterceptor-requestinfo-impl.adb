------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.REQUESTINFO.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with CORBA.Repository_Root;

with PolyORB.Annotations;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;

with PolyORB.CORBA_P.Codec_Utils;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Slots;
with PolyORB.Errors.Helper;
with PolyORB.QoS.Service_Contexts;
with PolyORB.References;
with PolyORB.Request_QoS;
with PolyORB.Smart_Pointers;

package body PortableInterceptor.RequestInfo.Impl is

   use Dynamic;
   use PolyORB.CORBA_P.Codec_Utils;
   use PolyORB.QoS;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Request_QoS;

   function To_CORBA_ParameterMode (Mode : PolyORB.Any.Flags)
      return CORBA.Repository_Root.ParameterMode;
   --  Convert PolyORB parameter mode flag to CORBA::ParameterMode.

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments (Self : access Object) return ParameterList is
      use PolyORB.Any.NVList.Internals;
      use PolyORB.Any.NVList.Internals.NV_Lists;

      Result : ParameterList;
      Iter   : Iterator := First (List_Of (Self.Request.Args).all);
   begin
      while not Last (Iter) loop
         declare
            Arg : constant Element_Access := Value (Iter);
         begin
            Append
              (Result,
               Parameter'
                 (CORBA.Any (Arg.Argument),
                  To_CORBA_ParameterMode (Arg.Arg_Modes)));
            Next (Iter);
         end;
      end loop;

      return Result;
   end Get_Arguments;

   ------------------
   -- Get_Contexts --
   ------------------

   function Get_Contexts (Self : access Object) return Dynamic.ContextList is
      pragma Unreferenced (Self);

      Result : Dynamic.ContextList;
   begin
      raise Program_Error;
      return Result;
   end Get_Contexts;

   --------------------
   -- Get_Exceptions --
   --------------------

   function Get_Exceptions (Self : access Object) return ExceptionList is
      use PolyORB.Any.ExceptionList;
      Result : ExceptionList;
   begin
      for J in 1 .. Get_Count (Self.Request.Exc_List) loop
         Append (Result, CORBA.TypeCode.Internals.To_CORBA_Object
                           (Item (Self.Request.Exc_List, J)));
      end loop;
      return Result;
   end Get_Exceptions;

   ---------------------------
   -- Get_Forward_Reference --
   ---------------------------

   function Get_Forward_Reference
     (Self : access Object)
      return CORBA.Object.Ref
   is
   begin
      if Get_Reply_Status (Object_Ptr (Self)) /= Location_Forward then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      declare
         Members : constant PolyORB.Errors.ForwardRequest_Members :=
                     PolyORB.Errors.Helper.From_Any
                       (Self.Request.Exception_Info);
         Ref     : PolyORB.References.Ref;
         Result  : CORBA.Object.Ref;
      begin
         PolyORB.References.Set
           (Ref,
            PolyORB.Smart_Pointers.Entity_Of (Members.Forward_Reference));

         CORBA.Object.Internals.Convert_To_CORBA_Ref (Ref, Result);

         return Result;
      end;
   end Get_Forward_Reference;

   -------------------
   -- Get_Operation --
   -------------------

   function Get_Operation (Self : access Object) return CORBA.String is
   begin
      return CORBA.To_CORBA_String (Self.Request.Operation.all);
   end Get_Operation;

   ---------------------------
   -- Get_Operation_Context --
   ---------------------------

   function Get_Operation_Context
     (Self : access Object)
      return Dynamic.RequestContext
   is
      pragma Unreferenced (Self);

      Result : Dynamic.RequestContext;
   begin
      raise Program_Error;
      return Result;
   end Get_Operation_Context;

   -------------------------------
   -- Get_Reply_Service_Context --
   -------------------------------

   function Get_Reply_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext
   is
      use Service_Context_Lists;
      use type Service_Id;

      SCP  : constant QoS_GIOP_Service_Contexts_Parameter_Access
        := QoS_GIOP_Service_Contexts_Parameter_Access
            (Extract_Reply_Parameter (GIOP_Service_Contexts, Self.Request));
      Iter : Iterator;
   begin
      if SCP /= null then
         Iter := First (SCP.Service_Contexts);
         while not Last (Iter) loop
            if Value (Iter).Context_Id = Service_Id (Id) then
               return
                 (Id,
                  IOP.ContextData
                  (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.To_Sequence
                   (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_octet.To_Element_Array
                    (To_Sequence (Value (Iter).Context_Data.all)))));
            end if;
            Next (Iter);
         end loop;
      end if;

      CORBA.Raise_Bad_Param
       (CORBA.Bad_Param_Members'(Minor     => 26,
                                 Completed => CORBA.Completed_No));
   end Get_Reply_Service_Context;

   ----------------------
   -- Get_Reply_Status --
   ----------------------

   function Get_Reply_Status
     (Self : access Object)
      return PortableInterceptor.ReplyStatus
   is
   begin
      if PolyORB.Any.Is_Empty (Self.Request.Exception_Info) then
         return Successful;

      elsif
        PolyORB.CORBA_P.Exceptions.Is_System_Exception
          (Self.Request.Exception_Info)
      then
         return System_Exception;

      elsif
        PolyORB.CORBA_P.Exceptions.Is_Forward_Request
          (Self.Request.Exception_Info)
      then
         return Location_Forward;

      elsif
        PolyORB.CORBA_P.Exceptions.Is_Needs_Addressing_Mode
          (Self.Request.Exception_Info)
      then
         return Transport_Retry;

      else
         return User_Exception;

      end if;
   end Get_Reply_Status;

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Self : access Object)
      return CORBA.Unsigned_Long
   is
   begin
      return Self.Request_Id;
   end Get_Request_Id;

   ---------------------------------
   -- Get_Request_Service_Context --
   ---------------------------------

   function Get_Request_Service_Context
     (Self : access Object;
      Id   : IOP.ServiceId)
      return IOP.ServiceContext
   is
      use Service_Context_Lists;
      use type Service_Id;

      SCP  : constant QoS_GIOP_Service_Contexts_Parameter_Access
        := QoS_GIOP_Service_Contexts_Parameter_Access
            (Extract_Request_Parameter (GIOP_Service_Contexts, Self.Request));
      Iter : Iterator;
   begin
      if SCP /= null then
         Iter := First (SCP.Service_Contexts);
         while not Last (Iter) loop
            if Value (Iter).Context_Id = Service_Id (Id) then
               return
                 (Id,
                  IOP.ContextData
                  (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.To_Sequence
                   (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_octet.To_Element_Array
                    (To_Sequence (Value (Iter).Context_Data.all)))));
            end if;
            Next (Iter);
         end loop;
      end if;

      CORBA.Raise_Bad_Param
       (CORBA.Bad_Param_Members'(Minor     => 26,
                                 Completed => CORBA.Completed_No));
   end Get_Request_Service_Context;

   ---------------------------
   -- Get_Response_Expected --
   ---------------------------

   function Get_Response_Expected
     (Self : access Object)
      return CORBA.Boolean
   is
      use PolyORB.Requests;
      use PolyORB.Requests.Unsigned_Long_Flags;
   begin
      if Is_Set (Sync_None, Self.Request.Req_Flags)
        or else Is_Set (Sync_With_Transport, Self.Request.Req_Flags)
      then
         return False;

      elsif Is_Set (Sync_With_Server, Self.Request.Req_Flags)
        or else Is_Set (Sync_With_Target, Self.Request.Req_Flags)
      then
         return True;

      else
         raise Program_Error;
      end if;
   end Get_Response_Expected;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Self : access Object) return CORBA.Any is
   begin
      return CORBA.Any (Self.Request.Result.Argument);
   end Get_Result;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot (Self : access Object; Id : SlotId) return CORBA.Any is
      use PolyORB.Annotations;
      use PolyORB.CORBA_P.Interceptors_Slots;

      Note : Slots_Note;
   begin
      Get_Note (Self.Request.Notepad, Note, Invalid_Slots_Note);
      return Get_Slot (Note, Id);
   end Get_Slot;

   --------------------
   -- Get_Sync_Scope --
   --------------------

   function Get_Sync_Scope (Self : access Object) return Messaging.SyncScope is
      use PolyORB.Requests;
      use PolyORB.Requests.Unsigned_Long_Flags;
   begin
      if Is_Set (Sync_None, Self.Request.Req_Flags) then
         return Messaging.Sync_None;

      elsif Is_Set (Sync_With_Transport, Self.Request.Req_Flags) then
         return Messaging.Sync_With_Transport;

      elsif Is_Set (Sync_With_Server, Self.Request.Req_Flags) then
         return Messaging.Sync_With_Server;

      elsif Is_Set (Sync_With_Target, Self.Request.Req_Flags) then
         return Messaging.Sync_With_Target;

      else
         raise Program_Error;
      end if;
   end Get_Sync_Scope;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.RequestInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self       : access Object;
      Request    : PolyORB.Requests.Request_Access;
      Request_Id : CORBA.Unsigned_Long)
   is
   begin
      Self.Request    := Request;
      Self.Request_Id := Request_Id;
   end Init;

   ----------------------------
   -- To_CORBA_ParameterMode --
   ----------------------------

   function To_CORBA_ParameterMode (Mode : PolyORB.Any.Flags)
      return CORBA.Repository_Root.ParameterMode
   is
      use type PolyORB.Any.Flags;
   begin
      if Mode = PolyORB.Any.ARG_IN then
         return CORBA.Repository_Root.PARAM_IN;

      elsif Mode = PolyORB.Any.ARG_OUT then
         return CORBA.Repository_Root.PARAM_OUT;

      elsif Mode = PolyORB.Any.ARG_INOUT then
         return CORBA.Repository_Root.PARAM_INOUT;

      else
         --  PolyORB.Any.IN_COPY_VALUE and others
         raise Program_Error;

      end if;
   end To_CORBA_ParameterMode;

end PortableInterceptor.RequestInfo.Impl;
