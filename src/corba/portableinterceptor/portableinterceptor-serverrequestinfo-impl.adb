------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               PORTABLEINTERCEPTOR.SERVERREQUESTINFO.IMPL                 --
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

with CORBA;
with PortableInterceptor.RequestInfo;

with PolyORB.CORBA_P.Interceptors; use PolyORB.CORBA_P.Interceptors;

package body PortableInterceptor.ServerRequestInfo.Impl is

--   -------------------------------
--   -- Add_Reply_Service_Context --
--   -------------------------------
--
--   procedure Add_Reply_Service_Context
--     (Self            : access Object;
--      Service_Context : in     CORBA.IOP.ServiceContext;
--      Replace         : in     CORBA.Boolean);

--   --------------------
--   -- Get_Adapter_Id --
--   --------------------
--
--   function Get_Adapter_Id (Self : access Object) return CORBA.OctetSeq;

--   ----------------------
--   -- Get_Adapter_Name --
--   ----------------------
--
--   function Get_Adapter_Name (Self : access Object) return AdapterName;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Self : access Object)
      return Dynamic.ParameterList
   is
   begin
      if Self.Point /= Receive_Request and Self.Point /= Send_Reply then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      --  If Receive_Request interception point called from Set_Exception
      --  then the arguments is not available and No_Resource (with minor
      --  code 1) raised. If Set_Exception called after Arguments then
      --  interception point called only once from Arguments.
      if not Self.Args_Present then
         CORBA.Raise_No_Resources
          (CORBA.No_Resources_Members'(Minor     => 1,
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
      if Self.Point = Receive_Request_Service_Contexts then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Contexts
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Contexts;

   --------------------
   -- Get_Exceptions --
   --------------------

   function Get_Exceptions
     (Self : access Object)
      return Dynamic.ExceptionList
   is
   begin
      if Self.Point = Receive_Request_Service_Contexts then
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
      if Self.Point /= Send_Other then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Forward_Reference
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Forward_Reference;

--   -------------------
--   -- Get_Object_Id --
--   -------------------
--
--   function Get_Object_Id (Self : access Object) return ObjectId;

   ---------------------------
   -- Get_Operation_Context --
   ---------------------------

   function Get_Operation_Context
     (Self : access Object)
      return Dynamic.RequestContext
   is
   begin
      if Self.Point /= Receive_Request and Self.Point /= Send_Reply then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Operation_Context
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Operation_Context;

   ----------------
   -- Get_ORB_Id --
   ----------------

   function Get_ORB_Id (Self : access Object) return ORBId is
      Result : ORBId;
   begin
      if Self.Point = Receive_Request_Service_Contexts then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise PolyORB.Not_Implemented;
      return Result;
   end Get_ORB_Id;

--   -------------------------------
--   -- Get_Reply_Service_Context --
--   -------------------------------
--
--   function Get_Reply_Service_Context
--     (Self : access Object;
--      id : in IOP.ServiceId)
--      return IOP.ServiceContext;

   ----------------------
   -- Get_Reply_Status --
   ----------------------

   function Get_Reply_Status (Self : access Object) return ReplyStatus is
   begin
      if Self.Point = Receive_Request_Service_Contexts
        or else Self.Point = Receive_Request
      then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Reply_Status
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Reply_Status;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Self : access Object) return CORBA.Any is
   begin
      if Self.Point /= Send_Reply then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return
        RequestInfo.Impl.Get_Result
         (RequestInfo.Impl.Object (Self.all)'Access);
   end Get_Result;

   ---------------------------
   -- Get_Sending_Exception --
   ---------------------------

   function Get_Sending_Exception (Self : access Object) return CORBA.Any is
   begin
      if Self.Point /= Send_Exception then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      return CORBA.Internals.To_CORBA_Any (Self.Request.Exception_Info);
   end Get_Sending_Exception;

   -------------------
   -- Get_Server_Id --
   -------------------

   function Get_Server_Id (Self : access Object) return ServerId is
      Result : ServerId;
   begin
      if Self.Point = Receive_Request_Service_Contexts then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Server_Id;

   -----------------------
   -- Get_Server_Policy --
   -----------------------

   function Get_Server_Policy
     (Self   : access Object;
      A_Type : in     CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (A_Type);

      Result : CORBA.Policy.Ref;
   begin
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Server_Policy;

   ---------------------------------------
   -- Get_Target_Most_Derived_Interface --
   ---------------------------------------

   function Get_Target_Most_Derived_Interface
     (Self : access Object)
      return CORBA.RepositoryId
   is
      Result : CORBA.RepositoryId;
   begin
      if Self.Point /= Receive_Request then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Target_Most_Derived_Interface;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self         : access Object;
      Point        : in     Server_Interception_Point;
      Request      : in     PolyORB.Requests.Request_Access;
      Args_Present : in     Boolean)
   is
   begin
      RequestInfo.Impl.Init
       (RequestInfo.Impl.Object (Self.all)'Access, Request);
      Self.Point        := Point;
      Self.Request      := Request;
      Self.Args_Present := Args_Present;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : in     Standard.String)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         PortableInterceptor.ServerRequestInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else CORBA.Is_Equivalent
           (Logical_Type_Id,
         PortableInterceptor.RequestInfo.Repository_Id);
   end Is_A;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Self : access Object;
      Id   : in     PortableInterceptor.SlotId;
      Data : in     CORBA.Any)
   is
   begin
      raise PolyORB.Not_Implemented;
   end Set_Slot;

   -----------------
   -- Target_Is_A --
   -----------------

   function Target_Is_A
     (Self : access Object;
      Id   : in     CORBA.RepositoryId)
      return CORBA.Boolean
   is
      pragma Unreferenced (Id);

      Result : constant CORBA.Boolean := False;
   begin
      if Self.Point /= Receive_Request then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise PolyORB.Not_Implemented;
      return Result;
   end Target_Is_A;

end PortableInterceptor.ServerRequestInfo.Impl;
