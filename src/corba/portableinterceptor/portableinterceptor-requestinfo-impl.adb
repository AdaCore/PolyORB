------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.REQUESTINFO.IMPL                    --
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

with PolyORB;

package body PortableInterceptor.RequestInfo.Impl is

   ---------------------------
   -- Get_Forward_Reference --
   ---------------------------

   function Get_Forward_Reference
     (Self : access Object)
      return CORBA.Object.Ref
   is
      Result : CORBA.Object.Ref;
   begin
      if Get_Reply_Status (Object_Ptr (Self)) /= Location_Forward then
         CORBA.Raise_Bad_Inv_Order
          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
                                        Completed => CORBA.Completed_No));
      end if;

      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Forward_Reference;

   -------------------
   -- Get_Operation --
   -------------------

   function Get_Operation (Self : access Object) return CORBA.String is
   begin
--      return CORBA.String (Self.Request.Operation);
      raise PolyORB.Not_Implemented;
      return CORBA.To_CORBA_String ("");
   end Get_Operation;

   ----------------------
   -- Get_Reply_Status --
   ----------------------

   function Get_Reply_Status
     (Self : access Object)
      return PortableInterceptor.ReplyStatus
   is
   begin
--      case Self.Kind is
--         when Receive_Reply =>
--            return Successful;
--
--         when Receive_Exception =>
--            raise PolyORB.Not_Implemented;
--
--         when Receive_Other =>
--            raise PolyORB.Not_Implemented;
--
--         when Send_Reply =>
--            return Successful;
--
--         when Send_Exception =>
--            raise PolyORB.Not_Implemented;
--
--         when Send_Other =>
--            raise PolyORB.Not_Implemented;
--
--         when Send_Request |
--              Send_Poll |
--              Receive_Request_Service_Contexts |
--              Receive_Request =>
--
--            CORBA.Raise_Bad_Inv_Order
--             (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
--                                           Completed => CORBA.Completed_No));
--      end case;
      raise PolyORB.Not_Implemented;
      return Successful;
   end Get_Reply_Status;

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Self : access Object)
      return CORBA.Unsigned_Long
   is
   begin
      raise PolyORB.Not_Implemented;
      return 0;
   end Get_Request_Id;

   ---------------------------
   -- Get_Response_Expected --
   ---------------------------

   function Get_Response_Expected
     (Self : access Object)
      return CORBA.Boolean
   is
      pragma Unreferenced (Self);
   begin
      --  In current PolyORB implementation all request are synchronous.
      return True;
   end Get_Response_Expected;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Self : access Object) return CORBA.Any is
      pragma Unreferenced (Self);

      Result : CORBA.Any;
   begin
--      if Self.Kind /= Receive_Reply and Self.Kind /= Send_Reply then
--         CORBA.Raise_Bad_Inv_Order
--          (CORBA.Bad_Inv_Order_Members'(Minor     => 14,
--                                        Completed => CORBA.Completed_No));
--      end if;
--
--      return Self.Request.Result.Argument;
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Result;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (Self : access Object;
      Id   : in     SlotId)
      return CORBA.Any
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Id);

      Result : CORBA.Any;
   begin
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Slot;

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
         PortableInterceptor.RequestInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

--   function Get_Request_Id
--     (Self : access Object)
--      return CORBA.Unsigned_Long;
--
--   function Get_Arguments
--     (Self : access Object)
--      return Dynamic.ParameterList;
--
--   function Get_Exceptions
--     (Self : access Object)
--      return Dynamic.ExceptionList;
--
--   function Get_Contexts
--     (Self : access Object)
--      return Dynamic.ContextList;
--
--   function Get_Operation_Context
--     (Self : access Object)
--      return Dynamic.RequestContext;
--
--   function Get_Sync_Scope
--     (Self : access Object)
--      return Messaging.SyncScope;
--
--   function Get_Slot
--     (Self : access Object;
--      Id   : in     PortableInterceptor.SlotId)
--      return CORBA.Any;
--
--   function Get_Request_Service_Context
--     (Self : access Object;
--      Id   : in     IOP.ServiceId)
--      return IOP.ServiceContext;
--
--   function Get_Reply_Service_Context
--     (Self : access Object;
--      Id   : in     IOP.ServiceId)
--      return IOP.ServiceContext;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self    : access Object;
      Request : in     PolyORB.Requests.Request_Access)
   is
   begin
      Self.Request := Request;
   end Init;

end PortableInterceptor.RequestInfo.Impl;
