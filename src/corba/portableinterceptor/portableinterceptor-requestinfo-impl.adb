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
with CORBA.Repository_Root;

with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;

package body PortableInterceptor.RequestInfo.Impl is

   use Dynamic;

   function To_CORBA_ParameterMode (Mode : in PolyORB.Any.Flags)
      return CORBA.Repository_Root.ParameterMode;
   --  Convert PolyORB parameter mode flag to CORBA::ParameterMode.

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Self : access Object)
      return ParameterList
   is
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
                 (CORBA.Internals.To_CORBA_Any (Arg.Argument),
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
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Contexts;

   --------------------
   -- Get_Exceptions --
   --------------------

   function Get_Exceptions
     (Self : access Object)
      return ExceptionList
   is
      use PolyORB.Any.ExceptionList;

      Result : ExceptionList;
   begin
      for J in 1 .. Get_Count (Self.Request.Exc_List) loop
         Append
           (Result,
            CORBA.TypeCode.Internals.To_CORBA_Object
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
      return CORBA.String (Self.Request.Operation);
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
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Operation_Context;

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
      raise PolyORB.Not_Implemented;
      return True;
   end Get_Response_Expected;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result (Self : access Object) return CORBA.Any is
   begin
      return CORBA.Internals.To_CORBA_Any (Self.Request.Result.Argument);
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

--   function Get_Sync_Scope
--     (Self : access Object)
--      return Messaging.SyncScope;
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

   ----------------------------
   -- To_CORBA_ParameterMode --
   ----------------------------

   function To_CORBA_ParameterMode (Mode : in PolyORB.Any.Flags)
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
         raise PolyORB.Not_Implemented;

      end if;
   end To_CORBA_ParameterMode;

end PortableInterceptor.RequestInfo.Impl;
