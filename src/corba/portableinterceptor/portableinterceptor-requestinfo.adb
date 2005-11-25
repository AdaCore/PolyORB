------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E I N T E R C E P T O R . R E Q U E S T I N F O       --
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

with PortableInterceptor.RequestInfo.Impl;

package body PortableInterceptor.RequestInfo is

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Self : in Local_Ref)
      return Dynamic.ParameterList
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Arguments
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Arguments;

   ------------------
   -- Get_Contexts --
   ------------------

   function Get_Contexts
     (Self : in Local_Ref)
      return Dynamic.ContextList
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Contexts
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Contexts;

   --------------------
   -- Get_Exceptions --
   --------------------

   function Get_Exceptions
     (Self : in Local_Ref)
      return Dynamic.ExceptionList
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Exceptions
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Exceptions;

   ---------------------------
   -- Get_Forward_Reference --
   ---------------------------

   function Get_Forward_Reference
     (Self : in Local_Ref)
      return CORBA.Object.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Forward_Reference
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Forward_Reference;

   -------------------
   -- Get_Operation --
   -------------------

   function Get_Operation
     (Self : Local_Ref)
      return CORBA.String
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Operation
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Operation;

   ---------------------------
   -- Get_Operation_Context --
   ---------------------------

   function Get_Operation_Context
     (Self : in Local_Ref)
      return Dynamic.RequestContext
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Operation_Context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Operation_Context;

   -------------------------------
   -- Get_Reply_Service_Context --
   -------------------------------

   function Get_Reply_Service_Context
     (Self : in Local_Ref;
      Id   : in IOP.ServiceId)
      return IOP.ServiceContext
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Reply_Service_Context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Id);
   end Get_Reply_Service_Context;

   ----------------------
   -- Get_Reply_Status --
   ----------------------

   function Get_Reply_Status
     (Self : in Local_Ref)
      return ReplyStatus
   is
   begin
      if Is_Nil (Self) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Reply_Status (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Reply_Status;

   --------------------
   -- Get_Request_Id --
   --------------------

   function Get_Request_Id
     (Self : in Local_Ref)
      return CORBA.Unsigned_Long
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Request_Id
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Request_Id;

   ---------------------------------
   -- Get_Request_Service_Context --
   ---------------------------------

   function Get_Request_Service_Context
     (Self : in Local_Ref;
      Id   : in IOP.ServiceId)
      return IOP.ServiceContext
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.RequestInfo.Impl.Get_Request_Service_Context
        (PortableInterceptor.RequestInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Id);
   end Get_Request_Service_Context;

   ---------------------------
   -- Get_Response_Expected --
   ---------------------------

   function Get_Response_Expected
     (Self : in Local_Ref)
      return CORBA.Boolean
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Response_Expected (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Response_Expected;

   ----------------
   -- Get_Result --
   ----------------

   function Get_Result
     (Self : in Local_Ref)
     return CORBA.Any
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Result (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Result;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (Self : in Local_Ref;
      Id   : in SlotId)
      return CORBA.Any
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Slot (Impl.Object_Ptr (Entity_Of (Self)), Id);
   end Get_Slot;

   --------------------
   -- Get_Sync_Scope --
   --------------------

   function Get_Sync_Scope
     (Self : in Local_Ref)
      return Messaging.SyncScope
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return Impl.Get_Sync_Scope (Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Sync_Scope;

end PortableInterceptor.RequestInfo;
