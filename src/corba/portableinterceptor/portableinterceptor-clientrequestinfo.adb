------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.CLIENTREQUESTINFO                   --
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

--  $Id$

with PortableInterceptor.ClientRequestInfo.Impl;

package body PortableInterceptor.ClientRequestInfo is

--   ---------------------------------
--   -- Add_Request_Service_Context --
--   ---------------------------------
--
--   procedure Add_Request_Service_Context
--     (Self            : in Local_Ref;
--      Service_Context : in IOP.ServiceContext;
--      Replace         : in CORBA.Boolean)
--   is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      PortableInterceptor.ClientRequestInfo.Impl.Add_Request_Service_Context
--       (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
--         (Entity_Of (Self)),
--        Service_Context,
--        Replace);
--   end Get_Effective_Component;

--   -----------------------------
--   -- Get_Effective_Component --
--   -----------------------------
--
--   function Get_Effective_Component
--     (Self : in Local_Ref;
--      Id   : in IOP.ComponentId)
--      return IOP.TaggedComponent
--   is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      return
--        PortableInterceptor.ClientRequestInfo.Impl.Get_Effective_Component
--         (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
--           (Entity_Of (Self)),
--          Id);
--   end Get_Effective_Component;

--   ------------------------------
--   -- Get_Effective_Components --
--   ------------------------------
--
--   function Get_Effective_Components
--     (Self : in Local_Ref;
--      Id   : in IOP.ComponentId)
--      return IOP.TaggedComponentSeq
--   is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      return
--        PortableInterceptor.ClientRequestInfo.Impl.Get_Effective_Components
--         (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
--           (Entity_Of (Self)),
--          Id);
--   end Get_Effective_Components;

--   ---------------------------
--   -- Get_Effective_Profile --
--   ---------------------------
--
--   function Get_Effective_Profile
--     (Self : in Local_Ref)
--      return IOP.TaggedProfile
--   is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      return PortableInterceptor.ClientRequestInfo.Impl.Get_Effective_Profile
--        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
--          (Entity_Of (Self)));
--   end Get_Effective_Profile;

   --------------------------
   -- Get_Effective_Target --
   --------------------------

   function Get_Effective_Target
     (Self : in Local_Ref)
      return CORBA.Object.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ClientRequestInfo.Impl.Get_Effective_Target
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
          (Entity_Of (Self)));
   end Get_Effective_Target;

   ----------------------------
   -- Get_Received_Exception --
   ----------------------------

   function Get_Received_Exception
     (Self : in Local_Ref)
      return CORBA.Any
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ClientRequestInfo.Impl.Get_Received_Exception
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
          (Entity_Of (Self)));
   end Get_Received_Exception;

   -------------------------------
   -- Get_Received_Exception_Id --
   -------------------------------

   function Get_Received_Exception_Id
     (Self : in Local_Ref)
      return CORBA.RepositoryId
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ClientRequestInfo.Impl.Get_Received_Exception_Id
         (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Received_Exception_Id;

   ------------------------
   -- Get_Request_Policy --
   ------------------------

   function Get_Request_Policy
     (Self     : in Local_Ref;
      IDL_Type : in CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ClientRequestInfo.Impl.Get_Request_Policy
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
          (Entity_Of (Self)),
         IDL_Type);
   end Get_Request_Policy;

   ----------------
   -- Get_Target --
   ----------------

   function Get_Target
     (Self : in Local_Ref)
      return CORBA.Object.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ClientRequestInfo.Impl.Get_Target
        (PortableInterceptor.ClientRequestInfo.Impl.Object_Ptr
          (Entity_Of (Self)));
   end Get_Target;

end PortableInterceptor.ClientRequestInfo;
