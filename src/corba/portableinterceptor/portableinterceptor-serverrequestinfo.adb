------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.SERVERREQUESTINFO                   --
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
with PortableInterceptor.ServerRequestInfo.Impl;

package body PortableInterceptor.ServerRequestInfo is

--   -------------------------------
--   -- Add_Reply_Service_Context --
--   -------------------------------
--
--   procedure Add_Reply_Service_Context
--     (Self            : in Local_Ref;
--      Service_Context : in CORBA.IOP.ServiceContext;
--      Replace         : in CORBA.Boolean)
--   is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      PortableInterceptor.ServerRequestInfo.Impl.Add_Reply_Service_Context
--       (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
--         (Entity_Of (Self)),
--        Service_Context,
--        Replace);

   ----------------------
   -- Get_Adapter_Name --
   ----------------------

   function Get_Adapter_Name (Self : in Local_Ref) return AdapterName is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_Adapter_Name
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Adapter_Name;

--   --------------------
--   -- Get_Adapter_Id --
--   --------------------
--
--   function Get_Adapter_Id (Self : in Local_Ref) return CORBA.OctetSeq is
--      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
--   begin
--
--      if CORBA.Object.Is_Nil (Self_Ref) then
--         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
--      end if;
--
--      return
--        PortableInterceptor.ServerRequestInfo.Impl.Get_Adapter_Id
--         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
--           (Entity_Of (Self)));
--   end Get_Adapter_Id;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id (Self : in Local_Ref) return ObjectId is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_Object_Id
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Object_Id;

   ----------------
   -- Get_ORB_Id --
   ----------------

   function Get_ORB_Id (Self : in Local_Ref) return ORBId is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_ORB_Id
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_ORB_Id;

   ---------------------------
   -- Get_Sending_Exception --
   ---------------------------

   function Get_Sending_Exception (Self : in Local_Ref) return CORBA.Any is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_Sending_Exception
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Sending_Exception;

   -------------------
   -- Get_Server_Id --
   -------------------

   function Get_Server_Id (Self : in Local_Ref) return ServerId is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_Server_Id
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Server_Id;

   -----------------------
   -- Get_Server_Policy --
   -----------------------

   function Get_Server_Policy
     (Self   : in Local_Ref;
      A_Type : in CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Get_Server_Policy
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)),
          A_Type);
   end Get_Server_Policy;

   ---------------------------------------
   -- Get_Target_Most_Derived_Interface --
   ---------------------------------------

   function Get_Target_Most_Derived_Interface
     (Self : in Local_Ref)
      return CORBA.RepositoryId
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        Impl.Get_Target_Most_Derived_Interface
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)));
   end Get_Target_Most_Derived_Interface;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (Self : in Local_Ref;
      Id   : in PortableInterceptor.SlotId;
      Data : in CORBA.Any)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ServerRequestInfo.Impl.Set_Slot
       (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
         (Entity_Of (Self)),
        Id,
        Data);
   end Set_Slot;

   -----------------
   -- Target_Is_A --
   -----------------

   function Target_Is_A
     (Self : in Local_Ref;
      Id   : in CORBA.RepositoryId)
      return CORBA.Boolean
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return
        PortableInterceptor.ServerRequestInfo.Impl.Target_Is_A
         (PortableInterceptor.ServerRequestInfo.Impl.Object_Ptr
           (Entity_Of (Self)),
          Id);
   end Target_Is_A;

end PortableInterceptor.ServerRequestInfo;
