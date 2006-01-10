------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O R T A B L E I N T E R C E P T O R . O R B I N I T I N F O       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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

with CORBA;

with PortableInterceptor.ORBInitInfo.Impl;

with PolyORB.Exceptions;

package body PortableInterceptor.ORBInitInfo is

   ------------------------------------
   -- Add_Client_Request_Interceptor --
   ------------------------------------

   procedure Add_Client_Request_Interceptor
     (Self        : Local_Ref;
      Interceptor : PortableInterceptor.ClientRequestInterceptor.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ORBInitInfo.Impl.Add_Client_Request_Interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Interceptor);
   end Add_Client_Request_Interceptor;

   -------------------------
   -- Add_IOR_Interceptor --
   -------------------------

   procedure Add_IOR_Interceptor
     (Self        : Local_Ref;
      Interceptor : PortableInterceptor.IORInterceptor.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ORBInitInfo.Impl.Add_IOR_Interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Interceptor);
   end Add_IOR_Interceptor;

   ------------------------------------
   -- Add_Server_Request_Interceptor --
   ------------------------------------

   procedure Add_Server_Request_Interceptor
     (Self        : Local_Ref;
      Interceptor : PortableInterceptor.ServerRequestInterceptor.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ORBInitInfo.Impl.Add_Server_Request_Interceptor
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Interceptor);
   end Add_Server_Request_Interceptor;

   ----------------------
   -- Allocate_Slot_Id --
   ----------------------

   function Allocate_Slot_Id
     (Self : Local_Ref)
      return PortableInterceptor.SlotId
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ORBInitInfo.Impl.Allocate_Slot_Id
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Allocate_Slot_Id;

   -----------------------
   -- Get_Codec_Factory --
   -----------------------

   function Get_Codec_Factory
     (Self : Local_Ref)
      return IOP.CodecFactory.Local_Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ORBInitInfo.Impl.Get_Codec_Factory
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_Codec_Factory;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out DuplicateName_Members) is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   :    out InvalidName_Members)
   is
   begin
      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   ----------------
   -- Get_ORB_Id --
   ----------------

   function Get_ORB_Id
     (Self : Local_Ref)
      return CORBA.String
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ORBInitInfo.Impl.Get_ORB_Id
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)));
   end Get_ORB_Id;

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Self : Local_Ref;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId;
      Obj  : CORBA.Object.Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ORBInitInfo.Impl.Register_Initial_Reference
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Id,
         Obj);
   end Register_Initial_Reference;

   -----------------------------
   -- Register_Policy_Factory --
   -----------------------------

   procedure Register_Policy_Factory
     (Self           : Local_Ref;
      IDL_Type       : CORBA.PolicyType;
      Policy_Factory : PortableInterceptor.PolicyFactory.Local_Ref)
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PortableInterceptor.ORBInitInfo.Impl.Register_Policy_Factory
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         IDL_Type,
         Policy_Factory);
   end Register_Policy_Factory;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Self : Local_Ref;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId)
      return CORBA.Object.Ref
   is
      Self_Ref : constant CORBA.Object.Ref := CORBA.Object.Ref (Self);
   begin

      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      return PortableInterceptor.ORBInitInfo.Impl.Resolve_Initial_References
        (PortableInterceptor.ORBInitInfo.Impl.Object_Ptr (Entity_Of (Self)),
         Id);
   end Resolve_Initial_References;

end PortableInterceptor.ORBInitInfo;
