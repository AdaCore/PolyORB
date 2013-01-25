------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  PORTABLEINTERCEPTOR.ORBINITINFO.IMPL                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.CORBA_P.Interceptors;
with PolyORB.CORBA_P.Interceptors_Policies;
with PolyORB.CORBA_P.Interceptors_Slots;

with PolyORB.Initial_References;

with PortableInterceptor.ORBInitInfo.Helper;

package body PortableInterceptor.ORBInitInfo.Impl is

   ------------------------------------
   -- Add_Client_Request_Interceptor --
   ------------------------------------

   procedure Add_Client_Request_Interceptor
     (Self        : access Object;
      Interceptor :
        PortableInterceptor.ClientRequestInterceptor.Local_Ref)
   is
   begin
      if Self.Post_Init_Done then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Name : constant String
           := CORBA.To_Standard_String
                (PortableInterceptor.ClientRequestInterceptor.Get_Name
                  (Interceptor));
      begin
         if Name /= "" then
            if
              PolyORB.CORBA_P.Interceptors.
                Is_Client_Request_Interceptor_Exists
                 (Name)
            then
               Helper.Raise_DuplicateName
                (DuplicateName_Members'(Name => CORBA.To_CORBA_String (Name)));
            end if;
         end if;
      end;

      PolyORB.CORBA_P.Interceptors.Add_Client_Request_Interceptor
       (Interceptor);
   end Add_Client_Request_Interceptor;

   -------------------------
   -- Add_IOR_Interceptor --
   -------------------------

   procedure Add_IOR_Interceptor
     (Self        : access Object;
      Interceptor : PortableInterceptor.IORInterceptor.Local_Ref)
   is
   begin
      if Self.Post_Init_Done then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Name : constant String
           := CORBA.To_Standard_String
                (PortableInterceptor.IORInterceptor.Get_Name (Interceptor));
      begin
         if Name /= "" then
            if
              PolyORB.CORBA_P.Interceptors.Is_IOR_Interceptor_Exists (Name)
            then
               Helper.Raise_DuplicateName
                (DuplicateName_Members'(Name => CORBA.To_CORBA_String (Name)));
            end if;
         end if;
      end;

      PolyORB.CORBA_P.Interceptors.Add_IOR_Interceptor (Interceptor);
   end Add_IOR_Interceptor;

   ------------------------------------
   -- Add_Server_Request_Interceptor --
   ------------------------------------

   procedure Add_Server_Request_Interceptor
     (Self        : access Object;
      Interceptor :
        PortableInterceptor.ServerRequestInterceptor.Local_Ref)
   is
   begin
      if Self.Post_Init_Done then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      declare
         Name : constant String
           := CORBA.To_Standard_String
                (PortableInterceptor.ServerRequestInterceptor.Get_Name
                  (Interceptor));
      begin
         if Name /= "" then
            if
              PolyORB.CORBA_P.Interceptors.
                Is_Server_Request_Interceptor_Exists
                 (Name)
            then
               Helper.Raise_DuplicateName
                (DuplicateName_Members'(Name =>
                                          CORBA.To_CORBA_String (Name)));
            end if;
         end if;
      end;

      PolyORB.CORBA_P.Interceptors.Add_Server_Request_Interceptor
       (Interceptor);
   end Add_Server_Request_Interceptor;

   ----------------------
   -- Allocate_Slot_Id --
   ----------------------

   function Allocate_Slot_Id
     (Self : access Object)
      return PortableInterceptor.SlotId
   is
   begin
      if Self.Post_Init_Done then
         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
      end if;

      return PolyORB.CORBA_P.Interceptors_Slots.Allocate_Slot_Id;
   end Allocate_Slot_Id;

   -------------------
   -- Get_Arguments --
   -------------------

   function Get_Arguments
     (Self : access Object)
     return CORBA.IDL_SEQUENCES.StringSeq
   is
      pragma Unreferenced (Self);
      Result : CORBA.IDL_SEQUENCES.StringSeq;
   begin
      raise Program_Error;
      return Result;
   end Get_Arguments;

   -----------------------
   -- Get_Codec_Factory --
   -----------------------

   function Get_Codec_Factory
     (Self : access Object) return IOP.CodecFactory.Local_Ref
   is
      pragma Unreferenced (Self);
      Result : IOP.CodecFactory.Local_Ref;
   begin
      Result.Set (PolyORB.Initial_References.Resolve_Initial_References
                    ("CodecFactory").Entity_Of);
      return Result;
   end Get_Codec_Factory;

   ----------------
   -- Get_ORB_Id --
   ----------------

   function Get_ORB_Id (Self : access Object) return CORBA.String is
      pragma Unreferenced (Self);

      Result : CORBA.String;

   begin
      raise Program_Error;
      return Result;
   end Get_ORB_Id;

   ----------
   -- Init --
   ----------

   procedure Init (Self : access Object) is
   begin
      Self.Post_Init_Done := False;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id, PortableInterceptor.ORBInitInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

   --------------------
   -- Post_Init_Done --
   --------------------

   procedure Post_Init_Done (Self : access Object) is
   begin
      pragma Assert (not Self.Post_Init_Done);

      Self.Post_Init_Done := True;
      PolyORB.CORBA_P.Interceptors_Slots.ORB_Initializer_Done := True;
   end Post_Init_Done;

   --------------------------------
   -- Register_Initial_Reference --
   --------------------------------

   procedure Register_Initial_Reference
     (Self : access Object;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId;
      Obj  : CORBA.Object.Ref)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Id);
      pragma Unreferenced (Obj);
   begin
--      if Impl.Object_Ptr (Entity_Of (Self)).Initialization_Complete then
--         CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
--      end if;
--
--      --  If string id is empty or id is already registered,
--      --  then raise InvalidName.
--
--      if Id = ""
--        or else not CORBA.Object.Is_Nil
--                      (PCIR.Resolve_Initial_References (To_String (Id)))
--      then
--         Raise_InvalidName ((null record));
--      end if;
--
--      --  If Ref is null, then raise Bad_Param with minor code 27
--
--      if CORBA.Object.Is_Nil (Obj) then
--         Raise_Bad_Param (Bad_Param_Members'(Minor     => 27,
--                                             Completed => Completed_No));
--      end if;
--
--      PCIR.Register_Initial_Reference (To_String (Id), Obj);

      raise Program_Error;
   end Register_Initial_Reference;

   -----------------------------
   -- Register_Policy_Factory --
   -----------------------------

   procedure Register_Policy_Factory
     (Self           : access Object;
      IDL_Type       : CORBA.PolicyType;
      Policy_Factory : PortableInterceptor.PolicyFactory.Local_Ref)
   is
      pragma Unreferenced (Self);

   begin
      PolyORB.CORBA_P.Interceptors_Policies.Register_Policy_Factory
        (IDL_Type,
         Policy_Factory);
   end Register_Policy_Factory;

   --------------------------------
   -- Resolve_Initial_References --
   --------------------------------

   function Resolve_Initial_References
     (Self : access Object;
      Id   : PortableInterceptor.ORBInitInfo.ObjectId)
      return CORBA.Object.Ref
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (Id);
--      Result : CORBA.Object.Ref
--        := PolyORB.CORBA_P.Initial_References.Resolve_Initial_References
--             (To_Standard_String (Id));

      Result : CORBA.Object.Ref;
   begin
--      if Impl.Object_Ptr (Entity_Of (Self)).Initialization_Complete then
--         Raise_Object_Not_Exist (Default_Sys_Member);
--      end if;
--
--      if CORBA.Object.Is_Nil (Result) then
--         Raise_InvalidName ((null record));
--      end if;
--
--      return Result;

      raise Program_Error;
      return Result;
   end Resolve_Initial_References;

end PortableInterceptor.ORBInitInfo.Impl;
