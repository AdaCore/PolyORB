------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O R T A B L E I N T E R C E P T O R . I O R I N F O . I M P L      --
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

with PolyORB.Annotations;
with PolyORB.CORBA_P.Policy_Management;

package body PortableInterceptor.IORInfo.Impl is

   -----------------------
   -- Add_IOR_Component --
   -----------------------

   procedure Add_IOR_Component
     (Self        : access Object;
      A_Component : in     IOP.TaggedComponent)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (A_Component);
   begin
      raise PolyORB.Not_Implemented;
   end Add_IOR_Component;

   ----------------------------------
   -- Add_IOR_Component_To_Profile --
   ----------------------------------

   procedure Add_IOR_Component_To_Profile
     (Self        : access Object;
      A_Component : in     IOP.TaggedComponent;
      Profile_Id  : in     IOP.ProfileId)
   is
      pragma Unreferenced (Self);
      pragma Unreferenced (A_Component);
      pragma Unreferenced (Profile_Id);
   begin
      raise PolyORB.Not_Implemented;
   end Add_IOR_Component_To_Profile;

--   --------------------------
--   -- Get_Adapter_Template --
--   --------------------------
--
--   function Get_Adapter_Template
--     (Self : access Object)
--      return ObjectReferenceTemplate.Abstract_Value_Ref
--   is
--      Result : ObjectReferenceTemplate.Abstract_Value_Ref;
--   begin
--      raise PolyORB.Not_Implemented;
--      return Result;
--   end Get_Adapter_Template;

--   -------------------------
--   -- Get_Current_Factory --
--   -------------------------
--
--   function Get_Current_Factory
--     (Self : access Object)
--      return ObjectReferenceFactory.Abstract_Value_Ref
--   is
--      Result : ObjectReferenceFactory.Abstract_Value_Ref;
--   begin
--      raise PolyORB.Not_Implemented;
--      return Result;
--   end Get_Current_Factory;

   --------------------------
   -- Get_Effective_Policy --
   --------------------------

   function Get_Effective_Policy
     (Self     : access Object;
      IDL_Type : in     CORBA.PolicyType)
      return CORBA.Policy.Ref
   is
      use PolyORB.CORBA_P.Policy_Management;

      Note : Policy_Manager_Note;
   begin
      if not Is_Registered (IDL_Type) then
         CORBA.Raise_Inv_Policy
          (CORBA.System_Exception_Members'(3, CORBA.Completed_No));
      end if;

      PolyORB.Annotations.Get_Note
       (PolyORB.POA.Notepad_Of (Self.POA).all,
        Note,
        Empty_Policy_Manager_Note);

      return Note.Overrides (IDL_Type);
   end Get_Effective_Policy;

   --------------------
   -- Get_Manager_Id --
   --------------------

   function Get_Manager_Id (Self : access Object) return AdapterManagerId is
      pragma Unreferenced (Self);
      Result : AdapterManagerId;
   begin
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_Manager_Id;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Self : access Object) return AdapterState is
      pragma Unreferenced (Self);
      Result : AdapterState;
   begin
      raise PolyORB.Not_Implemented;
      return Result;
   end Get_State;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : access Object;
      POA  : in     PolyORB.POA.Obj_Adapter_Access)
   is
   begin
      Self.POA := POA;
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
         PortableInterceptor.IORInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

--   -------------------------
--   -- Set_Current_Factory --
--   -------------------------
--
--   procedure Set_Current_Factory
--     (Self : access Object;
--      To   : in     ObjectReferenceFactory.Abstract_Value_Ref)
--   is
--   begin
--      raise PolyORB.Not_Implemented;
--   end Set_Current_Factory;

end PortableInterceptor.IORInfo.Impl;
