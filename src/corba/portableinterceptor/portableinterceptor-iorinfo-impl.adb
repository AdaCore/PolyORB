------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O R T A B L E I N T E R C E P T O R . I O R I N F O . I M P L      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2010, Free Software Foundation, Inc.          --
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

with Ada.Streams;

with PolyORB.Annotations;
with PolyORB.CORBA_P.Codec_Utils;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.QoS.Tagged_Components;

package body PortableInterceptor.IORInfo.Impl is

   -----------------------
   -- Add_IOR_Component --
   -----------------------

   procedure Add_IOR_Component
     (Self        : access Object;
      A_Component :        IOP.TaggedComponent)
   is
      use PolyORB.Obj_Adapter_QoS;
      use PolyORB.QoS;
      use PolyORB.QoS.Tagged_Components;
      use PolyORB.QoS.Tagged_Components.GIOP_Tagged_Component_Lists;

      QoS : QoS_GIOP_Tagged_Components_Parameter_Access
        := QoS_GIOP_Tagged_Components_Parameter_Access
        (Get_Object_Adapter_QoS (Self.POA, GIOP_Tagged_Components));

   begin
      if QoS = null then
         QoS := new QoS_GIOP_Tagged_Components_Parameter;
         Set_Object_Adapter_QoS
           (Self.POA, GIOP_Tagged_Components, QoS_Parameter_Access (QoS));
      end if;

      Append
        (QoS.Components,
         (Component_Id (A_Component.Tag),
          new Ada.Streams.Stream_Element_Array'
          (PolyORB.CORBA_P.Codec_Utils.To_Encapsulation
           (CORBA.IDL_SEQUENCES.IDL_SEQUENCE_Octet.Sequence
            (A_Component.Component_Data)))));
   end Add_IOR_Component;

   ----------------------------------
   -- Add_IOR_Component_To_Profile --
   ----------------------------------

   procedure Add_IOR_Component_To_Profile
     (Self        : access Object;
      A_Component : IOP.TaggedComponent;
      Profile_Id  : IOP.ProfileId)
   is
      use type IOP.ProfileId;

   begin
      if Profile_Id /= IOP.Tag_Internet_IOP then
         CORBA.Raise_Bad_Param
           (CORBA.System_Exception_Members'
            (CORBA.IDL_Exception_Members with
               Minor  => 29,
               Completed => CORBA.Completed_No));
      end if;

      Add_IOR_Component (Self, A_Component);
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
--      raise Program_Error;
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
--      raise Program_Error;
--      return Result;
--   end Get_Current_Factory;

   --------------------------
   -- Get_Effective_Policy --
   --------------------------

   function Get_Effective_Policy
     (Self     : access Object;
      IDL_Type : CORBA.PolicyType)
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
      raise Program_Error;
      return Result;
   end Get_Manager_Id;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Self : access Object) return AdapterState is
      pragma Unreferenced (Self);
      Result : AdapterState;
   begin
      raise Program_Error;
      return Result;
   end Get_State;

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : access Object;
      POA  : PolyORB.POA.Obj_Adapter_Access)
   is
   begin
      Self.POA := POA;
   end Init;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : not null access Object;
      Logical_Type_Id : Standard.String) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id, PortableInterceptor.IORInfo.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

--   -------------------------
--   -- Set_Current_Factory --
--   -------------------------
--
--   procedure Set_Current_Factory
--     (Self : access Object;
--      To   : ObjectReferenceFactory.Abstract_Value_Ref)
--   is
--   begin
--      raise Program_Error;
--   end Set_Current_Factory;

end PortableInterceptor.IORInfo.Impl;
