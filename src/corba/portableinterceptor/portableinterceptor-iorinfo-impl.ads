------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O R T A B L E I N T E R C E P T O R . I O R I N F O . I M P L      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.Local;
with PolyORB.POA;

package PortableInterceptor.IORInfo.Impl is

   type Object is new CORBA.Local.Object with private;

   type Object_Ptr is access all Object'Class;

   function Get_Effective_Policy
     (Self     : access Object;
      IDL_Type : CORBA.PolicyType)
      return CORBA.Policy.Ref;

   procedure Add_IOR_Component
     (Self        : access Object;
      A_Component : IOP.TaggedComponent);

   procedure Add_IOR_Component_To_Profile
     (Self        : access Object;
      A_Component : IOP.TaggedComponent;
      Profile_Id  : IOP.ProfileId);

   function Get_Manager_Id (Self : access Object) return AdapterManagerId;

   function Get_State (Self : access Object) return AdapterState;

--   function Get_Adapter_Template
--     (Self : access Object)
--      return ObjectReferenceTemplate.Abstract_Value_Ref;
--
--   function Get_Current_Factory
--     (Self : access Object)
--      return ObjectReferenceFactory.Abstract_Value_Ref;
--
--   procedure Set_Current_Factory
--     (Self : access Object;
--      To   : ObjectReferenceFactory.Abstract_Value_Ref);

   procedure Init
     (Self : access Object;
      POA  : PolyORB.POA.Obj_Adapter_Access);
   --  Internal subprogram for initial initialization.

private

   type Object is new CORBA.Local.Object with record
      POA : PolyORB.POA.Obj_Adapter_Access;
   end record;

   function Is_A
     (Self            : access Object;
      Logical_Type_Id : Standard.String)
      return Boolean;

end PortableInterceptor.IORInfo.Impl;
