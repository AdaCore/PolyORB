------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O R T A B L E I N T E R C E P T O R . I O R I N F O           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitely  --
-- nor implicitely specified by the CORBA Specification defined by the OMG. --
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

with CORBA.Policy;
with IOP;
--  with PortableInterceptor.ObjectReferenceFactory;
--  with PortableInterceptor.ObjectReferenceTemplate;

package PortableInterceptor.IORInfo is

   type Local_Ref is new CORBA.Object.Ref with null record;

   function Get_Effective_Policy
     (Self     : in Local_Ref;
      IDL_Type : in CORBA.PolicyType)
      return CORBA.Policy.Ref;

   procedure Add_IOR_Component
     (Self        : in Local_Ref;
      A_Component : in IOP.TaggedComponent);

   procedure Add_IOR_Component_To_Profile
     (Self        : in Local_Ref;
      A_Component : in IOP.TaggedComponent;
      Profile_Id  : in IOP.ProfileId);

   function Get_Manager_Id (Self : in Local_Ref) return AdapterManagerId;

   function Get_State (Self : in Local_Ref) return AdapterState;

--   function Get_Adapter_Template
--     (Self : in Local_Ref)
--      return ObjectReferenceTemplate.Abstract_Value_Ref;
--
--   function Get_Current_Factory
--     (Self : in Local_Ref)
--      return ObjectReferenceFactory.Abstract_Value_Ref;
--
--   procedure Set_Current_Factory
--     (Self : in Local_Ref;
--      To   : in ObjectReferenceFactory.Abstract_Value_Ref);

   --  Repository Ids

   Repository_Id                              : constant Standard.String
     := "IDL:omg.org/PortableInterceptor/IORInfo:1.0";

   Adapter_Template_Repository_Id             : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/adapter_template:1.0";

   Add_IOR_Component_Repository_Id            : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/add_ior_component:1.0";

   Add_IOR_Component_To_Profile_Repository_Id : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/add_ior_component_to_profile:1.0";

   Current_Factory_Repository_Id              : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/current_factory:1.0";

   Get_Effective_Policy_Repository_Id         : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/get_effective_policy:1.0";

   Manager_Id_Repository_Id                   : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/manager_id:1.0";

   State_Repository_Id                        : constant Standard.String
     := "IDL:PortableInterceptor/IORInfo/state:1.0";

end PortableInterceptor.IORInfo;
