------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 PORTABLEINTERCEPTOR.IORINTERCEPTOR_3_0                   --
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

with PortableInterceptor.IORInfo;
with PortableInterceptor.IORInterceptor;

package PortableInterceptor.IORInterceptor_3_0 is

   type Local_Ref is
      new PortableInterceptor.IORInterceptor.Local_Ref with null record;

   procedure Components_Established
     (Self : in Local_Ref;
      Info : in PortableInterceptor.IORInfo.Local_Ref);

   procedure Adapter_Manager_State_Changed
     (Self  : in Local_Ref;
      Id    : in AdapterManagerId;
      State : in AdapterState);

--   procedure Adapter_State_Changed
--     (Self      : in Local_Ref;
--      Templates : in ObjectReferenceTemplateSeq;
--      State     : in AdapterState);

   --  Repository Ids

   IORInterceptor_3_0_Repository_Root          : constant Standard.String
     := "IDL:PortableInterceptor/IORInterceptor_3_0";

   Repository_Id                               : constant Standard.String
     := IORInterceptor_3_0_Repository_Root & ":1.0";

   Adapter_Manager_State_Changed_Repository_Id : constant Standard.String
     := IORInterceptor_3_0_Repository_Root
          & "/adapter_manager_state_changed:1.0";

   Adapter_State_Changed_Repository_Id         : constant Standard.String
     := IORInterceptor_3_0_Repository_Root & "/adapter_state_changed:1.0";

   Components_Established_Repository_Id        : constant Standard.String
     := IORInterceptor_3_0_Repository_Root & "/components_established:1.0";

end PortableInterceptor.IORInterceptor_3_0;
