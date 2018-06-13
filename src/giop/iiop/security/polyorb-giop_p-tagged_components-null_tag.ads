------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.TAGGED_COMPONENTS.NULL_TAG                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  Used only as transport_mech in TAG_CSI_SEC_MECH_LIST tagged component.

package PolyORB.GIOP_P.Tagged_Components.Null_Tag is

   type TC_Null_Tag is new Tagged_Component
     (Tag => Tag_NULL_Tag, At_Most_Once => False)
     with null record;

   overriding procedure Marshall_Component_Data
     (C      : access TC_Null_Tag;
      Buffer : access Buffer_Type);

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Null_Tag;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   overriding procedure Release_Contents (C : access TC_Null_Tag);

   overriding function Duplicate
     (C : TC_Null_Tag)
     return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.Null_Tag;
