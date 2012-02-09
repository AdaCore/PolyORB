------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  TAG_CODE_SETS tagged component

with PolyORB.GIOP_P.Code_Sets;

package PolyORB.GIOP_P.Tagged_Components.Code_Sets is

   type Code_Set_Component is record
      Native_Code_Set      : GIOP_P.Code_Sets.Code_Set_Id;
      Conversion_Code_Sets : GIOP_P.Code_Sets.Code_Set_Id_List;
   end record;

   type TC_Code_Sets is new Tagged_Component
     (Tag => Tag_Code_Sets, At_Most_Once => False)
     with record
        For_Char_Data  : Code_Set_Component;
        For_Wchar_Data : Code_Set_Component;
     end record;
   --  Note: the at-most-once semantics of this component is not
   --  specified in the CORBA specification, par. 13.10.2.4, use
   --  default value.

   overriding procedure Marshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type);

   overriding procedure Unmarshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   overriding procedure Release_Contents
     (C : access TC_Code_Sets);

   overriding function Duplicate
     (C : TC_Code_Sets)
     return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.Code_Sets;
