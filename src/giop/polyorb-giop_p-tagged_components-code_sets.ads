------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2006 Free Software Foundation, Inc.           --
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

   procedure Marshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type);

   procedure Unmarshall_Component_Data
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type;
      Error  : out PolyORB.Errors.Error_Container);

   procedure Release_Contents
     (C : access TC_Code_Sets);

   function Duplicate (C : TC_Code_Sets) return Tagged_Component_Access;

end PolyORB.GIOP_P.Tagged_Components.Code_Sets;
