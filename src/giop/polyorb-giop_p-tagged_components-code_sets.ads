------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               POLYORB.GIOP_P.TAGGED_COMPONENTS.CODE_SETS                 --
--                                                                          --
--                                 S p e c                                  --
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

--  TAG_CODE_SETS tagged component

with PolyORB.GIOP_P.Code_Sets;

package PolyORB.GIOP_P.Tagged_Components.Code_Sets is

   type Code_Set_Component is record
      Native_Code_Set      : GIOP_P.Code_Sets.Code_Set_Id;
      Conversion_Code_Sets : GIOP_P.Code_Sets.Code_Set_Id_List;
   end record;

   type TC_Code_Sets is new Tagged_Component (Tag_Code_Sets) with record
      For_Char_Data  : Code_Set_Component;
      For_Wchar_Data : Code_Set_Component;
   end record;

   procedure Marshall
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type);

   procedure Unmarshall
     (C      : access TC_Code_Sets;
      Buffer : access Buffer_Type);

   procedure Release_Contents
     (C : access TC_Code_Sets);

end PolyORB.GIOP_P.Tagged_Components.Code_Sets;
