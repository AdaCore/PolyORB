------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . M I O P _ P . G R O U P S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2003 Free Software Foundation, Inc.            --
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

with PolyORB.Objects;
with PolyORB.Types;

package PolyORB.MIOP_P.Groups is

   ----------------
   -- Group_info --
   ----------------

   type Group_Info is record
      Group_Domain_Id : Types.String;
      Object_Group_Id : Types.Unsigned_Long_Long;
      Object_Group_Ref_Version : Types.Unsigned_Long := 0;
   end record;
   type Group_Info_Access is access all Group_Info;
   --  Group_Domain_Id and Object_Relation_Id are bound together
   --  Object_Group_Ref_Version is optional

   function Image
     (G_I : Group_Info)
     return String;
   --  Return a string representing the group

   -----------------------------------------
   -- Object Id <-> Group Info conversion --
   -----------------------------------------

   function To_Object_Id
     (G_I : Group_Info)
     return PolyORB.Objects.Object_Id_Access;

   function To_Group_Info
     (Oid : PolyORB.Objects.Object_Id_Access)
     return Group_Info;

end PolyORB.MIOP_P.Groups;
