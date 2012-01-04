------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . M I O P _ P . G R O U P S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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
