------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . O B J E C T _ M A P S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

package body PolyORB.Object_Maps is

   use PolyORB.POA_Types;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : Object_Map_Entry_Access) return Boolean is
   begin
      return Item = null;
   end Is_Null;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In
     (O_Map : Object_Map;
      Item  : PolyORB.Servants.Servant_Access)
     return Boolean
   is
   begin
      return not Is_Null (Get_By_Servant (Object_Map'Class (O_Map), Item));
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In
     (O_Map : Object_Map;
      Item  : PolyORB.POA_Types.Unmarshalled_Oid)
     return Boolean
   is
   begin
      return not Is_Null (Get_By_Id (Object_Map'Class (O_Map), Item));
   end Is_Object_Id_In;

end PolyORB.Object_Maps;
