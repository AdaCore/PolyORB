------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . O B J E C T _ M A P S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
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
