------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O B J E C T _ M A P S . S E Q               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

package body PolyORB.Object_Maps.Seq is

   use Active_Object_Map;
   use PolyORB.POA_Types;

   ----------------------
   -- Is_Servant_Equal --
   ----------------------

   function Is_Servant_Equal (Item : in Object_Map_Entry_Access;
                              To   : in PolyORB.POA_Types.Servant_Access)
                             return Boolean
   is
   begin
      return (Item.Servant.all = To.all);
   end Is_Servant_Equal;

   ------------------------
   -- Is_Object_Id_Equal --
   ------------------------

   function Is_Object_Id_Equal
     (Item : in Object_Map_Entry_Access;
      To   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
     return Boolean
   is
   begin
      return (Item.Oid.all = To.all);
   end Is_Object_Id_Equal;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Item : in Object_Map_Entry_Access)
                    return Boolean
   is
   begin
      return (Item = null);
   end Is_Null;

   ----------------
   -- Null_Entry --
   ----------------

   function Null_Entry return Object_Map_Entry_Access
   is
   begin
      return null;
   end Null_Entry;

   -------------
   -- New_Map --
   -------------

   function New_Map return Seq_Object_Map_Access
   is
      Map : Object_Map_Access;
   begin
      Map := new Seq_Object_Map;
      Seq_Object_Map_Access (Map).Map := new Active_Object_Map.Object_Map;
      return Seq_Object_Map_Access (Map);
   end New_Map;

   --------------
   -- Free_Map --
   --------------

   procedure Free_Map (S_Map : in out Seq_Object_Map_Access)
   is
      Map :  Active_Object_Map.Object_Map_Access := S_Map.Map;
   begin
      Free (Map);
      Free (S_Map);
   end Free_Map;

   ---------
   -- Add --
   ---------

   function Add (O_Map : access Seq_Object_Map;
                 Obj   : in     Object_Map_Entry_Access)
                return Integer
   is
   begin
      return Active_Object_Map.Add (O_Map.Map.all'Access, Obj);
   end Add;

   ----------------------
   -- Replace_By_Index --
   ----------------------

   procedure Replace_By_Index (O_Map : access Seq_Object_Map;
                               Obj   : in     Object_Map_Entry_Access;
                               Index : in     Integer)
   is
   begin
      Active_Object_Map.Replace_By_Index (O_Map.Map.all'Access, Obj, Index);
   end Replace_By_Index;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In (O_Map : in Seq_Object_Map;
                           Item  : in PolyORB.POA_Types.Servant_Access)
                          return Boolean
   is
   begin
      return Active_Object_Map.Is_Servant_In (O_Map.Map.all, Item);
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In
     (O_Map  : in Seq_Object_Map;
      Item   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
     return Boolean
   is
   begin
      return Active_Object_Map.Is_Object_Id_In (O_Map.Map.all, Item);
   end Is_Object_Id_In;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id (O_Map  : in Seq_Object_Map;
                       Item   : in PolyORB.POA_Types.Unmarshalled_Oid_Access)
                      return Object_Map_Entry_Access
   is
      An_Entry : Object_Map_Entry_Access;
   begin
      An_Entry := Active_Object_Map.Get_By_Id (O_Map.Map.all, Item);
      if Is_Null (An_Entry) then
         return null;
      end if;
      return An_Entry;
   exception
      when Index_Out_Of_Bounds =>
         return null;
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant (O_Map  : in Seq_Object_Map;
                            Item   : in PolyORB.POA_Types.Servant_Access)
                           return Object_Map_Entry_Access
   is
      An_Entry : Object_Map_Entry_Access;
   begin
      An_Entry := Active_Object_Map.Get_By_Servant (O_Map.Map.all, Item);
      if Is_Null (An_Entry) then
         return null;
      end if;
      return An_Entry;
   exception
      when Index_Out_Of_Bounds =>
         return null;
   end Get_By_Servant;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (O_Map : in Seq_Object_Map;
                          Index : in Integer)
                         return Object_Map_Entry_Access
   is
      An_Entry : Object_Map_Entry_Access;
   begin
      An_Entry := Active_Object_Map.Get_By_Index (O_Map.Map.all, Index);
      if Is_Null (An_Entry) then
         return null;
      end if;
      return An_Entry;
   exception
      when Index_Out_Of_Bounds =>
         return null;
   end Get_By_Index;

   ------------
   -- Remove --
   ------------

   function Remove (O_Map : access Seq_Object_Map;
                    Item  : in     PolyORB.POA_Types.Unmarshalled_Oid_Access)
                   return Object_Map_Entry_Access
   is
   begin
      return Active_Object_Map.Remove (O_Map.Map.all'Access, Item);
   end Remove;

   ---------------------
   -- Remove_By_Index --
   ---------------------

   function Remove_By_Index (O_Map : access Seq_Object_Map;
                             Index : in     Integer)
                            return Object_Map_Entry_Access
   is
   begin
      return Active_Object_Map.Remove_By_Index (O_Map.Map.all'Access, Index);
   end Remove_By_Index;

end PolyORB.Object_Maps.Seq;
