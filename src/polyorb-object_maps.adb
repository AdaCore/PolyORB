------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  P O L Y O R B . O B J E C T _ M A P S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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

--  $Id$

with PolyORB.Log;
with PolyORB.Types;

package body PolyORB.Object_Maps is

   use Map_Entry_Seqs;

   use PolyORB.Log;
   use PolyORB.POA_Types;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.object_maps");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Item : in Object_Map_Entry_Access)
     return Boolean;
   pragma Inline (Is_Null);

   function Is_Null
     (Item : in Object_Map_Entry_Access)
     return Boolean
   is
   begin
      return (Item = null);
   end Is_Null;

   ---------
   -- Add --
   ---------

   function Add
     (O_Map : access Object_Map;
      Obj   : in     Object_Map_Entry_Access)
     return Integer
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
   begin
      pragma Debug (O ("Add: enter"));

      for J in Elts'Range loop
         if Is_Null (Elts (J)) then
            pragma Debug (O ("Replacing element" & Integer'Image (J)));
            Replace_Element (O_Map.Map, 1 + J - Elts'First, Obj);
            return J;
         end if;
      end loop;

      pragma Debug (O ("Appending element"));
      Append (O_Map.Map, Obj);

      pragma Debug (O ("Add: leave"));
      return Elts'Last + 1;
   end Add;

   procedure Add
     (O_Map : access Object_Map;
      Obj   : in     Object_Map_Entry_Access;
      Index : in     Integer)
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
   begin
      pragma Debug (O ("Add: enter"));

      if not Is_Null (Elts (Index)) then
         raise Program_Error;
      end if;

      Replace_Element (O_Map.Map, 1 + Index - Elts'First, Obj);
      pragma Debug (O ("Add: leave"));
   end Add;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In
     (O_Map : in Object_Map;
      Item  : in PolyORB.Servants.Servant_Access)
     return Boolean is
   begin
      return not Is_Null (Get_By_Servant (O_Map, Item));
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In
     (O_Map : in Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Boolean is
   begin
      return not Is_Null (Get_By_Id (O_Map, Item));
   end Is_Object_Id_In;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id
     (O_Map : in Object_Map;
      Item  : in PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
   begin
      pragma Debug (O ("Get_By_Id: enter"));
      pragma Debug (O ("Object_Map'Size: " & Integer'Image (Elts'Last)));
      pragma Debug (O ("Looking for: "
                       & To_Standard_String (Item.Id)));

      for J in Elts'Range loop

         if not Is_Null (Elts (J)) then
            pragma Debug (O ("Examinating elt: "
                             & To_Standard_String (Elts (J).Oid.Id)));

            if Elts (J).Oid.all = Item then
               pragma Debug (O ("Found !"));
               return Elts (J);
            end if;
         end if;
      end loop;

      pragma Debug (O ("Not Found !"));
      return null;
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant
     (O_Map  : in Object_Map;
      Item   : in PolyORB.Servants.Servant_Access)
     return Object_Map_Entry_Access
   is
      use type PolyORB.Servants.Servant_Access;
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
   begin
      pragma Debug (O ("Get_By_Servant: enter"));
      pragma Debug (O ("Object_Map'Size: " & Integer'Image (Elts'Last)));

      for J in Elts'Range loop
         if not Is_Null (Elts (J)) then
            pragma Debug (O ("Examinating elt: "
                             & To_Standard_String (Elts (J).Oid.Id)));

            if Elts (J).Servant = Item then
               pragma Debug (O ("Found !"));
               return Elts (J);
            end if;
         end if;
      end loop;

      pragma Debug (O ("Not Found !"));
      return null;
   end Get_By_Servant;

   ------------------
   -- Remove_By_Id --
   ------------------

   function Remove_By_Id
     (O_Map : access Object_Map;
      Item  : in     PolyORB.POA_Types.Unmarshalled_Oid)
     return Object_Map_Entry_Access
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
   begin
      pragma Debug (O ("Remove_By_Id: enter"));
      pragma Debug (O ("Looking for: "
                       & To_Standard_String (Item.Id)));

      for J in Elts'Range loop
         if not Is_Null (Elts (J)) then
            pragma Debug (O ("Examinating elt: "
                             & To_Standard_String (Elts (J).Oid.Id)));

            if Elts (J).Oid.all = Item then
               declare
                  Old_Entry : constant Object_Map_Entry_Access
                    := Element_Of (O_Map.Map, J);
               begin
                  pragma Debug (O ("Found !"));
                  Replace_Element (O_Map.Map, J, null);
                  return Old_Entry;
               end;
            end if;
         end if;
      end loop;

      pragma Debug (O ("Not Found !"));
      return null;
   end Remove_By_Id;

end PolyORB.Object_Maps;
