------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . O B J E C T _ M A P                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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

with Sequences;

package body PolyORB.Object_Map is

   use Object_Map_Entry_Seqs;
   use Sequences;

   ---------
   -- Add --
   ---------

   function Add (O_Map : access Object_Map;
                 Obj   : in     Map_Entry)
                return Integer
   is
      Elts  : constant Element_Array := To_Element_Array (O_Map.Map);
      Index : Integer := Elts'Last + 1;
   begin
      for I in Elts'Range loop
         if Is_Null (Elts (I)) = True then
            Replace_Element (O_Map.Map, 1 + I - Elts'First, Obj);
            Index := I;
            return Index;
         end if;
      end loop;

      if Index > Elts'Last then
         Append (O_Map.Map, Obj);
      end if;

      return Index;
   end Add;

   ----------------------
   -- Replace_By_Index --
   ----------------------

   procedure Replace_By_Index (O_Map : access Object_Map;
                               Obj   : in     Map_Entry;
                               Index : in     Integer)
   is

   begin
      Replace_Element (O_Map.Map, Index, Obj);
   exception
      when Index_Error =>
         raise Index_Out_Of_Bounds;
   end Replace_By_Index;

   -------------------
   -- Is_Servant_In --
   -------------------

   function Is_Servant_In (O_Map  : in Object_Map;
                           Item   : Servant)
                          return Boolean
   is
      An_Entry : Map_Entry;
   begin
      An_Entry := Get_By_Servant (O_Map, Item);
      return not Is_Null (An_Entry);
   end Is_Servant_In;

   ---------------------
   -- Is_Object_Id_In --
   ---------------------

   function Is_Object_Id_In (O_Map  : in Object_Map;
                             Item   : Object_Id)
                            return Boolean
   is
      An_Entry : Map_Entry;
   begin
      An_Entry := Get_By_Id (O_Map, Item);
      return not Is_Null (An_Entry);
   end Is_Object_Id_In;

   ---------------
   -- Get_By_Id --
   ---------------

   function Get_By_Id (O_Map  : in Object_Map;
                       Item   : in Object_Id)
                      return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Object_Id_Equal (An_Entry, Item) = True then
               return An_Entry;
            end if;
         end if;
      end loop;
      return Null_Entry;
   end Get_By_Id;

   --------------------
   -- Get_By_Servant --
   --------------------

   function Get_By_Servant (O_Map  : in Object_Map;
                            Item   : in Servant)
                           return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Servant_Equal (An_Entry, Item) = True then
               return An_Entry;
            end if;
         end if;
      end loop;
      return Null_Entry;
   end Get_By_Servant;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (O_Map : in Object_Map;
                          Index : in Integer)
                         return Map_Entry
   is
      An_Entry : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;
      An_Entry := Element_Of (O_Map.Map, Index);
      return An_Entry;
   exception
      when Index_Error =>
         raise Index_Out_Of_Bounds;
   end Get_By_Index;

   ------------
   -- Remove --
   ------------

   function Remove (O_Map : access Object_Map;
                    Item  : in     Object_Id)
                   return Map_Entry
   is
      An_Entry  : Map_Entry;
      To_Remove : Map_Entry := Null_Entry;
      Index     : Natural;
   begin

      if Length (O_Map.Map) = 0 then
         return To_Remove;
      end if;

      for I in 1 .. Length (O_Map.Map) loop
         An_Entry := Element_Of (O_Map.Map, I);
         if Is_Null (An_Entry) = False then
            if Is_Object_Id_Equal (An_Entry, Item) = True then
               To_Remove := An_Entry;
               Index := I;
               exit;
            end if;
         end if;
      end loop;

      if Is_Null (To_Remove) = False then
         Replace_Element (O_Map.Map, Index, Null_Entry);
      end if;

      return To_Remove;
   end Remove;

   ---------------------
   -- Remove_By_Index --
   ---------------------

   function Remove_By_Index (O_Map : access Object_Map;
                             Index : in     Integer)
                            return Map_Entry
   is
      To_Remove : Map_Entry;
   begin
      if Length (O_Map.Map) = 0 then
         return Null_Entry;
      end if;

      To_Remove := Element_Of (O_Map.Map, Index);
      if Is_Null (To_Remove) = False then
         Replace_Element (O_Map.Map, Index, Null_Entry);
      end if;
      return To_Remove;
   exception
      when Index_Error =>
         raise Index_Out_Of_Bounds;
   end Remove_By_Index;

end PolyORB.Object_Map;
