------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . A N Y . E X C E P T I O N L I S T             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2002 Free Software Fundation                --
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

with Ada.Exceptions;

with PolyORB.Sequences.Unbounded.Search;

with PolyORB.Log;
with PolyORB.Smart_Pointers;

package body PolyORB.Any.ExceptionList is

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.any.exceptionlist");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Obj : in out Object) is
   begin
      pragma Debug (O ("Finalize : enter"));
      pragma Debug (O ("length" &
                       Integer'Image (Exception_Sequences.Length (Obj.List))));
      Exception_Sequences.Delete
        (Obj.List, 1, Exception_Sequences.Length (Obj.List));
      pragma Debug (O ("Finalize : leave"));
   exception
      when E : others =>
         pragma Debug (O ("Finalize: caught "
                          & Ada.Exceptions.Exception_Information (E)));
         raise;
   end Finalize;

   ---------------
   -- Get_Count --
   ---------------

   function Get_Count
     (Self : in Ref)
      return PolyORB.Types.Unsigned_Long is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      return PolyORB.Types.Unsigned_Long
        (Exception_Sequences.Length (Obj.List));
   end Get_Count;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in Ref;
      Exc : in PolyORB.Any.TypeCode.Object)
   is
      Obj : Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      Exception_Sequences.Append (Obj.List, Exc);
   end Add;

   ----------
   -- Item --
   ----------

   function Item
     (Self : in Ref;
      Index : in PolyORB.Types.Unsigned_Long)
      return PolyORB.Any.TypeCode.Object
   is
      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      return Exception_Sequences.Element_Of (Obj.List, Positive (Index));
   end Item;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in Ref;
      Index : in PolyORB.Types.Unsigned_Long)
   is
      Obj : Object_Ptr := Object_Ptr (Entity_Of (Self));
   begin
      Exception_Sequences.Delete (Obj.List, Positive (Index), 1);
   end Remove;

   -------------------
   -- Create_Object --
   -------------------

   function Create_Object return Object_Ptr;

   function Create_Object return Object_Ptr is
   begin
      return new Object;
   end Create_Object;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List (Self : out Ref) is
      Result : Ref;
   begin
      Set (Result, PolyORB.Smart_Pointers.Entity_Ptr (Create_Object));
      Self := Result;
   end Create_List;

   -------------------------
   -- Search_Exception_Id --
   -------------------------

   function Search_Exception_Id
     (Self : in Ref;
      Name : in PolyORB.Types.String)
     return PolyORB.Types.Unsigned_Long
   is
      use PolyORB.Types;

      function Match
        (Item : TypeCode.Object;
         Needle : PolyORB.Types.String)
        return Boolean;

      function Match
        (Item : TypeCode.Object;
         Needle : PolyORB.Types.String)
        return Boolean is
      begin
         pragma Debug
           (O ("Match : Id (Item) = """ &
               To_Standard_String (PolyORB.Any.TypeCode.Id (Item)) &
               """ and Needle = """ &
               To_Standard_String (Needle) &
               """"));
         return PolyORB.Any.TypeCode.Id (Item)
           = PolyORB.Types.RepositoryId (Needle);
      end Match;

      package Exception_Search is new Exception_Sequences.Search
        (PolyORB.Types.String, Match);

      Obj : constant Object_Ptr := Object_Ptr (Entity_Of (Self));

   begin
      pragma Debug (O ("Search_Exception_Id : Obj.list length is " &
                       PolyORB.Types.Unsigned_Long'Image (Get_Count (Self))));
      pragma Debug (O ("Search_Exception_Id : Name = """ &
                       To_Standard_String (Name) & """"));
      pragma Debug (O ("Search_Exception_Id : first excpt id = """ &
                       To_Standard_String
                       (TypeCode.Id
                        (Exception_Sequences.Element_Of
                         (Obj.List, 1))) & """"));
      return PolyORB.Types.Unsigned_Long
        (Exception_Search.Index (Obj.List, Name));
   end Search_Exception_Id;

end PolyORB.Any.ExceptionList;
