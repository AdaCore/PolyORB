------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         P O L Y O R B . D I C T                          --
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

package body PolyORB.Dict is

   use Dict_Entry_Seqs;

   procedure Set
     (D : in out Dict; K : Key; V : Value; Index : out Natural)
   is
      Existing : constant Natural := Find_Key (D, K);
   begin
      if Existing /= Null_Index then
         raise Duplicate_Key;
      end if;

      Append (D.Entries, Dict_Entry'(K, V));
      Index := Length (D.Entries);
   end Set;

   function Find_Key (D : Dict; K : Key) return Natural
   is
      Elements : constant Element_Array
        := To_Element_Array (D.Entries);
   begin
      for I in Elements'Range loop
         if Elements (I).K = K then
            return 1 + I - Elements'First;
         end if;
      end loop;

      return Null_Index;
   end Find_Key;

   function Find_Value (D : Dict; V : Value) return Natural
   is
      Elements : constant Element_Array
        := To_Element_Array (D.Entries);
   begin
      for I in Elements'Range loop
         if Elements (I).V = V then
            return 1 + I - Elements'First;
         end if;
      end loop;

      return Null_Index;
   end Find_Value;

   function Get (D : Dict; K : Key) return Value
   is
      Index : constant Natural := Find_Key (D, K);
   begin
      if Index = Null_Index then
         raise Bad_Element;
      end if;

      return Get_By_Index (D, Index);
   end Get;

   function Get_By_Index (D : Dict; Index : Natural) return Value is
   begin
      return Element_Of (D.Entries, Index).V;
   end Get_By_Index;

   procedure Remove (D : in out Dict; K : Key; V : out Value)
   is
      Index : constant Natural := Find_Key (D, K);
   begin
      if Index = Null_Index then
         raise Bad_Element;
      end if;

      Remove_By_Index (D, Index, V);
   end Remove;

   procedure Remove_By_Index
     (D : in out Dict; Index : Natural; V : out Value) is
   begin
      V := Get_By_Index (D, Index);
      Delete (D.Entries, Index, Index);
   end Remove_By_Index;

end PolyORB.Dict;
