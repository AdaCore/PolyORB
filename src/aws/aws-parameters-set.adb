------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   A W S . P A R A M E T E R S . S E T                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

with Ada.Strings.Fixed;

with AWS.URL;
with AWS.Containers.Tables.Set;

package body AWS.Parameters.Set is

   use AWS.Containers;

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : String) is
   begin
      Tables.Set.Add
        (Tables.Table_Type (Parameter_List),
         URL.Decode (Name),
         URL.Decode (Value));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Parameter_List : in out List; Parameters : String) is
      use Ada.Strings;

      P : String renames Parameters;
      C : Positive := P'First;
      I : Natural;
      S : Positive := P'First;
      E : Natural;
   begin
      --  Skip leading question mark if present.

      if P /= "" and then P (C) = '?' then
         C := Positive'Succ (C);
         S := Positive'Succ (S);
      end if;

      Parameter_List.Parameters := To_Unbounded_String ('?' & P (C .. P'Last));

      loop
         I := Fixed.Index (P (C .. P'Last), "=");

         exit when I = 0;

         S := I + 1;

         E := Fixed.Index (P (S .. P'Last), "&");

         if E = 0 then
            --  last parameter

            Add (Parameter_List, P (C .. I - 1), P (S .. P'Last));
            exit;

         else
            Add (Parameter_List, P (C .. I - 1), P (S .. E - 1));
            C := E + 1;
         end if;
      end loop;
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : Boolean) is
   begin
      Tables.Set.Case_Sensitive (Tables.Table_Type (Parameter_List), Mode);
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Parameter_List : in out List) is
   begin
      Tables.Set.Free (Tables.Table_Type (Parameter_List));
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parameter_List : in out List) is
   begin
      Tables.Set.Reset (Tables.Table_Type (Parameter_List));
      Parameter_List.Parameters := Null_Unbounded_String;
   end Reset;

end AWS.Parameters.Set;
