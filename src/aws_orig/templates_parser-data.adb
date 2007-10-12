------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 D A T A                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2006, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

separate (Templates_Parser)

package body Data is

   -----------
   -- Parse --
   -----------

   function Parse (Line : String) return Tree is

      Begin_Tag : constant String
        := To_String (Templates_Parser.Begin_Tag);

      End_Tag : constant String
        := To_String (Templates_Parser.End_Tag);

      function Build (Line : String) return Tree;
      --  Recursive function to build the tree

      -----------
      -- Build --
      -----------

      function Build (Line : String) return Tree is
         Start, Stop : Natural;
      begin
         if Line = "" then
            return null;

         else
            Start := Strings.Fixed.Index (Line, Begin_Tag);

            if Start = 0 then
               --  No more tag
               return new Node'(Text,
                                null,
                                To_Unbounded_String (Line));
            else
               Stop := Strings.Fixed.Index (Line, End_Tag);

               if Stop = 0 then
                  Exceptions.Raise_Exception
                    (Internal_Error'Identity,
                     "Tag variable not terminated (missing "
                     & End_Tag & ")");

               else
                  Stop := Stop + End_Tag'Length - 1;

                  if Start = Line'First then
                     return new Node'
                       (Var,
                        Build (Line (Stop + 1 .. Line'Last)),
                        Build (Line (Start .. Stop)));
                  else
                     return new Node'
                       (Text,
                        Build (Line (Start .. Line'Last)),
                        To_Unbounded_String
                          (Line (Line'First .. Start - 1)));
                  end if;
               end if;
            end if;
         end if;
      end Build;

   begin
      return Build (Line);
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (D : Tree) is
   begin
      if D = null then
         return;
      end if;

      case D.Kind is
         when Text =>
            Text_IO.Put (To_String (D.Value));

         when Var =>
            Text_IO.Put (Image (D.Var));
      end case;

      Print_Tree (D.Next);
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (D : in out Tree) is

      procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);

      P : Tree;
      T : Tree := D;

   begin
      while T /= null loop
         P := T;
         T := T.Next;

         if P.Kind = Var then
            Release (P.Var);
         end if;

         Free (P);
      end loop;
   end Release;

end Data;
