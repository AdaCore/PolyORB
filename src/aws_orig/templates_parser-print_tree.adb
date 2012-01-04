------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           P R I N T _ T R E E                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1999-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

separate (Templates_Parser)

procedure Print_Tree (T : Tree; Level : Natural := 0) is

   procedure Print_Indent (L : Natural);
   procedure Print_Indent (L : Natural) is
      use Ada.Strings.Fixed;
   begin
      Text_IO.Put ((L * 2) * ' ');
   end Print_Indent;

begin
   if T = null then
      return;
   end if;

   Print_Indent (Level);

   case T.Kind is
      when Info =>
         Text_IO.Put_Line ("[INFO] " & To_String (T.Filename)
                           & Natural'Image (T.Ref));
         declare
            I : Tree := T.I_File;
         begin
            while I /= null loop
               Text_IO.Put (" -> ");
               Text_IO.Put_Line (To_String (I.File.Info.Filename));
               I := I.Next;
            end loop;
         end;

         Print_Tree (T.Next, Level);

      when C_Info =>
         Text_IO.Put_Line ("[C_INFO] "
                           & Natural'Image (T.Used)
                           & ' ' & Boolean'Image (T.Obsolete));

         Print_Tree (T.Next, Level);

      when Text =>
         Text_IO.Put ("[TEXT] ");
         Data.Print_Tree (T.Text);
         Print_Tree (T.Next, Level);

      when If_Stmt  =>
         Text_IO.Put ("[IF_STMT] ");
         Expr.Print_Tree (T.Cond);
         Text_IO.New_Line;
         Print_Tree (T.N_True, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[ELSE]");
         Print_Tree (T.N_False, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_IF_STMT]");
         Print_Tree (T.Next, Level);

      when Table_Stmt =>
         Text_IO.Put_Line ("[TABLE_STMT] TERMINATE_SECTIONS="
                           & Boolean'Image (T.Terminate_Sections));
         Print_Tree (T.Sections, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_TABLE_STMT]");
         Print_Tree (T.Next, Level);

      when Section_Stmt =>
         Text_IO.Put_Line ("[SECTION_STMT]");
         Print_Tree (T.Next, Level + 1);
         Print_Tree (T.N_Section, Level);

      when Include_Stmt =>
         Text_IO.Put_Line ("[INCLUDE_STMT] "
                           & To_String (T.File.Info.Filename));
         Print_Tree (T.File.Info, Level + 1);
         Print_Tree (T.Next, Level);
   end case;

end Print_Tree;
