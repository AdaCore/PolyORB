------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S T Y L E S W                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-1998, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

package body Stylesw is

   -------------------------------------
   -- Set_Default_Style_Check_Options --
   -------------------------------------

   procedure Set_Default_Style_Check_Options is
      Discard : Boolean;

   begin
      Set_Style_Check_Option ('3', Discard);
      Set_Style_Check_Option ('b', Discard);
      Set_Style_Check_Option ('c', Discard);
      Set_Style_Check_Option ('e', Discard);
      Set_Style_Check_Option ('h', Discard);
      Set_Style_Check_Option ('i', Discard);
      Set_Style_Check_Option ('k', Discard);
      Set_Style_Check_Option ('m', Discard);
      Set_Style_Check_Option ('r', Discard);
      Set_Style_Check_Option ('s', Discard);
      Set_Style_Check_Option ('t', Discard);
   end Set_Default_Style_Check_Options;

   ----------------------------
   -- Set_Style_Check_Option --
   ----------------------------

   procedure Set_Style_Check_Option (C : Character; OK : out Boolean) is
   begin
      OK := True;

      case C is
         when '1' .. '9' =>
            Style_Check_Indentation := Character'Pos (C) - Character'Pos ('0');

         when 'b' =>
            Style_Check_Blanks_At_End    := True;

         when 'c' =>
            Style_Check_Comments         := True;

         when 'e' =>
            Style_Check_End_Labels       := True;

         when 'h' =>
            Style_Check_Horizontal_Tabs  := True;

         when 'i' =>
            Style_Check_If_Then_Layout   := True;

         when 'k' =>
            Style_Check_Kasing           := True;

         when 'm' =>
            Style_Check_Max_Line_Length  := True;

         when 'r' =>
            Style_Check_RM_Column_Layout := True;

         when 's' =>
            Style_Check_Specs            := True;

         when 't' =>
            Style_Check_Tokens           := True;

         when others =>
            OK := False;

      end case;
   end Set_Style_Check_Option;

end Stylesw;
