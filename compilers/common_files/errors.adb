------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               E R R O R S                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2007, Free Software Foundation, Inc.          --
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

with Ada.Command_Line;  use Ada.Command_Line;

with Output;    use Output;
with Namet;     use Namet;

package body Errors is

   -------------------
   -- Display_Error --
   -------------------

   procedure Display_Error (S : String) is
      procedure Check_Space;
      --  Ensure the last character of the name buffer is a space

      -----------------
      -- Check_Space --
      -----------------

      procedure Check_Space is
      begin
         if Name_Len > 0 and then Name_Buffer (Name_Len) /= ' ' then
            Add_Char_To_Name_Buffer (' ');
         end if;
      end Check_Space;

      --  ??? All the declarations below need clearer, meaningful names nad
      --  explanatory comments.

      L : Natural := 1;
      I : Natural := 1;
      N : Natural := 1;
      M : Boolean := False;
      --  True when the current character is a special insertion character
      J : Natural := S'First;

      type Message_Kind is (K_Error, K_Warning, K_Continuation);
      K : Message_Kind;

   begin
      if Error_Loc (L) = No_Location then
         Set_Str_To_Name_Buffer (Command_Name);
      else
         Set_Str_To_Name_Buffer (Image (Error_Loc (L)));
      end if;
      L := L + 1;
      Add_Str_To_Name_Buffer (": ");

      K := K_Error;
      for J in S'Range loop
         case S (J) is
            when '\' =>
               K := K_Continuation;
               exit;
            when '?' =>
               K := K_Warning;
               exit;
            when others =>
               null;
         end case;
      end loop;

      case K is
         when K_Error =>
            N_Errors := N_Errors + 1;

         when K_Warning =>
            N_Warnings := N_Warnings + 1;
            Add_Str_To_Name_Buffer ("warning: ");

         when K_Continuation =>
            null;
      end case;

      while J <= S'Last loop

         --  Process special insertion characters

         case S (J) is
            when '%' =>
               Check_Space;
               Get_Name_String_And_Append (Error_Name (N));
               N := N + 1;
               M := True;

            when '#' =>
               Check_Space;
               Add_Char_To_Name_Buffer ('"');
               Get_Name_String_And_Append (Error_Name (N));
               Add_Char_To_Name_Buffer ('"');
               N := N + 1;
               M := True;

            when '!' =>
               if L = 1 then
                  Add_Str_To_Name_Buffer (Image (Error_Loc (1)));

               elsif Error_Loc (1).File = Error_Loc (L).File then
                  Check_Space;
                  Add_Str_To_Name_Buffer ("at line ");
                  Add_Nat_To_Name_Buffer (Error_Loc (L).Line);

               else
                  Check_Space;
                  Add_Str_To_Name_Buffer ("in ");
                  Add_Str_To_Name_Buffer (Image (Error_Loc (1)));
               end if;
               L := L + 1;
               M := True;

            when '$' =>
               Add_Nat_To_Name_Buffer (Error_Int (I));
               I := I + 1;
               M := False;

            when '?' | '\' =>
               --  Already dealt with

               null;

            when others =>
               --  Add space after insertion if not provided by S

               if M then
                  if S (J) /= ' ' then
                     Add_Char_To_Name_Buffer (' ');
                  end if;
                  M := False;
               end if;

               Add_Char_To_Name_Buffer (S (J));
         end case;

         J := J + 1;
      end loop;

      Set_Standard_Error;
      Write_Line (Name_Buffer (1 .. Name_Len));
      Set_Standard_Output;

      --  Reset all insertion data to ensure it is not erroneously propagated
      --  from one error to another.

      Initialize;

   end Display_Error;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Error_Loc  := (others => No_Location);
      Error_Name := (others => No_Name);
      Error_Int  := (others => 0);
   end Initialize;

end Errors;
