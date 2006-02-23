with Ada.Command_Line;  use Ada.Command_Line;

with Output;    use Output;
with Namet;     use Namet;

package body Errors is

   procedure Check_Space;
   --  Check there is a trailing space in order to append a string to
   --  name buffer.

   -----------------
   -- Check_Space --
   -----------------

   procedure Check_Space is
   begin
      if Name_Len > 0 and then Name_Buffer (Name_Len) /= ' ' then
         Add_Char_To_Name_Buffer (' ');
      end if;
   end Check_Space;

   -------------------
   -- Display_Error --
   -------------------

   procedure Display_Error (S : String; K : Error_Kind := K_Error)
   is
      L : Natural := 1;
      I : Natural := 1;
      N : Natural := 1;
      M : Boolean := False; --  Meta-character
      J : Natural := S'First;
   begin
      if K = K_Error then
         N_Errors := N_Errors + 1;
      elsif K = K_Warning then
         N_Warnings := N_Warnings + 1;
      end if;

      if Error_Loc (L) = No_Location then
         Set_Str_To_Name_Buffer (Command_Name);
      else
         Set_Str_To_Name_Buffer (Image (Error_Loc (L)));
      end if;
      L := L + 1;
      Add_Str_To_Name_Buffer (": ");

      while J <= S'Last loop

         --  Escape meta-character

         if S (J) = '|' then
            if J < S'Last then
               J := J + 1;
            end if;
            Add_Char_To_Name_Buffer (S (J));

         elsif S (J) = '%' then
            Check_Space;
            Get_Name_String_And_Append (Error_Name (N));
            N := N + 1;
            M := True;

         elsif S (J) = '#' then
            Check_Space;
            Add_Char_To_Name_Buffer ('"'); -- "
            Get_Name_String_And_Append (Error_Name (N));
            Add_Char_To_Name_Buffer ('"'); -- "
            N := N + 1;
            M := True;

         elsif S (J) = '!' then
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

         elsif S (J) = '$' then
            Add_Nat_To_Name_Buffer (Error_Int (I));
            I := I + 1;
            M := False;

         else
            if M then
               Add_Char_To_Name_Buffer (' ');
               M := False;
            end if;
            Add_Char_To_Name_Buffer (S (J));
         end if;

         J := J + 1;
      end loop;
      Set_Standard_Error;
      Write_Line (Name_Buffer (1 .. Name_Len));
      Set_Standard_Output;

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
