with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;

with Backend;
with Namet; use Namet;
with Types; use Types;

package body Flags is

   ------------------
   -- Add_CPP_Flag --
   ------------------

   procedure Add_CPP_Flag (S : String) is
   begin
      CPP_Arg_Count := CPP_Arg_Count + 1;
      CPP_Arg_Values (CPP_Arg_Count) := new String'(S);
   end Add_CPP_Flag;

   ----------------
   -- Scan_Flags --
   ----------------

   procedure Scan_Flags
   is
      Found_Language : Boolean := False;
   begin
      Initialize_Option_Scan;
      for I in 1 .. Argument_Count loop
         Set_Str_To_Name_Buffer (Argument (I));
         if Name_Buffer (1) = '-'
           and then Name_Len > 1
           and then Backend.Is_Valid_Language (Name_Buffer (2 .. Name_Len))
         then
            if Found_Language then
               raise Invalid_Switch;
            end if;
            Backend.Set_Current_Language (Name_Buffer (2 .. Name_Len));
            Found_Language := True;
         end if;
      end loop;

      Set_Str_To_Name_Buffer  ("cppargs");
      if Found_Language then
         Add_Char_To_Name_Buffer (' ');
         Add_Str_To_Name_Buffer  (Backend.Current_Language);
      end if;

      Initialize_Option_Scan ('-', False, Name_Buffer (1 .. Name_Len));
      loop
         case Getopt ("E I: c g! d?") is
            when ASCII.NUL =>
               exit;

            when 'E' =>
               Preprocess_Only := True;

            when 'I' =>
               Add_CPP_Flag ("-I");
               Add_CPP_Flag (Parameter);

            when 'g' =>
               declare
                  P : constant String := Parameter;
               begin
                  if P'Length /= 1 then
                     raise Program_Error;
                  end if;
                  case P (1) is
                     when 'd' =>
                        Gen_Delegate := True;

                     when 'i' =>
                        Gen_Impl_Tmpl := True;

                     when 'D' =>
                        Gen_Dyn_Inv  := False;

                     when 'I' =>
                        Gen_Intf_Rep := False;

                     when others =>
                        raise Program_Error;
                  end case;
               end;

            when 'd' =>
               declare
                  P : constant String := Parameter;
               begin
                  for I in P'Range loop
                     case P (I) is
                        when 'a' =>
                           D_Analyzer := True;
                        when 's' =>
                           D_Scopes   := True;
                        when 't' =>
                           Print_Full_Tree := True;
                        when others =>
                           raise Program_Error;
                     end case;
                  end loop;
               end;

            when others =>
               --  This never happens.
               raise Program_Error;
         end case;
      end loop;

      if Main_Source = No_Name then
         Set_Str_To_Name_Buffer (Get_Argument);
         if Name_Len /= 0 then
            Main_Source := Name_Find;
         end if;
      end if;

      if Found_Language then
         Goto_Section (Backend.Current_Language);
         Backend.Configure;
      end if;

      if Main_Source = No_Name then
         Set_Str_To_Name_Buffer (Get_Argument);
         if Name_Len /= 0 then
            Main_Source := Name_Find;
         end if;
      end if;
   end Scan_Flags;

end Flags;
