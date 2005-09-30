------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                                F L A G S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                           Copyright (c) 2005                             --
--            Ecole Nationale Superieure des Telecommunications             --
--                                                                          --
-- IAC is free software; you  can  redistribute  it and/or modify it under  --
-- terms of the GNU General Public License  as published by the  Free Soft- --
-- ware  Foundation;  either version 2 of the liscence or (at your option)  --
-- any  later version.                                                      --
-- IAC is distributed  in the hope that it will be  useful, but WITHOUT ANY --
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or        --
-- FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for --
-- more details.                                                            --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; if not, write to the Free Software Foundation, Inc.,  --
-- 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.            --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;

with Backend;
with Namet; use Namet;

package body Flags is

   ------------------
   -- Add_CPP_Flag --
   ------------------

   procedure Add_CPP_Flag (S : String) is
   begin
      CPP_Arg_Count := CPP_Arg_Count + 1;
      CPP_Arg_Values (CPP_Arg_Count) := new String'(S);
   end Add_CPP_Flag;

   -------------------------
   -- Add_IAC_Search_Path --
   -------------------------

   procedure Add_IAC_Search_Path (S : String) is
   begin
      IAC_Search_Count := IAC_Search_Count + 1;
      IAC_Search_Paths (IAC_Search_Count) := new String'(S);
   end Add_IAC_Search_Path;

   ----------------
   -- Scan_Flags --
   ----------------

   procedure Scan_Flags
   is
      Found_Language : Boolean := False;
   begin
      --  Add the current directory to the search path, it will be added
      --  automatically to the preprocessor serach path
      Add_IAC_Search_Path (".");

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
      else
         Backend.Set_Current_Language
           (Backend.Current_Language);
      end if;

      Initialize_Option_Scan ('-', False, Name_Buffer (1 .. Name_Len));
      loop
         case Getopt ("E k I: c g! d? p o:") is
            when ASCII.NUL =>
               exit;

            when 'E' =>
               Preprocess_Only := True;

            when 'I' =>

               --  We add the parameter WITHOUT the ending directory separator

               if Parameter (Parameter'Last) = Directory_Separator then
                  Add_IAC_Search_Path
                    (Parameter (Parameter'First .. Parameter'Last - 1));
               else
                  Add_IAC_Search_Path (Parameter);
               end if;

            when 'k' =>
               Keep_TMP_Files := True;

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

            when 'p' =>
               Print_On_Stdout := True;

            when 'o' =>
               if Output_Directory /= null
                 or else not GNAT.OS_Lib.Is_Directory (Parameter)
               then
                  raise Invalid_Parameter;
               else
                  if Parameter (Parameter'Last) = Directory_Separator then
                     Output_Directory := new String'(Parameter);
                  else
                     Output_Directory := new String'
                       (Parameter & Directory_Separator);
                  end if;
               end if;

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
