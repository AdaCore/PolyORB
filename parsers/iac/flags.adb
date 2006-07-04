------------------------------------------------------------------------------
--                                                                          --
--                            POLYORB COMPONENTS                            --
--                                   IAC                                    --
--                                                                          --
--                                F L A G S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                        Copyright (c) 2005 - 2006                         --
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

with Ada.Command_Line;  use Ada.Command_Line;
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;       use GNAT.OS_Lib;

with Lexer;             use Lexer;
with Scopes;            use Scopes;

with Backend.BE_CORBA_Ada;
with Backend.BE_IDL;
with Backend.BE_Types;

with Namet; use Namet;

package body Flags is

   package BEA renames Backend.BE_CORBA_Ada;
   package BEI renames Backend.BE_IDL;
   package BET renames Backend.BE_Types;

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

      --  The command line parsing in IAC is a bit complicated. The
      --  structure of the command line is as follows :

      --  %iac [general_switches] [-<backend>] [backend_switches] file
      --       [-cppargs preprocessor_flags]

      --  Check wether the user precised the Backend to use ...

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

      --  ... else we use the default backend

      if not Found_Language then
         Backend.Set_Current_Language (Backend.Current_Language);
      end if;

      Set_Str_To_Name_Buffer  ("cppargs");
      Initialize_Option_Scan ('-', False, Name_Buffer (1 .. Name_Len));
      loop
         case Getopt ("b: c d! E e h! I: i k o: p r! s t! ada idl types") is

            when ASCII.NUL =>
               exit;

            when 'b' =>
               BEI.Default_Base := Natural'Value (Parameter);

            when 'c' =>
               BEA.Disable_Server_Code_Gen := True;

            when 'd' =>
               declare
                  P : constant String := Parameter;
               begin
                  for I in P'Range loop
                     case P (I) is

                        when 'a' =>
                           BEA.Print_Ada_Tree := True;

                        when 'b' =>
                           BEA.Disable_Pkg_Spec_Gen := True;

                        when 'f' =>
                           Print_Full_Tree := True;
                           BEI.Print_IDL_Tree := True;

                        when 'i' =>
                           BEA.Generate_Imported := True;
                           BEI.Generate_Imported := True;

                        when 'm' =>
                           D_Scopes   := True;

                        when 's' =>
                           BEA.Disable_Pkg_Body_Gen := True;

                        when 't' =>
                           BEA.Output_Tree_Warnings := True;

                        when 'w' =>
                           BEA.Output_Unit_Withing := True;

                        when others =>
                           raise Invalid_Switch;
                     end case;
                  end loop;
               end;

            when 'E' =>
               Preprocess_Only := True;

            when 'e' =>
               BEI.Expand_Tree := True;

            when 'I' =>

               --  We add the parameter WITHOUT the ending
               --  directory separator

               if Parameter (Parameter'Last) = Directory_Separator then
                  Add_IAC_Search_Path
                    (Parameter (Parameter'First .. Parameter'Last - 1));
               else
                  Add_IAC_Search_Path (Parameter);
               end if;

            when 'h' =>
               declare
                  S : constant String := Parameter;
               begin
                  BEA.Use_Minimal_Hash_Function := True;
                  case S (S'First) is
                     when 'c' =>
                        BEA.Optimize_CPU    := True;

                     when 'm' =>
                        BEA.Optimize_Memory := True;

                     when others =>
                        raise Program_Error;
                  end case;
               end;

            when 'i' =>
               BEA.Impl_Packages_Gen := True;

            when 'k' =>
               Keep_TMP_Files := True;

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

            when 'p' =>
               if Backend.Current_Language = "types" then
                  BET.Print_Types := True;
               end if;
               Print_On_Stdout := True;

            when 'r' =>
               declare
                  S : constant String := Parameter;
               begin
                  case S (S'First) is
                     when 's' =>
                        BEA.Use_SII := True;

                     when 'd' =>
                        BEA.Use_SII := False;

                     when 'o' =>
                        --  Buffers allocation optimization can be
                        --  used only with SII/SSI invokation

                        BEA.Use_SII := True;
                        BEA.Use_Optimized_Buffers_Allocation := True;

                     when 'a' =>
                        --  Marshalling optimization can be
                        --  used only with SII/SSI invokation

                        BEA.Use_Compiler_Alignment := True;
                        BEA.Use_SII := True;

                     when others =>
                        raise Program_Error;
                  end case;
               end;

            when 's' =>
               BEA.Disable_Client_Code_Gen := True;

            when others =>
               if Full_Switch /= "ada"
                 and then Full_Switch /= "idl"
                 and then Full_Switch /= "types"
               then
                  raise Invalid_Switch;
               end if;
         end case;
      end loop;

      --  When the user gives both "-s" and "-c", we generate the code
      --  for both client side and server side

      if BEA.Disable_Client_Code_Gen
        and then BEA.Disable_Server_Code_Gen
      then
         BEA.Disable_Client_Code_Gen := False;
         BEA.Disable_Server_Code_Gen := False;
      end if;

      --  When the user gives both "-db" and "-ds" we generate the
      --  code for both specs and bodies

      if BEA.Disable_Pkg_Body_Gen
        and then BEA.Disable_Pkg_Spec_Gen
      then
         BEA.Disable_Pkg_Body_Gen := False;
         BEA.Disable_Pkg_Spec_Gen := False;
      end if;

      if Main_Source = No_Name then
         Set_Str_To_Name_Buffer (Get_Argument);
         if Name_Len /= 0 then
            Main_Source := Name_Find;
         end if;
      end if;
   end Scan_Flags;

end Flags;
