with GNAT.Command_Line; use GNAT.Command_Line;

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

   procedure Scan_Flags is
   begin
      Initialize_Option_Scan ('-', False, "cppargs");

      loop
         case Getopt ("E I: c g! t d?") is
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

            when 't' =>
               Print_Full_Tree := True;

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

      Set_Str_To_Name_Buffer (Get_Argument);
      if Name_Len /= 0 then
         Main_Source := Name_Find;
      end if;
   end Scan_Flags;

end Flags;
