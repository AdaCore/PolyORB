------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S W I T C H - M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 2001 Free Software Foundation, Inc.               --
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

with Debug;    use Debug;
with Osint;    use Osint;
with Opt;      use Opt;

package body Switch.M is

   -- Scan_Make_Switches --
   ------------------------

   procedure Scan_Make_Switches (Switch_Chars : String) is
      Ptr : Integer := Switch_Chars'First;
      Max : Integer := Switch_Chars'Last;
      C   : Character := ' ';

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;

      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler (where it was already removed)

      if Switch_Chars'Length >= Ptr + 3
        and then Switch_Chars (Ptr .. Ptr + 3) = "gnat"
      then
         Osint.Fail
           ("invalid switch: """, Switch_Chars, """ (gnat not needed here)");
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switch_Chars (Ptr);

         --  Processing for a switch

         case C is

         when 'a' =>
            Ptr := Ptr + 1;
            Check_Readonly_Files := True;

         --  Processing for b switch

         when 'b' =>
            Ptr := Ptr + 1;
            Bind_Only := True;

         --  Processing for c switch

         when 'c' =>
            Ptr := Ptr + 1;
            Compile_Only := True;

         --  Processing for C switch

         when 'C' =>
            Ptr := Ptr + 1;
            Create_Mapping_File := True;

         when 'd' =>

            --  Note: for the debug switch, the remaining characters in this
            --  switch field must all be debug flags, since all valid switch
            --  characters are also valid debug characters.

            --  Loop to scan out debug flags

            while Ptr < Max loop
               Ptr := Ptr + 1;
               C := Switch_Chars (Ptr);
               exit when C = ASCII.NUL or else C = '/' or else C = '-';

               if C in '1' .. '9' or else
                  C in 'a' .. 'z' or else
                  C in 'A' .. 'Z'
               then
                  Set_Debug_Flag (C);
               else
                  raise Bad_Switch;
               end if;
            end loop;

            --  Make sure Zero_Cost_Exceptions is set if gnatdX set. This
            --  is for backwards compatibility with old versions and usage.

            if Debug_Flag_XX then
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := True;
            end if;

            return;

         --  Processing for f switch

         when 'f' =>
            Ptr := Ptr + 1;
            Force_Compilations := True;

         --  Processing for G switch

         when 'G' =>
            Ptr := Ptr + 1;
            Print_Generated_Code := True;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for i switch

         when 'i' =>
            Ptr := Ptr + 1;
            In_Place_Mode := True;

         --  Processing for j switch

         when 'j' =>
            Ptr := Ptr + 1;

            declare
               Max_Proc : Pos;
            begin
               Scan_Pos (Switch_Chars, Max, Ptr, Max_Proc);
               Maximum_Processes := Positive (Max_Proc);
            end;

         --  Processing for k switch

         when 'k' =>
            Ptr := Ptr + 1;
            Keep_Going := True;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;
            Link_Only := True;

         when 'M' =>
            Ptr := Ptr + 1;
            List_Dependencies := True;

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;
            Do_Not_Execute := True;

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Output_File_Name_Present then
               raise Too_Many_Output_Files;
            else
               Output_File_Name_Present := True;
            end if;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;
            Quiet_Output := True;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;
            Check_Switches := True;

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;
            Verbose_Mode := True;

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;
            No_Main_Subprogram := True;

         --  Ignore extra switch character

         when '/' | '-' =>
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         when others =>
            raise Bad_Switch;

         end case;
      end loop;

   exception
      when Bad_Switch =>
         Osint.Fail ("invalid switch: ", (1 => C));

      when Bad_Switch_Value =>
         Osint.Fail ("numeric value too big for switch: ", (1 => C));

      when Missing_Switch_Value =>
         Osint.Fail ("missing numeric value for switch: ", (1 => C));

      when Too_Many_Output_Files =>
         Osint.Fail ("duplicate -o switch");

   end Scan_Make_Switches;

end Switch.M;
