------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                            --
--                                                                          --
--   Copyright (C) 1992,1993,1994,1995,1996 Free Software Foundation, Inc.  --
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

--  Option switch scanning for both the compiler and the binder

--  Note: this version of the package should be usable in both Unix and DOS

with Debug;  use Debug;
with Osint;  use Osint;
with Opt;    use Opt;
with Output; use Output;
with Types;  use Types;

with System.WCh_Con; use System.WCh_Con;

package body Switch is

   Bad_Switch : exception;
   --  Exception raised if bad switch encountered

   Bad_Switch_Value : exception;
   --  Exception raised if bad switch value encountered

   Missing_Switch_Value : exception;
   --  Exception raised if no switch value encountered

   Too_Many_Output_Files : exception;
   --  Exception raised if the -o switch is encountered more than once

   Switch_Max_Value : constant := 999;
   --  Maximum value permitted in switches that take a value

   -------------------
   -- Scan_Switches --
   -------------------

   procedure Scan_Switches (Switch_Chars : String) is
      Ptr : Integer := Switch_Chars'First;
      Max : Integer := Switch_Chars'Last;
      C   : Character := ' ';

      Switches : String (Switch_Chars'Range);
      --  Copy of switches which is actually scanned. Switches in GNAT are
      --  always case sensitive and therefore not folded to upper case since
      --  that is the convention in GCC.

      function Scan_Int return Pos;
      --  Scan positive integer parameter for switch. On entry, Ptr points
      --  just past the switch character, on exit it points past the last
      --  digit of the integer value.

      function Scan_Int return Pos is
         Val : Int := 0;

      begin
         if Ptr > Max or else Switches (Ptr) not in '0' .. '9' then
            raise Missing_Switch_Value;
         end if;

         while Ptr <= Max and then Switches (Ptr) in '0' .. '9' loop
            Val := Val * 10 +
              Character'Pos (Switches (Ptr)) - Character'Pos ('0');
            Ptr := Ptr + 1;

            if Val > Switch_Max_Value then
               raise Bad_Switch_Value;
            end if;
         end loop;

         return Val;
      end Scan_Int;

   --  Start of processing for Scan_Switches

   begin
      for J in Switches'Range loop
         Switches (J) := Switch_Chars (J);
      end loop;

      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;
      else
         Ptr := Ptr + 1;
      end if;

      --  Loop to scan through switches given in switch string

      while Ptr <= Max loop
         C := Switches (Ptr);

         --  Processing for -a switch

         if C = 'a' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Assertions_Enabled := True;
            elsif Program = Make then
               Check_Internal_Files := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -b switch

         elsif C = 'b' then
            if Program = Compiler or else Program = Binder then
               Ptr := Ptr + 1;
               Brief_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -c switch

         elsif C = 'c' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Operating_Mode := Check_Semantics;
            elsif Program = Binder then
               Check_Only := True;
            elsif Program = Make then
               Compile_Only := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -d switch

         elsif C = 'd' then

            --  Note: for the debug switch, the remaining characters in this
            --  switch field must all be debug flags, since all valid switch
            --  characters are also valid debug characters.

            --  Loop to scan out debug flags

            while Ptr < Max loop
               Ptr := Ptr + 1;
               C := Switches (Ptr);
               exit when C = Ascii.NUL or else C = '/' or else C = '-';

               if C in '1' .. '9' or else C in 'a' .. 'z' then
                  Set_Debug_Flag (C);
               else
                  raise Bad_Switch;
               end if;
            end loop;

            return;

         --  Processing for -e switch

         elsif C = 'e' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Immediate_Errors := True;
            elsif Program = Binder then
               Elab_Dependency_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -f switch

         elsif C = 'f' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               All_Errors_Mode := True;
            elsif Program = Make then
               Force_Compilations := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -g switch

         elsif C = 'g' then
            Ptr := Ptr + 1;
            GNAT_Mode := True;

            if Program = Compiler then
               RM_Column_Check := True;
               Style_Check := True;
               Identifier_Character_Set := 'n';

            elsif Program = Make then
               raise Bad_Switch;

            --  Is this right for the binder ???

            elsif Program = Binder then
               null;
            end if;

         --  Processing for -i switch

         elsif C = 'i' then
            if Program = Compiler or else Program = Binder then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;
               C := Switches (Ptr);

               if C = '1' or else
                  C = '2' or else
                  C = '3' or else
                  C = '4' or else
                  C = '8' or else
                  C = 'p' or else
                  C = 'f' or else
                  C = 'n' or else
                  C = 'w'
               then
                  Identifier_Character_Set := C;
                  Ptr := Ptr + 1;
               else
                  raise Bad_Switch;
               end if;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -j switch

         elsif C = 'j' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               for J in WC_Encoding_Method loop
                  if Switches (Ptr) = WC_Encoding_Letters (J) then
                     Wide_Character_Encoding_Method := J;
                     exit;

                  elsif J = WC_Encoding_Method'Last then
                     raise Bad_Switch;
                  end if;
               end loop;

               Upper_Half_Encoding :=
                 Wide_Character_Encoding_Method in
                   WC_Upper_Half_Encoding_Method;

               Ptr := Ptr + 1;

            elsif Program = Make then
               declare
                  Tmp : Int := Scan_Int;

               begin
                  if Tmp < 1 then
                     Tmp := 1;
                  end if;
                  Maximum_Processes := Positive (Tmp);
               end;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -k switch

         elsif C = 'k' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Maximum_File_Name_Length := Scan_Int;
            elsif Program = Make then
               Keep_Going := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -l switch

         elsif C = 'l' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Full_List := True;
            elsif Program = Binder then
               Elab_Order_Output := True;
            elsif Program = Make then
               List_Dependencies := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -m switch

         elsif C = 'm' then
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               Maximum_Errors := Scan_Int;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -n switch

         elsif C = 'n' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Inline_Active := True;
            elsif Program = Binder then
               Bind_Main_Program := False;
            elsif Program = Make then
               Dont_Execute := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -o switch

         elsif C = 'o' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Suppress_Options.Overflow_Checks    := False;
               Suppress_Options.Elaboration_Checks := False;
            elsif Program = Binder or else Program = Make then
               if Output_Filename_Present then
                  raise Too_Many_Output_Files;
               else
                  Output_Filename_Present := True;
               end if;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -p switch

         elsif C = 'p' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Suppress_Options.Access_Checks        := True;
               Suppress_Options.Accessibility_Checks := True;
               Suppress_Options.Discriminant_Checks  := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Elaboration_Checks   := True;
               Suppress_Options.Index_Checks         := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Overflow_Checks      := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Division_Checks      := True;
               Suppress_Options.Length_Checks        := True;
               Suppress_Options.Range_Checks         := True;
               Suppress_Options.Storage_Checks       := True;
               Suppress_Options.Tag_Checks           := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -q switch

         elsif C = 'q' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Try_Semantics := True;
            elsif Program = Make then
               Quiet_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -r switch

         elsif C = 'r' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               RM_Column_Check := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -s switch

         elsif C = 's' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               Operating_Mode := Check_Syntax;
            elsif Program = Binder then
               All_Sources := True;
               Check_Source_Files := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -t switch

         elsif C = 't' then
            Ptr := Ptr + 1;

            if Program = Binder then
               Ignore_Time_Stamp_Errors := True;
            elsif Program = Compiler then
               Tree_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -u switch

         elsif C = 'u' then
            Ptr := Ptr + 1;

            if Program = Compiler then
               List_Units := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -v switch

         elsif C = 'v' then
            Ptr := Ptr + 1;

            if Program = Compiler
              or else Program = Binder
              or else Program = Make
            then
               Verbose_Mode := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for -w switch

         elsif C = 'w' then
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then

               case Switches (Ptr) is
                  when 's' => Warning_Mode := Suppress;
                  when 'e' => Warning_Mode := Treat_As_Error;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -x switch

         elsif C = 'x' then
            Ptr := Ptr + 1;

            if Program = Compiler then

               case Switches (Ptr) is
                  when '1' => Xref_Flag_1 := True;
                  when '2' => Xref_Flag_2 := True;
                  when '3' => Xref_Flag_3 := True;
                  when '4' => Xref_Flag_4 := True;
                  when '5' => Xref_Flag_5 := True;
                  when '6' => Xref_Flag_6 := True;
                  when '9' => Xref_Flag_9 := True;
                  when 'b' => Xref_Flag_B := True;
                  when 's' => Xref_Flag_S := True;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;

            elsif Program = Binder then
               All_Sources := False;
               Check_Source_Files := False;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -z switch

         elsif C = 'z' then
            Ptr := Ptr + 1;

            --  Allowed only for compiler, and only if this is the only
            --  -z switch, we do not allow multiple occurrences

            if Program = Compiler
              and then Distribution_Stub_Mode = No_Stubs
            then
               case Switches (Ptr) is
                  when 'R' =>
                     Distribution_Stub_Mode := Compile_Receiver_Stub_Spec;

                  when 'C' =>
                     Distribution_Stub_Mode := Compile_Caller_Stub_Spec;

                  when 'r' =>
                     Distribution_Stub_Mode := Generate_Receiver_Stub_Body;
                     Operating_Mode := Check_Semantics;

                  when 'c' =>
                     Distribution_Stub_Mode := Generate_Caller_Stub_Body;
                     Operating_Mode := Check_Semantics;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;

            else
               raise Bad_Switch;
            end if;

         --  Processing for -83 switch

         elsif C = '8' then

            if Program = Compiler then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switches (Ptr) /= '3' then
                  raise Bad_Switch;
               else
                  Ptr := Ptr + 1;
                  Ada_83_Switch := True;
                  Ada_95 := False;
                  Ada_83 := True;
               end if;

            else
               raise Bad_Switch;
            end if;

         --  Ignore extra switch character

         elsif C = '/' or else C = '-' then
            Ptr := Ptr + 1;

         --  Anything else is an error (illegal switch character)

         else
            raise Bad_Switch;
         end if;

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

   end Scan_Switches;

end Switch;
