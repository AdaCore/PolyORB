------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               S W I T C H                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

with Debug;    use Debug;
with Osint;    use Osint;
with Opt;      use Opt;
with Stylesw;  use Stylesw;
with Types;    use Types;

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

      function Scan_Nat return Nat;
      --  Scan natural integer parameter for switch. On entry, Ptr points
      --  just past the switch character, on exit it points past the last
      --  digit of the integer value.

      function Scan_Pos return Pos;
      --  Scan positive integer parameter for switch. On entry, Ptr points
      --  just past the switch character, on exit it points past the last
      --  digit of the integer value.

      --------------
      -- Scan_Nat --
      --------------

      function Scan_Nat return Nat is
         Val : Int := 0;

      begin
         if Ptr > Max or else Switch_Chars (Ptr) not in '0' .. '9' then
            raise Missing_Switch_Value;
         end if;

         while Ptr <= Max and then Switch_Chars (Ptr) in '0' .. '9' loop
            Val := Val * 10 +
              Character'Pos (Switch_Chars (Ptr)) - Character'Pos ('0');
            Ptr := Ptr + 1;

            if Val > Switch_Max_Value then
               raise Bad_Switch_Value;
            end if;
         end loop;

         return Val;
      end Scan_Nat;

      --------------
      -- Scan_Pos --
      --------------

      function Scan_Pos return Pos is
         Result : constant Int := Scan_Nat;

      begin
         if Result = 0 then
            raise Bad_Switch_Value;
         else
            return Result;
         end if;
      end Scan_Pos;

   --  Start of processing for Scan_Switches

   begin
      --  Skip past the initial character (must be the switch character)

      if Ptr = Max then
         raise Bad_Switch;
      else
         Ptr := Ptr + 1;
      end if;

      --  A little check, "gnat" at the start of a switch is not allowed
      --  except for the compiler (where it was already removed)

      if Switch_Chars'Length >= 5
        and then Switch_Chars (2 .. 5) = "gnat"
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

            if Program = Compiler then
               Assertions_Enabled := True;
            elsif Program = Make then
               Check_Readonly_Files := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for A switch

         when 'A' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Config_File := False;
            elsif Program = Binder then
               Ada_Bind_File := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for b switch

         when 'b' =>
            if Program = Compiler or else Program = Binder then
               Ptr := Ptr + 1;
               Brief_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for c switch

         when 'c' =>
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

         --  Processing for C switch

         when 'C' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Compress_Debug_Names := True;
            elsif Program = Binder then
               Ada_Bind_File := False;
            else
               raise Bad_Switch;
            end if;

         --  Processing for d switch

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

         --  Processing for D switch

         when 'D' =>
            Ptr := Ptr + 1;

            --  Note: -gnatD also sets -gnatx (to turn off cross-reference
            --  generation in the ali file) since otherwise this generation
            --  gets confused by the "wrong" Sloc values put in the tree.

            if Program = Compiler then
               Debug_Generated_Code := True;
               Xref_Active := False;
               Set_Debug_Flag ('g');

            else
               raise Bad_Switch;
            end if;

         --  Processing for e switch

         when 'e' =>
            Ptr := Ptr + 1;

            if Program = Binder then
               Elab_Dependency_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for E switch

         when 'E' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Dynamic_Elaboration_Checks := True;
            elsif Program = Binder then
               Exception_Tracebacks := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for f switch

         when 'f' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               All_Errors_Mode := True;
            elsif Program = Make then
               Force_Compilations := True;
            elsif Program = Binder then
               Force_RM_Elaboration_Order := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for F switch

         when 'F' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               External_Name_Exp_Casing := Uppercase;
               External_Name_Imp_Casing := Uppercase;
            else
               raise Bad_Switch;
            end if;

         --  Processing for g switch

         when 'g' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               GNAT_Mode                := True;
               Identifier_Character_Set := 'n';
               Warning_Mode             := Treat_As_Error;
               Check_Unreferenced       := True;
               Check_Withs              := True;

               Set_Default_Style_Check_Options;

            elsif Program = Make then
               raise Bad_Switch;

            --  For binder, we accept -gx where x is 0-3. If no level is
            --  given (i.e. -g not followed by 0-3, then -g2 is assumed.

            elsif Program = Binder then
               if Ptr <= Max then
                  C := Switch_Chars (Ptr);
                  if C in '0' .. '3' then
                     Debugger_Level :=
                       Character'Pos
                         (Switch_Chars (Ptr)) - Character'Pos ('0');
                     Ptr := Ptr + 1;
                  end if;
               else
                  Debugger_Level := 2;
               end if;
            end if;

         --  Processing for G switch

         when 'G' =>
            Ptr := Ptr + 1;
            Print_Generated_Code := True;

         --  Processing for h switch

         when 'h' =>
            Ptr := Ptr + 1;
            Usage_Requested := True;

         --  Processing for H switch

         when 'H' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               HLO_Active := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for i switch

         when 'i' =>
            if Program = Compiler or else Program = Binder then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;
               C := Switch_Chars (Ptr);

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
               Ptr := Ptr + 1;
               In_Place_Mode := True;
            end if;

         --  Processing for j switch

         when 'j' =>
            Ptr := Ptr + 1;

            if Program = Make then
               Maximum_Processes := Positive (Scan_Pos);
            else
               raise Bad_Switch;
            end if;

         --  Processing for k switch

         when 'k' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Maximum_File_Name_Length := Scan_Pos;
            elsif Program = Make then
               Keep_Going := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for l switch

         when 'l' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Full_List := True;
            elsif Program = Binder then
               Elab_Order_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for L switch

         when 'L' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := False;
            else
               raise Bad_Switch;
            end if;

         --  Processing for m switch

         when 'm' =>
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               Maximum_Errors := Scan_Pos;
            else
               raise Bad_Switch;
            end if;

         --  Processing for M switch

         when 'M' =>
            Ptr := Ptr + 1;

            if Program = Make then
               List_Dependencies := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for n switch

         when 'n' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Inline_Active := True;

            elsif Program = Binder then
               Bind_Main_Program := False;

               --  Note: The -L option of the binder also implies -n, so
               --  any change here must also be reflected in the processing
               --  for -L that is found in Gnatbind.Scan_Bind_Arg.

            elsif Program = Make then
               Do_Not_Execute := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for N switch

         when 'N' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Inline_Active := True;
               Front_End_Inlining := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for o switch

         when 'o' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Suppress_Options.Overflow_Checks := False;

            elsif Program = Binder or else Program = Make then
               if Output_Filename_Present then
                  raise Too_Many_Output_Files;
               else
                  Output_Filename_Present := True;
               end if;
            else
               raise Bad_Switch;
            end if;

         --  Processing for O switch

         when 'O' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Output_Filename_Present := True;
            elsif Program = Binder then
               Output_Object_List := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for p switch

         when 'p' =>
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

            elsif Program = Binder then
               Pessimistic_Elab_Order := True;

            else
               raise Bad_Switch;
            end if;

         --  Processing for P switch

         when 'P' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Polling_Required := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for q switch

         when 'q' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Try_Semantics := True;
            elsif Program = Make then
               Quiet_Output := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for q switch

         when 'Q' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Force_ALI_Tree_File := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for r switch

         when 'r' =>
            Ptr := Ptr + 1;

            --  Temporarily allow -gnatr to mean -gnatyl (use RM layout)
            --  for compatibility with pre 3.12 versions of GNAT,
            --  to be removed for 3.13 ???

            if Program = Compiler then
               Set_Style_Check_Options ("l");
            else
               raise Bad_Switch;
            end if;

         --  Processing for R switch

         when 'R' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Back_Annotate_Rep_Info := True;

               if Ptr <= Max
                 and then Switch_Chars (Ptr) in '0' .. '9'
               then
                  C := Switch_Chars (Ptr);

                  if C in '4' .. '9' then
                     raise Bad_Switch;
                  else
                     List_Representation_Info :=
                       Character'Pos (C) - Character'Pos ('0');
                     Ptr := Ptr + 1;
                  end if;

               else
                  List_Representation_Info := 1;
               end if;

            else
               raise Bad_Switch;
            end if;

         --  Processing for s switch

         when 's' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Operating_Mode := Check_Syntax;
            elsif Program = Binder then
               All_Sources := True;
               Check_Source_Files := True;

            elsif Program = Make then
               Check_Switches := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for t switch

         when 't' =>
            Ptr := Ptr + 1;

            if Program = Binder then
               Tolerate_Consistency_Errors := True;
            elsif Program = Compiler then
               Tree_Output := True;
               Back_Annotate_Rep_Info := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for T switch

         when 'T' =>
            Ptr := Ptr + 1;

            if Program = Compiler or else Program = Binder then
               Time_Slice_Set := True;
               Time_Slice_Value := Scan_Nat;
            else
               raise Bad_Switch;
            end if;

         --  Processing for u switch

         when 'u' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               List_Units := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for U switch

         when 'U' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Unique_Error_Tag := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for v switch

         when 'v' =>
            Ptr := Ptr + 1;

            if Program = Compiler
              or else Program = Binder
              or else Program = Make
            then
               Verbose_Mode := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for V switch

         when 'V' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               case Switch_Chars (Ptr) is
                  when 'n' | '0' =>
                     Validity_Checking := None;

                  when 'd' | '1' =>
                     Validity_Checking := Default;

                  when 'f' | '2' =>
                     Validity_Checking := Full;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;
            else
               raise Bad_Switch;
            end if;

         --  Processing for w switch

         when 'w' =>

            --  For the binder we only allow suppress/error cases

            if Program = Binder then
               Ptr := Ptr + 1;

               case Switch_Chars (Ptr) is

                  when 'e' =>
                     Warning_Mode  := Treat_As_Error;

                  when 's' =>
                     Warning_Mode  := Suppress;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;

            --  For the compiler, we allow all the possible switches

            elsif Program = Compiler then

               while Ptr < Max loop
                  Ptr := Ptr + 1;
                  C := Switch_Chars (Ptr);

                  case C is

                     when 'a' =>
                        Constant_Condition_Warnings  := True;
                        Elab_Warnings                := True;
                        Check_Unreferenced           := True;
                        Check_Withs                  := True;
                        Implementation_Unit_Warnings := True;
                        Ineffective_Inline_Warnings  := True;
                        Warn_On_Redundant_Constructs := True;

                     when 'A' =>
                        Constant_Condition_Warnings  := False;
                        Elab_Warnings                := False;
                        Check_Unreferenced           := False;
                        Check_Withs                  := False;
                        Implementation_Unit_Warnings := False;
                        Warn_On_Biased_Rounding      := False;
                        Warn_On_Hiding               := False;
                        Warn_On_Redundant_Constructs := False;
                        Ineffective_Inline_Warnings  := False;

                     when 'c' =>
                        Constant_Condition_Warnings := True;

                     when 'C' =>
                        Constant_Condition_Warnings := False;

                     when 'b' =>
                        Warn_On_Biased_Rounding := True;

                     when 'B' =>
                        Warn_On_Biased_Rounding := False;

                     when 'e' =>
                        Warning_Mode := Treat_As_Error;

                     when 'h' =>
                        Warn_On_Hiding := True;

                     when 'H' =>
                        Warn_On_Hiding := False;

                     when 'i' =>
                        Implementation_Unit_Warnings := True;

                     when 'I' =>
                        Implementation_Unit_Warnings := False;

                     when 'l' =>
                        Elab_Warnings := True;

                     when 'L' =>
                        Elab_Warnings := False;

                     when 'o' =>
                        Address_Clause_Overlay_Warnings := True;

                     when 'O' =>
                        Address_Clause_Overlay_Warnings := False;

                     when 'p' =>
                        Ineffective_Inline_Warnings := True;

                     when 'P' =>
                        Ineffective_Inline_Warnings := False;

                     when 'r' =>
                        Warn_On_Redundant_Constructs := True;

                     when 'R' =>
                        Warn_On_Redundant_Constructs := False;

                     when 's' =>
                        Warning_Mode  := Suppress;

                     when 'u' =>
                        Check_Unreferenced := True;
                        Check_Withs        := True;

                     when 'U' =>
                        Check_Unreferenced := False;
                        Check_Withs        := False;

                     --  Allow and ignore 'w' so that the old
                     --  format (e.g. -gnatwuwl) will work.

                     when 'w' =>
                        null;

                     when others =>
                        raise Bad_Switch;
                  end case;
               end loop;

               return;

            else
               raise Bad_Switch;
            end if;

         --  Processing for W switch

         when 'W' =>
            Ptr := Ptr + 1;

            if Program = Compiler
              or else Program = Make
              or else Program = Binder
            then
               for J in WC_Encoding_Method loop
                  if Switch_Chars (Ptr) = WC_Encoding_Letters (J) then
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

            else
               raise Bad_Switch;
            end if;

         --  Processing for x switch

         when 'x' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Xref_Active := False;

            elsif Program = Binder then
               All_Sources := False;
               Check_Source_Files := False;

            else
               raise Bad_Switch;
            end if;

         --  Processing for X switch

         when 'X' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Extensions_Allowed := True;

            else
               raise Bad_Switch;
            end if;

         --  Processing for y switch

         when 'y' =>
            if Program = Compiler then
               Ptr := Ptr + 1;

               if Ptr > Max then
                  Set_Default_Style_Check_Options;

               else
                  declare
                     OK  : Boolean;

                  begin
                     Set_Style_Check_Options
                       (Switch_Chars (Ptr .. Max), OK, Ptr);

                     if not OK then
                        raise Bad_Switch;
                     end if;
                  end;
               end if;

            else
               raise Bad_Switch;
            end if;

         --  Processing for z switch

         when 'z' =>
            Ptr := Ptr + 1;

            --  Allowed for compiler, only if this is the only
            --  -z switch, we do not allow multiple occurrences

            if Program = Compiler
              and then Distribution_Stub_Mode = No_Stubs
            then
               case Switch_Chars (Ptr) is
                  when 'r' =>
                     Distribution_Stub_Mode := Generate_Receiver_Stub_Body;

                  when 'c' =>
                     Distribution_Stub_Mode := Generate_Caller_Stub_Body;

                  when others =>
                     raise Bad_Switch;
               end case;

               Ptr := Ptr + 1;

            elsif Program = Binder
              or else Program = Make
            then
               No_Main_Subprogram := True;

            else
               raise Bad_Switch;
            end if;

         --  Processing for Z switch

         when 'Z' =>
            Ptr := Ptr + 1;

            if Program = Compiler then
               Zero_Cost_Exceptions_Set := True;
               Zero_Cost_Exceptions_Val := True;
            else
               raise Bad_Switch;
            end if;

         --  Processing for 83 switch

         when '8' =>

            if Program = Compiler then
               if Ptr = Max then
                  raise Bad_Switch;
               end if;

               Ptr := Ptr + 1;

               if Switch_Chars (Ptr) /= '3' then
                  raise Bad_Switch;
               else
                  Ptr := Ptr + 1;
                  Ada_95 := False;
                  Ada_83 := True;
               end if;

            else
               raise Bad_Switch;
            end if;

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

   end Scan_Switches;

end Switch;
