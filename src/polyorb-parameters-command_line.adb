------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . P A R A M E T E R S . C O M M A N D _ L I N E       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with Ada.Command_Line;

--  with System.IO;

package body PolyORB.Parameters.Command_Line is

   function Make_Flag (Section, Key : String) return String;
   --  Build flag string from (Section, Key) tuple

   -------------------------
   -- Command line source --
   -------------------------

   type Cmd_Line_Source is new Parameters_Source with null record;
   overriding function Get_Conf
     (Source       : access Cmd_Line_Source;
      Section, Key : String) return String;

   The_Cmd_Line_Source : aliased Cmd_Line_Source;

   overriding function Get_Conf
     (Source       : access Cmd_Line_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);

      use Ada.Command_Line;

      Flag  : constant String  := Make_Flag (Section, Key);
      F_Len : constant Natural := Flag'Length;

   begin
      for J in 1 .. Argument_Count loop
         declare
            Arg   : constant String  := Argument (J);
            A_Len : constant Natural := Arg'Length;

         begin
            if F_Len <= A_Len
              and then Flag = Arg (Arg'First .. Arg'First + F_Len - 1)
            then
               if F_Len = A_Len then
                  return "true";

               elsif Arg (Arg'First + F_Len) = '=' then
                  return Arg (Arg'First + F_Len + 1 .. Arg'Last);

               end if;
            end if;
         end;
      end loop;

      return "";
   end Get_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_Source (The_Cmd_Line_Source'Access);
   end Initialize;

   ---------------
   -- Make_Flag --
   ---------------

   function Make_Flag (Section, Key : String) return String is
      Result : String := "--polyorb-"
        & PolyORB.Utils.To_Lower (Section & "-" & Key);
      Last : Positive := Result'Last;

   begin
      for J in Result'Range loop
         case Result (J) is
            when
              '0' .. '9' |
              'A' .. 'Z' |
              'a' .. 'z' |
              '_'        =>
               null;
            when others =>
               Result (J) := '-';
         end case;
      end loop;
      while Result (Last) = '-' loop
         Last := Last - 1;
      end loop;
      return Result (Result'First .. Last);
   end Make_Flag;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.command_line",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"parameters_sources" & "parameters_sources.runtime",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Parameters.Command_Line;
