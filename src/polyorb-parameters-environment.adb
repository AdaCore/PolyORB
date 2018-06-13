------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . P A R A M E T E R S . E N V I R O N M E N T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with Interfaces.C.Strings;
with System;

package body PolyORB.Parameters.Environment is

   use Interfaces.C;
   use Interfaces.C.Strings;

   function Make_Env_Name (Section, Key : String) return String;
   --  Build environment variable from (Section, Key) tuple

   -----------------------------
   -- Environment data source --
   -----------------------------

   type Env_Source is new Parameters_Source with null record;

   overriding function Get_Conf
     (Source       : access Env_Source;
      Section, Key : String) return String;

   The_Env_Source : aliased Env_Source;

   --------------
   -- Get_Conf --
   --------------

   overriding function Get_Conf
     (Source       : access Env_Source;
      Section, Key : String) return String
   is
      pragma Unreferenced (Source);
      function getenv (Key : System.Address) return chars_ptr;
      pragma Import (C, getenv, "getenv");

      C_Key   : aliased char_array := To_C (Make_Env_Name (Section, Key));
      C_Value : constant chars_ptr := getenv (C_Key'Address);

   begin
      if C_Value = Null_Ptr then
         return "";
      else
         return Value (C_Value);
      end if;
   end Get_Conf;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_Source (The_Env_Source'Access);
   end Initialize;

   -------------------
   -- Make_Env_Name --
   -------------------

   function Make_Env_Name (Section, Key : String) return String is
      Result : String := "POLYORB_"
        & PolyORB.Utils.To_Upper (Section & "_" & Key);
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
               Result (J) := '_';
         end case;
      end loop;
      while Result (Last) = '_' loop
         Last := Last - 1;
      end loop;
      return Result (Result'First .. Last);
   end Make_Env_Name;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"parameters.environment",
       Conflicts => Empty,
       Depends   => +"parameters.command_line?",
       Provides  => +"parameters_sources" & "parameters_sources.runtime",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Parameters.Environment;
