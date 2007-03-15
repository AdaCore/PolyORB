------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . P A R A M E T E R S . E N V I R O N M E N T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
   function Get_Conf
     (Source       : access Env_Source;
      Section, Key : String) return String;

   The_Env_Source : aliased Env_Source;

   function Get_Conf
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
       Provides  => +"parameters_sources",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Parameters.Environment;
