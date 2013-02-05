------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . P A R A M E T E R S . M A C R O S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2013, Free Software Foundation, Inc.             --
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

with GNATCOLL.Templates;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.Parameters.Macros is

   use GNATCOLL.Templates;

   function Expand_Macros (S : String) return String;
   --  Substitute macros in S

   function Get_Macro (Name : String; Quoted : Boolean) return String;
   --  Get value for macro Name from configuration

   -------------------
   -- Expand_Macros --
   -------------------

   function Expand_Macros (S : String) return String is
   begin
      return GNATCOLL.Templates.Substitute
        (Str        => S,
         Substrings => No_Substitution,
         Callback   => Get_Macro'Access,
         Delimiter  => '$',
         Recursive  => True,
         Errors     => Report_Error);
   end Expand_Macros;

   ---------------
   -- Get_Macro --
   ---------------

   function Get_Macro (Name : String; Quoted : Boolean) return String is
      pragma Unreferenced (Quoted);

      Val : constant String :=
              Get_Conf (Section => "macros", Key => Name);

   begin
      if Val = "" then
         raise Invalid_Substitution with "macro " & Name & " undefined";
      end if;
      return Val;
   end Get_Macro;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;
   procedure Initialize is
   begin
      Expand_Macros_Hook := Expand_Macros'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   --  The static parameters source depends on parameters_sources.runtime
   --  so that its values can be overridden at run time.

   Register_Module
      (Module_Info'
         (Name      => +"parameters.macros",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => +"parameters_sources",
          Implicit  => True,
          Init      => Initialize'Access,
          Shutdown  => null));
end PolyORB.Parameters.Macros;
