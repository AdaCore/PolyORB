------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.GIOP_P.CODE_SETS.CONVERTERS.TEST                  --
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

package body PolyORB.GIOP_P.Code_Sets.Converters.Test is

   procedure Initialize;

   function Create_UCS2_Native_Converter return Wide_Converter_Access;

   function Create_UCS2_UTF16_Converter return Wide_Converter_Access;

   ----------------------------------
   -- Create_UCS2_Native_Converter --
   ----------------------------------

   function Create_UCS2_Native_Converter return Wide_Converter_Access is
   begin
      return new UCS2_Native_Wide_Converter;
   end Create_UCS2_Native_Converter;

   ---------------------------------
   -- Create_UCS2_UTF16_Converter --
   ---------------------------------

   function Create_UCS2_UTF16_Converter return Wide_Converter_Access is
   begin
      return new UCS2_UTF16_Wide_Converter;
   end Create_UCS2_UTF16_Converter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register_Native_Code_Set
        (16#00040001#,
         --  KS C5601:1987; Korean Hangul and Hanja Graphic Characters
         Create_UCS2_Native_Converter'Access,
         Create_UCS2_UTF16_Converter'Access);
      --  We intentionnaly use wrong converters to reduce code
      --  complexity: we only test Codeset_Incompatible exception
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"code_sets.converters.test",
          Conflicts => Empty,
          Depends   => +"code_sets.converters",
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.GIOP_P.Code_Sets.Converters.Test;
