------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               X E _ R E G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with GNAT.Registry; use GNAT.Registry;

package body XE_Reg is

   function Get_GNAT_Version return String;
   --  Returns the GNAT version number

   --------------------
   -- Get_GARLIC_Dir --
   --------------------

   function Get_GARLIC_Dir return String_Access is
      ACT_Key : HKEY;
      Result  : String_Access;
   begin

      --  First check the GCC_ROOT environent variable

      Result := Getenv ("GCC_ROOT");

      if Result.all /= "" then
         declare
            GCC_ROOT : constant String := Result.all;
         begin
            Free (Result);

            if GCC_ROOT (GCC_ROOT'Last) = '\'
              or else GCC_ROOT (GCC_ROOT'Last) = '/'
            then
               return new String'(GCC_ROOT & "lib\garlic");
            else
               return new String'(GCC_ROOT & "\lib\garlic");
            end if;
         end;
      end if;

      --  GCC_ROOT was not defined, look in the registry.

      --  Open "HKEY_LOCAL_MACHINE\SOFTWARE\Free Software Foundation\
      --          GNAT\<version>" key.

      ACT_Key := Open_Key
        (HKEY_LOCAL_MACHINE,
         "SOFTWARE\Ada Core Technologies\GNAT\" & Get_GNAT_Version);

      --  Get ROOT value

      Result := new String'(Query_Value (ACT_Key, "ROOT") & "\lib\garlic");

      Close_Key (ACT_Key);

      return Result;

   exception
      when Registry_Error =>
         return null;
   end Get_GARLIC_Dir;

   ----------------------
   -- Get_GNAT_Version --
   ----------------------

   function Get_GNAT_Version return String is
      GSVS : constant String := "5.03w";
   begin
      for K in GSVS'Range loop
         if GSVS (K) = ' ' then
            return GSVS (GSVS'First .. K - 1);
         end if;
      end loop;
      return "";
   end Get_GNAT_Version;

end XE_Reg;
