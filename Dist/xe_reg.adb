------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                               X E _ R E G                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2002 Free Software Foundation, Inc.           --
--                                                                          --
-- GNATDIST is  free software;  you  can redistribute  it and/or  modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 2,  or  (at your option) any later --
-- version. GNATDIST is distributed in the hope that it will be useful, but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or FITNESS  FOR A PARTICULAR PURPOSE.  See the  GNU General  Public --
-- License  for more details.  You should  have received a copy of the  GNU --
-- General Public License distributed with  GNATDIST; see file COPYING.  If --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
--                 GLADE  is maintained by ACT Europe.                      --
--                 (email: glade-report@act-europe.fr)                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.Registry; use GNAT.Registry;

package body XE_Reg is

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

      --  open "HKEY_LOCAL_MACHINE\SOFTWARE\Free Software Foundation" key

      ACT_Key := Open_Key (HKEY_LOCAL_MACHINE,
                           "SOFTWARE\Ada Core Technologies");

      --  get GCC value

      Result := new String'(Query_Value (ACT_Key, "GCC") & "\lib\garlic");

      Close_Key (ACT_Key);

      return Result;

   exception
      when Registry_Error =>
         return null;
   end Get_GARLIC_Dir;

end XE_Reg;
