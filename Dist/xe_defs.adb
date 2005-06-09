------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                              X E _ D E F S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2005 Free Software Foundation, Inc.           --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with XE_Utils;         use XE_Utils;
with XE_Defs.Defaults;

package body XE_Defs is

   function Get_Def_Protocol_Data return String is
   begin
      return Defaults.Default_Protocol_Data;
   end Get_Def_Protocol_Data;

   function Get_Def_Protocol_Name return String is
   begin
      return Defaults.Default_Protocol_Name;
   end Get_Def_Protocol_Name;

   function Get_Def_Storage_Data return String is
   begin
      return Defaults.Default_Storage_Data;
   end Get_Def_Storage_Data;

   function Get_Def_Storage_Name return String is
   begin
      return Defaults.Default_Storage_Name;
   end Get_Def_Storage_Name;

   function Get_PCS_Name return String is
   begin
      return Defaults.Default_PCS_Name;
   end Get_PCS_Name;

   function Get_Rsh_Command return String is
   begin
      return Defaults.Default_RSH_Command;
   end Get_Rsh_Command;

   function Get_Rsh_Options return String is
   begin
      return Defaults.Default_RSH_Options;
   end Get_Rsh_Options;

   procedure Initialize is
      Default_Args : Argument_List_Access := Argument_String_To_List (
                                               Defaults.Default_Dist_Flags);
   begin
      for J in Default_Args'Range loop
         if Default_Args (J)'Length > 0 then
            Scan_Dist_Arg (Default_Args (J).all);
         end if;
      end loop;
      Free (Default_Args);
   end Initialize;

end XE_Defs;
