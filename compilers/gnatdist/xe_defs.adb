------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              X E _ D E F S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1995-2008, Free Software Foundation, Inc.          --
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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with XE_Utils;         use XE_Utils;
with XE_Defs.Defaults;

package body XE_Defs is

   PCS_Name : String_Access := new String'(Defaults.Default_PCS_Name);

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
      return PCS_Name.all;
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
   begin
      Scan_Dist_Args (Defaults.Default_Dist_Flags);
   end Initialize;

   procedure Set_PCS_Name (S : String) is
   begin
      Free (PCS_Name);
      PCS_Name := new String'(S);
   end Set_PCS_Name;

end XE_Defs;
