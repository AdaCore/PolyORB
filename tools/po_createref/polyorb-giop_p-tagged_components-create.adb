------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TAGGED_COMPONENTS.CREATE                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

with PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
with PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Create;
with PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
with PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address.Create;
with PolyORB.GIOP_P.Tagged_Components.Code_Sets;
with PolyORB.GIOP_P.Tagged_Components.Code_Sets.Create;
with PolyORB.GIOP_P.Tagged_Components.Policies;
with PolyORB.GIOP_P.Tagged_Components.Policies.Create;

package body PolyORB.GIOP_P.Tagged_Components.Create is

   ------------------------------
   -- Create_Tagged_Components --
   ------------------------------

   procedure Create_Tagged_Components
     (Param      : Component_Array;
      TCs        : in out Tagged_Component_List;
      Error      : out Boolean)
   is
      use PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans.Create;
      use PolyORB.GIOP_P.Tagged_Components.SSL_Sec_Trans;
      use PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address.Create;
      use PolyORB.GIOP_P.Tagged_Components.Alternate_IIOP_Address;
      use PolyORB.GIOP_P.Tagged_Components.Code_Sets.Create;
      use PolyORB.GIOP_P.Tagged_Components.Code_Sets;
      use PolyORB.GIOP_P.Tagged_Components.Policies.Create;
      use PolyORB.GIOP_P.Tagged_Components.Policies;
      use PolyORB.GIOP_P.Tagged_Components;

      use PolyORB.Utils;
      use PolyORB.Types;

      TC               : Tagged_Component_Access;
      V_Error          : Boolean;
   begin
      Error := False;

      for J in Param'Range loop

         if Param (J).C_Type.all = "policies" then
            TC := new TC_Policies;
            Create_TC (Param (J), TC_Policies (TC.all), V_Error);

         elsif Param (J).C_Type.all = "code_set" then
            TC := new TC_Code_Sets;
            Create_TC (Param (J), TC_Code_Sets (TC.all), V_Error);

         elsif Param (J).C_Type.all = "alternate_address" then
            TC := new TC_Alternate_IIOP_Address;
            Create_TC (Param (J), TC_Alternate_IIOP_Address (TC.all), V_Error);

         elsif Param (J).C_Type.all = "ssl_trans" then
            TC := new TC_SSL_Sec_Trans;
            Create_TC (Param (J), TC_SSL_Sec_Trans (TC.all), V_Error);

         else
            Error := True;
            return;
         end if;

         if V_Error then
            Error := True;
            return;
         end if;

         --  We add the built component into the component list
         Add (TCs, TC);
      end loop;

   end Create_Tagged_Components;

end PolyORB.GIOP_P.Tagged_Components.Create;
