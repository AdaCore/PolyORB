------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.GIOP_P.TAGGED_COMPONENTS.CREATE                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
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
