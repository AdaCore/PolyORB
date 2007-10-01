------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . Q O S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Unchecked_Deallocation;

with PolyORB.Types;

package body PolyORB.QoS is

   use PolyORB.Types;

   -----------
   -- Image --
   -----------

   function Image (QoS : QoS_Parameters) return String is
      Result : PolyORB.Types.String := To_PolyORB_String ("");

   begin
      for J in QoS'Range loop
         if QoS (J) /= null then
            Result := Result
              & To_PolyORB_String (QoS_Kind'Image (QoS (J).Kind) & ",");
         end if;
      end loop;

      return To_Standard_String (Result);
   end Image;

   -------------
   -- Release --
   -------------

   procedure Release (QoS : in out QoS_Parameter_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation
        (QoS_Parameter'Class, QoS_Parameter_Access);
   begin
      if QoS /= null then
         Release_Contents (QoS);
         Free (QoS);
      end if;
   end Release;

   ----------------------
   -- Release_Contents --
   ----------------------

   procedure Release_Contents (QoS : access QoS_Parameter) is
      pragma Unreferenced (QoS);
   begin
      null;
   end Release_Contents;

end PolyORB.QoS;
