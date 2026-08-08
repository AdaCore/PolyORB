------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . Q O S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Unchecked_Deallocation;

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
        new PolyORB.Utils.Unchecked_Deallocation.Free

        (Object => QoS_Parameter'Class,

         Name   => QoS_Parameter_Access);
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
