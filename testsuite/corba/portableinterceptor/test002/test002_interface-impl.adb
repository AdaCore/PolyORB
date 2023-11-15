------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T 0 0 2 _ I N T E R F A C E . I M P L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2023, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Report;

with PortableInterceptor.Current;

with Test002_Interface.Skel;
pragma Warnings (Off, Test002_Interface.Skel);

with Test002_Globals;

package body Test002_Interface.Impl is

   use CORBA;
   use CORBA.TypeCode;
   use PolyORB.Utils.Report;
   use PortableInterceptor.Current;
   use Test002_Globals;

   ----------
   -- Proc --
   ----------

   procedure Proc (Self : access Object) is
      pragma Unreferenced (Self);

      Aux : Any;

   begin
      begin
         Aux := get_slot (PI_Current, Test_Slot);

         if Get_Type (Aux) /= TC_Long then
            Output ("Slot value correctly passed to servant STSC", False);
         elsif From_Any (Aux) /= Long (20) then
            Output ("Slot value correctly passed to servant STSC", False);
         else
            Output ("Slot value correctly passed to servant STSC", True);
         end if;

      exception
         when others =>
            Output ("Slot value correctly passed to servant STSC", False);
      end;

      --  Preparing for test of passing STSC to send interception
      --  point and coping STSC to SRSC.

      set_slot (PI_Current, Test_Slot, To_Any (Long (22)));
   end Proc;

end Test002_Interface.Impl;
