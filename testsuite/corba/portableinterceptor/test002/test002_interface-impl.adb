------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T 0 0 2 _ I N T E R F A C E . I M P L                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
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
         Aux := Get_Slot (PI_Current, Test_Slot);

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

      Set_Slot (PI_Current, Test_Slot, To_Any (Long (22)));
   end Proc;

end Test002_Interface.Impl;
