------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      A W S . D I S P A T C H E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2006, Free Software Foundation, Inc.          --
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

package body AWS.Dispatchers is

   procedure Release is
      new Ada.Unchecked_Deallocation (Handler'Class, Handler_Class_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Natural, Natural_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter.all := Dispatcher.Ref_Counter.all + 1;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter.all := Dispatcher.Ref_Counter.all - 1;
      if Dispatcher.Ref_Counter.all = 0 then
         Free (Dispatcher.Ref_Counter);
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Dispatcher : in out Handler_Class_Access) is
   begin
      Release (Dispatcher);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter := new Natural'(1);
   end Initialize;

   -----------------
   -- Ref_Counter --
   -----------------

   function Ref_Counter (Dispatcher : Handler) return Natural is
   begin
      if Dispatcher.Ref_Counter = null then
         return 0;
      else
         return Dispatcher.Ref_Counter.all;
      end if;
   end Ref_Counter;

end AWS.Dispatchers;
