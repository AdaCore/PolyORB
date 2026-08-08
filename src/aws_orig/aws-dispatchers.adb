------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      A W S . D I S P A T C H E R S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2000-2012, Free Software Foundation, Inc.          --
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

package body AWS.Dispatchers is

   procedure Release is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Handler'Class,


      Name   => Handler_Class_Access);

   procedure Free is
      new PolyORB.Utils.Unchecked_Deallocation.Free


     (Object => Natural,


      Name   => Natural_Access);

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
