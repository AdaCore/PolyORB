------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SMART_POINTERS.SYNC_COUNTERS                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2009-2021, Free Software Foundation, Inc.          --
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

separate (PolyORB.Smart_Pointers) package body Sync_Counters is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Unsigned_32;
      Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
      function Intrinsic_Sync_Add_And_Fetch
        (Ptr   : access Interfaces.Unsigned_32;
         Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
      pragma Import
        (Intrinsic, Intrinsic_Sync_Add_And_Fetch, "__sync_add_and_fetch_4");

   begin
      return Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

   ------------------------
   -- Sync_Sub_And_Fetch --
   ------------------------

   function Sync_Sub_And_Fetch
     (Ptr   : access Interfaces.Unsigned_32;
      Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32
   is
      function Intrinsic_Sync_Sub_And_Fetch
        (Ptr   : access Interfaces.Unsigned_32;
         Value : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
      pragma Import
        (Intrinsic, Intrinsic_Sync_Sub_And_Fetch, "__sync_sub_and_fetch_4");

   begin
      return Intrinsic_Sync_Sub_And_Fetch (Ptr, Value);
   end Sync_Sub_And_Fetch;

end Sync_Counters;
