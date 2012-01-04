------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . R T C O R B A _ P . T H R E A D P O O L M A N A G E R   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Initialization;

with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Utils.Strings.Lists;

package body PolyORB.RTCORBA_P.ThreadPoolManager is

   use PolyORB.Tasking.Mutexes;
   use type RTCORBA.ThreadpoolId;

   type Thread_Pool is record
      Lane  : PolyORB.Lanes.Lane_Root_Access;
      Index : RTCORBA.ThreadpoolId;
   end record;

   package Thread_Pool_Lists is
      new PolyORB.Utils.Chained_Lists (Thread_Pool);
   use Thread_Pool_Lists;

   Thread_Pool_List_Lock : PolyORB.Tasking.Mutexes.Mutex_Access;
   Thread_Pool_Index : RTCORBA.ThreadpoolId  := 0;
   Thread_Pools : Thread_Pool_Lists.List;

   ----------
   -- Lane --
   ----------

   function Lane
     (Index : RTCORBA.ThreadpoolId)
     return PolyORB.Lanes.Lane_Root_Access
   is
      It : Iterator;
      Result : PolyORB.Lanes.Lane_Root_Access;

   begin
      Enter (Thread_Pool_List_Lock);

      It := First (Thread_Pools);

      while not Last (It) loop
         if Value (It).Index = Index then
            Result := Value (It).Lane;
            exit;
         end if;
         Next (It);
      end loop;

      Leave (Thread_Pool_List_Lock);

      return Result;
   end Lane;

   ---------------------
   -- Lane_Registered --
   ---------------------

   function Lane_Registered
     (Index :    RTCORBA.ThreadpoolId)
     return Boolean
   is
      It : Iterator;
      Found : Boolean := False;

   begin
      Enter (Thread_Pool_List_Lock);

      It := First (Thread_Pools);

      while not Last (It) loop
         if Value (It).Index = Index then
            Found := True;
            exit;
         end if;
         Next (It);
      end loop;

      Leave (Thread_Pool_List_Lock);

      return Found;
   end Lane_Registered;

   -------------------
   -- Register_Lane --
   -------------------

   procedure Register_Lane
     (Lane  :        PolyORB.Lanes.Lane_Root_Access;
      Index :    out RTCORBA.ThreadpoolId)
   is
      New_Thread_Pool : Thread_Pool;

   begin
      --  Update Thread_Pool_Index

      Enter (Thread_Pool_List_Lock);

      New_Thread_Pool.Index := Thread_Pool_Index;
      Thread_Pool_Index := Thread_Pool_Index + 1;

      Leave (Thread_Pool_List_Lock);

      --  Update Thread_Pool_List

      New_Thread_Pool.Lane := Lane;
      Append (Thread_Pools, New_Thread_Pool);

      Index := New_Thread_Pool.Index;
   end Register_Lane;

   ---------------------
   -- Unregister_Lane --
   ---------------------

   procedure Unregister_Lane (Index : RTCORBA.ThreadpoolId) is
      function Index_Equality (TP : Thread_Pool) return Boolean;

      function Index_Equality (TP : Thread_Pool) return Boolean is
      begin
         return TP.Index = Index;
      end Index_Equality;

      procedure Remove is new Remove_G (Index_Equality);
   begin
      --  Destroy associated lane

      PolyORB.Lanes.Destroy (ThreadPoolManager.Lane (Index));

      --  Remove index from Thread_Pools list

      Enter (Thread_Pool_List_Lock);
      Remove (Thread_Pools, All_Occurrences => False);
      Leave (Thread_Pool_List_Lock);
   end Unregister_Lane;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is

   begin
      Create (Thread_Pool_List_Lock);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Utils.Strings;
   use PolyORB.Utils.Strings.Lists;

begin
   Register_Module
     (Module_Info'
      (Name      => +"polyorb.rtcorba_p.threadpoolpolicy",
       Conflicts => PolyORB.Utils.Strings.Lists.Empty,
       Depends   => +"tasking.mutexes",
       Provides  => PolyORB.Utils.Strings.Lists.Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.RTCORBA_P.ThreadPoolManager;
