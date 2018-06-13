------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . D S A _ P . C O N N E C T I O N S             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2011-2012, Free Software Foundation, Inc.          --
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

with Ada.Tags;
with Ada.Unchecked_Conversion;

with System.Partition_Interface;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.References;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Ilists;

package body PolyORB.DSA_P.Connections is

   use Ada.Tags;
   use System.Partition_Interface;

   package Stub_Lists is new PolyORB.Utils.Ilists.Lists
     (T             => RACW_Stub_Type'Class,
      T_Acc         => RACW_Stub_Type_Access,
      Link          => Link,
      Doubly_Linked => True);

   type Connection_Manager_Note is new Annotations.Note with record
      Stubs : Stub_Lists.List;
      --  List of stubs for objects reached through a given binding object
   end record;
   pragma Unreferenced (Connection_Manager_Note);

   package body Connection_Manager is

      function To_Stub_Access is
        new Ada.Unchecked_Conversion (RACW, RACW_Stub_Type_Access);

      ---------------
      -- Is_Remote --
      ---------------

      function Is_Remote (X : RACW) return Boolean is
      begin
         --  Note: can't take X'Tag since this would implicitly dereference
         --  an RACW.

         return To_Stub_Access (X)'Tag = Stub_Type'Tag;
      end Is_Remote;

      ------------------
      -- Is_Connected --
      ------------------

      function Is_Connected (X : RACW) return Boolean is
         T_Ref : aliased References.Ref;
      begin
         if not Is_Remote (X) then
            return True;
         end if;

         References.Set (T_Ref, To_Stub_Access (X).Target);

         declare
            use type Components.Component_Access;

            BO  : Smart_Pointers.Ref;
            Pro : Binding_Data.Profile_Access;

            Scope_Lock : Tasking.Mutexes.Scope_Lock
                           (References.Mutex_Of (T_Ref));
            pragma Unreferenced (Scope_Lock);
         begin
            References.Get_Binding_Info
              (R   => T_Ref,
               QoS => (others => null),
               BO  => BO,
               Pro => Pro);
            return not Smart_Pointers.Is_Nil (BO);
         end;
      end Is_Connected;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect (X : RACW) is
      begin
         raise Program_Error; --  TBD???
      end Disconnect;

      -------------------
      -- On_Disconnect --
      -------------------

      procedure On_Disconnect
        (X        : RACW;
         Callback : access procedure (X : RACW))
      is
      begin
         raise Program_Error; --  TBD???
      end On_Disconnect;

      ----------------------
      -- Unchecked_Forget --
      ----------------------

      procedure Unchecked_Forget (X : in out RACW) is
         R : RACW_Stub_Type_Access;
         for R'Address use X'Address;
         pragma Import (Ada, R);
      begin
         --  Remove X from its session's disconnect notification list

         --  TBD???

         if Is_Remote (X) then
            Free_Stub (R);
         end if;
         X := null;
      end Unchecked_Forget;

   end Connection_Manager;

end PolyORB.DSA_P.Connections;
