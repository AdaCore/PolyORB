------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T . F A C T O R Y . I M P L                     --
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

with CORBA.ORB;

with Test_Support;

with Test.Factory.Skel;
pragma Warnings (Off, Test.Factory.Skel);

package body Test.Factory.Impl is

   -----------------------
   -- Create_References --
   -----------------------

   function Create_References
     (Self  : access Object;
      Count : CORBA.Long)
      return Test.Factory.EchoSequence
   is
      pragma Unreferenced (Self);

      Result : Test.Factory.EchoSequence;

   begin
      for J in 1 .. Natural (Count) loop
         Append (Result, Test_Support.To_Object_Reference (J));
      end loop;

      return Result;
   end Create_References;

   -----------------
   -- Preallocate --
   -----------------

   procedure Preallocate
     (Self  : access Object;
      Count : CORBA.Long)
   is
      pragma Unreferenced (Self);

   begin
      Test_Support.Preallocate (Natural (Count));
   end Preallocate;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Self : access Object) is
      pragma Unreferenced (Self);

   begin
      CORBA.ORB.Shutdown (False);
   end Shutdown;

end Test.Factory.Impl;
