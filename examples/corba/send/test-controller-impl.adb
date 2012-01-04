------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T . C O N T R O L L E R . I M P L                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Test.Controller.Skel;
pragma Warnings (Off, Test.Controller.Skel);
with Test.Printer.Impl;

package body Test.Controller.Impl is

   -----------------
   -- Get_Printer --
   -----------------

   function Get_Printer (Self : access Object) return Test.Printer.Ref is
   begin
      return Self.Printer;
   end Get_Printer;

   -----------------
   -- Set_Printer --
   -----------------

   procedure Set_Printer (Self : access Object; Printer : Test.Printer.Ref) is
   begin
      Self.Printer := Printer;
   end Set_Printer;

   --------------------
   -- Set_Group_Size --
   --------------------

   procedure Set_Group_Size (Self : access Object; Size : Natural) is
   begin
      Self.Group_Size := Size;
   end Set_Group_Size;

   ----------------
   -- StopServer --
   ----------------

   procedure StopServer (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      CORBA.ORB.Shutdown (Wait_For_Completion => False);
   end StopServer;

   -------------
   -- Test_OK --
   -------------

   function Test_OK (Self : access Object) return CORBA.Boolean is
   begin
      return Test.Printer.Impl.PrintString_Called = Self.Group_Size
        and then Test.Printer.Impl.PrintLong_Called = Self.Group_Size;
   end Test_OK;

end Test.Controller.Impl;
