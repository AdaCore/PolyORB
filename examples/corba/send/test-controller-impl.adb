------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T . C O N T R O L L E R . I M P L                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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
