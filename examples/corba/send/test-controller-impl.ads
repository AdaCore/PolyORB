------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T . C O N T R O L L E R . I M P L                  --
--                                                                          --
--                                 S p e c                                  --
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

with CORBA;
with PortableServer;

package Test.Controller.Impl is

   type Object is new PortableServer.Servant_Base with private;

   function Get_Printer (Self : access Object) return Test.Printer.Ref;

   procedure Set_Printer (Self : access Object; Printer : Test.Printer.Ref);

   procedure Set_Group_Size (Self : access Object; Size : Natural);

   procedure StopServer (Self : access Object);

   function Test_OK (Self : access Object) return CORBA.Boolean;

private

   type Object is new PortableServer.Servant_Base with record
      Printer : Test.Printer.Ref;
      Group_Size : Natural := 0;
   end record;

end Test.Controller.Impl;
