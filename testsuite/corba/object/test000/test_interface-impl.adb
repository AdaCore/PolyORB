------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  T E S T _ I N T E R F A C E . I M P L                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;

with CORBA.ORB;
with PortableServer.POA;
with PortableServer.POA.Helper;

with Test_Interface.Skel;
pragma Warnings (Off, Test_Interface.Skel);

package body Test_Interface.Impl is

   function get_invalid_ref
     (Self : access Object) return Test_Interface.Ref'Class
   is
      pragma Unreferenced (Self);
      Root_POA : PortableServer.POA.Local_Ref;
      Result   : Test_Interface.Ref;
   begin
      Root_POA := PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      declare
         use PortableServer;
         use PortableServer.POA;
      begin
         Set (Result,
              CORBA.Object.Object_Of
              (Servant_To_Reference (Root_POA, new Object)));
         Deactivate_Object (Root_POA, Reference_To_Id (Root_POA, Result));
      end;
      return Result;
   exception
      when E : others =>
         Put_Line ("get_invalid_ref: server side exception "
           & Ada.Exceptions.Exception_Information (E));
         raise;
   end get_invalid_ref;

   procedure terminate_server (Self : access Object) is
      pragma Unreferenced (Self);
   begin
      GNAT.OS_Lib.OS_Exit (0);
   end terminate_server;

end Test_Interface.Impl;
