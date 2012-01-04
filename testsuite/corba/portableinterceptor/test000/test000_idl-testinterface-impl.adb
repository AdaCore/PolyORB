------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       T E S T 0 0 0 _ I D L . T E S T I N T E R F A C E . I M P L        --
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

with Test000_Idl.TestInterface.Helper;

with Test000_Idl.TestInterface.Skel;
pragma Warnings (Off, Test000_Idl.TestInterface.Skel);

with Test000_Globals;

package body Test000_Idl.TestInterface.Impl is

   ----------
   -- Init --
   ----------

   procedure Init
     (Self : access Object;
      Name : String)
   is
   begin
      Self.Name := CORBA.To_CORBA_String (Name);
   end Init;

   ----------
   -- Proc --
   ----------

   procedure Proc (Self : access Object) is
   begin
      Test000_Globals.Log_Point (CORBA.To_Standard_String (Self.Name));
      if Self.State = Normal then
         null;
      else
         Test000_Idl.TestInterface.Helper.Raise_TestException
           (TestException_Members'
            (CORBA.IDL_Exception_Members with null record));
      end if;
   end Proc;

   --------------------
   -- Process_Normal --
   --------------------

   procedure Process_Normal (Self : access Object) is
   begin
      Self.State := Normal;
   end Process_Normal;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (Self : access Object) is
   begin
      Self.State := Raise_Exception;
   end Raise_Exception;

end Test000_Idl.TestInterface.Impl;
