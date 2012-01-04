------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       T E S T 0 0 0 _ I D L . T E S T I N T E R F A C E . I M P L        --
--                                                                          --
--                                 S p e c                                  --
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

with PortableServer;

package Test000_Idl.TestInterface.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Proc (Self : access Object);

   procedure Process_Normal (Self : access Object);

   procedure Raise_Exception (Self : access Object);

   procedure Init
     (Self : access Object;
      Name : String);

private

   type Process_State is (Normal, Raise_Exception);

   type Object is
     new PortableServer.Servant_Base with
   record
      Name  : CORBA.String;
      State : Process_State := Normal;
   end record;

end Test000_Idl.TestInterface.Impl;
