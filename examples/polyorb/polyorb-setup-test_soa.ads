------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E T U P . T E S T _ S O A                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Set up a test ORB.

with PolyORB.References;

with PolyORB.Smart_Pointers;
pragma Warnings (Off, PolyORB.Smart_Pointers);
--  The dependency and pragma above should not be necessary
--  (because of the dependency and pragma on PolyORB.References,
--  which has Smart_Pointers in its closure). They are necessary to
--  work around a bug in GNAT 3.15.

package PolyORB.Setup.Test_SOA is

   pragma Elaborate_Body;

   procedure Initialize_Test_Object;
   --  Create the test object implementation.

   My_Ref : PolyORB.References.Ref;
   --  Object reference designating the created test object.

   procedure Run_Test;
   --  Execute the test server.

end PolyORB.Setup.Test_SOA;
