------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . S E T U P . T E S T _ S O A                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Set up a test ORB.

with PolyORB.References;
pragma Elaborate_All (PolyORB.References);

with PolyORB.Smart_Pointers;
pragma Elaborate_All (PolyORB.Smart_Pointers);
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
