------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    P O L Y O R B . S E T U P . U I P M C                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

pragma Warnings (Off);
--  No entities referenced.

with PolyORB.Protocols.GIOP.GIOP_1_2;
pragma Warnings (On);

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15
with PolyORB.Utils.Strings;

package body PolyORB.Setup.UIPMC is

   use PolyORB.Smart_Pointers;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      UIPMC_GOA := new Group_Object_Adapter;
      Create (UIPMC_GOA);
      Set (UIPMC_GOA_Ref, Entity_Ptr (UIPMC_GOA));
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"setup.uipmc",
       Conflicts => Empty,
       Depends   => +"protocols.giop.giop_1_2",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.Setup.UIPMC;
