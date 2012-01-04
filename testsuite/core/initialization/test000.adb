------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 0                               --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Text_IO;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

with PolyORB.Utils.Report;

procedure Test000 is

   use Ada.Text_IO;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Report;
   use PolyORB.Utils.Strings;

   generic
      Name : String;
   procedure Init;

   procedure Init is
   begin
      Put_Line ("Initializing module " & Name);
   end Init;

   procedure Init_Foo is new Init ("foo");
   procedure Init_Bar is new Init ("bar");
   procedure Init_Bazooka is new Init ("bazooka");
   procedure Init_Fred is new Init ("fred");

   Empty_List : String_Lists.List;

begin
   Register_Module
     (Module_Info'
      (Name      => +"foo",
       Conflicts => Empty_List,
       Depends   => Empty_List,
       Provides  => Empty_List,
       Implicit  => False,
       Init      => Init_Foo'Unrestricted_Access,
       Shutdown  => null));

   Register_Module
     (Module_Info'
      (Name      => +"bar",
       Depends   => Empty_List & "foo" & "baz",
       Conflicts => Empty_List,
       Provides  => Empty_List,
       Implicit  => False,
       Init      => Init_Bar'Unrestricted_Access,
       Shutdown  => null));

   Register_Module
     (Module_Info'
      (Name      => +"bazooka",
       Depends   => Empty_List,
       Conflicts => Empty_List,
       Provides  => Empty_List & "baz",
       Implicit  => False,
       Init      => Init_Bazooka'Unrestricted_Access,
       Shutdown  => null));

   Register_Module
     (Module_Info'
      (Name      => +"fred",
       Depends   => Empty_List & "bar" & "foo",
       Conflicts => Empty_List & "bazaar",
       Provides  => Empty_List,
       Implicit  => False,
       Init      => Init_Fred'Unrestricted_Access,
       Shutdown  => null));

   Initialize_World;

   Output ("Test initialization #0", True);
   End_Report;
end Test000;
