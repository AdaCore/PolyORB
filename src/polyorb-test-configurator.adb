------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . T E S T . C O N F I G U R A T O R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Text_IO; use Ada.Text_IO;
with PolyORB.Initialization; use PolyORB.Initialization;
with PolyORB.Utils.Strings; use PolyORB.Utils.Strings;

procedure PolyORB.Test.Initialization is

   use PolyORB.Initialization.String_Lists;

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
   procedure Init_Bazaar is new Init ("bazaar");
   procedure Init_Fred is new Init ("fred");
   procedure Init_Alf is new Init ("alf");
   procedure Init_Grumpf is new Init ("grumpf");

   Empty_List : String_Lists.List;

begin
   Register_Module
     (Module_Info'
      (Name => +"foo",
       Conflicts => Empty_List,
       Depends => Empty_List,
       Provides => Empty_List,
       Init => Init_Foo'Unrestricted_Access));
   Register_Module
     (Module_Info'
      (Name => +"bar",
       Depends => Empty_List & "foo" & "baz",
       Conflicts => Empty_List,
       Provides => Empty_List,
       Init => Init_Bar'Unrestricted_Access));
   Register_Module
     (Module_Info'
      (Name => +"bazooka",
       Depends => Empty_List,
       Conflicts => Empty_List,
       Provides => Empty_List & "baz",
       Init => Init_Bazooka'Unrestricted_Access));

   Register_Module
     (Module_Info'
      (Name => +"fred",
       Depends => Empty_List & "bar" & "foo",
       Conflicts => Empty_List & "bazaar",
       Provides => Empty_List,
       Init => Init_Fred'Unrestricted_Access));

   Initialize_World;
end PolyORB.Test.Initialization;
