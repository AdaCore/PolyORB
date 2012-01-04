------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . B I N D I N G _ D A T A . C R E A T E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Utils.Chained_Lists;
with Ada.Text_IO;

package body PolyORB.Binding_Data.Create is

   type Node is record
      Profile : String (1 .. 4);
      Create  : Create_Procedure;
   end record;

   package Lists is new PolyORB.Utils.Chained_Lists (Node);
   use Lists;

   Callbacks : Lists.List;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile : String;
      Create  : Create_Procedure)
   is
   begin
      Append (Callbacks, Node'(Profile => Profile, Create => Create));
   end Register;

   --------------------
   -- Create_Profile --
   --------------------

   procedure Create_Profile
     (Param          : Parameter_Profile;
      Profile        : out PolyORB.Binding_Data.Profile_Access;
      Error          : out Boolean)
   is
      Tag : constant String := Param.Profile_Type.all;
      It  : Iterator := First (Callbacks);
   begin
      Error := True;

      while not Last (It) loop
         declare
            Info : constant Node := Value (It).all;
         begin
            if Tag = Info.Profile then
               Value (It).Create
                 (Param, Profile, Error);
               return;
            end if;
         end;
         Next (It);
      end loop;

      Ada.Text_IO.Put_Line ("Unknown tag : " & Tag);
   end Create_Profile;

end PolyORB.Binding_Data.Create;
