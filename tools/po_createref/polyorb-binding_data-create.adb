------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . B I N D I N G _ D A T A . C R E A T E           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2007, Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
     (Profile : in String;
      Create  : in Create_Procedure)
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
