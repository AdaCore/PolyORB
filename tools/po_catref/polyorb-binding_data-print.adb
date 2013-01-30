------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           P O L Y O R B . B I N D I N G _ D A T A . P R I N T            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

with PO_Catref.Output;

package body PolyORB.Binding_Data.Print is

   use PO_Catref.Output;

   type Node is record
      Profile : Profile_Tag;
      Print   : Print_Procedure;
   end record;

   package Lists is new PolyORB.Utils.Chained_Lists (Node);
   use Lists;

   Callbacks : Lists.List;

   --------------
   -- Register --
   --------------

   procedure Register
     (Profile : Profile_Tag;
      Print   : Print_Procedure)
   is
   begin
      Append (Callbacks, Node'(Profile => Profile, Print => Print));
   end Register;

   -------------------
   -- Print_Profile --
   -------------------

   procedure Print_Profile (Profile : Profile_Access) is
      Tag : constant Profile_Tag := Get_Profile_Tag (Profile.all);
      It : Iterator := First (Callbacks);

   begin
      while not Last (It) loop
         declare
            Info : constant Node := Value (It).all;
         begin
            if Tag = Info.Profile then
               Value (It).Print (Profile);

               return;
            end if;
         end;
         Next (It);
      end loop;

      Put_Line ("Unknown tag", Tag'Img);
   end Print_Profile;

end PolyORB.Binding_Data.Print;
